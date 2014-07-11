package gov.nih.mipav.view.renderer.WildMagic.Render;

import gov.nih.mipav.util.MipavInitGPU;
import gov.nih.mipav.model.structures.ModelSimpleImage;
import gov.nih.mipav.model.structures.TransMatrix;
import gov.nih.mipav.model.structures.TransMatrixd;

import java.awt.image.BufferedImage;
import java.io.File;
import java.io.IOException;

import javax.imageio.ImageIO;
import javax.swing.JFrame;

import WildMagic.LibApplications.OpenGLApplication.JavaApplication3D;
import WildMagic.LibFoundation.Mathematics.ColorRGBA;
import WildMagic.LibFoundation.Mathematics.Matrix4f;
import WildMagic.LibFoundation.Mathematics.Matrix4d;
import WildMagic.LibGraphics.Rendering.AlphaState;
import WildMagic.LibGraphics.Rendering.GraphicsImage;
import WildMagic.LibGraphics.Rendering.ResourceIdentifier;
import WildMagic.LibGraphics.Rendering.Texture;
import WildMagic.LibGraphics.SceneGraph.Attributes;
import WildMagic.LibGraphics.SceneGraph.Polypoint;
import WildMagic.LibGraphics.SceneGraph.VertexBuffer;
import WildMagic.LibGraphics.Shaders.ImageCatalog;
import WildMagic.LibRenderers.OpenGLRenderer.OpenGLFrameBuffer;
import WildMagic.LibRenderers.OpenGLRenderer.OpenGLRenderer;


import javax.media.opengl.*;
import javax.media.opengl.awt.GLCanvas;

public class ImageRegistrationGPU extends JavaApplication3D
//implements GLEventListener, KeyListener
{
    /**  */
    private static final long serialVersionUID = 3444862706603129509L;

    protected static int m_iScreenCaptureCounter = 1000;

    protected VolumeHistogramEffect m_akCollapse2D;
    protected VolumeHistogramEffect m_akCollapseColumns;
    protected VolumeHistogramEffect m_akCollapseRows;
    
    protected ImageReduceEffect m_akImageReduceEntropy;
    protected VolumeHistogramEffect m_kImageEffectDual;
    protected LineMinimizationEffect m_kImageLineMinPass2a;
    protected LineMinimizationEffect m_kImageLineMinDual;
    protected LineMinimizationEffect m_kCalcTransform;

    private Polypoint m_kImagePointsDual;
    private Polypoint m_kHistogramPoints2D;
    private Polypoint m_kEntropyPoints2D;
    private Polypoint m_kBracketPoints;
    private Polypoint m_kTransformPoints;

    private OpenGLFrameBuffer m_kHistogramOutput;
    private OpenGLFrameBuffer m_kHistogramOutputB;
    private OpenGLFrameBuffer m_kEntropyOut;

    private OpenGLFrameBuffer m_kTransformOut;
    //private OpenGLFrameBuffer m_kTransformNewOut;
    private OpenGLFrameBuffer m_kBracketOut;
    private OpenGLFrameBuffer m_kBracketNewOut;
    
    private AlphaState m_kAlpha;
    private ModelSimpleImage m_kImageA;
    private ModelSimpleImage m_kImageB;
    private ModelSimpleImage m_kTarget;
    private ModelSimpleImage m_kMoving;
    private String m_kNameA = "imageA";
    private String m_kNameB = "imageB";
    private Texture m_kTextureA;
    private Texture m_kTextureB;
    
    private double m_dHx, m_dHy, m_dHxy;
    private double m_dOverlap;
    private Matrix4f m_kImageTransform = new Matrix4f(false);
    private Matrix4d m_kImageTransformd = new Matrix4d(false);

    protected JFrame m_kFrame;
    
    private float m_fUnitTolerance;
    private Matrix4f m_kToOrigin = new Matrix4f(false);
    private Matrix4f m_kFromOrigin = new Matrix4f(false);
    private Matrix4f m_kToOriginInv = new Matrix4f(false);
    private Matrix4f m_kFromOriginInv = new Matrix4f(false);
    private float m_fRigid = 1.0f;
    private float m_fDim;
    private float[] m_afStartPoint = new float[16];
    private float[] m_afPt = new float[16];
    private float m_fPtLength;
    private float[] m_afUnitDirections = new float[16];
    private float m_fMinDist;
    

    float[] m_afBracketB = new float[2];
    
    
    
    private boolean m_bDisposeComplete = false;
    private boolean m_bDispose = false;

    protected boolean m_bDisplay = false;
    protected boolean m_bInit = false;
    private boolean m_bCleanUp = false;
    private boolean m_bStatus = true;
    private boolean m_bCalcLineMin = false;
    
    private float[] m_afBracket = new float[3*4];
    
    private int m_iRenderLoops = 1;
    
    public ImageRegistrationGPU( ModelSimpleImage kTarget, ModelSimpleImage kMoving )
    {
        //super( "VolumeImageViewer", 0, 0, kImageA.extents[0],kImageA.extents[1],
        super( "ImageRegistrationGPU", 0, 0, 256, 256,
                new ColorRGBA( 0.0f,0.0f,0.0f,1.0f ) );
        m_pkRenderer = new OpenGLRenderer( m_eFormat, m_eDepth, m_eStencil,
                m_eBuffering, m_eMultisampling,
                m_iWidth, m_iHeight );
        //GetCanvas().getContext().setSwapInterval(0);  
        //GetCanvas().getContext().setSynchronized(true);  
        m_kTarget = kTarget;
        m_kMoving = kMoving;
        String kExternalDirs = MipavInitGPU.getExternalDirs();        
        ImageCatalog.SetActive( new ImageCatalog("Main", kExternalDirs) );     
        m_pkRenderer.SetExternalDir(kExternalDirs);
    } 
    
    public static ImageRegistrationGPU create( ModelSimpleImage kTarget, ModelSimpleImage kMoving )
    {
        final ImageRegistrationGPU kWorld = new ImageRegistrationGPU( kTarget, kMoving );
        final JFrame frame = new JFrame(kWorld.GetWindowTitle());
        frame.add( kWorld.GetCanvas() );
        
        // setting the frame to be undecorated removes the frame title bar and edges
        // this prevents flashing on-screen.
        frame.setUndecorated(true);
        // frame must be set to visible for the gl canvas to be properly initialized.
        frame.setVisible(true);
        frame.setBounds(0,0,
                kWorld.GetWidth(), kWorld.GetHeight() );
        frame.setVisible(false);
        kWorld.SetFrame(frame);
        if ( kWorld.GetCanvas().getContext().makeCurrent() != GLContext.CONTEXT_NOT_CURRENT )
        {
            kWorld.display( kWorld.GetCanvas() );
            kWorld.GetCanvas().getContext().release();
        }
        if ( !kWorld.checkStatus() )
        {
            kWorld.dispose( );
            return null;
        }
        return kWorld;
    }
    

    public void calcError(TransMatrix kTransform) {
        //if ( ((OpenGLRenderer)m_pkRenderer).GetContext() !=  GetCanvas().getContext().getCurrent() )
        {
            if ( GetCanvas().getContext().makeCurrent() == GLContext.CONTEXT_NOT_CURRENT )
            {
                System.err.println( "Context not made current" );
            }
        }
        setTransform(kTransform);
        m_bDisplay = true;
        display(GetCanvas());  
        GetCanvas().getContext().release();
    }
    
    public void calcError(TransMatrixd kTransform) {
        //if ( ((OpenGLRenderer)m_pkRenderer).GetContext() !=  GetCanvas().getContext().getCurrent() )
        {
            if ( GetCanvas().getContext().makeCurrent() == GLContext.CONTEXT_NOT_CURRENT )
            {
                System.err.println( "Context not made current" );
            }
        }
        setTransform(kTransform);
        m_bDisplay = true;
        display(GetCanvas());  
        GetCanvas().getContext().release();
    }
    public void calcLineMinimization()
    {
        //if ( ((OpenGLRenderer)m_pkRenderer).GetContext() !=  GetCanvas().getContext().getCurrent() )
        {
            if ( GetCanvas().getContext().makeCurrent() == GLContext.CONTEXT_NOT_CURRENT )
            {
                System.err.println( "Context not made current" );
            }
        }

        m_bDisplay = true;
        m_bCalcLineMin = true;
        display(GetCanvas());  
        GetCanvas().getContext().release();
    }
    
    public boolean checkStatus()
    {
        return m_bStatus;
    }
    
    public Matrix4f convertTo4D( Matrix4f kTransform )
    {
        if ( m_kImageA.nDims == 2 )
        {
            Matrix4f kMat = new Matrix4f(false);
            kMat.M00 = kTransform.M00;
            kMat.M01 = kTransform.M01;
            kMat.M02 = 0;
            kMat.M03 = kTransform.M02;            

            kMat.M10 = kTransform.M10;
            kMat.M11 = kTransform.M11;
            kMat.M12 = 0;
            kMat.M13 = kTransform.M12;
            kMat.transpose();
            return kMat;            
        }
        kTransform.transpose();
        return kTransform;
    }
    
    public void display(GLAutoDrawable arg0) {
        ((OpenGLRenderer)m_pkRenderer).SetDrawable( arg0 );
        if ( m_bDispose )
        {
            m_bDispose = false;
            dispose(arg0);
            return;
        }
        if ( !m_bInit )
        {
            init(arg0);
        }
        if ( !m_bDisplay )
        {
            return;
        }
        if ( m_bCleanUp )
        {
            cleanUp();
            OnResize(m_iWidth,m_iHeight);
            CreateScene(arg0);
            m_bCleanUp = false;
        }
        m_bDisplay = false;
        if ( m_bCalcLineMin )
        {
            m_bCalcLineMin = false;
            calcLineMin();
        }
        else
        {
            calcEntropy();
        }
    }
    
    public void dispose()
    {
        if ( !m_bDisposeComplete )
        {
            m_bDisposeComplete = true;
            GetCanvas().getContext().makeCurrent();
            m_bDispose = true; 
            display(GetCanvas());  
        }
    }

    public void dispose(GLAutoDrawable arg0)
    {

        cleanUp();
        if ( m_kTextureA != null )
        {
            m_kTextureA.Release(m_pkRenderer);
            m_kTextureA.GetImage().dispose();
            m_kTextureA.dispose();
            m_kTextureA = null;
        }
        if ( m_kTextureB != null )
        {
            m_kTextureB.Release(m_pkRenderer);
            m_kTextureB.GetImage().dispose();
            m_kTextureB.dispose();
            m_kTextureB = null;
        }

        if ( m_kBracketPoints != null )
        {
            m_pkRenderer.ReleaseResources(m_kBracketPoints);
            m_kBracketPoints.dispose();
            m_kBracketPoints = null;
        }
        if ( m_kTransformPoints != null )
        {
            m_pkRenderer.ReleaseResources(m_kTransformPoints);
            m_kTransformPoints.dispose();
            m_kTransformPoints = null;
        }
        
        if ( m_kImageEffectDual != null )
        {
            m_pkRenderer.ReleaseResources(m_kImageEffectDual);
            m_kImageEffectDual.dispose();
        }
        if ( m_kImageLineMinDual != null )
        {
            m_pkRenderer.ReleaseResources(m_kImageLineMinDual);
            m_kImageLineMinDual.dispose();
        }
        if ( m_kCalcTransform != null )
        {
            m_pkRenderer.ReleaseResources(m_kCalcTransform);
            m_kCalcTransform.dispose();
        }

        if ( m_kBracketOut != null )
        {
            m_kBracketOut.GetTarget(0).GetImage().dispose();
            m_kBracketOut.GetTarget(0).dispose();
            m_kBracketOut.TerminateBuffer();
            m_kBracketOut.dispose();
        }
        if ( m_kTransformOut != null )
        {
            m_kTransformOut.GetTarget(0).GetImage().dispose();
            m_kTransformOut.GetTarget(0).dispose();
            m_kTransformOut.TerminateBuffer();
            m_kTransformOut.dispose();
        }
        if ( m_kBracketNewOut != null )
        {
            m_kBracketNewOut.GetTarget(0).GetImage().dispose();
            m_kBracketNewOut.GetTarget(0).dispose();
            m_kBracketNewOut.TerminateBuffer();
            m_kBracketNewOut.dispose();
        }
    
        ImageCatalog.GetActive().dispose();
                
        super.dispose();
    }
    public float[] getBracketB()
    {
        return m_afBracketB;
    }

    public GLCanvas GetCanvas()
    {
        return ((OpenGLRenderer)m_pkRenderer).GetCanvas();
    }

    public double getError()
    {
        //System.err.println( "GetError " + m_dHxy + " " + m_dHx + " " + m_dHy );
        return m_dHxy / (m_dHx + m_dHy);
    }
    
    public void init(GLAutoDrawable arg0) {
        ((OpenGLRenderer)m_pkRenderer).SetDrawable( arg0 );
        //m_kGLAutoDrawable = arg0; 
        if ( m_bInit )
        {
            return;
        }
        
        for ( int i = 0; i < 16; i++ )
        {
            m_afStartPoint[i] = 0f;
            m_afPt[i] = 0f;
            m_afUnitDirections[i] = 0f;
        }

        m_pkRenderer.InitializeState();
        m_pkRenderer.SetBackgroundColor(new ColorRGBA(0,0,0,0));
        OnResize(m_iWidth,m_iHeight);

        CreateImageTextures();
        //m_spkScene = new Node();
        //CreateScene();

        // initial update of objects
        //m_spkScene.UpdateGS();
        //m_spkScene.UpdateRS();


        //((OpenGLRenderer)m_pkRenderer).ClearDrawable( );
        m_bInit = true;
        m_spkCamera = null;
        m_pkRenderer.Activate();
    }
    
    public void initImages( ModelSimpleImage kImageA, ModelSimpleImage kImageB, int iNBins )
    {
        //System.err.println( "initImages" );
        //m_iWidth = kImageA.extents[0];
        //m_iHeight = kImageA.extents[1];
        
        m_iWidth = iNBins;
        m_iHeight = iNBins;
        m_kImageA = kImageA;
        m_kImageB = kImageB;
        if ( m_bInit )
        {
            m_bCleanUp = true;
        }
    }
    
    public void initLineMin(Matrix4f kToOrigin, Matrix4f kFromOrigin,
            float rigid, float dim, double[] startPoint, double[] pt, int ptLength,
            double[] unitDirections, double unit_tolerance, double fMinDist,
            double bracketA, double functionA,
            double bracketB, double functionB,
            double bracketC, double functionC )
    {
        m_fUnitTolerance = (float)unit_tolerance;
        m_kToOrigin = convertTo4D(kToOrigin);
        m_kFromOrigin = convertTo4D(kFromOrigin);
        m_fRigid = rigid;
        m_fDim = dim;

        for ( int i = 0; i < startPoint.length; i++ )
        {
            m_afStartPoint[i] = (float)startPoint[i];
        }
        for ( int i = 0; i < unitDirections.length; i++ )
        {
            m_afUnitDirections[i] = (float)unitDirections[i];
        }
        for ( int i = 0; i < pt.length; i++ )
        {
            m_afPt[i] = (float)pt[i];
        }
        m_fPtLength = ptLength;
        m_fMinDist = (float)fMinDist;

        if ( m_kImageLineMinDual != null )
        {
            m_kImageLineMinDual.updateParameters( m_kToOrigin, m_kFromOrigin, 
                    m_fRigid, m_fDim, m_afStartPoint, m_afPt, m_fPtLength, m_afUnitDirections, m_fUnitTolerance, m_fMinDist );
        }
        if ( m_kCalcTransform != null )
        {
            m_kCalcTransform.updateParameters( m_kToOrigin, m_kFromOrigin, 
                    m_fRigid, m_fDim, m_afStartPoint, m_afPt, m_fPtLength, m_afUnitDirections, m_fUnitTolerance, m_fMinDist );
        }
        if ( m_kImageLineMinPass2a != null )
        {
            m_kImageLineMinPass2a.setMinDist(m_fMinDist);
        }

        int index = 0;
        m_afBracket[index++] = (float)bracketA;
        m_afBracket[index++] = (float)functionA;
        m_afBracket[index++] = -1f;
        m_afBracket[index++] = 0f;
        
        m_afBracket[index++] = (float)bracketB;
        m_afBracket[index++] = (float)functionB;
        m_afBracket[index++] = 0f;
        m_afBracket[index++] = 0f;
        
        m_afBracket[index++] = (float)bracketC;
        m_afBracket[index++] = (float)functionC;
        m_afBracket[index++] = 0f;
        m_afBracket[index++] = 0f;
        
        if ( m_kBracketOut != null )
        {
            if ( m_kBracketOut.GetTarget(0) != null )
            {
                if ( m_kBracketOut.GetTarget(0).GetImage() != null )
                {
                    m_kBracketOut.GetTarget(0).GetImage().SetData( m_afBracket, 1, 3 );
                    m_kBracketOut.GetTarget(0).Reload(true);
                }
            }
        }
    }
    
    public void reshape(GLAutoDrawable arg0, int iX, int iY, int iWidth, int iHeight){}

    public void SetFrame( JFrame kFrame )
    {
        m_kFrame = kFrame;
    }
    
    public void setFromOrigin( TransMatrix kFromOrigin )
    {
        m_kFromOrigin = convertTo4D(kFromOrigin);
        m_kFromOriginInv.copy(m_kFromOrigin).inverse();
        
    }
    
    public void setToOrigin( TransMatrix kToOrigin ) 
    {
        m_kToOrigin = convertTo4D(kToOrigin);
        m_kToOriginInv.copy(m_kToOrigin).inverse();
    }

    public void setTransform( TransMatrix kTransform )
    {
        if ( kTransform.getDim() == 3 )
        {
            m_kImageTransform.identity();
            m_kImageTransform.M00 = kTransform.M00;
            m_kImageTransform.M01 = kTransform.M01;
            m_kImageTransform.M02 = 0;
            m_kImageTransform.M03 = kTransform.M02;
            

            m_kImageTransform.M10 = kTransform.M10;
            m_kImageTransform.M11 = kTransform.M11;
            m_kImageTransform.M12 = 0;
            m_kImageTransform.M13 = kTransform.M12;
        }
        else
        {
            m_kImageTransform.copy(kTransform);
        }
        if ( m_kImageEffectDual != null )
        {
            /*
            Matrix4f toOrigin = new Matrix4f(false);
            Matrix4f fromOrigin = new Matrix4f(false);
            toOrigin.M03 = -m_kImageA.extents[0]/2.0f;
            toOrigin.M13 = -m_kImageA.extents[1]/2.0f;
            fromOrigin.M03 = m_kImageA.extents[0]/2.0f;
            fromOrigin.M13 = m_kImageA.extents[1]/2.0f;
            TransMatrix kMat = new TransMatrix(4,4);
            kMat.setTransform(0,0,0,0,0,15,1,1,1,0,0,0);
            m_kImageTransform.Copy(kMat);
            System.err.println( "captureImage" + m_iScreenCaptureCounter + " " + m_kImageTransform.ToString() );

            m_kImageTransform.MakeIdentity();
            m_kImageTransform.M00 = 0.9666009f;
            m_kImageTransform.M01 = -0.25924882f;
            m_kImageTransform.M02 = 0f;
            m_kImageTransform.M03 = 74.82431f;
            m_kImageTransform.M10 = 0.25845572f;
            m_kImageTransform.M11 = 0.9654881f;
            m_kImageTransform.M12 = 0f;
            m_kImageTransform.M13 = -57.23043f;
            System.err.println( m_kImageTransform.ToString() );
            m_kImageTransform.Inverse();
            System.err.println( m_kImageTransform.ToString() );
            
            m_kImageTransform.MakeIdentity();
            m_kImageTransform.M00 = 0.9652507f;
            m_kImageTransform.M01 = 0.25918508f;
            m_kImageTransform.M02 = 0f;
            m_kImageTransform.M03 = -56.043213f;
            m_kImageTransform.M10 = -0.25839218f;
            m_kImageTransform.M11 = 0.96636325f;
            m_kImageTransform.M12 = 0f;
            m_kImageTransform.M13 = 72.88661f;
            */
            //m_kImageTransform.MultLeft( m_kToOriginInv );
            //m_kImageTransform.Mult( m_kFromOriginInv );
            

            //System.err.println( "setTransform " + m_kImageTransform.ToString() );
            
            //m_kImageTransform.Inverse();

            //m_kImageTransform.MultLeft( m_kToOrigin );
            //m_kImageTransform.Mult( m_kFromOrigin );
            
            
            //m_kImageTransform.MultLeft(fromOrigin);
            //m_kImageTransform.Mult(toOrigin);
            m_kImageEffectDual.SetTransform(m_kImageTransform);
            //System.err.println( m_kImageTransform.ToString() );
        }
    }
    
    public void setTransform( TransMatrixd kTransform )
    {
        if ( kTransform.getDim() == 3 )
        {
            m_kImageTransformd.identity();
            m_kImageTransformd.M00 = kTransform.M00;
            m_kImageTransformd.M01 = kTransform.M01;
            m_kImageTransformd.M02 = 0;
            m_kImageTransformd.M03 = kTransform.M02;
            

            m_kImageTransformd.M10 = kTransform.M10;
            m_kImageTransformd.M11 = kTransform.M11;
            m_kImageTransformd.M12 = 0;
            m_kImageTransformd.M13 = kTransform.M12;
        }
        else
        {
            m_kImageTransformd.copy(kTransform);
        }
        if ( m_kImageEffectDual != null )
        {
            /*
            Matrix4f toOrigin = new Matrix4f(false);
            Matrix4f fromOrigin = new Matrix4f(false);
            toOrigin.M03 = -m_kImageA.extents[0]/2.0f;
            toOrigin.M13 = -m_kImageA.extents[1]/2.0f;
            fromOrigin.M03 = m_kImageA.extents[0]/2.0f;
            fromOrigin.M13 = m_kImageA.extents[1]/2.0f;
            TransMatrix kMat = new TransMatrix(4,4);
            kMat.setTransform(0,0,0,0,0,15,1,1,1,0,0,0);
            m_kImageTransform.Copy(kMat);
            System.err.println( "captureImage" + m_iScreenCaptureCounter + " " + m_kImageTransform.ToString() );

            m_kImageTransform.MakeIdentity();
            m_kImageTransform.M00 = 0.9666009f;
            m_kImageTransform.M01 = -0.25924882f;
            m_kImageTransform.M02 = 0f;
            m_kImageTransform.M03 = 74.82431f;
            m_kImageTransform.M10 = 0.25845572f;
            m_kImageTransform.M11 = 0.9654881f;
            m_kImageTransform.M12 = 0f;
            m_kImageTransform.M13 = -57.23043f;
            System.err.println( m_kImageTransform.ToString() );
            m_kImageTransform.Inverse();
            System.err.println( m_kImageTransform.ToString() );
            
            m_kImageTransform.MakeIdentity();
            m_kImageTransform.M00 = 0.9652507f;
            m_kImageTransform.M01 = 0.25918508f;
            m_kImageTransform.M02 = 0f;
            m_kImageTransform.M03 = -56.043213f;
            m_kImageTransform.M10 = -0.25839218f;
            m_kImageTransform.M11 = 0.96636325f;
            m_kImageTransform.M12 = 0f;
            m_kImageTransform.M13 = 72.88661f;
            */
            //m_kImageTransform.MultLeft( m_kToOriginInv );
            //m_kImageTransform.Mult( m_kFromOriginInv );
            

            //System.err.println( "setTransform " + m_kImageTransform.ToString() );
            
            //m_kImageTransform.Inverse();

            //m_kImageTransform.MultLeft( m_kToOrigin );
            //m_kImageTransform.Mult( m_kFromOrigin );
            
            
            //m_kImageTransform.MultLeft(fromOrigin);
            //m_kImageTransform.Mult(toOrigin);
            m_kImageEffectDual.SetTransform(m_kImageTransformd);
            //System.err.println( m_kImageTransform.ToString() );
        }
    }


    public boolean writeImage()
    {
        BufferedImage kScreenShot = m_pkRenderer.Screenshot();
        try {
            ImageIO.write(kScreenShot, "jpg", new File("captureImage" + m_iScreenCaptureCounter++ + "." + "jpg"));
        } catch (IOException e) {
            e.printStackTrace();
        }
        return true;
    }
    
    protected void CreateBracketMesh()
    {
        //System.err.println( iWidth + " " + iHeight + " " + iDepth );
        Attributes kAttributes = new Attributes();
        kAttributes.SetPChannels(3);
        
        int iVQuantity = 3;
        VertexBuffer pkVB = new VertexBuffer(kAttributes,iVQuantity);

        // generate geometry

        float[] afChannels = new float[iVQuantity];
        int iIndex = 0;
        afChannels[iIndex++] = 0; afChannels[iIndex++] = -1.0f; afChannels[iIndex++] = 0.0f;        
        afChannels[iIndex++] = 0; afChannels[iIndex++] = 0.0f; afChannels[iIndex++] = 0.5f;        
        afChannels[iIndex++] = 0; afChannels[iIndex++] = 0.95f; afChannels[iIndex++] = 1.0f;
        pkVB.SetPosition( afChannels );
        
        m_kBracketPoints = new Polypoint( pkVB );        
        AlphaState kAlpha = new AlphaState();
        kAlpha.BlendEnabled = true;
        kAlpha.SrcBlend = AlphaState.SrcBlendMode.SBF_ONE;
        kAlpha.DstBlend = AlphaState.DstBlendMode.DBF_ONE;
        m_kBracketPoints.AttachGlobalState(kAlpha);
        m_kBracketPoints.UpdateGS();
        m_kBracketPoints.UpdateRS();    
    }
    
    protected void CreateHistogramMesh(int iWidth, int iHeight)
    {
        Attributes kAttributes = new Attributes();
        kAttributes.SetPChannels(3);
        
        int iVQuantity = iWidth*iHeight;
        VertexBuffer pkVB = new VertexBuffer(kAttributes,iVQuantity);

        // generate geometry
        float fInv0 = 1.0f/(iWidth);
        float fInv1 = 1.0f/(iHeight);
        float fU, fV;
        int i0, i1;

        float[] afChannels = new float[iVQuantity];
        int iIndex = 0;
        
        for (i1 = 0; i1 < iHeight; i1++)
        {
            fV = i1*fInv1;
            for (i0 = 0; i0 < iWidth; i0++)
            {
                fU = i0*fInv0;
                afChannels[iIndex++] = ((2.0f*fU-1.0f));
                afChannels[iIndex++] = ((2.0f*fV-1.0f));
                afChannels[iIndex++] = 0f;
                
                //System.err.println( ((2.0f*fU-1.0f)) + " " + ((2.0f*fV-1.0f)));
            }
        }
        pkVB.SetPosition( afChannels );
        m_kHistogramPoints2D = new Polypoint( pkVB );
        m_kEntropyPoints2D = new Polypoint( pkVB );
        
        AlphaState kAlpha = new AlphaState();
        kAlpha.BlendEnabled = true;
        kAlpha.SrcBlend = AlphaState.SrcBlendMode.SBF_ONE;
        kAlpha.DstBlend = AlphaState.DstBlendMode.DBF_ONE;

        m_kHistogramPoints2D.AttachGlobalState(kAlpha);
        m_kHistogramPoints2D.UpdateGS();
        m_kHistogramPoints2D.UpdateRS();    
        
        m_kEntropyPoints2D.AttachGlobalState(kAlpha);
        m_kEntropyPoints2D.UpdateGS();
        m_kEntropyPoints2D.UpdateRS();            
    }
    
    protected ResourceIdentifier CreateImageMesh(int iWidth, int iHeight, int iDepth)
    {
        //System.err.println( iWidth + " " + iHeight + " " + iDepth );
        Attributes kAttributes = new Attributes();
        kAttributes.SetPChannels(3);
        
        int iVQuantity = iWidth*iHeight*iDepth;
        VertexBuffer pkVB = new VertexBuffer(kAttributes,iVQuantity);

        // generate geometry
        float fInv0 = 1.0f/(iWidth - 1.0f);
        float fInv1 = 1.0f/(iHeight - 1.0f);
        float fInv2 = 1.0f/(iDepth);
        float fU, fV, fW;
        int i0, i1, i2;

        float[] afChannels = new float[iVQuantity];
        int iIndex = 0;
        
        for (i2 = 0; i2 < iDepth; i2++ )
        {
            fW = i2*fInv2;
            for (i1 = 0; i1 < iHeight; i1++)
            {
                fV = i1*fInv1;
                for (i0 = 0; i0 < iWidth; i0++)
                {
                    fU = i0*fInv0;
                    afChannels[iIndex++] = ((2.0f*fU-1.0f));
                    afChannels[iIndex++] = ((2.0f*fV-1.0f));
                    afChannels[iIndex++] = ((2.0f*fW-1.0f));
                }
            }
        }
        pkVB.SetPosition( afChannels );
        
        m_kImagePointsDual = new Polypoint( pkVB );
        m_kAlpha = new AlphaState();
        m_kAlpha.BlendEnabled = true;
        m_kAlpha.SrcBlend = AlphaState.SrcBlendMode.SBF_ONE;
        m_kAlpha.DstBlend = AlphaState.DstBlendMode.DBF_ONE;
        m_kImagePointsDual.AttachGlobalState(m_kAlpha);
        m_kImagePointsDual.UpdateGS();
        m_kImagePointsDual.UpdateRS();    
        

        return m_pkRenderer.LoadVAO( m_kImagePointsDual );
        //return m_pkRenderer.LoadVBuffer( pkVB.GetAttributes(), pkVB.GetAttributes(), pkVB );
    }
    
    
    protected void CreateScene (GLAutoDrawable arg0)
    {           
        int iWidth = m_iWidth;
        int iHeight = m_iHeight;

        int iDepth = m_kImageA.nDims == 3 ? m_kImageA.extents[2] : 1;
        m_kHistogramOutput = CreateRenderTarget( arg0, "Histogram2D", iWidth, iHeight );
        m_kHistogramOutputB = CreateRenderTarget( arg0, "Histogram2DB", iWidth, iHeight  );
        m_kEntropyOut = CreateRenderTarget( arg0, "EntropyOut", 1, 1  );
                
        if ( CreateImageMesh(m_kImageA.extents[0],m_kImageA.extents[1],iDepth) == null )
        {
            CreateImageMesh( m_kImageA.extents[0],m_kImageA.extents[1], 1 );
            m_iRenderLoops = iDepth;
            System.err.println( "Switching to 2.5D" );
        }
        m_kImagePointsDual.AttachEffect(m_kImageEffectDual);
        m_kImageEffectDual.SetImageSize( m_kImageA.extents[0],m_kImageA.extents[1],iDepth );
        
        
        CreateHistogramMesh(m_iWidth, m_iHeight);
        m_akCollapse2D = new VolumeHistogramEffect( m_kHistogramOutput.GetTarget(0), VolumeHistogramEffect.NONE );
        m_akCollapseColumns = new VolumeHistogramEffect( m_kHistogramOutput.GetTarget(0), VolumeHistogramEffect.COLLAPSE_COLUMNS );
        m_akCollapseRows = new VolumeHistogramEffect( m_kHistogramOutput.GetTarget(0), VolumeHistogramEffect.COLLAPSE_ROWS );

        m_kHistogramPoints2D.AttachEffect(m_akCollapse2D);
        AlphaState pkAState = m_akCollapse2D.GetBlending(0);
        pkAState.BlendEnabled = true;
        pkAState.SrcBlend = AlphaState.SrcBlendMode.SBF_ONE;
        pkAState.DstBlend = AlphaState.DstBlendMode.DBF_ONE;
        
        m_kHistogramPoints2D.AttachEffect(m_akCollapseColumns);
        pkAState = m_akCollapseColumns.GetBlending(0);
        pkAState.BlendEnabled = true;
        pkAState.SrcBlend = AlphaState.SrcBlendMode.SBF_ONE;
        pkAState.DstBlend = AlphaState.DstBlendMode.DBF_ONE;
        
        m_kHistogramPoints2D.AttachEffect(m_akCollapseRows);
        pkAState = m_akCollapseRows.GetBlending(0);
        pkAState.BlendEnabled = true;
        pkAState.SrcBlend = AlphaState.SrcBlendMode.SBF_ONE;
        pkAState.DstBlend = AlphaState.DstBlendMode.DBF_ONE;
        
        double dSize = m_kImageA.dataSize;
        m_akImageReduceEntropy = new ImageReduceEffect( m_kHistogramOutputB.GetTarget(0), dSize );
        m_kEntropyPoints2D.AttachEffect( m_akImageReduceEntropy );     
/*
        m_kImageLineMinPass2a = new LineMinimizationEffect( m_kBracketOut.GetTarget(0), m_kEntropyOut.GetTarget(0), m_fMinDist, (float)dSize, m_kImageA.nDims  );
        m_kImageLineMinDual.SetImageSize( m_kImageA.extents[0],m_kImageA.extents[1],iDepth );
  */
    }

    private void calcEntropy()
    {        
        calcEntropy( m_kImageA, m_kImageA.dataSize ); 

        int nVoxels = m_kImageA.dataSize;
        if ( ( (m_kImageA.nDims < 3) && (m_dOverlap > 1000) ) ||
                ( (m_kImageA.nDims == 3) && (m_dOverlap > (0.15 * nVoxels)) ) ) {
            double nRatio = (nVoxels) / m_dOverlap;

            m_dHx  = (nRatio * m_dHx) - Math.log(nRatio);
            m_dHy  = (nRatio * m_dHy) - Math.log(nRatio);
            m_dHxy = (nRatio * m_dHxy) - Math.log(nRatio);
        } else {
            m_dHx = Math.log(nVoxels);
            m_dHy = Math.log(nVoxels);
            m_dHxy = 2.0 * Math.log(nVoxels);
        }
    }
    
    private void calcEntropy( ModelSimpleImage kImage, double dNumSamples )
    {
        m_kImagePointsDual.DetachAllEffects();
        m_kImagePointsDual.AttachEffect( m_kImageEffectDual );
        m_pkRenderer.Resize(m_kHistogramOutput.GetTarget(0).GetImage().GetBound(0),
                m_kHistogramOutput.GetTarget(0).GetImage().GetBound(1));
        m_kHistogramOutput.Enable();
        m_pkRenderer.ClearColorDepth();
        //m_pkRenderer.ClearBuffers();
        int iDepth = m_kImageA.nDims == 3 ? m_kImageA.extents[2] : 1;
        float fInv2 = 1.0f/(iDepth);
        float fW;
        for ( int i = 0; i < m_iRenderLoops; i++ )
        {
            if ( m_iRenderLoops == 1 )
            {
                m_kImageEffectDual.ZSlice(0f);
            }
            else
            {
                fW = i*fInv2;
                m_kImageEffectDual.ZSlice(((2.0f*fW-1.0f)));
                m_kImageEffectDual.UseZSlice();
            }
            m_pkRenderer.Draw(m_kImagePointsDual);
        }
        //writeImage();
        m_kHistogramOutput.Disable();
        //printTarget( "HistoOut" , m_kHistogramOutput.GetTarget(0) );

        m_pkRenderer.Resize(m_kHistogramOutputB.GetTarget(0).GetImage().GetBound(0),
                m_kHistogramOutputB.GetTarget(0).GetImage().GetBound(1));
        m_kHistogramOutputB.Enable();
        m_pkRenderer.ClearColorDepth();
        //m_pkRenderer.ClearBuffers();

        //m_kHistogramPoints2D.DetachAllEffects();
        //m_kHistogramPoints2D.AttachEffect(m_akCollapse2D);
        m_pkRenderer.Draw(m_kHistogramPoints2D);

        //m_kHistogramPoints2D.DetachAllEffects();
        //m_kHistogramPoints2D.AttachEffect(m_akCollapseColumns);
        //m_pkRenderer.Draw(m_kHistogramPoints2D);

        //m_kHistogramPoints2D.DetachAllEffects();
        //m_kHistogramPoints2D.AttachEffect(m_akCollapseRows);
        //m_pkRenderer.Draw(m_kHistogramPoints2D);
        
        //m_pkRenderer.Draw(m_kHistogramPoints2D);
        m_kHistogramOutputB.Disable();
        //printTarget( "HistoOut" , m_kHistogramOutputB.GetTarget(0) );
        ReduceDualA(dNumSamples);        
    }
    
    private void calcLineMin()
    {
        m_kImagePointsDual.DetachAllEffects();
        m_kImagePointsDual.AttachEffect( m_kImageLineMinDual );
        boolean bEarly = false;
        Texture kTarget = null;
        OpenGLFrameBuffer kCurrentBracket = m_kBracketOut;
        OpenGLFrameBuffer kNewBracket = m_kBracketNewOut;
        OpenGLFrameBuffer kTempBracket;
        for ( int i = 0; i < 100; i++ )
        {
            //m_kCalcTransform.SetTexture( kCurrentBracket.GetTarget(0), 0, 0 );
            //m_kImageLineMinPass2a.SetTexture( kCurrentBracket.GetTarget(0), 0, 0 );
            
            // 1. Create the transform matrix based on the current bracket.
            m_pkRenderer.Resize(m_kTransformOut.GetTarget(0).GetImage().GetBound(0),
                    m_kTransformOut.GetTarget(0).GetImage().GetBound(1));
            m_kTransformOut.Enable();
            m_pkRenderer.ClearColorDepth();
            //m_pkRenderer.ClearBuffers();
            m_pkRenderer.Draw(m_kTransformPoints);
            m_kTransformOut.Disable();    
            /*
            kTarget = m_kTransformOut.GetTarget(0);
            m_pkRenderer.GetTexImage( kTarget );
            for ( int j = 0; j < kTarget.GetImage().GetFloatData().length; j++ )
            {
                System.err.print( kTarget.GetImage().GetFloatData()[j] + " " );
            }
            System.err.println( " " ); */
            
            // 1. Render all image points w/LineMin1 shaders
            m_pkRenderer.Resize(m_kHistogramOutput.GetTarget(0).GetImage().GetBound(0),
                    m_kHistogramOutput.GetTarget(0).GetImage().GetBound(1));
            m_kHistogramOutput.Enable();
            m_pkRenderer.ClearColorDepth();
            //m_pkRenderer.ClearBuffers();
            m_pkRenderer.Draw(m_kImagePointsDual);
            m_kHistogramOutput.Disable();

            // 2. Render Histogram points
            m_pkRenderer.Resize(m_kHistogramOutputB.GetTarget(0).GetImage().GetBound(0),
                    m_kHistogramOutputB.GetTarget(0).GetImage().GetBound(1));
            m_kHistogramOutputB.Enable();
            m_pkRenderer.ClearColorDepth();
            //m_pkRenderer.ClearBuffers();
            m_pkRenderer.Draw(m_kHistogramPoints2D);
            m_kHistogramOutputB.Disable();
            
            // 3. Render Entropy points w/LineMin1 shader
            m_pkRenderer.Resize(m_kEntropyOut.GetTarget(0).GetImage().GetBound(0),
                    m_kEntropyOut.GetTarget(0).GetImage().GetBound(1));
            m_kEntropyOut.Enable();
            m_pkRenderer.ClearColorDepth();
            //m_pkRenderer.ClearBuffers();
            m_pkRenderer.Draw(m_kEntropyPoints2D);
            m_kEntropyOut.Disable();

            // 4. Render bracket points w/LineMin2 shader
            m_kBracketPoints.DetachAllEffects();
            m_kBracketPoints.AttachEffect(m_kImageLineMinPass2a);
            m_pkRenderer.Resize(kNewBracket.GetTarget(0).GetImage().GetBound(0),
                    kNewBracket.GetTarget(0).GetImage().GetBound(1));
            kNewBracket.Enable();
            m_pkRenderer.ClearColorDepth();
            //m_pkRenderer.ClearBuffers();
            m_pkRenderer.Draw(m_kBracketPoints);
            kNewBracket.Disable();            
            kTempBracket = kCurrentBracket;
            kCurrentBracket = kNewBracket;
            kNewBracket = kTempBracket;
            
            if ( (i%6) == 0 )
            {
                //kTarget = m_kBracketOut.GetTarget(0);
                kTarget = kCurrentBracket.GetTarget(0);
                m_pkRenderer.GetTexImage( kTarget );  
                /*
                System.err.println("");
                System.err.println("");
                System.err.println( "GPU BracketA = " + kTarget.GetImage().GetFloatData()[0] + " " + kTarget.GetImage().GetFloatData()[1]);
                System.err.println( "GPU BracketB = " + kTarget.GetImage().GetFloatData()[4] + " " + kTarget.GetImage().GetFloatData()[5]);
                System.err.println( "GPU BracketC = " + kTarget.GetImage().GetFloatData()[8] + " " + kTarget.GetImage().GetFloatData()[9]);                
                System.err.println( "   " + i + " xNew = " + kTarget.GetImage().GetFloatData()[3] + " yNew = " + 
                        kTarget.GetImage().GetFloatData()[6]  + " case: " + kTarget.GetImage().GetFloatData()[7] + " " + 
                        kTarget.GetImage().GetFloatData()[10] + " " + 
                        kTarget.GetImage().GetFloatData()[11]);
                        */
                if ( (Math.abs(kTarget.GetImage().GetFloatData()[8] - kTarget.GetImage().GetFloatData()[0]) <= m_fUnitTolerance) )
                {
                    m_afBracketB[0] = kTarget.GetImage().GetFloatData()[4];
                    m_afBracketB[1] = kTarget.GetImage().GetFloatData()[5];
                    bEarly = true;
                    break;
                }
                if ( kTarget.GetImage().GetFloatData()[3] == 0 )
                {
                    m_afBracketB[0] = kTarget.GetImage().GetFloatData()[4];
                    m_afBracketB[1] = kTarget.GetImage().GetFloatData()[5];
                    bEarly = true;
                    System.err.println( "BREAK EARLY BAD");
                    break;    
                }
            }
        }
        if ( !bEarly )
        {
            //kTarget = m_kBracketOut.GetTarget(0);
            kTarget = kCurrentBracket.GetTarget(0);
            m_pkRenderer.GetTexImage( kTarget );
            m_afBracketB[0] = kTarget.GetImage().GetFloatData()[4];
            m_afBracketB[1] = kTarget.GetImage().GetFloatData()[5];
/*
            System.err.println("");
            System.err.println("");
            System.err.println( "GPU BracketA = " + kTarget.GetImage().GetFloatData()[0] + " " + kTarget.GetImage().GetFloatData()[1]);
            System.err.println( "GPU BracketB = " + kTarget.GetImage().GetFloatData()[4] + " " + kTarget.GetImage().GetFloatData()[5]);
            System.err.println( "GPU BracketC = " + kTarget.GetImage().GetFloatData()[8] + " " + kTarget.GetImage().GetFloatData()[9]);
        */
        }
        //System.err.println( m_afBracketB[0] + " " + m_afBracketB[1] );
    }
    
    private void cleanUp()
    {
        //System.err.println( "VolumeImageViewerPoint dispose()" );
        if ( m_kHistogramPoints2D != null )
        {
            m_pkRenderer.ReleaseResources(m_kHistogramPoints2D);
            m_kHistogramPoints2D.dispose();
            m_kHistogramPoints2D = null;
        }
        if ( m_kEntropyPoints2D != null )
        {
            m_pkRenderer.ReleaseResources(m_kEntropyPoints2D);
            m_kEntropyPoints2D.dispose();
            m_kEntropyPoints2D = null;
        }
        if ( m_kImagePointsDual != null )
        {
            m_kImagePointsDual.DetachAllEffects();
            m_pkRenderer.ReleaseResources(m_kImagePointsDual);
            m_kImagePointsDual.dispose();
            m_kImagePointsDual = null;
        }

        if ( m_kHistogramOutput != null )
        {
            m_kHistogramOutput.GetTarget(0).GetImage().dispose();
            m_kHistogramOutput.GetTarget(0).dispose();
            m_kHistogramOutput.TerminateBuffer();
            m_kHistogramOutput.dispose();
        }
        if ( m_kHistogramOutputB != null )
        {
            m_kHistogramOutputB.GetTarget(0).GetImage().dispose();
            m_kHistogramOutputB.GetTarget(0).dispose();
            m_kHistogramOutputB.TerminateBuffer();
            m_kHistogramOutputB.dispose();
        }
        if ( m_kEntropyOut != null )
        {
            m_kEntropyOut.GetTarget(0).GetImage().dispose();
            m_kEntropyOut.GetTarget(0).dispose();
            m_kEntropyOut.TerminateBuffer();
            m_kEntropyOut.dispose();
        }
        if ( m_akImageReduceEntropy != null )
        {
            m_pkRenderer.ReleaseResources(m_akImageReduceEntropy);
            m_akImageReduceEntropy.dispose();
        }
        if ( m_akCollapse2D != null )
        {
            m_pkRenderer.ReleaseResources(m_akCollapse2D);
            m_akCollapse2D.dispose();
        }
        if ( m_akCollapseColumns != null )
        {
            m_pkRenderer.ReleaseResources(m_akCollapseColumns);
            m_akCollapseColumns.dispose();
        }
        if ( m_akCollapseRows != null )
        {
            m_pkRenderer.ReleaseResources(m_akCollapseRows);
            m_akCollapseRows.dispose();
        }
    }
    
    private void CreateImageTextures()
    {   
        m_kTextureA = new Texture();
        m_kTextureA.SetImage(VolumeImage.UpdateData(m_kTarget, m_kNameA ));
        m_kTextureA.SetShared(true);
        m_kTextureA.SetBorderColor( new ColorRGBA(0,0,0,0) );
        m_kTextureA.SetFilterType(Texture.FilterType.NEAREST);
        m_kTextureA.SetWrapType(0,Texture.WrapType.CLAMP_BORDER);
        m_kTextureA.SetWrapType(1,Texture.WrapType.CLAMP_BORDER);
        m_kTextureA.SetWrapType(2,Texture.WrapType.CLAMP_BORDER);           
        if ( !m_pkRenderer.LoadTexture( m_kTextureA ) )
        {
            m_bStatus = false;
        }
        
        m_kTextureB = new Texture();
        m_kTextureB.SetImage(VolumeImage.UpdateData(m_kMoving, m_kNameB ));
        m_kTextureB.SetShared(true);
        m_kTextureB.SetBorderColor( new ColorRGBA(0,0,0,0) );
        m_kTextureB.SetFilterType(Texture.FilterType.NEAREST);
        m_kTextureB.SetWrapType(0,Texture.WrapType.CLAMP_BORDER);
        m_kTextureB.SetWrapType(1,Texture.WrapType.CLAMP_BORDER);
        m_kTextureB.SetWrapType(2,Texture.WrapType.CLAMP_BORDER);           
        if ( !m_pkRenderer.LoadTexture( m_kTextureB ) )
        {
            m_bStatus = false;
        }

        m_kImageEffectDual = new VolumeHistogramEffect( m_kTextureA, m_kTextureB,
                m_kTarget.min, m_kTarget.max, m_kMoving.min, m_kMoving.max, 
                m_kTarget.extents[0],m_kTarget.extents[1], m_kImageTransform );
        /*
        m_kBracketOut = CreateRenderTargetInit( "bracketImage", 1, 3 );   

        CreateTransformMesh();
        m_kTransformOut = CreateRenderTargetInit( "Transform", 1, 4 );
        //m_kTransformNewOut = CreateRenderTargetInit( "TransformNew", 1, 4 );
        m_kBracketOut.GetTarget(0).GetImage().SetData( m_afBracket, 1, 4 );
        m_kCalcTransform = new LineMinimizationEffect( m_kBracketOut.GetTarget(0),
                (m_kTarget.nDims == 2),
                m_kToOrigin, m_kFromOrigin,
                m_fRigid, m_fDim, m_afStartPoint, m_afPt, m_fPtLength,
                m_afUnitDirections, m_fUnitTolerance, m_fMinDist );
        m_kTransformPoints.AttachEffect(m_kCalcTransform);           
        
        m_kImageLineMinDual = new LineMinimizationEffect( m_kTextureA, m_kTextureB, m_kTransformOut.GetTarget(0),
                m_kTarget.min, m_kTarget.max, m_kMoving.min, m_kMoving.max, 
                m_kTarget.extents[0],m_kTarget.extents[1] );
        
        CreateBracketMesh();
        m_kBracketNewOut = CreateRenderTargetInit( "bracketNew", 1, 3 );
        */
    }
    
    

    private OpenGLFrameBuffer CreateRenderTarget( GLAutoDrawable arg0, String kImageName, int iWidth, int iHeight )
    {      
        //System.err.println( kImageName + " " + iWidth + " " + iHeight );
        float[] afData = new float[iWidth*iHeight*4];
        GraphicsImage pkSceneImage = new GraphicsImage(GraphicsImage.FormatMode.IT_RGBA32,iWidth,iHeight,afData,
                kImageName);

        Texture[] akSceneTarget = new Texture[1];
        akSceneTarget[0] = new Texture();
        akSceneTarget[0].SetImage(pkSceneImage);
        akSceneTarget[0].SetShared(false);
        akSceneTarget[0].SetFilterType(Texture.FilterType.NEAREST);
        akSceneTarget[0].SetWrapType(0,Texture.WrapType.CLAMP);
        akSceneTarget[0].SetWrapType(1,Texture.WrapType.CLAMP);
        m_pkRenderer.LoadTexture( akSceneTarget[0] );
        OpenGLFrameBuffer kFB = new OpenGLFrameBuffer(m_eFormat,m_eDepth,m_eStencil,
                m_eBuffering,m_eMultisampling,m_pkRenderer, arg0 );
        kFB.InitializeBuffer(akSceneTarget);
        return kFB;
    }
        
    private OpenGLFrameBuffer CreateRenderTargetInit( GLAutoDrawable arg0, String kImageName, int iWidth, int iHeight )
    {      
        float[] afData = new float[iWidth*iHeight*4];
        for ( int i = 0; i < afData.length; i++ )
        {
            afData[i] = -1;
        }
        GraphicsImage pkSceneImage = new GraphicsImage(GraphicsImage.FormatMode.IT_RGBA32,iWidth,iHeight,afData,
                kImageName);

        Texture[] akSceneTarget = new Texture[1];
        akSceneTarget[0] = new Texture();
        akSceneTarget[0].SetImage(pkSceneImage);
        akSceneTarget[0].SetShared(false);
        akSceneTarget[0].SetFilterType(Texture.FilterType.NEAREST);
        akSceneTarget[0].SetWrapType(0,Texture.WrapType.CLAMP);
        akSceneTarget[0].SetWrapType(1,Texture.WrapType.CLAMP);
        m_pkRenderer.LoadTexture( akSceneTarget[0] );
        OpenGLFrameBuffer kFB = new OpenGLFrameBuffer(m_eFormat,m_eDepth,m_eStencil,
                m_eBuffering,m_eMultisampling,m_pkRenderer, arg0 );
        kFB.InitializeBuffer(akSceneTarget);
        return kFB;

    }

    private void CreateTransformMesh()
    {
        //System.err.println( iWidth + " " + iHeight + " " + iDepth );
        Attributes kAttributes = new Attributes();
        kAttributes.SetPChannels(3);
        
        int iVQuantity = 4;
        VertexBuffer pkVB = new VertexBuffer(kAttributes,iVQuantity);

        // generate geometry

        float[] afChannels = new float[iVQuantity];
        int iIndex = 0;
        afChannels[iIndex++] = 0; afChannels[iIndex++] = -1.0f; afChannels[iIndex++] = 0.0f;        
        afChannels[iIndex++] = 0; afChannels[iIndex++] = -0.34f; afChannels[iIndex++] = 0.3f;        
        afChannels[iIndex++] = 0; afChannels[iIndex++] = 0.34f; afChannels[iIndex++] = 0.7f;  
        afChannels[iIndex++] = 0; afChannels[iIndex++] = 0.95f; afChannels[iIndex++] = 1.0f;
        pkVB.SetPosition( afChannels );
        
        
        m_kTransformPoints = new Polypoint( pkVB );        
        AlphaState kAlpha = new AlphaState();
        kAlpha.BlendEnabled = true;
        kAlpha.SrcBlend = AlphaState.SrcBlendMode.SBF_ONE;
        kAlpha.DstBlend = AlphaState.DstBlendMode.DBF_ONE;
        m_kTransformPoints.AttachGlobalState(kAlpha);
        m_kTransformPoints.UpdateGS();
        m_kTransformPoints.UpdateRS();    
    }

    private void printTarget( String kMsg, Texture kTarget )
    {
        m_pkRenderer.GetTexImage( kTarget );
        float[] afData = kTarget.GetImage().GetFloatData();
        System.err.println( kMsg + "   TEXTURE NAME = " + kTarget.GetName() + " size = " +
                kTarget.GetImage().GetBound(0) + " " + kTarget.GetImage().GetBound(1));
        int iSum = 0;
        float fMax = 0, fMin = 1;
        int iCount = 0;
        for ( int i = 0; i < afData.length; i+=4 )
        {
            //if ( afData[i] != 0.0f )
            {
                System.err.print( afData[i] + " " + afData[i+1] + " " + afData[i+2] + " " + afData[i+3] + "  " );
                iCount++;
            }
            if ( ((iCount)%(m_iWidth) == 0) )
            {
                System.err.println("");
            }
            if ( afData[i] > fMax )
            {
                fMax = afData[i];
            }
            if ( afData[i] < fMin )
            {
                fMin = afData[i];
            }
            iSum += afData[i+3];
        }
        //System.err.println( "TOTAL: " + iSum + " " + fMin + " " + fMax );
    }
    
    private void ReduceDualA( double dNumSamples )
    { 
        Texture kTarget = null;
        m_pkRenderer.Resize(m_kEntropyOut.GetTarget(0).GetImage().GetBound(0),
                m_kEntropyOut.GetTarget(0).GetImage().GetBound(1));
        m_kEntropyOut.Enable();
        m_pkRenderer.ClearColorDepth();
        //m_pkRenderer.ClearBuffers();
        m_pkRenderer.Draw(m_kEntropyPoints2D);
        m_kEntropyOut.Disable();
        kTarget = m_kEntropyOut.GetTarget(0);
        
        double dEntropyDual = 0;
        double dOverlap = 0;
        double dEntropyX = 0, dEntropyY = 0;
        if ( kTarget != null )
        {
            int iIndex = 0;
            int iStep = (int)Math.max( 1.0, (int)(kTarget.GetImage().GetBytesPerPixel()/4.0f) );
            m_pkRenderer.GetTexImage( kTarget );
            

            //System.err.println( "Entropy = " + kTarget.GetImage().GetFloatData()[0] + " "
            //        + kTarget.GetImage().GetFloatData()[1] + " "
            //        + kTarget.GetImage().GetFloatData()[2] + " "
            //        + kTarget.GetImage().GetFloatData()[3] );
            
            for ( int i = 0; i < kTarget.GetImage().GetBound(1); i++  )
            {
                for ( int j = 0; j < kTarget.GetImage().GetBound(0); j++  )
                {
                    dEntropyDual += kTarget.GetImage().GetFloatData()[iIndex];
                    dEntropyY += kTarget.GetImage().GetFloatData()[iIndex+1];
                    dEntropyX += kTarget.GetImage().GetFloatData()[iIndex+2];
                    dOverlap += kTarget.GetImage().GetFloatData()[iIndex+3];
                    iIndex += iStep;
                }
            }
        }            
        dEntropyX = dEntropyX/dNumSamples;
        dEntropyY = dEntropyY/dNumSamples;
        
        m_dOverlap = dOverlap;
        m_dHx = dEntropyX;
        m_dHy = dEntropyY;
        m_dHxy = dEntropyDual/dNumSamples;
        //System.err.println( "GPU: " + m_iWidth + " " + dNumSamples + " " + m_dOverlap + " " + m_dHx + " " + m_dHy + " " + m_dHxy );
    }
}
