package gov.nih.mipav.view.renderer.WildMagic.Render;

import gov.nih.mipav.MipavInitGPU;
import gov.nih.mipav.model.structures.ModelSimpleImage;
import gov.nih.mipav.model.structures.TransMatrix;

import java.awt.image.BufferedImage;
import java.io.File;
import java.io.IOException;

import javax.imageio.ImageIO;
import javax.media.opengl.GLAutoDrawable;
import javax.media.opengl.GLCanvas;
import javax.media.opengl.GLContext;
import javax.swing.JFrame;

import WildMagic.LibApplications.OpenGLApplication.JavaApplication3D;
import WildMagic.LibFoundation.Mathematics.ColorRGBA;
import WildMagic.LibFoundation.Mathematics.Matrix4f;
import WildMagic.LibGraphics.Rendering.AlphaState;
import WildMagic.LibGraphics.Rendering.GraphicsImage;
import WildMagic.LibGraphics.Rendering.Texture;
import WildMagic.LibGraphics.SceneGraph.Attributes;
import WildMagic.LibGraphics.SceneGraph.Node;
import WildMagic.LibGraphics.SceneGraph.Polypoint;
import WildMagic.LibGraphics.SceneGraph.TriMesh;
import WildMagic.LibGraphics.SceneGraph.VertexBuffer;
import WildMagic.LibGraphics.Shaders.CompiledProgramCatalog;
import WildMagic.LibGraphics.Shaders.ImageCatalog;
import WildMagic.LibGraphics.Shaders.PixelProgramCatalog;
import WildMagic.LibGraphics.Shaders.SamplerInformation;
import WildMagic.LibGraphics.Shaders.VertexProgramCatalog;
import WildMagic.LibRenderers.OpenGLRenderer.OpenGLFrameBuffer;
import WildMagic.LibRenderers.OpenGLRenderer.OpenGLRenderer;

import com.mentorgen.tools.profile.runtime.Profile;

public class VolumeImageViewerPoint extends JavaApplication3D
//implements GLEventListener, KeyListener
{
    protected static int m_iScreenCaptureCounter = 0;
    protected Node m_spkScene;    

    protected VolumeHistogramEffect m_akTransformImage;
    protected VolumeHistogramEffect m_akCollapse2D;
    protected VolumeHistogramEffect m_akCollapseColumns;
    protected VolumeHistogramEffect m_akCollapseRows;
    
    protected ImageReduceEffect m_akImageReduceEntropy;
    protected VolumeHistogramEffect m_kImageEffectDual;
    protected LineMinimizationEffect m_kImageLineMinPass2a;
    protected LineMinimizationEffect m_kImageLineMinPass2b;
    //protected LineMinimizationEffect m_kImageLineMinEntropy;
    protected LineMinimizationEffect m_kImageLineMinDual;
    protected LineMinimizationEffect m_kCalcTransform;

    protected TriMesh m_pkPlane;
    private Polypoint m_kImagePointsDual;
    private Polypoint m_kHistogramPoints2D;
    private Polypoint m_kEntropyPoints2D;
    private Polypoint m_kBracketPoints;
    private Polypoint m_kTransformPoints;

    private OpenGLFrameBuffer m_kHistogramOutput;
    private OpenGLFrameBuffer m_kHistogramOutputB;
    private OpenGLFrameBuffer m_kEntropyOut;

    private OpenGLFrameBuffer m_kTransformOut;
    private OpenGLFrameBuffer m_kBracketOut;
    private OpenGLFrameBuffer m_kBracketNewOut;
    
    private int m_iCount = 0;
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

    protected JFrame m_kFrame;
    

    private Matrix4f m_kToOrigin = Matrix4f.IDENTITY;
    private Matrix4f m_kFromOrigin = Matrix4f.IDENTITY;
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
    
    public VolumeImageViewerPoint( ModelSimpleImage kTarget, ModelSimpleImage kMoving )
    {
        //super( "VolumeImageViewer", 0, 0, kImageA.extents[0],kImageA.extents[1],
        super( "VolumeImageViewer", 0, 0, 256, 256,
                new ColorRGBA( 0.0f,0.0f,0.0f,1.0f ) );
        m_pkRenderer = new OpenGLRenderer( m_eFormat, m_eDepth, m_eStencil,
                m_eBuffering, m_eMultisampling,
                m_iWidth, m_iHeight );
        GetCanvas().getContext().setSynchronized(true);  
        m_kTarget = kTarget;
        m_kMoving = kMoving;
        String kExternalDirs = MipavInitGPU.getExternalDirs();        
        ImageCatalog.SetActive( new ImageCatalog("Main", kExternalDirs) );      
        VertexProgramCatalog.SetActive(new VertexProgramCatalog("Main", kExternalDirs));       
        PixelProgramCatalog.SetActive(new PixelProgramCatalog("Main", kExternalDirs));
        CompiledProgramCatalog.SetActive(new CompiledProgramCatalog());
    } 
    public VolumeImageViewerPoint( ModelSimpleImage kImageA, ModelSimpleImage kImageB, int iNBins )
    {
        //super( "VolumeImageViewer", 0, 0, kImageA.extents[0],kImageA.extents[1],
        super( "VolumeImageViewer", 0, 0, iNBins, iNBins,
                new ColorRGBA( 0.0f,0.0f,0.0f,1.0f ) );
        m_pkRenderer = new OpenGLRenderer( m_eFormat, m_eDepth, m_eStencil,
                m_eBuffering, m_eMultisampling,
                m_iWidth, m_iHeight );
        GetCanvas().getContext().setSynchronized(true);  
        
        m_kTarget = m_kImageA = kImageA;
        m_kMoving = m_kImageB = kImageB;
        
        String kExternalDirs = MipavInitGPU.getExternalDirs();        
        ImageCatalog.SetActive( new ImageCatalog("Main", kExternalDirs) );      
        VertexProgramCatalog.SetActive(new VertexProgramCatalog("Main", kExternalDirs));       
        PixelProgramCatalog.SetActive(new PixelProgramCatalog("Main", kExternalDirs));
        CompiledProgramCatalog.SetActive(new CompiledProgramCatalog());
    }
    
    public static VolumeImageViewerPoint create( ModelSimpleImage kTarget, ModelSimpleImage kMoving )
    {
        final VolumeImageViewerPoint kWorld = new VolumeImageViewerPoint( kTarget, kMoving );
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
        }
        if ( !kWorld.checkStatus() )
        {
            return null;
        }
        return kWorld;
    }
    
    /**
     * @param args
     */
    public static VolumeImageViewerPoint create( ModelSimpleImage kImageA, ModelSimpleImage kImageB, boolean bShowFrame, int iNBins )
    {
        final VolumeImageViewerPoint kWorld = new VolumeImageViewerPoint(kImageA, kImageB, iNBins);
        final JFrame frame = new JFrame(kWorld.GetWindowTitle());
        frame.add( kWorld.GetCanvas() );
        
        // setting the frame to be undecorated removes the frame title bar and edges
        // this prevents flashing on-screen.
        frame.setUndecorated(!bShowFrame);
        // frame must be set to visible for the gl canvas to be properly initialized.
        frame.setVisible(true);
        frame.setBounds(0,0,
                kWorld.GetWidth(), kWorld.GetHeight() );
        frame.setVisible(bShowFrame);
        kWorld.SetFrame(frame);
        return kWorld;
    }
    
    public void calcError(TransMatrix kTransform) {
        if ( GetCanvas().getContext().makeCurrent() == GLContext.CONTEXT_NOT_CURRENT )
        {
            System.err.println( "Context not made current" );
        }
        setTransform(kTransform);
        m_bDisplay = true;
        display(GetCanvas());  
        GetCanvas().getContext().release();
    }
    
    public boolean checkStatus()
    {
        return m_bStatus;
    }
    
/*
    public void calcLineMinimization()
    {
        if ( GetCanvas().getContext().makeCurrent() == GLContext.CONTEXT_NOT_CURRENT )
        {
            System.err.println( "Context not made current" );
        }

        m_bDisplay = true;
        m_bCalcLineMin = true;
        display(GetCanvas());  
        GetCanvas().getContext().release();
        
        TransMatrix kMat = new TransMatrix(4,4);
        kMat.Copy( m_kImageTransform );
        kMat.Transpose();
        calcError( new TransMatrix(kMat) );
        getError();
    }
    */
    public void display(GLAutoDrawable arg0) {
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
            CreateScene();
            m_bCleanUp = false;
        }
        m_bDisplay = false;
        /*
        if ( m_bCalcLineMin )
        {
            m_bCalcLineMin = false;
            //calcLineMin();
        }
        else
        {
        */
            calcEntropy();
            /*
        }
        */
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

        if ( m_kTextureA != null )
        {
            m_kTextureA.GetImage().dispose();
            m_kTextureA.dispose();
            m_kTextureA = null;
        }
        if ( m_kTextureB != null )
        {
            m_kTextureB.GetImage().dispose();
            m_kTextureB.dispose();
            m_kTextureB = null;
        }

        m_kImageEffectDual.dispose();
        cleanUp();
    
        ImageCatalog.GetActive().dispose();
        VertexProgramCatalog.GetActive().dispose();     
        PixelProgramCatalog.GetActive().dispose();
        CompiledProgramCatalog.GetActive().dispose();
                
        super.dispose();
    }

/*
    public float[] getBracketB()
    {
        return m_afBracketB;
    }
*/
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
        
        GetCanvas().setAutoSwapBufferMode( false );

        m_pkRenderer.InitializeState();
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
    /*
    public void initLineMin(Matrix4f kToOrigin, Matrix4f kFromOrigin,
            float rigid, float dim, double[] startPoint, double[] pt, int ptLength,
            double[] unitDirections, double fMinDist,
            double bracketA, double functionA,
            double bracketB, double functionB,
            double bracketC, double functionC )
    {
        
        m_kToOrigin = convertTo4D(kToOrigin);
        m_kFromOrigin = convertTo4D(kFromOrigin);
        m_fRigid = rigid;
        m_fDim = dim;
        for ( int i = 0; i < pt.length; i++ )
        {
            m_afStartPoint[i] = (float)startPoint[i];
            m_afPt[i] = (float)pt[i];
            m_afUnitDirections[i] = (float)unitDirections[i];
        }
        m_fPtLength = ptLength;
        m_fMinDist = (float)fMinDist;

        if ( m_kImageLineMinDual != null )
        {
            m_kImageLineMinDual.updateParameters( m_kToOrigin, m_kFromOrigin, 
                    m_fRigid, m_fDim, m_afStartPoint, m_afPt, m_fPtLength, m_afUnitDirections, m_fMinDist );
        }
        if ( m_kCalcTransform != null )
        {
            m_kCalcTransform.updateParameters( m_kToOrigin, m_kFromOrigin, 
                    m_fRigid, m_fDim, m_afStartPoint, m_afPt, m_fPtLength, m_afUnitDirections, m_fMinDist );
        }

        int index = 0;
        m_afBracket[index++] = (float)bracketA;
        m_afBracket[index++] = (float)functionA;
        m_afBracket[index++] = 0f;
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
    */

    public void reshape(GLAutoDrawable arg0, int iX, int iY, int iWidth, int iHeight){}
    
    public void SetFrame( JFrame kFrame )
    {
        m_kFrame = kFrame;
    }
    
    /*
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
            return kMat;            
        }
        return kTransform;
    }
    */
    public void setTransform( TransMatrix kTransform )
    {
        if ( kTransform.getDim() == 3 )
        {
            m_kImageTransform.MakeIdentity();
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
            m_kImageTransform.Copy(kTransform);
        }
        if ( m_kImageEffectDual != null )
        {
            m_kImageEffectDual.SetTransform(m_kImageTransform);
        }
        if ( m_akTransformImage != null )
        {
            m_akTransformImage.SetTransform(m_kImageTransform);
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
/*
    protected void CreateBracketMesh()
    {
        //System.err.println( iWidth + " " + iHeight + " " + iDepth );
        Attributes kAttributes = new Attributes();
        kAttributes.SetPChannels(3);
        
        int iVQuantity = 3;
        VertexBuffer pkVB = new VertexBuffer(kAttributes,iVQuantity);

        // generate geometry

        float[] afChannels = pkVB.GetData();
        int iIndex = 0;
        afChannels[iIndex++] = 0; afChannels[iIndex++] = -1.0f; afChannels[iIndex++] = 0.0f;        
        afChannels[iIndex++] = 0; afChannels[iIndex++] = 0.0f; afChannels[iIndex++] = 0.5f;        
        afChannels[iIndex++] = 0; afChannels[iIndex++] = 0.95f; afChannels[iIndex++] = 1.0f;
        
        
        m_kBracketPoints = new Polypoint( pkVB );        
        AlphaState kAlpha = new AlphaState();
        kAlpha.BlendEnabled = true;
        kAlpha.SrcBlend = AlphaState.SrcBlendMode.SBF_ONE;
        kAlpha.DstBlend = AlphaState.DstBlendMode.DBF_ONE;
        m_kBracketPoints.AttachGlobalState(kAlpha);
        m_kBracketPoints.UpdateGS();
        m_kBracketPoints.UpdateRS();    
    }
    
    private void CreateTransformMesh()
    {
        //System.err.println( iWidth + " " + iHeight + " " + iDepth );
        Attributes kAttributes = new Attributes();
        kAttributes.SetPChannels(3);
        
        int iVQuantity = 3;
        VertexBuffer pkVB = new VertexBuffer(kAttributes,iVQuantity);

        // generate geometry

        float[] afChannels = pkVB.GetData();
        int iIndex = 0;
        afChannels[iIndex++] = 0; afChannels[iIndex++] = -1.0f; afChannels[iIndex++] = 0.0f;        
        afChannels[iIndex++] = 0; afChannels[iIndex++] = 0.0f; afChannels[iIndex++] = 0.5f;        
        afChannels[iIndex++] = 0; afChannels[iIndex++] = 0.95f; afChannels[iIndex++] = 1.0f;
        
        
        m_kTransformPoints = new Polypoint( pkVB );        
        AlphaState kAlpha = new AlphaState();
        kAlpha.BlendEnabled = true;
        kAlpha.SrcBlend = AlphaState.SrcBlendMode.SBF_ONE;
        kAlpha.DstBlend = AlphaState.DstBlendMode.DBF_ONE;
        m_kTransformPoints.AttachGlobalState(kAlpha);
        m_kTransformPoints.UpdateGS();
        m_kTransformPoints.UpdateRS();    
    }
*/
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

        float[] afChannels = pkVB.GetData();
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
            }
        }
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
    
    
    protected void CreateImageMesh(int iWidth, int iHeight, int iDepth)
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

        float[] afChannels = pkVB.GetData();
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
        
        m_kImagePointsDual = new Polypoint( pkVB );
        m_kAlpha = new AlphaState();
        m_kAlpha.BlendEnabled = true;
        m_kAlpha.SrcBlend = AlphaState.SrcBlendMode.SBF_ONE;
        m_kAlpha.DstBlend = AlphaState.DstBlendMode.DBF_ONE;
        m_kImagePointsDual.AttachGlobalState(m_kAlpha);
        m_kImagePointsDual.UpdateGS();
        m_kImagePointsDual.UpdateRS();    
    }

    protected void CreateScene ()
    {           
        int iWidth = m_iWidth;
        int iHeight = m_iHeight;

        int iDepth = m_kImageA.nDims == 3 ? m_kImageA.extents[2] : 1;
        m_kHistogramOutput = CreateRenderTarget( "Histogram2D", iWidth, iHeight, 1 );
        m_kHistogramOutputB = CreateRenderTarget( "Histogram2DB", iWidth, iHeight, 1  );
        m_kEntropyOut = CreateRenderTarget( "EntropyOut", 1, 1, 1  );
                
        CreateImageMesh(m_kImageA.extents[0],m_kImageA.extents[1],iDepth);
        m_kImagePointsDual.AttachEffect(m_kImageEffectDual);
        
        
        CreateHistogramMesh(m_iWidth, m_iHeight);
        m_akCollapse2D = new VolumeHistogramEffect( m_kHistogramOutput.GetTarget(0), VolumeHistogramEffect.NONE );
        m_akCollapseColumns = new VolumeHistogramEffect( m_kHistogramOutput.GetTarget(0), VolumeHistogramEffect.COLLAPSE_COLUMNS );
        m_akCollapseRows = new VolumeHistogramEffect( m_kHistogramOutput.GetTarget(0), VolumeHistogramEffect.COLLAPSE_ROWS );

        m_kHistogramPoints2D.AttachEffect(m_akCollapse2D);
        m_kHistogramPoints2D.AttachEffect(m_akCollapseColumns);
        AlphaState pkAState = m_akCollapseColumns.GetBlending(0);
        pkAState.BlendEnabled = true;
        pkAState.SrcBlend = AlphaState.SrcBlendMode.SBF_ONE;
        pkAState.DstBlend = AlphaState.DstBlendMode.DBF_ONE;
        
        m_kHistogramPoints2D.AttachEffect(m_akCollapseRows);
        pkAState = m_akCollapseRows.GetBlending(0);
        pkAState.BlendEnabled = true;
        pkAState.SrcBlend = AlphaState.SrcBlendMode.SBF_ONE;
        pkAState.DstBlend = AlphaState.DstBlendMode.DBF_ONE;
        
        double dSize = m_kImageA.dataSize;
        m_akImageReduceEntropy = new ImageReduceEffect( m_kHistogramOutputB.GetTarget(0), null, dSize );
        m_kEntropyPoints2D.AttachEffect( m_akImageReduceEntropy );     

        //m_kImageLineMinPass2a = new LineMinimizationEffect( m_kBracketOut.GetTarget(0), m_kEntropyOut.GetTarget(0), m_fMinDist, (float)dSize, m_kImageA.nDims  );
        //m_kImageLineMinPass2b = new LineMinimizationEffect( m_kBracketNewOut.GetTarget(0) );
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
        //m_kImagePointsDual.DetachAllEffects();
        //m_kImagePointsDual.AttachEffect( m_kImageEffectDual );
        m_pkRenderer.Resize(m_kHistogramOutput.GetTarget(0).GetImage().GetBound(0),
                m_kHistogramOutput.GetTarget(0).GetImage().GetBound(1));
        m_kHistogramOutput.Enable();
        m_pkRenderer.SetBackgroundColor(new ColorRGBA(0,0,0,0));
        m_pkRenderer.ClearBuffers();
        m_pkRenderer.Draw(m_kImagePointsDual);
        //writeImage();
        m_kHistogramOutput.Disable();

        m_pkRenderer.Resize(m_kHistogramOutputB.GetTarget(0).GetImage().GetBound(0),
                m_kHistogramOutputB.GetTarget(0).GetImage().GetBound(1));
        m_kHistogramOutputB.Enable();
        m_pkRenderer.SetBackgroundColor(new ColorRGBA(0,0,0,0));
        m_pkRenderer.ClearBuffers();
        m_pkRenderer.Draw(m_kHistogramPoints2D);
        m_kHistogramOutputB.Disable();
        ReduceDualA(dNumSamples);        
    }
    /*
    private void calcLineMin()
    {
        m_kImagePointsDual.DetachAllEffects();
        m_kImagePointsDual.AttachEffect( m_kImageLineMinDual );
        //m_kEntropyPoints2D.DetachAllEffects();
        //m_kEntropyPoints2D.AttachEffect( m_kImageLineMinEntropy );
        Texture kTarget = null;
        Matrix4f kMat = new Matrix4f(false);
        //for ( int i = 0; i < 100; i++ )
        {
            // 1. Create the transform matrix based on the current bracket.
            m_pkRenderer.Resize(m_kTransformOut.GetTarget(0).GetImage().GetBound(0),
                    m_kTransformOut.GetTarget(0).GetImage().GetBound(1));
            m_kTransformOut.Enable();
            m_pkRenderer.SetBackgroundColor(new ColorRGBA(0,0,0,0));
            m_pkRenderer.ClearBuffers();
            m_pkRenderer.Draw(m_kTransformPoints);
            m_kTransformOut.Disable();
            kTarget = m_kTransformOut.GetTarget(0);
            m_pkRenderer.GetTexImage( kTarget );
            System.err.println( " " );
            for ( int i = 0; i < kTarget.GetImage().GetFloatData().length; i++ )
            {
                System.err.print( kTarget.GetImage().GetFloatData()[i] + " " );
            }
            System.err.println( " " );
            kMat.M00 = kTarget.GetImage().GetFloatData()[0];
            kMat.M01 = kTarget.GetImage().GetFloatData()[1];
            kMat.M02 = kTarget.GetImage().GetFloatData()[2];
            kMat.M03 = kTarget.GetImage().GetFloatData()[3];
            
            kMat.M10 = kTarget.GetImage().GetFloatData()[4];
            kMat.M11 = kTarget.GetImage().GetFloatData()[5];
            kMat.M12 = kTarget.GetImage().GetFloatData()[6];
            kMat.M13 = kTarget.GetImage().GetFloatData()[7];
            
            kMat.M20 = kTarget.GetImage().GetFloatData()[8];
            kMat.M21 = kTarget.GetImage().GetFloatData()[9];
            kMat.M22 = kTarget.GetImage().GetFloatData()[10];
            kMat.M23 = kTarget.GetImage().GetFloatData()[11];
            
            
            // 1. Render all image points w/LineMin1 shaders
            m_pkRenderer.Resize(m_kHistogramOutput.GetTarget(0).GetImage().GetBound(0),
                    m_kHistogramOutput.GetTarget(0).GetImage().GetBound(1));
            m_kHistogramOutput.Enable();
            m_pkRenderer.SetBackgroundColor(new ColorRGBA(0,0,0,0));
            m_pkRenderer.ClearBuffers();
            m_pkRenderer.Draw(m_kImagePointsDual);
            //writeImage();
            m_kHistogramOutput.Disable();

            // 2. Render Histogram points
            m_pkRenderer.Resize(m_kHistogramOutputB.GetTarget(0).GetImage().GetBound(0),
                    m_kHistogramOutputB.GetTarget(0).GetImage().GetBound(1));
            m_kHistogramOutputB.Enable();
            m_pkRenderer.SetBackgroundColor(new ColorRGBA(0,0,0,0));
            m_pkRenderer.ClearBuffers();
            m_pkRenderer.Draw(m_kHistogramPoints2D);
            m_kHistogramOutputB.Disable();
            //kTarget = m_kHistogramOutputB.GetTarget(0);
            //m_pkRenderer.GetTexImage( kTarget );

            // 3. Render Entropy points w/LineMin1 shader
            m_pkRenderer.Resize(m_kEntropyOut.GetTarget(0).GetImage().GetBound(0),
                    m_kEntropyOut.GetTarget(0).GetImage().GetBound(1));
            m_kEntropyOut.Enable();
            m_pkRenderer.SetBackgroundColor(new ColorRGBA(0,0,0,0));
            m_pkRenderer.ClearBuffers();
            m_pkRenderer.Draw(m_kEntropyPoints2D);
            //ColorRGBA kResult = m_pkRenderer.GetPixelColor(0, 0);
            m_kEntropyOut.Disable();
            kTarget = m_kEntropyOut.GetTarget(0);
            m_pkRenderer.GetTexImage( kTarget );
            System.err.println( "Entropy = " + kTarget.GetImage().GetFloatData()[0] + " "
                    + kTarget.GetImage().GetFloatData()[1] + " "
                    + kTarget.GetImage().GetFloatData()[2] + " "
                    + kTarget.GetImage().GetFloatData()[3] );

            // 4. Render bracket points w/LineMin2 shader
            m_kBracketPoints.DetachAllEffects();
            m_kBracketPoints.AttachEffect(m_kImageLineMinPass2a);
            m_pkRenderer.Resize(m_kBracketNewOut.GetTarget(0).GetImage().GetBound(0),
                    m_kBracketNewOut.GetTarget(0).GetImage().GetBound(1));
            m_kBracketNewOut.Enable();
            m_pkRenderer.SetBackgroundColor(new ColorRGBA(0,0,0,0));
            m_pkRenderer.ClearBuffers();
            m_pkRenderer.Draw(m_kBracketPoints);
            m_pkRenderer.FrameBufferToTexture( m_kBracketOut.GetTarget(0) );
            m_kBracketNewOut.Disable();
            kTarget = m_kBracketNewOut.GetTarget(0);
            m_pkRenderer.GetTexImage( kTarget );
            System.err.println( "Entropy = " + kTarget.GetImage().GetFloatData()[3] + " "
                    + kTarget.GetImage().GetFloatData()[6] + " "
                    + kTarget.GetImage().GetFloatData()[7] + " "
                    + kTarget.GetImage().GetFloatData()[10] );
            
            
            // 5. Render bracket points w/pass-through
            //m_kBracketPoints.DetachAllEffects();
            //m_kBracketPoints.AttachEffect(m_kImageLineMinPass2b);
            //m_pkRenderer.Resize(m_kBracketOut.GetTarget(0).GetImage().GetBound(0),
            //        m_kBracketOut.GetTarget(0).GetImage().GetBound(1));
            //m_kBracketOut.Enable();
            //m_pkRenderer.SetBackgroundColor(new ColorRGBA(0,0,0,0));
            //m_pkRenderer.ClearBuffers();
            //m_pkRenderer.Draw(m_kBracketPoints);
            //m_kBracketOut.Disable();
        }
        kTarget = m_kBracketOut.GetTarget(0);
        m_pkRenderer.GetTexImage( kTarget );
        m_afBracketB[0] = kTarget.GetImage().GetFloatData()[4];
        m_afBracketB[1] = kTarget.GetImage().GetFloatData()[5];
        System.err.println( "GPU BracketA = " + kTarget.GetImage().GetFloatData()[0] + " " + kTarget.GetImage().GetFloatData()[1]);
        System.err.println( "BracketB = " + kTarget.GetImage().GetFloatData()[4] + " " + kTarget.GetImage().GetFloatData()[5]);
        System.err.println( "BracketC = " + kTarget.GetImage().GetFloatData()[8] + " " + kTarget.GetImage().GetFloatData()[9]);
        System.err.println( "yNew = " + kTarget.GetImage().GetFloatData()[2] );
        

        System.err.println( "Entropy = " + kTarget.GetImage().GetFloatData()[3] + " "
                + kTarget.GetImage().GetFloatData()[6] + " "
                + kTarget.GetImage().GetFloatData()[7] + " "
                + kTarget.GetImage().GetFloatData()[10] );

        m_kImageTransform.Copy(kMat);
    }
    */
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
            //m_pkRenderer.ReleaseResources(m_kImagePointsDual);
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
        //m_kTextureA.SetSamplerInformation( new SamplerInformation( m_kNameA, 0, 0 ) );
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
       // m_kTextureB.SetSamplerInformation( new SamplerInformation( m_kNameB, 0, 0 ) );
        //m_pkRenderer.LoadTexture( m_kTextureB );
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
        m_kTransformOut = CreateRenderTargetInit( "Transform", 1, 3 );
        m_kTransformOut.GetTarget(0).GetImage().SetData( m_afBracket, 1, 3 );
        m_kCalcTransform = new LineMinimizationEffect( m_kBracketOut.GetTarget(0), 
                m_kToOrigin, m_kFromOrigin,
                m_fRigid, m_fDim, m_afStartPoint, m_afPt, m_fPtLength,
                m_afUnitDirections, m_fMinDist );
        m_kTransformPoints.AttachEffect(m_kCalcTransform);           
        
        m_kImageLineMinDual = new LineMinimizationEffect( m_kTextureA, m_kTextureB, m_kTransformOut.GetTarget(0),
                m_kTarget.min, m_kTarget.max, m_kMoving.min, m_kMoving.max, 
                m_kTarget.extents[0],m_kTarget.extents[1] );
        
        CreateBracketMesh();
        m_kBracketNewOut = CreateRenderTargetInit( "bracketNew", 1, 3 );
        */
    }
        
    private OpenGLFrameBuffer CreateRenderTarget( String kImageName, int iWidth, int iHeight, int iDepth )
    {      
        float[] afData = new float[iWidth*iHeight*iDepth*4];

        if ( iDepth == 1 )
        {
            GraphicsImage pkSceneImage = new GraphicsImage(GraphicsImage.FormatMode.IT_RGBA32,iWidth,iHeight,afData,
                    kImageName);

            Texture[] akSceneTarget = new Texture[1];
            akSceneTarget[0] = new Texture();
            akSceneTarget[0].SetImage(pkSceneImage);
            akSceneTarget[0].SetShared(false);
            akSceneTarget[0].SetFilterType(Texture.FilterType.NEAREST);
            akSceneTarget[0].SetWrapType(0,Texture.WrapType.CLAMP);
            akSceneTarget[0].SetWrapType(1,Texture.WrapType.CLAMP);
            //akSceneTarget[0].SetSamplerInformation( new SamplerInformation( kImageName, 0, 0 ) );
            m_pkRenderer.LoadTexture( akSceneTarget[0] );

            //return new OpenGLFrameBuffer(m_eFormat,m_eDepth,m_eStencil,
            //        m_eBuffering,m_eMultisampling,m_pkRenderer,akSceneTarget,m_kGLAutoDrawable);
            return new OpenGLFrameBuffer(m_eFormat,m_eDepth,m_eStencil,
                    m_eBuffering,m_eMultisampling,m_pkRenderer,akSceneTarget,GetCanvas());
        } 
        GraphicsImage pkSceneImage = new GraphicsImage(GraphicsImage.FormatMode.IT_RGBA32,iWidth,iHeight,iDepth,afData,
                kImageName);

        Texture[] akSceneTarget = new Texture[1];
        akSceneTarget[0] = new Texture();
        akSceneTarget[0].SetImage(pkSceneImage);
        akSceneTarget[0].SetShared(false);
        akSceneTarget[0].SetFilterType(Texture.FilterType.NEAREST);
        akSceneTarget[0].SetWrapType(0,Texture.WrapType.CLAMP);
        akSceneTarget[0].SetWrapType(1,Texture.WrapType.CLAMP);
        akSceneTarget[0].SetWrapType(2,Texture.WrapType.CLAMP);
        //akSceneTarget[0].SetSamplerInformation( new SamplerInformation( kImageName, 0, 0 ) );
        m_pkRenderer.LoadTexture( akSceneTarget[0] );

        return new OpenGLFrameBuffer(m_eFormat,m_eDepth,m_eStencil,
                m_eBuffering,m_eMultisampling,m_pkRenderer,akSceneTarget,GetCanvas());
    }
    /*
    private OpenGLFrameBuffer CreateRenderTargetInit( String kImageName, int iWidth, int iHeight )
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
        akSceneTarget[0].SetSamplerInformation( new SamplerInformation( kImageName, SamplerInformation.Type.SAMPLER_2D, 0, 0 ) );
        m_pkRenderer.LoadTexture( akSceneTarget[0] );

        return new OpenGLFrameBuffer(m_eFormat,m_eDepth,m_eStencil,
                m_eBuffering,m_eMultisampling,m_pkRenderer,akSceneTarget,GetCanvas());

    }
*/
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
        Texture kTarget = null; //m_akImageReduceEntropy.GetTexture(0, 0);
        //printTarget( "Result", kTarget );
        //m_kEntropyPoints2D.DetachAllEffects();
        //m_kEntropyPoints2D.AttachEffect(m_akImageReduceEntropy);
        m_pkRenderer.Resize(m_kEntropyOut.GetTarget(0).GetImage().GetBound(0),
                m_kEntropyOut.GetTarget(0).GetImage().GetBound(1));
        m_kEntropyOut.Enable();
        m_pkRenderer.SetBackgroundColor(new ColorRGBA(0,0,0,0));
        m_pkRenderer.ClearBuffers();
        m_pkRenderer.Draw(m_kEntropyPoints2D);
        //ColorRGBA kResult = m_pkRenderer.GetPixelColor(0, 0);
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
                    /*
                    System.err.println( kTarget.GetImage().GetFloatData()[iIndex] + " " + 
                            kTarget.GetImage().GetFloatData()[iIndex+1] + " " +
                            kTarget.GetImage().GetFloatData()[iIndex+2] + " " +
                            kTarget.GetImage().GetFloatData()[iIndex+3]  );
                            */
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