package gov.nih.mipav.view.renderer.WildMagic.Render;

import gov.nih.mipav.MipavInitGPU;
import gov.nih.mipav.model.structures.ModelSimpleImage;
import gov.nih.mipav.model.structures.TransMatrix;
import gov.nih.mipav.view.Preferences;

import java.awt.Frame;
import java.awt.event.KeyListener;
import java.awt.event.WindowAdapter;
import java.awt.event.WindowEvent;
import java.awt.image.BufferedImage;
import java.io.File;
import java.io.IOException;

import javax.imageio.ImageIO;
import javax.media.opengl.GLAutoDrawable;
import javax.media.opengl.GLEventListener;

import WildMagic.LibFoundation.Mathematics.ColorRGB;
import WildMagic.LibFoundation.Mathematics.ColorRGBA;
import WildMagic.LibFoundation.Mathematics.Mathf;
import WildMagic.LibFoundation.Mathematics.Matrix3f;
import WildMagic.LibFoundation.Mathematics.Matrix4f;
import WildMagic.LibFoundation.Mathematics.Vector3f;
import WildMagic.LibFoundation.Mathematics.Vector4f;
import WildMagic.LibGraphics.Effects.ShaderEffect;
import WildMagic.LibGraphics.Rendering.AlphaState;
import WildMagic.LibGraphics.Rendering.GraphicsImage;
import WildMagic.LibGraphics.Rendering.Texture;
import WildMagic.LibGraphics.Rendering.WireframeState;
import WildMagic.LibGraphics.SceneGraph.Attributes;
import WildMagic.LibGraphics.SceneGraph.Geometry;
import WildMagic.LibGraphics.SceneGraph.Node;
import WildMagic.LibGraphics.SceneGraph.Polypoint;
import WildMagic.LibGraphics.SceneGraph.StandardMesh;
import WildMagic.LibGraphics.SceneGraph.Transformation;
import WildMagic.LibGraphics.SceneGraph.TriMesh;
import WildMagic.LibGraphics.Shaders.CompiledProgramCatalog;
import WildMagic.LibGraphics.Shaders.ImageCatalog;
import WildMagic.LibGraphics.Shaders.PixelProgramCatalog;
import WildMagic.LibGraphics.Shaders.SamplerInformation;
import WildMagic.LibGraphics.Shaders.VertexProgramCatalog;
import WildMagic.LibRenderers.OpenGLRenderer.OpenGLFrameBuffer;

import com.sun.opengl.util.Animator;

public class VolumeImageViewerPoint extends VolumeImageViewer
implements GLEventListener, KeyListener
{
    protected ImageReduceEffect[] m_akImageReduceEntropy;
    protected ImageReduceEffect[][] m_akImageReduceSum;
    protected VolumeHistogramEffect m_kImageEffectA;
    protected VolumeHistogramEffect m_kImageEffectB;
    protected VolumeHistogramEffect m_kImageEffectDual;
    protected TriMesh m_kCurrentRender;
    protected TriMesh m_pkCurrentPlane;
    private GLAutoDrawable m_kGLAutoDrawable = null;
    

    private TriMesh[] m_akSumTarget;
    private Polypoint m_kImagePoints;
    private Polypoint m_kImagePointsDual;

    private OpenGLFrameBuffer[] m_kHistogramOutput;
    private OpenGLFrameBuffer[][] m_akReduceOutput;
/*
    private Texture[] m_kHistogramOutput;
    private Texture[][] m_akReduceOutput;
    */
    private int m_iCount = 0;
    private boolean m_bFirst = true;
    private AlphaState m_kAlpha;
    private WireframeState m_kWireframe;
    private ModelSimpleImage m_kImageA;
    private ModelSimpleImage m_kImageB;
    private String m_kNameA = "imageA";
    private String m_kNameB = "imageB";
    private String m_kNameBSingle = "imageBSingle";
    private Texture m_kTextureA;
    private Texture m_kTextureB;
    private Texture m_kTextureBSingle;
    
    private double m_dHx_save;
    private double m_dHx, m_dHy, m_dHxy;
    private double m_dOverlap;
    private Matrix4f m_kImageTransform = new Matrix4f(false);
    private Matrix4f m_kImageTransformInv = new Matrix4f(false);
    

    private boolean m_bCalcImageA = true;
    private boolean m_bCalc2D = true;

    private boolean m_bDispose = false;
    
    public VolumeImageViewerPoint( ModelSimpleImage kImageA, ModelSimpleImage kImageB )
    {
        super( Math.max( kImageA.extents[0], kImageB.extents[0]), 
                Math.max( kImageA.extents[1], kImageB.extents[1]));
        m_kImageA = kImageA;
        m_kImageB = kImageB;
        
        String kExternalDirs = MipavInitGPU.getExternalDirs();        
        ImageCatalog.SetActive( new ImageCatalog("Main", kExternalDirs) );      
        VertexProgramCatalog.SetActive(new VertexProgramCatalog("Main", kExternalDirs));       
        PixelProgramCatalog.SetActive(new PixelProgramCatalog("Main", kExternalDirs));
        CompiledProgramCatalog.SetActive(new CompiledProgramCatalog());
    }
    /**
     * @param args
     */
    public static VolumeImageViewerPoint create( ModelSimpleImage kImageA, ModelSimpleImage kImageB, boolean bShowFrame )
    {
        final VolumeImageViewerPoint kWorld = new VolumeImageViewerPoint(kImageA, kImageB);
        final Frame frame = new Frame(kWorld.GetWindowTitle());
        frame.add( kWorld.GetCanvas() );
        final Animator animator = new Animator( kWorld.GetCanvas() );  
        /*
        frame.addWindowListener(new WindowAdapter() {
            public void windowClosing(WindowEvent e) {
                // Run this on another thread than the AWT event queue to
                // avoid deadlocks on shutdown on some platforms
                new Thread(new Runnable() {
                    public void run() {
                        animator.stop();
                    }
                }).start();
                frame.setVisible(false);
            }
        });
        */
        // setting the frame to be undecorated removes the frame title bar and edges
        // this prevents flashing on-screen.
        frame.setUndecorated(!bShowFrame);
        // frame must be set to visible for the gl canvas to be properly initialized.
        frame.setVisible(true);
        frame.setBounds(0,0,
                kWorld.GetWidth(), kWorld.GetHeight() );
        frame.setVisible(bShowFrame);
        kWorld.SetAnimator(animator);
        kWorld.SetFrame(frame);
        //animator.start();
        //kWorld.GetCanvas().display();
        return kWorld;
    }

    public void display(GLAutoDrawable arg0) {
        if ( m_bDispose )
        {
            dispose(arg0);
            return;
        }
        if ( m_kAnimator == null )
        {
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
        m_bDisplay = false;
        calcEntropy();
    }


    public void dispose()
    {
        m_bDispose = true;
        GetCanvas().display();  
    }

    public void dispose(GLAutoDrawable arg0)
    {
        //System.err.println( "VolumeImageViewerPoint dispose()" );
        if ( m_kImagePoints != null )
        {
            m_pkRenderer.ReleaseResources(m_kImagePoints);
            m_kImagePoints.dispose();
            m_kImagePoints = null;
        }   
        if ( m_kImagePointsDual != null )
        {
            m_pkRenderer.ReleaseResources(m_kImagePointsDual);
            m_kImagePointsDual.dispose();
            m_kImagePointsDual = null;
        }
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
        m_spkScene = null;
        m_pkPlane.dispose();

        m_kImageEffectA.dispose();        
        m_kImageEffectB.dispose();        
        m_kImageEffectDual.dispose();
              

        m_kHistogramOutput[0].GetTarget(0).GetImage().dispose();
        m_kHistogramOutput[0].GetTarget(0).dispose();
        m_kHistogramOutput[0].TerminateBuffer();
        m_kHistogramOutput[0].dispose();
        m_kHistogramOutput[1].GetTarget(0).GetImage().dispose();
        m_kHistogramOutput[1].GetTarget(0).dispose();
        m_kHistogramOutput[1].TerminateBuffer();
        m_kHistogramOutput[1].dispose();

        m_pkRenderer.ReleaseResources(m_akImageReduceEntropy[0]);
        m_akImageReduceEntropy[0].dispose();
        m_pkRenderer.ReleaseResources(m_akImageReduceEntropy[2]);
        m_akImageReduceEntropy[2].dispose();
        m_pkRenderer.ReleaseResources(m_akImageReduceEntropy[1]);
        m_akImageReduceEntropy[1].dispose();
        
        for ( int i = 0; i < (m_iCount-1); i++ )
        {
            m_pkRenderer.ReleaseResources(m_akSumTarget[i]);
            m_akSumTarget[i].dispose();      
            
            m_akReduceOutput[0][i].GetTarget(0).GetImage().dispose();  
            m_akReduceOutput[0][i].GetTarget(0).dispose();   
            m_akReduceOutput[0][i].TerminateBuffer();      
            m_akReduceOutput[0][i].dispose();            
            m_pkRenderer.ReleaseResources(m_akImageReduceSum[0][i]);
            m_akImageReduceSum[0][i].dispose();

            m_akReduceOutput[1][i].GetTarget(0).GetImage().dispose();  
            m_akReduceOutput[1][i].GetTarget(0).dispose();   
            m_akReduceOutput[1][i].TerminateBuffer();   
            m_akReduceOutput[1][i].dispose();            
            m_pkRenderer.ReleaseResources(m_akImageReduceSum[1][i]);
            m_akImageReduceSum[1][i].dispose();
        }
    
        ImageCatalog.GetActive().dispose();
        VertexProgramCatalog.GetActive().dispose();     
        PixelProgramCatalog.GetActive().dispose();
        CompiledProgramCatalog.GetActive().dispose();
        
        super.dispose(arg0);
    }
    
    public double getError(TransMatrix kTransform) {
        setTransform(kTransform);
        m_bDisplay = true;
        if ( m_bFirst )
        {
            m_bFirst = false;
            m_bCalcImageA = true;       
        }
        GetCanvas().display();  
        return m_dHxy / (m_dHx + m_dHy);
    }

    public void init(GLAutoDrawable arg0) {
        m_kGLAutoDrawable = arg0;
        super.init(arg0);
    }
    
    public void reshape(GLAutoDrawable arg0, int iX, int iY, int iWidth, int iHeight){}
    
    public void setTransform( TransMatrix kTransform )
    {
        /*
        Matrix3f kRotate = new Matrix3f( kTransform.M00, kTransform.M01, kTransform.M02,
                kTransform.M10, kTransform.M11, kTransform.M12,
                kTransform.M20, kTransform.M21, kTransform.M22 );
        Vector3f kTranslate = new Vector3f( kTransform.M03, kTransform.M13, kTransform.M23 );
        //System.err.println( "Translation = " + kTranslate.ToString() );
        Transformation kLocalTransform = new Transformation();
        kLocalTransform.SetRotate( kRotate );
        kLocalTransform.SetTranslate( kTranslate );
        kLocalTransform.GetHomogeneous(m_kImageTransform);
        */
        if ( kTransform.getDim() == 3 )
        {
            m_kImageTransformInv.MakeIdentity();
            m_kImageTransformInv.M00 = kTransform.M00;
            m_kImageTransformInv.M01 = kTransform.M01;
            m_kImageTransformInv.M02 = 0;
            m_kImageTransformInv.M03 = kTransform.M02;
            

            m_kImageTransformInv.M10 = kTransform.M10;
            m_kImageTransformInv.M11 = kTransform.M11;
            m_kImageTransformInv.M12 = 0;
            m_kImageTransformInv.M13 = kTransform.M12;
        }
        else
        {
            m_kImageTransformInv.Copy(kTransform);
        }
        //m_kImageTransform.Inverse(m_kImageTransformInv);
        m_kImageTransform.Copy(m_kImageTransformInv);
        if ( m_kImageEffectB != null )
        {
            m_kImageEffectB.SetTransform(m_kImageTransform);
        }
        if ( m_kImageEffectDual != null )
        {
            m_kImageEffectDual.SetTransform(m_kImageTransform);
        }
    }

    protected void CreateImageMesh(int iWidth, int iHeight)
    {
        Attributes kAttributes = new Attributes();
        kAttributes.SetPChannels(3);
        //kAttributes.SetCChannels(0,3);
        kAttributes.SetTChannels(0,3);
        StandardMesh kSM = new StandardMesh(kAttributes);
        //kSM.SetColor( new ColorRGBA( 1.0f, 0.0f, 0.0f, 0.0f ) );
        TriMesh kMesh = kSM.Rectangle(iWidth,iHeight,1.0f,1.0f);
        m_kImagePoints = new Polypoint( kMesh.VBuffer );
        m_kAlpha = new AlphaState();
        m_kAlpha.BlendEnabled = true;
        m_kAlpha.SrcBlend = AlphaState.SrcBlendMode.SBF_ONE;
        m_kAlpha.DstBlend = AlphaState.DstBlendMode.DBF_ONE;
        m_kImagePoints.AttachGlobalState(m_kAlpha);
        m_kImagePoints.UpdateGS();
        m_kImagePoints.UpdateRS();    
        

        //kSM.SetColor( new ColorRGBA( 1.0f, 1.0f, 0.0f, 0.0f ) );
        kMesh = kSM.Rectangle(iWidth,iHeight,1.0f,1.0f);
        m_kImagePointsDual = new Polypoint( kMesh.VBuffer );
        m_kAlpha = new AlphaState();
        m_kAlpha.BlendEnabled = true;
        m_kAlpha.SrcBlend = AlphaState.SrcBlendMode.SBF_ONE;
        m_kAlpha.DstBlend = AlphaState.DstBlendMode.DBF_ONE;
        m_kImagePointsDual.AttachGlobalState(m_kAlpha);
        m_kImagePointsDual.UpdateGS();
        m_kImagePointsDual.UpdateRS();    
    }
    
    protected TriMesh CreateLocalPlaneNode()
    {
        Attributes kAttributes = new Attributes();
        kAttributes.SetPChannels(3);
        kAttributes.SetTChannels(0,3);
        StandardMesh kSM = new StandardMesh(kAttributes);
        TriMesh kPlane = kSM.Rectangle(2,2,1.0f,1.0f);   
        return kPlane;
    }

    
    private void CreateImageTextures()
    {
        SamplerInformation.Type eSamplerType = SamplerInformation.Type.SAMPLER_3D;
        
        m_kTextureA = new Texture();
        m_kTextureA.SetImage(VolumeImage.UpdateData(m_kImageA, 0, m_kNameA ));
        m_kTextureA.SetShared(true);
        m_kTextureA.SetFilterType(Texture.FilterType.LINEAR);
        m_kTextureA.SetWrapType(0,Texture.WrapType.CLAMP_BORDER);
        m_kTextureA.SetWrapType(1,Texture.WrapType.CLAMP_BORDER);
        m_kTextureA.SetWrapType(2,Texture.WrapType.CLAMP_BORDER);                
        m_kTextureA.SetSamplerInformation( new SamplerInformation( m_kNameA, eSamplerType, 0, 0 ) );
        m_pkRenderer.LoadTexture( m_kTextureA );
        
        m_kTextureB = new Texture();
        m_kTextureB.SetImage(VolumeImage.UpdateData(m_kImageB, 0, m_kNameB ));
        m_kTextureB.SetShared(true);
        m_kTextureB.SetFilterType(Texture.FilterType.LINEAR);
        m_kTextureB.SetWrapType(0,Texture.WrapType.CLAMP_BORDER);
        m_kTextureB.SetWrapType(1,Texture.WrapType.CLAMP_BORDER);
        m_kTextureB.SetWrapType(2,Texture.WrapType.CLAMP_BORDER);           
        m_kTextureB.SetSamplerInformation( new SamplerInformation( m_kNameB, eSamplerType, 0, 0 ) );
        m_pkRenderer.LoadTexture( m_kTextureB );
        
        m_kTextureBSingle = new Texture();
        m_kTextureBSingle.SetImage(m_kTextureB.GetImage());
        m_kTextureBSingle.SetShared(true);
        m_kTextureBSingle.SetFilterType(Texture.FilterType.LINEAR);
        m_kTextureBSingle.SetWrapType(0,Texture.WrapType.CLAMP_BORDER);
        m_kTextureBSingle.SetWrapType(1,Texture.WrapType.CLAMP_BORDER);
        m_kTextureBSingle.SetWrapType(2,Texture.WrapType.CLAMP_BORDER);           
        m_kTextureBSingle.SetSamplerInformation( new SamplerInformation( m_kNameBSingle, eSamplerType, 0, 0 ) );
        m_pkRenderer.LoadTexture( m_kTextureBSingle );
    }

    protected void CreateScene ()
    {           
        m_spkScene = new Node();
        m_pkPlane = CreateLocalPlaneNode();
        m_spkScene.AttachChild(m_pkPlane);        
        m_pkPlane.UpdateGS();
        m_pkPlane.UpdateRS();

        CreateImageTextures();
        m_kImageEffectA = new VolumeHistogramEffect( m_kTextureA, 
                m_kImageA.min, m_kImageA.max,
                m_kImageA.extents[0],1 );
        
        m_kImageEffectB = new VolumeHistogramEffect( m_kTextureBSingle, 
                m_kImageB.min, m_kImageB.max, 
                m_kImageB.extents[0],1, m_kImageTransform, true ); //Matrix4f.IDENTITY, false );
        
        m_kImageEffectDual = new VolumeHistogramEffect( m_kTextureA, m_kTextureB,
                m_kImageA.min, m_kImageA.max, m_kImageB.min, m_kImageB.max, 
                m_kImageA.extents[0],m_kImageA.extents[1], m_kImageTransform, true );
        
        CreateImageMesh(m_kImageA.extents[0],m_kImageA.extents[1]);
        

        //int iWidth = Math.max(m_iWidth/2, 1);
        //int iHeight = Math.max(m_iHeight/2, 1);
        int iWidth = m_iWidth;
        int iHeight = m_iHeight;

        //m_kHistogramOutput = new Texture[2];
        m_kHistogramOutput = new OpenGLFrameBuffer[2];
        //m_kHistogramOutput[0] = CreateRenderTarget( "Histogram1D", m_kImageA.extents[0], 1 );
        //m_kHistogramOutput[1] = CreateRenderTarget( "Histogram2D", m_kImageA.extents[0], m_kImageA.extents[1] );
        m_kHistogramOutput[0] = CreateRenderTarget( "Histogram1D", iWidth, 1 );
        m_kHistogramOutput[1] = CreateRenderTarget( "Histogram2D", iWidth, iHeight );

        double dSize = m_kImageA.dataSize;
        m_akImageReduceEntropy = new ImageReduceEffect[3];
        m_akImageReduceEntropy[0] = new ImageReduceEffect( m_kHistogramOutput[0].GetTarget(0), m_kImageA.extents[0], 1, dSize, ImageReduceEffect.ENTROPY );
        m_akImageReduceEntropy[2] = new ImageReduceEffect( m_kHistogramOutput[0].GetTarget(0), m_kImageA.extents[0], 1, dSize, ImageReduceEffect.SUM );
        //dSize *= dSize;
        m_akImageReduceEntropy[1] = new ImageReduceEffect( m_kHistogramOutput[1].GetTarget(0), m_kImageA.extents[0], m_kImageA.extents[1], dSize, ImageReduceEffect.ENTROPY );
        
        

        int iSize = Math.min( m_kImageA.extents[0],m_kImageA.extents[1] );
        m_iCount = (int)Mathf.Log2( iSize );
        //System.err.println( iSize + " " + m_iCount );

        iWidth = Math.max(iWidth/2, 1);
         iHeight = Math.max(iHeight/2, 1);
        m_akImageReduceSum = new ImageReduceEffect[2][m_iCount-1];
        //m_akReduceOutput = new Texture[2][m_iCount-1];
        m_akReduceOutput = new OpenGLFrameBuffer[2][m_iCount-1];
        m_akSumTarget = new TriMesh[m_iCount-1];
        for ( int i = 0; i < (m_iCount-1); i++ )
        {
            String kImageName = new String("Reduce" + iWidth + "_" + 1);
            m_akReduceOutput[0][i] = CreateRenderTarget( kImageName, iWidth, 1 );            
            m_akImageReduceSum[0][i] = new ImageReduceEffect(m_akReduceOutput[0][i].GetTarget(0), iWidth, 1, 0, ImageReduceEffect.SUM );
            
            kImageName = new String("Reduce" + iWidth + "_" + iHeight);
            m_akReduceOutput[1][i] = CreateRenderTarget( kImageName, iWidth, iHeight );            
            m_akImageReduceSum[1][i] = new ImageReduceEffect(m_akReduceOutput[1][i].GetTarget(0), iWidth, iHeight, 0, ImageReduceEffect.SUM );
            
            m_akSumTarget[i] = CreateLocalPlaneNode();      
            m_akSumTarget[i].UpdateGS();
            m_akSumTarget[i].UpdateRS();
            
            iWidth = Math.max(iWidth/2, 1);
            iHeight = Math.max(iHeight/2, 1);  
            if ( iWidth < 2 || iHeight < 2 )
            {
                m_iCount = i+1;
                break;
            }
        }
    }
    
    protected void UpdateSlice( float fZ )
    {
        //m_spkEffectPlane.ZSlice( fZ );
        m_kImageEffectB.ZSlice( fZ );
        m_kImageEffectDual.ZSlice( fZ );
        //m_bDisplay = true;        
    }
    
    private void calcEntropy()
    {        
        if ( m_bCalcImageA )
        {
            m_bCalcImageA = false;    
            m_dHx_save = calcEntropyA( m_kImageA, m_kImageEffectA, m_kImageA.dataSize );
        }
        if ( m_bCalc2D )
        {
            calcEntropy( m_kImageA, m_kImageEffectB, m_kImageEffectDual, m_kImageA.dataSize ); 
            
            int nVoxels = m_kImageA.dataSize;
            if (m_dOverlap > (0.75 * nVoxels)) {
                double nRatio = ((double) nVoxels) / m_dOverlap;

                m_dHx  = (nRatio * m_dHx_save) - Math.log(nRatio);
                m_dHy  = (nRatio * m_dHy) - Math.log(nRatio);
                m_dHxy = (nRatio * m_dHxy) - Math.log(nRatio);
                
                //System.err.println( "GPU: " + m_kImageA.dataSize + " "  + m_dHx + " " + m_dHy + " " + m_dHxy + " " + m_dOverlap );

            } else {
                m_dHx = Math.log(nVoxels);
                m_dHy = Math.log(nVoxels);
                m_dHxy = 2.0 * Math.log(nVoxels);
                //System.out.println("nOvelap not high enough, less than 15% of voxels.");
            }
        }
    }
    
    public boolean writeImage()
    {
        BufferedImage kScreenShot = m_pkRenderer.Screenshot();
        try {
            String directory = new String( "E:\\MagicConsulting\\" ); 
            //System.err.println( directory );
            ImageIO.write(kScreenShot, "jpg", new File( directory + "GPU_DEBUG" + m_iScreenCaptureCounter++ + "." + "jpg"));
        } catch (IOException e) {
            e.printStackTrace();
        }
        return true;
    }
    
    
    private double calcEntropyA( ModelSimpleImage kImage, VolumeHistogramEffect kEffect, double dNumSamples )
    {
        m_kImagePoints.DetachAllEffects();
        m_kImagePoints.AttachEffect(kEffect);
        m_kHistogramOutput[0].Enable();
        m_pkRenderer.SetBackgroundColor(ColorRGBA.BLACK);
        m_pkRenderer.ClearBuffers();
        int iZExtents = (kImage.nDims == 3) ? kImage.extents[2]: 1;
        for ( int i = 0; i < iZExtents; i++ )
        {
            if ( iZExtents == 1 )
            {
                kEffect.ZSlice( 0 );
            }
            else
            {
                kEffect.ZSlice( (float)i / (float)(iZExtents-1) );
            }
            m_pkRenderer.Draw(m_kImagePoints);
        }
        m_kHistogramOutput[0].Disable();

        /*
        Texture kTarget = m_kHistogramOutput[0].GetTarget(0);       
        m_pkRenderer.GetTexImage( kTarget );

            System.err.println( "" );
            System.err.println( "" );
            System.err.println( "" );
            System.err.println( "" );
            System.err.println( "" );
            System.err.println( "TEXTURE NAME = " + kTarget.GetName() );
            float[] afData = kTarget.GetImage().GetFloatData();
            float fSum = 0;
            for ( int i = 0; i < afData.length; i++ )
            {
                if ( afData[i] != 0.0 )
                {
                    System.err.println( i + " " + afData[i] );
                    fSum += afData[i];
                }
            }
            System.err.println( fSum );
        */
        
        double fEntropy = Reduce();
        return fEntropy / dNumSamples;
    }
    
    
    private void calcEntropy( ModelSimpleImage kImage, ShaderEffect kEffect, ShaderEffect kEffectDual, double dNumSamples )
    {
        //long time = System.currentTimeMillis();
        m_kImagePoints.DetachAllEffects();
        m_kImagePoints.AttachEffect(kEffect);
        m_kImagePointsDual.DetachAllEffects();
        m_kImagePointsDual.AttachEffect(kEffectDual);
        
        m_kHistogramOutput[1].Enable();
        m_pkRenderer.SetBackgroundColor(ColorRGBA.BLACK);
        m_pkRenderer.ClearBuffers();
        int iZExtents = (kImage.nDims == 3) ? kImage.extents[2]: 1;
        for ( int i = 0; i < iZExtents; i++ )
        {
            if ( iZExtents == 1 )
            {
                UpdateSlice( 0f ); 
                //cpuHistogram( aiVal, iDim, 0f, m_kImageTransform, kImage, m_kImageB );
            }
            else
            {
                UpdateSlice( (float)i / (float)(iZExtents-1) );
                //cpuHistogram( aiVal, iDim, (float)i / (float)(iZExtents-1), m_kImageTransform, kImage, m_kImageB );
            }
            m_pkRenderer.Draw(m_kImagePointsDual);
            m_pkRenderer.Draw(m_kImagePoints);
            //writeImage();
        }
        m_kHistogramOutput[1].Disable();
        /*
        Texture kTarget = m_kHistogramOutput[1].GetTarget(0);       
        m_pkRenderer.GetTexImage( kTarget );
        System.err.println( "" );
        System.err.println( "" );
        System.err.println( "" );
        System.err.println( "" );
        System.err.println( "" );
        System.err.println( "TEXTURE NAME = " + kTarget.GetName() );
        float[] afData = kTarget.GetImage().GetFloatData();
        for ( int i = 0; i < afData.length; i++ )
        {
            if ( afData[i] != 0.0 )
            {
                System.err.println( i + " " + afData[i] );
            }
        }
        */
        ReduceDual(dNumSamples);
    }
    
    private void ReduceDual( double dNumSamples )
    {
        Texture kTarget = null;
        m_pkPlane.DetachAllEffects();
        m_pkPlane.AttachEffect( m_akImageReduceEntropy[1] );
 
        m_spkScene.DetachAllChildren();
        m_spkScene.AttachChild( m_pkPlane );
        m_spkScene.UpdateGS();
        m_spkScene.UpdateRS();
        float fVal;
        for ( int iTarget = 0; iTarget < (m_iCount-1); iTarget++ )
        {                    

            m_kCuller.ComputeVisibleSet(m_spkScene);
            m_akReduceOutput[1][iTarget].Enable();
            m_pkRenderer.SetBackgroundColor(ColorRGBA.BLACK);
            m_pkRenderer.ClearBuffers();
            m_pkRenderer.DrawScene(m_kCuller.GetVisibleSet());
            m_akReduceOutput[1][iTarget].Disable();
            kTarget = m_akReduceOutput[1][iTarget].GetTarget(0);
            /*
            m_pkRenderer.GetTexImage( kTarget );            
            System.err.println( "" );
            System.err.println( "" );
            System.err.println( "" );
            System.err.println( "" );
            System.err.println( "" );
            System.err.println( "Renderbuffer size " + m_iWidth + " " + m_iHeight );
            System.err.println( "TEXTURE NAME = " + kTarget.GetName() );
            for ( int i = 0; i < kTarget.GetImage().GetBound(1); i++  )
            {
                for ( int j = 0; j < kTarget.GetImage().GetBound(0); j++  )
                {
                    fVal = kTarget.GetImage().GetFloatData()[i*kTarget.GetImage().GetBound(0) + j];

                    if ( fVal != 0 )
                    {
                        System.err.println( i + " " + j + " " + fVal );
                    }
                }
            }        
*/
            m_akSumTarget[iTarget].DetachAllEffects();
            m_akSumTarget[iTarget].AttachEffect( m_akImageReduceSum[1][iTarget] );
            m_spkScene.DetachAllChildren();
            m_spkScene.AttachChild( m_akSumTarget[iTarget] );
            m_spkScene.UpdateGS();
            m_spkScene.UpdateRS();
        }

        double dEntropyMoving = 0;
        double dEntropyDual = 0;
        double dOverlap = 0;
        if ( kTarget != null )
        {
            int iIndex = 0;
            int iStep = (int)Math.max( 1.0, (int)(kTarget.GetImage().GetBytesPerPixel()/4.0f) );
            m_pkRenderer.GetTexImage( kTarget );
            for ( int i = 0; i < kTarget.GetImage().GetBound(1); i++  )
            {
                for ( int j = 0; j < kTarget.GetImage().GetBound(0); j++  )
                {
                    dEntropyMoving += kTarget.GetImage().GetFloatData()[iIndex];
                    dOverlap += kTarget.GetImage().GetFloatData()[iIndex+1];
                    dEntropyDual += kTarget.GetImage().GetFloatData()[iIndex+2];
                    //System.err.println( i + " " + j + " " + dEntropyMoving + " " + dOverlap + " " + dEntropyDual );
                    iIndex += iStep;
                }
            }
        }            
        m_dOverlap = dOverlap;
        m_dHy = dEntropyMoving/dNumSamples;
        m_dHxy = dEntropyDual/dNumSamples;
    }
    
    private double Reduce()
    {
        Texture kTarget = null;
        m_pkPlane.DetachAllEffects();
        m_pkPlane.AttachEffect( m_akImageReduceEntropy[0] );
 
        m_spkScene.DetachAllChildren();
        m_spkScene.AttachChild( m_pkPlane );
        m_spkScene.UpdateGS();
        m_spkScene.UpdateRS();
        float fVal;
        for ( int iTarget = 0; iTarget < (m_iCount-1); iTarget++ )
        {                    

            m_kCuller.ComputeVisibleSet(m_spkScene);
            m_akReduceOutput[0][iTarget].Enable();
            m_pkRenderer.SetBackgroundColor(ColorRGBA.BLACK);
            m_pkRenderer.ClearBuffers();
            m_pkRenderer.DrawScene(m_kCuller.GetVisibleSet());
            m_akReduceOutput[0][iTarget].Disable();
            kTarget = m_akReduceOutput[0][iTarget].GetTarget(0);
            /*
            m_pkRenderer.GetTexImage( kTarget );            
            System.err.println( "" );
            System.err.println( "" );
            System.err.println( "" );
            System.err.println( "" );
            System.err.println( "" );
            System.err.println( "Renderbuffer size " + m_iWidth + " " + m_iHeight );
            System.err.println( "TEXTURE NAME = " + kTarget.GetName() );
            float[] afData = kTarget.GetImage().GetFloatData();
            for ( int i = 0; i < afData.length; i++ )
            {
                if ( afData[i] != 0.0 )
                {
                    System.err.println( i + " " + afData[i] );
                }
            }
*/
            m_akSumTarget[iTarget].DetachAllEffects();
            m_akSumTarget[iTarget].AttachEffect( m_akImageReduceSum[0][iTarget] );
            m_spkScene.DetachAllChildren();
            m_spkScene.AttachChild( m_akSumTarget[iTarget] );
            m_spkScene.UpdateGS();
            m_spkScene.UpdateRS();
        }

        double fEntropy = 0;
        if ( kTarget != null )
        {
            int iIndex = 0;
            int iStep = (int)Math.max( 1.0, (int)(kTarget.GetImage().GetBytesPerPixel()/4.0f) );
            m_pkRenderer.GetTexImage( kTarget );
            for ( int i = 0; i < kTarget.GetImage().GetBound(1); i++  )
            {
                for ( int j = 0; j < kTarget.GetImage().GetBound(0); j++  )
                {
                    fEntropy += kTarget.GetImage().GetFloatData()[iIndex];
                    //System.err.println( i + " " + j + " " + fEntropy );
                    iIndex += iStep;
                }
            }
        }            
        
        return fEntropy;
    }
    

    private OpenGLFrameBuffer CreateSingleRenderTarget( String kImageName, int iWidth, int iHeight )
    {        
        float[] afData = new float[iWidth*iHeight];
        GraphicsImage pkSceneImage = new GraphicsImage(GraphicsImage.FormatMode.IT_L32F,iWidth,iHeight,afData,
                kImageName);
        Texture[] akSceneTarget = new Texture[1];
        akSceneTarget[0] = new Texture();
        akSceneTarget[0].SetImage(pkSceneImage);
        akSceneTarget[0].SetShared(true);
        akSceneTarget[0].SetFilterType(Texture.FilterType.NEAREST);
        akSceneTarget[0].SetWrapType(0,Texture.WrapType.CLAMP);
        akSceneTarget[0].SetWrapType(1,Texture.WrapType.CLAMP);
        akSceneTarget[0].SetSamplerInformation( new SamplerInformation( kImageName, SamplerInformation.Type.SAMPLER_2D, 0, 0 ) );
        m_pkRenderer.LoadTexture( akSceneTarget[0] );
        
        return new OpenGLFrameBuffer(m_eFormat,m_eDepth,m_eStencil,
                m_eBuffering,m_eMultisampling,m_pkRenderer,akSceneTarget,m_kGLAutoDrawable,0);
    }
    

    private OpenGLFrameBuffer CreateRenderTarget( String kImageName, int iWidth, int iHeight )
    {      
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
        akSceneTarget[0].SetSamplerInformation( new SamplerInformation( kImageName, SamplerInformation.Type.SAMPLER_2D, 0, 0 ) );
        m_pkRenderer.LoadTexture( akSceneTarget[0] );
        
        return new OpenGLFrameBuffer(m_eFormat,m_eDepth,m_eStencil,
                m_eBuffering,m_eMultisampling,m_pkRenderer,akSceneTarget,m_kGLAutoDrawable,0);
    }
    
    private void cpuHistogram( float[] aiVal, int iDim, float fZ, Matrix4f kMat, ModelSimpleImage kImageA, ModelSimpleImage kImageB )
    {        

        if ( iDim == 1 )
        {
            for ( int i = 0; i < m_kImagePoints.VBuffer.GetVertexQuantity(); i++ )
            {
                Vector3f kTC = m_kImagePoints.VBuffer.GetPosition3(i);
                kTC.Z = fZ;

                kTC.X = (kTC.X + 1.0f) * 0.5f;
                kTC.Y = (kTC.Y + 1.0f) * 0.5f;

                kTC.X *= (kImageA.extents[0]-1);
                kTC.Y *= (kImageA.extents[1]-1);
                if ( kImageA.nDims == 3 )
                {
                    kTC.Z *= (kImageA.extents[2]-1);
                }
                int iX = (int)kTC.X;
                int iY = (int)kTC.Y;
                int iZ = (int)kTC.Z;
                float fData = kImageA.data[ iZ * (kImageA.extents[0] + kImageA.extents[1]) + 
                                           iY * kImageA.extents[0] + iX ];
                int iXIndex = (int)(fData * kImageA.extents[0]);

                Vector4f kTC4 = new Vector4f(kTC.X, kTC.Y, 0, 1.0f );
                Vector4f kNewTC4 = new Vector4f();
                kMat.Mult( kTC4, kNewTC4 );

                //Vector4f kNewTC4Inv = new Vector4f();
                //m_kImageTransformInv.Mult( kNewTC4, kNewTC4Inv );
                //System.err.println( kTC4.ToString() + "            " + kNewTC4.ToString()  + "            " + kNewTC4Inv.ToString() );
                iX = (int)kNewTC4.X;
                iY = (int)kNewTC4.Y;
                iZ = 0;
                if ( iX < 0 || iX >= kImageA.extents[0]|| 
                        iY < 0 || iY >= kImageA.extents[1] )
                {}
                else
                {
                    fData = kImageB.data[ iZ * (kImageA.extents[0] + kImageA.extents[1]) + 
                                          iY * kImageA.extents[0] + iX ];
                    int iYIndex = (int)(fData * kImageA.extents[1]);     
                    if ( (iYIndex * kImageA.extents[0] + iXIndex) >= 0 && (iYIndex * kImageA.extents[0] + iXIndex) < aiVal.length )
                    {
                        aiVal[ iYIndex * kImageA.extents[0] + iXIndex]++;
                    }
                }
            }
        }
        else
        {
            for ( int i = 0; i < m_kImagePoints.VBuffer.GetVertexQuantity(); i++ )
            {
                Vector3f kTC = m_kImagePoints.VBuffer.GetPosition3(i);
                kTC.Z = fZ;

                kTC.X = (kTC.X + 1.0f) * 0.5f;
                kTC.Y = (kTC.Y + 1.0f) * 0.5f;

                kTC.X *= (kImageA.extents[0]-1);
                kTC.Y *= (kImageA.extents[1]-1);
                if ( kImageA.nDims == 3 )
                {
                    kTC.Z *= (kImageA.extents[2]-1);
                }
                int iX = (int)kTC.X;
                int iY = (int)kTC.Y;
                int iZ = (int)kTC.Z;
                float fData = kImageA.data[ iZ * (kImageA.extents[0] + kImageA.extents[1]) + 
                                           iY * kImageA.extents[0] + iX ];
                
                aiVal[(int)(fData * kImageA.extents[0])]++;
            }
        }
    }
    
    private float cpuReduce( float[] aiVal, double dLogN, double nVoxels, double[] adOverlap )
    {
        float fEntropy = 0;
        double p;
        for ( int i = 0; i < aiVal.length; i++ )
        {
            if ( aiVal[i] > 0 )
            {
                if ( adOverlap != null )
                {
                    //adOverlap[0] += aiVal[i];
                }
                //System.err.print( Math.log(aiVal[i]) + " " + dLogN + " " + aiVal[i] + "   =  " );
                //System.err.println( (aiVal[i] * (Math.log(aiVal[i]) - dLogN)) );
                //fEntropy += (aiVal[i] * (Math.log(aiVal[i]) - dLogN));
                p = ((double) aiVal[i]) / ((double) nVoxels);
                fEntropy += (-p * Math.log(p));
            }
        }
        
        return fEntropy;
    }
    
    /*
    private Texture CreateRenderTarget( String kImageName, int iWidth, int iHeight )
    {        
        float[] afData = new float[iWidth*iHeight];
        GraphicsImage pkSceneImage = new GraphicsImage(GraphicsImage.FormatMode.IT_L32F,iWidth,iHeight,afData,
                kImageName);
        Texture kSceneTarget = new Texture();
        kSceneTarget.SetImage(pkSceneImage);
        kSceneTarget.SetShared(true);
        kSceneTarget.SetFilterType(Texture.FilterType.NEAREST);
        kSceneTarget.SetWrapType(0,Texture.WrapType.CLAMP);
        kSceneTarget.SetWrapType(1,Texture.WrapType.CLAMP);
        kSceneTarget.SetSamplerInformation( new SamplerInformation( kImageName, SamplerInformation.Type.SAMPLER_2D, 0, 0 ) );
        m_pkRenderer.LoadTexture( kSceneTarget );
        return kSceneTarget;
    }
        
    private OpenGLFrameBuffer CreateFBO( Texture kTarget )
    {
        return new OpenGLFrameBuffer(m_eFormat,m_eDepth,m_eStencil,
                m_eBuffering,m_eMultisampling,m_pkRenderer,kTarget,m_kGLAutoDrawable,0);
    }
    */

}