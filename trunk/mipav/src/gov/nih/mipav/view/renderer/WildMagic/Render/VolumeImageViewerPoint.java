package gov.nih.mipav.view.renderer.WildMagic.Render;

import gov.nih.mipav.MipavInitGPU;
import gov.nih.mipav.model.structures.ModelSimpleImage;
import gov.nih.mipav.model.structures.TransMatrix;
import gov.nih.mipav.view.Preferences;

import javax.swing.JFrame;

import javax.media.opengl.GLAutoDrawable;
import javax.media.opengl.GLCanvas;
import javax.media.opengl.GLContext;

import WildMagic.LibApplications.OpenGLApplication.JavaApplication3D;
import WildMagic.LibFoundation.Mathematics.ColorRGBA;
import WildMagic.LibFoundation.Mathematics.Mathf;
import WildMagic.LibFoundation.Mathematics.Matrix4f;
import WildMagic.LibFoundation.Mathematics.Vector3f;
import WildMagic.LibGraphics.Rendering.AlphaState;
import WildMagic.LibGraphics.Rendering.GraphicsImage;
import WildMagic.LibGraphics.Rendering.Texture;
import WildMagic.LibGraphics.SceneGraph.Attributes;
import WildMagic.LibGraphics.SceneGraph.Node;
import WildMagic.LibGraphics.SceneGraph.Polypoint;
import WildMagic.LibGraphics.SceneGraph.StandardMesh;
import WildMagic.LibGraphics.SceneGraph.TriMesh;
import WildMagic.LibGraphics.SceneGraph.VertexBuffer;
import WildMagic.LibGraphics.Shaders.CompiledProgramCatalog;
import WildMagic.LibGraphics.Shaders.ImageCatalog;
import WildMagic.LibGraphics.Shaders.PixelProgramCatalog;
import WildMagic.LibGraphics.Shaders.SamplerInformation;
import WildMagic.LibGraphics.Shaders.VertexProgramCatalog;
import WildMagic.LibRenderers.OpenGLRenderer.OpenGLFrameBuffer;
import WildMagic.LibRenderers.OpenGLRenderer.OpenGLRenderer;

public class VolumeImageViewerPoint extends JavaApplication3D
//implements GLEventListener, KeyListener
{
    protected Node m_spkScene;    

    protected VolumeHistogramEffect m_akCollapse2D;
    protected VolumeHistogramEffect m_akCollapseColumns;
    protected VolumeHistogramEffect m_akCollapseRows;
    
    protected ImageReduceEffect m_akImageReduceEntropy;
    protected VolumeHistogramEffect m_kImageEffectDual;
    private GLAutoDrawable m_kGLAutoDrawable = null;

    private Polypoint m_kImagePointsDual;
    private Polypoint m_kHistogramPoints2D;
    private Polypoint m_kEntropyPoints2D;
    private Polypoint m_kHistogramPointsColumns;
    private Polypoint m_kHistogramPointsRows;

    private OpenGLFrameBuffer m_kHistogramOutput;
    private OpenGLFrameBuffer m_kHistogramOutputB;
    private OpenGLFrameBuffer m_kEntropyOut;
    
    private int m_iCount = 0;
    private AlphaState m_kAlpha;
    private ModelSimpleImage m_kImageA;
    private ModelSimpleImage m_kImageB;
    private String m_kNameA = "imageA";
    private String m_kNameB = "imageB";
    private Texture m_kTextureA;
    private Texture m_kTextureB;
    
    private double m_dHx, m_dHy, m_dHxy;
    private double m_dOverlap;
    private Matrix4f m_kImageTransform = new Matrix4f(false);

    protected JFrame m_kFrame;
    
    private boolean m_bDisposeComplete = false;
    private boolean m_bDispose = false;
    private float[] m_afJoint;
    private boolean m_bUseJoint = false;

    protected boolean m_bDisplay = true;
    protected boolean m_bInit = false;
    private boolean m_bPrint = false;
    private float m_l2DRunTime = 0;
    private float m_lEntropyRunTime = 0;
    private boolean m_bFirstRun = false;
    
    public VolumeImageViewerPoint( ModelSimpleImage kImageA, ModelSimpleImage kImageB, int iNBins )
    {
        super( "VolumeImageViewer", 0, 0, iNBins, iNBins,
                new ColorRGBA( 0.0f,0.0f,0.0f,1.0f ) );
        m_pkRenderer = new OpenGLRenderer( m_eFormat, m_eDepth, m_eStencil,
                m_eBuffering, m_eMultisampling,
                m_iWidth, m_iHeight );
        GetCanvas().getContext().setSynchronized(true);  
        m_afJoint = new float[iNBins*iNBins*4];
        
        m_kImageA = kImageA;
        m_kImageB = kImageB;
        
        String kExternalDirs = MipavInitGPU.getExternalDirs();        
        ImageCatalog.SetActive( new ImageCatalog("Main", kExternalDirs) );      
        VertexProgramCatalog.SetActive(new VertexProgramCatalog("Main", kExternalDirs));       
        PixelProgramCatalog.SetActive(new PixelProgramCatalog("Main", kExternalDirs));
        CompiledProgramCatalog.SetActive(new CompiledProgramCatalog());
    }
    
    public void SetFrame( JFrame kFrame )
    {
        m_kFrame = kFrame;
    }

    public GLCanvas GetCanvas()
    {
        return ((OpenGLRenderer)m_pkRenderer).GetCanvas();
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
        m_bDisplay = false;
        calcEntropy();
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
        //System.err.println( "VolumeImageViewerPoint dispose()" );
        if ( m_kHistogramPointsColumns != null )
        {
            m_pkRenderer.ReleaseResources(m_kHistogramPointsColumns);
            m_kHistogramPointsColumns.dispose();
            m_kHistogramPointsColumns = null;
        }
        if ( m_kHistogramPointsRows != null )
        {
            m_pkRenderer.ReleaseResources(m_kHistogramPointsRows);
            m_kHistogramPointsRows.dispose();
            m_kHistogramPointsRows = null;
        }
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

        m_kImageEffectDual.dispose();


        m_kHistogramOutput.GetTarget(0).GetImage().dispose();
        m_kHistogramOutput.GetTarget(0).dispose();
        m_kHistogramOutput.TerminateBuffer();
        m_kHistogramOutput.dispose();

        m_kHistogramOutputB.GetTarget(0).GetImage().dispose();
        m_kHistogramOutputB.GetTarget(0).dispose();
        m_kHistogramOutputB.TerminateBuffer();
        m_kHistogramOutputB.dispose();

        m_kEntropyOut.GetTarget(0).GetImage().dispose();
        m_kEntropyOut.GetTarget(0).dispose();
        m_kEntropyOut.TerminateBuffer();
        m_kEntropyOut.dispose();

        m_pkRenderer.ReleaseResources(m_akImageReduceEntropy);
        m_akImageReduceEntropy.dispose();

        m_pkRenderer.ReleaseResources(m_akCollapse2D);
        m_akCollapse2D.dispose();

        m_pkRenderer.ReleaseResources(m_akCollapseColumns);
        m_akCollapseColumns.dispose();

        m_pkRenderer.ReleaseResources(m_akCollapseRows);
        m_akCollapseRows.dispose();
    
        ImageCatalog.GetActive().dispose();
        VertexProgramCatalog.GetActive().dispose();     
        PixelProgramCatalog.GetActive().dispose();
        CompiledProgramCatalog.GetActive().dispose();
                
        super.dispose();
    }
    
    public void calcError(TransMatrix kTransform) {
        if ( GetCanvas().getContext().makeCurrent() == GLContext.CONTEXT_NOT_CURRENT )
        {
            System.err.println( "Context not made current" );
        }
        if ( m_bFirstRun )
        {
            long lStartTime = System.currentTimeMillis();

            setTransform( new TransMatrix(4,4));
            m_bDisplay = true;
            display(GetCanvas());  
            m_bUseJoint = false;

            long time = (System.currentTimeMillis() - lStartTime);
            System.err.println("");
            System.err.println( "nBins " + m_iWidth + " count " + m_iCount );
            System.err.println( "Initialization time: " + (time * .001f) + " seconds" );
            System.err.println( "2D Histogram time: " + (m_l2DRunTime * .001f) + " seconds" );
            System.err.println( "Entropy time: " + (m_lEntropyRunTime * .001f) + " seconds" );

            System.err.println("");
            m_bFirstRun = false;
        }
        setTransform(kTransform);
        m_bDisplay = true;
        display(GetCanvas());  
        m_bUseJoint = false;
        GetCanvas().getContext().release();
    }

    public double getError()
    {
        return m_dHxy / (m_dHx + m_dHy);
    }

    public void init(GLAutoDrawable arg0) {
        m_kGLAutoDrawable = arg0; 
        if ( m_bInit )
        {
            return;
        }
        
        arg0.setAutoSwapBufferMode( false );

        ((OpenGLRenderer)m_pkRenderer).SetDrawable( arg0 );
        ((OpenGLRenderer)m_pkRenderer).InitializeState();

        CreateScene();

        // initial update of objects
        m_spkScene.UpdateGS();
        m_spkScene.UpdateRS();


        //((OpenGLRenderer)m_pkRenderer).ClearDrawable( );
        m_bInit = true;
        m_spkCamera = null;
        m_pkRenderer.Activate();
    }
    
    public void reshape(GLAutoDrawable arg0, int iX, int iY, int iWidth, int iHeight){}
    
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
    }

    protected void CreateHistogramMesh(int iWidth, int iHeight)
    {
        Attributes kAttributes = new Attributes();
        kAttributes.SetPChannels(3);
        kAttributes.SetTChannels(0,3);
        
        int iVQuantity = iWidth*iHeight;
        VertexBuffer pkVB = new VertexBuffer(kAttributes,iVQuantity);

        // generate geometry
        float fInv0 = 1.0f/((float)iWidth);
        float fInv1 = 1.0f/((float)iHeight);
        float fU, fV;
        int i, i0, i1;

        Vector3f kYTmp = new Vector3f();
        Vector3f kXTmp = new Vector3f();
        for (i1 = 0, i = 0; i1 < iHeight; i1++)
        {
            fV = i1*fInv1;
            kYTmp.Set(0f, ((2.0f*fV-1.0f)), 0f );
            for (i0 = 0; i0 < iWidth; i0++)
            {
                fU = i0*fInv0;
                kXTmp.Set(((2.0f*fU-1.0f)), 0f, 0f);
                kXTmp.Add( kYTmp );
                //kXTmp.Add( kZTmp );
                pkVB.SetPosition3(i,kXTmp );

                pkVB.SetTCoord3(0,i, fU,fV,0);
                //System.err.println( kXTmp.ToString() + " " + fU + " " + fV );
                i++;
            }
        }
        m_kHistogramPoints2D = new Polypoint( pkVB );
        m_kEntropyPoints2D = new Polypoint( new VertexBuffer(pkVB) );
        m_kHistogramPointsColumns = new Polypoint( new VertexBuffer(pkVB) );
        m_kHistogramPointsRows = new Polypoint( new VertexBuffer(pkVB) );
        
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
        
        m_kHistogramPointsColumns.AttachGlobalState(kAlpha);
        m_kHistogramPointsColumns.UpdateGS();
        m_kHistogramPointsColumns.UpdateRS();    
        
        m_kHistogramPointsRows.AttachGlobalState(kAlpha);
        m_kHistogramPointsRows.UpdateGS();
        m_kHistogramPointsRows.UpdateRS();    
        
    }

    protected void CreateImageMesh(int iWidth, int iHeight, int iDepth)
    {
        //System.err.println( iWidth + " " + iHeight + " " + iDepth );
        Attributes kAttributes = new Attributes();
        kAttributes.SetPChannels(3);
        kAttributes.SetTChannels(0,3);
        
        int iVQuantity = iWidth*iHeight*iDepth;
        VertexBuffer pkVB = new VertexBuffer(kAttributes,iVQuantity);

        // generate geometry
        float fInv0 = 1.0f/(iWidth - 1.0f);
        float fInv1 = 1.0f/(iHeight - 1.0f);
        float fInv2 = 1.0f/(iDepth);
        float fU, fV, fW;
        int i, i0, i1, i2;

        Vector3f kZTmp = new Vector3f();
        Vector3f kYTmp = new Vector3f();
        Vector3f kXTmp = new Vector3f();
        for (i2 = 0, i = 0; i2 < iDepth; i2++ )
        {
            fW = i2*fInv2;
            //kZTmp.Set(0f, 0f, ((2.0f*fW-1.0f)));
            for (i1 = 0; i1 < iHeight; i1++)
            {
                fV = i1*fInv1;
                kYTmp.Set(0f, ((2.0f*fV-1.0f)), 0f );
                for (i0 = 0; i0 < iWidth; i0++)
                {
                    fU = i0*fInv0;
                    kXTmp.Set(((2.0f*fU-1.0f)), 0f, 0f);
                    kXTmp.Add( kYTmp );
                    //kXTmp.Add( kZTmp );
                    pkVB.SetPosition3(i,kXTmp );

                    pkVB.SetTCoord3(0,i, fU,fV,fW);

                    i++;
                }
            }
        }
        
        //StandardMesh kSM = new StandardMesh(kAttributes);
        //TriMesh kMesh = kSM.Rectangle(iWidth,iHeight,1,1);
        //m_kImagePointsDual = new Polypoint( kMesh.VBuffer );
        m_kImagePointsDual = new Polypoint( pkVB );
        m_kAlpha = new AlphaState();
        m_kAlpha.BlendEnabled = true;
        m_kAlpha.SrcBlend = AlphaState.SrcBlendMode.SBF_ONE;
        m_kAlpha.DstBlend = AlphaState.DstBlendMode.DBF_ONE;
        m_kImagePointsDual.AttachGlobalState(m_kAlpha);
        m_kImagePointsDual.UpdateGS();
        m_kImagePointsDual.UpdateRS();    
    }
    
    
    private void CreateImageTextures()
    {
        SamplerInformation.Type eSamplerType = SamplerInformation.Type.SAMPLER_3D;
        
        m_kTextureA = new Texture();
        m_kTextureA.SetImage(VolumeImage.UpdateData(m_kImageA, m_kNameA ));
        m_kTextureA.SetShared(true);
        m_kTextureA.SetFilterType(Texture.FilterType.NEAREST);
        m_kTextureA.SetWrapType(0,Texture.WrapType.CLAMP_BORDER);
        m_kTextureA.SetWrapType(1,Texture.WrapType.CLAMP_BORDER);
        m_kTextureA.SetWrapType(2,Texture.WrapType.CLAMP_BORDER);                
        m_kTextureA.SetSamplerInformation( new SamplerInformation( m_kNameA, eSamplerType, 0, 0 ) );
        m_pkRenderer.LoadTexture( m_kTextureA );
        
        m_kTextureB = new Texture();
        m_kTextureB.SetImage(VolumeImage.UpdateData(m_kImageB, m_kNameB ));
        m_kTextureB.SetShared(true);
        m_kTextureB.SetFilterType(Texture.FilterType.NEAREST);
        m_kTextureB.SetWrapType(0,Texture.WrapType.CLAMP_BORDER);
        m_kTextureB.SetWrapType(1,Texture.WrapType.CLAMP_BORDER);
        m_kTextureB.SetWrapType(2,Texture.WrapType.CLAMP_BORDER);           
        m_kTextureB.SetSamplerInformation( new SamplerInformation( m_kNameB, eSamplerType, 0, 0 ) );
        m_pkRenderer.LoadTexture( m_kTextureB );
    }

    protected void CreateScene ()
    {           
        
        int iWidth = m_iWidth;
        int iHeight = m_iHeight;
        m_kHistogramOutput = CreateRenderTarget( "Histogram2D", iWidth, iHeight );
        m_kHistogramOutputB = CreateRenderTarget( "Histogram2DB", iWidth, iHeight );
        m_kEntropyOut = CreateRenderTarget( "EntropyOut", 1, 1 );
        
        m_spkScene = new Node();

        CreateImageTextures();
        
        m_kImageEffectDual = new VolumeHistogramEffect( m_kTextureA, m_kTextureB,
                m_kImageA.min, m_kImageA.max, m_kImageB.min, m_kImageB.max, 
                m_kImageA.extents[0],m_kImageA.extents[1], m_iWidth, m_kImageTransform, true );
        int iDepth = m_kImageA.nDims == 3 ? m_kImageA.extents[2] : 1;
        CreateImageMesh(m_kImageA.extents[0],m_kImageA.extents[1],iDepth);
        m_kImagePointsDual.AttachEffect(m_kImageEffectDual);
        

        CreateHistogramMesh(m_iWidth, m_iHeight);
        m_akCollapse2D = new VolumeHistogramEffect( m_kHistogramOutput.GetTarget(0), VolumeHistogramEffect.NONE );
        m_akCollapseColumns = new VolumeHistogramEffect( m_kHistogramOutput.GetTarget(0), VolumeHistogramEffect.COLLAPSE_COLUMNS );
        m_akCollapseRows = new VolumeHistogramEffect( m_kHistogramOutput.GetTarget(0), VolumeHistogramEffect.COLLAPSE_ROWS );

        m_kHistogramPoints2D.AttachEffect(m_akCollapse2D);
        m_kHistogramPointsColumns.AttachEffect(m_akCollapseColumns);
        m_kHistogramPointsRows.AttachEffect(m_akCollapseRows);

        double dSize = m_kImageA.dataSize;
        m_akImageReduceEntropy = new ImageReduceEffect( m_kHistogramOutputB.GetTarget(0), dSize );
        m_kEntropyPoints2D.AttachEffect( m_akImageReduceEntropy );              
    }
    
    private void calcEntropy()
    {        
        calcEntropy( m_kImageA, m_kImageA.dataSize ); 

        int nVoxels = m_kImageA.dataSize;
        if ( ( (m_kImageA.nDims < 3) && (m_dOverlap > 1000) ) ||
                ( (m_kImageA.nDims == 3) && (m_dOverlap > (0.15 * nVoxels)) ) ) {
            double nRatio = ((double) nVoxels) / m_dOverlap;

            m_dHx  = (nRatio * m_dHx) - Math.log(nRatio);
            m_dHy  = (nRatio * m_dHy) - Math.log(nRatio);
            m_dHxy = (nRatio * m_dHxy) - Math.log(nRatio);
            if ( m_bPrint )
            {
                System.err.println( "GPU: " + m_kImageA.dataSize + " "  + m_dHx + " " + m_dHy + " " + m_dHxy + " " + m_dOverlap );
            }

        } else {
            m_dHx = Math.log(nVoxels);
            m_dHy = Math.log(nVoxels);
            m_dHxy = 2.0 * Math.log(nVoxels);
            if ( m_bPrint )
            {
                System.out.println("nOvelap not high enough, less than 15% of voxels.");
                System.err.println( "GPU: " + m_kImageA.dataSize + " "  + m_dHx + " " + m_dHy + " " + m_dHxy + " " + m_dOverlap );
            }
        }
    }
    
    private void calcEntropy( ModelSimpleImage kImage, double dNumSamples )
    {
        long lStartTime = 0;
        if ( m_bFirstRun )
        {
            lStartTime = System.currentTimeMillis();
            
            for ( int t = 0; t < 100; t++ )
            {

                m_kHistogramOutput.Enable();
                m_pkRenderer.SetBackgroundColor(new ColorRGBA(0,0,0,0));
                m_pkRenderer.ClearBuffers();
                m_pkRenderer.Draw(m_kImagePointsDual);
                m_kHistogramOutput.Disable();
            }

            long time = (System.currentTimeMillis() - lStartTime);
            m_l2DRunTime = time/100.0f;
            lStartTime = System.currentTimeMillis();


            for ( int t = 0; t < 100; t++ )
            {
                ReduceDualA(dNumSamples);
            }
            time = (System.currentTimeMillis() - lStartTime);
            m_lEntropyRunTime = time/100.0f;
            
        }
        if ( m_bUseJoint )
        {
            Texture kTarget = m_kHistogramOutput.GetTarget(0);
            kTarget.GetImage().SetData( m_afJoint, m_iWidth, m_iHeight);
            kTarget.Release();
            m_pkRenderer.LoadTexture( kTarget );
        }
        else
        {
            m_kHistogramOutput.Enable();
            m_pkRenderer.SetBackgroundColor(new ColorRGBA(0,0,0,0));
            m_pkRenderer.ClearBuffers();
            m_pkRenderer.Draw(m_kImagePointsDual);
            m_kHistogramOutput.Disable();
            
            m_kHistogramOutputB.Enable();
            m_pkRenderer.SetBackgroundColor(new ColorRGBA(0,0,0,0));
            m_pkRenderer.ClearBuffers();
            //System.err.println( "");
            //System.err.println( "");
            //System.err.println( "");
            //System.err.println( "Drawing pass through points ");
            m_pkRenderer.Draw(m_kHistogramPoints2D);
            m_pkRenderer.Draw(m_kHistogramPointsColumns);
            m_pkRenderer.Draw(m_kHistogramPointsRows);
            m_kHistogramOutputB.Disable();
            //printTarget( "2D Histogram -- collapsed", m_kHistogramOutputB.GetTarget(0) );
        }
        //m_pkRenderer.Finish();
        //m_pkRenderer.DisplayBackBuffer();
        ReduceDualA(dNumSamples);
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
        Texture kTarget = null; //m_akImageReduceEntropy.GetTexture(0, 0);
        //printTarget( "Result", kTarget );
        m_kEntropyOut.Enable();
        m_pkRenderer.SetBackgroundColor(new ColorRGBA(0,0,0,0));
        m_pkRenderer.ClearBuffers();
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
        
    public void setJoint( float[] afJoint )
    {
        for ( int i = 0; i < afJoint.length; i++ )
        {
            m_afJoint[i] = afJoint[i];
        }
        m_bUseJoint = true;
    }
    
    public void Print( boolean bPrint )
    {
        m_bPrint = bPrint;
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
}