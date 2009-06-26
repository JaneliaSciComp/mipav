package gov.nih.mipav.view.renderer.WildMagic.Render;

import gov.nih.mipav.view.renderer.WildMagic.VolumeTriPlanarInterface;

import java.awt.Frame;
import java.awt.event.KeyListener;
import java.awt.event.WindowAdapter;
import java.awt.event.WindowEvent;

import javax.media.opengl.GLAutoDrawable;
import javax.media.opengl.GLEventListener;

import WildMagic.LibFoundation.Mathematics.ColorRGBA;
import WildMagic.LibFoundation.Mathematics.Mathf;
import WildMagic.LibGraphics.Effects.ShaderEffect;
import WildMagic.LibGraphics.Effects.TextureEffect;
import WildMagic.LibGraphics.Rendering.AlphaState;
import WildMagic.LibGraphics.Rendering.GraphicsImage;
import WildMagic.LibGraphics.Rendering.Texture;
import WildMagic.LibGraphics.Rendering.WireframeState;
import WildMagic.LibGraphics.SceneGraph.Attributes;
import WildMagic.LibGraphics.SceneGraph.Geometry;
import WildMagic.LibGraphics.SceneGraph.Node;
import WildMagic.LibGraphics.SceneGraph.StandardMesh;
import WildMagic.LibGraphics.SceneGraph.TriMesh;
import WildMagic.LibRenderers.OpenGLRenderer.OpenGLFrameBuffer;

import com.sun.opengl.util.Animator;

public class VolumeImageViewerPoint extends VolumeImageViewer
implements GLEventListener, KeyListener
{
    protected Node m_kOffScreenScene; 
    protected VolumePlaneEffect m_spkEffectPlane;
    protected VolumeHistogramEffect m_spkEffectPoints;
    private GLAutoDrawable m_kGLAutoDrawable = null;
    /** Scene polygon displaying the first-pass rendering of the proxy-geometry: */
    private TriMesh m_kImagePoints;
    /** Off-screen buffer the first-pass rendering is drawn into: */
    private OpenGLFrameBuffer m_pkPBuffer;
    /** Alpha blending for this object. */
    protected AlphaState m_kAlpha;
    /** Wire-frame for this object. */
    protected WireframeState m_kWireframe = null;
    protected TriMesh m_kCurrentRender;

    protected TriMesh m_pkCurrentPlane;
    private int m_iCount = 0;
    private boolean m_bFirst = true;

    public VolumeImageViewerPoint( VolumeTriPlanarInterface kParentFrame, VolumeImage kVolumeImage )
    {
        super(kParentFrame, kVolumeImage);
    }
    /**
     * @param args
     */
    public static void main( VolumeTriPlanarInterface kParentFrame, VolumeImage kVolumeImage, boolean bShowFrame )
    {
        final VolumeImageViewerPoint kWorld = new VolumeImageViewerPoint(kParentFrame, kVolumeImage);
        final Frame frame = new Frame(kWorld.GetWindowTitle());
        frame.add( kWorld.GetCanvas() );
        final Animator animator = new Animator( kWorld.GetCanvas() );    
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
        animator.start();
    }

    public void display(GLAutoDrawable arg0) {
        if ( m_kAnimator == null )
        {
            return;
        }
        if ( !m_bInit )
        {
            return;
        }
        if ( m_bFirst )
        {
            m_bFirst = false;
            m_kCuller.ComputeVisibleSet(m_kOffScreenScene);
            // Enable rendering to the PBuffer:
            m_pkPBuffer.Enable();
            m_pkRenderer.SetBackgroundColor(ColorRGBA.BLACK);
            m_pkRenderer.ClearBuffers();
            int iZExtents = (m_kVolumeImage.GetImage().getNDims() == 3) ? m_kVolumeImage.GetImage().getExtents()[2]: 1;
            for ( int i = 0; i < iZExtents; i++ )
            {
                UpdateSlice( (float)i / (float)(iZExtents-1) );
                m_pkRenderer.DrawScene(m_kCuller.GetVisibleSet());
            }
            // Disable the PBuffer
            m_pkPBuffer.Disable();
            Texture kTarget = ((ShaderEffect)m_pkPlane.GetEffect(0)).GetPTexture(0,0);
            m_pkRenderer.GetTexImage( kTarget );
            String kImageName = kTarget.GetImage().GetName();
/*
            Texture kTarget = ((ShaderEffect)m_pkPlane.GetEffect(0)).GetPTexture(0,0);
            m_pkRenderer.GetTexImage( kTarget );
            for ( int i = 0; i < kTarget.GetImage().GetBound(1); i++  )
            {
                for ( int j = 0; j < kTarget.GetImage().GetBound(0); j++  )
                {
                    if ( i < 16 && j < 16 )
                        //if (  m_pkSceneTarget.GetImage().GetFloatData()[i] > 1.0 )
                    {
                        System.err.print( kTarget.GetImage().GetFloatData()[i] + " " );
                    }
                }

                if ( i < 16 )
                {
                    System.err.println( "" );
                }
            }
        */
            int iWidth = m_iWidth/2;
            int iHeight = m_iHeight/2;
            TriMesh kNewTarget = null;
            for ( int iTarget = 0; iTarget < (m_iCount-1); iTarget++ )
            {
                kNewTarget = CreateLocalPlaneNode();
                CreateRenderTarget( iWidth, iWidth, iHeight, kNewTarget );

                m_kCuller.ComputeVisibleSet(m_spkScene);
                m_pkPBuffer.Enable();
                m_pkRenderer.SetBackgroundColor(ColorRGBA.BLACK);
                m_pkRenderer.ClearBuffers();
                m_pkRenderer.DrawScene(m_kCuller.GetVisibleSet());
                m_pkPBuffer.Disable();

                m_spkScene.DetachAllChildren();
                m_spkScene.AttachChild( kNewTarget );
                m_spkScene.UpdateGS();
                m_spkScene.UpdateRS();

                iWidth = iWidth/2;
                iHeight = iHeight/2;                
                /*
                kTarget = ((ShaderEffect)kNewTarget.GetEffect(0)).GetPTexture(0,0);
                m_pkRenderer.GetTexImage( kTarget );
                for ( int i = 0; i < kTarget.GetImage().GetBound(1); i++  )
                {
                    for ( int j = 0; j < kTarget.GetImage().GetBound(0); j++  )
                    {
                        if ( i < 16 && j < 16 )
                            //if (  m_pkSceneTarget.GetImage().GetFloatData()[i] > 1.0 )
                        {
                            System.err.print( kTarget.GetImage().GetFloatData()[i] + " " );
                        }
                    }

                    if ( i < 16 )
                    {
                        System.err.println( "" );
                    }
                }
                */
            }

            float fMax = 0;
            if ( kNewTarget != null )
            {
                kTarget = ((ShaderEffect)kNewTarget.GetEffect(0)).GetPTexture(0,0);
                m_pkRenderer.GetTexImage( kTarget );
                for ( int i = 0; i < kTarget.GetImage().GetBound(1); i++  )
                {
                    for ( int j = 0; j < kTarget.GetImage().GetBound(0); j++  )
                    {
                        if ( (i==0) && (j==0) )
                        {
                            fMax = kTarget.GetImage().GetFloatData()[i];
                        }
                        if ( fMax < kTarget.GetImage().GetFloatData()[i] )
                        {
                            fMax = kTarget.GetImage().GetFloatData()[i];
                        }
                    }
                }
            }
            System.err.println( kImageName + " " + fMax );

            m_spkScene.DetachAllChildren();
            CreatePlaneNode();
            ScaledTextureEffect kScaledTexture =
                new ScaledTextureEffect(kImageName, 1.0f/(float)Math.log(fMax) );
            m_pkPlane.AttachEffect( kScaledTexture );
            m_spkScene.UpdateGS();
            m_spkScene.UpdateRS();
            m_pkRenderer.LoadResources(m_pkPlane);
            /*
            kTarget = ((ShaderEffect)m_pkPlane.GetEffect(0)).GetPTexture(0,0);
            m_pkRenderer.GetTexImage( kTarget );
            for ( int i = 0; i < kTarget.GetImage().GetBound(1); i++  )
            {
                for ( int j = 0; j < kTarget.GetImage().GetBound(0); j++  )
                {
                    if ( i < 16 && j < 16 )
                        //if (  m_pkSceneTarget.GetImage().GetFloatData()[i] > 1.0 )
                    {
                        System.err.print( kTarget.GetImage().GetFloatData()[i] + " " );
                    }
                }

                if ( i < 16 )
                {
                    System.err.println( "" );
                }
            }
            */
        }
        reshape( m_kGLAutoDrawable, 0,0, 256, 256 );
        m_kCuller.ComputeVisibleSet(m_spkScene);
        m_pkRenderer.ClearBuffers();
        m_pkRenderer.DrawScene(m_kCuller.GetVisibleSet());
        m_pkRenderer.DisplayBackBuffer();
        //m_bDisplay = false;
    }


    public void dispose(GLAutoDrawable arg0)
    {
        super.dispose(arg0);
    }

    public void init(GLAutoDrawable arg0) {
        m_kGLAutoDrawable = arg0;
        super.init(arg0);
    }
    
    public void reshape(GLAutoDrawable arg0, int iX, int iY, int iWidth, int iHeight)
    {
        if ( !m_bFirst )
        {
            super.reshape( arg0, iX, iY, iWidth, iHeight );
        }
    }


    protected void CreateImageMesh()
    {
        m_kOffScreenScene = new Node();        
        Attributes kAttributes = new Attributes();
        kAttributes.SetPChannels(3);
        kAttributes.SetTChannels(0,3);
        StandardMesh kSM = new StandardMesh(kAttributes);
        m_kImagePoints = kSM.Rectangle(256,256,1.0f,1.0f);
        m_kImagePoints.Type = Geometry.GeometryType.GT_POLYPOINT;
        m_kOffScreenScene.AttachChild(m_kImagePoints);

        m_kAlpha = new AlphaState();
        m_kAlpha.BlendEnabled = true;
        m_kAlpha.SrcBlend = AlphaState.SrcBlendMode.SBF_ONE;
        m_kAlpha.DstBlend = AlphaState.DstBlendMode.DBF_ONE;
        m_kOffScreenScene.AttachGlobalState(m_kAlpha);

        m_kWireframe = new WireframeState();
        m_kWireframe.Enabled = true;
        m_kWireframe.Fill = WireframeState.FillMode.FM_POINT;
        m_kOffScreenScene.AttachGlobalState(m_kWireframe);

        m_kOffScreenScene.UpdateGS();
        m_kOffScreenScene.UpdateRS();
    }
    

    protected TriMesh CreateLocalPlaneNode()
    {
        Attributes kAttributes = new Attributes();
        kAttributes.SetPChannels(3);
        kAttributes.SetTChannels(0,3);
        StandardMesh kSM = new StandardMesh(kAttributes);
        TriMesh kPlane = kSM.Rectangle(2,2,1.0f,1.0f);
        /*
        float fX0 = -1f;
        float fX1 =  1f;
        float fY0 = -1f;
        float fY1 =  1f;

        kPlane.VBuffer.SetPosition3(0, fX0, fY0, 0);
        kPlane.VBuffer.SetTCoord3(0, 0, 0,0,0.5f);

        kPlane.VBuffer.SetPosition3(1, fX1, fY0, 0);
        kPlane.VBuffer.SetTCoord3(0, 1, 1,0,0.5f);

        kPlane.VBuffer.SetPosition3(2, fX0, fY1, 0);
        kPlane.VBuffer.SetTCoord3(0, 2, 0,1,0.5f);

        kPlane.VBuffer.SetPosition3(3, fX1, fY1, 0);
        kPlane.VBuffer.SetTCoord3(0, 3, 1,1,0.5f);
        */
        return kPlane;
    }

    protected void CreateScene ()
    {   
        CreateImageMesh();
        m_spkEffectPoints = new VolumeHistogramEffect( m_kVolumeImage, null, true );
        m_kImagePoints.AttachEffect(m_spkEffectPoints);
        m_pkRenderer.LoadResources(m_kImagePoints);
        
        int iSize = Math.min( m_iWidth, m_iHeight );
        m_iCount = (int)Mathf.Log2( iSize );
        System.err.println( iSize + " " + m_iCount );

        m_spkScene = new Node();
        m_pkPlane = CreateLocalPlaneNode();
        m_spkScene.AttachChild(m_pkPlane);
        CreateRenderTarget( iSize, m_iWidth, m_iHeight, m_pkPlane );
    }
    
    private void CreateRenderTarget( int i, int iWidth, int iHeight, TriMesh kPlane )
    {

        m_iWidth = iWidth;
        m_iHeight = iHeight;
        m_spkCamera.Perspective = false;
        m_spkCamera.SetFrustum(-1,1,-1,1,1f,10.0f);
        m_pkRenderer.OnFrustumChange();
        m_pkRenderer.Resize(iWidth,iHeight);
        m_kGLAutoDrawable.setSize(iWidth,iHeight);
        
        float[] afData = new float[iWidth*iHeight];
        String kImageName = "HistogramImage" + i;
        
        //System.err.println( iWidth + " " + iHeight + " " + kImageName );
        GraphicsImage pkSceneImage = new GraphicsImage(GraphicsImage.FormatMode.IT_L32F,iWidth,iHeight,afData,
                kImageName);

        // Create the texture effect for the scene polygon.  The resources are
        // loaded so that the scene target texture has everything needed for
        // FrameBuffer::Create to succeed.
        ImageReduceEffect pkEffect = new ImageReduceEffect(kImageName, iWidth, iHeight);
        Texture kSceneTarget = pkEffect.GetPTexture(0,0);
        kSceneTarget.SetFilterType(Texture.FilterType.NEAREST);
        kSceneTarget.SetWrapType(0,Texture.WrapType.CLAMP);
        kSceneTarget.SetWrapType(1,Texture.WrapType.CLAMP);
        //m_pkSceneTarget = kSceneTarget;

        kPlane.AttachEffect(pkEffect);
        kPlane.UpdateGS();
        kPlane.UpdateRS();
        m_pkRenderer.LoadResources(kPlane);

        if ( m_pkPBuffer != null )
        {
            m_pkPBuffer.TerminateBuffer();
            m_pkPBuffer.dispose();
            m_pkPBuffer = null;
        }
        m_pkPBuffer = new OpenGLFrameBuffer(m_eFormat,m_eDepth,m_eStencil,
                m_eBuffering,m_eMultisampling,m_pkRenderer,kSceneTarget,m_kGLAutoDrawable,0);



    }

    protected void UpdateSlice( float fZ )
    {
        //m_spkEffectPlane.ZSlice( fZ );
        m_spkEffectPoints.ZSlice( fZ );
        m_bDisplay = true;        
    }
}
