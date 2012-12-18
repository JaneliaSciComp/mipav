package gov.nih.mipav.view.renderer.WildMagic.Render;

import gov.nih.mipav.util.MipavInitGPU;
import gov.nih.mipav.view.renderer.WildMagic.VolumeTriPlanarInterface;

import java.awt.Frame;
import java.awt.event.KeyEvent;
import java.awt.event.KeyListener;
import java.awt.event.WindowAdapter;
import java.awt.event.WindowEvent;
import java.awt.image.BufferedImage;
import java.io.File;
import java.io.IOException;

import javax.imageio.ImageIO;
import javax.media.opengl.GLAutoDrawable;
import javax.media.opengl.GLEventListener;
import javax.media.opengl.awt.GLCanvas;

import WildMagic.LibApplications.OpenGLApplication.JavaApplication3D;
import WildMagic.LibFoundation.Mathematics.ColorRGBA;
import WildMagic.LibFoundation.Mathematics.Vector3f;
import WildMagic.LibGraphics.Effects.ShaderEffect;
import WildMagic.LibGraphics.Rendering.CullState;
import WildMagic.LibGraphics.SceneGraph.Attributes;
import WildMagic.LibGraphics.SceneGraph.Culler;
import WildMagic.LibGraphics.SceneGraph.Node;
import WildMagic.LibGraphics.SceneGraph.StandardMesh;
import WildMagic.LibGraphics.SceneGraph.TriMesh;
import WildMagic.LibRenderers.OpenGLRenderer.OpenGLRenderer;

import com.jogamp.opengl.util.Animator;

public class VolumeImageViewer extends JavaApplication3D
    implements GLEventListener, KeyListener
{
    /**  */
    private static final long serialVersionUID = -2042201391319899982L;
    protected static int m_iScreenCaptureCounter = 0;
    /**
     * @param args
     */
    public static void main( VolumeTriPlanarInterface kParentFrame, VolumeImage kVolumeImage, boolean bShowFrame )
    {
        final VolumeImageViewer kWorld = new VolumeImageViewer(kParentFrame, kVolumeImage);
        final Frame frame = new Frame(kWorld.GetWindowTitle());
        frame.add( kWorld.GetCanvas() );
        final Animator animator = new Animator( kWorld.GetCanvas() );    
        frame.addWindowListener(new WindowAdapter() {
            @Override
			public void windowClosing(WindowEvent e) {
                // Run this on another thread than the AWT event queue to
                // avoid deadlocks on shutdown on some platforms
                new Thread(new Runnable() {
                    @Override
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
    protected Node m_spkScene;
    protected CullState m_kCull;
    protected Culler m_kCuller = new Culler(0,0,null);
    protected VolumeImage m_kVolumeImage;
    protected ShaderEffect m_spkEffect;
    protected TriMesh m_pkPlane;
    protected int m_iSlice = 0;
    protected Animator m_kAnimator;
    protected Frame m_kFrame;
    protected VolumeTriPlanarInterface m_kParent;
    protected boolean m_bDisplay = true;

    
    protected boolean m_bInit = false;
    protected boolean m_bDispose = false;
    
    public VolumeImageViewer( GLCanvas canvas, VolumeTriPlanarInterface kParentFrame, VolumeImage kVolumeImage )
    {
        super( "VolumeImageViewer", 0, 0,
               kVolumeImage.GetImage().getExtents()[0],
               kVolumeImage.GetImage().getExtents()[1],
               new ColorRGBA( 0.0f,0.0f,0.0f,1.0f ) );
        m_pkRenderer = new OpenGLRenderer( m_eFormat, m_eDepth, m_eStencil,
                                          m_eBuffering, m_eMultisampling,
                                           m_iWidth, m_iHeight, canvas );
        ((OpenGLRenderer)m_pkRenderer).GetCanvas().addGLEventListener( this );    
        ((OpenGLRenderer)m_pkRenderer).GetCanvas().addKeyListener( this );    
        m_kParent = kParentFrame;
        m_kVolumeImage = kVolumeImage;
        MipavInitGPU.InitGPU();
        m_pkRenderer.SetExternalDir(MipavInitGPU.getExternalDirs());
    }
    public VolumeImageViewer( VolumeTriPlanarInterface kParentFrame, VolumeImage kVolumeImage )
    {
        super( "VolumeImageViewer", 0, 0,
               kVolumeImage.GetImage().getExtents()[0],
               kVolumeImage.GetImage().getExtents()[1],
               new ColorRGBA( 0.0f,0.0f,0.0f,1.0f ) );
        m_pkRenderer = new OpenGLRenderer( m_eFormat, m_eDepth, m_eStencil,
                                          m_eBuffering, m_eMultisampling,
                                           m_iWidth, m_iHeight );
        ((OpenGLRenderer)m_pkRenderer).GetCanvas().addGLEventListener( this );    
        ((OpenGLRenderer)m_pkRenderer).GetCanvas().addKeyListener( this );    
        m_kParent = kParentFrame;
        m_kVolumeImage = kVolumeImage;
        MipavInitGPU.InitGPU();
        m_pkRenderer.SetExternalDir(MipavInitGPU.getExternalDirs());
    }

    @Override
	public void display(GLAutoDrawable arg0) {
        if ( m_kAnimator == null )
        {
            return;
        }      
        m_pkPlane.DetachAllEffects();
        m_pkPlane.AttachEffect(m_spkEffect);
        m_kCuller.ComputeVisibleSet(m_spkScene);
        m_pkRenderer.ClearBuffers();
        if (m_pkRenderer.BeginScene())
        {          
            m_pkRenderer.DrawScene(m_kCuller.GetVisibleSet());
            m_pkRenderer.EndScene();
            //writeImage();
        }
        m_pkRenderer.DisplayBackBuffer();
        m_bDisplay = false;
    }



    public void displayChanged(GLAutoDrawable arg0, boolean arg1, boolean arg2) 
    {       
        m_bDisplay = true;
    }
    
    @Override
	public void dispose(GLAutoDrawable arg0)
    {
        if ( m_spkScene != null )
        {
            m_spkScene.dispose();
            m_spkScene = null;
        }
        if ( m_kCuller !=  null )
        {
            m_kCuller.dispose();
            m_kCuller = null;
        }
        m_kVolumeImage = null;
        if ( m_spkEffect != null )
        {
            m_spkEffect.dispose();
            m_spkEffect = null;
        }
        if ( m_pkPlane != null )
        {
            m_pkPlane.dispose();
            m_pkPlane = null;
        }
        if ( m_kAnimator != null )
        {
            m_kAnimator.stop();
            m_kAnimator.remove(arg0);
            m_kAnimator = null;
        }
        if ( m_kFrame != null )
        {
            m_kFrame.setVisible(false);
            //m_kFrame.dispose();
            m_kFrame = null;
        }
        m_kParent = null;
        super.dispose();
    }

    public GLCanvas GetCanvas()
    {
        return ((OpenGLRenderer)m_pkRenderer).GetCanvas();
    }

    @Override
	public void init(GLAutoDrawable arg0) {
        if ( m_bInit )
        {
            return;
        }      
        if ( m_pkRenderer != null )
        {
            ((OpenGLRenderer)m_pkRenderer).SetDrawable( arg0 );
        }
        m_pkRenderer.InitializeState();
        super.OnInitialize();

        // set up camera
        m_spkCamera.SetFrustum(60.0f,1.0f,0.1f,100.0f);
        Vector3f kCLoc = new Vector3f(0.0f,0.0f,2.0f);
        Vector3f kCDir = Vector3f.UNIT_Z_NEG;
        Vector3f kCUp = Vector3f.UNIT_Y;
        Vector3f kCRight = Vector3f.UNIT_X;
        m_spkCamera.SetFrame(kCLoc,kCDir,kCUp,kCRight);

        m_spkCamera.Perspective = false;
        
        m_spkCamera.SetFrustum(-1,1,-1,1,1f,10.0f);
        m_pkRenderer.OnFrustumChange();

        CreateScene();

        // initial update of objects
        m_spkScene.UpdateGS();
        m_spkScene.UpdateRS();

        // initial culling of scene
        m_kCuller.SetCamera(m_spkCamera);
        m_kCuller.ComputeVisibleSet(m_spkScene);

        InitializeCameraMotion(0.001f,0.001f);
        InitializeObjectMotion(m_spkScene);
        m_bInit = true;
    }

    /** 
     * keyPressed callback.
     * @param kKey the KeyEvent triggering the callback.
     */
    @Override
	public void keyPressed(KeyEvent kKey)
    {
        char ucKey = kKey.getKeyChar();
        if ( ucKey == 'q' )
        {
            m_kAnimator.stop();
            m_kFrame.setVisible(false);
        }
        int iKey = kKey.getKeyCode();
        if ( m_kVolumeImage != null )
        {
            if (iKey == KeyEvent.VK_UP)
            {
                m_iSlice++; 
                if ( m_iSlice >= m_kVolumeImage.GetImage().getExtents()[2])
                {
                    m_iSlice = m_kVolumeImage.GetImage().getExtents()[2]-1;
                }            
                float fZ = ((float)m_iSlice)/(m_kVolumeImage.GetImage().getExtents()[2] -1);
                UpdateSlice(fZ);
                return;
            }
            if (iKey == KeyEvent.VK_DOWN)
            {
                m_iSlice--; 
                if ( m_iSlice < 0)
                {
                    m_iSlice = 0;
                }
                float fZ = ((float)m_iSlice)/(m_kVolumeImage.GetImage().getExtents()[2] -1);
                UpdateSlice(fZ);
                return;
            }
        }
    }


    @Override
	public void reshape(GLAutoDrawable arg0, int iX, int iY, int iWidth, int iHeight)
    {      
        if (iWidth > 0 && iHeight > 0)
        {
            m_iWidth = iWidth;
            m_iHeight = iHeight;
            m_spkCamera.Perspective = false;
            m_spkCamera.SetFrustum(-1,1,-1,1,1f,10.0f);
            m_pkRenderer.OnFrustumChange();
            m_pkRenderer.Resize(iWidth,iHeight);
            //arg0.setSize(iWidth,iHeight);
            m_bDisplay = true;
        }
    }
    
    public void SetAnimator( Animator kAnimator )
    {
        m_kAnimator = kAnimator;
    }

    public void SetFrame( Frame kFrame )
    {
        m_kFrame = kFrame;
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

    protected void CreatePlaneNode()
    {
        m_spkScene = new Node();
        Attributes kAttributes = new Attributes();
        kAttributes.SetPChannels(3);
        kAttributes.SetTChannels(0,3);
        StandardMesh kSM = new StandardMesh(kAttributes);
        m_pkPlane = kSM.Rectangle(2,2,1.0f,1.0f);
        
        float fX0 = -1f;
        float fX1 =  1f;
        float fY0 = -1f;
        float fY1 =  1f;

        m_pkPlane.VBuffer.SetPosition3(0, fX0, fY0, 0);
        m_pkPlane.VBuffer.SetTCoord3(0, 0, 0,0,0.5f);

        m_pkPlane.VBuffer.SetPosition3(1, fX1, fY0, 0);
        m_pkPlane.VBuffer.SetTCoord3(0, 1, 1,0,0.5f);

        m_pkPlane.VBuffer.SetPosition3(2, fX0, fY1, 0);
        m_pkPlane.VBuffer.SetTCoord3(0, 2, 0,1,0.5f);

        m_pkPlane.VBuffer.SetPosition3(3, fX1, fY1, 0);
        m_pkPlane.VBuffer.SetTCoord3(0, 3, 1,1,0.5f);

        m_spkScene.AttachChild(m_pkPlane);
    }
    
    protected void CreateScene ()
    {
        CreatePlaneNode();
        m_spkEffect = new VolumePlaneEffect( m_kVolumeImage, null, true, false );
        m_pkPlane.AttachEffect(m_spkEffect);
        m_pkRenderer.LoadResources(m_pkPlane);
        m_pkPlane.DetachAllEffects();
    }


    protected void UpdateSlice( float fZ )
    {
        m_pkPlane.VBuffer.SetTCoord3(0, 0, 0,0, fZ);
        m_pkPlane.VBuffer.SetTCoord3(0, 1, 1,0, fZ);
        m_pkPlane.VBuffer.SetTCoord3(0, 2, 0,1, fZ);
        m_pkPlane.VBuffer.SetTCoord3(0, 3, 1,1, fZ);    
        m_pkPlane.Reload(true);
        m_bDisplay = true;
    }
}
