package gov.nih.mipav.view.renderer.WildMagic;

import gov.nih.mipav.*;
import gov.nih.mipav.model.file.*;
import gov.nih.mipav.model.structures.*;

import gov.nih.mipav.view.*;

import java.awt.*;
import java.awt.event.*;

import javax.media.opengl.*;
import com.sun.opengl.util.*;

import gov.nih.mipav.view.WildMagic.LibApplications.OpenGLApplication.*;
import gov.nih.mipav.view.WildMagic.LibFoundation.Mathematics.*;
import gov.nih.mipav.view.WildMagic.LibGraphics.Effects.*;
import gov.nih.mipav.view.WildMagic.LibGraphics.SceneGraph.*;
import gov.nih.mipav.view.WildMagic.LibGraphics.Rendering.*;
import gov.nih.mipav.view.WildMagic.LibRenderers.OpenGLRenderer.*;

public class VolumeImageViewer extends JavaApplication3D
    implements GLEventListener, KeyListener
{
    public VolumeImageViewer( VolumeImage kVolumeImage )
    {
        super( "MultiTextures", 0, 0,
               kVolumeImage.GetImage().getExtents()[0],
               kVolumeImage.GetImage().getExtents()[1],
               new ColorRGBA( 0.0f,0.25f,0.75f,1.0f ) );
        m_pkRenderer = new OpenGLRenderer( m_eFormat, m_eDepth, m_eStencil,
                                          m_eBuffering, m_eMultisampling,
                                           m_iWidth, m_iHeight );
        ((OpenGLRenderer)m_pkRenderer).GetCanvas().addGLEventListener( this );       
        m_kVolumeImage = kVolumeImage;
    }

    public void SetAnimator( Animator kAnimator )
    {
        m_kAnimator = kAnimator;
    }

    public void SetFrame( Frame kFrame )
    {
        m_kFrame = kFrame;
    }


    /**
     * @param args
     */
    public static void main( VolumeImage kVolumeImage ) {
        VolumeImageViewer kWorld = new VolumeImageViewer(kVolumeImage);
        Frame frame = new Frame(kWorld.GetWindowTitle());
        //GLCanvas canvas = new GLCanvas();
  
        frame.add( kWorld.GetCanvas() );
        frame.setSize(kWorld.GetWidth(),
                      kWorld.GetHeight() );
        /* Animator serves the purpose of the idle function, calls display: */
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
                }
            });
        frame.setVisible(true);

        int iTop = frame.getInsets().top;
        int iBottom =  frame.getInsets().bottom;
        int iRight = frame.getInsets().right;
        int iLeft =  frame.getInsets().left;

        frame.setVisible(false);
        frame.setSize(kWorld.GetWidth() + iRight + iLeft,
                      kWorld.GetHeight() + iTop + iBottom );
        frame.setVisible(true);

        kWorld.SetAnimator(animator);
        kWorld.SetFrame(frame);

        // and all the rest happens in the display function...
        animator.start();
    }

    public void display(GLAutoDrawable arg0) {
        while ( m_bDisplayFirst )
        {
            UpdateSlice(m_iSlice);
            m_pkPlane.DetachAllEffects();
            m_pkPlane.AttachEffect(m_spkEffect);
            m_kCuller.ComputeVisibleSet(m_spkScene);
            m_pkRenderer.ClearBuffers();
            if (m_pkRenderer.BeginScene())
            {          
                m_pkRenderer.DrawScene(m_kCuller.GetVisibleSet());
                m_pkRenderer.EndScene();
            }
            m_pkRenderer.LoadSubTexture( m_pkVolumeNormalTarget, m_iSlice );
            m_pkRenderer.DisplayBackBuffer();
            m_iSlice++; 
            if ( m_iSlice >= m_kVolumeImage.GetImage().getExtents()[2])
            {
                m_iSlice = 0;
                m_bDisplayFirst = false;
                System.err.println("Done first pass");
            }
        }
        while ( m_bDisplaySecond )
        {
            UpdateSlice(m_iSlice);
            m_pkPlane.DetachAllEffects();
            m_pkPlane.AttachEffect(m_spkEffect2);
            m_kCuller.ComputeVisibleSet(m_spkScene);
            m_pkRenderer.ClearBuffers();
            if (m_pkRenderer.BeginScene())
            {          
                m_pkRenderer.DrawScene(m_kCuller.GetVisibleSet());
                m_pkRenderer.EndScene();
            }
            m_pkRenderer.LoadSubTexture( m_kVolumeImage.GetNormalMapTarget(), m_iSlice );
            m_pkRenderer.DisplayBackBuffer();
            m_iSlice++; 
            if ( m_iSlice >= m_kVolumeImage.GetImage().getExtents()[2])
            {
                m_bDisplaySecond = false;
                m_iSlice = 0;
                System.err.println("Done second pass");
            }
        }
        m_kAnimator.stop();
        m_kFrame.setVisible(false);
    }

    public void displayChanged(GLAutoDrawable arg0, boolean arg1, boolean arg2) { }


    public void init(GLAutoDrawable arg0) {
        arg0.setAutoSwapBufferMode( false );

        ((OpenGLRenderer)m_pkRenderer).SetDrawable( arg0 );
        ((OpenGLRenderer)m_pkRenderer).InitializeState();

        super.OnInitialize();

        // set up camera
        m_spkCamera.SetFrustum(60.0f,1.0f,0.1f,100.0f);
        Vector3f kCLoc = new Vector3f(0.0f,0.0f,2.0f);
        Vector3f kCDir = Vector3f.UNIT_Z_NEG;
        Vector3f kCUp = Vector3f.UNIT_Y;
        Vector3f kCRight = Vector3f.UNIT_X;
        m_spkCamera.SetFrame(kCLoc,kCDir,kCUp,kCRight);

        m_spkCamera.Perspective = false;
        m_spkCamera.SetFrustum(90.0f,m_iWidth/(float)m_iHeight,1f,10.0f);
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

        //((OpenGLRenderer)m_pkRenderer).ClearDrawable( );
    }

    public void reshape(GLAutoDrawable arg0, int iX, int iY, int iWidth, int iHeight)
    {
        if ( (iWidth != m_kVolumeImage.GetImage().getExtents()[0]) || 
                (iHeight != m_kVolumeImage.GetImage().getExtents()[1])  )
        {
            return;
        }
        if (iWidth > 0 && iHeight > 0)
        {
            m_iWidth = iWidth;
            m_iHeight = iHeight;
            m_spkCamera.Perspective = false;
            m_spkCamera.SetFrustum(90.0f,m_iWidth/(float)m_iHeight,1f,10.0f);
            m_pkRenderer.OnFrustumChange();

            if (m_pkRenderer != null)
            {
                m_pkRenderer.Resize(iWidth,iHeight);
            }
        }
    }

    public GLCanvas GetCanvas()
    {
        return ((OpenGLRenderer)m_pkRenderer).GetCanvas();
    }

    private void CreateScene ()
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

        m_spkEffect = new VolumeCalcEffect( m_kVolumeImage );
        m_pkPlane.AttachEffect(m_spkEffect);
        m_pkRenderer.LoadResources(m_pkPlane);
        m_spkEffect.SetStepSize(m_kVolumeImage);
        m_pkPlane.DetachAllEffects();

        GraphicsImage kImage = new GraphicsImage(GraphicsImage.FormatMode.IT_RGB888,m_iWidth,m_iHeight,
                                                 m_kVolumeImage.GetImage().getExtents()[2],(byte[])null,
                                            "VolumeNormals");
        m_pkVolumeNormalTarget = new Texture();
        m_pkVolumeNormalTarget.SetImage(kImage);
        m_spkEffect2 = new VolumeCalcEffect( "VolumeNormals", m_pkVolumeNormalTarget );
        m_pkPlane.AttachEffect(m_spkEffect2);
        m_pkRenderer.LoadResources(m_pkPlane);
        m_spkEffect.SetStepSize(m_kVolumeImage);
        m_pkPlane.DetachAllEffects();

        m_spkScene.AttachChild(m_pkPlane);
    }
    
    private void UpdateSlice( int iZ )
    {
        float fZ = ((float)iZ)/(m_kVolumeImage.GetImage().getExtents()[2] -1);
        m_pkPlane.VBuffer.SetTCoord3(0, 0, 0,0, fZ);
        m_pkPlane.VBuffer.SetTCoord3(0, 1, 1,0, fZ);
        m_pkPlane.VBuffer.SetTCoord3(0, 2, 0,1, fZ);
        m_pkPlane.VBuffer.SetTCoord3(0, 3, 1,1, fZ);    
        m_pkPlane.VBuffer.Release();
    }

    private Node m_spkScene;
    private Culler m_kCuller = new Culler(0,0,null);
    private VolumeImage m_kVolumeImage;
    private VolumeCalcEffect m_spkEffect;
    private VolumeCalcEffect m_spkEffect2;
    private TriMesh m_pkPlane;
    private int m_iSlice = 0;

    /** Texture for the first-pass rendering of the proxy-geometry: */
    private Texture m_pkVolumeNormalTarget;
    private boolean m_bDisplayFirst = true;
    private boolean m_bDisplaySecond = true;
    private Animator m_kAnimator;
    private Frame m_kFrame;
}
