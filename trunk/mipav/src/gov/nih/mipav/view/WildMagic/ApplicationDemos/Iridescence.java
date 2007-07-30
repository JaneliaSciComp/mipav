package gov.nih.mipav.view.WildMagic.ApplicationDemos;
/**
 * 
 */

import javax.media.opengl.*;
import com.sun.opengl.util.*;

import java.awt.*;
import java.awt.event.*;

import gov.nih.mipav.view.WildMagic.LibApplications.OpenGLApplication.*;
import gov.nih.mipav.view.WildMagic.LibFoundation.Mathematics.*;
import gov.nih.mipav.view.WildMagic.LibGraphics.Effects.*;
import gov.nih.mipav.view.WildMagic.LibGraphics.Detail.*;
import gov.nih.mipav.view.WildMagic.LibGraphics.Rendering.*;
import gov.nih.mipav.view.WildMagic.LibGraphics.SceneGraph.*;
import gov.nih.mipav.view.WildMagic.LibGraphics.Shaders.*;
import gov.nih.mipav.view.WildMagic.LibGraphics.Sorting.*;
import gov.nih.mipav.view.WildMagic.LibRenderers.OpenGLRenderer.*;

/**
 * @author Alexandra
 *
 */
public class Iridescence extends JavaApplication3D
    implements GLEventListener, KeyListener
{
    public Iridescence()
    {
        super("Iridescence",0,0,640,480, new ColorRGBA(0.5f,0.0f,1.0f,1.0f));
        m_pkRenderer = new OpenGLRenderer( m_eFormat, m_eDepth, m_eStencil,
                                          m_eBuffering, m_eMultisampling,
                                           m_iWidth, m_iHeight );
        ((OpenGLRenderer)m_pkRenderer).GetCanvas().addGLEventListener( this );       
        ((OpenGLRenderer)m_pkRenderer).GetCanvas().addKeyListener( this );       
        ((OpenGLRenderer)m_pkRenderer).GetCanvas().addMouseListener( this );       
        ((OpenGLRenderer)m_pkRenderer).GetCanvas().addMouseMotionListener( this );       

        ImageCatalog.SetActive( new ImageCatalog("Main", System.getProperties().getProperty("user.dir")) );      
        VertexProgramCatalog.SetActive(new VertexProgramCatalog("Main", System.getProperties().getProperty("user.dir")));       
        PixelProgramCatalog.SetActive(new PixelProgramCatalog("Main", System.getProperties().getProperty("user.dir")));
    }


    /**
     * @param args
     */
    public static void main(String[] args) {
        Vector3f testVec = new Vector3f(2.0f, 3.0f, 4.0f);
        System.out.println(testVec.X() + " " + testVec.Y() + " " +testVec.Z() + " done.");
        //System.out.println("Hello world!");
        Iridescence kWorld = new Iridescence();        
        Frame frame = new Frame(m_acWindowTitle);

        //GLCanvas canvas = new GLCanvas();
  


        frame.add( kWorld.GetCanvas() );
        frame.setSize(m_iWidth, m_iHeight);
        /* Animator serves the purpose of the idle function, calls display: */
        final Animator animator = new Animator( kWorld.GetCanvas() );
        frame.addWindowListener(new WindowAdapter() {
                public void windowClosing(WindowEvent e) {
                    // Run this on another thread than the AWT event queue to
                    // avoid deadlocks on shutdown on some platforms
                    new Thread(new Runnable() {
                            public void run() {
                                animator.stop();
                                System.exit(0);
                            }
                        }).start();
                }
            });
        frame.setVisible(true);
        animator.start();
        // and all the rest happens in the display function...

    }

    public void display(GLAutoDrawable arg0) {
        ((OpenGLRenderer)m_pkRenderer).SetDrawable( arg0 );

        MeasureTime();
        
        if (MoveCamera())
        {
            m_kCuller.ComputeVisibleSet(m_spkScene);
        }        

        if (MoveObject())
        {
            m_spkScene.UpdateGS();
            m_kCuller.ComputeVisibleSet(m_spkScene);
        }

        m_pkRenderer.ClearBuffers();
        if (m_pkRenderer.BeginScene())
        {
            m_pkRenderer.DrawScene(m_kCuller.GetVisibleSet());
            DrawFrameRate(8,GetHeight()-8,ColorRGBA.WHITE);
            m_pkRenderer.EndScene();
        }
        m_pkRenderer.DisplayBackBuffer();
        UpdateFrameCount();

        ((OpenGLRenderer)m_pkRenderer).ClearDrawable( );

        //ApplicationGUI.TheApplicationGUI.Display();
    }

    public void displayChanged(GLAutoDrawable arg0, boolean arg1, boolean arg2) {
        // TODO Auto-generated method stub
        
    }

    public void init(GLAutoDrawable arg0) {
        arg0.setAutoSwapBufferMode( false );

        ((OpenGLRenderer)m_pkRenderer).SetDrawable( arg0 );
        ((OpenGLRenderer)m_pkRenderer).InitializeState();

        super.OnInitialize();

        m_spkCamera.SetFrustum(-0.55f,0.55f,-0.4125f,0.4125f,1.0f,100.0f);
        Vector3f kCLoc = new Vector3f(0.0f,0.0f,-8.0f);
        Vector3f kCDir = new Vector3f(0.0f,0.0f,1.0f);
        Vector3f kCUp = new Vector3f(0.0f,1.0f,0.0f);
        Vector3f kCRight = kCDir.Cross(kCUp);
        m_spkCamera.SetFrame(kCLoc,kCDir,kCUp,kCRight);

        CreateScene();

        // initial update of objects
        m_spkScene.UpdateGS();
        m_spkScene.UpdateRS();

        // initial culling of scene
        m_kCuller.SetCamera(m_spkCamera);
        m_kCuller.ComputeVisibleSet(m_spkScene);

        InitializeCameraMotion(0.01f,0.001f);
        InitializeObjectMotion(m_spkScene);

        ((OpenGLRenderer)m_pkRenderer).ClearDrawable( );
    }

    public void reshape(GLAutoDrawable arg0, int iX, int iY, int iWidth, int iHeight) {
        ((OpenGLRenderer)m_pkRenderer).SetDrawable( arg0 );
        if (iWidth > 0 && iHeight > 0)
        {
            if (m_pkRenderer != null)
            {
                m_pkRenderer.Resize(iWidth,iHeight);
            }
            
            m_iWidth = iWidth;
            m_iHeight = iHeight;
        }
        ((OpenGLRenderer)m_pkRenderer).ClearDrawable( );
    }

    /*
    public GLCanvas GetCanvas()
    {
        return ((OpenGLRenderer)m_pkRenderer).GetCanvas();
    }
    */
    public GLCanvas GetCanvas()
    {
        return ((OpenGLRenderer)m_pkRenderer).GetCanvas();
    }

    private void CreateScene ()
    {
        m_spkScene = new Node();
        Attributes kAttr = new Attributes();
        kAttr.SetPChannels(3);
        kAttr.SetNChannels(3);
        kAttr.SetTChannels(0,2);
        StandardMesh kSM = new StandardMesh(kAttr);
        TriMesh pkMesh = kSM.Torus(20,20,2.0f,1.0f);
        m_spkScene.AttachChild(pkMesh);

        m_spkEffect = new IridescenceEffect("Leaf","Gradient");
        m_spkEffect.SetInterpolateFactor(0.5f);
        final int iPassQuantity = m_spkEffect.GetPassQuantity();
        for (int iPass = 0; iPass < iPassQuantity; iPass++)
        {
            m_spkEffect.LoadPrograms(iPass,m_pkRenderer.GetMaxColors(),m_pkRenderer.GetMaxTCoords(),
                    m_pkRenderer.GetMaxVShaderImages(),m_pkRenderer.GetMaxPShaderImages());
        }

        pkMesh.AttachEffect(m_spkEffect);
    }

    public void keyPressed(KeyEvent e) {
        char ucKey = e.getKeyChar();
        
        super.keyPressed(e);

        float fInterpolateFactor;

        switch (ucKey)
        {
        case '+':
        case '=':
            fInterpolateFactor = m_spkEffect.GetInterpolateFactor();
            fInterpolateFactor += 0.1f;
            if (fInterpolateFactor > 1.0f)
            {
                fInterpolateFactor = 1.0f;
            }

            m_spkEffect.SetInterpolateFactor(fInterpolateFactor);
            return;

        case '-':
        case '_':
            fInterpolateFactor = m_spkEffect.GetInterpolateFactor();
            fInterpolateFactor -= 0.1f;
            if (fInterpolateFactor < 0.0f)
            {
                fInterpolateFactor = 0.0f;
            }

            m_spkEffect.SetInterpolateFactor(fInterpolateFactor);
            return;
        case 's':
        case 'S':
            TestStreaming(m_spkScene,"Iridescence.wmof");
            return;
       }
        return;
    }

    private Node m_spkScene;
    private IridescenceEffect m_spkEffect;
    private Culler m_kCuller = new Culler(0,0,null);

}
