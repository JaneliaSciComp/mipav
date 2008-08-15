package gov.nih.mipav.view.renderer.WildMagic.Render;

import javax.media.opengl.*;
import com.sun.opengl.util.*;

import java.awt.*;
import java.awt.event.*;

import WildMagic.LibApplications.OpenGLApplication.JavaApplication3D;
import WildMagic.LibFoundation.Mathematics.*;
import WildMagic.LibGraphics.Rendering.*;
import WildMagic.LibGraphics.SceneGraph.*;
import WildMagic.LibGraphics.Shaders.*;
import WildMagic.LibRenderers.OpenGLRenderer.*;

public class SurfaceMaterialDisplay extends JavaApplication3D
implements GLEventListener, KeyListener
{
    public SurfaceMaterialDisplay( MaterialState kMaterial, Light[] akLights, boolean bMain )
    {
        super("Lighting",0,0,512,512, new ColorRGBA(ColorRGBA.BLACK));
        m_pkRenderer = new OpenGLRenderer( m_eFormat, m_eDepth, m_eStencil,
                m_eBuffering, m_eMultisampling,
                m_iWidth, m_iHeight );
        ((OpenGLRenderer)m_pkRenderer).GetCanvas().addGLEventListener( this );       
        ((OpenGLRenderer)m_pkRenderer).GetCanvas().addKeyListener( this );       
        ((OpenGLRenderer)m_pkRenderer).GetCanvas().addMouseListener( this );       
        ((OpenGLRenderer)m_pkRenderer).GetCanvas().addMouseMotionListener( this );       

        String kExternalDirs = getExternalDirs();        
        ImageCatalog.SetActive( new ImageCatalog("Main", kExternalDirs) );      
        VertexProgramCatalog.SetActive(new VertexProgramCatalog("Main", kExternalDirs));       
        PixelProgramCatalog.SetActive(new PixelProgramCatalog("Main", kExternalDirs));
        CompiledProgramCatalog.SetActive(new CompiledProgramCatalog());

        m_pkMaterial = kMaterial;
        m_akLights = akLights;
        m_bMain = bMain;
    }


    /**
     * @param args
     */
    public static void main(MaterialState kMaterial, Light[] akLights ) {
        SurfaceMaterialDisplay kWorld = new SurfaceMaterialDisplay(kMaterial, akLights, true);
        Frame frame = new Frame(kWorld.GetWindowTitle());

        frame.add( kWorld.GetCanvas() );
        frame.setSize(kWorld.GetWidth(), kWorld.GetHeight());
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
        animator.start();
        // and all the rest happens in the display function...

    }

    public void display(GLAutoDrawable arg0) {
        m_pkRenderer.ClearBuffers();
        if (m_pkRenderer.BeginScene())
        {
            m_pkRenderer.DrawScene(m_kCuller.GetVisibleSet());
            m_pkRenderer.EndScene();
        }
        m_pkRenderer.DisplayBackBuffer();
    }

    public void displayChanged(GLAutoDrawable arg0, boolean arg1, boolean arg2) {
        // TODO Auto-generated method stub

    }

    public void init(GLAutoDrawable arg0) {
        m_bInit = true;

        arg0.setAutoSwapBufferMode( false );

        ((OpenGLRenderer)m_pkRenderer).SetDrawable( arg0 );
        ((OpenGLRenderer)m_pkRenderer).InitializeState();

        super.OnInitialize();

        // set up camera
        m_spkCamera.SetFrustum(60.0f,m_iWidth/(float)m_iHeight,0.01f,10.0f);
        Vector3f kCDir = new Vector3f(0.0f,0.0f,1.0f);
        Vector3f kCUp = new Vector3f(0.0f, -1.0f,0.0f);
        Vector3f kCRight = new Vector3f();
        kCRight.Cross( kCDir, kCUp );
        Vector3f kCLoc = new Vector3f(kCDir);
        if ( m_bMain )
        {
            kCLoc.Scale(-2.4f);
        }
        else
        {
            kCLoc.Scale(-1.4f);
        }
        m_spkCamera.SetFrame(kCLoc,kCDir,kCUp,kCRight);

        CreateScene();

        // initial update of objects
        m_spkScene.UpdateGS();
        m_spkScene.UpdateRS();

        // initial culling of scene
        m_kCuller.SetCamera(m_spkCamera);
        m_kCuller.ComputeVisibleSet(m_spkScene);

        // ((OpenGLRenderer)m_pkRenderer).ClearDrawable( );
    }

    public void reshape(GLAutoDrawable arg0, int iX, int iY, int iWidth, int iHeight) {
        //((OpenGLRenderer)m_pkRenderer).SetDrawable( arg0 );
        if (iWidth > 0 && iHeight > 0)
        {
            if (m_pkRenderer != null)
            {
                m_pkRenderer.Resize(iWidth,iHeight);
            }

            m_iWidth = iWidth;
            m_iHeight = iHeight;
            m_spkCamera.SetFrustum(60.0f,m_iWidth/(float)m_iHeight,0.01f,10.0f);
        }
        //((OpenGLRenderer)m_pkRenderer).ClearDrawable( );
    }

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
        StandardMesh kSM = new StandardMesh(kAttr);
        m_spkSphere = kSM.Sphere(64,64,1.0f);

        m_spkSphere.AttachGlobalState(m_pkMaterial);

        m_kLightShader = new MipavLightingEffect( );
        m_spkSphere.AttachEffect(m_kLightShader);
        m_spkScene.AttachChild(m_spkSphere);
        m_pkRenderer.LoadResources(m_spkSphere);
        updateLighting( m_akLights );
    }

    public void setMaterial( MaterialState kMaterial )
    {
    	System.err.println( "setMaterial "  + kMaterial.Shininess );
    	
        //m_pkMaterial.Ambient.Copy( kMaterial.Ambient );
        //m_pkMaterial.Diffuse.Copy( kMaterial.Diffuse );
        //m_pkMaterial.Specular.Copy( kMaterial.Specular );
        //m_pkMaterial.Emissive.Copy( kMaterial.Emissive );
        //m_pkMaterial.Shininess = kMaterial.Shininess;
        //m_pkMaterial.Shininess = 1.0f;
        //m_spkScene.UpdateGS();
    }

    /**
     * Called from JPanelLight. Updates the lighting parameters.
     * @param akGLights, the set of GeneralLight objects.
     */
    public void updateLighting(Light[] akGLights )
    {
        if ( akGLights == null )
        {
            return;
        }
        if ( m_bInit )
        {
            for ( int i = 0; i < akGLights.length; i++ )
            {
                String kLightType = new String("Light"+(i)+"Type");
                float[] afType = new float[]{0,0,0,0};
                //if ( i < m_pkRenderer.GetMaxLights() )
                if ( i < 4 )
                {
                    if ( akGLights[i].On )
                    {
                        Light kLight = akGLights[i];
                        m_pkRenderer.SetLight( i, kLight );
                        if ( akGLights[i].Type == Light.LightType.LT_AMBIENT )
                        {
                            afType[0] = 0;
                        }
                        else if ( akGLights[i].Type == Light.LightType.LT_DIRECTIONAL )
                        {
                            afType[0] = 1;
                        }
                        else if ( akGLights[i].Type == Light.LightType.LT_POINT )
                        {
                            afType[0] = 2;
                        }
                        else if ( akGLights[i].Type == Light.LightType.LT_SPOT )
                        {
                            afType[0] = 3;
                        }
                        m_kLightShader.SetLight(kLightType, afType);
                    }
                    else
                    {
                        m_pkRenderer.SetLight( i, new Light() );
                        afType[0] = -1;
                        m_kLightShader.SetLight(kLightType, afType);
                    }
                }
            }
        }
    }

    private Node m_spkScene;
    private TriMesh m_spkSphere;
    private Culler m_kCuller = new Culler(0,0,null);

    private MipavLightingEffect m_kLightShader = null;
    private MaterialState m_pkMaterial;
    private boolean m_bInit = false;
    private Light[] m_akLights = null;
    private boolean m_bMain = false;
    
    private String getExternalDirs()
    {
        String jar_filename = "";
        String class_path_key = "java.class.path";
        String class_path = System.getProperty(class_path_key);
        for (String fn : class_path.split(";") ) {
            if (fn.endsWith("WildMagic.jar")) {
                jar_filename = fn;   
                String externalDirs = jar_filename.substring(0, jar_filename.indexOf("lib\\"));
                externalDirs = externalDirs.concat("WildMagic");
                return externalDirs;
            }
        }
        return System.getProperties().getProperty("user.dir");
    }
}
