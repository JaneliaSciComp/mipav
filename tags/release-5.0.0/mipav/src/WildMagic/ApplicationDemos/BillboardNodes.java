// Wild Magic Source Code
// David Eberly
// http://www.geometrictools.com
// Copyright (c) 1998-2007
//
// This library is free software; you can redistribute it and/or modify it
// under the terms of the GNU Lesser General Public License as published by
// the Free Software Foundation; either version 2.1 of the License, or (at
// your option) any later version.  The license is available for reading at
// either of the locations:
//     http://www.gnu.org/copyleft/lgpl.html
//     http://www.geometrictools.com/License/WildMagicLicense.pdf
//
// Version: 4.0.0 (2006/06/28)
//
// Ported to Java by Alexandra Bokinsky, PhD, Geometric Tools, Inc. (July 2007)
//

package WildMagic.ApplicationDemos;

import javax.media.opengl.*;
import com.sun.opengl.util.*;

import java.awt.*;
import java.awt.event.*;
import java.util.Vector;

import WildMagic.LibApplications.OpenGLApplication.*;
import WildMagic.LibFoundation.Mathematics.*;
import WildMagic.LibGraphics.Effects.*;
import WildMagic.LibGraphics.Detail.*;
import WildMagic.LibGraphics.ObjectSystem.*;
import WildMagic.LibGraphics.Rendering.*;
import WildMagic.LibGraphics.SceneGraph.*;
import WildMagic.LibGraphics.Shaders.*;
import WildMagic.LibRenderers.OpenGLRenderer.*;

public class BillboardNodes extends JavaApplication3D
    implements GLEventListener, KeyListener
{
    public BillboardNodes()
    {
        super("BillboardNodes",0,0,640,480, ColorRGBA.WHITE);
        m_eDepth = FrameBuffer.DepthType.DT_DEPTH_NONE;
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
    }


    /**
     * @param args
     */
    public static void main(String[] args) {
        Vector3f testVec = new Vector3f(2.0f, 3.0f, 4.0f);
        System.out.println(testVec.X + " " + testVec.Y + " " +testVec.Z + " done.");
        //System.out.println("Hello world!");

        BillboardNodes kWorld = new BillboardNodes();
        Frame frame = new Frame(kWorld.GetWindowTitle());
        //GLCanvas canvas = new GLCanvas();
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

        m_pkRenderer.SetCamera( m_spkCamera );
        if (MoveCamera())
        {
            m_spkBillboard0.UpdateGS();
            m_spkBillboard1.UpdateGS();
            m_kCuller.ComputeVisibleSet(m_spkScene);
        }
        
        if (MoveObject())
        {
            //m_spkScene.UpdateGS();
            m_kCuller.ComputeVisibleSet(m_spkScene);
        }
        
        // First: render opaque objects to an off-screen color/depth buffer
        //m_kSolidFBO.Enable();        
        m_kFBO.Enable();
        m_kFBO.DrawBuffers(new int[]{0});
        m_pkRenderer.SetBackgroundColor( ColorRGBA.WHITE );
        m_pkRenderer.ClearBuffers();

        m_spkScene.DetachGlobalState(GlobalState.StateType.ALPHA);
        m_spkScene.DetachGlobalState(GlobalState.StateType.ZBUFFER);
        //m_spkScene.AttachGlobalState(m_kAlpha1);
        //m_kZBuffer.Writable = true;
        m_spkScene.DetachChild(m_spkBillboard0);
        m_spkScene.DetachChild(m_spkBillboard1);
        m_spkScene.UpdateGS();
        m_spkScene.UpdateRS();
        m_kCuller.ComputeVisibleSet(m_spkScene);
        m_pkRenderer.DrawScene(m_kCuller.GetVisibleSet());
        //m_kSolidFBO.Disable();   
        
        // Second: render translucent objects, accumulating color and depth-complexity count.
        // Note: we share the depth buffer from rendering the opaque objects.
        // clear the color buffer, but NOT the depth buffer. 
        //m_pkRenderer.ClearBackBuffer();
        //m_pkRenderer.ClearBuffers();
        m_kFBO.DrawBuffers(new int[]{1,2});
        m_pkRenderer.SetBackgroundColor( new ColorRGBA(0.0f, 0.0f,0.0f,0.0f));
        m_pkRenderer.ClearBackBuffer();
        m_kZBuffer.Writable = false;
        m_spkScene.AttachGlobalState(m_kZBuffer);
        //m_spkScene.DetachGlobalState(GlobalState.StateType.ALPHA);
        m_spkScene.AttachGlobalState(m_kAlpha);
        m_spkScene.AttachChild(m_spkBillboard0);
        m_spkScene.AttachChild(m_spkBillboard1);
        m_spkScene.DetachChild( m_kGround );
        m_spkScene.UpdateGS();
        m_spkScene.UpdateRS();
        m_kCuller.ComputeVisibleSet(m_spkScene);
        m_pkRenderer.DrawScene(m_kCuller.GetVisibleSet());
        
        m_kFBO.Disable();

        m_spkScene.AttachChild( m_kGround );
        
        
        // Last: render screen-quad, which 
        m_spkScene.DetachGlobalState(GlobalState.StateType.ALPHA);
        m_spkScene.DetachGlobalState(GlobalState.StateType.ZBUFFER);

        m_pkRenderer.SetCamera(m_pkScreenCamera);
        m_pkRenderer.SetBackgroundColor( ColorRGBA.WHITE );
        m_pkRenderer.ClearBuffers();
        m_pkRenderer.Draw( m_pkPlane );        
        m_pkRenderer.DisplayBackBuffer();

        //((OpenGLRenderer)m_pkRenderer).ClearDrawable( );
    }

    public void displayChanged(GLAutoDrawable arg0, boolean arg1, boolean arg2) {
        // TODO Auto-generated method stub
        
    }

    public void init(GLAutoDrawable arg0) {
        ((OpenGLRenderer)m_pkRenderer).SetDrawable( arg0 );
        m_pkRenderer.InitializeState();
        super.OnInitialize();

        // set up camera
        m_spkCamera.SetFrustum(-0.055f,0.055f,-0.04125f,0.04125f,0.1f,100.0f);
        Vector3f kCLoc = new Vector3f(0.0f,-1.0f,0.25f);
        Vector3f kCDir = new Vector3f(0.0f,1.0f,0.0f);
        Vector3f kCUp = new Vector3f(0.0f,0.0f,1.0f);
        Vector3f kCRight = new Vector3f();
        kCRight.Cross( kCDir, kCUp );
        m_spkCamera.SetFrame(kCLoc,kCDir,kCUp,kCRight);
        
        
        m_pkScreenCamera = new Camera();
        m_pkScreenCamera.Perspective = false;
        m_pkScreenCamera.SetFrustum(-1,1,-1,1,1f,10.0f);

        CreateScene(arg0);

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
    

    public void reshape(GLAutoDrawable arg0, int iX, int iY, int iWidth, int iHeight) {
        //((OpenGLRenderer)m_pkRenderer).SetDrawable( arg0 );
        /*
        if (iWidth > 0 && iHeight > 0)
        {
            if (m_pkRenderer != null)
            {
                m_pkRenderer.Resize(iWidth,iHeight);
            }
            
            m_iWidth = iWidth;
            m_iHeight = iHeight;
        }
        */
        //((OpenGLRenderer)m_pkRenderer).ClearDrawable( );
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


    private void CreateScene (GLAutoDrawable arg0)
    {
        CreateRenderTarget(arg0, m_iWidth, m_iHeight);
        
        m_spkScene = new Node();
        m_spkWireframe = new WireframeState();
        m_spkScene.AttachGlobalState(m_spkWireframe);

        m_kAlpha1 = new AlphaState();
        m_kAlpha1.BlendEnabled = true;
        m_pkPlane.AttachGlobalState(m_kAlpha1);
        m_pkPlane.UpdateGS();
        m_pkPlane.UpdateRS();
        //m_spkScene.AttachGlobalState(m_kAlpha1);
        
        
        m_kAlpha = new AlphaState();
        m_kAlpha.BlendEnabled = true;
        m_kAlpha.SrcBlend = AlphaState.SrcBlendMode.SBF_ONE;
        m_kAlpha.DstBlend = AlphaState.DstBlendMode.DBF_ONE;
        m_spkScene.AttachGlobalState(m_kAlpha);
        
        m_kZBuffer = new ZBufferState();
        m_kZBuffer.Enabled = true;
        m_kZBuffer.Writable = false;
        m_spkScene.AttachGlobalState(m_kZBuffer);

        // Create the ground.  It covers a square with vertices (1,1,0), (1,-1,0),
        // (-1,1,0), and (-1,-1,0).  Multiply the texture coordinates by a factor
        // to enhance the wrap-around.
        Attributes kAttr = new Attributes();
        kAttr.SetPChannels(3);
        kAttr.SetTChannels(0,2);
        StandardMesh kSM = new StandardMesh(kAttr);
        m_kGround = kSM.Rectangle(2,2,16.0f,16.0f);
        int i;
        for (i = 0; i < m_kGround.VBuffer.GetVertexQuantity(); i++)
        {
            m_kGround.VBuffer.SetTCoord2(0,i, 
                    m_kGround.VBuffer.GetTCoord2fX(0,i) * 128.0f,
                    m_kGround.VBuffer.GetTCoord2fY(0,i) * 128.0f );

        }

        //ShaderEffect pkEffect = new TextureEffect("Horizontal");
        ShaderEffect pkEffect = new MaterialTextureEffect("Horizontal");
        //ShaderEffect pkEffect = new OrderIndpTransparencyEffect("Horizontal", 1f);
        Texture pkTexture = pkEffect.GetTexture(0,0);
        pkTexture.SetFilterType(Texture.FilterType.LINEAR_LINEAR);
        pkTexture.SetWrapType(0,Texture.WrapType.REPEAT);
        pkTexture.SetWrapType(1,Texture.WrapType.REPEAT);
        MaterialState kMaterial = new MaterialState();
        kMaterial.Diffuse.Set( 0.2f, 0.2f, 0.2f );
        m_kGround.AttachGlobalState(kMaterial);
        m_kGround.AttachEffect(pkEffect);
        m_spkScene.AttachChild(m_kGround);

        // Create a billboard node that causes a rectangle to always be facing
        // the camera.  This is the type of billboard for an avatar.
        m_spkBillboard0 = new BillboardNode(m_spkCamera);
        m_spkScene.AttachChild(m_spkBillboard0);

        // Create a rectangle mesh.  The mesh is in the xy-plane.  Do not apply
        // local transformations to the mesh.  Use the billboard node transforms
        // to control the mesh location and orientation.
        TriMesh pkMesh = kSM.Rectangle(2,2,0.125f,0.25f);

        //pkEffect = new TextureEffect("RedSky");
        //pkEffect = new MaterialTextureEffect("Stone");
        pkEffect = new OrderIndpTransparencyEffect("Leaf", 0.2f);
        pkMesh.AttachGlobalState(kMaterial);
        pkMesh.AttachEffect(pkEffect);
        m_spkBillboard0.AttachChild(pkMesh);

        // The billboard rotation is about its model-space up-vector (0,1,0).  In
        // this application, world-space up is (0,0,1).  Locally rotate the
        // billboard so it's up-vector matches the world's.
        m_spkBillboard0.Local.SetTranslate( -0.25f,0.0f,0.1f);
        Matrix3f kMatrix = new Matrix3f();
        kMatrix.FromAxisAngle(Vector3f.UNIT_X,Mathf.HALF_PI);
        m_spkBillboard0.Local.SetRotate( kMatrix );

        // Create a billboard node that causes an object to always be oriented
        // the same way relative to the camera.
        m_spkBillboard1 = new BillboardNode(m_spkCamera);
        m_spkScene.AttachChild(m_spkBillboard1);

        // Create a torus mesh.  Do not apply local transformations to the mesh.
        // Use the billboard node transforms to control the mesh location and
        // orientation.
        pkMesh = kSM.Torus(16,16,1.0f,0.25f);
        pkMesh.Local.SetUniformScale(0.1f);

        pkEffect = new OrderIndpTransparencyEffect("RedSky", 0.9f);
        pkMesh.AttachGlobalState(kMaterial);
        pkMesh.AttachEffect(pkEffect);
        m_spkBillboard1.AttachChild(pkMesh);

        // The billboard rotation is about its model-space up-vector (0,1,0).  In
        // this application, world-space up is (0,0,1).  Locally rotate the
        // billboard so it's up-vector matches the world's.
        m_spkBillboard1.Local.SetTranslate( 0.25f,0.0f,0.1f);
        Matrix3f kMatrix2 = new Matrix3f();
        kMatrix2.FromAxisAngle(Vector3f.UNIT_X,Mathf.HALF_PI);
        m_spkBillboard1.Local.SetRotate(kMatrix2);
    }

    private Node m_spkScene;
    private ZBufferState m_kZBuffer;
    private AlphaState m_kAlpha1;
    private AlphaState m_kAlpha;
    private WireframeState m_spkWireframe;
    private Culler m_kCuller = new Culler(0,0,null);

    private BillboardNode m_spkBillboard0, m_spkBillboard1;
    private FrameBuffer m_kFBO;
    //private FrameBuffer m_kSolidFBO;
    
    protected ShaderEffect m_spkPlaneEffect;
    protected TriMesh m_pkPlane;
    protected Camera m_pkScreenCamera;
    
    protected TriMesh m_kGround;

    public void keyPressed(KeyEvent e) {
        char ucKey = e.getKeyChar();
        
        super.keyPressed(e);

        AlphaState pkAState;

        if (ucKey == 'w' || ucKey == 'W')
        {
            m_spkWireframe.Enabled = !m_spkWireframe.Enabled;
            return;
        }
        if (ucKey == 'n' || ucKey == 'N')
        {
            m_spkBillboard0.GetObjectByName("test");
            Vector<GraphicsObject> kVector = new Vector<GraphicsObject>();
            m_spkBillboard0.GetAllObjectsByName( "test", kVector );
            m_spkBillboard0.GetObjectByID( 1 );
            return;
        }
        else if ( ucKey == 'a' )
        {
            ((OrderIndpTransparencyEffect)m_spkBillboard0.GetChild(0).GetEffect(0)).SetAlpha(0.8f);
        }
        else if ( ucKey == 'A' )
        {
            ((OrderIndpTransparencyEffect)m_spkBillboard0.GetChild(0).GetEffect(0)).SetAlpha(0.2f);
        }
        else if (ucKey == 's' || ucKey == 'S')
        {
            TestStreaming(m_spkScene,"BillboardNodes.wmof");
            return;
        }
    }
    
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
    

    

    private void CreateRenderTarget( GLAutoDrawable arg0, int iWidth, int iHeight )
    {        
        Texture[] akSceneTarget = new Texture[3];
        GraphicsImage pkSceneImage = new GraphicsImage(GraphicsImage.FormatMode.IT_RGBA32,iWidth,iHeight,(byte[])null,
                "ColorTex0");
        akSceneTarget[0] = new Texture();
        akSceneTarget[0].SetImage(pkSceneImage);
        akSceneTarget[0].SetShared(true);
        akSceneTarget[0].SetFilterType(Texture.FilterType.NEAREST);
        akSceneTarget[0].SetWrapType(0,Texture.WrapType.CLAMP);
        akSceneTarget[0].SetWrapType(1,Texture.WrapType.CLAMP);
        //akSceneTarget[0].SetSamplerInformation( new SamplerInformation( "ColorTex0", 0, 0 ) );
        m_pkRenderer.LoadTexture( akSceneTarget[0] );
        
        
        pkSceneImage = new GraphicsImage(GraphicsImage.FormatMode.IT_RGBA32,iWidth,iHeight,(float[])null,
        "ColorTex1");
        akSceneTarget[1] = new Texture();
        akSceneTarget[1].SetImage(pkSceneImage);
        akSceneTarget[1].SetShared(true);
        akSceneTarget[1].SetFilterType(Texture.FilterType.NEAREST);
        akSceneTarget[1].SetWrapType(0,Texture.WrapType.CLAMP);
        akSceneTarget[1].SetWrapType(1,Texture.WrapType.CLAMP);
        //akSceneTarget[1].SetSamplerInformation( new SamplerInformation( "ColorTex1", 0, 0 ) );
        m_pkRenderer.LoadTexture( akSceneTarget[1] );
        
        pkSceneImage = new GraphicsImage(GraphicsImage.FormatMode.IT_RGBA32,iWidth,iHeight,(float[])null,
                "ColorTex2");
        akSceneTarget[2] = new Texture();
        akSceneTarget[2].SetImage(pkSceneImage);
        akSceneTarget[2].SetShared(true);
        akSceneTarget[2].SetFilterType(Texture.FilterType.NEAREST);
        akSceneTarget[2].SetWrapType(0,Texture.WrapType.CLAMP);
        akSceneTarget[2].SetWrapType(1,Texture.WrapType.CLAMP);
        //akSceneTarget[2].SetSamplerInformation( new SamplerInformation( "ColorTex2", 0, 0 ) );
        m_pkRenderer.LoadTexture( akSceneTarget[2] );
      
        Attributes kAttributes = new Attributes();
        kAttributes.SetPChannels(3);
        kAttributes.SetTChannels(0,3);
        StandardMesh kSM = new StandardMesh(kAttributes);
        m_pkPlane = kSM.Rectangle(2,2,1.0f,1.0f);           
            
        m_spkPlaneEffect = new OrderIndpTransparencyEffect(akSceneTarget, m_kBackgroundColor );
        m_pkPlane.AttachEffect( m_spkPlaneEffect);
        m_pkPlane.UpdateGS();
        m_pkPlane.UpdateRS();
        
        m_kFBO = new OpenGLFrameBuffer(m_eFormat,m_eDepth,m_eStencil,
                m_eBuffering,m_eMultisampling,m_pkRenderer,akSceneTarget, arg0);
    }
    
}
