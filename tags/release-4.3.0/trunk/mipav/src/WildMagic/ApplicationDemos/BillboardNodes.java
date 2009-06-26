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
        super("BillboardNodes",0,0,640,480, new ColorRGBA(0.9f,0.9f,0.9f,1.0f));
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
        //((OpenGLRenderer)m_pkRenderer).SetDrawable( arg0 );

        MeasureTime();
        
        if (MoveCamera())
        {
            m_spkBillboard0.UpdateGS();
            m_spkBillboard1.UpdateGS();
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

        //((OpenGLRenderer)m_pkRenderer).ClearDrawable( );
    }

    public void displayChanged(GLAutoDrawable arg0, boolean arg1, boolean arg2) {
        // TODO Auto-generated method stub
        
    }

    public void init(GLAutoDrawable arg0) {
        arg0.setAutoSwapBufferMode( false );

        ((OpenGLRenderer)m_pkRenderer).SetDrawable( arg0 );
        ((OpenGLRenderer)m_pkRenderer).InitializeState();

        super.OnInitialize();

        // set up camera
        m_spkCamera.SetFrustum(-0.055f,0.055f,-0.04125f,0.04125f,0.1f,100.0f);
        Vector3f kCLoc = new Vector3f(0.0f,-1.0f,0.25f);
        Vector3f kCDir = new Vector3f(0.0f,1.0f,0.0f);
        Vector3f kCUp = new Vector3f(0.0f,0.0f,1.0f);
        Vector3f kCRight = new Vector3f();
        kCRight.Cross( kCDir, kCUp );
        m_spkCamera.SetFrame(kCLoc,kCDir,kCUp,kCRight);

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
        }
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


    private void CreateScene ()
    {
        m_spkScene = new Node();
        m_spkWireframe = new WireframeState();
        m_spkScene.AttachGlobalState(m_spkWireframe);

        // Create the ground.  It covers a square with vertices (1,1,0), (1,-1,0),
        // (-1,1,0), and (-1,-1,0).  Multiply the texture coordinates by a factor
        // to enhance the wrap-around.
        Attributes kAttr = new Attributes();
        kAttr.SetPChannels(3);
        kAttr.SetTChannels(0,2);
        StandardMesh kSM = new StandardMesh(kAttr);
        TriMesh pkGround = kSM.Rectangle(2,2,16.0f,16.0f);
        int i;
        for (i = 0; i < pkGround.VBuffer.GetVertexQuantity(); i++)
        {
            pkGround.VBuffer.SetTCoord2(0,i, 
                    pkGround.VBuffer.GetTCoord2fX(0,i) * 128.0f,
                    pkGround.VBuffer.GetTCoord2fY(0,i) * 128.0f );

        }

        ShaderEffect pkEffect = new TextureEffect("Horizontal");
        Texture pkTexture = pkEffect.GetPTexture(0,0);
        pkTexture.SetFilterType(Texture.FilterType.LINEAR_LINEAR);
        pkTexture.SetWrapType(0,Texture.WrapType.REPEAT);
        pkTexture.SetWrapType(1,Texture.WrapType.REPEAT);
        pkGround.AttachEffect(pkEffect);
        m_spkScene.AttachChild(pkGround);

        // Create a billboard node that causes a rectangle to always be facing
        // the camera.  This is the type of billboard for an avatar.
        m_spkBillboard0 = new BillboardNode(m_spkCamera);
        m_spkScene.AttachChild(m_spkBillboard0);

        // Create a rectangle mesh.  The mesh is in the xy-plane.  Do not apply
        // local transformations to the mesh.  Use the billboard node transforms
        // to control the mesh location and orientation.
        TriMesh pkMesh = kSM.Rectangle(2,2,0.125f,0.25f);

        pkEffect = new TextureEffect("RedSky");
        pkMesh.AttachEffect(pkEffect);
        m_spkBillboard0.AttachChild(pkMesh);

        // The billboard rotation is about its model-space up-vector (0,1,0).  In
        // this application, world-space up is (0,0,1).  Locally rotate the
        // billboard so it's up-vector matches the world's.
        m_spkBillboard0.Local.SetTranslate( -0.25f,0.0f,0.25f);
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

        pkMesh.AttachEffect(pkEffect);
        m_spkBillboard1.AttachChild(pkMesh);

        // The billboard rotation is about its model-space up-vector (0,1,0).  In
        // this application, world-space up is (0,0,1).  Locally rotate the
        // billboard so it's up-vector matches the world's.
        m_spkBillboard1.Local.SetTranslate( 0.25f,0.0f,0.25f);
        Matrix3f kMatrix2 = new Matrix3f();
        kMatrix2.FromAxisAngle(Vector3f.UNIT_X,Mathf.HALF_PI);
        m_spkBillboard1.Local.SetRotate(kMatrix2);
    }

    private Node m_spkScene;
    private WireframeState m_spkWireframe;
    private Culler m_kCuller = new Culler(0,0,null);

    private BillboardNode m_spkBillboard0, m_spkBillboard1;

    public void keyPressed(KeyEvent e) {
        char ucKey = e.getKeyChar();
        
        System.err.println( ucKey );
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
}
