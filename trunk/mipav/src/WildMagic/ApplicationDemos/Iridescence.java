// Wild Magic Source Code
// David Eberly
// http://www.geometrictools.com
// Copyright (c) 1998-2007
//
// This library is free software; you can redistribute it and/or modify it
// under the terms of the GNU Lesser General Public License as published by
// the Free Software Foundation; either version 2.1 of the License, or (at
// your option) any later version. The license is available for reading at
// either of the locations:
// http://www.gnu.org/copyleft/lgpl.html
// http://www.geometrictools.com/License/WildMagicLicense.pdf
//
// Version: 4.0.0 (2006/06/28)
//
// Ported to Java by Alexandra Bokinsky, PhD, Geometric Tools, Inc. (July 2007)
//

package WildMagic.ApplicationDemos;


import java.awt.Frame;
import java.awt.event.*;

import javax.media.opengl.*;

import WildMagic.LibApplications.OpenGLApplication.*;
import WildMagic.LibFoundation.Mathematics.*;
import WildMagic.LibGraphics.Effects.IridescenceEffect;
import WildMagic.LibGraphics.SceneGraph.*;
import WildMagic.LibGraphics.Shaders.*;
import WildMagic.LibRenderers.OpenGLRenderer.OpenGLRenderer;

import com.sun.opengl.util.Animator;


public class Iridescence extends JavaApplication3D implements GLEventListener, KeyListener {
    /**
     * The constructor initializes the OpenGLRender, and sets up the GLEvent, KeyEvent, and Mouse listeners. The last
     * three statements initialize the ImageCatalog, VertexProgramCatalog, and PixelProgramCatalog. The three catalogs
     * enable sharing of texture images, vertex program, and pixel programs in a program. The catalogs keep track of
     * images and programs that are currently loaded. If multiple effects use the same images and shader programs, the
     * loaded version is re-used. This saves graphics hardware memory as well as time for re-loading large textures.
     * 
     */
    public Iridescence() {
        super("Iridescence", 0, 0, 640, 480, new ColorRGBA(0.5f, 0.0f, 1.0f, 1.0f));
        m_pkRenderer = new OpenGLRenderer(m_eFormat, m_eDepth, m_eStencil, m_eBuffering, m_eMultisampling, m_iWidth,
                m_iHeight);
        ((OpenGLRenderer) m_pkRenderer).GetCanvas().addGLEventListener(this);
        ((OpenGLRenderer) m_pkRenderer).GetCanvas().addKeyListener(this);
        ((OpenGLRenderer) m_pkRenderer).GetCanvas().addMouseListener(this);
        ((OpenGLRenderer) m_pkRenderer).GetCanvas().addMouseMotionListener(this);

        String kExternalDirs = getExternalDirs();
        ImageCatalog.SetActive(new ImageCatalog("Main", kExternalDirs));
        VertexProgramCatalog.SetActive(new VertexProgramCatalog("Main", kExternalDirs));
        PixelProgramCatalog.SetActive(new PixelProgramCatalog("Main", kExternalDirs));
        CompiledProgramCatalog.SetActive(new CompiledProgramCatalog());
    }

    /**
     * Iridescence.main creates the Iridescence object and window frame to contain the GLCanvas. An Animator object is
     * created with the GLCanvas as an argument. The Animator provides the same function as the glutMainLoop() function
     * call commonly used in OpenGL applications.
     */
    public static void main(String[] args) {
        Iridescence kWorld = new Iridescence();
        Frame frame = new Frame(kWorld.GetWindowTitle());
        frame.add(kWorld.GetCanvas());
        frame.setSize(kWorld.GetWidth(), kWorld.GetHeight());
        /* Animator serves the purpose of the idle function, calls display: */
        final Animator animator = new Animator(kWorld.GetCanvas());
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

    /**
     * Iridescence.display() displays the scene. The frame rate is measured. Any camera motion that has occurred since
     * the last frame was displayed is applied and the culling system updated. Any object motions that has occurred is
     * also applied and the culling system updated. Renderer.ClearBuffers() is called, the scene and framerate are
     * drawn, and the back-buffer displayed with Renderer.DisplayBackBuffer().
     */
    public void display(GLAutoDrawable arg0) {
        // ((OpenGLRenderer)m_pkRenderer).SetDrawable( arg0 );

        MeasureTime();
        if (MoveCamera()) {
            m_kCuller.ComputeVisibleSet(m_spkScene);
        }
        if (MoveObject()) {
            m_spkScene.UpdateGS();
            m_kCuller.ComputeVisibleSet(m_spkScene);
        }
        if ( !m_bStereo) {
            m_pkRenderer.ClearBuffers();
            if (m_pkRenderer.BeginScene()) {
                m_pkRenderer.DrawScene(m_kCuller.GetVisibleSet());
                DrawFrameRate(8, GetHeight() - 8, ColorRGBA.WHITE);
                m_pkRenderer.EndScene();
            }
        } else {
            m_pkRenderer.ClearBuffers();
            if (m_pkRenderer.BeginScene()) {
                MoveRight();
                if (m_bRight) {
                    m_kCuller.ComputeVisibleSet(m_spkScene);
                    m_pkRenderer.SetColorMask(false, false, true, true);
                    m_pkRenderer.DrawScene(m_kCuller.GetVisibleSet());
                    DrawFrameRate(8, GetHeight() - 8, ColorRGBA.WHITE);
                    m_pkRenderer.EndScene();
                }
            }
            m_pkRenderer.ClearZBuffer();
            if (m_pkRenderer.BeginScene()) {
                MoveLeft();
                MoveLeft();
                if (m_bLeft) {
                    m_kCuller.ComputeVisibleSet(m_spkScene);
                    m_pkRenderer.SetColorMask(true, false, false, true);
                    m_pkRenderer.DrawScene(m_kCuller.GetVisibleSet());
                    DrawFrameRate(8, GetHeight() - 8, ColorRGBA.WHITE);
                    m_pkRenderer.EndScene();
                }

                MoveRight();
                m_pkRenderer.SetColorMask(true, true, true, true);
            }
        }
        m_pkRenderer.DisplayBackBuffer();
        UpdateFrameCount();
        // ((OpenGLRenderer)m_pkRenderer).ClearDrawable( );

        if (m_kShaderParamsWindow == null) {
            m_kShaderParamsWindow = new ApplicationGUI();
            m_kShaderParamsWindow.setParent(this);
            m_kShaderParamsWindow.AddUserVariables(m_spkEffect.GetVProgram(0));
            m_kShaderParamsWindow.Display();
            m_kShaderParamsWindow.setParent(this);
        }
    }

    public void displayChanged(GLAutoDrawable arg0, boolean arg1, boolean arg2) {}

    /**
     * Iridescence.init is called only once when the GLCanvas is initialized. It initializes the renderer object, sets
     * up the camera model, creates the scene, and initializes the culling object with the camera and scene objects.
     */
    public void init(GLAutoDrawable arg0) {
        arg0.setAutoSwapBufferMode(false);

        ((OpenGLRenderer) m_pkRenderer).SetDrawable(arg0);
        ((OpenGLRenderer) m_pkRenderer).InitializeState();

        super.OnInitialize();

        m_spkCamera.SetFrustum( -0.55f, 0.55f, -0.4125f, 0.4125f, 1.0f, 100.0f);
        Vector3f kCLoc = new Vector3f(0.0f, 0.0f, -8.0f);
        Vector3f kCDir = new Vector3f(0.0f, 0.0f, 1.0f);
        Vector3f kCUp = new Vector3f(0.0f, 1.0f, 0.0f);
        Vector3f kCRight = new Vector3f();
        kCRight.Cross(kCDir, kCUp);
        m_spkCamera.SetFrame(kCLoc, kCDir, kCUp, kCRight);

        CreateScene();

        // initial update of objects
        m_spkScene.UpdateGS();
        m_spkScene.UpdateRS();

        // initial culling of scene
        m_kCuller.SetCamera(m_spkCamera);
        m_kCuller.ComputeVisibleSet(m_spkScene);

        InitializeCameraMotion(0.01f, 0.001f);
        InitializeObjectMotion(m_spkScene);

        // ((OpenGLRenderer)m_pkRenderer).ClearDrawable( );
    }

    public void reshape(GLAutoDrawable arg0, int iX, int iY, int iWidth, int iHeight) {
        // ((OpenGLRenderer)m_pkRenderer).SetDrawable( arg0 );
        if (iWidth > 0 && iHeight > 0) {
            if (m_pkRenderer != null) {
                m_pkRenderer.Resize(iWidth, iHeight);
            }

            m_iWidth = iWidth;
            m_iHeight = iHeight;
        }
        // ((OpenGLRenderer)m_pkRenderer).ClearDrawable( );
    }

    public GLCanvas GetCanvas() {
        return ((OpenGLRenderer) m_pkRenderer).GetCanvas();
    }

    /**
     * Iridescence.CreateScene() creates the scene graph. The root node is m_spkScene. It contains a single TriMesh
     * object, the torus. The TriMesh object is created with a set of rendering Attributes with three channels for point
     * data (x,y,z); three channels for normal data (x,y,z); and two channels for texture-coordinate data (s,t). An
     * IridescenceEffect is created and attached to the torus.
     */
    private void CreateScene() {
        m_spkScene = new Node();

        // start arrow node --------
        Transformation trans = new Transformation();
        float rotationRedian = 0.5f * (float) Math.PI;

        Matrix3f matrix = new Matrix3f((float)Math.cos(rotationRedian),0.0f,(float)Math.sin(rotationRedian),
                                       0.0f,1.0f,0.0f,
                                       (float)-Math.sin(rotationRedian),0.0f,(float)Math.cos(rotationRedian));

        trans.SetRotate(matrix);
        //trans.SetTranslate(0.0f,0.0f,-0.25f);
        
        Attributes kAttr = new Attributes();
        kAttr.SetPChannels(3);
        kAttr.SetNChannels(3);
        kAttr.SetTChannels(0,2);
        StandardMesh kSM = new StandardMesh(kAttr);
        kSM.SetTransformation(trans);

        Node pkTrnNode = new Node();
        TriMesh pkMesh = kSM.Cone(6,6,0.5f,0.5f, false);
        pkMesh.Local.SetTranslate(-0.25f,0.0f,0.0f);
        pkTrnNode.AttachChild(pkMesh);
        
        //trans.SetTranslate(0.0f,0.0f,0.25f);
        TriMesh pkMesh2 = kSM.Cylinder(6,6,0.25f,0.5f, false);
        pkMesh2.Local.SetTranslate(0.25f,0.0f,0.0f);
        pkTrnNode.AttachChild(pkMesh2);

        // end arrow node --------
        m_spkScene.AttachChild(pkTrnNode);

        /*
        Attributes kAttr = new Attributes();
        kAttr.SetPChannels(3);
        kAttr.SetNChannels(3);
        kAttr.SetTChannels(0, 2);
        StandardMesh kSM = new StandardMesh(kAttr);
        // TriMesh pkMesh = kSM.Ellipsoid(50,50,1.5f,1.0f, 2.0f);
        TriMesh pkMesh = kSM.Torus(200, 200, 2.0f, 1.0f);

        pkMesh.Local.SetMatrix(new Matrix3f(new Vector3f(0f, 0f, 1f), new Vector3f(0.707f, 0.707f, 0f), new Vector3f(
                -0.707f, 0.707f, 0f), false));
        m_spkScene.AttachChild(pkMesh);
        */

        m_spkEffect = new IridescenceEffect("Leaf", "Gradient");
        m_spkEffect.SetInterpolateFactor(0.5f);
        final int iPassQuantity = m_spkEffect.GetPassQuantity();
        for (int iPass = 0; iPass < iPassQuantity; iPass++) {
            m_spkEffect.LoadPrograms(m_pkRenderer, iPass, m_pkRenderer.GetMaxColors(), m_pkRenderer.GetMaxTCoords(),
                                     m_pkRenderer.GetMaxVShaderImages(), m_pkRenderer.GetMaxPShaderImages());
        }

        pkMesh.AttachEffect(m_spkEffect);
        pkMesh2.AttachEffect(m_spkEffect);
//         m_pkRenderer.SetColorMask(m_abColorMask[0], m_abColorMask[1], m_abColorMask[2], m_abColorMask[3]);
    }

    /**
     * Iridescence.keyPressed() processes key-input from the user. The iridescence factor shader parameter can be
     * increased and decreased by pressing the + and - keys. A shader-editor GUI can be launched by pressing 'l'. The
     * scene-graph is streamed to disk by pressing the 's' key.
     */
    public void keyPressed(KeyEvent e) {
        char ucKey = e.getKeyChar();
        super.keyPressed(e);
        float fInterpolateFactor;
        switch (ucKey) {
            case '+':
            case '=':
                fInterpolateFactor = m_spkEffect.GetInterpolateFactor();
                fInterpolateFactor += 0.1f;
                if (fInterpolateFactor > 1.0f) {
                    fInterpolateFactor = 1.0f;
                }
                m_spkEffect.SetInterpolateFactor(fInterpolateFactor);
                return;
            case '-':
            case '_':
                fInterpolateFactor = m_spkEffect.GetInterpolateFactor();
                fInterpolateFactor -= 0.1f;
                if (fInterpolateFactor < 0.0f) {
                    fInterpolateFactor = 0.0f;
                }
                m_spkEffect.SetInterpolateFactor(fInterpolateFactor);
                return;
                /*
                 * case 'l': case 'L': ApplicationGUI kShaderParamsWindow = new ApplicationGUI();
                 * kShaderParamsWindow.setParent(this); kShaderParamsWindow.AddUserVariables(m_spkEffect.GetVProgram(0));
                 * kShaderParamsWindow.AddUserVariables(m_spkEffect.GetPProgram(0)); kShaderParamsWindow.Display(); return;
                 */
            case 's':
            case 'S':
                TestStreaming(m_spkScene, "Iridescence.wmof");
                return;
            case '1':
                m_bStereo = false;
                break;
            case '2':
                m_bStereo = true;
                m_bLeft = true;
                m_bRight = true;
                break;
            case 'l':
                m_bLeft = true;
                m_bRight = false;
                break;
            case 'r':
                m_bLeft = false;
                m_bRight = true;
                break;
        }
        return;
    }

    private Node m_spkScene;

    private IridescenceEffect m_spkEffect;

    private Culler m_kCuller = new Culler(0, 0, null);

    /** Window with the shader parameter interface: */
    private ApplicationGUI m_kShaderParamsWindow = null;

    private boolean[] m_abColorMask = new boolean[] {true, true, true, true};

    private boolean m_bStereo = false;

    private boolean m_bLeft = true;

    private boolean m_bRight = true;

    private String getExternalDirs() {
        String jar_filename = "";
        String class_path_key = "java.class.path";
        String class_path = System.getProperty(class_path_key);
        for (String fn : class_path.split(";")) {
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
