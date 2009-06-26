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

import WildMagic.LibApplications.OpenGLApplication.*;
import WildMagic.LibFoundation.Mathematics.*;
import WildMagic.LibGraphics.Effects.*;
import WildMagic.LibGraphics.Rendering.*;
import WildMagic.LibGraphics.SceneGraph.*;
import WildMagic.LibGraphics.Shaders.*;
import WildMagic.LibRenderers.OpenGLRenderer.*;

public class CameraAndLightNodes extends JavaApplication3D
    implements GLEventListener, KeyListener
{
    public CameraAndLightNodes()
    {
        super("CameraAndLightNodes",0,0,640,480, new ColorRGBA(0.5f,0.5f,1.0f,1.0f));
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
        CameraAndLightNodes kWorld = new CameraAndLightNodes();        
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
            m_kCuller.ComputeVisibleSet(m_spkScene);
        }        

        m_pkRenderer.ClearBuffers();
        if (m_pkRenderer.BeginScene())
        {
            m_pkRenderer.SetCamera(m_spkScreenCamera);
            m_pkRenderer.Draw(m_spkSky);

            m_pkRenderer.SetCamera(m_spkCamera);
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
        // scene -+--> groundPoly
        //        |
        //        +--> cameraNode --+--> lightFixture0 +--> lightNode0
        //                          |                  |
        //                          |                  +--> lightTarget0
        //                          |
        //                          +--> lightFixture1 +--> lightNode1
        //                                             |
        //                                             +--> lightTarget0

        m_spkCamera.SetFrustum(-0.55f,0.55f,-0.4125f,0.4125f,1.0f,1000.0f);
        Vector3f kCLoc = new Vector3f(0.0f,-100.0f,5.0f);
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

        InitializeCameraMotion(0.01f,0.001f);
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

    public GLCanvas GetCanvas()
    {
        return ((OpenGLRenderer)m_pkRenderer).GetCanvas();
    }

    private void CreateScene ()
    {
        CreateScreenPolygon();

        m_spkScene = new Node();
        m_spkWireframeState = new WireframeState();
        m_spkScene.AttachGlobalState(m_spkWireframeState);

        TriMesh pkGround = CreateGround();
        m_spkScene.AttachChild(pkGround);
        m_spkCNode = new CameraNode(m_spkCamera);
        m_spkScene.AttachChild(m_spkCNode);

        Node pkLFixture0 = CreateLightFixture(0);
        pkLFixture0.Local.SetTranslate(new Vector3f(25.0f,-5.75f,6.0f));
        pkLFixture0.Local.SetRotate( new Matrix3f(Vector3f.UNIT_X,-Mathf.HALF_PI));
        m_spkCNode.AttachChild(pkLFixture0);

        Node pkLFixture1 = CreateLightFixture(1);
        pkLFixture1.Local.SetTranslate(new Vector3f(25.0f,-5.75f,-6.0f));
        pkLFixture1.Local.SetRotate( new Matrix3f(Vector3f.UNIT_X,-Mathf.HALF_PI));
        m_spkCNode.AttachChild(pkLFixture1);
    }

    private TriMesh CreateGround ()
    {
        Attributes kAttr = new Attributes();
        kAttr.SetPChannels(3);
        kAttr.SetNChannels(3);
        kAttr.SetTChannels(0,2);
        VertexBuffer pkVBuffer = new VertexBuffer(kAttr,4);

        float fMax = 100.0f;
        pkVBuffer.SetPosition3(0,-fMax,-fMax,0.0f);
        pkVBuffer.SetPosition3(1,+fMax,-fMax,0.0f);
        pkVBuffer.SetPosition3(2,+fMax,+fMax,0.0f);
        pkVBuffer.SetPosition3(3,-fMax,+fMax,0.0f);
        pkVBuffer.SetNormal3(0, Vector3f.UNIT_Z);
        pkVBuffer.SetNormal3(1, Vector3f.UNIT_Z);
        pkVBuffer.SetNormal3(2, Vector3f.UNIT_Z);
        pkVBuffer.SetNormal3(3, Vector3f.UNIT_Z);
        pkVBuffer.SetTCoord2(0,0,0.0f,0.0f);
        pkVBuffer.SetTCoord2(0,1,8.0f,0.0f);
        pkVBuffer.SetTCoord2(0,2,8.0f,8.0f);
        pkVBuffer.SetTCoord2(0,3,0.0f,8.0f);

        IndexBuffer pkIBuffer = new IndexBuffer(6);
        int[] aiIndex = pkIBuffer.GetData();
        aiIndex[0] = 0;  aiIndex[1] = 1;  aiIndex[2] = 2;
        aiIndex[3] = 0;  aiIndex[4] = 2;  aiIndex[5] = 3;

        TriMesh pkMesh = new TriMesh(pkVBuffer,pkIBuffer);

        Light pkLight = new Light(Light.LightType.LT_AMBIENT);
        pkLight.Ambient = ColorRGB.WHITE;
        pkMesh.AttachLight(pkLight);

        TextureEffect pkEffect = new TextureEffect("Gravel");
        Texture pkTexture = pkEffect.GetPTexture(0,0);
        pkTexture.SetFilterType(Texture.FilterType.LINEAR_LINEAR);
        pkTexture.SetWrapType(0,Texture.WrapType.REPEAT);
        pkTexture.SetWrapType(1,Texture.WrapType.REPEAT);
        pkMesh.AttachEffect(pkEffect);

        AlphaState pkAState = pkEffect.GetBlending(0);
        pkAState.SrcBlend = AlphaState.SrcBlendMode.SBF_DST_COLOR;
        pkAState.DstBlend = AlphaState.DstBlendMode.DBF_ZERO;

        return pkMesh;
    }

    private TriMesh CreateLightTarget (Light pkLight)
    {
        // Create a parabolic rectangle patch that is illuminated by the light.
        // To hide the artifacts of vertex normal lighting on a grid, the patch
        // is slightly bent so that the intersection with a plane is nearly
        // circular.  The patch is translated slightly below the plane of the
        // ground to hide the corners and the jaggies.
        Attributes kAttr = new Attributes();
        kAttr.SetPChannels(3);
        kAttr.SetNChannels(3);
        TriMesh pkMesh = (new StandardMesh(kAttr)).Rectangle(64,64,8.0f,8.0f);
        VertexBuffer pkVBuffer = pkMesh.VBuffer;
        int iVQuantity = pkVBuffer.GetVertexQuantity();
        for (int i = 0; i < iVQuantity; i++)
        {
            float fX = pkVBuffer.GetPosition3fX(i);
            float fY = pkVBuffer.GetPosition3fY(i);
            float fZ = 1.0f - (fX*fX + fY*fY)/128.0f;
            pkVBuffer.SetPosition3(i, fX, fY, fZ);
        }
        pkMesh.UpdateMS();

        AlphaState pkAState = new AlphaState();
        pkAState.BlendEnabled = true;
        pkMesh.AttachGlobalState(pkAState);

        MaterialState pkMState = new MaterialState();
        pkMState.Emissive = new ColorRGB(0.0f,0.0f,0.0f);
        pkMState.Ambient = new ColorRGB(0.5f,0.5f,0.5f);
        pkMState.Diffuse = new ColorRGB(1.0f,0.85f,0.75f);
        pkMState.Specular = new ColorRGB(0.8f,0.8f,0.8f);
        pkMState.Shininess = 1.0f;
        pkMState.Alpha = 0.5f;
        pkMesh.AttachGlobalState(pkMState);
        pkMesh.AttachLight(pkLight);

        return pkMesh;
    }

    private Node CreateLightFixture (int iWhich)
    {
        Node pkLFixture = new Node();

        // point light illuminates the target
        Light pkPLight = new Light(Light.LightType.LT_POINT);
        pkPLight.Ambient = new ColorRGB(1.0f,1.0f,0.5f);
        pkPLight.Diffuse = new ColorRGB(1.0f,1.0f,0.5f);
        pkPLight.Specular = new ColorRGB(1.0f,1.0f,0.5f);
        if ( iWhich == 0 )
            m_spkAdjustableLight0 = pkPLight;
        else
            m_spkAdjustableLight1 = pkPLight;

        // the target itself
        TriMesh pkLTarget = CreateLightTarget(pkPLight);

        // Encapsulate the light in a light node.  Rotate the light node so the
        // light points down.
        LightNode pkLNode = new LightNode(pkPLight);

        pkLFixture.AttachChild(pkLNode);
        pkLFixture.AttachChild(pkLTarget);

        return pkLFixture;
    }

    private void CreateScreenPolygon ()
    {
        // The screen camera is designed to map (x,y,z) in [0,1]^3 to (x',y,'z')
        // in [-1,1]^2 x [0,1].
        m_spkScreenCamera = new Camera();
        m_spkScreenCamera.Perspective = false;
        m_spkScreenCamera.SetFrustum(0.0f,1.0f,0.0f,1.0f,0.0f,1.0f);
        m_spkScreenCamera.SetFrame(Vector3f.ZERO,Vector3f.UNIT_Z,
                                    Vector3f.UNIT_Y,Vector3f.UNIT_X);

        // Create a background screen polygon.
        Attributes kAttr = new Attributes();
        kAttr.SetPChannels(3);
        kAttr.SetTChannels(0,2);
        VertexBuffer pkVBuffer = new VertexBuffer(kAttr,4);
        pkVBuffer.SetPosition3(0,0.0f,0.0f,1.0f);
        pkVBuffer.SetPosition3(1,1.0f,0.0f,1.0f);
        pkVBuffer.SetPosition3(2,1.0f,1.0f,1.0f);
        pkVBuffer.SetPosition3(3,0.0f,1.0f,1.0f);
        pkVBuffer.SetTCoord2(0,0,0.0f,1.0f);
        pkVBuffer.SetTCoord2(0,1,1.0f,1.0f);
        pkVBuffer.SetTCoord2(0,2,1.0f,0.0f);
        pkVBuffer.SetTCoord2(0,3,0.0f,0.0f);
        IndexBuffer pkIBuffer = new IndexBuffer(6);
        int[] aiIndex = pkIBuffer.GetData();
        aiIndex[0] = 0;  aiIndex[1] = 1;  aiIndex[2] = 2;
        aiIndex[3] = 0;  aiIndex[4] = 2;  aiIndex[5] = 3;
        m_spkSky = new TriMesh(pkVBuffer,pkIBuffer);
        m_spkSky.AttachEffect(new TextureEffect("RedSky"));
    }

    protected void MoveForward ()
    {
        Vector3f kLocation = m_spkCNode.Local.GetTranslate();
        Vector3f kDirection = new Vector3f();
        m_spkCNode.Local.GetRotate().GetColumn(0, kDirection);
        kDirection.Scale(m_fTrnSpeed);
        kLocation.Add( kDirection );
        m_spkCNode.Local.SetTranslate(kLocation);
        m_spkCNode.UpdateGS();
        m_kCuller.ComputeVisibleSet(m_spkScene);
        kDirection = null;
    }

    protected void MoveBackward ()
    {
        Vector3f kLocation = m_spkCNode.Local.GetTranslate();
        Vector3f kDirection = new Vector3f();
        m_spkCNode.Local.GetRotate().GetColumn(0, kDirection);
        kDirection.Scale(m_fTrnSpeed);
        kLocation.Sub( kDirection );
        m_spkCNode.Local.SetTranslate(kLocation);
        m_spkCNode.UpdateGS();
        m_kCuller.ComputeVisibleSet(m_spkScene);
        kDirection = null;
    }

    protected void TurnLeft ()
    {
        Vector3f kUp = new Vector3f();
        m_spkCNode.Local.GetRotate().GetColumn(1, kUp);
        Matrix3f kNewRotate = new Matrix3f(kUp,m_fRotSpeed);
        kNewRotate.Mult( m_spkCNode.Local.GetRotate() );
        m_spkCNode.Local.SetRotate(kNewRotate);
        m_spkCNode.UpdateGS();
        m_kCuller.ComputeVisibleSet(m_spkScene);
        kUp = null;
    }

    protected void TurnRight ()
    {
        Vector3f kUp = new Vector3f();
        m_spkCNode.Local.GetRotate().GetColumn(1, kUp);
        Matrix3f kNewRotate = new Matrix3f(kUp,-m_fRotSpeed);
        kNewRotate.Mult( m_spkCNode.Local.GetRotate() );
        m_spkCNode.Local.SetRotate(kNewRotate);
        m_spkCNode.UpdateGS();
        m_kCuller.ComputeVisibleSet(m_spkScene);
        kUp = null;
    }

    protected void MoveUp ()
    {
        // disabled
    }

    protected void MoveDown ()
    {
        // disabled
    }

    protected void LookUp ()
    {
        // disabled
    }

    protected void LookDown ()
    {
        // disabled
    }

    public void keyPressed(KeyEvent e) {
        char ucKey = e.getKeyChar();
        
        System.err.println( ucKey );
        super.keyPressed(e);

        switch (ucKey)
        {
        case 'w':
        case 'W':
            m_spkWireframeState.Enabled = !m_spkWireframeState.Enabled;
            return;

        case '+':  // increase light intensity
        case '=':
            m_spkAdjustableLight0.Intensity += 0.1f;
            m_spkAdjustableLight1.Intensity += 0.1f;
            return;

        case '-':  // decrease light intensity
        case '_':
            if (m_spkAdjustableLight0.Intensity >= 0.1f)
            {
                m_spkAdjustableLight0.Intensity -= 0.1f;
            }
            if (m_spkAdjustableLight1.Intensity >= 0.1f)
            {
                m_spkAdjustableLight1.Intensity -= 0.1f;
            }
            return;
        case 's':
        case 'S':
            TestStreaming(m_spkScene,"CameraAndLightNodes.wmof");
            return;
        }
    }

    private Node m_spkScene;
    private WireframeState m_spkWireframeState;
    private CameraNode m_spkCNode;
    private Camera m_spkScreenCamera;
    private TriMesh m_spkSky;
    private Light m_spkAdjustableLight0, m_spkAdjustableLight1;
    private Culler m_kCuller = new Culler(0,0,null);
    
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
