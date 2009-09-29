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

public class BumpMaps extends JavaApplication3D
    implements GLEventListener, KeyListener
{
    public BumpMaps()
    {
        super("BumpMaps",0,0,640,480, new ColorRGBA(1.0f,1.0f,1.0f,1.0f));
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
        BumpMaps kWorld = new BumpMaps();        
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
        
        if (MoveObject())
        {
            UpdateBumpMap();
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
        m_pkRenderer.InitializeState();
        super.OnInitialize();

        // set up camera
        m_spkCamera.SetFrustum(-0.055f,0.055f,-0.04125f,0.04125f,0.1f,100.0f);
        Vector3f kCDir, kCLoc;
        if (m_bUseTorus)
        {
            kCDir = new Vector3f(0.0f,0.0f,1.0f);
            kCLoc = new Vector3f(0.0f,-0.25f,-3.0f);
        }
        else
        {
            kCDir = new Vector3f(0.0f,0.0f,-1.0f);
            kCLoc = new Vector3f(0.0f,0.0f,3.0f);
        }
        Vector3f kCUp = new Vector3f(0.0f,1.0f,0.0f);
        Vector3f kCRight = new Vector3f();
        kCRight.Cross( kCDir, kCUp );
        m_spkCamera.SetFrame(kCLoc,kCDir,kCUp,kCRight);

        CreateScene();

        // initial update of objects
        m_spkScene.UpdateGS();
        m_spkScene.UpdateRS();

        UpdateBumpMap();

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
        TriMesh pkMesh;
        if (m_bUseTorus)
        {
            pkMesh = CreateTorus();
            pkMesh.Local.SetRotate( new Matrix3f(Vector3f.UNIT_X,(float)(0.25f*Math.PI)));
        }
        else
        {
            pkMesh = CreateSquare();
        }
        m_spkScene.AttachChild(pkMesh);
    }

    private TriMesh CreateTorus ()
    {
        Attributes kAttr = new Attributes();
        kAttr.SetPChannels(3);
        if (m_bUseBumpMap)
        {
            kAttr.SetNChannels(3);
            kAttr.SetCChannels(0,3);
            kAttr.SetTChannels(0,2);
            kAttr.SetTChannels(1,2);
        }
        else
        {
            kAttr.SetTChannels(0,2);
        }

        StandardMesh kSM = new StandardMesh(kAttr);
        TriMesh pkMesh = kSM.Torus(32,32,1.0f,0.4f);

        VertexBuffer pkVB = pkMesh.VBuffer;
        int iVQuantity = pkVB.GetVertexQuantity();
        for (int i = 0; i < iVQuantity; i++)
        {
            pkVB.SetTCoord2(0,i,
                         pkVB.GetTCoord2fX(0,i) * 4.0f,
                         pkVB.GetTCoord2fY(0,i) * 4.0f);
            if (m_bUseBumpMap)
            {
                pkVB.SetTCoord2(1,i,
                             pkVB.GetTCoord2fX(1,i) * 4.0f,
                             pkVB.GetTCoord2fY(1,i) * 4.0f);
            }
        }

        if (m_bUseBumpMap)
        {
            SimpleBumpMapEffect pkEffect = new SimpleBumpMapEffect("Bricks",
                                                                   "BricksNormal",Vector3f.UNIT_Z);
            pkEffect.ComputeLightVectors(pkMesh);

            Texture pkBricks = pkEffect.GetTexture(0,0);
            pkBricks.SetFilterType(Texture.FilterType.LINEAR_LINEAR);
            pkBricks.SetWrapType(0,Texture.WrapType.REPEAT);
            pkBricks.SetWrapType(1,Texture.WrapType.REPEAT);

            Texture pkNormals = pkEffect.GetTexture(0,1);
            pkNormals.SetFilterType(Texture.FilterType.LINEAR_LINEAR);
            pkNormals.SetWrapType(0,Texture.WrapType.REPEAT);
            pkNormals.SetWrapType(1,Texture.WrapType.REPEAT);

            pkMesh.AttachEffect(pkEffect);
        }
        else
        {
            TextureEffect pkEffect = new TextureEffect("Bricks");

            Texture pkBricks = pkEffect.GetTexture(0,0);
            pkBricks.SetFilterType(Texture.FilterType.LINEAR_LINEAR);
            pkBricks.SetWrapType(0,Texture.WrapType.REPEAT);
            pkBricks.SetWrapType(1,Texture.WrapType.REPEAT);

            pkMesh.AttachEffect(pkEffect);
        }

        return pkMesh;
    }

    private TriMesh CreateSquare ()
    {
        Attributes kAttr = new Attributes();
        kAttr.SetPChannels(3);
        if (m_bUseBumpMap)
        {
            kAttr.SetNChannels(3);
            kAttr.SetCChannels(0,3);
            kAttr.SetTChannels(0,2);
            kAttr.SetTChannels(1,2);
        }
        else
        {
            kAttr.SetTChannels(0,2);
        }

        VertexBuffer pkVBuffer = new VertexBuffer(kAttr,4);

        pkVBuffer.SetPosition3(0,-1.0f,-1.0f,0.0f);
        pkVBuffer.SetPosition3(1,+1.0f,-1.0f,0.0f);
        pkVBuffer.SetPosition3(2,+1.0f,+1.0f,0.0f);
        pkVBuffer.SetPosition3(3,-1.0f,+1.0f,0.0f);

        if (m_bUseBumpMap)
        {
            pkVBuffer.SetNormal3(0, Vector3f.UNIT_Z);
            pkVBuffer.SetNormal3(1, Vector3f.UNIT_Z);
            pkVBuffer.SetNormal3(2, Vector3f.UNIT_Z);
            pkVBuffer.SetNormal3(3, Vector3f.UNIT_Z);

            pkVBuffer.SetTCoord2(0,0,0.0f,0.0f);
            pkVBuffer.SetTCoord2(0,1,1.0f,0.0f);
            pkVBuffer.SetTCoord2(0,2,1.0f,1.0f);
            pkVBuffer.SetTCoord2(0,3,0.0f,1.0f);

            pkVBuffer.SetTCoord2(1,0,0.0f,0.0f);
            pkVBuffer.SetTCoord2(1,1,1.0f,0.0f);
            pkVBuffer.SetTCoord2(1,2,1.0f,1.0f);
            pkVBuffer.SetTCoord2(1,3,0.0f,1.0f);
        }
        else
        {
            pkVBuffer.SetTCoord2(0,0,0.0f,0.0f);
            pkVBuffer.SetTCoord2(0,1,1.0f,0.0f);
            pkVBuffer.SetTCoord2(0,2,1.0f,1.0f);
            pkVBuffer.SetTCoord2(0,3,0.0f,1.0f);
        }

        IndexBuffer pkIBuffer = new IndexBuffer(6);
        int[] aiIndex = pkIBuffer.GetData();
        aiIndex[0] = 0;  aiIndex[1] = 1;  aiIndex[2] = 2;
        aiIndex[3] = 0;  aiIndex[4] = 2;  aiIndex[5] = 3;

        TriMesh pkMesh = new TriMesh(pkVBuffer,pkIBuffer);

        if (m_bUseBumpMap)
        {
            Vector3f kLightDirection = new Vector3f(-1.0f,-1.0f,-1.0f);
            kLightDirection.Normalize();
            SimpleBumpMapEffect pkEffect = new SimpleBumpMapEffect("Bricks",
                                                                   "BricksNormal",kLightDirection);
            pkEffect.ComputeLightVectors(pkMesh);
            pkMesh.AttachEffect(pkEffect);
        }
        else
        {
            TextureEffect pkEffect = new TextureEffect("Bricks");
            pkMesh.AttachEffect(pkEffect);
        }

        return pkMesh;
    }

    private void UpdateBumpMap ()
    {
        if (m_bUseBumpMap)
        {
            // The scene graph transformations have been updated, which means the
            // tangent-space light vectors need updating.
            TriMesh pkMesh = (TriMesh)(m_spkScene.GetChild(0));
            SimpleBumpMapEffect pkEffect =
                (SimpleBumpMapEffect)pkMesh.GetEffect(0);
            pkEffect.ComputeLightVectors(pkMesh);
            pkMesh.VBuffer.Release();
        }
    }

    private Node m_spkScene;
    private Culler m_kCuller = new Culler(0,0,null);
    private boolean m_bUseTorus = true;
    private boolean m_bUseBumpMap = true;

    public void keyPressed(KeyEvent e) {
        char ucKey = e.getKeyChar();
        
        System.err.println( ucKey );
        super.keyPressed(e);

        AlphaState pkAState;
        switch (ucKey)
        {
        case 'b':
        case 'B':
            m_bUseBumpMap = !m_bUseBumpMap;
            TriMesh pkMesh = (TriMesh)(m_spkScene.GetChild(0));
            if (m_bUseTorus)
            {
                Transformation kLocal = pkMesh.Local;
                pkMesh = CreateTorus();
                pkMesh.Local = kLocal;
            }
            else
            {
                pkMesh = CreateSquare();
            }
            m_spkScene.SetChild(0,pkMesh);
            m_spkScene.UpdateGS();
            m_spkScene.UpdateRS();
            UpdateBumpMap();
            m_kCuller.ComputeVisibleSet(m_spkScene);
            return;
        case 's':
        case 'S':
            TestStreaming(m_spkScene,"BumpMaps.wmof");
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
