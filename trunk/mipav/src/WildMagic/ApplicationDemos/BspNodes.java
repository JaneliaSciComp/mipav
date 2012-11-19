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



import java.awt.Frame;
import java.awt.event.KeyEvent;
import java.awt.event.KeyListener;
import java.awt.event.WindowAdapter;
import java.awt.event.WindowEvent;

import javax.media.opengl.GLAutoDrawable;
import javax.media.opengl.GLEventListener;

import WildMagic.LibFoundation.Mathematics.ColorRGB;
import WildMagic.LibFoundation.Mathematics.ColorRGBA;
import WildMagic.LibFoundation.Mathematics.Mathf;
import WildMagic.LibFoundation.Mathematics.Matrix3f;
import WildMagic.LibFoundation.Mathematics.Plane3f;
import WildMagic.LibFoundation.Mathematics.Vector2f;
import WildMagic.LibFoundation.Mathematics.Vector3f;
import WildMagic.LibGraphics.Effects.ShaderEffect;
import WildMagic.LibGraphics.Effects.TextureEffect;
import WildMagic.LibGraphics.Effects.VertexColor3Effect;
import WildMagic.LibGraphics.Rendering.CullState;
import WildMagic.LibGraphics.Rendering.WireframeState;
import WildMagic.LibGraphics.Rendering.ZBufferState;
import WildMagic.LibGraphics.SceneGraph.Attributes;
import WildMagic.LibGraphics.SceneGraph.Node;
import WildMagic.LibGraphics.SceneGraph.StandardMesh;
import WildMagic.LibGraphics.SceneGraph.TriMesh;
import WildMagic.LibGraphics.Sorting.BspNode;
import WildMagic.LibRenderers.OpenGLRenderer.OpenGLRenderer;

import com.jogamp.opengl.util.Animator;

public class BspNodes extends DemoBase
    implements GLEventListener, KeyListener
{

	private static final long serialVersionUID = -3814811425729875898L;

	/**
     * @param args
     */
    public static void main(String[] args) {
    	BspNodes kWorld = new BspNodes();
		/* Animator serves the purpose of the idle function, calls display: */
    	Frame frame = new Frame(kWorld.GetWindowTitle());
    	frame.add( kWorld.GetCanvas() );
    	frame.setSize(kWorld.GetCanvas().getWidth(), kWorld.GetCanvas().getHeight());
    	/* Animator serves the purpose of the idle function, calls display: */
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
    					System.exit(0);
    				}
    			}).start();
    		}
    	});
        frame.setVisible(true);
        animator.start();
    }


    private BspNode m_spkBsp;

    private WireframeState m_spkWireframe;

    // for rectangles used to visualize the binary separating planes
    private CullState m_spkRCull;

    private WireframeState m_spkRWireframe;

    public BspNodes()
    {
        super("BspNodes");
    }

    @Override
	public void display(GLAutoDrawable arg0) {
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
    }

    @Override
	public void dispose(GLAutoDrawable arg0)
    {
        ((OpenGLRenderer)m_pkRenderer).SetDrawable( arg0 );
        m_pkRenderer.ReleaseAllResources(m_spkBsp);
    }
    
    @Override
	public void init(GLAutoDrawable arg0) {
        ((OpenGLRenderer)m_pkRenderer).SetDrawable( arg0 );
        m_pkRenderer.InitializeState();
        super.OnInitialize();

        // set up camera
        m_spkCamera.SetFrustum(-0.055f,0.055f,-0.04125f,0.04125f,0.1f,100.0f);
        Vector3f kCLoc = new Vector3f(0.0f,-3.0f,0.25f);
        Vector3f kCDir = new Vector3f(0.0f,1.0f,0.0f);
        Vector3f kCUp = new Vector3f(0.0f,0.0f,1.0f);
        Vector3f kCRight = Vector3f.cross( kCDir, kCUp );
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
    }

    @Override
	public void keyTyped(KeyEvent e) {
        char ucKey = e.getKeyChar();
        
        System.err.println( ucKey );
        super.keyPressed(e);

        if (ucKey == 'w' || ucKey == 'W')
        {
            m_spkWireframe.Enabled = !m_spkWireframe.Enabled;
            return;
        }
        else if (ucKey == 's' || ucKey == 'S')
        {
            TestStreaming(m_spkScene,"BspNodes.wmof");
            return;
        }
    }
    @Override
	public void reshape(GLAutoDrawable arg0, int iX, int iY, int iWidth, int iHeight) {
        if (iWidth > 0 && iHeight > 0)
        {
            if (m_pkRenderer != null)
            {
                m_pkRenderer.Resize(iWidth,iHeight);
            }
            
            m_iWidth = iWidth;
            m_iHeight = iHeight;
        }
    }


	private BspNode CreateNode (final Vector2f rkV0, final Vector2f rkV1,
                                final ColorRGB rkColor)
    {
        Vector2f kDir = new Vector2f();
        kDir.Sub( rkV1, rkV0 );
        Vector3f kNormal = new Vector3f(kDir.Y,-kDir.X,0.0f);
        kNormal.normalize();
        float fConstant = kNormal.dot( new Vector3f(rkV0.X,rkV0.Y,0.0f));
        float fXExtent = 0.5f*kDir.Length();
        float fYExtent = 0.125f;
        Vector3f kTrn = new Vector3f(0.5f*(rkV0.X+rkV1.X),0.5f*(rkV0.Y+rkV1.Y),
                                     fYExtent+1e-03f);
        Matrix3f kRot = new Matrix3f(Vector3f.UNIT_Z, (float)Math.atan2(kDir.Y,kDir.X));
        kRot.mult( new Matrix3f(Vector3f.UNIT_X,Mathf.HALF_PI) );

        BspNode pkBsp = new BspNode(new Plane3f(kNormal,fConstant));

        Attributes kAttr = new Attributes();
        kAttr.SetPChannels(3);
        kAttr.SetCChannels(0,3);
        StandardMesh kSM = new StandardMesh(kAttr);
        TriMesh pkRect = kSM.Rectangle(2,2,fXExtent,fYExtent);
        pkRect.Local.SetTranslate(kTrn);
        pkRect.Local.SetRotate(kRot);
        for (int i = 0; i < 4; i++)
        {
            pkRect.VBuffer.SetColor3(0,i, rkColor);
        }

        pkRect.AttachEffect(new VertexColor3Effect());

        pkRect.AttachGlobalState(m_spkRCull);
        pkRect.AttachGlobalState(m_spkRWireframe);

        pkBsp.AttachCoplanarChild(pkRect);

        return pkBsp;
    }
    
	private void CreateScene ()
    {
        m_spkScene = new Node();
        m_spkWireframe = new WireframeState();
        m_spkScene.AttachGlobalState(m_spkWireframe);

        // Disable z-buffering.
        ZBufferState pkZS = new ZBufferState();
        pkZS.Enabled = false;
        pkZS.Writable = false;
        m_spkScene.AttachGlobalState(pkZS);

        // Create the ground.  It covers a square with vertices (1,1,0), (1,-1,0),
        // (-1,1,0), and (-1,-1,0).
        Attributes kAttr = new Attributes();
        kAttr.SetPChannels(3);
        kAttr.SetTChannels(0,2);
        StandardMesh kSM = new StandardMesh(kAttr);
        TriMesh pkGround = kSM.Rectangle(2,2,16.0f,16.0f);

        ShaderEffect pkEffect = new TextureEffect("Horizontal");
        pkGround.AttachEffect(pkEffect);
        pkGround.UpdateRS();
        m_spkScene.AttachChild(pkGround);

        // Partition the region above the ground into 5 convex pieces.  Each
        // plane is perpendicular to the ground (not required generally).  A
        // rectangle mesh representing the plane is attached to the plane as the
        // coplanar child.  Rectangles will be attached to each binary separating
        // plane, just for visualization purposes.  Disable back-face culling so
        // the rectangles are always visible.  Enable wireframe to see through the
        // rectangles.
        m_spkRCull = new CullState();
        m_spkRCull.Enabled = false;
        m_spkRWireframe = new WireframeState();
        m_spkRWireframe.Enabled = true;

        Vector2f kV0 = new Vector2f(-1.0f,1.0f);
        Vector2f kV1 = new Vector2f(1.0f,-1.0f);
        Vector2f kV2 = new Vector2f(-0.25f,0.25f);
        Vector2f kV3 = new Vector2f(-1.0f,-1.0f);
        Vector2f kV4 = new Vector2f(0.0f,0.0f);
        Vector2f kV5 = new Vector2f(1.0f,0.5f);
        Vector2f kV6 = new Vector2f(-0.75f,-7.0f/12.0f);
        Vector2f kV7 = new Vector2f(-0.75f,0.75f);
        Vector2f kV8 = new Vector2f(1.0f,1.0f);

        BspNode pkBsp0 = CreateNode(kV0,kV1,new ColorRGB(1.0f,0.0f,0.0f));
        BspNode pkBsp1 = CreateNode(kV2,kV3,new ColorRGB(0.0f,0.5f,0.0f));
        BspNode pkBsp2 = CreateNode(kV4,kV5,new ColorRGB(0.0f,0.0f,1.0f));
        BspNode pkBsp3 = CreateNode(kV6,kV7,new ColorRGB(0.0f,0.0f,0.0f));

        m_spkBsp = pkBsp0;
        pkBsp0.AttachPositiveChild(pkBsp1);
        pkBsp0.AttachNegativeChild(pkBsp2);
        pkBsp1.AttachPositiveChild(pkBsp3);

        // Attach an object in each convex region.  Since at least one object
        // is nonconvex, z-buffering must be enabled for that object.  However,
        // only z-writes need to occur for the convex objects.
        float fHeight = 0.1f;
        Vector2f kCenter = new Vector2f();
        TriMesh pkMesh;

        // (R0) Create a torus mesh.  The torus is not convex, so z-buffering is
        // required to obtain correct drawing.
        pkMesh = kSM.Torus(16,16,1.0f,0.25f);
        pkMesh.AttachGlobalState(new ZBufferState());
        pkEffect = new TextureEffect("Flower");
        pkMesh.AttachEffect(pkEffect);
        pkMesh.Local.SetUniformScale(0.1f);
        kCenter.Add(kV2,kV6);
        kCenter.Add(kV7);
        kCenter.Scale(1.0f/3.0f);
        pkMesh.Local.SetTranslate(new Vector3f(kCenter.X,kCenter.Y,fHeight));
        pkBsp3.AttachPositiveChild(pkMesh);

        // The remaining objects are convex, so z-buffering is not required for
        // them within the BSP tree because of the correct object sorting that
        // tree does.
        ZBufferState pkZBuffer = new ZBufferState();
        pkZBuffer.Enabled = false;
        pkZBuffer.Writable = false;

        // (R1) create a sphere mesh
        pkMesh = kSM.Sphere(32,16,1.0f);
        pkMesh.AttachGlobalState(pkZBuffer);
        pkMesh.AttachEffect(pkEffect);
        pkMesh.Local.SetUniformScale(0.1f);
        kCenter.Add(kV0,kV3);
        kCenter.Add(kV6);
        kCenter.Add(kV7);
        kCenter.Scale(1.0f/4.0f);
        pkMesh.Local.SetTranslate( new Vector3f(kCenter.X,kCenter.Y,fHeight));
        pkBsp3.AttachNegativeChild(pkMesh);

        // (R2) create a tetrahedron
        pkMesh = kSM.Tetrahedron();
        pkMesh.AttachGlobalState(pkZBuffer);
        pkMesh.AttachEffect(pkEffect);
        pkMesh.Local.SetUniformScale(0.1f);
        kCenter.Add(kV1,kV2);
        kCenter.Add(kV3);
        kCenter.Scale(1.0f/3.0f);
        pkMesh.Local.SetTranslate( new Vector3f(kCenter.X,kCenter.Y,fHeight));
        pkBsp1.AttachNegativeChild(pkMesh);

        // (R3) create a hexahedron
        pkMesh = kSM.Hexahedron();
        pkMesh.AttachGlobalState(pkZBuffer);
        pkMesh.AttachEffect(pkEffect);
        pkMesh.Local.SetUniformScale(0.1f);
        kCenter.Add(kV1,kV4);
        kCenter.Add(kV5);
        kCenter.Scale(1.0f/3.0f);
        pkMesh.Local.SetTranslate(new Vector3f(kCenter.X,kCenter.Y,fHeight));
        pkBsp2.AttachPositiveChild(pkMesh);

        // (R4) create an octahedron
        pkMesh = kSM.Octahedron();
        pkMesh.AttachGlobalState(pkZBuffer);
        pkMesh.AttachEffect(pkEffect);
        pkMesh.Local.SetUniformScale(0.1f);
        kCenter.Add(kV0,kV4);
        kCenter.Add(kV5);
        kCenter.Add(kV8);
        kCenter.Scale(1.0f/4.0f);
        pkMesh.Local.SetTranslate( new Vector3f(kCenter.X,kCenter.Y,fHeight));
        pkBsp2.AttachNegativeChild(pkMesh);

        m_spkScene.AttachChild(m_spkBsp);
    }
}
