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

import gov.nih.mipav.view.renderer.WildMagic.Render.VolumePreRenderEffect;

import java.awt.Frame;
import java.awt.event.KeyEvent;
import java.awt.event.KeyListener;
import java.awt.event.WindowAdapter;
import java.awt.event.WindowEvent;
import java.util.Iterator;
import java.util.TreeSet;

import javax.media.opengl.GL3;
import javax.media.opengl.GLAutoDrawable;
import javax.media.opengl.GLEventListener;
import javax.media.opengl.awt.GLCanvas;

import WildMagic.LibApplications.OpenGLApplication.ApplicationGUI;
import WildMagic.LibFoundation.Mathematics.ColorRGB;
import WildMagic.LibFoundation.Mathematics.ColorRGBA;
import WildMagic.LibFoundation.Mathematics.Mathf;
import WildMagic.LibFoundation.Mathematics.Matrix3f;
import WildMagic.LibFoundation.Mathematics.Vector3f;
import WildMagic.LibFoundation.Meshes.EdgeKey;
import WildMagic.LibGraphics.Effects.IridescenceEffect;
import WildMagic.LibGraphics.Effects.VertexColor3Effect;
import WildMagic.LibGraphics.Rendering.CullState;
import WildMagic.LibGraphics.Rendering.FrameBuffer;
import WildMagic.LibGraphics.Rendering.MaterialState;
import WildMagic.LibGraphics.Rendering.WireframeState;
import WildMagic.LibGraphics.SceneGraph.Attributes;
import WildMagic.LibGraphics.SceneGraph.IndexBuffer;
import WildMagic.LibGraphics.SceneGraph.Node;
import WildMagic.LibGraphics.SceneGraph.Polyline;
import WildMagic.LibGraphics.SceneGraph.StandardMesh;
import WildMagic.LibGraphics.SceneGraph.TriMesh;
import WildMagic.LibGraphics.SceneGraph.VertexBuffer;
import WildMagic.LibRenderers.OpenGLRenderer.OpenGLRenderer;

import com.jogamp.opengl.util.Animator;


public class GelatinBlob extends DemoBase implements GLEventListener, KeyListener {

	private static final long serialVersionUID = 7201911098737257202L;
	
	/**
	 * Iridescence.main creates the Iridescence object and window frame to contain the GLCanvas. An Animator object is
	 * created with the GLCanvas as an argument. The Animator provides the same function as the glutMainLoop() function
	 * call commonly used in OpenGL applications.
	 */
	public static void main(String[] args) {
		GelatinBlob kWorld = new GelatinBlob();
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
        animator.setRunAsFastAsPossible(true);
        animator.start();
	}	
	
	/**
	 * Iridescence.main creates the Iridescence object and window frame to contain the GLCanvas. An Animator object is
	 * created with the GLCanvas as an argument. The Animator provides the same function as the glutMainLoop() function
	 * call commonly used in OpenGL applications.
	 */
	public static void main(GLCanvas kWindow, Node scene, boolean bShared) {
		Iridescence kWorld = new Iridescence(kWindow,scene,bShared);
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
        animator.setRunAsFastAsPossible(true);
        animator.start();
	}

	private WireframeState m_spkWireframe;
	private CullState m_kCull;

	private IridescenceEffect m_spkEffect;

	/** Window with the shader parameter interface: */
	private ApplicationGUI m_kShaderParamsWindow = null;

	private boolean m_bStereo = false;

	private boolean m_bLeft = true;

	private boolean m_bRight = true;

    /** For testing the frame rate: */
    protected boolean m_bTestFrameRate = false;
    /** Rotation during frame rate tests: */
    protected Matrix3f m_kRotate = new Matrix3f();
	private TriMesh m_pkMesh;


	/**
	 * The constructor initializes the OpenGLRender, and sets up the GLEvent, KeyEvent, and Mouse listeners. The last
	 * three statements initialize the ImageCatalog, VertexProgramCatalog, and PixelProgramCatalog. The three catalogs
	 * enable sharing of texture images, vertex program, and pixel programs in a program. The catalogs keep track of
	 * images and programs that are currently loaded. If multiple effects use the same images and shader programs, the
	 * loaded version is re-used. This saves graphics hardware memory as well as time for re-loading large textures.
	 * 
	 */
	public GelatinBlob() {
		super("GelatinBlob");
        m_kRotate.fromAxisAngle(Vector3f.UNIT_Y, (float)Math.PI/18.0f);
	}
	
	

	public GelatinBlob(GLCanvas kWindow, Node scene, boolean bShared) {
		super("GelatinBlob", kWindow);
		m_spkScene = scene;
		m_bShared = bShared;
        m_kRotate.fromAxisAngle(Vector3f.UNIT_Y, (float)Math.PI/18.0f);
	}
	/**
	 * Iridescence.display() displays the scene. The frame rate is measured. Any camera motion that has occurred since
	 * the last frame was displayed is applied and the culling system updated. Any object motions that has occurred is
	 * also applied and the culling system updated. Renderer.ClearBuffers() is called, the scene and framerate are
	 * drawn, and the back-buffer displayed with Renderer.DisplayBackBuffer().
	 */
	@Override
	public void display(GLAutoDrawable arg0) {

	    MeasureTime();

	    MoveCamera();
	    if (MoveObject())
	    {
			m_spkScene.UpdateGS();
			m_kCuller.ComputeVisibleSet(m_spkScene);
	    }

	    PhysicsTick();

		m_pkRenderer.ClearBuffers();
		if (m_pkRenderer.BeginScene()) {
			m_pkRenderer.DrawScene(m_kCuller.GetVisibleSet());
			DrawFrameRate(8, GetHeight() - 8, ColorRGBA.WHITE);
			m_pkRenderer.EndScene();
		}
		m_pkRenderer.DisplayBackBuffer();

	    UpdateFrameCount();		
	}

	@Override
	public void dispose(GLAutoDrawable arg0)
	{
        ((OpenGLRenderer)m_pkRenderer).SetDrawable( arg0 );
        m_pkRenderer.ReleaseAllResources( m_spkScene );
	}

	
	/**
	 * Iridescence.init is called only once when the GLCanvas is initialized. It initializes the renderer object, sets
	 * up the camera model, creates the scene, and initializes the culling object with the camera and scene objects.
	 */
	@Override
	public void init(GLAutoDrawable arg0) {
		((OpenGLRenderer)m_pkRenderer).SetDrawable( arg0 );
		m_pkRenderer.InitializeState();
		super.OnInitialize();

		m_spkCamera.SetFrustum( -0.55f, 0.55f, -0.4125f, 0.4125f, 1.0f, 100.0f);
		Vector3f kCLoc = new Vector3f(0.0f, 0.0f, -8.0f);
		Vector3f kCDir = new Vector3f(0.0f, 0.0f, 1.0f);
		Vector3f kCUp = new Vector3f(0.0f, 1.0f, 0.0f);
		Vector3f kCRight = Vector3f.cross(kCDir, kCUp);
		m_spkCamera.SetFrame(kCLoc, kCDir, kCUp, kCRight);

		if ( !m_bShared )
		{
			CreateScene();

			// initial update of objects
			m_spkScene.UpdateGS();
			m_spkScene.UpdateRS();			
		}

		// initial culling of scene
		m_kCuller.SetCamera(m_spkCamera);
		m_kCuller.ComputeVisibleSet(m_spkScene);

		InitializeCameraMotion(0.01f, 0.001f);
		InitializeObjectMotion(m_spkScene);
	}

	/**
	 * Iridescence.keyPressed() processes key-input from the user. The iridescence factor shader parameter can be
	 * increased and decreased by pressing the + and - keys. A shader-editor GUI can be launched by pressing 'l'. The
	 * scene-graph is streamed to disk by pressing the 's' key.
	 */
	@Override
	public void keyTyped(KeyEvent e) {
		char ucKey = e.getKeyChar();
		System.err.println( ucKey );
		super.keyPressed(e);
		float fInterpolateFactor;
		if (ucKey == 'w' || ucKey == 'W')
		{
			m_spkWireframe.Enabled = true;
			m_spkWireframe.Fill = ( m_spkWireframe.Fill == WireframeState.FillMode.FM_FILL ) ? 
					WireframeState.FillMode.FM_LINE : ( m_spkWireframe.Fill == WireframeState.FillMode.FM_LINE ) ?
							WireframeState.FillMode.FM_POINT : 	WireframeState.FillMode.FM_FILL;
			m_spkScene.UpdateGS();
			return;
		}
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
		case 'c':
	        m_kCull.CullFace = CullState.CullMode.CT_FRONT;
		case 'b':
	        m_kCull.CullFace = CullState.CullMode.CT_BACK;
		}
		return;
	}

	@Override
	public void reshape(GLAutoDrawable arg0, int iX, int iY, int iWidth, int iHeight) {
		if (iWidth > 0 && iHeight > 0) {
			if (m_pkRenderer != null) {
				m_pkRenderer.Resize(iWidth, iHeight);
			}

			m_iWidth = iWidth;
			m_iHeight = iHeight;
		}
	}

	/**
	 * Iridescence.CreateScene() creates the scene graph. The root node is m_spkScene. It contains a single TriMesh
	 * object, the torus. The TriMesh object is created with a set of rendering Attributes with three channels for point
	 * data (x,y,z); three channels for normal data (x,y,z); and two channels for texture-coordinate data (s,t). An
	 * IridescenceEffect is created and attached to the torus.
	 */
	private void CreateScene() {
		m_spkScene = new Node();
		m_spkWireframe = new WireframeState();
        m_spkScene.AttachGlobalState(m_spkWireframe);

        m_kCull = new CullState();
        m_kCull.CullFace = CullState.CullMode.CT_BACK;
        m_kCull.FrontFace = CullState.FrontMode.FT_CCW;
        m_spkScene.AttachGlobalState(m_kCull);

        CreateIcosahedron();
        CreateSprings();
        CreateSegments();

        m_spkScene.AttachChild(mSegments);
        m_spkScene.AttachChild(mIcosahedron);
	}	
	
	private TriMesh mIcosahedron;
	private void CreateIcosahedron ()
	{
		Attributes attributes = new Attributes();
		attributes.SetPChannels(3);
		attributes.SetNChannels(3);
		attributes.SetTChannels(0, 2);
		StandardMesh stdMesh = new StandardMesh(attributes);
	    mIcosahedron = stdMesh.Icosahedron();

	    IridescenceEffect effect = new IridescenceEffect("Leaf", "Gradient");
	    effect.SetInterpolateFactor(0.5f);
		int iPassQuantity = effect.GetPassQuantity();
		for (int iPass = 0; iPass < iPassQuantity; iPass++) {
			effect.LoadPrograms(m_pkRenderer, mIcosahedron, iPass, m_pkRenderer.GetMaxColors(), m_pkRenderer.GetMaxTCoords(),
					m_pkRenderer.GetMaxVShaderImages(), m_pkRenderer.GetMaxPShaderImages());
		}
		mIcosahedron.AttachEffect(effect);
	}
	
	private PhysicsModule mModule;
	private void CreateSprings ()
	{
	    // The icosahedron has 12 vertices and 30 edges.  Each vertex is a
	    // particle in the system.  Each edge represents a spring.  To keep the
	    // icosahedron from collapsing, 12 immovable particles are added, each
	    // outside the icosahedron in the normal direction above a vertex.  The
	    // immovable particles are connected to their corresponding vertices with
	    // springs.
	    int numParticles = 24, numSprings = 42;

	    // Viscous forces applied.  If you set viscosity to zero, the cuboid
	    // wiggles indefinitely since there is no dissipation of energy.  If
	    // the viscosity is set to a positive value, the oscillations eventually
	    // stop.  The length of time to steady state is inversely proportional
	    // to the viscosity.
	    float step = 0.1f;  // simulation needs to run slower in release mode
	    float viscosity = 0.1f;
	    mModule = new PhysicsModule(numParticles, numSprings, step, viscosity);

	    // Set positions and velocities.  The first 12 positions are the vertices
	    // of the icosahedron.  The last 12 are the extra particles added to
	    // stabilize the system.
	    int i;
	    for (i = 0; i < 12; ++i)
	    {
	        mModule.SetMass(i, 1.0f);
	        mModule.SetPosition(i, mIcosahedron.VBuffer.GetPosition3(i) );
	        mModule.SetVelocity(i, new Vector3f(0.7f*Mathf.SymmetricRandom(), 0.7f*Mathf.SymmetricRandom(), 0.7f*Mathf.SymmetricRandom()));
	    }
	    for (i = 12; i < 24; ++i)
	    {
	        mModule.SetMass(i, Float.MAX_VALUE);
	        mModule.SetPosition(i, Vector3f.scale(2.0f, mIcosahedron.VBuffer.GetPosition3(i - 12) ));
	        mModule.SetVelocity(i, Vector3f.ZERO);
	    }

	    // Get unique set of edges for icosahedron.
	    TreeSet<EdgeKey> edgeSet = new TreeSet<EdgeKey>();
	    int numTriangles = mIcosahedron.GetTriangleQuantity();
	    int[] indices = mIcosahedron.IBuffer.GetData();
	    int index = 0;
	    for (i = 0; i < numTriangles; ++i)
	    {
	        int v0 = indices[index++];
	        int v1 = indices[index++];
	        int v2 = indices[index++];
	        edgeSet.add(new EdgeKey(v0, v1));
	        edgeSet.add(new EdgeKey(v1, v2));
	        edgeSet.add(new EdgeKey(v2, v0));
	    }

	    // Springs are at rest in the initial configuration.
	    final float constant = 10.0f;
	    Vector3f diff;
	    int spring = 0;
	    Iterator<EdgeKey> iterator = edgeSet.iterator();
	    while ( iterator.hasNext() )
	    {
	        final EdgeKey key = iterator.next();
	        int v0 = key.V[0], v1 = key.V[1];
	        diff = Vector3f.sub( mModule.GetPosition(v1), mModule.GetPosition(v0) );
	        mModule.SetSpring(spring, v0, v1, constant, diff.length());
	        ++spring;
	    }
	    for (i = 0; i < 12; ++i)
	    {
	        diff = Vector3f.sub( mModule.GetPosition(i + 12), mModule.GetPosition(i) );
	        mModule.SetSpring(spring, i, i + 12, constant, diff.Length());
	        ++spring;
	    }
	}

	Node mSegments;
	private void CreateSegments ()
	{
		Attributes attributes = new Attributes();
		attributes.SetPChannels(3);
		attributes.SetCChannels(0, 4);

	    VertexColor3Effect effect = new VertexColor3Effect();

	    int numSprings = mModule.GetNumSprings();
	    mSegments = new Node();
        float[] constant = new float[1];
        float[] length = new float[1];
        int[] v0 = new int[1];
        int[] v1 = new int[1];
	    for (int i = 0; i < numSprings; ++i)
	    {
	        mModule.GetSpring(i, v0, v1, constant, length);

	        VertexBuffer vbuffer = new VertexBuffer(attributes, 2);
	        vbuffer.SetPosition3(0, mModule.GetPosition(v0[0]) );
	        vbuffer.SetPosition3(1, mModule.GetPosition(v1[0]) );
	        vbuffer.SetColor4(0, 0, 1, 1, 1, 1);
	        vbuffer.SetColor4(0, 1, 1, 1, 1, 1);
	        Polyline segment = new Polyline(vbuffer, false, true);
	        segment.AttachEffect(effect);

	        mSegments.AttachChild(segment);
	    }
	}
	
	private void PhysicsTick ()
	{
	    mModule.Update((float)(System.currentTimeMillis() / 1000.0) );

	    // Update lattice.  The particle system and lattice maintain
	    // their own copy of the vertices, so this update is necessary.
	    int i;
	    for (i = 0; i < 12; ++i)
	    {
	    	mIcosahedron.VBuffer.SetPosition3(i, mModule.GetPosition(i));
	    }
	    mIcosahedron.UpdateGS();
	    mIcosahedron.UpdateMS();
	    mIcosahedron.UpdateRS();
	    mIcosahedron.Reload(true);

	    // Update the segments representing the springs.
	    int numSprings = mModule.GetNumSprings();
        float[] constant = new float[1];
        float[] length = new float[1];
        int[] v0 = new int[1];
        int[] v1 = new int[1];
	    for (i = 0; i < numSprings; ++i)
	    {
	        mModule.GetSpring(i, v0, v1, constant, length);

	        Polyline segment = (Polyline) mSegments.GetChild(i);
	        segment.VBuffer.SetPosition3(0, mModule.GetPosition(v0[0]) );
	        segment.VBuffer.SetPosition3(1, mModule.GetPosition(v1[0]) );
	        segment.Reload(true);
	    }
	}
}
