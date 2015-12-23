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
// Adapted from Iridescene demo, (Nov 07)
//

package WildMagic.ApplicationDemos;


import java.awt.Frame;
import java.awt.event.KeyEvent;
import java.awt.event.KeyListener;
import java.awt.event.WindowAdapter;
import java.awt.event.WindowEvent;

import javax.media.opengl.GL3;
import javax.media.opengl.GLAutoDrawable;
import javax.media.opengl.GLEventListener;
import javax.media.opengl.awt.GLCanvas;

import WildMagic.LibApplications.OpenGLApplication.ApplicationGUI;
import WildMagic.LibFoundation.Mathematics.ColorRGB;
import WildMagic.LibFoundation.Mathematics.ColorRGBA;
import WildMagic.LibFoundation.Mathematics.Matrix3f;
import WildMagic.LibFoundation.Mathematics.Vector3f;
import WildMagic.LibGraphics.Effects.LatticeEffect;
import WildMagic.LibGraphics.Effects.LightingEffect;
import WildMagic.LibGraphics.Rendering.AlphaState;
import WildMagic.LibGraphics.Rendering.FrameBuffer;
import WildMagic.LibGraphics.Rendering.Light;
import WildMagic.LibGraphics.Rendering.MaterialState;
import WildMagic.LibGraphics.SceneGraph.Attributes;
import WildMagic.LibGraphics.SceneGraph.Node;
import WildMagic.LibGraphics.SceneGraph.StandardMesh;
import WildMagic.LibGraphics.SceneGraph.TriMesh;
import WildMagic.LibRenderers.OpenGLRenderer.OpenGLRenderer;

import com.jogamp.opengl.util.Animator;

public class Lattice extends DemoBase implements GLEventListener, KeyListener {
	
	private static final long serialVersionUID = -9076947736708702300L;

	/**
	 * Lattice.main creates the Lattice object and window frame to contain the GLCanvas. An Animator object is created
	 * with the GLCanvas as an argument. The Animator provides the same function as the glutMainLoop() function call
	 * commonly used in OpenGL applications.
	 */
	public static void main(String[] args) {
		Lattice kWorld = new Lattice();
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
	
	public static void main(GLCanvas kWindow, Node scene, boolean bShared) {
		Lattice kWorld = new Lattice(kWindow,scene,bShared);
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

    /** For testing the frame rate: */
    protected boolean m_bTestFrameRate = false;
    /** Rotation during frame rate tests: */
    protected Matrix3f m_kRotate = new Matrix3f();
	
	private LatticeEffect m_spkEffect;

	private TriMesh m_pkMesh;

	private Light[] m_aspkDLight = new Light[2];

	private MaterialState m_pkMaterial;

	private ColorRGB m_kDiffuseColor;

	/** Window with the shader parameter interface: */
	private ApplicationGUI m_kShaderParamsWindow = null;
	
	// toggle stereo on/off with the 's' or 'S' key:
	private boolean m_bStereo = false;

	/**
	 * The constructor initializes the OpenGLRender, and sets up the GLEvent, KeyEvent, and Mouse listeners. The last
	 * three statements initialize the ImageCatalog, VertexProgramCatalog, and PixelProgramCatalog. The three catalogs
	 * enable sharing of texture images, vertex program, and pixel programs in a program. The catalogs keep track of
	 * images and programs that are currently loaded. If multiple effects use the same images and shader programs, the
	 * loaded version is re-used. This saves graphics hardware memory as well as time for re-loading large textures.
	 * 
	 */
	public Lattice() {
		super( "Lattice"  );
        m_kRotate.fromAxisAngle(Vector3f.UNIT_Y, (float)Math.PI/18.0f);
	}

	public Lattice(GLCanvas kWindow, Node scene, boolean bShared) {
		super("Lattice", kWindow);

		m_spkScene = scene;
		m_bShared = bShared;
        m_kRotate.fromAxisAngle(Vector3f.UNIT_Y, (float)Math.PI/18.0f);
	}

	/**
	 * Lattice.display() displays the scene. The frame rate is measured. Any camera motion that has occurred since the
	 * last frame was displayed is applied and the culling system updated. Any object motions that has occurred is also
	 * applied and the culling system updated. Renderer.ClearBuffers() is called, the scene and framerate are drawn, and
	 * the back-buffer displayed with Renderer.DisplayBackBuffer().
	 */
	@Override
	public void display(GLAutoDrawable arg0) {
		MeasureTime();
		if (MoveCamera()) {
			m_kCuller.ComputeVisibleSet(m_spkScene);
		}
		if (MoveObject()) {
			m_spkScene.UpdateGS();
			m_kCuller.ComputeVisibleSet(m_spkScene);
		}
        if ( m_bTestFrameRate )
        {
            Matrix3f kRotate = m_spkScene.Local.GetRotate();
            kRotate.mult(m_kRotate);
            m_spkScene.Local.SetRotate(kRotate);
            m_spkScene.UpdateGS();
            m_kCuller.ComputeVisibleSet(m_spkScene);
            
            if ( m_pkMesh != null )
            {
            	m_pkMesh.Local.SetRotateCopy(m_spkScene.Local.GetRotate());
            }
        }
		if (m_pkRenderer.BeginScene()) {
			if ( m_bStereo )
			{
				// Move view point right for right-eye view:
					MoveRight();
					m_kCuller.ComputeVisibleSet(m_spkScene);
					m_pkRenderer.DrawRight();
					m_pkRenderer.ClearBuffers();          
					m_pkRenderer.DrawScene(m_kCuller.GetVisibleSet());    
					// Move view point left (back to center):
					MoveLeft();
					// Move view point left for left-eye view:
					MoveLeft();
					m_kCuller.ComputeVisibleSet(m_spkScene);
					m_pkRenderer.DrawLeft();
					m_pkRenderer.ClearBuffers();
					m_pkRenderer.DrawScene(m_kCuller.GetVisibleSet());
					// Move view point right (back to center)
					MoveRight();
			}
			else
			{
				m_kCuller.ComputeVisibleSet(m_spkScene);
				m_pkRenderer.ClearBuffers();          
				m_pkRenderer.DrawScene(m_kCuller.GetVisibleSet()); 
				DrawFrameRate(8,GetHeight()-32,ColorRGBA.BLACK); 
			}
		}
		m_pkRenderer.DrawDefault();
		m_pkRenderer.DisplayBackBuffer();

		if (m_kShaderParamsWindow == null && !m_bShared) {
			m_kShaderParamsWindow = new ApplicationGUI();
			m_kShaderParamsWindow.setParent(this);
			m_kShaderParamsWindow.AddUserVariables(m_spkEffect.GetCProgram(0));
			m_kShaderParamsWindow.Display();
			m_kShaderParamsWindow.setParent(this);
		}
		UpdateFrameCount();
	}

	@Override
	public void dispose(GLAutoDrawable arg0)
	{
        ((OpenGLRenderer)m_pkRenderer).SetDrawable( arg0 );
        m_pkRenderer.ReleaseAllResources( m_spkScene );
	}

	/**
	 * Lattice.init is called only once when the GLCanvas is initialized. It initializes the renderer object, sets up
	 * the camera model, creates the scene, and initializes the culling object with the camera and scene objects.
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

		byte[] bStereo = new byte[1];
		arg0.getGL().glGetBooleanv( GL3.GL_STEREO, bStereo, 0 );
		if ( bStereo[0] == 0 )
		{
			System.err.println( "Stereo capable = false" );
		}
		else
		{
			System.err.println( "Stereo capable = true" );
		}
	}

	/**
	 * Lattice.keyPressed() processes key-input from the user. The iridescence factor shader parameter can be increased
	 * and decreased by pressing the + and - keys. A shader-editor GUI can be launched by pressing 'l'. The scene-graph
	 * is streamed to disk by pressing the 's' key.
	 */
	@Override
	public void keyTyped(KeyEvent e) {
		char ucKey = e.getKeyChar();
		super.keyPressed(e);
		switch (ucKey) {
        case 'f':
            m_bTestFrameRate = !m_bTestFrameRate;
            if ( m_bTestFrameRate )
            {
                ResetTime();
            }
            return;
		case 'l':
		case 'L':
			if ( m_spkEffect != null )
			{
				ApplicationGUI kShaderParamsWindow = new ApplicationGUI();
				kShaderParamsWindow.setParent(this);
				kShaderParamsWindow.AddUserVariables(m_spkEffect.GetCProgram(0));
				kShaderParamsWindow.Display();
			}
			return;
		case 's':
		case 'S':
			m_bStereo = !m_bStereo;
			return;
		case 'i':
			m_fTrnSpeed += .05;      
			return;
		case 'o':
			m_fTrnSpeed -= .05;        
			return;
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
	 * Lattice.CreateScene() creates the scene graph. The root node is m_spkScene. It contains a single TriMesh object,
	 * the torus. The TriMesh object is created with a set of rendering Attributes with three channels for point data
	 * (x,y,z); three channels for normal data (x,y,z); and two channels for texture-coordinate data (s,t). An
	 * LatticeEffect is created and attached to the torus.
	 */
	private void CreateScene() {
		m_spkScene = new Node();
		Attributes kAttr = new Attributes();
		kAttr.SetPChannels(3);
		kAttr.SetNChannels(3);
		kAttr.SetTChannels(0, 2);
		StandardMesh kSM = new StandardMesh(kAttr);
		m_pkMesh = kSM.Torus(220, 220, 2.0f, 1.0f);

		m_kDiffuseColor = new ColorRGB(0.34615f, 0.3143f, 0.0903f);

		// polished gold
		m_pkMaterial = new MaterialState();
		m_pkMaterial.Emissive = new ColorRGB(ColorRGB.BLACK);
		m_pkMaterial.Ambient = new ColorRGB(0.24725f, 0.2245f, 0.0645f);
		m_pkMaterial.Diffuse = m_kDiffuseColor;
		m_pkMaterial.Specular = new ColorRGB(0.797357f, 0.723991f, 0.208006f);
		m_pkMaterial.Shininess = 83.2f;
		m_pkMesh.AttachGlobalState(m_pkMaterial);

		m_spkEffect = new LatticeEffect("Leaf", "Gradient");
		final int iPassQuantity = m_spkEffect.GetPassQuantity();
		for (int iPass = 0; iPass < iPassQuantity; iPass++) {
			m_spkEffect.LoadPrograms(m_pkRenderer, m_pkMesh, iPass, m_pkRenderer.GetMaxColors(), m_pkRenderer.GetMaxTCoords(),
					m_pkRenderer.GetMaxVShaderImages(), m_pkRenderer.GetMaxPShaderImages());
		}
		m_pkMesh.AttachEffect(m_spkEffect);

		int i;
		for (i = 0; i < 2; i++) {
			m_aspkDLight[i] = new Light(Light.LightType.LT_DIRECTIONAL);
		}
		float fValue = (float) -Math.sqrt(1.0f / 3.0f);
		m_aspkDLight[0].Ambient = new ColorRGB(fValue, fValue, fValue);
		m_aspkDLight[1].Ambient = new ColorRGB(fValue, 0.0f, 0.0f);
		m_aspkDLight[0].DVector = new Vector3f( +fValue, +fValue, +fValue);
		m_aspkDLight[1].DVector = new Vector3f( +fValue, +fValue, -fValue);
		for (i = 0; i < 2; i++) {
			m_aspkDLight[i].Diffuse = new ColorRGB(ColorRGB.WHITE);
			m_aspkDLight[i].Specular = new ColorRGB(ColorRGB.WHITE);
		}

		LightingEffect kL0 = new LightingEffect();
		kL0.AttachLight(m_aspkDLight[0]);
		kL0.Configure();
		AlphaState pkAS;

		pkAS = kL0.GetBlending(0);
		pkAS.BlendEnabled = true;
		pkAS.SrcBlend = AlphaState.SrcBlendMode.SBF_ONE_MINUS_DST_COLOR;
		pkAS.DstBlend = AlphaState.DstBlendMode.DBF_ONE;

		LightingEffect kL1 = new LightingEffect();
		kL1.AttachLight(m_aspkDLight[1]);
		kL1.Configure();
		pkAS = kL1.GetBlending(0);
		pkAS.BlendEnabled = true;
		pkAS.SrcBlend = AlphaState.SrcBlendMode.SBF_ONE_MINUS_DST_COLOR;
		pkAS.DstBlend = AlphaState.DstBlendMode.DBF_ONE;

		m_pkMesh.AttachEffect(kL0);
		m_pkMesh.AttachEffect(kL1);
		m_pkMesh.UpdateRS();

		m_spkScene.AttachChild(m_pkMesh);
	}
}
