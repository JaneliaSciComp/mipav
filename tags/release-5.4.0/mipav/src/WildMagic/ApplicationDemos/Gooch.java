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
// Adapted from Iridescene demo, (Nov 07)
//

package WildMagic.ApplicationDemos;


import java.awt.Frame;
import java.awt.event.KeyEvent;
import java.awt.event.KeyListener;
import java.awt.event.WindowAdapter;
import java.awt.event.WindowEvent;

import javax.media.opengl.GLAutoDrawable;
import javax.media.opengl.GLEventListener;

import WildMagic.LibApplications.OpenGLApplication.ApplicationGUI;
import WildMagic.LibFoundation.Mathematics.ColorRGBA;
import WildMagic.LibFoundation.Mathematics.Vector3f;
import WildMagic.LibGraphics.Effects.Effect;
import WildMagic.LibGraphics.Effects.GoochEffect;
import WildMagic.LibGraphics.Effects.WireframeBehindEffect;
import WildMagic.LibGraphics.SceneGraph.Attributes;
import WildMagic.LibGraphics.SceneGraph.Node;
import WildMagic.LibGraphics.SceneGraph.StandardMesh;
import WildMagic.LibGraphics.SceneGraph.TriMesh;
import WildMagic.LibRenderers.OpenGLRenderer.OpenGLRenderer;

import com.jogamp.opengl.util.Animator;

public class Gooch extends DemoBase
implements GLEventListener, KeyListener
{
	private static final long serialVersionUID = -1402084549384096973L;

	/**
	 * Gooch.main creates the Gooch object and window frame to
	 * contain the GLCanvas. An Animator object is created with the GLCanvas
	 * as an argument. The Animator provides the same function as the
	 * glutMainLoop() function call commonly used in OpenGL applications. */
	public static void main(String[] args) {
		Gooch kWorld = new Gooch();        
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

	private GoochEffect m_spkEffect;

	private TriMesh m_pkMesh;

	/** Window with the shader parameter interface: */
	private ApplicationGUI m_kShaderParamsWindow = null;

	/**
	 * The constructor initializes the OpenGLRender, and sets up the GLEvent,
	 * KeyEvent, and Mouse listeners.  The last three statements initialize
	 * the ImageCatalog, VertexProgramCatalog, and PixelProgramCatalog. The
	 * three catalogs enable sharing of texture images, vertex program, and
	 * pixel programs in a program. The catalogs keep track of images and
	 * programs that are currently loaded. If multiple effects use the same
	 * images and shader programs, the loaded version is re-used. This saves
	 * graphics hardware memory as well as time for re-loading large textures.
	 *
	 */
	public Gooch() {
		super("Gooch");
	}

	/**
	 * Gooch.display() displays the scene. The frame rate is
	 * measured. Any camera motion that has occurred since the last frame was
	 * displayed is applied and the culling system updated. Any object motions
	 * that has occurred is also applied and the culling system
	 * updated. Renderer.ClearBuffers() is called, the scene and framerate are
	 * drawn, and the back-buffer displayed with Renderer.DisplayBackBuffer().
	 */
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

		if ( m_kShaderParamsWindow == null && m_spkEffect != null)
		{
			m_kShaderParamsWindow = new ApplicationGUI();
			m_kShaderParamsWindow.setParent(this);
			m_kShaderParamsWindow.AddUserVariables(m_spkEffect.GetCProgram(0));
			m_kShaderParamsWindow.Display();
			m_kShaderParamsWindow.setParent(this);
		}
	}

	@Override
	public void dispose(GLAutoDrawable arg0)
	{
        ((OpenGLRenderer)m_pkRenderer).SetDrawable( arg0 );
        m_pkRenderer.ReleaseAllResources( m_spkScene );
	}

	/**
	 * Gooch.init is called only once when the GLCanvas is initialized. It
	 * initializes the renderer object, sets up the camera model, creates the
	 * scene, and initializes the culling object with the camera and scene
	 * objects.
	 */
	@Override
	public void init(GLAutoDrawable arg0) {
		((OpenGLRenderer)m_pkRenderer).SetDrawable( arg0 );
		m_pkRenderer.InitializeState();
		super.OnInitialize();

		m_spkCamera.SetFrustum(-0.55f,0.55f,-0.4125f,0.4125f,1.0f,100.0f);
		Vector3f kCLoc = new Vector3f(0.0f,0.0f,-8.0f);
		Vector3f kCDir = new Vector3f(0.0f,0.0f,1.0f);
		Vector3f kCUp = new Vector3f(0.0f,1.0f,0.0f);
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
		InitializeObjectMotion(m_spkScene);
	}
	/**
	 * Gooch.keyPressed() processes key-input from the user. A shader-editor
	 * GUI can be launched by pressing 'l'. The scene-graph is streamed to
	 * disk by pressing the 's' key.
	 */
	@Override
	public void keyTyped(KeyEvent e) {
		char ucKey = e.getKeyChar();
		super.keyPressed(e);
		switch (ucKey)
		{
		case 'l':
		case 'L':
			ApplicationGUI kShaderParamsWindow = new ApplicationGUI();
			kShaderParamsWindow.setParent(this);
			kShaderParamsWindow.AddUserVariables(m_spkEffect.GetCProgram(0));
			kShaderParamsWindow.Display();
			return;
		case 's':
		case 'S':
			TestStreaming(m_spkScene,"Gooch.wmof");
			return;
		}
		return;
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

	/**
	 * Gooch.CreateScene() creates the scene graph. The root node is
	 * m_spkScene. It contains a single TriMesh object, the torus. The TriMesh
	 * object is created with a set of rendering Attributes with three
	 * channels for point data (x,y,z); three channels for normal data
	 * (x,y,z).  A GoochEffect is created and attached to the torus, and 
	 * a WireframeBehind effect is attached, which causes two-pass rendering.
	 */
	private void CreateScene ()
	{
		m_spkScene = new Node();
		Attributes kAttr = new Attributes();
		kAttr.SetPChannels(3);
		kAttr.SetNChannels(3);
		StandardMesh kSM = new StandardMesh(kAttr);
		m_pkMesh = kSM.Torus(50,50,2.0f,1.0f);

		m_spkEffect = new GoochEffect();

		final int iPassQuantity = m_spkEffect.GetPassQuantity();
		for (int iPass = 0; iPass < iPassQuantity; iPass++)
		{
			m_spkEffect.LoadPrograms(m_pkRenderer, iPass,m_pkRenderer.GetMaxColors(),m_pkRenderer.GetMaxTCoords(),
					m_pkRenderer.GetMaxVShaderImages(),m_pkRenderer.GetMaxPShaderImages());
		}
		m_pkMesh.AttachEffect(m_spkEffect);

		// Do a second effect, a wire-frame pass
		Effect pkEffect = new WireframeBehindEffect();
		m_pkMesh.AttachEffect(pkEffect);


		m_spkScene.AttachChild(m_pkMesh);
	}

}
