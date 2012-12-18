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

import javax.media.opengl.GLAutoDrawable;
import javax.media.opengl.GLEventListener;

import WildMagic.LibApplications.OpenGLApplication.ApplicationGUI;
import WildMagic.LibFoundation.Mathematics.ColorRGB;
import WildMagic.LibFoundation.Mathematics.ColorRGBA;
import WildMagic.LibFoundation.Mathematics.Matrix3f;
import WildMagic.LibFoundation.Mathematics.Vector3f;
import WildMagic.LibGraphics.Effects.IridescenceEffect;
import WildMagic.LibGraphics.Effects.VertexColor3Effect;
import WildMagic.LibGraphics.Rendering.CullState;
import WildMagic.LibGraphics.Rendering.MaterialState;
import WildMagic.LibGraphics.Rendering.WireframeState;
import WildMagic.LibGraphics.SceneGraph.Attributes;
import WildMagic.LibGraphics.SceneGraph.IndexBuffer;
import WildMagic.LibGraphics.SceneGraph.Node;
import WildMagic.LibGraphics.SceneGraph.StandardMesh;
import WildMagic.LibGraphics.SceneGraph.TriMesh;
import WildMagic.LibGraphics.SceneGraph.VertexBuffer;
import WildMagic.LibRenderers.OpenGLRenderer.OpenGLRenderer;

import com.jogamp.opengl.util.Animator;


public class Iridescence extends DemoBase implements GLEventListener, KeyListener {

	private static final long serialVersionUID = 7201911098737257202L;
	
	/**
	 * Iridescence.main creates the Iridescence object and window frame to contain the GLCanvas. An Animator object is
	 * created with the GLCanvas as an argument. The Animator provides the same function as the glutMainLoop() function
	 * call commonly used in OpenGL applications.
	 */
	public static void main(String[] args) {
		Iridescence kWorld = new Iridescence();
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

	private WireframeState m_spkWireframe;
	private CullState m_kCull;

	private IridescenceEffect m_spkEffect;

	/** Window with the shader parameter interface: */
	private ApplicationGUI m_kShaderParamsWindow = null;

	private boolean m_bStereo = false;

	private boolean m_bLeft = true;

	private boolean m_bRight = true;


	/**
	 * The constructor initializes the OpenGLRender, and sets up the GLEvent, KeyEvent, and Mouse listeners. The last
	 * three statements initialize the ImageCatalog, VertexProgramCatalog, and PixelProgramCatalog. The three catalogs
	 * enable sharing of texture images, vertex program, and pixel programs in a program. The catalogs keep track of
	 * images and programs that are currently loaded. If multiple effects use the same images and shader programs, the
	 * loaded version is re-used. This saves graphics hardware memory as well as time for re-loading large textures.
	 * 
	 */
	public Iridescence() {
		super("Iridescence");
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

		/*
		if (m_kShaderParamsWindow == null) {
			m_kShaderParamsWindow = new ApplicationGUI();
			m_kShaderParamsWindow.setParent(this);
			m_kShaderParamsWindow.AddUserVariables(m_spkEffect.GetCProgram(0));
			m_kShaderParamsWindow.Display();
			m_kShaderParamsWindow.setParent(this);
		}*/
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

		CreateScene();

		// initial update of objects
		m_spkScene.UpdateGS();
		m_spkScene.UpdateRS();

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
        m_kCull.CullFace = CullState.CullMode.CT_FRONT;
        m_kCull.FrontFace = CullState.FrontMode.FT_CCW;
        m_spkScene.AttachGlobalState(m_kCull);

		Attributes kAttr = new Attributes();
		kAttr.SetPChannels(3);
		//kAttr.SetNChannels(3);
		//kAttr.SetTChannels(0, 2);
        kAttr.SetCChannels(0,3);
        kAttr.SetTChannels(0,3);
        kAttr.SetTChannels(1,3);
		StandardMesh kSM = new StandardMesh(kAttr);
		//TriMesh pkMesh = kSM.Torus(200, 200, 2.0f, 1.0f);
		TriMesh pkMesh = Box();

		pkMesh.Local.SetMatrix(new Matrix3f(new Vector3f(0f, 0f, 1f), new Vector3f(0.707f, 0.707f, 0f), new Vector3f(
				-0.707f, 0.707f, 0f), false));
		m_spkScene.AttachChild(pkMesh);

		VertexColor3Effect effect = new VertexColor3Effect();
		pkMesh.AttachEffect(effect);
		
		for ( int i = 0; i < pkMesh.VBuffer.GetVertexQuantity(); i++ )
		{
			//pkMesh.VBuffer.SetColor3(0, i, new ColorRGB( 1, 0, 0 ) );
		}

		/*
		m_spkEffect = new IridescenceEffect("Leaf", "Gradient");
		m_spkEffect.SetInterpolateFactor(0.5f);
		int iPassQuantity = m_spkEffect.GetPassQuantity();
		for (int iPass = 0; iPass < iPassQuantity; iPass++) {
			m_spkEffect.LoadPrograms(m_pkRenderer, pkMesh, iPass, m_pkRenderer.GetMaxColors(), m_pkRenderer.GetMaxTCoords(),
					m_pkRenderer.GetMaxVShaderImages(), m_pkRenderer.GetMaxPShaderImages());
		}
		pkMesh.AttachEffect(m_spkEffect);*/
	}
	

    private TriMesh Box ()
    {
        Attributes kAttr = new Attributes();
        kAttr.SetPChannels(3);
        kAttr.SetCChannels(0,3);
        kAttr.SetTChannels(0,3);
        kAttr.SetTChannels(1,3);

  
  
        float fMaxX = 249.02344f;
        float fMaxY = 249.02344f;
        float fMaxZ = 320.6056f;

        float m_fMax = fMaxX;
        if (fMaxY > m_fMax) {
            m_fMax = fMaxY;
        }
        if (fMaxZ > m_fMax) {
            m_fMax = fMaxZ;
        }
        float m_fX = 0.77672833f;
        float m_fY = 0.77672833f;
        float m_fZ = 1.0f;

        int iVQuantity = 24;
        int iTQuantity = 12;
        VertexBuffer pkVB = new VertexBuffer(kAttr,iVQuantity);
        IndexBuffer pkIB = new IndexBuffer(3*iTQuantity);

        // generate connectivity (outside view)
        int i = 0;
        int[] aiIndex = pkIB.GetData();
        
        System.err.println( m_fX + " " + m_fY + " " + m_fZ );

        // generate geometry
        // front
        pkVB.SetPosition3(0,0,0,0);
        pkVB.SetPosition3(1,m_fX,0,0);
        pkVB.SetPosition3(2,m_fX,m_fY,0);
        pkVB.SetPosition3(3,0,m_fY,0);
        pkVB.SetColor3(0,0,0,0,0);
        pkVB.SetColor3(0,1,1,0,0);
        pkVB.SetColor3(0,2,1,1,0);
        pkVB.SetColor3(0,3,0,1,0);
        aiIndex[i++] = 0;  aiIndex[i++] = 2;  aiIndex[i++] = 1;
        aiIndex[i++] = 0;  aiIndex[i++] = 3;  aiIndex[i++] = 2;

        // back
        pkVB.SetPosition3(4,0,0,m_fZ);
        pkVB.SetPosition3(5,m_fX,0,m_fZ);
        pkVB.SetPosition3(6,m_fX,m_fY,m_fZ);
        pkVB.SetPosition3(7,0,m_fY,m_fZ);
        pkVB.SetColor3(0,4,0,0,1);
        pkVB.SetColor3(0,5,1,0,1);
        pkVB.SetColor3(0,6,1,1,1);
        pkVB.SetColor3(0,7,0,1,1);
        aiIndex[i++] = 4;  aiIndex[i++] = 5;  aiIndex[i++] = 6;
        aiIndex[i++] = 4;  aiIndex[i++] = 6;  aiIndex[i++] = 7;

        // top
        pkVB.SetPosition3(8,0,m_fY,0);
        pkVB.SetPosition3(9,m_fX,m_fY,0);
        pkVB.SetPosition3(10,m_fX,m_fY,m_fZ);
        pkVB.SetPosition3(11,0,m_fY,m_fZ);
        pkVB.SetColor3(0,8,0,1,0);
        pkVB.SetColor3(0,9,1,1,0);
        pkVB.SetColor3(0,10,1,1,1);
        pkVB.SetColor3(0,11,0,1,1);
        aiIndex[i++] = 8;  aiIndex[i++] = 10;  aiIndex[i++] = 9;
        aiIndex[i++] = 8;  aiIndex[i++] = 11;  aiIndex[i++] = 10;

        // bottom
        pkVB.SetPosition3(12,0,0,0);
        pkVB.SetPosition3(13,m_fX,0,0);
        pkVB.SetPosition3(14,m_fX,0,m_fZ);
        pkVB.SetPosition3(15,0,0,m_fZ);
        pkVB.SetColor3(0,12,0,0,0);
        pkVB.SetColor3(0,13,1,0,0);
        pkVB.SetColor3(0,14,1,0,1);
        pkVB.SetColor3(0,15,0,0,1);
        aiIndex[i++] = 12;  aiIndex[i++] = 13;  aiIndex[i++] = 14;
        aiIndex[i++] = 12;  aiIndex[i++] = 14;  aiIndex[i++] = 15;

        // right
        pkVB.SetPosition3(16,m_fX,0,0);
        pkVB.SetPosition3(17,m_fX,m_fY,0);
        pkVB.SetPosition3(18,m_fX,m_fY,m_fZ);
        pkVB.SetPosition3(19,m_fX,0,m_fZ);
        pkVB.SetColor3(0,16,1,0,0);
        pkVB.SetColor3(0,17,1,1,0);
        pkVB.SetColor3(0,18,1,1,1);
        pkVB.SetColor3(0,19,1,0,1);
        aiIndex[i++] = 16;  aiIndex[i++] = 17;  aiIndex[i++] = 18;
        aiIndex[i++] = 16;  aiIndex[i++] = 18;  aiIndex[i++] = 19;

        // left
        pkVB.SetPosition3(20,0,0,0);
        pkVB.SetPosition3(21,0,m_fY,0);
        pkVB.SetPosition3(22,0,m_fY,m_fZ);
        pkVB.SetPosition3(23,0,0,m_fZ);
        pkVB.SetColor3(0,20,0,0,0);
        pkVB.SetColor3(0,21,0,1,0);
        pkVB.SetColor3(0,22,0,1,1);
        pkVB.SetColor3(0,23,0,0,1);
        aiIndex[i++] = 20;  aiIndex[i++] = 22;  aiIndex[i++] = 21;
        aiIndex[i++] = 20;  aiIndex[i++] = 23;  aiIndex[i++] = 22;

        if (kAttr.GetMaxTCoords() > 0)
        {
            for (int iUnit = 0; iUnit < kAttr.GetMaxTCoords(); iUnit++)
            {
                if (kAttr.HasTCoord(iUnit))
                {
                    pkVB.SetTCoord3(iUnit,0,0,0,0);
                    pkVB.SetTCoord3(iUnit,1,1,0,0);
                    pkVB.SetTCoord3(iUnit,2,1,1,0);
                    pkVB.SetTCoord3(iUnit,3,0,1,0);

                    pkVB.SetTCoord3(iUnit,4,0,0,1);
                    pkVB.SetTCoord3(iUnit,5,1,0,1);
                    pkVB.SetTCoord3(iUnit,6,1,1,1);
                    pkVB.SetTCoord3(iUnit,7,0,1,1);

                    pkVB.SetTCoord3(iUnit,8,0,1,0);
                    pkVB.SetTCoord3(iUnit,9,1,1,0);
                    pkVB.SetTCoord3(iUnit,10,1,1,1);
                    pkVB.SetTCoord3(iUnit,11,0,1,1);

                    pkVB.SetTCoord3(iUnit,12,0,0,0);
                    pkVB.SetTCoord3(iUnit,13,1,0,0);
                    pkVB.SetTCoord3(iUnit,14,1,0,1);
                    pkVB.SetTCoord3(iUnit,15,0,0,1);

                    pkVB.SetTCoord3(iUnit,16,1,0,0);
                    pkVB.SetTCoord3(iUnit,17,1,1,0);
                    pkVB.SetTCoord3(iUnit,18,1,1,1);
                    pkVB.SetTCoord3(iUnit,19,1,0,1);

                    pkVB.SetTCoord3(iUnit,20,0,0,0);
                    pkVB.SetTCoord3(iUnit,21,0,1,0);
                    pkVB.SetTCoord3(iUnit,22,0,1,1);
                    pkVB.SetTCoord3(iUnit,23,0,0,1);
                }
            }
        }
        TriMesh kMesh = new TriMesh(pkVB,pkIB);
        kMesh.VBuffer.SetName("BOX");
        MaterialState kMaterial = new MaterialState();
        kMaterial.Emissive = new ColorRGB(ColorRGB.BLACK);
        kMaterial.Ambient = new ColorRGB(0.1f,0.1f,0.1f);
        kMaterial.Diffuse = new ColorRGB(1f,1f,1f);
        kMaterial.Specular = new ColorRGB(1f,1f,1f);
        kMaterial.Shininess = 128f;
        kMesh.AttachGlobalState(kMaterial);
        kMesh.UpdateMS(true);
        return kMesh;
    }
    

}
