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
import WildMagic.LibFoundation.Mathematics.Vector3f;
import WildMagic.LibGraphics.Effects.TextureEffect;
import WildMagic.LibGraphics.Rendering.AlphaState;
import WildMagic.LibGraphics.Rendering.Camera;
import WildMagic.LibGraphics.Rendering.Light;
import WildMagic.LibGraphics.Rendering.MaterialState;
import WildMagic.LibGraphics.Rendering.Texture;
import WildMagic.LibGraphics.Rendering.WireframeState;
import WildMagic.LibGraphics.SceneGraph.Attributes;
import WildMagic.LibGraphics.SceneGraph.CameraNode;
import WildMagic.LibGraphics.SceneGraph.IndexBuffer;
import WildMagic.LibGraphics.SceneGraph.LightNode;
import WildMagic.LibGraphics.SceneGraph.Node;
import WildMagic.LibGraphics.SceneGraph.StandardMesh;
import WildMagic.LibGraphics.SceneGraph.TriMesh;
import WildMagic.LibGraphics.SceneGraph.VertexBuffer;
import WildMagic.LibRenderers.OpenGLRenderer.OpenGLRenderer;

import com.jogamp.opengl.util.Animator;

public class CameraAndLightNodes extends DemoBase
implements GLEventListener, KeyListener
{

	private static final long serialVersionUID = -1413078577908421874L;

	/**
	 * @param args
	 */
	public static void main(String[] args) {
		CameraAndLightNodes kWorld = new CameraAndLightNodes();  
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


	private WireframeState m_spkWireframeState;

	private CameraNode m_spkCNode;

	private Camera m_spkScreenCamera;

	private TriMesh m_spkSky;

	private Light m_spkAdjustableLight0, m_spkAdjustableLight1;

	public CameraAndLightNodes()
	{
		super("CameraAndLightNodes",640,480);
	}

	@Override
	public void display(GLAutoDrawable arg0) {
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
	}

	@Override
	public void dispose(GLAutoDrawable arg0) 
	{
        ((OpenGLRenderer)m_pkRenderer).SetDrawable( arg0 );
        m_pkRenderer.ReleaseAllResources( m_spkScene );
	}

	@Override
	public void init(GLAutoDrawable arg0) {
		((OpenGLRenderer)m_pkRenderer).SetDrawable( arg0 );
		m_pkRenderer.InitializeState();
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
		Vector3f kCRight = Vector3f.cross( kCDir, kCUp );
		m_spkCamera.SetFrame(kCLoc,kCDir,kCUp,kCRight);

		CreateScene();

		// initial update of objects
		m_spkScene.UpdateGS();
		m_spkScene.UpdateRS();

		// initial culling of scene
		m_kCuller.SetCamera(m_spkCamera);
		m_kCuller.ComputeVisibleSet(m_spkScene);

		InitializeCameraMotion(0.01f,0.001f);
	}

	@Override
	public void keyTyped(KeyEvent e) {
		char ucKey = e.getKeyChar();

		super.keyPressed(e);

		switch (ucKey)
		{
		case 'w':
		case 'W':
			m_spkWireframeState.Enabled = true;
			m_spkWireframeState.Fill = ( m_spkWireframeState.Fill == WireframeState.FillMode.FM_FILL ) ? 
					WireframeState.FillMode.FM_LINE : ( m_spkWireframeState.Fill == WireframeState.FillMode.FM_LINE ) ?
							WireframeState.FillMode.FM_POINT : 	WireframeState.FillMode.FM_FILL;
			m_spkScene.UpdateGS();
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
		Texture pkTexture = pkEffect.GetTexture(0,0);
		pkTexture.SetFilterType(Texture.FilterType.LINEAR_LINEAR);
		pkTexture.SetWrapType(0,Texture.WrapType.REPEAT);
		pkTexture.SetWrapType(1,Texture.WrapType.REPEAT);
		pkMesh.AttachEffect(pkEffect);

		AlphaState pkAState = pkEffect.GetBlending(0);
		pkAState.SrcBlend = AlphaState.SrcBlendMode.SBF_DST_COLOR;
		pkAState.DstBlend = AlphaState.DstBlendMode.DBF_ZERO;

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

	@Override
	protected void LookDown ()
	{
		// disabled
	}

	@Override
	protected void LookUp ()
	{
		// disabled
	}

	@Override
	protected void MoveBackward ()
	{
		Vector3f kLocation = m_spkCNode.Local.GetTranslate();
		Vector3f kDirection = new Vector3f();
		m_spkCNode.Local.GetRotate().getColumn(0, kDirection);
		kDirection.scale(m_fTrnSpeed);
		kLocation.sub( kDirection );
		m_spkCNode.Local.SetTranslate(kLocation);
		m_spkCNode.UpdateGS();
		m_kCuller.ComputeVisibleSet(m_spkScene);
		kDirection = null;
	}
	@Override
	protected void MoveDown ()
	{
		// disabled
	}
	@Override
	protected void MoveForward ()
	{
		Vector3f kLocation = m_spkCNode.Local.GetTranslate();
		Vector3f kDirection = new Vector3f();
		m_spkCNode.Local.GetRotate().getColumn(0, kDirection);
		kDirection.scale(m_fTrnSpeed);
		kLocation.add( kDirection );
		m_spkCNode.Local.SetTranslate(kLocation);
		m_spkCNode.UpdateGS();
		m_kCuller.ComputeVisibleSet(m_spkScene);
		kDirection = null;
	}
	@Override
	protected void MoveUp ()
	{
		// disabled
	}
	@Override
	protected void TurnLeft ()
	{
		Vector3f kUp = new Vector3f();
		m_spkCNode.Local.GetRotate().getColumn(1, kUp);
		Matrix3f kNewRotate = new Matrix3f(kUp,m_fRotSpeed);
		kNewRotate.mult( m_spkCNode.Local.GetRotate() );
		m_spkCNode.Local.SetRotate(kNewRotate);
		m_spkCNode.UpdateGS();
		m_kCuller.ComputeVisibleSet(m_spkScene);
		kUp = null;
	}

	@Override
	protected void TurnRight ()
	{
		Vector3f kUp = new Vector3f();
		m_spkCNode.Local.GetRotate().getColumn(1, kUp);
		Matrix3f kNewRotate = new Matrix3f(kUp,-m_fRotSpeed);
		kNewRotate.mult( m_spkCNode.Local.GetRotate() );
		m_spkCNode.Local.SetRotate(kNewRotate);
		m_spkCNode.UpdateGS();
		m_kCuller.ComputeVisibleSet(m_spkScene);
		kUp = null;
	}

}
