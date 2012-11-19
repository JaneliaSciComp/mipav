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

import WildMagic.LibFoundation.Mathematics.ColorRGBA;
import WildMagic.LibFoundation.Mathematics.Matrix3f;
import WildMagic.LibFoundation.Mathematics.Vector3f;
import WildMagic.LibGraphics.Effects.SimpleBumpMapEffect;
import WildMagic.LibGraphics.Effects.TextureEffect;
import WildMagic.LibGraphics.Rendering.Texture;
import WildMagic.LibGraphics.SceneGraph.Attributes;
import WildMagic.LibGraphics.SceneGraph.IndexBuffer;
import WildMagic.LibGraphics.SceneGraph.Node;
import WildMagic.LibGraphics.SceneGraph.StandardMesh;
import WildMagic.LibGraphics.SceneGraph.Transformation;
import WildMagic.LibGraphics.SceneGraph.TriMesh;
import WildMagic.LibGraphics.SceneGraph.VertexBuffer;
import WildMagic.LibRenderers.OpenGLRenderer.OpenGLRenderer;

import com.jogamp.opengl.util.Animator;

public class BumpMaps extends DemoBase
implements GLEventListener, KeyListener
{
	private static final long serialVersionUID = -7105537604714708379L;

	/**
	 * @param args
	 */
	public static void main(String[] args) {
		BumpMaps kWorld = new BumpMaps();      
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


	private boolean m_bUseTorus = true;

	private boolean m_bUseBumpMap = true;

	private boolean m_bUpdateBumpMap = false;

	public BumpMaps()
	{
		super("BumpMaps");
	}

	@Override
	public void display(GLAutoDrawable arg0) {
        ((OpenGLRenderer)m_pkRenderer).SetDrawable( arg0 );
		if ( m_bUpdateBumpMap )
		{
			m_bUpdateBumpMap = false;
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
		}
		
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
		Vector3f kCRight = Vector3f.cross( kCDir, kCUp );
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
	}

	@Override
	public void keyTyped(KeyEvent e) {
		char ucKey = e.getKeyChar();

		super.keyPressed(e);

		switch (ucKey)
		{
		case 'b':
		case 'B':
			m_bUseBumpMap = !m_bUseBumpMap;
			m_bUpdateBumpMap = true;
			return;
		case 's':
		case 'S':
			TestStreaming(m_spkScene,"BumpMaps.wmof");
			return;
		}
	}
	
	@Override
	public void reshape(GLAutoDrawable arg0, int iX, int iY, int iWidth, int iHeight) {
        ((OpenGLRenderer)m_pkRenderer).SetDrawable( arg0 );
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
			kLightDirection.normalize();
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


		if (m_bUseBumpMap)
		{
			SimpleBumpMapEffect pkEffect = new SimpleBumpMapEffect("Bricks",
					"BricksNormal",Vector3f.UNIT_Z);
			pkEffect.ComputeLightVectors(pkMesh);
			pkMesh.AttachEffect(pkEffect);
	        m_pkRenderer.LoadResources( pkMesh );

			Texture pkBricks = pkEffect.GetTexture(0,0);
			pkBricks.SetFilterType(Texture.FilterType.LINEAR_LINEAR);
			pkBricks.SetWrapType(0,Texture.WrapType.REPEAT);
			pkBricks.SetWrapType(1,Texture.WrapType.REPEAT);

			Texture pkNormals = pkEffect.GetTexture(0,1);
			pkNormals.SetFilterType(Texture.FilterType.LINEAR_LINEAR);
			pkNormals.SetWrapType(0,Texture.WrapType.REPEAT);
			pkNormals.SetWrapType(1,Texture.WrapType.REPEAT);

		}
		else
		{
			TextureEffect pkEffect = new TextureEffect("Bricks");
			pkMesh.AttachEffect(pkEffect);
	        m_pkRenderer.LoadResources( pkMesh );

			Texture pkBricks = pkEffect.GetTexture(0,0);
			pkBricks.SetFilterType(Texture.FilterType.LINEAR_LINEAR);
			pkBricks.SetWrapType(0,Texture.WrapType.REPEAT);
			pkBricks.SetWrapType(1,Texture.WrapType.REPEAT);

		}
		pkMesh.UpdateRS();
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

}
