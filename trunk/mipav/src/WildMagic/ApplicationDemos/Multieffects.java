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
import WildMagic.LibFoundation.Mathematics.Plane3f;
import WildMagic.LibFoundation.Mathematics.Vector3f;
import WildMagic.LibGraphics.Effects.TextureEffect;
import WildMagic.LibGraphics.Rendering.AlphaState;
import WildMagic.LibGraphics.Rendering.CullState;
import WildMagic.LibGraphics.Rendering.WireframeState;
import WildMagic.LibGraphics.SceneGraph.Attributes;
import WildMagic.LibGraphics.SceneGraph.Node;
import WildMagic.LibGraphics.SceneGraph.StandardMesh;
import WildMagic.LibGraphics.SceneGraph.TriMesh;
import WildMagic.LibRenderers.OpenGLRenderer.OpenGLRenderer;

import com.jogamp.opengl.util.Animator;


public class Multieffects extends DemoBase
implements GLEventListener, KeyListener
{

	private static final long serialVersionUID = -5094705738989521135L;

	/**
	 * @param args
	 */
	public static void main(String[] args) {
		Multieffects kWorld = new Multieffects();
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

	private TextureEffect m_spkEffect;

	private int m_iActive;

	private Plane3f m_kPlane = new Plane3f( new Vector3f( .5f, -1, -1 ), new Vector3f( .5f, 1, 1), new Vector3f( .5f, -1, 1) );

	public Multieffects()
	{
		super( "Multieffects" );
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

		m_pkRenderer.EnableUserClipPlane(0, m_kPlane);
		m_pkRenderer.ClearBuffers();
		if (m_pkRenderer.BeginScene())
		{          
			m_pkRenderer.DrawScene(m_kCuller.GetVisibleSet());
			DrawFrameRate(arg0, 8,GetHeight()-8,ColorRGBA.WHITE);
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
		m_spkCamera.SetFrustum(60.0f,1.0f,0.1f,100.0f);
		Vector3f kCLoc = new Vector3f(0.0f,0.0f,2.0f);
		Vector3f kCDir = Vector3f.UNIT_Z_NEG;
		Vector3f kCUp = Vector3f.UNIT_Y;
		Vector3f kCRight = Vector3f.UNIT_X;
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

		super.keyPressed(e);

		AlphaState pkAState;

		if (ucKey == 'w' || ucKey == 'W')
		{
			m_spkWireframe.Enabled = !m_spkWireframe.Enabled;
			return;
		}

		switch (ucKey)
		{
		case 'n':
		case 'N':
			if (m_iActive == 0)
			{
				// switch to hard additive
				pkAState = m_spkEffect.GetBlending(0);
				pkAState.SrcBlend = AlphaState.SrcBlendMode.SBF_ONE;
				pkAState.DstBlend = AlphaState.DstBlendMode.DBF_ONE;
				m_iActive = 1;
			}
			else if (m_iActive == 1)
			{
				// soft additive
				pkAState = m_spkEffect.GetBlending(0);
				pkAState.SrcBlend = AlphaState.SrcBlendMode.SBF_ONE_MINUS_DST_COLOR;
				pkAState.DstBlend = AlphaState.DstBlendMode.DBF_ONE;
				m_iActive = 2;
			}
			else
			{
				// multiplicative
				pkAState = m_spkEffect.GetBlending(0);
				pkAState.SrcBlend = AlphaState.SrcBlendMode.SBF_DST_COLOR;
				pkAState.DstBlend = AlphaState.DstBlendMode.DBF_ZERO;
				m_iActive = 0;
			}
			return;
		case 's':
		case 'S':
			TestStreaming(m_spkScene,"Multieffect.wmof");
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

	private void CreateScene ()
	{
		m_spkScene = new Node();
		m_spkWireframe = new WireframeState();
		m_spkScene.AttachGlobalState(m_spkWireframe);
		m_spkScene.AttachGlobalState(new CullState());

		Attributes kAttr = new Attributes();
		kAttr.SetPChannels(3);
		kAttr.SetTChannels(0,2);
		kAttr.SetTChannels(1,2);
		StandardMesh kSM = new StandardMesh(kAttr);
		TriMesh pkPlane = kSM.Rectangle(2,2,1.0f,1.0f);

		// The primary texture is the first effect to be drawn.
		pkPlane.AttachEffect(new TextureEffect("Horizontal"));

		// The secondary texture is drawn as a second effect.
		m_spkEffect = new TextureEffect("Magician");
		pkPlane.AttachEffect(m_spkEffect);

		// Set the blending mode (multiplicative) for the secondary effect when
		// it is applied to the primary effect.
		AlphaState pkAState = m_spkEffect.GetBlending(0);
		pkAState.SrcBlend = AlphaState.SrcBlendMode.SBF_DST_COLOR;
		pkAState.DstBlend = AlphaState.DstBlendMode.DBF_ZERO;
		m_spkScene.AttachChild(pkPlane);

		m_iActive = 0;
		m_pkRenderer.EnableUserClipPlane(0, m_kPlane);
	}
}
