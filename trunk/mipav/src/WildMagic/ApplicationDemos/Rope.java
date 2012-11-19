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
import java.util.Calendar;

import javax.media.opengl.GLAutoDrawable;
import javax.media.opengl.GLEventListener;

import WildMagic.LibFoundation.Curves.BSplineCurve3f;
import WildMagic.LibFoundation.Mathematics.ColorRGBA;
import WildMagic.LibFoundation.Mathematics.Vector2f;
import WildMagic.LibFoundation.Mathematics.Vector3f;
import WildMagic.LibGraphics.Effects.RipplingOceanEffect;
import WildMagic.LibGraphics.Effects.TextureEffect;
import WildMagic.LibGraphics.Rendering.GraphicsImage;
import WildMagic.LibGraphics.Rendering.Texture;
import WildMagic.LibGraphics.Rendering.WireframeState;
import WildMagic.LibGraphics.SceneGraph.Attributes;
import WildMagic.LibGraphics.SceneGraph.Node;
import WildMagic.LibGraphics.Surfaces.TubeSurface;
import WildMagic.LibRenderers.OpenGLRenderer.OpenGLRenderer;

import com.jogamp.opengl.util.Animator;


public class Rope extends DemoBase
implements GLEventListener, KeyListener
{

	private static final long serialVersionUID = 3308024455457519161L;
	/**
	 * @param args
	 */
	public static void main(String[] args) {
		Rope kWorld = new Rope();        
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

	private static float Radial (float v) { 
		return 0.025f; 
	}

	private Node m_spkTrnNode;

	private WireframeState m_spkWireframe;

	private RipplingOceanEffect m_spkEffect;

	private GraphicsImage m_spkNormalMap;

	// The masses are located at the control points of a spline curve.  The
	// control points are connected by a mass-spring system.
	private BSplineCurve3f m_pkSpline;

	private int m_iNumCtrlPoints = 0;



	private Vector3f[] m_akCtrlPoint;

	private TubeSurface m_spkRope;

	// controlled frame rate
	float m_fLastIdle;

	// Time information.
	private boolean m_bStopped;
	private float m_fStopTime;
	// Wave information.
	private float m_fWaveSpeedAmplitude;
	private float m_fWaveHeightAmplitude;
	private float m_fBumpSpeedAmplitude;


	private float[] m_afWaveSpeed = new float[4];

	private float[] m_afWaveHeight = new float[4];
	private float[] m_afBumpSpeed = new float[4];

	private static boolean gs_bInitializedTime = false;

	private static long gs_lInitialSec;

	private static long gs_lInitialUSec;
	public Rope()
	{
		super( "Rope" );
		m_pkSpline = null;
		m_fLastIdle = 0.0f;
	}

	//----------------------------------------------------------------------------
	public void CreateRope ()
	{
		// create quadratic spline using particles as control points
		// int iNumCtrlPoints = m_pkModule.GetNumParticles();
		// Vector3f[] akCtrlPoint = m_pkModule.Positions();

		m_iNumCtrlPoints = 8;
		m_akCtrlPoint = new Vector3f[8];
		m_akCtrlPoint[0] = new Vector3f(0, 0, 1);
		m_akCtrlPoint[1] = new Vector3f(0.14285679f, 0.0f, 0.99625003f);
		m_akCtrlPoint[2] = new Vector3f(0.28571439f, 0.0f, 0.99624997f);
		m_akCtrlPoint[3] = new Vector3f(0.42857146f, 0.0f, 0.99624997f);
		m_akCtrlPoint[4] = new Vector3f(0.57142860f, 0.0f, 0.99624997f);
		m_akCtrlPoint[5] = new Vector3f(0.71428573f, 0.0f, 0.99624997f);
		m_akCtrlPoint[6] = new Vector3f(0.85714328f, 0.0f, 0.99624997f);
		m_akCtrlPoint[7] = new Vector3f(1.0f, 0.0f, 1.0f);
		//         akCtrlPoint[0] = new Vector3f(0, 0, 0);
		//         akCtrlPoint[1] = new Vector3f(0.1f, 0.1f, 0.1f);
		//         akCtrlPoint[2] = new Vector3f(0.2f, 0.2f, 0.2f);
		//         akCtrlPoint[3] = new Vector3f(0.3f, 0.3f, 0.3f);
		//         akCtrlPoint[4] = new Vector3f(0.4f, 0.4f, 0.4f);
		//         akCtrlPoint[5] = new Vector3f(0.5f, 0.5f, 0.5f);
		//         akCtrlPoint[6] = new Vector3f(1.0f, 1.0f, 1.0f);


		int iDegree = 2;
		m_pkSpline = new BSplineCurve3f(m_iNumCtrlPoints,m_akCtrlPoint,iDegree,
				false,true);

		// generate a tube surface whose medial axis is the spline
		Attributes kAttr = new Attributes();
		kAttr.SetPChannels(3);
		kAttr.SetTChannels(0,2);
		Vector2f kUVMin = new Vector2f(0.0f,0.0f);
		Vector2f kUVMax = new Vector2f(1.0f,1.0f);
		m_spkRope = new TubeSurface(m_pkSpline,Rope.Radial(0),false,Vector3f.UNIT_Z,
				64,8,kAttr,false,false,kUVMin,kUVMax);

		// attach a texture for the rope
		TextureEffect pkEffect = new TextureEffect("Rope");
		Texture pkTexture = pkEffect.GetTexture(0,0);
		pkTexture.SetFilterType(Texture.FilterType.LINEAR_LINEAR);
		pkTexture.SetWrapType(0,Texture.WrapType.REPEAT);
		pkTexture.SetWrapType(1,Texture.WrapType.REPEAT);
		m_spkRope.AttachEffect(pkEffect);

		m_spkTrnNode.AttachChild(m_spkRope);
	}
	@Override
	public void display(GLAutoDrawable arg0) {
		MeasureTime();

		MoveCamera();
		if (MoveObject())
		{
			m_spkScene.UpdateGS( 0.0f, true );
		}
		// float fTime = (m_bStopped ? m_fStopTime : GetTime());
		// m_spkEffect.SetTime(fTime);

		DoPhysical();
		m_kCuller.ComputeVisibleSet(m_spkScene);

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

		CreateScene();

		m_spkScene.UpdateGS(0.0f, true);;
		Vector3f transResult = new Vector3f(m_spkScene.WorldBound.GetCenter());
		transResult.neg();
		m_spkTrnNode.Local.SetTranslate(transResult);

		m_spkCamera.SetFrustum(-0.55f,0.55f,-0.4125f,0.4125f,1.0f,100.0f);
		Vector3f kCDir = new Vector3f(0.0f,-1.0f,0.0f);
		Vector3f kCUp = new Vector3f(0.0f,0.0f,1.0f);
		Vector3f kCRight = Vector3f.cross( kCDir, kCUp );
		Vector3f kTemp1 = Vector3f.scale( 0.5f, kCUp );
		Vector3f kTemp2 = Vector3f.scale( -3.0f*m_spkScene.WorldBound.GetRadius(), kCDir );
		Vector3f kCLoc = Vector3f.sub( kTemp2, kTemp1 );
		m_spkCamera.SetFrame(kCLoc,kCDir,kCUp,kCRight);

		// initial update of objects
		m_spkScene.UpdateGS(0.0f, true);
		m_spkScene.UpdateRS();

		// initial culling of scene
		m_kCuller.SetCamera(m_spkCamera);
		m_kCuller.ComputeVisibleSet(m_spkScene);

		InitializeCameraMotion(0.1f,0.001f);
		InitializeObjectMotion(m_spkScene);
	}
	@Override
	public void keyPressed(KeyEvent e) {
		char ucKey = e.getKeyChar();

		super.keyPressed(e);

		float fAmbient, fTextureRepeat;
		float[] afValue = new float[4];
		switch (ucKey)
		{
		case 'h':
			m_fWaveHeightAmplitude -= 0.1f;
			if (m_fWaveHeightAmplitude < 0.0f)
			{
				m_fWaveHeightAmplitude = 0.0f;
			}
			for (int i = 0; i < 4; i++)
			{
				afValue[i] = m_fWaveHeightAmplitude*m_afWaveHeight[i];
			}
			m_spkEffect.SetWaveHeight(afValue);
			return;
		case 'H':
			m_fWaveHeightAmplitude += 0.1f;
			for (int i = 0; i < 4; i++)
			{
				afValue[i] = m_fWaveHeightAmplitude*m_afWaveHeight[i];
			}
			m_spkEffect.SetWaveHeight(afValue);
			return;
		case 'v':
			m_fWaveSpeedAmplitude -= 0.1f;
			if (m_fWaveSpeedAmplitude < 0.0f)
			{
				m_fWaveSpeedAmplitude = 0.0f;
			}
			for (int i = 0; i < 4; i++)
			{
				afValue[i] = m_fWaveSpeedAmplitude*m_afWaveSpeed[i];
			}
			m_spkEffect.SetWaveSpeed(afValue);
			return;
		case 'V':
			m_fWaveSpeedAmplitude += 0.1f;
			for (int i = 0; i < 4; i++)
			{
				afValue[i] = m_fWaveSpeedAmplitude*m_afWaveSpeed[i];
			}
			m_spkEffect.SetWaveSpeed(afValue);
			return;
		case 'a':
			fAmbient = m_spkEffect.GetAmbient();
			fAmbient -= 0.05f;
			if (fAmbient < 0.0f)
			{
				fAmbient = 0.0f;
			}
			m_spkEffect.SetAmbient(fAmbient);
			return;
		case 'A':
			fAmbient = m_spkEffect.GetAmbient();
			fAmbient += 0.05f;
			if (fAmbient > 1.0f)
			{
				fAmbient = 1.0f;
			}
			m_spkEffect.SetAmbient(fAmbient);
			return;
		case 'r':
			m_fBumpSpeedAmplitude -= 0.1f;
			if (m_fBumpSpeedAmplitude < 0.0f)
			{
				m_fBumpSpeedAmplitude = 0.0f;
			}
			for (int i = 0; i < 4; i++)
			{
				afValue[i] = m_fBumpSpeedAmplitude*m_afBumpSpeed[i];
			}
			m_spkEffect.SetBumpSpeed(afValue);
			return;
		case 'R':
			m_fBumpSpeedAmplitude += 0.1f;
			for (int i = 0; i < 4; i++)
			{
				afValue[i] = m_fBumpSpeedAmplitude*m_afBumpSpeed[i];
			}
			m_spkEffect.SetBumpSpeed(afValue);
			return;
		case 'T':
			fTextureRepeat = m_spkEffect.GetTextureRepeat();
			fTextureRepeat += 0.1f;
			m_spkEffect.SetTextureRepeat(fTextureRepeat);
			return;
		case 't':
			fTextureRepeat = m_spkEffect.GetTextureRepeat();
			fTextureRepeat -= 0.1f;
			if (fTextureRepeat < 0.0f)
			{
				fTextureRepeat = 0.0f;
			}
			m_spkEffect.SetTextureRepeat(fTextureRepeat);
			return;
		case ' ':
			m_bStopped = !m_bStopped;
			m_fStopTime = GetTime();
			return;
		case 's':
		case 'S':
			TestStreaming(m_spkScene,"RipplingOcean.wmof");
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
		m_bStopped = false;
		m_fStopTime = GetTime();

		m_spkScene = new Node();
		m_spkTrnNode = new Node();
		m_spkScene.AttachChild(m_spkTrnNode);
		m_spkWireframe = new WireframeState();
		m_spkScene.AttachGlobalState(m_spkWireframe);

		CreateRope();
	}
	private void DoPhysical()
	{
		for (int i = 0; i < m_iNumCtrlPoints; i++)
		{
			m_pkSpline.SetControlPoint(i,m_akCtrlPoint[i]);
		}
		m_spkRope.UpdateSurface();
		m_spkRope.VBuffer.Release();
	}

	private float GetTime()
	{
		if (!gs_bInitializedTime)
		{
			gs_bInitializedTime = true;
			Calendar kCalendar = Calendar.getInstance();
			gs_lInitialSec = kCalendar.get(Calendar.SECOND);
			gs_lInitialUSec = 1000*kCalendar.get(Calendar.MILLISECOND);
		}
		Calendar kCalendar = Calendar.getInstance();
		long lCurrentSec = kCalendar.get(Calendar.SECOND);
		long lCurrentUSec = 1000*kCalendar.get(Calendar.MILLISECOND);
		long lDeltaSec = lCurrentSec - gs_lInitialSec;
		long lDeltaUSec = lCurrentUSec -gs_lInitialUSec;
		if (lDeltaUSec < 0)
		{
			lDeltaUSec += 1000000;
			lDeltaSec--;
		}

		return (float)(0.001*(1000*lDeltaSec + lDeltaUSec/1000));
	}

}
