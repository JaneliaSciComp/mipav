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

import WildMagic.LibFoundation.Mathematics.ColorRGBA;
import WildMagic.LibFoundation.Mathematics.Matrix3f;
import WildMagic.LibFoundation.Mathematics.Vector3f;
import WildMagic.LibGraphics.Effects.RipplingOceanEffect;
import WildMagic.LibGraphics.Rendering.GraphicsImage;
import WildMagic.LibGraphics.Rendering.WireframeState;
import WildMagic.LibGraphics.SceneGraph.Attributes;
import WildMagic.LibGraphics.SceneGraph.Node;
import WildMagic.LibGraphics.SceneGraph.StandardMesh;
import WildMagic.LibGraphics.SceneGraph.Transformation;
import WildMagic.LibGraphics.SceneGraph.TriMesh;
import WildMagic.LibRenderers.OpenGLRenderer.OpenGLRenderer;

import com.jogamp.opengl.util.Animator;


public class RipplingOcean extends DemoBase
implements GLEventListener, KeyListener
{

	private static final long serialVersionUID = -5455986857015040876L;
	/**
	 * @param args
	 */
	public static void main(String[] args) {
		RipplingOcean kWorld = new RipplingOcean();
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

	private RipplingOceanEffect m_spkEffect;


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

	private String m_kExternalDirs;
	public RipplingOcean()
	{
		super( "RipplingOcean" );

		m_kExternalDirs = getExternalDirs();        

		// Arbitrary constants to make particular looking waves.
		m_afWaveSpeed[0] = 0.2f;
		m_afWaveSpeed[1] = 0.15f;
		m_afWaveSpeed[2] = 0.4f;
		m_afWaveSpeed[3] = 0.4f;

		m_afWaveHeight[0] = -16.0f;
		m_afWaveHeight[1] = 10.0f;
		m_afWaveHeight[2] = 5.8f;
		m_afWaveHeight[3] = 8.5f;

		m_afBumpSpeed[0] = 0.031f;
		m_afBumpSpeed[1] = 0.04f;
		m_afBumpSpeed[2] = -0.03f;
		m_afBumpSpeed[3] = 0.02f;
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
		float fTime = (m_bStopped ? m_fStopTime : GetTime());
		m_spkEffect.SetTime(fTime);

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

		m_spkCamera.SetFrustum(-0.55f,0.55f,-0.4125f,0.4125f,1.0f,10000.0f);
		Vector3f kCLoc = new Vector3f(0.0f,-600.0f,-100.0f);
		Vector3f kCDir = new Vector3f(0.0f,1.0f,0.5f);
		kCDir.normalize();
		Vector3f kCUp = new Vector3f(0.0f,kCDir.Z,-kCDir.Y);
		Vector3f kCRight = new Vector3f(Vector3f.UNIT_X);
		kCRight.neg();
		m_spkCamera.SetFrame(kCLoc,kCDir,kCUp,kCRight);

		CreateScene();

		// initial update of objects
		m_spkScene.UpdateGS();
		m_spkScene.UpdateRS();

		// initial culling of scene
		m_kCuller.SetCamera(m_spkCamera);
		m_kCuller.ComputeVisibleSet(m_spkScene);

		InitializeCameraMotion(0.1f,0.001f);
		InitializeObjectMotion(m_spkScene);
	}

	@Override
	public void keyTyped(KeyEvent e) {
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
	private void CreateNormalMapFromHeightImage (final String acImageName)
	{
		// Given a height field z = H(x,y), normal vectors to the graph of the
		// function are
		//   N(x,y) = (dH/dx,dH/dy,1)/sqrt(1+(dH/dx)^2+(dH/dy)^2)
		// The terms dH/dx and dH/dy are the partial derivatives of H with respect
		// to x and y.  These are approximated by centered differences
		//   dH/dx = (H(x+dx,y) - H(x-dx,y))/(2*dx)
		//   dH/dy = (H(x,y+dy) - H(x,y-dy))/(2*dy)
		// When the height field is stored in an image I(x,y), where 0 <= x < xmax
		// and 0 <= y < ymax (x and y are integers), choose dx = 1 and dy = 1.
		// The image is gray scale, so the red, green, and blue channels all have
		// the same value.  It is sufficient to use the red channel, R(x,y).
		// The approximations are
		//   dH/dx = (R(x+1,y) - R(x-1,y))/2
		//   dH/dy = (R(x,y+1) - R(x,y-1))/2
		// Special handling is required at the image boundaries x = 0, x = xmax-1,
		// y = 0, and y = ymax-1.  Wrap-around is used here, so define values
		//   R(-1,y) = R(xmax-1,y)
		//   R(xmax,y) = R(0,y)
		//   R(x,-1) = R(x,ymax-1)
		//   R(x,ymax) = R(x,0)
		//
		// Since the red channel has values in [0,255], the approximations for
		// dH/dx and dH/dy are in [-255/2,255/2].  In this particular application,
		// these are scaled to [-100,100] (WHY?).

		GraphicsImage pkPlasma = 
			GraphicsImage.Load(acImageName, m_kExternalDirs);
		assert(pkPlasma != null);

		int iXMax = pkPlasma.GetBound(0);
		int iYMax = pkPlasma.GetBound(1);
		int iXMaxM1 = iXMax -1, iYMaxM1 = iYMax - 1;
		int iBPP = pkPlasma.GetBytesPerPixel();
		byte[] aucHeight = pkPlasma.GetData();
		final float fScale = 100.0f/255.0f;
		byte[] aucNormal = new byte[4*iXMax*iYMax];

		for (int iY = 0; iY < iYMax; iY++)
		{
			int iYm1 = (iY > 0 ? iY-1 : iYMaxM1);
			int iYp1 = (iY < iYMaxM1 ? iY+1 : 0);

			for (int iX = 0; iX < iXMax; iX++)
			{
				int iXm1 = (iX > 0 ? iX-1 : iXMaxM1);
				int iXp1 = (iX < iXMaxM1 ? iX+1 : 0);

				float fRXm1Y = (aucHeight[iBPP*(iXm1 + iY*iXMax)] & 0xFF);
				float fRXp1Y = (aucHeight[iBPP*(iXp1 + iY*iXMax)] & 0xFF);
				float fRXYm1 = (aucHeight[iBPP*(iX + iYm1*iXMax)] & 0xFF);
				float fRXYp1 = (aucHeight[iBPP*(iX + iYp1*iXMax)] & 0xFF);
				float fDHDX = 0.5f*(fRXp1Y - fRXm1Y);
				float fDHDY = 0.5f*(fRXYp1 - fRXYm1);

				Vector3f kNormal = new Vector3f(fScale*fDHDX,fScale*fDHDY,1.0f);
				kNormal.normalize();

				// Transform the normal vector from [-1,1]^3 to [0,255]^3 so it
				// can be stored as a color value.
				aucNormal[4*(iX+iY*iXMax) + 0] = (byte)(127.5f*(kNormal.X+1.0f));
				aucNormal[4*(iX+iY*iXMax) + 1] = (byte)(127.5f*(kNormal.Y+1.0f));
				aucNormal[4*(iX+iY*iXMax) + 2] = (byte)(127.5f*(kNormal.Z+1.0f));
				aucNormal[4*(iX+iY*iXMax) + 3] = 0;
			}
		}

		pkPlasma = null;
		new GraphicsImage(GraphicsImage.FormatMode.IT_RGBA8888,iXMax,iYMax,aucNormal,"NormalMap");
	}
	private void CreateScene ()
	{
		m_bStopped = false;
		m_fStopTime = GetTime();

		m_spkScene = new Node();
		m_spkWireframe = new WireframeState();
		m_spkScene.AttachGlobalState(m_spkWireframe);

		// The height field has origin (0,0,0) and up-direction of (0,0,-1).
		Attributes kAttr = new Attributes();
		kAttr.SetPChannels(3);
		kAttr.SetNChannels(3);
		kAttr.SetCChannels(0,3);
		kAttr.SetTChannels(0,2);
		Transformation kXFrm = new Transformation();
		kXFrm.SetRotate( new Matrix3f(Vector3f.UNIT_Y,(float)Math.PI));
		StandardMesh kSM = new StandardMesh(kAttr,false,kXFrm);
		TriMesh pkMesh = kSM.Rectangle(50,50,1400.0f,1200.0f);
		m_spkScene.AttachChild(pkMesh);

		CreateNormalMapFromHeightImage("Plasma");

		m_spkEffect = new RipplingOceanEffect("NormalMap","WaterGradient",
		"RippleSky");
		pkMesh.AttachEffect(m_spkEffect);

		// Fix the light direction for the entire simulation.
		Vector3f kLightDir = new Vector3f(0.0f,1.0f,1.0f);
		kLightDir.normalize();
		m_spkEffect.SetLightDir(kLightDir);

		// Arbitrary constants to make particular looking waves.
		float[] afWaveDirX = new float[]{0.25f,0.0f,-0.7f,-0.8f};
		float[] afWaveDirY = new float[]{0.0f,0.15f,-0.7f,0.1f};
		float[] afWaveOffset = new float[]{0.0f,0.2f,0.3f,-0.2f};
		m_spkEffect.SetWaveDirX(afWaveDirX);
		m_spkEffect.SetWaveDirY(afWaveDirY);
		m_spkEffect.SetWaveOffset(afWaveOffset);

		m_spkEffect.SetAverageDuDxDvDy(1.0f/24.0f);
		m_spkEffect.SetAmbient(0.3f);
		m_spkEffect.SetTextureRepeat(6.0f);

		m_fWaveSpeedAmplitude = 1.0f;
		m_fWaveHeightAmplitude = 1.0f;
		m_fBumpSpeedAmplitude = 1.0f;
		m_spkEffect.SetWaveSpeed(m_afWaveSpeed);
		m_spkEffect.SetWaveHeight(m_afWaveHeight);
		m_spkEffect.SetBumpSpeed(m_afBumpSpeed);
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
