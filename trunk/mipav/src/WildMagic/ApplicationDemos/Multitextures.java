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

import javax.media.opengl.*;

import com.jogamp.opengl.util.*;
import javax.media.opengl.awt.GLCanvas;

import java.awt.*;
import java.awt.event.*;

import WildMagic.LibApplications.OpenGLApplication.*;
import WildMagic.LibFoundation.Mathematics.*;
import WildMagic.LibGraphics.Effects.*;
import WildMagic.LibGraphics.Rendering.*;
import WildMagic.LibGraphics.SceneGraph.*;
import WildMagic.LibGraphics.Shaders.*;
import WildMagic.LibRenderers.OpenGLRenderer.*;
import gov.nih.mipav.model.structures.ModelImage;
import gov.nih.mipav.view.ViewUserInterface;
import gov.nih.mipav.view.renderer.WildMagic.Render.Sculptor_WM;
import gov.nih.mipav.view.renderer.WildMagic.Render.VolumeImage;

public class Multitextures extends JavaApplication3D
implements GLEventListener, KeyListener
{
	public Multitextures()
	{
		super( "MultiTextures", 0, 0, 512, 512,
				new ColorRGBA( 0.0f,0.25f,0.75f,1.0f ) );
		m_pkRenderer = new OpenGLRenderer( m_eFormat, m_eDepth, m_eStencil,
				m_eBuffering, m_eMultisampling,
				m_iWidth, m_iHeight );
		((OpenGLRenderer)m_pkRenderer).GetCanvas().setSize( m_iWidth, m_iHeight );  
		((OpenGLRenderer)m_pkRenderer).GetCanvas().addGLEventListener( this );       
		((OpenGLRenderer)m_pkRenderer).GetCanvas().addKeyListener( this );       
		((OpenGLRenderer)m_pkRenderer).GetCanvas().addMouseListener( this );       
		((OpenGLRenderer)m_pkRenderer).GetCanvas().addMouseMotionListener( this );       

		String kExternalDirs = getExternalDirs();        
		ImageCatalog.SetActive( new ImageCatalog("Main", kExternalDirs) );      
		VertexProgramCatalog.SetActive(new VertexProgramCatalog("Main", kExternalDirs));       
		PixelProgramCatalog.SetActive(new PixelProgramCatalog("Main", kExternalDirs));
		CompiledProgramCatalog.SetActive(new CompiledProgramCatalog());
	}


	/**
	 * @param args
	 */
	public static void main(String[] args) {
		Multitextures kWorld = new Multitextures();
		Frame frame = new Frame(kWorld.GetWindowTitle());
		frame.add( kWorld.GetCanvas() );
		frame.setSize(kWorld.GetCanvas().getWidth(), kWorld.GetCanvas().getHeight());
		/* Animator serves the purpose of the idle function, calls display: */
		final Animator animator = new Animator( kWorld.GetCanvas() );
		frame.addWindowListener(new WindowAdapter() {
			public void windowClosing(WindowEvent e) {
				// Run this on another thread than the AWT event queue to
				// avoid deadlocks on shutdown on some platforms
				new Thread(new Runnable() {
					public void run() {
						animator.stop();
						System.exit(0);
					}
				}).start();
			}
		});
		frame.setVisible(true);
		animator.start();
		// and all the rest happens in the display function...

	}

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

		((OpenGLRenderer)m_pkRenderer).EnableUserClipPlane(0, m_kPlane );
		m_pkRenderer.ClearBuffers();
		if (m_pkRenderer.BeginScene())
		{          
			m_pkRenderer.DrawScene(m_kCuller.GetVisibleSet());
			DrawFrameRate(8,GetHeight()-8,ColorRGBA.WHITE);

			if ( m_kSculptor.IsSculptDrawn() )
			{
				m_pkRenderer.Draw( m_kSculptor.getSculptImage() );
			}
			m_pkRenderer.EndScene();
		}
		m_pkRenderer.DisplayBackBuffer();
		UpdateFrameCount();
	}

	public void displayChanged(GLAutoDrawable arg0, boolean arg1, boolean arg2) { }

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

		m_kSculptor = new Sculptor_WM( GetCanvas() );
	}

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

	public GLCanvas GetCanvas()
	{
		return ((OpenGLRenderer)m_pkRenderer).GetCanvas();
	}

	private void CreateScene ()
	{
		m_spkScene = new Node();
		m_spkWireframe = new WireframeState();
		m_spkScene.AttachGlobalState(m_spkWireframe);

		Attributes kAttr = new Attributes();
		kAttr.SetPChannels(3);
		kAttr.SetTChannels(0,2);
		kAttr.SetTChannels(1,2);
		StandardMesh kSM = new StandardMesh(kAttr);
		TriMesh pkPlane = kSM.Rectangle(2,2,1.0f,1.0f);

		// multiplicative (the default)
		m_spkEffect = new MultitextureEffect(2);
		m_spkEffect.SetImageName(0,"Horizontal");
		m_spkEffect.SetImageName(1,"Magician");
		m_spkEffect.Configure();

		pkPlane.AttachEffect(m_spkEffect);
		m_spkScene.AttachChild(pkPlane);

		m_iActive = 0;
	}

	private Node m_spkScene;
	private WireframeState m_spkWireframe;
	private Culler m_kCuller = new Culler(0,0,null);

	MultitextureEffect m_spkEffect;
	int m_iActive;

	private Plane3f m_kPlane = new Plane3f( new Vector3f( .5f, -1, -1 ), new Vector3f( .5f, 1, 1), new Vector3f( .5f, -1, 1) );
	Sculptor_WM m_kSculptor;

	public void keyPressed(KeyEvent e) {
		char ucKey = e.getKeyChar();

		System.err.println( ucKey );
		super.keyPressed(e);

		AlphaState pkAState;

		if (ucKey == 'w' || ucKey == 'W')
		{
			m_spkWireframe.Enabled = !m_spkWireframe.Enabled;
			return;
		}

		switch (ucKey)
		{
		case 'e':
		{
			m_kSculptor.enableSculpt(true);
			return;
		}
		case 'f':
		{
			m_kSculptor.enableSculpt(false);
			return;
		}
		case 'i':
		{
			m_kSculptor.invertSculpt();
			return;
		}
		case 'n':
		case 'N':
			if (m_iActive == 0)
			{
				// switch to hard additive
				pkAState = m_spkEffect.GetBlending(1);
				pkAState.SrcBlend = AlphaState.SrcBlendMode.SBF_ONE;
				pkAState.DstBlend = AlphaState.DstBlendMode.DBF_ONE;
				m_spkEffect.Configure();
				m_iActive = 1;
			}
			else if (m_iActive == 1)
			{
				// soft additive
				pkAState = m_spkEffect.GetBlending(1);
				pkAState.SrcBlend = AlphaState.SrcBlendMode.SBF_ONE_MINUS_DST_COLOR;
				pkAState.DstBlend = AlphaState.DstBlendMode.DBF_ONE;
				m_spkEffect.Configure();
				m_iActive = 2;
			}
			else
			{
				// multiplicative
				pkAState = m_spkEffect.GetBlending(1);
				pkAState.SrcBlend = AlphaState.SrcBlendMode.SBF_DST_COLOR;
				pkAState.DstBlend = AlphaState.DstBlendMode.DBF_ZERO;
				m_spkEffect.Configure();
				m_iActive = 0;
			}
			return;
		case 'o':
			ViewUserInterface.create();
			// output textures as tif...
			//m_pkRenderer.FrameBufferToTexSubImage3D( m_spkEffect.GetTexture(0,1), 0, true );
			ModelImage tex = VolumeImage.CreateImageFromTexture(m_spkEffect.GetTexture(0,1).GetImage());
			tex.setImageName( tex.getImageName() + ".tif" );
			System.err.println( tex.getImageFileName() );
			ModelImage.saveImage(tex);
			break;
		case 's':
		case 'S':
			TestStreaming(m_spkScene,"Multitexture.wmof");
			return;
		}
		return;
	}

	private String getExternalDirs()
	{
		String jar_filename = "";
		String class_path_key = "java.class.path";
		String class_path = System.getProperty(class_path_key);
		for (String fn : class_path.split(";") ) {
			if (fn.endsWith("WildMagic.jar")) {
				jar_filename = fn;   
				String externalDirs = jar_filename.substring(0, jar_filename.indexOf("lib\\"));
				externalDirs = externalDirs.concat("WildMagic");
				return externalDirs;
			}
		}
		return System.getProperties().getProperty("user.dir");
	}


	public void dispose(GLAutoDrawable arg0) {}

}
