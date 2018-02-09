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
import javax.media.opengl.awt.GLCanvas;

import WildMagic.LibFoundation.Mathematics.ColorRGBA;
import WildMagic.LibFoundation.Mathematics.Vector3f;
import WildMagic.LibGraphics.Effects.ShaderEffect;
import WildMagic.LibGraphics.Rendering.AlphaState;
import WildMagic.LibGraphics.Rendering.CullState;
import WildMagic.LibGraphics.Rendering.GraphicsImage;
import WildMagic.LibGraphics.Rendering.Texture;
import WildMagic.LibGraphics.Rendering.WireframeState;
import WildMagic.LibGraphics.SceneGraph.Attributes;
import WildMagic.LibGraphics.SceneGraph.IndexBuffer;
import WildMagic.LibGraphics.SceneGraph.Node;
import WildMagic.LibGraphics.SceneGraph.TriMesh;
import WildMagic.LibGraphics.SceneGraph.VertexBuffer;
import WildMagic.LibGraphics.Shaders.PixelShader;
import WildMagic.LibGraphics.Shaders.Program;
import WildMagic.LibGraphics.Shaders.VertexShader;
import WildMagic.LibRenderers.OpenGLRenderer.OpenGLRenderer;

import com.jogamp.opengl.util.Animator;


public class VolumeTextures extends DemoBase
implements GLEventListener, KeyListener
{

	private static final long serialVersionUID = -1844630874114966424L;

	/**
	 * @param args
	 */
	public static void main(String[] args) {
		VolumeTextures kWorld = new VolumeTextures(); 
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
final int iBound = 256;


	private WireframeState m_spkWireframe;

	private ShaderEffect m_spkVolumeTexture;

	private float[] m_afCommonAlpha = new float[4];  // channel 0 has the alpha value

	public VolumeTextures()
	{
		super("VolumeTextures");
	}

	public VolumeTextures(GLCanvas kCanvas, Node scene, boolean bShared)
	{
		super("VolumeTextures", kCanvas);
		m_spkScene = scene;
		m_bShared = bShared;
	}

	@Override
	public void display(GLAutoDrawable arg0) {
        ((OpenGLRenderer)m_pkRenderer).SetDrawable( arg0 );
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
			DrawFrameRate(8,GetHeight()-8,ColorRGBA.BLACK);
			m_pkRenderer.EndScene();
		}
		m_pkRenderer.DisplayBackBuffer();
		UpdateFrameCount();
		
		Program pkCProgram = m_spkVolumeTexture.GetCProgram(0);
		pkCProgram.GetUC("CommonAlpha").SetDataSource(m_afCommonAlpha);
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
		m_spkCamera.SetFrustum(60.0f,m_iWidth/(float)m_iHeight,0.01f,100.0f);
		Vector3f kCLoc = new Vector3f(0.0f,0.0f,4.0f);
		Vector3f kCDir = new Vector3f(0.0f,0.0f,-1.0f);
		Vector3f kCUp = new Vector3f(0.0f,1.0f,0.0f);
		kCDir.normalize();
		kCUp.normalize();
		Vector3f kCRight = Vector3f.cross( kCDir, kCUp );
		m_spkCamera.SetFrame(kCLoc,kCDir,kCUp,kCRight);
		
		
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

		InitializeCameraMotion(0.005f,0.002f);
		InitializeObjectMotion(m_spkScene);
	}

	@Override
	public void keyPressed(KeyEvent e) {
		char ucKey = e.getKeyChar();

		super.keyPressed(e);

		switch (ucKey)
		{
		case 'w':
		case 'W':
			m_spkWireframe.Enabled = !m_spkWireframe.Enabled;
			return;
		case 's':
		case 'S':
			TestStreaming(m_spkScene,"VolumeTextures.wmof");
			return;
		}
		return;
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
		m_spkWireframe = new WireframeState();
		m_spkScene.AttachGlobalState(m_spkWireframe);

		AlphaState pkAState = new AlphaState();
		pkAState.BlendEnabled = true;
		m_spkScene.AttachGlobalState(pkAState);

		CullState pkCState = new CullState();
		pkCState.Enabled = false;
		m_spkScene.AttachGlobalState(pkCState);

		CreateVolumeTexture();
		CreateGridMesh();

	}
	void CreateGridMesh ()
	{
		final int iSlices = iBound;
		final int iDelta = 32;
		for (int i = 0; i < iSlices; i++)
		{
			float fW = i/(float)(iSlices-1);
			m_spkScene.AttachChild(Rectangle(iDelta,iDelta,fW));
		}
		m_spkScene.AttachEffect(m_spkVolumeTexture);
	}
	void CreateVolumeTexture ()
	{
		byte[] aucData = new byte[4*iBound*iBound*iBound];

		// Create three Gaussian distributions to be used for the RGB color
		// channels.  The alpha channel is constant and is stored as a pixel
		// shader constant.
		final float fRParam = 0.01f;
		final float fGParam = 0.01f;
		final float fBParam = 0.01f;
		final float fExtreme = 8.0f;
		Vector3f kRCenter = new Vector3f(0.5f*fExtreme,0.0f,0.0f);
		Vector3f kGCenter = new Vector3f(-0.5f*fExtreme,-0.5f*fExtreme,0.0f);
		Vector3f kBCenter = new Vector3f(-0.5f*fExtreme,+0.5f*fExtreme,0.0f);
		m_afCommonAlpha[0] = 0.05f;

		Vector3f kPoint = new Vector3f();
		Vector3f kDiff = new Vector3f();
		int i = 0;
		for (int iZ = 0; iZ < iBound; iZ++)
		{
			kPoint.Z = -fExtreme + 2.0f*fExtreme*iZ/(iBound-1);
			for (int iY = 0; iY < iBound; iY++)
			{
				kPoint.Y = -fExtreme + 2.0f*fExtreme*iY/(iBound-1);
				for (int iX = 0; iX < iBound; iX++)
				{
					kPoint.X = -fExtreme + 2.0f*fExtreme*iX/(iBound-1);
					kDiff.copy( kPoint ).sub( kRCenter );
					float fRSqr = kDiff.squaredLength();
					float fRGauss = 1.0f - fRParam*fRSqr;
					if (fRGauss < 0.0f)
					{
						fRGauss = 0.0f;
					}
					kDiff.copy( kPoint ).sub( kGCenter );
					fRSqr = kDiff.squaredLength();
					float fGGauss = 1.0f - fGParam*fRSqr;
					if (fGGauss < 0.0f)
					{
						fGGauss = 0.0f;
					}
					kDiff.copy( kPoint ).sub( kBCenter );
					fRSqr = kDiff.squaredLength();
					float fBGauss = 1.0f - fBParam*fRSqr;
					if (fBGauss < 0.0f)
					{
						fBGauss = 0.0f;
					}

					aucData[i++] = (byte)(255.0f*fRGauss);
					aucData[i++] = (byte)(255.0f*fGGauss);
					aucData[i++] = (byte)(255.0f*fBGauss);
					aucData[i++] = (byte)(255.0f);
				}
			}
		}
		kDiff = null;

		new GraphicsImage(
				GraphicsImage.FormatMode.IT_CUBE_RGBA8888,iBound,iBound,iBound,aucData,
				"VolumeImage");

		VertexShader pkVShader = new VertexShader("VolumeTexturesV");
		PixelShader pkPShader = new PixelShader("VolumeTexturesP");

		pkPShader.SetTextureQuantity(1);
		pkPShader.SetImageName(0,"VolumeImage", "BaseSampler");
		pkPShader.GetTexture(0).SetFilterType(Texture.FilterType.NEAREST);
		pkPShader.GetTexture(0).SetWrapType(0,Texture.WrapType.REPEAT);
		pkPShader.GetTexture(0).SetWrapType(1,Texture.WrapType.REPEAT);

		m_spkVolumeTexture = new ShaderEffect(1);
		m_spkVolumeTexture.SetVShader(0,pkVShader);
		m_spkVolumeTexture.SetPShader(0,pkPShader);
	}


	TriMesh Rectangle (int iXSamples, int iYSamples, float fW)
	{
		Attributes kAttr = new Attributes();
		kAttr.SetPChannels(3);
		kAttr.SetTChannels(0,3);

		int iVQuantity = iXSamples*iYSamples;
		int iTQuantity = 2*(iXSamples-1)*(iYSamples-1);
		VertexBuffer pkVB = new VertexBuffer(kAttr,iVQuantity);
		IndexBuffer pkIB = new IndexBuffer(3*iTQuantity);

		// generate geometry
		float fInv0 = 1.0f/(iXSamples - 1.0f);
		float fInv1 = 1.0f/(iYSamples - 1.0f);
		//Vector3f kZTmp = Vector3f.UNIT_Z.scale(2.0f*fW - 1.0f);
		float fU, fV;
		int i, i0, i1;
		for (i1 = 0, i = 0; i1 < iYSamples; i1++)
		{
			fV = i1*fInv1;
			//Vector3f kYTmp = Vector3f.UNIT_Y.scale(2.0f*fV - 1.0f);
			for (i0 = 0; i0 < iXSamples; i0++)
			{
				fU = i0*fInv0;
				//Vector3f kXTmp = Vector3f.UNIT_X.scale(2.0f*fU - 1.0f);
				//pkVB.Position3(i, kXTmp.add( kYTmp ).add( kZTmp ) );

				pkVB.SetPosition3(i, 
						2.0f*fU - 1.0f,
						2.0f*fV - 1.0f,
						2.0f*fW - 1.0f );

				pkVB.SetTCoord3(0,i, fU,fV,fW );
				i++;
			}
		}

		// generate connectivity
		int[] aiIndex = pkIB.GetData();
		for (i1 = 0, i = 0; i1 < iYSamples - 1; i1++)
		{
			for (i0 = 0; i0 < iXSamples - 1; i0++)
			{
				int iV0 = i0 + iXSamples * i1;
				int iV1 = iV0 + 1;
				int iV2 = iV1 + iXSamples;
				int iV3 = iV0 + iXSamples;
				aiIndex[i++] = iV0;
				aiIndex[i++] = iV1;
				aiIndex[i++] = iV2;
				aiIndex[i++] = iV0;
				aiIndex[i++] = iV2;
				aiIndex[i++] = iV3;
			}
		}
		return new TriMesh(pkVB,pkIB);
	}
}
