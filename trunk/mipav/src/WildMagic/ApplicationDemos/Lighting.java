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
import WildMagic.LibFoundation.Mathematics.Vector3f;
import WildMagic.LibGraphics.Effects.DefaultShaderEffect;
import WildMagic.LibGraphics.Rendering.Light;
import WildMagic.LibGraphics.Rendering.MaterialState;
import WildMagic.LibGraphics.Rendering.WireframeState;
import WildMagic.LibGraphics.SceneGraph.Attributes;
import WildMagic.LibGraphics.SceneGraph.Node;
import WildMagic.LibGraphics.SceneGraph.StandardMesh;
import WildMagic.LibGraphics.SceneGraph.TriMesh;
import WildMagic.LibRenderers.OpenGLRenderer.OpenGLRenderer;

import com.jogamp.opengl.util.Animator;

public class Lighting extends DemoBase
implements GLEventListener, KeyListener
{

	private static final long serialVersionUID = 1672971998301669720L;
	/**
	 * @param args
	 */
	public static void main(String[] args) {
		Lighting kWorld = new Lighting();
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


	private TriMesh m_spkPlane, m_spkSphere;

	private WireframeState m_spkWireframe;

	private int m_iAQuantity, m_iDQuantity, m_iPQuantity, m_iSQuantity;

	private Light[] m_aspkALight = new Light[8];

	private Light[] m_aspkDLight = new Light[8];
	

	private Light[] m_aspkPLight = new Light[8];
	private Light[] m_aspkSLight = new Light[8];
	private DefaultShaderEffect m_spkDefaultEffect;
	private int m_iLightQuantity;

	private String m_acCaption = new String("........");

	private boolean m_bUpdateEffects = false;

	private MaterialState m_pkMaterial;
	private ColorRGB m_kDiffuseColor;

	Long lMemory;
	String kMemory = new String();
	public Lighting()
	{
		super("Lighting");
	}
	@Override
	public void display(GLAutoDrawable arg0) {
		if ( m_bUpdateEffects )
		{
			UpdateEffectsOnIdle();
		}
        if (MoveCamera())
        {
            m_kCuller.ComputeVisibleSet(m_spkScene);
        }
        
        if (MoveObject())
        {
            m_spkScene.UpdateGS();
            m_kCuller.ComputeVisibleSet(m_spkScene);
        }
		MeasureTime();
		m_pkRenderer.ClearBuffers();
		if (m_pkRenderer.BeginScene())
		{
			m_pkRenderer.DrawScene(m_kCuller.GetVisibleSet());
			/*
            for ( int i = 0; i < 1000; i++ )
            {
                m_spkSphere.Local.SetTranslate( m_afRandX[i],
                                                m_afRandY[i],
                                                m_afRandZ[i] );
                m_spkSphere.Local.SetScale( m_afScaleX[i],
                                            m_afScaleY[i],
                                            m_afScaleZ[i] );
                m_spkSphere.UpdateGS();
                m_pkRenderer.Draw(m_spkSphere);
            }
			 */
			m_pkRenderer.Draw(8,16,ColorRGBA.BLACK,m_acCaption);
			DrawFrameRate(arg0, 8,GetHeight()-8,ColorRGBA.BLACK);
			m_pkRenderer.EndScene();
		}
		m_pkRenderer.DisplayBackBuffer();

		/*
        m_kDiffuseColor.R( m_kDiffuseColor.R() + .1f );
        if ( m_kDiffuseColor.R() > 1.0f )
        {
            m_kDiffuseColor.R( 1.0f );
            m_kDiffuseColor.G( m_kDiffuseColor.G() + .1f );
            if ( m_kDiffuseColor.G() > 1.0f )
            {
                m_kDiffuseColor.G( 1.0f );
                m_kDiffuseColor.B( m_kDiffuseColor.B() + .1f );
                if ( m_kDiffuseColor.B() > 1.0f )
                {
                    m_kDiffuseColor.R( 0.0f );
                    m_kDiffuseColor.G( 0.0f );
                    m_kDiffuseColor.B( 0.0f );
                }
            }
        }
		 */
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
		Vector3f kCLoc = new Vector3f(8.0f,0.0f,4.0f);
		Vector3f kCDir = new Vector3f(kCLoc);
		kCDir.neg();  // lookat origin
		kCDir.normalize();
		Vector3f kCUp = new Vector3f(kCDir.Z,0,-kCDir.X);
		Vector3f kCRight = new Vector3f(Vector3f.UNIT_Y);
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
		if (ucKey == 'w' || ucKey == 'W')
		{
			m_spkWireframe.Enabled = !m_spkWireframe.Enabled;
			return;
		}

		switch (ucKey)
		{
		case 'A':  // remove an ambient light
			if (m_iAQuantity > 0)
			{
				m_iAQuantity--;
				m_iLightQuantity--;
				UpdateEffects();
			}
			return;
		case 'a':  // add an ambient light
			if (m_iLightQuantity < 8)
			{
				m_iAQuantity++;
				m_iLightQuantity++;
				UpdateEffects();
			}
			return;
		case 'D':  // remove a directional light
			if (m_iDQuantity > 0)
			{
				m_iDQuantity--;
				m_iLightQuantity--;
				UpdateEffects();
			}
			return;
		case 'd':  // add a directional light
			if (m_iLightQuantity < 8)
			{
				m_iDQuantity++;
				m_iLightQuantity++;
				UpdateEffects();
			}
			return;
		case 'P':  // remove a point light
			if (m_iPQuantity > 0)
			{
				m_iPQuantity--;
				m_iLightQuantity--;
				UpdateEffects();
			}
			return;
		case 'p':  // add a point light
			if (m_iLightQuantity < 8)
			{
				m_iPQuantity++;
				m_iLightQuantity++;
				UpdateEffects();
			}
			return;
		case 'S':  // remove a spot light
			if (m_iSQuantity > 0)
			{
				m_iSQuantity--;
				m_iLightQuantity--;
				UpdateEffects();
			}
			return;
		case 's':  // add a spot light
			if (m_iLightQuantity < 8)
			{
				m_iSQuantity++;
				m_iLightQuantity++;
				UpdateEffects();
			}
			return;
		case 'v':
		case 'V':
			TestStreaming(m_spkScene,"Lighting.wmof");
			return;
		case 'm':
			m_kDiffuseColor.R = m_kDiffuseColor.R + .1f;
			if ( m_kDiffuseColor.R > 1.0f )
			{
				m_kDiffuseColor.R = 1.0f;
				m_kDiffuseColor.G = m_kDiffuseColor.G + .1f;
				if ( m_kDiffuseColor.G > 1.0f )
				{
					m_kDiffuseColor.G = 1.0f;
					m_kDiffuseColor.B = m_kDiffuseColor.B + .1f;
					if ( m_kDiffuseColor.B > 1.0f )
					{
						m_kDiffuseColor.R = 0.0f;
						m_kDiffuseColor.G = 0.0f;
						m_kDiffuseColor.B = 0.0f;
					}
				}
			}
			return;
		}

		if (m_iLightQuantity == 0)
		{
			return;
		}

		/*
        if ('0' <= ucKey && ucKey <= '0' + m_iLightQuantity - 1)
        {
            m_iActiveLight = (int)ucKey - (int)'0';
            return;
        }

        if (m_iActiveLight < 0 || m_iActiveLight >= m_iLightQuantity)
        {
            return false;
        }

        Light* pkLight = m_spkScene.GetLight(m_iActiveLight);

        switch (ucKey)
        {
        case 'i':
            pkLight.Intensity -= 0.125f;
            if (pkLight.Intensity < 0.0f)
            {
                pkLight.Intensity = 0.0f;
            }
            return;
        case 'I':
            pkLight.Intensity += 0.125f;
            return;
        case 'c':
            pkLight.Angle -= 0.1f;
            if (pkLight.Angle < 0.0f)
            {
                pkLight.Angle = 0.0f;
            }
            pkLight.SetAngle(pkLight.Angle);
            return;
        case 'C':
            pkLight.Angle += 0.1f;
            if (pkLight.Angle > Mathf::HALF_PI)
            {
                pkLight.Angle = Mathf::HALF_PI;
            }
            pkLight.SetAngle(pkLight.Angle);
            return;
        case 'e':
            pkLight.Exponent *= 0.5f;
            return;
        case 'E':
            pkLight.Exponent *= 2.0f;
            return;
        }
		 */        
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

	//----------------------------------------------------------------------------
	private void CreateLights ()
	{
		int i;
		for (i = 0; i < 8; i++)
		{
			m_aspkALight[i] = new Light(Light.LightType.LT_AMBIENT);
			m_aspkDLight[i] = new Light(Light.LightType.LT_DIRECTIONAL);
			m_aspkPLight[i] = new Light(Light.LightType.LT_POINT);
			m_aspkSLight[i] = new Light(Light.LightType.LT_SPOT);
		}

		// ambient lights
		float fValue = 0.75f;
		m_aspkALight[0].Ambient = new ColorRGB(fValue,fValue,fValue);
		m_aspkALight[1].Ambient = new ColorRGB(fValue,0.0f,  0.0f);
		m_aspkALight[2].Ambient = new ColorRGB(0.0f,  fValue,0.0f);
		m_aspkALight[3].Ambient = new ColorRGB(0.0f,  0.0f,  fValue);
		m_aspkALight[4].Ambient = new ColorRGB(0.0f,  fValue,fValue);
		m_aspkALight[5].Ambient = new ColorRGB(fValue,0.0f,  fValue);
		m_aspkALight[6].Ambient = new ColorRGB(fValue,fValue,0.0f);
		m_aspkALight[7].Ambient = new ColorRGB(fValue,fValue,fValue);

		// directional lights
		fValue = (float)-Math.sqrt(1.0f/3.0f);
		m_aspkDLight[0].Ambient = new ColorRGB(fValue,fValue,fValue);
		m_aspkDLight[1].Ambient = new ColorRGB(fValue,0.0f,  0.0f);
		m_aspkDLight[2].Ambient = new ColorRGB(0.0f,  fValue,0.0f);
		m_aspkDLight[3].Ambient = new ColorRGB(0.0f,  0.0f,  fValue);
		m_aspkDLight[4].Ambient = new ColorRGB(0.0f,  fValue,fValue);
		m_aspkDLight[5].Ambient = new ColorRGB(fValue,0.0f,  fValue);
		m_aspkDLight[6].Ambient = new ColorRGB(fValue,fValue,0.0f);
		m_aspkDLight[7].Ambient = new ColorRGB(fValue,fValue,fValue);
		m_aspkDLight[0].DVector = new Vector3f(+fValue,+fValue,+fValue);
		m_aspkDLight[1].DVector = new Vector3f(+fValue,+fValue,-fValue);
		m_aspkDLight[2].DVector = new Vector3f(+fValue,-fValue,+fValue);
		m_aspkDLight[3].DVector = new Vector3f(+fValue,-fValue,-fValue);
		m_aspkDLight[4].DVector = new Vector3f(-fValue,+fValue,+fValue);
		m_aspkDLight[5].DVector = new Vector3f(-fValue,+fValue,-fValue);
		m_aspkDLight[6].DVector = new Vector3f(-fValue,-fValue,+fValue);
		m_aspkDLight[7].DVector = new Vector3f(-fValue,-fValue,-fValue);
		for (i = 0; i < 8; i++)
		{
			m_aspkDLight[i].Diffuse = new ColorRGB(ColorRGB.WHITE);
			m_aspkDLight[i].Specular = new ColorRGB(ColorRGB.WHITE);
		}

		// point lights
		fValue = 0.1f;
		m_aspkPLight[0].Ambient = new ColorRGB(fValue,fValue,fValue);
		m_aspkPLight[1].Ambient = new ColorRGB(fValue,0.0f,  0.0f);
		m_aspkPLight[2].Ambient = new ColorRGB(0.0f,  fValue,0.0f);
		m_aspkPLight[3].Ambient = new ColorRGB(0.0f,  0.0f,  fValue);
		m_aspkPLight[4].Ambient = new ColorRGB(0.0f,  fValue,fValue);
		m_aspkPLight[5].Ambient = new ColorRGB(fValue,0.0f,  fValue);
		m_aspkPLight[6].Ambient = new ColorRGB(fValue,fValue,0.0f);
		m_aspkPLight[7].Ambient = new ColorRGB(fValue,fValue,fValue);
		fValue = 4.0f;
		m_aspkPLight[0].Position = new Vector3f(+fValue,+fValue,+fValue);
		m_aspkPLight[1].Position = new Vector3f(+fValue,+fValue,-fValue);
		m_aspkPLight[2].Position = new Vector3f(+fValue,-fValue,+fValue);
		m_aspkPLight[3].Position = new Vector3f(+fValue,-fValue,-fValue);
		m_aspkPLight[4].Position = new Vector3f(-fValue,+fValue,+fValue);
		m_aspkPLight[5].Position = new Vector3f(-fValue,+fValue,-fValue);
		m_aspkPLight[6].Position = new Vector3f(-fValue,-fValue,+fValue);
		m_aspkPLight[7].Position = new Vector3f(-fValue,-fValue,-fValue);
		for (i = 0; i < 8; i++)
		{
			m_aspkPLight[i].Diffuse = new ColorRGB(ColorRGB.WHITE);
			m_aspkPLight[i].Specular = new ColorRGB(ColorRGB.WHITE);
		}

		// spot lights
		fValue = 0.1f;
		m_aspkSLight[0].Ambient = new ColorRGB(fValue,fValue,fValue);
		m_aspkSLight[1].Ambient = new ColorRGB(fValue,0.0f,  0.0f);
		m_aspkSLight[2].Ambient = new ColorRGB(0.0f,  fValue,0.0f);
		m_aspkSLight[3].Ambient = new ColorRGB(0.0f,  0.0f,  fValue);
		m_aspkSLight[4].Ambient = new ColorRGB(0.0f,  fValue,fValue);
		m_aspkSLight[5].Ambient = new ColorRGB(fValue,0.0f,  fValue);
		m_aspkSLight[6].Ambient = new ColorRGB(fValue,fValue,0.0f);
		m_aspkSLight[7].Ambient = new ColorRGB(fValue,fValue,fValue);
		fValue = 4.0f;
		m_aspkSLight[0].Position = new Vector3f(+fValue,+fValue,+fValue);
		m_aspkSLight[1].Position = new Vector3f(+fValue,+fValue,-fValue);
		m_aspkSLight[2].Position = new Vector3f(+fValue,-fValue,+fValue);
		m_aspkSLight[3].Position = new Vector3f(+fValue,-fValue,-fValue);
		m_aspkSLight[4].Position = new Vector3f(-fValue,+fValue,+fValue);
		m_aspkSLight[5].Position = new Vector3f(-fValue,+fValue,-fValue);
		m_aspkSLight[6].Position = new Vector3f(-fValue,-fValue,+fValue);
		m_aspkSLight[7].Position = new Vector3f(-fValue,-fValue,-fValue);
		fValue = (float)-Math.sqrt(1.0f/3.0f);
		m_aspkSLight[0].DVector = new Vector3f(+fValue,+fValue,+fValue);
		m_aspkSLight[1].DVector = new Vector3f(+fValue,+fValue,-fValue);
		m_aspkSLight[2].DVector = new Vector3f(+fValue,-fValue,+fValue);
		m_aspkSLight[3].DVector = new Vector3f(+fValue,-fValue,-fValue);
		m_aspkSLight[4].DVector = new Vector3f(-fValue,+fValue,+fValue);
		m_aspkSLight[5].DVector = new Vector3f(-fValue,+fValue,-fValue);
		m_aspkSLight[6].DVector = new Vector3f(-fValue,-fValue,+fValue);
		m_aspkSLight[7].DVector = new Vector3f(-fValue,-fValue,-fValue);
		for (i = 0; i < 8; i++)
		{
			m_aspkSLight[i].Diffuse = new ColorRGB(ColorRGB.WHITE);
			m_aspkSLight[i].Specular = new ColorRGB(ColorRGB.WHITE);
			m_aspkSLight[i].Exponent = 1.0f;
			m_aspkSLight[i].SetAngle((float)(0.125f*Math.PI));
		}
	}
	//----------------------------------------------------------------------------
	private void CreatePlane ()
	{
		Attributes kAttr = new Attributes();
		kAttr.SetPChannels(3);
		kAttr.SetNChannels(3);
		StandardMesh kSM = new StandardMesh(kAttr);
		m_spkPlane = kSM.Rectangle(128,128,16.0f,16.0f);

		// polished copper
		MaterialState pkMaterial = new MaterialState();
		pkMaterial.Emissive = new ColorRGB(ColorRGB.BLACK);
		pkMaterial.Ambient = new ColorRGB(0.2295f,0.08825f,0.0275f);
		pkMaterial.Diffuse = new ColorRGB(0.5508f,0.2118f,0.066f);
		pkMaterial.Specular = new ColorRGB(0.580594f,0.223257f,0.0695701f);
		pkMaterial.Shininess = 51.2f;
		m_spkPlane.AttachGlobalState(pkMaterial);

		m_spkScene.AttachChild(m_spkPlane);
	}
	
	//private float[] m_afRandX;
	//private float[] m_afRandY;
	//private float[] m_afRandZ;

	//private float[] m_afScaleX;
	//private float[] m_afScaleY;
	//private float[] m_afScaleZ;

	private void CreateScene ()
	{
		m_spkScene = new Node();
		m_spkWireframe = new WireframeState();
		m_spkScene.AttachGlobalState(m_spkWireframe);

		CreateLights();
		CreatePlane();
		CreateSphere();

		m_spkDefaultEffect = new DefaultShaderEffect();
		m_spkPlane.AttachEffect(m_spkDefaultEffect);
		m_spkSphere.AttachEffect(m_spkDefaultEffect);
	}
	//----------------------------------------------------------------------------
	private void CreateSphere ()
	{
		Attributes kAttr = new Attributes();
		kAttr.SetPChannels(3);
		kAttr.SetNChannels(3);
		StandardMesh kSM = new StandardMesh(kAttr);
		m_spkSphere = kSM.Sphere(64,64,1.0f);
		m_spkSphere.Local.SetTranslate( new Vector3f(0.0f,0.0f,1.0f));
		
		m_kDiffuseColor = new ColorRGB(0.34615f,0.3143f,0.0903f);

		// polished gold
		m_pkMaterial = new MaterialState();
		m_pkMaterial.Emissive = new ColorRGB(ColorRGB.BLACK);
		m_pkMaterial.Ambient = new ColorRGB(0.24725f,0.2245f,0.0645f);
		m_pkMaterial.Diffuse = m_kDiffuseColor;
		m_pkMaterial.Specular = new ColorRGB(0.797357f,0.723991f,0.208006f);
		m_pkMaterial.Shininess = 83.2f;
		m_spkSphere.AttachGlobalState(m_pkMaterial);

		m_spkScene.AttachChild(m_spkSphere);
		/*
        m_afRandX = new float[1000];
        m_afRandY = new float[1000];
        m_afRandZ = new float[1000];

        m_afScaleX = new float[1000];
        m_afScaleY = new float[1000];
        m_afScaleZ = new float[1000];

        for ( int i = 0; i < 1000; i++ )
        {
            m_afRandX[i] = 2*Mathf.SymmetricRandom();
            m_afRandY[i] = 2*Mathf.SymmetricRandom();
            m_afRandZ[i] = 2*Mathf.SymmetricRandom();

            m_afScaleX[i] = (float)(.25f + .75*Mathf.UnitRandom());
            m_afScaleY[i] = (float)(.25f + .75*Mathf.UnitRandom());
            m_afScaleZ[i] = (float)(.25f + .75*Mathf.UnitRandom());
        }

        System.err.println( m_spkSphere.GetTriangleQuantity() * 1000 );
		 */
	}

	private void UpdateEffects ()
	{
		m_bUpdateEffects = true;
	}


	private void UpdateEffectsOnIdle ()
	{
		m_bUpdateEffects = false;
		m_acCaption = "";	
		if (m_iLightQuantity > 0)
		{
			if (m_spkPlane.GetEffectQuantity() > 0)
			{
				// Release the vertex buffers since the lighting effects have a
				// different vertex layout than the default effect (the lights
				// require normal vectors).
				m_spkPlane.Reload(true);
				m_spkSphere.Reload(true);

				m_spkPlane.DetachEffect(m_spkDefaultEffect);
				m_spkSphere.DetachEffect(m_spkDefaultEffect);
			}

			m_spkScene.DetachAllLights();
			for (int i = 0; i < m_iAQuantity; i++)
			{
				m_spkScene.AttachLight(m_aspkALight[i]);
				m_acCaption = m_acCaption.concat("a");
			}
			for (int i = 0; i < m_iDQuantity; i++)
			{
				m_spkScene.AttachLight(m_aspkDLight[i]);
				m_acCaption = m_acCaption.concat("d");
			}
			for (int i = 0; i < m_iPQuantity; i++)
			{
				m_spkScene.AttachLight(m_aspkPLight[i]);
				m_acCaption = m_acCaption.concat("p");
			}
			for (int i = 0; i < m_iSQuantity; i++)
			{
				m_spkScene.AttachLight(m_aspkSLight[i]);
				m_acCaption = m_acCaption.concat("s");
			}
		}
		else
		{
			if (m_spkPlane.GetEffectQuantity() == 0)
			{
				// Release the vertex buffers since the default effect has a
				// different vertex layout than the lighting effects (the lights
				// require normal vectors).
				m_spkPlane.Reload(true);
				m_spkSphere.Reload(true);

				m_spkPlane.AttachEffect(m_spkDefaultEffect);
				m_spkSphere.AttachEffect(m_spkDefaultEffect);
			}
		}

		m_spkScene.UpdateRS();
		//m_iActiveLight = -1;
	}

}
