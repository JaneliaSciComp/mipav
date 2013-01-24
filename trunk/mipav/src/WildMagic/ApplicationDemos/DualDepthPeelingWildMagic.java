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
import java.awt.event.KeyListener;
import java.awt.event.WindowAdapter;
import java.awt.event.WindowEvent;
import java.io.File;

import javax.media.opengl.GLAutoDrawable;
import javax.media.opengl.GLEventListener;

import WildMagic.LibFoundation.Mathematics.ColorRGBA;
import WildMagic.LibFoundation.Mathematics.Vector3f;
import WildMagic.LibGraphics.Effects.MaterialTextureEffect;
import WildMagic.LibGraphics.Effects.ShaderEffect;
import WildMagic.LibGraphics.Rendering.AlphaState;
import WildMagic.LibGraphics.Rendering.Camera;
import WildMagic.LibGraphics.Rendering.GlobalState;
import WildMagic.LibGraphics.Rendering.GraphicsImage;
import WildMagic.LibGraphics.Rendering.MaterialState;
import WildMagic.LibGraphics.Rendering.Texture;
import WildMagic.LibGraphics.Rendering.ZBufferState;
import WildMagic.LibGraphics.SceneGraph.Attributes;
import WildMagic.LibGraphics.SceneGraph.IndexBuffer;
import WildMagic.LibGraphics.SceneGraph.Node;
import WildMagic.LibGraphics.SceneGraph.StandardMesh;
import WildMagic.LibGraphics.SceneGraph.TriMesh;
import WildMagic.LibGraphics.SceneGraph.VertexBuffer;
import WildMagic.LibRenderers.OpenGLRenderer.OpenGLFrameBuffer;
import WildMagic.LibRenderers.OpenGLRenderer.OpenGLRenderer;

import com.jogamp.opengl.util.Animator;

public class DualDepthPeelingWildMagic extends DemoBase
    implements GLEventListener, KeyListener
{

	private static final long serialVersionUID = -1807630810806956342L;
	/**
     * @param args
     */
    public static void main(String[] args) {
    	DualDepthPeelingWildMagic kWorld = new DualDepthPeelingWildMagic(); 
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

    private ZBufferState m_kZBuffer;

    private AlphaState m_kAlpha;

    private OpenGLFrameBuffer m_kFBO =null;
    

    protected ShaderEffect m_spkPlaneEffect;
    
    
	protected TriMesh m_pkPlane;

      
    protected Camera m_pkScreenCamera;

    private TriMesh m_kTorus;
    protected TriMesh m_kGround;

    protected TriMesh m_kDragon;
    
    private boolean m_bDisplay = false;
    private boolean m_bFirst = true;
    public DualDepthPeelingWildMagic()
    {
        super("DualDepthPeelingWildMagic",1024,768);
        
        
		LoadModel( "dragon.obj" );
    }

	@Override
	public void display(GLAutoDrawable arg0) {
    	if ( !m_bDisplay )
    	{
    		return;
    	}
    	
        ((OpenGLRenderer)m_pkRenderer).SetDrawable( arg0 );
		MeasureTime();
        
        m_pkRenderer.SetCamera( m_spkCamera );
        if (MoveCamera())
        {
            m_kCuller.ComputeVisibleSet(m_spkScene);
        }
        
        if (MoveObject())
        {
            m_spkScene.UpdateGS();
            m_kCuller.ComputeVisibleSet(m_spkScene);
        }
                
        if ( m_kFBO == null || m_bFirst )
        {
        	m_bFirst = false;
            CreateRenderTarget(m_iWidth, m_iHeight, arg0);
        }

		m_spkScene.DetachChild( m_kDragon );
		m_spkScene.DetachChild( m_kTorus );

        // First: render opaque objects to an off-screen color/depth buffer
        m_kFBO.Enable();      
        m_kFBO.DrawBuffers(new int[]{0});
        m_pkRenderer.SetBackgroundColor( new ColorRGBA(0.0f, 0.0f,1.0f,0.0f));
		m_pkRenderer.ClearBuffers();
        m_pkRenderer.Draw( m_kGround );               
        m_kFBO.Disable();
        

        // Second: render translucent objects, accumulating color and depth-complexity count.
        // Note: we share the depth buffer from rendering the opaque objects.
        // clear the color buffer, but NOT the depth buffer. 
        m_kFBO.Enable();
        m_kFBO.DrawBuffers(new int[]{1,2});
        m_pkRenderer.SetBackgroundColor( new ColorRGBA(0.0f, 0.0f,0.0f,0.0f));
        m_pkRenderer.ClearBackBuffer();
        m_spkScene.AttachGlobalState(m_kZBuffer);
        m_spkScene.AttachGlobalState(m_kAlpha);
		m_spkScene.AttachChild( m_kTorus );
		m_spkScene.AttachChild( m_kDragon );
        m_spkScene.UpdateGS();
        m_spkScene.UpdateRS();
        m_kCuller.ComputeVisibleSet(m_spkScene);
        m_pkRenderer.DrawScene(m_kCuller.GetVisibleSet());

        m_kFBO.Disable();
        
        m_spkScene.DetachGlobalState(GlobalState.StateType.ALPHA);
        m_spkScene.DetachGlobalState(GlobalState.StateType.ZBUFFER);
      
        
        // Render the screen-space quad that displays the final combined images:        
        m_pkRenderer.SetCamera(m_pkScreenCamera);
        m_pkRenderer.SetBackgroundColor( ColorRGBA.BLACK );
		m_pkRenderer.ClearBuffers();
        m_pkRenderer.Draw( m_pkPlane );               

		//DrawFrameRate(arg0, 8,GetHeight()-8,ColorRGBA.BLACK);
        // Swap buffers:
        m_pkRenderer.DisplayBackBuffer();
		UpdateFrameCount();
    }
	
    @Override
	public void dispose(GLAutoDrawable arg0) {

        ((OpenGLRenderer)m_pkRenderer).SetDrawable( arg0 );
		m_kFBO.SetDrawable(arg0);
		m_kFBO.TerminateBuffer();
		m_kFBO = null;

        m_pkRenderer.ReleaseResources( m_pkPlane );
        m_pkRenderer.ReleaseResources( m_kGround );
        m_pkRenderer.ReleaseResources( m_kTorus );
        m_pkRenderer.ReleaseResources( m_kDragon );
    }
    
    @Override
	public void init(GLAutoDrawable arg0) {
        ((OpenGLRenderer)m_pkRenderer).SetDrawable( arg0 );
        m_pkRenderer.InitializeState();
        super.OnInitialize();

        // set up camera
        m_spkCamera.SetFrustum(60.0f,m_iWidth/(float)m_iHeight,0.01f,100.0f);
        Vector3f kCLoc = new Vector3f(0.0f,0.0f,2.5f);
        Vector3f kCDir = new Vector3f(0.0f,0.0f,-1.0f);
        Vector3f kCUp = new Vector3f(0.0f,1.0f,0.0f);
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
        InitializeObjectMotion(m_spkScene);
        
        m_spkScene.DetachAllChildren();
    }

    @Override
	public void reshape(GLAutoDrawable arg0, int iX, int iY, int iWidth, int iHeight) { 
        ((OpenGLRenderer)m_pkRenderer).SetDrawable( arg0 );
        if ( m_iWidth != iWidth || m_iHeight != iHeight )
        {
        	if (iWidth > 0 && iHeight > 0)
        	{
        		m_bDisplay = false;
        		m_pkRenderer.Resize(iWidth,iHeight);
        		m_iWidth = iWidth;
        		m_iHeight = iHeight;
                m_spkCamera.SetFrustum(60.0f,m_iWidth/(float)m_iHeight,0.01f,100.0f);
        		CreateRenderTarget(m_iWidth, m_iHeight, arg0);
        		m_bDisplay = true;
        	}
        }
    }
    private void CreateRenderTarget( int iWidth, int iHeight, GLAutoDrawable kDrawable )
    {            	
    	if ( m_kFBO != null )
    	{
    		m_kFBO.SetDrawable(kDrawable);
    		m_kFBO.TerminateBuffer();
    		m_kFBO = null;

            m_pkRenderer.ReleaseResources( m_pkPlane );
    	}
    	System.err.println( "CreateRenderTarget" );
    	
        m_kFBO = new OpenGLFrameBuffer(m_eFormat,m_eDepth,m_eStencil,
                m_eBuffering,m_eMultisampling,m_pkRenderer, kDrawable);
        
        Texture[] akSceneTarget = new Texture[3];
        GraphicsImage pkSceneImage = new GraphicsImage(GraphicsImage.FormatMode.IT_RGBA32,iWidth,iHeight,(float[])null,
                "ColorTex0");
        akSceneTarget[0] = new Texture();
        akSceneTarget[0].SetImage(pkSceneImage);
        akSceneTarget[0].SetShared(true);
        akSceneTarget[0].SetFilterType(Texture.FilterType.NEAREST);
        akSceneTarget[0].SetWrapType(0,Texture.WrapType.CLAMP);
        akSceneTarget[0].SetWrapType(1,Texture.WrapType.CLAMP);
        m_pkRenderer.LoadTexture( akSceneTarget[0] );
        
        
        pkSceneImage = new GraphicsImage(GraphicsImage.FormatMode.IT_RGBA32,iWidth,iHeight,(float[])null,
        "ColorTex1");
        akSceneTarget[1] = new Texture();
        akSceneTarget[1].SetImage(pkSceneImage);
        akSceneTarget[1].SetShared(true);
        akSceneTarget[1].SetFilterType(Texture.FilterType.NEAREST);
        akSceneTarget[1].SetWrapType(0,Texture.WrapType.CLAMP);
        akSceneTarget[1].SetWrapType(1,Texture.WrapType.CLAMP);
        m_pkRenderer.LoadTexture( akSceneTarget[1] );
        
        
        pkSceneImage = new GraphicsImage(GraphicsImage.FormatMode.IT_RGBA32,iWidth,iHeight,(float[])null,
        "ColorTex2");
        akSceneTarget[2] = new Texture();
        akSceneTarget[2].SetImage(pkSceneImage);
        akSceneTarget[2].SetShared(true);
        akSceneTarget[2].SetFilterType(Texture.FilterType.NEAREST);
        akSceneTarget[2].SetWrapType(0,Texture.WrapType.CLAMP);
        akSceneTarget[2].SetWrapType(1,Texture.WrapType.CLAMP);
        m_pkRenderer.LoadTexture( akSceneTarget[2] );
        
        m_kFBO.InitializeBuffer(akSceneTarget);


        m_pkScreenCamera = new Camera();
        m_pkScreenCamera.Perspective = false;
        m_pkScreenCamera.SetFrustum(0.0f,1.0f,0.0f,1.0f,0.0f,1.0f);
        m_pkScreenCamera.SetFrame(Vector3f.ZERO,Vector3f.UNIT_Z,
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
		pkVBuffer.SetTCoord2(0,0,0.0f,0.0f);
		pkVBuffer.SetTCoord2(0,1,1.0f,0.0f);
		pkVBuffer.SetTCoord2(0,2,1.0f,1.0f);
		pkVBuffer.SetTCoord2(0,3,0.0f,1.0f);
		IndexBuffer pkIBuffer = new IndexBuffer(6);
		int[] aiIndex = pkIBuffer.GetData();
		aiIndex[0] = 0;  aiIndex[1] = 1;  aiIndex[2] = 2;
		aiIndex[3] = 0;  aiIndex[4] = 2;  aiIndex[5] = 3;
		m_pkPlane = new TriMesh(pkVBuffer,pkIBuffer); 
        
        m_spkPlaneEffect = new OrderIndpTransparencyEffect(akSceneTarget, ColorRGBA.WHITE );
        m_pkPlane.AttachEffect( m_spkPlaneEffect );
        m_pkPlane.UpdateGS();
        m_pkPlane.UpdateRS();

        m_pkRenderer.LoadResources( m_pkPlane );
    }
    

    private void CreateScene ()
    {
        
        m_spkScene = new Node();        
                

        m_kAlpha = new AlphaState();
        m_kAlpha.BlendEnabled = true;
        m_kAlpha.SrcBlend = AlphaState.SrcBlendMode.SBF_ONE;
        m_kAlpha.DstBlend = AlphaState.DstBlendMode.DBF_ONE;
                
        m_kZBuffer = new ZBufferState();
        m_kZBuffer.Enabled = false;
        m_kZBuffer.Writable = false;
        

		OrderIndpTransparencyEffect pkEffect = new OrderIndpTransparencyEffect(.8f);
		m_kDragon.AttachEffect(pkEffect);
        pkEffect.LoadResources(m_pkRenderer, m_kDragon);
		
        Attributes kAttr = new Attributes();
        kAttr.SetPChannels(3);
        kAttr.SetTChannels(0,2);
        StandardMesh kSM = new StandardMesh(kAttr);
        m_kTorus = kSM.Torus(64,64,1.0f,0.25f);

        pkEffect = new OrderIndpTransparencyEffect("leaf", 0.6f);
        m_kTorus.AttachEffect(pkEffect);
        pkEffect.LoadResources(m_pkRenderer, m_kTorus);
        

        // Create the ground.  It covers a square with vertices (1,1,0), (1,-1,0),
        // (-1,1,0), and (-1,-1,0).  Multiply the texture coordinates by a factor
        // to enhance the wrap-around.
        kAttr = new Attributes();
        kAttr.SetPChannels(3);
        kAttr.SetTChannels(0,2);
        kSM = new StandardMesh(kAttr);
        m_kGround = kSM.Rectangle(2,2,10.0f,10.0f);
        m_kGround.Local.SetTranslate( 0, 0, -4 );
        MaterialTextureEffect pkGroundEffect = new MaterialTextureEffect("Horizontal");
        MaterialState kMaterial = new MaterialState();
        kMaterial.Diffuse.Set( 0.2f, 0.2f, 0.2f );
        m_kGround.AttachGlobalState(kMaterial);
        m_kGround.AttachEffect(pkGroundEffect);
        m_kGround.UpdateRS();
        m_kGround.UpdateGS();
        pkGroundEffect.LoadResources(m_pkRenderer, m_kGround);
    }


	void LoadModel( String model_filename)
	{
		Model g_model = new Model();
		System.err.println("loading OBJ...\n");
		
		String dir = new String( "WildMagic" + File.separator + "ApplicationDemos" + File.separator + "media" + File.separator + "models" + File.separator ); 
		
		g_model.loadModelFromFile( dir + model_filename );

		System.err.println("compiling mesh...\n");
		m_kDragon = g_model.compileModelVB();
		System.err.println("...done\n");

		float[] modelMin = new float[3];
		float[] modelMax = new float[3];
		g_model.computeBoundingBox(modelMin, modelMax);

		float[] diag = new float[]{ modelMax[0] - modelMin[0],
				modelMax[1] - modelMin[1],
				modelMax[2] - modelMin[2] };
		float g_bbScale = (float)(1.0 / Math.sqrt(diag[0]*diag[0] + diag[1]*diag[1] + diag[2]*diag[2]) * 1.5);
		float[] g_bbTrans = new float[]{ (float)( -g_bbScale * (modelMin[0] + 0.5 * diag[0])), 
				(float)( -g_bbScale * (modelMin[1] + 0.5 * diag[1]) ), 
				(float)( -g_bbScale * (modelMin[2] + 0.5 * diag[2]) ) };
		

		m_kDragon.Local.SetUniformScale( 2*g_bbScale );
		m_kDragon.Local.SetTranslate( 2*g_bbTrans[0], 2*g_bbTrans[1], 2*g_bbTrans[2] );
	}

    
}
