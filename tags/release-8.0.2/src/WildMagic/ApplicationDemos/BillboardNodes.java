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

import WildMagic.LibFoundation.Mathematics.ColorRGB;
import WildMagic.LibFoundation.Mathematics.ColorRGBA;
import WildMagic.LibFoundation.Mathematics.Mathf;
import WildMagic.LibFoundation.Mathematics.Matrix3f;
import WildMagic.LibFoundation.Mathematics.Vector3f;
import WildMagic.LibGraphics.Detail.BillboardNode;
import WildMagic.LibGraphics.Effects.MaterialTextureEffect;
import WildMagic.LibGraphics.Effects.ShaderEffect;
import WildMagic.LibGraphics.Rendering.AlphaState;
import WildMagic.LibGraphics.Rendering.Camera;
import WildMagic.LibGraphics.Rendering.GlobalState;
import WildMagic.LibGraphics.Rendering.GraphicsImage;
import WildMagic.LibGraphics.Rendering.MaterialState;
import WildMagic.LibGraphics.Rendering.Texture;
import WildMagic.LibGraphics.Rendering.WireframeState;
import WildMagic.LibGraphics.Rendering.ZBufferState;
import WildMagic.LibGraphics.SceneGraph.Attributes;
import WildMagic.LibGraphics.SceneGraph.Culler;
import WildMagic.LibGraphics.SceneGraph.IndexBuffer;
import WildMagic.LibGraphics.SceneGraph.Node;
import WildMagic.LibGraphics.SceneGraph.StandardMesh;
import WildMagic.LibGraphics.SceneGraph.TriMesh;
import WildMagic.LibGraphics.SceneGraph.VertexBuffer;
import WildMagic.LibRenderers.OpenGLRenderer.OpenGLFrameBuffer;
import WildMagic.LibRenderers.OpenGLRenderer.OpenGLRenderer;

import com.jogamp.opengl.util.Animator;

public class BillboardNodes extends DemoBase
    implements GLEventListener, KeyListener
{

	private static final long serialVersionUID = 3590795262168686102L;
	/**
     * @param args
     */
    public static void main(String[] args) {
    	BillboardNodes kWorld = new BillboardNodes();
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


    private Node m_spkScene;

    private ZBufferState m_kZBuffer;

    private AlphaState m_kAlpha;
    

    private WireframeState m_spkWireframe;

    private Culler m_kCuller = new Culler(0,0,null);


    private BillboardNode m_spkBillboard0, m_spkBillboard1;

    private OpenGLFrameBuffer m_kFBO;
    protected ShaderEffect m_spkPlaneEffect;
    protected TriMesh m_pkPlane;
    protected Camera m_pkScreenCamera;
    protected TriMesh m_kGround;
    private boolean m_bDisplay = false;

    private boolean m_bFirst = true;
    public BillboardNodes()
    {
        super("BillboardNodes");
    }
    
    @Override
	public void display(GLAutoDrawable arg0) {
    	if ( !m_bDisplay )
    	{
    		return;
    	}
    	
        ((OpenGLRenderer)m_pkRenderer).SetDrawable( arg0 );
        m_pkRenderer.SetCamera( m_spkCamera );
        if (MoveCamera())
        {
            m_spkBillboard0.UpdateGS();
            m_spkBillboard1.UpdateGS();
            m_kCuller.ComputeVisibleSet(m_spkScene);
        }
        
        if (MoveObject())
        {
            m_kCuller.ComputeVisibleSet(m_spkScene);
        } 
        
        if ( m_kFBO == null || m_bFirst )
        {
        	m_bFirst = false;
            CreateRenderTarget(m_iWidth, m_iHeight, arg0);
        }
        
        // First: render opaque objects to an off-screen color/depth buffer
        m_kFBO.Enable();
        m_kFBO.DrawBuffers(new int[]{0});
        m_pkRenderer.SetBackgroundColor( ColorRGBA.WHITE );
        m_pkRenderer.ClearBuffers();

        m_spkScene.DetachGlobalState(GlobalState.StateType.ALPHA);
        m_spkScene.DetachGlobalState(GlobalState.StateType.ZBUFFER);
        m_spkScene.DetachChild(m_spkBillboard0);
        m_spkScene.DetachChild(m_spkBillboard1);
        m_spkScene.UpdateGS();
        m_spkScene.UpdateRS();
        m_kCuller.ComputeVisibleSet(m_spkScene);
        m_pkRenderer.DrawScene(m_kCuller.GetVisibleSet());        
        m_kFBO.Disable();
        
        // Second: render translucent objects, accumulating color and depth-complexity count.
        // Note: we share the depth buffer from rendering the opaque objects.
        // clear the color buffer, but NOT the depth buffer. 
        m_kFBO.Enable();
        m_kFBO.DrawBuffers(new int[]{1,2});
        m_pkRenderer.SetBackgroundColor( new ColorRGBA(0.0f, 0.0f,0.0f,0.0f));
        m_pkRenderer.ClearBackBuffer();
        m_kZBuffer.Writable = false;
        m_spkScene.AttachGlobalState(m_kZBuffer);
        m_spkScene.AttachGlobalState(m_kAlpha);
        m_spkScene.AttachChild(m_spkBillboard0);
        m_spkScene.AttachChild(m_spkBillboard1);
        m_spkScene.DetachChild( m_kGround );
        m_spkScene.UpdateGS();
        m_spkScene.UpdateRS();
        m_kCuller.ComputeVisibleSet(m_spkScene);
        m_pkRenderer.DrawScene(m_kCuller.GetVisibleSet());
        
        m_kFBO.Disable();

        m_spkScene.AttachChild( m_kGround );
        
        
        // Last: render screen-quad, which 
        m_spkScene.DetachGlobalState(GlobalState.StateType.ALPHA);
        m_spkScene.DetachGlobalState(GlobalState.StateType.ZBUFFER);

        m_pkRenderer.SetCamera(m_pkScreenCamera);
        m_pkRenderer.SetBackgroundColor( ColorRGBA.WHITE );
        m_pkRenderer.ClearBuffers();
        m_pkRenderer.Draw( m_pkPlane );        
        m_pkRenderer.DisplayBackBuffer();
    }
        
    @Override
	public void dispose(GLAutoDrawable arg0) 
    {
        ((OpenGLRenderer)m_pkRenderer).SetDrawable( arg0 );
		m_kFBO.SetDrawable(arg0);
		m_kFBO.TerminateBuffer();
		m_kFBO = null;

        m_pkRenderer.ReleaseResources( m_pkPlane );
        m_pkRenderer.ReleaseResources( m_kGround );
        m_spkBillboard0 = null;
        m_spkBillboard1 = null;
        
        super.dispose();
    }
    
    @Override
	public GLCanvas GetCanvas()
    {
        return ((OpenGLRenderer)m_pkRenderer).GetCanvas();
    }

    @Override
	public void init(GLAutoDrawable arg0) {
        ((OpenGLRenderer)m_pkRenderer).SetDrawable( arg0 );
        m_pkRenderer.InitializeState();
        super.OnInitialize();

        // set up camera
        m_spkCamera.SetFrustum(-0.055f,0.055f,-0.04125f,0.04125f,0.1f,100.0f);
        Vector3f kCLoc = new Vector3f(0.0f,-1.0f,0.25f);
        Vector3f kCDir = new Vector3f(0.0f,1.0f,0.0f);
        Vector3f kCUp = new Vector3f(0.0f,0.0f,1.0f);
        Vector3f kCRight = Vector3f.cross( kCDir, kCUp );
        m_spkCamera.SetFrame(kCLoc,kCDir,kCUp,kCRight);
        
        
        m_pkScreenCamera = new Camera();
        m_pkScreenCamera.Perspective = false;
        m_pkScreenCamera.SetFrustum(-1,1,-1,1,1f,10.0f);

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
	public void keyPressed(KeyEvent e) {
        char ucKey = e.getKeyChar();
        
        super.keyPressed(e);

        if (ucKey == 'w' || ucKey == 'W')
        {
            m_spkWireframe.Enabled = !m_spkWireframe.Enabled;
            return;
        }
        else if ( ucKey == 'a' )
        {
            ((OrderIndpTransparencyEffect)m_spkBillboard0.GetChild(0).GetEffect(0)).SetAlpha(0.8f);
        }
        else if ( ucKey == 'A' )
        {
            ((OrderIndpTransparencyEffect)m_spkBillboard0.GetChild(0).GetEffect(0)).SetAlpha(0.2f);
        }
        else if (ucKey == 's' || ucKey == 'S')
        {
            TestStreaming(m_spkScene,"BillboardNodes.wmof");
            return;
        }
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
        m_spkWireframe = new WireframeState();
        m_spkScene.AttachGlobalState(m_spkWireframe);
       
        
        m_kAlpha = new AlphaState();
        m_kAlpha.BlendEnabled = true;
        m_kAlpha.SrcBlend = AlphaState.SrcBlendMode.SBF_ONE;
        m_kAlpha.DstBlend = AlphaState.DstBlendMode.DBF_ONE;
        m_spkScene.AttachGlobalState(m_kAlpha);
        
        m_kZBuffer = new ZBufferState();
        m_kZBuffer.Enabled = true;
        m_kZBuffer.Writable = false;
        m_spkScene.AttachGlobalState(m_kZBuffer);

        // Create the ground.  It covers a square with vertices (1,1,0), (1,-1,0),
        // (-1,1,0), and (-1,-1,0).
        Attributes kAttr = new Attributes();
        kAttr.SetPChannels(3);
        kAttr.SetTChannels(0,2);
        kAttr.SetCChannels(0,3);
        StandardMesh kSM = new StandardMesh(kAttr);
        m_kGround = kSM.Rectangle(2,2,16.0f,16.0f);
        MaterialTextureEffect pkGroundEffect = new MaterialTextureEffect("Horizontal");
        MaterialState kMaterial = new MaterialState();
        kMaterial.Diffuse.Set( 0.2f, 0.2f, 0.2f );
        m_kGround.AttachGlobalState(kMaterial);
        m_kGround.AttachEffect(pkGroundEffect);
        m_kGround.UpdateRS();
        pkGroundEffect.LoadResources(m_pkRenderer, m_kGround);
        m_spkScene.AttachChild(m_kGround);

        // Create a billboard node that causes a rectangle to always be facing
        // the camera.  This is the type of billboard for an avatar.
        m_spkBillboard0 = new BillboardNode(m_spkCamera);
        m_spkScene.AttachChild(m_spkBillboard0);

        // Create a rectangle mesh.  The mesh is in the xy-plane.  Do not apply
        // local transformations to the mesh.  Use the billboard node transforms
        // to control the mesh location and orientation.
        TriMesh pkMesh = kSM.Rectangle(2,2,0.125f,0.25f);

        OrderIndpTransparencyEffect pkEffect = new OrderIndpTransparencyEffect("Leaf", 0.2f);
        pkMesh.AttachGlobalState(kMaterial);
        pkMesh.AttachEffect(pkEffect);
        for ( int i = 0; i < pkMesh.VBuffer.GetVertexQuantity(); i++ )
        {
        	pkMesh.VBuffer.SetColor3(0,  i, ColorRGB.BLACK );
        }
        pkEffect.LoadResources(m_pkRenderer, pkMesh);
        m_spkBillboard0.AttachChild(pkMesh);

        // The billboard rotation is about its model-space up-vector (0,1,0).  In
        // this application, world-space up is (0,0,1).  Locally rotate the
        // billboard so it's up-vector matches the world's.
        m_spkBillboard0.Local.SetTranslate( -0.25f,0.0f,0.1f);
        Matrix3f kMatrix = new Matrix3f();
        kMatrix.fromAxisAngle(Vector3f.UNIT_X,Mathf.HALF_PI);
        m_spkBillboard0.Local.SetRotate( kMatrix );

        // Create a billboard node that causes an object to always be oriented
        // the same way relative to the camera.
        m_spkBillboard1 = new BillboardNode(m_spkCamera);
        m_spkScene.AttachChild(m_spkBillboard1);

        // Create a torus mesh.  Do not apply local transformations to the mesh.
        // Use the billboard node transforms to control the mesh location and
        // orientation.
        pkMesh = kSM.Torus(16,16,1.0f,0.25f);
        pkMesh.Local.SetUniformScale(0.1f);

        pkEffect = new OrderIndpTransparencyEffect("RedSky", 0.9f);
        pkMesh.AttachGlobalState(kMaterial);
        pkMesh.AttachEffect(pkEffect);
        pkEffect.LoadResources(m_pkRenderer, pkMesh);
        m_spkBillboard1.AttachChild(pkMesh);

        // The billboard rotation is about its model-space up-vector (0,1,0).  In
        // this application, world-space up is (0,0,1).  Locally rotate the
        // billboard so it's up-vector matches the world's.
        m_spkBillboard1.Local.SetTranslate( 0.25f,0.0f,0.1f);
        Matrix3f kMatrix2 = new Matrix3f();
        kMatrix2.fromAxisAngle(Vector3f.UNIT_X,Mathf.HALF_PI);
        m_spkBillboard1.Local.SetRotate(kMatrix2);
    }    
}
