

// Translated from C++ Version see below:
//--------------------------------------------------------------------------------------
// Order Independent Transparency with Dual Depth Peeling
//
// Author: Louis Bavoil
// Email: sdkfeedback@nvidia.com
//
// Depth peeling is traditionally used to perform order independent transparency (OIT)
// with N geometry passes for N transparency layers. Dual depth peeling enables peeling
// N transparency layers in N/2+1 passes, by peeling from the front and the back
// simultaneously using a min-max depth buffer. This sample performs either normal or
// dual depth peeling and blends on the fly.
//
// Copyright (c) NVIDIA Corporation. All rights reserved.
//--------------------------------------------------------------------------------------


package WildMagic.ApplicationDemos;

import java.awt.Frame;
import java.awt.event.KeyEvent;
import java.awt.event.KeyListener;
import java.awt.event.MouseEvent;
import java.awt.event.MouseListener;
import java.awt.event.MouseMotionListener;
import java.awt.event.WindowAdapter;
import java.awt.event.WindowEvent;
import java.io.File;

import com.jogamp.opengl.GL;
import com.jogamp.opengl.GL2;
import com.jogamp.opengl.GL2ES2;
import com.jogamp.opengl.GL2GL3;
import com.jogamp.opengl.GLAutoDrawable;
import com.jogamp.opengl.GLCapabilities;
import com.jogamp.opengl.GLEventListener;
import com.jogamp.opengl.GLException;
import com.jogamp.opengl.GLProfile;
import com.jogamp.opengl.awt.GLCanvas;
import com.jogamp.opengl.fixedfunc.GLLightingFunc;
import com.jogamp.opengl.fixedfunc.GLMatrixFunc;
import com.jogamp.opengl.fixedfunc.GLPointerFunc;
import com.jogamp.opengl.glu.GLU;

import com.jogamp.common.nio.Buffers;
import com.jogamp.opengl.util.Animator;

public class DualDepthPeeling implements GLEventListener, KeyListener, MouseListener, MouseMotionListener
{
	public final static int DUAL_PEELING_MODE = 0;
	public final static int F2B_PEELING_MODE = 1;
	public final static int WEIGHTED_AVERAGE_MODE = 2;
	public final static int WEIGHTED_SUM_MODE = 3;

	public final static float FOVY = 30.0f;
	public final static float ZNEAR = 0.0001f;
	public final static float ZFAR = 10.0f;
	public final static float FPS_TIME_WINDOW = 1;
	public final static float MAX_DEPTH = 1.0f;

	public static void main(String[] args) {
		System.out.println("dual_depth_peeling - sample comparing multiple order independent transparency techniques\n");
		System.out.println("  Commands:\n");
		System.out.println("     A/D       - Change uniform opacity\n");
		System.out.println("     1         - Dual peeling mode\n");
		System.out.println("     2         - Front to back peeling mode\n");
		System.out.println("     3         - Weighted average mode\n");
		System.out.println("     4         - Weighted sum mode\n");
		System.out.println("     R         - Reload all shaders\n");
		System.out.println("     B         - Change background color\n");
		System.out.println("     Q         - Toggle occlusion queries\n");
		System.out.println("     +/-       - Change number of geometry passes\n\n");


		DualDepthPeeling kWorld = new DualDepthPeeling();
		Frame frame = new Frame("Dual Depth Peeling");
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
	public int g_numPasses = 4;
	public int g_imageWidth = 1024;

	public int g_imageHeight = 768;
	public Model g_model;
	public int g_quadDisplayList;
	public int[] g_vboId = new int[1];

	public int[] g_eboId = new int[1];
	public boolean g_useOQ = true;

	public int[] g_queryId = new int[1];
	public String MODEL_FILENAME = new String( "dragon.obj" );

	public String SHADER_PATH = new String( "dual_depth_peeling/shaders/" );
	public GLSLProgramObject g_shaderDualInit;
	public GLSLProgramObject g_shaderDualPeel;
	public GLSLProgramObject g_shaderDualBlend;

	public GLSLProgramObject g_shaderDualFinal;
	public GLSLProgramObject g_shaderFrontInit;
	public GLSLProgramObject g_shaderFrontPeel;
	public GLSLProgramObject g_shaderFrontBlend;

	public GLSLProgramObject g_shaderFrontFinal;
	public GLSLProgramObject g_shaderAverageInit;

	public GLSLProgramObject g_shaderAverageFinal;
	public GLSLProgramObject g_shaderWeightedSumInit;

	public GLSLProgramObject g_shaderWeightedSumFinal;
	public float[] g_opacity = new float[]{0.6f};
	public char g_mode = WEIGHTED_AVERAGE_MODE;
	public boolean g_showOsd = true;
	public boolean g_bShowUI = true;

	public int g_numGeoPasses = 0;
	public boolean g_rotating = false;
	public boolean g_panning = false;
	public boolean g_scaling = false;
	public int g_oldX, g_oldY;
	public int g_newX, g_newY;
	public float g_bbScale = 1.0f;
	public float[] g_bbTrans = new float[]{0.0f, 0.0f, 0.0f};
	public float[] g_rot = new float[]{0.0f, 45.0f};

	public float[] g_pos = new float[]{0.0f, 0.0f, 2.0f};
	float[] g_white = new float[]{1.0f,1.0f,1.0f};
	float[] g_black = new float[]{0.0f};
	float[] g_backgroundColor = g_white;
	float[] g_FragWidth = new float[]{g_imageWidth};

	float[] g_FragHeight = new float[]{g_imageHeight};
	public int[]  g_dualBackBlenderFboId = new int[1];
	public int[]  g_dualPeelingSingleFboId = new int[1];
	public int[]  g_dualDepthTexId = new int[2];
	public int[]  g_dualFrontBlenderTexId = new int[2];
	public int[]  g_dualBackTempTexId = new int[2];

	public int[]  g_dualBackBlenderTexId = new int[1];
	public int[]  g_frontFboId = new int[2];
	public int[]  g_frontDepthTexId = new int[2];
	public int[]  g_frontColorTexId = new int[2];
	public int[]  g_frontColorBlenderTexId = new int[1];

	public int[]  g_frontColorBlenderFboId = new int[1];
	public int[] g_accumulationTexId = new int[2];

	public int[] g_accumulationFboId = new int[1];

	int g_drawBuffers[] = {GL.GL_COLOR_ATTACHMENT0,
			GL2GL3.GL_COLOR_ATTACHMENT1,
			GL2GL3.GL_COLOR_ATTACHMENT2,
			GL2GL3.GL_COLOR_ATTACHMENT3,
			GL2GL3.GL_COLOR_ATTACHMENT4,
			GL2GL3.GL_COLOR_ATTACHMENT5,
			GL2GL3.GL_COLOR_ATTACHMENT6
	};

	/** GLCanvas for Java/JOGL */
	private GLCanvas m_kCanvas;


	/** GL object from GLCanvas.getGL() used to access openGL calls. */
	private GL2 m_kGL = null;

	private boolean m_bShared = false;

	private boolean m_bReloadModel = false;

	public DualDepthPeeling()
	{
		InitGL();	
	}

	public DualDepthPeeling( GLCanvas canvas )
	{
		m_kCanvas = canvas;
		m_kCanvas.setSize(g_imageWidth, g_imageHeight);
		m_kCanvas.addGLEventListener( this );       
		m_kCanvas.addKeyListener( this );       
		m_kCanvas.addMouseListener( this );       
		m_kCanvas.addMouseMotionListener( this );    
	}

	@Override
	public void display(GLAutoDrawable drawable) {	
		m_kGL = drawable.getGL().getGL2();		
		GLU glu = GLU.createGLU(m_kGL);
		

		if ( !m_bShared && m_bReloadModel )
		{
			m_bReloadModel = false;

	        m_kGL.glDeleteBuffers(1,g_vboId,0);
	        m_kGL.glDeleteBuffers(1,g_eboId,0);
			
			LoadModel(m_kGL, MODEL_FILENAME);
		}
		
		g_numGeoPasses = 0;

		m_kGL.glMatrixMode(GLMatrixFunc.GL_MODELVIEW);
		m_kGL.glLoadIdentity();
		glu.gluLookAt(g_pos[0], g_pos[1], g_pos[2], g_pos[0], g_pos[1], 0, 0, 1, 0);
		m_kGL.glRotatef(g_rot[0], 1, 0, 0);
		m_kGL.glRotatef(g_rot[1], 0, 1, 0);
		m_kGL.glTranslatef(g_bbTrans[0], g_bbTrans[1], g_bbTrans[2]);
		m_kGL.glScalef(g_bbScale, g_bbScale, g_bbScale);

		switch (g_mode) {
		case DUAL_PEELING_MODE:
			RenderDualPeeling(m_kGL);
			break;
		case F2B_PEELING_MODE:
			RenderFrontToBackPeeling(m_kGL);
			break;
		case WEIGHTED_AVERAGE_MODE:
			RenderAverageColors(m_kGL);
			break;
		case WEIGHTED_SUM_MODE:
			RenderWeightedSum(m_kGL);
			break;
		}

        /* Call swapBuffers to render on-screen: */
		drawable.swapBuffers();
	}

	@Override
	public void dispose(GLAutoDrawable arg0) {
		// TODO Auto-generated method stub

	}

	public GLCanvas GetCanvas()
    {
        return m_kCanvas;
    }

	public Model getModel()
	{
		return g_model;
	}
	
	public int getModelEB( )
	{
		return g_eboId[0];
	}
	
	public int getModelVB( )
	{
		return g_vboId[0];
	}
	
	@Override
	public void init(GLAutoDrawable drawable) {
		System.err.println( "init" );
		//GL gl = drawable.setGL(new DebugGL2(drawable.getGL().getGL2()));
		//m_kGL = gl.getGL2();
		//GL gl = drawable.setGL(new TraceGL2(drawable.getGL().getGL2(), System.err));
		m_kGL = drawable.getGL().getGL2();		
		drawable.setAutoSwapBufferMode( false );



		// Allocate render targets first
		try {
			InitDualPeelingRenderTargets(m_kGL);
		} catch ( GLException e )
		{
			try {
				InitDualPeelingRenderTargets(m_kGL);
			} catch ( GLException e1 )
			{
				System.err.println( e1.getStackTrace() );
			}
		}
		InitFrontPeelingRenderTargets(m_kGL);
		InitAccumulationRenderTargets(m_kGL);
		
		m_kGL.glBindFramebuffer(GL.GL_FRAMEBUFFER, 0);

		BuildShaders(m_kGL);
		if ( !m_bShared )
		{
			LoadModel(m_kGL, MODEL_FILENAME);
		}
		MakeFullScreenQuad(m_kGL);

		m_kGL.glDisable(GL.GL_CULL_FACE);
		m_kGL.glDisable(GLLightingFunc.GL_LIGHTING);
		m_kGL.glDisable(GLLightingFunc.GL_NORMALIZE);

		m_kGL.glGenQueries(1, g_queryId, 0);
	}
	
	public void InitDualPeelingRenderTargets(GL2 gl)
	{
		gl.glGenTextures(2, g_dualDepthTexId, 0);
		gl.glGenTextures(2, g_dualFrontBlenderTexId, 0);
		gl.glGenTextures(2, g_dualBackTempTexId, 0);
		gl.glGenFramebuffers(1, g_dualPeelingSingleFboId, 0);
		for (int i = 0; i < 2; i++)
		{
			gl.glBindTexture( GL2GL3.GL_TEXTURE_RECTANGLE, g_dualDepthTexId[i]);
			gl.glTexParameteri( GL2GL3.GL_TEXTURE_RECTANGLE,  GL.GL_TEXTURE_WRAP_S,  GL2.GL_CLAMP);
			gl.glTexParameteri( GL2GL3.GL_TEXTURE_RECTANGLE,  GL.GL_TEXTURE_WRAP_T,  GL2.GL_CLAMP);
			gl.glTexParameteri( GL2GL3.GL_TEXTURE_RECTANGLE,  GL.GL_TEXTURE_MIN_FILTER,  GL.GL_NEAREST);
			gl.glTexParameteri( GL2GL3.GL_TEXTURE_RECTANGLE,  GL.GL_TEXTURE_MAG_FILTER,  GL.GL_NEAREST);
			
			//gl.glEnable( GL2.GL_PIXEL_UNPACK_BUFFER );
			
			gl.glTexImage2D( GL2GL3.GL_TEXTURE_RECTANGLE, 0,  GL2.GL_FLOAT_RG32_NV, g_imageWidth, g_imageHeight,
					0,  GL.GL_RGB,  GL.GL_FLOAT, null);

			gl.glBindTexture( GL2GL3.GL_TEXTURE_RECTANGLE, g_dualFrontBlenderTexId[i]);
			gl.glTexParameteri( GL2GL3.GL_TEXTURE_RECTANGLE,  GL.GL_TEXTURE_WRAP_S, GL2.GL_CLAMP);
			gl.glTexParameteri( GL2GL3.GL_TEXTURE_RECTANGLE,  GL.GL_TEXTURE_WRAP_T, GL2.GL_CLAMP);
			gl.glTexParameteri( GL2GL3.GL_TEXTURE_RECTANGLE,  GL.GL_TEXTURE_MIN_FILTER,  GL.GL_NEAREST);
			gl.glTexParameteri( GL2GL3.GL_TEXTURE_RECTANGLE,  GL.GL_TEXTURE_MAG_FILTER,  GL.GL_NEAREST);
			gl.glTexImage2D( GL2GL3.GL_TEXTURE_RECTANGLE, 0,  GL.GL_RGBA, g_imageWidth, g_imageHeight,
					0,  GL.GL_RGBA,  GL.GL_FLOAT, null);

			gl.glBindTexture( GL2GL3.GL_TEXTURE_RECTANGLE, g_dualBackTempTexId[i]);
			gl.glTexParameteri( GL2GL3.GL_TEXTURE_RECTANGLE,  GL.GL_TEXTURE_WRAP_S, GL2.GL_CLAMP);
			gl.glTexParameteri( GL2GL3.GL_TEXTURE_RECTANGLE,  GL.GL_TEXTURE_WRAP_T, GL2.GL_CLAMP);
			gl.glTexParameteri( GL2GL3.GL_TEXTURE_RECTANGLE,  GL.GL_TEXTURE_MIN_FILTER,  GL.GL_NEAREST);
			gl.glTexParameteri( GL2GL3.GL_TEXTURE_RECTANGLE,  GL.GL_TEXTURE_MAG_FILTER,  GL.GL_NEAREST);
			gl.glTexImage2D( GL2GL3.GL_TEXTURE_RECTANGLE, 0,  GL.GL_RGBA, g_imageWidth, g_imageHeight,
					0,  GL.GL_RGBA,  GL.GL_FLOAT, null);
		}

		gl.glGenTextures(1, g_dualBackBlenderTexId, 0);
		gl.glBindTexture( GL2GL3.GL_TEXTURE_RECTANGLE, g_dualBackBlenderTexId[0]);
		gl.glTexParameteri( GL2GL3.GL_TEXTURE_RECTANGLE,  GL.GL_TEXTURE_WRAP_S, GL2.GL_CLAMP);
		gl.glTexParameteri( GL2GL3.GL_TEXTURE_RECTANGLE,  GL.GL_TEXTURE_WRAP_T, GL2.GL_CLAMP);
		gl.glTexParameteri( GL2GL3.GL_TEXTURE_RECTANGLE,  GL.GL_TEXTURE_MIN_FILTER,  GL.GL_NEAREST);
		gl.glTexParameteri( GL2GL3.GL_TEXTURE_RECTANGLE,  GL.GL_TEXTURE_MAG_FILTER,  GL.GL_NEAREST);
		gl.glTexImage2D( GL2GL3.GL_TEXTURE_RECTANGLE, 0, GL.GL_RGB, g_imageWidth, g_imageHeight,
				0, GL.GL_RGB, GL.GL_FLOAT, null);

		gl.glGenFramebuffers(1, g_dualBackBlenderFboId, 0);
		gl.glBindFramebuffer( GL.GL_FRAMEBUFFER, g_dualBackBlenderFboId[0]);
		gl.glFramebufferTexture2D( GL.GL_FRAMEBUFFER, GL.GL_COLOR_ATTACHMENT0,
				GL2GL3.GL_TEXTURE_RECTANGLE, g_dualBackBlenderTexId[0], 0);

		gl.glBindFramebuffer( GL.GL_FRAMEBUFFER, g_dualPeelingSingleFboId[0]);

		int j = 0;
		gl.glFramebufferTexture2D( GL.GL_FRAMEBUFFER,  GL.GL_COLOR_ATTACHMENT0,
				GL2GL3.GL_TEXTURE_RECTANGLE, g_dualDepthTexId[j], 0);
		gl.glFramebufferTexture2D( GL.GL_FRAMEBUFFER,  GL2GL3.GL_COLOR_ATTACHMENT1,
				GL2GL3.GL_TEXTURE_RECTANGLE, g_dualFrontBlenderTexId[j], 0);
		gl.glFramebufferTexture2D( GL.GL_FRAMEBUFFER,  GL2GL3.GL_COLOR_ATTACHMENT2,
				GL2GL3.GL_TEXTURE_RECTANGLE, g_dualBackTempTexId[j], 0);

		j = 1;
		gl.glFramebufferTexture2D( GL.GL_FRAMEBUFFER,  GL2GL3.GL_COLOR_ATTACHMENT3,
				GL2GL3.GL_TEXTURE_RECTANGLE, g_dualDepthTexId[j], 0);
		gl.glFramebufferTexture2D( GL.GL_FRAMEBUFFER,  GL2GL3.GL_COLOR_ATTACHMENT4,
				GL2GL3.GL_TEXTURE_RECTANGLE, g_dualFrontBlenderTexId[j], 0);
		gl.glFramebufferTexture2D( GL.GL_FRAMEBUFFER,  GL2GL3.GL_COLOR_ATTACHMENT5,
				GL2GL3.GL_TEXTURE_RECTANGLE, g_dualBackTempTexId[j], 0);

		gl.glFramebufferTexture2D( GL.GL_FRAMEBUFFER,  GL2GL3.GL_COLOR_ATTACHMENT6,
				GL2GL3.GL_TEXTURE_RECTANGLE, g_dualBackBlenderTexId[0], 0);
	}
	
	
    @Override
	public void keyPressed(KeyEvent e) {
		// TODO Auto-generated method stub

	}

	@Override
	public void keyReleased(KeyEvent e) {
		switch(e.getKeyChar())
		{
		case 8:
			g_bShowUI = !g_bShowUI;
			break;
		case 'q':
			g_useOQ = !g_useOQ;
			break;
		case '+':
			g_numPasses++;
			break;
		case '-':
			g_numPasses--;
			break;
		case 'b':
			g_backgroundColor = (g_backgroundColor == g_white) ? g_black : g_white;
			break;
		case 'o':
			g_showOsd = !g_showOsd;
			break;
		case 'r':
			ReloadShaders(m_kGL);
			break;
		case '1':
			g_mode = DUAL_PEELING_MODE;
			break;
		case '2':
			g_mode = F2B_PEELING_MODE;
			break;
		case '3':
			g_mode = WEIGHTED_AVERAGE_MODE;
			break;
		case '4':
			g_mode = WEIGHTED_SUM_MODE;
			break;
		case 'a':
			g_opacity[0] -= 0.05;
			g_opacity[0] = (float)Math.max(g_opacity[0], 0.0);
			break;
		case 'd':
			g_opacity[0] += 0.05;
			g_opacity[0] = (float)Math.min(g_opacity[0], 1.0);
			break;
		case 'u':
			m_bReloadModel = true;
			break;
		}
	}

	@Override
	public void keyTyped(KeyEvent e) {
		// TODO Auto-generated method stub

	}

	@Override
	public void mouseClicked(MouseEvent e) {}

	@Override
	public void mouseDragged(MouseEvent e) {

		g_oldX = g_newX; g_oldY = g_newY;
		g_newX = e.getX();
		g_newY = e.getY();

		float rel_x = (g_newX - g_oldX) / (float)g_imageWidth;
		float rel_y = (g_newY - g_oldY) / (float)g_imageHeight;
		if (g_rotating)
		{
			g_rot[1] += (rel_x * 180);
			g_rot[0] += (rel_y * 180);
		}
		else if (g_panning)
		{
			g_pos[0] -= rel_x;
			g_pos[1] += rel_y;
		}
		else if (g_scaling)
		{
			g_pos[2] -= rel_y * g_pos[2];
		}
	
	}

	@Override
	public void mouseEntered(MouseEvent e) {}

	@Override
	public void mouseExited(MouseEvent e) {}
	
	@Override
	public void mouseMoved(MouseEvent e) {}
	
	@Override
	public void mousePressed(MouseEvent e) {

		g_newX = e.getX();
		g_newY = e.getY();

		g_scaling = false;
		g_panning = false;
		g_rotating = false;
		
		if (e.getButton() == MouseEvent.BUTTON1)
		{
			if (e.isShiftDown()) {
				g_scaling = true;
			} else if (e.isControlDown()) {
				g_panning = true;
			} else {
				g_rotating = true;
			}
		}
	}

	@Override
	public void mouseReleased(MouseEvent e) {}

	@Override
	public void reshape(GLAutoDrawable drawable, int x, int y, int width, int height) {
        GL2 gl = drawable.getGL().getGL2();

		if (g_imageWidth != width || g_imageHeight != height)
		{
			g_imageWidth = width;
			g_imageHeight = height;


			g_FragWidth[0] = g_imageWidth;
			g_FragHeight[0] = g_imageHeight;
			
			DeleteDualPeelingRenderTargets(gl);
			InitDualPeelingRenderTargets(gl);

			DeleteFrontPeelingRenderTargets(gl);
			InitFrontPeelingRenderTargets(gl);

			DeleteAccumulationRenderTargets(gl);
			InitAccumulationRenderTargets(gl);
		}

		gl.glMatrixMode(GLMatrixFunc.GL_PROJECTION);
		gl.glLoadIdentity();

		GLU glu = GLU.createGLU(gl);
		glu.gluPerspective(FOVY, (float)g_imageWidth/(float)g_imageHeight, ZNEAR, ZFAR);
		gl.glMatrixMode(GLMatrixFunc.GL_MODELVIEW);

		gl.glViewport(0, 0, g_imageWidth, g_imageHeight);

	}

	public void setModel( Model model, int vboID, int eboID )
	{
		g_model = model;
		g_vboId[0] = vboID;
		g_eboId[0] = eboID;
		m_bShared = true;
		


		float[] modelMin = new float[3];
		float[] modelMax = new float[3];
		g_model.computeBoundingBox(modelMin, modelMax);

		float[] diag = new float[]{ modelMax[0] - modelMin[0],
				modelMax[1] - modelMin[1],
				modelMax[2] - modelMin[2] };
		g_bbScale = (float)(1.0 / Math.sqrt(diag[0]*diag[0] + diag[1]*diag[1] + diag[2]*diag[2]) * 1.5);
		g_bbTrans = new float[]{ (float)( -g_bbScale * (modelMin[0] + 0.5 * diag[0])), 
				(float)( -g_bbScale * (modelMin[1] + 0.5 * diag[1]) ), 
				(float)( -g_bbScale * (modelMin[2] + 0.5 * diag[2]) ) };
	}

	//--------------------------------------------------------------------------
	void BuildShaders(GL2 gl)
	{
		System.err.println("\nloading shaders...\n");

		g_shaderDualInit = new GLSLProgramObject();
		String dir = new String( "WildMagic" + File.separator + "Shaders" + File.separator + "GLSL" + File.separator + "dual_depth_peeling" + File.separator + "shaders" + File.separator ); 
		
		g_shaderDualInit.attachVertexShader(gl, dir + "dual_peeling_init_vertex.glsl");
		g_shaderDualInit.attachFragmentShader(gl,  dir + "dual_peeling_init_fragment.glsl");
		g_shaderDualInit.link(gl);

		g_shaderDualPeel = new GLSLProgramObject();
		g_shaderDualPeel.attachVertexShader(gl, dir +  "shade_vertex.glsl");
		g_shaderDualPeel.attachVertexShader(gl,  dir + "dual_peeling_peel_vertex.glsl");
		g_shaderDualPeel.attachFragmentShader(gl, dir +  "shade_fragment.glsl");
		g_shaderDualPeel.attachFragmentShader(gl, dir +  "dual_peeling_peel_fragment.glsl");
		g_shaderDualPeel.link(gl);

		g_shaderDualBlend = new GLSLProgramObject();
		g_shaderDualBlend.attachVertexShader(gl, dir +  "dual_peeling_blend_vertex.glsl");
		g_shaderDualBlend.attachFragmentShader(gl, dir +  "dual_peeling_blend_fragment.glsl");
		g_shaderDualBlend.link(gl);

		g_shaderDualFinal = new GLSLProgramObject();
		g_shaderDualFinal.attachVertexShader(gl, dir +  "dual_peeling_final_vertex.glsl");
		g_shaderDualFinal.attachFragmentShader(gl,  dir + "dual_peeling_final_fragment.glsl");
		g_shaderDualFinal.link(gl);

		g_shaderFrontInit = new GLSLProgramObject();
		g_shaderFrontInit.attachVertexShader(gl, dir +  "shade_vertex.glsl");
		g_shaderFrontInit.attachVertexShader(gl, dir +  "front_peeling_init_vertex.glsl");
		g_shaderFrontInit.attachFragmentShader(gl, dir +  "shade_fragment.glsl");
		g_shaderFrontInit.attachFragmentShader(gl, dir +  "front_peeling_init_fragment.glsl");
		g_shaderFrontInit.link(gl);

		g_shaderFrontPeel = new GLSLProgramObject();
		g_shaderFrontPeel.attachVertexShader(gl,dir +   "shade_vertex.glsl");
		g_shaderFrontPeel.attachVertexShader(gl, dir +  "front_peeling_peel_vertex.glsl");
		g_shaderFrontPeel.attachFragmentShader(gl, dir +  "shade_fragment.glsl");
		g_shaderFrontPeel.attachFragmentShader(gl,  dir + "front_peeling_peel_fragment.glsl");
		g_shaderFrontPeel.link(gl);

		g_shaderFrontBlend = new GLSLProgramObject();
		g_shaderFrontBlend.attachVertexShader(gl, dir +  "front_peeling_blend_vertex.glsl");
		g_shaderFrontBlend.attachFragmentShader(gl, dir +  "front_peeling_blend_fragment.glsl");
		g_shaderFrontBlend.link(gl);

		g_shaderFrontFinal = new GLSLProgramObject();
		g_shaderFrontFinal.attachVertexShader(gl, dir +  "front_peeling_final_vertex.glsl");
		g_shaderFrontFinal.attachFragmentShader(gl, dir +  "front_peeling_final_fragment.glsl");
		g_shaderFrontFinal.link(gl);

		g_shaderAverageInit = new GLSLProgramObject();
		//g_shaderAverageInit.attachVertexShader(gl,dir +   "shade_vertex.glsl");
		g_shaderAverageInit.attachVertexShader(gl, dir +  "wavg_init_vertex.glsl");
		//g_shaderAverageInit.attachFragmentShader(gl, dir +  "shade_fragment.glsl");
		g_shaderAverageInit.attachFragmentShader(gl, dir +  "wavg_init_fragment2.glsl");
		g_shaderAverageInit.link(gl);

		g_shaderAverageFinal = new GLSLProgramObject();
		g_shaderAverageFinal.attachVertexShader(gl, dir +  "wavg_final_vertex.glsl");
		g_shaderAverageFinal.attachFragmentShader(gl,  dir + "wavg_final_fragment.glsl");
		g_shaderAverageFinal.link(gl);

		g_shaderWeightedSumInit = new GLSLProgramObject();
		g_shaderWeightedSumInit.attachVertexShader(gl, dir +  "shade_vertex.glsl");
		g_shaderWeightedSumInit.attachVertexShader(gl, dir +  "wsum_init_vertex.glsl");
		g_shaderWeightedSumInit.attachFragmentShader(gl, dir +  "shade_fragment.glsl");
		g_shaderWeightedSumInit.attachFragmentShader(gl, dir +  "wsum_init_fragment.glsl");
		g_shaderWeightedSumInit.link(gl);

		g_shaderWeightedSumFinal = new GLSLProgramObject();
		g_shaderWeightedSumFinal.attachVertexShader(gl, dir +  "wsum_final_vertex.glsl");
		g_shaderWeightedSumFinal.attachFragmentShader(gl, dir +  "wsum_final_fragment.glsl");
		g_shaderWeightedSumFinal.link(gl);
	}


	//--------------------------------------------------------------------------
	void DeleteAccumulationRenderTargets(GL2 gl)
	{
		gl.glDeleteFramebuffers(1, g_accumulationFboId, 0);
		gl.glDeleteTextures(2, g_accumulationTexId, 0);
	}


	//--------------------------------------------------------------------------
	void DeleteDualPeelingRenderTargets(GL2 gl)
	{
		gl.glDeleteFramebuffers(1, g_dualBackBlenderFboId, 0);
		gl.glDeleteFramebuffers(1, g_dualPeelingSingleFboId, 0);
		gl.glDeleteTextures(2, g_dualDepthTexId, 0);
		gl.glDeleteTextures(2, g_dualFrontBlenderTexId, 0);
		gl.glDeleteTextures(2, g_dualBackTempTexId, 0);
		gl.glDeleteTextures(1, g_dualBackBlenderTexId, 0);
	}


	//--------------------------------------------------------------------------
	void DeleteFrontPeelingRenderTargets(GL2 gl)
	{
		gl.glDeleteFramebuffers(2, g_frontFboId, 0);
		gl.glDeleteFramebuffers(1, g_frontColorBlenderFboId, 0);
		gl.glDeleteTextures(2, g_frontDepthTexId, 0);
		gl.glDeleteTextures(2, g_frontColorTexId, 0);
		gl.glDeleteTextures(1, g_frontColorBlenderTexId, 0);
	}


	//--------------------------------------------------------------------------
	void DestroyShaders(GL2 gl)
	{
		g_shaderDualInit.destroy(gl);
		g_shaderDualPeel.destroy(gl);
		g_shaderDualBlend.destroy(gl);
		g_shaderDualFinal.destroy(gl);

		g_shaderFrontInit.destroy(gl);
		g_shaderFrontPeel.destroy(gl);
		g_shaderFrontBlend.destroy(gl);
		g_shaderFrontFinal.destroy(gl);

		g_shaderAverageInit.destroy(gl);
		g_shaderAverageFinal.destroy(gl);

		g_shaderWeightedSumInit.destroy(gl);
		g_shaderWeightedSumFinal.destroy(gl);
	}


	//--------------------------------------------------------------------------
	void DrawModel(GL2 gl)
	{
		gl.glBindBuffer( GL.GL_ARRAY_BUFFER, g_vboId[0]);
		gl.glBindBuffer( GL.GL_ELEMENT_ARRAY_BUFFER, g_eboId[0]);
		int stride = g_model.getCompiledVertexSize() * Buffers.SIZEOF_FLOAT;
		int normalOffset = g_model.getCompiledNormalOffset() * Buffers.SIZEOF_FLOAT;
		gl.glVertexPointer(g_model.getPositionSize(), GL.GL_FLOAT, stride, 0);
		gl.glNormalPointer(GL.GL_FLOAT, stride, normalOffset);
		gl.glEnableClientState(GLPointerFunc.GL_VERTEX_ARRAY);
		gl.glEnableClientState(GLPointerFunc.GL_NORMAL_ARRAY);

		gl.glDrawElements( GL.GL_TRIANGLES, g_model.getCompiledIndexCount(), GL.GL_UNSIGNED_INT, 0);

		gl.glBindBuffer(GL.GL_ELEMENT_ARRAY_BUFFER, 0);
		gl.glBindBuffer(GL.GL_ARRAY_BUFFER, 0);
		gl.glDisableClientState(GLPointerFunc.GL_VERTEX_ARRAY);
		gl.glDisableClientState(GLPointerFunc.GL_NORMAL_ARRAY);

		g_numGeoPasses++;
	}


	//--------------------------------------------------------------------------
	void InitAccumulationRenderTargets(GL2 gl)
	{
		
		gl.glGenTextures(2, g_accumulationTexId, 0);

		gl.glBindTexture( GL2GL3.GL_TEXTURE_RECTANGLE, g_accumulationTexId[0]);
		gl.glTexParameteri( GL2GL3.GL_TEXTURE_RECTANGLE,  GL.GL_TEXTURE_WRAP_S,  GL2.GL_CLAMP);
		gl.glTexParameteri( GL2GL3.GL_TEXTURE_RECTANGLE,  GL.GL_TEXTURE_WRAP_T,  GL2.GL_CLAMP);
		gl.glTexParameteri( GL2GL3.GL_TEXTURE_RECTANGLE,  GL.GL_TEXTURE_MIN_FILTER,  GL.GL_NEAREST);
		gl.glTexParameteri( GL2GL3.GL_TEXTURE_RECTANGLE,  GL.GL_TEXTURE_MAG_FILTER,  GL.GL_NEAREST);
		gl.glTexImage2D( GL2GL3.GL_TEXTURE_RECTANGLE, 0,  GL2GL3.GL_RGBA16F,
				g_imageWidth, g_imageHeight, 0,  GL.GL_RGBA,  GL.GL_FLOAT, null);

		gl.glBindTexture( GL2GL3.GL_TEXTURE_RECTANGLE, g_accumulationTexId[1]);
		gl.glTexParameteri( GL2GL3.GL_TEXTURE_RECTANGLE,  GL.GL_TEXTURE_WRAP_S,  GL2.GL_CLAMP);
		gl.glTexParameteri( GL2GL3.GL_TEXTURE_RECTANGLE,  GL.GL_TEXTURE_WRAP_T,  GL2.GL_CLAMP);
		gl.glTexParameteri( GL2GL3.GL_TEXTURE_RECTANGLE,  GL.GL_TEXTURE_MIN_FILTER,  GL.GL_NEAREST);
		gl.glTexParameteri( GL2GL3.GL_TEXTURE_RECTANGLE,  GL.GL_TEXTURE_MAG_FILTER,  GL.GL_NEAREST);
		gl.glTexImage2D( GL2GL3.GL_TEXTURE_RECTANGLE, 0,  GL2.GL_FLOAT_R32_NV,
				g_imageWidth, g_imageHeight, 0,  GL.GL_RGBA,  GL.GL_FLOAT, null);

		gl.glGenFramebuffers(1, g_accumulationFboId, 0);
		gl.glBindFramebuffer( GL.GL_FRAMEBUFFER, g_accumulationFboId[0]);
		gl.glFramebufferTexture2D( GL.GL_FRAMEBUFFER,  GL.GL_COLOR_ATTACHMENT0,
				GL2GL3.GL_TEXTURE_RECTANGLE, g_accumulationTexId[0], 0);
		gl.glFramebufferTexture2D( GL.GL_FRAMEBUFFER,  GL2GL3.GL_COLOR_ATTACHMENT1,
				GL2GL3.GL_TEXTURE_RECTANGLE, g_accumulationTexId[1], 0);

		//int uiStatus = gl.glCheckFramebufferStatus(GL.GL_FRAMEBUFFER);
	}


	//--------------------------------------------------------------------------
	void InitFrontPeelingRenderTargets(GL2 gl)
	{
		gl.glGenTextures(2, g_frontDepthTexId, 0);
		gl.glGenTextures(2, g_frontColorTexId, 0);
		gl.glGenFramebuffers(2, g_frontFboId, 0);

		for (int i = 0; i < 2; i++)
		{
			gl.glBindTexture( GL2GL3.GL_TEXTURE_RECTANGLE, g_frontDepthTexId[i]);
			gl.glTexParameteri( GL2GL3.GL_TEXTURE_RECTANGLE,  GL.GL_TEXTURE_WRAP_S,  GL2.GL_CLAMP);
			gl.glTexParameteri( GL2GL3.GL_TEXTURE_RECTANGLE,  GL.GL_TEXTURE_WRAP_T,  GL2.GL_CLAMP);
			gl.glTexParameteri( GL2GL3.GL_TEXTURE_RECTANGLE,  GL.GL_TEXTURE_MIN_FILTER,  GL.GL_NEAREST);
			gl.glTexParameteri( GL2GL3.GL_TEXTURE_RECTANGLE,  GL.GL_TEXTURE_MAG_FILTER,  GL.GL_NEAREST);
			gl.glTexImage2D( GL2GL3.GL_TEXTURE_RECTANGLE, 0,  GL2GL3.GL_DEPTH_COMPONENT32F,
					g_imageWidth, g_imageHeight, 0,  GL2ES2.GL_DEPTH_COMPONENT,  GL.GL_FLOAT, null);

			gl.glBindTexture( GL2GL3.GL_TEXTURE_RECTANGLE, g_frontColorTexId[i]);
			gl.glTexParameteri( GL2GL3.GL_TEXTURE_RECTANGLE,  GL.GL_TEXTURE_WRAP_S,  GL2.GL_CLAMP);
			gl.glTexParameteri( GL2GL3.GL_TEXTURE_RECTANGLE,  GL.GL_TEXTURE_WRAP_T,  GL2.GL_CLAMP);
			gl.glTexParameteri( GL2GL3.GL_TEXTURE_RECTANGLE,  GL.GL_TEXTURE_MIN_FILTER,  GL.GL_NEAREST);
			gl.glTexParameteri( GL2GL3.GL_TEXTURE_RECTANGLE,  GL.GL_TEXTURE_MAG_FILTER,  GL.GL_NEAREST);
			gl.glTexImage2D( GL2GL3.GL_TEXTURE_RECTANGLE, 0,  GL.GL_RGBA, g_imageWidth, g_imageHeight,
					0,  GL.GL_RGBA,  GL.GL_FLOAT, null);

			gl.glBindFramebuffer( GL.GL_FRAMEBUFFER, g_frontFboId[i]);
			gl.glFramebufferTexture2D( GL.GL_FRAMEBUFFER,  GL.GL_DEPTH_ATTACHMENT,
					GL2GL3.GL_TEXTURE_RECTANGLE, g_frontDepthTexId[i], 0);
			gl.glFramebufferTexture2D( GL.GL_FRAMEBUFFER,  GL.GL_COLOR_ATTACHMENT0,
					GL2GL3.GL_TEXTURE_RECTANGLE, g_frontColorTexId[i], 0);
		}

		gl.glGenTextures(1, g_frontColorBlenderTexId, 0);
		gl.glBindTexture( GL2GL3.GL_TEXTURE_RECTANGLE, g_frontColorBlenderTexId[0]);
		gl.glTexParameteri( GL2GL3.GL_TEXTURE_RECTANGLE,  GL.GL_TEXTURE_WRAP_S, GL2.GL_CLAMP);
		gl.glTexParameteri( GL2GL3.GL_TEXTURE_RECTANGLE,  GL.GL_TEXTURE_WRAP_T, GL2.GL_CLAMP);
		gl.glTexParameteri( GL2GL3.GL_TEXTURE_RECTANGLE,  GL.GL_TEXTURE_MIN_FILTER,  GL.GL_NEAREST);
		gl.glTexParameteri( GL2GL3.GL_TEXTURE_RECTANGLE,  GL.GL_TEXTURE_MAG_FILTER,  GL.GL_NEAREST);
		gl.glTexImage2D( GL2GL3.GL_TEXTURE_RECTANGLE, 0, GL. GL_RGBA, g_imageWidth, g_imageHeight,
				0,  GL.GL_RGBA,  GL.GL_FLOAT, null);

		gl.glGenFramebuffers(1, g_frontColorBlenderFboId, 0);
		gl.glBindFramebuffer( GL.GL_FRAMEBUFFER, g_frontColorBlenderFboId[0]);
		gl.glFramebufferTexture2D( GL.GL_FRAMEBUFFER,  GL.GL_DEPTH_ATTACHMENT,
				GL2GL3.GL_TEXTURE_RECTANGLE, g_frontDepthTexId[0], 0);
		gl.glFramebufferTexture2D( GL.GL_FRAMEBUFFER,  GL.GL_COLOR_ATTACHMENT0,
				GL2GL3.GL_TEXTURE_RECTANGLE, g_frontColorBlenderTexId[0], 0);
	}


	void InitGL()
	{ 
	  	GLProfile kProfile = GLProfile.getMaxProgrammable(true);
		GLCapabilities kGlCapabilities = new GLCapabilities(kProfile);
		kGlCapabilities.setHardwareAccelerated(true);
		m_kCanvas = new GLCanvas(kGlCapabilities);
		m_kCanvas.setSize(g_imageWidth, g_imageHeight);
		m_kCanvas.addGLEventListener( this );       
		m_kCanvas.addKeyListener( this );       
		m_kCanvas.addMouseListener( this );       
		m_kCanvas.addMouseMotionListener( this );       
	}


	//--------------------------------------------------------------------------
	void LoadModel( GL2 gl, String model_filename)
	{
		g_model = new Model();
		System.err.println("loading OBJ...\n");
		
		String dir = new String( "WildMagic" + File.separator + "ApplicationDemos" + File.separator + "media" + File.separator + "models" + File.separator ); 
		
		g_model.loadModelFromFile( dir + model_filename );

		System.err.println("compiling mesh...\n");
		g_model.compileModel();

		System.err.println(g_model.getPositionCount() + " vertices");
		System.err.println((g_model.getIndexCount()/3) + " triangles");
		int totalVertexSize = g_model.getCompiledVertexCount() * Buffers.SIZEOF_FLOAT;
		int totalIndexSize = g_model.getCompiledIndexCount() * Buffers.SIZEOF_INT;

		gl.glGenBuffers(1, g_vboId, 0);
		gl.glBindBuffer(GL.GL_ARRAY_BUFFER, g_vboId[0]);
		gl.glBufferData(GL.GL_ARRAY_BUFFER, totalVertexSize, g_model.getCompiledVertices(), GL.GL_STATIC_DRAW);
		gl.glBindBuffer(GL.GL_ARRAY_BUFFER, 0);

		gl.glGenBuffers(1, g_eboId, 0);
		gl.glBindBuffer(GL.GL_ELEMENT_ARRAY_BUFFER, g_eboId[0]);
		gl.glBufferData(GL.GL_ELEMENT_ARRAY_BUFFER, totalIndexSize, g_model.getCompiledIndices(), GL.GL_STATIC_DRAW);
		gl.glBindBuffer(GL.GL_ELEMENT_ARRAY_BUFFER, 0);

		float[] modelMin = new float[3];
		float[] modelMax = new float[3];
		g_model.computeBoundingBox(modelMin, modelMax);

		float[] diag = new float[]{ modelMax[0] - modelMin[0],
				modelMax[1] - modelMin[1],
				modelMax[2] - modelMin[2] };
		g_bbScale = (float)(1.0 / Math.sqrt(diag[0]*diag[0] + diag[1]*diag[1] + diag[2]*diag[2]) * 1.5);
		g_bbTrans = new float[]{ (float)( -g_bbScale * (modelMin[0] + 0.5 * diag[0])), 
				(float)( -g_bbScale * (modelMin[1] + 0.5 * diag[1]) ), 
				(float)( -g_bbScale * (modelMin[2] + 0.5 * diag[2]) ) };
	}


	//--------------------------------------------------------------------------
	void MakeFullScreenQuad(GL2 gl)
	{
		//GLU glu = GLU.createGLU(gl);

		g_quadDisplayList = gl.glGenLists(1);
		gl.glNewList(g_quadDisplayList, GL2.GL_COMPILE);

		gl.glMatrixMode(GLMatrixFunc.GL_MODELVIEW);
		gl.glPushMatrix();
		gl.glLoadIdentity();
		//glu.gluOrtho2D(0.0f, 1.0f, 0.0f, 1.0f);
		gl.glOrtho(0.0f, 1.0f, 0.0f, 1.0f, -1, 1);
		gl.glBegin(GL2.GL_QUADS);
		{
			gl.glVertex2f(0.0f, 0.0f); 
			gl.glMultiTexCoord2f(0, 0.0f, 0.0f); 
			gl.glVertex2f(1.0f, 0.0f);
			gl.glMultiTexCoord2f(0, 1.0f, 0.0f);
			gl.glVertex2f(1.0f, 1.0f);
			gl.glMultiTexCoord2f(0, 1.0f, 1.0f);
			gl.glVertex2f(0.0f, 1.0f);			
			gl.glMultiTexCoord2f(0, 0.0f, 1.0f);
		}
		gl.glEnd();
		gl.glPopMatrix();

		gl.glEndList();
	}


	//--------------------------------------------------------------------------
	void ReloadShaders(GL2 gl)
	{
		DestroyShaders(gl);
		BuildShaders(gl);
	}


	//--------------------------------------------------------------------------
	void RenderAverageColors(GL2 gl)
	{
		//System.err.println( "Weighted avg mode" );
		gl.glDisable(GL.GL_DEPTH_TEST);

		// ---------------------------------------------------------------------
		// 1. Accumulate Colors and Depth Complexity
		// ---------------------------------------------------------------------

		gl.glBindFramebuffer(GL.GL_FRAMEBUFFER, g_accumulationFboId[0]);
		gl.glDrawBuffers(2, g_drawBuffers, 0);

		gl.glClearColor(0, 0, 0, 0);
		gl.glClear(GL.GL_COLOR_BUFFER_BIT);

		gl.glBlendEquation(GL.GL_FUNC_ADD);
		gl.glBlendFunc(GL.GL_ONE, GL.GL_ONE);
		gl.glEnable(GL.GL_BLEND);

		g_shaderAverageInit.bind(gl);
		g_shaderAverageInit.setUniform(gl,"Alpha", g_opacity, 1);
		DrawModel(gl);
		g_shaderAverageInit.unbind(gl);

		gl.glDisable(GL.GL_BLEND);


		// ---------------------------------------------------------------------
		// 2. Approximate Blending
		// ---------------------------------------------------------------------

		gl.glBindFramebuffer(GL.GL_FRAMEBUFFER, 0);
		gl.glDrawBuffer(GL.GL_BACK);

		g_shaderAverageFinal.bind(gl);
		g_shaderAverageFinal.setUniform(gl,"BackgroundColor", g_backgroundColor, 3);
		g_shaderAverageFinal.bindTextureRECT(gl,"ColorTex0", g_accumulationTexId[0], 0);
		g_shaderAverageFinal.bindTextureRECT(gl,"ColorTex1", g_accumulationTexId[1], 1);
		gl.glCallList(g_quadDisplayList);
		g_shaderAverageFinal.unbind(gl);
	}


	//--------------------------------------------------------------------------
	void RenderDualPeeling(GL2 gl)
	{
		gl.glDisable(GL.GL_DEPTH_TEST);
		gl.glEnable(GL.GL_BLEND);

		// ---------------------------------------------------------------------
		// 1. Initialize Min-Max Depth Buffer
		// ---------------------------------------------------------------------

		gl.glBindFramebuffer(GL.GL_FRAMEBUFFER, g_dualPeelingSingleFboId[0]);

		// Render targets 1 and 2 store the front and back colors
		// Clear to 0.0 and use MAX blending to filter written color
		// At most one front color and one back color can be written every pass
		gl.glDrawBuffers(2, g_drawBuffers, 1);
		gl.glClearColor(0, 0, 0, 0);
		gl.glClear(GL.GL_COLOR_BUFFER_BIT);

		// Render target 0 stores (-minDepth, maxDepth, alphaMultiplier)
		gl.glDrawBuffer(g_drawBuffers[0]);
		gl.glClearColor(-MAX_DEPTH, -MAX_DEPTH, 0, 0);
		gl.glClear(GL.GL_COLOR_BUFFER_BIT);
		gl.glBlendEquation(GL2GL3.GL_MAX);

		g_shaderDualInit.bind(gl);
		DrawModel(gl);
		g_shaderDualInit.unbind(gl);

		// ---------------------------------------------------------------------
		// 2. Dual Depth Peeling + Blending
		// ---------------------------------------------------------------------

		// Since we cannot blend the back colors in the geometry passes,
		// we use another render target to do the alpha blending
		//glBindFramebufferEXT(GL_FRAMEBUFFER_EXT, g_dualBackBlenderFboId);
		gl.glDrawBuffer(g_drawBuffers[6]);
		gl.glClearColor(g_backgroundColor[0], g_backgroundColor[1], g_backgroundColor[2], 0);
		gl.glClear(GL.GL_COLOR_BUFFER_BIT);

		int currId = 0;

		for (int pass = 1; g_useOQ || pass < g_numPasses; pass++) {
			currId = pass % 2;
			int prevId = 1 - currId;
			int bufId = currId * 3;

			//glBindFramebufferEXT(GL_FRAMEBUFFER_EXT, g_dualPeelingFboId[currId]);

			gl.glDrawBuffers(2, g_drawBuffers, bufId+1);
			gl.glClearColor(0, 0, 0, 0);
			gl.glClear(GL.GL_COLOR_BUFFER_BIT);

			gl.glDrawBuffer(g_drawBuffers[bufId+0]);
			gl.glClearColor(-MAX_DEPTH, -MAX_DEPTH, 0, 0);
			gl.glClear(GL.GL_COLOR_BUFFER_BIT);

			// Render target 0: RG32F MAX blending
			// Render target 1: RGBA MAX blending
			// Render target 2: RGBA MAX blending
			gl.glDrawBuffers(3, g_drawBuffers, bufId+0);
			gl.glBlendEquation(GL2GL3.GL_MAX);

			g_shaderDualPeel.bind(gl);
			g_shaderDualPeel.bindTextureRECT(gl,"DepthBlenderTex", g_dualDepthTexId[prevId], 0);
			g_shaderDualPeel.bindTextureRECT(gl,"FrontBlenderTex", g_dualFrontBlenderTexId[prevId], 1);
			g_shaderDualPeel.setUniform(gl,"Alpha", g_opacity, 1);
			DrawModel(gl);
			g_shaderDualPeel.unbind(gl);

			// Full screen pass to alpha-blend the back color
			gl.glDrawBuffer(g_drawBuffers[6]);

			gl.glBlendEquation(GL.GL_FUNC_ADD);
			gl.glBlendFunc(GL.GL_SRC_ALPHA, GL.GL_ONE_MINUS_SRC_ALPHA);

			if (g_useOQ) {
				gl.glBeginQuery(GL2GL3.GL_SAMPLES_PASSED, g_queryId[0]);
			}

			g_shaderDualBlend.bind(gl);
			g_shaderDualBlend.bindTextureRECT(gl,"TempTex", g_dualBackTempTexId[currId], 0);
			gl.glCallList(g_quadDisplayList);
			g_shaderDualBlend.unbind(gl);

			if (g_useOQ) {
				gl.glEndQuery(GL2GL3.GL_SAMPLES_PASSED);
				int[] sample_count = new int[]{0};
				gl.glGetQueryObjectuiv(g_queryId[0], GL2GL3.GL_QUERY_RESULT, sample_count, 0);
				if (sample_count[0] == 0) {
					break;
				}
			}
		}

		gl.glDisable(GL.GL_BLEND);

		// ---------------------------------------------------------------------
		// 3. Final Pass
		// ---------------------------------------------------------------------

		gl.glBindFramebuffer(GL.GL_FRAMEBUFFER, 0);
		gl.glDrawBuffer(GL.GL_BACK);

		g_shaderDualFinal.bind(gl);
		g_shaderDualFinal.bindTextureRECT(gl,"FrontBlenderTex", g_dualFrontBlenderTexId[currId], 1);
		g_shaderDualFinal.bindTextureRECT(gl,"BackBlenderTex", g_dualBackBlenderTexId[0], 2);
		gl.glCallList(g_quadDisplayList);
		g_shaderDualFinal.unbind(gl);
	}


	//--------------------------------------------------------------------------
	void RenderFrontToBackPeeling(GL2 gl)
	{
		// ---------------------------------------------------------------------
		// 1. Initialize Min Depth Buffer
		// ---------------------------------------------------------------------

		gl.glBindFramebuffer(GL.GL_FRAMEBUFFER, g_frontColorBlenderFboId[0]);
		gl.glDrawBuffer(g_drawBuffers[0]);

		gl.glClearColor(0, 0, 0, 1);
		gl.glClear(GL.GL_COLOR_BUFFER_BIT | GL.GL_DEPTH_BUFFER_BIT);

		gl.glEnable(GL.GL_DEPTH_TEST);

		g_shaderFrontInit.bind(gl);
		g_shaderFrontInit.setUniform(gl,"Alpha", g_opacity, 1);
		DrawModel(gl);
		g_shaderFrontInit.unbind(gl);

		// ---------------------------------------------------------------------
		// 2. Depth Peeling + Blending
		// ---------------------------------------------------------------------

		int numLayers = (g_numPasses - 1) * 2;
		for (int layer = 1; g_useOQ || layer < numLayers; layer++) {
			int currId = layer % 2;
			int prevId = 1 - currId;

			gl.glBindFramebuffer(GL.GL_FRAMEBUFFER, g_frontFboId[currId]);
			gl.glDrawBuffer(g_drawBuffers[0]);

			gl.glClearColor(0, 0, 0, 0);
			gl.glClear(GL.GL_COLOR_BUFFER_BIT | GL.GL_DEPTH_BUFFER_BIT);

			gl.glDisable(GL.GL_BLEND);
			gl.glEnable(GL.GL_DEPTH_TEST);

			if (g_useOQ) {
				gl.glBeginQuery(GL2GL3.GL_SAMPLES_PASSED, g_queryId[0]);
			}

			g_shaderFrontPeel.bind(gl);
			g_shaderFrontPeel.bindTextureRECT(gl,"DepthTex", g_frontDepthTexId[prevId], 0);
			g_shaderFrontPeel.setUniform(gl,"Alpha", g_opacity, 1);
			DrawModel(gl);
			g_shaderFrontPeel.unbind(gl);

			if (g_useOQ) {
				gl.glEndQuery(GL2GL3.GL_SAMPLES_PASSED);
			}


			gl.glBindFramebuffer(GL.GL_FRAMEBUFFER, g_frontColorBlenderFboId[0]);
			gl.glDrawBuffer(g_drawBuffers[0]);

			gl.glDisable(GL.GL_DEPTH_TEST);
			gl.glEnable(GL.GL_BLEND);

			gl.glBlendEquation(GL.GL_FUNC_ADD);
			gl.glBlendFuncSeparate(GL.GL_DST_ALPHA, GL.GL_ONE,
					GL.GL_ZERO, GL.GL_ONE_MINUS_SRC_ALPHA);

			g_shaderFrontBlend.bind(gl);
			g_shaderFrontBlend.bindTextureRECT(gl,"TempTex", g_frontColorTexId[currId], 0);
			gl.glCallList(g_quadDisplayList);
			g_shaderFrontBlend.unbind(gl);

			gl.glDisable(GL.GL_BLEND);

			if (g_useOQ) {
				int[] sample_count = new int[]{0};
				gl.glGetQueryObjectuiv(g_queryId[0], GL2GL3.GL_QUERY_RESULT, sample_count, 0);
				if (sample_count[0] == 0) {
					break;
				}
			}
		}

		// ---------------------------------------------------------------------
		// 3. Final Pass
		// ---------------------------------------------------------------------

		gl.glBindFramebuffer(GL.GL_FRAMEBUFFER, 0);
		gl.glDrawBuffer(GL.GL_BACK);
		gl.glDisable(GL.GL_DEPTH_TEST);

		g_shaderFrontFinal.bind(gl);
		g_shaderFrontFinal.setUniform(gl,"BackgroundColor", g_backgroundColor, 3);
		g_shaderFrontFinal.bindTextureRECT(gl,"ColorTex", g_frontColorBlenderTexId[0], 0);
		gl.glCallList(g_quadDisplayList);
		g_shaderFrontFinal.unbind(gl);
	}


	//--------------------------------------------------------------------------
	void RenderWeightedSum(GL2 gl)
	{
		gl.glDisable(GL.GL_DEPTH_TEST);

		// ---------------------------------------------------------------------
		// 1. Accumulate (alpha * color) and (alpha)
		// ---------------------------------------------------------------------

		gl.glBindFramebuffer(GL.GL_FRAMEBUFFER, g_accumulationFboId[0]);
		gl.glDrawBuffer(g_drawBuffers[0]);

		gl.glClearColor(0, 0, 0, 0);
		gl.glClear(GL.GL_COLOR_BUFFER_BIT);

		gl.glBlendEquation(GL.GL_FUNC_ADD);
		gl.glBlendFunc(GL.GL_ONE, GL.GL_ONE);
		gl.glEnable(GL.GL_BLEND);

		g_shaderWeightedSumInit.bind(gl);
		g_shaderWeightedSumInit.setUniform(gl,"Alpha", g_opacity, 1);
		DrawModel(gl);
		g_shaderWeightedSumInit.unbind(gl);

		gl.glDisable(GL.GL_BLEND);

		// ---------------------------------------------------------------------
		// 2. Weighted Sum
		// ---------------------------------------------------------------------

		gl.glBindFramebuffer(GL.GL_FRAMEBUFFER, 0);
		gl.glDrawBuffer(GL.GL_BACK);

		g_shaderWeightedSumFinal.bind(gl);
		g_shaderWeightedSumFinal.setUniform(gl,"BackgroundColor", g_backgroundColor, 3);
		g_shaderWeightedSumFinal.bindTextureRECT(gl,"ColorTex", g_accumulationTexId[0], 0);
		gl.glCallList(g_quadDisplayList);
		g_shaderWeightedSumFinal.unbind(gl);
	}
}
