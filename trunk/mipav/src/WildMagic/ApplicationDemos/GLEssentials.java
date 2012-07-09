

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
import java.nio.ByteBuffer;
import java.nio.FloatBuffer;
import java.nio.IntBuffer;

import javax.media.opengl.DebugGL2;
import javax.media.opengl.GL;
import javax.media.opengl.GL2;
import javax.media.opengl.GL2ES2;
import javax.media.opengl.GL2GL3;
import javax.media.opengl.GL3;
import javax.media.opengl.GLAutoDrawable;
import javax.media.opengl.GLCapabilities;
import javax.media.opengl.GLEventListener;
import javax.media.opengl.GLException;
import javax.media.opengl.GLProfile;
import javax.media.opengl.awt.GLCanvas;
import javax.media.opengl.fixedfunc.GLLightingFunc;
import javax.media.opengl.fixedfunc.GLMatrixFunc;
import javax.media.opengl.fixedfunc.GLPointerFunc;
import javax.media.opengl.glu.GLU;

import WildMagic.ApplicationDemos.GLEssentialSupport.UtilSrc.demoImage;
import WildMagic.ApplicationDemos.GLEssentialSupport.UtilSrc.demoModel;
import WildMagic.ApplicationDemos.GLEssentialSupport.UtilSrc.demoSource;
import WildMagic.ApplicationDemos.GLEssentialSupport.UtilSrc.matrixUtil;

import com.jogamp.common.nio.Buffers;
import com.jogamp.opengl.util.Animator;

public class GLEssentials implements GLEventListener, KeyListener, MouseListener, MouseMotionListener
{

	final static int POS_ATTRIB_IDX = 0;
	final static int NORMAL_ATTRIB_IDX = 1;
	final static int TEXCOORD_ATTRIB_IDX = 2;

	int m_defaultFBOName;

	demoModel m_quadModel;
	int m_quadPrimType;
	int m_quadElementType;
	int m_quadNumElements;
	int[] m_reflectVAOName;
	int m_reflectTexName;
	int[] m_reflectFBOName;
	int m_reflectWidth;
	int m_reflectHeight;
	int m_reflectPrgName;
	int  m_reflectModelViewUniformIdx;
	int  m_reflectProjectionUniformIdx;
	int m_reflectNormalMatrixUniformIdx;


	int m_characterPrgName;
	int m_characterMvpUniformIdx;
	int[] m_characterVAOName;
	int[] m_characterTexName;
	demoModel m_characterModel;
	int m_characterPrimType;
	int m_characterElementType;
	int m_characterNumElements;
	float m_characterAngle;


	int m_viewWidth = 512;
	int m_viewHeight = 512;


	boolean m_useVBOs = true;


	/** GLCanvas for Java/JOGL */
	private GLCanvas m_kCanvas;

	/** GL object from GLCanvas.getGL() used to access openGL calls. */
	private GL2 m_kGL = null;

	public static void main(String[] args) {

		GLEssentials kWorld = new GLEssentials();
		Frame frame = new Frame("GLEssentials");
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

	public GLEssentials()
	{
		InitGL();	
	}

	@Override
	public void display(GLAutoDrawable drawable) {	
		m_kGL = drawable.getGL().getGL2();	

		if ( !m_bInit )
		{
			init(drawable);
		}

		// Set up the modelview and projection matricies
		float[] modelView = new float[16];
		float[] projection = new float[16];
		float[] mvp = new float[16];

		// Bind our refletion FBO and render our scene

		m_kGL.glBindFramebuffer(GL.GL_FRAMEBUFFER, m_reflectFBOName[0]);


		m_kGL.glClear(GL.GL_COLOR_BUFFER_BIT | GL.GL_DEPTH_BUFFER_BIT);
		m_kGL.glViewport(0, 0, m_reflectWidth, m_reflectHeight);

		matrixUtil.mtxLoadPerspective(projection, 90, (float)m_reflectWidth / (float)m_reflectHeight,5.0f,10000f);

		matrixUtil.mtxLoadIdentity(modelView);

		// Invert Y so that everything is rendered up-side-down
		// as it should with a reflection

		matrixUtil.mtxScaleApply(modelView, 1, -1, 1);
		matrixUtil.mtxTranslateApply(modelView, 0, 300, -800);
		matrixUtil.mtxRotateXApply(modelView, -90.0f);	
		matrixUtil.mtxRotateApply(modelView, m_characterAngle, 0.7f, 0.3f, 1f);	

		matrixUtil.mtxMultiply(mvp, projection, modelView);

		// Use the program that we previously created
		m_kGL.glUseProgram(m_characterPrgName);

		// Set the modelview projection matrix that we calculated above
		// in our vertex shader
		m_kGL.glUniformMatrix4fv(m_characterMvpUniformIdx, 1, false, mvp, 0);

		// Bind our vertex array object
		m_kGL.glBindVertexArray(m_characterVAOName[0]);

		// Bind the texture to be used
		m_kGL.glBindTexture(GL.GL_TEXTURE_2D, m_characterTexName[0]);

		// Cull front faces now that everything is flipped 
		// with our inverted reflection transformation matrix
		m_kGL.glCullFace(GL.GL_FRONT);

		// Draw our object
		if(m_useVBOs)
		{
			m_kGL.glDrawElements(GL.GL_TRIANGLES, m_characterNumElements, m_characterElementType, 0);
		}
		else 
		{
			//m_kGL.glDrawElements(GL.GL_TRIANGLES, m_characterNumElements, m_characterElementType, m_characterModel.elements);
		}


		// Bind our default FBO to render to the screen
		m_kGL.glBindFramebuffer(GL.GL_FRAMEBUFFER, m_defaultFBOName);
		m_kGL.glViewport(0, 0, m_viewWidth, m_viewHeight);



		m_kGL.glClear(GL.GL_COLOR_BUFFER_BIT | GL.GL_DEPTH_BUFFER_BIT);

		// Use the program for rendering our character
		m_kGL.glUseProgram(m_characterPrgName);

		// Calculate the projection matrix
		matrixUtil.mtxLoadPerspective(projection, 90, (float)m_viewWidth / (float)m_viewHeight,5.0f,10000f);

		// Calculate the modelview matrix to render our character 
		//  at the proper position and rotation
		matrixUtil.mtxLoadTranslate(modelView, 0, 150, -450);
		matrixUtil.mtxRotateXApply(modelView, -90.0f);	
		matrixUtil.mtxRotateApply(modelView, m_characterAngle, 0.7f, 0.3f, 1f);

		// Multiply the modelview and projection matrix and set it in the shader
		matrixUtil.mtxMultiply(mvp, projection, modelView);

		// Have our shader use the modelview projection matrix 
		// that we calculated above
		m_kGL.glUniformMatrix4fv(m_characterMvpUniformIdx, 1, false, mvp, 0);

		// Bind the texture to be used
		m_kGL.glBindTexture(GL.GL_TEXTURE_2D, m_characterTexName[0]);

		// Bind our vertex array object
		m_kGL.glBindVertexArray(m_characterVAOName[0]);

		// Cull back faces now that we no longer render 
		// with an inverted matrix
		m_kGL.glCullFace(GL.GL_BACK);

		// Draw our character
		if(m_useVBOs)
		{
			m_kGL.glDrawElements(GL.GL_TRIANGLES, m_characterNumElements, m_characterElementType, 0);
		}
		else 
		{
			//m_kGL.glDrawElements(GL.GL_TRIANGLES, m_characterNumElements, m_characterElementType, m_characterModel.elements);
		}


		// Use our shader for reflections
		m_kGL.glUseProgram(m_reflectPrgName);

		matrixUtil.mtxLoadTranslate(modelView, 0, -50, -250);

		// Multiply the modelview and projection matrix and set it in the shader
		matrixUtil.mtxMultiply(mvp, projection, modelView);

		// Set the modelview matrix that we calculated above
		// in our vertex shader
		m_kGL.glUniformMatrix4fv(m_reflectModelViewUniformIdx, 1, false, modelView, 0);

		// Set the projection matrix that we calculated above
		// in our vertex shader
		m_kGL.glUniformMatrix4fv(m_reflectProjectionUniformIdx, 1, false, mvp, 0);

		float[] normalMatrix = new float[9];

		// Calculate the normal matrix so that we can 
		// generate texture coordinates in our fragment shader

		// The normal matrix needs to be the inverse transpose of the 
		//   top left 3x3 portion of the modelview matrix
		// We don't need to calculate the inverse transpose matrix
		//   here because this will always be an orthonormal matrix
		//   thus the the inverse tranpose is the same thing
		matrixUtil.mtx3x3FromTopLeftOf4x4(normalMatrix, modelView);

		// Set the normal matrix for our shader to use
		m_kGL.glUniformMatrix3fv(m_reflectNormalMatrixUniformIdx, 1,false, normalMatrix, 0);

		// Bind the texture we rendered-to above (i.e. the reflection texture)
		m_kGL.glBindTexture(GL.GL_TEXTURE_2D, m_reflectTexName);

		//#if !ESSENTIAL_GL_PRACTICES_IOS
		// Generate mipmaps from the rendered-to base level
		//   Mipmaps reduce shimmering pixels due to better filtering
		// This call is not accelarated on iOS 4 so do not use
		//   mipmaps here
		m_kGL.glGenerateMipmap(GL.GL_TEXTURE_2D);
		//#endif

		// Bind our vertex array object
		m_kGL.glBindVertexArray(m_reflectVAOName[0]);

		// Draw our refection plane
		if(m_useVBOs)
		{
			m_kGL.glDrawElements(GL.GL_TRIANGLES, m_quadNumElements, m_quadElementType, 0);
		}
		else 
		{
			//m_kGL.glDrawElements(GL.GL_TRIANGLES, m_quadNumElements, m_quadElementType, m_quadModel.elements);
		}
		//#endif // RENDER_REFLECTION

		// Update the angle so our character keeps spinning
		m_characterAngle++;


		/* Call swapBuffers to render on-screen: */
		drawable.swapBuffers();
	}


	public GLCanvas GetCanvas()
	{
		return m_kCanvas;
	}

	private boolean m_bInit = false;
	
	@Override
	public void init(GLAutoDrawable drawable) {
		//GL gl = drawable.setGL(new DebugGL2(drawable.getGL().getGL2()));
		//m_kGL = gl.getGL2();
		//GL gl = drawable.setGL(new TraceGL2(drawable.getGL().getGL2(), System.err));
		System.err.println( drawable.getGL() );
		System.err.println( drawable.getGLProfile() );
		System.err.println( "GL4bc " + drawable.getGL().isGL4bc() );
		System.err.println( "GL3bc " + drawable.getGL().isGL3bc() );
		System.err.println( "GL2 " + drawable.getGL().isGL2() );
		System.err.println( "GL4 " + drawable.getGL().isGL4() );
		System.err.println( "GL3 " + drawable.getGL().isGL3() );
		System.err.println( "GL2GL3 " + drawable.getGL().isGL2GL3() );
		System.err.println( "GLES2 " + drawable.getGL().isGLES2() );
		System.err.println( "GL2ES2 " + drawable.getGL().isGL2ES2() );
		System.err.println( "GLES1 " + drawable.getGL().isGLES1() );
		System.err.println( "GL2ES1 " + drawable.getGL().isGL2ES1() );
		try {
		m_kGL = drawable.getGL().getGL2();
		} catch ( GLException e )
		{
			e.printStackTrace();
		}
		drawable.setAutoSwapBufferMode( false );
		
		
		float  glLanguageVersion;
		String versionString = m_kGL.glGetString(GL2.GL_SHADING_LANGUAGE_VERSION);
		//System.err.println(versionString);
		if ( versionString.indexOf(" ") != -1 )
		{
			String num = versionString.substring( 0, versionString.indexOf(" ") );
			//System.err.println(num);
			glLanguageVersion = new Float(num).floatValue();	
		}
		else
		{
			glLanguageVersion = new Float(versionString).floatValue();
		}
		//System.err.println(glLanguageVersion);
		int version =  (int) (100.0 * glLanguageVersion + .5);	
		if ( version < 140 )
		{
			System.err.println("Shading language version:" + version + "does not support required rendering" );
			System.exit(0);
		}
		else
		{
			System.err.println("Shading language version:" + version );			
		}


		initWithDefaultFBO(0);
	}

	@Override
	public void keyPressed(KeyEvent e) {}

	@Override
	public void keyReleased(KeyEvent e) {}

	@Override
	public void keyTyped(KeyEvent e) {
		// TODO Auto-generated method stub

	}

	@Override
	public void mouseClicked(MouseEvent e) {}

	@Override
	public void mouseDragged(MouseEvent e) {}

	@Override
	public void mouseEntered(MouseEvent e) {}

	@Override
	public void mouseExited(MouseEvent e) {}

	@Override
	public void mouseMoved(MouseEvent e) {}

	@Override
	public void mousePressed(MouseEvent e) {}

	@Override
	public void mouseReleased(MouseEvent e) {}

	@Override
	public void reshape(GLAutoDrawable drawable, int x, int y, int width, int height) 
	{
		m_kGL.glViewport(0, 0, width, height);

		m_viewWidth = width;
		m_viewHeight = height;
	}


	void InitGL()
	{ 
		//GLProfile kProfile = GLProfile.getDefault();
		for ( int i = 0; i < GLProfile.GL_PROFILE_LIST_ALL.length; i++ )
		{
			System.err.println( GLProfile.GL_PROFILE_LIST_ALL[i] );
		}
		System.err.println( "" );
		System.err.println( "" );
		System.err.println( GLProfile.getDefault() );
		System.err.println( GLProfile.getMaxProgrammable() );
		//GLProfile kProfile = GLProfile.getMaxProgrammable();
		GLProfile kProfile = GLProfile.get( "GL2" );
		System.err.println( kProfile );
		GLCapabilities kGlCapabilities = new GLCapabilities(kProfile);
		kGlCapabilities.setHardwareAccelerated(true);
		m_kCanvas = new GLCanvas(kGlCapabilities);
		m_kCanvas.setSize(m_viewWidth, m_viewHeight);
		m_kCanvas.addGLEventListener( this );       
		m_kCanvas.addKeyListener( this );       
		m_kCanvas.addMouseListener( this );       
		m_kCanvas.addMouseMotionListener( this );       
	}


	private int[] buildVAO(demoModel model)
	{	

		int[] vaoName = new int[1];

		// Create a vertex array object (VAO) to cache model parameters
		m_kGL.glGenVertexArrays(1, vaoName, 0);
		m_kGL.glBindVertexArray(vaoName[0]);

		if(m_useVBOs)
		{
			int[] posBufferName = new int[1];

			// Create a vertex buffer object (VBO) to store positions
			m_kGL.glGenBuffers(1, posBufferName, 0);
			m_kGL.glBindBuffer(GL.GL_ARRAY_BUFFER, posBufferName[0]);

			FloatBuffer posData = FloatBuffer.wrap(model.positions);
			posData.rewind();
			// Allocate and load position data into the VBO
			m_kGL.glBufferData(GL.GL_ARRAY_BUFFER, model.positionArraySize, posData, GL.GL_STATIC_DRAW);

			// Enable the position attribute for this VAO
			m_kGL.glEnableVertexAttribArray(POS_ATTRIB_IDX);

			// Get the size of the position type so we can set the stride properly
			int posTypeSize = Buffers.SIZEOF_FLOAT;

			// Set up parmeters for position attribute in the VAO including, 
			//  size, type, stride, and offset in the currenly bound VAO
			// This also attaches the position VBO to the VAO
			m_kGL.glVertexAttribPointer(POS_ATTRIB_IDX,		// What attibute index will this array feed in the vertex shader (see buildProgram)
					model.positionSize,	// How many elements are there per position?
					model.positionType,	// What is the type of this data?
					false,				// Do we want to normalize this data (0-1 range for fixed-pont types)
					model.positionSize*posTypeSize, // What is the stride (i.e. bytes between positions)?
					0);	// What is the offset in the VBO to the position data?


			if(model.normals != null)
			{
				int[] normalBufferName = new int[1];

				// Create a vertex buffer object (VBO) to store positions
				m_kGL.glGenBuffers(1, normalBufferName, 0);
				m_kGL.glBindBuffer(GL.GL_ARRAY_BUFFER, normalBufferName[0]);

				FloatBuffer nData = FloatBuffer.wrap(model.normals);
				nData.rewind();
				// Allocate and load normal data into the VBO
				m_kGL.glBufferData(GL.GL_ARRAY_BUFFER, model.normalArraySize, nData, GL.GL_STATIC_DRAW);

				// Enable the normal attribute for this VAO
				m_kGL.glEnableVertexAttribArray(NORMAL_ATTRIB_IDX);

				// Get the size of the normal type so we can set the stride properly
				int normalTypeSize =  Buffers.SIZEOF_FLOAT;

				// Set up parmeters for position attribute in the VAO including, 
				//   size, type, stride, and offset in the currenly bound VAO
				// This also attaches the position VBO to the VAO
				m_kGL.glVertexAttribPointer(NORMAL_ATTRIB_IDX,	// What attibute index will this array feed in the vertex shader (see buildProgram)
						model.normalSize,	// How many elements are there per normal?
						model.normalType,	// What is the type of this data?
						false,				// Do we want to normalize this data (0-1 range for fixed-pont types)
						model.normalSize*normalTypeSize, // What is the stride (i.e. bytes between normals)?
						0);	// What is the offset in the VBO to the normal data?
			}

			if(model.texcoords != null)
			{
				int[] texcoordBufferName = new int[1];

				// Create a VBO to store texcoords
				m_kGL.glGenBuffers(1, texcoordBufferName, 0);
				m_kGL.glBindBuffer(GL.GL_ARRAY_BUFFER, texcoordBufferName[0]);

				FloatBuffer tData = FloatBuffer.wrap(model.texcoords);
				tData.rewind();
				// Allocate and load texcoord data into the VBO
				m_kGL.glBufferData(GL.GL_ARRAY_BUFFER, model.texcoordArraySize, tData, GL.GL_STATIC_DRAW);

				// Enable the texcoord attribute for this VAO
				m_kGL.glEnableVertexAttribArray(TEXCOORD_ATTRIB_IDX);

				// Get the size of the texcoord type so we can set the stride properly
				int texcoordTypeSize = Buffers.SIZEOF_FLOAT;

				// Set up parmeters for texcoord attribute in the VAO including,
				//   size, type, stride, and offset in the currenly bound VAO
				// This also attaches the texcoord VBO to VAO
				m_kGL.glVertexAttribPointer(TEXCOORD_ATTRIB_IDX,	// What attibute index will this array feed in the vertex shader (see buildProgram)
						model.texcoordSize,	// How many elements are there per texture coord?
						model.texcoordType,	// What is the type of this data in the array?
						true,				// Do we want to normalize this data (0-1 range for fixed-point types)
						model.texcoordSize*texcoordTypeSize,  // What is the stride (i.e. bytes between texcoords)?
						0);	// What is the offset in the VBO to the texcoord data?
			}

			int[] elementBufferName = new int[1];	

			// Create a VBO to vertex array elements
			// This also attaches the element array buffer to the VAO
			m_kGL.glGenBuffers(1, elementBufferName, 0);
			m_kGL.glBindBuffer(GL.GL_ELEMENT_ARRAY_BUFFER, elementBufferName[0]);

			IntBuffer iData = IntBuffer.wrap(model.elements);
			iData.rewind();
			// Allocate and load vertex array element data into VBO
			m_kGL.glBufferData(GL.GL_ELEMENT_ARRAY_BUFFER, model.elementArraySize, iData, GL.GL_STATIC_DRAW);	
		}
		/*
		else
		{

			// Enable the position attribute for this VAO
			m_kGL.glEnableVertexAttribArray(POS_ATTRIB_IDX);

			// Get the size of the position type so we can set the stride properly
			int posTypeSize = Buffers.SIZEOF_FLOAT;

			// Set up parmeters for position attribute in the VAO including,
			//  size, type, stride, and offset in the currenly bound VAO
			// This also attaches the position array in memory to the VAO
			m_kGL.glVertexAttribPointer(POS_ATTRIB_IDX,  // What attibute index will this array feed in the vertex shader? (also see buildProgram)
					model.positionSize,  // How many elements are there per position?
					model.positionType,  // What is the type of this data
					false,				// Do we want to normalize this data (0-1 range for fixed-pont types)
					model.positionSize*posTypeSize, // What is the stride (i.e. bytes between positions)?
					model.positions);    // Where is the position data in memory?

			if(model.normals != null)
			{			
				// Enable the normal attribute for this VAO
				m_kGL.glEnableVertexAttribArray(NORMAL_ATTRIB_IDX);

				// Get the size of the normal type so we can set the stride properly
				int normalTypeSize = Buffers.SIZEOF_FLOAT;

				// Set up parmeters for position attribute in the VAO including, 
				//   size, type, stride, and offset in the currenly bound VAO
				// This also attaches the position VBO to the VAO
				m_kGL.glVertexAttribPointer(NORMAL_ATTRIB_IDX,	// What attibute index will this array feed in the vertex shader (see buildProgram)
						model.normalSize,	// How many elements are there per normal?
						model.normalType,	// What is the type of this data?
						false,				// Do we want to normalize this data (0-1 range for fixed-pont types)
						model.normalSize*normalTypeSize, // What is the stride (i.e. bytes between normals)?
						model.normals);	    // Where is normal data in memory?
			}

			if(model.texcoords != null)
			{
				// Enable the texcoord attribute for this VAO
				m_kGL.glEnableVertexAttribArray(TEXCOORD_ATTRIB_IDX);

				// Get the size of the texcoord type so we can set the stride properly
				int texcoordTypeSize = Buffers.SIZEOF_FLOAT;

				// Set up parmeters for texcoord attribute in the VAO including, 
				//   size, type, stride, and offset in the currenly bound VAO
				// This also attaches the texcoord array in memory to the VAO	
				m_kGL.glVertexAttribPointer(TEXCOORD_ATTRIB_IDX,	// What attibute index will this array feed in the vertex shader (see buildProgram)
						model.texcoordSize,	// How many elements are there per texture coord?
						model.texcoordType,	// What is the type of this data in the array?
						false,				// Do we want to normalize this data (0-1 range for fixed-point types)
						model.texcoordSize*texcoordTypeSize,  // What is the stride (i.e. bytes between texcoords)?
						model.texcoords);	// Where is the texcood data in memory?
			}
		}
		 */

		GetGLError();

		return vaoName;
	}

	private void destroyVAO( int[] vaoName )
	{
		int index;
		int[] bufName = new int[1];

		// Bind the VAO so we can get data from it
		m_kGL.glBindVertexArray(vaoName[0]);

		// For every possible attribute set in the VAO
		for(index = 0; index < 16; index++)
		{
			// Get the VBO set for that attibute
			m_kGL.glGetVertexAttribiv(index, GL2.GL_VERTEX_ATTRIB_ARRAY_BUFFER_BINDING, bufName, 0);

			// If there was a VBO set...
			if(bufName[0] != -1)
			{
				//...delete the VBO
				m_kGL.glDeleteBuffers(1, bufName, 0);
			}
		}

		// Get any element array VBO set in the VAO
		m_kGL.glGetIntegerv(GL.GL_ELEMENT_ARRAY_BUFFER_BINDING, bufName, 0);

		// If there was a element array VBO set in the VAO
		if(bufName[0] != -1)
		{
			//...delete the VBO
			m_kGL.glDeleteBuffers(1, bufName, 0);
		}

		// Finally, delete the VAO
		m_kGL.glDeleteVertexArrays(1, vaoName, 0);

		GetGLError();
	}


	private int[] buildTexture(demoImage image )
	{
		int[] texName = new int[1];

		// Create a texture object to apply to model
		m_kGL.glGenTextures(1, texName, 0);
		m_kGL.glBindTexture(GL.GL_TEXTURE_2D, texName[0]);

		// Set up filter and wrap modes for this texture object
		m_kGL.glTexParameteri(GL.GL_TEXTURE_2D, GL.GL_TEXTURE_WRAP_S, GL.GL_CLAMP_TO_EDGE);
		m_kGL.glTexParameteri(GL.GL_TEXTURE_2D, GL.GL_TEXTURE_WRAP_T, GL.GL_CLAMP_TO_EDGE);
		m_kGL.glTexParameteri(GL.GL_TEXTURE_2D, GL.GL_TEXTURE_MAG_FILTER, GL.GL_LINEAR);
		m_kGL.glTexParameteri(GL.GL_TEXTURE_2D, GL.GL_TEXTURE_MIN_FILTER, GL.GL_LINEAR_MIPMAP_LINEAR);

		// Indicate that pixel rows are tightly packed 
		//  (defaults to stride of 4 which is kind of only good for
		//  RGBA or FLOAT data types)
		m_kGL.glPixelStorei(GL.GL_UNPACK_ALIGNMENT, 1);

		ByteBuffer tData = Buffers.newDirectByteBuffer( image.data );        
		tData.rewind();
		// Allocate and load image data into texture
		m_kGL.glTexImage2D(GL.GL_TEXTURE_2D, 0, image.format, image.width, image.height, 0,
				image.format, image.type, tData);

		// Create mipmaps for this texture for better image quality
		m_kGL.glGenerateMipmap(GL.GL_TEXTURE_2D);

		GetGLError();

		return texName;
	}


	private void deleteFBOAttachment( int attachment )
	{    
		int[] param = new int[1];

		m_kGL.glGetFramebufferAttachmentParameteriv(GL.GL_FRAMEBUFFER, attachment,
				GL.GL_FRAMEBUFFER_ATTACHMENT_OBJECT_TYPE,
				param, 0);

		if(GL.GL_RENDERBUFFER == param[0])
		{
			m_kGL.glGetFramebufferAttachmentParameteriv(GL.GL_FRAMEBUFFER, attachment,
					GL.GL_FRAMEBUFFER_ATTACHMENT_OBJECT_NAME,
					param, 0);

			m_kGL.glDeleteRenderbuffers(1, param, 0);
		}
		else if(GL.GL_TEXTURE == param[0])
		{

			m_kGL.glGetFramebufferAttachmentParameteriv(GL.GL_FRAMEBUFFER, attachment,
					GL.GL_FRAMEBUFFER_ATTACHMENT_OBJECT_NAME,
					param, 0);

			m_kGL.glDeleteTextures(1, param, 0);
		}

	}

	private void destroyFBO( int[] fboName )
	{ 
		if(0 == fboName[0])
		{
			return;
		}

		m_kGL.glBindFramebuffer(GL.GL_FRAMEBUFFER, fboName[0]);


		int[] maxColorAttachments = new int[]{1};


		// OpenGL ES on iOS 4 has only 1 attachment. 
		// There are many possible attachments on OpenGL 
		// on MacOSX so we query how many below
		//#if !ESSENTIAL_GL_PRACTICES_IOS
		m_kGL.glGetIntegerv(GL2.GL_MAX_COLOR_ATTACHMENTS, maxColorAttachments, 0);
		//#endif

		int colorAttachment;

		// For every color buffer attached
		for(colorAttachment = 0; colorAttachment < maxColorAttachments[0]; colorAttachment++)
		{
			// Delete the attachment
			deleteFBOAttachment(GL.GL_COLOR_ATTACHMENT0+colorAttachment);
		}

		// Delete any depth or stencil buffer attached
		deleteFBOAttachment(GL.GL_DEPTH_ATTACHMENT);

		deleteFBOAttachment(GL.GL_STENCIL_ATTACHMENT);

		m_kGL.glDeleteFramebuffers(1,fboName, 0);
	}



	private int[] buildFBOWithWidth( int width, int height)
	{
		int[] fboName = new int[1];
		int[] colorTexture = new int[1];

		// Create a texture object to apply to model
		m_kGL.glGenTextures(1, colorTexture, 0);
		m_kGL.glBindTexture(GL.GL_TEXTURE_2D, colorTexture[0]);

		// Set up filter and wrap modes for this texture object
		m_kGL.glTexParameteri(GL.GL_TEXTURE_2D, GL.GL_TEXTURE_WRAP_S, GL.GL_CLAMP_TO_EDGE);
		m_kGL.glTexParameteri(GL.GL_TEXTURE_2D, GL.GL_TEXTURE_WRAP_T, GL.GL_CLAMP_TO_EDGE);
		m_kGL.glTexParameteri(GL.GL_TEXTURE_2D, GL.GL_TEXTURE_MAG_FILTER, GL.GL_LINEAR);
		//#if ESSENTIAL_GL_PRACTICES_IOS
		//m_kGL.glTexParameteri(GL.GL_TEXTURE_2D, GL.GL_TEXTURE_MIN_FILTER, GL.GL_LINEAR);
		//#else
		m_kGL.glTexParameteri(GL.GL_TEXTURE_2D, GL.GL_TEXTURE_MIN_FILTER, GL.GL_LINEAR_MIPMAP_LINEAR);
		//#endif

		// Allocate a texture image with which we can render to
		// Pass NULL for the data parameter since we don't need to load image data.
		//     We will be generating the image by rendering to this texture
		m_kGL.glTexImage2D(GL.GL_TEXTURE_2D, 0, GL.GL_RGBA, 
				width, height, 0,
				GL.GL_RGBA, GL.GL_UNSIGNED_BYTE, null);

		int[] depthRenderbuffer = new int[1];
		m_kGL.glGenRenderbuffers(1, depthRenderbuffer, 0);
		m_kGL.glBindRenderbuffer(GL.GL_RENDERBUFFER, depthRenderbuffer[0]);
		m_kGL.glRenderbufferStorage(GL.GL_RENDERBUFFER, GL.GL_DEPTH_COMPONENT16, width, height);

		m_kGL.glGenFramebuffers(1, fboName, 0);
		m_kGL.glBindFramebuffer(GL.GL_FRAMEBUFFER, fboName[0]);	
		m_kGL.glFramebufferTexture2D(GL.GL_FRAMEBUFFER, GL.GL_COLOR_ATTACHMENT0, GL.GL_TEXTURE_2D, colorTexture[0], 0);
		m_kGL.glFramebufferRenderbuffer(GL.GL_FRAMEBUFFER, GL.GL_DEPTH_ATTACHMENT, GL.GL_RENDERBUFFER, depthRenderbuffer[0]);

		if(m_kGL.glCheckFramebufferStatus(GL.GL_FRAMEBUFFER) != GL.GL_FRAMEBUFFER_COMPLETE)
		{
			System.err.println("failed to make complete framebuffer object " + m_kGL.glCheckFramebufferStatus(GL.GL_FRAMEBUFFER));
			destroyFBO(fboName);
			return null;
		}

		GetGLError();

		return fboName;
	}

	private int buildProgramWithVertexSource(demoSource vertexSource, demoSource fragmentSource, 
			boolean hasNormal, boolean hasTexcoord )
	{
		int prgName;

		int[] logLength = new int[1];
		int[] status = new int[1];

		// String to pass to glShaderSource
		String[] sourceString = new String[]{new String()};  

		// Determine if GLSL version 140 is supported by this context.
		//  We'll use this info to generate a GLSL shader source string  
		//  with the proper version preprocessor string prepended
		float  glLanguageVersion;

		//#if ESSENTIAL_GL_PRACTICES_IOS
		//sscanf((char *)glGetString(GL_SHADING_LANGUAGE_VERSION), "OpenGL ES GLSL ES %f", &glLanguageVersion);
		//#else
		String versionString = m_kGL.glGetString(GL2.GL_SHADING_LANGUAGE_VERSION);
		//System.err.println(versionString);
		if ( versionString.indexOf(" ") != -1 )
		{
			String num = versionString.substring( 0, versionString.indexOf(" ") );
			//System.err.println(num);
			glLanguageVersion = new Float(num).floatValue();	
		}
		else
		{
			glLanguageVersion = new Float(versionString).floatValue();
		}
		//System.err.println(glLanguageVersion);
		//#endif

		// GL_SHADING_LANGUAGE_VERSION returns the version standard version form 
		//  with decimals, but the GLSL version preprocessor directive simply
		//  uses integers (thus 1.10 should 110 and 1.40 should be 140, etc.)
		//  We multiply the floating point number by 100 to get a proper
		//  number for the GLSL preprocessor directive
		int version =  (int) (100.0 * glLanguageVersion + .5);	
		//System.err.println(version);

		// Get the size of the version preprocessor string info so we know 
		//  how much memory to allocate for our sourceString
		//int versionStringSize = sizeof("#version 123\n");

		// Create a program object
		prgName = m_kGL.glCreateProgram();

		// Indicate the attribute indicies on which vertex arrays will be
		//  set with glVertexAttribPointer
		//  See buildVAO to see where vertex arrays are actually set
		m_kGL.glBindAttribLocation(prgName, POS_ATTRIB_IDX, "inPosition");

		if(hasNormal)
		{
			m_kGL.glBindAttribLocation(prgName, NORMAL_ATTRIB_IDX, "inNormal");
		}

		if(hasTexcoord)
		{
			m_kGL.glBindAttribLocation(prgName, TEXCOORD_ATTRIB_IDX, "inTexcoord");
		}

		//////////////////////////////////////
		// Specify and compile VertexShader //
		//////////////////////////////////////

		// Allocate memory for the source string including the version preprocessor information

		// Prepend our vertex shader source string with the supported GLSL version so
		//  the shader will work on ES, Legacy, and OpenGL 3.2 Core Profile contexts
		sourceString[0] += "#version " + version + "\n" + vertexSource.shaderProgram;

		int[] stringLength = new int[1];
		stringLength[0] = sourceString[0].length();
		int vertexShader = m_kGL.glCreateShader(GL2.GL_VERTEX_SHADER);	
		m_kGL.glShaderSource(vertexShader, 1, sourceString, stringLength, 0);
		m_kGL.glCompileShader(vertexShader);
		m_kGL.glGetShaderiv(vertexShader, GL2.GL_INFO_LOG_LENGTH, logLength, 0);

		if (logLength[0] > 0) 
		{
			byte[] abInfoLog = new byte[logLength[0]];
			m_kGL.glGetShaderInfoLog(vertexShader, logLength[0], logLength, 0, abInfoLog, 0);
			System.err.println( new String(abInfoLog) );
		}

		m_kGL.glGetShaderiv(vertexShader, GL2.GL_COMPILE_STATUS, status, 0);
		if (status[0] == 0)
		{
			System.err.println("Failed to compile vtx shader:\n" + sourceString );
			return 0;
		}

		sourceString[0] = null;

		// Attach the vertex shader to our program
		m_kGL.glAttachShader(prgName, vertexShader);


		/////////////////////////////////////////
		// Specify and compile Fragment Shader //
		/////////////////////////////////////////

		// Allocate memory for the source string including the version preprocessor	 information
		//sourceString = malloc(fragmentSource->byteSize + versionStringSize);
		sourceString[0] = new String();

		// Prepend our fragment shader source string with the supported GLSL version so
		//  the shader will work on ES, Legacy, and OpenGL 3.2 Core Profile contexts
		sourceString[0] +="#version " + version + "\n" + fragmentSource.shaderProgram;
		stringLength[0] = sourceString[0].length();

		int fragShader = m_kGL.glCreateShader(GL2.GL_FRAGMENT_SHADER);	
		m_kGL.glShaderSource(fragShader, 1, sourceString, stringLength, 0);
		m_kGL.glCompileShader(fragShader);
		m_kGL.glGetShaderiv(fragShader, GL2.GL_INFO_LOG_LENGTH, logLength, 0);

		if (logLength[0] > 0) 
		{
			byte[] abInfoLog = new byte[logLength[0]];
			m_kGL.glGetShaderInfoLog(fragShader, logLength[0], logLength, 0, abInfoLog, 0);
			System.err.println( new String(abInfoLog) );
		}

		m_kGL.glGetShaderiv(fragShader, GL2.GL_COMPILE_STATUS, status, 0);
		if (status[0] == 0)
		{
			System.err.println("Failed to compile frag shader:\n" + sourceString );
			return 0;
		}
		sourceString[0] = null;

		// Attach the fragment shader to our program
		m_kGL.glAttachShader(prgName, fragShader);


		//////////////////////
		// Link the program //
		//////////////////////

		m_kGL.glLinkProgram(prgName);
		m_kGL.glGetProgramiv(prgName, GL2.GL_INFO_LOG_LENGTH, logLength, 0);
		if (logLength[0] > 0)
		{
			byte[] abInfoLog = new byte[logLength[0]];
			m_kGL.glGetProgramInfoLog(prgName, logLength[0], logLength, 0, abInfoLog, 0);
			System.err.println( new String(abInfoLog) );
		}

		m_kGL.glGetProgramiv(prgName, GL2.GL_LINK_STATUS, status, 0);
		if (status[0] == 0)
		{
			System.err.println("Failed to link program");
			return 0;
		}

		m_kGL.glValidateProgram(prgName);
		m_kGL.glGetProgramiv(prgName, GL2.GL_INFO_LOG_LENGTH, logLength, 0);
		if (logLength[0] > 0)
		{
			byte[] abInfoLog = new byte[logLength[0]];
			m_kGL.glGetProgramInfoLog(prgName, logLength[0], logLength, 0, abInfoLog, 0);
			System.err.println( new String(abInfoLog) );
		}

		m_kGL.glGetProgramiv(prgName, GL2.GL_VALIDATE_STATUS, status, 0);
		if (status[0] == 0)
		{
			System.err.println("Failed to validate program");
			return 0;
		}


		m_kGL.glUseProgram(prgName);

		///////////////////////////////////////
		// Setup common program input points //
		///////////////////////////////////////


		int samplerLoc = m_kGL.glGetUniformLocation(prgName, "diffuseTexture");

		// Indicate that the diffuse texture will be bound to texture unit 0
		int unit = 0;
		m_kGL.glUniform1i(samplerLoc, unit);

		GetGLError();

		return prgName;

	}

	private void destroyProgram( int prgName )
	{	

		if(0 == prgName)
		{
			return;
		}

		int shaderNum;
		int[] shaderCount = new int[1];

		// Get the number of attached shaders
		m_kGL.glGetProgramiv(prgName, GL2.GL_ATTACHED_SHADERS, shaderCount, 0);

		int[] shaders = new int[shaderCount[0]];

		// Get the names of the shaders attached to the program
		m_kGL.glGetAttachedShaders(prgName,
				shaderCount[0],
				shaderCount, 0,
				shaders, 0);

		// Delete the shaders attached to the program
		for(shaderNum = 0; shaderNum < shaderCount[0]; shaderNum++)
		{
			m_kGL.glDeleteShader(shaders[shaderNum]);
		}

		shaders = null;

		// Delete the program
		m_kGL.glDeleteProgram(prgName);
		m_kGL.glUseProgram(0);
	}

	private void initWithDefaultFBO ( int defaultFBOName )
	{
		System.err.println( m_kGL.glGetString(GL.GL_RENDERER) + " " + 
				m_kGL.glGetString(GL.GL_VERSION) );

		////////////////////////////////////////////////////
		// Build all of our and setup initial state here  //
		// Don't wait until our real time run loop begins //
		////////////////////////////////////////////////////

		m_defaultFBOName = defaultFBOName;

		m_viewWidth = 512;
		m_viewHeight = 512;


		m_characterAngle = 0;

		m_useVBOs = true;


		//////////////////////////////
		// Load our character model //
		//////////////////////////////
		String dir = new String( "WildMagic" + File.separator + "ApplicationDemos" + File.separator + "GLEssentialSupport" + File.separator + "Resources" + File.separator ); 
		String filePathName = new String( dir + "demon.model" );
		m_characterModel = new demoModel(dir + "demon.model", "demon.model");

		// Build Vertex Buffer Objects (VBOs) and Vertex Array Object (VAOs) with our model data
		m_characterVAOName = buildVAO(m_characterModel);

		// Cache the number of element and primType to use later in our glDrawElements calls
		m_characterNumElements = m_characterModel.numElements;
		m_characterPrimType = m_characterModel.primType;
		m_characterElementType = m_characterModel.elementType;

		if(m_useVBOs)
		{
			//If we're using VBOs we can destroy all this memory since buffers are
			// loaded into GL and we've saved anything else we need
			m_characterModel.dispose();
			m_characterModel = null;
		}


		////////////////////////////////////
		// Load texture for our character //
		////////////////////////////////////

		filePathName = new String( dir + "demon.png" );
		demoImage image = new demoImage(filePathName, "demon.png", false);

		// Build a texture object with our image data
		m_characterTexName = buildTexture(image);

		// We can destroy the image once it's loaded into GL
		image.dispose();


		////////////////////////////////////////////////////
		// Load and Setup shaders for character rendering //
		////////////////////////////////////////////////////
		dir = new String( "WildMagic" + File.separator + "ApplicationDemos" + File.separator + "GLEssentialSupport" + File.separator + "Shaders" + File.separator ); 
		filePathName = new String( dir + "character.vsh" );
		demoSource vtxSource = new demoSource(filePathName, "character.vsh");

		filePathName = new String( dir + "character.fsh" );
		demoSource frgSource = new demoSource(filePathName, "character.fsh");

		// Build Program
		m_characterPrgName = buildProgramWithVertexSource(vtxSource, frgSource, false, true );

		vtxSource.dispose();
		vtxSource = null;
		frgSource.dispose();
		frgSource = null;


		m_characterMvpUniformIdx = m_kGL.glGetUniformLocation(m_characterPrgName, "modelViewProjectionMatrix");

		if(m_characterMvpUniformIdx < 0)
		{
			System.err.println("No modelViewProjectionMatrix in character shader");
		}

		m_reflectWidth = 512;
		m_reflectHeight = 512;

		////////////////////////////////////////////////
		// Load a model for a quad for the reflection //
		////////////////////////////////////////////////

		m_quadModel = new demoModel();
		// Build Vertex Buffer Objects (VBOs) and Vertex Array Object (VAOs) with our model data
		m_reflectVAOName = buildVAO(m_quadModel);

		// Cache the number of element and primType to use later in our glDrawElements calls
		m_quadNumElements = m_quadModel.numElements;
		m_quadPrimType    = m_quadModel.primType;
		m_quadElementType = m_quadModel.elementType;

		if(m_useVBOs)
		{
			//If we're using VBOs we can destroy all this memory since buffers are
			// loaded into GL and we've saved anything else we need 
			m_quadModel.dispose();
			m_quadModel = null;
		}

		/////////////////////////////////////////////////////
		// Create texture and FBO for reflection rendering //
		/////////////////////////////////////////////////////

		m_reflectFBOName = buildFBOWithWidth(m_reflectWidth, m_reflectHeight);

		// Get the texture we created in buildReflectFBO by binding the 
		// reflection FBO and getting the buffer attached to color 0
		m_kGL.glBindFramebuffer(GL.GL_FRAMEBUFFER, m_reflectFBOName[0]);

		int[] iReflectTexName = new int[1];
		m_kGL.glGetFramebufferAttachmentParameteriv(GL.GL_FRAMEBUFFER, GL.GL_COLOR_ATTACHMENT0,
				GL.GL_FRAMEBUFFER_ATTACHMENT_OBJECT_NAME,
				iReflectTexName, 0);

		m_reflectTexName = iReflectTexName[0];

		/////////////////////////////////////////////////////
		// Load and setup shaders for reflection rendering //
		/////////////////////////////////////////////////////

		filePathName = new String( dir + "reflect.vsh" );
		vtxSource = new demoSource(filePathName, "reflect.vsh");

		filePathName = new String( dir + "reflect.fsh" );
		frgSource = new demoSource(filePathName, "reflect.fsh");

		// Build Program
		m_reflectPrgName = buildProgramWithVertexSource(vtxSource, frgSource, true, false);
		vtxSource.dispose();
		vtxSource = null;
		frgSource.dispose();
		frgSource = null;


		m_reflectModelViewUniformIdx = m_kGL.glGetUniformLocation(m_reflectPrgName, "modelViewMatrix");
		if(m_reflectModelViewUniformIdx < 0)
		{
			System.err.println("No modelViewMatrix in reflection shader");
		}

		m_reflectProjectionUniformIdx = m_kGL.glGetUniformLocation(m_reflectPrgName, "modelViewProjectionMatrix");
		if(m_reflectProjectionUniformIdx < 0)
		{
			System.err.println("No modelViewProjectionMatrix in reflection shader");
		}

		m_reflectNormalMatrixUniformIdx = m_kGL.glGetUniformLocation(m_reflectPrgName, "normalMatrix");
		if(m_reflectNormalMatrixUniformIdx < 0)
		{
			System.err.println("No normalMatrix in reflection shader");
		}

		////////////////////////////////////////////////
		// Set up OpenGL state that will never change //
		////////////////////////////////////////////////

		// Depth test will always be enabled
		m_kGL.glEnable(GL.GL_DEPTH_TEST);

		// We will always cull back faces for better performance
		m_kGL.glEnable(GL.GL_CULL_FACE);

		// Always use this clear color
		m_kGL.glClearColor(0.5f, 0.4f, 0.5f, 1.0f);

		// Draw our scene once without presenting the rendered image.
		//   This is done in order to pre-warm OpenGL
		// We don't need to present the buffer since we don't actually want the 
		//   user to see this, we're only drawing as a pre-warm stage
		//[self render];

		// Reset the m_characterAngle which is incremented in render
		m_characterAngle = 0;

		// Check for errors to make sure all of our setup went ok
		GetGLError();
		
		m_bInit = true;
	}


	public void dispose(GLAutoDrawable arg0)
	{

		// Cleanup all OpenGL objects and 
		m_kGL.glDeleteTextures(1, m_characterTexName, 0);

		destroyVAO(m_characterVAOName);

		destroyProgram(m_characterPrgName);

		m_characterModel.dispose();

		destroyFBO(m_reflectFBOName);

		destroyVAO(m_reflectVAOName);

		destroyProgram(m_reflectPrgName);

		m_quadModel.dispose();
	}

	private void GetGLError()
	{		
		int err = m_kGL.glGetError();
		while (err != GL.GL_NO_ERROR && m_kGL.glGetString(err) != null) {
			System.err.println("GLError: " + 
					m_kGL.glGetString(err) );
			err = m_kGL.glGetError();
		}
	}

}
