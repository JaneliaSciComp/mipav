package gov.nih.mipav.view.renderer.WildMagic;

import gov.nih.mipav.model.algorithms.AlgorithmTransform;
import gov.nih.mipav.model.structures.ModelImage;
import gov.nih.mipav.model.structures.TransMatrix;
import gov.nih.mipav.util.MipavInitGPU;
import gov.nih.mipav.view.MipavUtil;
import gov.nih.mipav.view.Preferences;
import gov.nih.mipav.view.ViewJFrameImage;
import gov.nih.mipav.view.input.spacenav.SpaceNavigatorController;
import gov.nih.mipav.view.input.spacenav.SpaceNavigatorEvent;
import gov.nih.mipav.view.input.spacenav.SpaceNavigatorListener;
import gov.nih.mipav.view.input.spacenav.SpaceNavigatorPoller;
import gov.nih.mipav.view.renderer.WildMagic.Render.VolumeImage;
import gov.nih.mipav.view.renderer.WildMagic.Render.VolumeNode;
import gov.nih.mipav.view.renderer.WildMagic.Render.VolumeObject;
import gov.nih.mipav.view.renderer.WildMagic.Render.VolumeSurface;
import gov.nih.mipav.view.renderer.flythroughview.JpegImagesToMovie;
import gov.nih.mipav.view.renderer.flythroughview.MovieMaker;

import java.awt.*;
import java.awt.event.KeyEvent;
import java.awt.event.KeyListener;
import java.awt.event.MouseEvent;
import java.awt.event.MouseListener;
import java.awt.event.MouseMotionListener;
import java.awt.event.WindowAdapter;
import java.awt.event.WindowEvent;
import java.awt.image.BufferedImage;
import java.beans.PropertyChangeEvent;
import java.beans.PropertyChangeListener;
import java.io.File;
import java.io.IOException;
import java.text.DecimalFormat;
import java.util.Vector;

import javax.imageio.ImageIO;
import javax.media.MediaLocator;
import javax.media.opengl.GLAutoDrawable;
import javax.media.opengl.GLEventListener;
import javax.media.opengl.awt.GLCanvas;
import javax.swing.JDialog;
import javax.swing.JFrame;
import javax.swing.JOptionPane;

import WildMagic.LibApplications.OpenGLApplication.JavaApplication3D;
import WildMagic.LibFoundation.Mathematics.ColorRGBA;
import WildMagic.LibFoundation.Mathematics.Matrix3f;
import WildMagic.LibFoundation.Mathematics.Matrix4f;
import WildMagic.LibFoundation.Mathematics.Quaternion;
import WildMagic.LibFoundation.Mathematics.Vector3f;
import WildMagic.LibGraphics.Collision.Picker;
import WildMagic.LibGraphics.Rendering.CullState;
import WildMagic.LibGraphics.Rendering.Light;
import WildMagic.LibGraphics.SceneGraph.Culler;
import WildMagic.LibGraphics.SceneGraph.Node;
import WildMagic.LibGraphics.SceneGraph.Spatial;
import WildMagic.LibRenderers.OpenGLRenderer.OpenGLRenderer;

import com.jogamp.opengl.util.Animator;


public abstract class GPURenderBase extends JavaApplication3D
implements GLEventListener, KeyListener, MouseMotionListener,  MouseListener, SpaceNavigatorListener
{
	private static final long serialVersionUID = 9069227710441839806L;
    /** VolumeImage for ModelImageA, contains data and textures. */
    protected VolumeImage m_kVolumeImageA;
    /** VolumeImage for ModelImageB, contains data and textures. */
    protected VolumeImage m_kVolumeImageB;
    
    protected VolumeImage m_kVolumeImageA_Old;
    protected VolumeImage m_kVolumeImageB_Old;
    
    /** Scene-graph root node: */
    protected Node m_spkScene;
    /** Culling: turns backface/frontface culling on/off: */
    protected CullState m_spkCull;
    /** Culling out-of-view objects: */
    protected Culler m_kCuller = new Culler(0,0,null);

    /** Animator object, displays scene in rendering loop (similar to GLUTMainLoop() */
    protected Animator m_kAnimator;

    /** Set to true when init() is called: */
    protected boolean m_bInit = false;
    /** Scene translation, centers the scene: */
    protected Vector3f m_kTranslate = new Vector3f(Vector3f.ZERO);
    protected Matrix4f m_kSceneToWorld = new Matrix4f();
    /** Normalized volume extents: */
    protected float m_fX = 1f, m_fY = 1f, m_fZ = 1f, m_fMax = 1f;
    /** Flag for indicating the that Java Container is visible or not: */
    protected boolean m_bVisible = true;

    /** Lights from JPanelLight */
    protected Light[] m_akLights = null;

    /** For testing the frame rate: */
    protected boolean m_bTestFrameRate = false;
    /** Basic rotations for fixed input use: */
    protected Matrix3f m_kZRotate = new Matrix3f(), m_kYRotate = new Matrix3f(), m_kXRotate = new Matrix3f();
    /** List of objects displayed in the scene. */
    protected Vector<VolumeObject> m_kDisplayList = new Vector<VolumeObject>();
    /** Set to true when the surface has been added or modified. */
    protected boolean m_bSurfaceUpdate = false;
    /** Set to true when the surface mask should be recalculated. */
    protected boolean m_bSurfaceMaskUpdate = false;
    protected int m_iUpdateNormals = -1;
    protected boolean m_bExtract = false;
    
    protected boolean m_bDoClip = true;

    /** Picking support: */
    protected int m_iXPick = -1, m_iYPick = -1;
    /** Set to true when there is a pending pick operation */
    protected boolean m_bPickPending = false;
    /** For processing picking: */
    protected Picker m_kPicker = new Picker();
    /** Minimizes rendering, only render when modified is true. */
    protected boolean m_bModified = true;
    /** Screen capture counter. */
    private int m_iScreenCaptureCounter = 0;
    private int m_iMovieCounter = 0;

    /** Set to true when recording. */
    protected boolean m_bSnapshot = false;
    protected int m_iCaptureFPS;

    protected boolean m_bShared = false;
    protected boolean m_bStandAlone = false;

    protected Vector<VolumeObject> m_kDeleteList = new Vector<VolumeObject>();
	protected boolean updateBoundingCube = false;
    
	protected float nearPlane = 0.01f;
	
    protected boolean isSpaceNavCodeRunning = false;
    /** actual min and max values for the 3D mouse (the space navigator) */
    private static float spaceNavMax, spaceNavMin;
    /** logical min and max mouse values for calculations, this is the range of the normalized input values */
    private static int mouseMax = 100, mouseMin = -100;
    /** used for slowing down the rate that the 3D mouse moves the image*/
    private static float rotationScaleFactor = 0.5f;
	private static int translationScaleFactor = 3;
    /** used as cutoffs for making space navigator mouse less sensitive to the low range values that may not be intentional user input */
    private static int translationCutoff = 10, rotationCutoff = 10;
    /** used to change the speed that the 3D mouse moves the camera*/
    private static double mouseSpeedScaler = 1;
    /** used to invert the settings of the mouse */
    private static int invertTX = 1, invertTY = 1, invertTZ = 1, invertRX = 1, invertRY = 1, invertRZ = 1;
    /** holds the current value of the space navigator. normalized on a -100 to 100 scale for cross platform/calculating convenience */
    private static double tX, tY, tZ, rX, rY, rZ;
    
    private boolean useMouseTranslationCutoffs = true, useMouseRotationCutoffs = true;
    
    static{
    	String osName = System.getProperty("os.name").toLowerCase();
    	if(osName.startsWith("windows")) {
    		spaceNavMax = 1400;
    		spaceNavMin = -1400;
    	} else {
    		spaceNavMax = .35f;
    		spaceNavMin = -.35f;
    	} 
    }
    /**
     * Default GPURenderBase constructor.
     */
    public GPURenderBase()
    {
        super("GPUVolumeRender",0,0,512,512, new ColorRGBA(0.0f,0.0f,0.0f,0.0f));
    
        try {
    		if(SpaceNavigatorController.hasSpaceNavigator() && SpaceNavigatorPoller.getListeners().length == 0) {
    			SpaceNavigatorPoller.registerListener(this);
    		}
    	} catch (Error e) {
    		Preferences.debug("Unable to load space navigator libraries.  See console output for details.\n", Preferences.DEBUG_MINOR);
    		e.printStackTrace();
    	}
        checkIfSpaceNavNeedsCalibration();
    }
    
    /**
     * @param kParent
     * @param kAnimator
     * @param kVolumeImageA
     * @param kVolumeImageB
     */
    public GPURenderBase( VolumeTriPlanarInterface kParent, Animator kAnimator, 
            VolumeImage kVolumeImageA, VolumeImage kVolumeImageB )
    {
        super("GPUVolumeRender",0,0,512,512, new ColorRGBA(0.0f,0.0f,0.0f,0.0f));
        m_pkRenderer = new OpenGLRenderer( m_eFormat, m_eDepth, m_eStencil,
                m_eBuffering, m_eMultisampling,
                m_iWidth, m_iHeight );
        ((OpenGLRenderer)m_pkRenderer).GetCanvas().addGLEventListener( this );       
        ((OpenGLRenderer)m_pkRenderer).GetCanvas().addKeyListener( this );       
        ((OpenGLRenderer)m_pkRenderer).GetCanvas().addMouseListener( this );       
        ((OpenGLRenderer)m_pkRenderer).GetCanvas().addMouseMotionListener( this );       
        ((OpenGLRenderer)m_pkRenderer).GetCanvas().addMouseWheelListener( this ); 
        m_pkRenderer.SetExternalDir(MipavInitGPU.getExternalDirs());      

        m_kAnimator = kAnimator;
        m_kVolumeImageA = kVolumeImageA;
        m_kVolumeImageB = kVolumeImageB;

        m_kZRotate.fromAxisAngle(Vector3f.UNIT_Z, (float)Math.PI/18.0f);
        m_kYRotate.fromAxisAngle(Vector3f.UNIT_Y, (float)Math.PI/18.0f);
        m_kXRotate.fromAxisAngle(Vector3f.UNIT_X, (float)Math.PI/18.0f);
        
        try {
    		if(SpaceNavigatorController.hasSpaceNavigator() && SpaceNavigatorPoller.getListeners().length == 0) {
    			SpaceNavigatorPoller.registerListener(this);
    		}
    	} catch (Error e) {
    		Preferences.debug("Unable to load space navigator libraries.  See console output for details.\n", Preferences.DEBUG_MINOR);
    		e.printStackTrace();
    	}
        checkIfSpaceNavNeedsCalibration();
    
    }
    
    /**
     * Add a new scene-graph node to the display list.
     * @param kNode
     */
    public VolumeObject AddNode(Node kNode)
    {
        VolumeNode kVNode = new VolumeNode( m_kVolumeImageA,
                m_kTranslate,
                m_fX, m_fY, m_fZ, kNode);
        m_kDisplayList.add( 1, kVNode );
        UpdateSceneRotation();
        return kVNode;
    }
    
    protected boolean m_bDispose = false;
	public void dispose()
    {
		m_bDispose = true;
		//GetCanvas().display();
    }

    /**
     * memory cleanup.
     */
	public void dispose(GLAutoDrawable kDrawable)
    {
        m_kVolumeImageA = null;
        m_kVolumeImageB = null;
        if ( m_kAnimator != null ) {
        	m_kAnimator.remove(kDrawable);
            m_kAnimator.stop();
        	m_kAnimator = null;
        }
        if ( m_spkScene != null )
        {
            m_spkScene.dispose();
            m_spkScene = null;
        }

        if ( m_spkCull != null )
        {
            m_spkCull.dispose();
            m_spkCull = null;
        }
        if ( m_kCuller != null )
        {
            m_kCuller.dispose();
            m_kCuller = null;
        }
        m_kTranslate = null;
        if ( m_akLights != null )
        {
            for ( int i = 0; i < m_akLights.length; i++ )
            {
                if ( m_akLights[i] != null )
                {
                    m_akLights[i].dispose();
                    m_akLights[i] = null;
                }
            }
            m_akLights = null;
        }
        
        m_kPicker = null;

        if ( m_kDisplayList != null )
        {
            for ( int i = 0; i < m_kDisplayList.size(); i++ )
            {
                m_kDisplayList.get(i).dispose(m_pkRenderer);
            }
            m_kDisplayList.clear();
            m_kDisplayList = null;
        }
        
        m_kZRotate = null;
        
        SpaceNavigatorPoller.deRegisterListener(this);


        super.dispose();
    }

    /**
     * Returns the GLCanvas in the m_pkRenderer object.
     * @return OpenGLRenderer.GLCanvas
     */
    public GLCanvas GetCanvas()
    {
        return ((OpenGLRenderer)m_pkRenderer).GetCanvas();
    }

    /**
     * @return current screen capture number.
     */
    public int getCounter() {
        return m_iScreenCaptureCounter;
    }

    /**
     * @return ModelImage A.
     */
    public ModelImage getImage()
    {
        return m_kVolumeImageA.GetImage();
    }

    /**
     * @return ModelImage A.
     */
    public VolumeImage getVolumeImage()
    {
        return m_kVolumeImageA;
    }


    /**
     * @return the current light configuration.
     */
    public Light[] GetLights()
    {
        return m_akLights;
    }
    
    /**
     * Sets blending between imageA and imageB.
     * @param fValue the blend value (0-1)
     */
    public VolumeNode GetNode( String kSurfaceName )
    {
        for ( int i = 0; i < m_kDisplayList.size(); i++ )
        {
            if ( m_kDisplayList.get(i).GetName() != null )
            {
                if ( m_kDisplayList.get(i).GetName().equals(kSurfaceName))
                {
                    VolumeObject kObj = m_kDisplayList.get(i);
                    if ( kObj instanceof VolumeSurface )
                    {
                        return (VolumeNode)kObj;                  
                    }
                }
            }
        }
        return null;
    }

    /**
     * @return translation vector center of the volume rendered scene.
     */
    public Vector3f getTranslate() {
    	return m_kTranslate;
    }

    /**
     * Part of the KeyListener interface. 
     * @param e the key event.
     */
    @Override
	public void keyPressed(KeyEvent e) {
        char ucKey = e.getKeyChar();
        
        
        if(e.isAltDown()) {
	        
        	
        	boolean rotateProcessed = false;
	        Matrix3f rotateMatrix = null;
	        switch(e.getKeyCode()) {
	        case KeyEvent.VK_UP:
	        	rotateProcessed = true;
	        	rotateMatrix = Matrix3f.inverse(m_kXRotate);
	        	break;
	        	
	        case KeyEvent.VK_DOWN:
	        	rotateProcessed = true;
	        	rotateMatrix = m_kXRotate;
	        	break;
	        	
	        case KeyEvent.VK_RIGHT:
	        	rotateProcessed = true;
	        	rotateMatrix = m_kZRotate;
	        	break;
	        	
	        case KeyEvent.VK_LEFT:
	        	rotateProcessed = true;
	        	rotateMatrix = Matrix3f.inverse(m_kZRotate);
	        	break;
	        }
	        if(rotateProcessed) {	
	        	updateScene(rotateMatrix);
		        return;
	        }
        }
        
        
        super.keyPressed(e);
        switch (ucKey)
        {
        case 'f':
            m_bTestFrameRate = !m_bTestFrameRate;
            if ( m_bTestFrameRate )
            {
                ResetTime();
            }
            return;
        case 'q':
            setOrthographicProjection();
            return;
        case 'Q':
            setPerspectiveProjection();
            return;
        }

        return;
    }

    private void updateScene(Matrix3f rotateMatrix) {
        if(m_spkScene != null){
	    	Matrix3f kRotate = m_spkScene.Local.GetRotate();
	        kRotate.mult(rotateMatrix);
	        m_spkScene.Local.SetRotate(kRotate);
	        m_spkScene.UpdateGS();
	        m_kCuller.ComputeVisibleSet(m_spkScene);
	        
	        for ( int i = 0; i < m_kDisplayList.size(); i++ ) {
	            m_kDisplayList.get(i).GetScene().Local.SetRotateCopy(m_spkScene.Local.GetRotate());
	        }
        }
        return;
    }
    
    /* (non-Javadoc)
     * @see gov.nih.mipav.view.renderer.flythroughview.FlyThroughRenderInterface#record(boolean)
     */
    public void record(boolean bOn)
    {
        m_bSnapshot = bOn;
        if ( m_bSnapshot )
        {
            m_iCaptureFPS = (int)m_dFrameRate;
        }
        if ( !m_bSnapshot )
        {
           // instead of saving AVI movie, we save the QuickTime Movie. 
            saveQuickTimeMovie();
        }
    }
    
    public void reInitialize(VolumeImage kImageA, VolumeImage kImageB)
    {
    	m_kVolumeImageA_Old = m_kVolumeImageA;
    	m_kVolumeImageB_Old = m_kVolumeImageB;
    	m_kVolumeImageA = kImageA;
    	m_kVolumeImageB = kImageB;
    	m_bInit = false;
    }

    public VolumeObject RemoveNode( String kNodeName )
    {
        for ( int i = 0; i < m_kDisplayList.size(); i++ )
        {
            if ( m_kDisplayList.get(i).GetName() != null )
            {
                if ( m_kDisplayList.get(i).GetName().equals(kNodeName))
                {
                    VolumeObject kObj = m_kDisplayList.remove(i);
                    return kObj;
                }
            }
        }
        return null;
    }

    /**
     * Reset the z-axis. 
     */
    public void resetAxis()
    {
        m_spkScene.Local.SetRotateCopy( Matrix3f.IDENTITY );
        UpdateSceneRotation();
        updateBoundingCube = true;
    }

    /**
     * Reset x-axis. 
     */
    public void resetAxisX()
    {
        m_spkScene.Local.SetRotateCopy( Matrix3f.IDENTITY );
        m_spkScene.Local.GetRotate().fromAxisAngle( Vector3f.UNIT_X, (float)Math.PI/2.0f );
        UpdateSceneRotation();
        updateBoundingCube = true;
    }


    /**
     * Reset y-axis.
     */
    public void resetAxisY()
    {
        m_spkScene.Local.SetRotateCopy( Matrix3f.IDENTITY );
        m_spkScene.Local.GetRotate().fromAxisAngle( Vector3f.UNIT_Y, -(float)Math.PI/2.0f );
        UpdateSceneRotation();
        updateBoundingCube = true;
    }

    /**
     * Part of the GLEventListener interface. Called when the GLCanvas is resized.
     * @param arg0 the GLCanvas
     * @param iX the new x-position in screen coordinates.
     * @param iY the new y-position in screen coordinates.
     * @param iWidth the new width
     * @param iHeight the new height
     */
    @Override
	public void reshape(GLAutoDrawable arg0, int iX, int iY, int iWidth, int iHeight) {
        //if ( m_pkRenderer != null )
        //{
        //    ((OpenGLRenderer)m_pkRenderer).SetDrawable( arg0 );
        //}
    	if ( m_kVolumeImageA == null ) {
        	return;
        }
        if (iWidth > 0 && iHeight > 0)
        {
            m_iWidth = iWidth;
            m_iHeight = iHeight;
            if ( m_spkCamera != null ) {
	            if ( m_spkCamera.Perspective )
	            {
	                m_spkCamera.SetFrustum(30.0f,m_iWidth/(float)m_iHeight,getNearPlane(),10.0f);
	            }
	            else
	            {
	                m_spkCamera.SetFrustum(30.0f,m_iWidth/(float)m_iHeight,1f,10.0f);
	            }
            }

            if (m_pkRenderer != null)
            {
                m_pkRenderer.Resize(iWidth,iHeight);
            }
        }
    }
    
    /**
     * Save quick time movie.
     */
    public void saveAVIMovie() {
        File outputFile = null;
        File[] inputFile = new File[getCounter()];
        String directory = m_kVolumeImageA.GetImage().getFileInfo(0).getFileDirectory();
        //System.err.println( directory );
        for (int i = 0; i < getCounter(); i++) {
            inputFile[i] = new File(directory + "captureImage" + i + "." + "jpg");
            //System.err.println( directory + "captureImage" + i + "." + "jpg"  );
        }

        // Save to AVI file.
        String file = m_kVolumeImageA.GetImage().getImageName() + (m_iMovieCounter++) + ".avi";
        //System.err.println( file );
        outputFile = new File(file);

        //System.err.println( m_iCaptureFPS );
        try {
            MovieMaker movieMake = new MovieMaker(GetWidth(), GetHeight(), m_iCaptureFPS, outputFile, inputFile);
            movieMake.makeMovie();
        } catch (Throwable t) {
            t.printStackTrace();
        }

        for (int i = 0; i < getCounter(); i++) {
            inputFile[i].delete();
        }
        m_iScreenCaptureCounter = 0;
        inputFile = null;
        outputFile = null;
    }

    /**
     * Save AVI movie.
     */
    public void saveQuickTimeMovie() {
        MediaLocator oml;
        Vector<String> inputFiles = new Vector<String>();
        File[] inputFile = new File[getCounter()];
        String file;

        String directory = m_kVolumeImageA.GetImage().getFileInfo(0).getFileDirectory() + File.separatorChar;
        file = "file:" + directory + m_kVolumeImageA.GetImage().getImageName() + ".mov";

        oml = new MediaLocator(file);

        for (int i = 0; i < getCounter(); i++) {
            inputFiles.addElement(directory + "captureImage" + i + "." + "jpg");
            inputFile[i] = new File(directory + "captureImage" + i + "." + "jpg");
        }

        JpegImagesToMovie imageToMovie = new JpegImagesToMovie();
        imageToMovie.doIt(GetWidth(), GetHeight(), 30, inputFiles, oml);

        for (int i = 0; i < getCounter(); i++) {
            inputFile[i].delete();
        }
        m_iScreenCaptureCounter = 0;
        inputFiles = null;
        oml = null;

    }
    
	public void setCameraNearPlane( float distance )
	{
		if ( distance < 0 || distance > 10 )
		{
			return;
		}
		nearPlane = distance;
		m_spkCamera.SetFrustum(30.0f,m_iWidth/(float)m_iHeight,nearPlane,10.0f);
		m_pkRenderer.OnFrustumChange();
        updateBoundingCube = true;
	}
	
	/**
	 * Get the near plane
	 */
	public float getNearPlane() {
		return nearPlane;
	}
	
    /** 
     * Changes the projection to Orthographic Projection.
     */
    public void setOrthographicProjection()
    {
        m_spkCamera.Perspective = false;
        m_spkCamera.SetFrustum(60.0f,m_iWidth/(float)m_iHeight,1f,10.0f);
        m_pkRenderer.OnFrustumChange();
        updateBoundingCube = true;
    }
  
    
    /** 
     * Changes the projection to Perspective Projection.
     */
    public void setPerspectiveProjection()
    {
        m_spkCamera.Perspective = true;
        m_spkCamera.SetFrustum(30.0f,m_iWidth/(float)m_iHeight,getNearPlane(),10.0f);
        m_pkRenderer.OnFrustumChange();
        updateBoundingCube = true;
    }
    
    /**
     * Sets the currently visible flag. Used when the GLCanvas is removed from
     * the display panel or frame.
     * @param bVisible set to false when the GLCanvas container is not displayed.
     */
    public void setVisible( boolean bVisible )
    {
        m_bVisible = bVisible;
    }
	 
	 public void startAnimator(boolean bRunFast)
	 {
		 if ( m_kAnimator != null )
		 {
			 m_kAnimator.setRunAsFastAsPossible(bRunFast);
			 m_kAnimator.start();
		 }
	 }
    
    /**
     * Build a image with the current rotational transformation matrix. Step.1
     * convert the java3D transform matrix into a quaternion component. Step.2
     * convert the quaternion into image( our world ) coordinate
     * system. Step.3 convert the quaternion into a rotatin matrix. Quaternion
     * q ( w, x, y, z ): rotation of w around the vector ( x, y, z ); Convert
     * the quaternion into a rotation matrix. / \ | 1-2*y^2-2*z^2 2*x*y-2*w*z
     * 2*x*z+2*w*y | | 2*xy+2*w*z 1-2*x^2-2*z^2 2*y*z-2*w*x | | 2*x*z-2*w*y
     * 2*y*z+2*w*x 1-2*x^2-2*y^2 | \ / Step.4 Calling the transform algorithm
     * to extract the image.
     */
    public void updateImageFromRotation() {
        int interp = 0;
        double w, x, y, z;
        // Step.1
        Quaternion quat = new Quaternion();
        Matrix3f kRotate = m_spkScene.Local.GetRotate();
        quat.FromRotationMatrix(kRotate);
        
        // Step.2
        w = quat.W();
        x = quat.X();
        y = quat.Y();
        z = quat.Z();

        // Step.3
        TransMatrix transMtx = new TransMatrix(4);

        transMtx.set(0, 0, 1 - (2 * (y * y)) - (2 * (z * z)));
        transMtx.set(0, 1, 2 * ((x * y) - (w * z)));
        transMtx.set(0, 2, 2 * ((x * z) + (w * y)));
        transMtx.set(0, 3, 0);
        transMtx.set(1, 0, 2 * ((x * y) + (w * z)));
        transMtx.set(1, 1, 1 - (2 * (x * x)) - (2 * (z * z)));
        transMtx.set(1, 2, 2 * ((y * z) - (w * x)));
        transMtx.set(1, 3, 0);
        transMtx.set(2, 0, 2 * ((x * z) - (w * y)));
        transMtx.set(2, 1, 2 * ((y * z) + (w * x)));
        transMtx.set(2, 2, 1 - (2 * (x * x)) - (2 * (y * y)));
        transMtx.set(2, 3, 1);
        transMtx.set(3, 0, 0);
        transMtx.set(3, 1, 0);
        transMtx.set(3, 2, 0);
        transMtx.set(3, 3, 1);

        TransMatrix xfrm = new TransMatrix(4);

        WildMagic.LibFoundation.Mathematics.Vector3f center = m_kVolumeImageA.GetImage().getImageCentermm(false);

        xfrm.setTranslate(center.X, center.Y, center.Z);
        xfrm.mult(transMtx);
        xfrm.setTranslate(-center.X, -center.Y, -center.Z);

        // Step.4
        float[] resols = m_kVolumeImageA.GetImage().getFileInfo()[0].getResolutions();
        int[] dim = m_kVolumeImageA.GetImage().getFileInfo()[0].getExtents();
        int xDim = dim[0];
        int yDim = dim[1];
        int zDim = dim[2];
        AlgorithmTransform algoTrans = new AlgorithmTransform(m_kVolumeImageA.GetImage(), xfrm, interp, resols[0], resols[1], resols[2],
                                                              xDim, yDim, zDim, false, false, false);

        algoTrans.setUpdateOriginFlag(false);
        algoTrans.run();

        ModelImage resultImage1 = algoTrans.getTransformedImage();

        if ((algoTrans.isCompleted() == true) && (resultImage1 != null)) {
            resultImage1.calcMinMax();
           
            // The algorithm has completed and produced a new image to be displayed.
            try {
                new ViewJFrameImage(resultImage1, null, new Dimension(610, 200));
            } catch (OutOfMemoryError error) {
                MipavUtil.displayError("Out of memory: unable to open new frame");
            }
           
        }
    }
  

    public void setDefaultLighting()
    {
        if ( m_akLights == null )
        {
        	m_akLights = new Light[2];
        	m_akLights[0] = new Light(Light.LightType.LT_DIRECTIONAL);
        	m_akLights[0].Intensity = 0.5f;
        	m_akLights[0].Ambient.Set(1f, 1f, 1f);
        	m_akLights[0].Diffuse.Set(1f, 1f, 1f);
        	m_akLights[0].Specular.Set(1f, 1f, 1f);
        	m_akLights[0].Position.set(0f,0f,3f);
        	m_akLights[0].DVector.set( 0f, 0f, 1f );
		    		
        	m_akLights[1] = new Light();
        	m_akLights[1].Intensity = 0.5f;
        	m_akLights[1].Ambient.Set(1f, 1f, 1f);
        	m_akLights[1].Diffuse.Set(1f, 1f, 1f);
        	m_akLights[1].Specular.Set(1f, 1f, 1f);
        }
        updateLighting( m_akLights );
    }
    
    /**
     * Called from JPanelLight. Updates the lighting parameters.
     * @param akGLights the set of GeneralLight objects.
     */
    public void updateLighting(Light[] akGLights )
    {
        if ( akGLights == null )
        {
            return;
        }
        m_akLights = akGLights;
        if ( m_bInit )
        {
            for ( int i = 0; i < akGLights.length; i++ )
            {
                String kLightType = new String("Light"+(i)+"Type");
                float[] afType = new float[]{0,0,0,0};
                if ( i < 8 )
                {
                    if ( akGLights[i].On )
                    {
                        Light kLight = akGLights[i];
                        m_pkRenderer.SetLight( i, kLight );
                        if ( akGLights[i].Type == Light.LightType.LT_AMBIENT )
                        {
                            afType[0] = 0;
                        }
                        else if ( akGLights[i].Type == Light.LightType.LT_DIRECTIONAL )
                        {
                            afType[0] = 1;
                        }
                        else if ( akGLights[i].Type == Light.LightType.LT_POINT )
                        {
                            afType[0] = 2;
                        }
                        else if ( akGLights[i].Type == Light.LightType.LT_SPOT )
                        {
                            afType[0] = 3;
                        }
                        for ( int j = 0; j < m_kDisplayList.size(); j++ )
                        {
                            m_kDisplayList.get(j).SetLight(kLightType, afType);
                        }
                    }
                    else
                    {
                        m_pkRenderer.SetLight( i, new Light() );
                        afType[0] = -1;
                        for ( int j = 0; j < m_kDisplayList.size(); j++ )
                        {
                            m_kDisplayList.get(j).SetLight(kLightType, afType);
                        }
                    }
                }
            }
        }
    }
    

    /**
     * Writes a the frame buffer as a .jpg image to disk. The file name is captureImage + the image number.
     * @return true on successful write.
     */
    public boolean writeImage()
    {
        BufferedImage kScreenShot = m_pkRenderer.Screenshot();
        try {
            String directory = m_kVolumeImageA.GetImage().getFileInfo(0).getFileDirectory();
            //System.err.println( directory );
            ImageIO.write(kScreenShot, "jpg", new File( directory + "captureImage" + m_iScreenCaptureCounter++ + "." + "jpg"));
        } catch (IOException e) {
            e.printStackTrace();
        }
        return true;
    }
    
    protected Matrix3f GetSceneRotation()
    {
        return m_spkScene.Local.GetRotate();
    }


    /**
     * Updates the camera and displayed objects for rendering.
     */
    protected void Move()
    {        
        if (MoveCamera())
        {
            m_kCuller.ComputeVisibleSet(m_spkScene);
        }        
        if (MoveObject())
        {
            UpdateSceneRotation();
        }
        if ( m_bTestFrameRate )
        {
            Matrix3f kRotate = m_spkScene.Local.GetRotate();
            kRotate.mult(m_kZRotate);
            m_spkScene.Local.SetRotate(kRotate);
            m_spkScene.UpdateGS();
            m_kCuller.ComputeVisibleSet(m_spkScene);
            
            for ( int i = 0; i < m_kDisplayList.size(); i++ )
            {
                m_kDisplayList.get(i).GetScene().Local.SetRotateCopy(m_spkScene.Local.GetRotate());
            }
        }
    }
    
    /**
     * Renders the frame rate to the screen.
     */
    protected void RenderFrameRate(GLAutoDrawable kDraw)
    {
        //Draw frame rate:
        m_pkRenderer.SetCamera(m_spkCamera);
        if ( m_bTestFrameRate )
        {
            DrawFrameRate(50,50,ColorRGBA.WHITE);
        }
    }
    
    protected void SetSceneRotation(Matrix3f kRotation)
    {
        m_spkScene.Local.SetRotate(kRotation);
        UpdateSceneRotation();
    }
    
    /**
     * Updates the displayed objects based on any user mouse rotation.
     */
    protected void UpdateSceneRotation()
    {
    	if ( m_spkScene == null )
    	{
    		return;
    	}
        m_spkScene.UpdateGS();
        m_kCuller.ComputeVisibleSet(m_spkScene);
        
        for ( int i = 0; i < m_kDisplayList.size(); i++ )
        {
            m_kDisplayList.get(i).GetScene().Local.SetRotateCopy(m_spkScene.Local.GetRotate());
            m_kDisplayList.get(i).GetScene().UpdateGS();
        }
    }
    
    /**
     * Called by the doPick() method in Navigation behavior.
     * @return  the display list
     */
    public Vector<VolumeObject> getDisplayList() {
        return m_kDisplayList;
    }
    
    /**
     * Called by the NavigationBehavior to get normalized bounding box, x dim
     * @return  normalized bounding box x component.
     */
    public float getNormalizedXDim() {
    	return m_fX;
    }
    
    /**
     * Called by the NavigationBehavior to get normalized bounding box, y dim
     * @return   normalized bounding box y component.
     */
    public float getNormalizedYDim() {
    	return m_fY;
    }
    
    /**
     * Called by the NavigationBehavior to get normalized bounding box, z dim
     * @return  normalized bounding box z component. 
     */
    public float getNormalizedZDim() {
    	return m_fZ;
    }
    
    /**
     * Get the world matrix. 
     * @return world matrix
     */
    public Matrix4f getSceneToWorldMatrix() {
    	return m_kSceneToWorld;
    }
    

	/**
	 * 3D MOUSE MOTION CONSTANTS
	 */
	double defaultVelocityTranslational = 0.01;
	//double radiusScalarTranslational = 0.005;
	//int radiusPowerTranslational = 2;
	double transScalar = 0.05;
	double transPower = 1;
	float ZXDistanceStep = 0f;
	float ZYDistanceStep = 0f;
	float ZZDistanceStep = 0f;
    double defaultVelocityRotational = 1;
    double rotScalar = 0.25;
    double rotPower = 2;
    Vector3f zVector = new Vector3f(0,0,1);
    Vector3f currentLocation = new Vector3f();
    double mouseRot = 0;
    
	public void processSpaceNavEvent()
    {
    	System.out.println("tx: "+SpaceNavigatorController.getTX()+"\tty: "+SpaceNavigatorController.getTY()+"\ttz: "+SpaceNavigatorController.getTZ()+
    			"\trx: "+SpaceNavigatorController.getRX()+"\try: "+SpaceNavigatorController.getTX()+"\trz: "+SpaceNavigatorController.getRZ());
    	Vector3f oldLoc, newLoc, kLoc;
    	isSpaceNavCodeRunning = true;
    	double scaleValue;
    	if(m_spkMotionObject == null)
    		return;

    	getSpaceNavValues();
    	setMouseSpeedModifier();
    	
    	rotateAboutXAxis();
    	rotateAboutYAxis();
    	rotateAboutZAxis();
    	
    	//process the up and down, these are mapped to the z-axis
    	scaleValue = normalizeSpaceNavValue( SpaceNavigatorController.getTZ() );
    	if(useMouseTranslationCutoffs)
    	{
    		if(tZ < -translationCutoff || tZ > translationCutoff)
    		{
    			double temp;
    			if(tZ < 0)
    	        {
    	        	temp = tZ + translationCutoff;
    	        	if(temp > 0)
    	        		temp = 0;
    	        }
    	        else
    	        {
    	        	temp = tZ - translationCutoff;
    	        	if(temp < 0)
    	        		temp = 0;
    	        }
    			scaleValue = temp/100;
    		}
    		else
    		{
    			scaleValue = 0;
    		}
    	}
    	oldLoc = new Vector3f();
    	newLoc = new Vector3f();
        kLoc = new Vector3f(m_akWorldAxis[1]);
       
        currentLocation.copy(m_spkCamera.GetLocation());
        Vector3f zValue = Vector3f.mult(zVector, currentLocation);
        ZYDistanceStep = zValue.length();
        double ZYScaleTrans = ZYDistanceStep * transScalar;
        double ZYTrans = Math.pow(ZYScaleTrans, transPower);
        
        double yMouseScaleTranslational = defaultVelocityTranslational + ZYTrans;
        double yMouseVelocityTranslational = yMouseScaleTranslational * scaleValue;
        
        kLoc.scale((float)(yMouseVelocityTranslational));    
        
        oldLoc.copy(m_spkCamera.GetLocation());
        kLoc = Vector3f.sub(m_spkCamera.GetLocation(), kLoc);
        m_spkCamera.SetLocation(kLoc);
        newLoc.copy(m_spkCamera.GetLocation());
        newLoc.sub(oldLoc);
        if(tZ < 0)
        {
        	yCameraMove -= newLoc.length();
        }
        else
        {
        	yCameraMove += newLoc.length();
        }
    	
        //process forwards and backwards (zoom), these are mapped to the Y-Axis
    	scaleValue = normalizeSpaceNavValue( SpaceNavigatorController.getTY() );
    	if(useMouseTranslationCutoffs)
    	{
    		if(tY < -translationCutoff || tY > translationCutoff)
    		{
    			double temp;
    			if(tY < 0)
    	        {
    	        	temp = tY + translationCutoff;
    	        	if(temp > 0)
    	        		temp = 0;
    	        }
    	        else
    	        {
    	        	temp = tY - translationCutoff;
    	        	if(temp < 0)
    	        		temp = 0;
    	        }
    			scaleValue = temp/100;
    		}
    		else
    		{
    			scaleValue = 0;
    		}
    	}

        oldLoc = new Vector3f();
    	newLoc = new Vector3f();
        kLoc = new Vector3f(m_akWorldAxis[0]); 
        
        currentLocation.copy(m_spkCamera.GetLocation());
        ZZDistanceStep = zValue.length();
        double ZZScaleTrans = ZZDistanceStep * transScalar;
        double ZZTrans = Math.pow(ZZScaleTrans, transPower);
        
        double zMouseScaleTranslational = defaultVelocityTranslational + ZZTrans;
        double zMouseVelocityTranslational = zMouseScaleTranslational * scaleValue;
        
        kLoc.scale((float)(-zMouseVelocityTranslational));    
        
        oldLoc.copy(m_spkCamera.GetLocation());
        kLoc.add(m_spkCamera.GetLocation());
        m_spkCamera.SetLocation(kLoc);
        newLoc.copy(m_spkCamera.GetLocation());
        newLoc.sub(oldLoc);
        if(tY < 0)
        {
        	zCameraMove -= newLoc.length();
        }
        else
        {
        	zCameraMove += newLoc.length();
        }
        
        System.out.println(m_spkCamera.GetLocation());
        
//        System.out.println("zCameraMove: " + zCameraMove + ", mouseSpeedScaler: " + mouseSpeedScaler);
        
        //process the right and left movements 
        scaleValue = normalizeSpaceNavValue( SpaceNavigatorController.getTX() );
        if(useMouseTranslationCutoffs)
    	{
        	if(tX < -translationCutoff || tX > translationCutoff)
        	{
        		double temp;
    			if(tX < 0)
    	        {
    	        	temp = tX + translationCutoff;
    	        	if(temp > 0)
    	        		temp = 0;
    	        }
    	        else
    	        {
    	        	temp = tX - translationCutoff;
    	        	if(temp < 0)
    	        		temp = 0;
    	        }
    			if(temp != 0)
    				scaleValue = temp/100;
    		}
        	else
    		{
    			scaleValue = 0;
    		}
    	}
        oldLoc = new Vector3f();
    	newLoc = new Vector3f();
        kLoc = new Vector3f(m_akWorldAxis[2]); 
        
        //RADIUS TERM (c*r^a) CAN BE ADDED TO mouseScaleTranslational IF DESIRED:        
        //Vector3f currentLocation = new Vector3f();
        //currentLocation.copy(m_spkCamera.GetLocation());
        //double radius = currentLocation.length();
        //double radiusScaleStep = Math.pow(radius, radiusPowerTranslational);
        //double radiusScaleFinal = radiusScaleStep * radiusScalarTranslational;
	    
        currentLocation.copy(m_spkCamera.GetLocation());
        ZXDistanceStep = zValue.length();
        double ZXScaleTrans = ZXDistanceStep * transScalar;
        double ZXTrans = Math.pow(ZXScaleTrans, transPower);
        
        //ALTERNATE W/ RADIUS SCALING:
        //xMouseScaleTranslational = (defaultVelocityTranslational + radiusScaleFinal * (1 + ZXTrans));
        
        double xMouseScaleTranslational = defaultVelocityTranslational + ZXTrans;
        double xMouseVelocityTranslational = xMouseScaleTranslational * scaleValue;
        
        kLoc.scale((float)(xMouseVelocityTranslational));    
      	    
        oldLoc.copy(m_spkCamera.GetLocation());
        kLoc.add(m_spkCamera.GetLocation());
        m_spkCamera.SetLocation(kLoc);
        newLoc.copy(m_spkCamera.GetLocation());
        newLoc.sub(oldLoc);
        
        if(tX < 0)
        {
        	xCameraMove -= newLoc.length();
        }
        else
        {
        	xCameraMove += newLoc.length();
        }
        
	    //process the scene
    	UpdateSceneRotation();
    	
//    	printSpaceNavData();
		isSpaceNavCodeRunning = false;
		
		System.out.println(m_spkCamera.GetLocation());
		
	//	System.out.println("tX: " + tX + ", scaleValue: " + scaleValue + ", tCutoff: " + translationCutoff + ", rCutoff: " + rotationCutoff);
	    
	}
       
    private void rotateAboutXAxis()
    {
    	double rXValue = normalizeSpaceNavValue(SpaceNavigatorController.getRX());
    	Vector3f kAxis = new Vector3f(Vector3f.UNIT_X);
	    double fAngle;
	    Matrix3f kRot, kIncr = new Matrix3f();
    	
    	if(useMouseRotationCutoffs)
    	{
    		if(rX < -rotationCutoff || rX > rotationCutoff)
    		{
    			double temp;
    			if(rX < 0)
    	        {
    	        	temp = rX + rotationCutoff;
    	        	if(temp > 0)
    	        		temp = 0;
    	        }
    	        else
    	        {
    	        	temp = rX - rotationCutoff;
    	        	if(temp < 0)
    	        		temp = 0;
    	        }
    			if(temp != 0)
    				rXValue = temp/100;
    		}
    		else
    		{
    			rXValue = 0;
    		}
    	}
	    
    	kRot = m_spkMotionObject.Local.GetRotate();
    	
        currentLocation.copy(m_spkCamera.GetLocation());
        Vector3f zValue = Vector3f.mult(zVector, currentLocation);
        
// POSSIBLE "FINE MOVEMENT" BUTTON
        
//        if (fineMovementButton = true)
//    	{
//    		double zDistance = 0;
//    	}
//    	else
//    	{
//    		double zDistance = zValue.length();
//    	}
    	
        double zDistance = zValue.length();
        double zScaleRotStep = zDistance * rotScalar;
        double zScaleRot = Math.pow(zScaleRotStep, rotPower);
        
    	fAngle = invertRX * m_fRotSpeed * rXValue * rotationScaleFactor; 
    	double mouseScaleRotational = defaultVelocityRotational + zScaleRot;
    	double mouseVelocityRotational = mouseScaleRotational * fAngle;
    	
    	
    	if(Math.abs(mouseVelocityRotational) > 0.07)
    	{
    		if(mouseVelocityRotational > 0.07)
    		{
    			mouseRot = 0.07;
    		}
    		else
    		{
    			mouseRot = -0.07;
    		}
    	}
    	else
    	{
    		mouseRot = mouseVelocityRotational;
    	}    	    	    	
       
    	kIncr.fromAxisAngle(kAxis, (float)mouseRot);
    	kRot.multLeft( kIncr ).orthonormalize();
        m_spkMotionObject.Local.SetRotate(kRot);
        kIncr = null;
        kAxis = null;
    }
    
    //currently mapped to the RZ axis
    private void rotateAboutYAxis()
    {
    	double rYValue = normalizeSpaceNavValue(SpaceNavigatorController.getRZ());
    	Vector3f kAxis = new Vector3f(Vector3f.UNIT_Y);
	    double fAngle;
	    Matrix3f kRot, kIncr = new Matrix3f();
	    
	    if(useMouseRotationCutoffs)
    	{
    		if(rZ < -rotationCutoff || rZ > rotationCutoff)
    		{
    			double temp;
    			if(rZ < 0)
    	        {
    	        	temp = rZ + rotationCutoff;
    	        	if(temp > 0)
    	        		temp = 0;
    	        }
    	        else
    	        {
    	        	temp = rZ - rotationCutoff;
    	        	if(temp < 0)
    	        		temp = 0;
    	        }
    			if(temp != 0)
    				rYValue = temp/100;
    		}
    		else
    		{
    			rYValue = 0;
    		}
    	}
	    
    	kRot = m_spkMotionObject.Local.GetRotate();
    	currentLocation.copy(m_spkCamera.GetLocation());
        Vector3f zValue = Vector3f.mult(zVector, currentLocation);
        double zDistance = zValue.length();
        double zScaleRotStep = zDistance * rotScalar;
        double zScaleRot = Math.pow(zScaleRotStep, rotPower);
        
    	fAngle = invertRZ * m_fRotSpeed * rYValue * rotationScaleFactor; 
    	double mouseScaleRotational = defaultVelocityRotational + zScaleRot;
    	double mouseVelocityRotational = mouseScaleRotational * fAngle;
    	
    	if(Math.abs(mouseVelocityRotational) > 0.07)
    	{
    		if(mouseVelocityRotational > 0.07)
    		{
    			mouseRot = 0.07;
    		}
    		else
    		{
    			mouseRot = -0.07;
    		}
    	}
    	else
    	{
    		mouseRot = mouseVelocityRotational;
    	}    	    	
       
    	kIncr.fromAxisAngle(kAxis, (float)mouseRot);
        kRot.multLeft( kIncr ).orthonormalize();
        m_spkMotionObject.Local.SetRotate(kRot);
        kIncr = null;
        kAxis = null;
    }
    
    //currently mapped to the Y axis
    //TODO change method names to make more sense
    private void rotateAboutZAxis()
    {
    	double rZValue = normalizeSpaceNavValue(SpaceNavigatorController.getRY());
    	Vector3f kAxis = new Vector3f(Vector3f.UNIT_Z);
	    double fAngle;
	    Matrix3f kRot, kIncr = new Matrix3f();
	
	    if(useMouseRotationCutoffs)
    	{
    		if(rY < -rotationCutoff || rY > rotationCutoff)
    		{
    			double temp;
    			if(rY < 0)
    	        {
    	        	temp = rY + rotationCutoff;
    	        	if(temp > 0)
    	        		temp = 0;
    	        }
    	        else
    	        {
    	        	temp = rY - rotationCutoff;
    	        	if(temp < 0)
    	        		temp = 0;
    	        }
    			if(temp != 0)
    				rZValue = temp/100;
    		}
    		else
    		{
    			rZValue = 0;
    		}
    	}
    	kRot = m_spkMotionObject.Local.GetRotate();
    	currentLocation.copy(m_spkCamera.GetLocation());
        Vector3f zValue = Vector3f.mult(zVector, currentLocation);
        double zDistance = zValue.length();
        double zScaleRotStep = zDistance * rotScalar;
        double zScaleRot = Math.pow(zScaleRotStep, rotPower);
        
    	fAngle = invertRY * m_fRotSpeed * rZValue * rotationScaleFactor; 
    	double mouseScaleRotational = defaultVelocityRotational + zScaleRot;
    	double mouseVelocityRotational = mouseScaleRotational * fAngle;
    	    	
    	if(Math.abs(mouseVelocityRotational) > 0.07)
    	{
    		if(mouseVelocityRotational > 0.07)
    		{
    			mouseRot = 0.07;
    		}
    		else
    		{
    			mouseRot = -0.07;
    		}
    	}
    	else
    	{
    		mouseRot = mouseVelocityRotational;
    	}    	    	
       
    	kIncr.fromAxisAngle(kAxis, (float)mouseRot);
        kRot.multLeft( kIncr ).orthonormalize();
        m_spkMotionObject.Local.SetRotate(kRot);
        kIncr = null;
        kAxis = null;
    }
    
    private void setMouseSpeedModifier()
    {
    	//completely arbitrary numbers used to find what values work well
    	if(zCameraMove < 2 && zCameraMove > -3)
    	{
    		mouseSpeedScaler = normalizeValue(-zCameraMove, -3, 6, 0, 6);
    		return;
    	}
    	else if(zCameraMove <= -3)
    	{
    		mouseSpeedScaler = 6;
    		return;
    	}
    	mouseSpeedScaler = 1;
    	return;
    }
    
	public void processSpaceNavEvent(SpaceNavigatorEvent e) {}
	
	public void mousePressed(MouseEvent e)
	{
		super.mousePressed(e);
		try {
	    	if(SpaceNavigatorController.hasSpaceNavigator() && !SpaceNavigatorPoller.hasInstanceOf(this)) {
	    		SpaceNavigatorPoller.registerListener(this);
	    	}
		} catch (Error er) {
			Preferences.debug("Unable to load space navigator libraries.  See console output for details.\n", Preferences.DEBUG_MINOR);
			er.printStackTrace();
		}
	}
	
	/**
	 * takes a value from the spaceNavigotorController and normalizes the value between -1 to 1
	 * got formula from http://www.mathworks.com/matlabcentral/newsreader/view_thread/256540
	 * @param value
	 * @return
	 */
	private float normalizeSpaceNavValue(float value){
		float result = 2*(value - spaceNavMin) / (spaceNavMax - spaceNavMin) - 1;
		return result;
	}
	
	/**
	 * got this formula from the video at http://www.youtube.com/watch?v=7DQMAXaiXmk
	 * @param value
	 * @param minValue
	 * @param maxValue
	 * @param minRange
	 * @param maxRange
	 * @return
	 */
	private double normalizeValue(double value, double minValue, double maxValue, double minRange, double maxRange){
		double result;
		result = minRange + ( ( (value - minValue) * (maxRange - minRange) ) / (maxValue - minValue) );
		return result;
	}
	
	private void printSpaceNavData(){
		DecimalFormat dec = new DecimalFormat("0.00000");
		
		StringBuilder builder = new StringBuilder();
		builder.append("RX: ").append(dec.format(SpaceNavigatorController.getRX()));
		builder.append("\tRY: ").append(dec.format(SpaceNavigatorController.getRY()));
		builder.append("\tRZ: ").append(dec.format(SpaceNavigatorController.getRZ())).append("\t\t");
		builder.append("TX: ").append(dec.format(SpaceNavigatorController.getTX()));
		builder.append("\tTY: ").append(dec.format(SpaceNavigatorController.getTY()));
		builder.append("\tTZ: ").append(dec.format(SpaceNavigatorController.getTZ()));
		
		System.out.println(builder.toString());
	}

	
	protected static void checkIfSpaceNavNeedsCalibration(){		
		if(SpaceNavigatorController.hasSpaceNavigator() && SpaceNavigatorController.checkIfSpaceNavNeedsCalibration()){
			System.out.println("the 3D mouse may need to be calibrated to work properly.");
		}
	}
	
	public static void invertRX(){
		invertRX = invertRX * -1;
	}

	public static void invertRY(){
		invertRY = invertRY * -1;
	}

	public static void invertRZ(){
		invertRZ = invertRZ * -1;
	}
	
	public static void invertTX(){
		invertTX = invertTX * -1;
	}

	public static void invertTY(){
		invertTY = invertTY * -1;
	}

	public static void invertTZ() {
		invertTZ = invertTZ * -1; 
	}
	

	/** Helper method
	 * sets the values tX, tY, tZ, rX, rY, and rZ to the current space navigator values normalized on a -100 to 100 scale.
	 * This is done to make coding cross platform values easier by making them uniform across platforms. 
	 */
	private void getSpaceNavValues(){
		tX = normalizeValue(SpaceNavigatorController.getTX(), spaceNavMin, spaceNavMax, -100, 100);
		tY = normalizeValue(SpaceNavigatorController.getTY(), spaceNavMin, spaceNavMax, -100, 100);
		tZ = normalizeValue(SpaceNavigatorController.getTZ(), spaceNavMin, spaceNavMax, -100, 100);
		rX = normalizeValue(SpaceNavigatorController.getRX(), spaceNavMin, spaceNavMax, -100, 100);
		rY = normalizeValue(SpaceNavigatorController.getRY(), spaceNavMin, spaceNavMax, -100, 100);
		rZ = normalizeValue(SpaceNavigatorController.getRZ(), spaceNavMin, spaceNavMax, -100, 100);
	}
	
	public static void setTranslationCutoffValue(int value){
		translationCutoff = value;
	}
	public static int getTranslationCutoffValue(){
		return (int)translationCutoff;
	}
	public static void setRotationCutoffValue(int value){
		rotationCutoff = value;
	}
	public static int getRotationCutoffValue(){
		return (int)rotationCutoff;
	}

	public static int getTranslationScaleFactor() {
		return translationScaleFactor;
	}
	/**
	 * the translationScaleFactor is used to change how fast the camera moves around the object
	 * ranging from 1, the fastest, to 10, the slowest
	*/
	public static void setTranslationScaleFactor(int i) {
		if(i >= 1 && i <= 10)
			translationScaleFactor = i;
	}

	public boolean isUseMouseTranslationCutoffs() {
		return useMouseTranslationCutoffs;
	}

	public void setUseMouseTranslationCutoffs(boolean useMouseTranslationCutoffs) {
		this.useMouseTranslationCutoffs = useMouseTranslationCutoffs;
	}

	public boolean isUseMouseRotationCutoffs() {
		return useMouseRotationCutoffs;
	}

	public void setUseMouseRotationCutoffs(boolean useMouseRotationCutoffs) {
		this.useMouseRotationCutoffs = useMouseRotationCutoffs;
	}

	public static float getRotationScaleFactor() {
		return rotationScaleFactor;
	}
	
	/**
	 * the rotationScaleFactor is used to change how fast the camera moves around the object
	 * ranging from 1%, the slowest, to 100%, the fastest
	 * 
	 * @param rotationScaleFactor - value must be between 0.0f and 1.0f representing the percentage to scale the raw input speed
	 */
	public static void setRotationScaleFactor(float rotationScaleFactor) {//TODO finish making the type/values consistent for this and gui
		if(0f <= rotationScaleFactor && rotationScaleFactor <= 1.0f)
			GPURenderBase.rotationScaleFactor = rotationScaleFactor;
	}
}
