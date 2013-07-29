package gov.nih.mipav.view.renderer.WildMagic;

import static java.lang.System.nanoTime;
import static java.lang.System.out;
import gov.nih.mipav.model.algorithms.filters.OpenCL.filters.OpenCLAlgorithmVolumeCrop;
import gov.nih.mipav.model.algorithms.filters.OpenCL.filters.OpenCLAlgorithmVolumeNormals;
import gov.nih.mipav.model.file.FileTypeTable;
import gov.nih.mipav.model.file.FileWriteOptions;
import gov.nih.mipav.model.structures.ModelImage;
import gov.nih.mipav.model.structures.ModelLUT;
import gov.nih.mipav.model.structures.ModelRGB;
import gov.nih.mipav.model.structures.ModelStorageBase;
import gov.nih.mipav.model.structures.VOI;
import gov.nih.mipav.model.structures.VOIBase;
import gov.nih.mipav.model.structures.VOIContour;
import gov.nih.mipav.model.structures.VOIVector;
import gov.nih.mipav.util.MipavInitGPU;
import gov.nih.mipav.view.CustomUIBuilder;
import gov.nih.mipav.view.Preferences;
import gov.nih.mipav.view.Preferences.OperatingSystem;
import gov.nih.mipav.view.ViewJFrameImage;
import gov.nih.mipav.view.dialogs.JDialogBase;

import gov.nih.mipav.view.renderer.WildMagic.Interface.SurfaceState;
import gov.nih.mipav.view.renderer.WildMagic.Render.OrderIndpTransparencyEffect;
import gov.nih.mipav.view.renderer.WildMagic.Render.Sculptor_WM;
import gov.nih.mipav.view.renderer.WildMagic.Render.VolumeBoundingBox;
import gov.nih.mipav.view.renderer.WildMagic.Render.VolumeClip;
import gov.nih.mipav.view.renderer.WildMagic.Render.VolumeClipEffect;
import gov.nih.mipav.view.renderer.WildMagic.Render.VolumeDTI;
import gov.nih.mipav.view.renderer.WildMagic.Render.VolumeImage;
import gov.nih.mipav.view.renderer.WildMagic.Render.VolumeNode;
import gov.nih.mipav.view.renderer.WildMagic.Render.VolumeObject;
import gov.nih.mipav.view.renderer.WildMagic.Render.VolumeOrientationCube;
import gov.nih.mipav.view.renderer.WildMagic.Render.VolumeRayCast;
import gov.nih.mipav.view.renderer.WildMagic.Render.VolumeSlices;
import gov.nih.mipav.view.renderer.WildMagic.Render.VolumeSurface;
import gov.nih.mipav.view.renderer.WildMagic.Render.VolumeVOI;
import gov.nih.mipav.view.renderer.WildMagic.Render.MultiDimensionalTransfer.ClassificationWidget;

import java.awt.BorderLayout;
import java.awt.event.KeyEvent;
import java.awt.event.KeyListener;
import java.awt.event.MouseEvent;
import java.awt.event.MouseListener;
import java.awt.event.MouseMotionListener;
import java.util.Vector;

import javax.media.opengl.GLAutoDrawable;
import javax.media.opengl.GLEventListener;
import javax.media.opengl.awt.GLCanvas;
import javax.swing.JLabel;
import javax.swing.JPanel;
import javax.swing.JSlider;
import javax.swing.SwingConstants;
import javax.swing.event.ChangeEvent;
import javax.swing.event.ChangeListener;

import org.jocl.CL;

import WildMagic.LibFoundation.Mathematics.ColorRGB;
import WildMagic.LibFoundation.Mathematics.ColorRGBA;
import WildMagic.LibFoundation.Mathematics.Matrix3f;
import WildMagic.LibFoundation.Mathematics.Matrix4f;
import WildMagic.LibFoundation.Mathematics.Vector3f;
import WildMagic.LibFoundation.Mathematics.Vector4f;
import WildMagic.LibGraphics.Collision.PickRecord;
import WildMagic.LibGraphics.Effects.ShaderEffect;
import WildMagic.LibGraphics.Rendering.Camera;
import WildMagic.LibGraphics.Rendering.CullState;
import WildMagic.LibGraphics.Rendering.GraphicsImage;
import WildMagic.LibGraphics.Rendering.FrameBuffer;
import WildMagic.LibGraphics.Rendering.Light;
import WildMagic.LibGraphics.Rendering.MaterialState;
import WildMagic.LibGraphics.Rendering.Texture;
import WildMagic.LibGraphics.Rendering.WireframeState;
import WildMagic.LibGraphics.Rendering.ZBufferState;
import WildMagic.LibGraphics.SceneGraph.Attributes;
import WildMagic.LibGraphics.SceneGraph.Geometry;
import WildMagic.LibGraphics.SceneGraph.IndexBuffer;
import WildMagic.LibGraphics.SceneGraph.Node;
import WildMagic.LibGraphics.SceneGraph.Polyline;
import WildMagic.LibGraphics.SceneGraph.TriMesh;
import WildMagic.LibGraphics.SceneGraph.VertexBuffer;
import WildMagic.LibRenderers.OpenGLRenderer.OpenGLFrameBuffer;
import WildMagic.LibRenderers.OpenGLRenderer.OpenGLRenderer;

import com.jogamp.opengl.util.Animator;

//-javaagent:C:\GeometricToolsInc\mipav\src\lib\profile.jar
// -Dprofile.properties=C:\GeometricToolsInc\mipav\src\lib\profile.properties
//import com.mentorgen.tools.profile.runtime.Profile;


public class VolumeTriPlanarRenderBase extends GPURenderBase
implements GLEventListener, KeyListener, MouseMotionListener,  MouseListener, ChangeListener
{

	private static final long serialVersionUID = -8415814666722429687L;
    
	/** New sculpting object for WM-based sculpting. */
	protected Sculptor_WM m_kSculptor = null;

	/** Arbitrary clip plane equation: */
	protected float[] m_afArbEquation = new float[4];
	protected Vector4f m_kArbitraryClip;
	/** Enable Arbitrary clip plane: */
	protected boolean m_bArbClipOn = false;

	/** Enables/Disables rendering the second pass. When disabled, the
	 * back-facing polygons of the proxy-geometry are shown instead of the volume: */
	protected boolean m_bDisplaySecond = true;

	/** 3D Slice renderer. */
	protected VolumeSlices m_kSlices = null;
	protected ColorRGB[] m_akSlicesColor = new ColorRGB[] { new ColorRGB(1, 0, 0), new ColorRGB(0, 1, 0), new ColorRGB(1, 1, 0) };
	
	/** GPU-based ray cast renderer. */
	protected VolumeRayCast m_kVolumeRayCast = null;
	/** Clipping planes renderer */
	protected VolumeClip m_kVolumeClip = null;
	/** DTI renderer. */
	protected VolumeDTI m_kDTIDisplay = null;
	/** Volume bounding box renderer. */
	protected VolumeBoundingBox m_kVolumeBox = null;
	/** Orientation cube renderer */
	protected VolumeOrientationCube m_kVolumeCube = null;

	/** The first time the frame is rendererd use the shader to calculate the normals for the volume data. */
	protected boolean m_bFirstRender = true;

	/** Painting parameters: */
	protected boolean m_bPaintEnabled = false;
	protected boolean m_bPaint = false;
	protected boolean m_bErase = false;
	protected boolean m_bDropper = false;
	protected boolean m_bPaintCan = false;
	protected ColorRGBA m_kPaintColor = null;
	protected int m_iBrushSize = 1;

	/** Geodesic enabled on/off. */
	protected boolean m_bGeodesicEnabled = false;

	/** Stereo on/off. */
	protected int m_iStereo = 0;  

	/** BrainSurfaceFlattener pick correspondence enabled on/off. */
	protected boolean m_bPickCorrespondence = false;

	/** Set to true when cropping the volume in the shader. */
	protected boolean m_bCrop = false;
	/** Intensity level for GPU-surface extraction. */
	protected int m_iExtractLevel = 1;

	/** sample rate when mouse is released. */
	protected float fSample_mouseReleased;
	/** sample rate when mouse is dragged. */
	protected float fSample_mouseDragged;

	protected boolean m_bPlay4D = false;
	protected float m_fAnimateRate = 0;
	protected int m_iAnimateCount = 0;



	protected OpenGLFrameBuffer m_kFBO;    
	protected ShaderEffect m_spkPlaneEffect;
	protected TriMesh m_pkPlane;
	protected Camera m_pkScreenCamera;

	protected VolumeTriPlanarRenderBase m_kShared = null;

	protected boolean m_bUpdateCenterOnDisplay = false;    

	protected Vector3f m_kNewCenter = null;

	protected boolean m_bDisplay = false;


	protected Texture[] m_akSceneTarget = new Texture[4];

	protected boolean m_bWriteImage = false;
	
	protected static int ms_NumHistogramLUTs = 11;
	protected static Texture[] ms_akLUTTexture = new Texture[ms_NumHistogramLUTs];
	
	/** main container for this object, contains the scroll pane and the depth slider: */
	private JPanelGPU container;
	
	/**
	 * Default Constructor.
	 */
	 public VolumeTriPlanarRenderBase()
	{
		 super();
	}

	 /**
	  * Construct a stand-alone renderer
	  * @param kVolumeImageA volume data and textures for ModelImage A.
	  */
	 public VolumeTriPlanarRenderBase( VolumeImage kVolumeImageA  )
	 {
		 super();
         if ( Preferences.OperatingSystem.getOS() != OperatingSystem.OS_MAC )
         {
        	 m_eBuffering = FrameBuffer.BufferingType.BT_QUAD_STEREO;
         }
		 m_pkRenderer = new OpenGLRenderer( m_eFormat, m_eDepth, m_eStencil,
				 m_eBuffering, m_eMultisampling,
				 m_iWidth, m_iHeight );
		 ((OpenGLRenderer)m_pkRenderer).GetCanvas().addGLEventListener( this );       
		 ((OpenGLRenderer)m_pkRenderer).GetCanvas().addKeyListener( this );       
		 ((OpenGLRenderer)m_pkRenderer).GetCanvas().addMouseListener( this );       
		 ((OpenGLRenderer)m_pkRenderer).GetCanvas().addMouseMotionListener( this );       
		 ((OpenGLRenderer)m_pkRenderer).GetCanvas().addMouseWheelListener( this );
		 m_pkRenderer.SetExternalDir(MipavInitGPU.getExternalDirs());       
		 
		 m_bStandAlone = true;
		 m_kAnimator = new Animator();
		 if ( m_kAnimator != null )
		 {
			 m_kAnimator.add( GetCanvas() );
		 }
		 m_kVolumeImageA = kVolumeImageA;
		 m_kVolumeImageB = new VolumeImage();
		 m_kZRotate.fromAxisAngle(Vector3f.UNIT_Z, (float)Math.PI/18.0f);
		 m_kYRotate.fromAxisAngle(Vector3f.UNIT_Y, (float)Math.PI/18.0f);
	     m_kXRotate.fromAxisAngle(Vector3f.UNIT_X, (float)Math.PI/18.0f);
		 createContainer( GetCanvas() );
		 m_kAnimator.setRunAsFastAsPossible(true);
		 m_kAnimator.start();
	 }

	 /**
	  * Construct the Volume/Surface/Tri-Planar renderer.
	  * @param kParent parent user-interface and frame.
	  * @param kAnimator animator used to display the canvas.
	  * @param kVolumeImageA volume data and textures for ModelImage A.
	  * @param kVolumeImageB volume data and textures for ModelImage B.
	  */
	 public VolumeTriPlanarRenderBase( Animator kAnimator, 
			 VolumeImage kVolumeImageA, VolumeImage kVolumeImageB  )
	 {
		 super();
         if ( Preferences.OperatingSystem.getOS() != OperatingSystem.OS_MAC )
         {
        	 m_eBuffering = FrameBuffer.BufferingType.BT_QUAD_STEREO;
         }
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
		 if ( m_kAnimator != null )
		 {
			 m_kAnimator.add( GetCanvas() );
		 }
		 m_kVolumeImageA = kVolumeImageA;
		 m_kVolumeImageB = kVolumeImageB;
		 m_kZRotate.fromAxisAngle(Vector3f.UNIT_Z, (float)Math.PI/18.0f);
		 m_kYRotate.fromAxisAngle(Vector3f.UNIT_Y, (float)Math.PI/18.0f);
	     m_kXRotate.fromAxisAngle(Vector3f.UNIT_X, (float)Math.PI/18.0f);
	 }

	 /**
	  * Construct the Volume/Surface/Tri-Planar renderer.
	  * @param kParent parent user-interface and frame.
	  * @param kAnimator animator used to display the canvas.
	  * @param kVolumeImageA volume data and textures for ModelImage A.
	  * @param kVolumeImageB volume data and textures for ModelImage B.
	  */
	 public VolumeTriPlanarRenderBase( VolumeTriPlanarRenderBase kShared, GLCanvas kCanvas, 
			 VolumeImage kVolumeImageA, VolumeImage kVolumeImageB  )
	 {
		 super();
		 //m_eBuffering = FrameBuffer.BufferingType.BT_QUAD_STEREO;
		 m_pkRenderer = new OpenGLRenderer( m_eFormat, m_eDepth, m_eStencil,
				 m_eBuffering, m_eMultisampling,
				 m_iWidth, m_iHeight, kCanvas );
		 ((OpenGLRenderer)m_pkRenderer).GetCanvas().addGLEventListener( this );       
		 ((OpenGLRenderer)m_pkRenderer).GetCanvas().addKeyListener( this );       
		 ((OpenGLRenderer)m_pkRenderer).GetCanvas().addMouseListener( this );       
		 ((OpenGLRenderer)m_pkRenderer).GetCanvas().addMouseMotionListener( this );       
		 ((OpenGLRenderer)m_pkRenderer).GetCanvas().addMouseWheelListener( this );       
		 m_pkRenderer.SetExternalDir(MipavInitGPU.getExternalDirs());

		 m_kShared = kShared;
		 m_bShared = true;

		 m_kAnimator = new Animator(GetCanvas());
			 
		 m_kVolumeImageA = kVolumeImageA;
		 m_kVolumeImageB = kVolumeImageB;
		 m_kZRotate.fromAxisAngle(Vector3f.UNIT_Z, (float)Math.PI/18.0f);
		 m_kYRotate.fromAxisAngle(Vector3f.UNIT_Y, (float)Math.PI/18.0f);
	     m_kXRotate.fromAxisAngle(Vector3f.UNIT_X, (float)Math.PI/18.0f);
		 enableSculpt(false);
	 }




	 /**
	  * Add a new geodesic component to the surface.
	  * @param kSurface the surface the geodesic component is added to.
	  * @param kNew the new geodesic component.
	  * @param iGroup the display group to add to.
	  */
	 public void addGeodesic( TriMesh kSurface, Geometry kNew, int iGroup )
	 {
		 for ( int i = 0; i < m_kDisplayList.size(); i++ )
		 {
			 if ( m_kDisplayList.get(i).GetMesh() != null )
			 {
				 if ( m_kDisplayList.get(i).GetMesh() == kSurface)
				 {
					 ((VolumeSurface)(m_kDisplayList.get(i))).AddGeodesic(kNew, iGroup);
				 }
			 }
		 }
	 }

	 /**
	  * Add surfaces to the display list.
	  * @param akSurfaces
	  */
	 public void addSurface( SurfaceState kSurface )
	 {
		 VolumeSurface kVolumeSurfaces = new VolumeSurface( m_kVolumeImageA, m_kVolumeImageB,
				 m_kTranslate,
				 m_fX, m_fY, m_fZ,
				 kSurface );
		 m_kDisplayList.add( kVolumeSurfaces );
		 m_bSurfaceUpdate = true;
		 m_bSurfaceMaskUpdate = true;
	 }

	 /**
	  * Add surfaces to the display list.
	  * @param akSurfaces
	  */
	 public void addSurface( SurfaceState kSurface, boolean isFileCoords )
	 {
		 VolumeSurface kVolumeSurfaces = new VolumeSurface( m_kVolumeImageA, m_kVolumeImageB,
				 m_kTranslate,
				 m_fX, m_fY, m_fZ,
				 kSurface, true );
		 m_kDisplayList.add( kVolumeSurfaces );
		 m_bSurfaceUpdate = true;
		 m_bSurfaceMaskUpdate = true;
	 }


	 /**
	  * Add tract into the DTI display
	  * @param kLine   polyline
	  * @param iGroup  counter number
	  * @param centerIndex  center index color
	  */
	 public void addTract( VOIContour kContour, Polyline kLine, int iGroup )
	 {
		 if ( m_kDTIDisplay == null )
		 {
			 m_kDTIDisplay = new VolumeDTI( m_kVolumeImageA, m_kTranslate, m_fX, m_fY, m_fZ );
			 m_kDisplayList.add(1, m_kDTIDisplay);
		 }
		 m_kDTIDisplay.addPolyline( kContour, kLine, iGroup );
		 m_kDTIDisplay.SetDisplay( true );
	 }


	 public void addVolumeVOI( VolumeVOI kVOI )
	 {
		 m_kDisplayList.add( m_kDisplayList.size() - 1, kVOI );
	 }

	 /**
	  * Apply the sculpt region to the volume.
	  */
	 public boolean applySculpt(boolean bAll)
	 {
		 if ( m_kSculptor == null )
		 {
			 return false;
		 }

		 float[] afData = new float[16];

		 m_pkRenderer.SetConstantVPMatrix (0, afData);
		 Matrix4f kVP = new Matrix4f( afData, true );
		 Matrix4f kWorld = Matrix4f.mult(m_kSceneToWorld, kVP);
		 m_kSculptor.setWVPMatrix(kWorld);   
		 boolean bSculptTrue = false;
		 int iTSize = (m_kVolumeImageA.GetImage().is4DImage()&&bAll) ? m_kVolumeImageA.GetImage().getExtents()[3] : 1;
		 for ( int i = 0; i < iTSize; i++ )
		 {
			 if ( m_kVolumeImageA.GetVolumeTarget().GetImage().GetData() != null )
			 {
				 m_kSculptor.setTextureImageDataA( m_kVolumeImageA.GetVolumeTarget().GetImage().GetData() );
			 }

			 if ( m_kVolumeImageB.GetImage() != null )
			 {
				 if ( m_kVolumeImageB.GetVolumeTarget().GetImage().GetData() != null )
				 {
					 m_kSculptor.setTextureImageDataB( m_kVolumeImageB.GetVolumeTarget().GetImage().GetData() );
				 }
			 }
			 if ( m_kSculptor.applySculpt(m_kVolumeImageA.GetTimeSlice()) )
			 {
				 bSculptTrue = true;
			 }
			 if ( iTSize > 1 )
			 {
				 m_kVolumeImageA.update4D(true);
			 }
		 }
		 if ( bSculptTrue )
		 {
			 m_kVolumeImageA.ReleaseVolume();
			 if ( m_kVolumeImageB.GetImage() != null )
			 {
				 m_kVolumeImageB.ReleaseVolume();
			 }
			 m_kSculptor.clearSculpt();
			 return true;
		 }
		 return false;
	 }

	 /**
	  * Sets blending between imageA and imageB.
	  * @param fValue the blend value (0-1)
	  */
	 public void blend( String kSurfaceName, float fValue )
	 {
		 for ( int i = 0; i < m_kDisplayList.size(); i++ )
		 {
			 if ( m_kDisplayList.get(i).GetName() != null )
			 {
				 if ( m_kDisplayList.get(i).GetName().equals(kSurfaceName))
				 {
					 if ( ((VolumeSurface)(m_kDisplayList.get(i))).GetOpacity() == 1 )
					 {
						 m_bSurfaceUpdate = true;
					 }
					 m_kDisplayList.get(i).Blend( fValue );
					 // save the opacity property in surface attributes
					 ((VolumeSurface)(m_kDisplayList.get(i))).SetOpacity(fValue);
				 }
			 }
		 }
	 }

	 /**
	  * Clear the sculpt region.
	  */
	 public void clearSculpt()
	 {
		 if ( m_kSculptor != null )
		 {
			 m_kSculptor.clearSculpt();
		 }
	 }

	 /**
	  * Display the volume in Composite mode.
	  */
	 public void CMPMode()
	 {
		 if ( m_kVolumeRayCast != null )
		 {
			 m_kVolumeRayCast.CMPMode();
		 }
	 }
	 
	 /**
	  * Crop the clipped volume.
	  */
	 public void cropClipVolume()
	 {
		 m_bCrop = true;
		 GetCanvas().display();
	 }

	 protected void Move()
	 {        
		 if (MoveCamera())
		 {
			 m_kCuller.ComputeVisibleSet(m_spkScene);
			 updateBoundingCube = true;
		 }        
		 if (MoveObject())
		 {
			 UpdateSceneRotation();
			 updateBoundingCube = true;
		 }
		 if ( updateBoundingCube && m_bDoClip )
		 {
			 m_kVolumeRayCast.CheckViewIntersection( m_pkRenderer, m_kCuller );
			 updateBoundingCube = false;
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
	    
	    
	 /* (non-Javadoc)
	  * @see javax.media.opengl.GLEventListener#display(javax.media.opengl.GLAutoDrawable)
	  */
	 @Override
	 public void display(GLAutoDrawable arg0) {
		 MeasureTime();
		 
		 if ( m_kVolumeImageA == null ) {
			 return;
		 }           
		 if ( m_pkRenderer != null )
		 {
			 ((OpenGLRenderer)m_pkRenderer).SetDrawable( arg0 );
		 }
		 if ( m_kVolumeImageA_Old != null )
		 {
			 disposeShared(m_kVolumeImageA_Old, m_kVolumeImageB_Old );
			 m_kVolumeImageA_Old = null;
			 m_kVolumeImageB_Old = null;
		 }

		 if ( !m_bInit )
		 {
			 init(arg0);
		 }
		 if ( !m_bVisible || !m_bDisplay )
		 {
			 return;
		 }
		 if ( m_bDispose )
		 {
			 System.err.println( "Calling dispose " + this );
			 dispose(arg0);
			 return;
		 }
		 if ( m_kDeleteList.size () > 0 )
		 {
			 for ( int i = m_kDeleteList.size() -1; i >= 0; i-- )
			 {
				 VolumeObject kObj = m_kDeleteList.remove(0);
				 kObj.dispose(m_pkRenderer);
			 }
		 }


		 if ( m_kFBO == null || m_bFirstRender )
		 {
			 m_bFirstRender = false;
			 CreateRenderTarget(arg0, m_iWidth, m_iHeight);
		 }

		 if ( m_bUpdateCenterOnDisplay )
		 {
			 updateCenterOnDisplay();
		 }

		 Move();
		 Pick();
		 
		 
		 if ( profile )
		 {
//			  Profile.clear();
//			  Profile.start();
		 }

		 updateVOIs( m_kVolumeImageA.GetImage().getVOIs() );            
		 Render(arg0);
		 UpdateFrameCount();

	     //System.err.println( "fps: " + m_dFrameRate);

		 if ( m_iUpdateNormals == 1 )
		 {
			 long time = nanoTime();
			 ModelImage kImage = m_kVolumeImageA.GetImage();
			 OpenCLAlgorithmVolumeNormals oclNormals = new OpenCLAlgorithmVolumeNormals( kImage, CL.CL_DEVICE_TYPE_GPU );
			 oclNormals.run();
			 time = nanoTime() - time;
			 out.println("OCL GPU computation took: "+(time/1000000)+"ms");
			 m_iUpdateNormals = -1; 
			 ModelImage result = oclNormals.getDestImage();
			 m_kVolumeImageA.CopyNormalFiles(0, result);
		 }
		 if ( m_bPlay4D )
		 {
			 if ( m_iAnimateCount++ > m_fAnimateRate )
			 {
				 m_iAnimateCount = 0;
				 update4D( true );
			 }
		 }
		 if ( m_bSurfaceUpdate )
		 {
			 m_bSurfaceUpdate = false;
			 for ( int i = 1; i < m_kDisplayList.size(); i++ )
			 {
				 if ( m_kDisplayList.elementAt(i) instanceof VolumeSurface )
				 {
					 ((VolumeSurface)m_kDisplayList.elementAt(i)).SetPerPixelLighting( m_pkRenderer, true );
				 }
			 }
			 UpdateSceneRotation();
			 updateLighting( m_akLights );
		 }
		 if ( profile )
		 {
//			  Profile.stop();
//			  Profile.setFileName( "volume_render_profile" );
//			  Profile.shutdown();
//			  System.err.println( "Profile DONE" );
			  profile = false;
		 }
	 }

	 /**
	  * Displays the arbitrary clip plane position.
	  * @param bDisplay on/off.
	  */
	 public void displayArbitraryClipPlane( boolean bDisplay )
	 {
		 if ( m_kVolumeClip != null )
		 {
			 m_kVolumeClip.DisplayArb(bDisplay);
		 }
	 }

	 /**
	  * Called from JPanelDisplay. Sets the bounding box display on/off.
	  * @param bDisplay on/off.
	  */
	 public void displayBoundingBox( boolean bDisplay )
	 {
		 if ( m_kVolumeBox != null )
		 {
			 m_kVolumeBox.SetDisplay(bDisplay);
		 }
	 }

	 /**
	  * Called from JPanelClip. Sets the axis-aligned clip plane display on/off.
	  * @param iWhich the clip plane to set.
	  * @param bDisplay on/off.
	  */
	 public void displayClipPlane( int iWhich, boolean bDisplay )
	 {
		 if ( m_kVolumeClip != null )
		 {
			 m_kVolumeClip.displayClipPlane(iWhich, bDisplay);
		 }
	 }

	 /**
	  * Called from JPanelClip. Sets the axis-aligned clip plane display on/off and color.
	  * @param iWhich the clip plane to set.
	  * @param bDisplay on/off.
	  * @param kColor the clipping plane color.
	  */
	 public void displayClipPlane( int iWhich, boolean bDisplay, ColorRGB kColor )
	 {
		 setClipPlaneColor( iWhich, kColor );
		 displayClipPlane( iWhich, bDisplay );
	 }

	 /**
	  * Toggle display of the scene-graphe node on/off.
	  * @param kNode the node to toggle.
	  * @param bDisplay on/off.
	  */
	 public void displayNode( Node kNode, boolean bDisplay )
	 {
		 for ( int i = 0; i < m_kDisplayList.size(); i++ )
		 {
			 if ( m_kDisplayList.get(i) instanceof VolumeNode )
			 {
				 if ( ((VolumeNode)m_kDisplayList.get(i)).GetNode() == kNode )
				 {
					 m_kDisplayList.get(i).SetDisplay(bDisplay);
				 }
			 }
		 }
	 }

	 /**
	  * Called from JPanelDisplay. Sets the orientation cube display on/off.
	  * @param bDisplay on/off.
	  */
	 public void displayOrientationCube( boolean bDisplay )
	 {
		 if ( m_kVolumeCube != null )
		 {
			 m_kVolumeCube.SetDisplay(bDisplay);
		 }
	 }

	 /**
	  * Toggle surface display on/off.
	  * @param bDisplay
	  */
	 public void displaySurface(boolean bDisplay)
	 {
		 for ( int i = 0; i < m_kDisplayList.size(); i++ )
		 {
			 if ( m_kDisplayList.get(i) instanceof VolumeSurface )
			 {
				 m_kDisplayList.get(i).SetDisplay(bDisplay);
			 }
		 }
	 }

	 /**
	  * Toggle volume display on/off.
	  * @param bDisplay
	  */
	 public void displayVolumeRaycast( boolean bDisplay )
	 {
		 if ( m_kVolumeRayCast != null )
		 {
			 m_kVolumeRayCast.SetDisplay(bDisplay);
		 }
	 }

	 /**
	  * Toggle 3D Slice display on/off.
	  * @param bDisplay
	  */
	 public void displayVolumeSlices( boolean bDisplay )
	 {
		 if ( m_kSlices != null )
		 {
			 m_kSlices.SetDisplay( bDisplay );
		 }
	 }

	 /**
	  * Toggle 3D Slice display on/off.
	  */
	 public void toggleDisplayVolumeSlices()
	 {
		 if ( m_kSlices != null )
		 {
			 m_kSlices.SetDisplay( !m_kSlices.GetDisplay() );
		 }
	 }
	 
	 /**
	  * Toggle Surface display on/off.
	  */
	 public void toggleDisplaySurface()
	 {
		 for ( int i = 0; i < m_kDisplayList.size(); i++ )
		 {
			 if ( m_kDisplayList.get(i) instanceof VolumeSurface )
			 {
				 m_kDisplayList.get(i).SetDisplay( !m_kDisplayList.get(i).GetDisplay() );
			 }
		 }
	 }
	 
	 /**
	  * Toggle 3D VOI display on/off.
	  */
	 public void toggleDisplayVOI()
	 {
		 for ( int i = 0; i < m_kDisplayList.size(); i++ )
		 {
			 if ( m_kDisplayList.get(i) instanceof VolumeVOI )
			 {
				 m_kDisplayList.get(i).SetDisplay( !m_kDisplayList.get(i).GetDisplay() );
			 }
		 }
	 }
	 
	 /**
	  * Toggle 3D Volume display on/off.
	  */
	 public void toggleDisplayVolume()
	 {
		 if ( m_kVolumeRayCast != null )
		 {
			 m_kVolumeRayCast.SetDisplay( !m_kVolumeRayCast.GetDisplay() );
			 setVolumeBlend( 1.0f );
			 setVolumeSamplesMouseReleased( .7f );
			 setVolumeSamplesMouseDragged( .7f );
		 }
	 }

	 public void dispose(GLAutoDrawable kDrawable)
	 {
		 if ( m_kSculptor != null )
		 { 
			 m_kSculptor.disposeLocal();
			 m_kSculptor = null;
		 }
		 m_afArbEquation = null;

		 m_kNewCenter = null;      	

		 if ( m_kPaintColor != null )
		 {
			 m_kPaintColor = null;
		 }
		 if ( m_kArbitraryClip != null )
		 {
			 m_kArbitraryClip = null;
		 }
		 if ( m_kFBO != null )
		 {
			 m_kFBO.SetDrawable(kDrawable);
			 m_kFBO.TerminateBuffer();
			 m_kFBO = null;
		 }
		 if ( m_pkPlane != null )
		 {
			 m_pkRenderer.ReleaseResources(m_pkPlane);
		 }
		 if ( m_akSceneTarget != null )
		 {
			 for ( int i = 0; i < m_akSceneTarget.length; i++ )
			 {
				 if ( m_akSceneTarget[i] != null )
				 {
					 m_pkRenderer.ReleaseTexture(m_akSceneTarget[i]);
					 m_akSceneTarget[i].dispose();
					 m_akSceneTarget[i] = null;
				 }
			 }
		 }
		 m_akSceneTarget = null;
		 if ( m_pkScreenCamera != null )
		 {
			 m_pkScreenCamera.dispose();
			 m_pkScreenCamera = null;
		 }
		 if ( !m_bShared )
		 {
			 disposeShared(m_kVolumeImageA, m_kVolumeImageB );
		 }
		 // Shared context will delete the slices:
		 m_kDisplayList.remove( m_kSlices );
		 m_kSlices = null;
		 m_kShared = null;
		 super.dispose(kDrawable);
	 }

	 protected void disposeShared (VolumeImage imageA, VolumeImage imageB )
	 {
		 m_pkRenderer.ReleaseTexture( imageA.GetVolumeTarget() );
		 m_pkRenderer.ReleaseTexture( imageA.GetColorMapTarget() );
		 if ( imageA.GetImage().isColorImage() )
		 {
			 m_pkRenderer.ReleaseTexture( imageA.GetNormalMapTarget() );
		 }
		 m_pkRenderer.ReleaseTexture( imageA.GetGradientMapTarget() );
		 m_pkRenderer.ReleaseTexture( imageA.GetOpacityMapGMTarget() );
		 m_pkRenderer.ReleaseTexture( imageA.GetSurfaceTarget() );
		 m_pkRenderer.ReleaseTexture( imageA.GetScratchTarget() );
		 if ( imageB.GetImage() != null )
		 {
			 m_pkRenderer.ReleaseTexture( imageB.GetVolumeTarget() );
			 m_pkRenderer.ReleaseTexture( imageB.GetColorMapTarget() );
			 if ( imageB.GetImage().isColorImage() )
			 {
				 m_pkRenderer.ReleaseTexture( imageB.GetNormalMapTarget() );
			 }
			 m_pkRenderer.ReleaseTexture( imageB.GetGradientMapTarget() );
			 m_pkRenderer.ReleaseTexture( imageB.GetOpacityMapGMTarget() );
			 m_pkRenderer.ReleaseTexture( imageB.GetSurfaceTarget() );   
			 m_pkRenderer.ReleaseTexture( imageB.GetScratchTarget() );     	
		 }
		  for ( int i = 0; i < ms_akLUTTexture.length; i++ )
		  {
			  m_pkRenderer.ReleaseTexture( ms_akLUTTexture[i] );			  
		  }
		 //m_kSlices.dispose(m_pkRenderer);  
	 }


	 public boolean drawVOI( VOIBase kVOI, VolumeTriPlanarRenderBase kDisplay, VolumeImage kVolumeImage, Vector3f kTranslate)
	 {
		 VolumeVOI kVolumeVOI = kVOI.getVolumeVOI();
		 boolean bReturn = false;
		 if ( kVolumeVOI == null )
		 {
			 kVolumeVOI = kVOI.createVolumeVOI( kVolumeImage, kTranslate);
			 bReturn = true;
		 }

		 kVolumeVOI.showTextBox(true);
		 kVolumeVOI.setZCompare(true);
		 kDisplay.addVolumeVOI( kVolumeVOI );
		 return bReturn | kVolumeVOI.needsUpdate();
	 }

	 /**
	  * Display the volume in DDR mode.
	  */
	  public void DRRMode()
	 {
		 if ( m_kVolumeRayCast != null )
		 {
			 m_kVolumeRayCast.DRRMode();
		 }
	 }

	 /**
	  * Enables the arbitrary clip plane position.
	  * @param bEnable clipping enabled
	  * @param bDisplay on/off.
	  * @param kColor the arbitrary clip plane color.
	  */
	  public void enableArbitraryClipPlane( boolean bEnable, boolean bDisplay, ColorRGB kColor )
	 {
		  if ( m_kVolumeClip != null )
		  {
			  m_kVolumeClip.setArbColor(kColor);
			  m_kVolumeClip.DisplayArb(bDisplay);
		  }
		  if ( !bEnable )
		  {
			  setArbitraryClipPlane((m_kVolumeImageA.GetImage().getExtents()[0] -1), bEnable);
		  }
	 }

	  /**
	   * Enables the axis-aligned clipping planes.
	   * @param iWhich the clip plane to enable.
	   * @param bEnable clipping enabled
	   * @param bDisplay on/off.
	   */
	  public void enableClipPlane( int iWhich, boolean bEnable, boolean bDisplay )
	  {
		  displayClipPlane( iWhich, bDisplay );

		  float fValue = 0;
		  if ( bEnable )
		  {
			  fValue = m_kVolumeClip.GetValue(iWhich);
		  }
		  else
		  { 
			  if ( iWhich%2 == 0 )
				  fValue = 0;
			  else
				  fValue = 1;
		  }

		  float[] data = new float[4];
		  data[0] = fValue;
		  if ( m_kVolumeRayCast != null )
		  {
			  m_kVolumeRayCast.SetClip(iWhich,fValue,bEnable);
		  }
		  for ( int i = 0; i < m_kDisplayList.size(); i++ )
		  {
			  if ( m_kDisplayList.get(i) instanceof VolumeSurface )
			  {
				  ((VolumeSurface)m_kDisplayList.get(i)).SetClip(iWhich,fValue,bEnable);
			  }
		  }
	  }

	  /**
	   * Enables the eye clip plane.
	   * @param bEnable clipping enabled
	   * @param bDisplay on/off.
	   * @param kColor the eye clip plane color.
	   */
	  public void enableEyeClipPlane( boolean bEnable, boolean bDisplay, ColorRGB kColor )
	  {
		  if ( m_kVolumeClip != null )
		  {
			  m_kVolumeClip.setClipPlaneColor( VolumeClip.CLIP_EYE, kColor);
			  m_kVolumeClip.DisplayEye(bDisplay);
		  }
		  if ( !bEnable )
		  {
			  setEyeClipPlane(0, bDisplay, bEnable);
		  }
	  }

	  /**
	   * Enables the inverse-eye clip plane.
	   * @param bEnable clipping enabled
	   * @param bDisplay on/off.
	   * @param kColor the inverse-eye clip plane color.
	   */
	  public void enableEyeInvClipPlane( boolean bEnable, boolean bDisplay, ColorRGB kColor )
	  {
		  if ( m_kVolumeClip != null )
		  {
			  m_kVolumeClip.setClipPlaneColor( VolumeClip.CLIP_EYE_INV, kColor);
			  m_kVolumeClip.DisplayEyeInv(bDisplay);
		  }
		  if ( !bEnable )
		  {
			  setEyeInvClipPlane(m_kVolumeImageA.GetImage().getExtents()[2] - 1, bDisplay, bEnable);
		  }
	  }


	  /**
	   * Enable geodesic curve calculations/display.
	   * @param bEnable
	   */
	  public void enableGeodesic( boolean bEnable )
	  {
		  m_bGeodesicEnabled = bEnable;
	  }

	  /**
	   * Enable painting on TriMesh surfaces.
	   * @param kPaintColor paint color.
	   * @param iBrushSize brush size.
	   * @param bEnabled painting on/off.
	   * @param bPaint when true apply paint.
	   * @param bDropper when true do dropper mode.
	   * @param bPaintCan when true do paint can mode.
	   * @param bErase when true erase.
	   */
	  public void enablePaint( ColorRGBA kPaintColor, int iBrushSize, boolean bEnabled, boolean bPaint, boolean bDropper, boolean bPaintCan, boolean bErase )
	  {
		  m_kPaintColor = kPaintColor;
		  m_iBrushSize = iBrushSize;
		  m_bPaintEnabled = bEnabled;
		  m_bPaint = bPaint;
		  m_bDropper = bDropper;
		  m_bPaintCan = bPaintCan;
		  m_bErase = bErase;
	  }

	  /**
	   * Enables and disables sculpting.
	   * @param bSculpt true to enable sculpting, false to disable.
	   */
	  public void enableSculpt( boolean bSculpt )
	  {
		  if ( m_kSculptor == null )
		  {
			  m_kSculptor = new Sculptor_WM( ((OpenGLRenderer)m_pkRenderer).GetCanvas() );
			  m_kSculptor.setImage(m_kVolumeImageA.GetImage(), m_kVolumeImageB.GetImage());
		  }
		  m_kSculptor.enableSculpt(bSculpt);
	  }

	  /**
	   * Erase all surface paint.
	   */
	  public void eraseAllPaint()
	  {
		  for ( int i = 0; i < m_kDisplayList.size(); i++ )
		  {
			  if ( m_kDisplayList.get(i) instanceof VolumeSurface )
			  {
				  ((VolumeSurface)(m_kDisplayList.get(i))).EraseAllPaint(m_pkRenderer);
			  }
		  }
	  }


	  /** Extract a TriMesh surface from the Volume Rendering. */
	  public void extractMeshFromVolume()
	  {
		  m_bExtract = true;
	  }

	  public Matrix3f getArbitratyClip()
	  {
		  return new Matrix3f(m_kVolumeClip.ArbRotate().Local.GetRotate());
	  }
	  
	  /**
	   * Returns the container for this object. The container has a scroll pane and slider for the depth.
	   * @return
	   */
	  public JPanel getContainingPanel()
	  {
		  return container;
	  }

	  /** Get the group color with given group ID. 
	   * @param groupID  given group id
	   * @return  ColorRGB group color
	   */
	  public ColorRGB getGroupColor(int groupID) {
		  if ( m_kDTIDisplay != null )
		  {
			  return m_kDTIDisplay.getGroupColor(groupID);
		  }
		  return null;
	  }

	  /**
	   * Sets the inter-pupillary distance for stereo rendering.
	   * @param fIPD the IPD value.
	   */
	  public float getIPD()
	  {
		  return m_fTrnSpeed;        
	  }

	  /**
	   * Return the material properties of the given surface.
	   * @param kSurfaceName the surface to query.
	   * @return the material properties of the surface.
	   */
	  public MaterialState getMaterial( String kSurfaceName )
	  {
		  for ( int i = 0; i < m_kDisplayList.size(); i++ )
		  {
			  if ( m_kDisplayList.get(i).GetName() != null )
			  {
				  if ( m_kDisplayList.get(i).GetName().equals(kSurfaceName))
				  {
					  return ((VolumeSurface)(m_kDisplayList.get(i))).GetMaterial();
				  }
			  }
		  }
		  return null;
	  }

	  /**
	   * Return the opacity properties of the given surface.
	   * @param kSurfaceName the surface to query.
	   * @return the opacity value of the surface.
	   */
	  public float getOpacity( String kSurfaceName )
	  {
		  for ( int i = 0; i < m_kDisplayList.size(); i++ )
		  {
			  if ( m_kDisplayList.get(i).GetName() != null )
			  {
				  if ( m_kDisplayList.get(i).GetName().equals(kSurfaceName))
				  {
					  return ((VolumeSurface)(m_kDisplayList.get(i))).GetOpacity();
				  }
			  }
		  }
		  return 0;
	  }


	  public Sculptor_WM getSculpt()
	  {
		  return m_kSculptor;
	  }


	  /**
	   * Returns true when sculpting is enabled.
	   * @return true when sculpting is enabled, false otherwise.
	   */
	  public boolean getSculptEnabled()
	  {
		  if ( m_kSculptor == null )
		  {
			  return false;
		  }
		  return m_kSculptor.getEnable();
	  }

	  /**
	   * Return the TriMesh surface with the given name.
	   * @param kSurfaceName the name of the surface.
	   * @return TriMesh.
	   */
	  public TriMesh getSurface( String kSurfaceName )
	  {
		  for ( int i = 0; i < m_kDisplayList.size(); i++ )
		  {
			  if ( m_kDisplayList.get(i).GetName() != null )
			  {
				  if ( m_kDisplayList.get(i).GetName().equals(kSurfaceName))
				  {
					  return m_kDisplayList.get(i).GetMesh();
				  }
			  }
		  }
		  return null;
	  }

	  /**
	   * Return the TriMesh surface with the given name.
	   * @param kSurfaceName the name of the surface.
	   * @return TriMesh.
	   */
	  public VolumeSurface getVolumeSurface( String kSurfaceName )
	  {
		  for ( int i = 0; i < m_kDisplayList.size(); i++ )
		  {
			  if ( m_kDisplayList.get(i).GetName() != null )
			  {
				  if ( m_kDisplayList.get(i).GetName().equals(kSurfaceName))
				  {
					  return (VolumeSurface)m_kDisplayList.get(i);
				  }
			  }
		  }
		  return null;
	  }



	  /**
	   * Return the surface area for the given TriMesh surface.
	   * @param kSurfaceName the surface name.
	   * @return the surface-area of the mesh.
	   */
	  public float getSurfaceArea( String kSurfaceName )
	  {
		  for ( int i = 0; i < m_kDisplayList.size(); i++ )
		  {
			  if ( m_kDisplayList.get(i).GetName() != null )
			  {
				  if ( m_kDisplayList.get(i).GetName().equals(kSurfaceName))
				  {
					  return ((VolumeSurface)(m_kDisplayList.get(i))).GetSurfaceArea();
				  }
			  }
		  }
		  return 0;
	  }

	  /**
	   * Return the surface area for the given TriMesh surface.
	   * @param kSurfaceName the surface name.
	   * @return the surface-area of the mesh.
	   */
	  public String getSurfaceAreaString( String kSurfaceName )
	  {
		  for ( int i = 0; i < m_kDisplayList.size(); i++ )
		  {
			  if ( m_kDisplayList.get(i).GetName() != null )
			  {
				  if ( m_kDisplayList.get(i).GetName().equals(kSurfaceName))
				  {
					  return ((VolumeSurface)(m_kDisplayList.get(i))).GetSurfaceAreaString();
				  }
			  }
		  }
		  return "";
	  }


	  /**
	   * Return the center of the TriMesh surface.
	   * @param kSurfaceName the surface name.
	   * @return the calculated center.
	   */
	  public Vector3f getSurfaceCenter( String kSurfaceName )
	  {
		  for ( int i = 0; i < m_kDisplayList.size(); i++ )
		  {
			  if ( m_kDisplayList.get(i).GetName() != null )
			  {
				  if ( m_kDisplayList.get(i).GetName().equals(kSurfaceName))
				  {
					  return ((VolumeSurface)(m_kDisplayList.get(i))).GetCenter();
				  }
			  }
		  }
		  return null;
	  }

	  /**
	   * Return the volume of the TriMesh surface.
	   * @param kSurfaceName the surface name.
	   * @return the calculated volume.
	   */
	  public float getSurfaceVolume( String kSurfaceName )
	  {
		  for ( int i = 0; i < m_kDisplayList.size(); i++ )
		  {
			  if ( m_kDisplayList.get(i).GetName() != null )
			  {
				  if ( m_kDisplayList.get(i).GetName().equals(kSurfaceName))
				  {
					  return ((VolumeSurface)(m_kDisplayList.get(i))).GetVolume();
				  }
			  }
		  }
		  return 0;
	  }

	  /**
	   * Return the volume of the TriMesh surface.
	   * @param kSurfaceName the surface name.
	   * @return the calculated volume.
	   */
	  public String getSurfaceVolumeString( String kSurfaceName )
	  {
		  for ( int i = 0; i < m_kDisplayList.size(); i++ )
		  {
			  if ( m_kDisplayList.get(i).GetName() != null )
			  {
				  if ( m_kDisplayList.get(i).GetName().equals(kSurfaceName))
				  {
					  return ((VolumeSurface)(m_kDisplayList.get(i))).GetVolumeString();
				  }
			  }
		  }
		  return "";
	  }


	  /**
	   * Return the translation vector for the surface with the given name.
	   * @param kSurfaceName the surface to move.
	   * @return the translation vector
	   */
	  public Vector3f getTranslateSurface(String kSurfaceName)
	  {
		  for ( int i = 0; i < m_kDisplayList.size(); i++ )
		  {
			  if ( m_kDisplayList.get(i).GetName() != null )
			  {
				  if ( m_kDisplayList.get(i).GetName().equals(kSurfaceName))
				  {
					  return m_kDisplayList.get(i).GetTranslate();
				  }
			  }
		  }
		  return new Vector3f(Vector3f.ZERO);
	  }

	  /**
	   * Part of the GLEventListener interface. Init is called once when the
	   * GLCanvas is first displayed. Called again if the GLCanvas is removed
	   * from the frame and added to another window or panel. Initializes the
	   * display.
	   * @param arg0 GLCanvas
	   */
	  @Override
	  public void init(GLAutoDrawable arg0) {			  
		  if ( m_kVolumeImageA == null ) {
			  return;
		  }      
			//-javaagent:C:\GeometricToolsInc\mipav\src\lib\profile.jar
			//-Dprofile.properties=C:\GeometricToolsInc\mipav\src\lib\profile.properties
			//Profile.clear();
			//Profile.start();
		  if ( m_pkRenderer != null )
		  {
			  ((OpenGLRenderer)m_pkRenderer).SetDrawable( arg0 );
		  }
		  m_bInit = true;

		  m_pkRenderer.InitializeState();
		  m_pkRenderer.SetLineWidth(3);

		  super.OnInitialize();
		  if ( !m_bShared || m_bStandAlone )
		  {
			  loadShared( );
			  if ( !m_bStandAlone )
			  {
				  return;
			  }
		  }
		  
		  
//		  m_spkCamera.SetFrustum(60.0f,m_iWidth/(float)m_iHeight,0.01f,10.0f);
          m_spkCamera.SetFrustum(30.0f,m_iWidth/(float)m_iHeight,.5f,10.0f);
		  Vector3f kCDir = new Vector3f(0.0f,0.0f,1.0f);
		  Vector3f kCUp = new Vector3f(0.0f, -1.0f,0.0f);
		  Vector3f kCRight = Vector3f.cross( kCDir, kCUp );
		  Vector3f kCLoc = Vector3f.scale(-2.4f, kCDir);
		  m_spkCamera.SetFrame(kCLoc,kCDir,kCUp,kCRight);

		  if ( !m_bStandAlone )
		  {
			  //CopyShared( );
		  }

		  CreateScene( );
		  // initial update of objects
		  m_spkScene.UpdateGS();
		  m_spkScene.UpdateRS();

		  // initial culling of scene
		  m_kCuller.SetCamera(m_spkCamera);
		  m_kCuller.ComputeVisibleSet(m_spkScene);

		  InitializeCameraMotion(.05f, 0.1f);
		  UpdateCameraZoomSpeed(.05f);
		  InitializeObjectMotion(m_spkScene);
	  }

	  /**
	   * Invert the sculpt region.
	   */
	  public void invertSculpt()
	  {
		  if ( m_kSculptor != null )
		  {
			  m_kSculptor.invertSculpt();
		  }
	  }

	  /**
	   * Roll back the image back to center, reset the camera view close to original view. 
	   */
	  public void rollbackToCenter() {
		  
		  m_spkCamera.SetFrustum(30.0f,m_iWidth/(float)m_iHeight,.5f,10.0f);
		  Vector3f kCDir = new Vector3f(0.0f,0.0f,1.0f);
		  Vector3f kCUp = new Vector3f(0.0f, -1.0f,0.0f);
		  Vector3f kCRight = Vector3f.cross( kCDir, kCUp );
		  Vector3f kCLoc = Vector3f.scale(-2.4f, kCDir);
		  m_spkCamera.SetFrame(kCLoc,kCDir,kCUp,kCRight);
		  
		  m_spkCamera.SetFrame(kCLoc,kCDir,kCUp,kCRight);
		  InitializeCameraMotion(.05f, 0.1f);
		  UpdateCameraZoomSpeed(.05f);
		  resetAxis();
		  m_kCuller.ComputeVisibleSet(m_spkScene);
		  InitializeObjectMotion(m_spkScene);
		  
	  }
	  
	  /**
	   * Part of the KeyListener interface. Pressing 'b' toggles displaying the
	   * proxy-geometry versus the ray-traced volume.
	   * @param e the key event.
	   */
	  public void keyPressed(KeyEvent e) {
		  
		  int ikey = e.getKeyCode();
		  if ( ikey == KeyEvent.VK_HOME ) {
			  rollbackToCenter();
			  return;
		  }
		  
		  char ucKey = e.getKeyChar();
		  super.keyPressed(e);
		  switch (ucKey)
		  {
		  case '0':
			  m_iUpdateNormals = 0;
			  break;
		  case '1':
			  m_iUpdateNormals = 1;
			  break;
		  case '2':
			  m_iUpdateNormals = 2;
			  break;
		  case '3':
			  m_iUpdateNormals = 3;
			  break;
		  case 'z':
			  m_bDoClip = !m_bDoClip;
			  System.err.println( "Clipping is " + m_bDoClip );
			  break;
		  case 'b':
			  m_bDisplaySecond = !m_bDisplaySecond;
			  return;
		  case 'p':
			  displayBoundingBox( !m_kVolumeBox.GetDisplay() );
			  return;
		  case 'o':
			  displayOrientationCube( !m_kVolumeCube.GetDisplay() );
			  return;
		  case 'c':
			  for ( int i = 0; i < m_kDisplayList.size(); i++ )
			  {
				  if ( m_kDisplayList.get(i) instanceof VolumeSurface )
				  {
					  VolumeSurface kSurface = (VolumeSurface)m_kDisplayList.get(i);
					  kSurface.SetFrontface(true);
				  }
			  }
			  break;
		  case 'C':
			  for ( int i = 0; i < m_kDisplayList.size(); i++ )
			  {
				  if ( m_kDisplayList.get(i) instanceof VolumeSurface )
				  {
					  VolumeSurface kSurface = (VolumeSurface)m_kDisplayList.get(i);
					  kSurface.SetBackface(true);
				  }
			  }
			  break;
		  case 's':
			  m_bWriteImage = true;
			  break;
		  case 'v':
			  displayVolumeRaycast( !m_kVolumeRayCast.GetDisplay() );
			  setVolumeBlend( 1.0f );
			  setVolumeSamplesMouseReleased( .7f );
			  setVolumeSamplesMouseDragged( .7f );
			  break;
		  case 't':
			  m_kVolumeRayCast.printProgram();
			  break;
		  case 'w':
			  profile = true;
//			  Profile.clear();
//			  Profile.start();
			  break;
		  case 'W':
//			  Profile.stop();
//			  Profile.setFileName( "volume_render_profile" );
//			  Profile.shutdown();
//			  System.err.println( "Profile DONE" );
			  profile = false;
			  break;
		  }
		  
		 
		  
		  return;
	  }
	  boolean profile = false;

	  /**
	   * Display the volume in MIP mode.
	   */
	  public void MIPMode()
	  {
		  if ( m_kVolumeRayCast != null )
		  {
			  m_kVolumeRayCast.MIPMode();
		  }
	  }

		/* (non-Javadoc)
		 * @see java.awt.event.MouseListener#mouseClicked(java.awt.event.MouseEvent)
		 */
		public void mouseClicked(MouseEvent arg0) {
			if ( arg0.getClickCount() > 1 )
			{
				if ( container != null )
				{
					container.minMax();
				}
			}
		}
	  
	  /** Rotates the object with a virtual trackball:
	   * @param e the MouseEvent
	   */
	  public void mouseDragged(MouseEvent e)
	  {

		  if ( !getSculptEnabled() )
		  {
			  if ( !e.isControlDown() )
			  {
				  super.mouseDragged(e);
			  }
			  else if ( e.isControlDown() && (m_kVolumeClip != null) && (m_kVolumeClip.DisplayArb()) )
			  {
				  InitializeObjectMotion(m_kVolumeClip.ArbRotate());
				  super.mouseDragged(e);
				  InitializeObjectMotion(m_spkScene);
				  doClip(m_bArbClipOn);
			  }
			  else if ( e.isControlDown() && m_bPaintEnabled )
			  {
				  m_iXPick = e.getX();
				  m_iYPick = e.getY();
				  m_bPickPending = true;
			  }
		  }
		  if ( m_kVolumeRayCast != null ) {
			  m_kVolumeRayCast.setVolumeSamples( fSample_mouseDragged );
		  }
	  }

	  /** Rotates the object with a virtual trackball:
	   * @param e the MouseEvent
	   */
	  @Override
	  public void mousePressed(MouseEvent e)
	  {
		  if ( !e.isControlDown() )
		  {
			  super.mousePressed(e);
		  }
		  else if ( (m_kVolumeClip != null) && (m_kVolumeClip.DisplayArb()) )
		  {
			  InitializeObjectMotion(m_kVolumeClip.ArbRotate());
			  super.mousePressed(e);
			  InitializeObjectMotion(m_spkScene);
		  }
		  if ( e.isControlDown() && m_bPaintEnabled )
		  {
			  m_iXPick = e.getX();
			  m_iYPick = e.getY();
			  m_bPickPending = true;
		  }
		  if ( e.isControlDown() && m_bGeodesicEnabled )
		  {
			  m_iXPick = e.getX();
			  m_iYPick = e.getY();
			  m_bPickPending = true;
		  }
		  if ( e.isControlDown() && m_bPickCorrespondence )
		  {
			  m_iXPick = e.getX();
			  m_iYPick = e.getY();
			  m_bPickPending = true;
		  }
	  }

	  /** Rotates the object with a virtual trackball:
	   * @param e the MouseEvent
	   */
	  @Override
	  public void mouseReleased(MouseEvent e)
	  {
		  super.mouseReleased(e);
		  if ( m_kVolumeRayCast != null ) {
			  m_kVolumeRayCast.setVolumeSamples( fSample_mouseReleased );
		  }
	  }

	  /**
	   * Display the volume in Mullti-histo mode.
	   */
	  public void MULTIHISTOMode(boolean bOn)
	  {
		  if ( m_kVolumeRayCast != null )
		  {
			  m_kVolumeRayCast.MULTIHISTOMode(bOn);
		  }
	  }


	  /**
	   * Enables/disables picking correspondence points between the mesh and BrainSurfaceFlattener render.
	   * @param bOn
	   */
	  public void pickCorrespondence( boolean bOn )
	  {
		  m_bPickCorrespondence = bOn;
	  }

	  public void play4D( boolean bOn )
	  {
		  m_bPlay4D = bOn;
	  }


	  /**
	   * Causes the texture representation of all the surface meshes to be recalculated.
	   */
	  public void redrawSurfaceTexture()
	  {
		  m_bSurfaceUpdate = true;
	  }

	  /**
	   * Called by the ApplicationGUI. Causes the current shader to be reloaded
	   * from file, compiled and applied to the proxy-geometry.
	   */
	  @Override
	  public void reloadShaders()
	  {
		  if ( m_kVolumeRayCast != null )
		  {
			  m_kVolumeRayCast.ReloadVolumeShader( m_pkRenderer );
		  }
		  updateLighting(m_akLights);
	  }

	  /**
	   * Removes all geodesic components from the given surface.
	   * @param kSurface
	   */
	  public void removeAllGeodesic( TriMesh kSurface )
	  {
		  for ( int i = 0; i < m_kDisplayList.size(); i++ )
		  {
			  if ( m_kDisplayList.get(i).GetMesh() != null )
			  {
				  if ( m_kDisplayList.get(i).GetMesh() == kSurface)
				  {
					  ((VolumeSurface)(m_kDisplayList.get(i))).RemoveAllGeodesic();
				  }
			  }
		  }
	  }

	  /**
	   * Removes the specific geodesic component from the given surface.
	   * @param kSurface
	   * @param iNode
	   * @param iGroup
	   */
	  public void removeGeodesic( TriMesh kSurface, int iNode, int iGroup )
	  {
		  for ( int i = 0; i < m_kDisplayList.size(); i++ )
		  {
			  if ( m_kDisplayList.get(i).GetMesh() != null )
			  {
				  if ( m_kDisplayList.get(i).GetMesh() == kSurface)
				  {
					  ((VolumeSurface)(m_kDisplayList.get(i))).RemoveGeodesic(iNode, iGroup);
				  }
			  }
		  }
	  }
	  

	  public Vector<VOIContour> getPolylines( int iGroup )
	  {
		  if ( m_kDTIDisplay != null )
		  {
			  return m_kDTIDisplay.getPolylines(iGroup);
		  }
		  return null;
	  }

	  /** 
	   * Removes the specified polyline tract group.
	   * @param iGroup the group of polylines to remove.
	   */
	  public void removePolyline( int iGroup )
	  {
		  if ( m_kDTIDisplay != null )
		  {
			  m_kDTIDisplay.removePolyline(iGroup);
		  }
		  m_kDTIDisplay.SetDisplay( m_kDTIDisplay.GetDisplayTract() );
	  }

	  /**
	   * Sets blending between imageA and imageB.
	   * @param fValue the blend value (0-1)
	   */
	  public void removeSurface( String kSurfaceName )
	  {
		  for ( int i = 0; i < m_kDisplayList.size(); i++ )
		  {
			  if ( m_kDisplayList.get(i).GetName() != null )
			  {
				  if ( m_kDisplayList.get(i).GetName().equals(kSurfaceName))
				  {
					  VolumeObject kObj = m_kDisplayList.remove(i);
					  if ( kObj instanceof VolumeSurface )
					  {
						  m_bSurfaceUpdate = true;    
						  m_bSurfaceMaskUpdate = true;   
						  m_kVolumeImageA.removeSurfaceMask(kSurfaceName);
					  }
					  m_kDeleteList.add(kObj);
				  }
			  }
		  }
	  }

	  /**
	   * When the Geodesic object cuts the mesh along an open curve, the old mesh changes, but does not need to be deleted
	   * and no new mesh needs to be added. This function allows the Geodesic object to replace the original mesh with the
	   * sliced mesh in the surface renderer. ReplaceMesh is also used to undo cutting operations.
	   *
	   * @param  kOld  TriMesh old surface mesh
	   * @param  kNew  TriMesh new surface mesh
	   */
	  public void replaceGeodesic(TriMesh kOld, TriMesh kNew)
	  {
		  for ( int i = 0; i < m_kDisplayList.size(); i++ )
		  {
			  if ( m_kDisplayList.get(i).GetMesh() != null )
			  {
				  if ( m_kDisplayList.get(i).GetMesh() == kOld)
				  {
					  ((VolumeSurface)(m_kDisplayList.get(i))).ReplaceGeodesic(kNew);
				  }
			  }
		  }
	  }
	  @Override
	  public void reshape(GLAutoDrawable arg0, int iX, int iY, int iWidth, int iHeight) {
		  if ( m_iWidth != iWidth || m_iHeight != iHeight )
		  {
			  m_bDisplay = false;
			  super.reshape( arg0, iX, iY, iWidth, iHeight );     
			  if ( m_pkRenderer != null )
			  {
				  ((OpenGLRenderer)m_pkRenderer).SetDrawable( arg0 );
			  }
			  CreateRenderTarget(arg0, iWidth, iHeight);
			  if ( m_kSculptor != null )
			  {
				  m_kSculptor.enableSculpt(m_kSculptor.getEnable());
			  }
			  m_bDisplay = true;
		  }
	  }

	  /**
	   * Save the sculpted volume.
	   * @param options file writing options
	   * @param filtertype 
	   * @return boolean, true on sucess.
	   */
	  public boolean save(FileWriteOptions options, int filterType)
	  {
		  if ( m_kSculptor == null )
		  {
			  return false;
		  }
		  return m_kSculptor.save(options, filterType);
	  }

	  /**
	   * Copies the volume data from the texture representation into the ModelImage and writes it to disk.
	   */
	  public void saveCroppedImage()
	  {
		  ModelImage kImage = m_kVolumeImageA.GetImage();
		  VolumeClipEffect clipE = m_kVolumeRayCast.GetClipEffect();
		  Vector3f clip = clipE.getClip();
		  Vector3f clipI = clipE.getClipInv();
		  boolean doClip = clipE.isClip();
		  OpenCLAlgorithmVolumeCrop oclCrop = new OpenCLAlgorithmVolumeCrop( kImage );
		  oclCrop.setClip( clip, clipI, doClip );
		  Matrix3f rot = GetSceneRotation();
		  Matrix4f rot4 = new Matrix4f( rot.M00, rot.M01, rot.M02, 0,
				  rot.M10, rot.M11, rot.M12, 0,
				  rot.M20, rot.M21, rot.M22, 0,
				  0, 0, 0, 1
				  );
		  float[] rotMatrix = new float[16];
		  rot4.getData(rotMatrix);
		  oclCrop.setClipEyeArb( clipE.getClipEye(), clipE.getClipEyeInv(), clipE.getClipArb(), rotMatrix );
		  oclCrop.run();
		  ModelImage result = oclCrop.getDestImage();
		  result.calcMinMax();
		  //new ViewJFrameImage(result);
		  
		  result.setImageName( m_kVolumeImageA.GetImage().getImageName() + "_Crop" );
		  JDialogBase.updateFileInfo( m_kVolumeImageA.GetImage(), result );
		  result.calcMinMax();
		  int fileType = m_kVolumeImageA.GetImage().getFileInfo(0).getFileFormat();
		  String suffix = FileTypeTable.getFileTypeInfo(fileType).getDefaultExtension();
		  ModelImage.saveImage( result, result.getImageName() + suffix, result.getImageDirectory() );
		  
		  //System.err.println( result.getImageName() + suffix + "   " + result.getImageDirectory() );

	  }

	  /**
	   * Enables/Disables self-shadowing in the Surface mode.
	   * @param bShadow shadow on/off.
	   */
	  public void selfShadow(boolean bShadow)
	  {
		  if ( m_kVolumeRayCast != null )
		  {
			  m_kVolumeRayCast.SelfShadow(bShadow);
		  }
	  }

	  /**
	   * Sets blending between imageA and imageB.
	   * @param fValue the blend value (0-1)
	   */
	  public void setABBlend( float fValue )
	  {
		  if ( m_kVolumeRayCast != null )
		  {
			  m_kVolumeRayCast.setABBlend(fValue);
		  }

		  if ( m_kSlices != null )
		  {
			  m_kSlices.setABBlend(fValue);
		  }
	  }

	  public void setAnimationSpeed( float fValue )
	  {
		  m_fAnimateRate = 32 * ( 1 - fValue );
	  }

	  /**
	   * Sets the arbitrary clip plane color.
	   * @param kColor the new color.
	   */
	  public void setArbColor( ColorRGB kColor )
	  {
		  kColor.R = (float)(kColor.R/255.0);
		  kColor.G = (float)(kColor.G/255.0);
		  kColor.B = (float)(kColor.B/255.0);
		  if ( m_kVolumeClip != null )
		  {
			  m_kVolumeClip.setArbColor(kColor);
		  }
	  }

	  /**
	   * Enables the arbitrary clip plane position.
	   * @param f4 clip position (same value as aSlice in JPanelClip)
	   */
	  public void setArbitraryClipPlane( float f4, boolean bEnable )
	  {
		  int[] aiExtents = m_kVolumeImageA.GetImage().getExtents();
		  f4 /= (aiExtents[0] -1);
//		  f4 /= (m_kVolumeImageA.GetMaxExtent() -1);     
		  m_kArbitraryClip = new Vector4f(1,0,0,f4);
		  doClip(bEnable);
	  }


	  public void setArbitratyClip(Matrix3f kRotate)
	  {
		  m_kVolumeClip.ArbRotate().Local.SetRotate(kRotate);
		  doClip(m_bArbClipOn);
	  }

	  /**
	   * Turn backface culling on/off for the given surface.
	   * @param kSurfaceName surface name.
	   * @param bOn
	   */
	  public void setBackface(String kSurfaceName, boolean bOn)
	  {
		  for ( int i = 0; i < m_kDisplayList.size(); i++ )
		  {
			  if ( m_kDisplayList.get(i).GetName() != null )
			  {
				  if ( m_kDisplayList.get(i).GetName().equals(kSurfaceName))
				  {
					  ((VolumeSurface)m_kDisplayList.get(i)).SetBackface( bOn );
				  }
			  }
		  }
	  }

	  /**
	   * Sets the background color.
	   * @param kColor new background color.
	   */
	  public void setBackgroundColor( ColorRGBA kColor )
	  {
		  m_kBackgroundColor.Copy(kColor);
		  if ( m_kVolumeRayCast != null )
		  {
			  m_kVolumeRayCast.SetBackgroundColor( kColor );
		  }
		  if ( m_kVolumeBox != null )
		  {
			  m_kVolumeBox.SetBackgroundColor( kColor );
		  }
	  }

	  /**
	   * Called from JPanelDisplay. Sets the bounding box color.
	   * @param kColor bounding box color.
	   */
	  public void setBoundingBoxColor( ColorRGB kColor )
	  {
		  if ( m_kVolumeBox != null )
		  {
			  m_kVolumeBox.SetBoundingBoxColor(kColor);
		  }
	  }

	  /**
	   * Sets the color for the input slice frame.
	   *
	   * @param  i, the slice bounding frame.
	   * @param  color  the new color.
	   */
	  public void setBoundingBoxColor( int i, ColorRGB kColor )
	  {
		  if ( m_kSlices != null )
		  {
			  m_kSlices.SetBoundingBoxColor( i, kColor );
		  }
		  else if ( m_akSlicesColor != null && i < m_akSlicesColor.length && m_akSlicesColor[i] != null )
		  {
			  m_akSlicesColor[i].Copy(kColor);
		  }
	  }

	  /**
	   * Sets the position of the 3D slices.
	   * @param kCenter the intersection point of the three slices.
	   */
	  public void setCenter( Vector3f kCenter )
	  {
		  m_bUpdateCenterOnDisplay = true;
		  m_kNewCenter = kCenter;
	  }


	  /**
	   * Enable clipping for the given surface.
	   * @param kSurfaceName the surface to modify.
	   * @param bClip true enables clipping, false disables clipping.
	   */
	  public void setClipping(String kSurfaceName, boolean bClip)
	  {
		  for ( int i = 0; i < m_kDisplayList.size(); i++ )
		  {
			  if ( m_kDisplayList.get(i).GetName() != null )
			  {
				  if ( m_kDisplayList.get(i).GetName().equals(kSurfaceName))
				  {
					  ((VolumeSurface)m_kDisplayList.get(i)).SetClipping( bClip );
				  }
			  }
		  }
	  }

	  /**
	   * Called from JPanelClip. Sets the axis-aligned clip plane.
	   * @param iWhich the clip plane to set.
	   * @param fValue the new position of the clip plane (the same value as
	   * the slider in JPanelClip).
	   */
	  public void setClipPlane( int iWhich, float fValue, boolean bEnable )
	  {
		  int[] aiExtents = m_kVolumeImageA.GetImage().getExtents();
		  if ( iWhich < 2 )
		  {
			  fValue /= (aiExtents[0] -1);
		  }
		  else if ( iWhich < 4 )
		  {
			  fValue /= (aiExtents[1]-1);
		  }
		  else
		  {
			  fValue /= (aiExtents[2] -1);
		  }
		  if ( m_kVolumeClip != null )
		  {
			  m_kVolumeClip.setClipPlane(iWhich, fValue);
		  }

		  float[] data = new float[4];
		  data[0] = fValue;
		  if ( m_kVolumeRayCast != null )
		  {
			  m_kVolumeRayCast.SetClip(iWhich, fValue, bEnable);
		  }  
		  for ( int i = 0; i < m_kDisplayList.size(); i++ )
		  {
			  if ( m_kDisplayList.get(i) instanceof VolumeSurface )
			  {
				  ((VolumeSurface)m_kDisplayList.get(i)).SetClip(iWhich,fValue,bEnable);
			  }
		  }
	  }

	  /**
	   * Sets the axis-aligned clip plane color.
	   * @param iWhich one of the 6 clip planes
	   * @param kColor the new color.
	   */
	  public void setClipPlaneColor( int iWhich, ColorRGB kColor )
	  {
		  kColor.R = (float)(kColor.R/255.0);
		  kColor.G = (float)(kColor.G/255.0);
		  kColor.B = (float)(kColor.B/255.0);

		  if ( m_kVolumeClip != null )
		  {
			  m_kVolumeClip.setClipPlaneColor(iWhich, kColor);
		  }
	  }

	 /**
	 * @param kSurfaceName
	 * @param kColor
	 * @param bUpdate
	 */
	public void setColor( String kSurfaceName, ColorRGB kColor, boolean bUpdate )
	  {
		  for ( int i = 0; i < m_kDisplayList.size(); i++ )
		  {
			  if ( m_kDisplayList.get(i).GetName() != null )
			  {
				  if ( m_kDisplayList.get(i).GetName().equals(kSurfaceName))
				  {
					  m_kDisplayList.get(i).SetColor( kColor, bUpdate );
					  m_kVolumeImageA.setSurfaceMaskColor(kSurfaceName, kColor);
					  m_bSurfaceMaskUpdate = true;
				  }
			  }
		  }
	  }

	  public void SetCustomBlend(int iBlendEquation, int iLogicOp, int iSrcBlend, int iDstBlend, ColorRGBA kColor  )
	  {
		  m_kVolumeRayCast.SetCustomBlend( iBlendEquation, iLogicOp, iSrcBlend, iDstBlend, kColor );
	  }    


	  /** Turns on/off displaying all the glyphs.
	   * @param bDisplay when true display all the glyphs in the volume.
	   */
	  public void setDisplayAllGlyphs( boolean bDisplay )
	  {
		  if ( m_kDTIDisplay != null )
		  {
			  m_kDTIDisplay.setDisplayAllGlyphs(bDisplay);
			  m_bSurfaceUpdate |= bDisplay;
		  }
	  }  


	  /** Turns on/off displaying the fiber bundle tracts with 3D arrows.
	   * @param bDisplay when true display the tracts with 3D arrows.
	   */
	  public void setDisplayArrows( boolean bDisplay )
	  {
		  if ( m_kDTIDisplay != null )
		  {
			  m_kDTIDisplay.setDisplayArrows( bDisplay );
			  m_bSurfaceUpdate |= bDisplay;
		  }
	  }

	  /** Turns on/off displaying the fiber bundle tracts with ellipsoids.
	   * @param bDisplay when true display the tracts with Cylinders.
	   */
	  public void setDisplayCylinders( boolean bDisplay )
	  {
		  if ( m_kDTIDisplay != null )
		  {
			  m_kDTIDisplay.setDisplayCylinders( bDisplay );
			  m_bSurfaceUpdate |= bDisplay;
		  }
	  }

	  /** Turns on/off displaying the fiber bundle tracts with ellipsoids.
	   * @param bDisplay when true display the tracts with ellipsods.
	   */
	  public void setDisplayEllipsoids( boolean bDisplay )
	  {
		  if ( m_kDTIDisplay != null )
		  {
			  m_kDTIDisplay.setDisplayEllipsoids( bDisplay );
			  m_bSurfaceUpdate |= bDisplay;
		  }
	  }


	  /** Turns on/off displaying the fiber bundle tracts with tubes.
	   * @param bDisplay when true display the tracts with tubes.
	   */
	  public void setDisplayTubes( boolean bDisplay )
	  {
		  if ( m_kDTIDisplay != null )
		  {
			  m_kDTIDisplay.setDisplayTubes( bDisplay );
			  m_bSurfaceUpdate |= bDisplay;
		  }
	  }

	  /**
	   * Sets the sculpt drawing shape.
	   * @param shape (0 = free-hand, 1 = rectangular)
	   */
	  public void setDrawingShape(int shape)
	  {
		  if ( m_kSculptor == null )
		  {
			  return;
		  }
		  m_kSculptor.setDrawingShape(shape);
	  }

	  /** Sets the DTI Image for displaying the tensors as ellipsoids.
	   * @param kDTIImage.
	   */
	  public void setDTIImage( ModelImage kDTIImage, ModelImage kEigenVectorImage, ModelImage kEigenValueImage )
	  {
		  if ( m_kDTIDisplay == null )
		  {
			  m_kDTIDisplay = new VolumeDTI( m_kVolumeImageA, m_kTranslate, m_fX, m_fY, m_fZ );
			  m_kDisplayList.add(1, m_kDTIDisplay);
		  }
		  m_kDTIDisplay.setDTIImage(kDTIImage, kEigenVectorImage, kEigenValueImage, m_pkRenderer );
		  m_bSurfaceUpdate = true;
	  }

	  /** Set the m_iEllipsoidMod value. 
	   * @param iMod new m_iEllipsoidMod value.
	   */
	  public void setEllipseMod( int iMod )
	  {
		  if ( m_kDTIDisplay != null )
		  {
			  m_kDTIDisplay.setEllipseMod( iMod );
		  }
	  }


	  /**
	   * Sets the eye clip plane position.
	   * @param f4 clip position (same value as sSlice in JPanelClip)
	   * @param bDisplay on/off.
	   */
	  public void setEyeClipPlane( float f4, boolean bDisplay, boolean bEnable )
	  {
		  f4 /= (m_kVolumeImageA.GetImage().getExtents()[2] -1);
		  float[] afEquation = new float[]{0,0,1,f4};
		  float fZ = afEquation[3] * m_fZ;

		  if ( m_kVolumeClip != null )
		  {
			  m_kVolumeClip.DisplayEye(bDisplay);
			  m_kVolumeClip.setEyeClipPlane( fZ );
		  }
		  if ( m_kVolumeRayCast != null )
		  {
			  m_kVolumeRayCast.SetClipEye(afEquation, bEnable);
		  }
		  for ( int i = 0; i < m_kDisplayList.size(); i++ )
		  {
			  if ( m_kDisplayList.get(i) instanceof VolumeSurface )
			  {
				  ((VolumeSurface)m_kDisplayList.get(i)).SetClipEye(afEquation, bEnable);
			  }
		  }
	  }

	  /**
	   * Sets the eye clip plane color.
	   * @param kColor the new color.
	   */
	  public void setEyeColor( ColorRGB kColor )
	  {
		  kColor.R = (float)(kColor.R/255.0);
		  kColor.G = (float)(kColor.G/255.0);
		  kColor.B = (float)(kColor.B/255.0);
		  if ( m_kVolumeClip != null )
		  {
			  m_kVolumeClip.setClipPlaneColor( VolumeClip.CLIP_EYE, kColor);
		  }
	  }

	  /**
	   * Sets the inverse-eye clip plane position.
	   * @param f4 clip position (same value as sSliceInv in JPanelClip)
	   * @param bDisplay on/off.
	   */
	  public void setEyeInvClipPlane( float f4, boolean bDisplay, boolean bEnable )
	  {
		  f4 /= (m_kVolumeImageA.GetImage().getExtents()[2] -1);
		  float[] afEquation = new float[]{0,0,1,f4};
		  float fZ = afEquation[3] * m_fZ;

		  if ( m_kVolumeClip != null )
		  {
			  m_kVolumeClip.DisplayEyeInv(bDisplay);
			  m_kVolumeClip.setEyeInvClipPlane( fZ );
		  }
		  if ( m_kVolumeRayCast != null )
		  {
			  m_kVolumeRayCast.SetClipEyeInv(afEquation, bEnable);
		  }
		  for ( int i = 0; i < m_kDisplayList.size(); i++ )
		  {
			  if ( m_kDisplayList.get(i) instanceof VolumeSurface )
			  {
				  ((VolumeSurface)m_kDisplayList.get(i)).SetClipEyeInv(afEquation, bEnable);
			  }
		  }
	  }

	  /**
	   * Sets the inverse-eye clip plane color.
	   * @param kColor the new color.
	   */
	  public void setEyeInvColor( ColorRGB kColor )
	  {
		  kColor.R = (float)(kColor.R/255.0);
		  kColor.G = (float)(kColor.G/255.0);
		  kColor.B = (float)(kColor.B/255.0);
		  if ( m_kVolumeClip != null )
		  {
			  m_kVolumeClip.setClipPlaneColor( VolumeClip.CLIP_EYE_INV, kColor);
		  }
	  }


	  public boolean getGradientMagnitude()
	  {
		  return m_kVolumeRayCast.GetGradientMagnitude();
	  }

	  /**
	   * Enables/Disables Gradient Magnitude filter.
	   * @param bShow gradient magnitude filter on/off
	   */
	  public void setGradientMagnitude(boolean bShow)
	  {
		  if ( m_kVolumeRayCast != null )
		  {
			  m_kVolumeRayCast.SetGradientMagnitude(bShow);
		  }
	  }

	  /**
	   * Sets the ModelImage to use as an alternative to the volume ModelImage for surface texturing.
	   * @param kSurfaceName the surface to modify.
	   * @param kImage the alternate ModelImage to use for the surface texture.
	   */
	  public void setImageNew( String kSurfaceName, ModelImage kImage )
	  {
		  for ( int i = 0; i < m_kDisplayList.size(); i++ )
		  {
			  if ( m_kDisplayList.get(i).GetName() != null )
			  {
				  if ( m_kDisplayList.get(i).GetName().equals(kSurfaceName))
				  {
					  ((VolumeSurface)(m_kDisplayList.get(i))).SetImageNew(kImage);
				  }
			  }
		  }
	  }

	  /**
	   * Set intensity level for GPU-based surface extraction.
	   * @param iLevel
	   */
	  public void setIntenstityLevel( int iLevel )
	  {
		  m_iExtractLevel = iLevel;
	  }

	  /**
	   * Sets the inter-pupillary distance for stereo rendering.
	   * @param fIPD the IPD value.
	   */
	  public void setIPD( float fIPD )
	  {
		  m_fTrnSpeed = fIPD;        
	  }

	  /**
	   * Sets the LUT to use as an alternative to the volume lut for surface texturing.
	   * @param kSurfaceName the surface to modify.
	   * @param kLUT the new LUT.
	   * @param kRGBT the new ModelRGB (for color images).
	   */
	  public void setLUTNew( String kSurfaceName, ModelStorageBase kLUT )
	  {
		  for ( int i = 0; i < m_kDisplayList.size(); i++ )
		  {
			  if ( m_kDisplayList.get(i).GetName() != null )
			  {
				  if ( m_kDisplayList.get(i).GetName().equals(kSurfaceName))
				  {
					  ((VolumeSurface)(m_kDisplayList.get(i))).SetLUTNew(kLUT);
				  }
			  }
		  }
	  }

	  /**
	   * Sets the material for the given surface.
	   * @param kSurfaceName the surface to update.
	   * @param kMaterial the new material.
	   */
	  public void setMaterial( String kSurfaceName, MaterialState kMaterial, boolean bUpdate )
	  {
		  for ( int i = 0; i < m_kDisplayList.size(); i++ )
		  {
			  if ( m_kDisplayList.get(i).GetName() != null )
			  {
				  if ( m_kDisplayList.get(i).GetName().equals(kSurfaceName))
				  {
					  ((VolumeSurface)(m_kDisplayList.get(i))).SetMaterial(kMaterial, bUpdate);
					  m_kVolumeImageA.setSurfaceMaskColor(kSurfaceName, kMaterial.Diffuse);
					  m_bSurfaceMaskUpdate = true;
				  }
			  }
		  }
	  }



	  /**
	   * Enable surface picking for the given surface.
	   * @param kSurfaceName surface name.
	   * @param bOn picking on/off.
	   */
	  public void setPickable(String kSurfaceName, boolean bOn)
	  {
		  for ( int i = 0; i < m_kDisplayList.size(); i++ )
		  {
			  if ( m_kDisplayList.get(i).GetName() != null )
			  {
				  if ( m_kDisplayList.get(i).GetName().equals(kSurfaceName))
				  {
					  m_kDisplayList.get(i).SetPickable( bOn );
				  }
			  }
		  }
	  }
	  /**
	   * Set the polygon mode (FILL, LINE, POINT) for the given surface.
	   * @param kSurfaceName the surface to modify.
	   * @param eMode FILL, LINE, or POINT.
	   */
	  public void setPolygonMode(String kSurfaceName, WireframeState.FillMode eMode)
	  {
		  for ( int i = 0; i < m_kDisplayList.size(); i++ )
		  {
			  if ( m_kDisplayList.get(i).GetName() != null )
			  {
				  if ( m_kDisplayList.get(i).GetName().equals(kSurfaceName))
				  {
					  m_kDisplayList.get(i).SetPolygonMode( true, eMode );
				  }
			  }
		  }
	  }

	  /** Sets the polyline color for the specified fiber bundle tract group. 
	   * @param iGroup the fiber bundle group to set.
	   * @param kColor the new polyline color for the specified fiber bundle tract group. 
	   */
	  public void setPolylineColor( int iGroup, ColorRGB kColor )
	  {
		  if ( m_kDTIDisplay != null )
		  {
			  m_kDTIDisplay.setPolylineColor(iGroup, kColor);
		  }
	  }


	  public void setRGBTA(ModelRGB RGBT) {
		  if ( m_kVolumeRayCast != null )
		  {
			  m_kVolumeRayCast.setRGBTA(RGBT);
		  } 
		  if ( m_kSlices != null )
		  {
			  m_kSlices.setRGBTA(RGBT);
		  }
	  }

	  public void setRGBTB(ModelRGB RGBT) {
		  if ( m_kVolumeRayCast != null )
		  {
			  m_kVolumeRayCast.setRGBTB(RGBT);
		  } 
		  if ( m_kSlices != null )
		  {
			  m_kSlices.setRGBTB(RGBT);
		  }
	  }
	  /**
	   * Set the transparency value for the slice.
	   * @param i the slice to modify.
	   * @param fAlpha the new transparency value.
	   */
	  public void setSliceOpacity( int i, float fAlpha )
	  {
		  if ( m_kSlices != null )
		  {
			  m_kSlices.SetSliceOpacity( i, fAlpha );
		  }
	  }

	  /**
	   * Enable/disable stereo rendering.
	   * @param bEnable
	   */
	  public void setStereo( int iWhich )
	  {
		  m_iStereo = iWhich;
		  if ( iWhich == 1 ) // Red/Green
		  {
			  m_kVolumeRayCast.SetDisplay(false);
		  }
	  }    

	  /**
	   * Turns on surface texture display for the given surface. The user can use a separate ModelImage and LUT than the one displayed in the volume renderer.
	   * @param kSurfaceName the name of the surface to texture.
	   * @param bOn texture on/off.
	   * @param bUseNewImage when false use the current ModelImage, when true the user specifies the model image.
	   * @param bUseNewLUT when false use the current LUT, when true the user specifies the LUT.
	   */
	  public void setSurfaceTexture(String kSurfaceName, boolean bOn, boolean bUseNewImage, boolean bUseNewLUT)
	  {
		  for ( int i = 0; i < m_kDisplayList.size(); i++ )
		  {
			  if ( m_kDisplayList.get(i).GetName() != null )
			  {
				  if ( m_kDisplayList.get(i).GetName().equals(kSurfaceName))
				  {
					  ((VolumeSurface)m_kDisplayList.get(i)).SetSurfaceTexture( bOn, bUseNewImage, bUseNewLUT );
				  }
			  }
		  }
	  }


	  /** Sets the blend factor for displaying the ray-cast volume with other objects in the scene.
	   * @param fBlend the blend factor for the ray-cast volume.
	   */
	  public void setVolumeBlend( float fBlend )
	  {
		  if ( m_kVolumeRayCast != null )
		  {
			  m_kVolumeRayCast.setVolumeBlend(fBlend);
		  }
	  }

	  public void setVolumeColor(boolean flag) {
		  if ( m_kDTIDisplay != null )
		  {
			  m_kDTIDisplay.setVolumeColor(flag);
		  }
	  }

	  /** Set the sampling rate when the mouse is Dragged. */
	  public int setVolumeSamplesMouseDragged( float fSample )
	  {
		  fSample_mouseDragged = fSample;
		  if ( m_kVolumeRayCast != null )
			  return m_kVolumeRayCast.setVolumeSamples( fSample );
		  return -1;
	  }

	  /** Set the sampling rate when the mouse is released. */
	  public int setVolumeSamplesMouseReleased( float fSample )
	  {
		  fSample_mouseReleased = fSample;
		  if ( m_kVolumeRayCast != null )
			  return m_kVolumeRayCast.setVolumeSamples( fSample );
		  return -1;
	  }    

	  /**
	   * Set the mouse translation speed from the translation control slider
	   * @param translationSpeed   mouse translation speed
	   * @param rotationSpeed      mouse rotation speed
	   */
	  public void setMouseTranslationSpeed(float translationSpeed) {
		  UpdateCameraTranslationSpeed(translationSpeed);
		  return;
	  }
	  
	  /**
	   * Set the mouse rotation speed from the rotation control slider
	   * @param rotationSpeed      mouse rotation speed
	   */
	  public void setMouseRotationSpeed(float rotationSpeed) {
		  UpdateCameraRotationSpeed(rotationSpeed);
		  return;
	  }
	  
	  /**
	   * Set the mouse zoom speed from the zoom control slider
	   * @param zoomSpeed      mouse zoom speed
	   */
	  public void setMouseZoomSpeed(float zoomSpeed) {
		  UpdateCameraZoomSpeed(zoomSpeed);
		  return;
	  }
	  
	  
	  /** Turns on/off displaying the bounding box for the given plane.
	   * @param i the plane index (0-3) in file coordinates.
	   * @param bShow when true, the bounding box is displayed.
	   */
	  public void showBoundingBox( int i, boolean bShow )
	  {
		  if ( m_kSlices != null )
		  {
			  m_kSlices.ShowBoundingBox( i, bShow );
		  }
	  }

	  /** Turns on/off displaying the given plane.
	   * @param i the plane index (0-3) in file coordinates.
	   * @param bShow when true, the plane is displayed.
	   */
	  public void showSlice( int i, boolean bShow )
	  {
		  if ( m_kSlices != null )
		  {
			  m_kSlices.ShowSlice( i, bShow );
		  }
	  }


	  /**
	   * Smooth the given surface.
	   * @param kSurfaceName the name of the surface to smooth.
	   * @param iteration smooth iterations.
	   * @param alpha smooth factor.
	   * @param volumeLimit whether to use a volume % change limit.
	   * @param volumePercent the % volume change limiting factor
	   */
	  public void smoothMesh( String kSurfaceName, int iteration, float alpha, boolean volumeLimit, float volumePercent)
	  {
		  for ( int i = 0; i < m_kDisplayList.size(); i++ )
		  {
			  if ( m_kDisplayList.get(i).GetName() != null )
			  {
				  if ( m_kDisplayList.get(i).GetName().equals(kSurfaceName))
				  {
					  ((VolumeSurface)(m_kDisplayList.get(i))).smoothMesh(iteration, alpha, volumeLimit, volumePercent);
				  }
			  }
		  }
	  }

	  /**
	   * Smooth the given surface.
	   * @param kSurfaceName the name of the surface to smooth.
	   * @param iteration smooth iterations.
	   * @param lambda smooth factor.
	   * @param mu smooth factor.
	   */
	  public void smoothThree( String kSurfaceName, int iteration, float lambda, float mu)
	  {
		  for ( int i = 0; i < m_kDisplayList.size(); i++ )
		  {
			  if ( m_kDisplayList.get(i).GetName() != null )
			  {
				  if ( m_kDisplayList.get(i).GetName().equals(kSurfaceName))
				  {
					  ((VolumeSurface)(m_kDisplayList.get(i))).smoothThree(iteration, lambda, mu);
				  }
			  }
		  }
	  }

	  /**
	   * Smooth the given surface.
	   * @param kSurfaceName the name of the surface to smooth.
	   * @param iteration smooth iterations.
	   * @param fStiffness stiffness factor.
	   * @param volumeLimit whether to use a volume % change limit.
	   * @param volumePercent the % volume change limiting factor.
	   */
	  public void smoothTwo( String kSurfaceName, int iteration, float fStiffness, boolean volumeLimit, float volumePercent)
	  {
		  for ( int i = 0; i < m_kDisplayList.size(); i++ )
		  {
			  if ( m_kDisplayList.get(i).GetName() != null )
			  {
				  if ( m_kDisplayList.get(i).GetName().equals(kSurfaceName))
				  {
					  ((VolumeSurface)(m_kDisplayList.get(i))).smoothTwo(iteration, fStiffness, volumeLimit, volumePercent);
				  }
			  }
		  }
	  }


	  /**
	   * Display the volume in Surface mode.
	   */
	  public void SURFASTMode()
	  {
		  if ( m_kVolumeRayCast != null )
		  {
			  m_kVolumeRayCast.SURFASTMode();
		  }
	  }

	  /**
	   * Display the volume in Composite Surface mode.
	   */
	  public void SURMode()
	  {
		  if ( m_kVolumeRayCast != null )
		  {
			  m_kVolumeRayCast.SURMode();
		  }
	  }

	  /**
	   * Switches between different ways of displaying the geodesic path (Euclidean, Geodesic, or Mesh).
	   * @param kSurfaceName the surface the path is on.
	   * @param iWhich the type of display.
	   */
	  public void toggleGeodesicPathDisplay(String kSurfaceName, int iWhich)
	  {
		  for ( int i = 0; i < m_kDisplayList.size(); i++ )
		  {
			  if ( m_kDisplayList.get(i).GetName() != null )
			  {
				  if ( m_kDisplayList.get(i).GetName().equals(kSurfaceName))
				  {
					  ((VolumeSurface)(m_kDisplayList.get(i))).ToggleGeodesicPathDisplay(iWhich);
				  }
			  }
		  }
	  }

	  /**
	   * Changes the translation vector for the surface with the given name.
	   * @param kSurfaceName the surface to move.
	   * @param kTranslate the new translation vector
	   */
	  public void translateSurface(String kSurfaceName, Vector3f kTranslate)
	  {
		  for ( int i = 0; i < m_kDisplayList.size(); i++ )
		  {
			  if ( m_kDisplayList.get(i).GetName() != null )
			  {
				  if ( m_kDisplayList.get(i).GetName().equals(kSurfaceName))
				  {
					  m_kDisplayList.get(i).Translate(kTranslate);
				  }
			  }
		  }
	  }

	  /**
	   * Undo applying the sculpt region to the volume.
	   */
	  public void undoSculpt(boolean bAll)
	  {
		  if ( m_kSculptor == null )
		  {
			  return;
		  }
		  int iTSize = (m_kVolumeImageA.GetImage().is4DImage()&&bAll) ? m_kVolumeImageA.GetImage().getExtents()[3] : 1;
		  for ( int i = 0; i < iTSize; i++ )
		  {
			  if ( m_kVolumeImageA.GetVolumeTarget().GetImage().GetData() != null )
			  {
				  m_kSculptor.setTextureImageDataA( m_kVolumeImageA.GetVolumeTarget().GetImage().GetData() );
			  }
			  if ( m_kVolumeImageB.GetImage() != null )
			  {
				  if ( m_kVolumeImageB.GetVolumeTarget().GetImage().GetData() != null )
				  {
					  m_kSculptor.setTextureImageDataB( m_kVolumeImageB.GetVolumeTarget().GetImage().GetData() );
				  }
			  }
			  m_kSculptor.undoSculpt(m_kVolumeImageA.GetTimeSlice());
			  m_kVolumeImageA.ReleaseVolume();
			  if ( m_kVolumeImageB.GetImage() != null )
			  {
				  m_kVolumeImageB.ReleaseVolume();
			  }
			  if ( iTSize > 1 )
			  {
				  m_kVolumeImageA.update4D(true);
			  }
		  }
	  }

	  /**
	   * Causes the VolumeShader to update the copy of the ModelImage on the
	   * GPU.
	   * @param kImage the new image.
	   */
	  public void updateData()
	  {
		  if ( m_kSculptor != null )
		  {
			  m_kSculptor.setImage(m_kVolumeImageA.GetImage(), m_kVolumeImageB.GetImage());
		  }

	  }


	  public void updateLevWidgetState( Vector<ClassificationWidget> kLWS )
	  {
		  m_kVolumeRayCast.updateLevWidgetState( kLWS );
	  }

	  /**
	   * Called by the init() function. Creates and initialized the scene-graph.
	   * @param arg0 the GLCanvas
	   */
	  private void CopyShared ()
	  {
		  m_kSlices = m_kShared.m_kSlices;
	  }
	  /**
	   * Creates the three render-to-texture targets for rendering semi-transparent surfaces. 
	   * Creates the screen-space polygon and OrderIndTransparencyEffect that display the
	   * final result.
	   * The first render target is for the solid objects.
	   * Second render target is for translucent objects.
	   * Third render target is for accumulating the count of translucent objects per-pixel.
	   * @param arg0
	   * @param iWidth
	   * @param iHeight
	   */
	  protected void CreateRenderTarget( GLAutoDrawable kDrawable, int iWidth, int iHeight )
	  {        
		  if ( (m_akSceneTarget != null) && (m_akSceneTarget[0] != null) )
		  {
			  if ( m_akSceneTarget[0].GetImage() != null )
			  {
				  if ( (m_akSceneTarget[0].GetImage().GetBound(0) == iWidth) && (m_akSceneTarget[0].GetImage().GetBound(1) == iHeight) )
				  {
					  return;
				  }
			  }
		  }
		  
		  if ( m_kFBO != null )
		  {
			  m_kFBO.SetDrawable(kDrawable);
			  m_kFBO.TerminateBuffer();
			  m_kFBO = null;
		  }

		  m_kFBO = new OpenGLFrameBuffer(m_eFormat,m_eDepth,m_eStencil,
				  m_eBuffering,m_eMultisampling,m_pkRenderer, kDrawable);

		  for ( int i = 0; i < m_akSceneTarget.length; i++ )
		  {
			  if ( m_akSceneTarget[i] == null )
			  {
				  m_akSceneTarget[i] = new Texture();
				  m_akSceneTarget[i].SetShared(true);
				  m_akSceneTarget[i].SetFilterType(Texture.FilterType.NEAREST);
				  m_akSceneTarget[i].SetWrapType(0,Texture.WrapType.CLAMP);
				  m_akSceneTarget[i].SetWrapType(1,Texture.WrapType.CLAMP);
			  }
			  else
			  {
				  m_akSceneTarget[i].Release(m_pkRenderer);
			  }
			  GraphicsImage pkSceneImage = new GraphicsImage(GraphicsImage.FormatMode.IT_RGBA32,iWidth,iHeight,(float[])null,
					  "ColorTex" + i );
			  m_akSceneTarget[i].SetImage(pkSceneImage);
			  m_pkRenderer.LoadTexture( m_akSceneTarget[i] );
		  }

		  m_kFBO.InitializeBuffer(m_akSceneTarget);

		  if ( m_pkPlane == null )
		  {
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

			  m_spkPlaneEffect = new OrderIndpTransparencyEffect(m_akSceneTarget);
			  m_pkPlane.AttachEffect( m_spkPlaneEffect );
			  m_pkPlane.UpdateGS();
			  m_pkPlane.UpdateRS();

			  ZBufferState kZBuffer = new ZBufferState();
			  kZBuffer.Enabled = false;
			  kZBuffer.Writable = false;        

			  m_pkPlane.UpdateGS();
			  m_pkPlane.UpdateRS();

			  m_pkRenderer.LoadResources( m_pkPlane );

			  m_kVolumeRayCast.recreateShaderEffect(m_pkRenderer, m_akSceneTarget[3]);
		  }
	  }

	  /**
	   * Called by the init() function. Creates and initialized the scene-graph.
	   */
	  protected void CreateScene ()
	  {
		  // Create a scene graph with the face model as the leaf node.
		  m_spkScene = new Node();
		  m_spkCull = new CullState();
		  m_spkScene.AttachGlobalState(m_spkCull);

		  m_kVolumeRayCast = new VolumeRayCast( m_kVolumeImageA, m_kVolumeImageB );
		  m_kDisplayList.add(0, m_kVolumeRayCast);
		  m_kVolumeRayCast.CreateScene( );

		  m_kTranslate = m_kVolumeRayCast.GetTranslate();

		  m_fX = m_kVolumeImageA.GetScaleX();
		  m_fY = m_kVolumeImageA.GetScaleY();
		  m_fZ = m_kVolumeImageA.GetScaleZ();
		  m_fMax = Math.max( m_fX, Math.max( m_fY, m_fZ ) );

		  if ( m_kSlices == null )
		  {
			  m_kSlices = new VolumeSlices( m_pkRenderer, m_kVolumeImageA, m_kVolumeImageB, m_kTranslate, m_fX, m_fY, m_fZ );
			  displayVolumeSlices( true );
		  }
		  m_kSlices.SetBoundingBoxColor( 0, m_akSlicesColor[0] );
		  m_kSlices.SetBoundingBoxColor( 1, m_akSlicesColor[1] );
		  m_kSlices.SetBoundingBoxColor( 2, m_akSlicesColor[2] );
		  m_kSlices.SetTranslate(m_kTranslate);
		  m_kDisplayList.add(m_kDisplayList.size(), m_kSlices);

		  m_kVolumeClip = new VolumeClip( m_kVolumeImageA, m_kTranslate, m_fX, m_fY, m_fZ );
		  m_kDisplayList.add( m_kDisplayList.size(), m_kVolumeClip);

		  m_kVolumeBox = new VolumeBoundingBox( m_kVolumeImageA, m_kTranslate, m_fX, m_fY, m_fZ );
		  m_kDisplayList.add( m_kDisplayList.size(), m_kVolumeBox);

		  m_kVolumeCube = new VolumeOrientationCube( m_kVolumeImageA, m_kTranslate, m_fX, m_fY, m_fZ );
		  m_kDisplayList.add( m_kDisplayList.size(), m_kVolumeCube);

		  for ( int i = 0; i < m_pkRenderer.GetMaxLights(); i++ )
		  {
			  m_pkRenderer.SetLight( i, new Light() );
		  }
	  }

	  /**
	   * Calculates the rotation for the arbitrary clip plane.
	   * @param bEnable arbitrary clip plane on/off.
	   */
	  protected void doClip( boolean bEnable ) 
	  {           
		  m_bArbClipOn = bEnable;
		  if ( m_kArbitraryClip == null )
		  {
			  m_kArbitraryClip = new Vector4f(1,0,0,0);            
		  }
		  if ( m_kVolumeClip == null )
		  {
			  return;
		  }
		  Vector3f center = m_kVolumeClip.SetArbPlane( m_kArbitraryClip.W );

		  // Rotate normal vector:
		  Matrix3f kClipRotate = m_kVolumeClip.ArbRotate().Local.GetRotate();
		  Vector3f kNormal = new Vector3f( 1,0,0 );
		  kNormal = kClipRotate.mult(kNormal);
		  kNormal.normalize();


		  // Scale kNormal based on the scaled volume:
		  kNormal.set( kNormal.X * m_fX, kNormal.Y * m_fY, kNormal.Z * m_fZ );
		  kNormal.normalize();
		  m_afArbEquation[0] = kNormal.X;
		  m_afArbEquation[1] = kNormal.Y;
		  m_afArbEquation[2] = kNormal.Z;

		  center.add(m_kTranslate);
		  center = kClipRotate.mult(center);
		  center.set( center.X / m_fX, center.Y / m_fY, center.Z / m_fZ );
		  center.add(new Vector3f( .5f, .5f, .5f ));
		  m_afArbEquation[3] = kNormal.dot(center);

		  // Update shader with rotated normal and distance:
		  if ( m_kVolumeRayCast != null )
		  {
			  m_kVolumeRayCast.SetClipArb(m_afArbEquation, bEnable);
		  }
		  for ( int i = 0; i < m_kDisplayList.size(); i++ )
		  {
			  if ( m_kDisplayList.get(i) instanceof VolumeSurface )
			  {
				  ((VolumeSurface)m_kDisplayList.get(i)).SetClipArb(m_afArbEquation, bEnable);
			  }
		  }
	  }

	  public static int getHistogramLUTTextureIndex( String kCommand )
	  {
		  if ( kCommand.equals(CustomUIBuilder.PARAM_LUT_GRAY.getActionCommand()) )
		  {
			  return 0;
		  }
		  if ( kCommand.equals(CustomUIBuilder.PARAM_LUT_RED.getActionCommand()) )
		  {
			  return 1;
		  }
		  if ( kCommand.equals(CustomUIBuilder.PARAM_LUT_GREEN.getActionCommand()) )
		  {
			  return 2;
		  }
		  if ( kCommand.equals(CustomUIBuilder.PARAM_LUT_BLUE.getActionCommand()) )
		  {
			  return 3;
		  }
		  if ( kCommand.equals(CustomUIBuilder.PARAM_LUT_GRAY_BLUE_RED.getActionCommand()) )
		  {
			  return 4;
		  }
		  if ( kCommand.equals(CustomUIBuilder.PARAM_LUT_HOTMETAL.getActionCommand()) )
		  {
			  return 5;
		  }
		  if ( kCommand.equals(CustomUIBuilder.PARAM_LUT_SPECTRUM.getActionCommand()) )
		  {
			  return 6;
		  }
		  if ( kCommand.equals(CustomUIBuilder.PARAM_LUT_COOL_HOT.getActionCommand()) )
		  {
			  return 7;
		  }
		  if ( kCommand.equals(CustomUIBuilder.PARAM_LUT_SKIN.getActionCommand()) )
		  {
			  return 8;
		  }
		  if ( kCommand.equals(CustomUIBuilder.PARAM_LUT_BONE.getActionCommand()) )
		  {
			  return 9;
		  }
		  if ( kCommand.equals(CustomUIBuilder.PARAM_LUT_STRIPED.getActionCommand()) )
		  {
			  return 10;
		  }
		  return 0;
	  }
	  
	  public static Texture getHistogramLUTTexture( int index, boolean bReverse )
	  {
		  index = Math.min(index, ms_akLUTTexture.length-1);
		  index = Math.max(index, 0);
		  if ( bReverse )
		  {
			  int i1, i2;
			  byte temp;
			  byte[] aucData = ms_akLUTTexture[index].GetImage().GetData();
			  for ( int i = 0; i < 128; i++ )
			  {
				  i1 = i;
				  i2 = 256-i-1;
				  for ( int j = 0; j < 4; j++ )
				  {
					  temp = aucData[i1*4+j];
					  aucData[i1*4+j] = aucData[i2*4+j];
					  aucData[i2*4+j] = temp;
				  }				  
			  }		  
			  
			  ms_akLUTTexture[index].Reload(true);
		  }
		  return ms_akLUTTexture[index];
	  }
	  
	  
	  
	  private void initLUT(Texture[] m_akLUTTexture, ModelLUT kLUT, int index )
	  {
		  byte[] aucData = new byte[256 * 4];
		  GraphicsImage kColorMap = new GraphicsImage(GraphicsImage.FormatMode.IT_RGBA8888, 256, aucData, new String("ColorMap" + index));
		  m_akLUTTexture[index].SetImage(kColorMap);
		  m_akLUTTexture[index].SetName(kColorMap.GetName());

		  for (int i = 0, count = 0; i < 256; i++) {
			  int remappedValue = kLUT.getIndexedLUT()[i];
			  aucData[count++] = (byte) ( (remappedValue & 0x00ff0000) >> 16);
			  aucData[count++] = (byte) ( (remappedValue & 0x0000ff00) >> 8);
			  aucData[count++] = (byte) ( (remappedValue & 0x000000ff));
			  aucData[count++] = (byte) ( 255 );
		  }
	  }

	  private void loadShared ()
	  {
		  m_pkRenderer.LoadTexture( m_kVolumeImageA.GetVolumeTarget() );
		  m_pkRenderer.LoadTexture( m_kVolumeImageA.GetColorMapTarget() );
		  if ( m_kVolumeImageA.GetImage().isColorImage() )
		  {
			  m_pkRenderer.LoadTexture( m_kVolumeImageA.GetNormalMapTarget() );
		  }
		  m_pkRenderer.LoadTexture( m_kVolumeImageA.GetGradientMapTarget() );
		  m_pkRenderer.LoadTexture( m_kVolumeImageA.GetOpacityMapGMTarget() );
		  m_pkRenderer.LoadTexture( m_kVolumeImageA.GetSurfaceTarget() );
		  m_pkRenderer.LoadTexture( m_kVolumeImageA.GetScratchTarget() );
		  if ( m_kVolumeImageB.GetImage() != null )
		  {
			  m_pkRenderer.LoadTexture( m_kVolumeImageB.GetVolumeTarget() );
			  m_pkRenderer.LoadTexture( m_kVolumeImageB.GetColorMapTarget() );
			  if ( m_kVolumeImageB.GetImage().isColorImage() )
			  {
				  m_pkRenderer.LoadTexture( m_kVolumeImageB.GetNormalMapTarget() );
			  }
			  m_pkRenderer.LoadTexture( m_kVolumeImageB.GetGradientMapTarget() );
			  m_pkRenderer.LoadTexture( m_kVolumeImageB.GetOpacityMapGMTarget() );
			  m_pkRenderer.LoadTexture( m_kVolumeImageB.GetSurfaceTarget() );   
			  m_pkRenderer.LoadTexture( m_kVolumeImageB.GetScratchTarget() );     	
		  }

		  for ( int i = 0; i < ms_akLUTTexture.length; i++ )
		  {
			  ms_akLUTTexture[i] = new Texture();
			  ms_akLUTTexture[i].SetShared(true);
		  }

		  int index = 0;
		  ModelLUT kLUT = new ModelLUT(ModelLUT.GRAY, 256, new int[]{4,256});		  
		  initLUT(ms_akLUTTexture, kLUT, index++);

		  kLUT.makeRedTransferFunctions(); kLUT.makeLUT(256);
		  initLUT(ms_akLUTTexture, kLUT, index++);

		  kLUT.makeGreenTransferFunctions(); kLUT.makeLUT(256);
		  initLUT(ms_akLUTTexture, kLUT, index++);
		  kLUT.makeBlueTransferFunctions(); kLUT.makeLUT(256);
		  initLUT(ms_akLUTTexture, kLUT, index++);
		  kLUT.makeGrayBRTransferFunctions(); kLUT.makeLUT(256);
		  initLUT(ms_akLUTTexture, kLUT, index++);
		  kLUT.makeHotMetalTransferFunctions(); kLUT.makeLUT(256);
		  initLUT(ms_akLUTTexture, kLUT, index++);
		  kLUT.makeSpectrumTransferFunctions(); kLUT.makeLUT(256);
		  initLUT(ms_akLUTTexture, kLUT, index++);
		  kLUT.makeCoolHotTransferFunctions(); kLUT.makeLUT(256);
		  initLUT(ms_akLUTTexture, kLUT, index++);
		  kLUT.makeSkinTransferFunctions(); kLUT.makeLUT(256);
		  initLUT(ms_akLUTTexture, kLUT, index++);
		  kLUT.makeBoneTransferFunctions(); kLUT.makeLUT(256);
		  initLUT(ms_akLUTTexture, kLUT, index++);
		  kLUT.makeStripedLUT();
		  initLUT(ms_akLUTTexture, kLUT, index++);

		  for ( int i = 0; i < ms_akLUTTexture.length; i++ )
		  {
			  m_pkRenderer.LoadTexture( ms_akLUTTexture[i] );			  
		  }

		  m_fX = m_kVolumeImageA.GetScaleX();
		  m_fY = m_kVolumeImageA.GetScaleY();
		  m_fZ = m_kVolumeImageA.GetScaleZ();
		  //m_kSlices = new VolumeSlices( m_pkRenderer, m_kVolumeImageA, m_kVolumeImageB, m_kTranslate, m_fX, m_fY, m_fZ );
		  //displayVolumeSlices( true );        
	  }

	  /**
	   * Render the display list objects without the raycast volume.
	   */
	  protected void Render(GLAutoDrawable kDraw)
	  {

		  if ( m_iStereo == 0 )
		  { 
			  m_pkRenderer.SetBackgroundColor(m_kBackgroundColor);
			  m_pkRenderer.ClearBuffers();
			  if ( !m_bDisplaySecond )
			  {
				  /*
				  RenderWithTransparency(true);    
				  m_pkRenderer.SetCamera(m_pkScreenCamera);
				  m_pkRenderer.SetBackgroundColor( m_kBackgroundColor );
				  m_pkRenderer.ClearBuffers();
				  m_pkRenderer.Draw( m_pkPlane ); 
				  m_pkRenderer.SetCamera(m_spkCamera); */
				  //RenderWithTransparency(true);            
				  for ( int i = 0; i < m_kDisplayList.size(); i++ )
				  {
				  	  m_kDisplayList.get(i).Render( m_pkRenderer, m_kCuller, true, true );
				  }
				  if ( m_bSnapshot || m_bWriteImage )
				  {
					  writeImage();
					  m_bWriteImage = false;
				  }
				  m_pkRenderer.DisplayBackBuffer();
			  }
			  else
			  {
				  if ( m_kVolumeRayCast.GetDisplay() )
				  {
					  RenderWithTransparency(true);            
				  }
				  RenderWithTransparency(false);
				  if ( m_kVolumeRayCast.GetDisplay() )
				  {
					  m_kVolumeRayCast.Render( m_pkRenderer, m_kCuller, false, true );
					  m_pkRenderer.SetCamera(m_spkCamera);
				  }
				  for ( int i = 0; i < m_kDisplayList.size(); i++ )
				  {
					  m_kDisplayList.get(i).PostRender( m_pkRenderer, m_kCuller );
				  }
				  RenderSculpt();

				  RenderFrameRate(kDraw);                
				  if ( m_bSnapshot || m_bWriteImage )
				  {
					  writeImage();
					  m_bWriteImage = false;
				  }
				  m_pkRenderer.DisplayBackBuffer();
			  }
		  }
		  else
		  {          
			  m_pkRenderer.SetBackgroundColor(m_kBackgroundColor);
			  MoveRight();
			  {
				  m_kCuller.ComputeVisibleSet(m_spkScene);
				  if ( m_iStereo == 1 ) // red/green:
				  {
					  m_pkRenderer.ClearBuffers();
					  m_pkRenderer.SetColorMask( false, false, true, true );
				  }
				  else // shutter:
				  {
					  m_pkRenderer.DrawRight();
					  m_pkRenderer.ClearBuffers();
				  }           
				  if ( m_kVolumeRayCast.GetDisplay() )
				  {
					  RenderWithTransparency(true);            
				  }
				  RenderWithTransparency(false);
				  if ( m_kVolumeRayCast.GetDisplay() )
				  {
					  m_kVolumeRayCast.Render( m_pkRenderer, m_kCuller, false, true );
					  m_pkRenderer.SetCamera(m_spkCamera);
				  }
				  if ( m_iStereo == 1 ) // red/green:
				  {
					  m_pkRenderer.ClearZBuffer();
				  }    
			  }
			  MoveLeft();
			  MoveLeft();
			  m_kCuller.ComputeVisibleSet(m_spkScene);  
			  if ( m_iStereo == 1 ) // red/green:
			  {                
				  m_pkRenderer.SetColorMask( true, false, false, true );
			  }
			  else
			  {
				  m_pkRenderer.DrawLeft();
				  m_pkRenderer.ClearBuffers();
			  }
			  if ( m_kVolumeRayCast.GetDisplay() )
			  {
				  RenderWithTransparency(true);            
			  }
			  RenderWithTransparency(false);
			  if ( m_kVolumeRayCast.GetDisplay() )
			  {
				  m_kVolumeRayCast.Render( m_pkRenderer, m_kCuller, false, true );
				  m_pkRenderer.SetCamera(m_spkCamera);
			  }

			  m_pkRenderer.DrawDefault();
			  RenderFrameRate(kDraw);                
			  if ( m_bSnapshot )
			  {
				  writeImage();
			  }
			  m_pkRenderer.DisplayBackBuffer();

			  MoveRight();
			  m_pkRenderer.SetColorMask( true, true, true, true );
			  //m_pkRenderer.DrawDefault();
		  }
	  }

	  /**
	   * Render the sculpt image.
	   */
	  private void RenderSculpt()
	  {
		  if ( (m_kSculptor != null) && m_kSculptor.IsSculptDrawn() )
		  {
			  m_pkRenderer.Draw( m_kSculptor.getSculptImage() );
		  }
	  }


	  /**
	   * 
	   * Rendering multiple translucent surfaces is accomplished with the following steps:
	   *   1. A FrameBuffer Object (FBO) is created with three render-to-texture targets, and a depth buffer.
	   *   2. The first render target is activated. Solid objects are rendered: writing color and depth
	   *      information to the FBO target1. The color information displays the solid objects, the depth
	   *      information enables the solid objects to properly occlude the translucent objects.
	   *   3. The second and third render targets are activated. Translucent objects are rendered: writing
	   *      accumulated color and alpha to the second target and the number of overlapping surfaces per
	   *      pixel to the third target. In this pass the depth buffer is read-only.
	   *   4. Rendering to the three texture targets is disabled.
	   *   5. A screen-space polygon is rendered, it reads the three textures generated in the previous pass.
	   *      The first texture contains the color from the solid objects, the next two textures contain the
	   *      color information and weighted-average for rendering the translucent textures.
	   *
	   * @param bPreRender when true rendering into the VolumeRayCast back-image texture for compositing with 
	   * the VolumeRayCast rendering.
	   */
	  private void RenderWithTransparency(boolean bPreRender)
	  {
		  if ( bPreRender )
		  {


			 // m_pkRenderer.SetCamera(m_pkScreenCamera);

			  m_kFBO.Enable();
			  m_kFBO.DrawBuffers(new int[]{3});
				  
			  m_pkRenderer.SetBackgroundColor( m_kBackgroundColor );
			  m_pkRenderer.ClearBuffers();

			  for ( int i = 0; i < m_kDisplayList.size(); i++ )
			  {
			  	  m_kDisplayList.get(i).Render( m_pkRenderer, m_kCuller, true, true );
			  }
			  
			   m_kFBO.Disable(); 

			 // m_pkRenderer.SetCamera(m_spkCamera);
			  
			  return;
		  }
		  
		  
		  // First: render opaque objects to an off-screen color/depth buffer
		  m_kFBO.Enable();
		  m_kFBO.DrawBuffers(new int[]{0});
		  m_pkRenderer.SetBackgroundColor( m_kBackgroundColor );
		  m_pkRenderer.ClearBuffers();

		  if ( bPreRender )
		  {
			  m_kVolumeRayCast.Render( m_pkRenderer, m_kCuller, bPreRender, true );
		  }
		  for ( int i = 1; i < m_kDisplayList.size(); i++ )
		  {
			  m_kDisplayList.get(i).Render( m_pkRenderer, m_kCuller, bPreRender, true );
		  }
		  m_kFBO.Disable();

		  // Second: render semi-transparent objects:
		  m_kFBO.Enable();
		  m_kFBO.DrawBuffers(new int[]{1,2});
		  m_pkRenderer.SetBackgroundColor( new ColorRGBA(0.0f, 0.0f,0.0f,0.0f));
		  m_pkRenderer.ClearBackBuffer();

		  for ( int i = 1; i < m_kDisplayList.size(); i++ )
		  {
			  m_kDisplayList.get(i).Render( m_pkRenderer, m_kCuller, bPreRender, false );
		  }

		  m_kFBO.Disable();        

		  m_pkRenderer.SetCamera(m_pkScreenCamera);

		  if ( bPreRender )
		  {
			  m_kFBO.Enable();
			  m_kFBO.DrawBuffers(new int[]{3});
		  }
		  m_pkRenderer.SetBackgroundColor( m_kBackgroundColor );
		  m_pkRenderer.ClearBuffers();
		  m_pkRenderer.Draw( m_pkPlane ); 
		  if ( bPreRender )
		  { 
			  m_kFBO.Disable();  
		  }

		  m_pkRenderer.SetCamera(m_spkCamera);
	  }




	  protected void update4D( boolean bForward )
	  {
		  m_kVolumeImageA.update4D(bForward);
		  //m_kParent.setModified();
	  }



	  protected void updateCenterOnDisplay() {
		  m_bUpdateCenterOnDisplay = false;
		  if ( m_kSlices != null )
		  {
			  int[] aiExtents = m_kVolumeImageA.GetImage().getExtents();
			  m_kSlices.SetCenter( new Vector3f( (m_kNewCenter.X / (aiExtents[0] -1)),
					  (m_kNewCenter.Y / (aiExtents[1] -1)),
					  (m_kNewCenter.Z / (aiExtents[2] -1))  ) );

		  }
	  }


	  protected void updateVOIs( VOIVector kVOIs )
	  {
		  boolean bUpdateVOIs = false;
		  for ( int i = 0; i < m_kDisplayList.size(); i++ )
		  {
			  if ( m_kDisplayList.get(i) instanceof VolumeVOI )
			  {
				  m_kDisplayList.remove(i);
				  i--;
			  }
		  }
		  for ( int i = 0; i < kVOIs.size(); i++ )
		  {
			  VOI kVOI = kVOIs.get(i);
			  Vector<VOIBase> kCurves = kVOI.getCurves();
			  for ( int k = 0; k < kCurves.size(); k++ )
			  {
				  VOIBase kVOI3D = kCurves.get(k);
				  bUpdateVOIs |= drawVOI( kVOI3D, this, m_kVolumeImageA, m_kTranslate );
			  }
		  } 
		  if ( bUpdateVOIs )
		  {
			  UpdateSceneRotation();
			  //m_kParent.setModified();
		  }
	  }




	  /**
	   * Picking. If a display list object has picking enabled, find the picked polygon based on the mouse position. 
	   */
	   protected void Pick()
	  {
		  Vector3f kPos = new Vector3f(0,0,10);
		  Vector3f kDir = new Vector3f(0,0,1);  // the pick ray

		  if (m_bPickPending)
		  {
			  if (m_spkCamera.GetPickRay(m_iXPick,m_iYPick,GetWidth(),
					  GetHeight(),kPos,kDir))
			  {
				  m_bPickPending = false;
				  for ( int i = 0; i < m_kDisplayList.size(); i++ )
				  {
					  if ( m_kDisplayList.get(i).GetPickable() )
					  {
						  m_kPicker.Execute(m_kDisplayList.get(i).GetScene(),kPos,kDir,0.0f,
								  Float.MAX_VALUE);
						  if (m_kPicker.Records.size() > 0)
						  {
							  //System.err.println( kPos.X() + " " + kPos.Y() + " " + kPos.Z() );
							  //System.err.println( kDir.X() + " " + kDir.Y() + " " + kDir.Z() );
							  if ( m_bPaintEnabled )
							  {
								  //System.err.println("Picked " + m_kDisplayList.get(i).getClass().getName());
								  if ( m_bPaint )
								  {
									  m_kDisplayList.get(i).Paint( m_pkRenderer, m_kPicker.GetClosestNonnegative(), m_kPaintColor, m_iBrushSize );
								  }
								  else if ( m_bDropper || m_bPaintCan )
								  {
									  ColorRGBA kDropperColor = new ColorRGBA();
									  Vector3f kPickPoint = new Vector3f();
									  m_kDisplayList.get(i).Dropper( m_kPicker.GetClosestNonnegative(), kDropperColor, kPickPoint );
									  //m_kParent.setDropperColor( kDropperColor, kPickPoint );
								  } 
								  else if ( m_bErase )
								  {
									  m_kDisplayList.get(i).Erase( m_pkRenderer, m_kPicker.GetClosestNonnegative(), m_iBrushSize );
								  }
							  }
							  if ( m_bGeodesicEnabled )
							  {
								  //m_kParent.setGeodesic( m_kDisplayList.get(i).GetMesh(), m_kPicker.GetClosestNonnegative() );
							  }
							  if ( m_bPickCorrespondence )
							  {
								  PickRecord kRecord = m_kPicker.GetClosestNonnegative();
								  //m_kParent.PickCorrespondence( kRecord.iV0, kRecord.iV1, kRecord.iV2 );
							  }
						  }
					  }
				  }
			  }
		  }
	  }


	   @Override
	   protected void UpdateSceneRotation()
	   {
		   super.UpdateSceneRotation();
		   m_kSceneToWorld.copy(m_kVolumeRayCast.GetWorld());
	   }
	   
		void NormalKernel(  float[] a, float[] b, int width, int height, int depth, int color, int numElements) {

			for ( int iGID = 0; iGID < a.length; iGID++ )
			{
				// bound check, equivalent to the limit on a 'for' loop
				if (iGID >= numElements)  {
					break;
				}
				int indexTemp = iGID;
				int x = indexTemp % width;
				indexTemp -= x;
				indexTemp /= width;
				int y = indexTemp % height;
				indexTemp -= y;
				indexTemp /= height;
				int z = indexTemp;
				int sliceSize = width*height;
				int[] index = new int[14];
				index[0] = z * sliceSize + (y-1)*width + (x-1);
				index[2] = z * sliceSize + y*width + (x-1);
				index[3] = z * sliceSize + y*width + (x+1);
				index[4] = z * sliceSize + (y+1)*width + (x-1);
				index[5] = z * sliceSize + (y+1)*width + (x+1);
				index[6] = z * sliceSize + (y+1)*width + x;
				index[7] = z * sliceSize + (y-1)*width + x;
				index[8] = (z-1) * sliceSize + y*width + (x-1);
				index[9] = (z+1) * sliceSize + y*width + (x-1);
				index[10] = (z-1) * sliceSize + y*width + x;
				index[11] = (z+1) * sliceSize + y*width + x;
				index[12] = (z-1) * sliceSize + y*width + (x+1);
				index[13] = (z+1) * sliceSize + y*width + (x+1);
				// The default:
				float val = a[iGID];
				float[] values = new float[14];
				for ( int i = 0; i < 14; i++ ) {
					values[i] = val;
					if ( (index[i] < numElements) && (index[i] >= 0)  ) {
						values[i] = a[index[i]];
					}
				}
				float fDX = (float)(0.71f * (values[0] - values[1]) + (values[2] - values[3]) + .71 * (values[4] - values[5]));
				float fDY = (float)(0.71f * (values[0] - values[4]) + (values[6] - values[7]) + .71 * (values[1] - values[5]));
				float fDZ = (float)(0.71f * (values[8] - values[9]) + (values[10] - values[11]) + .71 * (values[12] - values[13]));
				float length = (float)Math.sqrt( fDX*fDX + fDY*fDY + fDZ*fDZ);
				// add the vector elements
				b[iGID*4 + 0] = 1;
				b[iGID*4 + 1] = fDX/length;
				b[iGID*4 + 2] = fDY/length;
				b[iGID*4 + 3] = fDZ/length;
			}
		}
		
		private class JPanelGPU extends JPanel
		{
			public JPanelGPU(BorderLayout borderLayout) {
				super(borderLayout);
			}

			public void minMax() {
				firePropertyChange( "min-max", null, container.getParent() );
			}
		}
		
		/**
		 * Creates the containing JPanel that will display this object.
		 * The JPanel contains a scroll pane with both horizontal and vertical scroll bars 
		 * and a horizontal slider for changing where the slice plane intersects the volume
		 * in depth.
		 * @param imageComponent
		 */
		private void createContainer( GLCanvas canvas ) 
		{
			container = new JPanelGPU(new BorderLayout());
			container.add(canvas, BorderLayout.CENTER );
			
			JPanel sliderPanel = new JPanel( new BorderLayout() );
			opacitySlider = new JSlider( SwingConstants.HORIZONTAL, 0, 100, 100 );
			opacityLabel = new JLabel( new Float( 1 ).toString() );
			sliderPanel.add( new JLabel( "[mm]" ), BorderLayout.WEST );
			sliderPanel.add( opacitySlider, BorderLayout.CENTER );
			sliderPanel.add( opacityLabel, BorderLayout.EAST );
			container.add(sliderPanel, BorderLayout.SOUTH);
			opacitySlider.addChangeListener(this);
		}
		
		/** Opacity sliders for when this renderer is displayed as a stand-alone app. */
		private JSlider opacitySlider;
		private JLabel opacityLabel;

		/* (non-Javadoc)
		 * @see javax.swing.event.ChangeListener#stateChanged(javax.swing.event.ChangeEvent)
		 */
		public void stateChanged(ChangeEvent arg0) {
			if ( arg0.getSource() == opacitySlider )
			{
				setVolumeBlend( opacitySlider.getValue() / 100f );
				opacityLabel.setText( new Float( opacitySlider.getValue() / 100f ).toString() );
			}
		}

}
