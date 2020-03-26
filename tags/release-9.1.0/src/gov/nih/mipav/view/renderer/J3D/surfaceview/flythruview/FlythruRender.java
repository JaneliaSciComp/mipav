package gov.nih.mipav.view.renderer.J3D.surfaceview.flythruview;


import WildMagic.LibFoundation.Curves.*;
import gov.nih.mipav.model.file.*;
import gov.nih.mipav.model.structures.*;

import gov.nih.mipav.view.*;
import gov.nih.mipav.view.renderer.MouseEventVector;
import gov.nih.mipav.view.renderer.flythroughview.*;
import gov.nih.mipav.view.renderer.J3D.*;
import gov.nih.mipav.view.renderer.J3D.surfaceview.*;
import gov.nih.mipav.view.renderer.J3D.model.structures.*;

import com.sun.j3d.utils.behaviors.mouse.*;
import com.sun.j3d.utils.geometry.*;
import com.sun.j3d.utils.picking.*;
import com.sun.j3d.utils.universe.*;

import java.awt.*;
import java.awt.event.*;
import java.awt.image.*;

import java.io.*;



import javax.imageio.*;


import javax.media.j3d.*;

import javax.swing.*;

import javax.vecmath.*;


/**
 * Virtual colonoscopy viewer. This class contains the static 'main' method to launch the application. This class is an
 * Applet which means that it can be embedded in a browser or it can be attached to a MainFrame when executed as an
 * application. The Applet contains a Canvas3D used for the 3D rendering of the colon surface data and for using the
 * mouse and keyboard for maneuvering the view of the colon while moving through it.
 */
public class FlythruRender extends SurfaceRender
        implements FlyThroughRenderInterface, FlyPathBehavior.Callback, MouseBehaviorCallback, 
        MouseListener, MouseMotionListener, MouseWheelListener {

    //~ Static fields/initializers -------------------------------------------------------------------------------------

    /** Use serialVersionUID for interoperability. */
    private static final long serialVersionUID = -3255657270255875848L;

    //~ Instance fields ------------------------------------------------------------------------------------------------

    /** DOCUMENT ME! */
    short[] buffer = null;

    /** DOCUMENT ME! */
    int capScreenWidth, capScreenHeight;

    /** DOCUMENT ME! */
    String directory = null;

    /** DOCUMENT ME! */
    int[] extents = new int[3];

    /** DOCUMENT ME! */
    String fileType = "jpg";

    /** DOCUMENT ME! */
    JpegImagesToMovie imageToMovie;

    /** DOCUMENT ME! */
    MovieMaker movieMake;

    /** DOCUMENT ME! */
    int[] pixels;

    /** A local reference to the volume renderer frame's progress bar. */
    JProgressBar rendererProgressBar;

    /** DOCUMENT ME! */
    Robot robot;

    /** DOCUMENT ME! */
    private int imageCounter = 0;

    /** DOCUMENT ME! */
    private VolumeCanvas3D kCanvas;

    /** DOCUMENT ME! */
    private ModelImage kImage;

    /** DOCUMENT ME! */
    private int[] m_aiBranchIndexUnvisitedMax = null;

    /** This is the range of path samples for each branch that are unvisited. */
    private int[] m_aiBranchIndexUnvisitedMin = null;

    /** DOCUMENT ME! */
    private int[] m_aiMeshConnectivity = null;

    /** Keep track of the last branch that was selected (at a branch point). */
    private int m_iLastSelectedBranchIndex = -1;

    /** This is for the collection of annotation points. */
    private FlyPathAnnotateList m_kAnnotateList = null;

    /** DOCUMENT ME! */
    private Group m_kAnnotatePointGroup = null;

    /** DOCUMENT ME! */
    private Shape3D m_kBranchConnectShape = null;

    /** This is the Java3D geometry for the path. */
    private Shape3D m_kBranchPathShape = null;

    /** This is the control frame which may need to be updated as the view changes. */
    private JPanelVirtualEndoscopySetup m_kControlFrame = null;

    /** Keep track of the FlyPathBehavior because it knows about the view from the path. */
    private FlyPathBehavior m_kFlyPathBehavior = null;

    /** DOCUMENT ME! */
    private FlyPathGraphCurve m_kFlyPathGraphCurve = null;

    /** This is the path extracted from the skeletonization. */
    private FlyPathGraphSamples m_kFlyPathGraphSamples = null;

    /** DOCUMENT ME! */
    private Group m_kGeodesicGroup = null;

    /** DOCUMENT ME! */
    private ModelLUT m_kMeanCurvaturesLUT = null;

    /** DOCUMENT ME! */
    private ModelTriangleMeshCurvatures m_kMeshCurvatures = null;

    /** DOCUMENT ME! */
    private MouseRotate m_kMouseRotateBehavior = null;

    /** DOCUMENT ME! */
    private Color3f m_kNormalColorPathUnvisited = null;

    /** These are the colors to use when rendering the normal/selected branch that is unvisited/visited. */
    private Color3f m_kNormalColorPathVisited = null;

    /**
     * These are the setup options passed to the constructor which we need to keep track of in case we need to use them
     * outside the constructor.
     */
    private SetupOptions m_kOptions = null;

    /** Used for picking. */
    private PickCanvas m_kPickCanvas;

    /** DOCUMENT ME! */
    private PointLight m_kPointLight = null;

    /** These are part of the scene and view. */
    private BranchGroup m_kSceneRoot = null;

    /** DOCUMENT ME! */
    private Color3f m_kSelectColorPathUnvisited = null;

    /** DOCUMENT ME! */
    private Color3f m_kSelectColorPathVisited = null;

    /** This it skeletonization of the binary volume. */
    private Skeleton3D m_kSkeleton = null;

    /** This is the surface geometry and its properties. */
    private ModelTriangleMesh m_kSurface = null;

    /** DOCUMENT ME! */
    private ModelTriangleMeshCurveSegments m_kSurfaceCurveSegments = null;

    /** DOCUMENT ME! */
    private IndexedGeometryArray m_kSurfaceGeometry = null;

    /** This is the Java3D geometry for the surface. */
    private GeometryInfo m_kSurfaceGeometryInfo = null;

    /** DOCUMENT ME! */
    private ModelTriangleMesh m_kSurfaceMesh = null;

    /** DOCUMENT ME! */
    private Shape3D m_kSurfaceShape = null;

    /**
     * The simple universe contains a single canvas, a viewing platform, and a scene graph. The scene graph will contain
     * all objects involved in the rendering.
     */
    private SimpleUniverse m_kUniverse;

    /** Describes the organization of the volume data in a linear array. */
    private ModelImage3DLayout m_kVolumeLayout = null;

    /** DOCUMENT ME! */
    private ViewJFrameVolumeView parentFrame;

    /** DOCUMENT ME! */
    private int saveCounter = 0;


    /** Current mouse press event time stamp. */
    long currEventTime;
    /** Previous mouse press event time stamp. */
    long prevEventTime;
    /** If any of the mouse move button pressed. */
    private boolean pressed;
    /** Time to wait for the next mouse event. */
    private long time = 10;
  
    /* camera viewing direction from the right mouse drag control. */
    public static int lookup = 0;
    public static int lookdown = 1;
    public static int lookright = 2;
    public static int lookleft = 3;
    
    
    //~ Constructors ---------------------------------------------------------------------------------------------------

    /**
     * View which renders virtual endoscopy fly through of a binarized volume.
     *
     * @param  _kImage       ModelImage Contains the image data and its properties.
     * @param  _config       Graphics configuration reference.
     * @param  _parentFrame  ViewJFrameVolumeView reference. initial virtual endoscopy view.
     */
    public FlythruRender(ModelImage _kImage, GraphicsConfiguration _config, ViewJFrameVolumeView _parentFrame) {
        super(_kImage, null, _config);
        parentFrame = _parentFrame;

        rendererProgressBar = ViewJFrameVolumeView.getRendererProgressBar();
    }

    //~ Methods --------------------------------------------------------------------------------------------------------

    /**
     * ReplaceMesh is used by the Geodesic when a mesh is cut. The original mesh is changed, but not deleted and no new
     * mesh is added. The orginal mesh, kOld, is replced by the new mesh, kNew:
     *
     * @param  kOld  DOCUMENT ME!
     * @param  kNew  DOCUMENT ME!
     */
    public void addMesh(ModelTriangleMesh kOld, ModelTriangleMesh kNew) {
        boolean bMeshFound = false;

        /* Find the old mesh, kOld, and remove it from the scene graph, adding
         * the new mesh, kNew, in it's place: */
        for (int iChild = 0; iChild < m_kSceneRoot.numChildren(); iChild++) {

            if (m_kSceneRoot.getChild(iChild) instanceof BranchGroup) {
                BranchGroup kBG = (BranchGroup) (m_kSceneRoot.getChild(iChild));

                if (kBG.getChild(0) instanceof Shape3D) {
                    Shape3D kShapeMesh = (Shape3D) (kBG.getChild(0));

                    if (kShapeMesh.getGeometry() instanceof ModelTriangleMesh) {
                        ModelTriangleMesh kChildMesh = (ModelTriangleMesh) (kShapeMesh.getGeometry());

                        if (kChildMesh == kOld) {
                            bMeshFound = true;

                            break;
                        }
                    }
                }
            }
        }

        if (bMeshFound) {
            Appearance kSurfaceAppearance = new Appearance();
            PolygonAttributes kSurfacePolygonAttributes = new PolygonAttributes();

            kSurfacePolygonAttributes.setCullFace(PolygonAttributes.CULL_NONE);
            kSurfacePolygonAttributes.setBackFaceNormalFlip(true);
            kSurfacePolygonAttributes.setPolygonMode(PolygonAttributes.POLYGON_FILL);
            kSurfacePolygonAttributes.setPolygonOffsetFactor(1.0f);
            kSurfacePolygonAttributes.setCapability(PolygonAttributes.ALLOW_MODE_READ);
            kSurfacePolygonAttributes.setCapability(PolygonAttributes.ALLOW_MODE_WRITE);

            kSurfaceAppearance.setPolygonAttributes(kSurfacePolygonAttributes);
            kSurfaceAppearance.setCapability(Appearance.ALLOW_POLYGON_ATTRIBUTES_READ);
            kSurfaceAppearance.setCapability(Appearance.ALLOW_POLYGON_ATTRIBUTES_WRITE);

            Material kSurfaceMaterial = new Material();

            kSurfaceMaterial.setLightingEnable(true);
            kSurfaceMaterial.setShininess(1.0f); // not shiny

            // kSurfaceMaterial.setDiffuseColor(0.0f, 1.0f, 0.0f);
            kSurfaceMaterial.setSpecularColor(0.0f, 0.0f, 0.0f);
            kSurfaceMaterial.setAmbientColor(0.0f, 0.0f, 0.0f);
            kSurfaceAppearance.setMaterial(kSurfaceMaterial);
            m_kSurfaceShape = new Shape3D();
            m_kSurfaceShape.setAppearance(kSurfaceAppearance);
            m_kSurfaceShape.setCapability(Shape3D.ALLOW_BOUNDS_READ);
            m_kSurfaceShape.setCapability(Shape3D.ALLOW_GEOMETRY_WRITE);
            m_kSurfaceShape.setCapability(Shape3D.ALLOW_APPEARANCE_READ);
            m_kSurfaceShape.setCapability(Shape3D.ALLOW_APPEARANCE_WRITE);

            BranchGroup kSurfaceBG = new BranchGroup();

            kSurfaceBG.setCapability(Group.ALLOW_CHILDREN_EXTEND);
            kSurfaceBG.setCapability(Group.ALLOW_CHILDREN_READ);
            kSurfaceBG.setCapability(Group.ALLOW_CHILDREN_WRITE);
            kSurfaceBG.addChild(m_kSurfaceShape);

            m_kSurfaceShape.setGeometry(m_kSurfaceMesh);
            m_kSceneRoot.addChild(kSurfaceBG);
        }
    }

    /* (non-Javadoc)
     * @see gov.nih.mipav.view.renderer.flythroughview.FlyThroughRenderInterface#autoRun()
     */
    public void autoRun() {
        m_kFlyPathBehavior.autoRun();
    }

    /**
     * Override called when window is closed.
     */
    public void dispose() {

        if (null != m_kControlFrame) {
            m_kControlFrame.disposeLocal();
        }

        m_kControlFrame = null;

        if (null != m_kSkeleton) {
            m_kSkeleton.dispose();
        }

        m_kSkeleton = null;

        m_kOptions = null;
        m_kVolumeLayout = null;

        if (m_kUniverse != null) {
            m_kUniverse.removeAllLocales();
            m_kUniverse = null;
        }

        m_kSceneRoot = null;
        m_kPointLight = null;
        m_kFlyPathBehavior = null;
        m_kMouseRotateBehavior = null;
        m_kFlyPathGraphSamples = null;
        m_kFlyPathGraphCurve = null;
        m_kSurfaceGeometry = null;
        m_kSurfaceGeometryInfo = null;
        m_kSurfaceShape = null;
        m_kBranchPathShape = null;
        m_kBranchConnectShape = null;
        m_kNormalColorPathVisited = null;
        m_kNormalColorPathUnvisited = null;
        m_kSelectColorPathVisited = null;
        m_kSelectColorPathUnvisited = null;
        m_aiBranchIndexUnvisitedMin = null;
        m_aiBranchIndexUnvisitedMax = null;
        m_kAnnotateList = null;
        m_kAnnotatePointGroup = null;
        m_kGeodesicGroup = null;

        m_kSurface = null;
        m_kMeshCurvatures = null;
        m_kSurfaceCurveSegments = null;
        m_kMeanCurvaturesLUT = null;
        m_aiMeshConnectivity = null;
        kImage = null;
        super.disposeLocal(false);
    }

    /**
     * For displaying the Path in the surface renderer:
     *
     * @return  Shape3D, the scaled FlyPath
     */
    public Shape3D getBranchPathShape() {
        Shape3D kBranchPathShape = new Shape3D();

        kBranchPathShape.setCapability(Shape3D.ALLOW_GEOMETRY_READ);
        kBranchPathShape.setCapability(Shape3D.ALLOW_GEOMETRY_WRITE);
        kBranchPathShape.setCapability(Shape3D.ALLOW_APPEARANCE_READ);
        kBranchPathShape.setCapability(Shape3D.ALLOW_APPEARANCE_WRITE);

        Color3f black = new Color3f(Color.black);
        Color3f kColor = new Color3f(0, 1, 0);
        Material kMaterial = new Material(black, new Color3f(kColor.x, kColor.y, kColor.z), black, black, 80f);

        kMaterial.setDiffuseColor(kColor.x, kColor.y, kColor.z);
        kMaterial.setSpecularColor(kColor.x, kColor.y, kColor.z);
        kMaterial.setAmbientColor(kColor.x, kColor.y, kColor.z);
        kMaterial.setEmissiveColor(kColor.x, kColor.y, kColor.z);
        kMaterial.setCapability(Material.ALLOW_COMPONENT_WRITE);

        Appearance kAppearance = new Appearance();

        kAppearance.setMaterial(kMaterial);

        kBranchPathShape.setAppearance(kAppearance);

        LineArray kLine = new LineArray(2, GeometryArray.COORDINATES | GeometryArray.COLOR_3);

        kLine.setCoordinate(0, new Point3d(0, 0, 0));
        kLine.setColor(0, kColor);
        kLine.setCoordinate(1, new Point3d(0, 0, 100));
        kLine.setColor(1, kColor);

        int iNumBranches = m_kFlyPathGraphCurve.getNumBranches();

        for (int iBranch = 0; iBranch < iNumBranches; iBranch++) {
            kBranchPathShape.addGeometry(createBranchPathGeometryScaled(iBranch));
        }

        return (Shape3D) kBranchPathShape;
    }


    /* (non-Javadoc)
     * @see gov.nih.mipav.view.renderer.flythroughview.FlyThroughRenderInterface#getBranchState()
     */
    public Object getBranchState() {
        return m_kFlyPathBehavior.getBranchState();
    }


    /* (non-Javadoc)
     * @see gov.nih.mipav.view.renderer.J3D.RenderViewBase#getCanvas()
     */
    public Canvas3D getCanvas() {
        return kCanvas;
    }

    /**
     * Return access to the Group data member m_kGeodesicGroup so Geodesic object can draw on the triangle mesh:
     *
     * @return  DOCUMENT ME!
     */
    public Group getGeodesicGroup() {
        return m_kGeodesicGroup;
    }

    /**
     * Access the pseudocolor mapping specified to use when rendering the computed mean curvatures values for the
     * specified surface.
     *
     * @return  ModelLUT Defines the mapping of mean curvatures values to pseudocolor values.
     */
    public ModelLUT getMeanCurvaturesLUT() {
        return m_kMeanCurvaturesLUT;
    }

    /**
     * DOCUMENT ME!
     *
     * @return  DOCUMENT ME!
     */
    public float getPathDist() {
        return m_kFlyPathBehavior.getPathDist();
    }

    /**
     * Get the pick canvas.
     *
     * @return  pickCanvas pick canvas.
     */
    public PickCanvas getPickCanvas() {
        return m_kPickCanvas;
    }

    /**
     * Scaled coordinates for the current position along the path for viewing.
     *
     * @return  Point3f A new instance created which contains the path position coordinates, scaled to match the
     *          ModelTriangleMesh in JPanelSurface.
     */
    public Point3f getPositionScaled() {
        Point3f kPoint = m_kFlyPathBehavior.getPathPosition();

        int[] aiExtents = kImage.getExtents();
        float[] afResolutions = kImage.getFileInfo(0).getResolutions();
        float[] afOrigins = kImage.getFileInfo(0).getOrigin();
        int[] aiDirections = kImage.getFileInfo(0).getAxisDirection();

        int xDim = aiExtents[0];
        int yDim = aiExtents[1];
        int zDim = aiExtents[2];

        float xBox = (xDim - 1) * afResolutions[0];
        float yBox = (yDim - 1) * afResolutions[1];
        float zBox = (zDim - 1) * afResolutions[2];
        float maxBox = Math.max(xBox, Math.max(yBox, zBox));

        kPoint.x = ((2.0f * (kPoint.x - afOrigins[0]) / aiDirections[0]) - ((xDim - 1) * afResolutions[0])) / maxBox;
        kPoint.y = ((2.0f * (kPoint.y - afOrigins[1]) / aiDirections[1]) - ((yDim - 1) * afResolutions[1])) / maxBox;
        kPoint.z = ((2.0f * (kPoint.z - afOrigins[2]) / aiDirections[2]) - ((zDim - 1) * afResolutions[2])) / maxBox;

        return kPoint;
    }

    /**
     * Return the interpolated sample coordinates in the volume of the current position along the path for viewing.
     *
     * @return  Point3f A new instance created which contains the interpolated sample coordinates.
     */
    public Point3f getSamplePosition() {
        Point3f kPos = m_kFlyPathBehavior.getPathPosition();
        WildMagic.LibFoundation.Mathematics.Vector3f kPosV = m_kVolumeLayout.getSamplePoint( kPos.x, kPos.y, kPos.z );
        return new Point3f( kPosV.X, kPosV.Y, kPosV.Z );
    }

    /**
     * Access the surface to be rendered when moving along the path.
     *
     * @return  ModelTriangleMesh Instance of the surface represented as a triangle mesh as specified in the setSurface
     *          method.
     */
    public ModelTriangleMesh getSurface() {
        return m_kSurface;
    }

    /**
     * DOCUMENT ME!
     *
     * @return  DOCUMENT ME!
     */
    public JPanel getSurfaceControl() {
        return m_kControlFrame.getMainPanel();
    }

    /**
     * Access the computed curvatures for the specified triangle mesh surface.
     *
     * @return  ModelTriangleMeshCurvatures Instance of the computed curvatures associated with the triangle mesh
     *          specifed in the setSurface method.
     */
    public ModelTriangleMeshCurvatures getSurfaceCurvatures() {
        return m_kMeshCurvatures;
    }


    /* (non-Javadoc)
     * @see gov.nih.mipav.view.renderer.flythroughview.FlyThroughRenderInterface#makeMove(java.lang.String)
     */
    public void makeMove(String cmd) {
    	
        m_kFlyPathBehavior.move(cmd);
    }

    /**
     * Implement MouseInterface abstract method.
     *
     * @param  kMouseEvent  MouseEvent Contains information about the particular mouse event.
     */
    public void mouseClicked(MouseEvent kMouseEvent) {

        // Pick event if the mouse is clicked with the shift key pressed.
        if (kMouseEvent.isShiftDown()) {

            if (null != m_kSurfaceShape) {
                m_kPickCanvas.setShapeLocation(kMouseEvent);

                PickResult kPickResult = m_kPickCanvas.pickClosest();

                if (null != kPickResult) {

                    // If we get here, we should be able to pick the first
                    // intersection since we executed a pick closest.
                    PickIntersection kPick = kPickResult.getIntersection(0);

                    // Get the virtual world coordinates of the picked point.
                    Point3f kPickPoint = new Point3f(kPick.getPointCoordinatesVW());

                    // Get the normal vector for the picked point.
                    Vector3f kPickNormal = kPick.getPointNormal();

                    // Get vector from current path position to the picked
                    // point.  This vector and the normal vector must be
                    // pointing in opposite directions.  If not, then the
                    // normal vector needs to be negated since the vertex
                    // ordering for the triangle mesh is not guaranteed.
                    Vector3f kV = new Vector3f();

                    kV.sub(kPickPoint, m_kFlyPathBehavior.getPathPosition());

                    if (kV.dot(kPickNormal) > 0.0f) {
                        kPickNormal.negate();
                    }

                    // Add the point to the annotation list.
                    m_kAnnotateList.addItem(m_kFlyPathBehavior.getBranchIndex(),
                                            m_kFlyPathBehavior.getNormalizedPathDistance(),
                                            m_kFlyPathBehavior.isPathMoveForward(), kPickPoint, kPickNormal);
                    resetRenderAnnotateList();
                }
            }
        }
    }

    /**
     * Implement MouseInterface abstract method.
     *
     * @param  kMouseEvent  MouseEvent Contains information about the particular mouse event.
     */
    public void mouseEntered(MouseEvent kMouseEvent) { // implementation required -- do nothing
    }

    /**
     * Implement MouseInterface abstract method.
     *
     * @param  kMouseEvent  MouseEvent Contains information about the particular mouse event.
     */
    public void mouseExited(MouseEvent kMouseEvent) { // implementation required -- do nothing
    }

    
  
    /**
     * Implement MouseInterface abstract method.
     *
     * @param  kMouseEvent  MouseEvent Contains information about the particular mouse event.
     */
	public void mousePressed(MouseEvent event) {

		if (SwingUtilities.isRightMouseButton(event) && !event.isControlDown()) {
			currEventTime = event.getWhen();

			if ((currEventTime - prevEventTime) < 10) {
				pressed = false;
				return;
			}

			pressed = true;
			RightMouse mouse = new RightMouse(event);
			prevEventTime = event.getWhen();
			mouse.start();
		}

	}
    
   
    
    /**
     * The mouseDragged event is used when the right mouse button press down and dragged to adjust the camera viewing direction. 
     * @param event MouseEvent right mouse button press down and drag
     */
	public void mouseDragged(MouseEvent event) {
		    float currX, currY;
		    float centerX, centerY;
		    currX = event.getX();
		    currY = event.getY();
		   
		    centerX = getCanvas().getWidth() / 2f;
		    centerY = getCanvas().getHeight() / 2f;
		    
		    int viewDirection = 0;
		    float verticalDistance, horizontalDistance;
		    
		    if ( SwingUtilities.isRightMouseButton(event) && event.isControlDown() ) {
	    		
		    	   currEventTime = event.getWhen();
				  
			         if ((currEventTime - prevEventTime) < 100) {
			             pressed = false;
			             return;
			         }
			      
			         verticalDistance = Math.abs(currY - centerY);
			         horizontalDistance = Math.abs(currX - centerX);
			         
			         if ( currY < centerY && verticalDistance > horizontalDistance ) {
			        	 viewDirection = lookdown;
			         } else if ( currY > centerY && verticalDistance > horizontalDistance ) {
			        	 viewDirection = lookup;
			         } else if ( currX < centerX && verticalDistance < horizontalDistance ) {
			        	 viewDirection = lookright;
			         } else if ( currX > centerX && verticalDistance < horizontalDistance ) {
			        	 viewDirection = lookleft;
			         }
			         
			         
			         pressed = true;
			         RightMouseDragged mouse = new RightMouseDragged(event, viewDirection);
			         prevEventTime = event.getWhen();
			         mouse.start();
	    	}
		
	}

    public void mouseMoved(MouseEvent kMouseEvent) {
    	
    }
    
    /**
     * Implement MouseInterface abstract method.
     *
     * @param  kMouseEvent  MouseEvent Contains information about the particular mouse event.
     */
    public void mouseReleased(MouseEvent event) {
    	pressed = false;
    	prevEventTime = event.getWhen();
    }

    /**
     * Mouse wheel event invoked from the middle mouse button roller.  Rolling forward to track the fly-thru path in forward direction.
     * Rolling backward to track the fly-thru path in backward direction.  
     * @param event mouse middle mouse roller event.    
     */
    public void mouseWheelMoved(MouseWheelEvent event) {
    	
    	  currEventTime = event.getWhen();

    	  if ((currEventTime - prevEventTime) < 1000) {
              pressed = false;
              return;
          }
         
          pressed = true;
          MouseWheel mouse = new MouseWheel(event);
          prevEventTime = event.getWhen();
          mouse.start();
    	
    	
    }
    
    /**
     * Right mouse button press down and drag event handler.   When user press down the right mouse button and dragged, the 
     * RightMouseDragged handler control the camera viewing direction.    Running on thread to solve mouse drag continued interaction. 
     * @author ruida
     *
     */
    class RightMouseDragged extends Thread {

        /** DOCUMENT ME! */
        MouseEvent currentEvent;

        /** int centerX, centerY;. */
        MouseEvent evt;

        /** DOCUMENT ME! */
        Object source;

        /** DOCUMENT ME! */
        long when;

        /** DOCUMENT ME! */
        int x, y, mod, id;
        
        /** camera viewding direction. */
        int viewDirection;
        
        /**
         * Creates new thread and sets up mouse event variables appropriately.
         *
         * @param  event  Original mouse event, from button.
         */
        public RightMouseDragged(MouseEvent event, int _viewDirection) {
            when = event.getWhen();
            currentEvent = event;
            id = MouseEvent.MOUSE_DRAGGED;
            source = event.getSource();
            viewDirection = _viewDirection;
            evt = event;
         
        }

        /**
         * Runs the thread. While the button is pressed, dispatches mouse dragged events at a rate consistent with the
         * velocity slider. Once the mouse is released, <code>pressed</code> will be set to false and the loop will
         * stop.
         */
        public synchronized void run() {

            while (pressed) {

            	getCanvas().dispatchEvent(evt);

            	if ( viewDirection == lookup ) {
            		makeMove("lookup");	
            	} else if ( viewDirection == lookdown ) {
            		makeMove("lookdown");
            	} else if ( viewDirection == lookleft ) {
            		makeMove("lookleft");
            	} else if ( viewDirection == lookright) {
            		makeMove("lookright");
            	}
            	
                when += time;
                
                try {
                	wait(time);
                } catch ( InterruptedException e ) {
                    e.printStackTrace();
                }

                evt = new MouseEvent(getCanvas(), id, when, mod, Math.round(x), Math.round(y), 0, false);

            }

        }
    }

    
    /** 
     * RightMouse press event handler.   When the right mouse press down, rotate the camera view in clockwise direction. 
     * If the alt key is down and the right mouse press down event rotate the camera view in counter clockwise dirction. 
     * Running on thread to ensure the continuous  mouse press down interaction.  
     * @author ruida
     *
     */
    class RightMouse extends Thread {

        /** DOCUMENT ME! */
        MouseEvent currentEvent;

        /** int centerX, centerY;. */
        MouseEvent evt;

        /** DOCUMENT ME! */
        Object source;

        /** DOCUMENT ME! */
        long when;

        /** DOCUMENT ME! */
        int x, y, mod, id;
        
        /** flag indicate if the rotation is clockwise or counter clockwise. */
        boolean isClockRotation;
        
        /**
         * Creates new thread and sets up mouse event variables appropriately.
         *
         * @param  event  Original mouse event, from button.
         */
        public RightMouse(MouseEvent event) {
            when = event.getWhen();
            currentEvent = event;
            id = MouseEvent.MOUSE_DRAGGED;
            source = event.getSource();
            evt = event;
         
        }

        /**
         * Runs the thread. While the button is pressed, dispatches mouse dragged events at a rate consistent with the
         * velocity slider. Once the mouse is released, <code>pressed</code> will be set to false and the loop will
         * stop.
         */
        public synchronized void run() {

            while (pressed) {

            	getCanvas().dispatchEvent(evt);

            	if ( evt.getButton() == MouseEvent.BUTTON3 && !evt.isAltDown()) {
            	        isClockRotation = true;
            	} else if ( evt.getButton() == MouseEvent.BUTTON3 && evt.isAltDown() ) {
            		    isClockRotation = false;
            	}
            	
            	if ( isClockRotation ) {
            		makeMove("clockwise");	
            	} else {
            		makeMove("counterclockwise");
            	}
            	
                when += time;
                
                try {
                	wait(time);
                } catch ( InterruptedException e ) {
                    e.printStackTrace();
                }

                evt = new MouseEvent(getCanvas(), id, when, mod, Math.round(x), Math.round(y), 0, false);

            }

        }
    }

    
    
    /**
     * Mouse middle button wheel roller event handler.   Rolling forward to track the fly-thru path in forward direction. 
     * Rolling backward to track the fly-thru path in the backward direction. 
     * @author ruida
     *
     */
    class MouseWheel extends Thread {

        /** DOCUMENT ME! */
        MouseEvent currentEvent;

        /** int centerX, centerY;. */
        MouseEvent evt;

        /** DOCUMENT ME! */
        Object source;

        /** DOCUMENT ME! */
        long when;

        /** DOCUMENT ME! */
        int x, y, mod, id;
        int moveForward;
    
        /**
         * Creates new thread and sets up mouse event variables appropriately.
         *
         * @param  event  Original mouse event, from button.
         */
        public MouseWheel(MouseWheelEvent event) {
            when = event.getWhen();
            currentEvent = event;
            id = MouseEvent.MOUSE_DRAGGED;
            source = event.getSource();
            moveForward = event.getWheelRotation(); 
            evt = new MouseEvent(getCanvas(), MouseEvent.MOUSE_PRESSED, when, mod, Math.round(x),
                                 Math.round(y), 1, false);
        }

        /**
         * Runs the thread. While the button is pressed, dispatches mouse dragged events at a rate consistent with the
         * velocity slider. Once the mouse is released, <code>pressed</code> will be set to false and the loop will
         * stop.
         */
        public synchronized void run() {

            while (pressed) {

            	getCanvas().dispatchEvent(evt);

            
            	if ( moveForward < 0 ) {
            		makeMove("forward");
            	} else {
            		makeMove("backward");
            	}
        	
                when += 50;
                
                try {
                	wait(50);
                } catch ( InterruptedException e ) {
                    e.printStackTrace();
                }

                evt = new MouseEvent(getCanvas(), id, when, mod, Math.round(x), Math.round(y), 0, false);

            }

        }
    }

    
    /**
     * DOCUMENT ME!
     *
     * @return  DOCUMENT ME!
     */
    public synchronized boolean readImage() {
        int xDimen, yDimen, bufferSize;

        try {
            extents[2] = saveCounter;
            captureImage = new ModelImage(ModelStorageBase.ARGB, extents, "Screen capture");
            xDimen = extents[0];
            yDimen = extents[1];
            bufferSize = 4 * xDimen * yDimen;
            buffer = new short[bufferSize];

            for (int i = 0; i < saveCounter; i++) {
                FileIO fileIO = new FileIO();
                ModelImage tempImage = fileIO.readImage("captureImage" + i + "." + fileType,
                                                        kImage.getFileInfo(0).getFileDirectory(), false, null);

                synchronized (this) {
                    tempImage.exportData(0, bufferSize, buffer);
                    captureImage.importData((i * bufferSize), buffer, false);
                }
            }

            if (kImage.getNDims() > 4) {
                MipavUtil.displayError(" Animate cannot handle images with more than 4 dimensions");
            }

            if (kImage.getAnimateFrame() == null) {
                Color borderCol = new Color(1, 1, 1);
                new ViewJFrameAnimate(captureImage, null, null, null, null, null, parentFrame, capScreenWidth,
                                      capScreenHeight, saveCounter, false, borderCol, true);

            }

        } catch (IOException error) {
            Preferences.debug("IO exception");

            return false;
        } catch (OutOfMemoryError error) {
            MipavUtil.displayError("FlythruRender ScreenCapture: unable to allocate enough memory for RGB image");

            return false;
        }

        return true;
    }

    /**
     * ReplaceMesh is used by the Geodesic when a mesh is cut. The original mesh is changed, but not deleted and no new
     * mesh is added. The orginal mesh, kOld, is replced by the new mesh, kNew:
     *
     * @param  kOld  DOCUMENT ME!
     * @param  kNew  DOCUMENT ME!
     */
    public void replaceMesh(ModelTriangleMesh kOld, ModelTriangleMesh kNew) {

        /* Find the old mesh, kOld, and remove it from the scene graph, adding
         * the new mesh, kNew, in it's place: */
        for (int iChild = 0; iChild < m_kSceneRoot.numChildren(); iChild++) {

            if (m_kSceneRoot.getChild(iChild) instanceof BranchGroup) {
                BranchGroup kBG = (BranchGroup) (m_kSceneRoot.getChild(iChild));

                if (kBG.getChild(0) instanceof Shape3D) {
                    Shape3D kShapeMesh = (Shape3D) (kBG.getChild(0));

                    if (kShapeMesh.getGeometry() instanceof ModelTriangleMesh) {
                        ModelTriangleMesh kChildMesh = (ModelTriangleMesh) (kShapeMesh.getGeometry());

                        if (kChildMesh == kOld) {
                            kShapeMesh.removeGeometry(kOld);
                            kShapeMesh.addGeometry(kNew);

                            break;
                        }
                    }
                }
            }
        }
    }

    /**
     * DOCUMENT ME!
     */
    public void resetCounter() {
        imageCounter = 0;
        saveCounter = 0;
    }

    /**
     * DOCUMENT ME!
     */
    public void resetRenderAnnotateList() {

        // Clear the nodes being rendered.
        m_kAnnotatePointGroup.removeAllChildren();

        // Setup to render all of the annotated points in the list.
        Point3f kPoint = new Point3f();

        for (int iItem = 0; iItem < m_kAnnotateList.getNumItems(); iItem++) {
            FlyPathAnnotateList.Item kItem = m_kAnnotateList.getItem(iItem);

            kItem.getPointPosition(kPoint);

            Transform3D kTransform = new Transform3D();

            kTransform.set(new Vector3f(kPoint));

            TransformGroup kTransformGroup = new TransformGroup();

            kTransformGroup.setTransform(kTransform);
            kTransformGroup.addChild(kItem.getShape().cloneTree());

            BranchGroup kBranchGroup = new BranchGroup();

            kBranchGroup.setCapability(BranchGroup.ALLOW_DETACH);
            kBranchGroup.addChild(kTransformGroup);
            kBranchGroup.compile();
            m_kAnnotatePointGroup.addChild(kBranchGroup);
        }
    }


    /**
     * Set the current traversing branch.
     *
     * @param  _state  Object
     */
    public void setCurrentState(Object _state) {
        m_kFlyPathBehavior.setBranch(_state);
    }

    /**
     * Use the specified pseudocolors to render the computed mean curvatures for the previously specified mesh surface.
     *
     * @param  kMeanCurvaturesLUT  ModelLUT Defines the mapping of mean curvatures values to pseudocolor values. This
     *                             may be null which is used to indicated that the mean curvatures are not to be
     *                             rendered.
     */
    public void setMeanCurvaturesLUT(ModelLUT kMeanCurvaturesLUT) {
        m_kMeanCurvaturesLUT = kMeanCurvaturesLUT;
        resetRenderSurfaceColors();
        resetRenderSurface();
    }

    /**
     * If the color LUT defined for mean curvature pseudocolor mapping is defined, then use it to reset the colors.
     *
     * @param  _color  DOCUMENT ME!
     */
    public void setRenderSurfaceColors(Color _color) {

        if (null != m_kSurfaceGeometryInfo) {
            Color4f[] akVertexColors = null;

            // First, try to use the mean curvatures color map if specified.
            // if (null != m_kMeanCurvaturesLUT)
            // {
            float[] afMeanCurvatures = m_kMeshCurvatures.getMeanCurvatures();
            int iNumVertices = afMeanCurvatures.length;

            // ModelLUTHelper kMap = new ModelLUTHelper(m_kMeanCurvaturesLUT);
            akVertexColors = new Color4f[iNumVertices];

            for (int iVertex = 0; iVertex < iNumVertices; ++iVertex) {
                akVertexColors[iVertex] = new Color4f(_color);
            }
            // }

            // Next, try to use the defined in the ModelTriangleMesh.
            /*
             * else if (GeometryArray.COLOR_3 == (GeometryArray.COLOR_3 & m_kSurface.getVertexFormat())) {
             * akVertexColors = new Color3f[m_kSurface.getVertexCount()]; m_kSurface.getColors(0, akVertexColors); }
             */
            m_kSurfaceGeometryInfo.setColors(akVertexColors);
        }

        resetRenderSurface();
    }

    /**
     * Add the specified surface to the rendering.
     *
     * @param  kSurface  ModelTriangleMesh Surface represented as a triagle mesh.
     */
    public void setSurface(ModelTriangleMesh kSurface) {
        // Compute the segmentation of the surface based on the skeletonization curve(s).

        m_kSurfaceCurveSegments = new ModelTriangleMeshCurveSegments(kSurface, m_kFlyPathGraphSamples,
                                                                     m_kFlyPathGraphCurve,
                                                                     m_kOptions.m_iSegmentSurfaceBranchSamplesReductionFactor);
        rendererProgressBar.setValue(80);
        rendererProgressBar.update(rendererProgressBar.getGraphics());

        m_aiMeshConnectivity = null;

        // Use the triangle mesh stored in the curve segments instance
        // which may be different than the one specified.
        m_kSurface = m_kSurfaceCurveSegments.getTriangleMesh();

        // Compute the curvatures for the input mesh.
        m_kMeshCurvatures = new ModelTriangleMeshCurvatures(kSurface);

        // Create GeometryInfo for this surface.  It will be a triangle mesh
        // with the coordinates and normals specified.
        m_kSurfaceGeometry = null;
        m_kSurfaceGeometryInfo = new GeometryInfo(GeometryInfo.TRIANGLE_ARRAY);
        m_kSurfaceGeometryInfo.setCoordinates(m_kMeshCurvatures.getCoordinates());
        m_kSurfaceGeometryInfo.setNormals(m_kMeshCurvatures.getVertexNormals());
        m_kSurfaceGeometryInfo.setUseCoordIndexOnly(true);
        resetRenderSurfaceColors();
        resetRenderSurfaceConnectivity();
        resetRenderSurface();
        resetRenderScene();
    }

    /**
     * Setup flythru renderer.
     *
     * @param  _kImage   original model image reference.
     * @param  kOptions  setup options reference.
     */
    public void setupRender(ModelImage _kImage, SetupOptions kOptions) {

        kImage = _kImage;
        createDirectory();

        // Remember any necessary input parameters.
        m_kOptions = kOptions;

        // Setup layout of 3D image for mapping sample coordinates
        // to real coordinates.
        int[] aiExtents = kImage.getExtents();
        float[] afResolutions = kImage.getFileInfo(0).getResolutions();
        float[] afOrigins = kImage.getFileInfo(0).getOrigin();
        int[] aiDirections = kImage.getFileInfo(0).getAxisDirection();

        m_kVolumeLayout = new ModelImage3DLayout(aiExtents[0], aiExtents[1], aiExtents[2],
                                                 aiDirections[0] * (+afResolutions[0]),
                                                 aiDirections[1] * (-afResolutions[1]),
                                                 aiDirections[2] * (-afResolutions[2]), afOrigins[0], afOrigins[1],
                                                 afOrigins[2]);

        // Perform the skeletonization of the input image.
        // Extract the centerline curve.
        rendererProgressBar.setValue(10);
        rendererProgressBar.update(rendererProgressBar.getGraphics());
        m_kSkeleton = new Skeleton3D(kImage, m_kVolumeLayout);
        rendererProgressBar.setValue(30);
        rendererProgressBar.update(rendererProgressBar.getGraphics());

        m_kFlyPathGraphSamples = m_kSkeleton.getPathGraph(kOptions.m_iMaxBranches, kOptions.m_fMinBranchLength);
        rendererProgressBar.setValue(50);
        rendererProgressBar.update(rendererProgressBar.getGraphics());

        m_kFlyPathGraphCurve = new FlyPathGraphCurve(m_kFlyPathGraphSamples, kOptions.m_fFractionNumControlPoints, 2);

        rendererProgressBar.setValue(52);
        rendererProgressBar.update(rendererProgressBar.getGraphics());

        // Setup the view.
        setSize(300, 300);
        setVisible(false);
        // setDefaultCloseOperation(DISPOSE_ON_CLOSE);
        // getContentPane().setLayout(new BorderLayout());
        // setTitle("Virtual Endoscopy View");

        // Create the frame window for controlling the applet.
        // Position the control frame just to the right of the view frame.
        // m_kControlFrame = new JPanelEndoscopyControl(this);
        // m_kControlFrame.setLocation(new Point(getWidth(), 0));
        // m_kControlFrame.setTitle("Virtual Endoscopy Control");

        // Default geometry/appearance properties.
        // Set the radius based on the maximum distance of "inside"
        // volume samples to the surface.
        float fRadius = 0.03f * m_kSkeleton.getMaxBoundaryDistance();
        Shape3D kAnnotateShape = (new Sphere(fRadius)).getShape();

        kAnnotateShape.getAppearance().getMaterial().setDiffuseColor(0.0f, 1.0f, 0.0f);
        kAnnotateShape.getAppearance().getMaterial().setSpecularColor(0.0f, 0.0f, 0.0f);
        kAnnotateShape.getAppearance().getMaterial().setShininess(1.0f);
        m_kNormalColorPathVisited = new Color3f(0.0f, 1.0f, 0.0f);
        m_kNormalColorPathUnvisited = new Color3f(0.0f, 0.25f, 0.0f);
        m_kSelectColorPathVisited = new Color3f(1.0f, 0.0f, 0.0f);
        m_kSelectColorPathUnvisited = new Color3f(0.25f, 0.0f, 0.0f);

        // Create the canvas, place it alone in the panel, and then
        // setup this class to receive mouse events.
        kCanvas = new VolumeCanvas3D(SimpleUniverse.getPreferredConfiguration());

        // getContentPane().add("Center", kCanvas);
        kCanvas.addMouseListener(this);
        kCanvas.addMouseWheelListener(this);
        kCanvas.addMouseMotionListener(this);

        // Create the universe for the scene and setup for 3 transforms:
        m_kUniverse = new SimpleUniverse(kCanvas, 3);

        // Get access to the viewing platform which contains everything
        // on the view side of the scene graph.
        ViewingPlatform kViewingPlatform = m_kUniverse.getViewingPlatform();
        ViewPlatform kViewPlatform = kViewingPlatform.getViewPlatform();
        View kView = m_kUniverse.getViewer().getView();

        // Create the node to render the branch paths and their connections.
        int iNumBranches = m_kFlyPathGraphCurve.getNumBranches();

        m_kBranchPathShape = new Shape3D();
        m_kBranchPathShape.setCapability(Shape3D.ALLOW_GEOMETRY_READ);
        m_kBranchPathShape.setCapability(Shape3D.ALLOW_GEOMETRY_WRITE);
        m_kBranchPathShape.setCapability(Shape3D.ALLOW_BOUNDS_READ);
        m_kBranchConnectShape = (Shape3D) m_kBranchPathShape.cloneTree();
        m_kBranchPathShape.removeAllGeometries();
        m_kBranchConnectShape.removeAllGeometries();
        m_aiBranchIndexUnvisitedMin = new int[iNumBranches];
        m_aiBranchIndexUnvisitedMax = new int[iNumBranches];

        for (int iBranch = 0; iBranch < iNumBranches; iBranch++) {
            LineArray kGeometryBranchConnect = createBranchConnectGeometry(iBranch);

            m_kBranchConnectShape.addGeometry(kGeometryBranchConnect);

            LineStripArray kGeometryBranchPath = createBranchPathGeometry(iBranch);

            m_kBranchPathShape.addGeometry(kGeometryBranchPath);

            m_aiBranchIndexUnvisitedMin[iBranch] = 0;
            m_aiBranchIndexUnvisitedMax[iBranch] = kGeometryBranchPath.getVertexCount() - 1;
        }

        // Create the node to render the surface.
        Appearance kSurfaceAppearance = new Appearance();
        PolygonAttributes kSurfacePolygonAttributes = new PolygonAttributes();

        kSurfacePolygonAttributes.setCullFace(PolygonAttributes.CULL_NONE);
        kSurfacePolygonAttributes.setBackFaceNormalFlip(true);
        kSurfacePolygonAttributes.setPolygonMode(PolygonAttributes.POLYGON_FILL);
        kSurfacePolygonAttributes.setPolygonOffsetFactor(1.0f);
        kSurfacePolygonAttributes.setCapability(PolygonAttributes.ALLOW_MODE_READ);
        kSurfacePolygonAttributes.setCapability(PolygonAttributes.ALLOW_MODE_WRITE);

        kSurfaceAppearance.setPolygonAttributes(kSurfacePolygonAttributes);
        kSurfaceAppearance.setCapability(Appearance.ALLOW_POLYGON_ATTRIBUTES_READ);
        kSurfaceAppearance.setCapability(Appearance.ALLOW_POLYGON_ATTRIBUTES_WRITE);

        Material kSurfaceMaterial = new Material();

        kSurfaceMaterial.setLightingEnable(true);
        kSurfaceMaterial.setShininess(1.0f); // not shiny

        // kSurfaceMaterial.setDiffuseColor(0.0f, 1.0f, 0.0f);
        kSurfaceMaterial.setSpecularColor(0.0f, 0.0f, 0.0f);
        kSurfaceMaterial.setAmbientColor(0.0f, 0.0f, 0.0f);
        kSurfaceAppearance.setMaterial(kSurfaceMaterial);
        m_kSurfaceShape = new Shape3D();
        m_kSurfaceShape.setAppearance(kSurfaceAppearance);
        m_kSurfaceShape.setCapability(Shape3D.ALLOW_BOUNDS_READ);
        m_kSurfaceShape.setCapability(Shape3D.ALLOW_GEOMETRY_WRITE);
        m_kSurfaceShape.setCapability(Shape3D.ALLOW_APPEARANCE_READ);
        m_kSurfaceShape.setCapability(Shape3D.ALLOW_APPEARANCE_WRITE);

        BranchGroup kSurfaceBG = new BranchGroup();

        kSurfaceBG.setCapability(Group.ALLOW_CHILDREN_EXTEND);
        kSurfaceBG.setCapability(Group.ALLOW_CHILDREN_READ);
        kSurfaceBG.setCapability(Group.ALLOW_CHILDREN_WRITE);
        kSurfaceBG.addChild(m_kSurfaceShape);

        // Setup this canvas for picking anything in the scene rendered
        // onto the 3D canvas.  Setup to get the detailed geometry info
        // on each pick.  Since we are interested in picking triangles,
        // setup the picking tolerance so that it is fast.
        m_kPickCanvas = new PickCanvas(kCanvas, kSurfaceBG);
        m_kPickCanvas.setMode(PickTool.GEOMETRY_INTERSECT_INFO);
        m_kPickCanvas.setTolerance(0.0f);
        m_kPickCanvas.setCapabilities(m_kSurfaceShape, PickCanvas.INTERSECT_FULL);

        // Create the node to render the annotation points as they are added.
        // Create default shape to render and create list to store all
        // of the annotations.
        m_kAnnotateList = new FlyPathAnnotateList();
        m_kAnnotateList.setDefaultShape(kAnnotateShape);
        m_kAnnotatePointGroup = new Group();
        m_kAnnotatePointGroup.setCapability(Group.ALLOW_CHILDREN_EXTEND);
        m_kAnnotatePointGroup.setCapability(Group.ALLOW_CHILDREN_WRITE);
        resetRenderAnnotateList();

        rendererProgressBar.setValue(55);
        rendererProgressBar.update(rendererProgressBar.getGraphics());

        m_kGeodesicGroup = new Group();
        m_kGeodesicGroup.setCapability(Group.ALLOW_CHILDREN_EXTEND);
        m_kGeodesicGroup.setCapability(Group.ALLOW_CHILDREN_WRITE);
        m_kGeodesicGroup.setCapability(Group.ALLOW_CHILDREN_READ);

        // Access the three transforms for the viewing platform.
        // (0) Camera position
        // (1) Camera orientation relative to base view direction.
        // (2) Camera base view direction.
        MultiTransformGroup kMultiTransformGroup = kViewingPlatform.getMultiTransformGroup();
        TransformGroup kTransformPosition = kMultiTransformGroup.getTransformGroup(0);
        TransformGroup kTransformDirection = kMultiTransformGroup.getTransformGroup(1);
        TransformGroup kTransformOrientation = kMultiTransformGroup.getTransformGroup(2);

        // Setup mouse behavior to adjust the camera orientation.
        // Change the scale factors (from the defaults) for how the virtual
        // trackball translate/rotate/zoom respond to the mouse movement.
        m_kMouseRotateBehavior = new MouseRotateExt();
        m_kMouseRotateBehavior.setTransformGroup(kTransformOrientation);
        m_kMouseRotateBehavior.setFactor(0.1 * m_kMouseRotateBehavior.getXFactor());
        m_kMouseRotateBehavior.setupCallback(this);

        // Setup behavior to handle flying down the path and
        // looking around.
        m_kFlyPathBehavior = new FlyPathBehavior(m_kFlyPathGraphCurve, m_kAnnotateList, kTransformPosition,
                                                 kTransformDirection, kTransformOrientation, this);
        // m_kFlyPathBehavior.setEnable(true);
        // m_kFlyPathBehavior.setupCallback(this);

        rendererProgressBar.setValue(56);
        rendererProgressBar.update(rendererProgressBar.getGraphics());
        
        // Setup the scene graph.
        m_kSceneRoot = new BranchGroup();
        m_kSceneRoot.setCapability(Group.ALLOW_CHILDREN_EXTEND);
        m_kSceneRoot.setCapability(Group.ALLOW_CHILDREN_READ);
        m_kSceneRoot.setCapability(Group.ALLOW_CHILDREN_WRITE);

        m_kSceneRoot.addChild(m_kBranchPathShape);
        m_kSceneRoot.addChild(m_kBranchConnectShape);
        m_kSceneRoot.addChild(kSurfaceBG);
        m_kSceneRoot.addChild(m_kAnnotatePointGroup);
        m_kSceneRoot.addChild(m_kGeodesicGroup);
        m_kSceneRoot.addChild(m_kMouseRotateBehavior);
        m_kSceneRoot.addChild(m_kFlyPathBehavior);
        m_kSceneRoot.compile();
        m_kUniverse.addBranchGraph(m_kSceneRoot);

        // Create a point light source positioned at the view point.
        m_kPointLight = new PointLight();
        m_kPointLight.setCapability(PointLight.ALLOW_INFLUENCING_BOUNDS_WRITE);
        m_kPointLight.setPosition(0.0f, 0.0f, 0.0f);

        PlatformGeometry kPlatformGeometry = new PlatformGeometry();

        kPlatformGeometry.addChild(m_kPointLight);
        kViewingPlatform.setPlatformGeometry(kPlatformGeometry);

        // Set the far clipping distance so that the volume can be seen.
        kView.setProjectionPolicy(View.PERSPECTIVE_PROJECTION);

        resetRenderScene();

        // Done with setup, now show the view and the control panel.
        setVisible(true);
    }

    /**
     * DOCUMENT ME!
     *
     * @param  _control  DOCUMENT ME!
     */
    public void setupRenderControl(JPanelVirtualEndoscopySetup _control) {
        m_kControlFrame = _control;
    }

    /**
     * Toggle between wireframe and filled polygon mode:
     */
    public void toggleWireframe() {

        if (m_kSurfaceShape.getAppearance().getPolygonAttributes().getPolygonMode() == PolygonAttributes.POLYGON_LINE) {
            m_kSurfaceShape.getAppearance().getPolygonAttributes().setPolygonMode(PolygonAttributes.POLYGON_FILL);
        } else {
            m_kSurfaceShape.getAppearance().getPolygonAttributes().setPolygonMode(PolygonAttributes.POLYGON_LINE);
        }
    }

    /**
     * MouseBehaviorCallback override. This method is called whenever any MouseBehavior-derived class causes a change to
     * the transformation which it is controlling.
     *
     * @param  iType       will be one of ROTATE, TRANSLATE or ZOOM
     * @param  kTransform  the updated Transform3D controlled by the MouseBehavior
     */
    public void transformChanged(int iType, Transform3D kTransform) {

        if (MouseBehaviorCallback.ROTATE == iType) {
            m_kControlFrame.setPathPosition(m_kFlyPathBehavior.getPathPosition());
            m_kControlFrame.setViewDirection(m_kFlyPathBehavior.getViewDirection());
            m_kControlFrame.setViewOrientation(m_kFlyPathBehavior.getViewOrientation());
        }
    }

    /**
     * Implementation of the FlyPathBehavior.Callback interface.
     *
     * @param  kFlyPathBehavior  reference to the MjFlyPathBehavior for which the view changed.
     * @param  iEvent            Bitmask identifies the event(s) which caused the view to change. Bitmask created from
     *                           OR of EVENT_* defintions.
     */
    public void viewChanged(FlyPathBehavior kFlyPathBehavior, int iEvent) {

        // Only update if there is a control frame defined.
        if ((null == m_kControlFrame) || (!kFlyPathBehavior.equals(m_kFlyPathBehavior))) {
            return;
        }

        // Update the control panel.
        if (FlyPathBehavior.EVENT_CHANGE_POSITION == (FlyPathBehavior.EVENT_CHANGE_POSITION & iEvent)) {
            m_kControlFrame.updatePosition(kFlyPathBehavior);

            // Update the path.
            resetRenderBranchPath();

            // Get the current set of triangle indices given the segmentation
            // based on the current curve position.  Only update if the
            // array changes.
            if (null != m_kSurfaceCurveSegments) {
                int iCurve = kFlyPathBehavior.getBranchIndex();
                int[] aiConnectivity = m_kSurfaceCurveSegments.getTriangleArrayIndices(iCurve,
                                                                                       kFlyPathBehavior.getNormalizedPathDistance());

                if (aiConnectivity != m_aiMeshConnectivity) {
                    m_aiMeshConnectivity = aiConnectivity;
                    resetRenderSurfaceConnectivity();
                    resetRenderSurface();
                    resetRenderScene();
                }
            }
        }

        if (FlyPathBehavior.EVENT_CHANGE_ORIENTATION == (FlyPathBehavior.EVENT_CHANGE_ORIENTATION & iEvent)) {
            m_kControlFrame.updateOrientation(kFlyPathBehavior);
        }

        if (FlyPathBehavior.EVENT_CHANGE_BRANCH == (FlyPathBehavior.EVENT_CHANGE_BRANCH & iEvent)) {
            resetRenderBranchConnect();
        }
    }


    /* (non-Javadoc)
     * @see gov.nih.mipav.view.renderer.J3D.RenderViewBase#writeImage()
     */
    public synchronized boolean writeImage() {
        int bufferSize, xDim, yDim;
        JSplitPane viewPane;
        Image imagePix;

        Dimension d = new Dimension();
        Point p = new Point();

        p.x = 0;
        p.y = 0;

        viewPane = parentFrame.getViewPanel();
        SwingUtilities.convertPointToScreen(p, viewPane);

        d.width = viewPane.getWidth();
        d.height = viewPane.getHeight();

        Rectangle currentRectangle = new Rectangle(p, d);

        // using Robot to capture image rectangle and transfering image into
        // the pixel buffer.
        try {

            if (imageCounter == 0) {
                robot = new Robot();
            }

            imagePix = robot.createScreenCapture(currentRectangle);

            xDim = currentRectangle.width;
            yDim = currentRectangle.height;
            xDim = xDim - (xDim % 4);
            yDim = yDim - (yDim % 4);
            capScreenWidth = xDim;
            capScreenHeight = yDim;
            bufferSize = 4 * xDim * yDim;

            if (imageCounter == 0) {
                extents[0] = xDim; // RGB
                extents[1] = yDim;
                extents[2] = 10;

                buffer = new short[bufferSize];
                pixels = new int[xDim * yDim];
            }

            BufferedImage copy = new BufferedImage(xDim, yDim, BufferedImage.TYPE_INT_RGB);

            Graphics2D g2d = copy.createGraphics();

            g2d.scale(1, 1);
            g2d.drawImage(imagePix, 0, 0, null);
            g2d.dispose();

            ImageIO.write(copy, fileType, new File(directory + "captureImage" + saveCounter + "." + fileType));

        } catch (OutOfMemoryError error) {
            MipavUtil.displayError("FlythruRender ScreenCapture: unable to allocate enough memory for RGB image");

            return false;
        } catch (AWTException error) {
            MipavUtil.displayError("Platform doesn't support screen capture.");

            return false;
        } catch (IOException e) {
            System.out.println("Could not save image");
        }

        saveCounter++;
        imagePix = null;
        pixels = null;
        buffer = null;
        robot = null;

        return true;
    }


    /* (non-Javadoc)
     * @see gov.nih.mipav.view.renderer.flythroughview.FlyThroughRenderInterface#getCounter()
     */
    public int getCounter() {
        return saveCounter;
    }

    /* (non-Javadoc)
     * @see javax.media.j3d.Canvas3D#getHeight()
     */
    public int getHeight() {
        return capScreenHeight;
    }

    /* (non-Javadoc)
     * @see javax.media.j3d.Canvas3D#getWidth()
     */
    public int getWidth() {
        return capScreenWidth;
    }
    
    /**
     * Called any time a change has been made to a new branch so that the connection between the branch and its parent
     * can be rendered.
     */
    protected void resetRenderBranchConnect() {

        // Access the GeometryArray-derived instance for the rendering
        // of the specified branch connection.
        GeometryArray kGeometryArray = (GeometryArray) m_kBranchConnectShape.getGeometry(m_kFlyPathBehavior.getBranchIndex());

        // Mark the color as being visited.
        kGeometryArray.setColor(0, m_kNormalColorPathVisited);
        kGeometryArray.setColor(1, m_kNormalColorPathVisited);

        // Are we at a branch point and is there a currently selected branch,
        // or a previous one?
        int iSelectedBranch = m_kFlyPathBehavior.getSelectedBranchIndex();

        if ((-1 != iSelectedBranch) || (-1 != m_iLastSelectedBranchIndex)) {

            // If a branch was selected and it is different from
            // the one currently selected, then reset all of its colors
            // to normal.
            if ((-1 != m_iLastSelectedBranchIndex) && (m_iLastSelectedBranchIndex != iSelectedBranch)) {
                int iBranch = m_iLastSelectedBranchIndex;
                GeometryStripArray kGeometryStripArray = (GeometryStripArray) m_kBranchPathShape.getGeometry(iBranch);

                // How many points are in the array?
                int iNumPoints = kGeometryStripArray.getVertexCount();
                int iIndex = 0;

                while (iIndex < m_aiBranchIndexUnvisitedMin[iBranch]) {
                    kGeometryStripArray.setColor(iIndex++, m_kNormalColorPathVisited);
                }

                while (iIndex <= m_aiBranchIndexUnvisitedMax[iBranch]) {
                    kGeometryStripArray.setColor(iIndex++, m_kNormalColorPathUnvisited);
                }

                while (iIndex < iNumPoints) {
                    kGeometryStripArray.setColor(iIndex++, m_kNormalColorPathVisited);
                }
            }

            // If this branch is selected, then reset the colors
            // for the entire path as being selected.  Redraw entire
            // branch just in case one of the unvisited limits changed.
            if (-1 != iSelectedBranch) {

                // Access the GeometryStripArray-derived instance for the rendering
                // of the specified branch path.
                int iBranch = m_kFlyPathBehavior.getBranchIndex();
                GeometryStripArray kGeometryStripArray = (GeometryStripArray) m_kBranchPathShape.getGeometry(iBranch);
                int iNumPoints = kGeometryStripArray.getVertexCount();

                // Convert the current branch point position to curve
                // sample index for rendering.
                int iBranchPoint = (int) (iNumPoints * m_kFlyPathBehavior.getNormalizedPathDistance());

                // What is the range of samples to be colored as unvisited.
                int iUnvisitedMin = m_aiBranchIndexUnvisitedMin[iBranch];
                int iUnvisitedMax = m_aiBranchIndexUnvisitedMax[iBranch];

                // Heading start-to-end?
                if (m_kFlyPathBehavior.isPathMoveForward()) {

                    for (int i = iBranchPoint; i < iNumPoints; i++) {

                        if ((iUnvisitedMin <= i) && (i <= iUnvisitedMax)) {
                            kGeometryStripArray.setColor(i, m_kSelectColorPathUnvisited);

                        } else {
                            kGeometryStripArray.setColor(i, m_kSelectColorPathVisited);
                        }
                    }
                } // Heading end-to-start?
                else {

                    for (int i = iBranchPoint; i >= 0; i--) {

                        if ((iUnvisitedMin <= i) && (i <= iUnvisitedMax)) {
                            kGeometryStripArray.setColor(i, m_kSelectColorPathUnvisited);

                        } else {
                            kGeometryStripArray.setColor(i, m_kSelectColorPathVisited);
                        }
                    }
                }
            }
        }

        m_iLastSelectedBranchIndex = iSelectedBranch;
    }

    /**
     * Called any time the position along the current curve changes and the color of the curve needs to change to show
     * what has been visited.
     */
    protected void resetRenderBranchPath() {

        // Access the GeometryStripArray-derived instance for the rendering
        // of the specified branch path.
        int iBranch = m_kFlyPathBehavior.getBranchIndex();
        GeometryStripArray kGeometryStripArray = (GeometryStripArray) m_kBranchPathShape.getGeometry(iBranch);

        // How many points are in the array?
        int iNumPoints = kGeometryStripArray.getVertexCount();

        // What is the range of samples to be colored as unvisited.
        int iUnvisitedMin = (int) (m_kFlyPathBehavior.getBranchDistUnvisitedMin() * (iNumPoints - 1));
        int iUnvisitedMax = (int) (m_kFlyPathBehavior.getBranchDistUnvisitedMax() * (iNumPoints - 1));

        // Initially, all paths were marked as being unvisited, and since
        // you cannot "unvisit" a point along a path, we just need to go
        // through and mark each node as visited or not.  Update any newly
        // visited points.
        for (int iIndex = m_aiBranchIndexUnvisitedMin[iBranch]; iIndex < iUnvisitedMin; iIndex++) {
            kGeometryStripArray.setColor(iIndex, m_kNormalColorPathVisited);
        }

        for (int iIndex = m_aiBranchIndexUnvisitedMax[iBranch]; iIndex > iUnvisitedMax; iIndex--) {
            kGeometryStripArray.setColor(iIndex, m_kNormalColorPathVisited);
        }

        // Save the new range of unvisited range of the path.
        m_aiBranchIndexUnvisitedMin[iBranch] = iUnvisitedMin;
        m_aiBranchIndexUnvisitedMax[iBranch] = iUnvisitedMax;
    }

    /**
     * Called any time the scene needs to be regenerated.
     */
    protected void resetRenderScene() {

        // Get access to the viewing platform which contains everything
        // on the view side of the scene graph.
        ViewingPlatform kViewingPlatform = m_kUniverse.getViewingPlatform();
        ViewPlatform kViewPlatform = kViewingPlatform.getViewPlatform();
        View kView = m_kUniverse.getViewer().getView();

        // Don't forget that the behaviors won't work unless the activation
        // radius for the ViewPlatform intersects the scheduling bounds
        // for that behavior.  Make sure the clipping distance is
        // the same for all projection policies.

        // Compute the bounds for the objects in the scene.
        // Compute the bounds for the entire scene (larger than objects bounds).
        BoundingSphere kObjectsBounds = new BoundingSphere((null != m_kSurfaceGeometry)
                                                           ? m_kSurfaceShape.getBounds()
                                                           : m_kBranchPathShape.getBounds());
        BoundingSphere kSceneBounds = new BoundingSphere(kObjectsBounds);

        kSceneBounds.setRadius(kSceneBounds.getRadius() * 100.0);

        // Things that depend on the bounds of the objects in the scene.
        m_kMouseRotateBehavior.setSchedulingBounds(kObjectsBounds);
        m_kFlyPathBehavior.setSchedulingBounds(kObjectsBounds);
        m_kPointLight.setInfluencingBounds(kSceneBounds);
        kViewPlatform.setActivationRadius((float) kSceneBounds.getRadius());
        kView.setBackClipDistance((float) kSceneBounds.getRadius());
    }

    /**
     * Called whenever any of the rendering properties of the surface are modified.
     */
    protected void resetRenderSurface() {

        if (null != m_kSurfaceGeometryInfo) {

            // Create an instance of IndexedGeometryArray given the GeometryInfo
            // instance.  Create by reference to avoid unnecessary copying.
            m_kSurfaceGeometry = m_kSurfaceGeometryInfo.getIndexedGeometryArray(false, // do not compact
                                                                                true, // create by-reference
                                                                                false, // non-interleaved
                                                                                true, // set USE_COORD_INDEX_ONLY
                                                                                false); // do not use java.nio.Buffer

            if (m_kSurfaceGeometryInfo.getColors() != null) {
                m_kSurfaceMesh = new ModelTriangleMesh(m_kSurfaceGeometryInfo.getCoordinates(),
                                                       m_kSurfaceGeometryInfo.getNormals(),
                                                       (Color4f[]) m_kSurfaceGeometryInfo.getColors(),
                                                       m_kSurfaceGeometryInfo.getCoordinateIndices());

            } else {
                m_kSurfaceMesh = new ModelTriangleMesh(m_kSurfaceGeometryInfo.getCoordinates(),
                                                       m_kSurfaceGeometryInfo.getNormals(),
                                                       m_kSurfaceGeometryInfo.getCoordinateIndices());
            }

            m_kSurfaceMesh.setCapability(Shape3D.ALLOW_GEOMETRY_READ);
            m_kSurfaceMesh.setCapability(GeometryArray.ALLOW_FORMAT_READ);
            m_kSurfaceMesh.setCapability(GeometryArray.ALLOW_REF_DATA_READ);
            m_kSurfaceMesh.setCapability(GeometryArray.ALLOW_COUNT_READ);
            m_kSurfaceMesh.setCapability(GeometryArray.ALLOW_COORDINATE_READ);
            m_kSurfaceMesh.setCapability(GeometryArray.ALLOW_NORMAL_READ);
            m_kSurfaceMesh.setCapability(IndexedGeometryArray.ALLOW_COORDINATE_INDEX_READ);
            m_kSurfaceMesh.setCapability(IndexedGeometryArray.ALLOW_NORMAL_INDEX_READ);

            m_kSurfaceShape.setGeometry(m_kSurfaceMesh);

            // Allow picking.
            m_kSurfaceGeometry.setCapability(Shape3D.ALLOW_GEOMETRY_READ);
            m_kSurfaceGeometry.setCapability(GeometryArray.ALLOW_FORMAT_READ);
            m_kSurfaceGeometry.setCapability(GeometryArray.ALLOW_REF_DATA_READ);
            m_kSurfaceGeometry.setCapability(GeometryArray.ALLOW_COUNT_READ);
            m_kSurfaceGeometry.setCapability(GeometryArray.ALLOW_COORDINATE_READ);
            m_kSurfaceGeometry.setCapability(GeometryArray.ALLOW_NORMAL_READ);
            m_kSurfaceGeometry.setCapability(IndexedGeometryArray.ALLOW_COORDINATE_INDEX_READ);
            m_kSurfaceGeometry.setCapability(IndexedGeometryArray.ALLOW_NORMAL_INDEX_READ);

            // m_kSurfaceShape.setGeometry(m_kSurfaceGeometry);
        }
    }

    /**
     * If the color LUT defined for mean curvature pseudocolor mapping is defined, then use it to reset the colors.
     */
    protected void resetRenderSurfaceColors() {

        if (null != m_kSurfaceGeometryInfo) {
            Color4f[] akVertexColors = null;

            // First, try to use the mean curvatures color map if specified.
            if (null != m_kMeanCurvaturesLUT) {
                float[] afMeanCurvatures = m_kMeshCurvatures.getMeanCurvatures();
                int iNumVertices = afMeanCurvatures.length;
                ModelLUTHelper kMap = new ModelLUTHelper(m_kMeanCurvaturesLUT);

                akVertexColors = new Color4f[iNumVertices];

                for (int iVertex = 0; iVertex < iNumVertices; ++iVertex) {
                    akVertexColors[iVertex] = new Color4f(new Color(kMap.mapValue(afMeanCurvatures[iVertex])));
                }
            } // Next, try to use the defined in the ModelTriangleMesh.
            else if (GeometryArray.COLOR_4 == (GeometryArray.COLOR_4 & m_kSurface.getVertexFormat())) {
                akVertexColors = new Color4f[m_kSurface.getVertexCount()];
                for ( int i = 0; i < m_kSurface.getVertexCount(); i++ )
                {
                    akVertexColors[i] = new Color4f();
                }
                m_kSurface.getColors(0, akVertexColors);
            }

            m_kSurfaceGeometryInfo.setColors(akVertexColors);
        }
    }

    /**
     * Resets the portion of the surface to be rendered based on the current triangle connectivity information.
     */
    protected void resetRenderSurfaceConnectivity() {

        // If the connectivity is not defined, then load up all of the
        // triangles from the original mesh.
        if (null != m_aiMeshConnectivity) {
            m_kSurfaceGeometryInfo.setCoordinateIndices(m_aiMeshConnectivity);
        } else if (null != m_kMeshCurvatures) {
            m_kSurfaceGeometryInfo.setCoordinateIndices(m_kMeshCurvatures.getCoordinateIndices());
        }
    }

    /**
     * Get the geometry to be used for rendering the connection of the specified branch to its parent branch.
     *
     * @param   iBranch  Index which identifies the branch.
     *
     * @return  LineArray instance that can be attached to a Shape3D node for rendering
     */
    private LineArray createBranchConnectGeometry(int iBranch) {

        // Create a single line segment.
        LineArray kLineArray = new LineArray(2, GeometryArray.COORDINATES | GeometryArray.COLOR_3);

        kLineArray.setCapability(GeometryArray.ALLOW_COLOR_WRITE);

        // Mark the connection as unvisited.
        kLineArray.setColor(0, m_kNormalColorPathUnvisited);
        kLineArray.setColor(1, m_kNormalColorPathUnvisited);

        // Get the index of the parent.
        int iBranchParent = m_kFlyPathGraphCurve.getBranchParentIndex(iBranch);

        // Set the point for the start of the child branch.
        Curve3f kCurveChild = m_kFlyPathGraphCurve.getCurvePosition(iBranch);
        float fTimeChild = kCurveChild.GetTime(0.0f, 100, 1e-02f);

        WildMagic.LibFoundation.Mathematics.Vector3f kVec = kCurveChild.GetPosition(fTimeChild);
        Point3f kPos = new Point3f( kVec.X, kVec.Y, kVec.Z );
        kLineArray.setCoordinate(0, kPos);

        // If there is no parent, then replicate the child start point
        if (iBranchParent < 0) {
        	kVec = kCurveChild.GetPosition(fTimeChild);
        	kPos = new Point3f( kVec.X, kVec.Y, kVec.Z );
            kLineArray.setCoordinate(1, kPos);
        } // Otherwise, set the point for the position along the parent

        // branch where the child branch starts.
        else {
            float fParentNormalizedDist = m_kFlyPathGraphCurve.getBranchParentNormalizedDist(iBranch);
            Curve3f kCurveParent = m_kFlyPathGraphCurve.getCurvePosition(iBranchParent);
            float fTimeParent = kCurveParent.GetTime(kCurveParent.GetTotalLength() * fParentNormalizedDist, 100,
                                                     1e-02f);
            kVec = kCurveParent.GetPosition(fTimeParent);
        	kPos = new Point3f( kVec.X, kVec.Y, kVec.Z );
            kLineArray.setCoordinate(1, kPos);
        }

        return kLineArray;
    }

    /**
     * Get the geometry to be used for rendering the path of the specified branch.
     *
     * @param   iBranch  Index which identifies the branch.
     *
     * @return  LineStripArray instance that can be attached to a Shape3D node for rendering
     */
    private LineStripArray createBranchPathGeometry(int iBranch) {
        int iNumVertex = 500;
        int[] aiNumStripVertex = new int[1];

        aiNumStripVertex[0] = iNumVertex;

        LineStripArray kLineStripArray = new LineStripArray(iNumVertex,
                                                            GeometryArray.COORDINATES | GeometryArray.COLOR_3,
                                                            aiNumStripVertex);

        kLineStripArray.setCapability(GeometryArray.ALLOW_COLOR_WRITE);
        kLineStripArray.setCapability(GeometryArray.ALLOW_COUNT_READ);

        Curve3f kCurve = m_kFlyPathGraphCurve.getCurvePosition(iBranch);
        float fStep = kCurve.GetTotalLength() / (float) (iNumVertex - 1);

        for (int iPoint = 0; iPoint < iNumVertex; ++iPoint) {
            float fDist = iPoint * fStep;
            float fTime = kCurve.GetTime(fDist, 100, 1e-02f);

            WildMagic.LibFoundation.Mathematics.Vector3f kVec = kCurve.GetPosition(fTime);
            Point3f kPos = new Point3f( kVec.X, kVec.Y, kVec.Z );
            kLineStripArray.setCoordinate(iPoint, kPos);
            kLineStripArray.setColor(iPoint, m_kNormalColorPathUnvisited);
        }

        return kLineStripArray;
    }

    /**
     * Get the geometry to be used for rendering the path of the specified branch.
     *
     * @param   iBranch  Index which identifies the branch.
     *
     * @return  LineStripArray instance that can be attached to a Shape3D node for rendering.
     *
     *          <p>The LineStripArray coordinates are scaled to match the ModelTriangleMesh in JPanelSurface.</p>
     */
    private LineStripArray createBranchPathGeometryScaled(int iBranch) {
        int iNumVertex = 500;
        int[] aiNumStripVertex = new int[1];

        aiNumStripVertex[0] = iNumVertex;

        LineStripArray kLineStripArray = new LineStripArray(iNumVertex,
                                                            GeometryArray.COORDINATES | GeometryArray.COLOR_3,
                                                            aiNumStripVertex);

        kLineStripArray.setCapability(GeometryArray.ALLOW_COLOR_WRITE);
        kLineStripArray.setCapability(GeometryArray.ALLOW_COUNT_READ);

        Curve3f kCurve = m_kFlyPathGraphCurve.getCurvePosition(iBranch);
        float fStep = kCurve.GetTotalLength() / (float) (iNumVertex - 1);

        int[] aiExtents = kImage.getExtents();
        float[] afResolutions = kImage.getFileInfo(0).getResolutions();
        float[] afOrigins = kImage.getFileInfo(0).getOrigin();
        int[] aiDirections = kImage.getFileInfo(0).getAxisDirection();

        int xDim = aiExtents[0];
        int yDim = aiExtents[1];
        int zDim = aiExtents[2];

        float xBox = (xDim - 1) * afResolutions[0];
        float yBox = (yDim - 1) * afResolutions[1];
        float zBox = (zDim - 1) * afResolutions[2];
        float maxBox = Math.max(xBox, Math.max(yBox, zBox));

        for (int iPoint = 0; iPoint < iNumVertex; ++iPoint) {
            float fDist = iPoint * fStep;
            float fTime = kCurve.GetTime(fDist, 100, 1e-02f);      
            WildMagic.LibFoundation.Mathematics.Vector3f kVec = kCurve.GetPosition(fTime);
            Point3f kPoint = new Point3f( kVec.X, kVec.Y, kVec.Z );

            kPoint.x = ((2.0f * (kPoint.x - afOrigins[0]) / aiDirections[0]) - ((xDim - 1) * afResolutions[0])) /
                           maxBox;
            kPoint.y = ((2.0f * (kPoint.y - afOrigins[1]) / aiDirections[1]) - ((yDim - 1) * afResolutions[1])) /
                           maxBox;
            kPoint.z = ((2.0f * (kPoint.z - afOrigins[2]) / aiDirections[2]) - ((zDim - 1) * afResolutions[2])) /
                           maxBox;

            kLineStripArray.setCoordinate(iPoint, kPoint);
            kLineStripArray.setColor(iPoint, m_kNormalColorPathVisited);
        }

        return kLineStripArray;
    }

    /**
     * Create flythru directory to save the captured images from AVI recorder.
     */
    private void createDirectory() {
        directory = kImage.getFileInfo(0).getFileDirectory() + "flythru" + File.separatorChar;

        File file = new File(directory);

        if (!file.exists()) {
            file.mkdir();
        }
    }

    //~ Inner Classes --------------------------------------------------------------------------------------------------

    /**
     * DOCUMENT ME!
     */
    public static class SetupOptions {

        /**
         * Percentage of path samples points to use for the number of control points for a BSpline curve fit. A larger
         * number may create a curve which oscilates more. A smaller number may create a curve which is too smooth to
         * even be close to the original path samples.
         */
        public float m_fFractionNumControlPoints = 0.07f;

        /** Minimum length of branch to extract. */
        public float m_fMinBranchLength = 100.0f;

        /** Maximum number of branches to extract. */
        public int m_iMaxBranches = 1;

        /**
         * The number of samples which define each branch path is reduced by this factor for the purpose of speeding up
         * the segmentation of the surface.
         */
        public int m_iSegmentSurfaceBranchSamplesReductionFactor = 2;
    }

    /**
     * Extend the MouseRotate class so that we can override the processMouseEvent method to ignore any events which
     * occur while the shift key is down. We use the shift key to perform picking operations.
     */
    private class MouseRotateExt extends MouseRotate {

        /**
         * Creates a new MouseRotateExt object.
         */
        public MouseRotateExt() {
            super();
        }

        /**
         * DOCUMENT ME!
         *
         * @param  kMouseEvent  DOCUMENT ME!
         */
        public synchronized void processMouseEvent(MouseEvent kMouseEvent) {
        	
            if (!kMouseEvent.isShiftDown() && kMouseEvent.getButton() != MouseEvent.BUTTON1 ) {
                // System.err.println("super process event");            	
                super.processMouseEvent(kMouseEvent);
            }
           
        }
    }

    /* (non-Javadoc)
     * @see gov.nih.mipav.view.renderer.flythroughview.FlyThroughRenderInterface#record(boolean)
     */
    public void record(boolean bOn) {}
    
    /* (non-Javadoc)
     * @see gov.nih.mipav.view.renderer.flythroughview.FlyThroughRenderInterface#getImage()
     */
    public ModelImage getImage()
    {
        return kImage;
    }
}
