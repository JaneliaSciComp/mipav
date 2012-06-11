package gov.nih.mipav.view.renderer.J3D.surfaceview.brainflattenerview;


import gov.nih.mipav.model.structures.*;
import gov.nih.mipav.view.renderer.J3D.*;
import gov.nih.mipav.view.renderer.J3D.model.structures.*;

import com.sun.j3d.utils.behaviors.mouse.*;
import com.sun.j3d.utils.geometry.*;
import com.sun.j3d.utils.picking.*;
import com.sun.j3d.utils.universe.*;

import java.awt.*;
import java.awt.event.*;

import javax.media.j3d.*;

import javax.swing.*;
import javax.swing.event.*;

import javax.vecmath.*;


/**
 * Cortical analysis applet and viewer. This class contains the static 'main' method to launch the application. This
 * class is an Applet which means that it can be embedded in a browser or it can be attached to a MainFrame when
 * executed as an application.
 */
public class MjCorticalAnalysis extends RenderViewBase implements MouseListener, MouseMotionListener {

    //~ Static fields/initializers -------------------------------------------------------------------------------------

    /** Use serialVersionUID for interoperability. */
    private static final long serialVersionUID = -6130870790192175575L;

    //~ Instance fields ------------------------------------------------------------------------------------------------

    /** DOCUMENT ME! */
    private int[] m_aiTriIndex = null;

    /** Colors for the picked points:. */
    private Color3f[] m_akPickColors = {
        new Color3f(1, 0, 0), new Color3f(0, 1, 0), new Color3f(0, 0, 1), new Color3f(1, 1, 0), new Color3f(0, 1, 1),
        new Color3f(1, 0, 1)
    };

    /** DOCUMENT ME! */
    private Color4f[] m_akTriColors = null;

    /**
     * Flag set to indicate whether curvature colors are rendered on the mesh surface. If not, then average convexity
     * colors are rendered.
     */
    private boolean m_bCurvatureColors = true;

    /** Flag set once inflation processing has been initialized. */
    private boolean m_bInflationInitialized = false;

    /**
     * Mouse events. Setting mousePressed and mouseReleased explicitly when the mouse events are received has deals with
     * getting multiply mouse event notifications for the same mouse press.
     */
    private boolean m_bMousePressed = false;

    /** DOCUMENT ME! */
    private boolean m_bMouseReleased = true;

    /** Turned on when picking with the mouse is enabled:. */
    private boolean m_bPickCorrespondenceEnabled = false;

    /** DOCUMENT ME! */
    private boolean m_bPickPunctureEnabled = false;

    /** number of picked points displayed. */
    private int m_iNumPicked = 0;

    /** DOCUMENT ME! */
    private int m_iRunningNumPicked = 0;

    /** Cortical mesh to be used in rendering scene. */
    private MjCorticalMesh m_kCortical = null;

    /** DOCUMENT ME! */
    private GeometryArray m_kGeometryArrayPlane = null;

    /** DOCUMENT ME! */
    private GeometryArray m_kGeometryArraySphere = null;

    /** DOCUMENT ME! */
    private ModelLUT m_kLUTConvexity = null;

    /** local reference to ModelLUT. */
    private ModelLUT m_kLUTCurvature = null;

    /**
     * BranchGroups for the Meshlines, for deleting and recreating the meshlines when the user changes the number of
     * latitude and longitude lines:.
     */
    private BranchGroup m_kMeshBranchRoot = null;

    /** DOCUMENT ME! */
    private BranchGroup m_kMeshLinesBranchRoot = null;

    /** DOCUMENT ME! */
    private MouseEvent m_kMouseEvent = null;

    /** DOCUMENT ME! */
    private JPanelBrainSurfaceFlattener m_kPanel = null;

    /** DOCUMENT ME! */
    private PickCanvas m_kPickCanvas = null;

    /** DOCUMENT ME! */
    private Shape3D m_kShapeMesh = null;

    /** DOCUMENT ME! */
    private Shape3D m_kShapePlane = null;

    /** DOCUMENT ME! */
    private Shape3D m_kShapeSphere = null;

    /** Switch between displaying the Sphere and the Plane:. */
    private Switch m_kSwitchDisplay = null;

    /** This node contains the latitude/longitude renderings for the mesh which can be enabled/disabled. */
    private Switch m_kSwitchLinesMesh = null;

    /** This node contains the latitude/longitude renderings for the plane which can be enabled/disabled. */
    private Switch m_kSwitchLinesPlane = null;

    /** DOCUMENT ME! */
    private BranchGroup m_kSwitchLinesPlaneBranch = null;

    /** This node contains the latitude/longitude renderings for the sphere which can be enabled/disabled. */
    private Switch m_kSwitchLinesSphere = null;

    /** DOCUMENT ME! */
    private BranchGroup m_kSwitchLinesSphereBranch = null;

    /** Currently selected transform that gets modified by the handler for the keyboard behavior. */
    private TransformGroup m_kTransformGroupActive = null;

    /** DOCUMENT ME! */
    private TransformGroup m_kTransformGroupMesh = null;

    /** DOCUMENT ME! */
    private TransformGroup m_kTransformGroupPlane = null;

    /** This node contains all of the cortical mesh related nodes in the scene which can be manipulated as a group. */
    private TransformGroup m_kTransformGroupPoseMesh = null;

    /** This node contains all of the plane related nodes in the scene which can be manipulated as a group. */
    private TransformGroup m_kTransformGroupPosePlane = null;

    /** This node contains all of the sphere related nodes in the scene which can be manipulated as a group. */
    private TransformGroup m_kTransformGroupPoseSphere = null;

    /** DOCUMENT ME! */
    private TransformGroup m_kTransformGroupSphere = null;

    /**
     * Geometry for the rendering of the cortical mesh is maintained here so that the scene can be updated when the mesh
     * is inflated.
     */
    private ModelTriangleMesh m_kTriangleMesh = null;

    /** DOCUMENT ME! */
    private int MAX_POINTS = 6;

    /** DOCUMENT ME! */
    private int MESH = 2;

    /** DOCUMENT ME! */
    private int PLANE = 1;

    /** DOCUMENT ME! */
    private int SPHERE = 0;

    //~ Constructors ---------------------------------------------------------------------------------------------------

    /**
     * A simple universe is created that contains a viewing platform. A canvas is created and attached to the simple
     * universe and is added to this instance The canvas is used for the 3D rendering. An initially empty scene graph
     * for the application is created and assigned to the universe.
     *
     * @param  _imageA  DOCUMENT ME!
     * @param  _imageB  DOCUMENT ME!
     * @param  _config  DOCUMENT ME!
     * @param  kParent  DOCUMENT ME!
     */
    public MjCorticalAnalysis(ModelImage _imageA, ModelImage _imageB, GraphicsConfiguration _config,
                              ViewJFrameVolumeView kParent) {
        super(_imageA, _imageB, _config);

        /* Create the canvas, place it alone in the panel, and then */
        /* create the simple universe with this canvas. */
        canvas = new VolumeCanvas3D(SimpleUniverse.getPreferredConfiguration());
        canvas.addMouseListener(this);
        canvas.addMouseMotionListener(this);

        universe = new SimpleUniverse(canvas);
        universe.getViewingPlatform().setNominalViewingTransform();

        /* Add and empty root node to the scene graph. */
        objBehaviorBG = new BranchGroup();
        objBehaviorBG.setCapability(BranchGroup.ALLOW_DETACH);
        universe.addBranchGraph(objBehaviorBG);

        m_kPanel = new JPanelBrainSurfaceFlattener(this, imageA, kParent);
    }

    //~ Methods --------------------------------------------------------------------------------------------------------

    /**
     * actionPerformed.
     *
     * @param  e  ActionEvent
     */
    public void actionPerformed(ActionEvent e) { }

    /**
     * Called on setup, when a new triangle mesh is loaded, or when the puncture triangle is selected and the
     * "recalculate conformal" button is pressed by the user:
     */
    public void calculateConformal() {

        /* Commonly used items. */
        Transform3D kTransform = new Transform3D();
        int[] aiConnect = m_kCortical.getConnectivity();
        Color4f[] akColor = m_kCortical.getColors();

        /* conformally map mesh to plane, sphere, and cylinder */
        /* set the user-selected puncture triangle: */
        m_kCortical.setPunctureTriangle(m_aiTriIndex);

        /* Restore previously picked triangle colors: */
        restoreTriColor();
        m_kCortical.computeConformalMapping();

        /* Setup for sphere nodes. */
        m_kTransformGroupPoseSphere = new TransformGroup();
        m_kTransformGroupPoseSphere.setCapability(TransformGroup.ALLOW_TRANSFORM_READ);
        m_kTransformGroupPoseSphere.setCapability(TransformGroup.ALLOW_TRANSFORM_WRITE);
        m_kTransformGroupPoseSphere.setCapability(Group.ALLOW_CHILDREN_EXTEND);
        m_kTransformGroupPoseSphere.getTransform(kTransform);
        m_kTransformGroupPoseSphere.setTransform(kTransform);
        m_kGeometryArraySphere = createGeometry(m_kCortical.getSphereCoordinates(), akColor, aiConnect);

        /* m_kShapeSphere: */
        Geometry kGeometrySphere = m_kGeometryArraySphere;
        m_kShapeSphere = new Shape3D(kGeometrySphere);
        m_kShapeSphere.setCapability(Shape3D.ALLOW_GEOMETRY_WRITE);
        m_kShapeSphere.setCapability(Shape3D.ALLOW_GEOMETRY_READ);
        m_kShapeSphere.setCapability(Geometry.ALLOW_INTERSECT);
        m_kShapeSphere.setPickable(true);
        m_kTransformGroupSphere = new TransformGroup();
        m_kTransformGroupSphere.setCapability(Group.ALLOW_CHILDREN_READ);
        m_kTransformGroupSphere.setCapability(Group.ALLOW_CHILDREN_WRITE);
        m_kTransformGroupSphere.setCapability(Group.ALLOW_CHILDREN_EXTEND);

        m_kTransformGroupSphere.getTransform(kTransform);
        kTransform.setTranslation(getTranslateToCenter(m_kShapeSphere));
        kTransform.setRotation(new AxisAngle4d(1, 0, 0, Math.PI));
        m_kTransformGroupSphere.setTransform(kTransform);
        m_kTransformGroupSphere.addChild(m_kShapeSphere);
        m_kTransformGroupPoseSphere.addChild(m_kTransformGroupSphere);

        /* Setup for plane nodes. */
        m_kTransformGroupPosePlane = new TransformGroup();
        m_kTransformGroupPosePlane.setCapability(TransformGroup.ALLOW_TRANSFORM_READ);
        m_kTransformGroupPosePlane.setCapability(TransformGroup.ALLOW_TRANSFORM_WRITE);
        m_kTransformGroupPosePlane.setCapability(Group.ALLOW_CHILDREN_EXTEND);
        m_kTransformGroupPosePlane.getTransform(kTransform);
        kTransform.setTranslation(new Vector3f(0.0f, 0.0f, 0.0f));
        m_kTransformGroupPosePlane.setTransform(kTransform);
        m_kGeometryArrayPlane = createGeometry(m_kCortical.getCylinderCoordinates(), akColor, aiConnect);
        m_kShapePlane = new Shape3D(m_kGeometryArrayPlane);
        m_kShapePlane.setCapability(Shape3D.ALLOW_GEOMETRY_WRITE);
        m_kShapePlane.setCapability(Shape3D.ALLOW_GEOMETRY_READ);
        m_kShapePlane.setCapability(Geometry.ALLOW_INTERSECT);
        m_kShapePlane.setPickable(true);
        m_kTransformGroupPlane = new TransformGroup();
        m_kTransformGroupPlane.setCapability(Group.ALLOW_CHILDREN_READ);
        m_kTransformGroupPlane.setCapability(Group.ALLOW_CHILDREN_WRITE);
        m_kTransformGroupPlane.setCapability(Group.ALLOW_CHILDREN_EXTEND);

        m_kTransformGroupPlane.getTransform(kTransform);
        kTransform.setTranslation(getTranslateToCenter(m_kShapePlane));
        kTransform.setRotation(new AxisAngle4d(1, 0, 0, Math.PI));
        m_kTransformGroupPlane.setTransform(kTransform);
        m_kTransformGroupPlane.addChild(m_kShapePlane);
        m_kTransformGroupPlane.setCapability(Group.ALLOW_CHILDREN_WRITE);
        m_kTransformGroupPlane.setCapability(Group.ALLOW_CHILDREN_READ);
        m_kTransformGroupPosePlane.addChild(m_kTransformGroupPlane);
        m_kTransformGroupPosePlane.setCapability(Group.ALLOW_CHILDREN_WRITE);
        m_kTransformGroupPosePlane.setCapability(Group.ALLOW_CHILDREN_READ);


        /* SwitchGroup Display : */
        m_kSwitchDisplay = new Switch();
        m_kSwitchDisplay.setCapability(Switch.ALLOW_SWITCH_READ);
        m_kSwitchDisplay.setCapability(Switch.ALLOW_SWITCH_WRITE);
        m_kSwitchDisplay.setWhichChild(Switch.CHILD_ALL);
        m_kSwitchDisplay.addChild(m_kTransformGroupPoseSphere);
        m_kSwitchDisplay.addChild(m_kTransformGroupPosePlane);
        m_kSwitchDisplay.setWhichChild(1);


        /* Setup for grouping all the nodes in the scene. */
        sceneRootTG = new TransformGroup();
        sceneRootTG.setCapability(TransformGroup.ALLOW_TRANSFORM_READ);
        sceneRootTG.setCapability(TransformGroup.ALLOW_TRANSFORM_WRITE);
        sceneRootTG.setCapability(TransformGroup.ALLOW_BOUNDS_READ);
        sceneRootTG.setCapability(TransformGroup.ALLOW_CHILDREN_EXTEND);
        sceneRootTG.setCapability(TransformGroup.ALLOW_CHILDREN_WRITE);
        sceneRootTG.addChild(m_kSwitchDisplay);

        /* Begin testing of algorithms. */
        int iVQ = m_kCortical.getVQuantity();
        int iTQ = m_kCortical.getTQuantity();

        /* Compute the bounds for the scene. */
        /* Setup the range of the scene that we are interested in */
        /* specified in a radius centered at the eye coordinates origin. */
        BoundingSphere kSceneBounds = new BoundingSphere(sceneRootTG.getBounds());
        float fSceneRadius = (float) kSceneBounds.getRadius() * 100.0f;


        /* Set the far clipping distance so that the volume can be seen. */
        /* Don't forget that the behaviors won't work unless the activation */
        /* radius for the ViewPlatform intersects the scheduling bounds */
        /* for that behavior.  Make sure the clipping distance is */
        /* the same for all projection policies. */
        ViewPlatform kViewPlatform = universe.getViewingPlatform().getViewPlatform();
        kViewPlatform.setActivationRadius(fSceneRadius);

        View kView = universe.getViewer().getView();
        kView.setProjectionPolicy(View.PERSPECTIVE_PROJECTION);
        kView.setBackClipDistance(fSceneRadius);
        kView.setFieldOfView(Math.PI / 2.5);

        /* Mouse Behaviors: */
        /* Create the rotate behavior node */
        MouseRotate behavior = new MouseRotate(canvas, sceneRootTG);
        behavior.setupCallback(this);
        behavior.setSchedulingBounds(kSceneBounds);
        behavior.setFactor(0.005);
        behavior.setEnable(true);
        sceneRootTG.addChild(behavior);

        /* Create the zoom behavior node */
        MouseZoom behaviorZoom = new MouseZoom(canvas, sceneRootTG);
        behaviorZoom.setupCallback(this);
        behaviorZoom.setSchedulingBounds(kSceneBounds);
        behaviorZoom.setFactor(0.005);
        sceneRootTG.addChild(behaviorZoom);

        /* Create the translate behavior node */
        MouseTranslate behaviorTranslate = new MouseTranslate(canvas, sceneRootTG);
        behaviorTranslate.setupCallback(this);
        behaviorTranslate.setSchedulingBounds(kSceneBounds);
        behaviorTranslate.setFactor(0.005);
        sceneRootTG.addChild(behaviorTranslate);

        /* Replace the compiled root node of the scene graph. */
        BranchGroup kNewSceneRoot = new BranchGroup();
        kNewSceneRoot.setCapability(BranchGroup.ALLOW_CHILDREN_EXTEND);
        kNewSceneRoot.setCapability(BranchGroup.ALLOW_CHILDREN_WRITE);
        kNewSceneRoot.setCapability(BranchGroup.ALLOW_DETACH);
        kNewSceneRoot.addChild(sceneRootTG);
        universe.getLocale().replaceBranchGraph(objBehaviorBG, kNewSceneRoot);
        objBehaviorBG = kNewSceneRoot;

        setupLight();

        /* Certain user interactions affect the pose of the */
        /* following collection of object renderings... */
        m_kTransformGroupActive = sceneRootTG;

        m_kPickCanvas = new PickCanvas(canvas, objBehaviorBG);
        m_kPickCanvas.setMode(PickTool.GEOMETRY_INTERSECT_INFO);
        m_kPickCanvas.setTolerance(0.0f);

        m_iNumPicked = 0;
        m_iRunningNumPicked = 0;
    }

    /**
     * toggle between display of mean curvature-based colors and average convexity-based colors.
     *
     * @return  DOCUMENT ME!
     */
    public boolean displayConvexityColors() {

        if (m_bCurvatureColors && (null != m_kCortical.getAvrConvexity())) {
            m_bCurvatureColors = false;
            setColorsLUT();
        }

        return m_bCurvatureColors;
    }

    /**
     * toggle between display of mean curvature-based colors and average convexity-based colors.
     *
     * @return  DOCUMENT ME!
     */
    public boolean displayCurvatureColors() {

        if (!m_bCurvatureColors) {
            m_bCurvatureColors = true;
            setColorsLUT();
        }

        return m_bCurvatureColors;
    }

    /**
     * Switch to displaying the plane:
     */
    public void displayPlane() {
        m_kSwitchDisplay.setWhichChild(PLANE);
    }

    /**
     * Switch to displaying the sphere:
     */
    public void displaySphere() {
        m_kSwitchDisplay.setWhichChild(SPHERE);
    }

    /**
     * DOCUMENT ME!
     */
    public void disposeLocal() {

        if (m_kPanel != null) {
            m_kPanel = null;
        }

        if (m_kTransformGroupPoseMesh != null) {
            m_kTransformGroupPoseMesh = null;
        }

        if (m_kTransformGroupPoseSphere != null) {
            m_kTransformGroupPoseSphere = null;
        }

        if (m_kTransformGroupPosePlane != null) {
            m_kTransformGroupPosePlane = null;
        }

        if (m_kTransformGroupActive != null) {
            m_kTransformGroupActive = null;
        }

        if (m_kSwitchLinesMesh != null) {
            m_kSwitchLinesMesh = null;
        }

        if (m_kSwitchLinesSphere != null) {
            m_kSwitchLinesSphere = null;
        }

        if (m_kSwitchLinesPlane != null) {
            m_kSwitchLinesPlane = null;
        }

        if (m_kSwitchDisplay != null) {
            m_kSwitchDisplay = null;
        }

        if (m_kTriangleMesh != null) {
            m_kTriangleMesh = null;
        }

        if (m_kGeometryArraySphere != null) {
            m_kGeometryArraySphere = null;
        }

        if (m_kGeometryArrayPlane != null) {
            m_kGeometryArrayPlane = null;
        }

        if (m_kShapeMesh != null) {
            m_kShapeMesh = null;
        }

        if (m_kShapeSphere != null) {
            m_kShapeSphere = null;
        }

        if (m_kShapePlane != null) {
            m_kShapePlane = null;
        }

        if (m_kTransformGroupMesh != null) {
            m_kTransformGroupMesh = null;
        }

        if (m_kTransformGroupSphere != null) {
            m_kTransformGroupSphere = null;
        }

        if (m_kTransformGroupPlane != null) {
            m_kTransformGroupPlane = null;
        }

        if (m_kCortical != null) {
            m_kCortical = null;
        }

        if (m_kLUTCurvature != null) {
            m_kLUTCurvature = null;
        }

        if (m_kLUTConvexity != null) {
            m_kLUTConvexity = null;
        }

        if (m_kPickCanvas != null) {
            m_kPickCanvas = null;
        }

        if (m_kMouseEvent != null) {
            m_kMouseEvent = null;
        }

        if (m_kMeshBranchRoot != null) {
            m_kMeshBranchRoot = null;
        }

        if (m_kMeshLinesBranchRoot != null) {
            m_kMeshLinesBranchRoot = null;
        }

        if (m_kSwitchLinesSphereBranch != null) {
            m_kSwitchLinesSphereBranch = null;
        }

        if (m_kSwitchLinesPlaneBranch != null) {
            m_kSwitchLinesPlaneBranch = null;
        }

        if (m_akTriColors != null) {
            m_akTriColors = null;
        }

        if (m_aiTriIndex != null) {
            m_aiTriIndex = null;
        }

        if (universe != null) {
            universe.removeAllLocales();
            universe = null;
        }
    }

    /**
     * Draw the user-selected point, either as a sphere on the triangle mesh, or as a black triangle, depending on which
     * picking is enabled:
     *
     * @param  kStart   DOCUMENT ME!
     * @param  aiIndex  DOCUMENT ME!
     * @param  iWhich   DOCUMENT ME!
     */
    public void drawPicked(Point3f kStart, int[] aiIndex, int iWhich) {

        if (m_bPickCorrespondenceEnabled) {
            drawPoint(kStart, aiIndex, iWhich);
        } else if (m_bPickPunctureEnabled) {
            drawTriangle(kStart, aiIndex, iWhich);
        }
    }

    /**
     * DOCUMENT ME!
     *
     * @return  DOCUMENT ME!
     */
    public JPanel getMainPanel() {
        return m_kPanel.getMainPanel();
    }

    /**
     * DOCUMENT ME!
     *
     * @return  DOCUMENT ME!
     */
    public float getMaxAverageConvexity() {
        return m_kCortical.getMaxAvrConvexity();
    }

    /**
     * return the max mean curvature:
     *
     * @return  DOCUMENT ME!
     */
    public float getMaxCurvature() {
        return m_kCortical.getMaxMeanCurvature();
    }

    /**
     * Display the mesh in the SurfaceRenderer:
     *
     * @return  a Branchgroup containing a copy of the ModelTriangleMesh
     */
    public BranchGroup getMesh() {

        if (m_kMeshBranchRoot == null) {
            m_kMeshBranchRoot = new BranchGroup();
            m_kMeshBranchRoot.setCapability(BranchGroup.ALLOW_DETACH);
            m_kMeshBranchRoot.setCapability(Group.ALLOW_CHILDREN_READ);
            m_kMeshBranchRoot.setCapability(Group.ALLOW_CHILDREN_WRITE);
            m_kMeshBranchRoot.setCapability(Node.ALLOW_COLLIDABLE_WRITE);
            m_kMeshBranchRoot.setCapability(Node.ALLOW_COLLIDABLE_READ);
            m_kMeshBranchRoot.addChild(m_kShapeMesh.cloneTree());
        }

        return m_kMeshBranchRoot;
    }

    /**
     * Display the mesh lines in the SurfaceRenderer:
     *
     * @return  a BranchGroup containing a copy of the TriangleMesh Lat/Lon Lines
     */
    public BranchGroup getMeshLines() {

        if (m_kMeshLinesBranchRoot == null) {
            Transform3D kTransform = new Transform3D();
            TransformGroup kTG = new TransformGroup();
            kTG.setTransform(kTransform);
            kTG.addChild(m_kSwitchLinesMesh);

            m_kMeshLinesBranchRoot = new BranchGroup();
            m_kMeshLinesBranchRoot.setCapability(BranchGroup.ALLOW_DETACH);
            m_kMeshLinesBranchRoot.setCapability(Group.ALLOW_CHILDREN_READ);
            m_kMeshLinesBranchRoot.setCapability(Group.ALLOW_CHILDREN_WRITE);
            m_kMeshLinesBranchRoot.setCapability(Node.ALLOW_COLLIDABLE_WRITE);
            m_kMeshLinesBranchRoot.setCapability(Node.ALLOW_COLLIDABLE_READ);
            m_kMeshLinesBranchRoot.addChild(kTG);
        }

        return m_kMeshLinesBranchRoot;
    }

    /**
     * DOCUMENT ME!
     *
     * @return  DOCUMENT ME!
     */
    public float getMinAverageConvexity() {
        return m_kCortical.getMinAvrConvexity();
    }

    /**
     * return the min mean curvature:
     *
     * @return  DOCUMENT ME!
     */
    public float getMinCurvature() {
        return m_kCortical.getMinMeanCurvature();
    }


    /**
     * Gets the scene state in terms of slices visible, which slice number, etc.
     *
     * @return  A private object extension specific to the class that extends this one.
     */
    public Object getSceneState() {
        return null;
    }

    /**
     * Display the mesh in the SurfaceRenderer:
     *
     * @return  DOCUMENT ME!
     */
    public ModelTriangleMesh getTMesh() {
        return m_kTriangleMesh;
    }

    /**
     * perform inflation step and then render shapes using the average convexity colors.
     *
     * @return  DOCUMENT ME!
     */
    public boolean inflation() {

        if (!m_bInflationInitialized) {
            m_bInflationInitialized = true;
            m_kCortical.doInflation(0);
        }

        m_kCortical.doInflation(1);
        m_kCortical.doInflation(2);

        // m_bCurvatureColors = false;
        m_kTriangleMesh.setCoordinates(0, m_kCortical.getPoints());

        return m_bCurvatureColors;
    }

    /**
     * Return whether or not picking correspondence points is enabled:
     *
     * @return  DOCUMENT ME!
     */
    public boolean isCorrespondencePickEnabled() {
        return m_bPickCorrespondenceEnabled;
    }

    /**
     * Returns whether or not picking is enabled:
     *
     * @return  DOCUMENT ME!
     */
    public boolean isPickEnabled() {
        return m_bPickCorrespondenceEnabled | m_bPickPunctureEnabled;
    }

    /**
     * Return whether or not picking the puncture triangle is enabled:
     *
     * @return  DOCUMENT ME!
     */
    public boolean isPuncturePickEnabled() {
        return m_bPickPunctureEnabled;
    }


    /**
     * DOCUMENT ME!
     *
     * @param  kEvent  the mouse event generated by a mouse clicked
     */
    public void mouseClicked(MouseEvent kEvent) { /* stub */
    }

    /**
     * DOCUMENT ME!
     *
     * @param  kEvent  the mouse event generated by a mouse drag
     */
    public void mouseDragged(MouseEvent kEvent) { }

    /**
     * DOCUMENT ME!
     *
     * @param  kEvent  the mouse event generated by a mouse entered
     */
    public void mouseEntered(MouseEvent kEvent) { /* stub */
    }

    /**
     * DOCUMENT ME!
     *
     * @param  kEvent  the mouse event generated by a mouse exit
     */
    public void mouseExited(MouseEvent kEvent) { /* stub */
    }

    /**
     * DOCUMENT ME!
     *
     * @param  kEvent  the event generated by a mouse movement
     */
    public void mouseMoved(MouseEvent kEvent) { }

    /**
     * DOCUMENT ME!
     *
     * @param  kMouseEvent  the mouse event generated by a mouse press
     */
    public void mousePressed(MouseEvent kMouseEvent) {

        /* Only capture mouse events when drawing the geodesic curve is
         * enabled, and only when the control key is down and the left mouse
         * button is pressed. */
        if ((m_bPickCorrespondenceEnabled | m_bPickPunctureEnabled) && kMouseEvent.isControlDown()) {

            /* m_bMousePressed and m_bMouseReleased are set explicitly to
             * prevent multiple mouse clicks at the same location.  If the mouse has been released, then set
             * mousePressed to true and save the location of the mouse event.
             */
            if (m_bMouseReleased == true) {

                if ((kMouseEvent.getButton() == MouseEvent.BUTTON1)) {
                    m_bMousePressed = true;
                    m_bMouseReleased = false;

                    m_kMouseEvent = kMouseEvent;
                }
            }
        }

    }

    /**
     * DOCUMENT ME!
     *
     * @param  kEvent  the mouse event generated by a mouse release
     */
    public void mouseReleased(MouseEvent kEvent) {

        /* If the mouse has been pressed, but not released: */
        if (m_bMousePressed && !m_bMouseReleased) {
            m_bMousePressed = false;
            m_bMouseReleased = true;

            /* If the pickCanvas is null, then do not try to pick */
            if (m_kPickCanvas == null) {
                return;
            }

            /* Set the location for picking that was stored when the mouse was
             * presed: */
            m_kPickCanvas.setShapeLocation(m_kMouseEvent);

            PickResult kPickResult = null;

            /* Try to get the closest picked polygon, catch the
             * javax.media.j3d.CapabilityNotSetException. */
            try {
                kPickResult = m_kPickCanvas.pickClosest();
            } catch (javax.media.j3d.CapabilityNotSetException e) {
                return;
            }

            /* If the pickResult is not null, mark the picked point and, if
             * this is the second point in a sequence, then draw the geodesic
             * curve. */
            if (kPickResult != null) {

                /* Pick the first intersection since we executed a pick
                 * closest. */
                int iClosestPick = 0;
                int iWhich = m_kSwitchDisplay.getWhichChild();

                if (iWhich == PLANE) {
                    float fMinDistance = Float.MAX_VALUE;

                    for (int i = 0; i < kPickResult.numIntersections(); i++) {
                        PickIntersection kPick = kPickResult.getIntersection(i);

                        /* Get the coordinates of the picked point on the mesh. */
                        Point3f kPickPoint = new Point3f(kPick.getPointCoordinates());

                        /* Get the triangle indices of the triangle that that
                         * pickPoint falls in:  */
                        int[] aiIndex = kPick.getPrimitiveCoordinateIndices();

                        float fDistance = closestPlanePointIndex(kPickPoint, aiIndex);

                        if (fDistance < fMinDistance) {
                            fMinDistance = fDistance;
                            iClosestPick = i;
                        }
                    }
                }

                PickIntersection kPick = kPickResult.getIntersection(iClosestPick);

                /* Get the coordinates of the picked point on the mesh. */
                Point3f kPickPoint = new Point3f(kPick.getPointCoordinates());

                /* Get the triangle indices of the triangle that that
                 * pickPoint falls in:  */
                int[] aiIndex = kPick.getPrimitiveCoordinateIndices();

                drawPicked(kPickPoint, aiIndex, iWhich);
            }
        }
    }

    /**
     * Remove all points drawn on the mesh, sphere, and plane:
     */
    public void removePoints() {

        for (int i = 0; i < m_iNumPicked; i++) {
            m_kTransformGroupSphere.removeChild(2);
            m_kTransformGroupPlane.removeChild(2);
            m_kTransformGroupMesh.removeChild(1);
        }

        m_iNumPicked = 0;
        m_iRunningNumPicked = 0;
    }

    /**
     * Sets the alpha blending of parameter for two image displaying.
     *
     * @param  value  amount [0,100] that is the percentage of Image A to be displayed and (1-percentage) of Image B to
     *                be displayed
     */
    public void setAlphaBlend(int value) { }

    /**
     * Used by the JPanelMouse the redisplay the recorded actions.
     *
     * @param  scene  Object
     */
    public void setGUI(Object scene) { }

    /**
     * DOCUMENT ME!
     *
     * @param  kLUT  DOCUMENT ME!
     */
    public void setLUTConvexity(ModelLUT kLUT) {

        if (m_kLUTConvexity != null) {
            m_kLUTConvexity = null;
        }

        m_kLUTConvexity = kLUT;

        setColorsLUT();
    }

    /**
     * Update the colormap based on the LUT:
     *
     * @param  kLUT  DOCUMENT ME!
     */
    public void setLUTCurvature(ModelLUT kLUT) {

        if (m_kLUTCurvature != null) {
            m_kLUTCurvature = null;
        }

        m_kLUTCurvature = kLUT;

        setColorsLUT();
    }

    /**
     * setRGBTA.
     *
     * @param  RGBT  ModelRGB
     */
    public void setRGBTA(ModelRGB RGBT) { }

    /**
     * setRGBTB.
     *
     * @param  RGBT  ModelRGB
     */
    public void setRGBTB(ModelRGB RGBT) { }

    /**
     * setSlice.
     *
     * @param  slice  int
     */
    public void setSlice(int slice) { }

    /**
     * setTimeSlice.
     *
     * @param  tSlice  int
     */
    public void setTimeSlice(int tSlice) { }

    /**
     * Perform all initialization prior to executing. Loads the sample mesh surface and sets up the rendering of the
     * scene.
     *
     * @param  kTriangleMesh  DOCUMENT ME!
     */
    public void setup(ModelTriangleMesh kTriangleMesh) {

        /* reset inflation initialization: */
        m_bInflationInitialized = false;

        m_kCortical = new MjCorticalMesh(kTriangleMesh.getVertexCount(), kTriangleMesh.getVertexCopy(),
                                         kTriangleMesh.getIndexCount() / 3, kTriangleMesh.getIndexCopy());

        /* Commonly used items. */
        Transform3D kTransform = new Transform3D();
        int[] aiConnect = m_kCortical.getConnectivity();
        Color4f[] akColor = m_kCortical.getColors();

        /* cortical mesh initializations */
        m_kCortical.computeMeanCurvature();

        /* Setup for mesh: */
        m_kMeshBranchRoot = null;
        m_kTransformGroupPoseMesh = new TransformGroup();
        m_kTransformGroupPoseMesh.setCapability(TransformGroup.ALLOW_TRANSFORM_READ);
        m_kTransformGroupPoseMesh.setCapability(TransformGroup.ALLOW_TRANSFORM_WRITE);
        m_kTransformGroupPoseMesh.getTransform(kTransform);
        m_kTransformGroupPoseMesh.setTransform(kTransform);

        /* TriangleMesh: */
        m_kTriangleMesh = createTriangleMesh(m_kCortical.getPoints(), kTriangleMesh.getNormalCopy(), akColor,
                                             aiConnect);
        /* For compatibliity: loading the mesh into the surface renderer,
         * appearance, polygon attributes must be non-null: */
        Appearance kMeshAppearance = new Appearance();
        kMeshAppearance.setPolygonAttributes( new PolygonAttributes() );
        kMeshAppearance.setCapability( Appearance.ALLOW_TRANSPARENCY_ATTRIBUTES_WRITE );
        kMeshAppearance.setCapability( Appearance.ALLOW_TRANSPARENCY_ATTRIBUTES_READ );
        kMeshAppearance.setCapability(Appearance.ALLOW_MATERIAL_READ);
        kMeshAppearance.setCapability(Appearance.ALLOW_POLYGON_ATTRIBUTES_WRITE);
        kMeshAppearance.setCapability(Appearance.ALLOW_POLYGON_ATTRIBUTES_READ);
        m_kShapeMesh = new Shape3D(m_kTriangleMesh, kMeshAppearance);

        Material kMaterial = new Material();
        kMaterial.setLightingEnable(true);
        m_kShapeMesh.getAppearance().setMaterial(kMaterial);
        m_kShapeMesh.setCapability(Shape3D.ALLOW_GEOMETRY_WRITE);
        m_kShapeMesh.setCapability(Shape3D.ALLOW_GEOMETRY_READ);
        m_kShapeMesh.setCapability(Shape3D.ALLOW_APPEARANCE_WRITE);
        m_kShapeMesh.setCapability(Shape3D.ALLOW_APPEARANCE_READ);
        m_kShapeMesh.setCapability(Geometry.ALLOW_INTERSECT);
        m_kShapeMesh.setPickable(true);
        m_kTransformGroupMesh = new TransformGroup();
        m_kTransformGroupMesh.setCapability(Group.ALLOW_CHILDREN_READ);
        m_kTransformGroupMesh.setCapability(Group.ALLOW_CHILDREN_WRITE);
        m_kTransformGroupMesh.setCapability(Group.ALLOW_CHILDREN_EXTEND);
        m_kTransformGroupMesh.addChild(m_kShapeMesh);
        m_kTransformGroupPoseMesh.addChild(m_kTransformGroupMesh);

        /* calculate the conformal mapping of mesh to the flattened plane and
         * sphere maps, and setup the display for the plane and sphere: */
        calculateConformal();
    }

    /**
     * Called when the mesh is created, or when the number of latitude or longitude lines is changed by the user. When
     * the user changes the number of latitude or longitude lines, the meshes are removed from the scene and recreated.
     *
     * @param  iNumLat  DOCUMENT ME!
     * @param  iNumLon  DOCUMENT ME!
     */
    public void setupLatLon(int iNumLat, int iNumLon) {

        /* Remove existing meshes from the scene: */
        if (m_kSwitchLinesSphereBranch != null) {
            m_kTransformGroupSphere.removeChild(m_kSwitchLinesSphereBranch);
        }

        if (m_kSwitchLinesPlaneBranch != null) {
            m_kTransformGroupPlane.removeChild(m_kSwitchLinesPlaneBranch);
        }

        /* bias for latitude/longitude to handle z-buffer fighting */
        float fMBias = 0.01f;
        float fSBias = 0.01f;
        float fPBias = 0.01f;

        /* iNumLat latitute and iNumLon longitude per object */
        Point3f[] akPolylinePointsMesh = new Point3f[0];
        Point3f[] akPolylinePointsSphere = new Point3f[0];
        Point3f[] akPolylinePointsPlane = new Point3f[0];
        int iNumPoints = 1 + (2 * (int) (iNumLat / 2)) + iNumLon;
        int[] aiPointsMesh = new int[iNumPoints];
        int[] aiPointsSphere = new int[iNumPoints];
        int[] aiPointsPlane = new int[iNumPoints];
        int iPolyline = 0;

        /* compute draw latitude lines */
        for (int i = -(int) (iNumLat / 2); i <= (int) (iNumLat / 2); i++) {
            float fZNormal = i / (float) (1 + (iNumLat / 2));

            MjCorticalMesh.Polylines kPolylines = m_kCortical.getLatitude(fZNormal, fMBias, fSBias, fPBias);

            /* how many points for this latitude? */
            aiPointsMesh[iPolyline] = kPolylines.akMVertex.length;
            aiPointsSphere[iPolyline] = kPolylines.akSVertex.length;
            aiPointsPlane[iPolyline] = kPolylines.akPVertex.length;
            ++iPolyline;

            /* append the vertices */
            akPolylinePointsMesh = combine(akPolylinePointsMesh, kPolylines.akMVertex);
            akPolylinePointsSphere = combine(akPolylinePointsSphere, kPolylines.akSVertex);
            akPolylinePointsPlane = combine(akPolylinePointsPlane, kPolylines.akPVertex);
        }

        /* compute longitude lines */
        for (int i = 0; i <= (iNumLon - 1); i++) {
            float fAngle = i * 2.0f * (float) Math.PI / (float) (iNumLon - 1);

            MjCorticalMesh.Polylines kPolylines = m_kCortical.getLongitude(fAngle, fMBias, fSBias, fPBias);

            /* how many points for this latitude? */
            aiPointsMesh[iPolyline] = kPolylines.akMVertex.length;
            aiPointsSphere[iPolyline] = kPolylines.akSVertex.length;
            aiPointsPlane[iPolyline] = kPolylines.akPVertex.length;
            ++iPolyline;

            /* append the vertices */
            akPolylinePointsMesh = combine(akPolylinePointsMesh, kPolylines.akMVertex);
            akPolylinePointsSphere = combine(akPolylinePointsSphere, kPolylines.akSVertex);
            akPolylinePointsPlane = combine(akPolylinePointsPlane, kPolylines.akPVertex);
        }

        /* Common appearance for latitude/longitude lines. */
        Appearance kAppearanceLines = new Appearance();

        /* Render mesh latitude and longitude lines. */
        /* Can be enabled/disabled through a Switch node. */
        m_kMeshLinesBranchRoot = null;

        LineStripArray kLinesMesh = new LineStripArray(akPolylinePointsMesh.length, LineStripArray.COORDINATES,
                                                       aiPointsMesh);
        kLinesMesh.setCoordinates(0, akPolylinePointsMesh);

        Shape3D kShapeLinesMesh = new Shape3D(kLinesMesh, kAppearanceLines);
        kShapeLinesMesh.setPickable(false);
        m_kSwitchLinesMesh = new Switch();
        m_kSwitchLinesMesh.setCapability(Switch.ALLOW_SWITCH_READ);
        m_kSwitchLinesMesh.setCapability(Switch.ALLOW_SWITCH_WRITE);
        m_kSwitchLinesMesh.setWhichChild(Switch.CHILD_ALL);
        m_kSwitchLinesMesh.addChild(kShapeLinesMesh);

        /* Render sphere latitude and longitude lines */
        LineStripArray kLinesSphere = new LineStripArray(akPolylinePointsSphere.length, LineStripArray.COORDINATES,
                                                         aiPointsSphere);
        kLinesSphere.setCoordinates(0, akPolylinePointsSphere);

        Shape3D kShapeLinesSphere = new Shape3D(kLinesSphere, kAppearanceLines);
        kShapeLinesSphere.setPickable(false);
        m_kSwitchLinesSphere = new Switch();
        m_kSwitchLinesSphere.setCapability(Switch.ALLOW_SWITCH_READ);
        m_kSwitchLinesSphere.setCapability(Switch.ALLOW_SWITCH_WRITE);
        m_kSwitchLinesSphere.setWhichChild(Switch.CHILD_ALL);
        m_kSwitchLinesSphere.addChild(kShapeLinesSphere);
        m_kSwitchLinesSphereBranch = new BranchGroup();
        m_kSwitchLinesSphereBranch.setCapability(BranchGroup.ALLOW_DETACH);
        m_kSwitchLinesSphereBranch.addChild(m_kSwitchLinesSphere);
        m_kTransformGroupSphere.addChild(m_kSwitchLinesSphereBranch);

        /* Render plane latitude and longitude lines */
        LineStripArray kLinesPlane = new LineStripArray(akPolylinePointsPlane.length, LineStripArray.COORDINATES,
                                                        aiPointsPlane);
        kLinesPlane.setCoordinates(0, akPolylinePointsPlane);

        Shape3D kShapeLinesPlane = new Shape3D(kLinesPlane, kAppearanceLines);
        kShapeLinesPlane.setPickable(false);
        m_kSwitchLinesPlane = new Switch();
        m_kSwitchLinesPlane.setCapability(Switch.ALLOW_SWITCH_READ);
        m_kSwitchLinesPlane.setCapability(Switch.ALLOW_SWITCH_WRITE);
        m_kSwitchLinesPlane.setWhichChild(Switch.CHILD_ALL);
        m_kSwitchLinesPlane.addChild(kShapeLinesPlane);
        m_kSwitchLinesPlaneBranch = new BranchGroup();
        m_kSwitchLinesPlaneBranch.setCapability(BranchGroup.ALLOW_DETACH);
        m_kSwitchLinesPlaneBranch.addChild(m_kSwitchLinesPlane);
        m_kTransformGroupPlane.addChild(m_kSwitchLinesPlaneBranch);
    }

    /**
     * stateChanged.
     *
     * @param  e  ChangeEvent
     */
    public void stateChanged(ChangeEvent e) { }

    /**
     * Toggle between displaying the latitude and longitude lines:
     */
    public void toggleLatLonLines() {
        boolean bEnabled = Switch.CHILD_ALL == m_kSwitchLinesMesh.getWhichChild();
        int iWhichChild = bEnabled ? Switch.CHILD_NONE : Switch.CHILD_ALL;
        m_kSwitchLinesMesh.setWhichChild(iWhichChild);
        m_kSwitchLinesSphere.setWhichChild(iWhichChild);
        m_kSwitchLinesPlane.setWhichChild(iWhichChild);
    }

    /**
     * Enables picking with the mouse and drawing the curve on the mesh.
     */
    public void togglePickCorrespondence() {
        m_bPickCorrespondenceEnabled = !m_bPickCorrespondenceEnabled;
    }

    /**
     * Enables picking with the mouse and drawing the curve on the mesh.
     */
    public void togglePickPuncture() {
        m_bPickPunctureEnabled = !m_bPickPunctureEnabled;

        if (m_bPickPunctureEnabled == false) {

            /* Restore previously picked triangle colors: */
            restoreTriColor();
        }
    }

    /**
     * transformChanged.
     *
     * @param  type       int
     * @param  transform  Transform3D
     */
    public void transformChanged(int type, Transform3D transform) {
        sceneRootTG.setTransform(transform);
    }

    /**
     * updateImageExtents.
     *
     * @return  boolean
     */
    public boolean updateImageExtents() {
        return false;
    }

    /**
     * updateImages.
     *
     * @return  boolean
     */
    public boolean updateImages() {
        return false;
    }

    /**
     * updateImages.
     *
     * @param   flag  boolean
     *
     * @return  boolean
     */
    public boolean updateImages(boolean flag) {
        return false;
    }

    /**
     * updateImages.
     *
     * @param   LUTa        ModelLUT
     * @param   LUTb        ModelLUT
     * @param   flag        boolean
     * @param   interpMode  int
     *
     * @return  boolean
     */
    public boolean updateImages(ModelLUT LUTa, ModelLUT LUTb, boolean flag, int interpMode) {
        return false;
    }

    /**
     * DOCUMENT ME!
     *
     * @throws  Throwable  DOCUMENT ME!
     */
    protected void finalize() throws Throwable {
        disposeLocal();
        super.finalize();
    }

    /**
     * DOCUMENT ME!
     *
     * @param   akPointA  DOCUMENT ME!
     * @param   akPointB  DOCUMENT ME!
     *
     * @return  DOCUMENT ME!
     */
    private static Point3f[] combine(Point3f[] akPointA, Point3f[] akPointB) {
        Point3f[] akPointCombined = new Point3f[akPointA.length + akPointB.length];
        int iIndex = 0;

        for (int i = 0; i < akPointA.length; i++) {
            akPointCombined[iIndex++] = akPointA[i];
        }

        for (int i = 0; i < akPointB.length; i++) {
            akPointCombined[iIndex++] = akPointB[i];
        }

        return akPointCombined;
    }

    /**
     * DOCUMENT ME!
     *
     * @param   akPoint    DOCUMENT ME!
     * @param   akColor    DOCUMENT ME!
     * @param   aiConnect  DOCUMENT ME!
     *
     * @return  DOCUMENT ME!
     */
    private static GeometryArray createGeometry(Point3f[] akPoint, Color4f[] akColor, int[] aiConnect) {
        GeometryInfo kGeometryInfo = new GeometryInfo(GeometryInfo.TRIANGLE_ARRAY);
        kGeometryInfo.setUseCoordIndexOnly(true);
        kGeometryInfo.setCoordinates(akPoint);
        kGeometryInfo.setColors(akColor);
        kGeometryInfo.setCoordinateIndices(aiConnect);

        GeometryArray kGeometry = kGeometryInfo.getIndexedGeometryArray();

        kGeometry.setCapability(IndexedGeometryArray.ALLOW_COORDINATE_INDEX_READ);
        kGeometry.setCapability(GeometryArray.ALLOW_COORDINATE_READ);
        kGeometry.setCapability(GeometryArray.ALLOW_COORDINATE_WRITE);
        kGeometry.setCapability(GeometryArray.ALLOW_COLOR_WRITE);
        kGeometry.setCapability(GeometryArray.ALLOW_COLOR_READ);
        kGeometry.setCapability(GeometryArray.ALLOW_NORMAL_READ);
        kGeometry.setCapability(GeometryArray.ALLOW_NORMAL_WRITE);
        kGeometry.setCapability(GeometryArray.ALLOW_COUNT_READ);
        kGeometry.setCapability(GeometryArray.ALLOW_COUNT_READ);
        kGeometry.setCapability(GeometryArray.ALLOW_FORMAT_READ);
        kGeometry.setCapability(GeometryArray.ALLOW_INTERSECT);

        return kGeometry;
    }

    /**
     * DOCUMENT ME!
     *
     * @param   akPoint    DOCUMENT ME!
     * @param   akNormal   DOCUMENT ME!
     * @param   akColor    DOCUMENT ME!
     * @param   aiConnect  DOCUMENT ME!
     *
     * @return  DOCUMENT ME!
     */
    private static ModelTriangleMesh createTriangleMesh(Point3f[] akPoint, Vector3f[] akNormal, Color4f[] akColor,
                                                        int[] aiConnect) {
        ModelTriangleMesh kTMesh = new ModelTriangleMesh(akPoint, akNormal, akColor, aiConnect);
        kTMesh.setCapability(IndexedGeometryArray.ALLOW_COORDINATE_INDEX_READ);
        kTMesh.setCapability(GeometryArray.ALLOW_COORDINATE_READ);
        kTMesh.setCapability(GeometryArray.ALLOW_COORDINATE_WRITE);
        kTMesh.setCapability(GeometryArray.ALLOW_COLOR_WRITE);
        kTMesh.setCapability(GeometryArray.ALLOW_COLOR_READ);
        kTMesh.setCapability(GeometryArray.ALLOW_NORMAL_READ);
        kTMesh.setCapability(GeometryArray.ALLOW_NORMAL_WRITE);
        kTMesh.setCapability(GeometryArray.ALLOW_COUNT_READ);
        kTMesh.setCapability(GeometryArray.ALLOW_COUNT_READ);
        kTMesh.setCapability(GeometryArray.ALLOW_FORMAT_READ);
        kTMesh.setCapability(GeometryArray.ALLOW_INTERSECT);

        return kTMesh;
    }

    /**
     * DOCUMENT ME!
     *
     * @param   kShape  DOCUMENT ME!
     *
     * @return  DOCUMENT ME!
     */
    private static Vector3d getTranslateToCenter(Shape3D kShape) {
        BoundingSphere kBounds = new BoundingSphere(kShape.getBounds());
        Point3d kCenter = new Point3d();
        kBounds.getCenter(kCenter);
        kCenter.negate();
        kBounds = null;

        return new Vector3d(kCenter);
    }

    /**
     * DOCUMENT ME!
     *
     * @param  fCMin       DOCUMENT ME!
     * @param  fCMax       DOCUMENT ME!
     * @param  afCArray    DOCUMENT ME!
     * @param  iVQuantity  DOCUMENT ME!
     * @param  akColor     DOCUMENT ME!
     */
    private static void setColors(float fCMin, float fCMax, float[] afCArray, int iVQuantity, Color4f[] akColor) {

        for (int i = 0; i < iVQuantity; i++) {

            if (afCArray[i] > 0.0f) {
                akColor[i].x = 0.5f + (0.5f * afCArray[i] / fCMax); /* red */
                akColor[i].y = akColor[i].x; /* green */
                akColor[i].z = 0.0f; /* blue */
            } else if (afCArray[i] < 0.0f) {
                akColor[i].x = 0.0f; /* red */
                akColor[i].y = 0.0f; /* green */
                akColor[i].z = 0.5f + (0.5f * (-afCArray[i] / fCMin)); /* blue */
            } else {
                akColor[i].set(0.0f, 0.0f, 0.0f, 1f);
            }
        }
    }

    /**
     * For the plane, all vertices have z-value set to zero. Because the mesh is closed, even for the plane, the edge
     * triangles wrap around the back of the mesh, and are co-planar with the forward-facing triangles (the ones that
     * display the data) -- when picking is done, the pickClosest sometimes returns the back-facing triangle. This
     * function returns the correct, visible triangle for the plane:
     *
     * @param   kStart   DOCUMENT ME!
     * @param   aiIndex  DOCUMENT ME!
     *
     * @return  DOCUMENT ME!
     */
    private float closestPlanePointIndex(Point3f kStart, int[] aiIndex) {
        Point3f[] akPlanePoints = m_kCortical.getCylinderCoordinates();

        float fDistance = Float.MAX_VALUE;
        float fMinDistance = Float.MAX_VALUE;

        for (int i = 0; i < aiIndex.length; i++) {
            fDistance = kStart.distanceSquared(akPlanePoints[aiIndex[i]]);

            if (fDistance < fMinDistance) {
                fMinDistance = fDistance;
            }
        }

        return fMinDistance;
    }

    /**
     * Draw the user-selected point as a sphere on the triangle mesh:
     *
     * @param  kStart   DOCUMENT ME!
     * @param  aiIndex  DOCUMENT ME!
     * @param  iWhich   DOCUMENT ME!
     */
    private void drawPoint(Point3f kStart, int[] aiIndex, int iWhich) {
        m_iNumPicked++;

        if (m_iNumPicked > MAX_POINTS) {
            m_kTransformGroupSphere.removeChild(2);
            m_kTransformGroupPlane.removeChild(2);
            m_kTransformGroupMesh.removeChild(1);
            m_iNumPicked--;
        }

        Shape3D kSphere = new Sphere(.05f).getShape();
        Color3f kColor = new Color3f(1f, 0f, 0f);
        Material kMaterial = new Material(m_akPickColors[m_iRunningNumPicked % MAX_POINTS], new Color3f(0f, 0f, 0f),
                                          m_akPickColors[m_iRunningNumPicked % MAX_POINTS],
                                          m_akPickColors[m_iRunningNumPicked % MAX_POINTS], 128.0f);
        kMaterial.setLightingEnable(true);
        m_iRunningNumPicked++;

        kSphere.getAppearance().setMaterial(kMaterial);
        kSphere.setPickable(false);

        int iVQuantity = m_kCortical.getVQuantity();
        Point3f[] akPlanePoints = m_kCortical.getCylinderCoordinates();
        Point3f[] akSpherePoints = m_kCortical.getSphereCoordinates();
        Point3f[] akMeshPoints = m_kCortical.getPoints();


        float fDistance = Float.MAX_VALUE;
        float fMinDistance = Float.MAX_VALUE;
        int iDistance = 0;

        /* Find the nearest triangle vertex to the picked point: */
        for (int i = 0; i < aiIndex.length; i++) {

            if (iWhich == SPHERE) {
                fDistance = kStart.distanceSquared(akSpherePoints[aiIndex[i]]);
            } else if (iWhich == PLANE) {
                fDistance = kStart.distanceSquared(akPlanePoints[aiIndex[i]]);
            } else if (iWhich == MESH) {
                fDistance = kStart.distanceSquared(akMeshPoints[aiIndex[i]]);
            }

            if (fDistance < fMinDistance) {
                fMinDistance = fDistance;
                iDistance = aiIndex[i];
            }
        }

        /* Sphere: */
        Transform3D kTransformSphere = new Transform3D();
        kTransformSphere.set(new Vector3f(akSpherePoints[iDistance]));

        TransformGroup kTransformGroupSphere = new TransformGroup();
        kTransformGroupSphere.setTransform(kTransformSphere);
        kTransformGroupSphere.addChild(kSphere.cloneTree());

        BranchGroup kBranchGroupSphere = new BranchGroup();
        kBranchGroupSphere.setCapability(BranchGroup.ALLOW_DETACH);
        kBranchGroupSphere.addChild(kTransformGroupSphere);
        m_kTransformGroupSphere.addChild(kBranchGroupSphere.cloneTree());

        /* Plane: */
        Transform3D kTransformPlane = new Transform3D();
        kTransformPlane.set(new Vector3f(akPlanePoints[iDistance]));

        TransformGroup kTransformGroupPlane = new TransformGroup();
        kTransformGroupPlane.setTransform(kTransformPlane);
        kTransformGroupPlane.addChild(kSphere.cloneTree());

        BranchGroup kBranchGroupPlane = new BranchGroup();
        kBranchGroupPlane.setCapability(BranchGroup.ALLOW_DETACH);
        kBranchGroupPlane.addChild(kTransformGroupPlane);
        m_kTransformGroupPlane.addChild(kBranchGroupPlane.cloneTree());


        /* Mesh: */
        Transform3D kTransformMesh = new Transform3D();
        kTransformMesh.set(new Vector3f(akMeshPoints[iDistance]));

        TransformGroup kTransformGroupMesh = new TransformGroup();
        kTransformGroupMesh.setTransform(kTransformMesh);
        kTransformGroupMesh.addChild(kSphere.cloneTree());

        BranchGroup kBranchGroupMesh = new BranchGroup();
        kBranchGroupMesh.setCapability(BranchGroup.ALLOW_DETACH);
        kBranchGroupMesh.addChild(kTransformGroupMesh);
        m_kTransformGroupMesh.addChild(kBranchGroupMesh.cloneTree());
    }

    /**
     * Draw the user-selected triangle as a black triangle, save the original color so it can be restored:
     *
     * @param  kStart   DOCUMENT ME!
     * @param  aiIndex  DOCUMENT ME!
     * @param  iWhich   DOCUMENT ME!
     */
    private void drawTriangle(Point3f kStart, int[] aiIndex, int iWhich) {
        Color4f[] akColor = m_kCortical.getColors();

        /* Restore previously picked triangle colors: */
        restoreTriColor();

        /* Set the picked triangle to black, saving it's color: */
        m_akTriColors = new Color4f[aiIndex.length];
        m_aiTriIndex = new int[aiIndex.length];

        for (int i = 0; i < aiIndex.length; i++) {
            m_akTriColors[i] = new Color4f();
            m_akTriColors[i].x = akColor[aiIndex[i]].x;
            m_akTriColors[i].y = akColor[aiIndex[i]].y;
            m_akTriColors[i].z = akColor[aiIndex[i]].z;
            m_akTriColors[i].w = akColor[aiIndex[i]].w;

            akColor[aiIndex[i]].x = 0f;
            akColor[aiIndex[i]].y = 0f;
            akColor[aiIndex[i]].z = 0f;

            m_aiTriIndex[i] = aiIndex[i];
        }

        m_kGeometryArraySphere.setColors(0, akColor);
        m_kTriangleMesh.setColors(0, akColor);
        m_kGeometryArrayPlane.setColors(0, akColor);
    }

    /**
     * Resets the previously picked triangle to it's orginal color:
     */
    private void restoreTriColor() {

        if ((m_akTriColors != null) && (m_aiTriIndex != null)) {
            Color4f[] akColor = m_kCortical.getColors();

            for (int i = 0; i < m_aiTriIndex.length; i++) {
                akColor[m_aiTriIndex[i]].x = m_akTriColors[i].x;
                akColor[m_aiTriIndex[i]].y = m_akTriColors[i].y;
                akColor[m_aiTriIndex[i]].z = m_akTriColors[i].z;
                akColor[m_aiTriIndex[i]].w = m_akTriColors[i].w;
            }

            m_akTriColors = null;
            m_aiTriIndex = null;

            m_kGeometryArraySphere.setColors(0, akColor);
            m_kTriangleMesh.setColors(0, akColor);
            m_kGeometryArrayPlane.setColors(0, akColor);
        }
    }

    /**
     * Update the colormap based on the LUT:
     */
    private void setColorsLUT() {
        int iLutSize = 0;
        float[] afXLUTa = null;
        float[] afYLUTa = null;
        int iLutHeightA = 0;
        int[] aiLutBufferRemapped = null;

        ModelLUT kLUT;

        float[] afCArray = null;

        if (!m_bCurvatureColors && (null != m_kCortical.getAvrConvexity())) {
            afCArray = m_kCortical.getAvrConvexity();
            kLUT = m_kLUTConvexity;
        } else if (m_bCurvatureColors) {
            afCArray = m_kCortical.getMeanCurvature();
            kLUT = m_kLUTCurvature;
        } else {
            return;
        }

        iLutSize = kLUT.getTransferFunction().size();
        afXLUTa = new float[iLutSize];
        afYLUTa = new float[iLutSize];

        for (int iLIndex = 0; iLIndex < iLutSize; iLIndex++) {
            afXLUTa[iLIndex] = kLUT.getTransferFunction().getPoint(iLIndex).X;
            afYLUTa[iLIndex] = 255 - kLUT.getTransferFunction().getPoint(iLIndex).Y;
        }

        iLutHeightA = kLUT.getExtents()[1];
        aiLutBufferRemapped = new int[iLutHeightA];
        kLUT.exportIndexedLUT(aiLutBufferRemapped);


        int iVQuantity = m_kCortical.getVQuantity();
        Color4f[] akColor = m_kCortical.getColors();

        for (int i = 0; i < iVQuantity; i++) {
            float fValue = afCArray[i];
            int iPixColor = 0;

            for (int iLut = 0; iLut < (iLutSize - 1); iLut++) {

                if ((fValue >= afXLUTa[iLut]) && (fValue <= afXLUTa[iLut + 1])) {
                    float fSlope = 0;

                    if ((afXLUTa[iLut + 1] - afXLUTa[iLut]) != 0) {
                        fSlope = (afYLUTa[iLut + 1] - afYLUTa[iLut]) / (afXLUTa[iLut + 1] - afXLUTa[iLut]);
                    }

                    iPixColor = (int) (afYLUTa[iLut] + (fSlope * (fValue - afXLUTa[iLut])) + 0.5);

                    break;
                }
            }

            int iValue = aiLutBufferRemapped[iPixColor];
            float fRed = (iValue & 0x00ff0000) >> 16;
            float fGreen = (iValue & 0x0000ff00) >> 8;
            float fBlue = (iValue & 0x000000ff);
            akColor[i].x = fRed / 255.0f;
            akColor[i].y = fGreen / 255.0f;
            akColor[i].z = fBlue / 255.0f;
        }

        m_kGeometryArraySphere.setColors(0, akColor);
        m_kTriangleMesh.setColors(0, akColor);
        m_kGeometryArrayPlane.setColors(0, akColor);
    }

    /**
     * Sets up a direction lightsource for the scene.
     */
    private void setupLight() {
        DirectionalLight kLightDr = new DirectionalLight(true, new Color3f(1f, 1f, 1f), new Vector3f(0f, 0f, -1f));
        kLightDr.setInfluencingBounds(bounds);

        BranchGroup kLightBG = new BranchGroup();
        kLightBG.setCapability(BranchGroup.ALLOW_DETACH);
        kLightBG.addChild(kLightDr);
        sceneRootTG.addChild(kLightBG);
    }
}
