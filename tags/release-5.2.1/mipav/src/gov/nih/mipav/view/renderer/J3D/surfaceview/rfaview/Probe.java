package gov.nih.mipav.view.renderer.J3D.surfaceview.rfaview;


import gov.nih.mipav.view.renderer.J3D.surfaceview.*;
import gov.nih.mipav.view.renderer.J3D.surfaceview.rfaview.mouse.*;

import com.sun.j3d.utils.geometry.*;
import com.sun.j3d.utils.picking.*;

import java.awt.*;

import javax.media.j3d.*;

import javax.vecmath.*;


/**
 * <p>Title: Probe</p>
 *
 * <p>Description: This class defines the basic geometry structures of the probe, such as Java3D image scene graph,
 * detection entry point, bone detection point, and probe detection guide line. The class also constructs three types of
 * probe, default type, thermal heat type, and regular type. Each probe has its own geometry shape and appearance. The
 * probe detection is used only by the 3D texture render for the current development phase. Raycast based renderer will
 * have the RFA probe detection later.</p>
 *
 * @author  Ruida Cheng
 */
public class Probe implements MouseBehaviorCallback {

    //~ Static fields/initializers -------------------------------------------------------------------------------------

    /** The default probe type probe, which is in cyan color. */
    public static int DEFAULT_PROBE = 0;

    /** The thermal heat type probe, which is in blue color. */
    public static int THERMAL_HEAT_PROBE = 1;

    /** The regular type probe, which is in gray color. */
    public static int REGULAR_PROBE = 2;

    /** The cool-tip type probe, which trys to mimic the real CoolTip probe. */
    public static int COOLTIP_PROBE = 3;

    //~ Instance fields ------------------------------------------------------------------------------------------------

    /**
     * The blue sphere branch group that branches underneath the vasculatureBG, and represents the blue sphere branch
     * group.
     */
    protected BranchGroup blueBG;

    /** The geometry group of the blue sphere. */
    protected Sphere blueSphere;

    /** blue sphere transform group that set up the translation. */
    protected TransformGroup blueSphereTG;

    /** The root branch group of the bone. */
    protected BranchGroup boneBG;

    /** The root branch group of the probe burning geometry shape. */
    protected BranchGroup burnBG;

    /** Reference to the cool-tip probe geometry structures. */
    protected CoolTipProbe coolTipProbe;

    /** Spheres hold the probe detetion starting and ending point geometry shapes. */
    protected Sphere[] dashSpheres;

    /** Reference to the default probe geometry structures. */
    protected DefaultProbe defaultProbe;

    /** Probe detection ending point transform group that represents the probe guide line ending point. */
    protected TransformGroup endPointTG;

    /** The root branch group of the enty point. */
    protected BranchGroup entryPointBG;

    /** The green sphere branch group that branches underneath the entryPointBG, and represents the green sphere. */
    protected BranchGroup greenBG;

    /** The geometry group of the green sphere. */
    protected Sphere greenSphere;

    /** Green sphere transform group that set up the translation. */
    protected TransformGroup greenSphereTG;

    /** The root branch group of the indicator. The group is the parent root of boneBG, entryPointBG, vascaulatureBG. */
    protected BranchGroup indicatorBG;

    /** Used to set the probe red guiding line pickable. */
    protected PickCanvas pickCanvas;

    /** The probe mouse behavior branchgroup root. */
    protected BranchGroup probeBehaviorBG;

    /** The root branch group of the probe. */
    protected BranchGroup probeRootBG;

    /** Branch group for all the probes. */
    protected BranchGroup probeRootParentBG;

    /** Transform group for all the probes. */
    protected TransformGroup probeRootParentTG;

    /** The probe mouse rotation behavior. */
    protected MouseRotate probeRotate;

    /** Parent of each light bulb. */
    protected BranchGroup[] probeShapeBG;

    /** Parent of all the light bulbs. */
    protected BranchGroup probesRootBG;

    /** The probe translation behavior. */
    protected MouseTranslate probeTranslate;

    /** The probe zoom behavior. */
    protected MouseZoom probeZoom;

    /** Reference to the regular probe geometry structures. */
    protected RegularProbe regularProbe;

    /** Probe detection starting point transform group that represents the probe guide line starting point. */
    protected TransformGroup startPointTG;

    /** The surface render reference. */
    protected SurfaceRender surfaceRender;

    /** Reference to the thermal heat probe geometry structures. */
    protected ThermalProbe thermalProbe;

    /** The root branch group of the vasculatureBG. */
    protected BranchGroup vasculatureBG;

    /**
     * The yellow sphere branch group that branches underneath the boneBG, and repreents the yellow sphere branch group.
     */
    protected BranchGroup yellowBG;

    /** The geometry group of the yellow sphere. */
    protected Sphere yellowSphere;

    /** Yellow sphere transform group that set up the translation. */
    protected TransformGroup yellowSphereTG;

    /** DOCUMENT ME! */
    Color3f blue = new Color3f(Color.blue);

    /** DOCUMENT ME! */
    Color3f emissiveColor = new Color3f(1.0f, 1.0f, 1.0f);

    /** DOCUMENT ME! */
    Color3f green = new Color3f(Color.green);

    /** DOCUMENT ME! */
    Color3f orange = new Color3f(new Color(225, 50, 0));

    /** Basic color scheme. */
    Color3f red = new Color3f(Color.red);

    /** DOCUMENT ME! */
    Color3f yellow = new Color3f(Color.yellow);

    /** X cylinder of the red target sign. */
    private Cylinder cylinderX;

    /** X cylinder transform group that hold the X cylinder translation. */
    private TransformGroup cylinderXTG;

    /** Y cylinder of the red target sign. */
    private Cylinder cylinderY;

    /** Y cylinder transform group that hold the Y cylinder translation. */
    private TransformGroup cylinderYTG;

    /** Z cylinder of the red target sign. */
    private Cylinder cylinderZ;

    /** Z cylinder transform group that hold the Z cylinder translation. */
    private TransformGroup cylinderZTG;

    /** The vector record the translation coordinate of the entry point. */
    private Point3f entryPoint = new Point3f();

    /** Red guide line branch group. */
    private BranchGroup lineBG;

    /** DOCUMENT ME! */
    private float m_fTwistAmount = 0;

    /** Transform3D to twist the volume about the Probe axis:. */
    private Transform3D m_kTwist = new Transform3D();

    /** Normal rotation red line branch group. */
    private BranchGroup nomalRedlineBG;

    /** Nomal red line shape. */
    private Shape3D nomalRedLineShape;

    /** Since we have three type of probes, we set three the number of probes. */
    private int numProbes = 4;

    /**
     * The probe's root transform group, which holds the red target sign, guide line, and the actual probe geometry
     * shape.
     */
    private TransformGroup probe;

    /** The current tranform positioning the probe in java3d space. */
    private Transform3D probeTransform;

    /** Initialize the probe type to default probe type. */
    private int probeType = REGULAR_PROBE;

    /** Entry point rotation redline branch group. */
    private BranchGroup rotationRedlineBG;

    /** roation red guide line shape. */
    private Shape3D rotationRedLineshape;

    /** Y original coordinate value. */
    private float yOrigin = 1.0f;

    //~ Constructors ---------------------------------------------------------------------------------------------------

    /**
     * The probe contructor. Initialize the image scene graph and build the three types of probe.
     *
     * @param  _surfaceRender  SurfaceRender parent reference.
     */
    public Probe(SurfaceRender _surfaceRender) {
        surfaceRender = _surfaceRender;
        init();
        buildProbes(numProbes);
        enableEntryPointRotation(false);
    }

    //~ Methods --------------------------------------------------------------------------------------------------------

    /**
     * Active the corresponding probe with the given probe type.
     *
     * @param  type  probe type.
     */
    public void activeProbe(int type) {

        if (!probeShapeBG[type].isLive()) {

            for (int i = 0; i < probeShapeBG.length; i++) {
                probeShapeBG[i].detach();
            }

            probe.addChild(probeShapeBG[type]);
        }

        if (type == DEFAULT_PROBE) {
            probeType = DEFAULT_PROBE;
        } else if (type == THERMAL_HEAT_PROBE) {
            probeType = THERMAL_HEAT_PROBE;
        } else if (type == REGULAR_PROBE) {
            probeType = REGULAR_PROBE;
        } else if (type == COOLTIP_PROBE) {
            probeType = COOLTIP_PROBE;
        }
    }

    /**
     * Draw the red guiding line along the start and end point.
     */
    public void addGuideLine() {
        LineArray la = new LineArray(2, LineArray.COORDINATES | LineArray.COLOR_3);
        Point3f[] pts = new Point3f[2];
        pts[0] = new Point3f(0.0f, 0.0f, 0.0f);
        pts[1] = new Point3f(0.0f, 0.0f, 0.0f);

        Color3f[] clrs = new Color3f[2];
        clrs[0] = new Color3f(1.0f, 0.0f, 0.0f);
        clrs[1] = new Color3f(1.0f, 0.0f, 0.0f);
        la.setCoordinates(0, pts);
        la.setColors(0, clrs);

        nomalRedLineShape = new Shape3D(la, null);
        nomalRedLineShape.setCapability(Shape3D.ALLOW_APPEARANCE_READ);
        nomalRedLineShape.setCapability(Shape3D.ALLOW_APPEARANCE_WRITE);

        // nomalRedLineShape.setCapability( Shape3D.ENABLE_PICK_REPORTING );
        nomalRedLineShape.setCapability(Shape3D.ALLOW_GEOMETRY_READ);
        nomalRedLineShape.setCapability(Shape3D.ALLOW_GEOMETRY_WRITE);
        nomalRedLineShape.setCapability(Geometry.ALLOW_INTERSECT);
        nomalRedLineShape.setPickable(false);

        try {
            pickCanvas.setCapabilities(nomalRedLineShape, PickTool.INTERSECT_FULL);
        } catch (RestrictedAccessException error) { }

        nomalRedlineBG = new BranchGroup();
        nomalRedlineBG.setCapability(Group.ALLOW_CHILDREN_EXTEND);
        nomalRedlineBG.setCapability(Group.ALLOW_CHILDREN_READ);
        nomalRedlineBG.setCapability(Group.ALLOW_CHILDREN_WRITE);
        nomalRedlineBG.setCapability(BranchGroup.ALLOW_DETACH);
        nomalRedlineBG.setCapability(Group.ALLOW_PICKABLE_READ);
        nomalRedlineBG.setCapability(Group.ALLOW_PICKABLE_WRITE);

        TransformGroup temp = new TransformGroup();
        temp.setCapability(TransformGroup.ALLOW_TRANSFORM_WRITE);
        temp.setCapability(TransformGroup.ALLOW_TRANSFORM_READ);
        temp.setCapability(Group.ALLOW_CHILDREN_READ);
        temp.setCapability(Group.ALLOW_CHILDREN_WRITE);
        temp.setCapability(TransformGroup.ENABLE_PICK_REPORTING);
        nomalRedlineBG.addChild(temp);

        temp.addChild(nomalRedLineShape);

        LineArray rotationRedLine = new LineArray(2, LineArray.COORDINATES | LineArray.COLOR_3);
        Point3f[] rotaionRedLinePts = new Point3f[2];
        rotaionRedLinePts[0] = new Point3f(0f, 0f, 0f);
        rotaionRedLinePts[1] = new Point3f(0f, 0f, 0f);

        Color3f[] rotationRedLineClrs = new Color3f[2];
        rotationRedLineClrs[0] = new Color3f(1.0f, 0.0f, 0.0f);
        rotationRedLineClrs[1] = new Color3f(1.0f, 0.0f, 0.0f);
        rotationRedLine.setCoordinates(0, rotaionRedLinePts);
        rotationRedLine.setColors(0, rotationRedLineClrs);

        rotationRedLineshape = new Shape3D(rotationRedLine, null);
        rotationRedLineshape.setCapability(Shape3D.ALLOW_APPEARANCE_READ);
        rotationRedLineshape.setCapability(Shape3D.ALLOW_APPEARANCE_WRITE);

        // rotationRedLineshape.setCapability( Shape3D.ENABLE_PICK_REPORTING );
        rotationRedLineshape.setCapability(Shape3D.ALLOW_GEOMETRY_READ);
        rotationRedLineshape.setCapability(Shape3D.ALLOW_GEOMETRY_WRITE);
        rotationRedLineshape.setCapability(Geometry.ALLOW_INTERSECT);
        rotationRedLineshape.setPickable(false);

        try {
            pickCanvas.setCapabilities(rotationRedLineshape, PickTool.INTERSECT_FULL);
        } catch (RestrictedAccessException error) { }

        rotationRedlineBG = new BranchGroup();
        rotationRedlineBG.setCapability(Group.ALLOW_CHILDREN_EXTEND);
        rotationRedlineBG.setCapability(Group.ALLOW_CHILDREN_READ);
        rotationRedlineBG.setCapability(Group.ALLOW_CHILDREN_WRITE);
        rotationRedlineBG.setCapability(BranchGroup.ALLOW_DETACH);

        TransformGroup tg = new TransformGroup();

        tg.setCapability(TransformGroup.ALLOW_TRANSFORM_WRITE);
        tg.setCapability(TransformGroup.ALLOW_TRANSFORM_READ);
        tg.setCapability(Group.ALLOW_CHILDREN_READ);
        tg.setCapability(Group.ALLOW_CHILDREN_WRITE);
        tg.setCapability(TransformGroup.ENABLE_PICK_REPORTING);
        rotationRedlineBG.addChild(tg);

        tg.addChild(rotationRedLineshape);

        lineBG = new BranchGroup();
        lineBG.setCapability(Group.ALLOW_CHILDREN_EXTEND);
        lineBG.setCapability(Group.ALLOW_CHILDREN_READ);
        lineBG.setCapability(Group.ALLOW_CHILDREN_WRITE);
        lineBG.setCapability(BranchGroup.ALLOW_DETACH);

        lineBG.addChild(nomalRedlineBG);

        probe.addChild(lineBG);
    }

    /**
     * Build the bone ( yellow sphere ) branch group image scene graph.
     */
    public void buildBoneBG() {
        yellowBG = new BranchGroup();
        yellowBG.setCapability(BranchGroup.ALLOW_DETACH);
        yellowBG.setCapability(BranchGroup.ALLOW_CHILDREN_READ);
        yellowBG.setCapability(BranchGroup.ALLOW_CHILDREN_WRITE);
        yellowBG.setCapability(BranchGroup.ALLOW_CHILDREN_EXTEND);

        Material mat;
        Appearance app = new Appearance();

        mat = new Material(yellow, yellow, yellow, yellow, 80f);
        mat.setCapability(Material.ALLOW_COMPONENT_READ);
        mat.setCapability(Material.ALLOW_COMPONENT_WRITE);
        app.setMaterial(mat);

        yellowSphere = new Sphere(.01f, app);
        yellowSphere.setCapability(Sphere.ENABLE_APPEARANCE_MODIFY);
        yellowSphere.getShape().setCapability(Shape3D.ALLOW_APPEARANCE_READ);
        yellowSphere.getShape().setCapability(Shape3D.ALLOW_APPEARANCE_WRITE);
        yellowSphere.getShape().setAppearanceOverrideEnable(true);
        yellowSphere.getShape().setCapability(Geometry.ALLOW_INTERSECT);

        yellowSphereTG = new TransformGroup();

        yellowSphereTG.setCapability(TransformGroup.ALLOW_TRANSFORM_READ);
        yellowSphereTG.setCapability(TransformGroup.ALLOW_TRANSFORM_WRITE);
        yellowSphereTG.setCapability(TransformGroup.ALLOW_CHILDREN_READ);

        yellowSphereTG.addChild(yellowSphere);
        yellowBG.addChild(yellowSphereTG);

    }

    /**
     * Build the entry point ( green sphere ) branch group image scene graph.
     */
    public void buildEntryPointBG() {
        greenBG = new BranchGroup();
        greenBG.setCapability(BranchGroup.ALLOW_DETACH);
        greenBG.setCapability(BranchGroup.ALLOW_CHILDREN_READ);
        greenBG.setCapability(BranchGroup.ALLOW_CHILDREN_WRITE);
        greenBG.setCapability(BranchGroup.ALLOW_CHILDREN_EXTEND);

        Material mat;
        Appearance app = new Appearance();

        mat = new Material(green, green, green, green, 80f);
        mat.setCapability(Material.ALLOW_COMPONENT_READ);
        mat.setCapability(Material.ALLOW_COMPONENT_WRITE);
        app.setMaterial(mat);

        greenSphere = new Sphere(.01f, app);
        greenSphere.setCapability(Sphere.ENABLE_APPEARANCE_MODIFY);
        greenSphere.getShape().setCapability(Shape3D.ALLOW_APPEARANCE_READ);
        greenSphere.getShape().setCapability(Shape3D.ALLOW_APPEARANCE_WRITE);
        greenSphere.getShape().setAppearanceOverrideEnable(true);
        greenSphere.getShape().setCapability(Geometry.ALLOW_INTERSECT);

        greenSphereTG = new TransformGroup();

        greenSphereTG.setCapability(TransformGroup.ALLOW_TRANSFORM_READ);
        greenSphereTG.setCapability(TransformGroup.ALLOW_TRANSFORM_WRITE);
        greenSphereTG.setCapability(TransformGroup.ALLOW_CHILDREN_READ);

        greenSphereTG.addChild(greenSphere);
        greenBG.addChild(greenSphereTG);
    }

    /**
     * Build the vasculature ( blue sphere ) branch group image scene graph.
     */
    public void buildVasculatureBG() {
        blueBG = new BranchGroup();
        blueBG.setCapability(BranchGroup.ALLOW_DETACH);
        blueBG.setCapability(BranchGroup.ALLOW_CHILDREN_READ);
        blueBG.setCapability(BranchGroup.ALLOW_CHILDREN_WRITE);
        blueBG.setCapability(BranchGroup.ALLOW_CHILDREN_EXTEND);

        Material mat;
        Appearance app = new Appearance();

        mat = new Material(blue, blue, blue, blue, 80f);
        mat.setCapability(Material.ALLOW_COMPONENT_READ);
        mat.setCapability(Material.ALLOW_COMPONENT_WRITE);
        app.setMaterial(mat);

        blueSphere = new Sphere(.01f, app);
        blueSphere.setCapability(Sphere.ENABLE_APPEARANCE_MODIFY);
        blueSphere.getShape().setCapability(Shape3D.ALLOW_APPEARANCE_READ);
        blueSphere.getShape().setCapability(Shape3D.ALLOW_APPEARANCE_WRITE);
        blueSphere.getShape().setAppearanceOverrideEnable(true);
        blueSphere.getShape().setCapability(Geometry.ALLOW_INTERSECT);

        blueSphereTG = new TransformGroup();

        blueSphereTG.setCapability(TransformGroup.ALLOW_TRANSFORM_READ);
        blueSphereTG.setCapability(TransformGroup.ALLOW_TRANSFORM_WRITE);
        blueSphereTG.setCapability(TransformGroup.ALLOW_CHILDREN_READ);

        blueSphereTG.addChild(blueSphere);
        blueBG.addChild(blueSphereTG);

    }

    /**
     * Enable the probe to rotate around the entry point.
     *
     * @param  flag  boolean <code>true</code> means around the entry point, <code>false</code> around the origin.
     */
    public void enableEntryPointRotation(boolean flag) {
        Transform3D greenTrans = new Transform3D();
        Transform3D cylinderXTransform = new Transform3D();
        Matrix3d matrixAdjustCylinderX = new Matrix3d();
        Transform3D cylinderYTransform = new Transform3D();
        matrixAdjustCylinderX.rotX(Math.PI / 2);

        Matrix3d matrixAdjustCylinderY = new Matrix3d();
        matrixAdjustCylinderY.rotX(0 / 2);

        Transform3D cylinderZTransform = new Transform3D();
        Matrix3d matrixAdjustCylinderZ = new Matrix3d();
        matrixAdjustCylinderZ.rotZ(Math.PI / 2);

        Transform3D startTrans = new Transform3D();
        Transform3D endTrans = new Transform3D();

        removeIndicator();

        nomalRedlineBG.detach();
        rotationRedlineBG.detach();

        if (flag) {
            yOrigin = 0.0f;
            greenSphereTG.getTransform(greenTrans);
            lineBG.addChild(rotationRedlineBG);
        } else {
            yOrigin = 1.0f;
            lineBG.addChild(nomalRedlineBG);
        }

        cylinderXTransform.setTranslation(new Vector3f(0.0f, yOrigin, 0.0f));
        cylinderXTransform.setScale(0.005);
        cylinderXTransform.setRotationScale(matrixAdjustCylinderX);
        cylinderXTG.setTransform(cylinderXTransform);

        cylinderYTransform.setTranslation(new Vector3f(0.0f, yOrigin, 0.0f));
        cylinderYTransform.setScale(0.005);
        cylinderYTransform.setRotationScale(matrixAdjustCylinderY);
        cylinderYTG.setTransform(cylinderYTransform);

        cylinderZTransform.setTranslation(new Vector3f(0.0f, yOrigin, 0.0f));
        cylinderZTransform.setScale(0.005);
        cylinderZTransform.setRotationScale(matrixAdjustCylinderZ);
        cylinderZTG.setTransform(cylinderZTransform);

        startTrans.setTranslation(new Vector3f(0.0f, yOrigin, 0.0f));
        startPointTG.setTransform(startTrans);
        endTrans.setTranslation(new Vector3f(0.0f, yOrigin - 1.0f, 0.0f));
        endPointTG.setTransform(endTrans);

        Vector3f startVector = new Vector3f();
        Vector3f endVector = new Vector3f();

        startTrans.get(startVector);
        endTrans.get(endVector);

        updateRotationRedline(startVector, endVector);

        Vector3f translate = new Vector3f();
        Transform3D probeTrans = new Transform3D();
        greenTrans.get(translate);
        probeRootParentTG.getTransform(probeTrans);
        probeTrans.setTranslation(translate);
        probeRootParentTG.setTransform(probeTrans);

        if (!flag && (surfaceRender.getProbeDialog() != null) && surfaceRender.getProbeDialog().isSnapToTarget()) {
            Point3f center = surfaceRender.getProbeDialog().getTargetSurface().getCenter();
            Vector3f surfaceCenter = new Vector3f(center.x, center.y, center.z);
            Transform3D t3d = new Transform3D();

            probeRootParentTG.getTransform(t3d);
            t3d.setTranslation(surfaceCenter);
            probeRootParentTG.setTransform(t3d);
        }

        if (probeType == DEFAULT_PROBE) {
            defaultProbe.enableEntryPointRotation(flag);
        } else if (probeType == THERMAL_HEAT_PROBE) {
            thermalProbe.enableEntryPointRotation(flag);
        } else if (probeType == REGULAR_PROBE) {
            regularProbe.enableEntryPointRotation(flag);
        } else if (probeType == COOLTIP_PROBE) {
            coolTipProbe.enableEntryPointRotation(flag);
        }
    }

    /**
     * Enable probe mouse behavior.
     *
     * @param  flag  <code>true</code> means turn on, <code>false</code> means turn off.
     */
    public void enableProbeBehavior(boolean flag) {

        if (flag) {

            if (!probeBehaviorBG.isLive()) {
                probeRootParentTG.addChild(probeBehaviorBG);
            }
        } else {

            if (probeBehaviorBG.isLive()) {
                probeBehaviorBG.detach();
            }
        }
    }

    /**
     * Find the probe with the given geometry shape.
     *
     * @param   pickedShape  geometry shape of the probe.
     *
     * @return  boolean find the probe shape or not
     */
    public boolean findProbe(Shape3D pickedShape) {

        if (probeType == DEFAULT_PROBE) {
            return defaultProbe.findProbe(pickedShape);
        } else if (probeType == THERMAL_HEAT_PROBE) {
            return thermalProbe.findProbe(pickedShape);
        } else if (probeType == REGULAR_PROBE) {
            return regularProbe.findProbe(pickedShape);
        } else if (probeType == COOLTIP_PROBE) {
            return coolTipProbe.findProbe(pickedShape);
        }

        return false;
    }

    /**
     * The red target sign's x cylinder transform group, which regards as the center of the burning point.
     *
     * @return  Transform3D burning point's transform in world coordinate.
     */
    public Transform3D getCoordinate() {
        Transform3D t2, t4, t5, result;

        t2 = new Transform3D();
        t4 = new Transform3D();
        t5 = new Transform3D();
        result = new Transform3D();

        probeRootParentTG.getTransform(t2);
        probe.getTransform(t4);
        cylinderXTG.getTransform(t5);
        result.mul(t2);
        result.mul(t4);
        result.mul(t5);

        return result;
    }

    /**
     * Get the red target sign x cylinder transform group.
     *
     * @return  TransformGroup x cylidner transform gorup.
     */
    public TransformGroup getCylinderXTG() {
        return cylinderXTG;
    }

    /**
     * Get the end point transform. Used by the burning process to decide the recorded probing path.
     *
     * @return  Transform3D end point transform in world coordinate.
     */
    public Transform3D getEndPointCoordinate() {
        Transform3D t2, t4, t5, result;

        t2 = new Transform3D();
        t4 = new Transform3D();
        t5 = new Transform3D();
        result = new Transform3D();

        probeRootParentTG.getTransform(t2);
        probe.getTransform(t4);
        endPointTG.getTransform(t5);
        result.mul(t2);
        result.mul(t4);
        result.mul(t5);

        return result;
    }

    /**
     * Get the entry pont transform group. Used by the burning process to decide the recorded probing entry point.
     *
     * @return  Point3f get the entry point transform in world coordinate.
     */
    public Point3f getEntryPoint() {
        return (Point3f) (entryPoint.clone());
    }

    /**
     * Return the root to the probes branches.
     *
     * @return  BranchGroup probes branch group
     */
    public BranchGroup getProbesRootBG() {
        return probesRootBG;
    }

    /**
     * Return the probe position transformation.
     *
     * @return  the probe transform
     */
    public Transform3D getProbeTransform() {
        return probeTransform;
    }

    /**
     * Return the current active probe type.
     *
     * @return  int probe type.
     */
    public int getProbeType() {
        return probeType;
    }

    /**
     * Get the probe's transform group.
     *
     * @return  TransformGroup the probe's transform group.
     */
    public TransformGroup getProbRootParentTG() {
        return probeRootParentTG;
    }

    /**
     * Get the start point transform. Used by the burning process to decide the recorded probing path.
     *
     * @return  Transform3D start point transform in world coordinate.
     */
    public Transform3D getStartPointCoordinate() {
        Transform3D t2, t4, t5, result;

        t2 = new Transform3D();
        t4 = new Transform3D();
        t5 = new Transform3D();
        result = new Transform3D();

        probeRootParentTG.getTransform(t2);
        probe.getTransform(t4);
        startPointTG.getTransform(t5);
        result.mul(t2);
        result.mul(t4);
        result.mul(t5);

        return result;
    }

    /**
     * Get the current probe transform position.
     *
     * @return  Transform3D probe tranform
     */
    public Transform3D getTransform() {
        Transform3D t3d = new Transform3D();
        getProbRootParentTG().getTransform(t3d);

        return t3d;
    }

    /**
     * Create image scene graph.
     */
    public void init() {
        createImageSceneGraph(numProbes);
    }

    /**
     * Check the probe root branch group is attached or not. JPanelProbe call this method during probe detection and
     * burning.
     *
     * @return  boolean probe branch group live or not.
     */
    public boolean isProbeRootParentBGLive() {
        return probeRootParentBG.isLive();
    }

    /**
     * Detach and attach the probe root branch group.
     */
    public void navigation() {

        if (!probeRootParentBG.isLive()) {
            probeRootParentTG.addChild(probeRootParentBG);
        } else {
            probeRootParentBG.detach();
        }
    }

    /** 
     * Resets the probe navigation (rotation, translation, zoom) to the
     * origin. Updates the SurfaceRender with the reset probe position.
     */
    public void resetProbeTransform()
    {
        /* reset the probe transform to the identity matrix: */
        probeRootParentTG.setTransform( new Transform3D() );
        probeTransform = new Transform3D();
        /* Update the surface probe positions: */
        surfaceRender.setProbeTG(probeTransform, false);
        /* re-render the slices: */
        surfaceRender.updateProbe(true, true, true);
    }

    /**
     * Detach all the detection sphere branches.
     */
    public void removeIndicator() {
        yellowBG.detach();
        greenBG.detach();
        blueBG.detach();
    }

    /**
     * When the probe is picked, change the probe geometry shape color to green to indicate picked.
     *
     * @param  flag  boolean picked or not.
     */
    public void setProbeGreenColor(boolean flag) {

        if (probeType == DEFAULT_PROBE) {
            defaultProbe.setProbeGreenColor(flag);
        } else if (probeType == THERMAL_HEAT_PROBE) {
            thermalProbe.setProbeGreenColor(flag);
        } else if (probeType == REGULAR_PROBE) {
            regularProbe.setProbeGreenColor(flag);
        } else if (probeType == COOLTIP_PROBE) {
            coolTipProbe.setProbeGreenColor(flag);
        }
    }

    /**
     * When the probe is picked and rotated by mouse, detecting the probing tissue along the probing path.
     *
     * @param  type       not used.
     * @param  transform  Transform3D not used.
     */
    public void transformChanged(int type, Transform3D transform) {
        probeTransform = transform;

        surfaceRender.getProbeDialog().removeProbingPath();

        if (surfaceRender.getSurfaceDialog().isProbePicked()) {
            surfaceRender.getProbeDialog().detectTissue();

            if (type == MouseBehaviorCallback.ROTATE_LEFTBUTTON) {
                surfaceRender.setProbeTG(transform, false);

                /* Update the surface probe positions: */
                surfaceRender.updateProbe(true, false, false);
            }
        }
    }

    /**
     * Called when the JPanelProbe twist slider is changed. The twist transform is updated based on the input value
     * fValue, and passed to the SurfaceRender class for display.
     *
     * @param  fValue        float rotation angle in degree
     * @param  bIsAdjusting  boolean adjusting flag
     */
    public void twist(float fValue, boolean bIsAdjusting) {

        /* The initial ProbeVector direction: */
        Vector3f kProbeVector = new Vector3f(0, 0, -1);

        /* Get the current probe transformation to update the probe axis: */
        Transform3D kTransform = new Transform3D();
        probeRootParentTG.getTransform(kTransform);
        kTransform.transform(kProbeVector);

        /* Calculate the incrementatl twist amount:  */
        fValue = fValue - m_fTwistAmount;
        m_fTwistAmount += fValue;

        /* Create a transform that rotates the incremental twist amount about
         * the probe axis: */
        Transform3D kTwist = new Transform3D();
        kTwist.setRotation(new AxisAngle4f(kProbeVector, (float) (Math.PI * (fValue / 180.0f))));

        m_kTwist.set(kTwist);

        /* Add any probe translation so that the twist is about the axis of
         * the probe: */
        Vector3f kTransVector = new Vector3f();
        kTransform.get(kTransVector);
        m_kTwist.setTranslation(kTransVector);
        surfaceRender.setProbeTG(m_kTwist, true);
        surfaceRender.updateProbe(true, !bIsAdjusting, false);

        /* Reset probe translation for m_kTwist to zero: */
        m_kTwist.setTranslation(new Vector3f(0f, 0f, 0f));
    }

    /**
     * Update the yellow sphere's tranlation component with the given x, y, z coordinates.
     *
     * @param  x  x position in the world coordinate.
     * @param  y  y position in the world coordinate.
     * @param  z  z position in the world coordinate.
     */
    public void updateBoneBG(float x, float y, float z) {

        // set the translation first.
        Transform3D trans = new Transform3D();
        trans.setTranslation(new Vector3f(x, y, z));
        yellowSphereTG.setTransform(trans);

        // if yellow sphere is detached, attach it.
        if (!yellowBG.isLive()) {
            boneBG.addChild(yellowBG);
        }
    }

    /**
     * Update the green sphere's translation component with the given x, y, z coordinate.
     *
     * @param  x  x position in the world coordinate.
     * @param  y  y position in the world coordinate.
     * @param  z  z position in the world coordinate.
     */
    public void updateEntryPointBG(float x, float y, float z) {
        Transform3D trans = new Transform3D();
        trans.setTranslation(new Vector3f(x, y, z));

        // update entry point translation component vector
        entryPoint.x = x;
        entryPoint.y = y;
        entryPoint.z = z;
        greenSphereTG.setTransform(trans);

        if (!greenBG.isLive()) {
            entryPointBG.addChild(greenBG);

        }
    }

    /**
     * Set the probe's translation.
     *
     * @param  kPosition  the probe's translation component.
     */
    public void updatePosition(Point3f kPosition) {
        Transform3D t = new Transform3D();
        probeRootParentTG.getTransform(t);

        // In MIPAV, +x goes right, +y goes down, +z goes into screen.
        // In Java3D, +x goes right, +y goes up, +z goes out of screen.
        t.setTranslation(new Vector3d(kPosition.x, -kPosition.y, -kPosition.z));
        probeRootParentTG.setTransform(t);
    }

    /**
     * Update the blue sphere's translation component with the given x, y, z coordinate.
     *
     * @param  x  x position in the world coordinate.
     * @param  y  y position in the world coordinate.
     * @param  z  z position in the world coordinate.
     */
    public void updateVasculatureBG(float x, float y, float z) {
        Transform3D trans = new Transform3D();
        trans.setTranslation(new Vector3f(x, y, z));
        blueSphereTG.setTransform(trans);

        if (!blueBG.isLive()) {
            vasculatureBG.addChild(blueBG);

        }
    }

    /**
     * Set the probe translation in its local coordinate.
     *
     * @param  value  translation component y value.
     */
    protected void setProbeCoordinate(float value) {
        Transform3D transX = new Transform3D();
        Vector3f vX = new Vector3f();

        cylinderXTG.getTransform(transX);
        transX.get(vX);
        vX.y = (yOrigin - value);
        transX.setTranslation(vX);
        cylinderXTG.setTransform(transX);

        Transform3D transY = new Transform3D();
        Vector3f vY = new Vector3f();

        cylinderYTG.getTransform(transY);
        transY.get(vY);
        vY.y = (yOrigin - value);
        transY.setTranslation(vY);

        Matrix3d matrixAdjustCylinderY = new Matrix3d();

        matrixAdjustCylinderY.rotX(0 / 2);
        transY.setRotationScale(matrixAdjustCylinderY);
        cylinderYTG.setTransform(transY);

        Transform3D transZ = new Transform3D();
        Vector3f vZ = new Vector3f();

        cylinderZTG.getTransform(transZ);
        transZ.get(vZ);
        vZ.y = (yOrigin - value);
        transZ.setTranslation(vZ);
        cylinderZTG.setTransform(transZ);

        if (probeType == DEFAULT_PROBE) {
            defaultProbe.setProbeCoordinate(value);
        } else if (probeType == THERMAL_HEAT_PROBE) {
            thermalProbe.setProbeCoordinate(value);
        } else if (probeType == REGULAR_PROBE) {
            regularProbe.setProbeCoordinate(value);
        } else if (probeType == COOLTIP_PROBE) {
            coolTipProbe.setProbeCoordinate(value);
        }

    }

    /**
     * Build the three types of probe. The basic geometry structure of the probes include the red target, entry and
     * ending point, the red guide line, etc.
     *
     * @param  iNumProbes  total different types of the probe.
     */
    private void buildProbes(int iNumProbes) {

        probe = new TransformGroup();
        probe.setCapability(TransformGroup.ALLOW_CHILDREN_READ);
        probe.setCapability(TransformGroup.ALLOW_CHILDREN_EXTEND);
        probe.setCapability(TransformGroup.ALLOW_CHILDREN_WRITE);
        probe.setCapability(TransformGroup.ALLOW_TRANSFORM_READ);

        probesRootBG.addChild(probe);

        Appearance crossAppearance = new Appearance();
        Material croossMaterials = new Material(red, red, red, red, 80f);

        croossMaterials.setCapability(Material.ALLOW_COMPONENT_READ);
        croossMaterials.setCapability(Material.ALLOW_COMPONENT_WRITE);
        crossAppearance.setMaterial(croossMaterials);

        cylinderX = new Cylinder(0.0025f, 0.05f, Primitive.GENERATE_NORMALS, crossAppearance);

        cylinderX.getShape(0).setCapability(Shape3D.ALLOW_APPEARANCE_READ);
        cylinderX.getShape(0).setCapability(Shape3D.ALLOW_APPEARANCE_WRITE);
        cylinderX.getShape(0).setCapability(Shape3D.ALLOW_GEOMETRY_WRITE);
        cylinderX.getShape(0).setCapability(Shape3D.ALLOW_GEOMETRY_READ);
        cylinderX.getShape(0).setCapability(Geometry.ALLOW_INTERSECT);
        cylinderX.getShape(0).setAppearanceOverrideEnable(true);

        Transform3D cylinderXTransform = new Transform3D();

        cylinderXTransform.setTranslation(new Vector3f(0.0f, 1.0f, 0.0f));
        cylinderXTransform.setScale(0.005);

        Matrix3d matrixAdjustCylinderX = new Matrix3d();

        matrixAdjustCylinderX.rotX(Math.PI / 2);
        cylinderXTransform.setRotationScale(matrixAdjustCylinderX);
        cylinderXTG = new TransformGroup(cylinderXTransform);

        cylinderXTG.setCapability(TransformGroup.ALLOW_TRANSFORM_READ);
        cylinderXTG.setCapability(TransformGroup.ALLOW_TRANSFORM_WRITE);
        cylinderXTG.setCapability(TransformGroup.ALLOW_CHILDREN_READ);
        cylinderXTG.setCapability(TransformGroup.ALLOW_CHILDREN_WRITE);
        cylinderXTG.setCapability(TransformGroup.ALLOW_CHILDREN_EXTEND);
        cylinderXTG.addChild(cylinderX);
        probe.addChild(cylinderXTG);

        cylinderY = new Cylinder(0.0025f, 0.05f, Primitive.GENERATE_NORMALS, crossAppearance);

        cylinderY.getShape(0).setCapability(Shape3D.ALLOW_APPEARANCE_READ);
        cylinderY.getShape(0).setCapability(Shape3D.ALLOW_APPEARANCE_WRITE);
        cylinderY.getShape(0).setCapability(Shape3D.ALLOW_GEOMETRY_WRITE);
        cylinderY.getShape(0).setCapability(Shape3D.ALLOW_GEOMETRY_READ);
        cylinderY.getShape(0).setCapability(Geometry.ALLOW_INTERSECT);
        cylinderY.getShape(0).setAppearanceOverrideEnable(true);

        Transform3D cylinderYTransform = new Transform3D();

        cylinderYTransform.setTranslation(new Vector3f(0.0f, 1.0f, 0.0f));
        cylinderYTransform.setScale(0.005);

        Matrix3d matrixAdjustCylinderY = new Matrix3d();

        matrixAdjustCylinderY.rotX(0 / 2);
        cylinderYTransform.setRotationScale(matrixAdjustCylinderY);

        cylinderYTG = new TransformGroup(cylinderYTransform);
        cylinderYTG.setCapability(TransformGroup.ALLOW_TRANSFORM_READ);
        cylinderYTG.setCapability(TransformGroup.ALLOW_TRANSFORM_WRITE);
        cylinderYTG.setCapability(TransformGroup.ALLOW_CHILDREN_READ);
        cylinderYTG.addChild(cylinderY);
        probe.addChild(cylinderYTG);

        cylinderZ = new Cylinder(0.0025f, 0.05f, Primitive.GENERATE_NORMALS, crossAppearance);

        cylinderZ.getShape(0).setCapability(Shape3D.ALLOW_APPEARANCE_READ);
        cylinderZ.getShape(0).setCapability(Shape3D.ALLOW_APPEARANCE_WRITE);
        cylinderZ.getShape(0).setCapability(Shape3D.ALLOW_GEOMETRY_WRITE);
        cylinderZ.getShape(0).setCapability(Shape3D.ALLOW_GEOMETRY_READ);
        cylinderZ.getShape(0).setCapability(Geometry.ALLOW_INTERSECT);
        cylinderZ.getShape(0).setAppearanceOverrideEnable(true);

        Transform3D cylinderZTransform = new Transform3D();

        cylinderZTransform.setTranslation(new Vector3f(0.0f, 1.0f, 0.0f));
        cylinderZTransform.setScale(0.005);

        Matrix3d matrixAdjustCylinderZ = new Matrix3d();

        matrixAdjustCylinderZ.rotZ(Math.PI / 2);
        cylinderZTransform.setRotationScale(matrixAdjustCylinderZ);
        cylinderZTG = new TransformGroup(cylinderZTransform);
        cylinderZTG.setCapability(TransformGroup.ALLOW_TRANSFORM_READ);
        cylinderZTG.setCapability(TransformGroup.ALLOW_TRANSFORM_WRITE);
        cylinderZTG.setCapability(TransformGroup.ALLOW_CHILDREN_READ);
        cylinderZTG.addChild(cylinderZ);
        probe.addChild(cylinderZTG);

        // dash lines
        dashSpheres = new Sphere[2];
        startPointTG = new TransformGroup();
        endPointTG = new TransformGroup();

        for (int j = 0; j < 2; j++) {
            Material dashBallMaterials;
            Appearance dashBallsAppearance = new Appearance();

            if (j == 1) {
                dashBallMaterials = new Material(red, red, red, red, 80f);
                dashBallMaterials.setCapability(Material.ALLOW_COMPONENT_READ);
                dashBallMaterials.setCapability(Material.ALLOW_COMPONENT_WRITE);
                dashBallsAppearance.setMaterial(dashBallMaterials);
            } else { // j == 0
                dashBallsAppearance.setCapability(Appearance.ALLOW_TRANSPARENCY_ATTRIBUTES_WRITE);

                TransparencyAttributes tap = new TransparencyAttributes();
                tap.setCapability(TransparencyAttributes.ALLOW_VALUE_WRITE);
                tap.setTransparencyMode(TransparencyAttributes.BLENDED);
                tap.setTransparency(1.0f);
                dashBallsAppearance.setTransparencyAttributes(tap);
            }

            dashSpheres[j] = new Sphere(.005f, dashBallsAppearance);
            dashSpheres[j].setCapability(Sphere.ENABLE_APPEARANCE_MODIFY);
            dashSpheres[j].getShape().setCapability(Shape3D.ALLOW_APPEARANCE_READ);
            dashSpheres[j].getShape().setCapability(Shape3D.ALLOW_APPEARANCE_WRITE);
            dashSpheres[j].getShape().setAppearanceOverrideEnable(true);
            dashSpheres[j].getShape().setCapability(Geometry.ALLOW_INTERSECT);

            Transform3D sphereTransform = new Transform3D();

            if (j == 1) {
                sphereTransform.setTranslation(new Vector3f(0.0f, 0.0f, 0.0f));
            } else {
                sphereTransform.setTranslation(new Vector3f(0.0f, 2.0f, 0.0f));
            }

            TransformGroup sphereTG = new TransformGroup(sphereTransform);

            sphereTG.setCapability(TransformGroup.ALLOW_TRANSFORM_READ);
            sphereTG.setCapability(TransformGroup.ALLOW_TRANSFORM_WRITE);
            sphereTG.setCapability(TransformGroup.ALLOW_CHILDREN_READ);

            if (j == 1) {
                endPointTG = sphereTG;
            } else if (j == 0) {
                startPointTG = sphereTG;
            }

            sphereTG.addChild(dashSpheres[j]);
            probe.addChild(sphereTG);

            Transform3D startTrans = new Transform3D();
            Transform3D endTrans = new Transform3D();

            startPointTG.getTransform(startTrans);
            endPointTG.getTransform(endTrans);

            Vector3f startVector = new Vector3f();
            Vector3f endVector = new Vector3f();

            startTrans.get(startVector);
            endTrans.get(endVector);

            addGuideLine();
        }

        defaultProbe = new DefaultProbe();
        thermalProbe = new ThermalProbe();
        regularProbe = new RegularProbe();
        coolTipProbe = new CoolTipProbe();

        for (int i = 0; i < iNumProbes; i++) {
            probeShapeBG[i] = new BranchGroup();
            probeShapeBG[i].setCapability(BranchGroup.ALLOW_DETACH);
            probeShapeBG[i].setCapability(Group.ALLOW_CHILDREN_EXTEND);
            probeShapeBG[i].setCapability(BranchGroup.ALLOW_CHILDREN_READ);
            probeShapeBG[i].setCapability(BranchGroup.ALLOW_CHILDREN_WRITE);

            if (i == DEFAULT_PROBE) {
                probeShapeBG[i].addChild(defaultProbe.getProbe());
            } else if (i == THERMAL_HEAT_PROBE) {
                probeShapeBG[i].addChild(thermalProbe.getProbe());
            } else if (i == REGULAR_PROBE) {
                probeShapeBG[i].addChild(regularProbe.getProbe());
                probe.addChild(probeShapeBG[i]);
            } else if (i == COOLTIP_PROBE) {
                probeShapeBG[i].addChild(coolTipProbe.getProbe());
            }
        }

        /** Probe position */
        Transform3D arrowTransform = new Transform3D();
        Matrix3d matrixAdjustGeometry = new Matrix3d();
        matrixAdjustGeometry.rotX(Math.PI / 2);

        arrowTransform.setTranslation(new Vector3f(0.0f, 0.0f, 0.0f));
        arrowTransform.setScale(0.1);
        arrowTransform.setRotationScale(matrixAdjustGeometry);
        probe.setTransform(arrowTransform);
    }

    /**
     * Create Java3D image scene graph structure.
     *
     * @param  iNumProbes  number of the probe types.
     */
    private void createImageSceneGraph(int iNumProbes) {

        /** probes root branch group */
        probeRootParentBG = new BranchGroup();
        probeRootParentBG.setCapability(Group.ALLOW_CHILDREN_EXTEND);
        probeRootParentBG.setCapability(Group.ALLOW_CHILDREN_READ);
        probeRootParentBG.setCapability(Group.ALLOW_CHILDREN_WRITE);
        probeRootParentBG.setCapability(Node.ALLOW_PICKABLE_READ);
        probeRootParentBG.setCapability(Node.ALLOW_PICKABLE_WRITE);
        probeRootParentBG.setCapability(BranchGroup.ALLOW_DETACH);

        /** probes root transform group */
        probeRootParentTG = new TransformGroup();
        probeRootParentTG.setCapability(TransformGroup.ALLOW_TRANSFORM_READ);
        probeRootParentTG.setCapability(TransformGroup.ALLOW_TRANSFORM_WRITE);
        probeRootParentTG.setCapability(Group.ALLOW_CHILDREN_READ);
        probeRootParentTG.setCapability(Group.ALLOW_CHILDREN_WRITE);
        probeRootParentTG.setCapability(Group.ALLOW_CHILDREN_EXTEND);
        probeRootParentTG.setCapability(BranchGroup.ALLOW_DETACH);

        /** Root branch gropu of the probe */
        probeRootBG = new BranchGroup();
        probeRootBG.setCapability(Group.ALLOW_CHILDREN_EXTEND);
        probeRootBG.setCapability(Group.ALLOW_CHILDREN_READ);
        probeRootBG.setCapability(Group.ALLOW_CHILDREN_WRITE);
        probeRootBG.setCapability(Node.ALLOW_PICKABLE_READ);
        probeRootBG.setCapability(Node.ALLOW_PICKABLE_WRITE);
        probeRootBG.setCapability(BranchGroup.ALLOW_DETACH);

        surfaceRender.getSceneRootTG().addChild(probeRootBG);
        probeRootBG.addChild(probeRootParentTG);

        probeBehaviorBG = new BranchGroup();
        probeBehaviorBG.setCapability(Group.ALLOW_CHILDREN_EXTEND);
        probeBehaviorBG.setCapability(Group.ALLOW_CHILDREN_READ);
        probeBehaviorBG.setCapability(Group.ALLOW_CHILDREN_WRITE);
        probeBehaviorBG.setCapability(BranchGroup.ALLOW_DETACH);

        probeRotate = new MouseRotate(probeRootParentTG, surfaceRender.getSceneRootTG(), true);
        probeRotate.setupCallback(this);
        probeBehaviorBG.addChild(probeRotate);
        probeRotate.setSchedulingBounds(surfaceRender.bounds);
        probeRotate.setFactor(0.0045);

        probeTranslate = new MouseTranslate(probeRootParentTG);
        probeTranslate.setupCallback(this);
        probeBehaviorBG.addChild(probeTranslate);
        probeTranslate.setSchedulingBounds(surfaceRender.bounds);
        probeTranslate.setFactor(0.0045);

        probeZoom = new MouseZoom(probeRootParentTG);
        probeZoom.setupCallback(this);
        probeBehaviorBG.addChild(probeZoom);
        probeZoom.setSchedulingBounds(surfaceRender.bounds);
        probeZoom.setFactor(0.0045);

        probeShapeBG = new BranchGroup[iNumProbes];

        probesRootBG = new BranchGroup();
        probesRootBG.setCapability(BranchGroup.ALLOW_DETACH);
        probesRootBG.setCapability(BranchGroup.ALLOW_CHILDREN_READ);
        probesRootBG.setCapability(BranchGroup.ALLOW_CHILDREN_WRITE);
        probesRootBG.setCapability(BranchGroup.ALLOW_CHILDREN_EXTEND);
        probeRootParentBG.addChild(probesRootBG);

        entryPointBG = new BranchGroup();
        entryPointBG.setCapability(BranchGroup.ALLOW_DETACH);
        entryPointBG.setCapability(BranchGroup.ALLOW_CHILDREN_READ);
        entryPointBG.setCapability(BranchGroup.ALLOW_CHILDREN_WRITE);
        entryPointBG.setCapability(BranchGroup.ALLOW_CHILDREN_EXTEND);

        boneBG = new BranchGroup();
        boneBG.setCapability(BranchGroup.ALLOW_DETACH);
        boneBG.setCapability(BranchGroup.ALLOW_CHILDREN_READ);
        boneBG.setCapability(BranchGroup.ALLOW_CHILDREN_WRITE);
        boneBG.setCapability(BranchGroup.ALLOW_CHILDREN_EXTEND);

        vasculatureBG = new BranchGroup();
        vasculatureBG.setCapability(BranchGroup.ALLOW_DETACH);
        vasculatureBG.setCapability(BranchGroup.ALLOW_CHILDREN_READ);
        vasculatureBG.setCapability(BranchGroup.ALLOW_CHILDREN_WRITE);
        vasculatureBG.setCapability(BranchGroup.ALLOW_CHILDREN_EXTEND);

        indicatorBG = new BranchGroup();
        indicatorBG.setCapability(BranchGroup.ALLOW_DETACH);
        indicatorBG.setCapability(BranchGroup.ALLOW_CHILDREN_READ);
        indicatorBG.setCapability(BranchGroup.ALLOW_CHILDREN_WRITE);
        indicatorBG.setCapability(BranchGroup.ALLOW_CHILDREN_EXTEND);

        indicatorBG.addChild(boneBG);
        indicatorBG.addChild(vasculatureBG);
        indicatorBG.addChild(entryPointBG);

        buildEntryPointBG();
        buildBoneBG();
        buildVasculatureBG();

        surfaceRender.getSceneRootTG().addChild(indicatorBG);

    }

    /**
     * Update the shape of the entry point rotation red line.
     *
     * @param  startPtTransVector  red line start point vector
     * @param  endPtTransVector    red line end point vector
     */
    private void updateRotationRedline(Vector3f startPtTransVector, Vector3f endPtTransVector) {

        LineArray la = new LineArray(2, LineArray.COORDINATES | LineArray.COLOR_3);
        Point3f[] pts = new Point3f[2];
        pts[0] = new Point3f(startPtTransVector.x, startPtTransVector.y, startPtTransVector.z);
        pts[1] = new Point3f(endPtTransVector.x, endPtTransVector.y, endPtTransVector.z);

        Color3f[] clrs = new Color3f[2];
        clrs[0] = new Color3f(1.0f, 0.0f, 0.0f);
        clrs[1] = new Color3f(1.0f, 0.0f, 0.0f);
        la.setCoordinates(0, pts);
        la.setColors(0, clrs);

        nomalRedLineShape.setGeometry(la);

        try {
            pickCanvas.setCapabilities(nomalRedLineShape, PickTool.INTERSECT_FULL);
        } catch (RestrictedAccessException error) { }

        LineArray rotationRedLine = new LineArray(2, LineArray.COORDINATES | LineArray.COLOR_3);
        Point3f[] rotaionRedLinePts = new Point3f[2];
        rotaionRedLinePts[0] = new Point3f(startPtTransVector.x, startPtTransVector.y, startPtTransVector.z);
        rotaionRedLinePts[1] = new Point3f(endPtTransVector.x, endPtTransVector.y, endPtTransVector.z);

        Color3f[] rotationRedLineClrs = new Color3f[2];
        rotationRedLineClrs[0] = new Color3f(1.0f, 0.0f, 0.0f);
        rotationRedLineClrs[1] = new Color3f(1.0f, 0.0f, 0.0f);
        rotationRedLine.setCoordinates(0, rotaionRedLinePts);
        rotationRedLine.setColors(0, rotationRedLineClrs);

        rotationRedLineshape.setGeometry(rotationRedLine);
    }

}
