package gov.nih.mipav.view.renderer.J3D.surfaceview.rfaview;


import com.sun.j3d.utils.geometry.*;
import com.sun.j3d.utils.picking.*;

import java.awt.*;

import javax.media.j3d.*;

import javax.vecmath.*;


/**
 * <p>Title: ThermalProbe</p>
 *
 * <p>Description: Thermal probe has its own geometry and type of burning. This class builds the geometry shape of the
 * thermal probe.</p>
 *
 * @author  Ruida Cheng
 */
public class ThermalProbe {

    //~ Instance fields ------------------------------------------------------------------------------------------------

    /** Agent to set the theraml probe pickable. */
    protected PickCanvas pickCanvas;
    // thermal probe

    /** Attenuation lighting colors. */
    Color3f ambientColor = new Color3f(1.0f, 0.0f, 0.0f);

    /** DOCUMENT ME! */
    Color3f blue = new Color3f(Color.blue);

    /** DOCUMENT ME! */
    Color3f diffuseColor = new Color3f(1.0f, 0.0f, 0.0f);

    /** DOCUMENT ME! */
    Color3f emissiveColor = new Color3f(0.0f, 0.0f, 0.0f);

    /** DOCUMENT ME! */
    Color3f green = new Color3f(Color.green);

    /** DOCUMENT ME! */
    Color3f orange = new Color3f(new Color(225, 50, 0));

    /** Thermal probe colors. */
    Color3f red = new Color3f(Color.red);

    /** DOCUMENT ME! */
    Color3f sepcualarColor = new Color3f(1.0f, 1.0f, 1.0f);

    /** DOCUMENT ME! */
    Color3f yellow = new Color3f(Color.yellow);

    /** DOCUMENT ME! */
    private Appearance appBody;

    /** DOCUMENT ME! */
    private Appearance appConeHead;

    /** DOCUMENT ME! */
    private Appearance appHead;

    /** Thermal probe appearance. */
    private Appearance appTip;

    /** DOCUMENT ME! */
    private Cone coneHeadBottom;

    /** DOCUMENT ME! */
    private Cone coneHeadBottomIndicator;

    /** DOCUMENT ME! */
    private TransformGroup coneHeadBottomIndicatorTG;

    /** DOCUMENT ME! */
    private TransformGroup coneHeadBottomTG;

    /** DOCUMENT ME! */
    private Cone coneHeadTop;

    /** DOCUMENT ME! */
    private Cone coneHeadTopIndicator;

    /** DOCUMENT ME! */
    private TransformGroup coneHeadTopIndicatorTG;

    /** DOCUMENT ME! */
    private TransformGroup coneHeadTopTG;

    /** Thermal probe geometry shapes. */
    private Cone coneTip;

    // indicator probe
    /** Indicator probe geometry shapes. */
    private Cone coneTipIndicator;

    /** Indicator probe geometry shapes' transform groups. */
    private TransformGroup coneTipIndicatorTG;

    /** Thermal probe geometry shapes' transform groups. */
    private TransformGroup coneTipTG;

    /** DOCUMENT ME! */
    private Cylinder cylinderBody;

    /** DOCUMENT ME! */
    private Cylinder cylinderBodyIndicator;

    /** DOCUMENT ME! */
    private TransformGroup cylinderBodyIndicatorTG;

    /** DOCUMENT ME! */
    private TransformGroup cylinderBodyTG;

    /** DOCUMENT ME! */
    private Cylinder cylinderHead;

    /** DOCUMENT ME! */
    private Cylinder cylinderHeadIndicator;

    /** DOCUMENT ME! */
    private TransformGroup cylinderHeadIndicatorTG;

    /** DOCUMENT ME! */
    private TransformGroup cylinderHeadTG;

    /** DOCUMENT ME! */
    private Cylinder cylinderTail;

    /** DOCUMENT ME! */
    private Cylinder cylinderTailIndicator;

    /** DOCUMENT ME! */
    private TransformGroup cylinderTailIndicatorTG;

    /** DOCUMENT ME! */
    private TransformGroup cylinderTailTG;

    /** DOCUMENT ME! */
    private Cylinder cylinderTip;

    /** DOCUMENT ME! */
    private Cylinder cylinderTipIndicator;

    /** DOCUMENT ME! */
    private TransformGroup cylinderTipIndicatorTG;

    /** DOCUMENT ME! */
    private TransformGroup cylinderTipTG;

    /** The root branch group of the indicator probe. */
    private BranchGroup indicatorProbeBG;

    /** Switch group to switch between thermal branch group and the indicator branch group. */
    private Switch switchGroup = new Switch();

    /** The root transform group of the thermal probe. */
    private TransformGroup thermalProbe;

    /** The root branch group of the theraml probe. */
    private BranchGroup thermalProbeBG;

    /** Y original coordinate value. */
    private float yOrigin;

    //~ Constructors ---------------------------------------------------------------------------------------------------

    /**
     * Constructor to initialize the thermal probe geometry.
     */
    public ThermalProbe() {
        init();
    }

    //~ Methods --------------------------------------------------------------------------------------------------------

    /**
     * Enable the probe to rotate around the entry point.
     *
     * @param  flag  boolean <code>true</code> means around the entry point, <code>false</code> around the origin.
     */
    public void enableEntryPointRotation(boolean flag) {

        Transform3D ConeTipT3D = new Transform3D();
        Matrix3d matrixAdjustCylinder = new Matrix3d();
        Transform3D cylinderTipT3D = new Transform3D();
        Transform3D cylinderBodyT3D = new Transform3D();
        Transform3D coneHeadBottomT3D = new Transform3D();
        Matrix3d mAdjustCylinder = new Matrix3d();
        Transform3D cylinderHeadT3D = new Transform3D();
        Transform3D coneHeadTopT3D = new Transform3D();
        Transform3D cylinderTailT3D = new Transform3D();
        Transform3D ConeTipIndicatorT3D = new Transform3D();
        Transform3D cylinderTipIndicatorT3D = new Transform3D();
        Transform3D cylinderBodyIndicatorT3D = new Transform3D();
        Transform3D coneHeadBottomIndicatorT3D = new Transform3D();
        Transform3D cylinderHeadIndicatorT3D = new Transform3D();
        Transform3D coneHeadTopIndicatorT3D = new Transform3D();
        Transform3D cylinderTailIndicatorT3D = new Transform3D();

        if (flag) {
            yOrigin = 1.0f;
        } else {
            yOrigin = 0.0f;
        }

        matrixAdjustCylinder.rotZ(Math.PI);
        ConeTipT3D.setTranslation(new Vector3f(0.0f, 1.025f - yOrigin, 0.0f));
        ConeTipT3D.setRotation(matrixAdjustCylinder);
        coneTipTG.setTransform(ConeTipT3D);

        cylinderTipT3D.set(new Vector3f(0.0f, 1.1f - yOrigin, 0.0f));
        cylinderTipTG.setTransform(cylinderTipT3D);

        cylinderBodyT3D.set(new Vector3f(0.0f, 1.25f - yOrigin, 0.0f));
        cylinderBodyTG.setTransform(cylinderBodyT3D);

        mAdjustCylinder.rotZ(Math.PI);
        coneHeadBottomT3D.setTranslation(new Vector3f(0.0f, 1.35f - yOrigin, 0.0f));
        coneHeadBottomT3D.setRotation(mAdjustCylinder);
        coneHeadBottomTG.setTransform(coneHeadBottomT3D);

        cylinderHeadT3D.set(new Vector3f(0.0f, 1.415f - yOrigin, 0.0f));
        cylinderHeadTG.setTransform(cylinderHeadT3D);

        coneHeadTopT3D.setTranslation(new Vector3f(0.0f, 1.48f - yOrigin, 0.0f));
        coneHeadTopTG.setTransform(coneHeadTopT3D);

        cylinderTailT3D.set(new Vector3f(0.0f, 1.51f - yOrigin, 0.0f));
        cylinderTailTG.setTransform(cylinderTailT3D);

        ConeTipIndicatorT3D.setTranslation(new Vector3f(0.0f, 1.025f - yOrigin, 0.0f));
        ConeTipIndicatorT3D.setRotation(matrixAdjustCylinder);
        coneTipIndicatorTG.setTransform(ConeTipIndicatorT3D);

        cylinderTipIndicatorT3D.set(new Vector3f(0.0f, 1.1f - yOrigin, 0.0f));
        cylinderTipIndicatorTG.setTransform(cylinderTipIndicatorT3D);

        cylinderBodyIndicatorT3D.set(new Vector3f(0.0f, 1.25f - yOrigin, 0.0f));
        cylinderBodyIndicatorTG.setTransform(cylinderBodyIndicatorT3D);

        coneHeadBottomIndicatorT3D.setTranslation(new Vector3f(0.0f, 1.35f - yOrigin, 0.0f));
        coneHeadBottomIndicatorT3D.setRotation(mAdjustCylinder);
        coneHeadBottomIndicatorTG.setTransform(coneHeadBottomIndicatorT3D);

        cylinderHeadIndicatorT3D.set(new Vector3f(0.0f, 1.415f - yOrigin, 0.0f));
        cylinderHeadIndicatorTG.setTransform(cylinderHeadIndicatorT3D);

        coneHeadTopIndicatorT3D.setTranslation(new Vector3f(0.0f, 1.48f - yOrigin, 0.0f));
        coneHeadTopIndicatorTG.setTransform(coneHeadTopIndicatorT3D);

        cylinderTailIndicatorT3D.set(new Vector3f(0.0f, 1.51f - yOrigin, 0.0f));
        cylinderTailIndicatorTG.setTransform(cylinderTailIndicatorT3D);
    }

    /**
     * Get the thermal probe root tranform group.
     *
     * @return  TransformGroup thermal probe.
     */
    public TransformGroup getProbe() {
        return thermalProbe;
    }

    /**
     * Check whether the probe being picked or not.
     *
     * @param   pickedShape  picked geometry shape.
     *
     * @return  boolean <code>true</code> probe picked, <code>false</code> probe not picked.
     */
    protected boolean findProbe(Shape3D pickedShape) {

        if (coneTip.getShape(0) == pickedShape) {
            return true;
        } else if (cylinderTip.getShape(0) == pickedShape) {
            return true;
        } else if (cylinderBody.getShape(0) == pickedShape) {
            return true;
        } else if (coneHeadTop.getShape(0) == pickedShape) {
            return true;
        } else if (coneHeadTop.getShape(0) == pickedShape) {
            return true;
        } else if (coneHeadBottom.getShape(0) == pickedShape) {
            return true;
        } else if (cylinderHead.getShape(0) == pickedShape) {
            return true;
        } else if (cylinderTail.getShape(0) == pickedShape) {
            return true;
        } else {
            return false;
        }
    }

    /**
     * Initialize the therml probe's geometry shapes and indictor probe's geometry shapes.
     */
    protected void init() {

        // Create a group that will be the root of the arrow
        thermalProbe = new TransformGroup();
        thermalProbe.setCapability(TransformGroup.ALLOW_CHILDREN_READ);
        thermalProbe.setCapability(TransformGroup.ALLOW_TRANSFORM_READ);

        thermalProbeBG = new BranchGroup();
        thermalProbeBG.setCapability(Group.ALLOW_CHILDREN_EXTEND);
        thermalProbeBG.setCapability(Group.ALLOW_CHILDREN_READ);
        thermalProbeBG.setCapability(Group.ALLOW_CHILDREN_WRITE);
        thermalProbeBG.setCapability(Node.ALLOW_PICKABLE_READ);
        thermalProbeBG.setCapability(Node.ALLOW_PICKABLE_WRITE);

        indicatorProbeBG = new BranchGroup();
        indicatorProbeBG.setCapability(Group.ALLOW_CHILDREN_EXTEND);
        indicatorProbeBG.setCapability(Group.ALLOW_CHILDREN_READ);
        indicatorProbeBG.setCapability(Group.ALLOW_CHILDREN_WRITE);
        indicatorProbeBG.setCapability(Node.ALLOW_PICKABLE_READ);
        indicatorProbeBG.setCapability(Node.ALLOW_PICKABLE_WRITE);

        thermalProbe.addChild(switchGroup);
        switchGroup.addChild(thermalProbeBG);
        switchGroup.addChild(indicatorProbeBG);
        switchGroup.setWhichChild(Switch.CHILD_NONE);
        switchGroup.setCapability(Switch.ALLOW_SWITCH_WRITE);
        switchGroup.setCapability(Switch.ALLOW_CHILDREN_WRITE);
        switchGroup.setCapability(Switch.ALLOW_CHILDREN_READ);
        switchGroup.setWhichChild(0);

        // Create a thermal probe tip
        appTip = new Appearance();

        Material matTip = new Material(emissiveColor, emissiveColor, yellow, sepcualarColor, 50.0f);

        matTip.setCapability(Material.ALLOW_COMPONENT_READ);
        matTip.setCapability(Material.ALLOW_COMPONENT_WRITE);
        appTip.setMaterial(matTip);

        // cone tip
        coneTip = new Cone(0.005f, 0.05f, Primitive.GENERATE_NORMALS, appTip);
        coneTip.getShape(0).setCapability(Shape3D.ALLOW_APPEARANCE_READ);
        coneTip.getShape(0).setCapability(Shape3D.ALLOW_APPEARANCE_WRITE);
        coneTip.getShape(0).setCapability(Shape3D.ALLOW_GEOMETRY_WRITE);
        coneTip.getShape(0).setCapability(Shape3D.ALLOW_GEOMETRY_READ);
        coneTip.getShape(0).setCapability(Geometry.ALLOW_INTERSECT);
        coneTip.getShape(0).setAppearanceOverrideEnable(false);

        Transform3D ConeTipT3D = new Transform3D();
        Matrix3d matrixAdjustCylinder = new Matrix3d();

        matrixAdjustCylinder.rotZ(Math.PI);
        ConeTipT3D.setTranslation(new Vector3f(0.0f, 1.025f, 0.0f));
        ConeTipT3D.setRotation(matrixAdjustCylinder);
        coneTipTG = new TransformGroup(ConeTipT3D);
        coneTipTG.setCapability(TransformGroup.ALLOW_TRANSFORM_READ);
        coneTipTG.setCapability(TransformGroup.ALLOW_TRANSFORM_WRITE);
        coneTipTG.setCapability(TransformGroup.ALLOW_CHILDREN_READ);

        // Add the cone to the cone transform group
        coneTipTG.addChild(coneTip);
        thermalProbeBG.addChild(coneTipTG);

        // cylinder tip
        cylinderTip = new Cylinder(0.005f, 0.1f, Primitive.GENERATE_NORMALS, appTip);
        cylinderTip.getShape(0).setCapability(Shape3D.ALLOW_APPEARANCE_READ);
        cylinderTip.getShape(0).setCapability(Shape3D.ALLOW_APPEARANCE_WRITE);
        cylinderTip.getShape(0).setCapability(Shape3D.ALLOW_GEOMETRY_WRITE);
        cylinderTip.getShape(0).setCapability(Shape3D.ALLOW_GEOMETRY_READ);
        cylinderTip.getShape(0).setCapability(Geometry.ALLOW_INTERSECT);
        cylinderTip.getShape(0).setAppearanceOverrideEnable(false);

        Transform3D cylinderTipT3D = new Transform3D();

        cylinderTipT3D.set(new Vector3f(0.0f, 1.1f, 0.0f));
        cylinderTipTG = new TransformGroup(cylinderTipT3D);
        cylinderTipTG.setCapability(TransformGroup.ALLOW_CHILDREN_READ);
        cylinderTipTG.setCapability(TransformGroup.ALLOW_TRANSFORM_READ);
        cylinderTipTG.setCapability(TransformGroup.ALLOW_TRANSFORM_WRITE);
        cylinderTipTG.addChild(cylinderTip);
        thermalProbeBG.addChild(cylinderTipTG);

        // Create thermal probe body
        appBody = new Appearance();

        Material matBody = new Material(emissiveColor, emissiveColor, blue, sepcualarColor, 50.0f);
        matBody.setCapability(Material.ALLOW_COMPONENT_READ);
        matBody.setCapability(Material.ALLOW_COMPONENT_WRITE);
        appBody.setMaterial(matBody);

        cylinderBody = new Cylinder(0.005f, 0.2f, Primitive.GENERATE_NORMALS, appBody);
        cylinderBody.getShape(0).setCapability(Shape3D.ALLOW_APPEARANCE_READ);
        cylinderBody.getShape(0).setCapability(Shape3D.ALLOW_APPEARANCE_WRITE);
        cylinderBody.getShape(0).setCapability(Shape3D.ALLOW_GEOMETRY_WRITE);
        cylinderBody.getShape(0).setCapability(Shape3D.ALLOW_GEOMETRY_READ);
        cylinderBody.getShape(0).setCapability(Geometry.ALLOW_INTERSECT);
        cylinderBody.getShape(0).setAppearanceOverrideEnable(false);

        Transform3D cylinderBodyT3D = new Transform3D();

        cylinderBodyT3D.set(new Vector3f(0.0f, 1.25f, 0.0f));
        cylinderBodyTG = new TransformGroup(cylinderBodyT3D);
        cylinderBodyTG.setCapability(TransformGroup.ALLOW_CHILDREN_READ);
        cylinderBodyTG.setCapability(TransformGroup.ALLOW_TRANSFORM_READ);
        cylinderBodyTG.setCapability(TransformGroup.ALLOW_TRANSFORM_WRITE);
        cylinderBodyTG.addChild(cylinderBody);
        thermalProbeBG.addChild(cylinderBodyTG);

        // Create thermal probe head
        appConeHead = new Appearance();

        Material matConHead = new Material(emissiveColor, emissiveColor, red, sepcualarColor, 50.0f);

        matConHead.setCapability(Material.ALLOW_COMPONENT_READ);
        matConHead.setCapability(Material.ALLOW_COMPONENT_WRITE);
        appConeHead.setMaterial(matConHead);

        // cone head bottom
        coneHeadBottom = new Cone(0.01f, 0.03f, Primitive.GENERATE_NORMALS, appConeHead);
        coneHeadBottom.getShape(0).setCapability(Shape3D.ALLOW_APPEARANCE_READ);
        coneHeadBottom.getShape(0).setCapability(Shape3D.ALLOW_APPEARANCE_WRITE);
        coneHeadBottom.getShape(0).setCapability(Shape3D.ALLOW_GEOMETRY_WRITE);
        coneHeadBottom.getShape(0).setCapability(Shape3D.ALLOW_GEOMETRY_READ);
        coneHeadBottom.getShape(0).setCapability(Geometry.ALLOW_INTERSECT);
        coneHeadBottom.getShape(0).setAppearanceOverrideEnable(false);

        Transform3D coneHeadBottomT3D = new Transform3D();
        Matrix3d mAdjustCylinder = new Matrix3d();

        mAdjustCylinder.rotZ(Math.PI);
        coneHeadBottomT3D.setTranslation(new Vector3f(0.0f, 1.35f, 0.0f));
        coneHeadBottomT3D.setRotation(mAdjustCylinder);
        coneHeadBottomTG = new TransformGroup(coneHeadBottomT3D);
        coneHeadBottomTG.setCapability(TransformGroup.ALLOW_TRANSFORM_READ);
        coneHeadBottomTG.setCapability(TransformGroup.ALLOW_TRANSFORM_WRITE);
        coneHeadBottomTG.setCapability(TransformGroup.ALLOW_CHILDREN_READ);
        coneHeadBottomTG.addChild(coneHeadBottom);
        thermalProbeBG.addChild(coneHeadBottomTG);

        appHead = new Appearance();

        Material matHead = new Material(emissiveColor, emissiveColor, orange, sepcualarColor, 50.0f);

        matHead.setCapability(Material.ALLOW_COMPONENT_READ);
        matHead.setCapability(Material.ALLOW_COMPONENT_WRITE);
        appHead.setMaterial(matHead);

        cylinderHead = new Cylinder(0.01f, 0.1f, Primitive.GENERATE_NORMALS, appHead);
        cylinderHead.getShape(0).setCapability(Shape3D.ALLOW_APPEARANCE_READ);
        cylinderHead.getShape(0).setCapability(Shape3D.ALLOW_APPEARANCE_WRITE);
        cylinderHead.getShape(0).setCapability(Shape3D.ALLOW_GEOMETRY_WRITE);
        cylinderHead.getShape(0).setCapability(Shape3D.ALLOW_GEOMETRY_READ);
        cylinderHead.getShape(0).setCapability(Geometry.ALLOW_INTERSECT);
        cylinderHead.getShape(0).setAppearanceOverrideEnable(false);

        Transform3D cylinderHeadT3D = new Transform3D();

        cylinderHeadT3D.set(new Vector3f(0.0f, 1.415f, 0.0f));
        cylinderHeadTG = new TransformGroup(cylinderHeadT3D);
        cylinderHeadTG.setCapability(TransformGroup.ALLOW_CHILDREN_READ);
        cylinderHeadTG.setCapability(TransformGroup.ALLOW_TRANSFORM_READ);
        cylinderHeadTG.setCapability(TransformGroup.ALLOW_TRANSFORM_WRITE);
        cylinderHeadTG.addChild(cylinderHead);
        thermalProbeBG.addChild(cylinderHeadTG);

        coneHeadTop = new Cone(0.01f, 0.03f, Primitive.GENERATE_NORMALS, appConeHead);
        coneHeadTop.getShape(0).setCapability(Shape3D.ALLOW_APPEARANCE_READ);
        coneHeadTop.getShape(0).setCapability(Shape3D.ALLOW_APPEARANCE_WRITE);
        coneHeadTop.getShape(0).setCapability(Shape3D.ALLOW_GEOMETRY_WRITE);
        coneHeadTop.getShape(0).setCapability(Shape3D.ALLOW_GEOMETRY_READ);
        coneHeadTop.getShape(0).setCapability(Geometry.ALLOW_INTERSECT);
        coneHeadTop.getShape(0).setAppearanceOverrideEnable(false);

        Transform3D coneHeadTopT3D = new Transform3D();

        coneHeadTopT3D.setTranslation(new Vector3f(0.0f, 1.48f, 0.0f));
        coneHeadTopTG = new TransformGroup(coneHeadTopT3D);
        coneHeadTopTG.setCapability(TransformGroup.ALLOW_TRANSFORM_READ);
        coneHeadTopTG.setCapability(TransformGroup.ALLOW_TRANSFORM_WRITE);
        coneHeadTopTG.setCapability(TransformGroup.ALLOW_CHILDREN_READ);
        coneHeadTopTG.addChild(coneHeadTop);
        thermalProbeBG.addChild(coneHeadTopTG);

        cylinderTail = new Cylinder(0.005f, 0.05f, Primitive.GENERATE_NORMALS, appBody);
        cylinderTail.getShape(0).setCapability(Shape3D.ALLOW_APPEARANCE_READ);
        cylinderTail.getShape(0).setCapability(Shape3D.ALLOW_APPEARANCE_WRITE);
        cylinderTail.getShape(0).setCapability(Shape3D.ALLOW_GEOMETRY_WRITE);
        cylinderTail.getShape(0).setCapability(Shape3D.ALLOW_GEOMETRY_READ);
        cylinderTail.getShape(0).setCapability(Geometry.ALLOW_INTERSECT);
        cylinderTail.getShape(0).setAppearanceOverrideEnable(false);

        Transform3D cylinderTailT3D = new Transform3D();

        cylinderTailT3D.set(new Vector3f(0.0f, 1.51f, 0.0f));
        cylinderTailTG = new TransformGroup(cylinderTailT3D);
        cylinderTailTG.setCapability(TransformGroup.ALLOW_CHILDREN_READ);
        cylinderTailTG.setCapability(TransformGroup.ALLOW_TRANSFORM_READ);
        cylinderTailTG.setCapability(TransformGroup.ALLOW_TRANSFORM_WRITE);
        cylinderTailTG.addChild(cylinderTail);
        thermalProbeBG.addChild(cylinderTailTG);


        // set pickable
        try {
            pickCanvas.setCapabilities(coneTip.getShape(0), PickTool.INTERSECT_FULL);
            pickCanvas.setCapabilities(cylinderTip.getShape(0), PickTool.INTERSECT_FULL);
            pickCanvas.setCapabilities(cylinderBody.getShape(0), PickTool.INTERSECT_FULL);
            pickCanvas.setCapabilities(coneHeadTop.getShape(0), PickTool.INTERSECT_FULL);
            pickCanvas.setCapabilities(coneHeadBottom.getShape(0), PickTool.INTERSECT_FULL);
            pickCanvas.setCapabilities(cylinderHead.getShape(0), PickTool.INTERSECT_FULL);
            pickCanvas.setCapabilities(cylinderTail.getShape(0), PickTool.INTERSECT_FULL);
        } catch (RestrictedAccessException error) { }

        // Create indicator probe
        Material mat;
        Appearance app = new Appearance();
        mat = new Material(emissiveColor, emissiveColor, green, sepcualarColor, 50.0f);
        mat.setCapability(Material.ALLOW_COMPONENT_READ);
        mat.setCapability(Material.ALLOW_COMPONENT_WRITE);
        app.setMaterial(mat);

        // cone tip
        coneTipIndicator = new Cone(0.005f, 0.05f, Primitive.GENERATE_NORMALS, app);
        coneTipIndicator.getShape(0).setCapability(Shape3D.ALLOW_APPEARANCE_READ);
        coneTipIndicator.getShape(0).setCapability(Shape3D.ALLOW_APPEARANCE_WRITE);
        coneTipIndicator.getShape(0).setCapability(Shape3D.ALLOW_GEOMETRY_WRITE);
        coneTipIndicator.getShape(0).setCapability(Shape3D.ALLOW_GEOMETRY_READ);
        coneTipIndicator.getShape(0).setCapability(Geometry.ALLOW_INTERSECT);
        coneTipIndicator.getShape(0).setAppearanceOverrideEnable(false);

        Transform3D ConeTipIndicatorT3D = new Transform3D();
        ConeTipIndicatorT3D.setTranslation(new Vector3f(0.0f, 1.025f, 0.0f));
        ConeTipIndicatorT3D.setRotation(matrixAdjustCylinder);

        coneTipIndicatorTG = new TransformGroup(ConeTipIndicatorT3D);
        coneTipIndicatorTG.setCapability(TransformGroup.ALLOW_TRANSFORM_READ);
        coneTipIndicatorTG.setCapability(TransformGroup.ALLOW_TRANSFORM_WRITE);
        coneTipIndicatorTG.setCapability(TransformGroup.ALLOW_CHILDREN_READ);

        // Add the cone to the cone transform group
        coneTipIndicatorTG.addChild(coneTipIndicator);
        indicatorProbeBG.addChild(coneTipIndicatorTG);

        // cylinder tip
        cylinderTipIndicator = new Cylinder(0.005f, 0.1f, Primitive.GENERATE_NORMALS, app);
        cylinderTipIndicator.getShape(0).setCapability(Shape3D.ALLOW_APPEARANCE_READ);
        cylinderTipIndicator.getShape(0).setCapability(Shape3D.ALLOW_APPEARANCE_WRITE);
        cylinderTipIndicator.getShape(0).setCapability(Shape3D.ALLOW_GEOMETRY_WRITE);
        cylinderTipIndicator.getShape(0).setCapability(Shape3D.ALLOW_GEOMETRY_READ);
        cylinderTipIndicator.getShape(0).setCapability(Geometry.ALLOW_INTERSECT);
        cylinderTipIndicator.getShape(0).setAppearanceOverrideEnable(false);

        Transform3D cylinderTipIndicatorT3D = new Transform3D();
        cylinderTipIndicatorT3D.set(new Vector3f(0.0f, 1.1f, 0.0f));
        cylinderTipIndicatorTG = new TransformGroup(cylinderTipIndicatorT3D);
        cylinderTipIndicatorTG.setCapability(TransformGroup.ALLOW_CHILDREN_READ);
        cylinderTipIndicatorTG.setCapability(TransformGroup.ALLOW_TRANSFORM_READ);
        cylinderTipIndicatorTG.setCapability(TransformGroup.ALLOW_TRANSFORM_WRITE);
        cylinderTipIndicatorTG.addChild(cylinderTipIndicator);
        indicatorProbeBG.addChild(cylinderTipIndicatorTG);

        // Create indicator probe body
        cylinderBodyIndicator = new Cylinder(0.005f, 0.2f, Primitive.GENERATE_NORMALS, app);
        cylinderBodyIndicator.getShape(0).setCapability(Shape3D.ALLOW_APPEARANCE_READ);
        cylinderBodyIndicator.getShape(0).setCapability(Shape3D.ALLOW_APPEARANCE_WRITE);
        cylinderBodyIndicator.getShape(0).setCapability(Shape3D.ALLOW_GEOMETRY_WRITE);
        cylinderBodyIndicator.getShape(0).setCapability(Shape3D.ALLOW_GEOMETRY_READ);
        cylinderBodyIndicator.getShape(0).setCapability(Geometry.ALLOW_INTERSECT);
        cylinderBodyIndicator.getShape(0).setAppearanceOverrideEnable(false);


        Transform3D cylinderBodyIndicatorT3D = new Transform3D();
        cylinderBodyIndicatorT3D.set(new Vector3f(0.0f, 1.25f, 0.0f));
        cylinderBodyIndicatorTG = new TransformGroup(cylinderBodyIndicatorT3D);
        cylinderBodyIndicatorTG.setCapability(TransformGroup.ALLOW_CHILDREN_READ);
        cylinderBodyIndicatorTG.setCapability(TransformGroup.ALLOW_TRANSFORM_READ);
        cylinderBodyIndicatorTG.setCapability(TransformGroup.ALLOW_TRANSFORM_WRITE);
        cylinderBodyIndicatorTG.addChild(cylinderBodyIndicator);
        indicatorProbeBG.addChild(cylinderBodyIndicatorTG);

        // cone head bottom
        coneHeadBottomIndicator = new Cone(0.01f, 0.03f, Primitive.GENERATE_NORMALS, app);
        coneHeadBottomIndicator.getShape(0).setCapability(Shape3D.ALLOW_APPEARANCE_READ);
        coneHeadBottomIndicator.getShape(0).setCapability(Shape3D.ALLOW_APPEARANCE_WRITE);
        coneHeadBottomIndicator.getShape(0).setCapability(Shape3D.ALLOW_GEOMETRY_WRITE);
        coneHeadBottomIndicator.getShape(0).setCapability(Shape3D.ALLOW_GEOMETRY_READ);
        coneHeadBottomIndicator.getShape(0).setCapability(Geometry.ALLOW_INTERSECT);
        coneHeadBottomIndicator.getShape(0).setAppearanceOverrideEnable(false);


        Transform3D coneHeadBottomIndicatorT3D = new Transform3D();
        coneHeadBottomIndicatorT3D.setTranslation(new Vector3f(0.0f, 1.35f, 0.0f));
        coneHeadBottomIndicatorT3D.setRotation(mAdjustCylinder);
        coneHeadBottomIndicatorTG = new TransformGroup(coneHeadBottomIndicatorT3D);
        coneHeadBottomIndicatorTG.setCapability(TransformGroup.ALLOW_TRANSFORM_READ);
        coneHeadBottomIndicatorTG.setCapability(TransformGroup.ALLOW_TRANSFORM_WRITE);
        coneHeadBottomIndicatorTG.setCapability(TransformGroup.ALLOW_CHILDREN_READ);
        coneHeadBottomIndicatorTG.addChild(coneHeadBottomIndicator);
        indicatorProbeBG.addChild(coneHeadBottomIndicatorTG);

        cylinderHeadIndicator = new Cylinder(0.01f, 0.1f, Primitive.GENERATE_NORMALS, app);
        cylinderHeadIndicator.getShape(0).setCapability(Shape3D.ALLOW_APPEARANCE_READ);
        cylinderHeadIndicator.getShape(0).setCapability(Shape3D.ALLOW_APPEARANCE_WRITE);
        cylinderHeadIndicator.getShape(0).setCapability(Shape3D.ALLOW_GEOMETRY_WRITE);
        cylinderHeadIndicator.getShape(0).setCapability(Shape3D.ALLOW_GEOMETRY_READ);
        cylinderHeadIndicator.getShape(0).setCapability(Geometry.ALLOW_INTERSECT);
        cylinderHeadIndicator.getShape(0).setAppearanceOverrideEnable(false);


        Transform3D cylinderHeadIndicatorT3D = new Transform3D();
        cylinderHeadIndicatorT3D.set(new Vector3f(0.0f, 1.415f, 0.0f));
        cylinderHeadIndicatorTG = new TransformGroup(cylinderHeadIndicatorT3D);
        cylinderHeadIndicatorTG.setCapability(TransformGroup.ALLOW_CHILDREN_READ);
        cylinderHeadIndicatorTG.setCapability(TransformGroup.ALLOW_TRANSFORM_READ);
        cylinderHeadIndicatorTG.setCapability(TransformGroup.ALLOW_TRANSFORM_WRITE);
        cylinderHeadIndicatorTG.addChild(cylinderHeadIndicator);
        indicatorProbeBG.addChild(cylinderHeadIndicatorTG);

        coneHeadTopIndicator = new Cone(0.01f, 0.03f, Primitive.GENERATE_NORMALS, app);
        coneHeadTopIndicator.getShape(0).setCapability(Shape3D.ALLOW_APPEARANCE_READ);
        coneHeadTopIndicator.getShape(0).setCapability(Shape3D.ALLOW_APPEARANCE_WRITE);
        coneHeadTopIndicator.getShape(0).setCapability(Shape3D.ALLOW_GEOMETRY_WRITE);
        coneHeadTopIndicator.getShape(0).setCapability(Shape3D.ALLOW_GEOMETRY_READ);
        coneHeadTopIndicator.getShape(0).setCapability(Geometry.ALLOW_INTERSECT);
        coneHeadTopIndicator.getShape(0).setAppearanceOverrideEnable(false);

        Transform3D coneHeadTopIndicatorT3D = new Transform3D();
        coneHeadTopIndicatorT3D.setTranslation(new Vector3f(0.0f, 1.48f, 0.0f));
        coneHeadTopIndicatorTG = new TransformGroup(coneHeadTopIndicatorT3D);
        coneHeadTopIndicatorTG.setCapability(TransformGroup.ALLOW_TRANSFORM_READ);
        coneHeadTopIndicatorTG.setCapability(TransformGroup.ALLOW_TRANSFORM_WRITE);
        coneHeadTopIndicatorTG.setCapability(TransformGroup.ALLOW_CHILDREN_READ);
        coneHeadTopIndicatorTG.addChild(coneHeadTopIndicator);
        indicatorProbeBG.addChild(coneHeadTopIndicatorTG);

        cylinderTailIndicator = new Cylinder(0.005f, 0.05f, Primitive.GENERATE_NORMALS, app);
        cylinderTailIndicator.getShape(0).setCapability(Shape3D.ALLOW_APPEARANCE_READ);
        cylinderTailIndicator.getShape(0).setCapability(Shape3D.ALLOW_APPEARANCE_WRITE);
        cylinderTailIndicator.getShape(0).setCapability(Shape3D.ALLOW_GEOMETRY_WRITE);
        cylinderTailIndicator.getShape(0).setCapability(Shape3D.ALLOW_GEOMETRY_READ);
        cylinderTailIndicator.getShape(0).setCapability(Geometry.ALLOW_INTERSECT);
        cylinderTailIndicator.getShape(0).setAppearanceOverrideEnable(false);

        Transform3D cylinderTailIndicatorT3D = new Transform3D();
        cylinderTailIndicatorT3D.set(new Vector3f(0.0f, 1.51f, 0.0f));
        cylinderTailIndicatorTG = new TransformGroup(cylinderTailIndicatorT3D);
        cylinderTailIndicatorTG.setCapability(TransformGroup.ALLOW_CHILDREN_READ);
        cylinderTailIndicatorTG.setCapability(TransformGroup.ALLOW_TRANSFORM_READ);
        cylinderTailIndicatorTG.setCapability(TransformGroup.ALLOW_TRANSFORM_WRITE);
        cylinderTailIndicatorTG.addChild(cylinderTailIndicator);
        indicatorProbeBG.addChild(cylinderTailIndicatorTG);
    }

    /**
     * Set the probe translation in the local coordinate system.
     *
     * @param  value  y coordinate value of the local coordinate system.
     */
    protected void setProbeCoordinate(float value) {
        Transform3D transConeTip = new Transform3D();
        Vector3f vConeTip = new Vector3f();

        coneTipTG.getTransform(transConeTip);
        transConeTip.get(vConeTip);
        vConeTip.y = (1.025f - yOrigin - value);
        transConeTip.setTranslation(vConeTip);
        coneTipTG.setTransform(transConeTip);

        Transform3D transCylinderTip = new Transform3D();
        Vector3f vCylinderTip = new Vector3f();

        cylinderTipTG.getTransform(transCylinderTip);
        transCylinderTip.get(vCylinderTip);
        vCylinderTip.y = (1.1f - yOrigin - value);
        transCylinderTip.setTranslation(vCylinderTip);
        cylinderTipTG.setTransform(transCylinderTip);

        Transform3D transCylinderBody = new Transform3D();
        Vector3f vCylinderBody = new Vector3f();

        cylinderBodyTG.getTransform(transCylinderBody);
        transCylinderBody.get(vCylinderBody);
        vCylinderBody.y = (1.25f - yOrigin - value);
        transCylinderBody.setTranslation(vCylinderBody);
        cylinderBodyTG.setTransform(transCylinderBody);

        Transform3D transConeHeadTop = new Transform3D();
        Vector3f vConeHeadTop = new Vector3f();

        coneHeadTopTG.getTransform(transConeHeadTop);
        transConeHeadTop.get(vConeHeadTop);
        vConeHeadTop.y = (1.48f - yOrigin - value);
        transConeHeadTop.setTranslation(vConeHeadTop);
        coneHeadTopTG.setTransform(transConeHeadTop);

        Transform3D transConeHeadBottom = new Transform3D();
        Vector3f vConeHeadBottom = new Vector3f();

        coneHeadBottomTG.getTransform(transConeHeadBottom);
        transConeHeadBottom.get(vConeHeadBottom);
        vConeHeadBottom.y = (1.35f - yOrigin - value);
        transConeHeadBottom.setTranslation(vConeHeadBottom);
        coneHeadBottomTG.setTransform(transConeHeadBottom);

        Transform3D transCylinderHead = new Transform3D();
        Vector3f vCylinderHead = new Vector3f();

        cylinderHeadTG.getTransform(transCylinderHead);
        transCylinderHead.get(vCylinderHead);
        vCylinderHead.y = (1.415f - yOrigin - value);
        transCylinderHead.setTranslation(vCylinderHead);
        cylinderHeadTG.setTransform(transCylinderHead);

        Transform3D transCylinderTail = new Transform3D();
        Vector3f vCylinderTail = new Vector3f();

        cylinderTailTG.getTransform(transCylinderTail);
        transCylinderTail.get(vCylinderTail);
        vCylinderTail.y = (1.51f - yOrigin - value);
        transCylinderTail.setTranslation(vCylinderTail);
        cylinderTailTG.setTransform(transCylinderTail);

        // indicator probe
        coneTipIndicatorTG.setTransform(transConeTip);
        cylinderTipIndicatorTG.setTransform(transCylinderTip);
        cylinderBodyIndicatorTG.setTransform(transCylinderBody);
        coneHeadTopIndicatorTG.setTransform(transConeHeadTop);
        coneHeadBottomIndicatorTG.setTransform(transConeHeadBottom);
        cylinderHeadIndicatorTG.setTransform(transCylinderHead);
        cylinderTailIndicatorTG.setTransform(transCylinderTail);
    }

    /**
     * Switch the default probe between thermal probe and the indicator probe.
     *
     * @param  flag  boolean <code>true</code> green color, <code>false</code> theraml probe color.
     */
    protected void setProbeGreenColor(boolean flag) {

        if (flag) {
            switchGroup.setWhichChild(1);
        } else {
            switchGroup.setWhichChild(0);
        }

    }
}
