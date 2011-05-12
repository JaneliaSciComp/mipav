package gov.nih.mipav.view.renderer.J3D.surfaceview.rfaview;


import com.sun.j3d.utils.geometry.*;
import com.sun.j3d.utils.picking.*;

import java.awt.*;

import javax.media.j3d.*;

import javax.vecmath.*;


/**
 * <p>Title: RegualorProbe</p>
 *
 * <p>Description: Regular probe has its own geometry and type of burning. This class builds the geometry shape of the
 * regular probe.</p>
 *
 * @author  Ruida Cheng
 */
public class RegularProbe {

    //~ Instance fields ------------------------------------------------------------------------------------------------

    /** Agent to set the regular probe pickable. */
    protected PickCanvas pickCanvas;

    /** Attenuation lighting setup. */
    Color3f ambientColor = new Color3f(1.0f, 0.0f, 0.0f);

    /** DOCUMENT ME! */
    Color3f diffuseColor = new Color3f(1.0f, 0.0f, 0.0f);

    /** DOCUMENT ME! */
    Color3f emissiveColor = new Color3f(0.0f, 0.0f, 0.0f);

    /** The indicator probe's green color. */
    Color3f green = new Color3f(Color.green);

    /** DOCUMENT ME! */
    Color3f sepcualarColor = new Color3f(1.0f, 1.0f, 1.0f);

    /** The regular probe's gray color. */
    Color3f yellow = new Color3f(Color.yellow);

    /** DOCUMENT ME! */
    private Appearance appTip;

    /** Indicator probe cone shape. */
    private Cone coneIndicator;

    /** Tranformgroup of the indicator cone shape. */
    private TransformGroup coneIndicatorTG;

    /** Cone geometry shape part of the regular probe. */
    private Cone coneTip;

    /** The probe tip cone geometry shape. */
    private TransformGroup coneTipTG;

    /**
     * Indicator is used by the regular probe to switch color between gray and green. Indicator is the green color
     * probe. I prefer switching the branch group rather than changing the java3d geometry appearance because the
     * appearance changes waste time and memory.
     */
    /** Indicator probe cylinder shape. */
    private Cylinder cylinderIndicator;

    /** Transformgroup of the indicator cylinder shape. */
    private TransformGroup cylinderIndicatorTG;

    /** Cylinder geometry shape part of the regular probe. */
    private Cylinder cylinderTip;

    /** The probe tip cylinder geometry shape. */
    private TransformGroup cylinderTipTG;

    /** Indicator probe branch group. */
    private BranchGroup indicatorProbeBG;

    /** The root transform group the the regular probe. */
    private TransformGroup regularProbe;

    /** Regular probe branch group. */
    private BranchGroup regularProbeBG;

    /** The switch group that switch between regular probe and indicator probe. */
    private Switch switchGroup = new Switch();

    /** Y original coordinate value. */
    private float yOrigin;

    //~ Constructors ---------------------------------------------------------------------------------------------------

    /**
     * Constructor to build the regular probe geometry shape.
     */
    public RegularProbe() {
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
        Transform3D ConeIndicatorT3D = new Transform3D();

        if (flag) {
            yOrigin = 1.0f;
        } else {
            yOrigin = 0.0f;
        }

        matrixAdjustCylinder.rotZ(Math.PI);
        ConeTipT3D.setTranslation(new Vector3f(0.0f, 1.05f - yOrigin, 0.0f));
        ConeTipT3D.setRotation(matrixAdjustCylinder);
        coneTipTG.setTransform(ConeTipT3D);

        cylinderTipT3D.set(new Vector3f(0.0f, 1.4f - yOrigin, 0.0f));
        cylinderTipTG.setTransform(cylinderTipT3D);

        ConeIndicatorT3D.setTranslation(new Vector3f(0.0f, 1.05f - yOrigin, 0.0f));
        ConeIndicatorT3D.setRotation(matrixAdjustCylinder);
        coneIndicatorTG.setTransform(ConeIndicatorT3D);

        Transform3D cylinderIndicatorT3D = new Transform3D();

        cylinderIndicatorT3D.set(new Vector3f(0.0f, 1.4f - yOrigin, 0.0f));
        cylinderIndicatorTG.setTransform(cylinderIndicatorT3D);
    }

    /**
     * Get the root transform group of the regular probe.
     *
     * @return  TransformGroup the root of the regular probe.
     */
    public TransformGroup getProbe() {
        return regularProbe;
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
        } else {
            return false;
        }
    }

    /**
     * Initialize the regular probe geometry shape.
     */
    protected void init() {

        // Create a group that will be the root of the arrow
        regularProbe = new TransformGroup();
        regularProbe.setCapability(TransformGroup.ALLOW_CHILDREN_READ);
        regularProbe.setCapability(TransformGroup.ALLOW_TRANSFORM_READ);

        regularProbeBG = new BranchGroup();
        regularProbeBG.setCapability(Group.ALLOW_CHILDREN_EXTEND);
        regularProbeBG.setCapability(Group.ALLOW_CHILDREN_READ);
        regularProbeBG.setCapability(Group.ALLOW_CHILDREN_WRITE);
        regularProbeBG.setCapability(Node.ALLOW_PICKABLE_READ);
        regularProbeBG.setCapability(Node.ALLOW_PICKABLE_WRITE);

        indicatorProbeBG = new BranchGroup();
        indicatorProbeBG.setCapability(Group.ALLOW_CHILDREN_EXTEND);
        indicatorProbeBG.setCapability(Group.ALLOW_CHILDREN_READ);
        indicatorProbeBG.setCapability(Group.ALLOW_CHILDREN_WRITE);
        indicatorProbeBG.setCapability(Node.ALLOW_PICKABLE_READ);
        indicatorProbeBG.setCapability(Node.ALLOW_PICKABLE_WRITE);

        regularProbe.addChild(switchGroup);
        switchGroup.addChild(regularProbeBG);
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
        coneTip = new Cone(0.005f, 0.1f, Primitive.GENERATE_NORMALS, appTip);
        coneTip.getShape(0).setCapability(Shape3D.ALLOW_APPEARANCE_READ);
        coneTip.getShape(0).setCapability(Shape3D.ALLOW_APPEARANCE_WRITE);
        coneTip.getShape(0).setCapability(Shape3D.ALLOW_GEOMETRY_WRITE);
        coneTip.getShape(0).setCapability(Shape3D.ALLOW_GEOMETRY_READ);
        coneTip.getShape(0).setCapability(Geometry.ALLOW_INTERSECT);
        coneTip.getShape(0).setAppearanceOverrideEnable(false);

        Transform3D ConeTipT3D = new Transform3D();
        Matrix3d matrixAdjustCylinder = new Matrix3d();

        matrixAdjustCylinder.rotZ(Math.PI);
        ConeTipT3D.setTranslation(new Vector3f(0.0f, 1.05f, 0.0f));
        ConeTipT3D.setRotation(matrixAdjustCylinder);
        coneTipTG = new TransformGroup(ConeTipT3D);
        coneTipTG.setCapability(TransformGroup.ALLOW_TRANSFORM_READ);
        coneTipTG.setCapability(TransformGroup.ALLOW_TRANSFORM_WRITE);
        coneTipTG.setCapability(TransformGroup.ALLOW_CHILDREN_READ);

        // Add the cone to the cone transform group
        coneTipTG.addChild(coneTip);
        regularProbeBG.addChild(coneTipTG);

        // cylinder tip
        cylinderTip = new Cylinder(0.005f, 0.7f, Primitive.GENERATE_NORMALS, appTip);
        cylinderTip.getShape(0).setCapability(Shape3D.ALLOW_APPEARANCE_READ);
        cylinderTip.getShape(0).setCapability(Shape3D.ALLOW_APPEARANCE_WRITE);
        cylinderTip.getShape(0).setCapability(Shape3D.ALLOW_GEOMETRY_WRITE);
        cylinderTip.getShape(0).setCapability(Shape3D.ALLOW_GEOMETRY_READ);
        cylinderTip.getShape(0).setCapability(Geometry.ALLOW_INTERSECT);
        cylinderTip.getShape(0).setAppearanceOverrideEnable(false);

        Transform3D cylinderTipT3D = new Transform3D();

        cylinderTipT3D.set(new Vector3f(0.0f, 1.4f, 0.0f));
        cylinderTipTG = new TransformGroup(cylinderTipT3D);
        cylinderTipTG.setCapability(TransformGroup.ALLOW_CHILDREN_READ);
        cylinderTipTG.setCapability(TransformGroup.ALLOW_TRANSFORM_READ);
        cylinderTipTG.setCapability(TransformGroup.ALLOW_TRANSFORM_WRITE);
        cylinderTipTG.addChild(cylinderTip);
        regularProbeBG.addChild(cylinderTipTG);

        // set pickable
        try {
            pickCanvas.setCapabilities(coneTip.getShape(0), PickTool.INTERSECT_FULL);
            pickCanvas.setCapabilities(cylinderTip.getShape(0), PickTool.INTERSECT_FULL);
        } catch (RestrictedAccessException error) { }

        // build the indicator probe geometry shape.
        Material mat;
        Appearance app = new Appearance();

        mat = new Material(emissiveColor, emissiveColor, green, sepcualarColor, 50.0f);
        mat.setCapability(Material.ALLOW_COMPONENT_READ);
        mat.setCapability(Material.ALLOW_COMPONENT_WRITE);
        app.setMaterial(mat);

        // cone tip
        coneIndicator = new Cone(0.005f, 0.1f, Primitive.GENERATE_NORMALS, app);
        coneIndicator.getShape(0).setCapability(Shape3D.ALLOW_APPEARANCE_READ);
        coneIndicator.getShape(0).setCapability(Shape3D.ALLOW_APPEARANCE_WRITE);
        coneIndicator.getShape(0).setCapability(Shape3D.ALLOW_GEOMETRY_WRITE);
        coneIndicator.getShape(0).setCapability(Shape3D.ALLOW_GEOMETRY_READ);
        coneIndicator.getShape(0).setCapability(Geometry.ALLOW_INTERSECT);
        coneIndicator.getShape(0).setAppearanceOverrideEnable(false);

        Transform3D ConeIndicatorT3D = new Transform3D();
        ConeIndicatorT3D.setTranslation(new Vector3f(0.0f, 1.05f, 0.0f));
        ConeIndicatorT3D.setRotation(matrixAdjustCylinder);
        coneIndicatorTG = new TransformGroup(ConeIndicatorT3D);
        coneIndicatorTG.setCapability(TransformGroup.ALLOW_TRANSFORM_READ);
        coneIndicatorTG.setCapability(TransformGroup.ALLOW_TRANSFORM_WRITE);
        coneIndicatorTG.setCapability(TransformGroup.ALLOW_CHILDREN_READ);

        // Add the cone to the cone transform group
        coneIndicatorTG.addChild(coneIndicator);
        indicatorProbeBG.addChild(coneIndicatorTG);

        // cylinder tip
        cylinderIndicator = new Cylinder(0.005f, 0.7f, Primitive.GENERATE_NORMALS, app);
        cylinderIndicator.getShape(0).setCapability(Shape3D.ALLOW_APPEARANCE_READ);
        cylinderIndicator.getShape(0).setCapability(Shape3D.ALLOW_APPEARANCE_WRITE);
        cylinderIndicator.getShape(0).setCapability(Shape3D.ALLOW_GEOMETRY_WRITE);
        cylinderIndicator.getShape(0).setCapability(Shape3D.ALLOW_GEOMETRY_READ);
        cylinderIndicator.getShape(0).setCapability(Geometry.ALLOW_INTERSECT);
        cylinderIndicator.getShape(0).setAppearanceOverrideEnable(false);

        Transform3D cylinderIndicatorT3D = new Transform3D();

        cylinderIndicatorT3D.set(new Vector3f(0.0f, 1.4f, 0.0f));
        cylinderIndicatorTG = new TransformGroup(cylinderIndicatorT3D);
        cylinderIndicatorTG.setCapability(TransformGroup.ALLOW_CHILDREN_READ);
        cylinderIndicatorTG.setCapability(TransformGroup.ALLOW_TRANSFORM_READ);
        cylinderIndicatorTG.setCapability(TransformGroup.ALLOW_TRANSFORM_WRITE);
        cylinderIndicatorTG.addChild(cylinderIndicator);
        indicatorProbeBG.addChild(cylinderIndicatorTG);

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
        vConeTip.y = (1.05f - yOrigin - value);
        transConeTip.setTranslation(vConeTip);
        coneTipTG.setTransform(transConeTip);

        Transform3D transCylinderTip = new Transform3D();
        Vector3f vCylinderTip = new Vector3f();

        cylinderTipTG.getTransform(transCylinderTip);
        transCylinderTip.get(vCylinderTip);
        vCylinderTip.y = (1.4f - yOrigin - value);
        transCylinderTip.setTranslation(vCylinderTip);
        cylinderTipTG.setTransform(transCylinderTip);

        // indicator probe
        coneIndicatorTG.setTransform(transConeTip);
        cylinderIndicatorTG.setTransform(transCylinderTip);
    }


    /**
     * Switch the default probe between color cyan and color green.
     *
     * @param  flag  boolean <code>true</code> green color, <code>false</code> gray color.
     */
    protected void setProbeGreenColor(boolean flag) {

        if (flag) {
            switchGroup.setWhichChild(1);
        } else {
            switchGroup.setWhichChild(0);
        }
    }
}
