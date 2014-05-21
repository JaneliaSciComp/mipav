package gov.nih.mipav.view.renderer.J3D.surfaceview.rfaview;


import com.sun.j3d.utils.geometry.*;
import com.sun.j3d.utils.picking.*;

import java.awt.*;

import javax.media.j3d.*;

import javax.vecmath.*;


/**
 * <p>Title: DefaultPorbe</p>
 *
 * <p>Description: The default probe is the cyan probe. This class builds the default probe. It also defines the
 * geometry shape of the default probe.</p>
 *
 * @author  Ruida Cheng
 */
public class DefaultProbe {

    //~ Instance fields ------------------------------------------------------------------------------------------------

    /** Ambient, emissive, sepcualar, diffuse color is used for the attenuation lighting. */
    protected Color3f ambientColor = new Color3f(1.0f, 0.0f, 0.0f);

    /** Define the cyan color. */
    protected Color3f cyan = new Color3f(Color.cyan);

    /** DOCUMENT ME! */
    protected Color3f diffuseColor = new Color3f(1.0f, 0.0f, 0.0f);

    /** DOCUMENT ME! */
    protected Color3f emissiveColor = new Color3f(0.0f, 0.0f, 0.0f);

    /** Define the green color. */
    protected Color3f green = new Color3f(Color.green);

    /** Agent to set the probe pickable. */
    protected PickCanvas pickCanvas;

    /** DOCUMENT ME! */
    protected Color3f sepcualarColor = new Color3f(1.0f, 1.0f, 1.0f);

    /** Cone shape part of the probe. */
    private Cone coneDefault;

    /** Indicator probe cone shape. */
    private Cone coneIndicator;

    /** Transform group of the indicator cone shape. */
    private TransformGroup coneIndicatorTG;

    /** Transform group of the cone shape. */
    private TransformGroup coneTG;

    /** Cylinder shape part of the probe. */
    private Cylinder cylinderDefault;

    /**
     * Indicator is used by the default probe to switch color between cyan and green. Indicator is the green color
     * probe. I prefer switching the branch group rather than changing the java3d geometry appearance because the
     * appearance changes waste time and memory. /** Indicator probe cylinder shape.
     */
    private Cylinder cylinderIndicator;

    /** Transform group of the indicator cylinder shape. */
    private TransformGroup cylinderIndicatorTG;

    /** Transform group of the cylinder shape. */
    private TransformGroup cylinderTG;

    /** The root of the default probe. */
    private TransformGroup defaultProbe;

    /** The root branch group of the cyan default robe. */
    private BranchGroup defaultProbeBG;

    /** The root branch group of the green indicator probe. */
    private BranchGroup indicatorProbeBG;

    /** Switch group used to switch between cyan and green probes. */
    private Switch switchGroup = new Switch();

    /** Y original coordinate value. */
    private float yOrigin;

    //~ Constructors ---------------------------------------------------------------------------------------------------

    /**
     * Constructor, initialize the probe shapes.
     */
    public DefaultProbe() {
        init();
    }

    //~ Methods --------------------------------------------------------------------------------------------------------

    /**
     * Enable the probe to rotate around the entry point.
     *
     * @param  flag  boolean <code>true</code> means around the entry point, <code>false</code> around the origin.
     */
    public void enableEntryPointRotation(boolean flag) {
        Transform3D cylinderTrans = new Transform3D();
        Transform3D coneTrans = new Transform3D();
        Matrix3d matrixAdjustCylinder = new Matrix3d();
        Transform3D cylinderIndicatorTrans = new Transform3D();
        Transform3D coneIndicatorTrans = new Transform3D();

        if (flag) {
            yOrigin = 1.0f;
        } else {
            yOrigin = 0.0f;
        }

        cylinderTrans.setTranslation(new Vector3f(0.0f, 1.75f - yOrigin, 0.0f));
        cylinderTG.setTransform(cylinderTrans);

        matrixAdjustCylinder.rotZ(Math.PI);
        coneTrans.setTranslation(new Vector3f(0.0f, 1.25f - yOrigin, 0.0f));
        coneTrans.setRotation(matrixAdjustCylinder);
        coneTG.setTransform(coneTrans);

        cylinderIndicatorTrans.setTranslation(new Vector3f(0.0f, 1.75f - yOrigin, 0.0f));
        cylinderIndicatorTG.setTransform(cylinderIndicatorTrans);

        coneIndicatorTrans.setTranslation(new Vector3f(0.0f, 1.25f - yOrigin, 0.0f));
        coneIndicatorTrans.setRotation(matrixAdjustCylinder);
        coneIndicatorTG.setTransform(coneIndicatorTrans);
    }

    /**
     * Check whether the probe being picked or not.
     *
     * @param   pickedShape  picked geometry shape.
     *
     * @return  boolean <code>true</code> probe picked, <code>false</code> probe not picked.
     */
    protected boolean findProbe(Shape3D pickedShape) {

        if (cylinderDefault.getShape(0) == pickedShape) {
            return true;
        } else if (coneDefault.getShape(0) == pickedShape) {
            return true;
        } else {
            return false;
        }
    }

    /**
     * Get the root of the default probe.
     *
     * @return  TransformGroup The root transform group of the probe.
     */
    protected TransformGroup getProbe() {
        return defaultProbe;
    }

    /**
     * Initialize the probe geometry shapes.
     */
    protected void init() {

        // Create appearance for display of light.
        Appearance appearance = new Appearance();

        /** Set the attenuation lighting effect. */
        Material probeMaterials = new Material(emissiveColor, emissiveColor, cyan, sepcualarColor, 50.0f);
        probeMaterials.setCapability(Material.ALLOW_COMPONENT_READ);
        probeMaterials.setCapability(Material.ALLOW_COMPONENT_WRITE);
        appearance.setMaterial(probeMaterials);

        // Create a group that will be the root of the arrow
        defaultProbe = new TransformGroup();
        defaultProbe.setCapability(TransformGroup.ALLOW_CHILDREN_READ);
        defaultProbe.setCapability(TransformGroup.ALLOW_TRANSFORM_READ);

        defaultProbeBG = new BranchGroup();
        defaultProbeBG.setCapability(Group.ALLOW_CHILDREN_EXTEND);
        defaultProbeBG.setCapability(Group.ALLOW_CHILDREN_READ);
        defaultProbeBG.setCapability(Group.ALLOW_CHILDREN_WRITE);
        defaultProbeBG.setCapability(Node.ALLOW_PICKABLE_READ);
        defaultProbeBG.setCapability(Node.ALLOW_PICKABLE_WRITE);

        indicatorProbeBG = new BranchGroup();
        indicatorProbeBG.setCapability(Group.ALLOW_CHILDREN_EXTEND);
        indicatorProbeBG.setCapability(Group.ALLOW_CHILDREN_READ);
        indicatorProbeBG.setCapability(Group.ALLOW_CHILDREN_WRITE);
        indicatorProbeBG.setCapability(Node.ALLOW_PICKABLE_READ);
        indicatorProbeBG.setCapability(Node.ALLOW_PICKABLE_WRITE);

        defaultProbe.addChild(switchGroup);
        switchGroup.addChild(defaultProbeBG);
        switchGroup.addChild(indicatorProbeBG);
        switchGroup.setWhichChild(Switch.CHILD_NONE);
        switchGroup.setCapability(Switch.ALLOW_SWITCH_WRITE);
        switchGroup.setCapability(Switch.ALLOW_CHILDREN_WRITE);
        switchGroup.setCapability(Switch.ALLOW_CHILDREN_READ);
        switchGroup.setWhichChild(0);

        // create cylinder
        cylinderDefault = new Cylinder(0.01f, 0.5f, Primitive.GENERATE_NORMALS, appearance);
        cylinderDefault.getShape(0).setCapability(Shape3D.ALLOW_APPEARANCE_READ);
        cylinderDefault.getShape(0).setCapability(Shape3D.ALLOW_APPEARANCE_WRITE);
        cylinderDefault.getShape(0).setCapability(Shape3D.ALLOW_GEOMETRY_WRITE);
        cylinderDefault.getShape(0).setCapability(Shape3D.ALLOW_GEOMETRY_READ);
        cylinderDefault.getShape(0).setCapability(Geometry.ALLOW_INTERSECT);
        cylinderDefault.getShape(0).setAppearanceOverrideEnable(false);

        Transform3D cylinderTrans = new Transform3D();

        cylinderTrans.setTranslation(new Vector3f(0.0f, 1.75f, 0.0f));
        cylinderTG = new TransformGroup(cylinderTrans);
        cylinderTG.setCapability(TransformGroup.ALLOW_CHILDREN_READ);
        cylinderTG.setCapability(TransformGroup.ALLOW_TRANSFORM_READ);
        cylinderTG.setCapability(TransformGroup.ALLOW_TRANSFORM_WRITE);
        cylinderTG.addChild(cylinderDefault);
        defaultProbeBG.addChild(cylinderTG);

        // Create a cone
        coneDefault = new Cone(0.01f, 0.5f, Primitive.GENERATE_NORMALS, appearance);
        coneDefault.getShape(0).setCapability(Shape3D.ALLOW_APPEARANCE_READ);
        coneDefault.getShape(0).setCapability(Shape3D.ALLOW_APPEARANCE_WRITE);
        coneDefault.getShape(0).setCapability(Shape3D.ALLOW_GEOMETRY_WRITE);
        coneDefault.getShape(0).setCapability(Shape3D.ALLOW_GEOMETRY_READ);
        coneDefault.getShape(0).setCapability(Geometry.ALLOW_INTERSECT);
        coneDefault.getShape(0).setAppearanceOverrideEnable(false);

        // Create the transform and transform group that will position the cone on the arrow
        Transform3D coneTrans = new Transform3D();
        Matrix3d matrixAdjustCylinder = new Matrix3d();

        // coneTrans.setRotationScale( matrixAdjustCylinder );
        matrixAdjustCylinder.rotZ(Math.PI);
        coneTrans.setTranslation(new Vector3f(0.0f, 1.25f, 0.0f));
        coneTrans.setRotation(matrixAdjustCylinder);
        coneTG = new TransformGroup(coneTrans);
        coneTG.setCapability(TransformGroup.ALLOW_TRANSFORM_READ);
        coneTG.setCapability(TransformGroup.ALLOW_TRANSFORM_WRITE);
        coneTG.setCapability(TransformGroup.ALLOW_CHILDREN_READ);

        // Add the cone to the cone transform group
        coneTG.addChild(coneDefault);
        defaultProbeBG.addChild(coneTG);

        // set pickable
        try {
            PickCanvas.setCapabilities(cylinderDefault.getShape(0), PickTool.INTERSECT_FULL);
            PickCanvas.setCapabilities(coneDefault.getShape(0), PickTool.INTERSECT_FULL);
        } catch (RestrictedAccessException error) { }

        // following builds the green indicator probe geometry. */
        Material mat;
        Appearance app = new Appearance();
        mat = new Material(emissiveColor, emissiveColor, green, sepcualarColor, 50.0f);
        mat.setCapability(Material.ALLOW_COMPONENT_READ);
        mat.setCapability(Material.ALLOW_COMPONENT_WRITE);
        app.setMaterial(mat);

        // create cylinder
        cylinderIndicator = new Cylinder(0.01f, 0.5f, Primitive.GENERATE_NORMALS, app);
        cylinderIndicator.getShape(0).setCapability(Shape3D.ALLOW_APPEARANCE_READ);
        cylinderIndicator.getShape(0).setCapability(Shape3D.ALLOW_APPEARANCE_WRITE);
        cylinderIndicator.getShape(0).setCapability(Shape3D.ALLOW_GEOMETRY_WRITE);
        cylinderIndicator.getShape(0).setCapability(Shape3D.ALLOW_GEOMETRY_READ);
        cylinderIndicator.getShape(0).setCapability(Geometry.ALLOW_INTERSECT);
        cylinderIndicator.getShape(0).setAppearanceOverrideEnable(false);

        Transform3D cylinderIndicatorTrans = new Transform3D();

        cylinderIndicatorTrans.setTranslation(new Vector3f(0.0f, 1.75f, 0.0f));
        cylinderIndicatorTG = new TransformGroup(cylinderIndicatorTrans);
        cylinderIndicatorTG.setCapability(TransformGroup.ALLOW_CHILDREN_READ);
        cylinderIndicatorTG.setCapability(TransformGroup.ALLOW_TRANSFORM_READ);
        cylinderIndicatorTG.setCapability(TransformGroup.ALLOW_TRANSFORM_WRITE);
        cylinderIndicatorTG.addChild(cylinderIndicator);
        indicatorProbeBG.addChild(cylinderIndicatorTG);

        // Create a cone
        coneIndicator = new Cone(0.01f, 0.5f, Primitive.GENERATE_NORMALS, app);
        coneIndicator.getShape(0).setCapability(Shape3D.ALLOW_APPEARANCE_READ);
        coneIndicator.getShape(0).setCapability(Shape3D.ALLOW_APPEARANCE_WRITE);
        coneIndicator.getShape(0).setCapability(Shape3D.ALLOW_GEOMETRY_WRITE);
        coneIndicator.getShape(0).setCapability(Shape3D.ALLOW_GEOMETRY_READ);
        coneIndicator.getShape(0).setCapability(Geometry.ALLOW_INTERSECT);
        coneIndicator.getShape(0).setAppearanceOverrideEnable(false);

        // Create the transform and transform group that will position the cone on the arrow
        Transform3D coneIndicatorTrans = new Transform3D();
        coneIndicatorTrans.setTranslation(new Vector3f(0.0f, 1.25f, 0.0f));
        coneIndicatorTrans.setRotation(matrixAdjustCylinder);
        coneIndicatorTG = new TransformGroup(coneIndicatorTrans);
        coneIndicatorTG.setCapability(TransformGroup.ALLOW_TRANSFORM_READ);
        coneIndicatorTG.setCapability(TransformGroup.ALLOW_TRANSFORM_WRITE);
        coneIndicatorTG.setCapability(TransformGroup.ALLOW_CHILDREN_READ);

        // Add the cone to the cone transform group
        coneIndicatorTG.addChild(coneIndicator);
        indicatorProbeBG.addChild(coneIndicatorTG);

    }

    /**
     * Set the probe translation in the local coordinate system.
     *
     * @param  value  y coordinate value of the local coordinate system.
     */
    protected void setProbeCoordinate(float value) {

        // default probe
        Transform3D transCylinder = new Transform3D();
        Vector3f vCylinder = new Vector3f();

        cylinderTG.getTransform(transCylinder);
        transCylinder.get(vCylinder);
        vCylinder.y = (1.75f - yOrigin - value);
        transCylinder.setTranslation(vCylinder);
        cylinderTG.setTransform(transCylinder);

        Transform3D transCone = new Transform3D();
        Vector3f vCone = new Vector3f();

        coneTG.getTransform(transCone);
        transCone.get(vCone);
        vCone.y = (1.25f - yOrigin - value);
        transCone.setTranslation(vCone);
        coneTG.setTransform(transCone);

        // indicator probe
        cylinderIndicatorTG.setTransform(transCylinder);
        coneIndicatorTG.setTransform(transCone);
    }

    /**
     * Switch the default probe between color cyan and color green.
     *
     * @param  flag  boolean <code>true</code> green color, <code>false</code> cyan color.
     */
    protected void setProbeGreenColor(boolean flag) {

        if (flag) {
            switchGroup.setWhichChild(1);
        } else {
            switchGroup.setWhichChild(0);
        }
    }

}
