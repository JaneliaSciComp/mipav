package gov.nih.mipav.view.renderer.J3D.surfaceview;


import gov.nih.mipav.view.renderer.J3D.*;

import com.sun.j3d.utils.geometry.*;
import com.sun.j3d.utils.picking.*;

import java.awt.*;

import javax.media.j3d.*;

import javax.vecmath.*;


/**
 * Holds parent BranchGroup and an array of 8 spheres that represent the lights on the surface.
 */
public class ViewJComponentLightBulbs {

    //~ Instance fields ------------------------------------------------------------------------------------------------

    /** Arrows representing light bulb directions. */
    private BranchGroup[] m_akLightArrows;

    /** Parent of each light bulb. */
    private BranchGroup[] m_akLightBG;

    /** Material properties for each light bulb. */
    private Material[] m_akLightMaterials;

    /** Shapes representing light bulbs. */
    private Sphere[] m_akLightSpheres;

    /** Parent transform group. */
    private TransformGroup[] m_akLightTransformGroup;

    /** Parent of all the light bulbs. */
    private BranchGroup m_kLightRoot;

    /** A black sphere is off. */
    private Color3f s_kBlack = new Color3f(Color.black);

    //~ Constructors ---------------------------------------------------------------------------------------------------

    /**
     * Create an array of light bulb objects that can be rendered or picked.
     *
     * @param  iNumLights  int Number of lights to create in the array.
     * @param  pickCanvas  PickCanvas Used for picking a light from the array.
     */
    public ViewJComponentLightBulbs(int iNumLights, PickCanvas pickCanvas) {

        m_akLightBG = new BranchGroup[iNumLights];
        m_akLightSpheres = new Sphere[iNumLights];
        m_akLightMaterials = new Material[iNumLights];
        m_akLightTransformGroup = new TransformGroup[iNumLights];
        m_akLightArrows = new BranchGroup[iNumLights];

        m_kLightRoot = new BranchGroup();
        m_kLightRoot.setCapability(BranchGroup.ALLOW_DETACH);
        m_kLightRoot.setCapability(BranchGroup.ALLOW_CHILDREN_READ);
        m_kLightRoot.setCapability(BranchGroup.ALLOW_CHILDREN_WRITE);

        // Note that cylinder and cone geometry is defined relative to
        // the y-axis and instead we want it relative to the z-axis.
        // So, we can apply a rotation by 90 about the x axis.
        Matrix3d kMatrixAdjustGeometry = new Matrix3d();
        kMatrixAdjustGeometry.rotX(Math.PI / 2);

        for (int i = 0; i < iNumLights; i++) {
            m_akLightBG[i] = new BranchGroup();
            m_akLightBG[i].setCapability(BranchGroup.ALLOW_DETACH);
            m_akLightBG[i].setCapability(Group.ALLOW_CHILDREN_EXTEND);
            m_akLightBG[i].setCapability(BranchGroup.ALLOW_CHILDREN_READ);
            m_akLightBG[i].setCapability(BranchGroup.ALLOW_CHILDREN_WRITE);

            // Create appearance for display of light.  Make the initial light color gray.
            Appearance kAppearance = new Appearance();
            Color3f black = new Color3f(Color.black);
            Color3f white = new Color3f(Color.white);
            Color3f gray = new Color3f(0.7f, 0.7f, 0.7f);

            m_akLightMaterials[i] = new Material(white, black, gray, gray, 80f);

            m_akLightMaterials[i].setCapability(Material.ALLOW_COMPONENT_READ);
            m_akLightMaterials[i].setCapability(Material.ALLOW_COMPONENT_WRITE);
            kAppearance.setMaterial(m_akLightMaterials[i]);

            // Create a transform group node to scale and position the object.
            Transform3D t = new Transform3D();

            m_akLightTransformGroup[i] = new TransformGroup(t);
            m_akLightTransformGroup[i].setCapability(TransformGroup.ALLOW_TRANSFORM_WRITE);
            m_akLightTransformGroup[i].setCapability(TransformGroup.ALLOW_TRANSFORM_READ);
            m_akLightTransformGroup[i].setCapability(TransformGroup.ALLOW_LOCAL_TO_VWORLD_READ);
            m_akLightTransformGroup[i].setCapability(BranchGroup.ALLOW_CHILDREN_READ);
            m_akLightTransformGroup[i].setCapability(BranchGroup.ALLOW_CHILDREN_WRITE);
            m_akLightTransformGroup[i].setCapability(BranchGroup.ALLOW_DETACH);
            m_akLightTransformGroup[i].setCapability(TransformGroup.ALLOW_CHILDREN_EXTEND);
            m_akLightSpheres[i] = new Sphere(.5f, kAppearance);
            m_akLightSpheres[i].setCapability(Sphere.ENABLE_APPEARANCE_MODIFY);
            m_akLightSpheres[i].getShape().setCapability(Shape3D.ALLOW_APPEARANCE_READ);
            m_akLightSpheres[i].getShape().setCapability(Shape3D.ALLOW_APPEARANCE_WRITE);
            m_akLightSpheres[i].getShape().setAppearanceOverrideEnable(true);
            m_akLightSpheres[i].getShape().setCapability(Geometry.ALLOW_INTERSECT);

            // Create a cylinder
            Cylinder kCylinder = new Cylinder(0.1f, 0.7f, Primitive.GENERATE_NORMALS, kAppearance);

            // Create a group that will be the root of the arrow
            m_akLightArrows[i] = new BranchGroup();
            m_akLightArrows[i].setCapability(BranchGroup.ALLOW_DETACH);

            TransformGroup kArrow = new TransformGroup();
            m_akLightArrows[i].addChild(kArrow);

            // Add the cylinder to the arrow group
            kArrow.addChild(kCylinder);

            // Create a cone
            // Create the transform and transform group that will position the cone on the arrow
            Cone kCone = new Cone(0.3f, 0.5f, Primitive.GENERATE_NORMALS, kAppearance);
            Transform3D kConeTrans = new Transform3D();

            kConeTrans.set(new Vector3f(0.0f, 0.5f, 0.0f));

            TransformGroup kConeTG = new TransformGroup(kConeTrans);

            // Add the cone to the cone transform group
            kConeTG.addChild(kCone);
            kArrow.addChild(kConeTG);

            Transform3D kArrowTransform = new Transform3D();

            kArrowTransform.setTranslation(new Vector3f(0.0f, 0.0f, 0.5f));
            kArrowTransform.setRotationScale(kMatrixAdjustGeometry);
            kArrow.setTransform(kArrowTransform);

            // Allow the user to "pick" any of the spheres
            try {
                pickCanvas.setCapabilities(m_akLightSpheres[i].getShape(), PickTool.INTERSECT_FULL);
            } catch (RestrictedAccessException error) { }

            m_akLightBG[i].addChild(m_akLightSpheres[i]);
            m_akLightBG[i].addChild(m_akLightArrows[i]);
            m_akLightTransformGroup[i].addChild(m_akLightBG[i]);
            m_kLightRoot.addChild(m_akLightTransformGroup[i]);
        }
    }

    //~ Methods --------------------------------------------------------------------------------------------------------

    /**
     * Determine if a given shape is the sphere for one of the lights.
     *
     * @param   kPickedShape  Shape3D Reference to a picked shape.
     *
     * @return  int Index of the corresponding light object in the array, or -1 if picked shape does not correspond to
     *          any light object.
     */
    public int getPickedLight(Shape3D kPickedShape) {

        for (int i = 0; i < m_akLightSpheres.length; i++) {

            if (m_akLightSpheres[i].getShape() == kPickedShape) {
                return i;
            }
        }

        return -1;
    }

    /**
     * Accessor that returns the root node for the light bulbs.
     *
     * @return  Root node that holds light bulbs.
     */
    public BranchGroup getRoot() {
        return m_kLightRoot;
    }

    /**
     * Update the properties of the specified light bulb.
     *
     * @param  index   int Index of the bulb in the array.
     * @param  kLight  GeneralLight Contains properties which describe the light.
     */
    public void setLightAttributes(int index, GeneralLight kLight) {

        setLightDirectional(index, kLight.isTypeDirectional() || kLight.isTypeSpot());
        setLightPosition(index, kLight.createJava3dLightPosition());
        setLightDirection(index, kLight.createJava3dLightDirection());
        setLightColor(index, kLight.isEnabled() ? kLight.getIntensifiedColor() : s_kBlack);
    }

    /**
     * Set the light color.
     *
     * @param  i       light index
     * @param  kColor  light color
     */
    private void setLightColor(int i, Color3f kColor) {
        m_akLightMaterials[i].setEmissiveColor(kColor);
    }

    /**
     * The the light direction.
     *
     * @param  i           light index
     * @param  kDirection  lighting direction
     */
    private void setLightDirection(int i, Vector3f kDirection) {

        Vector3f kW = new Vector3f(kDirection.x, kDirection.y, kDirection.z);

        // Compute orthonormal basis from direction vector.
        Vector3f kU = new Vector3f();

        if (Math.abs(kW.x) >= Math.abs(kW.y)) {

            // x or z is the largest magnitude, swap them
            float dInvLength = (float) Math.sqrt((kW.x * kW.x) + (kW.z * kW.z));

            kU.set(-kW.z / dInvLength, 0.0f, +kW.x / dInvLength);
        } else {

            // y or z is the largest magnitude, swap them
            float dInvLength = (float) Math.sqrt((kW.y * kW.y) + (kW.z * kW.z));

            kU.set(0.0f, +kW.z / dInvLength, -kW.y / dInvLength);
        }

        Vector3f kV = new Vector3f();

        kV.cross(kW, kU);

        Matrix3f m = new Matrix3f();

        m.setColumn(0, kU);
        m.setColumn(1, kV);
        m.setColumn(2, kW);
        m.setScale(0.1f);

        Transform3D t = new Transform3D();

        m_akLightTransformGroup[i].getTransform(t);
        t.setRotationScale(m);
        m_akLightTransformGroup[i].setTransform(t);
    }

    /**
     * Enable the light direction arrow.
     *
     * @param  i             light index
     * @param  bDirectional  show direction arrow or not.
     */
    private void setLightDirectional(int i, boolean bDirectional) {
        m_akLightArrows[i].detach();

        if (bDirectional) {
            m_akLightBG[i].addChild(m_akLightArrows[i]);
        }
    }

    /**
     * The the light move positions.
     *
     * @param  i          light index
     * @param  kPosition  position coordinate
     */
    private void setLightPosition(int i, Point3f kPosition) {
        Transform3D t = new Transform3D();
        m_akLightTransformGroup[i].getTransform(t);
        t.setTranslation(new Vector3d(kPosition.x, kPosition.y, kPosition.z));
        m_akLightTransformGroup[i].setTransform(t);
    }
}
