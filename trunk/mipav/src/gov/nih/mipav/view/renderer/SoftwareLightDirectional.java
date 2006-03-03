package gov.nih.mipav.view.renderer;

import javax.vecmath.*;

/**
 * The class for directional lights in a general lighting system to calculate
 * diffuse and specular colors at a point in space.
 *
 * See ShearWarpRendering.pdf for a detailed description of the lighting
 * model.
 */

public class SoftwareLightDirectional
    extends SoftwareLight {

    /**
     * Copy (deep) constructor.
     * @param kThat Light What to make "this" light like.
     */
    public SoftwareLightDirectional (SoftwareLightDirectional kThat) {
        super(kThat);
        direction = new Vector3f(kThat.direction);
    }

    /**
     * Construct a directional light.  The default colors are all black, the
     * intensity is zero, and the light is off.  The attenuation parameters
     * are set to 1 (no attenuation) since a directional light does not have a
     * position that identifies its source.  The default direction is the
     * zero vector.
     */
    public SoftwareLightDirectional() {
        direction = new Vector3f(0.0f, 0.0f, 0.0f);
    }

    /**
     * Create a deep copy of this Light instance.
     * @return Light Deep copy of this Light instance.
     */
    public SoftwareLight copy() {
        return new SoftwareLightDirectional(this);
    }

    /**
     * Compute the contribution of color from this light on the specified
     * material with specified vertex properties.
     * @param kMaterial SoftwareMaterial Default properties for the material
     * including emissive, ambient, diffuse, and specular colors along
     * with the specular shininess which can be used to disable specular
     * highlighting.
     * @param kVertexProperty SoftwareVertexProperty Properties of the
     * vertex which include its position and normal vector along with
     * optional material property overrides for the diffuse and specular
     * colors.
     * @param kEye Point3f The location of the observer in the coordinate
     * system defined for the light.
     * @return Color3f Reference to the computed color contribution by
     * this light with this vertex and its material properties.  The
     * instance returned is owned by this light.
     */
    public final Color3f colorOf(SoftwareMaterial kMaterial,
                           SoftwareVertexProperty kVertexProperty,
                           Point3f kEye) {
        initView(kEye, kVertexProperty);

        // no ambient light
        m_kColor.set(0.0f, 0.0f, 0.0f);

        float fDdN = addDiffuse(kMaterial, kVertexProperty, direction);
        if (fDdN < 0.0f && kMaterial.hasSpecular()) {
            addSpecular(kMaterial, kVertexProperty, direction, fDdN);
        }

        m_kColor.scale(intensity);
        return m_kColor;
    }

    /**
     * Set the light direction
     * @param v   light direction
     */
    public void setDirection(Vector3f v) {
        direction.set(v);
        normalize(direction);
    }

    /**
     * Set the light direction.
     * @param fX float X coordinate of the light direction vector.
     * @param fY float Y coordinate of the light direction vector.
     * @param fZ float Z coordinate of the light direction vector.
     */
    public void setDirection(float fX, float fY, float fZ) {
        direction.set(fX, fY, fZ);
        normalize(direction);
    }

    /**
     * Get the light direction
     * @return direction   light direction
     */
    public Vector3f getDirection() {
        return direction;
    }

    // the light's direction in the world
    protected Vector3f direction;
}
