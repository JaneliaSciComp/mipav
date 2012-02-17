package gov.nih.mipav.view.renderer.J3D;


import javax.vecmath.*;


/**
 * The class for ambient lights in a general lighting system to calculate ambient colors at a point in space. The other
 * derived classes for lights make no ambient contribution, only diffuse and specular. As such, at most one ambient
 * light is really needed in an environment.
 *
 * <p>See ShearWarpRendering.pdf for a detailed description of the lighting model.</p>
 */

public class SoftwareLightAmbient extends SoftwareLight {

    //~ Constructors ---------------------------------------------------------------------------------------------------

    /**
     * Construct an ambient light. The default colors are all black, the intensity is zero, and the light is off. The
     * attenuation parameter are set to 1 (no attenuation) since an ambient light does not have a position that
     * identifies its source.
     */
    public SoftwareLightAmbient() {
        super();
    }

    /**
     * Copy (deep) constructor.
     *
     * @param  kThat  Light What to make "this" light like.
     */
    public SoftwareLightAmbient(SoftwareLightAmbient kThat) {
        super(kThat);
    }

    //~ Methods --------------------------------------------------------------------------------------------------------

    /**
     * Compute the contribution of color from this light on the specified material with specified vertex properties.
     *
     * @param   kMaterial        SoftwareMaterial Default properties for the material including emissive, ambient,
     *                           diffuse, and specular colors along with the specular shininess which can be used to
     *                           disable specular highlighting.
     * @param   kVertexProperty  SoftwareVertexProperty Properties of the vertex which include its position and normal
     *                           vector along with optional material property overrides for the diffuse and specular
     *                           colors.
     * @param   kEye             Point3f The location of the observer in the coordinate system defined for the light.
     *
     * @return  Color3f Reference to the computed color contribution by this light with this vertex and its material
     *          properties. The instance returned is owned by this light.
     */
    public final Color3f colorOf(SoftwareMaterial kMaterial, SoftwareVertexProperty kVertexProperty, Point3f kEye) {

        m_kColor.set(0.0f, 0.0f, 0.0f);
        addAmbient(kMaterial.ambient);
        m_kColor.scale(intensity);

        return m_kColor;
    }

    /**
     * Create a deep copy of this Light instance.
     *
     * @return  Light Deep copy of this Light instance.
     */
    public SoftwareLight copy() {
        return new SoftwareLightAmbient(this);
    }
}
