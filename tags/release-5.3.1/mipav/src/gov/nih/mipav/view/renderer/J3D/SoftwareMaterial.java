package gov.nih.mipav.view.renderer.J3D;


import javax.vecmath.*;


/**
 * This class maintains the following material properties:
 *
 * <p>- emissive color (unaffected by lights) - ambient color (affected only by ambient lights) - diffuse color (for
 * diffuse lighting; can be replaced by vertex color) - specular color (for specular lighting; can be replaced by vertex
 * color) - specular shininess (set to zero to disable specular effects)</p>
 */
public class SoftwareMaterial {

    //~ Instance fields ------------------------------------------------------------------------------------------------

    /** DOCUMENT ME! */
    public Color3f ambient = new Color3f(0.2f, 0.2f, 0.2f);

    /** DOCUMENT ME! */
    public Color3f diffuse = new Color3f(1.0f, 1.0f, 1.0f);

    /** Defaults colors set according to defaults for Java3D Material. */
    public Color3f emissive = new Color3f(0.0f, 0.0f, 0.0f);

    /** Disable specular lighting effects. */
    public float shininess = 5.0f;

    /** DOCUMENT ME! */
    public Color3f specular = new Color3f(1.0f, 1.0f, 1.0f);

    //~ Constructors ---------------------------------------------------------------------------------------------------

    /**
     * Default constructor. Sets the colors to defaults and disables specular properties.
     */
    public SoftwareMaterial() { }

    //~ Methods --------------------------------------------------------------------------------------------------------

    /**
     * Query whether the material has specular properties.
     *
     * @return  boolean True if the material has specular properties.
     */
    public boolean hasSpecular() {
        return shininess > 0.0f;
    }
}
