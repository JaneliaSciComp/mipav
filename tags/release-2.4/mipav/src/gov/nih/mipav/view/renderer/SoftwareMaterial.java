package gov.nih.mipav.view.renderer;

import javax.vecmath.*;

/**
 * This class maintains the following material properties:
 *
 * - emissive color (unaffected by lights)
 * - ambient color (affected only by ambient lights)
 * - diffuse color (for diffuse lighting; can be replaced by vertex color)
 * - specular color (for specular lighting; can be replaced by vertex color)
 * - specular shininess (set to zero to disable specular effects)
 */
public class SoftwareMaterial {

    /**
     * Defaults colors set according to defaults for Java3D Material.
     */
    public Color3f emissive = new Color3f(0.0f, 0.0f, 0.0f);
    public Color3f ambient = new Color3f(0.2f, 0.2f, 0.2f);
    public Color3f diffuse = new Color3f(1.0f, 1.0f, 1.0f);
    public Color3f specular = new Color3f(1.0f, 1.0f, 1.0f);

    /**
     * Disable specular lighting effects.
     */
    public float shininess = 5.0f;

    /**
     * Default constructor.  Sets the colors to defaults and disables
     * specular properties.
     */
    public SoftwareMaterial() {
    }

    /**
     * Query whether the material has specular properties.
     * @return boolean True if the material has specular properties.
     */
    public boolean hasSpecular() {
        return shininess > 0.0f;
    }
}
