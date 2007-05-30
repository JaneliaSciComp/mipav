package gov.nih.mipav.model.structures;


/**
 * Stand-in for the java3d class javax.vecmath.Color3f.
 *
 * @author  mccreedy
 */
public class Color3Df {

    //~ Instance fields ------------------------------------------------------------------------------------------------

    /** First color component. */
    public float x;

    /** Second color component. */
    public float y;

    /** Third color component. */
    public float z;

    //~ Constructors ---------------------------------------------------------------------------------------------------

    /**
     * Creates a new Color3Df object with 0 for all its component values.
     */
    public Color3Df() {
        this(0.0f, 0.0f, 0.0f);
    }

    /**
     * Creates a new Color3Df object.
     *
     * @param  newX  first color component
     * @param  newY  second color component
     * @param  newZ  third color component
     */
    public Color3Df(float newX, float newY, float newZ) {
        x = newX;
        y = newY;
        z = newZ;
    }
}
