package gov.nih.mipav.model.structures;


/**
 * Stand-in for the java3d class javax.vecmath.Color4f.
 *
 * @author  mccreedy
 */
public class Color4Df {

    //~ Instance fields ------------------------------------------------------------------------------------------------

    /** Fouth color component. */
    public float w;

    /** First color component. */
    public float x;

    /** Second color component. */
    public float y;

    /** Third color component. */
    public float z;

    //~ Constructors ---------------------------------------------------------------------------------------------------

    /**
     * Creates a new Color4Df object with 0 for all its component values.
     */
    public Color4Df() {
        this(0.0f, 0.0f, 0.0f, 0.0f);
    }

    /**
     * Creates a new Color4Df object.
     *
     * @param  newX  first color component
     * @param  newY  second color component
     * @param  newZ  third color component
     * @param  newW  fourth color component
     */
    public Color4Df(float newX, float newY, float newZ, float newW) {
        x = newX;
        y = newY;
        z = newZ;
        w = newW;
    }
}
