package gov.nih.mipav.model.structures;


/**
 * This is a simple 4D vector structure.
 *
 * @version  0.1 Oct 27, 1997
 * @author   Matthew J. McAuliffe, Ph.D.
 */


public class Vector4Df {

    //~ Instance fields ------------------------------------------------------------------------------------------------

    /** DOCUMENT ME! */
    protected float w;

    /** DOCUMENT ME! */
    protected float x;

    /** DOCUMENT ME! */
    protected float y;

    /** DOCUMENT ME! */
    protected float z;

    //~ Constructors ---------------------------------------------------------------------------------------------------

    /**
     * Default constructor. The vector is set to zero.
     */
    public Vector4Df() {
        w = 0;
        x = 0;
        y = 0;
        z = 0;
    }

    /**
     * Sets the initial values of the vector.
     *
     * @param  w  first parameter of the vector
     * @param  x  second parameter of the vector
     * @param  y  third parameter of the vector
     * @param  z  forth parameter of the vector
     */
    public Vector4Df(float w, float x, float y, float z) {
        this.w = w;
        this.x = x;
        this.y = y;
        this.z = z;
    }
}
