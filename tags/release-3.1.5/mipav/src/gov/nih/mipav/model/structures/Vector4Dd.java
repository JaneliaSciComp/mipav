package gov.nih.mipav.model.structures;


/**
 * This is a simple 4D vector structure.
 *
 * @version  0.1 Oct 27, 1997
 * @author   Matthew J. McAuliffe, Ph.D.
 */


public class Vector4Dd {

    //~ Instance fields ------------------------------------------------------------------------------------------------

    /** DOCUMENT ME! */
    protected double w;

    /** DOCUMENT ME! */
    protected double x;

    /** DOCUMENT ME! */
    protected double y;

    /** DOCUMENT ME! */
    protected double z;

    //~ Constructors ---------------------------------------------------------------------------------------------------

    /**
     * Default constructor. The vector is set to zero.
     */
    public Vector4Dd() {
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
    public Vector4Dd(double w, double x, double y, double z) {
        this.w = w;
        this.x = x;
        this.y = y;
        this.z = z;
    }
}
