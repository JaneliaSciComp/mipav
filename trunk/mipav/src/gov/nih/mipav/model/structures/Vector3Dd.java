package gov.nih.mipav.model.structures;


/**
 * This is a simple 3D double vector structure.
 *
 * @version  0.1 Oct 27, 1997
 * @author   Matthew J. McAuliffe, Ph.D.
 */


public class Vector3Dd {

    //~ Instance fields ------------------------------------------------------------------------------------------------

    /** DOCUMENT ME! */
    public double x;

    /** DOCUMENT ME! */
    public double y;

    /** DOCUMENT ME! */
    public double z;

    //~ Constructors ---------------------------------------------------------------------------------------------------

    /**
     * Default constructor.
     */
    public Vector3Dd() {
        x = 0;
        y = 0;
        z = 0;
    }

    /**
     * Sets the initial values of the 3D vector.
     *
     * @param  x  first parameter of the vector
     * @param  y  second parameter of the vector
     * @param  z  third parameter of the vector
     */
    public Vector3Dd(double x, double y, double z) {
        this.x = x;
        this.y = y;
        this.z = z;
    }

    //~ Methods --------------------------------------------------------------------------------------------------------

    /**
     * returns length of input vector.
     *
     * @return  DOCUMENT ME!
     */
    public double length() {
        return (Math.sqrt(squaredLength()));
    }

    /**
     * scales the input vector to the new length and returns it.
     *
     * @param  newlen  DOCUMENT ME!
     */
    public void scale(double newlen) {
        double len = length();

        if (len != 0.0) {
            x *= newlen / len;
            y *= newlen / len;
            z *= newlen / len;
        }
    }

    /**
     * returns squared length of input vector.
     *
     * @return  DOCUMENT ME!
     */
    public double squaredLength() {
        return ((x * x) + (y * y) + (z * z));
    }

}
