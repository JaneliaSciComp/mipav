package gov.nih.mipav.model.structures;


/**
 * Simple integer 3D point.
 *
 * @version  0.1 Oct 27, 1997
 * @author   Matthew J. McAuliffe, Ph.D.
 */


public class Point3D {

    //~ Static fields/initializers -------------------------------------------------------------------------------------

    /** Use serialVersionUID for interoperability. */
    private static final long serialVersionUID = -3302565934514310154L;

    //~ Instance fields ------------------------------------------------------------------------------------------------

    /** X coordinate of point. */
    public int x;

    /** Y coordinate of point. */
    public int y;

    /** Z coordinate of point. */
    public int z;

    //~ Constructors ---------------------------------------------------------------------------------------------------

    /**
     * Initializes structure to (0,0,0).
     */
    public Point3D() {
        x = 0;
        y = 0;
        z = 0;
    }

    /**
     * Initializes structure to the supplied values.
     *
     * @param  x  X coordinate value.
     * @param  y  Y coordinate value.
     * @param  z  Z coordinate value.
     */
    public Point3D(int x, int y, int z) {
        this.x = x;
        this.y = y;
        this.z = z;
    }

    //~ Methods --------------------------------------------------------------------------------------------------------

    /**
     * Prints out this point object in a user readable form.
     *
     * @return  This object as a string.
     */
    public String toString() {
        return new String("x: " + x + " y: " + y + " z: " + z);
    }


}
