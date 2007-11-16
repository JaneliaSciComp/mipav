package gov.nih.mipav.model.structures;


import java.io.*;


/**
 * Simple float 3D point.
 *
 * @version  0.1 Oct 27, 1997
 * @author   Matthew J. McAuliffe
 */


public class Point3Df extends PointND {

    //~ Static fields/initializers -------------------------------------------------------------------------------------

    /** Use serialVersionUID for interoperability. */
    private static final long serialVersionUID = -5639938104845144804L;

    //~ Instance fields ------------------------------------------------------------------------------------------------

    /** X coordinate of point. */
    public float x;

    /** Y coordinate of point. */
    public float y;

    /** Z coordinate of point. */
    public float z;

    //~ Constructors ---------------------------------------------------------------------------------------------------

    /**
     * Initializes structure to (0,0,0).
     */
    public Point3Df() {
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
    public Point3Df(float x, float y, float z) {
        this.x = x;
        this.y = y;
        this.z = z;
    }

    /**
     * Initializes structure to the supplied values.
     *
     * @param  x  X coordinate value.
     * @param  y  Y coordinate value.
     * @param  z  Z coordinate value.
     */
    public Point3Df(int x, int y, int z) {
        this.x = (float) x;
        this.y = (float) y;
        this.z = (float) z;
    }

    //~ Methods --------------------------------------------------------------------------------------------------------

    /**
     * Compares this point to another for equality.
     *
     * @param   o  object to compare this Point3Df to
     *
     * @return  true if coordinates are the same in both points. false otherwise.
     */
    public boolean equals(Object o) {

        if (o == null) {
            return false;
        }

        Point3Df p = (Point3Df) o;

        if ((p.x == this.x) && (p.y == this.y) && (p.z == this.z)) {
            return true;
        }

        return false;
    }

    /**
     * Override hashcode because we're overriding equals.
     *
     * @return  integer that is the same for equal objects
     */
    public int hashCode() {
        return (int) (x + y + z);
    }

    /**
     * Prints out this point object in a user readable form.
     *
     * @return  This object as a string.
     */
    public String toString() {
        return new String("x: " + x + " y: " + y + " z: " + z);
    }
}
