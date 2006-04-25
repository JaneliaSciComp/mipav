package gov.nih.mipav.model.structures;


/**
 * This is a simple 2D vector structure.
 *
 * @version  0.1 Oct 27, 1997
 * @author   Matthew J. McAuliffe, Ph.D.
 */


public class Vector2Dd {

    //~ Instance fields ------------------------------------------------------------------------------------------------

    /** DOCUMENT ME! */
    public double x;

    /** DOCUMENT ME! */
    public double y;

    //~ Constructors ---------------------------------------------------------------------------------------------------

    /**
     * Creates a new Vector2Dd object.
     */
    public Vector2Dd() {
        x = 0;
        y = 0;
    }

    /**
     * Creates a new Vector2Dd object.
     *
     * @param  x  first parameter of the vector
     * @param  y  second parameter of the vector
     */
    public Vector2Dd(double x, double y) {
        this.x = x;
        this.y = y;
    }

    //~ Methods --------------------------------------------------------------------------------------------------------

    /**
     * Forms vector product of input vector and its self.
     *
     * @param   vect  input vector
     *
     * @return  vector cross product ( Is used to determine (as seen from this vector) if the input vector lies to the
     *          left (result = positive), colinear (result = 0), left(result = negative);
     */
    public final double crossProductVectors(Vector2Dd vect) {
        return ((x * vect.y) - (y * vect.x));
    }

    /**
     * Calculates the normal length of the vector.
     *
     * @return  normalized vector
     */
    public final Vector2Dd getNormVector() {
        double norm = (double) Math.sqrt((double) ((x * x) + (y * y)));

        return (new Vector2Dd(x / norm, y / norm));
    }

    /**
     * Calculates the length of the vector.
     *
     * @return  length of vector
     */
    public final double length() {
        return (double) Math.sqrt((double) ((x * x) + (y * y)));
    }

    /**
     * Normalizes the vector.
     */
    public final void normalizeVector() {
        double norm = (double) Math.sqrt((double) ((x * x) + (y * y)));
        x = x / norm;
        y = y / norm;
    }
}
