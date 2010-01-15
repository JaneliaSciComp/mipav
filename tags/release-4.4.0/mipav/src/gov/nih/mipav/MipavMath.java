package gov.nih.mipav;


import WildMagic.LibFoundation.Mathematics.BitHacks;
import WildMagic.LibFoundation.Mathematics.Vector3f;


/**
 * Math functions not found in Java's Math class or they are slow.
 *
 * @author   not attributable
 * @version  1.0
 */
public class MipavMath {

    //~ Methods --------------------------------------------------------------------------------------------------------

    /**
     * Calculates the 3D euclidian distance between two points.
     *
     * @param   pt1  first point
     * @param   pt2  second point
     *
     * @return  returns the distance
     */
    public static final double distance(final Vector3f pt1, final Vector3f pt2) {
        return Math.sqrt(((pt2.X - pt1.X) * (pt2.X - pt1.X)) + ((pt2.Y - pt1.Y) * (pt2.Y - pt1.Y)) +
                         ((pt2.Z - pt1.Z) * (pt2.Z - pt1.Z)));
    }

    /**
     * Calculates the 2D euclidian distance between two points.
     *
     * @param   x1  first x coordinate
     * @param   x2  second x coordinate
     * @param   y1  first y coordinate
     * @param   y2  second y coordinate
     *
     * @return  returns the distance
     */
    public static final double distance(final int x1, final int x2, final int y1, final int y2) {
        return Math.sqrt(((x2 - x1) * (x2 - x1)) + ((y2 - y1) * (y2 - y1)));
    }

    /**
     * Finds the distance between two points.
     *
     * @param   x1  x coordinate of the first point
     * @param   x2  x coordinate of the second point
     * @param   y1  y coordinate of the first point
     * @param   y2  y coordinate of the second point
     *
     * @return  the distance as a double
     */
    public static final double distance(final float x1, final float x2, final float y1, final float y2) {
        return Math.sqrt(((x2 - x1) * (x2 - x1)) + ((y2 - y1) * (y2 - y1)));
    }

    /**
     * Finds the distance between two 2D points.
     *
     * @param   x1  x coordinate of the first point
     * @param   x2  x coordinate of the second point
     * @param   y1  y coordinate of the first point
     * @param   y2  y coordinate of the second point
     *
     * @return  the distance as a double
     */
    public static final double distance(final double x1, final double x2, final double y1, final double y2) {
        return Math.sqrt(((x2 - x1) * (x2 - x1)) + ((y2 - y1) * (y2 - y1)));
    }

    /**
     * Calculates the actual distance between two 3D points using voxel resolutions.
     *
     * @param   x0   x coordinate of the first point
     * @param   y0   y coordinate of the first point
     * @param   z0   z coordinate of the first point
     * @param   x1   x coordinate of the second point
     * @param   y1   y coordinate of the second point
     * @param   z1   z coordinate of the second point
     * @param   res  Voxel resolutions.
     *
     * @return  DOCUMENT ME!
     */
    public static final double distance(final float x0, final float y0, final float z0, final float x1, final float y1,
                                        final float z1, final float[] res) {

        return Math.sqrt(((x1 - x0) * (x1 - x0) * (res[0] * res[0])) + ((y1 - y0) * (y1 - y0) * (res[1] * res[1])) +
                         ((z1 - z0) * (z1 - z0) * (res[2] * res[2])));
    }

    /**
     * Returns the real length (adjusted by the image resolution) of the line between (x[0], y[0]) and (x[1], y[1]).
     *
     * @param   x    array of the x coordinates
     * @param   y    array of the y coordinates
     * @param   res  resolutions in each dimension
     *
     * @return  returns the length
     */
    public static final double length(final float[] x, final float[] y, final float[] res) {
        double length;

        // length is (x1-x0) squared times the x resolution
        // plus (y1-y0) squared times the y resolution
        length = Math.sqrt(((x[1] - x[0]) * (x[1] - x[0]) * (res[0]) * (res[0])) +
                           ((y[1] - y[0]) * (y[1] - y[0]) * (res[1]) * (res[1])));

        return length;
    }

    /**
     * Returns the real length (adjusted by the image resolution) of the line between (x[0], y[0]) and (x[1], y[1]).
     *
     * @param   x    array of the x coordinates
     * @param   y    array of the y coordinates
     * @param   res  resolutions in each dimension
     *
     * @return  returns the length
     */
    public static final double length(final double[] x, final double[] y, final float[] res) {
        double length;

        // length is (x1-x0) squared times the x resolution
        // plus (y1-y0) squared times the y resolution
        length = Math.sqrt(((x[1] - x[0]) * (x[1] - x[0]) * (res[0]) * (res[0])) +
                           ((y[1] - y[0]) * (y[1] - y[0]) * (res[1]) * (res[1])));

        return length;
    }

    /**
     * Returns the real length (adjusted by the image resolution) of the line between (x0, y0) and (x1, y1).
     *
     * @param   x0   x coordinate of the first point
     * @param   y0   y coordinate of the first point
     * @param   x1   x coordinate of the second point
     * @param   y1   y coordinate of the second point
     * @param   res  resolutions in each dimension
     *
     * @return  returns the length
     */
    public static final double length(final float x0, final float y0, final float x1, final float y1,
                                      final float[] res) {
        double length;

        // length is (x1-x0) squared times the x resolution
        // plus (y1-y0) squared times the y resolution
        length = Math.sqrt(((x1 - x0) * (x1 - x0) * (res[0]) * (res[0])) +
                           ((y1 - y0) * (y1 - y0) * (res[1]) * (res[1])));

        return length;
    }

    /**
     * Returns the real length (adjusted by the image resolution) of the line between (x0, y0) and (x1, y1).
     *
     * @param   x0   x coordinate of the first point
     * @param   y0   y coordinate of the first point
     * @param   x1   x coordinate of the second point
     * @param   y1   y coordinate of the second point
     * @param   res  resolutions in each dimension
     *
     * @return  returns the length
     */
    public static final double length(final double x0, final double y0, final double x1, final double y1,
                                      final float[] res) {
        double length;

        // length is (x1-x0) squared times the x resolution
        // plus (y1-y0) squared times the y resolution
        length = Math.sqrt(((x1 - x0) * (x1 - x0) * (res[0]) * (res[0])) +
                           ((y1 - y0) * (y1 - y0) * (res[1]) * (res[1])));

        return length;
    }

    /**
     * Round the value of type float to the closest integer. Java's Math.round is incredibly slow.
     * Note that this function does not work correctly for a = -0.49999997f, the closest floating point
     * number to -0.5f, and returns -1, while the slower Java Math.round at least returns the
     * correct value of 0. 
     *
     * @param   a  float the value to be rounded.
     *
     * @return  int Returns the closest integer.
     */
    public static final int round(final float a) {

        if (a < 0) {
            return ((int) (a - 0.5f));
        } else {
            return ((int) (a + 0.5f));
        }
    }

    /**
     * Round the value of type double to the closest integer. Java's Math.round is incredibly slow.
     *
     * @param   a  float the value to be rounded.
     *
     * @return  int Returns the closest integer.
     */
    public static final int round(final double a) {

        if (a < 0) {
            return ((int) (a - 0.5d));
        } else {
            return ((int) (a + 0.5d));
        }
    }
    
    /**
     * Calculates magnitude from real and imaginary parts magnitude = ( (real)^2 + (imaginary)^2 )^(1/2);
     */
    public static final float magnitude(float real, float imaginary){
        return (float)Math.sqrt(real*real + imaginary*imaginary);
    }
    
    /**
     * Calculates phase from real and imaginary parts.
     *
     * @return  phase  = arctan(imagData/realData);
     */
    public static final float phase(float real, float imaginary) {
           return (float)Math.atan2(imaginary, real);
    }
    
    /**
     * Determines if the input is an exact power of two. 
     * @param value
     * @return true if the input is an exact power of two, false otherwise.
     */
    public static final  boolean isPowerOfTwo(int value) {
        return BitHacks.IsPowerOfTwo(value);
    }

    /**
     * Calculates and returns the integer >= input value that is an exact power of two.
     * @param value number to calculate next power of two for.
     * @return integer >= value that is an exact power of two.
     */
    public static int nextPowerOfTwo( int value )
    {
        int exp = (int) Math.ceil( Math.log( value ) / Math.log( 2 ) );
        return (int) Math.pow( 2, exp );
    }
    
    /**
     * Calculate the dimension value to power of 2.
     * 
     * @param dim dimension value.
     * 
     * @return value dimension value in power of 2
     */
    public static int dimPowerOfTwo(int dim) {

        // 128^3 x 4 is 8MB
        // 256^3 x 4 is 64MB
        if (dim <= 16) {
            return 16;
        } else if (dim <= 32) {
            return 32;
        } else if (dim <= 64) {

            if (dim > 40) {
                return 64;
            } 
            return 32;
        } else if (dim <= 128) {

            if (dim > 80) {
                return 128;
            } 
            return 64;
        } else if (dim <= 256) {

            if (dim > 160) {
                return 256;
            } 
            return 128;
        } else if (dim <= 512) {

            if (dim > 448) {
                return 512;
            } 
            return 256;
        } else {
            return 512;
        }
    }

}
