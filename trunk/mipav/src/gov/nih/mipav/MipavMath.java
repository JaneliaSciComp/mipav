package gov.nih.mipav;


import gov.nih.mipav.model.structures.*;


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
    public static final double distance(final Point3Df pt1, final Point3Df pt2) {
        return Math.sqrt(((pt2.x - pt1.x) * (pt2.x - pt1.x)) + ((pt2.y - pt1.y) * (pt2.y - pt1.y)) +
                         ((pt2.z - pt1.z) * (pt2.z - pt1.z)));
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
}
