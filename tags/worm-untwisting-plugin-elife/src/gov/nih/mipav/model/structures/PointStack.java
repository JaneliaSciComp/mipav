package gov.nih.mipav.model.structures;


import java.awt.*;


/**
 * PointStack is a structure used specifically in ViewComponentEditImage for use in the active level set generation and
 * was built for speed. However, I have included it in the model package in the hope that it can be used elsewhere.
 *
 * @version  0.1 April 7, 1998
 * @author   Matthew J. McAuliffe
 */


public class PointStack {

    //~ Instance fields ------------------------------------------------------------------------------------------------

    /** DOCUMENT ME! */
    protected int[] xBuffer = new int[100]; // x coordinates

    /** DOCUMENT ME! */
    protected int[] yBuffer = new int[100]; // y coordinates

    /** DOCUMENT ME! */
    private int chunk = 50;

    /** DOCUMENT ME! */
    private int ptr = 0;

    //~ Constructors ---------------------------------------------------------------------------------------------------

    /**
     * PointStack.
     *
     * @param  capacity  initial size of the arrays
     */
    public PointStack(int capacity) {
        xBuffer = new int[capacity];
        yBuffer = new int[capacity];
    }

    //~ Methods --------------------------------------------------------------------------------------------------------

    /**
     * Adds a point to the arrays. Allocates more memory if necessary.
     *
     * @param  x  x coordinate
     * @param  y  y coordinate
     */
    public void addPoint(int x, int y) {

        int[] xTemp, yTemp;

        xBuffer[ptr] = x;
        yBuffer[ptr] = y;
        ptr++;

        if (ptr >= xBuffer.length) {
            xTemp = new int[xBuffer.length + chunk];
            yTemp = new int[yBuffer.length + chunk];

            System.arraycopy(xBuffer, 0, xTemp, 0, xBuffer.length);
            System.arraycopy(yBuffer, 0, yTemp, 0, yBuffer.length);

            xBuffer = xTemp;
            yBuffer = yTemp;
        }
    }

    /**
     * Repackages arrays into a java polygon returns points in polygon form.
     *
     * @return  DOCUMENT ME!
     */
    public Polygon exportPolygon() {

        return (new Polygon(xBuffer, yBuffer, ptr));
    }

    /**
     * Get a point and stores it in the two input arrays.
     *
     * @param  index  pointer into array
     * @param  x      x coordinate
     * @param  y      y coordinate
     */
    public void getPoint(int index, int[] x, int[] y) {

        x[0] = xBuffer[index];
        y[0] = yBuffer[index];
    }

    /**
     * Gets the x coordinate of a point.
     *
     * @param   index  pointer into array
     *
     * @return  x coordinate of point
     */
    public int getPointX(int index) {
        return xBuffer[index];
    }

    /**
     * Gets the y coordinate of a point.
     *
     * @param   index  pointer into array
     *
     * @return  y coordinate of point
     */
    public int getPointY(int index) {
        return yBuffer[index];
    }

    /**
     * Removes all elements in array by setting data pointer to zero.
     */
    public void reset() {
        ptr = 0;
    }

    /**
     * Sets the pointer to specific location return -1 if index exceeds location of valid data.
     *
     * @param   index  DOCUMENT ME!
     *
     * @return  DOCUMENT ME!
     */
    public int setIndex(int index) {

        if (index < ptr) {
            ptr = index;

            return index;
        } else {
            return -1;
        }

    }

    /**
     * Returns the number of points.
     *
     * @return  the size of the array (i.e., the number of points)
     */
    public int size() {
        return ptr;
    }
}
