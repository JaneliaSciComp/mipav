package gov.nih.mipav.model.structures;


/**
 * <p>Title: TransferFunction</p>
 *
 * <p>Description:</p>
 *
 * <p>Simple class to hold an array of points that describe a transfer function.</p>
 *
 * @author   not attributable
 * @version  1.0
 */
public class TransferFunction extends ModelSerialCloneable {

    //~ Static fields/initializers -------------------------------------------------------------------------------------

    /** Use serialVersionUID for interoperability. */
    private static final long serialVersionUID = -6859218214454354638L;

    //~ Instance fields ------------------------------------------------------------------------------------------------

    /** The total capacity of the 2D point array. Intial capapcity = 10 */
    private int capacity = 10;

    /** Points to the next available storage location for the point. */
    private int endPtr = 0;

    /** The array of 2D points that define the line. */
    private Point2Df[] pts;

    /** The array of pre-calculated slope values. */
    private float[] slopes;

    //~ Constructors ---------------------------------------------------------------------------------------------------

    /**
     * Simple class to hold an array of points that describe a transfer function.
     */
    public TransferFunction() {
        pts = new Point2Df[capacity];
    }

    //~ Methods --------------------------------------------------------------------------------------------------------

    /**
     * Adds a point to the function at the end.
     *
     * @param  pt  DOCUMENT ME!
     */
    public void addPoint(Point2Df pt) {

        checkSize();

        pts[endPtr] = new Point2Df(pt.x, pt.y);
        endPtr++;
        recalculateSlopes();
    }


    /**
     * Adds a point to the function at the end.
     *
     * @param  x  the x coordinate of point to be added
     * @param  y  the y coordinate of point to be added
     */
    public void addPoint(float x, float y) {

        checkSize();

        pts[endPtr] = new Point2Df(x, y);
        endPtr++;
        recalculateSlopes();
    }


    /**
     * Copies the object that extends this class.
     *
     * @return  An exact copy of this class.
     */
    public Object clone() {
        TransferFunction cloned = new TransferFunction();
        cloned.pts = pts;
        cloned.slopes = slopes;
        cloned.capacity = capacity;
        cloned.endPtr = endPtr;

        return cloned;
    }


    /**
     * Exports the float arrays of the points of the curve.
     *
     * @param  x  array of x coordinates
     * @param  y  array of y coordinates
     */
    public void exportArrays(float[] x, float[] y) {
        int i;

        if ((x == null) || (y == null)) {
            return;
        }

        if ((x.length != y.length) || (x.length < size())) {
            return;
        }

        for (i = 0; i < size(); i++) {
            x[i] = ((Point2Df) (getPoint(i))).x;
            y[i] = ((Point2Df) (getPoint(i))).y;
        }
    }

    /**
     * Gets the points that describe the function.
     *
     * @return  the 2D point array of floats
     */
    public Point2Df[] getFunction() {
        return pts;
    }

    /**
     * Get the point at the specified index.
     *
     * @param   index  the index of the point to be returned.
     *
     * @return  the 2D point
     */
    public Point2Df getPoint(int index) {

        if ((index >= 0) && (index < endPtr)) {
            return pts[index];
        } else {
            return null;
        }
    }

    /**
     * Method to remapped a value using the transfer function.
     *
     * @param   inputValue  the value to be remapped
     * @param   height      return range max of the remapped value
     *
     * @return  the remapped value
     */
    public final float getRemappedValue(float inputValue, int height) {

        int i;
        float slope = 0;
        float ptX1, ptX2, ptY1;
        float rmValue = 0;

        if (pts == null) {
            return 0;
        }

        for (i = 0; i < (endPtr - 1); i++) {
            ptX1 = pts[i].x;
            ptX2 = pts[i + 1].x;
            ptY1 = (height - 1) - pts[i].y;


            if ((inputValue >= ptX1) && (inputValue <= ptX2)) {

                if ((ptX2 - ptX1) != 0) {
                    slope = slopes[i]; // (ptY2 - ptY1) / (ptX2 - ptX1);
                } else {
                    slope = 0;
                }

                return ptY1 + (slope * (inputValue - ptX1));
            }
        }

        return rmValue;
    }


    /**
     * Import points into the contour.
     *
     * @param  x  array of x points
     * @param  y  array of y points
     * @param  n  number of points in the array
     */
    public void importArrays(float[] x, float[] y, int n) {
        int i;

        try {
            removeAll();

            for (i = 0; i < n; i++) {
                addPoint(x[i], y[i]);
            }
        } catch (OutOfMemoryError error) {
            System.gc();
            throw error;
        }
    }

    /**
     * Inserts a point to the transfer function at the specified index.
     *
     * @param  pt     the 2D point to be added to the function.
     * @param  index  the index the point is to be inserted.
     */
    public void insertPoint(Point2Df pt, int index) {

        checkSize();

        for (int i = endPtr - 1; i >= index; i--) {
            pts[i + 1] = pts[i];
        }

        pts[index] = new Point2Df(pt.x, pt.y);
        endPtr++;
        recalculateSlopes();
    }

    /**
     * Inserts a point to the transfer function at the specified index.
     *
     * @param  x      the x coordinate of the 2D point to be added to the function.
     * @param  y      the y coordinate of the 2D point to be added to the function.
     * @param  index  the index the point is to be inserted.
     */
    public void insertPoint(float x, float y, int index) {

        checkSize();

        for (int i = endPtr - 1; i >= index; i--) {
            pts[i + 1] = pts[i];
        }

        pts[index] = new Point2Df(x, y);
        endPtr++;
        recalculateSlopes();
    }


    /**
     * Decides whether parameter 'index' is an endpoint of this transfer function.
     *
     * @param   index  int The index to test whether or not it is an endpoint
     *
     * @return  boolean
     */
    public boolean isEndpoint(int index) {
        return (index == 0) || (index == (endPtr - 1));
    }

    /**
     * Removes all points from the funcitons.
     */
    public void removeAll() {
        endPtr = 0;
        recalculateSlopes();
    }

    /**
     * Removes a point in the function at the specified index.
     *
     * @param  index  the index where the point is to be removed
     */
    public void removePoint(int index) {

        for (int i = index; i < endPtr; i++) {
            pts[i] = pts[i + 1];
        }

        endPtr--;
        recalculateSlopes();
    }

    /**
     * Replaces the point at param index with point of pt.
     *
     * @param  pt     Point2Df the new point
     * @param  index  int the index of the point to be replaced
     */
    public void replacePoint(Point2Df pt, int index) {

        if ((index >= 0) && (index < pts.length)) {
            pts[index].x = pt.x;
            pts[index].y = pt.y;
        }

        recalculateSlopes();
    }

    /**
     * Replaces the point at param index with point of params x and y.
     *
     * @param  x      float the x-coordinate of the new point
     * @param  y      float the y-coordinate of the new point
     * @param  index  int the index of the point to be replaced
     */
    public void replacePoint(float x, float y, int index) {

        if ((index >= 0) && (index < pts.length)) {
            pts[index].x = x;
            pts[index].y = y;
        }

        recalculateSlopes();
    }

    /**
     * Indicated the number of points in the function.
     *
     * @return  the number of points in the function.
     */
    public int size() {
        return endPtr;
    }

    /**
     * Cleans up memory.
     */
    protected void finalize() {

        for (int i = 0; i < pts.length; i++) {
            pts[i] = null;
        }

        pts = null;
    }

    /**
     * Checks the size of the point array to make sure it is big enough. If not the Points buffer is reallocated to a
     * larger size.
     */
    private void checkSize() {

        if (endPtr == pts.length) {
            capacity = capacity + 10;

            Point2Df[] tmpPts = pts;
            pts = new Point2Df[capacity];

            for (int i = 0; i < endPtr; i++) {
                pts[i] = tmpPts[i];
            }
        }
    }

    /**
     * DOCUMENT ME!
     */
    private void recalculateSlopes() {

        if (endPtr > 1) {
            float[] in = new float[endPtr];
            float[] out = new float[endPtr];

            for (int i = 0; i < endPtr; i++) {
                in[i] = pts[i].x;
                out[i] = 255.0f - (pts[i].y);
            }

            slopes = new float[endPtr - 1];

            for (int i = 1; i < endPtr; i++) {

                if (in[i] == -in[i - 1]) {
                    slopes[i - 1] = 0.0f;
                } else {
                    slopes[i - 1] = (out[i] - out[i - 1]) / (in[i] - in[i - 1]);
                }
            }
        }
    }

}
