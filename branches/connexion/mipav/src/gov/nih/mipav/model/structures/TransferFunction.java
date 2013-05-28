package gov.nih.mipav.model.structures;

import WildMagic.LibFoundation.Mathematics.Vector2f;
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

    /** The total capacity of the 2D point array. Initial capacity = 10 */
    private int capacity = 10;

    /** Points to the next available storage location for the point. */
    private int endPtr = 0;

    /** The array of 2D points that define the line. */
    private Vector2f[] pts;

    /** The array of pre-calculated slope values. */
    private float[] slopes;

    //~ Constructors ---------------------------------------------------------------------------------------------------

    /**
     * Simple class to hold an array of points that describe a transfer function.
     */
    public TransferFunction() {
        pts = new Vector2f[capacity];
    }

    //~ Methods --------------------------------------------------------------------------------------------------------

    /**
     * Adds a point to the function at the end.
     *
     * @param  pt  DOCUMENT ME!
     */
    public synchronized void addPoint(Vector2f pt) {

        checkSize();
        
        pts[endPtr] = new Vector2f(pt.X, pt.Y);
        endPtr++;
        recalculateSlopes();
    }


    /**
     * Adds a point to the function at the end.
     *
     * @param  x  the x coordinate of point to be added
     * @param  y  the y coordinate of point to be added
     */
    public synchronized void addPoint(float x, float y) {

        checkSize();

        pts[endPtr] = new Vector2f(x, y);
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
     * Copies the object that extends this class.
     *
     * @return  An exact copy of this class.
     */
    public TransferFunction( TransferFunction kTransfer )
    {
        pts = new Vector2f[ kTransfer.pts.length ];
        for ( int i = 0; i < kTransfer.pts.length; i++ )
        {
            if ( kTransfer.pts[i] != null )
            {
                pts[i] = new Vector2f( kTransfer.pts[i] );
            }
        }
        slopes = new float[ kTransfer.slopes.length ];
        for ( int i = 0; i < kTransfer.slopes.length; i++ )
        {
            slopes[i] = kTransfer.slopes[i];
        }
        capacity = kTransfer.capacity;
        endPtr = kTransfer.endPtr;
    }


    /**
     * Exports the float arrays of the points of the curve.
     *
     * @param  x  array of x coordinates
     * @param  y  array of y coordinates
     */
    public synchronized void exportArrays(float[] x, float[] y) {
        int i;

        if ((x == null) || (y == null)) {
            return;
        }

        if ((x.length != y.length) || (x.length < size())) {
            return;
        }
        for (i = 0; i < size(); i++) {
            x[i] = ((Vector2f) (getPoint(i))).X;
            y[i] = ((Vector2f) (getPoint(i))).Y;
        }
    }

    /**
     * Gets the points that describe the function.
     *
     * @return  the 2D point array of floats
     */
    public synchronized Vector2f[] getFunction() {
        return pts;
    }

    /**
     * Get the point at the specified index.
     *
     * @param   index  the index of the point to be returned.
     *
     * @return  the 2D point
     */
    public synchronized Vector2f getPoint(int index) {

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
    public synchronized final float getRemappedValue(float inputValue, int height) {

        int i;
        float slope = 0;
        float ptX1, ptX2, ptY1;
        float rmValue = 0;

        if (pts == null) {
            return 0;
        }

        for (i = 0; i < (endPtr - 1); i++) {
            ptX1 = pts[i].X;
            ptX2 = pts[i + 1].X;
            ptY1 = (height - 1) - pts[i].Y;


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
    public synchronized void importArrays(float[] x, float[] y, int n) {
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
    public synchronized void insertPoint(Vector2f pt, int index) {

        checkSize();
        
        for (int i = endPtr - 1; i >= index; i--) {
            pts[i + 1] = pts[i];
        }

        pts[index] = new Vector2f(pt.X, pt.Y);
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
    public synchronized void insertPoint(float x, float y, int index) {

        checkSize();
        
        for (int i = endPtr - 1; i >= index; i--) {
            pts[i + 1] = pts[i];
        }

        pts[index] = new Vector2f(x, y);
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
    public  synchronized boolean isEndpoint(int index) {
        return (index == 0) || (index == (endPtr - 1));
    }

    /**
     * Removes all points from the funcitons.
     */
    public synchronized Vector2f[] removeAll() {
        endPtr = 0;
        recalculateSlopes();
        
        return pts;
    }

    /**
     * Removes a point in the function at the specified index.
     *
     * @param  index  the index where the point is to be removed
     */
    public synchronized Vector2f removePoint(int index) {
    	Vector2f old = pts[index];
    	
        for (int i = index; i < endPtr; i++) {
            pts[i] = pts[i + 1];
        }

        endPtr--;
        recalculateSlopes();

        return old;
    }

    /**
     * Replaces the point at param index with point of pt.
     *
     * @param  pt     Vector2f the new point
     * @param  index  int the index of the point to be replaced
     */
    public synchronized void replacePoint(Vector2f pt, int index) {
    	
        if ((index >= 0) && (index < pts.length)) {
            pts[index].X = pt.X;
            pts[index].Y = pt.Y;
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
    public synchronized void replacePoint(float x, float y, int index) {

        if ((index >= 0) && (index < pts.length)) {
            pts[index].X = x;
            pts[index].Y = y;
        }

        recalculateSlopes();
    }

    /**
     * Indicated the number of points in the function.
     *
     * @return  the number of points in the function.
     */
    public synchronized int size() {
        return endPtr;
    }
    
    public String toString()
    {
        String kString = new String("Transfer " + endPtr + " " );
        for ( int i = 0; i < pts.length; i++ )
        {
            if ( pts[i] != null )
            {
                kString = kString.concat( pts[i] + "   " );
            }
        }
        return kString;
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
    private synchronized void checkSize() {

        if (endPtr == pts.length) {
            capacity = capacity + 10;

            Vector2f[] tmpPts = pts;
            pts = new Vector2f[capacity];

            for (int i = 0; i < endPtr; i++) {
                pts[i] = tmpPts[i];
            }
        }
    }

    /**
     * DOCUMENT ME!
     */
    private synchronized void recalculateSlopes() {

        if (endPtr > 1) {
            float[] in = new float[endPtr];
            float[] out = new float[endPtr];
//System.err.println("Pts.length: " + pts.length + ", endPtr: " + endPtr);
            for (int i = 0; i < endPtr ; i++) {
                in[i] = pts[i].X;
                out[i] = 255.0f - (pts[i].Y);
            }
//System.err.println("endPtr - 1 = " + (endPtr - 1));
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
