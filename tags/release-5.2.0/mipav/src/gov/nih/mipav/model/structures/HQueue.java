package gov.nih.mipav.model.structures;


import gov.nih.mipav.util.MipavMath;


/**
 * This class is used specifically for the watershed algorithm. It is a version of Hierarcial Queue that greatly
 * increases the speed of the watershed process.
 * 
 * @version 0.1 Mar, 1998
 * @author Matthew J. McAuliffe, Ph.D.
 * @author Ray Lert
 */
public class HQueue {

    // ~ Instance fields
    // ------------------------------------------------------------------------------------------------

    /** DOCUMENT ME! */
    private final float hmin;

    /** DOCUMENT ME! */
    private final IntVector[] intVect;

    /** DOCUMENT ME! */
    private final int MAX;

    /** DOCUMENT ME! */
    private int ptr; // points to the lowest (first) bin with data in it

    /** DOCUMENT ME! */
    private final float slope;

    // ~ Constructors
    // ---------------------------------------------------------------------------------------------------

    /**
     * Hierarcial Queue - constructor.
     * 
     * @param hmin minimum value of data to be mapped into the queue
     * @param hmax maximum value of the queue to be mapped into the queue
     * @param MAX number of queue locations
     * @param queueSize used to initialize individual queue sizes.
     */
    public HQueue(final float hmin, final float hmax, final int MAX, final int queueSize) {

        intVect = new IntVector[MAX]; // allocate integer vectors

        for (int i = 0; i < MAX; i++) {

            /*
             * if (i < 10){ // allocate larger arrays for small values (i.e. background) intVect[i] = new
             * IntVector(imageSize/20, imageSize/40); } else if ( i >= 10 && i <= 50){ intVect[i] = new
             * IntVector(imageSize/40, imageSize/80); } else if ( i >= 51 && i <= 100){ intVect[i] = new IntVector(4000,
             * 2000); }else {
             */
            intVect[i] = new IntVector(4000, 2000);
            // }
        }

        slope = (MAX - 1) / (hmax - hmin);
        this.hmin = hmin;
        this.MAX = MAX;
        ptr = MAX + 1;
    }

    /**
     * Hierarcial Queue - constructor.
     * 
     * @param hmin minimum value of data to be mapped into the queue
     * @param hmax maximum value of the queue to be mapped into the queue
     * @param MAX number of queue locations
     * @param imageHisto histogram of image used to set queue sizes
     */
    public HQueue(final float hmin, final float hmax, final int MAX, final int[] imageHisto) {

        intVect = new IntVector[MAX]; // allocate integer vectors

        for (int i = 0; i < MAX; i++) {

            if (imageHisto[i] > 0) {
                intVect[i] = new IntVector(imageHisto[i] + 10, 1000);
            } else {
                intVect[i] = new IntVector(100, 100);
            }

        }

        slope = (MAX - 1) / (hmax - hmin);
        this.hmin = hmin;
        this.MAX = MAX;
        ptr = MAX + 1;
    }

    // ~ Methods
    // --------------------------------------------------------------------------------------------------------

    /**
     * add - adds a value to the queue.
     * 
     * @param imgIndex an index to a location in the image
     * @param GradImIn float value (gradient magnitude of the image at the location imgIndex;
     */
    public final void add(final int imgIndex, final float GradImIn) {
        int index;

        index = MipavMath.round( (GradImIn - hmin) * slope); // remap data into queue range
        intVect[index].addElement(imgIndex); // add image location(index) to queue

        if (index < ptr) { // update pointers
            ptr = index;
        }
    }

    /**
     * cloneElementsAt();
     * 
     * @param i start index
     * @param j end index
     * 
     * @return cloned integer vector beginning at i and ending at j
     */
    public IntVector cloneElementsAt(int i, int j) {

        IntVector pixelsVector = new IntVector();

        int temp;

        if (i > j) {
            temp = i;
            i = j;
            j = temp;
        }

        pixelsVector = intVect[i].copy();
        i++;

        for (temp = i; temp <= j; temp++) {
            pixelsVector = intVect[temp].copy(pixelsVector);
        }

        return pixelsVector;
    }

    /**
     * dispose - cleans up memory.
     */
    public void dispose() {

        for (int i = 0; i < MAX; i++) {
            intVect[i] = null;
        }

        System.gc();
    }

    /**
     * first - returns the last value in the first queue location that has image indexes in it. (LIFO)
     * 
     * @return DOCUMENT ME!
     */
    public final int first() {

        final int pt = intVect[ptr].lastElement(); // / Get most recent (LIFO)
        intVect[ptr].removeLastElement();

        // int pt = intVect[ptr].firstElement(); /// FIFO - very inefficient method
        // intVect[ptr].removeElementAt(0);
        // int pt = intVect[ptr].smallestElement();

        while (intVect[ptr].isEmpty()) {
            ptr++;

            if (ptr == MAX) {
                return pt;
            }
        }

        return pt;
    }

    /**
     * firstAt - removes and returns last element at a specific queue index.
     * 
     * @param index indicates a specific location in the queue return last value (image index) at a specific queue
     *            location
     * 
     * @return DOCUMENT ME!
     */
    public int firstAt(final int index) {
        int pt;
        pt = intVect[index].lastElement();
        intVect[index].removeLastElement();

        return pt;
    }

    /**
     * isEmpty - returns a flag indicating if the entire hierarcial queue is empty.
     * 
     * @return flag indicating if the entire queue structure is empty
     */
    public boolean isEmpty() {

        if (ptr < MAX) {
            return false;
        } else {
            return true;
        }
    }

    /**
     * isEmptyAt - returns a flag indicating if a specific queue is empty.
     * 
     * @param index location in the queue
     * 
     * @return flag indicating if a queue location is empty
     */
    public boolean isEmptyAt(final int index) {
        return intVect[index].isEmpty();
    }

}
