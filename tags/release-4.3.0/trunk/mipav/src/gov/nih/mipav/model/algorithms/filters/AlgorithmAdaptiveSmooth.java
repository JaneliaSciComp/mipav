package gov.nih.mipav.model.algorithms.filters;


import gov.nih.mipav.model.algorithms.*;
import gov.nih.mipav.model.structures.*;

import gov.nih.mipav.view.*;

import java.io.*;


/**
 * This code is a port of image noise removal filter software by Karlis Freivalds found at
 * http://www.gradetools.com/karlisf with one major modification. In the original program the weighted sum could be
 * applied to all pixels in the (2*range+1)*(2*range+1) local square. In this program the lowest cost pixel at the edge
 * of the (2*range+1)*(2*range+1) local neighborhood is found and that pixel and the pixels along the low cost path back
 * to the original center pixel are the only pixels used in the weighted sum. At the start of the program RGB color
 * space is transformed in YCrCb space and at the end of the program YCrCb space is transformed into RGB space. Hence,
 * all of the program operations occur in YcrCb space. Frequently Asked Questions About Color by Charles Poynton at
 * http://www.poynton.com/colorFAQ.html reveals that the RGB to YCrCb and YCrCb to RGB conversion equations used in the
 * Freivalds program are significantly different from the standard ones. Black and white images are not transformed into
 * another space. ----------- Description ----------- The filter reduces noise in the image without blurring the edges.
 * The results are much better than can be achieved by using the popular adaptive median or smoothing filters. Like
 * modern compression algorithms (including JPEG) the filter operates in Y,Cr, Cb color space, hence it is useful for
 * removing compression artifacts. ----------------- How does it work? ----------------- It is based on the search of
 * the pixel neighbor graph. The main steps of the algorithm are: - create the pixel neighbor graph, set edge weights
 * based on the differences between the pixels - for the pixel being filtered calculate the shortest path from it to all
 * the other pixels reachable with path length < d - replace the pixel value with the weighted average of all pixels in
 * the path from the low cost edge pixel to the center pixel with weights taken as the Gaussian function of the path
 * length In this way only the relevant filtering neighborhood for each pixel is selected. The size and shape of the
 * neighborhood adopt to the image features, including curved ones and sharp corners. Note that the edge graph is
 * created with a median filtered version of the YCrCb space. The median filter only obtains a median of the 3 by 3 area
 * if no edge is detected in any of Y, Cr, or Cb spaces. If an edge is detected in any of the 3 spaces, then the Y, Cr,
 * and Cb values are all unchanged. An edge is detected if the difference between the center pixel and any one of its 4
 * nearest neighbors is greater than a threshold or if the center pixel is larger than all of its four neighbors by
 * threshold/2 or if the center pixel is smaller than all of its four neighbors by threshold/2. The Y, Cr, and Cb spaces
 * have 3 different thresholds. The pixel neighbor or edge graph is an array of arrays, with the first array having a
 * length equal to the number of the pixels in the image and the second array having 8 integers. The 8 values are the
 * edge weights calculated from the intensity differences between the center pixel and one of the 8 neighboring pixels.
 * edge weight = sqrt((dy*wY)**2 + (dr*wR)**2 + (dB*wB)**2), where dy, dR, and dB are the Y, Cr, and Cb differences and
 * wY, wR, and wB are the Y, Cr, and Cb weights derived from the Y, Cr, and Cb radiuses. A bigger relative radius
 * results in a smaller weight for radiuses >= 0.1, which should typically be the case. After the edge graph is created,
 * the Y, Cr, and Cb spaces are separately filtered. The filter uses the pixel as the center of a square with sides of
 * length 2*range + 1, with range = (int)(radius + 0.999). A priority queue is created with all of the queue elements
 * having a key, a baseKey, a value, and an origin. After the priority queue is created, the filter table is created.
 * Then the distMap distance function table is created. The distMap at a given position inside the maxLength =
 * (2*range+1)*(2*range+1) local square is equal to distWeight times the distance from the center pixel. Finally, the
 * function enhancePixel, which returns the filtered pixel value at coordinate (x,y), is applied to every pixel in the
 * image. If neighboring pixels are connected to the center pixel via a low cost path, queue elements corresponding to
 * the neighboring pixels are added to the priority queue. If no queue element is retrieved from the queue, then the
 * original value is passed unchanged. If elements are retrieved from the queue, then those Y, Cr, or Cb pixel values
 * which had retrieved queue elements in the path from the low cost edge pixel to the center pixel are put into a
 * weighted sum. The weight is derived from an index into the filter array given by the queue element key times the
 * invFilterLen. value gives the location within the maxLength = (2*range+1)*(2*range+1) local area. value = x - minX +
 * (y - minY)*(2*range + 1). baseKey contains a sum of all the edgeGraph values over the low cost path from the center
 * pixel to the selected pixel. key = baseKey + the distMap value for the selected pixel. origin is set to the value of
 * the queue elment which provides the neighboring pixel on the path back to the center pixel. The center pixel origin =
 * -1. A position is only added to the priority queue if key < maxDistance. Initially, the seen array is set equal to 0
 * for all queue elements within the local area. After a queue element has been added and retrieved from the queue, seen
 * is set equal to 2 for that position to prevent later addition to the queue. The variable last gives the number of
 * queue elements in the priority queue. The places array of length maxLength gives the queue element number of that
 * local area position or -1 if the local area position has no element present in the priority queue. The priority queue
 * is implemented as a binary heap and the binary heap is implemented as an array. A binary heap is a "nearly full"
 * binary tree(only the bottom level may not be complete). This binary heap has the property that for every node other
 * than a leaf, its key value is less than or equal to that of its children. In a binary heap the item of highest
 * priority is always at the root of the tree or at node 0. When the highest priority item at the root is removed, the
 * item of highest priority among the remainder moves into the root position. The children of node i are at 2*i + 1 and
 * 2*i + 2. The parent of node i is at (int)((i-1)/2). Adding an element e to the priority queue: The places of e.value
 * returns a number p. p >= 0 if the queue element is already in the priority queue. p = -1 if the queue element is not
 * in the priority queue. If element e is already in the queue with a smaller key value than the key value attached to
 * e, then simply return without putting e in the queue. If e is not in the queue and last, the number of queue
 * elements, has already reached the maximum size, then return with a full queue error. If e is not in the queue and
 * last is less than maxsize, set a new queue position i equal to last and then increment last. If e is already in the
 * queue, set i to p. Then, place e at queue position i. As long as i > 0 and the key of queue element i is less than
 * the key of its parent at position (int)((i-1)/2), then interchange the parent and the child queue elements. Set the
 * places of the moved queue elements to the new queue positions. Getting an element e from the priority queue: The
 * highest priority queue element, that is the queue element with the smallest key value, is always at queue[0], the
 * root of the queue. Hence, queue[0] is always returned. If last == 0, no elements are in the queue, so return with a
 * priority queue is empty error message. If last is not zero, set the places of the queue[0] element to -1 to indicate
 * that it is no longer in the queue. Decrement last. If last now equals zero, then the queue is now empty, so simply
 * return. If last does not equal zero, then move queue[last] into the root queue[0] position. Keep looping as long as
 * the right child position, 2*i + 2, is <= last. If the key of the left child position at j - 1 is less than the key of
 * the right child position at j, then decrement j to select the left child. If j == last, then the right child position
 * is empty, so decrement to select the left child. Otherwise, the right child position at j has the smaller key so do
 * not decrement j. If the key of the parent is greater than the key of the child, then swap the parent and child. Set
 * the places of the moved queue elements to the new queue positions. Stop looping when the key of the parent is <= the
 * key of the child. -------- Settings -------- You can adjust several parameters to obtain the best noise reduction
 * level for each particular image: - Filter radius for each Y, Cr, Cb color channel: Larger radius will remove more
 * noise, however some detail also can be lost. - Edge sensitivity: larger values will preserve more edges but sacrifice
 * smoothness in low contrast areas. For very large values banding may appear. - Cr and Cb 2x reduction: The resolution
 * of Cr and Cb channels is reduced two times, which improves the speed of the algorithm for large radiuses with little
 * loss of quality. Recommended settings are. - for high quality scanned images radius Y 2, Cr 2 Cb 2 Edge Sensitivity
 * 3072 Cr and Cb 2x reduction false - for high quality JPEG images radius Y 2, Cr 3 Cb 4 Edge Sensitivity 3072 Cr and
 * Cb 2x reduction false - for low quality JPEG images radius Y 3, Cr 4 Cb 6 Edge Sensitivity 3072 Cr and Cb 2x
 * reduction true
 */
public class AlgorithmAdaptiveSmooth extends AlgorithmBase {

    //~ Instance fields ------------------------------------------------------------------------------------------------

    /** conatains Y, Cr, or Cb array for processing. */
    private int[] data;

    /** Conatains pixel values from black and white image for processing. */
    private float[] dataBW;

    /** 256*deltaCb = threshold used in isEdgeY for Cb component. */
    private int deltaCb = 100;

    /** 256*deltaCr = threshold used in isEdgeY for Cr component. */
    private int deltaCr = 30;

    /** 256*deltaY = threshold used in isEdgeY for Y component. */
    private int deltaY = 15;

    /** The distMap at a postion inside the (2*range+1)*(2*range+1) local square. */
    /** equals the distWeight times the distance from the center pixel. */
    private float[] distMap;

    /** DOCUMENT ME! */
    private float distWeight = 12 * 256; // for color

    /** Contains edge weight between center and neighbor pixel in ARGB. */
    private int[][] edgeGraph;

    /** Contains edge weight between center and neighbor pixel in black and white. */
    private float[][] edgeGraphBW;

    /** DOCUMENT ME! */
    private double[] filter;

    /** DOCUMENT ME! */
    private int height;

    /** Contains key from queue, which has sum of edgeGraph and distMap values. */
    private float[] histDistance;

    /** contains postion of originating queue element in. */
    /** (2*range+1)*(2*range+1) local square. */
    private int[] histOrigin;

    /** contains x position of queue element. */
    private int[] histX;

    /** contains y position of queue element. */
    private int[] histY;

    /** DOCUMENT ME! */
    private double invFilterLen;

    /** DOCUMENT ME! */
    private double invWeight;

    /** length = xDim * yDim. */
    private int length;

    /** otherwise = (12.0/255.0)* (srcImage.getMax() - srcImage.getMin()). */
    private int maxDistance = 40 * 256;

    /** maxLength = size of complete square local area = (2*range+1)*(2*range+1). */
    private int maxLength;

    /** DOCUMENT ME! */
    private PriorityQueue Q;

    /** DOCUMENT ME! */
    private float radius;

    /** DOCUMENT ME! */
    private float radiusCb = 5.0f;

    /** DOCUMENT ME! */
    private float radiusCr = 4.0f;

    /** DOCUMENT ME! */
    private float radiusY = 2.0f;

    /** DOCUMENT ME! */
    private int range = 3;

    /** DOCUMENT ME! */
    private boolean reduce = false;

    /** Initially 0, set to 2 after queue element is added and retrieved from queue. */
    private char[] seen;

    /** DOCUMENT ME! */
    private int width;

    /** weights assigned to Y, Cr, and Cb differences. */
    private double wY, wR, wB;

    /** DOCUMENT ME! */
    private int xDim, yDim;

    //~ Constructors ---------------------------------------------------------------------------------------------------

    /**
     * Creates a new AlgorithmAdaptiveSmooth object.
     *
     * @param  destImage   denoised image
     * @param  srcImg      2D source image
     * @param  radiusY     DOCUMENT ME!
     * @param  radiusCr    DOCUMENT ME!
     * @param  radiusCb    DOCUMENT ME!
     * @param  distWeight  DOCUMENT ME!
     * @param  reduce      DOCUMENT ME!
     */
    public AlgorithmAdaptiveSmooth(ModelImage destImage, ModelImage srcImg, float radiusY, float radiusCr,
                                   float radiusCb, float distWeight, boolean reduce) {
        super(destImage, srcImg);
        this.radiusY = radiusY;
        this.distWeight = distWeight;
        this.radiusCr = radiusCr;
        this.reduce = reduce;
        this.radiusCb = radiusCb;
    }

    //~ Methods --------------------------------------------------------------------------------------------------------

    /**
     * Prepares this class for destruction.
     */
    public void finalize() {
        srcImage = null;
        destImage = null;
        data = null;
        histX = null;
        histY = null;
        histOrigin = null;
        histDistance = null;
        filter = null;
        seen = null;

        if (edgeGraph != null) {

            for (int i = 0; i < edgeGraph.length; i++) {
                edgeGraph[i] = null;
            }

            edgeGraph = null;
        }

        distMap = null;
        super.finalize();
    }


    /**
     * Starts the adaptive noise removal filter algorithm.
     */
    public void runAlgorithm() {

        if (srcImage == null) {
            displayError("Source Image is null");

            return;
        } else {

            if (srcImage.getNDims() == 2) {

                if (srcImage.getType() == ModelStorageBase.ARGB) {
                    calc2D();
                } else if (!srcImage.isColorImage()) {
                    calc2DBW();
                }
            }
            /*else if (srcImage.getNDims() > 2) {
             *  if (do25D) {     calc25D(); } else {     calc3D(); }}*/
        }
    }

    /**
     * This method returns the edge weight based on the color differences between pixels.
     *
     * @param   dy  DOCUMENT ME!
     * @param   dr  DOCUMENT ME!
     * @param   db  DOCUMENT ME!
     *
     * @return  DOCUMENT ME!
     */
    int getEdgeWeight(int dy, int dr, int db) {
        return (int) (Math.sqrt((dy * dy * wY * wY) + (dr * dr * wR * wR) + (db * db * wB * wB)) + 0.5);
    }

    /**
     * Prepares the data and runs the algorithm for a 2D ARGB image Convert from RGB to YCrCb space, run process, and
     * convert back from YCrCb space to RGB space.
     */
    private void calc2D() {

        int[] imgBuffer;
        int[] Y;
        int[] Cr;
        int[] Cb;

        xDim = srcImage.getExtents()[0];
        yDim = srcImage.getExtents()[1];

        try {
            length = xDim * yDim;
            Y = new int[length];
            Cr = new int[length];
            Cb = new int[length];
            imgBuffer = new int[4 * length];
            srcImage.exportData(0, 4 * length, imgBuffer); // locks and releases lock
           
        } catch (IOException error) {
            cleanup();
            displayError("AlgorithmAdaptiveSmooth: Image(s) locked");
            setCompleted(false);

            return;
        } catch (OutOfMemoryError e) {
            cleanup();
            displayError("AlgorithmAdaptiveSmooth:  Out of Memory");
            setCompleted(false);

            return;
        }


        fireProgressStateChanged(0, srcImage.getImageName(), "Converting from RGB to YCrCb ...");

        rgb2yCrCb(imgBuffer, Y, Cr, Cb);

        if (threadStopped) {
            cleanup();
            finalize();

            return;
        }

        process(Y, Cr, Cb);

        if (threadStopped) {
            cleanup();
            finalize();

            return;
        }

        fireProgressStateChanged((.95f), srcImage.getImageName(), "Converting fromYCrCb to RGB ...");
     
        yCrCb2rgb(imgBuffer, Y, Cr, Cb);

        if (threadStopped) {
            cleanup();
            finalize();

            return;
        }

        if (destImage != null) {

            try {
                destImage.importData(0, imgBuffer, true);
            } catch (IOException error) {
                cleanup();
                displayError("AlgorithmAdaptiveSmooth: destImage locked " + error);
                setCompleted(false);

                return;
            }
        } else {

            try {
                srcImage.importData(0, imgBuffer, true);
            } catch (IOException error) {
                cleanup();
                displayError("AlgorithmAdaptiveSmooth: srcImage locked " + error);
                setCompleted(false);

                return;
            }
        }

        fireProgressStateChanged(100, srcImage.getImageName(), "Converting fromYCrCb to RGB ...");

        if (threadStopped) {
            cleanup();
            finalize();

            return;
        }

        cleanup();
        setCompleted(true);
    }

    /**
     * Prepares the data and runs the algorithm for a 2D black and white image Run process.
     */
    private void calc2DBW() {

        float[] imgBuffer;

        xDim = srcImage.getExtents()[0];
        yDim = srcImage.getExtents()[1];

        try {
            length = xDim * yDim;
            imgBuffer = new float[length];
            srcImage.exportData(0, length, imgBuffer); // locks and releases lock
        } catch (IOException error) {
            cleanup();
            displayError("AlgorithmAdaptiveSmooth: Image(s) locked");
            setCompleted(false);

            return;
        } catch (OutOfMemoryError e) {
            cleanup();
            displayError("AlgorithmAdaptiveSmooth:  Out of Memory");
            setCompleted(false);

            return;
        }


        fireProgressStateChanged(0, srcImage.getImageName(), "Performing median filter ...");


        process(imgBuffer);

        if (threadStopped) {
            cleanup();
            finalize();

            return;
        }

        if (destImage != null) {

            try {
                destImage.importData(0, imgBuffer, true);
            } catch (IOException error) {
                cleanup();
                displayError("AlgorithmAdaptiveSmooth: destImage locked " + error);
                setCompleted(false);
                return;
            }
        } else {

            try {
                srcImage.importData(0, imgBuffer, true);
            } catch (IOException error) {
                cleanup();
                displayError("AlgorithmAdaptiveSmooth: srcImage locked " + error);
                setCompleted(false);

                return;
            }
        }

        fireProgressStateChanged(100, srcImage.getImageName(), "Performing median filter ...");

        if (threadStopped) {
            cleanup();
            finalize();

            return;
        }

        cleanup();
        setCompleted(true);
    }


    /**
     * Sets structures to null.
     */
    private void cleanup() {
        data = null;
        histX = null;
        histY = null;
        histOrigin = null;
        histDistance = null;
        filter = null;
        seen = null;

        if (edgeGraph != null) {

            for (int i = 0; i < edgeGraph.length; i++) {
                edgeGraph[i] = null;
            }

            edgeGraph = null;
        }

        distMap = null;
        System.gc();
    }

    /**
     * Create the pixel edge graph as an array of arrays, with the first array having a length equal to the number of
     * the pixels in the image and the second array having 8 integers. The 8 values are the edge weights = absolute
     * differences between neighboring pixel values. Used for black and white images.
     *
     * @param  Y  DOCUMENT ME!
     */
    private void createEdgeGraph(float[] Y) {
        int i, j, i1;
        int x, y;
        int gLength = width * height;
        edgeGraphBW = new float[gLength][8];

        for (i = 0; i < gLength; i++) {

            for (j = 0; j < 8; j++) {
                edgeGraphBW[i][j] = 0;
            }
        }

        for (y = 0; y < height; y++) {

            for (x = 0; x < width; x++) {
                i = x + (y * width);

                float startY = Y[i];

                if (x > 0) {
                    edgeGraphBW[i][0] = Math.abs(startY - Y[i - 1]);
                }

                if (y > 0) {
                    i1 = i - width;
                    edgeGraphBW[i][1] = Math.abs(startY - Y[i1]);
                }

                if (x < (width - 1)) {
                    i1 = i + 1;
                    edgeGraphBW[i][2] = Math.abs(startY - Y[i1]);
                }

                if (y < (height - 1)) {
                    i1 = i + width;
                    edgeGraphBW[i][3] = Math.abs(startY - Y[i1]);
                }

                if ((x > 0) && (y > 0)) {
                    i1 = i - width - 1;
                    edgeGraphBW[i][4] = Math.abs(startY - Y[i1]);
                }

                if ((x > 0) && (y < (height - 1))) {
                    i1 = i - 1 + width;
                    edgeGraphBW[i][5] = Math.abs(startY - Y[i1]);
                }

                if ((x < (width - 1)) && (y < (height - 1))) {
                    i1 = i + 1 + width;
                    edgeGraphBW[i][6] = Math.abs(startY - Y[i1]);
                }

                if ((x < (width - 1)) && (y > 0)) {
                    i1 = i + 1 - width;
                    edgeGraphBW[i][7] = Math.abs(startY - Y[i1]);
                }

            }
        }

    }

    /**
     * Create the pixel edge graph as an array of arrays, with the first array having a length equal to the number of
     * the pixels in the image and the second array having 8 integers. The 8 values are the edge weights calculated from
     * the difference between the center pixel and one of the 8 neighboring pixels. edge weight = sqrt((dy*wY)**2 +
     * (dr*wR)**2 + (dB*wB)**2), where dy, dR, and dB are the Y, Cr, and Cb differences and wY, wR, and wB are the Y,
     * Cr, and Cb weights derived from the Y, Cr, and Cb radiuses. A bigger relative radius results in a smaller weight
     * for radiuses >= 0.1, which should typically be the case. Used for color images.
     *
     * @param  Y   DOCUMENT ME!
     * @param  Cr  DOCUMENT ME!
     * @param  Cb  DOCUMENT ME!
     */
    private void createEdgeGraph(int[] Y, int[] Cr, int[] Cb) {
        int i, j, i1;
        int x, y;
        int gLength = width * height;
        edgeGraph = new int[gLength][8];

        for (i = 0; i < gLength; i++) {

            for (j = 0; j < 8; j++) {
                edgeGraph[i][j] = 0;
            }
        }

        if (radiusY >= 0.1) {
            wY = 1.0 / (radiusY * radiusY);
        } else {
            wY = 1.0;
        }

        if (radiusCr >= 0.1) {
            wR = 1.0 / (radiusCr * radiusCr);
        } else {
            wR = 0.0;
        }

        if (radiusCb >= 0.1) {
            wB = 1.0 / (radiusCb * radiusCb);
        } else {
            wB = 0.0;
        }

        invWeight = 1.0 / Math.sqrt((wY * wY) + (wR * wR) + (wB * wB));
        wY *= invWeight;
        wR *= invWeight;
        wB *= invWeight;

        for (y = 0; y < height; y++) {

            for (x = 0; x < width; x++) {
                i = x + (y * width);

                int startY = Y[i];
                int startR = Cr[i];
                int startB = Cb[i];

                if (x > 0) {
                    edgeGraph[i][0] = getEdgeWeight(startY - Y[i - 1], startR - Cr[i - 1], startB - Cb[i - 1]);
                }

                if (y > 0) {
                    i1 = i - width;
                    edgeGraph[i][1] = getEdgeWeight(startY - Y[i1], startR - Cr[i1], startB - Cb[i1]);
                }

                if (x < (width - 1)) {
                    i1 = i + 1;
                    edgeGraph[i][2] = getEdgeWeight(startY - Y[i1], startR - Cr[i1], startB - Cb[i1]);
                }

                if (y < (height - 1)) {
                    i1 = i + width;
                    edgeGraph[i][3] = getEdgeWeight(startY - Y[i1], startR - Cr[i1], startB - Cb[i1]);
                }

                if ((x > 0) && (y > 0)) {
                    i1 = i - width - 1;
                    edgeGraph[i][4] = getEdgeWeight(startY - Y[i1], startR - Cr[i1], startB - Cb[i1]);
                }

                if ((x > 0) && (y < (height - 1))) {
                    i1 = i - 1 + width;
                    edgeGraph[i][5] = getEdgeWeight(startY - Y[i1], startR - Cr[i1], startB - Cb[i1]);
                }

                if ((x < (width - 1)) && (y < (height - 1))) {
                    i1 = i + 1 + width;
                    edgeGraph[i][6] = getEdgeWeight(startY - Y[i1], startR - Cr[i1], startB - Cb[i1]);
                }

                if ((x < (width - 1)) && (y > 0)) {
                    i1 = i + 1 - width;
                    edgeGraph[i][7] = getEdgeWeight(startY - Y[i1], startR - Cr[i1], startB - Cb[i1]);
                }

            }
        }

    }

    /**
     * This method returns the filtered coordinate value at coordinate (x,y) for an ARGB image. If neighboring pixels
     * are connected to the center pixel via a low cost path, queue elements corresponding to the neighboring pixel are
     * added to the priority queue. If no queue element is retrieved from the queue, then the original value is passed
     * unchanged. If elements are retrieved from the queue, then those Y, Cr, or Cb pixel values which had retrieved
     * queue elements in the path from the low cost edge pixel to the center pixel are put into a weighted sum. The
     * weight is derived from an index into the filter array given by the queue element key times the invFilterLen.
     *
     * @param   x  DOCUMENT ME!
     * @param   y  DOCUMENT ME!
     *
     * @return  result
     */
    private int enhancePixel(int x, int y) {
        int i, j;
        int count;
        int result;
        double sum;
        double divisor;
        double dist;
        double weight;
        boolean foundLowEdgeCost;
        int lowestEdgeCount = 0;
        float lowestKey;
        int minX, minY;
        int locX, locY;
        boolean originFound;
        int r2 = (2 * range) + 1;
        int originX, originY;
        boolean nextFound;

        count = search(x, y);

        if (count == 0) {
            result = data[x + (y * width)];
        } else {
            foundLowEdgeCost = false;
            minX = Math.max(x - range, 0);
            minY = Math.max(y - range, 0);
            lowestKey = Float.MAX_VALUE;

            for (i = range; (i >= 1) && (!foundLowEdgeCost); i--) {

                for (j = 0; j < count; j++) {
                    locX = histX[j] - minX;
                    locY = histY[j] - minY;

                    if ((locX == (i - range)) || (locX == (i + range)) || (locY == (i - range)) ||
                            (locY == (i + range))) {
                        foundLowEdgeCost = true;

                        if (histDistance[j] < lowestKey) {
                            lowestKey = histDistance[j];
                            lowestEdgeCount = j;
                        }
                    }
                }
            }

            sum = 0.0;
            divisor = 0.0;
            i = lowestEdgeCount;
            originFound = false;

            while (!originFound) {
                dist = histDistance[i];
                weight = filter[(int) (dist * invFilterLen)];
                sum += data[histX[i] + (histY[i] * width)] * weight;
                divisor += weight;

                if (histOrigin[i] == -1) {
                    originFound = true;
                } else {
                    originX = (histOrigin[i] % r2) + minX;
                    originY = (histOrigin[i] / r2) + minY;
                    nextFound = false;

                    for (j = 0; (j < count) && (!nextFound); j++) {

                        if ((histX[j] == originX) && (histY[j] == originY)) {
                            nextFound = true;
                            i = j;
                        }
                    }
                }
            }

            result = (int) ((sum / divisor) + 0.5);
        }

        return result;
    }

    /**
     * This method returns the filtered coordinate value at coordinate (x,y) for a black and white image. If neighboring
     * pixels are connected to the center pixel via a low cost path, queue elements corresponding to the neighboring
     * pixels are added to the priority queue. If no queue element is retrieved from the queue, then the original value
     * is passed unchanged. If elements are retrieved from the queue, then those pixel values which had retrieved queue
     * elements in the path from the low cost edge pixel to the center pixel are put into a weighted sum. The weight is
     * derived from an index into the filter array given by the queue key times the invFilterLen. The filter array is
     * generated with a Gaussian function.
     *
     * @param   x  DOCUMENT ME!
     * @param   y  DOCUMENT ME!
     *
     * @return  result
     */
    private float enhancePixelBW(int x, int y) {
        int i, j;
        int count;
        float result;
        double sum;
        double divisor;
        double dist;
        double weight;
        boolean foundLowEdgeCost;
        int lowestEdgeCount = 0;
        float lowestKey;
        int minX, minY;
        int locX, locY;
        boolean originFound;
        int r2 = (2 * range) + 1;
        int originX, originY;
        boolean nextFound;

        count = searchBW(x, y);

        if (count == 0) {
            result = dataBW[x + (y * width)];
        } else {
            foundLowEdgeCost = false;
            minX = Math.max(x - range, 0);
            minY = Math.max(y - range, 0);
            lowestKey = Float.MAX_VALUE;

            for (i = range; (i >= 1) && (!foundLowEdgeCost); i--) {

                for (j = 0; j < count; j++) {
                    locX = histX[j] - minX;
                    locY = histY[j] - minY;

                    if ((locX == (i - range)) || (locX == (i + range)) || (locY == (i - range)) ||
                            (locY == (i + range))) {
                        foundLowEdgeCost = true;

                        if (histDistance[j] < lowestKey) {
                            lowestKey = histDistance[j];
                            lowestEdgeCount = j;
                        }
                    }
                }
            }

            sum = 0.0;
            divisor = 0.0;
            i = lowestEdgeCount;
            originFound = false;

            while (!originFound) {
                dist = histDistance[i];
                weight = filter[(int) (dist * invFilterLen)];
                sum += dataBW[histX[i] + (histY[i] * width)] * weight;
                divisor += weight;

                if (histOrigin[i] == -1) {
                    originFound = true;
                } else {
                    originX = (histOrigin[i] % r2) + minX;
                    originY = (histOrigin[i] / r2) + minY;
                    nextFound = false;

                    for (j = 0; (j < count) && (!nextFound); j++) {

                        if ((histX[j] == originX) && (histY[j] == originY)) {
                            nextFound = true;
                            i = j;
                        }
                    }
                }
            }

            result = (float) (sum / divisor);
        }

        return result;
    }

    /**
     * This method performs the filtering on ARGB images.
     */
    private void filterProcess() {
        int i, k;
        int filterLen;
        float len1;
        int r2;
        int[] data1;
        int dataLength;

        if (radius < 0.1) {
            return;
        }

        maxDistance = (int) (radius * (distWeight + 512));
        range = (int) (radius + 0.999f);
        maxLength = ((2 * range) + 1) * ((2 * range) + 1);

        try {
            histX = new int[maxLength];
            histY = new int[maxLength];
            histOrigin = new int[maxLength];
            histDistance = new float[maxLength];
            seen = new char[maxLength];
            Q = new PriorityQueue(maxLength);
        } catch (OutOfMemoryError e) {
            displayError("AlgorithmAdaptiveSmooth: Out of memory");
            setCompleted(false);
            

            return;
        }

        // Precalculate the filter table
        filterLen = range * 16;
        len1 = radius * 16;
        filter = new double[filterLen + 1];

        for (i = 0; i < (filterLen + 1); i++) {
            filter[i] = Math.exp((-3.0 * i * i) / (len1 * len1));
        }

        invFilterLen = ((double) filterLen) / maxDistance;

        // Precalculate the distance function table
        distMap = new float[maxLength];
        r2 = (range * 2) + 1;

        for (i = -range; i <= range; i++) {

            for (k = -range; k <= range; k++) {
                distMap[(i + range) + ((k + range) * r2)] = (float) (distWeight * Math.sqrt((i * i) + (k * k)));
            }
        }

        // Apply the filter to the whole image
        dataLength = width * height;

        try {
            data1 = new int[dataLength];
        } catch (OutOfMemoryError e) {
            displayError("AlgorithmAdaptiveSmooth: Out of memory creating data1 " + e);
            setCompleted(false);
            

            return;
        }

        for (k = 0; k < height; k++) {

            for (i = 0; i < width; i++) {
                data1[(k * width) + i] = enhancePixel(i, k);
            }
        }

        for (i = 0; i < dataLength; i++) {
            data[i] = data1[i];
        }

        data1 = null;
        histX = null;
        histY = null;
        histOrigin = null;
        histDistance = null;
        seen = null;
        Q.queue = null;
        Q.places = null;
        filter = null;
        distMap = null;
    }

    /**
     * This method performs the filtering on black and white images.
     */
    private void filterProcessBW() {
        int i, k;
        int filterLen;
        float len1;
        int r2;
        float[] data1;
        int dataLength;

        if (radius < 0.1) {
            return;
        }

        maxDistance = (int) (radius * (distWeight + 512));
        range = (int) (radius + 0.999f);
        maxLength = ((2 * range) + 1) * ((2 * range) + 1);

        try {
            histX = new int[maxLength];
            histY = new int[maxLength];
            histOrigin = new int[maxLength];
            histDistance = new float[maxLength];
            seen = new char[maxLength];
            Q = new PriorityQueue(maxLength);
        } catch (OutOfMemoryError e) {
            displayError("AlgorithmAdaptiveSmooth: Out of memory");
            setCompleted(false);
            

            return;
        }

        // Precalculate the filter table
        filterLen = range * 16;
        len1 = radius * 16;
        filter = new double[filterLen + 1];

        for (i = 0; i < (filterLen + 1); i++) {
            filter[i] = Math.exp((-3.0 * i * i) / (len1 * len1));
        }

        invFilterLen = ((double) filterLen) / maxDistance;

        // Precalculate the distance function table
        distMap = new float[maxLength];
        r2 = (range * 2) + 1;

        for (i = -range; i <= range; i++) {

            for (k = -range; k <= range; k++) {
                distMap[(i + range) + ((k + range) * r2)] = (float) (distWeight * Math.sqrt((i * i) + (k * k)));
            }
        }

        // Apply the filter to the whole image
        dataLength = width * height;

        try {
            data1 = new float[dataLength];
        } catch (OutOfMemoryError e) {
            displayError("AlgorithmAdaptiveSmooth: Out of memory creating data1 " + e);
            setCompleted(false);
            

            return;
        }

        for (k = 0; k < height; k++) {

            for (i = 0; i < width; i++) {
                data1[(k * width) + i] = enhancePixelBW(i, k);
            }
        }

        for (i = 0; i < dataLength; i++) {
            dataBW[i] = data1[i];
        }

        data1 = null;
        histX = null;
        histY = null;
        histOrigin = null;
        histDistance = null;
        seen = null;
        Q.queue = null;
        Q.places = null;
        filter = null;
        distMap = null;
    }

    /**
     * Returns true if pixel differs from any of its 4 nearest neighbors by thr or if pixel differs from all of its 4
     * nearest neighbors by thr/2. Returns false otherwise. Used in color images.
     *
     * @param   Y        DOCUMENT ME!
     * @param   counter  DOCUMENT ME!
     * @param   delta    DOCUMENT ME!
     *
     * @return  DOCUMENT ME!
     */
    private boolean isEdgeY(int[] Y, int counter, int delta) {
        int thr = delta * 256;
        int c = Y[counter];
        int c1 = Y[counter + 1];
        int c2 = Y[counter - 1];
        int c3 = Y[counter + xDim];
        int c4 = Y[counter - xDim];

        if (Math.abs(c - c1) > thr) {
            return true;
        }

        if (Math.abs(c - c2) > thr) {
            return true;
        }

        if (Math.abs(c - c3) > thr) {
            return true;
        }

        if (Math.abs(c - c4) > thr) {
            return true;
        }

        int maxValue = Math.max(Math.max(Math.max(c1, c2), c3), c4);

        if (c >= (maxValue + (thr / 2))) {
            return true;
        }

        int minValue = Math.min(Math.min(Math.min(c1, c2), c3), c4);

        if (c <= (minValue - (thr / 2))) {
            return true;
        }

        return false;
    }

    /**
     * Returns true if pixel differs from any of its 4 nearest neighbors by thr or if pixel differs from all of its 4
     * nearest neighbors by thr/2. Returns false otherwise. Used in black and white images.
     *
     * @param   Y        DOCUMENT ME!
     * @param   counter  DOCUMENT ME!
     * @param   thr      DOCUMENT ME!
     *
     * @return  DOCUMENT ME!
     */
    private boolean isEdgeY(float[] Y, int counter, float thr) {
        float c = Y[counter];
        float c1 = Y[counter + 1];
        float c2 = Y[counter - 1];
        float c3 = Y[counter + xDim];
        float c4 = Y[counter - xDim];

        if (Math.abs(c - c1) > thr) {
            return true;
        }

        if (Math.abs(c - c2) > thr) {
            return true;
        }

        if (Math.abs(c - c3) > thr) {
            return true;
        }

        if (Math.abs(c - c4) > thr) {
            return true;
        }

        float maxValue = Math.max(Math.max(Math.max(c1, c2), c3), c4);

        if (c >= (maxValue + (thr / 2))) {
            return true;
        }

        float minValue = Math.min(Math.min(Math.min(c1, c2), c3), c4);

        if (c <= (minValue - (thr / 2))) {
            return true;
        }

        return false;
    }


    /**
     * Finds the median value of the list. Median assumes the list of values starts at index 1, not an index of 0. (ie.,
     * 1st element is not included.)
     *
     * @param   list  List of numbers
     *
     * @return  The median.
     *
     * @author  parsonsd
     */
    private int median(int[] list) {
        int N;
        int med;

        N = list.length - 1;

        if ((N % 2) != 0) {
            med = list[(N / 2) + 1];
        } else {
            med = (list[N / 2] + list[(N / 2) + 1]) / 2;
        }

        return (med);
    }

    /**
     * Finds the median value of the list. Median assumes the list of values starts at index 1, not an index of 0. (ie.,
     * 1st element is not included.)
     *
     * @param   list  List of numbers
     *
     * @return  The median.
     *
     * @author  parsonsd
     */
    private float median(float[] list) {
        int N;
        float med;

        N = list.length - 1;

        if ((N % 2) != 0) {
            med = list[(N / 2) + 1];
        } else {
            med = (list[N / 2] + list[(N / 2) + 1]) / 2;
        }

        return (med);
    }

    /**
     * If on an edge, do not filter. If not on an edge, median filter over the local 3 by 3 square.
     *
     * @param  Y  DOCUMENT ME!
     */
    private void medianFilter(float[] Y) {
        int x, y;
        int i, j;
        int counter;
        float[] Y1 = new float[length];
        float[] ya = new float[9];
        int index;
        int adr;
        float threshold;

        threshold = (float) ((15.0f / 255.0f) * (srcImage.getMax() - srcImage.getMin()));

        for (i = 0; i < length; i++) {
            Y1[i] = Y[i];
        }

        for (y = 1; y < (yDim - 1); y++) {

            for (x = 1; x < (xDim - 1); x++) {
                counter = x + (y * xDim);

                if (isEdgeY(Y1, counter, threshold)) { }
                else {
                    index = 0;

                    for (j = -1; j <= 1; j++) {

                        for (i = -1; i <= 1; i++) {
                            adr = (j * xDim) + i;
                            ya[index] = Y1[counter + adr];
                            index++;
                        }
                    }

                    shell(ya);
                    Y[counter] = median(ya);
                }
            }
        }

    }

    /**
     * If on an edge in Y, Cr, or Cb space, no filtering is performed. If not on an edge, median filter over the 3 by 3
     * local square.
     *
     * @param  Y   DOCUMENT ME!
     * @param  Cr  DOCUMENT ME!
     * @param  Cb  DOCUMENT ME!
     */
    private void medianFilter(int[] Y, int[] Cr, int[] Cb) {
        int x, y;
        int i, j;
        int counter;
        int[] Y1 = new int[length];
        int[] Cr1 = new int[length];
        int[] Cb1 = new int[length];
        int[] ya = new int[9];
        int[] cra = new int[9];
        int[] cba = new int[9];
        int index;
        int adr;

        for (i = 0; i < length; i++) {
            Y1[i] = Y[i];
            Cr1[i] = Cr[i];
            Cb1[i] = Cb[i];
        }

        for (y = 1; y < (yDim - 1); y++) {

            for (x = 1; x < (xDim - 1); x++) {
                counter = x + (y * xDim);

                if (isEdgeY(Y1, counter, deltaY) || isEdgeY(Cr, counter, deltaCr) || isEdgeY(Cb, counter, deltaCb)) { }
                else {
                    index = 0;

                    for (j = -1; j <= 1; j++) {

                        for (i = -1; i <= 1; i++) {
                            adr = (j * xDim) + i;
                            ya[index] = Y1[counter + adr];
                            cra[index] = Cr1[counter + adr];
                            cba[index] = Cb1[counter + adr];
                            index++;
                        }
                    }

                    shell(ya);
                    Y[counter] = median(ya);
                    shell(cra);
                    Cr[counter] = median(cra);
                    shell(cba);
                    Cb[counter] = median(cba);
                }
            }
        }

    }

    /**
     * Uses a median filtered copy of Y space to create a pixel neighbor graph, with the edge weights based on the
     * differences between pixels. Then the Y space is filtered.
     *
     * @param  Y  DOCUMENT ME!
     */
    private void process(float[] Y) {
        int i;
        float[] fY = new float[length];

        for (i = 0; i < length; i++) {
            fY[i] = Y[i];
        }

        dataBW = Y;
        radius = radiusY;
        width = xDim;
        height = yDim;
        
        fireProgressStateChanged((.1f), srcImage.getImageName(), "Performing median filter ...");
        
        
        medianFilter(fY);
        
        fireProgressStateChanged((.3f), srcImage.getImageName(), "Creating edge graph ...");
               
        createEdgeGraph(fY);
        
        fireProgressStateChanged((.5f), srcImage.getImageName(), "Filtering in Y space ...");
                
        filterProcessBW();

        if (threadStopped) {
            cleanup();
            finalize();

            return;
        }

        if (edgeGraph != null) {

            for (i = 0; i < edgeGraph.length; i++) {
                edgeGraph[i] = null;
            }

            edgeGraph = null;
        }

        fY = null;
    }

    /**
     * Uses a median filtered copy of Y, Cr, and Cb spaces to create a pixel neighbor graph, with the edge weights based
     * on the differences between pixels. Then, if reduce is false, the Y, Cr, and Cb spaces are separately filtered. If
     * reduce is true, only the Y space is filtered, the Y, Cr, and Cb spaces are shrunk by a factor of 2, a new pixel
     * neighbor graph is created based on the shrunken spaces, the Cr and Cb spaces are filtered, and finally the Cr and
     * Cb spaces are expanded back to their original sizes.
     *
     * @param  Y   DOCUMENT ME!
     * @param  Cr  DOCUMENT ME!
     * @param  Cb  DOCUMENT ME!
     */
    private void process(int[] Y, int[] Cr, int[] Cb) {
        int i;
        int newWidth;
        int newHeight;
        int[] newY;
        int[] newR;
        int[] newB;
        int[] fY = new int[length];
        int[] fR = new int[length];
        int[] fB = new int[length];

        for (i = 0; i < length; i++) {
            fY[i] = Y[i];
            fR[i] = Cr[i];
            fB[i] = Cb[i];
        }

        data = Y;
        radius = radiusY;
        width = xDim;
        height = yDim;
        fireProgressStateChanged((.1f), srcImage.getImageName(), "Performing median filter ...");
        
        medianFilter(fY, fR, fB);
        fireProgressStateChanged((.3f), srcImage.getImageName(), "Creating edge graph ...");
        
        createEdgeGraph(fY, fR, fB);
        fireProgressStateChanged((.5f), srcImage.getImageName(), "Filtering in Y space ...");
        
        filterProcess();

        if (threadStopped) {
            cleanup();
            finalize();

            return;
        }

        if (!reduce) {
            data = Cb;
            radius = radiusCb;
            fireProgressStateChanged((.7f), srcImage.getImageName(), "Filtering in Cb space ...");
            
            filterProcess();

            if (threadStopped) {
                cleanup();
                finalize();

                return;
            }

            data = Cr;
            radius = radiusCr;
            fireProgressStateChanged((.9f), srcImage.getImageName(), "Filtering in Cr space ...");
            
            filterProcess();

            if (threadStopped) {
                cleanup();
                finalize();

                return;
            }

            if (edgeGraph != null) {

                for (i = 0; i < edgeGraph.length; i++) {
                    edgeGraph[i] = null;
                }

                edgeGraph = null;
            }
        } // if (!reduce)
        else { // reduce

            if (edgeGraph != null) {

                for (i = 0; i < edgeGraph.length; i++) {
                    edgeGraph[i] = null;
                }

                edgeGraph = null;
            }

            newWidth = (xDim + 2) / 2;
            newHeight = (yDim + 2) / 2;

            newY = new int[newWidth * newHeight];
            newR = new int[newWidth * newHeight];
            newB = new int[newWidth * newHeight];

            shrink2X(Y, newY);
            shrink2X(Cr, newR);
            shrink2X(Cb, newB);

            width = newWidth;
            height = newHeight;

            createEdgeGraph(newY, newR, newB);
            data = newB;
            radius = radiusCb / 2.0f;
            fireProgressStateChanged((.7f), srcImage.getImageName(), "Filtering in Cb space ...");
            
            filterProcess();

            if (threadStopped) {
                cleanup();
                finalize();

                return;
            }

            data = newR;
            radius = radiusCr / 2.0f;
            fireProgressStateChanged((.9f), srcImage.getImageName(), "Filtering in Cr space ...");
            
            filterProcess();

            if (threadStopped) {
                cleanup();
                finalize();

                return;
            }

            unShrink2X(Cr, newR);
            unShrink2X(Cb, newB);

            newY = null;
            newR = null;
            newB = null;

            if (edgeGraph != null) {

                for (i = 0; i < edgeGraph.length; i++) {
                    edgeGraph[i] = null;
                }

                edgeGraph = null;
            }
        } // else reduce

        fY = null;
        fR = null;
        fB = null;
    }

    /**
     * Convert from RGB space to YCrCb space.
     *
     * @param  imgBuffer  DOCUMENT ME!
     * @param  Y          DOCUMENT ME!
     * @param  Cr         DOCUMENT ME!
     * @param  Cb         DOCUMENT ME!
     */
    private void rgb2yCrCb(int[] imgBuffer, int[] Y, int[] Cr, int[] Cb) {
        int i;
        int red, green, blue;

        for (i = 0; i < length; i++) {
            red = imgBuffer[(4 * i) + 1];
            green = imgBuffer[(4 * i) + 2];
            blue = imgBuffer[(4 * i) + 3];
            Y[i] = (red * 77) + (green * 151) + (blue * 28);

            // 32768 = 128 * 256
            Cr[i] = (red * 256) - Y[i] + 32768;
            Cb[i] = (blue * 256) - Y[i] + 32768;
        }
    }

    /**
     * This method finds all neighbor pixels within maxDistance from the initial pixel (x,y). For each pixel the
     * shortest path value is also obtained. This method is for ARGB images
     *
     * @param   x  DOCUMENT ME!
     * @param   y  DOCUMENT ME!
     *
     * @return  count
     */
    private int search(int x, int y) {
        int i;
        int r2;
        int minX;
        int maxX;
        int minY;
        int maxY;
        QueueElement elem;
        int count;

        r2 = (range * 2) + 1;

        minX = Math.max(x - range, 0);
        maxX = Math.min(x + range, width - 1);
        minY = Math.max(y - range, 0);
        maxY = Math.min(y + range, height - 1);

        for (i = 0; i < maxLength; i++) {
            seen[i] = 0;
        }

        elem = new QueueElement();
        elem.key = 0;
        elem.baseKey = 0;
        elem.value = x - minX + ((y - minY) * r2);
        elem.origin = -1; // no origin
        Q.addElement(elem);
        count = 0;

        while (Q.notEmpty() && (count < maxLength)) {
            elem = Q.getElement();

            int ind = elem.value;
            int y1 = (ind / r2) + minY;
            int x1 = (ind % r2) + minX;
            float key = elem.baseKey;

            seen[ind] = 2;
            histX[count] = x1;
            histY[count] = y1;
            histOrigin[count] = elem.origin;
            histDistance[count++] = elem.key;

            int dataInd = x1 + (y1 * width);
            // int *graphElement = edgeGraph[dataInd];

            elem.origin = elem.value;

            if ((x1 > minX) && (seen[ind - 1] <= 1)) {
                elem.value = ind - 1;
                elem.baseKey = key + edgeGraph[dataInd][0];
                elem.key = elem.baseKey + distMap[ind - 1];

                if (elem.key < maxDistance) {
                    Q.addElement(elem);
                }
            }

            if ((y1 > minY) && (seen[ind - r2] <= 1)) {
                elem.value = ind - r2;
                elem.baseKey = key + edgeGraph[dataInd][1];
                elem.key = elem.baseKey + distMap[ind - r2];

                if (elem.key < maxDistance) {
                    Q.addElement(elem);
                }
            }

            if ((x1 < maxX) && (seen[ind + 1] <= 1)) {
                elem.value = ind + 1;
                elem.baseKey = key + edgeGraph[dataInd][2];
                elem.key = elem.baseKey + distMap[ind + 1];

                if (elem.key < maxDistance) {
                    Q.addElement(elem);
                }
            }

            if ((y1 < maxY) && (seen[ind + r2] <= 1)) {
                elem.value = ind + r2;
                elem.baseKey = key + edgeGraph[dataInd][3];
                elem.key = elem.baseKey + distMap[ind + r2];

                if (elem.key < maxDistance) {
                    Q.addElement(elem);
                }
            }

            if ((x1 > minX) && (y1 > minY) && (seen[ind - 1 - r2] <= 1)) {
                elem.value = ind - 1 - r2;
                elem.baseKey = key + edgeGraph[dataInd][4];
                elem.key = elem.baseKey + distMap[ind - 1 - r2];

                if (elem.key < maxDistance) {
                    Q.addElement(elem);
                }
            }

            if ((x1 > minX) && (y1 < maxY) && (seen[ind - 1 + r2] <= 1)) {
                elem.value = ind - 1 + r2;
                elem.baseKey = key + edgeGraph[dataInd][5];
                elem.key = elem.baseKey + distMap[ind - 1 + r2];

                if (elem.key < maxDistance) {
                    Q.addElement(elem);
                }
            }

            if ((x1 < maxX) && (y1 < maxY) && (seen[ind + 1 + r2] <= 1)) {
                elem.value = ind + 1 + r2;
                elem.baseKey = key + edgeGraph[dataInd][6];
                elem.key = elem.baseKey + distMap[ind + 1 + r2];

                if (elem.key < maxDistance) {
                    Q.addElement(elem);
                }
            }

            if ((x1 < maxX) && (y1 > minY) && (seen[ind + 1 - r2] <= 1)) {
                elem.value = ind + 1 - r2;
                elem.baseKey = key + edgeGraph[dataInd][7];
                elem.key = elem.baseKey + distMap[ind + 1 - r2];

                if (elem.key < maxDistance) {
                    Q.addElement(elem);
                }
            }

        }

        return count;
    }

    /**
     * This method finds all neighbor pixels within maxDistance from the initial pixel (x,y). For each pixel the
     * shortest path value is also obtained. This image is for black and white images
     *
     * @param   x  DOCUMENT ME!
     * @param   y  DOCUMENT ME!
     *
     * @return  count
     */
    private int searchBW(int x, int y) {
        int i;
        int r2;
        int minX;
        int maxX;
        int minY;
        int maxY;
        QueueElement elem;
        int count;

        r2 = (range * 2) + 1;

        minX = Math.max(x - range, 0);
        maxX = Math.min(x + range, width - 1);
        minY = Math.max(y - range, 0);
        maxY = Math.min(y + range, height - 1);

        for (i = 0; i < maxLength; i++) {
            seen[i] = 0;
        }

        elem = new QueueElement();
        elem.key = 0;
        elem.baseKey = 0;
        elem.value = x - minX + ((y - minY) * r2);
        elem.origin = -1; // no origin
        Q.addElement(elem);
        count = 0;

        while (Q.notEmpty() && (count < maxLength)) {
            elem = Q.getElement();

            int ind = elem.value;
            int y1 = (ind / r2) + minY;
            int x1 = (ind % r2) + minX;
            float key = elem.baseKey;

            seen[ind] = 2;
            histX[count] = x1;
            histY[count] = y1;
            histOrigin[count] = elem.origin;
            histDistance[count++] = elem.key;

            int dataInd = x1 + (y1 * width);
            // int *graphElement = edgeGraph[dataInd];

            elem.origin = elem.value;

            if ((x1 > minX) && (seen[ind - 1] <= 1)) {
                elem.value = ind - 1;
                elem.baseKey = key + edgeGraphBW[dataInd][0];
                elem.key = elem.baseKey + distMap[ind - 1];

                if (elem.key < maxDistance) {
                    Q.addElement(elem);
                }
            }

            if ((y1 > minY) && (seen[ind - r2] <= 1)) {
                elem.value = ind - r2;
                elem.baseKey = key + edgeGraphBW[dataInd][1];
                elem.key = elem.baseKey + distMap[ind - r2];

                if (elem.key < maxDistance) {
                    Q.addElement(elem);
                }
            }

            if ((x1 < maxX) && (seen[ind + 1] <= 1)) {
                elem.value = ind + 1;
                elem.baseKey = key + edgeGraphBW[dataInd][2];
                elem.key = elem.baseKey + distMap[ind + 1];

                if (elem.key < maxDistance) {
                    Q.addElement(elem);
                }
            }

            if ((y1 < maxY) && (seen[ind + r2] <= 1)) {
                elem.value = ind + r2;
                elem.baseKey = key + edgeGraphBW[dataInd][3];
                elem.key = elem.baseKey + distMap[ind + r2];

                if (elem.key < maxDistance) {
                    Q.addElement(elem);
                }
            }

            if ((x1 > minX) && (y1 > minY) && (seen[ind - 1 - r2] <= 1)) {
                elem.value = ind - 1 - r2;
                elem.baseKey = key + edgeGraphBW[dataInd][4];
                elem.key = elem.baseKey + distMap[ind - 1 - r2];

                if (elem.key < maxDistance) {
                    Q.addElement(elem);
                }
            }

            if ((x1 > minX) && (y1 < maxY) && (seen[ind - 1 + r2] <= 1)) {
                elem.value = ind - 1 + r2;
                elem.baseKey = key + edgeGraphBW[dataInd][5];
                elem.key = elem.baseKey + distMap[ind - 1 + r2];

                if (elem.key < maxDistance) {
                    Q.addElement(elem);
                }
            }

            if ((x1 < maxX) && (y1 < maxY) && (seen[ind + 1 + r2] <= 1)) {
                elem.value = ind + 1 + r2;
                elem.baseKey = key + edgeGraphBW[dataInd][6];
                elem.key = elem.baseKey + distMap[ind + 1 + r2];

                if (elem.key < maxDistance) {
                    Q.addElement(elem);
                }
            }

            if ((x1 < maxX) && (y1 > minY) && (seen[ind + 1 - r2] <= 1)) {
                elem.value = ind + 1 - r2;
                elem.baseKey = key + edgeGraphBW[dataInd][7];
                elem.key = elem.baseKey + distMap[ind + 1 - r2];

                if (elem.key < maxDistance) {
                    Q.addElement(elem);
                }
            }

        }

        return count;
    }

    /**
     * DOCUMENT ME!
     *
     * @param  buffer  input buffer to be sorted Sort an array into ascending numerical order by Shell's method
     *                 Reference: Numerical Recipes in C The Art of Scientific Computing Second Edition by William H.
     *                 Press, Saul A. Teukolsky, William T. Vetterling, Brian P. Flannery, pp. 331- 332.
     */
    private void shell(float[] buffer) {
        int i, j, inc;
        float v;
        inc = 1;

        do {
            inc *= 3;
            inc++;
        } while (inc <= buffer.length);

        do {
            inc /= 3;

            for (i = inc + 1; i <= buffer.length; i++) {
                v = buffer[i - 1];
                j = i;

                while (buffer[j - inc - 1] > v) {
                    buffer[j - 1] = buffer[j - inc - 1];
                    j -= inc;

                    if (j <= inc) {
                        break;
                    }
                }

                buffer[j - 1] = v;
            }
        } while (inc > 1);
    }

    /**
     * DOCUMENT ME!
     *
     * @param  buffer  input buffer to be sorted Sort an array into ascending numerical order by Shell's method
     *                 Reference: Numerical Recipes in C The Art of Scientific Computing Second Edition by William H.
     *                 Press, Saul A. Teukolsky, William T. Vetterling, Brian P. Flannery, pp. 331- 332.
     */
    private void shell(int[] buffer) {
        int i, j, inc;
        int v;
        inc = 1;

        do {
            inc *= 3;
            inc++;
        } while (inc <= buffer.length);

        do {
            inc /= 3;

            for (i = inc + 1; i <= buffer.length; i++) {
                v = buffer[i - 1];
                j = i;

                while (buffer[j - inc - 1] > v) {
                    buffer[j - 1] = buffer[j - inc - 1];
                    j -= inc;

                    if (j <= inc) {
                        break;
                    }
                }

                buffer[j - 1] = v;
            }
        } while (inc > 1);
    }


    /**
     * Shrink a space down by a factor of 2.
     *
     * @param  inData   DOCUMENT ME!
     * @param  outData  DOCUMENT ME!
     */
    private void shrink2X(int[] inData, int[] outData) {
        int newW;
        int inP = 0;
        int outP = 0;

        newW = (xDim + 2) / 2;
        shrinkLine1(inData, inP, outData, outP);

        inP += xDim;
        outP += newW;

        for (int i = 0; i < ((yDim - 1) / 2); i++) {
            shrinkLine2(inData, inP, outData, outP);
            inP += 2 * xDim;
            outP += newW;
        }

        if ((yDim & 1) == 0) {
            shrinkLine1(inData, inP, outData, outP);
        }
    }

    /**
     * Always used to shrink the first line by a factor of 2 and if yDim is even used to shrink the last line by a
     * factor of 2.
     *
     * @param  inData   DOCUMENT ME!
     * @param  inP      DOCUMENT ME!
     * @param  outData  DOCUMENT ME!
     * @param  outP     DOCUMENT ME!
     */
    private void shrinkLine1(int[] inData, int inP, int[] outData, int outP) {
        outData[outP++] = inData[inP++];

        for (int i = 0; i < ((xDim - 1) / 2); i++) {
            outData[outP++] = (inData[inP] + inData[inP + 1]) >> 1;
            inP += 2;
        }

        if ((xDim & 1) == 0) {
            outData[outP] = inData[inP];
        }
    }

    /**
     * Used to shrink all lines but the beginning line for all spaces and the end line for spaces with even yDim.
     *
     * @param  inData   DOCUMENT ME!
     * @param  inP      DOCUMENT ME!
     * @param  outData  DOCUMENT ME!
     * @param  outP     DOCUMENT ME!
     */
    private void shrinkLine2(int[] inData, int inP, int[] outData, int outP) {
        outData[outP++] = (inData[inP] + inData[inP + xDim]) / 2;
        inP++;

        for (int i = 0; i < ((xDim - 1) / 2); i++) {
            outData[outP++] = (inData[inP] + inData[inP + 1] + inData[inP + xDim] + inData[inP + xDim + 1]) >> 2;
            inP += 2;
        }

        if ((xDim & 1) == 0) {
            outData[outP] = (inData[inP] + inData[inP + xDim]) / 2;
        }
    }

    /**
     * Expands a space that has been shrunk by a factor of 2 back to its original size.
     *
     * @param  outData  DOCUMENT ME!
     * @param  inData   DOCUMENT ME!
     */
    private void unShrink2X(int[] outData, int[] inData) {
        int newW = (xDim + 2) / 2;
        int inP = 0;
        int outP = 0;

        unShrinkLine1(inData, inP, outData, outP);

        inP += newW;
        outP += xDim;

        int newH = (yDim - 1) / 2;

        if ((yDim & 1) != 0) {
            newH--;
        }

        for (int i = 0; i < newH; i++) {
            unShrinkLine2(inData, inP, outData, outP);
            outP += 2 * xDim;
            inP += newW;
        }

        if ((yDim & 1) != 0) {
            unShrinkLine2Last(inData, inP, outData, outP);
        } else {
            unShrinkLine1(inData, inP, outData, outP);
        }
    }

    /**
     * Used to expand the first line in all shrunken spaces and to expand the last line in shrunken spaces when yDim is
     * even.
     *
     * @param  inData   DOCUMENT ME!
     * @param  inP      DOCUMENT ME!
     * @param  outData  DOCUMENT ME!
     * @param  outP     DOCUMENT ME!
     */
    private void unShrinkLine1(int[] inData, int inP, int[] outData, int outP) {
        outData[outP++] = inData[inP++];

        for (int i = 0; i < ((xDim - 1) / 2); i++) {
            outData[outP] = outData[outP + 1] = inData[inP++];
            outP += 2;
        }

        if ((xDim & 1) == 0) {
            outData[outP] = inData[inP];
        }
    }

    /**
     * Used to expand all but the beginning and ending lines in shrunken spaces.
     *
     * @param  inData   DOCUMENT ME!
     * @param  inP      DOCUMENT ME!
     * @param  outData  DOCUMENT ME!
     * @param  outP     DOCUMENT ME!
     */
    private void unShrinkLine2(int[] inData, int inP, int[] outData, int outP) {
        outData[outP] = outData[outP + xDim] = inData[inP++];
        outP++;

        int newW = (xDim + 2) / 2;
        int limit = (xDim - 1) / 2;

        if ((xDim & 1) != 0) {
            limit--;
        }

        for (int i = 0; i < limit; i++) {
            outData[outP] = ((inData[inP] * 9) + ((inData[inP - 1] + inData[inP - newW]) * 3) +
                             inData[inP - newW - 1]) >> 4;
            outData[outP + 1] = ((inData[inP] * 9) + ((inData[inP + 1] + inData[inP - newW]) * 3) +
                                 inData[inP - newW + 1]) >> 4;
            outData[outP + xDim] = ((inData[inP] * 9) + ((inData[inP - 1] + inData[inP + newW]) * 3) +
                                    inData[inP + newW - 1]) >> 4;
            outData[outP + xDim + 1] = ((inData[inP] * 9) + ((inData[inP + 1] + inData[inP + newW]) * 3) +
                                        inData[inP + newW + 1]) >> 4;

            // outData[count+1] = outData[count] = outData[0] = outData[1] = *indata++;
            inP++;
            outP += 2;
        }

        if ((xDim & 1) != 0) {
            outData[outP] = ((inData[inP] * 9) + ((inData[inP - 1] + inData[inP - newW]) * 3) +
                             inData[inP - newW - 1]) >> 4;
            outData[outP + 1] = ((inData[inP] * 3) + inData[inP - newW]) >> 2;
            outData[outP + xDim] = ((inData[inP] * 9) + ((inData[inP - 1] + inData[inP + newW]) * 3) +
                                    inData[inP + newW - 1]) >> 4;
            outData[outP + xDim + 1] = ((inData[inP] * 3) + inData[inP + newW]) >> 2;
            inP++;
            outP += 2;
        }

        if ((xDim & 1) == 0) {
            outData[outP] = outData[outP + xDim] = inData[inP];
        }
    }

    /**
     * If yDim is odd, used to expand the last line in shrunken space into 2 lines in expanded space.
     *
     * @param  inData   DOCUMENT ME!
     * @param  inP      DOCUMENT ME!
     * @param  outData  DOCUMENT ME!
     * @param  outP     DOCUMENT ME!
     */
    private void unShrinkLine2Last(int[] inData, int inP, int[] outData, int outP) {
        outData[outP] = outData[outP + xDim] = inData[inP++];
        outP++;

        int newW = (xDim + 2) / 2;

        int limit = (xDim - 1) / 2;

        if ((xDim & 1) != 0) {
            limit--;
        }

        for (int i = 0; i < limit; i++) {
            outData[outP] = ((inData[inP] * 9) + ((inData[inP - 1] + inData[inP - newW]) * 3) +
                             inData[inP - newW - 1]) >> 4;
            outData[outP + 1] = ((inData[inP] * 9) + ((inData[inP + 1] + inData[inP - newW]) * 3) +
                                 inData[inP - newW + 1]) >> 4;
            outData[outP + xDim] = ((inData[inP] * 3) + inData[inP - 1]) >> 2;
            outData[outP + xDim + 1] = ((inData[inP] * 3) + inData[inP + 1]) >> 2;
            inP++;
            outP += 2;
        }

        if ((xDim & 1) != 0) {
            outData[outP] = ((inData[inP] * 9) + ((inData[inP - 1] + inData[inP - newW]) * 3) +
                             inData[inP - newW - 1]) >> 4;
            outData[outP + 1] = ((inData[inP] * 3) + inData[inP - newW]) >> 2;
            outData[outP + xDim] = ((inData[inP] * 3) + inData[inP - 1]) >> 2;
            outData[outP + xDim + 1] = inData[inP];
            inP++;
            outP += 2;
        } else {
            outData[outP] = outData[outP + xDim] = inData[inP];
        }
    }

    /**
     * Convert from YCrCb space to RGB space.
     *
     * @param  imgBuffer  DOCUMENT ME!
     * @param  Y          DOCUMENT ME!
     * @param  Cr         DOCUMENT ME!
     * @param  Cb         DOCUMENT ME!
     */
    private void yCrCb2rgb(int[] imgBuffer, int[] Y, int[] Cr, int[] Cb) {
        int i;
        int crm, cbm;
        int red, green, blue;

        for (i = 0; i < length; i++) {
            crm = Cr[i] - 32768;
            cbm = Cb[i] - 32768;
            red = (Y[i] + crm) / 256;
            green = (Y[i] - (((130 * crm) + (48 * cbm)) / 256)) / 256;
            blue = (Y[i] + cbm) / 256;

            if (red < 0) {
                red = 0;
            }

            if (green < 0) {
                green = 0;
            }

            if (blue < 0) {
                blue = 0;
            }

            if (red > 255) {
                red = 255;
            }

            if (green > 255) {
                green = 255;
            }

            if (blue > 255) {
                blue = 255;
            }

            imgBuffer[4 * i] = 255;
            imgBuffer[(4 * i) + 1] = red;
            imgBuffer[(4 * i) + 2] = green;
            imgBuffer[(4 * i) + 3] = blue;
        }
    }

    //~ Inner Classes --------------------------------------------------------------------------------------------------

    /**
     * Implements the priority queue.
     */
    class PriorityQueue {

        /** DOCUMENT ME! */
        int last;

        /** DOCUMENT ME! */
        int maxRange;

        /** DOCUMENT ME! */
        int maxSize;

        /** DOCUMENT ME! */
        int[] places;

        /** DOCUMENT ME! */
        QueueElement[] queue;

        /**
         * Creates a new PriorityQueue object.
         *
         * @param  count  DOCUMENT ME!
         */
        public PriorityQueue(int count) {
            maxSize = count;
            last = 0;
            queue = new QueueElement[count];

            for (int i = 0; i < count; i++) {
                queue[i] = new QueueElement();
            }

            maxRange = count;
            places = new int[maxRange];

            for (int i = 0; i < maxRange; i++) {
                places[i] = -1;
            }
        }

        /**
         * DOCUMENT ME!
         *
         * @param  e  DOCUMENT ME!
         */
        public void addElement(QueueElement e) {
            int p;
            int i, j;
            QueueElement temp = new QueueElement();

            p = places[e.value];

            if ((p >= 0) && (e.key >= queue[p].key)) {

                // aready in with smaller key
                return;
            }

            if (p < 0) {

                if (last >= maxSize) {
                    MipavUtil.displayError("Queue is full");

                    return;
                }

                i = last++;
            } else {
                i = p;
            }

            queue[i].key = e.key;
            queue[i].baseKey = e.baseKey;
            queue[i].value = e.value;
            queue[i].origin = e.origin;

            while ((i > 0) && (queue[i].key < queue[j = ((i - 1) >> 1)].key)) {

                // push x up the tree by exchanging it with its parent
                temp.key = queue[i].key;
                temp.baseKey = queue[i].baseKey;
                temp.value = queue[i].value;
                temp.origin = queue[i].origin;
                queue[i].key = queue[j].key;
                queue[i].baseKey = queue[j].baseKey;
                queue[i].value = queue[j].value;
                queue[i].origin = queue[j].origin;
                queue[j].key = temp.key;
                queue[j].baseKey = temp.baseKey;
                queue[j].value = temp.value;
                queue[j].origin = temp.origin;
                places[queue[i].value] = i;

                // update location of current element
                i = j;
            }

            places[queue[i].value] = i;
        }

        /**
         * DOCUMENT ME!
         *
         * @return  DOCUMENT ME!
         */
        public QueueElement getElement() {
            int i, j;
            QueueElement e = new QueueElement();
            QueueElement temp = new QueueElement();

            if (last == 0) {
                MipavUtil.displayError("Priority queue is empty");

                return null;
            }

            e.key = queue[0].key;
            e.baseKey = queue[0].baseKey;
            e.value = queue[0].value;
            e.origin = queue[0].origin;
            places[e.value] = -1;
            --last;

            if (last == 0) {
                return e;
            }

            queue[0].key = queue[last].key;
            queue[0].baseKey = queue[last].baseKey;
            queue[0].value = queue[last].value;
            queue[0].origin = queue[last].origin;
            i = 0;

            while ((j = (2 * i) + 2) <= last) {

                if ((queue[j - 1].key < queue[j].key) || (j == last)) { // only has one child
                    j--; // left child
                }

                if (queue[i].key > queue[j].key) {

                    // swap parent and child
                    temp.key = queue[i].key;
                    temp.baseKey = queue[i].baseKey;
                    temp.value = queue[i].value;
                    temp.origin = queue[i].origin;
                    queue[i].key = queue[j].key;
                    queue[i].baseKey = queue[j].baseKey;
                    queue[i].value = queue[j].value;
                    queue[i].origin = queue[j].origin;
                    queue[j].key = temp.key;
                    queue[j].baseKey = temp.baseKey;
                    queue[j].value = temp.value;
                    queue[j].origin = temp.origin;
                    places[queue[i].value] = i;
                    i = j;
                } else {
                    break; // don't push further down
                }
            }

            // pushed all the way to leaf node
            places[queue[i].value] = i;

            return e;
        }

        /**
         * DOCUMENT ME!
         *
         * @return  DOCUMENT ME!
         */
        public boolean notEmpty() {

            if (last > 0) {
                return true;
            } else {
                return false;
            }
        }
    }

    /**
     * Implements a queue element.
     */
    class QueueElement {

        /** DOCUMENT ME! */
        float baseKey;

        /** DOCUMENT ME! */
        float key;

        /** DOCUMENT ME! */
        int origin;

        /** DOCUMENT ME! */
        int value;

        /**
         * Creates a new QueueElement object.
         */
        public QueueElement() { }
    }


}
