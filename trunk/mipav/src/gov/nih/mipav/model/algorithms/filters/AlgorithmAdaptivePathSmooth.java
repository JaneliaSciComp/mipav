package gov.nih.mipav.model.algorithms.filters;


import gov.nih.mipav.model.algorithms.*;
import gov.nih.mipav.model.structures.*;

import gov.nih.mipav.view.*;

import java.io.*;
import java.util.Arrays;


/**
 * This code is a port of image noise removal filter software by Karlis Freivalds found at
 * http://www.gradetools.com/karlisf with major modifications. In the original program the weighted sum could be applied
 * to all pixels in the (2*range+1)*(2*range+1) local square. In this program the lowest cost pixel at the edge of the
 * (2*range+1)*(2*range+1) local neighborhood is found and that pixel and the pixels along the low cost path back to the
 * original center pixel are the only pixels used in the weighted sum. The low cost path uses nearest 8 neighbors in 2D
 * or nearest 26 neighbors in 3D. The program also has the option of including the pixels neighboring the low cost path
 * if the difference between the neighboring pixel and the low cost pixel is less than threshold. Note that the
 * neigboring pixel is a nearest 4 in 2D or nearest 6 in 3D. Another major modification is that the weighting scheme is
 * much simpler. The weight only depends on the Gaussian function of the distance from the center pixel. The original
 * software only handled 2D, while this software does 2.5D and 3D. The old software left boundary pixels unfiltered, but
 * this is no longer the case. The old software used median filtering in creating the edge graph, but the median
 * filtering has been commented out in this program. At the start of the program RGB color space is transformed in YCrCb
 * space and at the end of the program YCrCb space is transformed into RGB space. Hence, all of the program operations
 * occur in YcrCb space. Frequently Asked Questions About Color by Charles Poynton at
 * http://www.poynton.com/colorFAQ.html reveals that the RGB to YCrCb and YCrCb to RGB conversion equations used in the
 * Freivalds program are significantly different from the standard ones. Black and white images are not transformed into
 * another space. ----------- Description ----------- The filter reduces noise in the image without blurring the edges.
 * The results are much better than can be achieved by using the popular adaptive median or smoothing filters. Like
 * modern compression algorithms (including JPEG) the filter operates in Y,Cr, Cb color space, hence it is useful for
 * removing compression artifacts. ----------------- How does it work? ----------------- It is based on the search of
 * the pixel neighbor graph. The main steps of the algorithm are: - create the pixel neighbor graph, set edge weights
 * based on the differences between the pixels - for the pixel being filtered calculate the shortest path from it to all
 * the other pixels reachable with path length < d. If a neighboring pixel differs in intensity by threshold or more,
 * then the neighboring pixel is not added to the path. If includeNeighbors is true, then the nearest 4 or nearest 6
 * neigbors of path pixels will also be added if they differ from the path pixels by less than threshold. However, the
 * nearest neighbor weight will only be half that of a path pixel. - replace the pixel value with the weighted average
 * of all pixels in the path from the low cost edge pixel to the center pixel with weights taken as the Gaussian
 * function of the distance from the center pixel. In this way only the relevant filtering neighborhood for each pixel
 * is selected. The size and shape of the neighborhood adopt to the image features, including curved ones and sharp
 * corners. Presently the median filtering step is commented out. If the median filtering should be commented back in:
 * Note that the edge graph is created with a median filtered version of the YCrCb space. The median filter only obtains
 * a median of the 3 by 3 area if no edge is detected in any of Y, Cr, or Cb spaces. If an edge is detected in any of
 * the 3 spaces, then the Y, Cr, and Cb values are all unchanged. An edge is detected if the difference between the
 * center pixel and any one of its 4 nearest neighbors is greater than a threshold or if the center pixel is larger than
 * all of its four neighbors by threshold/2 or if the center pixel is smaller than all of its four neighbors by
 * threshold/2. The Y, Cr, and Cb spaces have 3 different thresholds in isEdgeY - 256*deltaY, 256*deltaCr, and
 * 256*deltaCb. The pixel neighbor or edge graph is an array of arrays, with the first array having a length equal to
 * the number of the pixels in the image and the second array having 8 integers for 2D color or 8 floats for 2D black
 * and white. The 8 values for 2D color are the edge weights calculated from the intensity differences between the
 * center pixel and one of the 8 neighboring pixels. edge weight = sqrt((dy*wY)**2 + (dr*wR)**2 + (dB*wB)**2), where dy,
 * dR, and dB are the Y, Cr, and Cb differences and wY, wR, and wB are the Y, Cr, and Cb weights derived from the Y, Cr,
 * and Cb radiuses. A bigger relative radius results in a smaller weight for radiuses >= 0.1, which should typically be
 * the case. For 2D black and white the edge weights are simply the absolute values of the differences between the 2
 * pixels. For 2D and 2.5D the computer memory is adequate for the edge graph to encompass the entire image. However,
 * for 3D with 26 nearest neighbors an edge graph for an entire image would have impossibly large memory requirements.
 * Therefore, an edge graph of size (2*range+1)*(2*range+1)*(2*range+1)*26 is separately created for every voxel in the
 * image. Memory requirements are reduced, but at the penalty of a much larger execution time. After the edge graph is
 * created, the Y, Cr, and Cb spaces are separately filtered. The filter uses the pixel as the center of a square with
 * sides of length 2*range + 1, with range = (int)(radius + 0.999). A priority queue is created with all of the queue
 * elements having a key, a value, and an origin. After the priority queue is created, the filter table is created.
 * Finally, the function enhancePixel, which returns the filtered pixel value at coordinate (x,y), is applied to every
 * pixel in the image. If neighboring pixels are connected to the center pixel via a low cost path, queue elements
 * corresponding to the neighboring pixels are added to the priority queue. If no queue element is retrieved from the
 * queue, then the original value is passed unchanged. If elements are retrieved from the queue, then those Y, Cr, or Cb
 * pixel values which had retrieved queue elements in the path from the low cost edge pixel to the center pixel are put
 * into a weighted sum. The weight is derived from the Gaussian of the distance from the center pixel with a pixel at a
 * distance of radius from the center having a weight of Math.exp(-2). If includeNeigbors is true, then a nearest 4
 * neighbor in 2D or a nearest 6 neighbor in 3D whose difference with the path pixel is less than threshold is included
 * in the weighting. However, a neighbor pixel is weighted a factor of 2 less than the path pixel. value gives the
 * location within the maxLength = (2*range+1)*(2*range+1) local area. value = range + range*(2*range + 1) is the center
 * of the local area. key contains a sum of all the edgeGraph values over the low cost path from the center pixel to the
 * selected pixel. origin is set to the value of the queue elment which provides the neighboring pixel on the path back
 * to the center pixel. The center pixel origin = -1. A position is only added to the priority queue if the difference
 * in neighboring pixel intensities is less than threshold. Initially, the seen array is set equal to 0 for all queue
 * elements within the local area. After a queue element has been added and retrieved from the queue, seen is set equal
 * to 2 for that position to prevent later addition to the queue. The variable last gives the number of queue elements
 * in the priority queue. The places array of length maxLength gives the queue element number of that local area
 * position or -1 if the local area position has no element present in the priority queue. The priority queue is
 * implemented as a binary heap and the binary heap is implemented as an array. A binary heap is a "nearly full" binary
 * tree(only the bottom level may not be complete). This binary heap has the property that for every node other than a
 * leaf, its key value is less than or equal to that of its children. In a binary heap the item of highest priority is
 * always at the root of the tree or at node 0. When the highest priority item at the root is removed, the item of
 * highest priority among the remainder moves into the root position. The children of node i are at 2*i + 1 and 2*i + 2.
 * The parent of node i is at (int)((i-1)/2). Adding an element e to the priority queue: The places of e.value returns a
 * number p. p >= 0 if the queue element is already in the priority queue. p = -1 if the queue element is not in the
 * priority queue. If element e is already in the queue with a smaller key value than the key value attached to e, then
 * simply return without putting e in the queue. If e is not in the queue and last, the number of queue elements, has
 * already reached the maximum size, then return with a full queue error. If e is not in the queue and last is less than
 * maxsize, set a new queue position i equal to last and then increment last. If e is already in the queue, set i to p.
 * Then, place e at queue position i. As long as i > 0 and the key of queue element i is less than the key of its parent
 * at position (int)((i-1)/2), then interchange the parent and the child queue elements. Set the places of the moved
 * queue elements to the new queue positions. Getting an element e from the priority queue: The highest priority queue
 * element, that is the queue element with the smallest key value, is always at queue[0], the root of the queue. Hence,
 * queue[0] is always returned. If last == 0, no elements are in the queue, so return with a priority queue is empty
 * error message. If last is not zero, set the places of the queue[0] element to -1 to indicate that it is no longer in
 * the queue. Decrement last. If last now equals zero, then the queue is now empty, so simply return. If last does not
 * equal zero, then move queue[last] into the root queue[0] position. Keep looping as long as the right child position,
 * 2*i + 2, is <= last. If the key of the left child position at j - 1 is less than the key of the right child position
 * at j, then decrement j to select the left child. If j == last, then the right child position is empty, so decrement
 * to select the left child. Otherwise, the right child position at j has the smaller key so do not decrement j. If the
 * key of the parent is greater than the key of the child, then swap the parent and child. Set the places of the moved
 * queue elements to the new queue positions. Stop looping when the key of the parent is <= the key of the child.
 * -------- Settings -------- You can adjust several parameters to obtain the best noise reduction level for each
 * particular image: - Filter radius for each Y, Cr, Cb color channel: Larger radius will remove more noise, however
 * some detail also can be lost. - threshold - neighboring pixels will not be added to the path if the intensity
 * difference >= threshold. - Cr and Cb 2x reduction: The resolution of Cr and Cb channels is reduced two times, which
 * improves the speed of the algorithm for large radiuses with little loss of quality. Recommended settings are. - for
 * high quality scanned images radius Y 2, Cr 2 Cb 2 Cr and Cb 2x reduction false - for high quality JPEG images radius
 * Y 2, Cr 3 Cb 4 Cr and Cb 2x reduction false - for low quality JPEG images radius Y 3, Cr 4 Cb 6 Cr and Cb 2x
 * reduction true
 */
public class AlgorithmAdaptivePathSmooth extends AlgorithmBase {

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

    /** DOCUMENT ME! */
    private int depth;

    /** The distMap at a position inside the (2*range+1)*(2*range+1) local square. */
    /** is a gaussian function of the distance from the center pixel. */
    private float[] distMap;

    /** In 3D if do25D == true, process each slice separately. */
    private boolean do25D = true;

    /** Contains edge weight between center and neighbor pixel in ARGB. */
    private int[][] edgeGraph;

    /** Contains edge weight between center and neighbor pixel in black and white. */
    private float[][] edgeGraphBW;

    /** DOCUMENT ME! */
    private int height;

    /** Contains key from queue, which has sum of edgeGraph values. */
    private float[] histDistance;

    /** contains postion of originating queue element in. */
    /** (2*range+1)*(2*range+1) local square. */
    private int[] histOrigin;

    /** contains x position of queue element. */
    private int[] histX;

    /** contains y position of queue element. */
    private int[] histY;

    /** contains z position of queue element. */
    private int[] histZ;

    /** if includeNeighbors == true, include nearest neighbors of pixels along. */
    /** the low cost path if they differ from the pixels by < threshold. */
    private boolean includeNeighbors = false;

    /** DOCUMENT ME! */
    private double invWeight;

    /** length = xDim * yDim. */
    private int length;

    /** maxLength = size of complete square local area = (2*range+1)*(2*range+1). */
    private int maxLength;

    /** DOCUMENT ME! */
    private float originalThreshold;

    /** Use to store pixels along the low cost path. */
    private int[] pathX;

    /** DOCUMENT ME! */
    private int[] pathY;

    /** DOCUMENT ME! */
    private int[] pathZ;

    /** DOCUMENT ME! */
    private int pIndex;

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

    /** If reduce is true, shrink the dimensions of the Cr and Cb spaces. */
    /** by a factor of 2 before filtering. Unshrink them after filtering. */
    private boolean reduce = false;

    /** Initially 0, set to 2 after queue element is added and retrieved from queue. */
    private char[] seen;

    /** DOCUMENT ME! */
    private float threshold; // if pixel differences >= threshold pixel
                             // cannot be added to path
                             // defaults to 0.1*(imageMax - imageMin)

    /** totalLength = length for 2D or 25D, = length * zDim for 3D. */
    private int totalLength;

    /** DOCUMENT ME! */
    private int width;

    /** weights assigned to Y, Cr, and Cb differences. */
    private double wY, wR, wB;

    /** DOCUMENT ME! */
    private int xDim, yDim, zDim;

    /** The smallest and largest values that can be reached from a center pixel. */
    private int xMin, xMax, yMin, yMax, zMin, zMax;

    /** Used to store positions of neighbor pixels that are added to the. */
    /** weighted sum. */
    private int[] xNeighbor;

    /** DOCUMENT ME! */
    private int xp, yp, zp;

    /** DOCUMENT ME! */
    private int[] yNeighbor;

    /** DOCUMENT ME! */
    private int[] zNeighbor;

    //~ Constructors ---------------------------------------------------------------------------------------------------

    /**
     * Creates a new AlgorithmAdaptivePathSmooth object.
     *
     * @param  destImage         denoised image
     * @param  srcImg            2D source image
     * @param  radiusY           DOCUMENT ME!
     * @param  radiusCr          DOCUMENT ME!
     * @param  radiusCb          DOCUMENT ME!
     * @param  threshold         DOCUMENT ME!
     * @param  includeNeighbors  DOCUMENT ME!
     * @param  reduce            DOCUMENT ME!
     * @param  do25D             DOCUMENT ME!
     */
    public AlgorithmAdaptivePathSmooth(ModelImage destImage, ModelImage srcImg, float radiusY, float radiusCr,
                                       float radiusCb, float threshold, boolean includeNeighbors, boolean reduce,
                                       boolean do25D) {
        super(destImage, srcImg);
        this.radiusY = radiusY;
        this.radiusCr = radiusCr;
        this.radiusCb = radiusCb;
        this.threshold = threshold;
        originalThreshold = threshold;
        this.includeNeighbors = includeNeighbors;
        this.reduce = reduce;
        this.do25D = do25D;
    }

    //~ Methods --------------------------------------------------------------------------------------------------------

    /**
     * Prepares this class for destruction.
     */
    public void finalize() {
        srcImage = null;
        destImage = null;
        data = null;
        dataBW = null;
        histX = null;
        histY = null;
        histOrigin = null;
        histDistance = null;
        seen = null;

        if (edgeGraph != null) {

            for (int i = 0; i < edgeGraph.length; i++) {
                edgeGraph[i] = null;
            }

            edgeGraph = null;
        }

        if (edgeGraphBW != null) {

            for (int i = 0; i < edgeGraphBW.length; i++) {
                edgeGraphBW[i] = null;
            }

            edgeGraphBW = null;
        }

        distMap = null;
        xNeighbor = null;
        yNeighbor = null;
        zNeighbor = null;
        pathX = null;
        pathY = null;
        pathZ = null;

        if (Q != null) {
            Q.places = null;
            Q.queue = null;
            Q = null;
        }

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

            // Notice that ARGB_USHORT and ARGB_FLOAT are not handled here.
            if (srcImage.getNDims() == 2) {

                if (srcImage.getType() == ModelStorageBase.ARGB) {
                    calc2D();
                } else if (!srcImage.isColorImage()) {
                    calc2DBW();
                }
            } else if (srcImage.getNDims() > 2) {

                if (do25D) {

                    if (srcImage.getType() == ModelStorageBase.ARGB) {
                        calc25D();
                    } else if (!srcImage.isColorImage()) {
                        calc25DBW();
                    }
                } else {

                    if (srcImage.getType() == ModelStorageBase.ARGB) {
                        calc3D();
                    } else if (!srcImage.isColorImage()) {
                        calc3DBW();
                    }
                }
            }
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
     * Prepares the data and runs the algorithm for a 3D ARGB image Convert from RGB to YCrCb space, run process, and
     * convert back from YCrCb space to RGB space. Processes one slice at a time.
     */
    private void calc25D() {

        int i;
        int[] imgBuffer;
        int[] Y;
        int[] Cr;
        int[] Cb;

        xDim = srcImage.getExtents()[0];
        yDim = srcImage.getExtents()[1];
        zDim = srcImage.getExtents()[2];

        try {
            length = xDim * yDim;
            totalLength = length;
            Y = new int[length];
            Cr = new int[length];
            Cb = new int[length];
            imgBuffer = new int[4 * length];
        } catch (OutOfMemoryError e) {
            cleanup();
            displayError("AlgorithmAdaptivePathSmooth:  Out of Memory");
            setCompleted(false);

            return;
        }

        fireProgressStateChanged(0, srcImage.getImageName(), "Processing slice 1 ...");
        
        

        for (i = 0; i < zDim; i++) {

            try {
                srcImage.exportData(4 * i * length, 4 * length, imgBuffer); // locks and releases lock
            } catch (IOException error) {
                cleanup();
                displayError("AlgorithmAdaptivePathSmooth: Image(s) locked");
                setCompleted(false);

                return;
            }

            fireProgressStateChanged((int)(i * (100) / zDim), 
                    srcImage.getImageName(), "Processing slice " + (i + 1) + "...");
           
            rgb2yCrCb(imgBuffer, Y, Cr, Cb);

            if (threadStopped) {
                cleanup();
                finalize();

                return;
            }

            process(Y, Cr, Cb, false);

            if (threadStopped) {
                cleanup();
                finalize();

                return;
            }

            yCrCb2rgb(imgBuffer, Y, Cr, Cb);

            if (threadStopped) {
                cleanup();
                finalize();

                return;
            }

            if (destImage != null) {

                try {
                    destImage.importData(4 * i * length, imgBuffer, false);
                } catch (IOException error) {
                    cleanup();
                    displayError("AlgorithmAdaptivePathSmooth: destImage locked " + error);
                    setCompleted(false);
                    return;
                }
            } else {

                try {
                    srcImage.importData(4 * i * length, imgBuffer, false);
                } catch (IOException error) {
                    cleanup();
                    displayError("AlgorithmAdaptivePathSmooth: srcImage locked " + error);
                    setCompleted(false);
                    return;
                }
            }
        } // for (i = 0; i < zDim; i++)

        if (destImage != null) {
            destImage.calcMinMax();
        } else {
            srcImage.calcMinMax();
        }

        fireProgressStateChanged(100, srcImage.getImageName(), "Processing slice " + (i + 1) + "...");
   
        if (threadStopped) {
            cleanup();
            finalize();

            return;
        }

        cleanup();
        setCompleted(true);
    }

    /**
     * Prepares the data and runs the algorithm for a 3D black and white image Processes one slice at a time.
     */
    private void calc25DBW() {

        int i;
        float[] imgBuffer;

        xDim = srcImage.getExtents()[0];
        yDim = srcImage.getExtents()[1];
        zDim = srcImage.getExtents()[2];

        try {
            length = xDim * yDim;
            totalLength = length;
            imgBuffer = new float[length];
           
        } catch (OutOfMemoryError e) {
            cleanup();
            displayError("AlgorithmAdaptivePathSmooth:  Out of Memory");
            setCompleted(false);

            return;
        }


        fireProgressStateChanged(0, srcImage.getImageName(), "Processing slice 1 ...");

        for (i = 0; i < zDim; i++) {

            try {
                srcImage.exportData(i * length, length, imgBuffer); // locks and releases lock
            } catch (IOException error) {
                cleanup();
                displayError("AlgorithmAdaptivePathSmooth: Image(s) locked");
                setCompleted(false);

                return;
            }

            fireProgressStateChanged((int)(i * 100 / zDim), srcImage.getImageName(), "Processing slice " + (i + 1) + "...");
            

            process(imgBuffer, false);

            if (threadStopped) {
                cleanup();
                finalize();

                return;
            }

            if (destImage != null) {

                try {
                    destImage.importData(i * length, imgBuffer, false);
                } catch (IOException error) {
                    cleanup();
                    displayError("AlgorithmAdaptivePathSmooth: destImage locked " + error);
                    setCompleted(false);

                    return;
                }
            } else {

                try {
                    srcImage.importData(i * length, imgBuffer, false);
                } catch (IOException error) {
                    cleanup();
                    displayError("AlgorithmAdaptivePathSmooth: srcImage locked " + error);
                    setCompleted(false);

                    return;
                }
            }
        } // for (i = 0; i < zDim; i++)

        if (destImage != null) {
            destImage.calcMinMax();
        } else {
            srcImage.calcMinMax();
        }

        fireProgressStateChanged(100, srcImage.getImageName(), "Processing slice " + (i + 1) + "...");
        
        if (threadStopped) {
            cleanup();
            finalize();

            return;
        }

        cleanup();
    
        setCompleted(true);
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
            totalLength = length;
            Y = new int[length];
            Cr = new int[length];
            Cb = new int[length];
            imgBuffer = new int[4 * length];
            srcImage.exportData(0, 4 * length, imgBuffer); // locks and releases lock
           
        } catch (IOException error) {
            cleanup();
            displayError("AlgorithmAdaptivePathSmooth: Image(s) locked");
            setCompleted(false);

            return;
        } catch (OutOfMemoryError e) {
            cleanup();
            displayError("AlgorithmAdaptivePathSmooth:  Out of Memory");
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

        process(Y, Cr, Cb, true);

        if (threadStopped) {
            cleanup();
            finalize();

            return;
        }

        fireProgressStateChanged(95, srcImage.getImageName(), "Converting fromYCrCb to RGB ...");
        
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
                displayError("AlgorithmAdaptivePathSmooth: destImage locked " + error);
                setCompleted(false);
                return;
            }
        } else {

            try {
                srcImage.importData(0, imgBuffer, true);
            } catch (IOException error) {
                cleanup();
                displayError("AlgorithmAdaptivePathSmooth: srcImage locked " + error);
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
            totalLength = length;
            imgBuffer = new float[length];
            srcImage.exportData(0, length, imgBuffer); // locks and releases lock
     
        } catch (IOException error) {
            cleanup();
            displayError("AlgorithmAdaptivePathSmooth: Image(s) locked");
            setCompleted(false);

            return;
        } catch (OutOfMemoryError e) {
            cleanup();
            displayError("AlgorithmAdaptivePathSmooth:  Out of Memory");
            setCompleted(false);

            return;
        }


        fireProgressStateChanged(0, srcImage.getImageName(), "Performing adaptive path smooth filter ...");


        process(imgBuffer, true);

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
                displayError("AlgorithmAdaptivePathSmooth: destImage locked " + error);
                setCompleted(false);
                return;
            }
        } else {

            try {
                srcImage.importData(0, imgBuffer, true);
            } catch (IOException error) {
                cleanup();
                displayError("AlgorithmAdaptivePathSmooth: srcImage locked " + error);
                setCompleted(false);
               
                return;
            }
        }

        fireProgressStateChanged(100, srcImage.getImageName(), "Performing adaptive path smooth filter ...");

        if (threadStopped) {
            cleanup();
            finalize();

            return;
        }

        cleanup();
      
        setCompleted(true);
    }

    /**
     * Prepares the data and runs the algorithm for a 3D ARGB image Convert from RGB to YCrCb space, run process, and
     * convert back from YCrCb space to RGB space.
     */
    private void calc3D() {
        int[] imgBuffer;
        int[] Y;
        int[] Cr;
        int[] Cb;

        xDim = srcImage.getExtents()[0];
        yDim = srcImage.getExtents()[1];
        zDim = srcImage.getExtents()[2];

        if ((radiusY < 0.1) || (radiusCr < 0.1) || (radiusCb < 0.1)) {
            return;
        }

        try {
            length = xDim * yDim;
            totalLength = length * zDim;
            Y = new int[totalLength];
            Cr = new int[totalLength];
            Cb = new int[totalLength];
            imgBuffer = new int[4 * totalLength];
            srcImage.exportData(0, 4 * totalLength, imgBuffer); // locks and releases lock
           
        } catch (IOException error) {
            cleanup();
            displayError("AlgorithmAdaptivePathSmooth: Image(s) locked");
            setCompleted(false);

            return;
        } catch (OutOfMemoryError e) {
            cleanup();
            displayError("AlgorithmAdaptivePathSmooth:  Out of Memory");
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

        fireProgressStateChanged(5, srcImage.getImageName(), "Converting from RGB to YCrCb ...");
        process3D(Y, Cr, Cb);

        if (threadStopped) {
            cleanup();
            finalize();

            return;
        }

        fireProgressStateChanged(95, srcImage.getImageName(), "Converting fromYCrCb to RGB ...");
        
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
                displayError("AlgorithmAdaptivePathSmooth: destImage locked " + error);
                setCompleted(false);

                return;
            }
        } else {

            try {
                srcImage.importData(0, imgBuffer, true);
            } catch (IOException error) {
                cleanup();
                displayError("AlgorithmAdaptivePathSmooth: srcImage locked " + error);
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
     * Prepares the data and runs the algorithm for a 3D black and white image Run process3D.
     */
    private void calc3DBW() {
        float[] imgBuffer;

        xDim = srcImage.getExtents()[0];
        yDim = srcImage.getExtents()[1];
        zDim = srcImage.getExtents()[2];

        int r2, r22;
        int i, j, k;
        float scale;

        if (radiusY < 0.1) {
            return;
        }

        range = (int) (radiusY + 0.999f);
        maxLength = ((2 * range) + 1) * ((2 * range) + 1) * ((2 * range) + 1);

        try {
            length = xDim * yDim;
            totalLength = length * zDim;
            imgBuffer = new float[totalLength];
            srcImage.exportData(0, totalLength, imgBuffer); // locks and releases lock
        } catch (IOException error) {
            cleanup();
            displayError("AlgorithmAdaptivePathSmooth: Image(s) locked");
            setCompleted(false);

            return;
        } catch (OutOfMemoryError e) {
            cleanup();
            displayError("AlgorithmAdaptivePathSmooth:  Out of Memory");
            setCompleted(false);

            return;
        }
        
        
        fireProgressStateChanged(0, srcImage.getImageName(), "Performing adaptive path smooth filter ...");

        try {
            histX = new int[maxLength];
            histY = new int[maxLength];
            histZ = new int[maxLength];
            histOrigin = new int[maxLength];
            histDistance = new float[maxLength];
            seen = new char[maxLength];
            Q = new PriorityQueue(maxLength);

            if (includeNeighbors) {
                xNeighbor = new int[maxLength];
                yNeighbor = new int[maxLength];
                zNeighbor = new int[maxLength];
                pathX = new int[maxLength];
                pathY = new int[maxLength];
                pathZ = new int[maxLength];
            }
        } catch (OutOfMemoryError e) {
            displayError("AlgorithmAdaptivePathSmooth: Out of memory");
            setCompleted(false);

            return;
        }

        // Precalculate the distance function table
        // distMap = Math.exp(-2) at a distance of radius from the center pixel.
        distMap = new float[maxLength];

        r2 = (range * 2) + 1;
        r22 = r2 * r2;

        scale = radiusY * radiusY / 2.0f;

        for (i = -range; i <= range; i++) {

            for (j = -range; j <= range; j++) {

                for (k = -range; k <= range; k++) {
                    distMap[(i + range) + ((j + range) * r2) + ((k + range) * r22)] = (float) Math.exp(-((i * i) +
                                                                                                         (+j * j) +
                                                                                                         (k * k)) /
                                                                                                           scale);
                }
            }
        }

        process3D(imgBuffer);

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
                displayError("AlgorithmAdaptivePathSmooth: destImage locked " + error);
                setCompleted(false);
                return;
            }
        } else {

            try {
                srcImage.importData(0, imgBuffer, true);
            } catch (IOException error) {
                cleanup();
                displayError("AlgorithmAdaptivePathSmooth: srcImage locked " + error);
                setCompleted(false);

                return;
            }
        }

        fireProgressStateChanged(100, srcImage.getImageName(), "Performing adaptive path smooth filter ...");
        

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
        dataBW = null;
        histX = null;
        histY = null;
        histZ = null;
        histOrigin = null;
        histDistance = null;
        seen = null;

        if (edgeGraph != null) {

            for (int i = 0; i < edgeGraph.length; i++) {
                edgeGraph[i] = null;
            }

            edgeGraph = null;
        }

        if (edgeGraphBW != null) {

            for (int i = 0; i < edgeGraphBW.length; i++) {
                edgeGraphBW[i] = null;
            }

            edgeGraphBW = null;
        }

        distMap = null;
        xNeighbor = null;
        yNeighbor = null;
        zNeighbor = null;
        pathX = null;
        pathY = null;
        pathZ = null;

        if (Q != null) {
            Q.places = null;
            Q.queue = null;
            Q = null;
        }

        System.gc();
    }

    /**
     * Create the pixel edge graph as an array of arrays, with the first array having a length equal to the number of
     * the pixels in the image and the second array having 8 floats. The 8 values are the edge weights = absolute
     * differences between neighboring pixel values. Used for black and white images.
     *
     * @param  Y  DOCUMENT ME!
     */
    private void createEdgeGraph(float[] Y) {
        int i, j, i1;
        int x, y;
        int gLength = xDim * yDim;
        edgeGraphBW = new float[gLength][8];

        for (i = 0; i < gLength; i++) {

            for (j = 0; j < 8; j++) {
                edgeGraphBW[i][j] = 0;
            }
        }

        for (y = 0; y < yDim; y++) {

            for (x = 0; x < xDim; x++) {
                i = x + (y * xDim);

                float startY = Y[i];

                if (x > 0) {
                    edgeGraphBW[i][0] = Math.abs(startY - Y[i - 1]);
                }

                if (y > 0) {
                    i1 = i - xDim;
                    edgeGraphBW[i][1] = Math.abs(startY - Y[i1]);
                }

                if (x < (xDim - 1)) {
                    i1 = i + 1;
                    edgeGraphBW[i][2] = Math.abs(startY - Y[i1]);
                }

                if (y < (yDim - 1)) {
                    i1 = i + xDim;
                    edgeGraphBW[i][3] = Math.abs(startY - Y[i1]);
                }

                if ((x > 0) && (y > 0)) {
                    i1 = i - xDim - 1;
                    edgeGraphBW[i][4] = Math.abs(startY - Y[i1]);
                }

                if ((x > 0) && (y < (yDim - 1))) {
                    i1 = i - 1 + xDim;
                    edgeGraphBW[i][5] = Math.abs(startY - Y[i1]);
                }

                if ((x < (xDim - 1)) && (y < (yDim - 1))) {
                    i1 = i + 1 + xDim;
                    edgeGraphBW[i][6] = Math.abs(startY - Y[i1]);
                }

                if ((x < (xDim - 1)) && (y > 0)) {
                    i1 = i + 1 - xDim;
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
        threshold = (originalThreshold / 3.0f) *
                        (float) Math.sqrt((256 * 256 * wY * wY) + (358 * 358 * wR * wR) + (456 * 456 * wB * wB));

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
     * Create the pixel edge graph as an array of arrays, with the first array having a length =
     * (2*range+1)*(2*range+1)*(2*range+1) and the second array having 26 integers. The 26 values are the edge weights =
     * absolute differences between neighboring pixel values. Used for black and white images.
     *
     * @param  Y  DOCUMENT ME!
     */
    private void createEdgeGraph3D(float[] Y) {
        int i, j, i1;
        int x, y, z;
        edgeGraphBW = new float[maxLength][26];

        int r2 = (2 * range) + 1;
        int r22 = r2 * r2;

        for (i = 0; i < maxLength; i++) {

            for (j = 0; j < 26; j++) {
                edgeGraphBW[i][j] = 0;
            }
        }

        for (z = zMin; z <= zMax; z++) {

            for (y = yMin; y <= yMax; y++) {

                for (x = xMin; x <= xMax; x++) {
                    i = (x - xp + range) + ((y - yp + range) * r2) + ((z - zp + range) * r22);

                    float startY = Y[i];

                    if (x > xMin) {
                        edgeGraphBW[i][0] = Math.abs(startY - Y[i - 1]);
                    }

                    if (y > yMin) {
                        i1 = i - r2;
                        edgeGraphBW[i][1] = Math.abs(startY - Y[i1]);
                    }

                    if (x < xMax) {
                        i1 = i + 1;
                        edgeGraphBW[i][2] = Math.abs(startY - Y[i1]);
                    }

                    if (y < yMax) {
                        i1 = i + r2;
                        edgeGraphBW[i][3] = Math.abs(startY - Y[i1]);
                    }

                    if ((x > xMin) && (y > yMin)) {
                        i1 = i - r2 - 1;
                        edgeGraphBW[i][4] = Math.abs(startY - Y[i1]);
                    }

                    if ((x > xMin) && (y < yMax)) {
                        i1 = i - 1 + r2;
                        edgeGraphBW[i][5] = Math.abs(startY - Y[i1]);
                    }

                    if ((x < xMax) && (y < yMax)) {
                        i1 = i + 1 + r2;
                        edgeGraphBW[i][6] = Math.abs(startY - Y[i1]);
                    }

                    if ((x < xMax) && (y > yMin)) {
                        i1 = i + 1 - r2;
                        edgeGraphBW[i][7] = Math.abs(startY - Y[i1]);
                    }

                    if (z > zMin) {
                        i1 = i - r22;
                        edgeGraphBW[i][8] = Math.abs(startY - Y[i1]);
                    }

                    if (z < zMax) {
                        i1 = i + r22;
                        edgeGraphBW[i][9] = Math.abs(startY - Y[i1]);
                    }

                    if ((x > xMin) && (z > zMin)) {
                        i1 = i - 1 - r22;
                        edgeGraphBW[i][10] = Math.abs(startY - Y[i1]);
                    }

                    if ((x > xMin) && (z < zMax)) {
                        i1 = i - 1 + r22;
                        edgeGraphBW[i][11] = Math.abs(startY - Y[i1]);
                    }

                    if ((x < xMax) && (z > zMin)) {
                        i1 = i + 1 - r22;
                        edgeGraphBW[i][12] = Math.abs(startY - Y[i1]);
                    }

                    if ((x < xMax) && (z < zMax)) {
                        i1 = i + 1 + r22;
                        edgeGraphBW[i][13] = Math.abs(startY - Y[i1]);
                    }

                    if ((y > yMin) && (z > zMin)) {
                        i1 = i - r2 - r22;
                        edgeGraphBW[i][14] = Math.abs(startY - Y[i1]);
                    }

                    if ((y > yMin) && (z < zMax)) {
                        i1 = i - r2 + r22;
                        edgeGraphBW[i][15] = Math.abs(startY - Y[i1]);
                    }

                    if ((y < yMax) && (z > zMin)) {
                        i1 = i + r2 - r22;
                        edgeGraphBW[i][16] = Math.abs(startY - Y[i1]);
                    }

                    if ((y < yMax) && (z < zMax)) {
                        i1 = i + r2 + r22;
                        edgeGraphBW[i][17] = Math.abs(startY - Y[i1]);
                    }

                    if ((x > xMin) && (y > yMin) && (z > zMin)) {
                        i1 = i - 1 - r2 - r22;
                        edgeGraphBW[i][18] = Math.abs(startY - Y[i1]);
                    }

                    if ((x > xMin) && (y > yMin) && (z < zMax)) {
                        i1 = i - 1 - r2 + r22;
                        edgeGraphBW[i][19] = Math.abs(startY - Y[i1]);
                    }

                    if ((x > xMin) && (y < yMax) && (z > zMin)) {
                        i1 = i - 1 + r2 - r22;
                        edgeGraphBW[i][20] = Math.abs(startY - Y[i1]);
                    }

                    if ((x > xMin) && (y < yMax) && (z < zMax)) {
                        i1 = i - 1 + r2 + r22;
                        edgeGraphBW[i][21] = Math.abs(startY - Y[i1]);
                    }

                    if ((x < xMax) && (y > yMin) && (z > zMin)) {
                        i1 = i + 1 - r2 - r22;
                        edgeGraphBW[i][22] = Math.abs(startY - Y[i1]);
                    }

                    if ((x < xMax) && (y > yMin) && (z < zMax)) {
                        i1 = i + 1 - r2 + r22;
                        edgeGraphBW[i][23] = Math.abs(startY - Y[i1]);
                    }

                    if ((x < xMax) && (y < yMax) && (z > zMin)) {
                        i1 = i + 1 + r2 - r22;
                        edgeGraphBW[i][24] = Math.abs(startY - Y[i1]);
                    }

                    if ((x < xMax) && (y < yMax) && (z < zMax)) {
                        i1 = i + 1 + r2 + r22;
                        edgeGraphBW[i][25] = Math.abs(startY - Y[i1]);
                    }
                }

            }
        }

    }

    /**
     * Create the pixel edge graph as an array of arrays, with the first array having a length =
     * (2*range+1)*(2*range+1)*(2*range+1) and the second array having 26 integers. The 26 values are the edge weights
     * calculated from the difference between the center pixel and one of the 26 neighboring pixels. edge weight =
     * sqrt((dy*wY)**2 + (dr*wR)**2 + (dB*wB)**2), where dy, dR, and dB are the Y, Cr, and Cb differences and wY, wR,
     * and wB are the Y, Cr, and Cb weights derived from the Y, Cr, and Cb radiuses. A bigger relative radius results in
     * a smaller weight for radiuses >= 0.1, which should typically be the case. Used for color images.
     *
     * @param  Y   DOCUMENT ME!
     * @param  Cr  DOCUMENT ME!
     * @param  Cb  DOCUMENT ME!
     */
    private void createEdgeGraph3D(int[] Y, int[] Cr, int[] Cb) {
        int i, j, i1;
        int x, y;
        int z;
        edgeGraph = new int[maxLength][26];

        int r2 = (2 * range) + 1;
        int r22 = r2 * r2;

        for (i = 0; i < maxLength; i++) {

            for (j = 0; j < 26; j++) {
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
        threshold = (originalThreshold / 3.0f) *
                        (float) Math.sqrt((256 * 256 * wY * wY) + (358 * 358 * wR * wR) + (456 * 456 * wB * wB));

        for (z = zMin; z <= zMax; z++) {

            for (y = yMin; y <= yMax; y++) {

                for (x = xMin; x <= xMax; x++) {
                    i = (x - xp + range) + ((y - yp + range) * r2) + ((z - zp + range) * r22);

                    int startY = Y[i];
                    int startR = Cr[i];
                    int startB = Cb[i];

                    if (x > xMin) {
                        edgeGraph[i][0] = getEdgeWeight(startY - Y[i - 1], startR - Cr[i - 1], startB - Cb[i - 1]);
                    }

                    if (y > yMin) {
                        i1 = i - r2;
                        edgeGraph[i][1] = getEdgeWeight(startY - Y[i1], startR - Cr[i1], startB - Cb[i1]);
                    }

                    if (x < xMax) {
                        i1 = i + 1;
                        edgeGraph[i][2] = getEdgeWeight(startY - Y[i1], startR - Cr[i1], startB - Cb[i1]);
                    }

                    if (y < yMax) {
                        i1 = i + r2;
                        edgeGraph[i][3] = getEdgeWeight(startY - Y[i1], startR - Cr[i1], startB - Cb[i1]);
                    }

                    if ((x > xMin) && (y > yMin)) {
                        i1 = i - r2 - 1;
                        edgeGraph[i][4] = getEdgeWeight(startY - Y[i1], startR - Cr[i1], startB - Cb[i1]);
                    }

                    if ((x > xMin) && (y < yMax)) {
                        i1 = i - 1 + r2;
                        edgeGraph[i][5] = getEdgeWeight(startY - Y[i1], startR - Cr[i1], startB - Cb[i1]);
                    }

                    if ((x < xMax) && (y < yMax)) {
                        i1 = i + 1 + r2;
                        edgeGraph[i][6] = getEdgeWeight(startY - Y[i1], startR - Cr[i1], startB - Cb[i1]);
                    }

                    if ((x < xMax) && (y > yMin)) {
                        i1 = i + 1 - r2;
                        edgeGraph[i][7] = getEdgeWeight(startY - Y[i1], startR - Cr[i1], startB - Cb[i1]);
                    }

                    if (z > zMin) {
                        i1 = i - r22;
                        edgeGraph[i][8] = getEdgeWeight(startY - Y[i1], startR - Cr[i1], startB - Cb[i1]);
                    }

                    if (z < zMax) {
                        i1 = i + r22;
                        edgeGraph[i][9] = getEdgeWeight(startY - Y[i1], startR - Cr[i1], startB - Cb[i1]);
                    }

                    if ((x > xMin) && (z > zMin)) {
                        i1 = i - 1 - r22;
                        edgeGraph[i][10] = getEdgeWeight(startY - Y[i1], startR - Cr[i1], startB - Cb[i1]);
                    }

                    if ((x > xMin) && (z < zMax)) {
                        i1 = i - 1 + r22;
                        edgeGraph[i][11] = getEdgeWeight(startY - Y[i1], startR - Cr[i1], startB - Cb[i1]);
                    }

                    if ((x < xMax) && (z > zMin)) {
                        i1 = i + 1 - r22;
                        edgeGraph[i][12] = getEdgeWeight(startY - Y[i1], startR - Cr[i1], startB - Cb[i1]);
                    }

                    if ((x < xMax) && (z < zMax)) {
                        i1 = i + 1 + r22;
                        edgeGraph[i][13] = getEdgeWeight(startY - Y[i1], startR - Cr[i1], startB - Cb[i1]);
                    }

                    if ((y > yMin) && (z > zMin)) {
                        i1 = i - r2 - r22;
                        edgeGraph[i][14] = getEdgeWeight(startY - Y[i1], startR - Cr[i1], startB - Cb[i1]);
                    }

                    if ((y > yMin) && (z < zMax)) {
                        i1 = i - r2 + r22;
                        edgeGraph[i][15] = getEdgeWeight(startY - Y[i1], startR - Cr[i1], startB - Cb[i1]);
                    }

                    if ((y < yMax) && (z > zMin)) {
                        i1 = i + r2 - r22;
                        edgeGraph[i][16] = getEdgeWeight(startY - Y[i1], startR - Cr[i1], startB - Cb[i1]);
                    }

                    if ((y < yMax) && (z < zMax)) {
                        i1 = i + r2 + r22;
                        edgeGraph[i][17] = getEdgeWeight(startY - Y[i1], startR - Cr[i1], startB - Cb[i1]);
                    }

                    if ((x > xMin) && (y > yMin) && (z > zMin)) {
                        i1 = i - 1 - r2 - r22;
                        edgeGraph[i][18] = getEdgeWeight(startY - Y[i1], startR - Cr[i1], startB - Cb[i1]);
                    }

                    if ((x > xMin) && (y > yMin) && (z < zMax)) {
                        i1 = i - 1 - r2 + r22;
                        edgeGraph[i][19] = getEdgeWeight(startY - Y[i1], startR - Cr[i1], startB - Cb[i1]);
                    }

                    if ((x > xMin) && (y < yMax) && (z > zMin)) {
                        i1 = i - 1 + r2 - r22;
                        edgeGraph[i][20] = getEdgeWeight(startY - Y[i1], startR - Cr[i1], startB - Cb[i1]);
                    }

                    if ((x > xMin) && (y < yMax) && (z < zMax)) {
                        i1 = i - 1 + r2 + r22;
                        edgeGraph[i][21] = getEdgeWeight(startY - Y[i1], startR - Cr[i1], startB - Cb[i1]);
                    }

                    if ((x < xMax) && (y > yMin) && (z > zMin)) {
                        i1 = i + 1 - r2 - r22;
                        edgeGraph[i][22] = getEdgeWeight(startY - Y[i1], startR - Cr[i1], startB - Cb[i1]);
                    }

                    if ((x < xMax) && (y > yMin) && (z < zMax)) {
                        i1 = i + 1 - r2 + r22;
                        edgeGraph[i][23] = getEdgeWeight(startY - Y[i1], startR - Cr[i1], startB - Cb[i1]);
                    }

                    if ((x < xMax) && (y < yMax) && (z > zMin)) {
                        i1 = i + 1 + r2 - r22;
                        edgeGraph[i][24] = getEdgeWeight(startY - Y[i1], startR - Cr[i1], startB - Cb[i1]);
                    }

                    if ((x < xMax) && (y < yMax) && (z < zMax)) {
                        i1 = i + 1 + r2 + r22;
                        edgeGraph[i][25] = getEdgeWeight(startY - Y[i1], startR - Cr[i1], startB - Cb[i1]);
                    }
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
     * weight is simply the Gaussian function of the distance from the center pixel. If includeNeighbors is true, then
     * any of the low cost path pixels' 4 nearest neighbors that differ from the low cost path pixel by less than
     * threshold are added to the sum with a weighting factor half that of a low cost path pixel.
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
        double weight;
        boolean foundLowEdgeCost;
        int lowestEdgeCount = 0;
        float lowestKey;
        int locX, locY;
        boolean originFound;
        int r2 = (2 * range) + 1;
        int originX, originY;
        boolean nextFound;
        boolean alreadyIncluded;
        int neighborCount = 0;
        int pathCount = 0;

        count = search(x, y);

        if (count == 0) {
            result = data[x + (y * width)];
        } else {
            foundLowEdgeCost = false;
            lowestKey = Float.MAX_VALUE;

            for (i = range; (i >= 1) && (!foundLowEdgeCost); i--) {

                for (j = 0; j < count; j++) {
                    locX = histX[j] - x;
                    locY = histY[j] - y;

                    if ((locX == i) || (locX == -i) || (locY == i) || (locY == -i)) {
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
                weight = distMap[(histX[i] - x + range) + (r2 * (histY[i] - y + range))];
                sum += data[histX[i] + (histY[i] * width)] * weight;
                divisor += weight;

                if (includeNeighbors) {
                    pathX[pathCount] = histX[i];
                    pathY[pathCount++] = histY[i];
                }

                if (histOrigin[i] == -1) {
                    originFound = true;
                } else {
                    originX = (histOrigin[i] % r2) + x - range;
                    originY = (histOrigin[i] / r2) + y - range;
                    nextFound = false;

                    for (j = 0; (j < count) && (!nextFound); j++) {

                        if ((histX[j] == originX) && (histY[j] == originY)) {
                            nextFound = true;
                            i = j;
                        }
                    }
                }
            }

            if (includeNeighbors) {

                for (i = 0; i < pathCount; i++) {

                    if (((pathX[i] - 1) >= (x - range)) && ((pathX[i] - 1) >= 0) &&
                            (Math.abs(data[pathX[i] + (pathY[i] * width)] - data[pathX[i] - 1 + (pathY[i] * width)]) <
                                 threshold)) {
                        alreadyIncluded = false;

                        for (j = 0; (j < pathCount) && (!alreadyIncluded); j++) {

                            if (((pathX[i] - 1) == pathX[j]) && (pathY[i] == pathY[j])) {
                                alreadyIncluded = true;
                            }
                        }

                        for (j = 0; (j < neighborCount) && (!alreadyIncluded); j++) {

                            if (((pathX[i] - 1) == xNeighbor[j]) && (pathY[i] == yNeighbor[j])) {
                                alreadyIncluded = true;
                            }
                        }

                        if (!alreadyIncluded) {
                            weight = distMap[(pathX[i] - 1 - x + range) + (r2 * (pathY[i] - y + range))];
                            sum += data[pathX[i] - 1 + (pathY[i] * width)] * weight / 2.0f;
                            divisor += weight / 2.0f;
                            xNeighbor[neighborCount] = pathX[i] - 1;
                            yNeighbor[neighborCount++] = pathY[i];
                        }
                    }

                    if (((pathX[i] + 1) <= (x + range)) && ((pathX[i] + 1) <= (width - 1)) &&
                            (Math.abs(data[pathX[i] + (pathY[i] * width)] - data[pathX[i] + 1 + (pathY[i] * width)]) <
                                 threshold)) {
                        alreadyIncluded = false;

                        for (j = 0; (j < pathCount) && (!alreadyIncluded); j++) {

                            if (((pathX[i] + 1) == pathX[j]) && (pathY[i] == pathY[j])) {
                                alreadyIncluded = true;
                            }
                        }

                        for (j = 0; (j < neighborCount) && (!alreadyIncluded); j++) {

                            if (((pathX[i] + 1) == xNeighbor[j]) && (pathY[i] == yNeighbor[j])) {
                                alreadyIncluded = true;
                            }
                        }

                        if (!alreadyIncluded) {
                            weight = distMap[(pathX[i] + 1 - x + range) + (r2 * (pathY[i] - y + range))];
                            sum += data[pathX[i] + 1 + (pathY[i] * width)] * weight / 2.0f;
                            divisor += weight / 2.0f;
                            xNeighbor[neighborCount] = pathX[i] + 1;
                            yNeighbor[neighborCount++] = pathY[i];
                        }
                    }

                    if (((pathY[i] - 1) >= (y - range)) && ((pathY[i] - 1) >= 0) &&
                            (Math.abs(data[pathX[i] + (pathY[i] * width)] - data[pathX[i] + ((pathY[i] - 1) * width)]) <
                                 threshold)) {
                        alreadyIncluded = false;

                        for (j = 0; (j < pathCount) && (!alreadyIncluded); j++) {

                            if ((pathX[i] == pathX[j]) && ((pathY[i] - 1) == pathY[j])) {
                                alreadyIncluded = true;
                            }
                        }

                        for (j = 0; (j < neighborCount) && (!alreadyIncluded); j++) {

                            if ((pathX[i] == xNeighbor[j]) && ((pathY[i] - 1) == yNeighbor[j])) {
                                alreadyIncluded = true;
                            }
                        }

                        if (!alreadyIncluded) {
                            weight = distMap[(pathX[i] - x + range) + (r2 * (pathY[i] - 1 - y + range))];
                            sum += data[pathX[i] + ((pathY[i] - 1) * width)] * weight / 2.0f;
                            divisor += weight / 2.0f;
                            xNeighbor[neighborCount] = pathX[i];
                            yNeighbor[neighborCount++] = pathY[i] - 1;
                        }
                    }

                    if (((pathY[i] + 1) <= (y + range)) && ((pathY[i] + 1) <= (height - 1)) &&
                            (Math.abs(data[pathX[i] + (pathY[i] * width)] - data[pathX[i] + ((pathY[i] + 1) * width)]) <
                                 threshold)) {
                        alreadyIncluded = false;

                        for (j = 0; (j < pathCount) && (!alreadyIncluded); j++) {

                            if ((pathX[i] == pathX[j]) && ((pathY[i] + 1) == pathY[j])) {
                                alreadyIncluded = true;
                            }
                        }

                        for (j = 0; (j < neighborCount) && (!alreadyIncluded); j++) {

                            if ((pathX[i] == xNeighbor[j]) && ((pathY[i] + 1) == yNeighbor[j])) {
                                alreadyIncluded = true;
                            }
                        }

                        if (!alreadyIncluded) {
                            weight = distMap[(pathX[i] - x + range) + (r2 * (pathY[i] + 1 - y + range))];
                            sum += data[pathX[i] + ((pathY[i] + 1) * width)] * weight / 2.0f;
                            divisor += weight / 2.0f;
                            xNeighbor[neighborCount] = pathX[i];
                            yNeighbor[neighborCount++] = pathY[i] + 1;
                        }
                    }
                } // for (i = 0; i < pathCount; i++)
            } // if (includeNeighbors)

            result = (int) ((sum / divisor) + 0.5);
        }

        return result;
    }

    /**
     * This method returns the filtered coordinate value at coordinate (x,y,z) for an ARGB image. If neighboring pixels
     * are connected to the center pixel via a low cost path, queue elements corresponding to the neighboring pixel are
     * added to the priority queue. If no queue element is retrieved from the queue, then the original value is passed
     * unchanged. If elements are retrieved from the queue, then those Y, Cr, or Cb pixel values which had retrieved
     * queue elements in the path from the low cost edge pixel to the center pixel are put into a weighted sum. The
     * weight is simply the Gaussian function of the distance from the center pixel. If includeNeighbors is true, then
     * any of the low cost path pixels' 6 nearest neighbors that differ from the low cost path pixel by less than
     * threshold, are added to the sum with a weighting factor half that of a low cost path pixel.
     *
     * @param   x  DOCUMENT ME!
     * @param   y  DOCUMENT ME!
     * @param   z  DOCUMENT ME!
     *
     * @return  result
     */
    private int enhancePixel(int x, int y, int z) {
        int i, j;
        int count;
        int result;
        double sum;
        double divisor;
        double weight;
        boolean foundLowEdgeCost;
        int lowestEdgeCount = 0;
        float lowestKey;
        int locX, locY, locZ;
        boolean originFound;
        int r2 = (2 * range) + 1;
        int r22 = r2 * r2;
        int sliceSize = width * height;
        int originX, originY, originZ;
        boolean nextFound;
        boolean alreadyIncluded;
        int neighborCount = 0;
        int pathCount = 0;

        count = search(x, y, z);

        if (count == 0) {
            result = data[x + (y * width) + (z * sliceSize)];
        } else {
            foundLowEdgeCost = false;
            lowestKey = Float.MAX_VALUE;

            for (i = range; (i >= 1) && (!foundLowEdgeCost); i--) {

                for (j = 0; j < count; j++) {
                    locX = histX[j] - x;
                    locY = histY[j] - y;
                    locZ = histZ[j] - z;

                    if ((locX == i) || (locX == -i) || (locY == i) || (locY == -i) || (locZ == i) || (locZ == -i)) {
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
                weight = distMap[(histX[i] - x + range) + (r2 * (histY[i] - y + range)) + (r22 * (histZ[i] - z + range))];
                sum += data[histX[i] + (histY[i] * width) + (histZ[i] * sliceSize)] * weight;
                divisor += weight;

                if (includeNeighbors) {
                    pathX[pathCount] = histX[i];
                    pathY[pathCount] = histY[i];
                    pathZ[pathCount++] = histZ[i];
                }

                if (histOrigin[i] == -1) {
                    originFound = true;
                } else {
                    originX = ((histOrigin[i] % r22) % r2) + x - range;
                    originY = ((histOrigin[i] % r22) / r2) + y - range;
                    originZ = (histOrigin[i] / r22) + z - range;
                    nextFound = false;

                    for (j = 0; (j < count) && (!nextFound); j++) {

                        if ((histX[j] == originX) && (histY[j] == originY) && (histZ[j] == originZ)) {
                            nextFound = true;
                            i = j;
                        }
                    }
                }
            }

            if (includeNeighbors) {

                for (i = 0; i < pathCount; i++) {

                    if (((pathX[i] - 1) >= (x - range)) && ((pathX[i] - 1) >= 0) &&
                            (Math.abs(data[pathX[i] + (pathY[i] * width) + (pathZ[i] * sliceSize)] -
                                          data[pathX[i] - 1 + (pathY[i] * width) + (pathZ[i] * sliceSize)]) <
                                 threshold)) {
                        alreadyIncluded = false;

                        for (j = 0; (j < pathCount) && (!alreadyIncluded); j++) {

                            if (((pathX[i] - 1) == pathX[j]) && (pathY[i] == pathY[j]) && (pathZ[i] == pathZ[j])) {
                                alreadyIncluded = true;
                            }
                        }

                        for (j = 0; (j < neighborCount) && (!alreadyIncluded); j++) {

                            if (((pathX[i] - 1) == xNeighbor[j]) && (pathY[i] == yNeighbor[j]) &&
                                    (pathZ[i] == zNeighbor[j])) {
                                alreadyIncluded = true;
                            }
                        }

                        if (!alreadyIncluded) {
                            weight = distMap[(pathX[i] - 1 - x + range) + (r2 * (pathY[i] - y + range)) +
                                             (r22 * (pathZ[i] - z + range))];
                            sum += data[pathX[i] - 1 + (pathY[i] * width) + (pathZ[i] * sliceSize)] * weight / 2.0f;
                            divisor += weight / 2.0f;
                            xNeighbor[neighborCount] = pathX[i] - 1;
                            yNeighbor[neighborCount] = pathY[i];
                            zNeighbor[neighborCount++] = pathZ[i];
                        }
                    }

                    if (((pathX[i] + 1) <= (x + range)) && ((pathX[i] + 1) <= (width - 1)) &&
                            (Math.abs(data[pathX[i] + (pathY[i] * width) + (pathZ[i] * sliceSize)] -
                                          data[pathX[i] + 1 + (pathY[i] * width) + (pathZ[i] * sliceSize)]) <
                                 threshold)) {
                        alreadyIncluded = false;

                        for (j = 0; (j < pathCount) && (!alreadyIncluded); j++) {

                            if (((pathX[i] + 1) == pathX[j]) && (pathY[i] == pathY[j]) && (pathZ[i] == pathZ[j])) {
                                alreadyIncluded = true;
                            }
                        }

                        for (j = 0; (j < neighborCount) && (!alreadyIncluded); j++) {

                            if (((pathX[i] + 1) == xNeighbor[j]) && (pathY[i] == yNeighbor[j]) &&
                                    (pathZ[i] == zNeighbor[j])) {
                                alreadyIncluded = true;
                            }
                        }

                        if (!alreadyIncluded) {
                            weight = distMap[(pathX[i] + 1 - x + range) + (r2 * (pathY[i] - y + range)) +
                                             (r22 * (pathZ[i] - z + range))];
                            sum += data[pathX[i] + 1 + (pathY[i] * width) + (pathZ[i] * sliceSize)] * weight / 2.0f;
                            divisor += weight / 2.0f;
                            xNeighbor[neighborCount] = pathX[i] + 1;
                            yNeighbor[neighborCount] = pathY[i];
                            zNeighbor[neighborCount++] = pathZ[i];
                        }
                    }

                    if (((pathY[i] - 1) >= (y - range)) && ((pathY[i] - 1) >= 0) &&
                            (Math.abs(data[pathX[i] + (pathY[i] * width) + (pathZ[i] * sliceSize)] -
                                          data[pathX[i] + ((pathY[i] - 1) * width) + (pathZ[i] * sliceSize)]) <
                                 threshold)) {
                        alreadyIncluded = false;

                        for (j = 0; (j < pathCount) && (!alreadyIncluded); j++) {

                            if ((pathX[i] == pathX[j]) && ((pathY[i] - 1) == pathY[j]) && (pathZ[i] == pathZ[j])) {
                                alreadyIncluded = true;
                            }
                        }

                        for (j = 0; (j < neighborCount) && (!alreadyIncluded); j++) {

                            if ((pathX[i] == xNeighbor[j]) && ((pathY[i] - 1) == yNeighbor[j]) &&
                                    (pathZ[i] == zNeighbor[j])) {
                                alreadyIncluded = true;
                            }
                        }

                        if (!alreadyIncluded) {
                            weight = distMap[(pathX[i] - x + range) + (r2 * (pathY[i] - 1 - y + range)) +
                                             (r22 * (pathZ[i] - z + range))];
                            sum += data[pathX[i] + ((pathY[i] - 1) * width) + (pathZ[i] * sliceSize)] * weight / 2.0f;
                            divisor += weight / 2.0f;
                            xNeighbor[neighborCount] = pathX[i];
                            yNeighbor[neighborCount] = pathY[i] - 1;
                            zNeighbor[neighborCount++] = pathZ[i];
                        }
                    }

                    if (((pathY[i] + 1) <= (y + range)) && ((pathY[i] + 1) <= (height - 1)) &&
                            (Math.abs(data[pathX[i] + (pathY[i] * width) + (pathZ[i] * sliceSize)] -
                                          data[pathX[i] + ((pathY[i] + 1) * width) + (pathZ[i] * sliceSize)]) <
                                 threshold)) {
                        alreadyIncluded = false;

                        for (j = 0; (j < pathCount) && (!alreadyIncluded); j++) {

                            if ((pathX[i] == pathX[j]) && ((pathY[i] + 1) == pathY[j]) && (pathZ[i] == pathZ[j])) {
                                alreadyIncluded = true;
                            }
                        }

                        for (j = 0; (j < neighborCount) && (!alreadyIncluded); j++) {

                            if ((pathX[i] == xNeighbor[j]) && ((pathY[i] + 1) == yNeighbor[j]) &&
                                    (pathZ[i] == zNeighbor[j])) {
                                alreadyIncluded = true;
                            }
                        }

                        if (!alreadyIncluded) {
                            weight = distMap[(pathX[i] - x + range) + (r2 * (pathY[i] + 1 - y + range)) +
                                             (r22 * (pathZ[i] - z + range))];
                            sum += data[pathX[i] + ((pathY[i] + 1) * width) + (pathZ[i] * sliceSize)] * weight / 2.0f;
                            divisor += weight / 2.0f;
                            xNeighbor[neighborCount] = pathX[i];
                            yNeighbor[neighborCount] = pathY[i] + 1;
                            zNeighbor[neighborCount++] = pathZ[i];
                        }
                    }

                    if (((pathZ[i] - 1) >= (z - range)) && ((pathZ[i] - 1) >= 0) &&
                            (Math.abs(data[pathX[i] + (pathY[i] * width) + (pathZ[i] * sliceSize)] -
                                          data[pathX[i] + (pathY[i] * width) + ((pathZ[i] - 1) * sliceSize)]) <
                                 threshold)) {
                        alreadyIncluded = false;

                        for (j = 0; (j < pathCount) && (!alreadyIncluded); j++) {

                            if ((pathX[i] == pathX[j]) && (pathY[i] == pathY[j]) && ((pathZ[i] - 1) == pathZ[j])) {
                                alreadyIncluded = true;
                            }
                        }

                        for (j = 0; (j < neighborCount) && (!alreadyIncluded); j++) {

                            if ((pathX[i] == xNeighbor[j]) && (pathY[i] == yNeighbor[j]) &&
                                    ((pathZ[i] - 1) == zNeighbor[j])) {
                                alreadyIncluded = true;
                            }
                        }

                        if (!alreadyIncluded) {
                            weight = distMap[(pathX[i] - x + range) + (r2 * (pathY[i] - y + range)) +
                                             (r22 * (pathZ[i] - 1 - z + range))];
                            sum += data[pathX[i] + (pathY[i] * width) + ((pathZ[i] - 1) * sliceSize)] * weight / 2.0f;
                            divisor += weight / 2.0f;
                            xNeighbor[neighborCount] = pathX[i];
                            yNeighbor[neighborCount] = pathY[i];
                            zNeighbor[neighborCount++] = pathZ[i] - 1;
                        }
                    }

                    if (((pathZ[i] + 1) <= (z + range)) && ((pathZ[i] + 1) <= (depth - 1)) &&
                            (Math.abs(data[pathX[i] + (pathY[i] * width) + (pathZ[i] * sliceSize)] -
                                          data[pathX[i] + (pathY[i] * width) + ((pathZ[i] + 1) * sliceSize)]) <
                                 threshold)) {
                        alreadyIncluded = false;

                        for (j = 0; (j < pathCount) && (!alreadyIncluded); j++) {

                            if ((pathX[i] == pathX[j]) && (pathY[i] == pathY[j]) && ((pathZ[i] + 1) == pathZ[j])) {
                                alreadyIncluded = true;
                            }
                        }

                        for (j = 0; (j < neighborCount) && (!alreadyIncluded); j++) {

                            if ((pathX[i] == xNeighbor[j]) && (pathY[i] == yNeighbor[j]) &&
                                    ((pathZ[i] + 1) == zNeighbor[j])) {
                                alreadyIncluded = true;
                            }
                        }

                        if (!alreadyIncluded) {
                            weight = distMap[(pathX[i] - x + range) + (r2 * (pathY[i] - y + range)) +
                                             (r22 * (pathZ[i] + 1 - z + range))];
                            sum += data[pathX[i] + (pathY[i] * width) + ((pathZ[i] + 1) * sliceSize)] * weight / 2.0f;
                            divisor += weight / 2.0f;
                            xNeighbor[neighborCount] = pathX[i];
                            yNeighbor[neighborCount] = pathY[i];
                            zNeighbor[neighborCount++] = pathZ[i] + 1;
                        }
                    }
                } // for (i = 0; i < pathCount; i++)
            } // if (includeNeighbors)

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
     * simply the Gaussian function of the distance from the center pixel. If includeNeighbors is true, then any of the
     * low cost path pixels' 4 nearest neighbors that differ from the low cost path pixel by less than threshold are
     * added to the sum with a weighting factor half that of a low cost path pixel.
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
        double weight;
        boolean foundLowEdgeCost;
        int lowestEdgeCount = 0;
        float lowestKey;
        int locX, locY;
        boolean originFound;
        int r2 = (2 * range) + 1;
        int originX, originY;
        boolean nextFound;
        boolean alreadyIncluded;
        int neighborCount = 0;
        int pathCount = 0;

        count = searchBW(x, y);

        if (count == 0) {
            result = dataBW[x + (y * xDim)];
        } else {
            foundLowEdgeCost = false;
            lowestKey = Float.MAX_VALUE;

            for (i = range; (i >= 1) && (!foundLowEdgeCost); i--) {

                for (j = 0; j < count; j++) {
                    locX = histX[j] - x;
                    locY = histY[j] - y;

                    if ((locX == i) || (locX == -i) || (locY == i) || (locY == -i)) {
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
                weight = distMap[(histX[i] - x + range) + (r2 * (histY[i] - y + range))];
                sum += dataBW[histX[i] + (histY[i] * xDim)] * weight;
                divisor += weight;

                if (includeNeighbors) {
                    pathX[pathCount] = histX[i];
                    pathY[pathCount++] = histY[i];
                }

                if (histOrigin[i] == -1) {
                    originFound = true;
                } else {
                    originX = (histOrigin[i] % r2) + x - range;
                    originY = (histOrigin[i] / r2) + y - range;
                    nextFound = false;

                    for (j = 0; (j < count) && (!nextFound); j++) {

                        if ((histX[j] == originX) && (histY[j] == originY)) {
                            nextFound = true;
                            i = j;
                        }
                    }
                }
            }

            if (includeNeighbors) {

                for (i = 0; i < pathCount; i++) {

                    if (((pathX[i] - 1) >= (x - range)) && ((pathX[i] - 1) >= 0) &&
                            (Math.abs(dataBW[pathX[i] + (pathY[i] * xDim)] - dataBW[pathX[i] - 1 + (pathY[i] * xDim)]) <
                                 threshold)) {
                        alreadyIncluded = false;

                        for (j = 0; (j < pathCount) && (!alreadyIncluded); j++) {

                            if (((pathX[i] - 1) == pathX[j]) && (pathY[i] == pathY[j])) {
                                alreadyIncluded = true;
                            }
                        }

                        for (j = 0; (j < neighborCount) && (!alreadyIncluded); j++) {

                            if (((pathX[i] - 1) == xNeighbor[j]) && (pathY[i] == yNeighbor[j])) {
                                alreadyIncluded = true;
                            }
                        }

                        if (!alreadyIncluded) {
                            weight = distMap[(pathX[i] - 1 - x + range) + (r2 * (pathY[i] - y + range))];
                            sum += dataBW[pathX[i] - 1 + (pathY[i] * xDim)] * weight / 2.0f;
                            divisor += weight / 2.0f;
                            xNeighbor[neighborCount] = pathX[i] - 1;
                            yNeighbor[neighborCount++] = pathY[i];
                        }
                    }

                    if (((pathX[i] + 1) <= (x + range)) && ((pathX[i] + 1) <= (xDim - 1)) &&
                            (Math.abs(dataBW[pathX[i] + (pathY[i] * xDim)] - dataBW[pathX[i] + 1 + (pathY[i] * xDim)]) <
                                 threshold)) {
                        alreadyIncluded = false;

                        for (j = 0; (j < pathCount) && (!alreadyIncluded); j++) {

                            if (((pathX[i] + 1) == pathX[j]) && (pathY[i] == pathY[j])) {
                                alreadyIncluded = true;
                            }
                        }

                        for (j = 0; (j < neighborCount) && (!alreadyIncluded); j++) {

                            if (((pathX[i] + 1) == xNeighbor[j]) && (pathY[i] == yNeighbor[j])) {
                                alreadyIncluded = true;
                            }
                        }

                        if (!alreadyIncluded) {
                            weight = distMap[(pathX[i] + 1 - x + range) + (r2 * (pathY[i] - y + range))];
                            sum += dataBW[pathX[i] + 1 + (pathY[i] * xDim)] * weight / 2.0f;
                            divisor += weight / 2.0f;
                            xNeighbor[neighborCount] = pathX[i] + 1;
                            yNeighbor[neighborCount++] = pathY[i];
                        }
                    }

                    if (((pathY[i] - 1) >= (y - range)) && ((pathY[i] - 1) >= 0) &&
                            (Math.abs(dataBW[pathX[i] + (pathY[i] * xDim)] - dataBW[pathX[i] + ((pathY[i] - 1) * xDim)]) <
                                 threshold)) {
                        alreadyIncluded = false;

                        for (j = 0; (j < pathCount) && (!alreadyIncluded); j++) {

                            if ((pathX[i] == pathX[j]) && ((pathY[i] - 1) == pathY[j])) {
                                alreadyIncluded = true;
                            }
                        }

                        for (j = 0; (j < neighborCount) && (!alreadyIncluded); j++) {

                            if ((pathX[i] == xNeighbor[j]) && ((pathY[i] - 1) == yNeighbor[j])) {
                                alreadyIncluded = true;
                            }
                        }

                        if (!alreadyIncluded) {
                            weight = distMap[(pathX[i] - x + range) + (r2 * (pathY[i] - 1 - y + range))];
                            sum += dataBW[pathX[i] + ((pathY[i] - 1) * xDim)] * weight / 2.0f;
                            divisor += weight / 2.0f;
                            xNeighbor[neighborCount] = pathX[i];
                            yNeighbor[neighborCount++] = pathY[i] - 1;
                        }
                    }

                    if (((pathY[i] + 1) <= (y + range)) && ((pathY[i] + 1) <= (yDim - 1)) &&
                            (Math.abs(dataBW[pathX[i] + (pathY[i] * xDim)] - dataBW[pathX[i] + ((pathY[i] + 1) * xDim)]) <
                                 threshold)) {
                        alreadyIncluded = false;

                        for (j = 0; (j < pathCount) && (!alreadyIncluded); j++) {

                            if ((pathX[i] == pathX[j]) && ((pathY[i] + 1) == pathY[j])) {
                                alreadyIncluded = true;
                            }
                        }

                        for (j = 0; (j < neighborCount) && (!alreadyIncluded); j++) {

                            if ((pathX[i] == xNeighbor[j]) && ((pathY[i] + 1) == yNeighbor[j])) {
                                alreadyIncluded = true;
                            }
                        }

                        if (!alreadyIncluded) {
                            weight = distMap[(pathX[i] - x + range) + (r2 * (pathY[i] + 1 - y + range))];
                            sum += dataBW[pathX[i] + ((pathY[i] + 1) * xDim)] * weight / 2.0f;
                            divisor += weight / 2.0f;
                            xNeighbor[neighborCount] = pathX[i];
                            yNeighbor[neighborCount++] = pathY[i] + 1;
                        }
                    }
                } // if (i = 0; i < pathCount; i++)
            } // if (includeNeighbors)

            result = (float) (sum / divisor);
        }

        return result;
    }

    /**
     * This method returns the filtered coordinate value at coordinate (x,y,z) for a black and white image. If
     * neighboring pixels are connected to the center pixel via a low cost path, queue elements corresponding to the
     * neighboring pixels are added to the priority queue. If no queue element is retrieved from the queue, then the
     * original value is passed unchanged. If elements are retrieved from the queue, then those pixel values which had
     * retrieved queue elements in the path from the low cost edge pixel to the center pixel are put into a weighted
     * sum. The weight is simply the Gaussian function of the distance from the center pixel. If includeNeighbors is
     * true, then any of the low cost path pixels' 6 nearest neighbors that differ from the low cost path pixel by less
     * than threshold are added to the sum with a weighting factor half that of a low cost path pixel.
     *
     * @param   x  DOCUMENT ME!
     * @param   y  DOCUMENT ME!
     * @param   z  DOCUMENT ME!
     *
     * @return  result
     */
    private float enhancePixelBW(int x, int y, int z) {
        int i, j;
        int count;
        float result;
        double sum;
        double divisor;
        double weight;
        boolean foundLowEdgeCost;
        int lowestEdgeCount = 0;
        float lowestKey;
        int locX, locY, locZ;
        boolean originFound;
        int r2 = (2 * range) + 1;
        int r22 = r2 * r2;
        int sliceSize = xDim * yDim;
        int originX, originY, originZ;
        boolean nextFound;
        boolean alreadyIncluded;
        int neighborCount = 0;
        int pathCount = 0;

        count = searchBW(x, y, z);

        if (count == 0) {
            result = dataBW[x + (y * xDim) + (z * sliceSize)];
        } else {
            foundLowEdgeCost = false;
            lowestKey = Float.MAX_VALUE;

            for (i = range; (i >= 1) && (!foundLowEdgeCost); i--) {

                for (j = 0; j < count; j++) {
                    locX = histX[j] - x;
                    locY = histY[j] - y;
                    locZ = histZ[j] - z;

                    if ((locX == i) || (locX == -i) || (locY == i) || (locY == -i) || (locZ == i) || (locZ == -i)) {
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
                weight = distMap[(histX[i] - x + range) + (r2 * (histY[i] - y + range)) + (r22 * (histZ[i] - z + range))];
                sum += dataBW[histX[i] + (histY[i] * xDim) + (histZ[i] * sliceSize)] * weight;
                divisor += weight;

                if (includeNeighbors) {
                    pathX[pathCount] = histX[i];
                    pathY[pathCount] = histY[i];
                    pathZ[pathCount++] = histZ[i];
                }

                if (histOrigin[i] == -1) {
                    originFound = true;
                } else {
                    originX = ((histOrigin[i] % r22) % r2) + x - range;
                    originY = ((histOrigin[i] % r22) / r2) + y - range;
                    originZ = (histOrigin[i] / r22) + z - range;
                    nextFound = false;

                    for (j = 0; (j < count) && (!nextFound); j++) {

                        if ((histX[j] == originX) && (histY[j] == originY) && (histZ[j] == originZ)) {
                            nextFound = true;
                            i = j;
                        }
                    }
                }
            }

            if (includeNeighbors) {

                for (i = 0; i < pathCount; i++) {

                    if (((pathX[i] - 1) >= (x - range)) && ((pathX[i] - 1) >= 0) &&
                            (Math.abs(dataBW[pathX[i] + (pathY[i] * xDim) + (pathZ[i] * sliceSize)] -
                                          dataBW[pathX[i] - 1 + (pathY[i] * xDim) + (pathZ[i] * sliceSize)]) <
                                 threshold)) {
                        alreadyIncluded = false;

                        for (j = 0; (j < pathCount) && (!alreadyIncluded); j++) {

                            if (((pathX[i] - 1) == pathX[j]) && (pathY[i] == pathY[j]) && (pathZ[i] == pathZ[j])) {
                                alreadyIncluded = true;
                            }
                        }

                        for (j = 0; (j < neighborCount) && (!alreadyIncluded); j++) {

                            if (((pathX[i] - 1) == xNeighbor[j]) && (pathY[i] == yNeighbor[j]) &&
                                    (pathZ[i] == zNeighbor[j])) {
                                alreadyIncluded = true;
                            }
                        }

                        if (!alreadyIncluded) {
                            weight = distMap[(pathX[i] - 1 - x + range) + (r2 * (pathY[i] - y + range)) +
                                             (r22 * (pathZ[i] - z + range))];
                            sum += dataBW[pathX[i] - 1 + (pathY[i] * xDim) + (pathZ[i] * sliceSize)] * weight / 2.0f;
                            divisor += weight / 2.0f;
                            xNeighbor[neighborCount] = pathX[i] - 1;
                            yNeighbor[neighborCount] = pathY[i];
                            zNeighbor[neighborCount++] = pathZ[i];
                        }
                    }

                    if (((pathX[i] + 1) <= (x + range)) && ((pathX[i] + 1) <= (xDim - 1)) &&
                            (Math.abs(dataBW[pathX[i] + (pathY[i] * xDim) + (pathZ[i] * sliceSize)] -
                                          dataBW[pathX[i] + 1 + (pathY[i] * xDim) + (pathZ[i] * sliceSize)]) <
                                 threshold)) {
                        alreadyIncluded = false;

                        for (j = 0; (j < pathCount) && (!alreadyIncluded); j++) {

                            if (((pathX[i] + 1) == pathX[j]) && (pathY[i] == pathY[j]) && (pathZ[i] == pathZ[j])) {
                                alreadyIncluded = true;
                            }
                        }

                        for (j = 0; (j < neighborCount) && (!alreadyIncluded); j++) {

                            if (((pathX[i] + 1) == xNeighbor[j]) && (pathY[i] == yNeighbor[j]) &&
                                    (pathZ[i] == zNeighbor[j])) {
                                alreadyIncluded = true;
                            }
                        }

                        if (!alreadyIncluded) {
                            weight = distMap[(pathX[i] + 1 - x + range) + (r2 * (pathY[i] - y + range)) +
                                             (r22 * (pathZ[i] - z + range))];
                            sum += dataBW[pathX[i] + 1 + (pathY[i] * xDim) + (pathZ[i] * sliceSize)] * weight / 2.0f;
                            divisor += weight / 2.0f;
                            xNeighbor[neighborCount] = pathX[i] + 1;
                            yNeighbor[neighborCount] = pathY[i];
                            zNeighbor[neighborCount++] = pathZ[i];
                        }
                    }

                    if (((pathY[i] - 1) >= (y - range)) && ((pathY[i] - 1) >= 0) &&
                            (Math.abs(dataBW[pathX[i] + (pathY[i] * xDim) + (pathZ[i] * sliceSize)] -
                                          dataBW[pathX[i] + ((pathY[i] - 1) * xDim) + (pathZ[i] * sliceSize)]) <
                                 threshold)) {
                        alreadyIncluded = false;

                        for (j = 0; (j < pathCount) && (!alreadyIncluded); j++) {

                            if ((pathX[i] == pathX[j]) && ((pathY[i] - 1) == pathY[j]) && (pathZ[i] == pathZ[j])) {
                                alreadyIncluded = true;
                            }
                        }

                        for (j = 0; (j < neighborCount) && (!alreadyIncluded); j++) {

                            if ((pathX[i] == xNeighbor[j]) && ((pathY[i] - 1) == yNeighbor[j]) &&
                                    (pathZ[i] == zNeighbor[j])) {
                                alreadyIncluded = true;
                            }
                        }

                        if (!alreadyIncluded) {
                            weight = distMap[(pathX[i] - x + range) + (r2 * (pathY[i] - 1 - y + range)) +
                                             (r22 * (pathZ[i] - z + range))];
                            sum += dataBW[pathX[i] + ((pathY[i] - 1) * xDim) + (pathZ[i] * sliceSize)] * weight / 2.0f;
                            divisor += weight / 2.0f;
                            xNeighbor[neighborCount] = pathX[i];
                            yNeighbor[neighborCount] = pathY[i] - 1;
                            zNeighbor[neighborCount++] = pathZ[i];
                        }
                    }

                    if (((pathY[i] + 1) <= (y + range)) && ((pathY[i] + 1) <= (yDim - 1)) &&
                            (Math.abs(dataBW[pathX[i] + (pathY[i] * xDim) + (pathZ[i] * sliceSize)] -
                                          dataBW[pathX[i] + ((pathY[i] + 1) * xDim) + (pathZ[i] * sliceSize)]) <
                                 threshold)) {
                        alreadyIncluded = false;

                        for (j = 0; (j < pathCount) && (!alreadyIncluded); j++) {

                            if ((pathX[i] == pathX[j]) && ((pathY[i] + 1) == pathY[j]) && (pathZ[i] == pathZ[j])) {
                                alreadyIncluded = true;
                            }
                        }

                        for (j = 0; (j < neighborCount) && (!alreadyIncluded); j++) {

                            if ((pathX[i] == xNeighbor[j]) && ((pathY[i] + 1) == yNeighbor[j]) &&
                                    (pathZ[i] == zNeighbor[j])) {
                                alreadyIncluded = true;
                            }
                        }

                        if (!alreadyIncluded) {
                            weight = distMap[(pathX[i] - x + range) + (r2 * (pathY[i] + 1 - y + range)) +
                                             (r22 * (pathZ[i] - z + range))];
                            sum += dataBW[pathX[i] + ((pathY[i] + 1) * xDim) + (pathZ[i] * sliceSize)] * weight / 2.0f;
                            divisor += weight / 2.0f;
                            xNeighbor[neighborCount] = pathX[i];
                            yNeighbor[neighborCount] = pathY[i] + 1;
                            zNeighbor[neighborCount++] = pathZ[i];
                        }
                    }

                    if (((pathZ[i] - 1) >= (z - range)) && ((pathZ[i] - 1) >= 0) &&
                            (Math.abs(dataBW[pathX[i] + (pathY[i] * xDim) + (pathZ[i] * sliceSize)] -
                                          dataBW[pathX[i] + (pathY[i] * xDim) + ((pathZ[i] - 1) * sliceSize)]) <
                                 threshold)) {
                        alreadyIncluded = false;

                        for (j = 0; (j < pathCount) && (!alreadyIncluded); j++) {

                            if ((pathX[i] == pathX[j]) && (pathY[i] == pathY[j]) && ((pathZ[i] - 1) == pathZ[j])) {
                                alreadyIncluded = true;
                            }
                        }

                        for (j = 0; (j < neighborCount) && (!alreadyIncluded); j++) {

                            if ((pathX[i] == xNeighbor[j]) && (pathY[i] == yNeighbor[j]) &&
                                    ((pathZ[i] - 1) == zNeighbor[j])) {
                                alreadyIncluded = true;
                            }
                        }

                        if (!alreadyIncluded) {
                            weight = distMap[(pathX[i] - x + range) + (r2 * (pathY[i] - y + range)) +
                                             (r22 * (pathZ[i] - 1 - z + range))];
                            sum += dataBW[pathX[i] + (pathY[i] * xDim) + ((pathZ[i] - 1) * sliceSize)] * weight / 2.0f;
                            divisor += weight / 2.0f;
                            xNeighbor[neighborCount] = pathX[i];
                            yNeighbor[neighborCount] = pathY[i];
                            zNeighbor[neighborCount++] = pathZ[i] - 1;
                        }
                    }

                    if (((pathZ[i] + 1) <= (z + range)) && ((pathZ[i] + 1) <= (zDim - 1)) &&
                            (Math.abs(dataBW[pathX[i] + (pathY[i] * xDim) + (pathZ[i] * sliceSize)] -
                                          dataBW[pathX[i] + (pathY[i] * xDim) + ((pathZ[i] + 1) * sliceSize)]) <
                                 threshold)) {
                        alreadyIncluded = false;

                        for (j = 0; (j < pathCount) && (!alreadyIncluded); j++) {

                            if ((pathX[i] == pathX[j]) && (pathY[i] == pathY[j]) && ((pathZ[i] + 1) == pathZ[j])) {
                                alreadyIncluded = true;
                            }
                        }

                        for (j = 0; (j < neighborCount) && (!alreadyIncluded); j++) {

                            if ((pathX[i] == xNeighbor[j]) && (pathY[i] == yNeighbor[j]) &&
                                    ((pathZ[i] + 1) == zNeighbor[j])) {
                                alreadyIncluded = true;
                            }
                        }

                        if (!alreadyIncluded) {
                            weight = distMap[(pathX[i] - x + range) + (r2 * (pathY[i] - y + range)) +
                                             (r22 * (pathZ[i] + 1 - z + range))];
                            sum += dataBW[pathX[i] + (pathY[i] * xDim) + ((pathZ[i] + 1) * sliceSize)] * weight / 2.0f;
                            divisor += weight / 2.0f;
                            xNeighbor[neighborCount] = pathX[i];
                            yNeighbor[neighborCount] = pathY[i];
                            zNeighbor[neighborCount++] = pathZ[i] + 1;
                        }
                    }
                } // for (i = 0; i < pathCount; i++)
            } // if (includeNeighbors)

            result = (float) (sum / divisor);
        }

        return result;
    }

    /**
     * This method performs the 2D filtering on ARGB images.
     */
    private void filterProcess() {
        int i, k;
        int r2;
        int[] data1;
        int dataLength;
        float scale;

        if (radius < 0.1) {
            return;
        }

        range = (int) (radius + 0.999f);
        maxLength = ((2 * range) + 1) * ((2 * range) + 1);

        try {
            histX = new int[maxLength];
            histY = new int[maxLength];
            histOrigin = new int[maxLength];
            histDistance = new float[maxLength];
            seen = new char[maxLength];
            Q = new PriorityQueue(maxLength);

            if (includeNeighbors) {
                xNeighbor = new int[maxLength];
                yNeighbor = new int[maxLength];
                pathX = new int[maxLength];
                pathY = new int[maxLength];
            }
        } catch (OutOfMemoryError e) {
            displayError("AlgorithmAdaptivePathSmooth: Out of memory");
            setCompleted(false);
            

            return;
        }

        // Precalculate the distance function table
        // distMap = Math.exp(-2) at a distance of radius from the center pixel.
        distMap = new float[maxLength];
        r2 = (range * 2) + 1;

        scale = radius * radius / 2.0f;

        for (i = -range; i <= range; i++) {

            for (k = -range; k <= range; k++) {
                distMap[(i + range) + ((k + range) * r2)] = (float) Math.exp(-((i * i) + (k * k)) / scale);
            }
        }

        // Apply the filter to the whole image
        dataLength = width * height;

        try {
            data1 = new int[dataLength];
        } catch (OutOfMemoryError e) {
            displayError("AlgorithmAdaptivePathSmooth: Out of memory creating data1 " + e);
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
        distMap = null;
    }

    /**
     * This method performs the 2D filtering on black and white images.
     */
    private void filterProcessBW() {
        int i, k;
        int r2;
        int dataLength;
        float scale;
        float[] data1BW;

        if (radiusY < 0.1) {
            return;
        }

        range = (int) (radiusY + 0.999f);
        maxLength = ((2 * range) + 1) * ((2 * range) + 1);

        try {
            histX = new int[maxLength];
            histY = new int[maxLength];
            histOrigin = new int[maxLength];
            histDistance = new float[maxLength];
            seen = new char[maxLength];
            Q = new PriorityQueue(maxLength);

            if (includeNeighbors) {
                xNeighbor = new int[maxLength];
                yNeighbor = new int[maxLength];
                pathX = new int[maxLength];
                pathY = new int[maxLength];
            }
        } catch (OutOfMemoryError e) {
            displayError("AlgorithmAdaptivePathSmooth: Out of memory");
            setCompleted(false);
            

            return;
        }

        // Precalculate the distance function table
        // distMap = Math.exp(-2) at a distance of radius from the center pixel.
        distMap = new float[maxLength];
        r2 = (range * 2) + 1;

        scale = radiusY * radiusY / 2.0f;

        for (i = -range; i <= range; i++) {

            for (k = -range; k <= range; k++) {
                distMap[(i + range) + ((k + range) * r2)] = (float) Math.exp(-((i * i) + (k * k)) / scale);
            }
        }

        // Apply the filter to the whole image
        dataLength = xDim * yDim;

        try {
            data1BW = new float[dataLength];
        } catch (OutOfMemoryError e) {
            displayError("AlgorithmAdaptivePathSmooth: Out of memory creating data1 " + e);
            setCompleted(false);
            

            return;
        }

        for (k = 0; k < yDim; k++) {

            for (i = 0; i < xDim; i++) {
                data1BW[(k * xDim) + i] = enhancePixelBW(i, k);
            }
        }

        for (i = 0; i < dataLength; i++) {
            dataBW[i] = data1BW[i];
        }

        data1BW = null;
        histX = null;
        histY = null;
        histOrigin = null;
        histDistance = null;
        seen = null;
        Q.queue = null;
        Q.places = null;
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

        for (i = 0; i < length; i++) {
            Y1[i] = Y[i];
        }

        for (y = 1; y < (yDim - 1); y++) {

            for (x = 1; x < (xDim - 1); x++) {
                counter = x + (y * xDim);

                if (isEdgeY(Y1, counter, originalThreshold)) { }
                else {
                    index = 0;

                    for (j = -1; j <= 1; j++) {

                        for (i = -1; i <= 1; i++) {
                            adr = (j * xDim) + i;
                            ya[index] = Y1[counter + adr];
                            index++;
                        }
                    }

                    Arrays.sort(ya);
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

        // Multiplicative factors based on the original
        // deltaY = 15, deltaCr = 30, and deltaCb = 100;
        deltaY = (int) (originalThreshold + 0.5f);
        deltaCr = (int) ((2 * originalThreshold) + 0.5f);
        deltaCb = (int) ((6.67 * originalThreshold) + 0.5f);

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

                    Arrays.sort(ya);
                    Y[counter] = median(ya);
                    Arrays.sort(cra);
                    Cr[counter] = median(cra);
                    Arrays.sort(cba);
                    Cb[counter] = median(cba);
                }
            }
        }

    }

    /**
     * Create a pixel neighbor graph, with the edge weights based on the differences between pixels. Then the Y space is
     * filtered.
     *
     * @param  Y           DOCUMENT ME!
     * @param  doProgress  DOCUMENT ME!
     */
    private void process(float[] Y, boolean doProgress) {
        int i;
        float[] fY = new float[length];

        for (i = 0; i < length; i++) {
            fY[i] = Y[i];
        }

        dataBW = Y;

    
        if (doProgress) {
            fireProgressStateChanged(20, srcImage.getImageName(), "Creating edge graph ...");
        }
        
        createEdgeGraph(fY);

        if (doProgress) {
            fireProgressStateChanged(50, srcImage.getImageName(), "Filtering in Y space ...");
        }
      
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
     * Create a pixel neighbor graph, with the edge weights based on the differences between pixels. Then, if reduce is
     * false, the Y, Cr, and Cb spaces are separately filtered at full size. If reduce is true, only the Y space is
     * filtered at full size, the Y, Cr, and Cb spaces are shrunk by a factor of 2, a new pixel neighbor graph is
     * created based on the shrunken spaces, the shrunken Cr and Cb spaces are filtered, and finally the Cr and Cb
     * spaces are expanded back to their original sizes.
     *
     * @param  Y           DOCUMENT ME!
     * @param  Cr          DOCUMENT ME!
     * @param  Cb          DOCUMENT ME!
     * @param  doProgress  DOCUMENT ME!
     */
    private void process(int[] Y, int[] Cr, int[] Cb, boolean doProgress) {
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

      
        if (doProgress) {
            fireProgressStateChanged(20, srcImage.getImageName(), "Creating edge graph ...");
        }
       

        createEdgeGraph(fY, fR, fB);

        if (doProgress) {
            fireProgressStateChanged(50, srcImage.getImageName(), "Filtering in Y space ...");
        }
        
        filterProcess();

        if (threadStopped) {
            cleanup();
            finalize();

            return;
        }

        if (!reduce) {
            data = Cb;
            radius = radiusCb;

            if (doProgress) {
                fireProgressStateChanged(70, srcImage.getImageName(), "Filtering in Cb space ...");
            }
            

            filterProcess();

            if (threadStopped) {
                cleanup();
                finalize();

                return;
            }

            data = Cr;
            radius = radiusCr;

            if (doProgress) {
                fireProgressStateChanged(90, srcImage.getImageName(), "Filtering in Cr space ...");
            }
            
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

            fireProgressStateChanged(70, srcImage.getImageName(), "Filtering in Cb space ...");
           
            filterProcess();

            if (threadStopped) {
                cleanup();
                finalize();

                return;
            }

            data = newR;
            radius = radiusCr / 2.0f;

            fireProgressStateChanged(90, srcImage.getImageName(), "Filtering in Cr space ...");
            
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
     * Create a pixel neighbor graph, with the edge weights based on the differences between pixels. Then the Y space is
     * filtered.
     *
     * @param  Y  DOCUMENT ME!
     */
    private void process3D(float[] Y) {
        int i, j;
        int index;
        int r2 = (2 * range) + 1;
        int r22 = r2 * r2;
        int sliceSize = xDim * yDim;
        int x, y, z;
        float[] fY = new float[maxLength];
        float[] data1BW;
        int newValue = 0;
        int oldValue = 0;

        try {
            data1BW = new float[totalLength];
        } catch (OutOfMemoryError e) {
            displayError("AlgorithmAdaptivePathSmooth: Out of memory creating data1BW " + e);
            setCompleted(false);
           
            return;
        }

        dataBW = Y;

        for (pIndex = 0; pIndex < totalLength; pIndex++) {
            newValue = pIndex * 100 / totalLength;

            if (newValue > oldValue) {
                fireProgressStateChanged(newValue, srcImage.getImageName(), "Converting from RGB to YCrCb ...");
               
            }

            oldValue = newValue;
            xp = (pIndex % sliceSize) % xDim;
            xMin = Math.max(xp - range, 0);
            xMax = Math.min(xp + range, xDim - 1);
            yp = (pIndex % sliceSize) / xDim;
            yMin = Math.max(yp - range, 0);
            yMax = Math.min(yp + range, yDim - 1);
            zp = pIndex / sliceSize;
            zMin = Math.max(zp - range, 0);
            zMax = Math.min(zp + range, zDim - 1);

            for (z = zMin; z <= zMax; z++) {

                for (y = yMin; y <= yMax; y++) {

                    for (x = xMin; x <= xMax; x++) {
                        index = x + (y * xDim) + (z * sliceSize);
                        j = (x - xp + range) + ((y - yp + range) * r2) + ((z - zp + range) * r22);
                        fY[j] = Y[index];
                    }

                }
            }

            createEdgeGraph3D(fY);

            data1BW[pIndex] = enhancePixelBW(xp, yp, zp);

            if (threadStopped) {
                cleanup();
                finalize();

                return;
            }
        }

        for (i = 0; i < totalLength; i++) {
            Y[i] = data1BW[i];
        }

        if (edgeGraph != null) {

            for (i = 0; i < edgeGraph.length; i++) {
                edgeGraph[i] = null;
            }

            edgeGraph = null;
        }

        fY = null;
        data1BW = null;
        histX = null;
        histY = null;
        histZ = null;
        histOrigin = null;
        histDistance = null;
        seen = null;
        Q.queue = null;
        Q.places = null;
        distMap = null;
    }

    /**
     * Create a pixel neighbor graph, with the edge weights based on the differences between pixels. Then, if reduce is
     * false, the Y, Cr, and Cb spaces are separately filtered at full size. If reduce is true, only the Y space is
     * filtered at full size, the Y, Cr, and Cb spaces are shrunk by a factor of 2, a new pixel neighbor graph is
     * created based on the shrunken spaces, the shrunken Cr and Cb spaces are filtered, and finally the Cr and Cb
     * spaces are expanded back to their original sizes.
     *
     * @param  Y   DOCUMENT ME!
     * @param  Cr  DOCUMENT ME!
     * @param  Cb  DOCUMENT ME!
     */
    private void process3D(int[] Y, int[] Cr, int[] Cb) {
        int i, j;
        int index;
        int x, y, z;
        int newWidth;
        int newHeight;
        int newDepth;
        int[] newY;
        int[] newR;
        int[] newB;
        int[] data1;
        int[] fY;
        int[] fR;
        int[] fB;
        int r2;
        int r22;
        int sliceSize = xDim * yDim;
        float scale;
        int k;
        int newValue = 5;
        int oldValue = 5;

        try {
            data1 = new int[totalLength];
        } catch (OutOfMemoryError e) {
            displayError("AlgorithmAdaptivePathSmooth: Out of memory creating data1 " + e);
            setCompleted(false);

            return;
        }

        range = (int) (radiusY + 0.999f);
        maxLength = ((2 * range) + 1) * ((2 * range) + 1) * ((2 * range) + 1);
        r2 = (2 * range) + 1;
        r22 = r2 * r2;

        try {
            histX = new int[maxLength];
            histY = new int[maxLength];
            histZ = new int[maxLength];
            histOrigin = new int[maxLength];
            histDistance = new float[maxLength];
            seen = new char[maxLength];
            Q = new PriorityQueue(maxLength);
            distMap = new float[maxLength];
            fY = new int[maxLength];
            fR = new int[maxLength];
            fB = new int[maxLength];

            if (includeNeighbors) {
                xNeighbor = new int[maxLength];
                yNeighbor = new int[maxLength];
                zNeighbor = new int[maxLength];
                pathX = new int[maxLength];
                pathY = new int[maxLength];
                pathZ = new int[maxLength];
            }
        } catch (OutOfMemoryError e) {
            displayError("AlgorithmAdaptivePathSmooth: Out of memory " + e);
            setCompleted(false);

            return;
        }

        // Precalculate the distance function table
        // distMap = Math.exp(-2) at a distance of radius from the center pixel.

        scale = radiusY * radiusY / 2.0f;

        for (i = -range; i <= range; i++) {

            for (j = -range; j <= range; j++) {

                for (k = -range; k <= range; k++) {
                    distMap[(i + range) + ((j + range) * r2) + ((k + range) * r22)] = (float) Math.exp(-((i * i) +
                                                                                                         (+j * j) +
                                                                                                         (k * k)) /
                                                                                                           scale);
                }
            }
        }

        data = Y;
        radius = radiusY;
        width = xDim;
        height = yDim;
        depth = zDim;


        for (pIndex = 0; pIndex < totalLength; pIndex++) {
            newValue = 5 + (pIndex * 35 / totalLength);

            if (newValue > oldValue) {
                fireProgressStateChanged(newValue, srcImage.getImageName(), "Filtering in Y space ...");
                
            }

            oldValue = newValue;
            xp = (pIndex % sliceSize) % xDim;
            xMin = Math.max(xp - range, 0);
            xMax = Math.min(xp + range, xDim - 1);
            yp = (pIndex % sliceSize) / xDim;
            yMin = Math.max(yp - range, 0);
            yMax = Math.min(yp + range, yDim - 1);
            zp = pIndex / sliceSize;
            zMin = Math.max(zp - range, 0);
            zMax = Math.min(zp + range, zDim - 1);

            for (z = zMin; z <= zMax; z++) {

                for (y = yMin; y <= yMax; y++) {

                    for (x = xMin; x <= xMax; x++) {
                        index = x + (y * xDim) + (z * sliceSize);
                        j = (x - xp + range) + ((y - yp + range) * r2) + ((z - zp + range) * r22);
                        fY[j] = Y[index];
                        fR[j] = Cr[index];
                        fB[j] = Cb[index];
                    }
                }
            }

            createEdgeGraph3D(fY, fR, fB);
            data1[pIndex] = enhancePixel(xp, yp, zp);

            if (threadStopped) {
                cleanup();
                finalize();

                return;
            }
        }

        for (i = 0; i < totalLength; i++) {
            Y[i] = data1[i];
        }

        if (!reduce) {
            range = (int) (radiusCb + 0.999f);
            maxLength = ((2 * range) + 1) * ((2 * range) + 1) * ((2 * range) + 1);
            r2 = (2 * range) + 1;
            r22 = r2 * r2;

            try {
                histX = new int[maxLength];
                histY = new int[maxLength];
                histZ = new int[maxLength];
                histOrigin = new int[maxLength];
                histDistance = new float[maxLength];
                seen = new char[maxLength];
                Q = new PriorityQueue(maxLength);
                distMap = new float[maxLength];
                fY = new int[maxLength];
                fR = new int[maxLength];
                fB = new int[maxLength];

                if (includeNeighbors) {
                    xNeighbor = new int[maxLength];
                    yNeighbor = new int[maxLength];
                    zNeighbor = new int[maxLength];
                    pathX = new int[maxLength];
                    pathY = new int[maxLength];
                    pathZ = new int[maxLength];
                }
            } catch (OutOfMemoryError e) {
                displayError("AlgorithmAdaptivePathSmooth: Out of memory " + e);
                setCompleted(false);

                return;
            }

            // Precalculate the distance function table
            // distMap = Math.exp(-2) at a distance of radius from the center pixel.

            scale = radiusCb * radiusCb / 2.0f;

            for (i = -range; i <= range; i++) {

                for (j = -range; j <= range; j++) {

                    for (k = -range; k <= range; k++) {
                        distMap[(i + range) + ((j + range) * r2) + ((k + range) * r22)] = (float) Math.exp(-((i * i) +
                                                                                                             (+j * j) +
                                                                                                             (k * k)) /
                                                                                                               scale);
                    }
                }
            }

            data = Cb;


            for (pIndex = 0; pIndex < totalLength; pIndex++) {
                newValue = 40 + (pIndex * 30 / totalLength);

                if (newValue > oldValue) {
                    fireProgressStateChanged((newValue), srcImage.getImageName(), "Filtering in Cb space ...");
                    
                }

                oldValue = newValue;
                xp = (pIndex % sliceSize) % xDim;
                xMin = Math.max(xp - range, 0);
                xMax = Math.min(xp + range, xDim - 1);
                yp = (pIndex % sliceSize) / xDim;
                yMin = Math.max(yp - range, 0);
                yMax = Math.min(yp + range, yDim - 1);
                zp = pIndex / sliceSize;
                zMin = Math.max(zp - range, 0);
                zMax = Math.min(zp + range, zDim - 1);

                for (z = zMin; z <= zMax; z++) {

                    for (y = yMin; y <= yMax; y++) {

                        for (x = xMin; x <= xMax; x++) {
                            index = x + (y * xDim) + (z * sliceSize);
                            j = (x - xp + range) + ((y - yp + range) * r2) + ((z - zp + range) * r22);
                            fY[j] = Y[index];
                            fR[j] = Cr[index];
                            fB[j] = Cb[index];
                        }
                    }
                }

                createEdgeGraph3D(fY, fR, fB);
                data1[pIndex] = enhancePixel(xp, yp, zp);

                if (threadStopped) {
                    cleanup();
                    finalize();

                    return;
                }
            }

            for (i = 0; i < totalLength; i++) {
                Cb[i] = data1[i];
            }

            range = (int) (radiusCr + 0.999f);
            maxLength = ((2 * range) + 1) * ((2 * range) + 1) * ((2 * range) + 1);
            r2 = (2 * range) + 1;
            r22 = r2 * r2;

            try {
                histX = new int[maxLength];
                histY = new int[maxLength];
                histZ = new int[maxLength];
                histOrigin = new int[maxLength];
                histDistance = new float[maxLength];
                seen = new char[maxLength];
                Q = new PriorityQueue(maxLength);
                distMap = new float[maxLength];
                fY = new int[maxLength];
                fR = new int[maxLength];
                fB = new int[maxLength];

                if (includeNeighbors) {
                    xNeighbor = new int[maxLength];
                    yNeighbor = new int[maxLength];
                    zNeighbor = new int[maxLength];
                    pathX = new int[maxLength];
                    pathY = new int[maxLength];
                    pathZ = new int[maxLength];
                }
            } catch (OutOfMemoryError e) {
                displayError("AlgorithmAdaptivePathSmooth: Out of memory " + e);
                setCompleted(false);

                return;
            }

            // Precalculate the distance function table
            // distMap = Math.exp(-2) at a distance of radius from the center pixel.

            scale = radiusCr * radiusCr / 2.0f;

            for (i = -range; i <= range; i++) {

                for (j = -range; j <= range; j++) {

                    for (k = -range; k <= range; k++) {
                        distMap[(i + range) + ((j + range) * r2) + ((k + range) * r22)] = (float) Math.exp(-((i * i) +
                                                                                                             (+j * j) +
                                                                                                             (k * k)) /
                                                                                                               scale);
                    }
                }
            }

            data = Cr;

            for (pIndex = 0; pIndex < totalLength; pIndex++) {
                newValue = 70 + (pIndex * 25 / totalLength);

                if (newValue > oldValue) {
                    fireProgressStateChanged((newValue), srcImage.getImageName(), "Filtering in Cr space ...");
                    
                }

                oldValue = newValue;
                xp = (pIndex % sliceSize) % xDim;
                xMin = Math.max(xp - range, 0);
                xMax = Math.min(xp + range, xDim - 1);
                yp = (pIndex % sliceSize) / xDim;
                yMin = Math.max(yp - range, 0);
                yMax = Math.min(yp + range, yDim - 1);
                zp = pIndex / sliceSize;
                zMin = Math.max(zp - range, 0);
                zMax = Math.min(zp + range, zDim - 1);

                for (z = zMin; z <= zMax; z++) {

                    for (y = yMin; y <= yMax; y++) {

                        for (x = xMin; x <= xMax; x++) {
                            index = x + (y * xDim) + (z * sliceSize);
                            j = (x - xp + range) + ((y - yp + range) * r2) + ((z - zp + range) * r22);
                            fY[j] = Y[index];
                            fR[j] = Cr[index];
                            fB[j] = Cb[index];
                        }
                    }
                }

                createEdgeGraph3D(fY, fR, fB);
                data1[pIndex] = enhancePixel(xp, yp, zp);

                if (threadStopped) {
                    cleanup();
                    finalize();

                    return;
                }
            }

            for (i = 0; i < totalLength; i++) {
                Cr[i] = data1[i];
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
            newDepth = (zDim + 2) / 2;

            float xRes = srcImage.getFileInfo(0).getResolutions()[0];
            float yRes = srcImage.getFileInfo(0).getResolutions()[1];
            float zRes = srcImage.getFileInfo(0).getResolutions()[2];

            float newXRes = xRes * xDim / newWidth;
            float newYRes = yRes * yDim / newHeight;
            float newZRes = zRes * zDim / newDepth;

            TransMatrix matrix = new TransMatrix(4);

            newY = new int[newWidth * newHeight * newDepth];

            float[] newYf = new float[newWidth * newHeight * newDepth];

            float[] Yf = new float[totalLength];

            for (i = 0; i < totalLength; i++) {
                Yf[i] = Y[i];
            }

            AlgorithmTransform.transformTrilinear(Yf, newYf, matrix, xDim, yDim, zDim, xRes, yRes, zRes, newWidth,
                                                  newHeight, newDepth, newXRes, newYRes, newZRes, null);
            Yf = null;

            for (i = 0; i < newYf.length; i++) {
                newY[i] = (int) (newYf[i] + 0.5f);
            }

            newYf = null;

            newB = new int[newWidth * newHeight * newDepth];

            float[] newBf = new float[newWidth * newHeight * newDepth];

            float[] Cbf = new float[totalLength];

            for (i = 0; i < totalLength; i++) {
                Cbf[i] = Cb[i];
            }

            AlgorithmTransform.transformTrilinear(Cbf, newBf, matrix, xDim, yDim, zDim, xRes, yRes, zRes, newWidth,
                                                  newHeight, newDepth, newXRes, newYRes, newZRes, null);
            Cbf = null;

            for (i = 0; i < newBf.length; i++) {
                newB[i] = (int) (newBf[i] + 0.5f);
            }

            newBf = null;

            newR = new int[newWidth * newHeight * newDepth];

            float[] newRf = new float[newWidth * newHeight * newDepth];

            float[] Crf = new float[totalLength];

            for (i = 0; i < totalLength; i++) {
                Crf[i] = Cr[i];
            }

            AlgorithmTransform.transformTrilinear(Crf, newRf, matrix, xDim, yDim, zDim, xRes, yRes, zRes, newWidth,
                                                  newHeight, newDepth, newXRes, newYRes, newZRes, null);
            Crf = null;

            for (i = 0; i < newRf.length; i++) {
                newR[i] = (int) (newRf[i] + 0.5f);
            }

            newRf = null;

            width = newWidth;
            height = newHeight;
            depth = newDepth;
            sliceSize = newWidth * newHeight;
            totalLength = sliceSize * newDepth;

            radius = radiusCb / 2.0f;
            range = (int) (radius + 0.999f);
            maxLength = ((2 * range) + 1) * ((2 * range) + 1) * ((2 * range) + 1);
            r2 = (2 * range) + 1;
            r22 = r2 * r2;

            try {
                histX = new int[maxLength];
                histY = new int[maxLength];
                histZ = new int[maxLength];
                histOrigin = new int[maxLength];
                histDistance = new float[maxLength];
                seen = new char[maxLength];
                Q = new PriorityQueue(maxLength);
                distMap = new float[maxLength];
                fY = new int[maxLength];
                fR = new int[maxLength];
                fB = new int[maxLength];

                if (includeNeighbors) {
                    xNeighbor = new int[maxLength];
                    yNeighbor = new int[maxLength];
                    zNeighbor = new int[maxLength];
                    pathX = new int[maxLength];
                    pathY = new int[maxLength];
                    pathZ = new int[maxLength];
                }
            } catch (OutOfMemoryError e) {
                displayError("AlgorithmAdaptivePathSmooth: Out of memory " + e);
                setCompleted(false);

                return;
            }

            try {
                data1 = new int[totalLength];
            } catch (OutOfMemoryError e) {
                displayError("AlgorithmAdaptivePathSmooth: Out of memory creating data1 " + e);
                setCompleted(false);

                return;
            }

            // Precalculate the distance function table
            // distMap = Math.exp(-2) at a distance of radius from the center pixel.

            scale = radius * radius / 2.0f;

            for (i = -range; i <= range; i++) {

                for (j = -range; j <= range; j++) {

                    for (k = -range; k <= range; k++) {
                        distMap[(i + range) + ((j + range) * r2) + ((k + range) * r22)] = (float) Math.exp(-((i * i) +
                                                                                                             (+j * j) +
                                                                                                             (k * k)) /
                                                                                                               scale);
                    }
                }
            }

            data = newB;


            for (pIndex = 0; pIndex < totalLength; pIndex++) {
                newValue = 40 + (pIndex * 30 / totalLength);

                if (newValue > oldValue) {
                    fireProgressStateChanged((newValue), srcImage.getImageName(), "Filtering in Cb space ...");
                    
                }

                oldValue = newValue;
                xp = (pIndex % sliceSize) % width;
                xMin = Math.max(xp - range, 0);
                xMax = Math.min(xp + range, width - 1);
                yp = (pIndex % sliceSize) / width;
                yMin = Math.max(yp - range, 0);
                yMax = Math.min(yp + range, height - 1);
                zp = pIndex / sliceSize;
                zMin = Math.max(zp - range, 0);
                zMax = Math.min(zp + range, depth - 1);

                for (z = zMin; z <= zMax; z++) {

                    for (y = yMin; y <= yMax; y++) {

                        for (x = xMin; x <= xMax; x++) {
                            index = x + (y * width) + (z * sliceSize);
                            j = (x - xp + range) + ((y - yp + range) * r2) + ((z - zp + range) * r22);
                            fY[j] = newY[index];
                            fR[j] = newR[index];
                            fB[j] = newB[index];
                        }
                    }
                }

                createEdgeGraph3D(fY, fR, fB);
                data1[pIndex] = enhancePixel(xp, yp, zp);

                if (threadStopped) {
                    cleanup();
                    finalize();

                    return;
                }
            }

            for (i = 0; i < totalLength; i++) {
                newB[i] = data1[i];
            }

            radius = radiusCr / 2.0f;
            range = (int) (radius + 0.999f);
            maxLength = ((2 * range) + 1) * ((2 * range) + 1) * ((2 * range) + 1);
            r2 = (2 * range) + 1;
            r22 = r2 * r2;

            try {
                histX = new int[maxLength];
                histY = new int[maxLength];
                histZ = new int[maxLength];
                histOrigin = new int[maxLength];
                histDistance = new float[maxLength];
                seen = new char[maxLength];
                Q = new PriorityQueue(maxLength);
                distMap = new float[maxLength];
                fY = new int[maxLength];
                fR = new int[maxLength];
                fB = new int[maxLength];

                if (includeNeighbors) {
                    xNeighbor = new int[maxLength];
                    yNeighbor = new int[maxLength];
                    zNeighbor = new int[maxLength];
                    pathX = new int[maxLength];
                    pathY = new int[maxLength];
                    pathZ = new int[maxLength];
                }
            } catch (OutOfMemoryError e) {
                displayError("AlgorithmAdaptivePathSmooth: Out of memory " + e);
                setCompleted(false);

                return;
            }

            // Precalculate the distance function table
            // distMap = Math.exp(-2) at a distance of radius from the center pixel.

            scale = radius * radius / 2.0f;

            for (i = -range; i <= range; i++) {

                for (j = -range; j <= range; j++) {

                    for (k = -range; k <= range; k++) {
                        distMap[(i + range) + ((j + range) * r2) + ((k + range) * r22)] = (float) Math.exp(-((i * i) +
                                                                                                             (+j * j) +
                                                                                                             (k * k)) /
                                                                                                               scale);
                    }
                }
            }

            data = newR;

            for (pIndex = 0; pIndex < totalLength; pIndex++) {
                newValue = 70 + (pIndex * 25 / totalLength);

                if (newValue > oldValue) {
                    fireProgressStateChanged((newValue), srcImage.getImageName(), "Filtering in Cr space ...");
                    
                }

                oldValue = newValue;
                xp = (pIndex % sliceSize) % width;
                xMin = Math.max(xp - range, 0);
                xMax = Math.min(xp + range, width - 1);
                yp = (pIndex % sliceSize) / width;
                yMin = Math.max(yp - range, 0);
                yMax = Math.min(yp + range, height - 1);
                zp = pIndex / sliceSize;
                zMin = Math.max(zp - range, 0);
                zMax = Math.min(zp + range, depth - 1);

                for (z = zMin; z <= zMax; z++) {

                    for (y = yMin; y <= yMax; y++) {

                        for (x = xMin; x <= xMax; x++) {
                            index = x + (y * width) + (z * sliceSize);
                            j = (x - xp + range) + ((y - yp + range) * r2) + ((z - zp + range) * r22);
                            fY[j] = newY[index];
                            fR[j] = newR[index];
                            fB[j] = newB[index];
                        }
                    }
                }

                createEdgeGraph3D(fY, fR, fB);
                data1[pIndex] = enhancePixel(xp, yp, zp);

                if (threadStopped) {
                    cleanup();
                    finalize();

                    return;
                }
            }

            for (i = 0; i < totalLength; i++) {
                newR[i] = data1[i];
            }

            if (edgeGraph != null) {

                for (i = 0; i < edgeGraph.length; i++) {
                    edgeGraph[i] = null;
                }

                edgeGraph = null;
            }

            newBf = new float[newWidth * newHeight * newDepth];

            Cbf = new float[xDim * yDim * zDim];

            for (i = 0; i < totalLength; i++) {
                newBf[i] = newB[i];
            }

            AlgorithmTransform.transformTrilinear(newBf, Cbf, matrix, newWidth, newHeight, newDepth, newXRes, newYRes,
                                                  newZRes, xDim, yDim, zDim, xRes, yRes, zRes, null);
            newBf = null;

            for (i = 0; i < Cbf.length; i++) {
                Cb[i] = (int) (Cbf[i] + 0.5f);
            }

            Cbf = null;

            newRf = new float[newWidth * newHeight * newDepth];

            Crf = new float[xDim * yDim * zDim];

            for (i = 0; i < totalLength; i++) {
                newRf[i] = newR[i];
            }

            AlgorithmTransform.transformTrilinear(newRf, Crf, matrix, newWidth, newHeight, newDepth, newXRes, newYRes,
                                                  newZRes, xDim, yDim, zDim, xRes, yRes, zRes, null);
            newRf = null;

            for (i = 0; i < Crf.length; i++) {
                Cr[i] = (int) (Crf[i] + 0.5f);
            }

            Crf = null;

            newY = null;
            newR = null;
            newB = null;

            totalLength = xDim * yDim * zDim;
        } // reduce

        fY = null;
        fR = null;
        fB = null;
        data1 = null;
        histX = null;
        histY = null;
        histZ = null;
        histOrigin = null;
        histDistance = null;
        seen = null;
        Q.queue = null;
        Q.places = null;
        distMap = null;
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

        for (i = 0; i < totalLength; i++) {
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
        elem.value = range + (range * r2);
        elem.origin = -1; // no origin
        Q.addElement(elem);
        count = 0;

        while (Q.notEmpty() && (count < maxLength)) {
            elem = Q.getElement();

            int ind = elem.value;
            int y1 = (ind / r2) + y - range;
            int x1 = (ind % r2) + x - range;
            float key = elem.key;

            seen[ind] = 2;
            histX[count] = x1;
            histY[count] = y1;
            histOrigin[count] = elem.origin;
            histDistance[count++] = elem.key;

            int dataInd = x1 + (y1 * width);

            elem.origin = elem.value;

            if ((x1 > minX) && (seen[ind - 1] <= 1)) {
                elem.value = ind - 1;
                elem.key = key + edgeGraph[dataInd][0];

                if (edgeGraph[dataInd][0] < threshold) {
                    Q.addElement(elem);
                }
            }

            if ((y1 > minY) && (seen[ind - r2] <= 1)) {
                elem.value = ind - r2;
                elem.key = key + edgeGraph[dataInd][1];

                if (edgeGraph[dataInd][1] < threshold) {
                    Q.addElement(elem);
                }
            }

            if ((x1 < maxX) && (seen[ind + 1] <= 1)) {
                elem.value = ind + 1;
                elem.key = key + edgeGraph[dataInd][2];

                if (edgeGraph[dataInd][2] < threshold) {
                    Q.addElement(elem);
                }
            }

            if ((y1 < maxY) && (seen[ind + r2] <= 1)) {
                elem.value = ind + r2;
                elem.key = key + edgeGraph[dataInd][3];

                if (edgeGraph[dataInd][3] < threshold) {
                    Q.addElement(elem);
                }
            }

            if ((x1 > minX) && (y1 > minY) && (seen[ind - 1 - r2] <= 1)) {
                elem.value = ind - 1 - r2;
                elem.key = key + edgeGraph[dataInd][4];

                if (edgeGraph[dataInd][4] < threshold) {
                    Q.addElement(elem);
                }
            }

            if ((x1 > minX) && (y1 < maxY) && (seen[ind - 1 + r2] <= 1)) {
                elem.value = ind - 1 + r2;
                elem.key = key + edgeGraph[dataInd][5];

                if (edgeGraph[dataInd][5] < threshold) {
                    Q.addElement(elem);
                }
            }

            if ((x1 < maxX) && (y1 < maxY) && (seen[ind + 1 + r2] <= 1)) {
                elem.value = ind + 1 + r2;
                elem.key = key + edgeGraph[dataInd][6];

                if (edgeGraph[dataInd][6] < threshold) {
                    Q.addElement(elem);
                }
            }

            if ((x1 < maxX) && (y1 > minY) && (seen[ind + 1 - r2] <= 1)) {
                elem.value = ind + 1 - r2;
                elem.key = key + edgeGraph[dataInd][7];

                if (edgeGraph[dataInd][7] < threshold) {
                    Q.addElement(elem);
                }
            }

        }

        return count;
    }

    /**
     * This method finds all neighbor pixels within maxDistance from the initial pixel (x,y,z). For each pixel the
     * shortest path value is also obtained. This method is for ARGB images
     *
     * @param   x  DOCUMENT ME!
     * @param   y  DOCUMENT ME!
     * @param   z  DOCUMENT ME!
     *
     * @return  count
     */
    private int search(int x, int y, int z) {
        int i;
        int r2;
        int r22;
        int minX;
        int maxX;
        int minY;
        int maxY;
        int minZ;
        int maxZ;
        QueueElement elem;
        int count;
        int sliceSize = width * height;

        r2 = (range * 2) + 1;
        r22 = r2 * r2;

        minX = Math.max(x - range, 0);
        maxX = Math.min(x + range, width - 1);
        minY = Math.max(y - range, 0);
        maxY = Math.min(y + range, height - 1);
        minZ = Math.max(z - range, 0);
        maxZ = Math.min(z + range, depth - 1);

        for (i = 0; i < maxLength; i++) {
            seen[i] = 0;
        }

        elem = new QueueElement();
        elem.key = 0;
        elem.value = range + (range * r2) + (range * r22);
        elem.origin = -1; // no origin
        Q.addElement(elem);
        count = 0;

        while (Q.notEmpty() && (count < maxLength)) {
            elem = Q.getElement();

            int ind = elem.value;
            int z1 = (ind / r22) + z - range;
            int y1 = ((ind % r22) / r2) + y - range;
            int x1 = ((ind % r22) % r2) + x - range;
            float key = elem.key;

            seen[ind] = 2;
            histX[count] = x1;
            histY[count] = y1;
            histZ[count] = z1;
            histOrigin[count] = elem.origin;
            histDistance[count++] = elem.key;

            int dataInd = (x1 - x + range) + ((y1 - y + range) * r2) + ((z1 - z + range) * r22);

            elem.origin = elem.value;

            if ((x1 > minX) && (seen[ind - 1] <= 1)) {
                elem.value = ind - 1;
                elem.key = key + edgeGraph[dataInd][0];

                if (edgeGraph[dataInd][0] < threshold) {
                    Q.addElement(elem);
                }
            }

            if ((y1 > minY) && (seen[ind - r2] <= 1)) {
                elem.value = ind - r2;
                elem.key = key + edgeGraph[dataInd][1];

                if (edgeGraph[dataInd][1] < threshold) {
                    Q.addElement(elem);
                }
            }

            if ((x1 < maxX) && (seen[ind + 1] <= 1)) {
                elem.value = ind + 1;
                elem.key = key + edgeGraph[dataInd][2];

                if (edgeGraph[dataInd][2] < threshold) {
                    Q.addElement(elem);
                }
            }

            if ((y1 < maxY) && (seen[ind + r2] <= 1)) {
                elem.value = ind + r2;
                elem.key = key + edgeGraph[dataInd][3];

                if (edgeGraph[dataInd][3] < threshold) {
                    Q.addElement(elem);
                }
            }

            if ((x1 > minX) && (y1 > minY) && (seen[ind - 1 - r2] <= 1)) {
                elem.value = ind - 1 - r2;
                elem.key = key + edgeGraph[dataInd][4];

                if (edgeGraph[dataInd][4] < threshold) {
                    Q.addElement(elem);
                }
            }

            if ((x1 > minX) && (y1 < maxY) && (seen[ind - 1 + r2] <= 1)) {
                elem.value = ind - 1 + r2;
                elem.key = key + edgeGraph[dataInd][5];

                if (edgeGraph[dataInd][5] < threshold) {
                    Q.addElement(elem);
                }
            }

            if ((x1 < maxX) && (y1 < maxY) && (seen[ind + 1 + r2] <= 1)) {
                elem.value = ind + 1 + r2;
                elem.key = key + edgeGraph[dataInd][6];

                if (edgeGraph[dataInd][6] < threshold) {
                    Q.addElement(elem);
                }
            }

            if ((x1 < maxX) && (y1 > minY) && (seen[ind + 1 - r2] <= 1)) {
                elem.value = ind + 1 - r2;
                elem.key = key + edgeGraph[dataInd][7];

                if (edgeGraph[dataInd][7] < threshold) {
                    Q.addElement(elem);
                }
            }

            if ((z1 > minZ) && (seen[ind - r22] <= 1)) {
                elem.value = ind - r22;
                elem.key = key + edgeGraph[dataInd][8];

                if (edgeGraph[dataInd][8] < threshold) {
                    Q.addElement(elem);
                }
            }

            if ((z1 < maxZ) && (seen[ind + r22] <= 1)) {
                elem.value = ind + r22;
                elem.key = key + edgeGraph[dataInd][9];

                if (edgeGraph[dataInd][9] < threshold) {
                    Q.addElement(elem);
                }
            }

            if ((x1 > minX) && (z1 > minZ) && (seen[ind - 1 - r22] <= 1)) {
                elem.value = ind - 1 - r22;
                elem.key = key + edgeGraph[dataInd][10];

                if (edgeGraph[dataInd][10] < threshold) {
                    Q.addElement(elem);
                }
            }

            if ((x1 > minX) && (z1 < maxZ) && (seen[ind - 1 + r22] <= 1)) {
                elem.value = ind - 1 + r22;
                elem.key = key + edgeGraph[dataInd][11];

                if (edgeGraph[dataInd][11] < threshold) {
                    Q.addElement(elem);
                }
            }

            if ((x1 < maxX) && (z1 > minZ) && (seen[ind + 1 - r22] <= 1)) {
                elem.value = ind + 1 - r22;
                elem.key = key + edgeGraph[dataInd][12];

                if (edgeGraph[dataInd][12] < threshold) {
                    Q.addElement(elem);
                }
            }

            if ((x1 < maxX) && (z1 < maxZ) && (seen[ind + 1 + r22] <= 1)) {
                elem.value = ind + 1 + r22;
                elem.key = key + edgeGraph[dataInd][13];

                if (edgeGraph[dataInd][13] < threshold) {
                    Q.addElement(elem);
                }
            }

            if ((y1 > minY) && (z1 > minZ) && (seen[ind - r2 - r22] <= 1)) {
                elem.value = ind - r2 - r22;
                elem.key = key + edgeGraph[dataInd][14];

                if (edgeGraph[dataInd][14] < threshold) {
                    Q.addElement(elem);
                }
            }

            if ((y1 > minY) && (z1 < maxZ) && (seen[ind - r2 + r22] <= 1)) {
                elem.value = ind - r2 + r22;
                elem.key = key + edgeGraph[dataInd][15];

                if (edgeGraph[dataInd][15] < threshold) {
                    Q.addElement(elem);
                }
            }

            if ((y1 < maxY) && (z1 > minZ) && (seen[ind + r2 - r22] <= 1)) {
                elem.value = ind + r2 - r22;
                elem.key = key + edgeGraph[dataInd][16];

                if (edgeGraph[dataInd][16] < threshold) {
                    Q.addElement(elem);
                }
            }

            if ((y1 < maxY) && (z1 < maxZ) && (seen[ind + r2 + r22] <= 1)) {
                elem.value = ind + r2 + r22;
                elem.key = key + edgeGraph[dataInd][17];

                if (edgeGraph[dataInd][17] < threshold) {
                    Q.addElement(elem);
                }
            }

            if ((x1 > minX) && (y1 > minY) && (z1 > minZ) && (seen[ind - 1 - r2 - r22] <= 1)) {
                elem.value = ind - 1 - r2 - r22;
                elem.key = key + edgeGraph[dataInd][18];

                if (edgeGraph[dataInd][18] < threshold) {
                    Q.addElement(elem);
                }
            }

            if ((x1 > minX) && (y1 > minY) && (z1 < maxZ) && (seen[ind - 1 - r2 + r22] <= 1)) {
                elem.value = ind - 1 - r2 + r22;
                elem.key = key + edgeGraph[dataInd][19];

                if (edgeGraph[dataInd][19] < threshold) {
                    Q.addElement(elem);
                }
            }

            if ((x1 > minX) && (y1 < maxY) && (z1 > minZ) && (seen[ind - 1 + r2 - r22] <= 1)) {
                elem.value = ind - 1 + r2 - r22;
                elem.key = key + edgeGraph[dataInd][20];

                if (edgeGraph[dataInd][20] < threshold) {
                    Q.addElement(elem);
                }
            }

            if ((x1 > minX) && (y1 < maxY) && (z1 < maxZ) && (seen[ind - 1 + r2 + r22] <= 1)) {
                elem.value = ind - 1 + r2 + r22;
                elem.key = key + edgeGraph[dataInd][21];

                if (edgeGraph[dataInd][21] < threshold) {
                    Q.addElement(elem);
                }
            }

            if ((x1 < maxX) && (y1 > minY) && (z1 > minZ) && (seen[ind + 1 - r2 - r22] <= 1)) {
                elem.value = ind + 1 - r2 - r22;
                elem.key = key + edgeGraph[dataInd][22];

                if (edgeGraph[dataInd][22] < threshold) {
                    Q.addElement(elem);
                }
            }

            if ((x1 < maxX) && (y1 > minY) && (z1 < maxZ) && (seen[ind + 1 - r2 + r22] <= 1)) {
                elem.value = ind + 1 - r2 + r22;
                elem.key = key + edgeGraph[dataInd][23];

                if (edgeGraph[dataInd][23] < threshold) {
                    Q.addElement(elem);
                }
            }

            if ((x1 < maxX) && (y1 < maxY) && (z1 > minZ) && (seen[ind + 1 + r2 - r22] <= 1)) {
                elem.value = ind + 1 + r2 - r22;
                elem.key = key + edgeGraph[dataInd][24];

                if (edgeGraph[dataInd][24] < threshold) {
                    Q.addElement(elem);
                }
            }

            if ((x1 < maxX) && (y1 < maxY) && (z1 < maxZ) && (seen[ind + 1 + r2 + r22] <= 1)) {
                elem.value = ind + 1 + r2 + r22;
                elem.key = key + edgeGraph[dataInd][25];

                if (edgeGraph[dataInd][25] < threshold) {
                    Q.addElement(elem);
                }
            }
        }

        return count;
    }

    /**
     * This method finds all neighbor pixels within maxDistance from the initial pixel (x,y). For each pixel the
     * shortest path value is also obtained. This method is for black and white images.
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
        maxX = Math.min(x + range, xDim - 1);
        minY = Math.max(y - range, 0);
        maxY = Math.min(y + range, yDim - 1);

        for (i = 0; i < maxLength; i++) {
            seen[i] = 0;
        }

        elem = new QueueElement();
        elem.key = 0;
        elem.value = range + (range * r2);
        elem.origin = -1; // no origin
        Q.addElement(elem);
        count = 0;

        while (Q.notEmpty() && (count < maxLength)) {
            elem = Q.getElement();

            int ind = elem.value;
            int y1 = (ind / r2) + y - range;
            int x1 = (ind % r2) + x - range;
            float key = elem.key;

            seen[ind] = 2;
            histX[count] = x1;
            histY[count] = y1;
            histOrigin[count] = elem.origin;
            histDistance[count++] = elem.key;

            int dataInd = x1 + (y1 * xDim);

            elem.origin = elem.value;

            if ((x1 > minX) && (seen[ind - 1] <= 1)) {
                elem.value = ind - 1;
                elem.key = key + edgeGraphBW[dataInd][0];

                if (edgeGraphBW[dataInd][0] < threshold) {
                    Q.addElement(elem);
                }
            }

            if ((y1 > minY) && (seen[ind - r2] <= 1)) {
                elem.value = ind - r2;
                elem.key = key + edgeGraphBW[dataInd][1];

                if (edgeGraphBW[dataInd][1] < threshold) {
                    Q.addElement(elem);
                }
            }

            if ((x1 < maxX) && (seen[ind + 1] <= 1)) {
                elem.value = ind + 1;
                elem.key = key + edgeGraphBW[dataInd][2];

                if (edgeGraphBW[dataInd][2] < threshold) {
                    Q.addElement(elem);
                }
            }

            if ((y1 < maxY) && (seen[ind + r2] <= 1)) {
                elem.value = ind + r2;
                elem.key = key + edgeGraphBW[dataInd][3];

                if (edgeGraphBW[dataInd][3] < threshold) {
                    Q.addElement(elem);
                }
            }

            if ((x1 > minX) && (y1 > minY) && (seen[ind - 1 - r2] <= 1)) {
                elem.value = ind - 1 - r2;
                elem.key = key + edgeGraphBW[dataInd][4];

                if (edgeGraphBW[dataInd][4] < threshold) {
                    Q.addElement(elem);
                }
            }

            if ((x1 > minX) && (y1 < maxY) && (seen[ind - 1 + r2] <= 1)) {
                elem.value = ind - 1 + r2;
                elem.key = key + edgeGraphBW[dataInd][5];

                if (edgeGraphBW[dataInd][5] < threshold) {
                    Q.addElement(elem);
                }
            }

            if ((x1 < maxX) && (y1 < maxY) && (seen[ind + 1 + r2] <= 1)) {
                elem.value = ind + 1 + r2;
                elem.key = key + edgeGraphBW[dataInd][6];

                if (edgeGraphBW[dataInd][6] < threshold) {
                    Q.addElement(elem);
                }
            }

            if ((x1 < maxX) && (y1 > minY) && (seen[ind + 1 - r2] <= 1)) {
                elem.value = ind + 1 - r2;
                elem.key = key + edgeGraphBW[dataInd][7];

                if (edgeGraphBW[dataInd][7] < threshold) {
                    Q.addElement(elem);
                }
            }

        }

        return count;
    }

    /**
     * This method finds all neighbor pixels within maxDistance from the initial pixel (x,y,z). For each pixel the
     * shortest path value is also obtained. This method is for black and white images.
     *
     * @param   x  DOCUMENT ME!
     * @param   y  DOCUMENT ME!
     * @param   z  DOCUMENT ME!
     *
     * @return  count
     */
    private int searchBW(int x, int y, int z) {
        int i;
        int r2;
        int r22;
        int minX;
        int maxX;
        int minY;
        int maxY;
        int minZ;
        int maxZ;
        QueueElement elem;
        int count;
        int sliceSize = xDim * yDim;

        r2 = (range * 2) + 1;
        r22 = r2 * r2;

        minX = Math.max(x - range, 0);
        maxX = Math.min(x + range, xDim - 1);
        minY = Math.max(y - range, 0);
        maxY = Math.min(y + range, yDim - 1);
        minZ = Math.max(z - range, 0);
        maxZ = Math.min(z + range, zDim - 1);

        for (i = 0; i < maxLength; i++) {
            seen[i] = 0;
        }

        elem = new QueueElement();
        elem.key = 0;
        elem.value = range + (range * r2) + (range * r22);
        elem.origin = -1; // no origin
        Q.addElement(elem);
        count = 0;

        while (Q.notEmpty() && (count < maxLength)) {
            elem = Q.getElement();

            int ind = elem.value;
            int z1 = (ind / r22) + z - range;
            int y1 = ((ind % r22) / r2) + y - range;
            int x1 = ((ind % r22) % r2) + x - range;
            float key = elem.key;

            seen[ind] = 2;
            histX[count] = x1;
            histY[count] = y1;
            histZ[count] = z1;
            histOrigin[count] = elem.origin;
            histDistance[count++] = elem.key;

            int dataInd = (x1 - x + range) + ((y1 - y + range) * r2) + ((z1 - z + range) * r22);

            elem.origin = elem.value;

            if ((x1 > minX) && (seen[ind - 1] <= 1)) {
                elem.value = ind - 1;
                elem.key = key + edgeGraphBW[dataInd][0];

                if (edgeGraphBW[dataInd][0] < threshold) {
                    Q.addElement(elem);
                }
            }

            if ((y1 > minY) && (seen[ind - r2] <= 1)) {
                elem.value = ind - r2;
                elem.key = key + edgeGraphBW[dataInd][1];

                if (edgeGraphBW[dataInd][1] < threshold) {
                    Q.addElement(elem);
                }
            }

            if ((x1 < maxX) && (seen[ind + 1] <= 1)) {
                elem.value = ind + 1;
                elem.key = key + edgeGraphBW[dataInd][2];

                if (edgeGraphBW[dataInd][2] < threshold) {
                    Q.addElement(elem);
                }
            }

            if ((y1 < maxY) && (seen[ind + r2] <= 1)) {
                elem.value = ind + r2;
                elem.key = key + edgeGraphBW[dataInd][3];

                if (edgeGraphBW[dataInd][3] < threshold) {
                    Q.addElement(elem);
                }
            }

            if ((x1 > minX) && (y1 > minY) && (seen[ind - 1 - r2] <= 1)) {
                elem.value = ind - 1 - r2;
                elem.key = key + edgeGraphBW[dataInd][4];

                if (edgeGraphBW[dataInd][4] < threshold) {
                    Q.addElement(elem);
                }
            }

            if ((x1 > minX) && (y1 < maxY) && (seen[ind - 1 + r2] <= 1)) {
                elem.value = ind - 1 + r2;
                elem.key = key + edgeGraphBW[dataInd][5];

                if (edgeGraphBW[dataInd][5] < threshold) {
                    Q.addElement(elem);
                }
            }

            if ((x1 < maxX) && (y1 < maxY) && (seen[ind + 1 + r2] <= 1)) {
                elem.value = ind + 1 + r2;
                elem.key = key + edgeGraphBW[dataInd][6];

                if (edgeGraphBW[dataInd][6] < threshold) {
                    Q.addElement(elem);
                }
            }

            if ((x1 < maxX) && (y1 > minY) && (seen[ind + 1 - r2] <= 1)) {
                elem.value = ind + 1 - r2;
                elem.key = key + edgeGraphBW[dataInd][7];

                if (edgeGraphBW[dataInd][7] < threshold) {
                    Q.addElement(elem);
                }
            }

            if ((z1 > minZ) && (seen[ind - r22] <= 1)) {
                elem.value = ind - r22;
                elem.key = key + edgeGraphBW[dataInd][8];

                if (edgeGraphBW[dataInd][8] < threshold) {
                    Q.addElement(elem);
                }
            }

            if ((z1 < maxZ) && (seen[ind + r22] <= 1)) {
                elem.value = ind + r22;
                elem.key = key + edgeGraphBW[dataInd][9];

                if (edgeGraphBW[dataInd][9] < threshold) {
                    Q.addElement(elem);
                }
            }

            if ((x1 > minX) && (z1 > minZ) && (seen[ind - 1 - r22] <= 1)) {
                elem.value = ind - 1 - r22;
                elem.key = key + edgeGraphBW[dataInd][10];

                if (edgeGraphBW[dataInd][10] < threshold) {
                    Q.addElement(elem);
                }
            }

            if ((x1 > minX) && (z1 < maxZ) && (seen[ind - 1 + r22] <= 1)) {
                elem.value = ind - 1 + r22;
                elem.key = key + edgeGraphBW[dataInd][11];

                if (edgeGraphBW[dataInd][11] < threshold) {
                    Q.addElement(elem);
                }
            }

            if ((x1 < maxX) && (z1 > minZ) && (seen[ind + 1 - r22] <= 1)) {
                elem.value = ind + 1 - r22;
                elem.key = key + edgeGraphBW[dataInd][12];

                if (edgeGraphBW[dataInd][12] < threshold) {
                    Q.addElement(elem);
                }
            }

            if ((x1 < maxX) && (z1 < maxZ) && (seen[ind + 1 + r22] <= 1)) {
                elem.value = ind + 1 + r22;
                elem.key = key + edgeGraphBW[dataInd][13];

                if (edgeGraphBW[dataInd][13] < threshold) {
                    Q.addElement(elem);
                }
            }

            if ((y1 > minY) && (z1 > minZ) && (seen[ind - r2 - r22] <= 1)) {
                elem.value = ind - r2 - r22;
                elem.key = key + edgeGraphBW[dataInd][14];

                if (edgeGraphBW[dataInd][14] < threshold) {
                    Q.addElement(elem);
                }
            }

            if ((y1 > minY) && (z1 < maxZ) && (seen[ind - r2 + r22] <= 1)) {
                elem.value = ind - r2 + r22;
                elem.key = key + edgeGraphBW[dataInd][15];

                if (edgeGraphBW[dataInd][15] < threshold) {
                    Q.addElement(elem);
                }
            }

            if ((y1 < maxY) && (z1 > minZ) && (seen[ind + r2 - r22] <= 1)) {
                elem.value = ind + r2 - r22;
                elem.key = key + edgeGraphBW[dataInd][16];

                if (edgeGraphBW[dataInd][16] < threshold) {
                    Q.addElement(elem);
                }
            }

            if ((y1 < maxY) && (z1 < maxZ) && (seen[ind + r2 + r22] <= 1)) {
                elem.value = ind + r2 + r22;
                elem.key = key + edgeGraphBW[dataInd][17];

                if (edgeGraphBW[dataInd][17] < threshold) {
                    Q.addElement(elem);
                }
            }

            if ((x1 > minX) && (y1 > minY) && (z1 > minZ) && (seen[ind - 1 - r2 - r22] <= 1)) {
                elem.value = ind - 1 - r2 - r22;
                elem.key = key + edgeGraphBW[dataInd][18];

                if (edgeGraphBW[dataInd][18] < threshold) {
                    Q.addElement(elem);
                }
            }

            if ((x1 > minX) && (y1 > minY) && (z1 < maxZ) && (seen[ind - 1 - r2 + r22] <= 1)) {
                elem.value = ind - 1 - r2 + r22;
                elem.key = key + edgeGraphBW[dataInd][19];

                if (edgeGraphBW[dataInd][19] < threshold) {
                    Q.addElement(elem);
                }
            }

            if ((x1 > minX) && (y1 < maxY) && (z1 > minZ) && (seen[ind - 1 + r2 - r22] <= 1)) {
                elem.value = ind - 1 + r2 - r22;
                elem.key = key + edgeGraphBW[dataInd][20];

                if (edgeGraphBW[dataInd][20] < threshold) {
                    Q.addElement(elem);
                }
            }

            if ((x1 > minX) && (y1 < maxY) && (z1 < maxZ) && (seen[ind - 1 + r2 + r22] <= 1)) {
                elem.value = ind - 1 + r2 + r22;
                elem.key = key + edgeGraphBW[dataInd][21];

                if (edgeGraphBW[dataInd][21] < threshold) {
                    Q.addElement(elem);
                }
            }

            if ((x1 < maxX) && (y1 > minY) && (z1 > minZ) && (seen[ind + 1 - r2 - r22] <= 1)) {
                elem.value = ind + 1 - r2 - r22;
                elem.key = key + edgeGraphBW[dataInd][22];

                if (edgeGraphBW[dataInd][22] < threshold) {
                    Q.addElement(elem);
                }
            }

            if ((x1 < maxX) && (y1 > minY) && (z1 < maxZ) && (seen[ind + 1 - r2 + r22] <= 1)) {
                elem.value = ind + 1 - r2 + r22;
                elem.key = key + edgeGraphBW[dataInd][23];

                if (edgeGraphBW[dataInd][23] < threshold) {
                    Q.addElement(elem);
                }
            }

            if ((x1 < maxX) && (y1 < maxY) && (z1 > minZ) && (seen[ind + 1 + r2 - r22] <= 1)) {
                elem.value = ind + 1 + r2 - r22;
                elem.key = key + edgeGraphBW[dataInd][24];

                if (edgeGraphBW[dataInd][24] < threshold) {
                    Q.addElement(elem);
                }
            }

            if ((x1 < maxX) && (y1 < maxY) && (z1 < maxZ) && (seen[ind + 1 + r2 + r22] <= 1)) {
                elem.value = ind + 1 + r2 + r22;
                elem.key = key + edgeGraphBW[dataInd][25];

                if (edgeGraphBW[dataInd][25] < threshold) {
                    Q.addElement(elem);
                }
            }

        }

        return count;
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

        for (i = 0; i < totalLength; i++) {
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
            queue[i].value = e.value;
            queue[i].origin = e.origin;

            while ((i > 0) && (queue[i].key < queue[j = ((i - 1) >> 1)].key)) {

                // push x up the tree by exchanging it with its parent
                temp.key = queue[i].key;
                temp.value = queue[i].value;
                temp.origin = queue[i].origin;
                queue[i].key = queue[j].key;
                queue[i].value = queue[j].value;
                queue[i].origin = queue[j].origin;
                queue[j].key = temp.key;
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
            e.value = queue[0].value;
            e.origin = queue[0].origin;
            places[e.value] = -1;
            --last;

            if (last == 0) {
                return e;
            }

            queue[0].key = queue[last].key;
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
                    temp.value = queue[i].value;
                    temp.origin = queue[i].origin;
                    queue[i].key = queue[j].key;
                    queue[i].value = queue[j].value;
                    queue[i].origin = queue[j].origin;
                    queue[j].key = temp.key;
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
