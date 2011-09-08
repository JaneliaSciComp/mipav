package gov.nih.mipav.model.algorithms.filters;


import gov.nih.mipav.model.algorithms.*;
import gov.nih.mipav.model.structures.*;

import java.io.*;

import java.util.*;


/**
 * Algorithm to apply a median filter to an image, placing it in a new ModelImage, or returning the changed picture to
 * the same image. Algorithm takes values in the neighborhood that correspond to the "on" values in the kernel (mask)
 * and places them into a sorted list. If the variance (square of standard Deviation) isn't 0 and the pixel's value is
 * outside the bounds given by the given fraction of the standard deviation, then the median of the list is computed and
 * placed into that element of the image. If the variance is 0, the median of the list is always computed and placed
 * into that element of the image.
 * 
 * Color images can use either component filtering or vector filtering.  In vector filtering
 * the red, green, and blue are all fitered at once; that is, the new pixel value will be
 * selected from red, green, and blue from one pixel in the neighborhood.  With vector 
 * filtering the variance is always set to 0, and the vector median is always used.
 *
 * <p>If any element in the kernel which does not have a corresponding element in the image, then the pixel is
 * unfiltered. This results in a border of unfiltered pixels in the image 1/2 of the kernel-size large.</p>
 * 
 * When adaptiveSize is true, the Adaptive median filter discussed in Digital Image Processing Second Edition
 * by Rafael C. Gonzalez and Richard E. Woods in Chapter 5.3 pages 241-243 is implemented.  It has 3 main purposes: 1. remove salt and pepper (impulse) noise 2. provide smoothening of other noise that may
 * not be impulsive 3. reduce distortion such as excessive thinning or thickening of object bounderies. Depending on 
 * certain conditions, the kernel size can incresase during the filtering operation. 
 * 
 *
 * @version  1.0; 17 February 2000
 * @author   David Parsons (parsonsd)
 * @author   Matthew J. McAuliffe, Ph.D.
 * @author   Paul F. Hemler, Ph.D.
 */
public class AlgorithmMedian extends AlgorithmBase {

    //~ Static fields/initializers -------------------------------------------------------------------------------------

    /** square kernel (2D only). */
    public static final int SQUARE_KERNEL = 0;

    /** (3D only). */
    public static final int CUBE_KERNEL = 0;

    /** cross (2D only). */
    public static final int CROSS_KERNEL = 1;

    /** (3D only). */
    public static final int AXIAL_KERNEL = 1;

    /** X-shaped kernel, from 1 corner to opposite corner. */
    public static final int X_KERNEL = 2;

    /** horizontal (2D only). */
    public static final int HORZ_KERNEL = 3;

    /** vertical (2D only). */
    public static final int VERT_KERNEL = 4;
    
    public static final int COMPONENT_FILTER = 1;
    
    public static final int VECTOR_MAGNITUDE_FILTER = 2;
    
    public static final int VECTOR_DIRECTION_FILTER = 3;

    //~ Instance fields ------------------------------------------------------------------------------------------------

    /** Border buffer number of slices. */
    int bdrBufferDepth;

    /** Border buffer number of rows. */
    int bdrBufferHeight;

    /** Border buffer number of columns. */
    int bdrBufferWidth;

    /** Source buffer number of slices. */
    int srcBufferDepth;

    /** Source buffer number of rows. */
    int srcBufferHeight;

    /** Source buffer number of columns. */
    int srcBufferWidth;

    /** the blue channel. */
    private boolean bChannel = true;

    /** Used for 3D loop control. */
    private int currentSlice = 0; //

    /** true means apply to entire image, false only region. */
    private boolean entireImage;

    /** the green channel. */
    private boolean gChannel = true;

    /** The kernel radius. */
    private int halfK[];

    /** indicates the image is a color image. */
    private boolean isColorImage = false;

    /** number of times to filter the image. */
    private int iterations;

    /** mask to determine the region of pixels used in a median filter. */
    private byte[][] kernel; // kernelNumber is first index

    /** The index of the kernel center. */
    private int kernelCenter[];

    /** The kernel. */
    private float[][] kernelMask;
    private float[][] kernelVectorMask;

    /** user-selectable shape of the region for neighbor-selection. */
    private int kernelShape;
    
    /** Smallest kernel size used in adaptive filtering */
    private int minimumSize;

    /** dimension of the kernel (ie., 5 = 5x5, 7 = 7x7, 9 = 9x9, etc.). */
    private int kernelSize[];
    
    /** If true adaptively changes the size of the kernel mask */
    private boolean adaptiveSize = false;
    
   /** When adaptiveSize is true, the maximum size the kernel mask can be increased to. */
    private int maximumSize;
    
    /** Number of kernels used, more than 1 if adaptive filtering is used */
    private int kernelNumber = 1;

    /** contains VOI. */
    private BitSet mask = null;

    /** The kernel. */
    private int maskCenter[];

    /** Used for 3D loop control. */
    private int numberOfSlices;

    /** // if T, filter the red channel. */
    private boolean rChannel = true;

    /** do all filtering slice-by-slice, rather than as a volume. */
    private boolean sliceFiltering;

    /** only filter pixels with a value further than stdDevLimit from the mean. */
    private float stdDevLimit;

    /** number of elements in a pixel. Monochrome = 1, Color = 4. (a, R, G, B) */
    private int valuesPerPixel = 1;
    
    /** Either COMPONENT_FILTER, VECTOR_MAGNITUDE_FILTER, or VECTOR_DIRECTION_FILTER
     *  For a vector filter the new red, green, and blue will all come from the same pixel. */
    private int filterType = COMPONENT_FILTER;

    //~ Constructors ---------------------------------------------------------------------------------------------------

    /**
     * Constructor for 2D images in which changes are returned to the source image.
     *
     * @param  srcImg    Source image model.
     * @param  iters     Number of interations of the median filter.
     * @param  kSize     Kernel size: dimension of the kernel (ie., 5 = 5x5, 7 = 7x7, 9 = 9x9, etc.).
     * @param  kShape    Kernel shape: element neighbors to include when finding the median.
     * @param  stdDev    Inner-bounds by which to process pixels (pixel values outside this bound will be median
     *                   filtered).
     * @param  adaptiveSize If true, adaptively changes the size of the kernel mask
     * @param  maximumSize If adaptiveSize is true, the maximum size the kernel mask can be increased to.
     * @param  maskFlag  Flag that indicates that the median filtering will be performed for the whole image if equal to
     *                   true.
     */
    public AlgorithmMedian(ModelImage srcImg, int iters, int kSize, int kShape, float stdDev, 
                           boolean adaptiveSize, int maximumSize, boolean maskFlag) {
        super(null, srcImg);

        if (srcImg.isColorImage()) {
            isColorImage = true;
            valuesPerPixel = 4;
        }

        // else, already false
        entireImage = maskFlag;
        iterations = iters;
        minimumSize = kSize; // dimension of the kernel
        kernelShape = kShape; // set up the mask (kernel) used to filter
        stdDevLimit = stdDev; // inside magnitude bounds of pixel value to adjust
        this.adaptiveSize = adaptiveSize;
        this.maximumSize = maximumSize;
        sliceFiltering = true; // as a default--though a different value doesn't make much sense in a 2D application.
        numberOfSlices = 1; // 2D images may only have 1 slice.
        makeKernel();

        // PFH start section 6/28/2004
        // makeKernel sets halfK, which we now use
        srcBufferWidth = srcImage.getExtents()[0];
        srcBufferHeight = srcImage.getExtents()[1];
        srcBufferDepth = 1;

        // allow for a border of halfK around the source image
        bdrBufferWidth = srcBufferWidth + (2 * halfK[kernelNumber-1]);
        bdrBufferHeight = srcBufferHeight + (2 * halfK[kernelNumber-1]);
        bdrBufferDepth = 1;
        // PFH end section 6/28/2004

        if (entireImage == false) {
            mask = srcImage.generateVOIMask();
        }
    }

    /**
     * Constructor for 2D images in which changes are placed in a predetermined destination image.
     *
     * @param  destImg   Image model where result image is stored.
     * @param  srcImg    Source image model.
     * @param  iters     Number of interations of the median filter.
     * @param  kSize     Kernel size: dimension of the kernel (ie., 5 = 5x5, 7 = 7x7, 9 = 9x9, etc.).
     * @param  kShape    Kernel shape: element neighbors to include when finding the median.
     * @param  stdDev    Inner-bounds by which to process pixels (pixel values outside this bound will be median
     *                   filtered).
     * @param  adaptiveSize If true, adaptively changes the size of the kernel mask
     * @param  maximumSize If adaptiveSize is true, the maximum size the kernel mask can be increased to.
     * @param  maskFlag  Flag that indicates that the median filtering will be performed for the whole image if equal to
     *                   true.
     */
    public AlgorithmMedian(ModelImage destImg, ModelImage srcImg, int iters, int kSize, int kShape, float stdDev,
                           boolean adaptiveSize, int maximumSize, boolean maskFlag) {

        super(destImg, srcImg);

        if (srcImg.isColorImage()) {
            isColorImage = true;
            valuesPerPixel = 4;
        }

        // else, already false
        entireImage = maskFlag;
        iterations = iters;
        minimumSize = kSize; // dimension of the kernel
        kernelShape = kShape; // set up the mask (kernel) used to filter
        stdDevLimit = stdDev; // inside magnitude bounds of pixel value
        this.adaptiveSize = adaptiveSize;
        this.maximumSize = maximumSize;
        sliceFiltering = true; // as a default--this doesn't make much sense in a 2D application.
        numberOfSlices = 1; // 2D images may only have 1 slice.
        makeKernel();

        // makeKernel sets halfK, which we now use
        srcBufferWidth = srcImage.getExtents()[0];
        srcBufferHeight = srcImage.getExtents()[1];
        srcBufferDepth = 1;

        // allow for a border of halfK around the source image
        bdrBufferWidth = srcBufferWidth + (2 * halfK[kernelNumber-1]);
        bdrBufferHeight = srcBufferHeight + (2 * halfK[kernelNumber-1]);
        bdrBufferDepth = 1;

        if (entireImage == false) {
            mask = srcImage.generateVOIMask();
        }
    }

    /**
     * Constructor for 3D images in which changes are returned to the source image.
     *
     * @param  srcImg        Source image model.
     * @param  iters         Number of interations of the median filter.
     * @param  kSize         Kernel size: dimension of the kernel (ie., 5 = 5x5, 7 = 7x7, 9 = 9x9, etc.).
     * @param  kShape        Kernel shape: element neighbors to include when finding the median.
     * @param  stdDev        Inner-bounds by which to process pixels (pixel values outside this bound will be median
     *                       filtered).
     * @param  adaptiveSize If true, adaptively changes the size of the kernel mask
     * @param  maximumSize If adaptiveSize is true, the maximum size the kernel mask can be increased to.
     * @param  sliceBySlice  Each slice in a volume image is to be filtered separately (when true), else the volume will
     *                       use a kernel with 3 dimensions.
     * @param  maskFlag      Flag that indicates that the median filtering will be performed for the whole image if
     *                       equal to true.
     */
    public AlgorithmMedian(ModelImage srcImg, int iters, int kSize, int kShape, float stdDev, 
                           boolean adaptiveSize, int maximumSize, boolean sliceBySlice,
                           boolean maskFlag) {
        super(null, srcImg);

        if (srcImg.isColorImage()) {
            isColorImage = true;
            valuesPerPixel = 4;
        }

        // else, already false
        entireImage = maskFlag;
        iterations = iters;
        minimumSize = kSize; // dimension of the kernel
        kernelShape = kShape; // set up the mask (kernel) used to filter
        stdDevLimit = stdDev; // inside magnitude bounds of pixel value
        this.adaptiveSize = adaptiveSize;
        this.maximumSize = maximumSize;
        sliceFiltering = sliceBySlice;
        numberOfSlices = srcImage.getExtents()[2];
        makeKernel();

        // makeKernel sets halfK, which we now use to set dimensions
        // of the source and border buffer
        srcBufferWidth = srcImage.getExtents()[0];
        srcBufferHeight = srcImage.getExtents()[1];
        srcBufferDepth = srcImage.getExtents()[2];

        bdrBufferWidth = srcBufferWidth + (2 * halfK[kernelNumber-1]);
        bdrBufferHeight = srcBufferHeight + (2 * halfK[kernelNumber-1]);

        if (sliceFiltering == true) {

            // do not add extra slices for slice by slice filtering
            bdrBufferDepth = srcBufferDepth;
        } else {

            // add extra slices for 3D filtering
            bdrBufferDepth = srcBufferDepth + (2 * halfK[kernelNumber-1]);
        }

        if (entireImage == false) {
            mask = srcImage.generateVOIMask();
        }
    }

    /**
     * Constructor for 3D images in which changes are placed in a predetermined destination image.
     *
     * @param  destImg       Image model where result image is stored.
     * @param  srcImg        Source image model.
     * @param  iters         Number of interations of the median filter.
     * @param  kSize         Kernel size: dimension of the kernel (ie., 5 = 5x5, 7 = 7x7, 9 = 9x9, etc.).
     * @param  kShape        Kernel shape: element neighbors to include when finding the median.
     * @param  stdDev        Inner-bounds by which to process pixels (pixel values outside this bound will be median
     *                       filtered).
     * @param  adaptiveSize If true, adaptively changes the size of the kernel mask
     * @param  maximumSize If adaptiveSize is true, the maximum size the kernel mask can be increased to.
     * @param  sliceBySlice  Each slice in a volume image is filtered separately (when true), else the volume will use a
     *                       kernel with 3 dimensions.
     * @param  maskFlag      Flag that indicates that the median filtering will be performed for the whole image if
     *                       equal to true.
     */
    public AlgorithmMedian(ModelImage destImg, ModelImage srcImg, int iters, int kSize, int kShape, float stdDev,
                           boolean  adaptiveSize, int maximumSize, boolean sliceBySlice, boolean maskFlag) {

        super(destImg, srcImg);

        if (srcImg.isColorImage()) {
            isColorImage = true;
            valuesPerPixel = 4;
        }

        // else, already false
        entireImage = maskFlag;
        iterations = iters;
        minimumSize = kSize; // dimension of the kernel
        kernelShape = kShape; // set up the mask (kernel) used to filter
        stdDevLimit = stdDev; // inside magnitude bounds of pixel value
        this.adaptiveSize = adaptiveSize;
        this.maximumSize = maximumSize;
        sliceFiltering = sliceBySlice;
        numberOfSlices = srcImage.getExtents()[2];
        makeKernel();

        // makeKernel sets halfK, which we now use to set dimensions
        // of the source and border buffer
        srcBufferWidth = srcImage.getExtents()[0];
        srcBufferHeight = srcImage.getExtents()[1];
        srcBufferDepth = srcImage.getExtents()[2];

        bdrBufferWidth = srcBufferWidth + (2 * halfK[kernelNumber-1]);
        bdrBufferHeight = srcBufferHeight + (2 * halfK[kernelNumber-1]);

        if (sliceFiltering == true) {

            // do not add extra slices for slice by slice filtering
            bdrBufferDepth = srcBufferDepth;
        } else {

            // add extra slices for 3D filtering
            bdrBufferDepth = srcBufferDepth + (2 * halfK[kernelNumber-1]);
        }

        if (entireImage == false) {
            mask = srcImage.generateVOIMask();
        }
    }

    //~ Methods --------------------------------------------------------------------------------------------------------

    /**
     * Prepares this class for destruction.
     */
    public void finalize() {
        mask = null;
        kernel = null;
        kernelMask = null;
        destImage = null;
        srcImage = null;
        super.finalize();
    }

    /**
     * Starts the algorithm.
     */
    public void runAlgorithm() {

        if (srcImage == null) {
            displayError("Source Image is null");

            return;
        }

        

        fireProgressStateChanged(srcImage.getImageName(), "Filtering ...");
        
        if (destImage != null) { // if there exists a destination image

            if (srcImage.getNDims() == 2) {
                calcStoreInDestBorder2D();
            } else if (srcImage.getNDims() > 2) {
                calcStoreInDestBorder3D();
            }
        } else { // there is no image but the original source.

            if (srcImage.getNDims() == 2) {
                calcInPlaceBorder2D();
            } else if (srcImage.getNDims() > 2) {
                calcInPlaceBorder3D();
            }
        }
    }

    /**
     * RGB images are median filtered by 'channel.' That is, each color, red, blue and green, is filtered independently
     * of the other two colors. This median filter permits selectively filtering any combination of the three channels
     * instead of simply filtering all three.
     *
     * @param  filterType Either COMPONENT_FILTER, VECTOR_MAGNITUDE_FILTER, or VECTOR_DIRECTION_FILTER
     * @param  r  Filter red channel.
     * @param  g  Filter green channel.
     * @param  b  Filter blue channel.
     */
    public void setRGBChannelFilter(int filterType, boolean r, boolean g, boolean b) {

        if (isColorImage) { // just in case somebody called for a mono image
            this.filterType = filterType;
            rChannel = r;
            gChannel = g;
            bChannel = b;
        }
    }

    /**
     * Median filters the source image. Replaces the original image with the filtered image.
     */
    private void calcInPlace2D() {

        int length; // total number of data-elements (pixels) in image
        float[] buffer; // data-buffer (for pixel data) which is the "heart" of the image
        float[] resultBuffer; // copy-to buffer (for pixel data) for image-data after filtering

        try {

            if (!isColorImage) {

                // image length is length in 2 dims
                length = srcImage.getExtents()[0] * srcImage.getExtents()[1];
            } else { // if (isColorImage) {

                // image length is length in 2 dims
                // by 4 color elements per pixel
                length = srcImage.getExtents()[0] * srcImage.getExtents()[1] * 4; // 1 each for ARGB
            }

            buffer = new float[length];
            resultBuffer = new float[length];
            srcImage.exportData(0, length, buffer); // locks and releases lock
        } catch (IOException error) {
            buffer = null;
            resultBuffer = null;
            errorCleanUp("Algorithm Median: source image locked", true);

            return;
        } catch (OutOfMemoryError e) {
            buffer = null;
            resultBuffer = null;
            errorCleanUp("Algorithm Median reports: Out of memory when creating image buffer", true);

            return;
        }

        if (isColorImage && (filterType == VECTOR_MAGNITUDE_FILTER)) {
            this.sliceVectorMagnitudeFilter(buffer, resultBuffer, 0, "image");    
        }
        else if (isColorImage && (filterType == VECTOR_DIRECTION_FILTER)) {
            this.sliceVectorDirectionFilter(buffer, resultBuffer, 0, "image");    
        }
        else {
            this.sliceFilter(buffer, resultBuffer, 0, "image"); // filter this slice
        }
        if (threadStopped) {
            finalize();

            return;
        }

        try { // place buffer data into the image
            srcImage.importData(0, resultBuffer, true);
        } catch (IOException error) {
            buffer = null;
            resultBuffer = null;
            errorCleanUp("Algorithm Median: Source image locked", true);

            return;
        }

        setCompleted(true);
    }

    /**
     * Median filters the source image and replaces the source image with the median filtered image.
     */
    private void calcInPlace3D() {

        int imageSliceLength = valuesPerPixel * srcImage.getSliceSize();
        int length;
        float[] buffer;
        float[] resultBuffer;

        try {

            if (!isColorImage) {

                // image length is length in 3 dims
                length = srcImage.getSliceSize() * srcImage.getExtents()[2];
            } else { // if (isColorImage) {

                // image length is length in 3 dims
                // by 4 color elements per pixel
                length = srcImage.getExtents()[0] * srcImage.getExtents()[1] * srcImage.getExtents()[2] * 4; // 1 each for ARGB
            }

            buffer = new float[length];
            resultBuffer = new float[length];
            srcImage.exportData(0, length, buffer); // locks and releases lock
            
        } catch (IOException error) {
            buffer = null;
            resultBuffer = null;
            errorCleanUp("Algorithm Median: Source image locked", true);

            return;
        } catch (OutOfMemoryError e) {
            buffer = null;
            resultBuffer = null;
            errorCleanUp("Algorithm Median: Out of memory", true);

            return;
        }

        if (sliceFiltering) {

            for (currentSlice = 0; (currentSlice < numberOfSlices) && !threadStopped; currentSlice++) {
                if (isColorImage && (filterType == VECTOR_MAGNITUDE_FILTER)) {
                    sliceVectorMagnitudeFilter(buffer, resultBuffer, currentSlice * imageSliceLength,
                            "slice " + String.valueOf(currentSlice + 1));
                }
                else if (isColorImage && (filterType == VECTOR_DIRECTION_FILTER)) {
                    sliceVectorDirectionFilter(buffer, resultBuffer, currentSlice * imageSliceLength,
                            "slice " + String.valueOf(currentSlice + 1));
                }
                else {
                    sliceFilter(buffer, resultBuffer, currentSlice * imageSliceLength,
                            "slice " + String.valueOf(currentSlice + 1));
                }
            }
        } else { // volume kernel requested

            if (isColorImage && (filterType == VECTOR_MAGNITUDE_FILTER)) {
                volumeVectorMagnitudeColorFilter(buffer, resultBuffer);
            }
            else if (isColorImage && (filterType == VECTOR_DIRECTION_FILTER)) {
                volumeVectorDirectionColorFilter(buffer, resultBuffer);
            }
            else if (isColorImage) {
                volumeColorFilter(buffer, resultBuffer);
            } else {
                volumeFilter(buffer, resultBuffer);
            }
        }

        if (threadStopped) {
            finalize();

            return;
        }

        try {
            srcImage.importData(0, resultBuffer, true);
        } catch (IOException error) {
            buffer = null;
            resultBuffer = null;
            errorCleanUp("Algorithm Median: source image locked", true);
            setThreadStopped(true);

            return;
        }

        setCompleted(true);
    }

    /**
     * Replaces the original image with the Median filtered image.
     *
     * <p>Make a copy of the srcImage in a buffer with borders for the kernel. Copy the closest rows and columns data
     * into the added borders.</p>
     */
    private void calcInPlaceBorder2D() {

        if (isColorImage) {

            // I don't know how to deal with color images now, so call the
            // original routine.
            calcInPlace2D();

            return;
        }

        int srcBufferLength = srcBufferWidth * srcBufferHeight;
        float[] srcBuffer;
        float[] resultBuffer;

        try {
            srcBuffer = new float[srcBufferLength];
            resultBuffer = new float[srcBufferLength];
            srcImage.exportData(0, srcBufferLength, srcBuffer); // locks and releases lock
        } catch (IOException error) {
            srcBuffer = null;
            resultBuffer = null;
            errorCleanUp("Algorithm Mode: source image locked", true);

            return;
        } catch (OutOfMemoryError e) {
            srcBuffer = null;
            resultBuffer = null;
            errorCleanUp("Algorithm Mode reports: Out of memory when creating image buffer", true);

            return;
        }

        int bdrBufferLength = bdrBufferWidth * bdrBufferHeight;

        if (isColorImage) {
            bdrBufferLength *= 4;
        }

        float[] bdrBuffer;

        try {
            bdrBuffer = new float[bdrBufferLength];
        } catch (OutOfMemoryError e) {
            srcBuffer = null;
            bdrBuffer = null;
            resultBuffer = null;
            errorCleanUp("Algorithm Mode reports: Out of memory when creating border buffer", true);

            return;
        }
        
        // copy image data from buffer into borderBuffer
        copy2DSrcBufferToBdrBuffer(srcBuffer, bdrBuffer, 0, 0);

        fireProgressStateChanged(10);
        
        this.sliceFilterBorder(bdrBuffer, resultBuffer, 0, 0); // filter this slice

        fireProgressStateChanged("Importing result ...");
        fireProgressStateChanged(90);

        if (threadStopped) {
            finalize();

            return;
        }

        try { // place buffer data into the image
            srcImage.importData(0, resultBuffer, true);
        } catch (IOException error) {
            srcBuffer = null;
            bdrBuffer = null;
            resultBuffer = null;
            errorCleanUp("Algorithm Mode: Source image locked", true);

            return;
        }
        fireProgressStateChanged(100, null, "Importing result ...");

        setCompleted(true);
    }

    /**
     * Mode filters the source image and replaces the source image with the mode filtered image.
     */
    private void calcInPlaceBorder3D() {

        if (isColorImage) {

            // I don't know how to deal with color images now, so call the
            // original routine.
            calcInPlace3D();

            return;
        }

        int srcBufferLength = srcBufferWidth * srcBufferHeight * srcBufferDepth;
        float[] srcBuffer;
        float[] resultBuffer;

        try {
            srcBuffer = new float[srcBufferLength];
            resultBuffer = new float[srcBufferLength];
            srcImage.exportData(0, srcBufferLength, srcBuffer); // locks and releases lock
        } catch (IOException error) {
            srcBuffer = null;
            resultBuffer = null;
            errorCleanUp("Algorithm Mode: Source image locked", true);

            return;
        } catch (OutOfMemoryError e) {
            srcBuffer = null;
            resultBuffer = null;
            errorCleanUp("Algorithm Mode: Out of memory", true);

            return;
        }

        int bdrBufferLength = bdrBufferWidth * bdrBufferHeight * bdrBufferDepth;
        float[] bdrBuffer;

        try {
            bdrBuffer = new float[bdrBufferLength];
        } catch (OutOfMemoryError e) {
            srcBuffer = null;
            bdrBuffer = null;
            resultBuffer = null;
            errorCleanUp("Algorithm Mode: Out of memory", true);

            return;
        }


        // copy image data from srcBuffer into bdrBuffer
        copy3DSrcBufferToBdrBuffer(srcBuffer, bdrBuffer);

        int srcSliceLength = srcBufferWidth * srcBufferHeight;
        int bdrSliceLength = bdrBufferWidth * bdrBufferHeight;

        fireProgressStateChanged(10);

        if (sliceFiltering) {

            for (currentSlice = 0; (currentSlice < numberOfSlices) && !threadStopped; currentSlice++) {
            	fireProgressStateChanged(10 + (80 *currentSlice) / (srcBufferDepth - 1));
               

                sliceFilterBorder(bdrBuffer, resultBuffer, currentSlice * bdrSliceLength,
                                  currentSlice * srcSliceLength);
            }
        } else { // volume kernel requested
            volumeFilterBorder(bdrBuffer, resultBuffer);
        }
        
        fireProgressStateChanged("Importing result");
        fireProgressStateChanged(90);

        if (threadStopped) {
            finalize();

            return;
        }

        try {
            srcImage.importData(0, resultBuffer, true);
        } catch (IOException error) {
            srcBuffer = null;
            resultBuffer = null;
            errorCleanUp("Algorithm Mode: source image locked", true);
            setThreadStopped(true);

            return;
        }

        setCompleted(true);
    }

    /**
     * This function produces a new image that has been median filtered and places filtered image in the destination
     * image.
     */
    private void calcStoreInDest2D() {

        int length; // total number of data-elements (pixels) in image
        float[] buffer; // data-buffer (for pixel data) which is the "heart" of the image
        float[] resultBuffer; // copy-to buffer (for pixel data) for image-data after filtering

        try {
            destImage.setLock(ModelStorageBase.RW_LOCKED);
        } catch (IOException error) {
            errorCleanUp("Algorithm Median reports: destination image locked", false);

            return;
        }

        try {

            if (!isColorImage) {

                // image length is length in 2 dims
                length = srcImage.getExtents()[0] * srcImage.getExtents()[1];
            } else { // if (isColorImage) {

                // image length is length in 2 dims
                // by 4 color elements per pixel
                length = srcImage.getExtents()[0] * srcImage.getExtents()[1] * 4; // 1 each for ARGB
            }

            buffer = new float[length];
            resultBuffer = new float[length];
            srcImage.exportData(0, length, buffer); // locks and releases lock
        } catch (IOException error) {
            buffer = null;
            resultBuffer = null;
            errorCleanUp("Algorithm Median reports: source image locked", true);

            return;
        } catch (OutOfMemoryError e) {
            buffer = null;
            resultBuffer = null;
            errorCleanUp("Algorithm Median reports: out of memory", true);

            return;
        }


        if (isColorImage && (filterType == VECTOR_MAGNITUDE_FILTER)) {
            sliceVectorMagnitudeFilter(buffer, resultBuffer, 0, "image");    
        }
        else if (isColorImage && (filterType == VECTOR_DIRECTION_FILTER)) {
            sliceVectorDirectionFilter(buffer, resultBuffer, 0, "image");    
        }
        else {
            sliceFilter(buffer, resultBuffer, 0, "image"); // filter image based on provided info.
        }
        destImage.releaseLock(); // we didn't want to allow the image to be adjusted by someone else
       
        if (threadStopped) {
            finalize();

            return;
        }

        try { // but now place buffer data into the image
            destImage.importData(0, resultBuffer, true);
        } catch (IOException error) {
            buffer = null;
            resultBuffer = null;
            errorCleanUp("Algorithm Median reports: destination image still locked", true);

            return;
        }

        setCompleted(true);
    }

    /**
     * This function produces a new volume image that has been median filtered. Image can be filtered by filtering each
     * slice individually, or by filtering using a kernel-volume.
     */
    private void calcStoreInDest3D() {

        int length;
        int imageSliceLength = valuesPerPixel * srcImage.getSliceSize(); // cover case of color image
        float[] buffer;
        float[] resultBuffer;

        try {
            destImage.setLock(ModelStorageBase.RW_LOCKED);
        } catch (IOException error) {
            errorCleanUp("Algorithm Median reports: destination image locked", false);

            return;
        }

        try {

            if (!isColorImage) {

                // image length is length in 3 dims
                length = srcImage.getExtents()[0] * srcImage.getExtents()[1] * srcImage.getExtents()[2];
            } else { // if (isColorImage) {

                // image length is length in 3 dims
                // by 4 color elements per pixel
                length = srcImage.getExtents()[0] * srcImage.getExtents()[1] * srcImage.getExtents()[2] * 4; // 1 each for ARGB
            }

            buffer = new float[length];
            srcImage.exportData(0, length, buffer); // locks and releases lock
            
        } catch (IOException error) {
            buffer = null;
            resultBuffer = null;
            errorCleanUp("Algorithm Median: source image locked", true);

            return;
        } catch (OutOfMemoryError e) {
            buffer = null;
            resultBuffer = null;
            errorCleanUp("Algorithm Median: Out of memory creating process buffer", true);

            return;
        }

        try {
            resultBuffer = new float[length];
        } catch (OutOfMemoryError e) {
            buffer = null;
            resultBuffer = null;
            errorCleanUp("Algorithm Median reports: Out of memory because of resultBuffer", true);

            return;
        }

        if (sliceFiltering) {

            for (currentSlice = 0; (currentSlice < numberOfSlices) && !threadStopped; currentSlice++) {
                if (isColorImage && (filterType == VECTOR_MAGNITUDE_FILTER)) {
                    sliceVectorMagnitudeFilter(buffer, resultBuffer, currentSlice * imageSliceLength,
                            "slice " + String.valueOf(currentSlice + 1));
                }
                else if (isColorImage && (filterType == VECTOR_DIRECTION_FILTER)) {
                    sliceVectorDirectionFilter(buffer, resultBuffer, currentSlice * imageSliceLength,
                            "slice " + String.valueOf(currentSlice + 1));
                }
                else {
                    sliceFilter(buffer, resultBuffer, currentSlice * imageSliceLength,
                            "slice " + String.valueOf(currentSlice + 1));
                }
            }
        } else { // requested volume filter

            if ((filterType == VECTOR_MAGNITUDE_FILTER) && isColorImage) {
                volumeVectorMagnitudeColorFilter(buffer, resultBuffer);
            }
            else if ((filterType == VECTOR_DIRECTION_FILTER) && isColorImage) {
                volumeVectorDirectionColorFilter(buffer, resultBuffer);
            }
            else if (isColorImage) { // for color image
                volumeColorFilter(buffer, resultBuffer);
            } else { // for mono image
                volumeFilter(buffer, resultBuffer);
                // borderVolumeFilter(buffer, resultBuffer);
            }
        }

        destImage.releaseLock();

        if (threadStopped) {
            finalize();

            return;
        }

        try {
            destImage.importData(0, resultBuffer, true);
        } catch (IOException e) {
            buffer = null;
            resultBuffer = null;
            errorCleanUp("Algorithm Median reports: destination image still locked", true);

            return;
        }

        setCompleted(true);
    }

    /**
     * This method produces a new image that has been mode filtered and places filtered image in the destination image.
     */
    private void calcStoreInDestBorder2D() {

        if (isColorImage) {

            // I don't know how to deal with color images now, so call the
            // original routine.
            calcStoreInDest2D();

            return;
        }

        try {
            destImage.setLock(ModelStorageBase.RW_LOCKED);
        } catch (IOException error) {
            errorCleanUp("Algorithm Median reports: destination image locked", false);

            return;
        }

        int srcBufferLength = srcBufferWidth * srcBufferHeight;
        
        float[] srcBuffer;
        float[] resultBuffer;

        try {
            srcBuffer = new float[srcBufferLength];
            resultBuffer = new float[srcBufferLength];
            srcImage.exportData(0, srcBufferLength, srcBuffer); // locks and releases lock
        } catch (IOException error) {
            srcBuffer = null;
            resultBuffer = null;
            errorCleanUp("Algorithm Mode: source image locked", true);

            return;
        } catch (OutOfMemoryError e) {
            srcBuffer = null;
            resultBuffer = null;
            errorCleanUp("Algorithm Mode reports: Out of memory when creating image buffer", true);

            return;
        }

        int bdrBufferLength = bdrBufferWidth * bdrBufferHeight;

        if (isColorImage) {
            bdrBufferLength *= 4;
        }

        float[] bdrBuffer;

        try {
            bdrBuffer = new float[bdrBufferLength];
        } catch (OutOfMemoryError e) {
            srcBuffer = null;
            bdrBuffer = null;
            resultBuffer = null;
            errorCleanUp("Algorithm Mode reports: Out of memory when creating border buffer", true);

            return;
        }



        // copy image data from buffer into borderBuffer
        copy2DSrcBufferToBdrBuffer(srcBuffer, bdrBuffer, 0, 0);

        fireProgressStateChanged(10);

        this.sliceFilterBorder(bdrBuffer, resultBuffer, 0, 0); // filter this slice

        destImage.releaseLock(); // we didn't want to allow the image to be adjusted by someone else

        fireProgressStateChanged("Importing result ...");
        fireProgressStateChanged(90);

        if (threadStopped) {
            finalize();

            return;
        }

        try { // place buffer data into the image
            destImage.importData(0, resultBuffer, true);
        } catch (IOException error) {
            srcBuffer = null;
            bdrBuffer = null;
            resultBuffer = null;
            errorCleanUp("Algorithm Mode: Source image locked", true);

            return;
        }
        fireProgressStateChanged(100, null, null);

        setCompleted(true);
    }

    /**
     * Median filters the source image and replaces the source image with the mode filtered image.
     */
    private void calcStoreInDestBorder3D() {
    	
        if (isColorImage) {

            // I don't know how to deal with color images now, so call the
            // original routine.
            calcStoreInDest3D();

            return;
        }

        try {
            destImage.setLock(ModelStorageBase.RW_LOCKED);
        } catch (IOException error) {
            errorCleanUp("Algorithm Mode reports: destination image locked", false);

            return;
        }

        int srcBufferLength = srcBufferWidth * srcBufferHeight * srcBufferDepth;
        float[] srcBuffer;
        float[] resultBuffer;

        try {
            srcBuffer = new float[srcBufferLength];
            resultBuffer = new float[srcBufferLength];
            srcImage.exportData(0, srcBufferLength, srcBuffer); // locks and releases lock
        } catch (IOException error) {
            srcBuffer = null;
            resultBuffer = null;
            errorCleanUp("Algorithm Mode: Source image locked", true);

            return;
        } catch (OutOfMemoryError e) {
            srcBuffer = null;
            resultBuffer = null;
            errorCleanUp("Algorithm Mode: Out of memory", true);

            return;
        }

        int bdrBufferLength = bdrBufferWidth * bdrBufferHeight * bdrBufferDepth;
        float[] bdrBuffer;

        try {
            bdrBuffer = new float[bdrBufferLength];
        } catch (OutOfMemoryError e) {
            srcBuffer = null;
            bdrBuffer = null;
            resultBuffer = null;
            errorCleanUp("Algorithm Mode: Out of memory", true);

            return;
        }

        // copy image data from srcBuffer into bdrBuffer
        copy3DSrcBufferToBdrBuffer(srcBuffer, bdrBuffer);

        int srcSliceLength = srcBufferWidth * srcBufferHeight;
        int bdrSliceLength = bdrBufferWidth * bdrBufferHeight;

        fireProgressStateChanged(10);

        if (sliceFiltering) {

            for (currentSlice = 0; (currentSlice < numberOfSlices) && !threadStopped; currentSlice++) {
                fireProgressStateChanged(10 + (80 *currentSlice) / (srcBufferDepth - 1));
              
                sliceFilterBorder(bdrBuffer, resultBuffer, currentSlice * bdrSliceLength,
                                  currentSlice * srcSliceLength);
            }
        } else { // volume kernel requested
            volumeFilterBorder(bdrBuffer, resultBuffer);
        }
        
        fireProgressStateChanged("Importing result");
        fireProgressStateChanged(90);

        destImage.releaseLock(); // we didn't want to allow the image to be adjusted by someone else
     
        if (threadStopped) {
            finalize();

            return;
        }

        try {
            destImage.importData(0, resultBuffer, true);
        } catch (IOException error) {
            srcBuffer = null;
            bdrBuffer = null;
            resultBuffer = null;
            errorCleanUp("Algorithm Mode: source image locked", true);
            setThreadStopped(true);

            return;
        }
        fireProgressStateChanged(100, null, null);

        setCompleted(true);
    }

    /**
     * Method copies the data in srcBuffer centered in the border buffer That is, the src image is completely surrounded
     * by a border. The values in the border locations are copies of nearby srcBuffer values.
     *
     * @param  srcBuffer             float [] the source image buffer
     * @param  bdrBuffer             float [] the border image buffer
     * @param  srcBufferOffsetIndex  offset into srcBuffer
     * @param  bdrBufferOffsetIndex  offset into bdrBuffer
     */
    private void copy2DSrcBufferToBdrBuffer(float[] srcBuffer, float[] bdrBuffer, int srcBufferOffsetIndex,
                                            int bdrBufferOffsetIndex) {

        // index into the border buffer
        int bdrBufferIdx = 0;

        // borderBuffer includes the kernel radius worth of rows at the bottom
        // and top of the image, plus the kernel radius worth of columns
        // on the current row
        // brdBufferKernelOffset is the offset (index) into the borderBuffer of
        // where the first pixel in the sourceBuffer should be copied
        int brdBufferKernelOffset = (halfK[kernelNumber-1] * bdrBufferWidth) + halfK[kernelNumber-1];

        int brdBufferOffset = brdBufferKernelOffset + bdrBufferOffsetIndex;

        int srcBufferIdx = srcBufferOffsetIndex;
        int srcRow, srcCol;

        // assume for now that buffering for 2D slices is fast
        // so we will not update the progressBar

        // copy image data into the borderBuffer
        for (srcRow = 0; srcRow < srcBufferHeight; ++srcRow) {
            bdrBufferIdx = (srcRow * bdrBufferWidth) + brdBufferOffset;

            for (srcCol = 0; srcCol < srcBufferWidth; ++srcCol) {
                bdrBuffer[bdrBufferIdx++] = srcBuffer[srcBufferIdx++];
            }
        }

        int row, col;

        // copy image data to the borders
        // first source image row to lower bdrBuffer rows
        for (row = 0; row < halfK[kernelNumber-1]; ++row) {
            srcBufferIdx = srcBufferOffsetIndex;

            // bdrBufferIdx = row * bdrBufferWidth + halfK;
            bdrBufferIdx = (row * bdrBufferWidth) + halfK[kernelNumber-1] + bdrBufferOffsetIndex;

            for (col = 0; col < srcBufferWidth; ++col) {
                bdrBuffer[bdrBufferIdx++] = srcBuffer[srcBufferIdx++];
            }

            // copy data to columns in bdrBuffer that are NOT in srcBuffer
            bdrBufferIdx = (row * bdrBufferWidth) + bdrBufferOffsetIndex;

            for (col = 0; col < halfK[kernelNumber-1]; ++col) {
                bdrBuffer[bdrBufferIdx++] = srcBuffer[srcBufferOffsetIndex];
                bdrBuffer[(row * bdrBufferWidth) + bdrBufferWidth - 1 - col + bdrBufferOffsetIndex] = srcBuffer[srcBufferWidth -
                                                                                                                1];
            }
        }

        // last source image row to upper bdrBuffer rows
        int srcBufferFirstColIdx = (srcBufferWidth * (srcBufferHeight - 1)) + srcBufferOffsetIndex;
        int srcBufferLastColIdx = (srcBufferWidth * srcBufferHeight) - 1 + srcBufferOffsetIndex;

        for (row = srcBufferHeight + halfK[kernelNumber-1]; row < bdrBufferHeight; ++row) {
            srcBufferIdx = srcBufferFirstColIdx;
            bdrBufferIdx = (row * bdrBufferWidth) + halfK[kernelNumber-1] + bdrBufferOffsetIndex;

            for (col = 0; col < srcBufferWidth; ++col) {
                bdrBuffer[bdrBufferIdx++] = srcBuffer[srcBufferIdx++];
            }

            // copy data to columns in bdrBuffer that are NOT in srcBuffer
            bdrBufferIdx = (row * bdrBufferWidth) + bdrBufferOffsetIndex;

            for (col = 0; col < halfK[kernelNumber-1]; ++col) {
                bdrBuffer[bdrBufferIdx] = srcBuffer[srcBufferFirstColIdx];
                bdrBuffer[(row * bdrBufferWidth) + bdrBufferWidth - 1 - col + bdrBufferOffsetIndex] = srcBuffer[srcBufferLastColIdx];
                bdrBufferIdx++;
            }
        }

        // remaining image columns
        for (srcRow = 0; srcRow < srcBufferHeight; ++srcRow) {
            bdrBufferIdx = ((srcRow + halfK[kernelNumber-1]) * bdrBufferWidth) + bdrBufferOffsetIndex;
            srcBufferIdx = (srcRow * srcBufferWidth) + srcBufferOffsetIndex;

            for (srcCol = 0; srcCol < halfK[kernelNumber-1]; ++srcCol) {
                bdrBuffer[bdrBufferIdx++] = srcBuffer[srcBufferIdx];
            }

            srcBufferIdx = (srcRow * srcBufferWidth) + srcBufferWidth - 1 + srcBufferOffsetIndex; // 6/28;
            bdrBufferIdx = ((srcRow + halfK[kernelNumber-1]) * bdrBufferWidth) + halfK[kernelNumber-1] + srcBufferWidth + bdrBufferOffsetIndex; // 6/28;

            for (srcCol = 0; srcCol < halfK[kernelNumber-1]; ++srcCol) {
                bdrBuffer[bdrBufferIdx++] = srcBuffer[srcBufferIdx];
            }
        }

    }

    /**
     * Method copies the data in srcBuffer so it is centered in the border buffer That is, the src image is completely
     * surrounded by a border. The values in the border locations are copies of nearby srcBuffer values.
     *
     * @param  srcBuffer  float [] the source image buffer
     * @param  bdrBuffer  float [] the border image buffer
     */
    private void copy3DSrcBufferToBdrBuffer(float[] srcBuffer, float[] bdrBuffer) {

        int srcBufferSliceLength = srcBufferWidth * srcBufferHeight;
        int bdrBufferSliceLength = bdrBufferWidth * bdrBufferHeight;

        // offset allows for having extra kernel slices for volumeFilteringBorder
        int bdrBufferSliceOffset;

        if (sliceFiltering == true) {

            // slice filtering does not have extra border slice
            bdrBufferSliceOffset = 0;
        } else {
            bdrBufferSliceOffset = halfK[kernelNumber-1] * bdrBufferSliceLength;
        }

        int sliceIndex;

        for (sliceIndex = 0; sliceIndex < srcBufferDepth; sliceIndex++) {
            
            //fireProgressStateChanged( ((float) (sliceIndex) / (srcBufferDepth - 1)), null, null);
          

            copy2DSrcBufferToBdrBuffer(srcBuffer, bdrBuffer, sliceIndex * srcBufferSliceLength,
                                       (sliceIndex * bdrBufferSliceLength) + bdrBufferSliceOffset);
        }

        if (sliceFiltering == false) {

            // assume slice replication for the kernel slices is quick, do
            // don't mess with the progressBar in this part

            // copy data into the extra border slices
            int copyFromIndex, copyToIndex, dataIndex;

            // for the lower border slice(s)
            for (sliceIndex = 0; sliceIndex < halfK[kernelNumber-1]; sliceIndex++) {
                copyFromIndex = bdrBufferSliceOffset;
                copyToIndex = sliceIndex * bdrBufferSliceLength;

                for (dataIndex = 0; dataIndex < bdrBufferSliceLength; dataIndex++) {
                    bdrBuffer[copyToIndex++] = bdrBuffer[copyFromIndex++];
                }
            }

            for (sliceIndex = bdrBufferDepth - halfK[kernelNumber-1]; sliceIndex < bdrBufferDepth; sliceIndex++) {
                copyFromIndex = bdrBufferSliceOffset + ((srcBufferDepth - 1) * bdrBufferSliceLength);
                copyToIndex = sliceIndex * bdrBufferSliceLength;

                for (dataIndex = 0; dataIndex < bdrBufferSliceLength; dataIndex++) {
                    bdrBuffer[copyToIndex++] = bdrBuffer[copyFromIndex++];
                }
            }
        } // end if (sliceFilter == false)

    } // end copy3DSrcBufferToBdrBuffer(...)

    /**
     * Compiles a list of the values neighboring the desired pixel, that are defined in the kernel.
     * @param   kn    The kernel number; always 0 if adaptiveSize is false
     * @param   i     The central pixel to find neighbors for.
     * @param   data  float [] Image data
     * @param   is2D  True indicates that the neighbors are found along a 2D slice (or 2D image) instead of neighbors in
     *                a 3D volume.
     *
     * @return  The neighboring pixel list corresponds to the kernel chosen.
     */
    private float[] getBorderBufferNeighborList(int kn, int i, float[] data, boolean is2D) {
        int row, col;
        int kCenter = kernelCenter[kn]; // index to the central element of the kernel

        // (this is the mask for which elements in data are used.)
        int width = 0; // width of slice in number of pixels
        int height = 0; // height of slice in number of pixels

        width = bdrBufferWidth;
        height = bdrBufferHeight;

        // place all the masked 'on' elements into the data-list
        int count = 0;

        if (is2D) {

            for (row = -halfK[kn]; row <= halfK[kn]; row++) { // go through all rows

                for (col = -halfK[kn]; col <= halfK[kn]; col++) { // go through all columns

                    if (kernel[kn][kCenter + col + (row * kernelSize[kn])] != 0) { // but don't bother copying into the list if

                        // we don't want that element (the kernel's pixl is zero)
                        kernelMask[kn][count++] = data[i + col + (row * width)];
                    }
                }
            }
        } else { // find neighbors in a volume

            int slice;

            // System.out.println("********** getBorderBufferNeighborList ( " + i +" ) **********");
            // halfK-number of kernelSize slices (to get to the center slice)
            for (slice = -halfK[kn]; slice <= halfK[kn]; slice++) {

                for (row = -halfK[kn]; row <= halfK[kn]; row++) {

                    for (col = -halfK[kn]; col <= halfK[kn]; col++) {

                        if (kernel[kn][kCenter + col + (row * kernelSize[kn]) + (slice * kernelSize[kn] * kernelSize[kn])] != 0) {
                            kernelMask[kn][count++] = data[i + col + (row * width) + (slice * width * height)];
                        }
                    }
                }
            }
        }

        return (kernelMask[kn]);
    }

    /**
     * Compiles a list of the values neighboring the desired pixel, that are defined in the kernel.
     *
     * <p>Color images are processed differently from the monochrome images because although colour images use the same
     * size kernel as mono images, it fills the kernel with brightness levels that are spread out in the data set. The
     * Neighbor list still reports the monochromatic brightness values. That is, for a color image: the neighbors of the
     * central pixel with the same color are returned in the neighbor list's kernel.</p>
     * @param   kn    The kernel number; always 0 if adaptiveSize is false.
     * @param   i     The central pixel to find neighbors for.
     * @param   data  float [] Image data
     * @param   is2D  True indicates that the neighbors are found along a 2D slice (or 2D image) instead of neighbors in
     *                a 3D volume.
     *
     * @return  The neighboring pixel list corresponds to the kernel chosen.
     */
    private float[] getNeighborList(int kn, int i, float[] data, boolean is2D) {
        int row, col;
        int kCenter = kernelCenter[kn]; // index to the central element of the kernel

        // (this is the mask for which elements in data are used.)
        int width = 0; // width of slice in number of pixels
        int height = 0; // height of slice in number of pixels

        width = srcImage.getExtents()[0];
        height = srcImage.getExtents()[1];

        int sliceWidth = width * valuesPerPixel; // width of slice in number of elements

        // place all the masked 'on' elements into the data-list
        int count = 0;

        // color images are different from the mono images in that though color images use the same size
        // kernel as mono images, but fill it with brightness levels that are spread out in the data set.
        if (isColorImage) { // 2D filtering of color images is a little different than of mono images

            int kcol;
            int leftBound = -halfK[kn] * 4;
            int rightBound = halfK[kn] * 4;

            if (is2D) {

                for (row = -halfK[kn]; row <= halfK[kn]; row++) { // go through all rows

                    for (col = leftBound, kcol = -halfK[kn]; col <= rightBound; col += 4, kcol++) { // go through every 4th
                                                                                                // column

                        if (kernel[kn][kCenter + kcol + (row * kernelSize[kn])] != 0) { // but don't bother copying into the
                                                                                // list if

                            // we don't want that element (the kernel's pixl is zero)
                            kernelMask[kn][count++] = data[i + col + (row * sliceWidth)];
                        }
                    }
                }
            } else { // find neighbors in a volume

                int slice;
                //System.out.println("********** getBorderBufferNeighborList ( " + i + " ) **********");

                // halfK-number of kernelSize slices (to get to the center slice)
                for (slice = -halfK[kn]; slice <= halfK[kn]; slice++) {

                    for (row = -halfK[kn]; row <= halfK[kn]; row++) {

                        for (col = leftBound, kcol = -halfK[kn]; col <= rightBound; col += 4, kcol++) {

                            if (kernel[kn][kCenter + kcol + (row * kernelSize[kn]) + (slice * kernelSize[kn] * kernelSize[kn])] != 0) {
                                kernelMask[kn][count++] = data[i + col + (row * sliceWidth) + (slice * sliceWidth * height)];
                            }
                        }
                    }
                }
            }
        } else { // a mono image

            if (is2D) {

                for (row = -halfK[kn]; row <= halfK[kn]; row++) { // go through all rows

                    for (col = -halfK[kn]; col <= halfK[kn]; col++) { // go through all columns

                        if (kernel[kn][kCenter + col + (row * kernelSize[kn])] != 0) { // but don't bother copying into the
                                                                               // list if

                            // we don't want that element (the kernel's pixl is zero)
                            kernelMask[kn][count++] = data[i + col + (row * width)];
                        }
                    }
                }
            } else { // find neighbors in a volume

                int slice;

                // halfK-number of kernelSize slices (to get to the center slice)
                for (slice = -halfK[kn]; slice <= halfK[kn]; slice++) {

                    for (row = -halfK[kn]; row <= halfK[kn]; row++) {

                        for (col = -halfK[kn]; col <= halfK[kn]; col++) {

                            if (kernel[kn][kCenter + col + (row * kernelSize[kn]) + (slice * kernelSize[kn] * kernelSize[kn])] != 0) {
                                kernelMask[kn][count++] = data[i + col + (row * width) + (slice * width * height)];
                            }
                        }
                    }
                }
            }
        }

        return (kernelMask[kn]);
    }
    
    /**
     * Compiles a list of the values neighboring the desired pixel, that are defined in the kernel.
     *
     * <p>Only used with color images in vector filtering.
     * Returned float array has red, green, blue, red, green, blue, etc.
     * @param   kn    The kernel number; always zero if adaptiveSize is false.
     * @param   i     The central pixel to find neighbors for.
     * @param   data  float [] Image data
     * @param   is2D  True indicates that the neighbors are found along a 2D slice (or 2D image) instead of neighbors in
     *                a 3D volume.
     *
     * @return  The neighboring pixel list corresponds to the kernel chosen.
     */
    private float[] getVectorNeighborList(int kn, int i, float[] data, boolean is2D) {
        int row, col;
        int kCenter = kernelCenter[kn]; // index to the central element of the kernel

        // (this is the mask for which elements in data are used.)
        int width = 0; // width of slice in number of pixels
        int height = 0; // height of slice in number of pixels

        width = srcImage.getExtents()[0];
        height = srcImage.getExtents()[1];

        int sliceWidth = width * valuesPerPixel; // width of slice in number of elements

        // place all the masked 'on' elements into the data-list
        int count = 0;

        // color images are different from the mono images in that though color images use the same size
        // kernel as mono images, but fill it with brightness levels that are spread out in the data set.

            int kcol;
            int leftBound = -halfK[kn] * 4;
            int rightBound = halfK[kn] * 4;

            if (is2D) {

                for (row = -halfK[kn]; row <= halfK[kn]; row++) { // go through all rows

                    for (col = leftBound, kcol = -halfK[kn]; col <= rightBound; col += 4, kcol++) { // go through every 4th
                                                                                                // column

                        if (kernel[kn][kCenter + kcol + (row * kernelSize[kn])] != 0) { // but don't bother copying into the
                                                                                // list if

                            // we don't want that element (the kernel's pixl is zero)
                            kernelVectorMask[kn][count++] = data[i + 1 + col + (row * sliceWidth)];
                            kernelVectorMask[kn][count++] = data[i + 2 + col + (row * sliceWidth)];
                            kernelVectorMask[kn][count++] = data[i + 3 + col + (row * sliceWidth)];
                        }
                    }
                }
            } else { // find neighbors in a volume

                int slice;
                //System.out.println("********** getBorderBufferNeighborList ( " + i + " ) **********");

                // halfK-number of kernelSize slices (to get to the center slice)
                for (slice = -halfK[kn]; slice <= halfK[kn]; slice++) {

                    for (row = -halfK[kn]; row <= halfK[kn]; row++) {

                        for (col = leftBound, kcol = -halfK[kn]; col <= rightBound; col += 4, kcol++) {

                            if (kernel[kn][kCenter + kcol + (row * kernelSize[kn]) + (slice * kernelSize[kn] * kernelSize[kn])] != 0) {
                                kernelVectorMask[kn][count++] = data[i + 1 + col + (row * sliceWidth) + (slice * sliceWidth * height)];
                                kernelVectorMask[kn][count++] = data[i + 2 + col + (row * sliceWidth) + (slice * sliceWidth * height)];
                                kernelVectorMask[kn][count++] = data[i + 3 + col + (row * sliceWidth) + (slice * sliceWidth * height)];
                            }
                        }
                    }
                }
            }
       
        return (kernelVectorMask[kn]);
    }

    /**
     * Forms kernel.
     */
    private void makeKernel() {
        int i;

        try {
            if (adaptiveSize) {
                kernelNumber = (maximumSize - minimumSize)/2 + 1;
                kernelSize = new int[kernelNumber];
                for (i = 0; i < kernelNumber; i++) {
                    kernelSize[i] = minimumSize + 2*i;
                }
            }
            else {
                kernelNumber = 1;
                kernelSize = new int[1];
                kernelSize[0] = minimumSize;
            }

            kernel = new byte[kernelNumber][];
            if (sliceFiltering) {
                for (i = 0; i < kernelNumber; i++) {
                    kernel[i] = new byte[kernelSize[i]*kernelSize[i]];
                }
            } else if (!sliceFiltering) {
                for (i = 0; i < kernelNumber; i++) {
                    kernel[i] = new byte[kernelSize[i]*kernelSize[i]*kernelSize[i]];
                }
            }
        } catch (OutOfMemoryError e) {
            displayError("Algorithm Median reports: not enough memory to form a kernel mask.");
            setCompleted(false);
            setThreadStopped(true);

            return;
        }

        setKernel();
        makeKernelMask();
    }

    /**
     * Makes the kernel mask. The kernel mask is the list of values pulled from the image which will be used to find the
     * median of the central pixel. Its length is <i>(number of pixels to be used to determine median)</i>.
     *
     * <p>Thus the kernel center (decided here), has the value of the location of the central pixel shown in the window.
     * The value of the kernel center is the number of pixels picked up to median sort.</p>
     *
     * <p>Note that for symmetric masks always have kernelCenter = count/2 and maskCenter = count/2.</p>
     */
    private void makeKernelMask() {
        int n;
        kernelMask = new float[kernelNumber][];
        kernelVectorMask = new float[kernelNumber][];
        kernelCenter = new int[kernelNumber];
        maskCenter = new int[kernelNumber];
        for (n = 0; n < kernelNumber; n++) {
            // figure how many kernel elements are actually in the kernel-mask
            int count = 0; // start counting from one, since sort starts with element 1 (even empty mask must have 1
                           // element!)
    
            for (int m = 0; m < kernel[n].length; m++) {
    
                if (kernel[n][m] != 0) { // if this element is marked 'on'
                    count++;
                }
            }
    
            kernelMask[n] = new float[count];
            kernelVectorMask[n] = new float[3*count];
    
            if (sliceFiltering) { // 2D
    
                if (kernelShape == SQUARE_KERNEL) {
                    kernelCenter[n] = count / 2; // whole square
                    maskCenter[n] = halfK[n] * (kernelSize[n] + 1); // count/2 : I feel dumb
                } else if (kernelShape == CROSS_KERNEL) {
                    kernelCenter[n] = halfK[n] * (kernelSize[n] + 1);
                    maskCenter[n] = kernelSize[n] - 1;
                } else if ((kernelShape == VERT_KERNEL) || (kernelShape == HORZ_KERNEL)) {
                    kernelCenter[n] = halfK[n] * (kernelSize[n] + 1);
                    maskCenter[n] = (kernelSize[n] / 2);
                } else if (kernelShape == X_KERNEL) { // sizeof kernel is same as CROSS_KERNEL
                    kernelCenter[n] = halfK[n] * (kernelSize[n] + 1);
                    maskCenter[n] = kernelSize[n] - 1;
                }
            } else { // 3D
    
                if (kernelShape == CUBE_KERNEL) {
                    kernelCenter[n] = count / 2;
                    maskCenter[n] = halfK[n] * ((kernelSize[n] * kernelSize[n]) + kernelSize[n] + 1);
                } else if (kernelShape == AXIAL_KERNEL) {
                    kernelCenter[n] = (kernelSize[n] * kernelSize[n] * kernelSize[n]) / 2; // whole cube
                    maskCenter[n] = count / 2; // i feel dumb...
                } else if (kernelShape == X_KERNEL) { // sizeof kernel is same as AXIAL_KERNEL
                    kernelCenter[n] = (kernelSize[n] * kernelSize[n] * kernelSize[n]) / 2; // whole cube
                    maskCenter[n] = count / 2; // i feel dumb...
                }
            }
            // not entirely dumb.  mc = count/2 because of the symmetry of the mask.  custom masks may be diff.
            // & i'd like to include custom masks someday....
        } // for (n = 0; n < kernelNumber; n++)
    }

    /**
     * Finds the mean value (average) in the list.
     *
     * @param   list  float [] List of numbers
     *
     * @return  float The mean.
     *
     * @author  parsonsd
     */
    private float mean(float[] list) {
        int i;
        float sum = 0;

        for (i = 0; i < list.length; i++) {
            sum += list[i];
        }

        return (float) (sum / (list.length));
    }

    /**
     * Finds the median value of the list.
     *
     * @param   list  float [] List of numbers
     *
     * @return  The median.
     *
     * @author  parsonsd
     */
    private float median(float[] list) {
        int N;
        float med;

        N = list.length - 1;

        if ((N % 2) == 0) {
            med = list[(N / 2)];
        } else {
            med = (list[N / 2] + list[(N / 2) + 1]) / 2;
        }

        return (med);
    }
    
    /**
     * Only used with color images in vector filtering.  Returns index of pixel whose
     * sum of squares difference from the other pixels in the list is smallest.
     * @param list
     * @return
     */
    private int vectorMagnitudeMedian(float[] list){
        int i, j;
        int index = 0;
        float total;
        float smallestTotal = Float.MAX_VALUE;
        for (i = 0; i < list.length; i +=3) {
            total = 0.0f;
            for (j = 0; j < list.length; j += 3) {
                total +=  (list[i] - list[j])*(list[i] - list[j]) +
                          (list[i+1] - list[j+1])*(list[i+1] - list[j+1]) +
                          (list[i+2] - list[j+2])*(list[i+2] - list[j+2]);
            }
            if (total < smallestTotal) {
                index = i;
                smallestTotal = total;
            }
        }
        return index;
    }
    
    /**
     * Only used with color images in vector filtering.  Returns index of pixel whose
     * sum of angles difference from the other pixels in the list is smallest.
     * @param list
     * @return
     */
    private int vectorDirectionMedian(float[] list){
        int i, j;
        int index = 0;
        float total;
        float smallestTotal = Float.MAX_VALUE;
        float dotProduct;
        double normI;
        double normJ;
        for (i = 0; i < list.length; i +=3) {
            total = 0.0f;
            normI = Math.sqrt(list[i]*list[i] + list[i+1]*list[i+1] + list[i+2]*list[i+2]);
            for (j = 0; j < list.length; j += 3) {
                dotProduct = (list[i]*list[j] + list[i+1]*list[j+1] + list[i+2]*list[j+2]);
                normJ = Math.sqrt(list[j]*list[j] + list[j+1]*list[j+1] + list[j+2]*list[j+2]);
                if ((normI == 0.0) && (normJ == 0.0)) {
                    ; // total += 0    
                }
                else if ((normI == 0.0) || (normJ == 0.0)) {
                    total += Math.PI/2.0;
                }
                else {
                     total +=  Math.acos(Math.min(1.0,dotProduct/(normI*normJ)));
                }
            }
            if (total < smallestTotal) {
                index = i;
                smallestTotal = total;
            }
        }
        return index;
    }

    /**
     * If the progress bar is visible, sets the text to:<br>
     * <tt>Copying all <i>color</i> values ...</tt>
     *
     * @param  colorText  The color to use. Eg., "red" or "blue".
     */
    private void setCopyColorText(String colorText) {
        fireProgressStateChanged(-1, null, "Copying all " + colorText + " values ...");
    }

    /**
     * Fill in the mask for which pixels are used in filtering.
     */
    private void setKernel() {
        int n;
        int i;
        halfK = new int[kernelNumber];
        for (n = 0; n < kernelNumber; n++) {
            halfK[n] = kernelSize[n] / 2;
        }

        // square/box
        if ((kernelShape == SQUARE_KERNEL) || (kernelShape == CUBE_KERNEL)) {
            for (n = 0 ; n < kernelNumber; n++) {
                for (i = 0; i < kernel[n].length; i++) {
                    kernel[n][i] = 1;
                }
            }
        } // end square/cube kernel

        // cross/axial
        else if ((kernelShape == CROSS_KERNEL) || (kernelShape == AXIAL_KERNEL)) {
            int row; // indicates current row
            int col; // indicates current column

            if (sliceFiltering) {

                for (n = 0; n < kernelNumber; n++) {
                    for (i = 0; i < kernel[n].length; i++) {
                        row = i / kernelSize[n];
                        col = i % kernelSize[n];
    
                        if (col == halfK[n]) {
                            kernel[n][i] = 1;
                        } else if (row == halfK[n]) {
                            kernel[n][i] = 1;
                        } else {
                            kernel[n][i] = 0;
                        }
                    }
                }
            } else { // volume filtering

                int slice;

                for (n = 0; n < kernelNumber; n++) {
                    for (i = 0; i < kernel[n].length; i++) {
                        slice = i / (kernelSize[n] * kernelSize[n]);
                        row = (i % (kernelSize[n] * kernelSize[n])) / kernelSize[n];
                        col = i % kernelSize[n];
    
                        if (slice == halfK[n]) {
    
                            if (col == halfK[n]) {
                                kernel[n][i] = 1;
                            } else if (row == halfK[n]) {
                                kernel[n][i] = 1;
                            } else {
                                kernel[n][i] = 0;
                            }
                        } else if ((row == halfK[n]) && (col == halfK[n])) {
                            kernel[n][i] = 1;
                        } else {
                            kernel[n][i] = 0;
                        }
                    }
                }
            }
        } // end cross/axial
        else if (kernelShape == VERT_KERNEL) {
            int col; // indicates current column

            if (sliceFiltering) {

                for (n = 0; n < kernelNumber; n++) {
                    for (i = 0; i < kernel[n].length; i++) {
                        col = i % kernelSize[n];
    
                        if (col == halfK[n]) {
                            kernel[n][i] = 1;
                        } else {
                            kernel[n][i] = 0;
                        }
                    }
                }
            } else { // volume filtering
            }
        } // end vert
        else if (kernelShape == HORZ_KERNEL) {
            int row; // indicates current row

            if (sliceFiltering) {

                for (n = 0; n < kernelNumber; n++) {
                    for (i = 0; i < kernel[n].length; i++) {
                        row = i / kernelSize[n];
    
                        if (row == halfK[n]) {
                            kernel[n][i] = 1;
                        } else {
                            kernel[n][i] = 0;
                        }
                    }
                }
            } else { // volume filtering
            }
        } // end vert

        // 'x' kernel
        else if (kernelShape == X_KERNEL) {
            int row; // indicates current row
            int col; // indicates current column
            int revcol; // runs opposite of the col.

            if (sliceFiltering) {

                for (n = 0; n < kernelNumber; n++) {
                    for (i = 0; i < kernel[n].length; i++) {
                        row = i / kernelSize[n];
                        col = i % kernelSize[n];
                        revcol = kernelSize[n] - 1 - col;
    
                        if (row == col) {
                            kernel[n][i] = 1;
                        } else if (row == revcol) {
                            kernel[n][i] = 1;
                        } else {
                            kernel[n][i] = 0;
                        }
                    }
                }
            } else { // volume filtering

                int slice;

                for (n = 0; n < kernelNumber; n++) {
                    for (i = 0; i < kernel[n].length; i++) {
                        slice = i / (kernelSize[n] * kernelSize[n]);
                        row = (i % (kernelSize[n] * kernelSize[n])) / kernelSize[n];
                        col = i % kernelSize[n];
                        revcol = kernelSize[n] - 1 - col;
    
                        if ((slice == col) || (slice == revcol)) {
    
                            if (row == col) {
                                kernel[n][i] = 1;
                            } else if (row == revcol) {
                                kernel[n][i] = 1;
                            } else {
                                kernel[n][i] = 0;
                            }
                        } else {
                            kernel[n][i] = 0;
                        }
                    }
                }
            }
        } // end 'x' kernel

    }



    /**
     * Allows a single slice to be filtered. Note that a progressBar must be created first.
     *
     * @param  srcBuffer            float [] Source buffer.
     * @param  destBuffer           float[] Destination Buffer.
     * @param  bufferStartingPoint  Starting point for the buffer.
     * @param  msgString            A text message that can be displayed as a message text in the progressBar.
     */
    private void sliceFilter(float[] srcBuffer, float[] destBuffer, int bufferStartingPoint, String msgString) {
        int i, a, pass; // counting....   i is the offset from the bufferStartingPoint

        // a adds support for 3D filtering by counting as the pixel at the starting point plus the counter offset
        int buffStart = bufferStartingPoint; // data element at the buffer. a = bufferStartingPoint+i
        int sliceLength = srcImage.getSliceSize();
        int imageSliceLength = sliceLength * valuesPerPixel; // since there are 4 values for every color pixel
        int kCenter = maskCenter[0]; // to find the middle pixel of the kernel-mask
        int width = srcImage.getExtents()[0]; // width of slice in number of pixels (
        int height = srcImage.getExtents()[1]; // height of slice in number of pixels
        int sliceWidth = width * valuesPerPixel; // width of slice, which, in color images is (4*width)
        int sliceHeight = height; // height of image, which, actually doesn't change
        int initialIndex = 0; // first element is alpha

        float[] tempBuffer;

        float average; // arithmetic mean
        float sigma; // standard deviation

        float[] maskedList; // list of buffer-values that were showing inside the mask

        int row, col; // row and column vars for easier reading [(0,0) is in the top-left corner]
        int mod; // 1% length of slice for percent complete

        // these bounds "frame" the interior of the slice which may be filtered (&adjusted);
        // image outside the frame may not
        int upperBound, lowerBound, // bounds on the row
            leftBound, rightBound; // bounds on the column
        
        float a1, a2, b1, b2;
        int kn;
        float zmin, zmed, zmax;
        boolean loop;

        if (isColorImage) {
            upperBound = halfK[kernelNumber-1];
            leftBound = halfK[kernelNumber-1] * 4;
            lowerBound = sliceHeight - halfK[kernelNumber-1] - 1;
            rightBound = sliceWidth - (halfK[kernelNumber-1] * 4) - 1;

            // data element at the buffer (a = i+bufferStartingPoint) must start on an alpha value
            buffStart = bufferStartingPoint - (bufferStartingPoint % 4); // & no effect if bufferStartingPoint%4 == 0
                                                                         // !!!

            // copy all alpha values in this slice
            setCopyColorText("alpha");

            for (a = buffStart, i = 0; i < imageSliceLength; a += 4, i += 4) {
                destBuffer[a] = srcBuffer[a]; // copy alpha;
            }

        } else { // monochrome image
            upperBound = leftBound = halfK[kernelNumber-1];
            rightBound = sliceWidth - halfK[kernelNumber-1] - 1;
            lowerBound = sliceHeight - halfK[kernelNumber-1] - 1;
        }

        mod = (imageSliceLength * numberOfSlices) / 100; // mod is 1 percent of length of slice * the number of slices.

        for (pass = 0; (pass < iterations) && !threadStopped; pass++) {
            a = buffStart; // set/reset a to address pixels from the beginning of this buffer.

            if (isColorImage) { // color image dealt with in special way

                // choose i so the proper colors go
                // copy only needed RGB values
                initialIndex = 0; // start with alpha on each pass (routine moved so we don't do it for each pass)

                while ((initialIndex < 3) && !threadStopped) { // alpha:0, R:1, G:2, B:3.  But alpha must be copied
                    ++initialIndex; // next initial index
                    a += initialIndex; // keep the pixel location up with color indexed to

                    if ((numberOfSlices > 1) ) { // 3D image     update progressBar

                        fireProgressStateChanged( ((float) (currentSlice + (pass * numberOfSlices)) /
                                (iterations * numberOfSlices)), null, null);
                       }

                    if (!rChannel && (initialIndex == 1)) {

                        // when looking at the image reds but we're not filtering the red channel
                        // copy all red values
                        setCopyColorText("red");

                        for (i = initialIndex; i < imageSliceLength; a += 4, i += 4) {
                            destBuffer[a] = srcBuffer[a];
                        }
                    } else if (!gChannel && (initialIndex == 2)) {

                        // when looking at the image greens but we're not filtering the greens channel
                        // copy all greens values
                        setCopyColorText("green");

                        for (i = initialIndex; i < imageSliceLength; a += 4, i += 4) {
                            destBuffer[a] = srcBuffer[a];
                        }
                    } else if (!bChannel && (initialIndex == 3)) {

                        // when looking at the image blues but we're  not filtering the blues channel
                        // copy all blue values
                        setCopyColorText("blue");

                        for (i = initialIndex; i < imageSliceLength; a += 4, i += 4) {
                            destBuffer[a] = srcBuffer[a];
                        }
                    } else {
                        fireProgressStateChanged(-1, null, "Filtering " + msgString + " (pass " + String.valueOf(pass + 1) +
                                " of " + iterations + ") ...");

                        // if we needed to filter the image, we dropped through the selection to filter the
                        // color given by int initialIndex
                        for (i = initialIndex; (i < imageSliceLength) && !threadStopped; a += 4, i += 4) {

                            if (numberOfSlices == 1) { // 2D image     update progressBar

                                if ((((i - initialIndex) % mod) == 0)) {
                                    
                                    fireProgressStateChanged( ((float) ((3 * (pass * sliceLength)) +
                                            ((initialIndex - 1) * sliceLength) +
                                            (i / 4)) /
                                       (3 * iterations * sliceLength)), null, null);
                                }
                            }

                            if ((entireImage == true) || mask.get(a / 4)) { // may have problems in masking .....
                                row = i / sliceWidth;
                                col = i % sliceWidth;

                                if ((row < upperBound) || (row > lowerBound)) {
                                    destBuffer[a] = srcBuffer[a]; // row too far up or down--out of bounds
                                } else if ((col < leftBound) || (col > rightBound)) {
                                    destBuffer[a] = srcBuffer[a]; // column too far left or right--out of bounds
                                } else { // in bounds
                                    if (adaptiveSize) {
                                        kn = 0;
                                        loop = true;
                                        while (loop) {
                                             maskedList = getNeighborList(kn++, a, srcBuffer, true);
                                             Arrays.sort(maskedList);
                                             zmed = median(maskedList);
                                             zmin = maskedList[0];
                                             zmax = maskedList[maskedList.length-1];
                                             a1 = zmed - zmin;
                                             a2 = zmed - zmax;
                                             if ((a1 > 0.0f) && (a2 < 0.0f)) {
                                                 loop = false;
                                                 b1 = srcBuffer[a] - zmin;
                                                 b2 = srcBuffer[a] - zmax;
                                                 if ((b1 > 0.0f) && (b2 < 0.0f)) {
                                                     destBuffer[a] = srcBuffer[a];
                                                 }
                                                 else {
                                                     destBuffer[a] = zmed;
                                                 }
                                             } // if (a1 > 0.0f_ && (a2 < 0.0f))
                                             else if (kn == kernelNumber) {
                                                 loop = false;
                                                 destBuffer[a] = zmed;
                                             }
                                        } // while (loop) 
                                    } // if (adaptiveSize)
                                    else { // not adaptiveSize
                                        maskedList = getNeighborList(0, a, srcBuffer, true);
    
                                        // verify that this element is an outlier
                                        if (stdDevLimit == 0.0) { // anything is an outlier
                                            Arrays.sort(maskedList);
                                            destBuffer[a] = median(maskedList);
                                        } else { // look for outlierness
                                            average = mean(maskedList);
                                            sigma = standardDeviation(maskedList, average);
    
                                            if ((maskedList[kCenter] > (average + (stdDevLimit * sigma))) ||
                                                    (maskedList[kCenter] < (average - (stdDevLimit * sigma)))) {
                                                Arrays.sort(maskedList);
                                                destBuffer[a] = median(maskedList);
                                            } else { // if element was not an outlier, pixel is fine.
                                                destBuffer[a] = srcBuffer[a];
                                            }
                                        }
                                    }
                                } // not adaptiveSize
                            } else { // not part of the VOI so just copy this into the destination buffer.
                                destBuffer[a] = srcBuffer[a];
                            }
                        }
                    }

                    a = buffStart; // reset the index back to the beginning of the filter area
                }
            } else { // monochrome image

                fireProgressStateChanged(-1, null, "Filtering " + msgString + " (pass " + String.valueOf(pass + 1) + " of " +
                        iterations + ") ...");

                if (numberOfSlices > 1) { // 3D image     update progressBar
                    fireProgressStateChanged( (((float) (currentSlice + (pass * numberOfSlices)) /
                            (iterations * numberOfSlices))), null, null);
               }
               

                for (i = 0; (i < imageSliceLength) && !threadStopped; i++) {

                    if (numberOfSlices == 1) { // 2D image     update progressBar

                        if (((i % mod) == 0)) {
                            fireProgressStateChanged( ((float) ((pass * imageSliceLength) + i) /
                                    (iterations * imageSliceLength)), null, null);
                            
                        }
                    }

                    if ((entireImage == true) || mask.get(a)) {

                        // Median stuff here
                        row = i / width;
                        col = i % width;

                        if ((row < upperBound) || (row > lowerBound)) {
                            destBuffer[a] = srcBuffer[a]; // row too far up or down--out of bounds
                        } else if ((col < leftBound) || (col > rightBound)) {
                            destBuffer[a] = srcBuffer[a]; // column too far left or right--out of bounds
                        } else { // in bounds
                            if (adaptiveSize) {
                                kn = 0;
                                loop = true;
                                while (loop) {
                                     maskedList = getNeighborList(kn++, a, srcBuffer, true);
                                     Arrays.sort(maskedList);
                                     zmed = median(maskedList);
                                     zmin = maskedList[0];
                                     zmax = maskedList[maskedList.length-1];
                                     a1 = zmed - zmin;
                                     a2 = zmed - zmax;
                                     if ((a1 > 0.0f) && (a2 < 0.0f)) {
                                         loop = false;
                                         b1 = srcBuffer[a] - zmin;
                                         b2 = srcBuffer[a] - zmax;
                                         if ((b1 > 0.0f) && (b2 < 0.0f)) {
                                             destBuffer[a] = srcBuffer[a];
                                         }
                                         else {
                                             destBuffer[a] = zmed;
                                         }
                                     } // if (a1 > 0.0f_ && (a2 < 0.0f))
                                     else if (kn == kernelNumber) {
                                         loop = false;
                                         destBuffer[a] = zmed;
                                     }
                                } // while (loop)     
                            } // if (adaptiveSize)
                            else { // not adaptiveSize
                                maskedList = getNeighborList(0, a, srcBuffer, true);
    
                                // verify that this element is an outlier
                                if (stdDevLimit == 0.0) { // anything is an outlier
                                	Arrays.sort(maskedList);
                                    destBuffer[a] = median(maskedList);
                                } else { // look for outlierness
                                    average = mean(maskedList);
                                    sigma = standardDeviation(maskedList, average);
    
                                    if ((maskedList[kCenter] > (average + (stdDevLimit * sigma))) ||
                                            (maskedList[kCenter] < (average - (stdDevLimit * sigma)))) {
                                    	Arrays.sort(maskedList);
                                        destBuffer[a] = median(maskedList);
                                    } else { // if element was not an outlier, pixel is fine.
                                        destBuffer[a] = srcBuffer[a];
                                    }
                                }
                            }
                        }
                    } else { // not part of the VOI so just copy this into the destination buffer.
                        destBuffer[a] = srcBuffer[a];
                    }

                    a++; // address the next data element from the bufferStartingPoint
                }
            }

            // now set up for the repeat for multiple iterations.
            // But only bother with copying over if there are more iterations.
            if (pass < (iterations - 1)) {
                tempBuffer = destBuffer; // swap dest & src buffers
                destBuffer = srcBuffer;
                srcBuffer = tempBuffer;
            }
        }
        // destBuffer should now be copied over for the size of imageSliceLength.  You may return....
    }
    
    /**
     * Allows a single slice to be filtered. Note that a progressBar must be created first.
     * Only used on color with vector magnitude filtering.
     * Red, green, and blue are all filtered together; that is, the
     * new red, green, and blue will all come from the same pixel.
     *
     * @param  srcBuffer            float [] Source buffer.
     * @param  destBuffer           float[] Destination Buffer.
     * @param  bufferStartingPoint  Starting point for the buffer.
     * @param  msgString            A text message that can be displayed as a message text in the progressBar.
     */
    private void sliceVectorMagnitudeFilter(float[] srcBuffer, float[] destBuffer, int bufferStartingPoint, String msgString) {
        int i, a, pass; // counting....   i is the offset from the bufferStartingPoint

        // a adds support for 3D filtering by counting as the pixel at the starting point plus the counter offset
        int buffStart = bufferStartingPoint; // data element at the buffer. a = bufferStartingPoint+i
        int sliceLength = srcImage.getSliceSize();
        int imageSliceLength = sliceLength * valuesPerPixel; // since there are 4 values for every color pixel
        int width = srcImage.getExtents()[0]; // width of slice in number of pixels (
        int height = srcImage.getExtents()[1]; // height of slice in number of pixels
        int sliceWidth = width * valuesPerPixel; // width of slice, which, in color images is (4*width)
        int sliceHeight = height; // height of image, which, actually doesn't change
        int index = 0; 

        float[] tempBuffer;

        float[] maskedList; // list of buffer-values that were showing inside the mask

        int row, col; // row and column vars for easier reading [(0,0) is in the top-left corner]
        int mod; // 1% length of slice for percent complete

        // these bounds "frame" the interior of the slice which may be filtered (&adjusted);
        // image outside the frame may not
        int upperBound, lowerBound, // bounds on the row
            leftBound, rightBound; // bounds on the column
        
        upperBound = halfK[0];
        leftBound = halfK[0] * 4;
        lowerBound = sliceHeight - halfK[0] - 1;
        rightBound = sliceWidth - (halfK[0] * 4) - 1;

        // data element at the buffer (a = i+bufferStartingPoint) must start on an alpha value
        buffStart = bufferStartingPoint - (bufferStartingPoint % 4); // & no effect if bufferStartingPoint%4 == 0
                                                                     // !!!

        // copy all alpha values in this slice
        setCopyColorText("alpha");

        for (a = buffStart, i = 0; i < imageSliceLength; a += 4, i += 4) {
            destBuffer[a] = srcBuffer[a]; // copy alpha;
        }
        
        mod = (imageSliceLength * numberOfSlices) / 100; // mod is 1 percent of length of slice * the number of slices.

        for (pass = 0; (pass < iterations) && !threadStopped; pass++) {
            
            if ((numberOfSlices > 1)) { // 3D image     update progressBar

                fireProgressStateChanged( (((float) (currentSlice + (pass * numberOfSlices)) /
                        (iterations * numberOfSlices))), null, null);
             
            }
            a = buffStart; // set/reset a to address pixels from the beginning of this buffer.


                
            fireProgressStateChanged(-1, null, "Filtering " + msgString + " (pass " + String.valueOf(pass + 1) +
                    " of " + iterations + ") ...");

                        // if we needed to filter the image, we dropped through the selection to filter the
                        // color given by int initialIndex
                        for (i = 0; (i < imageSliceLength) && !threadStopped; a += 4, i += 4) {

                            if (numberOfSlices == 1) { // 2D image     update progressBar

                                if (((i % mod) == 0)) {
                                    fireProgressStateChanged( ((float) ((pass * sliceLength) +
                                            (i / 4)) /
                                            (iterations * sliceLength)), null, null);
                                    
                                }
                            }

                            if ((entireImage == true) || mask.get(a / 4)) { // may have problems in masking .....
                                row = i / sliceWidth;
                                col = i % sliceWidth;

                                if ((row < upperBound) || (row > lowerBound)) {
                                    destBuffer[a+1] = srcBuffer[a+1]; // row too far up or down--out of bounds
                                    destBuffer[a+2] = srcBuffer[a+2];
                                    destBuffer[a+3] = srcBuffer[a+3];
                                } else if ((col < leftBound) || (col > rightBound)) {
                                    destBuffer[a+1] = srcBuffer[a+1]; // column too far left or right--out of bounds
                                    destBuffer[a+2] = srcBuffer[a+2];
                                    destBuffer[a+3] = srcBuffer[a+3];
                                } else { // in bounds
                                    maskedList = getVectorNeighborList(0, a, srcBuffer, true);

                                    index = vectorMagnitudeMedian(maskedList);
                                    destBuffer[a+1] = maskedList[index];
                                    destBuffer[a+2] = maskedList[index+1];
                                    destBuffer[a+3] = maskedList[index+2];
                                }
                            } else { // not part of the VOI so just copy this into the destination buffer.
                                destBuffer[a+1] = srcBuffer[a+1];
                                destBuffer[a+2] = srcBuffer[a+2];
                                destBuffer[a+3] = srcBuffer[a+3];
                            }
                        }

                    
            // now set up for the repeat for multiple iterations.
            // But only bother with copying over if there are more iterations.
            if (pass < (iterations - 1)) {
                tempBuffer = destBuffer; // swap dest & src buffers
                destBuffer = srcBuffer;
                srcBuffer = tempBuffer;
            }
        }
        // destBuffer should now be copied over for the size of imageSliceLength.  You may return....

    }
    
    /**
     * Allows a single slice to be filtered. Note that a progressBar must be created first.
     * Only used on color with vector direction filtering.
     * Red, green, and blue are all filtered together; that is, the
     * new red, green, and blue will all come from the same pixel.
     *
     * @param  srcBuffer            float [] Source buffer.
     * @param  destBuffer           float[] Destination Buffer.
     * @param  bufferStartingPoint  Starting point for the buffer.
     * @param  msgString            A text message that can be displayed as a message text in the progressBar.
     */
    private void sliceVectorDirectionFilter(float[] srcBuffer, float[] destBuffer, int bufferStartingPoint, String msgString) {
        int i, a, pass; // counting....   i is the offset from the bufferStartingPoint

        // a adds support for 3D filtering by counting as the pixel at the starting point plus the counter offset
        int buffStart = bufferStartingPoint; // data element at the buffer. a = bufferStartingPoint+i
        int sliceLength = srcImage.getSliceSize();
        int imageSliceLength = sliceLength * valuesPerPixel; // since there are 4 values for every color pixel
        int width = srcImage.getExtents()[0]; // width of slice in number of pixels (
        int height = srcImage.getExtents()[1]; // height of slice in number of pixels
        int sliceWidth = width * valuesPerPixel; // width of slice, which, in color images is (4*width)
        int sliceHeight = height; // height of image, which, actually doesn't change
        int index = 0; 

        float[] tempBuffer;

        float[] maskedList; // list of buffer-values that were showing inside the mask

        int row, col; // row and column vars for easier reading [(0,0) is in the top-left corner]
        int mod; // 1% length of slice for percent complete

        // these bounds "frame" the interior of the slice which may be filtered (&adjusted);
        // image outside the frame may not
        int upperBound, lowerBound, // bounds on the row
            leftBound, rightBound; // bounds on the column
        
        upperBound = halfK[0];
        leftBound = halfK[0] * 4;
        lowerBound = sliceHeight - halfK[0] - 1;
        rightBound = sliceWidth - (halfK[0] * 4) - 1;

        // data element at the buffer (a = i+bufferStartingPoint) must start on an alpha value
        buffStart = bufferStartingPoint - (bufferStartingPoint % 4); // & no effect if bufferStartingPoint%4 == 0
                                                                     // !!!

        // copy all alpha values in this slice
        setCopyColorText("alpha");

        for (a = buffStart, i = 0; i < imageSliceLength; a += 4, i += 4) {
            destBuffer[a] = srcBuffer[a]; // copy alpha;
        }
        
        mod = (imageSliceLength * numberOfSlices) / 100; // mod is 1 percent of length of slice * the number of slices.

        for (pass = 0; (pass < iterations) && !threadStopped; pass++) {
            
            if ((numberOfSlices > 1)) { // 3D image     update progressBar
                fireProgressStateChanged( (((float) (currentSlice + (pass * numberOfSlices)) /
                        (iterations * numberOfSlices))), null, null);
            }
            a = buffStart; // set/reset a to address pixels from the beginning of this buffer.


                
            fireProgressStateChanged(-1, null, "Filtering " + msgString + " (pass " + String.valueOf(pass + 1) +
                    " of " + iterations + ") ...");

                        // if we needed to filter the image, we dropped through the selection to filter the
                        // color given by int initialIndex
                        for (i = 0; (i < imageSliceLength) && !threadStopped; a += 4, i += 4) {

                            if (numberOfSlices == 1) { // 2D image     update progressBar

                                if (((i % mod) == 0)) {
                                    fireProgressStateChanged( ((float) ((pass * sliceLength) +
                                            (i / 4)) /
                                            (iterations * sliceLength)), null, null);
                                }
                            }

                            if ((entireImage == true) || mask.get(a / 4)) { // may have problems in masking .....
                                row = i / sliceWidth;
                                col = i % sliceWidth;

                                if ((row < upperBound) || (row > lowerBound)) {
                                    destBuffer[a+1] = srcBuffer[a+1]; // row too far up or down--out of bounds
                                    destBuffer[a+2] = srcBuffer[a+2];
                                    destBuffer[a+3] = srcBuffer[a+3];
                                } else if ((col < leftBound) || (col > rightBound)) {
                                    destBuffer[a+1] = srcBuffer[a+1]; // column too far left or right--out of bounds
                                    destBuffer[a+2] = srcBuffer[a+2];
                                    destBuffer[a+3] = srcBuffer[a+3];
                                } else { // in bounds
                                    maskedList = getVectorNeighborList(0, a, srcBuffer, true);

                                    index = vectorDirectionMedian(maskedList);
                                    destBuffer[a+1] = maskedList[index];
                                    destBuffer[a+2] = maskedList[index+1];
                                    destBuffer[a+3] = maskedList[index+2];
                                }
                            } else { // not part of the VOI so just copy this into the destination buffer.
                                destBuffer[a+1] = srcBuffer[a+1];
                                destBuffer[a+2] = srcBuffer[a+2];
                                destBuffer[a+3] = srcBuffer[a+3];
                            }
                        }

                    
            // now set up for the repeat for multiple iterations.
            // But only bother with copying over if there are more iterations.
            if (pass < (iterations - 1)) {
                tempBuffer = destBuffer; // swap dest & src buffers
                destBuffer = srcBuffer;
                srcBuffer = tempBuffer;
            }
        }
        // destBuffer should now be copied over for the size of imageSliceLength.  You may return....

    }

    /**
     * Allows a single slice to be filtered. Note that a progressBar must be created first.
     *
     * @param  srcBdrBuffer             float[] Source buffer.
     * @param  destBuffer               float[] Destination Buffer.
     * @param  srcBufferStartingPoint   Starting point for the buffer.
     * @param  destBufferStartingPoint  Starting point for the buffer.
     */
    private void sliceFilterBorder(float[] srcBdrBuffer, float[] destBuffer, int srcBufferStartingPoint,
                                   int destBufferStartingPoint) {
    	int pass;
        int kn;
        boolean loop;
        float a1, a2, b1, b2;
        float zmin, zmed, zmax;
        int x, y;

        // space allocated for extra rows used by the kernel
        int srcBrdBufferKernelOffset = (halfK[kernelNumber-1] * bdrBufferWidth) + halfK[kernelNumber-1];

        // space for previous slices when slice filtering a 3D volume
        int srcBdrBufferOffset = srcBufferStartingPoint + srcBrdBufferKernelOffset;

        int srcBdrBufferIdx;
        int destRow, destCol, destBufferIdx;
        float[] maskedList;
        
        float average; // arithmetic mean
        float sigma; // standard deviation
        int kCenter = maskCenter[0]; // to find the middle pixel of the kernel-mask
        
        for (pass = 0; (pass < iterations) && !threadStopped; pass++) {
            destBufferIdx = destBufferStartingPoint;
	        for (destRow = 0; destRow < srcBufferHeight; destRow++) {
	        	fireProgressStateChanged(10 + (80*pass)/iterations + (80* destRow) / (iterations *(srcBufferHeight - 1)));
	            srcBdrBufferIdx = (destRow * bdrBufferWidth) + srcBdrBufferOffset;
	
	            for (destCol = 0; destCol < srcBufferWidth; destCol++, srcBdrBufferIdx++, destBufferIdx++) {
	
	                if (entireImage || mask.get(destBufferIdx)) {
	                    if (adaptiveSize) {
	                        kn = 0;
	                        loop = true;
	                        while (loop) {
	                             maskedList = getBorderBufferNeighborList(kn++, srcBdrBufferIdx, srcBdrBuffer, true);
	                             Arrays.sort(maskedList);
	                             zmed = median(maskedList);
	                             zmin = maskedList[0];
	                             zmax = maskedList[maskedList.length-1];
	                             a1 = zmed - zmin;
	                             a2 = zmed - zmax;
	                             if ((a1 > 0.0f) && (a2 < 0.0f)) {
	                                 loop = false;
	                                 b1 = srcBdrBuffer[srcBdrBufferIdx] - zmin;
	                                 b2 = srcBdrBuffer[srcBdrBufferIdx] - zmax;
	                                 if ((b1 > 0.0f) && (b2 < 0.0f)) {
	                                     destBuffer[destBufferIdx] = srcBdrBuffer[srcBdrBufferIdx];
	                                 }
	                                 else {
	                                     destBuffer[destBufferIdx] = zmed;
	                                 }
	                             } // if (a1 > 0.0f_ && (a2 < 0.0f))
	                             else if (kn == kernelNumber) {
	                                 loop = false;
	                                 destBuffer[destBufferIdx] = zmed;
	                             }
	                        } // while (loop)         
	                    } // if (adaptiveSize)
	                    else { // not adaptiveSize
	                    	
	                        maskedList = getBorderBufferNeighborList(0, srcBdrBufferIdx, srcBdrBuffer, true);
	                        
	                        //verify that this element is an outlier
	                        if (stdDevLimit == 0.0) { // anything is an outlier
	                        	Arrays.sort(maskedList);
	                            destBuffer[destBufferIdx] = median(maskedList);
	                        } else { // look for outlierness
	                            average = mean(maskedList);
	                            sigma = standardDeviation(maskedList, average);
	
	                            if ((maskedList[kCenter] > (average + (stdDevLimit * sigma))) ||
	                                    (maskedList[kCenter] < (average - (stdDevLimit * sigma)))) {
	                            	Arrays.sort(maskedList);
	                                destBuffer[destBufferIdx] = median(maskedList);
	                            } else { // if element was not an outlier, pixel is fine.
	                                destBuffer[destBufferIdx] = srcBdrBuffer[srcBdrBufferIdx];
	                            }
	                        }
	                        
	                    } // else not adaptiveSize
	                } else {
	                    destBuffer[destBufferIdx] = srcBdrBuffer[srcBdrBufferIdx];
	                }
	            }
	        }
	        // now set up for the repeat for multiple iterations.
            // But only bother with copying over if there are more iterations.
            if (pass < (iterations - 1)) {
                for ( y = 0; y < srcBufferHeight; y++) {
                	for (x = 0; x < srcBufferWidth; x++) {
                		srcBdrBuffer[(x + halfK[0]) + bdrBufferWidth*(y + halfK[0])] = destBuffer[x + srcBufferWidth*y];
                	}
                }
            }
        }
    }

    /**
     * Finds the standard deviation of the values in the input list (defined as: s = [(1/(N-1))*SUM (from 0 to N-1)[ (Xi
     * - <bold>X</bold>)^2]]^(1/2)).
     *
     * @param   list     float [] The list of numbers.
     * @param   average  Arithmetic mean of the values in list.
     *
     * @return  The standard deviation.
     */
    private float standardDeviation(float[] list, float average) {
        int i;
        int N = list.length;

        double sum = 0.0;

        for (i = 0; i < N; i++) {
            sum += (list[i] - average) * (list[i] - average);
        }

        return ((float) Math.sqrt(sum / (N - 1)));
    }

    /**
     * Filter a Color 3D image with a 3D kernel.
     * Allows median filtering to include the picture elements at greater
     * depths than only the current slice. This method allows selected band filtering,
     * and does not filter the alpha band.
     *
     * @param  srcBuffer   float [] Source image.
     * @param  destBuffer  float [] Destination image.
     *
     * @see    volumeFilter
     */
    private void volumeColorFilter(float[] srcBuffer, float[] destBuffer) {
        int i, pass; // counting the current element
        int initialIndex; // reference to the color band being filtered/copied: aRBG: 0, 1, 2, 3;

        // it is an offset to the identified pixel, or column, of the slice
        int row, // ease of reading to find the row, column and slice
            column, // (all starting at 0) associated with the current element
            slice; // [(0,0,0) starts at the closest upper-left corner]
        int kCenter = maskCenter[0];
        int width = srcImage.getExtents()[0]; // width of slice in number of pixels
        int height = srcImage.getExtents()[1]; // height of slice in number of pixels
        int sliceWidth = width * valuesPerPixel; // width of slice in number of intensity values

        // (as in colors per pixel (1 for mono, 4 for color))
        int sliceSize = width * height; // in pixels (or elements)
        int imageSliceLength = width * height * valuesPerPixel; // in values-pixels
        int imageSize = sliceSize * numberOfSlices; // in pixels (or elements)
        int imageLength = imageSliceLength * numberOfSlices; // in (values-pixels)
        float[] tempBuffer;

        float average; // arithmetic mean
        float sigma; // standard deviation

        float[] maskedList; // list of buffer-values that were showing inside the mask
        
        int kn;
        boolean loop;
        float a1, a2, b1, b2;
        float zmin, zmed, zmax;

        // these bounds "frame" the interior of the slice which may be filtered (&adjusted);
        // image outside the frame may not
        int leftBound, rightBound, // bounds on the column
            upperBound, lowerBound, // bounds on the row
            aheadBound, behindBound; // bounds on the slice

        // (a note on orientation: object front is facing in the same direction as
        // viewer, thus ahead of viewer is into monitor, behind is out of monitor and
        // a more positive number of slices is farther forward.)
        upperBound = halfK[kernelNumber-1];
        lowerBound = height - halfK[kernelNumber-1] - 1;
        behindBound = halfK[kernelNumber-1];
        aheadBound = numberOfSlices - halfK[kernelNumber-1] - 1;

        // we may say that each column is a pixel intensity: mono images have 1 per pixel, 4 in color;
        // these calculations are done seperately for color & mono images in sliceFilter().
        leftBound = halfK[kernelNumber-1] * valuesPerPixel;
        rightBound = sliceWidth - (valuesPerPixel * halfK[kernelNumber-1]) - 1; // in color: (4*width - 4*halfK - 1); mono: (width -
                                                                // halfK - 1)

        int mod = (imageSize) / 100; // mod is 1 percent of length of slice * the number of slices.

        // copy all alpha values in the image
        setCopyColorText("alpha");

        for (i = 0; i < imageLength; i += 4) {
            destBuffer[i] = srcBuffer[i]; // copy alpha;
        }

        // choose i so the proper colors go alongside the initial index
        // so we get the right output statements in the progress bar
        // copy only needed RGB values
        initialIndex = 0; // start with alpha on each pass (routine moved so we don't do it for each pass)

        while ((initialIndex < 3) && !threadStopped) { // alpha:0, R:1, G:2, B:3.  But alpha must be copied
            ++initialIndex; // next initial index

            if (!rChannel && (initialIndex == 1)) {

                // when looking at the image reds but we're not filtering the red channel
                // copy all red values
                setCopyColorText("red");

                for (i = initialIndex; i < imageLength; i += 4) {
                    destBuffer[i] = srcBuffer[i];
                }
            } else if (!gChannel && (initialIndex == 2)) {

                // when looking at the image greens but we're not filtering the greens channel
                // copy all greens values
                setCopyColorText("green");

                for (i = initialIndex; i < imageLength; i += 4) {
                    destBuffer[i] = srcBuffer[i];
                }
            } else if (!bChannel && (initialIndex == 3)) {

                // when looking at the image blues but we're  not filtering the blues channel
                // copy all blue values
                setCopyColorText("blue");

                for (i = initialIndex; i < imageLength; i += 4) {
                    destBuffer[i] = srcBuffer[i];
                }
            } else {

                for (pass = 0; (pass < iterations) && !threadStopped; pass++) {

                    
                        if (initialIndex == 1) {
                            fireProgressStateChanged(-1, null, "Filtering red channel (pass " + String.valueOf(pass + 1) + " of " +
                                    iterations + ") ...");

                        } else if (initialIndex == 2) {
                            fireProgressStateChanged(-1, null, "Filtering green channel (pass " + String.valueOf(pass + 1) + " of " +
                                    iterations + ") ...");
                        } else if (initialIndex == 3) {
                            fireProgressStateChanged(-1, null, "Filtering blue channel (pass " + String.valueOf(pass + 1) + " of " +
                                    iterations + ") ...");
                        }

                    // if we needed to filter the image, we dropped through the selection to filter the
                    // color given by int initialIndex
                    for (i = initialIndex; (i < imageLength) && !threadStopped; i += 4) {

                        if ((((i - initialIndex) % mod) == 0)) {
                            fireProgressStateChanged( ((float) ((iterations * (initialIndex - 1) * imageSize) +
                                    (imageSize * pass) + (i / 4)) /
                                    (3 * iterations * imageSize)), null, null);
                        }

                        if ((entireImage == true) || mask.get(i / valuesPerPixel)) {

                            // Median stuff here
                            slice = i / imageSliceLength;
                            row = (i % imageSliceLength) / sliceWidth;
                            column = i % sliceWidth;

                            if ((row < upperBound) || (row > lowerBound)) {
                                destBuffer[i] = srcBuffer[i]; // row too far up or down--out of bounds
                            } else if ((column < leftBound) || (column > rightBound)) {
                                destBuffer[i] = srcBuffer[i]; // column too far left or right--out of bounds
                            } else if ((slice < behindBound) || (slice > aheadBound)) {
                                destBuffer[i] = srcBuffer[i]; // slice too far ahead or behind--out of bounds
                            } else { // in bounds
                                if (adaptiveSize) {
                                    kn = 0;
                                    loop = true;
                                    while (loop) {
                                         maskedList = getNeighborList(kn++, i, srcBuffer, false);
                                         Arrays.sort(maskedList);
                                         zmed = median(maskedList);
                                         zmin = maskedList[0];
                                         zmax = maskedList[maskedList.length-1];
                                         a1 = zmed - zmin;
                                         a2 = zmed - zmax;
                                         if ((a1 > 0.0f) && (a2 < 0.0f)) {
                                             loop = false;
                                             b1 = srcBuffer[i] - zmin;
                                             b2 = srcBuffer[i] - zmax;
                                             if ((b1 > 0.0f) && (b2 < 0.0f)) {
                                                 destBuffer[i] = srcBuffer[i];
                                             }
                                             else {
                                                 destBuffer[i] = zmed;
                                             }
                                         } // if (a1 > 0.0f_ && (a2 < 0.0f))
                                         else if (kn == kernelNumber) {
                                             loop = false;
                                             destBuffer[i] = zmed;
                                         }
                                    } // while (loop)             
                                } // if (adaptiveSize)
                                else { // not adaptiveSize
                                    maskedList = getNeighborList(0, i, srcBuffer, false);
    
                                    // verify that this element is an outlier
                                    if (stdDevLimit == 0.0) { // anything is an outlier
                                    	Arrays.sort(maskedList);
                                        destBuffer[i] = median(maskedList);
                                    } else { // look for outlierness
                                        average = mean(maskedList);
                                        sigma = standardDeviation(maskedList, average);
    
                                        if ((maskedList[kCenter] > (average + (stdDevLimit * sigma))) ||
                                                (maskedList[kCenter] < (average - (stdDevLimit * sigma)))) {
                                        	Arrays.sort(maskedList);
                                            destBuffer[i] = median(maskedList);
                                        } else { // if element was not an outlier, pixel is fine.
                                            destBuffer[i] = srcBuffer[i];
                                        }
                                    }
                                }
                            } // else not adaptiveSize
                        } else { // not part of the VOI so just copy this into the destination buffer.
                            destBuffer[i] = srcBuffer[i];
                        }
                    }

                    // now set up for the repeat for multiple iterations.
                    // But only bother with copying over if there are more iterations.
                    if (pass < (iterations - 1)) {
                        tempBuffer = destBuffer; // swap src & dest buffer
                        destBuffer = srcBuffer;
                        srcBuffer = tempBuffer;
                    }
                }

                if ((iterations % 2) == 0) { // if even number of iterations, then
                    tempBuffer = destBuffer; // swap src & dest buffer is necessary
                    destBuffer = srcBuffer; // to keep other colours not-yet-filtered from
                    srcBuffer = tempBuffer; // filtering from the wrong buffer, overwriting the real src
                }
            }
        }
    }
    
    /**
     * Filter a Color 3D image with a 3D kernel with vector magnitude filtering.
     * Allows median filtering to include the picture elements at greater
     * depths than only the current slice. This method does not filter the alpha band.  Red,
     * green, and blue are all filtered together; that is, the new red, green, and blue will
     * all come from the same pixel.
     *
     * @param  srcBuffer   float [] Source image.
     * @param  destBuffer  float [] Destination image.
     *
     * @see    volumeFilter
     */
    private void volumeVectorMagnitudeColorFilter(float[] srcBuffer, float[] destBuffer) {
        int i, pass; // counting the current element
        int index;

        // it is an offset to the identified pixel, or column, of the slice
        int row, // ease of reading to find the row, column and slice
            column, // (all starting at 0) associated with the current element
            slice; // [(0,0,0) starts at the closest upper-left corner]
        int width = srcImage.getExtents()[0]; // width of slice in number of pixels
        int height = srcImage.getExtents()[1]; // height of slice in number of pixels
        int sliceWidth = width * valuesPerPixel; // width of slice in number of intensity values

        // (as in colors per pixel (1 for mono, 4 for color))
        int sliceSize = width * height; // in pixels (or elements)
        int imageSliceLength = width * height * valuesPerPixel; // in values-pixels
        int imageSize = sliceSize * numberOfSlices; // in pixels (or elements)
        int imageLength = imageSliceLength * numberOfSlices; // in (values-pixels)
        float[] tempBuffer;

        float[] maskedList; // list of buffer-values that were showing inside the mask

        // these bounds "frame" the interior of the slice which may be filtered (&adjusted);
        // image outside the frame may not
        int leftBound, rightBound, // bounds on the column
            upperBound, lowerBound, // bounds on the row
            aheadBound, behindBound; // bounds on the slice

        // (a note on orientation: object front is facing in the same direction as
        // viewer, thus ahead of viewer is into monitor, behind is out of monitor and
        // a more positive number of slices is farther forward.)
        upperBound = halfK[0];
        lowerBound = height - halfK[0] - 1;
        behindBound = halfK[0];
        aheadBound = numberOfSlices - halfK[0] - 1;

        // we may say that each column is a pixel intensity: mono images have 1 per pixel, 4 in color;
        // these calculations are done seperately for color & mono images in sliceFilter().
        leftBound = halfK[0] * valuesPerPixel;
        rightBound = sliceWidth - (valuesPerPixel * halfK[0]) - 1; // in color: (4*width - 4*halfK - 1); mono: (width -
                                                                // halfK - 1)

        int mod = (imageSize) / 100; // mod is 1 percent of length of slice * the number of slices.

        // copy all alpha values in the image
        setCopyColorText("alpha");

        for (i = 0; i < imageLength; i += 4) {
            destBuffer[i] = srcBuffer[i]; // copy alpha;
        }

        // choose i so the proper colors go alongside the initial index
        // so we get the right output statements in the progress bar
        // copy only needed RGB values   

                for (pass = 0; (pass < iterations) && !threadStopped; pass++) {

                    fireProgressStateChanged(-1, null, "Filtering pass " + String.valueOf(pass + 1) + " of " +
                            iterations + ") ...");

                    // if we needed to filter the image, we dropped through the selection to filter the
                    // color given by int initialIndex
                    for (i = 0; (i < imageLength) && !threadStopped; i += 4) {

                        if (((i % mod) == 0)) {
                            fireProgressStateChanged( ((float) ((imageSize * pass) + (i / 4)) /
                                    (iterations * imageSize)), null, null);
                        }

                        if ((entireImage == true) || mask.get(i / valuesPerPixel)) {

                            // Median stuff here
                            slice = i / imageSliceLength;
                            row = (i % imageSliceLength) / sliceWidth;
                            column = i % sliceWidth;

                            if ((row < upperBound) || (row > lowerBound)) {
                                destBuffer[i+1] = srcBuffer[i+1]; // row too far up or down--out of bounds
                                destBuffer[i+2] = srcBuffer[i+2];
                                destBuffer[i+3] = srcBuffer[i+3];
                            } else if ((column < leftBound) || (column > rightBound)) {
                                destBuffer[i+1] = srcBuffer[i+1]; // column too far left or right--out of bounds
                                destBuffer[i+2] = srcBuffer[i+2];
                                destBuffer[i+3] = srcBuffer[i+3];
                            } else if ((slice < behindBound) || (slice > aheadBound)) {
                                destBuffer[i+1] = srcBuffer[i+1]; // slice too far ahead or behind--out of bounds
                                destBuffer[i+2] = srcBuffer[i+2];
                                destBuffer[i+3] = srcBuffer[i+3];
                            } else { // in bounds
                                maskedList = getVectorNeighborList(0, i, srcBuffer, false);

                                index = vectorMagnitudeMedian(maskedList);
                                destBuffer[i+1] = maskedList[index];
                                destBuffer[i+2] = maskedList[index+1];
                                destBuffer[i+3] = maskedList[index+2];
                            }
                        } else { // not part of the VOI so just copy this into the destination buffer.
                            destBuffer[i+1] = srcBuffer[i+1];
                            destBuffer[i+2] = srcBuffer[i+2];
                            destBuffer[i+3] = srcBuffer[i+3];
                        }
                    }

                    // now set up for the repeat for multiple iterations.
                    // But only bother with copying over if there are more iterations.
                    if (pass < (iterations - 1)) {
                        tempBuffer = destBuffer; // swap src & dest buffer
                        destBuffer = srcBuffer;
                        srcBuffer = tempBuffer;
                    }
                }

                if ((iterations % 2) == 0) { // if even number of iterations, then
                    tempBuffer = destBuffer; // swap src & dest buffer is necessary
                    destBuffer = srcBuffer; // to keep other colours not-yet-filtered from
                    srcBuffer = tempBuffer; // filtering from the wrong buffer, overwriting the real src
                }
    }
    
    /**
     * Filter a Color 3D image with a 3D kernel with vector direction filtering.
     * Allows median filtering to include the picture elements at greater
     * depths than only the current slice. This method does not filter the alpha band.  Red,
     * green, and blue are all filtered together; that is, the new red, green, and blue will
     * all come from the same pixel.
     *
     * @param  srcBuffer   float [] Source image.
     * @param  destBuffer  float [] Destination image.
     *
     * @see    volumeFilter
     */
    private void volumeVectorDirectionColorFilter(float[] srcBuffer, float[] destBuffer) {
        int i, pass; // counting the current element
        int index;

        // it is an offset to the identified pixel, or column, of the slice
        int row, // ease of reading to find the row, column and slice
            column, // (all starting at 0) associated with the current element
            slice; // [(0,0,0) starts at the closest upper-left corner]
        int width = srcImage.getExtents()[0]; // width of slice in number of pixels
        int height = srcImage.getExtents()[1]; // height of slice in number of pixels
        int sliceWidth = width * valuesPerPixel; // width of slice in number of intensity values

        // (as in colors per pixel (1 for mono, 4 for color))
        int sliceSize = width * height; // in pixels (or elements)
        int imageSliceLength = width * height * valuesPerPixel; // in values-pixels
        int imageSize = sliceSize * numberOfSlices; // in pixels (or elements)
        int imageLength = imageSliceLength * numberOfSlices; // in (values-pixels)
        float[] tempBuffer;

        float[] maskedList; // list of buffer-values that were showing inside the mask

        // these bounds "frame" the interior of the slice which may be filtered (&adjusted);
        // image outside the frame may not
        int leftBound, rightBound, // bounds on the column
            upperBound, lowerBound, // bounds on the row
            aheadBound, behindBound; // bounds on the slice

        // (a note on orientation: object front is facing in the same direction as
        // viewer, thus ahead of viewer is into monitor, behind is out of monitor and
        // a more positive number of slices is farther forward.)
        upperBound = halfK[0];
        lowerBound = height - halfK[0] - 1;
        behindBound = halfK[0];
        aheadBound = numberOfSlices - halfK[0] - 1;

        // we may say that each column is a pixel intensity: mono images have 1 per pixel, 4 in color;
        // these calculations are done seperately for color & mono images in sliceFilter().
        leftBound = halfK[0] * valuesPerPixel;
        rightBound = sliceWidth - (valuesPerPixel * halfK[0]) - 1; // in color: (4*width - 4*halfK - 1); mono: (width -
                                                                // halfK - 1)

        int mod = (imageSize) / 100; // mod is 1 percent of length of slice * the number of slices.

        // copy all alpha values in the image
        setCopyColorText("alpha");

        for (i = 0; i < imageLength; i += 4) {
            destBuffer[i] = srcBuffer[i]; // copy alpha;
        }

        // choose i so the proper colors go alongside the initial index
        // so we get the right output statements in the progress bar
        // copy only needed RGB values   

                for (pass = 0; (pass < iterations) && !threadStopped; pass++) {
                  
                    fireProgressStateChanged(-1, null, "Filtering pass " + String.valueOf(pass + 1) + " of " +
                            iterations + ") ...");

                    // if we needed to filter the image, we dropped through the selection to filter the
                    // color given by int initialIndex
                    for (i = 0; (i < imageLength) && !threadStopped; i += 4) {

                        if (((i % mod) == 0)) {
                            fireProgressStateChanged( ((float) ((imageSize * pass) + (i / 4)) /
                                    (iterations * imageSize)), null, null);
                        }

                        if ((entireImage == true) || mask.get(i / valuesPerPixel)) {

                            // Median stuff here
                            slice = i / imageSliceLength;
                            row = (i % imageSliceLength) / sliceWidth;
                            column = i % sliceWidth;

                            if ((row < upperBound) || (row > lowerBound)) {
                                destBuffer[i+1] = srcBuffer[i+1]; // row too far up or down--out of bounds
                                destBuffer[i+2] = srcBuffer[i+2];
                                destBuffer[i+3] = srcBuffer[i+3];
                            } else if ((column < leftBound) || (column > rightBound)) {
                                destBuffer[i+1] = srcBuffer[i+1]; // column too far left or right--out of bounds
                                destBuffer[i+2] = srcBuffer[i+2];
                                destBuffer[i+3] = srcBuffer[i+3];
                            } else if ((slice < behindBound) || (slice > aheadBound)) {
                                destBuffer[i+1] = srcBuffer[i+1]; // slice too far ahead or behind--out of bounds
                                destBuffer[i+2] = srcBuffer[i+2];
                                destBuffer[i+3] = srcBuffer[i+3];
                            } else { // in bounds
                                maskedList = getVectorNeighborList(0, i, srcBuffer, false);

                                index = vectorDirectionMedian(maskedList);
                                destBuffer[i+1] = maskedList[index];
                                destBuffer[i+2] = maskedList[index+1];
                                destBuffer[i+3] = maskedList[index+2];
                            }
                        } else { // not part of the VOI so just copy this into the destination buffer.
                            destBuffer[i+1] = srcBuffer[i+1];
                            destBuffer[i+2] = srcBuffer[i+2];
                            destBuffer[i+3] = srcBuffer[i+3];
                        }
                    }

                    // now set up for the repeat for multiple iterations.
                    // But only bother with copying over if there are more iterations.
                    if (pass < (iterations - 1)) {
                        tempBuffer = destBuffer; // swap src & dest buffer
                        destBuffer = srcBuffer;
                        srcBuffer = tempBuffer;
                    }
                }

                if ((iterations % 2) == 0) { // if even number of iterations, then
                    tempBuffer = destBuffer; // swap src & dest buffer is necessary
                    destBuffer = srcBuffer; // to keep other colours not-yet-filtered from
                    srcBuffer = tempBuffer; // filtering from the wrong buffer, overwriting the real src
                }
    }

    /**
     * Filter a 3D image with a 3D kernel. Allows median filtering to include the picture elements at greater depths
     * than only the current slice.
     *
     * <p><em>Note that this volume filter will correctly filter color images on all bands (aRGB) because the neighbor
     * list is correct (see getNeighborList()). This means, however, it will not selectivly filter any bands (one may
     * not filter only the Red channel, for instance), and will also filter all alpha values as well. Of course,
     * progress bar updates will not include any color information. For these reasons it a useable, but limited color
     * filter.</em></p>
     *
     * @param  srcBuffer   float[] Source image.
     * @param  destBuffer  float [] Destination image.
     */
    // some code has been left in to allow this method to properly filter
    // color images, although the other method is included.
    private void volumeFilter(float[] srcBuffer, float[] destBuffer) {
        int i, pass; // counting the current element
        int row, // ease of reading to find the row, column and slice
            column, // (all starting at 0) associated with the current element
            slice; // [(0,0,0) starts at the closest upper-left corner]
        int imageSliceLength = srcImage.getSliceSize() * valuesPerPixel;
        int imageLength = imageSliceLength * numberOfSlices;
        int kCenter = maskCenter[0];
        int width = srcImage.getExtents()[0]; // width of slice in number of pixels
        int height = srcImage.getExtents()[1]; // height of slice in number of pixels
        int sliceWidth = width * valuesPerPixel; // width of slice in number of intensity values

        // (as in colors per pixel (1 for mono, 4 for color))
        float[] tempBuffer;

        float average; // arithmetic mean
        float sigma; // standard deviation

        float[] maskedList; // list of buffer-values that were showing inside the mask
        
        int kn;
        boolean loop;
        float a1, a2, b1, b2;
        float zmin, zmed, zmax;

        // these bounds "frame" the interior of the slice which may be filtered (&adjusted);
        // image outside the frame may not
        int leftBound, rightBound, // bounds on the column
            upperBound, lowerBound, // bounds on the row
            aheadBound, behindBound; // bounds on the slice

        // (a note on orientation: object front is facing in the same direction as
        // viewer, thus ahead of viewer is into monitor, behind is out of monitor and
        // a more positive number of slices is farther forward.)
        upperBound = halfK[kernelNumber-1];
        lowerBound = height - halfK[kernelNumber-1] - 1;
        behindBound = halfK[kernelNumber-1];
        aheadBound = numberOfSlices - halfK[kernelNumber-1] - 1;

        // we may say that each column is a pixel intensity: mono images have 1 per pixel, 4 in color;
        // these calculations are done seperately for color & mono images in sliceFilter().
        leftBound = halfK[kernelNumber-1] * valuesPerPixel;
        rightBound = sliceWidth - (valuesPerPixel * halfK[kernelNumber-1]) - 1; // in color: (4*width - 4*halfK - 1); mono: (width -
                                                                // halfK - 1)

        int mod = (imageLength) / 100; // mod is 1 percent of length of slice * the number of slices.

        for (pass = 0; (pass < iterations) && !threadStopped; pass++) {

            fireProgressStateChanged(-1, null, "Filtering image (pass " + String.valueOf(pass + 1) + " of " + iterations +
                    ") ...");

            for (i = 0; (i < imageLength) && !threadStopped; i++) {

                if (((i % mod) == 0)) {
                    fireProgressStateChanged( ((float) ((pass * imageLength) + i) /
                            (iterations * imageLength)), null, null);
                }

                if ((entireImage == true) || mask.get(i / valuesPerPixel)) {

                    // Median stuff here
                    slice = i / imageSliceLength;
                    row = (i % imageSliceLength) / sliceWidth;
                    column = i % sliceWidth;

                    if ((row < upperBound) || (row > lowerBound)) {
                        destBuffer[i] = srcBuffer[i]; // row too far up or down--out of bounds
                    } else if ((column < leftBound) || (column > rightBound)) {
                        destBuffer[i] = srcBuffer[i]; // column too far left or right--out of bounds
                    } else if ((slice < behindBound) || (slice > aheadBound)) {
                        destBuffer[i] = srcBuffer[i]; // slice too far ahead or behind--out of bounds
                    } else { // in bounds
                        if (adaptiveSize) {
                            kn = 0;
                            loop = true;
                            while (loop) {
                                 maskedList = getNeighborList(kn++, i, srcBuffer, false);
                                 Arrays.sort(maskedList);
                                 zmed = median(maskedList);
                                 zmin = maskedList[0];
                                 zmax = maskedList[maskedList.length-1];
                                 a1 = zmed - zmin;
                                 a2 = zmed - zmax;
                                 if ((a1 > 0.0f) && (a2 < 0.0f)) {
                                     loop = false;
                                     b1 = srcBuffer[i] - zmin;
                                     b2 = srcBuffer[i] - zmax;
                                     if ((b1 > 0.0f) && (b2 < 0.0f)) {
                                         destBuffer[i] = srcBuffer[i];
                                     }
                                     else {
                                         destBuffer[i] = zmed;
                                     }
                                 } // if (a1 > 0.0f_ && (a2 < 0.0f))
                                 else if (kn == kernelNumber) {
                                     loop = false;
                                     destBuffer[i] = zmed;
                                 }
                            } // while (loop)                 
                        } // if (adaptiveSize)
                        else { // not adaptiveSize
                            maskedList = getNeighborList(0, i, srcBuffer, false);
    
                            // verify that this element is an outlier
                            if (stdDevLimit == 0.0) { // anything is an outlier
                            	Arrays.sort(maskedList);
                                destBuffer[i] = median(maskedList);
                            } else { // look for outlierness
                                average = mean(maskedList);
                                sigma = standardDeviation(maskedList, average);
    
                                if ((maskedList[kCenter] > (average + (stdDevLimit * sigma))) ||
                                        (maskedList[kCenter] < (average - (stdDevLimit * sigma)))) {
                                	Arrays.sort(maskedList);
                                    destBuffer[i] = median(maskedList);
                                } else { // if element was not an outlier, pixel is fine.
                                    destBuffer[i] = srcBuffer[i];
                                }
                            }
                        }
                    } // else not adaptiveSize
                } else { // not part of the VOI so just copy this into the destination buffer.
                    destBuffer[i] = srcBuffer[i];
                }
            }

            // now set up for the repeat for multiple iterations.
            // But only bother with copying over if there are more iterations.
            if (pass < (iterations - 1)) {
                tempBuffer = destBuffer; // swap src & dest buffer
                destBuffer = srcBuffer;
                srcBuffer = tempBuffer;
            }
        }
    }

    /**
     * Method performs median filtering on a volume that has been centerend in a border buffer and places the result in
     * the destination buffer.
     *
     * @param  srcBdrBuffer  float[]
     * @param  destBuffer    float[]
     */

    private void volumeFilterBorder(float[] srcBdrBuffer, float[] destBuffer) {

    	int pass;
    	int x, y, z;
    	// space allocated for extra slices, rows, and cols used by the kernel
        int srcBrdBufferKernelOffset = (halfK[kernelNumber-1] * bdrBufferWidth * bdrBufferHeight) + 
                                       (halfK[kernelNumber-1] * bdrBufferWidth) + halfK[kernelNumber-1];

        float[] maskedList;
        
        int kn;
        boolean loop;
        float a1, a2, b1, b2;
        float zmin, zmed, zmax;

        int srcSliceLength = srcBufferWidth * srcBufferHeight;
        int srcBdrBufferSliceLength = bdrBufferWidth * bdrBufferHeight;
        int srcBdrBufferIdx, srcBdrBufferSliceOffset;
        int destSlice, destRow, destCol, destBufferIdx;
        for (pass = 0; (pass < iterations) && !threadStopped; pass++) {
            destBufferIdx = 0;
	        for (destSlice = 0; (destSlice < srcBufferDepth) && !threadStopped; destSlice++) {
	
	            fireProgressStateChanged(10 + (80*pass)/iterations + (80* destSlice) / (iterations *(srcBufferDepth - 1)));
	
	            srcBdrBufferSliceOffset = (destSlice * srcBdrBufferSliceLength) + srcBrdBufferKernelOffset;
	
	            for (destRow = 0; destRow < srcBufferHeight; destRow++) {
	
	                srcBdrBufferIdx = (destRow * bdrBufferWidth) + srcBdrBufferSliceOffset;
	
	                for (destCol = 0; destCol < srcBufferWidth; destCol++, srcBdrBufferIdx++, destBufferIdx++) {
	                    if (entireImage || mask.get(destBufferIdx)) {
	                        if (adaptiveSize) {
	                            kn = 0;
	                            loop = true;
	                            while (loop) {
	                                 maskedList = getBorderBufferNeighborList(kn++, srcBdrBufferIdx, srcBdrBuffer, false);
	                                 Arrays.sort(maskedList);
	                                 zmed = median(maskedList);
	                                 zmin = maskedList[0];
	                                 zmax = maskedList[maskedList.length-1];
	                                 a1 = zmed - zmin;
	                                 a2 = zmed - zmax;
	                                 if ((a1 > 0.0f) && (a2 < 0.0f)) {
	                                     loop = false;
	                                     b1 = srcBdrBuffer[srcBdrBufferIdx] - zmin;
	                                     b2 = srcBdrBuffer[srcBdrBufferIdx] - zmax;
	                                     if ((b1 > 0.0f) && (b2 < 0.0f)) {
	                                         destBuffer[destBufferIdx] = srcBdrBuffer[srcBdrBufferIdx];
	                                     }
	                                     else {
	                                         destBuffer[destBufferIdx] = zmed;
	                                     }
	                                 } // if (a1 > 0.0f_ && (a2 < 0.0f))
	                                 else if (kn == kernelNumber) {
	                                     loop = false;
	                                     destBuffer[destBufferIdx] = zmed;
	                                 }
	                            } // while (loop)             
	                        } // if (adaptiveSize)
	                        else { // not adaptiveSize
	                            maskedList = getBorderBufferNeighborList(0, srcBdrBufferIdx, srcBdrBuffer, false);
	                            Arrays.sort(maskedList);
	                            destBuffer[destBufferIdx] = median(maskedList);
	                        } // else not adaptiveSize
	                    } else {
	                        destBuffer[destBufferIdx] = srcBdrBuffer[srcBdrBufferIdx];
	                    }
	                }
	            }
	        }
	    }
        if (pass < (iterations - 1)) {
        	for (z = 0; z < srcBufferDepth; z++) {
	            for ( y = 0; y < srcBufferHeight; y++) {
	            	for (x = 0; x < srcBufferWidth; x++) {
	            		srcBdrBuffer[(x + halfK[0]) + bdrBufferWidth*(y + halfK[0]) + 
	            		             srcBdrBufferSliceLength*(z + halfK[0])] = 
	            	    destBuffer[x + srcBufferWidth*y + srcSliceLength * z];
	            	}
	            }
        	}
        }
    }
}
