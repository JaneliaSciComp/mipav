package gov.nih.mipav.model.algorithms.filters;


import gov.nih.mipav.model.algorithms.*;
import gov.nih.mipav.model.structures.*;

import java.io.*;
import java.util.Arrays;


/**
 * AlgorithmMode applies one of the user specified kernels to a 2D or 3D BYTE, SHORT, INTEGER, UBYTE, USHORT, UINTEGER.
 * AlgorithmMode is a spatial filtering technique that replaces the central kernel pixel with the mode of the pixels
 * under the kernelMask. The mode of a set of values is the value that occurs most frequently. If all values are unique,
 * that is, all numbers occur once in the kernelMask, the algorithm does NOT change the value of the central pixel.
 *
 * <p>Mode filtering is currently thought to be useful in filtering a segmented image to get rid of small clusters of
 * one value within a large cluser of a different value. We are currently investigating mode filtering in cleaning up a
 * C-Means classification of brain images, which were detected using the Brain Extraction Tool (BET). Mode filtering can
 * be applied to gray scale images to remove some noise, however, we have found that a 3X3X3 cube kernel significantly
 * blurs the image, while an axis kernel appear to clean up some of the noise without too much blurring. The utility
 * more filtering on gray scale images need more investigation.</p>
 *
 * <p>Giving credit where credit is due, this algorithm and its associated dialog, JDialogMode started with the
 * AlgorithmMedian and JDialogMedian code. The color image stuff, iteration control, and standard deviation were
 * removed. Furthermore, all processing is done using integer images and buffers, as finding the mode of floating (real)
 * numbers is not meaningful.</p>
 *
 * @author   Paul Hemler
 * @version  1.0 June 24, 2004
 */
public class AlgorithmMode extends AlgorithmBase {

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

    /** Used for 3D loop control. */
    private int currentSlice = 0;

    /** true means apply to entire image, false only region. */
    private boolean entireImage;

    /** The kernel radius. */
    private int halfK;

    /** mask to determine the region of pixels used in the mode filter. */
    private byte[] kernel;

    /** The index of the kernel center. */
    private int kernelCenter;

    /** The kernel. */
    private int[] kernelMask;

    /** user-selectable shape of the region for neighbor-selection. */
    private int kernelShape;

    /** dimension of the kernel (ie., 5 = 5x5, 7 = 7x7, 9 = 9x9, etc.). */
    private int kernelSize;

    /** Used for 3D loop control. */
    private int numberOfSlices;

    /** do all filtering slice-by-slice, rather than as a volume. */
    private boolean sliceFiltering;

    //~ Constructors ---------------------------------------------------------------------------------------------------

    /**
     * Constructor for 2D images in which changes are returned to the source image.
     *
     * @param  srcImg    Source image model.
     * @param  kSize     Kernel size: dimension of the kernel (ie., 5 = 5x5, 7 = 7x7, 9 = 9x9, etc.).
     * @param  kShape    Kernel shape: element neighbors to include when finding the mode.
     * @param  maskFlag  Flag that indicates that the mode filtering will be performed for the whole image if equal to
     *                   true.
     */
    public AlgorithmMode(ModelImage srcImg, int kSize, int kShape, boolean maskFlag) {
        super(null, srcImg);

        if (srcImg.isColorImage()) {
            displayError("AlgorithmMode does NOT work on color images");

            return;
        }

        if (srcImg.getNDims() != 2) {
            displayError("AlgorithmMode 2D constructor called without a 2D image");

            return;
        }

        entireImage = maskFlag;
        kernelSize = kSize; // dimension of the kernel
        kernelShape = kShape; // set up the mask (kernel) used to filter
        sliceFiltering = true; // as a default--though a different value doesn't make much sense in a 2D application.
        numberOfSlices = 1; // 2D images may only have 1 slice.
        makeKernel();

        // makeKernel sets halfK, which we now use
        srcBufferWidth = srcImage.getExtents()[0];
        srcBufferHeight = srcImage.getExtents()[1];
        srcBufferDepth = 1;

        // allow for a border of halfK around the source image
        bdrBufferWidth = srcBufferWidth + (2 * halfK);
        bdrBufferHeight = srcBufferHeight + (2 * halfK);
        bdrBufferDepth = 1;

        if (entireImage == false) {
            mask = srcImage.generateVOIMask();
        }
    }

    /**
     * Constructor for 2D images in which changes are placed in a predetermined destination image.
     *
     * @param  destImg   Image model where result image is stored.
     * @param  srcImg    Source image model.
     * @param  kSize     Kernel size: dimension of the kernel (ie., 5 = 5x5, 7 = 7x7, 9 = 9x9, etc.).
     * @param  kShape    Kernel shape: element neighbors to include when finding the mode.
     * @param  maskFlag  Flag that indicates that the mode filtering will be performed for the whole image if equal to
     *                   true.
     */
    public AlgorithmMode(ModelImage destImg, ModelImage srcImg, int kSize, int kShape, boolean maskFlag) {

        super(destImg, srcImg);

        if (srcImg.isColorImage()) {
            displayError("AlgorithmMode does NOT work on color images");

            return;
        }

        if ((srcImg.getNDims() != 2) || (destImg.getNDims() != 2)) {
            displayError("AlgorithmMode 2D constructor called without a 2D image");

            return;
        }

        entireImage = maskFlag;
        kernelSize = kSize; // dimension of the kernel
        kernelShape = kShape; // set up the mask (kernel) used to filter
        sliceFiltering = true; // as a default--this doesn't make much sense in a 2D application.
        numberOfSlices = 1; // 2D images may only have 1 slice.
        makeKernel();

        // makeKernel sets halfK, which we now use
        srcBufferWidth = srcImage.getExtents()[0];
        srcBufferHeight = srcImage.getExtents()[1];
        srcBufferDepth = 1;

        // allow for a border of halfK around the source image
        bdrBufferWidth = srcBufferWidth + (2 * halfK);
        bdrBufferHeight = srcBufferHeight + (2 * halfK);
        bdrBufferDepth = 1;

        if (entireImage == false) {
            mask = srcImage.generateVOIMask();
        }
    }

    /**
     * Constructor for 3D images in which changes are returned to the source image.
     *
     * @param  srcImg        Source image model.
     * @param  kSize         Kernel size: dimension of the kernel (ie., 5 = 5x5, 7 = 7x7, 9 = 9x9, etc.).
     * @param  kShape        Kernel shape: element neighbors to include when finding the mode.
     * @param  sliceBySlice  Each slice in a volume image is to be filtered separately (when true), else the volume will
     *                       use a kernel with 3 dimensions.
     * @param  maskFlag      Flag that indicates that the mode filtering will be performed for the whole image if equal
     *                       to true.
     */
    public AlgorithmMode(ModelImage srcImg, int kSize, int kShape, boolean sliceBySlice, boolean maskFlag) {
        super(null, srcImg);

        if (srcImg.isColorImage()) {
            displayError("AlgorithmMode does NOT work on color images");

            return;
        }

        if (srcImg.getNDims() != 3) {
            displayError("AlgorithmMode 3D constructor called without a 3D image");

            return;
        }

        entireImage = maskFlag;
        kernelSize = kSize; // dimension of the kernel
        kernelShape = kShape; // set up the mask (kernel) used to filter
        sliceFiltering = sliceBySlice;
        numberOfSlices = srcImage.getExtents()[2];
        makeKernel();

        // makeKernel sets halfK, which we now use to set dimensions
        // of the source and border buffer
        srcBufferWidth = srcImage.getExtents()[0];
        srcBufferHeight = srcImage.getExtents()[1];
        srcBufferDepth = srcImage.getExtents()[2];

        bdrBufferWidth = srcBufferWidth + (2 * halfK);
        bdrBufferHeight = srcBufferHeight + (2 * halfK);

        if (sliceFiltering == true) {

            // do not add extra slices for slice by slice filtering
            bdrBufferDepth = srcBufferDepth;
        } else {

            // add extra slices for 3D filtering
            bdrBufferDepth = srcBufferDepth + (2 * halfK);
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
     * @param  kSize         Kernel size: dimension of the kernel (ie., 5 = 5x5, 7 = 7x7, 9 = 9x9, etc.).
     * @param  kShape        Kernel shape: element neighbors to include when finding the mode.
     * @param  sliceBySlice  Each slice in a volume image is filtered separately (when true), else the volume will use a
     *                       kernel with 3 dimensions.
     * @param  maskFlag      Flag that indicates that the mode filtering will be performed for the whole image if equal
     *                       to true.
     */
    public AlgorithmMode(ModelImage destImg, ModelImage srcImg, int kSize, int kShape, boolean sliceBySlice,
                         boolean maskFlag) {

        super(destImg, srcImg);

        if (srcImg.isColorImage()) {
            displayError("AlgorithmMode does NOT work on color images");

            return;
        }

        if (srcImg.getNDims() != 3) {
            displayError("AlgorithmMode 3D constructor called without a 3D image");

            return;
        }

        entireImage = maskFlag;
        kernelSize = kSize; // dimension of the kernel
        kernelShape = kShape; // set up the mask (kernel) used to filter
        sliceFiltering = sliceBySlice;
        numberOfSlices = srcImage.getExtents()[2];
        makeKernel();

        // makeKernel sets halfK, which we now use to set dimensions
        // of the source and border buffer
        srcBufferWidth = srcImage.getExtents()[0];
        srcBufferHeight = srcImage.getExtents()[1];
        srcBufferDepth = srcImage.getExtents()[2];

        bdrBufferWidth = srcBufferWidth + (2 * halfK);
        bdrBufferHeight = srcBufferHeight + (2 * halfK);

        if (sliceFiltering == true) {

            // do not add extra slices for slice by slice filtering
            bdrBufferDepth = srcBufferDepth;
        } else {

            // add extra slices for 3D filtering
            bdrBufferDepth = srcBufferDepth + (2 * halfK);
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
     * Mode filters the 2D source image. Replaces the original image with the filtered image.
     */
    private void calcInPlaceBorder2D() {

        int srcBufferLength = srcBufferWidth * srcBufferHeight;
        int[] srcBuffer;
        int[] resultBuffer;

        try {
            srcBuffer = new int[srcBufferLength];
            resultBuffer = new int[srcBufferLength];
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
        int[] bdrBuffer;

        try {
            bdrBuffer = new int[bdrBufferLength];
        } catch (OutOfMemoryError e) {
            srcBuffer = null;
            bdrBuffer = null;
            resultBuffer = null;
            errorCleanUp("Algorithm Mode reports: Out of memory when creating border buffer", true);

            return;
        }

        fireProgressStateChanged(srcImage.getImageName(), ""); // let user know what is going on
        

        // progressBar stuff here is a bit of overkill, unless we have a really
        // larger 2D image
        fireProgressStateChanged("Buffering ...");
        //fireProgressStateChanged(0);

        // copy image data from buffer into borderBuffer
        copy2DSrcBufferToBdrBuffer(srcBuffer, bdrBuffer, 0, 0);

        //fireProgressStateChanged(100);

        fireProgressStateChanged("Filtering...");
       // fireProgressStateChanged(0);

        this.sliceFilterBorder(bdrBuffer, resultBuffer, 0, 0); // filter this slice

        fireProgressStateChanged(100);

         // filtering work should be done.

        if (threadStopped) {
            finalize();

            return;
        }

        // place buffer data into the image
        try {
            srcImage.importData(0, resultBuffer, true);
        } catch (IOException error) {
            srcBuffer = null;
            bdrBuffer = null;
            resultBuffer = null;
            errorCleanUp("Algorithm Mode: Source image locked", true);

            return;
        }

        setCompleted(true);

    }

    /**
     * Mode filters the 3D source image and replaces it with the mode filtered image.
     */
    private void calcInPlaceBorder3D() {

        int srcBufferLength = srcBufferWidth * srcBufferHeight * srcBufferDepth;
        int[] srcBuffer;
        int[] resultBuffer;

        try {
            srcBuffer = new int[srcBufferLength];
            resultBuffer = new int[srcBufferLength];
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
        int[] bdrBuffer;

        try {
            bdrBuffer = new int[bdrBufferLength];
        } catch (OutOfMemoryError e) {
            srcBuffer = null;
            bdrBuffer = null;
            resultBuffer = null;
            errorCleanUp("Algorithm Mode: Out of memory", true);

            return;
        }

        this.fireProgressStateChanged(srcImage.getImageName(), "Buffering ...");
        

        // copy image data from srcBuffer into bdrBuffer
        copy3DSrcBufferToBdrBuffer(srcBuffer, bdrBuffer);

        int srcSliceLength = srcBufferWidth * srcBufferHeight;
        int bdrSliceLength = bdrBufferWidth * bdrBufferHeight;

        fireProgressStateChanged("Filtering...");

        if (sliceFiltering) {

            for (currentSlice = 0; (currentSlice < numberOfSlices) && !threadStopped; currentSlice++) {

                fireProgressStateChanged(Math.round((((float) (currentSlice) / (srcBufferDepth - 1)) * 100)));

                sliceFilterBorder(bdrBuffer, resultBuffer, currentSlice * bdrSliceLength,
                                  currentSlice * srcSliceLength);
            }
        } else { // volume kernel requested
            volumeFilterBorder(bdrBuffer, resultBuffer);
        }

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
     * This function produces a new image that has been mode filtered and places filtered image in the destination
     * image.
     */
    private void calcStoreInDestBorder2D() {

        int srcBufferLength = srcBufferWidth * srcBufferHeight;
        int[] srcBuffer;
        int[] resultBuffer;

        try {
            destImage.setLock(ModelStorageBase.RW_LOCKED);
        } catch (IOException error) {
            errorCleanUp("Algorithm Mode reports: destination image locked", false);

            return;
        }

        try {
            srcBuffer = new int[srcBufferLength];
            resultBuffer = new int[srcBufferLength];
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
        int[] bdrBuffer;

        try {
            bdrBuffer = new int[bdrBufferLength];
        } catch (OutOfMemoryError e) {
            srcBuffer = null;
            bdrBuffer = null;
            resultBuffer = null;
            errorCleanUp("Algorithm Mode reports: Out of memory when creating border buffer", true);

            return;
        }

        fireProgressStateChanged(srcImage.getImageName(), "Filtering image ..."); // let user know what is going on
        

        // progressBar stuff here is a bit of overkill, unless we have a really
        // larger 2D image
        fireProgressStateChanged("Buffering ...");
        fireProgressStateChanged(0);

        // copy image data from buffer into borderBuffer
        copy2DSrcBufferToBdrBuffer(srcBuffer, bdrBuffer, 0, 0);

        fireProgressStateChanged(100);

        fireProgressStateChanged("Filtering...");
        fireProgressStateChanged(0);

        this.sliceFilterBorder(bdrBuffer, resultBuffer, 0, 0); // filter this slice
        destImage.releaseLock(); // we didn't want to allow the image to be adjusted by someone else

        fireProgressStateChanged(100);

         // filtering work should be done.

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

        
        setCompleted(true);
    }

    /**
     * Mode filters the 3D source image and makes a new image for the mode filtered image.
     */
    private void calcStoreInDestBorder3D() {

        int srcBufferLength = srcBufferWidth * srcBufferHeight * srcBufferDepth;
        int[] srcBuffer;
        int[] resultBuffer;

        try {
            destImage.setLock(ModelStorageBase.RW_LOCKED);
        } catch (IOException error) {
            errorCleanUp("Algorithm Mode reports: destination image locked", false);

            return;
        }

        try {
            srcBuffer = new int[srcBufferLength];
            resultBuffer = new int[srcBufferLength];
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
        int[] bdrBuffer;

        try {
            bdrBuffer = new int[bdrBufferLength];
        } catch (OutOfMemoryError e) {
            srcBuffer = null;
            bdrBuffer = null;
            resultBuffer = null;
            errorCleanUp("Algorithm Mode: Out of memory", true);

            return;
        }

        this.fireProgressStateChanged(srcImage.getImageName(), "Buffering ...");
        

        // copy image data from srcBuffer into bdrBuffer
        copy3DSrcBufferToBdrBuffer(srcBuffer, bdrBuffer);

        int srcSliceLength = srcBufferWidth * srcBufferHeight;
        int bdrSliceLength = bdrBufferWidth * bdrBufferHeight;

        fireProgressStateChanged("Filtering...");

        if (sliceFiltering) {

            for (currentSlice = 0; (currentSlice < numberOfSlices) && !threadStopped; currentSlice++) {

                fireProgressStateChanged(Math.round((((float) (currentSlice) / (srcBufferDepth - 1)) * 100)));

                sliceFilterBorder(bdrBuffer, resultBuffer, currentSlice * bdrSliceLength,
                                  currentSlice * srcSliceLength);
            }
        } else { // volume kernel requested
            volumeFilterBorder(bdrBuffer, resultBuffer);
        }

        destImage.releaseLock(); // we didn't want to allow the image to be adjusted by someone else
         // filtering work should be done.

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

        
        setCompleted(true);
    }

    /**
     * Method copies the data in srcBuffer centered in the border buffer That is, the src image is completely surrounded
     * by a border. The values in the border locations are copies of nearby srcBuffer values.
     *
     * @param  srcBuffer             int[] the source image buffer
     * @param  bdrBuffer             int[] the border image buffer
     * @param  srcBufferOffsetIndex  offset into srcBuffer
     * @param  bdrBufferOffsetIndex  offset into bdrBuffer
     */
    private void copy2DSrcBufferToBdrBuffer(int[] srcBuffer, int[] bdrBuffer, int srcBufferOffsetIndex,
                                            int bdrBufferOffsetIndex) {

        // index into the border buffer
        int bdrBufferIdx = 0;

        // borderBuffer includes the kernel radius worth of rows at the bottom
        // and top of the image, plus the kernel radius worth of columns
        // on the current row
        // brdBufferKernelOffset is the offset (index) into the borderBuffer of
        // where the first pixel in the sourceBuffer should be copied
        int brdBufferKernelOffset = (halfK * bdrBufferWidth) + halfK;

        int brdBufferOffset = brdBufferKernelOffset + bdrBufferOffsetIndex;

        int srcBufferIdx = srcBufferOffsetIndex;
        int srcRow, srcCol;

        // assume for now that buffering for 2D slices is fast
        // so we will not update the progressBar

        // copy image data into the borderBuffer
        for (srcRow = 0; srcRow < srcBufferHeight; ++srcRow) {
            bdrBufferIdx = (srcRow * bdrBufferWidth) + brdBufferOffset;

            for (srcCol = 0; srcCol < srcBufferWidth; ++srcCol, srcBufferIdx++, bdrBufferIdx++) {
                bdrBuffer[bdrBufferIdx] = srcBuffer[srcBufferIdx];
            }
        }

        int row, col;

        // copy image data to the borders
        // first source image row to lower bdrBuffer rows
        for (row = 0; row < halfK; ++row) {
            srcBufferIdx = srcBufferOffsetIndex; // 6/28
            bdrBufferIdx = (row * bdrBufferWidth) + halfK + bdrBufferOffsetIndex;

            for (col = 0; col < srcBufferWidth; ++col) {
                bdrBuffer[bdrBufferIdx++] = srcBuffer[srcBufferIdx++];
            }

            // copy data to columns in bdrBuffer that are NOT in srcBuffer
            bdrBufferIdx = (row * bdrBufferWidth) + bdrBufferOffsetIndex;

            for (col = 0; col < halfK; ++col) {
                bdrBuffer[bdrBufferIdx++] = srcBuffer[srcBufferOffsetIndex]; // 6/28
                bdrBuffer[(row * bdrBufferWidth) + bdrBufferWidth - 1 - col + bdrBufferOffsetIndex] = srcBuffer[srcBufferWidth -
                                                                                                                1]; // 6/28
            }
        }

        // last source image row to upper bdrBuffer rows
        int srcBufferFirstColIdx = (srcBufferWidth * (srcBufferHeight - 1)) + srcBufferOffsetIndex;
        int srcBufferLastColIdx = (srcBufferWidth * srcBufferHeight) - 1 + srcBufferOffsetIndex;

        for (row = srcBufferHeight + halfK; row < bdrBufferHeight; ++row) {
            srcBufferIdx = srcBufferFirstColIdx;
            bdrBufferIdx = (row * bdrBufferWidth) + halfK + bdrBufferOffsetIndex;

            for (col = 0; col < srcBufferWidth; ++col) {
                bdrBuffer[bdrBufferIdx++] = srcBuffer[srcBufferIdx++];
            }

            // copy data to columns in bdrBuffer that are NOT in srcBuffer
            bdrBufferIdx = (row * bdrBufferWidth) + bdrBufferOffsetIndex;

            for (col = 0; col < halfK; ++col) {
                bdrBuffer[bdrBufferIdx] = srcBuffer[srcBufferFirstColIdx];
                bdrBuffer[(row * bdrBufferWidth) + bdrBufferWidth - 1 - col + bdrBufferOffsetIndex] = srcBuffer[srcBufferLastColIdx]; // 6/28
                bdrBufferIdx++;
            }
        }

        // remaining image columns
        for (srcRow = 0; srcRow < srcBufferHeight; ++srcRow) {
            bdrBufferIdx = ((srcRow + halfK) * bdrBufferWidth) + bdrBufferOffsetIndex; // 6/28
            srcBufferIdx = (srcRow * srcBufferWidth) + srcBufferOffsetIndex; // 6/28

            for (srcCol = 0; srcCol < halfK; ++srcCol) {
                bdrBuffer[bdrBufferIdx++] = srcBuffer[srcBufferIdx];
            }

            srcBufferIdx = (srcRow * srcBufferWidth) + srcBufferWidth - 1 + srcBufferOffsetIndex;
            bdrBufferIdx = ((srcRow + halfK) * bdrBufferWidth) + halfK + srcBufferWidth + bdrBufferOffsetIndex;

            for (srcCol = 0; srcCol < halfK; ++srcCol) {
                bdrBuffer[bdrBufferIdx++] = srcBuffer[srcBufferIdx];
            }
        }

    }

    /**
     * Method copies the data in srcBuffer centered in the border buffer That is, the src image is completely surrounded
     * by a border. The values in the border locations are copies of nearby srcBuffer values.
     *
     * @param  srcBuffer  int[] the source image buffer
     * @param  bdrBuffer  int[] the border image buffer
     */
    private void copy3DSrcBufferToBdrBuffer(int[] srcBuffer, int[] bdrBuffer) {

        int srcBufferSliceLength = srcBufferWidth * srcBufferHeight;
        int bdrBufferSliceLength = bdrBufferWidth * bdrBufferHeight;

        // offset allows for having extra kernel slices for volumeFilteringBorder
        int bdrBufferSliceOffset;

        if (sliceFiltering == true) {

            // slice filtering does not have extra border slice
            bdrBufferSliceOffset = 0;
        } else {
            bdrBufferSliceOffset = halfK * bdrBufferSliceLength;
        }

        int sliceIndex;

        for (sliceIndex = 0; sliceIndex < srcBufferDepth; sliceIndex++) {

            fireProgressStateChanged(Math.round((((float) (sliceIndex) / (srcBufferDepth - 1)) * 100)));

            copy2DSrcBufferToBdrBuffer(srcBuffer, bdrBuffer, sliceIndex * srcBufferSliceLength,
                                       (sliceIndex * bdrBufferSliceLength) + bdrBufferSliceOffset);
        }

        if (sliceFiltering == false) {

            // assume slice replication for the kernel slices is quick, do
            // don't mess with the progressBar in this part

            // copy data into the extra border slices
            int copyFromIndex, copyToIndex, dataIndex;

            // for the lower border slice(s)
            for (sliceIndex = 0; sliceIndex < halfK; sliceIndex++) {
                copyFromIndex = bdrBufferSliceOffset;
                copyToIndex = sliceIndex * bdrBufferSliceLength;

                for (dataIndex = 0; dataIndex < bdrBufferSliceLength; dataIndex++) {
                    bdrBuffer[copyToIndex++] = bdrBuffer[copyFromIndex++];
                }
            }

            for (sliceIndex = bdrBufferDepth - halfK; sliceIndex < bdrBufferDepth; sliceIndex++) {
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
     *
     * @param   i     The central pixel to find neighbors for.
     * @param   data  Image data
     * @param   is2D  True indicates that the neighbors are found along a 2D slice (or 2D image) instead of neighbors in
     *                a 3D volume.
     *
     * @return  The neighboring pixel list corresponds to the kernel chosen.
     */
    private int[] getBorderBufferNeighborList(int i, int[] data, boolean is2D) {
        int row, col; // index variables
        int kCenter = kernelCenter; // index to the central element of the kernel

        // (this is the mask for which elements in data are used.)

        int width = bdrBufferWidth; // number of columns in the border buffer
        int height = bdrBufferHeight; // number of rows in the border buffer

        // place all the masked 'on' elements into the data-list
        int count = 0;

        if (is2D) {

            for (row = -halfK; row <= halfK; row++) { // go through all rows

                for (col = -halfK; col <= halfK; col++) { // go through all columns

                    if (kernel[kCenter + col + (row * kernelSize)] != 0) { // but don't bother copying into the list if

                        // we don't want that element (the kernel's pixl is zero)
                        kernelMask[count++] = data[i + col + (row * width)];
                    }
                }
            }
        } else { // find neighbors in a volume

            int slice;

            for (slice = -halfK; slice <= halfK; slice++) {

                for (row = -halfK; row <= halfK; row++) {

                    for (col = -halfK; col <= halfK; col++) {

                        if (kernel[kCenter + col + (row * kernelSize) + (slice * kernelSize * kernelSize)] != 0) {
                            kernelMask[count++] = data[i + col + (row * width) + (slice * width * height)];
                        }
                    }
                }
            }
        }

        return (kernelMask);
    }

    /**
     * Forms kernel.
     */
    private void makeKernel() {

        try {

            if (sliceFiltering) {
                kernel = new byte[kernelSize * kernelSize];
            } else if (!sliceFiltering) {
                kernel = new byte[kernelSize * kernelSize * kernelSize];
            }
        } catch (OutOfMemoryError e) {
            displayError("Algorithm Mode reports: not enough memory to form a kernel mask.");
            setCompleted(false);
            setThreadStopped(true);

            return;
        }

        setKernel();
        makeKernelMask();
    }

    /**
     * Makes the kernel mask. The kernel mask is the list of values pulled from the image which will be used to find the
     * mode of the central pixel. Its length is <i>(number of pixels to be used to determine mode)</i>.
     *
     * <p>Thus the kernel center (decided here), has the value of the location of the central pixel shown in the window.
     * The value of the kernel center is the number of pixels picked up to mode sort.</p>
     *
     * <p>Note that for symmetric masks always have kernelCenter = count/2 and maskCenter = count/2.</p>
     */
    private void makeKernelMask() {
        halfK = kernelSize / 2;

        // figure how many kernel elements are actually in the kernel-mask
        int count = 0; // start counting from one, since sort starts with element 1 (even empty mask must have 1
                       // element!)

        for (int m = 0; m < kernel.length; m++) {

            if (kernel[m] != 0) { // if this element is marked 'on'
                count++;
            }
        }

        kernelMask = new int[count]; // must have the leading element empty: the sort starts with element 1

        if (sliceFiltering) { // 2D

            if (kernelShape == SQUARE_KERNEL) {
                kernelCenter = count / 2; // whole square
            } else if (kernelShape == CROSS_KERNEL) {
                kernelCenter = halfK * (kernelSize + 1);
            } else if ((kernelShape == VERT_KERNEL) || (kernelShape == HORZ_KERNEL)) {
                kernelCenter = halfK * (kernelSize + 1);
            } else if (kernelShape == X_KERNEL) { // sizeof kernel is same as CROSS_KERNEL
                kernelCenter = halfK * (kernelSize + 1);
            }
        } else { // 3D

            if (kernelShape == CUBE_KERNEL) {
                kernelCenter = count / 2;
            } else if (kernelShape == AXIAL_KERNEL) {
                kernelCenter = (kernelSize * kernelSize * kernelSize) / 2; // whole cube
            } else if (kernelShape == X_KERNEL) { // sizeof kernel is same as AXIAL_KERNEL
                kernelCenter = (kernelSize * kernelSize * kernelSize) / 2; // whole cube
            }
        }
        // not entirely dumb.  mc = count/2 because of the symmetry of the mask.  custom masks may be diff.
        // & i'd like to include custom masks someday....
    }

    /**
     * Finds the mode value of the list. If the numbers in the list are unique, return the value of the second
     * parameter. This method assumes the parameter list is sorted, and counts consecutive array elements containing the
     * same value.
     *
     * @param   list  int[] List of numbers
     * @param   val   Value to return if all list elements are unique
     *
     * @return  the mode of the values in the list
     */
    private int mode(int[] list, int val) {
        int count = 1;
        int maxCount = 1;
        int rtnVal = val;

        for (int i = 0; i < (list.length - 1); ++i) {

            if (list[i] == list[i + 1]) {
                ++count;

                if (count > maxCount) {
                    maxCount = count;
                    rtnVal = list[i];
                }
            } else {
                count = 1;
            }
        }

        // if maxCount == 1 we never changed the value of rtnVal,
        // so this step is unnecessary.  Let's keep it to make
        // things clearer.
        if (maxCount == 1) {
            rtnVal = val;
        }

        return (rtnVal);
    }

    /**
     * Fill in the mask for which pixels are used in filtering.
     */
    private void setKernel() {
        int i;
        int halfK = kernelSize / 2;

        // square/box
        if ((kernelShape == SQUARE_KERNEL) || (kernelShape == CUBE_KERNEL)) {

            for (i = 0; i < kernel.length; i++) {
                kernel[i] = 1;
            }
        } // end square/cube kernel

        // cross/axial
        else if ((kernelShape == CROSS_KERNEL) || (kernelShape == AXIAL_KERNEL)) {
            int row; // indicates current row
            int col; // indicates current column

            if (sliceFiltering) {

                for (i = 0; i < kernel.length; i++) {
                    row = i / kernelSize;
                    col = i % kernelSize;

                    if (col == halfK) {
                        kernel[i] = 1;
                    } else if (row == halfK) {
                        kernel[i] = 1;
                    } else {
                        kernel[i] = 0;
                    }
                }
            } else { // volume filtering

                int slice;

                for (i = 0; i < kernel.length; i++) {
                    slice = i / (kernelSize * kernelSize);
                    row = (i % (kernelSize * kernelSize)) / kernelSize;
                    col = i % kernelSize;

                    if (slice == halfK) {

                        if (col == halfK) {
                            kernel[i] = 1;
                        } else if (row == halfK) {
                            kernel[i] = 1;
                        } else {
                            kernel[i] = 0;
                        }
                    } else if ((row == halfK) && (col == halfK)) {
                        kernel[i] = 1;
                    } else {
                        kernel[i] = 0;
                    }
                }
            }
        } // end cross/axial
        else if (kernelShape == VERT_KERNEL) {
            int col; // indicates current column

            if (sliceFiltering) {

                for (i = 0; i < kernel.length; i++) {
                    col = i % kernelSize;

                    if (col == halfK) {
                        kernel[i] = 1;
                    } else {
                        kernel[i] = 0;
                    }
                }
            } else { // volume filtering
            }
        } // end vert
        else if (kernelShape == HORZ_KERNEL) {
            int row; // indicates current row

            if (sliceFiltering) {

                for (i = 0; i < kernel.length; i++) {
                    row = i / kernelSize;

                    if (row == halfK) {
                        kernel[i] = 1;
                    } else {
                        kernel[i] = 0;
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

                for (i = 0; i < kernel.length; i++) {
                    row = i / kernelSize;
                    col = i % kernelSize;
                    revcol = kernelSize - 1 - col;

                    if (row == col) {
                        kernel[i] = 1;
                    } else if (row == revcol) {
                        kernel[i] = 1;
                    } else {
                        kernel[i] = 0;
                    }
                }
            } else { // volume filtering

                int slice;

                for (i = 0; i < kernel.length; i++) {
                    slice = i / (kernelSize * kernelSize);
                    row = (i % (kernelSize * kernelSize)) / kernelSize;
                    col = i % kernelSize;
                    revcol = kernelSize - 1 - col;

                    if ((slice == col) || (slice == revcol)) {

                        if (row == col) {
                            kernel[i] = 1;
                        } else if (row == revcol) {
                            kernel[i] = 1;
                        } else {
                            kernel[i] = 0;
                        }
                    } else {
                        kernel[i] = 0;
                    }
                }
            }
        } // end 'x' kernel

    }

  

    /**
     * Performs mode filtering on a single slice with a 2D or 3D buffer.
     *
     * @param  srcBdrBuffer             Source buffer.
     * @param  destBuffer               Destination Buffer.
     * @param  srcBufferStartingPoint   Index of the first pixel in the source buffer
     * @param  destBufferStartingPoint  Index of the first pixel for the slice to be filtered in the destination buffer.
     */
    private void sliceFilterBorder(int[] srcBdrBuffer, int[] destBuffer, int srcBufferStartingPoint,
                                   int destBufferStartingPoint) {

        // space allocated for extra rows used by the kernel
        int srcBrdBufferKernelOffset = (halfK * bdrBufferWidth) + halfK;

        // space for previous slices when slice filtering a 3D volume
        int srcBdrBufferOffset = srcBufferStartingPoint + srcBrdBufferKernelOffset;

        int srcBdrBufferIdx;
        int destRow, destCol, destBufferIdx = destBufferStartingPoint;
        int[] maskedList;

        for (destRow = 0; destRow < srcBufferHeight; destRow++) {
            srcBdrBufferIdx = (destRow * bdrBufferWidth) + srcBdrBufferOffset;

            for (destCol = 0; destCol < srcBufferWidth; destCol++) {

                maskedList = getBorderBufferNeighborList(srcBdrBufferIdx, srcBdrBuffer, true);
                Arrays.sort(maskedList);
                destBuffer[destBufferIdx++] = mode(maskedList, srcBdrBuffer[srcBdrBufferIdx++]);

            }
        }
    } // end sliceFilterBorder(...)

    /**
     * Assumes a monochrome image of type BYTE, SHORT, INTEGER, UBYTE, USHORT, UINTEGER.
     *
     * @param  srcBdrBuffer  int[] the border buffer containing the source image data
     * @param  destBuffer    int[] the destination buffer
     */
    private void volumeFilterBorder(int[] srcBdrBuffer, int[] destBuffer) {

        // space allocated for extra slices, rows, and cols used by the kernel
        int srcBrdBufferKernelOffset = (halfK * bdrBufferWidth * bdrBufferHeight) + (halfK * bdrBufferWidth) + halfK;

        int[] maskedList;

        int srcBdrBufferSliceLength = bdrBufferWidth * bdrBufferHeight;
        int srcBdrBufferIdx, srcBdrBufferSliceOffset;
        int destSlice, destRow, destCol, destBufferIdx = 0;

        for (destSlice = 0; (destSlice < srcBufferDepth) && !threadStopped; destSlice++) {

            fireProgressStateChanged(Math.round((((float) (destSlice) / (srcBufferDepth - 1)) * 100)));

            srcBdrBufferSliceOffset = (destSlice * srcBdrBufferSliceLength) + srcBrdBufferKernelOffset;

            for (destRow = 0; destRow < srcBufferHeight; destRow++) {

                srcBdrBufferIdx = (destRow * bdrBufferWidth) + srcBdrBufferSliceOffset;

                for (destCol = 0; destCol < srcBufferWidth; destCol++) {

                    maskedList = getBorderBufferNeighborList(srcBdrBufferIdx, srcBdrBuffer, false);
                    Arrays.sort(maskedList);
                    destBuffer[destBufferIdx++] = mode(maskedList, srcBdrBuffer[srcBdrBufferIdx++]);

                }
            }
        }
    }

} // end class AlgorithmMode
