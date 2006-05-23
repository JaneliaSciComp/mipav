package gov.nih.mipav.model.algorithms.filters;


import gov.nih.mipav.model.algorithms.*;
import gov.nih.mipav.model.structures.*;

import gov.nih.mipav.view.*;

import java.awt.*;

import java.io.*;

import java.util.*;


/**
 * Algorithm to apply a mean filter to an image, placing it in a new ModelImage if New image is selected or returning
 * the changed picture to the same image if Replace image is selected. Define a region including the target pixel and
 * neigbors. Then the mean is given by the sum of all the pixels in the region divided by the total number of pixels in
 * the region. The target regions are either odd lengthed squares for slice averaging or odd lengthed cubes for volume
 * multislice averaging. For 2D imaging only slice averaging can be used since there is only 1 slice. For 3D averaging
 * slice averaging is the default, but slice averaging can be selected with the Apply slice kernel button and volume
 * averaging can be selected with the Apply volume kernel button. If slice averaging is selected, a 3 x 3 kernel size is
 * the default, but a 3 x 3, 5 x 5, 7 x 7, 9 x 9, or 11 x 11 size may be selected. If volume averaging is selected, a 3
 * x 3 x 3 kernel size is the default, but a 3 x 3 x 3, 5 x 5 x 5, 7 x 7 x 7, 9 x 9 x 9, or 11 x 11 x 11 kernel size may
 * be selected. With slice averaging the time requirement will grow as the square of the kernel length and with volume
 * averaging the time requirement will grow as the cube of the kernel length.
 *
 * <p>Whole image application of the mean is the default, but the user can select either whole image or voi region(s).
 * If voi region(s) is selected, then the mean value of the pixel will replace the original value of the pixel inside
 * the voi regions(s) and the pixel values outside the voi regions(s) will remain unchanged.</p>
 *
 * <p>For color images the default is to obtain the means of the red, green, and blue channels. However, if Red Channel,
 * Green Channel, or Blue Channel is unchecked, then that color channel will pass thru with no mean averaging performed.
 * </p>
 *
 * <p>Note the the term average does not always represent mean. If fact, there are three types of average, the commonly
 * used mean and median and the infrequently used mode. The mean is defined as the sum of the values in a group divided
 * by the number of values in a group. The median value is defined as the value which has the same number of values
 * greater than it and less than it. The mode is defined as the value with the greatest number of occurrences.</p>
 *
 * <p>If part of the neighborhood kernel falls outside the image when the pixel is on or near the boundary, simply only
 * use that portion of the kernel inside the image. Consider, for example, the upper right corner pixel in a 2D image
 * using a 3 x 3 kernel. In this case only the target pixel and its lower, left, and diagonal left low neighbors will be
 * used. The sum of these 4 pixels will be divided by 4.</p>
 * 
 * <p> If the image is color and separateChannels is false, then the image is decomposed into 
 * components parallel and perpindicular to the direction (redVector, greenVector, blueVector).
 * Only the parallel component is filtered.  After filtering the parallel component,
 * the parallel and perpindicular components are added back together.
 * The filtering takes place in parallel and perpindicular directions
 * in hue-saturation space.  This algorithm is from "Linear Colour-Dependent
 * Image Filtering based on Vector Decomposition" by Stephen J. Sangwine and
 * Barnabas N. Gatsheni.   In a later paper "Colour-Dependent Linear Vector Image
 * Filtering" by Stephen J. Sangwine, Todd A. Ell, and Barbabas N. Gatsheni, 3
 * major limitations of this scheme are discussed: 1.) Limited color selectivity
 * Filter as the cosine of the angle between any hue and the reference hue.
 * 2.) Luminance filtering, even in pixels not close to the chosen color direction.
 * and 3.) The opponent color to the color of interest is not separable from the
 * color of interest using a linear operation, although a nonlinear extraction using
 * the sign bit would be easy enough to perform. </p>
 */


public class AlgorithmMean extends AlgorithmBase {

    //~ Instance fields ------------------------------------------------------------------------------------------------

    /** DOCUMENT ME! */
    private boolean bChannel = true; // the blue channel

    /** DOCUMENT ME! */
    private int currentSlice = 0; //

    /** DOCUMENT ME! */
    private boolean entireImage; // true means apply to entire image, false only region

    /** DOCUMENT ME! */
    private boolean gChannel = true; // the green channel

    /** DOCUMENT ME! */
    private int halfK;


    /** DOCUMENT ME! */
    private boolean isColorImage = false; // indicates the image is a color image

    /** DOCUMENT ME! */
    private int kernelSize; // dimension of the kernel (ie., 5 = 5x5, 7 = 7x7, 9 = 9x9, etc.)


    /** DOCUMENT ME! */
    private BitSet mask = null;

    /** DOCUMENT ME! */
    private int numberOfSlices;

    /** DOCUMENT ME! */
    private boolean rChannel = true; // if T, filter the red channel

    /** DOCUMENT ME! */
    private boolean sliceFiltering; // do all filtering slice-by-slice, rather than as a volume
                                    // (applies only to 3D/volume images)

    /** DOCUMENT ME! */
    private int valuesPerPixel = 1; // number of elements in a pixel.  Monochrome = 1, Color = 4. (a, R, G, B)
    // For color images if separateChannels is true, take means of red, green, and blue components
    // separately.  If separateChannels is false, decompose image into components parallel
    // and perpindicular to the direction given by (redVector, greenVector, and blueVector)
    // and only filter the parallel component before adding the 2 components back together.
    private boolean separateChannels = true;
    private float redNorm = 0.0f;
    private float greenNorm = 0.0f;
    private float blueNorm  = 0.0f;
    private float midGray = 127.5f;
    private float colorMax = 255.0f;

    //~ Constructors ---------------------------------------------------------------------------------------------------

    /**
     * Constructor for 2D images in which changes are returned to the source image.
     *
     * @param  srcImg    Source image model.
     * @param  kSize     Kernel size: dimension of the kernel (ie., 5 = 5x5, 7 = 7x7, 9 = 9x9, etc.).
     * @param  maskFlag  Flag that indicates that the mean filtering will be performed for the whole image if equal to
     *                   true.
     */
    public AlgorithmMean(ModelImage srcImg, int kSize, boolean maskFlag) {
        super(null, srcImg);

        if (srcImg.isColorImage()) {
            isColorImage = true;
            valuesPerPixel = 4;
        }

        // else, already false
        entireImage = maskFlag;
        kernelSize = kSize; // dimension of the kernel
        halfK = (kernelSize - 1) / 2;
        sliceFiltering = true; // as a default--though a different value doesn't make much sense in a 2D application.
        numberOfSlices = 1; // 2D images may only have 1 slice.

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
     * @param  maskFlag  Flag that indicates that the mean filtering will be performed for the whole image if equal to
     *                   true.
     */
    public AlgorithmMean(ModelImage destImg, ModelImage srcImg, int kSize, boolean maskFlag) {

        super(destImg, srcImg);

        if (srcImg.isColorImage()) {
            isColorImage = true;
            valuesPerPixel = 4;
        }

        // else, already false
        entireImage = maskFlag;
        kernelSize = kSize; // dimension of the kernel
        halfK = (kernelSize - 1) / 2;
        sliceFiltering = true; // as a default--this doesn't make much sense in a 2D application.
        numberOfSlices = 1; // 2D images may only have 1 slice.

        if (entireImage == false) {
            mask = srcImage.generateVOIMask();
        }
    }

    /**
     * Constructor for 3D images in which changes are returned to the source image.
     *
     * @param  srcImg        Source image model.
     * @param  kSize         Kernel size: dimension of the kernel (ie., 5 = 5x5, 7 = 7x7, 9 = 9x9, etc.).
     * @param  sliceBySlice  Each slice in a volume image is to be filtered separately (when true), else the volume will
     *                       use a kernel with 3 dimensions.
     * @param  maskFlag      Flag that indicates that the mean filtering will be performed for the whole image if equal
     *                       to true.
     */
    public AlgorithmMean(ModelImage srcImg, int kSize, boolean sliceBySlice, boolean maskFlag) {
        super(null, srcImg);

        if (srcImg.isColorImage()) {
            isColorImage = true;
            valuesPerPixel = 4;
        }

        // else, already false
        entireImage = maskFlag;
        kernelSize = kSize; // dimension of the kernel
        halfK = (kernelSize - 1) / 2;
        sliceFiltering = sliceBySlice;
        numberOfSlices = srcImage.getExtents()[2];

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
     * @param  sliceBySlice  Each slice in a volume image is filtered separately (when true), else the volume will use a
     *                       kernel with 3 dimensions.
     * @param  maskFlag      Flag that indicates that the mean filtering will be performed for the whole image if equal
     *                       to true.
     */
    public AlgorithmMean(ModelImage destImg, ModelImage srcImg, int kSize, boolean sliceBySlice, boolean maskFlag) {

        super(destImg, srcImg);

        if (srcImg.isColorImage()) {
            isColorImage = true;
            valuesPerPixel = 4;
        }

        // else, already false
        entireImage = maskFlag;
        kernelSize = kSize; // dimension of the kernel
        halfK = (kernelSize - 1) / 2;
        sliceFiltering = sliceBySlice;
        numberOfSlices = srcImage.getExtents()[2];

        if (entireImage == false) {
            mask = srcImage.generateVOIMask();
        }
    }

    //~ Methods --------------------------------------------------------------------------------------------------------

    /**
     * Prepares this class for destruction.
     */
    public void finalize() {
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

        constructLog();

        if (destImage != null) { // if there exists a destination image

            if (srcImage.getNDims() == 2) {
                calcStoreInDest2D();
            } else if (srcImage.getNDims() > 2) {
                calcStoreInDest3D();
            }
        } else { // there is no image but the original source.

            if (srcImage.getNDims() == 2) {
                calcInPlace2D();
            } else if (srcImage.getNDims() > 2) {
                calcInPlace3D();
            }
        }
    }

    /**
     * 
     * If separateChannels is true, RGB images are mean filtered by 'channel.' 
     * That is, each color, red, blue and green, is filtered independently of
     * the other two colors. This mean filter permits selectively filtering any combination of the three channels
     * instead of simply filtering all three.
     * If separateChannels is false, decompose image into components parallel
     * and perpindicular to the direction given by (redVector, greenVector, and blueVector)
     * and only filter the parallel component before adding the 2 components back together.
     * @param  separateChannels
     * @param  r  Filter red channel.
     * @param  g  Filter green channel.
     * @param  b  Filter blue channel.
     * @param  redVector
     * @param  greenVector
     * @param  blueVector
     */
    public void setRGBChannelFilter(boolean separateChannels, 
                  boolean r, boolean g, boolean b,
                  int redVector, int greenVector, int blueVector) {
        double offsetR, offsetG, offsetB, denom;

        if (isColorImage) { // just in case somebody called for a mono image
            this.separateChannels = separateChannels;
            rChannel = r;
            gChannel = g;
            bChannel = b;
            if (srcImage.getType() == ModelStorageBase.ARGB_USHORT) {
                midGray = 32767.5f;
                colorMax = 65535.0f;
            }
            offsetR = redVector - midGray;
            offsetG = greenVector - midGray;
            offsetB = blueVector - midGray;
            denom = Math.sqrt(offsetR*offsetR + offsetG*offsetG + offsetB*offsetB);
            redNorm = (float)(offsetR/denom);
            greenNorm = (float)(offsetG/denom);
            blueNorm = (float)(offsetB/denom);
        }
    }


    /**
     * Creates the standard progressBar. Stores in the class-global, progressBar.
     */
    private void buildProgressBar() {

        try {

            if (pBarVisible == true) {
                progressBar = new ViewJProgressBar(srcImage.getImageName(), "Filtering image ...", 0, 100, true, this,
                                                   this);

                int xScreen = Toolkit.getDefaultToolkit().getScreenSize().width;
                int yScreen = Toolkit.getDefaultToolkit().getScreenSize().height;
                progressBar.setLocation(xScreen / 2, yScreen / 2);
                progressBar.setVisible(true);
            }
        } catch (NullPointerException npe) {

            if (Preferences.debugLevel(Preferences.DEBUG_ALGORITHM)) {
                Preferences.debug("AlgrithmMean: NullPointerException found while building progress bar.");
            }
        }
    }

    /**
     * Mean filters the source image. Replaces the original image with the filtered image.
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
            errorCleanUp("Algorithm Mean: source image locked", true);

            return;
        } catch (OutOfMemoryError e) {
            buffer = null;
            resultBuffer = null;
            errorCleanUp("Algorithm Mean reports: Out of memory when creating image buffer", true);

            return;
        }

        this.buildProgressBar(); // let user know what is going on
        if (isColorImage && !separateChannels) {
            this.sliceVectorFilter(buffer, resultBuffer, 0, "image"); // filter this slice    
        }
        else {
            this.sliceFilter(buffer, resultBuffer, 0, "image"); // filter this slice
        }
        disposeProgressBar(); // filtering work should be done.

        if (threadStopped) {
            finalize();

            return;
        }

        try { // place buffer data into the image
            srcImage.importData(0, resultBuffer, true);
        } catch (IOException error) {
            buffer = null;
            resultBuffer = null;
            errorCleanUp("Algorithm Mean: Source image locked", true);

            return;
        }

        setCompleted(true);
    }

    /**
     * Mean filters the source image and replaces the source image with the mean filtered image.
     */
    private void calcInPlace3D() {

        int imageSliceLength = valuesPerPixel * srcImage.getExtents()[0] * srcImage.getExtents()[1];
        int length;
        float[] buffer;
        float[] resultBuffer;

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
            resultBuffer = new float[length];
            srcImage.exportData(0, length, buffer); // locks and releases lock
            this.buildProgressBar();
        } catch (IOException error) {
            buffer = null;
            resultBuffer = null;
            errorCleanUp("Algorithm Mean: Source image locked", true);

            return;
        } catch (OutOfMemoryError e) {
            buffer = null;
            resultBuffer = null;
            errorCleanUp("Algorithm Mean: Out of memory", true);

            return;
        }

        if (sliceFiltering) {

            for (currentSlice = 0; (currentSlice < numberOfSlices) && !threadStopped; currentSlice++) {
                if (isColorImage && !separateChannels) {
                    sliceVectorFilter(buffer, resultBuffer, currentSlice * imageSliceLength,
                            "slice " + String.valueOf(currentSlice + 1));    
                }
                else {
                    sliceFilter(buffer, resultBuffer, currentSlice * imageSliceLength,
                            "slice " + String.valueOf(currentSlice + 1));
                }
            }
        } else { // volume kernel requested

            if (isColorImage && !separateChannels) {
                volumeVectorFilter(buffer, resultBuffer);
            }
            else if (isColorImage) { // for color image
                volumeColorFilter(buffer, resultBuffer);
            } else { // for mono image
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
            errorCleanUp("Algorithm Mean: source image locked", true);
            setThreadStopped(true);

            return;
        }

        progressBar.dispose();
        setCompleted(true);
    }

    /**
     * This function produces a new image that has been mean filtered and places filtered image in the destination
     * image.
     */
    private void calcStoreInDest2D() {

        int length; // total number of data-elements (pixels) in image
        float[] buffer; // data-buffer (for pixel data) which is the "heart" of the image
        float[] resultBuffer; // copy-to buffer (for pixel data) for image-data after filtering

        try {
            destImage.setLock(ModelStorageBase.RW_LOCKED);
        } catch (IOException error) {
            errorCleanUp("Algorithm Mean reports: destination image locked", false);

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
            errorCleanUp("Algorithm Mean reports: source image locked", true);

            return;
        } catch (OutOfMemoryError e) {
            buffer = null;
            resultBuffer = null;
            errorCleanUp("Algorithm Mean reports: out of memory", true);

            return;
        }

        this.buildProgressBar();
        if (isColorImage && !separateChannels) {
            sliceVectorFilter(buffer, resultBuffer, 0, "image");    
        }
        else {
            sliceFilter(buffer, resultBuffer, 0, "image"); // filter image based on provided info.
        }
        destImage.releaseLock(); // we didn't want to allow the image to be adjusted by someone else
        progressBar.dispose();

        if (threadStopped) {
            finalize();

            return;
        }

        try { // but now place buffer data into the image
            destImage.importData(0, resultBuffer, true);
        } catch (IOException error) {
            buffer = null;
            resultBuffer = null;
            errorCleanUp("Algorithm Mean reports: destination image still locked", true);

            return;
        }

        setCompleted(true);
    }

    /**
     * This function produces a new volume image that has been mean filtered. Image can be filtered by filtering each
     * slice individually, or by filtering using a kernel-volume.
     */
    private void calcStoreInDest3D() {

        int length;
        int imageSliceLength = valuesPerPixel * srcImage.getExtents()[0] * srcImage.getExtents()[1]; // cover case of color image
        float[] buffer;
        float[] resultBuffer;

        try {
            destImage.setLock(ModelStorageBase.RW_LOCKED);
        } catch (IOException error) {
            errorCleanUp("Algorithm Mean reports: destination image locked", false);

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
            this.buildProgressBar();
        } catch (IOException error) {
            buffer = null;
            resultBuffer = null;
            errorCleanUp("Algorithm Mean: source image locked", true);

            return;
        } catch (OutOfMemoryError e) {
            buffer = null;
            resultBuffer = null;
            errorCleanUp("Algorithm Mean: Out of memory creating process buffer", true);

            return;
        }

        try {
            resultBuffer = new float[length];
        } catch (OutOfMemoryError e) {
            buffer = null;
            resultBuffer = null;
            errorCleanUp("Algorithm Mean reports: Out of memory because of resultBuffer", true);

            return;
        }

        if (sliceFiltering) {

            for (currentSlice = 0; (currentSlice < numberOfSlices) && !threadStopped; currentSlice++) {
                if (isColorImage && !separateChannels) {
                    sliceVectorFilter(buffer, resultBuffer, currentSlice * imageSliceLength,
                            "slice " + String.valueOf(currentSlice + 1));    
                }
                else {
                    sliceFilter(buffer, resultBuffer, currentSlice * imageSliceLength,
                            "slice " + String.valueOf(currentSlice + 1));
                }
            }
        } else { // requested volume filter

            if (isColorImage && !separateChannels) {
                volumeVectorFilter(buffer, resultBuffer);
            }
            else if (isColorImage) { // for color image
                volumeColorFilter(buffer, resultBuffer);
            } else { // for mono image
                volumeFilter(buffer, resultBuffer);
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
            errorCleanUp("Algorithm Mean reports: destination image still locked", true);

            return;
        }

        progressBar.dispose();
        setCompleted(true);
    }

    /**
     * Constructs a string of the construction parameters and outputs the string to the messsage frame if the logging
     * procedure is turned on.
     */
    private void constructLog() {

        historyString = new String("Mean(" + String.valueOf(kernelSize) + ", " + String.valueOf(entireImage) + ")" +
                                   "\n");

    }


    /**
     * DOCUMENT ME!
     *
     * @param   i     The central pixel to find neighbors for.
     * @param   data  Image data
     * @param   is2D  True indicates that the neighbors are found along a 2D slice (or 2D image) instead of neighbors in
     *                a 3D volume.
     *
     * @return  mean value of the kernel neighborhood
     */
    private float getMean(int i, float[] data, boolean is2D) {
        int row, col;
        int width = 0; // width of slice in number of pixels
        int height = 0; // height of slice in number of pixels
        int depth = 0;
        float total = 0.0f;
        float mean;
        width = srcImage.getExtents()[0];
        height = srcImage.getExtents()[1];

        if (!is2D) {
            depth = srcImage.getExtents()[2];
        }


        int sliceWidth = width * valuesPerPixel; // width of slice in number of elements
        int imageSliceLength = srcImage.getSliceSize() * valuesPerPixel;
        int sliceLoc = i / imageSliceLength;
        int rowLoc = (i % imageSliceLength) / sliceWidth;
        int columnLoc = i % sliceWidth;
        int leftBound;
        int rightBound;
        int upperBound;
        int lowerBound;
        int behindBound;
        int aheadBound;

        if (isColorImage) {
            leftBound = Math.max(-((columnLoc/4)*4), -4 * halfK);
            rightBound = Math.min(sliceWidth - 1 - columnLoc, 4 * halfK);
        } else {
            leftBound = Math.max(-columnLoc, -halfK);
            rightBound = Math.min(width - 1 - columnLoc, halfK);
        }

        upperBound = Math.max(-rowLoc, -halfK);
        lowerBound = Math.min(height - 1 - rowLoc, halfK);
        behindBound = Math.max(-sliceLoc, -halfK);
        aheadBound = Math.min(depth - 1 - sliceLoc, halfK);


        // place all the masked 'on' elements into the data-list
        int count = 0;

        // colour images are different from the mono images in that though colour images use the same size
        // kernel as mono images, but fill it with brightness levels that are spread out in the data set.
        if (is2D) {

            for (row = upperBound; row <= lowerBound; row++) { // go through all rows

                for (col = leftBound; col <= rightBound; col += valuesPerPixel) { //
                    total += data[i + col + (row * sliceWidth)];
                    count++;
                }
            }
        } else { // find neighbors in a volume

            int slice;

            for (slice = behindBound; slice <= aheadBound; slice++) {

                for (row = upperBound; row <= lowerBound; row++) {

                    for (col = leftBound; col <= rightBound; col += valuesPerPixel) {
                        total += data[i + col + (row * sliceWidth) + (slice * sliceWidth * height)];
                        count++;
                    }
                }
            }
        }

        mean = total / count;

        return (mean);
    }

    /**
     * If the progress bar is visible, sets the text to:<br>
     * <tt>Copying all <i>color</i> values ...</tt>
     *
     * @param  colorText  The color to use. Eg., "red" or "blue".
     * @param  process    DOCUMENT ME!
     */
    private void setCopyColorText(String colorText, boolean process) {

        try {

            if (pBarVisible == true) {

                if (process) {
                    progressBar.setMessage("Averaging " + colorText + " values ... ");
                } else {
                    progressBar.setMessage("Copying all " + colorText + " values ... ");
                }
            }
        } catch (NullPointerException npe) {

            if (Preferences.debugLevel(Preferences.DEBUG_ALGORITHM)) {
                Preferences.debug("AlgrithmMean: NullPointerException found while setting progress bar text.");
            }
        }
    }


    /**
     * Allows a single slice to be filtered. Note that a progressBar must be created first.
     *
     * @param  srcBuffer            Source buffer.
     * @param  destBuffer           Destination Buffer.
     * @param  bufferStartingPoint  Starting point for the buffer.
     * @param  msgString            A text message that can be displayed as a message text in the progressBar.
     */
    private void sliceFilter(float[] srcBuffer, float[] destBuffer, int bufferStartingPoint, String msgString) {
        int i, a; // counting....   i is the offset from the bufferStartingPoint

        // a adds support for 3D filtering by counting as the pixel at the starting point plus the counter offset
        int buffStart = bufferStartingPoint; // data element at the buffer. a = bufferStartingPoint+i
        int sliceLength = srcImage.getSliceSize();
        int imageSliceLength = sliceLength * valuesPerPixel; // since there are 4 values for every color pixel
        int initialIndex = 0; // first element is alpha

        int mod; // 1% length of slice for percent complete

        if (isColorImage) {

            // data element at the buffer (a = i+bufferStartingPoint) must start on an alpha value
            buffStart = bufferStartingPoint - (bufferStartingPoint % 4); // & no effect if bufferStartingPoint%4 == 0
                                                                         // !!!

            // copy all alpha values in this slice
            setCopyColorText("alpha", false);

            for (a = buffStart, i = 0; i < imageSliceLength; a += 4, i += 4) {
                destBuffer[a] = srcBuffer[a]; // copy alpha;
            }

        }

        mod = (imageSliceLength * numberOfSlices) / 100; // mod is 1 percent of length of slice * the number of slices.

        a = buffStart; // set/reset a to address pixels from the beginning of this buffer.

        if (isColorImage) { // color image dealt with in special way

            // choose i so the proper colors go
            // copy only needed RGB values
            initialIndex = 0; // start with alpha on each pass (routine moved so we don't do it for each pass)

            while ((initialIndex < 3) && !threadStopped) { // alpha:0, R:1, G:2, B:3.  But alpha must be copied
                ++initialIndex; // next initial index
                a += initialIndex; // keep the pixel location up with color indexed to

                if ((numberOfSlices > 1) && (pBarVisible == true)) { // 3D image     update progressBar

                    // do a progress bar update
                    progressBar.updateValue(Math.round((((float) (currentSlice) / numberOfSlices) * 100)), activeImage);
                }

                if ((initialIndex == 1) && (!rChannel)) {

                    // when looking at the image reds but we're not filtering the red channel
                    // copy all red values
                    setCopyColorText("red", false);

                    for (i = initialIndex; i < imageSliceLength; a += 4, i += 4) {
                        destBuffer[a] = srcBuffer[a];
                    }
                } else if ((initialIndex == 2) && (!gChannel)) {

                    // when looking at the image greens but we're not filtering the greens channel
                    // copy all greens values
                    setCopyColorText("green", false);

                    for (i = initialIndex; i < imageSliceLength; a += 4, i += 4) {
                        destBuffer[a] = srcBuffer[a];
                    }
                } else if ((initialIndex == 3) && (!bChannel)) {

                    // when looking at the image blues but we're  not filtering the blues channel
                    // copy all blue values
                    setCopyColorText("blue", false);

                    for (i = initialIndex; i < imageSliceLength; a += 4, i += 4) {
                        destBuffer[a] = srcBuffer[a];
                    }
                } else {

                    // if we needed to filter the image, we dropped through the selection to filter the
                    // color given by int initialIndex
                    if (numberOfSlices == 1) {

                        if (initialIndex == 1) {
                            setCopyColorText("red", true);
                        } else if (initialIndex == 2) {
                            setCopyColorText("green", true);
                        } else if (initialIndex == 3) {
                            setCopyColorText("blue", true);
                        }
                    } // if (numberOfSlices == 1)
                    else {
                        progressBar.setMessage("Averaging values ...");
                    }

                    for (i = initialIndex; (i < imageSliceLength) && !threadStopped; a += 4, i += 4) {

                        if (numberOfSlices == 1) { // 2D image     update progressBar

                            if ((((i - initialIndex) % mod) == 0) && (pBarVisible == true)) {
                                progressBar.updateValue(Math.round((float) (((initialIndex - 1) * sliceLength) +
                                                                            (i / 4)) / (3 * sliceLength) * 100),
                                                        activeImage);
                            }
                        }

                        if ((entireImage == true) || mask.get(a / 4)) { // may have problems in masking .....

                            // in bounds
                            destBuffer[a] = getMean(a, srcBuffer, true);
                        } else { // not part of the VOI so just copy this into the destination buffer.
                            destBuffer[a] = srcBuffer[a];
                        }
                    }
                }

                a = buffStart; // reset the index back to the beginning of the filterarea
            }
        } else { // monochrome image

            if (pBarVisible) {

                if (numberOfSlices > 1) { // 3D image     update progressBar

                    // do a progress bar update
                    progressBar.updateValue(Math.round((((float) (currentSlice) / (numberOfSlices)) * 100)),
                                            activeImage);
                }
            }

            for (i = 0; (i < imageSliceLength) && !threadStopped; i++) {

                if (numberOfSlices == 1) { // 2D image     update progressBar

                    if (((i % mod) == 0) && (pBarVisible == true)) {
                        progressBar.updateValue(Math.round((float) i / (imageSliceLength) * 100), activeImage);
                    }
                }

                if ((entireImage == true) || mask.get(a)) {
                    destBuffer[a] = getMean(a, srcBuffer, true);
                } else { // not part of the VOI so just copy this into the destination buffer.
                    destBuffer[a] = srcBuffer[a];
                }

                a++; // address the next data element from the bufferStartingPoint
            }
        }

        // destBuffer should now be copied over for the size of imageSliceLength.  You may return....
    }
    
    /**
     * Allows a single slice to be filtered. Note that a progressBar must be created first.
     *
     * @param  srcBuffer            Source buffer.
     * @param  destBuffer           Destination Buffer.
     * @param  bufferStartingPoint  Starting point for the buffer.
     * @param  msgString            A text message that can be displayed as a message text in the progressBar.
     */
    private void sliceVectorFilter(float[] srcBuffer, float[] destBuffer, int bufferStartingPoint, String msgString) {
        int i, a; // counting....   i is the offset from the bufferStartingPoint

        // a adds support for 3D filtering by counting as the pixel at the starting point plus the counter offset
        int buffStart = bufferStartingPoint; // data element at the buffer. a = bufferStartingPoint+i
        int sliceLength = srcImage.getSliceSize();
        int imageSliceLength = sliceLength * valuesPerPixel; // since there are 4 values for every color pixel
        int initialIndex = 0; // first element is alpha
        int slicePerpLength = 3 * sliceLength;
        float perpBuffer[];
        int index;
        float redMult;
        float greenMult;
        float blueMult;
        
        perpBuffer = new float[slicePerpLength];

        int mod; // 1% length of slice for percent complete

        // data element at the buffer (a = i+bufferStartingPoint) must start on an alpha value
        buffStart = bufferStartingPoint - (bufferStartingPoint % 4); // & no effect if bufferStartingPoint%4 == 0
                                                                     // !!!

        for (a = buffStart, i = 0, index = 0; i < imageSliceLength; a += 4, i += 4, index += 3) {
            destBuffer[a] = srcBuffer[a]; // copy alpha;
            if ((entireImage == true) || mask.get(a / 4)) { 
                srcBuffer[a+1] = srcBuffer[a+1] - midGray;
                srcBuffer[a+2] = srcBuffer[a+2] - midGray;
                srcBuffer[a+3] = srcBuffer[a+3] - midGray;
                redMult = -redNorm*redNorm*srcBuffer[a+1]
                          -2*redNorm*greenNorm*srcBuffer[a+2]
                          -2*redNorm*blueNorm*srcBuffer[a+3]
                          +blueNorm*blueNorm*srcBuffer[a+1]
                          +greenNorm*greenNorm*srcBuffer[a+1];
                greenMult = -2*redNorm*srcBuffer[a+1]*greenNorm
                            -greenNorm*greenNorm*srcBuffer[a+2]
                            -2*greenNorm*blueNorm*srcBuffer[a+3]
                            +blueNorm*blueNorm*srcBuffer[a+2]
                            +redNorm*redNorm*srcBuffer[a+2];
                blueMult = -2*redNorm*srcBuffer[a+1]*blueNorm
                           -2*greenNorm*srcBuffer[a+2]*blueNorm
                           -blueNorm*blueNorm*srcBuffer[a+3]
                           +greenNorm*greenNorm*srcBuffer[a+3]
                           +redNorm*redNorm*srcBuffer[a+3];
                perpBuffer[index] = 0.5f*(srcBuffer[a+1] + redMult);
                srcBuffer[a+1] = 0.5f*(srcBuffer[a+1] - redMult);
                perpBuffer[index+1] = 0.5f*(srcBuffer[a+2] + greenMult);
                srcBuffer[a+2] = 0.5f*(srcBuffer[a+2] - greenMult);
                perpBuffer[index+2] = 0.5f*(srcBuffer[a+3] + blueMult);
                srcBuffer[a+3] = 0.5f*(srcBuffer[a+3] - blueMult);
            }
        }

        mod = (imageSliceLength * numberOfSlices) / 100; // mod is 1 percent of length of slice * the number of slices.

        a = buffStart; // set/reset a to address pixels from the beginning of this buffer.

        // choose i so the proper colors go
        // copy only needed RGB values
        initialIndex = 0; // start with alpha on each pass (routine moved so we don't do it for each pass)

        while ((initialIndex < 3) && !threadStopped) { // alpha:0, R:1, G:2, B:3.  But alpha must be copied
            ++initialIndex; // next initial index
            a += initialIndex; // keep the pixel location up with color indexed to

            if ((numberOfSlices > 1) && (pBarVisible == true)) { // 3D image     update progressBar

                // do a progress bar update
                progressBar.updateValue(Math.round((((float) (currentSlice) / numberOfSlices) * 100)), activeImage);
            }

            

            // if we needed to filter the image, we dropped through the selection to filter the
            // color given by int initialIndex
            if (numberOfSlices == 1) {

                if (initialIndex == 1) {
                    setCopyColorText("red", true);
                } else if (initialIndex == 2) {
                    setCopyColorText("green", true);
                } else if (initialIndex == 3) {
                    setCopyColorText("blue", true);
                }
            } // if (numberOfSlices == 1)
            else {
                progressBar.setMessage("Averaging values ...");
            }

            for (i = initialIndex, index = 0; (i < imageSliceLength) && !threadStopped;
                                           a += 4, i += 4, index += 3) {

                if (numberOfSlices == 1) { // 2D image     update progressBar

                    if ((((i - initialIndex) % mod) == 0) && (pBarVisible == true)) {
                        progressBar.updateValue(Math.round((float) (((initialIndex - 1) * sliceLength) +
                                                                    (i / 4)) / (3 * sliceLength) * 100),
                                                activeImage);
                    }
                }

                if ((entireImage == true) || mask.get(a / 4)) { // may have problems in masking .....

                    // in bounds
                    destBuffer[a] = getMean(a, srcBuffer, true) 
                                    + perpBuffer[index + initialIndex - 1] + midGray;
                    if (destBuffer[a] < 0.0f) {
                        destBuffer[a] = 0.0f;
                    }
                    else if (destBuffer[a] > colorMax) {
                        destBuffer[a] = colorMax;
                    }
                } else { // not part of the VOI so just copy this into the destination buffer.
                    destBuffer[a] = srcBuffer[a];
                }
            }

        a = buffStart; // reset the index back to the beginning of the filterarea
        }

    // destBuffer should now be copied over for the size of imageSliceLength.  You may return....
    }

    /**
     * Filter a Color 3D image with a 3D kernel. Allows mean filtering to include the picture elements at greater depths
     * than only the current slice. This method allows selected band filtering, and does not filter the alpha band.
     *
     * @param  srcBuffer   Source image.
     * @param  destBuffer  Destination image.
     *
     * @see    volumeFilter
     */
    private void volumeColorFilter(float[] srcBuffer, float[] destBuffer) {
        int i; // counting the current element
        int initialIndex; // reference to the color band being filtered/copied: aRBG: 0, 1, 2, 3;
                          // it is an offset to the identified pixel, or column, of the slice
        int width = srcImage.getExtents()[0]; // width of slice in number of pixels
        int height = srcImage.getExtents()[1]; // height of slice in number of pixels
        int sliceSize = width * height; // in pixels (or elements)
        int imageSliceLength = width * height * valuesPerPixel; // in values-pixels
        int imageSize = sliceSize * numberOfSlices; // in pixels (or elements)
        int imageLength = imageSliceLength * numberOfSlices; // in (values-pixels)


        // we may say that each column is a pixel intensity: mono images have 1 per pixel, 4 in color;
        // these calculations are done seperately for color & mono images in sliceFilter().

        int mod = (imageSize) / 100; // mod is 1 percent of length of slice * the number of slices.

        // copy all alpha values in the image
        setCopyColorText("alpha", false);

        for (i = 0; i < imageLength; i += 4) {
            destBuffer[i] = srcBuffer[i]; // copy alpha;
        }

        // choose i so the proper colors go alongside the initial index
        // so we get the right output statements in the progress bar
        // copy only needed RGB values
        initialIndex = 0; // start with alpha on each pass (routine moved so we don't do it for each pass)

        while ((initialIndex < 3) && !threadStopped) { // alpha:0, R:1, G:2, B:3.  But alpha must be copied
            ++initialIndex; // next initial index

            if ((initialIndex == 1) && (!rChannel)) {

                // when looking at the image reds but we're not filtering the red channel
                // copy all red values
                setCopyColorText("red", false);

                for (i = initialIndex; i < imageLength; i += 4) {
                    destBuffer[i] = srcBuffer[i];
                }
            } else if ((initialIndex == 2) && (!gChannel)) {

                // when looking at the image greens but we're not filtering the greens channel
                // copy all greens values
                setCopyColorText("green", false);

                for (i = initialIndex; i < imageLength; i += 4) {
                    destBuffer[i] = srcBuffer[i];
                }
            } else if ((initialIndex == 3) && (!bChannel)) {

                // when looking at the image blues but we're  not filtering the blues channel
                // copy all blue values
                setCopyColorText("blue", false);

                for (i = initialIndex; i < imageLength; i += 4) {
                    destBuffer[i] = srcBuffer[i];
                }
            } else {

                if (initialIndex == 1) {
                    setCopyColorText("red", true);
                } else if (initialIndex == 2) {
                    setCopyColorText("green", true);
                } else if (initialIndex == 3) {
                    setCopyColorText("blue", true);
                }

                // if we needed to filter the image, we dropped through the selection to filter the
                // color given by int initialIndex
                for (i = initialIndex; (i < imageLength) && !threadStopped; i += 4) {

                    if ((((i - initialIndex) % mod) == 0) && (pBarVisible == true)) {
                        progressBar.updateValue(Math.round(((float) (((initialIndex - 1) * imageSize) + (i / 4)) /
                                                                (3 * imageSize) * 100)), activeImage);
                    }

                    if ((entireImage == true) || mask.get(i / valuesPerPixel)) {
                        destBuffer[i] = getMean(i, srcBuffer, false);
                    } else { // not part of the VOI so just copy this into the destination buffer.
                        destBuffer[i] = srcBuffer[i];
                    }
                }
            }
        }
    }
    
    /**
     * Filter a Color 3D image with a 3D kernel. Allows mean filtering to include the picture elements at greater depths
     * than only the current slice. This method allows selected band filtering, and does not filter the alpha band.
     *
     * @param  srcBuffer   Source image.
     * @param  destBuffer  Destination image.
     *
     * @see    volumeFilter
     */
    private void volumeVectorFilter(float[] srcBuffer, float[] destBuffer) {
        int i; // counting the current element
        int initialIndex; // reference to the color band being filtered/copied: aRBG: 0, 1, 2, 3;
                          // it is an offset to the identified pixel, or column, of the slice
        int width = srcImage.getExtents()[0]; // width of slice in number of pixels
        int height = srcImage.getExtents()[1]; // height of slice in number of pixels
        int sliceSize = width * height; // in pixels (or elements)
        int imageSliceLength = width * height * valuesPerPixel; // in values-pixels
        int imageSize = sliceSize * numberOfSlices; // in pixels (or elements)
        int imageLength = imageSliceLength * numberOfSlices; // in (values-pixels)
        int volumePerpLength = 3 * width * height * numberOfSlices;
        float perpBuffer[];
        int index;
        float redMult;
        float greenMult;
        float blueMult;
        
        perpBuffer = new float[volumePerpLength];

        // we may say that each column is a pixel intensity: mono images have 1 per pixel, 4 in color;
        // these calculations are done seperately for color & mono images in sliceFilter().

        int mod = (imageSize) / 100; // mod is 1 percent of length of slice * the number of slices.

        for (i = 0, index = 0; i < imageLength; i += 4, index += 3) {
            destBuffer[i] = srcBuffer[i]; // copy alpha;
            if ((entireImage == true) || mask.get(i / 4)) { 
                srcBuffer[i+1] = srcBuffer[i+1] - midGray;
                srcBuffer[i+2] = srcBuffer[i+2] - midGray;
                srcBuffer[i+3] = srcBuffer[i+3] - midGray;
                redMult = -redNorm*redNorm*srcBuffer[i+1]
                          -2*redNorm*greenNorm*srcBuffer[i+2]
                          -2*redNorm*blueNorm*srcBuffer[i+3]
                          +blueNorm*blueNorm*srcBuffer[i+1]
                          +greenNorm*greenNorm*srcBuffer[i+1];
                greenMult = -2*redNorm*srcBuffer[i+1]*greenNorm
                            -greenNorm*greenNorm*srcBuffer[i+2]
                            -2*greenNorm*blueNorm*srcBuffer[i+3]
                            +blueNorm*blueNorm*srcBuffer[i+2]
                            +redNorm*redNorm*srcBuffer[i+2];
                blueMult = -2*redNorm*srcBuffer[i+1]*blueNorm
                           -2*greenNorm*srcBuffer[i+2]*blueNorm
                           -blueNorm*blueNorm*srcBuffer[i+3]
                           +greenNorm*greenNorm*srcBuffer[i+3]
                           +redNorm*redNorm*srcBuffer[i+3];
                perpBuffer[index] = 0.5f*(srcBuffer[i+1] + redMult);
                srcBuffer[i+1] = 0.5f*(srcBuffer[i+1] - redMult);
                perpBuffer[index+1] = 0.5f*(srcBuffer[i+2] + greenMult);
                srcBuffer[i+2] = 0.5f*(srcBuffer[i+2] - greenMult);
                perpBuffer[index+2] = 0.5f*(srcBuffer[i+3] + blueMult);
                srcBuffer[i+3] = 0.5f*(srcBuffer[i+3] - blueMult);
            }
        }

        // choose i so the proper colors go alongside the initial index
        // so we get the right output statements in the progress bar
        // copy only needed RGB values
        initialIndex = 0; // start with alpha on each pass (routine moved so we don't do it for each pass)

        while ((initialIndex < 3) && !threadStopped) { // alpha:0, R:1, G:2, B:3.  But alpha must be copied
            ++initialIndex; // next initial index

            

            if (initialIndex == 1) {
                setCopyColorText("red", true);
            } else if (initialIndex == 2) {
                setCopyColorText("green", true);
            } else if (initialIndex == 3) {
                setCopyColorText("blue", true);
            }

            // if we needed to filter the image, we dropped through the selection to filter the
            // color given by int initialIndex
            for (i = initialIndex, index = 0; (i < imageLength) && !threadStopped; i += 4,
                                                index += 3) {

                if ((((i - initialIndex) % mod) == 0) && (pBarVisible == true)) {
                    progressBar.updateValue(Math.round(((float) (((initialIndex - 1) * imageSize) + (i / 4)) /
                                                            (3 * imageSize) * 100)), activeImage);
                }

                if ((entireImage == true) || mask.get(i / valuesPerPixel)) {
                    destBuffer[i] = getMean(i, srcBuffer, false) +
                    + perpBuffer[index + initialIndex - 1] + midGray;
                    if (destBuffer[i] < 0.0f) {
                        destBuffer[i] = 0.0f;
                    }
                    else if (destBuffer[i] > colorMax) {
                        destBuffer[i] = colorMax;
                    }
                } else { // not part of the VOI so just copy this into the destination buffer.
                    destBuffer[i] = srcBuffer[i];
                }
            }
        }
    }

    /**
     * Filter a 3D image with a 3D kernel. Allows mean filtering to include the picture elements at greater depths than
     * only the current slice.
     *
     * <p><em>Note that this volume filter will correctly filter color images on all bands (aRGB) because the neighbor
     * list is correct (see getNeighborList()). This means, however, it will not selectivly filter any bands (one may
     * not filter only the Red channel, for instance), and will also filter all alpha values as well. Of course,
     * progress bar updates will not include any color information. For these reasons it a useable, but limited color
     * filter.</em></p>
     *
     * @param  srcBuffer   Source image.
     * @param  destBuffer  Destination image.
     *
     * @see    volumeColorFilter
     * @see    getNeighborList
     */
    // some code has been left in to allow this method to properly filter
    // color images, although the other method is included.
    private void volumeFilter(float[] srcBuffer, float[] destBuffer) {
        int i; // counting the current element

        int imageLength = srcImage.getSliceSize() * valuesPerPixel * numberOfSlices;


        // we may say that each column is a pixel intensity: mono images have 1 per pixel, 4 in color;
        // these calculations are done seperately for color & mono images in sliceFilter().

        int mod = (imageLength) / 100; // mod is 1 percent of length of slice * the number of slices


        for (i = 0; (i < imageLength) && !threadStopped; i++) {

            if (((i % mod) == 0) && (pBarVisible == true)) {
                progressBar.updateValue(Math.round((((float) (i) / (imageLength - 1)) * 100)), activeImage);
            }

            if ((entireImage == true) || mask.get(i / valuesPerPixel)) {
                destBuffer[i] = getMean(i, srcBuffer, false);
            } else { // not part of the VOI so just copy this into the destination buffer.
                destBuffer[i] = srcBuffer[i];
            }
        }

    }

}
