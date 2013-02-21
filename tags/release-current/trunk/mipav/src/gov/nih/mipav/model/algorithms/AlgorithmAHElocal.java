package gov.nih.mipav.model.algorithms;


import gov.nih.mipav.model.structures.*;

import gov.nih.mipav.view.*;

import java.io.*;

import java.util.*;


/**
 * algorithm to apply an adaptive histogram to an image, placing it in a new ModelImage, or returning the changed
 * picture to the same image.
 *
 * <p>This is an algorithm to apply within a neighborhood, that is it selects a region of adjacent pixels, the number
 * of adjacent pixels is of a user-selected size, and forms the histogram from that region. The region selection routine
 * is adopted from AlgorithmMedian. The value of the cell (the brightness of the pixel at a particular location) is then
 * mapped to the region. Over the entire image, this means that all pixels will still be brighter or darker than other
 * pixels, but the brightness has been shifted making minor variations more apparent. The cumulative distribution is
 * changed so that it becomes step-wise linear, making the darker pixels use the lowest possible values available to the
 * image, the brightest use the highest possible values.</p>
 *
 * <p>This algorithm may be used to perform a contrast-limited or 'clamped' histogram equalization. It is set by
 * supplying a percentage of the maximum number of pixels in any particular brightness level. As the regional histograms
 * are tabulated, the brightness with the maximum number of pixels attributed to it is remembered. The algorithm will
 * then evenly redistribute the total number of pixels from any brightness which has a greater number than the max times
 * this fraction to all other (less populous) brightnesses. (If the supplied percentage is 80, for instance, the maximum
 * number of pixels to any brightness level, will be four-fifths the largest number number of pixels of any shade found.
 * </p>
 *
 * <p>The principle methodology of this algorithm, is to take each window, form a histogram; clamp so there is no
 * brightness which can have more than a certain number of pixels; scale the brightnesses so that the values are spread
 * evenly from the maximum to the minimum brightness. (ie., scale * histogram(i)).</p>
 *
 * <ul>
 *   <li>Primary source: Stark, J Alex. <u>Adaptive Image Contrast Enhancement Using Generalizations of Histogram
 *     Equalization</u>.</li>
 *   <li>Background Information:
 *
 *     <ul>
 *       <li>Russ, John. <u>Image Processing HandbookM</u>.</li>
 *       <li>Tidestav, Claes. <u>Short Introduction to Adaptive Equalization</u>.</li>
 *       <li>Rabie, Tamer; Rangayan, Rangaraj; Paranjape, Raman. <u>Adaptive-Neighborhood Image Deblurring</u>.</li>
 *     </ul>
 *   </li>
 * </ul>
 *
 * <p>According to Freedman and Diaconis as summarized in "Recent Developments in NonParametric Density Estimation" by
 * Alan Julian Izenman, Journal of the American Statistical Association, March, 1991, Vol. 86, No. 413, pp. 205 - 224:
 * The ideal histogram bin width W is given by W = 2(IQR)pow(N,-1/3) where IQR is the interquartile range(the 75
 * percentile minus the 25th percentile) and N is the number of available samples.</p>
 *
 * @version  1.00; 24 Sep 2001
 * @author   David Parsons (parsonsd)
 * @author   Matthew J. McAuliffe, Ph.D.
 * @see      gov.nih.mipav.model.algorithms.filters.AlgorithmMedian
 */
public class AlgorithmAHElocal extends AlgorithmBase {

    //~ Static fields/initializers -------------------------------------------------------------------------------------

    /** square kernel. */
    public static final int SQUARE_KERNEL = 0;

    /** cross (+). */
    public static final int CROSS_KERNEL = 1;

    /** along the major axis (+). */
    public static final int AXIAL_KERNEL = 1;

    /** DOCUMENT ME! */
    public static final int scaleOnLocal = 0;

    /** DOCUMENT ME! */
    public static final int scaleOnSlice = 1;

    /** DOCUMENT ME! */
    public static final int scaleOnImage = 2;

    //~ Instance fields ------------------------------------------------------------------------------------------------

    /** pixel does not get remapped when pixel is below threshold. */
    protected float absMinThreshold = 0;

    /** the blue channel. */
    protected boolean bChannel = true;

    /**
     * a pixel (no matter whether represented as a float or int), has a brightnessLevel, which is in the histogram as a
     * value between 0 and totalBins.
     */
    protected int brightnessLevel;

    /** maximum of the image buffer. */
    protected float bufMax;

    /** minimum of the image buffer. */
    protected float bufMin; //

    /** perform a "contrast limited" AHE. */
    protected boolean clampingIsNecessary = false;

    /** DOCUMENT ME! */
    protected float clipLevel = 0;

    /** DOCUMENT ME! */
    protected int currentSlice = 0;

    /** true means apply to entire image, false only region. */
    protected boolean entireImage;

    /** the green channel. */
    protected boolean gChannel = true;

    /** DOCUMENT ME! */
    protected int halfK;

    /** (cumulative) histogram. */
    protected int[] histogram;

    /** the scaled histogram. */
    protected float[] histomap;

    /** image minimum; used as offset for images to build histogram. */
    protected float imageOffset;

    /** indicates the image is a color image. */
    protected boolean isColorImage = false;

    /** DOCUMENT ME! */
    protected float[] kernel;

    /** DOCUMENT ME! */
    protected int kernelCenter;

    /** mask to determine the region of pixels used in a median filter. */
    protected byte[] kernelMask;

    /** user-selectable shape of the region for neighbor-selection. */
    protected int kernelShape;

    /** dimension of the kernel (ie., 5 = 5x5, 7 = 7x7, 9 = 9x9, etc.). */
    protected int kernelSize;

    /** DOCUMENT ME! */
    protected BitSet mask = null;

    /** DOCUMENT ME! */
    protected int maskCenter;

    /** range of max - min over local kernel, slice, or entire image. */
    protected float maxScale;

    /** flag how/when to set the maximum scale. default per slice */
    protected int maxScaleRule = scaleOnSlice;

    /** DOCUMENT ME! */
    protected float minThreshold;

    /** min over local kernel, slice, or entire image. */
    protected float minValue;

    /** DOCUMENT ME! */
    protected int numberOfSlices;

    /** if T, filter the red channel. */
    protected boolean rChannel = true;

    /** here for memory/speed concerns. */
    protected float[] sortBuffer;

    /** threshold the image while processing. */
    protected boolean thresholdingIsNecessary = false;

    /** number of histogram and histomap values actually used. */
    protected int totalBins;

    /** one unit is one totalBin-th of the distance between max & min brightness. */
    protected float unitSize;

    /** number of elements in a pixel. Monochrome = 1, Color = 4. (a, R, G, B) */
    protected int valuesPerPixel = 1;
    
    int numColors;
    
    int colorUsed;

    //~ Constructors ---------------------------------------------------------------------------------------------------

    /**
     * Constructor for images in which changes are returned to the source image.
     *
     * @param  srcImg      source image model
     * @param  kSize       size of kernel
     * @param  kShape      shape of kernel
     * @param  wholeImage  If true whole image used
     */
    public AlgorithmAHElocal(ModelImage srcImg, int kSize, int kShape, boolean wholeImage) {
        super(null, srcImg);

        if (srcImg.isColorImage()) {
            isColorImage = true;
            valuesPerPixel = 4;
        }

        kernelSize = kSize;
        kernelShape = kShape;
        makeKernelMask(); // define which pixels in the neighborhood to use
        makeKernel(); // define the kernel to be of a given size for its shape
        entireImage = wholeImage;

        if (entireImage == false) {
            mask = srcImage.generateVOIMask();
        }

        try {
            sortBuffer = new float[kernel.length];
            histogram = new int[2000];
            histomap = new float[2000];
        } catch (OutOfMemoryError oome) {
            errorCleanUp("AlgorithmAHElocal reports: out of memory", true);
            setCompleted(false);
        }
    }

    /**
     * Constructor for images in which changes are placed in a predetermined destination image.
     *
     * @param  destImg     image model where result image is to stored
     * @param  srcImg      source image model
     * @param  kSize       size of kernel
     * @param  kShape      shape of kernel
     * @param  wholeImage  If true whole image used
     */
    public AlgorithmAHElocal(ModelImage destImg, ModelImage srcImg, int kSize, int kShape, boolean wholeImage) {

        super(destImg, srcImg);

        if (srcImg.isColorImage()) {
            isColorImage = true;
            valuesPerPixel = 4;
        }

        kernelSize = kSize;
        kernelShape = kShape;
        makeKernelMask(); // define which pixels in the neighborhood to use
        makeKernel(); // define the kernel to be of a given size for its shape
        entireImage = wholeImage;

        if (entireImage == false) {
            mask = srcImage.generateVOIMask();
        }

        try { // slower if this must be done every slice.
            sortBuffer = new float[kernel.length];
            histogram = new int[2000];
            histomap = new float[2000];
        } catch (OutOfMemoryError oome) {
            errorCleanUp("AlgorithmAHElocal reports: out of memory", true);
            setCompleted(false);
        }
    }

    //~ Methods --------------------------------------------------------------------------------------------------------

    /**
     * Prepares this class for destruction.
     */
    public void finalize() {
        destImage = null;
        srcImage = null;
        kernel = null;
        kernelMask = null;
        sortBuffer = null;
        histogram = null;
        histomap = null;
        super.finalize();
    }

    /**
     * starts the algorithm.
     */
    public void runAlgorithm() {

        if (srcImage == null) {
            displayError("Source Image is null");

            return;
        }

        

        if (destImage != null) { // if there exists a destination image

            if (srcImage.getNDims() == 2) {
                calcStoreInDest2D();
            } else if (srcImage.getNDims() > 2) {
                calcStoreInDest3D();
            }
        } else { // the destination is the original source.

            if (srcImage.getNDims() == 2) {
                calcInPlace2D();
            } else if (srcImage.getNDims() > 2) {
                calcInPlace3D();
            }
        }
    }

    /**
     * The Clip Level is a percentage of the maximum number of pixels in any particular brightness level. This method
     * allows one to set that percentage. As the regional histograms are tabulated, the brightness with the maximum
     * number of pixels attributed to it is remembered. The algorithm will then evenly redistibute the total number of
     * pixels from any brightness which has a greater number than the max times this fraction to all other (less
     * populous) brightnesses.
     *
     * @param  sectorPercentage  DOCUMENT ME!
     */
    public void setClipLevel(int sectorPercentage) {
        clipLevel = (float) (sectorPercentage) / 100;
    }

    /**
     * Redistribute from brightnesses with at a least a specified percentage of the brightness with the maximum number
     * of pixels.
     *
     * @param  beClamped  DOCUMENT ME!
     */
    public void setContrastLimited(boolean beClamped) {
        clampingIsNecessary = beClamped;
    }

    /**
     * permit the user to select the scale on which the algorithm forms the brightness pattern of the histogram. The
     * default is scaleOnSlice.
     *
     * <p>The options are:</p>
     *
     * <ul>
     *   <li>scale On Local neighborhood</li>
     *   <li>scale on Slice</li>
     *   <li>scale on image</li>
     * </ul>
     *
     * @param  scaleRule  DOCUMENT ME!
     */
    public void setHistogramScaleRule(int scaleRule) {
        maxScaleRule = ((scaleRule < 0) || (scaleRule >= 3)) ? scaleOnSlice : scaleRule;
    }

    /**
     * sets minimum threshold values. Pixels are copied over when both the source pixel and the local neighborhood
     * surrounding the pixel are all less than the given minimum pixel value.
     *
     * @param  minThresh  DOCUMENT ME!
     */
    public void setMinimumThreshold(float minThresh) {
        minThreshold = minThresh;
    }

    /**
     * RGB images are histogram equalized by 'channel.' That is, each color, red, blue and green, is run independently
     * of the other two colors. This permits selectively filtering any combination of the three channels instead of
     * simply trying to handle all three at once. True filters that channel.
     *
     * @param  r  DOCUMENT ME!
     * @param  g  DOCUMENT ME!
     * @param  b  DOCUMENT ME!
     */
    public void setRGBChannelFilter(boolean r, boolean g, boolean b) {

        if (isColorImage) { // in case a mono image
            rChannel = r;
            gChannel = g;
            bChannel = b;
        }
    }

    /**
     * If useThreshold true only copy when both the source pixel and its local neighborhoold are are less than
     * minThreshold.
     *
     * @param  useThreshold  DOCUMENT ME!
     */
    public void setThresholdImage(boolean useThreshold) {
        thresholdingIsNecessary = useThreshold;
    }

    /**
     * make histogram uses the class-global vars <code>kernel</code> and <code>histogram</code>.
     *
     * @return  the index to the bin with the highest count.
     */
    protected int buildHistogram() {
        int brightnessLevel = 0;
        int i;
        int maxCount = 0;
        int maxbin = 0;
        float idealWidth;
        int idealBins;

        float maxValue = -Float.MAX_VALUE;

        if (maxScaleRule == scaleOnLocal) {
            maxValue = -Float.MAX_VALUE; // clear the histogram maximum
            minValue = Float.MAX_VALUE;
        }

        for (i = 0; i < kernel.length; i++) {
            sortBuffer[i] = kernel[i];
        }

        Arrays.sort(sortBuffer);

        idealWidth = (float) (2.0f * (sortBuffer[(3 * kernel.length / 4) - 1] - sortBuffer[(kernel.length / 4) - 1]) *
                                  Math.pow(kernel.length, -1.0 / 3.0));

        // calculates largest and smallest pixel values in a sector
        bufMin = sortBuffer[0];
        bufMax = sortBuffer[kernel.length - 1];

        // calculates how many there ought to be given idealwidth formula above
        if (idealWidth >= ((bufMax - bufMin)/1024.0)) {
            idealBins = (int) (((bufMax - bufMin) / idealWidth) + 0.5f);
        }
        else {
        	idealBins = 256;
        }
        
        
        // Preferences.debug("idealBins = " + idealBins + "\n", Preferences.DEBUG_ALGORITHM);

        int type = srcImage.getType();

        totalBins = findTotalBins(type, idealBins); // total bins will be ideal bins except for a few types

        if (totalBins < 5) {
            totalBins = 5;
        }

        // totalBins = (int)(bufMax - bufMin + 1);
        findImageOffset(type);

        // commented out because find image offset takes care of this condition
        // if (isColorImage) { // should this be bufMin or should this be imageOffset?
        // imageOffset = 0;// bufMin is used here & calculating unitsize, but
        // }                   // imageOffset is used in calculating pixel values (and set once).

        // one unit is the distance of one totalBin-th in brightness:
        unitSize = (bufMax - imageOffset) / (totalBins - 1);

        // clear histogram
        if (totalBins > histogram.length) {

            // System.out.println("idealWidth = " + idealWidth + " bufMin = " + bufMin + " bufMax = " + bufMax);
            // System.out.println("Total bins = " + totalBins);
            try {
                histogram = new int[totalBins]; // histogram is both normal histogram & cumulative histogram
                histomap = new float[totalBins]; // histomap is the scaled version of the histogram
            } catch (OutOfMemoryError oome) {
                Preferences.debug("out of memory making histogram & histomap", Preferences.DEBUG_ALGORITHM);
            }
        } else {

            for (i = 0; i < totalBins; i++) {
                histogram[i] = 0;
            }
        }

        // build new histogram.
        for (i = 0; i < kernel.length; i++) {

            try {
                brightnessLevel = (int) ((kernel[i] - imageOffset) / (unitSize));
                histogram[brightnessLevel]++;

                if (maxScaleRule == scaleOnLocal) {

                    // find max brightness in the sector
                    if (kernel[i] > maxValue) {
                        maxValue = kernel[i];
                    }

                    if (kernel[i] < minValue) {
                        minValue = kernel[i];
                    }
                }

                if (histogram[brightnessLevel] > maxCount) {
                    maxCount = histogram[brightnessLevel]; // store the count
                    maxbin = brightnessLevel;
                }
            } catch (ArrayIndexOutOfBoundsException aioobe) {
                Preferences.debug("<HALT>\n", Preferences.DEBUG_ALGORITHM);
                Preferences.debug("kernel: " + kernel[i] + "; imageOffset: " + imageOffset + "; unitSize: " + unitSize +
                                  "\n", Preferences.DEBUG_ALGORITHM);
                Preferences.debug("at histogram: " + (int) (brightnessLevel) + "\n", Preferences.DEBUG_ALGORITHM);
                Preferences.debug("totalbins: " + totalBins + "\n", Preferences.DEBUG_ALGORITHM);
                Preferences.debug("</HALT>\n", Preferences.DEBUG_ALGORITHM);
                MipavUtil.displayError("Algorithm AHElocal reports: buildHistogram out of bounds");
                setCompleted(false);

                return 0;
            }
        }

        if (maxScaleRule == scaleOnLocal) {

            if (isColorImage) {
                maxScale = maxValue;
            } else {
                maxScale = maxValue - minValue;
            }
        }

        return maxbin;
    }

    /**
     * Histogram equalization on the source image. Replaces the original image with the filtered image.
     */
    protected void calcInPlace2D() {
        int color;
        int length; // total number of data-elements (pixels) in image

        numberOfSlices = 1;

        float[] buffer; // data-buffer (for pixel data) which is the "heart" of the image
        float[] resultBuffer; // copy-to buffer (for pixel data) for image-data after filtering

        length = srcImage.getSliceSize();
        
        numColors = 0;

        if (rChannel) {
            numColors++;
        }

        if (gChannel) {
            numColors++;
        }

        if (bChannel) {
            numColors++;
        }

        colorUsed = 0;

        try {
            buffer = new float[length];
            resultBuffer = new float[length];
        } catch (OutOfMemoryError oome) {
            buffer = null;
            resultBuffer = null;
            errorCleanUp("AlgorithmAHElocal reports: out of memory", true);

            return;
        }

        try {
            fireProgressStateChanged(srcImage.getImageName(), "Equalizing Histogram ...");


            if (!isColorImage) {
                srcImage.exportData(0, length, buffer); // locks and releases lock

                try {
                    fireProgressStateChanged("Processing Image");
                } catch (NullPointerException npe) {

                    if (threadStopped) {
                        Preferences.debug("somehow you managed to cancel the algorithm and dispose the progressbar between checking for threadStopping and using it.",
                                          Preferences.DEBUG_ALGORITHM);
                    }
                }

                if (maxScaleRule == scaleOnImage) {
                    maxScale = (float) (srcImage.getMax() - srcImage.getMin());
                    minValue = (float) srcImage.getMin();
                }

                this.monoSliceEqualizer(buffer, resultBuffer);

                // check for threadstopping BEFORE importing data back into the sourceImage
                if (threadStopped) {
                    finalize();

                    return;
                }

                srcImage.importData(0, resultBuffer, true);
            } else { // if (isColorImage) {

                for (color = 1; (color < valuesPerPixel) && !threadStopped; color++) { // for each color

                    if (((color == 1) && rChannel) || ((color == 2) && gChannel) // process only desired channels
                            || ((color == 3) && bChannel)) { // -- and not alpha channel at all --
                        colorUsed++;
                        fireProgressStateChanged("Processing color " + Integer.toString(color));


                        srcImage.exportRGBData(color, 0, length, buffer); // get the slice

                        if (maxScaleRule == scaleOnImage) {
                            maxScale = getMax(color);
                        }

                        monoSliceEqualizer(buffer, resultBuffer);

                        // check for threadstopping BEFORE importing data back into the sourceImage
                        if (threadStopped) {
                            finalize();

                            return;
                        }

                        // then store the channel back into the destination image
                        srcImage.importRGBData(color, 0, resultBuffer, false);
                    }
                } // for (color = 1; color < valuesPerPixel && !threadStopped; color++)

                srcImage.calcMinMax();
            } // else isColorImage
        } catch (IOException error) {
            errorCleanUp("AlgorithmAHElocal reports: source image locked", false);

            return;
        } catch (OutOfMemoryError e) {
            buffer = null;
            resultBuffer = null;
            errorCleanUp("AlgorithmAHElocal reports: out of memory", true);

            return;
        }

        if (threadStopped) {
            finalize();

            return;
        }

        setCompleted(true);
    }

    /**
     * Histogram Equalization on the source image and replaces the source image with the processed image.
     */
    protected void calcInPlace3D() {
        int i, color;
        int length;

        numberOfSlices = srcImage.getExtents()[2];

        float[] buffer;
        float[] resultBuffer;

        numColors = 0;

        if (rChannel) {
            numColors++;
        }

        if (gChannel) {
            numColors++;
        }

        if (bChannel) {
            numColors++;
        }

        colorUsed = 0;


        length = srcImage.getExtents()[0] * srcImage.getExtents()[1];

        try {
            buffer = new float[length];
            resultBuffer = new float[length];
        } catch (OutOfMemoryError oome) {
            buffer = null;
            resultBuffer = null;
            errorCleanUp("Algorithm AHElocal: Out of memory", true);

            return;
        }

        try {
            fireProgressStateChanged(srcImage.getImageName(), "Equalizing Histogram ...");


            if (!isColorImage) {

                if (maxScaleRule == scaleOnImage) {
                    maxScale = (float) (srcImage.getMax() - srcImage.getMin());
                    minValue = (float) srcImage.getMin();
                }

                // image length is length in 3 dims
                for (i = 0; (i < numberOfSlices) && !threadStopped; i++) {

                    fireProgressStateChanged((int) (((float) (i) / numberOfSlices) * 100));
                    fireProgressStateChanged("Processing slice " + Integer.toString(i + 1));

                    srcImage.exportData(length * i, length, buffer); // locks and releases lock
                    monoSliceEqualizer(buffer, resultBuffer);

                    // check for threadstopping BEFORE importing data back into the sourceImage
                    if (threadStopped) {
                        finalize();

                        return;
                    }

                    // then store the channel back into the destination image
                    srcImage.importData(length * i, resultBuffer, false);
                }
            } else { // if (isColorImage) {

                // color images are handled by performing all slices in the image
                // per color; then operating on the next color.
                for (color = 1; (color < valuesPerPixel) && !threadStopped; color++) { // for each color

                    if (((color == 1) && rChannel) || ((color == 2) && gChannel) // and if it is a desired color
                            || ((color == 3) && bChannel)) { // --and not alpha--  process it

                        if (maxScaleRule == scaleOnImage) {
                            maxScale = getMax(color);
                        }

                        colorUsed++;

                        for (i = 0; (i < numberOfSlices) && !threadStopped; i++) { // get all the slices


                            fireProgressStateChanged((int) (((float) ((Math.max(0, colorUsed - 1) * numberOfSlices) +
                                                                      i) / (numColors * numberOfSlices)) * 100));

                            if ((color == 1) && rChannel) {
                                fireProgressStateChanged("Processing red slice " + Integer.toString(i + 1));
                            } else if ((color == 2) && gChannel) {
                                fireProgressStateChanged("Processing green slice " + Integer.toString(i + 1));
                            } else if ((color == 3) && bChannel) {
                                fireProgressStateChanged("Processing blue slice " + Integer.toString(i + 1));
                            }

                            srcImage.exportRGBData(color, 4 * length * i, length, buffer); // grab the next slice

                            monoSliceEqualizer(buffer, resultBuffer);

                            // check for threadstopping BEFORE importing data back into the sourceImage
                            if (threadStopped) {
                                finalize();

                                return;
                            }

                            srcImage.importRGBData(color, 4 * length * i, resultBuffer, false);
                        } // for (i = 0; i < numberOfSlices && !threadStopped; i++)
                    }
                } // for (color = 1; color < valuesPerPixel && !threadStopped; color++)
            } // else isColorImage

            srcImage.calcMinMax();
        } catch (IOException error) {
            errorCleanUp("Algorithm AHElocal:  image locked", false);

            return;
        } catch (OutOfMemoryError e) {
            buffer = null;
            resultBuffer = null;
            errorCleanUp("Algorithm AHElocal: Out of memory", true);

            return;
        }

        if (threadStopped) {
            finalize();

            return;
        }

        setCompleted(true);
    }

    /**
     * This function produces a new image that has had itself histogram equalized. places filtered image in the
     * destination image.
     */
    protected void calcStoreInDest2D() {
        int color;
        int length; // total number of data-elements (pixels) in image

        numberOfSlices = 1;

        float[] buffer; // data-buffer (for pixel data) which is the "heart" of the image
        float[] resultBuffer; // copy-to buffer (for pixel data) for image-data after filtering

        length = srcImage.getExtents()[0] * srcImage.getExtents()[1];
        
        numColors = 0;

        if (rChannel) {
            numColors++;
        }

        if (gChannel) {
            numColors++;
        }

        if (bChannel) {
            numColors++;
        }

        colorUsed = 0;

        try {
            buffer = new float[length];
            resultBuffer = new float[length];
        } catch (OutOfMemoryError oome) {
            buffer = null;
            resultBuffer = null;
            errorCleanUp("Algorithm AHElocal: Out of memory", true);

            return;
        }
        // destImage.releaseLock(); // we need to be able to alter the dest image

        try {
            fireProgressStateChanged(srcImage.getImageName(), "Equalizing Histogram ...");


            if (!isColorImage) {
                srcImage.exportData(0, length, buffer); // locks and releases lock

                try {
                    fireProgressStateChanged("Processing Image");
                } catch (NullPointerException npe) {

                    if (threadStopped) {
                        Preferences.debug("somehow you managed to cancel the algorithm and dispose the progressbar between checking for threadStopping and using it.",
                                          Preferences.DEBUG_ALGORITHM);
                    }
                }

                if (maxScaleRule == scaleOnImage) {
                    maxScale = (float) (srcImage.getMax() - srcImage.getMin());
                    minValue = (float) srcImage.getMin();
                }

                this.monoSliceEqualizer(buffer, resultBuffer);

                // check for threadstopping BEFORE importing data back into the Image
                if (threadStopped) {
                    finalize();

                    return;
                }

                destImage.importData(0, resultBuffer, true);
            } else { // if (isColorImage) {

                for (color = 0; (color < valuesPerPixel) && !threadStopped; color++) { // for each color

                    fireProgressStateChanged("Processing color " + Integer.toString(color));

                    srcImage.exportRGBData(color, 0, length, buffer); // grab the slice

                    if (((color == 1) && rChannel) || ((color == 2) && gChannel) // process only desired channels
                            || ((color == 3) && bChannel)) { // -- and not alpha channel at all --
                        colorUsed++;
                        if (maxScaleRule == scaleOnImage) {
                            maxScale = getMax(color);
                        }

                        monoSliceEqualizer(buffer, resultBuffer);
                    } else { // don't process this color, just copy it as found
                        setCopyColorText(Integer.toString(color));
                        copyBuffers(resultBuffer, buffer);
                    }

                    // check for threadstopping BEFORE importing data back into the Image
                    if (threadStopped) {
                        finalize();

                        return;
                    }

                    // then store the channel back into the destination image
                    destImage.importRGBData(color, 0, resultBuffer, false);
                }

                destImage.calcMinMax();
            }
        } catch (IOException error) {
            errorCleanUp("Algorithm AHElocal reports: image locked", false);

            return;
        } catch (OutOfMemoryError e) {
            buffer = null;
            resultBuffer = null;
            errorCleanUp("Algorithm AHElocal reports: out of memory", true);

            return;
        }

        if (threadStopped) {
            finalize();

            return;
        }

        setCompleted(true);
    }

    /**
     * This function produces a new volume image that has been histogram equalized. The Image is changed by filtering
     * each slice individually.
     */
    protected void calcStoreInDest3D() {
        int i, color;
        int length;

        numberOfSlices = srcImage.getExtents()[2];

        float[] buffer;
        float[] resultBuffer;
        numColors = 0;

        if (rChannel) {
            numColors++;
        }

        if (gChannel) {
            numColors++;
        }

        if (bChannel) {
            numColors++;
        }

        colorUsed = 0;

        length = srcImage.getExtents()[0] * srcImage.getExtents()[1];

        try {
            buffer = new float[length];
            resultBuffer = new float[length];
        } catch (OutOfMemoryError oome) {
            buffer = null;
            resultBuffer = null;
            errorCleanUp("Algorithm AHElocal: Out of memory", true);

            return;
        }

        try {
            fireProgressStateChanged(srcImage.getImageName(), "Equalizing Histogram ...");


            if (!isColorImage) {

                if (maxScaleRule == scaleOnImage) {
                    maxScale = (float) (srcImage.getMax() - srcImage.getMin());
                    minValue = (float) srcImage.getMin();
                }

                for (i = 0; (i < numberOfSlices) && !threadStopped; i++) { // process for all slices


                    fireProgressStateChanged((int) ((float) (i) / numberOfSlices * 100));
                    fireProgressStateChanged("Processing slice " + Integer.toString(i + 1));

                    srcImage.exportData(length * i, length, buffer); // locks and releases lock
                    monoSliceEqualizer(buffer, resultBuffer);

                    // check for threadstopping BEFORE importing data back into the Image
                    if (threadStopped) {
                        finalize();

                        return;
                    }

                    destImage.importData(length * i, resultBuffer, true);
                }
            } else { // if (isColorImage) {

                for (color = 1; (color < valuesPerPixel) && !threadStopped; color++) {

                    if (((color == 1) && rChannel) || ((color == 2) && gChannel) // process only desired channels
                            || ((color == 3) && bChannel)) {
                        colorUsed++;
                    }

                    if (maxScaleRule == scaleOnImage) {
                        maxScale = getMax(color);
                    }

                    for (i = 0; (i < numberOfSlices) && !threadStopped; i++) { // for all slices at once


                        fireProgressStateChanged((int) (((float) ((Math.max(0, colorUsed - 1) * numberOfSlices) + i) /
                                                             (numColors * numberOfSlices)) * 100));

                        if ((color == 1) && rChannel) {
                            fireProgressStateChanged("Processing red slice " + Integer.toString(i + 1));
                        } else if ((color == 2) && gChannel) {
                            fireProgressStateChanged("Processing green slice " + Integer.toString(i + 1));
                        } else if ((color == 3) && bChannel) {
                            fireProgressStateChanged("Processing blue slice " + Integer.toString(i + 1));
                        }

                        srcImage.exportRGBData(color, 4 * length * i, length, buffer);

                        if (((color == 1) && rChannel) || ((color == 2) && gChannel) // process only desired channels
                                || ((color == 3) && bChannel)) { // -- and not alpha channel at all --
                            monoSliceEqualizer(buffer, resultBuffer);
                        } else { // or don't process this color and just copy it as found
                            copyBuffers(resultBuffer, buffer);
                        }

                        // check for threadstopping BEFORE importing data back into the Image
                        if (threadStopped) {
                            finalize();

                            return;
                        }

                        // but always then store the channel back into the destination image
                        destImage.importRGBData(color, 4 * length * i, resultBuffer, false);
                    }
                }

                destImage.calcMinMax(); // then calculate the min/max for all colors in the image
            }
        } // clean up errors....
        catch (IOException error) {
            errorCleanUp("Algorithm AHElocal: Source image locked", false);

            return;
        } catch (OutOfMemoryError e) {
            buffer = null;
            resultBuffer = null;
            errorCleanUp("Algorithm AHElocal: Out of memory", true);

            return;
        }

        if (threadStopped) {
            finalize();

            return;
        }

        // or the processing completed okay, so dispose of the user notification

        setCompleted(true);
    }


    /**
     * after generating a histogram, <code>clip</code> will reduce the number of pixels attributed to that brightness
     * level.
     *
     * <p>The idea is to limit any brightness level in the histogram to a set value; the number of pixels that are above
     * the limit are tallied. The total number of pixels that were found above the limit, is then spread evenly among
     * the brightnesses that are below limit.</p>
     *
     * @param  histogram  array where the brightness values of all pixels in the image have been counted.
     * @param  clipLimit  no brightness may have any more than this number of pixels. Obviously, this method will not
     *                    achieve anything when <code>clipLimit</code> is as large or larger than the largest brightness
     *                    in <code>histogram</code>.
     */
    protected final void clip(int[] histogram, int clipLimit) {
        int excess = 0;
        int totalExcess = 0;
        int totalCapacity = 0;
        int minChannel = 0;
        int maxChannel = 0;
        int minHist;
        int maxHist;
        int i, j;

        // tally the excess in all bins
        for (i = 0; i < totalBins; i++) {
            excess = histogram[i] - clipLimit;

            if (excess > 0) { // if there were too many of a particular brightness
                totalExcess += excess; // then total-up the excess
            } else if (excess < 0) {
                totalCapacity -= excess;
            }
        }

        if (totalCapacity < totalExcess) {
            totalExcess = totalCapacity;
        }

        for (i = 0; i < totalExcess; i++) {
            maxHist = -Integer.MAX_VALUE;
            minHist = Integer.MAX_VALUE;

            for (j = 0; j < totalBins; j++) {

                if (histogram[j] > maxHist) {
                    maxHist = histogram[j];
                    maxChannel = j;
                }

                if (histogram[j] < minHist) {
                    minHist = histogram[j];
                    minChannel = j;
                }
            }

            histogram[maxChannel] -= 1;
            histogram[minChannel] += 1;
        }

    }

    /**
     * Copies all values from the srcbuffer to the destination buffer. the format is intended to envision the
     * Organization of dest[i] := src[i] float dest destination array float src source array
     *
     * @param  dest  DOCUMENT ME!
     * @param  src   DOCUMENT ME!
     */
    protected final void copyBuffers(float[] dest, float[] src) {
        int i;

        if (dest.length < src.length) {
            return;
        } // cannot copy!!

        for (i = 0; i < src.length; i++) {
            dest[i] = src[i]; // copy buffer into resultBuffer
        }
    }

    /**
     * Finds the local maximum and minimum values in the given range in order, as given by the starting and stoping
     * values.
     *
     * @param  buf       float array of values
     * @param  bufStart  where to begin looking
     * @param  bufEnd    where to stop
     */
    protected final void findBufferMinMax(float[] buf, int bufStart, int bufEnd) {
        int i;

        bufMin = (float) Integer.MAX_VALUE; // preset the min to an absurdly high value
        bufMax = (float) Integer.MIN_VALUE; // preset the max to an absurdly low value

        for (i = bufStart; i < bufEnd; i++) { // then find the min/max in the buffer

            if (buf[i] < bufMin) {
                bufMin = buf[i];
            }

            if (buf[i] > bufMax) {
                bufMax = buf[i];
            }
        }
    }

    /**
     * gets the bottom end of the image. for any image but color images, the bottom end is defined as the buffer minimum
     * for all images except color images; there the offset is defined to be zero
     *
     * @param  type  DOCUMENT ME!
     */
    protected final void findImageOffset(int type) {
        imageOffset = bufMin;

        if (isColorImage) { // color
            imageOffset = 0.0f;
        }

        Preferences.debug("image offset is: " + imageOffset, Preferences.DEBUG_ALGORITHM);

        return;
    }

    /**
     * finds the total number of bins based on the type of image.
     *
     * @param   type   DOCUMENT ME!
     * @param   ideal  DOCUMENT ME!
     *
     * @return  the total number of bins.
     */
    protected final int findTotalBins(int type, int ideal) {
        int totalBins;

        if ((type == ModelStorageBase.UBYTE) || (type == ModelStorageBase.BYTE) || (type == ModelStorageBase.ARGB)) {
            totalBins = Math.min(ideal, 256);
        } else if ((type == ModelStorageBase.FLOAT) || (type == ModelStorageBase.DOUBLE) ||
                       (type == ModelStorageBase.ARGB_FLOAT)) {
            totalBins = ideal;
        } else {
            totalBins = Math.min(ideal, (int) (bufMax - bufMin + 1));
        }

        return totalBins;
    }

    /**
     * gets the image max based on 1st) whether or not the image is color. 2) the color.
     *
     * @param   color  DOCUMENT ME!
     *
     * @return  DOCUMENT ME!
     */
    protected final float getMax(int color) {

        if (!isColorImage) {
            return (float) srcImage.getMax();
        }

        if (color == 0) {
            return 1;
        } else if (color == 1) {
            return (float) srcImage.getMaxR();
        } else if (color == 2) {
            return (float) srcImage.getMaxG();
        } else if (color == 3) {
            return (float) srcImage.getMaxB();
        } else {
            return 0;
        }
    }

    /**
     * gets the image max based on 1st) whether or not the image is color. 2) the color.
     *
     * @param   color  DOCUMENT ME!
     *
     * @return  DOCUMENT ME!
     */
    protected final float getMin(int color) {

        if (!isColorImage) {
            return (float) srcImage.getMin();
        }

        if (color == 0) {
            return 0;
        } else if (color == 1) {
            return (float) srcImage.getMinR();
        } else if (color == 2) {
            return (float) srcImage.getMinG();
        } else if (color == 3) {
            return (float) srcImage.getMinB();
        } else {
            return 0;
        }
    }

    /**
     * Compiles a list of the values neighboring the desired pixel, that are defined in the kernel.
     *
     * <p>The Neighbor list reports the monochromatic brightness values. returned in the neighbor list's kernel.</p>
     *
     * @param  i     The central pixel to find neighbors for.
     * @param  data  Image data. The data is to be arranged always as a monochromatic, 2d set. Returned in kernel: The
     *               neighboring pixel list
     */
    protected final void getNeighborList(int i, float[] data) {
        int row, col;
        int width = srcImage.getExtents()[0]; // width of slice in number of pixels
        int pixelLocation = 0;

        // place all the masked 'on' elements into the data-list
        int count = 0;

        for (row = -halfK; row <= halfK; row++) { // go through all rows

            for (col = -halfK; col <= halfK; col++) { // go through all columns

                if (kernelMask[maskCenter + col + (row * kernelSize)] != 0) {

                    // but don't bother copying into the list if
                    // we don't want that element (the mask pixel is zero)
                    pixelLocation = i + col + (row * width);

                    try {

                        // if the kernel is entirely over the image
                        if ((pixelLocation >= 0) && (pixelLocation < data.length)) {
                            kernel[count++] = data[pixelLocation];
                        } else { // if the kernel overhangs ...
                            kernel[count++] = data[i];
                        }
                    } catch (ArrayIndexOutOfBoundsException aioobe) {
                        Preferences.debug("kernlength: " + kernel.length + ";, count: " + count + "; " +
                                          "in dataset: " + Integer.toString(pixelLocation) + "; datalen: " +
                                          data.length + "\n", Preferences.DEBUG_ALGORITHM);
                        Preferences.debug("\n", Preferences.DEBUG_ALGORITHM);
                    }
                }
            }
        }

        return;
    }

    /**
     * Compiles a list of the values neighboring the desired pixel, that are defined in the kernel.
     *
     * <p>The Neighbor list reports the monochromatic brightness values. returned in the neighbor list's kernel.</p>
     *
     * @param   i     The central pixel to find neighbors for.
     * @param   data  Image data. The data is to be arranged always as a monochromatic, 2d set.
     *
     * @return  aboveThreshold
     */
    protected final boolean getThresholdNeighborList(int i, float[] data) {
        int row, col;
        int width = srcImage.getExtents()[0]; // width of slice in number of pixels
        boolean aboveThreshold = (data[i] >= minThreshold) ? true : false;

        if (!aboveThreshold) {
            return false;
        }
        // won't look for the thresh-hold breaker within the mask

        int pixelLocation = 0;

        // place all the masked 'on' elements into the data-list
        int count = 0;

        for (row = -halfK; row <= halfK; row++) { // go through all rows

            for (col = -halfK; col <= halfK; col++) { // go through all columns

                if (kernelMask[maskCenter + col + (row * kernelSize)] != 0) {

                    // but don't bother copying into the list if
                    // we don't want that element (the mask pixl is zero)
                    pixelLocation = i + col + (row * width);

                    if ((pixelLocation < 0) || (pixelLocation >= data.length)) {

                        // if the kernel overhangs part of the image
                        pixelLocation = i;
                    }

                    try {
                        kernel[count++] = data[pixelLocation];
                    } catch (ArrayIndexOutOfBoundsException aioobe) {
                        Preferences.debug("kernlength: " + kernel.length + ";, count: " + count + "; " +
                                          "in dataset: " + Integer.toString(pixelLocation) + "; datalen: " +
                                          data.length + "\n", Preferences.DEBUG_ALGORITHM);
                        Preferences.debug("\n", Preferences.DEBUG_ALGORITHM);
                    }
                }
            }
        }

        return aboveThreshold;
    }


    /**
     * Makes the kernel. Creates out of memory, sets the center and the size.
     */
    protected void makeKernel() {

        // figure how many kernel elements are actually in the kernel-mask
        int count = 0; // start counting from zero

        for (int m = 0; m < kernelMask.length; m++) {

            if (kernelMask[m] != 0) { // if this element is marked 'on'
                count++;
            }
        }

        kernel = new float[count];

        if (kernelShape == SQUARE_KERNEL) {
            kernelCenter = count / 2;
            maskCenter = halfK * (kernelSize + 1);
        } else if ((kernelShape == CROSS_KERNEL) || (kernelShape == AXIAL_KERNEL)) {
            kernelCenter = count / 2;
            maskCenter = halfK * (kernelSize + 1);
        }
        // mc = count/2 because of the symmetry of the mask.  custom masks may be diff.
        // & i'd like to include custom masks someday....
    }

    /**
     * Forms kernel.
     */
    protected void makeKernelMask() {

        try {
            kernelMask = new byte[kernelSize * kernelSize];
        } catch (OutOfMemoryError e) {
            kernelMask = null;
            errorCleanUp("Algorithm AHElocal reports: not enough memory to form a kernel mask.", true);

            return;
        }

        setKernelMask();
    }


    /**
     * Allows a single monochrome image slice to be filtered. Any color image may be processed in this so long as each
     * color plane is separate and provided one at a time. This means, extract <tt>[aRGB aRGB ...]</tt> buffer into 3
     * seperate buffers <tt>[RRRRRRRRRRR...] , [GGGGGG....] and [BBBBBBBB]</tt> and feed each into the sliceFilter
     * one-at-a-time.
     *
     * <p>The benefit is one sliceFilter for all image types; just break up each RGB image into these seperate
     * monochrome images sliceFilter and then reassemble int aRGB.</p>
     *
     * <p>Only length of one slice (image.getExtents[0] * image.getExtents[1]) Will be copied at a time. Note that a
     * progressBar must be created first.</p>
     *
     * @param  srcBuffer   source buffer
     * @param  destBuffer  destination Buffer
     */
    protected final void monoSliceEqualizer(float[] srcBuffer, float[] destBuffer) {
        int i; // counting....

        int width = srcImage.getExtents()[0]; // width of slice in number of pixels (
        int height = srcImage.getExtents()[1]; // height of slice in number of pixels
        int sliceLength = width * height;
        float maxValue;

        int onePercent = sliceLength / 100; // mod is 1% of total number

        if (maxScaleRule == scaleOnSlice) {
            maxValue = srcBuffer[0];
            minValue = srcBuffer[0];

            for (i = 1; i < sliceLength; i++) {

                if (srcBuffer[i] > maxValue) {
                    maxValue = srcBuffer[i];
                }

                if (srcBuffer[i] < minValue) {
                    minValue = srcBuffer[i];
                }
            }

            if (isColorImage) {
                maxScale = maxValue;
            } else {
                maxScale = maxValue - minValue;
            }
        }

        // for all pixels in the slice:
        for (i = 0; (i < sliceLength) && !threadStopped; i++) {


            // if display is showing, update the progress bar
            if ((numberOfSlices == 1) && ((i % onePercent) == 0)) { // filtering but a single slice
                if (!isColorImage) {
                    fireProgressStateChanged(Math.round(((float) i / sliceLength) * 100));
                }
                else {
                    fireProgressStateChanged((int) (((float) ((Math.max(0, colorUsed - 1) * sliceLength) +
                            i) / (numColors * sliceLength)) * 100));
                }
            }

            // if pixel, i, is to be adjusted (ie., adjusting entire
            // image or this pixel is in the VOImask),
            // then do adjusting
            if (entireImage || mask.get(i)) {

                if (thresholdingIsNecessary) {
                    destBuffer[i] = thresholdProcPixel(i, srcBuffer);
                } else {
                    destBuffer[i] = procPixel(i, srcBuffer);
                }
                // }
            } else { // else, copy pixel data over.
                destBuffer[i] = srcBuffer[i];
            }
        }

        /* if (pBarVisible) {
         * fireProgressStateChanged("Filtering "+ msgString +" (pass "+ String.valueOf(pass+1) +" of "+ iterations +")
         * ..."); if (numberOfSlices > 1) {    // 3D image     update progressBar // do a progress bar update
         * fireProgressStateChanged(Math.round(( ( (float)(currentSlice)/(numberOfSlices))* 100))); } } for ( i = 0; i <
         * imageSliceLength; i++){ if (numberOfSlices == 1) {   // 2D image     update progressBar if (i%mod == 0 &&
         * pBarVisible == true) { fireProgressStateChanged(Math.round ( (  (float)(
         * (pass*imageSliceLength)+i)/(iterations*(imageSliceLength-1))*100)) ); } }
         */
    }

    /**
     * a debug tool.
     *
     * @param  histo  DOCUMENT ME!
     */
    protected final void printhisto(int[] histo) {

        for (int i = 0; i < totalBins; i++) {
            Preferences.debug("histogram[" + i + "] = " + histo[i] + "\n", Preferences.DEBUG_ALGORITHM);
        }
    }

    /**
     * process pixel at pixel <i>pix</i> using data from the <i>srcBuffer</i>. builds a list of monochromatic image
     * values of the neighboring pixels image values (using a map generated by the kernelMask), builds the histogram of
     * those values, clamps when necessary, then scales the cumulative histogram, getting the brightness back out.
     *
     * <p>This is the entire algorithm on one pixel.</p>
     *
     * @param   pix        the pixel to process; generates the histogram from the neighbors around this image-data
     *                     element.
     * @param   srcBuffer  all the monochromatic image data -- which means that 4-color image data must be seperated out
     *                     into monochrome color slices for this method.
     *
     * @return  DOCUMENT ME!
     */
    protected final float procPixel(int pix, float[] srcBuffer) {
        int maxBin = 0; // index to the bin with the highest count.
        float scale; // normalization value of max over the number of histogram partitions.

        getNeighborList(pix, srcBuffer);
        maxBin = buildHistogram(); // drop pixel into histo and find max of all values in histo

        if (clampingIsNecessary) {
            clip(histogram, (int) (clipLevel * histogram[maxBin])); // clamp if necessary
        }

        scale = (float) (maxScale) / (kernel.length); // largest scale value over the number of positions to fill
                                                      // (compare to makeScaled(...) after makeCumulative(..)
        histomap[0] = (float) histogram[0] * scale; // scale histogram at brightness 0

        for (int bin = 1; bin < totalBins; bin++) {
            histogram[bin] += histogram[bin - 1]; // make cumulative
            histomap[bin] = (float) histogram[bin] * scale; // makeScaled(histogram);//scale whole histogram
        }

        // find the brightness level of the center pixel
        // and place into dest[center];
        brightnessLevel = (int) ((srcBuffer[pix] - imageOffset) / (unitSize));

        try { // if (brightnessLevel < totalBins) {

            if (isColorImage) {
                return histomap[brightnessLevel];
            } else {
                return (histomap[brightnessLevel] + minValue);
            }
        } catch (ArrayIndexOutOfBoundsException exc) {
            return srcBuffer[pix];
        }

    }

    /**
     * Fill in the mask for which pixels are used in filtering.
     */
    protected void setKernelMask() {
        int i;

        halfK = kernelSize / 2; // set class var, half-kernel

        // square/box
        if (kernelShape == SQUARE_KERNEL) {

            for (i = 0; i < kernelMask.length; i++) {
                kernelMask[i] = 1;
            }
        } // cross/axial
        else if ((kernelShape == CROSS_KERNEL) || (kernelShape == AXIAL_KERNEL)) {
            int row; // indicates current row
            int col; // indicates current column

            for (i = 0; i < kernelMask.length; i++) {
                row = i / kernelSize;
                col = i % kernelSize;

                if (col == halfK) {
                    kernelMask[i] = 1;
                } else if (row == halfK) {
                    kernelMask[i] = 1;
                } else {
                    kernelMask[i] = 0;
                }
            }
        }
    }

    /**
     * process pixel at pixel <i>pix</i> using data from the <i>srcBuffer</i>. builds a list of monochromatic image
     * values of the neighboring pixels image values (using a map generated by the kernelMask), builds the histogram of
     * those values, clamps when necessary, then scales the cumulative histogram, getting the brightness back out.
     *
     * <p>This is the entire algorithm on one pixel.</p>
     *
     * @param   pix        the pixel to process; generates the histogram from the neighbors around this image-data
     *                     element.
     * @param   srcBuffer  all the monochromatic image data -- which means that 4-color image data must be separated out
     *                     into monochrome color slices for this method.
     *
     * @return  DOCUMENT ME!
     */
    protected final float thresholdProcPixel(int pix, float[] srcBuffer) {
        int maxBin = 0; // index to the bin with the highest count.
        float scale; // normalization value of max over the number of histogram partitions.

        if (getThresholdNeighborList(pix, srcBuffer)) { // get neighbor list

            // finish the processing pixel.  all further steps are same as procPixel()
            maxBin = buildHistogram(); // drop pixel into histo and find max of all values in histo

            if (clampingIsNecessary) {
                clip(histogram, (int) (clipLevel * histogram[maxBin])); // clamp if necessary
            }

            scale = (float) (maxScale) / (kernel.length); // largest scale value over the number of positions to fill
                                                          // (compare to makeScaled(...) after makeCumulative(..)
            histomap[0] = (float) histogram[0] * scale; // scale histogram at brightness 0

            for (int bin = 1; bin < totalBins; bin++) {
                histogram[bin] += histogram[bin - 1]; // make cumulative
                histomap[bin] = (float) histogram[bin] * scale; // makeScaled(histogram);//scale whole histogram
            }

            // find the brightness level of the center pixel
            // and place into dest[center];
            brightnessLevel = (int) ((srcBuffer[pix] - imageOffset) / (unitSize));

            try { // if (brightnessLevel < totalBins) {

                if (isColorImage) {
                    return histomap[brightnessLevel];
                } else {
                    return (histomap[brightnessLevel] + minValue);
                }
            } catch (ArrayIndexOutOfBoundsException exc) {
                return srcBuffer[pix];
            }
        } else {
            return srcBuffer[pix];
        }
    }


    /**
     * If the progress bar is visible, sets the text to:<br>
     * <tt>Copying all <i>color</i> values ...</tt>
     *
     * @param  colorText  the color to use. Eg., "red" or "blue"
     */
    private void setCopyColorText(String colorText) {


        fireProgressStateChanged("Copying all " + colorText + " values ... ");

    }

}
