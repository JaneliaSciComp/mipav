package gov.nih.mipav.model.algorithms;


import gov.nih.mipav.model.structures.*;

import gov.nih.mipav.view.*;

import java.io.*;
import java.util.Arrays;


/**
 * Algorithm that matches the transforms a match or source image so as to make its histogram equal to the histogram of a
 * base image. In order to easily note the great similarity of the histograms, the histogram displays must both be in
 * linear mode rather than log mode so that small differences are not exaggerated. The algorithm is as follows: 1.) Make
 * the histogram for the match image. 2.) Make the histogram cumulative. 3.) Create the scaled cumulative histogram
 * mapping scaled so that the last bin of mapping has the range of the source image. 4.) For the base image create a
 * histogram with the same number of bins as the match image. 5.) Make the histogram cumulative. 6.) Create the scaled
 * cumulative histogram cumHist scaled so that the last bin of cumHist has the range of the source image. 7.) For each
 * value of mapping[i] find the cumHist[j] that is closest in value. 8.) In remap[i] put the value of srcImage
 * corresponding to the jth bin. 9.) For each srcBuffer[i] find the bin number brightnessLevel and replace the
 * srcBuffer[i] with the remap[brightnessLevel]. References: 1.) Digital Image Processing by Rafael C. Gonzalez and
 * Richard E. Woods, Addison-Wesley Publishing Company, 1992, pp. 173-182. 2.) Two-Dimensional Signal and Image
 * Processing by Jae S. Lim, Prentice-Hall, Inc., 1990, pp. 453-459.
 *
 * <p>According to Freedman and Diaconis as summarized in "Recent Developments in NonParametric Density Estimation" by
 * Alan Julian Izenman, Journal of the American Statistical Association, March, 1991, Vol. 86, No. 413, pp. 205 - 224:
 * The ideal histogram bin width W is given by W = 2(IQR)pow(N,-1/3) where IQR is the inrterquartile range(the 75
 * percentile minus the 25th percentile) and N is the number of available samples.</p>
 */
public class AlgorithmHistogramMatch extends AlgorithmBase {

    //~ Instance fields ------------------------------------------------------------------------------------------------

    /** DOCUMENT ME! */
    private ModelImage baseImage;

    /** DOCUMENT ME! */
    private boolean bChannel = true; // the blue channel

    /** DOCUMENT ME! */
    private float bufMax;

    /** DOCUMENT ME! */
    private float bufMin;

    /** DOCUMENT ME! */
    private boolean gChannel = true; // the green channel

    /** DOCUMENT ME! */
    private boolean isColorImage = false; // indicates the image being messed with is a colour image

    /** DOCUMENT ME! */
    private boolean rChannel = true; // if T, filter the red channel

    /** DOCUMENT ME! */
    private int valuesPerPixel = 1; // number of elements in a pixel.  Monochrome = 1, Color = 4. (a, R, G, B)

    //~ Constructors ---------------------------------------------------------------------------------------------------

    /**
     * Constructor for 3D images in which changes are returned to the source image.
     *
     * @param  srcImg     source image model
     * @param  baseImage  image whose histogram serves as a model
     */
    public AlgorithmHistogramMatch(ModelImage srcImg, ModelImage baseImage) {
        super(null, srcImg);
        this.baseImage = baseImage;

        if (srcImg.isColorImage()) {
            isColorImage = true;
            valuesPerPixel = 4;
        }
    }

    /**
     * Constructor for 3D images in which changes are placed in a predetermined destination image.
     *
     * @param  destImg    image model where result image is to stored
     * @param  srcImg     source image model
     * @param  baseImage  image whose histogram serves as a model
     */
    public AlgorithmHistogramMatch(ModelImage destImg, ModelImage srcImg, ModelImage baseImage) {
        super(destImg, srcImg);
        this.baseImage = baseImage;

        if (srcImg.isColorImage()) {
            isColorImage = true;
            valuesPerPixel = 4;
        }
    }

    //~ Methods --------------------------------------------------------------------------------------------------------

    /**
     * Prepares this class for destruction.
     */
    public void finalize() {
        baseImage = null;
        destImage = null;
        srcImage = null;
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

        

        // destinationImage being null sets the algo to
        // put replace the source image with the
        // resultant image.
        if (destImage != null) { // if there exists a destination image
            calcStoreInDest();
        } else { // the destination is the original source.
            calcInPlace();
        }
    }

    /**
     * Histogram matching of the source image to the base image. Replaces the original image with the filtered image.
     */
    private void calcInPlace() {
        int colour;
        int length;
        int baseLength;
        int i;

        float[] buffer;
        float[] baseBuffer;

        length = srcImage.getExtents()[0];

        for (i = 1; i < srcImage.getNDims(); i++) {
            length *= srcImage.getExtents()[i];
        }

        baseLength = baseImage.getExtents()[0];

        for (i = 1; i < baseImage.getNDims(); i++) {
            baseLength *= baseImage.getExtents()[i];
        }

        try {
            buffer = new float[length];
            baseBuffer = new float[baseLength];
        } catch (OutOfMemoryError oome) {
            buffer = null;
            baseBuffer = null;
            errorCleanUp("Algorithm Histogram Match reports: out of memory", true);

            return;
        }

        try {
            fireProgressStateChanged(srcImage.getImageName(), "Matching Histogram ...");
            

            if (!isColorImage) {
                srcImage.exportData(0, length, buffer); // locks and releases lock
                baseImage.exportData(0, baseLength, baseBuffer);

                try {
                    fireProgressStateChanged("Processing Image");
                    fireProgressStateChanged(45); // not quite midway
                } catch (NullPointerException npe) {

                    if (threadStopped) {
                        Preferences.debug("somehow you managed to cancel the algorithm and dispose the progressbar between checking for threadStopping and using it.",
                                          Preferences.DEBUG_ALGORITHM);
                    }
                }

                this.filter(buffer, baseBuffer);

                if (threadStopped) { // do BEFORE buffer has been exported to Image
                    finalize();

                    return;
                }

                srcImage.importData(0, buffer, true);
            } else { // if (isColorImage) {

                for (colour = 0; (colour < valuesPerPixel) && !threadStopped; colour++) { // for each color

                 
                	fireProgressStateChanged((int) (((float) (colour) / valuesPerPixel) * 100));
                	fireProgressStateChanged("Processing colour " + Integer.toString(colour));
             

                    srcImage.exportRGBData(colour, 0, length, buffer); // get the slice
                    baseImage.exportRGBData(colour, 0, baseLength, baseBuffer);

                    if (((colour == 1) && rChannel) || ((colour == 2) && gChannel) // process only desired channels
                            || ((colour == 3) && bChannel)) { // -- and not alpha channel at all --
                        filter(buffer, baseBuffer);
                    }

                    if (threadStopped) { // do BEFORE buffer has been exported to Image
                        finalize();

                        return;
                    }

                    // then store the channel back into the destination image
                    srcImage.importRGBData(colour, 0, buffer, false);
                }

                srcImage.calcMinMax();
            }
        } catch (IOException error) {
            errorCleanUp("AlgorithmHistogramMatch reports: source image locked", false);

            return;
        } catch (OutOfMemoryError e) {
            buffer = null;
            baseBuffer = null;
            errorCleanUp("AlgorithmHistogramMatch reports: out of memory", true);

            return;
        }

        if (threadStopped) {
            finalize();

            return;
        }

        setCompleted(true);
    }

    /**
     * This function produces a new image that has had itself histogram matched to the base image. places filtered image
     * in the destination image.
     */
    private void calcStoreInDest() {
        int colour;
        int length;
        int baseLength;
        int i;

        float[] buffer;
        float[] baseBuffer;

        length = srcImage.getExtents()[0];

        for (i = 1; i < srcImage.getNDims(); i++) {
            length *= srcImage.getExtents()[i];
        }

        baseLength = baseImage.getExtents()[0];

        for (i = 1; i < baseImage.getNDims(); i++) {
            baseLength *= baseImage.getExtents()[i];
        }

        try {
            buffer = new float[length];
            baseBuffer = new float[baseLength];
        } catch (OutOfMemoryError oome) {
            buffer = null;
            baseBuffer = null;
            errorCleanUp("Algorithm Histogram match: Out of memory", true);

            return;
        }

        destImage.releaseLock(); // we need to be able to alter the dest image

        try {
            fireProgressStateChanged(srcImage.getImageName(), "Matching Histogram ...");
            

            if (!isColorImage) {
                srcImage.exportData(0, length, buffer); // locks and releases lock
                baseImage.exportData(0, baseLength, baseBuffer);

                try {
                    fireProgressStateChanged("Processing Image");
                    fireProgressStateChanged(45); // a little less than midway
                } catch (NullPointerException npe) {

                    if (threadStopped) {
                        Preferences.debug("somehow you managed to cancel the algorithm and dispose the progressbar between checking for threadStopping and using it.",
                                          Preferences.DEBUG_ALGORITHM);
                    }
                }

                this.filter(buffer, baseBuffer);

                if (threadStopped) { // do before copying back into image
                    finalize();

                    return;
                }

                destImage.importData(0, buffer, true);
            } else { // if (isColorImage) {

                for (colour = 0; (colour < valuesPerPixel) && !threadStopped; colour++) { // for each color

                
                	fireProgressStateChanged((int) (((float) (colour) / valuesPerPixel) * 100));
                	fireProgressStateChanged("Processing colour " + Integer.toString(colour));
           
                    srcImage.exportRGBData(colour, 0, length, buffer); // grab the slice
                    baseImage.exportRGBData(colour, 0, baseLength, baseBuffer);

                    if (((colour == 1) && rChannel) || ((colour == 2) && gChannel) // process only desired channels
                            || ((colour == 3) && bChannel)) { // -- and not alpha channel at all --
                        filter(buffer, baseBuffer);
                    }

                    if (threadStopped) { // do before copying back into image
                        finalize();

                        return;
                    }

                    // then store the channel back into the destination image
                    destImage.importRGBData(colour, 0, buffer, false);
                }

                destImage.calcMinMax();
            }
        } catch (IOException error) {
            displayError("Algorithm Histogram Match reports: image locked");
            setCompleted(false);

            return;
        } catch (OutOfMemoryError e) {
            buffer = null;
            baseBuffer = null;
            displayError("Algorithm Histogram Match reports: out of memory");
            setCompleted(false);

            return;
        }

        if (threadStopped) {
            finalize();

            return;
        }

        
        setCompleted(true);
    }

    /**
     * just break up each RGB image into these separate monochrome images sliceFilter and then reassemble int aRGB.
     *
     * @param  srcBuffer   source buffer
     * @param  baseBuffer  base Buffer
     */
    private void filter(float[] srcBuffer, float[] baseBuffer) {

        int i, j, jMin;
        float diff;
        float baseOffset;

        int bin, // counter-var
            totalBins; // number of brightness-values in img.  also, sizeof(histogram)
        int brightnessLevel; // a pixel (no matter whether represented as a float or int), has a brightnessLevel,

        // which is in the histogram as a value between 0 and totalBins.
        int[] hist;
        float[] cumHist; // scaled cumulative histogram for base image.
        float[] remap; // contains the value of srcImage corresponding to the

        // cumHist[j] closest to the mapping[i]
        float unitSize; // one unit is one totalBin-th of the distance between max

        // & min brightness in srcImage.
        float baseUnitSize; // one unit is one totalBin-th of the distance between max

        // & min brightness in baseImage.
        float[] mapping; // scaled cumulative histogram for source image.
        float scale; // scale factor that scales the value in the last bin of a cumulative

        // histogram to the maximum value of the source image.
        float range; // range of the source image
        float srcMin;
        float idealWidth = getIdealWidth(srcBuffer);
        int idealBins;

        findBufferMinMax(srcBuffer, 0, srcBuffer.length); // calculates largest and smallest pixel values
        srcMin = bufMin;
        if (idealWidth >= ((bufMax - bufMin)/1024.0)) {
            idealBins = (int) (((bufMax - bufMin) / idealWidth) + 0.5f);
        }
        else {
        	idealBins = 256;
        }
        Preferences.debug("idealBins = " + idealBins + "\n", Preferences.DEBUG_ALGORITHM);

        int type = srcImage.getType();

        if ((type == ModelStorageBase.UBYTE) || (type == ModelStorageBase.BYTE) || (type == ModelStorageBase.ARGB)) {
            totalBins = Math.min(idealBins, 256);
        } else if ((type != ModelStorageBase.FLOAT) && (type != ModelStorageBase.DOUBLE) &&
                       (type != ModelStorageBase.ARGB_FLOAT)) {
            totalBins = Math.min(idealBins, (int) (bufMax - bufMin + 1));
        } else {
            totalBins = idealBins;
        }

        unitSize = (bufMax - bufMin) / (totalBins - 1); // one unit is the distance of one totalBin-th in brightness

        range = bufMax - bufMin;

        mapping = new float[totalBins];
        hist = new int[totalBins]; // hist is both normal histogram & cumulative histogram

        // calculate the cumulative histogram of the source image
        // clear histogram
        for (bin = 0; bin < totalBins; bin++) {
            hist[bin] = 0;
        }

        // for each pixel in the source image,
        // find the appropriate brightness and account for it
        // in the histogram.
        for (i = 0; i < srcBuffer.length; i++) {

            try {
                brightnessLevel = (int) ((srcBuffer[i] - bufMin) / (unitSize));
                hist[brightnessLevel]++;
            } catch (ArrayIndexOutOfBoundsException aioobe) {
                Preferences.debug("<HALT>\n", Preferences.DEBUG_ALGORITHM);
                Preferences.debug("i= " + i + "\n", Preferences.DEBUG_ALGORITHM);
                Preferences.debug("srcbuffer: " + srcBuffer[i] + "; bufMin: " + bufMin + "; unitSize: " + unitSize +
                                  "\n", Preferences.DEBUG_ALGORITHM);
                Preferences.debug("at hist: " + ((int) (srcBuffer[i] - bufMin) / unitSize) + "\n", 
                		Preferences.DEBUG_ALGORITHM);
                Preferences.debug("totalbins: " + totalBins + "\n", Preferences.DEBUG_ALGORITHM);
                Preferences.debug("</HALT>\n", Preferences.DEBUG_ALGORITHM);
                MipavUtil.displayError("Algorithm Histogram match reports: out of bounds");
                setCompleted(false);

                return;
            }
        }

        // make histogram cumulative
        for (bin = 1; bin < totalBins; bin++) {
            hist[bin] += hist[bin - 1];
        }

        // mapping is a scaled version of the source image cumulative histogram
        // Scaled so that the range of the source image appears in the last bin
        scale = range / (hist[totalBins - 1]);

        for (bin = 0; bin < totalBins; bin++) {
            mapping[bin] = hist[bin] * scale;
        }

        findBufferMinMax(baseBuffer, 0, baseBuffer.length); // calculates largest and smallest pixel values
        baseOffset = bufMin;

        // Use same totalBins as for srcImage
        baseUnitSize = ((bufMax - bufMin) / (totalBins-1)); // one unit is the distance of one totalBin-th in brightness

        cumHist = new float[totalBins];

        // calculate the cumulative histogram of the base image
        // clear histogram
        for (bin = 0; bin < totalBins; bin++) {
            hist[bin] = 0;
        }

        // for each pixel in the base image,
        // find the appropriate brightness and account for it
        // in the histogram.
        for (i = 0; i < baseBuffer.length; i++) {

            try {
                brightnessLevel = (int) ((baseBuffer[i] - baseOffset) / (baseUnitSize));
                hist[brightnessLevel]++;
            } catch (ArrayIndexOutOfBoundsException aioobe) {
                Preferences.debug("<HALT>\n", Preferences.DEBUG_ALGORITHM);
                Preferences.debug("i= " + i + "\n", Preferences.DEBUG_ALGORITHM);
                Preferences.debug("basebuffer: " + baseBuffer[i] + "; offset: " + baseOffset + ";baseUnitSize: " +
                                  baseUnitSize + "\n", Preferences.DEBUG_ALGORITHM);
                Preferences.debug("at hist: " + ((int) (baseBuffer[i] - baseOffset) / baseUnitSize) + "\n", 
                		Preferences.DEBUG_ALGORITHM);
                Preferences.debug("totalbins: " + totalBins + "\n", Preferences.DEBUG_ALGORITHM);
                Preferences.debug("</HALT>\n", Preferences.DEBUG_ALGORITHM);
                MipavUtil.displayError("Algorithm Histogram match reports: out of bounds");
                setCompleted(false);

                return;
            }
        }

        // make histogram cumulative
        for (bin = 1; bin < totalBins; bin++) {
            hist[bin] += hist[bin - 1];
        }

        // cumHist is a scaled image of the base image cumulative histogram
        // Scaled so that the range of the source image appears in the last bin
        scale = range / (hist[totalBins - 1]);

        for (bin = 0; bin < totalBins; bin++) {
            cumHist[bin] = hist[bin] * scale;
        }

        // Since both histograms never decrease j can never decrease
        // Find the cumHist[j] closest in value to mapping[i]
        // and place the value of srcImage corresponding to the jth bin
        // in remap[i].
        jMin = 0;
        remap = new float[totalBins];

        for (i = 0; i < totalBins; i++) {
            diff = Float.MAX_VALUE;

            for (j = jMin; j < totalBins; j++) {

                if ((Math.abs(mapping[i] - cumHist[j])) < diff) {
                    diff = Math.abs(mapping[i] - cumHist[j]);
                    remap[i] = srcMin + (j * unitSize);
                    jMin = j;
                }
            }
        }

        // Find the bin # brightnessLevel for every pixel
        // and replace srcBuffer[i]
        for (i = 0; i < srcBuffer.length; i++) {
            brightnessLevel = (int) ((srcBuffer[i] - srcMin) / (unitSize));

            try {
                srcBuffer[i] = remap[brightnessLevel];
            } catch (ArrayIndexOutOfBoundsException exc) { }
        }

    }

    /** 
     * Calculate idealWidth for bins.
     */
    private float getIdealWidth(float[] srcBuffer) {
        float[] sortBuffer = new float[srcBuffer.length];

        for (int i = 0; i < srcBuffer.length; i++) {
            sortBuffer[i] = srcBuffer[i];
        }

        Arrays.sort(sortBuffer);
        return (float) (2.0f *(sortBuffer[(3 * srcBuffer.length / 4) - 1] -
                                       sortBuffer[(srcBuffer.length / 4) - 1]) *
                                  Math.pow(srcBuffer.length, -1.0 / 3.0));
	}

	/**
     * Finds the local maximum and minimum values in the given range in order, as given by the starting and stoping
     * values.
     *
     * @param  buf       float array of values
     * @param  bufStart  where to begin looking
     * @param  bufEnd    where to stop
     */
    private void findBufferMinMax(float[] buf, int bufStart, int bufEnd) {
        int i;

        bufMin = Float.MAX_VALUE; // preset the min to an absurdly high value
        bufMax = -Float.MAX_VALUE; // preset the max to an absurdly low value

        for (i = bufStart; i < bufEnd; i++) { // then find the min/max in the buffer

            if (buf[i] < bufMin) {
                bufMin = buf[i];
            }

            if (buf[i] > bufMax) {
                bufMax = buf[i];
            }
        }
    }

}
