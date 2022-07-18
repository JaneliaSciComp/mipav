import gov.nih.mipav.model.algorithms.*;
import gov.nih.mipav.model.structures.*;

import gov.nih.mipav.view.*;

import java.awt.*;

import java.io.*;

import java.util.*;


/**
 * Description: Segmentation algorithm for color images that requires user input to "help" the algorithm find "greenish"
 * areas (punctate) by having the user show what types of colors/hues should be excluded and included.
 */
public class PlugInAlgorithmNEISeg extends AlgorithmBase {

    //~ Instance fields ------------------------------------------------------------------------------------------------

    /** DOCUMENT ME! */
    private ModelImage destImageHSB_Hue = null;

    /** DOCUMENT ME! */
    private ModelImage destImageRGB = null;

    /** DOCUMENT ME! */
    private ModelImage destImageRGB_AllRatios = null;

    /** DOCUMENT ME! */
    private float excludeMaxH;

    /** DOCUMENT ME! */
    private float[] excludeMeanB;

    /** DOCUMENT ME! */
    private float[] excludeMeanBlue;

    /** DOCUMENT ME! */
    private float[] excludeMeanGreen;

    /** DOCUMENT ME! */
    private float[] excludeMeanH;

    /** DOCUMENT ME! */
    private float[] excludeMeanRed;

    /** DOCUMENT ME! */
    private float[] excludeMeanS;

    /** DOCUMENT ME! */
    private float excludeMinH;

    /** DOCUMENT ME! */
    private float excludeRGRatio = 0f;

    /** DOCUMENT ME! */
    private ModelImage finalImage = null;

    /** DOCUMENT ME! */
    private float includeMaxH;

    /** DOCUMENT ME! */
    private float[] includeMeanB;

    /** DOCUMENT ME! */
    private float[] includeMeanBlue;

    /** DOCUMENT ME! */
    private float[] includeMeanGreen;

    /** DOCUMENT ME! */
    private float[] includeMeanH;

    /** DOCUMENT ME! */
    private float[] includeMeanRed;

    /** DOCUMENT ME! */
    private float[] includeMeanS;

    /** DOCUMENT ME! */
    private float includeMinH;

    /** DOCUMENT ME! */
    private float includeRGRatio = 0f;

    /** DOCUMENT ME! */
    private int numExclude = 0;

    /** DOCUMENT ME! */
    private int numInclude = 0;

    /** DOCUMENT ME! */
    private long[] numPixels = { 0, 0, 0, 0 };

    //~ Constructors ---------------------------------------------------------------------------------------------------

    /**
     * Construct the algorithm.
     *
     * @param  destImg_AllRatios  DOCUMENT ME!
     * @param  destImg_Hue        DOCUMENT ME!
     * @param  destImg_RGB        DOCUMENT ME!
     * @param  finalImage         DOCUMENT ME!
     * @param  srcImg             DOCUMENT ME!
     */
    public PlugInAlgorithmNEISeg(ModelImage destImg_AllRatios, ModelImage destImg_Hue, ModelImage destImg_RGB,
                                 ModelImage finalImage, ModelImage srcImg) {
        this.destImageRGB_AllRatios = destImg_AllRatios;
        this.destImageHSB_Hue = destImg_Hue;
        this.destImageRGB = destImg_RGB;
        this.finalImage = finalImage;
        this.srcImage = srcImg;
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
     * Starts the program.
     */
    public void runAlgorithm() {

        if ((srcImage == null) && (destImage == null)) {
            displayError("Eye Segmentation.run(): Source  and/or Destination image is null");

            return;
        }

        if (srcImage.isColorImage() == false) {
            displayError("Eye Segmentation.run(): Source Image is not a RGB type");

            return;
        }

        
        calcStoreInDest();
    }

    /**
     * Checks each pixel within the selected VOI Region and runs several different functions for include/exclude to see
     * if pixels should be included or excluded for each image type created Currently, there are 4 images created...
     * each ARGB, for 4 different checks...
     */
    private void calcStoreInDest() {
        int i;
        int id;
        int lengthIn; // total number of data-elements (pixels) in image
        float[] buffer; // data-buffer (for pixel data) which is the "heart" of the image\
        float[] bufferDest_RGB_AllRatios;
        float[] bufferDest_RGB;
        float[] bufferDest_HSB_Hue;
        float[] bufferDest_final;
        fireProgressStateChanged("Calculating VOI Means...");


        int xDim = srcImage.getExtents()[0];

        try {
            lengthIn = 4 * srcImage.getSliceSize();
            buffer = new float[lengthIn];
            bufferDest_RGB = new float[lengthIn];
            bufferDest_RGB_AllRatios = new float[lengthIn];
            bufferDest_HSB_Hue = new float[lengthIn];
            bufferDest_final = new float[lengthIn];
        } catch (OutOfMemoryError e) {
            buffer = null;
            System.gc();
            displayError("Algorithm Eye Segmentation reports: Out of memory when creating image buffer");
            setCompleted(false);

            return;
        }

        int mod = lengthIn / 20;

        try {
            srcImage.exportData(0, lengthIn, buffer); // locks and releases lock
        } catch (IOException error) {
            buffer = null;
            displayError("Algorithm Eye Segmenation : Input Image(s) locked");
            setCompleted(false);

            return;
        }

        calculateVOIMeans(buffer);

        // progressBar.setValue(70);
        float avgRed = 0f, avgGreen = 0f, avgBlue = 0f;
        float avgHue = 0f, avgSaturation = 0f, avgBrightness = 0f;
        int fxDim = 4 * xDim;
        fireProgressStateChanged("Segmenting image...");
        fireProgressStateChanged(70);

        float[] hsb = new float[3];
        boolean includeRGB;
        boolean includeHSB;
        boolean includeRGB_AllRatios;

        for (i = 0, id = 0; i < lengthIn; i += 4, id += 4) {

            if (((i % mod) == 0)) {
                fireProgressStateChanged(Math.round((float) (i) / (lengthIn) * 40) + 70);
            }

            avgHue = 0f;
            avgSaturation = 0f;
            avgBrightness = 0f;
            avgRed = 0f;
            avgGreen = 0f;
            avgBlue = 0f;
            includeRGB = false;
            includeHSB = false;
            includeRGB_AllRatios = false;

            try {

                // calculate the average red, green, and blue pixels in the 4 pixels surrounding the pixel
                avgRed = (buffer[i + 1] + buffer[i + 1 - fxDim] + buffer[i + 1 + fxDim] + buffer[i + 1 - 4] +
                          buffer[i + 1 + 4]) / 9.0f;
                avgGreen = (buffer[i + 2] + buffer[i + 2 - fxDim] + buffer[i + 2 + fxDim] + buffer[i + 2 - 4] +
                            buffer[i + 2 + 4]) / 9.0f;
                avgBlue = (buffer[i + 3] + buffer[i + 3 - fxDim] + buffer[i + 3 + fxDim] + buffer[i + 3 - 4] +
                           buffer[i + 3 + 4]) / 9.0f;

                if ((avgRed / avgGreen) < excludeRGRatio) {
                    bufferDest_RGB[id + 1] = 20;
                    bufferDest_RGB[id + 2] = 150;
                    bufferDest_RGB[id + 3] = 100;
                    includeRGB = true;
                    numPixels[0]++;
                }

                if (includePointRGB_AllRatios(avgRed, avgGreen, avgBlue)) {
                    bufferDest_RGB_AllRatios[id + 1] = 10;
                    bufferDest_RGB_AllRatios[id + 2] = 120;
                    bufferDest_RGB_AllRatios[id + 3] = 40;
                    includeRGB_AllRatios = true;
                    numPixels[1]++;
                }

                // calculate the average Hue, Saturation, and Brightness in a 9 pixel region
                Color.RGBtoHSB((int) buffer[i + 1 - fxDim], (int) buffer[i + 2 - fxDim], (int) buffer[i + 3 - fxDim],
                               hsb);
                avgHue += hsb[0];
                avgSaturation += hsb[1];
                avgBrightness += hsb[2];
                Color.RGBtoHSB((int) buffer[i + 1 + fxDim], (int) buffer[i + 2 + fxDim], (int) buffer[i + 3 + fxDim],
                               hsb);
                avgHue += hsb[0];
                avgSaturation += hsb[1];
                avgBrightness += hsb[2];
                Color.RGBtoHSB((int) buffer[i + 1 + 4], (int) buffer[i + 2 + 4], (int) buffer[i + 3 + 4], hsb);
                avgHue += hsb[0];
                avgSaturation += hsb[1];
                avgBrightness += hsb[2];
                Color.RGBtoHSB((int) buffer[i + 1 - 4], (int) buffer[i + 2 - 4], (int) buffer[i + 3 - 4], hsb);
                avgHue += hsb[0];
                avgSaturation += hsb[1];
                avgBrightness += hsb[2];
                Color.RGBtoHSB((int) buffer[i + 1], (int) buffer[i + 2], (int) buffer[i + 3], hsb);
                avgHue += hsb[0];
                avgSaturation += hsb[1];
                avgBrightness += hsb[2];
                avgHue /= 5.0f;
                avgSaturation /= 5.0f;
                avgBrightness /= 5.0f;

                if (includePointHSB_Hue(avgHue, avgSaturation, avgBrightness)) {
                    bufferDest_HSB_Hue[id + 1] = 140;
                    bufferDest_HSB_Hue[id + 2] = 200;
                    bufferDest_HSB_Hue[id + 3] = 100;
                    includeHSB = true;
                    numPixels[2]++;
                }

                // if at least two of the 3 algorithms (RGB, RGB All Ratios, and HSB)
                // return true, add this point into a final image
                if ((includeRGB && includeHSB) || (includeRGB && includeRGB_AllRatios) ||
                        (includeHSB && includeRGB_AllRatios)) {
                    bufferDest_final[id + 1] = 0;
                    bufferDest_final[id + 2] = 150;
                    bufferDest_final[id + 3] = 0;
                    numPixels[3]++;
                }
            } catch (IndexOutOfBoundsException ex) {
                // skip this pixel
            }
        }

        fireProgressStateChanged(100);

        if (threadStopped) {
            buffer = null;
            bufferDest_RGB = null;
            bufferDest_RGB_AllRatios = null;
            bufferDest_HSB_Hue = null;
            bufferDest_final = null;
            finalize();

            return;
        }

        try {
            destImageRGB.importData(0, bufferDest_RGB, true);
            destImageRGB_AllRatios.importData(0, bufferDest_RGB_AllRatios, true);
            destImageHSB_Hue.importData(0, bufferDest_HSB_Hue, true);
            finalImage.importData(0, bufferDest_final, true);
        } catch (IOException error) {
            displayError("Algorithm Eye Segmentation: Output Image(s) locked");
            setCompleted(false);


            return;
        }

        ViewUserInterface.getReference().setDataText(" Image name: " + srcImage.getImageName() + "\n");
        ViewUserInterface.getReference().setDataText(" Number of contours excluded: " + numExclude + ", included: " +
                                                     numInclude + "\n");
        ViewUserInterface.getReference().setDataText(" Number of punctate pixels in RGB method: " + numPixels[0] +
                                                     "\n");
        ViewUserInterface.getReference().setDataText(" Number of punctate pixels in RGB All Ratios method: " +
                                                     numPixels[1] + "\n");
        ViewUserInterface.getReference().setDataText(" Number of punctate pixels in HSB - Hue method: " + numPixels[2] +
                                                     "\n");
        ViewUserInterface.getReference().setDataText(" Number of punctate pixels in 2-out-of-3 (voting) method: " +
                                                     numPixels[3] + "\n\n");

        setCompleted(true);
    }

    /**
     * Calculates the Red, Green, and Blue mean per each section that should be either included or excluded, and saves
     * these means into several different\ float[] buffers. VOI(0) is the exclude, VOI(1) is the include, and VOI(2) is
     * the Area to examine
     *
     * @param  buffer  float[]
     */
    private void calculateVOIMeans(float[] buffer) {
        Vector<BitSet> includeMaskVector = new Vector<BitSet>();
        Vector<BitSet> excludeMaskVector = new Vector<BitSet>();
        ViewVOIVector VOIs = srcImage.getVOIs();
        VOIs.VOIAt(0).setAllActive(false);
        VOIs.VOIAt(1).setAllActive(false);

        VOI currentVOI = null;

        for (int y = 0; y < 2; y++) {
            VOIs.VOIAt(0).setAllActive(false);
            VOIs.VOIAt(1).setAllActive(false);
            currentVOI = VOIs.VOIAt(y);

            for (int v = 0; v < currentVOI.getCurves().size(); v++) {
                currentVOI.setAllActive(false);
                currentVOI.setActive(true);
                ((VOIBase) currentVOI.getCurves().elementAt(v)).setActive(true);

                if (y == 0) {

                    // System.err.println("exclude");
                    excludeMaskVector.add((BitSet) srcImage.generateVOIMask(false, true).clone());
                } else if (y == 1) {

                    // System.err.println("include");
                    includeMaskVector.add((BitSet) srcImage.generateVOIMask(false, true).clone());
                }
            }
        }

        numExclude = excludeMaskVector.size();
        numInclude = includeMaskVector.size();
        excludeMeanH = new float[numExclude];
        excludeMeanS = new float[numExclude];
        excludeMeanB = new float[numExclude];
        includeMeanH = new float[numInclude];
        includeMeanS = new float[numInclude];
        includeMeanB = new float[numInclude];
        excludeMeanRed = new float[numExclude];
        excludeMeanGreen = new float[numExclude];
        excludeMeanBlue = new float[numExclude];
        includeMeanRed = new float[numInclude];
        includeMeanGreen = new float[numInclude];
        includeMeanBlue = new float[numInclude];

        float[] excludeNumPixels = new float[numExclude];
        float[] includeNumPixels = new float[numInclude];
        BitSet currentMask = null;
        float[] hsb = new float[3];
        excludeMinH = 1000f;
        excludeMaxH = 0f;
        includeMinH = 1000f;
        includeMaxH = 0f;

        /**
         * Run through the entire buffer checking and check each VOI (include/exclude) ... if the pixel is in the VOI,
         * add in the Red/green and blue average as well as hue saturation and brightness for that VOI (it is VOI
         * specific)
         */
        for (int i = 0; i < buffer.length; i += 4) {

            for (int c = 0; c < numExclude; c++) {
                currentMask = (BitSet) excludeMaskVector.elementAt(c);

                if (currentMask.get(i / 4)) {

                    // get the r g and b values
                    excludeMeanRed[c] += buffer[i + 1];
                    excludeMeanGreen[c] += buffer[i + 2];
                    excludeMeanBlue[c] += buffer[i + 3];

                    // get the hue, saturation, and brightness values
                    Color.RGBtoHSB((int) buffer[i + 1], (int) buffer[i + 2], (int) buffer[i + 3], hsb);
                    excludeMeanH[c] += hsb[0];
                    excludeMeanS[c] += hsb[1];
                    excludeMeanB[c] += hsb[2];
                    excludeNumPixels[c]++;
                }
            }

            for (int c = 0; c < numInclude; c++) {
                currentMask = (BitSet) includeMaskVector.elementAt(c);

                if (currentMask.get(i / 4)) {

                    // get the r g and b values
                    includeMeanRed[c] += buffer[i + 1];
                    includeMeanGreen[c] += buffer[i + 2];
                    includeMeanBlue[c] += buffer[i + 3];

                    // get the hue, saturation, and brightness values
                    Color.RGBtoHSB((int) buffer[i + 1], (int) buffer[i + 2], (int) buffer[i + 3], hsb);
                    includeMeanH[c] += hsb[0];
                    includeMeanS[c] += hsb[1];
                    includeMeanB[c] += hsb[2];
                    includeNumPixels[c]++;
                }
            }
        }

        float excludeBGRatio = 0f;
        float excludeBRRatio = 0f;

        /**
         * Calculate the exclude and include means (divide by numpixels for VOIs)
         */
        for (int c = 0; c < numExclude; c++) {

            // calculate the means for RGB
            excludeMeanRed[c] /= excludeNumPixels[c];
            excludeMeanGreen[c] /= excludeNumPixels[c];
            excludeMeanBlue[c] /= excludeNumPixels[c];
            excludeRGRatio += (excludeMeanRed[c] / excludeMeanGreen[c]);
            excludeBGRatio += (excludeMeanBlue[c] / excludeMeanGreen[c]);
            excludeBRRatio += (excludeMeanBlue[c] / excludeMeanRed[c]);

            // calculate the means for HSB
            excludeMeanH[c] /= excludeNumPixels[c];
            excludeMeanS[c] /= excludeNumPixels[c];
            excludeMeanB[c] /= excludeNumPixels[c];

            if (excludeMeanH[c] < excludeMinH) {
                excludeMinH = excludeMeanH[c];
            }

            if (excludeMeanH[c] > excludeMaxH) {
                excludeMaxH = excludeMeanH[c];
            }
        }

        /**
         * calculate the exclude average ratios for ALL vois
         */
        excludeRGRatio /= numExclude;
        excludeBGRatio /= numExclude;
        excludeBRRatio /= numExclude;

        // float includeRGRatio = 0f;
        float includeBGRatio = 0f;
        float includeBRRatio = 0f;

        for (int c = 0; c < numInclude; c++) {

            // calculate the means for RGB
            includeMeanRed[c] /= includeNumPixels[c];
            includeMeanGreen[c] /= includeNumPixels[c];
            includeMeanBlue[c] /= includeNumPixels[c];
            includeRGRatio += (includeMeanRed[c] / includeMeanGreen[c]);
            includeBGRatio += (includeMeanBlue[c] / includeMeanGreen[c]);
            includeBRRatio += (includeMeanBlue[c] / includeMeanRed[c]);

            // calculate the means for HSB
            includeMeanH[c] /= includeNumPixels[c];
            includeMeanS[c] /= includeNumPixels[c];
            includeMeanB[c] /= includeNumPixels[c];

            if (includeMeanH[c] < includeMinH) {
                includeMinH = includeMeanH[c];
            }

            if (includeMeanH[c] > includeMaxH) {
                includeMaxH = includeMeanH[c];
            }
        }

        /**
         * calculate the include average ratios (for ALL vois)
         */
        includeRGRatio /= numInclude;
        includeBGRatio /= numInclude;
        includeBRRatio /= numInclude;
        System.err.println("Include Red Green Ratio: " + includeRGRatio);
        System.err.println("Include Blue Green Ratio: " + includeBGRatio);
        System.err.println("Include Blue Red Ratio: " + includeBRRatio);
        System.err.println("Exclude Red Green Ratio: " + excludeRGRatio);
        System.err.println("Exclude Blue Green Ratio: " + excludeBGRatio);
        System.err.println("Exclude Blue Red Ratio: " + excludeBRRatio);
        System.err.println("Include min Hue: " + includeMinH + " , max Hue: " + includeMaxH);
        System.err.println("Exclude min Hue: " + excludeMinH + " , max Hue: " + excludeMaxH);
    }

    /**
     * include or exclude the pixel by finding the VOI with the closest HUE. if the difference in hues is within a
     * tolerance, next check saturation... .. then check brightness
     *
     * @param   avgHue         float average hue
     * @param   avgSaturation  float average saturation
     * @param   avgBrightness  float average brightness
     *
     * @return  boolean include pixel?
     */
    private boolean includePointHSB_Hue(float avgHue, float avgSaturation, float avgBrightness) {
        int includeIndex = -1;
        int excludeIndex = -1;
        float[] diffExcludeHSB = new float[] { 1000f, 1000f, 1000f };
        float[] diffIncludeHSB = new float[] { 1000f, 1000f, 1000f };
        float tol = .001f;
        float diffHue;

        for (int i = 0; i < excludeMeanH.length; i++) {
            diffHue = Math.abs(excludeMeanH[i] - avgHue);

            if (diffHue < diffExcludeHSB[0]) {
                excludeIndex = i;
                diffExcludeHSB[0] = diffHue;
            }
        }

        for (int i = 0; i < includeMeanH.length; i++) {
            diffHue = Math.abs(includeMeanH[i] - avgHue);

            if (diffHue < diffIncludeHSB[0]) {
                includeIndex = i;
                diffIncludeHSB[0] = diffHue;
            }
        }

        if (Math.abs(diffIncludeHSB[0] - diffExcludeHSB[0]) < tol) {
            diffIncludeHSB[1] = Math.abs(includeMeanS[includeIndex] - avgSaturation);
            diffExcludeHSB[1] = Math.abs(excludeMeanS[excludeIndex] - avgSaturation);

            if (Math.abs(diffIncludeHSB[1] - diffExcludeHSB[1]) < tol) {
                diffIncludeHSB[2] = Math.abs(includeMeanB[includeIndex] - avgBrightness);
                diffExcludeHSB[2] = Math.abs(excludeMeanB[excludeIndex] - avgBrightness);

                if (diffIncludeHSB[2] < diffExcludeHSB[2]) {
                    return true;
                } else {
                    return false;
                }
            } else if (diffIncludeHSB[1] < diffExcludeHSB[1]) {
                return true;
            } else {
                return false;
            }
        } else if (diffIncludeHSB[0] < diffExcludeHSB[0]) {
            return true;
        }
        // closer to exclude
        else {
            return false;
        }
    }

    /**
     * include or exclude the point based on the red to green ratio ... if within tolerance, next check blue to green
     * ratio... ... then check blue to red ratio
     *
     * @param   avgRed    float average red
     * @param   avgGreen  float average green
     * @param   avgBlue   float average blue
     *
     * @return  boolean include pixel?
     *
     *          <p>private boolean includePointRGB_RGRatio(float avgRed, float avgGreen, float avgBlue) {</p>
     *
     *          <p>float diffIncludeRatio = 1000f; float diffExcludeRatio = 1000f;</p>
     *
     *          <p>int includeIndex = -1; int excludeIndex = -1;</p>
     *
     *          <p>float tol = .1f;</p>
     *
     *          <p>float diffRatio = 0f;</p>
     *
     *          <p>float[] avgRGBRatio = new float[] { (avgRed / avgGreen), (avgBlue / avgGreen), (avgBlue / avgRed)};
     *          </p>
     *
     *          <p>for (int i = 0; i < excludeMeanRed.length; i++) {</p>
     *
     *          <p>diffRatio = Math.abs( (excludeMeanRed[i] / excludeMeanGreen[i]) - avgRGBRatio[0]);</p>
     *
     *          <p>if (diffRatio < diffExcludeRatio) { excludeIndex = i; diffExcludeRatio = diffRatio; } }</p>
     *
     *          <p>for (int i = 0; i < includeMeanRed.length; i++) {</p>
     *
     *          <p>diffRatio = Math.abs( (includeMeanRed[i] / includeMeanGreen[i]) - avgRGBRatio[0]);</p>
     *
     *          <p>if (diffRatio < diffIncludeRatio) { includeIndex = i; diffIncludeRatio = diffRatio; } }</p>
     *
     *          <p>if (Math.abs(diffIncludeRatio - diffExcludeRatio) < tol) { diffIncludeRatio = Math.abs(
     *          (includeMeanBlue[includeIndex] / includeMeanGreen[includeIndex]) - avgRGBRatio[2]); diffExcludeRatio =
     *          Math.abs( (excludeMeanBlue[excludeIndex] / excludeMeanGreen[excludeIndex]) - avgRGBRatio[2]);</p>
     *
     *          <p>if (Math.abs(diffIncludeRatio - diffExcludeRatio) < tol) { diffIncludeRatio = Math.abs(
     *          (includeMeanBlue[includeIndex] / includeMeanRed[includeIndex]) - avgRGBRatio[1]); diffExcludeRatio =
     *          Math.abs( (excludeMeanBlue[excludeIndex] / excludeMeanRed[excludeIndex]) - avgRGBRatio[1]);</p>
     *
     *          <p>if (diffIncludeRatio < diffExcludeRatio) { return true; } else { return false; } } else if
     *          (diffIncludeRatio < diffExcludeRatio) { return true; } else { return false; } } else if
     *          (diffIncludeRatio < diffExcludeRatio) { return true; } // closer to exclude else { return false; } }</p>
     */
    /**
     * includes or excludes pixel by finding the VOI with the closest matching red to green ratio, blue to green ratio,
     * and blue to red ratio. if the VOI is an include VOI, include the pixel, otherwise exclude it
     *
     * @param   avgRed    float average red
     * @param   avgGreen  float average green
     * @param   avgBlue   float average blue
     *
     * @return  boolean include pixel?
     */
    private boolean includePointRGB_AllRatios(float avgRed, float avgGreen, float avgBlue) {
        float diffIncludeRatio = 1000f;
        float diffExcludeRatio = 1000f;
        float diffRatio = 0f;
        float avgRGRatio = avgRed / avgGreen;
        float avgBGRatio = avgBlue / avgGreen;
        float avgBRRatio = avgBlue / avgRed;

        for (int i = 0; i < excludeMeanRed.length; i++) {
            diffRatio = (Math.abs((excludeMeanRed[i] / excludeMeanGreen[i]) - avgRGRatio) +
                         Math.abs((excludeMeanBlue[i] / excludeMeanGreen[i]) - avgBGRatio) +
                         Math.abs((excludeMeanBlue[i] / excludeMeanRed[i]) - avgBRRatio));

            if (diffRatio < diffExcludeRatio) {
                diffExcludeRatio = diffRatio;
            }
        }

        for (int i = 0; i < includeMeanRed.length; i++) {
            diffRatio = (Math.abs((includeMeanRed[i] / includeMeanGreen[i]) - avgRGRatio) +
                         Math.abs((includeMeanBlue[i] / includeMeanGreen[i]) - avgBGRatio) +
                         Math.abs((includeMeanBlue[i] / includeMeanRed[i]) - avgBRRatio));

            if (diffRatio < diffIncludeRatio) {
                diffIncludeRatio = diffRatio;
            }
        }

        if (diffIncludeRatio < diffExcludeRatio) {
            return true;
        } else {
            return false;
        }
    }

    /**
     * (currently unused) Include or exclude pixel by matching the VOI with the closest hue, brightness, AND saturation
     * (finding the least difference)... if VOI is include, include the pixel otherwise exclude @param avgHue float
     * average hue @param avgSaturation float average saturation @param avgBrightness float average brightness @return
     * boolean
     *
     * private boolean includePointHSB_All(float avgHue, float avgSaturation, float avgBrightness) {
     *
     * float diffExcludeHSB = 1000f; float diffIncludeHSB = 1000f;
     *
     * float diffAll = 0f;
     *
     * for (int i = 0; i < excludeMeanRed.length; i++) {
     *
     * diffAll = (Math.abs(excludeMeanH[i] - avgHue) + Math.abs(excludeMeanS[i] - avgSaturation) +
     * Math.abs(excludeMeanB[i] - avgBrightness));
     *
     * if (diffAll < diffExcludeHSB) { diffExcludeHSB = diffAll; } }
     *
     * for (int i = 0; i < includeMeanRed.length; i++) {
     *
     * diffAll = (Math.abs(includeMeanH[i] - avgHue) + Math.abs(includeMeanS[i] - avgSaturation) +
     * Math.abs(includeMeanB[i] - avgBrightness));
     *
     * if (diffAll < diffIncludeHSB) { diffIncludeHSB = diffAll; } }
     *
     * if (diffIncludeHSB < diffExcludeHSB) { return true; } else { return false; } }
     */
}
