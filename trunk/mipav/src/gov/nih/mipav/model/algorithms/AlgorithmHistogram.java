package gov.nih.mipav.model.algorithms;


import gov.nih.mipav.model.file.*;
import gov.nih.mipav.model.structures.*;

import gov.nih.mipav.view.*;

import java.io.*;

import java.util.*;


/**
 * Calculates the histogram for an image. The number of bins is determined by the extents of the histogram model. The
 * calling algorithm can specify if the histogram is to be calculated for whole image or only over VOI regions. The Otsu
 * and Maximum Entrophy thresholds are also automatically calculated and stored in the ModelHistogram structure. The
 * Otsu and Max entropy can take time (minutes) to calculate for histgrams with bins larger than (10,000).
 *
 * @version  0.1 September 18, 1997
 * @see      ModelHistogram
 */
public class AlgorithmHistogram extends AlgorithmBase {

    //~ Instance fields ------------------------------------------------------------------------------------------------

    /** Flag indicating whether or not histogram data is output to the data window. */
    private boolean dataOutput = false;

    /** If true the histogram should be calculated for the entire image. */
    private boolean entireImage = true;

    /** DOCUMENT ME! */
    private double[] highValue = null;

    /** DOCUMENT ME! */
    private int[] histoBuffer = null;

    /** Reference to the histogram storage object. */
    private ModelHistogram histogram;

    /** Reference to the image. */
    private ModelImage image;

    /** DOCUMENT ME! */
    private double[] lowValue = null;

    /**
     * Indicates which channel of the RGB image the histogram should be calculated. 1 = Red channel 2 = Green channel 3
     * = Blue channel
     */
    private int RGBOffset;

    /**
     * Used to indicate the number of histogram bins when using the constructors that do NOT pass in the histogram
     * object.
     */
    private int summaryBins = 256;

    //~ Constructors ---------------------------------------------------------------------------------------------------

    /**
     * Constructs the histogram calculation object for an image.
     *
     * @param  image        ModelImage the image
     * @param  summaryBins  number of bins in the histogram
     */
    public AlgorithmHistogram(ModelImage image, int summaryBins) {
        this.image = image;
        this.summaryBins = summaryBins;
        dataOutput = true;
    }

    /**
     * Constructs the histogram calculation object.
     *
     * @param  histogram  model of a histogram
     * @param  image      model of the source image
     * @param  maskFlag   Flag that indicates that the histogram will be calculated for the whole image if equal to true
     */
    public AlgorithmHistogram(ModelHistogram histogram, ModelImage image, boolean maskFlag) {
        this.histogram = histogram;
        this.image = image;
        this.entireImage = maskFlag;

        if (entireImage == false) {
            mask = image.generateVOIMask();
        }
    }

    /**
     * Constructs the histogram calculation object for an image.
     *
     * @param  image        ModelImage the image
     * @param  summaryBins  number of bins in the histogram
     * @param  maskFlag     Flag that indicates that the histogram will be calculated for the whole image if equal to
     *                      true
     */
    public AlgorithmHistogram(ModelImage image, int summaryBins, boolean maskFlag) {
        this.image = image;
        this.summaryBins = summaryBins;
        dataOutput = true;
        this.entireImage = maskFlag;

        if (entireImage == false) {
            mask = image.generateVOIMask();
        }
    }

    /**
     * Constructs the histogram calculation object for an RGB image.
     *
     * @param  image        ModelImage the image
     * @param  summaryBins  number of bins in the histogram
     * @param  RGBOffset    correct offset for RED = 1 , GREEN = 2, or BLUE = 3 component to be exported
     */
    public AlgorithmHistogram(ModelImage image, int summaryBins, int RGBOffset) {
        this.image = image;
        this.summaryBins = summaryBins;
        this.RGBOffset = RGBOffset;
        dataOutput = true;
    }

    /**
     * Constructs the histogram calculation object for an RGB image.
     *
     * @param  histogram  model of a histogram for a RGB component
     * @param  RGBOffset  correct offset for RED = 1 , GREEN = 2, or BLUE = 3 component to be exported
     * @param  image      model of the source image
     * @param  maskFlag   Flag that indicates that the histogram will be calculated for the whole image if equal to true
     */
    public AlgorithmHistogram(ModelHistogram histogram, int RGBOffset, ModelImage image, boolean maskFlag) {
        this.histogram = histogram;
        this.RGBOffset = RGBOffset;
        this.image = image;
        this.entireImage = maskFlag;

        if (entireImage == false) {
            mask = image.generateVOIMask();
        }
    }

    /**
     * Constructs the histogram calculation object for an RGB image.
     *
     * @param  image        ModelImage the image
     * @param  summaryBins  number of bins in the histogram
     * @param  RGBOffset    correct offset for RED = 1 , GREEN = 2, or BLUE = 3 component to be exported
     * @param  maskFlag     Flag that indicates that the histogram will be calculated for the whole image if equal to
     *                      true
     */
    public AlgorithmHistogram(ModelImage image, int summaryBins, int RGBOffset, boolean maskFlag) {
        this.image = image;
        this.summaryBins = summaryBins;
        this.RGBOffset = RGBOffset;
        dataOutput = true;
        this.entireImage = maskFlag;

        if (entireImage == false) {
            mask = image.generateVOIMask();
        }
    }

    //~ Methods --------------------------------------------------------------------------------------------------------

    /**
     * Dispose of local variables.
     */
    public void disposeLocal() {

        if (histogram != null) {
            histogram = null;
        }

        if (image != null) {
            image = null;
        }

        histoBuffer = null;
        lowValue = null;
        highValue = null;
    }

    /**
     * Sets a flag to indicate if the histogram is to be calculated for the entire image or just the VOI.
     *
     * @param  flag  true = histogram of entire image, false = histogram of VOI
     */
    public void entireImage(boolean flag) {
        entireImage = flag;
    }

    /**
     * Prepares this class for destruction.
     */
    public void finalize() {
        disposeLocal();
        super.finalize();
    }

    /**
     * DOCUMENT ME!
     *
     * @return  double[]
     */
    public double[] getHighValue() {
        return highValue;
    }

    /**
     * DOCUMENT ME!
     *
     * @return  int[]
     */
    public int[] getHistoBuffer() {
        return histoBuffer;
    }

    /**
     * DOCUMENT ME!
     *
     * @return  double[]
     */
    public double[] getLowValue() {
        return lowValue;
    }

    /**
     * Accessor to the histogram.
     *
     * @return  returns the model of the histogram
     */
    public ModelHistogram getModelHistogram() {
        return histogram;
    }

    /**
     * Calculates the histogram.
     */
    public void runAlgorithm() {

        int i;
        int length = 1;
        int bins = 1;
        float[] imgBuffer;

        if (dataOutput) {
            runData(summaryBins);

            return;
        }

        if (histogram == null) {
            displayError("Histogram is null");

            return;
        }

        if (image == null) {
            displayError("Source Image is null");

            return;
        }

        try {
            bins = 1;

            for (i = 0; i < histogram.getNDims(); i++) {
                bins *= histogram.getExtents()[i];
            }

            histoBuffer = new int[bins];
            histogram.exportData(0, bins, histoBuffer); // locks and releases lock
            buildProgressBar("Histogram", "Calculating histogram...", 0, 100);
        } catch (IOException error) {
            errorCleanUp("Algorithm Histogram: Histogram locked", false);

            return;
        } catch (OutOfMemoryError e) {
            errorCleanUp("Algorithm Histogram: Out of memory", false);

            return;
        }

        int z, zStop;
        int value;
        double imageMax, imageMin;
        double divisor;
        double factor;

        switch (image.getType()) {

            case ModelStorageBase.BYTE:
                imageMin = -128;
                imageMax = 127;
                break;

            case ModelStorageBase.UBYTE:
                imageMin = 0;
                imageMax = 255;
                break;

            case ModelStorageBase.ARGB:
                imageMin = 0;
                imageMax = 255;
                break;

            case ModelStorageBase.ARGB_USHORT:
                if (RGBOffset == 1) {
                    imageMin = image.getMinR();
                    imageMax = image.getMaxR();
                } else if (RGBOffset == 2) {
                    imageMin = image.getMinG();
                    imageMax = image.getMaxG();
                } else {
                    imageMin = image.getMinB();
                    imageMax = image.getMaxB();
                }

                break;

            case ModelStorageBase.SHORT:
                imageMin = (double) image.getMin();
                imageMax = (double) image.getMax();
                break;

            default: {
                imageMin = (double) image.getMin();
                imageMax = (double) image.getMax();
                break;
            }
        }

        length = image.getSliceSize();
        imgBuffer = new float[length];

        if (image.getNDims() > 2) {
            zStop = image.getExtents()[2];
        } else {
            zStop = 1;
        }

        if (entireImage == false) {
            mask = image.generateVOIMask();
        }

        initProgressBar();

        try {
            image.setLock(ModelStorageBase.W_LOCKED);
        } catch (IOException error) {
            errorCleanUp("Algorithm Histogram: Image locked", false);

            return;
        }

        for (z = 0; z < zStop; z++) {

            try {

                if (image.getType() == ModelStorageBase.COMPLEX) {
                    image.exportMagData(2 * z * length, length, imgBuffer);
                } else if (image.isColorImage()) {
                    image.exportRGBDataNoLock(RGBOffset, 4 * z * length, length, imgBuffer);
                } else {
                    image.exportDataNoLock(z * length, length, imgBuffer);
                }
            } catch (IOException error) {
                errorCleanUp("Algorithm Histogram: image bounds exceeded", false);
                image.releaseLock();

                return;
            }

            if ((image.getType() == ModelStorageBase.COMPLEX) && (image.getLogMagDisplay() == true)) {

                for (i = 0; i < length; i++) {
                    imgBuffer[i] = (float) (0.4342944819 * java.lang.Math.log(1 + imgBuffer[i]));
                }
            }

            divisor = imageMax - imageMin;

            if (divisor == 0) {
                divisor = 1;
            }

            factor = (bins - 1) / divisor;

            // This is the part that actually calculates the histogram
            if (entireImage == false) {

                for (i = 0; i < length; i++) {

                    if (mask.get((z * length) + i)) { // just calc. for VOI
                        value = (int) (((imgBuffer[i] - imageMin) * factor) + 0.5f);
                        histoBuffer[value]++;
                    }
                }
            } else {

                // Calculate for the entire image
                for (i = 0; i < length; i++) {
                    value = (int) (((imgBuffer[i] - imageMin) * factor) + 0.5f);

                    // System.out.println( "histoBuffer[value] = " + histoBuffer[value]);
                    histoBuffer[value]++;
                }
            }

            if (isProgressBarVisible()) {
                progressBar.updateValue(Math.round((float) (z + 1) / zStop * 100), runningInSeparateThread);
            }
        }

        image.releaseLock();

        try {
            histogram.importData(0, histoBuffer, true); // locks and releases lock
        } catch (IOException error) {
            errorCleanUp("Algorithm Histogram: histogram locked", false);

            return;
        }

        int stRange = -1;
        int endRange = -1;
        int mode = 0;
        float mean = 0;
        long temp;

        for (i = 0; i < bins; i++) {
            temp = histoBuffer[i];

            if ((temp != 0) && (stRange < 0)) {
                stRange = i;
            }

            if (temp != 0) {
                endRange = i;
            }

            if (temp > histoBuffer[mode]) {
                mode = i;
            }

            mean = mean + ((long) i * temp);
        }

        mean = mean / bins;
        histogram.setTotalPixels(length * zStop);
        histogram.setMean(mean);
        histogram.setMode(mode);
        histogram.setStartRange(stRange);
        histogram.setEndRange(endRange);

        // Threshold functions
        histogram.setMaxEntropyThreshold(entropySplit(histoBuffer));

        // attempt an otsu threshold
        histogram.setOtsuThreshold(otsuThreshold(image, histoBuffer));

        disposeProgressBar();
        setCompleted(true);
        imgBuffer = null;
        histoBuffer = null;
        System.gc();
    }

    /**
     * Calculates the histogram and outputs the number of counts per bin and area/volume information.
     *
     * @param  bins  number of bins in the histogram
     */
    public void runData(int bins) {
        double imageMax, imageMin;
        float[] imgBuffer;
        int z, zStop;
        int value;
        int length;
        double divisor;
        double factor;
        int i;
        ViewUserInterface UI;

        if (image == null) {
            displayError("Source Image is null");

            return;
        }

        buildProgressBar("Histogram", "Calculating histogram data...", 0, 100);
        initProgressBar();

        UI = image.getUserInterface();

        switch (image.getType()) {


            case ModelStorageBase.ARGB:
            case ModelStorageBase.ARGB_USHORT:
            case ModelStorageBase.ARGB_FLOAT:
                if (RGBOffset == 1) {
                    imageMin = image.getMinR();
                    imageMax = image.getMaxR();
                } else if (RGBOffset == 2) {
                    imageMin = image.getMinG();
                    imageMax = image.getMaxG();
                } else {
                    imageMin = image.getMinB();
                    imageMax = image.getMaxB();
                }

                break;

            default:
                imageMin = (double) image.getMin();
                imageMax = (double) image.getMax();
                break;
        }

        if ((image.getType() != ModelStorageBase.FLOAT) && (image.getType() != ModelStorageBase.DOUBLE) &&
                (image.getType() != ModelStorageBase.ARGB_FLOAT)) {
            bins = Math.min(bins, (int) Math.round(imageMax - imageMin + 1));
            bins = Math.min(bins, 4096);
        }

        histoBuffer = new int[bins];
        length = image.getSliceSize();
        imgBuffer = new float[length];
        lowValue = new double[bins];
        highValue = new double[bins];


        if (image.getNDims() > 2) {
            zStop = image.getExtents()[2];
        } else {
            zStop = 1;
        }

        try {
            image.setLock(ModelStorageBase.W_LOCKED);
        } catch (IOException error) {
            errorCleanUp("Algorithm Histogram: Image locked", false);

            return;
        }

        for (z = 0; z < zStop; z++) {

            try {

                if (image.getType() == ModelStorageBase.COMPLEX) {
                    image.exportMagData(2 * z * length, length, imgBuffer);
                } else if (image.isColorImage()) {
                    image.exportRGBDataNoLock(RGBOffset, 4 * z * length, length, imgBuffer);
                } else {
                    image.exportDataNoLock(z * length, length, imgBuffer);
                }
            } catch (IOException error) {
                errorCleanUp("Algorithm Histogram: image bounds exceeded", false);
                image.releaseLock();

                return;
            }

            if ((image.getType() == ModelStorageBase.COMPLEX) && (image.getLogMagDisplay() == true)) {

                for (i = 0; i < length; i++) {
                    imgBuffer[i] = (float) (0.4342944819 * java.lang.Math.log(1 + imgBuffer[i]));
                }
            }

            divisor = imageMax - imageMin;

            if (divisor == 0) {
                divisor = 1;
            }

            factor = (bins - 1) / divisor;

            // This is the part that actually calculates the histogram
            // Calculate for the entire image
            for (i = 0; i < length; i++) {

                if (((entireImage == false) && mask.get(i + (length * z))) || (entireImage == true)) {
                    value = (int) (((imgBuffer[i] - imageMin) * factor) + 0.5f);
                    histoBuffer[value]++;

                    if (histoBuffer[value] == 1) {
                        lowValue[value] = imgBuffer[i];
                        highValue[value] = imgBuffer[i];
                    } // if histoBuffer[value] == 1)
                    else {

                        if (imgBuffer[i] < lowValue[value]) {
                            lowValue[value] = imgBuffer[i];
                        } else if (imgBuffer[i] > highValue[value]) {
                            highValue[value] = imgBuffer[i];
                        }
                    } // else
                }
            }

            if (isProgressBarVisible()) {
                progressBar.updateValue(Math.round((float) (z + 1) / zStop * 100), runningInSeparateThread);
            }
        }

        image.releaseLock();

        if (image.getNDims() == 2) {
            int[] units = image.getFileInfo()[0].getUnitsOfMeasure();

            if ((units[0] != FileInfoBase.UNKNOWN_MEASURE) && (units[0] == units[1])) {
                float[] res = image.getFileInfo()[0].getResolutions();
                float pixelSize = res[0] * res[1];
                String unitsStr = FileInfoBase.getUnitsOfMeasureStr(units[0]);


                if (image.isColorImage()) {

                    if (RGBOffset == 1) {

                        for (i = 0; i < bins; i++) {

                            if (histoBuffer[i] >= 1) {

                                if (lowValue[i] == highValue[i]) {
                                    UI.setDataText(image.getImageName() + " red intensity = " + lowValue[i] +
                                                   "\t\t count = " + histoBuffer[i] + "\t area = " +
                                                   (pixelSize * histoBuffer[i]) + " square " + unitsStr + "\n");
                                } // if (lowValue[i] == highValue[i])
                                else { // lowValue[i] != highValue[i]
                                    UI.setDataText(image.getImageName() + " red intensities = " + lowValue[i] + " to " +
                                                   highValue[i] + "\t count = " + histoBuffer[i] + "\t area = " +
                                                   (pixelSize * histoBuffer[i]) + " square " + unitsStr + "\n");
                                } // lowValue[i] != highValue[i]
                            } // if (histoBuffer[i] >= 1)
                        } // for (i = 0; i < bins; i++)
                    } // if (RGBOffset == 1)
                    else if (RGBOffset == 2) {

                        for (i = 0; i < bins; i++) {

                            if (histoBuffer[i] >= 1) {

                                if (lowValue[i] == highValue[i]) {
                                    UI.setDataText(image.getImageName() + " green intensity = " + lowValue[i] +
                                                   "\t\t count = " + histoBuffer[i] + "\t area = " +
                                                   (pixelSize * histoBuffer[i]) + " square " + unitsStr + "\n");
                                } // if (lowValue[i] == highValue[i])
                                else { // lowValue[i] != highValue[i]
                                    UI.setDataText(image.getImageName() + " green intensities = " + lowValue[i] +
                                                   " to " + highValue[i] + "\t count = " + histoBuffer[i] +
                                                   "\t area = " + (pixelSize * histoBuffer[i]) + " square " + unitsStr +
                                                   "\n");
                                } // lowValue[i] != highValue[i]
                            } // if (histoBuffer[i] >= 1)
                        } // for (i = 0; i < bins; i++)
                    } // else if (RGBOffset == 2)
                    else { // RGBOffset == 3

                        for (i = 0; i < bins; i++) {

                            if (histoBuffer[i] >= 1) {

                                if (lowValue[i] == highValue[i]) {
                                    UI.setDataText(image.getImageName() + " blue intensity = " + lowValue[i] +
                                                   "\t\t count = " + histoBuffer[i] + "\t area = " +
                                                   (pixelSize * histoBuffer[i]) + " square " + unitsStr + "\n");
                                } // if (lowValue[i] == highValue[i])
                                else { // lowValue[i] != highValue[i]
                                    UI.setDataText(image.getImageName() + " blue intensities = " + lowValue[i] +
                                                   " to " + highValue[i] + "\t count = " + histoBuffer[i] +
                                                   "\t area = " + (pixelSize * histoBuffer[i]) + " square " + unitsStr +
                                                   "\n");
                                } // lowValue[i] != highValue[i]
                            } // if (histoBuffer[i] >= 1)
                        } // for (i = 0; i < bins; i++)
                    } // else RGBOffset == 3
                } // color image
                else { // other image type

                    for (i = 0; i < bins; i++) {

                        if (histoBuffer[i] >= 1) {

                            if (lowValue[i] == highValue[i]) {
                                UI.setDataText(image.getImageName() + " intensity = " + lowValue[i] + "\t\t count = " +
                                               histoBuffer[i] + "\t area = " + (pixelSize * histoBuffer[i]) +
                                               " square " + unitsStr + "\n");
                            } // if (lowValue[i] == highValue[i])
                            else { // lowValue[i] != highValue[i]
                                UI.setDataText(image.getImageName() + " intensities = " + lowValue[i] + " to " +
                                               highValue[i] + "\t count = " + histoBuffer[i] + "\t area = " +
                                               (pixelSize * histoBuffer[i]) + " square " + unitsStr + "\n");
                            } // lowValue[i] != highValue[i]
                        } // if (histoBuffer[i] >= 1)
                    } // for (i = 0; i < bins; i++)
                } // other image type
            } // specify area units
            else { // no area units

                if (image.isColorImage()) {

                    if (RGBOffset == 1) {

                        for (i = 0; i < bins; i++) {

                            if (histoBuffer[i] >= 1) {

                                if (lowValue[i] == highValue[i]) {
                                    UI.setDataText(image.getImageName() + " red intensity = " + lowValue[i] +
                                                   "\t\t count = " + histoBuffer[i] + "\n");
                                } // if (lowValue[i] == highValue[i])
                                else { // lowValue[i] != highValue[i]
                                    UI.setDataText(image.getImageName() + " red intensities = " + lowValue[i] + " to " +
                                                   highValue[i] + "\t count = " + histoBuffer[i] + "\n");
                                } // lowValue[i] != highValue[i]
                            } // if (histoBuffer[i] >= 1)
                        } // for (i = 0; i < bins; i++)
                    } // if (RGBOffset == 1)
                    else if (RGBOffset == 2) {

                        for (i = 0; i < bins; i++) {

                            if (histoBuffer[i] >= 1) {

                                if (lowValue[i] == highValue[i]) {
                                    UI.setDataText(image.getImageName() + " green intensity = " + lowValue[i] +
                                                   "\t\t count = " + histoBuffer[i] + "\n");
                                } // if (lowValue[i] == highValue[i])
                                else { // lowValue[i] != highValue[i]
                                    UI.setDataText(image.getImageName() + " green intensities = " + lowValue[i] +
                                                   " to " + highValue[i] + "\t count = " + histoBuffer[i] + "\n");
                                } // lowValue[i] != highValue[i]
                            } // if (histoBuffer[i] >= 1)
                        } // for (i = 0; i < bins; i++)
                    } // else if (RGBOffset == 2)
                    else { // RGBOffset == 3

                        for (i = 0; i < bins; i++) {

                            if (histoBuffer[i] >= 1) {

                                if (lowValue[i] == highValue[i]) {
                                    UI.setDataText(image.getImageName() + " blue intensity = " + lowValue[i] +
                                                   "\t\t count = " + histoBuffer[i] + "\n");
                                } // if (lowValue[i] == highValue[i])
                                else { // lowValue[i] != highValue[i]
                                    UI.setDataText(image.getImageName() + " blue intensities = " + lowValue[i] +
                                                   " to " + highValue[i] + "\t count = " + histoBuffer[i] + "\n");
                                } // lowValue[i] != highValue[i]
                            } // if (histoBuffer[i] >= 1)
                        } // for (i = 0; i < bins; i++)
                    } // else RGBOffset == 3
                } // color image
                else { // other image type

                    for (i = 0; i < bins; i++) {

                        if (histoBuffer[i] >= 1) {

                            if (lowValue[i] == highValue[i]) {
                                UI.setDataText(image.getImageName() + " intensity = " + lowValue[i] + "\t\t count = " +
                                               histoBuffer[i] + "\n");
                            } // if (lowValue[i] == highValue[i])
                            else { // lowValue[i] != highValue[i]
                                UI.setDataText(image.getImageName() + " intensities = " + lowValue[i] + " to " +
                                               highValue[i] + "\t count = " + histoBuffer[i] + "\n");
                            } // lowValue[i] != highValue[i]
                        } // if (histoBuffer[i] >= 1)
                    } // for (i = 0; i < bins; i++)
                } // other image type
            } // no area units
        } // if (image.getNDims() == 2)
        else if (image.getNDims() == 3) {
            int[] units = image.getFileInfo()[0].getUnitsOfMeasure();

            if ((units[0] != FileInfoBase.UNKNOWN_MEASURE) && (units[0] == units[1]) && (units[0] == units[2])) {
                float[] res = image.getFileInfo()[0].getResolutions();
                float pixelSize = res[0] * res[1] * res[2];
                String unitsStr = FileInfoBase.getUnitsOfMeasureStr(units[0]);

                if (image.isColorImage()) {

                    if (RGBOffset == 1) {

                        for (i = 0; i < bins; i++) {

                            if (histoBuffer[i] >= 1) {

                                if (lowValue[i] == highValue[i]) {
                                    UI.setDataText(image.getImageName() + " red intensity = " + lowValue[i] +
                                                   "\t\t count = " + histoBuffer[i] + "\t volume = " +
                                                   (pixelSize * histoBuffer[i]) + " cubic " + unitsStr + "\n");
                                } // if (lowValue[i] == highValue[i])
                                else { // lowValue[i] != highValue[i]
                                    UI.setDataText(image.getImageName() + " red intensities = " + lowValue[i] + " to " +
                                                   highValue[i] + "\t count = " + histoBuffer[i] + "\t volume = " +
                                                   (pixelSize * histoBuffer[i]) + " cubic " + unitsStr + "\n");
                                } // lowValue[i] != highValue[i]
                            } // if (histoBuffer[i] >= 1)
                        } // for (i = 0; i < bins; i++)
                    } // if (RGBOffset == 1)
                    else if (RGBOffset == 2) {

                        for (i = 0; i < bins; i++) {

                            if (histoBuffer[i] >= 1) {

                                if (lowValue[i] == highValue[i]) {
                                    UI.setDataText(image.getImageName() + " green intensity = " + lowValue[i] +
                                                   "\t\t count = " + histoBuffer[i] + "\t volume = " +
                                                   (pixelSize * histoBuffer[i]) + " cubic " + unitsStr + "\n");
                                } // if (lowValue[i] == highValue[i])
                                else { // lowValue[i] != highValue[i]
                                    UI.setDataText(image.getImageName() + " green intensities = " + lowValue[i] +
                                                   " to " + highValue[i] + "\t count = " + histoBuffer[i] +
                                                   "\t volume = " + (pixelSize * histoBuffer[i]) + " cubic " +
                                                   unitsStr + "\n");
                                } // lowValue[i] != highValue[i]
                            } // if (histoBuffer[i] >= 1)
                        } // for (i = 0; i < bins; i++)
                    } // else if (RGBOffset == 2)
                    else { // RGBOffset == 3

                        for (i = 0; i < bins; i++) {

                            if (histoBuffer[i] >= 1) {

                                if (lowValue[i] == highValue[i]) {
                                    UI.setDataText(image.getImageName() + " blue intensity = " + lowValue[i] +
                                                   "\t\t count = " + histoBuffer[i] + "\t volume = " +
                                                   (pixelSize * histoBuffer[i]) + " cubic " + unitsStr + "\n");
                                } // if (lowValue[i] == highValue[i])
                                else { // lowValue[i] != highValue[i]
                                    UI.setDataText(image.getImageName() + " blue intensities = " + lowValue[i] +
                                                   " to " + highValue[i] + "\t count = " + histoBuffer[i] +
                                                   "\t volume = " + (pixelSize * histoBuffer[i]) + " cubic " +
                                                   unitsStr + "\n");
                                } // lowValue[i] != highValue[i]
                            } // if (histoBuffer[i] >= 1)
                        } // for (i = 0; i < bins; i++)
                    } // else RGBOffset == 3
                } // color image
                else { // other image type

                    for (i = 0; i < bins; i++) {

                        if (histoBuffer[i] >= 1) {

                            if (lowValue[i] == highValue[i]) {
                                UI.setDataText(image.getImageName() + " intensity = " + lowValue[i] + "\t\t count = " +
                                               histoBuffer[i] + "\t volume = " + (pixelSize * histoBuffer[i]) +
                                               " cubic " + unitsStr + "\n");
                            } // if (lowValue[i] == highValue[i])
                            else { // lowValue[i] != highValue[i]
                                UI.setDataText(image.getImageName() + " intensities = " + lowValue[i] + " to " +
                                               highValue[i] + "\t count = " + histoBuffer[i] + "\t volume = " +
                                               (pixelSize * histoBuffer[i]) + " cubic " + unitsStr + "\n");
                            } // lowValue[i] != highValue[i]
                        } // if (histoBuffer[i] >= 1)
                    } // for (i = 0; i < bins; i++)
                } // other image type
            } // specify area units
            else { // no area units

                if (image.isColorImage()) {

                    if (RGBOffset == 1) {

                        for (i = 0; i < bins; i++) {

                            if (histoBuffer[i] >= 1) {

                                if (lowValue[i] == highValue[i]) {
                                    UI.setDataText(image.getImageName() + " red intensity = " + lowValue[i] +
                                                   "\t\t count = " + histoBuffer[i] + "\n");
                                } // if (lowValue[i] == highValue[i])
                                else { // lowValue[i] != highValue[i]
                                    UI.setDataText(image.getImageName() + " red intensities = " + lowValue[i] + " to " +
                                                   highValue[i] + "\t count = " + histoBuffer[i] + "\n");
                                } // lowValue[i] != highValue[i]
                            } // if (histoBuffer[i] >= 1)
                        } // for (i = 0; i < bins; i++)
                    } // if (RGBOffset == 1)
                    else if (RGBOffset == 2) {

                        for (i = 0; i < bins; i++) {

                            if (histoBuffer[i] >= 1) {

                                if (lowValue[i] == highValue[i]) {
                                    UI.setDataText(image.getImageName() + " green intensity = " + lowValue[i] +
                                                   "\t\t count = " + histoBuffer[i] + "\n");
                                } // if (lowValue[i] == highValue[i])
                                else { // lowValue[i] != highValue[i]
                                    UI.setDataText(image.getImageName() + " green intensities = " + lowValue[i] +
                                                   " to " + highValue[i] + "\t count = " + histoBuffer[i] + "\n");
                                } // lowValue[i] != highValue[i]
                            } // if (histoBuffer[i] >= 1)
                        } // for (i = 0; i < bins; i++)
                    } // else if (RGBOffset == 2)
                    else { // RGBOffset == 3

                        for (i = 0; i < bins; i++) {

                            if (histoBuffer[i] >= 1) {

                                if (lowValue[i] == highValue[i]) {
                                    UI.setDataText(image.getImageName() + " blue intensity = " + lowValue[i] +
                                                   "\t\t count = " + histoBuffer[i] + "\n");
                                } // if (lowValue[i] == highValue[i])
                                else { // lowValue[i] != highValue[i]
                                    UI.setDataText(image.getImageName() + " blue intensities = " + lowValue[i] +
                                                   " to " + highValue[i] + "\t count = " + histoBuffer[i] + "\n");
                                } // lowValue[i] != highValue[i]
                            } // if (histoBuffer[i] >= 1)
                        } // for (i = 0; i < bins; i++)
                    } // else RGBOffset == 3
                } // color image
                else { // other image type

                    for (i = 0; i < bins; i++) {

                        if (histoBuffer[i] >= 1) {

                            if (lowValue[i] == highValue[i]) {
                                UI.setDataText(image.getImageName() + " intensity = " + lowValue[i] + "\t\t count = " +
                                               histoBuffer[i] + "\n");
                            } // if (lowValue[i] == highValue[i])
                            else { // lowValue[i] != highValue[i]
                                UI.setDataText(image.getImageName() + " intensities = " + lowValue[i] + " to " +
                                               highValue[i] + "\t count = " + histoBuffer[i] + "\n");
                            } // lowValue[i] != highValue[i]
                        } // if (histoBuffer[i] >= 1)
                    } // for (i = 0; i < bins; i++)
                } // other image type
            } // no area units
        } // else if (image.geteNDims() = 3)

        // in this algorithm, we must clear the paint mask, since it was generated during generateVOIMask in the
        // constructor In the future, we might consider using a separate paint mask so that the image's paint mask can
        // remain even after this algorithm is run
        if (mask != null) {
            mask.clear();
        }

        Vector imageFrameVector = image.getImageFrameVector();

        for (i = 0; i < imageFrameVector.size(); i++) {
            Object object = imageFrameVector.elementAt(i);

            if (object instanceof ViewJFrameImage) {
                ViewJFrameImage imageFrame = (ViewJFrameImage) object;

                imageFrame.updateImages(true);
            }
        }

        disposeProgressBar();
        setCompleted(true);
        imgBuffer = null;
        System.gc();
    }

    /**
     * Calculate maximum entropy split of a histogram. This method is very similar to Otsu's method. Rather than
     * maximising the inter-class variance, it maximizes the inter-class entropy. Entropy is a measure of the
     * uncertainity of an event taking place. You can calculate it as: S = -(sum)p*log2(p) so it is very straightforward
     * to do using the histogram data. (p is the probability of a pixel greyscale value in the image, and (sum) is the
     * greek capital sigma. It is customary to use log in base 2. P.K. Sahoo, S. Soltani, K.C. Wong and, Y.C. Chen "A
     * Survey of Thresholding Techniques", Computer Vision, Graphics, and Image Processing, Vol. 41, pp.233-260, 1988.
     *
     * @param   hist  histogram to be thresholded.
     *
     * @return  index of the maximum entropy split.`
     *
     * @throws  IllegalArgumentException  DOCUMENT ME!
     */
    private int entropySplit(int[] hist) {

        // Normalize histogram, that is makes the sum of all bins equal to 1.
        double sum = 0;

        for (int i = 0; i < hist.length; ++i) {
            sum += hist[i];
        }

        if (sum == 0) {

            // This should not normally happen, but...
            throw new IllegalArgumentException("Empty histogram: sum of all bins is zero.");
        }

        double[] normalizedHist = new double[hist.length];

        for (int i = 0; i < hist.length; i++) {
            normalizedHist[i] = hist[i] / sum;
        }

        //
        double[] pT = new double[hist.length];

        pT[0] = normalizedHist[0];

        for (int i = 1; i < hist.length; i++) {
            pT[i] = pT[i - 1] + normalizedHist[i];
        }

        // Entropy for black and white parts of the histogram
        final double epsilon = Double.MIN_VALUE;
        double[] hB = new double[hist.length];
        double[] hW = new double[hist.length];

        for (int t = 0; t < hist.length; t++) {

            // Black entropy
            if (pT[t] > epsilon) {
                double hhB = 0;

                for (int i = 0; i <= t; i++) {

                    if (normalizedHist[i] > epsilon) {
                        hhB -= normalizedHist[i] / pT[t] * Math.log(normalizedHist[i] / pT[t]);
                    }
                }

                hB[t] = hhB;
            } else {
                hB[t] = 0;
            }

            // White  entropy
            double pTW = 1 - pT[t];

            if (pTW > epsilon) {
                double hhW = 0;

                for (int i = t + 1; i < hist.length; ++i) {

                    if (normalizedHist[i] > epsilon) {
                        hhW -= normalizedHist[i] / pTW * Math.log(normalizedHist[i] / pTW);
                    }
                }

                hW[t] = hhW;
            } else {
                hW[t] = 0;
            }
        }

        // Find histogram index with maximum entropy
        double jMax = hB[0] + hW[0];
        int tMax = 0;

        for (int t = 1; t < hist.length; ++t) {
            double j = hB[t] + hW[t];

            if (j > jMax) {
                jMax = j;
                tMax = t;
            }
        }

        return tMax;
    }

    /**
     * This algorithm is an implementation of Otsu thresholding technique based on the minimization of inter-class
     * variance [otsu79].
     *
     * @param    image        DOCUMENT ME!
     * @param    histoBuffer  DOCUMENT ME!
     *
     * @return   DOCUMENT ME!
     *
     * @Article  {otsu79, author = "N. Otsu", title = "A threshold selection method from gray level histograms", journal
     *           = "{IEEE} Trans. Systems, Man and Cybernetics", year = "1979", volume = "9", pages = "62--66", month =
     *           mar, keywords = "threshold selection", note = "minimize inter class variance", }
     */

    private int otsuThreshold(ModelImage image, int[] histoBuffer) {

        int width = image.getExtents()[0];
        int height = image.getExtents()[1];
        int N = width * height;
        float[] probabilityHistogram;

        probabilityHistogram = new float[histoBuffer.length];

        for (int i = 0; i < probabilityHistogram.length; i++) {
            probabilityHistogram[i] = ((float) histoBuffer[i]) / ((float) N);
        }

        GrayLevelClass C1 = new GrayLevelClass(true, probabilityHistogram);
        GrayLevelClass C2 = new GrayLevelClass(false, probabilityHistogram);

        float fullMu = (C1.getOmega() * C1.getMu()) + (C2.getOmega() * C2.getMu());
        double sigmaMax = 0;
        int threshold = 0;

        /** Start  **/
        double sigma;

        for (int i = 0; i < (probabilityHistogram.length - 1); i++) {

            // double sigma = C1.getOmega() * ( Math.pow( C1.getMu() - fullMu, 2 ) )
            // + C2.getOmega() * ( Math.pow( C2.getMu() - fullMu, 2 ) );

            sigma = (C1.getOmega() * ((C1.getMu() - fullMu) * (C1.getMu() - fullMu))) +
                    (C2.getOmega() * ((C2.getMu() - fullMu) * (C2.getMu() - fullMu)));

            if (sigma > sigmaMax) {
                sigmaMax = sigma;
                threshold = C1.getThreshold();
            }

            C1.addToEnd();
            C2.removeFromBeginning();
        }

        return threshold;
    }

    //~ Inner Classes --------------------------------------------------------------------------------------------------

    /**
     * Used specifally by the Otsu threshold algorithm to define a gray level object for calculating probabilities.
     */
    private class GrayLevelClass {

        /** DOCUMENT ME! */
        private int index;

        /** DOCUMENT ME! */
        private float mu;

        /** DOCUMENT ME! */
        private float omega;

        /** DOCUMENT ME! */
        private float[] probabilityHistogram;

        /**
         * Creates a new GrayLevelClass object.
         *
         * @param  first          DOCUMENT ME!
         * @param  probHistogram  DOCUMENT ME!
         */
        public GrayLevelClass(boolean first, float[] probHistogram) {
            probabilityHistogram = probHistogram;

            if (first) {
                index = 1;
                omega = probabilityHistogram[index - 1];

                if (omega == 0) {
                    mu = 0;
                } else {
                    mu = 1 * probabilityHistogram[index - 1] / omega;
                }
            } else {
                index = 2;
                omega = 0;
                mu = 0;

                for (int i = index; i < probabilityHistogram.length; i++) {
                    omega += probabilityHistogram[i - 1];
                    mu += probabilityHistogram[i - 1] * i;
                }

                if (omega == 0) {
                    mu = 0;
                } else {
                    mu /= omega;
                }
            }
        }

        /**
         * DOCUMENT ME!
         */
        public final void addToEnd() {
            index++;
            mu = 0;
            omega = 0;

            for (int i = 1; i < index; i++) {
                omega += probabilityHistogram[i - 1];
                mu += i * probabilityHistogram[i - 1];
            }

            if (omega == 0) {
                mu = 0;
            } else {
                mu /= omega;
            }
        }

        /**
         * DOCUMENT ME!
         *
         * @return  DOCUMENT ME!
         */
        public final float getMu() {
            return mu;
        }

        /**
         * DOCUMENT ME!
         *
         * @return  DOCUMENT ME!
         */
        public final float getOmega() {
            return omega;
        }

        /**
         * DOCUMENT ME!
         *
         * @return  DOCUMENT ME!
         */
        public final int getThreshold() {
            return index;
        }

        /**
         * DOCUMENT ME!
         */
        public final void removeFromBeginning() {
            index++;
            mu = 0;
            omega = 0;

            for (int i = index; i < probabilityHistogram.length; i++) {
                omega += probabilityHistogram[i - 1];
                mu += i * probabilityHistogram[i - 1]; // i*
            }

            if (omega == 0) {
                mu = 0;
            } else {
                mu /= omega;
            }
        }

        /**
         * DOCUMENT ME!
         *
         * @return  DOCUMENT ME!
         */
        public final String toString() {
            StringBuffer ret = new StringBuffer();

            ret.append("Index : " + index + "\n");
            ret.append("Mu : " + mu + "\n");
            ret.append("Omega : " + omega + "\n");

            return ret.toString();
        }
    }

}
