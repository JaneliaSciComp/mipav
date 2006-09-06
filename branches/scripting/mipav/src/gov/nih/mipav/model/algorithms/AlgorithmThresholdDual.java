package gov.nih.mipav.model.algorithms;


import gov.nih.mipav.model.structures.*;

import java.io.*;


/**
 * This algorithm has two modes of operation:
 *
 * <p>1. Generates binary image based on threshold values passed in by the constructor. Inside the volume of interest:
 * If the image intensity is greater than or equal to the threshold[0] and less than or equal to the threshold[1], the
 * binary image is set to one. Otherwise, it is set to zero. Outside the volume of interest: The binary image is set
 * equal to zero.</p>
 *
 * <p>2. Inside the volume of interest: Preserves all grey scale values from threshold[0] to threshold[1] and sets all
 * values less than the lower threshold and all values greater than the upper threshold to the fillValue supplied to
 * this algorithm via the constructor. Outside the volume of interest: Sets all gray scale values to the fill value.</p>
 *
 * @version  1.0 March 8, 2000
 * @author   Matthew J. McAuliffe, Ph.D.
 */
public class AlgorithmThresholdDual extends AlgorithmBase {

    //~ Instance fields ------------------------------------------------------------------------------------------------

    /** If true produce binary image of threshold region. */
    private boolean binaryFlag;

    /**
     * Flag, if true, indicates that the whole image should be processed. If false on process the image over the mask
     * areas.
     */
    private boolean entireImage;

    /** All values outside the thresholds are set to this value. */
    private float fillValue;

    /**
     * Inverse threshold: true means turn all pixels outside of the lower and upper thresholds to the given fill value,
     * false means turn all data within the lower and upper thresholds to the fill value (fill value can also be binary
     * (0)).
     */
    private boolean isInverse = true;

    /** Array of two thresholds. threshold[0] = Minimum threshold, threshold[1] = Maximum threshold. */
    private float[] threshold;

    //~ Constructors ---------------------------------------------------------------------------------------------------

    /**
     * Creates a new AlgorithmThresholdDual object.
     *
     * @param  srcImg      source image model
     * @param  threshold   array of two thresholds
     * @param  fillValue   all values outside the thresholds are set to this value
     * @param  binaryFlag  if true a binary image is produced, if false gray scale values between the thresholds are
     *                     retained
     * @param  maskFlag    true indicates that the whole image should be processed
     * @param  isInverse   true means turn all pixels outside of the lower and upper thresholds to the given fill value,
     *                     false means turn all data within the lower and upper thresholds to the fill value
     */
    public AlgorithmThresholdDual(ModelImage srcImg, float[] threshold, float fillValue, boolean binaryFlag,
                                  boolean maskFlag, boolean isInverse) {

        super(null, srcImg);

        this.threshold = threshold;
        this.fillValue = fillValue;
        this.binaryFlag = binaryFlag;
        entireImage = maskFlag;
        this.isInverse = isInverse;

        if (entireImage == false) {
            mask = srcImage.generateVOIMask();
        }
    }

    /**
     * Creates a new AlgorithmThresholdDual object.
     *
     * @param  destImg     image model where result image is to be stored
     * @param  srcImg      source image model
     * @param  threshold   array of two thresholds
     * @param  fillValue   all values outside the thresholds are set to this value
     * @param  binaryFlag  if true a binary image is produced, if false gray scale values between the thresholds are
     *                     retained
     * @param  maskFlag    true indicates that the whole image should be processed
     * @param  isInverse   true means turn all pixels outside of the lower and upper thresholds to the given fill value,
     *                     false means turn all data within the lower and upper thresholds to the fill value
     */
    public AlgorithmThresholdDual(ModelImage destImg, ModelImage srcImg, float[] threshold, float fillValue,
                                  boolean binaryFlag, boolean maskFlag, boolean isInverse) {

        super(destImg, srcImg);

        this.threshold = threshold;
        this.fillValue = fillValue;
        this.binaryFlag = binaryFlag;
        entireImage = maskFlag;
        this.isInverse = isInverse;

        if (entireImage == false) {
            mask = srcImage.generateVOIMask();
        }
    }

    //~ Methods --------------------------------------------------------------------------------------------------------

    /**
     * Prepares this class for destruction.
     */
    public void finalize() {
        threshold = null;
        destImage = null;
        srcImage = null;
        super.finalize();
    }

    /**
     * Starts the program.
     */
    public void runAlgorithm() {

        if (srcImage == null) {
            displayError("Source Image is null");
            setCompleted(false);

            return;
        }

        constructLog();

        if (destImage != null) {

            if ((destImage.getType() != ModelImage.BOOLEAN) && (binaryFlag == true)) {
                destImage.reallocate(ModelStorageBase.BOOLEAN);
            }

            if (srcImage.getNDims() == 2) {
                calcStoreInDest2D();
            } else if (srcImage.getNDims() > 2) {
                calcStoreInDest34D();
            }
        } else {

            if (srcImage.getNDims() == 2) {
                calcInPlace2D();
            } else if (srcImage.getNDims() > 2) {
                calcInPlace34D();
            }
        }
    }

    /**
     * Replace 2D source image with the thresholded image.
     */
    private void calcInPlace2D() {

        int i;
        int length;
        float[] buffer;

        try {
            length = srcImage.getSliceSize();
            buffer = new float[length];
            srcImage.exportData(0, length, buffer); // locks and releases lock
            buildProgressBar(srcImage.getImageName(), "Thresholding image ...", 0, 100);
        } catch (IOException error) {
            buffer = null;
            errorCleanUp("Algorithm Threshold: Image(s) locked", true);

            return;
        } catch (OutOfMemoryError e) {
            buffer = null;
            errorCleanUp("Algorithm Threshold: Out of Memory", true);

            return;
        }

        int mod = length / 100; // mod is 1 percent of length

        

        for (i = 0; (i < length) && !threadStopped; i++) {

            if (((i % mod) == 0) && isProgressBarVisible()) {
                progressBar.updateValue(Math.round((float) i / (length - 1) * 100), runningInSeparateThread);
            }

            if (isInverse) {

                if (((entireImage == true) || mask.get(i)) &&
                        ((buffer[i] >= threshold[0]) && (buffer[i] <= threshold[1]))) {

                    if (binaryFlag == true) {
                        buffer[i] = 1;
                    }
                } else {

                    if (binaryFlag == true) {
                        buffer[i] = 0;
                    } else {
                        buffer[i] = fillValue;
                    }
                }
            } else {

                if (((entireImage == true) || mask.get(i)) &&
                        ((buffer[i] < threshold[0]) || (buffer[i] > threshold[1]))) {

                    if (binaryFlag == true) {
                        buffer[i] = 1;
                    }
                } else {

                    if (binaryFlag == true) {
                        buffer[i] = 0;
                    } else {
                        buffer[i] = fillValue;
                    }
                }

            }
        }

        if (threadStopped) {
            finalize();

            return;
        }

        try {

            if ((binaryFlag == true) && (srcImage.getType() != ModelImage.BOOLEAN)) {
                srcImage.reallocate(ModelImage.BOOLEAN);
            }

            srcImage.importData(0, buffer, true);
        } catch (IOException error) {
            buffer = null;
            errorCleanUp("Algorithm Threshold: Image(s) locked", true);

            return;
        }

        disposeProgressBar();
        setCompleted(true);
    }

    /**
     * Replace 3D or 4D source image with the thresholded image.
     */
    private void calcInPlace34D() {

        int i;
        int length;
        float[] buffer;

        try {
            length = srcImage.getSliceSize() * srcImage.getExtents()[2];

            if (srcImage.getNDims() == 4) {
                length = length * srcImage.getExtents()[3];
            }

            buffer = new float[length];
            srcImage.exportData(0, length, buffer); // locks and releases lock
            buildProgressBar(srcImage.getImageName(), "Thresholding image ...", 0, 100);
        } catch (IOException error) {
            buffer = null;
            errorCleanUp("Algorithm Threshold: Image(s) locked", true);

            return;
        } catch (OutOfMemoryError e) {
            buffer = null;
            errorCleanUp("Algorithm Threshold: Out of Memory", true);

            return;
        }

        int mod = length / 100; // mod is 1 percent of length

        

        for (i = 0; (i < length) && !threadStopped; i++) {

            if (((i % mod) == 0) && isProgressBarVisible()) {
                progressBar.updateValue(Math.round((float) i / (length - 1) * 100), runningInSeparateThread);
            }

            if (isInverse) {

                if (((entireImage == true) || mask.get(i)) &&
                        ((buffer[i] >= threshold[0]) && (buffer[i] <= threshold[1]))) {

                    if (binaryFlag == true) {
                        buffer[i] = 1;
                    }
                } else {

                    if (binaryFlag == true) {
                        buffer[i] = 0;
                    } else {
                        buffer[i] = fillValue;
                    }
                }
            } else {

                if (((entireImage == true) || mask.get(i)) &&
                        ((buffer[i] < threshold[0]) || (buffer[i] > threshold[1]))) {

                    if (binaryFlag == true) {
                        buffer[i] = 1;
                    }
                } else {

                    if (binaryFlag == true) {
                        buffer[i] = 0;
                    } else {
                        buffer[i] = fillValue;
                    }
                }

            }
        }

        if (threadStopped) {
            finalize();

            return;
        }

        try {

            if ((binaryFlag == true) && (srcImage.getType() != ModelImage.BOOLEAN)) {
                srcImage.reallocate(ModelImage.BOOLEAN);
            }

            srcImage.importData(0, buffer, true);
        } catch (IOException error) {
            buffer = null;
            errorCleanUp("Algorithm Threshold: Image(s) locked", true);

            return;
        }

        disposeProgressBar();
        setCompleted(true);

    }

    /**
     * Stores the thresholded image into the destination image.
     */
    private void calcStoreInDest2D() {

        int i;
        int length;
        float[] buffer;

        try {
            destImage.setLock();
        } catch (IOException error) {
            displayError("Algorithm Threshold: Image(s) locked");
            setCompleted(false);

            return;
        }

        try {
            length = srcImage.getSliceSize();
            buffer = new float[length];
            srcImage.exportData(0, length, buffer); // locks and releases lock
            buildProgressBar(srcImage.getImageName(), "Thresholding image ...", 0, 100);
        } catch (IOException error) {
            buffer = null;
            errorCleanUp("Algorithm Threshold: Image(s) locked", true);

            return;
        } catch (OutOfMemoryError e) {
            buffer = null;
            errorCleanUp("Algorithm Threshold: Out of Memory", true);

            return;
        }

        int mod = length / 100; // mod is 1 percent of length

        

        for (i = 0; (i < length) && !threadStopped; i++) {

            if (((i % mod) == 0) && isProgressBarVisible()) {
                progressBar.updateValue(Math.round((float) i / (length - 1) * 100), runningInSeparateThread);
            }

            if (isInverse) {

                if (((entireImage == true) || mask.get(i)) &&
                        ((buffer[i] >= threshold[0]) && (buffer[i] <= threshold[1]))) {

                    if (binaryFlag == true) {
                        destImage.set(i, 1);
                    } else {
                        destImage.set(i, buffer[i]);
                    }
                } else {

                    if (binaryFlag == true) {
                        destImage.set(i, 0);
                    } else {
                        destImage.set(i, fillValue);
                    }
                }
            } else {

                if (((entireImage == true) || mask.get(i)) &&
                        ((buffer[i] < threshold[0]) || (buffer[i] > threshold[1]))) {

                    if (binaryFlag == true) {
                        destImage.set(i, 1);
                    } else {
                        destImage.set(i, buffer[i]);
                    }
                } else {

                    if (binaryFlag == true) {
                        destImage.set(i, 0);
                    } else {
                        destImage.set(i, fillValue);
                    }
                }

            }
        }

        if (threadStopped) {
            finalize();

            return;
        }

        destImage.calcMinMax();
        destImage.releaseLock();
        disposeProgressBar();
        setCompleted(true);
    }

    /**
     * Stores the thresholded image into the destination image.
     */
    private void calcStoreInDest34D() {

        int i;
        int length;
        float[] buffer;

        try {
            destImage.setLock();
        } catch (IOException error) {
            displayError("Algorithm Threshold: Image(s) locked");
            setCompleted(false);

            return;
        }

        try {
            length = srcImage.getSliceSize() * srcImage.getExtents()[2];

            if (srcImage.getNDims() == 4) {
                length = length * srcImage.getExtents()[3];
            }

            buffer = new float[length];
            srcImage.exportData(0, length, buffer); // locks and releases lock
            buildProgressBar(srcImage.getImageName(), "Thresholding image ...", 0, 100);
        } catch (IOException error) {
            buffer = null;
            errorCleanUp("Algorithm Threshold: Image(s) locked", true);

            return;
        } catch (OutOfMemoryError e) {
            buffer = null;
            errorCleanUp("Algorithm Threshold: Out of Memory", true);

            return;
        }

        int mod = length / 100; // mod is 1 percent of length

        

        for (i = 0; (i < length) && !threadStopped; i++) {

            if (((i % mod) == 0) && isProgressBarVisible()) {
                progressBar.updateValue(Math.round((float) i / (length - 1) * 100), runningInSeparateThread);
            }

            if (isInverse) {

                if (((entireImage == true) || mask.get(i)) &&
                        ((buffer[i] >= threshold[0]) && (buffer[i] <= threshold[1]))) {

                    if (binaryFlag == true) {
                        destImage.set(i, 1);
                    } else {
                        destImage.set(i, buffer[i]);
                    }
                } else {

                    if (binaryFlag == true) {
                        destImage.set(i, 0);
                    } else {
                        destImage.set(i, fillValue);
                    }
                }
            } else {

                if (((entireImage == true) || mask.get(i)) &&
                        ((buffer[i] < threshold[0]) || (buffer[i] > threshold[1]))) {

                    if (binaryFlag == true) {
                        destImage.set(i, 1);
                    } else {
                        destImage.set(i, buffer[i]);
                    }
                } else {

                    if (binaryFlag == true) {
                        destImage.set(i, 0);
                    } else {
                        destImage.set(i, fillValue);
                    }
                }

            }
        }

        if (threadStopped) {
            finalize();

            return;
        }

        destImage.calcMinMax();
        destImage.releaseLock();
        disposeProgressBar();
        setCompleted(true);
    }

    /**
     * Constructs a string of the contruction parameters and outputs the string to the messsage frame if the logging
     * procedure is turned on.
     */
    private void constructLog() {
        historyString = new String("Threshold(" + String.valueOf(threshold[0]) + ", " + String.valueOf(threshold[1]) +
                                   ", " + String.valueOf(fillValue) + ", " + String.valueOf(binaryFlag) + ", " +
                                   String.valueOf(entireImage) + ")\n");
    }
}
