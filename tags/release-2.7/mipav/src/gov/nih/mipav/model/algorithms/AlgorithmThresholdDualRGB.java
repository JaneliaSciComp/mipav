package gov.nih.mipav.model.algorithms;


import gov.nih.mipav.model.structures.*;

import java.io.*;


/**
 * Runs threshold (lower and upper) on color images and replaces values either outside (inverse) or inside (normal) with
 * the fill values specified.
 *
 * @author   Ben Link
 * @version  1.0
 */
public class AlgorithmThresholdDualRGB extends AlgorithmBase {

    //~ Instance fields ------------------------------------------------------------------------------------------------

    /**
     * Flag, if true, indicates that the whole image should be processed. If false on process the image over the mask
     * areas.
     */
    private boolean entireImage;

    /** Arrays containing fill values for each channel (red=0,green=1,blue=2). */
    private float[] fillValues;

    /** fill values are for values outside (inverse) or inside (normal/false). */
    private boolean isInverse;

    /** DOCUMENT ME! */
    private float[] thresholdB;

    /** DOCUMENT ME! */
    private float[] thresholdG;

    /** Three arrays (R,G,B) containing minimum and maximum threshold for each channel. */
    private float[] thresholdR;

    /** Which channels to use (0 = red, 1 = green, 2 = blue). */
    private boolean[] useChannels;

    //~ Constructors ---------------------------------------------------------------------------------------------------

    /**
     * Default constructor for running algorithm on and modifying source image.
     *
     * @param  srcImage     ModelImage source
     * @param  thresholdR   float[] red thresh
     * @param  thresholdG   float[] green thresh
     * @param  thresholdB   float[] blue thresh
     * @param  fillValues   float[] fill values [3] (r,g,b)
     * @param  useChannels  boolean[] use channels [3] (r,g,b)
     * @param  maskFlag     boolean whole image or voi
     * @param  isInverse    boolean replace values outside (true) or inside (false)
     */
    public AlgorithmThresholdDualRGB(ModelImage srcImage, float[] thresholdR, float[] thresholdG, float[] thresholdB,
                                     float[] fillValues, boolean[] useChannels, boolean maskFlag, boolean isInverse) {
        super(null, srcImage);
        this.thresholdR = thresholdR;
        this.thresholdG = thresholdG;
        this.thresholdB = thresholdB;
        this.fillValues = fillValues;
        this.useChannels = useChannels;
        entireImage = maskFlag;
        this.isInverse = isInverse;

        if (entireImage == false) {
            mask = srcImage.generateVOIMask();
        }
    }

    /**
     * Default constructor for running algorithm into a destination image.
     *
     * @param  destImg      ModelImage destination
     * @param  srcImg       ModelImage source
     * @param  thresholdR   float[] red thresh
     * @param  thresholdG   float[] green thresh
     * @param  thresholdB   float[] blue thresh
     * @param  fillValues   float[] fill values [3] (r,g,b)
     * @param  useChannels  boolean[] use channels [3] (r,g,b)
     * @param  maskFlag     boolean whole image or voi
     * @param  isInverse    boolean replace values outside (true) or inside (false)
     */
    public AlgorithmThresholdDualRGB(ModelImage destImg, ModelImage srcImg, float[] thresholdR, float[] thresholdG,
                                     float[] thresholdB, float[] fillValues, boolean[] useChannels, boolean maskFlag,
                                     boolean isInverse) {
        super(destImg, srcImg);
        this.thresholdR = thresholdR;
        this.thresholdG = thresholdG;
        this.thresholdB = thresholdB;
        this.fillValues = fillValues;
        this.useChannels = useChannels;
        this.isInverse = isInverse;
        entireImage = maskFlag;

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
            length = srcImage.getSliceSize() * 4;
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

        initProgressBar();

        for (i = 0; (i < length) && !threadStopped; i++) {

            if (((i % mod) == 0) && isProgressBarVisible()) {
                progressBar.updateValue(Math.round((float) i / (length - 1) * 100), runningInSeparateThread);
            }

            // alpha channel.... don't touch
            if ((i % 4) == 0) { }
            else if ((i % 4) == 1) { // red channel

                if (useChannels[0]) {

                    if (isInverse) {

                        if (((entireImage == true) || mask.get(i / 4)) &&
                                ((buffer[i] >= thresholdR[0]) && (buffer[i] <= thresholdR[1]))) { }
                        else {
                            buffer[i] = fillValues[0];
                        }
                    } else {

                        if (((entireImage == true) || mask.get(i / 4)) &&
                                ((buffer[i] < thresholdR[0]) || (buffer[i] > thresholdR[1]))) { }
                        else {
                            buffer[i] = fillValues[0];
                        }
                    }
                }
            } else if ((i % 4) == 2) { // green channel

                if (useChannels[1]) {

                    if (isInverse) {

                        if (((entireImage == true) || mask.get(i / 4)) &&
                                ((buffer[i] >= thresholdG[0]) && (buffer[i] <= thresholdG[1]))) { }
                        else {
                            buffer[i] = fillValues[1];
                        }
                    } else {

                        if (((entireImage == true) || mask.get(i / 4)) &&
                                ((buffer[i] < thresholdG[0]) || (buffer[i] > thresholdG[1]))) { }
                        else {
                            buffer[i] = fillValues[1];
                        }
                    }
                }
            } else if ((i % 4) == 3) { // blue channel

                if (useChannels[2]) {

                    if (isInverse) {

                        if (((entireImage == true) || mask.get(i / 4)) &&
                                ((buffer[i] >= thresholdB[0]) && (buffer[i] <= thresholdB[1]))) { }
                        else {
                            buffer[i] = fillValues[2];
                        }
                    } else {

                        if (((entireImage == true) || mask.get(i / 4)) &&
                                ((buffer[i] < thresholdB[0]) || (buffer[i] > thresholdB[1]))) { }
                        else {
                            buffer[i] = fillValues[2];
                        }
                    }
                }
            }
        }

        if (threadStopped) {
            finalize();

            return;
        }

        try {
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
            length = srcImage.getSliceSize() * srcImage.getExtents()[2] * 4;

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

        initProgressBar();

        for (i = 0; (i < length) && !threadStopped; i++) {

            if (((i % mod) == 0) && isProgressBarVisible()) {
                progressBar.updateValue(Math.round((float) i / (length - 1) * 100), runningInSeparateThread);
                // alpha channel.... don't touch
            }

            if ((i % 4) == 0) { }
            else if ((i % 4) == 1) { // red channel

                if (useChannels[0]) {

                    if (isInverse) {

                        if (((entireImage == true) || mask.get(i / 4)) &&
                                ((buffer[i] >= thresholdR[0]) && (buffer[i] <= thresholdR[1]))) { }
                        else {
                            buffer[i] = fillValues[0];
                        }
                    } else {

                        if (((entireImage == true) || mask.get(i / 4)) &&
                                ((buffer[i] < thresholdR[0]) || (buffer[i] > thresholdR[1]))) { }
                        else {
                            buffer[i] = fillValues[0];
                        }

                    }
                }
            } else if ((i % 4) == 2) { // green channel

                if (useChannels[1]) {

                    if (isInverse) {

                        if (((entireImage == true) || mask.get(i / 4)) &&
                                ((buffer[i] >= thresholdG[0]) && (buffer[i] <= thresholdG[1]))) { }
                        else {
                            buffer[i] = fillValues[1];
                        }
                    } else {

                        if (((entireImage == true) || mask.get(i / 4)) &&
                                ((buffer[i] < thresholdG[0]) || (buffer[i] > thresholdG[1]))) { }
                        else {
                            buffer[i] = fillValues[1];
                        }
                    }
                }

            } else if ((i % 4) == 3) { // blue channel

                if (useChannels[2]) {

                    if (isInverse) {

                        if (((entireImage == true) || mask.get(i / 4)) &&
                                ((buffer[i] >= thresholdB[0]) && (buffer[i] <= thresholdB[1]))) { }
                        else {
                            buffer[i] = fillValues[2];
                        }
                    } else {

                        if (((entireImage == true) || mask.get(i / 4)) &&
                                ((buffer[i] < thresholdB[0]) || (buffer[i] > thresholdB[1]))) { }
                        else {
                            buffer[i] = fillValues[2];
                        }
                    }
                }
            }

        }

        if (threadStopped) {
            finalize();

            return;
        }

        try {
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

        System.err.println("IS INVERSE: " + isInverse);

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
            length = srcImage.getSliceSize() * 4;
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

        initProgressBar();

        for (i = 0; (i < length) && !threadStopped; i++) {

            if (((i % mod) == 0) && isProgressBarVisible()) {
                progressBar.updateValue(Math.round((float) i / (length - 1) * 100), runningInSeparateThread);
            }

            // alpha channel.... don't touch
            if ((i % 4) == 0) {
                destImage.set(i, buffer[i]);
            } else if ((i % 4) == 1) { // red channel

                if (useChannels[0]) {

                    if (isInverse) {

                        if (((entireImage == true) || mask.get(i / 4)) &&
                                ((buffer[i] >= thresholdR[0]) && (buffer[i] <= thresholdR[1]))) {
                            destImage.set(i, buffer[i]);
                        } else {
                            destImage.set(i, fillValues[0]);
                        }
                    } else {

                        if (((entireImage == true) || mask.get(i / 4)) &&
                                ((buffer[i] < thresholdR[0]) || (buffer[i] > thresholdR[1]))) {
                            destImage.set(i, buffer[i]);
                        } else {
                            destImage.set(i, fillValues[0]);
                        }
                    }
                } else {
                    destImage.set(i, buffer[i]);
                }
            } else if ((i % 4) == 2) { // green channel

                if (useChannels[1]) {

                    if (isInverse) {

                        if (((entireImage == true) || mask.get(i / 4)) &&
                                ((buffer[i] >= thresholdG[0]) && (buffer[i] <= thresholdG[1]))) {
                            destImage.set(i, buffer[i]);
                        } else {
                            destImage.set(i, fillValues[1]);
                        }
                    } else {

                        if (((entireImage == true) || mask.get(i / 4)) &&
                                ((buffer[i] < thresholdG[0]) || (buffer[i] > thresholdG[1]))) {
                            destImage.set(i, buffer[i]);
                        } else {
                            destImage.set(i, fillValues[1]);
                        }
                    }
                } else {
                    destImage.set(i, buffer[i]);
                }
            } else if ((i % 4) == 3) { // blue channel

                if (useChannels[2]) {

                    if (isInverse) {

                        if (((entireImage == true) || mask.get(i / 4)) &&
                                ((buffer[i] >= thresholdB[0]) && (buffer[i] <= thresholdB[1]))) {
                            destImage.set(i, buffer[i]);
                        } else {
                            destImage.set(i, fillValues[2]);
                        }
                    } else {

                        if (((entireImage == true) || mask.get(i / 4)) &&
                                ((buffer[i] < thresholdB[0]) || (buffer[i] > thresholdB[1]))) {
                            destImage.set(i, buffer[i]);
                        } else {
                            destImage.set(i, fillValues[2]);
                        }
                    }
                } else {
                    destImage.set(i, buffer[i]);
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
            length = srcImage.getSliceSize() * srcImage.getExtents()[2] * 4;

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

        initProgressBar();

        for (i = 0; (i < length) && !threadStopped; i++) {

            if (((i % mod) == 0) && isProgressBarVisible()) {
                progressBar.updateValue(Math.round((float) i / (length - 1) * 100), runningInSeparateThread);
            }

            if ((i % 4) == 0) {
                destImage.set(i, buffer[i]);
            } else if ((i % 4) == 1) { // red channel

                if (useChannels[0]) {

                    if (isInverse) {

                        if (((entireImage == true) || mask.get(i / 4)) &&
                                ((buffer[i] >= thresholdR[0]) && (buffer[i] <= thresholdR[1]))) {
                            destImage.set(i, buffer[i]);
                        } else {
                            destImage.set(i, fillValues[0]);
                        }
                    } else {

                        if (((entireImage == true) || mask.get(i / 4)) &&
                                ((buffer[i] < thresholdR[0]) || (buffer[i] > thresholdR[1]))) {
                            destImage.set(i, buffer[i]);
                        } else {
                            destImage.set(i, fillValues[0]);
                        }
                    }
                } else {
                    destImage.set(i, buffer[i]);
                }
            } else if ((i % 4) == 2) { // green channel

                if (useChannels[1]) {

                    if (isInverse) {

                        if (((entireImage == true) || mask.get(i / 4)) &&
                                ((buffer[i] >= thresholdG[0]) && (buffer[i] <= thresholdG[1]))) {
                            destImage.set(i, buffer[i]);
                        } else {
                            destImage.set(i, fillValues[1]);
                        }
                    } else {

                        if (((entireImage == true) || mask.get(i / 4)) &&
                                ((buffer[i] < thresholdG[0]) || (buffer[i] > thresholdG[1]))) {
                            destImage.set(i, buffer[i]);
                        } else {
                            destImage.set(i, fillValues[1]);
                        }
                    }
                } else {
                    destImage.set(i, buffer[i]);
                }
            } else if ((i % 4) == 3) { // blue channel

                if (useChannels[2]) {

                    if (isInverse) {

                        if (((entireImage == true) || mask.get(i / 4)) &&
                                ((buffer[i] >= thresholdB[0]) && (buffer[i] <= thresholdB[1]))) {
                            destImage.set(i, buffer[i]);
                        } else {
                            destImage.set(i, fillValues[2]);
                        }
                    } else {

                        if (((entireImage == true) || mask.get(i / 4)) &&
                                ((buffer[i] < thresholdB[0]) || (buffer[i] > thresholdB[1]))) {
                            destImage.set(i, buffer[i]);
                        } else {
                            destImage.set(i, fillValues[2]);
                        }
                    }
                } else {
                    destImage.set(i, buffer[i]);
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
        historyString = new String("Threshold(" + String.valueOf(thresholdR[0]) + ", " + String.valueOf(thresholdR[1]) +
                                   ", " + String.valueOf(thresholdG[0]) + ", " + String.valueOf(thresholdG[1]) + ", " +
                                   String.valueOf(thresholdB[0]) + ", " + String.valueOf(thresholdB[1]) + ", " +
                                   String.valueOf(fillValues[0]) + ", " + String.valueOf(entireImage) + ")\n");
    }
}
