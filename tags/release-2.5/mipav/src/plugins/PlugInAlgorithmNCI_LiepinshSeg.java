import gov.nih.mipav.model.algorithms.*;
import gov.nih.mipav.model.structures.*;

import java.io.*;

import java.util.*;


/**
 * Simple algorithm that.
 *
 * @version  1.0 Dec 30, 2002
 * @author   Matthew J. McAuliffe, Ph.D.
 */

public class PlugInAlgorithmNCI_LiepinshSeg extends AlgorithmBase {

    //~ Instance fields ------------------------------------------------------------------------------------------------

    /** DOCUMENT ME! */
    private float blueChannelThresholdValue;

    /** DOCUMENT ME! */
    private BitSet mask = null;

    //~ Constructors ---------------------------------------------------------------------------------------------------

    /**
     * Creates a new PlugInAlgorithmNCI_LiepinshSeg object.
     *
     * @param  destImg  image model where result image is to stored
     * @param  srcImg   source image model
     */
    public PlugInAlgorithmNCI_LiepinshSeg(ModelImage destImg, ModelImage srcImg) {
        super(destImg, srcImg);
        mask = srcImage.generateVOIMask();
    }

    /**
     * Creates a new PlugInAlgorithmNCI_LiepinshSeg object.
     *
     * @param  destImg     image model where result image is to stored
     * @param  srcImg      source image model
     * @param  redValue    DOCUMENT ME!
     * @param  greenValue  DOCUMENT ME!
     * @param  blueValue   DOCUMENT ME!
     */
    public PlugInAlgorithmNCI_LiepinshSeg(ModelImage destImg, ModelImage srcImg, float redValue, float greenValue,
                                          float blueValue) {
        super(destImg, srcImg);
        mask = srcImage.generateVOIMask();
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

        constructLog();
        calcStoreInDest();
    }

    /**
     * set the blue concentrated threshold value.
     *
     * @param  blueThreshold  blue threshold value.
     */
    public void setBlueChannelThreshold(float blueThreshold) {
        blueChannelThresholdValue = blueThreshold;
    }


    /**
     * Calculates the gray scale image.
     */
    private void calcStoreInDest() {

        int i, m, k, j;
        int f, t, z;
        int id;
        int offsetIn, offsetOut;
        int lengthIn, lengthOut; // total number of data-elements (pixels) in image
        float[] buffer; // data-buffer (for pixel data) which is the "heart" of the image\
        float[] bufferDest;


        int xDim = srcImage.getExtents()[0];
        int yDim = srcImage.getExtents()[1];

        // mask = srcImage.generateVOIMask();

        try {
            lengthIn = 4 * xDim * yDim;
            lengthOut = xDim * yDim;
            buffer = new float[lengthIn];
            bufferDest = new float[srcImage.getSliceSize()];
            buildProgressBar(srcImage.getImageName(), "Processing image ...", 0, 100);
        } catch (OutOfMemoryError e) {
            buffer = null;
            bufferDest = null;
            System.gc();
            displayError("Algorithm NCI-Liepinsh reports: Out of memory when creating image buffer");
            setCompleted(false);

            return;
        }

        initProgressBar();

        int mod = lengthOut / 20;

        if (srcImage.getNDims() == 5) {
            f = srcImage.getExtents()[4];
        } else {
            f = 1;
        }

        if (srcImage.getNDims() >= 4) {
            t = srcImage.getExtents()[3];
        } else {
            t = 1;
        }

        if (srcImage.getNDims() >= 3) {
            z = srcImage.getExtents()[2];
        } else {
            z = 1;
        }

        int maskCnt = 0;
        int totalLength = f * t * z * lengthIn;

        for (m = 0; (m < f) && !threadStopped; m++) {

            for (k = 0; (k < t) && !threadStopped; k++) {

                for (j = 0; (j < z) && !threadStopped; j++) {

                    try {
                        offsetIn = (m * k * j * lengthIn) + (k * j * lengthIn) + (j * lengthIn);
                        offsetOut = (m * k * j * lengthOut) + (k * j * lengthOut) + (j * lengthOut);
                        srcImage.exportData(offsetIn, lengthIn, buffer); // locks and releases lock
                    } catch (IOException error) {
                        buffer = null;
                        bufferDest = null;
                        displayError("Algorithm folicle Segmenation : Input Image(s) locked");
                        setCompleted(false);

                        return;
                    }

                    for (i = 0, id = 0; (i < lengthIn) && !threadStopped; i += 4, id++) {

                        if (mask.get(id)) {
                            maskCnt++;

                            if (((id % mod) == 0) && isProgressBarVisible()) {
                                progressBar.updateValue(Math.round((float) (i + offsetIn) / (totalLength - 1) * 100),
                                                        activeImage);
                            }

                            if ((buffer[i + 3] > buffer[i + 2]) && (buffer[i + 3] > buffer[i + 1])) {
                                float diffBG = Math.abs(buffer[i + 3] - buffer[i + 2]);
                                float diffBR = Math.abs(buffer[i + 3] - buffer[i + 1]);

                                if ((diffBR > blueChannelThresholdValue) && (diffBG > blueChannelThresholdValue)) {
                                    bufferDest[id] = 50;
                                }
                                // bufferDest[id] = diffBG + diffBR;
                            }

                            if ((bufferDest[id] != 50) && (buffer[i + 1] >= buffer[i + 2]) &&
                                    (buffer[i + 1] >= buffer[i + 3])) {
                                float diffBG = Math.abs(buffer[i + 1] - buffer[i + 2]);
                                float diffBR = Math.abs(buffer[i + 1] - buffer[i + 3]);

                                // if ( (diffBR > 1) && (diffBG > 1) ) {
                                bufferDest[id] = 100;
                            }

                            /*
                             * if ( bufferDest[id] != 50 && bufferDest[id] != 100 && (buffer[i+1] > 10) &&
                             * (buffer[i+2] > 10) && (buffer[i+3] > 10) ) { //System.out.println("); if (
                             * (Math.abs(buffer[i + 3] - buffer[i + 2]) +      Math.abs(buffer[i + 3] - buffer[i + 1]) +
                             *      Math.abs(buffer[i + 2] - buffer[i + 1])) < 50) {
                             *
                             *  if ( Math.abs(buffer[i + 3] - buffer[i + 2]) < 25 &&         Math.abs(buffer[i + 3] -
                             * buffer[i + 1]) < 25 &&         Math.abs(buffer[i + 2] - buffer[i + 1]) < 25) {
                             *
                             *      bufferDest[id] = 255;    } } }
                             */
                        } else {
                            bufferDest[id] = 0;
                        }
                    }

                    float sum = 0;
                    int counter50 = 0;
                    int counter100 = 0;

                    for (i = xDim; (i < (bufferDest.length - xDim)) && !threadStopped; i++) {

                        if (bufferDest[i] != 0) {
                            sum += bufferDest[i];

                            if (bufferDest[i] == 50) {
                                counter50++;
                            } else if (bufferDest[i] == 100) {
                                counter100++;
                            }
                        }
                    }

                    if (threadStopped) {
                        buffer = null;
                        bufferDest = null;
                        finalize();

                        return;
                    }

                    float bluePercent = ((counter50 / (float) maskCnt) * 100);
                    float brownPercent = ((counter100 / (float) maskCnt) * 100);
                    destImage.getUserInterface().getMessageFrame().getData().append(destImage.getImageName() + "\n");
                    destImage.getUserInterface().getMessageFrame().getData().append("Pixels with blue label  = " +
                                                                                    counter50 + "   total pixels " +
                                                                                    maskCnt + "   percent pixels = " +
                                                                                    bluePercent + "\n");
                    destImage.getUserInterface().getMessageFrame().getData().append("Pixels with brown label  = " +
                                                                                    counter100 + "   total pixels " +
                                                                                    maskCnt + "   percent pixels = " +
                                                                                    brownPercent + "\n");
                    destImage.getUserInterface().getMessageFrame().getData().append("Blue / Brown ratio of percents  = " +
                                                                                    (bluePercent / brownPercent) +
                                                                                    "\n\n");

                    try {
                        destImage.importData(offsetOut, bufferDest, false);
                    } catch (IOException error) {
                        displayError("Algorithm NCI_Liepinsh: Output Image(s) locked");
                        setCompleted(false);
                        disposeProgressBar();

                        return;
                    }
                }
            } // t loop
        } // f loop

        if (threadStopped) {
            buffer = null;
            bufferDest = null;
            finalize();

            return;
        }

        destImage.calcMinMax();
        disposeProgressBar();
        setCompleted(true);
    }

    /**
     * Constructs a string of the contruction parameters and out puts the string to the messsage frame if the logging
     * procedure is turned on.
     */
    private void constructLog() {
        historyString = new String("Eye Segmentation()\n");
    }


}
