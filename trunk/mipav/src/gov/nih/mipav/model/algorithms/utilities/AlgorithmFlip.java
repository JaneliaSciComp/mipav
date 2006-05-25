package gov.nih.mipav.model.algorithms.utilities;


import gov.nih.mipav.model.algorithms.*;
import gov.nih.mipav.model.file.*;
import gov.nih.mipav.model.structures.*;

import gov.nih.mipav.view.*;

import java.io.*;


/**
 * Flips 2D, 3D or 4D grays scale or color dataset about X or Y axis.
 *
 * @version  1.0 July 14, 2000
 * @author   Matthew J. McAuliffe, Ph.D.
 */

public class AlgorithmFlip extends AlgorithmBase {

    //~ Static fields/initializers -------------------------------------------------------------------------------------

    /** Flip along Y axis. */
    public static final int Y_AXIS = 0;

    /** Flip along X axis. */
    public static final int X_AXIS = 1;

    /** Flip along Z axis. */
    public static final int Z_AXIS = 2;

    //~ Instance fields ------------------------------------------------------------------------------------------------

    /** Axis to flip along. */
    private int flipAxis = Y_AXIS;

    //~ Constructors ---------------------------------------------------------------------------------------------------

    /**
     * Flips 2D, 3D or 4D grays scale or color dataset about X or Y axis.
     *
     * @param  srcImg    source image model
     * @param  flipMode  flip about which axis
     */
    public AlgorithmFlip(ModelImage srcImg, int flipMode) {
        super(null, srcImg);

        if ((flipMode == Y_AXIS) || (flipMode == X_AXIS) || (flipMode == Z_AXIS)) {
            flipAxis = flipMode;
        } else {
            flipAxis = Y_AXIS;
        }
    }

    /**
     * Flips 2D, 3D or 4D grays scale or color dataset about X or Y axis.
     *
     * @param  srcImg    source image model
     * @param  flipMode  flip about which axis
     * @param  progress  mode of progress bar (see AlgorithmBase)
     */
    public AlgorithmFlip(ModelImage srcImg, int flipMode, int progress) {
        this(srcImg, flipMode);
        progressMode = progress;
    }

    //~ Methods --------------------------------------------------------------------------------------------------------

    /**
     * Prepares this class for destruction.
     */
    public void finalize() {
        super.finalize();
    }


    /**
     * Runs the flip algorithm.
     */
    public void runAlgorithm() {

        if (srcImage == null) {
            displayError("Source Image is null");

            return;
        }

        constructLog();

        if (srcImage.getNDims() == 2) {
            calcInPlace(1);
        } else if (srcImage.getNDims() == 3) {
            calcInPlace(srcImage.getExtents()[2]);
        } else if (srcImage.getNDims() == 4) {
            calcInPlace(srcImage.getExtents()[2] * srcImage.getExtents()[3]);
        }
    }

    /**
     * Generates the flipped image and replaces the source image with the flippeded image.
     *
     * @param  nImages  Number of images to be flipped. If 2D image then nImage = 1, if 3D or 4D image where each image
     *                  is to processed independently then nImages equals the number of images in the volume.
     */
    private void calcInPlace(int nImages) {
        int x, y, j, index, s;
        int length, totalLength;
        int start;
        float[] buffer;
        float[] resultBuffer;
        int xDim = srcImage.getExtents()[0];
        int yDim = srcImage.getExtents()[1];
        int cf;
        boolean logMagDisplay = false;

        try {

            if (srcImage.isColorImage()) {
                cf = 4;
            } else if ((srcImage.getType() == ModelStorageBase.COMPLEX) ||
                           (srcImage.getType() == ModelStorageBase.DCOMPLEX)) {
                cf = 2;
                logMagDisplay = srcImage.getLogMagDisplay();
            } else {
                cf = 1;
            }

            length = cf * srcImage.getSliceSize();
            totalLength = length * nImages;
            buffer = new float[length];
            resultBuffer = new float[length * nImages];
            buildProgressBar(srcImage.getImageName(), "Flipping image ...", 0, 100);
        } catch (OutOfMemoryError e) {
            buffer = null;
            resultBuffer = null;
            System.gc();
            displayError("Algorithm Flip: Out of memory");
            setCompleted(false);
            disposeProgressBar();

            return;
        }

        int mod = totalLength / 10; // mod is 10 percent of length
        initProgressBar();

        for (s = 0; (s < nImages) && !threadStopped; s++) {
            start = s * length;

            try {
                srcImage.exportData(start, length, buffer); // locks and releases lock
            } catch (IOException error) {
                displayError("Algorithm Flip: Image(s) locked");
                setCompleted(false);
                disposeProgressBar();

                return;
            }

            if (cf == 1) {

                if (flipAxis == Y_AXIS) {

                    for (y = 0; (y < yDim) && !threadStopped; y++) {

                        for (x = 0; (x < xDim) && !threadStopped; x++) {
                            index = ((y * xDim) + x);

                            if ((((start + index) % mod) == 0) && isProgressBarVisible()) {
                                progressBar.updateValue(Math.round((float) (start + index) / (totalLength - 1) * 100),
                                                        activeImage);
                            }

                            resultBuffer[start + index] = buffer[(y * xDim) + (xDim - 1 - x)];
                        }
                    }
                } else if (flipAxis == X_AXIS) {

                    for (y = 0; (y < yDim) && !threadStopped; y++) {

                        for (x = 0; (x < xDim) && !threadStopped; x++) {
                            index = ((y * xDim) + x);

                            if ((((start + index) % mod) == 0) && isProgressBarVisible()) {
                                progressBar.updateValue(Math.round((float) (start + index) / (totalLength - 1) * 100),
                                                        activeImage);
                            }

                            resultBuffer[start + index] = buffer[((yDim - 1 - y) * xDim) + x];
                        }
                    }
                } else {
                    for (y = 0; (y < yDim) && !threadStopped; y++) {

                        for (x = 0; (x < xDim) && !threadStopped; x++) {
                            index = ( (y * xDim) + x);
                            resultBuffer[((nImages - 1)*length) - start + index] = buffer[index];
                        }
                    }
                }
            } // end of if (cf == 1)
            else { // cf == 2 or 4

                if (flipAxis == Y_AXIS) {

                    for (y = 0; (y < yDim) && !threadStopped; y++) {

                        for (x = 0; (x < xDim) && !threadStopped; x++) {

                            for (j = 0; (j < cf) && !threadStopped; j++) {
                                index = ((cf * y * xDim) + (cf * x) + j);

                                if ((((start + index) % mod) == 0) && isProgressBarVisible()) {
                                    progressBar.updateValue(Math.round((float) (start + index) / (totalLength - 1) *
                                                                           100), activeImage);
                                }

                                resultBuffer[start + index] = buffer[(cf * y * xDim) + (cf * (xDim - 1 - x)) + j];
                            }
                        }
                    }
                } else if (flipAxis == X_AXIS) {

                    for (y = 0; (y < yDim) && !threadStopped; y++) {

                        for (x = 0; (x < xDim) && !threadStopped; x++) {

                            for (j = 0; (j < cf) && !threadStopped; j++) {
                                index = ((cf * y * xDim) + (cf * x) + j);

                                if ((((start + index) % mod) == 0) && isProgressBarVisible()) {
                                    progressBar.updateValue(Math.round((float) (start + index) / (totalLength - 1) *
                                                                           100), activeImage);
                                }

                                resultBuffer[start + index] = buffer[(cf * (yDim - 1 - y) * xDim) + (cf * x) + j];
                            }
                        }
                    }
                } else {
                    for (y = 0; (y < yDim) && !threadStopped; y++) {

                        for (x = 0; (x < xDim) && !threadStopped; x++) {

                            for (j = 0; (j < cf) && !threadStopped; j++) {
                                index = ( (cf * y * xDim) + (cf * x) + j);
                                resultBuffer[((nImages - 1)*length) - start + index] = buffer[index];
                            }
                        }
                    }
                }

            } // end of else for cf == 2 or 4
        }

        if (threadStopped) {
            buffer = null;
            resultBuffer = null;
            finalize();

            return;
        }

        try {

            if (cf == 2) {
                srcImage.importData(0, resultBuffer, false);
            } else {
                srcImage.importData(0, resultBuffer, true);
            }
        } catch (IOException error) {
            resultBuffer = null;
            buffer = null;
            displayError("Algorithm Flip: Image(s) locked");
            setCompleted(false);
            disposeProgressBar();

            return;
        } catch (OutOfMemoryError e) {
            resultBuffer = null;
            buffer = null;
            System.gc();
            displayError("Algorithm Flip: Out of memory");
            setCompleted(false);
            disposeProgressBar();

            return;
        }

        if (cf == 2) {
            srcImage.setLogMagDisplay(logMagDisplay);
            srcImage.calcMinMaxMag(logMagDisplay);
        }

        FileInfoBase[] fileInfo = srcImage.getFileInfo();

        if (flipAxis == Y_AXIS) {
            index = 0;
        } else if (flipAxis == X_AXIS) {
            index = 1;
        } else {
            index = 2;
        }

        float loc = fileInfo[0].getOrigin(index);
        int orient = fileInfo[0].getAxisOrientation(index);

        if ((orient == FileInfoBase.ORI_L2R_TYPE) || (orient == FileInfoBase.ORI_S2I_TYPE) ||
                (orient == FileInfoBase.ORI_P2A_TYPE)) {
            loc = loc - ((fileInfo[0].getExtents()[index] - 1) * fileInfo[0].getResolutions()[index]);
        } else {
            loc = loc + ((fileInfo[0].getExtents()[index] - 1) * fileInfo[0].getResolutions()[index]);
        }

        orient = FileInfoBase.oppositeOrient(orient);

        for (int i = 0; i < fileInfo.length; i++) {
            fileInfo[i].setAxisOrientation(orient, index);
            fileInfo[i].setOrigin(loc, index);
        }

        disposeProgressBar();
        setCompleted(true);
    }

    /**
     * Constructs a string of the contruction parameters and out puts the string to the messsage frame if the logging
     * procedure is turned on.
     */
    private void constructLog() {

        if (flipAxis == Y_AXIS) {
            historyString = new String("Flip(Y_AXIS)\n");
        } else if (flipAxis == X_AXIS) {
            historyString = new String("Flip(X_AXIS)\n");
        } else {
            historyString = new String("Flip(Z_AXIS)\n");
        }
    }

}
