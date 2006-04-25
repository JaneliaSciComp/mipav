package gov.nih.mipav.model.algorithms.utilities;


import gov.nih.mipav.model.algorithms.*;
import gov.nih.mipav.model.file.*;
import gov.nih.mipav.model.structures.*;
import gov.nih.mipav.model.structures.jama.*;

import gov.nih.mipav.view.*;

import java.io.*;

import java.text.*;


/**
 * An Algorithm to rotate 3D or 4D dataset 90 degrees about X, Y, or Z axis. 2D Images can also be rotated. A new
 * rotated image with modified dimensions and resolutions created and can be accessed through returnImage().
 *
 * @version  1.0 July 25, 2000
 * @author   Harman J. Singh
 * @author   Matthew J. McAuliffe, Ph.D.
 */
public class AlgorithmRotate extends AlgorithmBase {

    //~ Static fields/initializers -------------------------------------------------------------------------------------

    /** Rotate about the x axis 90 degrees. */
    public static final int X_AXIS_PLUS = 0;

    /** Rotate about the x axis -90 degrees (or 270 degrees). */
    public static final int X_AXIS_MINUS = 1;

    /** Rotate about the y axis 90 degrees. */
    public static final int Y_AXIS_PLUS = 2;

    /** Rotate about the y axis -90 degrees (or 270 degrees). */
    public static final int Y_AXIS_MINUS = 3;

    /** Rotate about the z axis 90 degrees. */
    public static final int Z_AXIS_PLUS = 4;

    /** Rotate about the z axis -90 degrees (or 270 degrees). */
    public static final int Z_AXIS_MINUS = 5;

    //~ Instance fields ------------------------------------------------------------------------------------------------

    /** Indicates the rotation axis. Default is z axis. */
    private int rotateAxis = Z_AXIS_PLUS;

    //~ Constructors ---------------------------------------------------------------------------------------------------

    /**
     * Creates new algorithm for rotating. Sets the source image and axis parameter.
     *
     * @param  srcImg      Source image model.
     * @param  rotateMode  Rotate about which axis.
     */
    public AlgorithmRotate(ModelImage srcImg, int rotateMode) {
        super(null, srcImg);

        if ((rotateMode == Y_AXIS_PLUS) || (rotateMode == Y_AXIS_MINUS) || (rotateMode == X_AXIS_PLUS) ||
                (rotateMode == X_AXIS_MINUS) || (rotateMode == Z_AXIS_PLUS) || (rotateMode == Z_AXIS_MINUS)) {
            rotateAxis = rotateMode;
        } else {
            rotateAxis = Z_AXIS_PLUS; // default rotate mode
        }
    }

    /**
     * Creates new algorithm for rotating. Sets the source image and axis parameter.
     *
     * @param  srcImg      Source image model.
     * @param  rotateMode  Rotate about which axis.
     * @param  progress    Progress mode (see AlgorithmBase).
     */
    public AlgorithmRotate(ModelImage srcImg, int rotateMode, int progress) {
        this(srcImg, rotateMode);
        progressMode = progress;
    }

    //~ Methods --------------------------------------------------------------------------------------------------------

    /**
     * Prepares this class for destruction.
     */
    public void finalize() {
        srcImage = null;
        super.finalize();
    }

    /**
     * Returns the rotated image.
     *
     * @return  The rotated image.
     */
    public ModelImage returnImage() {
        return destImage;
    }

    /**
     * Runs the rotation algorithm. The algorithm is run in place so automatically replaces the source model image.
     * Resets image orientation, axis orientations, and start locations.
     */
    public void runAlgorithm() {

        if (srcImage == null) {
            displayError("Source Image is null");

            return;
        }

        constructLog();

        if (srcImage.getNDims() == 2) {
            calcInPlace2D();
        } else if (srcImage.getNDims() >= 3) {
            calcInPlace3D();
        }

        disposeProgressBar();
    }

    /**
     * Helper method for finding biggest (absolute value) of three numbers.
     *
     * @param   zero  DOCUMENT ME!
     * @param   one   DOCUMENT ME!
     * @param   two   DOCUMENT ME!
     *
     * @return  Index of number that is the biggest.
     */
    private int absBiggest(double zero, double one, double two) {

        if (Math.abs(zero) > Math.abs(one)) {

            if (Math.abs(zero) > Math.abs(two)) {
                return 0;
            } else {
                return 2;
            }
        } else {

            if (Math.abs(one) > Math.abs(two)) {
                return 1;
            } else {
                return 2;
            }
        }
    }

    /**
     * Calculates the rotated image and replaces the source image with the rotated image. Two dimensional images call
     * this method.
     */
    private void calcInPlace2D() {
        int i, j, k, w;
        int length;
        float[] buffer;
        int[] oldDimExtents = new int[2];
        oldDimExtents = srcImage.getExtents();

        int colorFactor;
        int[] newDimExtents;
        float[] newResolutions = srcImage.getFileInfo(0).getResolutions();
        ;

        if ((rotateAxis != Z_AXIS_PLUS) && (rotateAxis != Z_AXIS_MINUS)) {
            return;
        }

        try {

            if (srcImage.isColorImage()) {
                colorFactor = 4; // ARGB Image
            } else {
                colorFactor = 1; // Grey Scale Image
            }

            buildProgressBar(srcImage.getImageName(), "Rotating image ...", 0, 100);
        } catch (OutOfMemoryError e) {
            buffer = null;
            errorCleanUp("Algorithm Rotate: Out of memory", true);

            return;
        }

        try {
            newDimExtents = new int[2];
            length = colorFactor * srcImage.getSliceSize();
            buffer = new float[length];
            newDimExtents[0] = oldDimExtents[1];
            newDimExtents[1] = oldDimExtents[0];
            newResolutions[0] = srcImage.getFileInfo(0).getResolutions()[1];
            newResolutions[1] = srcImage.getFileInfo(0).getResolutions()[0];
            destImage = new ModelImage(srcImage.getType(), newDimExtents, srcImage.getImageName(),
                                       srcImage.getUserInterface());
        } catch (OutOfMemoryError e) {
            buffer = null;
            errorCleanUp("Algorithm Rotate: Out of memory", true);

            return;
        }

        int mod = length / 10; // mod is 10 percent of length
        initProgressBar();

        if (colorFactor == 1) {

            try {

                if (rotateAxis == Z_AXIS_PLUS) {

                    try {
                        srcImage.setLock(ModelStorageBase.W_LOCKED);
                    } catch (IOException error) {
                        throw error;
                    }

                    i = 0;

                    // gets new data for rotated image
                    for (j = 0; (j < oldDimExtents[0]) && !threadStopped; j++) {

                        for (k = oldDimExtents[1] - 1; (k >= 0) && !threadStopped; k--) {

                            if (((i % mod) == 0) && isProgressBarVisible()) {
                                progressBar.updateValue(Math.round((float) (i) / (length - 1) * 100), activeImage);
                            }

                            buffer[i] = srcImage.getFloat(j, k); // places new pixel data in buffer
                            i++;
                        }
                    }

                    srcImage.releaseLock();
                } else { // rotateAxis = Z_AXIS_MINUS
                    i = 0;

                    // gets new data for rotated image
                    for (j = oldDimExtents[0] - 1; (j >= 0) && !threadStopped; j--) {

                        for (k = 0; (k < oldDimExtents[1]) && !threadStopped; k++) {

                            if (((i % mod) == 0) && isProgressBarVisible()) {
                                progressBar.updateValue(Math.round((float) (i) / (length - 1) * 100), activeImage);
                            }

                            buffer[i] = srcImage.getFloat(j, k); // places new pixel data in buffer
                            i++;
                        }
                    }
                }

                if (threadStopped) {
                    buffer = null;
                    finalize();

                    return;
                }
            } catch (IOException error) {
                errorCleanUp("Algorithm Rotate: Image(s) locked", false);

                return;
            }
        } else { // else colorFactor = 4

            try {

                if (rotateAxis == Z_AXIS_PLUS) {

                    try {
                        srcImage.setLock(ModelStorageBase.W_LOCKED);
                    } catch (IOException error) {
                        throw error;
                    }

                    i = 0;

                    // gets new data for rotated image
                    for (j = 0; (j < oldDimExtents[0]) && !threadStopped; j++) {

                        for (k = oldDimExtents[1] - 1; (k >= 0) && !threadStopped; k--) {

                            // must repeat 4 times for each pixel in order to get ARGB data
                            for (w = 0; (w < 4) && !threadStopped; w++) {

                                if (((i % mod) == 0) && isProgressBarVisible()) {
                                    progressBar.updateValue(Math.round((float) (i) / (length - 1) * 100), activeImage);
                                }

                                buffer[i] = srcImage.getFloat((k * oldDimExtents[0] * 4) + ((4 * j) + w)); // puts new pixel data in buffer
                                i++;
                            }
                        }
                    }

                    srcImage.releaseLock();
                } else { // rotateAxis = Z_AXIS_MINUS
                    i = 0;

                    // gets new data for rotated image
                    for (j = oldDimExtents[0] - 1; (j >= 0) && !threadStopped; j--) {

                        for (k = 0; (k < oldDimExtents[1]) && !threadStopped; k++) {

                            // must repeat 4 times for each pixel in order to get ARGB data
                            for (w = 0; (w < 4) && !threadStopped; w++) {

                                if (((i % mod) == 0) && isProgressBarVisible()) {
                                    progressBar.updateValue(Math.round((float) (i) / (length - 1) * 100), activeImage);
                                }

                                buffer[i] = srcImage.getFloat((k * oldDimExtents[0] * 4) + ((4 * j) + w)); // puts new pixel data in buffer
                                i++;
                            }
                        }
                    }
                }

                if (threadStopped) {
                    buffer = null;
                    finalize();

                    return;
                }
            } catch (IOException error) {
                errorCleanUp("Algorithm Rotate: Image(s) locked", true);

                return;
            }
        }

        if (threadStopped) {
            buffer = null;
            finalize();

            return;
        }

        try {
            destImage.importData(0, buffer, true); // imports new pixel data into new image
        } catch (IOException error) {
            errorCleanUp("Algorithm Rotate: Image(s) locked", false);

            return;
        }

        // sets file info of new image to the same as the old source image
        FileInfoBase newFileInfo = (FileInfoBase) srcImage.getFileInfo(0).clone();

        if (srcImage.getFileInfo(0).getAxisOrientation()[1] != FileInfoBase.ORI_UNKNOWN_TYPE) { // if were set to some
                                                                                                // nontrivial value
            newFileInfo.setAxisOrientation(srcImage.getFileInfo(0).getAxisOrientation()[2], 2);

            if (rotateAxis == Z_AXIS_PLUS) {
                newFileInfo.setAxisOrientation(srcImage.getFileInfo(0).getAxisOrientation()[0], 1);
                newFileInfo.setAxisOrientation(FileInfoBase.oppositeOrient(srcImage.getFileInfo(0).getAxisOrientation()[1]),
                                               0);
            } else { // Z_AXIS_MINUS
                newFileInfo.setAxisOrientation(srcImage.getFileInfo(0).getAxisOrientation()[1], 0);
                newFileInfo.setAxisOrientation(FileInfoBase.oppositeOrient(srcImage.getFileInfo(0).getAxisOrientation()[0]),
                                               1);
            }
        }

        newFileInfo.setExtents(newDimExtents);
        newFileInfo.setResolutions(newResolutions);
        destImage.setFileInfo(newFileInfo, 0);

        if (srcImage.getFileInfo(0).getFileFormat() == FileBase.DICOM) {
            FileInfoDicom newDicomInfo = (FileInfoDicom) srcImage.getFileInfo(0).clone();

            newDicomInfo.setValue("0028,0011", new Short((short) newDimExtents[0]), 2); // columns
            newDicomInfo.setValue("0028,0010", new Short((short) newDimExtents[1]), 2); // rows

            // set image orientation too!
            destImage.setFileInfo(newDicomInfo, 0);
        }

        destImage.calcMinMax();
        destImage.releaseLock();

        disposeProgressBar();
        setCompleted(true);
    }

    /**
     * Calculates the rotated image and replaces the source image with the rotated image. Three dimensional and higher
     * dimensional images call this method.
     */
    private void calcInPlace3D() {

        int s, i, j, k, w, t;
        int colorFactor;
        int tOffset;
        int index;
        int stride, tmpStart;
        int length, totalLength;
        int start;
        float[] buffer;
        float[] resultBuffer;

        int[] oldDimExtents;
        int oldTDim;
        int[] newDimExtents;
        float[] oldResolutions = (float[]) srcImage.getFileInfo(0).getResolutions().clone();
        float[] newResolutions;
        int[] oldAxisOrients = (int[]) srcImage.getFileInfo(0).getAxisOrientation().clone();
        int[] newAxisOrients;
        boolean[] oppositeOrients;
        float[] oldStartLocations = (float[]) srcImage.getFileInfo(0).getOrigin().clone();
        float[] newStartLocations;
        int[] direct;
        int[] orderIndex;

        if (srcImage.getExtents().length > 3) {
            oldTDim = srcImage.getExtents()[3];
        } else {
            oldTDim = 1;
        }

        // CHANGE THIS so multiply origin by rotation matrix


        double[][] array = new double[1][4];

        array[0][0] = oldStartLocations[0];
        array[0][1] = oldStartLocations[1];
        array[0][2] = oldStartLocations[2];
        array[0][3] = 1;

        Matrix mat = new Matrix(array);
        TransMatrix tMat = null;

        try {

            if (srcImage.isColorImage()) {
                colorFactor = 4; // ARGB image
            } else {
                colorFactor = 1; // Gray Scale Image
            }

            buildProgressBar(srcImage.getImageName(), "Rotating image ...", 0, 100);
        } catch (OutOfMemoryError e) {
            buffer = null;
            resultBuffer = null;
            errorCleanUp("Algorithm Rotate: Out of memory", true);

            return;
        }

        try {
            oldDimExtents = new int[srcImage.getNDims()];
            oldDimExtents = (int[]) srcImage.getExtents().clone();

            newDimExtents = new int[srcImage.getNDims()];
            newResolutions = new float[srcImage.getNDims()];
            newAxisOrients = new int[srcImage.getNDims()];
            newStartLocations = new float[srcImage.getNDims()];

            /* initialize newAxisOrients and newStartLocations */
            newAxisOrients = (int[]) srcImage.getFileInfo(0).getAxisOrientation().clone();
            newStartLocations = (float[]) srcImage.getFileInfo(0).getOrigin().clone();

            oppositeOrients = new boolean[3];
            direct = new int[3];
            orderIndex = new int[srcImage.getNDims()];

            for (i = 0; i < 3; i++) {
                oppositeOrients[i] = false;
                direct[i] = 1;
                orderIndex[i] = i;
            }

            if ((rotateAxis == Y_AXIS_PLUS) || (rotateAxis == Y_AXIS_MINUS)) {

                // calculates size  and initializes buffer for a single slice and
                // total buffer for whole image
                length = colorFactor * srcImage.getExtents()[2] * srcImage.getExtents()[1];
                totalLength = length * oldDimExtents[0] * oldTDim;
                buffer = new float[length];
                resultBuffer = new float[totalLength];
                orderIndex[0] = 2;
                orderIndex[1] = 1;
                orderIndex[2] = 0;

                if (rotateAxis == Y_AXIS_PLUS) {
                    oppositeOrients[2] = true;
                } else {
                    oppositeOrients[0] = true;
                }
            } else if ((rotateAxis == X_AXIS_PLUS) || (rotateAxis == X_AXIS_MINUS)) {
                length = colorFactor * srcImage.getExtents()[0] * srcImage.getExtents()[2];
                totalLength = length * oldDimExtents[1] * oldTDim;
                buffer = new float[length];
                resultBuffer = new float[totalLength];
                orderIndex[0] = 0;
                orderIndex[1] = 2;
                orderIndex[2] = 1;

                if (rotateAxis == X_AXIS_PLUS) {
                    oppositeOrients[1] = true;
                } else {
                    oppositeOrients[2] = true;
                }
            } else {
                length = colorFactor * srcImage.getExtents()[1] * srcImage.getExtents()[0];
                totalLength = length * oldDimExtents[2] * oldTDim;
                buffer = new float[length];
                resultBuffer = new float[totalLength];
                orderIndex[0] = 1;
                orderIndex[1] = 0;
                orderIndex[2] = 2;

                if (rotateAxis == Z_AXIS_PLUS) {
                    oppositeOrients[0] = true;
                } else {
                    oppositeOrients[1] = true;
                }
            }

            // for all rotations (x,y, and z, plus or minus)


            for (i = 0; i < 3; i++) {
                j = orderIndex[i];
                newDimExtents[i] = oldDimExtents[j];
                newResolutions[i] = oldResolutions[j];
                newAxisOrients[i] = oldAxisOrients[j];

                if ((oldAxisOrients[j] == FileInfoBase.ORI_A2P_TYPE) ||
                        (oldAxisOrients[j] == FileInfoBase.ORI_R2L_TYPE) ||
                        (oldAxisOrients[j] == FileInfoBase.ORI_I2S_TYPE)) {
                    direct[j] = 1;
                } else {
                    direct[j] = -1;
                }

                if (oppositeOrients[i] == true) {
                    newAxisOrients[i] = FileInfoBase.oppositeOrient(newAxisOrients[i]);
                    //       newStartLocations[i] = oldStartLocations[j]            + direct[j] * ( oldDimExtents[j] - 1
                    // ) * oldResolutions[j];
                } else {
                    // newStartLocations[i] = oldStartLocations[j];
                }

                if ((newAxisOrients[i] == FileInfoBase.ORI_A2P_TYPE) ||
                        (newAxisOrients[i] == FileInfoBase.ORI_R2L_TYPE) ||
                        (newAxisOrients[i] == FileInfoBase.ORI_I2S_TYPE)) {
                    direct[i] = 1;
                } else {
                    direct[i] = -1;
                }
            }

            if (newDimExtents.length > 3) {
                newDimExtents[3] = oldTDim;
                newResolutions[3] = oldResolutions[3];
            }

            destImage = new ModelImage(srcImage.getType(), newDimExtents, srcImage.getImageName(),
                                       srcImage.getUserInterface());
        } catch (OutOfMemoryError e) {
            buffer = null;
            resultBuffer = null;
            errorCleanUp("Algorithm Rotate: Out of memory", true);

            return;
        }

        int mod = totalLength / 10; // mod is 10 percent of length
        initProgressBar();

        int tStop;

        if (newDimExtents.length > 3) {
            tStop = newDimExtents[3];
        } else {
            tStop = 1;
        }

        float[] minimums = null;
        float[] maximums = null;

        try {

            if (srcImage.getFileInfo(0).getFileFormat() == FileBase.MINC) {
                minimums = new float[newDimExtents[2]];
                maximums = new float[newDimExtents[2]];
            }
        } catch (OutOfMemoryError error) {
            buffer = null;
            resultBuffer = null;
            errorCleanUp("Algorithm Rotate: Out of memory", true);

            return;
        }

        for (t = 0; (t < tStop) && !threadStopped; t++) {
            tOffset = colorFactor * newDimExtents[0] * newDimExtents[1] * newDimExtents[2] * t;

            for (s = 0; (s < newDimExtents[2]) && !threadStopped; s++) {
                float min = Float.MAX_VALUE;
                float max = Float.MIN_VALUE;

                if (colorFactor == 1) {
                    start = s * length;

                    try {

                        if (rotateAxis == Y_AXIS_PLUS) {

                            try {
                                srcImage.setLock(ModelStorageBase.W_LOCKED);
                            } catch (IOException error) {
                                throw error;
                            }

                            stride = oldDimExtents[0] * oldDimExtents[1];
                            tmpStart = newDimExtents[2] - s - 1;
                            i = 0;

                            // calculates pixel data for rotated image
                            for (j = 0; (j < oldDimExtents[1]) && !threadStopped; j++) {

                                for (k = 0; (k < oldDimExtents[2]) && !threadStopped; k++) {

                                    // puts pixel data for the particular slice into buffer
                                    buffer[i] = srcImage.getFloat(tmpStart + (k * stride) + tOffset);

                                    if (buffer[i] > max) {
                                        max = buffer[i];
                                    }

                                    if (buffer[i] < min) {
                                        min = buffer[i];
                                    }

                                    i++;
                                }

                                tmpStart += oldDimExtents[0];
                            }

                            srcImage.releaseLock();

                        } else if (rotateAxis == Y_AXIS_MINUS) {

                            try {
                                srcImage.setLock(ModelStorageBase.W_LOCKED);
                            } catch (IOException error) {
                                throw error;
                            }

                            stride = oldDimExtents[0] * oldDimExtents[1];
                            tmpStart = s;
                            i = 0;

                            // calculates pixel data for rotated image
                            for (j = 0; (j < oldDimExtents[1]) && !threadStopped; j++) {

                                for (k = oldDimExtents[2] - 1; (k >= 0) && !threadStopped; k--) {

                                    // puts pixel data for the particular slice into the buffer
                                    buffer[i] = srcImage.getFloat(tmpStart + (k * stride) + tOffset);

                                    if (buffer[i] > max) {
                                        max = buffer[i];
                                    }

                                    if (buffer[i] < min) {
                                        min = buffer[i];
                                    }

                                    i++;
                                }

                                tmpStart += oldDimExtents[0];
                            }

                            srcImage.releaseLock();

                        } else if (rotateAxis == X_AXIS_MINUS) {

                            try {
                                srcImage.setLock(ModelStorageBase.W_LOCKED);
                            } catch (IOException error) {
                                throw error;
                            }

                            stride = oldDimExtents[0] * oldDimExtents[1];
                            i = 0;
                            tmpStart = (newDimExtents[2] - s - 1) * oldDimExtents[0];

                            // calculates pixel data for rotated image
                            for (j = 0; (j < oldDimExtents[2]) && !threadStopped; j++) {

                                for (k = 0; (k < oldDimExtents[0]) && !threadStopped; k++) {

                                    // puts pixel data for the particular slice into the buffer
                                    buffer[i] = srcImage.getFloat(tmpStart + k + tOffset);

                                    if (buffer[i] > max) {
                                        max = buffer[i];
                                    }

                                    if (buffer[i] < min) {
                                        min = buffer[i];
                                    }

                                    i++;
                                }

                                tmpStart += stride;
                            }

                            srcImage.releaseLock();
                        } else if (rotateAxis == X_AXIS_PLUS) {

                            try {
                                srcImage.setLock(ModelStorageBase.W_LOCKED);
                            } catch (IOException error) {
                                throw error;
                            }

                            stride = oldDimExtents[0] * oldDimExtents[1];
                            i = 0;
                            tmpStart = (s * oldDimExtents[0]) + (stride * (oldDimExtents[2] - 1));

                            // calculates pixel data for rotated image
                            for (j = 0; (j < oldDimExtents[2]) && !threadStopped; j++) {

                                for (k = 0; (k < oldDimExtents[0]) && !threadStopped; k++) {

                                    // puts pixel data for the particular slice into the buffer
                                    buffer[i] = srcImage.getFloat(tmpStart + k + tOffset);

                                    if (buffer[i] > max) {
                                        max = buffer[i];
                                    }

                                    if (buffer[i] < min) {
                                        min = buffer[i];
                                    }

                                    i++;
                                }

                                tmpStart -= stride;
                            }

                            srcImage.releaseLock();
                        } else if (rotateAxis == Z_AXIS_PLUS) {

                            try {
                                srcImage.setLock(ModelStorageBase.W_LOCKED);
                            } catch (IOException error) {
                                throw error;
                            }

                            i = 0;

                            // calculates pixel data for rotated image
                            for (j = 0; (j < oldDimExtents[0]) && !threadStopped; j++) {

                                for (k = oldDimExtents[1] - 1; (k >= 0) && !threadStopped; k--) {

                                    // puts pixel data for the particular slice into the buffer
                                    buffer[i] = srcImage.getFloat(j, k, s, t);

                                    if (buffer[i] > max) {
                                        max = buffer[i];
                                    }

                                    if (buffer[i] < min) {
                                        min = buffer[i];
                                    }

                                    i++;
                                }
                            }

                            srcImage.releaseLock();
                        } else { // rotateAxis = Z_AXIS_MINUS
                            i = 0;

                            // calculates pixel data for rotated image
                            for (j = oldDimExtents[0] - 1; (j >= 0) && !threadStopped; j--) {

                                for (k = 0; (k < oldDimExtents[1]) && !threadStopped; k++) {

                                    // puts pixel data for particular slice into the buffer
                                    buffer[i] = srcImage.getFloat(j, k, s, t);

                                    if (buffer[i] > max) {
                                        max = buffer[i];
                                    }

                                    if (buffer[i] < min) {
                                        min = buffer[i];
                                    }

                                    i++;
                                }
                            }
                        }
                    } catch (IOException error) {
                        buffer = null;
                        resultBuffer = null;
                        minimums = null;
                        maximums = null;
                        errorCleanUp("Algorithm Rotate: Image(s) locked", true);

                        return;
                    } catch (OutOfMemoryError error) {
                        buffer = null;
                        resultBuffer = null;
                        minimums = null;
                        maximums = null;
                        errorCleanUp("Algorithm Rotate: Out of Memory", true);

                        return;
                    }

                    // copies data from buffer for the particular slice into the resultbuffer
                    // for the whole image
                    for (index = 0; (index < length) && !threadStopped; index++) {

                        if ((((start + index + tOffset) % mod) == 0) && isProgressBarVisible()) {
                            progressBar.updateValue(Math.round((float) (start + index + tOffset) / (totalLength - 1) *
                                                                   100), activeImage);
                        }

                        resultBuffer[start + index + tOffset] = buffer[index];
                    }
                } else { // else colorFactor = 4
                    start = s * length;

                    try {

                        if (rotateAxis == Y_AXIS_PLUS) {

                            try {
                                srcImage.setLock(ModelStorageBase.W_LOCKED);
                            } catch (IOException error) {
                                throw error;
                            }

                            stride = oldDimExtents[0] * oldDimExtents[1];
                            tmpStart = newDimExtents[2] - s - 1;
                            i = 0;

                            // calculates new pixel data for particular slice of rotated image
                            for (j = 0; (j < oldDimExtents[1]) && !threadStopped; j++) {

                                for (k = 0; (k < oldDimExtents[2]) && !threadStopped; k++) {

                                    // must copy all ARGB values for each pixel
                                    for (w = 0; (w < 4) && !threadStopped; w++) {

                                        // putS pixel data into buffer
                                        buffer[i] = srcImage.getFloat((tmpStart * 4) + (k * stride * 4) + w + tOffset);

                                        if (buffer[i] > max) {
                                            max = buffer[i];
                                        }

                                        if (buffer[i] < min) {
                                            min = buffer[i];
                                        }

                                        i++;
                                    }
                                }

                                tmpStart += oldDimExtents[0];
                            }

                            srcImage.releaseLock();

                        } else if (rotateAxis == Y_AXIS_MINUS) {

                            try {
                                srcImage.setLock(ModelStorageBase.W_LOCKED);
                            } catch (IOException error) {
                                throw error;
                            }

                            stride = oldDimExtents[0] * oldDimExtents[1];
                            tmpStart = s;
                            i = 0;

                            // calculates new pixel data for particular slice of rotated image
                            for (j = 0; (j < oldDimExtents[1]) && !threadStopped; j++) {

                                for (k = oldDimExtents[2] - 1; (k >= 0) && !threadStopped; k--) {

                                    // must copy all ARGB values for each pixel
                                    for (w = 0; w < 4; w++) {

                                        // puts pixel data into buffer
                                        buffer[i] = srcImage.getFloat((tmpStart * 4) + (k * stride * 4) + w + tOffset);

                                        if (buffer[i] > max) {
                                            max = buffer[i];
                                        }

                                        if (buffer[i] < min) {
                                            min = buffer[i];
                                        }

                                        i++;
                                    }
                                }

                                tmpStart += oldDimExtents[0];
                            }

                            srcImage.releaseLock();

                        } else if (rotateAxis == X_AXIS_MINUS) {

                            try {
                                srcImage.setLock(ModelStorageBase.W_LOCKED);
                            } catch (IOException error) {
                                throw error;
                            }

                            stride = oldDimExtents[0] * oldDimExtents[1];
                            i = 0;
                            tmpStart = (newDimExtents[2] - s - 1) * oldDimExtents[0];

                            // calculates new pixel data for particular slice of rotated image
                            for (j = 0; (j < oldDimExtents[2]) && !threadStopped; j++) {

                                for (k = 0; (k < oldDimExtents[0]) && !threadStopped; k++) {

                                    // must copy all ARGB values for each pixel
                                    for (w = 0; (w < 4) && !threadStopped; w++) {

                                        // puts pixel data into buffer
                                        buffer[i] = srcImage.getFloat((tmpStart * 4) + (k * 4) + w + tOffset);

                                        if (buffer[i] > max) {
                                            max = buffer[i];
                                        }

                                        if (buffer[i] < min) {
                                            min = buffer[i];
                                        }

                                        i++;
                                    }
                                }

                                tmpStart += stride;
                            }

                            srcImage.releaseLock();
                        } else if (rotateAxis == X_AXIS_PLUS) {

                            try {
                                srcImage.setLock(ModelStorageBase.W_LOCKED);
                            } catch (IOException error) {
                                throw error;
                            }

                            stride = oldDimExtents[0] * oldDimExtents[1];
                            i = 0;
                            tmpStart = (s * oldDimExtents[0]) + (stride * (oldDimExtents[2] - 1));

                            // calculates new pixel data for particular slice of rotated image
                            for (j = 0; (j < oldDimExtents[2]) && !threadStopped; j++) {

                                for (k = 0; (k < oldDimExtents[0]) && !threadStopped; k++) {

                                    // must copy all ARGB values for each pixel
                                    for (w = 0; (w < 4) && !threadStopped; w++) {

                                        // puts pixel data into buffer
                                        buffer[i] = srcImage.getFloat((tmpStart * 4) + (k * 4) + w + tOffset);

                                        if (buffer[i] > max) {
                                            max = buffer[i];
                                        }

                                        if (buffer[i] < min) {
                                            min = buffer[i];
                                        }

                                        i++;
                                    }
                                }

                                tmpStart -= stride;
                            }

                            srcImage.releaseLock();
                        } else if (rotateAxis == Z_AXIS_PLUS) {

                            try {
                                srcImage.setLock(ModelStorageBase.W_LOCKED);
                            } catch (IOException error) {
                                throw error;
                            }

                            i = 0;

                            // calculates new pixel data for particluar slice of rotated image
                            for (j = 0; (j < oldDimExtents[0]) && !threadStopped; j++) {

                                for (k = oldDimExtents[1] - 1; (k >= 0) && !threadStopped; k--) {

                                    // must copy all ARGB values for each pixel
                                    for (w = 0; (w < 4) && !threadStopped; w++) {

                                        // puts pizel data into buffer
                                        buffer[i] = srcImage.getFloat((s * (oldDimExtents[0] * oldDimExtents[1] * 4)) +
                                                                      (k * oldDimExtents[0] * 4) + ((4 * j) + w) +
                                                                      tOffset);

                                        if (buffer[i] > max) {
                                            max = buffer[i];
                                        }

                                        if (buffer[i] < min) {
                                            min = buffer[i];
                                        }

                                        i++;
                                    }
                                }
                            }

                            srcImage.releaseLock();
                        } else { // rotateAxis = Z_AXIS_MINUS
                            i = 0;

                            // calculates new pxiel data for particular slice of rotated image
                            for (j = oldDimExtents[0] - 1; (j >= 0) && !threadStopped; j--) {

                                for (k = 0; (k < oldDimExtents[1]) && !threadStopped; k++) {

                                    // must copy all ARGB values for each pixel
                                    for (w = 0; (w < 4) && !threadStopped; w++) {

                                        // puts pixel data into buffer
                                        buffer[i] = srcImage.getFloat((s * (oldDimExtents[0] * oldDimExtents[1] * 4)) +
                                                                      (k * oldDimExtents[0] * 4) + ((4 * j) + w) +
                                                                      tOffset);

                                        if (buffer[i] > max) {
                                            max = buffer[i];
                                        }

                                        if (buffer[i] < min) {
                                            min = buffer[i];
                                        }

                                        i++;
                                    }
                                }
                            }
                        }
                    } catch (IOException error) {
                        buffer = null;
                        resultBuffer = null;
                        minimums = null;
                        maximums = null;
                        errorCleanUp("Algorithm Rotate: Image(s) locked", true);

                        return;
                    } catch (OutOfMemoryError error) {
                        buffer = null;
                        resultBuffer = null;
                        minimums = null;
                        maximums = null;
                        errorCleanUp("Algorithm Rotate: Out of Memory", true);

                        return;
                    }

                    // copies pixel data in buffer for particular slice into the resultBuffer
                    // which has the pixel data for the whole image
                    for (index = 0; (index < length) && !threadStopped; index++) {

                        if ((((start + index + tOffset) % mod) == 0) && isProgressBarVisible()) {
                            progressBar.updateValue(Math.round((float) (start + index + tOffset) / (totalLength - 1) *
                                                                   100), activeImage);
                        }

                        resultBuffer[start + index + tOffset] = buffer[index];
                    }
                }

                if (threadStopped) {
                    buffer = null;
                    resultBuffer = null;
                    minimums = null;
                    maximums = null;
                    finalize();

                    return;
                }

                if (srcImage.getFileInfo(0).getFileFormat() == FileBase.MINC) {
                    minimums[s] = min;
                    maximums[s] = max;
                }
            } // for (s = 0; s < newDimExtents[2]; s++)
        } // for (t = 0; t < tStop; t++)

        if (threadStopped) {
            buffer = null;
            resultBuffer = null;
            minimums = null;
            maximums = null;
            finalize();

            return;
        }

        try {

            // imports the rotated image data into the destImage
            destImage.importData(0, resultBuffer, false);
        } catch (IOException error) {
            errorCleanUp("Algorithm Rotate: Image(s) locked", false);

            return;
        } catch (OutOfMemoryError e) {
            errorCleanUp("Algorithm Rotate: Out of memory", true);

            return;
        }

        int orientation = FileInfoBase.UNKNOWN_ORIENT;

        if (newAxisOrients[2] != FileInfoBase.ORI_UNKNOWN_TYPE) {

            if ((newAxisOrients[2] == FileInfoBase.ORI_A2P_TYPE) || (newAxisOrients[2] == FileInfoBase.ORI_P2A_TYPE)) {
                orientation = FileInfoBase.CORONAL;
            } else if ((newAxisOrients[2] == FileInfoBase.ORI_S2I_TYPE) ||
                           (newAxisOrients[2] == FileInfoBase.ORI_I2S_TYPE)) {
                orientation = FileInfoBase.AXIAL;
            } else if ((newAxisOrients[2] == FileInfoBase.ORI_R2L_TYPE) ||
                           (newAxisOrients[2] == FileInfoBase.ORI_L2R_TYPE)) {
                orientation = FileInfoBase.SAGITTAL;
            }
        }

        TransMatrix rotMatrix = new TransMatrix(4);
        rotMatrix.identity();

        if (rotateAxis == X_AXIS_PLUS) {
            rotMatrix.setRotate(90, 0, 0, TransMatrix.DEGREES);
        } else if (rotateAxis == X_AXIS_MINUS) {
            rotMatrix.setRotate(-90, 0, 0, TransMatrix.DEGREES);
        } else if (rotateAxis == Y_AXIS_PLUS) {
            rotMatrix.setRotate(0, 90, 0, TransMatrix.DEGREES);
        } else if (rotateAxis == Y_AXIS_MINUS) {
            rotMatrix.setRotate(0, -90, 0, TransMatrix.DEGREES);
        } else if (rotateAxis == Z_AXIS_PLUS) {
            rotMatrix.setRotate(0, 0, 90, TransMatrix.DEGREES);
        } else if (rotateAxis == Z_AXIS_MINUS) {
            rotMatrix.setRotate(0, 0, -90, TransMatrix.DEGREES);
        }

        // BEN CHANGE

        calcStartLocations(newStartLocations);

        for (int m = 0; m < 3; m++) {

            if (Math.abs(newStartLocations[m]) < .000001f) {
                newStartLocations[m] = 0f;
            }
        }

        tMat = (TransMatrix) srcImage.getMatrix().clone();
        tMat.timesEquals(rotMatrix.inverse());
        destImage.setMatrix(tMat);

        // Set the file info for the new rotated image identical to the original image,
        // and then adjusts the appropriate info.

        // For all image formats other than DICOM...
        if (srcImage.getFileInfo(0).getFileFormat() != FileBase.DICOM) {
            FileInfoBase[] newFileInfo = new FileInfoBase[newDimExtents[2]];

            for (i = 0; i < newDimExtents[2]; i++) {
                newFileInfo[i] = (FileInfoBase) srcImage.getFileInfo(0).clone();
                newFileInfo[i].setExtents(newDimExtents);
                newFileInfo[i].setImageOrientation(orientation);
                newFileInfo[i].setAxisOrientation(newAxisOrients);
                newFileInfo[i].setResolutions(newResolutions);
                newFileInfo[i].setOrigin(newStartLocations[0], 0);
                newFileInfo[i].setOrigin(newStartLocations[1], 1);
                newFileInfo[i].setOrigin(newStartLocations[2] + (direct[2] * newResolutions[2] * i), 2);
                newFileInfo[i].setSliceSpacing(newResolutions[2]);

                if (newFileInfo[i].getFileFormat() == FileBase.MINC) {
                    newFileInfo[i].setRescaleSlope(((FileInfoMinc) newFileInfo[i]).calculateSlope(maximums[i],
                                                                                                  minimums[i]));
                    newFileInfo[i].setRescaleIntercept(((FileInfoMinc) newFileInfo[i]).calculateIntercept(minimums[i],
                                                                                                          newFileInfo[i].getRescaleSlope()));
                }
            }

            destImage.setFileInfo(newFileInfo);
        } else { // If file is DICOM...

            FileInfoDicom[] newDicomInfo = new FileInfoDicom[newDimExtents[2]];
            StringBuffer newTagPixelSpc = null;
            StringBuffer newTagSliceSpc = null;
            String imageOrient = null;
            TransMatrix matrix = null;
            float fSliceLocation = 0.f;

            FileInfoDicom oldDicomInfo = (FileInfoDicom) srcImage.getFileInfo(0);
            String[] tmp = oldDicomInfo.parseTagValue("0028,0030");

            // pixel spacing, slice thickness, and spacing between slices changes for
            // X- or Y-axis rotation, but not for Z-axis rotation.  Also should not be set if
            // it wasn't there in the first place.
            if ((rotateAxis != Z_AXIS_PLUS) && (rotateAxis != Z_AXIS_PLUS) && (tmp != null)) {
                newTagPixelSpc = new StringBuffer(Float.toString(newResolutions[0]) + "\\" +
                                                  Float.toString(newResolutions[1]));
                newTagSliceSpc = new StringBuffer(Float.toString(newResolutions[2]));
            }

            matrix = oldDicomInfo.getPatientOrientation();

            if (matrix != null) {
                rotMatrix.timesEquals(matrix);
                imageOrient = matrixToDICOMString(rotMatrix);
            }

            for (i = 0; i < newDimExtents[2]; i++) {

                // more correct information for a Z-axis rotation, so copy the file info on a slice basis
                if ((rotateAxis == Z_AXIS_PLUS) || (rotateAxis == Z_AXIS_MINUS)) {
                    newDicomInfo[i] = (FileInfoDicom) srcImage.getFileInfo(i).clone();
                } // not possible for other rotations because the z-dimension is different
                else {
                    newDicomInfo[i] = (FileInfoDicom) srcImage.getFileInfo(0).clone();
                }

                newDicomInfo[i].setExtents(newDimExtents);
                newDicomInfo[i].setImageOrientation(orientation);
                newDicomInfo[i].setAxisOrientation(newAxisOrients);
                newDicomInfo[i].setResolutions(newResolutions);
                newDicomInfo[i].setOrigin(newStartLocations);
                newDicomInfo[i].setOrigin(newStartLocations[2] + (direct[2] * newResolutions[2] * i), 2);
                newDicomInfo[i].setValue("0028,0011", new Short((short) newDimExtents[0]), 2); // columns
                newDicomInfo[i].setValue("0028,0010", new Short((short) newDimExtents[1]), 2); // rows
                newDicomInfo[i].setValue("0020,0013", Short.toString((short) (i + 1)),
                                         Short.toString((short) (i + 1)).length()); // instance number

                // if wasn't previously set, don't set it; if Z-axis rotation, don't set it
                if (newTagPixelSpc != null) {
                    newDicomInfo[i].setValue("0028,0030", newTagPixelSpc.toString(), newTagPixelSpc.length()); // pixel spacing
                }

                if (newTagSliceSpc != null) {
                    newDicomInfo[i].setValue("0018,0050", newTagSliceSpc.toString(), newTagSliceSpc.length()); // slice thickness
                    newDicomInfo[i].setValue("0018,0088", newTagSliceSpc.toString(), newTagSliceSpc.length()); // spacing between slices
                }

                newDicomInfo[i].setValue("0020,0037", imageOrient, imageOrient.length());

                String position = null;
                DecimalFormat nf = new DecimalFormat("##0.000000");
                String sliceLocation = null;

                if (orientation == FileInfoBase.SAGITTAL) {
                    fSliceLocation = newStartLocations[0] + (direct[0] * i * newResolutions[0]);
                    position = positionToDICOMString(fSliceLocation, newStartLocations[1], newStartLocations[2]);
                    sliceLocation = nf.format(fSliceLocation);
                } else if (orientation == FileInfoBase.CORONAL) {
                    fSliceLocation = newStartLocations[1] + (direct[1] * i * newResolutions[1]);
                    position = positionToDICOMString(newStartLocations[0], fSliceLocation, newStartLocations[2]);
                    sliceLocation = nf.format(fSliceLocation);
                } else if (orientation == FileInfoBase.AXIAL) {
                    fSliceLocation = newStartLocations[2] + (direct[2] * i * newResolutions[2]);
                    position = positionToDICOMString(newStartLocations[0], newStartLocations[1], fSliceLocation);
                    sliceLocation = nf.format(fSliceLocation);
                }

                newDicomInfo[i].setValue("0020,1041", sliceLocation, sliceLocation.length()); // image location
                newDicomInfo[i].setValue("0020,0032", position, position.length());
            }

            destImage.setFileInfo(newDicomInfo);
        }

        destImage.calcMinMax();
        destImage.releaseLock();
        setCompleted(true);
    }

    /**
     * Calculates the new start locations based on image orientation.
     *
     * @param  newLoc  float[] buffer to store the new start locations
     */
    private void calcStartLocations(float[] newLoc) {

        float[] oldLoc = srcImage.getFileInfo()[0].getOrigin();
        float[] oldRes = srcImage.getFileInfo()[0].getResolutions();
        int[] oldDims = srcImage.getExtents();

        if (srcImage.getNDims() == 3) {
            newLoc[0] = oldLoc[0];
            newLoc[1] = oldLoc[1];
            newLoc[2] = oldLoc[2];

            if (srcImage.getFileInfo(0).getImageOrientation() == FileInfoBase.SAGITTAL) {

                if ((rotateAxis == X_AXIS_PLUS) || (rotateAxis == Y_AXIS_MINUS)) {
                    newLoc[0] = oldLoc[0] - ((oldDims[2] - 1) * oldRes[2]);
                } else if ((rotateAxis == X_AXIS_MINUS) || (rotateAxis == Z_AXIS_PLUS)) {
                    newLoc[2] = oldLoc[2] - ((oldDims[1] - 1) * oldRes[1]);
                } else if ((rotateAxis == Y_AXIS_PLUS) || (rotateAxis == Z_AXIS_MINUS)) {
                    newLoc[1] = oldLoc[1] + ((oldDims[0] - 1) * oldRes[0]);
                }
            } else if (srcImage.getFileInfo(0).getImageOrientation() == FileInfoBase.AXIAL) {

                if ((rotateAxis == X_AXIS_PLUS) || (rotateAxis == Y_AXIS_MINUS)) {
                    newLoc[2] = oldLoc[2] + ((oldDims[2] - 1) * oldRes[2]);
                } else if ((rotateAxis == X_AXIS_MINUS) || (rotateAxis == Z_AXIS_PLUS)) {
                    newLoc[1] = oldLoc[1] + ((oldDims[1] - 1) * oldRes[1]);
                } else if ((rotateAxis == Y_AXIS_PLUS) || (rotateAxis == Z_AXIS_MINUS)) {
                    newLoc[0] = oldLoc[0] + ((oldDims[0] - 1) * oldRes[0]);
                }
            } else if (srcImage.getFileInfo(0).getImageOrientation() == FileInfoBase.CORONAL) {

                if ((rotateAxis == X_AXIS_PLUS) || (rotateAxis == Y_AXIS_MINUS)) {
                    newLoc[1] = oldLoc[1] + ((oldDims[2] - 1) * oldRes[2]);
                } else if ((rotateAxis == X_AXIS_MINUS) || (rotateAxis == Z_AXIS_PLUS)) {
                    newLoc[2] = oldLoc[2] - ((oldDims[1] - 1) * oldRes[1]);
                } else if ((rotateAxis == Y_AXIS_PLUS) || (rotateAxis == Z_AXIS_MINUS)) {
                    newLoc[0] = oldLoc[0] + ((oldDims[0] - 1) * oldRes[0]);
                }
            } else {

                if ((rotateAxis == X_AXIS_PLUS) || (rotateAxis == Y_AXIS_MINUS)) {
                    newLoc[1] = oldLoc[1] - ((oldDims[2] - 1) * oldRes[2]);
                    newLoc[2] = oldLoc[2] - ((oldDims[1] - 1) * oldRes[1]);
                } else if ((rotateAxis == X_AXIS_MINUS) || (rotateAxis == Z_AXIS_PLUS)) {
                    newLoc[0] = oldLoc[0] - ((oldDims[1] - 1) * oldRes[1]);
                    newLoc[1] = oldLoc[1] - ((oldDims[0] - 1) * oldRes[0]);
                } else if ((rotateAxis == Y_AXIS_PLUS) || (rotateAxis == Z_AXIS_MINUS)) {
                    newLoc[0] = oldLoc[0] - ((oldDims[2] - 1) * oldRes[2]);
                    newLoc[2] = oldLoc[2] - ((oldDims[0] - 1) * oldRes[0]);
                }
            }
        }

    }

    /**
     * Constructs a string of the contruction parameters and outputs the string to the message frame if the logging
     * procedure is turned on.
     */
    private void constructLog() {

        if (rotateAxis == Y_AXIS_PLUS) {
            historyString = new String("Rotate(Y_AXIS_PLUS" + ")\n");
        } else if (rotateAxis == Y_AXIS_MINUS) {
            historyString = new String("Rotate(Y_AXIS_MINUS" + ")\n");
        } else if (rotateAxis == X_AXIS_PLUS) {
            historyString = new String("Rotate(X_AXIS_PLUS" + ")\n");
        } else if (rotateAxis == X_AXIS_MINUS) {
            historyString = new String("Rotate(X_AXIS_MINUS" + ")\n");
        } else if (rotateAxis == Z_AXIS_PLUS) {
            historyString = new String("Rotate(Z_AXIS_PLUS" + ")\n");
        } else {
            historyString = new String("Rotate(Z_AXIS_MINUS" + ")\n");
        }
    }

    /**
     * Convert the matrix to the String format (decimal string) to be stored in the DICOM tag (0020,0037) image
     * (patient) orientation.
     *
     * @param   matrix  Transformation matrix to be converted.
     *
     * @return  The string version of the transformation matrix (i.e. the directional cosines for the first two rows of
     *          the matrix delimited by "\")
     */
    private String matrixToDICOMString(TransMatrix matrix) {
        double[][] dMatrix = matrix.getArray();
        String strMatrix = new String();

        DecimalFormat nf = new DecimalFormat("##0.0000000");

        strMatrix = nf.format(dMatrix[0][0]) + "\\" + nf.format(dMatrix[0][1]) + "\\" + nf.format(dMatrix[0][2]) +
                    "\\" + nf.format(dMatrix[1][0]) + "\\" + nf.format(dMatrix[1][1]) + "\\" + nf.format(dMatrix[1][2]);

        return strMatrix;
    }

    /**
     * Convert the image position to the String format (decimal string) to be stored in the DICOM tag (0020,0032) image
     * (patient) orientation.
     *
     * @param   pt0  X position of patient.
     * @param   pt1  Y position of patient.
     * @param   pt2  Z position of patient.
     *
     * @return  The string version of the patient position delimited by "\".
     */
    private String positionToDICOMString(double pt0, double pt1, double pt2) {
        String strPosition = new String();

        DecimalFormat nf = new DecimalFormat("##0.0#####");

        strPosition = nf.format(pt0) + "\\" + nf.format(pt1) + "\\" + nf.format(pt2);

        return strPosition;

    }

}
