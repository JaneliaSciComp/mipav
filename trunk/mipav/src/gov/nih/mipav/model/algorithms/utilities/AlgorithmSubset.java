package gov.nih.mipav.model.algorithms.utilities;


import gov.nih.mipav.model.algorithms.*;
import gov.nih.mipav.model.file.*;
import gov.nih.mipav.model.structures.*;

import gov.nih.mipav.view.*;

import java.io.*;


/**
 * Algorithm to create a 3D subset image from a 4D image. The user specifies the dimension to remove - x, y, z, or t and
 * the value of the removed dimension.
 */
public class AlgorithmSubset extends AlgorithmBase {

    //~ Static fields/initializers -------------------------------------------------------------------------------------

    /** Remove x dimension. */
    public static final int REMOVE_X = 0;

    /** Remove y dimension. */
    public static final int REMOVE_Y = 1;

    /** Remove z dimension. */
    public static final int REMOVE_Z = 2;

    /** Remove t dimension. */
    public static final int REMOVE_T = 3;

    //~ Instance fields ------------------------------------------------------------------------------------------------

    /** Dimension to be removed. */
    private int removeDim;

    /** Slice size, volume size. */
    private int slice, volume;

    /** Slice value for removed dimension. */
    private int sliceNum;

    /** Size of volume in x, y, z, and time. */
    private int xDim, yDim, zDim, tDim;

    //~ Constructors ---------------------------------------------------------------------------------------------------

    /**
     * import source and destination images into the class.
     *
     * @param  srcImage   source image (image to clip from)
     * @param  destImage  destination image (image to paste to)
     * @param  removeDim  the dimension to be removed
     * @param  sliceNum   slice value for removed dimension
     */
    public AlgorithmSubset(ModelImage srcImage, ModelImage destImage, int removeDim, int sliceNum) {
        super(destImage, srcImage);
        this.removeDim = removeDim;
        this.sliceNum = sliceNum;

        // get local attributes from this.srcImage
        xDim = srcImage.getExtents()[0];
        yDim = srcImage.getExtents()[1];
        zDim = srcImage.getExtents()[2];
        tDim = srcImage.getExtents()[3];
    }

    //~ Methods --------------------------------------------------------------------------------------------------------

    /**
     * Prepares this class for destruction.
     */
    public void finalize() {
        super.finalize();
    }

    /**
     * Runs the algorithm.
     */
    public void runAlgorithm() {
        int z;
        float[] destResolutions;
        int[] destUnitsOfMeasure;
        int axisOfChange = 2;
        float[] imageBuffer;
        float[] nextPositionCoords;
        float[] imagePositionCoords;
        int t;

        constructLog();

        try {
            destResolutions = new float[3];
            destUnitsOfMeasure = new int[3];
            nextPositionCoords = new float[3];
            imagePositionCoords = new float[3];
        } catch (OutOfMemoryError e) {
            imageBuffer = null;
            System.gc();
            displayError("AlgorithmSubset reports: Out of memory");
            setCompleted(false);
            disposeProgressBar();

            return;
        }

        if (removeDim == REMOVE_T) {
            volume = xDim * yDim * zDim;
            destResolutions[0] = srcImage.getFileInfo(0).getResolutions()[0];
            destResolutions[1] = srcImage.getFileInfo(0).getResolutions()[1];
            destResolutions[2] = srcImage.getFileInfo(0).getResolutions()[2];
            destUnitsOfMeasure[0] = srcImage.getFileInfo(0).getUnitsOfMeasure()[0];
            destUnitsOfMeasure[1] = srcImage.getFileInfo(0).getUnitsOfMeasure()[1];
            destUnitsOfMeasure[2] = srcImage.getFileInfo(0).getUnitsOfMeasure()[2];

            try {

                // create  <FileInfoBase fileInfoBuffer;> buffer; may be of type FileInfoDicom
                if (srcImage.isColorImage()) {
                    imageBuffer = new float[4 * volume];
                } else {
                    imageBuffer = new float[volume];
                }

                buildProgressBar(srcImage.getImageName(), "Creating 3D subset...", 0, 100);
            } catch (OutOfMemoryError e) {
                imageBuffer = null;
                System.gc();
                displayError("AlgorithmSubset reports: Out of memory");
                setCompleted(false);
                disposeProgressBar();

                return;
            }

            // make a location & view the progressbar; make length & increment of progressbar.
            initProgressBar();

            // No DICOM 4D images
            if ((srcImage.getFileInfo()[0]).getFileFormat() == FileBase.DICOM) {

                // determine along which axis the imagePositionCoords the image varies
                if (srcImage.getFileInfo(1) != null) {
                    imagePositionCoords = convertIntoFloat(((FileInfoDicom) srcImage.getFileInfo(0)).parseTagValue("0020,0032"));
                    nextPositionCoords = convertIntoFloat(((FileInfoDicom) srcImage.getFileInfo(1)).parseTagValue("0020,0032"));

                    // check along which axis the image coords change --- so far, only deal with orthogonal basis axis
                    // to figure out which axis the slice changes in, check the first slice and the second slice for a
                    // a difference along the basis.
                    if ((nextPositionCoords[0] != imagePositionCoords[0]) &&
                            (nextPositionCoords[1] == imagePositionCoords[1]) &&
                            (nextPositionCoords[2] == imagePositionCoords[2])) { // change ONLY in X-axis
                        axisOfChange = 0;
                    } else if ((nextPositionCoords[0] == imagePositionCoords[0]) &&
                                   (nextPositionCoords[1] != imagePositionCoords[1]) &&
                                   (nextPositionCoords[2] == imagePositionCoords[2])) { // change ONLY in Y-axis
                        axisOfChange = 1;
                    } else if ((nextPositionCoords[0] == imagePositionCoords[0]) &&
                                   (nextPositionCoords[1] == imagePositionCoords[1]) &&
                                   (nextPositionCoords[2] != imagePositionCoords[2])) { // change ONLY in Z-axis
                        axisOfChange = 2;
                    } else { // change ONLY in ANY OTHER axis
                        MipavUtil.displayWarning("Remove Slices does not support changes in\n" +
                                                 "image position (DICOM tag 0020,0032)\n" +
                                                 "in more than one dimension.");
                        setCompleted(false);

                        return;
                    }
                }
            }

            if (threadStopped) {
                imageBuffer = null;
                finalize();

                return;
            }

            try {

                // try copying the sliceNum slice out of srcImage, making it the entire destination
                if (srcImage.isColorImage()) {
                    srcImage.exportData(sliceNum * 4 * volume, 4 * volume, imageBuffer);
                    destImage.importData(0, imageBuffer, true);
                } else {
                    srcImage.exportData(sliceNum * volume, volume, imageBuffer);
                    destImage.importData(0, imageBuffer, true);
                }
            } catch (IOException error) {
                displayError("AlgorithmSubset reports: Destination image already locked.");
                setCompleted(false);

                return;
            }

            FileInfoBase fileInfoBuffer; // buffer of any old type

            for (z = 0; (z < zDim) && !threadStopped; z++) {
                fileInfoBuffer = (FileInfoBase) srcImage.getFileInfo((sliceNum * zDim) + z).clone();
                fileInfoBuffer.setExtents(destImage.getExtents());
                fileInfoBuffer.setResolutions(destResolutions);
                fileInfoBuffer.setUnitsOfMeasure(destUnitsOfMeasure);
                destImage.setFileInfo(fileInfoBuffer, z);
            }

            if (threadStopped) {
                imageBuffer = null;
                finalize();

                return;
            }
        } // if (removeDim == REMOVE_T)
        else if (removeDim == REMOVE_Z) {
            slice = xDim * yDim;
            volume = slice * zDim;
            destResolutions[0] = srcImage.getFileInfo(0).getResolutions()[0];
            destResolutions[1] = srcImage.getFileInfo(0).getResolutions()[1];
            destResolutions[2] = srcImage.getFileInfo(0).getResolutions()[3];
            destUnitsOfMeasure[0] = srcImage.getFileInfo(0).getUnitsOfMeasure()[0];
            destUnitsOfMeasure[1] = srcImage.getFileInfo(0).getUnitsOfMeasure()[1];
            destUnitsOfMeasure[2] = srcImage.getFileInfo(0).getUnitsOfMeasure()[3];

            try {

                if (srcImage.isColorImage()) {
                    imageBuffer = new float[4 * slice];
                } else {
                    imageBuffer = new float[slice];
                }

                buildProgressBar(srcImage.getImageName(), "Creating 3D subset...", 0, 100);
            } catch (OutOfMemoryError e) {
                imageBuffer = null;
                System.gc();
                displayError("AlgorithmSubset reports: Out of memory");
                setCompleted(false);
                disposeProgressBar();

                return;
            }

            // make a location & view the progressbar; make length & increment of progressbar.
            initProgressBar();

            for (t = 0; (t < tDim) && !threadStopped; t++) {

                if (isProgressBarVisible()) {
                    progressBar.updateValue(Math.round((float) (t) / (tDim - 1) * 100), runningInSeparateThread);
                }

                try {

                    // try copying the sliceNum slice out of srcImage, making it the entire destination
                    if (srcImage.isColorImage()) {
                        srcImage.exportData((t * 4 * volume) + (sliceNum * 4 * slice), 4 * slice, imageBuffer);
                        destImage.importData(t * 4 * slice, imageBuffer, false);
                    } else {
                        srcImage.exportData((t * volume) + (sliceNum * slice), slice, imageBuffer);
                        destImage.importData(t * slice, imageBuffer, false);
                    }
                } catch (IOException error) {
                    displayError("AlgorithmSubset reports: Destination image already locked.");
                    setCompleted(false);

                    return;
                }
            } // for (t = 0; t < tDim; t++)

            if (threadStopped) {
                imageBuffer = null;
                finalize();

                return;
            }

            FileInfoBase fileInfoBuffer; // buffer of any old type

            for (t = 0; (t < tDim) && !threadStopped; t++) {
                fileInfoBuffer = (FileInfoBase) srcImage.getFileInfo((t * zDim) + sliceNum).clone();
                fileInfoBuffer.setExtents(destImage.getExtents());
                fileInfoBuffer.setResolutions(destResolutions);
                fileInfoBuffer.setUnitsOfMeasure(destUnitsOfMeasure);
                destImage.setFileInfo(fileInfoBuffer, t);
            }

            if (threadStopped) {
                imageBuffer = null;
                finalize();

                return;
            }

            destImage.calcMinMax();
        } // else if (removeDim == REMOVE_Z)
        else if (removeDim == REMOVE_Y) {
            slice = xDim * zDim;
            destResolutions[0] = srcImage.getFileInfo(0).getResolutions()[0];
            destResolutions[1] = srcImage.getFileInfo(0).getResolutions()[2];
            destResolutions[2] = srcImage.getFileInfo(0).getResolutions()[3];
            destUnitsOfMeasure[0] = srcImage.getFileInfo(0).getUnitsOfMeasure()[0];
            destUnitsOfMeasure[1] = srcImage.getFileInfo(0).getUnitsOfMeasure()[2];
            destUnitsOfMeasure[2] = srcImage.getFileInfo(0).getUnitsOfMeasure()[3];

            try {

                if (srcImage.isColorImage()) {
                    imageBuffer = new float[4 * slice];
                } else {
                    imageBuffer = new float[slice];
                }

                buildProgressBar(srcImage.getImageName(), "Creating 3D subset...", 0, 100);
            } catch (OutOfMemoryError e) {
                imageBuffer = null;
                System.gc();
                displayError("AlgorithmSubset reports: Out of memory");
                setCompleted(false);
                disposeProgressBar();

                return;
            }

            // make a location & view the progressbar; make length & increment of progressbar.
            initProgressBar();

            for (t = 0; (t < tDim) && !threadStopped; t++) {

                if (isProgressBarVisible()) {
                    progressBar.updateValue(Math.round((float) (t) / (tDim - 1) * 100), runningInSeparateThread);
                }

                try {

                    // try copying the sliceNum slice out of srcImage, making it the entire destination
                    if (srcImage.isColorImage()) {
                        srcImage.exportRGBSliceXZ(t, sliceNum, imageBuffer);
                        destImage.importData(t * 4 * slice, imageBuffer, false);
                    } else {
                        srcImage.exportSliceXZ(t, sliceNum, imageBuffer);
                        destImage.importData(t * slice, imageBuffer, false);
                    }
                } catch (IOException error) {
                    displayError("AlgorithmSubset reports: Destination image already locked.");
                    setCompleted(false);

                    return;
                }
            } // for (t = 0; t < tDim; t++)

            if (threadStopped) {
                imageBuffer = null;
                finalize();

                return;
            }

            FileInfoBase fileInfoBuffer; // buffer of any old type

            for (t = 0; (t < tDim) && !threadStopped; t++) {
                fileInfoBuffer = (FileInfoBase) srcImage.getFileInfo(0).clone();
                fileInfoBuffer.setExtents(destImage.getExtents());
                fileInfoBuffer.setResolutions(destResolutions);
                fileInfoBuffer.setUnitsOfMeasure(destUnitsOfMeasure);
                destImage.setFileInfo(fileInfoBuffer, t);
            }

            if (threadStopped) {
                imageBuffer = null;
                finalize();

                return;
            }

            destImage.calcMinMax();
        } // else if (removeDim == REMOVE_Y)
        else { // removeDim == REMOVE_X
            slice = zDim * yDim;
            destResolutions[0] = srcImage.getFileInfo(0).getResolutions()[1];
            destResolutions[1] = srcImage.getFileInfo(0).getResolutions()[2];
            destResolutions[2] = srcImage.getFileInfo(0).getResolutions()[3];
            destUnitsOfMeasure[0] = srcImage.getFileInfo(0).getUnitsOfMeasure()[1];
            destUnitsOfMeasure[1] = srcImage.getFileInfo(0).getUnitsOfMeasure()[2];
            destUnitsOfMeasure[2] = srcImage.getFileInfo(0).getUnitsOfMeasure()[3];

            try {

                if (srcImage.isColorImage()) {
                    imageBuffer = new float[4 * slice];
                } else {
                    imageBuffer = new float[slice];
                }

                buildProgressBar(srcImage.getImageName(), "Creating 3D subset...", 0, 100);
            } catch (OutOfMemoryError e) {
                imageBuffer = null;
                System.gc();
                displayError("AlgorithmSubset reports: Out of memory");
                setCompleted(false);
                disposeProgressBar();

                return;
            }

            // make a location & view the progressbar; make length & increment of progressbar.
            initProgressBar();

            for (t = 0; (t < tDim) && !threadStopped; t++) {

                if (isProgressBarVisible()) {
                    progressBar.updateValue(Math.round((float) (t) / (tDim - 1) * 100), runningInSeparateThread);
                }

                try {

                    // try copying the sliceNum slice out of srcImage, making it the entire destination
                    if (srcImage.isColorImage()) {
                        srcImage.exportRGBSliceZY(t, sliceNum, imageBuffer);
                        destImage.importData(t * 4 * slice, imageBuffer, false);
                    } else {
                        srcImage.exportSliceZY(t, sliceNum, imageBuffer);
                        destImage.importData(t * slice, imageBuffer, false);
                    }
                } catch (IOException error) {
                    displayError("AlgorithmSubset reports: Destination image already locked.");
                    setCompleted(false);

                    return;
                }
            } // for (t = 0; t < tDim; t++)

            if (threadStopped) {
                imageBuffer = null;
                finalize();

                return;
            }

            FileInfoBase fileInfoBuffer; // buffer of any old type

            for (t = 0; (t < tDim) && !threadStopped; t++) {
                fileInfoBuffer = (FileInfoBase) srcImage.getFileInfo(0).clone();
                fileInfoBuffer.setExtents(destImage.getExtents());
                fileInfoBuffer.setResolutions(destResolutions);
                fileInfoBuffer.setUnitsOfMeasure(destUnitsOfMeasure);
                destImage.setFileInfo(fileInfoBuffer, t);
            }

            if (threadStopped) {
                imageBuffer = null;
                finalize();

                return;
            }

            destImage.calcMinMax();
        } // else REMOVE_X

        // Clean up and let the calling dialog know that algorithm did its job
        disposeProgressBar();
        setCompleted(true);
    }

    /**
     * Constructs a string of the contruction parameters and outputs the string to the messsage frame if the logging
     * procedure is turned on.
     */
    private void constructLog() {
        String subsetStr = new String();

        if (removeDim == REMOVE_X) {
            subsetStr = "remove at x = ";
        } else if (removeDim == REMOVE_Y) {
            subsetStr = "remove at y = ";
        } else if (removeDim == REMOVE_Z) {
            subsetStr = "remove at z = ";
        } else {
            subsetStr = "remove at t = ";
        }

        subsetStr = subsetStr + sliceNum;
        historyString = new String("AlgorithmSubset(" + subsetStr + ")" + "\n");

    }
}
