package gov.nih.mipav.model.algorithms.utilities;


import gov.nih.mipav.model.algorithms.AlgorithmBase;
import gov.nih.mipav.model.file.*;
import gov.nih.mipav.model.scripting.parameters.ActionEnums.ImageDimension;
import gov.nih.mipav.model.structures.ModelImage;

import gov.nih.mipav.view.*;

import java.io.IOException;


/**
 * Algorithm to create a 3D subset image from a 4D image. The user specifies the dimension to remove - x, y, z, or t and
 * the value of the removed dimension.
 */
public class AlgorithmSubset extends AlgorithmBase {
    /** @deprecated See ActionEnums.ImageDimension.X */
    public static final int REMOVE_X = 0;

    /** @deprecated See ActionEnums.ImageDimension.Y */
    public static final int REMOVE_Y = 1;

    /** @deprecated See ActionEnums.ImageDimension.Z */
    public static final int REMOVE_Z = 2;

    /** @deprecated See ActionEnums.ImageDimension.T */
    public static final int REMOVE_T = 3;

    // ~ Instance fields
    // ------------------------------------------------------------------------------------------------

    /** Dimension to be removed. */
    private ImageDimension removeDim;

    /** Slice value for removed dimension. */
    private final int sliceNum;

    // ~ Constructors
    // ---------------------------------------------------------------------------------------------------

    /**
     * import source and destination images into the class.
     * 
     * @param srcImage source image (image to clip from)
     * @param destImage destination image (image to paste to)
     * @param removeDim the dimension to be removed
     * @param sliceNum slice value for removed dimension
     */
    public AlgorithmSubset(final ModelImage srcImage, final ModelImage destImage, final ImageDimension removeDim,
            final int sliceNum) {
        super(destImage, srcImage);
        this.removeDim = removeDim;
        this.sliceNum = sliceNum;
    }

    /**
     * import source and destination images into the class.
     * 
     * @deprecated Use the constructor that takes an ImageDimension.
     * 
     * @param srcImage source image (image to clip from)
     * @param destImage destination image (image to paste to)
     * @param removeDim the dimension to be removed
     * @param sliceNum slice value for removed dimension
     */
    public AlgorithmSubset(final ModelImage srcImage, final ModelImage destImage, final int removeDimInt,
            final int sliceNum) {
        super(destImage, srcImage);
        this.sliceNum = sliceNum;

        switch (removeDimInt) {
            case REMOVE_X:
                removeDim = ImageDimension.X;
                break;
            case REMOVE_Y:
                removeDim = ImageDimension.Y;
                break;
            case REMOVE_Z:
                removeDim = ImageDimension.Z;
                break;
            case REMOVE_T:
                removeDim = ImageDimension.T;
                break;
            default:
                removeDim = ImageDimension.T;
                Preferences.debug("Unrecognized removeDimInt: " + removeDimInt + ".  Defaulting to T dimension.\n",
                        Preferences.DEBUG_ALGORITHM);
        }
    }

    // ~ Methods
    // --------------------------------------------------------------------------------------------------------

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
        float[] destResolutions;
        int[] destUnitsOfMeasure;
        float[] imageBuffer;

        try {
            destResolutions = new float[3];
            destUnitsOfMeasure = new int[3];
        } catch (final OutOfMemoryError e) {
            imageBuffer = null;
            System.gc();
            displayError("AlgorithmSubset reports: Out of memory");
            setCompleted(false);
            return;
        }

        // No DICOM 4D images
        if (removeDim == ImageDimension.T) {
            if ( (srcImage.getFileInfo()[0]).getFileFormat() == FileUtility.DICOM) {

                // determine along which axis the imagePositionCoords the image varies
                if (srcImage.getFileInfo(1) != null) {
                    final float[] imagePositionCoords = convertIntoFloat( ((FileInfoDicom) srcImage.getFileInfo(0))
                            .parseTagValue("0020,0032"));
                    final float[] nextPositionCoords = convertIntoFloat( ((FileInfoDicom) srcImage.getFileInfo(1))
                            .parseTagValue("0020,0032"));

                    // check along which axis the image coords change --- so far, only deal with orthogonal basis axis
                    // to figure out which axis the slice changes in, check the first slice and the second slice for a
                    // a difference along the basis.
                    if ( (nextPositionCoords[0] != imagePositionCoords[0])
                            && (nextPositionCoords[1] == imagePositionCoords[1])
                            && (nextPositionCoords[2] == imagePositionCoords[2])) { // change ONLY in X-axis
                        // axisOfChange = 0;
                    } else if ( (nextPositionCoords[0] == imagePositionCoords[0])
                            && (nextPositionCoords[1] != imagePositionCoords[1])
                            && (nextPositionCoords[2] == imagePositionCoords[2])) { // change ONLY in Y-axis
                        // axisOfChange = 1;
                    } else if ( (nextPositionCoords[0] == imagePositionCoords[0])
                            && (nextPositionCoords[1] == imagePositionCoords[1])
                            && (nextPositionCoords[2] != imagePositionCoords[2])) { // change ONLY in Z-axis
                        // axisOfChange = 2;
                    } else { // change ONLY in ANY OTHER axis
                        MipavUtil.displayWarning("Remove Slices does not support changes in\n"
                                + "image position (DICOM tag 0020,0032)\n" + "in more than one dimension.");
                        setCompleted(false);

                        return;
                    }
                }
            }
        }

        /* axisFlip is always false: */
        final boolean[] axisFlip = {false, false, false};

        /* set the axisOrder variable for remapping coordinate axes: */
        final int[] axisOrder = {0, 1, 2}; /* no remapping is the default (REMOVE_T or REMOVE_Z) */
        if (removeDim == ImageDimension.Y) {
            /* get the ZY slice: */
            axisOrder[1] = 2;
            axisOrder[2] = 1;
        } else if (removeDim == ImageDimension.X) {
            /* get the XZ slice: */
            axisOrder[0] = 2;
            axisOrder[2] = 0;
        }

        /* The third axis, is 2 for REMOVE_T condition, 3 otherwise: */
        final int axis3 = (removeDim == ImageDimension.T) ? 2 : 3;

        /* calculate slice size: */
        int slice = (srcImage.getExtents()[axisOrder[0]] * srcImage.getExtents()[axisOrder[1]]);
        if (removeDim == ImageDimension.T) {
            slice *= srcImage.getExtents()[axis3];
        }

        destResolutions[0] = srcImage.getFileInfo(0).getResolutions()[axisOrder[0]];
        destResolutions[1] = srcImage.getFileInfo(0).getResolutions()[axisOrder[1]];
        destResolutions[2] = srcImage.getFileInfo(0).getResolutions()[axis3];
        destUnitsOfMeasure[0] = srcImage.getFileInfo(0).getUnitsOfMeasure()[axisOrder[0]];
        destUnitsOfMeasure[1] = srcImage.getFileInfo(0).getUnitsOfMeasure()[axisOrder[1]];
        destUnitsOfMeasure[2] = srcImage.getFileInfo(0).getUnitsOfMeasure()[axis3];

        /* If this is a color image: */
        final int buffFactor = (srcImage.isColorImage()) ? 4 : 1;
        try {
            imageBuffer = new float[buffFactor * slice];
            fireProgressStateChanged(srcImage.getImageName(), "Creating 3D subset...");
        } catch (final OutOfMemoryError e) {
            imageBuffer = null;
            System.gc();
            displayError("AlgorithmSubset reports: Out of memory");
            setCompleted(false);
            return;
        }

        /* If REMOVE_T export the desired volume: */
        if (removeDim == ImageDimension.T) {
            try {
                srcImage.exportData(sliceNum * buffFactor * slice, buffFactor * slice, imageBuffer);
                destImage.importData(0, imageBuffer, true);
            } catch (final IOException error) {
                displayError("AlgorithmSubset reports: Destination image already locked.");
                setCompleted(false);
                return;
            }
        }
        /* Othewise export the remapped slices one at a time: */
        else {
            // make a location & view the progressbar; make length & increment of progressbar.
            final int tDim = srcImage.getExtents()[3];
            for (int t = 0; (t < tDim) && !threadStopped; t++) {
                fireProgressStateChanged(Math.round((float) (t) / (tDim - 1) * 100));

                try {
                    srcImage.export(axisOrder, axisFlip, t, sliceNum, imageBuffer);
                    destImage.importData(t * buffFactor * slice, imageBuffer, false);
                } catch (final IOException error) {
                    displayError("AlgorithmSubset reports: Destination image already locked.");
                    setCompleted(false);
                    return;
                }
            } // for (t = 0; t < tDim; t++)
        }
        if (threadStopped) {
            imageBuffer = null;
            finalize();
            return;
        }

        /* copy srcImage FileInfoBase information into destImage : */
        FileInfoBase fileInfoBuffer; // buffer of any old type
        final int dim3 = srcImage.getExtents()[axis3];
        for (int d = 0; (d < dim3) && !threadStopped; d++) {
            fileInfoBuffer = (FileInfoBase) srcImage.getFileInfo(0).clone();
            fileInfoBuffer.setExtents(destImage.getExtents());
            fileInfoBuffer.setResolutions(destResolutions);
            fileInfoBuffer.setUnitsOfMeasure(destUnitsOfMeasure);
            destImage.setFileInfo(fileInfoBuffer, d);
        }

        if (threadStopped) {
            imageBuffer = null;
            finalize();
            return;
        }
        destImage.calcMinMax();

        // Clean up and let the calling dialog know that algorithm did its job
        setCompleted(true);
    }
}
