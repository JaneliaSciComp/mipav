package gov.nih.mipav.model.algorithms.utilities;


import gov.nih.mipav.model.algorithms.*;
import gov.nih.mipav.model.file.*;
import gov.nih.mipav.model.structures.*;

import java.io.*;

import java.text.*;


/**
 * An Algorithm to rotate 3D or 4D dataset 90 or 180 degrees about X, Y, or Z axis. 2D Images can also be rotated. A new
 * rotated image with modified dimensions and resolutions created and can be accessed through returnImage().
 *
 * @version  1.0 July 25, 2000
 * @author   Harman J. Singh
 * @author   Matthew J. McAuliffe, Ph.D.
 */
public class AlgorithmRotate extends AlgorithmBase {

    //~ Static fields/initializers -------------------------------------------------------------------------------------

    /** Rotate about the x axis 180 degrees. */
    public static final int X_AXIS_180 = 0;

    /** Rotate about the x axis 90 degrees. */
    public static final int X_AXIS_PLUS = 1;

    /** Rotate about the x axis -90 degrees (or 270 degrees). */
    public static final int X_AXIS_MINUS = 2;

    /** Rotate about the y axis 180 degrees. */
    public static final int Y_AXIS_180 = 3;

    /** Rotate about the y axis 90 degrees. */
    public static final int Y_AXIS_PLUS = 4;

    /** Rotate about the y axis -90 degrees (or 270 degrees). */
    public static final int Y_AXIS_MINUS = 5;

    /** Rotate about the z axis 180 degrees. */
    public static final int Z_AXIS_180 = 6;

    /** Rotate about the z axis 90 degrees. */
    public static final int Z_AXIS_PLUS = 7;

    /** Rotate about the z axis -90 degrees (or 270 degrees). */
    public static final int Z_AXIS_MINUS = 8;

    /* axisOrder and axisFlip changed to match the rotateAxis value: */
    private int[] axisOrder = { 0, 1, 2, 3 };
    private boolean[] axisFlip = { false, false, false, false };

    //private TransMatrix rotMatrix = new TransMatrix(4);
    /**
     * Creates new algorithm for rotating. Sets the source image and axis parameter.
     *
     * @param  srcImg      Source image model.
     * @param  rotateMode  Rotate about which axis.
     */
    public AlgorithmRotate(ModelImage srcImg, int[] order, boolean[] flip ) {
        super(null, srcImg);
        for ( int i = 0; i < axisOrder.length; i++ )
        {
            this.axisOrder[i] = order[i];
            this.axisFlip[i] = flip[i];
        }
        //rotMatrix = null;
    }
    
    /**
     * Creates new algorithm for rotating. Sets the source image and axis parameter.
     *
     * @param  srcImg      Source image model.
     * @param  rotateMode  Rotate about which axis.
     */
    public AlgorithmRotate(ModelImage srcImg, int rotateMode) {
        super(null, srcImg);

        if ((rotateMode == Y_AXIS_180) || (rotateMode == Y_AXIS_PLUS) || (rotateMode == Y_AXIS_MINUS) ||
                (rotateMode == X_AXIS_180) || (rotateMode == X_AXIS_PLUS) || (rotateMode == X_AXIS_MINUS) ||
                (rotateMode == Z_AXIS_180) || (rotateMode == Z_AXIS_PLUS) || (rotateMode == Z_AXIS_MINUS)) {
            init(rotateMode);
        } else {
            init(Z_AXIS_PLUS); // default rotate mode
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
        // progressMode = progress;
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

        
        calcInPlace();
    }


    private void init(int rotateAxis)
    {
        if (rotateAxis == X_AXIS_180) {
            axisFlip[1] = true;
            axisFlip[2] = true;
            //rotMatrix.setRotate(180, 0, 0, TransMatrix.DEGREES);
        } else if (rotateAxis == X_AXIS_PLUS) {
            axisOrder[1] = 2;
            axisOrder[2] = 1;
            axisFlip[1] = true;
            //rotMatrix.setRotate(90, 0, 0, TransMatrix.DEGREES);
            // rotMatrix gives y' = -z, z' = y;
        } else if (rotateAxis == X_AXIS_MINUS) {
            axisOrder[1] = 2;
            axisOrder[2] = 1;
            axisFlip[2] = true;
            //rotMatrix.setRotate(-90, 0, 0, TransMatrix.DEGREES);
            // rotMatrix gives y' = z, z' = -y;
        } else if (rotateAxis == Y_AXIS_180) {
            axisFlip[0] = true;
            axisFlip[2] = true;
            //rotMatrix.setRotate(0, 180, 0, TransMatrix.DEGREES);
        } else if (rotateAxis == Y_AXIS_PLUS) {
            axisOrder[0] = 2;
            axisOrder[2] = 0;
            axisFlip[2] = true;
            //rotMatrix.setRotate(0, 90, 0, TransMatrix.DEGREES);
            // rotMatrix gives x' = z, z' = -x
        } else if (rotateAxis == Y_AXIS_MINUS) {
            axisOrder[0] = 2;
            axisOrder[2] = 0;
            axisFlip[0] = true;
            //rotMatrix.setRotate(0, -90, 0, TransMatrix.DEGREES);
            // rotMatrix gives x' = -z, z' = x
        } else if (rotateAxis == Z_AXIS_180) {
            axisFlip[0] = true;
            axisFlip[1] = true;
            //rotMatrix.setRotate(0, 0, 180, TransMatrix.DEGREES);
        } else if (rotateAxis == Z_AXIS_PLUS) {
            axisOrder[0] = 1;
            axisOrder[1] = 0;
            axisFlip[0] = true;
            //rotMatrix.setRotate(0, 0, 90, TransMatrix.DEGREES);
        } else if (rotateAxis == Z_AXIS_MINUS) {
            axisOrder[0] = 1;
            axisOrder[1] = 0;
            axisFlip[1] = true;
            //rotMatrix.setRotate(0, 0, -90, TransMatrix.DEGREES);
        }
    }

    /**
     * Calculates the rotated image and replaces the source image with the rotated image.
     */
    private void calcInPlace() {
        destImage = srcImage.export( axisOrder, axisFlip );
        destImage.setImageName( srcImage.getImageName() );
        int orientation = destImage.getImageOrientation();
        
        destImage.calcMinMax();

        // Set the file info for the new rotated image identical to the original image,
        // and then adjusts the appropriate info.
        // For all image formats other than DICOM
        if (srcImage.getFileInfo(0).getFileFormat() != FileUtility.DICOM) {
            if (srcImage.getNDims() >= 3) {
                destImage.getMatrixHolder().replaceMatrices(srcImage.getMatrixHolder().getMatrices());
            }
        } else { // If file is DICOM...

            
            int[] newDimExtents;
            int[] newAxisOrients;
            float[] newResolutions;
            float[] newStartLocations;
            int[] newUnitsOfMeasure;

            try {
                newDimExtents = new int[destImage.getNDims()];
                newAxisOrients = new int[destImage.getNDims()];
                newResolutions = new float[destImage.getNDims()];
                newUnitsOfMeasure = new int[destImage.getNDims()];
                newStartLocations = new float[destImage.getNDims()];
            } catch (OutOfMemoryError e) {
                errorCleanUp("Algorithm Rotate: Out of memory", true);

                return;
            }

            for (int i = 0; i < destImage.getNDims(); i++) {
                newDimExtents[i] = destImage.getExtents()[i];
                newResolutions[i] = destImage.getResolutions(0)[i];
                newUnitsOfMeasure[i] = destImage.getUnitsOfMeasure()[i];
                newStartLocations[i] = destImage.getOrigin()[i];

                if (i < 3) {
                    newAxisOrients[i] = destImage.getAxisOrientation()[i];
                }
            }
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
            //if ((rotateAxis != Z_AXIS_180) && (rotateAxis != Z_AXIS_PLUS) && (rotateAxis != Z_AXIS_PLUS) &&
            //        (tmp != null)) {
                newTagPixelSpc = new StringBuffer(Float.toString(newResolutions[0]) + "\\" +
                                                  Float.toString(newResolutions[1]));
                newTagSliceSpc = new StringBuffer(Float.toString(newResolutions[2]));
            //}

            matrix = oldDicomInfo.getPatientOrientation();
            if (matrix != null) {
                imageOrient = matrixToDICOMString(matrix);
            }

            FileDicomTagTable[] childTagTables = new FileDicomTagTable[newDimExtents[2] - 1];

            // first create all of the new file infos (reference and children) and fill them with tags from the old
            // file info.  some of these tag values will be overridden in the next loop
            for (int i = 0; i < newDimExtents[2]; i++) {

                if (i == 0) {

                    // create a new reference file info
                    newDicomInfo[0] = new FileInfoDicom(oldDicomInfo.getFileName(), oldDicomInfo.getFileDirectory(),
                                                        oldDicomInfo.getFileFormat());
                    newDicomInfo[0].vr_type = oldDicomInfo.vr_type;
                    newDicomInfo[0].setDataType(oldDicomInfo.getDataType());
                } else {

                    // all other slices are children of the first file info..
                    newDicomInfo[i] = new FileInfoDicom(oldDicomInfo.getFileName(), oldDicomInfo.getFileDirectory(),
                                                        oldDicomInfo.getFileFormat(), newDicomInfo[0]);
                    
                    newDicomInfo[i].vr_type = oldDicomInfo.vr_type;
                    newDicomInfo[i].setDataType(oldDicomInfo.getDataType());

                    childTagTables[i - 1] = newDicomInfo[i].getTagTable();
                }

                if (axisOrder[2] == 2) { // z-axis doesn't change

                    // more correct information for a Z-axis rotation, so copy the file info on a slice basis
                    newDicomInfo[i].getTagTable().importTags((FileInfoDicom) srcImage.getFileInfo(i));
                } else {

                    // not possible for other rotations because the z-dimension is different
                    newDicomInfo[i].getTagTable().importTags(oldDicomInfo);
                }
            }

            newDicomInfo[0].getTagTable().attachChildTagTables(childTagTables);

            
            for (int i = 0; i < newDimExtents[2]; i++) {

                newDicomInfo[i] = (FileInfoDicom) srcImage.getFileInfo(0).clone();
                newDicomInfo[i].setExtents(newDimExtents);
                newDicomInfo[i].setImageOrientation(orientation);
                newDicomInfo[i].setAxisOrientation(newAxisOrients);
                newDicomInfo[i].setResolutions(newResolutions);
                newDicomInfo[i].setOrigin(newStartLocations);
                newDicomInfo[i].setOrigin(newStartLocations[2] + (newResolutions[2] * i), 2);
                newDicomInfo[i].getTagTable().setValue("0028,0011", new Short((short) newDimExtents[0]), 2); // columns
                newDicomInfo[i].getTagTable().setValue("0028,0010", new Short((short) newDimExtents[1]), 2); // rows
                newDicomInfo[i].getTagTable().setValue("0020,0013", Short.toString((short) (i + 1)),
                                                       Short.toString((short) (i + 1)).length()); // instance number

                // if wasn't previously set, don't set it; if Z-axis rotation, don't set it
                if (newTagPixelSpc != null) {
                    newDicomInfo[i].getTagTable().setValue("0028,0030", newTagPixelSpc.toString(),
                                                           newTagPixelSpc.length()); // pixel spacing
                }

                if (newTagSliceSpc != null) {
                    newDicomInfo[i].getTagTable().setValue("0018,0050", newTagSliceSpc.toString(),
                                                           newTagSliceSpc.length()); // slice thickness
                    newDicomInfo[i].getTagTable().setValue("0018,0088", newTagSliceSpc.toString(),
                                                           newTagSliceSpc.length()); // spacing between slices
                }

                if ((imageOrient != null) && (newDicomInfo[i].getTagTable().getValue("0020,0037") != null)) {
                    newDicomInfo[i].getTagTable().setValue("0020,0037", imageOrient, imageOrient.length());
                }

                String position = null;
                DecimalFormat nf = new DecimalFormat("##0.000000");
                String sliceLocation = null;

                if (orientation == FileInfoBase.SAGITTAL) {
                    fSliceLocation = newStartLocations[0] + (i * newResolutions[0]);
                    position = positionToDICOMString(fSliceLocation, newStartLocations[1], newStartLocations[2]);
                    sliceLocation = nf.format(fSliceLocation);
                } else if (orientation == FileInfoBase.CORONAL) {
                    fSliceLocation = newStartLocations[1] + (i * newResolutions[1]);
                    position = positionToDICOMString(newStartLocations[0], fSliceLocation, newStartLocations[2]);
                    sliceLocation = nf.format(fSliceLocation);
                } else if (orientation == FileInfoBase.AXIAL) {
                    fSliceLocation = newStartLocations[2] + (i * newResolutions[2]);
                    position = positionToDICOMString(newStartLocations[0], newStartLocations[1], fSliceLocation);
                    sliceLocation = nf.format(fSliceLocation);
                }

                if (newDicomInfo[i].getTagTable().getValue("0020,1041") != null) {
                    newDicomInfo[i].getTagTable().setValue("0020,1041", sliceLocation, sliceLocation.length()); // image location
                }

                if (newDicomInfo[i].getTagTable().getValue("0020,0032") != null) {
                    newDicomInfo[i].getTagTable().setValue("0020,0032", position, position.length());
                }
            }
            
            destImage.setFileInfo(newDicomInfo);
            if (srcImage.getNDims() >= 3) {
                destImage.getMatrixHolder().replaceMatrices(srcImage.getMatrixHolder().getMatrices());
            }
        }

        destImage.releaseLock();
        setCompleted(true);
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
        String strMatrix = new String();

        DecimalFormat nf = new DecimalFormat("##0.0000000");

        strMatrix = nf.format(matrix.Get(0, 0)) + "\\" + nf.format(matrix.Get(0, 1)) + "\\" + nf.format(matrix.Get(0, 2)) +
                    "\\" + nf.format(matrix.Get(1, 0)) + "\\" + nf.format(matrix.Get(1, 1)) + "\\" + nf.format(matrix.Get(1, 2));

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
