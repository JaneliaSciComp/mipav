package gov.nih.mipav.model.algorithms.utilities;


import gov.nih.mipav.model.algorithms.*;
import gov.nih.mipav.model.file.*;
import gov.nih.mipav.model.structures.*;

import java.io.*;

import java.text.*;
import java.util.Iterator;
import java.util.LinkedHashMap;


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

        if ((rotateMode == Y_AXIS_180) || (rotateMode == Y_AXIS_PLUS) || (rotateMode == Y_AXIS_MINUS) ||
                (rotateMode == X_AXIS_180) || (rotateMode == X_AXIS_PLUS) || (rotateMode == X_AXIS_MINUS) ||
                (rotateMode == Z_AXIS_180) || (rotateMode == Z_AXIS_PLUS) || (rotateMode == Z_AXIS_MINUS)) {
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

    /**
     * Calculates the rotated image and replaces the source image with the rotated image.
     */
    private void calcInPlace() {
        boolean doVOI = false;
        ModelImage tmpMask = null;
        ModelImage maskImage = null;
        short [] maskBuffer = null;
        float [] maskSliceBuffer = null;
        // 2 NIFTI matrices
        TransMatrix matrixQ = null;
        TransMatrix matrixS = null;
        if ((srcImage.getVOIs() != null) && (!srcImage.getVOIs().isEmpty()) && (srcImage.getNDims() <= 3)) {
            doVOI = true;
        }

        /* axisOrder and axisFlip changed to match the rotateAxis value: */
        int[] axisOrder = { 0, 1, 2, 3 };
        boolean[] axisFlip = { false, false, false, false };

        TransMatrix rotMatrix = new TransMatrix(4);

        if (rotateAxis == X_AXIS_180) {
            axisFlip[1] = true;
            axisFlip[2] = true;
            rotMatrix.setRotate(180, 0, 0, TransMatrix.DEGREES);
        } else if (rotateAxis == X_AXIS_PLUS) {
            axisOrder[1] = 2;
            axisOrder[2] = 1;
            axisFlip[1] = true;
            rotMatrix.setRotate(90, 0, 0, TransMatrix.DEGREES);
            // rotMatrix gives y' = -z, z' = y;
        } else if (rotateAxis == X_AXIS_MINUS) {
            axisOrder[1] = 2;
            axisOrder[2] = 1;
            axisFlip[2] = true;
            rotMatrix.setRotate(-90, 0, 0, TransMatrix.DEGREES);
            // rotMatrix gives y' = z, z' = -y;
        } else if (rotateAxis == Y_AXIS_180) {
            axisFlip[0] = true;
            axisFlip[2] = true;
            rotMatrix.setRotate(0, 180, 0, TransMatrix.DEGREES);
        } else if (rotateAxis == Y_AXIS_PLUS) {
            axisOrder[0] = 2;
            axisOrder[2] = 0;
            axisFlip[2] = true;
            rotMatrix.setRotate(0, 90, 0, TransMatrix.DEGREES);
            // rotMatrix gives x' = z, z' = -x
        } else if (rotateAxis == Y_AXIS_MINUS) {
            axisOrder[0] = 2;
            axisOrder[2] = 0;
            axisFlip[0] = true;
            rotMatrix.setRotate(0, -90, 0, TransMatrix.DEGREES);
            // rotMatrix gives x' = -z, z' = x
        } else if (rotateAxis == Z_AXIS_180) {
            axisFlip[0] = true;
            axisFlip[1] = true;
            rotMatrix.setRotate(0, 0, 180, TransMatrix.DEGREES);
        } else if (rotateAxis == Z_AXIS_PLUS) {
            axisOrder[0] = 1;
            axisOrder[1] = 0;
            axisFlip[0] = true;
            rotMatrix.setRotate(0, 0, 90, TransMatrix.DEGREES);
        } else if (rotateAxis == Z_AXIS_MINUS) {
            axisOrder[0] = 1;
            axisOrder[1] = 0;
            axisFlip[1] = true;
            rotMatrix.setRotate(0, 0, -90, TransMatrix.DEGREES);
        }

        int buffFactor = 1;

        if (srcImage.isColorImage()) {
            buffFactor = 4; // ARGB Image
        } else if ((srcImage.getType() == ModelStorageBase.COMPLEX) ||
                       (srcImage.getType() == ModelStorageBase.DCOMPLEX)) {
            buffFactor = 2;
        }

        int slice = buffFactor * srcImage.getExtents()[axisOrder[0]] * srcImage.getExtents()[axisOrder[1]];
        float[] sliceBuffer = new float[slice];
        int tDim = 1;
        int volume = 1;
        int zDim = 1;

        if (srcImage.getNDims() == 4) {
            zDim = srcImage.getExtents()[2];
            tDim = srcImage.getExtents()[3];
            volume = slice * zDim;
        } else if (srcImage.getNDims() == 3) {
            zDim = srcImage.getExtents()[2];
        }

        int yDim = srcImage.getExtents()[1];
        int xDim = srcImage.getExtents()[0];
        int x, y, c;
        float[] oldVol;
        float[] newVol;
        int oldSlice = buffFactor * xDim * yDim;
        int newSlice;
        int oldXDim = buffFactor * xDim;
        int newXDim;
        int vol = buffFactor * xDim * yDim * zDim;

        int[] newDimExtents;
        int[] newAxisOrients;
        float[] newResolutions;
        float[] newStartLocations;
        int[] newUnitsOfMeasure;
        FileInfoBase[] newFileInfo;

        try {
            newDimExtents = new int[srcImage.getNDims()];
            // setAxisOrientation requires length 3
            newAxisOrients = new int[3];
            newResolutions = new float[srcImage.getNDims()];
            newUnitsOfMeasure = new int[srcImage.getNDims()];
            newStartLocations = new float[srcImage.getNDims()];
        } catch (OutOfMemoryError e) {
            errorCleanUp("Algorithm Rotate: Out of memory", true);

            return;
        }

        for (int i = 0; i < srcImage.getNDims(); i++) {
            newDimExtents[i] = srcImage.getExtents()[axisOrder[i]];
            newResolutions[i] = srcImage.getResolutions(0)[axisOrder[i]];
            newUnitsOfMeasure[i] = srcImage.getUnitsOfMeasure()[axisOrder[i]];
            newStartLocations[i] = srcImage.getOrigin()[axisOrder[i]];

            if (i < 3) {
                newAxisOrients[i] = srcImage.getAxisOrientation()[axisOrder[i]];

                if (axisFlip[i]) {
                    newAxisOrients[i] = FileInfoBase.oppositeOrient(newAxisOrients[i]);
                }
            }
        }

        destImage = new ModelImage(srcImage.getType(), newDimExtents, srcImage.getImageName());
        if (doVOI) {
            maskImage = srcImage.generateShortImage(1);
            tmpMask = new ModelImage(ModelImage.SHORT, newDimExtents, "VOI Mask");
            if ((rotateAxis == X_AXIS_PLUS) || (rotateAxis == X_AXIS_MINUS) ||
                (rotateAxis == Y_AXIS_PLUS) || (rotateAxis == Y_AXIS_MINUS)) {
                maskBuffer = new short[xDim * yDim * zDim];
                
                try {
                    maskImage.exportData(0, xDim * yDim * zDim, maskBuffer); // locks and releases lock
                } catch (IOException error) {
                    displayError("AlgorithmRotate: maskImage locked");
                    setCompleted(false);
    
                    return;
                }   
            } // if ((rotateAxis == X_AXIS_PLUS) || (rotateAxis == X_AXIS_MINUS) ||
            else {
                maskSliceBuffer = new float[srcImage.getExtents()[axisOrder[0]] * srcImage.getExtents()[axisOrder[1]]];
            }
        } // if (doVOI)

        /* Export the ModelImage data, remapping the axes: */
        if (rotateAxis == X_AXIS_PLUS) {
            oldVol = new float[vol];
            newVol = new float[vol];
            newSlice = buffFactor * xDim * zDim;
            newXDim = buffFactor * xDim;

            for (int t = 0; (t < tDim) && !threadStopped; t++) {

                try {
                    srcImage.exportData(t * vol, vol, oldVol);
                } catch (IOException error) {
                    displayError("AlgorithmSubset reports: Destination image already locked.");
                    setCompleted(false);

                    return;
                }

                for (int z = 0; (z < zDim) && !threadStopped; z++) {
                    fireProgressStateChanged(Math.round((float) ((t * zDim) + z) / ((tDim * zDim) - 1) * 100));

                    for (y = 0; y < yDim; y++) {

                        for (x = 0; x < xDim; x++) {
                            if (doVOI) {
                                tmpMask.set(x, (zDim - 1- z), y, maskBuffer[(z * oldSlice) + (y * oldXDim) + x]);
                            }
                            for (c = 0; c < buffFactor; c++) {
                                newVol[(y * newSlice) + ((zDim - 1 - z) * newXDim) + (x * buffFactor) + c] = oldVol[(z *
                                                                                                                         oldSlice) +
                                                                                                                    (y *
                                                                                                                         oldXDim) +
                                                                                                                    (x *
                                                                                                                         buffFactor) +
                                                                                                                    c];
                            }
                        }
                    }
                }

                try {
                    destImage.importData(t * vol, newVol, false);
                } catch (IOException error) {
                    displayError("AlgorithmSubset reports: Destination image already locked.");
                    setCompleted(false);

                    return;
                }
            }
        } // if (rotateAxis == X_AXIS_PLUS)
        else if (rotateAxis == X_AXIS_MINUS) {
            oldVol = new float[vol];
            newVol = new float[vol];
            newSlice = buffFactor * xDim * zDim;
            newXDim = buffFactor * xDim;

            for (int t = 0; (t < tDim) && !threadStopped; t++) {

                try {
                    srcImage.exportData(t * vol, vol, oldVol);
                } catch (IOException error) {
                    displayError("AlgorithmSubset reports: Destination image already locked.");
                    setCompleted(false);

                    return;
                }

                for (int z = 0; (z < zDim) && !threadStopped; z++) {
                    fireProgressStateChanged(Math.round((float) ((t * zDim) + z) / ((tDim * zDim) - 1) * 100));

                    for (y = 0; y < yDim; y++) {

                        for (x = 0; x < xDim; x++) {
                            if (doVOI) {
                                tmpMask.set(x, z, yDim - 1 - y, maskBuffer[(z * oldSlice) + (y * oldXDim) + x]);
                            }
                            for (c = 0; c < buffFactor; c++) {
                                newVol[((yDim - 1 - y) * newSlice) + (z * newXDim) + (x * buffFactor) + c] = oldVol[(z *
                                                                                                                         oldSlice) +
                                                                                                                    (y *
                                                                                                                         oldXDim) +
                                                                                                                    (x *
                                                                                                                         buffFactor) +
                                                                                                                    c];
                            }
                        }
                    }
                }

                try {
                    destImage.importData(t * vol, newVol, false);
                } catch (IOException error) {
                    displayError("AlgorithmSubset reports: Destination image already locked.");
                    setCompleted(false);

                    return;
                }
            }
        } // else if (rotateAxis == X_AXIS_MINUS)
        else if (rotateAxis == Y_AXIS_PLUS) {
            oldVol = new float[vol];
            newVol = new float[vol];
            newSlice = buffFactor * yDim * zDim;
            newXDim = buffFactor * zDim;

            for (int t = 0; (t < tDim) && !threadStopped; t++) {

                try {
                    srcImage.exportData(t * vol, vol, oldVol);
                } catch (IOException error) {
                    displayError("AlgorithmSubset reports: Destination image already locked.");
                    setCompleted(false);

                    return;
                }

                for (int z = 0; (z < zDim) && !threadStopped; z++) {
                    fireProgressStateChanged(Math.round((float) ((t * zDim) + z) / ((tDim * zDim) - 1) * 100));

                    for (y = 0; y < yDim; y++) {

                        for (x = 0; x < xDim; x++) {
                            if (doVOI) {
                                tmpMask.set(z, y, (xDim - 1 - x), maskBuffer[(z * oldSlice) + (y * oldXDim) + x]);
                            }

                            for (c = 0; c < buffFactor; c++) {
                                newVol[((xDim - 1 - x) * newSlice) + (y * newXDim) + (z * buffFactor) + c] = oldVol[(z *
                                                                                                                         oldSlice) +
                                                                                                                    (y *
                                                                                                                         oldXDim) +
                                                                                                                    (x *
                                                                                                                         buffFactor) +
                                                                                                                    c];
                            }
                        }
                    }
                }

                try {
                    destImage.importData(t * vol, newVol, false);
                } catch (IOException error) {
                    displayError("AlgorithmSubset reports: Destination image already locked.");
                    setCompleted(false);

                    return;
                }
            }
        } // else if (rotateAxis == Y_AXIS_PLUS)
        else if (rotateAxis == Y_AXIS_MINUS) {
            oldVol = new float[vol];
            newVol = new float[vol];
            newSlice = buffFactor * yDim * zDim;
            newXDim = buffFactor * zDim;

            for (int t = 0; (t < tDim) && !threadStopped; t++) {

                try {
                    srcImage.exportData(t * vol, vol, oldVol);
                } catch (IOException error) {
                    displayError("AlgorithmSubset reports: Destination image already locked.");
                    setCompleted(false);

                    return;
                }

                for (int z = 0; (z < zDim) && !threadStopped; z++) {
                    fireProgressStateChanged(Math.round((float) ((t * zDim) + z) / ((tDim * zDim) - 1) * 100));

                    for (y = 0; y < yDim; y++) {

                        for (x = 0; x < xDim; x++) {
                            if (doVOI) {
                                tmpMask.set((zDim - 1 - z), y, x, maskBuffer[(z * oldSlice) + (y * oldXDim) + x]);
                            }
                            for (c = 0; c < buffFactor; c++) {
                                newVol[(x * newSlice) + (y * newXDim) + ((zDim - 1 - z) * buffFactor) + c] = oldVol[(z *
                                                                                                                         oldSlice) +
                                                                                                                    (y *
                                                                                                                         oldXDim) +
                                                                                                                    (x *
                                                                                                                         buffFactor) +
                                                                                                                    c];
                            }
                        }
                    }
                }

                try {
                    destImage.importData(t * vol, newVol, false);
                } catch (IOException error) {
                    displayError("AlgorithmSubset reports: Destination image already locked.");
                    setCompleted(false);

                    return;
                }
            }
        } // else if (rotateAxis == Y_AXIS_MINUS)
        else {

            for (int t = 0; (t < tDim) && !threadStopped; t++) {

                for (int z = 0; (z < zDim) && !threadStopped; z++) {
                    fireProgressStateChanged(Math.round((float) ((t * zDim) + z) / ((tDim * zDim) - 1) * 100));

                    try {
                        srcImage.export(axisOrder, axisFlip, t, z, sliceBuffer);
                        destImage.importData((t * volume) + (z * slice), sliceBuffer, false);
                    } catch (IOException error) {
                        displayError("AlgorithmRotate reports: Destination image already locked.");
                        setCompleted(false);

                        return;
                    }
                    
                    if (doVOI) {
                        try {
                            maskImage.export(axisOrder, axisFlip, t, z, maskSliceBuffer);
                            tmpMask.importData((t * volume) + (z * slice), maskSliceBuffer, false);
                        } catch (IOException error) {
                            displayError("AlgorithmRotate reports: tmpMask locked.");
                            setCompleted(false);

                            return;
                        }    
                    }
                }
            }
        } // else

        if (threadStopped) {
            sliceBuffer = null;
            finalize();

            return;
        }

        destImage.calcMinMax();

        float min = (float) destImage.getMin();
        float max = (float) destImage.getMax();

        int orientation = FileInfoBase.UNKNOWN_ORIENT;

        if (srcImage.getNDims() >= 3) {

            if (newAxisOrients[2] != FileInfoBase.ORI_UNKNOWN_TYPE) {

                if ((newAxisOrients[2] == FileInfoBase.ORI_A2P_TYPE) ||
                        (newAxisOrients[2] == FileInfoBase.ORI_P2A_TYPE)) {
                    orientation = FileInfoBase.CORONAL;
                } else if ((newAxisOrients[2] == FileInfoBase.ORI_S2I_TYPE) ||
                               (newAxisOrients[2] == FileInfoBase.ORI_I2S_TYPE)) {
                    orientation = FileInfoBase.AXIAL;
                } else if ((newAxisOrients[2] == FileInfoBase.ORI_R2L_TYPE) ||
                               (newAxisOrients[2] == FileInfoBase.ORI_L2R_TYPE)) {
                    orientation = FileInfoBase.SAGITTAL;
                }
            }
        }

        calcStartLocations(newStartLocations);
        
        if (srcImage.getFileInfo()[0] instanceof FileInfoNIFTI) {
            int i, j;
            MatrixHolder matHolder = null;
            matHolder = srcImage.getMatrixHolder();

            if (matHolder != null) {
                LinkedHashMap<String, TransMatrix> matrixMap = matHolder.getMatrixMap();
                Iterator<String> iter = matrixMap.keySet().iterator();
                String nextKey = null;
                
                TransMatrix tempMatrix = null;
                
                while (iter.hasNext()) {
                    nextKey = iter.next();
                    tempMatrix = matrixMap.get(nextKey);
                    if (tempMatrix.isNIFTI()) { 
                        
                        if (tempMatrix.isQform()) {
                            matrixQ = tempMatrix.clone();
                            TransMatrix rotQ = rotMatrix.clone();
                            rotQ.Mult(matrixQ);
                            for (i = 0; i < 4; i++) {
                                for (j = 0; j < 4; j++) {
                                    matrixQ.set(i, j, rotQ.get(i, j));        
                                }
                            }
                            rotQ = null;
                        } // if (tempMatrix.isQform())
                        else { // tempMatrix is sform
                            matrixS = tempMatrix.clone();
                            TransMatrix rotS = rotMatrix.clone();
                            rotS.Mult(matrixS);
                            for (i = 0; i < 4; i++) {
                                for (j = 0; j < 4; j++) {
                                    matrixS.set(i, j, rotS.get(i, j));        
                                }
                            }
                            rotS = null;
                        } // else tempMatrix is sform
                    }
                }
            } // if (matHolder != null)    
        } // if (fileInfo[0] instanceof FileInfoNIFTI)

        // Set the file info for the new rotated image identical to the original image,
        // and then adjusts the appropriate info.
        // For all image formats other than DICOM
        if (srcImage.getFileInfo(0).getFileFormat() != FileUtility.DICOM) {
            int newZDimExtents;
            int index;
            if (srcImage.getNDims() >= 3) {
                newFileInfo = new FileInfoBase[newDimExtents[2]*tDim];
                newZDimExtents = newDimExtents[2];
            }
            else {
                newFileInfo = new FileInfoBase[1];
                newZDimExtents = 1;
            }

            for (int t = 0; t < tDim; t++) {
                for (int i = 0; i < newZDimExtents; i++) {
                    index = i + t * newZDimExtents;
                    newFileInfo[index] = (FileInfoBase) srcImage.getFileInfo(0).clone();
                    newFileInfo[index].setExtents(newDimExtents);
                    newFileInfo[index].setImageOrientation(orientation);
                    newFileInfo[index].setAxisOrientation(newAxisOrients);
                    newFileInfo[index].setResolutions(newResolutions);
                    newFileInfo[index].setUnitsOfMeasure(newUnitsOfMeasure);
                    newFileInfo[index].setOrigin(newStartLocations[0], 0);
                    newFileInfo[index].setOrigin(newStartLocations[1], 1);
    
                    if (srcImage.getNDims() >= 3) {
                        newFileInfo[index].setOrigin(newStartLocations[2] + (newResolutions[2] * i), 2);
                        newFileInfo[index].setSliceThickness(newResolutions[2]);
                    }
    
                    if (newFileInfo[index].getFileFormat() == FileUtility.MINC) {
                        newFileInfo[index].setRescaleSlope(FileInfoMinc.calculateSlope(min, max, 
                        		newFileInfo[index].getMax(), newFileInfo[index].getMin()));
                        
                        newFileInfo[index].setRescaleIntercept(FileInfoMinc.calculateIntercept(min, newFileInfo[index].getRescaleSlope(),
                        		newFileInfo[index].getMin()));
                    }
                    
                    if (matrixQ != null) {
                        ((FileInfoNIFTI)newFileInfo[index]).setMatrixQ(matrixQ);
                    }
                    if (matrixS != null) {
                        ((FileInfoNIFTI)newFileInfo[index]).setMatrixS(matrixS);
                    }
                }
            }

            destImage.setFileInfo(newFileInfo);
            if (srcImage.getNDims() >= 3) {
                System.err.println("doing new matrix operation");
                if (srcImage.getFileInfo()[0] instanceof FileInfoNIFTI) {
                    MatrixHolder matHolder = null;
                    matHolder = destImage.getMatrixHolder();
                    matHolder.clearMatrices();
                    if (matrixQ != null) {
                        matHolder.addMatrix(matrixQ);
                    }
                    if (matrixS != null) {
                        matHolder.addMatrix(matrixS);
                    }
                } // if (fileInfo[0] instanceof FileInfoNIFTI)
                else {
                    destImage.getMatrixHolder().replaceMatrices(srcImage.getMatrixHolder().getMatrices());
                    TransMatrix tMat = new TransMatrix(rotMatrix);
                    tMat.setTransformID(TransMatrix.TRANSFORM_ANOTHER_DATASET);
                    tMat.Set(0, 3, newFileInfo[0].getOrigin()[0]);
                    tMat.Set(1, 3, newFileInfo[0].getOrigin()[1]);
                    tMat.Set(2, 3, newFileInfo[0].getOrigin()[2]);
                    System.err.println("new matrix added: " + tMat);
                    destImage.getMatrixHolder().addMatrix(tMat);
                }
            }
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
            if ((rotateAxis != Z_AXIS_180) && (rotateAxis != Z_AXIS_PLUS) && (rotateAxis != Z_AXIS_PLUS) &&
                    (tmp != null)) {
                newTagPixelSpc = new StringBuffer(Float.toString(newResolutions[0]) + "\\" +
                                                  Float.toString(newResolutions[1]));
                newTagSliceSpc = new StringBuffer(Float.toString(newResolutions[2]));
            }

            matrix = oldDicomInfo.getPatientOrientation();

            if (matrix != null) {
                rotMatrix.Mult(matrix);
                imageOrient = matrixToDICOMString(rotMatrix);
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

                if ((rotateAxis == Z_AXIS_180) || (rotateAxis == Z_AXIS_PLUS) || (rotateAxis == Z_AXIS_MINUS)) {

                    // more correct information for a Z-axis rotation, so copy the file info on a slice basis
                    newDicomInfo[i].getTagTable().importTags((FileInfoDicom) srcImage.getFileInfo(i));
                } else {

                    // not possible for other rotations because the z-dimension is different
                    newDicomInfo[i].getTagTable().importTags(oldDicomInfo);
                }
            }

            newDicomInfo[0].getTagTable().attachChildTagTables(childTagTables);

            for (int i = 0; i < newDimExtents[2]; i++) {
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

                if (newDicomInfo[i].getTagTable().getValue("0020,0037") != null) {
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

                TransMatrix tMat = new TransMatrix(rotMatrix);
                tMat.setTransformID(TransMatrix.TRANSFORM_ANOTHER_DATASET);
                tMat.Set(0, 3, newDicomInfo[0].getOrigin()[0]);
                tMat.Set(1, 3, newDicomInfo[0].getOrigin()[1]);
                tMat.Set(2, 3, newDicomInfo[0].getOrigin()[2]);
                System.err.println("new matrix added: " + tMat);
                destImage.getMatrixHolder().addMatrix(tMat);
            }
        }
        
        if (doVOI) {
            // ******* Make algorithm for VOI extraction.
            tmpMask.calcMinMax();

            AlgorithmVOIExtraction VOIExtAlgo = new AlgorithmVOIExtraction(tmpMask);
            VOIExtAlgo.setRunningInSeparateThread(runningInSeparateThread);
            VOIExtAlgo.run();

            VOIVector resultVOIs = tmpMask.getVOIs();
            VOIVector srcVOIs = srcImage.getVOIs();

            for (int ii = 0; ii < resultVOIs.size(); ii++) {
                int id = ((VOI) (resultVOIs.elementAt(ii))).getID();

                for (int jj = 0; jj < srcVOIs.size(); jj++) {

                    if (((VOI) (srcVOIs.elementAt(jj))).getID() == id) {
                        ((VOI) (resultVOIs.elementAt(ii))).setName(((VOI) (srcVOIs.elementAt(jj))).getName());
                    }
                }
            }

            destImage.setVOIs(tmpMask.getVOIs());
            tmpMask.disposeLocal();
            maskImage.disposeLocal();    
        } // if (doVOI)

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

            if (rotateAxis == X_AXIS_180) {

                if (oldLoc[1] > 0.0f) {
                    newLoc[1] = oldLoc[1] - ((oldDims[1] - 1) * oldRes[1]);
                } else {
                    newLoc[1] = oldLoc[1] + ((oldDims[1] - 1) * oldRes[1]);
                }

                if (oldLoc[2] > 0.0f) {
                    newLoc[2] = oldLoc[2] - ((oldDims[2] - 1) * oldRes[2]);
                } else {
                    newLoc[2] = oldLoc[2] + ((oldDims[2] - 1) * oldRes[2]);
                }
            } else if (rotateAxis == X_AXIS_PLUS) {

                if (oldLoc[2] > 0.0f) {
                    newLoc[1] = oldLoc[2] - ((oldDims[2] - 1) * oldRes[2]);
                } else {
                    newLoc[1] = oldLoc[2] + ((oldDims[2] - 1) * oldRes[2]);
                }

                newLoc[2] = oldLoc[1];
            } else if (rotateAxis == X_AXIS_MINUS) {
                newLoc[1] = oldLoc[2];

                if (oldLoc[1] > 0.0f) {
                    newLoc[2] = oldLoc[1] - ((oldDims[1] - 1) * oldRes[1]);
                } else {
                    newLoc[2] = oldLoc[1] + ((oldDims[1] - 1) * oldRes[1]);
                }
            } else if (rotateAxis == Y_AXIS_180) {

                if (oldLoc[0] > 0.0f) {
                    newLoc[0] = oldLoc[0] - ((oldDims[0] - 1) * oldRes[0]);
                } else {
                    newLoc[0] = oldLoc[0] + ((oldDims[0] - 1) * oldRes[0]);
                }

                if (oldLoc[2] > 0.0f) {
                    newLoc[2] = oldLoc[2] - ((oldDims[2] - 1) * oldRes[2]);
                } else {
                    newLoc[2] = oldLoc[2] + ((oldDims[2] - 1) * oldRes[2]);
                }
            } else if (rotateAxis == Y_AXIS_PLUS) {
                newLoc[0] = oldLoc[2];

                if (oldLoc[0] > 0.0f) {
                    newLoc[2] = oldLoc[0] - ((oldDims[0] - 1) * oldRes[0]);
                } else {
                    newLoc[2] = oldLoc[0] + ((oldDims[0] - 1) * oldRes[0]);
                }
            } else if (rotateAxis == Y_AXIS_MINUS) {

                if (oldLoc[2] > 0.0f) {
                    newLoc[0] = oldLoc[2] - ((oldDims[2] - 1) * oldRes[2]);
                } else {
                    newLoc[0] = oldLoc[2] + ((oldDims[2] - 1) * oldRes[2]);
                }

                newLoc[2] = oldLoc[0];
            } else if (rotateAxis == Z_AXIS_180) {

                if (oldLoc[0] > 0.0f) {
                    newLoc[0] = oldLoc[0] - ((oldDims[0] - 1) * oldRes[0]);
                } else {
                    newLoc[0] = oldLoc[0] + ((oldDims[0] - 1) * oldRes[0]);
                }

                if (oldLoc[1] > 0.0f) {
                    newLoc[1] = oldLoc[1] - ((oldDims[1] - 1) * oldRes[1]);
                } else {
                    newLoc[1] = oldLoc[1] + ((oldDims[1] - 1) * oldRes[1]);
                }
            } else if (rotateAxis == Z_AXIS_PLUS) {

                if (oldLoc[1] > 0.0f) {
                    newLoc[0] = oldLoc[1] - ((oldDims[1] - 1) * oldRes[1]);
                } else {
                    newLoc[0] = oldLoc[1] + ((oldDims[1] - 1) * oldRes[1]);
                }

                newLoc[1] = oldLoc[0];
            } else if (rotateAxis == Z_AXIS_MINUS) {
                newLoc[0] = oldLoc[1];

                if (oldLoc[0] > 0.0f) {
                    newLoc[1] = oldLoc[0] - ((oldDims[0] - 1) * oldRes[0]);
                } else {
                    newLoc[1] = oldLoc[0] + ((oldDims[0] - 1) * oldRes[0]);
                }
            }
        }

        for (int m = 0; m < srcImage.getNDims(); m++) {

            if (Math.abs(newLoc[m]) < .000001f) {
                newLoc[m] = 0f;
            }
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
