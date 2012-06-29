package gov.nih.mipav.util;


import gov.nih.mipav.util.MipavMath;

import gov.nih.mipav.model.file.*;
import gov.nih.mipav.model.structures.*;

import WildMagic.LibFoundation.Mathematics.Vector2f;
import WildMagic.LibFoundation.Mathematics.Vector3d;
import WildMagic.LibFoundation.Mathematics.Vector3f;


/**
 * MipavCoordinateSystems class. This class provides the interface layer between objects and classes that display
 * information about the ModelImage class and the ModelImage class itself.
 * 
 * <p>
 * FileCoordinates -- how the ModelImage data is stored on disk and read into the ModelStorageBase 1D array.
 * </p>
 * 
 * <p>
 * ModelCoordinates -- mapping of the Axial, Coronal, and Sagittal FileCoordinates values into x, y, z (0, 1, 2)
 * positions for array indexing (based on the FileInfoBase.AXIAL, CORONAL, and SAGITTIAL constant defines).
 * </p>
 * 
 * <p>
 * PatientCoordinates, the coordinate axes for displaying the Axial, Coronal, and Sagittal views of the ModelImage data.
 * The mapping of Human-Anatomical axes (Left-Right, Anterior-Posterior, Superior-Inferior) onto PatientCoordinates:
 * Axial = { R2L, A2P, I2S }; Coronal = { R2L, S2I, A2P }; Sagittal = { A2P, S2I, R2L }
 * </p>
 * 
 * <p>
 * This class provides static functions for transforming points from one Coordinate Space into another.
 * </p>
 */
public class MipavCoordinateSystems {

    // ~ Methods
    // --------------------------------------------------------------------------------------------------------

    /**
     * FileToModel transform. Transforms points in FileCoordinates into ModelCoordinates. ModelCoordinates map the
     * Axial, Coronal, and Sagittal values from the FileCoordinates point into the x, y, and z positions of the output
     * point.
     * 
     * @param pIn the point in FileCoordinates to be transformed
     * @param pOut return: the transformed point in ModelCoordinates
     * @param image the ModelImage for which the transform is done.
     */
    public static final void fileToModel(final Vector3f pIn, final Vector3f pOut, final ModelStorageBase image) {

        // get the axial, coronal and sagittal positions for this image
        final int[] axialOrder = MipavCoordinateSystems.getAxisOrder(image, FileInfoBase.AXIAL);

        final float[] filePoint = {pIn.X, pIn.Y, pIn.Z};

        // re-map the FileCoordinates point into x=I/S, y=A/P and z=L/R
        pOut.X = filePoint[axialOrder[2]];
        pOut.Y = filePoint[axialOrder[1]];
        pOut.Z = filePoint[axialOrder[0]];
    }

    /**
     * FileToPatient transform. Transforms points that are in FileCoordinates into PatientCoordinates space, based on
     * the ModelImage and the desired oriented view -- either FileInfoBase.AXIAL, FileInfoBase.CORONAL,
     * FileInfoBase.SAGITTAL, or FileInfoBase.UNKNOWN_ORIENT. Always allows for flipping of the axies using the image
     * orientation information as needed.
     * 
     * @param pIn the FileCoordinates Point
     * @param pOut the Point in PatientCoordinates
     * @param image the ModelImage for which the point is being transformed.
     * @param orientation the desired PatientCoordinates orientation.
     */
    public static final void fileToPatient(final Vector3f pIn, final Vector3f pOut, final ModelStorageBase image,
            final int orientation) {
        MipavCoordinateSystems.fileToPatient(pIn, pOut, image, orientation, true);
    }

    /**
     * FileToPatient transform. Transforms points that are in FileCoordinates into PatientCoordinates space, based on
     * the ModelImage and the desired oriented view -- either FileInfoBase.AXIAL, FileInfoBase.CORONAL,
     * FileInfoBase.SAGITTAL, or FileInfoBase.UNKNOWN_ORIENT:
     * 
     * @param pIn the FileCoordinates Point
     * @param pOut the Point in PatientCoordinates
     * @param image the ModelImage for which the point is being transformed.
     * @param orientation the desired PatientCoordinates orientation.
     * @param bFlip when true use axisFlip to flip the PatientCoordinates axis
     */
    public static final void fileToPatient(final Vector3f pIn, final Vector3f pOut, final ModelStorageBase image,
            final int orientation, final boolean bFlip) {

        // axisOrder represents the mapping of FileCoordinates volume axes to PatientCoordinates axes
        final int[] axisOrder = MipavCoordinateSystems.getAxisOrder(image, orientation);

        // axisFlip represents whether to invert the axes after they are reordered
        final boolean[] axisFlip = MipavCoordinateSystems.getAxisFlip(image, orientation);

        MipavCoordinateSystems.fileToPatient(pIn, pOut, image, orientation, bFlip, axisOrder, axisFlip);
    }

    /**
     * FileToPatient transform. Transforms points that are in FileCoordinates into PatientCoordinates space, based on
     * the ModelImage and the desired oriented view -- either FileInfoBase.AXIAL, FileInfoBase.CORONAL,
     * FileInfoBase.SAGITTAL, or FileInfoBase.UNKNOWN_ORIENT:
     * 
     * @param pIn input point in FileCoordinates
     * @param pOut output point in PatientCoordinates
     * @param image input image
     * @param orientation patient orientation
     * @param bFlip flag to use axisFlip for inverting axes
     * @param axisOrder mapping of input and output image orientation axes.
     * @param axisFlip invert flags for the output orientation axes.
     */
    public static final void fileToPatient(final Vector3f pIn, final Vector3f pOut, final ModelStorageBase image,
            final int orientation, final boolean bFlip, final int[] axisOrder, final boolean[] axisFlip) {
        // extents gets the image extents re-mapped to PatientCoordinates
        final int[] extents = image.getExtents(orientation);

        // The modelPoint
        final float[] modelPoint = new float[3];
        modelPoint[0] = pIn.X;
        modelPoint[1] = pIn.Y;
        modelPoint[2] = pIn.Z;

        // transformed patientPoint
        final float[] patientPoint = new float[3];

        // First reorder the point indices based on the axisOrder re-mapping from FileCoordinates to PatientCoordinates
        // space
        for (int i = 0; i < 3; i++) {
            patientPoint[i] = modelPoint[axisOrder[i]];
        }

        // Then invert the point, using the appropriate extents
        for (int i = 0; i < 3; i++) {

            if (axisFlip[i] && bFlip) {
                patientPoint[i] = extents[i] - patientPoint[i] - 1;
            }
        }

        // assign the transformed point to pOut
        pOut.X = MipavMath.round(patientPoint[0]);
        pOut.Y = MipavMath.round(patientPoint[1]);
        pOut.Z = MipavMath.round(patientPoint[2]);
    }

    /**
     * Translates the input point into ScannerCoordinates, based on the input image, kImage.
     * 
     * @param kInput the input point in FileCoordinates
     * @param kOutput the transformed point in ScannerCoordinates
     * @param kImage the image for which the point is being transformed.
     */
    public static final void fileToScanner(final Vector3f kInput, final Vector3f kOutput, final ModelImage kImage) {

        final Vector3f kOriginLPS = MipavCoordinateSystems.originLPS(kImage);

        if ( (kImage.getMatrixHolder().containsType(TransMatrix.TRANSFORM_SCANNER_ANATOMICAL))
                || (kImage.getFileInfo()[0].getFileFormat() == FileUtility.DICOM)) {

            final float[] afResolutions = kImage.getResolutions(0);

            final TransMatrix dicomMatrix = (kImage).getMatrix(); // Gets composite matrix

            Vector3f kScaledInput = new Vector3f(kInput.X * afResolutions[0], kInput.Y * afResolutions[1],
                    kInput.Z * afResolutions[2]);
            
            // Finally convert the point to axial millimeter DICOM space.
            dicomMatrix.transformAsPoint3Df(kScaledInput, kOutput);
        } else {

            final int[] axisOrder = new int[] {0, 1, 2};
            final boolean[] axisFlip = new boolean[] {false, false, false};
            MipavCoordinateSystems.toLPS(kImage, axisOrder, axisFlip);

            final float[] afRes = kImage.getResolutions(0);
            final float[] filePoint = new float[3];
            filePoint[0] = kInput.X;
            filePoint[1] = kInput.Y;
            filePoint[2] = kInput.Z;

            final float[] scannerPoint = new float[3];
            // First reorder the point indices based on the axisOrder re-mapping
            for (int i = 0; i < 3; i++) {
                scannerPoint[i] = filePoint[axisOrder[i]];
            }

            final int[] extents = kImage.getExtents();
            // Then invert the point, using the appropriate extents
            for (int i = 0; i < 3; i++) {
                if (axisFlip[i]) {
                    scannerPoint[i] = extents[axisOrder[i]] - scannerPoint[i] - 1;
                }
            }

            kOutput.X = scannerPoint[0] * afRes[axisOrder[0]];
            kOutput.Y = scannerPoint[1] * afRes[axisOrder[1]];
            if (afRes.length >= 3) {
                kOutput.Z = scannerPoint[2] * afRes[axisOrder[2]];
            }
        }

        // Returned point represents the current position in coronal, sagittal, axial order (L/R, A/P, I/S axis space)
        kOutput.Add(kOriginLPS);
    }

    /**
     * Get the booean axisFlip array that describes how FileCoordinate axes are inverted in PatientCoordinates. The
     * values are determined by the FileInfoBase.ORI_L2R_TYPE, etc. values stored in the input
     * image.FileInfoBase.axisOrientation and by the desired patient viewing orientation.
     * 
     * @param image DOCUMENT ME!
     * @param iOrientation the desired PatientCoordinates view of the data (AXIAL, SAGITTAL, CORONAL)
     * 
     * @return DOCUMENT ME!
     */
    public static final boolean[] getAxisFlip(final ModelStorageBase image, final int iOrientation) {
        return MipavCoordinateSystems.getAxisFlip(image, iOrientation, false);
    }

    /**
     * Get the booean axisFlip array that describes how FileCoordinate axes are inverted in PatientCoordinates. The
     * values are determined by the FileInfoBase.ORI_L2R_TYPE, etc. values stored in the input
     * image.FileInfoBase.axisOrientation and by the desired patient viewing orientation.
     * 
     * @param image DOCUMENT ME!
     * @param iOrientation the desired PatientCoordinates view of the data (AXIAL, SAGITTAL, CORONAL)
     * @param bDICOM whether we are trying to find the axis flip array for a translation into DICOM spaces
     * 
     * @return DOCUMENT ME!
     */
    public static final boolean[] getAxisFlip(final ModelStorageBase image, final int iOrientation, boolean bDICOM) {
        final boolean[] abAxisFlip = {false, false, false};
        final int[] aiAxisOrientation = MipavCoordinateSystems.getAxisOrientation(image);

        if ( (aiAxisOrientation != null) && (iOrientation != FileInfoBase.UNKNOWN_ORIENT)) {

            for (int i = 0; i < 3; i++) {

                if (aiAxisOrientation[i] == FileInfoBase.ORI_L2R_TYPE) {

                    if ( (iOrientation == FileInfoBase.AXIAL) || (iOrientation == FileInfoBase.CORONAL)) {
                        abAxisFlip[0] = true;
                    } else {
                        abAxisFlip[2] = true;
                    }
                } else if (aiAxisOrientation[i] == FileInfoBase.ORI_P2A_TYPE) {

                    if (iOrientation == FileInfoBase.AXIAL) {
                        abAxisFlip[1] = true;
                    } else if (iOrientation == FileInfoBase.SAGITTAL) {
                        abAxisFlip[0] = true;
                    } else {
                        abAxisFlip[2] = true;
                    }
                } else if (aiAxisOrientation[i] == FileInfoBase.ORI_S2I_TYPE) {

                    if (iOrientation == FileInfoBase.AXIAL) {
                        abAxisFlip[2] = true;
                    }
                } else if (aiAxisOrientation[i] == FileInfoBase.ORI_I2S_TYPE) {

                    if (iOrientation != FileInfoBase.AXIAL) {
                        abAxisFlip[1] = true;
                    }
                }
            }
        }

        if ( !image.getRadiologicalView() && (iOrientation != FileInfoBase.SAGITTAL)
                && (iOrientation != FileInfoBase.UNKNOWN_ORIENT) && !bDICOM) {
            abAxisFlip[0] = !abAxisFlip[0];
        }

        return abAxisFlip;

    }

    /**
     * Get the axisOrder array that describes how FileCoordinate axes are remapped for the PatientCoordinates. The
     * axisOrder values are determined by the FileInfoBase.ORI_L2R_TYPE, etc. values stored in the input image and by
     * the desired patient viewing orientation.
     * 
     * @param image DOCUMENT ME!
     * @param iOrientation the desired PatientCoordinates view of the data (AXIAL, SAGITTAL, CORONAL)
     * 
     * @return DOCUMENT ME!
     */
    public static final int[] getAxisOrder(final ModelStorageBase image, final int iOrientation) {
        final int[] aiAxisOrder = new int[3];
        final int[] aiAxisOrientation = MipavCoordinateSystems.getAxisOrientation(image);

        if ( (aiAxisOrientation != null) && (iOrientation != FileInfoBase.UNKNOWN_ORIENT)) {

            for (int i = 0; i < 3; i++) {

                if ( (aiAxisOrientation[i] == FileInfoBase.ORI_R2L_TYPE)
                        || (aiAxisOrientation[i] == FileInfoBase.ORI_L2R_TYPE)) {

                    if ( (iOrientation == FileInfoBase.AXIAL) || (iOrientation == FileInfoBase.CORONAL)) {
                        aiAxisOrder[0] = i;
                    } else {
                        aiAxisOrder[2] = i;
                    }
                } else if ( (aiAxisOrientation[i] == FileInfoBase.ORI_A2P_TYPE)
                        || (aiAxisOrientation[i] == FileInfoBase.ORI_P2A_TYPE)) {

                    if (iOrientation == FileInfoBase.AXIAL) {
                        aiAxisOrder[1] = i;
                    } else if (iOrientation == FileInfoBase.SAGITTAL) {
                        aiAxisOrder[0] = i;
                    } else {
                        aiAxisOrder[2] = i;
                    }
                } else if ( (aiAxisOrientation[i] == FileInfoBase.ORI_S2I_TYPE)
                        || (aiAxisOrientation[i] == FileInfoBase.ORI_I2S_TYPE)) {

                    if (iOrientation == FileInfoBase.AXIAL) {
                        aiAxisOrder[2] = i;
                    } else {
                        aiAxisOrder[1] = i;
                    }
                }
            }
        } else if ( (iOrientation == FileInfoBase.SAGITTAL)) {
            aiAxisOrder[0] = 2;
            aiAxisOrder[1] = 1;
            aiAxisOrder[2] = 0;
        } else if ( (iOrientation == FileInfoBase.CORONAL)) {
            aiAxisOrder[0] = 0;
            aiAxisOrder[1] = 2;
            aiAxisOrder[2] = 1;
        } else {
            aiAxisOrder[0] = 0;
            aiAxisOrder[1] = 1;
            aiAxisOrder[2] = 2;
        }

        return aiAxisOrder;
    }

    /**
     * Inverts the input axisOrder so that points can be mapped back from PatientCoordinates to FileCoordinates. The
     * Inverse map is based only on the input axisOrder map.
     * 
     * @param axisOrder DOCUMENT ME!
     * @param axisOrderInverse DOCUMENT ME!
     */
    public static final void getAxisOrderInverse(final int[] axisOrder, final int[] axisOrderInverse) {

        for (int i = 0; i < 3; i++) {
            axisOrderInverse[axisOrder[i]] = i;
        }
    }

    /**
     * Gets the axisOrientation variables stored in the image.FileInfoBase object for the input ModelImage, image. If
     * the orientation is undefined, the axes are assigned values based on the image orientation:
     * 
     * @param image DOCUMENT ME!
     * 
     * @return DOCUMENT ME!
     */
    public static final int[] getAxisOrientation(final ModelStorageBase image) {
        final int imageOrientation = image.getImageOrientation();

        if (imageOrientation == FileInfoBase.UNKNOWN_ORIENT) {
            return null;
        }

        final int[] aiAxisOrientation = image.getAxisOrientation();

        if (aiAxisOrientation[0] == FileInfoBase.ORI_UNKNOWN_TYPE) {

            if (imageOrientation == FileInfoBase.AXIAL) {
                aiAxisOrientation[0] = FileInfoBase.ORI_R2L_TYPE;
                aiAxisOrientation[1] = FileInfoBase.ORI_A2P_TYPE;
                aiAxisOrientation[2] = FileInfoBase.ORI_I2S_TYPE;
            } else if (imageOrientation == FileInfoBase.CORONAL) {
                aiAxisOrientation[0] = FileInfoBase.ORI_R2L_TYPE;
                aiAxisOrientation[1] = FileInfoBase.ORI_S2I_TYPE;
                aiAxisOrientation[2] = FileInfoBase.ORI_A2P_TYPE;
            } else if (imageOrientation == FileInfoBase.SAGITTAL) {
                aiAxisOrientation[0] = FileInfoBase.ORI_A2P_TYPE;
                aiAxisOrientation[1] = FileInfoBase.ORI_S2I_TYPE;
                aiAxisOrientation[2] = FileInfoBase.ORI_R2L_TYPE;
            }
        }

        return aiAxisOrientation;
    }

    /**
     * Returns the direction vectors for displaying the surface so that it aligns properly with the input ModelImage.
     * The direction vectors are transformed into ModelCoordinates.
     * 
     * @param kImage the ModelImage for which the direction vectors are returned.
     * 
     * @return the direction vectors for the object.
     */
    public static final int[] getModelDirections(final ModelStorageBase kImage) {
        final boolean[] axialFlip = MipavCoordinateSystems.getAxisFlip(kImage, FileInfoBase.AXIAL);

        final int[] aiModelFlip = {1, 1, 1};

        if (axialFlip[1]) {
            aiModelFlip[0] = -1;
        }

        if (axialFlip[0]) {
            aiModelFlip[1] = -1;
        }

        if (axialFlip[2]) {
            aiModelFlip[2] = -1;
        }

        return aiModelFlip;
    }

    /**
     * ModelToFile transform. Transforms points in ModelCoordinates (x = Axial, y = Coronal, z = Sagittal) into
     * FileCoordinates.
     * 
     * @param pIn the point in ModelCoordinates to be transformed
     * @param pOut return: the transformed point in FileCoordinates
     * @param image the ModelImage for which the transform is done.
     */
    public static final void modelToFile(final Vector3f pIn, final Vector3f pOut, final ModelStorageBase image) {

        // get the axial, coronal and sagittal positions for this image
        final int[] axialOrder = MipavCoordinateSystems.getAxisOrder(image, FileInfoBase.AXIAL);

        final float[] modelPoint = new float[3];
        modelPoint[axialOrder[2]] = pIn.X;
        modelPoint[axialOrder[1]] = pIn.Y;
        modelPoint[axialOrder[0]] = pIn.Z;

        pOut.X = modelPoint[0];
        pOut.Y = modelPoint[1];
        pOut.Z = modelPoint[2];
    }

    /**
     * PatientToFile transform. Transforms points that are in PatientCoordinates into FileCoordinates space, based on
     * the ModelImage and the given oriented view -- either FileInfoBase.AXIAL, FileInfoBase.CORONAL,
     * FileInfoBase.SAGITTAL, or FileInfoBase.UNKNOWN_ORIENT. Always allows for flipping of the axies using the image
     * orientation information as needed.
     * 
     * @param pIn the FileCoordinates Point
     * @param pOut the Point in PatientCoordinates
     * @param image the ModelImage for which the point is being transformed.
     * @param orientation the desired PatientCoordinates orientation.
     */
    public static final void patientToFile(final Vector3f pIn, final Vector3f pOut, final ModelStorageBase image,
            final int orientation) {
        MipavCoordinateSystems.patientToFile(pIn, pOut, image, orientation, true);
    }

    /**
     * PatientToFile transform. Transforms points that are in PatientCoordinates into FileCoordinates space, based on
     * the ModelImage and the given oriented view -- either FileInfoBase.AXIAL, FileInfoBase.CORONAL,
     * FileInfoBase.SAGITTAL, or FileInfoBase.UNKNOWN_ORIENT:
     * 
     * @param pIn the PatientCoordinates Point
     * @param pOut the Point in FileCoordinates
     * @param image the ModelImage for which the point is being transformed.
     * @param orientation the given PatientCoordinates orientation.
     * @param bFlip when true use axisFlip to flip the PatientCoordinates axis
     */
    public static final void patientToFile(final Vector3f pIn, final Vector3f pOut, final ModelStorageBase image,
            final int orientation, final boolean bFlip) {

        // axisOrder represents the mapping of FileCoordinates volume axes to PatientCoordinates space axes
        final int[] axisOrder = MipavCoordinateSystems.getAxisOrder(image, orientation);

        // axisFlip represents whether to invert the axes after they are reordered
        final boolean[] axisFlip = MipavCoordinateSystems.getAxisFlip(image, orientation);

        // extents gets the image extents re-mapped to patient coordinates
        final int[] extents = image.getExtents(orientation);

        // axisOrderInverse is the inverse axis re-mapping from PatientCoordinates to FileCoordinates
        final int[] axisOrderInverse = new int[3];

        // get the inverse mapping
        MipavCoordinateSystems.getAxisOrderInverse(axisOrder, axisOrderInverse);

        // input point in PatientCoordinates
        final float[] patientPoint = new float[3];
        patientPoint[0] = pIn.X;
        patientPoint[1] = pIn.Y;
        patientPoint[2] = pIn.Z;

        // output point in FileCoordinates
        final float[] modelPoint = new float[3];

        // First invert
        for (int i = 0; i < 3; i++) {

            if (axisFlip[i] && bFlip) {
                patientPoint[i] = extents[i] - patientPoint[i] - 1;
            }
        }

        // then remap the axes
        for (int i = 0; i < 3; i++) {
            modelPoint[i] = patientPoint[axisOrderInverse[i]];
        }

        // assign the transformed point to pOut
        pOut.X = MipavMath.round(modelPoint[0]);
        pOut.Y = MipavMath.round(modelPoint[1]);
        pOut.Z = MipavMath.round(modelPoint[2]);
    }
    
    /**
     * PatientToFile transform. Transforms points that are in PatientCoordinates into FileCoordinates space, based on
     * the ModelImage and the given oriented view -- either FileInfoBase.AXIAL, FileInfoBase.CORONAL,
     * FileInfoBase.SAGITTAL, or FileInfoBase.UNKNOWN_ORIENT. Always allows for flipping of the axies using the image
     * orientation information as needed.
     * 
     * @param pIn the FileCoordinates Point
     * @param pOut the Point in PatientCoordinates
     * @param image the ModelImage for which the point is being transformed.
     * @param orientation the desired PatientCoordinates orientation.
     */
    public static final void patientToFile(final Vector3d pIn, final Vector3d pOut, final ModelStorageBase image,
            final int orientation) {
        MipavCoordinateSystems.patientToFile(pIn, pOut, image, orientation, true);
    }
    
    
    /**
     * PatientToFile transform. Transforms points that are in PatientCoordinates into FileCoordinates space, based on
     * the ModelImage and the given oriented view -- either FileInfoBase.AXIAL, FileInfoBase.CORONAL,
     * FileInfoBase.SAGITTAL, or FileInfoBase.UNKNOWN_ORIENT:
     * 
     * @param pIn the PatientCoordinates Point
     * @param pOut the Point in FileCoordinates
     * @param image the ModelImage for which the point is being transformed.
     * @param orientation the given PatientCoordinates orientation.
     * @param bFlip when true use axisFlip to flip the PatientCoordinates axis
     */
    public static final void patientToFile(final Vector3d pIn, final Vector3d pOut, final ModelStorageBase image,
            final int orientation, final boolean bFlip) {

        // axisOrder represents the mapping of FileCoordinates volume axes to PatientCoordinates space axes
        final int[] axisOrder = MipavCoordinateSystems.getAxisOrder(image, orientation);

        // axisFlip represents whether to invert the axes after they are reordered
        final boolean[] axisFlip = MipavCoordinateSystems.getAxisFlip(image, orientation);

        // extents gets the image extents re-mapped to patient coordinates
        final int[] extents = image.getExtents(orientation);

        // axisOrderInverse is the inverse axis re-mapping from PatientCoordinates to FileCoordinates
        final int[] axisOrderInverse = new int[3];

        // get the inverse mapping
        MipavCoordinateSystems.getAxisOrderInverse(axisOrder, axisOrderInverse);

        // input point in PatientCoordinates
        final double[] patientPoint = new double[3];
        patientPoint[0] = pIn.X;
        patientPoint[1] = pIn.Y;
        patientPoint[2] = pIn.Z;

        // output point in FileCoordinates
        final double[] modelPoint = new double[3];

        // First invert
        for (int i = 0; i < 3; i++) {

            if (axisFlip[i] && bFlip) {
                patientPoint[i] = extents[i] - patientPoint[i] - 1;
            }
        }

        // then remap the axes
        for (int i = 0; i < 3; i++) {
            modelPoint[i] = patientPoint[axisOrderInverse[i]];
        }

        // assign the transformed point to pOut
        pOut.X = Math.round(modelPoint[0]);
        pOut.Y = Math.round(modelPoint[1]);
        pOut.Z = Math.round(modelPoint[2]);
    }

    /**
     * Translates the input point into FileCoordinates, based on the input image, kImage.
     * 
     * @param kInput the input point in ScannerCoordinates
     * @param kOutput the transformed point in FileCoordinates
     * @param kImage the image for which the point is being transformed.
     */
    public static final void scannerToFile(final Vector3f kInput, final Vector3f kOutput, final ModelImage kImage) {
        // The input point kInput represents the current position in coronal, sagittal, axial order (L/R, A/P, I/S axis
        // space)

        // subtract the scanner origin:
        final Vector3f kOriginLPS = MipavCoordinateSystems.originLPS(kImage);
        scannerToFile(kInput, kOutput, kOriginLPS, kImage);
    }

    /**
     * Translates the input point into FileCoordinates, based on the input image, kImage.
     * 
     * @param kInput the input point in ScannerCoordinates
     * @param kOutput the transformed point in FileCoordinates
     * @param kImage the image for which the point is being transformed.
     */
    public static final void scannerToFile(final Vector3f kInput, final Vector3f kOutput, final Vector3f kOriginLPS, final ModelImage kImage) {
        // The input point kInput represents the current position in coronal, sagittal, axial order (L/R, A/P, I/S axis
        // space)

        final Vector3f kTemp = new Vector3f();
        kTemp.Sub(kInput, kOriginLPS);

        if ( (kImage.getMatrixHolder().containsType(TransMatrix.TRANSFORM_SCANNER_ANATOMICAL))
                || (kImage.getFileInfo()[0].getFileFormat() == FileUtility.DICOM)) {

            // Invert the dicomMatrix Transform
            final TransMatrix dicomMatrix = new TransMatrix( (kImage).getMatrix());
            dicomMatrix.Inverse();
            
            // convert the point from DICOM space
            dicomMatrix.transformAsPoint3Df(new Vector3f(kTemp.X, kTemp.Y, kTemp.Z), kTemp);

            // divide the resolutions from result:
            final float[] afResolutions = kImage.getResolutions(0);
            kOutput.X = kTemp.X / afResolutions[0];
            kOutput.Y = kTemp.Y / afResolutions[1];
            kOutput.Z = kTemp.Z / afResolutions[2];
        } else {
            final int[] axisOrder = new int[] {0, 1, 2};
            final boolean[] axisFlip = new boolean[] {false, false, false};
            MipavCoordinateSystems.toLPS(kImage, axisOrder, axisFlip);

            final float[] afAxialRes = kImage.getResolutions(0);

            final float[] tmpPoint = new float[3];
            tmpPoint[0] = kTemp.X / afAxialRes[axisOrder[0]];
            tmpPoint[1] = kTemp.Y / afAxialRes[axisOrder[1]];
            tmpPoint[2] = kTemp.Z / afAxialRes[axisOrder[2]];

            MipavCoordinateSystems.fromLPS(kImage, axisOrder, axisFlip);
            final float[] filePoint = new float[3];
            for (int i = 0; i < 3; i++) {
                filePoint[i] = tmpPoint[axisOrder[i]];
            }

            final int[] extents = kImage.getExtents();
            // Then invert the point, using the appropriate extents
            for (int i = 0; i < 3; i++) {
                if (axisFlip[i]) {
                    filePoint[i] = extents[i] - filePoint[i] - 1;
                }
            }
            kOutput.X = filePoint[0];
            kOutput.Y = filePoint[1];
            kOutput.Z = filePoint[2];
        }
    }

    
    
    /**
     * 
     * @param pIn the FileCoordinates Point
     * @param image the ModelImage for which the point is being transformed.
     * @param orientation the desired PatientCoordinates orientation.
     * @param bFlip when true use axisFlip to flip the PatientCoordinates axis
     */
    public static final float[][] getPatientTextureCoordinates(final Vector3f pIn, final ModelStorageBase image,
            final int orientation, final boolean bFlip) {

        // axisOrder represents the mapping of FileCoordinates volume axes to PatientCoordinates axes
        final int[] axisOrder = MipavCoordinateSystems.getAxisOrder(image, orientation);

        // axisFlip represents whether to invert the axes after they are reordered
        final boolean[] axisFlip = MipavCoordinateSystems.getAxisFlip(image, orientation);

        // extents gets the image extents re-mapped to PatientCoordinates
        final int[] extents = image.getExtents(orientation);

        final Vector3f pOut = new Vector3f();
        MipavCoordinateSystems.fileToPatient(pIn, pOut, image, orientation, bFlip);
        if (axisFlip[2]) {
            pOut.Z = (extents[2] - 1) - pOut.Z;
        }

        final float[][] patientTC = new float[4][3];

        final Vector3f pIn0 = new Vector3f(0, 0, 0);
        final Vector3f pOut0 = new Vector3f();
        MipavCoordinateSystems.fileToPatient(pIn0, pOut0, image, orientation, bFlip);
        final Vector3f pIn1 = new Vector3f(image.getExtents()[0] - 1, image.getExtents()[1] - 1,
                image.getExtents()[2] - 1);
        final Vector3f pOut1 = new Vector3f();
        MipavCoordinateSystems.fileToPatient(pIn1, pOut1, image, orientation, bFlip);

        patientTC[0][axisOrder[0]] = pOut0.X / extents[0];
        patientTC[0][axisOrder[1]] = pOut0.Y / extents[1];
        patientTC[0][axisOrder[2]] = pOut.Z / extents[2];

        patientTC[1][axisOrder[0]] = pOut1.X / extents[0];
        patientTC[1][axisOrder[1]] = pOut0.Y / extents[1];
        patientTC[1][axisOrder[2]] = pOut.Z / extents[2];

        patientTC[2][axisOrder[0]] = pOut0.X / extents[0];
        patientTC[2][axisOrder[1]] = pOut1.Y / extents[1];
        patientTC[2][axisOrder[2]] = pOut.Z / extents[2];

        patientTC[3][axisOrder[0]] = pOut1.X / extents[0];
        patientTC[3][axisOrder[1]] = pOut1.Y / extents[1];
        patientTC[3][axisOrder[2]] = pOut.Z / extents[2];

        return patientTC;
    }

    /**
     * FileToModel transform. Transforms points in FileCoordinates into ModelCoordinates. ModelCoordinates map the
     * Axial, Coronal, and Sagittal values from the FileCoordinates point into the x, y, and z positions of the output
     * point.
     * 
     * @param image the ModelImage for which the transform is done.
     */
    public static final int fileToModel(final int iIndex, final ModelStorageBase image) {

        // get the axial, coronal and sagittal positions for this image
        final int[] axialOrder = MipavCoordinateSystems.getAxisOrder(image, FileInfoBase.AXIAL);

        final int[] filePoint = {0, 1, 2};

        // re-map the FileCoordinates point into x=I/S, y=A/P and z=L/R
        if (iIndex == 0) {
            return filePoint[axialOrder[2]];
        }
        if (iIndex == 1) {
            return filePoint[axialOrder[1]];
        }
        return filePoint[axialOrder[0]];
    }

    /**
     * FileToModel transform. Transforms points in FileCoordinates into ModelCoordinates. ModelCoordinates map the
     * Axial, Coronal, and Sagittal values from the FileCoordinates point into the x, y, and z positions of the output
     * point.
     * 
     * @param image the ModelImage for which the transform is done.
     */
    public static final int fileToPatient(final int iIndex, final ModelStorageBase image, final int orientation) {

        // get the axial, coronal and sagittal positions for this image
        final int[] axialOrder = MipavCoordinateSystems.getAxisOrder(image, orientation);

        final int[] filePoint = {0, 1, 2};

        // re-map the FileCoordinates point into x=I/S, y=A/P and z=L/R
        if (iIndex == 0) {
            return filePoint[axialOrder[2]];
        }
        if (iIndex == 1) {
            return filePoint[axialOrder[1]];
        }
        return filePoint[axialOrder[0]];
    }

    /**
     * Returns the axisOrder and axisFlip mapping of the input image to LPS coordinates.
     * 
     * @param kImage input image.
     * @param axisOrder output mapping axis re-order.
     * @param axisFlip output mapping axis flip.
     * @return true if the image is not already LPS i.e the image would change to convert to LPS.
     */
    public static final boolean toLPS(final ModelImage kImage, final int[] axisOrder, final boolean[] axisFlip) {
        final int[] axisLPS = new int[] {FileInfoBase.ORI_R2L_TYPE, FileInfoBase.ORI_A2P_TYPE,
                FileInfoBase.ORI_I2S_TYPE};
        return MipavCoordinateSystems.matchOrientation(axisLPS, kImage.getAxisOrientation(), axisOrder, axisFlip);
    }

    /**
     * Returns the axisOrder and axisFlip mapping of the input image from LPS coordinates.
     * 
     * @param kImage input image.
     * @param axisOrder output mapping axis re-order.
     * @param axisFlip output mapping axis flip.
     * @return true if the image is not already LPS i.e the image would change to convert to LPS.
     */
    public static final boolean fromLPS(final ModelImage kImage, final int[] axisOrder, final boolean[] axisFlip) {
        final int[] axisLPS = new int[] {FileInfoBase.ORI_R2L_TYPE, FileInfoBase.ORI_A2P_TYPE,
                FileInfoBase.ORI_I2S_TYPE};
        return MipavCoordinateSystems.matchOrientation(kImage.getAxisOrientation(), axisLPS, axisOrder, axisFlip);
    }

    /**
     * Returns the axisOrder and axisFlip arrays for mapping axisB image orientation onto axisA image orientation. The
     * returned arrays describe how to reorient axisB so that it is in the target orientation axisA.
     * 
     * @param axisA target image orientation (L2R, R2L, A2P, P2A, I2S, S2I, etc.)
     * @param axisB image orientation to re-map so that it matches axisA
     * @param axisOrder re-mapping of the axes.
     * @param axisFlip invert flags for the new axes.
     * @return false when the orientations already match, true when the axisOrder and axisFlip arrays are set.
     */
    public static final boolean matchOrientation(final int[] axisA, final int[] axisB, final int[] axisOrder,
            final boolean[] axisFlip) {
        boolean bMatches = true;
        for (int i = 0; i < 3; i++) {
            if (axisA[i] != axisB[i]) {
                bMatches = false;
                break;
            }
        }
        if (bMatches) {
            return false;
        }
        if (axisA[0] == FileInfoBase.ORI_UNKNOWN_TYPE || axisA[1] == FileInfoBase.ORI_UNKNOWN_TYPE
                || axisA[2] == FileInfoBase.ORI_UNKNOWN_TYPE) {
            return false;
        }
        if (axisB[0] == FileInfoBase.ORI_UNKNOWN_TYPE || axisB[1] == FileInfoBase.ORI_UNKNOWN_TYPE
                || axisB[2] == FileInfoBase.ORI_UNKNOWN_TYPE) {
            axisOrder[0] = 0;
            axisOrder[1] = 1;
            axisOrder[2] = 2;
            axisFlip[0] = false;
            axisFlip[1] = false;
            axisFlip[2] = false;
            return true;
        }

        for (int i = 0; i < 3; i++) {
            if ( (axisA[i] == FileInfoBase.ORI_L2R_TYPE) || (axisA[i] == FileInfoBase.ORI_R2L_TYPE)) {
                for (int j = 0; j < 3; j++) {
                    if ( (axisB[j] == FileInfoBase.ORI_L2R_TYPE) || (axisB[j] == FileInfoBase.ORI_R2L_TYPE)) {
                        axisOrder[i] = j;
                        break;
                    }
                }
            } else if ( (axisA[i] == FileInfoBase.ORI_P2A_TYPE) || (axisA[i] == FileInfoBase.ORI_A2P_TYPE)) {
                for (int j = 0; j < 3; j++) {
                    if ( (axisB[j] == FileInfoBase.ORI_P2A_TYPE) || (axisB[j] == FileInfoBase.ORI_A2P_TYPE)) {
                        axisOrder[i] = j;
                        break;
                    }
                }
            } else if ( (axisA[i] == FileInfoBase.ORI_I2S_TYPE) || (axisA[i] == FileInfoBase.ORI_S2I_TYPE)) {
                for (int j = 0; j < 3; j++) {
                    if ( (axisB[j] == FileInfoBase.ORI_I2S_TYPE) || (axisB[j] == FileInfoBase.ORI_S2I_TYPE)) {
                        axisOrder[i] = j;
                        break;
                    }
                }
            }
        }
        for (int i = 0; i < 3; i++) {
            if (axisA[i] != axisB[axisOrder[i]]) {
                axisFlip[i] = true;
            } else {
                axisFlip[i] = false;
            }
        }
        return true;
    }

    /**
     * Returns the image origin in LPS space. Determines the mapping of the current image space to LPS (x = R2L, y =
     * A2P, z = I2S) space. Returns the origin in the new coordinate space.
     * 
     * @return image origin in LPS coordinate space.
     */
    public static final Vector3f originLPS(final ModelImage kImage) {
        final int[] axisOrient = kImage.getAxisOrientation();
        final int[] axisOrder = new int[] {0, 1, 2};
        final boolean[] axisFlip = new boolean[] {false, false, false};
        MipavCoordinateSystems.toLPS(kImage, axisOrder, axisFlip);

        final float[] afUpperLeft = kImage.getOrigin();
        
        if ( kImage.getExtents().length < 3 && (!kImage.isDicomImage()))
        {
            return new Vector3f( afUpperLeft[0], afUpperLeft[1], afUpperLeft[2] );
        }
        final float[] afLowerRight = new float[3];

        final int[] extents = kImage.getExtents();
        final float[] afRes = kImage.getResolutions(0);
        for (int i = 0; i < 2; i++) {
            if ( (axisOrient[i] == 1) || (axisOrient[i] == 4) || (axisOrient[i] == 5)) {
                afLowerRight[i] = afUpperLeft[i] + ((extents[i] - 1) * afRes[i]);
            } else {
                afLowerRight[i] = afUpperLeft[i] - ((extents[i] - 1) * afRes[i]);
            }
        }
        if ((kImage.getExtents().length < 3) && (kImage.isDicomImage())) {
            afLowerRight[2] = afUpperLeft[2];
        }
        else {
            if ( (axisOrient[2] == 1) || (axisOrient[2] == 3) || (axisOrient[2] == 5)) {
                afLowerRight[2] = afUpperLeft[2] + ((extents[2] - 1) * afRes[2]);
            } else {
                afLowerRight[2] = afUpperLeft[2] - ((extents[2] - 1) * afRes[2]);
            }
        }

        
        final Vector3f kOutput = new Vector3f();
        kOutput.X = afUpperLeft[axisOrder[0]];
        kOutput.Y = afUpperLeft[axisOrder[1]];
        kOutput.Z = afUpperLeft[axisOrder[2]];
        
        if ( (kImage.getMatrixHolder().containsType(TransMatrix.TRANSFORM_SCANNER_ANATOMICAL))
                || (kImage.getFileInfo()[0].getFileFormat() == FileUtility.DICOM)) {
        }
        else {
	        kOutput.X = axisFlip[0] ? afLowerRight[axisOrder[0]] : afUpperLeft[axisOrder[0]];
	        kOutput.Y = axisFlip[1] ? afLowerRight[axisOrder[1]] : afUpperLeft[axisOrder[1]];
	        kOutput.Z = axisFlip[2] ? afLowerRight[axisOrder[2]] : afUpperLeft[axisOrder[2]];
        }
        return kOutput;
    }
    
    /**
	 * Convert the Cartesian coordinate into the Polar coordinate, point based. 
	 * 
	 * @param in     image pixel coordinate, in.x = x, in.y = y.
	 * @param out    polar coordinate  out.x = r, out.y = theta. 
	 * @param image  source image. 
	 */
	public static final void CartesianToPolar2D(Vector2f in, Vector2f out, ModelImage image) {
		// get the Pixel coordinate center
		Vector3f center = image.getImageCenter();

		float centerX = center.X;
		float centerY = center.Y;

		float xx = in.X;
		float yy = in.Y;
		
		// Convert the pixel coordinate to Cartesian coordinate. 
		float x = xx - centerX;
		float y = centerY - yy;
		
		// calculate the Polar coordinate  
		float r = getRadius(x, y);
		float angle = getAngleInDegree(x, y);
		
		out.X = r;
		out.Y = angle;
	}
	
	/**
	 * Convert the Cartesian coordinate into the Polar coordinate.
	 * 
	 * @param image       source model image.
	 * @param imageR      image R buffer to save the r value of Polar coordinate.
	 * @param imageTheta  image Theta buffer to save the theta value of the Polar coordinate. 
	 */
	public static final void CartesianToPolar2D(ModelImage image,
			ModelSimpleImage imageR, ModelSimpleImage imageTheta) {

		// get the Pixel coordinate center
		Vector3f center = image.getImageCenter();

		float centerX = center.X;
		float centerY = center.Y;

		int xDim = image.getExtents()[0];
		int yDim = image.getExtents()[1];

		for (int yy = 0; yy < yDim; yy++) {
			for (int xx = 0; xx < xDim; xx++) {
				// Convert the pixel coordinate to Cartesian coordinate. 
				float x = xx - centerX;
				float y = centerY - yy;

				// calculate the Polar coordinate  
				float r = getRadius(x, y);
				float angle = getAngleInDegree(x, y);

				// save the R, angle values in the R and Theta images. 
				imageR.setValue(xx, yy, r);
				imageTheta.setValue(xx, yy, angle);

			}
		}
	}

	/**
	 * Convert the Polar coordinate to Cartesian coordinate, point based. 
	 * @param in     polar coordinate,  in.x = r, in.y = theta.
	 * @param out    image pixel coordinate, out.x = x, out.y = y;
	 * @param image  source image. 
	 */
	public static void PolarToCartesian2D(Vector2f in, Vector2f out, ModelImage image) {
		// get the center of Cartesian space
		Vector3f center = image.getImageCenter();

		float centerX = center.X;
		float centerY = center.Y;
		
		float r = in.X;
		float angle = (float)((in.Y / (float) 360) * Math.PI * 2);

		// -- Need convert (x,y) into pixel coordinates
		int x = (int)(getCartesianX(r, angle) + centerX);
		int y = (int)(centerY - getCartesianY(r, angle));

		out.X = x;
		out.Y = y;
	}
	
    /**
     * Whether this routine is necessary, the question will to be decided in the future.
     * This approach just converts x, y Cartesian from polar coordinate perspective. 
     * Actually, the imageR and imageTheta should be the input images, and the source image should hold
     * the Cartesian coordinate results.   We need to decide this later.   
	 * Reference: imageJ Polar coordinate transformer. 
	 * 
     * @param image       source image
     * @param imageR      polar coordinate R image. 
     * @param imageTheta  polar coordinate Theta image. 
     */
	public static void PolarToCartisian2D(ModelImage image, ModelSimpleImage imageR, ModelSimpleImage imageTheta) {

		// get the center of Cartesian space
		Vector3f center = image.getImageCenter();

		float centerX = center.X;
		float centerY = center.Y;

		int xDim = image.getExtents()[0];
		int yDim = image.getExtents()[1];

		// longest radius from the center of image to the 4 corners.
		double radius_longest;

		// Set up the Polar Grid:
		// Use y values for angles
		// -- Need 360 degrees (0 to 359...)

		// Line width will be:
		// -- equal to radius -- Need to find the greatest radius
		// -- (4 possibilities: from center to each corner)
		// -- Top-Left Corner (0,0):
		double radius = Math.sqrt((centerX - 0) * (centerX - 0) + (centerY - 0)
				* (centerY - 0));
		// -- Top-Right Corner (xDim, 0):
		double radiusTemp = Math.sqrt((xDim - centerX) * (xDim - centerX)
				+ (centerY - 0) * (centerY - 0));
		if (radiusTemp > radius)
			radius = radiusTemp;
		// -- Bottom-Left Corner (0, yDim):
		radiusTemp = Math.sqrt((centerX - 0) * (centerX - 0) + (yDim - centerY)
				* (yDim - centerY));
		if (radiusTemp > radius)
			radius = radiusTemp;
		// -- Bottom-Right Corner (xDim , yDim):
		radiusTemp = Math.sqrt((centerX - xDim) * (centerX - xDim)
				+ (centerY - yDim) * (centerY - yDim));
		if (radiusTemp > radius)
			radius = radiusTemp;
		int radiusInt = (int) radius;
		radius_longest = radiusInt;

		// Fill the Polar Grid
		for (int yy = 0; yy < 360; yy++) {
			for (int xx = 0; xx < radius_longest; xx++) {

				// For each polar pixel, need to convert it to Cartesian
				// coordinates
				float r = xx;
				float angle = (float)((yy / (float) 360) * Math.PI * 2);

				// -- Need convert (x,y) into pixel coordinates
				int x = (int)(getCartesianX(r, angle) + centerX);
				int y = (int)(centerY - getCartesianY(r, angle));

				imageR.setValue(x, y, r);
				imageTheta.setValue(x, y, angle);
				
			}
		}

	}

	/**
	 * Givem Polar coordinate system r and theta, calculate the Cartesian coordinate X. 
	 * @param r      polar coordinate r value.
	 * @param angle  poloar coordinate theta value.
	 * @return  Cartesian coordinate X value.  
	 */
	private static float getCartesianX(float r, float angle) {
		return (float)(r * Math.cos(angle));
	}

	/**
	 * Givem Polar coordinate system r and theta, calculate the Cartesian coordinate Y. 
	 * @param r      polar coordinate r value.
	 * @param angle  poloar coordinate theta value.
	 * @return  Cartesian coordinate Y value.  
	 */
	
	private static float getCartesianY(float r, float angle) {
		float y = (float)(r * Math.sin(angle));
		boolean clockWise = true;
		return clockWise ? -y : y;
	}

	/**
	 * Get the radius for the polar coordinate from the given Cartesian coordinate
	 * @param x  Cartesian x coordinate
	 * @param y  Cartesian y coordinate
	 * @return  polar coordinate radius 
	 */
	private static float getRadius(float x, float y) {
		return (float) Math.sqrt(x * x + y * y);
	}

	/**
	 * Get the polar theta angle in Degree [0, 360].
	 * Reference: imageJ polar coordinate transformer. 
	 * 
	 * @param x  Cartesian x coordinate
	 * @param y  Cartesian y coordinate
	 * @return  the polar theta angle in degree. 
	 */
	private static float getAngleInDegree(float x, float y) {

		// Returns an angle in the range [0, 360]
		boolean clockWise = true;
		float angle = (float) Math.toDegrees(Math.atan2(y, x));
		
		if (angle < 0) {
			angle += 360;
		}
		return clockWise ? 360f - angle : angle;
	
	}

	/***
	 * Get the polar theta value in radians [0 , 2*PI].  
	 * Reference:  http://en.wikipedia.org/wiki/Polar_coordinate_system.
	 * 
	 * @param x  Cartesian x coordinate
	 * @param y  Cartesian y coordinate
	 * @return  the polar theta angle in radians. 
	 */
	private static float getAngleInRadians(float x, float y) {
		if (x > 0 && y >= 0) {
			return (float) Math.atan2(y, x);
		} else if (x > 0 && y < 0) {
			return (float) (Math.atan2(y, x) + 2 * Math.PI);
		} else if (x < 0) {
			return (float) (Math.atan2(y, x) + Math.PI);
		} else if (x == 0 && y > 0) {
			return (float) (0.5f * Math.PI);
		} else if (x == 0 && y < 0) {
			return (float) (1.5f * Math.PI);
		} else { // x = 0 && y = 0
			return 0;
		}
	}
    

}
