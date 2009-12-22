package gov.nih.mipav;


import gov.nih.mipav.model.file.*;
import gov.nih.mipav.model.structures.*;

import java.util.Iterator;

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
    public static final void fileToModel(Vector3f pIn, Vector3f pOut, ModelStorageBase image) {

        // get the axial, coronal and sagittal positions for this image
        int[] axialOrder = MipavCoordinateSystems.getAxisOrder(image, FileInfoBase.AXIAL);

        float[] filePoint = {pIn.X, pIn.Y, pIn.Z};

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
    public static final void fileToPatient(Vector3f pIn, Vector3f pOut, ModelStorageBase image, int orientation) {
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
    public static final void fileToPatient(Vector3f pIn, Vector3f pOut, ModelStorageBase image, int orientation,
            boolean bFlip) {

        // axisOrder represents the mapping of FileCoordinates volume axes to PatientCoordinates axes
        int[] axisOrder = MipavCoordinateSystems.getAxisOrder(image, orientation);

        // axisFlip represents whether to invert the axes after they are reordered
        boolean[] axisFlip = MipavCoordinateSystems.getAxisFlip(image, orientation);

        MipavCoordinateSystems.fileToPatient(pIn, pOut, image, orientation, bFlip, axisOrder, axisFlip);
    }
    
    public static final void fileToPatient(Vector3f pIn, Vector3f pOut, ModelStorageBase image, int orientation,
            boolean bFlip, int[] axisOrder, boolean[] axisFlip) {
        // extents gets the image extents re-mapped to PatientCoordinates
        int[] extents = image.getExtents(orientation);

        // The modelPoint
        float[] modelPoint = new float[3];
        modelPoint[0] = pIn.X;
        modelPoint[1] = pIn.Y;
        modelPoint[2] = pIn.Z;

        // transformed patientPoint
        float[] patientPoint = new float[3];

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
    public static final void fileToScanner(Vector3f kInput, Vector3f kOutput, ModelImage kImage) {
        
        Vector3f kOriginLPS = MipavCoordinateSystems.originLPS( kImage );

        if ( (kImage.getMatrixHolder().containsType(TransMatrix.TRANSFORM_SCANNER_ANATOMICAL))
                || (kImage.getFileInfo()[0].getFileFormat() == FileUtility.DICOM)) {

            float[] afResolutions = kImage.getResolutions(0);

			TransMatrix dicomMatrix = ((ModelImage) kImage).getMatrix(); // Gets composite matrix
			
			
            // Finally convert the point to axial millimeter DICOM space.
            dicomMatrix.transformAsPoint3Df(new Vector3f(kInput.X * afResolutions[0], kInput.Y * afResolutions[1],
                    kInput.Z * afResolutions[2]), kOutput);
        } else {

            int[] axisOrder = new int[]{0,1,2};
            boolean[] axisFlip = new boolean[]{false,false,false};
            toLPS( kImage, axisOrder, axisFlip );
            
            
            float[] afRes = kImage.getResolutions(0);
            float[] filePoint = new float[3];
            filePoint[0] = kInput.X;
            filePoint[1] = kInput.Y;
            filePoint[2] = kInput.Z;

            float[] scannerPoint = new float[3];
            // First reorder the point indices based on the axisOrder re-mapping
            for (int i = 0; i < 3; i++) {
                scannerPoint[i] = filePoint[axisOrder[i]];
            }

            int[] extents = kImage.getExtents();
            // Then invert the point, using the appropriate extents
            for (int i = 0; i < 3; i++) {
                if (axisFlip[i]) {
                    scannerPoint[i] = extents[axisOrder[i]] - scannerPoint[i] - 1;
                }
            }

            kOutput.X = scannerPoint[0]*afRes[axisOrder[0]];
            kOutput.Y = scannerPoint[1]*afRes[axisOrder[1]];
            kOutput.Z = scannerPoint[2]*afRes[axisOrder[2]];
        }

        // Returned point represents the current position in coronal, sagittal, axial order (L/R, A/P, I/S axis space)
        kOutput.Add( kOriginLPS );

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
    public static final boolean[] getAxisFlip(ModelStorageBase image, int iOrientation) {
        return getAxisFlip(image, iOrientation, false);
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
    public static final boolean[] getAxisFlip(ModelStorageBase image, int iOrientation, boolean bDICOM) {
        boolean[] abAxisFlip = {false, false, false};
        int[] aiAxisOrientation = getAxisOrientation(image);

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
    public static final int[] getAxisOrder(ModelStorageBase image, int iOrientation) {
        int[] aiAxisOrder = new int[3];
        int[] aiAxisOrientation = getAxisOrientation(image);

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
    public static final void getAxisOrderInverse(int[] axisOrder, int[] axisOrderInverse) {

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
    public static final int[] getAxisOrientation(ModelStorageBase image) {
        int imageOrientation = image.getImageOrientation();

        if (imageOrientation == FileInfoBase.UNKNOWN_ORIENT) {
            return null;
        }

        int[] aiAxisOrientation = image.getAxisOrientation();

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
     * Returns the direction vectors for displaying the surface so that it aligns properly with the
     * input ModelImage. The direction vectors are transformed into ModelCoordinates.
     * 
     * @param kImage the ModelImage for which the direction vectors are returned.
     * 
     * @return the direction vectors for the object.
     */
    public static final int[] getModelDirections(ModelStorageBase kImage) {
        boolean[] axialFlip = getAxisFlip(kImage, FileInfoBase.AXIAL);

        int[] aiModelFlip = {1, 1, 1};

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
    public static final void modelToFile(Vector3f pIn, Vector3f pOut, ModelStorageBase image) {

        // get the axial, coronal and sagittal positions for this image
        int[] axialOrder = MipavCoordinateSystems.getAxisOrder(image, FileInfoBase.AXIAL);

        float[] modelPoint = new float[3];
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
    public static final void patientToFile(Vector3f pIn, Vector3f pOut, ModelStorageBase image, int orientation) {
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
    public static final void patientToFile(Vector3f pIn, Vector3f pOut, ModelStorageBase image, int orientation,
            boolean bFlip) {

        // axisOrder represents the mapping of FileCoordinates volume axes to PatientCoordinates space axes
        int[] axisOrder = MipavCoordinateSystems.getAxisOrder(image, orientation);

        // axisFlip represents whether to invert the axes after they are reordered
        boolean[] axisFlip = MipavCoordinateSystems.getAxisFlip(image, orientation);

        // extents gets the image extents re-mapped to patient coordinates
        int[] extents = image.getExtents(orientation);

        // axisOrderInverse is the inverse axis re-mapping from PatientCoordinates to FileCoordinates
        int[] axisOrderInverse = new int[3];

        // get the inverse mapping
        MipavCoordinateSystems.getAxisOrderInverse(axisOrder, axisOrderInverse);

        // input point in PatientCoordinates
        float[] patientPoint = new float[3];
        patientPoint[0] = pIn.X;
        patientPoint[1] = pIn.Y;
        patientPoint[2] = pIn.Z;

        // output point in FileCoordinates
        float[] modelPoint = new float[3];

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
     * Translates the input point into FileCoordinates, based on the input image, kImage.
     * 
     * @param kInput the input point in ScannerCoordinates
     * @param kOutput the transformed point in FileCoordinates
     * @param kImage the image for which the point is being transformed.
     */
    public static final void scannerToFile(Vector3f kInput, Vector3f kOutput, ModelImage kImage) {
        // The input point kInput represents the current position in coronal, sagittal, axial order (L/R, A/P, I/S axis
        // space)

        
        // subtract the scanner origin:
        Vector3f kOriginLPS = MipavCoordinateSystems.originLPS(kImage);

        Vector3f kTemp = new Vector3f();
        kTemp.Sub( kInput, kOriginLPS );

        if ( (kImage.getMatrixHolder().containsType(TransMatrix.TRANSFORM_SCANNER_ANATOMICAL))
                || (kImage.getFileInfo()[0].getFileFormat() == FileUtility.DICOM)) {

            // Invert the dicomMatrix Transform
            TransMatrix dicomMatrix = new TransMatrix( ((ModelImage) kImage).getMatrix());
            dicomMatrix.Inverse();

            // convert the point from DICOM space
            dicomMatrix.transformAsPoint3Df(new Vector3f(kTemp.X, kTemp.Y, kTemp.Z), kTemp);

            // divide the resolutions from result:
            float[] afResolutions = kImage.getResolutions(0);
            kOutput.X = kTemp.X / afResolutions[0];
            kOutput.Y = kTemp.Y / afResolutions[1];
            kOutput.Z = kTemp.Z / afResolutions[2];
        } else {
            int[] axisOrder = new int[]{0,1,2};
            boolean[] axisFlip = new boolean[]{false,false,false};
            toLPS( kImage, axisOrder, axisFlip );
            
            float[] afAxialRes = kImage.getResolutions(0);

            float[] tmpPoint = new float[3];
            tmpPoint[0] = kTemp.X / afAxialRes[axisOrder[0]];
            tmpPoint[1] = kTemp.Y / afAxialRes[axisOrder[1]];
            tmpPoint[2] = kTemp.Z / afAxialRes[axisOrder[2]];

            fromLPS( kImage, axisOrder, axisFlip );
            float[] filePoint = new float[3];
            for (int i = 0; i < 3; i++) {
                filePoint[i] = tmpPoint[axisOrder[i]];
            }

            int[] extents = kImage.getExtents();
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
    public static final float[][] getPatientTextureCoordinates(Vector3f pIn, ModelStorageBase image, int orientation,
            boolean bFlip) {

        // axisOrder represents the mapping of FileCoordinates volume axes to PatientCoordinates axes
        int[] axisOrder = MipavCoordinateSystems.getAxisOrder(image, orientation);

        // axisFlip represents whether to invert the axes after they are reordered
        boolean[] axisFlip = MipavCoordinateSystems.getAxisFlip(image, orientation);

        // extents gets the image extents re-mapped to PatientCoordinates
        int[] extents = image.getExtents(orientation);

        Vector3f pOut = new Vector3f();
        MipavCoordinateSystems.fileToPatient(pIn, pOut, image, orientation, bFlip);
        if (axisFlip[2]) {
            pOut.Z = (extents[2] - 1) - pOut.Z;
        }

        float[][] patientTC = new float[4][3];

        Vector3f pIn0 = new Vector3f(0, 0, 0);
        Vector3f pOut0 = new Vector3f();
        MipavCoordinateSystems.fileToPatient(pIn0, pOut0, image, orientation, bFlip);
        Vector3f pIn1 = new Vector3f(image.getExtents()[0] - 1, image.getExtents()[1] - 1, image.getExtents()[2] - 1);
        Vector3f pOut1 = new Vector3f();
        MipavCoordinateSystems.fileToPatient(pIn1, pOut1, image, orientation, bFlip);

        patientTC[0][axisOrder[0]] = pOut0.X / (float) extents[0];
        patientTC[0][axisOrder[1]] = pOut0.Y / (float) extents[1];
        patientTC[0][axisOrder[2]] = pOut.Z / (float) extents[2];

        patientTC[1][axisOrder[0]] = pOut1.X / (float) extents[0];
        patientTC[1][axisOrder[1]] = pOut0.Y / (float) extents[1];
        patientTC[1][axisOrder[2]] = pOut.Z / (float) extents[2];

        patientTC[2][axisOrder[0]] = pOut0.X / (float) extents[0];
        patientTC[2][axisOrder[1]] = pOut1.Y / (float) extents[1];
        patientTC[2][axisOrder[2]] = pOut.Z / (float) extents[2];

        patientTC[3][axisOrder[0]] = pOut1.X / (float) extents[0];
        patientTC[3][axisOrder[1]] = pOut1.Y / (float) extents[1];
        patientTC[3][axisOrder[2]] = pOut.Z / (float) extents[2];

        return patientTC;
    }

    /**
     * FileToModel transform. Transforms points in FileCoordinates into ModelCoordinates. ModelCoordinates map the
     * Axial, Coronal, and Sagittal values from the FileCoordinates point into the x, y, and z positions of the output
     * point.
     * 
     * @param image the ModelImage for which the transform is done.
     */
    public static final int fileToModel(int iIndex, ModelStorageBase image) {

        // get the axial, coronal and sagittal positions for this image
        int[] axialOrder = MipavCoordinateSystems.getAxisOrder(image, FileInfoBase.AXIAL);

        int[] filePoint = {0, 1, 2};

        // re-map the FileCoordinates point into x=I/S, y=A/P and z=L/R
        if (iIndex == 0)
            return filePoint[axialOrder[2]];
        if (iIndex == 1)
            return filePoint[axialOrder[1]];
        return filePoint[axialOrder[0]];
    }

    /**
     * FileToModel transform. Transforms points in FileCoordinates into ModelCoordinates. ModelCoordinates map the
     * Axial, Coronal, and Sagittal values from the FileCoordinates point into the x, y, and z positions of the output
     * point.
     * 
     * @param image the ModelImage for which the transform is done.
     */
    public static final int fileToPatient(int iIndex, ModelStorageBase image, int orientation) {

        // get the axial, coronal and sagittal positions for this image
        int[] axialOrder = MipavCoordinateSystems.getAxisOrder(image, orientation);

        int[] filePoint = {0, 1, 2};

        // re-map the FileCoordinates point into x=I/S, y=A/P and z=L/R
        if (iIndex == 0)
            return filePoint[axialOrder[2]];
        if (iIndex == 1)
            return filePoint[axialOrder[1]];
        return filePoint[axialOrder[0]];
    }

    public static final int axisOrderToImageOrientation( int[] axisOrder )
    {

        int orientation = FileInfoBase.UNKNOWN_ORIENT;
        if ( axisOrder.length < 3 )
        {
            return orientation;
        }
        if (axisOrder[2] == 0 )
        {
            orientation = FileInfoBase.SAGITTAL;
        }        
        else if (axisOrder[2] == 1 )
        {
            orientation = FileInfoBase.CORONAL;
        }        
        else if (axisOrder[2] == 2 )
        {
            orientation = FileInfoBase.AXIAL;
        }            
        return orientation;
    }
    
    /**
     * Returns the axisOrder and axisFlip mapping of the input image to LPS coordinates.
     * @param kImage input image.
     * @param axisOrder output mapping axis re-order.
     * @param axisFlip output mapping axis flip.
     * @return true if the image is not already LPS i.e the image would change to convert to LPS.
     */
    public static final boolean toLPS( ModelImage kImage, int[] axisOrder, boolean[] axisFlip )
    {
        int[] axisLPS = new int[]{ FileInfoBase.ORI_R2L_TYPE, FileInfoBase.ORI_A2P_TYPE, FileInfoBase.ORI_I2S_TYPE };
        return matchOrientation( axisLPS, kImage.getAxisOrientation(), axisOrder, axisFlip );
    }    
    
    /**
     * Returns the axisOrder and axisFlip mapping of the input image from LPS coordinates.
     * @param kImage input image.
     * @param axisOrder output mapping axis re-order.
     * @param axisFlip output mapping axis flip.
     * @return true if the image is not already LPS i.e the image would change to convert to LPS.
     */
    public static final boolean fromLPS( ModelImage kImage, int[] axisOrder, boolean[] axisFlip )
    {
        int[] axisLPS = new int[]{ FileInfoBase.ORI_R2L_TYPE, FileInfoBase.ORI_A2P_TYPE, FileInfoBase.ORI_I2S_TYPE };
        return matchOrientation( kImage.getAxisOrientation(), axisLPS, axisOrder, axisFlip );
    }
    
    public static final boolean matchOrientation( int[] axisA, int[] axisB, int[] axisOrder, boolean[] axisFlip )
    {
        boolean bMatches = true;
        for ( int i = 0; i < 3; i++ )
        {
            if (axisA[i] != axisB[i])
            {
                bMatches = false;
                break;
            }
        }
        if ( bMatches )
        {
            return false;
        }
        for ( int i = 0; i < 3; i++ )
        {
            if ( (axisA[i] == FileInfoBase.ORI_L2R_TYPE) || (axisA[i] == FileInfoBase.ORI_R2L_TYPE) )
            {
                for ( int j = 0; j < 3; j++ )
                {
                    if ( (axisB[j] == FileInfoBase.ORI_L2R_TYPE) || (axisB[j] == FileInfoBase.ORI_R2L_TYPE) )
                    {
                        axisOrder[i] = j;
                        break;
                    }
                }
            }
            else if ( (axisA[i] == FileInfoBase.ORI_P2A_TYPE) || (axisA[i] == FileInfoBase.ORI_A2P_TYPE) )
            {
                for ( int j = 0; j < 3; j++ )
                {
                    if ( (axisB[j] == FileInfoBase.ORI_P2A_TYPE) || (axisB[j] == FileInfoBase.ORI_A2P_TYPE) )
                    {
                        axisOrder[i] = j;
                        break;
                    }
                }
            }
            else if ( (axisA[i] == FileInfoBase.ORI_I2S_TYPE) || (axisA[i] == FileInfoBase.ORI_S2I_TYPE) )
            {
                for ( int j = 0; j < 3; j++ )
                {
                    if ( (axisB[j] == FileInfoBase.ORI_I2S_TYPE) || (axisB[j] == FileInfoBase.ORI_S2I_TYPE) )
                    {
                        axisOrder[i] = j;
                        break;
                    }
                }
            }
        }
        for ( int i = 0; i < 3; i++ )
        {
            if ( axisA[i] != axisB[axisOrder[i]] )
            {
                axisFlip[i] = true;
            }
            else
            {
                axisFlip[i] = false;
            }
        }
        return true;
    }
    
    /**
     * Translates the input point into ScannerCoordinates, based on the input image, kImage.
     * 
     * @param kInput the input point in FileCoordinates
     * @param kOutput the transformed point in ScannerCoordinates
     * @param kImage the image for which the point is being transformed.
     */
    public static final Vector3f originLPS(ModelImage kImage) {
        int[] axisOrient = kImage.getAxisOrientation();
        int[] axisOrder = new int[]{0,1,2};
        boolean[] axisFlip = new boolean[]{false,false,false};
        toLPS( kImage, axisOrder, axisFlip );

        float[] afUpperLeft = kImage.getOrigin();
        float[] afLowerRight = new float[3];

        int[] extents = kImage.getExtents();
        float[] afRes = kImage.getResolutions(0);
        for ( int i = 0; i < 3; i++ )
        {
            if ((axisOrient[i] == 1) || (axisOrient[i] == 4) || (axisOrient[i] == 5))
            {
                afLowerRight[i] = afUpperLeft[i] + (extents[0] * afRes[i]);
            }
            else
            {
                afLowerRight[i] = afUpperLeft[i] - (extents[0] * afRes[i]);
            }
        }
        System.err.println("");
        System.err.println("");
        System.err.println( "Upper Left " + afUpperLeft[0] + " " + afUpperLeft[1] + " " + afUpperLeft[2] );
        System.err.println( "Lower Right " + afLowerRight[0] + " " + afLowerRight[1] + " " + afLowerRight[2] );

        Vector3f kOutput = new Vector3f();
        kOutput.X = axisFlip[0] ? afLowerRight[ axisOrder[0] ] : afUpperLeft[ axisOrder[0] ];
        kOutput.Y = axisFlip[1] ? afLowerRight[ axisOrder[1] ] : afUpperLeft[ axisOrder[1] ];
        kOutput.Z = axisFlip[2] ? afLowerRight[ axisOrder[2] ] : afUpperLeft[ axisOrder[2] ];
        System.err.println( "--> LPS Origin " + kOutput.ToString() );
        return kOutput;
    }


}
