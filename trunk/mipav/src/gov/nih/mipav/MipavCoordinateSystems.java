package gov.nih.mipav;


import gov.nih.mipav.model.file.*;
import gov.nih.mipav.model.structures.*;


/**
 * MipavCoordinateSystems class. This class provides the interface layer between objects and classes that display
 * information about the ModelImage class and the ModelImage class itself.
 *
 * <p>FileCoordinates -- how the ModelImage data is stored on disk and read into the ModelStorageBase 1D array.</p>
 *
 * <p>ModelCoordinates -- mapping of the Axial, Coronal, and Sagittal FileCoordinates values into x, y, z (0, 1, 2)
 * positions for array indexing (based on the FileInfoBase.AXIAL, CORONAL, and SAGITTIAL constant defines).</p>
 *
 * <p>PatientCoordinates, the coordinate axes for displaying the Axial, Coronal, and Sagittal views of the ModelImage
 * data. The mapping of Human-Anatomical axes (Left-Right, Anterior-Posterior, Superior-Inferior) onto
 * PatientCoordinates: Axial = { R2L, A2P, I2S }; Coronal = { R2L, S2I, A2P }; Sagittal = { A2P, S2I, R2L }</p>
 *
 * <p>This class provides static functions for transforming points from one Coordinate Space into another.</p>
 */
public class MipavCoordinateSystems {

    //~ Methods --------------------------------------------------------------------------------------------------------

    /**
     * FileToModel transform. Transforms points in FileCoordinates into ModelCoordinates. ModelCoordinates map the
     * Axial, Coronal, and Sagittal values from the FileCoordinates point into the x, y, and z positions of the output
     * point.
     *
     * @param  pIn    the point in FileCoordinates to be transformed
     * @param  pOut   return: the transformed point in ModelCoordinates
     * @param  image  the ModelImage for which the transform is done.
     */
    public static final void fileToModel(Point3Df pIn, Point3Df pOut, ModelStorageBase image) {

        // get the axial, coronal and sagittal positions for this image
        int[] axialOrder = MipavCoordinateSystems.getAxisOrder(image, FileInfoBase.AXIAL);

        float[] filePoint = { pIn.x, pIn.y, pIn.z };

        // re-map the FileCoordinates point into x=I/S, y=A/P and z=L/R
        pOut.x = filePoint[axialOrder[2]];
        pOut.y = filePoint[axialOrder[1]];
        pOut.z = filePoint[axialOrder[0]];
    }

    /**
     * FileToPatient transform. Transforms points that are in FileCoordinates into PatientCoordinates space, based on
     * the ModelImage and the desired oriented view -- either FileInfoBase.AXIAL, FileInfoBase.CORONAL,
     * FileInfoBase.SAGITTAL, or FileInfoBase.UNKNOWN_ORIENT. Always allows for flipping of the axies using the image
     * orientation information as needed.
     *
     * @param  pIn          the FileCoordinates Point
     * @param  pOut         the Point in PatientCoordinates
     * @param  image        the ModelImage for which the point is being transformed.
     * @param  orientation  the desired PatientCoordinates orientation.
     */
    public static final void fileToPatient(Point3Df pIn, Point3Df pOut, ModelStorageBase image, int orientation) {
        MipavCoordinateSystems.fileToPatient(pIn, pOut, image, orientation, true);
    }

    /**
     * FileToPatient transform. Transforms points that are in FileCoordinates into PatientCoordinates space, based on
     * the ModelImage and the desired oriented view -- either FileInfoBase.AXIAL, FileInfoBase.CORONAL,
     * FileInfoBase.SAGITTAL, or FileInfoBase.UNKNOWN_ORIENT:
     *
     * @param  pIn          the FileCoordinates Point
     * @param  pOut         the Point in PatientCoordinates
     * @param  image        the ModelImage for which the point is being transformed.
     * @param  orientation  the desired PatientCoordinates orientation.
     * @param  bFlip        when true use axisFlip to flip the PatientCoordinates axis
     */
    public static final void fileToPatient(Point3Df pIn, Point3Df pOut, ModelStorageBase image, int orientation,
                                           boolean bFlip) {

        // axisOrder represents the mapping of FileCoordinates volume axes to PatientCoordinates axes
        int[] axisOrder = MipavCoordinateSystems.getAxisOrder(image, orientation);

        // axisFlip represents whether to invert the axes after they are reordered
        boolean[] axisFlip = MipavCoordinateSystems.getAxisFlip(image, orientation);

        // extents gets the image extents re-mapped to PatientCoordinates
        int[] extents = image.getExtents(orientation);

        // The modelPoint
        float[] modelPoint = new float[3];
        modelPoint[0] = pIn.x;
        modelPoint[1] = pIn.y;
        modelPoint[2] = pIn.z;

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
        pOut.x = MipavMath.round(patientPoint[0]);
        pOut.y = MipavMath.round(patientPoint[1]);
        pOut.z = MipavMath.round(patientPoint[2]);
    }

    /**
     * Translates the input point into ScannerCoordinates, based on the input image, kImage.
     *
     * @param  kInput   the input point in FileCoordinates
     * @param  kOutput  the transformed point in ScannerCoordinates
     * @param  kImage   the image for which the point is being transformed.
     */
    public static final void fileToScanner(Point3Df kInput, Point3Df kOutput, ModelImage kImage) {
        float[] afAxialOrigin = kImage.getOrigin(0, FileInfoBase.AXIAL);

        if ((kImage.getMatrixHolder().containsType(TransMatrix.TRANSFORM_SCANNER_ANATOMICAL)) ||
                (kImage.getFileInfo()[0].getFileFormat() == FileUtility.DICOM)) {

            float[] afResolutions = kImage.getResolutions(0);

            TransMatrix dicomMatrix = (TransMatrix) (((ModelImage) kImage).getMatrix().clone());

           // System.err.println("DICOM MATRIX (fileToScanner): " + dicomMatrix);
           // System.err.println("axial origin: " + afAxialOrigin[0] + ", " + afAxialOrigin[1] + ", " + afAxialOrigin[2]);
            
            
            // Finally convert the point to axial millimeter DICOM space.
            dicomMatrix.transform(new Point3Df(kInput.x * afResolutions[0], kInput.y * afResolutions[1],
                                               kInput.z * afResolutions[2]), kOutput);
        } else {
        	//System.err.println("not dicom");
            float[] afAxialRes = kImage.getResolutions(0, FileInfoBase.AXIAL);

            MipavCoordinateSystems.fileToPatient(kInput, kOutput, kImage, FileInfoBase.AXIAL, false);

            kOutput.x *= afAxialRes[0];
            kOutput.y *= afAxialRes[1];
            kOutput.z *= afAxialRes[2];

            boolean[] axisFlip = MipavCoordinateSystems.getAxisFlip(kImage, FileInfoBase.AXIAL, true);

            if (axisFlip[0]) {
                kOutput.x = -kOutput.x;
            }

            if (axisFlip[1]) {
                kOutput.y = -kOutput.y;
            }

            if (axisFlip[2]) {
                kOutput.z = -kOutput.z;
            }
        }

        // Returned point represents the current position in coronal, sagittal, axial order (L/R, A/P, I/S axis space)
        kOutput.x += afAxialOrigin[0];
        kOutput.y += afAxialOrigin[1];
        kOutput.z += afAxialOrigin[2];
        
    }

    /**
     * Get the booean axisFlip array that describes how FileCoordinate axes are inverted in PatientCoordinates. The
     * values are determined by the FileInfoBase.ORI_L2R_TYPE, etc. values stored in the input
     * image.FileInfoBase.axisOrientation and by the desired patient viewing orientation.
     *
     * @param   image         DOCUMENT ME!
     * @param   iOrientation  the desired PatientCoordinates view of the data (AXIAL, SAGITTAL, CORONAL)
     *
     * @return  DOCUMENT ME!
     */
    public static final boolean[] getAxisFlip(ModelStorageBase image, int iOrientation) {
        return getAxisFlip(image, iOrientation, false);
    }

    /**
     * Get the booean axisFlip array that describes how FileCoordinate axes are inverted in PatientCoordinates. The
     * values are determined by the FileInfoBase.ORI_L2R_TYPE, etc. values stored in the input
     * image.FileInfoBase.axisOrientation and by the desired patient viewing orientation.
     *
     * @param   image         DOCUMENT ME!
     * @param   iOrientation  the desired PatientCoordinates view of the data (AXIAL, SAGITTAL, CORONAL)
     * @param   bDICOM        whether we are trying to find the axis flip array for a translation into DICOM spaces
     *
     * @return  DOCUMENT ME!
     */
    public static final boolean[] getAxisFlip(ModelStorageBase image, int iOrientation, boolean bDICOM) {
        boolean[] abAxisFlip = { false, false, false };
        int[] aiAxisOrientation = getAxisOrientation(image);

        if ((aiAxisOrientation != null) && (iOrientation != FileInfoBase.UNKNOWN_ORIENT)) {

            for (int i = 0; i < 3; i++) {

                if (aiAxisOrientation[i] == FileInfoBase.ORI_L2R_TYPE) {

                    if ((iOrientation == FileInfoBase.AXIAL) || (iOrientation == FileInfoBase.CORONAL)) {
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

        if (!image.getRadiologicalView() && (iOrientation != FileInfoBase.SAGITTAL) &&
                (iOrientation != FileInfoBase.UNKNOWN_ORIENT) && !bDICOM) {
            abAxisFlip[0] = !abAxisFlip[0];
        }

        return abAxisFlip;

    }

    /**
     * Get the axisOrder array that describes how FileCoordinate axes are remapped for the PatientCoordinates. The
     * axisOrder values are determined by the FileInfoBase.ORI_L2R_TYPE, etc. values stored in the input image and by
     * the desired patient viewing orientation.
     *
     * @param   image         DOCUMENT ME!
     * @param   iOrientation  the desired PatientCoordinates view of the data (AXIAL, SAGITTAL, CORONAL)
     *
     * @return  DOCUMENT ME!
     */
    public static final int[] getAxisOrder(ModelStorageBase image, int iOrientation) {
        int[] aiAxisOrder = new int[3];
        int[] aiAxisOrientation = getAxisOrientation(image);

        if ((aiAxisOrientation != null) && (iOrientation != FileInfoBase.UNKNOWN_ORIENT)) {

            for (int i = 0; i < 3; i++) {

                if ((aiAxisOrientation[i] == FileInfoBase.ORI_R2L_TYPE) ||
                        (aiAxisOrientation[i] == FileInfoBase.ORI_L2R_TYPE)) {

                    if ((iOrientation == FileInfoBase.AXIAL) || (iOrientation == FileInfoBase.CORONAL)) {
                        aiAxisOrder[0] = i;
                    } else {
                        aiAxisOrder[2] = i;
                    }
                } else if ((aiAxisOrientation[i] == FileInfoBase.ORI_A2P_TYPE) ||
                               (aiAxisOrientation[i] == FileInfoBase.ORI_P2A_TYPE)) {

                    if (iOrientation == FileInfoBase.AXIAL) {
                        aiAxisOrder[1] = i;
                    } else if (iOrientation == FileInfoBase.SAGITTAL) {
                        aiAxisOrder[0] = i;
                    } else {
                        aiAxisOrder[2] = i;
                    }
                } else if ((aiAxisOrientation[i] == FileInfoBase.ORI_S2I_TYPE) ||
                               (aiAxisOrientation[i] == FileInfoBase.ORI_I2S_TYPE)) {

                    if (iOrientation == FileInfoBase.AXIAL) {
                        aiAxisOrder[2] = i;
                    } else {
                        aiAxisOrder[1] = i;
                    }
                }
            }
        } else if ((iOrientation == FileInfoBase.SAGITTAL)) {
            aiAxisOrder[0] = 2;
            aiAxisOrder[1] = 1;
            aiAxisOrder[2] = 0;
        } else if ((iOrientation == FileInfoBase.CORONAL)) {
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
     * @param  axisOrder         DOCUMENT ME!
     * @param  axisOrderInverse  DOCUMENT ME!
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
     * @param   image  DOCUMENT ME!
     *
     * @return  DOCUMENT ME!
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
     * Returns the direction vectors for displaying the ModelTriangleMesh surface so that it aligns properly with the
     * input ModelImage. The direction vectors are transformed into ModelCoordinates.
     *
     * @param   kImage  the ModelImage for which the direction vectors are returned.
     *
     * @return  the direction vectors for the ModelTriangleMesh object.
     */
    public static final int[] getModelDirections(ModelStorageBase kImage) {
        boolean[] axialFlip = getAxisFlip(kImage, FileInfoBase.AXIAL);

        int[] aiModelFlip = { 1, 1, 1 };

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
     * @param  pIn    the point in ModelCoordinates to be transformed
     * @param  pOut   return: the transformed point in FileCoordinates
     * @param  image  the ModelImage for which the transform is done.
     */
    public static final void modelToFile(Point3Df pIn, Point3Df pOut, ModelStorageBase image) {

        // get the axial, coronal and sagittal positions for this image
        int[] axialOrder = MipavCoordinateSystems.getAxisOrder(image, FileInfoBase.AXIAL);

        float[] modelPoint = new float[3];
        modelPoint[axialOrder[2]] = pIn.x;
        modelPoint[axialOrder[1]] = pIn.y;
        modelPoint[axialOrder[0]] = pIn.z;

        pOut.x = modelPoint[0];
        pOut.y = modelPoint[1];
        pOut.z = modelPoint[2];
    }

    /**
     * PatientToFile transform. Transforms points that are in PatientCoordinates into FileCoordinates space, based on
     * the ModelImage and the given oriented view -- either FileInfoBase.AXIAL, FileInfoBase.CORONAL,
     * FileInfoBase.SAGITTAL, or FileInfoBase.UNKNOWN_ORIENT. Always allows for flipping of the axies using the image
     * orientation information as needed.
     *
     * @param  pIn          the FileCoordinates Point
     * @param  pOut         the Point in PatientCoordinates
     * @param  image        the ModelImage for which the point is being transformed.
     * @param  orientation  the desired PatientCoordinates orientation.
     */
    public static final void patientToFile(Point3Df pIn, Point3Df pOut, ModelStorageBase image, int orientation) {
        MipavCoordinateSystems.patientToFile(pIn, pOut, image, orientation, true);
    }

    /**
     * PatientToFile transform. Transforms points that are in PatientCoordinates into FileCoordinates space, based on
     * the ModelImage and the given oriented view -- either FileInfoBase.AXIAL, FileInfoBase.CORONAL,
     * FileInfoBase.SAGITTAL, or FileInfoBase.UNKNOWN_ORIENT:
     *
     * @param  pIn          the PatientCoordinates Point
     * @param  pOut         the Point in FileCoordinates
     * @param  image        the ModelImage for which the point is being transformed.
     * @param  orientation  the given PatientCoordinates orientation.
     * @param  bFlip        when true use axisFlip to flip the PatientCoordinates axis
     */
    public static final void patientToFile(Point3Df pIn, Point3Df pOut, ModelStorageBase image, int orientation,
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
        patientPoint[0] = pIn.x;
        patientPoint[1] = pIn.y;
        patientPoint[2] = pIn.z;

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
        pOut.x = MipavMath.round(modelPoint[0]);
        pOut.y = MipavMath.round(modelPoint[1]);
        pOut.z = MipavMath.round(modelPoint[2]);
    }

    /**
     * Translates the input point into FileCoordinates, based on the input image, kImage.
     *
     * @param  kInput   the input point in ScannerCoordinates
     * @param  kOutput  the transformed point in FileCoordinates
     * @param  kImage   the image for which the point is being transformed.
     */
    public static final void scannerToFile(Point3Df kInput, Point3Df kOutput, ModelImage kImage) {
        // The input point kInput represents the current position in coronal, sagittal, axial order (L/R, A/P, I/S axis
        // space)

        // subtract the scanner origin:
        float[] afAxialOrigin = kImage.getOrigin(0, FileInfoBase.AXIAL);

        Point3Df kTemp = new Point3Df();
        kTemp.x = kInput.x - afAxialOrigin[0];
        kTemp.y = kInput.y - afAxialOrigin[1];
        kTemp.z = kInput.z - afAxialOrigin[2];

        if ((kImage.getMatrixHolder().containsType(TransMatrix.TRANSFORM_SCANNER_ANATOMICAL)) ||
                (kImage.getFileInfo()[0].getFileFormat() == FileUtility.DICOM)) {

            // Invert the dicomMatrix Transform
            TransMatrix dicomMatrix = (TransMatrix) (((ModelImage) kImage).getMatrix().clone());
            dicomMatrix.invert();

            // convert the point from DICOM space
            dicomMatrix.transform(new Point3Df(kTemp.x, kTemp.y, kTemp.z), kTemp);

            // divide the resolutions from result:
            float[] afResolutions = kImage.getResolutions(0);
            kOutput.x = kTemp.x / afResolutions[0];
            kOutput.y = kTemp.y / afResolutions[1];
            kOutput.z = kTemp.z / afResolutions[2];
        } else {
            float[] afAxialRes = kImage.getResolutions(0, FileInfoBase.AXIAL);

            kTemp.x /= afAxialRes[0];
            kTemp.y /= afAxialRes[1];
            kTemp.z /= afAxialRes[2];

            boolean[] axisFlip = MipavCoordinateSystems.getAxisFlip(kImage, FileInfoBase.AXIAL, true);

            if (axisFlip[0]) {
                kOutput.x = -kOutput.x;
            }

            if (axisFlip[1]) {
                kOutput.y = -kOutput.y;
            }

            if (axisFlip[2]) {
                kOutput.z = -kOutput.z;
            }

            MipavCoordinateSystems.patientToFile(kTemp, kOutput, kImage, FileInfoBase.AXIAL, false);
        }
    }
}
