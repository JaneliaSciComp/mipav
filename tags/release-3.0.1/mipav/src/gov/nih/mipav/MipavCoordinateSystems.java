package gov.nih.mipav;


import gov.nih.mipav.model.file.*;
import gov.nih.mipav.model.structures.*;


/**
 * MipavCoordinateSystems class. This class provides the interface layer
 * between objects and classes that display information about the ModelImage
 * class and the ModelImage class itself.
 *
 * FileCoordinates -- how the ModelImage data is stored on disk and read into
 * the ModelStorageBase 1D array.
 *
 * ModelCoordinates -- mapping of the Axial, Coronal, and Sagittal
 * FileCoordinates values into x, y, z (0, 1, 2) positions for array indexing
 * (based on the FileInfoBase.AXIAL, CORONAL, and SAGITTIAL constant defines).
 *
 * PatientCoordinates, the coordinate axes for displaying the Axial, Coronal,
 * and Sagittal views of the ModelImage data. The mapping of Human-Anatomical
 * axes (Left-Right, Anterior-Posterior, Superior-Inferior) onto
 * PatientCoordinates: Axial = { R2L, A2P, I2S }; Coronal = { R2L, S2I, A2P };
 * Sagittal = { A2P, S2I, R2L }
 *
 * This class provides static functions for transforming points from one
 * Coordinate Space into another.
 *
 */
public class MipavCoordinateSystems
{

    /**
     * FileToModel transform. Transforms points in FileCoordinates into
     * ModelCoordinates. ModelCoordinates map the Axial, Coronal, and Sagittal
     * values from the FileCoordinates point into the x, y, and z positions of
     * the output point.
     * @param pIn, the point in FileCoordinates to be transformed
     * @param pOut, return: the transformed point in ModelCoordinates
     * @param image, the ModelImage for which the transform is done.
     */
    public static final void FileToModel(Point3Df pIn, Point3Df pOut,
                                         ModelStorageBase image)
    {
        /* get the axial, coronal and sagittal positions for this image: */        
        int[] axialOrder = MipavCoordinateSystems.getAxisOrder( image, FileInfoBase.AXIAL );
        int[] coronalOrder = MipavCoordinateSystems.getAxisOrder( image, FileInfoBase.CORONAL );
        int[] sagittalOrder = MipavCoordinateSystems.getAxisOrder( image, FileInfoBase.SAGITTAL );

        float[] filePoint = { pIn.x, pIn.y, pIn.z };

        /* re-map the FileCoordinates point into x=axial, y=coronal and
         * z=sagittal: */        
        pOut.x = filePoint[ axialOrder[2] ];
        pOut.y = filePoint[ coronalOrder[2] ];
        pOut.z = filePoint[ sagittalOrder[2] ];
    }

    /**
     * ModelToFile transform. Transforms points in ModelCoordinates (x =
     * Axial, y = Coronal, z = Sagittal) into FileCoordinates.
     * @param pIn, the point in ModelCoordinates to be transformed
     * @param pOut, return: the transformed point in FileCoordinates
     * @param image, the ModelImage for which the transform is done.
     */
    public static final void ModelToFile(Point3Df pIn, Point3Df pOut,
                                         ModelStorageBase image )
    {
        /* get the axial, coronal and sagittal positions for this image: */        
        int[] axialOrder = MipavCoordinateSystems.getAxisOrder( image, FileInfoBase.AXIAL );
        int[] coronalOrder = MipavCoordinateSystems.getAxisOrder( image, FileInfoBase.CORONAL );
        int[] sagittalOrder = MipavCoordinateSystems.getAxisOrder( image, FileInfoBase.SAGITTAL );

        float[] modelPoint = new float[3];
        modelPoint[ axialOrder[2] ] = pIn.x;
        modelPoint[ coronalOrder[2] ] = pIn.y;
        modelPoint[ sagittalOrder[2] ] = pIn.z;

        pOut.x = modelPoint[ 0 ];
        pOut.y = modelPoint[ 1 ];
        pOut.z = modelPoint[ 2 ];
    }

    /** FileToPatient transform. Transforms points that are in FileCoordinates
     * into PatientCoordinates space, based on the ModelImage and the desired
     * oriented view -- either FileInfoBase.AXIAL, FileInfoBase.CORONAL,
     * FileInfoBase.SAGITTAL, or FileInfoBase.UNKNOWN_ORIENT:
     * @param pIn, the FileCoordinates Point
     * @param pOut, the Point in PatientCoordinates
     * @param image, the ModelImage for which the point is being transformed.
     * @param orientation, the desired PatientCoordinates orientation.
     */
    public static final void FileToPatient( Point3Df pIn, Point3Df pOut,
                                            ModelStorageBase image, int orientation )
    {
        /* axisOrder represents the mapping of FileCoordinates volume axes to
         * PatientCoordinates axes: */
        int[] axisOrder = MipavCoordinateSystems.getAxisOrder( image, orientation );

        /* axisFlip represents whether to invert the axes after they are
         * reordered: */
        boolean[] axisFlip = MipavCoordinateSystems.getAxisFlip( image, orientation );

        /* extents gets the image extents re-mapped to PatientCoordinates */
        int[] extents = image.getExtents( orientation );

        /* The modelPoint: */
        float[] modelPoint = new float[3];
        modelPoint[0] = pIn.x;
        modelPoint[1] = pIn.y;
        modelPoint[2] = pIn.z;

        /* transformed patientPoint: */
        float[] patientPoint = new float[3];

        /* First reorder the point indices based on the axisOrder re-mapping
         * from FileCoordinates to PatientCoordinates space: */
        for ( int i = 0; i < 3; i++ )
        {
            patientPoint[i] = modelPoint[ axisOrder[i] ];
        }

        /* Then invert the point, using the appropriate extents: */
        for ( int i = 0; i < 3; i++ )
        {
            if ( axisFlip[ i] )
            {
                patientPoint[i] = extents[ i ] - patientPoint[ i ] - 1;
            }
        }
        /* assign the transformed point to pOut: */
        pOut.x = Math.round( patientPoint[0] );
        pOut.y = Math.round( patientPoint[1] );
        pOut.z = Math.round( patientPoint[2] );
    }

    /** FileToSlice transform. Transforms a point in FileCoordinates into
     * PatientCoordinates space, based on the ModelImage and the desired view
     * orientation -- either FileInfoBase.AXIAL, FileInfoBase.CORONAL,
     * FileInfoBase.SAGITTAL, or FileInfoBase.UNKNOWN_ORIENT. Returns only the
     * z-value of the transformed point.
     * @param pIn, the FileCoordinates Point
     * @param image, the ModelImage for which the point is being transformed.
     * @param orientation, the desired PatientCoordinates view orientation.
     * @param bFlip, when true use axisFlip to flip the PatientCoordinates axis
     * @return the z-value of the pIn input point in PatientCoordinates
     */
    public static final int FileToSlice( Point3Df pIn, ModelStorageBase image,
                                         int orientation, boolean bFlip )
    {
        /* axisOrder represents the mapping of FileCoordinates volume axes to
         * PatientCoordinates space axes: */
        int[] axisOrder = MipavCoordinateSystems.getAxisOrder( image, orientation );

        /* axisFlip represents whether to invert the axes after they are reordered: */
        boolean[] axisFlip = MipavCoordinateSystems.getAxisFlip( image, orientation );

        /* extents gets the image extents re-mapped to patient coordinates */
        int[] extents = image.getExtents( orientation );

        /* The modelPoint: */
        float[] modelPoint = new float[3];
        modelPoint[0] = pIn.x;
        modelPoint[1] = pIn.y;
        modelPoint[2] = pIn.z;

        /* transformed patientPoint: */
        float[] patientPoint = new float[3];

        /* First reorder the point indices based on the axisOrder re-mapping
         * from File to Patient space: */
        for ( int i = 0; i < 3; i++ )
        {
            patientPoint[i] = modelPoint[ axisOrder[i] ];
        }

        /* Then invert the point, using the appropriate extents: */
        for ( int i = 0; i < 3; i++ )
        {
            if ( axisFlip[ i] && bFlip )
            {
                patientPoint[i] = extents[ i ] - patientPoint[ i ] - 1;
            }
        }
        /* return the z-value of the transformed point: */
        return Math.round( patientPoint[2] );
    }



    /** PatientToFile transform. Transforms points that are in
     * PatientCoordinates into FileCoordinates space, based on the ModelImage
     * and the given oriented view -- either FileInfoBase.AXIAL,
     * FileInfoBase.CORONAL, FileInfoBase.SAGITTAL, or
     * FileInfoBase.UNKNOWN_ORIENT:
     * @param pIn, the PatientCoordinates Point
     * @param pOut, the Point in FileCoordinates
     * @param image, the ModelImage for which the point is being transformed.
     * @param orientation, the given PatientCoordinates orientation.
     */
    public static final void PatientToFile(Point3Df pIn, Point3Df pOut,
                                           ModelStorageBase image, int orientation)
    {
        /* axisOrder represents the mapping of FileCoordinates volume axes to
         * PatientCoordinates space axes: */
        int[] axisOrder = MipavCoordinateSystems.getAxisOrder( image, orientation );

        /* axisFlip represents whether to invert the axes after they are reordered: */
        boolean[] axisFlip = MipavCoordinateSystems.getAxisFlip( image, orientation );

        /* extents gets the image extents re-mapped to patient coordinates */
        int[] extents = image.getExtents( orientation );

        /* axisOrderInverse is the inverse axis re-mapping from
         * PatientCoordinates to FileCoordinates: */
        int[] axisOrderInverse = new int[3];
        /* get the inverse mapping: */
        getAxisOrderInverse( axisOrder, axisOrderInverse );

        /* input point in PatientCoordinates: */
        float[] patientPoint = new float[3];
        patientPoint[0] = pIn.x;
        patientPoint[1] = pIn.y;
        patientPoint[2] = pIn.z;

        /* output point in FileCoordinates: */
        float[] modelPoint = new float[3];

        /* First invert: */
        for ( int i = 0; i < 3; i++ )
        {
            if ( axisFlip[ i ] )
            {
                patientPoint[i] = extents[ i ] - patientPoint[ i ] - 1;
            }
        }
        /* then remap the axes: */
        for ( int i = 0; i < 3; i++ )
        {
            modelPoint[i] = patientPoint[ axisOrderInverse[i] ];
        }

        /* assign the transformed point to pOut: */
        pOut.x = Math.round( modelPoint[0] );
        pOut.y = Math.round( modelPoint[1] );
        pOut.z = Math.round( modelPoint[2] );

    }

    /* Inverts the input axisOrder so that points can be mapped back from
     * PatientCoordinates to FileCoordinates. The Inverse map is based only on
     * the input axisOrder map.
     * @param axisOrder, the axisOrder that is being inverted
     * @param axisOrderInverse, return: the inverse of axisOrder parameter so
     * that axisOrderInverse undoes the axisOrder re-mapping of coordinate
     * axes.
     */
    public static final void getAxisOrderInverse( int[] axisOrder, int[] axisOrderInverse )
    {
        for ( int i = 0; i < 3; i++ )
        {
            axisOrderInverse[ axisOrder[i] ] = i;
        }
    }


    /* Gets the axisOrientation variables stored in the image.FileInfoBase
     * object for the input ModelImage, image. If the orientation is
     * undefined, the axes are assigned values based on the image
     * orientation:
     * @param image, the ModelImage for which the axisOrientation is returned
     * @return, the axisOrientation of the ModelImage, null if the ModelImage
     * is of FileInfoBase.UNKNOWN_ORIENT
     */
    public static final int[] getAxisOrientation( ModelStorageBase image  )
    {
        int imageOrientation = image.getImageOrientation();
        if (imageOrientation == FileInfoBase.UNKNOWN_ORIENT )
        {
            return null;
        }

        int[] aiAxisOrientation = image.getAxisOrientation();
        if ( aiAxisOrientation[0] == FileInfoBase.ORI_UNKNOWN_TYPE )
        {
            if ( imageOrientation == FileInfoBase.AXIAL )
            {
                aiAxisOrientation[0] = FileInfoBase.ORI_R2L_TYPE;
                aiAxisOrientation[1] = FileInfoBase.ORI_A2P_TYPE;
                aiAxisOrientation[2] = FileInfoBase.ORI_I2S_TYPE;
            }
            else if ( imageOrientation == FileInfoBase.CORONAL )
            {
                aiAxisOrientation[0] = FileInfoBase.ORI_R2L_TYPE;
                aiAxisOrientation[1] = FileInfoBase.ORI_S2I_TYPE;
                aiAxisOrientation[2] = FileInfoBase.ORI_A2P_TYPE;
            }
            else if ( imageOrientation == FileInfoBase.SAGITTAL )
            {
                aiAxisOrientation[0] = FileInfoBase.ORI_A2P_TYPE;
                aiAxisOrientation[1] = FileInfoBase.ORI_S2I_TYPE;
                aiAxisOrientation[2] = FileInfoBase.ORI_R2L_TYPE;
            }
        }
        return aiAxisOrientation;
    }


    /* Get the booean axisFlip array that describes how FileCoordinate axes
     * are inverted in PatientCoordinates. The values are determined by the
     * FileInfoBase.ORI_L2R_TYPE, etc. values stored in the input
     * image.FileInfoBase.axisOrientation and by the desired patient viewing
     * orientation.
     *
     * @param image, the image storing the data
     * @param iOrientation the desired PatientCoordinates view of the data (AXIAL, SAGITTAL, CORONAL)
     * @return axisFlip, how to invert the FileCoordinates axes for the given PatientCoordinate view
     */
    public static final boolean[] getAxisFlip( ModelStorageBase image, int iOrientation )
    {
        boolean[] abAxisFlip = { false, false, false };
        int[] aiAxisOrientation = getAxisOrientation( image );

        if ( (aiAxisOrientation != null) &&
             (iOrientation != FileInfoBase.UNKNOWN_ORIENT ) )
        {
            for (int i = 0; i < 3; i++) {

                if (aiAxisOrientation[ i ] == FileInfoBase.ORI_L2R_TYPE)
                {
                    if ((iOrientation == FileInfoBase.AXIAL) ||
                        (iOrientation == FileInfoBase.CORONAL))
                    {
                        abAxisFlip[0] = true;
                    }
                    else
                    {
                        abAxisFlip[2] = true;
                    }
                }
                else if (aiAxisOrientation[ i ] == FileInfoBase.ORI_P2A_TYPE)
                {
                    if (iOrientation == FileInfoBase.AXIAL)
                    {
                        abAxisFlip[1] = true;
                    }
                    else if (iOrientation == FileInfoBase.SAGITTAL)
                    {
                        abAxisFlip[0] = true;
                    }
                    else
                    {
                        abAxisFlip[2] = true;
                    }
                }
                else if (aiAxisOrientation[ i ] == FileInfoBase.ORI_S2I_TYPE)
                {
                    if (iOrientation == FileInfoBase.AXIAL)
                    {
                        abAxisFlip[2] = true;
                    }
                }
                else if (aiAxisOrientation[ i ] == FileInfoBase.ORI_I2S_TYPE)
                {
                    if (iOrientation != FileInfoBase.AXIAL)
                    {
                        abAxisFlip[1] = true;
                    }
                }
            }
        }

        if ( !image.getRadiologicalView() &&
             (iOrientation != FileInfoBase.SAGITTAL) &&
             (iOrientation != FileInfoBase.UNKNOWN_ORIENT) )
        {
            abAxisFlip[0] = !abAxisFlip[0];
        }

        return abAxisFlip;

    }

    /* Get the axisOrder array that describes how FileCoordinate axes are
     * remapped for the PatientCoordinates. The axisOrder values are
     * determined by the FileInfoBase.ORI_L2R_TYPE, etc. values stored in the
     * input image and by the desired patient viewing orientation.
     *
     * @param image, the image storing the data
     * @param iOrientation the desired PatientCoordinates view of the data (AXIAL, SAGITTAL, CORONAL)
     * @return axisOrder, how to re-map the FileCoordinates axes for the given PatientCoordinate view.
     */
    public static final int[] getAxisOrder( ModelStorageBase image, int iOrientation )
    {
        int[] aiAxisOrder = new int[3];
        int[] aiAxisOrientation = getAxisOrientation( image );

        if ( (aiAxisOrientation != null)  &&
             (iOrientation != FileInfoBase.UNKNOWN_ORIENT ) )
        {
            for ( int i = 0; i < 3; i++ )
            {
                if ( (aiAxisOrientation[ i ] == FileInfoBase.ORI_R2L_TYPE) ||
                     (aiAxisOrientation[ i ] == FileInfoBase.ORI_L2R_TYPE)    )
                {
                    if ( ( iOrientation == FileInfoBase.AXIAL )   ||
                         ( iOrientation == FileInfoBase.CORONAL )   )
                    {
                        aiAxisOrder[0] =  i;
                    }
                    else
                    {
                        aiAxisOrder[2] =  i;
                    }
                }
                else if ( (aiAxisOrientation[ i ]==FileInfoBase.ORI_A2P_TYPE) ||
                          (aiAxisOrientation[ i ]==FileInfoBase.ORI_P2A_TYPE)  )
                {
                    if ( iOrientation == FileInfoBase.AXIAL )
                    {
                        aiAxisOrder[1] =  i;
                    }
                    else if ( iOrientation == FileInfoBase.SAGITTAL )
                    {
                        aiAxisOrder[0] =  i;
                    }
                    else
                    {
                        aiAxisOrder[2] =  i;
                    }
                }
                else if ( (aiAxisOrientation[ i ]==FileInfoBase.ORI_S2I_TYPE) ||
                          (aiAxisOrientation[ i ]==FileInfoBase.ORI_I2S_TYPE) )
                {
                    if ( iOrientation == FileInfoBase.AXIAL )
                    {
                        aiAxisOrder[2] =  i;
                    }
                    else
                    {
                        aiAxisOrder[1] =  i;
                    }
                }
            }
        }
        else if ( (iOrientation == FileInfoBase.SAGITTAL) )
        {
            aiAxisOrder[0] = 2;
            aiAxisOrder[1] = 1;
            aiAxisOrder[2] = 0;
        }
        else if ( (iOrientation == FileInfoBase.CORONAL) )
        {
            aiAxisOrder[0] = 0;
            aiAxisOrder[1] = 2;
            aiAxisOrder[2] = 1;
        }
        else
        {
            aiAxisOrder[0] = 0;
            aiAxisOrder[1] = 1;
            aiAxisOrder[2] = 2;
        }

        return aiAxisOrder;
    }

    /**
     * Returns the direction vectors for displaying the ModelTriangleMesh
     * surface so that it aligns properly with the input ModelImage. The
     * direction vectors are transformed into ModelCoordinates.
     * @param kImage, the ModelImage for which the direction vectors are returned.
     * @return the direction vectors for the ModelTriangleMesh object.
     */
    public static final int[] getModelDirections(  ModelStorageBase kImage )
    {
        boolean[] axialFlip = getAxisFlip( kImage, FileInfoBase.AXIAL );
        boolean[] coronalFlip = getAxisFlip( kImage, FileInfoBase.CORONAL );
        boolean[] sagittalFlip = getAxisFlip( kImage, FileInfoBase.SAGITTAL );

        int[] aiModelFlip = { 1, 1, 1 };
        if ( coronalFlip[2] )
        {
            aiModelFlip[0] = -1;
        }
        if ( sagittalFlip[2] )
        {
            aiModelFlip[1] = -1;
        }
        if ( axialFlip[2] )
        {
            aiModelFlip[2] = -1;
        }
        return aiModelFlip;
    }

    /** Translates the input point into ScannerCoordinates, based on
     * the input image, kImage.
     * @param kInput, the input point in FileCoordinates
     * @param kOutput, the transformed point in ScannerCoordinates
     * @param kImage, the image for which the point is being transformed.
     */
    public static final void getScannerCoordinates( Point3Df kInput, Point3Df kOutput, ModelStorageBase kImage )
    {
        float[] afAxialOrigin = kImage.getOrigin( 0, FileInfoBase.AXIAL );
        float[] afCoronalOrigin = kImage.getOrigin( 0, FileInfoBase.CORONAL );
        float[] afSagittalOrigin = kImage.getOrigin( 0, FileInfoBase.SAGITTAL );

        if (( kImage.getFileInfo()[0].getTransformID() == FileInfoBase.TRANSFORM_SCANNER_ANATOMICAL) ||
            (kImage.getFileInfo()[0].getFileFormat() == FileUtility.DICOM))
        {
            float[] afResolutions = kImage.getResolutions( 0 );

            TransMatrix dicomMatrix = (TransMatrix) (((ModelImage)kImage).getMatrix().clone());
            // Finally convert the point to axial millimeter DICOM space.
            dicomMatrix.transform( new Point3Df( kInput.x * afResolutions[0],
                                                 kInput.y * afResolutions[1],
                                                 kInput.z * afResolutions[2] ), kOutput);
            /* After transform kOutput is in sagittal, coronal, axial slice
             * order, swap sagittal and coronal:*/
            float tmp = kOutput.x;
            kOutput.x = kOutput.y;
            kOutput.y = tmp;
        }
        else
        {
            float[] afAxialRes = kImage.getResolutions( 0, FileInfoBase.AXIAL );
            float[] afCoronalRes = kImage.getResolutions( 0, FileInfoBase.CORONAL );
            float[] afSagittalRes = kImage.getResolutions( 0, FileInfoBase.SAGITTAL );

            kOutput.x = afCoronalRes[2] * MipavCoordinateSystems.FileToSlice( kInput, kImage, FileInfoBase.CORONAL, false );
            kOutput.y = afSagittalRes[2] * MipavCoordinateSystems.FileToSlice( kInput, kImage, FileInfoBase.SAGITTAL, false );
            kOutput.z = afAxialRes[2] * MipavCoordinateSystems.FileToSlice( kInput, kImage, FileInfoBase.AXIAL, false );
        }
        /* Returned point represents the current position in coronal,
         * sagittal, axial order (A/P, L/R, I/S axis space): */
        kOutput.x += afCoronalOrigin[2];
        kOutput.y += afSagittalOrigin[2];
        kOutput.z += afAxialOrigin[2];
    }

}
