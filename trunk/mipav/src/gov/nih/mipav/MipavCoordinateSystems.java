package gov.nih.mipav;


import gov.nih.mipav.model.file.*;
import gov.nih.mipav.model.structures.*;


/* MipavCoordinateSystems upgrade: TODO: TEMPORARY CLASS: */
/**
 * MipavCoordinateSystems class. This is a temporary class to define and
 * isolate the different Coordinate Systems in Mipav. The class functionality
 * will be move to a permanent class and/or this class will be fleshed out
 * further.
 *
 * Mipav Coordinate Systems:
 *
 * 1). File -- sample coordinates, represented by the FileInfoBase.ORI_L2R_TYPE, etc.
 *
 * 2). Model -- ModelStorageBase coordinates -- how the data is stored in memory
 *
 * 3). Patient -- data rearranged for either the AXIAL, SAGITTAL, or CORONAL views of the data.
 *
 * 4). Screen -- Mouse coordinates, so far.
 *
 * This class provides static functions for transforming points from one
 * Coordinate Space into another.
 *
 */
public class MipavCoordinateSystems
{

    public static final void FileToModel(Point3Df pIn, Point3Df pOut,
                                         ModelStorageBase image)
    {
        int[] axialOrder = MipavCoordinateSystems.getAxisOrder( image, FileInfoBase.AXIAL );
        int[] coronalOrder = MipavCoordinateSystems.getAxisOrder( image, FileInfoBase.CORONAL );
        int[] sagittalOrder = MipavCoordinateSystems.getAxisOrder( image, FileInfoBase.SAGITTAL );

        float[] filePoint = { pIn.x, pIn.y, pIn.z };

        pOut.x = filePoint[ axialOrder[2] ];
        pOut.y = filePoint[ coronalOrder[2] ];
        pOut.z = filePoint[ sagittalOrder[2] ];
    }

    public static final void ModelToFile(Point3Df pIn, Point3Df pOut,
                                         ModelStorageBase image )
    {
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

    /** Model to Patient transform. Transforms points that are in model space
     * into patient space, based on the desired orientation -- either AXIAL,
     * SAGITTAL, or CORONAL: */
    public static final void FileToPatient( Point3Df pIn, Point3Df pOut,
                                            ModelStorageBase image, int orientation )
    {
        /* axisOrder represents the mapping of model space volume axes to patient space axes: */
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
         * from Model to Patient space: */
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
        pOut.x = patientPoint[0];
        pOut.y = patientPoint[1];
        pOut.z = patientPoint[2];
    }

    /** Patient to Model transform. Transforms points that are in patient space
     * (either AXIAL, SAGITTAL, or CORONAL) into Model space: */
    public static final void PatientToFile(Point3Df pIn, Point3Df pOut,
                                           ModelStorageBase image, int orientation)
    {
        /* axisOrder represents the mapping of model space volume axes to patient space axes: */
        int[] axisOrder = MipavCoordinateSystems.getAxisOrder( image, orientation );

        /* axisFlip represents whether to invert the axes after they are reordered: */
        boolean[] axisFlip = MipavCoordinateSystems.getAxisFlip( image, orientation );

        /* extents gets the image extents re-mapped to patient coordinates */
        int[] extents = image.getExtents( orientation );

        /* axisOrderInverse is the inverse axis re-mapping from Patient
         * Coordinates to Model Coordinates: */
        int[] axisOrderInverse = new int[3];
        /* get the inverse mapping: */
        getAxisOrderInverse( axisOrder, axisOrderInverse );

        /* input point in patient coordinates: */
        float[] patientPoint = new float[3];
        patientPoint[0] = pIn.x;
        patientPoint[1] = pIn.y;
        patientPoint[2] = pIn.z;

        /* output point in model coordinates: */
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
        pOut.x = modelPoint[0];
        pOut.y = modelPoint[1];
        pOut.z = modelPoint[2];

    }

    /* Inverts the input axisOrder so that points can be mapped back from
     * Patient Coordinates to Model Coordinates. The Inverse map is based only
     * on the input axisOrder map. */
    public static final void getAxisOrderInverse( int[] axisOrder, int[] axisOrderInverse )
    {
        for ( int i = 0; i < 3; i++ )
        {
            axisOrderInverse[ axisOrder[i] ] = i;
        }
    }


    /* Gets the axisOrientation variables stored in the image.FileInfoBase
     * object. If the orientation is undefined, the axes are assigned values
     * based on the image orientation: */
    public static final int[] getAxisOrientation( ModelStorageBase image  )
    {
        int[] aiAxisOrientation = image.getAxisOrientation();

        int imageOrientation = image.getImageOrientation();

        if ( aiAxisOrientation[0] == FileInfoBase.ORI_UNKNOWN_TYPE )
        {
            if ( (imageOrientation == FileInfoBase.AXIAL) || (imageOrientation == FileInfoBase.UNKNOWN_ORIENT) )
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


    /* Get the booean axisFlip array that describes how Model Coordinate axes
     * are inverted in Patient Coordinates. The values are determined by the
     * FileInfoBase.ORI_L2R_TYPE, etc. values stored in the input image and by the
     * desired patient viewing orientation.
     *
     * @param image, the image storing the data
     * @param iOrientation the desired Patient Coordinates view of the data (AXIAL, SAGITTAL, CORONAL)
     * @return axisFlip, how to invert the Model axes for the given Patient Coordinate view
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

        if ( image.getRadiologicalView() &&
             (iOrientation != FileInfoBase.SAGITTAL) &&
             (iOrientation != FileInfoBase.UNKNOWN_ORIENT) )
        {
            abAxisFlip[0] = !abAxisFlip[0];
        }

        return abAxisFlip;

    }

    /* Get the axisOrder array that describes how Model Coordinate axes are
     * remapped for the Patient Coordinates. The axisOrder values are
     * determined by the FileInfoBase.ORI_L2R_TYPE, etc. values stored in the
     * input image and by the desired patient viewing orientation.
     *
     * @param image, the image storing the data
     * @param iOrientation the desired Patient Coordinates view of the data (AXIAL, SAGITTAL, CORONAL)
     * @return axisOrder, how to re-map the Model axes for the given Patient Coordinate view
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

    /** Translates the input point into model scanner coordinates, based on the input image, kImage.
     * @param kInput, the input point in FileCoordinates
     * @param kOutput, the transformed point in ScannerCoordinates
     * @param kImage, the image for which the point is being transformed.
     */
    public static final void getScannerCoordinates( Point3Df kInput, Point3Df kOutput, ModelStorageBase kImage )
    {
        Point3Df kAxial = new Point3Df();
        MipavCoordinateSystems.FileToPatient( kInput, kAxial, kImage, FileInfoBase.AXIAL );
        float[] afAxialRes = kImage.getResolutions( 0, FileInfoBase.AXIAL );
        float[] afAxialOrigin = kImage.getOrigin( 0, FileInfoBase.AXIAL );

        Point3Df kCoronal = new Point3Df();
        MipavCoordinateSystems.FileToPatient( kInput, kCoronal, kImage, FileInfoBase.CORONAL );
        float[] afCoronalRes = kImage.getResolutions( 0, FileInfoBase.CORONAL );
        float[] afCoronalOrigin = kImage.getOrigin( 0, FileInfoBase.CORONAL );

        Point3Df kSagittal = new Point3Df();
        MipavCoordinateSystems.FileToPatient( kInput, kSagittal, kImage, FileInfoBase.SAGITTAL );
        float[] afSagittalRes = kImage.getResolutions( 0, FileInfoBase.SAGITTAL );
        float[] afSagittalOrigin = kImage.getOrigin( 0, FileInfoBase.SAGITTAL );
        
        kOutput.x = kCoronal.z * afCoronalRes[2] + afCoronalOrigin[2];
        kOutput.y = kSagittal.z * afSagittalRes[2] + afSagittalOrigin[2];
        kOutput.z = kAxial.z * afAxialRes[2] + afAxialOrigin[2];
    }

}
