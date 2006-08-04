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
    /** File to Mode transform currently is identity: */
    public static final void FileToModel(Point3Df pIn, Point3Df pOut,
                                            ModelStorageBase image)
    {
        pOut.x = pIn.x;
        pOut.y = pIn.y;
        pOut.z = pIn.z;
    }

    /** File to Mode transform currently is identity: */
    public static final void ModelToFile(Point3Df pIn, Point3Df pOut,
                                            ModelStorageBase image)
    {
        pOut.x = pIn.x;
        pOut.y = pIn.y;
        pOut.z = pIn.z;
    }

    /** Model to Patient transform. Transforms points that are in model space
     * into patient space, based on the desired orientation -- either AXIAL,
     * SAGITTAL, or CORONAL: */
    public static final void ModelToPatient( Point3Df pIn, Point3Df pOut,
                                             ModelStorageBase image, int orientation )
    {
        //System.err.println( "TESTING" );
        test( image, orientation );
        //System.err.println( "DONE" );

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
    public static final void PatientToModel(Point3Df pIn, Point3Df pOut,
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

    /* Testing Model->Patient and Patient->Model transforms for all permutations of axisOrder and axisFlip: */
    private static final void test( ModelStorageBase image, int orientation )
    {
        test( image, orientation, new int[] { 0, 1, 2 } );
        test( image, orientation, new int[] { 0, 2, 1 } );
        test( image, orientation, new int[] { 1, 0, 2 } );
        test( image, orientation, new int[] { 1, 2, 0 } );
        test( image, orientation, new int[] { 2, 0, 1 } );
        test( image, orientation, new int[] { 2, 1, 0 } );
    }

    /* Testing Model->Patient and Patient->Model transforms for all permutations of axisOrder and axisFlip: */
    private static final void test( ModelStorageBase image, int orientation, int[] axisOrder )
    {
        test( image, orientation, axisOrder, new boolean[] { false, false, false } );
        test( image, orientation, axisOrder, new boolean[] { false, false, true } );
        test( image, orientation, axisOrder, new boolean[] { false, true, false } );
        test( image, orientation, axisOrder, new boolean[] { false, true, true } );
        test( image, orientation, axisOrder, new boolean[] { true, false, false } );
        test( image, orientation, axisOrder, new boolean[] { true, false, true } );
        test( image, orientation, axisOrder, new boolean[] { true, true, false } );
        test( image, orientation, axisOrder, new boolean[] { true, true, true } );
    }


    /* Testing Model->Patient and Patient->Model transforms for all permutations of axisOrder and axisFlip: */
    private static final void test(  ModelStorageBase image, int orientation, int[] axisOrder, boolean[] axisFlip )
    {
        float[] modelPoint = { 10, 20, 30 };
        float[] patientPoint = new float[3];

        int[] extents = image.getExtents( orientation );

        int[] axisOrderInverse = new int[3];
        getAxisOrderInverse( axisOrder, axisOrderInverse );

        // Model -> Patient
        for ( int i = 0; i < 3; i++ )
        {
            patientPoint[i] = modelPoint[ axisOrder[i] ];
        }
        for ( int i = 0; i < 3; i++ )
        {
            if ( axisFlip[ i ] )
            {
                patientPoint[i] = extents[ i ] - patientPoint[ i ] - 1;
            }
        }

        // Patient -> Model
        for ( int i = 0; i < 3; i++ )
        {
            if ( axisFlip[ i ] )
            {
                patientPoint[i] = extents[ i ] - patientPoint[ i ] - 1;
            }
        }
        float[] newModelPoint = new float[3];
        for ( int i = 0; i < 3; i++ )
        {
            newModelPoint[i] = patientPoint[ axisOrderInverse[i] ];
        }
        if ( (modelPoint[0] != newModelPoint[0]) ||
             (modelPoint[1] != newModelPoint[1]) ||
             (modelPoint[2] != newModelPoint[2])    )
        {
            System.err.println( "TEST FAILED: " + axisOrder[0] + " " + axisOrder[1] + " " + axisOrder[2]
                                + " " + axisFlip[0] + " " + axisFlip[1] + " " + axisFlip[2] );
        }
    }

    /* Inverts the input axisOrder so that points can be mapped back from
     * Patient Coordinates to Model Coordinates. The Inverse map is based only
     * on the input axisOrder map. */
    private static final void getAxisOrderInverse( int[] axisOrder, int[] axisOrderInverse )
    {
        for ( int i = 0; i < 3; i++ )
        {
            axisOrderInverse[ axisOrder[i] ] = i;
        }
    }


    /* Transform points from Patient Coordinates to Screen Coordinates, given
     * a scale factors (zoom * resolutions).
     * This function will change, depending on the scale factors...
     */
    public static final void PatientToScreen(Point3Df pIn, Point2Df pOut, Point2Df factors,
                                             ModelStorageBase image, 
                                             int orientation)
    {
        int[] extents = image.getExtents( orientation );
        pOut.x = (pIn.x * factors.x);
        pOut.y = (pIn.y * factors.y);
    }


    /* Transform points from Screen Coordinates to Patient Coordinates, given
     * a scale factors (zoom * resolutions).
     * This function will change, depending on the scale factors...
     */
    public static final void ScreenToPatient(Point3Df pIn, Point3Df pOut, Point2Df factors,
                                             ModelStorageBase image, 
                                             int orientation)
    {
        int[] extents = image.getExtents( orientation );
        pOut.x = (pIn.x / factors.x);
        pOut.y = (pIn.y / factors.y);
        pOut.z = pIn.z;
    }

    /* Transform points from Model Coordinates to Screen Coordinates: */
    public static final void ModelToScreen( Point3Df pIn, Point2Df pOut, Point2Df factors,
                                            ModelStorageBase image, int orientation )
    {
        Point3Df patient = new Point3Df();
        MipavCoordinateSystems.ModelToPatient( pIn, patient, image, orientation );
        MipavCoordinateSystems.PatientToScreen( patient, pOut, factors, image, orientation );
    }

    /* Transform points from Screen Coordinates to Model Coordinates: */
    public static final void ScreenToModel( Point3Df pIn, Point3Df pOut, Point2Df factors,
                                            ModelStorageBase image, int orientation )
    {
        Point3Df patient = new Point3Df();
        MipavCoordinateSystems.ScreenToPatient( pIn, patient, factors, image, orientation );
        MipavCoordinateSystems.PatientToModel( patient, pOut, image, orientation );
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
        boolean[] abAxisFlip = new boolean[3];
        abAxisFlip[0] = false;
        abAxisFlip[1] = false;
        abAxisFlip[2] = false;

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

        if ( (aiAxisOrientation != null) &&
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
        else if ( iOrientation == FileInfoBase.SAGITTAL )
        {
            aiAxisOrder[0] = 2;
            aiAxisOrder[1] = 1;
            aiAxisOrder[2] = 0;
        }
        else if ( iOrientation == FileInfoBase.CORONAL )
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



}
