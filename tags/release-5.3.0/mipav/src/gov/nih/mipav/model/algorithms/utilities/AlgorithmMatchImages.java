package gov.nih.mipav.model.algorithms.utilities;


import gov.nih.mipav.util.MipavCoordinateSystems;
import gov.nih.mipav.model.algorithms.*;
import gov.nih.mipav.model.file.*;
import gov.nih.mipav.model.file.FileInfoBase.Unit;
import gov.nih.mipav.model.structures.*;
import gov.nih.mipav.view.dialogs.*;



/**
 * Matches two ModelImages. 
 * 
 * The output of this Algorithm is two ModelImages that have the same units of measure, 
 * the same resolutions, and the same extents. If the user chooses, the image orientations 
 * (left-to-right, anterior-to-posterior, inferior-to-superior, etc) will be matched. If the 
 * user chooses, the image origins will be matched.
 * 
 * Either imageA, imageB or sometimes neither will be different after the algorithm completes. 
 * The result images are returned with the getImageA() and getImageB() functions.
 *
 */
public class AlgorithmMatchImages extends AlgorithmBase {

    /** srcImageB: image to match to srcImage */
    private ModelImage srcImageB = null;
    /** flag for turning on/off using the image origins to match the images */
    private boolean doOrigins = true;
    /** flag for turning on/off using the image orientations to match the images */
    private boolean doOrients = true;
    /**  When true resolution matching defaults to the reference image. */
    private boolean useReferenceResolutions = false;
    /** image values to use for padding the images, if necessary */
    private float[] padValue = new float[]{0,0,0};
    
    /**
     * Create an AlgorithmMatchImages to match the two input images.
     * @param kImageA  target image to match to
     * @param kImageB  image that is changing to match to imageA
     * @param doOrigins flag for turning on/off using the image origins to match the images
     * @param doOrients flag for turning on/off using the image orientations to match the images
     */
    public AlgorithmMatchImages(ModelImage kImageA, ModelImage kImageB,
            final boolean doOrigins, final boolean doOrients ) 
    {
        srcImage = kImageA;
        srcImageB = kImageB;
        this.doOrigins = doOrigins;
        this.doOrients = doOrients;
    }    
    
    /**
     * Create an AlgorithmMatchImages to match the two input images.
     * @param kImageA  target image to match to
     * @param kImageB  image that is changing to match to imageA
     * @param doOrigins flag for turning on/off using the image origins to match the images
     * @param doOrients flag for turning on/off using the image orientations to match the images
     */
    public AlgorithmMatchImages(ModelImage kImageA, ModelImage kImageB,
            final boolean doOrigins, final boolean doOrients, final boolean useReferenceResolutions ) 
    {
        srcImage = kImageA;
        srcImageB = kImageB;
        this.doOrigins = doOrigins;
        this.doOrients = doOrients;
        this.useReferenceResolutions = useReferenceResolutions;
    }

    /**
     * remove local memory
     */
    public void disposeLocal() 
    {
        srcImageB = null;
        padValue = null;
    }

    /**
     * Prepares this class for destruction.
     */
    public void finalize() {
        disposeLocal();
        super.finalize();
    }
    
    /**
     * Returns imageA, which may have changed during the match.
     * @return imageA
     */
    public ModelImage getImageA()
    {
        return srcImage;
    }

    /**
     * Returns imageB, which may have changed during the match.
     * @return imageB
     */
    public ModelImage getImageB()
    {
        return srcImageB;
    }
    
    /**
     * Sets the pad value for single-channel images.
     * @param value value used to pad images.
     */
    public void setPadValue( float value )
    {
        padValue[0] = value;
    }
    
    /**
     * Sets the pad value for color images
     * @param red red pad value
     * @param green green pad value
     * @param blue blue pad value
     */
    public void setPadValue( float red, float green, float blue )
    {
        padValue[0] = red;
        padValue[1] = green;
        padValue[2] = blue;
    }

    /* (non-Javadoc)
     * @see gov.nih.mipav.model.algorithms.AlgorithmBase#runAlgorithm()
     */
    public void runAlgorithm() {
        // back up imageB in case it changes.
        ModelImage imageB_back = srcImageB;
        boolean bDeletePrevousB = false;
        // if the orientations are used to match the orientations, match orientations first.
        if ( doOrients )
        {
            int[] axisA = srcImage.getAxisOrientation();
            int[] axisB = srcImageB.getAxisOrientation();
            int[] axisOrder = { 0, 1, 2, 3 };
            boolean[] axisFlip = { false, false, false, false };
            if ( MipavCoordinateSystems.matchOrientation( axisA, axisB, axisOrder, axisFlip ) )
            {
                AlgorithmRotate rotateAlgo = new AlgorithmRotate( srcImageB, axisOrder, axisFlip );
                rotateAlgo.setRunningInSeparateThread(false);
                rotateAlgo.run();
                srcImageB = rotateAlgo.returnImage();
                if ( imageB_back != srcImageB )
                {
                    bDeletePrevousB = true;
                    imageB_back = srcImageB;
                }
                //new ViewJFrameImage((ModelImage)srcImageB.clone(), null, null, false);
            }
        }
        // We don't want to deleted the original version of imageA
        boolean bDeletePrevousA = false;
        // back up imageA in case it changes
        ModelImage imageA_back = srcImage;
        // match the units for imageA and imageB: changes the internal fileInfo in imageB
        matchUnits( srcImage, srcImageB );
        //  match the image resolutions for imageA and imageB
        float[] afNewRes = matchResolutions( srcImage, srcImageB );
        if ( afNewRes != null )
        {
            // Change the resolutions for imageA
            srcImage = changeResolutions( srcImage, afNewRes );
            if ( imageA_back != srcImage )
            {
                // if the image changes, set the back up version and flag for deleting the backup if imageA changes again.
                imageA_back = srcImage;
                bDeletePrevousA = true;
            }
            // Change the resolutions for imageB
            srcImageB = changeResolutions( srcImageB, afNewRes );
            if ( imageB_back != srcImageB )
            {
                // if imageB changes, deleted the backup copy.
                if ( bDeletePrevousB )
                {
                    imageB_back.disposeLocal();
                }
                imageB_back = srcImageB;
                bDeletePrevousB = true;
            }
        }
        // match the origins and extents of the two images
        int iDim = srcImage.getNDims();
        int[] padAFront = new int[iDim];
        int[] padABack = new int[iDim];
        int[] padBFront = new int[iDim];
        int[] padBBack = new int[iDim];
        if ( matchOriginsExtents( srcImage, srcImageB, padAFront, padABack, padBFront, padBBack ) )
        {
            // pad imageA if necessary
            srcImage = padImage(srcImage, padAFront, padABack );
            if ( imageA_back != srcImage && bDeletePrevousA )
            {
                // if the image changes and the backup is not the original, deleted the backup of imageA
                imageA_back.disposeLocal();
            }
            // pad imageB if necessary
            srcImageB = padImage(srcImageB, padBFront, padBBack );
            if ( imageB_back != srcImageB && bDeletePrevousB )
            {
                // if imageB changes, delete the backup
                imageB_back.disposeLocal();
            }
        }
        
        fireProgressStateChanged(100);
        setCompleted(true);
    }
    

    
    /**
     * Matches the units of measure for the two input images.
     * @param imageA target image to match to.
     * @param imageB image that is being changed.
     */
    private void matchUnits(ModelImage imageA, ModelImage imageB)
    {
        if ( imageA.getNDims() != imageB.getNDims() )
        {
            return;
        }
        float[] afResB = new float[imageA.getNDims()];
        for ( int i = 0; i < imageA.getNDims(); i++ )
        {
            afResB[i] = imageB.getResolutions(0)[i];
            if ( imageA.getUnitsOfMeasure()[i] != imageB.getUnitsOfMeasure()[1] )
            {
                afResB[i] = (float)((Unit.getUnitFromLegacyNum( imageB.getUnitsOfMeasure()[i])).
                		getConversionFactor(Unit.getUnitFromLegacyNum(imageA.getUnitsOfMeasure()[i])) * imageB.getResolutions(0)[i]);
            }
        }
        if ( imageB.getFileInfo() != null ) {
            for (int i = 0; i < imageB.getFileInfo().length; i++) {
                imageB.getFileInfo()[i].setResolutions( afResB );
                imageB.getFileInfo()[i].setUnitsOfMeasure(imageA.getUnitsOfMeasure());
            }
        }
    }

    /**
     * Match the resolutions of the two images. Determines the lowest resolution 
     * of either image along each dimension.
     * If useReferenceResolutions is true, the reference image (imageA) resolutions are used.
     * @param imageA
     * @param imageB
     * @return new resolutions that will be applied to both images.
     */
    private float[] matchResolutions( ModelImage imageA, ModelImage imageB )
    {

        if ( imageA.getNDims() != imageB.getNDims() )
        {
            return null;
        }
        float[] afRes = new float[imageA.getNDims()];
        for ( int i = 0; i < imageA.getNDims(); i++ )
        {
            if ( (imageA.getResolutions(0)[i] < imageB.getResolutions(0)[i]) || useReferenceResolutions )
            {
                afRes[i] = imageA.getResolutions(0)[i];
            }
            else
            {
                afRes[i] = imageB.getResolutions(0)[i];
            }
        }
        return afRes;
    }

    /**
     * Changes the resolutions of the input image. 
     * @param kImage input image.
     * @param afNewRes new resolutions.
     * @return new ModelImage, or the input image if unchanged.
     */
    private ModelImage changeResolutions( ModelImage kImage, float[] afNewRes )
    {
        ModelImage kNewImage = kImage;
        boolean bChange = false;
        for ( int i = 0; i < kImage.getNDims(); i++ )
        {
            System.err.println( afNewRes[i] + " " + kImage.getResolutions(0)[i] );
            if ( afNewRes[i] != kImage.getResolutions(0)[i] )
            {
                bChange = true;
            }
        }
        if ( bChange )
        {
            int[] newExtents = new int[kImage.getNDims()];
            for ( int i = 0; i < kImage.getNDims(); i++ )
            {
                newExtents[i] = Math.round(kImage.getResolutions(0)[i] * kImage.getExtents()[i] / afNewRes[i]);
            }
            if ( kImage.getNDims() == 3 )
            {
                AlgorithmTransform transformFunct = new AlgorithmTransform( kImage, new TransMatrix(4), AlgorithmTransform.TRILINEAR,
                        afNewRes[0], afNewRes[1], afNewRes[2], 
                        newExtents[0], newExtents[1], newExtents[2], 
                        true, true, false);

                transformFunct.setRunningInSeparateThread(false);
                transformFunct.run();

                if (transformFunct.isCompleted() == false) {
                    transformFunct.finalize();
                    transformFunct = null;
                }

                kNewImage = transformFunct.getTransformedImage();
                kNewImage.calcMinMax();

                transformFunct.disposeLocal();
                transformFunct = null;
            }
        }
        return kNewImage;
    }
    

    /**
     * Matches the origins and extents of the two input images. Matching origins is an option the user sets.
     * @param imageA input image A.
     * @param imageB input image B.
     * @param padAFront output values for adding or removing voxels to the front of imageA [left,top,front,time]
     * @param padABack output values for adding or removing voxels to the back of imageA [right,bottom,back,time]
     * @param padBFront output values for adding or removing voxels to the front of imageB [left,top,front,time]
     * @param padBBack output values for adding or removing voxels to the back of imageB [right,bottom,back,time]
     * @return true if either image requires changing to match the two images, false if neither image requires changing.
     */
    private boolean matchOriginsExtents(ModelImage imageA, ModelImage imageB, 
            int[] padAFront, int[] padABack, int[] padBFront, int[] padBBack )
    {
        float[] afOriginA = new float[imageA.getNDims()];
        float[] afOriginB = new float[imageB.getNDims()];
        int iDims = Math.min( imageA.getNDims(), imageB.getNDims() );
        for (int i = 0; i < iDims; i++) {
            afOriginA[i] = imageA.getOrigin()[i];
            afOriginB[i] = imageB.getOrigin()[i];
        }
        boolean bMatches = true;
        for ( int i = 0; i < iDims; i++ )
        {
            if ( afOriginA[i] != afOriginB[i] )
            {
                bMatches = false;
                break;
            }
        }
        for ( int i = 0; i < iDims; i++ )
        {
            padAFront[i] = 0;
            padBFront[i] = 0;
        }
        if ( !bMatches && doOrigins )
        {
            int[] axisA = imageA.getAxisOrientation();            
            float[] afDiff = new float[iDims];
            for ( int i = 0; i < Math.min( 3, iDims ); i++ )
            {
                afDiff[i] = Math.abs( afOriginA[i] - afOriginB[i] );

                if ((axisA[i] == FileInfoBase.ORI_A2P_TYPE) || (axisA[i] == FileInfoBase.ORI_R2L_TYPE) ||
                        (axisA[i] == FileInfoBase.ORI_I2S_TYPE))
                {
                    // subtract
                    if ( afOriginB[i] < afOriginA[i] )
                    {
                        padAFront[i] = (int) Math.ceil( afDiff[i] / imageA.getResolutions(0)[i] );
                    }
                    if ( afOriginA[i] < afOriginB[i] )
                    {
                        padBFront[i] = (int) Math.ceil( afDiff[i] / imageA.getResolutions(0)[i] );
                    }
                }
                else
                {
                    // add
                    if ( afOriginB[i] < afOriginA[i] )
                    {
                        padBFront[i] = (int) Math.ceil( afDiff[i] / imageA.getResolutions(0)[i] );
                    }
                    if ( afOriginA[i] < afOriginB[i] )
                    {
                        padAFront[i] = (int) Math.ceil( afDiff[i] / imageA.getResolutions(0)[i] );
                    }
                }
            }                       
        }
        for ( int i = 0; i < iDims; i++ )
        {
            padABack[i] = 0;
            padBBack[i] = 0;
            int sizeA = padAFront[i] + imageA.getExtents()[i];
            int sizeB = padBFront[i] + imageB.getExtents()[i];
            if ( sizeA < sizeB )
            {
                padABack[i] = sizeB - sizeA;
            }
            else if ( sizeA > sizeB )
            {
                padBBack[i] = sizeA - sizeB;
            }
        }
        boolean bPadA = false;
        boolean bPadB = false;
        for ( int i = 0; i < iDims; i++ )
        {
            if ( padAFront[i] != 0 || padABack[i] != 0 )
            {
                bPadA = true;
            }
            if ( padBFront[i] != 0 || padBBack[i] != 0 )
            {
                bPadB = true;
            }
        }

        return (bPadA || bPadB);
    }
    

    /**
     * Adds or removes margins to the input image.
     * @param kImage input images.
     * @param padFront margins to add or remove to the front of the image [left,top,front,time]
     * @param padBack margins to add or remove to the end of the image [right,bottom,back,time]
     * @return new image or the original input image if no change.
     */
    private ModelImage padImage( ModelImage kImage, int[] padFront, int[] padBack )
    {
        int[] xBounds = new int[]{0,0};
        int[] yBounds = new int[]{0,0};
        int[] zBounds = new int[]{0,0};
        int[] tBounds = new int[]{0,0};

        if ( padFront.length > 0 )
        {
            xBounds[0] = padFront[0];  
            xBounds[1] = padBack[0];
        }
        if ( padFront.length > 1 )
        {
            yBounds[0] = padFront[1];  
            yBounds[1] = padBack[1];
        }
        if ( padFront.length > 2 )
        {
            zBounds[0] = padFront[2];  
            zBounds[1] = padBack[2];
        }
        if ( padFront.length > 3 )
        {
            tBounds[0] = padFront[3];  
            tBounds[1] = padBack[3];
        }
        
        if ( (xBounds[0] == 0) && (xBounds[1] == 0) &&
             (yBounds[0] == 0) && (yBounds[1] == 0) &&
             (zBounds[0] == 0) && (zBounds[1] == 0) &&
             (tBounds[0] == 0) && (tBounds[1] == 0)    )
        {
            return kImage;
        }
        int[] destExtents = new int[kImage.getNDims()];
        if ( kImage.getNDims() > 0 )
        {
            destExtents[0] = xBounds[0] + xBounds[1] + kImage.getExtents()[0];
        }
        if ( kImage.getNDims() > 1 )
        {
            destExtents[1] = yBounds[0] + yBounds[1] + kImage.getExtents()[1];
        }
        if ( kImage.getNDims() > 2 )
        {
            destExtents[2] = zBounds[0] + zBounds[1] + kImage.getExtents()[2];
        }
        if ( kImage.getNDims() > 3 )
        {
            destExtents[3] = tBounds[0] + tBounds[1] + kImage.getExtents()[3];
        }
        
        try {
            ModelImage resultImage = new ModelImage(kImage.getType(), destExtents,
                    JDialogBase.makeImageName(kImage.getImageName(), "_pad"));

            resultImage.setAll(kImage.getMin());
            AlgorithmAddMargins padAlgo = new AlgorithmAddMargins(kImage, resultImage, xBounds, yBounds, zBounds);
            padAlgo.setPadValue( padValue );
            padAlgo.setTMargins( tBounds );
            padAlgo.setRunningInSeparateThread(false);
            padAlgo.run();
            return resultImage;
        } catch ( OutOfMemoryError e )
        {
            System.err.println( destExtents[0] + " " + destExtents[1] + " " + destExtents[2] );
        }
        return null;
    }

}
