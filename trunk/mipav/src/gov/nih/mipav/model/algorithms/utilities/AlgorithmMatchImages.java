package gov.nih.mipav.model.algorithms.utilities;


import gov.nih.mipav.MipavCoordinateSystems;
import gov.nih.mipav.model.algorithms.*;
import gov.nih.mipav.model.file.*;
import gov.nih.mipav.model.structures.*;
import WildMagic.LibFoundation.Mathematics.Matrix3f;
import WildMagic.LibFoundation.Mathematics.Matrix2f;

import gov.nih.mipav.view.*;
import gov.nih.mipav.view.dialogs.*;

import java.io.*;
import java.util.*;



public class AlgorithmMatchImages extends AlgorithmBase {

    private ModelImage srcImageB = null;
    private boolean doOrigins = true;
    private boolean doOrients = true;
    private float[] padValue = new float[]{0,0,0};
    
    public AlgorithmMatchImages(ModelImage kImageA, ModelImage kImageB,
            final boolean doOrigins, final boolean doOrients ) 
    {
        srcImage = kImageA;
        srcImageB = kImageB;
        this.doOrigins = doOrigins;
        this.doOrients = doOrients;
    }

    public void disposeLocal() 
    {
        srcImageB = null;
    }

    /**
     * Prepares this class for destruction.
     */
    public void finalize() {
        disposeLocal();
        super.finalize();
    }
    
    public ModelImage getImageA()
    {
        return srcImage;
    }

    public ModelImage getImageB()
    {
        return srcImageB;
    }
    
    public void setPadValue( float value )
    {
        padValue[0] = value;
    }
    
    public void setPadValue( float red, float green, float blue )
    {
        padValue[0] = red;
        padValue[1] = green;
        padValue[2] = blue;
    }

    public void runAlgorithm() {
        
        int[] axisA = srcImage.getAxisOrientation();
        int[] axisB = srcImageB.getAxisOrientation();

        if ( doOrients )
        {
            int[] axisOrder = { 0, 1, 2, 3 };
            boolean[] axisFlip = { false, false, false, false };
            if ( MipavCoordinateSystems.matchOrientation( axisA, axisB, axisOrder, axisFlip ) )
            {
                AlgorithmRotate rotateAlgo = new AlgorithmRotate( srcImageB, axisOrder, axisFlip );
                rotateAlgo.setRunningInSeparateThread(false);
                rotateAlgo.run();
                srcImageB = rotateAlgo.returnImage();
                //new ViewJFrameImage((ModelImage)srcImageB.clone(), null, null, false);
            }
        }
        matchUnits( srcImage, srcImageB );
        float[] afNewRes = matchResolutions( srcImage, srcImageB );
        if ( afNewRes != null )
        {
            srcImage = changeResolutions( srcImage, afNewRes );
            srcImageB = changeResolutions( srcImageB, afNewRes );
        }
        int[] xBoundA = new int[]{0,0};
        int[] yBoundA = new int[]{0,0};
        int[] zBoundA = new int[]{0,0};
        int[] xBoundB = new int[]{0,0};
        int[] yBoundB = new int[]{0,0};
        int[] zBoundB = new int[]{0,0};
        if ( matchOriginsExtents( srcImage, srcImageB, xBoundA, yBoundA, zBoundA, xBoundB, yBoundB, zBoundB ) )
        {
            srcImage = padImage(srcImage, xBoundA, yBoundA, zBoundA );
            srcImageB = padImage(srcImageB, xBoundB, yBoundB, zBoundB );
        }
        
        fireProgressStateChanged(100);
        setCompleted(true);
    }
    

    
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
                afResB[i] = (float)((FileInfoBase.conversionUnits[ imageB.getUnitsOfMeasure()[i]] * imageB.getResolutions(0)[i])/
                    FileInfoBase.conversionUnits[ imageA.getUnitsOfMeasure()[i]]);
            }
        }
        if ( imageB.getFileInfo() != null ) {
            for (int i = 0; i < imageB.getFileInfo().length; i++) {
                imageB.getFileInfo()[i].setResolutions( afResB );
                imageB.getFileInfo()[i].setUnitsOfMeasure(imageA.getUnitsOfMeasure());
            }
        }
    }

    private float[] matchResolutions( ModelImage imageA, ModelImage imageB )
    {

        if ( imageA.getNDims() != imageB.getNDims() )
        {
            return null;
        }
        float[] afRes = new float[imageA.getNDims()];
        for ( int i = 0; i < imageA.getNDims(); i++ )
        {
            if ( imageA.getResolutions(0)[i] < imageB.getResolutions(0)[i] )
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

    private ModelImage changeResolutions( ModelImage kImage, float[] afNewRes )
    {
        ModelImage kNewImage = kImage;
        boolean bChange = false;
        for ( int i = 0; i < kImage.getNDims(); i++ )
        {
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
                        false, true, false);

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
    
    private boolean matchOriginsExtents(ModelImage imageA, ModelImage imageB, 
            int[] xBoundsA, int[] yBoundsA, int[] zBoundsA,
            int[] xBoundsB, int[] yBoundsB, int[] zBoundsB
            )
    {
        float[] afOriginA = new float[imageA.getNDims()];
        float[] afOriginB = new float[imageB.getNDims()];
        int iDims = imageA.getNDims();
        for (int i = 0; i < iDims; i++) {
            afOriginA[i] = imageA.getOrigin()[i];
            afOriginB[i] = imageB.getOrigin()[i];
        }
        boolean bMatches = true;
        for ( int i = 0; i < Math.min(afOriginA.length, afOriginB.length); i++ )
        {
            if ( afOriginA[i] != afOriginB[i] )
            {
                bMatches = false;
                break;
            }
        }
        int[] padAFront = new int[]{0,0,0};
        int[] padBFront = new int[]{0,0,0};
        if ( !bMatches && doOrigins )
        {
            int[] axisA = imageA.getAxisOrientation();            
            float[] afDiff = new float[iDims];
            for ( int i = 0; i < iDims; i++ )
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
        int[] padABack = new int[]{0,0,0};
        int[] padBBack = new int[]{0,0,0};
        for ( int i = 0; i < iDims; i++ )
        {
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
        if ( bPadA )
        {
            //System.err.println( "Padding A" );
            //System.err.println( "    front = " + padAFront[0] + " " + padAFront[1] + " " + padAFront[2] );
            //System.err.println( "    back  = " + padABack[0] + " " + padABack[1] + " " + padABack[2] );
            if ( padAFront.length > 0 )
            {
                xBoundsA[0] = padAFront[0];  
                xBoundsA[1] = padABack[0];
            }
            if ( padAFront.length > 1 )
            {
                yBoundsA[0] = padAFront[1];  
                yBoundsA[1] = padABack[1];
            }
            if ( padAFront.length > 2 )
            {
                zBoundsA[0] = padAFront[2];  
                zBoundsA[1] = padABack[2];
            }
        }
        if ( bPadB )
        {
            //System.err.println( "Padding B" );
            //System.err.println( "    front = " + padBFront[0] + " " + padBFront[1] + " " + padBFront[2] );
            //System.err.println( "    back  = " + padBBack[0] + " " + padBBack[1] + " " + padBBack[2] );
            if ( padBFront.length > 0 )
            {
                xBoundsB[0] = padBFront[0];  
                xBoundsB[1] = padBBack[0];
            }
            if ( padBFront.length > 1 )
            {
                yBoundsB[0] = padBFront[1];  
                yBoundsB[1] = padBBack[1];
            }
            if ( padBFront.length > 2 )
            {
                zBoundsB[0] = padBFront[2];  
                zBoundsB[1] = padBBack[2];
            }
        }
        return (bPadA || bPadB);
    }
    
    private ModelImage padImage( ModelImage kImage, int[] xBounds, int[] yBounds, int[] zBounds )
    {
        if ( (xBounds[0] == 0) && (xBounds[1] == 0) &&
             (yBounds[0] == 0) && (yBounds[1] == 0) &&
             (zBounds[0] == 0) && (zBounds[1] == 0)    )
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
        
        ModelImage resultImage = new ModelImage(kImage.getType(), destExtents,
                                     JDialogBase.makeImageName(kImage.getImageName(), "_pad"));
        
        resultImage.setAll(kImage.getMin());
        AlgorithmAddMargins padAlgo = new AlgorithmAddMargins(kImage, resultImage, xBounds, yBounds, zBounds);
        padAlgo.setPadValue( padValue );
        padAlgo.setRunningInSeparateThread(false);
        padAlgo.run();
        return resultImage;
    }

}
