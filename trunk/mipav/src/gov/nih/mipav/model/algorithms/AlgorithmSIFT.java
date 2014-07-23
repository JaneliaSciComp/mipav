package gov.nih.mipav.model.algorithms;

import gov.nih.mipav.model.structures.*;
import gov.nih.mipav.view.*;

import java.io.*;
import java.util.Vector;

import WildMagic.LibFoundation.Mathematics.Vector3f;

public class AlgorithmSIFT extends AlgorithmBase implements AlgorithmInterface {
	
	private static final int aOp = 1;
	
	private static final int bOp = 2;
	
	private int operationType = aOp;
	
	private float bufferA[];
	
	private float bufferB[];
	
	//~ Constructors ---------------------------------------------------------------------------------------------------

    /**
     * AlgorithmSIFT - default constructor.
     */
    public AlgorithmSIFT() { }
    
    /**
     * AlgorithmSIFT.
     *
     * @param  destImg  DOCUMENT ME!
     * @param  srcImg   DOCUMENT ME!
     */
    public AlgorithmSIFT(ModelImage destImg, ModelImage srcImg) {
        super(destImg, srcImg);
    }


    //~ Methods --------------------------------------------------------------------------------------------------------

    /**
     * finalize -
     */
    public void finalize() {
        super.finalize();
    }

    /**
     * Starts the program.
     */
    public void runAlgorithm() {

        int xDim;
        int yDim;
        int sliceSize;
        AlgorithmTransform algoTrans;
        int constantFOV;
        int interp;
        TransMatrix xfrm;
        float iXres;
        float iYres;
        int iXdim;
        int iYdim;
        double factor;
        int oXdim;
        int oYdim;
        float oXres;
        float oYres;
        boolean doRotateCenter;
        int[] units;
        boolean doVOI;
        boolean doClip;
        boolean doPad;
        Vector3f center;
        float fillValue;
        boolean doUpdateOrigin;
        Vector<float[]> diffPyr = new Vector<float[]>();
        ModelImage imageA;
        ModelImage imageB;
        int[] derivOrder = new int[2];
        float sigma;
        int[] kExtents = new int[2];
        ModelImage inputImage;
        float[] GData;
        float[] sigmaArray;
        AlgorithmConvolver convolver;
        boolean entireImage;
        int iExtents[] = new int[2];
        float resArray[] = new float[2];
        int bufSize;
        float diffBuffer[];
        int i;
        
        if (srcImage == null) {
            displayError("Source Image is null");
            finalize();

            return;
        }
        
        fireProgressStateChanged(srcImage.getImageName(), "Running SIFT ...");
        
        srcImage.makeUnitsOfMeasureIdentical();
        xDim = srcImage.getExtents()[0];
        yDim = srcImage.getExtents()[1];
        sliceSize = xDim * yDim;
        
        // Preserve the field of view = dim * res = transformedDim * transformedRes.
        // May lead to duplicate values at ends of image
        constantFOV = 0;
        // The alternative using constantFOV = 1 matches start and end values without duplication
        // but does not preserve the field of view.
        // This uses constantFOV = 1
        interp = AlgorithmTransform.BILINEAR;
        xfrm = new TransMatrix(3);
        xfrm.identity();
        iXres = srcImage.getFileInfo(0).getResolutions()[0];
        iYres = srcImage.getFileInfo(0).getResolutions()[1];
        iXdim = srcImage.getExtents()[0];
        iYdim = srcImage.getExtents()[1];
        factor = 2.0;
        oXdim = (int)Math.round(iXdim * factor);
        oYdim = (int)Math.round(iYdim * factor);
        oXres = iXres * (iXdim - constantFOV)/ (oXdim - constantFOV);
        oYres = iYres * (iYdim - constantFOV) / (oYdim - constantFOV);
        // Since ( ( (iXres * (iXdim - constantFOV)) == (oXres * (oXdim - constantFOV)))
        // && ( (iYres * (iYdim - constantFOV)) == (oYres * (oYdim - constantFOV)))),
        // the zoom factors are 1.0.
        doRotateCenter = false;
        units = new int[srcImage.getUnitsOfMeasure().length];
        for (i = 0; i < units.length; i++) {
            units[i] = srcImage.getUnitsOfMeasure(i);
        }
        doVOI = false;
        doClip = true;
        doPad = false;
        center = null;
        fillValue = (float)srcImage.getMin();
        doUpdateOrigin = true;
        
        // If the first level of the pyramid is sampled at the same rate as the input image, the highest
        // spatial frequencies will be ignored.  This is due to the initial smoothing, which is needed to
        // provide separation of peaks for robust detection.  Therefore, we expand the input image by a factor
        // of 2, using bilinear interpolation, prior to building the pyramid;
        algoTrans = new AlgorithmTransform(srcImage, xfrm, interp, oXres, oYres, oXdim, oYdim, units, doVOI, doClip,
                doPad, doRotateCenter, center);
        algoTrans.setFillValue(fillValue);
        algoTrans.setUpdateOriginFlag(doUpdateOrigin);
        algoTrans.run();
        inputImage = algoTrans.getTransformedImage();
        algoTrans.finalize();
        algoTrans = null;
        
        factor = 1/1.5;
        derivOrder[0] = 0;
        derivOrder[1] = 0;
        sigma = (float)Math.sqrt(2.0);
        // For key localization, all smoothing operations are done using sigma = sqrt(2.0), which can be
        // approximated with sufficient accuracy using a 1D kernel with 7 sample points.
        kExtents[0] = 7;
        kExtents[1] = 7;
        GData = new float[kExtents[0] * kExtents[1]];
        sigmaArray = new float[]{sigma, sigma};
        GenerateGaussian G = new GenerateGaussian(GData, kExtents, sigmaArray, derivOrder);
        G.calc(false);
        entireImage = true;
        while ((iXdim >= 7) && (iYdim > 7)) {
        	iXres = oXres;
            iYres = oYres;
            resArray[0] = iXres;
            resArray[1] = iYres;
            iXdim = oXdim;
            iYdim = oYdim;
            iExtents[0] = iXdim;
            iExtents[1] = iYdim;
            bufSize = iXdim * iYdim;
        	imageA = new ModelImage(ModelStorageBase.FLOAT, iExtents, "imageA");
            imageA.getFileInfo()[0].setResolutions(resArray);
            imageB = new ModelImage(ModelStorageBase.FLOAT, iExtents, "imageB");
            imageB.getFileInfo()[0].setResolutions(resArray);
        	convolver = new AlgorithmConvolver(inputImage, GData, kExtents, entireImage, image25D);
            //convolver.setMinProgressValue(0);
            //convolver.setMaxProgressValue(10);
            //linkProgressToAlgorithm(convolver);
            convolver.addListener(this);
            operationType = aOp;
            convolver.run();
            
            try {
            	imageA.importData(0, bufferA, true);
            }
            catch(IOException e) {
            	MipavUtil.displayError("IOException " + e + " on imageA.importData(0, bufferA, true");
            	setCompleted(false);
            	return;
            }
            
            convolver = new AlgorithmConvolver(imageA, GData, kExtents, entireImage, image25D);
            //convolver.setMinProgressValue(0);
            //convolver.setMaxProgressValue(10);
            //linkProgressToAlgorithm(convolver);
            convolver.addListener(this);
            operationType = bOp;
            convolver.run();
            
            try {
            	imageB.importData(0, bufferB, true);
            }
            catch(IOException e) {
            	MipavUtil.displayError("IOException " + e + " on imageB.importData(0, bufferB, true");
            	setCompleted(false);
            	return;
            }
            
            diffBuffer = new float[bufSize];
            for (i = 0; i < bufSize; i++) {
            	diffBuffer[i] = bufferA[i] - bufferB[i];
            }
            
            diffPyr.add(diffBuffer);
            
            oXdim = (int)Math.round(iXdim * factor);
            oYdim = (int)Math.round(iYdim * factor);
            oXres = iXres * (iXdim - constantFOV)/ (oXdim - constantFOV);
            oYres = iYres * (iYdim - constantFOV) / (oYdim - constantFOV);
            algoTrans = new AlgorithmTransform(imageB, xfrm, interp, oXres, oYres, oXdim, oYdim, units, doVOI, doClip,
                    doPad, doRotateCenter, center);
            algoTrans.setFillValue(fillValue);
            algoTrans.setUpdateOriginFlag(doUpdateOrigin);
            algoTrans.run();
            inputImage = algoTrans.getTransformedImage();
            algoTrans.finalize();
            algoTrans = null;
            imageA.disposeLocal();
            imageA = null;
            imageB.disposeLocal();
            imageB = null;
            bufferA = null;
            bufferB = null;
        } // while ((iXdim >= 7) && (iYdim > 7))
        
        setCompleted(true);
        return;
    }
    
    public void algorithmPerformed(AlgorithmBase algorithm){
        if(!algorithm.isCompleted()){
            finalize();
            return;
        }
        if (algorithm instanceof AlgorithmConvolver) {
            AlgorithmConvolver convolver = (AlgorithmConvolver) algorithm;
            if (operationType == aOp) {
                bufferA = convolver.getOutputBuffer();
            }
            else if (operationType == bOp) {
            	bufferB = convolver.getOutputBuffer();
            }
        }
    }
}