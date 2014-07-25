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
        Vector<float[][]> diffPyr = new Vector<float[][]>();
        Vector<int[]>extentsPyr = new Vector<int[]>();
        Vector<double[]>magPyr = new Vector<double[]>();
        Vector<double[]>orPyr = new Vector<double[]>();
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
        float diffBuffer[][];
        int i;
        int pyramidLevels = 0;
        byte[][][] minMax = null;
        int x;
        int y;
        float[][][] diffLevels = null;
        int extentLevels[][] = null;
        int yl;
        int xl;
        int xh;
        int yh;
        double maxGrad = -Double.MAX_VALUE;
        double magBuffer[];
        double orBuffer[];
        double diffX;
        double diffY;
        double thresholdGrad;
        int xkDim;
        int ykDim;
        float GOrData[];
        int halfWin;
        double magLevels[][];
        double orLevels[][];
        int index;
        int ym;
        int xm;
        double sum;
        double weight;
        double weightSum;
        double orientation;
        long histoBins[] = new long[36];
        int binNum;
        
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
        
        // Calculate Gaussian for orientation histogram
        sigma = (float)(3.0 * Math.sqrt(2.0));
        xkDim = Math.round(11 * sigma);

        if ((xkDim % 2) == 0) {
            xkDim++;
        }

        if (xkDim < 3) {
            xkDim = 3;
        }

        kExtents[0] = xkDim;
        halfWin = (xkDim - 1)/2;

        ykDim = Math.round(11 * sigma);

        if ((ykDim % 2) == 0) {
            ykDim++;
        }

        if (ykDim < 3) {
            ykDim = 3;
        }

        kExtents[1] = ykDim;

        GOrData = new float[xkDim * ykDim];
        sigmaArray = new float[]{sigma, sigma};
        GenerateGaussian GOr = new GenerateGaussian(GOrData, kExtents, sigmaArray, derivOrder);
        GOr.calc(false);
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
            magBuffer = new double[bufSize];
            orBuffer = new double[bufSize];
        	imageA = new ModelImage(ModelStorageBase.FLOAT, iExtents, "imageA");
            imageA.getFileInfo()[0].setResolutions(resArray);
            imageA.getFileInfo()[0].setUnitsOfMeasure(units);
            imageB = new ModelImage(ModelStorageBase.FLOAT, iExtents, "imageB");
            imageB.getFileInfo()[0].setResolutions(resArray);
            imageB.getFileInfo()[0].setUnitsOfMeasure(units);
        	convolver = new AlgorithmConvolver(inputImage, GData, kExtents, entireImage, image25D);
            //convolver.setMinProgressValue(0);
            //convolver.setMaxProgressValue(10);
            //linkProgressToAlgorithm(convolver);
            convolver.addListener(this);
            operationType = aOp;
            convolver.run();
            
            for (y = 0; y < iYdim; y++) {
            	for (x = 0; x < iXdim; x++) {
            		i = x + y * iXdim;
            		if (x < iXdim - 1) {
            		    diffX = bufferA[i+1] - bufferA[i];
            		}
            		else {
            			diffX = bufferA[i] - bufferA[i-1];
            		}
            		if (y < iYdim - 1) {
            		    diffY = bufferA[i+xDim] - bufferA[i];
            		}
            		else {
            			diffY = bufferA[i] - bufferA[i-xDim];
            		}
            		magBuffer[i] = Math.sqrt(diffX*diffX + diffY*diffY);
            		if (magBuffer[i] > maxGrad) {
            			maxGrad = magBuffer[i];
            		}
            		orBuffer[i] = Math.atan2(diffY, diffX);
            	}
            }
            magPyr.add(magBuffer);
            orPyr.add(orBuffer);
            
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
            
            diffBuffer = new float[iYdim][iXdim];
            for (y = 0; y < iYdim; y++) {
            	for (x = 0; x < iXdim; x++) {
            	    i = x + y * iXdim;
            	    diffBuffer[y][x] = bufferA[i] - bufferB[i];
            	}
            }
            
            diffPyr.add(diffBuffer);
            extentsPyr.add(iExtents);
            pyramidLevels++;
            
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
        
        Preferences.debug("The number of pyramid levels = " + pyramidLevels + "\n", Preferences.DEBUG_ALGORITHM);
        extentLevels = new int[pyramidLevels][];
        diffLevels =  new float[pyramidLevels][][];
        minMax = new byte[pyramidLevels][][];
        magLevels = new double[pyramidLevels][];
        orLevels = new double[pyramidLevels][];
        for (i = 0; i < pyramidLevels; i++) {
        	extentLevels[i] = extentsPyr.get(i);
        	diffLevels[i] = diffPyr.get(i);
        	minMax[i] = new byte[extentLevels[i][1]][extentLevels[i][0]];
        	magLevels[i] = magPyr.get(i);
        	orLevels[i] = orPyr.get(i);
        }
        
        thresholdGrad = 0.1 * maxGrad;
        
        for (i = 0; i < pyramidLevels; i++) {
        	for (y = 0; y < extentLevels[i][1]; y++) {
        	     for (x = 0; x < extentLevels[i][0]; x++) {
        	    	 // Find minima
        	    	 minMax[i][y][x] = -1;
        	    	 if ((x > 0) && (diffLevels[i][y][x] > diffLevels[i][y][x-1])) {
        	    		 minMax[i][y][x] = 0;
        	    		 continue;
        	    	 }
        	    	 if ((x < (extentLevels[i][0]-1)) && (diffLevels[i][y][x] > diffLevels[i][y][x+1])) {
        	    		 minMax[i][y][x] = 0;
        	    		 continue;
        	    	 }
        	    	 if ((y > 0) && (diffLevels[i][y][x] > diffLevels[i][y-1][x])) {
        	    		 minMax[i][y][x] = 0;
        	    		 continue;
        	    	 }
        	    	 if ((y < (extentLevels[i][1]-1)) && (diffLevels[i][y][x] > diffLevels[i][y+1][x])) {
        	    		 minMax[i][y][x] = 0;
        	    		 continue;
        	    	 }
        	    	 if ((x > 0) && (y > 0) && (diffLevels[i][y][x] > diffLevels[i][y-1][x-1])) {
        	    		 minMax[i][y][x] = 0;
        	    		 continue;
        	    	 }
        	    	 if ((x < (extentLevels[i][0]-1)) && (y > 0) && (diffLevels[i][y][x] > diffLevels[i][y-1][x+1])) {
        	    		 minMax[i][y][x] = 0;
        	    		 continue;
        	    	 }
        	    	 if ((x > 0) && (y < (extentLevels[i][1]-1)) && (diffLevels[i][y][x] > diffLevels[i][y+1][x-1])) {
        	    		 minMax[i][y][x] = 0;
        	    		 continue;
        	    	 }
        	    	 if ((x < (extentLevels[i][0]-1)) && (y < (extentLevels[i][1]-1)) && (diffLevels[i][y][x] > diffLevels[i][y+1][x+1])) {
        	    		 minMax[i][y][x] = 0;
        	    		 continue;
        	    	 }
        	    	 if (i > 0) {
        	    	     xl = (int)Math.round(1.5*x);
        	    	     yl = (int)Math.round(1.5*y);
        	    	     if (diffLevels[i][y][x] > diffLevels[i-1][yl][xl]) {
        	    	    	 minMax[i][y][x] = 0;
        	    	    	 continue;
        	    	     }
        	    	     if ((xl > 0) && (diffLevels[i][y][x] > diffLevels[i-1][yl][xl-1])) {
            	    		 minMax[i][y][x] = 0;
            	    		 continue;
            	    	 }
            	    	 if ((xl < (extentLevels[i-1][0]-1)) && (diffLevels[i][y][x] > diffLevels[i-1][yl][xl+1])) {
            	    		 minMax[i][y][x] = 0;
            	    		 continue;
            	    	 }
            	    	 if ((yl > 0) && (diffLevels[i][y][x] > diffLevels[i-1][yl-1][xl])) {
            	    		 minMax[i][y][x] = 0;
            	    		 continue;
            	    	 }
            	    	 if ((yl < (extentLevels[i-1][1]-1)) && (diffLevels[i][y][x] > diffLevels[i-1][yl+1][xl])) {
            	    		 minMax[i][y][x] = 0;
            	    		 continue;
            	    	 }
            	    	 if ((xl > 0) && (yl > 0) && (diffLevels[i][y][x] > diffLevels[i-1][yl-1][xl-1])) {
            	    		 minMax[i][y][x] = 0;
            	    		 continue;
            	    	 }
            	    	 if ((xl < (extentLevels[i-1][0]-1)) && (yl > 0) && (diffLevels[i][y][x] > diffLevels[i-1][yl-1][xl+1])) {
            	    		 minMax[i][y][x] = 0;
            	    		 continue;
            	    	 }
            	    	 if ((xl > 0) && (yl < (extentLevels[i-1][1]-1)) && (diffLevels[i][y][x] > diffLevels[i-1][yl+1][xl-1])) {
            	    		 minMax[i][y][x] = 0;
            	    		 continue;
            	    	 }
            	    	 if ((xl < (extentLevels[i-1][0]-1)) && (yl < (extentLevels[i-1][1]-1)) &&
            	    			 (diffLevels[i][y][x] > diffLevels[i-1][yl+1][xl+1])) {
            	    		 minMax[i][y][x] = 0;
            	    		 continue;
            	    	 }
        	    	 } // if (i > 0)
        	    	 if (i < (pyramidLevels-1)) {
        	    		 xh = (int)Math.round(x/1.5);
        	    	     yh = (int)Math.round(y/1.5);
        	    	     if (diffLevels[i][y][x] > diffLevels[i+1][yh][xh]) {
        	    	    	 minMax[i][y][x] = 0;
        	    	    	 continue;
        	    	     }
        	    	     if ((xh > 0) && (diffLevels[i][y][x] > diffLevels[i+1][yh][xh-1])) {
            	    		 minMax[i][y][x] = 0;
            	    		 continue;
            	    	 }
            	    	 if ((xh < (extentLevels[i+1][0]-1)) && (diffLevels[i][y][x] > diffLevels[i+1][yh][xh+1])) {
            	    		 minMax[i][y][x] = 0;
            	    		 continue;
            	    	 }
            	    	 if ((yh > 0) && (diffLevels[i][y][x] > diffLevels[i+1][yh-1][xh])) {
            	    		 minMax[i][y][x] = 0;
            	    		 continue;
            	    	 }
            	    	 if ((yh < (extentLevels[i+1][1]-1)) && (diffLevels[i][y][x] > diffLevels[i+1][yh+1][xh])) {
            	    		 minMax[i][y][x] = 0;
            	    		 continue;
            	    	 }
            	    	 if ((xh > 0) && (yh > 0) && (diffLevels[i][y][x] > diffLevels[i+1][yh-1][xh-1])) {
            	    		 minMax[i][y][x] = 0;
            	    		 continue;
            	    	 }
            	    	 if ((xh < (extentLevels[i+1][0]-1)) && (yh > 0) && (diffLevels[i][y][x] > diffLevels[i+1][yh-1][xh+1])) {
            	    		 minMax[i][y][x] = 0;
            	    		 continue;
            	    	 }
            	    	 if ((xh > 0) && (yh < (extentLevels[i+1][1]-1)) && (diffLevels[i][y][x] > diffLevels[i+1][yh+1][xh-1])) {
            	    		 minMax[i][y][x] = 0;
            	    		 continue;
            	    	 }
            	    	 if ((xh < (extentLevels[i+1][0]-1)) && (yh < (extentLevels[i+1][1]-1)) &&
            	    			 (diffLevels[i][y][x] > diffLevels[i+1][yh+1][xh+1])) {
            	    		 minMax[i][y][x] = 0;
            	    		 continue;
            	    	 }	 
        	    	 } // if (i < (pyramidLevels-1))
        	    	 if (minMax[i][y][x] != -1) {
        	    	     // Find maxima
        	    		 minMax[i][y][x] = 1;
        	    		 if ((x > 0) && (diffLevels[i][y][x] < diffLevels[i][y][x-1])) {
            	    		 minMax[i][y][x] = 0;
            	    		 continue;
            	    	 }
            	    	 if ((x < (extentLevels[i][0]-1)) && (diffLevels[i][y][x] < diffLevels[i][y][x+1])) {
            	    		 minMax[i][y][x] = 0;
            	    		 continue;
            	    	 }
            	    	 if ((y > 0) && (diffLevels[i][y][x] < diffLevels[i][y-1][x])) {
            	    		 minMax[i][y][x] = 0;
            	    		 continue;
            	    	 }
            	    	 if ((y < (extentLevels[i][1]-1)) && (diffLevels[i][y][x] < diffLevels[i][y+1][x])) {
            	    		 minMax[i][y][x] = 0;
            	    		 continue;
            	    	 }
            	    	 if ((x > 0) && (y > 0) && (diffLevels[i][y][x] < diffLevels[i][y-1][x-1])) {
            	    		 minMax[i][y][x] = 0;
            	    		 continue;
            	    	 }
            	    	 if ((x < (extentLevels[i][0]-1)) && (y > 0) && (diffLevels[i][y][x] < diffLevels[i][y-1][x+1])) {
            	    		 minMax[i][y][x] = 0;
            	    		 continue;
            	    	 }
            	    	 if ((x > 0) && (y < (extentLevels[i][1]-1)) && (diffLevels[i][y][x] < diffLevels[i][y+1][x-1])) {
            	    		 minMax[i][y][x] = 0;
            	    		 continue;
            	    	 }
            	    	 if ((x < (extentLevels[i][0]-1)) && (y < (extentLevels[i][1]-1)) && (diffLevels[i][y][x] < diffLevels[i][y+1][x+1])) {
            	    		 minMax[i][y][x] = 0;
            	    		 continue;
            	    	 }
            	    	 if (i > 0) {
            	    	     xl = (int)Math.round(1.5*x);
            	    	     yl = (int)Math.round(1.5*y);
            	    	     if (diffLevels[i][y][x] < diffLevels[i-1][yl][xl]) {
            	    	    	 minMax[i][y][x] = 0;
            	    	    	 continue;
            	    	     }
            	    	     if ((xl > 0) && (diffLevels[i][y][x] < diffLevels[i-1][yl][xl-1])) {
                	    		 minMax[i][y][x] = 0;
                	    		 continue;
                	    	 }
                	    	 if ((xl < (extentLevels[i-1][0]-1)) && (diffLevels[i][y][x] < diffLevels[i-1][yl][xl+1])) {
                	    		 minMax[i][y][x] = 0;
                	    		 continue;
                	    	 }
                	    	 if ((yl > 0) && (diffLevels[i][y][x] < diffLevels[i-1][yl-1][xl])) {
                	    		 minMax[i][y][x] = 0;
                	    		 continue;
                	    	 }
                	    	 if ((yl < (extentLevels[i-1][1]-1)) && (diffLevels[i][y][x] < diffLevels[i-1][yl+1][xl])) {
                	    		 minMax[i][y][x] = 0;
                	    		 continue;
                	    	 }
                	    	 if ((xl > 0) && (yl > 0) && (diffLevels[i][y][x] < diffLevels[i-1][yl-1][xl-1])) {
                	    		 minMax[i][y][x] = 0;
                	    		 continue;
                	    	 }
                	    	 if ((xl < (extentLevels[i-1][0]-1)) && (yl > 0) && (diffLevels[i][y][x] < diffLevels[i-1][yl-1][xl+1])) {
                	    		 minMax[i][y][x] = 0;
                	    		 continue;
                	    	 }
                	    	 if ((xl > 0) && (yl < (extentLevels[i-1][1]-1)) && (diffLevels[i][y][x] < diffLevels[i-1][yl+1][xl-1])) {
                	    		 minMax[i][y][x] = 0;
                	    		 continue;
                	    	 }
                	    	 if ((xl < (extentLevels[i-1][0]-1)) && (yl < (extentLevels[i-1][1]-1)) &&
                	    			 (diffLevels[i][y][x] < diffLevels[i-1][yl+1][xl+1])) {
                	    		 minMax[i][y][x] = 0;
                	    		 continue;
                	    	 }
            	    	 } // if (i > 0)
            	    	 if (i < (pyramidLevels-1)) {
            	    		 xh = (int)Math.round(x/1.5);
            	    	     yh = (int)Math.round(y/1.5);
            	    	     if (diffLevels[i][y][x] < diffLevels[i+1][yh][xh]) {
            	    	    	 minMax[i][y][x] = 0;
            	    	    	 continue;
            	    	     }
            	    	     if ((xh > 0) && (diffLevels[i][y][x] < diffLevels[i+1][yh][xh-1])) {
                	    		 minMax[i][y][x] = 0;
                	    		 continue;
                	    	 }
                	    	 if ((xh < (extentLevels[i+1][0]-1)) && (diffLevels[i][y][x] < diffLevels[i+1][yh][xh+1])) {
                	    		 minMax[i][y][x] = 0;
                	    		 continue;
                	    	 }
                	    	 if ((yh > 0) && (diffLevels[i][y][x] < diffLevels[i+1][yh-1][xh])) {
                	    		 minMax[i][y][x] = 0;
                	    		 continue;
                	    	 }
                	    	 if ((yh < (extentLevels[i+1][1]-1)) && (diffLevels[i][y][x] < diffLevels[i+1][yh+1][xh])) {
                	    		 minMax[i][y][x] = 0;
                	    		 continue;
                	    	 }
                	    	 if ((xh > 0) && (yh > 0) && (diffLevels[i][y][x] < diffLevels[i+1][yh-1][xh-1])) {
                	    		 minMax[i][y][x] = 0;
                	    		 continue;
                	    	 }
                	    	 if ((xh < (extentLevels[i+1][0]-1)) && (yh > 0) && (diffLevels[i][y][x] < diffLevels[i+1][yh-1][xh+1])) {
                	    		 minMax[i][y][x] = 0;
                	    		 continue;
                	    	 }
                	    	 if ((xh > 0) && (yh < (extentLevels[i+1][1]-1)) && (diffLevels[i][y][x] < diffLevels[i+1][yh+1][xh-1])) {
                	    		 minMax[i][y][x] = 0;
                	    		 continue;
                	    	 }
                	    	 if ((xh < (extentLevels[i+1][0]-1)) && (yh < (extentLevels[i+1][1]-1)) &&
                	    			 (diffLevels[i][y][x] < diffLevels[i+1][yh+1][xh+1])) {
                	    		 minMax[i][y][x] = 0;
                	    		 continue;
                	    	 }	 
            	    	 } // if (i < (pyramidLevels-1))
        	    	 } // if (minMax[i][y][x] != -1)
        	     } // for (x = 0; x < extentLevels[i][0]; x++)
        	} // for (y = 0; y < extentLevels[i][1]; y++)
        	
        	for (y = 0; y < extentLevels[i][1]; y++) {
       	        for (x = 0; x < extentLevels[i][0]; x++) {
       	    	    index = x + y * extentLevels[i][0];
       	    	    sum = 0.0;
       	    	    weightSum = 0.0;
       	    	    for (ym = -halfWin; (ym <= halfWin) && ((y + ym) >= 0) && ((y + ym) < extentLevels[i][1]); ym++) {
       	    	    	for (xm = -halfWin; (xm <= halfWin) && ((x + xm) >= 0) && ((x + xm) < extentLevels[i][0]); xm++) {
       	    	    		index = (x + xm) + extentLevels[i][0] * (y + ym);
       	    	    		weight = Math.min(thresholdGrad, magLevels[i][index]) * GOrData[(xm + halfWin) + xkDim * (ym + halfWin)];
       	    	    		sum += weight * orLevels[i][index];
       	    	    		weightSum += weight;
       	    	    	} // for (xm = -halfWin; (xm <= halfWin) && ((x + xm) >= 0) && ((x + xm) < extentLevels[i][0]); xm++) 
       	    	    } // for (ym = -halfWin; (ym <= halfWin) && ((y + ym) >= 0) && ((y + ym) < extentLevels[i][1]); ym++)
       	    	    orientation = sum / weightSum;
       	    	    // orientation goes from -PI to PI
       	    	    binNum = (int)((orientation + Math.PI)/(Math.PI/18.0));
       	    	    if (binNum == 36) {
       	    	    	binNum = 35;
       	    	    }
       	    	    histoBins[binNum]++;
       	        } // for (x = 0; x < extentLevels[i][0]; x++)
        	} // for (y = 0; y < extentLevels[i][1]; y++)
        } // for (i = 0; i < pyramidLevels; i++)
        
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