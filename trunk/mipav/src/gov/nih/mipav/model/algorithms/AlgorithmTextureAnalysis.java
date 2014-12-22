package gov.nih.mipav.model.algorithms;


import gov.nih.mipav.model.algorithms.filters.FFTUtility;
import gov.nih.mipav.model.algorithms.utilities.AlgorithmChangeType;
import gov.nih.mipav.model.algorithms.utilities.AlgorithmRGBtoGray;
import gov.nih.mipav.model.structures.ModelImage;
import gov.nih.mipav.model.structures.ModelStorageBase;
import gov.nih.mipav.model.structures.TransMatrix;
import gov.nih.mipav.view.MipavUtil;
import gov.nih.mipav.view.ViewJFrameImage;

import java.io.IOException;

import WildMagic.LibFoundation.Mathematics.Vector3f;


/**
 * 
 Overview
 * 
 * This code contains the texture analysis functions for the paper `Texture Analysis and Segmentation Using Modulation
 * Features, Generative Models, and Weighted Curve Evolution', by I. Kokkinos, G. Evangelopoulos and P. Maragos,
 * appearing in IEEE Transactions on Pattern Analysis and Machine Intelligence, Volume 31, Issue 1, Jan. 2009
 * Page(s):142 - 157.
 * 
 * This toolbox was originally developed in and written for MATLAB, with emphasis on efficient algorithm implementations
 * for multiband image filtering, demodulation in amplitude (AM) and frequency (FM) signals via the regularized 2D
 * discrete energy separation algorithm and probabilistic localization of texture, edge, smooth image regions.
 * 
 * Author of the original MATLAB toolbox is Iasonas Kokkinos, currently Assistant Professor at Ecole Centrale Paris,
 * with partial contributions from Georgios Evangelopoulos. The original MATLAB toolbox has been ported from MATLAB to
 * Java by William Gandler. The original MATLAB toolbox can be found at http://cvsp.cs.ntua.gr/software/texture/.
 * Permission to port the original code was generously granted by Iasonas Kokkinos.
 * 
 * The provided functions include:
 * 
 * multi-scale & orientation filterbanks for gabors and edges projection on the basis elements of the underlying
 * generative models demodulation with regularized/complex esa channel selection based on the amplitude/teager/mdl
 * criterion texture/edge/smooth classification based on mdl criterion
 * 
 * Parameters are primarily related to the filterbank construction and the final classification stage.
 * 
 * 
 * References:
 * 
 * 1.) I. Kokkinos, G. Evangelopoulos and P. Maragos, Texture Analysis and Segmentation using Modulation Features,
 * Generative Models and Weighted Curve Evolution, IEEE Transactions on Pattern Analysis & Machine Intelligence, vol.
 * 31, no. 1, pp. 142-157, Jan. 2009.
 * 
 * 2.) G. Evangelopoulos, I. Kokkinos and P. Maragos, Advances in Variational Image Segmentation using AM-FM Models:
 * Regularized Demodulation and Probabilistic Cue Integration, Proc. Int' l Workshop on Variational and Level Set
 * Methods (VLSM-05), Beijing, China, Oct. 2005, Springer LNCS, vol. 3275, pp. 121-136.
 * 
 * 3.) I. Kokkinos, G. Evangelopoulos and P. Maragos, Advances in Texture Analysis: Energy Dominant Component and
 * Multiple Hypothesis Testing, Proc. IEEE Int' l Conf. on Image Processing (ICIP-04), Singapore, Oct. 2004, vol. 3, pp.
 * 1509-1512.
 * 
 * 4.) I. Kokkinos, G. Evangelopoulos and P. Maragos, Modulation-Feature Based Textured Image Segmentation Using Curve
 * Evolution, Proc. IEEE Int' l Conf. on Image Processing (ICIP-04), Singapore, Oct. 2004, vol. 2, pp. 1204-1207.
 * 
 */

public class AlgorithmTextureAnalysis extends AlgorithmBase {

    private final ModelImage[] srcImage;

    private final boolean[] scaleImage;

    // epsilon = D1MACH(4)
    // Machine epsilon is the smallest positive epsilon such that
    // (1.0 + epsilon) != 1.0.
    // epsilon = 2**(1 - doubleDigits) = 2**(1 - 53) = 2**(-52)
    // epsilon = 2.2204460e-16
    // epsilon is called the largest relative spacing
    private final double epsilon = Math.pow(2.0, -52);
    
    private boolean displayFilters = false;
    
    ModelImage texttd2Image;
    
    ModelImage texttd3Image;
    
    ModelImage edgetd2Image;
    
    ModelImage edgetd3Image;
    
    ModelImage gImage;

    // ~ Constructors
    // ---------------------------------------------------------------------------------------------------
    public AlgorithmTextureAnalysis(final ModelImage[] srcImage, final boolean scaleImage[]) {
        // super(null, srcImg);
        this.srcImage = srcImage;
        this.scaleImage = scaleImage;
    }

    /**
     * Starts the program.
     */
    @Override
    public void runAlgorithm() {
        ModelImage destImage = null;
        AlgorithmChangeType changeTypeAlgo;
        final boolean image25D = true;
        AlgorithmTransform algoTrans;
        AlgorithmRGBtoGray gAlgo;
        ModelImage grayImage = null;
        ModelImage inputImage = null;
        int i;
        int k;
        int x;
        int y;
        boolean setupFilters = true;
        final int lastXDim = srcImage[0].getExtents()[0];
        final int lastYDim = srcImage[0].getExtents()[1];
        int inputXDim;
        int inputYDim;
        final int ndirs = 10;
        final int nscales = 4;
        // Proportional to number of oscillations within Gaussian
        final double sig2omega = 1.0;
        int minSize;
        double largestPeriod;
        // Range of radians per pixel for sinusoid of Gabor filter
        final double radianStart = 0.7; // Highest frequency
        double radianEnd; // Lowest frequency
        double textinvDes[][][][][] = new double[ndirs*nscales][][][][];
        double texttd1[][][] = new double[ndirs*nscales][][];
        double texttd2[][][] = new double[ndirs*nscales][][];
        double texttd3[][][] = new double[ndirs*nscales][][];
        double texttd22[][][] = new double[ndirs*nscales][][];
        double texttd23[][][] = new double[ndirs*nscales][][];
        double texttd33[][][] = new double[ndirs*nscales][][];
        double textfd1[][][] = new double[ndirs*nscales][][];
        double textfd1Imag[][][] = new double[ndirs*nscales][][];
        double textfd2[][][] = new double[ndirs*nscales][][];
        double textfd2Imag[][][] = new double[ndirs*nscales][][];
        double textfd3[][][] = new double[ndirs*nscales][][];
        double textfd3Imag[][][] = new double[ndirs*nscales][][];
        double textfd22[][][] = new double[ndirs*nscales][][];
        double textfd22Imag[][][] = new double[ndirs*nscales][][];
        double textfd23[][][] = new double[ndirs*nscales][][];
        double textfd23Imag[][][] = new double[ndirs*nscales][][];
        double textfd33[][][] = new double[ndirs*nscales][][];
        double textfd33Imag[][][] = new double[ndirs*nscales][][];
        double textsigmas[] = new double[ndirs*nscales];
        int textps[] = new int[ndirs*nscales];
        String textdomain = null;
        double edgeinvDes[][][][][] = new double[ndirs*nscales][][][][];
        double edgetd1[][][] = new double[ndirs*nscales][][];
        double edgetd2[][][] = new double[ndirs*nscales][][];
        double edgetd3[][][] = new double[ndirs*nscales][][];
        double edgetd22[][][] = new double[ndirs*nscales][][];
        double edgetd23[][][] = new double[ndirs*nscales][][];
        double edgetd33[][][] = new double[ndirs*nscales][][];
        double edgefd1[][][] = new double[ndirs*nscales][][];
        double edgefd1Imag[][][] = new double[ndirs*nscales][][];
        double edgefd2[][][] = new double[ndirs*nscales][][];
        double edgefd2Imag[][][] = new double[ndirs*nscales][][];
        double edgefd3[][][] = new double[ndirs*nscales][][];
        double edgefd3Imag[][][] = new double[ndirs*nscales][][];
        double edgefd22[][][] = new double[ndirs*nscales][][];
        double edgefd22Imag[][][] = new double[ndirs*nscales][][];
        double edgefd23[][][] = new double[ndirs*nscales][][];
        double edgefd23Imag[][][] = new double[ndirs*nscales][][];
        double edgefd33[][][] = new double[ndirs*nscales][][];
        double edgefd33Imag[][][] = new double[ndirs*nscales][][];
        double edgesigmas[] = new double[ndirs*nscales];
        int edgeps[] = new int[ndirs*nscales];
        String edgedomain = null;
        double sgx[] = new double[ndirs * nscales];
        double invVariance2;
        double factorSharpness;
        double factorMdl;
        int scaleswt[] = new int[nscales];
        String DCAmethod = null;
        String esameth = null;
        for (i = 0; i < srcImage.length; i++) {
            inputXDim = srcImage[i].getExtents()[0];
            inputYDim = srcImage[i].getExtents()[1];
            if (srcImage[i].isColorImage()) {
                final boolean thresholdAverage = false;
                final float threshold = 0.0f;
                final boolean intensityAverage = false;
                final boolean equalRange = true;
                final float minR = 0.0f;
                final float minG = 0.0f;
                final float minB = 0.0f;
                float redValue;
                float greenValue;
                float blueValue;
                float maxR;
                float maxG;
                float maxB;
                if (srcImage[i].getMinR() == srcImage[i].getMaxR()) {
                    redValue = 0.0f;
                    greenValue = 0.5f;
                    blueValue = 0.5f;
                } else if (srcImage[i].getMinG() == srcImage[i].getMaxG()) {
                    redValue = 0.5f;
                    greenValue = 0.0f;
                    blueValue = 0.5f;
                } else if (srcImage[i].getMinB() == srcImage[i].getMaxB()) {
                    redValue = 0.5f;
                    greenValue = 0.5f;
                    blueValue = 0.0f;
                } else {
                    redValue = (float) (1.0 / 3.0);
                    greenValue = redValue;
                    blueValue = redValue;

                }
                maxR = (float) srcImage[i].getMaxR();
                maxG = (float) srcImage[i].getMaxG();
                maxB = (float) srcImage[i].getMaxB();
                grayImage = new ModelImage(ModelStorageBase.FLOAT, srcImage[i].getExtents(), "grayImage");
                gAlgo = new AlgorithmRGBtoGray(grayImage, srcImage[i], redValue, greenValue, blueValue, thresholdAverage, threshold, intensityAverage,
                        equalRange, minR, maxR, minG, maxG, minB, maxB);
                gAlgo.run();
                gAlgo.finalize();
            } // if (srcImage.isColorImage())
            if (scaleImage[i]) {
                destImage = new ModelImage(ModelStorageBase.DOUBLE, srcImage[i].getExtents(), "changeTypeImage");
                if (srcImage[i].isColorImage()) {
                    changeTypeAlgo = new AlgorithmChangeType(destImage, grayImage, grayImage.getMin(), grayImage.getMax(), 0.0, 1.0, image25D);
                    grayImage.disposeLocal();
                    grayImage = null;
                } else {
                    changeTypeAlgo = new AlgorithmChangeType(destImage, srcImage[i], srcImage[i].getMin(), srcImage[i].getMax(), 0.0, 1.0, image25D);
                }
                changeTypeAlgo.run();
                changeTypeAlgo.finalize();
                changeTypeAlgo = null;

                final boolean doPad = false;
                final TransMatrix xfrm = new TransMatrix(3);
                xfrm.identity();
                final int interp = AlgorithmTransform.BILINEAR;
                final int oXdim = 219;
                final int oYdim = 146;
                inputXDim = oXdim;
                inputYDim = oYdim;
                final float oXres = srcImage[i].getFileInfo()[0].getResolutions()[0] * srcImage[i].getExtents()[0] / oXdim;
                final float oYres = srcImage[i].getFileInfo()[0].getResolutions()[1] * srcImage[i].getExtents()[1] / oYdim;
                final int units[] = srcImage[i].getUnitsOfMeasure();
                final boolean doClip = true;
                final boolean doVOI = false;
                final boolean doRotateCenter = false;
                final Vector3f center = new Vector3f();
                final float fillValue = 0.0f;
                final boolean doUpdateOrigin = false;
                final boolean isSATransform = false;
                algoTrans = new AlgorithmTransform(destImage, xfrm, interp, oXres, oYres, oXdim, oYdim, units, doVOI,
                		doClip, doPad, doRotateCenter, center);
                algoTrans.setFillValue(fillValue);
                algoTrans.setUpdateOriginFlag(doUpdateOrigin);
                algoTrans.setUseScannerAnatomical(isSATransform);
                algoTrans.setSuppressProgressBar(true);

                algoTrans.run();
                destImage.disposeLocal();

                destImage = algoTrans.getTransformedImage();
                algoTrans.disposeLocal();
                algoTrans = null;
                destImage.calcMinMax();
            } // if (scaleImage[i])

            // Construct filterbank once, off-line
            // If image dimensions change for different images
            // you will need to reconstruct (and wait)

            if ( (i == 0) || (inputXDim != lastXDim) || (inputYDim != lastYDim)) {
                setupFilters = true;
            } else {
                setupFilters = false;
            }

            if (setupFilters) {
                minSize = Math.min(inputXDim, inputYDim);
                largestPeriod = minSize / 4.0;
                radianEnd = 2.0 * Math.PI / largestPeriod;

                // Construct filters and their time or frequency response
                // For efficiency use the time domain for small
                // filters and the frequency domain for large ones
                T1_responses(texttd1, texttd2, texttd3, texttd22, texttd23, texttd33,
                		textfd1, textfd1Imag, textfd2, textfd2Imag, textfd3, textfd3Imag, 
                		textfd22, textfd22Imag, textfd23, textfd23Imag, textfd33, textfd33Imag, textsigmas, textps,
                		nscales, ndirs, sig2omega, radianStart, radianEnd, inputXDim, inputYDim, "texture", textdomain);
                T1_responses(edgetd1, edgetd2, edgetd3, edgetd22, edgetd23, edgetd33,
                		edgefd1, edgefd1Imag, edgefd2, edgefd2Imag, edgefd3, edgefd3Imag, 
                		edgefd22, edgefd22Imag, edgefd23, edgefd23Imag, edgefd33, edgefd33Imag, edgesigmas, edgeps,
                		nscales, ndirs, sig2omega, radianStart, radianEnd, inputXDim, inputYDim, "edge", edgedomain);
                
                if (destImage != null) {
                	inputImage = destImage;
                }
                else if (grayImage != null) {
                	inputImage = grayImage;
                }
                else {
                	inputImage = srcImage[i];
                }
                
                // Terms computed off-line for weighted projection on basis
                
                // These account for boundary conditions & for a 
                // non-zero mean value of the even filter
                T2z0_projection_terms(textinvDes, texttd1, texttd2, texttd3, texttd22, texttd23, texttd33,
                		textfd1, textfd1Imag, textfd2, textfd2Imag, textfd3, textfd3Imag, 
                		textfd22, textfd22Imag, textfd23, textfd23Imag, textfd33, textfd33Imag,
                		textps, nscales, ndirs, textdomain, inputImage);
                
                T2z0_projection_terms(edgeinvDes, edgetd1, edgetd2, edgetd3, edgetd22, edgetd23, edgetd33,
                		edgefd1, edgefd1Imag, edgefd2, edgefd2Imag, edgefd3, edgefd3Imag, 
                		edgefd22, edgefd22Imag, edgefd23, edgefd23Imag, edgefd33, edgefd33Imag,
                		edgeps, nscales, ndirs, edgedomain, inputImage);
                
                if (displayFilters) {
                    double texttimetd1[][][] = new double[ndirs*nscales][][];
                    double texttimetd2[][][] = new double[ndirs*nscales][][];
                    double texttimetd3[][][] = new double[ndirs*nscales][][];
                    double texttimetd22[][][] = new double[ndirs*nscales][][];
                    double texttimetd23[][][] = new double[ndirs*nscales][][];
                    double texttimetd33[][][] = new double[ndirs*nscales][][];
                    double texttimefd1[][][] = new double[ndirs*nscales][][];
                    double texttimefd1Imag[][][] = new double[ndirs*nscales][][];
                    double texttimefd2[][][] = new double[ndirs*nscales][][];
                    double texttimefd2Imag[][][] = new double[ndirs*nscales][][];
                    double texttimefd3[][][] = new double[ndirs*nscales][][];
                    double texttimefd3Imag[][][] = new double[ndirs*nscales][][];
                    double texttimefd22[][][] = new double[ndirs*nscales][][];
                    double texttimefd22Imag[][][] = new double[ndirs*nscales][][];
                    double texttimefd23[][][] = new double[ndirs*nscales][][];
                    double texttimefd23Imag[][][] = new double[ndirs*nscales][][];
                    double texttimefd33[][][] = new double[ndirs*nscales][][];
                    double texttimefd33Imag[][][] = new double[ndirs*nscales][][];
                    double texttimesigmas[] = new double[ndirs*nscales];
                    int texttimeps[] = new int[ndirs*nscales];
                    String texttimedomain = "time";
                    double edgetimetd1[][][] = new double[ndirs*nscales][][];
                    double edgetimetd2[][][] = new double[ndirs*nscales][][];
                    double edgetimetd3[][][] = new double[ndirs*nscales][][];
                    double edgetimetd22[][][] = new double[ndirs*nscales][][];
                    double edgetimetd23[][][] = new double[ndirs*nscales][][];
                    double edgetimetd33[][][] = new double[ndirs*nscales][][];
                    double edgetimefd1[][][] = new double[ndirs*nscales][][];
                    double edgetimefd1Imag[][][] = new double[ndirs*nscales][][];
                    double edgetimefd2[][][] = new double[ndirs*nscales][][];
                    double edgetimefd2Imag[][][] = new double[ndirs*nscales][][];
                    double edgetimefd3[][][] = new double[ndirs*nscales][][];
                    double edgetimefd3Imag[][][] = new double[ndirs*nscales][][];
                    double edgetimefd22[][][] = new double[ndirs*nscales][][];
                    double edgetimefd22Imag[][][] = new double[ndirs*nscales][][];
                    double edgetimefd23[][][] = new double[ndirs*nscales][][];
                    double edgetimefd23Imag[][][] = new double[ndirs*nscales][][];
                    double edgetimefd33[][][] = new double[ndirs*nscales][][];
                    double edgetimefd33Imag[][][] = new double[ndirs*nscales][][];
                    double edgetimesigmas[] = new double[ndirs*nscales];
                    int edgetimeps[] = new int[ndirs*nscales];
                    String edgetimedomain = "time";
                    double dhout[][] = null;
                    double dhoutImag[][] = null;
                    double dw1[] = null;
                    double dw2[] = null;
                    double mx[] = null;
                    double mag;
                    
                    T1_responses(texttimetd1, texttimetd2, texttimetd3, texttimetd22, texttimetd23, texttimetd33,
                    		texttimefd1, texttimefd1Imag, texttimefd2, texttimefd2Imag, texttimefd3, texttimefd3Imag, 
                    		texttimefd22, texttimefd22Imag, texttimefd23, texttimefd23Imag, texttimefd33, texttimefd33Imag, 
                    		texttimesigmas, texttimeps,
                    		nscales, ndirs, sig2omega, radianStart, radianEnd, inputXDim, inputYDim, "texture", texttimedomain);
                    T1_responses(edgetimetd1, edgetimetd2, edgetimetd3, edgetimetd22, edgetimetd23, edgetimetd33,
                    		edgetimefd1, edgetimefd1Imag, edgetimefd2, edgetimefd2Imag, edgetimefd3, edgetimefd3Imag, 
                    		edgetimefd22, edgetimefd22Imag, edgetimefd23, edgetimefd23Imag, edgetimefd33, edgetimefd33Imag, 
                    		edgetimesigmas, edgetimeps,
                    		nscales, ndirs, sig2omega, radianStart, radianEnd, inputXDim, inputYDim, "edge", edgetimedomain);
                    // Max of gabor filter response, frequency domain
                    mx = new double[200 * 200];
                    for (k = 0; k < 40 ; k++) {
                        freqz2(dhout, dhoutImag, dw1, dw2, texttimetd2[k], 200, 200);
                        for (y = 0; y < 200; y++) {
                        	for (x = 0; x < 200; x++) {
                        		mag = Math.sqrt(dhout[y][x]*dhout[y][x] + dhoutImag[y][x]*dhoutImag[y][x]);
                        		if (mag > mx[x + 200 * y]) {
                        			mx[x + 200 * y] = mag;
                        		}
                        	}
                        }
                    }
                    int dExtents[] = new int[2];
                    dExtents[0] = 200;
                    dExtents[1] = 200;
                    gImage = new ModelImage(ModelStorageBase.DOUBLE, dExtents, "max_gabor_response");
                    try {
                    	gImage.importData(0, mx, true);
                    }
                    catch (IOException e) {
                    	MipavUtil.displayError("IOException " + e + " on gImage.importData(0, mx, true)");
                    	setCompleted(false);
                    	return;
                    }
                    new ViewJFrameImage(gImage);
                    // Show texture and edge filters at a single scale and orientation
                    int filId = 10;
                    int texttimeLength = texttimetd2[0].length * texttimetd2[0][0].length;
                    double buffer[] = new double[texttimeLength];
       
                    for (y = 0; y < texttimetd2[0].length; y++) {
                    	for (x = 0; x < texttimetd2[0][0].length; x++) {
                    		buffer[x + y * texttimetd2[0][0].length] = texttimetd2[filId][y][x];
                    	}
                    }
                    
                    dExtents[0] = texttimetd2[0][0].length;
                    dExtents[1] = texttimetd2[0].length;
                    texttd2Image = new ModelImage(ModelStorageBase.DOUBLE, dExtents, "text_td2_Image");
                    try {
                    	texttd2Image.importData(0, buffer, true);
                    }
                    catch (IOException e) {
                    	MipavUtil.displayError("IOException " + e + " on texttd2Image.importData(0, buffer, true)");
                    	setCompleted(false);
                    	return;
                    }
                    new ViewJFrameImage(texttd2Image);
                    
                    for (y = 0; y < texttimetd2[0].length; y++) {
                    	for (x = 0; x < texttimetd2[0][0].length; x++) {
                    		buffer[x + y * texttimetd2[0][0].length] = texttimetd3[filId][y][x];
                    	}
                    }
                    texttd3Image = new ModelImage(ModelStorageBase.DOUBLE, dExtents, "text_td3_Image");
                    try {
                    	texttd3Image.importData(0, buffer, true);
                    }
                    catch (IOException e) {
                    	MipavUtil.displayError("IOException " + e + " on texttd3Image.importData(0, buffer, true)");
                    	setCompleted(false);
                    	return;
                    }
                    new ViewJFrameImage(texttd3Image);
                    
                    int edgetimeLength = edgetimetd2[0].length * edgetimetd2[0][0].length;
                    buffer = new double[edgetimeLength];
       
                    for (y = 0; y < edgetimetd2[0].length; y++) {
                    	for (x = 0; x < edgetimetd2[0][0].length; x++) {
                    		buffer[x + y * edgetimetd2[0][0].length] = edgetimetd2[filId][y][x];
                    	}
                    }
                    dExtents[0] = edgetimetd2[0][0].length;
                    dExtents[1] = edgetimetd2[0].length;
                    edgetd2Image = new ModelImage(ModelStorageBase.DOUBLE, dExtents, "edge_td2_Image");
                    try {
                    	edgetd2Image.importData(0, buffer, true);
                    }
                    catch (IOException e) {
                    	MipavUtil.displayError("IOException " + e + " on edgetd2Image.importData(0, buffer, true)");
                    	setCompleted(false);
                    	return;
                    }
                    new ViewJFrameImage(edgetd2Image);
                    
                    for (y = 0; y < edgetimetd2[0].length; y++) {
                    	for (x = 0; x < edgetimetd2[0][0].length; x++) {
                    		buffer[x + y * edgetimetd2[0][0].length] = edgetimetd3[filId][y][x];
                    	}
                    }
                    edgetd3Image = new ModelImage(ModelStorageBase.DOUBLE, dExtents, "edge_td3_Image");
                    try {
                    	edgetd3Image.importData(0, buffer, true);
                    }
                    catch (IOException e) {
                    	MipavUtil.displayError("IOException " + e + " on edgetd3Image.importData(0, buffer, true)");
                    	setCompleted(false);
                    	return;
                    }
                    new ViewJFrameImage(edgetd3Image);
                } // if (displayFilters)
            } // if (setupFilters)
            
            // mdl criterion terms (refer to paper)
            // G: gaussian function with maximum at (x == 0) equal to 1
            // sum G = (filter_scales.^2) * [2*PI]
            for (k = 0; k < ndirs*nscales; k++) {
                sgx[k] = (2.0 * Math.PI)*textsigmas[k]*textsigmas[k];
            }
            invVariance2 = 1.0/(2.0 * 0.03 * 0.03);
            factorSharpness = 1.0;
            factorMdl = 1.0;
            
            // Determine scales over which the decision is taken
            for (k = 0; k < nscales; k++) {
            	scaleswt[k] = k;
            }
            
            // For the whole process to run at a single scale modify the code above
            // by an outer for loop
            //for (scaleInd = 0; scaleInd < nscales; scaleInd++) {
                 //scaleswt = scaleInd;
           // }
            
            // Main part:
            // Multi-scale and orientation filtering for texture/edge signals
            
            // Note: Filtering for texture is bundled with demodulation
            // This 'inflates' the filter responses where they
            // are decreased due to a mismatch between 
            // the signal's and the Gabor's central frequencies.
            
            // channel selection criterion
            // 'mdl' : mdl -like criterion (current)
            // 'teag' : teager energy (teager-based DCA)
            // 'ampl' : amplitude
            DCAmethod = "mdl";
            
            // Choose demodulation algorithm
            // 'gesa' : Gabor - ESA
            // 'cesa' : Complex-ESA
            // '' : no demodulation (use Gabor filter's amplitude/frequency)
            
            esameth = "gesa";
            T2z1_filter(texttd1, texttd2, texttd3, textfd1, textfd1Imag, textfd2, textfd2Imag, textfd3, textfd3Imag, 
            		textfd22, textfd22Imag, textfd23, textfd23Imag, textfd33, textfd33Imag,
            		textps, textdomain, scaleswt, ndirs, inputImage);
        } // for (i = 0; i < srcImage.length; i++)

    }
    
    private void T2z1_filter(double td1[][][], double td2[][][], double td3[][][],
    		double fd1[][][], double fd1Imag[][][], double fd2[][][], double fd2Imag[][][],
    		double fd3[][][], double fd3Imag[][][], double fd22[][][], double fd22Imag[][][],
    		double fd23[][][], double fd23Imag[][][], double fd33[][][], double fd33Imag[][][],
    		int ps[], String domain, int scaleswt[], int ndirs, ModelImage inputImage) {
    	// fields that are being accumulated
    	// A: amplitude, ph: phase, idx: filter index
    	// en: model-based decrease in reconstruction error (sum of squares)
    	// Fx, Fy; frequency components
    	double critDCA = -1.0E11;
    	int xDim = inputImage.getExtents()[0];
    	int yDim = inputImage.getExtents()[1];
    	int length = xDim * yDim;
    	double inputImageFFT[] = new double[length];
    	double inputImageFFTImag[] = new double[length];
    	FFTUtility fft;
    	int k;
    	int scaleInd;
    	int filStart;
    	double inputIm[][] = new double[yDim][xDim];
    	int y;
    	int x;
    	double preComputedIc1[][] = null;
    	double preComputedIc2[][] = null;
    	double preComputedIc3[][] = null;
    	double preComputedSc1[][] = null;
    	double preComputedsm[][] = null;
    	double fftImagePatch[] = null;
    	double fftImagePatchImag[] = null;
    	double fftSupportPatch[] = null;
    	double fftSupportPatchImag[] = null;
    	double endc[][] = null;
    	int dirInd;
    	int filInd;
    	try {
    		inputImage.exportData(0, length, inputImageFFT);
    	}
    	catch(IOException e) {
    		MipavUtil.displayError("IOException " + e + " on inputImage.exportData(0, length, inputImageFFT");
    		return;
    	}
    	
    	for (y = 0; y < yDim; y++) {
    		for (x = 0; x < xDim; x++) {
    			inputIm[y][x] = inputImageFFT[x + y * xDim];
    		}
    	}
    	
    	fft = new FFTUtility(inputImageFFT, inputImageFFTImag, yDim, xDim, 1, -1, FFTUtility.FFT);
	    fft.run();
	    fft.finalize();
	    fft = null;
	    fft = new FFTUtility(inputImageFFT, inputImageFFTImag, 1, yDim, xDim, -1, FFTUtility.FFT);
	    fft.run();
	    fft.finalize();
	    fft = null;
	    
	    for (k = 0; k < scaleswt.length; k++) {
	    	scaleInd = scaleswt[k];
	    	filStart = scaleInd * ndirs;
	    	
	    	// Once for every scale, construct dc model for background
	    	if (domain.equals("time")) {
	    		T2z1b_get_responses_time(preComputedIc1, null, null, preComputedSc1, preComputedsm, null, null, null,
	    				null, null, null,
	            		td1[filStart], null, null, 
	            		null, null, null, inputIm, 1);		
	    	} // if (domain.equals("time"))
	    	else if (domain.equals("freq")) {
	    		T2z1a_make_image_structure(fftImagePatch, fftImagePatchImag,
		                   fftSupportPatch, fftSupportPatchImag,
		                   inputIm, ps[filStart]);
	    		 T2z1b_get_responses_freq(preComputedIc1, null, null, preComputedSc1, preComputedsm, null, null, null, null, null, null,
 	            		fftSupportPatch, fftSupportPatchImag, ps[filStart], fftImagePatch, fftImagePatchImag, 
 	            		fd1[filStart], fd1Imag[filStart], fd2[filStart], fd2Imag[filStart],
 	            		fd3[filStart], fd3Imag[filStart], fd22[filStart], fd22Imag[filStart],
 	            		fd23[filStart], fd23Imag[filStart], fd33[filStart], fd33Imag[filStart], 1);
	    	} // else if (domain.equals("freq"))
	    	endc = new double[preComputedIc1.length][preComputedIc1[0].length];
	    	for (y = 0; y < preComputedIc1.length; y++) {
	    		for (x = 0; x < preComputedIc1[0].length; x++) {
	    			endc[y][x] = preComputedIc1[y][x] * preComputedsm[y][x];
	    		}
	    	}
	    	
	    	// And now loop over orientations
	    	for (dirInd = 0; dirInd < ndirs; dirInd++) {
	    	    filInd = filStart + dirInd;
	    	    
	    	    // Construct fields involved in projection
	    	    if (domain.equals("time")) {
	    	        T2z1b_get_responses_time(null, preComputedIc2, preComputedIc3, null, null, null, null, null,
	    				null, null, null,
	            		null, td2[filInd], td3[filInd], 
	            		null, null, null, inputIm, 2);
	    	    }
	    	    else if (domain.equals("freq")) {
	    	        T2z1b_get_responses_freq(null, preComputedIc2, preComputedIc3, null, null, null, null, null, null, null, null,
 	            		null, null, ps[filInd], fftImagePatch, fftImagePatchImag, 
 	            		null, null, fd2[filInd], fd2Imag[filInd],
 	            		fd3[filInd], fd3Imag[filInd], null, null,
 	            		null, null, null, null, 2);
	    	    }
	    	    
	    	    // Estimate projection onto basis elements
	    	} // for (dirInd = 0; dirInd < ndirs; dirInd++)
	    } // for (k = 0; k < scaleswt.length; k++)
    	
    }
    
    private void T2z0_projection_terms(double invDes[][][][][], double td1[][][], double td2[][][], double td3[][][],
    		double td22[][][], double td23[][][], double td33[][][],
    		double fd1[][][], double fd1Imag[][][], double fd2[][][], double fd2Imag[][][],
    		double fd3[][][], double fd3Imag[][][], double fd22[][][], double fd22Imag[][][],
    		double fd23[][][], double fd23Imag[][][], double fd33[][][], double fd33Imag[][][],
    		int ps[], int nscales, int ndirs, String filtDomain, ModelImage inputImage) {
    	// Precompute terms required for weighted projection on basis
    	int sizen0 = inputImage.getExtents()[0];
    	int sizem0 = inputImage.getExtents()[1];
    	double domain[][] = new double[sizem0][sizen0];  
    	double fftImagePatch[] = null;
    	double fftImagePatchImag[] = null;
    	double fftSupportPatch[] = null;
    	double fftSupportPatchImag[] = null;
    	double Sc1[][] = null;
    	double Sc2[][] = null;
    	double Sc3[][] = null;
    	double Sc22[][] = null;
    	double Sc23[][] = null;
    	double Sc33[][] = null;
    	int patchSize;
    	int y;
    	int x;
    	int sc;
    	int offsetsc;
    	int dirInd;
    	int filInd;
    	for (y = 0; y < sizem0; y++) {
    		for (x = 0; x < sizen0; x++) {
    			domain[y][x] = 1.0;
    		}
    	}
    	
    	for (sc = 1; sc <= nscales; sc++) {
    	    offsetsc = (sc-1)*ndirs;
    	    patchSize = ps[offsetsc];
    	    if (filtDomain.equals("freq")) {
    	        T2z1a_make_image_structure(fftImagePatch, fftImagePatchImag,
    	        		                   fftSupportPatch, fftSupportPatchImag,
    	        		                   domain, patchSize);	
    	    }
    	    
    	    for (dirInd = 1; dirInd <= ndirs; dirInd++) {
    	        filInd = offsetsc + dirInd;
    	        if (filtDomain.equals("freq")) {
    	            T2z1b_get_responses_freq(null, null, null, null, null, Sc1, Sc2, Sc3, Sc22, Sc23, Sc33,
    	            		fftSupportPatch, fftSupportPatchImag, patchSize, null, null, 
    	            		fd1[filInd-1], fd1Imag[filInd-1], fd2[filInd-1], fd2Imag[filInd-1],
    	            		fd3[filInd-1], fd3Imag[filInd-1], fd22[filInd-1], fd22Imag[filInd-1],
    	            		fd23[filInd-1], fd23Imag[filInd-1], fd33[filInd-1], fd33Imag[filInd-1], 0);
    	        } // if (filtDomain[offsetsc].equals("freq"))
    	        else if (filtDomain.equals("time")) {
    	            T2z1b_get_responses_time(null, null, null, null, null, Sc1, Sc2, Sc3, Sc22, Sc23, Sc33,
    	            		td1[filInd-1], td2[filInd-1], td3[filInd-1], 
    	            		td22[filInd-1], td23[filInd-1], td33[filInd-1], domain, 0);	
    	        } //  else if (filtDomain[offsetsc].equals("time"))
    	        invDes[filInd-1] = T2z0a_invert_design(Sc1, Sc2, Sc3, Sc22, Sc23, Sc33);
    	    } // for (dirInd = 1; dirInd <= ndirs; dirInd++) 
    	} // for (sc = 1; sc <= nscales; sc++)
    }
    
    private double[][][][] T2z0a_invert_design(double Sc1[][], double Sc2[][], double Sc3[][],
    		double Sc22[][], double Sc23[][], double Sc33[][]) {
    	int y;
    	int x;
    	double invDes[][][][] = new double[3][3][Sc1.length][Sc1[0].length];
    	double convqpinb[][][][] = new double[3][3][Sc1.length][Sc1[0].length];
    	double det[][] = new double[Sc1.length][Sc1[0].length];
    	double invDet[][] = new double[Sc1.length][Sc1[0].length];
    	int k1;
    	int k2;
    	// dc * dc
    	for (y = 0; y < Sc1.length; y++) {
    		for (x = 0; x < Sc1[0].length; x++) {
    			convqpinb[0][0][y][x] = Sc1[y][x];
    		}
    	}
        // sin * dc / cos * dc
    	for (y = 0; y < Sc1.length; y++) {
    		for (x = 0; x < Sc1[0].length; x++) {
    			convqpinb[0][1][y][x] = Sc2[y][x];
    			convqpinb[1][0][y][x] = Sc2[y][x];
    			convqpinb[0][2][y][x] = Sc3[y][x];
    			convqpinb[2][0][y][x] = Sc3[y][x];
    		}
    	}
    	
    	// cos * cos / sin * cos / sin * sin
    	for (y = 0; y < Sc1.length; y++) {
    		for (x = 0; x < Sc1[0].length; x++) {
    			convqpinb[1][1][y][x] = Sc22[y][x];
    			convqpinb[1][2][y][x] = Sc23[y][x];
    			convqpinb[2][1][y][x] = Sc23[y][x];
    			convqpinb[2][2][y][x] = Sc33[y][x];
    		}
    	}
    	
    	for (y = 0; y < Sc1.length; y++) {
    		for (x = 0; x < Sc1[0].length; x++) {
    			det[y][x] = convqpinb[0][0][y][x] * convqpinb[1][1][y][x] * convqpinb[2][2][y][x] -
    					    convqpinb[0][0][y][x] * convqpinb[1][2][y][x] * convqpinb[2][1][y][x] -
    					    convqpinb[1][0][y][x] * convqpinb[0][1][y][x] * convqpinb[2][2][y][x] +
    					    convqpinb[1][0][y][x] * convqpinb[0][2][y][x] * convqpinb[2][1][y][x] +
    					    convqpinb[2][0][y][x] * convqpinb[0][1][y][x] * convqpinb[1][2][y][x] -
    					    convqpinb[2][0][y][x] * convqpinb[0][2][y][x] * convqpinb[1][1][y][x];
    		}
    	}
    	
    	for (y = 0; y < Sc1.length; y++) {
    		for (x = 0; x < Sc1[0].length; x++) {
    		    invDes[0][0][y][x] = (convqpinb[1][1][y][x] * convqpinb[2][2][y][x] - convqpinb[1][2][y][x] * convqpinb[2][1][y][x]);
    		    invDes[0][1][y][x] = -(convqpinb[0][1][y][x] * convqpinb[2][2][y][x] - convqpinb[0][2][y][x] * convqpinb[2][1][y][x]);
    		    invDes[0][2][y][x] = (convqpinb[0][1][y][x] * convqpinb[1][2][y][x] - convqpinb[0][2][y][x] * convqpinb[1][1][y][x]);
    		    invDes[1][0][y][x] = -(convqpinb[1][0][y][x] * convqpinb[2][2][y][x] - convqpinb[1][2][y][x] * convqpinb[2][0][y][x]);
    		    invDes[1][1][y][x] = (convqpinb[0][0][y][x] * convqpinb[2][2][y][x] - convqpinb[0][2][y][x] * convqpinb[2][0][y][x]);
    		    invDes[1][2][y][x] = -(convqpinb[0][0][y][x] * convqpinb[1][2][y][x] - convqpinb[0][2][y][x] * convqpinb[1][0][y][x]);
    		    invDes[2][0][y][x] = (convqpinb[1][0][y][x] * convqpinb[2][1][y][x] - convqpinb[1][1][y][x] * convqpinb[2][0][y][x]);
    		    invDes[2][1][y][x] = -(convqpinb[0][0][y][x] * convqpinb[2][1][y][x] - convqpinb[0][1][y][x] * convqpinb[2][0][y][x]);
    		    invDes[2][2][y][x] = (convqpinb[0][0][y][x] * convqpinb[1][1][y][x] - convqpinb[0][1][y][x] * convqpinb[1][0][y][x]);
    		    if (det[y][x] >= 0.0) {
    		    	invDet[y][x] = 1.0/Math.max(det[y][x], 1.0E-8);
    		    }
    		    else {
    		    	invDet[y][x] = -1.0/Math.max(Math.abs(det[y][x]), 1.0E-8);
    		    }
    		}
    	}
    	
    	for (k1 = 0; k1 < 3; k1++) {
    		for (k2 = 0; k2 < 3; k2++) {
	    	    for (y = 0; y < Sc1.length; y++) {
	    		    for (x = 0; x < Sc1[0].length; x++) {
	    			    invDes[1][k2][y][x] = invDes[k1][k2][y][x] * invDet[y][x];	    		    }
	    		}
	    	}
    	}
    	
    	return invDes;
    }
    
    private void T2z1b_get_responses_time(double preComputedIc1[][], double preComputedIc2[][],
    		double preComputedIc3[][], double preComputedSc1[][], double preComputedsm[][],
    		double Sc1[][], double Sc2[][], double Sc3[][],
    		double Sc22[][], double Sc23[][], double Sc33[][],double td1[][], double td2[][], double td3[][],
    		double td22[][], double td23[][], double td33[][], double inputIm[][] , int findNorm) {
    	double onesm[][];
    	int y;
    	int x;
    	switch(findNorm) {
    	case 0:
    		Sc1 = filter2(td1, inputIm);
    		Sc2 = filter2(td2, inputIm);
    		Sc3 = filter2(td3, inputIm);
    		Sc22 = filter2(td22, inputIm);
    		Sc23 = filter2(td23, inputIm);
    		Sc33 = filter2(td33, inputIm);
    		break;
    	case 1:
    		preComputedIc1 = filter2(td1, inputIm);
    		onesm = new double[inputIm.length][inputIm[0].length];
    		for (y = 0; y < inputIm.length; y++) {
    			for (x = 0; x < inputIm[0].length; x++) {
    				onesm[y][x] = 1.0;
    			}
    		}
    		preComputedSc1 = filter2(td1, onesm);
    		preComputedsm = new double[preComputedIc1.length][preComputedIc1[0].length];
    		for (y = 0; y < preComputedIc1.length; y++) {
    			for (x = 0; x < preComputedIc1[0].length; x++) {
    				preComputedsm[y][x] = preComputedIc1[y][x]/preComputedSc1[y][x];
    			}
    		}
    		break;
    	case 2:
    		preComputedIc2 = filter2(td2, inputIm);
    		preComputedIc3 = filter2(td3, inputIm);
    	}
    	
    }
    
    private double[][] filter2(double A[][], double B[][]) {
    	double C[][];
    	rot180(A);
    	C = conv2(B, A);
    	return C;
    }
    
    private double[][] conv2(double A[][], double B[][]) {
    	double L[][];
    	double S[][];
    	int ml;
    	int nl;
    	int ms;
    	int ns;
    	int y;
    	int x;
    	int y2;
    	int x2;
    	double C[][];
    	double Cout[][];
    	double large;
    	int yoff;
    	int xoff;
        if (A.length * A[0].length >= B.length * B[0].length) {
        	ml = A.length;
        	nl = A[0].length;
        	L = A;
        	ms = B.length;
        	ns = B[0].length;
        	S = B;
        }
        else {
        	ml = B.length;
        	nl = B[0].length;
        	L = B;
        	ms = A.length;
        	ns = A[0].length;
        	S = A;
        }
        C = new double[ml+ms-1][nl+ns-1];
        for (y = 0; y < ml; y++) {
        	for (x = 0; x < nl; x++) {
        		large = L[y][x];
        		for (y2 = 0; y2 < ms; y2++) {
        			for (x2 = 0; x2 < ns; x2++) {
        				C[y+y2][x+x2] += S[y2][x2]*large;
        			}
        		}
        	}
        }
        Cout = new double[A.length][A[0].length];
        yoff = (int)Math.floor(B.length/2.0);
        xoff = (int)Math.floor(B[0].length/2.0);
        for (y = 0; y < A.length; y++) {
        	for (x = 0; x < A[0].length; x++) {
        		Cout[y][x] = C[y + yoff][x + xoff];
        	}
        }
        return Cout;
    }
    
    private double[] conv(double A[], double B[]) {
    	double L[];
    	double S[];
    	int ml;
    	int ms;
    	int y;
    	int y2;
    	double C[];
    	double Cout[];
    	double large;
    	int yoff;
        if (A.length >= B.length) {
        	ml = A.length;
        	L = A;
        	ms = B.length;
        	S = B;
        }
        else {
        	ml = B.length;
        	L = B;
        	ms = A.length;
        	S = A;
        }
        C = new double[ml+ms-1];
        for (y = 0; y < ml; y++) {
    		large = L[y];
    		for (y2 = 0; y2 < ms; y2++) {
    	        C[y+y2] += S[y2]*large;
    		}
        }
        Cout = new double[A.length];
        yoff = (int)Math.floor(B.length/2.0);
        for (y = 0; y < A.length; y++) {
        	Cout[y] = C[y + yoff];
        }
        return Cout;
    }
    
    private void T2z1b_get_responses_freq(double preComputedIc1[][], double preComputedIc2[][],
    		double preComputedIc3[][], double preComputedSc1[][], double preComputedsm[][],
    		double Sc1[][], double Sc2[][], double Sc3[][],
    		double Sc22[][], double Sc23[][], double Sc33[][],
    		double fftSupportPatch[], double fftSupportPatchImag[], int patchSize,
    		double fftImagePatch[], double fftImagePatchImag[],
    		double fd1[][], double fd1Imag[][], double fd2[][], double fd2Imag[][],
                         double fd3[][], double fd3Imag[][], double fd22[][], double fd22Imag[][],
                         double fd23[][], double fd23Imag[][], double fd33[][], double fd33Imag[][],
                         int findNorm) {
    	int sizem = fd1.length;
    	int sizen = fd1[0].length;
    	int y;
    	int x;
    	int yDim = sizem - 2 * patchSize;
    	int xDim = sizen - 2 * patchSize;
    	FFTUtility fft;
    	double ScR[] = new double[sizem * sizen];
    	double ScI[] = new double[sizem * sizen];
    	for (y = 0; y < sizem; y++) {
    		for (x = 0; x < sizen; x++) {
    			fd1[y][x] = fd1[y][x]/65535.0;
    			fd1Imag[y][x] = fd1Imag[y][x]/65535.0;
    			fd2[y][x] = fd2[y][x]/65535.0;
    			fd2Imag[y][x] = fd2Imag[y][x]/65535.0;
    			fd22[y][x] = fd22[y][x]/65535.0;
    			fd22Imag[y][x] = fd22Imag[y][x]/65535.0;
    			fd33[y][x] = fd33[y][x]/65535.0;
    			fd33Imag[y][x] = fd33Imag[y][x]/65535.0;
    			fd3[y][x] = -fd3Imag[y][x] - 1.0;
    			fd3Imag[y][x] = fd3[y][x];
    			fd23[y][x] = -fd23Imag[y][x] - 1.0;
    			fd23Imag[y][x] = fd23[y][x];
    		}
    	}
    	// Inverse fftshifts
    	fftshift(fd1);
    	fftshift(fd1Imag);
    	fftshift(fd2);
    	fftshift(fd2Imag);
    	fftshift(fd3);
    	fftshift(fd3Imag);
    	fftshift(fd22);
    	fftshift(fd22Imag);
    	fftshift(fd23);
    	fftshift(fd23Imag);
    	fftshift(fd33);
    	fftshift(fd33Imag);
    	
    	switch(findNorm) {
    	case 0:
	    	for (y = 0; y < sizem; y++) {
	    		for (x = 0; x < sizen; x++) {
	    			ScR[x + y * sizen] = fd1[y][x] * fftSupportPatch[x + y * sizen] - fd1Imag[y][x] * fftSupportPatchImag[x + y * sizen];
	    			ScI[x + y * sizen] = fd1Imag[y][x] * fftSupportPatch[x + y * sizen] + fd1[y][x] * fftSupportPatchImag[x + y * sizen];
	    		}
	    	}
	    	
	    	fft = new FFTUtility(ScR, ScI, sizem, sizen, 1, 1, FFTUtility.FFT);
		    fft.run();
		    fft.finalize();
		    fft = null;
		    fft = new FFTUtility(ScR, ScI, 1, sizem, sizen, 1, FFTUtility.FFT);
		    fft.run();
		    fft.finalize();
		    fft = null;
		    
		    Sc1 = new double[yDim][xDim];
		    for (y = 0; y < yDim; y++) {
		    	for (x = 0; x < xDim; x++) {
		    		Sc1[y][x] = ScR[x + patchSize + sizen * (y + patchSize)];
		    	}
		    }
		    
		    for (y = 0; y < sizem; y++) {
	    		for (x = 0; x < sizen; x++) {
	    			ScR[x + y * sizen] = fd2[y][x] * fftSupportPatch[x + y * sizen] - fd2Imag[y][x] * fftSupportPatchImag[x + y * sizen];
	    			ScI[x + y * sizen] = fd2Imag[y][x] * fftSupportPatch[x + y * sizen] + fd2[y][x] * fftSupportPatchImag[x + y * sizen];
	    		}
	    	}
	    	
	    	fft = new FFTUtility(ScR, ScI, sizem, sizen, 1, 1, FFTUtility.FFT);
		    fft.run();
		    fft.finalize();
		    fft = null;
		    fft = new FFTUtility(ScR, ScI, 1, sizem, sizen, 1, FFTUtility.FFT);
		    fft.run();
		    fft.finalize();
		    fft = null;
		    
		    Sc2 = new double[yDim][xDim];
		    for (y = 0; y < yDim; y++) {
		    	for (x = 0; x < xDim; x++) {
		    		Sc2[y][x] = ScR[x + patchSize + sizen * (y + patchSize)];
		    	}
		    }
		    
		    for (y = 0; y < sizem; y++) {
	    		for (x = 0; x < sizen; x++) {
	    			ScR[x + y * sizen] = fd3[y][x] * fftSupportPatch[x + y * sizen] - fd3Imag[y][x] * fftSupportPatchImag[x + y * sizen];
	    			ScI[x + y * sizen] = fd3Imag[y][x] * fftSupportPatch[x + y * sizen] + fd3[y][x] * fftSupportPatchImag[x + y * sizen];
	    		}
	    	}
	    	
	    	fft = new FFTUtility(ScR, ScI, sizem, sizen, 1, 1, FFTUtility.FFT);
		    fft.run();
		    fft.finalize();
		    fft = null;
		    fft = new FFTUtility(ScR, ScI, 1, sizem, sizen, 1, FFTUtility.FFT);
		    fft.run();
		    fft.finalize();
		    fft = null;
		    
		    Sc3 = new double[yDim][xDim];
		    for (y = 0; y < yDim; y++) {
		    	for (x = 0; x < xDim; x++) {
		    		Sc3[y][x] = ScR[x + patchSize + sizen * (y + patchSize)];
		    	}
		    }
		    
		    for (y = 0; y < sizem; y++) {
	    		for (x = 0; x < sizen; x++) {
	    			ScR[x + y * sizen] = fd22[y][x] * fftSupportPatch[x + y * sizen] - fd22Imag[y][x] * fftSupportPatchImag[x + y * sizen];
	    			ScI[x + y * sizen] = fd22Imag[y][x] * fftSupportPatch[x + y * sizen] + fd22[y][x] * fftSupportPatchImag[x + y * sizen];
	    		}
	    	}
	    	
	    	fft = new FFTUtility(ScR, ScI, sizem, sizen, 1, 1, FFTUtility.FFT);
		    fft.run();
		    fft.finalize();
		    fft = null;
		    fft = new FFTUtility(ScR, ScI, 1, sizem, sizen, 1, FFTUtility.FFT);
		    fft.run();
		    fft.finalize();
		    fft = null;
		    
		    Sc22 = new double[yDim][xDim];
		    for (y = 0; y < yDim; y++) {
		    	for (x = 0; x < xDim; x++) {
		    		Sc22[y][x] = ScR[x + patchSize + sizen * (y + patchSize)];
		    	}
		    }
		    
		    for (y = 0; y < sizem; y++) {
	    		for (x = 0; x < sizen; x++) {
	    			ScR[x + y * sizen] = fd23[y][x] * fftSupportPatch[x + y * sizen] - fd23Imag[y][x] * fftSupportPatchImag[x + y * sizen];
	    			ScI[x + y * sizen] = fd23Imag[y][x] * fftSupportPatch[x + y * sizen] + fd23[y][x] * fftSupportPatchImag[x + y * sizen];
	    		}
	    	}
	    	
	    	fft = new FFTUtility(ScR, ScI, sizem, sizen, 1, 1, FFTUtility.FFT);
		    fft.run();
		    fft.finalize();
		    fft = null;
		    fft = new FFTUtility(ScR, ScI, 1, sizem, sizen, 1, FFTUtility.FFT);
		    fft.run();
		    fft.finalize();
		    fft = null;
		    
		    Sc23 = new double[yDim][xDim];
		    for (y = 0; y < yDim; y++) {
		    	for (x = 0; x < xDim; x++) {
		    		Sc23[y][x] = ScR[x + patchSize + sizen * (y + patchSize)];
		    	}
		    }
		    
		    for (y = 0; y < sizem; y++) {
	    		for (x = 0; x < sizen; x++) {
	    			ScR[x + y * sizen] = fd33[y][x] * fftSupportPatch[x + y * sizen] - fd33Imag[y][x] * fftSupportPatchImag[x + y * sizen];
	    			ScI[x + y * sizen] = fd33Imag[y][x] * fftSupportPatch[x + y * sizen] + fd33[y][x] * fftSupportPatchImag[x + y * sizen];
	    		}
	    	}
	    	
	    	fft = new FFTUtility(ScR, ScI, sizem, sizen, 1, 1, FFTUtility.FFT);
		    fft.run();
		    fft.finalize();
		    fft = null;
		    fft = new FFTUtility(ScR, ScI, 1, sizem, sizen, 1, FFTUtility.FFT);
		    fft.run();
		    fft.finalize();
		    fft = null;
		    
		    Sc33 = new double[yDim][xDim];
		    for (y = 0; y < yDim; y++) {
		    	for (x = 0; x < xDim; x++) {
		    		Sc33[y][x] = ScR[x + patchSize + sizen * (y + patchSize)];
		    	}
		    }
		    
		    break;
    	case 1:
	    	for (y = 0; y < sizem; y++) {
	    		for (x = 0; x < sizen; x++) {
	    			ScR[x + y * sizen] = fd1[y][x] * fftSupportPatch[x + y * sizen] - fd1Imag[y][x] * fftSupportPatchImag[x + y * sizen];
	    			ScI[x + y * sizen] = fd1Imag[y][x] * fftSupportPatch[x + y * sizen] + fd1[y][x] * fftSupportPatchImag[x + y * sizen];
	    		}
	    	}
	    	
	    	fft = new FFTUtility(ScR, ScI, sizem, sizen, 1, 1, FFTUtility.FFT);
		    fft.run();
		    fft.finalize();
		    fft = null;
		    fft = new FFTUtility(ScR, ScI, 1, sizem, sizen, 1, FFTUtility.FFT);
		    fft.run();
		    fft.finalize();
		    fft = null;
		    
		    preComputedSc1 = new double[yDim][xDim];
		    for (y = 0; y < yDim; y++) {
		    	for (x = 0; x < xDim; x++) {
		    		preComputedSc1[y][x] = ScR[x + patchSize + sizen * (y + patchSize)];
		    	}
		    }
		    
		    for (y = 0; y < sizem; y++) {
	    		for (x = 0; x < sizen; x++) {
	    			ScR[x + y * sizen] = fd1[y][x] * fftImagePatch[x + y * sizen] - fd1Imag[y][x] * fftImagePatchImag[x + y * sizen];
	    			ScI[x + y * sizen] = fd1Imag[y][x] * fftImagePatch[x + y * sizen] + fd1[y][x] * fftImagePatchImag[x + y * sizen];
	    		}
	    	}
	    	
	    	fft = new FFTUtility(ScR, ScI, sizem, sizen, 1, 1, FFTUtility.FFT);
		    fft.run();
		    fft.finalize();
		    fft = null;
		    fft = new FFTUtility(ScR, ScI, 1, sizem, sizen, 1, FFTUtility.FFT);
		    fft.run();
		    fft.finalize();
		    fft = null;
		    
		    preComputedIc1 = new double[yDim][xDim];
		    for (y = 0; y < yDim; y++) {
		    	for (x = 0; x < xDim; x++) {
		    		preComputedIc1[y][x] = ScR[x + patchSize + sizen * (y + patchSize)];
		    	}
		    }
		    
		    preComputedsm = new double[yDim][xDim];
		    for (y = 0; y < yDim; y++) {
		    	for (x = 0; x < xDim; x++) {
		    		preComputedsm[y][x] = preComputedIc1[y][x]/preComputedSc1[y][x];
		    	}
		    }
    		
    		break;
    	case 2:
    		for (y = 0; y < sizem; y++) {
	    		for (x = 0; x < sizen; x++) {
	    			ScR[x + y * sizen] = fd2[y][x] * fftImagePatch[x + y * sizen] - fd2Imag[y][x] * fftImagePatchImag[x + y * sizen];
	    			ScI[x + y * sizen] = fd2Imag[y][x] * fftImagePatch[x + y * sizen] + fd2[y][x] * fftImagePatchImag[x + y * sizen];
	    		}
	    	}
	    	
	    	fft = new FFTUtility(ScR, ScI, sizem, sizen, 1, 1, FFTUtility.FFT);
		    fft.run();
		    fft.finalize();
		    fft = null;
		    fft = new FFTUtility(ScR, ScI, 1, sizem, sizen, 1, FFTUtility.FFT);
		    fft.run();
		    fft.finalize();
		    fft = null;
		    
		    preComputedIc2 = new double[yDim][xDim];
		    for (y = 0; y < yDim; y++) {
		    	for (x = 0; x < xDim; x++) {
		    		preComputedIc2[y][x] = ScR[x + patchSize + sizen * (y + patchSize)];
		    	}
		    }
		    
		    for (y = 0; y < sizem; y++) {
	    		for (x = 0; x < sizen; x++) {
	    			ScR[x + y * sizen] = fd3[y][x] * fftImagePatch[x + y * sizen] - fd3Imag[y][x] * fftImagePatchImag[x + y * sizen];
	    			ScI[x + y * sizen] = fd3Imag[y][x] * fftImagePatch[x + y * sizen] + fd3[y][x] * fftImagePatchImag[x + y * sizen];
	    		}
	    	}
	    	
	    	fft = new FFTUtility(ScR, ScI, sizem, sizen, 1, 1, FFTUtility.FFT);
		    fft.run();
		    fft.finalize();
		    fft = null;
		    fft = new FFTUtility(ScR, ScI, 1, sizem, sizen, 1, FFTUtility.FFT);
		    fft.run();
		    fft.finalize();
		    fft = null;
		    
		    preComputedIc3 = new double[yDim][xDim];
		    for (y = 0; y < yDim; y++) {
		    	for (x = 0; x < xDim; x++) {
		    		preComputedIc3[y][x] = ScR[x + patchSize + sizen * (y + patchSize)];
		    	}
		    }
    	} // switch (fnorm)
    }
    
    // Works for both fftshift and ifftshift
    private void fftshift(double mtx[][]) {
    	int sizem = mtx.length;
    	int sizen = mtx[0].length;
    	int y;
    	int x;
    	int DCY = (int)Math.ceil(sizem/2.0);
    	int DCX = (int)Math.ceil(sizen/2.0);
    	double temp[][] = new double[sizem][sizen];
    	for (y = 0; y < DCY; y++) {
    		for (x = 0; x < DCX; x++) {
    			temp[y][x] = mtx[y + DCY][x + DCX];
    		}
    		for (x = DCX; x < sizen; x++) {
    			temp[y][x] = mtx[y + DCY][x - DCX];
    		}
    	}
    	for (y = DCY; y < sizem; y++) {
    		for (x = 0; x < DCX; x++) {
    			temp[y][x] = mtx[y - DCY][x + DCX];
    		}
    		for (x = DCX; x < sizen; x++) {
    			temp[y][x] = mtx[y - DCY][x - DCX];
    		}
    	}
    	for (y = 0; y < sizem; y++) {
    		for (x = 0; x < sizen; x++) {
    			mtx[y][x] = temp[y][x];
    		}
    	}
    }
    
    private void T2z1a_make_image_structure(double fftImagePatch[], double fftImagePatchImag[],
    		                                double fftSupportPatch[], double fftSupportPatchImag[],
    		                                double input[][], int patchSize) {
        int sizem0 = input.length;
        int sizen0 = input[0].length;
        int padXDim = sizen0 + 2*patchSize;
        int padYDim = sizem0 + 2 * patchSize;
        int y;
        int x;
        double timeImagePatch[] = new double[padXDim * padYDim];
        double timeSupportPatch[] = new double[padXDim * padYDim];
        fftImagePatch = new double[padXDim * padYDim];
        fftImagePatchImag = new double[padXDim * padYDim];
        fftSupportPatch = new double[padXDim * padYDim];
        fftSupportPatchImag = new double[padXDim * padYDim];
        FFTUtility fft;
        for (y = 0; y < sizem0; y++) {
            for (x = 0; x < sizen0; x++) {
            	timeImagePatch[x + patchSize + padXDim * (y + patchSize)] = input[y][x];
            	timeSupportPatch[x + patchSize + padXDim * (y + patchSize)] = 1.0;
            	fftImagePatch[x + patchSize + padXDim * (y + patchSize)] = input[y][x];
            	fftSupportPatch[x + patchSize + padXDim * (y + patchSize)] = 1.0;
            }
        }
        
        fft = new FFTUtility(fftImagePatch, fftImagePatchImag, padYDim, padXDim, 1, -1, FFTUtility.FFT);
	    fft.run();
	    fft.finalize();
	    fft = null;
	    fft = new FFTUtility(fftImagePatch, fftImagePatchImag, 1, padYDim, padXDim, -1, FFTUtility.FFT);
	    fft.run();
	    fft.finalize();
	    fft = null;
	    
	    fft = new FFTUtility(fftSupportPatch, fftSupportPatchImag, padYDim, padXDim, 1, -1, FFTUtility.FFT);
	    fft.run();
	    fft.finalize();
	    fft = null;
	    fft = new FFTUtility(fftSupportPatch, fftSupportPatchImag, 1, padYDim, padXDim, -1, FFTUtility.FFT);
	    fft.run();
	    fft.finalize();
	    fft = null; 
	    
	    return;
    }

    private void T1_responses(double td1[][][], double td2[][][], double td3[][][],
    		                  double td22[][][], double td23[][][], double td33[][][],
    		                  double fd1[][][], double fd1Imag[][][], double fd2[][][], double fd2Imag[][][],
    		                  double fd3[][][], double fd3Imag[][][], double fd22[][][], double fd22Imag[][][],
    		                  double fd23[][][], double fd23Imag[][][], double fd33[][][], double fd33Imag[][][],
    		                  double sigmas[], int ps[],
    		final int nscales, final int ndirs, final double sig2omega, final double radianStart, final double radianEnd,
            final int inputXDim, final int inputYDim, final String filterType, String domain) {
        final double omegas[][] = new double[nscales * ndirs][];
        final double amplitudes[][] = new double[nscales * ndirs][];
        final double filterAngle[] = new double[nscales * ndirs];
        final double sigmaX[] = new double[nscales * ndirs];
        int filInd;
        int scInd;
        int drInd;
        int szfm;
        int szfn;
        int i;
        double fm[];
        double fn[];
        double xfreq[][];
        double yfreq[][];
        int y;
        int x;

        filterbank_DCA_2D(omegas, amplitudes, filterAngle, sigmaX, nscales, filterType, ndirs, sig2omega, radianStart, radianEnd);

        filInd = 0;
        for (scInd = 1; scInd <= nscales; scInd++) {
            
           
            for (drInd = 1; drInd <= ndirs; drInd++) {
                filInd++;
                if (domain == null) {
                    if (scInd < 4) {
                        domain = "time";
                    } else {
                        domain = "freq";
                    }
                } // if (domain == null);
                sigmas[filInd - 1] = sigmaX[filInd - 1];

                if (domain.equals("freq")) {
                    ps[filInd-1] = 3 * (int) Math.ceil(sigmas[filInd - 1]);
                    szfm = inputYDim + 2 * ps[0];
                    szfn = inputXDim + 2 * ps[0];
                    fm = new double[szfm];
                    fn = new double[szfn];
                    if ( (szfm % 2) == 1) {
                        for (i = 0; i < szfm; i++) {
                            fm[i] = -1.0 + 1.0 / szfm + (2.0 * i) / szfm;
                        }
                    } else {
                        for (i = 0; i < szfm; i++) {
                            fm[i] = -1.0 + (2.0 * i) / szfm;
                        }
                    }
                    if ( (szfn % 2) == 1) {
                        for (i = 0; i < szfn; i++) {
                            fn[i] = -1.0 + 1.0 / szfn + (2.0 * i) / szfn;
                        }
                    } else {
                        for (i = 0; i < szfn; i++) {
                            fn[i] = -1.0 + (2.0 * i) / szfn;
                        }
                    }
                    xfreq = new double[szfn][szfm];
                    yfreq = new double[szfn][szfm];
                    for (y = 0; y < szfn; y++) {
                        for (x = 0; x < szfm; x++) {
                            xfreq[y][x] = fm[x];
                        }
                    }
                    for (x = 0; x < szfm; x++) {
                        for (y = 0; y < szfn; y++) {
                            yfreq[y][x] = fn[y];
                        }
                    }
                } // if (domain[filInd-1].equals("freq"))
                else {
                    xfreq = null;
                    yfreq = null;
                    ps[filInd-1] = -1;
                }
                T1z2_get_filter_struct(td1[filInd-1], td2[filInd-1],td3[filInd-1],
                		td22[filInd-1], td23[filInd-1], td33[filInd-1],
                		fd1[filInd-1], fd1Imag[filInd-1], fd2[filInd-1], fd2Imag[filInd-1],
                		fd3[filInd-1], fd3Imag[filInd-1], fd22[filInd-1], fd22Imag[filInd-1],
                		fd23[filInd-1], fd23[filInd-1], fd33[filInd-1], fd33Imag[filInd-1],
                		omegas[filInd - 1], amplitudes[filInd - 1], filterAngle[filInd - 1], sigmaX[filInd - 1],
                		domain, xfreq, yfreq);
            } // for (drInd = 1; drInd <= ndirs; drInd++)
        } // for (scInd = 1; scInd <= nscales; scInd++)

    }

    private void T1z2_get_filter_struct(double td1[][], double td2[][], double td3[][], 
    		                            double td22[][], double td23[][], double td33[][],
    		                            double fd1[][], double fd1Imag[][], double fd2[][], double fd2Imag[][],
    		                            double fd3[][], double fd3Imag[][], double fd22[][], double fd22Imag[][],
    		                            double fd23[][], double fd23Imag[][], double fd33[][], double fd33Imag[][],
    		final double omegas[], final double amplitudes[], final double filterAngle, final double sigmaX, 
    		final String domain, final double xfreq[][], final double yfreq[][]) {
        final double omegasgb0[] = null;
        final double amplitudesgb0[] = null;
        final double amplitudesgb0Imag[] = null;
        final double omegasgbe[] = null;
        final double amplitudesgbe[] = null;
        final double amplitudesgbeImag[] = null;
        final double omegasgbo[] = null;
        final double amplitudesgbo[] = null;
        final double amplitudesgboImag[] = null;
        final double omegasgbee[] = null;
        final double amplitudesgbee[] = null;
        final double amplitudesgbeeImag[] = null;
        final double omegasgbeo[] = null;
        final double amplitudesgbeo[] = null;
        final double amplitudesgbeoImag[] = null;
        final double omegasgboo[] = null;
        final double amplitudesgboo[] = null;
        final double amplitudesgbooImag[] = null;
        double w1fd1[] = null;
        double w2fd1[] = null;
        double w1fd2[] = null;
        double w2fd2[] = null;
        double w1fd3[] = null;
        double w2fd3[] = null;
        double w1fd22[] = null;
        double w2fd22[] = null;
        double w1fd23[] = null;
        double w2fd23[] = null;
        double w1fd33[] = null;
        double w2fd33[] = null;
        int sz[] = new int[2];
        // gaussian (constant basis)
        T1z2a_convert_filter(omegasgb0, amplitudesgb0, amplitudesgb0Imag, omegas, amplitudes, filterAngle, sigmaX, 0);

        // even part of gabor complex
        T1z2a_convert_filter(omegasgbe, amplitudesgbe, amplitudesgbeImag, omegas, amplitudes, filterAngle, sigmaX, 1);

        // odd part of gabor complex
        T1z2a_convert_filter(omegasgbo, amplitudesgbo, amplitudesgboImag, omegas, amplitudes, filterAngle, sigmaX, 2);

        // even basis squared * gaussian
        T1z2a_convert_filter(omegasgbee, amplitudesgbee, amplitudesgbeeImag, omegas, amplitudes, filterAngle, sigmaX, 11);

        // even basis * odd basis * gaussian
        T1z2a_convert_filter(omegasgbeo, amplitudesgbeo, amplitudesgbeoImag, omegas, amplitudes, filterAngle, sigmaX, 12);

        // odd basis squared * gaussian
        T1z2a_convert_filter(omegasgboo, amplitudesgboo, amplitudesgbooImag, omegas, amplitudes, filterAngle, sigmaX, 22);

        if (domain.equals("time") || domain.equals("freq")) {
            td1 = T1z2b_time_resp(omegasgb0, amplitudesgb0, amplitudesgb0Imag, filterAngle, sigmaX);
            td2 = T1z2b_time_resp(omegasgbe, amplitudesgbe, amplitudesgbeImag, filterAngle, sigmaX);
            td3 = T1z2b_time_resp(omegasgbo, amplitudesgbo, amplitudesgboImag, filterAngle, sigmaX);
            td22 = T1z2b_time_resp(omegasgbee, amplitudesgbee, amplitudesgbeeImag, filterAngle, sigmaX);
            td23 = T1z2b_time_resp(omegasgbeo, amplitudesgbeo, amplitudesgbeoImag, filterAngle, sigmaX);
            td33 = T1z2b_time_resp(omegasgboo, amplitudesgboo, amplitudesgbooImag, filterAngle, sigmaX);
            if (domain.equals("freq")) {
                sz[0] = xfreq.length;
                sz[1] = xfreq[0].length; 
                freqz2(fd1, fd1Imag, w1fd1, w2fd1, td1, sz[1], sz[0]);
                freqz2(fd2, fd2Imag, w1fd2, w2fd2, td2, sz[1], sz[0]);
                freqz2(fd3, fd3Imag, w1fd3, w2fd3, td3, sz[1], sz[0]);
                freqz2(fd22, fd22Imag, w1fd22, w2fd22, td22, sz[1], sz[0]);
                freqz2(fd23, fd23Imag, w1fd23, w2fd23, td23, sz[1], sz[0]);
                freqz2(fd33, fd33Imag, w1fd33, w2fd33, td33, sz[1], sz[0]);
            } // if (domain.equals("freq"))
        } // if (domain.equals("time") || domain.equals("freq"))
        else if (domain.equals("freq_pure")) {
            Tzz_freq_resp(fd1, fd1Imag, xfreq, yfreq, sigmaX, filterAngle, omegasgb0, amplitudesgb0, amplitudesgb0Imag);
            Tzz_freq_resp(fd2, fd2Imag, xfreq, yfreq, sigmaX, filterAngle, omegasgbe, amplitudesgbe, amplitudesgbeImag);
            Tzz_freq_resp(fd3, fd3Imag, xfreq, yfreq, sigmaX, filterAngle, omegasgbo, amplitudesgbo, amplitudesgboImag);
            Tzz_freq_resp(fd22, fd22Imag, xfreq, yfreq, sigmaX, filterAngle, omegasgbee, amplitudesgbee, amplitudesgbeeImag);
            Tzz_freq_resp(fd23, fd23Imag, xfreq, yfreq, sigmaX, filterAngle, omegasgbeo, amplitudesgbeo, amplitudesgbeoImag);
            Tzz_freq_resp(fd33, fd33Imag, xfreq, yfreq, sigmaX, filterAngle, omegasgboo, amplitudesgboo, amplitudesgbooImag);
        } // else if (domain.equals("freq_pure"))
    }
    
    private void Tzz_freq_resp(double hout[][], double houtImag[][], double omegax[][], double omegay[][], double sigmaX, 
    		double filterAngle, double omegas[],
    		double amplitudes[], double amplitudesImag[]) {
    	int y;
    	int x;
    	double invSigmaX;
    	double cosang;
    	double sinang;
    	int sizem;
    	int sizen;
    	double maxAbsOmegas;
    	int i;
    	double rotatedx[][][];
    	double rotatedy[][][];
    	int cnt;
    	int sig1[] = new int[]{0,-1,1};
    	int sig2[] = new int[]{0,-1,1};
    	int sig1Index;
    	int sig2Index;
    	double absAmp[];
    	double maxAbsAmp;
    	double threshMin;
    	boolean omegaswt[];
    	double carrier;
    	double cenx;
    	double ceny;
    	double offx;
    	double offy;
    	double distCen[][];
    	double gaussianTerm;
    	int j;
    	double diffx;
    	double diffy;
    	double denom;
    	
        for (y = 0; y < omegax.length; y++) {
        	for (x = 0; x < omegax[0].length; x++) {
        		omegax[y][x] = -omegax[y][x];
        	}
        }
        for (y = 0; y < omegay.length; y++) {
        	for (x = 0; x < omegay[0].length; x++) {
        		omegay[y][x] = -omegay[y][x];
        	}
        }
        
        invSigmaX = 1.0/(Math.PI * sigmaX);
        cosang = Math.cos(filterAngle);
        sinang = Math.sin(filterAngle);
        
        sizem = omegax.length;
        sizen = omegax[0].length;
        
        maxAbsOmegas = 0.0;
        for (i = 0; i < omegas.length; i++) {
        	if (Math.abs(omegas[i]) > maxAbsOmegas) {
        		maxAbsOmegas = Math.abs(omegas[i]);
        	}
        }
        if ((maxAbsOmegas + 3.0 * invSigmaX) > 1.0) {
        	rotatedx = new double[sizem][sizen][9];
        	rotatedy = new double[sizem][sizen][9];
        }
        else {
        	rotatedx = new double[sizem][sizen][1];
        	rotatedy = new double[sizem][sizen][1];
        }
        
        for (y = 0; y < sizem; y++) {
        	for (x = 0; x < sizen; x++) {
        		rotatedx[y][x][0] = omegax[y][x]*cosang + omegay[y][x]*sinang;
        		rotatedy[y][x][0] = -omegax[y][x]*sinang + omegay[y][x]*cosang;
        	}
        }
        
        cnt = 0;
        for (sig1Index = 0; sig1Index <= 2; sig1Index++) {
            for (sig2Index = 0; sig2Index <= 2; sig2Index++) {
                for (y = 0; y < sizem; y++) {
                	for (x = 0; x < sizen; x++) {
                		rotatedx[y][x][cnt] = rotatedx[y][x][0] + 2.0*sig1[sig1Index]*cosang + 2.0*sig2[sig2Index]*sinang;
                		rotatedy[y][x][cnt] = rotatedy[y][x][0] - 2.0*sig1[sig1Index]*sinang + 2.0*sig2[sig2Index]*cosang;
                	}
                } // for (y = 0; y < sizem; y++)
                cnt++;
            }
        }
        
        absAmp = new double[amplitudes.length];
        maxAbsAmp = 0.0;
        for (i = 0; i < amplitudes.length; i++) {
        	absAmp[i] = Math.sqrt(amplitudes[i]*amplitudes[i] + amplitudesImag[i]*amplitudesImag[i]);
        	if (absAmp[i] > maxAbsAmp) {
        		maxAbsAmp = absAmp[i];
        	}
        }
        threshMin = 0.01 * maxAbsAmp;
        omegaswt = new boolean[amplitudes.length];
        for (i = 0; i < amplitudes.length; i++) {
        	if (absAmp[i] > threshMin) {
        		omegaswt[i] = true;
        	}
        }
        
        distCen = new double[sizem][sizen];
        hout = new double[sizem][sizen];
        houtImag = new double[sizem][sizen];
        denom = 2.0 * invSigmaX * invSigmaX;
        for (i = 0; i < amplitudes.length; i++) {
        	if (omegaswt[i]) {
        	    carrier = omegas[i];
        	    cenx = carrier * cosang;
        	    ceny = carrier * sinang;
        	    offx = (-cenx * cosang) - ceny*sinang;
        	    offy = (cenx * sinang) - ceny*cosang;
        	    if ((Math.abs(carrier) + 3.0*invSigmaX) > 1.0) {
        	        for (y = 0; y < sizem; y++) {
    	    		    for (x = 0; x < sizen; x++) { 
        	    			distCen[y][x] = Double.MAX_VALUE;
        	    			for (j = 0; j < 9; j++) {
	        	    			diffx = rotatedx[y][x][j] - offx;
	        	    			diffy = rotatedy[y][x][j] - offy;
	        	    			if (diffx*diffx + diffy*diffy < distCen[y][x]) {
	        	    				distCen[y][x] = diffx*diffx + diffy*diffy;
	        	    			}
        	    			}
        	    		}
        	    	}
        	    } //  if ((Math.abs(carrier) + 3.0*invSigmaX) > 1.0)
        	    else {
        	    	for (y = 0; y < sizem; y++) {
        	    		for (x = 0; x < sizen; x++) {
        	    			diffx = rotatedx[y][x][0] - offx;
        	    			diffy = rotatedy[y][x][0] - offy;
        	    			distCen[y][x] = diffx*diffx + diffy*diffy;
        	    		}
        	    	}
        	    } // else
        	    for (y = 0; y < sizem; y++) {
        	    	for (x = 0; x < sizen; x++) {
        	    		gaussianTerm = Math.exp(-distCen[y][x]/denom)/2.0;
        	    		hout[y][x] = hout[y][x] + amplitudes[i]*gaussianTerm;
        	    		houtImag[y][x] = houtImag[y][x] + amplitudesImag[i]*gaussianTerm;
        	    	}
        	    }
        	} // if (omegaswt[i])
        } // for (i = 0; i < amplitudes.length; i++)
       
    	return;
    }
    
    private void freqz2(double hout[][], double houtImag[][], double w1[], double w2[], double a[][], int n1, int n2) {
    	double apad[][] = null;
    	int y;
    	int x;
    	int yoff;
    	int xoff;
    	int i;
    	int w1off;
    	double w1scale;
    	int w2off;
    	double w2scale;
    	boolean useMesh = false;
    	double FFTR[];
    	double FFTI[];
    	FFTUtility fft;
    	double t1[][];
    	double t2[][];
    	double w1g[][];
    	double w2g[][];
    	int yout;
    	int xout;
    	double maxAbsImag;
    	
    	hout = new double[n2][n1];
		houtImag = new double[n2][n1];
    	
    	w1 = new double[n1];
	    w1off = (int)Math.floor(n1/2.0);
	    w1scale = 2.0/n1;
	    for (i = 0; i < n1; i++) {
	    	w1[i] = (i - w1off)*w1scale; 
	    }
	    
	    w2 = new double[n2];
	    w2off = (int)Math.floor(n2/2.0);
	    w2scale = 2.0/n2;
	    for (i = 0; i < n2; i++) {
	    	w2[i] = (i - w2off)*w2scale; 
	    }
	    rot180(a);
	    
    	if ((a.length > n2) || (a[0].length > n1)) {
    		useMesh = true;
    	}
    	else if (a.length != n2 || a[0].length != n1) {
    	    apad = new double[n2][n1];
    	    yoff  = (int)Math.floor(n2/2.0) - (int)Math.floor(a.length/2.0);
    	    xoff = (int)Math.floor(n1/2.0) - (int)Math.floor(a[0].length/2.0);
    	    
    	    for (y = 0; y < a.length; y++) {
    	    	for (x = 0; x < a[0].length; x++) {
    	    		apad[y + yoff][x + xoff] = a[y][x];
    	    	}
    	    }
    	} // if (a.length != n2 || a[0].length != n1)
    	else {
    		apad = a;
    	}
    	
    	    
	    if (!useMesh) {
	    	// Inverse fftshift
	    	fftshift(apad);
		    FFTR = new double[n2 * n1];
		    FFTI = new double[n2 * n1];
		    for (y = 0; y < n2; y++) {
		    	for (x = 0; x < n1; x++) {
		    		FFTR[x + y * n1] = apad[y][x];
		    	}
		    }
		    // For FFTUtility calls are scaled by 1/n for the inverse transform
		    // In MATLAB ifft has a scaling of 1/M
		    fft = new FFTUtility(FFTR, FFTI, n2, n1, 1, -1, FFTUtility.FFT);
		    fft.run();
		    fft.finalize();
		    fft = null;
		    fft = new FFTUtility(FFTR, FFTI, 1, n2, n1, -1, FFTUtility.FFT);
		    fft.run();
		    fft.finalize();
		    fft = null;
		    for (y = 0; y < n2; y++) {
		    	for (x = 0; x < n1; x++) {
		    		hout[y][x] = FFTR[x + y * n1];
		    		houtImag[y][x] = FFTI[x + y * n1];
		    	}
		    }
		    fftshift(hout);
		    fftshift(houtImag);
	    } // if (!useMesh)
	    else { // useMesh
		    w1g = new double[n2][n1];
		    w2g = new double[n2][n1];
		    for (y = 0; y < n2; y++) {
		    	for (x = 0; x < n1; x++) {
		    		w1g[y][x] = w1[x];
		    	}
		    }
		    
		    for (x = 0; x < n1; x++) {
		    	for (y = 0; y < n2; y++) {
		    		w2g[y][x] = w2[y];
		    	}
		    }
		    
		    t1 = new double[a.length][a[0].length];
	        t2 = new double[a.length][a[0].length];
	        if ( (a[0].length % 2) == 1) {
	            for (y = 0; y < a.length; y++) {
		            for (x = 0; x < a[0].length; x++) {
		                t1[y][x] = -a[0].length/2.0 + 1.0 / 2.0 + x;
		            }
	            }
	        } else {
	        	for (y = 0; y < a.length; y++) {
		            for (x = 0; x < a[0].length; x++) {
		                t1[y][x] = -a[0].length/2.0 + x;
		            }
	        	}
	        }
	        if ( (a.length % 2) == 1) {
	        	for (x = 0; x < a[0].length; x++) {
		            for (y = 0; y < a.length; y++) {
		                t2[y][x] = -a.length/2.0 + 1.0 / 2.0 + y;
		            }
	        	}
	        } else {
	        	for (x = 0; x < a[0].length; x++) {
		            for (y = 0; y < a.length; y++) {
		                t2[y][x] = -a.length/2.0 + y;
		            }
	        	}
	        }
	        
	        for (yout = 0; yout < n2; yout++) {
	            for (xout = 0; xout < n1; xout++) {
	            	for (y = 0; y < a.length; y++) {
	            		for (x = 0; x < a[0].length; x++) {
	            			hout[yout][xout] += Math.cos(Math.PI*(w1g[yout][xout]*t1[y][x] + w2g[yout][xout]*t2[y][x])*a[y][x]);
	            			houtImag[yout][xout] -= Math.sin(Math.PI*(w1g[yout][xout]*t1[y][x] + w2g[yout][xout]*t2[y][x])*a[y][x]);
	            		}
	            	}
	            }
	        } // for (yout = 0; yout < n2; yout++)
	    }
	    
	    maxAbsImag = 0.0;
	    for (y = 0; y < n2; y++) {
	    	for (x = 0; x < n1; x++) {
	    		if (Math.abs(houtImag[y][x]) > maxAbsImag) {
	    			maxAbsImag = Math.abs(houtImag[y][x]);
	    		}
	    	}
	    }
	    
	    if (maxAbsImag < Math.sqrt(epsilon)) {
	    	for (y = 0; y < n2; y++) {
	    		for (x = 0; x < n1; x++) {
	    			houtImag[y][x] = 0.0;
	    		}
	    	}
	    }
	    
	    return;
    	
    }
    
    private void rot180(double mtx[][]) {
        int sizem = mtx.length;
        int sizen = mtx[0].length;
        int y;
        int x;
        double temp[][] = new double[sizem][sizen];
        for (y = 0; y < sizem; y++) {
        	for (x = 0; x < sizen; x++) {
        		temp[y][x] = mtx[sizem - 1 - y][sizen - 1 - x];
        	}
        }
        for (y = 0; y < sizem; y++) {
        	for (x = 0; x < sizen; x++) {
        		mtx[y][x] = temp[y][x];
        	}
        }
    }

    private double[][] T1z2b_time_resp(final double omegas[], final double amplitudes[], final double amplitudesImag[], final double filterAngle,
            final double sigmaX) {
        int mx;
        int grx[][];
        int gry[][];
        int y;
        int x;
        int sz;
        double cosa;
        double sina;
        double gaussian[][];
        double sum;
        int namp;
        int cen;
        double res[][];
        double rotx[][];
        int k;
        double ampPos;
        double ampPosImag;
        double ampPosAbs;
        double ampNeg;
        double ampNegImag;
        double omega;
        double ampDiff;
        double ampDiffImag;
        double ampDiffAbs;

        mx = (int) Math.ceil(Math.max(5 * sigmaX, 5));
        sz = 2 * mx + 1;
        grx = new int[sz][sz];
        gry = new int[sz][sz];
        for (y = 0; y < sz; y++) {
            for (x = 0; x < sz; x++) {
                grx[y][x] = x - mx;
            }
        }
        for (x = 0; x < sz; x++) {
            for (y = 0; y < sz; y++) {
                gry[y][x] = y - mx;
            }
        }

        cosa = Math.cos(filterAngle);
        sina = Math.sin(filterAngle);
        gaussian = new double[sz][sz];
        for (y = 0; y < sz; y++) {
            for (x = 0; x < sz; x++) {
                gaussian[y][x] = Math.exp( - (grx[y][x] * grx[y][x] + gry[y][x] * gry[y][x]) / (2.0 * sigmaX * sigmaX));
            }
        }
        sum = 0.0;
        for (y = 0; y < sz; y++) {
            for (x = 0; x < sz; x++) {
                sum += gaussian[y][x];
            }
        }
        for (y = 0; y < sz; y++) {
            for (x = 0; x < sz; x++) {
                gaussian[y][x] = gaussian[y][x] / sum;
            }
        }

        rotx = new double[sz][sz];
        for (y = 0; y < sz; y++) {
            for (x = 0; x < sz; x++) {
                rotx[y][x] = grx[y][x] * cosa + gry[y][x] * sina;
            }
        }

        namp = (amplitudes.length - 1) / 2;
        cen = (amplitudes.length + 1) / 2;
        res = new double[sz][sz];
        for (y = 0; y < sz; y++) {
            for (x = 0; x < sz; x++) {
                res[y][x] = gaussian[y][x] * amplitudes[cen - 1] / 2.0;
            }
        }

        for (k = 1; k <= namp; k++) {
            ampPos = amplitudes[cen + k - 1];
            ampPosImag = amplitudesImag[cen + k - 1];
            ampNeg = amplitudes[cen - k - 1];
            ampNegImag = amplitudesImag[cen - k - 1];
            omega = Math.PI * omegas[cen + k - 1];
            ampPosAbs = Math.sqrt(ampPos * ampPos + ampPosImag * ampPosImag);
            if (ampPosAbs > 1.0E-8) {
                ampDiff = ampPos - ampNeg;
                ampDiffImag = ampPosImag - ampNegImag;
                ampDiffAbs = Math.sqrt(ampDiff * ampDiff + ampDiffImag * ampDiffImag);
                if ( (ampDiffAbs / ampPosAbs) < 0.001) {
                    for (y = 0; y < sz; y++) {
                        for (x = 0; x < sz; x++) {
                            res[y][x] = res[y][x] + ampPos * gaussian[y][x] * Math.cos(omega * rotx[y][x]);
                        }
                    }
                } // if ((ampDiffAbs/ampPosAbs) < 0.001)
                else {
                    for (y = 0; y < sz; y++) {
                        for (x = 0; x < sz; x++) {
                            res[y][x] = res[y][x] - ampPosImag * gaussian[y][x] * Math.sin(omega * rotx[y][x]);
                        }
                    }
                }
            } // if (ampPosAbs < 1.0E-8)
        } // for (k = 1; k <= namp; k++)

        return res;
    }

    private void T1z2a_convert_filter(double omegasOut[], double amplitudesOut[], double amplitudesOutImag[], final double omegas[], final double amplitudes[],
            final double filterAngle, final double sigmaX, final int conj) {
        int sz0;
        double amplitudesOriginal[];
        int i;
        double omegas2[];
        double amplitudesEven[];
        double amplitudesOddImag[];
        double amplitudesUns[] = null;
        double amplitudesUnsImag[] = null;
        double amplitudesMult[] = null;
        double amplitudesMultImag[] = null;
        double A[];
        double B[];
        double C[];
        int sz1;

        sz0 = amplitudes.length - 1;

        amplitudesOriginal = new double[amplitudes.length + sz0];
        for (i = 0; i < amplitudes.length; i++) {
            amplitudesOriginal[i] = amplitudes[i];
        }
        omegas2 = new double[2 * omegas.length - 1];
        for (i = 0; i < omegas.length; i++) {
            omegas2[i] = omegas[i];
        }
        for (i = 0; i < omegas.length - 1; i++) {
            omegas2[omegas.length + i] = omegas[i + 1] + omegas[omegas.length - 1];
        }
        amplitudesEven = new double[2 * amplitudesOriginal.length - 1];
        for (i = 0; i < amplitudesOriginal.length - 1; i++) {
            amplitudesEven[i] = amplitudesOriginal[amplitudesOriginal.length - 1 - i];
        }
        for (i = 0; i < amplitudesOriginal.length; i++) {
            amplitudesEven[i + amplitudesOriginal.length - 1] = amplitudesOriginal[i];
        }
        amplitudesOddImag = new double[2 * amplitudesOriginal.length - 1];
        for (i = 0; i < amplitudesOriginal.length - 1; i++) {
            amplitudesOddImag[i] = -amplitudesOriginal[amplitudesOriginal.length - 1 - i];
        }
        for (i = 0; i < amplitudesOriginal.length; i++) {
            amplitudesOddImag[i + amplitudesOriginal.length - 1] = amplitudesOriginal[i];
        }
        omegasOut = new double[omegas2.length - 1];
        for (i = 0; i < omegas2.length - 1; i++) {
            omegasOut[i] = -omegas2[omegas2.length - 1 - i];
        }
        for (i = 0; i < omegas2.length; i++) {
            omegasOut[i + omegas2.length - 1] = omegas2[i];
        }

        switch (conj) {
            case 0:
                amplitudesUns = new double[amplitudesEven.length];
                for (i = 0; i < amplitudesUns.length; i++) {
                    amplitudesUns[i] = amplitudesEven[i];
                }
                amplitudesMult = new double[amplitudesEven.length];
                for (i = 0; i < amplitudesMult.length; i++) {
                    amplitudesMult[i] = amplitudesEven[i];
                }
                break;
            case 1:
                amplitudesUns = new double[amplitudesEven.length];
                for (i = 0; i < amplitudesUns.length; i++) {
                    amplitudesUns[i] = amplitudesEven[i];
                }
                break;
            case 2:
                amplitudesUnsImag = new double[amplitudesOddImag.length];
                for (i = 0; i < amplitudesUnsImag.length; i++) {
                    amplitudesUnsImag[i] = amplitudesOddImag[i];
                }
                break;
            case 11:
                amplitudesUns = new double[amplitudesEven.length];
                for (i = 0; i < amplitudesUns.length; i++) {
                    amplitudesUns[i] = amplitudesEven[i];
                }
                amplitudesMult = new double[amplitudesEven.length];
                for (i = 0; i < amplitudesMult.length; i++) {
                    amplitudesMult[i] = amplitudesEven[i];
                }
                break;
            case 12:
                amplitudesUnsImag = new double[amplitudesOddImag.length];
                for (i = 0; i < amplitudesUnsImag.length; i++) {
                    amplitudesUnsImag[i] = amplitudesOddImag[i];
                }
                amplitudesMult = new double[amplitudesEven.length];
                for (i = 0; i < amplitudesMult.length; i++) {
                    amplitudesMult[i] = amplitudesEven[i];
                }
                break;
            case 22:
                amplitudesUnsImag = new double[amplitudesOddImag.length];
                for (i = 0; i < amplitudesUnsImag.length; i++) {
                    amplitudesUnsImag[i] = amplitudesOddImag[i];
                }
                amplitudesMultImag = new double[amplitudesOddImag.length];
                for (i = 0; i < amplitudesMultImag.length; i++) {
                    amplitudesMultImag[i] = amplitudesOddImag[i];
                }
                break;
        } // switch (conj)

        switch (conj) {
            case 0:
                amplitudesOut = new double[] {2};
                amplitudesOutImag = new double[1];
                omegasOut = new double[] {0};
                break;
            case 1:
            case 2:
                if (amplitudesUns != null) {
                    amplitudesOut = amplitudesUns;
                    amplitudesOutImag = new double[amplitudesOut.length];
                } else {
                    amplitudesOutImag = amplitudesUnsImag;
                    amplitudesOut = new double[amplitudesOutImag.length];
                }
                break;
            default:
                // Estimate the fourier series coefficients of filter^2 = convolution of 1-d filters
                // Both A and B are the same length
                if (amplitudesUns != null) {
                    sz1 = amplitudesUns.length;
                    A = amplitudesUns;
                } else {
                    sz1 = amplitudesUnsImag.length;
                    A = amplitudesUnsImag;
                }
                if (amplitudesMult != null) {
                    B = amplitudesMult;
                } else {
                    B = amplitudesMultImag;
                }
                C = conv(A, B);
                for (i = 0; i < sz1; i++) {
                	C[i] = C[i]/2.0;
                }
                if ( (amplitudesUns != null) && (amplitudesMult != null)) {
                    amplitudesOut = C;
                    amplitudesOutImag = new double[amplitudesOut.length];
                } else if ( (amplitudesUnsImag != null) && (amplitudesMultImag != null)) {
                    amplitudesOut = new double[sz1];
                    for (i = 0; i < sz1; i++) {
                        amplitudesOut[i] = -C[i];
                    }
                    amplitudesOutImag = new double[amplitudesOut.length];
                } else {
                    amplitudesOutImag = C;
                    amplitudesOut = new double[amplitudesOutImag.length];
                }
        } // switch (conj)
    }

    private void filterbank_DCA_2D(double omegas[][], double amplitudes[][], double filterAngle[], double sigmaX[], final int nscales, final String filterType,
            final int ndirs, final double sig2omega, final double radianStart, final double radianEnd) {
        // Put central frequencies on logarithmic scale
        double factr;
        final double radianPerPixel[] = new double[nscales];
        int i;
        int scale;
        double rpp;
        double sigmaXTemp;
        int angle;
        int counter;
        double omegasInit[];
        double amplitudesInit[];
        double temp[];
        int dim;
        int dim2;
        int j;
        double val;
        int k;
        double sum;

        factr = Math.exp(Math.log(radianEnd / radianStart) / (nscales - 1));
        for (i = 0; i < nscales; i++) {
            radianPerPixel[i] = radianStart * Math.pow(factr, i);
        }

        for (scale = 1; scale <= nscales; scale++) {
            rpp = radianPerPixel[scale - 1];
            sigmaXTemp = sig2omega / rpp;
            if (filterType.equals("edge")) {
                rpp = rpp / 4.0;
            }
            for (angle = 1; angle <= ndirs; angle++) {
                counter = (scale - 1) * ndirs + angle;
                sigmaX[counter - 1] = sigmaXTemp;
                filterAngle[counter - 1] = Math.PI * (angle - 1) / ndirs;

                // Fields used for every single gabor filter
                if (filterType.equals("texture")) {
                    omegas[counter - 1] = new double[] {0, rpp};
                    amplitudes[counter - 1] = new double[] {0.0, 1.0};
                } else { // filterType.equals("edge"))
                    dim = 0;
                    for (val = 0; val <= 1.0; val += rpp) {
                        dim++;
                    }
                    temp = new double[dim];
                    for (val = 0, j = 0; val <= 1.0; val += rpp) {
                        temp[j++] = val;
                    }
                    dim2 = 0;
                    for (j = 1; j <= dim - 1; j += 2) {
                        dim2++;
                    }
                    omegasInit = new double[dim2];
                    for (j = 1, k = 0; j <= dim - 1; j += 2) {
                        omegasInit[k++] = temp[j];
                    }
                    amplitudesInit = new double[dim2];
                    for (j = 0; j < dim2; j++) {
                        amplitudesInit[j] = 1.0 / Math.max(omegasInit[j], epsilon);
                    }
                    omegas[counter - 1] = new double[dim2 + 1];
                    amplitudes[counter - 1] = new double[dim2 + 1];
                    sum = 0.0;
                    for (j = 1; j < dim2 + 1; j++) {
                        omegas[counter - 1][j] = omegasInit[j - 1];
                        amplitudes[counter - 1][j] = amplitudesInit[j - 1];
                        sum += amplitudes[counter - 1][j] * amplitudes[counter - 1][j];
                    }
                    sum = Math.sqrt(sum);
                    for (j = 1; j < dim2 + 1; j++) {
                        amplitudes[counter - 1][j] = amplitudes[counter - 1][j] / sum;
                    }
                } // else
            } // for (angle = 1; angle <= ndirs; angle++)
        } // for (scale = 1; scale <= nscales; scale++)
    }
}
