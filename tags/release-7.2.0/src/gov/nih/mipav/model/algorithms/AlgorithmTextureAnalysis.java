package gov.nih.mipav.model.algorithms;


import gov.nih.mipav.model.algorithms.utilities.AlgorithmChangeType;
import gov.nih.mipav.model.algorithms.utilities.AlgorithmRGBtoGray;
import gov.nih.mipav.model.structures.*;
import gov.nih.mipav.view.*;

import java.io.*;

import WildMagic.LibFoundation.Mathematics.Vector3f;

/**
 * 
Overview

This code contains the texture analysis functions for the paper `Texture Analysis and Segmentation Using Modulation Features,
Generative Models, and Weighted Curve Evolution', by I. Kokkinos, G. Evangelopoulos and P. Maragos, appearing in IEEE
Transactions on Pattern Analysis and Machine Intelligence, Volume 31, Issue 1, Jan. 2009 Page(s):142 - 157.

This toolbox was originally developed in and written for MATLAB, with emphasis on efficient algorithm implementations
for multiband image filtering, demodulation in amplitude (AM) and frequency (FM) signals via the regularized 2D discrete
energy separation algorithm and probabilistic localization of texture, edge, smooth image regions.

Author of the original MATLAB toolbox is Iasonas Kokkinos, currently Assistant Professor at Ecole Centrale Paris,
with partial contributions from Georgios Evangelopoulos.  The original MATLAB toolbox has been ported from MATLAB
to Java by William Gandler.  The original MATLAB toolbox can be found at http://cvsp.cs.ntua.gr/software/texture/.
Permission to port the original code was generously granted by Iasonas Kokkinos.

The provided functions include:

    multi-scale & orientation filterbanks for gabors and edges
    projection on the basis elements of the underlying generative models
    demodulation with regularized/complex esa
    channel selection based on the amplitude/teager/mdl criterion
    texture/edge/smooth classification based on mdl criterion 
    
    Parameters are primarily related to the filterbank construction and the final classification stage.
    
    
References:

    1.) I. Kokkinos, G. Evangelopoulos and P. Maragos,
    Texture Analysis and Segmentation using Modulation Features, Generative Models and Weighted Curve Evolution,
    IEEE Transactions on Pattern Analysis & Machine Intelligence, vol. 31, no. 1, pp. 142-157, Jan. 2009.
    
    2.) G. Evangelopoulos, I. Kokkinos and P. Maragos,
    Advances in Variational Image Segmentation using AM-FM Models: Regularized Demodulation and Probabilistic Cue Integration,
    Proc. Int' l Workshop on Variational and Level Set Methods (VLSM-05), Beijing, China, Oct. 2005, Springer LNCS, vol. 3275, pp. 121-136.
   
    3.) I. Kokkinos, G. Evangelopoulos and P. Maragos,
    Advances in Texture Analysis: Energy Dominant Component and Multiple Hypothesis Testing,
    Proc. IEEE Int' l Conf. on Image Processing (ICIP-04), Singapore, Oct. 2004, vol. 3, pp. 1509-1512.
 
    4.) I. Kokkinos, G. Evangelopoulos and P. Maragos,
    Modulation-Feature Based Textured Image Segmentation Using Curve Evolution,
    Proc. IEEE Int' l Conf. on Image Processing (ICIP-04), Singapore, Oct. 2004, vol. 2, pp. 1204-1207. 
 *
 */

public class AlgorithmTextureAnalysis extends AlgorithmBase {
	
	private ModelImage[] srcImage;
	
	private boolean[] scaleImage;
	
	 // epsilon = D1MACH(4)
    // Machine epsilon is the smallest positive epsilon such that
    // (1.0 + epsilon) != 1.0.
    // epsilon = 2**(1 - doubleDigits) = 2**(1 - 53) = 2**(-52)
    // epsilon = 2.2204460e-16
    // epsilon is called the largest relative spacing
	private double epsilon = Math.pow(2.0, -52);
	
	//~ Constructors ---------------------------------------------------------------------------------------------------
	public AlgorithmTextureAnalysis(ModelImage[] srcImage, boolean scaleImage[]) {
    	//super(null, srcImg);
		this.srcImage = srcImage;
		this.scaleImage = scaleImage;
    }
	
	/**
     * Starts the program.
     */
    public void runAlgorithm() {
    	ModelImage destImage = null;
    	AlgorithmChangeType changeTypeAlgo;
    	boolean image25D = true;
    	AlgorithmTransform algoTrans;
        AlgorithmRGBtoGray gAlgo;
        ModelImage grayImage = null;
        int i;
        boolean setupFilters = true;
        int lastXDim = srcImage[0].getExtents()[0];
        int lastYDim = srcImage[0].getExtents()[1];
        int inputXDim;
        int inputYDim;
        int ndirs = 10;
        int nscales = 4;
        // Proportional to number of oscillations within Gaussian
        double sig2omega = 1.0;
        int minSize;
        double largestPeriod;
        // Range of radians per pixel for sinusoid of Gabor filter
        double radianStart = 0.7; // Highest frequency
        double radianEnd; // Lowest frequency
        for (i = 0; i < srcImage.length; i++) {
        	inputXDim = srcImage[i].getExtents()[0];
        	inputYDim = srcImage[i].getExtents()[1];
	        if (srcImage[i].isColorImage()) {
	        	boolean thresholdAverage = false;
	    		float threshold = 0.0f;
	    		boolean intensityAverage = false;
	    		boolean equalRange = true;
	    		float minR = 0.0f;
	    		float minG = 0.0f;
	    		float minB = 0.0f;
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
	            }
	            else if (srcImage[i].getMinG() == srcImage[i].getMaxG()) {
	            	redValue = 0.5f;
	            	greenValue = 0.0f;
	            	blueValue = 0.5f;
	            }
	            else if (srcImage[i].getMinB() == srcImage[i].getMaxB()) {
	            	redValue = 0.5f;
	            	greenValue = 0.5f;
	            	blueValue = 0.0f;
	            }
	            else {
	            	redValue = (float)(1.0/3.0);
	            	greenValue = redValue;
	            	blueValue = redValue;
	            	
	            }
	            maxR = (float)srcImage[i].getMaxR();
	            maxG = (float)srcImage[i].getMaxG();
	            maxB = (float)srcImage[i].getMaxB();
	            grayImage = new ModelImage(ModelStorageBase.FLOAT, srcImage[i].getExtents(), "grayImage");
	            gAlgo = new AlgorithmRGBtoGray(grayImage, srcImage[i], redValue, greenValue, blueValue, thresholdAverage, threshold, 
	            		intensityAverage, equalRange, minR, maxR, minG, maxG, minB, maxB);
	            gAlgo.run();
	            gAlgo.finalize();
	        } // if (srcImage.isColorImage())
	        if (scaleImage[i]) {
		        destImage = new ModelImage(ModelStorageBase.DOUBLE, srcImage[i].getExtents(), "changeTypeImage");
		        if (srcImage[i].isColorImage()) {
		        	changeTypeAlgo = new AlgorithmChangeType(destImage, grayImage, grayImage.getMin(), grayImage.getMax(), 0.0, 1.0, image25D);
		        	grayImage.disposeLocal();
		        	grayImage = null;
		        }
		        else {
		            changeTypeAlgo = new AlgorithmChangeType(destImage, srcImage[i], srcImage[i].getMin(), srcImage[i].getMax(),
		            		0.0, 1.0, image25D);
		        }
		        changeTypeAlgo.run();
		        changeTypeAlgo.finalize();
		        changeTypeAlgo = null;
		        
		        final boolean doPad = false;
		        TransMatrix xfrm = new TransMatrix(3);
		        xfrm.identity();
		        final int interp = AlgorithmTransform.BILINEAR;
		        int oXdim = 219;
		        int oYdim = 146;
		        inputXDim = oXdim;
		        inputYDim = oYdim;
		        float oXres = srcImage[i].getFileInfo()[0].getResolutions()[0] * srcImage[i].getExtents()[0]/oXdim;
		        float oYres = srcImage[i].getFileInfo()[0].getResolutions()[1] * srcImage[i].getExtents()[1]/oYdim;
		        final int units[] = srcImage[i].getUnitsOfMeasure();
		        final boolean doClip = true;
		        final boolean doVOI = false;
		        final boolean doRotateCenter = false;
		        final Vector3f center = new Vector3f();
		        final float fillValue = 0.0f;
		        final boolean doUpdateOrigin = false;
		        final boolean isSATransform = false;
		        algoTrans = new AlgorithmTransform(destImage, xfrm, interp, oXres, oYres,
		                oXdim, oYdim, units, doVOI, doClip, doPad, doRotateCenter, center);
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
	        
	        if ((i == 0) || (inputXDim != lastXDim) || (inputYDim != lastYDim)) {
	        	setupFilters = true;
	        }
	        else {
	        	setupFilters = false;
	        }
	        
	        if (setupFilters) {
	            minSize = Math.min(inputXDim, inputYDim);
	            largestPeriod = minSize/4.0;
	            radianEnd = 2.0 * Math.PI/largestPeriod;
	            
	            T1_responses(nscales, ndirs, sig2omega, radianStart, radianEnd,
	            		inputXDim, inputYDim, "texture", null);
	        } // if (setupFilters)
        
        } // for (i = 0; i < srcImage.length; i++) 

    }
    
    private void T1_responses(int nscales, int ndirs, double sig2omega, double radianStart, double radianEnd, 
    		int inputXDim, int inputYDim, String filterType, String domain) {
    	double omegas[][] = null;
    	double amplitudes[][] = null;
    	double filterAngle[] = null;
    	double sigmaX[] = null;
    	int filInd;
    	int scInd;
    	int drInd;
    	double sigmas[] = new double[ndirs];
    	int ps[] = new int[1];
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
    	    if (domain == null) {
    	        if (scInd < 4) {
    	        	domain = "time";
    	        }
    	        else {
    	        	domain = "freq";
    	        }
    	    } // if (domain == null);
    	    for (drInd = 1; drInd <= ndirs; drInd++) {
    	        filInd++;
    	        sigmas[filInd-1] = sigmaX[filInd-1];
    	        
    	        if (domain.equals("freq")) {
    	           ps[0] = 3 * (int)Math.ceil(sigmas[filInd-1]);
    	           szfm = inputYDim + 2 * ps[0];
    	           szfn = inputXDim + 2 * ps[0];
    	           fm = new double[szfm];
    	           fn = new double[szfn];
    	           if ((szfm % 2) == 1) {
    	               for (i = 0; i < szfm; i++) {
    	            	   fm[i] = -1.0 + 1.0/szfm + (2.0*i)/szfm;
    	               }
    	           }
    	           else {
    	        	   for (i = 0; i < szfm; i++) {
    	            	   fm[i] = -1.0 + (2.0*i)/szfm;
    	               }   
    	           }
    	           if ((szfn % 2) == 1) {
    	               for (i = 0; i < szfn; i++) {
    	            	   fn[i] = -1.0 + 1.0/szfn + (2.0*i)/szfn;
    	               }
    	           }
    	           else {
    	        	   for (i = 0; i < szfn; i++) {
    	            	   fn[i] = -1.0 + (2.0*i)/szfn;
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
    	        } // if (domain.equals("freq"))
    	        else {
    	        	xfreq = null;
    	        	yfreq = null;
    	        	ps = null;
    	        }
    	        T1z2_get_filter_struct(omegas[filInd-1], amplitudes[filInd-1], filterAngle[filInd-1], sigmaX[filInd-1],
    	        		domain, xfreq, yfreq, ps);
    	    } // for (drInd = 1; drInd <= ndirs; drInd++)
    	} // for (scInd = 1; scInd <= nscales; scInd++) 
    	
    }
    
    private void T1z2_get_filter_struct(double omegas[], double amplitudes[], double filterAngle, double sigmaX,
    		String domain, double xfreq[][], double yfreq[][], int ps[]) {
    	double omegasOut[] = null;
    	double amplitudesOut[] = null;
    	double amplitudesOutImag[] = null;
    	// gaussian (constant basis)
    	T1z2a_convert_filter(omegasOut, amplitudesOut, amplitudesOutImag, omegas, amplitudes, filterAngle, sigmaX, 0);
    }
    
    private void T1z2a_convert_filter(double omegasOut[], double amplitudesOut[], double amplitudesOutImag[], 
    		double omegas[], double amplitudes[], double filterAngle,double sigmaX, int conj) {
        int sz0;
        double amplitudesOriginal[];
        int i;
        double omegas2[];
        double amplitudesEven[];
        double amplitudesOddImag[];
        double omegas3[];
        double amplitudesUns[] = null;
        double amplitudesUnsImag[] = null;
        double amplitudesMult[] = null;
        double amplitudesMultImag[];
        
        sz0 = amplitudes.length-1;
        
        amplitudesOriginal = new double[amplitudes.length + sz0];
        for (i = 0; i < amplitudes.length; i++) {
        	amplitudesOriginal[i] = amplitudes[i];
        }
        omegas2 = new double[2*omegas.length-1];
        for (i = 0; i < omegas.length; i++) {
        	omegas2[i] = omegas[i];
        }
        for (i = 0; i < omegas.length-1; i++) {
        	omegas2[omegas.length + i] = omegas[i+1] + omegas[omegas.length-1];
        }
        amplitudesEven = new double[2*amplitudesOriginal.length-1];
        for (i = 0; i < amplitudesOriginal.length-1; i++) {
        	amplitudesEven[i] = amplitudesOriginal[amplitudesOriginal.length-1-i];
        }
        for (i = 0; i < amplitudesOriginal.length; i++) {
        	amplitudesEven[i+amplitudesOriginal.length-1] = amplitudesOriginal[i];
        }
        amplitudesOddImag = new double[2*amplitudesOriginal.length-1];
        for (i = 0; i < amplitudesOriginal.length-1; i++) {
        	amplitudesOddImag[i] = -amplitudesOriginal[amplitudesOriginal.length-1-i];
        }
        for (i = 0; i < amplitudesOriginal.length; i++) {
        	amplitudesOddImag[i+amplitudesOriginal.length-1] = amplitudesOriginal[i];
        }
        omegas3 = new double[omegas2.length-1];
        for (i = 0; i < omegas2.length-1; i++) {
            omegas3[i] = -omegas2[omegas2.length-1-i];	
        }
        for (i = 0; i < omegas2.length; i++) {
        	omegas3[i + omegas2.length-1] = omegas2[i];
        }
        
        switch (conj) {
            case 0:
            	amplitudesUns = new double[amplitudesEven.length];
            	for (i = 0; i < amplitudesUns.length; i++) {
            		amplitudes[i] = amplitudesEven[i];
            	}
            	amplitudesMult = new double[amplitudesEven.length];
            	for (i = 0; i < amplitudesMult.length; i++) {
            		amplitudesMult[i] = amplitudesEven[i];
            	}
            	break;
            case 1:
            	amplitudesUns = new double[amplitudesEven.length];
            	for (i = 0; i < amplitudesUns.length; i++) {
            		amplitudes[i] = amplitudesEven[i];
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
            		amplitudes[i] = amplitudesEven[i];
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
               amplitudesOut = new double[]{2};
               omegasOut = new double[]{0};
               break;
            case 1:
            case 2:
               amplitudesOut = amplitudesUns;
               amplitudesOutImag = amplitudesUnsImag;
               break;
            default:
            	// Estimate the fourier series coefficients of filter^2 = convolution of 1-d filters
        } // switch (conj)
    }
    
    private void filterbank_DCA_2D(double omegas[][], double amplitudes[][], double filterAngle[], double sigmaX[],
    		int nscales, String filterType, int ndirs, double sig2omega, 
    		double radianStart, double radianEnd) {
    	// Put central frequencies on logarithmic scale
    	double factr;
    	double radianPerPixel[] = new double[nscales];
    	int i;
    	int scale;
    	double rpp;
    	double sigmaXTemp;
    	int angle;
    	int counter;
    	int numCounted = nscales * ndirs;
    	sigmaX = new double[numCounted];
    	filterAngle = new double[numCounted];
        double omegasInit[];
        omegas = new double[numCounted][];
        double amplitudesInit[];
        amplitudes = new double[numCounted][];
        double temp[];
        int dim;
        int dim2;
        int j;
        double val;
        int k;
        double sum;
    	
    	factr = Math.exp(Math.log(radianEnd/radianStart)/(nscales-1));
    	for (i = 0; i < nscales; i++) {
    		radianPerPixel[i] = radianStart * Math.pow(factr, i);
    	}
    	
    	for (scale = 1; scale <= nscales; scale++) {
    	    rpp = radianPerPixel[scale-1];
    	    sigmaXTemp = sig2omega/rpp;
    	    if (filterType.equals("edge")) {
    	    	rpp = rpp/4.0;
    	    }
    	    for (angle = 1; angle <= ndirs; angle++) {
    	        counter = (scale-1)*ndirs + angle;
    	        sigmaX[counter-1] = sigmaXTemp;
    	        filterAngle[counter-1] = Math.PI * (angle - 1)/ndirs;
    	        
    	        // Fields used for every single gabor filter
    	        if (filterType.equals("texture")) {
    	            omegas[counter-1] = new double[]{0, rpp};
    	            amplitudes[counter-1] = new double[]{0.0, 1.0};
    	        }
    	        else { // filterType.equals("edge"))
    	            dim = 0;
    	            for (val = 0; val <= 1.0; val += rpp) {
    	            	dim++;
    	            }
    	            temp = new double[dim];
    	            for (val = 0, j = 0; val <= 1.0; val += rpp) {
    	                temp[j++] = val;	
    	            }
    	            dim2 = 0;
    	            for (j = 1; j <= dim-1; j +=2) {
    	            	dim2++;
    	            }
    	            omegasInit = new double[dim2];
    	            for (j = 1, k = 0; j <= dim-1; j += 2) {
    	                omegasInit[k++] = temp[j];	
    	            }
    	            amplitudesInit = new double[dim2];
    	            for (j = 0; j < dim2; j++) {
    	            	amplitudesInit[j] = 1.0/Math.max(omegasInit[j], epsilon); 
    	            }
    	            omegas[counter-1] = new double[dim2+1];
    	            amplitudes[counter-1] = new double[dim2+1];
    	            sum = 0.0;
    	            for (j = 1; j < dim2 + 1; j++) {
    	            	omegas[counter-1][j] = omegasInit[j-1];
    	            	amplitudes[counter-1][j] = amplitudesInit[j-1];
    	            	sum += amplitudes[counter-1][j]*amplitudes[counter-1][j];
    	            }
    	            sum = Math.sqrt(sum);
    	            for (j = 1; j < dim2 + 1; j++) {
        	        	amplitudes[counter-1][j] = amplitudes[counter-1][j]/sum;
        	        }
    	        } // else
    	    } // for (angle = 1; angle <= ndirs; angle++)
    	} // for (scale = 1; scale <= nscales; scale++)
    }
}