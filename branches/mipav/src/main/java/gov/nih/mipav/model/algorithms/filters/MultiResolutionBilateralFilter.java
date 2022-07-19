package gov.nih.mipav.model.algorithms.filters;


import gov.nih.mipav.model.algorithms.*;
import gov.nih.mipav.model.algorithms.filters.PyWavelets.DiscreteWavelet;
import gov.nih.mipav.model.algorithms.filters.PyWavelets.MODE;
import gov.nih.mipav.model.algorithms.filters.PyWavelets.WAVELET_NAME;
import gov.nih.mipav.model.structures.*;

import gov.nih.mipav.view.*;

import java.io.*;
import java.util.Arrays;

    /**
     * This is a port of multi_bilateral.py by Yiqian Wang.
     * References:
     * 1.) Feature preserving image denoising with multiresolution filters by
     *     Yiqian Wang and You-Yi Jau.
     * 2.) D. L. Donoho and I. M. Johnstone. "Ideal spatial adaptation
       by wavelet shrinkage." Biometrika 81.3 (1994): 425-455.
       :DOI:10.1093/biomet/81.3.425
     */

    public class MultiResolutionBilateralFilter extends AlgorithmBase {
    	private final int BORDER_REFLECT_101 = 4; // gfedcb|abcdefgh|gfedcba
    	//The type of wavelet to perform. Can be any of the options
        //pywt.wavelist outputs. For example, this may be any of {db1, db2,
        // db3, db4, haar}
    	private PyWavelets.WAVELET_NAME wavelet_name = PyWavelets.WAVELET_NAME.DB;
    	private int wavelet_order = 8;
    	// The number of wavelet decomposition levels to use.  The default is
        // three less than the maximum number of possible decomposition levels
    	private int wavelet_levels = 4;
    	// Thresholding method to be used. The currently supported method is
        // "BayesShrink". 
    	private boolean estimateNoiseStandardDeviation = true;
    	private double noiseStandardDeviation;
    	// PyWavelets.FILTER_SOFT or PyWavelets.FILTER_HARD
    	// An optional argument to choose the type of denoising performed. It
        // noted that choosing soft thresholding given additive noise finds the
        // best approximation of the original image.
    	private int filterType = PyWavelets.FILTER_SOFT;
    	// Kernel diameter
    	private int d = 5;
    	private double sigmaColor = 0.5;
    	private double sigmaSpace = 1.8;
    	private double epsilon;
    	public MultiResolutionBilateralFilter(ModelImage destImg, ModelImage srcImg, PyWavelets.WAVELET_NAME wavelet_name, 
    			int wavelet_order, int wavelet_levels, boolean estimateNoiseStandardDeviation,
    			double noiseStandardDeviation, int filterType, int d, double sigmaColor, double sigmaSpace) {
    		 super(destImg, srcImg);
    		 this.wavelet_name = wavelet_name;
    		 this.wavelet_order = wavelet_order;
    		 this.wavelet_levels = wavelet_levels;
    		 this.estimateNoiseStandardDeviation = estimateNoiseStandardDeviation;
    		 this.noiseStandardDeviation = noiseStandardDeviation;
    		 this.filterType = filterType;
    		 this.d = d;
    		 this.sigmaColor = sigmaColor;
    		 this.sigmaSpace = sigmaSpace;
    	}
    	
    	public void runAlgorithm() {
    		if (srcImage.isColorImage()) {
    			denoiseColor();
    		}
    		else {
    			denoiseBW();
    		}
    	}
    	
    	public void denoiseBW() {
    	   // epsilon = D1MACH(4)
	       // Machine epsilon is the smallest positive epsilon such that
	       // (1.0 + epsilon) != 1.0.
	       // epsilon = 2**(1 - doubleDigits) = 2**(1 - 53) = 2**(-52)
	       // epsilon = 2.2204460e-16
	       // epsilon is called the largest relative spacing
    	   ModelImage outputImage;
    	   if (destImage != null) {
    		   outputImage = destImage;
    	   }
    	   else {
    		   outputImage = srcImage;
    	   }
	       epsilon = 1.0;
	       double neweps = 1.0;

	       while (true) {

	           if (1.0 == (1.0 + neweps)) {
	               break;
	           } else {
	               epsilon = neweps;
	               neweps = neweps / 2.0;
	           }
	       } // while(true)
    		PyWavelets py = new PyWavelets();
    		DiscreteWavelet w  = py.discrete_wavelet(wavelet_name, wavelet_order);
    		DiscreteWavelet wavelets[] = new DiscreteWavelet[]{w, w};
    		MODE modes[] = new MODE[]{MODE.MODE_SYMMETRIC, MODE.MODE_SYMMETRIC};
    		int axes[] = new int[]{0,1};
    		int xDim = srcImage.getExtents()[0];
    		int yDim = srcImage.getExtents()[1];
    		int length = xDim * yDim;
    		double buffer[] = null;
    		double buf2D[][] = null;
    		int i, x, y;	
    		buffer = new double[length];
    		int currentLevel;
		    try {
		    	srcImage.exportData(0, length, buffer);
		    }
		    catch (IOException e) {
		    	MipavUtil.displayError("IOException on srcImage.exportData(0, length, buffer)");
		    	setCompleted(false);
		    	return;
		    }
		    buf2D = new double[yDim][xDim];
		    for (y = 0; y < yDim; y++) {
		    	for (x = 0; x < xDim; x++) {
		    		buf2D[y][x] = buffer[x + y*xDim];
		    	}
		    }
		    double coeffs[][][] = py.wavedec2(buf2D, wavelets, modes, wavelet_levels, axes);
		    double LP[][] = coeffs[0];
		    
		    if (estimateNoiseStandardDeviation) {
		        double detail_coeffs[][] = coeffs[coeffs.length-1];
		        noiseStandardDeviation = _sigma_est_dwt(detail_coeffs, "Gaussian");
		    }
		    
		    double var = noiseStandardDeviation * noiseStandardDeviation;
		    
		    AlgorithmBilateralFilter bf = new AlgorithmBilateralFilter();
		    int borderType = BORDER_REFLECT_101;
		    double LP_bilateral[][];
		    for (currentLevel = 0; currentLevel < wavelet_levels; currentLevel++) {
		    	// --- denoise LP with bilateral
		        LP_bilateral = bf.bilateralFilter(LP, 1, d, sigmaColor, sigmaSpace, borderType);
		    	
		    	// --- denoise HP with thresholding
		    	double details[][][] = new double[3][][];
		    	double denoised_detail[][][] = new double[3][][];
		    	 for (i = 0; i < 3; i++) {
		    		 details[i] = coeffs[3*currentLevel + i + 1];
		    	     double threshold = _bayes_thresh(details[i], var);
		    	     if (filterType == PyWavelets.FILTER_HARD) {
		    	    	 denoised_detail[i] = hard(details[i], threshold, 0.0);
		    	     }
		    	     else {
		    	    	 denoised_detail[i] = soft(details[i], threshold, 0.0);
		    	     }
		    	 }
		    	 double coeffs_rec[][][] = new double[4][LP_bilateral.length][LP_bilateral[0].length];
		    	 coeffs_rec[0] = LP_bilateral;
		    	 for (i = 1; i < 4; i++) {
		    		 for (y = 0; y < LP_bilateral.length; y++) {
		    			 for (x = 0; x < LP_bilateral[0].length; x++) {
		    				 coeffs_rec[i][y][x] = denoised_detail[i-1][y][x];
		    			 }
		    		 }
		    	 }
		    	 LP = py.waverec2(coeffs_rec, wavelets, modes, axes);
		    } // for (currentLevel = 0; currentLevel < wavelet_levels; currentLevel++)
		    
		    double img_out[][] = bf.bilateralFilter(LP, 1, d, sigmaColor, sigmaSpace, borderType);
		    for (y = 0; y < yDim; y++) {
		    	for (x = 0; x < xDim; x++) {
		    		buffer[x + y*xDim] = img_out[y][x];
		    	}
		    }
		    
		    try {
		    	outputImage.importData(0, buffer, true);
		    }
		    catch (IOException e) {
		    	MipavUtil.displayError("IOException on outputImage.importData(0, buffer, true)");
		    	setCompleted(false);
		    	return;
		    }
		    
		    setCompleted(true);
		    return;
    	} // public void denoiseBW()
    	
    	public double[][] hard(double data[][], double value, double substitute) {
        	// default substitute = 0.0
        	double data2[][] = data.clone();
        	int i,j;
            for (i = 0; i < data2.length; i++) {
            	for (j = 0; j < data2[i].length; j++) {
    	            if (Math.abs(data2[i][j]) < value) {
    	            	data2[i][j] = substitute;
    	            }
            	}
            }
            return data2;
        }
    	
    	public double[][] soft(double data[][], double value, double substitute) {
        	// Default substitute = 0
        	int i, j;
        	double magnitude[][] = new double[data.length][];
        	double thresholded[][] = new double[data.length][];
        	for (i = 0; i < data.length; i++) {
        		magnitude[i] = new double[data[i].length];
        		thresholded[i] = new double[data[i].length];
        	}
        	for (i = 0; i < data.length; i++) {
        		for (j = 0; j < data[i].length; j++) {
    	    		if (data[i][j] == 0.0) {
    	    			thresholded[i][j] = 0.0;
    	    		}
    	    		else {
    	    		    magnitude[i][j] = Math.abs(data[i][j]);
    	    		    thresholded[i][j] = (1.0 - value/magnitude[i][j]);
    	    		    if (thresholded[i][j] < 0.0) {
    	    		    	thresholded[i][j] = 0.0;
    	    		    }
    	    		    thresholded[i][j] = data[i][j] * thresholded[i][j];
    	    		}
        		}
        	}
        	
        	if (substitute == 0) {
        		return thresholded;
        	}
        	else {
        		for (i = 0; i < thresholded.length; i++) {
        			for (j = 0; j < thresholded[i].length; j++) {
    	    			if (magnitude[i][j] < value) {
    	    				thresholded[i][j] = substitute;
    	    			}
        			}
        		}
        		return thresholded;
        	}
        }
    	
    	private double _bayes_thresh(double details[][], double var) {
    		// BayesShrink threshold for a zero-mean details coeff array.
    	    // Equivalent to:  dvar = np.var(details) for 0-mean details array	
    		int y, x;
    		int len = details.length * details[0].length;
    		double sum = 0.0;
    		for (y = 0; y < details.length; y++) {
    			for (x = 0; x < details[0].length; x++) {
    				sum += (details[y][x]*details[y][x]);
    			}
    		}
    		double dvar = sum/len;
    		double thresh = var / Math.sqrt(Math.max(dvar - var, epsilon));
    		return thresh;
    	}
    	
    	private double _sigma_est_dwt(double detail_coeffs[][], String distribution) {
    		int i,y,x;
    		if (!distribution.equalsIgnoreCase("Gaussian")) {
    			MipavUtil.displayError("_sigma_est_dwt only supports the Gaussian distribution");
    			return Double.NaN;
    		}
    		// Consider regions with detail coefficients exactly zero to be masked out
    		// Note that masking out of nonzero values is not done in nl_means_detail_wt_pgm.m
    		// in NLFMT Image Denoising codes
    		int numNonZero = 0;
    		for (y = 0; y < detail_coeffs.length; y++) {
    			for (x = 0; x < detail_coeffs[0].length; x++) {
    				if (detail_coeffs[y][x] != 0.0) {
    					numNonZero++;
    				}
    			}
    		}
    		double buffer[] = new double[numNonZero];
    		for (i = 0, y = 0; y < detail_coeffs.length; y++) {
    			for (x = 0; x < detail_coeffs[0].length; x++) {
    				if (detail_coeffs[y][x] != 0.0) {
    					buffer[i++] = Math.abs(detail_coeffs[y][x]);
    				}
    			}
    		}
    		// 75th quantile of the underlying, symmetric noise distribution
    		// denom = scipy.stats.norm.ppf(0.75)
    		double denom = 0.67448975;
    		Arrays.sort(buffer);
    		double median;
    		if ((buffer.length %2) == 0) {
			    median = (buffer[buffer.length/2] + buffer[buffer.length/2 - 1])/2.0; 
			}
			else {
				median = buffer[(buffer.length - 1)/2];
			}
    		double sigma = median/denom;
    		return sigma;
    	}
    	
    	public void denoiseColor() {
    		ModelImage outputImage;
     	   if (destImage != null) {
     		   outputImage = destImage;
     	   }
     	   else {
     		   outputImage = srcImage;
     	   }
    		// epsilon = D1MACH(4)
 	        // Machine epsilon is the smallest positive epsilon such that
 	        // (1.0 + epsilon) != 1.0.
 	        // epsilon = 2**(1 - doubleDigits) = 2**(1 - 53) = 2**(-52)
 	        // epsilon = 2.2204460e-16
 	        // epsilon is called the largest relative spacing
 	        epsilon = 1.0;
 	        double neweps = 1.0;

 	        while (true) {

 	            if (1.0 == (1.0 + neweps)) {
 	                break;
 	            } else {
 	                epsilon = neweps;
 	                neweps = neweps / 2.0;
 	            }
 	        } // while(true)
    		PyWavelets py = new PyWavelets();
    		DiscreteWavelet w  = py.discrete_wavelet(wavelet_name, wavelet_order);
    		DiscreteWavelet wavelets[] = new DiscreteWavelet[]{w, w};
    		MODE modes[] = new MODE[]{MODE.MODE_SYMMETRIC, MODE.MODE_SYMMETRIC};
    		int axes[] = new int[]{0,1};
    		int xDim = srcImage.getExtents()[0];
    		int yDim = srcImage.getExtents()[1];
    		int length = xDim * yDim;
    		float floatBuf[] = null;
    		double buf2D[][] = null;
    		int i, x, y, c;
    		double varR;
    		double varG;
    		double varB;
    		floatBuf = new float[length];
    		int currentLevel;
			try {
				srcImage.exportRGBData(1, 0, length, floatBuf);
			}
			catch (IOException e) {
		    	MipavUtil.displayError("IOException on srcImage.exportRGBData(1, 0, length, floatBuf)");
		    	setCompleted(false);
		    	return;
		    }
			buf2D = new double[yDim][xDim];
		    for (y = 0; y < yDim; y++) {
		    	for (x = 0; x < xDim; x++) {
		    		buf2D[y][x] = floatBuf[x + y*xDim];
		    	}
		    }
		    double coeffsR[][][] = py.wavedec2(buf2D, wavelets, modes, wavelet_levels, axes);
		    double LPR[][] = coeffsR[0];
		    
		    try {
				srcImage.exportRGBData(2, 0, length, floatBuf);
			}
			catch (IOException e) {
		    	MipavUtil.displayError("IOException on srcImage.exportRGBData(2, 0, length, floatBuf)");
		    	setCompleted(false);
		    	return;
		    }
		    for (y = 0; y < yDim; y++) {
		    	for (x = 0; x < xDim; x++) {
		    		buf2D[y][x] = floatBuf[x + y*xDim];
		    	}
		    }
		    double coeffsG[][][] = py.wavedec2(buf2D, wavelets, modes, wavelet_levels, axes);
		    double LPG[][] = coeffsG[0];
		    
		    try {
				srcImage.exportRGBData(3, 0, length, floatBuf);
			}
			catch (IOException e) {
		    	MipavUtil.displayError("IOException on srcImage.exportRGBData(3, 0, length, floatBuf)");
		    	setCompleted(false);
		    	return;
		    }
		    for (y = 0; y < yDim; y++) {
		    	for (x = 0; x < xDim; x++) {
		    		buf2D[y][x] = floatBuf[x + y*xDim];
		    	}
		    }
		    double coeffsB[][][] = py.wavedec2(buf2D, wavelets, modes, wavelet_levels, axes);
		    double LPB[][] = coeffsB[0];
		    
		    double LP[][] = new double[LPR.length][3*LPR[0].length];
		    for (y = 0; y < LPR.length; y++) {
		    	for (x = 0; x < LPR[0].length; x++) {
		    		LP[y][3*x] = LPR[y][x];
		    		LP[y][3*x+1] = LPG[y][x];
		    		LP[y][3*x+2] = LPB[y][x];
		    	}
		    }
		    
		    if (estimateNoiseStandardDeviation) {
		        double detail_coeffs[][] = coeffsR[coeffsR.length-1];
		        noiseStandardDeviation = _sigma_est_dwt(detail_coeffs, "Gaussian");
		        varR = noiseStandardDeviation * noiseStandardDeviation;
		        
		        detail_coeffs = coeffsG[coeffsG.length-1];
		        noiseStandardDeviation = _sigma_est_dwt(detail_coeffs, "Gaussian");
		        varG = noiseStandardDeviation * noiseStandardDeviation;
		        
		        detail_coeffs = coeffsB[coeffsB.length-1];
		        noiseStandardDeviation = _sigma_est_dwt(detail_coeffs, "Gaussian");
		        varB = noiseStandardDeviation * noiseStandardDeviation;
		    }
		    else {
		    	varR = noiseStandardDeviation * noiseStandardDeviation;
		    	varG = varR;
		    	varB = varR;
		    }
		    
		    AlgorithmBilateralFilter bf = new AlgorithmBilateralFilter();
		    int borderType = BORDER_REFLECT_101;
		    double LP_bilateral[][];
		    
		    for (currentLevel = 0; currentLevel < wavelet_levels; currentLevel++) {
		    	// --- denoise LP with bilateral
		        LP_bilateral = bf.bilateralFilter(LP, 1, d, sigmaColor, sigmaSpace, borderType);
		        
		        // --- denoise HP with thresholding
		    	double details[][][] = new double[3][][];
		    	double denoised_detail[][][] = new double[3][][];
		    	 for (i = 0; i < 3; i++) {
		    		 details[i] = coeffsR[3*currentLevel + i + 1];
		    	     double threshold = _bayes_thresh(details[i], varR);
		    	     if (filterType == PyWavelets.FILTER_HARD) {
		    	    	 denoised_detail[i] = hard(details[i], threshold, 0.0);
		    	     }
		    	     else {
		    	    	 denoised_detail[i] = soft(details[i], threshold, 0.0);
		    	     }
		    	 }
		    	 double coeffs_rec[][][] = new double[4][LP_bilateral.length][LP_bilateral[0].length/3];
		    	 for (y = 0; y < LP_bilateral.length; y++) {
	    			 for (x = 0; x < LP_bilateral[0].length/3; x++) {
	    				 coeffs_rec[0][y][x] = LP_bilateral[y][3*x];
	    			 }
	    		 }
		    	 for (i = 1; i < 4; i++) {
		    		 for (y = 0; y < LP_bilateral.length; y++) {
		    			 for (x = 0; x < LP_bilateral[0].length/3; x++) {
		    				 coeffs_rec[i][y][x] = denoised_detail[i-1][y][x];
		    			 }
		    		 }
		    	 }
		    	 LPR = py.waverec2(coeffs_rec, wavelets, modes, axes);
		    	 
		    	 for (i = 0; i < 3; i++) {
		    		 details[i] = coeffsG[3*currentLevel + i + 1];
		    	     double threshold = _bayes_thresh(details[i], varG);
		    	     if (filterType == PyWavelets.FILTER_HARD) {
		    	    	 denoised_detail[i] = hard(details[i], threshold, 0.0);
		    	     }
		    	     else {
		    	    	 denoised_detail[i] = soft(details[i], threshold, 0.0);
		    	     }
		    	 }
		  
		    	 for (y = 0; y < LP_bilateral.length; y++) {
	    			 for (x = 0; x < LP_bilateral[0].length/3; x++) {
	    				 coeffs_rec[0][y][x] = LP_bilateral[y][3*x+1];
	    			 }
	    		 }
		    	 for (i = 1; i < 4; i++) {
		    		 for (y = 0; y < LP_bilateral.length; y++) {
		    			 for (x = 0; x < LP_bilateral[0].length/3; x++) {
		    				 coeffs_rec[i][y][x] = denoised_detail[i-1][y][x];
		    			 }
		    		 }
		    	 }
		    	 LPG = py.waverec2(coeffs_rec, wavelets, modes, axes);
		    	 
		    	 for (i = 0; i < 3; i++) {
		    		 details[i] = coeffsB[3*currentLevel + i + 1];
		    	     double threshold = _bayes_thresh(details[i], varB);
		    	     if (filterType == PyWavelets.FILTER_HARD) {
		    	    	 denoised_detail[i] = hard(details[i], threshold, 0.0);
		    	     }
		    	     else {
		    	    	 denoised_detail[i] = soft(details[i], threshold, 0.0);
		    	     }
		    	 }
		  
		    	 for (y = 0; y < LP_bilateral.length; y++) {
	    			 for (x = 0; x < LP_bilateral[0].length/3; x++) {
	    				 coeffs_rec[0][y][x] = LP_bilateral[y][3*x+2];
	    			 }
	    		 }
		    	 for (i = 1; i < 4; i++) {
		    		 for (y = 0; y < LP_bilateral.length; y++) {
		    			 for (x = 0; x < LP_bilateral[0].length/3; x++) {
		    				 coeffs_rec[i][y][x] = denoised_detail[i-1][y][x];
		    			 }
		    		 }
		    	 }
		    	 LPB = py.waverec2(coeffs_rec, wavelets, modes, axes);
		    	 
		    	 LP = new double[LPR.length][3*LPR[0].length];
		    	 for (y = 0; y < LPR.length; y++) {
				    	for (x = 0; x < LPR[0].length; x++) {
				    		LP[y][3*x] = LPR[y][x];
				    		LP[y][3*x+1] = LPG[y][x];
				    		LP[y][3*x+2] = LPB[y][x];
				    	}
				    }
		    } // for (currentLevel = 0; currentLevel < wavelet_levels; currentLevel++)
		    
		    for (c = 1; c <= 3; c++) {
		        for (y = 0; y < yDim; y++) {
		        	for (x = 0; x < xDim; x++) {
		        		floatBuf[x + y*xDim] = (float)LP[y][3*x + c-1];
		        	}
		        }
		        try {
		        	outputImage.importRGBData(c, 0, floatBuf, false);
		        }
		        catch (IOException e) {
		        	MipavUtil.displayError("IOException on outputImage.importRGBData(c, 0, floatBuf, false)");
		        	setCompleted(false);
		        	return;
		        }
		    } // for (c = 1; c <= 3; c++)
		    
		    outputImage.calcMinMax();
		    
		    setCompleted(true);
		    return;
    	} // public void denoiseColor()
    }
