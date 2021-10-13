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
    			denoiseBW();
    		}
    		else {
    			denoiseColor();
    		}
    	}
    	
    	public void denoiseBW() {
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
    		int channels = 1;
    		int x, y;	
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
		    for (currentLevel = 0; currentLevel < wavelet_levels; currentLevel++) {
		    	// --- denoise LP with bilateral
		    	double LP_bilateral[][] = bf.bilateralFilter(LP, 1, d, sigmaColor, sigmaSpace, borderType);
		    	
		    	// --- denoise HP with thresholding
		    	double detail_coeffs[][] = coeffs[3*currentLevel + 3];
		    } // for (currentLevel = 0; currentLevel < wavelet_levels; currentLevel++)
    	} // public void denoiseBW()
    	
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
    		int channels = 3;
    		int x, y;
    		floatBuf = new float[length];
			try {
				srcImage.exportRGBData(1, 0, length, floatBuf);
			}
			catch (IOException e) {
		    	MipavUtil.displayError("IOException on srcImage.exportRGBData(1, 0, length, floatBuf)");
		    	setCompleted(false);
		    	return;
		    }
    	} // public void denoiseColor()
    }
