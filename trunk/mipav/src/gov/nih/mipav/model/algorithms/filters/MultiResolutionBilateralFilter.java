package gov.nih.mipav.model.algorithms.filters;


import gov.nih.mipav.model.algorithms.*;
import gov.nih.mipav.model.structures.*;

import gov.nih.mipav.view.*;

import java.io.*;

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
    	//The type of wavelet to perform. Can be any of the options
        //pywt.wavelist outputs. For example, this may be any of {db1, db2,
        // db3, db4, haar}
    	private PyWavelets.WAVELET_NAME wavelet_name = PyWavelets.WAVELET_NAME.DB;
    	private int wavelet_order = 8;
    	// The number of wavelet decomposition levels to use.  The default is
        // three less than the maximum number of possible decomposition levels
    	private int wavelet_levels = 4;
    	// Thresholding method to be used. The currently supported methods are
        // "BayesShrink" and "VisuShrink". 
    	private String threshold_type = "BayesShrink";
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
    			int wavelet_order, int wavelet_levels, String threshold_type, boolean estimateNoiseStandardDeviation,
    			double noiseStandardDeviation, int filterType, int d, double sigmaColor, double sigmaSpace) {
    		 super(destImg, srcImg);
    		 this.wavelet_name = wavelet_name;
    		 this.wavelet_order = wavelet_order;
    		 this.wavelet_levels = wavelet_levels;
    		 this.threshold_type = threshold_type;
    		 this.estimateNoiseStandardDeviation = estimateNoiseStandardDeviation;
    		 this.noiseStandardDeviation = noiseStandardDeviation;
    		 this.filterType = filterType;
    		 this.d = d;
    		 this.sigmaColor = sigmaColor;
    		 this.sigmaSpace = sigmaSpace;
    	}
    	
    	public void runAlgorithm() {
    		
    	}
    }
