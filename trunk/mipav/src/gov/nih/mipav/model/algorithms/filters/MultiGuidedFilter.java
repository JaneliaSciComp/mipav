package gov.nih.mipav.model.algorithms.filters;


import java.io.IOException;
import java.util.Arrays;

import gov.nih.mipav.model.algorithms.*;
import gov.nih.mipav.model.algorithms.filters.PyWavelets.DiscreteWavelet;
import gov.nih.mipav.model.algorithms.filters.PyWavelets.MODE;
import gov.nih.mipav.model.structures.*;
import gov.nih.mipav.view.*;

/**
 * This is a port of multi_guided_est.py by Yiqian Wang.
 * References:
 * 1.) Feature preserving image denoising with multiresolution filters by
 *     Yiqian Wang and You-Yi Jau.
 * 2.) D. L. Donoho and I. M. Johnstone. "Ideal spatial adaptation
   by wavelet shrinkage." Biometrika 81.3 (1994): 425-455.
   :DOI:10.1093/biomet/81.3.425
   3.) Reference: J. Immerkaer, “Fast Noise Variance Estimation”, 
    Computer Vision and Image Understanding, 
    Vol. 64, No. 2, pp. 300-302, Sep. 1996 [PDF]
 */

public class MultiGuidedFilter extends AlgorithmBase {
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
	// Kernel radius
	private int d = 3;
	private double epsilon;
	
	public MultiGuidedFilter(ModelImage destImg, ModelImage srcImg, PyWavelets.WAVELET_NAME wavelet_name, 
			int wavelet_order, int wavelet_levels, boolean estimateNoiseStandardDeviation,
			double noiseStandardDeviation, int filterType, int d) {
		 super(destImg, srcImg);
		 this.wavelet_name = wavelet_name;
		 this.wavelet_order = wavelet_order;
		 this.wavelet_levels = wavelet_levels;
		 this.estimateNoiseStandardDeviation = estimateNoiseStandardDeviation;
		 this.noiseStandardDeviation = noiseStandardDeviation;
		 this.filterType = filterType;
		 this.d = d;
	}
	
	public void runAlgorithm() {
		if (srcImage.isColorImage()) {
			runColorImage();
		}
		else {
			runGrayImage();
		}
	}
	
	public void runGrayImage() {
		ModelImage outputImage;
 	    if (destImage != null) {
 		    outputImage = destImage;
 	    }
 	    else {
 		    outputImage = srcImage;
 	    }
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
	    
	    double LP_filter[][];
	    double im[];
	    double imin;
	    double imax;
	    double imrange;
	    ModelImage img;
	    ModelImage resultImage;
	    double sigmaColor;
	    AlgorithmGuidedFilter algoGuidedFilter;
	    int imextents[];
	    for (currentLevel = 0; currentLevel < wavelet_levels; currentLevel++) {
    		 double s = 2*estimate_noise_fast(LP);
    		 imin = Double.MAX_VALUE;
    		 imax = -Double.MAX_VALUE;
    		 for (y = 0; y < LP.length; y++) {
    			 for (x = 0; x < LP[0].length; x++) {
    				 if (LP[y][x] < imin) {
    					 imin = LP[y][x];
    				 }
    				 if (LP[y][x] > imax) {
    					 imax = LP[y][x];
    				 }
    			 }
    		 }
    		 im = new double[LP.length* LP[0].length];
    		 imrange = imax - imin;
    		 for (y = 0; y < LP.length; y++) {
    			 for (x = 0; x < LP[0].length; x++) {
    			     im[x+ y*LP[0].length] = 255.0*(LP[y][x] - imin)/imrange;
    			 }
    		 }
    		 imextents = new int[] {LP[0].length, LP.length};
    		 img = new ModelImage(ModelImage.DOUBLE, imextents, "img");
    		 resultImage = new ModelImage(ModelImage.DOUBLE, imextents, "resultImage");
    		 try {
    			 img.importData(0, im, true);
    		 }
    		 catch (IOException e) {
    			 MipavUtil.displayError("IOException on img.importData(0, im, true)");
    			 setCompleted(false);
    			 return;
    		 }
    		 sigmaColor = s*s; // Multiplication by 255*255 takes place in AlgorithmGuidedFilter
    		 algoGuidedFilter = new AlgorithmGuidedFilter(resultImage, img, img, d, sigmaColor);
    		 algoGuidedFilter.run();
    		 double im_out[] = new double[LP.length * LP[0].length];
    		 try {
    			 resultImage.exportData(0, LP.length * LP[0].length, im_out);
    		 }
    		 catch(IOException e) {
    			 MipavUtil.displayError("IOException on resultImage.exportData(0, LP.length * LP[0].length, im_out");
    			 setCompleted(false);
    			 return;
    		 }
    		 img.disposeLocal();
    		 resultImage.disposeLocal();
    		 LP_filter = new double[LP.length][LP[0].length];
    		 for (y = 0; y < LP.length; y++) {
    			 for (x = 0; x < LP[0].length; x++) {
    				 LP_filter[y][x] = (im_out[x + y*LP[0].length]/255.0)*imrange + imin;
    			 }
    		 }
    		 
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
	    	 double coeffs_rec[][][] = new double[4][LP_filter.length][LP_filter[0].length];
	    	 coeffs_rec[0] = LP_filter;
	    	 for (i = 1; i < 4; i++) {
	    		 for (y = 0; y < LP_filter.length; y++) {
	    			 for (x = 0; x < LP_filter[0].length; x++) {
	    				 coeffs_rec[i][y][x] = denoised_detail[i-1][y][x];
	    			 }
	    		 }
	    	 }
	    	 LP = py.waverec2(coeffs_rec, wavelets, modes, axes);
	    } // for (currentLevel = 0; currentLevel < wavelet_levels; currentLevel++)
	    imin = Double.MAX_VALUE;
		 imax = -Double.MAX_VALUE;
		 for (y = 0; y < LP.length; y++) {
			 for (x = 0; x < LP[0].length; x++) {
				 if (LP[y][x] < imin) {
					 imin = LP[y][x];
				 }
				 if (LP[y][x] > imax) {
					 imax = LP[y][x];
				 }
			 }
		 }
		 im = new double[LP.length* LP[0].length];
		 imrange = imax - imin;
		 for (y = 0; y < LP.length; y++) {
			 for (x = 0; x < LP[0].length; x++) {
			     im[x+ y*LP[0].length] = 255.0*(LP[y][x] - imin)/imrange;
			 }
		 }
		 imextents = new int[] {LP[0].length, LP.length};
		 img = new ModelImage(ModelImage.DOUBLE, imextents, "img");
		 resultImage = new ModelImage(ModelImage.DOUBLE, imextents, "resultImage");
		 try {
			 img.importData(0, im, true);
		 }
		 catch (IOException e) {
			 MipavUtil.displayError("IOException on img.importData(0, im, true)");
			 setCompleted(false);
			 return;
		 }
		 sigmaColor = 2500/(255.0*255.0); // Multiplication by 255*255 takes place in AlgorithmGuidedFilter
		 algoGuidedFilter = new AlgorithmGuidedFilter(resultImage, img, img, d, sigmaColor);
		 algoGuidedFilter.run();
		 double im_out[] = new double[LP.length * LP[0].length];
		 try {
			 resultImage.exportData(0, LP.length * LP[0].length, im_out);
		 }
		 catch(IOException e) {
			 MipavUtil.displayError("IOException on resultImage.exportData(0, LP.length * LP[0].length, im_out");
			 setCompleted(false);
			 return;
		 }
		 img.disposeLocal();
		 resultImage.disposeLocal();
	     for (i = 0; i < im_out.length; i++) {
		     im_out[i] = (im_out[i]/255.0)*imrange + imin;
		 }
	     try {
	    	 outputImage.importData(0, im_out, true);
	     }
	     catch (IOException e) {
	    	 MipavUtil.displayError("IOException on outputImage.importData(0, im_out, true)");
	    	 setCompleted(false);
	    	 return;
	     }
	     setCompleted(true);
	     return;
	}
	
	public void runColorImage() {
		ModelImage outputImage;
 	    if (destImage != null) {
 		    outputImage = destImage;
 	    }
 	    else {
 		    outputImage = srcImage;
 	    }
 	   PyWavelets py = new PyWavelets();
		DiscreteWavelet w  = py.discrete_wavelet(wavelet_name, wavelet_order);
		DiscreteWavelet wavelets[] = new DiscreteWavelet[]{w, w};
		MODE modes[] = new MODE[]{MODE.MODE_SYMMETRIC, MODE.MODE_SYMMETRIC};
		int axes[] = new int[]{0,1};
		int xDim = srcImage.getExtents()[0];
		int yDim = srcImage.getExtents()[1];
		int length = xDim * yDim;
		float buffer[] = null;
		double buf2D[][] = null;
		int i, x, y, c;	
		buffer = new float[length];
		double LP[][][] = new double[3][][];
		double LP_filter[][];
	    double im[];
	    double imin;
	    double imax;
	    double imrange;
	    ModelImage img;
	    ModelImage resultImage;
	    double sigmaColor;
	    AlgorithmGuidedFilter algoGuidedFilter;
	    int imextents[];
		for (c = 0; c < 3; c++) {
		int currentLevel;	
		try {
	    	srcImage.exportRGBData(c+1, 0, length, buffer);
	    }
	    catch (IOException e) {
	    	MipavUtil.displayError("IOException on srcImage.exportRGBData(" + (c+1) + ", 0, length, buffer)");
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
	    LP[c] = coeffs[0];
	    
	    if (estimateNoiseStandardDeviation) {
	        double detail_coeffs[][] = coeffs[coeffs.length-1];
	        noiseStandardDeviation = _sigma_est_dwt(detail_coeffs, "Gaussian");
	    }
	    
	    double var = noiseStandardDeviation * noiseStandardDeviation;
	    
	    
	    for (currentLevel = 0; currentLevel < wavelet_levels; currentLevel++) {
    		 double s = 2*estimate_noise_fast(LP[c]);
    		 imin = Double.MAX_VALUE;
    		 imax = -Double.MAX_VALUE;
    		 for (y = 0; y < LP[c].length; y++) {
    			 for (x = 0; x < LP[c][0].length; x++) {
    				 if (LP[c][y][x] < imin) {
    					 imin = LP[c][y][x];
    				 }
    				 if (LP[c][y][x] > imax) {
    					 imax = LP[c][y][x];
    				 }
    			 }
    		 }
    		 im = new double[LP[c].length* LP[c][0].length];
    		 imrange = imax - imin;
    		 for (y = 0; y < LP[c].length; y++) {
    			 for (x = 0; x < LP[c][0].length; x++) {
    			     im[x+ y*LP[c][0].length] = 255.0*(LP[c][y][x] - imin)/imrange;
    			 }
    		 }
    		 imextents = new int[] {LP[c][0].length, LP[c].length};
    		 img = new ModelImage(ModelImage.DOUBLE, imextents, "img");
    		 resultImage = new ModelImage(ModelImage.DOUBLE, imextents, "resultImage");
    		 try {
    			 img.importData(0, im, true);
    		 }
    		 catch (IOException e) {
    			 MipavUtil.displayError("IOException on img.importData(0, im, true)");
    			 setCompleted(false);
    			 return;
    		 }
    		 sigmaColor = s*s; // Multiplication by 255*255 takes place in AlgorithmGuidedFilter
    		 algoGuidedFilter = new AlgorithmGuidedFilter(resultImage, img, img, d, sigmaColor);
    		 algoGuidedFilter.run();
    		 double im_out[] = new double[LP[c].length * LP[c][0].length];
    		 try {
    			 resultImage.exportData(0, LP[c].length * LP[c][0].length, im_out);
    		 }
    		 catch(IOException e) {
    			 MipavUtil.displayError("IOException on resultImage.exportData(0, LP[c].length * LP[c][0].length, im_out");
    			 setCompleted(false);
    			 return;
    		 }
    		 img.disposeLocal();
    		 resultImage.disposeLocal();
    		 LP_filter = new double[LP[c].length][LP[c][0].length];
    		 for (y = 0; y < LP[c].length; y++) {
    			 for (x = 0; x < LP[c][0].length; x++) {
    				 LP_filter[y][x] = (im_out[x + y*LP[c][0].length]/255.0)*imrange + imin;
    			 }
    		 }
    		 
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
	    	 double coeffs_rec[][][] = new double[4][LP_filter.length][LP_filter[0].length];
	    	 coeffs_rec[0] = LP_filter;
	    	 for (i = 1; i < 4; i++) {
	    		 for (y = 0; y < LP_filter.length; y++) {
	    			 for (x = 0; x < LP_filter[0].length; x++) {
	    				 coeffs_rec[i][y][x] = denoised_detail[i-1][y][x];
	    			 }
	    		 }
	    	 }
	    	 LP[c] = py.waverec2(coeffs_rec, wavelets, modes, axes);
	    } // for (currentLevel = 0; currentLevel < wavelet_levels; currentLevel++)
		} // for (c = 0; c < 3; c++)
		imin = Double.MAX_VALUE;
		 imax = -Double.MAX_VALUE;
		 float imfloat[] = new float[LP[0].length* LP[0][0].length];
		 
		 imextents = new int[] {LP[0][0].length, LP[0].length};
		 img = new ModelImage(ModelImage.ARGB_FLOAT, imextents, "img");
		 resultImage = new ModelImage(ModelImage.ARGB_FLOAT, imextents, "resultImage");
		 for (c = 0; c < 3; c++) {
		 for (y = 0; y < LP[c].length; y++) {
			 for (x = 0; x < LP[c][0].length; x++) {
				 if (LP[c][y][x] < imin) {
					 imin = LP[c][y][x];
				 }
				 if (LP[c][y][x] > imax) {
					 imax = LP[c][y][x];
				 }
			 }
		 }
		 } // for (c = 0; c < 3; c++)
		 imrange = imax - imin;
		 
		 for (c = 0; c < 3; c++) {
		 for (y = 0; y < LP[c].length; y++) {
			 for (x = 0; x < LP[c][0].length; x++) {
			     imfloat[x+ y*LP[c][0].length] = (float)(255.0*(LP[c][y][x] - imin)/imrange);
			 }
		 }
		 
		 
		 try {
			 img.importRGBData(c+1, 0, imfloat, false);
		 }
		 catch (IOException e) {
			 MipavUtil.displayError("IOException on img.importRGBData(" + (c+1) + ", 0, imfloat, false)");
			 setCompleted(false);
			 return;
		 }
		 } // for (c = 0; c < 3; c++)
		 img.calcMinMax();
		 
		 sigmaColor = 2500/(255.0*255.0); // Multiplication by 255*255 takes place in AlgorithmGuidedFilter
		 algoGuidedFilter = new AlgorithmGuidedFilter(resultImage, img, img, d, sigmaColor);
		 algoGuidedFilter.run();
		 float im_out_float[] = new float[LP[0].length * LP[0][0].length];
		 img.disposeLocal();
		 for (c = 0; c < 3; c++) {
			 try {
				 resultImage.exportRGBData(c+1, 0, LP[c].length * LP[c][0].length, im_out_float);
			 }
			 catch(IOException e) {
				 MipavUtil.displayError("IOException on resultImage.exportRGBData(" + (c+1) + ", 0, LP[c].length * LP[c][0].length, im_out_float");
				 setCompleted(false);
				 return;
			 }
			 
		     for (i = 0; i < im_out_float.length; i++) {
			     im_out_float[i] = (float)((im_out_float[i]/255.0)*imrange + imin);
			 }
		     try {
		    	 outputImage.importRGBData(c+1, 0, im_out_float, false);
		     }
		     catch (IOException e) {
		    	 MipavUtil.displayError("IOException on outputImage.importRGBData(" + (c+1) + ", 0, im_out_float, false)");
		    	 setCompleted(false);
		    	 return;
		     }
		 } // for (c = 0; c < 3; c++)
		 
		 resultImage.disposeLocal();
		 outputImage.calcMinMax();
	     setCompleted(true);
	     return;
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
	
	public double estimate_noise_fast(double img[][]) {
		// Returns noise standard deviation
		double M[][] = new double[][] {{1, -2, 1},
	         {-2, 4, -2},
	         {1, -2, 1}};
	   //double bgr2k[] = new double[] {0.114, 0.587, 0.299};
	   int H = img.length;
	   int W = img[0].length;
	   double img_pad[][] = new double[H+2][W+2];
	   int y,x,i,j;
	   double sum;
	   double sigma = 0.0;
	   for (y = 0; y < H; y++) {
		   for (x = 0; x < W; x++) {
			   img_pad[y+1][x+1] = img[y][x];
		   }
	   }
	   for (y = 1; y < H+1; y++) {
		   for (x = 1; x < W+1; x++) {
			   sum = 0.0;
			   for (i = -1; i <= 1; i++) {
				   for (j = -1; j <= 1; j++) {
					   sum += img_pad[y + i][x+i]*M[i+1][j+1];
				   }
			   }
			   sigma += Math.abs(sum);
		   }
	   }
	   sigma = sigma * Math.sqrt(0.5 * Math.PI) / (6 * (W-2) * (H-2));
	   return sigma;
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
}