package gov.nih.mipav.model.algorithms;

import java.io.IOException;

import gov.nih.mipav.model.file.FileIO;
import gov.nih.mipav.model.structures.ModelImage;
import gov.nih.mipav.model.structures.ModelStorageBase;
import gov.nih.mipav.view.MipavUtil;
import gov.nih.mipav.view.ViewUserInterface;

/**
 * 
 * @author aailb
 *MIT License
 *imageQualityIndex.m by Zhou Wang ported with his kind permission.
 *The original source code for imageQualityIndex.m is Copyright (c) 2001 The University of Texas at Austin
 *ssim_index.m by Zhou Wang ported with his kind permission.
 *The original source code for ssim.index.m is Copyright(c) 2003 Zhou Wang
 *Other metrics are from full_ref.py, no_ref.py and utils.py by Andrew Khalel.
  full_ref.py, no_ref.py and utils.py are Copyright (c) 2018 Andrew Khalel

Permission is hereby granted, free of charge, to any person obtaining a copy
of this software and associated documentation files (the "Software"), to deal
in the Software without restriction, including without limitation the rights
to use, copy, modify, merge, publish, distribute, sublicense, and/or sell
copies of the Software, and to permit persons to whom the Software is
furnished to do so, subject to the following conditions:

The above copyright notice and this permission notice shall be included in all
copies or substantial portions of the Software.

THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE
AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM,
OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE
SOFTWARE.

## Implemented metrics
- [x] Mean Squared Error (MSE) 
- [x] Root Mean Squared Error (RMSE)
- [x] Peak Signal-to-Noise Ratio (PSNR) [[1]](https://ieeexplore.ieee.org/abstract/document/1284395/)
- [x] Structural Similarity Index (SSIM) [[1]](https://ieeexplore.ieee.org/abstract/document/1284395/)
- [x] Universal Quality Image Index (UQI) [[2]](https://ieeexplore.ieee.org/document/995823/)
- [x] Multi-scale Structural Similarity Index (MS-SSIM) [[3]](https://ieeexplore.ieee.org/abstract/document/1292216/)
- [x] Erreur Relative Globale Adimensionnelle de Synthese (ERGAS) [[4]](https://hal.archives-ouvertes.fr/hal-00395027/)
- [x] Spatial Correlation Coefficient (SCC) [[5]](https://www.tandfonline.com/doi/abs/10.1080/014311698215973)
- [x] Relative Average Spectral Error (RASE) [[6]](https://ieeexplore.ieee.org/document/1304896/)
- [x] Spectral Angle Mapper (SAM) [[7]](https://ntrs.nasa.gov/search.jsp?R=19940012238)
- [x] Spectral Distortion Index (D_lambda) [[8]](https://www.ingentaconnect.com/content/asprs/pers/2008/00000074/00000002/art00003)
- [x] Spatial Distortion Index (D_S) [[8]](https://www.ingentaconnect.com/content/asprs/pers/2008/00000074/00000002/art00003)
- [x] Quality with No Reference (QNR) [[8]](https://www.ingentaconnect.com/content/asprs/pers/2008/00000074/00000002/art00003)
- [x] Visual Information Fidelity (VIF) [[9]](https://ieeexplore.ieee.org/abstract/document/1576816/)
- [x] Block Sensitive - Peak Signal-to-Noise Ratio (PSNR-B) [[10]](https://ieeexplore.ieee.org/abstract/document/5535179/)

Available metrics list

mse, rmse, psnr, rmse_sw, uqi, ssim, ergas, scc, rase, sam, msssim, vifp, psnrb 

## References
[1] "Image quality assessment: from error visibility to structural similarity." 2004)<br/>
[2] "A universal image quality index." (2002)<br/>
[3] "Multiscale structural similarity for image quality assessment." (2003)<br/>
[4] "Quality of high resolution synthesised images: Is there a simple criterion?." (2000)<br/>
[5] "A wavelet transform method to merge Landsat TM and SPOT panchromatic data." (1998)<br/>
[6] "Fusion of multispectral and panchromatic images using improved IHS and PCA mergers based on wavelet decomposition." (2004)<br/>
[7] "Discrimination among semi-arid landscape endmembers using the spectral angle mapper (SAM) algorithm." (1992)<br/>
[8] "Multispectral and panchromatic data fusion assessment without reference." (2008)<br/>
[9] "Image information and visual quality." (2006)<br/>
[10] "Quality Assessment of Deblocked Images" (2011)<br/>

 */

public class ImageQuality extends AlgorithmBase {
	
	private ViewUserInterface UI;
	private int metrics[];
	private double results[];
	private ModelImage referenceImage;
	private ModelImage testImage;
	public final int MEAN_SQUARED_ERROR = 1;
	public final int ROOT_MEAN_SQUARED_ERROR = 2;
	public final int PEAK_SIGNAL_TO_NOISE_RATIO = 3;
	public final int STRUCTURAL_SIMILARITY_INDEX = 4;
	public final int SSIM_WITH_AUTOMATIC_DOWNSAMPLING = 5;
	public final int UNIVERSAL_QUALITY_IMAGE_INDEX = 6;
	//public final int MULTI_SCALE_STRUCTURAL_SIMILARITY_INDEX;
	public final int ERGAS = 7;
	public final int SPATIAL_CORRELATION_COEFFICIENT = 8;
	public final int RELATIVE_AVERAGE_SPECTRAL_ERROR = 9;
	public final int SPECTRAL_ANGLE_MAPPER = 10;
	//public final int SPECTRAL_DISTORTION_INDEX = 10;
	//public final int SPATIAL_DISTORTION_INDEX = 11;
	//public final int QUALITY_WITH_NO_REFERENCE = 12;
	public final int VISUAL_INFORMATION_FIDELITY = 11;
 	public final int BLOCK_SENSITIVE_PEAK_SIGNAL_TO_NOISE_RATIO = 12;
 	public final int RMSE_SW = 13;
 	
 	private double testBuffer[] = null;
    private float testRedBuffer[] = null;
    private float testGreenBuffer[] = null;
    private float testBlueBuffer[] = null;
    private double referenceBuffer[] = null;
    private float referenceRedBuffer[] = null;
    private float referenceGreenBuffer[] = null;
    private float referenceBlueBuffer[] = null;
    private double testYBuffer[];
    private double referenceYBuffer[];
    private double rmse_sw_mean;
    private double universalImageQualityIndex;
    private double structuralSimilarityIndex;
    private double sam_mean;
    private double psnr_b;
    private int length = 1;
    //private boolean onlyTestImageRequired = false;
    private boolean YCrCbRequired = false;
    private boolean isColor = false;
    
    private double meanSquareError;
    private double meanRedSquareError;
    private double meanGreenSquareError;
    private double meanBlueSquareError;
    private double rootMeanSquareError;
    private double peakSignalToNoiseRatio;
    private double rmse_map[][] = null;
    private double rmse_red_map[][] = null;
    private double rmse_green_map[][] = null;
    private double rmse_blue_map[][] = null;
    private double ergas_mean;
    private double spatialCorrelationCoefficient;
    private double rase_mean;
    private double vifp_mean;
    
    private ModelImage gry = null;
    private ModelImage gry_noise = null;
    private ModelImage gry_const = null;
    private ModelImage clr = null;
    private ModelImage clr_noise = null;
    private ModelImage clr_const = null;
    
    private int nDims;
    private int xDim;
    private int yDim;
    // Window size
    // Default = 8 for RMSE_SW
    // Default = 8 for UNIVERSAL_QUALITY_IMAGE_INDEX
    // Default = 11 for STRUCTURAL_SIMILARITY_INDEX
    // Default = 11 for SSIM_WITH_AUTOMATIC_DOWNSAMPLING
    // Default = 8 for RELATIVE_AVERAGE_SPECTRAL_ERROR
    // Default = 8 for ERGAS
    private int ws;
    // First SSIM constant (default = 0.01)
    private double k1 = 0.01;
    // Second SSIM constant (default = 0.03)
    private double k2 = 0.03;
    // sigma used in STRUCTURAL_SIMILARITY_INDEX
    private double sigma = 1.5;
    // Ratio of high resolution to low resolution (default = 4.0) used in ERGAS
    private double r = 4.0;
    // high pass filter for spatial processing (default=[[-1,-1,-1],[-1,8,-1],[-1,-1,-1]]).
    // Used in SPATIAL_CORRELATION_COEFFICIENT
    private double win[][] = new double[][] {{-1,-1,-1},{-1,8,-1},{-1,-1,-1}};
    // variance of the visual noise (default = 2) used in VISUAL_INFORMATION_FIDELITY
    private double sigma_nsq = 2.0;
    
    public final int BORDER_CONSTANT = 0; // iiiiii|abcdefgh|iiiiiii with some specified i
	public final int BORDER_REPLICATE = 1; // aaaaaa|abcdefgh|hhhhhhh
	public final int BORDER_REFLECT = 2; // fedcba|abcdefgh|hgfedcb
	public final int BORDER_WRAP = 3; // cdefgh|abcdefgh|abcdefg
	public final int BORDER_REFLECT_101 = 4; // gfedcb|abcdefgh|gfedcba
	public final int BORDER_DEFAULT = BORDER_REFLECT_101;
	
	public ImageQuality() {
		UI = ViewUserInterface.getReference();
		String fileDir = "C:/Image Quality/sewar-master/sewar/tests/res/";
		final FileIO fileIO = new FileIO();
		fileIO.setQuiet(true);
    	fileIO.setSuppressProgressBar(true);
		gry = fileIO.readTiff("lena512gray.tiff", fileDir, true);
		gry_noise = fileIO.readTiff("lena512gray_noise.tiff", fileDir, true);
		gry_const = fileIO.readTiff("lena512gray_constant.tiff", fileDir, true);
		clr = fileIO.readTiff("lena512color.tiff", fileDir, true);
		clr_noise = fileIO.readTiff("lena512color_noise.tiff", fileDir, true);
		clr_const = fileIO.readTiff("lena512color_constant.tiff", fileDir, true);
	}
	
	public void testMse() {
		// All tests passed for meanSquareError
		int testsFailed = 0;
		double eps = 1.0E-3;
		metrics = new int[] {MEAN_SQUARED_ERROR};
		results = new double[1];
		ImageQuality iq = new ImageQuality(clr, clr, metrics,ws, k1, k2,sigma,r,win,sigma_nsq,results);
		iq.runAlgorithm();
		if (results[0] != 0.0) {
			System.err.println("Mean squared error = " + results[0] + " for clr, clr\n");
			testsFailed++;
		}
		
		iq = new ImageQuality(gry, gry, metrics, ws, k1,k2, sigma,r,win,sigma_nsq,results);
		iq.runAlgorithm();
		if (results[0] != 0.0) {
			System.err.println("Mean squared error = " + results[0] + " for gry, gry\n");
			testsFailed++;
		}
		
		iq = new ImageQuality(clr, clr_noise, metrics, ws, k1,k2,sigma,r,win,sigma_nsq,results);
		iq.runAlgorithm();
		if ((Math.abs(results[0] - 2391.465875)) > eps) {
			System.err.println("Mean squared error = " + results[0] + " for clr, clr_noise\n");
			testsFailed++;
		}
		
		iq = new ImageQuality(gry, gry_noise, metrics, ws, k1,k2,sigma,r,win,sigma_nsq,results);
		iq.runAlgorithm();
		if ((Math.abs(results[0] - 2025.913940)) > eps) {
			System.err.println("Mean squared error = " + results[0] + " for gry, gry_noise\n");
			testsFailed++;
		}
		
		iq = new ImageQuality(clr, clr_const, metrics, ws,k1,k2,sigma,r,win,sigma_nsq,results);
		iq.runAlgorithm();
		if ((Math.abs(results[0] - 2302.953958)) > eps) {
			System.err.println("Mean squared error = " + results[0] + " for clr, clr_const\n");
			testsFailed++;
		}
		
		iq = new ImageQuality(gry, gry_const, metrics, ws,k1,k2,sigma,r,win,sigma_nsq,results);
		iq.runAlgorithm();
		if ((Math.abs(results[0] - 2016.476768)) > eps) {
			System.err.println("Mean squared error = " + results[0] + " for gry, gry_const\n");
			testsFailed++;
		}
		
		if (testsFailed > 0) {
			System.err.println(testsFailed + " tests failed for mean squared error");
		}
		else {
			System.out.println("All tests passed for meanSquaredError");
		}
		gry.disposeLocal();
		gry = null;
		gry_noise.disposeLocal();
		gry_noise = null;
		gry_const.disposeLocal();
		gry_const = null;
		clr.disposeLocal();
		clr = null;
		clr_noise.disposeLocal();
		clr_noise = null;
		clr_const.disposeLocal();
		clr_const = null;
	}
	
	public void testRmse() {
		// All tests passed for rootMeanSquaredError
		int testsFailed = 0;
		double eps = 1.0E-3;
		metrics = new int[] {ROOT_MEAN_SQUARED_ERROR};
		results = new double[1];
		ImageQuality iq = new ImageQuality(clr, clr, metrics,ws,k1,k2,sigma,r,win,sigma_nsq,results);
		iq.runAlgorithm();
		if (results[0] != 0.0) {
			System.err.println("Root mean squared error = " + results[0] + " for clr, clr\n");
			testsFailed++;
		}
		
		iq = new ImageQuality(gry, gry, metrics, ws, k1,k2,sigma,r,win,sigma_nsq,results);
		iq.runAlgorithm();
		if (results[0] != 0.0) {
			System.err.println("Root mean squared error = " + results[0] + " for gry, gry\n");
			testsFailed++;
		}
		
		metrics = new int[] {RMSE_SW};
		ws = 8;
		iq = new ImageQuality(clr, clr, metrics,ws,k1,k2,sigma,r,win,sigma_nsq,results);
		iq.runAlgorithm();
		if (results[0] != 0.0) {
			System.err.println("Root mean squared error sliding window = " + results[0] + " for clr, clr\n");
			testsFailed++;
		}
		
		iq = new ImageQuality(gry, gry, metrics, ws, k1,k2,sigma,r,win,sigma_nsq,results);
		iq.runAlgorithm();
		if (results[0] != 0.0) {
			System.err.println("Root mean squared error sliding window = " + results[0] + " for gry, gry\n");
			testsFailed++;
		}
		
		metrics = new int[] {ROOT_MEAN_SQUARED_ERROR};
		iq = new ImageQuality(gry, gry_const, metrics, ws,k1,k2,sigma,r,win,sigma_nsq,results);
		iq.runAlgorithm();
		double rmse = results[0];
		metrics = new int[] {RMSE_SW};
		ws = 510;
		iq = new ImageQuality(gry, gry_const, metrics, ws,k1,k2,sigma,r,win,sigma_nsq,results);
		iq.runAlgorithm();
		double rmse_sw = results[0];
		if ((Math.abs(rmse - rmse_sw)) > eps) {
			System.err.println("Root mean squared error = " + rmse + " for gry, gry_const\n");
			System.err.println("Root mean squared error sliding window = " + rmse_sw + " for gry, gry_const\n");
			testsFailed++;
		}
		
		if (testsFailed > 0) {
			System.err.println(testsFailed + " tests failed for root mean squared error");
		}
		else {
			System.out.println("All tests passed for rootMeanSquaredError");
		}
		gry.disposeLocal();
		gry = null;
		gry_noise.disposeLocal();
		gry_noise = null;
		gry_const.disposeLocal();
		gry_const = null;
		clr.disposeLocal();
		clr = null;
		clr_noise.disposeLocal();
		clr_noise = null;
		clr_const.disposeLocal();
		clr_const = null;
	}
	
	public void testPsnr() {
		// All tests passed for peakSignalToNoiseRatio
		int testsFailed = 0;
		double eps = 1.0E-3;
		metrics = new int[] {PEAK_SIGNAL_TO_NOISE_RATIO};
		results = new double[1];
		ImageQuality iq = new ImageQuality(clr, clr, metrics,ws,k1,k2,sigma,r,win,sigma_nsq,results);
		iq.runAlgorithm();
		if (!Double.isInfinite(results[0])) {
			System.err.println("Peak signal to noise ratio = " + results[0] + " for clr, clr\n");
			testsFailed++;
		}
		
		iq = new ImageQuality(gry, gry, metrics, ws, k1,k2,sigma,r,win,sigma_nsq,results);
		iq.runAlgorithm();
		if (!Double.isInfinite(results[0])) {
			System.err.println("Peak signal to noise ratio = " + results[0] + " for gry, gry\n");
			testsFailed++;
		}
		
		iq = new ImageQuality(clr, clr_noise, metrics, ws, k1,k2,sigma,r,win,sigma_nsq,results);
		iq.runAlgorithm();
		if ((Math.abs(results[0] - 14.344162)) > eps) {
			System.err.println("Peak signal to noise ratio = " + results[0] + " for clr, clr_noise\n");
			testsFailed++;
		}
		
		iq = new ImageQuality(gry, gry_noise, metrics, ws, k1,k2,sigma,r,win,sigma_nsq,results);
		iq.runAlgorithm();
		if ((Math.abs(results[0] - 15.064594)) > eps) {
			System.err.println("Peak signal to noise ratio = " + results[0] + " for gry, gry_noise\n");
			testsFailed++;
		}
		
		iq = new ImageQuality(clr, clr_const, metrics, ws,k1,k2,sigma,r,win,sigma_nsq,results);
		iq.runAlgorithm();
		if ((Math.abs(results[0] - 14.507951)) > eps) {
			System.err.println("Peak signal to noise ratio = " + results[0] + " for clr, clr_const\n");
			testsFailed++;
		}
		
		iq = new ImageQuality(gry, gry_const, metrics, ws,k1,k2,sigma,r,win,sigma_nsq,results);
		iq.runAlgorithm();
		if ((Math.abs(results[0] - 15.084871)) > eps) {
			System.err.println("Peak signal to noise ratio = " + results[0] + " for gry, gry_const\n");
			testsFailed++;
		}
		
		if (testsFailed > 0) {
			System.err.println(testsFailed + " tests failed for peaks signal to noise ratio");
		}
		else {
			System.out.println("All tests passed for peakSignalToNoiseRatio");
		}
		gry.disposeLocal();
		gry = null;
		gry_noise.disposeLocal();
		gry_noise = null;
		gry_const.disposeLocal();
		gry_const = null;
		clr.disposeLocal();
		clr = null;
		clr_noise.disposeLocal();
		clr_noise = null;
		clr_const.disposeLocal();
		clr_const = null;
	}
	
	public void testErgas() {
		// All tests passed for ergas
		r = 4;
		ws = 8;
		int testsFailed = 0;
		double eps = 1.0E-3;
		metrics = new int[] {ERGAS};
		results = new double[1];
		ImageQuality iq = new ImageQuality(clr, clr, metrics,ws,k1,k2,sigma,r,win,sigma_nsq,results);
		iq.runAlgorithm();
		if (results[0] != 0) {
			System.err.println("ergas = " + results[0] + " for clr, clr\n");
			testsFailed++;
		}
		
		iq = new ImageQuality(gry, gry, metrics, ws, k1,k2,sigma,r,win,sigma_nsq,results);
		iq.runAlgorithm();
		if (results[0] != 0) {
			System.err.println("ergas = " + results[0] + " for gry, gry\n");
			testsFailed++;
		}
		if (testsFailed > 0) {
			System.err.println(testsFailed + " tests failed for ergas");
		}
		else {
			System.out.println("All tests passed for ergas");
		}
		gry.disposeLocal();
		gry = null;
		gry_noise.disposeLocal();
		gry_noise = null;
		gry_const.disposeLocal();
		gry_const = null;
		clr.disposeLocal();
		clr = null;
		clr_noise.disposeLocal();
		clr_noise = null;
		clr_const.disposeLocal();
		clr_const = null;
	}
	
	public void testScc() {
		// All tests passed for spatial correlation coefficient
		ws = 8;
		int testsFailed = 0;
		double eps = 1.0E-3;
		metrics = new int[] {SPATIAL_CORRELATION_COEFFICIENT};
		results = new double[1];
		ImageQuality iq = new ImageQuality(clr, clr, metrics,ws,k1,k2,sigma,r,win,sigma_nsq,results);
		iq.runAlgorithm();
		if (results[0] != 1.0) {
			System.err.println("Spatial correlation coefficient = " + results[0] + " for clr, clr\n");
			testsFailed++;
		}
		
		iq = new ImageQuality(gry, gry, metrics, ws, k1,k2,sigma,r,win,sigma_nsq,results);
		iq.runAlgorithm();
		if (results[0] != 1.0) {
			System.err.println("Spatial correlation coefficient = " + results[0] + " for gry, gry\n");
			testsFailed++;
		}
		
		if (testsFailed > 0) {
			System.err.println(testsFailed + " tests failed for spatial correlation coefficient");
		}
		else {
			System.out.println("All tests passed for spatial correlation coefficient");
		}
		gry.disposeLocal();
		gry = null;
		gry_noise.disposeLocal();
		gry_noise = null;
		gry_const.disposeLocal();
		gry_const = null;
		clr.disposeLocal();
		clr = null;
		clr_noise.disposeLocal();
		clr_noise = null;
		clr_const.disposeLocal();
		clr_const = null;
	}
	
	public void testRase() {
		// All tests passed for relative average spectral error
		ws = 8;
		int testsFailed = 0;
		double eps = 1.0E-3;
		metrics = new int[] {RELATIVE_AVERAGE_SPECTRAL_ERROR};
		results = new double[1];
		ImageQuality iq = new ImageQuality(clr, clr, metrics,ws,k1,k2,sigma,r,win,sigma_nsq,results);
		iq.runAlgorithm();
		if (results[0] != 0) {
			System.err.println("Relative average spectral error = " + results[0] + " for clr, clr\n");
			testsFailed++;
		}
		
		iq = new ImageQuality(gry, gry, metrics, ws, k1,k2,sigma,r,win,sigma_nsq,results);
		iq.runAlgorithm();
		if (results[0] != 0) {
			System.err.println("Relative average spectral error = " + results[0] + " for gry, gry\n");
			testsFailed++;
		}
		if (testsFailed > 0) {
			System.err.println(testsFailed + " tests failed for relative average spectral error");
		}
		else {
			System.out.println("All tests passed for relative average spectral error");
		}
		gry.disposeLocal();
		gry = null;
		gry_noise.disposeLocal();
		gry_noise = null;
		gry_const.disposeLocal();
		gry_const = null;
		clr.disposeLocal();
		clr = null;
		clr_noise.disposeLocal();
		clr_noise = null;
		clr_const.disposeLocal();
		clr_const = null;
	}
	
	public void testUqi() {
		// All tests passed for universal quality image index
		ws = 8;
		int testsFailed = 0;
		double eps = 1.0E-3;
		metrics = new int[] {UNIVERSAL_QUALITY_IMAGE_INDEX};
		results = new double[1];
		ImageQuality iq = new ImageQuality(clr, clr, metrics,ws,k1,k2,sigma,r,win,sigma_nsq,results);
		iq.runAlgorithm();
		if (results[0] != 1.0) {
			System.err.println("Universal quality image index = " + results[0] + " for clr, clr\n");
			testsFailed++;
		}
		
		iq = new ImageQuality(gry, gry, metrics, ws, k1,k2,sigma,r,win,sigma_nsq,results);
		iq.runAlgorithm();
		if (results[0] != 1.0) {
			System.err.println("Spatial Universal quality image index = " + results[0] + " for gry, gry\n");
			testsFailed++;
		}
		
		String fileDir = "C:/Image Quality/sewar-master/sewar/tests/res/";
		final FileIO fileIO = new FileIO();
		fileIO.setQuiet(true);
    	fileIO.setSuppressProgressBar(true);
    	// Lena examples from https://ece.uwaterloo.ca/~z70wang/research/quality_index/demo_lena.html
    	// Original MSE = 0, UQI = 1
		ModelImage gryA = fileIO.readJimi("lenaA.gif", fileDir, false);
		// Impulse salt pepper noise MSE = 225, Q = 0.6494
		ModelImage gryB = fileIO.readJimi("lenaB.gif", fileDir, false);
		iq = new ImageQuality(gryA, gryB, metrics, ws, k1,k2,sigma,r,win,sigma_nsq,results);
		// Running gave Universal quality image index = 0.6493759202505665, which matches 0.6494.
		iq.runAlgorithm();
		if (Math.abs(results[0] - 0.6494) > 1.0E-4) {
			if (results[0] != 1.0) {
				System.err.println("Spatial Universal quality image index = " + results[0] + " for gryA, gryB\n");
				testsFailed++;
			}	
		}
		// Additive Gaussian noise MSE = 225, Q = 0.3891
		ModelImage gryC = fileIO.readJimi("lenaC.gif", fileDir, false);
		iq = new ImageQuality(gryA, gryC, metrics, ws, k1,k2,sigma,r,win,sigma_nsq,results);
		// Running gave Universal quality image index = 0.3891114199004425, which matches 0.3891
		iq.runAlgorithm();
		if (Math.abs(results[0] - 0.3891) > 1.0E-4) {
			if (results[0] != 1.0) {
				System.err.println("Spatial Universal quality image index = " + results[0] + " for gryA, gryC\n");
				testsFailed++;
			}	
		}
		// Multiplicative speckle noise MSE = 225, Q = 0.4408
		ModelImage gryD = fileIO.readJimi("lenaD.gif", fileDir, false);
		iq = new ImageQuality(gryA, gryD, metrics, ws, k1,k2,sigma,r,win,sigma_nsq,results);
		// Running gave Universal quality image index = 0.4407595077383508, which matches 0.4408
		iq.runAlgorithm();
		if (Math.abs(results[0] - 0.4408) > 1.0E-4) {
			if (results[0] != 1.0) {
				System.err.println("Spatial Universal quality image index = " + results[0] + " for gryA, gryD\n");
				testsFailed++;
			}	
		}
		// Mean shift MSE = 225, Q = .9894
		ModelImage gryE = fileIO.readJimi("lenaE.gif", fileDir, false);
		iq = new ImageQuality(gryA, gryE, metrics, ws, k1,k2,sigma,r,win,sigma_nsq,results);
		// Running gave Universal quality image index = 0.9894240066005174, which matches 0.9894
		iq.runAlgorithm();
		if (Math.abs(results[0] - 0.9894) > 1.0E-4) {
			if (results[0] != 1.0) {
				System.err.println("Spatial Universal quality image index = " + results[0] + " for gryA, gryE\n");
				testsFailed++;
			}	
		}
		// Contrast shifting MSE = 225, Q = 0.9372
		ModelImage gryF = fileIO.readJimi("lenaF.gif", fileDir, false);
		iq = new ImageQuality(gryA, gryF, metrics, ws, k1,k2,sigma,r,win,sigma_nsq,results);
		// Running gave Universal quality image index = 0.9371941115064664, which matches 0.9372
		iq.runAlgorithm();
		if (Math.abs(results[0] - 0.9372) > 1.0E-4) {
			if (results[0] != 1.0) {
				System.err.println("Spatial Universal quality image index = " + results[0] + " for gryA, gryF\n");
				testsFailed++;
			}	
		}
		// Blurring MSE = 225, Q = 0.3461
		ModelImage gryG = fileIO.readJimi("lenaG.gif", fileDir, false);
		iq = new ImageQuality(gryA, gryG, metrics, ws, k1,k2,sigma,r,win,sigma_nsq,results);
		// Running gave Universal quality image index = 0.3461221853315234, which matches 0.3461
		iq.runAlgorithm();
		if (Math.abs(results[0] - 0.3461) > 1.0E-4) {
			if (results[0] != 1.0) {
				System.err.println("Spatial Universal quality image index = " + results[0] + " for gryA, gryG\n");
				testsFailed++;
			}	
		}
		// JPEG compression MSE = 215, Q = 0.2876
		ModelImage gryH = fileIO.readJimi("lenaH.gif", fileDir, false);
		iq = new ImageQuality(gryA, gryH, metrics, ws, k1,k2,sigma,r,win,sigma_nsq,results);
		// Running gave Universal quality image index = 0.28755478329174916, which matches 0.2876.
		iq.runAlgorithm();
		if (Math.abs(results[0] - 0.2876) > 1.0E-4) {
			if (results[0] != 1.0) {
				System.err.println("Spatial Universal quality image index = " + results[0] + " for gryA, gryH\n");
				testsFailed++;
			}	
		}
		if (testsFailed > 0) {
			System.err.println(testsFailed + " tests failed for universal quality image index");
		}
		else {
			System.out.println("All tests passed for universal quality image index");
		}
		gryA.disposeLocal();
		gryA = null;
		gryB.disposeLocal();
		gryB = null;
		gryC.disposeLocal();
		gryC = null;
		gryD.disposeLocal();
		gryD = null;
		gryE.disposeLocal();
		gryE = null;
		gryF.disposeLocal();
		gryF = null;
		gryG.disposeLocal();
		gryG = null;
		gryH.disposeLocal();
		gryH = null;
		gry.disposeLocal();
		gry = null;
		gry_noise.disposeLocal();
		gry_noise = null;
		gry_const.disposeLocal();
		gry_const = null;
		clr.disposeLocal();
		clr = null;
		clr_noise.disposeLocal();
		clr_noise = null;
		clr_const.disposeLocal();
		clr_const = null;
	}
	
	public void testSsim() {
		// All tests passed for structural similarity index
		ws = 11;
		int testsFailed = 0;
		double eps = 1.0E-3;
		metrics = new int[] {STRUCTURAL_SIMILARITY_INDEX};
		results = new double[1];
		ImageQuality iq = new ImageQuality(clr, clr, metrics,ws,k1,k2,sigma,r,win,sigma_nsq,results);
		iq.runAlgorithm();
		if (results[0] != 1.0) {
			System.err.println("Structural similarity index = " + results[0] + " for clr, clr\n");
			testsFailed++;
		}
		
		iq = new ImageQuality(gry, gry, metrics, ws, k1,k2,sigma,r,win,sigma_nsq,results);
		iq.runAlgorithm();
		if (results[0] != 1.0) {
			System.err.println("Structural similarity index = " + results[0] + " for gry, gry\n");
			testsFailed++;
		}
		
		if (testsFailed > 0) {
			System.err.println(testsFailed + " tests failed for structural similarity index");
		}
		else {
			System.out.println("All tests passed for structural similarity index");
		}
		
		gry.disposeLocal();
		gry = null;
		gry_noise.disposeLocal();
		gry_noise = null;
		gry_const.disposeLocal();
		gry_const = null;
		clr.disposeLocal();
		clr = null;
		clr_noise.disposeLocal();
		clr_noise = null;
		clr_const.disposeLocal();
		clr_const = null;
	}
	
	public void testSsimDownload() {
		// All tests passed for structural similarity index with automatic downloading
		ws = 11;
		int testsFailed = 0;
		double eps = 1.0E-3;
		metrics = new int[] {SSIM_WITH_AUTOMATIC_DOWNSAMPLING};
		results = new double[1];
		ImageQuality iq = new ImageQuality(clr, clr, metrics,ws,k1,k2,sigma,r,win,sigma_nsq,results);
		iq.runAlgorithm();
		if (results[0] != 1.0) {
			System.err.println("Structural similarity index with automatic downloading = " + results[0] + " for clr, clr\n");
			testsFailed++;
		}
		
		iq = new ImageQuality(gry, gry, metrics, ws, k1,k2,sigma,r,win,sigma_nsq,results);
		iq.runAlgorithm();
		if (results[0] != 1.0) {
			System.err.println("Structural similarity index with automatic downloading = " + results[0] + " for gry, gry\n");
			testsFailed++;
		}
		
		/*String fileDir = "C:/Image Quality/sewar-master/sewar/tests/res/";
		final FileIO fileIO = new FileIO();
		fileIO.setQuiet(true);
    	fileIO.setSuppressProgressBar(true);
    	// Einstein picture examples from https://ece.uwaterloo.ca/~z70wang/research/ssim/.
    	// Original image, MSE = 0, SSIM = 1
    	ModelImage gryA = fileIO.readJimi("EinsteinA.jpg", fileDir, false);
    	// MSE = 144, SSIM = 0.988
    	ModelImage gryB = fileIO.readJimi("EinsteinB.jpg", fileDir, false);
    	iq = new ImageQuality(gryA, gryB, metrics, ws, k1,k2,sigma,r,win,sigma_nsq,results);
    	iq.runAlgorithm();*/

		
		if (testsFailed > 0) {
			System.err.println(testsFailed + " tests failed for structural similarity index with automatic downloading");
		}
		else {
			System.out.println("All tests passed for structural similarity index with automatic downloading");
		}
		
		/*gryA.disposeLocal();
		gryA = null;
		gryB.disposeLocal();
		gryB = null;*/
		gry.disposeLocal();
		gry = null;
		gry_noise.disposeLocal();
		gry_noise = null;
		gry_const.disposeLocal();
		gry_const = null;
		clr.disposeLocal();
		clr = null;
		clr_noise.disposeLocal();
		clr_noise = null;
		clr_const.disposeLocal();
		clr_const = null;
	}

	public void testSam() {
		// All tests passed for spectral angle mapper
		int testsFailed = 0;
		double eps = 1.0E-3;
		metrics = new int[] {SPECTRAL_ANGLE_MAPPER};
		results = new double[1];
		ImageQuality iq = new ImageQuality(clr, clr, metrics,ws,k1,k2,sigma,r,win,sigma_nsq,results);
		iq.runAlgorithm();
		if (Math.abs(results[0]) >= eps) {
			System.err.println("Spectral angle mapper = " + results[0] + " for clr, clr\n");
			testsFailed++;
		}
		
		iq = new ImageQuality(gry, gry, metrics, ws, k1,k2,sigma,r,win,sigma_nsq,results);
		iq.runAlgorithm();
		if (Math.abs(results[0]) >= eps) {
			System.err.println("Spectral angle mapper = " + results[0] + " for gry, gry\n");
			testsFailed++;
		}
		
		if (testsFailed > 0) {
			System.err.println(testsFailed + " tests failed for spectral angle mapper");
		}
		else {
			System.out.println("All tests passed for spectral angle mapper");
		}
		
		gry.disposeLocal();
		gry = null;
		gry_noise.disposeLocal();
		gry_noise = null;
		gry_const.disposeLocal();
		gry_const = null;
		clr.disposeLocal();
		clr = null;
		clr_noise.disposeLocal();
		clr_noise = null;
		clr_const.disposeLocal();
		clr_const = null;
	}
	
	public void testPSNRB() {
		// gry passes, clr does not.
		int testsFailed = 0;
		double eps = 1.0E-3;
		metrics = new int[] {BLOCK_SENSITIVE_PEAK_SIGNAL_TO_NOISE_RATIO};
		results = new double[1];
		ImageQuality iq = new ImageQuality(gry, gry_noise, metrics,ws,k1,k2,sigma,r,win,sigma_nsq,results);
		iq.runAlgorithm();
		if (Math.abs(15.0646-results[0]) >= eps) {
			System.err.println("Block sensitive peak signal to noise ratio = " + results[0] + " for gry, gry_noise\n");
			testsFailed++;
		}
		
		iq = new ImageQuality(clr, clr_noise, metrics, ws, k1,k2,sigma,r,win,sigma_nsq,results);
		iq.runAlgorithm();
		// Value computed for psnrb using MATLAB for the first channel of the image. 
		// There could be discrepancy in the results compared to MATLAB when using the Y channel of the image, due to scaling issues in Python vs MATLAB 
		if (Math.abs(14.5881-results[0]) >= eps) {
			System.err.println("Block sensitive peak signal to noise ratio = " + results[0] + " for clr, clr_noise\n");
			testsFailed++;
		}
		
		if (testsFailed > 0) {
			System.err.println(testsFailed + " tests failed for block sensitive peak signal to noise ratio");
		}
		else {
			System.out.println("All tests passed for block sensitive peak signal to noise ratio");
		}
		
		gry.disposeLocal();
		gry = null;
		gry_noise.disposeLocal();
		gry_noise = null;
		gry_const.disposeLocal();
		gry_const = null;
		clr.disposeLocal();
		clr = null;
		clr_noise.disposeLocal();
		clr_noise = null;
		clr_const.disposeLocal();
		clr_const = null;	
	}
	
	public void testVIF() {
		// All tests passed for visual information fidelity
		int testsFailed = 0;
		double eps = 1.0E-3;
		metrics = new int[] {VISUAL_INFORMATION_FIDELITY};
		results = new double[1];
		ImageQuality iq = new ImageQuality(clr, clr, metrics,ws,k1,k2,sigma,r,win,sigma_nsq,results);
		iq.runAlgorithm();
		if (Math.abs(1.0-results[0]) >= eps) {
			System.err.println("Visual information fidelity = " + results[0] + " for clr, clr\n");
			testsFailed++;
		}
		
		iq = new ImageQuality(gry, gry, metrics, ws, k1,k2,sigma,r,win,sigma_nsq,results);
		iq.runAlgorithm();
		if (Math.abs(1.0-results[0]) >= eps) {
			System.err.println("Visual information fidelity = " + results[0] + " for gry, gry\n");
			testsFailed++;
		}
		
		iq = new ImageQuality(gry, gry_noise, metrics, ws, k1,k2,sigma,r,win,sigma_nsq,results);
		iq.runAlgorithm();
		if (Math.abs(0.120490551257006-results[0]) >= eps) {
			System.err.println("Visual information fidelity = " + results[0] + " for gry, gry_noise\n");
			testsFailed++;
		}
		
		iq = new ImageQuality(gry, gry_const, metrics, ws, k1,k2,sigma,r,win,sigma_nsq,results);
		iq.runAlgorithm();
		if (Math.abs(0.981413452665522-results[0]) >= eps) {
			System.err.println("Visual information fidelity = " + results[0] + " for gry, gry_const\n");
			testsFailed++;
		}
		
		if (testsFailed > 0) {
			System.err.println(testsFailed + " tests failed for visual information fidelity");
		}
		else {
			System.out.println("All tests passed for visual information fidelity");
		}
		
		gry.disposeLocal();
		gry = null;
		gry_noise.disposeLocal();
		gry_noise = null;
		gry_const.disposeLocal();
		gry_const = null;
		clr.disposeLocal();
		clr = null;
		clr_noise.disposeLocal();
		clr_noise = null;
		clr_const.disposeLocal();
		clr_const = null;	
	}
	
	public ImageQuality(ModelImage referenceImage, ModelImage testImage, int metrics[], int ws,
			double k1, double k2, double sigma, double r, double win[][], double sigma_nsq, 
			double results[]) {
		if (metrics == null) {
			MipavUtil.displayError("metrics is null in ImageQuality");
			return;
		}
		if (metrics.length == 0) {
			MipavUtil.displayError("metrics.length == 0 in ImageQuality");
			return;
		}
		if (metrics.length > 13) {
			MipavUtil.displayError("metrics.length > 13 in ImageQuality");
			return;
		}
		if (results == null) {
			MipavUtil.displayError("results == null in ImageQuality");
			return;
		}
		if (results.length != metrics.length) {
			MipavUtil.displayError("results.length != metrics.length in ImageQuality");
			return;
		}
		if (testImage == null) {
			MipavUtil.displayError("testImage is null in ImageQuality");
			return;
		}
		isColor = testImage.isColorImage();
		for (int i = 0; i < metrics.length; i++) {
			if ((metrics[i] < 1) || (metrics[i] > 13)) {
				MipavUtil.displayError("Illegal metrics[" + i+ "] in ImageQuality");
				return;
			}
			if (isColor && (metrics[i] == BLOCK_SENSITIVE_PEAK_SIGNAL_TO_NOISE_RATIO)) {
				YCrCbRequired = true;
			}
			if (metrics[i] == RMSE_SW) {
				if (ws < 1) {
		    		 System.err.println("ws < 1 for rmse_sw");
		    		 setCompleted(false);
		    		 return;
		    	 }
			}
		}
		this.metrics = metrics;
		this.results = results;
		
		this.testImage = testImage;
    	if (referenceImage == null) {
			MipavUtil.displayError("referenceImage is null in ImageQuality");
			return;
		}
    	if (referenceImage.isColorImage() && (!testImage.isColorImage())) {
    		MipavUtil.displayError("referenceImage is color but test image is not color in ImageQuality");
    		return;
    	}
    	if ((!referenceImage.isColorImage()) && testImage.isColorImage()) {
    		MipavUtil.displayError("referenceImage is not color but test image is color in ImageQuality");
    		return;
    	}
    	if (referenceImage.getNDims() != testImage.getNDims()) {
    		MipavUtil.displayError("referenceImage has " + referenceImage.getNDims() + " dimensions but testImage has " +
    	    testImage.getNDims() + " dimensions in ImageQuality");
    		return;
    	}
    	for (int i = 0; i < referenceImage.getNDims(); i++) {
    		if (referenceImage.getExtents()[i] != testImage.getExtents()[i]) {
    			MipavUtil.displayError("For dimension " + (i+1) + " referenceImage length = " + referenceImage.getExtents()[i] +
    					" but testImage length = " + testImage.getExtents()[i] + " in ImageQuality");
    			return;
    		}
    	}
    	referenceImage.calcMinMax();
    	this.referenceImage = referenceImage;
    	this.ws = ws;
    	this.k1 = k1;
    	this.k2 = k2;
    	this.sigma = sigma;
    	this.r = r;
    	this.win = win;
    	this.sigma_nsq = sigma_nsq;
	}
    
    public void runAlgorithm() {
    	int i;
    	UI = ViewUserInterface.getReference();
	    nDims = testImage.getNDims();
	    xDim = testImage.getExtents()[0];
	    yDim = testImage.getExtents()[1];
	    
	    for (i = 0; i < nDims; i++) {
	    	length *= testImage.getExtents()[i];
	    }
	    if (!isColor) {
	        testBuffer = new double[length];
	        try {
	        	testImage.exportData(0, length, testBuffer);
	        }
	        catch (IOException e) {
	        	MipavUtil.displayError("IOException on testImage.exportData(0, length, testBuffer in ImageQuality");
	        	setCompleted(false);
	        	return;
	        }
	        
	        referenceBuffer = new double[length];
	        try {
	        	referenceImage.exportData(0, length, referenceBuffer);
	        }
	        catch (IOException e) {
	        	MipavUtil.displayError("IOException on referenceImage.exportData(0, length, referenceBuffer in ImageQuality");
	        	setCompleted(false);
	        	return;
	        }
	    }
	    else {
	        testRedBuffer = new float[length];
	        try {
	        	testImage.exportRGBData(1, 0, length, testRedBuffer);
	        }
	        catch (IOException e) {
	        	MipavUtil.displayError("IOException on testImage.exportRGBData(1, 0, length, testRedBuffer");
	        	setCompleted(false);
	        	return;
	        }
	        
	        testGreenBuffer = new float[length];
	        try {
	        	testImage.exportRGBData(2, 0, length, testGreenBuffer);
	        }
	        catch (IOException e) {
	        	MipavUtil.displayError("IOException on testImage.exportRGBData(2, 0, length, testGreenBuffer");
	        	setCompleted(false);
	        	return;
	        }
	        
	        testBlueBuffer = new float[length];
	        try {
	        	testImage.exportRGBData(3, 0, length, testBlueBuffer);
	        }
	        catch (IOException e) {
	        	MipavUtil.displayError("IOException on testImage.exportRGBData(3, 0, length, testBlueBuffer");
	        	setCompleted(false);
	        	return;
	        }
	        
	        referenceRedBuffer = new float[length];
	        try {
	        	referenceImage.exportRGBData(1, 0, length, referenceRedBuffer);
	        }
	        catch (IOException e) {
	        	MipavUtil.displayError("IOException on referenceImage.exportRGBData(1, 0, length, referenceRedBuffer");
	        	setCompleted(false);
	        	return;
	        }
	        
	        referenceGreenBuffer = new float[length];
	        try {
	        	referenceImage.exportRGBData(2, 0, length, referenceGreenBuffer);
	        }
	        catch (IOException e) {
	        	MipavUtil.displayError("IOException on referenceImage.exportRGBData(2, 0, length, referenceGreenBuffer");
	        	setCompleted(false);
	        	return;
	        }
	        
	        referenceBlueBuffer = new float[length];
	        try {
	        	referenceImage.exportRGBData(3, 0, length, referenceBlueBuffer);
	        }
	        catch (IOException e) {
	        	MipavUtil.displayError("IOException on referenceImage.exportRGBData(3, 0, length, referenceBlueBuffer");
	        	setCompleted(false);
	        	return;
	        }
	    } // else isColor	
	    
	    if (YCrCbRequired) {
            testYBuffer = new double[length];
            referenceYBuffer = new double[length];
            double range;
            if (referenceImage.getType() == ModelStorageBase.ARGB) {
            	range = 255.0;
            }
            else {
            	range = referenceImage.getMax() - referenceImage.getMin();
            }
            for (i = 0; i < length; i++) {
            	testYBuffer[i] = (0.299*testRedBuffer[i] + 0.587*testGreenBuffer[i] + 0.114*testBlueBuffer[i])/range;
            	referenceYBuffer[i] = (0.299*referenceRedBuffer[i] + 0.587*referenceGreenBuffer[i] + 0.114*referenceBlueBuffer[i])/range;
            }
        } // if (YCrCbRequired)
	    
	    for (i = 0; i < metrics.length; i++) {
		    switch(metrics[i]) {
		    case MEAN_SQUARED_ERROR:
		    	mse();
		    	results[i] = meanSquareError;
		    	break;
		    case ROOT_MEAN_SQUARED_ERROR:
		    	rmse();
		    	results[i] = rootMeanSquareError;
		    	break;
		    case PEAK_SIGNAL_TO_NOISE_RATIO:
		    	psnr();
		    	results[i] = peakSignalToNoiseRatio;
		    	break;
		    case STRUCTURAL_SIMILARITY_INDEX:
		    	ssim(false);
		    	results[i] = structuralSimilarityIndex;
		    	break;
		    case SSIM_WITH_AUTOMATIC_DOWNSAMPLING:
		    	ssim(true);
		    	results[i] = structuralSimilarityIndex;
		    	break;
		    case UNIVERSAL_QUALITY_IMAGE_INDEX:
		    	uqi();
		    	results[i] = universalImageQualityIndex;
		    	break;
		    //case MULTI_SCALE_STRUCTURAL_SIMILARITY_INDEX:
		    	//break;
		    case ERGAS:
		    	ergas();
		    	results[i] = ergas_mean;
		    	break;
		    case SPATIAL_CORRELATION_COEFFICIENT:
		    	scc();
		    	results[i] = spatialCorrelationCoefficient;
		    	break;
		    case RELATIVE_AVERAGE_SPECTRAL_ERROR:
		    	rase();
		    	results[i] = rase_mean;
		    	break;
		    case SPECTRAL_ANGLE_MAPPER:
		    	sam();
		    	results[i] = sam_mean;
		    	break;
		    //case SPECTRAL_DISTORTION_INDEX:
		    	//break;
		    //case SPATIAL_DISTORTION_INDEX:
		    	//break;
		    //case QUALITY_WITH_NO_REFERENCE:
		    	//break;
		    case VISUAL_INFORMATION_FIDELITY:
		    	vifp();
		    	results[i] = vifp_mean;
		    	break;
		    case BLOCK_SENSITIVE_PEAK_SIGNAL_TO_NOISE_RATIO:
		    	psnrb();
		    	results[i] = psnr_b;
		    	break;
		    case RMSE_SW:
		    	 rmse_sw();
		    	 results[i] = rmse_sw_mean;
		         break;
		    } // switch(metrics[i])
	    } // for (i = 0; i < metrics.length; i++)
	    setCompleted(true);
	    return;
	}
    
    private void mse() {
    	int i;
    	double diff;
    	double totalSquareDiff = 0.0;
    	double totalRedSquareDiff = 0.0;
    	double totalGreenSquareDiff = 0.0;
    	double totalBlueSquareDiff = 0.0;
    	if (!isColor) {
    	    for (i = 0; i < length; i++) {
    	        diff = 	referenceBuffer[i] - testBuffer[i];
    	        totalSquareDiff += diff*diff;
    	    }
    	    meanSquareError = totalSquareDiff/length;
    	}
    	else {
    		for (i = 0; i < length; i++) {
    	        diff = 	referenceRedBuffer[i] - testRedBuffer[i];
    	        totalRedSquareDiff += diff*diff;
    	        diff = 	referenceGreenBuffer[i] - testGreenBuffer[i];
    	        totalGreenSquareDiff += diff*diff;
    	        diff = 	referenceBlueBuffer[i] - testBlueBuffer[i];
    	        totalBlueSquareDiff += diff*diff;
    	    } 
    		totalSquareDiff = totalRedSquareDiff + totalGreenSquareDiff + totalBlueSquareDiff;
    		meanRedSquareError = totalRedSquareDiff/length;
    		meanGreenSquareError = totalGreenSquareDiff/length;
    		meanBlueSquareError = totalBlueSquareDiff/length;
    		meanSquareError = totalSquareDiff/(3.0*length);
    		UI.setDataText("Red mean squared error = " + meanRedSquareError + "\n");
    	    System.out.println("Red mean squared error = " + meanRedSquareError);
    	    UI.setDataText("Green mean squared error = " + meanGreenSquareError + "\n");
    	    System.out.println("Green mean squared error = " + meanGreenSquareError);
    	    UI.setDataText("Blue mean squared error = " + meanBlueSquareError + "\n");
    	    System.out.println("Blue mean squared error = " + meanBlueSquareError);
    	}
    	UI.setDataText("Mean squared error = " + meanSquareError + "\n");
	    System.out.println("Mean squared error = " + meanSquareError);
    }
    
    private void rmse() {
    	int i;
    	double diff;
    	double totalSquareDiff = 0.0;
    	double totalRedSquareDiff = 0.0;
    	double totalGreenSquareDiff = 0.0;
    	double totalBlueSquareDiff = 0.0;
    	double rootMeanRedSquareError;
        double rootMeanGreenSquareError;
        double rootMeanBlueSquareError;
    	if (!isColor) {
    	    for (i = 0; i < length; i++) {
    	        diff = 	referenceBuffer[i] - testBuffer[i];
    	        totalSquareDiff += diff*diff;
    	    }
    	    rootMeanSquareError = Math.sqrt(totalSquareDiff/length);
    	}
    	else {
    		for (i = 0; i < length; i++) {
    	        diff = 	referenceRedBuffer[i] - testRedBuffer[i];
    	        totalRedSquareDiff += diff*diff;
    	        diff = 	referenceGreenBuffer[i] - testGreenBuffer[i];
    	        totalGreenSquareDiff += diff*diff;
    	        diff = 	referenceBlueBuffer[i] - testBlueBuffer[i];
    	        totalBlueSquareDiff += diff*diff;
    	    } 
    		totalSquareDiff = totalRedSquareDiff + totalGreenSquareDiff + totalBlueSquareDiff;
    		rootMeanRedSquareError = Math.sqrt(totalRedSquareDiff/length);
    		rootMeanGreenSquareError = Math.sqrt(totalGreenSquareDiff/length);
    		rootMeanBlueSquareError = Math.sqrt(totalBlueSquareDiff/length);
    		rootMeanSquareError = Math.sqrt(totalSquareDiff/(3.0*length));
    		UI.setDataText("Red root mean squared error = " + rootMeanRedSquareError + "\n");
    	    System.out.println("Red root mean squared error = " + rootMeanRedSquareError);
    	    UI.setDataText("Green root mean squared error = " + rootMeanGreenSquareError + "\n");
    	    System.out.println("Green root mean squared error = " + rootMeanGreenSquareError);
    	    UI.setDataText("Blue root mean squared error = " + rootMeanBlueSquareError + "\n");
    	    System.out.println("Blue root mean squared error = " + rootMeanBlueSquareError);
    	}
    	UI.setDataText("Root mean squared error = " + rootMeanSquareError + "\n");
	    System.out.println("Root mean squared error = " + rootMeanSquareError);
    }
    
    private void psnr() {
    	// calculates peak signal-to-noise ratio (psnr).

    	// param GT: first (original) input image.
    	// param P: second (deformed) input image.
    	// Originally param MAX: maximum value of datarange (if None, MAX is calculated using image dtype).
    	// Now always use image max

    	// returns:  float -- psnr value in dB.
    
    	//if MAX is None:
    	double range;
    	double redRange;
    	double greenRange;
    	double blueRange;
    	if ((referenceImage.getType() == ModelStorageBase.BYTE) || (referenceImage.getType() == ModelStorageBase.UBYTE) ||
    		(referenceImage.getType() == ModelStorageBase.ARGB)) {
    		range = 255.0;
    		redRange = 255.0;
    		greenRange = 255.0;
    		blueRange = 255.0;
    	}
    	else {
            range = referenceImage.getMax() - referenceImage.getMin();
            redRange = range;
            greenRange = range;
            blueRange = range;
    	}
        mse();
        if (isColor) {
        	double peakRedSignalToNoiseRatio;
        	double peakGreenSignalToNoiseRatio;
        	double peakBlueSignalToNoiseRatio;
        	
        	if (meanRedSquareError == 0) {
            	peakRedSignalToNoiseRatio = Double.POSITIVE_INFINITY;
            }
            else {
            	peakRedSignalToNoiseRatio = 10.0 * Math.log10((redRange*redRange)/meanRedSquareError);
            }
            UI.setDataText("Peak red signal to noise ratio = " + peakRedSignalToNoiseRatio + "\n");
    	    System.out.println("Peak red signal to noise ratio = " + peakRedSignalToNoiseRatio);	
    	    
    	    if (meanGreenSquareError == 0) {
            	peakGreenSignalToNoiseRatio = Double.POSITIVE_INFINITY;
            }
            else {
            	peakGreenSignalToNoiseRatio = 10.0 * Math.log10((greenRange*greenRange)/meanGreenSquareError);
            }
            UI.setDataText("Peak green signal to noise ratio = " + peakGreenSignalToNoiseRatio + "\n");
    	    System.out.println("Peak green signal to noise ratio = " + peakGreenSignalToNoiseRatio);
    	    
    	    if (meanBlueSquareError == 0) {
            	peakBlueSignalToNoiseRatio = Double.POSITIVE_INFINITY;
            }
            else {
            	peakBlueSignalToNoiseRatio = 10.0 * Math.log10((blueRange*blueRange)/meanBlueSquareError);
            }
            UI.setDataText("Peak blue signal to noise ratio = " + peakBlueSignalToNoiseRatio + "\n");
    	    System.out.println("Peak blue signal to noise ratio = " + peakBlueSignalToNoiseRatio);
        } // if (isColor)
        if (meanSquareError == 0) {
        	peakSignalToNoiseRatio = Double.POSITIVE_INFINITY;
        }
        else {
        	peakSignalToNoiseRatio = 10.0 * Math.log10((range*range)/meanSquareError);
        }
        UI.setDataText("Peak signal to noise ratio = " + peakSignalToNoiseRatio + "\n");
	    System.out.println("Peak signal to noise ratio = " + peakSignalToNoiseRatio);
    }
        

    
    
    public double[][] copyMakeBorder(double src[][], int top, int bottom, int left, int right, int borderType, double borderValue) {
    	int i,j;
    	double dst[][] = new double[src.length + top + bottom][src[0].length + left + right];
    	for (i = 0; i < src.length; i++) {
    		for (j = 0; j < src[0].length; j++) {
    			dst[i+top][j+left] = src[i][j];
    		}
    	}
    	
    	for (i = 0; i < top; i++) {
    		for (j = 0; j < left; j++) {
    		   switch (borderType) {
    		   case BORDER_CONSTANT:
    			   dst[i][j] = borderValue;
    			   break;
    		   case BORDER_REPLICATE:
    			   dst[i][j] = src[0][0];
    			   break;
    		   case BORDER_REFLECT:
    			   dst[i][j] = src[top - 1 - i][left - 1 - j];
    			   break;
    		   case BORDER_REFLECT_101:
     			   dst[i][j] = src[top - i][left - j];
     			   break;
    		   }
    		  
 		   }
    		
    		for (j = left; j < src[0].length + left; j++) {
    			switch (borderType) {
    			case BORDER_CONSTANT:
    				dst[i][j] = borderValue;
    				break;
    			case BORDER_REPLICATE:
    				dst[i][j] = src[0][j - left];
    				break;
    			case BORDER_REFLECT:
    				dst[i][j] = src[top - 1 - i][j - left];
    				break;
    			case BORDER_REFLECT_101:
    				dst[i][j] = src[top - i][j - left];
    				break;
    			}
    		}
    		
    		for (j = src[0].length + left; j < src[0].length + left + right; j++) {
    		    switch(borderType) {
    		    case BORDER_CONSTANT:
     			   dst[i][j] = borderValue;
     			   break;
    		    case BORDER_REPLICATE:
    		    	dst[i][j] = src[0][src[0].length-1];
    		    	break;
    		    case BORDER_REFLECT:
    		    	dst[i][j] = src[top - 1 - i][2*src[0].length + left - 1 - j];
    		    	break;
    		    case BORDER_REFLECT_101:
    		    	dst[i][j] = src[top - i][2*src[0].length + left - 2 - j];
    		    	break;
    		    }
    		}
    	}
    	
    	for (i = top + src.length; i < top + src.length + bottom; i++) {
            for (j = 0; j < left; j++) {
    		    switch (borderType) {
    		    case BORDER_CONSTANT:
     			   dst[i][j] = borderValue;
     			   break;
    		    case BORDER_REPLICATE:
    		    	dst[i][j] = src[src.length-1][0];
    		    	break;
    		    case BORDER_REFLECT:
    		    	dst[i][j] = src[2*src.length + top - 1 - i][left - 1 - j];
    		    	break;
    		    case BORDER_REFLECT_101:
    		    	dst[i][j] = src[2*src.length + top - 2 - i][left - j];
    		    	break;
    		    }
    		}
            
            for (j = left; j < src[0].length + left; j++) {
    			switch (borderType) {
    			case BORDER_CONSTANT:
    				dst[i][j] = borderValue;
    				break;
    			case BORDER_REPLICATE:
    				dst[i][j] = src[src.length-1][j - left];
    				break;
    			case BORDER_REFLECT:
    				dst[i][j] = src[2*src.length + top - 1 - i][j - left];
    				break;
    			case BORDER_REFLECT_101:
    				dst[i][j] = src[2*src.length + top - 2 - i][j - left];
    				break;
    			}
    		}
    		
    		for (j = src[0].length + left; j < src[0].length + left + right; j++) {
    		    switch(borderType) {
    		    case BORDER_CONSTANT:
     			   dst[i][j] = borderValue;
     			   break;
    		    case BORDER_REPLICATE:
    		    	dst[i][j] = src[src.length-1][src[0].length-1];
    		    	break;
    		    case BORDER_REFLECT:
    		        dst[i][j] = src[2*src.length + top - 1 - i][2*src[0].length + left - 1 - j];
    		        break;
    		    case BORDER_REFLECT_101:
    		        dst[i][j] = src[2*src.length + top - 2 - i][2*src[0].length + left - 2 - j];
    		        break;
    		    }
    		}	
    	}
    	
    	for (i = top; i < src.length + top; i++) {
    		for (j = 0; j < left; j++) {
    			switch(borderType) {
    			case BORDER_CONSTANT:
    				dst[i][j] = borderValue;
    				break;
    			case BORDER_REPLICATE:
    				dst[i][j] = src[i - top][0];
    				break;
    			case BORDER_REFLECT:
    				dst[i][j] = src[i - top][left - 1 - j];
    				break; 
    			case BORDER_REFLECT_101:
    				dst[i][j] = src[i - top][left - j];
    				break;  
    			}
    		}
    		
    		for (j = src[0].length + left; j < src[0].length + left + right; j++) {
    		    switch(borderType) {
    		    case BORDER_CONSTANT:
     			   dst[i][j] = borderValue;
     			   break;
    		    case BORDER_REPLICATE:
    		    	dst[i][j] = src[i - top][src[0].length-1];
    		    	break;
    		    case BORDER_REFLECT:
    		        dst[i][j] = src[i - top][2*src[0].length + left - 1 - j];
    		        break;
    		    case BORDER_REFLECT_101:
    		        dst[i][j] = src[i - top][2*src[0].length + left - 2 - j];
    		        break;
    		    }
    		}	
    	}
    	
    	
    	return dst;
    }
    
    private double[][] filter2Same(double img[][], double win[][]) {
    	int top = win.length/2;
    	int bottom = win.length -top - 1;
    	int left = win[0].length/2;
    	int right = win[0].length - left - 1;
    	double imgpad[][] = copyMakeBorder(img, top, bottom, left, right, BORDER_REFLECT, 0.0);
    	int h = img.length;
		int w = img[0].length;
		double result[][] = new double[h][w];
		double sum;
		int y,x,i,j;
		for (y = top; y < h + top; y++) {
			for (x = left; x < w + left; x++) {
			    sum = 0.0;
			    for (i = -top; i <= bottom; i++) {
			    	for (j = -left; j <= right; j++) {
			    		sum += imgpad[y + i][x + j] * win[i+top][j+left];
			    	}
			    }
			    result[y-top][x-left] = sum;
			}
		}
		return result;
    }
    
    private double[][] correlationFilter(double img[][]) {
    	int top = win.length/2;
    	int bottom = win.length -top - 1;
    	int left = win[0].length/2;
    	int right = win[0].length - left - 1;
    	double imgpad[][] = copyMakeBorder(img, top, bottom, left, right, BORDER_REFLECT, 0.0);
    	int h = img.length;
		int w = img[0].length;
		double result[][] = new double[h][w];
		double sum;
		int y,x,i,j;
		for (y = top; y < h + top; y++) {
			for (x = left; x < w + left; x++) {
			    sum = 0.0;
			    for (i = -top; i <= bottom; i++) {
			    	for (j = -left; j <= right; j++) {
			    		sum += imgpad[y + i][x + j] * win[i+top][j+left];
			    	}
			    }
			    result[y-top][x-left] = sum;
			}
		}
		return result;
    }
	
	private double[][] uniformFilter(double img[][], int size1, int size2) {
		double imgpad[][] = copyMakeBorder(img, size1, size2, size1, size2, BORDER_REFLECT, 0.0);
		int h = img.length;
		int w = img[0].length;
		double result[][] = new double[h][w];
		double area = (size1+size2+1)*(size1+size2+1);
		double sum;
		int y,x,i,j;
		for (y = size1; y < h + size1; y++) {
			for (x = size1; x < w + size1; x++) {
			    sum = 0.0;
			    for (i = -size1; i <= size2; i++) {
			    	for (j = -size1; j <= size2; j++) {
			    		sum += imgpad[y + i][x + j];
			    	}
			    }
			    result[y-size1][x-size1] = sum/area;
			}
		}
		return result;
	}
    
    private void rmse_sw () {
    	// calculates root mean squared error (rmse) using sliding window.

    	// param GT: first (original) input image.
    	// param P: second (deformed) input image.
    	// param ws: sliding window size (default = 8).

    	// returns:  tuple -- rmse value,rmse map.
    	int x,y;
    	int size1 = ws/2;
    	int size2 = ws - size1 - 1;
    	double errors[][] = new double[yDim][xDim];
    	double diff;
    	int index;
    	double total = 0.0;
    	int s = (int)Math.round(ws/2.0);
    	int numValues = (yDim - 2*s)*(xDim - 2*s);
        if (!isColor) {
        	rmse_map = new double[yDim][xDim];
        	for (y = 0; y < yDim; y++) {
        		for (x = 0; x < xDim; x++) {
        			index = x + y*xDim;
        		    diff = referenceBuffer[index] - testBuffer[index];
        		    errors[y][x] = diff*diff;
        		}
        	}
        	errors = uniformFilter(errors, size1, size2);
        	for (y = 0; y < yDim; y++) {
        		for (x = 0; x < xDim; x++) {
        			rmse_map[y][x] = Math.sqrt(errors[y][x]);	
        		}
        	}
        	for (y = s; y < yDim - s; y++) {
        		for (x = s; x < xDim - s; x++) {
        			total += rmse_map[y][x];
        		}
        	}
        	rmse_sw_mean = total/numValues;
        }
        else {
        	rmse_red_map = new double[yDim][xDim];
        	rmse_green_map = new double[yDim][xDim];
        	rmse_blue_map = new double[yDim][xDim];
        	for (y = 0; y < yDim; y++) {
        		for (x = 0; x < xDim; x++) {
        			index = x + y*xDim;
        		    diff = referenceRedBuffer[index] - testRedBuffer[index];
        		    errors[y][x] = diff*diff;
        		}
        	}
        	errors = uniformFilter(errors, size1, size2);
        	for (y = 0; y < yDim; y++) {
        		for (x = 0; x < xDim; x++) {
        			rmse_red_map[y][x] = Math.sqrt(errors[y][x]);	
        		}
        	}
        	for (y = s; y < yDim - s; y++) {
        		for (x = s; x < xDim - s; x++) {
        			total += rmse_red_map[y][x];
        		}
        	}
        	for (y = 0; y < yDim; y++) {
        		for (x = 0; x < xDim; x++) {
        			index = x + y*xDim;
        		    diff = referenceGreenBuffer[index] - testGreenBuffer[index];
        		    errors[y][x] = diff*diff;
        		}
        	}
        	errors = uniformFilter(errors, size1, size2);
        	for (y = 0; y < yDim; y++) {
        		for (x = 0; x < xDim; x++) {
        			rmse_green_map[y][x] = Math.sqrt(errors[y][x]);	
        		}
        	}
        	for (y = s; y < yDim - s; y++) {
        		for (x = s; x < xDim - s; x++) {
        			total += rmse_green_map[y][x];
        		}
        	}
        	
        	for (y = 0; y < yDim; y++) {
        		for (x = 0; x < xDim; x++) {
        			index = x + y*xDim;
        		    diff = referenceBlueBuffer[index] - testBlueBuffer[index];
        		    errors[y][x] = diff*diff;
        		}
        	}
        	errors = uniformFilter(errors, size1, size2);
        	for (y = 0; y < yDim; y++) {
        		for (x = 0; x < xDim; x++) {
        			rmse_blue_map[y][x] = Math.sqrt(errors[y][x]);		
        		}
        	}
        	for (y = s; y < yDim - s; y++) {
        		for (x = s; x < xDim - s; x++) {
        			total += rmse_blue_map[y][x];
        		}
        	}
        	rmse_sw_mean = total/(3.0*numValues);
        }
        
    	return;
    }
    
    
    
    private double[][] filter2Valid(double img[][], double filter[][]) {
    	int outYDim = Math.max(img.length,filter.length) - Math.min(img.length,filter.length)+1;
    	int outXDim = Math.max(img[0].length,filter[0].length) - Math.min(img[0].length,filter[0].length)+1;
    	double out[][] = new double[outYDim][outXDim];
    	int top = filter.length/2;
    	int bottom = filter.length -top - 1;
    	int left = filter[0].length/2;
    	int right = filter[0].length - left - 1;
    	int i,j,m,n,ii,jj;
    	for(i=top; i < img.length-bottom; ++i) {
    		for (j = left; j < img[0].length - right; j++) {
    			for(m=-top; m <= bottom; ++m) {
    				ii = i + m;
    				for (n = -left; n <= right; n++) {
    				    jj = j + n;	
    				    out[i-top][j-left] += img[ii][jj] * filter[m+top][n+left];
    				}
    			}
    		}
    	}
    	return out;
    }
    
    private void ergas() {
    	// calculates erreur relative globale adimensionnelle de synthese (ergas).

    	// param GT: first (original) input image.
    	// param P: second (deformed) input image.
    	// param r: ratio of high resolution to low resolution (default=4).
    	// param ws: sliding window size (default = 8).

    	// returns:  float -- ergas value.
    	rmse_map = null;
    	rmse_red_map = null;
    	rmse_green_map = null;
    	rmse_blue_map = null;
        int nb = 1;
        int size1 = ws/2;
    	int size2 = ws - size1 - 1;
    	int x,y;
    	int index;
    	int wsSquared = ws*ws;
    	double total = 0;
    	double refBuf2D[][] = new double[yDim][xDim];
    	double ergas_map[][] = new double[yDim][xDim];
    	double means_map[][];
    	double red_means_map[][];
    	double green_means_map[][];
    	double blue_means_map[][];
    	int s = (int)Math.round(ws/2.0);
    	int numValues = (yDim - 2*s)*(xDim - 2*s);
        
        rmse_sw();
        if (!isColor) {
        	for (y = 0; y < yDim; y++) {
        		for (x = 0; x < xDim; x++) {
        			index = x + y*xDim;
        			refBuf2D[y][x] = referenceBuffer[index];
        		}
        	}
            means_map = uniformFilter(refBuf2D, size1, size2);
            for (y = 0; y < yDim; y++) {
        		for (x = 0; x < xDim; x++) {
        			means_map[y][x] = means_map[y][x]/wsSquared;
        			// Avoid division by zero
        			if (means_map[y][x] != 0) {
        		        ergas_map[y][x] = 100*r*Math.sqrt(rmse_map[y][x]*rmse_map[y][x])/(means_map[y][x]*means_map[y][x]*nb);
        			}
        		}
            }
        } // if (!isColor)
        else { // isColor
        	for (y = 0; y < yDim; y++) {
        		for (x = 0; x < xDim; x++) {
        			index = x + y*xDim;
        			refBuf2D[y][x] = referenceRedBuffer[index];
        		}
        	}
            red_means_map = uniformFilter(refBuf2D, size1, size2);
            for (y = 0; y < yDim; y++) {
        		for (x = 0; x < xDim; x++) {
        			index = x + y*xDim;
        			refBuf2D[y][x] = referenceGreenBuffer[index];
        		}
        	}
            green_means_map = uniformFilter(refBuf2D, size1, size2);
            for (y = 0; y < yDim; y++) {
        		for (x = 0; x < xDim; x++) {
        			index = x + y*xDim;
        			refBuf2D[y][x] = referenceBlueBuffer[index];
        		}
        	}
            blue_means_map = uniformFilter(refBuf2D, size1, size2);
            for (y = 0; y < yDim; y++) {
            	for (x = 0; x < xDim; x++) {
            		total = 0.0;
            		red_means_map[y][x] = red_means_map[y][x]/wsSquared;
            		green_means_map[y][x] = green_means_map[y][x]/wsSquared;
            		blue_means_map[y][x] = blue_means_map[y][x]/wsSquared;
            		if (red_means_map[y][x] != 0.0) {
            			total += (rmse_red_map[y][x]*rmse_red_map[y][x])/(red_means_map[y][x]*red_means_map[y][x]);	
            		}
            		if (green_means_map[y][x] != 0.0) {
            			total += (rmse_green_map[y][x]*rmse_green_map[y][x])/(green_means_map[y][x]*green_means_map[y][x]);	
            		}
            		if (blue_means_map[y][x] != 0.0) {
            			total += (rmse_blue_map[y][x]*rmse_blue_map[y][x])/(blue_means_map[y][x]*blue_means_map[y][x]);	
            		}
            		ergas_map[y][x] = 100*r*Math.sqrt(total/nb);
            	}
            } 
        } // isColor
        total = 0.0;
        for (y = s; y < yDim - s; y++) {
        	for (x = s; x < xDim -s; x++) {
        		total += ergas_map[y][x];
        	}
        }
        ergas_mean = total/numValues;
        UI.setDataText("ergas = " + ergas_mean + "\n");
        System.out.println("ergas = " + ergas_mean);
    }
    
    private void scc() {
    	// calculates spatial correlation coefficient (scc).

    	// param GT: first (original) input image.
    	// param P: second (deformed) input image.
    	// param fltr: high pass filter for spatial processing (default=[[-1,-1,-1],[-1,8,-1],[-1,-1,-1]]).
    	// param ws: sliding window size (default = 8).

    	// returns:  float -- scc value.
    	double refBuf2D[][] = new double[yDim][xDim];
    	double testBuf2D[][] = new double[yDim][xDim];
    	int x,y;
    	int index;
    	double GT_hp[][];
    	double P_hp[][];
    	double mu1[][];
    	double mu2[][];
    	double GT_sum_sq[][] = new double[yDim][xDim];
    	double P_sum_sq[][] = new double[yDim][xDim];
    	double GT_P_sum_mul[][] = new double[yDim][xDim];
    	double GTGT[][] = new double[yDim][xDim];
    	double PP[][] = new double[yDim][xDim];
    	double GTP[][] = new double[yDim][xDim];
    	double filtGTGT[][];
    	double filtPP[][];
    	double filtGTP[][];
    	double sigmaGT_sq;
    	double sigmaP_sq;
    	double sigmaGT_P; 
    	double den;
    	double inversews2 = 1.0/(ws*ws);
	    double window[][] = new double[ws][ws];
	    for (y = 0; y < ws; y++) {
	    	for (x = 0; x < ws; x++) {
	    		window[y][x] = inversews2;
	    	}
	    }
    	double total = 0.0;
    	
    	if (!isColor) {
    		for (y = 0; y < yDim; y++) {
    			for (x = 0; x < xDim; x++) {
    				index = x + y*xDim;
    				refBuf2D[y][x] = referenceBuffer[index];
    				testBuf2D[y][x] = testBuffer[index];
    			}
    		}
    		
    	    GT_hp = correlationFilter(refBuf2D);
    	    P_hp = correlationFilter(testBuf2D);
    	    mu1 = filter2Same(GT_hp, window);
	    	mu2 = filter2Same(P_hp,window);
	    	for (y = 0; y < yDim; y++) {
	    		for (x = 0; x < xDim; x++) {
	    		    GT_sum_sq[y][x] = mu1[y][x] * mu1[y][x];
	    		    P_sum_sq[y][x] = mu2[y][x] * mu2[y][x];
	    		    GT_P_sum_mul[y][x] = mu1[y][x] * mu2[y][x];
	    		    GTGT[y][x] = GT_hp[y][x] * GT_hp[y][x];
	    		    PP[y][x] = P_hp[y][x] * P_hp[y][x];
	    		    GTP[y][x] = GT_hp[y][x] * P_hp[y][x];
	    		}
	    	}
	    	filtGTGT = filter2Same(GTGT, window);
	    	filtPP = filter2Same(PP, window);
	    	filtGTP = filter2Same(GTP, window);
	    	for (y = 0; y < yDim; y++) {
	    		for (x = 0; x < xDim; x++) {
	    			sigmaGT_sq = Math.max(0,filtGTGT[y][x] - GT_sum_sq[y][x]);
	    			sigmaP_sq = Math.max(0,filtPP[y][x] -P_sum_sq[y][x]);
	    			sigmaGT_P = filtGTP[y][x] - GT_P_sum_mul[y][x];
	    			den = Math.sqrt(sigmaGT_sq) * Math.sqrt(sigmaP_sq);
	    			if (den != 0) {
	    				total += sigmaGT_P / den;
	    			}
	    		}
	    	}
	    	spatialCorrelationCoefficient = total/length;
    	} // if (!isColor)
    	else { // isColor
    		for (y = 0; y < yDim; y++) {
    			for (x = 0; x < xDim; x++) {
    				index = x + y*xDim;
    				refBuf2D[y][x] = referenceRedBuffer[index];
    				testBuf2D[y][x] = testRedBuffer[index];
    			}
    		}
    		
    	    GT_hp = correlationFilter(refBuf2D);
    	    P_hp = correlationFilter(testBuf2D);
    	    mu1 = filter2Same(GT_hp, window);
	    	mu2 = filter2Same(P_hp,window);
	    	for (y = 0; y < yDim; y++) {
	    		for (x = 0; x < xDim; x++) {
	    		    GT_sum_sq[y][x] = mu1[y][x] * mu1[y][x];
	    		    P_sum_sq[y][x] = mu2[y][x] * mu2[y][x];
	    		    GT_P_sum_mul[y][x] = mu1[y][x] * mu2[y][x];
	    		    GTGT[y][x] = GT_hp[y][x] * GT_hp[y][x];
	    		    PP[y][x] = P_hp[y][x] * P_hp[y][x];
	    		    GTP[y][x] = GT_hp[y][x] * P_hp[y][x];
	    		}
	    	}
	    	filtGTGT = filter2Same(GTGT, window);
	    	filtPP = filter2Same(PP, window);
	    	filtGTP = filter2Same(GTP, window);
	    	for (y = 0; y < yDim; y++) {
	    		for (x = 0; x < xDim; x++) {
	    			sigmaGT_sq = Math.max(0,filtGTGT[y][x] - GT_sum_sq[y][x]);
	    			sigmaP_sq = Math.max(0,filtPP[y][x] -P_sum_sq[y][x]);
	    			sigmaGT_P = filtGTP[y][x] - GT_P_sum_mul[y][x];
	    			den = Math.sqrt(sigmaGT_sq) * Math.sqrt(sigmaP_sq);
	    			if (den != 0) {
	    				total += sigmaGT_P / den;
	    			}
	    		}
	    	}
	    	
	    	for (y = 0; y < yDim; y++) {
    			for (x = 0; x < xDim; x++) {
    				index = x + y*xDim;
    				refBuf2D[y][x] = referenceGreenBuffer[index];
    				testBuf2D[y][x] = testGreenBuffer[index];
    			}
    		}
    		
    	    GT_hp = correlationFilter(refBuf2D);
    	    P_hp = correlationFilter(testBuf2D);
    	    mu1 = filter2Same(GT_hp, window);
	    	mu2 = filter2Same(P_hp,window);
	    	for (y = 0; y < yDim; y++) {
	    		for (x = 0; x < xDim; x++) {
	    		    GT_sum_sq[y][x] = mu1[y][x] * mu1[y][x];
	    		    P_sum_sq[y][x] = mu2[y][x] * mu2[y][x];
	    		    GT_P_sum_mul[y][x] = mu1[y][x] * mu2[y][x];
	    		    GTGT[y][x] = GT_hp[y][x] * GT_hp[y][x];
	    		    PP[y][x] = P_hp[y][x] * P_hp[y][x];
	    		    GTP[y][x] = GT_hp[y][x] * P_hp[y][x];
	    		}
	    	}
	    	filtGTGT = filter2Same(GTGT, window);
	    	filtPP = filter2Same(PP, window);
	    	filtGTP = filter2Same(GTP, window);
	    	for (y = 0; y < yDim; y++) {
	    		for (x = 0; x < xDim; x++) {
	    			sigmaGT_sq = Math.max(0,filtGTGT[y][x] - GT_sum_sq[y][x]);
	    			sigmaP_sq = Math.max(0,filtPP[y][x] -P_sum_sq[y][x]);
	    			sigmaGT_P = filtGTP[y][x] - GT_P_sum_mul[y][x];
	    			den = Math.sqrt(sigmaGT_sq) * Math.sqrt(sigmaP_sq);
	    			if (den != 0) {
	    				total += sigmaGT_P / den;
	    			}
	    		}
	    	}
	    	
	    	for (y = 0; y < yDim; y++) {
    			for (x = 0; x < xDim; x++) {
    				index = x + y*xDim;
    				refBuf2D[y][x] = referenceBlueBuffer[index];
    				testBuf2D[y][x] = testBlueBuffer[index];
    			}
    		}
    		
    	    GT_hp = correlationFilter(refBuf2D);
    	    P_hp = correlationFilter(testBuf2D);
    	    mu1 = filter2Same(GT_hp, window);
	    	mu2 = filter2Same(P_hp,window);
	    	for (y = 0; y < yDim; y++) {
	    		for (x = 0; x < xDim; x++) {
	    		    GT_sum_sq[y][x] = mu1[y][x] * mu1[y][x];
	    		    P_sum_sq[y][x] = mu2[y][x] * mu2[y][x];
	    		    GT_P_sum_mul[y][x] = mu1[y][x] * mu2[y][x];
	    		    GTGT[y][x] = GT_hp[y][x] * GT_hp[y][x];
	    		    PP[y][x] = P_hp[y][x] * P_hp[y][x];
	    		    GTP[y][x] = GT_hp[y][x] * P_hp[y][x];
	    		}
	    	}
	    	filtGTGT = filter2Same(GTGT, window);
	    	filtPP = filter2Same(PP, window);
	    	filtGTP = filter2Same(GTP, window);
	    	for (y = 0; y < yDim; y++) {
	    		for (x = 0; x < xDim; x++) {
	    			sigmaGT_sq = Math.max(0,filtGTGT[y][x] - GT_sum_sq[y][x]);
	    			sigmaP_sq = Math.max(0,filtPP[y][x] -P_sum_sq[y][x]);
	    			sigmaGT_P = filtGTP[y][x] - GT_P_sum_mul[y][x];
	    			den = Math.sqrt(sigmaGT_sq) * Math.sqrt(sigmaP_sq);
	    			if (den != 0) {
	    				total += sigmaGT_P / den;
	    			}
	    		}
	    	}
    		spatialCorrelationCoefficient = total/(3.0*length);   	
    	} // isColor
    	UI.setDataText("Spatial correlation coefficient = " + spatialCorrelationCoefficient + "\n");
        System.out.println("Spatial correlation coefficient = " + spatialCorrelationCoefficient);	
    }
    
    private void rase() {
    	// calculates relative average spectral error (rase).

    	// param GT: first (original) input image.
    	// param P: second (deformed) input image.
    	// param ws: sliding window size (default = 8).

    	// returns:  float -- rase value.
    	int x,y,index;
    	int size1 = ws/2;
    	int size2 = ws - size1 - 1;
    	double GT_means[][];
    	double red_GT_means[][];
    	double green_GT_means[][];
    	double blue_GT_means[][];
    	double refBuf2D[][] = new double[yDim][xDim];
    	int wsSquared = ws*ws;
    	double M;
    	double rase_map[][] = new double[yDim][xDim];
    	int s = (int)Math.round(ws/2.0);
    	int numValues = (yDim - 2*s)*(xDim - 2*s);
    	double total = 0.0;
    	rmse_sw();
    	if (!isColor) {
    		for (y = 0; y < yDim; y++) {
        		for (x = 0; x < xDim; x++) {
        			index = x + y*xDim;
        			refBuf2D[y][x] = referenceBuffer[index];
        		}
        	}
            GT_means = uniformFilter(refBuf2D, size1, size2);
            for (y = 0; y < yDim; y++) {
        		for (x = 0; x < xDim; x++) {
        			GT_means[y][x] = GT_means[y][x]/wsSquared;
        			rase_map[y][x] = (100.0/GT_means[y][x])*rmse_map[y][x];
        		}
            }
    	} // if (!isColor)
    	else { // isColor
    		for (y = 0; y < yDim; y++) {
        		for (x = 0; x < xDim; x++) {
        			index = x + y*xDim;
        			refBuf2D[y][x] = referenceRedBuffer[index];
        		}
        	}
            red_GT_means = uniformFilter(refBuf2D, size1, size2);
            for (y = 0; y < yDim; y++) {
        		for (x = 0; x < xDim; x++) {
        			index = x + y*xDim;
        			refBuf2D[y][x] = referenceGreenBuffer[index];
        		}
        	}
            green_GT_means = uniformFilter(refBuf2D, size1, size2);
            for (y = 0; y < yDim; y++) {
        		for (x = 0; x < xDim; x++) {
        			index = x + y*xDim;
        			refBuf2D[y][x] = referenceBlueBuffer[index];
        		}
        	}
            blue_GT_means = uniformFilter(refBuf2D, size1, size2);
            for (y = 0; y < yDim; y++) {
        		for (x = 0; x < xDim; x++) {
        			red_GT_means[y][x] = red_GT_means[y][x]/wsSquared;
        			green_GT_means[y][x] = green_GT_means[y][x]/wsSquared;
        			blue_GT_means[y][x] = blue_GT_means[y][x]/wsSquared;
        			M = (red_GT_means[y][x] + green_GT_means[y][x] + blue_GT_means[y][x])/3;
        			rase_map[y][x] = (100.0/M)*Math.sqrt((rmse_red_map[y][x]*rmse_red_map[y][x] +
        					rmse_green_map[y][x]*rmse_green_map[y][x] + rmse_blue_map[y][x]*rmse_blue_map[y][x])/3.0);
        		}
            }
    	} // isColor
    	for (y = s; y < yDim - s; y++) {
        	for (x = s; x < xDim - s; x++) {
        		total += rase_map[y][x];
        	}
        }
        rase_mean = total/numValues;
    	UI.setDataText("Relative average spectral error = " + rase_mean + "\n");
    	System.out.println("Relative average spectral error = " + rase_mean);
    }
    
    private void uqi() {
    	// calculates universal image quality index (uqi).

    	// param GT: first (original) input image.
    	// param P: second (deformed) input image.
    	// param ws: sliding window size (default = 8).

    	// returns:  float -- uqi value.
    	int x,y;
    	int N = ws*ws;
    	double GT[][] = new double[yDim][xDim];
    	double P[][] = new double[yDim][xDim];
    	double GT_sq[][] = new double[yDim][xDim];
    	double P_sq[][] = new double[yDim][xDim];
    	double GT_P[][] = new double[yDim][xDim];
    	double GT_sum[][];
    	double P_sum[][];
    	double GT_sq_sum[][];
    	double P_sq_sum[][];
    	double GT_P_sum[][];
    	double GT_P_sum_mul;
    	double GT_P_sum_sq_sum_mul;
    	double numerator;
    	double denominator1;
    	double denominator;
    	double q_map[][];
    	int index;
    	double total = 0.0;
    	double filter[][] = new double[ws][ws];
    	for (y = 0; y < ws; y++) {
    		for (x = 0; x < ws; x++) {
    			filter[y][x] = 1.0;
    		}
    	}
    	int filtXDim;
    	int filtYDim;
        if (!isColor) {
        	for (y = 0; y < yDim; y++) {
        		for (x = 0; x < xDim; x++) {
        			index = x + y*xDim;
        		    GT[y][x] = referenceBuffer[index];
        		    P[y][x] = testBuffer[index];
        		    GT_sq[y][x] = referenceBuffer[index]*referenceBuffer[index];
        		    P_sq[y][x] = testBuffer[index]*testBuffer[index];
        		    GT_P[y][x] = referenceBuffer[index]*testBuffer[index];
        		    
        		}
        	}
        	GT_sum = filter2Valid(GT, filter);
        	P_sum = filter2Valid(P, filter);
        	GT_sq_sum = filter2Valid(GT_sq, filter);
        	P_sq_sum = filter2Valid(P_sq, filter);
        	GT_P_sum = filter2Valid(GT_P, filter);
        	filtYDim = GT_sum.length;
        	filtXDim = GT_sum[0].length;
        	q_map = new double[filtYDim][filtXDim];
        	for (y = 0; y < filtYDim; y++) {
        		for (x = 0; x < filtXDim; x++) {
        		    GT_P_sum_mul =  GT_sum[y][x]*P_sum[y][x];	
        		    GT_P_sum_sq_sum_mul = GT_sum[y][x]*GT_sum[y][x] + P_sum[y][x]*P_sum[y][x];
        		    numerator = 4*(N*GT_P_sum[y][x] - GT_P_sum_mul)*GT_P_sum_mul;
        		    denominator1 = N*(GT_sq_sum[y][x] + P_sq_sum[y][x]) - GT_P_sum_sq_sum_mul;
        		    denominator = denominator1*GT_P_sum_sq_sum_mul;
        		    q_map[y][x] = 1.0;
        		    if ((denominator1 == 0) && (GT_P_sum_sq_sum_mul != 0)) {
        		    	q_map[y][x] = 2*GT_P_sum_mul/GT_P_sum_sq_sum_mul;	
        		    }
        		    if (denominator != 0) {
        		    	q_map[y][x] = numerator/denominator;	
        		    }
        		}
        	}
        	for (y = 0; y < filtYDim; y++) {
        		for (x = 0; x < filtXDim; x++) {
        		    total += q_map[y][x];    
        		}
        	}
        	universalImageQualityIndex = total/(filtYDim*filtXDim);
        } // if (!isColor)
        else { // isColor
        	for (y = 0; y < yDim; y++) {
        		for (x = 0; x < xDim; x++) {
        			index = x + y*xDim;
        		    GT[y][x] = referenceRedBuffer[index];
        		    P[y][x] = testRedBuffer[index];
        		    GT_sq[y][x] = referenceRedBuffer[index]*referenceRedBuffer[index];
        		    P_sq[y][x] = testRedBuffer[index]*testRedBuffer[index];
        		    GT_P[y][x] = referenceRedBuffer[index]*testRedBuffer[index];
        		    
        		}
        	}
        	GT_sum = filter2Valid(GT, filter);
        	P_sum = filter2Valid(P, filter);
        	GT_sq_sum = filter2Valid(GT_sq, filter);
        	P_sq_sum = filter2Valid(P_sq, filter);
        	GT_P_sum = filter2Valid(GT_P, filter);
        	filtYDim = GT_sum.length;
        	filtXDim = GT_sum[0].length;
        	q_map = new double[filtYDim][filtXDim];
        	for (y = 0; y < filtYDim; y++) {
        		for (x = 0; x < filtXDim; x++) {
        		    GT_P_sum_mul =  GT_sum[y][x]*P_sum[y][x];	
        		    GT_P_sum_sq_sum_mul = GT_sum[y][x]*GT_sum[y][x] + P_sum[y][x]*P_sum[y][x];
        		    numerator = 4*(N*GT_P_sum[y][x] - GT_P_sum_mul)*GT_P_sum_mul;
        		    denominator1 = N*(GT_sq_sum[y][x] + P_sq_sum[y][x]) - GT_P_sum_sq_sum_mul;
        		    denominator = denominator1*GT_P_sum_sq_sum_mul;
        		    q_map[y][x] = 1.0;
        		    if ((denominator1 == 0) && (GT_P_sum_sq_sum_mul != 0)) {
        		    	q_map[y][x] = 2*GT_P_sum_mul/GT_P_sum_sq_sum_mul;	
        		    }
        		    if (denominator != 0) {
        		    	q_map[y][x] = numerator/denominator;	
        		    }
        		}
        	}
        	for (y = 0; y < filtYDim; y++) {
        		for (x = 0; x < filtXDim; x++) {
        		    total += q_map[y][x];    
        		}
        	}
        	
        	for (y = 0; y < yDim; y++) {
        		for (x = 0; x < xDim; x++) {
        			index = x + y*xDim;
        		    GT[y][x] = referenceGreenBuffer[index];
        		    P[y][x] = testGreenBuffer[index];
        		    GT_sq[y][x] = referenceGreenBuffer[index]*referenceGreenBuffer[index];
        		    P_sq[y][x] = testGreenBuffer[index]*testGreenBuffer[index];
        		    GT_P[y][x] = referenceGreenBuffer[index]*testGreenBuffer[index];
        		    
        		}
        	}
        	GT_sum = filter2Valid(GT, filter);
        	P_sum = filter2Valid(P, filter);
        	GT_sq_sum = filter2Valid(GT_sq, filter);
        	P_sq_sum = filter2Valid(P_sq, filter);
        	GT_P_sum = filter2Valid(GT_P, filter);
        	for (y = 0; y < filtYDim; y++) {
        		for (x = 0; x < filtXDim; x++) {
        		    GT_P_sum_mul =  GT_sum[y][x]*P_sum[y][x];	
        		    GT_P_sum_sq_sum_mul = GT_sum[y][x]*GT_sum[y][x] + P_sum[y][x]*P_sum[y][x];
        		    numerator = 4*(N*GT_P_sum[y][x] - GT_P_sum_mul)*GT_P_sum_mul;
        		    denominator1 = N*(GT_sq_sum[y][x] + P_sq_sum[y][x]) - GT_P_sum_sq_sum_mul;
        		    denominator = denominator1*GT_P_sum_sq_sum_mul;
        		    q_map[y][x] = 1.0;
        		    if ((denominator1 == 0) && (GT_P_sum_sq_sum_mul != 0)) {
        		    	q_map[y][x] = 2*GT_P_sum_mul/GT_P_sum_sq_sum_mul;	
        		    }
        		    if (denominator != 0) {
        		    	q_map[y][x] = numerator/denominator;	
        		    }
        		}
        	}
        	for (y = 0; y < filtYDim; y++) {
        		for (x = 0; x < filtXDim; x++) {
        		    total += q_map[y][x];    
        		}
        	}
        	
        	for (y = 0; y < yDim; y++) {
        		for (x = 0; x < xDim; x++) {
        			index = x + y*xDim;
        		    GT[y][x] = referenceBlueBuffer[index];
        		    P[y][x] = testBlueBuffer[index];
        		    GT_sq[y][x] = referenceBlueBuffer[index]*referenceBlueBuffer[index];
        		    P_sq[y][x] = testBlueBuffer[index]*testBlueBuffer[index];
        		    GT_P[y][x] = referenceBlueBuffer[index]*testBlueBuffer[index];
        		    
        		}
        	}
        	GT_sum = filter2Valid(GT, filter);
        	P_sum = filter2Valid(P, filter);
        	GT_sq_sum = filter2Valid(GT_sq, filter);
        	P_sq_sum = filter2Valid(P_sq, filter);
        	GT_P_sum = filter2Valid(GT_P, filter);
        	for (y = 0; y < filtYDim; y++) {
        		for (x = 0; x < filtXDim; x++) {
        		    GT_P_sum_mul =  GT_sum[y][x]*P_sum[y][x];	
        		    GT_P_sum_sq_sum_mul = GT_sum[y][x]*GT_sum[y][x] + P_sum[y][x]*P_sum[y][x];
        		    numerator = 4*(N*GT_P_sum[y][x] - GT_P_sum_mul)*GT_P_sum_mul;
        		    denominator1 = N*(GT_sq_sum[y][x] + P_sq_sum[y][x]) - GT_P_sum_sq_sum_mul;
        		    denominator = denominator1*GT_P_sum_sq_sum_mul;
        		    q_map[y][x] = 1.0;
        		    if ((denominator1 == 0) && (GT_P_sum_sq_sum_mul != 0)) {
        		    	q_map[y][x] = 2*GT_P_sum_mul/GT_P_sum_sq_sum_mul;	
        		    }
        		    if (denominator != 0) {
        		    	q_map[y][x] = numerator/denominator;	
        		    }
        		}
        	}
        	for (y = 0; y < filtYDim; y++) {
        		for (x = 0; x < filtXDim; x++) {
        		    total += q_map[y][x];    
        		}
        	}
        	universalImageQualityIndex = total/(3.0*filtYDim*filtXDim);
        } // else is Color
    	UI.setDataText("Universal quality image index = " + universalImageQualityIndex + "\n");
    	System.out.println("Universal quality image index = " + universalImageQualityIndex);
    }

    private void ssim(boolean downSampling) {
    	// calculates structural similarity index (ssim).

    	// param GT: first (original) input image.
    	// param P: second (deformed) input image.
    	// param ws: sliding window size (default = 11).
    	// param K1: First constant for SSIM (default = 0.01).
    	// param K2: Second constant for SSIM (default = 0.03).
    	// param MAX: Maximum value of datarange (if None, MAX is calculated using image dtype).
    	// sigma = 1.5

    	// returns:  tuple -- ssim value, cs value.	
    	//if MAX is None:
    	
    	int x,y;
    	int size1 = ws/2;
    	int size2 = ws - size1 - 1;
    	int index;
    	int filtXDim;
    	int filtYDim;
    	double refBuf2D[][] = new double[yDim][xDim];
    	double testBuf2D[][] = new double[yDim][xDim];
    	int scaleYDim = yDim;
    	int scaleXDim = xDim;
    	double scaleReferenceBuffer[] = null;
    	double scaleTestBuffer[] = null;
    	float scaleReferenceRedBuffer[] = null;
    	float scaleTestRedBuffer[] = null;
    	float scaleReferenceGreenBuffer[] = null;
    	float scaleTestGreenBuffer[] = null;
    	float scaleReferenceBlueBuffer[] = null;
        float scaleTestBlueBuffer[] = null;
    	int scaleIndex;
    	int f = Math.max(1,Math.round(Math.min(xDim,yDim)/256));
    	if (downSampling && (f > 1)) {
    		// automatic downsampling
    		
    		// downsampling by f
    		// use a simple low-pass filter 
			double lpf[][] = new double[f][f];
			double fval = 1.0/(f*f);
			for (y = 0; y < f; y++) {
				for (x = 0; x < f; x++) {
					lpf[y][x] = fval;
				}
			}
			if (!isColor) {
				for (y = 0; y < yDim; y++) {
		    		for (x = 0; x < xDim; x++) {
		    			index = x + y*xDim;
		    			refBuf2D[y][x] = referenceBuffer[index];
		    			testBuf2D[y][x] = testBuffer[index];
		    		}
				}
				double imgGT[][] = filter2Same(refBuf2D, lpf);
				double imgP[][] = filter2Same(testBuf2D,lpf);
				scaleYDim = 1 + (yDim - 1)/f;
				scaleXDim = 1 + (xDim - 1)/f;
				scaleReferenceBuffer = new double[scaleYDim*scaleXDim];
				scaleTestBuffer = new double[scaleYDim*scaleXDim];
				for (y = 0; y < scaleYDim; y++) {
					for (x = 0; x < scaleXDim; x++) {
					    scaleIndex = x + y*scaleXDim;
					    scaleReferenceBuffer[scaleIndex] = imgGT[f*y][f*x];
					    scaleTestBuffer[scaleIndex] = imgP[f*y][f*x];
					}
				}
			} // if (!isColor)
			else { // isColor
				for (y = 0; y < yDim; y++) {
		    		for (x = 0; x < xDim; x++) {
		    			index = x + y*xDim;
		    			refBuf2D[y][x] = referenceRedBuffer[index];
		    			testBuf2D[y][x] = testRedBuffer[index];
		    		}
				}
				double imgGT[][] = filter2Same(refBuf2D, lpf);
				double imgP[][] = filter2Same(testBuf2D,lpf);
				scaleYDim = 1 + (yDim - 1)/f;
				scaleXDim = 1 + (xDim - 1)/f;
				scaleReferenceRedBuffer = new float[scaleYDim*scaleXDim];
				scaleTestRedBuffer = new float[scaleYDim*scaleXDim];
				for (y = 0; y < scaleYDim; y++) {
					for (x = 0; x < scaleXDim; x++) {
					    scaleIndex = x + y*scaleXDim;
					    scaleReferenceRedBuffer[scaleIndex] = (float)imgGT[f*y][f*x];
					    scaleTestRedBuffer[scaleIndex] = (float)imgP[f*y][f*x];
					}
				}	
				
				for (y = 0; y < yDim; y++) {
		    		for (x = 0; x < xDim; x++) {
		    			index = x + y*xDim;
		    			refBuf2D[y][x] = referenceGreenBuffer[index];
		    			testBuf2D[y][x] = testGreenBuffer[index];
		    		}
				}
				imgGT = filter2Same(refBuf2D, lpf);
				imgP = filter2Same(testBuf2D,lpf);
				scaleReferenceGreenBuffer = new float[scaleYDim*scaleXDim];
				scaleTestGreenBuffer = new float[scaleYDim*scaleXDim];
				for (y = 0; y < scaleYDim; y++) {
					for (x = 0; x < scaleXDim; x++) {
					    scaleIndex = x + y*scaleXDim;
					    scaleReferenceGreenBuffer[scaleIndex] = (float)imgGT[f*y][f*x];
					    scaleTestGreenBuffer[scaleIndex] = (float)imgP[f*y][f*x];
					}
				}
				
				for (y = 0; y < yDim; y++) {
		    		for (x = 0; x < xDim; x++) {
		    			index = x + y*xDim;
		    			refBuf2D[y][x] = referenceBlueBuffer[index];
		    			testBuf2D[y][x] = testBlueBuffer[index];
		    		}
				}
				imgGT = filter2Same(refBuf2D, lpf);
				imgP = filter2Same(testBuf2D,lpf);
				scaleReferenceBlueBuffer = new float[scaleYDim*scaleXDim];
				scaleTestBlueBuffer = new float[scaleYDim*scaleXDim];
				for (y = 0; y < scaleYDim; y++) {
					for (x = 0; x < scaleXDim; x++) {
					    scaleIndex = x + y*scaleXDim;
					    scaleReferenceBlueBuffer[scaleIndex] = (float)imgGT[f*y][f*x];
					    scaleTestBlueBuffer[scaleIndex] = (float)imgP[f*y][f*x];
					}
				}	
			} // isColor
			refBuf2D = new double[scaleYDim][scaleXDim];
			testBuf2D = new double[scaleYDim][scaleXDim];
    	} // if ((downSampling) && (f > 1))
    	else {
    		if (!isColor) {
    			scaleReferenceBuffer = referenceBuffer;
    			scaleTestBuffer = testBuffer;
    		}
    		else { // isColor
    			scaleReferenceRedBuffer = referenceRedBuffer;
    			scaleTestRedBuffer = testRedBuffer;
    			scaleReferenceGreenBuffer = referenceGreenBuffer;
    			scaleTestGreenBuffer = testGreenBuffer;
    			scaleReferenceBlueBuffer = referenceBlueBuffer;
    			scaleTestBlueBuffer = testBlueBuffer;
    		} // isColor
    	}
    	
        double range;
        if ((referenceImage.getType() == ModelStorageBase.UBYTE) || (referenceImage.getType() == ModelStorageBase.BYTE) ||
        		(referenceImage.getType() == ModelStorageBase.ARGB)) {
        	range = 255.0;
        }
        else {
        	range = referenceImage.getMax() - referenceImage.getMin();
        }
       
        double c1 = k1*k1*range*range;
        double c2 = k2*k2*range*range;
    	double GTGT[][] = new double[scaleYDim][scaleXDim];
    	double PP[][] = new double[scaleYDim][scaleXDim];
    	double GTP[][] = new double[scaleYDim][scaleXDim];
    	
    	double window[][] = new double[ws][ws];
    	double sum = 0.0;
    	for (y = -size1; y <= size2; y++) {
    		for (x = -size1; x <= size2; x++) {
    		    window[y+size1][x+size1] = Math.exp(-((x*x + y*y)/(2.0*sigma*sigma)));
    		    sum += window[y+size1][x+size1];
    		}
    	}
    	if (sum != 0.0) {
	    	for (y = 0; y < ws; y++) {
	    		for (x = 0; x < ws; x++) {
	    			window[y][x] = window[y][x]/sum;
	    		}
	    	}
    	}
    	
    	if (!isColor) {
	    	for (y = 0; y < scaleYDim; y++) {
	    		for (x = 0; x < scaleXDim; x++) {
	    			index = x + y*scaleXDim;
	    			refBuf2D[y][x] = scaleReferenceBuffer[index];
	    			testBuf2D[y][x] = scaleTestBuffer[index];
	    			GTGT[y][x] = refBuf2D[y][x] * refBuf2D[y][x];
	    		    PP[y][x] = testBuf2D[y][x] * testBuf2D[y][x];
	    		    GTP[y][x] = refBuf2D[y][x] * testBuf2D[y][x];
	    		}
	    	}
	    	
	    	double mu1[][] = filter2Valid(refBuf2D, window);
	    	double mu2[][] = filter2Valid(testBuf2D,window);
	    	filtYDim = mu1.length;
	    	filtXDim = mu1[0].length;
	    	double GT_sum_sq[][] = new double[filtYDim][filtXDim];
	    	double P_sum_sq[][] = new double[filtYDim][filtXDim];
	    	double GT_P_sum_mul[][] = new double[filtYDim][filtXDim];
	    	for (y = 0; y < filtYDim; y++) {
	    		for (x = 0; x < filtXDim; x++) {
	    		    GT_sum_sq[y][x] = mu1[y][x] * mu1[y][x];
	    		    P_sum_sq[y][x] = mu2[y][x] * mu2[y][x];
	    		    GT_P_sum_mul[y][x] = mu1[y][x] * mu2[y][x];
	    		}
	    	}
	    	double filtGTGT[][] = filter2Valid(GTGT, window);
	    	double filtPP[][] = filter2Valid(PP, window);
	    	double filtGTP[][] = filter2Valid(GTP, window);
	    	double sigmaGT_sq[][] = new double[filtYDim][filtXDim];
	    	double sigmaP_sq[][] = new double[filtYDim][filtXDim];
	    	double sigmaGT_P[][] = new double[filtYDim][filtXDim];
	    	for (y = 0; y < filtYDim; y++) {
	    		for (x = 0; x < filtXDim; x++) {
	    			sigmaGT_sq[y][x] = filtGTGT[y][x] - GT_sum_sq[y][x];
	    			sigmaP_sq[y][x] = filtPP[y][x] - P_sum_sq[y][x];
	    			sigmaGT_P[y][x] = filtGTP[y][x] - GT_P_sum_mul[y][x];
	    		}
	    	}
	    	double ssim_map[][] = new double[filtYDim][filtXDim];
	    	if ((c1 > 0) && (c2 > 0)) {
	    		for (y = 0; y < filtYDim; y++) {
		    		for (x = 0; x < filtXDim; x++) {
		    			ssim_map[y][x] = ((2*GT_P_sum_mul[y][x] + c1)*(2*sigmaGT_P[y][x] + c2))/
		    					((GT_sum_sq[y][x] + P_sum_sq[y][x] + c1)*(sigmaGT_sq[y][x] + sigmaP_sq[y][x] + c2));	
		    		}
	    		}
	    	} // if ((c1 > 0) && (c2 > 0))
	    	else {
	    		for (y = 0; y < filtYDim; y++) {
		    		for (x = 0; x < filtXDim; x++) {
		    			double numerator1 = 2*GT_P_sum_mul[y][x] + c1;
		    			double numerator2 = 2*sigmaGT_P[y][x] + c2;
		    		    double denominator1 = GT_sum_sq[y][x] + P_sum_sq[y][x] + c1;
		    			double denominator2 = sigmaGT_sq[y][x] + sigmaP_sq[y][x] + c2;
		    			ssim_map[y][x] = 1.0;
		    			if (denominator1*denominator2 > 0) {
		    			   ssim_map[y][x] = (numerator1*numerator2)/(denominator1*denominator2);
		    			}
		    			if ((denominator1 != 0) & (denominator2 == 0)) {
		    			   ssim_map[y][x] = numerator1/denominator1;
		    			}
		    		}
	    		}
	    	}
	    	sum = 0.0;
	    	for (y = 0; y < filtYDim; y++) {
	    		for (x = 0; x < filtXDim; x++) {
	    			sum += ssim_map[y][x];
	    		}
	    	}
	    	structuralSimilarityIndex = sum/(filtXDim * filtYDim);
    	}
    	else { // isColor
	    	for (y = 0; y < scaleYDim; y++) {
	    		for (x = 0; x < scaleXDim; x++) {
	    			index = x + y*scaleXDim;
	    			refBuf2D[y][x] = scaleReferenceRedBuffer[index];
	    			testBuf2D[y][x] = scaleTestRedBuffer[index];
	    			GTGT[y][x] = refBuf2D[y][x] * refBuf2D[y][x];
	    		    PP[y][x] = testBuf2D[y][x] * testBuf2D[y][x];
	    		    GTP[y][x] = refBuf2D[y][x] * testBuf2D[y][x];
	    		}
	    	}
	    	
	    	double mu1[][] = filter2Valid(refBuf2D, window);
	    	double mu2[][] = filter2Valid(testBuf2D,window);
	    	filtYDim = mu1.length;
	    	filtXDim = mu1[0].length;
	    	double GT_sum_sq[][] = new double[filtYDim][filtXDim];
	    	double P_sum_sq[][] = new double[filtYDim][filtXDim];
	    	double GT_P_sum_mul[][] = new double[filtYDim][filtXDim];
	    	for (y = 0; y < filtYDim; y++) {
	    		for (x = 0; x < filtXDim; x++) {
	    		    GT_sum_sq[y][x] = mu1[y][x] * mu1[y][x];
	    		    P_sum_sq[y][x] = mu2[y][x] * mu2[y][x];
	    		    GT_P_sum_mul[y][x] = mu1[y][x] * mu2[y][x];
	    		}
	    	}
	    	double filtGTGT[][] = filter2Valid(GTGT, window);
	    	double filtPP[][] = filter2Valid(PP, window);
	    	double filtGTP[][] = filter2Valid(GTP, window);
	    	double sigmaGT_sq[][] = new double[filtYDim][filtXDim];
	    	double sigmaP_sq[][] = new double[filtYDim][filtXDim];
	    	double sigmaGT_P[][] = new double[filtYDim][filtXDim];
	    	for (y = 0; y < filtYDim; y++) {
	    		for (x = 0; x < filtXDim; x++) {
	    			sigmaGT_sq[y][x] = filtGTGT[y][x] - GT_sum_sq[y][x];
	    			sigmaP_sq[y][x] = filtPP[y][x] - P_sum_sq[y][x];
	    			sigmaGT_P[y][x] = filtGTP[y][x] - GT_P_sum_mul[y][x];
	    		}
	    	}
	    	double ssim_map[][] = new double[filtYDim][filtXDim];
	    	if ((c1 > 0) && (c2 > 0)) {
	    		for (y = 0; y < filtYDim; y++) {
		    		for (x = 0; x < filtXDim; x++) {
		    			ssim_map[y][x] = ((2*GT_P_sum_mul[y][x] + c1)*(2*sigmaGT_P[y][x] + c2))/
		    					((GT_sum_sq[y][x] + P_sum_sq[y][x] + c1)*(sigmaGT_sq[y][x] + sigmaP_sq[y][x] + c2));	
		    		}
	    		}
	    	} // if ((c1 > 0) && (c2 > 0))
	    	else {
	    		for (y = 0; y < filtYDim; y++) {
		    		for (x = 0; x < filtXDim; x++) {
		    			double numerator1 = 2*GT_P_sum_mul[y][x] + c1;
		    			double numerator2 = 2*sigmaGT_P[y][x] + c2;
		    		    double denominator1 = GT_sum_sq[y][x] + P_sum_sq[y][x] + c1;
		    			double denominator2 = sigmaGT_sq[y][x] + sigmaP_sq[y][x] + c2;
		    			ssim_map[y][x] = 1.0;
		    			if (denominator1*denominator2 > 0) {
		    			   ssim_map[y][x] = (numerator1*numerator2)/(denominator1*denominator2);
		    			}
		    			if ((denominator1 != 0) & (denominator2 == 0)) {
		    			   ssim_map[y][x] = numerator1/denominator1;
		    			}
		    		}
	    		}
	    	}
	    	sum = 0.0;
	    	for (y = 0; y < filtYDim; y++) {
	    		for (x = 0; x < filtXDim; x++) {
	    			sum += ssim_map[y][x];
	    		}
	    	}
	    	
	    	for (y = 0; y < scaleYDim; y++) {
	    		for (x = 0; x < scaleXDim; x++) {
	    			index = x + y*scaleXDim;
	    			refBuf2D[y][x] = scaleReferenceGreenBuffer[index];
	    			testBuf2D[y][x] = scaleTestGreenBuffer[index];
	    			GTGT[y][x] = refBuf2D[y][x] * refBuf2D[y][x];
	    		    PP[y][x] = testBuf2D[y][x] * testBuf2D[y][x];
	    		    GTP[y][x] = refBuf2D[y][x] * testBuf2D[y][x];
	    		}
	    	}
	    	mu1 = filter2Valid(refBuf2D, window);
	    	mu2 = filter2Valid(testBuf2D,window);
	    	for (y = 0; y < filtYDim; y++) {
	    		for (x = 0; x < filtXDim; x++) {
	    		    GT_sum_sq[y][x] = mu1[y][x] * mu1[y][x];
	    		    P_sum_sq[y][x] = mu2[y][x] * mu2[y][x];
	    		    GT_P_sum_mul[y][x] = mu1[y][x] * mu2[y][x];
	    		}
	    	}
	    	filtGTGT = filter2Valid(GTGT, window);
	    	filtPP = filter2Valid(PP, window);
	    	filtGTP = filter2Valid(GTP, window);
	    	for (y = 0; y < filtYDim; y++) {
	    		for (x = 0; x < filtXDim; x++) {
	    			sigmaGT_sq[y][x] = filtGTGT[y][x] - GT_sum_sq[y][x];
	    			sigmaP_sq[y][x] = filtPP[y][x] - P_sum_sq[y][x];
	    			sigmaGT_P[y][x] = filtGTP[y][x] - GT_P_sum_mul[y][x];
	    		}
	    	}
	    	if ((c1 > 0) && (c2 > 0)) {
	    		for (y = 0; y < filtYDim; y++) {
		    		for (x = 0; x < filtXDim; x++) {
		    			ssim_map[y][x] = ((2*GT_P_sum_mul[y][x] + c1)*(2*sigmaGT_P[y][x] + c2))/
		    					((GT_sum_sq[y][x] + P_sum_sq[y][x] + c1)*(sigmaGT_sq[y][x] + sigmaP_sq[y][x] + c2));	
		    		}
	    		}
	    	} // if ((c1 > 0) && (c2 > 0))
	    	else {
	    		for (y = 0; y < filtYDim; y++) {
		    		for (x = 0; x < filtXDim; x++) {
		    			double numerator1 = 2*GT_P_sum_mul[y][x] + c1;
		    			double numerator2 = 2*sigmaGT_P[y][x] + c2;
		    		    double denominator1 = GT_sum_sq[y][x] + P_sum_sq[y][x] + c1;
		    			double denominator2 = sigmaGT_sq[y][x] + sigmaP_sq[y][x] + c2;
		    			ssim_map[y][x] = 1.0;
		    			if (denominator1*denominator2 > 0) {
		    			   ssim_map[y][x] = (numerator1*numerator2)/(denominator1*denominator2);
		    			}
		    			if ((denominator1 != 0) & (denominator2 == 0)) {
		    			   ssim_map[y][x] = numerator1/denominator1;
		    			}
		    		}
	    		}
	    	}
	    	for (y = 0; y < filtYDim; y++) {
	    		for (x = 0; x < filtXDim; x++) {
	    			sum += ssim_map[y][x];
	    		}
	    	}
	    	
	    	for (y = 0; y < scaleYDim; y++) {
	    		for (x = 0; x < scaleXDim; x++) {
	    			index = x + y*scaleXDim;
	    			refBuf2D[y][x] = scaleReferenceBlueBuffer[index];
	    			testBuf2D[y][x] = scaleTestBlueBuffer[index];
	    			GTGT[y][x] = refBuf2D[y][x] * refBuf2D[y][x];
		    		PP[y][x] = testBuf2D[y][x] * testBuf2D[y][x];
		    		GTP[y][x] = refBuf2D[y][x] * testBuf2D[y][x];
	    		}
	    	}
	    	mu1 = filter2Valid(refBuf2D, window);
	    	mu2 = filter2Valid(testBuf2D,window);
	    	for (y = 0; y < filtYDim; y++) {
	    		for (x = 0; x < filtXDim; x++) {
	    		    GT_sum_sq[y][x] = mu1[y][x] * mu1[y][x];
	    		    P_sum_sq[y][x] = mu2[y][x] * mu2[y][x];
	    		    GT_P_sum_mul[y][x] = mu1[y][x] * mu2[y][x];
	    		}
	    	}
	    	filtGTGT = filter2Valid(GTGT, window);
	    	filtPP = filter2Valid(PP, window);
	    	filtGTP = filter2Valid(GTP, window);
	    	for (y = 0; y < filtYDim; y++) {
	    		for (x = 0; x < filtXDim; x++) {
	    			sigmaGT_sq[y][x] = filtGTGT[y][x] - GT_sum_sq[y][x];
	    			sigmaP_sq[y][x] = filtPP[y][x] - P_sum_sq[y][x];
	    			sigmaGT_P[y][x] = filtGTP[y][x] - GT_P_sum_mul[y][x];
	    		}
	    	}
	    	if ((c1 > 0) && (c2 > 0)) {
	    		for (y = 0; y < filtYDim; y++) {
		    		for (x = 0; x < filtXDim; x++) {
		    			ssim_map[y][x] = ((2*GT_P_sum_mul[y][x] + c1)*(2*sigmaGT_P[y][x] + c2))/
		    					((GT_sum_sq[y][x] + P_sum_sq[y][x] + c1)*(sigmaGT_sq[y][x] + sigmaP_sq[y][x] + c2));	
		    		}
	    		}
	    	} // if ((c1 > 0) && (c2 > 0))
	    	else {
	    		for (y = 0; y < filtYDim; y++) {
		    		for (x = 0; x < filtXDim; x++) {
		    			double numerator1 = 2*GT_P_sum_mul[y][x] + c1;
		    			double numerator2 = 2*sigmaGT_P[y][x] + c2;
		    		    double denominator1 = GT_sum_sq[y][x] + P_sum_sq[y][x] + c1;
		    			double denominator2 = sigmaGT_sq[y][x] + sigmaP_sq[y][x] + c2;
		    			ssim_map[y][x] = 1.0;
		    			if (denominator1*denominator2 > 0) {
		    			   ssim_map[y][x] = (numerator1*numerator2)/(denominator1*denominator2);
		    			}
		    			if ((denominator1 != 0) & (denominator2 == 0)) {
		    			   ssim_map[y][x] = numerator1/denominator1;
		    			}
		    		}
	    		}
	    	}
	    	for (y = 0; y < filtYDim; y++) {
	    		for (x = 0; x < filtXDim; x++) {
	    			sum += ssim_map[y][x];
	    		}
	    	}
	    	structuralSimilarityIndex = sum/(3.0*filtXDim*filtYDim);	
    	} // isColor
    	UI.setDataText("Structural similarity index = " + structuralSimilarityIndex + "\n");
    	System.out.println("Structural similarity index = " + structuralSimilarityIndex);
    }
    
    private void sam() {
    	 // calculates spectral angle mapper (sam).

    	// param GT: first (original) input image.
    	// param P: second (deformed) input image.

    	// returns:  float -- sam value.
    	int x,y;
    	int index;
    	double dotProduct =  0.0;
    	double referenceSumOfSquares = 0.0;
    	double testSumOfSquares = 0.0;
    	double value;
    	double red_sam_mean;
    	double green_sam_mean;
    	double blue_sam_mean;
    	if (!isColor) {
    	     for (y = 0; y < yDim; y++) {
    	    	 for (x = 0; x < xDim; x++) {
    	    		 index = x + y*xDim;
    	    		 dotProduct += (referenceBuffer[index]*testBuffer[index]);
    	    		 referenceSumOfSquares += (referenceBuffer[index]*referenceBuffer[index]);
    	    		 testSumOfSquares += (testBuffer[index]*testBuffer[index]);
    	    	 }	 
    	     }
    	     value = dotProduct/(Math.sqrt(referenceSumOfSquares) * Math.sqrt(testSumOfSquares));
    	     if (value < -1) {
    	    	 value = -1.0;
    	     }
    	     if (value > 1.0) {
    	    	 value = 1.0;
    	     }
    	     sam_mean = Math.acos(value);
    	} // if (!isColor)
    	else { // isColor
    		for (y = 0; y < yDim; y++) {
   	    	    for (x = 0; x < xDim; x++) {
   	    		   index = x + y*xDim;
   	    		   dotProduct += (referenceRedBuffer[index]*testRedBuffer[index]);
   	    		   referenceSumOfSquares += (referenceRedBuffer[index]*referenceRedBuffer[index]);
   	    		   testSumOfSquares += (testRedBuffer[index]*testRedBuffer[index]);
   	    	    }	 
	   	     }
	   	     value = dotProduct/(Math.sqrt(referenceSumOfSquares) * Math.sqrt(testSumOfSquares));
	   	     if (value < -1) {
	   	    	 value = -1.0;
	   	     }
	   	     if (value > 1.0) {
	   	    	 value = 1.0;
	   	     }
	   	     red_sam_mean = Math.acos(value);
   	     
	   	     dotProduct =  0.0;
	    	 referenceSumOfSquares = 0.0;
	         testSumOfSquares = 0.0;
	   	     for (y = 0; y < yDim; y++) {
	 	    	 for (x = 0; x < xDim; x++) {
	 	    		 index = x + y*xDim;
	 	    		 dotProduct += (referenceGreenBuffer[index]*testGreenBuffer[index]);
	 	    		 referenceSumOfSquares += (referenceGreenBuffer[index]*referenceGreenBuffer[index]);
	 	    		 testSumOfSquares += (testGreenBuffer[index]*testGreenBuffer[index]);
	 	    	 }	 
	 	     }
	 	     value = dotProduct/(Math.sqrt(referenceSumOfSquares) * Math.sqrt(testSumOfSquares));
	 	     if (value < -1) {
	 	    	 value = -1.0;
	 	     }
	 	     if (value > 1.0) {
	 	    	 value = 1.0;
	 	     }
	 	     green_sam_mean = Math.acos(value);
	 	     
	 	     dotProduct =  0.0;
	    	 referenceSumOfSquares = 0.0;
	         testSumOfSquares = 0.0;
	 	     for (y = 0; y < yDim; y++) {
	 	    	 for (x = 0; x < xDim; x++) {
	 	    		 index = x + y*xDim;
	 	    		 dotProduct += (referenceBlueBuffer[index]*testBlueBuffer[index]);
	 	    		 referenceSumOfSquares += (referenceBlueBuffer[index]*referenceBlueBuffer[index]);
	 	    		 testSumOfSquares += (testBlueBuffer[index]*testBlueBuffer[index]);
	 	    	 }	 
	 	     }
	 	     value = dotProduct/(Math.sqrt(referenceSumOfSquares) * Math.sqrt(testSumOfSquares));
	 	     if (value < -1) {
	 	    	 value = -1.0;
	 	     }
	 	     if (value > 1.0) {
	 	    	 value = 1.0;
	 	     }
	 	     blue_sam_mean = Math.acos(value);
	 	     sam_mean = (red_sam_mean + green_sam_mean + blue_sam_mean)/3.0;
    	} // isColor
    	UI.setDataText("Spectral angle mapper = " + sam_mean + "\n");
    	System.out.println("Spectral angle mapper = " + sam_mean);
    	
    }
    
    private void psnrb() {
    	// Calculates PSNR with Blocking Effect Factor for a given pair of images (PSNR-B)

    	// param GT: first (original) input image in YCbCr format or Grayscale.
    	// param P: second (corrected) input image in YCbCr format or Grayscale..
    	// return: float -- psnr_b.
    	double imdiff[];
    	int i;
    	double bef;
    	double range;
    	double maxGT;
    	double minGT;
    	if ((referenceImage.getType() == ModelStorageBase.UBYTE) || (referenceImage.getType() == ModelStorageBase.BYTE)) {
    		range = 255.0;
    	}
    	else if (isColor) {
    		range = 1.0;
    	}
    	else {
    		minGT = Double.MAX_VALUE;
    		maxGT = -Double.MAX_VALUE;
    		for (i = 0; i < referenceBuffer.length; i++) {
    			if (referenceBuffer[i] > maxGT) {
    				maxGT = referenceBuffer[i];
    			}
    			if (referenceBuffer[i] < minGT) {
    				minGT = referenceBuffer[i];
    			}
    		}
    		range = maxGT - minGT;
    	}
    	if (!isColor) {
    	    imdiff = new double[referenceBuffer.length];	
    	    for (i = 0; i < referenceBuffer.length; i++) {
    	    	imdiff[i] = referenceBuffer[i] - testBuffer[i];
    	    }
    	    bef = _compute_bef(testBuffer, 8);
    	    
    	} // if (!isColor)
    	else { // isColor
    	    imdiff = new double[referenceYBuffer.length];
    	    for (i = 0; i < referenceYBuffer.length; i++) {
    	    	imdiff[i] = referenceYBuffer[i] - testYBuffer[i];
    	    }
    	    bef = _compute_bef(testYBuffer, 8);
    	    
    	} // isColor
    	double total = 0.0;
    	for (i = 0; i < imdiff.length; i++) {
    		total += (imdiff[i] * imdiff[i]);
    	}
    	double mse = total/imdiff.length;
    	double mse_b = mse + bef;
    	
    	/*if (maxP > 2) {
    		psnr_b = 10 * Math.log10((255.0*255.0)/mse_b);
    	}
    	else {
    		psnr_b = 10 * Math.log10(1.0/mse_b);
    	}*/
    	psnr_b = 10 * Math.log10(range*range/mse_b);
    	
    	UI.setDataText("PSNR with Blocking Effect Factor = " + psnr_b + "\n");
    	System.out.println("PSNR with Blocking Effect Factor = " + psnr_b);
    	return;
    }
    
    private double _compute_bef(double im[], int block_size) {
    	// Calculates Blocking Effect Factor (BEF) for a given grayscale/one channel image

    	//C. Yim and A. C. Bovik, "Quality Assessment of Deblocked Images," in IEEE Transactions on Image Processing,
    		// vol. 20, no. 1, pp. 88-98, Jan. 2011.

    	// param im: input image (numpy ndarray)
    	// param block_size: Size of the block over which DCT was performed during compression
    	// return: float -- bef.
    	
        int i,j,index,x,y;
    	int h[] = new int[xDim-1]; 
    	for (i = 0; i < xDim-1; i++) {
    		h[i] = i;
    	}
    	int num_h_b = (xDim-1)/block_size;
    	int h_b[] = new int[num_h_b];
    	for (i = 0; i < num_h_b; i++) {
    		h_b[i] = block_size-1 + i*block_size;
    	}
    	int h_bc[] = new int[h.length - h_b.length];
    	index = 0;
    	for (j = 0; j < num_h_b; j++) {
	    	for (i = 0; i < block_size-1; i++) {
	    		h_bc[index++] = j*block_size + i;
	    	}
    	}

    	int v[] = new int[yDim-1];
    	for (i = 0; i < yDim-1; i++) {
    		v[i] = i;
    	}
    	int num_v_b = (yDim-1)/block_size;
    	int v_b[] = new int[num_v_b];
        for (i = 0; i < num_v_b; i++) {
        	v_b[i] = block_size-1 + i*block_size;
        }
        int v_bc[] = new int[v.length - v_b.length];
        index = 0;
    	for (j = 0; j < num_v_b; j++) {
	    	for (i = 0; i < block_size-1; i++) {
	    		v_bc[index++] = j*block_size + i;
	    	}
    	}
        
    	
    	double d_b = 0;
    	double d_bc = 0;
    	double diff;

    	// h_b for loop
    	for (j = 0; j < h_b.length; j++) {
    		x = h_b[j];
    		for (y = 0; y < yDim; y++) {
    			diff = im[x + y*xDim] - im[x+1 + y*xDim];
    		    d_b += (diff*diff);
    		}
    	}

    	// h_bc for loop
    	for (j = 0; j < h_bc.length; j++) {
    		x = h_bc[j];
    		for (y = 0; y < yDim; y++) {
    			diff = im[x + y*xDim] - im[x+1 + y*xDim];
    		    d_bc += (diff*diff);
    		}
    	}

    	// v_b for loop
    	for (j = 0; j < v_b.length; j++) {
    		y = v_b[j];
    		for (x = 0; x < xDim; x++) {
    			diff = im[x + y*xDim] - im[x + (y+1)*xDim];
    			d_b += (diff*diff);
    		}
    	}

    	// V_bc for loop
    	for (j = 0; j < v_bc.length; j++) {
    		y = v_bc[j];
    		for (x = 0; x < xDim; x++) {
    		    diff = im[x + y*xDim] - im[x + (y+1)*xDim];
    		    d_bc += (diff*diff);
    		}
    	}

    	// N code
    	double n_hb = yDim * ((double)xDim/(double)block_size) - 1;
    	double n_hbc = (yDim * (xDim - 1)) - n_hb;
    	double n_vb = xDim * ((double)yDim/(double)block_size) - 1;
    	double n_vbc = (xDim * (yDim - 1)) - n_vb;

    	// D code
    	d_b /= (n_hb + n_vb);
    	d_bc /= (n_hbc + n_vbc);

    	// Log
    	double t;
    	if (d_b > d_bc) {
    		t = log2(block_size)/log2(Math.min(yDim, xDim));
    	}
    	else {
    		t = 0;
    	}

    	// BEF
    	double bef = t*(d_b - d_bc);

    	return bef;
    }
    
    private double log2(double input) {
        return (Math.log10(input) / Math.log10(2.0));
	 }
    
    private void vifp() {
    	// calculates Pixel Based Visual Information Fidelity (vif-p).

    	// param GT: first (original) input image.
    	// param P: second (deformed) input image.
    	// param sigma_nsq: variance of the visual noise (default = 2)

    	// returns:  float -- vif-p value.
    	double eps = 1.0E-10;
    	double num = 0.0;
    	double den = 0.0;
    	int scale;
    	int N;
    	double win[][];
    	int	ws;
    	double sigma;
        double GT[][] = null;
        double P[][] = null;
        double GTFilter[][];
        double PFilter[][];
        double total;
        int x,y,index;
        int c;
        int numColor;
        int scaleYDim = yDim;
        int scaleXDim = xDim;
        double mu1[][];
        double mu2[][];
        double GT_sum_sq;
        double P_sum_sq;
        double GT_P_sum_mul;
        double GTGT[][];
        double PP[][];
        double GTP[][];
        double GTGTFilter[][];
        double PPFilter[][];
        double GTPFilter[][];
        double sigmaGT_sq;
        double sigmaP_sq;
        double sigmaGT_P;
        double g;
        double sv_sq;
        double red_vifp_mean = 0.0;
        double green_vifp_mean = 0.0;
        double blue_vifp_mean = 0.0;
        
        if (isColor) {
        	numColor = 3;
        }
        else {
        	numColor = 1;
        }
        for (c = 0; c < numColor; c++) {
	    	for (scale = 1; scale <= 4; scale++) {
	    	    N = (int)Math.round(Math.pow(2.0,5-scale)+1);
	    	    ws = N;
	    	    sigma = N/5.0;
	    	    win = new double[ws][ws];
	    	    total = 0.0;
	    	    for (y = -N/2; y <= N/2; y++) {
	    	    	for (x = -N/2; x <= N/2; x++) {
	    	    		win[y+N/2][x+N/2] = Math.exp(-((x*x + y*y)/(2.0*sigma*sigma)));
	    	    		total += win[y+N/2][x+N/2];
	    	    	}
	    	    }
	    	    for (y = 0; y < N; y++) {
	    	    	for (x = 0; x < N; x++) {
	    	    		win[y][x] = win[y][x]/total;
	    	    	}
	    	    }
	    	    if (scale == 1) {
	    	    	GT = new double[yDim][xDim];
	    	    	P = new double[yDim][xDim];
	    	    	for (y = 0; y < yDim; y++) {
	    	    		for (x = 0; x < xDim; x++) {
	    	    			index = x + y*xDim;
	    	    			if (!isColor) {
	    	    			    GT[y][x] = referenceBuffer[index];
	    	    			    P[y][x] = testBuffer[index];
	    	    			}
	    	    			else if (c == 0) {
	    	    				GT[y][x] = referenceRedBuffer[index];
	    	    				P[y][x] = testRedBuffer[index];
	    	    			}
	    	    			else if (c == 1) {
	    	    				GT[y][x] = referenceGreenBuffer[index];
	    	    				P[y][x] = testGreenBuffer[index];
	    	    			}
	    	    			else {
	    	    				GT[y][x] = referenceBlueBuffer[index];
	    	    				P[y][x] = testBlueBuffer[index];
	    	    			}
	    	    		}
	    	    	}
	    	    } // if (scale == 1)
	    	    else { // scale > 1
	                GTFilter = filter2Valid(GT,win);
	                PFilter = filter2Valid(P,win);
	                scaleYDim = GTFilter.length/2;
	                scaleXDim = GTFilter[0].length/2;
	                GT = new double[scaleYDim][scaleXDim];
	                P = new double[scaleYDim][scaleXDim];
	                for (y = 0; y < scaleYDim; y++) {
	                	for (x = 0; x < scaleXDim; x++) {
	                		GT[y][x] = GTFilter[2*y][2*x];
	                		P[y][x] = PFilter[2*y][2*x];
	                	}
	                }
	    	    } // scale > 1
	    	    GTGT = new double[GT.length][GT[0].length];
	    	    PP = new double[GT.length][GT[0].length];
	    	    GTP = new double[GT.length][GT[0].length];
	    	    for (y = 0; y < GT.length; y++) {
	    	    	for (x = 0; x < GT[0].length; x++) {
	    	    		GTGT[y][x] = GT[y][x]*GT[y][x];
	    	    		PP[y][x] = P[y][x]*P[y][x];
	    	    		GTP[y][x] = GT[y][x]*P[y][x];
	    	    	}
	    	    }
	    	    mu1 = filter2Valid(GT,win);
	    	    mu2 = filter2Valid(P,win);
	    	    GTGTFilter = filter2Valid(GTGT, win);
	    	    PPFilter = filter2Valid(PP, win);
	    	    GTPFilter = filter2Valid(GTP, win);
	    	    scaleYDim = mu1.length;
	    	    scaleXDim = mu1[0].length;
	    	    for (y = 0; y < scaleYDim; y++) {
	    	    	for (x = 0; x < scaleXDim; x++) {
	    	    		GT_sum_sq = mu1[y][x] * mu1[y][x];
	    	    		P_sum_sq = mu2[y][x] * mu2[y][x];
	    	    		GT_P_sum_mul = mu1[y][x] * mu2[y][x];
	    	    		sigmaGT_sq = Math.max(0,GTGTFilter[y][x] - GT_sum_sq);
	    	    		sigmaP_sq = Math.max(0,PPFilter[y][x] - P_sum_sq);
	    	    		sigmaGT_P = GTPFilter[y][x] - GT_P_sum_mul;
	    	    		g=sigmaGT_P /(sigmaGT_sq+eps);
	    	    		sv_sq=sigmaP_sq-g*sigmaGT_P;
	    	    		if (sigmaGT_sq<eps) {
	    	    		    g = 0.0;
	    	    		    sv_sq = sigmaP_sq;
	    	    		    sigmaGT_sq = 0.0;
	    	    		}
	    	    		if (sigmaP_sq<eps) {
	    	    		    g = 0.0;
	    	    		    sv_sq = 0.0;
	    	    		}
	    	    		if (g < 0.0) {
	    	    		    sv_sq = sigmaP_sq;
	    	    		    g = 0.0;
	    	    		}
	    	    		if (sv_sq < eps) {
	    	    			sv_sq = eps;
	    	    		}
	    	    		num += Math.log10(1.0 + (g*g)*sigmaGT_sq/(sv_sq+sigma_nsq));
	    	    		den += Math.log10(1.0+sigmaGT_sq/sigma_nsq);
	    	    	}
	    	    }
	    	} // for (scale = 1; scale <= 4; scale++)
	    	if (!isColor) {
	    		vifp_mean = num/den;
	    	}
	    	else if (c == 0) {
	    		red_vifp_mean = num/den;
	    	}
	    	else if (c == 1) {
	    		green_vifp_mean = num/den;
	    	}
	    	else {
	    		blue_vifp_mean = num/den;
	    	}
        } // for (c = 0; c < numColor; c++)
        if (isColor) {
        	vifp_mean = (red_vifp_mean + green_vifp_mean + blue_vifp_mean)/3.0;
        }
        UI.setDataText("Visual information fidelity = " + vifp_mean + "\n");
        System.out.println("Visual information fidelity = " + vifp_mean);
        return;
    }
	
}