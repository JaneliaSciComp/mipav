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

Copyright (c) 2018 Andrew Khalel

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
	public final int UNIVERSAL_QUALITY_IMAGE_INDEX = 5;
	public final int MULTI_SCALE_STRUCTURAL_SIMILARITY_INDEX = 6;
	public final int ERGAS = 7;
	public final int SPATIAL_CORRELATION_COEFFICIENT = 8;
	public final int RELATIVE_AVERAGE_SPECTRAL_ERROR = 9;
	public final int SPECTRAL_ANGLE_MAPPER = 10;
	public final int SPECTRAL_DISTORTION_INDEX = 11;
	public final int SPATIAL_DISTORTION_INDEX = 12;
	public final int QUALITY_WITH_NO_REFERENCE = 13;
 	public final int BLOCK_SENSITIVE_PEAK_SIGNAL_TO_NOISE_RATIO = 14;
 	public final int RMSE_SW = 15;
 	
 	private double testBuffer[] = null;
    private float testRedBuffer[] = null;
    private float testGreenBuffer[] = null;
    private float testBlueBuffer[] = null;
    private double referenceBuffer[] = null;
    private float referenceRedBuffer[] = null;
    private float referenceGreenBuffer[] = null;
    private float referenceBlueBuffer[] = null;
    private double testYBuffer[];
    private double testCrBuffer[];
    private double testCbBuffer[];
    private double referenceYBuffer[];
    private double referenceCrBuffer[];
    private double referenceCbBuffer[];
    private double rmse_sw_mean;
    private double uqi_mean;
    private int length = 1;
    private boolean onlyTestImageRequired = false;
    private boolean YCrCbRequired = false;
    private boolean isColor = false;
    
    private double meanSquareError;
    private double meanRedSquareError;
    private double meanGreenSquareError;
    private double meanBlueSquareError;
    private double rootMeanSquareError;
    private double peakSignalToNoiseRatio;
    private double universalImageQualityIndex;
    
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
    // Defualt = 8 for UNIVERSAL_QUALITY_IMAGE_INDEX
    private int ws;
    
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
		ImageQuality iq = new ImageQuality(clr, clr, metrics,ws, results);
		iq.runAlgorithm();
		if (results[0] != 0.0) {
			System.err.println("Mean squared error = " + results[0] + " for clr, clr\n");
			testsFailed++;
		}
		
		iq = new ImageQuality(gry, gry, metrics, ws, results);
		iq.runAlgorithm();
		if (results[0] != 0.0) {
			System.err.println("Mean squared error = " + results[0] + " for gry, gry\n");
			testsFailed++;
		}
		
		iq = new ImageQuality(clr, clr_noise, metrics, ws, results);
		iq.runAlgorithm();
		if ((Math.abs(results[0] - 2391.465875)) > eps) {
			System.err.println("Mean squared error = " + results[0] + " for clr, clr_noise\n");
			testsFailed++;
		}
		
		iq = new ImageQuality(gry, gry_noise, metrics, ws, results);
		iq.runAlgorithm();
		if ((Math.abs(results[0] - 2025.913940)) > eps) {
			System.err.println("Mean squared error = " + results[0] + " for gry, gry_noise\n");
			testsFailed++;
		}
		
		iq = new ImageQuality(clr, clr_const, metrics, ws,results);
		iq.runAlgorithm();
		if ((Math.abs(results[0] - 2302.953958)) > eps) {
			System.err.println("Mean squared error = " + results[0] + " for clr, clr_const\n");
			testsFailed++;
		}
		
		iq = new ImageQuality(gry, gry_const, metrics, ws,results);
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
		ImageQuality iq = new ImageQuality(clr, clr, metrics,ws,results);
		iq.runAlgorithm();
		if (results[0] != 0.0) {
			System.err.println("Root mean squared error = " + results[0] + " for clr, clr\n");
			testsFailed++;
		}
		
		iq = new ImageQuality(gry, gry, metrics, ws, results);
		iq.runAlgorithm();
		if (results[0] != 0.0) {
			System.err.println("Root mean squared error = " + results[0] + " for gry, gry\n");
			testsFailed++;
		}
		
		metrics = new int[] {RMSE_SW};
		ws = 8;
		iq = new ImageQuality(clr, clr, metrics,ws,results);
		iq.runAlgorithm();
		if (results[0] != 0.0) {
			System.err.println("Root mean squared error sliding window = " + results[0] + " for clr, clr\n");
			testsFailed++;
		}
		
		iq = new ImageQuality(gry, gry, metrics, ws, results);
		iq.runAlgorithm();
		if (results[0] != 0.0) {
			System.err.println("Root mean squared error sliding window = " + results[0] + " for gry, gry\n");
			testsFailed++;
		}
		
		metrics = new int[] {ROOT_MEAN_SQUARED_ERROR};
		iq = new ImageQuality(gry, gry_const, metrics, ws,results);
		iq.runAlgorithm();
		double rmse = results[0];
		metrics = new int[] {RMSE_SW};
		ws = 510;
		iq = new ImageQuality(gry, gry_const, metrics, ws,results);
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
		// Taking maxValue = 255 instead of the actual referenceImage.getMax() causes all tests to pass.
		// However, this version of the software will use the actual referenceImage.getMax().
		int testsFailed = 0;
		double eps = 1.0E-3;
		metrics = new int[] {PEAK_SIGNAL_TO_NOISE_RATIO};
		results = new double[1];
		ImageQuality iq = new ImageQuality(clr, clr, metrics,ws,results);
		iq.runAlgorithm();
		if (!Double.isInfinite(results[0])) {
			System.err.println("Peak signal to noise ratio = " + results[0] + " for clr, clr\n");
			testsFailed++;
		}
		
		iq = new ImageQuality(gry, gry, metrics, ws, results);
		iq.runAlgorithm();
		if (!Double.isInfinite(results[0])) {
			System.err.println("Peak signal to noise ratio = " + results[0] + " for gry, gry\n");
			testsFailed++;
		}
		
		iq = new ImageQuality(clr, clr_noise, metrics, ws, results);
		iq.runAlgorithm();
		if ((Math.abs(results[0] - 14.344162)) > eps) {
			System.err.println("Peak signal to noise ratio = " + results[0] + " for clr, clr_noise\n");
			testsFailed++;
		}
		
		iq = new ImageQuality(gry, gry_noise, metrics, ws, results);
		iq.runAlgorithm();
		if ((Math.abs(results[0] - 15.064594)) > eps) {
			System.err.println("Peak signal to noise ratio = " + results[0] + " for gry, gry_noise\n");
			testsFailed++;
		}
		
		iq = new ImageQuality(clr, clr_const, metrics, ws,results);
		iq.runAlgorithm();
		if ((Math.abs(results[0] - 14.507951)) > eps) {
			System.err.println("Peak signal to noise ratio = " + results[0] + " for clr, clr_const\n");
			testsFailed++;
		}
		
		iq = new ImageQuality(gry, gry_const, metrics, ws,results);
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
	
	public ImageQuality(ModelImage referenceImage, ModelImage testImage, int metrics[], int ws, double results[]) {
		if (metrics == null) {
			MipavUtil.displayError("metrics is null in ImageQuality");
			return;
		}
		if (metrics.length == 0) {
			MipavUtil.displayError("metrics.length == 0 in ImageQuality");
			return;
		}
		if (metrics.length > 15) {
			MipavUtil.displayError("metrics.length > 15 in ImageQuality");
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
			if ((metrics[i] < 1) || (metrics[i] > 15)) {
				MipavUtil.displayError("Illegal metrics[" + i+ "] in ImageQuality");
				return;
			}
			if ((metrics[i] == QUALITY_WITH_NO_REFERENCE) && (metrics.length == 1)) {
				onlyTestImageRequired = true;
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
	    if (!onlyTestImageRequired) {
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
	    } // if (!onlyTestImageRequired)
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
	    } // else isColor
	    
	    if (!onlyTestImageRequired) {
	    	if (!isColor) {
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
	    } // if (!onlyTestImageRequired)
	    
	    if (YCrCbRequired) {
            testYBuffer = new double[length];
            testCrBuffer = new double[length];
            testCbBuffer =  new double[length];
            referenceYBuffer = new double[length];
            referenceCrBuffer = new double[length];
            referenceCbBuffer = new double[length];
            double delta;
            if (referenceImage.getType() == ModelStorageBase.ARGB) {
            	delta = 128;
            }
            else if (referenceImage.getType() == ModelStorageBase.ARGB_USHORT) {
            	delta = 32768;
            }
            else {
            	delta = 0.5;
            }
            for (i = 0; i < length; i++) {
            	testYBuffer[i] = 0.299*testRedBuffer[i] + 0.587*testGreenBuffer[i] + 0.114*testBlueBuffer[i];
            	testCrBuffer[i] = (testRedBuffer[i] - testYBuffer[i])*0.713 + delta;
            	testCbBuffer[i] = (testBlueBuffer[i] - testYBuffer[i])*0.564 + delta;
            	referenceYBuffer[i] = 0.299*referenceRedBuffer[i] + 0.587*referenceGreenBuffer[i] + 0.114*referenceBlueBuffer[i];
            	referenceCrBuffer[i] = (referenceRedBuffer[i] - referenceYBuffer[i])*0.713 + delta;
            	referenceCbBuffer[i] = (referenceBlueBuffer[i] - referenceYBuffer[i])*0.564 + delta;
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
		    	break;
		    case UNIVERSAL_QUALITY_IMAGE_INDEX:
		    	break;
		    case MULTI_SCALE_STRUCTURAL_SIMILARITY_INDEX:
		    	break;
		    case ERGAS:
		    	break;
		    case SPATIAL_CORRELATION_COEFFICIENT:
		    	break;
		    case RELATIVE_AVERAGE_SPECTRAL_ERROR:
		    	break;
		    case SPECTRAL_ANGLE_MAPPER:
		    	break;
		    case SPECTRAL_DISTORTION_INDEX:
		    	break;
		    case SPATIAL_DISTORTION_INDEX:
		    	break;
		    case QUALITY_WITH_NO_REFERENCE:
		    	break;
		    case BLOCK_SENSITIVE_PEAK_SIGNAL_TO_NOISE_RATIO:
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
        double maxValue = referenceImage.getMax();
        // maxValue = 255; causes all tests to pass.
        mse();
        if (isColor) {
        	double maxRedValue = referenceImage.getMaxR();
        	double maxGreenValue = referenceImage.getMaxG();
        	double maxBlueValue = referenceImage.getMaxB();
        	double peakRedSignalToNoiseRatio;
        	double peakGreenSignalToNoiseRatio;
        	double peakBlueSignalToNoiseRatio;
        	
        	if (meanRedSquareError == 0) {
            	peakRedSignalToNoiseRatio = Double.POSITIVE_INFINITY;
            }
            else {
            	peakRedSignalToNoiseRatio = 10.0 * Math.log10((maxRedValue*maxRedValue)/meanRedSquareError);
            }
            UI.setDataText("Peak red signal to noise ratio = " + peakRedSignalToNoiseRatio + "\n");
    	    System.out.println("Peak red signal to noise ratio = " + peakRedSignalToNoiseRatio);	
    	    
    	    if (meanGreenSquareError == 0) {
            	peakGreenSignalToNoiseRatio = Double.POSITIVE_INFINITY;
            }
            else {
            	peakGreenSignalToNoiseRatio = 10.0 * Math.log10((maxGreenValue*maxGreenValue)/meanGreenSquareError);
            }
            UI.setDataText("Peak green signal to noise ratio = " + peakGreenSignalToNoiseRatio + "\n");
    	    System.out.println("Peak green signal to noise ratio = " + peakGreenSignalToNoiseRatio);
    	    
    	    if (meanBlueSquareError == 0) {
            	peakBlueSignalToNoiseRatio = Double.POSITIVE_INFINITY;
            }
            else {
            	peakBlueSignalToNoiseRatio = 10.0 * Math.log10((maxBlueValue*maxBlueValue)/meanBlueSquareError);
            }
            UI.setDataText("Peak blue signal to noise ratio = " + peakBlueSignalToNoiseRatio + "\n");
    	    System.out.println("Peak blue signal to noise ratio = " + peakBlueSignalToNoiseRatio);
        } // if (isColor)
        if (meanSquareError == 0) {
        	peakSignalToNoiseRatio = Double.POSITIVE_INFINITY;
        }
        else {
        	peakSignalToNoiseRatio = 10.0 * Math.log10((maxValue*maxValue)/meanSquareError);
        }
        UI.setDataText("Peak signal to noise ratio = " + peakSignalToNoiseRatio + "\n");
	    System.out.println("Peak signal to noise ratio = " + peakSignalToNoiseRatio);
    }
        

    private void uqi() {
    	// calculates universal image quality index (uqi).

    	// param GT: first (original) input image.
    	// param P: second (deformed) input image.
    	// param ws: sliding window size (default = 8).

    	// returns:  float -- uqi value.
    		
    }
    
    private void _uqi_single(double mean1D[], double reference1D[], double test1D[]) {
    	/*N = ws**2
    	window = np.ones((ws,ws))

    	GT_sq = GT*GT
    	P_sq = P*P
    	GT_P = GT*P

    	GT_sum = uniform_filter(GT, ws)    
    	P_sum =  uniform_filter(P, ws)     
    	GT_sq_sum = uniform_filter(GT_sq, ws)  
    	P_sq_sum = uniform_filter(P_sq, ws)  
    	GT_P_sum = uniform_filter(GT_P, ws)

    	GT_P_sum_mul = GT_sum*P_sum
    	GT_P_sum_sq_sum_mul = GT_sum*GT_sum + P_sum*P_sum
    	numerator = 4*(N*GT_P_sum - GT_P_sum_mul)*GT_P_sum_mul
    	denominator1 = N*(GT_sq_sum + P_sq_sum) - GT_P_sum_sq_sum_mul
    	denominator = denominator1*GT_P_sum_sq_sum_mul

    	q_map = np.ones(denominator.shape)
    	index = np.logical_and((denominator1 == 0) , (GT_P_sum_sq_sum_mul != 0))
    	q_map[index] = 2*GT_P_sum_mul[index]/GT_P_sum_sq_sum_mul[index]
    	index = (denominator != 0)
    	q_map[index] = numerator[index]/denominator[index]

    	s = int(np.round(ws/2))
    	return np.mean(q_map[s:-s,s:-s])*/
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
        if (!isColor) {
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
        		    total += Math.sqrt(errors[y][x]);	
        		}
        	}
        	rmse_sw_mean = total/length;
        }
        else {
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
        		    total += Math.sqrt(errors[y][x]);	
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
        		    total += Math.sqrt(errors[y][x]);	
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
        		    total += Math.sqrt(errors[y][x]);	
        		}
        	}
        	rmse_sw_mean = total/(3.0*length);
        }
        
    	return;
    }
    
    
	
}