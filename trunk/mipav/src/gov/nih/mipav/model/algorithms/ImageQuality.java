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
- [x] Erreur Relative Globale Adimensionnelle de Synthèse (ERGAS) [[4]](https://hal.archives-ouvertes.fr/hal-00395027/)
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
    private int length = 1;
    private boolean onlyTestImageRequired = false;
    private boolean YCrCbRequired = false;
    private boolean isColor = false;
    
    private double meanSquareError;
    private double rootMeanSquareError;
    
    private ModelImage gry = null;
    private ModelImage gry_noise = null;
    private ModelImage gry_const = null;
    private ModelImage clr = null;
    private ModelImage clr_noise = null;
    private ModelImage clr_const = null;
	
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
		double eps = 1.0E-4;
		metrics = new int[] {MEAN_SQUARED_ERROR};
		results = new double[1];
		ImageQuality iq = new ImageQuality(clr, clr, metrics,results);
		iq.runAlgorithm();
		if (results[0] != 0.0) {
			System.err.println("Mean squared error = " + results[0] + " for clr, clr\n");
			testsFailed++;
		}
		
		iq = new ImageQuality(gry, gry, metrics, results);
		iq.runAlgorithm();
		if (results[0] != 0.0) {
			System.err.println("Mean squared error = " + results[0] + " for gry, gry\n");
			testsFailed++;
		}
		
		iq = new ImageQuality(clr, clr_noise, metrics, results);
		iq.runAlgorithm();
		if ((Math.abs(results[0] - 2391.465875)) > eps) {
			System.err.println("Mean squared error = " + results[0] + " for clr, clr_noise\n");
			testsFailed++;
		}
		
		iq = new ImageQuality(gry, gry_noise, metrics, results);
		iq.runAlgorithm();
		if ((Math.abs(results[0] - 2025.913940)) > eps) {
			System.err.println("Mean squared error = " + results[0] + " for gry, gry_noise\n");
			testsFailed++;
		}
		
		iq = new ImageQuality(clr, clr_const, metrics, results);
		iq.runAlgorithm();
		if ((Math.abs(results[0] - 2302.953958)) > eps) {
			System.err.println("Mean squared error = " + results[0] + " for clr, clr_const\n");
			testsFailed++;
		}
		
		iq = new ImageQuality(gry, gry_const, metrics, results);
		iq.runAlgorithm();
		if ((Math.abs(results[0] - 2016.476768)) > eps) {
			System.err.println("Mean squared error = " + results[0] + " for gry, gry_const\n");
			testsFailed++;
		}
		
		if (testsFailed > 0) {
			System.err.println(testsFailed + " tests failed for mean square error");
		}
		else {
			System.out.println("All tests passed for meanSquareError");
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
	
	public ImageQuality(ModelImage referenceImage, ModelImage testImage, int metrics[], double results[]) {
		if (metrics == null) {
			MipavUtil.displayError("metrics is null in ImageQuality");
			return;
		}
		if (metrics.length == 0) {
			MipavUtil.displayError("metrics.length == 0 in ImageQuality");
			return;
		}
		if (metrics.length > 14) {
			MipavUtil.displayError("metrics.length > 14 in ImageQuality");
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
			if ((metrics[i] < 1) || (metrics[i] > 14)) {
				MipavUtil.displayError("Illegal metrics[" + i+ "] in ImageQuality");
				return;
			}
			if ((metrics[i] == QUALITY_WITH_NO_REFERENCE) && (metrics.length == 1)) {
				onlyTestImageRequired = true;
			}
			if (isColor && (metrics[i] == BLOCK_SENSITIVE_PEAK_SIGNAL_TO_NOISE_RATIO)) {
				YCrCbRequired = true;
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
	    	this.referenceImage = referenceImage;
	    } // if (!onlyTestImageRequired)
	}
    
    public void runAlgorithm() {
    	int i;
    	UI = ViewUserInterface.getReference();
	    int nDims = testImage.getNDims();
	    
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
    	double meanRedSquareError;
    	double totalGreenSquareDiff = 0.0;
    	double meanGreenSquareError;
    	double totalBlueSquareDiff = 0.0;
    	double meanBlueSquareError;
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
    	double rootMeanRedSquareError;
    	double totalGreenSquareDiff = 0.0;
    	double rootMeanGreenSquareError;
    	double totalBlueSquareDiff = 0.0;
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
	
}