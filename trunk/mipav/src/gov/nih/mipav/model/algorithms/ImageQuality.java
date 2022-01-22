package gov.nih.mipav.model.algorithms;

import java.io.IOException;

import gov.nih.mipav.model.structures.ModelImage;
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
	private int metric;
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
	
	public ImageQuality() {
		UI = ViewUserInterface.getReference();
	}
	
	public ImageQuality(ModelImage referenceImage, ModelImage testImage, int metric) {
		if ((metric < 1) || (metric > 14)) {
			MipavUtil.displayError("Illegal metric in ImageQuality");
			return;
		}
		if (testImage == null) {
			MipavUtil.displayError("testImage is null in ImageQuality");
			return;
		}
		this.testImage = testImage;
	    if (metric != QUALITY_WITH_NO_REFERENCE) {
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
	    } // if (metric != QUALITY_WITH_NO_REFERENCE)
	}
	
    public void runAlgorithm() {
    	int i;
    	UI = ViewUserInterface.getReference();
	    int nDims = testImage.getNDims();
	    double testBuffer[] = null;
	    double testRedBuffer[] = null;
	    double testGreenBuffer[] = null;
	    double testBlueBuffer[] = null;
	    double referenceBuffer[] = null;
	    double referenceRedBuffer[] = null;
	    double referenceGreenBuffer[] = null;
	    double referenceBlueBuffer[] = null;
	    int length = 1;
	    for (i = 0; i < nDims; i++) {
	    	length *= testImage.getExtents()[i];
	    }
	    boolean isColor = testImage.isColorImage();
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
	        testRedBuffer = new double[length];
	        try {
	        	testImage.exportRGBData(1, 0, length, testRedBuffer);
	        }
	        catch (IOException e) {
	        	MipavUtil.displayError("IOException on testImage.exportRGBData(1, 0, length, testRedBuffer");
	        	setCompleted(false);
	        	return;
	        }
	        
	        testGreenBuffer = new double[length];
	        try {
	        	testImage.exportRGBData(2, 0, length, testGreenBuffer);
	        }
	        catch (IOException e) {
	        	MipavUtil.displayError("IOException on testImage.exportRGBData(2, 0, length, testGreenBuffer");
	        	setCompleted(false);
	        	return;
	        }
	        
	        testBlueBuffer = new double[length];
	        try {
	        	testImage.exportRGBData(3, 0, length, testBlueBuffer);
	        }
	        catch (IOException e) {
	        	MipavUtil.displayError("IOException on testImage.exportRGBData(3, 0, length, testBlueBuffer");
	        	setCompleted(false);
	        	return;
	        }
	        if (metric == BLOCK_SENSITIVE_PEAK_SIGNAL_TO_NOISE_RATIO) {
	        	
	        } // if (metric == BLOCK_SENSITIVE_PEAK_SIGNAL_TO_NOISE_RATIO)
	    } // else isColor
	    
	    if (metric != QUALITY_WITH_NO_REFERENCE) {
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
		        referenceRedBuffer = new double[length];
		        try {
		        	referenceImage.exportRGBData(1, 0, length, referenceRedBuffer);
		        }
		        catch (IOException e) {
		        	MipavUtil.displayError("IOException on referenceImage.exportRGBData(1, 0, length, referenceRedBuffer");
		        	setCompleted(false);
		        	return;
		        }
		        
		        referenceGreenBuffer = new double[length];
		        try {
		        	referenceImage.exportRGBData(2, 0, length, referenceGreenBuffer);
		        }
		        catch (IOException e) {
		        	MipavUtil.displayError("IOException on referenceImage.exportRGBData(2, 0, length, referenceGreenBuffer");
		        	setCompleted(false);
		        	return;
		        }
		        
		        referenceBlueBuffer = new double[length];
		        try {
		        	referenceImage.exportRGBData(3, 0, length, referenceBlueBuffer);
		        }
		        catch (IOException e) {
		        	MipavUtil.displayError("IOException on referenceImage.exportRGBData(3, 0, length, referenceBlueBuffer");
		        	setCompleted(false);
		        	return;
		        }
		    } // else isColor	
	    	if (metric == BLOCK_SENSITIVE_PEAK_SIGNAL_TO_NOISE_RATIO) {
	    		
	    	} // if (metric == BLOCK_SENSITIVE_PEAK_SIGNAL_TO_NOISE_RATIO)
	    } // metric != QUALITY_WITH_NO_REFERENCE
	    
	    switch(metric) {
	    case MEAN_SQUARED_ERROR:
	    	break;
	    case ROOT_MEAN_SQUARED_ERROR:
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
	    }
	}
	
}