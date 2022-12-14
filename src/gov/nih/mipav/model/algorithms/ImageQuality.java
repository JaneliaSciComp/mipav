package gov.nih.mipav.model.algorithms;

import java.io.IOException;
import java.util.Collections;
import java.util.Vector;

import Jama.EigenvalueDecomposition;
import Jama.Matrix;
import gov.nih.mipav.model.algorithms.filters.FFTUtility;
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
 *ssim_index.m and ssim.m by Zhou Wang ported with his kind permission.
 *The original source code for ssim.index.m is Copyright(c) 2003 Zhou Wang
 *The original source code for ssim.m is Copyright(c) 2009 Zhou Wang
 *msssim.m and ssim_index.new.m by Zhou Wang ported with his kind permission.
 *
 *For VISUAL_INFORMATION_FIDELITY:
 *% -----------COPYRIGHT NOTICE STARTS WITH THIS LINE------------
% Copyright (c) 2005 The University of Texas at Austin
% All rights reserved.
% 
% Permission is hereby granted, without written agreement and without license or royalty fees, to use, copy, 
% modify, and distribute this code (the source files) and its documentation for
% any purpose, provided that the copyright notice in its entirety appear in all copies of this code, and the 
% original source of this code, Laboratory for Image and Video Engineering (LIVE, http://live.ece.utexas.edu)
% at the University of Texas at Austin (UT Austin, 
% http://www.utexas.edu), is acknowledged in any publication that reports research using this code. The research
% is to be cited in the bibliography as:
% 
% H. R. Sheikh and A. C. Bovik, "Image Information and Visual Quality", IEEE Transactions on 
% Image Processing, (to appear).
% 
% IN NO EVENT SHALL THE UNIVERSITY OF TEXAS AT AUSTIN BE LIABLE TO ANY PARTY FOR DIRECT, INDIRECT, SPECIAL, INCIDENTAL, 
% OR CONSEQUENTIAL DAMAGES ARISING OUT OF THE USE OF THIS DATABASE AND ITS DOCUMENTATION, EVEN IF THE UNIVERSITY OF TEXAS
% AT AUSTIN HAS BEEN ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.
% 
% THE UNIVERSITY OF TEXAS AT AUSTIN SPECIFICALLY DISCLAIMS ANY WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE IMPLIED 
% WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE. THE DATABASE PROVIDED HEREUNDER IS ON AN "AS IS" BASIS,
% AND THE UNIVERSITY OF TEXAS AT AUSTIN HAS NO OBLIGATION TO PROVIDE MAINTENANCE, SUPPORT, UPDATES, ENHANCEMENTS, OR MODIFICATIONS.
% 
% -----------COPYRIGHT NOTICE ENDS WITH THIS LINE------------
%
%This is an implementation of the algorithm for calculating the
%Visual Information Fidelity (VIF) measure (may also be known as the Sheikh
%-Bovik Index) between two images. Please refer
%to the following paper:
%
%H. R. Sheikh and A. C. Bovik "Image Information and Visual Quality"
%IEEE Transactios on Image Processing, in publication, May 2005.
%Download manuscript draft from http://live.ece.utexas.edu in the
%Publications link
%
%This implementation is slightly differnet from the one used to report
%results in the paper above. The modification have to do with using more
%subands than those used in the paper, better handling of image boundaries,
%and a window that automatically resizes itself based on the scale.
%
%Report bugfixes and comments to hamid.sheikh@ieee.org
%
%----------------------------------------------------------------------
% Prerequisites: The Steerable Pyramid toolbox. Available at
% http://www.cns.nyu.edu/~lcv/software.html
%
%Input : (1) img1: The reference image
%        (2) img2: The distorted image (order is important)
%
%Output: (1) VIF te visual information fidelity measure between the two images

%Default Usage:
%   Given 2 test images img1 and img2, whose dynamic range is 0-255
%
%   vif = vifvec(img1, img2);
%
%Advanced Usage:
%   Users may want to modify the parameters in the code. 
%   (1) Modify sigma_nsq to find tune for your image dataset.
%   (2) MxM is the block size that denotes the size of a vector used in the
%   GSM model.
%   (3) subbands included in the computation
 *
 *
 *
 *NQM is ported with the kind permission of Professor Brian L. Evans:
 *Copyright (c) 1999-2021 The University of Texas at Austin
 All rights reserved.

 Permission is hereby granted, without written agreement and without license or royalty fees, to use, copy, modify,
 and distribute this software and its documentation for any purpose, provided that the copyright notice in its
 entirety appear in all copies of this software.
 IN NO EVENT SHALL THE UNIVERSITY OF TEXAS AT AUSTIN BE LIABLE TO ANY PARTY FOR DIRECT, INDIRECT, SPECIAL, INCIDENTAL,
 OR CONSEQUENTIAL DAMAGES ARISING OUT OF THE USE OF THIS SOFTWARE AND ITS DOCUMENTATION, EVEN IF THE UNIVERSITY OF
 TEXAS AT AUSTIN HAS BEEN ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.

 THE UNIVERSITY OF TEXAS AT AUSTIN SPECIFICALLY DISCLAIMS ANY WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE IMPLIED
 WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE. THE DATABASE PROVIDED HEREUNDER IS ON AN "AS IS"
 BASIS, AND THE UNIVERSITY OF TEXAS AT AUSTIN HAS NO OBLIGATION TO PROVIDE MAINTENANCE, SUPPORT, UPDATES, ENHANCEMENTS,
 OR MODIFICATIONS.
 
 VSR is ported with the kind permissions of Dr. Sheila S. Hemami and Professor Damon Chandler at Ritsumeikan University
 in Japan.  They are the co_inventors of the method.
 Author: D. M. Chander
 Copyright 1998-2007 Image Coding and Analysis Lab, Oklahoma State 
 University, Stillwater, OK 74078 USA.
 $Revision: BETA 1.02.0.0 $  $Date: 2007/10/04/ 15:00:00 $ 
 
 For DWT code used for VSNR:
 ////////////////////////////////////////////////////////////////////////////
//                                                                        //
// COPYRIGHT (c) 1998, 2002, VCL                                          //
// ------------------------------                                         //
// Permission to use, copy, modify, distribute and sell this software     //
// and its documentation for any purpose is hereby granted without fee,   //
// provided that the above copyright notice appear in all copies and      //
// that both that copyright notice and this permission notice appear      //
// in supporting documentation.  VCL makes no representations about       //
// the suitability of this software for any purpose.                      //
//                                                                        //
// DISCLAIMER:                                                            //
// -----------                                                            //
// The code provided hereunder is provided as is without warranty         //
// of any kind, either express or implied, including but not limited      //
// to the implied warranties of merchantability and fitness for a         //
// particular purpose.  The author(s) shall in no event be liable for     //
// any damages whatsoever including direct, indirect, incidental,         //
// consequential, loss of business profits or special damages.            //
//                                                                        //


 **Other metrics are from full_ref.py, no_ref.py and utils.py by Andrew Khalel.
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
	public final int MULTI_SCALE_STRUCTURAL_SIMILARITY_INDEX = 7;
	public final int ERGAS = 8;
	public final int SPATIAL_CORRELATION_COEFFICIENT = 9;
	public final int RELATIVE_AVERAGE_SPECTRAL_ERROR = 10;
	public final int SPECTRAL_ANGLE_MAPPER = 11;
	//public final int SPECTRAL_DISTORTION_INDEX = 10;
	//public final int SPATIAL_DISTORTION_INDEX = 11;
	//public final int QUALITY_WITH_NO_REFERENCE = 12;
	public final int VISUAL_INFORMATION_FIDELITY = 12;
	public final int PIXEL_BASED_VISUAL_INFORMATION_FIDELITY = 13;
 	public final int BLOCK_SENSITIVE_PEAK_SIGNAL_TO_NOISE_RATIO = 14;
 	public final int RMSE_SW = 15;
 	public final int NQM = 16;
 	public final int VSNR = 17;
 	
 	// method choices for MSSSIM
 	private static final int PRODUCT = 1;
 	private final static int WTD_SUM = 2;
 	
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
    private double overall_mssim;
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
    private double vif;
    private double nqm_value;
    private double vsnr_value;
    
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
    // Default = 11 for MULTI_SCALE_STRUCTURAL_SIMILARITY_INDEX
    // Default = 8 for RELATIVE_AVERAGE_SPECTRAL_ERROR
    // Default = 8 for ERGAS
    // Default = 8 for SPATIAL_CORRELATION_COEFFICIENT
    private int ws;
    // First STRUCTURAL_SIMILARITY_INDEX, SSIM_WITH_AUTOMATIC_DOWNSAMPLING, and MULTI_SCALE_STRUCTURAL_SIMILARITY_INDEX constant (default = 0.01)
    private double k1 = 0.01;
    // Second STRUCTURAL_SIMILARITY_INDEX, SSIM_WITH_AUTOMATIC_DOWNSAMPLING, and MULTI_SCALE_STRUCTURAL_SIMILARITY_INDEX constant (default = 0.03)
    private double k2 = 0.03;
    // sigma used in STRUCTURAL_SIMILARITY_INDEX, SSIM_WITH_AUTOMATIC_DOWNSAMPLING, and MULTI_SCALE_STRUCTURAL_SIMILARITY_INDEX
    private double sigma = 1.5;
    // Used in MSSSIM
    private int level = 5;
    private double weight[] = new double[] {0.0448, 0.2856, 0.3001, 0.2363, 0.1333};
    private int method = PRODUCT;
    // Ratio of high resolution to low resolution (default = 4.0) used in ERGAS
    private double r = 4.0;
    // high pass filter for spatial processing (default=[[-1,-1,-1],[-1,8,-1],[-1,-1,-1]]).
    // Used in SPATIAL_CORRELATION_COEFFICIENT
    private double win[][] = new double[][] {{-1,-1,-1},{-1,8,-1},{-1,-1,-1}};
    // variance of the visual noise used in PIXEL_BASED_VISUAL_INFORMATION_FIDELITY and VISUAL_INFORMATION_FIDELITY
    // Default = 2 for PIXEL_BASED_VISUAL_INFORMATION_FIDELITY
    // Default = 0.4 for VISUAL_INFORMATION_FIDELITY
    private double sigma_nsq;
    // subbands included in the computation for VISUAL_INFORMATION_FIDELITY
    // subbands decreased 1 from MATLAB values to go from 1 to 0 based indexing
    private int subbands[] = new int[] {3, 6, 9, 12, 15, 18, 21, 24};
    // MxM is the block size that denotes the size of a vector used in the GSM model in VISUAL_INFORMATION_FIDELITY.
    private int M = 3;
    // VA is the viewing angle in NQM.  Default 4.0 degrees.
    private double VA = 4.0;
    
    // Used in VSNR
    // specification of the linear combination weight ALPHA (in the range
    //  [0, 1]) which specifies the relative contributions of perceived
    //  distortion contrast and disruption of global precedence toward the
    //  total visual distortion.  By default, ALPHA = 0.04 (which uses 4% 
    //  contribution from perceived distortion contrast, and 96%
    //  contribution from disruption of global precedence).
    private double alpha = 0.04;
    //  additional specification of the viewing conditions and DWT options.  
    //  The structure vsnr_data contains the following data members 
    //  which must be set prior to passing the structure to the VSNR 
    //  function:
    // display device black-level offset 
    // (default = 0, for 8-bit sRGB)
    private int vsnr_data_b = 0;
    // display device pixel-value-to-voltage gain 
    // (default = 0.02874, for 8-bit sRGB)
    private double vsnr_data_k = 0.02874;
    // display device pixel-value-to-voltage gain 
    // (default = 2.2, for 8-bit sRGB)
    private double vsnr_data_g = 2.2;
    // display device resolution (pixels/inch) 
    //(default = 96)
    private double vsnr_data_r = 96.0; 
    // viewing distance (inches) 
    // (default = 19.1)
    private double vsnr_data_v = 19.1; 
    // number of DWT levels 
    // (default = 5)
    private int vsnr_data_num_levels = 5; 
    // DWT filter gains (may be all ones)
    // (default = 2.^[1:num_levels])
    private int vsnr_data_filter_gains[] = null; 
    // spatial frequencies for DWT levels
    // not an input parameter
    private double vsnr_data_fs[] = null; 
    // the pixel-value-to-luminance lookup table
    // not an input parameter
    private double vsnr_data_pix2lum_table[] = null;
    // the total distortion contrast threshold
    // not an input parameter
    private double vsnr_data_CTe;
    // mean of reference buffer
    // not an input parameter
    private double vsnr_data_mX;
    // not an input parameter
    private double vsnr_data_mL;
    // zeta value used to estimate image contrast at each scale
    // not an input parameter
    private double vsnr_data_zeta;
    // not an input parameter
    private double vsnr_data_Cis[] = null;
    // not an input parameter
    private double vsnr_data_Ci; 
    // not an input parameter
    private double vsnr_data_Ce;

    
    public final int BORDER_CONSTANT = 0; // iiiiii|abcdefgh|iiiiiii with some specified i
	public final int BORDER_REPLICATE = 1; // aaaaaa|abcdefgh|hhhhhhh
	public final int BORDER_REFLECT = 2; // fedcba|abcdefgh|hgfedcb
	public final int BORDER_WRAP = 3; // cdefgh|abcdefgh|abcdefg
	public final int BORDER_REFLECT_101 = 4; // gfedcb|abcdefgh|gfedcba
	public final int BORDER_DEFAULT = BORDER_REFLECT_101;
	
	// For DWT:
	static final double ALPHA =   -1.586134342;
	static final double BETA =    -0.052980118;
	static final double GAMMA =    0.882911075;
	static final double DELTA =    0.443506852;
	static final double TWOALPHA = 2 * ALPHA;
	static final double TWOBETA =  2 * BETA;
	static final double TWOGAMMA = 2 * GAMMA;
	static final double TWODELTA = 2 * DELTA;
	boolean GWAVELIFT_NORM_1_1 = true;


	
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
		ImageQuality iq = new ImageQuality(clr, clr, metrics,ws, k1, k2,sigma,r,win,sigma_nsq,subbands,M,level,weight,method,VA,
				alpha, vsnr_data_b, vsnr_data_k, vsnr_data_g, vsnr_data_r, vsnr_data_v,
				vsnr_data_num_levels, vsnr_data_filter_gains, 
				results);
		iq.runAlgorithm();
		if (results[0] != 0.0) {
			System.err.println("Mean squared error = " + results[0] + " for clr, clr\n");
			testsFailed++;
		}
		
		iq = new ImageQuality(gry, gry, metrics, ws, k1,k2, sigma,r,win,sigma_nsq,subbands,M,level,weight,method,VA,
				alpha, vsnr_data_b, vsnr_data_k, vsnr_data_g, vsnr_data_r, vsnr_data_v,
				vsnr_data_num_levels, vsnr_data_filter_gains, 
				results);
		iq.runAlgorithm();
		if (results[0] != 0.0) {
			System.err.println("Mean squared error = " + results[0] + " for gry, gry\n");
			testsFailed++;
		}
		
		iq = new ImageQuality(clr, clr_noise, metrics, ws, k1,k2,sigma,r,win,sigma_nsq,subbands,M,level,weight,method,VA,
				alpha, vsnr_data_b, vsnr_data_k, vsnr_data_g, vsnr_data_r, vsnr_data_v,
				vsnr_data_num_levels, vsnr_data_filter_gains, 
				results);
		iq.runAlgorithm();
		if ((Math.abs(results[0] - 2391.465875)) > eps) {
			System.err.println("Mean squared error = " + results[0] + " for clr, clr_noise\n");
			testsFailed++;
		}
		
		iq = new ImageQuality(gry, gry_noise, metrics, ws, k1,k2,sigma,r,win,sigma_nsq,subbands,M,level,weight,method,VA,
				alpha, vsnr_data_b, vsnr_data_k, vsnr_data_g, vsnr_data_r, vsnr_data_v,
				vsnr_data_num_levels, vsnr_data_filter_gains, 
				results);
		iq.runAlgorithm();
		if ((Math.abs(results[0] - 2025.913940)) > eps) {
			System.err.println("Mean squared error = " + results[0] + " for gry, gry_noise\n");
			testsFailed++;
		}
		
		iq = new ImageQuality(clr, clr_const, metrics, ws,k1,k2,sigma,r,win,sigma_nsq,subbands,M,level,weight,method,VA,
				alpha, vsnr_data_b, vsnr_data_k, vsnr_data_g, vsnr_data_r, vsnr_data_v,
				vsnr_data_num_levels, vsnr_data_filter_gains, 
				results);
		iq.runAlgorithm();
		if ((Math.abs(results[0] - 2302.953958)) > eps) {
			System.err.println("Mean squared error = " + results[0] + " for clr, clr_const\n");
			testsFailed++;
		}
		
		iq = new ImageQuality(gry, gry_const, metrics, ws,k1,k2,sigma,r,win,sigma_nsq,subbands,M,level,weight,method,VA,
				alpha, vsnr_data_b, vsnr_data_k, vsnr_data_g, vsnr_data_r, vsnr_data_v,
				vsnr_data_num_levels, vsnr_data_filter_gains, 
				results);
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
		ImageQuality iq = new ImageQuality(clr, clr, metrics,ws,k1,k2,sigma,r,win,sigma_nsq,subbands,M,level,weight,method,VA,
				alpha, vsnr_data_b, vsnr_data_k, vsnr_data_g, vsnr_data_r, vsnr_data_v,
				vsnr_data_num_levels, vsnr_data_filter_gains, 
				results);
		iq.runAlgorithm();
		if (results[0] != 0.0) {
			System.err.println("Root mean squared error = " + results[0] + " for clr, clr\n");
			testsFailed++;
		}
		
		iq = new ImageQuality(gry, gry, metrics, ws, k1,k2,sigma,r,win,sigma_nsq,subbands,M,level,weight,method,VA,
				alpha, vsnr_data_b, vsnr_data_k, vsnr_data_g, vsnr_data_r, vsnr_data_v,
				vsnr_data_num_levels, vsnr_data_filter_gains, 
				results);
		iq.runAlgorithm();
		if (results[0] != 0.0) {
			System.err.println("Root mean squared error = " + results[0] + " for gry, gry\n");
			testsFailed++;
		}
		
		metrics = new int[] {RMSE_SW};
		ws = 8;
		iq = new ImageQuality(clr, clr, metrics,ws,k1,k2,sigma,r,win,sigma_nsq,subbands,M,level,weight,method,VA,
				alpha, vsnr_data_b, vsnr_data_k, vsnr_data_g, vsnr_data_r, vsnr_data_v,
				vsnr_data_num_levels, vsnr_data_filter_gains, 
				results);
		iq.runAlgorithm();
		if (results[0] != 0.0) {
			System.err.println("Root mean squared error sliding window = " + results[0] + " for clr, clr\n");
			testsFailed++;
		}
		
		iq = new ImageQuality(gry, gry, metrics, ws, k1,k2,sigma,r,win,sigma_nsq,subbands,M,level,weight,method,VA,
				alpha, vsnr_data_b, vsnr_data_k, vsnr_data_g, vsnr_data_r, vsnr_data_v,
				vsnr_data_num_levels, vsnr_data_filter_gains, 
				results);
		iq.runAlgorithm();
		if (results[0] != 0.0) {
			System.err.println("Root mean squared error sliding window = " + results[0] + " for gry, gry\n");
			testsFailed++;
		}
		
		metrics = new int[] {ROOT_MEAN_SQUARED_ERROR};
		iq = new ImageQuality(gry, gry_const, metrics, ws,k1,k2,sigma,r,win,sigma_nsq,subbands,M,level,weight,method,VA,
				alpha, vsnr_data_b, vsnr_data_k, vsnr_data_g, vsnr_data_r, vsnr_data_v,
				vsnr_data_num_levels, vsnr_data_filter_gains, 
				results);
		iq.runAlgorithm();
		double rmse = results[0];
		metrics = new int[] {RMSE_SW};
		ws = 510;
		iq = new ImageQuality(gry, gry_const, metrics, ws,k1,k2,sigma,r,win,sigma_nsq,subbands,M,level,weight,method,VA,
				alpha, vsnr_data_b, vsnr_data_k, vsnr_data_g, vsnr_data_r, vsnr_data_v,
				vsnr_data_num_levels, vsnr_data_filter_gains, results);
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
		ImageQuality iq = new ImageQuality(clr, clr, metrics,ws,k1,k2,sigma,r,win,sigma_nsq,subbands,M,level,weight,method,VA,
				alpha, vsnr_data_b, vsnr_data_k, vsnr_data_g, vsnr_data_r, vsnr_data_v,
				vsnr_data_num_levels, vsnr_data_filter_gains, 
				results);
		iq.runAlgorithm();
		if (!Double.isInfinite(results[0])) {
			System.err.println("Peak signal to noise ratio = " + results[0] + " for clr, clr\n");
			testsFailed++;
		}
		
		iq = new ImageQuality(gry, gry, metrics, ws, k1,k2,sigma,r,win,sigma_nsq,subbands,M,level,weight,method,VA,
				alpha, vsnr_data_b, vsnr_data_k, vsnr_data_g, vsnr_data_r, vsnr_data_v,
				vsnr_data_num_levels, vsnr_data_filter_gains, 
				results);
		iq.runAlgorithm();
		if (!Double.isInfinite(results[0])) {
			System.err.println("Peak signal to noise ratio = " + results[0] + " for gry, gry\n");
			testsFailed++;
		}
		
		iq = new ImageQuality(clr, clr_noise, metrics, ws, k1,k2,sigma,r,win,sigma_nsq,subbands,M,level,weight,method,VA,
				alpha, vsnr_data_b, vsnr_data_k, vsnr_data_g, vsnr_data_r, vsnr_data_v,
				vsnr_data_num_levels, vsnr_data_filter_gains, 
				results);
		iq.runAlgorithm();
		if ((Math.abs(results[0] - 14.344162)) > eps) {
			System.err.println("Peak signal to noise ratio = " + results[0] + " for clr, clr_noise\n");
			testsFailed++;
		}
		
		iq = new ImageQuality(gry, gry_noise, metrics, ws, k1,k2,sigma,r,win,sigma_nsq,subbands,M,level,weight,method,VA,
				alpha, vsnr_data_b, vsnr_data_k, vsnr_data_g, vsnr_data_r, vsnr_data_v,
				vsnr_data_num_levels, vsnr_data_filter_gains, 
				results);
		iq.runAlgorithm();
		if ((Math.abs(results[0] - 15.064594)) > eps) {
			System.err.println("Peak signal to noise ratio = " + results[0] + " for gry, gry_noise\n");
			testsFailed++;
		}
		
		iq = new ImageQuality(clr, clr_const, metrics, ws,k1,k2,sigma,r,win,sigma_nsq,subbands,M,level,weight,method,VA,
				alpha, vsnr_data_b, vsnr_data_k, vsnr_data_g, vsnr_data_r, vsnr_data_v,
				vsnr_data_num_levels, vsnr_data_filter_gains, 
				results);
		iq.runAlgorithm();
		if ((Math.abs(results[0] - 14.507951)) > eps) {
			System.err.println("Peak signal to noise ratio = " + results[0] + " for clr, clr_const\n");
			testsFailed++;
		}
		
		iq = new ImageQuality(gry, gry_const, metrics, ws,k1,k2,sigma,r,win,sigma_nsq,subbands,M,level,weight,method,VA,
				alpha, vsnr_data_b, vsnr_data_k, vsnr_data_g, vsnr_data_r, vsnr_data_v,
				vsnr_data_num_levels, vsnr_data_filter_gains, 
				results);
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
		ImageQuality iq = new ImageQuality(clr, clr, metrics,ws,k1,k2,sigma,r,win,sigma_nsq,subbands,M,level,weight,method,VA,
				alpha, vsnr_data_b, vsnr_data_k, vsnr_data_g, vsnr_data_r, vsnr_data_v,
				vsnr_data_num_levels, vsnr_data_filter_gains, 
				results);
		iq.runAlgorithm();
		if (results[0] != 0) {
			System.err.println("ergas = " + results[0] + " for clr, clr\n");
			testsFailed++;
		}
		
		iq = new ImageQuality(gry, gry, metrics, ws, k1,k2,sigma,r,win,sigma_nsq,subbands,M,level,weight,method,VA,
				alpha, vsnr_data_b, vsnr_data_k, vsnr_data_g, vsnr_data_r, vsnr_data_v,
				vsnr_data_num_levels, vsnr_data_filter_gains, 
				results);
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
		ImageQuality iq = new ImageQuality(clr, clr, metrics,ws,k1,k2,sigma,r,win,sigma_nsq,subbands,M,level,weight,method,VA,
				alpha, vsnr_data_b, vsnr_data_k, vsnr_data_g, vsnr_data_r, vsnr_data_v,
				vsnr_data_num_levels, vsnr_data_filter_gains, 
				results);
		iq.runAlgorithm();
		if (results[0] != 1.0) {
			System.err.println("Spatial correlation coefficient = " + results[0] + " for clr, clr\n");
			testsFailed++;
		}
		
		iq = new ImageQuality(gry, gry, metrics, ws, k1,k2,sigma,r,win,sigma_nsq,subbands,M,level,weight,method,VA,
				alpha, vsnr_data_b, vsnr_data_k, vsnr_data_g, vsnr_data_r, vsnr_data_v,
				vsnr_data_num_levels, vsnr_data_filter_gains, 
				results);
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
		ImageQuality iq = new ImageQuality(clr, clr, metrics,ws,k1,k2,sigma,r,win,sigma_nsq,subbands,M,level,weight,method,VA,
				alpha, vsnr_data_b, vsnr_data_k, vsnr_data_g, vsnr_data_r, vsnr_data_v,
				vsnr_data_num_levels, vsnr_data_filter_gains, 
				results);
		iq.runAlgorithm();
		if (results[0] != 0) {
			System.err.println("Relative average spectral error = " + results[0] + " for clr, clr\n");
			testsFailed++;
		}
		
		iq = new ImageQuality(gry, gry, metrics, ws, k1,k2,sigma,r,win,sigma_nsq,subbands,M,level,weight,method,VA,
				alpha, vsnr_data_b, vsnr_data_k, vsnr_data_g, vsnr_data_r, vsnr_data_v,
				vsnr_data_num_levels, vsnr_data_filter_gains, 
				results);
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
		ImageQuality iq = new ImageQuality(clr, clr, metrics,ws,k1,k2,sigma,r,win,sigma_nsq,subbands,M,level,weight,method,VA,
				alpha, vsnr_data_b, vsnr_data_k, vsnr_data_g, vsnr_data_r, vsnr_data_v,
				vsnr_data_num_levels, vsnr_data_filter_gains, 
				results);
		iq.runAlgorithm();
		if (results[0] != 1.0) {
			System.err.println("Universal quality image index = " + results[0] + " for clr, clr\n");
			testsFailed++;
		}
		
		iq = new ImageQuality(gry, gry, metrics, ws, k1,k2,sigma,r,win,sigma_nsq,subbands,M,level,weight,method,VA,
				alpha, vsnr_data_b, vsnr_data_k, vsnr_data_g, vsnr_data_r, vsnr_data_v,
				vsnr_data_num_levels, vsnr_data_filter_gains, 
				results);
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
		iq = new ImageQuality(gryA, gryB, metrics, ws, k1,k2,sigma,r,win,sigma_nsq,subbands,M,level,weight,method,VA,
				alpha, vsnr_data_b, vsnr_data_k, vsnr_data_g, vsnr_data_r, vsnr_data_v,
				vsnr_data_num_levels, vsnr_data_filter_gains, 
				results);
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
		iq = new ImageQuality(gryA, gryC, metrics, ws, k1,k2,sigma,r,win,sigma_nsq,subbands,M,level,weight,method,VA,
				alpha, vsnr_data_b, vsnr_data_k, vsnr_data_g, vsnr_data_r, vsnr_data_v,
				vsnr_data_num_levels, vsnr_data_filter_gains, 
				results);
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
		iq = new ImageQuality(gryA, gryD, metrics, ws, k1,k2,sigma,r,win,sigma_nsq,subbands,M,level,weight,method,VA,
				alpha, vsnr_data_b, vsnr_data_k, vsnr_data_g, vsnr_data_r, vsnr_data_v,
				vsnr_data_num_levels, vsnr_data_filter_gains, 
				results);
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
		iq = new ImageQuality(gryA, gryE, metrics, ws, k1,k2,sigma,r,win,sigma_nsq,subbands,M,level,weight,method,VA,
				alpha, vsnr_data_b, vsnr_data_k, vsnr_data_g, vsnr_data_r, vsnr_data_v,
				vsnr_data_num_levels, vsnr_data_filter_gains, 
				results);
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
		iq = new ImageQuality(gryA, gryF, metrics, ws, k1,k2,sigma,r,win,sigma_nsq,subbands,M,level,weight,method,VA,
				alpha, vsnr_data_b, vsnr_data_k, vsnr_data_g, vsnr_data_r, vsnr_data_v,
				vsnr_data_num_levels, vsnr_data_filter_gains, 
				results);
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
		iq = new ImageQuality(gryA, gryG, metrics, ws, k1,k2,sigma,r,win,sigma_nsq,subbands,M,level,weight,method,VA,
				alpha, vsnr_data_b, vsnr_data_k, vsnr_data_g, vsnr_data_r, vsnr_data_v,
				vsnr_data_num_levels, vsnr_data_filter_gains, 
				results);
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
		iq = new ImageQuality(gryA, gryH, metrics, ws, k1,k2,sigma,r,win,sigma_nsq,subbands,M,level,weight,method,VA,
				alpha, vsnr_data_b, vsnr_data_k, vsnr_data_g, vsnr_data_r, vsnr_data_v,
				vsnr_data_num_levels, vsnr_data_filter_gains, 
				results);
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
		ImageQuality iq = new ImageQuality(clr, clr, metrics,ws,k1,k2,sigma,r,win,sigma_nsq,subbands,M,level,weight,method,VA,
				alpha, vsnr_data_b, vsnr_data_k, vsnr_data_g, vsnr_data_r, vsnr_data_v,
				vsnr_data_num_levels, vsnr_data_filter_gains, 
				results);
		iq.runAlgorithm();
		if (results[0] != 1.0) {
			System.err.println("Structural similarity index = " + results[0] + " for clr, clr\n");
			testsFailed++;
		}
		
		iq = new ImageQuality(gry, gry, metrics, ws, k1,k2,sigma,r,win,sigma_nsq,subbands,M,level,weight,method,VA,
				alpha, vsnr_data_b, vsnr_data_k, vsnr_data_g, vsnr_data_r, vsnr_data_v,
				vsnr_data_num_levels, vsnr_data_filter_gains, 
				results);
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
		ImageQuality iq = new ImageQuality(clr, clr, metrics,ws,k1,k2,sigma,r,win,sigma_nsq,subbands,M,level,weight,method,VA,
				alpha, vsnr_data_b, vsnr_data_k, vsnr_data_g, vsnr_data_r, vsnr_data_v,
				vsnr_data_num_levels, vsnr_data_filter_gains, 
				results);
		iq.runAlgorithm();
		if (results[0] != 1.0) {
			System.err.println("Structural similarity index with automatic downloading = " + results[0] + " for clr, clr\n");
			testsFailed++;
		}
		
		iq = new ImageQuality(gry, gry, metrics, ws, k1,k2,sigma,r,win,sigma_nsq,subbands,M,level,weight,method,VA,
				alpha, vsnr_data_b, vsnr_data_k, vsnr_data_g, vsnr_data_r, vsnr_data_v,
				vsnr_data_num_levels, vsnr_data_filter_gains, 
				results);
		iq.runAlgorithm();
		if (results[0] != 1.0) {
			System.err.println("Structural similarity index with automatic downloading = " + results[0] + " for gry, gry\n");
			testsFailed++;
		}
		
		String fileDir = "C:/Image Quality/sewar-master/sewar/tests/res/";
		final FileIO fileIO = new FileIO();
		fileIO.setQuiet(true);
    	fileIO.setSuppressProgressBar(true);
    	// Einstein picture examples from https://ece.uwaterloo.ca/~z70wang/research/ssim/.
    	// Original image, MSE = 0, SSIM = 1
    	ModelImage gryA = fileIO.readJimi("EinsteinA.jpg", fileDir, false);
    	// MSE = 144, SSIM = 0.988
    	ModelImage gryB = fileIO.readJimi("EinsteinB.jpg", fileDir, false);
    	iq = new ImageQuality(gryA, gryB, metrics, ws, k1,k2,sigma,r,win,sigma_nsq,subbands,M,level,weight,method,VA,
    			alpha, vsnr_data_b, vsnr_data_k, vsnr_data_g, vsnr_data_r, vsnr_data_v,
				vsnr_data_num_levels, vsnr_data_filter_gains, 
    			results);
    	iq.runAlgorithm();
    	
    	// MSE = 144, SSIM = 0.913
    	ModelImage gryC = fileIO.readJimi("EinsteinC.jpg", fileDir, false);
    	iq = new ImageQuality(gryA, gryC, metrics, ws, k1,k2,sigma,r,win,sigma_nsq,subbands,M,level,weight,method,VA,
    			alpha, vsnr_data_b, vsnr_data_k, vsnr_data_g, vsnr_data_r, vsnr_data_v,
				vsnr_data_num_levels, vsnr_data_filter_gains, 
    			results);
    	iq.runAlgorithm();

		
		if (testsFailed > 0) {
			System.err.println(testsFailed + " tests failed for structural similarity index with automatic downloading");
		}
		else {
			System.out.println("All tests passed for structural similarity index with automatic downloading");
		}
		
		gryA.disposeLocal();
		gryA = null;
		gryB.disposeLocal();
		gryB = null;
		gryC.disposeLocal();
		gryC = null;
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
	
	public void testMsssim() {
		// All tests passed for multi scale structural similarity index
		ws = 11;
		int testsFailed = 0;
		double eps = 1.0E-3;
		metrics = new int[] {MULTI_SCALE_STRUCTURAL_SIMILARITY_INDEX};
		results = new double[1];
		ImageQuality iq = new ImageQuality(clr, clr, metrics,ws,k1,k2,sigma,r,win,sigma_nsq,subbands,M,level,weight,method,VA,
				alpha, vsnr_data_b, vsnr_data_k, vsnr_data_g, vsnr_data_r, vsnr_data_v,
				vsnr_data_num_levels, vsnr_data_filter_gains, 
				results);
		iq.runAlgorithm();
		if (results[0] != 1.0) {
			System.err.println("Multi scale structural similarity index = " + results[0] + " for clr, clr\n");
			testsFailed++;
		}
		
		iq = new ImageQuality(gry, gry, metrics, ws, k1,k2,sigma,r,win,sigma_nsq,subbands,M,level,weight,method,VA,
				alpha, vsnr_data_b, vsnr_data_k, vsnr_data_g, vsnr_data_r, vsnr_data_v,
				vsnr_data_num_levels, vsnr_data_filter_gains, 
				results);
		iq.runAlgorithm();
		if (results[0] != 1.0) {
			System.err.println("Multi scale structural similarity index = " + results[0] + " for gry, gry\n");
			testsFailed++;
		}
		
		iq = new ImageQuality(gry, gry_noise, metrics, ws, k1,k2,sigma,r,win,sigma_nsq,subbands,M,level,weight,method,VA,
				alpha, vsnr_data_b, vsnr_data_k, vsnr_data_g, vsnr_data_r, vsnr_data_v,
				vsnr_data_num_levels, vsnr_data_filter_gains, 
				results);
		iq.runAlgorithm();
		if (Math.abs(results[0] - 0.631429952770791) > eps) {
			System.err.println("Multi scale structural similarity index = " + results[0] + " for gry, gry_noise\n");
			testsFailed++;
		}
		
		if (testsFailed > 0) {
			System.err.println(testsFailed + " tests failed for multi scale structural similarity index");
		}
		else {
			System.out.println("All tests passed for multi scale structural similarity index");
		}
		
		gry.disposeLocal();
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
		ImageQuality iq = new ImageQuality(clr, clr, metrics,ws,k1,k2,sigma,r,win,sigma_nsq,subbands,M,level,weight,method,VA,
				alpha, vsnr_data_b, vsnr_data_k, vsnr_data_g, vsnr_data_r, vsnr_data_v,
				vsnr_data_num_levels, vsnr_data_filter_gains, 
				results);
		iq.runAlgorithm();
		if (Math.abs(results[0]) >= eps) {
			System.err.println("Spectral angle mapper = " + results[0] + " for clr, clr\n");
			testsFailed++;
		}
		
		iq = new ImageQuality(gry, gry, metrics, ws, k1,k2,sigma,r,win,sigma_nsq,subbands,M,level,weight,method,VA,
				alpha, vsnr_data_b, vsnr_data_k, vsnr_data_g, vsnr_data_r, vsnr_data_v,
				vsnr_data_num_levels, vsnr_data_filter_gains, 
				results);
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
	
	public void testNQM() {
		int testsFailed = 0;
		double eps = 1.0E-3;
		double VA = 4.0;
		metrics = new int[] {NQM};
		results = new double[1];
		
		ImageQuality iq = new ImageQuality(gry, gry, metrics, ws, k1,k2,sigma,r,win,sigma_nsq,subbands,M,level,weight,method,VA,
				alpha, vsnr_data_b, vsnr_data_k, vsnr_data_g, vsnr_data_r, vsnr_data_v,
				vsnr_data_num_levels, vsnr_data_filter_gains, 
				results);
		iq.runAlgorithm();
		
		iq = new ImageQuality(gry, gry_noise, metrics, ws, k1,k2,sigma,r,win,sigma_nsq,subbands,M,level,weight,method,VA,
				alpha, vsnr_data_b, vsnr_data_k, vsnr_data_g, vsnr_data_r, vsnr_data_v,
				vsnr_data_num_levels, vsnr_data_filter_gains, 
				results);
		iq.runAlgorithm();
		
		String referenceFileDir = "C:/Image Quality/tid2008/reference_images/";
		String distortedFileDir = "C:/Image Quality/tid2008/distorted_images/";
		final FileIO fileIO = new FileIO();
		fileIO.setQuiet(true);
    	fileIO.setSuppressProgressBar(true);
    	ModelImage colorRef = fileIO.readJimi("I01.BMP", referenceFileDir, false);
    	ModelImage colorA = fileIO.readJimi("I01_01_1.BMP", distortedFileDir, false);
    	iq = new ImageQuality(colorRef, colorA, metrics, ws, k1,k2,sigma,r,win,sigma_nsq,subbands,M,level,weight,method,VA,
    			alpha, vsnr_data_b, vsnr_data_k, vsnr_data_g, vsnr_data_r, vsnr_data_v,
				vsnr_data_num_levels, vsnr_data_filter_gains, 
    			results);
		iq.runAlgorithm();
		System.out.println("Website NQM for I01_01_1.BMP = 30.7619");
		System.out.println("Calculated NQM for I01_01_1.BMP = " + results[0]);
		
		if (testsFailed > 0) {
			System.err.println(testsFailed + " tests failed for NQM");
		}
		else {
			System.out.println("All tests passed for NQM");
		}
		
		colorRef.disposeLocal();
		colorRef = null;
		colorA.disposeLocal();
		colorA = null;
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
		ImageQuality iq = new ImageQuality(gry, gry_noise, metrics,ws,k1,k2,sigma,r,win,sigma_nsq,subbands,M,level,weight,method,VA,
				alpha, vsnr_data_b, vsnr_data_k, vsnr_data_g, vsnr_data_r, vsnr_data_v,
				vsnr_data_num_levels, vsnr_data_filter_gains, results);
		iq.runAlgorithm();
		if (Math.abs(15.0646-results[0]) >= eps) {
			System.err.println("Block sensitive peak signal to noise ratio = " + results[0] + " for gry, gry_noise\n");
			testsFailed++;
		}
		
		iq = new ImageQuality(clr, clr_noise, metrics, ws, k1,k2,sigma,r,win,sigma_nsq,subbands,M,level,weight,method,VA,
				alpha, vsnr_data_b, vsnr_data_k, vsnr_data_g, vsnr_data_r, vsnr_data_v,
				vsnr_data_num_levels, vsnr_data_filter_gains, 
				results);
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
		// Visual information fidelity = 0.9999999999999993
		// Visual information fidelity = 0.9999999999999992
		// Website VIF for contrast enhanced image = 1.10
		// Calculated VIF for contrast enhanced image = 1.1199409945177197
        // Website VIF for blurred image = 0.07
        // Calculated VIF for blurred image = 0.22238035280832483
        // Website VIF for JPEG compressed image = 0.10
        // Calculated VIF for JPEG compressed image = 0.216630221353973

		// All tests passed for visual information fidelity
		sigma_nsq = 0.4;
		int testsFailed = 0;
		double eps = 1.0E-3;
		metrics = new int[] {VISUAL_INFORMATION_FIDELITY};
		results = new double[1];
		ImageQuality iq = new ImageQuality(clr, clr, metrics,ws,k1,k2,sigma,r,win,sigma_nsq,subbands,M,level,weight,method,VA,
				alpha, vsnr_data_b, vsnr_data_k, vsnr_data_g, vsnr_data_r, vsnr_data_v,
				vsnr_data_num_levels, vsnr_data_filter_gains, 
				results);
		iq.runAlgorithm();
		if (Math.abs(1.0-results[0]) >= eps) {
			System.err.println("Visual information fidelity = " + results[0] + " for clr, clr\n");
			testsFailed++;
		}
		
		iq = new ImageQuality(gry, gry, metrics, ws, k1,k2,sigma,r,win,sigma_nsq,subbands,M,level,weight,method,VA,
				alpha, vsnr_data_b, vsnr_data_k, vsnr_data_g, vsnr_data_r, vsnr_data_v,
				vsnr_data_num_levels, vsnr_data_filter_gains, 
				results);
		iq.runAlgorithm();
		if (Math.abs(1.0-results[0]) >= eps) {
			System.err.println("Visual information fidelity = " + results[0] + " for gry, gry\n");
			testsFailed++;
		}
		
		String fileDir = "C:/Image Quality/vifvec_release/";
		final FileIO fileIO = new FileIO();
		fileIO.setQuiet(true);
    	fileIO.setSuppressProgressBar(true);
    	// goldhill picture examples from https://live.ece.edu/research/Quality/VIF.htm
    	// Reference VIF = 1.0
    	ModelImage gryA = fileIO.readJimi("goldhill.gif", fileDir, false);
    	// Contrast enhanced VIF = 1.10
    	// Note that an enhanced version of the image gives a VIF > 1.0
    	ModelImage gryB = fileIO.readJimi("goldhill_cs.gif", fileDir, false);
    	iq = new ImageQuality(gryA, gryB, metrics, ws, k1,k2,sigma,r,win,sigma_nsq,subbands,M,level,weight,method,VA,
    			alpha, vsnr_data_b, vsnr_data_k, vsnr_data_g, vsnr_data_r, vsnr_data_v,
				vsnr_data_num_levels, vsnr_data_filter_gains, 
    			results);
		iq.runAlgorithm();
		System.out.println("Website VIF for contrast enhanced image = 1.10");
		System.out.println("Calculated VIF for contrast enhanced image = " + results[0]);
		
		// Blurred image VIF = 0.07
		ModelImage gryC = fileIO.readJimi("goldhill_blur.gif", fileDir, false);
		iq = new ImageQuality(gryA, gryC, metrics, ws, k1,k2,sigma,r,win,sigma_nsq,subbands,M,level,weight,method,VA,
				alpha, vsnr_data_b, vsnr_data_k, vsnr_data_g, vsnr_data_r, vsnr_data_v,
				vsnr_data_num_levels, vsnr_data_filter_gains, 
				results);
		iq.runAlgorithm();
		System.out.println("Website VIF for blurred image = 0.07");
		System.out.println("Calculated VIF for blurred image = " + results[0]);
		
		// JPEG compressed VIF = 0.10
		ModelImage gryD = fileIO.readJimi("goldhill_jpeg.gif", fileDir, false);
		iq = new ImageQuality(gryA, gryD, metrics, ws, k1,k2,sigma,r,win,sigma_nsq,subbands,M,level,weight,method,VA,
				alpha, vsnr_data_b, vsnr_data_k, vsnr_data_g, vsnr_data_r, vsnr_data_v,
				vsnr_data_num_levels, vsnr_data_filter_gains, 
				results);
		iq.runAlgorithm();
		System.out.println("Website VIF for JPEG compressed image = 0.10");
		System.out.println("Calculated VIF for JPEG compressed image = " + results[0]);
		
		if (testsFailed > 0) {
			System.err.println(testsFailed + " tests failed for visual information fidelity");
		}
		else {
			System.out.println("All tests passed for visual information fidelity");
		}
		
		gryA.disposeLocal();
		gryA = null;
		gryB.disposeLocal();
		gryB = null;
		gryC.disposeLocal();
		gryC = null;
		gryD.disposeLocal();
		gryD = null;
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
	
	public void testVIFP() {
		// All tests passed for pixel based visual information fidelity
		sigma_nsq = 2.0;
		int testsFailed = 0;
		double eps = 1.0E-3;
		metrics = new int[] {PIXEL_BASED_VISUAL_INFORMATION_FIDELITY};
		results = new double[1];
		ImageQuality iq = new ImageQuality(clr, clr, metrics,ws,k1,k2,sigma,r,win,sigma_nsq,subbands,M,level,weight,method,VA,
				alpha, vsnr_data_b, vsnr_data_k, vsnr_data_g, vsnr_data_r, vsnr_data_v,
				vsnr_data_num_levels, vsnr_data_filter_gains, 
				results);
		iq.runAlgorithm();
		if (Math.abs(1.0-results[0]) >= eps) {
			System.err.println("Pixel based visual information fidelity = " + results[0] + " for clr, clr\n");
			testsFailed++;
		}
		
		iq = new ImageQuality(gry, gry, metrics, ws, k1,k2,sigma,r,win,sigma_nsq,subbands,M,level,weight,method,VA,
				alpha, vsnr_data_b, vsnr_data_k, vsnr_data_g, vsnr_data_r, vsnr_data_v,
				vsnr_data_num_levels, vsnr_data_filter_gains, 
				results);
		iq.runAlgorithm();
		if (Math.abs(1.0-results[0]) >= eps) {
			System.err.println("Pixel based visual information fidelity = " + results[0] + " for gry, gry\n");
			testsFailed++;
		}
		
		iq = new ImageQuality(gry, gry_noise, metrics, ws, k1,k2,sigma,r,win,sigma_nsq,subbands,M,level,weight,method,VA,
				alpha, vsnr_data_b, vsnr_data_k, vsnr_data_g, vsnr_data_r, vsnr_data_v,
				vsnr_data_num_levels, vsnr_data_filter_gains, 
				results);
		iq.runAlgorithm();
		if (Math.abs(0.120490551257006-results[0]) >= eps) {
			System.err.println("Pixel based visual information fidelity = " + results[0] + " for gry, gry_noise\n");
			testsFailed++;
		}
		
		iq = new ImageQuality(gry, gry_const, metrics, ws, k1,k2,sigma,r,win,sigma_nsq,subbands,M,level,weight,method,VA,
				alpha, vsnr_data_b, vsnr_data_k, vsnr_data_g, vsnr_data_r, vsnr_data_v,
				vsnr_data_num_levels, vsnr_data_filter_gains, 
				results);
		iq.runAlgorithm();
		if (Math.abs(0.981413452665522-results[0]) >= eps) {
			System.err.println("Pixel based visual information fidelity = " + results[0] + " for gry, gry_const\n");
			testsFailed++;
		}
		
		if (testsFailed > 0) {
			System.err.println(testsFailed + " tests failed for pixel based visual information fidelity");
		}
		else {
			System.out.println("All tests passed for pixel based visual information fidelity");
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
	
	public void testVSNR() {
		int testsFailed = 0;
		double eps = 1.0E-3;
		metrics = new int[] {VSNR};
		results = new double[1];
		ImageQuality iq = new ImageQuality(clr, clr, metrics,ws,k1,k2,sigma,r,win,sigma_nsq,subbands,M,level,weight,method,VA,
				alpha, vsnr_data_b, vsnr_data_k, vsnr_data_g, vsnr_data_r, vsnr_data_v,
				vsnr_data_num_levels, vsnr_data_filter_gains, 
				results);
		iq.runAlgorithm();
		
		
		iq = new ImageQuality(gry, gry, metrics, ws, k1,k2,sigma,r,win,sigma_nsq,subbands,M,level,weight,method,VA,
				alpha, vsnr_data_b, vsnr_data_k, vsnr_data_g, vsnr_data_r, vsnr_data_v,
				vsnr_data_num_levels, vsnr_data_filter_gains, 
				results);
		iq.runAlgorithm();
		
		iq = new ImageQuality(gry, gry_noise, metrics, ws, k1,k2,sigma,r,win,sigma_nsq,subbands,M,level,weight,method,VA,
				alpha, vsnr_data_b, vsnr_data_k, vsnr_data_g, vsnr_data_r, vsnr_data_v,
				vsnr_data_num_levels, vsnr_data_filter_gains, 
				results);
		iq.runAlgorithm();
		
		
		iq = new ImageQuality(gry, gry_const, metrics, ws, k1,k2,sigma,r,win,sigma_nsq,subbands,M,level,weight,method,VA,
				alpha, vsnr_data_b, vsnr_data_k, vsnr_data_g, vsnr_data_r, vsnr_data_v,
				vsnr_data_num_levels, vsnr_data_filter_gains, 
				results);
		iq.runAlgorithm();
		
		String referenceFileDir = "C:/Image Quality/tid2008/reference_images/";
		String distortedFileDir = "C:/Image Quality/tid2008/distorted_images/";
		final FileIO fileIO = new FileIO();
		fileIO.setQuiet(true);
    	fileIO.setSuppressProgressBar(true);
    	ModelImage colorRef = fileIO.readJimi("I01.BMP", referenceFileDir, false);
    	ModelImage colorA = fileIO.readJimi("I01_01_1.BMP", distortedFileDir, false);
    	iq = new ImageQuality(colorRef, colorA, metrics, ws, k1,k2,sigma,r,win,sigma_nsq,subbands,M,level,weight,method,VA,
    			alpha, vsnr_data_b, vsnr_data_k, vsnr_data_g, vsnr_data_r, vsnr_data_v,
				vsnr_data_num_levels, vsnr_data_filter_gains, 
    			results);
		iq.runAlgorithm();
		System.out.println("Website VSNR for I01_01_1.BMP = 31.0664");
		System.out.println("Calculated VSNR for I01_01_1.BMP = " + results[0]);
		
		
		if (testsFailed > 0) {
			System.err.println(testsFailed + " tests failed for VSNR");
		}
		else {
			System.out.println("All tests passed for VSNR");
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
	
	// Used to call MEAN_SQUARED_ERROR, ROOT_MEAN_SQUARED_ERROR, PEAK_SIGNAL_TO_NOISE_RATIO, 
	// SPECTRAL_ANGLE_MAPPER, and BLOCK_SENSITIVE_PEAK_SIGNAL_TO_NOISE_RATIO
	public ImageQuality(ModelImage referenceImage, ModelImage testImage, int metrics[], double results[]) {
		    this(referenceImage, testImage, metrics, 8, 0.01,0.03,1.5,4.0,new double[][] {{-1,-1,-1},{-1,8,-1},{-1,-1,-1}},
		    		2.0,new int[] {3, 6, 9, 12, 15, 18, 21, 24},3,5,new double[] {0.0448, 0.2856, 0.3001, 0.2363, 0.1333},
		    		PRODUCT,4.0,0.04, 0, 0.02874, 2.2, 96.0, 19.1,5, null, results);
	}
	
	// Used to call RMSE_SW, UNIVERSAL_QUALITY_IMAGE_INDEX, and RELATIVE_AVERAGE_SPECTRAL_ERROR
	public ImageQuality(ModelImage referenceImage, ModelImage testImage, int metrics[], int ws, double results[]) {
	    this(referenceImage, testImage, metrics, ws, 0.01,0.03,1.5,4.0,new double[][] {{-1,-1,-1},{-1,8,-1},{-1,-1,-1}},
	    		2.0,new int[] {3, 6, 9, 12, 15, 18, 21, 24},3,5,new double[] {0.0448, 0.2856, 0.3001, 0.2363, 0.1333},
	    		PRODUCT,4.0,0.04, 0, 0.02874, 2.2, 96.0, 19.1,5, null, results);
    }
	
	// Used to call STRUCTURAL_SIMILARITY_INDEX and SSIM_WITH_AUTOMATIC_DOWNSAMPLING
	public ImageQuality(ModelImage referenceImage, ModelImage testImage, int metrics[], int ws, double k1, double k2,
			double sigma, double results[]) {
	    this(referenceImage, testImage, metrics, ws, k1,k2,sigma,4.0,new double[][] {{-1,-1,-1},{-1,8,-1},{-1,-1,-1}},
	    		2.0,new int[] {3, 6, 9, 12, 15, 18, 21, 24},3,5,new double[] {0.0448, 0.2856, 0.3001, 0.2363, 0.1333},
	    		PRODUCT,4.0,0.04, 0, 0.02874, 2.2, 96.0, 19.1,5, null, results);
    }
	
	// Used to call MULTI_SCALE_STRUCTURAL_SIMILARITY_INDEX
	public ImageQuality(ModelImage referenceImage, ModelImage testImage, int metrics[], int ws, double k1, double k2,
			double sigma, int level, double weight[], int method, double results[]) {
	    this(referenceImage, testImage, metrics, ws, k1,k2,sigma,4.0,new double[][] {{-1,-1,-1},{-1,8,-1},{-1,-1,-1}},
	    		2.0,new int[] {3, 6, 9, 12, 15, 18, 21, 24},3,level,weight,
	    		method,4.0,0.04, 0, 0.02874, 2.2, 96.0, 19.1,5, null, results);
    }
	
	// Used in ERGAS
	public ImageQuality(ModelImage referenceImage, ModelImage testImage, int metrics[], int ws, double r, double results[]) {
	    this(referenceImage, testImage, metrics, ws, 0.01,0.03,1.5,r,new double[][] {{-1,-1,-1},{-1,8,-1},{-1,-1,-1}},
	    		2.0,new int[] {3, 6, 9, 12, 15, 18, 21, 24},3,5,new double[] {0.0448, 0.2856, 0.3001, 0.2363, 0.1333},
	    		PRODUCT,4.0,0.04, 0, 0.02874, 2.2, 96.0, 19.1,5, null, results);
    }
	
	// Used in SPATIAL_CORRELATION_COEFFICIENT
	public ImageQuality(ModelImage referenceImage, ModelImage testImage, int metrics[], int ws, double win[][], double results[]) {
	    this(referenceImage, testImage, metrics, ws, 0.01,0.03,1.5,4.0,win,
	    		2.0,new int[] {3, 6, 9, 12, 15, 18, 21, 24},3,5,new double[] {0.0448, 0.2856, 0.3001, 0.2363, 0.1333},
	    		PRODUCT,4.0,0.04, 0, 0.02874, 2.2, 96.0, 19.1,5, null, results);
    }
	
	// Used in PIXEL_BASED_VISUAL_INFORMATION_FIDELITY
	public ImageQuality(ModelImage referenceImage, ModelImage testImage, int metrics[], double sigma_nsq, double results[]) {
	    this(referenceImage, testImage, metrics, 8, 0.01,0.03,1.5,4.0,new double[][] {{-1,-1,-1},{-1,8,-1},{-1,-1,-1}},
	    		sigma_nsq,new int[] {3, 6, 9, 12, 15, 18, 21, 24},3,5,new double[] {0.0448, 0.2856, 0.3001, 0.2363, 0.1333},
	    		PRODUCT,4.0,0.04, 0, 0.02874, 2.2, 96.0, 19.1,5, null, results);
    }
	
	// Used in VISUAL_INFORMATION_FIDELITY
	public ImageQuality(ModelImage referenceImage, ModelImage testImage, int metrics[], double sigma_nsq, 
			int subbands[], int M, double results[]) {
	    this(referenceImage, testImage, metrics, 8, 0.01,0.03,1.5,4.0,new double[][] {{-1,-1,-1},{-1,8,-1},{-1,-1,-1}},
	    		sigma_nsq,subbands,M,5,new double[] {0.0448, 0.2856, 0.3001, 0.2363, 0.1333},
	    		PRODUCT,4.0,0.04, 0, 0.02874, 2.2, 96.0, 19.1,5, null, results);
    }
	
    // Used in NQM
	public ImageQuality(ModelImage referenceImage, ModelImage testImage, double VA,  int metrics[], double results[]) {
    this(referenceImage, testImage, metrics, 8, 0.01,0.03,1.5,4.0,new double[][] {{-1,-1,-1},{-1,8,-1},{-1,-1,-1}},
    		2.0,new int[] {3, 6, 9, 12, 15, 18, 21, 24},3,5,new double[] {0.0448, 0.2856, 0.3001, 0.2363, 0.1333},
    		PRODUCT,VA,0.04, 0, 0.02874, 2.2, 96.0, 19.1,5, null, results);
    }
	
	// Used in VSNR
	public ImageQuality(ModelImage referenceImage, ModelImage testImage, int metrics[], double alpha,
			int vsnr_data_b, double vsnr_data_k, double vsnr_data_g, double vsnr_data_r,
			double vsnr_data_v, int vsnr_data_num_levels, int vsnr_data_filter_gains[],
			double results[]) {
	    this(referenceImage, testImage, metrics, 8, 0.01,0.03,1.5,4.0,new double[][] {{-1,-1,-1},{-1,8,-1},{-1,-1,-1}},
	    		2.0,new int[] {3, 6, 9, 12, 15, 18, 21, 24},3,5,new double[] {0.0448, 0.2856, 0.3001, 0.2363, 0.1333},
	    		PRODUCT,4.0,alpha, vsnr_data_b, vsnr_data_k, vsnr_data_g, vsnr_data_r, 
	    		vsnr_data_v, vsnr_data_num_levels, vsnr_data_filter_gains, results);
    }
	
	public ImageQuality(ModelImage referenceImage, ModelImage testImage, int metrics[], int ws,
			double k1, double k2, double sigma, double r, double win[][], double sigma_nsq, 
			int subbands[], int M, int level, double weight[], int method, double VA, double alpha,
			int vsnr_data_b, double vsnr_data_k, double vsnr_data_g, double vsnr_data_r,
			double vsnr_data_v, int vsnr_data_num_levels, int vsnr_data_filter_gains[],
			double results[]) {
		if (metrics == null) {
			MipavUtil.displayError("metrics is null in ImageQuality");
			return;
		}
		if (metrics.length == 0) {
			MipavUtil.displayError("metrics.length == 0 in ImageQuality");
			return;
		}
		if (metrics.length > 17) {
			MipavUtil.displayError("metrics.length > 17 in ImageQuality");
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
			if ((metrics[i] < 1) || (metrics[i] > 17)) {
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
    	this.subbands = subbands;
    	this.M = M;
    	this.level = level;
    	this.weight = weight;
    	this.method = method;
    	this.VA = VA;
    	this.alpha = alpha;
    	this.vsnr_data_b = vsnr_data_b;
    	this.vsnr_data_k = vsnr_data_k;
    	this.vsnr_data_g = vsnr_data_g;
    	this.vsnr_data_r = vsnr_data_r;
    	this.vsnr_data_v = vsnr_data_v;
    	this.vsnr_data_num_levels = vsnr_data_num_levels;
    	this.vsnr_data_filter_gains = vsnr_data_filter_gains;
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
	        
	        // Handle gray scale image stored in color format
	        
	        boolean sameColorValues = true;
	        for (i = 0; (i < length) && sameColorValues; i++) {
	        	if ((testRedBuffer[i] != testGreenBuffer[i]) || (testRedBuffer[i] != testBlueBuffer[i]) ||
	        			(referenceRedBuffer[i] != referenceGreenBuffer[i]) || (referenceRedBuffer[i] != referenceBlueBuffer[i])) {
	        		sameColorValues = false;
	        	}
	        }
	        
	        if (sameColorValues) {
	        	testBuffer = new double[length];
	        	referenceBuffer = new double[length];
	        	for (i = 0; i < length; i++) {
	        		testBuffer[i] = (double)testRedBuffer[i];
	        		referenceBuffer[i] = (double)referenceBuffer[i];
	        	}
	        	testRedBuffer = null;
	        	testGreenBuffer = null;
	        	testBlueBuffer = null;
	        	referenceRedBuffer = null;
	        	referenceGreenBuffer = null;
	        	referenceBlueBuffer = null;
	        	isColor = false;
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
		    case MULTI_SCALE_STRUCTURAL_SIMILARITY_INDEX:
		    	msssim();
		    	results[i] = overall_mssim;
		    	break;
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
		    	vifvec();
		    	results[i] = vif;
		    	break;
		    case PIXEL_BASED_VISUAL_INFORMATION_FIDELITY:
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
		    case NQM:
		    	nqm();
		    	results[i] = nqm_value;
		    	break;
		    case VSNR:
		    	vsnr();
		    	results[i] = vsnr_value;
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
    
    private void msssim() {
    	// Multi-scale Structural Similarity Index (MS-SSIM)
    	// Z. Wang, E. P. Simoncelli and A. C. Bovik, "Multi-scale structural similarity
    	// for image quality assessment," Invited Paper, IEEE Asilomar Conference on
    	// Signals, Systems and Computers, Nov. 2003
    	
    	// calculates multi-scale structural similarity index (ms-ssim).

    	// param GT: first (original) input image.
    	// param P: second (deformed) input image.
    	// param weights: weights for each scale (default = [0.0448, 0.2856, 0.3001, 0.2363, 0.1333]).
    	// param ws: sliding window size (default = 11).
    	// param K1: First constant for SSIM (default = 0.01).
    	// param K2: Second constant for SSIM (default = 0.03).
    	// param MAX: Maximum value of datarange (if None, MAX is calculated using image dtype).
        // sigma = 1.5
    	// level = 5 default
    	// method = PRODUCT DEFAULT, alternative WTD_SUM
    	
    	// returns:  float -- ms-ssim value.
    	int i;
    	if (k1 < 0) {
    		System.err.println("k1 < 0 in msssim()");
    		return;
    	}
    	
    	if (k2 < 0) {
    		System.err.println("k2 < 0 in msssim()");
    		return;
    	}
    	
    	if (xDim < ws) {
    		System.err.println("xDim < ws in msssim()");
    		return;
    	}
    	
    	if (yDim < ws) {
    		System.err.println("yDim < ws in msssim()");
    		return;
    	}
    	
    	if (xDim < 11) {
    		System.err.println("xDim < 11 in msssim()");
    		return;
    	}
    	
    	if (yDim < 11) {
    		System.err.println("yDim < 11 in msssim()");
    		return;
    	}
    	
    	if (ws < 3) {
    		System.err.println("ws < 3 in msssim()");
    		return;
    	}
    	
    	if (level < 1) {
    		System.err.println("level < 1 in mssim()");
    		return;
    	}
    	
    	int min_img_size = (int)(Math.min(xDim,yDim)/Math.pow(2.0,level-1));
    	if (ws > min_img_size) {
    		System.err.println("ws > min_img_size in msssim()");
    		return;
    	}
    	
    	if (weight.length != level) {
    		System.err.println("weight.length != level in msssim()");
    		return;
    	}
    	
    	double weight_sum = 0.0;
    	for (i = 0; i < weight.length; i++) {
    		weight_sum += weight[i];
    	}
    	
    	if (weight_sum == 0) {
    		System.err.println("Sum of weights = 0 in msssim()");
    		return;
    	}
    	
    	if ((method != PRODUCT) && (method != WTD_SUM)) {
    		System.err.println("method != PRODUCT and methd != WTD_SUM in msssim()");
    		return;
    	}
    	
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
    	int scaleYDim2;
    	int scaleXDim2;
    	double scaleReferenceBuffer[] = null;
    	double scaleTestBuffer[] = null;
    	float scaleReferenceRedBuffer[] = null;
    	float scaleTestRedBuffer[] = null;
    	float scaleReferenceGreenBuffer[] = null;
    	float scaleTestGreenBuffer[] = null;
    	float scaleReferenceBlueBuffer[] = null;
        float scaleTestBlueBuffer[] = null;
    	int scaleIndex;
    	int l;
    	double mssim[] = new double[level];
    	double mcs[] = new double[level];
    	double sumcs;
    	
    	
    	double downsample_filter[][] = new double[][] {{0.25,0.25},{0.25,0.25}};
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
    	for (l = 0; l < level; l++) {
    		double GTGT[][] = new double[scaleYDim][scaleXDim];
        	double PP[][] = new double[scaleYDim][scaleXDim];
        	double GTP[][] = new double[scaleYDim][scaleXDim];	
        	
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
    	    	double cs_map[][] = new double[filtYDim][filtXDim];
    	    	if ((c1 > 0) && (c2 > 0)) {
    	    		for (y = 0; y < filtYDim; y++) {
    		    		for (x = 0; x < filtXDim; x++) {
    		    			ssim_map[y][x] = ((2*GT_P_sum_mul[y][x] + c1)*(2*sigmaGT_P[y][x] + c2))/
    		    					((GT_sum_sq[y][x] + P_sum_sq[y][x] + c1)*(sigmaGT_sq[y][x] + sigmaP_sq[y][x] + c2));
    		    			cs_map[y][x] = (2*sigmaGT_P[y][x] + c2)/(sigmaGT_sq[y][x] + sigmaP_sq[y][x] + c2);
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
    		    			
    		    			cs_map[y][x] = 1.0;
    		    			if (denominator2 > 0.0) {
    		    				cs_map[y][x] = numerator2/denominator2;	
    		    			}
    		    		}
    	    		}
    	    	}
    	    	sum = 0.0;
    	    	sumcs = 0.0;
    	    	for (y = 0; y < filtYDim; y++) {
    	    		for (x = 0; x < filtXDim; x++) {
    	    			sum += ssim_map[y][x];
    	    			sumcs += cs_map[y][x];
    	    		}
    	    	}
    	    	mssim[l] = sum/(filtXDim * filtYDim);
    	    	mcs[l] = sumcs/(filtXDim * filtYDim);
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
    	    	double cs_map[][] = new double[filtYDim][filtXDim];
    	    	if ((c1 > 0) && (c2 > 0)) {
    	    		for (y = 0; y < filtYDim; y++) {
    		    		for (x = 0; x < filtXDim; x++) {
    		    			ssim_map[y][x] = ((2*GT_P_sum_mul[y][x] + c1)*(2*sigmaGT_P[y][x] + c2))/
    		    					((GT_sum_sq[y][x] + P_sum_sq[y][x] + c1)*(sigmaGT_sq[y][x] + sigmaP_sq[y][x] + c2));
    		    			cs_map[y][x] = (2*sigmaGT_P[y][x] + c2)/(sigmaGT_sq[y][x] + sigmaP_sq[y][x] + c2);
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
    		    			
    		    			cs_map[y][x] = 1.0;
    		    			if (denominator2 > 0.0) {
    		    				cs_map[y][x] = numerator2/denominator2;	
    		    			}
    		    		}
    	    		}
    	    	}
    	    	sum = 0.0;
    	    	sumcs = 0.0;
    	    	for (y = 0; y < filtYDim; y++) {
    	    		for (x = 0; x < filtXDim; x++) {
    	    			sum += ssim_map[y][x];
    	    			sumcs += cs_map[y][x];
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
    		    			cs_map[y][x] = (2*sigmaGT_P[y][x] + c2)/(sigmaGT_sq[y][x] + sigmaP_sq[y][x] + c2);
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
    		    			
    		    			cs_map[y][x] = 1.0;
    		    			if (denominator2 > 0.0) {
    		    				cs_map[y][x] = numerator2/denominator2;	
    		    			}
    		    		}
    	    		}
    	    	}
    	    	for (y = 0; y < filtYDim; y++) {
    	    		for (x = 0; x < filtXDim; x++) {
    	    			sum += ssim_map[y][x];
    	    			sumcs += cs_map[y][x];
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
    		    			cs_map[y][x] = (2*sigmaGT_P[y][x] + c2)/(sigmaGT_sq[y][x] + sigmaP_sq[y][x] + c2);
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
    		    			
    		    			cs_map[y][x] = 1.0;
    		    			if (denominator2 > 0.0) {
    		    				cs_map[y][x] = numerator2/denominator2;	
    		    			}
    		    		}
    	    		}
    	    	}
    	    	for (y = 0; y < filtYDim; y++) {
    	    		for (x = 0; x < filtXDim; x++) {
    	    			sum += ssim_map[y][x];
    	    			sumcs += cs_map[y][x];
    	    		}
    	    	}
    	        mssim[l] = sum/(3.0*filtXDim*filtYDim);	
    	        mcs[l] = sumcs/(3.0*filtXDim*filtYDim);
        	} // isColor
        	if (!isColor) {
	        	for (y = 0; y < scaleYDim; y++) {
		    		for (x = 0; x < scaleXDim; x++) {
		    			index = x + y*scaleXDim;
		    			refBuf2D[y][x] = scaleReferenceBuffer[index];
		    			testBuf2D[y][x] = scaleTestBuffer[index];
		    		}
				}
				double imgGT[][] = filter2Same(refBuf2D, downsample_filter);
				double imgP[][] = filter2Same(testBuf2D,downsample_filter);
				scaleYDim2 = 1 + (scaleYDim - 1)/2;
				scaleXDim2 = 1 + (scaleXDim - 1)/2;
				scaleReferenceBuffer = new double[scaleYDim2*scaleXDim2];
				scaleTestBuffer = new double[scaleYDim2*scaleXDim2];
				for (y = 0; y < scaleYDim2; y++) {
					for (x = 0; x < scaleXDim2; x++) {
					    scaleIndex = x + y*scaleXDim2;
					    scaleReferenceBuffer[scaleIndex] = imgGT[2*y][2*x];
					    scaleTestBuffer[scaleIndex] = imgP[2*y][2*x];
					}
				}
		    } // if (!isColor)
			else { // isColor
				for (y = 0; y < scaleYDim; y++) {
		    		for (x = 0; x < scaleXDim; x++) {
		    			index = x + y*scaleXDim;
		    			refBuf2D[y][x] = scaleReferenceRedBuffer[index];
		    			testBuf2D[y][x] = scaleTestRedBuffer[index];
		    		}
				}
				double imgGT[][] = filter2Same(refBuf2D, downsample_filter);
				double imgP[][] = filter2Same(testBuf2D,downsample_filter);
				scaleYDim2 = 1 + (scaleYDim - 1)/2;
				scaleXDim2 = 1 + (scaleXDim - 1)/2;
				scaleReferenceRedBuffer = new float[scaleYDim*scaleXDim2];
				scaleTestRedBuffer = new float[scaleYDim2*scaleXDim2];
				for (y = 0; y < scaleYDim2; y++) {
					for (x = 0; x < scaleXDim2; x++) {
					    scaleIndex = x + y*scaleXDim2;
					    scaleReferenceRedBuffer[scaleIndex] = (float)imgGT[2*y][2*x];
					    scaleTestRedBuffer[scaleIndex] = (float)imgP[2*y][2*x];
					}
				}	
				
				for (y = 0; y < scaleYDim; y++) {
		    		for (x = 0; x < scaleXDim; x++) {
		    			index = x + y*scaleXDim;
		    			refBuf2D[y][x] = scaleReferenceGreenBuffer[index];
		    			testBuf2D[y][x] = scaleTestGreenBuffer[index];
		    		}
				}
				imgGT = filter2Same(refBuf2D, downsample_filter);
				imgP = filter2Same(testBuf2D,downsample_filter);
				scaleReferenceGreenBuffer = new float[scaleYDim2*scaleXDim2];
				scaleTestGreenBuffer = new float[scaleYDim2*scaleXDim2];
				for (y = 0; y < scaleYDim2; y++) {
					for (x = 0; x < scaleXDim2; x++) {
					    scaleIndex = x + y*scaleXDim2;
					    scaleReferenceGreenBuffer[scaleIndex] = (float)imgGT[2*y][2*x];
					    scaleTestGreenBuffer[scaleIndex] = (float)imgP[2*y][2*x];
					}
				}
				
				for (y = 0; y < scaleYDim; y++) {
		    		for (x = 0; x < scaleXDim; x++) {
		    			index = x + y*scaleXDim;
		    			refBuf2D[y][x] = scaleReferenceBlueBuffer[index];
		    			testBuf2D[y][x] = scaleTestBlueBuffer[index];
		    		}
				}
				imgGT = filter2Same(refBuf2D, downsample_filter);
				imgP = filter2Same(testBuf2D,downsample_filter);
				scaleReferenceBlueBuffer = new float[scaleYDim2*scaleXDim2];
				scaleTestBlueBuffer = new float[scaleYDim2*scaleXDim2];
				for (y = 0; y < scaleYDim2; y++) {
					for (x = 0; x < scaleXDim2; x++) {
					    scaleIndex = x + y*scaleXDim2;
					    scaleReferenceBlueBuffer[scaleIndex] = (float)imgGT[2*y][2*x];
					    scaleTestBlueBuffer[scaleIndex] = (float)imgP[2*y][2*x];
					}
				}	
			} // isColor
        scaleYDim = scaleYDim2;
        scaleXDim = scaleXDim2;
		refBuf2D = new double[scaleYDim][scaleXDim];
		testBuf2D = new double[scaleYDim][scaleXDim];
    	} // for (l = 1; l <= level; l++)
    	
    	if (method == PRODUCT) {
		   // overall_mssim = prod(mssim_array.^weight);
    	   overall_mssim = 1.0;
		   for (i = 0; i < level-1; i++) {
			   overall_mssim *= Math.pow(mcs[i],weight[i]);
		   }
		   overall_mssim *= Math.pow(mssim[level-1],weight[level-1]);
    	}
		else {
		   for (i = 0; i < weight.length; i++) {
			   weight[i] = weight[i]/weight_sum;
		   }
		   overall_mssim = 0.0;
		   for (i = 0; i < level-1; i++) {
			   overall_mssim += mcs[i]*weight[i];
		   }
		   overall_mssim += mssim[level-1]*weight[level-1];
		}

    	UI.setDataText("Multi scale structural similarity index = " + overall_mssim + "\n");
    	System.out.println("Multi scale structural similarity index = " + overall_mssim);	
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
    	int f = (int)Math.max(1,Math.round(Math.min(xDim,yDim)/256.0));
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
        UI.setDataText("Pixel based visual information fidelity = " + vifp_mean + "\n");
        System.out.println("Pixel based visual information fidelity = " + vifp_mean);
        return;
    }
    
    private void vifvec() {
    	int i,j,x,y,index,kk,r,c;
    	int sub;
    	double g[][];
    	double vv[][];
    	double ss[][];
    	double lambda[];
    	//double cu[][];
    	//int neigvals;
    	int lev;
    	int winsize;
    	double offsetf;
    	int offset;
    	double gvalid[][];
    	double vvvalid[][];
    	double ssvalid[][];
    	double temp1;
    	double temp2;
    	double sumnum;
    	double sumden;
    	if (referenceImage.isColorImage()) {
    		referenceBuffer = new double[length];
    		for (i = 0; i < length; i++) {
    			referenceBuffer[i] = (double)Math.max(referenceRedBuffer[i],Math.max(referenceGreenBuffer[i],referenceBlueBuffer[i]));
    		}
    	}
    	
    	if (testImage.isColorImage()) {
    		testBuffer = new double[length];
    		for (i = 0; i < length; i++) {
    			testBuffer[i] = (double)Math.max(testRedBuffer[i],Math.max(testGreenBuffer[i],testBlueBuffer[i]));
    		}
    	}
    	
    	
    	double refBuf2D[][] = new double[yDim][xDim];
    	double testBuf2D[][] = new double[yDim][xDim];
    	for (y = 0; y < yDim; y++) {
    		for (x = 0; x < xDim; x++) {
    		    index = x + y*xDim;
    		    refBuf2D[y][x] = referenceBuffer[index];
    		    testBuf2D[y][x] = testBuffer[index];
    		}
    	}
    	
    	// Do wavelet decomposition. This requires the Steerable Pyramid. You can
    	// use your own wavelet as long as the cell arrays org and dist contain
    	// corresponding subbands from the reference and the distorted images
    	// respectively.
    	Vector<double[][]>org = new Vector<double[][]>();
    	Vector<int[]>pind = new Vector<int[]>();
    	Vector<int[]>harmonics = new Vector<int[]>();
    	Vector<double[][]>steermtx = new Vector<double[][]>();
    	PyramidToolbox pyramid = new PyramidToolbox();
    	pyramid.buildSpyr(org, pind, harmonics, steermtx, refBuf2D, 4, "sp5Filters", BORDER_REFLECT_101); // compute transform
    	Collections.reverse(org);
    	pind.clear();
    	harmonics.clear();
    	steermtx.clear();
    	Vector<double[][]>dist = new Vector<double[][]>();
    	pyramid.buildSpyr(dist, pind, harmonics, steermtx, testBuf2D, 4, "sp5Filters", BORDER_REFLECT_101); 
    	Collections.reverse(dist);
    	
    	// calculate the parameters of the distortion channel
    	Vector<double[][]>g_all = new Vector<double[][]>();
    	Vector<double[][]>vv_all = new Vector<double[][]>();
        vifsub_est_M(g_all,vv_all,org,dist);
        
        // calculate the parameters of the reference image
        double ssarr[][][] = new double[org.size()][][];
        double larr[][] = new double[org.size()][M*M];
        //double cuarr[][][] = new double[subbands.length][][];
        //refparams_vecgsm(ssarr, larr, cuarr, org);
        refparams_vecgsm(ssarr, larr, org);
        
        // reorder subbands. This is needed since the outputs of the above functions
        // are not in the same order
        int subbands_max = -1;
        for (i = 0; i < subbands.length; i++) {
        	if (subbands[i] > subbands_max) {
                subbands_max = subbands[i];
        	}
        }
        double vvtemp[][][] =new double[subbands_max+1][][];
        double ggtemp[][][] = new double[subbands_max+1][][];
        for(kk=0; kk < subbands.length; kk++) {
            vvtemp[subbands[kk]] = vv_all.get(kk);
            ggtemp[subbands[kk]] = g_all.get(kk);
        }
        
        // compute reference and distorted image information from each subband
        sumnum = 0.0;
        sumden = 0.0;
        for (i = 0; i < subbands.length; i++) {
            sub=subbands[i];
            g=ggtemp[sub];
            vv=vvtemp[sub];
            ss=ssarr[sub];
            lambda = larr[sub]; 
            //cu=cuarr[sub];

            // how many eigenvalues to sum over. default is all.
            // neigvals=lambda.length;
            
            // compute the size of the window used in the distortion channel estimation, and use it to calculate the offset from subband borders
            // we do this to avoid all coefficients that may suffer from boundary
            // effects
            lev=(int)Math.ceil(sub/6.0);
            winsize=(int)Math.pow(2,lev)+1; 
            offsetf=(winsize-1)/2.0;
            offset=(int)Math.ceil(offsetf/M);
            
            
            // select only valid portion of the output.
            gvalid = new double[g.length-2*offset][g[0].length-2*offset];
            for (r = 0; r < gvalid.length; r++) {
            	for (c = 0; c < gvalid[0].length; c++) {
            		gvalid[r][c] = g[r+offset][c+offset];
            	}
            }
            vvvalid = new double[vv.length-2*offset][vv[0].length-2*offset];
            for (r = 0; r < vvvalid.length; r++) {
            	for (c = 0; c < vvvalid[0].length; c++) {
            		vvvalid[r][c] = vv[r+offset][c+offset];
            	}
            }
            ssvalid = new double[ss.length-2*offset][ss[0].length-2*offset];
            for (r = 0; r < ssvalid.length; r++) {
            	for (c = 0; c < ssvalid[0].length; c++) {
            		ssvalid[r][c] = ss[r+offset][c+offset];
            	}
            }
            
            // VIF
            temp1=0; 
            temp2=0;
            for (j=0; j < lambda.length; j++) {
            	for (r = 0; r < gvalid.length; r++) {
            		for (c = 0; c < gvalid[0].length; c++) {
            			// distorted image information for the i'th subband
            			temp1 += log2(1 + gvalid[r][c]*gvalid[r][c]*ssvalid[r][c]*lambda[j]/(vvvalid[r][c]+sigma_nsq));
            			// reference image information
            			temp2 += log2(1 + ssvalid[r][c]*lambda[j]/sigma_nsq);
            		}
            	}
            } // for (j=0; j < lambda.length; j++)
            sumnum += temp1;
            sumden += temp2;
            
        } // for (i = 0; i < subbands[i].length; i++)

        // compuate VIF
        vif=sumnum/sumden;
    	
    	UI.setDataText("Visual information fidelity = " + vif + "\n");
        System.out.println("Visual information fidelity = " + vif);
        return;	
    }
    
    private void refparams_vecgsm(double ssarr[][][], double l_arr[][], /*double cu_arr[][][],*/ Vector<double[][]> org) {

		//This function computes the parameters of the reference image. This is
		// called by vifvec.m.
    	int i;
    	int sub;
    	double y[][];
    	int sizeyy;
    	int sizeyx;
    	int ys,xs;
    	double ytrunc[][];
    	double temp[][];
    	int ii,j,k;
    	double mcu[];
    	double sum;
    	double cu[][];
    	int xindex;
    	int yindex;
    	double ss[][];
    	double sssum[];
    	double sssumyx[][];
    	double d[];

		for (i = 0; i < subbands.length; i++) {
		    sub=subbands[i];
		    y=org.get(sub);
		    
		    sizeyy= (int)Math.floor((double)y.length/(double)M)*M; // crop  to exact multiple size
		    sizeyx= (int)Math.floor((double)y[0].length/(double)M)*M;
		    ytrunc = new double[sizeyy][sizeyx];
		    for (ys = 0; ys < sizeyy; ys++) {
		    	for (xs = 0; xs < sizeyx; xs++) {
		    		ytrunc[ys][xs] = y[ys][xs];
		    	}
		    }
		    
		    // Collect MxM blocks. Rearrange each block into an
		    // M^2 dimensional vector and collect all such vectors.
		    // Collect ALL possible MXM blocks (even those overlapping) from the subband
		    temp= new double[M*M][(ytrunc.length-M+1)*(ytrunc[0].length-M+1)];
		    for (j = 1; j <= M; j++) {
		        for (k=1; k <= M; k++) {
		        	for (xs = j-1; xs <= ytrunc[0].length-1+j-M; xs++) {
		        		for (ys = k-1; ys <= ytrunc.length-1+k-M; ys++) {
		        			temp[(k-1) + (j-1)*M][ys-(k-1) + (xs-(j-1))*(ytrunc.length-M+1)] = ytrunc[ys][xs];
		        		}
		        	}
		        }
		    }
		    
		    // estimate mean and covariance
		    mcu = new double[M*M];
		    for (ii = 0; ii < M*M; ii++) {
		        sum = 0.0;
		        for (j = 0; j < temp[0].length; j++) {
		        	sum += temp[ii][j];
		        }
		        mcu[ii] = sum/temp[0].length;
		    }
		   
		    cu = new double[temp.length][temp.length];
		    Matrix tr = new Matrix(temp.length,temp[0].length);
		    for (ii = 0; ii < temp.length; ii++) {
		    	for (j = 0; j < temp[0].length; j++) {
		    		tr.set(ii,j,(temp[ii][j] - mcu[ii]));
		    	}
		    }
		    Matrix trtrtranspose = tr.times(tr.transpose());
		    for (ii = 0; ii < temp.length; ii++) {
		    	for (j = 0; j < temp.length; j++) {
		    	    cu[ii][j] = trtrtranspose.getArray()[ii][j]/temp[0].length; //  % covariance matrix for U
		    	}
		    }
		    
		    // Collect MxM blocks as above. Use ONLY non-overlapping blocks to
		    // calculate the S field
		    temp= new double[M*M][(1 + (ytrunc.length-1)/M)*(1 + (ytrunc[0].length-1)/M)];
		    for (j = 1; j <= M; j++) {
		        for (k = 1; k <= M; k++) {
		        	for (xs = j-1, xindex = 0; xs <= ytrunc[0].length-1; xs+=M, xindex++) {
		        		for (ys = k-1, yindex = 0; ys <= ytrunc.length-1; ys+=M, yindex++) {
		        			temp[(k-1) + (j-1)*M][yindex + xindex*(1 + (ytrunc.length-1)/M)] = ytrunc[ys][xs];
		        		}
		        	}
		        }
		    }

		    // Calculate the S field
		    Matrix cuMat = new Matrix(cu);
		    Matrix cuinv = cuMat.inverse();
		    ss = (cuinv.times(new Matrix(temp))).getArray();
		    sssum = new double[ss[0].length];
		    for (j = 0; j < ss[0].length; j++) {
		    	sum = 0.0;
		    	for (ii = 0; ii < ss.length; ii++) {
		    		sum += ss[ii][j]*temp[ii][j];
		    	}
		    	sssum[j] = sum/(M*M);
		    }
		    sssumyx = new double[sizeyy/M][sizeyx/M];
		    for (xs = 0; xs < sizeyx/M; xs++) {
			    for (ys = 0; ys < sizeyy/M; ys++) {
			        sssumyx[ys][xs] = sssum[ys + xs*sizeyy/M];	
			    }
		    }
		    
		    // Eigen-decomposition
		    EigenvalueDecomposition dec = new EigenvalueDecomposition(cuMat);
		    d = dec.getRealEigenvalues();
		    for (ii = 0; ii < M*M; ii++) {
		    	l_arr[sub][ii] = d[ii];
		    }
		    
		    // rearrange for output
		    ssarr[sub]=sssumyx;
		    temp=null;
		    //cu_arr[sub]=cu;
		} // for (i = 0; i < subbands.length; i++)
    }


    
    private double[][] filter2SameWithDownSample(double img[][], double win[][], int borderType, int downSampleY, int downSampleX,
    		int startY, int startX, int stopY, int stopX) {
    	int top = win.length/2;
    	int bottom = win.length -top - 1;
    	int left = win[0].length/2;
    	int right = win[0].length - left - 1;
    	double imgpad[][] = copyMakeBorder(img, top, bottom, left, right, borderType, 0.0);
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
		if ((downSampleY == 1) && (downSampleX == 1)) {
		    return result;
		}
		else {
			int scaleYDim = 1 + (stopY - startY - 1)/downSampleY;
			int scaleXDim = 1 + (stopX - startX - 1)/downSampleX;
			double sampledResult[][] = new double[scaleYDim][scaleXDim];
			for (y = 0; y < scaleYDim; y++) {
				for (x = 0; x < scaleXDim; x++) {
					sampledResult[y][x] = result[startY + downSampleY*y][startX + downSampleX*x];
				}
			}
			return sampledResult;
		}
    }
    
    private void vifsub_est_M(Vector<double[][]> g_all, Vector<double[][]> vv_all,
    		Vector<double[][]>org, Vector<double[][]>dist) { 

	    // uses convolution for determining the parameters of the distortion channel
	    // Called by vifvec.m
	
	    double tol = 1e-15; // tolernace for zero variance. Variance below this is set to zero, and zero is set to this value to avoid numerical issues.
	    int i;
	    int sub;
	    double y[][];
	    double yn[][];
	    int lev;
	    int winsize;
	    double win[][];
	    double winnorm[][];
	    double val;
	    int xs,ys;
	    int newsizey;
	    int newsizex;
	    double truncy[][];
	    double truncyn[][];
	    int winstepy;
	    int winstepx;
	    int winstarty;
	    int winstartx;
	    int winstopy;
	    int winstopx;
	    double mean_x[][];
	    double mean_y[][];
	    double yyn[][];
	    double y2[][];
	    double yn2[][];
	    double cov_xy[][];
	    double ss_x[][];
	    double ss_y[][];
	    double g[][];
	    double vv[][];
	
	    for (i=0; i < subbands.length; i++) {
	        sub=subbands[i];
	        y=org.get(sub);
	        yn=dist.get(sub);
	
	        // compute the size of the window used in the distortion channel estimation
	        lev=(int)Math.ceil(sub/6.0);
	        winsize=(int)Math.pow(2,lev)+1; 
	        win = new double[winsize][winsize];
	        winnorm = new double[winsize][winsize];
	        double winsum = winsize*winsize;
	        val = 1.0/(winsize*winsize);
	        for (ys = 0; ys < winsize; ys++) {
	        	for (xs = 0; xs < winsize; xs++) {
	        		win[ys][xs] = 1.0;
	        		winnorm[ys][xs] = val;
	        	}
	        }
	        
	        // force subband size to be multiple of M
	        newsizey=(int)Math.floor((double)y.length/(double)M)*M;
	        newsizex=(int)Math.floor((double)y[0].length/(double)M)*M;
	        
	        truncy = new double[newsizey][newsizex];
	        truncyn = new double[newsizey][newsizex];
	        yyn = new double[newsizey][newsizex];
	        y2 = new double[newsizey][newsizex];
	        yn2 = new double[newsizey][newsizex];
	        for (ys = 0; ys < newsizey; ys++) {
	        	for (xs = 0; xs < newsizex; xs++) {
	        		truncy[ys][xs] = y[ys][xs];
	        		truncyn[ys][xs] = yn[ys][xs];
	        		yyn[ys][xs] = y[ys][xs]*yn[ys][xs];
	        		y2[ys][xs] = y[ys][xs]*y[ys][xs];
	        		yn2[ys][xs] = yn[ys][xs]*yn[ys][xs];
	        	}
	        }
	
	        // Correlation with downsampling. This is faster than downsampling after
	        // computing full correlation.
	        winstepy = M;
	        winstepx = M;
	        winstarty = (int)Math.floor(M/2.0);
	        winstartx = (int)Math.floor(M/2.0);
	        winstopy = truncy.length - (int)Math.ceil(M/2.0) + 1;
	        winstopx = truncy[0].length - (int)Math.ceil(M/2.0) + 1;
	        
	        // mean
	        mean_x = filter2SameWithDownSample(truncy,winnorm,BORDER_REFLECT_101,winstepy, winstepx,
	        		 winstarty, winstartx, winstopy, winstopx);
	        mean_y = filter2SameWithDownSample(truncyn,winnorm,BORDER_REFLECT_101,winstepy, winstepx,
	        		 winstarty, winstartx, winstopy, winstopx);
	        // cov
	        cov_xy = filter2SameWithDownSample(yyn,win,BORDER_REFLECT_101,winstepy, winstepx,
	        		 winstarty, winstartx, winstopy, winstopx);
	        for (ys = 0; ys < cov_xy.length; ys++) {
	        	for (xs = 0; xs < cov_xy[0].length; xs++) {
	        		cov_xy[ys][xs] = cov_xy[ys][xs] - winsum*mean_x[ys][xs]*mean_y[ys][xs];
	        	}
	        }
	       
	        // var
	        // get rid of numerical problems, very small negative numbers, or very
	        // small positive numbers, or other theoretical impossibilities.
	        ss_x = filter2SameWithDownSample(y2,win,BORDER_REFLECT_101,winstepy, winstepx,
	        		 winstarty, winstartx, winstopy, winstopx);
	        for (ys = 0; ys < ss_x.length; ys++) {
	        	for (xs = 0; xs < ss_x[0].length; xs++) {
	        		ss_x[ys][xs] = Math.max(0.0,ss_x[ys][xs] - winsum*mean_x[ys][xs]*mean_x[ys][xs]);
	        	}
	        }
	        
	        ss_y = filter2SameWithDownSample(yn2,win,BORDER_REFLECT_101,winstepy, winstepx,
	        		 winstarty, winstartx, winstopy, winstopx);
	        for (ys = 0; ys < ss_y.length; ys++) {
	        	for (xs = 0; xs < ss_y[0].length; xs++) {
	        		ss_y[ys][xs] = Math.max(0.0,ss_y[ys][xs] - winsum*mean_y[ys][xs]*mean_y[ys][xs]);
	        	}
	        }
	       
	        // Regression 
	        g = new double[cov_xy.length][cov_xy[0].length];
	        for (ys = 0; ys < cov_xy.length; ys++) {
	        	for (xs = 0; xs < cov_xy[0].length; xs++) {
	        		g[ys][xs] = cov_xy[ys][xs]/(ss_x[ys][xs] + tol);
	        	}
	        }
	       
	        
	        // Variance of error in regression
	        vv = new double[ss_y.length][ss_y[0].length];
	        for (ys = 0; ys < vv.length; ys++) {
	        	for (xs = 0; xs < vv[0].length; xs++) {
	        		vv[ys][xs] = (ss_y[ys][xs] - g[ys][xs]*cov_xy[ys][xs])/winsum;
	        	}
	        }
	        
	        // get rid of numerical problems, very small negative numbers, or very
	        // small positive numbers, or other theoretical impossibilities.
	        for (ys = 0; ys < g.length; ys++) {
	        	for (xs = 0; xs < g[0].length; xs++) {
	        		if (ss_x[ys][xs] < tol) {
	        			g[ys][xs] = 0;
	        			vv[ys][xs] = ss_y[ys][xs];
	        			ss_x[ys][xs] = 0;
	        		}
	        		
	        		if (ss_y[ys][xs] < tol) {
	        			g[ys][xs] = 0.0;
	        			vv[ys][xs] = 0.0;
	        		}
	        		
	        		// constrain g to be non-negative. 
	        		if (g[ys][xs] < 0) {
	        			vv[ys][xs] = ss_y[ys][xs];
	        			g[ys][xs] = 0.0;
	        		}
	        		
	        		// take care of numerical errors, vv could be very small negative
	        		if (vv[ys][xs] < tol) {
	        			vv[ys][xs] = tol;
	        		}
	        	}
	        }
	        
	        g_all.add(g);
	        vv_all.add(vv);
	        
	    } // for (i=0; i < subbands.length; i++)
    }
    
    //function y=nqm(O,I,VA,N)
    private void nqm() {
    		// NQM   Nonlinear Weighted Signal to noise ratio for additive noise.
    		//      RATIO = NQM(MOD_RES,RES,ANGLE, DIM) computes the nonlinear weighted signal to
    		//      noise ratio of the restored image MOD_RES with respect to the model
    		//      restored image MOD_RES and returns the result in dB.
    		//      ANGLE is the viewing angle. DIM is the [yDim xDim] dimension of the image 
    		
    		//      Note that NQM is a measure of additive noise only. The model restored 
    		//      image is assumed to have the same freq. distortion as the restored image.  
    		
    		//      Function calls: CTF, CMASKN, GTHRESH
    		
    		//      Ref: N. Damera-Venkata, T. Kite, W. Geisler, B. Evans and A. Bovik, "Image
    		//      Quality Assessment Based on a Degradation Model", IEEE Trans. on Image 
    		//      Processing, Vol. 9, No. 4, Apr. 2000  
    		
    		//      Ref: E. Peli, "Contrast in Complex Images", Journal of the Optical
    		//      Society of America A, Vol. 7, No. 10, Oct. 1990
    		
    		//      See also CSFMOD, CSF, PSNR, WSNR
    		
    		//      Niranjan Damera-Venkata March 1998 
    		 

    		//[x,y]=size(O);

    		//[xplane,yplane]=meshgrid(-x/2:x/2-1);
    		//plane=(xplane+i*yplane);
    		//r=abs(plane);
    	    int i,j;
    	    int index;
    		double xplane[][] = new double[yDim][xDim];
    		for (i = 0; i < yDim; i++) {
    		    for (j = 0; j < xDim; j++) {
    		    	xplane[i][j] = -xDim/2.0 + j;
    		    }
    		}
    		double yplane[][] = new double[yDim][xDim];
    		for (j = 0; j < xDim; j++) {
    			for (i = 0; i < yDim; i++) {
    				yplane[i][j] = -yDim/2.0 + i;
    			}
    		}
    		double r[][] = new double[yDim][xDim];
    		for (i = 0; i < yDim; i++) {
    			for (j = 0; j < xDim; j++) {
    				r[i][j] = Math.sqrt(xplane[i][j]*xplane[i][j] + yplane[i][j]*yplane[i][j]);
    			}
    		}
    		
    		double FO[] = new double[length];
    		double FOImag[] = new double[length];
    		double FI[] = new double[length];
    		double FIImag[] = new double[length];
    		for (i = 0; i < length; i++) {
    			if (isColor) {
    				FO[i] = 0.2989 * referenceRedBuffer[i] + 0.5870 * referenceGreenBuffer[i] + 0.1140 * referenceBlueBuffer[i];
	    			FI[i] = 0.2989 * testRedBuffer[i] + 0.5870 * testGreenBuffer[i] + 0.1140 * testBlueBuffer[i];	
    			}
    			else {
	    			FO[i] = referenceBuffer[i];
	    			FI[i] = testBuffer[i];
    			}
    		}
    		FFTUtility fft = new FFTUtility(FO, FOImag, yDim, xDim, 1, -1, FFTUtility.FFT);
    		fft.setShowProgress(false);
    		fft.run();
    		fft.finalize();
    		fft = null;
    		fft = new FFTUtility(FO, FOImag, 1, yDim, xDim, -1, FFTUtility.FFT);
    		fft.setShowProgress(false);
    		fft.run();
    		fft.finalize();
    		fft = null;
    	 
    		fft = new FFTUtility(FI, FIImag, yDim, xDim, 1, -1, FFTUtility.FFT);
    		fft.setShowProgress(false);
    		fft.run();
    		fft.finalize();
    		fft = null;
    		fft = new FFTUtility(FI, FIImag, 1, yDim, xDim, -1, FFTUtility.FFT);
    		fft.setShowProgress(false);
    		fft.run();
    		fft.finalize();
    		fft = null;
    		
    		//%%%%%%%%%%% Decompose image with cosine log filter bank %%%%%%%%%%%%%%%%
    		double G_0[][] = new double[yDim][xDim];
    		double G_1[][] = new double[yDim][xDim];
    		double G_2[][] = new double[yDim][xDim];
    		double G_3[][] = new double[yDim][xDim];
    		double G_4[][] = new double[yDim][xDim];
    		double G_5[][] = new double[yDim][xDim];
    		for (i = 0; i < yDim; i++) {
    		    for (j = 0; j < xDim; j++) {
    		        if (r[i][j] <= 2.0) {
    		        	G_0[i][j]=0.5*(1+Math.cos(Math.PI*log2((r[i][j]+2) )-Math.PI));	
    		        }
    		        else {
    		        	G_0[i][j]=0.5*(1+Math.cos(Math.PI*log2(4.0 )-Math.PI));		
    		        }
    		        if ((r[i][j] >= 1.0) && (r[i][j] <= 4.0)) {
    		        	G_1[i][j]=0.5*(1+Math.cos(Math.PI*log2(r[i][j] )-Math.PI));	
    		        }
    		        else {
    		        	G_1[i][j]=0.5*(1+Math.cos(Math.PI*log2(4.0 )-Math.PI));	
    		        }
    		        if ((r[i][j] >= 2.0) && (r[i][j] <= 8.0)) {
    		        	G_2[i][i]=0.5*(1+Math.cos(Math.PI*log2(r[i][j])));	
    		        }
    		        else {
    		        	G_2[i][i]=0.5*(1+Math.cos(Math.PI*log2(5.0)));	
    		        }
    		        if ((r[i][j] >= 4.0) && (r[i][j] <= 16.0)) {
    		        	G_3[i][j]=0.5*(1+Math.cos(Math.PI*log2(r[i][j])-Math.PI));	
    		        }
    		        else {
    		        	G_3[i][j]=0.5*(1+Math.cos(Math.PI*log2(4.0)-Math.PI));
    		        }
    		        if ((r[i][j] >= 8.0) && (r[i][j] <= 32.0)) {
    		        	G_4[i][j]=0.5*(1+Math.cos(Math.PI*log2(r[i][j])));	
    		        }
    		        else {
    		        	G_4[i][j]=0.5*(1+Math.cos(Math.PI*log2(5.0)));	
    		        }
    		        if ((r[i][j] >= 16.0) && (r[i][j] <= 64.0)) {
    		        	G_5[i][j]=0.5*(1+Math.cos(Math.PI*log2(r[i][j])-Math.PI));	
    		        }
    		        else {
    		        	G_5[i][j]=0.5*(1+Math.cos(Math.PI*log2(4.0)-Math.PI));
    		        }
    		    }
    		}
    		
    		double GS_0[][] = fftshift(G_0);
    		for (i = 0; i < yDim; i++) {
		        G_0[i] = null;	
		    }
    		G_0 = null;
    		double GS_1[][] = fftshift(G_1);
    		for (i = 0; i < yDim; i++) {
		        G_1[i] = null;	
		    }
    		G_1 = null;
    		double GS_2[][] = fftshift(G_2);
    		for (i = 0; i < yDim; i++) {
		        G_2[i] = null;	
		    }
    		G_2 = null;
    		double GS_3[][] = fftshift(G_3);
    		for (i = 0; i < yDim; i++) {
		        G_3[i] = null;	
		    }
    		G_3 = null;
    		double GS_4[][] = fftshift(G_4);
    		for (i = 0; i < yDim; i++) {
		        G_4[i] = null;	
		    }
    		G_4 = null;
    		double GS_5[][] = fftshift(G_5);
    		for (i = 0; i < yDim; i++) {
		        G_5[i] = null;	
		    }
    		G_5 = null;
    		
    		double l_0[] = new double[length];
    		double l_0Imag[] = new double[length];
    		double li_0[] = new double[length];
    		double li_0Imag[] = new double[length];
    		for (i = 0; i < yDim; i++) {
    			for (j = 0; j < xDim; j++) {
    				index = j + i*xDim;
    				l_0[index] = GS_0[i][j]*FO[index];
    				l_0Imag[index] = GS_0[i][j]*FOImag[index];
    				li_0[index] = GS_0[i][j]*FI[index];
    				li_0Imag[index] = GS_0[i][j]*FIImag[index];
    			}
    		}
    		for (i = 0; i < yDim; i++) {
		        GS_0[i] = null;	
		    }
    		GS_0 = null;
    		
    		FFTUtility ifft = new FFTUtility(l_0, l_0Imag, yDim, xDim, 1, 1, FFTUtility.FFT);
    		ifft.setShowProgress(false);
    		ifft.run();
    		ifft.finalize();
    		ifft = null;
    		ifft = new FFTUtility(l_0, l_0Imag, 1, yDim, xDim, 1, FFTUtility.FFT);
    		ifft.setShowProgress(false);
    		ifft.run();
    		ifft.finalize();
    		ifft = null;
    		l_0Imag = null;
    		
    		ifft = new FFTUtility(li_0, li_0Imag, yDim, xDim, 1, 1, FFTUtility.FFT);
    		ifft.setShowProgress(false);
    		ifft.run();
    		ifft.finalize();
    		ifft = null;
    		ifft = new FFTUtility(li_0, li_0Imag, 1, yDim, xDim, 1, FFTUtility.FFT);
    		ifft.setShowProgress(false);
    		ifft.run();
    		ifft.finalize();
    		ifft = null;
    		li_0Imag = null;
    		
    		double a_1[] = new double[length];
    		double a_1Imag[] = new double[length];
    		double ai_1[] = new double[length];
    		double ai_1Imag[] = new double[length];
    		for (i = 0; i < yDim; i++) {
    			for (j = 0; j < xDim; j++) {
    				index = j + i*xDim;
    				a_1[index] = GS_1[i][j]*FO[index];
    				a_1Imag[index] = GS_1[i][j]*FOImag[index];
    				ai_1[index] = GS_1[i][j]*FI[index];
    				ai_1Imag[index] = GS_1[i][j]*FIImag[index];
    			}
    		}
    		for (i = 0; i < yDim; i++) {
		        GS_1[i] = null;	
		    }
    		GS_1 = null;
    		
    		ifft = new FFTUtility(a_1, a_1Imag, yDim, xDim, 1, 1, FFTUtility.FFT);
    		ifft.setShowProgress(false);
    		ifft.run();
    		ifft.finalize();
    		ifft = null;
    		ifft = new FFTUtility(a_1, a_1Imag, 1, yDim, xDim, 1, FFTUtility.FFT);
    		ifft.setShowProgress(false);
    		ifft.run();
    		ifft.finalize();
    		ifft = null;
    		a_1Imag = null;
    		
    		ifft = new FFTUtility(ai_1, ai_1Imag, yDim, xDim, 1, 1, FFTUtility.FFT);
    		ifft.setShowProgress(false);
    		ifft.run();
    		ifft.finalize();
    		ifft = null;
    		ifft = new FFTUtility(ai_1, ai_1Imag, 1, yDim, xDim, 1, FFTUtility.FFT);
    		ifft.setShowProgress(false);
    		ifft.run();
    		ifft.finalize();
    		ifft = null;
    		ai_1Imag = null;
    		
    		double a_2[] = new double[length];
    		double a_2Imag[] = new double[length];
    		double ai_2[] = new double[length];
    		double ai_2Imag[] = new double[length];
    		for (i = 0; i < yDim; i++) {
    			for (j = 0; j < xDim; j++) {
    				index = j + i*xDim;
    				a_2[index] = GS_2[i][j]*FO[index];
    				a_2Imag[index] = GS_2[i][j]*FOImag[index];
    				ai_2[index] = GS_2[i][j]*FI[index];
    				ai_2Imag[index] = GS_2[i][j]*FIImag[index];
    			}
    		}
    		for (i = 0; i < yDim; i++) {
		        GS_2[i] = null;	
		    }
    		GS_2 = null;
    		
    		ifft = new FFTUtility(a_2, a_2Imag, yDim, xDim, 1, 1, FFTUtility.FFT);
    		ifft.setShowProgress(false);
    		ifft.run();
    		ifft.finalize();
    		ifft = null;
    		ifft = new FFTUtility(a_2, a_2Imag, 1, yDim, xDim, 1, FFTUtility.FFT);
    		ifft.setShowProgress(false);
    		ifft.run();
    		ifft.finalize();
    		ifft = null;
    		a_2Imag = null;
    		
    		ifft = new FFTUtility(ai_2, ai_2Imag, yDim, xDim, 1, 1, FFTUtility.FFT);
    		ifft.setShowProgress(false);
    		ifft.run();
    		ifft.finalize();
    		ifft = null;
    		ifft = new FFTUtility(ai_2, ai_2Imag, 1, yDim, xDim, 1, FFTUtility.FFT);
    		ifft.setShowProgress(false);
    		ifft.run();
    		ifft.finalize();
    		ifft = null;
    		ai_2Imag = null;
    		
    		double a_3[] = new double[length];
    		double a_3Imag[] = new double[length];
    		double ai_3[] = new double[length];
    		double ai_3Imag[] = new double[length];
    		for (i = 0; i < yDim; i++) {
    			for (j = 0; j < xDim; j++) {
    				index = j + i*xDim;
    				a_3[index] = GS_3[i][j]*FO[index];
    				a_3Imag[index] = GS_3[i][j]*FOImag[index];
    				ai_3[index] = GS_3[i][j]*FI[index];
    				ai_3Imag[index] = GS_3[i][j]*FIImag[index];
    			}
    		}
    		for (i = 0; i < yDim; i++) {
		        GS_3[i] = null;	
		    }
    		GS_3 = null;
    		
    		ifft = new FFTUtility(a_3, a_3Imag, yDim, xDim, 1, 1, FFTUtility.FFT);
    		ifft.setShowProgress(false);
    		ifft.run();
    		ifft.finalize();
    		ifft = null;
    		ifft = new FFTUtility(a_3, a_3Imag, 1, yDim, xDim, 1, FFTUtility.FFT);
    		ifft.setShowProgress(false);
    		ifft.run();
    		ifft.finalize();
    		ifft = null;
    		a_3Imag = null;
    		
    		ifft = new FFTUtility(ai_3, ai_3Imag, yDim, xDim, 1, 1, FFTUtility.FFT);
    		ifft.setShowProgress(false);
    		ifft.run();
    		ifft.finalize();
    		ifft = null;
    		ifft = new FFTUtility(ai_3, ai_3Imag, 1, yDim, xDim, 1, FFTUtility.FFT);
    		ifft.setShowProgress(false);
    		ifft.run();
    		ifft.finalize();
    		ifft = null;
    		ai_3Imag = null;
    		
    		double a_4[] = new double[length];
    		double a_4Imag[] = new double[length];
    		double ai_4[] = new double[length];
    		double ai_4Imag[] = new double[length];
    		for (i = 0; i < yDim; i++) {
    			for (j = 0; j < xDim; j++) {
    				index = j + i*xDim;
    				a_4[index] = GS_4[i][j]*FO[index];
    				a_4Imag[index] = GS_4[i][j]*FOImag[index];
    				ai_4[index] = GS_4[i][j]*FI[index];
    				ai_4Imag[index] = GS_4[i][j]*FIImag[index];
    			}
    		}
    		for (i = 0; i < yDim; i++) {
		        GS_4[i] = null;	
		    }
    		GS_4 = null;
    		
    		ifft = new FFTUtility(a_4, a_4Imag, yDim, xDim, 1, 1, FFTUtility.FFT);
    		ifft.setShowProgress(false);
    		ifft.run();
    		ifft.finalize();
    		ifft = null;
    		ifft = new FFTUtility(a_4, a_4Imag, 1, yDim, xDim, 1, FFTUtility.FFT);
    		ifft.setShowProgress(false);
    		ifft.run();
    		ifft.finalize();
    		ifft = null;
    		a_4Imag = null;
    		
    		ifft = new FFTUtility(ai_4, ai_4Imag, yDim, xDim, 1, 1, FFTUtility.FFT);
    		ifft.setShowProgress(false);
    		ifft.run();
    		ifft.finalize();
    		ifft = null;
    		ifft = new FFTUtility(ai_4, ai_4Imag, 1, yDim, xDim, 1, FFTUtility.FFT);
    		ifft.setShowProgress(false);
    		ifft.run();
    		ifft.finalize();
    		ifft = null;
    		ai_4Imag = null;
    		
    		double a_5[] = new double[length];
    		double a_5Imag[] = new double[length];
    		double ai_5[] = new double[length];
    		double ai_5Imag[] = new double[length];
    		for (i = 0; i < yDim; i++) {
    			for (j = 0; j < xDim; j++) {
    				index = j + i*xDim;
    				a_5[index] = GS_5[i][j]*FO[index];
    				a_5Imag[index] = GS_5[i][j]*FOImag[index];
    				ai_5[index] = GS_5[i][j]*FI[index];
    				ai_5Imag[index] = GS_5[i][j]*FIImag[index];
    			}
    		}
    		for (i = 0; i < yDim; i++) {
		        GS_5[i] = null;	
		    }
    		GS_5 = null;
    		FO = null;
    		FOImag = null;
    		FI = null;
    		FIImag = null;
    		
    		ifft = new FFTUtility(a_5, a_5Imag, yDim, xDim, 1, 1, FFTUtility.FFT);
    		ifft.setShowProgress(false);
    		ifft.run();
    		ifft.finalize();
    		ifft = null;
    		ifft = new FFTUtility(a_5, a_5Imag, 1, yDim, xDim, 1, FFTUtility.FFT);
    		ifft.setShowProgress(false);
    		ifft.run();
    		ifft.finalize();
    		ifft = null;
    		a_5Imag = null;
    		
    		ifft = new FFTUtility(ai_5, ai_5Imag, yDim, xDim, 1, 1, FFTUtility.FFT);
    		ifft.setShowProgress(false);
    		ifft.run();
    		ifft.finalize();
    		ifft = null;
    		ifft = new FFTUtility(ai_5, ai_5Imag, 1, yDim, xDim, 1, FFTUtility.FFT);
    		ifft.setShowProgress(false);
    		ifft.run();
    		ifft.finalize();
    		ifft = null;
    		ai_5Imag = null;
    		
    		// %%%%%%%%%%%%%%%%%%%%%Compute contrast images%%%%%%%%%%%%%%%%%%%
    		double c1[] = new double[length];
    		double c2[] = new double[length];
    		double c3[] = new double[length];
    		double c4[] = new double[length];
    		double c5[] = new double[length];
    		double ci1[] = new double[length];
    		double ci2[] = new double[length];
    		double ci3[] = new double[length];
    		double ci4[] = new double[length];
    		double ci5[] = new double[length];
    		for (i = 0; i < length; i++) {
	    		c1[i]=(a_1[i]/l_0[i]);
	    		c2[i]=(a_2[i]/(l_0[i]+a_1[i]));
	    		c3[i]=(a_3[i]/(l_0[i]+a_1[i]+a_2[i]));
	    		c4[i]=(a_4[i]/(l_0[i]+a_1[i]+a_2[i]+a_3[i]));
	    		c5[i]=(a_5[i]/(l_0[i]+a_1[i]+a_2[i]+a_3[i]+a_4[i]));
	
	    		ci1[i]=(ai_1[i]/li_0[i]);
	    		ci2[i]=(ai_2[i]/(li_0[i]+ai_1[i]));
	    		ci3[i]=(ai_3[i]/(li_0[i]+ai_1[i]+ai_2[i]));
	    		ci4[i]=(ai_4[i]/(li_0[i]+ai_1[i]+ai_2[i]+ai_3[i]));
	    		ci5[i]=(ai_5[i]/(li_0[i]+ai_1[i]+ai_2[i]+ai_3[i]+ai_4[i]));
    		}
    		
    		// %%%%%%%%%%%%%%%%%%%%%%Detection Thresholds%%%%%%%%%%%%%%%%%%%%%%

    		double d1=ctf(2.0/VA);
    		double d2=ctf(4.0/VA);
    		double d3=ctf(8.0/VA);
    		double d4=ctf(16.0/VA);
    		double d5=ctf(32.0/VA);
    		
    		// %%%%%%%%%%%%%%%%%%Account for suprathrshold effects (See Bradley and Ozhawa)%%%%
    		ai_1 = cmaskn(c1,ci1,a_1,ai_1,1.0);
    		ai_2 = cmaskn(c2,ci2,a_2,ai_2,2.0);
    		ai_3 = cmaskn(c3,ci3,a_3,ai_3,3.0);
    		ai_4 = cmaskn(c4,ci4,a_4,ai_4,4.0);
    		ai_5 = cmaskn(c5,ci5,a_5,ai_5,5.0);
    		
    		//%%%%%%%%%%Apply detection thresholds%%%%%%%%%%%%%%%%%%%%%%
    		double A_1[] = gthresh(c1,d1,a_1);
    		double AI_1[] = gthresh(ci1,d1,ai_1);
    		double A_2[] = gthresh(c2,d2,a_2);
    		double AI_2[] = gthresh(ci2,d2,ai_2);
    		double A_3[] = gthresh(c3,d3,a_3);
    		double AI_3[] = gthresh(ci3,d3,ai_3);
    		double A_4[] = gthresh(c4,d4,a_4);
    		double AI_4[] = gthresh(ci4,d4,ai_4);
    		double A_5[] = gthresh(c5,d5,a_5);
    		double AI_5[] = gthresh(ci5,d5,ai_5);
    		
    		// %%%%%%%%reconstruct images%%%%%%%%%%%%%%%%%%%
    		double y1[] = new double[length];
    		double y2[] = new double[length];
    		for (i = 0; i < length; i++) {
    		    y1[i] = (A_1[i]+A_2[i]+A_3[i]+A_4[i]+A_5[i]);
    		    y2[i] = (AI_1[i]+AI_2[i]+AI_3[i]+AI_4[i]+AI_5[i]);
    		}

    		// %%%%%%%%%%%%%%%compute SNR%%%%%%%%%%%%%%%%%%%%
    		double np = 0.0;
    		double sp = 0.0;
    		double diff;
    		for (i = 0; i < length; i++) {
    		    diff = y1[i]-y2[i];
    		    np += (diff*diff);
    		    sp += (y1[i]*y1[i]);
    		}

    		nqm_value=10*Math.log10(sp/np);
    		UI.setDataText("NQM = " + nqm_value + "\n");
    		System.out.println("NQM = " + nqm_value);
    }
    
    // Global detection thresholds
    // Niranjan Damera-Venkata
    // March 1998

    private double[] gthresh(double x[], double T,double z[]) {

        int i;
        double y[] = new double[length];
        for (i = 0; i < length; i++) {
        	if (Math.abs(x[i]) < T) {
        		y[i] = 0.0;
        	}
        	else {
        		y[i] = z[i];
        	}
        }
        return y;
    }
    
    // Bradley and Ozhawa masking functions
    // Niranjan Damera-Venkata
    // March 1998

    private double[] cmaskn(double c[], double ci[],double a[], double ai[], double i) {
        int j;
        double ci2[] = new double[length];
	    for (j = 0; j < length; j++) {
	    	if (Math.abs(ci[j]) > 1.0) {
	    		ci2[j] = 1.0;
	    	}
	    	else {
	    		ci2[j] = ci[j];
	    	}
	    }
	
	    double ct=ctf(i);
	    double T[] = new double[length];
	    for (j = 0; j < length; j++) {
	    	T[j] = ct*(.86*((c[j]/ct)-1)+.3);
	    }
	    
	    double y[] = new double[length];
	    for (j = 0; j < length; j++) {
	    	if ((Math.abs(ci2[j]-c[j])-T[j]) < 0.0) {
	    		y[j] = a[j];
	    	}
	    	else {
	    		y[j] = ai[j];
	    	}
	    }
	
	    return y;
	 }


    
    // Bandpass contrast threshold function
    // You may want to modify the default function based on your imaging system
    // measurements
    // Niranjan Damera-Venkata
    // March 1998

    private double ctf(double f_r) {

       double y=1./(200*(2.6*(0.0192+0.114*(f_r))*Math.exp(-Math.pow((0.114*f_r),1.1))));
       return y;
    }

    
    private double[][] fftshift(double in[][]) {
    	int yDim = in.length;
    	int xDim = in[0].length;
		double out[][] = new double[yDim][xDim];
		int highestxquad1Index;
		int highestyquad1Index;
		int quad1height;
		int quad1width;
		int quad2height;
		int quad2width;
		int quad3height;
		int quad3width;
		int quad4height;
		int quad4width;
		int y,x;
		if ((yDim %2) == 1) {
			//yodd = true;
			highestyquad1Index = (yDim - 1)/2;
		}
		else {
			//yodd = false;
			highestyquad1Index = yDim/2 - 1;
		}
		if ((xDim % 2) == 1) {
			//xodd = true;
			highestxquad1Index = (xDim - 1)/2;
		}
		else {
			//xodd = false;
			highestxquad1Index = xDim/2 - 1;
		}
		quad1width = highestxquad1Index + 1;
		quad1height = highestyquad1Index + 1;
		quad2width = xDim - quad1width;
		quad2height = quad1height;
		quad3width = quad2width;
		quad3height = yDim - quad1height;
		quad4width = quad1width;
		quad4height = quad3height;
		double quad1[][] = new double[quad1height][quad1width];
		double quad2[][] = new double[quad2height][quad2width];
		double quad3[][] = new double[quad3height][quad3width];
		double quad4[][] = new double[quad4height][quad4width];
		for (y = 0; y <= highestyquad1Index; y++) {
			for (x = 0; x <= highestxquad1Index; x++) {
				quad1[y][x] = in[y][x];
			}
		}
		for (y = 0; y <= highestyquad1Index; y++) {
			for (x = highestxquad1Index+1; x < xDim; x++) {
				quad2[y][x - highestxquad1Index-1] = in[y][x];
			}
		}
		for (y = highestyquad1Index+1; y < yDim; y++) {
			for (x = highestxquad1Index+1; x < xDim; x++) {
				quad3[y - highestyquad1Index - 1][x - highestxquad1Index - 1] = in[y][x];
			}
		}
		for (y = highestyquad1Index+1; y < yDim; y++) {
			for (x = 0; x <= highestxquad1Index; x++) {
				quad4[y - highestyquad1Index - 1][x] = in[y][x];
			}
		}
		
		// Move third quadrant to first
		for (y = 0; y < quad3height; y++) {
			for (x = 0; x < quad3width; x++) {
				out[y][x] = quad3[y][x];
			}
		}
		// Move fourth quadrant to second
		for (y = 0; y < quad4height; y++) {
			for (x = 0; x < quad4width; x++) {
				out[y][x + quad3width] = quad4[y][x];
			}
		}
		// Move first quadrant to third
		for (y = 0; y < quad1height; y++) {
			for (x = 0; x < quad1width; x++) {
				out[y+quad3height][x + quad3width] = quad1[y][x];
			}
		}
		// Move second quadrant to fourth
		for (y = 0; y < quad2height; y++) {
			for (x = 0; x < quad2width; x++) {
				out[y+quad3height][x] = quad2[y][x];
			}
		}
		return out;
	}
    
    private void vsnr() {
    	// VSNR Visual signal-to-noise ratio for digital images.
    	// returns the visual signal-to-noise 
    	// ratio for the distorted image DST_IMG relative to the original
    	//  image SRC_IMG.  The result is in dB in the range 0 - Inf. 

    	int i;
    	double var;
    	double d = 1.0;
    	int pixel_vals;
    	if (vsnr_data_filter_gains == null) {
    	    vsnr_data_filter_gains = new int[vsnr_data_num_levels];	
    	    vsnr_data_filter_gains[0] = 2;
    	    for (i = 1; i < vsnr_data_num_levels; i++) {
    	        vsnr_data_filter_gains[i] = 2 * vsnr_data_filter_gains[i-1];	
    	    }
    	}
    	
    	// spatial frequencies for DWT levels
    	vsnr_data_fs = new double[vsnr_data_num_levels];
    	var = vsnr_data_r*vsnr_data_v*Math.tan(Math.PI/180);
    	vsnr_data_fs[0] = var/2.0;
    	for (i = 1; i < vsnr_data_num_levels; i++) {
    		vsnr_data_fs[i] = vsnr_data_fs[i-1]/2.0;
    	}
    	
    	// initialize the pixel-value-to-luminance lookup table
    	vsnr_data_pix2lum_table = new double[256];
    	for (pixel_vals = 0; pixel_vals <= 255; pixel_vals++) {
    		var = vsnr_data_b + vsnr_data_k*pixel_vals;	
    		vsnr_data_pix2lum_table[pixel_vals] = Math.pow(var,vsnr_data_g);
    	}
    	
    	// analyze the source image
    	analyze_src_img();
    	
    	// measure the actual CSNRs of the distorted image
    	double csnrs_act[] = analyze_dst_img();

    	if ((csnrs_act.length == 1) && (csnrs_act[0] == -1)) { // subthreshold distortions
    	  vsnr_value = Double.POSITIVE_INFINITY;
    	}
    	else {
    	  // if csnrs_act is a vector, this denotes the distortions 
    	  // are suprathreshold (visible)
    	  if (csnrs_act.length == vsnr_data_num_levels) {

    	    // compute the assumed best CSNRs for the given Ce
    	    double csnrs_str = find_best_csnrs(vsnr_data_Ce);

    	    // convert the CSNRs to contrasts
    	    double Ces_act;
    	    double Ces_str;
    	    double diff;
    	    double totalSquared = 0.0;
    	    for (i = 0; i < vsnr_data_num_levels; i++) {
    	        Ces_act = vsnr_data_Cis[i] / csnrs_act[i];
    	        Ces_str = vsnr_data_Cis[i] / csnrs_str;  
    	        diff = Ces_str - Ces_act;
    	        totalSquared += (diff * diff);
    	    }

    	    // compute the distances
    	    double d_gp = Math.sqrt(totalSquared);
    	    double d_pc = vsnr_data_Ce;
    	    d = alpha*d_pc + (1 - alpha)*d_gp/Math.sqrt(2);
    	    
    	  // brightness-only difference  
    	  }
    	  else if (csnrs_act.length == 1)  {  
    	    d = csnrs_act[0];
    	  }
    	  else {
    		  System.err.println("Impossible value for csnrs_act.lenth = " + csnrs_act.length);
    		  System.exit(0);
    	  }
    	  
    	  // compute the VSNR
    	  vsnr_value = 20*Math.log10(vsnr_data_Ci / d);
    	}
    	UI.setDataText("VSNR = " + vsnr_value + "\n");
		System.out.println("VSNR = " + vsnr_value);


    }
    
    private double find_best_csnrs(double Ce) {
            int i;
    		double save_Ce = Ce;
    		double deltaCe = 0.01 * Ce;
    		double v_idx_min = 0; 
    		double v_idx_max = 1;
    		double csnrs = 0.0;

    		int num_iters = 0;
    		while (num_iters < 32) {
    		  double v_idx = 0.5 * (v_idx_min + v_idx_max);
    		  if (num_iters > 0) {
    		    Ce = save_Ce;
    		  }

    		  // compute the best CSNRs for the given visibility index
    		  csnrs = best_csnrs(v_idx);
    		  // compute the corresponding distortion contrast
    		  double totalSquared = 0.0;
    		  double div;
    		  for (i = 0; i < vsnr_data_Cis.length; i++) {
    	          div = vsnr_data_Cis[i]/csnrs;
    	          totalSquared += (div * div);
    		  }
    		  double hat_Ce = Math.sqrt(totalSquared);

    		  if (v_idx_min == v_idx_max) {
    		    // punt
    		    break;
    		  }

    		  double diffCe = hat_Ce - Ce;
    		  if (Math.abs(diffCe) < deltaCe) {
    		    // close enough
    		    break;
    		  }
    		  else if (diffCe < 0) {
    		    // Ce too low
    		    v_idx_min = v_idx;
    		  }
    		  else {
    		    // Ce too high
    		    v_idx_max = v_idx;
    		  }
    		}

    		return csnrs;
    }

    
    private double[] analyze_dst_img() {
    	int i, index, x, y, s, or;
    	double c;
    	double mean;
    	double refBuffer[] = null;
    	double tBuffer[] = null;
    	double res[] = null;
    	if (isColor) {
    		refBuffer = new double[length];
    		tBuffer = new double[length];
    		for (i = 0; i < length; i++) {
    			refBuffer[i] = 0.2989 * referenceRedBuffer[i] + 0.5870 * referenceGreenBuffer[i] + 0.1140 * referenceBlueBuffer[i];
    			tBuffer[i] = 0.2989 * testRedBuffer[i] + 0.5879 * testGreenBuffer[i] + 0.1140 * testBlueBuffer[i];
    		}
    	}
    	else {
    		refBuffer = referenceBuffer;
    		tBuffer = testBuffer;
    	}
    	
    	// compute the error image and its total RMS contrast
    	double err_img[] = new double[length];
    	for (i = 0; i < length; i++) {
    	    err_img[i] = Math.round(tBuffer[i]	- refBuffer[i] + vsnr_data_mX);
    	    if (err_img[i] > 255.0) {
    	    	err_img[i] = 255.0;
    	    }
    	    if (err_img[i] < 0.0) {
    	    	err_img[i] = 0.0;
    	    }
    	}
    	
    	double err_img_lum[] = new double[length];
    	double total = 0.0;
    	for (i = 0; i < length; i++) {
    	    err_img_lum[i] = vsnr_data_pix2lum_table[(int)err_img[i]];
    	    total += err_img_lum[i];
    	}
    	double mean_err_img_lum = total/length;
    	
    	total = 0.0;
    	double diff;
    	for (i = 0; i < length; i++) {
    		diff = err_img_lum[i] - mean_err_img_lum;
    		total += (diff * diff);
    	}
    	double std = Math.sqrt(total/length);
    	vsnr_data_Ce = std / vsnr_data_mL;
    	
    	// if the distortion contrast is suprathreshold
    	// (i.e., if the distortions are visible)...
    	if (vsnr_data_Ce > vsnr_data_CTe) {
    	  // compute the distortion contrast at each scale
          double LL0[][] = new double[yDim][xDim];
          for (y = 0; y < yDim; y++) {
        	  for (x = 0; x < xDim; x++) {
        		  index = x + y * xDim;
        		  LL0[y][x] = err_img[index];
        	  }
          }
    	  double err_bands[][][][] = imdwt(LL0, vsnr_data_num_levels);
    	  double vsnr_data_Ces[] = new double[vsnr_data_num_levels];
    	  for (s = 0; s < vsnr_data_num_levels; s++) {
              for (or = 0; or < 3; or++) {
              // estimate the contrast at scale s, orient o
              	// Standard deviation normalized by N rather than N-1
              	total = 0.0;
                  for (y = 0; y < err_bands[s][or].length; y++) {
                  	for (x = 0; x < err_bands[s][or][0].length; x++) {
                  	    total += err_bands[s][or][y][x];
                  	} // for (x = 0; x < err_bands[s][or][0].length; x++)
                  } // for (y = 0; y < err_bands[s][or].length; y++) 
                  mean = total/(err_bands[s][or].length * err_bands[s][or][0].length);
                  total = 0.0;
                  for (y = 0; y < err_bands[s][or].length; y++) {
                  	for (x = 0; x < err_bands[s][or][0].length; x++) {
                  	    diff = err_bands[s][or][y][x] - mean;
                  	    total += diff*diff;
                  	} // for (x = 0; x < err_bands[s][or][0].length; x++)
                  } // for (y = 0; y < err_bands[s][or].length; y++) 
                  std = Math.sqrt(total/(err_bands[s][or].length * err_bands[s][or][0].length));	
                 
          		c = std/(vsnr_data_zeta * vsnr_data_filter_gains[s]);
          		vsnr_data_Ces[s] = vsnr_data_Ces[s] + c*c;
              } // for (or = 0; or < 3; or++)
          } // for (s = 0; s < vsnr_data_num_levels; s++)
    	  
    	  for (s = 0; s < vsnr_data_num_levels; s++) {
    	      vsnr_data_Ces[s] = Math.sqrt(vsnr_data_Ces[s]);
    	  }
    	  
    	  // compute the actual CSNRs
    	  res = new double[vsnr_data_num_levels];
    	  for (s = 0; s < vsnr_data_num_levels; s++) {
    	      res[s] = vsnr_data_Cis[s]/ vsnr_data_Ces[s];
    	  }
    	}
    	else {
    	  double lum_diff = Math.abs(mean_err_img_lum - vsnr_data_mL);
    	  if (lum_diff / vsnr_data_mL < 0.01) {
    	    // distortions are subthreshold (undetectable)
    	    res = new double[] {-1};
    	  }
    	  else {
    	    // punt to Weber's law for brightness-only differences
    	    res = new double [] {1.5};
    	    vsnr_data_Ci = Math.pow(10,(vsnr_data_mL / lum_diff));
    	  }
    	}
        return res;


    }
    
    private void analyze_src_img() {
    	// % compute some useful statistics about the original image
    	// mean pixel value
    	int i, x, y, index, s, or;
    	double diff;
    	double c;
    	double total = 0.0;
    	double src_img_lum[];
    	double std;
    	double refBuffer[] = null;
    	if (isColor) {
    		refBuffer = new double[length];
    		for (i = 0; i < length; i++) {
    			refBuffer[i] = 0.2989 * referenceRedBuffer[i] + 0.5870 * referenceGreenBuffer[i] + 0.1140 * referenceBlueBuffer[i];
    		}
    	}
    	else {
    		refBuffer = referenceBuffer;
    	}
    	
		for (i = 0; i < length; i++) {
    		total += refBuffer[i];
    	}
    	vsnr_data_mX = total/length;
    	
    	// mean luminance
    	total = 0;
    	src_img_lum = new double[length];
    	for (i = 0; i < length; i++) {
    		src_img_lum[i] = vsnr_data_pix2lum_table[(int)Math.round(refBuffer[i])];
    		total += (src_img_lum[i]);
    	}
    	vsnr_data_mL = total/length;
    	
    	// total RMS image contrast
    	// Standard deviation normalized by N rather than N-1
        total = 0.0;
        for (i = 0; i < length; i++) {
        	diff = src_img_lum[i] - vsnr_data_mL;
        	total += diff*diff;
        }
        std = Math.sqrt(total/length);	
        vsnr_data_Ci = std/vsnr_data_mL;
    	
        // zeta value used to estimate image contrast at each scale
        vsnr_data_zeta = vsnr_data_mL *
          Math.pow((vsnr_data_b + vsnr_data_k*vsnr_data_mX),(1 - vsnr_data_g)) /
          (vsnr_data_k * vsnr_data_g);  

        // compute the image contrast at each scale
        // IMDWT Forward discrete wavelet transform of an image.
        //  [BANDS] = IMDWT(IMG, NLEVELS) returns the subbands of a discrete
        //  wavelet transform of the image IMG using the 9/7 biorthogonal 
        //  filters.  The number of decomposition levels is specified via the
        //  parameter NLEVELS (default = 5).
        
	     // The Decompose() method uses the first image in the list as the
	     // source image.  It then decomposes (forward DWTs) this image
	     // and stores the results as depicted above, in the list.  The
	     // source image, WaveList->Image() remains untouched.  L1 is
	     // the smooth (low-passed) image whose rows have been transformed.
	     // H1 is the detail (high-passed) image whose rows have been
	     // transformed.  HH1 is the detail (high-high passed) image whose
	     // rows then columns have been transformed.  HL1 is the detail
	     // (high-low passed) image whose rows then columns have been
	     // transformed.  LH1 is the detail (low-high passed) image whose
	     // rows then columns have been transformed.  Finally, LL1 is the
	     // smooth (low-low-passed) image whose rows then columns have been
	     // transformed.  LL1 is used as the source image for the next
	     // level of decomposition.  For this reason, the LL buffers are
	     // stored last in the zero-based indices [0, 6, 12, 18, 24, ...].
         // LL0  | L1  | H1  | HH1 | HL1 | LH1 | LL1 
         double LL0[][] = new double[yDim][xDim];
         for (y = 0; y < yDim; y++) {
        	 for (x = 0; x < xDim; x++) {
        		 index = x + y * xDim;
        		 LL0[y][x] = refBuffer[index];
        	 }
         }

        double vsnr_data_src_bands[][][][] = imdwt(LL0, vsnr_data_num_levels);
        
        vsnr_data_Cis = new double[vsnr_data_num_levels];
        for (s = 0; s < vsnr_data_num_levels; s++) {
            for (or = 0; or < 3; or++) {
            // estimate the contrast at scale s, orient o
            	// Standard deviation normalized by N rather than N-1
            	total = 0.0;
                for (y = 0; y < vsnr_data_src_bands[s][or].length; y++) {
                	for (x = 0; x < vsnr_data_src_bands[s][or][0].length; x++) {
                	    total += vsnr_data_src_bands[s][or][y][x];
                	} // for (x = 0; x < vsnr_data_src_bands[s][or][0].length; x++)
                } // for (y = 0; y < vsnr_data_src_bands[s][or].length; y++) 
                double mean = total/(vsnr_data_src_bands[s][or].length * vsnr_data_src_bands[s][or][0].length);
                total = 0.0;
                for (y = 0; y < vsnr_data_src_bands[s][or].length; y++) {
                	for (x = 0; x < vsnr_data_src_bands[s][or][0].length; x++) {
                	    diff = vsnr_data_src_bands[s][or][y][x] - mean;
                	    total += diff*diff;
                	} // for (x = 0; x < vsnr_data_src_bands[s][or][0].length; x++)
                } // for (y = 0; y < vsnr_data_src_bands[s][or].length; y++) 
                std = Math.sqrt(total/(vsnr_data_src_bands[s][or].length * vsnr_data_src_bands[s][or][0].length));	
               
        		c = std/(vsnr_data_zeta * vsnr_data_filter_gains[s]);
        		vsnr_data_Cis[s] = vsnr_data_Cis[s] + c*c;
            } // for (or = 0; or < 3; or++)
        } // for (s = 0; s < vsnr_data_num_levels; s++)
        
        for (s = 0; s < vsnr_data_num_levels; s++) {
        	vsnr_data_Cis[s] = Math.sqrt(vsnr_data_Cis[s]);	
        }
        
        // compute the total distortion contrast threshold
        double ctsnrs = best_csnrs(0);
        double cTes[] = new double[vsnr_data_num_levels];
        double totalSquared = 0.0;
        for (s = 0; s < vsnr_data_num_levels; s++) {
            cTes[s] = vsnr_data_Cis[s]/ ctsnrs;
            totalSquared += (cTes[s]*cTes[s]);
        }
        vsnr_data_CTe = Math.sqrt(totalSquared);

    }
    
    private double best_csnrs(double v_idx) {
            int i;
    		double a0 = 59.8;
    		double a1 = -0.1258; 
    		double a2 = -0.1087;

    		double b2 = (-1 - a2)*v_idx + a2;
    		double b1 = (1 - a1)*v_idx + a1;
    		double b0 = -a0*v_idx + a0;  
     
    		double res = 0.0;
    		for (i = 0; i < vsnr_data_fs.length; i++) {
    		    res = Math.max(res, b0*(Math.pow(vsnr_data_fs[i],(b2*Math.log(vsnr_data_fs[i]) + b1))));
    		}
    		return res;
    }

    
    private double[][][][] imdwt(double LL0[][], int num_levels) {
    	int i, s, num_orients, or;
        int num_bands = 6*num_levels+1;
        double bands[][][] = new double[num_bands][][];
        int cx = LL0[0].length; // image width
        int cy = LL0.length; // image height
        int half_cx = cx >> 1;
	    int half_cy = cy >> 1;
	    bands[0] = LL0;
    	for (i = 1; i < num_bands; i += 6 ) {
            // make room for the L and H subbands
    		bands[i] = new double[cy][half_cx];
    		bands[i+1] = new double[cy][half_cx];
	        // make room for the HH, HL, LH, and LL subbands
	        bands[i+2] = new double[half_cy][half_cx];
	        bands[i+3] = new double[half_cy][half_cx];
	        bands[i+4] = new double[half_cy][half_cx];
	        bands[i+5] = new double[half_cy][half_cx];
	
	        // divide the dimensions by 2 for the next scale
	        cy >>= 1; half_cx >>= 1; half_cy >>= 1;

    	} // for (i = 0; i < num_bands; i += 6 )
    	
    	
    	// perform the forward DWT
    	int scale_index;
    	double LL[][];
    	double L[][];
    	double H[][];
    	for (scale_index = 0; scale_index < num_levels; ++scale_index) {
    		// 2D transform
		    //
		    // extract the source image (or the LL subband
		    // from the previous level of decomposition)
		    //
			// buf_type& LL = WaveList.LL(scale_index);
			LL = bands[6*scale_index];

		    // grab a reference to the L and H subbands
		    L = bands[6*scale_index+1];
		    H = bands[6*scale_index+2];

		    // filter the rows and downsample horizontally
		    DoTransformRows(LL, L, H);

		    // filter the columns and downsample vertically
		    //DoTransformCols(
		      //H, WaveList.HL(scale_index), WaveList.HH(scale_index)
		      //);
		    DoTransformCols(H, bands[6*scale_index+4], bands[6*scale_index+3]);
		    //DoTransformCols(
		      //L, WaveList.LL(scale_index), WaveList.LH(scale_index)
		      //);
		    DoTransformCols(L, bands[6*scale_index+6], bands[6*scale_index+5]);
    		 
    	} // for (scale_index = 0; scale_index < vsnr_data_num_levels; ++scale_index)
    	
    	boolean testWithReconstruct = false;
    	if (testWithReconstruct) {
    		// Ratio of error norm to original norm = 1.0811313924279027E-15
			// VSNR = 118.64166398052696
			// Ratio of error norm to original norm = 1.046812260394941E-15
			// VSNR = 98.63308987130611
			// Ratio of error norm to original norm = 1.046812260394941E-15
			// Ratio of error norm to original norm = 9.910623761895389E-16
			// VSNR = 7.2918431170333
			// Ratio of error norm to original norm = 1.046812260394941E-15
			// Ratio of error norm to original norm = 1.0317699615517395E-15
			// VSNR = 35.39332288920834
			// Ratio of error norm to original norm = 1.0844799139090019E-15
			// Ratio of error norm to original norm = 1.06215200623753E-15
    		int x,y;
    	     double bandsReconstruct[][][] = new double[num_bands][][];
    	     for (s = 0; s < num_bands; s++) {
    	    	 bandsReconstruct[s] = new double[bands[s].length][bands[s][0].length];
    	    	 for (y = 0; y < bands[s].length; y++) {
    	    		 for (x = 0; x < bands[s][0].length; x++) {
    	    		     bandsReconstruct[s][y][x] = bands[s][y][x];	 
    	    		 }
    	    	 }
    	     }
    	     Reconstruct(bandsReconstruct, vsnr_data_num_levels);
    	     double LL0Reconstruct[][] = bandsReconstruct[0];
    	     double reconstructError[][] = new double[yDim][xDim];
    	     double LL0TotalSquared = 0;
    	     double errorTotalSquared = 0;
    	     for (y = 0; y < yDim; y++) {
    	         for (x = 0; x < xDim; x++) {
    	        	 reconstructError[y][x] = LL0Reconstruct[y][x] - LL0[y][x];
    	        	 LL0TotalSquared += (LL0[y][x]*LL0[y][x]);
    	        	 errorTotalSquared += (reconstructError[y][x]*reconstructError[y][x]);
    	         }
    	     }
    	     double normRatio = Math.sqrt(errorTotalSquared/LL0TotalSquared);
    	     System.out.println("Ratio of error norm to original norm = " + normRatio);
    	     UI.setDataText("Ratio of error norm to original norm = " + normRatio + "\n");
    	} // if (testWithReconstruct)
    	
    	double p_cell_mx[][][][] = new double[num_levels][][][];
        for (s = 1; s <= num_levels; ++s)  {
    	    if (s == num_levels) {
    	        num_orients = 4;	
    	    }
    	    else {
    	    	num_orients = 3;
    	    }
    	    p_cell_mx[s-1] = new double[num_orients][][];
    	    for (or = 0; or < num_orients; or++) {
    	        if (or == 0) {
    	          // return LH(scale_index);
    	        	p_cell_mx[s-1][or] = bands[6*s-1];
    	        }
    	        else if (or == 1) {
    	          // return HL(scale_index);
    	        	p_cell_mx[s-1][or] = bands[6*s-2];
    	        }
    	        else if (or == 2) {
    	        	// return HH(scale_index)
    	        	p_cell_mx[s-1][or] = bands[6*s-3];
    	        }
    	        else {
    	        	// return LL(scale_index)
    	        	p_cell_mx[s-1][or] = bands[6*s];
    	        }
    	    } // for (or = 0; or < num_orients; or++)
    	} // for (s = 1; s <= vsnr_data_num_levels; ++s)
        return p_cell_mx;

    } // imdwt

    public void DoTransformRows(double Buffer[][], double SBuffer[][], double DBuffer[][]) {
    	  final int sw = SBuffer[0].length;
    	  final int sh = SBuffer.length;
    	  final int sw_minus_one = sw - 1;

    	  double px_row[];
    	  double pd_row[];
    	  double ps_row[];

    	  int x;
    	  double d_res0, old_d_res, d_res, X2n;
    	  int y;
    	  for (y = 0; y < sh; ++y)
    	  {
    	    px_row = Buffer[y];
    	    pd_row = DBuffer[y];
    	    ps_row = SBuffer[y];

    	    d_res0 = old_d_res =
    	      px_row[1] + ALPHA * (px_row[0] + px_row[2]);
    	      pd_row[0] = old_d_res;
    	    for (x = 1; x < sw_minus_one; ++x)
    	    {
    	      X2n = px_row[x << 1];
    	      d_res =
    	        px_row[(x << 1) + 1] +
    	        ALPHA * (X2n + px_row[(x << 1) + 2]);

    	      pd_row[x] = d_res;
    	      ps_row[x] = X2n + BETA * (d_res + old_d_res);
    	      old_d_res = d_res;
    	    }
    	    d_res =
    	      px_row[(sw << 1) - 1] +
    	      TWOALPHA * px_row[(sw << 1) - 2];
    	    pd_row[sw_minus_one] = d_res;
    	    ps_row[0] = px_row[0] + TWOBETA * d_res0;
    	    ps_row[sw_minus_one] =
    	      px_row[sw_minus_one << 1] +
    	      BETA * (d_res + old_d_res);

    	    d_res0 = old_d_res =
    	      pd_row[0] + GAMMA * (ps_row[0] + ps_row[1]);
    	    pd_row[0] = old_d_res;
    	    for (x = 1; x < sw_minus_one; ++x)
    	    {
    	      d_res =
    	        pd_row[x] + GAMMA * (
    	         ps_row[x] + ps_row[x + 1]);
    	      pd_row[x] = d_res;
    	      SBuffer[y][x] += DELTA * (d_res + old_d_res);
    	      old_d_res = d_res;
    	    }
    	    d_res =
    	      pd_row[sw_minus_one] +
    	      TWOGAMMA * ps_row[sw_minus_one];
    	    pd_row[sw_minus_one] = d_res;
    	    SBuffer[y][0] += TWODELTA * d_res0;
    	    SBuffer[y][sw_minus_one] += DELTA * (d_res + old_d_res);
    	  }
    	  
    	  if (GWAVELIFT_NORM_1_1) {
    		  final double B0 =       1.0/1.23017410558578;
    		  final double B1 =       1.0/1.62578613134411;
    		  for (y = 0; y < SBuffer.length; y++) {
    			  for (x = 0; x < SBuffer[0].length; x++) {
    				  SBuffer[y][x] *= B0;  
    			  }
    		  }
    		  for (y = 0; y < DBuffer.length; y++) {
    			  for (x = 0; x < DBuffer[0].length; x++) {
    				  DBuffer[y][x] *= B1;  
    			  }
    		  }
    	    }
    	    else {
    		  final double K =        0.8698643;
    		  final double KINV =     1.1496046;
    		  for (y = 0; y < SBuffer.length; y++) {
    			  for (x = 0; x < SBuffer[0].length; x++) {
    				  SBuffer[y][x] *= KINV;  
    			  }
    		  }
    		  for (y = 0; y < DBuffer.length; y++) {
    			  for (x = 0; x < DBuffer[0].length; x++) {
    				  DBuffer[y][x] *= -K;  
    			  }
    		  }
    	    }
	
    }

    public void DoTransformCols(double Buffer[][], double SBuffer[][], double DBuffer[][]) {
  	    final int sw = SBuffer[0].length;
  	    final int sh = SBuffer.length;
  	    final int sh_minus_one = sh - 1;

    	  int x, y;
    	  int ysave0, ysave1;
    	  double d_res0, old_d_res, d_res, X2n;
    	  for (x = 0; x < sw; ++x)
    	  {

    	    d_res0 = old_d_res =
    	      Buffer[1][x] + ALPHA * (
    	        Buffer[0][x] + Buffer[2][x]);
    	    DBuffer[0][x] = old_d_res;
    	    for (y = 1; y < sh_minus_one; ++y)
    	    {
    	      ysave0 = (y << 1);
    	      ysave1 = y;

    	      X2n = Buffer[ysave0][x];
    	      d_res =
    	        Buffer[ysave0 + 1][x] +
    	        ALPHA * (X2n + Buffer[ysave0 + 2][x]);

    	      DBuffer[ysave1][x] = d_res;
    	      SBuffer[ysave1][x] = X2n + BETA * (d_res + old_d_res);
    	      old_d_res = d_res;
    	    }
    	    d_res =
    	      Buffer[(sh << 1) - 1][x] +
    	      TWOALPHA * Buffer[(sh << 1) - 2][x];
    	    DBuffer[sh_minus_one][x] =  d_res;
    	    SBuffer[0][x] = Buffer[0][x] + TWOBETA * d_res0;
    	    SBuffer[sh_minus_one][x] =
    	      Buffer[sh_minus_one << 1][x] +
    	      BETA * (d_res + old_d_res);

    	    d_res0 = old_d_res =
    	      DBuffer[0][x] + GAMMA * (SBuffer[0][x] + SBuffer[1][x]);
    	    DBuffer[0][x] = old_d_res;
    	    for (y = 1; y < sh_minus_one; ++y)
    	    {
    	      ysave0 = y;
    	      d_res = DBuffer[ysave0][x] + GAMMA * (
    	        SBuffer[ysave0][x] + SBuffer[ysave0 + 1][x]);
    	      DBuffer[ysave0][x] = d_res;
    	      SBuffer[y][x] += DELTA * (d_res + old_d_res);
    	      old_d_res = d_res;
    	    }
    	    d_res =
    	      DBuffer[sh_minus_one][x] +
    	      TWOGAMMA * SBuffer[sh_minus_one][x];
    	    DBuffer[sh_minus_one][x] = d_res;
    	    SBuffer[0][x] += TWODELTA * d_res0;
    	    SBuffer[sh_minus_one][x] += DELTA * (d_res + old_d_res);
    	  }
    	  
    	  if (GWAVELIFT_NORM_1_1) {
    		  final double B0 =       1.0/1.23017410558578;
    		  final double B1 =       1.0/1.62578613134411;
    		  for (y = 0; y < SBuffer.length; y++) {
    			  for (x = 0; x < SBuffer[0].length; x++) {
    				  SBuffer[y][x] *= B0;  
    			  }
    		  }
    		  for (y = 0; y < DBuffer.length; y++) {
    			  for (x = 0; x < DBuffer[0].length; x++) {
    				  DBuffer[y][x] *= B1;  
    			  }
    		  }
    	    }
    	    else {
    		  final double K =        0.8698643;
    		  final double KINV =     1.1496046;
    		  for (y = 0; y < SBuffer.length; y++) {
    			  for (x = 0; x < SBuffer[0].length; x++) {
    				  SBuffer[y][x] *= KINV;  
    			  }
    		  }
    		  for (y = 0; y < DBuffer.length; y++) {
    			  for (x = 0; x < DBuffer[0].length; x++) {
    				  DBuffer[y][x] *= -K;  
    			  }
    		  }
    	    }
	
    }

	 // The Reconstruct() method uses the last four buffers (HHn, HLn,
	 // LHn, LLn) in the list as the starting point, and then inverse
	 // transforms their columns, storing the result in the fourth and
	 // fifth from the last positions (Hn and Ln, repsectively).  It
	 // then inverse transforms these results, storing the result in
	 // the LL(n-1) position.  This latter result is then used to
	 // reconstruct the next level.  Upon completion, the inverse
	 // transformed image is stored in the first position of the list.
     private void Reconstruct(double bands[][][], int num_scales) {
    	  int scale_index;
    	  for ( scale_index = num_scales; scale_index >= 1; --scale_index) {
    		// extract the HH and HL subbands from the list
    		    double HH[][] = bands[6*scale_index-3];
    		    double HL[][] = bands[6*scale_index-2];
    		    //
    		    // extract the H subband from the list
    		    // (this buffer will be overwritten)
    		    //
    		    double H[][] = bands[6*scale_index-4];
    		    //
    		    // upsample vertically, filter the columns, and then
    		    // sum the results (which are stored in H)
    		    // H = filter(HL_up_2 w/ filt7) + filter(HH_up_2 w/ filt9)
    		    //
    		    DoUntransformCols(H, HL, HH);

    		    // extract the LH and LL subbands from the list
    		    double LH[][] = bands[6*scale_index-1];
    		    double LL[][] = bands[6*scale_index];
    		    //
    		    // extract the L subband from the list
    		    // (this buffer will be overwritten)
    		    //
    		    double L[][] = bands[6*scale_index-5];
    		    //
    		    // upsample vertically, filter the columns, and then
    		    // sum the results (which are stored in L)
    		    // L = filter(LL_up_2 w/ filt7) + filter(LH_up_2 w/ filt9)
    		    //
    		    DoUntransformCols(L, LL, LH);

    		    //
    		    // extract the buffer for the reconstructed image
    		    // (this buffer will be overwritten)
    		    //
    		    double LLprev[][] = bands[6*(scale_index - 1)];
    		    //
    		    // upsample horizontally, filter the rows, and then
    		    // sum the results (which are stored in LL_prev_scale)
    		    // LLprev = filter(L_up_2 w/ filt7) + filter(H_up_2 w/ filt9)
    		    //
    		    DoUntransformRows(LLprev, L, H);
          }
     } // Reconstruct
     
     private void DoUntransformCols( double Buffer[][], double SBuffer[][], double DBuffer[][]) {
    	   //
    	   // NOTE: all widths are the same while Buffer's
    	   // height is 2x that of SBuffer and DBuffer
    	   //
    	   final int sw = SBuffer[0].length;
    	   final int sh = SBuffer.length;
    	   final int sh_minus_one = sh - 1;
    	   int y,x;

    	   if (GWAVELIFT_NORM_1_1) {
     		  final double B0 =       1.0/1.23017410558578;
     		  final double B1 =       1.0/1.62578613134411;
     		  for (y = 0; y < SBuffer.length; y++) {
     			  for (x = 0; x < SBuffer[0].length; x++) {
     				  SBuffer[y][x] /= B0;  
     			  }
     		  }
     		  for (y = 0; y < DBuffer.length; y++) {
     			  for (x = 0; x < DBuffer[0].length; x++) {
     				  DBuffer[y][x] /= B1;  
     			  }
     		  }
     	    }
     	    else {
     		  final double K =        0.8698643;
     		  final double KINV =     1.1496046;
     		  for (y = 0; y < SBuffer.length; y++) {
     			  for (x = 0; x < SBuffer[0].length; x++) {
     				  SBuffer[y][x] *= K;  
     			  }
     		  }
     		  for (y = 0; y < DBuffer.length; y++) {
     			  for (x = 0; x < DBuffer[0].length; x++) {
     				  DBuffer[y][x] *= -KINV;  
     			  }
     		  }
     	    }

    	   int ysave0;
    	   double s_res, d_res, d_res0, d_res_last;
    	   for (x = 0; x < sw; ++x)
    	   {

    	     SBuffer[0][x] -= (TWODELTA * DBuffer[0][x]);
    	     for (y = 1; y < sh; ++y)
    	     {
    	       ysave0 = y;
    	       SBuffer[y][x] -= DELTA * (
    	         DBuffer[ysave0][x] + DBuffer[ysave0 - 1][x]);
    	     }

    	     d_res0 = d_res_last =
    	       DBuffer[0][x] - GAMMA * (SBuffer[0][x] + SBuffer[1][x]);
    	     DBuffer[0][x] = d_res_last;
    	     for (y = 1; y < sh_minus_one; ++y)
    	     {
    	       ysave0 = y;

    	       s_res = SBuffer[ysave0][x];
    	       d_res = DBuffer[ysave0][x] -
    	         GAMMA * (s_res + SBuffer[ysave0 + 1][x]);

    	       DBuffer[ysave0][x] = d_res;
    	         Buffer[y << 1][x] =  s_res - BETA * (d_res + d_res_last);
    	       d_res_last = d_res;
    	     }
    	     d_res =
    	       DBuffer[sh_minus_one][x] -
    	       TWOGAMMA * SBuffer[sh_minus_one][x];
    	       DBuffer[sh_minus_one][x] =  d_res;
    	       Buffer[0][x]= SBuffer[0][x] - TWOBETA * d_res0;
    	       Buffer[sh_minus_one << 1][x] =
    	       SBuffer[sh_minus_one][x] -
    	       BETA * (d_res + d_res_last);

    	     for (y = 0; y < sh_minus_one; ++y)
    	     {
    	       ysave0 = (y << 1);
    	       Buffer[ysave0 + 1][x] =
    	         DBuffer[y][x] - ALPHA * (
    	           Buffer[ysave0][x] + Buffer[ysave0 + 2][x]);
    	     }
    	     Buffer[(sh << 1) - 1][x] =
    	       DBuffer[sh_minus_one][x] -
    	       TWOALPHA * Buffer[(sh << 1) - 2][x];
    	   }
	 
     }

     private void DoUntransformRows( double Buffer[][], double SBuffer[][], double DBuffer[][]) {
    	   //
    	   // NOTE: widths and heights are the same
    	   //
    	   final int sw = SBuffer[0].length;
    	   final int sh = SBuffer.length;
    	   final int sw_minus_one = sw - 1;

    	   double px_row[];
     	   double pd_row[];
     	   double ps_row[];
     	   int y,x;
          
     	  if (GWAVELIFT_NORM_1_1) {
     		  final double B0 =       1.0/1.23017410558578;
     		  final double B1 =       1.0/1.62578613134411;
     		  for (y = 0; y < SBuffer.length; y++) {
     			  for (x = 0; x < SBuffer[0].length; x++) {
     				  SBuffer[y][x] /= B0;  
     			  }
     		  }
     		  for (y = 0; y < DBuffer.length; y++) {
     			  for (x = 0; x < DBuffer[0].length; x++) {
     				  DBuffer[y][x] /= B1;  
     			  }
     		  }
     	    }
     	    else {
     		  final double K =        0.8698643;
     		  final double KINV =     1.1496046;
     		  for (y = 0; y < SBuffer.length; y++) {
     			  for (x = 0; x < SBuffer[0].length; x++) {
     				  SBuffer[y][x] *= K;  
     			  }
     		  }
     		  for (y = 0; y < DBuffer.length; y++) {
     			  for (x = 0; x < DBuffer[0].length; x++) {
     				  DBuffer[y][x] *= -KINV;  
     			  }
     		  }
     	    }
    	   
    	   double s_res, d_res, old_d_res, d_res0;
    	   for (y = 0; y < sh; ++y)
    	   {
    	     px_row = Buffer[y];
    	     pd_row = DBuffer[y];
    	     ps_row = SBuffer[y];

    	     SBuffer[y][0] -= (TWODELTA * pd_row[0]);
    	     for (x = 1; x < sw; ++x)
    	     {
    	       SBuffer[y][x] -= DELTA * (
    	         pd_row[x] + pd_row[x - 1]);
    	     }

    	     d_res0 = old_d_res =
    	       pd_row[0] - GAMMA * (ps_row[0] + ps_row[1]);
    	     pd_row[0] = old_d_res;
    	     for (x = 1; x < sw_minus_one; ++x)
    	     {
    	       s_res = ps_row[x];
    	       d_res =
    	         pd_row[x] -
    	         GAMMA * (s_res + ps_row[x + 1]);

    	       pd_row[x] = d_res;
    	       px_row[x << 1] = s_res - BETA * (d_res + old_d_res);
    	       old_d_res = d_res;
    	     }
    	     d_res =
    	       pd_row[sw_minus_one] -
    	       TWOGAMMA * ps_row[sw_minus_one];
    	     pd_row[sw_minus_one] = d_res;
    	     px_row[0] = ps_row[0] - TWOBETA * d_res0;
    	     px_row[sw_minus_one << 1] =
    	       ps_row[sw_minus_one] - BETA * (d_res + old_d_res);

    	     for (x = 0; x < sw_minus_one; ++x)
    	     {
    	       px_row[(x << 1) + 1] =
    	         pd_row[x] - ALPHA * (
    	           px_row[x << 1] +
    	           px_row[(x << 1) + 2]);
    	     }
    	     px_row[(sw << 1) - 1] =
    	       pd_row[sw_minus_one] -
    	       TWOALPHA * px_row[(sw << 1) - 2];
    	   }
	 
     }
    
}