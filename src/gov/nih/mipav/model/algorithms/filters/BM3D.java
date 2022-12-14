package gov.nih.mipav.model.algorithms.filters;


import gov.nih.mipav.model.algorithms.*;
import gov.nih.mipav.model.structures.*;

import gov.nih.mipav.view.*;

import java.io.*;
import java.util.ArrayList;
import java.util.Collections;
import java.util.Comparator;
/*
 * This is a port of the GITHUB code BM3D_py-master.
 * The README.md file has:
 * [![LICENSE](https://img.shields.io/badge/license-Anti%20996-blue.svg)](https://github.com/996icu/996.ICU/blob/master/LICENSE)
 * https://github.com/996icu/996.ICU/blob/master/LICENSE has:
 * Copyright (c) <year> <copyright holders>
	

	"Anti 996" License Version 1.0 (Draft)
	

	Permission is hereby granted to any individual or legal entity
	obtaining a copy of this licensed work (including the source code,
	documentation and/or related items, hereinafter collectively referred
	to as the "licensed work"), free of charge, to deal with the licensed
	work for any purpose, including without limitation, the rights to use,
	reproduce, modify, prepare derivative works of, distribute, publish
	and sublicense the licensed work, subject to the following conditions:
	

	1. The individual or the legal entity must conspicuously display,
	without modification, this License and the notice on each redistributed
	or derivative copy of the Licensed Work.
	

	2. The individual or the legal entity must strictly comply with all
	applicable laws, regulations, rules and standards of the jurisdiction
	relating to labor and employment where the individual is physically
	located or where the individual was born or naturalized; or where the
	legal entity is registered or is operating (whichever is stricter). In
	case that the jurisdiction has no such laws, regulations, rules and
	standards or its laws, regulations, rules and standards are
	unenforceable, the individual or the legal entity are required to
	comply with Core International Labor Standards.
	

	3. The individual or the legal entity shall not induce, suggest or force
	its employee(s), whether full-time or part-time, or its independent
	contractor(s), in any methods, to agree in oral or written form, to
	directly or indirectly restrict, weaken or relinquish his or her
	rights or remedies under such laws, regulations, rules and standards
	relating to labor and employment as mentioned above, no matter whether
	such written or oral agreements are enforceable under the laws of the
	said jurisdiction, nor shall such individual or the legal entity
	limit, in any methods, the rights of its employee(s) or independent
	contractor(s) from reporting or complaining to the copyright holder or
	relevant authorities monitoring the compliance of the license about
	its violation(s) of the said license.
	

	THE LICENSED WORK IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND,
	EXPRESS OR IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF
	MERCHANTABILITY, FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT.
	IN NO EVENT SHALL THE COPYRIGHT HOLDER BE LIABLE FOR ANY CLAIM,
	DAMAGES OR OTHER LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR
	OTHERWISE, ARISING FROM, OUT OF OR IN ANY WAY CONNECTION WITH THE
	LICENSED WORK OR THE USE OR OTHER DEALINGS IN THE LICENSED WORK.
	
	Reference: "An Analysis and Implementation of the BM3D Image Denoising
	Method" by Marc Lebrun, Published in Image Processing on Line on
	2012-08-08.

 */
import java.util.Vector;

import Jama.Matrix;

public class BM3D extends AlgorithmBase {
	
	// destImage[0] is output of stage 1 with hard thresholding
	// destImage[1] is output of stage 2 with Wiener thresholding
	private ModelImage[] destImage;
	
	private boolean estimateNoiseStandardDeviation = true;
	
	// White Gaussian noise standard deviation
	private double sigma; 
	
	// Hard thresholding search window size
	private int n_H = 16;
	
	// Hard thresholding patch size
	private int k_H = 8;
	
	// Hard thresholding maximum number of similar patches kept
	private int N_H = 16;
	
	// Hard thresholding  In order to speed up the processing, the loop over of the pixels of the
	// image is done with a step p (integer) in row and column.  For example if p = 3 the algorithm
	// is accelerated by a 9 factor.
	private int p_H = 3;
	
	// Hard thresholding maximum threshold for the distance between two similar patches
	// tauMatch_H = 2500 if sigma < 40 and 5000 otherwise.  Moreover, if N_H >= 32, this
	// threshold is multiplied by a factor of 5.
	private double tauMatch_H;
	
	private boolean useSD_H = false;
	
	// Threshold for 2D transform applied to each patch of the 3D group.  This threshold only
	// appears for sigma > 40 since tau_2D_H = 0 for sigma <= 40.  Applying a theshold to the
	// 2D transform coefficients for low values of sigma is useless since there is no 
	// improvement after the second step.  Moreover for a noise lower than 5 the results are
	// degraded.  tau_2D_H is a Bior1.5 transform whatever the value of sigma.  For the 
	// bi-orthogonal spline wavelet the vanishing moments of the  decomposing and reconstructing
	// wavelete functions are 1 and 5 repectively.
	private String tau_2D_H = "BIOR";
	
	// Coefficient hard thresholding level of the 3D group in the transform domain during the first
	// filtering sub-step.  The chosen value is 2.7.
	private double lambda3D_H = 2.7;
	
	// Wiener thresholding search window size
    private int n_W = 16;
    
    // Wiener thresholding patch size
 	private int k_W = 8;
 	
    // Wiener thresholding maximum number of similar patches kept
 	private int N_W = 32;
 	
    // Wiener thresholding  In order to speed up the processing, the loop over of the pixels of the
 	// image is done with a step p (integer) in row and column.  For example if p = 3 the algorithm
 	// is accelerated by a 9 factor.
 	private int p_W = 3;
 	
    // Wiener thresholding maximum threshold for the distance between two similar patches
 	// tauMatch_W = 400 is a good value for sigma between 0 and 40
 	private double tauMatch_W;
 	
 	private boolean useSD_W = true;
 	
    // Threshold for 2D transform applied to each patch of the 3D group.
 	// A 2D DCT transform is used.
 	private String tau_2D_W = "DCT";
	
	public BM3D(ModelImage[] destImage, ModelImage srcImg, 
			boolean estimateNoiseStandardDeviation, double sigma, int n_H, int N_H,
			int p_H, boolean useSD_H, String tau_2D_H, double lambda3D_H,
			int n_W, int N_W, int p_W, boolean useSD_W, String tau_2D_W) {
		super(null, srcImg);
		this.destImage = destImage;
		this.estimateNoiseStandardDeviation = estimateNoiseStandardDeviation;
		this.sigma = sigma;
		this.n_H = n_H;
		this.p_H = p_H;
		this.useSD_H = useSD_H;
		this.tau_2D_H = tau_2D_H;
		this.lambda3D_H = lambda3D_H;
		this.n_W = n_W;
		this.N_W = N_W;
		this.p_W = p_W;
		this.useSD_W = useSD_W;
		this.tau_2D_W = tau_2D_W;
	}
	
	public void runAlgorithm() {
		int x, y;
		if (srcImage == null) {
            displayError("Source Image is null");

            return;
        }
		
		if (estimateNoiseStandardDeviation) {
        	double noiseStd[] = new double[1];
        	// Obtain noise standard deviation with subband containing finest level diagonal details
        	PyWavelets pAlgo = new PyWavelets(srcImage, noiseStd);
        	pAlgo.run();
        	sigma = noiseStd[0];
        	System.out.println("Noise standard deviation estimated as " + sigma);
        	pAlgo.finalize();
        	pAlgo = null;
        }
		
		if (sigma < 35.0) {
		    tauMatch_H = 2500.0;	
		}
		else {
			tauMatch_H = 5000.0;
		}
		
		if (sigma < 35.0) {
		    tauMatch_W = 400.0;	
		}
		else {
			tauMatch_W = 3500.0;
		}
		
		if ((tau_2D_H == "BIOR") || (sigma < 40.)) {
			k_H = 8;
		}
		else {
			k_H = 12;
		}
		
		
		if ((tau_2D_W == "BIOR") || (sigma < 40.)) {
			k_W = 8;
		}
		else {
			k_W = 12;
		}
		
		ModelImage noisy_im_p = symetrize(srcImage, n_H);
		
		double img_basic_pad[][] = bm3d_1st_step(sigma, noisy_im_p, n_H, k_H, N_H, p_H, lambda3D_H, tauMatch_H, useSD_H, tau_2D_H);
		double img_basic_arr[] = new double[srcImage.getExtents()[1]*srcImage.getExtents()[0]];
		for (y = n_H; y < noisy_im_p.getExtents()[1] - n_H; y++) {
			for (x = n_H; x < noisy_im_p.getExtents()[0] - n_H; x++) {
				img_basic_arr[(y-n_H)*srcImage.getExtents()[0] + (x-n_H)] = img_basic_pad[y][x];
				if (Double.isNaN(img_basic_arr[(y-n_H)*srcImage.getExtents()[0] + (x-n_H)])) {
					System.err.println("NaN found in img_basic_arr at y = " + (y-n_H) + " x = " + (x-n_H));
					setCompleted(false);
					return;
				}
			}
		}
		
		try {
			destImage[0].importData(0, img_basic_arr, true);
		}
		catch (IOException e) {
			MipavUtil.displayError("IOException on destImage[0].importData(0, img_basic_arr, true)");
			setCompleted(false);
			return;
		}
		
		ModelImage img_basic_p = symetrize(destImage[0], n_W);
		
		noisy_im_p.disposeLocal();
		noisy_im_p = null;
		noisy_im_p = symetrize(srcImage, n_W);
		double img_denoised_pad[][] = bm3d_2nd_step(sigma, noisy_im_p, img_basic_p, n_W, k_W, N_W, p_W, tauMatch_W, useSD_W, tau_2D_W);
		double img_denoised_arr[] = new double[srcImage.getExtents()[1]*srcImage.getExtents()[0]];
		for (y = n_W; y < noisy_im_p.getExtents()[1] - n_W; y++) {
			for (x = n_W; x < noisy_im_p.getExtents()[0] - n_W; x++) {
				img_denoised_arr[(y-n_W)*srcImage.getExtents()[0] + (x-n_W)] = img_denoised_pad[y][x];
			}
		}
		
		noisy_im_p.disposeLocal();
		noisy_im_p = null;
		img_basic_p.disposeLocal();
		img_basic_p = null;
		
		try {
			destImage[1].importData(0, img_denoised_arr, true);
		}
		catch (IOException e) {
			MipavUtil.displayError("IOException on destImage[1].importData(0, img_denoised_arr, true)");
			setCompleted(false);
			return;
		}
		
		setCompleted(true);
		return;
	}
	
	private double[][] bm3d_2nd_step(double sigma, ModelImage img_noisy, ModelImage img_basic, int nWien, int kWien, 
			int NWien, int pWien, double tauMatch, boolean useSD, String tau_2D) {
		int ii,jj,h,i,j,k,m,n,w,x,y;
		int acc_pointer, i_r, j_r, nSx_r;
		int ni,nj;
		double group_3D_img[][][];
		double group_3D_est[][][];
		double group_3D[][][];
		double group_3D_transpose[][][];
		double weight[] = new double[1];
	    int height = img_noisy.getExtents()[1];
	    int width = img_noisy.getExtents()[0];
	    int row_ind[] = ind_initialize(height - kWien + 1, nWien, pWien);
	    int column_ind[] = ind_initialize(width - kWien + 1, nWien, pWien);
	    
	    double kaiserWindow[][] = get_kaiserWindow(kWien);
	    int threshold_count[][] = new int[height][width];
	    int ri_rj_N__ni_nj[][][][] = precompute_BM(img_basic, kWien, NWien, nWien, tauMatch, threshold_count);
	    int group_len = 0;
	    for (h = 0; h < height; h++) {
	    	for (w = 0; w < width; w++) {
	    		group_len += threshold_count[h][w];
	    	}
	    }
	    double group_3D_table[][][] = new double[group_len][kWien][kWien];
	    double weight_table[][] = new double[height][width];
	    
	    // i_j_ipatch_jpatch__v
	    Vector<Vector<double[][]>>noisy_patches = image2patches(img_noisy, kWien, kWien);  
	    // i_j_ipatch_jpatch__v
	    Vector<Vector<double[][]>>basic_patches = image2patches(img_basic, kWien, kWien); 
	    Vector<Vector<double[][]>>fre_noisy_patches = new Vector<Vector<double[][]>>();
	    Vector<Vector<double[][]>>fre_basic_patches = new Vector<Vector<double[][]>>();
	    for (ii = 0; ii < noisy_patches.size(); ii++) {
	    	Vector<double[][]> subVector = noisy_patches.get(ii);
	    	Vector<double[][]> subfreVector = new Vector<double[][]>();
	    	for (jj = 0; jj < subVector.size(); jj++) {
	            double current_patch[][] = subVector.get(jj);
		    	if (tau_2D.equalsIgnoreCase("DCT")) {
		    		// Forward 2D Discrete Cosine Transform
		    		double new_patch_1[][] = new double[current_patch.length][current_patch[0].length];
		    		double new_patch_2[][] = new double[current_patch.length][current_patch[0].length];
		    		double yk;
		    		for (i = 0; i < current_patch[0].length; i++) {
		    			for (k = 0; k < current_patch.length; k++) {
		    				yk = 0.0;
		    			    for (n = 0; n < current_patch.length; n++) {
		    			        yk += 2.0*current_patch[n][i]*Math.cos(Math.PI*k*(2*n+1)/(2.0*current_patch.length));
		    			    }
		    			    if (k == 0) {
	    			        	yk = yk * Math.sqrt(1.0/(4.0*current_patch.length));
	    			        }
	    			        else {
	    			        	yk = yk * Math.sqrt(1.0/(2.0*current_patch.length));
	    			        }
	    			        new_patch_1[k][i] = yk;
		    			}
		    		}
		    		for (i = 0; i < current_patch.length; i++) {
		    			for (k = 0; k < current_patch[0].length; k++) {
		    				yk = 0;
		    				for (n = 0; n < current_patch[0].length; n++) {
		    					yk += 2.0*new_patch_1[i][n]*Math.cos(Math.PI*k*(2*n+1)/(2.0*current_patch[0].length));
		    				}
		    				if (k == 0) {
	    			        	yk = yk * Math.sqrt(1.0/(4.0*current_patch[0].length));
	    			        }
	    			        else {
	    			        	yk = yk * Math.sqrt(1.0/(2.0*current_patch[0].length));
	    			        }
	    			        new_patch_2[i][k] = yk;
		    			}
		    		}
		    		subfreVector.add(new_patch_2);
		    	} // if (tau_2D.equalsIgnoreCase("DCT"))
		    	
		    	else { // "BIOR"
		    		if (current_patch.length != current_patch[0].length) {
		    			System.err.println("current_patch.length != current_patch[0],length as required for BIOR");
		    			return null;
		    		}
		    		int iter_max = (int)log2(current_patch.length);
		    		PyWavelets py = new PyWavelets();
		    		double coeffs[][][] = py.BM3Dwavedec2(current_patch, iter_max);
		    		double waveim[][] = new double[current_patch.length][current_patch.length];
		    		
		    		int N = 1;
		    		waveim[0][0] = coeffs[0][0][0];
		    		for (i = 0; i < iter_max; i++) {
		    			for (y = N; y < 2*N; y++) {
		    				for (x = N; x < 2*N; x++) {
		    					waveim[y][x] = coeffs[3*i+3][y-N][x-N];
		    				}
		    			}
		    			for (y = 0; y < N; y++) {
		    				for (x = N; x < 2*N; x++) {
		    					waveim[y][x] = -coeffs[3*i+2][y][x-N];
		    				}
		    			}
		    			for (y = N; y < 2*N; y++) {
		    				for (x = 0; x < N; x++) {
		    					waveim[y][x] = -coeffs[3*i+1][y-N][x];
		    				}
		    			}
		    			N *= 2;
		    		} // for (i = 0; i < iter_max; i++)
		    	    subfreVector.add(waveim);
		    	} // else "BIOR"
	    	} // for (jj = 0; jj < subVector.size(); jj++)
	    	fre_noisy_patches.add(subfreVector);
	    } // for (ii = 0; ii < noisy_patches.size(); ii++)
	    
	    for (ii = 0; ii < basic_patches.size(); ii++) {
	    	Vector<double[][]> subVector = basic_patches.get(ii);
	    	Vector<double[][]> subfreVector = new Vector<double[][]>();
	    	for (jj = 0; jj < subVector.size(); jj++) {
	            double current_patch[][] = subVector.get(jj);
		    	if (tau_2D.equalsIgnoreCase("DCT")) {
		    		// Forward 2D Discrete Cosine Transform
		    		double new_patch_1[][] = new double[current_patch.length][current_patch[0].length];
		    		double new_patch_2[][] = new double[current_patch.length][current_patch[0].length];
		    		double yk;
		    		for (i = 0; i < current_patch[0].length; i++) {
		    			for (k = 0; k < current_patch.length; k++) {
		    				yk = 0.0;
		    			    for (n = 0; n < current_patch.length; n++) {
		    			        yk += 2.0*current_patch[n][i]*Math.cos(Math.PI*k*(2*n+1)/(2.0*current_patch.length));
		    			    }
		    			    if (k == 0) {
	    			        	yk = yk * Math.sqrt(1.0/(4.0*current_patch.length));
	    			        }
	    			        else {
	    			        	yk = yk * Math.sqrt(1.0/(2.0*current_patch.length));
	    			        }
	    			        new_patch_1[k][i] = yk;
		    			}
		    		}
		    		for (i = 0; i < current_patch.length; i++) {
		    			for (k = 0; k < current_patch[0].length; k++) {
		    				yk = 0;
		    				for (n = 0; n < current_patch[0].length; n++) {
		    					yk += 2.0*new_patch_1[i][n]*Math.cos(Math.PI*k*(2*n+1)/(2.0*current_patch[0].length));
		    				}
		    				if (k == 0) {
	    			        	yk = yk * Math.sqrt(1.0/(4.0*current_patch[0].length));
	    			        }
	    			        else {
	    			        	yk = yk * Math.sqrt(1.0/(2.0*current_patch[0].length));
	    			        }
	    			        new_patch_2[i][k] = yk;
		    			}
		    		}
		    		subfreVector.add(new_patch_2);
		    	} // if (tau_2D.equalsIgnoreCase("DCT"))
		    	
		    	else { // "BIOR"
		    		if (current_patch.length != current_patch[0].length) {
		    			System.err.println("current_patch.length != current_patch[0],length as required for BIOR");
		    			return null;
		    		}
		    		int iter_max = (int)log2(current_patch.length);
		    		PyWavelets py = new PyWavelets();
		    		double coeffs[][][] = py.BM3Dwavedec2(current_patch, iter_max);
		    		double waveim[][] = new double[current_patch.length][current_patch.length];
		    		
		    		int N = 1;
		    		waveim[0][0] = coeffs[0][0][0];
		    		for (i = 0; i < iter_max; i++) {
		    			for (y = N; y < 2*N; y++) {
		    				for (x = N; x < 2*N; x++) {
		    					waveim[y][x] = coeffs[3*i+3][y-N][x-N];
		    				}
		    			}
		    			for (y = 0; y < N; y++) {
		    				for (x = N; x < 2*N; x++) {
		    					waveim[y][x] = -coeffs[3*i+2][y][x-N];
		    				}
		    			}
		    			for (y = N; y < 2*N; y++) {
		    				for (x = 0; x < N; x++) {
		    					waveim[y][x] = -coeffs[3*i+1][y-N][x];
		    				}
		    			}
		    			N *= 2;
		    		} // for (i = 0; i < iter_max; i++)
		    		
		    	    subfreVector.add(waveim);
		    	} // else "BIOR"
	    	} // for (jj = 0; jj < subVector.size(); jj++)
	    	fre_basic_patches.add(subfreVector);
	    } // for (ii = 0; ii < basic_patches.size(); ii++)
	    
	    acc_pointer = 0;
	    for (i = 0; i < row_ind.length; i++) {
	    	i_r = row_ind[i];
	    	for (j = 0; j < column_ind.length; j++) {
	    		j_r = column_ind[j];
	    		nSx_r = threshold_count[i_r][j_r];
	    		group_3D_img = build_3D_group(fre_noisy_patches, ri_rj_N__ni_nj[i_r][j_r], nSx_r);
	    		group_3D_est = build_3D_group(fre_basic_patches, ri_rj_N__ni_nj[i_r][j_r], nSx_r);
	    		group_3D = wiener_filtering_hadamard(group_3D_img, group_3D_est, sigma, !useSD,weight);
	    		group_3D_transpose = new double[nSx_r][group_3D.length][group_3D.length];
	    		for (k = acc_pointer; k < acc_pointer + nSx_r; k++) {
	    			for (m = 0; m < group_3D.length; m++) {
	    				for (n = 0; n < group_3D.length; n++) {
	    					group_3D_table[k][m][n] = group_3D[m][n][k-acc_pointer];
	    					group_3D_transpose[k-acc_pointer][m][n] = group_3D[m][n][k-acc_pointer];
	    				}
	    			}
	    		}
	    		acc_pointer += nSx_r;
	    		
	    		if (useSD) {
	                weight[0] = sd_weighting(group_3D_transpose);
	    		}
	    		
	    		weight_table[i_r][j_r] = weight[0];
	    	} // for (j = 0; j < column_ind.length; j++)
	    } // for (i = 0; i < row_ind.length; i++)
	    
	   for (ii = 0; ii < group_len; ii++) {
		    if (tau_2D.equalsIgnoreCase("DCT")) {
	    		// Reverse 2D Discrete Cosine Transform
	    		double new_patch_1[][] = new double[kWien][kWien];
	    		double yk;
	    		for (i = 0; i < kWien; i++) {
	    			for (k = 0; k < kWien; k++) {
	    				yk = group_3D_table[ii][0][i]/Math.sqrt(kWien);
	    			    for (n = 1; n < kWien; n++) {
	    			        yk += Math.sqrt(2.0/kWien) * group_3D_table[ii][n][i]*Math.cos(Math.PI*n*(2*k+1)/(2.0*kWien));  
	    			    }
				        new_patch_1[k][i] = yk;
	    			}
	    		}
	    		for (i = 0; i < kWien; i++) {
	    			for (k = 0; k < kWien; k++) {
	    				yk = new_patch_1[i][0]/Math.sqrt(kWien);
	    				for (n = 1; n < kWien; n++) {
	    					yk += Math.sqrt(2.0/kWien) * new_patch_1[i][n]*Math.cos(Math.PI*n*(2*k+1)/(2.0*kWien));
	    				}
				        group_3D_table[ii][i][k] = yk;
	    			}
	    		}
	    	} // if (tau_2D.equalsIgnoreCase("DCT"))
	    	else { // "BIOR"
	    		PyWavelets py = new PyWavelets();
	    		int level = (int)log2(kWien);
	    		double group_2D[][] = py.BM3Dwaverec2(group_3D_table[ii],level);
	    		for (j = 0; j < kWien; j++) {
	    			for (k = 0; k < kWien; k++) {
	    				group_3D_table[ii][j][k] = group_2D[j][k];
	    			}
	    		}
	    	} // else "BIOR"
	   } // for (ii = 0; ii < group_len; ii++)
	    
	    // for i in range(1000):
        //     patch = group_3D_table[i]
        //     print(i, '----------------------------')
        //     print(patch)
        //     cv2.imshow('', patch.astype(np.uint8))
        //     cv2.waitKey()

        // aggregation part
	    double numerator[][] = new double[height][width];
	    double denominator[][] = new double[height][width];
	    for (i = 0; i < nWien; i++) {
	    	for (j = 0; j < width; j++) {
	    		denominator[i][j] = 1.0;
	    	}
	    }
	    for (i = height - nWien; i < height; i++) {
	    	for (j = 0; j < width; j++) {
	    		denominator[i][j] = 1.0;
	    	}
	    }
	    for (i = nWien; i < height - nWien; i++) {
	    	for (j = 0; j < nWien; j++) {
	    		denominator[i][j] = 1.0;
	    	}
	    }
	    
	    for (i = nWien; i < height - nWien; i++) {
	    	for (j = width - nWien; j < width; j++) {
	    		denominator[i][j] = 1.0;
	    	}
	    }
	    acc_pointer = 0;
	    double patch[][];
	    for (i = 0; i < row_ind.length; i++) {
	    	i_r = row_ind[i];
	    	for (j = 0; j < column_ind.length; j++) {
	    		j_r = column_ind[j];
	    		nSx_r = threshold_count[i_r][j_r];
	    		int N_ni_nj[][] = ri_rj_N__ni_nj[i_r][j_r];
	    		group_3D = new double[nSx_r][kWien][kWien];
	    		for (k = acc_pointer; k < acc_pointer + nSx_r; k++) {
	    			for (m = 0; m < kWien; m++) {
	    				for (n = 0; n < kWien; n++) {
	    					group_3D[k-acc_pointer][m][n] = group_3D_table[k][m][n];
	    				}
	    			}
	    		}
	    		acc_pointer += nSx_r;
	    		weight[0] = weight_table[i_r][j_r];
	    		for (n = 0; n < nSx_r; n++) {
	    			ni = N_ni_nj[n][0];
	    			nj = N_ni_nj[n][1];
	    			patch = group_3D[n];
	    			
	    			for (k = 0; k < kWien; k++) {
	    				for (m = 0; m < kWien; m++) {
	    					numerator[ni + k][nj + m] += patch[k][m] * kaiserWindow[k][m] * weight[0];
	    					denominator[ni + k][nj + m] += kaiserWindow[k][m] * weight[0];
	    				}
	    			}
	    			
	    		} // for (n = 0; n < nSx_r; n++)
	    	} // for (j = 0; j < column_ind.length; j++)
	    } // for (i = 0; i < row_ind.length; i++)
	    double img_denoised_pad[][] = new double[height][width];
	    for (h = 0; h < height; h++) {
	    	for (w = 0; w < width; w++) {
	    		img_denoised_pad[h][w] = numerator[h][w]/denominator[h][w];
	    	}
	    }
	    return img_denoised_pad;
	}
	
	private double[][][] wiener_filtering_hadamard(double group_3D_img[][][], double group_3D_est[][][], double sigma,
			boolean doWeight, double weight[]) {
		// wiener_filtering after hadamard transform
		int i,j,k;
		double value;
		int n = group_3D_img.length;
		int nSx_r = group_3D_img[0][0].length;
		double group_3D_img_h[][][] = new double[n][n][];
		double group_3D_est_h[][][] = new double[n][n][];
	    double coef = 1.0 / nSx_r;
	    WalshHadamardTransform3 wht3 = new WalshHadamardTransform3();
	    
        for (j = 0; j < n; j++) {
        	for (k = 0; k < n; k++) {
        		// false means don't multiply by 1.0/ nSx_r
        		group_3D_img_h[j][k] = wht3.fhtnat(group_3D_img[j][k], false);
        		group_3D_est_h[j][k] = wht3.fhtnat(group_3D_est[j][k], false);
        	}
        }
	         
	    // wiener filtering in this block
	    weight[0] = 0.0;
	    for (i = 0; i < nSx_r; i++) {
	        for (j = 0; j < n; j++) {
	        	for (k = 0; k < n; k++) {
	        		value = group_3D_est_h[j][k][i] * group_3D_est_h[j][k][i] * coef;
	        		value /= (value + sigma * sigma);
	        		group_3D_est_h[j][k][i] = group_3D_img_h[j][k][i] * value * coef;
	        		weight[0] += value;
	        	}
	        }
	    }
	    
	    for (j = 0; j < n; j++) {
        	for (k = 0; k < n; k++) {
        		// false means don't multiply by 1.0/ nSx_r
        		group_3D_est[j][k] = wht3.fhtnat(group_3D_est_h[j][k], false);
        	}
        }
	    
	    if (doWeight) {
	        if (weight[0] > 0.0) {
	        	weight[0] = 1. / (sigma * sigma * weight[0]);	
	        }
	        else {
	        	weight[0] = 1.0;
	        }
	    } // if (doWeight)
	    return group_3D_est;	
	}
	
	private ModelImage symetrize(ModelImage img, int pad) {
		int x,y;
		int xDim = img.getExtents()[0];
		int yDim = img.getExtents()[1];
		int length = xDim * yDim;
		double buf[] = new double[length];
		try {
			img.exportData(0, length, buf);
		}
		catch (IOException e) {
			MipavUtil.displayError("IOException on img.exportData(0, length, buf)");
			System.exit(-1);
		}
		
		// Symmetric reflection padding
		int paddedXDim = xDim + 2*pad;
		int paddedYDim = yDim + 2*pad;
		int paddedLength = paddedXDim * paddedYDim;
		double padBuf[] = new double[paddedLength];
		for (y = 0; y < pad; y++) {
			for (x = 0; x < pad; x++) {
				padBuf[y*paddedXDim + x] = buf[(pad - 1 - y)*xDim + (pad - 1 - x)];
			}
			
			for (x = pad; x < xDim + pad; x++) {
				padBuf[y*paddedXDim + x] = buf[(pad - 1 - y)*xDim + (x - pad)]; 
			}
			
			for (x = xDim + pad; x < xDim + 2*pad; x++) {
				padBuf[y*paddedXDim + x] = buf[(pad - 1 - y)*xDim + 2*xDim + pad - 1 - x];
			}
		}
		
		for (y = yDim + pad; y < yDim + 2*pad; y++) {
			for (x = 0; x < pad; x++) {
				padBuf[y*paddedXDim + x] = buf[(2*yDim + pad - 1 - y)*xDim + (pad - 1 - x)];
			}
			
			for (x = pad; x < xDim + pad; x++) {
				padBuf[y*paddedXDim + x] = buf[(2*yDim + pad - 1 - y)*xDim + (x - pad)]; 
			}
			
			for (x = xDim + pad; x < xDim + 2*pad; x++) {
				padBuf[y*paddedXDim + x] = buf[(2*yDim + pad - 1 - y)*xDim + 2*xDim + pad - 1 - x];
			}
		}
		
		for (y = pad; y < yDim + pad; y++) {
			for (x = 0; x < pad; x++) {
				padBuf[y*paddedXDim + x] = buf[(y - pad)*xDim + (pad - 1 - x)];
			}
			for (x = pad; x < xDim + pad; x++) {
				padBuf[y*paddedXDim + x] = buf[(y - pad)*xDim + x - pad];
			}
			for (x = xDim + pad; x < xDim + 2*pad; x++) {
				padBuf[y*paddedXDim + x] = buf[(y - pad)*xDim + 2*xDim + pad - 1 - x];
			}
		}
		
		int paddedExtents[] = new int[] {paddedXDim, paddedYDim};
		ModelImage padImage = new ModelImage(ModelStorageBase.DOUBLE, paddedExtents, "padImage");
		try {
			padImage.importData(0, padBuf, true);
		}
		catch(IOException e) {
			MipavUtil.displayError("IOException on padImage.importData(0, padBuf, true)");
			System.exit(-1);
		}
		return padImage;
	}
	
	private double[][] bm3d_1st_step(double sigma, ModelImage img_noisy, int nHard, int kHard, int NHard, int pHard, double lambdaHard3D,
			double tauMatch, boolean useSD, String tau_2D) {
		int i,ii,h,j,jj,k,m,n,w,acc_pointer, i_r, j_r,x,y;
		int nSx_r;
		int ni, nj;
		double group_3D[][][];
		double group_3D_transpose[][][];
		double weight[] = new double[1];
	    int width = img_noisy.getExtents()[0];
	    int height = img_noisy.getExtents()[1];
	    
	    int row_ind[] = ind_initialize(height - kHard + 1, nHard, pHard);
	    int column_ind[] = ind_initialize(width - kHard + 1, nHard, pHard);
	    
	    double kaiserWindow[][] = get_kaiserWindow(kHard);
	    int threshold_count[][] = new int[height][width];
	    int ri_rj_N__ni_nj[][][][] = precompute_BM(img_noisy, kHard, NHard, nHard, tauMatch, threshold_count);
	    int group_len = 0;
	    for (h = 0; h < height; h++) {
	    	for (w = 0; w < width; w++) {
	    		group_len += threshold_count[h][w];
	    	}
	    }
	    double group_3D_table[][][] = new double[group_len][kHard][kHard];
	    double weight_table[][] = new double[height][width];
	    
	    // i_j_ipatch_jpatch__v
	    Vector<Vector<double[][]>>all_patches = image2patches(img_noisy, kHard, kHard);
	    Vector<Vector<double[][]>>fre_all_patches = new Vector<Vector<double[][]>>();
	    for (ii = 0; ii < all_patches.size(); ii++) {
	    	Vector<double[][]> subVector = all_patches.get(ii);
	    	Vector<double[][]> subfreVector = new Vector<double[][]>();
	    	for (jj = 0; jj < subVector.size(); jj++) {
	            double current_patch[][] = subVector.get(jj);
		    	if (tau_2D.equalsIgnoreCase("DCT")) {
		    		// Forward 2D Discrete Cosine Transform
		    		double new_patch_1[][] = new double[current_patch.length][current_patch[0].length];
		    		double new_patch_2[][] = new double[current_patch.length][current_patch[0].length];
		    		double yk;
		    		for (i = 0; i < current_patch[0].length; i++) {
		    			for (k = 0; k < current_patch.length; k++) {
		    				yk = 0.0;
		    			    for (n = 0; n < current_patch.length; n++) {
		    			        yk += 2.0*current_patch[n][i]*Math.cos(Math.PI*k*(2*n+1)/(2.0*current_patch.length));
		    			    }
		    			    if (k == 0) {
	    			        	yk = yk * Math.sqrt(1.0/(4.0*current_patch.length));
	    			        }
	    			        else {
	    			        	yk = yk * Math.sqrt(1.0/(2.0*current_patch.length));
	    			        }
	    			        new_patch_1[k][i] = yk;
		    			}
		    		}
		    		for (i = 0; i < current_patch.length; i++) {
		    			for (k = 0; k < current_patch[0].length; k++) {
		    				yk = 0;
		    				for (n = 0; n < current_patch[0].length; n++) {
		    					yk += 2.0*new_patch_1[i][n]*Math.cos(Math.PI*k*(2*n+1)/(2.0*current_patch[0].length));
		    				}
		    				if (k == 0) {
	    			        	yk = yk * Math.sqrt(1.0/(4.0*current_patch[0].length));
	    			        }
	    			        else {
	    			        	yk = yk * Math.sqrt(1.0/(2.0*current_patch[0].length));
	    			        }
	    			        new_patch_2[i][k] = yk;
		    			}
		    		}
		    		subfreVector.add(new_patch_2);
		    	} // if (tau_2D.equalsIgnoreCase("DCT"))
		    	
		    	else { // "BIOR"
		    		if (current_patch.length != current_patch[0].length) {
		    			System.err.println("current_patch.length != current_patch[0],length as required for BIOR");
		    			return null;
		    		}
		    		int iter_max = (int)log2(current_patch.length);
		    		PyWavelets py = new PyWavelets();
		    		double coeffs[][][] = py.BM3Dwavedec2(current_patch, iter_max);
		    		double waveim[][] = new double[current_patch.length][current_patch.length];
		    		
		    		int N = 1;
		    		waveim[0][0] = coeffs[0][0][0];
		    		for (i = 0; i < iter_max; i++) {
		    			for (y = N; y < 2*N; y++) {
		    				for (x = N; x < 2*N; x++) {
		    					waveim[y][x] = coeffs[3*i+3][y-N][x-N];
		    				}
		    			}
		    			for (y = 0; y < N; y++) {
		    				for (x = N; x < 2*N; x++) {
		    					waveim[y][x] = -coeffs[3*i+2][y][x-N];
		    				}
		    			}
		    			for (y = N; y < 2*N; y++) {
		    				for (x = 0; x < N; x++) {
		    					waveim[y][x] = -coeffs[3*i+1][y-N][x];
		    				}
		    			}
		    			N *= 2;
		    		} // for (i = 0; i < iter_max; i++)
		    		
		    	    subfreVector.add(waveim);
		    	} // else "BIOR"
	    	} // for (jj = 0; jj < subVector.size(); jj++)
	    	fre_all_patches.add(subfreVector);
	    } // for (ii = 0; ii < all_patches.size(); ii++)
	    
	    acc_pointer = 0;
	    for (i = 0; i < row_ind.length; i++) {
	    	i_r = row_ind[i];
	    	for (j = 0; j < column_ind.length; j++) {
	    		j_r = column_ind[j];
	    		nSx_r = threshold_count[i_r][j_r];
	    		group_3D = build_3D_group(fre_all_patches, ri_rj_N__ni_nj[i_r][j_r], nSx_r);
	    		group_3D = ht_filtering_hadamard(group_3D, sigma, lambdaHard3D, !useSD, weight);
	    		group_3D_transpose = new double[nSx_r][group_3D.length][group_3D.length];
	    		for (k = acc_pointer; k < acc_pointer + nSx_r; k++) {
	    			for (m = 0; m < group_3D.length; m++) {
	    				for (n = 0; n < group_3D.length; n++) {
	    					group_3D_table[k][m][n] = group_3D[m][n][k-acc_pointer];
	    					group_3D_transpose[k-acc_pointer][m][n] = group_3D[m][n][k-acc_pointer];
	    				}
	    			}
	    		}
	    		acc_pointer += nSx_r;
	    		
	    		if (useSD) {
	                weight[0] = sd_weighting(group_3D_transpose);
	    		}
	    		
	    		weight_table[i_r][j_r] = weight[0];
	    	} // for (j = 0; j < column_ind.length; j++)
	    } // for (i = 0; i < row_ind.length; i++)
	    
	    
	    for (ii = 0; ii < group_len; ii++) {	
	        if (tau_2D.equalsIgnoreCase("DCT")) {
	    		// Reverse 2D Discrete Cosine Transform
	    		double new_patch_1[][] = new double[kHard][kHard];
	    		double yk;
	    		for (i = 0; i < kHard; i++) {
	    			for (k = 0; k < kHard; k++) {
	    				yk = group_3D_table[ii][0][i]/Math.sqrt(kHard);
	    			    for (n = 1; n < kHard; n++) {
	    			        yk += Math.sqrt(2.0/kHard) * group_3D_table[ii][n][i]*Math.cos(Math.PI*n*(2*k+1)/(2.0*kHard));  
	    			    }
    			        new_patch_1[k][i] = yk;
	    			}
	    		}
	    		for (i = 0; i < kHard; i++) {
	    			for (k = 0; k < kHard; k++) {
	    				yk = new_patch_1[i][0]/Math.sqrt(kHard);
	    				for (n = 1; n < kHard; n++) {
	    					yk += Math.sqrt(2.0/kHard) * new_patch_1[i][n]*Math.cos(Math.PI*n*(2*k+1)/(2.0*kHard));
	    				}
    			        group_3D_table[ii][i][k] = yk;
	    			}
	    		}
	    	} // if (tau_2D.equalsIgnoreCase("DCT"))
	    	else { // "BIOR"
	    		PyWavelets py = new PyWavelets();
	    		int level = (int)log2(kHard);
	    		double group_2D[][] = py.BM3Dwaverec2(group_3D_table[ii],level);
	    		for (j = 0; j < kHard; j++) {
	    			for (k = 0; k < kHard; k++) {
	    				group_3D_table[ii][j][k] = group_2D[j][k];
	    			}
	    		}
	    	} // else "BIOR"
	    } // for (ii = 0; ii < group_len; ii++)
	    	
    	// group_3D_table = np.maximum(group_3D_table, 0)
	    // for i in range(1000):
	    //     patch = group_3D_table[i]
	    //     print(i, '----------------------------')
	    //     print(patch)
	    //     print(np.min(patch))
	    //     print(np.max(patch))
	    //     print(np.sum(patch))
	    //     cv2.imshow('', patch.astype(np.uint8))
	    //     cv2.waitKey()

	    // aggregation part
	    double numerator[][] = new double[height][width];
	    double denominator[][] = new double[height][width];
	    for (i = 0; i < nHard; i++) {
	    	for (j = 0; j < width; j++) {
	    		denominator[i][j] = 1.0;
	    	}
	    }
	    for (i = height - nHard; i < height; i++) {
	    	for (j = 0; j < width; j++) {
	    		denominator[i][j] = 1.0;
	    	}
	    }
	    for (i = nHard; i < height - nHard; i++) {
	    	for (j = 0; j < nHard; j++) {
	    		denominator[i][j] = 1.0;
	    	}
	    }
	    
	    for (i = nHard; i < height - nHard; i++) {
	    	for (j = width - nHard; j < width; j++) {
	    		denominator[i][j] = 1.0;
	    	}
	    }
	    acc_pointer = 0;
	    double patch[][];
	    for (i = 0; i < row_ind.length; i++) {
	    	i_r = row_ind[i];
	    	for (j = 0; j < column_ind.length; j++) {
	    		j_r = column_ind[j];
	    		nSx_r = threshold_count[i_r][j_r];
	    		int N_ni_nj[][] = ri_rj_N__ni_nj[i_r][j_r];
	    		group_3D = new double[nSx_r][kHard][kHard];
	    		for (k = acc_pointer; k < acc_pointer + nSx_r; k++) {
	    			for (m = 0; m < kHard; m++) {
	    				for (n = 0; n < kHard; n++) {
	    					group_3D[k-acc_pointer][m][n] = group_3D_table[k][m][n];
	    				}
	    			}
	    		}
	    		acc_pointer += nSx_r;
	    		weight[0] = weight_table[i_r][j_r];
	    		for (n = 0; n < nSx_r; n++) {
	    			ni = N_ni_nj[n][0];
	    			nj = N_ni_nj[n][1];
	    			patch = group_3D[n];
	    			
	    			for (k = 0; k < kHard; k++) {
	    				for (m = 0; m < kHard; m++) {
	    					numerator[ni + k][nj + m] += patch[k][m] * kaiserWindow[k][m] * weight[0];
	    					denominator[ni + k][nj + m] += kaiserWindow[k][m] * weight[0];
	    				}
	    			}
	    			
	    		} // for (n = 0; n < nSx_r; n++)
	    	} // for (j = 0; j < column_ind.length; j++)
	    } // for (i = 0; i < row_ind.length; i++)
	    double img_basic[][] = new double[height][width];
	    for (h = 0; h < height; h++) {
	    	for (w = 0; w < width; w++) {
	    		img_basic[h][w] = numerator[h][w]/denominator[h][w];
	    	}
	    }
	    return img_basic;
	}
	
	private double sd_weighting(double group_3D[][][]) {
		int i,j,k;
	    int N = group_3D.length * group_3D[0].length * group_3D[0][0].length;
	    
	    double mean = 0.0;
	    double std = 0.0;
	    for (i = 0; i < group_3D.length; i++) {
	    	for (j = 0; j < group_3D[0].length; j++) {
	    		for (k = 0; k < group_3D[0][0].length; k++) {
	    			mean += group_3D[i][j][k];
	    			std += (group_3D[i][j][k] * group_3D[i][j][k]);
	    		}
	    	}
	    }

	    double res = (std - mean * mean / N) / (N - 1.0);
	    double weight = 0.0;
	    if (res > 0.0) {
	    	weight = 1.0/Math.sqrt(res);
	    }
	    return weight;
	}
	
	private double[][][] ht_filtering_hadamard(double group_3D[][][], double sigma, 
			double lambdaHard3D, boolean doWeight, double weight[]) {
		// hard threshold filtering after 1D hadamard transform
		// group_3D shape=(n*n, nSx_r)
		int i,j,k;
		int n = group_3D.length;
		int nSx_r = group_3D[0][0].length;
		double group_3D_h[][][] = new double[n][n][];
		double coef_norm = Math.sqrt(nSx_r);
	    WalshHadamardTransform3 wht3 = new WalshHadamardTransform3();
	    
        for (j = 0; j < n; j++) {
        	for (k = 0; k < n; k++) {
        		// false means don't multiply by 1.0/ nSX_r during transform 
        		group_3D_h[j][k] = wht3.fhtnat(group_3D[j][k], false);
        	}
        }
	    
	    double T = lambdaHard3D * sigma * coef_norm;
	    weight[0] = 0.0;
	    for (i = 0; i < nSx_r; i++) {
	        for (j = 0; j < n; j++) {
	        	for (k = 0; k < n; k++) {
	        		if (Math.abs(group_3D_h[j][k][i]) > T) {
	        			weight[0] += 1.0;
	        		}
	        		else {
	        			group_3D_h[j][k][i] = 0.0;
	        		}
	        	}
	        }
	    } // for (i = 0; i < nSx_r; i++)
	    
	    for (j = 0; j < n; j++) {
        	for (k = 0; k < n; k++) {
        		// true means multiply by 1.0/ nSX_r during transform
        		group_3D[j][k] = wht3.fhtnat(group_3D_h[j][k], true);
        	}
        }
	    
	    if (doWeight) {
	        if (weight[0] > 0.0) {
	        	weight[0] = 1. / (sigma * sigma * weight[0]);	
	        }
	        else {
	        	weight[0] = 1.0;
	        }
	    } // if (doWeight)
	    return group_3D;
	}
	
	private double[][][] build_3D_group(Vector<Vector<double[][]>> fre_all_patches, int N__ni_nj[][], int nSx_r) {
		// stack frequency patches into a 3D block
	    // fre_all_patches: all frequency patches
	    // N__ni_nj: the position of the N most similar patches
	    // nSx_r: how many similar patches according to threshold
	    // return: the 3D block
		int i, j, n, ni, nj;
		int k = fre_all_patches.get(0).get(0).length;
		for (i = 0; i < fre_all_patches.size(); i++) {
			Vector<double[][]>subVector = fre_all_patches.get(i);
			for (j = 0; j < subVector.size(); j++) {
				double patch[][] = subVector.get(j);
				if ((patch.length != k) || (patch[0].length != k)) {
					System.err.println("In build_3D_group not all patch.length and patch[0].length are the same");
					return null;
				}
			}
		}
		double group_3D[][][] = new double[nSx_r][][];
		for (n = 0; n < nSx_r; n++) {
			ni = N__ni_nj[n][0];
			nj = N__ni_nj[n][1];
			group_3D[n] = fre_all_patches.get(ni).get(nj);
		}
		double group_3D_transpose[][][] = new double[k][k][nSx_r];
		for (i = 0; i < k; i++) {
			for (j = 0; j < k; j++) {
				for (n = 0; n < nSx_r; n++) {
					group_3D_transpose[i][j][n] = group_3D[n][i][j];
				}
			}
		}
		return group_3D_transpose;
	}
	
	private double log2(double x) {
    	return (Math.log(x)/Math.log(2));
    }
	
	private Vector<Vector<double[][]>> image2patches(ModelImage im, int patch_h, int patch_w) {
	    
	    // cut the image into patches
		// BIOR requires square patches, that is patch_h = patch_w
		
		int i,j,k,m,r,s;
	    int im_w = im.getExtents()[0];
	    int im_h = im.getExtents()[1];
	    int length = im_w * im_h;
		double buf[] = new double[length];
		try {
			im.exportData(0, length, buf);
		}
		catch (IOException e) {
			MipavUtil.displayError("IOException on im.exportData(0, length, buf) in image2patches");
			System.exit(-1);
		}
		double img_buf[][] = new double[im_h][im_w];
		for (i = 0; i < im_h; i++) {
			for (j = 0; j < im_w; j++) {
				img_buf[i][j] = buf[i*im_w + j];
			}
		}
	    
	    Vector<Vector<double[][]>> imVector= new Vector<Vector<double[][]>>();
	    for (i = 0; i < (im_h - patch_h + 1); i++) {
	    	Vector<double[][]> subVector = new Vector<double[][]>();
	    	for (j = 0; j < (im_w - patch_w + 1); j++) {
    				double patch[][] = new double[patch_h][patch_w];
    				for (r = i; r < i+patch_h; r++) {
    					for (s = j; s < j+patch_w; s++) {
    						patch[r-i][s-j] = img_buf[r][s];
    					}
    				}
    				subVector.add(patch);
	    	}
	    	imVector.add(subVector);
	    }
	    return imVector;
	}
	
	private int[][][][] precompute_BM(ModelImage img, int kHW, int NHW, int nHW, double tauMatch, int[][] threshold_count) {
		// search for similar patches
	    // img: input image type ModelStorageBase.DOUBLE
	    // kHW: length of side of patch
	    // NHW: how many patches are stacked
	    // nHW: length of side of search area
	    // tauMatch: threshold determine whether two patches are similar
	    // return ri_rj_N__ni_nj: The top N most similar patches to the referred patch
	    // return threshold_count: according to tauMatch how many patches are similar to the referred one
		int i,j,h,w,di,dj;
		int width = img.getExtents()[0];
		int height = img.getExtents()[1];
		int length = width * height;
		double buf[] = new double[length];
		try {
			img.exportData(0, length, buf);
		}
		catch (IOException e) {
			MipavUtil.displayError("IOException on img.exportData(0, length, buf) in precompute_BM");
			System.exit(-1);
		}
		double img_buf[][] = new double[height][width];
		for (i = 0; i < height; i++) {
			for (j = 0; j < width; j++) {
				img_buf[i][j] = buf[i*width + j];
			}
		}
		int Ns = 2 * nHW + 1;
	    double threshold = tauMatch * kHW * kHW;
	    // di, dj, ph, pw
	    double sum_table[][][][] = new double[Ns][Ns][height][width];
	    for (i = 0; i < Ns; i++) {
	    	for (j = 0; j < Ns; j++) {
	    		for (h = 0; h < height; h++) {
	    			for (w = 0; w < width; w++) {
	    				sum_table[i][j][h][w] = 2 * threshold;
	    			}
	    		}
	    	}
	    }
	    double row_add_mat[][] = new double[height][height];
	    double column_add_mat[][] = new double[width][width];
	    get_add_patch_matrix(height, width, nHW, kHW, row_add_mat, column_add_mat);
	    double diff_margin[][] = new double[height][width];
	    for (i = nHW; i < height - nHW; i++) {
	    	for (j = nHW; j < width - nHW; j++) {
	    		diff_margin[i][j] = 1.0;
	    	}
	    }
	    double sum_margin[][] = new double[height][width];
	    for (i = 0; i < height; i++) {
	    	for (j = 0; j < width; j++) {
	    		sum_margin[i][j] = (1 - diff_margin[i][j]) * 2 * threshold;
	    	}
	    }
	    
	    double diff_table_2[][] = new double[height][width];
	    double sum_diff_2[][];
	    for (di = -nHW; di < nHW + 1; di++) {
	        for (dj = -nHW; dj < nHW + 1; dj++) {
	        	double t_img[][] = translation_2d_mat(img_buf, -dj, -di);
	        	for (i = 0; i < height; i++) {
	        		for (j = 0; j < width; j++) {
	        			diff_table_2[i][j] = (img_buf[i][j]- t_img[i][j]) * (img_buf[i][j] - t_img[i][j]) * diff_margin[i][j];
	        		}
	        	}
	        	
	        	sum_diff_2 = (((new Matrix(row_add_mat)).times(new Matrix(diff_table_2))).times(new Matrix(column_add_mat))).getArray();
	        	// # sum_table (2n+1, 2n+1, height, width)
	        	for (i = 0; i < height; i++) {
	        		for (j = 0; j < width; j++) {
	        			sum_table[di + nHW][dj + nHW][i][j] = Math.max(sum_diff_2[i][j], sum_margin[i][j]);
	        		}
	        	}
	        } // for (dj = -nHW; dj < nHW + 1; dj++)
	    } // for (di = -nHW; di < nHW + 1; di++)
	    // di_dj, ph_pw
	    double sum_table_reshape[][] = new double[Ns * Ns][length];
	    for (i = 0; i < Ns; i++) {
	    	for (j = 0; j < Ns; j++) {
	    		for (h = 0; h < height; h++) {
	    			for (w = 0; w < width; w++) {
	    				sum_table_reshape[i*Ns + j][h*width + w] = sum_table[i][j][h][w];
	    			}
	    		}
	    	}
	    }
	    // ph_pw__di_dj
	    double sum_table_T[][] = new double[length][Ns * Ns];
	    for (i = 0; i < Ns * Ns; i++) {
	    	for (j = 0; j < length; j++) {
	    		sum_table_T[j][i] = sum_table_reshape[i][j];
	    	}
	    }
	    int argsort[][] = new int[length][NHW];
	    ArrayList <indexValueItem> indexValueList = new ArrayList<indexValueItem>();
	    for (i = 0; i < length; i++) {
	    	indexValueList.clear();
	    	for (j = 0; j < Ns * Ns; j++) {
	    		indexValueList.add(new indexValueItem(j, sum_table_T[i][j]));
	    	}
	    	Collections.sort(indexValueList, new indexValueComparator());
	    	for (j = 0; j < NHW; j++) {
	    		argsort[i][j] = indexValueList.get(j).getIndex();
	    	}
	    }
	    for (i = 0; i < length; i++) {
	    	argsort[i][0] = (Ns * Ns - 1)/2;
	    }
	    int argsort_di[][] = new int[length][NHW];
	    for (i = 0; i < length; i++) {
	    	for (j = 0; j < NHW; j++) {
	    		argsort_di[i][j] = argsort[i][j] / Ns - nHW;
	    	}
	    }
	    int argsort_dj[][] = new int[length][NHW];
	    for (i = 0; i < length; i++) {
	    	for (j = 0; j < NHW; j++) {
	    		argsort_dj[i][j] = argsort[i][j] % Ns - nHW;
	    	}
	    }
	    int near_pi[][][] = new int[height][width][NHW];
	    for (h = 0; h < height; h++) {
	    	for (w = 0; w < width; w++) {
	    		for (i = 0; i < NHW; i++) {
	    			near_pi[h][w][i] = argsort_di[h*width + w][i] + h;
	    		}
	    	}
	    }
	    int near_pj[][][] = new int[height][width][NHW];
	    for (h = 0; h < height; h++) {
	    	for (w = 0; w < width; w++) {
	    		for (i = 0; i < NHW; i++) {
	    			near_pj[h][w][i] = argsort_dj[h*width + w][i] + w;
	    		}
	    	}
	    }
	    int ri_rj_N__ni_nj[][][][] = new int[height][width][NHW][2];
	    for (h = 0; h < height; h++) {
	    	for (w = 0; w < width; w++) {
	    		for (i = 0; i < NHW; i++) {
	    		    ri_rj_N__ni_nj[h][w][i][0] = near_pi[h][w][i];
	    		    ri_rj_N__ni_nj[h][w][i][1] = near_pj[h][w][i];
	    		}
	    	}
	    }
	    byte sum_filter[][] = new byte[length][Ns * Ns];
	    for (i = 0; i < length; i++) {
	    	for (j = 0; j < Ns * Ns; j++) {
	    		if (sum_table_T[i][j] < threshold) {
	    			sum_filter[i][j] = 1;
	    		}
	    		else {
	    			sum_filter[i][j] = 0;
	    		}
	    	}
	    }
	    int threshold_count_init[] = new int[length];
	    for (i = 0; i < length; i++) {
	    	for (j = 0; j < Ns * Ns; j++) {
	    	    threshold_count_init[i] += sum_filter[i][j];	
	    	}
	    }
	    int threshold_count_2[] = closest_power_of_2(threshold_count_init, NHW);
	    for (h = 0; h < height; h++) {
	    	for (w = 0; w < width; w++) {
	    		threshold_count[h][w] = threshold_count_2[h*width + w];
	    	}
	    }
	    return ri_rj_N__ni_nj;
	}
	
	private int[] closest_power_of_2(int[] M, int max_) {
	    int i;
	    int length = M.length;
	    int M_out[] = new int[length];
	    for (i = 0; i < length; i++) {
	        if (M[i] > max_) {
	        	M_out[i] = max_;
	        }
	        else {
	        	M_out[i] = M[i];
	        }
	    }
        while (max_ > 1) {
        	for (i = 0; i < length; i++) {
	        	if ((max_/2 < M_out[i]) && (max_ > M_out[i])) {
	        		M_out[i] = max_/2;
	        	}
        	}
        	max_ = max_/2;
        }
        return M_out;
	}
	
	private class indexValueComparator implements Comparator<indexValueItem> {

        /**
         * DOCUMENT ME!
         * 
         * @param o1 DOCUMENT ME!
         * @param o2 DOCUMENT ME!
         * 
         * @return DOCUMENT ME!
         */
        public int compare(indexValueItem o1, indexValueItem o2) {
            double a = o1.getValue();
            double b = o2.getValue();
            int i = o1.getIndex();
            int j = o2.getIndex();

            if (a < b) {
                return -1;
            } else if (a > b) {
                return 1;
            } else if (i < j) {
            	return -1;
            } else if (i > j) {
            	return 1;
            } else {
                return 0;
            }
        }

    }
	
	private class indexValueItem {
		private int index;
		private double value;
		
		public indexValueItem(int index, double value) {
			this.index = index;
			this.value = value;
		}
		
		public int getIndex() {
			return index;
		}
		
		public double getValue() {
			return value;
		}
		
		
	}
	
	private void get_add_patch_matrix(int h, int w, int nHW, int kHW, double[][] row_add_mat, double[][] column_add_mat) {
		int i, j, k;
	    double row_add_pad[][] = new double[h][h];
	    for (i = h; i < h - nHW; i++) {
	    	row_add_pad[i][i] = 1.0;
	    	row_add_mat[i][i] = 1.0;
	    }
	    for (k = 1; k < kHW; k++) {
	    	double trans_mat[][] = translation_2d_mat(row_add_pad, k, 0);
	    	for (i = 0; i < h; i++) {
	    		for (j = 0; j < h; j++) {
	    			row_add_mat[i][j] += trans_mat[i][j];
	    		}
	    	}
	    } // for (k = 1; k < kHW; k++)
	    
	    double column_add_pad[][] = new double[w][w];
	    for (i = w; i < w - nHW; i++) {
	    	column_add_pad[i][i] = 1.0;
	    	column_add_mat[i][i] = 1.0;
	    }
	    for (k = 1; k < kHW; k++) {
	    	double trans_mat[][] = translation_2d_mat(column_add_pad, 0, k);
	    	for (i = 0; i < w; i++) {
	    		for (j = 0; j < w; j++) {
	    			column_add_mat[i][j] += trans_mat[i][j];
	    		}
	    	}
	    } // for (k = 1; k < kHW; k++)
	}
	
	private double[][] translation_2d_mat(double mat[][], int right, int down) {
		int i, j, iroll, jroll;
		double roll_mat[][] = new double[mat.length][mat[0].length];
		for (i = 0; i < mat.length; i++) {
			if (down >= 0) {
			    iroll = (i + down)%mat.length;
			}
			else {
				iroll = i + down;
				while (iroll < 0) {
					iroll += mat.length;
				}
			}
			for (j = 0; j < mat[0].length; j++) {
				if (right >= 0) {
				    jroll = (j + right)%mat[0].length;	
				}
				else {
					jroll = j + right;
					while (jroll < 0) {
						jroll += mat[0].length;
					}
				}
				roll_mat[iroll][jroll] = mat[i][j];
			}
		}
		return roll_mat;
	}
	
	private double[][] get_kaiserWindow(int kHW) {
	    int i,j;
	    double k[] = kaiser(kHW, 2);
	    double k_2d[][] = new double[k.length][k.length];
	    for (i = 0; i < k.length; i++) {
	    	for (j = 0; j < k.length; j++) {
	    		k_2d[i][j] = k[i]*k[j];
	    	}
	    }
	    return k_2d;
	}
	
	private double[] kaiser(int M, double beta) {
		// M is the number of points in the output window\
		// If zero or less, an empty array is returned
		
		// beta shape parameter for window
		
		// Returns double[] out 
		// The window with the maximum value normalized to one (the value one
		// appears only if the number of samples is odd).
		int i;
		double n;
		if (M <= 0) {
			return null;
		}
		
		double out[] = new double[M];
		double realArg;
		double imagArg = 0.0;
		double initialOrder = 0.0;
		int sequenceNumber = 1; // Number of sequential Bessel function orders calculated
    	double realResult[] = new double[1];
    	double imagResult[] = new double[1];
    	int[] nz = new int[1];
		int[] errorFlag = new int[1];
		double maxValue = - Double.MAX_VALUE;
		realArg = beta;
		Bessel denomBessel = new Bessel(Bessel.BESSEL_I, realArg, imagArg, initialOrder,
				Bessel.UNSCALED_FUNCTION, sequenceNumber, realResult, imagResult,
				nz, errorFlag);
		denomBessel.run();
		double denomResult = realResult[0];
		for (i = 0, n = -(M-1.0)/2.0; i < M; i++, n+= 1.0) {
			realArg = beta*Math.sqrt(1.0 - 4.0*n*n/((M-1.0)*(M-1.0)));
			Bessel numBessel = new Bessel(Bessel.BESSEL_I, realArg, imagArg, initialOrder,
					Bessel.UNSCALED_FUNCTION, sequenceNumber, realResult, imagResult,
					nz, errorFlag);
			numBessel.run();
			double numResult = realResult[0];
			out[i] = numResult/denomResult;
			if (out[i] > maxValue) {
				maxValue = out[i];
			}
		}
		if ((M % 2) == 1) {
			for (i = 0; i < M; i++) {
				out[i] = out[i]/maxValue;
			}
		}
		return out;
	}
	
	private int[] ind_initialize(int max_size, int N, int step) {
		int i;
		Vector<Integer>vecInd = new Vector<Integer>();
		for (i = N; i < max_size - N; i += step) {
			vecInd.add(i);
		}
		if (vecInd.lastElement() < max_size - N - 1) {
		    vecInd.add(max_size - N - 1);
		}
		int ind[] = new int[vecInd.size()];
		for (i = 0; i < vecInd.size(); i++) {
			ind[i] = vecInd.get(i);
		}
	    vecInd.clear();
	    return ind;
	}
}
