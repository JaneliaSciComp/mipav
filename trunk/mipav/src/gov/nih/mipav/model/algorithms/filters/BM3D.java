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
	
	public BM3D(ModelImage destImage, ModelImage srcImg, double sigma, int n_H, int N_H,
			int p_H, boolean useSD_H, String tau_2D_H, double lambda3D_H,
			int n_W, int N_W, int p_W, boolean useSD_W, String tau_2D_W) {
		super(destImage, srcImg);
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
		
		double img_basic[] = bm3d_1st_step(sigma, noisy_im_p, n_H, k_H, N_H, p_H, lambda3D_H, tauMatch_H, useSD_H, tau_2D_H);
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
		int paddedXDim = xDim + pad;
		int paddedYDim = yDim + pad;
		int paddedLength = paddedXDim * paddedYDim;
		double padBuf[] = new double[paddedLength];
		for (y = 0; y < pad; y++) {
			for (x = 0; x < pad; x++) {
				padBuf[y*paddedXDim + x] = buf[(pad - 1 - y)*xDim + (pad - 1 - x)];
			}
			
			for (x = xDim + pad; x < xDim + 2*pad; x++) {
				padBuf[y*paddedXDim + x] = buf[(pad - 1 - y)*xDim + 2*xDim + pad - 1 - x];
			}
		}
		
		for (y = yDim + pad; y < yDim + 2*pad; y++) {
			for (x = 0; x < pad; x++) {
				padBuf[y*paddedXDim + x] = buf[(2*yDim + pad - 1 - y)*xDim + (pad - 1 - x)];
			}
			
			for (x = xDim + pad; x < xDim + 2*pad; x++) {
				padBuf[y*paddedXDim + x] = buf[(2*yDim + pad - 1 - y)*xDim + 2*xDim + pad - 1 - x];
			}
		}
		
		for (y = pad; y < yDim + pad; y++) {
			for (x = pad; x < xDim + pad; x++) {
				padBuf[y*paddedXDim + x] = buf[(y - pad)*xDim + x - pad];
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
	
	private double[] bm3d_1st_step(double sigma, ModelImage img_noisy, int nHard, int kHard, int NHard, int pHard, double lambdaHard3D,
			double tauMatch, boolean useSD, String tau_2D) {
	    int width = img_noisy.getExtents()[0];
	    int height = img_noisy.getExtents()[1];
	    int length = width*height;
	    
	    int row_ind[] = ind_initialize(height - kHard + 1, nHard, pHard);
	    int column_ind[] = ind_initialize(width - kHard + 1, nHard, pHard);
	    
	    double kaiserWindow[][] = get_kaiserWindow(kHard);
	    int threshold_count[] = new int[1];
	    double ri_rj_N__ni_nj[][][] = precompute_BM(img_noisy, kHard, NHard, nHard, tauMatch, threshold_count);
	    double numerator[] = new double[length];
	    double denominator[] = new double[length];
	    double img_basic[] = new double[length];
	    return img_basic;
	}
	
	private double[][][] precompute_BM(ModelImage img, int kHW, int NHW, int nHW, double tauMatch, int[] threshold_count) {
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
	    //near_pi = argsort_di.reshape((height, width, -1)) + np.arange(height)[:, np.newaxis, np.newaxis]
	    double ri_rj_N__ni_nj[][][] = null;
	    return ri_rj_N__ni_nj;
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
		int i, n;
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
		for (i = 0, n = -(M-1)/2; n <= (M-1)/2; i++, n++) {
			realArg = beta*Math.sqrt(1.0 - 4.0*n*n/((M-1.0)*(M-1.0)));
			Bessel numBessel = new Bessel(Bessel.BESSEL_I, realArg, imagArg, initialOrder,
					Bessel.UNSCALED_FUNCTION, sequenceNumber, realResult, imagResult,
					nz, errorFlag);
			numBessel.run();
			double numResult = realResult[0];
			realArg = beta;
			Bessel denomBessel = new Bessel(Bessel.BESSEL_I, realArg, imagArg, initialOrder,
					Bessel.UNSCALED_FUNCTION, sequenceNumber, realResult, imagResult,
					nz, errorFlag);
			denomBessel.run();
			double denomResult = realResult[0];
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
