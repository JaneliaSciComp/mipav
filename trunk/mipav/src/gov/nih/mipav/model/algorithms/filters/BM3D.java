package gov.nih.mipav.model.algorithms.filters;


import gov.nih.mipav.model.algorithms.*;
import gov.nih.mipav.model.structures.*;

import gov.nih.mipav.view.*;

import java.io.*;
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
		
		int xDim = srcImage.getExtents()[0];
		int yDim = srcImage.getExtents()[1];
		int length = xDim * yDim;
		double noisy_im[] = new double[length];
		try {
			srcImage.exportData(0, length, noisy_im);
		}
		catch (IOException e) {
			MipavUtil.displayError("IOExcception on srcImage.exportData(0, length, noisy_im)");
			setCompleted(false);
			return;
		}
		
		// Symmetric reflection padding
		int paddedXDim = xDim + 2*n_H;
		int paddedYDim = yDim + 2*n_H;
		int paddedLength = paddedXDim * paddedYDim;
		double noisy_im_p_buffer[] = new double[paddedLength];
		for (y = 0; y < n_H; y++) {
			for (x = 0; x < n_H; x++) {
				noisy_im_p_buffer[y*paddedXDim + x] = noisy_im[(n_H - 1 - y)*xDim + (n_H - 1 - x)];
			}
			
			for (x = xDim + n_H; x < xDim + 2*n_H; x++) {
				noisy_im_p_buffer[y*paddedXDim + x] = noisy_im[(n_H - 1 - y)*xDim + 2*xDim + n_H - 1 - x];
			}
		}
		
		for (y = yDim + n_H; y < yDim + 2*n_H; y++) {
			for (x = 0; x < n_H; x++) {
				noisy_im_p_buffer[y*paddedXDim + x] = noisy_im[(2*yDim + n_H - 1 - y)*xDim + (n_H - 1 - x)];
			}
			
			for (x = xDim + n_H; x < xDim + 2*n_H; x++) {
				noisy_im_p_buffer[y*paddedXDim + x] = noisy_im[(2*yDim + n_H - 1 - y)*xDim + 2*xDim + n_H - 1 - x];
			}
		}
		
		for (y = n_H; y < yDim + n_H; y++) {
			for (x = n_H; x < xDim + n_H; x++) {
				noisy_im_p_buffer[y*paddedXDim + x] = noisy_im[(y - n_H)*xDim + x - n_H];
			}
		}
		
		int paddedExtents[] = new int[] {paddedXDim, paddedYDim};
		ModelImage noisy_im_p = new ModelImage(ModelStorageBase.DOUBLE, paddedExtents, "noisy_im_p");
		try {
			noisy_im_p.importData(0, noisy_im_p_buffer, true);
		}
		catch(IOException e) {
			MipavUtil.displayError("IOException on noisy_im_p.importData(0, noisy_im_p_buffer, true)");
			setCompleted(false);
			return;
		}
		
		double img_basic[] = bm3d_1st_step(sigma, noisy_im_p, n_H, k_H, N_H, p_H, lambda3D_H, tauMatch_H, useSD_H, tau_2D_H);
	}
	
	private double[] bm3d_1st_step(double sigma, ModelImage img_noisy, int nHard, int kHard, int NHard, int pHard, double lambdaHard3D,
			double tauMatch, boolean useSD, String tau_2D) {
	    int width = img_noisy.getExtents()[0];
	    int height = img_noisy.getExtents()[1];
	    int length = width*height;
	    double numerator[] = new double[length];
	    double denominator[] = new double[length];
	    double img_basic[] = new double[length];
	    return img_basic;
	}
}
