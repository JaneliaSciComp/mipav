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
	private int n_H;
	
	// Hard thresholding patch size
	private int k_H;
	
	// Hard thresholding maximum number of similar patches kept
	private int N_H;
	
	// Hard thresholding  In order to speed up the processing, the loop over of the pixels of the
	// image is done with a step p (integer) in row and column.  For example if p = 3 the algorithm
	// is accelerated by a 9 factor.
	private int p_H;
	
	// Hard thresholding maximum threshold for the distance between two similar patches
	// tauMatch_H = 2500 if sigma < 40 and 5000 otherwise.  Moreover, if N_H >= 32, this
	// threshold is multiplied by a factor of 5.
	private double tauMatch_H;
	
	private boolean useSD_H;
	
	// Threshold for 2D transform applied to each patch of the 3D group.  This threshold only
	// appears for sigma > 40 since tau_2D_H = 0 for sigma <= 40.  Applying a theshold to the
	// 2D transform coefficients for low values of sigma is useless since there is no 
	// improvement after the second step.  Moreover for a noise lower than 5 the results are
	// degraded.  tau_2D_H is a Bior1.5 transform whatever the value of sigma.  For the 
	// bi-orthogonal spline wavelet the vanishing moments of the  decomposing and reconstructing
	// wavelete functions are 1 and 5 repectively.
	private double tau_2D_H;
	
	// Coefficient thresholding level of the 3D group in the transform domain during the first
	// filtering sub-step.  The chosen value is 2.7.
	private double lambda3D_H;
	
	// Wiener thresholding search window size
    private int n_W;
    
    // Wiener thresholding patch size
 	private int k_W;
 	
    // Wiener thresholding maximum number of similar patches kept
 	private int N_W;
 	
    // Wiener thresholding  In order to speed up the processing, the loop over of the pixels of the
 	// image is done with a step p (integer) in row and column.  For example if p = 3 the algorithm
 	// is accelerated by a 9 factor.
 	private int p_W;
 	
    // Wiener thresholding maximum threshold for the distance between two similar patches
 	// tauMatch_W = 400 is a good value for sigma between 0 and 40
 	private double tauMatch_W;
 	
 	private boolean useSD_W;
 	
    // Threshold for 2D transform applied to each patch of the 3D group.
 	// A 2D DCT transform is used.
 	private double tau_2D_W;
	
	public BM3D(ModelImage destImage, ModelImage srcImg, double sigma, int n_H, int k_H, int N_H,
			int p_H, double tauMatch_H, boolean useSD_H, double tau_2D_H, double lambda3D_H,
			int n_W, int k_W, int N_W, int p_W, double tauMatch_W, boolean useSD_W, double tau_2D_W) {
		super(destImage, srcImg);
		this.sigma = sigma;
		this.n_H = n_H;
		this.k_H = k_H;
		this.p_H = p_H;
		this.tauMatch_H = tauMatch_H;
		this.useSD_H = useSD_H;
		this.tau_2D_H = tau_2D_H;
		this.lambda3D_H = lambda3D_H;
		this.n_W = n_W;
		this.k_W = k_W;
		this.N_W = N_W;
		this.p_W = p_W;
		this.useSD_W = useSD_W;
		this.tau_2D_W = tau_2D_W;
	}
	
	public void runAlgorithm() {
		
	}
}
