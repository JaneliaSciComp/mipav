package gov.nih.mipav.model.algorithms.filters;


import gov.nih.mipav.model.algorithms.AlgorithmBase;
import gov.nih.mipav.model.algorithms.filters.AlgorithmRiceWaveletTools.EigenvalueComplex;
import gov.nih.mipav.model.algorithms.filters.AlgorithmRiceWaveletTools.EigenvalueComplexComparator;
import gov.nih.mipav.model.structures.*;

import gov.nih.mipav.view.MipavUtil;
import gov.nih.mipav.view.Preferences;
import gov.nih.mipav.view.ViewJComponentGraph;
import gov.nih.mipav.view.ViewJFrameGraph;

import java.io.IOException;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.Collections;
import java.util.Comparator;
import java.util.List;

import de.jtem.numericalMethods.algebra.linear.decompose.Eigenvalue;

/** 
 * This code is ported from the MATLAB source code of BiorthogonalWavelets by Brian Moore
 * Below is the license for the original source code.
 */

/**
 * Copyright (c) 2012, Brian Moore
Copyright (c) 2004, Ben Barrowes
All rights reserved.

Redistribution and use in source and binary forms, with or without
modification, are permitted provided that the following conditions are
met:

    * Redistributions of source code must retain the above copyright
      notice, this list of conditions and the following disclaimer.
    * Redistributions in binary form must reproduce the above copyright
      notice, this list of conditions and the following disclaimer in
      the documentation and/or other materials provided with the distribution

THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS "AS IS"
AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE
IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE
ARE DISCLAIMED. IN NO EVENT SHALL THE COPYRIGHT OWNER OR CONTRIBUTORS BE
LIABLE FOR ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR
CONSEQUENTIAL DAMAGES (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF
SUBSTITUTE GOODS OR SERVICES; LOSS OF USE, DATA, OR PROFITS; OR BUSINESS
INTERRUPTION) HOWEVER CAUSED AND ON ANY THEORY OF LIABILITY, WHETHER IN
CONTRACT, STRICT LIABILITY, OR TORT (INCLUDING NEGLIGENCE OR OTHERWISE)
ARISING IN ANY WAY OUT OF THE USE OF THIS SOFTWARE, EVEN IF ADVISED OF THE
POSSIBILITY OF SUCH DAMAGE.\
*/

public  class BiorthogonalWavelets extends AlgorithmBase {
	// Syntax defaults to the CDF 9/7 wavelets used in JPEG2000
	// Author of original code: Brian Moore
	// brimoor@umich.edu
	
	/** D1MACH(4). */
    private double epsilon = Double.NaN;
    
    // If you used BWDesignTool to generate a biorthogonal wavelet filter and saved the output variables
    // [h, ht, g, gt, inds_h, inds_ht, inds_g, inds_gt] you may pass them directly into BiorthogonalWavellets.
    private double h[];
    
    private double ht[];
    
    private int inds_h[];
    
    private int inds_ht[];
    
    private double g[];
    
    private double gt[];
    
    private int inds_g[];
    
    private int inds_gt[];
    
    private ModelImage transformImage;
    
    private ModelImage compressedTransformImage;
    
    private ModelImage reconstructedImage;
    
    private int iterations;
    
    // 0 <= compressionFactor <= 1.
    // Alternatively, when compressionFactor == -1, the lowest frequency approximation is retained.
    private double compressionFactor;
    
    // Type can be "spline" or "CDF" and controls the type of biorthogonal wavelet filter used.
    String type;
    
    // 2*(l+R) + 1 and 2*(lt + C) + 1 are the lengths of the lowpass biorthogonal filters h and ht,
    // where R and C are the number of real and complex zeros of P(t), deg(P) = l + lt - 1,
    // respectively when type == "CDF".
    private int l;
    
    private int lt;
    
    // N+1 and Nt+1 are the lengths of the lowpass biorthogonal spline filters h and ht,
    // respectively when type == "spline".
    private int N;
    
    private int Nt;
    
    // Plot result or not
    private boolean display = true;
    
    // Forward transform computation method
    // "conv" or "matrix"
    String method = "conv";
    
    public BiorthogonalWavelets(ModelImage transformImage, ModelImage compressedTransformImage, 
    		ModelImage reconstructedImage, ModelImage srcImg, int iterations, double compressionFactor,
    		boolean display, String method,
    		String type, int len, int lent) {
    	super(null, srcImg);
    	this.transformImage = transformImage;
    	this.compressedTransformImage = compressedTransformImage;
    	this.reconstructedImage = reconstructedImage;
    	this.iterations = iterations;
    	this.compressionFactor = compressionFactor;
    	this.display = display;
    	this.method = method;
    	this.type = type;
    	if (type.equalsIgnoreCase("CDF")) {
    		l = len;
    		lt = lent;
    	}
    	else if (type.equalsIgnoreCase("spline")) {
    		N = len;
    		Nt = lent;
    	}
    }
    
    public void runAlgorithm() {
    	
    }
    
    private void computeEpsilon() {
    	// epsilon = D1MACH(4)
        // Machine epsilon is the smallest positive epsilon such that
        // (1.0 + epsilon) != 1.0.
        // epsilon = 2**(1 - doubleDigits) = 2**(1 - 53) = 2**(-52)
        // epsilon = 2.2204460e-16
        // epsilon is called the largest relative spacing
        epsilon = 1.0;
        double neweps = 1.0;

        while (true) {

            if (1.0 == (1.0 + neweps)) {
                break;
            } else {
                epsilon = neweps;
                neweps = neweps / 2.0;
            }
        } // while(true)	
    }
	
	public void CDFWavelets(int l, int lt, boolean display) {
		 // Inputs:   disp can be 'true' or 'false' and controls whether or not to
		 //           display H(w), Ht(w), G(w), and Gt(w). The default is 'true'.
		 
		 // Outputs:  Computes the Cohen, Daubechies, and Feauveau (CDF) lowpass 
		 //           biorthogonal wavelet filters, h and ht, of length 2*(l+R)+1 and
		 //           2*(lt+C)+1, where R and C are the number of real and complex
		 //           zeros of P(t), deg(P)=l+lt-1, along with the corresponding
		 //           indices of the filter coefficients in inds_h and inds_ht,
		 //           respectively. The highpass filters, g and gt, are also returned
		 //           along with the filter indices, inds_g and inds_gt, respectively
		 
		 // Note:     As per CDF, the real zeros of P(t) are placed in ht, and the
		 //           complex zeros of P(t) are placed in h.
		 
		 // Author:   Brian Moore
		 //           brimoor@umich.edu

		int N;
		double h1[];
		int i;
		int j;
		double sqrt2;
		double val;
		int kk;
		int Nt;
		double ht1[];
		double val1;
		int K;
		double P[];
		int jj;
		double r[][];
		double realRoots[];
		double complexRoots[][];
		double C;
		double Ct;
		double prod;
		double A;
		double h2[];
		double a;
		double b;
		double u[];
		double ht2[];
		int lower;
		int upper;
		int res;
		double w[];
		float wfloat[];
		double spacing;
		double HwReal[];
		double HwImag[];
		float absHw[];
		double HtwReal[];
		double HtwImag[];
		float absHtw[];
		double GwReal[];
		double GwImag[];
		float absGw[];
		double GtwReal[];
		double GtwImag[];
		float absGtw[];
		
		if (Double.isNaN(epsilon)) {
			computeEpsilon();
		}
	    // Compute left half of h
		N = 2 * l;
		h1 = new double[N+1];
		sqrt2 = Math.sqrt(2.0);
		val = sqrt2/Math.pow(2.0, N);
		for (i = 0; i < h1.length; i++) {
		    h1[i] = val;
		}
		for (kk = 0; kk <= N; kk++) {
			h1[kk] = h1[kk] * nchoosek(N, kk);
		}
		
		// Compute left half of ht
		Nt = 2 * lt;
		ht1 = new double[Nt+1];
		val1 = sqrt2/Math.pow(2.0, Nt);
		for (i = 0; i < ht1.length; i++) {
			ht1[i] = val1;
		}
		for (kk = 0; kk <= Nt; kk++) {
			ht1[kk] = ht1[kk] * nchoosek(Nt, kk);
		}
		
		// Compute right halves of h and ht
		K = l + lt;
		P = new double[K];
		for (jj = 0; jj <= K-1; jj++) {
			P[K-jj-1] = nchoosek(K-1+jj,jj);
		}
		r = cplxpair(roots(P));
		jj = 0;
		while (jj < r.length) {
			if (r[jj][1] == 0.0) {
				break;
			}
			jj = jj + 1;
		} // while (jj < r.length)
		
		if ((jj % 2) == 1) {
			MipavUtil.displayError("All complex roots were not conjugates");
		    return;
		}
		else if (jj == 0) {
			// All real roots
			realRoots = new double[r.length];
			for (i = 0; i < r.length; i++) {
				realRoots[i] = r[i][0];
			}
			complexRoots = null;
			C= 1;
			Ct = P[0];
		}
		else if (jj == r.length) {
			// All complex roots
			realRoots = null;
			complexRoots = new double[r.length][2];
			for (i = 0; i < r.length; i++) {
				complexRoots[i][0] = r[i][0];
				complexRoots[i][1] = r[i][1];
			}
			C = P[0];
			Ct = 1;
		}
		else {
			// Mix of real and complex roots
			realRoots = new double[r.length - jj];
			for (i = 0; i < r.length - jj; i++) {
				realRoots[i] = r[i + jj][0];
			}
			complexRoots = new double[jj][2];
			for (i = 0; i < jj; i++) {
				complexRoots[i][0] = r[i][0];
				complexRoots[i][1] = r[i][1];
			}
			prod = -r[jj][0];
			for (i = jj+1; i < r.length; i++) {
				prod = prod * (-r[i][0]);
			}
			A = 1.0/prod;
			C = P[0]/A;
			Ct = A;
		}
		
		h2 = new double[]{C};
		for (jj = 0; jj < complexRoots.length; jj += 2) {
		    a = complexRoots[jj][0];
		    b = -complexRoots[jj][1];
		    u = new double[]{0.0625,a/2-0.25,a*a+b*b-a+0.375,a/2-0.25,0.0625};
		    h2 = conv(u,h2);
		}
		
		ht2 = new double[]{Ct};
		for (jj = 0; jj < realRoots.length; jj++) {
			u = new double[]{-0.25,0.5-realRoots[jj],-0.25};
			ht2 = conv(u,ht2);
		}
		
		// Compute h and ht via convolution
		h = conv(h1,h2);
		ht = conv(ht1,ht2);
		
		lower = -(h.length - 1)/2;
		upper = (h.length - 1)/2;
		inds_h = new int[upper - lower + 1];
		for (i = 0; i < upper - lower + 1; i++) {
			inds_h[i] = lower + i;
		}
		
		lower = -(ht.length - 1)/2;
		upper = (ht.length - 1)/2;
		inds_ht = new int[upper - lower + 1];
		for (i = 0; i < upper - lower + 1; i++) {
			inds_ht[i] = lower + i;
		}
		
		// Compute g and gt
		inds_g = new int[inds_ht.length];
		for (i = 0; i < inds_ht.length; i++) {
			inds_g[i] = 1 - inds_ht[inds_ht.length-1-i];
		}
		
		inds_gt = new int[inds_h.length];
		for (i = 0; i < inds_h.length; i++) {
			inds_gt[i] = 1 - inds_h[inds_h.length-1-i];
		}
		
		g = new double[ht.length];
		for (i = 0; i < ht.length; i++) {
			g[i] = ht[ht.length-1-i] * (2 * (inds_ht[inds_ht.length-1-i] % 2) - 1);
		}
		
		gt = new double[h.length];
		for (i = 0; i < h.length; i++) {
			gt[i] = h[h.length-1-i] * (2 * (inds_h[inds_h.length-1-i] % 2) - 1);
		}
		
		// Plot the frequency responses if desired
		if (display) {
			Preferences.debug(realRoots.length + " real zeros\n", Preferences.DEBUG_ALGORITHM);
			Preferences.debug(complexRoots.length + " complex zeros\n", Preferences.DEBUG_ALGORITHM);
			
			res = 1000;
			w = new double[res];
			wfloat = new float[res];
			spacing = Math.PI/(res - 1.0);
			for (i = 0; i < res; i++) {
			    w[i] = i * spacing;	
			    wfloat[i] = (float)w[i];
			}
			
			HwReal = new double[res];
			HwImag = new double[res];
			absHw = new float[res];
			for (i = 0; i < inds_h.length; i++) {
			    for (j = 0; j < res; j++) {
			        HwReal[j] = HwReal[j] + h[i]*Math.cos(inds_h[i] * w[j]);
			        HwImag[j] = HwImag[j] + h[i]*Math.sin(inds_h[i] * w[j]);
			    }
			}
			for (i = 0; i < res; i++) {
				absHw[i] = (float)Math.sqrt(HwReal[i]*HwReal[i] + HwImag[i]*HwImag[i]);
			}
			String HwTitle = "Frequency response of " + h.length + " / " + ht.length + " Hw CDF Wavelet";
			ViewJFrameGraph HwGraph = new ViewJFrameGraph(wfloat, absHw, HwTitle, "Frequency", "abs(Hw)");
			HwGraph.setVisible(true);
			ViewJComponentGraph HwVGraph = HwGraph.getGraph();
			HwVGraph.setDomain(0.0f,(float)Math.PI);
			HwVGraph.setRange(0.0f,(float)Math.sqrt(2.0));
			
			HtwReal = new double[res];
			HtwImag = new double[res];
			absHtw = new float[res];
			for (i = 0; i < inds_ht.length; i++) {
			    for (j = 0; j < res; j++) {
			        HtwReal[j] = HtwReal[j] + ht[i]*Math.cos(inds_ht[i] * w[j]);
			        HtwImag[j] = HtwImag[j] + ht[i]*Math.sin(inds_ht[i] * w[j]);
			    }
			}
			for (i = 0; i < res; i++) {
				absHtw[i] = (float)Math.sqrt(HtwReal[i]*HtwReal[i] + HtwImag[i]*HtwImag[i]);
			}
			String HtwTitle = "Frequency response of " + h.length + " / " + ht.length + " Htw CDF Wavelet";
			ViewJFrameGraph HtwGraph = new ViewJFrameGraph(wfloat, absHtw, HtwTitle, "Frequency", "abs(Htw)");
			HtwGraph.setVisible(true);
			ViewJComponentGraph HtwVGraph = HtwGraph.getGraph();
			HtwVGraph.setDomain(0.0f,(float)Math.PI);
			HtwVGraph.setRange(0.0f,(float)Math.sqrt(2.0));
			
			GwReal = new double[res];
			GwImag = new double[res];
			absGw = new float[res];
			for (i = 0; i < inds_g.length; i++) {
			    for (j = 0; j < res; j++) {
			        GwReal[j] = GwReal[j] + g[i]*Math.cos(inds_g[i] * w[j]);
			        GwImag[j] = GwImag[j] + g[i]*Math.sin(inds_g[i] * w[j]);
			    }
			}
			for (i = 0; i < res; i++) {
				absGw[i] = (float)Math.sqrt(GwReal[i]*GwReal[i] + GwImag[i]*GwImag[i]);
			}
			String GwTitle = "Frequency response of " + h.length + " / " + ht.length + " Gw CDF Wavelet";
			ViewJFrameGraph GwGraph = new ViewJFrameGraph(wfloat, absGw, GwTitle, "Frequency", "abs(Gw)");
			GwGraph.setVisible(true);
			ViewJComponentGraph GwVGraph = GwGraph.getGraph();
			GwVGraph.setDomain(0.0f,(float)Math.PI);
			GwVGraph.setRange(0.0f,(float)Math.sqrt(2.0));
			
			GtwReal = new double[res];
			GtwImag = new double[res];
			absGtw = new float[res];
			for (i = 0; i < inds_gt.length; i++) {
			    for (j = 0; j < res; j++) {
			        GtwReal[j] = GtwReal[j] + gt[i]*Math.cos(inds_gt[i] * w[j]);
			        GtwImag[j] = GtwImag[j] + gt[i]*Math.sin(inds_gt[i] * w[j]);
			    }
			}
			for (i = 0; i < res; i++) {
				absGtw[i] = (float)Math.sqrt(GtwReal[i]*GtwReal[i] + GtwImag[i]*GtwImag[i]);
			}
			String GtwTitle = "Frequency response of " + h.length + " / " + ht.length + " Gtw CDF Wavelet";
			ViewJFrameGraph GtwGraph = new ViewJFrameGraph(wfloat, absGtw, GtwTitle, "Frequency", "abs(Gtw)");
			GtwGraph.setVisible(true);
			ViewJComponentGraph GtwVGraph = GtwGraph.getGraph();
			GtwVGraph.setDomain(0.0f,(float)Math.PI);
			GtwVGraph.setRange(0.0f,(float)Math.sqrt(2.0));
		} // if (display)
	}
	
	public void SplineWavelets(int N, int Nt, boolean display) {
		// Inputs:   disp can be 'true' or 'false' and controls whether or not to
		//           display H(w), Ht(w), G(w), and Gt(w). The default is 'true'.
		
		// Outputs:  Computes the biorthogonal spline lowpass filters, h and ht, of
		//           length Nt+1 and 2*N+Nt-1, along with the corresponding
		//           indices of each filter coefficient in inds_h and inds_ht,
		//           respectively. The highpass filters, g and gt, are also returned
		//           along with the filter indices, inds_g and inds_gt, respectively
		
		// Author:   Brian Moore
		//           brimoor@umich.edu
		int i, j;
		int lt;
		double sqrt2;
		double val;
		int kk;
		int l;
		int len;
		int max;
		double h1[];
		double h2[][];
		int jj;
		double dB[];
		double h2sum[];
		int res;
		double w[];
		float wfloat[];
		double spacing;
		double HwReal[];
		double HwImag[];
		float absHw[];
		double HtwReal[];
		double HtwImag[];
		float absHtw[];
		double GwReal[];
		double GwImag[];
		float absGw[];
		double GtwReal[];
		double GtwImag[];
		float absGtw[];
	
		if ((Nt % 2) != (N % 2)) {
			MipavUtil.displayError("N and Nt must have the same parity");
			return;
		}
		
		// Compute ht
		if ((Nt % 2) == 1) {
			// odd case
			lt = (Nt - 1)/2;
			inds_ht = new int[2*lt + 2];
			for ( i = 0; i < 2*lt+2; i++) {
				inds_ht[i] = -lt + i;
			}
		}
		else {
			// even case
			lt = Nt/2;
			inds_ht = new int[2*lt + 1];
			for (i = 0; i < 2*lt+1; i++) {
				inds_ht[i] = -lt + i;
			}
		}
		
		sqrt2 = Math.sqrt(2.0);
		val = sqrt2/Math.pow(2.0, Nt);
		ht = new double[Nt + 1];
	    for (kk = 0; kk <= Nt; kk++) {
	        ht[kk] = val * nchoosek(Nt, kk);	
	    }
	    
	    // Compute h
	    if ((N % 2) == 1) {
	    	// odd case
	    	l = (N - 1)/2;
	    	len = 2*N+Nt - 1; // will be even
	    	inds_h = new int[len];
	    	for (i = 0; i < len; i++) {
	    		inds_h[i] = -len/2+1+i;
	    	}
	    	max = l + lt;
	    }
	    else {
	    	// even case
	    	l = N/2;
	    	len = 2*N+Nt - 1; // will be odd
	    	inds_h = new int[len];
	    	for (i = 0; i < len; i++) {
	    		inds_h[i] = -(len-1)/2 + i;
	    	}
	    	max = l + lt - 1;
	    }
	    
	    // Compute the left half
	    h1 = new double[N+1];
	    val = sqrt2/Math.pow(2.0, N);
	    for (kk = 0; kk <= N; kk++) {
	    	h1[kk] = val * nchoosek(N, kk);
	    }
	    
	    // Compute the right half
	    h2 = new double[max+1][2*max+1];
	    for (jj = 0; jj <= max; jj++) {
	    	dB = diffBinomial(2*jj); // dB has length 2*jj+1
	    	for (kk = 0; kk < 2*jj+1; kk++) {
	    		h2[jj][max-jj+kk] = nchoosek(max+jj, jj)/(Math.pow(2.0,(2*jj)))*
	    				(-1 + 2*((jj+1)%2))*dB[kk];
	    	}
	    }
	    h2sum =  new double[2*max+1];
	    for (i = 0; i < 2*max+1; i++) {
	    	for (j = 0; j < max + 1; j++) {
	    	    h2sum[i] = h2sum[i] + h2[j][i];	
	    	}
	    }
	    
	    // Convolve the left and right halves to get the product
	    h = conv(h1, h2sum);
	    
	    // Compute g and gt
	    inds_g = new int[inds_ht.length];
		for (i = 0; i < inds_ht.length; i++) {
			inds_g[i] = 1 - inds_ht[inds_ht.length-1-i];
		}
		
		inds_gt = new int[inds_h.length];
		for (i = 0; i < inds_h.length; i++) {
			inds_gt[i] = 1 - inds_h[inds_h.length-1-i];
		}
		
		g = new double[ht.length];
		for (i = 0; i < ht.length; i++) {
			g[i] = ht[ht.length-1-i] * (2 * (inds_ht[inds_ht.length-1-i] % 2) - 1);
		}
		
		gt = new double[h.length];
		for (i = 0; i < h.length; i++) {
			gt[i] = h[h.length-1-i] * (2 * (inds_h[inds_h.length-1-i] % 2) - 1);
		}
		
		// Plot the frequency responses if desired
		if (display) {
			
			res = 1000;
			w = new double[res];
			wfloat = new float[res];
			spacing = Math.PI/(res - 1.0);
			for (i = 0; i < res; i++) {
			    w[i] = i * spacing;	
			    wfloat[i] = (float)w[i];
			}
			
			HwReal = new double[res];
			HwImag = new double[res];
			absHw = new float[res];
			for (i = 0; i < inds_h.length; i++) {
			    for (j = 0; j < res; j++) {
			        HwReal[j] = HwReal[j] + h[i]*Math.cos(inds_h[i] * w[j]);
			        HwImag[j] = HwImag[j] + h[i]*Math.sin(inds_h[i] * w[j]);
			    }
			}
			for (i = 0; i < res; i++) {
				absHw[i] = (float)Math.sqrt(HwReal[i]*HwReal[i] + HwImag[i]*HwImag[i]);
			}
			String HwTitle = "Frequency response of " + h.length + " / " + ht.length + " Hw Spline Wavelet";
			ViewJFrameGraph HwGraph = new ViewJFrameGraph(wfloat, absHw, HwTitle, "Frequency", "abs(Hw)");
			HwGraph.setVisible(true);
			ViewJComponentGraph HwVGraph = HwGraph.getGraph();
			HwVGraph.setDomain(0.0f,(float)Math.PI);
			HwVGraph.setRange(0.0f,(float)Math.sqrt(2.0));
			
			HtwReal = new double[res];
			HtwImag = new double[res];
			absHtw = new float[res];
			for (i = 0; i < inds_ht.length; i++) {
			    for (j = 0; j < res; j++) {
			        HtwReal[j] = HtwReal[j] + ht[i]*Math.cos(inds_ht[i] * w[j]);
			        HtwImag[j] = HtwImag[j] + ht[i]*Math.sin(inds_ht[i] * w[j]);
			    }
			}
			for (i = 0; i < res; i++) {
				absHtw[i] = (float)Math.sqrt(HtwReal[i]*HtwReal[i] + HtwImag[i]*HtwImag[i]);
			}
			String HtwTitle = "Frequency response of " + h.length + " / " + ht.length + " Htw Spline Wavelet";
			ViewJFrameGraph HtwGraph = new ViewJFrameGraph(wfloat, absHtw, HtwTitle, "Frequency", "abs(Htw)");
			HtwGraph.setVisible(true);
			ViewJComponentGraph HtwVGraph = HtwGraph.getGraph();
			HtwVGraph.setDomain(0.0f,(float)Math.PI);
			HtwVGraph.setRange(0.0f,(float)Math.sqrt(2.0));
			
			GwReal = new double[res];
			GwImag = new double[res];
			absGw = new float[res];
			for (i = 0; i < inds_g.length; i++) {
			    for (j = 0; j < res; j++) {
			        GwReal[j] = GwReal[j] + g[i]*Math.cos(inds_g[i] * w[j]);
			        GwImag[j] = GwImag[j] + g[i]*Math.sin(inds_g[i] * w[j]);
			    }
			}
			for (i = 0; i < res; i++) {
				absGw[i] = (float)Math.sqrt(GwReal[i]*GwReal[i] + GwImag[i]*GwImag[i]);
			}
			String GwTitle = "Frequency response of " + h.length + " / " + ht.length + " Gw Spline Wavelet";
			ViewJFrameGraph GwGraph = new ViewJFrameGraph(wfloat, absGw, GwTitle, "Frequency", "abs(Gw)");
			GwGraph.setVisible(true);
			ViewJComponentGraph GwVGraph = GwGraph.getGraph();
			GwVGraph.setDomain(0.0f,(float)Math.PI);
			GwVGraph.setRange(0.0f,(float)Math.sqrt(2.0));
			
			GtwReal = new double[res];
			GtwImag = new double[res];
			absGtw = new float[res];
			for (i = 0; i < inds_gt.length; i++) {
			    for (j = 0; j < res; j++) {
			        GtwReal[j] = GtwReal[j] + gt[i]*Math.cos(inds_gt[i] * w[j]);
			        GtwImag[j] = GtwImag[j] + gt[i]*Math.sin(inds_gt[i] * w[j]);
			    }
			}
			for (i = 0; i < res; i++) {
				absGtw[i] = (float)Math.sqrt(GtwReal[i]*GtwReal[i] + GtwImag[i]*GtwImag[i]);
			}
			String GtwTitle = "Frequency response of " + h.length + " / " + ht.length + " Gtw Spline Wavelet";
			ViewJFrameGraph GtwGraph = new ViewJFrameGraph(wfloat, absGtw, GtwTitle, "Frequency", "abs(Gtw)");
			GtwGraph.setVisible(true);
			ViewJComponentGraph GtwVGraph = GtwGraph.getGraph();
			GtwVGraph.setDomain(0.0f,(float)Math.PI);
			GtwVGraph.setRange(0.0f,(float)Math.sqrt(2.0));
		} // if (display)
	}
	
	private double[] diffBinomial(int n) {
		// Return: Binomial coefficients of (x-y)^n in descending powers of x
		int j;
		double vect[] = new double[n+1];
		for (j = 0; j <= n; j++) {
		    vect[j]	= nchoosek(n,j) * (-1 + 2*(j+1%2));
		}
		return vect;
	}
	
	private double[][] FastConvWaveletMult(double mat[][], double h[], double g[], int inds_h[], int inds_g[]) {
		// For now assume mat and mat2 are real
		// Returns mat2 = WM * mat , where size(WM) = [size(mat,1),size(mat,1)]
        // IMt = WMt * IM * WN' can be computed by calling
	    // IMt = FastConvWaveletMult(FastConvWaveletMult(IM,ht,gt,inds_ht,inds_gt)',h,g,inds_h,inds_g)';
		int i, j;
		FFTUtility fft;
        int M = mat.length; // rows
        int N = mat[0].length; // columns
        double mat2[][] = new double[M][N];
        double mat2I[][] = new double[M][N];
        int numinds_hgezero = 0;
        for (i = 0; i < inds_h.length; i++) {
        	if (inds_h[i] >= 0) {
        		numinds_hgezero++;
        	}
        }
        int inds[] = new int[numinds_hgezero];
        for (i = 0, j = 0; i < inds_h.length; i++) {
        	if (inds_h[i] >= 0) {
        		inds[j++] = i;
        	}
        }
        double vec[] = new double[numinds_hgezero + M - h.length + inds[0]];
        for (i = 0; i < numinds_hgezero; i++) {
        	vec[i] = h[inds[i]];
        }
        for (i = 0; i < M - h.length; i++) {
        	vec[i + numinds_hgezero] = 0.0;
        }
        for (i = 0; i < inds[0]; i++) {
        	vec[i + numinds_hgezero + M - h.length] = h[i];
        }
        double vecReverse[] = new double[vec.length];
        for (i = 0; i < vec.length; i++) {
        	vecReverse[i] = vec[vec.length - 1 - i];
        }
        int numinds_ggezero = 0;
        for (i = 0; i < inds_g.length; i++) {
        	if (inds_g[i] >= 0) {
        		numinds_ggezero++;
        	}
        }
        int inds2[] = new int[numinds_ggezero];
        for (i = 0, j = 0; i < inds_g.length; i++) {
        	if (inds_g[i] >= 0) {
        		inds2[j++] = i;
        	}
        }
        double vec2[] = new double[numinds_ggezero + M - g.length + inds2[0]];
        for (i = 0; i < numinds_ggezero; i++) {
        	vec2[i] = g[inds2[i]];
        }
        for (i = 0; i < M - g.length; i++) {
        	vec2[i + numinds_ggezero] = 0.0;
        }
        for (i = 0; i < inds2[0]; i++) {
        	vec2[i + numinds_ggezero + M - g.length] = g[i];
        }
        double vec2Reverse[] = new double[vec2.length];
        for (i = 0; i < vec2.length; i++) {
        	vec2Reverse[i] = vec2[vec2.length - 1 - i];
        }
        
        // Instantiate the 1d FFT routine
 		// -1 for forward transform
        double vecReverseI[] = new double[vecReverse.length];
 		fft = new FFTUtility(vecReverse, vecReverseI, 1, vecReverse.length, 1, -1,
 				FFTUtility.FFT);
 		fft.setShowProgress(false);
 		fft.run();
 		fft.finalize();
 		fft = null;
 		
	 	// Instantiate the 1d FFT routine
		// -1 for forward transform
	    double vec2ReverseI[] = new double[vec2Reverse.length];
		fft = new FFTUtility(vec2Reverse, vec2ReverseI, 1, vec2Reverse.length, 1, -1,
				FFTUtility.FFT);
		fft.setShowProgress(false);
		fft.run();
		fft.finalize();
		fft = null;
        
        // fft along m-point FFT of each column
		// M The dimension of the current variable
		// N  The spacing of the current variable
		double matvec[] = new double[M*N];
        for (i = 0; i < M; i++) {
        	for (j = 0; j < N; j++) {
        		matvec[j + i * N] = mat[i][j];
        	}
        }
        double matvecI[] = new double[M*N];
        fft = new FFTUtility(matvec, matvecI, 1, M, N, -1, FFTUtility.FFT);
        fft.setShowProgress(false);
		fft.run();
		fft.finalize();
		fft = null;
		
		double vals[] = new double[M];
		double valsI[] = new double[M];
		double vals2[] = new double[M];
		double vals2I[] = new double[M];
		for (i = 0; i < N; i++) {
		    for (j = 0; j < M; j++) {
		    	vals[j] = matvec[i + j * N] * vecReverse[j] - matvecI[i + j * N] * vecReverseI[j];
		    	valsI[j] = matvec[i + j * N] * vecReverseI[j] + matvecI[i + j * N] * vecReverse[j];
		    }
	    	// 1 for inverse transform
	    	fft = new FFTUtility(vals, valsI, 1, M, 1, 1, FFTUtility.FFT);
	    	fft.setShowProgress(false);
			fft.run();
			fft.finalize();
			fft = null;
			mat2[0][i] = vals[vals.length-1];
			mat2I[0][i] = valsI[valsI.length-1];
			for (j = 1; j < M/2; j++) {
				mat2[j][i] = vals[2*j-1];
				mat2I[j][i] = valsI[2*j-1];
			}
			
			for (j = 0; j < M; j++) {
		    	vals2[j] = matvec[i + j * N] * vec2Reverse[j] - matvecI[i + j * N] * vec2ReverseI[j];
		    	vals2I[j] = matvec[i + j * N] * vec2ReverseI[j] + matvecI[i + j * N] * vec2Reverse[j];
		    }
	    	// 1 for inverse transform
	    	fft = new FFTUtility(vals2, vals2I, 1, M, 1, 1, FFTUtility.FFT);
	    	fft.setShowProgress(false);
			fft.run();
			fft.finalize();
			fft = null;
			mat2[M/2][i] = vals2[vals2.length-1];
			mat2I[M/2][i] = vals2I[vals2I.length-1];
			for (j = 1; j < M/2; j++) {
				mat2[M/2+j][i] = vals2[2*j-1];
				mat2I[M/2+j][i] = vals2I[2*j-1];
			}
		} // for (i = 0; i < N; i++)
        return mat2;
	}
	
	private double[][] WaveletMatrix(int M, double h[], double g[], int inds_h[], int inds_g[]) {
		// Computes the the M x M biorthogonal wavelet matrix defined by h and g
		int i, j, jj;
        double W[][] = new double[M][M];
        
        int numinds_hgezero = 0;
        for (i = 0; i < inds_h.length; i++) {
        	if (inds_h[i] >= 0) {
        		numinds_hgezero++;
        	}
        }
        int inds[] = new int[numinds_hgezero];
        for (i = 0, j = 0; i < inds_h.length; i++) {
        	if (inds_h[i] >= 0) {
        		inds[j++] = i;
        	}
        }
        double vec[] = new double[numinds_hgezero + M - h.length + inds[0]];
        for (i = 0; i < numinds_hgezero; i++) {
        	vec[i] = h[inds[i]];
        }
        for (i = 0; i < M - h.length; i++) {
        	vec[i + numinds_hgezero] = 0.0;
        }
        for (i = 0; i < inds[0]; i++) {
        	vec[i + numinds_hgezero + M - h.length] = h[i];
        }
        
        for (jj = 1; jj <= M/2; jj++) {
             W[jj-1] = circshift(vec, 2*(jj-1));	
        } // for (jj = 1; jj <= M/2; jj++)
        
        int numinds_ggezero = 0;
        for (i = 0; i < inds_g.length; i++) {
        	if (inds_g[i] >= 0) {
        		numinds_ggezero++;
        	}
        }
        inds = new int[numinds_ggezero];
        for (i = 0, j = 0; i < inds_g.length; i++) {
        	if (inds_g[i] >= 0) {
        		inds[j++] = i;
        	}
        }
        vec = new double[numinds_ggezero + M - g.length + inds[0]];
        for (i = 0; i < numinds_ggezero; i++) {
        	vec[i] = g[inds[i]];
        }
        for (i = 0; i < M - g.length; i++) {
        	vec[i + numinds_ggezero] = 0.0;
        }
        for (i = 0; i < inds[0]; i++) {
        	vec[i + numinds_ggezero + M - g.length] = g[i];
        }
        for (jj = 1; jj <= M/2; jj++) {
            W[M/2+jj-1] = circshift(vec, 2*(jj-1));	
       } // for (jj = 1; jj <= M/2; jj++)
        return W;
	}
	
	private double[] circshift(double vec[], int shift) {
		int i;
		double vecshift[] = new double[vec.length];
		for (i = 0; i < shift; i++) {
			vecshift[i] = vec[vec.length - shift + i];
		}
		for (i = shift; i < vec.length; i++) {
			vecshift[i] = vec[i - shift];
		}
		return vecshift;
	}
	
	private double[] conv(double u[], double v[]) {
	    int m = u.length;
	    int n = v.length;
	    double w[] = new double[m+n-1];
	    int j,k;
	    int lower;
	    int upper;
	    for (k = 0; k < m+n-1; k++) {
	        lower = Math.max(0,k-n+1);
	        upper = Math.min(k, m-1);
	        for (j = lower; j <= upper; j++) {
	        	w[k] += (u[j] * v[k-j]);
	        }
	    }
	    return w;
	}
	
	private double[][] roots(double p[]) {
		int i;
		int n = p.length- 1;
		double A[][] = new double[n][n];
		for (i = 0; i < n-1; i++) {
			A[i+1][i] = 1.0;
		}
		for (i = 0; i < n; i++) {
			A[0][i] = -p[i+1]/p[0];
		}
		// A = V * (diagonal eigenvalues) * V'
        // In EigevalueDecomposition the columns of V represent the eigenvectors
		 double eigenvalueR[] = new double[n];
	     double eI[] = new double[n];
	     double V[][] = new double[n][n];
	     Eigenvalue.decompose( A, V, eigenvalueR, eI );
	     double e[][] = new double[n][2];
	     for (i = 0; i < n; i++) {
	    	 e[i][0] = eigenvalueR[i];
	    	 e[i][1] = eI[i];
	     }
	     return e;
	}
	
	private double[][] cplxpair(double p[][]) {
		int i, j;
		double tolerance = 100.0 * epsilon;
		double realPart;
		double imagPart;
		double ratio;
		boolean isReal;
		double result[][] = new double[p.length][2];
		double minDist;
		double distance;
		double realPart2;
		double imagPart2;
		int conjugateIndex = -1;
		int resultIndex = 0;
		// Sort by increasing real parts
		// If real parts are equal, sort by increasing imaginary parts
		List<realImaginaryItem> list = new ArrayList<realImaginaryItem>();
		for (i = 0; i < p.length; i++) {
            list.add(new realImaginaryItem(p[i][0], p[i][1]));
        }
        Collections.sort(list, new realImaginaryComparator());
        List<Double>realList = new ArrayList<Double>();
        List<realImaginaryItem>complexList = new ArrayList<realImaginaryItem>();
        for (i = 0; i < list.size(); i++) {
            realPart = list.get(i).getReal();
            imagPart = list.get(i).getImaginary();
            if ((realPart == 0.0) && (imagPart == 0.0)) {
            	isReal = true;
            }
            else {
                ratio = Math.abs(imagPart)/(zabs(realPart,imagPart));
                if (ratio < tolerance) {
                	isReal = true;
                }
                else {
                	isReal = false;
                }
            }
            if (isReal) {
            	realList.add(realPart);
            }
            else {
            	complexList.add(new realImaginaryItem(realPart, imagPart));
            }
        } // for (i = 0; i < list.size(); i++)
        int unpaired = 0;
        boolean complexPaired[] = new boolean[complexList.size()];
        for (i = 0; i < complexList.size(); i++) {
        	if (!complexPaired[i]) {
        	    minDist = Double.MAX_VALUE;
        	    realPart = complexList.get(i).getReal();
    	        imagPart = complexList.get(i).getImaginary();
        	    for (j = i+1; j < complexList.size(); j++) {
        	    	if (!complexPaired[j]) {
	        	        realPart2 = complexList.get(j).getReal();
	        	        imagPart2 = complexList.get(j).getImaginary();
	        	        distance = zabs(realPart-realPart2,imagPart+imagPart2);
	        	        if (distance < minDist) {
	        	        	minDist = distance;
	        	        	conjugateIndex = j;
	        	        }
        	    	} // if (!complexPaired[j])
        	    } // for (j = i+1; j < complexList.size(); j++)
        	    if (minDist < tolerance) {
        	    	complexPaired[i] = true;
        	    	complexPaired[j] = true;
        	    	realPart2 = complexList.get(conjugateIndex).getReal();
        	        imagPart2 = complexList.get(conjugateIndex).getImaginary();
        	    	if (imagPart < 0.0) {
        	    	    result[resultIndex][0] = realPart;
        	    	    result[resultIndex++][1] = imagPart;
        	    	    result[resultIndex][0] = realPart;
        	    	    result[resultIndex++][1] = -imagPart;
        	    	}
        	    	else {
        	    		result[resultIndex][0] = realPart;
        	    	    result[resultIndex++][1] = -imagPart;
        	    	    result[resultIndex][0] = realPart;
        	    	    result[resultIndex++][1] = imagPart;	
        	    	}
        	    } // if (minDist < tolerance)
        	    else {
        	    	unpaired++;
        	    }
        	} // if (!complexPaired[i])
        } // for (i = 0; i < complexList.size(); i++)
        if (unpaired > 0) {
        	MipavUtil.displayError("cplxpair found " + unpaired + " complex numbers");
        }
        for (i = 0; i < realList.size(); i++) {
            result[resultIndex][0] = realList.get(i);
            result[resultIndex++][1] = 0.0;
        }
        return result;
	}
	
	/**
     * zabs computes the absolute value or magnitude of a double precision complex variable zr + j*zi.
     * 
     * @param zr double
     * @param zi double
     * 
     * @return double
     */
    private double zabs(final double zr, final double zi) {
        double u, v, q, s;
        u = Math.abs(zr);
        v = Math.abs(zi);
        s = u + v;

        // s * 1.0 makes an unnormalized underflow on CDC machines into a true
        // floating zero
        s = s * 1.0;

        if (s == 0.0) {
            return 0.0;
        } else if (u > v) {
            q = v / u;

            return (u * Math.sqrt(1.0 + (q * q)));
        } else {
            q = u / v;

            return (v * Math.sqrt(1.0 + (q * q)));
        }
    }
	
	private class realImaginaryComparator implements Comparator<realImaginaryItem> {

        /**
         * DOCUMENT ME!
         * 
         * @param o1 DOCUMENT ME!
         * @param o2 DOCUMENT ME!
         * 
         * @return DOCUMENT ME!
         */
        public int compare(final realImaginaryItem o1, final realImaginaryItem o2) {
            final double a = o1.getReal();
            final double b = o2.getReal();
            final double c = o1.getImaginary();
            final double d = o2.getImaginary();

            if (a < b) {
                return -1;
            } else if (a > b) {
                return 1;
            } else if (c < d) {
            	return -1;
            } else if (c > d) {
            	return 1;
            } else {
                return 0;
            }
        }

    }
	
	private class realImaginaryItem {

        /** DOCUMENT ME! */
        private final double real;

        /** DOCUMENT ME! */
        private final double imaginary;

        /**
         * Creates a new realImaginaryItem object.
         * 
         * @param real
         * @param imaginary
         */
        public realImaginaryItem(final double real, final double imaginary) {
            this.real = real;
            this.imaginary = imaginary;
        }

        /**
         * DOCUMENT ME!
         * 
         * @return DOCUMENT ME!
         */
        public double getReal() {
            return real;
        }

        /**
         * DOCUMENT ME!
         * 
         * @return DOCUMENT ME!
         */
        public double getImaginary() {
            return imaginary;
        }

    }
	
	private long nchoosek(int n, int k) {
	    long ans = factorial(n)/(factorial(k) * factorial(n-k));
	    return ans;
	}
	
	/**
     * Returns the factorial of a nonnegative integer.
     *
     * @param   number  integer whose factorial is being returned
     *
     * @return  number!
     */
    public long factorial(int number) {
        long i, j;

        if (number < 0) {
            MipavUtil.displayError("A factorial cannot be performed on a negative number");

            return -1L;
        }

        if (number == 0) {
            return 1L;
        } else {

            for (i = 1, j = 1; i <= number; i++) {
                j = j * i;
            }

            return j;
        }
    }
}
