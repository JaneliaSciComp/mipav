package gov.nih.mipav.model.algorithms;


import gov.nih.mipav.model.algorithms.utilities.AlgorithmRGBtoGray;
import gov.nih.mipav.model.structures.*;
import gov.nih.mipav.view.*;

import java.io.*;

	/* Original MATLAB code Copyright (c) 2017, Shreyamsha Kumar B. K.
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
	POSSIBILITY OF SUCH DAMAGE.
	*/

	// 2D NL-means Filter.
	
    // Author : B.K. SHREYAMSHA KUMAR
	// Created on 06-05-2011.
	// Updated on 06-05-2011.

	/*-----------COPYRIGHT NOTICE STARTS WITH THIS LINE------------
	Copyright (c) 2012 B. K. Shreyamsha Kumar 
	All rights reserved.
	 
	Permission is hereby granted, without written agreement and without license or royalty fees, to use, copy, modify, and distribute this code (the source files) and its documentation for any purpose, provided that the copyright notice in its entirety appear in all copies of this code, and the original source of this code, This should be acknowledged in any publication that reports research using this code. The research
	is to be cited in the bibliography as:
	
	B. K. Shreyamsha Kumar, "Image Denoising based on Non Local-means Filter and its Method Noise Thresholding", Signal, Image and Video Processing, Vol. 7, Issue 6, pp. 1211-1227, 2013. (doi: 10.1007/s11760-012-0389-y)
	
	-----------COPYRIGHT NOTICE ENDS WITH THIS LINE------------
	
	This software release consists of an implementation of the algorithm described in the paper:
	B. K. Shreyamsha Kumar, "Image Denoising based on Non Local-means Filter and its Method Noise Thresholding", Signal, Image and Video Processing, Vol. 7, Issue 6, pp. 1211-1227, 2013. (doi: 10.1007/s11760-012-0389-y)
	
	Download the original manuscript from http://link.springer.com/article/10.1007/s11760-012-0389-y
	
	Download manuscript draft from https://sites.google.com/site/shreyamsha in the Publications link.
	
	It consists of the following files:
	
	readme.txt: this file
	
	nlmeans_detail_wt_pgm: main program
	
	support .m files: nlmeans_filt2D.m, threshf.m,bayesthf.m,gauss_ker2D.m


	Support .m files from other sources: vifvec.m, ind2wtree.m, refparams_vecgsm.m, vifsub_est_M.m --> from the author 
	H. R. Sheikh and A. C. Bovik, "Image Information and Visual Quality"., IEEE Transactions on Image Processing, http://live.ece.utexas.edu. 
	
	If you are using this image quality index-visual information fidelity (VIF), please cite it.
	
	Prerequisites for VIF: The Steerable Pyramid toolbox available at http://www.cns.nyu.edu/~lcv/software.html
	*/




public class NLMeans_filt2D extends AlgorithmBase {
	// NL-means filter parameters
	private int ksize = 7; // Neighbor window size Should be odd
	private int ssize = 21; // Search window size Should be odd
	// Sigmas for Gaussian kernel generation
	private double sigmaX = 5.0;
	private double sigmaY = 5.0;
	private double noise_std = 20.0;
	
	// Wavelet Transform Parameters
	int Nlevels = 3;
	int NoOfBands = 3*Nlevels+1;
	String wname = "db8"; // db8 sym8 db16 coif5 bior6.8
	String sorh = "s"; // s or h or t -> trimmed
	
	// Test on lena512.jpg, Barbara512.png, boat512.jpg, and baboon512.jpg
	
	//~ Constructors ---------------------------------------------------------------------------------------------------

    /**
     * AlgorithmBRISK - default constructor.
     */
    public NLMeans_filt2D() { }
    
    public NLMeans_filt2D(ModelImage destImage, ModelImage srcImg, double sigmaX, double sigmaY,
    		int ksize, int ssize, double noise_std) {
    	super(destImage, srcImg);
    	this.sigmaX = sigmaX;
    	this.sigmaY = sigmaY;
    	this.ksize = ksize;
    	this.ssize = ssize;
    	this.noise_std = noise_std;
    }
	/**
     * Starts the program.
     */
    public void runAlgorithm() {
    	int x, y;
    	int xInc;
    	int yInc;
    	double gauss_win[][];
    	double filt_h;
    	int paddedYDim;
    	int paddedXDim;
    	int ii, jj;
    	double xtemp[][];
    	double search_win[][];
    	double euclid_dist[][];
    	double wt_dist[][];
    	int kr,kc;
    	double sq_dist;
    	double n2;
    	double filth2;
    	double weight[][];
    	double sum_wt;
    	double weightn[][];
    	double sum_pix;
    	double im_rec[];
    	
    	if (srcImage == null) {
            displayError("Source Image is null");
            finalize();

            return;
        }
    	
    	int xDim = srcImage.getExtents()[0];
    	int yDim = srcImage.getExtents()[1];
    	int length = xDim*yDim;
    	double srcBuffer[] = new double[length];
    	
       if (srcImage.isColorImage()) {
    	// make grayscale
	       AlgorithmRGBtoGray gAlgo;
	       boolean intensityAverage = false;
	       float threshold = 0.0f;
	       boolean thresholdAverage = false;
	       boolean equalRange = true;
	       float redValue = 0.2989f;
	       float greenValue = 0.5870f;
	       float blueValue = 0.1140f;
	       // minR, minG, minB, maxR, maxG, maxB not used
	       float minR = 0.0f;
	       float minG = 0.0f;
	       float minB = 0.0f;
	       float maxR = 255.0f;
	       float maxG = 255.0f;
	       float maxB = 255.0f;
    	   ModelImage img = new ModelImage(ModelStorageBase.FLOAT, srcImage.getExtents(), srcImage.getImageName() + "_gray");
    	   if (srcImage.getMinR() == srcImage.getMaxR()) {
				redValue = 0.0f;
				greenValue = 0.5f;
				blueValue = 0.5f;
			} else if (srcImage.getMinG() == srcImage.getMaxG()) {
				redValue = 0.5f;
				greenValue = 0.0f;
				blueValue = 0.5f;
			} else if (srcImage.getMinB() == srcImage.getMaxB()) {
				redValue = 0.5f;
				greenValue = 0.5f;
				blueValue = 0.0f;
			} 
    	   
			gAlgo = new AlgorithmRGBtoGray(img, srcImage,
					redValue, greenValue, blueValue, thresholdAverage,
					threshold, intensityAverage, equalRange, minR, maxR,
					minG, maxG, minB, maxB);
			gAlgo.run();
			gAlgo.finalize();
			try {
	    		img.exportData(0, length, srcBuffer);
	    	}
	    	catch (IOException e) {
	    		MipavUtil.displayError("IOException on img.exportData(0, length, srcBuffer");
	    		setCompleted(false);
	    		return;
	    	}
			img.disposeLocal();
			img = null;
       }
       else {
	    	try {
	    		srcImage.exportData(0, length, srcBuffer);
	    	}
	    	catch (IOException e) {
	    		MipavUtil.displayError("IOException on srcImage.exportData(0, length, srcBuffer");
	    		setCompleted(false);
	    		return;
	    	}
       }
    	

        fireProgressStateChanged(srcImage.getImageName(), "NLMeans_filt2D ...");
    	int half_ksize = (int)Math.floor(ksize/2);
    	int half_ssize = (int)Math.floor(ssize/2);	
    	
    	// To take care of boundaries.'
    	paddedYDim = yDim + ssize - 1;
    	paddedXDim = xDim + ssize - 1;
    	double xm[][] = new double[paddedYDim][paddedXDim];
    	for (y = half_ssize; y < yDim + half_ssize; y++) {
    		for (x = half_ssize; x < xDim + half_ssize; x++) {
    		    xm[y][x] = srcBuffer[(x - half_ssize) + (y - half_ssize)*xDim];	
    		}
    	}
    	for (y = 0; y < half_ssize; y++) {
    		for (x = 0; x < paddedXDim; x++) {
    			xm[y][x] = xm[ssize-1-y][x];
    		}
    	}
    	for (y = yDim + half_ssize, yInc = 0; y <= yDim + ssize - 2; y++, yInc++) {
    		for (x = 0; x < paddedXDim; x++) {
    			xm[y][x] = xm[yDim + half_ssize-2-yInc][x];
    		}
    	}
    	for (y = 0; y < paddedYDim; y++) {
    		for (x = 0; x < half_ssize; x++) {
    			xm[y][x] = xm[y][ssize-1-x];
    		}
    	}
    	for (y = 0; y < paddedYDim; y++) {
    		for (x = xDim + half_ssize, xInc = 0; x <= xDim + ssize - 2; x++, xInc++) {
    		    xm[y][x] = xm[y][xDim + half_ssize - 2 - xInc];	
    		}
    	}
    	
    	// Gaussian kernel generation
        gauss_win = gauss_ker2D();    
        
        // NL-means Filter Implementation
        filt_h = 0.55 * noise_std;
        xtemp = new double[ksize][ksize];
        search_win = new double[ssize][ssize];
        euclid_dist = new double[ksize][ksize];
        wt_dist = new double[ksize][ksize];
        n2 = 2*noise_std*noise_std;
        filth2 = filt_h*filt_h;
        weight = new double[ssize-ksize+1][ssize-ksize+1];
        weightn = new double[ssize-ksize+1][ssize-ksize+1];
        im_rec = new double[length];
        for (ii=half_ssize; ii < paddedYDim-half_ssize; ii++) {
            for (jj=half_ssize; jj < paddedXDim-half_ssize; jj++) {
                for (y = 0, yInc = 0; y < ksize; y++, yInc++) {
                	for (x = 0, xInc = 0; x < ksize; x++, xInc++) {
                		xtemp[y][x] = xm[ii-half_ksize+yInc][jj-half_ksize+xInc];
                	}
                }
                for (y = 0, yInc = 0; y < ssize; y++, yInc++) {
                	for (x = 0, xInc = 0; x < ssize; x++, xInc++) {
                		search_win[y][x] = xm[ii-half_ssize+yInc][jj-half_ssize+xInc];
                	}
                }
                sum_wt = 0.0;
                for (kr = 0; kr <= ssize - ksize; kr++) {
                	for (kc = 0; kc <= ssize - ksize; kc++) {
                		sq_dist = 0.0;
                	    for (y = 0; y <= ksize-1; y++) {
                	    	for (x = 0; x <= ksize-1; x++) {
                	    		euclid_dist[y][x] = xtemp[y][x] - search_win[y+kr][x+kc];
                	    		euclid_dist[y][x] = euclid_dist[y][x]*euclid_dist[y][x];
                	    		wt_dist[y][x] = gauss_win[y][x]*euclid_dist[y][x];
                	    		sq_dist += wt_dist[y][x];
                	    	}
                	    }
                	    sq_dist = sq_dist/(ksize * ksize);
                	    if (sq_dist > n2) {
                	    	weight[kr][kc] = Math.exp(-(sq_dist - n2)/filth2);
                	    }
                	    else {
                	    	weight[kr][kc] = 1.0;
                	    }
                	    sum_wt += weight[kr][kc];
                	}
                }
                for (kr = 0; kr <= ssize - ksize; kr++) {
                	for (kc = 0; kc <= ssize - ksize; kc++) {
                		weightn[kr][kc] = weight[kr][kc]/sum_wt;
                	}
                }
                sum_pix = 0.0;
                for (y = half_ksize; y < ssize-half_ksize; y++) {
                	for (x = half_ksize; x < ssize-half_ksize; x++) {
                	    sum_pix += search_win[y][x]*weightn[y-half_ksize][x-half_ksize];	
                	}
                }
                im_rec[(ii-half_ssize)*xDim + (jj-half_ssize)] = sum_pix;
            }
        }
        if (destImage != null) {
        	try {
        		destImage.importData(0, im_rec, true);
        	}
        	catch(IOException e) {
        		MipavUtil.displayError("IOException on destImage.importData(0, im_rec, true)");
        		setCompleted(false);
        		return;
        	}
        }
        else {
        	try {
        		srcImage.importData(0, im_rec, true);
        	}
        	catch(IOException e) {
        		MipavUtil.displayError("IOException on srcImage.importData(0, im_rec, true)");
        		setCompleted(false);
        		return;
        	}
        }
        
        setCompleted(true);
        return;
    }
    
    private double[][] gauss_ker2D() {
    	int x,y;
    	double expy;
    	int half_length = (int)Math.floor(ksize/2);
    	double window[][] = new double[ksize][ksize];
    	for (y = -half_length; y <= half_length; y++) {
    		expy = Math.exp(-y*y/(2.0*sigmaY*sigmaY));
    		for (x = -half_length; x <= half_length; x++) {
    		    window[y+half_length][x+half_length] = expy*Math.exp(-x*x/(2.0*sigmaX*sigmaX));	
    		}
    	}
    	return window;
    }
}