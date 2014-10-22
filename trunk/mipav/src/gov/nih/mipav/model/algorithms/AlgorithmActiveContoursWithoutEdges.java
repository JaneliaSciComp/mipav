package gov.nih.mipav.model.algorithms;


import gov.nih.mipav.model.algorithms.utilities.AlgorithmRGBtoGray;
import gov.nih.mipav.model.structures.*;
import gov.nih.mipav.view.*;

import java.io.IOException;
import java.util.*;

public class AlgorithmActiveContoursWithoutEdges extends AlgorithmBase  {
	
	/**Copyright (c) 2009, Yue Wu
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
	
	%   Active contour with Chen-Vese Method 
%   for image segementation
%
%   Implemented by Yue Wu (yue.wu@tufts.edu)
%   Tufts University
%   Feb 2009
%   http://sites.google.com/site/rexstribeofimageprocessing/
% 
%   all rights reserved 
%   Last update 02/26/2009

% Description: This code implements the paper: "Active Contours Without
% Edges" by Chan and Vese for method 'chen', the paper:"Active Contours Without
% Edges for vector image" by Chan and Vese for method 'vector', and the paper
% "A Multiphase Level Set Framework for Image Segmentation Using the 
% Mumford and Shah Model" by Chan and Vese. 


	This is a port to Java of the MATLAB code in chenvese.m and supporting MATLAB files
	written by Yue Wu.
	
	  References: 1.) Active Contours Without Edges by Tony F. Chan and 
	                 Luminita A. Vese, IEEE Transactions on Image Processing,
	                 Vol. 10, No. 2, February, 2001, pp. 266- 277.
	              2.) Active Contours Without Edges for Vector-Valued Images by
	                 Tony F. Chan, B. Yezrielev, and Luminita A. Vese, Journal of
	                 Visual Communication and Image Representation, 11, 2000, pp. 130-141.
	              3.) A Multiphase Level Set Framework for Image Segmentation Using the 
	                 Mumford and Shah Model by Tony F. Chan and Luminita A. Vese, International
	                 Journal of Computer Vision, 50(3), 2002, pp. 271-293.
	 */
	// User supplied VOI
	private static final int user = 0;
	
	//Create a small circular mask
	private static final int small = 1;
	
	// Create a medium circular mask
	private static final int medium = 2;
	
	// Create a large circular mask
	private static final int large = 3;
	
	// Create a mask with holes around
	private static final int whole = 4;
	
	// Create a two layer mask with one layer small circular mask
	// and the other layer with holes (only works for method multiphase)
	private static final int whole_small = 5;
	
	private int mask;
	
	
	// The total mumber of iterations
	private int numIter;
	
	// The length parameter which has a scaling role
	// If we have to detect all or as many objects as possible and of any size, then mu
	// should be small.  If we have to detect only larger objects (for example objects
	// formed by grouping), and to not detect smaller objects (like points, due to the
	// noise), then mu has to be larger.
	// In reference 1 mu varies from 3.3E-6 * 255 * 255 to 2 * 255 * 255
	private double mu = 0.2;
	
	// General Chan-Vese method
	private static final int chan = 1;
	
	// Chan-Vese method for vector (RGB or multispectral) image
	private static final int vector = 2;
	
	// Chan-Vese method for multiphase (2 phases applied here)
	// Two level set functions result in 4 segments
	private static final int multiphase = 3;
	
	private int method = chan;
	
	private double epsilon = 1.0;
	
	// The step space
	private double h = 1.0;
	
	// The time step
	// A value of delt = 0.01 was used in 1 example in reference 1.
	private double delt = 0.1;
	
	private double lambda1 = 1.0;
	
	private double lambda2 = 1.0;
	
	// Area parameter was set to 0.02 * 255 * 255 in 1 example in reference 1
	private double nu = 0.0;
	
	// One example in reference 1 had 1 iteration of reinitialization and 4
	// examples had 5 iterations of reinitialization.
	private int reinitializations = 0;

	
	public AlgorithmActiveContoursWithoutEdges(ModelImage srcImg, int mask, int numIter, double mu, int method) {
		super(null, srcImg);
		this.mask = mask;
		this.numIter = numIter;
		this.mu = mu;
		this.method = method;
	}
	
	public void runAlgorithm() {
		int i;
		int nVOI;
        ViewVOIVector VOIs;
        VOI cVOI;
        VOI c2VOI;
        VOIContour contour;
        int xDim;
        int yDim;
        int sliceSize;
        double buffer[];
        boolean snear[] = new boolean[1];
		int i1[] = new int[1];
		int i2[] = new int[1];
		double phi[];
		double nextPhi[];
		double H[];
		int x;
		int y;
		int index;
		double c1NumCount;
	    double c1DenomCount;
		double c1;
		double c2NumCount;
		double c2DenomCount;
		double c2;
		boolean convergence;
		double phixplus;
		double phixminus;
		double phiyplus;
		double phiyminus;
		double delh;
		double hsquared;
		double phixdiff;
		double phiydiff;
		double s;
		AlgorithmTransform transform;
		TransMatrix xfrm;
		int oXdim;
		int oYdim;
		float oXres;
		float oYres;
		boolean transformVOI = true;
		boolean clip = true;
		boolean pad = false;
		ModelImage image;
		int numFound = 0;
		int n1 = -1;
		int n2 = -2;
		float redValue;
		float greenValue;
		float blueValue;
		AlgorithmRGBtoGray gAlgo;
		boolean thresholdAverage = false;
		float threshold = 0.0f;
		boolean intensityAverage = false;
		boolean equalRange = true;
		float minR = 0.0f;
		float minG = 0.0f;
		float minB = 0.0f;
		float maxR;
		float maxG;
		float maxB;
        
        fireProgressStateChanged(srcImage.getImageName(), "Evolving the level set ...");
        
        // Initializations on input image and VOIs
        // Resize original image
        xDim = srcImage.getExtents()[0];
        yDim = srcImage.getExtents()[1];
        // Resize scale
        s = 200.0/Math.min(xDim,yDim);
        if (s < 1.0) {
        	xfrm = new TransMatrix(3);
        	xfrm.identity();
        	oXdim = (int)Math.round(xDim * s);
            oYdim = (int)Math.round(yDim * s);
            oXres = (srcImage.getFileInfo(0).getResolutions()[0] * xDim)/ oXdim;
            oYres = (srcImage.getFileInfo(0).getResolutions()[1] * yDim)/ oYdim;
        	transform = new AlgorithmTransform(srcImage, xfrm, AlgorithmTransform.BILINEAR, oXres, oYres, oXdim,
                    oYdim, transformVOI, clip, pad);
            transform.run();
            image = transform.getTransformedImage();
            image.calcMinMax();
            transform.disposeLocal();
            transform = null;
        } // if (s < 1.0)
        else {
        	image = srcImage;
        }
		
		VOIs = image.getVOIs();
        nVOI = VOIs.size();

        numFound = 0;
        for (i = 0; i < nVOI; i++) {

            if (VOIs.VOIAt(i).getCurveType() == VOI.CONTOUR) {
                if (numFound == 0) {
                	n1 = i;
                }
                else if (numFound == 1) {
                    n2 = i;	
                }
                else {
                    MipavUtil.displayError("Exiting! Found an impossible third VOI.CONTOUR");
                    setCompleted(false);
                    return;
                }
                numFound++;
            }
        }
        
        if ((numFound == 2) && ((method == chan) || (method == vector))) {
        	MipavUtil.displayError("Found an impossible second VOI.CONTOUR for method == chen or method == vector");
        	setCompleted(false);
        	return;
        }
        
        if ((numFound == 1) && (method == multiphase)) {
        	MipavUtil.displayError("multiphase requires two VOI.CONTOUR but only one is present");
        	setCompleted(false);
        	return;
        }

        cVOI = VOIs.VOIAt(n1);
        if (method == multiphase) {
        	c2VOI = VOIs.VOIAt(n2);
        }
        
        if ((method == chan) && (image.isColorImage())) {
            if (image.getMinR() == image.getMaxR()) {
                redValue = 0.0f;
                greenValue = 0.5f;
                blueValue = 0.5f;
            }
            else if (image.getMinG() == image.getMaxG()) {
            	redValue = 0.5f;
            	greenValue = 0.0f;
            	blueValue = 0.5f;
            }
            else if (image.getMinB() == image.getMaxB()) {
            	redValue = 0.5f;
            	greenValue = 0.5f;
            	blueValue = 0.0f;
            }
            else {
            	redValue = (float)(1.0/3.0);
            	greenValue = redValue;
            	blueValue = redValue;
            }
            maxR = (float)image.getMaxR();
            maxG = (float)image.getMaxG();
            maxB = (float)image.getMaxB();
            gAlgo = new AlgorithmRGBtoGray(image, redValue, greenValue, blueValue, thresholdAverage, threshold, intensityAverage,
            		equalRange, minR, maxR, minG, maxG, minB, maxB);
            gAlgo.run();
            gAlgo.finalize();
        } // if ((method == chen) && (image.isColorImage()))
        
        // Core function
        if ((method == chan) || (method == vector)) {
        	
        } // if ((method == chan) || (method == vector))
        contour = (VOIContour)cVOI.getCurves().elementAt(0);
        
        
        sliceSize = xDim * yDim;
        buffer = new double[sliceSize];
        try {
        	srcImage.exportData(0, sliceSize, buffer);
        }
        catch (IOException e) {
        	MipavUtil.displayError("IOException " + e + " on srcImage.exportData(0, sliceize, buffer)");
        	setCompleted(false);
        	return;
        }
        
        phi = new double[sliceSize];
        nextPhi = new double[sliceSize];
        H = new double[sliceSize];
        for (y = 0; y < yDim; y++) {
        	for (x = 0; x < xDim; x++) {
        	    index = x + y * xDim;
        	    phi[index] = contour.pinpol((double)x, (double)y, snear, i1, i2);
        	    H[index] = 0.5 * (1 + (2.0/Math.PI)*Math.atan(phi[index]/epsilon));
        	}
        }
        
        c1NumCount = 0.0;
        c1DenomCount = 0;
        c2NumCount = 0.0;
        c2DenomCount = 0;
        for (i = 0; i < sliceSize; i++) {
        	if (phi[i] >= 0.0) {
        		// On voi boundary or in voi
        		c1NumCount += (buffer[i]*H[i]);
        		c1DenomCount += H[i];
        	}
        	else {
        		// Outside voi boundary
        		c2NumCount += (buffer[i] * (1.0 - H[i]));
        		c2DenomCount += (1.0 - H[i]);
        	}
        } // for (i = 0; i < sliceSize; i++)
        c1 = c1NumCount/c1DenomCount;
        c2 = c2NumCount/c2DenomCount;
        
        convergence = false;
        while (!convergence) {
            for (y = 1; y < yDim - 1; y++) {
            	for (x = 1; x < xDim - 1; x++) {
            		index = x + y * xDim;
            		phixminus = phi[index] - phi[index-1];
            		phixplus = phi[index+1] - phi[index];
            		phiyminus = phi[index] - phi[index - xDim];
            		phiyplus = phi[index + xDim] - phi[index];
            		delh = (1.0/(Math.PI))*(epsilon/((epsilon*epsilon) + (phi[index]*phi[index])));
            		hsquared = h * h;
            		phixdiff = phi[index+1] - phi[index-1];
            		phiydiff = phi[index+xDim] - phi[index-xDim];
            	}
            }
        } // while (!convergence)
		
	}
}