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
        VOIContour contour = null;
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
		double T[];
		double ip1;
		double im1;
		double ipxdim;
		double imxdim;
		double maxT;
		double thre;
		int pixelsFound;
		int sumX;
		int sumY;
		int cx;
		int cy;
		byte[] m;
		byte[] m2;
		int r = 0;
		int diffy;
		int dy2;
		int diffx;
		int r2;
		int siz;
		int sx;
		int itop;
		int jtop;
		int jmed;
		int j;
		AlgorithmMorphology2D dilateAlgo;
		ModelImage dilateImage;
		int dilateExtents[];
		int kernel;
		float circleDiameter;
		int morphologyMethod;
		int itersDilation;
		int itersErosion;
		int numPruningPixels;
		int edgingType;
		boolean wholeImage;
		byte mcrop[];
		int xbase;
		int ybase;
		byte M[];
		int padSize;
		byte tem[][];
        
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
        	oXdim = xDim;
        	oYdim = yDim;
        }
		
		if (mask == user) {
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
	        contour = (VOIContour)cVOI.getCurves().elementAt(0);
		} // if (mask == user)
        
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
        
        sliceSize = oXdim * oYdim;
        buffer = new double[sliceSize];
        try {
        	image.exportData(0, sliceSize, buffer);
        }
        catch (IOException e) {
        	MipavUtil.displayError("IOException " + e + " on image.exportData(0, sliceize, buffer)");
        	setCompleted(false);
        	return;
        }
        
        if ((mask >= 1) && (mask <= 5)) {
            T = new double[sliceSize];
            for (y = 0; y < oYdim; y++) {
            	for (x = 0; x < oXdim; x++) {
            		index = x + y * oXdim;
            		if (x >= 1) {
            			im1 = buffer[index-1];
            		}
            		else {
            			im1 = 0.0;
            		}
            		if (x < oXdim - 1) {
            			ip1 = buffer[index+1];
            		}
            		else {
            			ip1 = 0.0;
            		}
            		if (y >= 1) {
            			imxdim = buffer[index - oXdim];
            		}
            		else {
            			imxdim = 0.0;
            		}
            		if (y < oYdim - 1) {
            			ipxdim = buffer[index + oXdim];
            		}
            		else {
            			ipxdim = 0;
            		}
            		T[index] = im1 + ip1 + imxdim + ipxdim - 4*buffer[index];
            	}
            } // for (y = 0; y < oYdim; y++)
            maxT = -Double.MAX_VALUE;
            for (i = 0; i < sliceSize; i++) {
                if (Math.abs(T[i]) > maxT) {
                	maxT = Math.abs(T[i]);
                }
            }
            thre = 0.5 * maxT;
            pixelsFound = 0;
            sumX = 0;
            sumY = 0;
            for (y = 0; y < oYdim; y++) {
            	for (x = 0; x < oXdim; x++) {
            		index = x + y * oXdim;
            		if (T[index] > thre) {
            			pixelsFound++;
            			sumX += x;
            			sumY += y;
            		}
            	}
            } // for (y = 0; y < oYdim; y++)
            cx = (int)Math.round((double)sumX/(double)pixelsFound);
            cy = (int)Math.round((double)sumY/(double)pixelsFound);
            
            if ((mask == small) || (mask == whole_small)) {
            	r = 10;
            }
            else if (mask == medium) {
            	r = Math.min(Math.min(cx, oXdim - cx - 1), Math.min(cy, oYdim - cy - 1));
            	r = Math.max(2*r/3, 25);
            }
            else if (mask == large) {
            	r = Math.min(Math.min(cx, oXdim - cx - 1), Math.min(cy, oYdim - cy - 1));
            	r = Math.max(2*r/3, 60);	
            }
            if ((mask == small) || (mask == medium) || (mask == large) || (mask == whole_small)) {
            	m = new byte[sliceSize];
            	r2 = r*r;
                for (y = 0; y < oYdim; y++) {
                	diffy = y - cy;
                	dy2 = diffy * diffy;
                	for (x = 0; x < oXdim; x++) {
                		diffx = x - cx;
                		index = x + y * oXdim;
                		if (diffx*diffx + dy2 < r2) {
                			m[index] = 1;
                		}
                	}
                }
            } // if ((mask == small) || (mask == medium) || (mask == large) || (mask == whole_small)) 
            if ((mask == whole) || (mask == whole_small)) {
            	r = 9;
            	siz = (int)Math.round(Math.ceil(Math.max(oXdim, oYdim)/2.0/(r+1.0))*3*(r+1));
            	m2 = new byte[siz * siz];
            	sx = (int)Math.round(siz/2.0);
            	itop = (int)Math.round(siz/2.0/(r+1.0));
            	jtop = (int)Math.round(0.9*siz/2.0/(r+1.0));
            	jmed = (int)Math.round((1.0 + jtop)/2.0);
            	for (j = 1 - jmed; j <= jtop - jmed; j++) {
            		for (i = 1; i <= itop; i++) {
            		    m2[(2*i-1)*(r+1)-1 + siz*(sx+2*j*(r+1)-1)] = 1;	
            		}
            	}
            	dilateExtents = new int[2];
            	dilateExtents[0] = siz;
            	dilateExtents[1] = siz;
            	dilateImage = new ModelImage(ModelStorageBase.BYTE, dilateExtents, "dilateImage");
            	try {
            		dilateImage.importData(0, m2, true);
            	}
            	catch (IOException e) {
            		MipavUtil.displayError("IOException " + e + " on dilateImage.importData(0, m2, true)");
            		setCompleted(false);
            		return;
            	}
            	kernel = AlgorithmMorphology2D.SIZED_CIRCLE;
            	circleDiameter = (2*r+1)*dilateImage.getFileInfo()[0].getResolutions()[0];
            	morphologyMethod = AlgorithmMorphology2D.DILATE;
            	itersDilation = 1;
            	itersErosion = 0;
            	numPruningPixels = 0;
            	edgingType = 0;
            	wholeImage = true;
            	dilateAlgo = new AlgorithmMorphology2D(dilateImage, kernel, circleDiameter, morphologyMethod, itersDilation,
                        itersErosion, numPruningPixels, edgingType, wholeImage);
                dilateAlgo.run();
                dilateAlgo.finalize();
                dilateAlgo = null;
            	try {
            		dilateImage.exportData(0, siz*siz, m2);
            	}
            	catch(IOException e) {
            		MipavUtil.displayError("IOException " + e + " on dilateImage.exportData(0, siz*siz, m2)");
            		setCompleted(false);
            		return;
            	}
            	dilateImage.disposeLocal();
            	dilateImage = null;
            	mcrop = new byte[sliceSize];
            	xbase = (int)Math.round(siz/2.0 - oXdim/2.0 - 7);
            	ybase = (int)Math.round(siz/2.0 - oYdim/2.0 - 7);
            	for (y = 0; y < oYdim; y++) {
            		for (x = 0; x < oXdim; x++) {
            			mcrop[x + y * oXdim] = m2[x + xbase + (y + ybase) * siz];
            		}
            	}
            	tem = new byte[sliceSize][2];
            	for (y = 0; y < oYdim; y++) {
            		for (x = 0; x < oXdim; x++) {
            			tem[x + y * oXdim][0] = mcrop[x + y * oXdim];
            		}
            	}
            	padSize = (int)Math.floor(2.0*r/3.0);
            	M = new byte[(oXdim + padSize) * (oYdim + padSize)];
            	for (y = 0; y < oYdim ; y++) {
            	    for (x = 0; x < oXdim; x++) {
            	    	M[x + y * (oXdim + padSize)] = mcrop[x + y * oXdim];
            	    }
            	}
            	for (y = 0; y < oYdim; y++) {
            		for (x = 0; x < oXdim; x++) {
            			tem[x + y * oXdim][1] = M[x + padSize + (y + padSize) * (oXdim + padSize)];
            		}
            	}
            } // if ((mask == whole) || (mask == whole_small))
        } // if ((mask >= 1) && (mask <= 5))
        
        // Core function
        if ((method == chan) || (method == vector)) {
        	
        } // if ((method == chan) || (method == vector))
        
        
        
        
        
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