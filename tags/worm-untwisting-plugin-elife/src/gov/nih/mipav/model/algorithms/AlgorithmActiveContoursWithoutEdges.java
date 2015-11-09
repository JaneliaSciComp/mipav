package gov.nih.mipav.model.algorithms;


import gov.nih.mipav.model.algorithms.utilities.AlgorithmRGBtoGray;
import gov.nih.mipav.model.structures.*;
import gov.nih.mipav.view.*;

import java.awt.Color;
import java.io.IOException;

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
	// Only for chan and vector methods
	private static final int small = 1;
	
	// Create a medium circular mask
	// Only for chan and vector methods
	private static final int medium = 2;
	
	// Create a large circular mask
	// Only for chan and vector methods
	private static final int large = 3;
	
	// Create a mask with holes all over the mask
	// Can be used in all methods
	private static final int holes = 4;
	
	// Create a two layer mask with one layer small circular mask
	// and the other layer with holes (only works for method twophase)
	private static final int holes_small = 5;
	
	private int maskType;
	
	
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
	
	// Chan-Vese method for twophase (2 phases applied here)
	// Two level set functions result in 4 segments
	private static final int twophase = 3;
	
	private int method = chan;
	
	// epsilon = D1MACH(4)
    // Machine epsilon is the smallest positive epsilon such that
    // (1.0 + epsilon) != 1.0.
    // epsilon = 2**(1 - doubleDigits) = 2**(1 - 53) = 2**(-52)
    // epsilon = 2.2204460e-16
    // epsilon is called the largest relative spacing
    private double epsilon = Math.pow(2, -52);

	
	public AlgorithmActiveContoursWithoutEdges(ModelImage destImg, ModelImage srcImg, int maskType, int numIter, double mu, int method) {
		super(destImg, srcImg);
		this.maskType = maskType;
		this.numIter = numIter;
		this.mu = mu;
		this.method = method;
	}
	
	public void runAlgorithm() {
		int i;
		int nVOI;
        ViewVOIVector VOIs;
        int xDim;
        int yDim;
        int sliceSize;
        double buffer[];
		int x;
		int y;
		int index;
		double c1;
		double c2;
		ModelImage image = null;
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
		byte mask1[] = null;
		byte mask2[] = null;
		byte m2[] = null;
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
		boolean wholeImage = true;
		int xbase;
		int ybase;
		byte M[];
		int padSize;
		ModelImage uByteImage = null;
		int IDOffset;
		boolean XOR;
		boolean onlyActive;
		ModelImage distImage;
		int extents[];
		AlgorithmMorphology2D distMapAlgo2D;
		float FGDist[];
		float BGDist[];
		double phi0[];
		double phi1[];
		double phi2[];
	    int n;
	    int layer = 1;
	    int inidx;
	    int outidx;
	    double forceImage[];
	    int firstColor = 1;
	    int secondColor = 2;
	    double L[];
	    int offset;
	    double Heav[];
	    double Heav1[];
	    double Heav2[];
	    double sumLH;
	    double sumL1mH;
	    double force[];
	    double kap[];
	    double maxKap;
	    double maxForce;
	    double dt;
	    double old[];
	    double neww[];
	    boolean indicator;
	    byte seg[];
	    AlgorithmVOIExtraction VOIExtractionAlgo;
	    int nb1Num;
	    int nb2Num;
	    int c11Num;
	    int c12Num;
	    int c21Num;
	    int c22Num;
	    int nb1Index[];
	    int nb2Index[];
	    int cc11Index[];
	    int cc12Index[];
	    int cc21Index[];
	    int cc22Index[];
	    int nb1;
	    int nb2;
	    int i11;
	    int i12;
	    int i21;
	    int i22;
	    double f_image11[];
	    double f_image12[];
	    double f_image21[];
	    double f_image22[];
	    double c11;
	    double c12;
	    double c21;
	    double c22;
	    double curvature[];
	    double curvature1[];
	    double curvature2[];
	    double fim1[];
	    double fim2[];
	    double maxfim1;
	    double maxfim2;
	    double force1[];
	    double force2[];
	    double oldm[][];
	    double newm[][];
	    byte seg11[];
	    byte seg12[];
	    byte seg21[];
	    byte seg22[];
	    ModelImage erodeImage;
	    AlgorithmMorphology2D erodeAlgo;
        
        fireProgressStateChanged(srcImage.getImageName(), "Evolving the level set ...");
        
        if ((method == twophase) && ((maskType == small) || (maskType == medium) || (maskType == large))) {
        	MipavUtil.displayError("twophase requires 2 masks but only gets one");
        	setCompleted(false);
        	return;
        }
        
        // Initializations on input image and VOIs
        // Resize original image
        xDim = srcImage.getExtents()[0];
        yDim = srcImage.getExtents()[1];
        if (srcImage.isColorImage()) {
            image = (ModelImage)srcImage.clone();
        }
        else {
        	image = srcImage;
        }
        sliceSize = xDim * yDim;
        extents = new int[2];
        extents[0] = xDim;
        extents[1] = yDim;
        
        if (image.isColorImage()) {
            if (image.getMinR() == image.getMaxR()) {
                redValue = 0.0f;
                greenValue = 0.5f;
                blueValue = 0.5f;
                layer = 2;
                firstColor = 2;
                secondColor = 3;
            }
            else if (image.getMinG() == image.getMaxG()) {
            	redValue = 0.5f;
            	greenValue = 0.0f;
            	blueValue = 0.5f;
            	layer = 2;
            	firstColor = 1;
            	secondColor = 3;
            }
            else if (image.getMinB() == image.getMaxB()) {
            	redValue = 0.5f;
            	greenValue = 0.5f;
            	blueValue = 0.0f;
            	layer = 2;
            }
            else {
            	redValue = (float)(1.0/3.0);
            	greenValue = redValue;
            	blueValue = redValue;
            	layer = 3;
            	
            }
            maxR = (float)image.getMaxR();
            maxG = (float)image.getMaxG();
            maxB = (float)image.getMaxB();
            gAlgo = new AlgorithmRGBtoGray(image, redValue, greenValue, blueValue, thresholdAverage, threshold, intensityAverage,
            		equalRange, minR, maxR, minG, maxG, minB, maxB);
            gAlgo.run();
            gAlgo.finalize();
            if (method == chan) {
                layer = 1;
            }
        } // if (image.isColorImage())
		
		if (maskType == user) {
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
	        	MipavUtil.displayError("Found an impossible second VOI.CONTOUR for method == chan or method == vector");
	        	setCompleted(false);
	        	return;
	        }
	        
	        if ((numFound == 1) && (method == twophase)) {
	        	MipavUtil.displayError("twophase requires two VOI.CONTOUR but only one is present");
	        	setCompleted(false);
	        	return;
	        }
	
	        for (i = 0; i < nVOI; i++) {
	        	if (i == n1) {
	        		VOIs.VOIAt(i).setAllActive(true);
	        	}
	        	else {
	        		VOIs.VOIAt(i).setAllActive(false);
	        	}
	        }
	        
	        IDOffset = 1;
	        XOR = false;
	        onlyActive = true;
	        uByteImage = image.generateUnsignedByteImage(IDOffset, XOR, onlyActive);
	        mask1 = new byte[sliceSize];
	        try {
	        	uByteImage.exportData(0, sliceSize, mask1);
	        }
	        catch (IOException e) {
	        	MipavUtil.displayError("IOException " + e + "on uByteImage.exportData(0, sliceSize, mask1");
	        	setCompleted(false);
	        	return;
	        }
	        uByteImage.disposeLocal();
	        uByteImage = null;
	        if (method == twophase) {
	        	mask2 = new byte[sliceSize];
	        	for (i = 0; i < nVOI; i++) {
		        	if (i == n2) {
		        		VOIs.VOIAt(i).setAllActive(true);
		        	}
		        	else {
		        		VOIs.VOIAt(i).setAllActive(false);
		        	}
		        }
	        	IDOffset = 1;
		        XOR = false;
		        onlyActive = true;
		        uByteImage = image.generateUnsignedByteImage(IDOffset, XOR, onlyActive);
		        try {
		        	uByteImage.exportData(0, sliceSize, mask2);
		        }
		        catch (IOException e) {
		        	MipavUtil.displayError("IOException " + e + "on uByteImage.exportData(0, sliceSize, mask2");
		        	setCompleted(false);
		        	return;
		        }
		        uByteImage.disposeLocal();
		        uByteImage = null;
	        } // if (method == twophase)
		} // if (mask == user)
        
        buffer = new double[sliceSize];
        try {
        	image.exportData(0, sliceSize, buffer);
        }
        catch (IOException e) {
        	MipavUtil.displayError("IOException " + e + " on image.exportData(0, sliceize, buffer)");
        	setCompleted(false);
        	return;
        }
        
        if ((maskType >= 1) && (maskType <= 5)) {
            T = new double[sliceSize];
            for (y = 0; y < yDim; y++) {
            	for (x = 0; x < xDim; x++) {
            		index = x + y * xDim;
            		if (x >= 1) {
            			im1 = buffer[index-1];
            		}
            		else {
            			im1 = 0.0;
            		}
            		if (x < xDim - 1) {
            			ip1 = buffer[index+1];
            		}
            		else {
            			ip1 = 0.0;
            		}
            		if (y >= 1) {
            			imxdim = buffer[index - xDim];
            		}
            		else {
            			imxdim = 0.0;
            		}
            		if (y < yDim - 1) {
            			ipxdim = buffer[index + xDim];
            		}
            		else {
            			ipxdim = 0;
            		}
            		T[index] = im1 + ip1 + imxdim + ipxdim - 4*buffer[index];
            	}
            } // for (y = 0; y < yDim; y++)
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
            for (y = 0; y < yDim; y++) {
            	for (x = 0; x < xDim; x++) {
            		index = x + y * xDim;
            		if (Math.abs(T[index]) > thre) {
            			pixelsFound++;
            			sumX += x;
            			sumY += y;
            		}
            	}
            } // for (y = 0; y < yDim; y++)
            cx = (int)Math.round((double)sumX/(double)pixelsFound);
            cy = (int)Math.round((double)sumY/(double)pixelsFound);
            mask1 = new byte[sliceSize];
            if (method == twophase) {	
            	mask2 = new byte[sliceSize];
            }
           
            if ((maskType == small) || (maskType == holes_small)) {
            	r = 10;
            }
            else if (maskType == medium) {
            	r = Math.min(Math.min(cx, xDim - cx - 1), Math.min(cy, yDim - cy - 1));
            	r = Math.max(2*r/3, 25);
            }
            else if (maskType == large) {
            	r = Math.min(Math.min(cx, xDim - cx - 1), Math.min(cy, yDim - cy - 1));
            	r = Math.max(2*r/3, 60);	
            }
            if ((maskType == small) || (maskType == medium) || (maskType == large) || (maskType == holes_small)) {
            	r2 = r*r; 
                for (y = 0; y < yDim; y++) {
                	diffy = y - cy;
                	dy2 = diffy * diffy;
                	for (x = 0; x < xDim; x++) {
                		diffx = x - cx;
                		index = x + y * xDim;
                		if (diffx*diffx + dy2 < r2) {
                			if (maskType == holes_small) {
                			    mask2[index] = 1;
                			}
                			else {
                				mask1[index] = 1;
                			}
                		}
                	}
                }
            } // if ((maskType == small) || (maskType == medium) || (maskType == large) || (maskType == holes_small)) 
            if ((maskType == holes) || (maskType == holes_small)) {
            	r = 9;
            	siz = (int)Math.round(Math.ceil(Math.max(xDim, yDim)/2.0/(r+1.0))*3*(r+1));
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
            	xbase = (int)Math.round(siz/2.0 - xDim/2.0 - 7);
            	ybase = (int)Math.round(siz/2.0 - yDim/2.0 - 7);
            	for (y = 0; y < yDim; y++) {
            		for (x = 0; x < xDim; x++) {
            			mask1[x + y * xDim] = m2[x + xbase + (y + ybase) * siz];
            		}
            	}
            	if ((maskType == holes) && (method == twophase)) {
	            	padSize = (int)Math.floor(2.0*r/3.0);
	            	M = new byte[(xDim + padSize) * (yDim + padSize)];
	            	for (y = 0; y < yDim ; y++) {
	            	    for (x = 0; x < xDim; x++) {
	            	    	M[x + y * (xDim + padSize)] = mask1[x + y * xDim];
	            	    }
	            	}
	            	for (y = 0; y < yDim; y++) {
	            		for (x = 0; x < xDim; x++) {
	            			mask2[x + y * xDim] = M[x + padSize + (y + padSize) * (xDim + padSize)];
	            		}
	            	}
            	} // if ((maskType == holes) && (method == twophase))
            } // if ((maskType == holes) || (maskType == holes_small))
        } // if ((maskType >= 1) && (maskType <= 5))
        
        // Core function
        if ((method == chan) || (method == vector)) {
        	seg = new byte[sliceSize];
            // SDF
        	// Get the distance map of the initial mask
        	distImage = new ModelImage(ModelStorageBase.UBYTE, extents, "distImage");
        	// AlgorithmMorphology2D requires BOOLEAN, BYTE, UBYTE, SHORT, or USHORT for entry 
        	// Reallocated to float in AlgorithmMorphology2D distanceMap
        	try {
        		distImage.importData(0, mask1, true);
        	}
        	catch (IOException e) {
        		MipavUtil.displayError("IOexception " + e + " on distance.importData(0, mask1, true");
        		setCompleted(false);
        		return;
        	}
        	kernel = 0;
        	distMapAlgo2D = new AlgorithmMorphology2D(distImage, kernel, 0,
                    AlgorithmMorphology2D.DISTANCE_MAP, 0, 0, 0, 0,
                    wholeImage);
        	distMapAlgo2D.run();
        	distMapAlgo2D.finalize();
        	distMapAlgo2D = null;
        	FGDist = new float[sliceSize];
        	try {
        		distImage.exportData(0, sliceSize, FGDist);
        	}
        	catch (IOException e) {
        		MipavUtil.displayError("IOException " + e + " on distImage.exportData(0, sliceSize, FGDist)");
        		setCompleted(false);
        		return;
        	}
        	distImage.reallocate(ModelStorageBase.UBYTE);
        	try {
        		distImage.importData(0, mask1, true);
        	}
        	catch (IOException e) {
        		MipavUtil.displayError("IOexception " + e + " on distance.importData(0, mask1, true");
        		setCompleted(false);
        		return;
        	}
        	distMapAlgo2D = new AlgorithmMorphology2D(distImage, kernel, 0,
                    AlgorithmMorphology2D.BG_DISTANCE_MAP, 0, 0, 0, 0,
                    wholeImage);
        	distMapAlgo2D.run();
        	distMapAlgo2D.finalize();
        	distMapAlgo2D = null;
        	BGDist = new float[sliceSize];
        	try {
        		distImage.exportData(0, sliceSize, BGDist);
        	}
        	catch (IOException e) {
        		MipavUtil.displayError("IOException " + e + " on distImage.exportData(0, sliceSize, BGDist)");
        		setCompleted(false);
        		return;
        	}
        	distImage.disposeLocal();
        	distImage = null;
        	phi0 = new double[sliceSize];
        	for (i = 0; i < sliceSize; i++) {
        		phi0[i] = BGDist[i] - FGDist[i] + mask1[i] - 0.5;
        	}
    
        	// End intialization
        	
        	L = new double[sliceSize];
        	force = new double[sliceSize];
        	old = new double[sliceSize];
     	    neww = new double[sliceSize];
     	    // Main loop
        	iloop: for (n = 1; n <= numIter; n++) {
        		inidx = 0;
            	outidx = 0;
        	    for (i = 0; i < sliceSize; i++) {
        	    	// frontground index
        	        if (phi0[i] >= 0) {
        	        	inidx++;
        	        }
        	        // background index
        	        else {
        	        	outidx++;
        	        }
        	    } // for (i = 0; i < sliceSize; i++)
        	    // Initial image force for each layer
        	    forceImage = new double[sliceSize];
        	    for (i = 1; i <= layer; i++) {
        	        if (srcImage.isColorImage() && method != chan) {
        	        	if (i == 1) {
        	        		offset = firstColor;
        	        	}
        	        	else if (i == 2) {
        	        		offset = secondColor;
        	        	}
        	        	else {
        	        		offset = 3;
        	        	}
        	        	try {
        	                srcImage.exportRGBDataNoLock(offset, 0, sliceSize, L);	
        	        	}
        	        	catch (IOException e) {
        	        		MipavUtil.displayError("IOException " + e + " on srcImage.exportRGBDataNoLock(offset, 0, sliceSize, L)");
        	        		setCompleted(false);
        	        		return;
        	        	}
        	        } // if (srcImage.isColorImage() && method != chan)
        	        else {
        	        	try {
        	        	    image.exportData(0, sliceSize, L);
        	        	}
        	        	catch (IOException e) {
        	        		MipavUtil.displayError("IOException " + e + " on image.exportData(0, sliceSize, L)");
        	        		setCompleted(false);
        	        		return;
        	        	}	
        	        }
        	        sumLH = 0.0;
        	        sumL1mH = 0.0;
        	        Heav = Heaviside(phi0);
        	        for (j = 0; j < sliceSize; j++) {
        	            sumLH += (L[j] * Heav[j]);
        	            sumL1mH += (L[j] * (1.0 - Heav[j]));
        	        } // for (j = 0; j < sliceSize; j++)
        	        c1 = sumLH/(inidx + epsilon);
        	        c2 = sumL1mH/(outidx + epsilon);
        	        // Sum image force on all components (used for vector image)
        	        // If method == chan, this loop is only executed once as a result of layer == 1.
        	        for (j = 0; j < sliceSize; j++) {
        	        	forceImage[j] = -(L[j]-c1)*(L[j]-c1) + (L[j]-c2)*(L[j]-c2) + forceImage[j];
        	        }
        	    } // for (i = 1; i <= layer; i++)
        	    
        	    // Calculate the external force of the image   
        	    maxKap = -Double.MAX_VALUE;
        	    kap = kappa(phi0, xDim);
        	    for (i = 0; i < sliceSize; i++) {
        	        if (Math.abs(kap[i]) > maxKap) {
        	        	maxKap = Math.abs(kap[i]);
        	        }
        	    }
        	    for (i = 0; i < sliceSize; i++) {
        	    	force[i] = mu * kap[i]/maxKap + 1.0/layer*forceImage[i];
        	    }
        	    
        	    // Normalize the force
        	    maxForce = -Double.MAX_VALUE;
        	    for (i = 0; i < sliceSize; i++) {
        	    	if (Math.abs(force[i]) > maxForce) {
        	    		maxForce = Math.abs(force[i]);
        	    	}
        	    }
        	    
        	    for (i = 0; i < sliceSize; i++) {
        	    	force[i] = force[i]/maxForce;
        	    }
        	    
        	    // Get stepsize dt
        	    dt = 0.5;
        	    
        	    // Get parameters for checking whether to stop
        	    for (i = 0; i < sliceSize; i++) {
        	    	old[i] = phi0[i];
        	    	phi0[i] = phi0[i] + dt * force[i];
        	    	neww[i] = phi0[i];
        	    } 
        	    indicator = checkstop(old, neww, dt);
        	    if (indicator) {
        	        break iloop;    
        	    } // if (indicator)
        	} // iloop: for (n = 1; n <= numIter; n++)
        	if (srcImage.isColorImage()) {
    	    	image.disposeLocal();
    	    	image = null;
    	    }
        	// Get mask from levelset
        	for (i = 0; i < sliceSize; i++) {
	        	if (phi0[i] <= 0.0) {
	        		seg[i] = 1;
	        	}
	        } // for (i = 0; i < sliceSize; i++)
        	try {
        		destImage.importData(0, seg, true);
        	}
        	catch (IOException e) {
        		MipavUtil.displayError("IOExcepion " + e + " on maskImage.importData(0, seg, true)");
        		setCompleted(false);
        		return;
        	}
        	VOIExtractionAlgo = new AlgorithmVOIExtraction(destImage);
        	VOIExtractionAlgo.run();
        	VOIExtractionAlgo.finalize();
        	VOIExtractionAlgo = null;
            
            VOIVector kVOIs = destImage.getVOIs();
            if (kVOIs == null) {
            	MipavUtil.displayError("destImage.getVOIs() == null");
            	setCompleted(false);
            	return;
            }
            if (kVOIs.size() == 0) {
            	MipavUtil.displayError("destImage.getVOIs().size() == 0");
            	setCompleted(false);
            	return;
            }
            for (i = 0; i < kVOIs.size(); i++ )
            {
                VOI kCurrentGroup = kVOIs.get(i);
                kCurrentGroup.setAllActive(true);
            }
            destImage.groupVOIs();
            kVOIs = destImage.getVOIs();
            srcImage.setVOIs(kVOIs);
            destImage.resetVOIs();
            setCompleted(true);
            return;
        } // if ((method == chan) || (method == vector))
        else if (method == twophase) {
        	if (srcImage.isColorImage()) {
        	    image.disposeLocal();
        	    image = null;
        	}
        	seg11 = new byte[sliceSize];
        	seg12 = new byte[sliceSize];
        	seg21 = new byte[sliceSize];
        	seg22 = new byte[sliceSize];
            // Initializations
        	// Get the distance map of the initial massk
        	distImage = new ModelImage(ModelStorageBase.UBYTE, extents, "distImage");
        	// AlgorithmMorphology2D requires BOOLEAN, BYTE, UBYTE, SHORT, or USHORT for entry 
        	// Reallocated to float in AlgorithmMorphology2D distanceMap
        	try {
        		distImage.importData(0, mask1, true);
        	}
        	catch (IOException e) {
        		MipavUtil.displayError("IOexception " + e + " on distance.importData(0, mask1, true");
        		setCompleted(false);
        		return;
        	}
        	kernel = 0;
        	distMapAlgo2D = new AlgorithmMorphology2D(distImage, kernel, 0,
                    AlgorithmMorphology2D.DISTANCE_MAP, 0, 0, 0, 0,
                    wholeImage);
        	distMapAlgo2D.run();
        	distMapAlgo2D.finalize();
        	distMapAlgo2D = null;
        	FGDist = new float[sliceSize];
        	try {
        		distImage.exportData(0, sliceSize, FGDist);
        	}
        	catch (IOException e) {
        		MipavUtil.displayError("IOException " + e + " on distImage.exportData(0, sliceSize, FGDist)");
        		setCompleted(false);
        		return;
        	}
        	distImage.reallocate(ModelStorageBase.UBYTE);
        	try {
        		distImage.importData(0, mask1, true);
        	}
        	catch (IOException e) {
        		MipavUtil.displayError("IOexception " + e + " on distance.importData(0, mask1, true");
        		setCompleted(false);
        		return;
        	}
        	distMapAlgo2D = new AlgorithmMorphology2D(distImage, kernel, 0,
                    AlgorithmMorphology2D.BG_DISTANCE_MAP, 0, 0, 0, 0,
                    wholeImage);
        	distMapAlgo2D.run();
        	distMapAlgo2D.finalize();
        	distMapAlgo2D = null;
        	BGDist = new float[sliceSize];
        	try {
        		distImage.exportData(0, sliceSize, BGDist);
        	}
        	catch (IOException e) {
        		MipavUtil.displayError("IOException " + e + " on distImage.exportData(0, sliceSize, BGDist)");
        		setCompleted(false);
        		return;
        	}
        	phi1 = new double[sliceSize];
        	for (i = 0; i < sliceSize; i++) {
        		phi1[i] = BGDist[i] - FGDist[i] + mask1[i] - 0.5;
        	}
        	distImage.reallocate(ModelStorageBase.UBYTE);
        	try {
        		distImage.importData(0, mask2, true);
        	}
        	catch (IOException e) {
        		MipavUtil.displayError("IOexception " + e + " on distance.importData(0, mask2, true");
        		setCompleted(false);
        		return;
        	}
        	distMapAlgo2D = new AlgorithmMorphology2D(distImage, kernel, 0,
                    AlgorithmMorphology2D.DISTANCE_MAP, 0, 0, 0, 0,
                    wholeImage);
        	distMapAlgo2D.run();
        	distMapAlgo2D.finalize();
        	distMapAlgo2D = null;
        	try {
        		distImage.exportData(0, sliceSize, FGDist);
        	}
        	catch (IOException e) {
        		MipavUtil.displayError("IOException " + e + " on distImage.exportData(0, sliceSize, FGDist)");
        		setCompleted(false);
        		return;
        	}
        	distImage.reallocate(ModelStorageBase.UBYTE);
        	try {
        		distImage.importData(0, mask2, true);
        	}
        	catch (IOException e) {
        		MipavUtil.displayError("IOexception " + e + " on distance.importData(0, mask2, true");
        		setCompleted(false);
        		return;
        	}
        	distMapAlgo2D = new AlgorithmMorphology2D(distImage, kernel, 0,
                    AlgorithmMorphology2D.BG_DISTANCE_MAP, 0, 0, 0, 0,
                    wholeImage);
        	distMapAlgo2D.run();
        	distMapAlgo2D.finalize();
        	distMapAlgo2D = null;
        	try {
        		distImage.exportData(0, sliceSize, BGDist);
        	}
        	catch (IOException e) {
        		MipavUtil.displayError("IOException " + e + " on distImage.exportData(0, sliceSize, BGDist)");
        		setCompleted(false);
        		return;
        	}
        	phi2 = new double[sliceSize];
        	for (i = 0; i < sliceSize; i++) {
        		phi2[i] = BGDist[i] - FGDist[i] + mask2[i] - 0.5;
        	}
        	
        	L = new double[sliceSize];
        	// Main loop
        	iloop2: for (n = 1; n <= numIter; n++) {
        		nb1Num = 0;
        		nb2Num = 0;
            	c11Num = 0;
            	c12Num = 0;
            	c21Num = 0;
            	c22Num = 0;
        	    for (i = 0; i < sliceSize; i++) {
        	    	// Narrow band for each phase
        	    	// Narrow band of phi1
        	    	if ((phi1[i] < 1.2) && (phi1[i] >= -1.2)) {
        	    		nb1Num++;
        	    	}
        	    	// Narrow band of phi2
        	    	if ((phi2[i] < 1.2) && (phi2[i] >= -1.2)) {
        	    		nb2Num++;
        	    	}
        	        if (phi1[i] >= 0) {
        	        	if (phi2[i] >= 0) {
        	        	    c11Num++;
        	        	}
        	        	else {
        	        	    c12Num++;	
        	        	}
        	        } // if (phi1[i] >= 0)
        	        else {
        	        	if (phi2[i] >= 0) {
        	        		c21Num++;
        	        	}
        	        	else {
        	        		c22Num++;
        	        	}
        	        }
        	    } // for (i = 0; i < sliceSize; i++)
        	    nb1Index = new int[nb1Num];
        	    nb2Index = new int[nb2Num];
        	    cc11Index = new int[c11Num];
        	    cc12Index = new int[c12Num];
        	    cc21Index = new int[c21Num];
        	    cc22Index = new int[c22Num];
        	    nb1 = 0;
        	    nb2 = 0;
        	    i11 = 0;
        	    i12 = 0;
        	    i21 = 0;
        	    i22 = 0;
        	    for (i = 0; i < sliceSize; i++) {
        	    	// Narrow band for each phase
        	    	// Narrow band of phi1
        	    	if ((phi1[i] < 1.2) && (phi1[i] >= -1.2)) {
        	    		nb1Index[nb1++] = i;
        	    	}
        	    	// Narrow band of phi2
        	    	if ((phi2[i] < 1.2) && (phi2[i] >= -1.2)) {
        	    		nb2Index[nb2++] = i;
        	    	}
        	        if (phi1[i] >= 0) {
        	        	if (phi2[i] >= 0) {
        	        	    cc11Index[i11++] = i;
        	        	}
        	        	else {
        	        	    cc12Index[i12++] = i;	
        	        	}
        	        } // if (phi1[i] >= 0)
        	        else {
        	        	if (phi2[i] >= 0) {
        	        	    cc21Index[i21++] = i;
        	        	}
        	        	else {
        	        		cc22Index[i22++] = i;
        	        	}
        	        }
        	    } // for (i = 0; i < sliceSize; i++)
        	    
        	    // Initial image force for each layer
        	    f_image11 = new double[sliceSize];
        	    f_image12 = new double[sliceSize];
        	    f_image21 = new double[sliceSize];
        	    f_image22 = new double[sliceSize];
        	    for (i = 0; i < layer; i++) {
        	    	if (srcImage.isColorImage()) {
        	    		if (i == 0) {
        	        		offset = firstColor;
        	        	}
        	        	else if (i == 1) {
        	        		offset = secondColor;
        	        	}
        	        	else {
        	        		offset = 3;
        	        	}
        	    		try {
        	    		    srcImage.exportRGBDataNoLock(offset, 0, sliceSize, L);
        	    		}
        	    		catch (IOException e) {
        	    			MipavUtil.displayError("IOException " + e + " on srcImage.exportRGBDataNoLock(offset, 0, sliceSize, L)");
        	    			setCompleted(false);
        	    			return;
        	    		}
        	    	} // if (srcImage.isColorImage())
        	    	else {
        	        	try {
        	        	    image.exportData(0, sliceSize, L);
        	        	}
        	        	catch (IOException e) {
        	        		MipavUtil.displayError("IOException " + e + " on image.exportData(0, sliceSize, L)");
        	        		setCompleted(false);
        	        		return;
        	        	}	
        	        }
        	    	c11 = 0.0;
        	    	if (c11Num == 0) {
        	    		c11 = epsilon;
        	    	}
        	    	else {
        	    		for (j = 0; j < c11Num; j++) {
        	    		    c11 += L[cc11Index[j]];	
        	    		}
        	    		c11 = c11/c11Num;
        	    	}
        	    	c12 = 0.0;
        	    	if (c12Num == 0) {
        	    		c12 = epsilon;
        	    	}
        	    	else {
        	    		for (j = 0; j < c12Num; j++) {
        	    			c12 += L[cc12Index[j]];
        	    		}
        	    		c12 = c12/c12Num;
        	    	}
        	    	c21 = 0.0;
        	    	if (c21Num == 0) {
        	    		c21 = epsilon;
        	    	}
        	    	else {
        	    		for (j = 0; j < c21Num; j++) {
        	    			c21 += L[cc21Index[j]];
        	    		}
        	    		c21 = c21/c21Num;
        	    	}
        	    	c22 = 0.0;
        	    	if (c22Num == 0) {
        	    		c22 = epsilon;
        	    	}
        	    	else {
        	    		for (j = 0; j < c22Num; j++) {
        	    			c22 += L[cc22Index[j]];
        	    		}
        	    		c22 = c22/c22Num;
        	    	}
        	    	
        	    	// Force calculation and normalization force on each partition
        	    	Heav1 = Heaviside(phi1);
        	    	Heav2 = Heaviside(phi2);
        	    	// Sum image force on all components (used for vector image)
        	    	for (j = 0; j < sliceSize; j++) {
        	    	    f_image11[j] = (L[j] - c11) * (L[j] - c11) * Heav1[j] * Heav2[j] + f_image11[j];
        	    	    f_image12[j] = (L[j] - c12) * (L[j] - c12) * Heav1[j] * (1.0 - Heav2[j]) + f_image12[j];
        	    	    f_image21[j] = (L[j] - c21) * (L[j] - c21) * (1.0 - Heav1[j]) * Heav2[j] + f_image21[j];
        	    	    f_image22[j] = (L[j] - c22) * (L[j] - c22) * (1.0 - Heav1[j]) * (1.0 - Heav2[j]) + f_image22[j];
        	    	} // for (j = 0; j < sliceSize; j++)
        	    } // for (i = 0; i < layer; i++)
        	    
        	    // Calculate the external force of the image
        	    
        	    // Curvature on phi1
        	    curvature = kappa(phi1, xDim);
        	    for (i = 0; i < sliceSize; i++) {
        	    	curvature[i] = mu * curvature[i];
        	    }
        	    curvature1 = new double[nb1Num];
        	    for (i = 0; i < nb1Num; i++) {
        	    	curvature1[i] = curvature[nb1Index[i]];
        	    }
        	    // Image force on phi1
        	    fim1 = new double[nb1Num];
        	    maxfim1 = -Double.MAX_VALUE;
        	    for (i = 0; i < nb1Num; i++) {
        	    	fim1[i] = (1.0/layer) * (-f_image11[nb1Index[i]] + f_image21[nb1Index[i]] -
        	    			                 f_image12[nb1Index[i]] + f_image22[nb1Index[i]]);
        	    	if (Math.abs(fim1[i]) > maxfim1) {
        	    		maxfim1 = Math.abs(fim1[i]);
        	    	}
        	    }
        	    for (i = 0; i < nb1Num; i++) {
        	    	fim1[i] = fim1[i]/(maxfim1 + epsilon);
        	    }
        	    
        	    // Curvature on phi2
        	    curvature = kappa(phi2, xDim);
        	    for (i = 0; i < sliceSize; i++) {
        	    	curvature[i] = mu * curvature[i];
        	    }
        	    curvature2 = new double[nb2Num];
        	    for (i = 0; i < nb2Num; i++) {
        	    	curvature2[i] = curvature[nb2Index[i]];
        	    }
        	    // Image force on phi2
        	    fim2 = new double[nb2Num];
        	    maxfim2 = -Double.MAX_VALUE;
        	    for (i = 0; i < nb2Num; i++) {
        	    	fim2[i] = (1.0/layer) * (-f_image11[nb2Index[i]] + f_image12[nb2Index[i]] -
        	    			                 f_image21[nb2Index[i]] + f_image22[nb2Index[i]]);
        	    	if (Math.abs(fim2[i]) > maxfim2) {
        	    		maxfim2 = Math.abs(fim2[i]);
        	    	}
        	    }
        	    for (i = 0; i < nb2Num; i++) {
        	    	fim2[i] = fim2[i]/(maxfim2 + epsilon);
        	    }
        	    
        	    // Force on phi1 and phi2
        	    force1 = new double[nb1Num];
        	    for (i = 0; i < nb1Num; i++) {
        	    	force1[i] = curvature1[i] + fim1[i];
        	    }
        	    force2 = new double[nb2Num];
        	    for (i = 0; i < nb2Num; i++) {
        	    	force2[i] = curvature2[i] + fim2[i];
        	    }
        	    
        	    // delta t
        	    dt = 1.5;
        	    
        	    oldm = new double[2][sliceSize];
        	    for (i = 0; i < sliceSize; i++) {
        	    	oldm[0][i] = phi1[i];
        	    	oldm[1][i] = phi2[i];
        	    }
        	    
        	    // Update of phi1 and phi2
        	    for (i = 0; i < nb1Num; i++) {
        	    	phi1[nb1Index[i]] = phi1[nb1Index[i]] + dt * force1[i];
        	    }
        	    for (i = 0; i < nb2Num; i++) {
        	    	phi2[nb2Index[i]] = phi2[nb2Index[i]] + dt * force2[i];
        	    }
        	    
        	    newm = new double[2][sliceSize];
        	    for (i = 0; i < sliceSize; i++) {
        	    	newm[0][i] = phi1[i];
        	    	newm[1][i] = phi2[i];
        	    }
        	    
        	    indicator = checkstop(oldm, newm, dt);
        	    
        	    if (indicator) {
        	        break iloop2;    
        	    } // if (indicator)
        	    
        	    // Re-initializations
        	    reinitialization(phi1, 0.6, xDim);
        	    reinitialization(phi2, 0.6, xDim);
        	} // for (n = 1; n <= numIter; n++)
        	// Make mask from SDF
	    	// Get mask from levelset
	    	for (i = 0; i < sliceSize; i++) {
	    		if (phi1[i] >= 0) {
	    		    if (phi2[i] >= 0) {
	    		    	seg11[i] = 1;
	    		    }
	    		    else {
	    		    	seg12[i] = 1;
	    		    }
	    		} // if (phi1[i] >= 0)
	    		else {
	    			if (phi2[i] >= 0) {
	    				seg21[i] = 1;
	    			}
	    			else {
	    				seg22[i]  = 1;
	    			}
	    		}
	    	} // for (i = 0; i < sliceSize; i++)
	    	erodeImage = new ModelImage(ModelStorageBase.BYTE, extents, "erodeImage");
        	try {
        		erodeImage.importData(0, seg11, true);
        	}
        	catch (IOException e) {
        		MipavUtil.displayError("IOException " + e + " on erodeImage.importData(0, seg11, true)");
        		setCompleted(false);
        		return;
        	}
	    	kernel = AlgorithmMorphology2D.SIZED_CIRCLE;
        	circleDiameter = 3*erodeImage.getFileInfo()[0].getResolutions()[0];
        	morphologyMethod = AlgorithmMorphology2D.ERODE;
        	itersDilation = 0;
        	itersErosion = 1;
        	numPruningPixels = 0;
        	edgingType = 0;
        	erodeAlgo = new AlgorithmMorphology2D(erodeImage, kernel, circleDiameter, morphologyMethod, itersDilation,
                    itersErosion, numPruningPixels, edgingType, wholeImage);
            erodeAlgo.run();
            erodeAlgo.finalize();
            erodeAlgo = null;
            try {
        		erodeImage.exportData(0, sliceSize, seg11);
        	}
        	catch(IOException e) {
        		MipavUtil.displayError("IOException " + e + " on erodeImage.exportData(0, sliceSize, seg11)");
        		setCompleted(false);
        		return;
        	}
            VOIExtractionAlgo = new AlgorithmVOIExtraction(erodeImage);
        	VOIExtractionAlgo.run();
        	VOIExtractionAlgo.finalize();
        	VOIExtractionAlgo = null;
            
            VOIVector kVOIs = erodeImage.getVOIs();
            for (i = 0; i < kVOIs.size(); i++ )
            {
                VOI kCurrentGroup = kVOIs.get(i);
                kCurrentGroup.setAllActive(true);
                kCurrentGroup.setColor(Color.RED);
            }
            erodeImage.groupVOIs();
            kVOIs = erodeImage.getVOIs();
            srcImage.resetVOIs();
            srcImage.addVOIs(kVOIs);
        	try {
        		erodeImage.importData(0, seg12, true);
        	}
        	catch (IOException e) {
        		MipavUtil.displayError("IOException " + e + " on erodeImage.importData(0, seg12, true)");
        		setCompleted(false);
        		return;
        	}
        	erodeAlgo = new AlgorithmMorphology2D(erodeImage, kernel, circleDiameter, morphologyMethod, itersDilation,
                    itersErosion, numPruningPixels, edgingType, wholeImage);
            erodeAlgo.run();
            erodeAlgo.finalize();
            erodeAlgo = null;
            try {
        		erodeImage.exportData(0, sliceSize, seg12);
        	}
        	catch(IOException e) {
        		MipavUtil.displayError("IOException " + e + " on erodeImage.exportData(0, sliceSize, seg12)");
        		setCompleted(false);
        		return;
        	}
            VOIExtractionAlgo = new AlgorithmVOIExtraction(erodeImage);
        	VOIExtractionAlgo.run();
        	VOIExtractionAlgo.finalize();
        	VOIExtractionAlgo = null;
            
            kVOIs = erodeImage.getVOIs();
            for (i = 0; i < kVOIs.size(); i++ )
            {
                VOI kCurrentGroup = kVOIs.get(i);
                kCurrentGroup.setAllActive(true);
                kCurrentGroup.setColor(Color.GREEN);
            }
            erodeImage.groupVOIs();
            kVOIs = erodeImage.getVOIs();
            srcImage.addVOIs(kVOIs);
        	
        	try {
        		erodeImage.importData(0, seg21, true);
        	}
        	catch (IOException e) {
        		MipavUtil.displayError("IOException " + e + " on erodeImage.importData(0, seg21, true)");
        		setCompleted(false);
        		return;
        	}
        	erodeAlgo = new AlgorithmMorphology2D(erodeImage, kernel, circleDiameter, morphologyMethod, itersDilation,
                    itersErosion, numPruningPixels, edgingType, wholeImage);
            erodeAlgo.run();
            erodeAlgo.finalize();
            erodeAlgo = null;
        	try {
        		erodeImage.exportData(0, sliceSize, seg21);
        	}
        	catch(IOException e) {
        		MipavUtil.displayError("IOException " + e + " on erodeImage.exportData(0, sliceSize, seg21)");
        		setCompleted(false);
        		return;
        	}
        	VOIExtractionAlgo = new AlgorithmVOIExtraction(erodeImage);
        	VOIExtractionAlgo.run();
        	VOIExtractionAlgo.finalize();
        	VOIExtractionAlgo = null;
            
            kVOIs = erodeImage.getVOIs();
            for (i = 0; i < kVOIs.size(); i++ )
            {
                VOI kCurrentGroup = kVOIs.get(i);
                kCurrentGroup.setAllActive(true);
                kCurrentGroup.setColor(Color.BLUE);
            }
            erodeImage.groupVOIs();
            kVOIs = erodeImage.getVOIs();
            srcImage.addVOIs(kVOIs);
        	try {
        		erodeImage.importData(0, seg22, true);
        	}
        	catch (IOException e) {
        		MipavUtil.displayError("IOException " + e + " on erodeImage.importData(0, seg22, true)");
        		setCompleted(false);
        		return;
        	}
        	erodeAlgo = new AlgorithmMorphology2D(erodeImage, kernel, circleDiameter, morphologyMethod, itersDilation,
                    itersErosion, numPruningPixels, edgingType, wholeImage);
            erodeAlgo.run();
            erodeAlgo.finalize();
            erodeAlgo = null;
        	try {
        		erodeImage.exportData(0, sliceSize, seg22);
        	}
        	catch(IOException e) {
        		MipavUtil.displayError("IOException " + e + " on erodeImage.exportData(0, sliceSize, seg22)");
        		setCompleted(false);
        		return;
        	}
        	VOIExtractionAlgo = new AlgorithmVOIExtraction(erodeImage);
        	VOIExtractionAlgo.run();
        	VOIExtractionAlgo.finalize();
        	VOIExtractionAlgo = null;
            
            kVOIs = erodeImage.getVOIs();
            for (i = 0; i < kVOIs.size(); i++ )
            {
                VOI kCurrentGroup = kVOIs.get(i);
                kCurrentGroup.setAllActive(true);
                kCurrentGroup.setColor(Color.CYAN);
            }
            erodeImage.groupVOIs();
            kVOIs = erodeImage.getVOIs();
            srcImage.addVOIs(kVOIs);
        	erodeImage.disposeLocal();
        	erodeImage = null;
        	seg = new byte[sliceSize];
        	for (i = 0; i < sliceSize; i++) {
        		seg[i] = (byte)(seg11[i] + 2 * seg12[i] + 3 * seg21[i] + 4 * seg22[i]);
        	}
        	try {
        		destImage.importData(0, seg, true);
        	}
        	catch (IOException e) {
        		MipavUtil.displayError("IOException " + e + " on destImage.importData(0, seg, true)");
        		setCompleted(false);
        		return;
        	}
        	setCompleted(true);
        	return;
        } // else if (method == twophase)
        
	}
	
	// Reinitialize the distance map for active contour
	private void reinitialization(double D[], double dt, int xDim) {
		int sliceSize = D.length;
	    int yDim = sliceSize/xDim;
	    double T[] = new double[(xDim + 2)*(yDim + 2)];
	    int x;
	    int y;
	    double a[] = new double[sliceSize];
	    double b[] = new double[sliceSize];
	    double c[] = new double[sliceSize];
	    double d[] = new double[sliceSize];
	    double ap[];
	    double am[];
	    double bp[];
	    double bm[];
	    double cp[];
	    double cm[];
	    double dp[];
	    double dm[];
	    int index;
	    int i;
	    double G[];
	    double signD[];
	    
	    for (y = 0; y < yDim; y++) {
	    	for (x = 0; x < xDim; x++) {
	    		T[(x+1) + (y+1)*(xDim+2)] = D[x + y*xDim];
	    	}
	    }
	    for (x = 0; x < xDim + 2; x++) {
	    	T[x] = 0.0;
	    	T[x + (yDim+1)*(xDim+2)] = 0.0;
	    }
	    for (y = 0; y < yDim + 2; y++) {
	    	T[y*(xDim+2)] = 0.0;
	    	T[(xDim+1) + y*(xDim+2)] = 0.0;
	    }
	    
	    // Differences on all directions
	    for (y = 0; y < yDim; y++) {
	    	for (x = 0; x < xDim; x++) {
	    		index = x + y * xDim;
	    		a[index] = D[index] - T[x + 1 + y * (xDim + 2)];
	    		b[index] = T[x + 1 + (y + 2) * (xDim + 2)] - D[index];
	    		c[index] = D[index] - T[x + (y + 1) * (xDim+2)];
	    		d[index] = T[x + 2 + (y + 1) * (xDim + 2)] - D[index];
	    	}
	    }
	    T = null;
	    ap = new double[sliceSize];
	    am = new double[sliceSize];
	    bp = new double[sliceSize];
	    bm = new double[sliceSize];
	    cp = new double[sliceSize];
	    cm = new double[sliceSize];
	    dp = new double[sliceSize];
	    dm = new double[sliceSize];
	    
	    for (i = 0; i < sliceSize; i++) {
	        ap[i] = Math.max(a[i], 0.0);
	        am[i] = Math.min(a[i], 0.0);
	        bp[i] = Math.max(b[i], 0.0);
	        bm[i] = Math.min(b[i], 0.0);
	        cp[i] = Math.max(c[i], 0.0);
	        cm[i] = Math.min(c[i], 0.0);
	        dp[i] = Math.max(d[i], 0.0);
	        dm[i] = Math.min(d[i], 0.0);
	    }
	    a = null;
	    b = null;
	    c = null;
	    d = null;
	    G = new double[sliceSize];
	    for (i = 0; i < sliceSize; i++) {
	    	if (D[i] > 0.0) {
	    		G[i] = Math.sqrt(Math.max(ap[i]*ap[i], bm[i]*bm[i]) + Math.max(cp[i]*cp[i],dm[i]*dm[i])) - 1.0;
	    	}
	    	if (D[i] < 0.0) {
	    		G[i] = Math.sqrt(Math.max(am[i]*am[i],bp[i]*bp[i]) + Math.max(cm[i]*cm[i],dp[i]*dp[i])) - 1.0;
	    	}
	    }
	    ap = null;
	    am = null;
	    bp = null;
	    bm = null;
	    cp = null;
	    cm = null;
	    dp = null;
	    dm = null;
	    signD = new double[sliceSize];
	    for (i = 0; i < sliceSize; i++) {
	    	signD[i] = D[i]/Math.sqrt(D[i]*D[i] + 1.0);
	    	D[i] = D[i] - dt * signD[i] * G[i];
	    }
		return;
	}
	
	    // Indicate whether we should perform further iterations or stop
		private boolean checkstop(double[][] old, double[][] neww, double dt) {
			int sliceSize = old[0].length;
			int i;
			boolean indicator = true;
			int M1;
			int M2;
			double Q1 = 0.0;
			double Q2 = 0.0;
	        
		    M1 = 0;
		    M2 = 0;
		    for (i = 0; i < sliceSize; i++) {
		    	if (Math.abs(old[0][i]) < 1.0) {
		    		M1++;
		    		Q1 += Math.abs(neww[0][i] - old[0][i]);
		    	}
		    	if (Math.abs(old[1][i]) < 1.0) {
		    		M2++;
		    		Q2 += Math.abs(neww[1][i] - old[1][i]);
		    	}
		    }
		    Q1 = Q1/M1;
		    Q2 = Q2/M2;
		    if ((Q1 <= dt * 0.18 * 0.18) && (Q2 <= dt * 0.18 * 0.18)) {
		    	indicator = true;
		    }
		    else {
		    	indicator = false;
		    }
			return indicator;
		}
	
	// Indicate whether we should perform further iterations or stop
	private boolean checkstop(double[] old, double[] neww, double dt) {
		int M = 0;
		int sliceSize = old.length;
		int i;
		double Q = 0.0;
		boolean indicator = true;
        for (i = 0; i < sliceSize; i++) {
        	if (Math.abs(neww[i]) <= 0.5) {
        		M++;
        		Q += Math.abs(neww[i] - old[i]);
        	}
        }
        Q = Q/M;
        if (Q <= dt * 0.18 * 0.18) {
        	indicator = true;
        }
        else {
        	indicator = false;
        }
	    return indicator;
	}
	
	// Heaviside step function (smoothed version)
	private double[] Heaviside(double z[]) {
	    int length = z.length;
	    double H[] = new double[length];
	    int i;
	    for (i = 0; i < length; i++) {
	    	if (z[i] >= 1.0E-5) {
            	H[i] = 1.0;
            }
            else if ((z[i] < 1.0E-5) && (z[i] > -1.0E-5)) {
            	H[i] = 0.5*(1 + z[i]/1.0E-5+ 1.0/Math.PI*Math.sin(Math.PI*z[i]/1.0E-5));
            }
	    }
	    return H;
	}
	
	// Get curvature information of input image
	private double[] kappa(double I[], int xDim) {
	    int sliceSize = I.length;
	    int yDim = sliceSize/xDim;
	    double P[] = new double[(xDim + 2)*(yDim + 2)];
	    double fy[] = new double[sliceSize];
	    double fx[] = new double[sliceSize];
	    double fyy[] = new double[sliceSize];
	    double fxx[] = new double[sliceSize];
	    double fxy[] = new double[sliceSize];
	    double G[] = new double[sliceSize];
	    double K[] = new double[sliceSize];
	    double KG[] = new double[sliceSize];
	    int x;
	    int y;
	    int i;
	    double maxKG;
	    for (y = 0; y < yDim; y++) {
	    	for (x = 0; x < xDim; x++) {
	    		P[(x+1) + (y+1)*(xDim+2)] = I[x + y*xDim];
	    	}
	    }
	    for (x = 0; x < xDim + 2; x++) {
	    	P[x] = 1.0;
	    	P[x + (yDim+1)*(xDim+2)] = 1.0;
	    }
	    for (y = 0; y < yDim + 2; y++) {
	    	P[y*(xDim+2)] = 1.0;
	    	P[(xDim+1) + y*(xDim+2)] = 1.0;
	    }
	    
	    // Central difference
	    for (y = 0; y < yDim; y++) {
	    	for (x = 0; x < xDim; x++) {
	    	    fy[x + y * xDim] = P[x + 1 + (y + 2) * (xDim + 2)] - P[x + 1 + y * (xDim + 2)];
	    	    fx[x + y * xDim] = P[x + 2 + (y + 1) * (xDim + 2)] - P[x + (y + 1) * (xDim + 2)];
	    	    fyy[x + y * xDim] = P[x + 1 + (y + 2) * (xDim + 2)] + P[x + 1 + y * (xDim + 2)] - 2.0 * I[x + y * xDim];
	    	    fxx[x + y * xDim] = P[x + 2 + (y + 1) * (xDim + 2)] + P[x + (y + 1) * (xDim + 2)] - 2.0 * I[x + y * xDim];
	    	    fxy[x + y * xDim] = 0.25 * (P[x + 2 + (y + 2) * (xDim + 2)] - P[x + 2 + y * (xDim + 2)] +
	    	    		                    P[x + (y + 2) * (xDim + 2)] - P[x + y * (xDim + 2)]);
	    	} // for (x = 0; x < xDim; x++)
	    } // for (y = 0; y < yDim; y++)
	    
	    for (i = 0; i < sliceSize; i++) {
	    	G[i] = Math.sqrt(fx[i]*fx[i] + fy[i]*fy[i]);
	    	K[i] = (fxx[i]*fy[i]*fy[i] - 2.0*fxy[i]*fx[i]*fy[i] + fyy[i]*fx[i]*fx[i])/(Math.pow(fx[i]*fx[i] + fy[i]*fy[i] + epsilon, 1.5));
	    	KG[i] = K[i] * G[i];
	    }
	    
	    for (x = 0; x < xDim; x++) {
    		KG[x] = epsilon;
    		KG[x + (yDim-1)*xDim] = epsilon; 
    	}
    	
    	for (y = 0; y < yDim; y++) {
    		KG[y*xDim] = epsilon;
    		KG[xDim-1 + y*xDim] = epsilon;
    	}
    	
    	maxKG = 0.0;
    	for (i = 0; i < sliceSize; i++) {
    		if (Math.abs(KG[i]) > maxKG) {
    			maxKG = Math.abs(KG[i]);
    		}
    	}
    	
    	for (i = 0; i < sliceSize; i++) {
    		KG[i] = KG[i]/maxKG;
    	}
	    
	    return KG;
	}
}