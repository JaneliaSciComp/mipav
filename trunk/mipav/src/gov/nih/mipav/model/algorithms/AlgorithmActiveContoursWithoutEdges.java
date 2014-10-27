package gov.nih.mipav.model.algorithms;


import gov.nih.mipav.model.algorithms.utilities.AlgorithmRGBtoGray;
import gov.nih.mipav.model.structures.*;
import gov.nih.mipav.view.*;

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
	
	// epsilon = D1MACH(4)
    // Machine epsilon is the smallest positive epsilon such that
    // (1.0 + epsilon) != 1.0.
    // epsilon = 2**(1 - doubleDigits) = 2**(1 - 53) = 2**(-52)
    // epsilon = 2.2204460e-16
    // epsilon is called the largest relative spacing
    private double epsilon = Math.pow(2, -52);
	
	// The step space
	//private double h = 1.0;
	
	// The time step
	// A value of delt = 0.01 was used in 1 example in reference 1.
	//private double delt = 0.1;
	
	//private double lambda1 = 1.0;
	
	//private double lambda2 = 1.0;
	
	// Area parameter was set to 0.02 * 255 * 255 in 1 example in reference 1
	//private double nu = 0.0;
	
	// One example in reference 1 had 1 iteration of reinitialization and 4
	// examples had 5 iterations of reinitialization.
	//private int reinitializations = 0;

	
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
        int xDim;
        int yDim;
        int sliceSize;
        double buffer[];
		int x;
		int y;
		int index;
		double c1;
		double c2;
		//AlgorithmTransform transform;
		//TransMatrix xfrm;
		int oXdim;
		int oYdim;
		//float oXres;
		//float oYres;
		//boolean transformVOI = true;
		//boolean clip = true;
		//boolean pad = false;
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
		byte mtemp[] = null;
		byte[][] m = null;
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
		boolean wholeImage = true;
		byte mcrop[];
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
	    ModelImage maskImage;
	    AlgorithmVOIExtraction VOIExtractionAlgo;
        
        fireProgressStateChanged(srcImage.getImageName(), "Evolving the level set ...");
        
        if ((method == multiphase) && ((mask == small) || (mask == medium) || (mask == large))) {
        	MipavUtil.displayError("multiphase requires 2 masks but only gets one");
        	setCompleted(false);
        	return;
        }
        
        // Initializations on input image and VOIs
        // Resize original image
        xDim = srcImage.getExtents()[0];
        yDim = srcImage.getExtents()[1];
        // Resize scale
        /*s = 200.0/Math.min(xDim,yDim);
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
        }*/
        image = srcImage;
        oXdim = xDim;
        oYdim = yDim;
        sliceSize = oXdim * oYdim;
        extents = new int[2];
        extents[0] = oXdim;
        extents[1] = oYdim;
        
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
            if (method == chan) {
	            maxR = (float)image.getMaxR();
	            maxG = (float)image.getMaxG();
	            maxB = (float)image.getMaxB();
	            gAlgo = new AlgorithmRGBtoGray(image, redValue, greenValue, blueValue, thresholdAverage, threshold, intensityAverage,
	            		equalRange, minR, maxR, minG, maxG, minB, maxB);
	            gAlgo.run();
	            gAlgo.finalize();
	            layer = 1;
            }
        } // if (image.isColorImage())
		
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
	
	        for (i = 0; i < nVOI; i++) {
	        	if (i == n1) {
	        		VOIs.VOIAt(i).setActive(true);
	        	}
	        	else {
	        		VOIs.VOIAt(i).setActive(false);
	        	}
	        }
	        
	        IDOffset = 1;
	        XOR = false;
	        onlyActive = true;
	        uByteImage = image.generateUnsignedByteImage(IDOffset, XOR, onlyActive);
	        mtemp = new byte[sliceSize];
	        m = new byte[sliceSize][numFound];
	        try {
	        	uByteImage.exportData(0, sliceSize, mtemp);
	        }
	        catch (IOException e) {
	        	MipavUtil.displayError("IOException " + e + "on uByteImage.exportData(0, sliceSize, mtemp");
	        	setCompleted(false);
	        	return;
	        }
	        uByteImage.disposeLocal();
	        uByteImage = null;
	        for (i = 0; i < sliceSize; i++) {
	        	m[i][0] = mtemp[i];
	        }
	        if (method == multiphase) {
	        	for (i = 0; i < nVOI; i++) {
		        	if (i == n2) {
		        		VOIs.VOIAt(i).setActive(true);
		        	}
		        	else {
		        		VOIs.VOIAt(i).setActive(false);
		        	}
		        }
	        	IDOffset = 1;
		        XOR = false;
		        onlyActive = true;
		        uByteImage = image.generateUnsignedByteImage(IDOffset, XOR, onlyActive);
		        try {
		        	uByteImage.exportData(0, sliceSize, mtemp);
		        }
		        catch (IOException e) {
		        	MipavUtil.displayError("IOException " + e + "on uByteImage.exportData(0, sliceSize, mtemp");
		        	setCompleted(false);
		        	return;
		        }
		        uByteImage.disposeLocal();
		        uByteImage = null;
		        for (i = 0; i < sliceSize; i++) {
	        		m[i][1] = mtemp[i];
	        	}
	        } // if (method == multiphase)
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
            if ((mask == whole) || (mask == whole_small)) {
            	m = new byte[sliceSize][2];
            }
            else {
            	m = new byte[sliceSize][1];
            }
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
            	r2 = r*r; 
                for (y = 0; y < oYdim; y++) {
                	diffy = y - cy;
                	dy2 = diffy * diffy;
                	for (x = 0; x < oXdim; x++) {
                		diffx = x - cx;
                		index = x + y * oXdim;
                		if (diffx*diffx + dy2 < r2) {
                			if (mask == whole_small) {
                			    m[index][1] = 1;
                			}
                			else {
                				m[index][0] = 1;
                			}
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
            	for (y = 0; y < oYdim; y++) {
            		for (x = 0; x < oXdim; x++) {
            			m[x + y * oXdim][0] = mcrop[x + y * oXdim];
            		}
            	}
            	if (mask == whole) {
	            	padSize = (int)Math.floor(2.0*r/3.0);
	            	M = new byte[(oXdim + padSize) * (oYdim + padSize)];
	            	for (y = 0; y < oYdim ; y++) {
	            	    for (x = 0; x < oXdim; x++) {
	            	    	M[x + y * (oXdim + padSize)] = mcrop[x + y * oXdim];
	            	    }
	            	}
	            	for (y = 0; y < oYdim; y++) {
	            		for (x = 0; x < oXdim; x++) {
	            			m[x + y * oXdim][1] = M[x + padSize + (y + padSize) * (oXdim + padSize)];
	            		}
	            	}
            	} // if (mask == whole)
            } // if ((mask == whole) || (mask == whole_small))
        } // if ((mask >= 1) && (mask <= 5))
        
        // Core function
        if ((method == chan) || (method == vector)) {
        	seg = new byte[sliceSize];
            // SDF
        	// Get the distance map of the initial mask
        	for (i = 0; i < sliceSize; i++) {
        		mtemp[i] = m[i][0];
        	}
        	distImage = new ModelImage(ModelStorageBase.UBYTE, extents, "distImage");
        	// AlgorithmMorphology2D requires BOOLEAN, BYTE, UBYTE, SHORT, or USHORT for entry 
        	// Reallocated to float in AlgorithmMorphology2D distanceMapForShapeInterpolation
        	try {
        		distImage.importData(0, mtemp, true);
        	}
        	catch (IOException e) {
        		MipavUtil.displayError("IOexception " + e + " on distance.importData(0, mtemp, true");
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
        		distImage.importData(0, mtemp, true);
        	}
        	catch (IOException e) {
        		MipavUtil.displayError("IOexception " + e + " on distance.importData(0, mtemp, true");
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
        		phi0[i] = FGDist[i] + BGDist[i] + mtemp[i] - 0.5;
        	}
        	// Initial force, set to epsilon to avoid division by zeros
        	//force = epsilon;
        	// End intialization
        	
        	// Main loop
        	inidx = 0;
        	outidx = 0;
        	iloop: for (n = 1; n <= numIter; n++) {
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
        	    // Initial image force for each layeer
        	    forceImage = new double[sliceSize];
        	    L = new double[sliceSize];
        	    for (i = 1; i <= layer; i++) {
        	        if (image.isColorImage()) {
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
        	                image.exportRGBDataNoLock(offset, 0, sliceSize, L);	
        	        	}
        	        	catch (IOException e) {
        	        		MipavUtil.displayError("IOException " + e + " on image.exportRGBDataNoLock(offset, 0, sliceSize, L)");
        	        		setCompleted(false);
        	        		return;
        	        	}
        	        } // if (image.isColorImage())
        	        else {
        	        	try {
        	        	    image.exportData(0, sliceSize, L);
        	        	}
        	        	catch (IOException e) {
        	        		MipavUtil.displayError("IOException " + e + " on image.exportData(0, sliceSize, L)");
        	        		setCompleted(false);
        	        		return;
        	        	}
        	        } // else
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
        	    force = new double[sliceSize];
        	    maxKap = -Double.MAX_VALUE;
        	    kap = kappa(phi0, oXdim);
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
        	    old = new double[sliceSize];
        	    neww = new double[sliceSize];
        	    for (i = 0; i < sliceSize; i++) {
        	    	old[i] = phi0[i];
        	    	phi0[i] = phi0[i] + dt * force[i];
        	    	neww[i] = phi0[i];
        	    } 
        	    indicator = checkstop(old, neww, dt);
        	    if (indicator) {
        	        break iloop;    
        	    } // if (indicator)
        	} // for (n = 1; n <= numIter; n++)
        	// Get mask from levelset
        	for (i = 0; i < sliceSize; i++) {
	        	if (phi0[i] <= 0.0) {
	        		seg[i] = 1;
	        	}
	        } // for (i = 0; i < sliceSize; i++) 
        	maskImage = new ModelImage(ModelStorageBase.BYTE, extents, "SegmentationImage");
        	try {
        		maskImage.importData(0, seg, true);
        	}
        	catch (IOException e) {
        		MipavUtil.displayError("IOExcepion " + e + " on maskImage.importData(0, seg, true)");
        		setCompleted(false);
        		return;
        	}
        	VOIExtractionAlgo = new AlgorithmVOIExtraction(maskImage);
        	VOIExtractionAlgo.run();
            
            VOIVector kVOIs = maskImage.getVOIs();
            for (i = 0; i < kVOIs.size(); i++ )
            {
                VOI kCurrentGroup = kVOIs.get(i);
                kCurrentGroup.setAllActive(true);
            }
            maskImage.groupVOIs();
            kVOIs = maskImage.getVOIs();
            //image.setVOIs(kVOIs);
            image.addVOIs(kVOIs);
            maskImage.disposeLocal();
            maskImage = null;
            setCompleted(true);
            return;
        } // if ((method == chan) || (method == vector))
        
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