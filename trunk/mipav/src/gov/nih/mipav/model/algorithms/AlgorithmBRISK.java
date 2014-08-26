package gov.nih.mipav.model.algorithms;


import gov.nih.mipav.model.structures.*;
import gov.nih.mipav.view.*;
import gov.nih.mipav.view.dialogs.*;

import java.io.*;
import java.util.Iterator;
import java.util.Vector;

import javax.vecmath.Point2d;

/**
BRISK - Binary Robust Invariant Scalable Keypoints
Reference implementation of
[1] Stefan Leutenegger,Margarita Chli and Roland Siegwart, BRISK:
	Binary Robust Invariant Scalable Keypoints, in Proceedings of
	the IEEE International Conference on Computer Vision (ICCV2011).

Copyright (C) 2011  The Autonomous Systems Lab (ASL), ETH Zurich,
Stefan Leutenegger, Simon Lynen and Margarita Chli.

This file is part of BRISK.

Redistribution and use in source and binary forms, with or without
modification, are permitted provided that the following conditions are met:
   * Redistributions of source code must retain the above copyright
     notice, this list of conditions and the following disclaimer.
   * Redistributions in binary form must reproduce the above copyright
     notice, this list of conditions and the following disclaimer in the
     documentation and/or other materials provided with the distribution.
   * Neither the name of the ASL nor the names of its contributors may be 
     used to endorse or promote products derived from this software without 
     specific prior written permission.

THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS "AS IS" AND
ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE IMPLIED
WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE ARE
DISCLAIMED. IN NO EVENT SHALL THE COPYRIGHT HOLDERS BE LIABLE FOR ANY
DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES
(INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES;
LOSS OF USE, DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER CAUSED AND
ON ANY THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT
(INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE OF THIS
SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.
*/

public class AlgorithmBRISK extends AlgorithmBase {
	
	// List of images to be matched
	private ModelImage destImage[] = null;
	
	// FAST/AGAST detection threshold.  The default value is 60.
	private double threshold = 60.0;
	
	// Number of octaves for the detection. The default value is 4
	private int octaves = 4;
	
	private boolean rotationInvariant = true;
	
	private boolean scaleInvariant = true;
	
	// Scale factor for the BRISK pattern.  The default value is 1.0.
	private double patternScale = 1.0;
	
	private static final int TYPE_STANDARD = 1;
	
	private static final int TYPE_S = 2;
	
	private static final int TYPE_U = 3;
	
	private static final int TYPE_SU = 4;
	
	// BRISK special types are 'S', 'U', and 'SU'.  By default, the standard BRISK is used.
	private int type = TYPE_STANDARD;
	
	// First intialize BRISK.  This will create the pattern look-up table so this
	// may take some fraction of a second.  Do not rerun!
	// After initialization stages are:
	// 1.) Setting up the detector and detecting the keypoints.  Optionally get the points back.
	// 2.) Descriptor extraction.  Construct the extractor.  Make sure to do this only once:
	//     this will build up the look-up tables, which is consuming a considerable amount of time.
	// Constructor variants for arbitrary costumization available.
	// Get the descriptors and arbitrary keypoints.
	// 3.) Matching.  Construct the matcher and process an arbitrary number of images.
	//     Use radiusMatch, or alternatively use knnMatch (or match for k == 1)
	
	// Detect the keypoints.  Optionally get the keypoints back.
	private boolean detect = true;
	
	// Get the descriptors and the corresponding keypoints.
	private boolean describe = true;
	
	private static final int NO_MATCH = 1;
	
	private static final int radiusMatch = 2;
	
	private static final int knnMatch = 3;
	
	private int match = radiusMatch;
	
	private Vector<Double>radiusList = null;
	
	private Vector<Integer>numberList = null;
	
	// Short pair maximum distance
	private double dMax = 5.85;
	
	// Long pair maximum distance
	private double dMin = 8.2;
	
	private Vector<Integer>indexChange = new Vector<Integer>();
	
	private static final double basicSize = 12.0;
	
	// Scales discretization
	private static final int scales = 64;
	
	// 40->4 Octaves - else, this needs to be adjusted
	private static final double scaleRange = 30.0;
	
    // Discretization of the rotation look-up pairs
    private final int n_rot = 1204;
    
    private final double safetyFactor = 1.0;
    
    // Total number of collocation  points
    private int points;
    
    // Pattern properties
    private BriskPatternPoint patternPoints[]; // [i][rotation][scale]
    
    // Lists the scaling per scale index [scale]
    private double scaleList[];
    
    // Lists the total pattern size per scale index [scale]
    private int sizeList[];
    
    // d < dMax
    private BriskShortPair shortPairs[];
    
    // d > dMin
    private  BriskLongPair longPairs[];
    
    // Number of shortPairs
    private int numShortPairs;
    
    // Number of longPairs
	private int numLongPairs;
	
	// Number of uchars the descriptor consists of
	private int strings;
	
	//~ Constructors ---------------------------------------------------------------------------------------------------

    /**
     * AlgorithmBRISK - default constructor.
     */
    public AlgorithmBRISK() { }
    
    /**
     * @param destImage List of images to be matched
     * @param srcImg Source image
     * @param threshold FAST/AGAST detection threshold
     * @param octaves Number of octaves for the detection
     * @param rotationInvariant
     * @param scaleInvariant
     * @param patternScale Scale factor for the BRISK pattern.
     * @param type
     * @param detect Detect the keypoints.  Optionally get the keypoints back.
     * @param describe  Get the descriptors and the corresponding keypoints.
     * @param match NO_MATCH, radiusMatch, or knnMatch
     * @param radiusList
     * @param numberList
     * @param dMax Short pair maximum distance
     * @param dMin Long pair maximum distance
     * @param indexChange
     */
    public AlgorithmBRISK(ModelImage destImage[], ModelImage srcImg, double threshold, int octaves,
    		boolean rotationInvariant, boolean scaleInvariant,
    		double patternScale, int type, boolean detect, boolean describe, int match,
    		Vector<Double>radiusList, Vector<Integer>numberList, double dMax, double dMin, 
    		Vector<Integer>indexChange) {
    	super(null, srcImg);
    	this.destImage = destImage;
    	this.threshold = threshold;
    	this.octaves = octaves;
    	this.rotationInvariant = rotationInvariant;
    	this.scaleInvariant = scaleInvariant;
    	this.patternScale = patternScale;
    	this.type = type;
    	this.detect = detect;
    	this.describe = describe;
    	this.match = match;
    	this.radiusList = radiusList;
    	this.numberList = numberList;
    	this.dMax = dMax;
    	this.dMin = dMin;
    	this.indexChange = indexChange;
    }
    
    /**
     * Starts the program.
     */
    public void runAlgorithm() {
    	int xDim;
    	int yDim;
    	int sliceSize;
    	
    	if (srcImage == null) {
            displayError("Source Image is null");
            finalize();

            return;
        }
    	

        fireProgressStateChanged(srcImage.getImageName(), "BRISK ...");

        xDim = srcImage.getExtents()[0];
        yDim = srcImage.getExtents()[1];
        sliceSize = xDim * yDim;
        
        if (describe) {
        	if ((radiusList != null)  && (radiusList.size() != 0) && (numberList != null) &&
        		(radiusList.size() == numberList.size())) {
        	    generateKernel();	
        	}
        	BriskDescriptorExtractor();
        }
    }
    
    // Create a descriptor with a standard pattern
    private void BriskDescriptorExtractor() {
        radiusList = new Vector<Double>();
        numberList = new Vector<Integer>();
        double f = 0.85 * patternScale;
        
        // This is the standard pattern found to be suitable
        radiusList.add(0, Double.valueOf(0.0));
        radiusList.add(1, Double.valueOf(f * 2.9));
        radiusList.add(2, Double.valueOf(f * 4.9));
        radiusList.add(3, Double.valueOf(f * 7.4));
        radiusList.add(4, Double.valueOf(f * 10.8));
        
        numberList.add(0, Integer.valueOf(1));
        numberList.add(1, Integer.valueOf(10));
        numberList.add(2, Integer.valueOf(14));
        numberList.add(3, Integer.valueOf(15));
        numberList.add(4, Integer.valueOf(20));
        dMax = 5.85 * patternScale;
        dMin = 8.2 * patternScale;
        indexChange = null;
    }
    
    // Call this to generate the kernel:
    // Circle of radius r (pixels), with n points;
    // short pairings with dMax, long pairings with dMin
    private void generateKernel() {
        // Get the total number of points
    	final int rings = radiusList.size();
    	// Remember the total number of points
    	points = 0;
    	for (int ring = 0; ring < rings; ring++) {
    		points += numberList.get(ring);
    	}
    	// Set up the patterns
    	patternPoints = new BriskPatternPoint[points * scales * n_rot];
    	
        int patternIterator = 0;
    	
    	// Define the scale discretization
    	final double lb_scale = Math.log(scaleRange)/Math.log(2.0);
    	final double lb_scale_step = lb_scale/scales;
    	
    	scaleList = new double[scales];
    	sizeList = new int[scales];
    	
    	final double sigma_scale = 1.3;
    	
    	for (int scale = 0; scale < scales; ++scale) {
    		scaleList[scale] = Math.pow(2.0, scale * lb_scale_step);
    		sizeList[scale] = 0;
    		
    		// Generate the pattern points look-up
    		double alpha, theta;
    		for (int rot = 0; rot <n_rot; ++rot) {
    			// This is the rotation of the feature
    		    theta = (double)rot * 2.0 * Math.PI/(double)n_rot;
    		    for (int ring = 0; ring < rings; ++ring) {
    		        for (int num = 0; num < numberList.get(ring); ++num) {
    		            alpha = ((double)num) * 2.0 * Math.PI/(double)numberList.get(ring);
    		            patternPoints[patternIterator] = new BriskPatternPoint();
    		            // Feature rotation plus angle of the point
    		            patternPoints[patternIterator].setX(scaleList[scale] * radiusList.get(ring) * Math.cos(alpha + theta));
    		            patternPoints[patternIterator].setY(scaleList[scale] * radiusList.get(ring) * Math.sin(alpha + theta));
    		            // and the gaussian kernel sigma
    		            if (ring == 0) {
    		            	patternPoints[patternIterator].setSigma(sigma_scale * scaleList[scale] * 0.5);
    		            }
    		            else {
    		            	patternPoints[patternIterator].setSigma(sigma_scale * scaleList[scale] * 
    		            			((double)radiusList.get(ring))* Math.sin(Math.PI/numberList.get(ring)));
    		            }
    		            // Adapt the sizeList if necessary
    		            final int size = (int)Math.ceil(((scaleList[scale] * radiusList.get(ring)) + 
    		            		patternPoints[patternIterator].getSigma())) +1;
    		            if (sizeList[scale] < size) {
    		            	sizeList[scale] = size;
    		            }
    		            
    		            // Increment the iterator
    		            ++patternIterator;
    		        } // for (int num = 0; num < numberList.get(ring); ++num)
    		    } // for (int ring = 0; ring < rings; ++ring)
    		} // for (int rot = 0; rot <n_rot; ++rot)
    	} // for (int scale = 0; scale < scales; ++scale)
    	
    	// Now also generate the pairings
    	shortPairs = new BriskShortPair[points * (points - 1)/2];
    	longPairs = new BriskLongPair[points * (points - 1)/2];
    	numShortPairs = 0;
    	numLongPairs = 0;
    	
    	// Fill indexChange with 0..n if empty
    	if (indexChange == null) {
    		indexChange = new Vector<Integer>();
    	}
    	int indSize = indexChange.size();
    	if (indSize == 0) {
    		indexChange.setSize(points * (points-1)/2);
    		indSize = indexChange.size();
    	}
    	for (int i = 0; i < indSize; i++) {
    		indexChange.add(i, Integer.valueOf(i));
    	}
    	final double dMin_sq = dMin * dMin;
    	final double dMax_sq = dMax * dMax;
    	for (int i = 1; i < points; i++) {
    		// Find all the pairs
    	    for (int j = 0; j < i; j++) {
    	        // Point pair distance
    	    	final double dx = patternPoints[j].getX() - patternPoints[i].getX();
    	    	final double dy = patternPoints[j].getY() - patternPoints[i].getY();
    	    	final double norm_sq = (dx*dx + dy*dy);
    	    	if (norm_sq > dMin_sq) {
    	    	    // Save to long pairs
    	    		longPairs[numLongPairs] = new BriskLongPair();
    	    		longPairs[numLongPairs].setWeighted_dx((int)((dx/norm_sq)*2048.0 + 0.5));
    	    		longPairs[numLongPairs].setWeighted_dy((int)((dy/norm_sq)*2048.0 + 0.5));
    	    		longPairs[numLongPairs].setI(i);
    	    		longPairs[numLongPairs].setJ(j);
    	    		++numLongPairs;
    	    	} // if (norm_sq > dMin_sq)
    	    	else if (norm_sq < dMax_sq) {
    	    	    // Save to short pairs
    	    		// Make sure the user passes something sensible
    	    		if (numShortPairs >= indSize) {
    	    			MipavUtil.displayError("numShortPairs = " + numShortPairs + " >= " + " indSize = " + indSize);
    	    			setCompleted(false);
    	    			return;
    	    		}
    	    		shortPairs[indexChange.get(numShortPairs)] = new BriskShortPair();
    	    		shortPairs[indexChange.get(numShortPairs)].setJ(j);
    	    		shortPairs[indexChange.get(numShortPairs)].setI(i);
    	    		++numShortPairs;
    	    	} // else if (norm_sq < dMax_sq)
    	    } // for (int j = 0; j < i; j++)
    	} // for (int i = 1; i < points; i++)
    	
    	// no bits:
    	strings =  (int)Math.ceil(((float)numShortPairs)/128.0)*4 *4;
    } // private void (generateKernel)
    
    private int smoothedIntensity(ModelImage image, ModelImage integral, final double key_x, final double key_y,
    		final int scale, final int rot, final int point) {
    	// Get the float position
    	final BriskPatternPoint briskPoint = patternPoints[scale * n_rot * points + rot * points + point];
    	final double xf = briskPoint.getX() + key_x;
    	final double yf = briskPoint.getY() + key_y;
    	final int x = (int)xf;
    	final int y = (int)yf;
    	int xDim = image.getExtents()[0];
    	int yDim = image.getExtents()[1];
    	int sliceSize = xDim * yDim;
    	double doubleBuffer[] = new double[sliceSize];
    	try {
    		image.exportData(0, sliceSize, doubleBuffer);
    	}
    	catch (IOException e) {
    		MipavUtil.displayError("IOException " + e + " on image.exportData(0, sliceSize, doubleBuffer) in smoothedIntensity");
    		setCompleted(false);
    		return -1;
    	}
    	
    	// integralXDim = xDim + 1
    	int integralXDim = integral.getExtents()[0];
    	int integralYDim = integral.getExtents()[1];
    	int  integralSlice = integralXDim * integralYDim;
    	double integralBuffer[] = new double[integralSlice];
    	try {
    		integral.exportData(0, integralSlice, integralBuffer);
    	}
    	catch (IOException e) {
    		MipavUtil.displayError("IOException " + e + " on integral.exportData(0, integralSlice, integralBuffer) in smoothedIntensity");
    		setCompleted(false);
    		return -1;
    	}
    	
    	// Get the sigma
    	final double sigma_half = briskPoint.getSigma();
    	final double area = 4.0 * sigma_half * sigma_half;
    	
    	// Calculate output
    	int ret_val;
    	if (sigma_half < 0.5) {
    		// Interpolation multipliers
    		final int r_x = (int)((xf -x)*1024);
    		final int r_y = (int)((yf - y)*1024);
    		final int r_x_1 = (1024 - r_x);
    		final int r_y_1 = (1024 - r_y);
    		int ptr = x + y*xDim;
    		// Just interpolate
    		ret_val = (r_x_1*r_y_1*(int)doubleBuffer[ptr]);
    		ptr++;
    		ret_val += (r_x*r_y_1*(int)doubleBuffer[ptr]);
    		ptr += xDim;
    		ret_val += (r_x*r_y*(int)doubleBuffer[ptr]);
    		ptr--;
    		ret_val += (r_x_1*r_y*(int)doubleBuffer[ptr]);
    		return (ret_val + 512)/1024;
    	} // if (sigma_half < 0.5)
    	
    	// This is the standard case (simple, not speed optimized yet):
    	
    	// Scaling
    	final int scaling = (int)(4194304.0/area);
    	final int scaling2 = (int)(((double)scaling) * area/1024.0);
    	
    	// Calculate borders
    	final double x_1 = xf - sigma_half;
    	final double x1 = xf + sigma_half;
    	final double y_1 = yf - sigma_half;
    	final double y1 = yf + sigma_half;
    	
    	final int x_left = (int)(x_1 + 0.5);
    	final int y_top = (int)(y_1 + 0.5);
    	final int x_right = (int)(x1 + 0.5);
    	final int y_bottom = (int)(y1 + 0.5);
    	
    	// Overlap area - multiplication factors:
    	final double r_x_1 = (double) x_left - x_1 + 0.5;
    	final double r_y_1 = (double) y_top - y_1 + 0.5;
    	final double r_x1 = x1 - (double)x_right + 0.5;
    	final double r_y1 = y1 - (double)y_bottom + 0.5;
    	final int dx = x_right - x_left - 1;
    	final int dy = y_bottom - y_top - 1;
    	final int A = (int)((r_x_1*r_y_1)*scaling);
    	final int B = (int)((r_x1 * r_y_1)*scaling);
    	final int C = (int)((r_x1*r_y1)*scaling);
    	final int D = (int)((r_x_1*r_y1)*scaling);
    	final int r_x_1_i = (int)(r_x_1*scaling);
    	final int r_y_1_i = (int)(r_y_1*scaling);
    	final int r_x1_i = (int)(r_x1*scaling);
    	final int r_y1_i = (int)(r_y1*scaling);
    	
    	if (dx + dy > 2) {
    		// Now the calculation
    		int ptr = x_left + xDim * y_top;
    		// First the corners
    		ret_val = A*(int)doubleBuffer[ptr];
    		ptr += dx + 1;
    		ret_val += B * (int)doubleBuffer[ptr];
    		ptr += dy*xDim + 1;
    		ret_val += C*(int)doubleBuffer[ptr];
    		ptr -= dx+1;
    		ret_val += D*(int)doubleBuffer[ptr];
    		
    		// Next the edges
    		int ptr_integral = x_left + integralXDim*y_top + 1;
    		// Find a simple path through the different surface corners
    		final int tmp1 = ptr_integral;
    		ptr_integral += dx;
    		final int tmp2 = ptr_integral;
    		ptr_integral += integralXDim;
    		final int tmp3 = ptr_integral;
    		ptr_integral++;
    		final int tmp4 = ptr_integral;
    		ptr_integral += dy*integralXDim;
    		final int tmp5 = ptr_integral;
    		ptr_integral--;
    		final int tmp6 = ptr_integral;
    		ptr_integral += integralXDim;
    		final int tmp7 = ptr_integral;
    		ptr_integral -= dx;
    		final int tmp8 = ptr_integral;
    		ptr_integral -= integralXDim;
    		final int tmp9 = ptr_integral;
    		ptr_integral--;
    		final int tmp10 = ptr_integral;
    		ptr_integral -= dy*integralXDim;
    		final int tmp11 = ptr_integral;
    		ptr_integral++;
    		final int tmp12 = ptr_integral;
    		
    		// Assign the wieghted surface integrals
    		final int upper = (tmp3 - tmp2 + tmp1 - tmp12) * r_y_1_i;
    		final int middle = (tmp6 - tmp3 + tmp12 - tmp9) * scaling;
    		final int left = (tmp9 - tmp12 + tmp11 - tmp10) * r_x_1_i;
    		final int right = (tmp5 - tmp4 + tmp3 - tmp6) * r_x1_i;
    		final int bottom = (tmp7 - tmp6 + tmp9 - tmp8) * r_y1_i;
    		return (ret_val+upper+middle+left+right+bottom+scaling2/2)/scaling2;
    	} // if (dx + dy > 2)
    	
    	// Now the calculation
    	int ptr = x_left + xDim * y_top;
    	// First row
    	ret_val = A * (int)doubleBuffer[ptr];
    	ptr++;
    	final int end1 = ptr + dx;
    	for (; ptr < end1; ptr++) {
    		ret_val += r_y_1_i * (int)doubleBuffer[ptr];
    	}
    	ret_val += B * (int)doubleBuffer[ptr];
    	// Middle ones
    	ptr += xDim - dx - 1;
    	int end_j = ptr + dy * xDim;
    	for (; ptr < end_j; ptr += xDim-dx-1) {
    		ret_val += r_x_1_i * (int)doubleBuffer[ptr];
    		ptr++;
    		final int end2 = ptr + dx;
    		for (; ptr < end2; ptr++) {
    			ret_val += (int)doubleBuffer[ptr]*scaling;
    		}
    		ret_val += r_x1_i * (int)doubleBuffer[ptr];
    	}
    	// Last row
    	ret_val += D * (int)doubleBuffer[ptr];
    	ptr++;
    	final int end3 = ptr + dx;
    	for (; ptr < end3; ptr++) {
    		ret_val += r_y1_i * (int)doubleBuffer[ptr];
    	}
    	ret_val += C * (int)doubleBuffer[ptr];
    	
    	return (ret_val + scaling2/2)/scaling2;
    } // private int smoothedIntensity
    
    private boolean RoiPredicate(final double minX, final double minY, final double maxX, final double maxY,
    		                     final KeyPoint keyPt) {
        Point2d pt = keyPt.getPt();
        return (pt.x < minX) || (pt.x >= maxX) || (pt.y < minY) || (pt.y >= maxY);
    }
    
    // This is the subclass keypoint computation implementation
    private void computeImpl(ModelImage image, Vector<KeyPoint>keypoints, short[][] descriptors) {
    	int xDim = image.getExtents()[0];
    	int yDim = image.getExtents()[1];
    	int sliceSize = xDim * yDim;
    	double doubleBuffer[] = new double[sliceSize];
    	try {
    		image.exportData(0, sliceSize, doubleBuffer);
    	}
    	catch (IOException e) {
    		MipavUtil.displayError("IOException " + e + " on image.exportData(0, sliceSize, doubleBuffer) in computeImp1");
    		setCompleted(false);
    		return;
    	}
        // Remove keypoints very close to the border
    	int ksize = keypoints.size();
    	// Remember the scale per keypoint
    	Vector<Integer>kscales = new Vector<Integer>();
    	kscales.setSize(ksize);
    	final double log2 = 0.693147180559945;
    	final double lb_scaleRange = Math.log(scaleRange)/log2;
    	//Iterator <KeyPoint> beginning = keypoints.iterator();
    	int beginning = 0;
    	//Iterator <Integer> beginningkscales = kscales.iterator();
    	int beginningkscales = 0;
    	final double basicSize06 = basicSize * 0.6;
    	int basicScale = 0;
    	if (!scaleInvariant) {
    	    basicScale = Math.max((int)(scales/lb_scaleRange*(Math.log(1.45*basicSize/basicSize06)/log2)+0.5), 0);
    	    for (int k = 0; k < ksize; k++) {
    	        int scale;
    	        if (scaleInvariant) {
    	        	scale = Math.max((int)(scales/lb_scaleRange*(Math.log(keypoints.get(k).getSize()/basicSize06)/log2)+0.5), 0);
    	        	// Saturate
    	        	if (scale >= scales) {
    	        		scale = scales - 1;
    	        	}
    	        	kscales.set(k, Integer.valueOf(scale));
    	        } // if (scaleInvariant)
    	        else {
    	        	scale = basicScale;
    	        	kscales.set(k, Integer.valueOf(scale));
    	        }
    	        final int border = sizeList[scale];
    	        final int border_x = xDim - border;
    	        final int border_y = yDim - border;
    	        if (RoiPredicate(border, border, border_x, border_y, keypoints.get(k))) {
    	        	keypoints.remove(beginning+k);
    	        	kscales.remove(beginningkscales + k);
    	        	if (k == 0) {
    	        		beginning = 0;
    	        		beginningkscales = 0;
    	        	}
    	        	ksize--;
    	        	k--;
    	        } // if (RoiPredicate(border, border, border_x, border_y, keypoints.get(k)))
    	    } // for (int k = 0; k < ksize; k++)
    	    
    	    // First, calculate the integral image over the whole image:
    	    // Current integral image
    	    int extents[] = new int[2];
    	    extents[0] = xDim + 1;
    	    extents[1] = yDim+1;
    	    double integralBuffer[] = new double[extents[0]*extents[1]];
    	    for (int y = 0; y < yDim+1; y++) {
    	    	for (int x = 0; x < xDim + 1; x++) {
    	    		for (int y2 = 0; y2 < y; y2++) {
    	    			for (int x2 = 0; x2 < x; x2++) {
    	    				integralBuffer[x + y*(xDim+1)] += doubleBuffer[x2 + y2*xDim];
    	    			}
    	    		}
    	    	}
    	    }
    	    ModelImage integral = new ModelImage(ModelStorageBase.DOUBLE, extents, "integral");
    	    try {
    	    	integral.importData(0, integralBuffer, true);
    	    }
    	    catch (IOException e){
    	    	MipavUtil.displayError("IOexception " + e + " on integral.importData(0, integralBuffer, true) in computeImpl");
    	    	setCompleted(false);
    	    	return;
    	    }
    	    
    	    // For temporary use
    	    int values[] = new int[points];
    	    
    	    // Create the descriptors
    	    // ksize is the number of descriptos
    	    // strings is the number of shorts the descriptor consists of
    	    descriptors = new short[ksize][strings];
    	    
    	    // Now do the extraction for all keypoints:
    	    
    	    // Temporary variables containing gray values at sample pointsZZ
    	    int t1;
    	    int t2;
    	    
    	    // The feature orientation
    	    int direction0;
    	    int direction1;
    	    
    	    // Points to the start of descriptors
    	    int ptr = 0;
    	    for (int k = 0; k < ksize; k++) {
    	        int theta;
    	        KeyPoint kp = keypoints.get(k);
    	        final int scale = kscales.get(k);
    	        int shifter = 0;
    	        // Points to start of values
    	        int pvalues = 0;
    	        final double x = kp.getPt().x;
    	        final double y = kp.getPt().y;
    	        if (true /* kp.getAngle() == -1) */) {
    	            if (!rotationInvariant) {
    	            	// Don't compute the gradient direction, just assign a rotation of 0 degrees
    	            	theta = 0;
    	            }
    	            else {
    	            	// Get the gray values in the unrotated pattern
    	            	for (int i = 0; i < points; i++) {
    	            		values[pvalues++] = smoothedIntensity(image, integral, x, y, scale, 0 , i);
    	            	}
    	            	
    	            	direction0 = 0;
    	            	direction1 = 0;
    	            	// Now iterate through the long pairings
    	            	for (int iter = 0; iter <numLongPairs; ++iter) {
    	            	    t1 = values[longPairs[iter].getI()];
    	            	    t2 = values[longPairs[iter].getJ()];
    	            	    final int delta_t = t1 - t2;
    	            	    // Update the direction
    	            	    final int tmp0 = delta_t*longPairs[iter].getWeighted_dx()/1024;
    	            	    final int tmp1 = delta_t*longPairs[iter].getWeighted_dy()/1024;
    	            	    direction0 += tmp0;
    	            	    direction1 += tmp1;
    	            	} // for (int iter = 0; iter <numLongPairs; ++iter)
    	            	kp.setAngle(Math.atan2((double)direction1, (double)direction0)/Math.PI*180.0);
    	            	theta = (int)((n_rot*kp.getAngle())/360.0 + 0.5);
    	            	if (theta < 0) {
    	            		theta += n_rot;
    	            	}
    	            	if (theta >= (int)n_rot) {
    	            		theta -= n_rot;
    	            	}
    	            } // else
    	        } // if (true /* kp.getAngle() == -1) */)
    	        else {
    	        	// Figure out the direction
    	        } // else
    	    } // for (int k = 0; k < ksize; k++)
    	} // if (!scaleInvariant)
    } // private void computeImp1
    
    // Some helper classes for the Brisk pattern representation
    private class BriskPatternPoint {
    	private double x;  // x coordinate relative to center
    	private double y;  // y coordinate relative to center
    	private double sigma; // Gaussian smoothing sigma
    	
    	public BriskPatternPoint() {
    		
    	}
    	
    	public BriskPatternPoint(double x, double y, double sigma) {
    		this.x = x;
    		this.y = y;
    		this.sigma = sigma;
    	}
    	
    	public void setX(double x) {
    		this.x = x;
    	}
    	
    	public void setY(double y) {
    		this.y = y;
    	}
    	
    	public void setSigma(double sigma) {
    		this.sigma = sigma;
    	}
    	
    	public double getX() {
    		return x;
    	}
    	
    	public double getY() {
    		return y;
    	}
    	
    	public double getSigma() {
    		return sigma;
    	}
    }
    
    private class BriskShortPair {
    	private int i; // index of the first pattern point
    	private int j; // index of other pattern point
    	
    	public BriskShortPair() {
    		
    	}
    	
    	public BriskShortPair(int i, int j) {
    		this.i = i;
    		this.j = j;
    	}
    	
    	public void setI(int i) {
    		this.i = i;
    	}
    	
    	public void setJ(int j) {
    		this.j = j;
    	}
    	
    	public int getI() {
    		return i;
    	}
    	
    	public int getJ() {
    		return j;
    	}
    }
    
    private class BriskLongPair {
    	private int i; // index of the first pattern point
    	private int j; // index of other pattern point
    	private int weighted_dx; // 1024.0/dx
    	private int weighted_dy; // 1024.0/dy
    	
    	public BriskLongPair() {
    		
    	}
    	
    	public BriskLongPair(int i, int j, int weighted_dx, int weighted_dy) {
    		this.i = i;
    		this.j = j;
    		this.weighted_dx = weighted_dx;
    		this.weighted_dy = weighted_dy;
    	}
    	
    	public void setI(int i) {
    		this.i = i;
    	}
    	
    	public void setJ(int j) {
    		this.j = j;
    	}
    	
    	public void setWeighted_dx(int weighted_dx) {
    		this.weighted_dx = weighted_dx;
    	}
    	
    	public void setWeighted_dy(int weighted_dy) {
    		this.weighted_dy = weighted_dy;
    	}
    	
    	public int getI() {
    		return i;
    	}
    	
    	public int getJ() {
    		return j;
    	}
    	
    	public int getWeighted_dx() {
    		return weighted_dx;
    	}
    	
    	public int getWeighted_dy() {
    		return weighted_dy;
    	}
    }
    
    // Data structure for salient point detectors
    public class KeyPoint {
    	// Coordinates of the keypoint
    	private Point2d pt;
    	// Diameter of meaningful keypoint neighborhood
    	private double size;
    	// Computed orientation of the keypoint (-1 if not applicable)
    	// Its possible values are in the range [0,360) degrees.
    	// It is measured relative to the image coordinate system
    	// (y-axis is directed downward), i.e. clockwise
    	private double angle = -1.0;
    	// The response by which the most strong keypoints have been selected.
    	// Can be used for further sorting or subsampling
    	private double response = 0.0;
    	// Octave (pyramid layer) from which the keypoint has been selected
    	private int octave = 0;
    	// Object id that can be used to cluster keypoint by an object they belong to
    	private int class_id = -1;
    	
    	public KeyPoint() {
    		
    	}
    	
    	public KeyPoint(Point2d pt, double size) {
    		this.pt = pt;
    		this.size = size;
    		this.angle = -1.0;
    		this.response = 0.0;
    		this.octave = 0;
    		this.class_id = -1;
    	}
    	
    	public void setAngle(double angle) {
    		this.angle = angle;
    	}
    	
    	public Point2d getPt() {
    		return pt;
    	}
    	
    	public double getSize() {
    		return size;
    	}
    	
    	public double getAngle() {
    		return angle;
    	}
    }
    
}
