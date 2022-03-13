package gov.nih.mipav.model.algorithms;

import gov.nih.mipav.model.algorithms.AlgorithmBase;
import gov.nih.mipav.model.structures.ModelImage;
import gov.nih.mipav.model.structures.ModelStorageBase;

import java.io.IOException;
import java.util.Arrays;
import java.util.Vector;

import gov.nih.mipav.view.ViewJProgressBar;

/**
 * % Copyright (c) 2013 Peter Kovesi
% www.peterkovesi.com/matlabfns/
% 
% Permission is hereby granted, free of charge, to any person obtaining a copy
% of this software and associated documentation files (the "Software"), to deal
% in the Software without restriction, subject to the following conditions:
% 
% The above copyright notice and this permission notice shall be included in 
% all copies or substantial portions of the Software.
%
% The Software is provided "as is", without warranty of any kind.
 * 
 * 
 * The image must be a color image
 * First the slic.m software performs a SLIC Simple Linear Iterative Clustering SuperPixels.
 * SLIC is followed by SPDBSCAN.m to perform a DBSCAN clustering of superpixels.  This results in
 * a simple and fast segmentation of an image.
 * 
 * Ported MATLAB files:
 *  slic.m Implementation of Achanta, Shaji, Smith, Lucchi, Fua and Susstrunk's SLIC Superpixels.
•	spdbscan.m Implements DBSCAN clustering of superpixels.
•	cleanupregions.m Cleans up small regions in a segmentation. Used by slic.m
•	mcleanupregions.m Morphological version of cleanupregions.m The output is not quite as nice but the execution is much faster.
•	finddisconnected.m Finds groupings of disconnected labeled regions. Used by mcleanupregions.m to reduce execution time.
•	makeregionsdistinct.m Ensures labeled regions are distinct.
•	renumberregions.m Ensures all regions in labeled image have a unique label and that the label numbering forms a contiguous sequence.
•	regionadjacency.m Computes adjacency matrix for an image of labeled segmented regions.
•	drawregionboundaries.m Draw boundaries of labeled regions in an image.
•	maskimage.m used by drawregionboundaries.m
    circularstruct.m Generates a circular structuring element, used by mcleanupregions.m
•	dbscan.m Basic implementation of DBSCAN
•	testdbscan.m Function to test/demonstrate dbscan.m


 * 
 * References:
•	R. Achanta, A. Shaji, K. Smith, A. Lucchi, P. Fua and S. Susstrunk. "SLIC Superpixels Compared
    to State-of-the-Art Superpixel Methods" PAMI. Vol 34 No 11. November 2012. pp 2274-2281.
•	Martin Ester, Hans-Peter Kriegel, Jörg Sander, Xiaowei Xu (1996). "A density-based algorithm
    for discovering clusters in large spatial databases with noise". Proceedings of the Second International
    Conference on Knowledge Discovery and Data Mining (KDD-96). AAAI Press. pp. 226-231.

 */

public class DBSCANClusteringSegment extends AlgorithmBase {
	
	//~ Instance fields ------------------------------------------------------------------------------------------------
	// SLIC input parameters:
    private final int MEAN_CENTER = 1;
    private final int MEDIAN_CENTER = 2;
	// Number of desired superpixels. Note that this is nominal
	// the actual number of superpixels generated will generally
	// be a bit larger, especially if parameter m is small.
	private int k;
	// Weighting factor between colour and spatial
	// differences. Values from about 5 to 40 are useful.  Use a
	// large value to enforce superpixels with more regular and
	// smoother shapes. Try a value of 10 to start with.
	private double m = 10.0;
	// mRegions morphologically smaller than this are merged with
	// adjacent regions. Try a value of 1 or 1.5.  Use 0 to
	// disable.
	private double seRadius = 1.0;
	// String "mean" or "median" indicating how the cluster
	// colour centre should be computed. Defaults to "mean"
	private int center = MEAN_CENTER;
	// Optional median filtering window size.  Image compression
	// can result in noticeable artifacts in the a*b* components
	// of the image.  Median filtering can reduce this. mw can be
	// a single value in which case the same median filtering is
	// applied to each L* a* and b* components.  Alternatively it
	// can be a 2-vector where mw(1) specifies the median
	// filtering window to be applied to L* and mw(2) is the
	// median filtering window to be applied to a* and b*.
	private int mw1 = 0;
	private int mw2 = 0;
	private int nItr = 10;
	
	// SPDBSCAN input parameter:
	// Matching tolerance value/distance threshold that controls which
	// superpixels are clustered together.  This value is in L*a*b*
	// colour units.  Try a value in the range 5-10 to start with.
	// Changing the value of E by just 1 unit can give a significant
	// difference. 
	private double E = 7.5;
	
	//~ Constructors ---------------------------------------------------------------------------------------------------
	
		public DBSCANClusteringSegment(ModelImage destImg, ModelImage srcImg, int k,
				double m, double seRadius, int center, int mw1, int mw2, 
				int nItr, double E) {
			super(destImg, srcImg);
			this.k = k;
			this.m = m;
			this.seRadius = seRadius;
			this.center = center;
			this.mw1 = mw1;
			this.mw2 = mw2;
			this.nItr = nItr;
			this.E = E;
		}
	
	public void runAlgorithm() {
		double scaleMax;
		float buffer[];
		double Labbuf[][];
		int i;
		double varR;
		double varG;
		double varB;
		double X,Y,Z;
		double varX, varY, varZ;
		// Observer = 2 degrees, Illuminant = D65
        double XN = 95.047;
        double YN = 100.000;
        double ZN = 108.883;
        double L;
        double a;
        double b;
        double minL = Double.MAX_VALUE;
        double maxL = -Double.MAX_VALUE;
        double mina = Double.MAX_VALUE;
        double maxa = -Double.MAX_VALUE;
        double minb = Double.MAX_VALUE;
        double maxb = -Double.MAX_VALUE;
		if (srcImage == null) {
            displayError("Source Image is null");
            finalize();

            return;
        }
		
		if (!srcImage.isColorImage()) {
			displayError("Source image must be a color image");
			finalize();
			return;
		}
		
		// Convert image to L*a*b* colourspace.  This gives us a colourspace that is
	    // nominally perceptually uniform. This allows us to use the euclidean
	    // distance between colour coordinates to measure differences between
	    // colours.  Note the image becomes double after conversion.  We may want to
	    // go to signed shorts to save memory.
		// The adapted white point is D65.
		// Convert RGB to CIE 1976 L*a*b
		// The three coordinates of CIELAB represent the lightness of the color(L* = 0 yields black and L* = 100 indicates diffuse 
	    // white; specular white may be higher), its position between red/magenta and green(a*, negative values indicate green
	    // while positive values indicate magenta) and its position between yellow and blue(b*, negative values indicate blue 
	    // and positive values indicate yellow).  The asterisk(*) after L, a, and b are part of the full name, since they represent 
	    // L*, a*, and b*, to distinguish them from Hunter's L, a, and b.
	    		  
	    // The L* coordinate ranges from 0 to 100.  The possible range of a* and b* coordinates depends on the color space that one
	    // is converting from.  
	    // R = 0, G = 0, B = 0 => L* = 0, a* = 0, b* = 0
	    // R = 255, G = 0, B = 0 => L* = 53.2, a* = 80.1, b* = 67.22
	    // R = 0, G = 255, B = 0 => L* = 87.7, a* = -86.2, b* = 83.2
	    // R = 0, G = 0, B = 255 => L* = 32.3, a* = 79.2, b* = -107.9
	    // R = 255, G = 255, B = 0 => L* = 97.1, a* = -21.6, b* = 94.5
	    // R = 255, G = 0, B = 255 => L* = 60.3, a* = 98.3, b* = -60.8
	    // R = 0, G = 255, B = 255 => L* = 91.1, a* = -48.1, b* = -14.1
	    // R = 255, G = 255, B = 255 => L* = 100.0, a* = 0.00525, b* = -0.0104
	    // so the range of a* equals about the range of b* and the range of a* equals about twice the range of L*.
	    // The simplest distance metric delta E is CIE76 = sqrt((L2* - L1*)**2 + (a2* - a1*)**2 + (b2* - b1*)**2)
	    		
	    // XW, YW, and ZW (also called XN, YN, ZN or X0, Y0, Z0) are reference white tristimulus values - typically the white
	    // of a perfectly reflecting diffuser under CIE standard D65 illumination(defined by x = 0.3127 and y = 0.3291 in the
	    // CIE chromatcity diagram).  The 2 degrees, D65 reference tristimulus values are: XN = 95.047, YN = 100.000, and ZN = 108.883.
	    
	    // Scale factor used in RGB-CIELab conversions.  255 for ARGB, could be higher for ARGB_USHORT.
		int xDim = srcImage.getExtents()[0];
		int yDim = srcImage.getExtents()[1];
		int length = xDim * yDim;
		scaleMax = Math.max(255.0, srcImage.getMax());
		buffer =  new float[4 * length];
		Labbuf = new double[length][3];
		try {
			srcImage.exportData(0, length, buffer);
		}
		catch(IOException e) {
			e.printStackTrace();
			setCompleted(false);
			return;
		}
		for (i = 0; i < buffer.length; i += 4) {
            varR = buffer[i+1]/scaleMax;
            varG = buffer[i+2]/scaleMax;
            varB = buffer[i+3]/scaleMax;
            
            if (varR <= 0.04045) {
                varR = varR/12.92;
            }
            else {
                varR = Math.pow((varR + 0.055)/1.055, 2.4);
            }
            if (varG <= 0.04045) {
                varG = varG/12.92;
            }
            else {
                varG = Math.pow((varG + 0.055)/1.055, 2.4);
            }
            if (varB <= 0.04045) {
                varB = varB/12.92;
            }
            else {
                varB = Math.pow((varB + 0.055)/1.055, 2.4);
            }
            
            varR = 100.0 * varR;
            varG = 100.0 * varG;
            varB = 100.0 * varB;
            
            // Observer = 2 degrees, Illuminant = D65
            X = 0.4124*varR + 0.3576*varG + 0.1805*varB;
            Y = 0.2126*varR + 0.7152*varG + 0.0722*varB;
            Z = 0.0193*varR + 0.1192*varG + 0.9505*varB;
            
            varX = X/ XN;
            varY = Y/ YN;
            varZ = Z/ ZN;
            
            if (varX > 0.008856) {
                varX = Math.pow(varX, 1.0/3.0);
            }
            else {
                varX = (7.787 * varX) + (16.0/116.0);
            }
            if (varY > 0.008856) {
                varY = Math.pow(varY, 1.0/3.0);
            }
            else {
                varY = (7.787 * varY) + (16.0/116.0);
            }
            if (varZ > 0.008856) {
                varZ = Math.pow(varZ, 1.0/3.0);
            }
            else {
                varZ = (7.787 * varZ) + (16.0/116.0);
            }
            
            L = ((116.0 * varY) - 16.0);
            a = (500.0 * (varX - varY));
            b = (200.0 * (varY - varZ));
            
            if (L < minL) {
                minL = L;
            }
            if (L > maxL) {
                maxL = L;
            }
            if (a < mina) {
                mina = a;
            }
            if (a > maxa) {
                maxa = a;
            }
            if (b < minb) {
                minb = b;
            }
            if (b > maxb) {
                maxb = b;
            }
            
            Labbuf[i/4][0] = L;
            Labbuf[i/4][1] = a;
            Labbuf[i/4][2] = b;
        } // for (i = 0; i < buffer.length; i += 4)
}
	
}