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
 *	spdbscan.m Implements DBSCAN clustering of superpixels.
 *	cleanupregions.m Cleans up small regions in a segmentation. Used by slic.m
 *	mcleanupregions.m Morphological version of cleanupregions.m The output is not quite as nice but the execution is much faster.
 *	finddisconnected.m Finds groupings of disconnected labeled regions. Used by mcleanupregions.m to reduce execution time.
 *	makeregionsdistinct.m Ensures labeled regions are distinct.
 *	renumberregions.m Ensures all regions in labeled image have a unique label and that the label numbering forms a contiguous sequence.
 *	regionadjacency.m Computes adjacency matrix for an image of labeled segmented regions.
 *	drawregionboundaries.m Draw boundaries of labeled regions in an image.
 *	maskimage.m used by drawregionboundaries.m
 *  circularstruct.m Generates a circular structuring element, used by mcleanupregions.m
 *	dbscan.m Basic implementation of DBSCAN
 *	testdbscan.m Function to test/demonstrate dbscan.m


 * 
 * References:
 *	R. Achanta, A. Shaji, K. Smith, A. Lucchi, P. Fua and S. Susstrunk. "SLIC Superpixels Compared
 *  to State-of-the-Art Superpixel Methods" PAMI. Vol 34 No 11. November 2012. pp 2274-2281.
 *	Martin Ester, Hans-Peter Kriegel, Jorg Sander, Xiaowei Xu (1996). "A density-based algorithm
 *  for discovering clusters in large spatial databases with noise". Proceedings of the Second International
 *  Conference on Knowledge Discovery and Data Mining (KDD-96). AAAI Press. pp. 226-231.

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
	// Changing the value of Ec by just 1 unit can give a significant
	// difference. 
	private double Ec = 7.5;
	
	//~ Constructors ---------------------------------------------------------------------------------------------------
	
		public DBSCANClusteringSegment(ModelImage destImg, ModelImage srcImg, int k,
				double m, double seRadius, int center, int mw1, int mw2, 
				int nItr, double Ec) {
			super(destImg, srcImg);
			this.k = k;
			this.m = m;
			this.seRadius = seRadius;
			this.center = center;
			this.mw1 = mw1;
			this.mw2 = mw2;
			this.nItr = nItr;
			this.Ec = Ec;
		}
	
	public void runAlgorithm() {
		double scaleMax;
		float buffer[];
		double Labbuf[][];
		int i,j,index,n;
		double varR;
		double varG;
		int x,y;
		double varB;
		double XL,YL,ZL;
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
        int halfmw1;
        int halfmw2;
        int ymin;
        int ymax;
        int xmin;
        int xmax;
        int medlength;
        double med[] = null;
        double medarray[];
        int medptr;
        double S;
        int nodeCols;
        int nodeRows;
        double vSpacing;
        double C[][];
        int l[][];
        double d[][];
        int kk;
        double r;
        int ri;
        double c;
        int ci;
        int cc;
        int rr;
        int intS;
        int rmin;
        int rmax;
        int cmin;
        int cmax;
        int rlen;
        int clen;
        double subim[][][];
        double Ckk[];
        double D[][];
        AmAl amal;
        int N;
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
		
		// SLIC Simple Linear Iterative Clustering SuperPixels
		
		// Implementation of Achanta, Shaji, Smith, Lucchi, Fua and Susstrunk's
		// SLIC Superpixels
		
		// Usage:   [l, Am, Sp, d] = slic(im, k, m, seRadius, colopt, mw)
		
		// Arguments:  im - Image to be segmented.
		//              k - Number of desired superpixels. Note that this is nominal
		//                  the actual number of superpixels generated will generally
		//                  be a bit larger, especially if parameter m is small.
		//              m - Weighting factor between colour and spatial
		//                  differences. Values from about 5 to 40 are useful.  Use a
		//                  large value to enforce superpixels with more regular and
		//                  smoother shapes. Try a value of 10 to start with.
		//       seRadius - Regions morphologically smaller than this are merged with
		//                  adjacent regions. Try a value of 1 or 1.5.  Use 0 to
		//                  disable.
		//         colopt - String 'mean' or 'median' indicating how the cluster
		//                  colour centre should be computed. Defaults to 'mean'
		//             mw - Optional median filtering window size.  Image compression
		//                  can result in noticeable artifacts in the a*b* components
		//                  of the image.  Median filtering can reduce this. mw can be
		//                  a single value in which case the same median filtering is
		//                  applied to each L* a* and b* components.  Alternatively it
		//                  can be a 2-vector where mw(1) specifies the median
		//                  filtering window to be applied to L* and mw(2) is the
		//                  median filtering window to be applied to a* and b*.
		
		// Returns:    l - Labeled image of superpixels. Labels range from 1 to k.
		//             Am - Adjacency matrix of segments.  Am(i, j) indicates whether
		//                  segments labeled i and j are connected/adjacent
		//             Sp - Superpixel attribute structure array with fields:
		//                   L  - Mean L* value
		//                   a  - Mean a* value
		//                   b  - Mean b* value
		//                   r  - Mean row value
		//                   c  - Mean column value
		//                   stdL  - Standard deviation of L* 
		//                   stda  - Standard deviation of a* 
		//                   stdb  - Standard deviation of b* 
		//                   N - Number of pixels
		//                   edges - List of edge numbers that bound each
		//                           superpixel. This field is allocated, but not set,
		//                           by SLIC. Use SPEDGES for this.
		//              d - Distance image giving the distance each pixel is from its
		//                  associated superpixel centre.
		
		// It is suggested that use of this function is followed by SPDBSCAN to perform a
		// DBSCAN clustering of superpixels.  This results in a simple and fast
		// segmentation of an image.
		
		// Minor variations from the original algorithm as defined in Achanta et al's
		// paper:
		
		// - SuperPixel centres are initialised on a hexagonal grid rather than a square
		//   one. This results in a segmentation that will be nominally 6-connected
		//   which hopefully facilitates any subsequent post-processing that seeks to
		//   merge superpixels.
		// - Initial cluster positions are not shifted to point of lowest gradient
		//   within a 3x3 neighbourhood because this will be rendered irrelevant the
		//   first time cluster centres are updated.
		
		// Reference: R. Achanta, A. Shaji, K. Smith, A. Lucchi, P. Fua and
		// S. Susstrunk. "SLIC Superpixels Compared to State-of-the-Art Superpixel
		// Methods"  PAMI. Vol 34 No 11.  November 2012. pp 2274-2281.
		
		// See also: SPDBSCAN, MCLEANUPREGIONS, REGIONADJACENCY, DRAWREGIONBOUNDARIES, RGB2LAB

		// Copyright (c) 2013 Peter Kovesi
		// www.peterkovesi.com/matlabfns/
		 
		// Permission is hereby granted, free of charge, to any person obtaining a copy
		// of this software and associated documentation files (the "Software"), to deal
		// in the Software without restriction, subject to the following conditions:
		
		// The above copyright notice and this permission notice shall be included in 
		// all copies or substantial portions of the Software.
		
		// The Software is provided "as is", without warranty of any kind.

		// Feb  2013
		// July 2013 Super pixel attributes returned as a structure array

		// Note that most of the computation time is not in the clustering, but rather
		// in the region cleanup process.

		
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
			srcImage.exportData(0, 4*length, buffer);
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
            XL = 0.4124*varR + 0.3576*varG + 0.1805*varB;
            YL = 0.2126*varR + 0.7152*varG + 0.0722*varB;
            ZL = 0.0193*varR + 0.1192*varG + 0.9505*varB;
            
            varX = XL/ XN;
            varY = YL/ YN;
            varZ = ZL/ ZN;
            
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
		
		// Apply median filtering to colour components if mw has been supplied
	    // and/or non-zero
		if ((mw1 > 0) || (mw2 > 0)) {
			med = new double[length];
		}
		if (mw1 > 0) {
			halfmw1 = (mw1 -1)/2;
		    for (y = 0; y < yDim; y++) {
		    	for (x = 0; x < xDim; x++) {
		    	    index = x + y * xDim;
		    	    ymin = Math.max(0,y-halfmw1);
		    	    ymax = Math.min(yDim-1,y+halfmw1);
		    	    xmin = Math.max(0,x-halfmw1);
	    	    	xmax = Math.min(xDim-1,x+halfmw1);
	    	    	medlength = (ymax-ymin+1)*(xmax-xmin+1);
	    	    	medarray = new double[medlength];
		    	    for (j = ymin, medptr = 0; j <= ymax; y++) {
		    	    	for (i = xmin; i <= xmax; x++, medptr++) {
		    	    	    medarray[medptr] = Labbuf[i + j * xDim][0];
		    	    	}
		    	    }
		    	    Arrays.sort(medarray);
		    	    if ((medlength % 2) == 1) {
		    	    	med[index] = medarray[(medlength-1)/2];
		    	    }
		    	    else {
		    	    	med[index] = (medarray[medlength/2] + medarray[(medlength/2)-1])/2.0;
		    	    }
		    	}
		    }
		    for (y = 0; y < yDim; y++) {
		    	for (x = 0; x < xDim; x++) {
		    	    index = x + y * xDim;
		    	    Labbuf[index][0] = med[index];
		    	}
		    }
		} // if (mw1 > 0)
		if (mw2 > 0) {
			halfmw2 = (mw2 -1)/2;
			for (n = 1; n <= 2; n++) {
			    for (y = 0; y < yDim; y++) {
			    	for (x = 0; x < xDim; x++) {
			    	    index = x + y * xDim;
			    	    ymin = Math.max(0,y-halfmw2);
			    	    ymax = Math.min(yDim-1,y+halfmw2);
			    	    xmin = Math.max(0,x-halfmw2);
		    	    	xmax = Math.min(xDim-1,x+halfmw2);
		    	    	medlength = (ymax-ymin+1)*(xmax-xmin+1);
		    	    	medarray = new double[medlength];
			    	    for (j = ymin, medptr = 0; j <= ymax; y++) {
			    	    	for (i = xmin; i <= xmax; x++, medptr++) {
			    	    	    medarray[medptr] = Labbuf[i + j * xDim][n];
			    	    	}
			    	    }
			    	    Arrays.sort(medarray);
			    	    if ((medlength % 2) == 1) {
			    	    	med[index] = medarray[(medlength-1)/2];
			    	    }
			    	    else {
			    	    	med[index] = (medarray[medlength/2] + medarray[(medlength/2)-1])/2.0;
			    	    }
			    	}
			    }
			    for (y = 0; y < yDim; y++) {
			    	for (x = 0; x < xDim; x++) {
			    	    index = x + y * xDim;
			    	    Labbuf[index][n] = med[index];
			    	}
			    }
			} // for (n = 1; n <= 2; n++)
		} // if (mw2 > 0)
		
		// Nominal spacing between grid elements assuming hexagonal grid
	    S = Math.sqrt(length / (k * Math.sqrt(3.0)/2.0));
	    
	    // Get nodes per row allowing a half column margin at one end that alternates
	    // from row to row
	    nodeCols = (int)Math.round(xDim/S - 0.5);
	    // Given an integer number of nodes per row recompute S
	    S = xDim/(nodeCols + 0.5); 

	    // Get number of rows of nodes allowing 0.5 row margin top and bottom
	    nodeRows = (int)Math.round(yDim/(Math.sqrt(3.0)/2.0*S));
	    vSpacing = (double)yDim/(double)nodeRows;

	    // Recompute k
	    k = nodeRows * nodeCols;
	    
	    // Allocate memory and initialise clusters, labels and distances.
	    C = new double[6][k];          // Cluster centre data  1:3 is mean Lab value,
	                                   // 4:5 is col, row of centre, 6 is No of pixels
	    Ckk = new double[6];
	    // Pixel labels.
	    l = new int[yDim][xDim];
	    for (y = 0; y < yDim; y++) {
	    	for (x = 0; x < xDim; x++) {
	    		l[y][x] = -1;
	    	}
	    }
	    // Pixel distances from cluster centres.
	    d = new double[yDim][xDim];
	    for (y = 0; y < yDim; y++) {
	    	for (x = 0; x < xDim; x++) {
	    		d[y][x] = Double.POSITIVE_INFINITY;
	    	}
	    }
	    
	    // Initialise clusters on a hexagonal grid
	    kk = 0;
	    r = vSpacing/2;
	    rr = (int)Math.round(r)-1;
	    
	    for (ri = 1; ri <= nodeRows; ri++) {
	        // Following code alternates the starting column for each row of grid
	        // points to obtain a hexagonal pattern. Note S and vSpacing are kept
	        // as doubles to prevent errors accumulating across the grid.
	        if ((ri % 2) == 1) {
	        	c = S/2.0; 
	        }
	        else {
	        	c = S;
	        }
	        
	        for (ci = 1; ci <= nodeCols; ci++) {
	            cc = (int)Math.round(c)-1; 
	            index = cc + rr*xDim;
	            C[0][kk] = Labbuf[index][0];
	            C[1][kk] = Labbuf[index][1];
	            C[2][kk] = Labbuf[index][2];
	            C[3][kk] = cc;
	            C[4][kk] = rr;
	            c = c+S;
	            kk = kk+1;
	        } // for (ci = 1; ci <= nodeCols; ci++)
	        
	        r = r+vSpacing;
	        rr = (int)Math.round(r)-1;
	    } // for (ri = 1; ri <= nodeRows; ri++)
	    
	    
	    // Now perform the clustering.  10 iterations is suggested but I suspect n
	    // could be as small as 2 or even 1
	    intS = (int)Math.round(S);  // We need S to be an integer from now on
	    
	    for (n = 1; n <= nItr; n++) {
	       for (kk = 0; kk < k; kk++) {  // for each cluster

	           // Get subimage around cluster
	           rmin = (int)Math.max(C[4][kk]-intS, 0);   
	           rmax = (int)Math.min(C[4][kk]+S, yDim-1);
	           if (rmax < rmin) {
	        	   System.err.println("rmax < rmin");
	        	   setCompleted(false);
	        	   return;
	           }
	           rlen = rmax-rmin+1;
	           cmin = (int)Math.max(C[3][kk]-S, 0);   
	           cmax = (int)Math.min(C[3][kk]+S, xDim-1);
	           if (cmax < cmin)  {
	        	   System.err.println("cmax < cmin");
	        	   setCompleted(false);
	        	   return;
	           }
	           clen = cmax-cmin+1;
	           subim = new double[rlen][clen][3];
	           for (y = rmin; y <= rmax; y++) {
	        	   for (x = cmin; x <= cmax; x++) {
	        		   index = x + y * xDim;
	        		   for (i = 0; i < 3; i++) {
	        			   subim[y-rmin][x-cmin][i] = Labbuf[index][i];
	        		   }
	        	   }
	           }
	           
	           // Compute distances D between C(:,kk) and subimage
	           for (i = 0; i < 6; i++) {
	        	   Ckk[i] = C[i][kk]; 
	           }
	           D = dist(Ckk, subim, rmin, cmin, S, m);
	           

	           // If any pixel distance from the cluster centre is less than its
	           // previous value update its distance and label
	           for (y = rmin; y <= rmax; y++) {
	        	   for (x = cmin; x <= cmax; x++) {
	        		   if (D[y-rmin][x-cmin] < d[y][x]) {
	        			   d[y][x] = D[y-rmin][x-cmin];
	        			   l[y][x] = kk;
	        		   }
	        	   }
	           }           
	       } // for (kk = 0; kk < k; kk++)
	       
	       // Update cluster centres with mean values
	       for (i = 0; i < 6; i++) {
	    	   for (j = 0; j < k; j++) {
	    		   C[i][j] = 0.0;
	    	   }
	       }
	       for (y = 0; y < yDim; y++) {
	           for (x = 0; x < xDim; x++) {
	        	  index = x + y * xDim;
	        	  C[0][l[y][x]] = C[0][l[y][x]] + Labbuf[index][0];
	        	  C[1][l[y][x]] = C[1][l[y][x]] + Labbuf[index][1];
	        	  C[2][l[y][x]] = C[2][l[y][x]] + Labbuf[index][2];
	        	  C[3][l[y][x]] = C[3][l[y][x]] + x;
	        	  C[4][l[y][x]] = C[4][l[y][x]] + y;
	        	  C[5][l[y][x]] = C[5][l[y][x]] + 1.0;
	           } // for (x = 0; x < xDim; x++)
	       } // for (y = 0; y < yDim; y++)
	       
	       // Divide by number of pixels in each superpixel to get mean values
	       for (kk = 0; kk < k; kk++) { 
	    	   for (i = 0; i < 5; i++) {
	    		   C[i][kk] = Math.round(C[i][kk]/C[5][kk]);
	    	   }
	       } // for (kk = 0; kk < k; kk++)
	       
	       // Note the residual error, E, is not calculated because we are using a
	       // fixed number of iterations 
	    } // for (n = 1; n <= nItr; n++)
	       
       // Cleanup small orphaned regions and 'spurs' on each region using
       // morphological opening on each labeled region.  The cleaned up regions are
       // assigned to the nearest cluster. The regions are renumbered and the
       // adjacency matrix regenerated.  This is needed because the cleanup is
       // likely to change the number of labeled regions.
       if (seRadius > 0) {
           //[l, Am] = mcleanupregions(l, seRadius);
    	   amal = mcleanupregions(l, seRadius);
       } // if (seRadius > 0)
       else {
    	   int maxLabel = makeregionsdistinct(l,4); 
    	   Renum re = renumberregions(l);
	   	   for (y = 0; y < yDim; y++) {
	   	       for (x = 0; x < xDim; x++) {
	   	    	   l[y][x] = re.nL[y][x];
	   	       }
	   	   }
	   	   amal = regionadjacency(l,8);
       } // else
       
       // Recompute the final superpixel attributes and write information into
       // the Sp struct array.
       N = amal.Ami.length;
       SP Sp = new SP(N);
	   int Y[][] = new int[yDim][xDim];
	   int X[][] = new int[yDim][xDim];
	   for (y = 0; y < yDim; y++) {
		   for (x = 0; x < xDim; x++) {
			   X[y][x] = x;
			   Y[y][x] = y;
		   }
	   }
	   
	   byte mask[][] = new byte[yDim][xDim];
	   int nm;
	   for (n = 0; n < N; n++) {
		   nm = 0;
	       for (y = 0; y < yDim; y++) {
	    	   for (x = 0; x < xDim; x++) {
	    		   if (l[y][x] == (n+1)) {
	    			   mask[y][x] = 1;
	    			   nm++;
	    		   }
	    		   else {
	    			   mask[y][x] = 0;
	    		   }
	    	   }
	       } // for (y = 0; y < yDim; y++)
	       if (center == MEAN_CENTER) {
	    	   double sumL = 0.0;
	    	   double suma = 0.0;
	    	   double sumb = 0.0;
	    	   for (y = 0; y < yDim; y++) {
	    		   for (x = 0; x < xDim; x++) {
	    			   if (mask[y][x] == 1) {
	    				   index = x + y * xDim;
	    				   sumL += Labbuf[index][0];
	    				   suma += Labbuf[index][1];
	    				   sumb += Labbuf[index][2];
	    			   }
	    		   }
	    	   } // for (y = 0; y < yDim; y++)
	    	   Sp.L[n] = sumL/nm;
	    	   Sp.a[n] = suma/nm;
	    	   Sp.b[n] = sumb/nm;
	       } // if (center == MEAN_CENTER)
	       else if (center == MEDIAN_CENTER) {
	           double Larray[] = new double[nm];
	           double aarray[] = new double[nm];
	           double barray[] = new double[nm];
	           for (y = 0, i = 0; y < yDim; y++) {
	    		   for (x = 0; x < xDim; x++) {
	    			   if (mask[y][x] == 1) {
	    				   index = x + y * xDim;
	    				   Larray[i]= Labbuf[index][0];
	    				   aarray[i]= Labbuf[index][1];
	    				   barray[i] = Labbuf[index][2];
	    			   }
	    		   }
	    	   } // for (y = 0, i = 0; y < yDim; y++)
	           Arrays.sort(Larray);
	           Arrays.sort(aarray);
	           Arrays.sort(barray);
	           if ((nm % 2) == 1) {
	    	    	Sp.L[n] = Larray[(nm-1)/2];
	    	    	Sp.a[n] = aarray[(nm-1)/2];
	    	    	Sp.b[n] = barray[(nm-1)/2];
	    	    }
	    	    else {
	    	    	Sp.L[n] = (Larray[nm/2] + Larray[(nm/2)-1])/2.0;
	    	    	Sp.a[n] = (aarray[nm/2] + aarray[(nm/2)-1])/2.0;
	    	    	Sp.b[n] = (barray[nm/2] + barray[(nm/2)-1])/2.0;
	    	    }
	       } // else if (center == MEDIAN_CENTER)
	       
	       double sumy = 0.0;
	       double sumx = 0.0;
	       for (y = 0; y < yDim; y++) {
	    	   for (x = 0; x < xDim; x++) {
	    		   if (mask[y][x] == 1) {
	    			   sumy += Y[y][x];
	    			   sumx += X[y][x];
	    		   }
	    	   }
	       }
	       Sp.r[n] = sumy/nm;
	       Sp.c[n] = sumx/nm;
	       
	       // Compute standard deviations of the colour components of each super
	       // pixel. This can be used by code seeking to merge superpixels into
	       // image segments.  Note these are calculated relative to the mean colour
	       // component irrespective of the centre being calculated from the mean or
	       // median colour component values.
	       double sumL = 0.0;
    	   double suma = 0.0;
    	   double sumb = 0.0;
    	   for (y = 0; y < yDim; y++) {
    		   for (x = 0; x < xDim; x++) {
    			   if (mask[y][x] == 1) {
    				   index = x + y * xDim;
    				   sumL += Labbuf[index][0];
    				   suma += Labbuf[index][1];
    				   sumb += Labbuf[index][2];
    			   }
    		   }
    	   } // for (y = 0; y < yDim; y++)
    	   double meanL = sumL/nm;
    	   double meana = suma/nm;
    	   double meanb = sumb/nm;
    	   
    	   double diff;
    	   double Lsquared = 0.0;
    	   double asquared = 0.0;
    	   double bsquared = 0.0;
    	   for (y = 0; y < yDim; y++) {
    		   for (x = 0; x < xDim; x++) {
    			   if (mask[y][x] == 1) {
    				   index = x + y * xDim;
    				   diff = (Labbuf[index][0] - meanL);
    				   Lsquared += (diff*diff);
    				   diff = (Labbuf[index][1] - meana);
    				   asquared += (diff*diff);
    				   diff = (Labbuf[index][2] - meanb);
    				   bsquared += (diff*diff);
    			   }
    		   }
    	   } // for (y = 0; y < yDim; y++)
    	   Sp.stdL[n] = Math.sqrt(Lsquared/(nm - 1.0));
		   Sp.stda[n] = Math.sqrt(asquared/(nm - 1.0));
		   Sp.stdb[n] = Math.sqrt(bsquared/(nm - 1.0));
		   
		   // Record number of pixels in superpixel too.
		   Sp.N[n] = nm;
	   } // for (n = 0; n < N; n++)
	   
	   // SPDBSCAN SuperPixel DBSCAN clustering for image segmentation
	   
	   // Usage:  [lc, C, regionsC] = spdbscan(l, Cp, Am, E)
	   
	   // Arguments:  (Note this code is structured assuming the input superpixels
	   //              have been generated using SLIC)
	   //       l   - Labeled image of clusters/regions generated by a superpixel
	   //             algorithm,  such as  SLIC. 
	   //       Cp  - 5 x Np array, as returned by SLIC. Each column giving the
	   //             attributes of each superpixel region.  Only the first 3
	   //             attributes, the Lab colour values, are used. 
	   //       Am  - An adjacency matrix of the labeled image (also returned by SLIC).
	   //       E   - Matching tolerance value/distance threshold that controls which
	   //             superpixels are clustered together.  This value is in L*a*b*
	   //             colour units.  Try a value in the range 5-10 to start with.
	   //             Changing the value of E by just 1 unit can give a significant
	   //             difference. 
	   
	   // Returns:
	   //        lc - New labeled image corresponding to the new clustered regions of
	   //             superpixels.
	   //         C - Cell array of length Nc listing indices of superpixel regions
	   //             associated with each cluster.
	   //  regionsC - Array of length Np listing the cluster number associated with
	   //             each superpixel region.  
	   
	   // This function performs an image segmentation using the DBSCAN algorithm to
	   // form clusters of superpixels.  In determining the neighbourhood of any
	   // superpixel the following criterion is used: If any two superpixels are
	   // adjacent the clustering distance measure is the lab colour distance between
	   // the colour centres of the two superpixels.  If any two superpixels are not
	   // adjacent the clustering distance measure is assumed infinite.  The use of an
	   // adjacency matrix allows the DBSCAN scan algorithm to be efficient in
	   // determining the neighbourhood of each superpixel.
	   
	   // See also: SLIC, REGIONADJACENCY, DBSCAN, DRAWREGIONBOUNDARIES

	   // DBSCAN Reference: 
	   // Martin Ester, Hans-Peter Kriegel, Jorg Sander, Xiaowei Xu (1996). "A
	   // density-based algorithm for discovering clusters in large spatial databases
	   // with noise".  Proceedings of the Second International Conference on Knowledge
	   // Discovery and Data Mining (KDD-96). AAAI Press. pp. 226-231.  
	   // Also see: http://en.wikipedia.org/wiki/DBSCAN

	   // Copyright (c) 2013 Peter Kovesi
	   // www.peterkovesi.com/matlabfns/
	    
	   // Permission is hereby granted, free of charge, to any person obtaining a copy
	   // of this software and associated documentation files (the "Software"), to deal
	   // in the Software without restriction, subject to the following conditions:
	    
	   // The above copyright notice and this permission notice shall be included in 
	   // all copies or substantial portions of the Software.
	   
	   // The Software is provided "as is", without warranty of any kind.

	   // ** To Do: Make the distance measure incorporate some measure of any edge
	   // continuity that adjacent superpixels might have **
	   // ** Allow attributes other than Lab colour to be used **

	   // March 2013
	   // July  2013  Changes to accommondate superpixel attributes being passed as a
	   //             struct array
	   
	   int Np = Sp.N.length;
	   
	   int regionsC[] = new int[Np+1];
	   Vector<Vector<Integer>> Cl = new Vector<Vector<Integer>>();
	   Vector<Integer> intVec;
	   int Nc = 0; //Cluster counter
	   // Array to keep track of superpixels that have been visited
	   boolean Pvisit[] = new boolean[Np+1];
	   Vector<Integer> neighbours;
	   Vector<Integer> neighboursP;
	   int ind;
	   int nb;
	   
	   for (n = 1; n <= Np; n++) {
		   if (!Pvisit[n]) { // If this superpixel not visited yet
			   Pvisit[n] = true; // mark it as visited
			   neighbours = regionQueryM(Sp, amal, n-1, Ec);
			   
			   // Form a cluster
			   // Increment number of clusters and process nieghbourhood
			   Nc = Nc + 1;
			   intVec = new Vector<Integer>();
			   // Initialize cluster Nc with point n
			   intVec.add(n);
			   // and mark superpixel n as being a member of the cluster Nc.
			   regionsC[n] = Nc;
			   
			   // Initialize index into neighbours vector
			   ind = 0;
			   
			   // For each superpixel Sp in list of neighbours
			   while (ind < neighbours.size()) {
		           nb = neighbours.get(ind);
		           
		           if (!Pvisit[nb]) { // If this neighbour has not been visited
		        	   Pvisit[nb] = true; // mark it as visited
		        	   
		        	   // Find the neighbours of this neighbour and
		        	   // add them to the neighbours list
		        	   neighboursP = regionQueryM(Sp, amal, nb-1, Ec);
		        	   neighbours.addAll(neighboursP);
		           } // if (!Pvisit[nb])
		           
		           // If this neighbour nb is not yet a member of any cluster add it
		           // to this cluster
		           if (regionsC[nb] == 0) {
		        	   intVec.add(nb);
		        	   regionsC[nb] = Nc;
		           }
		           
		           // Increment neighbour point index and process next neighbour
		           ind = ind + 1;
			   } // while (ind < neighbours.size())
			   Cl.add(intVec);
		   } // if (!Pvisit[n])
	   } // for (n = 1; n <= Np; n++)
	   
	   // Generate new labeled image corresponding to the new clustered regions
	   int lc[] = new int[length];
	   for (n = 1; n < regionsC.length; n++) {
		   for (y = 0; y < yDim; y++) {
			   for (x = 0; x < xDim; x++) {
				   if (l[y][x] == n) {
					   index = x + y*xDim;
					   lc[index] = regionsC[n];
				   }
			   }
		   }
	   }
	   
	   try {
		   destImage.importData(0, lc, true);
	   }
	   catch (IOException e) {
		   System.err.println("IOException " + e + " on destImage.importData(0, lc, true)");
		   setCompleted(false);
		   return;
	   }
	   
	   setCompleted(true);
	   return;
		
    }
	
	private Vector<Integer> regionQueryM(SP Sp, AmAl amal, int n, double Ec) {
		// Find indices of all superpixels adjacent to superpixel n with mean Lab 
		// colour difference less than Ec.
		
		// Arguments:
		//             Sp - The struct array of superpixel attributes
		//             Am - Adjacency matrix
		//              n - Index of point of interest
		//             Ec - Colour distance threshold
		
		int i,j;
		double E2 = Ec*Ec;
		Vector<Integer>neighbours = new Vector<Integer>();
		
		// Get indices of all pixels connected to superpixel connected to superpixel n
		Vector<Integer> ind = new Vector<Integer>();
		if (amal.Ami != null) {
			for (i = 0; i < amal.Ami.length; i++) {
		        if (amal.Ami[i] == n) {
		        	ind.add(amal.Amj[i]);
		        }
			}
			
			for (j = 0; j < ind.size(); j++) {
				i = ind.get(j);
				// Test if distance^2 < E^2
				double Ldiff = Sp.L[i] - Sp.L[n];
				double adiff = Sp.a[i] - Sp.a[n];
				double bdiff = Sp.b[i] - Sp.b[n];
				double dist2 = Ldiff*Ldiff + adiff*adiff + bdiff*bdiff;
				if (dist2 < E2) {
					neighbours.add(i+1);
				}
			}
		}
		return neighbours;
	}
	
	class SP {
		public SP(int n) {
		    L = new double[n];	
		    a = new double[n];
		    b = new double[n];
		    stdL = new double[n];
		    stda = new double[n];
		    stdb = new double[n];
		    r = new double[n];
		    c = new double[n];
		    N = new int[n];
		}
		double L[];
		double a[];
		double b[];
		double stdL[];
		double stda[];
		double stdb[];
		double r[];
		double c[];
		int N[];
	}
	
	private AmAl mcleanupregions(int seg[][], double seRadius) {
		// MCLEANUPREGIONS  Morphological clean up of small segments in an image of segmented regions
		
		// Usage: [seg, Am] = mcleanupregions(seg, seRadius)
		
		// Arguments: seg - A region segmented image, such as might be produced by a
		//                  graph cut algorithm.  All pixels in each region are labeled
		//                  by an integer.
		//       seRadius - Structuring element radius.  This can be set to 0 in which
		//                  case  the function will simply ensure all labeled regions
		//                  are distinct and relabel them if necessary. 
		
		// Returns:   seg - The updated segment image.
		//             Am - Adjacency matrix of segments.  Am(i, j) indicates whether
		//                  segments labeled i and j are connected/adjacent
		
		// Typical application:
		// If a graph cut or superpixel algorithm fails to converge stray segments
		// can be left in the result.  This function tries to clean things up by:
		// 1) Checking there is only one region for each segment label. If there is
		//    more than one region they are given unique labels.
		// 2) Eliminating regions below the structuring element size
		
		// Note that regions labeled 0 are treated as a 'privileged' background region
		// and is not processed/affected by the function.
		
		// See also: REGIONADJACENCY, RENUMBERREGIONS, CLEANUPREGIONS, MAKEREGIONSDISTINCT

		// Copyright (c) 2013 Peter Kovesi
		// www.peterkovesi.com/matlabfns/
		
		// Permission is hereby granted, free of charge, to any person obtaining a copy
		// of this software and associated documentation files (the "Software"), to deal
		// in the Software without restriction, subject to the following conditions:
		 
		// The above copyright notice and this permission notice shall be included in 
		// all copies or substantial portions of the Software.
		
		// The Software is provided "as is", without warranty of any kind.
		
		// March   2013 
		// June    2013  Improved morphological cleanup process using distance map

		// function [seg, Am, mask] = mcleanupregions(seg, seRadius)
		
		int maxlabel;
		int yDim = seg.length;
		int xDim = seg[0].length;
		int length = xDim * yDim;
		int l;
		int x,y,index,m,n;
		int y2,x2;
		byte b[][];
		byte mask[][];
		byte bopen[];
		ModelImage bImage;
		int extents[];
		int option = 2;
		AlgorithmMorphology2D algoMorph2D;
		int iterDilate = 1;
		int iterErode = 1;
		int numPruningPixels = 0;
		boolean entireImage = true;
		Vector<Vector<Integer>> list;
		// 1) Ensure every segment is distinct 	
		maxlabel = makeregionsdistinct(seg,4);
		
		// 2) Perform a morphological opening on each segment, subtract the opening
	    // from the orignal segment to obtain regions to be reassigned to
	    // neighbouring segments.
	    if (seRadius > 0) {
	    	// Accurate and not noticeably slower
            // if radius is small
	        if (seRadius < 1.0) {
				System.err.println("seRadius must be >= 1.0");
				System.exit(0);
			}
			
			// Diameter of structuring element
			int dia = (int)Math.ceil(2.0*seRadius);
			
			// If diameter is an even value, add 1 to generate a center pixel
			if ((dia % 2) == 0) {
				dia = dia + 1;
			}
	        mask = new byte[yDim][xDim];
	        b = new byte[yDim][xDim];
	        bopen = new byte[length];
	        extents = new int[] {xDim,yDim};
	        bImage = new ModelImage(ModelStorageBase.BYTE, extents, "bImage");
	        
	        if (option == 1) {
	        	for (l = 1; l <= maxlabel; l++) {
	        	    for (y = 0; y < yDim; y++) {
	        	    	for (x = 0; x < xDim; x++) {
	        	    		index = x + y * xDim;
	        	    	    if (seg[y][x] == l) {
	        	    	    	b[y][x] = 1;
	        	    	    	bopen[index] = 1;
	        	    	    }
	        	    	    else {
	        	    	    	b[y][x] = 0;
	        	    	    	bopen[index] = 0;
	        	    	    }
	        	    	}
	        	    } // for (y = 0; y < yDim; y++)
	        	    try {
	        	    	bImage.importData(0, bopen, true);
	        	    }
	        	    catch (IOException e) {
	        	    	System.err.println("IOException " + e + " on bImage.importData(0, bopen, true)");
	        	    	System.exit(0);
	        	    }
	        	    algoMorph2D = new AlgorithmMorphology2D(bImage,AlgorithmMorphology2D.SIZED_CIRCLE,(float)dia,
	        	    		AlgorithmMorphology2D.OPEN,iterDilate,iterErode,numPruningPixels,AlgorithmMorphology2D.INNER_EDGING,
	        	    		entireImage);
	        	    algoMorph2D.run();
	        	    try {
	        	    	bImage.exportData(0, length, bopen);
	        	    }
	        	    catch (IOException e) {
	        	    	System.err.println("IOException " + e + " on bImage.exportData(0, length, bopen)");
	        	    	System.exit(0);
	        	    }
	        	    algoMorph2D.finalize();
	        	    for (y = 0; y < yDim; y++) {
	        	    	for (x = 0; x < xDim; x++) {
	        	    		index = x + y*xDim;
	        	    		if ((b[y][x] == 1) && (bopen[index] == 0)) {
	        	    			mask[y][x] = 1;
	        	    		}
	        	    	}
	        	    }
	        	} // for (l = 1; l <= maxlabel; l++)
	        } // if (option == 1)
	        else { // option == 2
	        	// Rather than perform a morphological opening on every
	            // individual region in sequence the following finds separate
	            // lists of unconnected regions and performs openings on these.
	            // Typically an image can be covered with only 5 or 6 lists of
	            // unconnected regions.  Seems to be about 2X speed of option
	            // 1. (I was hoping for more...)
	            list = finddisconnected(seg);	
	            
	            for (n = 0; n < list.size(); n++) {
	                for (y = 0; y < yDim; y++) {
	                	for (x = 0; x < xDim; x++) {
	                		index = x + y * xDim;
	                		b[y][x] = 0;
	                		bopen[index] = 0;
	                	}
	                }
	                for (m = 0; m < list.get(n).size(); m++) {
	                	for (y = 0; y < yDim; y++) {
		        	    	for (x = 0; x < xDim; x++) {
		        	    		index = x + y * xDim;
		        	    	    if (seg[y][x] == list.get(n).get(m)) {
		        	    	    	b[y][x] = 1;
		        	    	    	bopen[index] = 1;
		        	    	    }
		        	    	   
		        	    	}
		        	    } // for (y = 0; y < yDim; y++)	
	                } // for (m = 0; m < list.get(n).size(); m++)
	                try {
	        	    	bImage.importData(0, bopen, true);
	        	    }
	        	    catch (IOException e) {
	        	    	System.err.println("IOException " + e + " on bImage.importData(0, bopen, true)");
	        	    	System.exit(0);
	        	    }
	        	    algoMorph2D = new AlgorithmMorphology2D(bImage,AlgorithmMorphology2D.SIZED_CIRCLE,(float)dia,
	        	    		AlgorithmMorphology2D.OPEN,iterDilate,iterErode,numPruningPixels,AlgorithmMorphology2D.INNER_EDGING,
	        	    		entireImage);
	        	    algoMorph2D.run();
	        	    try {
	        	    	bImage.exportData(0, length, bopen);
	        	    }
	        	    catch (IOException e) {
	        	    	System.err.println("IOException " + e + " on bImage.exportData(0, length, bopen)");
	        	    	System.exit(0);
	        	    }
	        	    algoMorph2D.finalize();
	        	    for (y = 0; y < yDim; y++) {
	        	    	for (x = 0; x < xDim; x++) {
	        	    		index = x + y*xDim;
	        	    		if ((b[y][x] == 1) && (bopen[index] == 0)) {
	        	    			mask[y][x] = 1;
	        	    		}
	        	    	}
	        	    }
	            } // for (n = 0; n < list.size(); n++)
	        } // else option == 2
	        
	        // Compute distance map on inverse of map
	        byte invmask[][] = new byte[yDim][xDim];
	        int idxy[][] = new int[yDim][xDim];
	        int idxx[][] = new int[yDim][xDim];
	        for (y = 0; y < yDim; y++) {
	        	for (x = 0; x < xDim; x++) {
	        		if (mask[y][x] == 0) {
	        			invmask[y][x] = 1;
	        		}
	        	}
	        }
	        
	        double distanceSquared;
	        double currentDistSquared;
	        double diffx;
	        double diffy;
	        for (y = 0; y < yDim; y++) {
	        	for (x = 0; x < xDim; x++) {
	        		if (invmask[y][x] == 1) {
	        			idxy[y][x] = y;
	        			idxx[y][x] = x;
	        		}
	        		else {
	        			distanceSquared = Double.MAX_VALUE;
	        			for (y2 = 0; y2 < yDim; y2++) {
	        				for (x2 = 0; x2 < xDim; x2++) {
	        					if (invmask[y2][x2] == 1) {
	        						diffx = x - x2;
	        						diffy = y - y2;
	        						currentDistSquared = diffx * diffx + diffy * diffy;
	        						if (currentDistSquared < distanceSquared) {
	        							distanceSquared = currentDistSquared;
	        							idxy[y][x] = y2;
	        							idxx[y][x] = x2;
	        						}
	        					}
	        				}
	        			}
	        		}
	        	}
	        } // for (y = 0; y < yDim; y++)
	        
	        // Assign a label to every pixel in the masked area using the label of
	        // the closest pixel not in the mask as computed by bwdist
	        int seg2[][] = new int[yDim][xDim];
	        for (y = 0; y < yDim; y++) {
	        	for (x = 0; x < xDim; x++) {
	        	    seg2[y][x] = seg[y][x];	
	        	}
	        }
	        
	        for (y = 0; y < yDim; y++) {
	        	for (x = 0; x < xDim; x++) {
	        		if (mask[y][x] == 1) {
	        			seg[y][x] = seg2[idxy[y][x]][idxx[y][x]];
	        		}
	        	}
	        }
	    } // if (seRadius > 0)
		
	    // 3) As some regions will have been relabled, possibly broken into several
	    // parts, or absorbed into others and no longer exist we ensure all regions
	    // are distinct again, and renumber the regions so that they sequentially
	    // increase from 1.  We also need to reconstruct the adjacency matrix to
	    // reflect the changed number of regions and their relabeling.

	    int maxLabel = makeregionsdistinct(seg,4);
	    //[seg, minLabel, maxLabel] = renumberregions(seg);
	    Renum re = renumberregions(seg);
	    for (y = 0; y < yDim; y++) {
	    	for (x = 0; x < xDim; x++) {
	    		seg[y][x] = re.nL[y][x];
	    	}
	    }
	    AmAl amal = regionadjacency(seg,8);
	    return amal;
	}
	
	private Renum renumberregions(int L[][]) {
		// Usage: [nL, minLabel, maxLabel] = renumberregions(L)
		
		// Argument:   L - A labeled image segmenting an image into regions, such as
		//                 might be produced by a graph cut or superpixel algorithm.
		//                 All pixels in each region are labeled by an integer.
		
		// Returns:   nL - A relabeled version of L so that label numbers form a
		//                 sequence 1:maxRegions  or 0:maxRegions-1 depending on
		//                 whether L has a region labeled with 0s or not.
		//      minLabel - Minimum label in the renumbered image.  This will be 0 or 1.
		//      maxLabel - Maximum label in the renumbered image.
		
		// Application: Segmentation algorithms can produce a labeled image with a non
		// contiguous numbering of regions 1 4 6 etc. This function renumbers them into a
		// contiguous sequence.  If the input image has a region labeled with 0s this
		// region is treated as a privileged 'background region' and retains its 0
		// labeling. The resulting image will have labels ranging over 0:maxRegions-1.
		// Otherwise the image will be relabeled over the sequence 1:maxRegions
		
		// See also: CLEANUPREGIONS, REGIONADJACENCY

		// Copyright (c) 2010 Peter Kovesi
		// www.peterkovesi.com/matlabfns/
		
		// Permission is hereby granted, free of charge, to any person obtaining a copy
		// of this software and associated documentation files (the "Software"), to deal
		// in the Software without restriction, subject to the following conditions:
		 
		// The above copyright notice and this permission notice shall be included in 
		// all copies or substantial portions of the Software.
		
		// October  2010
		// February 2013 Return label numbering range
		int x,y,i,index,n;
	    Renum re = new Renum();
	    re.nL = new int[L.length][L[0].length];
	    for (y = 0; y < L.length; y++) {
	    	for (x = 0; x < L[0].length; x++) {
	    		re.nL[y][x] = L[y][x];
	    	}
	    }
	    
	    // Sorted list of unique labels
	    int length = L.length * L[0].length;
	    int Larray[] = new int[length];
	    for (y = 0; y < L.length; y++) {
	    	for (x = 0; x < L[0].length; x++) {
	    		index = x + y * L[0].length;
	    		Larray[index] = L[y][x];
	    	}
	    }
	    Arrays.sort(Larray);
	    int N = 1;
	    for (i = 1; i < length; i++) {
	    	if (Larray[i] > Larray[i-1]) {
	    		N++;
	    	}
	    }
	    int labels[] = new int[N];
	    labels[0] = Larray[0];
	    for (i = 1,index = 1; i < length; i++) {
	    	if (Larray[i] > Larray[i-1]) {
	    		labels[index++] = Larray[i];
	    	}
	    }
	    
	    // If there is a label of 0 we ensure that we do not renumber that region
	    // by removing it from the list of labels to be renumbered.
	    if (labels[0] == 0) {
	    	int tempLabels[] = new int[N-1];
	    	for (i = 1; i < N; i++) {
	    		tempLabels[i-1] = labels[i]; 
	    	}
	    	labels = new int[N-1];
	    	for (i = 0; i < N-1; i++) {
	    		labels[i] = tempLabels[i];
	    	}
	    	tempLabels = null;
	    	re.minLabel = 0;
	    	re.maxLabel = N-1;
	    }
	    else {
	    	re.minLabel = 1;
	    	re.maxLabel = N;
	    }
	    
	    // Now do the relabelling
	    int count = 1;
	    for (i = 0; i < labels.length; i++) {
	        n = labels[i];
	        for (y = 0; y < L.length; y++) {
	        	for (x = 0; x < L[0].length; x++) {
	        		if (L[y][x] == n) {
	        			re.nL[y][x] = count;
	        		}
	        	}
	        }
	        count = count + 1;
	    }
	    return re;
	}
	
	class Renum {
		public Renum() {
			
		}
		int nL[][];
		int minLabel;
		int maxLabel;
	}
	
	private Vector<Vector<Integer>> finddisconnected(int l[][]) {
		// FINDDISCONNECTED find groupings of disconnected labeled regions
		
		// Usage: list = finddisconnected(l)
		
		// Argument:   l - A labeled image segmenting an image into regions, such as
		//                 might be produced by a graph cut or superpixel algorithm.
		//                 All pixels in each region are labeled by an integer.
		
		// Returns: list - A cell array of lists of regions that are not
		//                 connected. Typically there are 5 to 6 lists.
		
		// Used by MCLEANUPREGIONS to reduce the number of morphological closing
		// operations 
		
		// See also: MCLEANUPREGIONS, REGIONADJACENCY

		// Copyright (c) 2013 Peter Kovesi
		// www.peterkovesi.com/matlabfns/
	
		// Permission is hereby granted, free of charge, to any person obtaining a copy
		// of this software and associated documentation files (the "Software"), to deal
		// in the Software without restriction, subject to the following conditions:
		
		// The above copyright notice and this permission notice shall be included in 
		// all copies or substantial portions of the Software.
		
		// The Software is provided "as is", without warranty of any kind.

		// PK July 2013
        boolean debug = false;
        AmAl amal = regionadjacency(l,8);
        int yDim = l.length;
        int xDim = l[0].length;
        int x,y,n,i,j,m,p;
        // Number of labels
        int N = 0;
        for (y = 0; y < yDim; y++) {
        	for (x = 0; x < xDim; x++) {
        		if (l[y][x] > N) {
        			N = l[y][x];
        		}
        	}
        }
        
        // Array for keeping track of visisted labels
        boolean visited[] = new boolean[N+1];
        Vector<Vector<Integer>>list = new Vector<Vector<Integer>>();
        Vector<Integer>notConnected = new Vector<Integer>();
        int alm;
        int intp;
        boolean found = false;
        for (n = 1; n <= N; n++) {
            if (!visited[n]) {
                Vector<Integer> intVec = new Vector<Integer>();
                intVec.add(n);
                visited[n] = true;
                
                // Find all regions not directly connected to n and not visited
                notConnected.clear();
                for (i = 1; i < N; i++) {
                	notConnected.add(i);
                }
                if (amal.Ami != null) {
	                for (i = 0; i < amal.Ami.length; i++) {
	                	if (amal.Ami[i] == n) {
	                		notConnected.removeElement(amal.Amj[i]);
	                	}
	                }
                }
                for (i = 1; i <= N; i++) {
                	if (visited[i]) {
                		notConnected.removeElement(i);
                	}
                }
                
                // For each unconnected region check that it is not already 
                // connected to a region in the list.  If not, add to list.
                for (i = 0; i < notConnected.size(); i++) {
                	m = notConnected.get(i);
                	if (amal.Al != null) {
                		if (amal.Al[m] != null) {
                			found = false;
                			for (j = 0; j < amal.Al[m].length && (!found); j++) {
                			    alm = amal.Al[m][j];
                			    for (p = 0; p < intVec.size() && (!found); p++) {
                			        intp = intVec.get(p);
                			        if (alm == intp) {
                			        	found = true;
                			        }
                			    }
                			}
                			if (!found) {
                				intVec.add(m);
                				visited[m] = true;
                			}
                		} // if (amal.Al[m] != null)
                	} // if (amal.Al != null)
                } // for (i = 0; i < notConnected.size(); i++)
                list.add(intVec);
            } // if (!visited[n])
        } // for (n = 1; n <= N; n++)
        return list;
	}
	
	private AmAl regionadjacency(int L[][], int connectivity) {
		// REGIONADJACENCY Computes adjacency matrix for image of labeled segmented regions
		
		// Usage:  [Am, Al] = regionadjacency(L, connectivity)
		
		// Arguments:  L - A region segmented image, such as might be produced by a
		//                 graph cut or superpixel algorithm.  All pixels in each
		//                 region are labeled by an integer.
		//  connectivity - 8 or 4.  If not specified connectivity defaults to 8.
		
		// Returns:   Am - An adjacency matrix indicating which labeled regions are
		//                 adjacent to each other, that is, they share boundaries. Am
		//                 is sparse to save memory.
		//            Al - A cell array representing the adjacency list corresponding
		//                 to Am.  Al{n} is an array of the region indices adjacent to
		//                 region n.
		
		// Regions with a label of 0 are not processed. They are considered to be
		// 'background regions' that are not to be considered.  If you want to include
		// these regions you should assign a new positive label to these areas using, say
		// >> L(L==0) = max(L(:)) + 1;
		
		// See also: CLEANUPREGIONS, RENUMBERREGIONS, SLIC

		// Copyright (c) 2013 Peter Kovesi
		// www.peterkovesi.com/matlabfns/
		
		// Permission is hereby granted, free of charge, to any person obtaining a copy
		// of this software and associated documentation files (the "Software"), to deal
		// in the Software without restriction, subject to the following conditions:
	
		// The above copyright notice and this permission notice shall be included in 
		// all copies or substantial portions of the Software.
		
		// February 2013  Original version
		// July     2013  Speed improvement in sparse matrix formation (4x)

	    // function  [Am, varargout] = regionadjacency(L, connectivity)

		//if ~exist('connectivity', 'var'), connectivity = 8; end
		if ((connectivity != 4) && (connectivity != 8)) {
			System.err.println("In regionadjacency connectivity =" + connectivity + " instead of the required 4 or 8");
			System.exit(0);
		}
		int x,y,index,ii;
		int yDim = L.length;
		int xDim = L[0].length;
		int length = xDim * yDim;
		int LArray[] = new int[length];
		int N;
		int n,r,c;
		
		// Identify the unique labels in the image, excluding 0 as a label.
		for (y = 0; y < yDim; y++) {
			for (x = 0; x < xDim; x++) {
				index = x + y * xDim;
				LArray[index] = L[y][x];
			}
		}
		Arrays.sort(LArray);
		int numUnique = 1;
        for (ii = 1; ii < length; ii++) {
        	if (LArray[ii] > LArray[ii-1]) {
        		numUnique++;
        	}
        }
        int labels[] = new int[numUnique];
        labels[0] = LArray[0];
        for (ii = 1, index = 1; ii < length; ii++) {
        	if (LArray[ii] > LArray[ii-1]) {
        		labels[index++] = LArray[ii];
        	}
        }
        // Required size of adjacency matrix
        N = labels[labels.length-1];
        // Remove 0 from label list
        boolean hasZero = false;
        for (ii = 0; ii < labels.length && (!hasZero); ii++) {
        	if (labels[ii] == 0) {
        		hasZero = true;
        	}
        }
        if (hasZero) {
        	int tempLabel[] = new int[labels.length-1];
        	for (ii = 0, index = 0; ii < labels.length; ii++) {
        		if (labels[ii] != 0) {
        			tempLabel[index++] = labels[ii];
        		}
        	}
        	labels = new int[tempLabel.length];
        	for (ii = 0; ii < labels.length; ii++) {
        		labels[ii] = tempLabel[ii];
        	}
        	tempLabel = null;
        } // if (hasZero)
        
        if (labels.length == 0) {
        	System.out.println("Warning! There are no objects in the image");
        	AmAl amal = new AmAl();
        	return amal;
        }
        
        // Strategy:  Step through the labeled image.  For 8-connectedness inspect 
        // pixels as follows and set the appropriate entries in the adjacency
        // matrix. 
        //      x - o
        //    / | \
        //  o   o   o
        
        // For 4-connectedness we only inspect the following pixels
        //      x - o
        //      | 
        //      o  
        
        // Because the adjacency search looks 'forwards' a final OR operation is
        // performed on the adjacency matrix and its transpose to ensure
        // connectivity both ways.

        // Allocate vectors for forming row, col, value triplets used to construct
        // sparse matrix.  Forming these vectors first is faster than filling
        // entries directly into the sparse matrix
        int i[] = null; // row value
        int j[] = null; // col value
        int s[] = null; // value
        int lencon = 0;
        
        if (connectivity == 8) {
        	lencon = 3*(yDim-1) + 4*(yDim-1)*(xDim-2);
        	i = new int[lencon];
        	j = new int[lencon];
        	s = new int[lencon];
            n = 0;
            for (r = 0; r < yDim-1; r++) {

                // Handle pixels in 1st column
                i[n] = L[r][0]; j[n] = L[r][1]; s[n] = 1; n=n+1;
                i[n] = L[r][0]; j[n] = L[r+1][0]; s[n] = 1; n=n+1;
                i[n] = L[r][0]; j[n] = L[r+1][1]; s[n] = 1; n=n+1;
                
                // ... now the rest of the column
                for (c = 1; c < xDim-1; c++) {
                   i[n] = L[r][c]; j[n] = L[r][c+1]; s[n] = 1; n=n+1;
                   i[n] = L[r][c]; j[n] = L[r+1][c-1]; s[n] = 1; n=n+1;
                   i[n] = L[r][c]; j[n] = L[r+1][c]; s[n] = 1; n=n+1;
                   i[n] = L[r][c]; j[n] = L[r+1][c+1]; s[n] = 1; n=n+1;
                }
            }
        } // if (connectivity == 8)
            
        else if (connectivity == 4) {
            n = 0;
            lencon = 2*(xDim-1)*(yDim-1);
            i = new int[lencon];
        	j = new int[lencon];
        	s = new int[lencon];
            for (r = 0; r < yDim-1; r++) {
                for (c = 0; c < xDim-1; c++) {
                    i[n] = L[r][c]; j[n] = L[r][c+1]; s[n] = 1; n=n+1;
                    i[n] = L[r][c]; j[n] = L[r+1][c]; s[n] = 1; n=n+1;
                }
            }
        
        } // else if (connectivity == 4) 
        
       int numAmEntries = 0;
       Vector<Integer> Ami = new Vector<Integer>();
       Vector<Integer> Amj = new Vector<Integer>();
       boolean haveEntry;
       boolean haveReverseEntry;
       for (r = 0; r < lencon; r++) {
    	   if ((i[r] != 0) && (j[r] != 0)) {
    		   if (i[r] != j[r]) {
    			   haveEntry = false;
    			   haveReverseEntry = false;
    			   for (c = 0; c < numAmEntries; c++) {
    				   if ((Ami.get(c) == i[r]) && (Ami.get(c) == j[r])) {
    					    haveEntry = true;   
    				   }
    				   if ((Ami.get(c) == j[r]) && (Ami.get(c) == i[r])) {
   					       haveReverseEntry = true;   
   				       }
    			   }
    			   if (!haveEntry) {
    				   Ami.add(i[r]);
    				   Amj.add(j[r]);
    				   numAmEntries++;
    			   }
    			   if (!haveReverseEntry) {
    				   Ami.add(j[r]);
    				   Amj.add(i[r]);
    				   numAmEntries++;
    			   }

    		   }
    	   }
       }
       
      
       AmAl amal = new AmAl();
       amal.Ami = new int[numAmEntries];
       amal.Amj = new int[numAmEntries];
       int entry = 0;
       for (r = 0; r < numAmEntries; r++) {
    	   amal.Ami[r] = Ami.get(r);
    	   amal.Amj[r] = Amj.get(r);
       }
       
       amal.Al = new int[N][];
       int numAlEntries;
       for (r = 0; r < N; r++) {
    	   numAlEntries = 0;
    	   for (c = 0; c < numAmEntries; c++) {
    	       if (amal.Ami[c] == r) {
    	    	   numAlEntries++;
    	       }
    	   }
    	   amal.Al[r] = new int[numAlEntries];
    	   entry = 0;
    	   for (c = 0; c < numAmEntries; c++) {
    		   if (amal.Ami[c] == r) {
    		       amal.Al[r][entry++] = amal.Amj[c];   
    		   }
    	   }
       }
       return amal;
	}
    
	
	class AmAl {
		public AmAl() {
			
		}
		int Ami[];
		int Amj[];
		int Al[][];
	}
	
	private int makeregionsdistinct(int seg[][], int connectivity) {
		// MAKEREGIONSDISTINCT Ensures labeled segments are distinct
		
		// Usage: [seg, maxlabel] = makeregionsdistinct(seg, connectivity)
		
		// Arguments: seg - A region segmented image, such as might be produced by a
		//                  superpixel or graph cut algorithm.  All pixels in each
		//                  region are labeled by an integer.
		//   connectivity - Optional parameter indicating whether 4 or 8 connectedness
		//                  should be used.  Defaults to 4.
		
		// Returns:   seg - A labeled image where all segments are distinct.
		//       maxlabel - Maximum segment label number.
		
		// Typical application: A graphcut or superpixel algorithm may terminate in a few
		// cases with multiple regions that have the same label.  This function
		// identifies these regions and assigns a unique label to them.
		
		// See also: SLIC, CLEANUPREGIONS, RENUMBERREGIONS

		// Copyright (c) 2013 Peter Kovesi
		// www.peterkovesi.com/matlabfns/
	 
		// Permission is hereby granted, free of charge, to any person obtaining a copy
		// of this software and associated documentation files (the "Software"), to deal
		// in the Software without restriction, subject to the following conditions:
		 
		// The above copyright notice and this permission notice shall be included in 
		// all copies or substantial portions of the Software.
		
		// The Software is provided "as is", without warranty of any kind.

		// June 2013

		// if ~exist('connectivity', 'var'), connectivity = 4; end
	    
	    // Ensure every segment is distinct but do not touch segments 
	    // with a label of 0
		int x,y,i;
		int maxlabel;
		int l;
	    //labels = unique(seg(:))';
		int yDim = seg.length;
		int xDim = seg[0].length;
		int length = xDim*yDim;
		int segArray[] = new int[length];
		int index;
		int num = 0;
		int n;
		for (y = 0; y < yDim; y++) {
			for (x = 0; x < xDim; x++) {
				index = x + y * xDim;
				segArray[index] = seg[y][x];
			}
		}
		Arrays.sort(segArray);
		int numUnique = 1;
        for (i = 1; i < length; i++) {
        	if (segArray[i] > segArray[i-1]) {
        		numUnique++;
        	}
        }
        int labels[] = new int[numUnique];
        labels[0] = segArray[0];
        for (i = 1, index = 1; i < length; i++) {
        	if (segArray[i] > segArray[i-1]) {
        		labels[index++] = segArray[i];
        	}
        }
        maxlabel = labels[labels.length-1];
        // Remove 0 from label list
        boolean hasZero = false;
        for (i = 0; i < labels.length && (!hasZero); i++) {
        	if (labels[i] == 0) {
        		hasZero = true;
        	}
        }
        if (hasZero) {
        	int tempLabel[] = new int[labels.length-1];
        	for (i = 0, index = 0; i < labels.length; i++) {
        		if (labels[i] != 0) {
        			tempLabel[index++] = labels[i];
        		}
        	}
        	labels = new int[tempLabel.length];
        	for (i = 0; i < labels.length; i++) {
        		labels[i] = tempLabel[i];
        	}
        	tempLabel = null;
        } // if (hasZero)
        
        int bl[][] = new int[yDim][xDim];
        for (i = 0; i < labels.length; i++) {
            l = labels[i];
            for (y = 0; y < yDim; y++) {
            	for (x = 0; x < xDim; x++) {
            		if (seg[y][x] == l) {
            			bl[y][x] = -1;
            		}
            		else {
            			bl[y][x] = 0;
            		}
            	}
            }
            if (connectivity == 4) {
                num = bwlabel4(bl);	
            }
            if (num > 1) {
            	// We have more than 1 region with the same label
            	for (n = 2; n <= num; n++) {
            	    // Generate a new label
            		maxlabel = maxlabel + 1;
            		// And assign to this segment
            		for (y = 0; y < yDim; y++) {
            			for (x = 0; x < xDim; x++) {
            				if (bl[y][x] == n) {
            				    seg[y][x] = maxlabel;	
            				}
            			}
            		}
            	} // for (n = 2; n <= num; n++)
            } // if (num > 1)
        } // for (i = 0; i < labels.length; i++)
        return maxlabel;
	}
	
	private int bwlabel4(int bl[][]) {
		// All values must initially be zeros and -1
		int yDim = bl.length;
		int xDim = bl[0].length;
		int newLabel = 0;
		int x,y;
		int x2,y2;
		int minY;
		int maxY;
		int minX;
		int maxX;
		int nextMinY;
		int nextMaxY;
		int nextMinX;
		int nextMaxX;
		boolean setLabel = false;
		for (y = 0; y < yDim; y++) {
			for (x = 0; x < xDim; x++) {
				if (bl[y][x] == -1) {
					newLabel++;
					bl[y][x] = newLabel;
					boolean changed = true;
					minY = Math.max(0,y-1);
					nextMinY = minY;
					maxY = Math.min(yDim-1,y+1);
					nextMaxY = maxY;
					minX = Math.max(0,x-1);
					nextMinX = minX;
					maxX = Math.min(xDim-1,x+1);
					nextMaxX = maxX;
					while (changed) {
					    changed = false;
					    for (y2 = minY; y2 <= maxY; y2++) {
					    	for (x2 = minX; x2 <= maxX; x2++) {
					    		setLabel = false;
					    		if (bl[y2][x2] == -1) {
					    			if (y2 > minY) {
					    				if (bl[y2-1][x2] == newLabel) {
					    					bl[y2][x2] = newLabel;
					    					setLabel = true;
					    					changed = true;
					    					if ((y2 == maxY) && (maxY < yDim-1)) {
					    						nextMaxY = maxY+1;
					    					}
					    				}
					    			}
					    			if (!setLabel) {
					    				if (y2 < maxY) {
					    					if (bl[y2+1][x2] == newLabel) {
					    						bl[y2][x2] = newLabel;
					    						setLabel = true;
					    						changed = true;
					    						if ((y2 == minY) && (minY > 0)) {
					    							nextMinY = minY-1;
					    						}
					    					}
					    				}
					    			}
					    			if (!setLabel) {
					    				if (x2 > minX) {
					    					if (bl[y2][x2-1] == newLabel) {
					    						bl[y2][x2] = newLabel;
					    						setLabel = true;
					    						changed = true;
					    						if ((x2 == maxX) && (maxX < xDim-1)) {
					    							nextMaxX = maxX + 1;
					    						}
					    					}
					    				}
					    			}
					    			if (!setLabel) {
					    				if (x2 < maxX) {
					    					if (bl[y2][x2+1] == newLabel) {
					    						bl[y2][x2] = newLabel;
					    						changed = true;
					    						if ((x2 == minX) && (minX > 0)) {
					    							nextMinX = minX-1;
					    						}
					    					}
					    				}
					    			}
					    		} // if (bl[y2][x2] == -1)
					    	}
					    }
					    if (changed) {
					    	minX = nextMinX;
					    	maxX = nextMaxX;
					    	minY = nextMinY;
					    	maxY = nextMaxY;
					    }
					} // while (changed)
				} 
			}
		}
		return newLabel;
	}
	
	private double[][] dist(double C[], double im[][][], int r1, int c1, double S, double m) {
		// Arguments:   C - Cluster being considered
		//             im - sub-image surrounding cluster centre
		//         r1, c1 - row and column of top left corner of sub image within the
		//                  overall image.
		//              S - grid spacing
		//              m - weighting factor between colour and spatial differences.
		
		// Returns:     D - Distance image giving distance of every pixel in the
		//                  subimage from the cluster centre
		
		// Distance = sqrt( dc^2 + (ds/S)^2*m^2 )
		// where:
		// dc = sqrt(dl^2 + da^2 + db^2)  % Colour distance
		// ds = sqrt(dx^2 + dy^2)         % Spatial distance
		
		// m is a weighting factor representing the nominal maximum colour distance
		// expected so that one can rank colour similarity relative to distance
		// similarity.  try m in the range [1-40] for L*a*b* space
		
		// ?? Might be worth trying the Geometric Mean instead ??
		//  Distance = sqrt(dc * ds)
		// but having a factor 'm' to play with is probably handy

		// This code could be more efficient

		// Squared spatial distance
		// ds is a fixed 'image' we should be able to exploit this
		// and use a fixed meshgrid for much of the time somehow...	
		int x,y,n;
		double diff;
		int rows = im.length;
		int cols = im[0].length;
		double X[][] = new double[rows][cols];
		double Y[][] = new double[rows][cols];
		double ds2[][] = new double[rows][cols];
		double dc2[][] = new double[rows][cols];
		double D[][] = new double[rows][cols];
		double Ssquared = S*S;
		double msquared = m*m;
		for (y = 0; y < rows; y++) {
			for (x = 0; x < cols; x++) {
				X[y][x] = c1 + x;
				Y[y][x] = r1 + y;
				// x and y dist form cluster center
				X[y][x] = X[y][x] - C[3];
				Y[y][x] = Y[y][x] - C[4];
				ds2[y][x] = X[y][x]*X[y][x] + Y[y][x]*Y[y][x];
			}
		}
		
		// Squared color differences
		for (n = 0; n < 3; n++) {
			for (y = 0; y < rows; y++) {
				for (x = 0; x < cols; x++) {
					diff = im[y][x][n] - C[n];
					dc2[y][x] += (diff*diff);
				}
			}
		}
		
		for (y = 0; y < rows; y++) {
			for (x = 0; x < cols; x++) {
				D[y][x] = Math.sqrt(dc2[y][x] + ds2[y][x]/Ssquared*msquared);
			}
		}
		return D;
		
	}
	
	
}