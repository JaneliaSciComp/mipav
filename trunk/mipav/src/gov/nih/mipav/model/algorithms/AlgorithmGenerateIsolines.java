package gov.nih.mipav.model.algorithms;

import gov.nih.mipav.model.algorithms.*;
import gov.nih.mipav.model.structures.*;
import gov.nih.mipav.view.MipavUtil;

import java.awt.Graphics;
import java.io.*;
import java.util.*;

public class AlgorithmGenerateIsolines extends AlgorithmBase {
	/**
	 * Ported from Isolines.java
	 * ##library.name##
	 * ##library.sentence##
	 * ##library.url##
	 *
	 *  FindIsolines.java
	 *
	 *  Fast implementation of marching squares
	 *
	 *  AUTHOR:   Murphy Stein, Greg Borenstein
	 *            New York University
	 *  CREATED:  Jan-Sept 2012 
	 *
	 *  LICENSE:  BSD
	 *
	 *  Copyright (c) 2012 New York University.
	 *  All rights reserved.
	 *
	 *  Redistribution and use in source and binary forms are permitted
	 *  provided that the above copyright notice and this paragraph are
	 *  duplicated in all such forms and that any documentation,
	 *  advertising materials, and other materials related to such
	 *  distribution and use acknowledge that the software was developed
	 *  by New York Univserity.  The name of the
	 *  University may not be used to endorse or promote products derived
	 *  from this software without specific prior written permission.
	 *  THIS SOFTWARE IS PROVIDED ``AS IS'' AND WITHOUT ANY EXPRESS OR
	 *  IMPLIED WARRANTIES, INCLUDING, WITHOUT LIMITATION, THE IMPLIED
	 *  WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE.
	 * 
	 * @author      ##author##
	 * @modified    ##date##
	 * @version     ##library.prettyVersion## (##library.version##)
	 */
	
	/**
	 * This is a fast implementation of the marching squares algorithm for finding isolines (lines of equal color) in an image.
	 * 
	 * @example IsolinesFromDepthImage 
	 * 
	 *
	 */
	private double threshold;
	private float intensityR;
	private float intensityG;
	private float intensityB;
	
	private double in[];
	private float out[][];
	
	private double[] vx;		// tmp
	private double[] vy;
	private double[] minx;
	private double[] miny;
	private double[] maxx;
	private double[] maxy;
	
	private int[] cd;	
	private double[] cx;	
	private double[] cy;
	private double[] cu;
	private int[] cl;
	private int[] co;
	private double[] cu2;	
	private double[] tips;
			
	private int n = 0;
	private int xDim;
	private int yDim;
	private int sliceSize;
	private double STEP_SIZE = 0.25;
	private int numContours = 0;
	
	private final int CASE0		=	0x00000000; 
	private final int CASE1		=	0x00000001;
	private final int CASE2		=	0x00000010;
	private final int CASE3		=	0x00000011;
	private final int CASE4		=	0x00000100;
	private final int CASE5		=	0x00000101;
	private final int CASE6		=	0x00000110;
	private final int CASE7		=	0x00000111;
	private final int CASE8		=	0x00001000;
	private final int CASE9		=	0x00001001;
	private final int CASE10	=	0x00001010;
	private final int CASE11	=	0x00001011;
	private final int CASE12	=	0x00001100;
	private final int CASE13	=	0x00001101;
	private final int CASE14	=	0x00001110;
	private final int CASE15	=	0x00001111;
	private final int WHITE		=	1;
	private final int BLACK		=	1;
	
	public AlgorithmGenerateIsolines(ModelImage destImg, ModelImage srcImg, double threshold, 
			float intensityR, float intensityG, float intensityB) {
        super(destImg, srcImg);
        this.threshold = threshold;
        this.intensityR = intensityR;
        this.intensityG = intensityG;
        this.intensityB = intensityB;
    }
	
	public void runAlgorithm() {
		int i;
        int z, t;
        if (srcImage == null) {
            displayError("Source Image is null");

            return;
        }
        
        fireProgressStateChanged(srcImage.getImageName(), "Generating isolines ...");
        
        xDim = srcImage.getExtents()[0];
        yDim = srcImage.getExtents()[1];
        sliceSize = xDim * yDim;
        in = new double[sliceSize];
        out = new float[3][sliceSize];
        cd = new int[sliceSize];	
		cx = new double[sliceSize];	
		cy = new double[sliceSize];
		cu = new double[sliceSize];
		cl = new int[sliceSize];
		co = new int[sliceSize];
		cu = new double[sliceSize];
		minx = new double[sliceSize];
		miny = new double[sliceSize];
		maxx = new double[sliceSize];
		maxy = new double[sliceSize];
		vx = new double[sliceSize];
		vy = new double[sliceSize];
		tips = new double[sliceSize];
		
        int zDim = 1;
        if (srcImage.getNDims() > 2) {
        	zDim = srcImage.getExtents()[2];
        }
        int volume = zDim * sliceSize;
        int tDim = 1;
        if (srcImage.getNDims() > 3) {
        	tDim = srcImage.getExtents()[3];
        }
        
        for (t = 0; t < tDim; t++) {
        	for (z = 0; z < zDim; z++) {
        		try {
                    srcImage.exportData((t * volume) + (z * sliceSize), sliceSize, in); // locks and releases lock
                } catch (IOException error) {
                    in = null;
                    errorCleanUp("Algorithm Generate Isolines: Image(s) locked", true);

                    return;
                }
        		
        		for (i = 0; i < sliceSize; i++) {
        		    out[0][i] = (float)in[i];
        		    out[1][i] = (float)in[i];
        		    out[2][i] = (float)in[i];
        		}
        		
				////////////////////////////////////////////////////
				// FIND ISOLINES
				////////////////////////////////////////////////////
        		createOnePixelBorder(threshold + 1);
        		preCodeImage();
        		numContours = findIsolines();
        		drawContours();
        		try {
        			destImage.importRGBData(1,4*((t * volume) + (z * sliceSize)),out[0],false);
        		}
        		catch(IOException e) {
        			out = null;
                    errorCleanUp("Algorithm Generate Isolines: Image(s) locked", true);

                    return;
        		}
        		
        		try {
        			destImage.importRGBData(2,4*((t * volume) + (z * sliceSize)),out[1],false);
        		}
        		catch(IOException e) {
        			out = null;
                    errorCleanUp("Algorithm Generate Isolines: Image(s) locked", true);

                    return;
        		}

        		try {
        			destImage.importRGBData(3,4*((t * volume) + (z * sliceSize)),out[2],false);
        		}
        		catch(IOException e) {
        			out = null;
                    errorCleanUp("Algorithm Generate Isolines: Image(s) locked", true);

                    return;
        		}

        		
        	} // for (z = 0; z < zDim; z++)
        } // for (t = 0; t < tDim; t++) 
        destImage.calcMinMax();
        setCompleted(true);
	} // runAlgorithm
	
	////////////////////////////////////////////////////
	// CREATE single pixel, 0-valued border around pix
	////////////////////////////////////////////////////
	private void createOnePixelBorder(double borderval) {
		for (int i = 0, j = sliceSize-1; i < xDim; i++, j--) {
			in[i] = borderval;
			in[j] = borderval;
		}
		for (int i = 0, j = xDim-1; i < yDim; i++, j+=xDim) {
			in[i] = borderval;
			in[j] = borderval;
		}    
	}
	
	////////////////////////////////////////////////////
	// CODE each 2x2 pixel
	// depends only on whether each of four corners is
	// above or below threshold.
	////////////////////////////////////////////////////
	private void preCodeImage() {
		int b0, b1, b2, b3;
		for (int x = 0; x < xDim - 1; x++) {
			for (int y = 0; y < yDim - 1; y++) {
				b0 = in[ixy(x + 0, y + 0)] < threshold ? 0x00001000 : 0x00000000;
				b1 = in[ixy(x + 1, y + 0)] < threshold ? 0x00000100 : 0x00000000;
				b2 = in[ixy(x + 1, y + 1)] < threshold ? 0x00000010 : 0x00000000;
				b3 = in[ixy(x + 0, y + 1)] < threshold ? 0x00000001 : 0x00000000;
				cd[ixy(x, y)] = b0 | b1 | b2 | b3;
			}
		} 
	}
	
	public int ixy(int x, int y) {
		return x + y * xDim;
	}
	
	////////////////////////////////////////////////////
	// SEARCH image for contours from topleft corner
	// to bottomright corner.
	////////////////////////////////////////////////////    
	private int findIsolines() {
	int vi = 0;
	int next = -1;
	int i = 0, x = 0, y = 0;
	//double tox = 0, fromx = 0;
	//double toy = 0, fromy = 0;
	int fromedge = -1;
	int toedge = -1;
	int code = 0;
	double avg = 0;
	int contournum = -1;
	int length = 0;
	while (i < sliceSize) {
	
	fromedge = toedge;			
	if (next < 0) next = ++i;
	x = next % xDim;
	y = next / xDim;
	if (x >= (xDim - 1) || y >= (yDim - 1)) {
	next = -1;
	continue;
	}
	code = cd[next];
	
	switch (code) {
	case CASE0:									// CASE 0
	toedge = -1;
	break;
	case CASE1:									// CASE 1
	cd[next] = 0;
	//out ("case 1");
	toedge = 2;
	break;
	case CASE2:									// CASE 2
	cd[next] = 0;
	//out ("case 2");
	toedge = 1;
	break;
	case CASE3:									// CASE 3
	cd[next] = 0;
	toedge = 1;
	//out ("case 3");
	
	break;
	case CASE4:									// CASE 4
	cd[next] = 0;
	toedge = 0;
	//out ("case 4");
	
	break;
	case CASE5:									// CASE 5, saddle
	avg = 0.25 * (double)(in[ixy(x + 0, y + 0)] + in[ixy(x + 1, y + 0)] + in[ixy(x + 0, y + 1)] + in[ixy(x + 1, y + 1)]);
	if (avg > threshold) {
	if (fromedge == 3) {						// treat as case 1, then switch code to case 4
	toedge = 2;								
	cd[next] = CASE4;
	} else {									// treat as case 4, then switch code to case 1
	toedge = 0;								
	cd[next] = CASE1;
	}
	} else {
	if (fromedge == 3) {						// treat as case 7, then switch code to case 13
	toedge = 0;								
	cd[next] = CASE13;
	} else {									// treat as case 13, then switch code to case 7
	toedge = 2;								
	cd[next] = CASE7;
	}
	}
	//out ("case 5");
	
	break;
	case CASE6:									// CASE 6
	cd[next] = 0;
	toedge = 0;
	//out ("case 6");
	
	break;
	case CASE7:									// CASE 7
	cd[next] = 0;
	toedge = 0;
	//out ("case 7");
	
	break;
	case CASE8:									// CASE 8
	cd[next] = 0;
	toedge = 3;
	//out ("case 8");
	
	break;
	case CASE9:									// CASE 9
	cd[next] = 0;
	toedge = 2;
	//out ("case 9");
	
	break;
	case CASE10:									// CASE 10, saddle
	avg = 0.25 * (double)(in[ixy(x + 0, y + 0)] + in[ixy(x + 1, y + 0)] + in[ixy(x + 0, y + 1)] + in[ixy(x + 1, y + 1)]);
	if (avg > threshold) {
	if (fromedge == 0) {						// treat as case 8, then switch code to case 2
	toedge = 3;								
	cd[next] = CASE2;
	} else {									// treat as case 2, then switch code to case 8
	toedge = 1;								
	cd[next] = CASE8;
	}
	} else {
	if (fromedge == 2) {						// treat as case 14, then switch code to case 11
	toedge = 3;								
	cd[next] = CASE11;
	} else {									// treat as case 11, then switch code to case 14
	toedge = 1;								
	cd[next] = CASE14;
	}
	}
	//out ("case 10");
	
	break;
	case CASE11:									// CASE 11
	cd[next] = 0;
	toedge = 1;
	//out ("case 11");
	
	break;
	case CASE12:									// CASE 12
	cd[next] = 0;
	toedge = 3;
	//out ("case 12");
	
	break;
	case CASE13:									// CASE 13
	cd[next] = 0;
	toedge = 2;
	//out ("case 13");
	
	break;
	case CASE14:									// CASE 14
	cd[next] = 0;
	toedge = 3;
	//out ("case 14");
	
	break;
	case CASE15:									// CASE 15
	toedge = -1;
	break;
	default:
	//out("Uh oh, unknown case");
	break;
	}
	
	if (fromedge == -1 && toedge > -1) {					// starting a new contour
	contournum++;
	}
	
	switch (toedge) {			
	case 0: 
	cx[vi] = x + t(in[ixy(x + 0, y + 0)], in[ixy(x + 1, y + 0)]);
	cy[vi++] = (double)y;
	next = ixy(x + 0, y - 1);
	cl[contournum] = ++length;			
	break;
	case 1:
	cx[vi] = (double)x + 1;
	cy[vi++] = y + t(in[ixy(x + 1, y + 0)], in[ixy(x + 1, y + 1)]);
	next = ixy(x + 1, y + 0);
	cl[contournum] = ++length;			
	break;
	case 2:
	cx[vi] = x + t(in[ixy(x + 0, y + 1)], in[ixy(x + 1, y + 1)]);
	cy[vi++] = (double)(y + 1);
	next = ixy(x, y + 1);
	cl[contournum] = ++length;			
	break;
	case 3:
	cx[vi] = (double)x;
	cy[vi++] = y + t(in[ixy(x + 0, y + 0)], in[ixy(x + 0, y + 1)]);
	next = ixy(x - 1, y + 0);
	cl[contournum] = ++length;			
	break;
	default:
	next = -1;
	length = 0;
	break;
	}
	
	}
	
	numContours = contournum + 1;
	int sum = 0;
	for (i = 0; i < numContours; i++) {
	co[i] = sum;
	sum += cl[i];
	}
	
	computeBoundingBoxes();
	
	return numContours;
	}
	
	////////////////////////////////////////////////////
	// LERP between to values
	////////////////////////////////////////////////////    
	public double t(double A, double B) {
	    if (A-B == 0) return 0;
	    return ((A - threshold) / (A - B));
	}
	
	// POLYGON HIT TESTING ROUTINES
	
	public void computeBoundingBoxes() {
		for (int k = 0; k < getNumContours(); k++) {
			int o = co[k];
			minx[k] = cx[o];
			miny[k] = cy[o];
			maxx[k] = cx[o];
			maxy[k] = cy[o];
			for (int i = 1; i < getContourLength(k); i++) {
				int j = o + i;
				if (cx[j] < minx[k]) 
					minx[k] = cx[j];
				if (cx[j] > maxx[k]) 
					maxx[k] = cx[j];
				if (cy[j] < miny[k]) 
					miny[k] = cy[j];
				else if (cy[j] > maxy[k]) 
					maxy[k] = cy[j];
			}
		}
	}

	public int getContourLength(int contour) {
		return cl[contour];
	}
	
  
	public int getNumContours() {
		return numContours;
	}
	
	public void drawContours() {
		for (int k = 0; k < getNumContours(); k++) {
			drawContour(k);
		}
	}
	
	public void drawContour(int contour){
		double cx = 0, cy = 0;
		for (int i = 0; i < getContourLength(contour); i++) {
			cx += getContourX(contour, i);
		    cy += getContourY(contour, i);
		}
		cx = cx / getContourLength(contour);
		cy = cy / getContourLength(contour);
		    
		    double s = 1.0;
		    for (int i = 0; i < getContourLength(contour); i++) {
		      int xi = (int)((getContourX(contour, i) - cx) * s + cx);
		      int yi = (int)((getContourY(contour, i) - cy) * s + cy);
		      //int xj = (int)((getContourX(contour, i - 1) - cx) * s + cx);
		      //int yj = (int)((getContourY(contour, i - 1) - cy) * s + cy);
		      //int tx = (xi + xj) / 2;
		      //int ty = (yi + yj) / 2;
		      //int nx = (int)(10 * (getContourY(contour, i) - getContourY(contour, i - 1)));
		      //int ny = (int)(10 * (getContourX(contour, i - 1) - getContourX(contour, i)));
		      
		      //g.drawLine(xi, yi, xj, yj);
		      out[0][xi + xDim*yi] = intensityR;
		      out[1][xi + xDim*yi] = intensityG;
		      out[2][xi + xDim*yi] = intensityB;
		    }
	}
	
	public double getContourX(int contour, int v) {	
		int o = co[contour];
		return cx[wrap(o + v, o, o + cl[contour])];
	}
  
	public double getContourY(int contour, int v) {
		int o = co[contour];
		return cy[wrap(o + v, o, o + cl[contour])];
	}
	
	public int wrap(int i, int lo, int hi) {
		int l = hi - lo;
		int d = i - lo;
		int w = 0;
		if (d < 0) w = hi - ((-d) % l);
		else w = lo + d % l;
		if (w == hi) w = lo;
		if (w < lo) {
			System.out.println("went below lo");
		} else if (w >= hi) {
			System.out.println("went above hi");
		}
		
    
		return w;		
	}
}