package gov.nih.mipav.model.algorithms.utilities;

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
	private double STEP_LENGTH = 0.25;
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

        if (srcImage == null) {
            displayError("Source Image is null");

            return;
        }
        
        fireProgressStateChanged(srcImage.getImageName(), "Generating isolines ...");
        
        xDim = srcImage.getExtents()[0];
        yDim = srcImage.getExtents()[1];
        int length = xDim * yDim;
        in = new double[length];
        cd = new int[length];	
		cx = new double[length];	
		cy = new double[length];
		cu = new double[length];
		cl = new int[length];
		co = new int[length];
		cu = new double[length];
		minx = new double[length];
		miny = new double[length];
		maxx = new double[length];
		maxy = new double[length];
		vx = new double[length];
		vy = new double[length];
		tips = new double[length];
		
        int zDim = 1;
        if (srcImage.getNDims() > 2) {
        	zDim = srcImage.getExtents()[2];
        }
        int tDim = 1;
        if (srcImage.getNDims() > 3) {
        	tDim = srcImage.getExtents()[3];
        }
	}
}