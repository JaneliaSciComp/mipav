package gov.nih.mipav.model.algorithms;

import gov.nih.mipav.model.structures.*;
import gov.nih.mipav.model.structures.jama.GeneralizedEigenvalue;
import gov.nih.mipav.model.structures.jama.LinearEquations2;
import gov.nih.mipav.view.*;

import java.awt.Color;
import java.awt.Graphics;
import java.awt.Rectangle;
import java.awt.event.MouseEvent;
import java.awt.event.MouseListener;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.Collections;
import java.util.Comparator;
import java.util.Vector;
import java.util.concurrent.locks.Condition;
import java.util.concurrent.locks.Lock;
import java.util.concurrent.locks.ReentrantLock;

import de.jtem.numericalMethods.algebra.linear.decompose.Eigenvalue;

public class SchwarzChristoffelMapping extends AlgorithmBase implements MouseListener {
	
	// This is a port of portions of the Schwarz-Christoffel Toolbox from MATLAB to Java
	// with the kind permission of Professor Toby Driscoll.  The original code is:
	// Version 2.3   January 15, 2003
	// Copyright (c) 1994-2003 by Toby Driscoll (driscoll@math.udel.edu).

	// How much progress information to show during and after the solution to the parameter problem.
	//private boolean traceSolution = false;
	
	// Desired accuracy in the map.  This may not be met exactly.
	private double tolerance = 1.0E-8;
	
	// eps returns the distance from 1.0 to the next larger double-precision number, that is, eps = 2^-52.
	private double eps;
	
	private final Lock accessLock = new ReentrantLock();
	private final Condition canProcessMouseClick = accessLock.newCondition();
	private int xClick;
	private int yClick;
	
	// polygon vertices
	// w[i][0] x coordinate of ith vertex
	// w[i][1] y coordinate of ith vertex
	private double w[][];
	
	public SchwarzChristoffelMapping() {
		
	}
	
	public SchwarzChristoffelMapping(ModelImage destImg, ModelImage srcImg, double w[][]) {
		super(destImg, srcImg);
		this.w = w;
	}
	
	public void runAlgorithm() {
		// eps returns the distance from 1.0 to the next larger double-precision number, that is, eps = 2^-52.
    	// epsilon = D1MACH(4)
        // Machine epsilon is the smallest positive epsilon such that
        // (1.0 + epsilon) != 1.0.
        // epsilon = 2**(1 - doubleDigits) = 2**(1 - 53) = 2**(-52)
        // epsilon = 2.2204460e-16
        // epsilon is called the largest relative spacing
        eps = 1.0;
        double neweps = 1.0;


        while (true) {

            if (1.0 == (1.0 + neweps)) {
                break;
            } else {
                eps = neweps;
                neweps = neweps / 2.0;
            }
        } // while(true)
        
        //testRectmap1();
        testDiskmap1();
        //testDiskmap2();
		
	}
	
	public void testDiskmap1() {
		int i;
		int j;
		double w[][] = new double[4][2];
		w[0][0] = 1;
		w[0][1] = 1;
		w[1][0] = -1;
		w[1][1] = 1;
		w[2][0] = -1;
		w[2][1] = -1;
		w[3][0] = 1;
		w[3][1] = -1;
		scmap M = diskmap(w, tolerance, null, null);
		double wc[] = new double[2];
		M = center(M, wc);
		// Points for unit circle
		int n = 180;
		double wcir[][] = new double[n][2];
		double xcir[] = new double[n];
		double ycir[] = new double[n];
		for (i = 0; i < n; i++) {
			wcir[i][0] = Math.cos(i*2.0*Math.PI/(n+1.0));
			wcir[i][1] = Math.sin(i*2.0*Math.PI/(n+1.0));
			xcir[i] = wcir[i][0];
			ycir[i] = wcir[i][1];
		}
		polygon poly = new polygon(xcir, ycir, null);
		double beta[] = new double[poly.angle.length];
		for (i = 0; i < poly.angle.length; i++) {
			beta[i] = poly.angle[i] - 1.0;
		}
		float xPointArray[] = new float[n+1];
		float yPointArray[] = new float[n+1];
		int nqpts = 5;
		double axlim[] = new double[4];
		// Minimum line segment length, as a proportion of the axes box
        double minlen = 0.005;
        // Maximum line segment length, as a proportion of the axes box
        double maxlen = 0.02;
		ViewJFrameGraph pointGraph = plotpoly(xPointArray, yPointArray, wcir, beta, false, axlim);
		double qdat[][] = new double[nqpts][2*beta.length+2];
		scqdata(qdat, beta, nqpts);
		ViewJComponentGraph graph = pointGraph.getGraph();
		Rectangle graphBounds = graph.getGraphBounds();
		Graphics g = graph.getGraphics();
		double xScale = graphBounds.width / (axlim[1] - axlim[0]);
        double yScale = graphBounds.height / (axlim[3] - axlim[2]);
		double len = Math.max(graphBounds.width, graphBounds.height);
		minlen = len * minlen;
		maxlen = len * maxlen;
		
		double X[][] = new double[201][9];
		double Y[][] = new double[201][9];
		double wp[][] = new double[201*9][2];
		double zp[][] = new double[201*9][2];
		for (i = 0; i < 201; i++) {
			for (j = 0; j < 9; j++) {
			    X[i][j] = -0.8 + 0.2*j;
			    Y[i][j] = -1.0 + 0.01*i;
			    wp[201*j + i][0] = X[i][j];
			    wp[201*j + i][1] = Y[i][j];
			}
		}
		int flag[] = null;
		int maxiter = 200;
		flag = evalinv(zp, M, wp, M.qdata, null, maxiter);
		Vector<Double>x1Vector = new Vector<Double>();
		Vector<Double>y1Vector = new Vector<Double>();
		Vector<Double>x2Vector = new Vector<Double>();
		Vector<Double>y2Vector = new Vector<Double>();
		for (i = 0; i < 9; i++) {
			for (j = 0; j < 200; j++) {
				double posx1 = zp[201*i + j][0];
	    		double posy1 = zp[201*i + j][1];
	    		double posx2 = zp[201*i + j + 1][0];
	    		double posy2 = zp[201*i + j + 1][1];
	    		x1Vector.add(posx1);
	    		y1Vector.add(posy1);
	    		x2Vector.add(posx2);
	    		y2Vector.add(posy2);
	    	    int x1 =  (int)Math.round(graphBounds.x + xScale*(posx1 - axlim[0]));
			    int y1 =  (int)Math.round(graphBounds.y + yScale*(posy1 - axlim[2]));
			    y1 = -y1 + 2*graphBounds.y + graphBounds.height;
			    int x2 =  (int)Math.round(graphBounds.x + xScale*(posx2 - axlim[0]));
			    int y2 =  (int)Math.round(graphBounds.y + yScale*(posy2 - axlim[2]));
			    y2 = -y2 + 2*graphBounds.y + graphBounds.height;
			    graph.drawLine(g, x1, y1, x2, y2);	
			}
		}
		for (i = 0; i < 201; i++) {
			for (j = 0; j < 9; j++) {
			    wp[201*j + i][0] = Y[i][j];
			    wp[201*j + i][1] = X[i][j];
			}
		}
		flag = evalinv(zp, M, wp, M.qdata, null, maxiter);
		for (i = 0; i < 9; i++) {
			for (j = 0; j < 200; j++) {
				double posx1 = zp[201*i + j][0];
	    		double posy1 = zp[201*i + j][1];
	    		double posx2 = zp[201*i + j + 1][0];
	    		double posy2 = zp[201*i + j + 1][1];
	    		x1Vector.add(posx1);
	    		y1Vector.add(posy1);
	    		x2Vector.add(posx2);
	    		y2Vector.add(posy2);
	    	    int x1 =  (int)Math.round(graphBounds.x + xScale*(posx1 - axlim[0]));
			    int y1 =  (int)Math.round(graphBounds.y + yScale*(posy1 - axlim[2]));
			    y1 = -y1 + 2*graphBounds.y + graphBounds.height;
			    int x2 =  (int)Math.round(graphBounds.x + xScale*(posx2 - axlim[0]));
			    int y2 =  (int)Math.round(graphBounds.y + yScale*(posy2 - axlim[2]));
			    y2 = -y2 + 2*graphBounds.y + graphBounds.height;
			    graph.drawLine(g, x1, y1, x2, y2);	
			}
		}
		
		graph.setX1Vector(x1Vector);
		graph.setY1Vector(y1Vector);
		graph.setX2Vector(x2Vector);
		graph.setY2Vector(y2Vector);
		graph.setAddSchwarzChristoffelLines(true);
		graph.paintComponent(g);
	}
	
	public void testDiskmap2() {
		int i;
		double w[][] = new double[4][2];
		w[0][0] = -4;
		w[0][1] = -1;
		w[1][0] = 4;
		w[1][1] = -1;
		w[2][0] = 4;
		w[2][1] = 1;
		w[3][0] = -4;
		w[3][1] = 1;
		scmap M = diskmap(w, tolerance, null, null);
		double wc[] = new double[2];
		M = center(M, wc);
		double R[] = new double[4];
		for (i = 0; i < 4; i++) {
			R[i] = 0.2*(i+1);
		}
		double prevertex[][] = M.prevertex;
		double theta[] = new double[4];
		for (i = 0; i < 4; i++) {
			theta[i] = Math.atan2(prevertex[i][1], prevertex[i][0]);
		}
		diskplot(M, R, theta, null);
	}
	
	public void testRectmap1() {
		// Example from users guide
		// Test with
		// SchwarzChristoffelMapping sc = new SchwarzChristoffelMapping();
    	// sc.runAlgorithm();
		double w[][] = new double[6][2];
        w[0][0] = -5;
        w[0][1] = -1;
        w[1][0] = -5;
        w[1][1] = -3;
        w[2][0] = 5;
        w[2][1] = -3;
        w[3][0] = 5;
        w[3][1] = 1;
        w[4][0] = 5;
        w[4][1] = 3;
        w[5][0] = -5;
        w[5][1] = 3;
        int corner[] = new int[4];
        corner[0] = 0;
        corner[1] = 1;
        corner[2] = 3;
        corner[3] = 4;
        scmap M = rectmap(w, corner, tolerance, null, null, null);
        rectplot(M, 10, 10);
	}
	
	public scmap diskmap(double w[][], double tolerance, double z[][], double c[]) {
		// Schwarz-Christoffel disk map object
		// diskmap constructs a Schwarz-Christoffel disk map object for the polygon
		// whose vertices are given by w.  The parameter problem is solved using
		// default options for the prevertices and the multiplicative constant.
		// If z is supplied diskmap creates a diskmap object having the given
		// prevertices z (the multiplicative constant is found automatically).
		// There is no checking to ensure that the prevertices are consistent with
		// the given polygon.  diskmap uses the supplied constant c when provided.
		// Based on original MATLAB routine copyright 1998-2001 by Toby Driscoll.
		int i;
		int nqpts;
		double qdata[][] = null;
		double wn[][] = null;
		double betan[] = null;
		double z0[][] = null;
		double x[] = new double[w.length];
		double y[] = new double[w.length];
		for (i = 0; i < w.length; i++) {
			x[i] = w[i][0];
			y[i] = w[i][1];
		}
		polygon poly = new polygon(x, y, null);
		double beta[] = new double[poly.angle.length];
		for (i = 0; i < poly.angle.length; i++) {
			beta[i] = poly.angle[i] - 1.0;
		}
		if ((z == null) || (z.length == 0)) {
			// Solve parameter problem
			// Apply scfix to enforce solver rules
			wn = new double[w.length+2][2];
			betan = new double[w.length+2];
			// Number of vertices added by scfix
			int verticesAdded[] = new int[1];
			int initialVertices = w.length;
			for (i = 0; i < w.length; i++) {
				wn[i][0] = w[i][0];
				wn[i][1] = w[i][1];
				betan[i] = beta[i];
			}
			scfix(wn, betan, verticesAdded, null, "d", w, beta, null);
			double wn2[][];
			double betan2[];
			if ((verticesAdded[0] == 0) ||(verticesAdded[0] == 1)) {
			    wn2 = new double[initialVertices + verticesAdded[0]][2];
				betan2 = new double[initialVertices + verticesAdded[0]];
				for (i = 0; i < initialVertices + verticesAdded[0]; i++) {
					wn2[i][0] = wn[i][0];
					wn2[i][1] = wn[i][1];
					betan2[i] = betan[i];
				}
			}
			else {
				wn2 = wn;
				betan2 = betan;
			}
			double xn[] = new double[wn2.length];
			double yn[] = new double[wn2.length];
			for (i = 0; i < wn2.length; i++) {
				xn[i] = wn2[i][0];
				yn[i] = wn2[i][1];
			}
			double alpha[] = new double[betan2.length];
			for (i = 0; i < betan2.length; i++) {
				alpha[i] = betan2[i] + 1.0;
			}
			poly = new polygon(xn, yn, alpha);
			c = new double[2];
			z = new double[wn2.length][2];
			nqpts = Math.max((int)Math.ceil(-Math.log10(tolerance)), 4);
			qdata = new double[nqpts][2*betan2.length+2];
			dparam(z, c, qdata, wn2, betan2, z0, tolerance);
		} // if ((z == null) || (z.length == 0))
		
		if ((qdata == null) || (qdata.length == 0)) {
		    // Base accuracy of quadrature on given options	
			nqpts = (int)Math.ceil(-Math.log10(tolerance));
			beta = new double[poly.angle.length];
			for (i = 0; i < poly.angle.length; i++) {
				beta[i] = poly.angle[i] - 1.0;
			}
			qdata = new double[nqpts][2*beta.length+2];
			scqdata(qdata, beta, nqpts);
		} // if ((qdata == null) || (qdata.length == 0))
		
		if ((c == null) || (c.length == 0)) {
		    // Find constant
			w = poly.vertex;
			beta = new double[poly.angle.length];
			for (i = 0; i < poly.angle.length; i++) {
				beta[i] = poly.angle[i] - 1.0;
			}
			boolean found = false;
			int idx = -1;
			for (i = 1; i < z.length && (!found); i++) {
			    if ((!Double.isInfinite(z[i][0])) && (!Double.isInfinite(z[i][1]))) {
			        idx = i+1;
			        found = true;
			    }
			}
			double mid[][] = new double[1][2];
			mid[0][0] = (z[0][0] + z[idx][0])/2.0;
			mid[0][1] = (z[0][1] + z[idx][1])/2.0;
			double z02[][] = new double[1][2];
			z02[0][0] = z[0][0];
			z02[0][1] = z[0][1];
			int sing0[] = new int[1];
			sing0[0] = 0;
			double zidx[][] = new double[1][2];
			zidx[0][0] = z[idx][0];
			zidx[0][1] = z[idx][1];
			int singidx[] = new int[1];
			singidx[0] = idx;
			double I1[][] = dquad(z02, mid, sing0, z, beta, qdata);
			double I2[][] = dquad(zidx, mid, singidx, z, beta, qdata);
		    double I[] = new double[2];
		    I[0] = I1[0][0] - I2[0][0];
		    I[1] = I1[0][1] - I2[0][1];
		    double diffw[] = new double[2];
		    diffw[0] = w[idx][0] - w[0][0];
		    diffw[1] = w[idx][1] - w[0][1];
		    double cr[] = new double[1];
		    double ci[] = new double[1];
		    zdiv(diffw[0], diffw[1], I[0], I[1], cr, ci);
		    c[0] = cr[0];
		    c[1] = ci[0];
		} // if ((c == null) || (c.length == 0))
		scmap map = new scmap();
		map.prevertex = z;
		map.constant[0] = c[0];
		map.constant[1]= c[1];
		map.qdata = qdata;
		map.poly = poly;
		
		// Find conformal center
		beta = new double[poly.angle.length];
		for (i = 0; i < poly.angle.length; i++) {
			beta[i] = poly.angle[i] - 1.0;
		}
		double zp[][] = new double[1][2];
		double center[][] = dmap(zp, poly.vertex, beta,map.prevertex,map.constant,map.qdata);
		map.center[0] = center[0][0];
		map.center[1] = center[0][1];
		
		// Fill in apparent accuracy
		map.accuracy = diskAccuracy(map);
		return map;
	}
	
	private double diskAccuracy(scmap M) {
	    // Apparent accuracy of Schwarz-Christoffel disk map.
		// diskAccuracy estimates the accuracy of the Schwarz-Christoffel disk
		// map M.  The technique used is to compare the differences between
		// successive finite vertices to the integral between the corresponding
		// prevertices, and return the maximum.
		
		// See also diskmap.
		
		// Original MATLAB accuracy routine copyright 1998 by Toby Driscoll.
		
		// If an accuracy has been assigned, don't question it.
		int i, j;
		double acc;
		if (!Double.isNaN(M.accuracy)) {
			acc =  M.accuracy;
			return acc;
		}
		
		// Get data for low-level funcions
		polygon p = M.poly;
		double w[][] = p.vertex;
		double beta[] = new double[p.angle.length];
		for (i = 0; i < p.angle.length; i++) {
			beta[i] = p.angle[i]  - 1;
		}
		double z[][] = M.prevertex;
		double c[] = M.constant;
		double qdata[][] = M.qdata;
		
		// Test accuracy by integrating between consecutive finite prevertices, and
		// comparing to differences of vertices.
		int n = w.length;
		int numidx = 0;
		for (i = 0; i < n; i++) {
			if ((!Double.isInfinite(w[i][0])) && (!Double.isInfinite(w[i][1]))) {
				numidx++;
			}
		} // for (i = 0; i < n; i++)
		int idx[] = new int[numidx];
		double wf[][] = new double[numidx][2]; // finite vertices
		for (i = 0, j = 0; i < n; i++) {
			if ((!Double.isInfinite(w[i][0])) && (!Double.isInfinite(w[i][1]))) {
				idx[j] = i;
				wf[j][0] = w[i][0];
				wf[j++][1] = w[i][1];
			}
		} // for (i = 0, j = 0; i < n; i++)
		
		// Two columns hold endpoint indices for integrations
		int idx2[][] = new int[idx.length][2];
		for (i = 0; i < idx.length; i++) {
			idx2[i][0] = idx[i];
			if (i < idx.length-1) {
				idx2[i][1] = idx[i+1];
			}
			else {
				idx2[i][1] = idx[0];
			}
		} // for (i = 0; i < idx.length; i++)
		
		double mid[][] = new double[idx.length][2];
		
		// Do the integrations
		double z1[][] = new double[numidx][2];
		double z2[][] = new double[numidx][2];
		int i1[] = new int[numidx];
		int i2[] = new int[numidx];
		for (i = 0; i < numidx; i++) {
			z1[i][0] = z[idx2[i][0]][0];
			z1[i][1] = z[idx2[i][0]][1];
			z2[i][0] = z[idx2[i][1]][0];
			z2[i][1] = z[idx2[i][1]][1];
			i1[i] = idx2[i][0];
			i2[i] = idx2[i][1];
		}
		
		double I1[][] = dquad(z1,mid,i1,z,beta,qdata);
		double I2[][] = dquad(z2,mid,i2,z,beta,qdata);
		double I[][] = new double[numidx][2];
		for (i = 0; i < numidx; i++) {
			I[i][0] = I1[i][0] - I2[i][0];
			I[i][1] = I1[i][1] - I2[i][1];
		}
		
		double diffwf[][] = new double[numidx][2];
		for (i = 0; i < numidx-1; i++) {
			diffwf[i][0] = wf[i+1][0] - wf[i][0];
			diffwf[i][1] = wf[i+1][1] - wf[i][1];
		}
		diffwf[numidx-1][0] = wf[0][0] - wf[numidx-1][0];
		diffwf[numidx-1][1] = wf[0][1] - wf[numidx-1][1];
		double cI[][] = new double[numidx][2];
		double cr[] = new double[1];
		double ci[] = new double[1];
		for (i = 0; i < numidx; i++) {
			zmlt(c[0],c[1],I[i][0],I[i][1],cr,ci);
			cI[i][0] = cr[0];
			cI[i][1] = ci[0];
		}
		acc = 0.0;
		for (i = 0; i < numidx; i++) {
			double currentAcc = zabs(cI[i][0] - diffwf[i][0],cI[i][1] - diffwf[i][1]);
			if (currentAcc > acc) {
				acc = currentAcc;
			}
		} // for (i = 0; i < numidx; i++)
		System.out.println("Accuracy = " + acc);
		return acc;
	}
	
	private scmap center(scmap map, double wc[]) {
		// center (map, wc) computes a map conformally equivalent to map but with
		// the conformal center wc (provided wc is inside the polygon of map),
		// and returns the new map.  If wc is empty, you will be asked to select
		// it graphically.
		
		// See also diskmap.
		
		// Original MATLAB routine copyright 1998 by Toby Driscoll.
		
		int i,j;
		double cr[] = new double[1];
		double ci[] = new double[1];
		scmap mapout = new scmap();
		// Set center
		polygon p = map.poly;
		double qdata[][] = map.qdata;
		double qdataout[][] = new double[qdata.length][qdata[0].length];
		for (i = 0; i < qdata.length; i++) {
			for (j = 0; j < qdataout[0].length; j++) {
				qdataout[i][j] = qdata[i][j];
			}
		}
		double z[][] = map.prevertex;
		double w[][] = p.vertex;
		double beta[] = new double[p.angle.length];
		for (i = 0; i < p.angle.length; i++) {
			beta[i] = p.angle[i] - 1.0;
		}
		double x[] = new double[w.length];
		double yar[] = new double[w.length];
		for (i = 0; i < w.length; i++) {
			x[i] = w[i][0];
			yar[i] = w[i][1];
		}
		double angleout[] = new double[p.angle.length];
		for (i = 0; i < p.angle.length; i++) {
			angleout[i] = p.angle[i];
		}
		polygon pout = new polygon(x,yar,angleout);
		
		if ((wc == null) || (wc.length == 0)) {
			
		}
		boolean indexout[] = new boolean[wc.length];
		boolean onvtx[][] = new boolean[w.length][wc.length];
		double wcin[][] = new double[1][2];
		wcin[0][0] = wc[0];
		wcin[0][1] = wc[1];
		isinpoly(indexout, onvtx, wcin, w, beta, eps);
		boolean haveInfinity = false;
		for (i = 0; i < w.length && (!haveInfinity); i++) {
			if (Double.isInfinite(w[i][0]) || Double.isInfinite(w[i][1])) {
				haveInfinity = true;
			}
		}
		if ((!haveInfinity) && (!indexout[0])) {
			MipavUtil.displayError("Conformal center must be inside polygon");
			return null;
		}
		
		// Find inverse image of wc under current map
		double zc[][] = new double[wcin.length][2];
		int flag[] = null;
		boolean ode = true;
		boolean newton = true;
		double tol = 1.0E-8;
		int maxiter = 10;
		flag = dinvmap(zc, wcin, w, beta, z, map.constant, qdata, null,
				ode, newton, tol, maxiter);
		
		// Use Moebius transform to reset prevertices
		double y[][] = new double[z.length][2];
		for (i = 0; i < z.length; i++) {
			zmlt(zc[0][0], zc[0][1], z[i][0], z[i][1], cr, ci);
			zdiv(z[i][0] - zc[0][0], z[i][1] - zc[0][1], 1 - cr[0], -ci[0], cr, ci);
			y[i][0] = cr[0];
			y[i][1] = ci[0];
		} 
		// Force it to be exact.
		y[y.length-1][0] = 1;
		y[y.length-1][1] = 0;
		for (i = 0; i < y.length-1; i++) {
		    double yabs = zabs(y[i][0], y[i][1]);
		    y[i][0] = y[i][0]/yabs;
		    y[i][1] = y[i][1]/yabs;
		}
		
		// Recalculate constant
		double mid[][] = new double[1][2];
		mid[0][0] = (y[0][0] + y[1][0])/2.0;
		mid[0][1] = (y[0][1] + y[1][1])/2.0;
		double y0[][] = new double[1][2];
		y0[0][0] = y[0][0];
		y0[0][1] = y[0][1];
		int sing0[] = new int[1];
		sing0[0] = 0;
		double y1[][] = new double[1][2];
		y1[0][0] = y[1][0];
		y1[0][1] = y[1][1];
		int sing1[] = new int[1];
		sing1[0] = 1;
		double I1[][] = dquad(y0, mid, sing0, y, beta, qdata);
		double I2[][] = dquad(y1, mid, sing1, y, beta, qdata);
		double I[] = new double[2];
		I[0] = I1[0][0] - I2[0][0];
		I[1] = I1[0][1] - I2[0][1];
		double diffw[] = new double[2];
		diffw[0] = w[1][0] - w[0][0];
		diffw[1] = w[1][1] - w[0][1];
		zdiv(diffw[0], diffw[1], I[0], I[1], cr, ci);
		double c[] = new double[2];
		c[0] = cr[0];
		c[1] = ci[0];
		
		// Assign new values
		mapout.prevertex = y;
		mapout.constant = c;
		mapout.center = wc;
		mapout.poly = pout;
		mapout.qdata = qdataout;
		mapout.accuracy = diskAccuracy(mapout);
		return mapout;
	}
	
	private int[] evalinv(double zp[][], scmap M, double wp[][], double tol[][], double z0[][], int maxiter) {
		// Invert Schwarz-Christoffel disk map at points.
		// evalinv evaluates the inverse of the Schwarz-Christoffel map M at the points
		// wp in the polygon. evalinv attempts to give an answer accurate to tol.  If
		// tol is smaller than the accuracy of M, this is unlikely to be met.
		// If z0 is supplied, z0 are the given starting points.  z0 must be the
		// same size as wp or a complex scalar (to be expanded to that size).
		// It is used for the starting approximation to the inverse image of wp.
		// The starting guess need not be close to the correct answer; however, the
		// straight line segment between wp[k] and the forward image of z0[k] must
		// lie entirely inside the polygon, for each k. evalinv aslo returns a vector of 
		// indices where the methods was unable to produce a sufficiently small residual.
		// A warning was issued when this occurs.
		// Original MATLAB evalinv routine copyright 1998 by Toby Driscoll.
		int i;
		double qdata[][] = null;
		double tolerance = M.accuracy;
		double z02[][] = null;
		int flag[] = null;
		polygon p = M.poly;
		double vertex[][] = p.vertex;
		double beta[] = new double[p.angle.length];
		for (i = 0; i < beta.length; i++) {
			beta[i] = p.angle[i] - 1;
		}
		
		// Check inputs/supply defaults
		if ((tol != null) && (tol.length != 0)) {
			if (tol.length == 1) {
			    qdata = M.qdata;
			    tolerance = M.accuracy;
			}
			else {
				qdata = tol;
				tolerance = Math.pow(10.0, -qdata.length);
			}
		} // if ((tol != null) && (tol.length != 0))
		
		if ((z0 != null) && (z0.length != 0)) {
		    if (z0.length == 1) {
		    	z02 = new double[wp.length][2];
		    	for (i = 0; i < wp.length; i++) {
		    		z02[i][0] = z0[0][0];
		    		z02[i][1] = z0[0][1];
		    	}
		    } // if (z0.length == 1)
		    else if (z0.length != wp.length) {
		    	MipavUtil.displayError("Argument z0 must be a complex scalar or the same size as wp");
		    	return null;
		    }
		    else {
		    	z02 = z0;
		    }
		} // if ((z0 != null) && (z0.length != 0))
		
		flag = dinvmap(zp, wp, vertex, beta, M.prevertex, M.constant, qdata, z02, true,  true, tolerance, maxiter);
		return flag;
	}
	
	private int[] dinvmap(double zp[][], double wp[][], double w[][],
			double beta[], double z[][], double c[], double qdat[][], double z0[][],
			boolean ode, boolean newton, double tol, int maxiter) {
	    // Schwarz-Christoffel disk inverse map.	
	    // dinvmap computes the inverse of the Schwarz-Christoffel disk map (i.e., from
	    // a polygon to the disk) at the points given in vector wp.  The other
		// arguments are as in dparam.
		
		// The default algorithm is to solve an ODE in order to obtain a fair
		// approximation for zp, and then improve zp with Newton iterations.
		// The 2 other possbile algorithms are:
		// 1.) Use ODE only.
		// 2.) Use Newton only; take z0 as an initial guess.
		// The ODE solution at wp requires a vector z0 whose forward image w0
		// is such that for each j, the line segment connecting wp[j] and w0[j]
		// lies inside the polygon.  By default z0 is chosen by a fairly robust
		// automatic process.  Using the parameters ode and newton, you can choose
		// to use either an ODE solution or Newton solution exclusively.
		
		// dinvmap has two interpretations.  If the ODE solution is being used,
		// z0 overrides the automatic selection of initial points.  (This can
		// be handy in convex polygons, where the choice of z0 is trivial.)
		// Otherwise, z0 is taken as an initial guess to zp.  In either case,
		// if length(z0) == 1, the value z0 is used for all elements of wp;
		// otherwise, length(z0) should equal length(wp).
		
		// tol, error tolerance for solution (default 1e-8)
	    // maxiter, maximum number of Newton iterations (default 10)
		
		// flag has a vector of indices where the method was unable to produce a
		// sufficiently small residual.  A warning is issued when this occurs.
		
		// Original MATLAB routine copyright 1998 by Toby Driscoll.
		int flag[] = null;
		int i, j, k, m;
		double qdat2[][];
		double zn[][];
		double w0[][];
		double F[][] = null;
		double logreal;
		double logimag;
		double betamult[][][];
		double cr[] = new double[1];
		double ci[] = new double[1];
		double sumbeta[][];
		double expbeta[][];
		double dF[][];
		
		int n = w.length;
		for (i = 0; i < wp.length; i++) {
			zp[i][0] = 0;
			zp[i][1] = 0;
		}
		int lenwp = wp.length;
		
		if ((qdat == null) || (qdat.length == 0)) {
			qdat2 = new double[1][1];
			qdat2[0][0] = tol;
		}
		else if (qdat.length == 1) {
			int nqpts = (int)Math.max(Math.ceil(-Math.log10(qdat[0][0])), 2);
			qdat2 = new double[nqpts][2*beta.length+2];
			scqdata(qdat2, beta, nqpts);	
		}
		else {
			qdat2 = qdat;
		}
		
		boolean done[] = new boolean[wp.length];
		// First, trap all points indistinguishable from vertices, or they will cause
		// trouble.
		// Modified 05/14/2007 to work around bug in matlab 2007a.
		for (j = 0; j < n; j++) {
			int numidx = 0;
			for (i = 0; i < lenwp; i++) {
				if (zabs(wp[i][0] - w[j][0], wp[i][1] - w[j][1]) < 3.0*eps) {
					numidx++;
				}
			} // for (i = 0; i < lenwp; i++)
			int idx[] = new int[numidx];
			for (i = 0, k = 0; i < lenwp; i++) {
				if (zabs(wp[i][0] - w[j][0], wp[i][1] - w[j][1]) < 3.0*eps) {
					idx[k++] = i;
				}
			} // for (i = 0, k = 0; i < lenwp; i++)
			for (i = 0; i < numidx; i++) {
				zp[idx[i]][0] = z[j][0];
				zp[idx[i]][1] = z[j][1];
				done[idx[i]] = true;
			} // for (i = 0; i < numidx; i++)
		} // for (j = 0; j < n; j++)
		int sumdone = 0;
		for (i = 0; i < done.length; i++) {
			if (done[i]) {
				sumdone++;
			}
		} // for (i = 0; i < done.length; i++)
		// lenwp contains number not done
		lenwp = lenwp - sumdone;
		if (lenwp == 0) {
			return null;
		}

		// ODE
		if (ode) {
			double z02[][];
			double w02[][];
			double z03[][];
			double w03[][];
			double wpnotdone[][] = new double[lenwp][2];
			for (i = 0, j = 0; i < done.length; i++) {
				if (!done[i]) {
					wpnotdone[j][0] = wp[i][0];
					wpnotdone[j++][1] = wp[i][1];
				}
			} // for (i = 0, j = 0; i < done.length; i++)
			if ((z0 == null) || (z0.length == 0)) {
			  // Pick a value z0 (not a singularity) and compute the map there.	
				z03 = new double[lenwp][2];
				w03 = new double[lenwp][2];
				scimapz0(z03, w03, "d", wpnotdone, w, beta, z, c, qdat2, null);
			} // if ((z0 == null) || (z0.length == 0)) 
			else {
			    w0 = dmap(z0,w,beta,z,c,qdat2);
			    if ((z0.length == 1) && (lenwp > 1)) {
			        z02 = new double[lenwp][2];
			        w02 = new double[lenwp][2];
			        for (i = 0; i < lenwp; i++) {
			        	z02[i][0] = z0[0][0];
			        	z02[i][1] = z0[0][1];
			        	w02[i][0] = w0[0][0];
			        	w02[i][1] = w0[0][1];
			        }
			    } // if ((z0.length == 1) && (lenwp > 1)
			    else {
			    	w02 = w0;
			    	z02 = z0;
			    }
			    w03 = new double[lenwp][2];
			    z03 = new double[lenwp][2];
			    for (i = 0, j = 0; i < done.length; i++) {
			    	if (!done[i]) {
			    		w03[j][0] = w02[i][0];
			    		w03[j][1] = w02[i][1];
			    		z03[j][0] = z02[i][0];
			    		z03[j++][1] = z02[i][1];
			    	}
			    } // for (i = 0, j = 0; i < done.length; i++)
			} // else
			
			// Use relaxed ODE tol if improving with Newton.
			double odetol;
			if (newton) {
			    odetol = Math.max(tol,  1.0E-4);
			}
			else {
				odetol = tol;
			}
			double abstol = odetol;
			double reltol = odetol;
			
			// Rescale dependent coordinate
			double scale[][] = new double[lenwp][2];
			for (i = 0; i < lenwp; i++) {
				scale[i][0] = wpnotdone[i][0] - w03[i][0];
				scale[i][1] = wpnotdone[i][1] - w03[i][1];
			}
			
			// Solve ODE
			double z04[] = new double[2*z03.length];
			for (i = 0; i < z03.length; i++) {
				z04[i] = z03[i][0];
				z04[i + z03.length] = z03[i][1];
			}
			//double tspan[] = new double[3];
			//tspan[0] = 0;
			//tspan[1] = 0.5;
			//tspan[2] = 1.0;
			//double t[] = new double[tspan.length];
			//double y[][] = new double[tspan.length][z04.length];
			//dimapfun(t, y, tspan, z04, abstol, reltol,scale, z, beta, c);
			// Because ode113.m is copyrighted by MATLAB use a port of the
			// original FORTRAN ode.f code 
			double yarr[][] = new double[3][z04.length];
			for (i = 0; i < z04.length; i++) {
				yarr[0][i] = z04[i];
			}
			double coef = 0.1;
			double t[] = new double[1];
			double relerr[] = new double[1];
			double abserr[] = new double[1];
			int iflag[] = new int[1];
			ODEModel modODE;
			double tout;
			while (true) {
				t[0] = 0;
				tout = 0.5;
				relerr[0] = coef*reltol;
				abserr[0] = coef*abstol;
				iflag[0] = 1;
				for (i = 0; i < z04.length; i++) {
				    z04[i] = yarr[0][i];
				}
				modODE = new ODEModel(z04.length, z04, t, tout, relerr,
						abserr, iflag, scale, z, beta, c);
				modODE.driver();
				System.out.println(modODE.getErrorMessage());
				if ((iflag[0] >= 3) && (iflag[0] <= 5)) {
					System.out.println("Final time reached = " + t[0]);
				}
				if ((iflag[0] != 3) || (coef >= 1024.0)) {
					break;
				}
				coef = 2.0 * coef;
			} // while (true)
			for (i = 0; i < z04.length; i++) {
				yarr[1][i] = z04[i];
			}
			coef = 0.1;
			while (true) {
				t[0] = 0.5;
				tout = 1.0;
				relerr[0] = coef*reltol;
				abserr[0] = coef*abstol;
				iflag[0] = 1;
				for (i = 0; i < z04.length; i++) {
				    z04[i] = yarr[1][i];	
				}
				modODE = new ODEModel(z04.length, z04, t, tout, relerr,
						abserr, iflag, scale, z, beta, c);
				modODE.driver();
				System.out.println(modODE.getErrorMessage());
				if ((iflag[0] >= 3) && (iflag[0] <= 5)) {
					System.out.println("Final time reached = " + t[0]);
				}
				if ((iflag[0] != 3) || (coef >= 1024.0)) {
					break;
				}
				coef = 2.0 * coef;
			} // while (true)
			for (i = 0; i < z04.length; i++) {
				yarr[2][i] = z04[i];
			}
			m = yarr.length;
			int leny = yarr[0].length;
			for (i = 0, j = 0; i < done.length; i++) {
				if (!done[i]) {
				    zp[i][0] = yarr[m-1][j];
					zp[i][1] = yarr[m-1][j + lenwp];
					j++;
				}
			} // for (i = 0, j = 0; i < done.length; i++)
			double abszp;
			for (i = 0; i < zp.length; i++) {
				abszp = zabs(zp[i][0], zp[i][1]);
				if (abszp > 1.0) {
					zp[i][0] = zp[i][0]/abszp;
					zp[i][1] = zp[i][1]/abszp;
				}
			} // for (i = 0; i < zp.length; i++)
		} // if (ode)
		
		// Newton iterations
		if (newton) {
		    if (!ode) {
		        if ((z0.length == 1) & (lenwp > 1)) {
		        	zn = new double[lenwp][2];
		        	for (i = 0; i < lenwp; i++) {
		        		zn[i][0] = z0[0][0];
		        		zn[i][1] = z0[0][1];
		        	}
		        } // if ((z0.length == 1) & (lenwp > 1))
		        else {
		        	zn = new double[z0.length][2];
		        	for (i = 0; i < z0.length; i++) {
		        		zn[i][0] = z0[i][0];
		        		zn[i][1] = z0[i][1];
		        	}
		        } // else
		        for (i = 0; i < done.length; i++) {
		        	if (done[i]) {
		        		zn[i][0] = zp[i][0];
		        		zn[i][1] = zp[i][1];
		        	}
		        }
		    } // if (!ode)
		    else { // ode
		    	zn = new double[zp.length][2];
		    	for (i = 0; i < zp.length; i++) {
		    		zn[i][0] = zp[i][0];
		    		zn[i][1] = zp[i][1];
		    	}
		    } // else ode
		    
		    k = 0;
		    while ((sumdone < done.length) && (k < maxiter)) {
		        m = done.length - sumdone;
		        double znnotdone[][] = new double[m][2];
		        double wpnotdone[][] = new double[m][2];
		        for (i = 0, j = 0; i < done.length; i++) {
		        	if (!done[i]) {
		        		znnotdone[j][0] = zn[i][0];
		        		znnotdone[j][1] = zn[i][1];
		        		wpnotdone[j][0] = wp[i][0];
		        		wpnotdone[j][1] = wp[i][1];
		        		j++;
		        	}
		        } // for (i = 0, j = 0; i < done.length; i++)
		        w0 = dmap(znnotdone, w, beta, z, c, qdat2);
		        F = new double[m][2];
		        for (i = 0; i < m; i++) {
		        	F[i][0] = wpnotdone[i][0] - w0[i][0];
		        	F[i][1] = wpnotdone[i][1] - w0[i][1];
		        }
		        betamult = new double[n][m][2];
		        for (i = 0; i < n; i++) {
		        	for (j = 0; j < m; j++) {
		        		zdiv(znnotdone[j][0], znnotdone[j][1], z[i][0], z[i][1], cr, ci);
		        		logreal = Math.log(zabs(1.0 - cr[0], -ci[0]));
		        		logimag = Math.atan2(-ci[0], 1- cr[0]);
		        		betamult[i][j][0] = beta[i] * logreal;
		        		betamult[i][j][1] = beta[i] * logimag;
		        	}
		        } // for (i = 0; i < n; i++)
		        sumbeta = new double[m][2];
		        for (j = 0; j < m; j++) {
		            for (i = 0; i < n; i++) {
		            	sumbeta[j][0] += betamult[i][j][0];
		            	sumbeta[j][1] += betamult[i][j][1];
		            }
		        } // for (j = 0; j < m; j++)
		        expbeta = new double[m][2];
		        for (i = 0; i < m; i++) {
		        	double var = Math.exp(sumbeta[i][0]);
		        	expbeta[i][0] = var*Math.cos(sumbeta[i][1]);
		        	expbeta[i][1] = var*Math.sin(sumbeta[i][1]);
		        } // for (i = 0; i < m; i++)
		        dF = new double[m][2];
		        for (i = 0; i < m; i++) {
		        	zmlt(c[0], c[1], expbeta[i][0], expbeta[i][1], cr, ci);
		        	dF[i][0] = cr[0];
		        	dF[i][1] = ci[0];
		        } // for (i = 0; i < m; i++)
		        for (i = 0, j = 0; i < done.length; i++) {
		        	if (!done[i]) {
		        		zdiv(F[j][0], F[j][1], dF[j][0], dF[j][1], cr, ci);
		        		zn[i][0] = zn[i][0] + cr[0];
		        		zn[i][1] = zn[i][1] + ci[0];
		        		j++;
		        	}
		        } // for (i = 0, j = 0; i < done.length; i++)
		        for (i = 0; i < zn.length; i++) {
		        	double znabs = zabs(zn[i][0], zn[i][1]);
		        	if (znabs > 1) {
		        		zn[i][0] = zn[i][0]/znabs;
		        		zn[i][1] = zn[i][1]/znabs;
		        	}
		        }
		        for (i = 0, j = 0; i < done.length; i++) {
		        	if (!done[i]) {
		        		if (zabs(F[j][0], F[j][1]) < tol) {
		        			done[i] = true;
		        			sumdone++;
		        		}
		        		j++;
		        	}
		        } // for (i = 0, j = 0; i < done.length; i++)
		        k = k + 1;
		    } //  while ((sumdone < done.length) && (k < maxiter))
		    double maxtol = 0.0;
		    if (F != null) {
			    for (i = 0; i < F.length; i++) {
			    	double currentTol = zabs(F[i][0], F[i][1]);
			    	if (currentTol > maxtol) {
			    		maxtol = currentTol;
			    	}
			    }
		    } // if (F != null)
		    if (maxtol > tol) {
		    	MipavUtil.displayWarning("Check solution: maximum residual = " + maxtol);
		    }
		    for (i = 0; i <zn.length; i++) {
		    	zp[i][0] = zn[i][0];
		    	zp[i][1] = zn[i][1];
		    }
		} // if (newton)
		int numnotdone = 0;
		for (i = 0; i < done.length; i++) {
			if (!done[i]) {
			    numnotdone++;	
			}
		} // for (i = 0; i < done.length; i++)
		flag = new int[numnotdone];
		for (i = 0, j = 0; i < done.length; i++) {
			if (!done[i]) {
				flag[j++] = i;
			}
		} // for (i = 0, j = 0; i < done.length; i++)
		return flag;
	}
	
	class ODEModel extends ODE {
		double scale[][];
		double z[][];
		double beta[];
		double c[];
		public ODEModel(int neqn, double y[], double t[], double tout, double relerr[],
				double abserr[], int iflag[], double scale[][], double z[][], 
				double beta[], double c[]) {
			super(neqn, y, t, tout, relerr, abserr, iflag);
			this.scale = scale;
			this.z = z;
			this.beta = beta;
			this.c = c;
			
		}
		
		/**
		 * DOCUMENT ME!
		 */
		public void driver() {
			super.driver();
		}
		
		public void f(double x, double yy[], double yp[]) {
			int i;
			double cr[] = new double[1];
			double ci[] = new double[1];
			int lenyy = yy.length;
			int lenzp = lenyy/2;
			double zp[][] = new double[lenzp][2];
			for (i = 0; i < lenzp; i++) {
				zp[i][0] = yy[i];
			}
			for (i = lenzp; i < lenyy; i++) {
				zp[i-lenzp][1] = yy[i];
			}
			
			double fprime[][] = dderiv(zp, z, beta, c);
			double f[][] = new double[scale.length][2];
			for (i = 0; i < scale.length; i++) {
				zdiv(scale[i][0], scale[i][1], fprime[i][0], fprime[i][1], cr, ci);
				f[i][0] = cr[0];
				f[i][1] = ci[0];
			}
			
			for (i = 0; i < f.length; i++) {
				yp[i] = f[i][0];
				yp[f.length + i] = f[i][1];
			}	
		}
	}
	
	private void dimapfun(double t[], double y[][], double wp[], double yp[], double abstol, double reltol,
			double scale[][], double z[][], double beta[], double c[]) {
		// Used by dinvmap for the solution of an ODE.
		// t is the vector of output times
		// For each time the y values y1 to yn of the differential equations
		// in different columns
		// wp are the time span
		// yp are the intial conditions
		// zdot is a column vector dy1/dt, dy2/dt,...,dyn/dt
		// Original MATLAB dimapfun copyright 1998 by Toby Driscoll
		int i;
		double cr[] = new double[1];
		double ci[] = new double[1];
		int lenyp = yp.length;
		int lenzp = lenyp/2;
		double zp[][] = new double[lenyp][2];
		for (i = 0; i < lenzp; i++) {
			zp[i][0] = yp[i];
		}
		for (i = lenzp; i < lenyp; i++) {
			zp[i][1] = yp[i];
		}
		
		double fprime[][] = dderiv(zp, z, beta, c);
		double f[][] = new double[scale.length][2];
		for (i = 0; i < scale.length; i++) {
			zdiv(scale[i][0], scale[i][1], fprime[i][0], fprime[i][1], cr, ci);
			f[i][0] = cr[0];
			f[i][1] = ci[0];
		}
		
		double zdot[] = new double[2*f.length];
		for (i = 0; i < f.length; i++) {
			zdot[i] = f[i][0];
			zdot[f.length + i] = f[i][1];
		}
	}
	
	private double[][] dderiv(double zp[][], double z[][], double beta[], double c[]) {
		// Derivative of the disk map.
		// dderiv returns the derivative at the points of zp of the 
		// Schwarz-Christoffel disk map defined by z, beta, and c.
		// Original MATLAB routine copyright 1998 by Toby Driscoll.
		int i,j;
		double cr[] = new double[1];
		double ci[] = new double[1];
		double zprow[][] = new double[zp.length][2];
		for (i = 0; i < zp.length; i++) {
			zprow[i][0] = zp[i][0];
			zprow[i][1] = zp[i][1];
		}
		double fprime[][] = new double[zp.length][2];
		
		int npts = zp.length;
		double terms[] = new double[2];
		double logterms[] = new double[2];
		double betaterms[] = new double[2];
		double sumterms[][] = new double[npts][2];
		for (j = 0; j < npts; j++) {
		    for (i = 0; i < beta.length; i++) {
				zdiv(zprow[j][0], zprow[j][1], z[i][0], z[i][1], cr, ci);
				terms[0] = 1 - cr[0];
				terms[1] = -ci[0];
				logterms[0] = Math.log(zabs(terms[0], terms[1]));
				logterms[1] = Math.atan2(terms[1],terms[0]);
				betaterms[0] = logterms[0] * beta[i];
				betaterms[1] = logterms[1] * beta[i];
				sumterms[j][0] += betaterms[0];
				sumterms[j][1] += betaterms[1];
			}
		}
		
		for (i = 0; i < npts; i++) {
			double expterm = Math.exp(sumterms[i][0]);
			zmlt(c[0], c[1],expterm*Math.cos(sumterms[i][1]), 
					expterm*Math.sin(sumterms[i][1]), cr, ci);
			fprime[i][0] = cr[0];
			fprime[i][1] = ci[0];
		}
	    
		return fprime;
	}
	
	private void scimapz0(double z0[][], double w0[][], String prefix, double wp[][],
			double w[][], double beta[], double z[][], double c[], double qdat[][],
			double aux[][]) {
	    // scimapz0 returns starting points for computing inverses of 
		// Schwarz-Christoffel maps.
		
		// Each wp[j] (in the polygon plane) requires z0[j] (in the fundamental domain)
		// whose image w0[j] is such that the line segment from w0[j] to wp[j] lies in
		// the target (interior or exterior) region.  The algorithm here is to choose
		// z0[j] as a (weighted) average of successive pairs of adjacent prevertices.
		// The resulting w0[j] is on a polygon side.  Each choice is tested by looking
		// for intersections of the segment with (other) sides of the polygon.
		
		// After randomly trying 10 weights with such prevertex pairs, the routine gives
		// up.  Failures are pretty rare.  Slits are the most likely cause of trouble,
		// since  the intersection method doesn't know "which side" of the slit it's on.
		// In such a case you will have to supply starting points manually, perhaps by
		// a continuation method.
		
		// See also hpinvmap, dinvmap, deinvmap, rinvmap, stinvmap.
		
		// Original MATLAB routine copyright 1997 by Toby Driscoll.  Last updated 05/07/97.
		
		// P.S.  This file illustrates why the different domains in the SC toolbox
		// have mostly independent M-files.  The contingencies for the various
		// geometries become rather cumbersome.
		
		int i,j,k,p;
		double cr[] = new double[1];
		double ci[] = new double[1];
		int n = w.length;
		boolean from_disk = false;
		boolean from_hp = false;
		boolean from_strip = false;
		boolean from_rect = false;
		double shape[][] = new double[wp.length][2];
		for (i = 0; i < wp.length; i++) {
			shape[i][0] = wp[i][0];
			shape[i][1] = wp[i][1];
			z0[i][0] = wp[i][0];
			z0[i][1] = wp[i][1];
			w0[i][0] = wp[i][0];
			w0[i][1] = wp[i][1];
		}
		if (prefix.substring(0,1).equalsIgnoreCase("d")) {
			from_disk = true;
		}
		else if (prefix.equalsIgnoreCase("hp")) {
			from_hp = true;
		}
		else if (prefix.equalsIgnoreCase("st")) {
			from_strip = true;
		}
		else if (prefix.equalsIgnoreCase("r")) {
			from_rect = true;
		}
		double w2[][];
		double z2[][];
		double beta2[];
		double qdat2[][];
		double argw[] = new double[n];
		int kinf = -1;
		// Calculate arguments of the directed polygon edges.
		if (from_strip) {
		    // Renumber to put left end of strip first
			int numinf = 0;
			for (i = 0; i < z.length; i++) {
				if (Double.isInfinite(z[i][0]) || (Double.isInfinite(z[i][1]))) {
					numinf++;
				}
			}
			int atinf[] = new int[numinf];
			for (i = 0, j = 0; i < z.length; i++) {
				if (Double.isInfinite(z[i][0]) || (Double.isInfinite(z[i][1]))) {
					atinf[j++] = i;
				}
			}
			int renum[] = new int[n];
			for (i = 0; i < n-atinf[0]; i++) {
				renum[i] = i+atinf[0];
			}
			for (i = n-atinf[0]; i < n; i++) {
				renum[i] = i-n+atinf[0];
			}
			w2 = new double[n][2];
			z2 = new double[n][2];
			beta2 = new double[n];
			qdat2 = new double[qdat.length][2*n+1];
			for (i = 0; i < n; i++) {
				w2[i][0] = w[renum[i]][0];
				w2[i][1] = w[renum[i]][1];
				z2[i][0] = z[renum[i]][0];
				z2[i][1] = z[renum[i]][1];
				if (Double.isInfinite(z2[i][0]) || Double.isInfinite(z2[i][1])) {
				    kinf = i;	
				}
				beta2[i] = beta[renum[i]];
				for (j = 0; j < qdat.length; j++) {
					qdat2[j][i] = qdat[j][renum[i]];
					qdat2[j][n+1+i] = qdat[j][n+1+renum[i]];
				}
			} // for (i = 0; i < n; i++)
			double diffw[] = new double[2];
			diffw[0] = w[2][0] - w[1][0];
			diffw[1] = w[2][1] - w[1][1];
			double argwp[] = new double[n];
			argwp[0] = Math.atan2(diffw[1], diffw[0]);
			for (i = 1; i < n-1; i++) {
				argwp[i] = argwp[i-1] - Math.PI*beta[i+1];
			}
			argwp[n-1] = argwp[n-2] - Math.PI*beta[0];
			argw[0] = argwp[n-1];
			for (i = 1; i < n; i++) {
				argw[i] = argwp[i-1];
			}
		} // if (from_strip)
		else {
			double diffw[] = new double[2];
			diffw[0] = w[1][0] - w[0][0];
			diffw[1] = w[1][1] - w[0][1];
			argw[0] = Math.atan2(diffw[1], diffw[0]);
			for (i = 1; i < n; i++) {
				argw[i] = argw[i-1] - Math.PI*beta[i];
			}
			w2 = w;
			z2 = z;
			beta2 = beta;
			qdat2 = qdat;
		} // else
		
		// Express each side in a form to allow detection of intersections.
		boolean infty[] = new boolean[n];
		for (i = 0; i < n; i++) {
			if (Double.isInfinite(w[i][0]) || Double.isInfinite(w[i][1])) {
				infty[i] = true;
			}
		} // for (i = 0; i < n; i++)
		int fwd[] = new int[n];
		for (i = 0; i < n-1; i++) {
			fwd[i] = i+1;
		}
		fwd[n-1] = 0;
		double anchor[][] = new double[n][2];
		for (i = 0; i < n; i++) {
			if (!infty[i]) {
				anchor[i][0] = w[i][0];
				anchor[i][1] = w[i][1];
			}
			else {
				anchor[i][0] = w[fwd[i]][0];
				anchor[i][1] = w[fwd[i]][1];  // Use the finite endpoint
			}
		} // for (i = 0; i < n; i++)
		double direcn[][] = new double[n][2];
		for (i = 0; i < n; i++) {
			direcn[i][0] = Math.cos(argw[i]);
			direcn[i][1] = Math.sin(argw[i]);
			if (infty[i]) {
				// reverse
				direcn[i][0] = -direcn[i][0];
				direcn[i][1] = -direcn[i][1];
			}
		} // for (i = 0; i < n; i++)
		double len[] = new double[n];
		for (i = 0; i < n; i++) {
			len[i] = zabs(w[fwd[i]][0] - w[i][0], w[fwd[i]][1] - w[i][1]);
		}
		
		double argz[] = new double[n];
		if (from_disk) {
			for (i = 0; i < n; i++) {
				argz[i] = Math.atan2(z[i][1], z[i][0]);
				if (argz[i] <= 0) {
					argz[i] = argz[i] + 2.0 * Math.PI;
				}
			}
		} // if (from_disk)
		
		double L[] = null;
		if (from_rect) {
			// Extra argument given
			L = qdat[0];
			qdat2 = aux;
		}
		
		double factor = 0.5;  // images of midpoints of preverts
		boolean done[] = new boolean[wp.length];
		int m = wp.length;
		double wpnotdone[][];
		int iter = 0;
		double tol;
		if (qdat2.length > 1) {
		    tol = 1000 * Math.pow(10.0, -qdat2.length);	
		}
		else {
			tol = qdat2[0][0];
		}
		
		double zbase[][] = new double[n][2];
		double wbase[][] = new double[n][2];
		for (i = 0; i < n; i++) {
			zbase[i][0] = Double.NaN;
			wbase[i][0] = Double.NaN;
		}
		int idx[] = null;
		boolean active[] = new boolean[done.length];
		int numactive;
		double A[][] = new double[2][2];
		double dif[] = new double[2];
		double Ainv[][] = new double[2][2];
		double s[] = new double[2];
		RandomNumberGen randomGen = new RandomNumberGen();
		while (m > 0) {  // while some not done
			// Choose a "basis point" on each side of the polygon.
			for (j = 0; j < n; j++) {
				if (from_disk) {
					if (j < n-1) {
						zbase[j][0] = Math.cos(factor*argz[j] + (1-factor)*argz[j+1]);
						zbase[j][1] = Math.sin(factor*argz[j] + (1-factor)*argz[j+1]);
					}
					else {
						zbase[j][0] = Math.cos(factor*argz[n-1] 
								+ (1-factor)*(2.0*Math.PI + argz[0]));
						zbase[j][1] = Math.sin(factor*argz[n-1] 
								+ (1-factor)*(2.0*Math.PI + argz[0]));
					}
				} // if (from_disk)
				else if (from_hp) {
					if (j < n-2) { // between two finite points
						zbase[j][0] = z2[j][0] + factor*(z2[j+1][0] - z2[j][0]);
						zbase[j][1] = z2[j][1] + factor*(z2[j+1][1] - z2[j][1]);
					}
					else if (j == n-2) { // between x[n-2] & Inf
						if (10.0 >= zabs(z2[n-2][0], z2[n-2][1])) {
							zbase[j][0] = 10.0/factor;
							zbase[j][1] = 0.0;
						}
						else { 
							zbase[j][0] = z2[n-2][0]/factor;
							zbase[j][1] = z2[n-2][1]/factor;
						}
					} // else if (j == n-2)
					else { // between -Inf and x[0]
						if (-10.0 <= zabs(z2[0][0],z2[0][1])) {
							zbase[j][0] = -10.0/factor;
						    zbase[j][1] = 0.0;	
						}
						else {
							zbase[j][0] = z2[0][0]/factor;
							zbase[j][1] = z2[0][1]/factor;
						}
					}
				} // else if (from_hp)
				else if (from_strip) {
				    if (j == 0) {
			    		zbase[j][0] = Math.min(-1, z2[1][0])/factor;
			    		zbase[j][1] = 0.0;
				    } // if (j == 0)
				    else if (j == kinf-1) {
				        zbase[j][0] = Math.max(1,z2[kinf-1][0])/factor;
				        zbase[j][1] = 0.0;
				    } // else if (j == kinf-1)
				    else if (j == kinf) {
				    	zbase[j][0] = Math.max(1,z2[kinf+1][0])/factor;
				    	zbase[j][1] = 1.0;
				    } // else if (j == kinf)
				    else if (j == n-1) {
				    	zbase[j][0] = Math.min(-1,z2[n-1][0])/factor;
				    	zbase[j][1] = 1.0;
				    } // else if (j == n-1)
				    else {
				    	zbase[j][0] = z2[j][0] + factor*(z2[j+1][0] - z2[j][0]);
				    	zbase[j][1] = z2[j][1] + factor*(z2[j+1][1] - z2[j][1]);
				    }
				} // else if (from_strip)
				else if (from_rect) {
					int rem = (j+1)%n;
					zbase[j][0] = z2[j][0] + factor * (z2[rem][0] - z2[j][0]);
					zbase[j][1] = z2[j][1] + factor * (z2[rem][1] - z2[j][1]);
					double maximagz2 = -Double.MAX_VALUE;
					for (i = 0; i < z2.length; i++) {
						if (z2[i][1] > maximagz2) {
							maximagz2 = z2[i][1];
						}
					}
					// Can't use 0 or iK' as basis points.
					if (zabs(zbase[j][0], zbase[j][1]) < 1.0E-4) {
						zbase[j][1] = zbase[j][1] + 0.2;
					}
					else if (zabs(zbase[j][0],zbase[j][1] - maximagz2) < 1.0E-4) {
						zbase[j][1] = zbase[j][1] - 0.2;
					}
				} // else if (from_rect)
				
				// Find images of all the z-plane basis points.
				if (from_rect) {
					double neww[][] = new double[1][2];
					double zpnew[][] = new double[1][2];
					zpnew[0][0] = zbase[j][0];
					zpnew[0][1] = zbase[j][1];
					rmap(neww, zpnew, w2, beta2, z2, c, L, qdat2);
					wbase[j][0] = neww[0][0];
					wbase[j][1] = neww[0][1];
				} // if (from_rect)
				else if (from_disk) {
					double zpnew[][] = new double[1][2];
					zpnew[0][0] = zbase[j][0];
					zpnew[0][1] = zbase[j][1];
					double neww[][] = dmap(zpnew, w2 , beta2, z2, c, qdat2);
					wbase[j][0] = neww[0][0];
					wbase[j][1] = neww[0][1];
				} // else if (from_disk)
				else if (from_hp) {
				  // Fill in hpmap	
				} // else if (from_hp)
				else if (from_strip) {
				  // Fill in stmap	
				} // else if (from_strip)
				
				// Project each base point exactly to the nearest polygon side.
				// This is needed to make intersection detection go smoothly in
				// borderline cases.
				zmlt(wbase[j][0] - anchor[j][0], wbase[j][1] - anchor[j][1],
						direcn[j][0], -direcn[j][1], cr, ci);
				double proj = cr[0];
				wbase[j][0] = anchor[j][0] + proj*direcn[j][0];
				wbase[j][1] = anchor[j][1] + proj*direcn[j][1];
			} // from (j = 0; j < n; j++)
			
			if ((idx == null) || (idx.length == 0)) {
				// First time through, assign nearest basis point to each image point.
				double dist[] =  new double[m];
				idx = new int[m];
				wpnotdone = new double[m][2];
				for (i = 0, j = 0; i < done.length; i++) {
					if (!done[i]) {
						wpnotdone[j][0] = wp[i][0];
						wpnotdone[j++][1] = wp[i][1];
					}
				}
				for (i = 0; i < m; i++) {
					dist[i] = Double.MAX_VALUE;
					idx[i] = -1;
					for (j = 0; j < n; j++) {
						double currentDist = zabs(wpnotdone[i][0] - wbase[j][0],
								wpnotdone[i][1] - wbase[j][1]);
						if (currentDist < dist[i]) {
							dist[i] = currentDist;
							idx[i] = j;
						}
					}
				} // for (i = 0; i < m; i++)
			} // if ((idx == null) || (idx.length == 0))
			else {
				// Other times, just change those that failed.
				for (i = 0; i < done.length; i++) {
					if (!done[i]) {
						idx[i] = ((idx[i]+1)%n);
					}
				} // for (i = 0; i < done.length; i++)
			} // else
			for (i = 0; i < done.length; i++) {
				if (!done[i]) {
					z0[i][0] = zbase[idx[i]][0];
					z0[i][1] = zbase[idx[i]][1];
					w0[i][0] = wbase[idx[i]][0];
					w0[i][1] = wbase[idx[i]][1];
				}
			} // for (i = 0; i < done.length; i++)
			
			// Now, cycle thru basis points
			loopj: for (j = 0; j < n; j++) {
				// Those points who come from basis j and need checking.
				numactive = 0;
				for (i = 0; i < done.length; i++) {
				   active[i] = (idx[i] == j)	&& (!done[i]);
				   if (active[i]) {
					   numactive++;
				   }
				} // for (i = 0; i < done.length; i++)
				if (numactive > 0) {
					// Assume for now that it's good
					for (i = 0; i < done.length; i++) {
						if (active[i]) {
							done[i] = true;
						}
					} // for (i = 0; i < done.length; i++)
					// Test line segment for intersections with other sides.
					// We'll parameterize line segment and polygon side, compute
					// parameters at intersection, and check parameters at intersection.
					for (k = 0; k < n; k++) {
						if (k != j) {
						    A[0][0] = direcn[k][0];
						    A[1][0] = direcn[k][1];
						    for (p = 0; p < active.length; p++) {
						    	if (active[p]) {
						    	    dif[0] = w0[p][0] - wp[p][0];
						    	    dif[1] = w0[p][1] - wp[p][1];
						    	    A[0][1] = dif[0];
						    	    A[1][1] = dif[1];
						    	    // Get line segment and side parameters at intersection.
						    	    LinearEquations2 le2 = new LinearEquations2();
						    	    GeneralizedEigenvalue ge = new GeneralizedEigenvalue();
						    	    double anorm;
						    	    double rcond[] = new double[1];
						    	    double work[] = new double[2];
						    	    int iwork[] = new int[2];
						    	    int info[] = new int[1];
						    	    int ipiv[] = new int[2];
						    	    anorm = ge.dlange('1', 2, 2, A, 2, work);
						    	    le2.dgetrf(2, 2, A, 2, ipiv, info);
						    	    if (info[0] > 0) {
						    	    	MipavUtil.displayError("In dgetrf factor U is exactly singular");
						    	    }
						    	    le2.dgecon('1', 2, A, 2, anorm, rcond, work, iwork, info);
						    	    // Reciprocal condition number of A in 1-norm.
						    	    if (rcond[0] < eps) {
						    	        // All 4 involved points are collinear
						    	    	zdiv(wp[p][0] - anchor[k][0], wp[p][1] - anchor[k][1],
						    	    			direcn[k][0], direcn[k][1], cr, ci);
						    	    	double wpx = cr[0];
						    	    	zdiv(w0[p][0] - anchor[k][0], w0[p][1] - anchor[k][1],
						    	    			direcn[k][0], direcn[k][1], cr, ci);
						    	    	double w0x = cr[0];
						    	    	if ((wpx*w0x < 0) || (((wpx-len[k])*(w0x-len[k])) < 0)) {
						    	    		// Intersection interior to segment: it's no good
						    	    		done[p] = false;
						    	    	}
						    	    } // if (rcond[0] < eps)
						    	    else {
						    	        dif[0] = w0[p][0] - anchor[k][0];
						    	        dif[1] = w0[p][1] - anchor[k][1];
						    	        double detA = A[0][0]*A[1][1] - A[0][1]*A[1][0];
						    	        Ainv[0][0] = A[1][1]/detA;
						    	        Ainv[0][1] = -A[0][1]/detA;
						    	        Ainv[1][0] = -A[1][0]/detA;
						    	        Ainv[1][1] = A[0][0]/detA;
						    	        s[0] = Ainv[0][0]*dif[0] + Ainv[0][1]*dif[1];
						    	        s[1] = Ainv[1][0]*dif[0] + Ainv[1][1]*dif[1];
						    	        // Intersection occurs interior to side? and segment?
						    	        if ((s[0] >= 0) && (s[0] <= len[k])) {
						    	            if (Math.abs(s[1] - 1) < tol) {
						    	                // Special case: wp[p] is on polygon side k
						    	            	z0[p][0] = zbase[k][0];
						    	            	z0[p][1] = zbase[k][1];
						    	            	w0[p][0] = wbase[k][0];
						    	            	w0[p][1] = wbase[k][1];
						    	            } // if (Math.abs(s[1] - 1) < tol)
						    	            else if (Math.abs(s[1]) < tol) {
						    	                // Happens when two sides are partially coincident
						    	            	// (slit)
						    	            	// Check against normal to that side
						    	            	zmlt(wp[p][0] - w0[p][0], -(wp[p][1] - w0[p][1]),
						    	            			-direcn[k][1], direcn[k][0], cr, ci);
						    	            	if (cr[0] > 0) {
						    	            		// Line segment came from "outside"
						    	            		done[p] = false;
						    	            	} // if (cr[0] > 0)
						    	            } // else if (Math.abs(s[1]) < tol)
					    	            	else if ((s[1] > 0) && (s[1] < 1)) {
					    	            	    // Intersection interior to segment:
					    	            		// it's no good
					    	            		done[p] = false;
					    	            	} // else if ((s[1] > 0) && (s[1] < 1))
						    	        } // if ((s[0] >= 0) && (s[0] <= len[k]))
						    	    } // else
						    	} // if (active[p])
						    } // for (p = 0; p < active.length; p++)
						} // if (k != j)
					} // for (k = 0; k < n; k++)
					
					// Short circuit if everything is finished
					m = 0;
					for (i= 0; i < done.length; i++) {
						if (!done[i]) {
							m++;
						}
					} // for (i= 0; i < done.length; i++)
					if (m == 0) {
					    break loopj;	
					} // if (m == 0)
				} // if (numactive > 0)
			} // for (j = 0; j < n; j++)
			if (iter > 2*n) {
				MipavUtil.displayError("Can't seem to choose starting points.  Supply them manually");
				return;
			}
			else {
				iter = iter + 1;
			}
			// Abandon midpoints
			factor = randomGen.genUniformRandomNum(0.0, 1.0);
		} // while (m > 0)
 	}
	
	private double[][] dmap(double zp[][], double w[][], double beta[],
			double z[][], double c[], double qdat[][]) {
		// Schwarz-christoffel disk map.
		// dmap computes the values of the Schwarz-Christoffel disk map at the
		// points in the vector zp.  The arguments w, beta, z, c, and qdat are
		// as in dparam.  dmap returns a vector wp the same size as zp.
		
		// Original MATLAB routine copyright 1998 by Toby Driscoll.
		int i, j;
		double cr[] = new double[1];
		double ci[] = new double[1];
		double tol;
		if ((zp == null) || (zp.length == 0)) {
			return null;
		}
		
		int n = z.length;
		
		// Quadrature data and error tolerance
		if ((qdat == null) || (qdat.length == 0)) {
		    tol =- 1.0e-8;  
		    qdat = new double[8][2*beta.length+2];
			scqdata(qdat, beta, 8);
		}
		else {
			tol = Math.pow(10.0, -qdat.length);
		}
		//int shape = zp.length;
		int p = zp.length;
		double wp[][] = new double[p][2];
		
		// For each point in zp, find the nearest prevertex
		double dist[] = new double[p];
		int sing[] = new int[p]; // Indices of prevertices
		for (i = 0; i < p; i++) {
			dist[i] = Double.MAX_VALUE;
			sing[i] = -1;
			for (j = 0; j < n; j++) {
				double currentDist = zabs(zp[i][0] - z[j][0],zp[i][1] - z[j][1]);
				if (currentDist < dist[i]) {
					dist[i] = currentDist;
					sing[i] = j;
				}
			}
		} // for (i = 0; i < p; i++)
		
		// Screen out images of prevertices
		boolean vertex[] = new boolean[p];
		for (i = 0; i < p; i++) {
			if (dist[i] < tol) {
				vertex[i] = true;
				wp[i][0] = w[sing[i]][0];
				wp[i][1] = w[sing[i]][1];
			}
		} // for (i = 0; i < p; i++)
		
		// "Bad" points are closest to a prevertex of infinity.
		boolean atinf[] = new boolean[n];
		for (i = 0; i < n; i++) {
			if (Double.isInfinite(w[i][0]) || Double.isInfinite(w[i][1])) {
				atinf[i] = true;
			}
		} // for (i = 0; i < w.length; i++)
		
		boolean bad[] = new boolean[p];
		int numbad = 0;
		for (i = 0; i < p; i++) {
			if ((atinf[sing[i]]) && (!vertex[i])) {
				bad[i] = true;
				numbad++;
			}
		} // for (i = 0; i < p; i++)
		
		double wc[] = new double[2];
		if (numbad > 0) {
			// Can't integrate starting at pre-infinity: find conformal center to use
			// as integration basis.
			double z1[][] = new double[1][2];
			double z2[][] = new double[1][2];
			int sing1[] = new int[1];
			double I1[][];
			if ((!Double.isInfinite(w[n-2][0])) && (!Double.isInfinite(w[n-2][1]))) {
			    z1[0][0] = z[n-2][0];
			    z1[0][1] = z[n-2][1];   
			    sing1[0] = n-2;
			    I1 = dquad(z1,z2,sing1, z, beta, qdat);
			    zmlt(c[0], c[1], I1[0][0], I1[0][1], cr, ci);
			    wc[0] = w[n-2][0] + cr[0];
			    wc[1] = w[n-2][1] + ci[0];
			}
			else {
				z1[0][0] = z[n-1][0];
			    z1[0][1] = z[n-1][1];   
			    sing1[0] = n-1;
			    I1 = dquad(z1,z2,sing1, z, beta, qdat);
			    zmlt(c[0], c[1], I1[0][0], I1[0][1], cr, ci);
			    wc[0] = w[n-1][0] + cr[0];
			    wc[1] = w[n-1][1] + ci[0];	
			}
		} // if (numbad > 0)
		
		// zs = the starting singularities
		double zs[][] = new double[p][2];
		double ws[][] = new double[p][2];
		for (i = 0; i < p; i++) {
			zs[i][0] = z[sing[i]][0];
			zs[i][1] = z[sing[i]][1];
			ws[i][0] = w[sing[i]][0];
			ws[i][1] = w[sing[i]][1];
		}
		
		// Compute the map directly at "normal" points.
		boolean normal[] = new boolean[p];
		int numnormal = 0;
		for (i = 0; i < p; i++) {
			normal[i] = (!bad[i]) && (!vertex[i]);
			if (normal[i]) {
				numnormal++;
			}
		}
		double I[][];
		if (numnormal > 0) {
		    double zsnormal[][] = new double[numnormal][2];
		    double zpnormal[][] = new double[numnormal][2];
		    int singnormal[] = new int[numnormal];
		    for (i = 0, j = 0; i < p; i++) {
		    	if (normal[i]) {
		    		zsnormal[j][0] = zs[i][0];
		    		zsnormal[j][1] = zs[i][1];
		    		zpnormal[j][0] = zp[i][0];
		    		zpnormal[j][1] = zp[i][1];
		    		singnormal[j++] = sing[i];
		    	}
		    } // for (i = 0, j = 0; i < p; i++)
		    I = dquad(zsnormal, zpnormal, singnormal, z,beta, qdat);
		    for (i = 0, j = 0; i < p; i++) {
		    	if (normal[i]) {
		    	    zmlt(c[0], c[1], I[j][0], I[j][1], cr, ci);
		    	    j++;
		    	    wp[i][0] = ws[i][0] + cr[0];
		    	    wp[i][1] = ws[i][1] + ci[0];
		    	}
		    } // for (i = 0, j = 0; i < p; i++)
		} // if (numnormal > 0)
		
		// Compute map at "bad" points, using conformal center as basis, to avoid
		// integration where right endpoint is too close to a singularity.
		if (numbad > 0) {
	        double zpbad[][] = new double[numbad][2];
	        for (i = 0, j = 0; i < p; i++) {
	        	if (bad[i]) {
	        		zpbad[j][0] = zp[i][0];
	        		zpbad[j++][1] = zp[i][1];
	        	}
	        }
	        double zerosbad[][] = new double[numbad][2];
	        int singbad[] = new int[numbad];
	        for (i = 0; i < numbad; i++) {
	        	singbad[i] = -1;
	        }
	        I = dquad(zpbad, zerosbad, singbad, z, beta, qdat);
	        for (i = 0, j = 0; i < p; i++) {
	        	if (bad[i]) {
	        		zmlt(c[0], c[1], I[j][0], I[j][1], cr, ci);
	        		j++;
	        		wp[i][0] = wc[0] - cr[0];
	        		wp[i][1] = wc[1] - ci[0];
	        	}
	        } // for (i = 0, j = 0; i < p; i++)
		} // if (numbad > 0) 
		return wp;
	}
	
	public scmap rectmap(double w[][], int corner[], double tolerance,
			double z[][], double c[], double L[]) {
		// Schwarz-Christoffel rectangle map object
		// rectmap constructs a Schwarz-Christoffel rectangle map object for the polygon
		// whose vertices are given by w.  corner is a 4 integer vector containing the
		// indices of the vertices that are the images of the rectangle's corners.
		// These indices should be specified in counterclockwise order, and the first
		// two should be the endpoints of a long side of the rectangle.
		// If corner is null, rectmap requires you to choose the corners graphically.
		// If w, z, c, and L are given, rectmap constructs a rectmap using the
		// explicitly given prevertices z[][], constant c[0], and strip length L[0].
		// Original MATLAB routine copyright 1998-2001 by Toby Driscoll.
		int i;
		double qdata[][] = null;
		double wn[][] = null;
		double betan[] = null;
		int cornern[] = null;
		double z0[][] = null;
		//rparam(double z[][], double c[], double L[], double qdat[][], 
				//double w[][], double beta[], int cnr[], double z0[][], double tol)
		// z, c, L, and qdat are outputs
		// w, beta, cnr, z0, and tolerance are inputs
		double x[] = new double[w.length];
		double y[] = new double[w.length];
		for (i = 0; i < w.length; i++) {
			x[i] = w[i][0];
			y[i] = w[i][1];
		}
		polygon poly = new polygon(x, y, null);
		double beta[] = new double[poly.angle.length];
		for (i = 0; i < poly.angle.length; i++) {
			beta[i] = poly.angle[i] - 1.0;
		}
		if ((z == null) || (z.length == 0)) {
			if ((corner == null) || (corner.length == 0)) {
				String msg[] = new String[2];
		        msg[0] = "Select the images of the corners of the rectangle";
		        msg[1] = "Go in counerclockwise order and select a long rectangle edge first";
		        corner = scselect(w, beta, 4, "Select corners", msg);
			} // if ((corner == null) || (corner.length == 0))
			
			// Apply scfix to enforce solver rules
			// For type "r" wn and betan will stay the same in length or increase by one
			wn = new double[w.length+1][2];
			betan = new double[w.length+1];
			cornern = new int[corner.length];
			// Number of vertices added by scfix
			int verticesAdded[] = new int[1];
			int initialVertices = w.length;
			scfix(wn, betan, verticesAdded, cornern, "r", w, beta, corner);
			double wn2[][];
			double betan2[];
			if (verticesAdded[0] == 0) {
			    wn2 = new double[initialVertices][2];
				betan2 = new double[initialVertices];
				for (i = 0; i < initialVertices; i++) {
					wn2[i][0] = wn[i][0];
					wn2[i][1] = wn[i][1];
					betan2[i] = betan[i];
				}
			}
			else {
				wn2 = wn;
				betan2 = betan;
			}
			double xn[] = new double[wn2.length];
			double yn[] = new double[wn2.length];
			for (i = 0; i < wn2.length; i++) {
				xn[i] = wn2[i][0];
				yn[i] = wn2[i][1];
			}
			double alpha[] = new double[betan2.length];
			for (i = 0; i < betan2.length; i++) {
				alpha[i] = betan2[i] + 1.0;
			}
			poly = new polygon(xn, yn, alpha);
		    L = new double[2];
			c = new double[2];
			z = new double[wn2.length][2];
			int nqpts = Math.max((int)Math.ceil(-Math.log10(tolerance)), 4);
			qdata = new double[nqpts][2*betan2.length+2];
			// Solve parameter problem (always necessary)
		    rparam(z, c, L, qdata, wn2, betan2, cornern, z0, tolerance );
		} // if ((z == null) || (z.length == 0))
		else {
			// Prevertices, etc. given.  Renumber to conform
			corner = new int[4];
			int renum[] = new int[w.length];
			rcorners(w, beta, z, corner, renum);
			int nqpts = (int)Math.ceil(-Math.log10(tolerance));
		    qdata = new double[nqpts][2*beta.length+2];
			scqdata(qdata, beta, nqpts);
			for (i = 0; i < w.length; i++) {
				x[i] = w[i][0];
				y[i] = w[i][1];
			}
			double  alpha[] = new double[beta.length];
			for (i = 0; i < beta.length; i++) {
				alpha[i] = beta[i] + 1.0;
			}
			poly = new polygon(x, y, alpha);
		} // else
		scmap M = new scmap();
		M.prevertex = z;
		M.constant[0] = c[0];
		M.constant[1]= c[1];
		M.stripL = L;
		M.qdata = qdata;
		M.poly = poly;
		
		// Now fill in apparent accuracy
		M.accuracy = rectAccuracy(M);
		return M;
	}
	
	public void diskplot(scmap M, double R[], double theta[], double error[]) {
	    // Visualize a Schwarz-Christoffel disk map.
		// diskplot plots the polygon associated with the Schwarz-Christoffel
		// disk map and the images of circles and radii under the S-C
		// transformation.  If R.length == 1 and theta.length == 1, R[0] is the
		// number of evenly spaced circles and theta[0] is the number of
		// evenly spaced rays.  If R.length > 1 and theta.length > 1, then the
		// circles are plotted at radii given by the entries of R and rays at
		// the angles specified in theta.
		
		// tolerance normally defaults to max(1.0E-4, M.accuracy)
		
		// From original MATLAB plot routine copyright 1998 by Toby Driscoll
		int i;
		int nqpts;
		polygon p = M.poly;
		double w[][] = p.vertex;
		double beta[] = new double[p.angle.length];
		for (i = 0; i < beta.length; i++) {
			beta[i] = p.angle[i] - 1;
		}
		double z[][] = M.prevertex;
		double c[] = M.constant;
		if (R == null) {
			R = new double[1];
			R[0] = 10;
		}
		if (theta == null) {
			theta = new double[1];
			theta[0] = 10;
		}
		if (error != null) {
			nqpts = (int)Math.ceil(-Math.log10(error[0]));
		}
		else {
			nqpts = 5;
		}
		dplot(w, beta, z, c, R, theta, nqpts);
	}
	
	private void dplot(double w[][], double beta[], double z[][], double c[],
			double R[], double theta[], int nqpts) {
		// Image of polar grid under Schwarz-Christoffel disk map.
		// dplot will adptielvely plot the images under the Schwarz-Christoffel 
		// disk amp of circles and rays in the unit disk. 
		// If R.length == 1 and theta.length == 1, R[0] is the
		// number of evenly spaced circles and theta[0] is the number of
		// evenly spaced rays.  If R.length > 1 and theta.length > 1, then the
		// circles are plotted at radii given by the entries of R and rays at
		// the angles specified in theta.
		
		// nqpts Number of quadrature points per integration.
		// Approximately equals -log10(error).  Increase if plot
	    // has false little zigzags in curves. 
		
		// Original MATLAB dplot routine copyright 1998 by Toby Driscoll.
		
		int i;
		int j;
		int m;
		int Rlength;
		int thetalength;
		double spacing;
		double R2[];
		double theta2[];
		int n = w.length;
		// Minimum line segment length, as a proportion of the axes box
        double minlen = 0.005;
        // Maximum line segment length, as a proportion of the axes box
        double maxlen = 0.02;
        // Max allowed number of adaptive refinements made to meet other requirements 
        int maxrefn = 16;
		double axlim[] = new double[4];
		Vector<Double>x1Vector = new Vector<Double>();
		Vector<Double>y1Vector = new Vector<Double>();
		Vector<Double>x2Vector = new Vector<Double>();
		Vector<Double>y2Vector = new Vector<Double>();
		Vector<Double>tpReal = new Vector<Double>();
		Vector<Double>tpImag = new Vector<Double>();
		Vector<Double>RpReal = new Vector<Double>();
		Vector<Double>RpImag = new Vector<Double>();
		Vector<Double>zpReal = new Vector<Double>();
		Vector<Double>zpImag = new Vector<Double>();
		Vector<Double>wpReal = new Vector<Double>();
		Vector<Double>wpImag = new Vector<Double>();
		Vector<Boolean>newlog = new Vector<Boolean>();
		double zp[][];
		double neww[][];
		// Empty arguments default to 10.
		if ((R == null) || (R.length == 0)) {
			R = new double[1];
			R[0] = 10;
		}
		if ((theta == null) || (theta.length == 0)) {
			theta = new double[1];
			theta[0] = 10;
		}
		
		// Integer arguments must be converted to specific values
		if ((R.length == 1) && (R[0] == Math.round(R[0]))) {
			Rlength = (int)R[0];
			R2 = new double[Rlength];
		    spacing = (1.0/(R[0] + 1.0));
		    for (i = 1; i <= Rlength; i++) {
		        R2[i-1] = i*spacing;	
		    }
		} // if ((R.length == 1) && (R[0] == Math.round(R[0])))
		else {
			R2 = new double[R.length];
			for (i = 0; i < R.length; i++) {
				R2[i] = R[i];
			}
		}
		if ((theta.length == 1) && (theta[0] == Math.round(theta[0]))) {
			thetalength = (int)theta[0];
			theta2 = new double[thetalength];
		    spacing = ((2.0*Math.PI)/theta[0]);
		    for (i = 0; i < thetalength; i++) {
		        theta2[i] = i*spacing;	
		    }
		} // if ((theta.length == 1) && (theta[0] == Math.round(theta[0])))
		else {
			theta2 = new double[theta.length];
			for (i = 0; i < theta.length; i++) {
				theta2[i] = theta[i];
			}
		}
		float xPointArray[] = new float[n+1];
		float yPointArray[] = new float[n+1];
		ViewJFrameGraph pointGraph = plotpoly(xPointArray, yPointArray, w, beta, false, axlim);
		double qdat[][] = new double[nqpts][2*beta.length+2];
		scqdata(qdat, beta, nqpts);
		ViewJComponentGraph graph = pointGraph.getGraph();
		Rectangle graphBounds = graph.getGraphBounds();
		Graphics g = graph.getGraphics();
		double xScale = graphBounds.width / (axlim[1] - axlim[0]);
        double yScale = graphBounds.height / (axlim[3] - axlim[2]);
		double len = Math.max(graphBounds.width, graphBounds.height);
		minlen = len * minlen;
		maxlen = len * maxlen;
		
		// Plot circles...
		Vector<Double> linhx[][] = new Vector[R2.length][2];
		Vector<Double> linhy[][] = new Vector[R2.length][2];
		for (i = 0; i < R2.length; i++) {
			for (j = 0; j < 2; j++) {
				linhx[i][j] = new Vector<Double>();
				linhy[i][j] = new Vector<Double>();
			}
		}
		for (j = 0; j < R2.length; j++) {
		    // Start with evenly spaced theta
			tpReal.clear();
			tpImag.clear();
			// Increase from 20 in original code to 200.
			for (i = 0; i < 200; i++) {
				tpReal.add(i*(2.0*Math.PI)/199.0);
				tpImag.add(0.0);
			}
			newlog.clear();
			for (i = 0; i < 200; i++) {
				newlog.add(true);
			}
			wpReal.clear();
			wpImag.clear();
			for (i = 0; i < 200; i++) {
				wpReal.add(Double.NaN);
				wpImag.add(0.0);
			}
			
			// The individual points will be shown as they are found.
			
			// Adaptive refinement to make curve smooth
			int iter = 0;
			while (iter < maxrefn) {
				int numnewlog = 0;
				for (i = 0; i < newlog.size(); i++) {
				    if (newlog.get(i)) {
				    	numnewlog++;
				    }
				} // for (i = 0; i < newlog.size(); i++)
				if (numnewlog == 0) {
					break;
				}
				zp = new double[numnewlog][2];
				for (i = 0, m = 0; i < newlog.size(); i++) {
				    if (newlog.get(i)) {
				    	zp[m][0] = R2[j]*Math.cos(tpReal.get(i));
				    	zp[m++][1] = R2[j]*Math.sin(tpReal.get(i));
				    }
				} // for (i = 0, m = 0; i < newlog.length; i++)
				neww = new double[numnewlog][2];
				neww = dmap(zp,w,beta,z,c,qdat);
				for (i = 0, m = 0; i < newlog.size(); i++) {
					if (newlog.get(i)) {
					    wpReal.set(i, neww[m][0]);
					    wpImag.set(i, neww[m++][1]);
					} 
				} // for (i = 0, m = 0; i < newlog.size(); i++)
				iter = iter + 1;
				
				linhx[j][0].clear();
				linhy[j][0].clear();
				linhx[j][1].clear();
				linhy[j][1].clear();
				// Update the points to show progress
				for (i = 0; i < wpReal.size(); i++) {
				    linhx[j][0].add(wpReal.get(i));
				    linhy[j][0].add(wpImag.get(i));
				}
				for (i = 0; i < zp.length; i++) {
					linhx[j][1].add(zp[i][0]);
				    linhy[j][1].add(zp[i][1]);	
				}
				
				// Add points to tp where necessary
				scpadapt(tpReal, tpImag, wpReal, wpImag, newlog, minlen, maxlen, axlim);
			} // while (iter < maxrefn)
		} // for (j = 0; j < R2.length; j++)
		
		for (i = 0; i < R2.length; i++) {
		    if ((linhx[i][0] != null)&& (linhy[i][0] != null)) {
		    	for (j = 0; j < linhx[i][0].size()-1; j++) {
		    		double posx1 = linhx[i][0].get(j);
		    		double posy1 = linhy[i][0].get(j);
		    		double posx2 = linhx[i][0].get(j+1);
		    		double posy2 = linhy[i][0].get(j+1);
		    		x1Vector.add(posx1);
		    		y1Vector.add(posy1);
		    		x2Vector.add(posx2);
		    		y2Vector.add(posy2);
		    	    int x1 =  (int)Math.round(graphBounds.x + xScale*(posx1 - axlim[0]));
    			    int y1 =  (int)Math.round(graphBounds.y + yScale*(posy1 - axlim[2]));
    			    y1 = -y1 + 2*graphBounds.y + graphBounds.height;
    			    int x2 =  (int)Math.round(graphBounds.x + xScale*(posx2 - axlim[0]));
    			    int y2 =  (int)Math.round(graphBounds.y + yScale*(posy2 - axlim[2]));
    			    y2 = -y2 + 2*graphBounds.y + graphBounds.height;
    			    graph.drawLine(g, x1, y1, x2, y2);
		    	}
		    }
		} // for (i = 0; i < R2.length; i++)
		
		// Plot radii
		linhx = new Vector[theta2.length][2];
		linhy = new Vector[theta2.length][2];
		for (i = 0; i < theta2.length; i++) {
			for (j = 0; j < 2; j++) {
				linhx[i][j] = new Vector<Double>();
				linhy[i][j] = new Vector<Double>();
			}
		}
		for (j = 0; j < theta2.length; j++) {
			RpReal.clear();
			RpImag.clear();
			zpReal.clear();
			zpImag.clear();
			// Increase from 14 in original code to 140.
			for (i = 0; i < 140; i++) {
				RpReal.add(i/139.0);
				RpImag.add(0.0);
				zpReal.add(RpReal.get(i)*Math.cos(theta2[j]));
				zpImag.add(RpReal.get(i)*Math.sin(theta2[j]));
			}
			newlog.clear();
			for (i = 0; i < 140; i++) {
				newlog.add(true);
			}
			wpReal.clear();
			wpImag.clear();
			for (i = 0; i < 140; i++) {
				wpReal.add(Double.NaN);
				wpImag.add(0.0);
			}
			
			// The individual points will be shown as they are found.
			
			// Adaptive refinement to make curve smooth
			int iter = 0;
			while (iter < maxrefn) {
				int numnewlog = 0;
				for (i = 0; i < newlog.size(); i++) {
				    if (newlog.get(i)) {
				    	numnewlog++;
				    }
				} // for (i = 0; i < newlog.size(); i++)
				if (numnewlog == 0) {
					break;
				}
				double zpnew[][] = new double[numnewlog][2];
				for (i = 0, m = 0; i < newlog.size(); i++) {
				    if (newlog.get(i)) {
				    	zpnew[m][0] = zpReal.get(i);
				    	zpnew[m++][1] = zpImag.get(i);
				    }
				} // for (i = 0, m = 0; i < newlog.length; i++)
				neww = dmap(zpnew, w, beta, z, c, qdat);
				for (i = 0, m = 0; i < newlog.size(); i++) {
					if (newlog.get(i)) {
					    wpReal.set(i, neww[m][0]);
					    wpImag.set(i, neww[m++][1]);
					} 
				} // for (i = 0, m = 0; i < newlog.size(); i++)
				iter = iter + 1;
				
				linhx[j][0].clear();
				linhy[j][0].clear();
				linhx[j][1].clear();
				linhy[j][1].clear();
				// Update the points to show progress
				for (i = 0; i < wpReal.size(); i++) {
				    linhx[j][0].add(wpReal.get(i));
				    linhy[j][0].add(wpImag.get(i));
				    linhx[j][1].add(zpReal.get(i));
				    linhy[j][1].add(zpImag.get(i));
				}
				
				scpadapt(zpReal, zpImag, wpReal, wpImag, newlog, minlen, maxlen, axlim);
			} // while (iter < maxrefn)
		} // for (j = 0; j < theta2.length; j++)
		
		for (i = 0; i < theta2.length; i++) {
		    if ((linhx[i][0] != null)&& (linhy[i][0] != null)) {
		    	for (j = 0; j < linhx[i][0].size()-1; j++) {
		    		double posx1 = linhx[i][0].get(j);
		    		
		    		double posy1 = linhy[i][0].get(j);
		    		double posx2 = linhx[i][0].get(j+1);
		    		double posy2 = linhy[i][0].get(j+1);
		    		x1Vector.add(posx1);
		    		y1Vector.add(posy1);
		    		x2Vector.add(posx2);
		    		y2Vector.add(posy2);
		    	    int x1 =  (int)Math.round(graphBounds.x + xScale*(posx1 - axlim[0]));
    			    int y1 =  (int)Math.round(graphBounds.y + yScale*(posy1 - axlim[2]));
    			    y1 = -y1 + 2*graphBounds.y + graphBounds.height;
    			    int x2 =  (int)Math.round(graphBounds.x + xScale*(posx2 - axlim[0]));
    			    int y2 =  (int)Math.round(graphBounds.y + yScale*(posy2 - axlim[2]));
    			    y2 = -y2 + 2*graphBounds.y + graphBounds.height;
    			    graph.drawLine(g, x1, y1, x2, y2);
		    	}
		    }
		} // for (i = 0; i < theta2.length; i++)
		
		graph.setX1Vector(x1Vector);
		graph.setY1Vector(y1Vector);
		graph.setX2Vector(x2Vector);
		graph.setY2Vector(y2Vector);
		graph.setAddSchwarzChristoffelLines(true);
		graph.paintComponent(g);
	}
	
	public void rectplot(scmap M, int nre, int nim) {
	    // plot plots the polygon associated with the Schwarz-Christoffel
		// rectangle map M and the images of nre evenly spaced vertical 
	    // and nim horizontal line segments.  
		// The usual values are nre = nim = 10
		// Extracted from original routine by Toby Driscoll copyright 1998.
		int i;
		polygon p = M.poly;
		double w[][] = p.vertex;
		double beta[] = new double[p.angle.length];
		for (i = 0; i < p.angle.length; i++) {
			beta[i] = p.angle[i] - 1;
		}
		double z[][] = M.prevertex;
		double c[] = M.constant;
		double L[] = M.stripL;
		rplot(w, beta, z, c, L, nre, nim);
	}
	
	private void rplot(double w[][], double beta[], double z[][], double c[],
			double L[], int re, int im) {
		// Image of cartesian grid under Schwarz-Christoffel rectangle map.
		// rplot will adaptively plot the images under the Schwarz-Christoffel
		// rectangle map of re evenly spaced vertical and im evenly spaced
		// horizontal lines.
		// Extracted form original routine by Toby Driscoll copyright 1998.
		
		// Number of quadrature points per integration.
		// Approximately equals -log10(error).  Increase if plot
		// has false little zigzags in curves. 
		int nqpts = 5; 
		// Minimum line segment length, as a proportion of the axes box
        double minlen = 0.005;
        // Maximum line segment length, as a proportion of the axes box
        double maxlen = 0.02;
        // Max allowed number of adaptive refinements made to meet other requirements 
        int maxrefn = 16;
        double axlim[] = new double[4];
		int i, j, m;
		Vector<Double>x1Vector = new Vector<Double>();
		Vector<Double>y1Vector = new Vector<Double>();
		Vector<Double>x2Vector = new Vector<Double>();
		Vector<Double>y2Vector = new Vector<Double>();
		Vector<Double>zpReal = new Vector<Double>();
		Vector<Double>zpImag = new Vector<Double>();
		Vector<Double>wpReal = new Vector<Double>();
		Vector<Double>wpImag = new Vector<Double>();
		Vector<Boolean>newlog = new Vector<Boolean>();
		int n = w.length;
		int corners[] = new int[4];
		int renum[] = new int[w.length];
		rcorners(w, beta, z, corners, renum);
		double rect[][] = new double[4][2];
		for (i = 0; i < 4; i++) {
			rect[i][0] = z[corners[i]][0];
			rect[i][1] = z[corners[i]][1];
		}
		
		double Kp = rect[1][1];
		// double K[] = new double[2];
		// For testRectmap1 K[0] = 2.1555087314127523 K[1] = 0.0
		// K[0] = rect[0][0];
		//K[1] = rect[0][1];
	    double K = rect[0][0];
		double recoords[] = new double[re];
		double rexspacing = 2.0*K/(re+1);
		for (i = 1; i <= re; i++) {
			recoords[i-1] = -K + i*rexspacing;
		}
		double imcoords[] = new double[im];
		double imspacing = Kp/(im+1);
		for (i = 1; i <= im; i++) {
			imcoords[i-1] = i*imspacing;
		}
		float xPointArray[] = new float[n+1];
		float yPointArray[] = new float[n+1];
		ViewJFrameGraph pointGraph = plotpoly(xPointArray, yPointArray, w, beta, false, axlim);
		double qdat[][] = new double[nqpts][2*beta.length+2];
		scqdata(qdat, beta, nqpts);
		ViewJComponentGraph graph = pointGraph.getGraph();
		Rectangle graphBounds = graph.getGraphBounds();
		Graphics g = graph.getGraphics();
		double xScale = graphBounds.width / (axlim[1] - axlim[0]);
        double yScale = graphBounds.height / (axlim[3] - axlim[2]);
		double len = Math.max(graphBounds.width, graphBounds.height);
		minlen = len * minlen;
		maxlen = len * maxlen;
		
		// Plot vertical lines
		Vector<Double> linhx[][] = new Vector[re][2];
		Vector<Double> linhy[][] = new Vector[re][2];
		for (i = 0; i < re; i++) {
			for (j = 0; j < 2; j++) {
				linhx[i][j] = new Vector<Double>();
				linhy[i][j] = new Vector<Double>();
			}
		}
		for (j = 0; j < re; j++) {
		    // Start evenly spaced
			zpReal.clear();
			zpImag.clear();
			for (i = 0; i < 15; i++) {
				zpReal.add(recoords[j]);
				zpImag.add(i*Kp/14.0);
			}
			newlog.clear();
			for (i = 0; i < 15; i++) {
				newlog.add(true);
			}
			wpReal.clear();
			wpImag.clear();
			for (i = 0; i < 15; i++) {
				wpReal.add(Double.NaN);
				wpImag.add(0.0);
			}
			
			// The individual points will be shown as they are found.
			
			// Adaptive refinement to make curve smooth
			int iter = 0;
			while (iter < maxrefn) {
				int numnewlog = 0;
				for (i = 0; i < newlog.size(); i++) {
				    if (newlog.get(i)) {
				    	numnewlog++;
				    }
				} // for (i = 0; i < newlog.size(); i++)
				if (numnewlog == 0) {
					break;
				}
				double zpnew[][] = new double[numnewlog][2];
				for (i = 0, m = 0; i < newlog.size(); i++) {
				    if (newlog.get(i)) {
				    	zpnew[m][0] = zpReal.get(i);
				    	zpnew[m++][1] = zpImag.get(i);
				    }
				} // for (i = 0, m = 0; i < newlog.length; i++)
				double neww[][] = new double[numnewlog][2];
				rmap(neww, zpnew, w, beta, z, c, L, qdat);
				for (i = 0, m = 0; i < newlog.size(); i++) {
					if (newlog.get(i)) {
					    wpReal.set(i, neww[m][0]);
					    wpImag.set(i, neww[m++][1]);
					} 
				} // for (i = 0, m = 0; i < newlog.size(); i++)
				iter = iter + 1;
				
				linhx[j][0].clear();
				linhy[j][0].clear();
				linhx[j][1].clear();
				linhy[j][1].clear();
				// Update the points to show progress
				for (i = 0; i < wpReal.size(); i++) {
				    linhx[j][0].add(wpReal.get(i));
				    linhy[j][0].add(wpImag.get(i));
				    linhx[j][1].add(zpReal.get(i));
				    linhy[j][1].add(zpImag.get(i));
				}
				
				// Add points to zp where necessary
				scpadapt(zpReal, zpImag, wpReal, wpImag, newlog, minlen, maxlen, axlim);
			} // while (iter < maxrefn)
		} // for (j = 0; j < re; j++)
		
		
		for (i = 0; i < re; i++) {
		    if ((linhx[i][0] != null)&& (linhy[i][0] != null)) {
		    	for (j = 0; j < linhx[i][0].size()-1; j++) {
		    		double posx1 = linhx[i][0].get(j);
		    		double posy1 = linhy[i][0].get(j);
		    		double posx2 = linhx[i][0].get(j+1);
		    		double posy2 = linhy[i][0].get(j+1);
		    		x1Vector.add(posx1);
		    		y1Vector.add(posy1);
		    		x2Vector.add(posx2);
		    		y2Vector.add(posy2);
		    	    int x1 =  (int)Math.round(graphBounds.x + xScale*(posx1 - axlim[0]));
    			    int y1 =  (int)Math.round(graphBounds.y + yScale*(posy1 - axlim[2]));
    			    y1 = -y1 + 2*graphBounds.y + graphBounds.height;
    			    int x2 =  (int)Math.round(graphBounds.x + xScale*(posx2 - axlim[0]));
    			    int y2 =  (int)Math.round(graphBounds.y + yScale*(posy2 - axlim[2]));
    			    y2 = -y2 + 2*graphBounds.y + graphBounds.height;
    			    graph.drawLine(g, x1, y1, x2, y2);
		    	}
		    }
		}
		
		// Plot horizontal lines
		linhx = new Vector[im][2];
		linhy = new Vector[im][2];
		for (i = 0; i < im; i++) {
			for (j = 0; j < 2; j++) {
				linhx[i][j] = new Vector<Double>();
				linhy[i][j] = new Vector<Double>();
			}
		}
		for (j = 0; j < im; j++) {
		    // Start evenly spaced
			zpReal.clear();
			zpImag.clear();
			for (i = 0; i < 15; i++) {
				zpReal.add(-K + 2.0*i*K/14.0);
				zpImag.add(imcoords[j]);
			}
			newlog.clear();
			for (i = 0; i < 15; i++) {
				newlog.add(true);
			}
			wpReal.clear();
			wpImag.clear();
			for (i = 0; i < 15; i++) {
				wpReal.add(Double.NaN);
				wpImag.add(0.0);
			}
			
			// The individual points will be shown as they are found.
			
			// Adaptive refinement to make curve smooth
			int iter = 0;
			while (iter < maxrefn) {
				int numnewlog = 0;
				for (i = 0; i < newlog.size(); i++) {
				    if (newlog.get(i)) {
				    	numnewlog++;
				    }
				} // for (i = 0; i < newlog.size(); i++)
				if (numnewlog == 0) {
					break;
				}
				double zpnew[][] = new double[numnewlog][2];
				for (i = 0, m = 0; i < newlog.size(); i++) {
				    if (newlog.get(i)) {
				    	zpnew[m][0] = zpReal.get(i);
				    	zpnew[m++][1] = zpImag.get(i);
				    }
				} // for (i = 0, m = 0; i < newlog.length; i++)
				double neww[][] = new double[numnewlog][2];
				rmap(neww, zpnew, w, beta, z, c, L, qdat);
				for (i = 0, m = 0; i < newlog.size(); i++) {
					if (newlog.get(i)) {
					    wpReal.set(i, neww[m][0]);
					    wpImag.set(i, neww[m++][1]);
					} 
				} // for (i = 0, m = 0; i < newlog.size(); i++)
				iter = iter + 1;
				
				linhx[j][0].clear();
				linhy[j][0].clear();
				linhx[j][1].clear();
				linhy[j][1].clear();
				// Update the points to show progress
				for (i = 0; i < wpReal.size(); i++) {
				    linhx[j][0].add(wpReal.get(i));
				    linhy[j][0].add(wpImag.get(i));
				    linhx[j][1].add(zpReal.get(i));
				    linhy[j][1].add(zpImag.get(i));
				}
				
				// Add points to zp where necessary
				scpadapt(zpReal, zpImag, wpReal, wpImag, newlog, minlen, maxlen, axlim);
			} // while (iter < maxrefn)
		} // for (j = 0; j < im; j++)
		
		
		for (i = 0; i < im; i++) {
		    if ((linhx[i][0] != null)&& (linhy[i][0] != null)) {
		    	for (j = 0; j < linhx[i][0].size()-1; j++) {
		    		double posx1 = linhx[i][0].get(j);
		    		double posy1 = linhy[i][0].get(j);
		    		double posx2 = linhx[i][0].get(j+1);
		    		double posy2 = linhy[i][0].get(j+1);
		    		x1Vector.add(posx1);
		    		y1Vector.add(posy1);
		    		x2Vector.add(posx2);
		    		y2Vector.add(posy2);
		    	    int x1 =  (int)Math.round(graphBounds.x + xScale*(posx1 - axlim[0]));
    			    int y1 =  (int)Math.round(graphBounds.y + yScale*(posy1 - axlim[2]));
    			    y1 = -y1 + 2*graphBounds.y + graphBounds.height;
    			    int x2 =  (int)Math.round(graphBounds.x + xScale*(posx2 - axlim[0]));
    			    int y2 =  (int)Math.round(graphBounds.y + yScale*(posy2 - axlim[2]));
    			    y2 = -y2 + 2*graphBounds.y + graphBounds.height;
    			    graph.drawLine(g, x1, y1, x2, y2);
		    	}
		    }
		}
		
		graph.setX1Vector(x1Vector);
		graph.setY1Vector(y1Vector);
		graph.setX2Vector(x2Vector);
		graph.setY2Vector(y2Vector);
		graph.setAddSchwarzChristoffelLines(true);
		graph.paintComponent(g);
	}
	
	private void scpadapt(Vector<Double> zpReal, Vector<Double> zpImag,
		Vector<Double> wpReal, Vector<Double> wpImag, Vector<Boolean> newlog,
			double minlen, double maxlen, double clip[]) {
		// This function looks for unacceptable nonsmoothness in the curve(s)
		// represented by wp.  At such points it adds in-between points to zp.
		// On return zp is the combined vector of points in order, wp has NaN's 
		// at the new points, and newlog is a 0-1 vector flagging the newly added 
		// points.
		
		// The algorithm is basically stolen form fplot.  If extrapolation of the
		// linear interpolation that the graphics will use results in an estimate
		// too far from reality, refinement is called for.
		
		// When the clip argument is used, the criteria do not apply for points
		// outside the clip region.  Why refine for invisible points?
		
		// wp can have multiple columns (curves).  Any curve needing refinement
		// causes it for all of them.
		
		// This should become the adaptive routine for all SC plotting.  Allowances
		// must be made for cases when the ends of zp need not be bounded (as with
		// the half-plane and strip).  Perhaps the newly added points should be
		// spaced exponentially rather than added algebraically.
		
		// Original MATLAB routine copyright 1997-2003 by Toby Driscoll.
		int i, j;
		double cr[] = new double[1];
		double ci[] = new double[1];
		int m = wpReal.size();
		double dwp[][] = new double[m-1][2];
		for (i = 0; i < m-1; i++) {
			dwp[i][0] = wpReal.get(i+1) - wpReal.get(i);
			dwp[i][1] = wpImag.get(i+1) - wpImag.get(i);
		}
		
		// Infinities at the ends of zp mean that we go out forever.  However,
		// we will pick a finite bound.
		boolean linf = (Double.isInfinite(zpReal.get(0)) || Double.isInfinite(zpImag.get(0)));
		if (linf) {
			double absVal = zabs(zpReal.get(2) - zpReal.get(1), zpImag.get(2) - zpImag.get(1));
			double maxVal = Math.max(3, absVal * absVal);
			double signVal[] = sign(zpReal.get(1) - zpReal.get(2),
					                zpImag.get(1) - zpImag.get(2));
			zpReal.set(0, zpReal.get(1) + maxVal*signVal[0]);
			zpImag.set(0, zpImag.get(1) + maxVal*signVal[1]);
		} // if (linf)
		boolean rinf = (Double.isInfinite(zpReal.get(m-1)) || Double.isInfinite(zpImag.get(m-1)));
		if (rinf) {
			double absVal = zabs(zpReal.get(m-2) - zpReal.get(m-3), 
					zpImag.get(m-2) - zpImag.get(m-3));	
			double maxVal = Math.max(3, absVal * absVal);
			double signVal[] = sign(zpReal.get(m-2) - zpReal.get(m-3),
	                zpImag.get(m-2) - zpImag.get(m-3));
			zpReal.set(m-1, zpReal.get(m-2) + maxVal*signVal[0]);
			zpImag.set(m-1, zpImag.get(m-2) + maxVal*signVal[1]);
		} // if (rinf)
		
		// Sines of the turning angles at interior points
		double sines[] = new double[m-2];
		for (i = 0; i < m-2; i++) {
		    zmlt(dwp[i][0], dwp[i][1], dwp[i+1][0], -dwp[i+1][1], cr, ci);
		    double ang = Math.atan2(ci[0], cr[0]);
		    double sinang = Math.sin(ang);
		    sines[i] = Math.abs(sinang);
		} // for (i = 0; i < m-2; i++)
		
		// Distances from linear extrapolation to actual value.  Each interior
		// point has a column, with two rows being errors to either side.
		double absdwp[] = new double[m-1];
		for (i = 0; i < m-1; i++) {
			absdwp[i] = zabs(dwp[i][0], dwp[i][1]);
		}
		double err[][] = new double[m-2][2];
		for (i = 0; i < m-2; i++) {
			err[i][0] = absdwp[i]*sines[i];
			err[i][1] = absdwp[i+1]*sines[i];
		}
		
		
		// Forget about NaNs
		for (i = 0; i < m-2; i++) {
			for (j = 0; j < 2; j++) {
				if (Double.isNaN(err[i][j])) {
					err[i][j] = 0.0;
				}
			}
		}
		
		for (i = 0; i < absdwp.length; i++) {
			if (Double.isNaN(absdwp[i])) {
				absdwp[i] = 0;
			}
		}
		
		// Flag large errors
		boolean bad[][] = new boolean[m][2];
		for (i = 1; i < m-1; i++) {
			for (j = 0; j < 2; j++) {
				if (err[i-1][j] > maxlen/12.0) {
					bad[i][j] = true;
				}
			}
		}
		
		// Also flag if the segments themselves are too long.
		// However, exclude where segments are very short.
		boolean longv[] = new boolean[m-1];
		boolean shortv[] = new boolean[m-1];
		for (i = 0; i < m-1; i++) {
			if (absdwp[i] > maxlen) {
				longv[i] = true;
			}
			if (absdwp[i] < minlen) {
				shortv[i] = true;
			}
		} //  for (i = 0; i < m-1; i++)
		bad[0][0] = false;
		for (i = 1; i < m; i++) {
			bad[i][0] = (bad[i][0] || longv[i-1]) && (!shortv[i-1]);
		}
		for (i = 0; i < m-1; i++) {
			bad[i][1] = (bad[i][1] || longv[i]) && (!shortv[i]);
		}
		bad[m-1][1] = false;
		
		// Find clipped points
		double xp[] = new double[m];
		for (i = 0; i < m; i++) {
			xp[i] = wpReal.get(i);
		}
		double yp[] = new double[m];
		for (i = 0; i < m; i++) {
			yp[i] = wpImag.get(i);
		}
		boolean lohi[] = new boolean[m];
		boolean lfrt[] = new boolean[m];
		for (i = 0; i < m; i++) {
			lohi[i] = (yp[i] < clip[2]) || (yp[i] > clip[3]) || Double.isInfinite(xp[i]) ||
					   Double.isInfinite(yp[i]);
			lfrt[i] = (xp[i] < clip[0]) || (xp[i] > clip[1]) || Double.isInfinite(xp[i]) ||
					   Double.isInfinite(yp[i]);
		}
		
		// To be refined, you or a neighbor must be inside
		boolean inside[] = new boolean[m];
		for (i = 0; i < m; i++) {
			inside[i] = (!lohi[i]) && (!lfrt[i]);
		}
		
		for (i = 1; i < m; i++) {
			bad[i][0] = bad[i][0] && (inside[i] || inside[i-1]);
		}
		for (i = 0; i < m-1; i++) {
			bad[i][1] = bad[i][1] && (inside[i] || inside[i+1]);
		}
		
		// Exception: Extend the lines that bound the axes box.  If you cross one
		// horizontal and one vertical among those lines between adjacent points,
		// you must refine in-between.  Otherwise, you could miss valid inside
		// points, or get a line that crosses back into the axes box.
		boolean lh[] = new boolean[m];
		boolean lr[] = new boolean[m];
		for (i = 0; i < m; i++) {
			lh[i] = lohi[i] && (!lfrt[i]);
			lr[i] = lfrt[i] && (!lohi[i]);
		}
		boolean flag[] = new boolean[m-1];
		int numflag = 0;
		for (i = 0; i < m-1; i++) {
			flag[i] = (lh[i] && lr[i+1]) || (lr[i] && lh[i+1]);
			if (flag[i]) {
				numflag++;
			}
		} // for (i = 0; i < m-1; i++)
		if (numflag > 0) {
		    for (i = 0; i < m-1; i++) {
		    	if (flag[i]) {
		    		bad[i+1][0] = true;
		    		bad[i][1] = true;
		    	}
		    }
		} // if (numflag > 0)
		
		// Refinement
		double nans[][] = new double[m][2];
		for (i = 0; i < m; i++) {
			nans[i][0] = Double.NaN;
		}
		double zp2[][][] = new double[3][m][2];
		for (i = 0; i < m; i++) {
			zp2[0][i][0] = Double.NaN;
			zp2[1][i][0] = zpReal.get(i);
			zp2[1][i][1] = zpImag.get(i);
			zp2[2][i][0] = Double.NaN;
		}
		double new2[][] = new double[3][m];
		for (i = 0; i < m; i++) {
			new2[0][i] = Double.NaN;
			new2[2][i] = Double.NaN;
		}
		
		// Add new source points.  Always go one-third of the distance toward
		// the offending party.
		int numback = 0;
		for (i = 0; i < m; i++) {
			if (bad[i][0]) {
				numback++;
			}
		} // for (i = 0; i < m; i++)
		if (numback > 0) {
		    int back[] = new int[numback];
		    for (i = 0, j = 0; i < m; i++) {
		    	if (bad[i][0]) {
		    		back[j++] = i;
		    	}
		    } // for (i = 0, j = 0; i < m; i++)
		    for (i = 0; i < numback; i++) {
		    	j = back[i];
		    	zp2[0][j][0] = (zp2[1][j-1][0] + 2.0*zp2[1][j][0])/3.0;
		    	zp2[0][j][1] = (zp2[1][j-1][1] + 2.0*zp2[1][j][1])/3.0;
		    	new2[0][j] = 1;
		    }
		} // if (numback > 0)
		int numforw = 0;
		for (i = 0; i < m; i++) {
			if (bad[i][1]) {
				numforw++;
			}
		} // for (i = 0; i < m; i++)
		if (numforw > 0) {
		    int forw[] = new int[numforw];
		    for (i = 0, j = 0; i < m; i++) {
		    	if (bad[i][1]) {
		    		forw[j++] = i;
		    	}
		    } // for (i = 0, j = 0; i < m; i++)
		    for (i = 0; i < numforw; i++) {
		    	j = forw[i];
		    	zp2[2][j][0] = (zp2[1][j+1][0] + 2.0*zp2[1][j][0])/3.0;
		    	zp2[2][j][1] = (zp2[1][j+1][1] + 2.0*zp2[1][j][1])/3.0;
		    	new2[2][j] = 1;
		    }
		} // if (numforw > 0)
		
		// If we had infinities at the ends, put them back
		if (linf) {
			zp2[1][0][0] = Double.POSITIVE_INFINITY;
		}
		if (rinf) {
			zp2[1][m-1][0] = Double.POSITIVE_INFINITY;
		}
		
		// Restack zp into a vector, deleting NaNs
		zpReal.clear();
		zpImag.clear();
		newlog.clear();
		for (j = 0; j < m; j++) {
			for (i = 0; i < 3; i++) {
				if ((!Double.isNaN(zp2[i][j][0])) && (!Double.isNaN(zp2[i][j][1]))) {
				    zpReal.add(zp2[i][j][0]);
				    zpImag.add(zp2[i][j][1]);
				    if (new2[i][j] != 0) {
				        newlog.add(true);
				    }
				    else {
				    	newlog.add(false);
				    }
				}
			}
		} // for (j = 0; j < m; j++)
		
		// Update wp
		double wpold[][] =  new double[wpReal.size()][2];
		for (i = 0; i < wpReal.size(); i++) {
			wpold[i][0] = wpReal.get(i);
			wpold[i][1] = wpImag.get(i);
		}
		wpReal.clear();
		wpImag.clear();
		for (i = 0; i < zpReal.size(); i++) {
			wpReal.add(Double.NaN);
			wpImag.add(0.0);
		}
		for (i = 0, j = 0; i < zpReal.size(); i++) {
			if (!newlog.get(i)) {
				wpReal.set(i, wpold[j][0]);
				wpImag.set(i, wpold[j++][1]);
			}
		}
	}
	
	private void rmap(double wp[][], double zp[][], double w[][], double beta[],
			double z[][], double c[], double L[], double qdat[][]) {
		// Schwarz-Christoffel rectangle map
		// rmap computes the values of the Schwarz-Christoffel rectangle map
		// at the points in the vector zp.  wp is the same size as zp.
		// The remaining arguments are in as rparam
		// Original MATLAB routine copyright 1998 by Toby Driscoll.
		int i, j;
		double qdat2[][] = null;
		if ((zp == null) || (zp.length == 0)) {
			wp = null;
			return;
		}
		int n = w.length;
		int corners[] = new int[4];
		int renum[] = new int[w.length];
		rcorners(w, beta, z, corners, renum);
		
		if (qdat == null) {
			qdat2 = new double[8][2*beta.length+2];
			scqdata(qdat2, beta, 8);		
		}
		else if ((qdat.length == 1) && (qdat[0].length == 1)) {
			int nqpts = (int)Math.max(Math.ceil(-Math.log10(qdat[0][0])), 8);
			qdat2 = new double[nqpts][2*beta.length+2];
			scqdata(qdat2, beta, nqpts);	
		}
		else {
			qdat2 = qdat;
		}
		
		// Map prevertices to strip.
		double K = -Double.MAX_VALUE;
		for (i = 0; i < z.length; i++) {
			if (z[i][0] > K) {
				K = z[i][0];
			}
		}
		double Kp = -Double.MAX_VALUE;
		for (i = 0; i < z.length; i++) {
			if (z[i][1] > Kp) {
				Kp = z[i][1];
			}
		}
		double zs[][] = new double[z.length][2];
		double zsprime[][] = new double[z.length][2];
		r2strip(zs, zsprime, z, z, L[0]);
		for (i = 0; i < zs.length; i++) {
			// Put them *exactly* on edges
			zs[i][1] = Math.round(zs[i][1]);
		}
		
		// Add in ends of strip
		double diffzs[] = new double[n];
		int nonzerodiff = 0;
		for (i = 0; i < n-1; i++) {
			diffzs[i] = zs[i+1][1] - zs[i][1];
			if (diffzs[i] != 0) {
				nonzerodiff++;
			}
		}
		diffzs[n-1] = zs[0][1] - zs[n-1][1];
		if (diffzs[n-1] != 0) {
			nonzerodiff++;
		}
		int ends[] = new int[nonzerodiff];
		for (i = 0, j = 0; i < diffzs.length; i++) {
			if (diffzs[i] != 0) {
			    ends[j++] = i;	
			}
		}
		double zs2[][] = new double[n+2][2];
		double ws[][] = new double[n+2][2];
		double bs[] = new double[n+2];
		for (i = 0; i <= ends[0]; i++) {
			zs2[i][0] = zs[i][0];
			zs2[i][1] = zs[i][1];
			ws[i][0] = w[i][0];
			ws[i][1] = w[i][1];
			bs[i] = beta[i];
		}
		zs2[ends[0]+1][0] = Double.POSITIVE_INFINITY;
		zs2[ends[0]+1][1] = 0;
		ws[ends[0]+1][0] = Double.NaN;
		ws[ends[0]+1][1] = 0;
		bs[ends[0]+1] = 0;
		for (i = ends[0]+1; i <= ends[1]; i++) {
			zs2[i+1][0] = zs[i][0];
			zs2[i+1][1] = zs[i][1];
			ws[i+1][0] = w[i][0];
			ws[i+1][1] = w[i][1];
			bs[i+1] = beta[i];
		}
		zs2[ends[1]+2][0] = Double.NEGATIVE_INFINITY;
		zs2[ends[1]+2][1] = 0;
		ws[ends[1]+2][0] = Double.NaN;
		ws[ends[1]+2][1] = 0;
		bs[ends[1]+2] = 0;
		for (i = ends[1]+1; i < n; i++) {
			zs2[i+2][0] = zs[i][0];
			zs2[i+2][1] = zs[i][1];
			ws[i+2][0] = w[i][0];
			ws[i+2][1] = w[i][1];
			bs[i+2] = beta[i];
		}
		// Put dummy columns into qdat2 after ends[0] and ends[1] columns
		// qdat2 is nqpts rows by 2*n+2 columns
		// so we are expanding it to 2*n+6 columns with the insertion
		// of 2 more of column n and 2 more of column 2*n+1.
		double qdat3[][] = new double[qdat2.length][qdat2[0].length+4];
		for (i = 0; i < qdat2.length; i++) {
			for (j = 0; j <= ends[0]; j++) {
				qdat3[i][j] = qdat2[i][j];
				qdat3[i][j+2+n+1] = qdat2[i][j+n+1];
			}
			qdat3[i][ends[0]+1] = qdat2[i][n];
			qdat3[i][ends[0]+3+n+1] = qdat2[i][2*n+1];
			for (j = ends[0]+1; j <= ends[1]; j++) {
				qdat3[i][j+1] = qdat2[i][j];
				qdat3[i][j+3+n+1] = qdat2[i][j+n+1];
			}
			qdat3[i][ends[1]+2] = qdat2[i][n];
			qdat3[i][ends[1]+4+n+1] = qdat2[i][2*n+1];
			for (j = ends[1]+1; j <= n-1; j++) {
				qdat3[i][j+2] = qdat2[i][j];
				qdat3[i][j+4+n+1] = qdat2[i][j+n+1];
			}
			qdat3[i][n+2] = qdat2[i][n];
			qdat3[i][2*n+5] = qdat2[i][2*n+1];
		} // for (i = 0; i < qdat2.length; i++)
		
		for (i = 0; i < wp.length; i++) {
			wp[i][0] = 0;
			wp[i][1] = 0;
		}
		int p = zp.length;
		
		// Trap points which map to +/- Infinity on the strip.
		for (i = 0; i < p; i++) {
			if (zabs(zp[i][0],zp[i][1]) < 2.0*eps) {
				zp[i][0] = zp[i][0] + 100.0*eps;
			}
		}
		
		for (i = 0; i < p; i++) {
			if (zabs(zp[i][0],zp[i][1] - Kp) < 2.0*eps) {
				zp[i][1] = zp[i][1] - 100.0*eps*Kp;
			}
		}
		
		// Map from rectangle to strip.
		double yp[][] = new double[zp.length][2];
		double ypprime[][] = new double[zp.length][2];
		r2strip(yp, ypprime, zp, z, L[0]);
		
		// Now map from strip to polygon
		stmap(wp, yp, ws, bs, zs2, c, qdat3);
	}
	
	private void stmap(double wp[][], double zp[][], double w[][], double beta[],
			double z[][], double c[], double qdat[][]) {
		// Schwarz-Christoffel strip map.
		// stmap computes the values of the Schwarz-Christoffel strip map
		// at the points in the vector zp.  The arguments w, beta, z, c, and 
		// qdat are as in stparam.  stmap returns a vector wp the same size
		// as zp.
		// Original matlab routine copyright 1998 by Toby Driscoll.
		int i, j;
		double qdat2[][] = null;
		double cr[] = new double[1];
		double ci[] = new double[1];
		double mid1[][] = null;
		double mid2[][] = null;
		
		if ((zp == null) || (zp.length == 0)) {
			wp = null;
			return;
		}
		
		int n = w.length;
		
		// Quadrature data;
		if (qdat == null) {
			qdat2 = new double[8][2*beta.length+2];
			scqdata(qdat2, beta, 8);		
		}
		else if ((qdat.length == 1) && (qdat[0].length == 1)) {
			int nqpts = (int)Math.max(Math.ceil(-Math.log10(qdat[0][0])), 8);
			qdat2 = new double[nqpts][2*beta.length+2];
			scqdata(qdat2, beta, nqpts);	
		}
		else {
			qdat2 = qdat;
		}
		double tol = Math.pow(10.0, -qdat.length);
		
		//int shape = zp.length;
		double zprow[][] = new double[zp.length][2];
		for (i = 0; i < zp.length; i++) {
			zprow[i][0] = zp[i][0];
			zprow[i][1] = zp[i][1];
		}
		int p = zp.length;
		for (i = 0; i < wp.length; i++) {
			wp[i][0] = 0;
			wp[i][1] = 0;
		}
		
		// For each point in zp, find the nearest prevertex
		double dist[] = new double[p];
		int sing[] = new int[p];
		for (i = 0; i < p; i++) {
			dist[i] = Double.MAX_VALUE;
			sing[i] = -1;
			for (j = 0; j < n; j++) {
				double presentDist = zabs(zprow[i][0] - z[j][0], zprow[i][1] - z[j][1]);
				if (presentDist < dist[i]) {
					dist[i] = presentDist;
					sing[i] = j;
				}
			}
		} // for (i = 0; i < p; i++)
		
		// Screen out images of prevertices
		boolean vertex[] = new boolean[p];
		for (i = 0; i < p; i++) {
			if (dist[i] < tol) {
			    vertex[i] = true;
			    wp[i][0] = w[sing[i]][0];
			    wp[i][1] = w[sing[i]][1];
			}
		}
		
		boolean leftend[] = new boolean[p];
		for (i = 0; i < p; i++) {
			if ((Double.isInfinite(zp[i][0]) || Double.isInfinite(zp[i][1])) && (zp[i][0] < 0.0)) {
				leftend[i] = true;
			}
		}
		
		int windex = -1;
		for (i = 0; i < z.length && (windex == -1); i++) {
			if (Double.isInfinite(z[i][0]) && (z[i][0] < 0) && (z[i][1] == 0)) {
				windex = i;
			}
		}
		for (i = 0; i < p; i++) {
			if (leftend[i]) {
				wp[i][0] = w[windex][0];
				wp[i][1] = w[windex][1];
			}
		}
		
		boolean rightend[] = new boolean[p];
		for (i = 0; i < p; i++) {
			if ((Double.isInfinite(zp[i][0]) || Double.isInfinite(zp[i][1])) && (zp[i][0] > 0.0)) {
				rightend[i] = true;
			}
		}
		
		windex = -1;
		for (i = 0; i < z.length && (windex == -1); i++) {
			if (Double.isInfinite(z[i][0]) && (z[i][0] > 0) && (z[i][1] == 0)) {
				windex = i;
			}
		}
		for (i = 0; i < p; i++) {
			if (rightend[i]) {
				wp[i][0] = w[windex][0];
				wp[i][1] = w[windex][1];
			}
		}
		
		for (i = 0; i < p; i++) {
			vertex[i] = vertex[i] || leftend[i] || rightend[i];
		}
		
		// "Bad" points are closest to a prevertex of infinity.
		boolean atinf[] = new boolean[n];
		for (i = 0; i < n; i++) {
			if (Double.isInfinite(w[i][0]) || Double.isInfinite(w[i][1])) {
				// Infinite vertices
				atinf[i] = true;
			}
		}
		
		
		boolean bad[] = new boolean[p];
		int numbad = 0;
		for (i = 0; i < p; i++) {
			if ((atinf[sing[i]]) && (!vertex[i])) {
				bad[i] = true;
				numbad++;
			}
		}
		
		if (numbad > 0) {
			// we can't begin integrations at a preimage of infinity.  We
			// will pick the next-closest qualified prevertex
			double zf[][] = new double[z.length][2];
			for (i = 0; i < z.length; i++) {
				zf[i][0] = z[i][0];
				zf[i][1] = z[i][1];
			}
			for (i = 0; i < n; i++) {
				if (Double.isInfinite(w[i][0]) || Double.isInfinite(w[i][1])) {
				    zf[i][0] = Double.POSITIVE_INFINITY;
				    zf[i][1] = 0.0;
				}
			}
			double zprowbad[][] = new double[numbad][2];
			for (i = 0, j = 0; i < p; i++) {
				if (bad[i]) {
					zprowbad[j][0] = zprow[i][0];
					zprowbad[j++][1] = zprow[i][1];
				}
			}
			double tmp[] = new double[numbad];
			int s[] = new int[numbad];
			for (i = 0; i < numbad; i++) {
				tmp[i] = Double.MAX_VALUE;
				s[i] = -1;
				for (j = 0; j < n; n++) {
					double presentTmp = zabs(zprowbad[i][0] - zf[j][0], zprowbad[i][1] - zf[j][1]);
					if (presentTmp < tmp[i]) {
						tmp[i] = presentTmp;
						s[i] = j;
					}
				}
			} // for (i = 0; i < p; i++)
			for (i = 0, j = 0; i < p; i++) {
				if (bad[i]) {
					sing[i] = s[j++];
				}
			} // for (i = 0, j = 0; i < p; i++)
			
			// Because we no longer integrate from the closest prevertex,  we
			// must go in stages to maintain accuracy.
			mid1 = new double[numbad][2];
			mid2 = new double[numbad][2];
			for (i = 0; i < numbad; i++) {
				mid1[i][0] = z[s[i]][0];
				mid1[i][1] = 0.5;
			}
			for (i = 0, j = 0; i < p; i++) {
			    if (bad[i]) {
			    	mid2[j][0] = zp[i][0];
			    	mid2[j++][1] = 0.5;
			    }
			} // for (i = 0, j = 0; i < p; i++)
		} // if (numbad > 0)
		else {
			// all clear
			bad = new boolean[p];
		}
		
		// zs = the starting singularities
		double zs[][] = new double[p][2];
		for (i = 0; i < p; i++) {
			zs[i][0] = z[sing[i]][0];
			zs[i][1] = z[sing[i]][1];
		}
		// ws = map(zs)
		double ws[][] = new double[p][2];
		for (i = 0; i < p; i++) {
			ws[i][0] = w[sing[i]][0];
			ws[i][1] = w[sing[i]][1];
		}
		
		// Compute the map directly at "normal" points.
		boolean normal[] = new boolean[p];
		int numnormal = 0;
		for (i = 0; i < p; i++) {
			normal[i] = (!bad[i]) && (!vertex[i]);
			if (normal[i]) {
				numnormal++;
			}
		} // for (i = 0; i < p; i++) 
		
		if (numnormal > 0) {
		    double zsnormal[][] = new double[numnormal][2];
		    double zpnormal[][] = new double[numnormal][2];
		    int singnormal[] = new int[numnormal];
		    for (i = 0, j = 0; i < p; i++) {
		        if (normal[i]) {
		        	zsnormal[j][0] = zs[i][0];
		        	zsnormal[j][1] = zs[i][1];
		        	zpnormal[j][0] = zp[i][0];
		        	zpnormal[j][1] = zp[i][1];
		        	singnormal[j++] = sing[i];
		        }
		    } // for (i = 0, j = 0; i < p; i++)
		    double I[][] = stquad(zsnormal, zpnormal, singnormal, z, beta, qdat2);
		    for (i = 0, j = 0; i < p; i++) {
		    	if (normal[i]) {
		    		zmlt(c[0], c[1], I[j][0], I[j][1], cr, ci);
		    		j++;
		    		wp[i][0] = ws[i][0] + cr[0];
		    		wp[i][1] = ws[i][1] + ci[0];
		    	}
		    } // for (i = 0, j = 0; i < p; i++)
		} // if (numnormal > 0)
		
		// Compute map at "bad" points in stages.
		if (numbad > 0) {
			double zsbad[][] = new double[numbad][2];
			double zpbad[][] = new double[numbad][2];
			int singbad[]= new int[numbad];
			int zerosbad[] = new int[numbad];
			for (i = 0, j = 0; i < p; i++) {
			    if (bad[i]) {
			    	zsbad[j][0] = zs[i][0];
			    	zsbad[j][1] = zs[i][1];
			    	zpbad[j][0] = zp[i][0];
			    	zpbad[j][1] = zp[i][1];
			    	singbad[j++] = sing[i];
			    }
			} // for (i = 0, j = 0; i < p; i++)
			double I1[][] = stquad(zsbad, mid1, singbad, z, beta, qdat2);
			double I2[][] = stquadh(mid1, mid2, zerosbad, z, beta, qdat2);
			double I3[][] = stquad(zpbad, mid2, zerosbad, z, beta, qdat2);
			for (i = 0, j = 0; i < p; i++) {
			    if (bad[i]) {
			    	zmlt(c[0], c[1], (I1[j][0] + I2[j][0] - I3[j][0]), 
			    			(I1[j][1] + I2[j][1] - I3[j][1]), cr, ci);
			    	j++;
			    	wp[i][0] = ws[i][0] + cr[0];
			    	wp[i][1] = ws[i][1] + ci[0];
			    }
			} // for (i = 0, j = 0; i < p; i++)
		} // if (numbad > 0)
	}
	
	private ViewJFrameGraph plotpoly(float xPointArray[], float yPointArray[],
			double w[][], double beta[], boolean addMarkerLabel, double axlim[]) {
		// plotpoly plots the polygon whose vertices are in vector w
		// and whose turning angles are in beta.  Vertices at infinity
		// permitted, but there must be at least two consecutive finite
		// vertices somewhere in w.
		// If addMarkerLabel is true markers and numeric labels are put at vertices
		// Extracted from original MATLAB routine by Toby Driscoll copyright 1998.
		int n = w.length;
		int numfinite = 0;
		int numinfinite = 0;
		int i, j;
		double theta;
		double Rx[] = new double[2];
		double Ry[] = new double[2];
		double RR[] = new double[4];
		double lim[] = new double[4];
		double denom;
		double costheta;
		double sintheta;
		double minRR;
		boolean atinf[] = new boolean[w.length];
		for (i = 0; i < w.length; i++) {
			if ((!Double.isInfinite(w[i][0])) && (!Double.isInfinite(w[i][1]))) {
				numfinite++;
			}
			else {
				numinfinite++;
				atinf[i] = true;
			}
		}
		double wf[][] = new double[numfinite][2];
		for (i = 0, j = 0; i < w.length; i++) {
			if ((!Double.isInfinite(w[i][0])) && (!Double.isInfinite(w[i][1]))) {
				wf[j][0] = w[i][0];
				wf[j++][1] = w[i][1];
			}
		}
		
		double minrealwf = Double.MAX_VALUE;
		double maxrealwf = -Double.MAX_VALUE;
		double minimagwf = Double.MAX_VALUE;
		double maximagwf = -Double.MAX_VALUE;
		for (i = 0; i < wf.length; i++) {
			if (wf[i][0] < minrealwf) {
				minrealwf = wf[i][0];
			}
			if (wf[i][0] > maxrealwf) {
				maxrealwf = wf[i][0];
			}
			if (wf[i][1] < minimagwf) {
				minimagwf = wf[i][1];
			}
			if (wf[i][1] > maximagwf) {
				maximagwf = wf[i][1];
			}
		}
		lim[0] = minrealwf;
		lim[1] = maxrealwf;
		lim[2] = minimagwf;
		lim[3] = maximagwf;
		for (i = 0; i < 4; i++) {
			axlim[i] = lim[i];
		}
		double maxdiff = Math.max(lim[1] - lim[0], lim[3] - lim[2]);
		double fac = 0.6;
		if (numinfinite > 0) {
			fac = 0.7;
		}
		double meanlim01 = (lim[0] + lim[1])/2.0;
		double meanlim23 = (lim[2] + lim[3])/2.0;
		lim[0] = meanlim01 - fac * maxdiff;
		lim[1] = meanlim01 + fac * maxdiff;
		lim[2] = meanlim23 - fac * maxdiff;
		lim[3] = meanlim23 + fac * maxdiff;
		double R = Math.max(lim[1] - lim[0], lim[3] - lim[2]);
		int first = -1;
		for (i = 0; i < n-1 && (first == -1); i++) {
			if ((!atinf[i]) && (!atinf[i+1])) {
			    first = i;	
			}
		}
		if ((first == -1) && (!atinf[n-1]) && (!atinf[0])) {
			first = n-1;
		}
		if (first == -1) {
			MipavUtil.displayError("There must be two consecutive finite vertices.");
			return null;
		}
		int renum[] = new int[n];
		for (i = 0; i < n - first; i++) {
			renum[i] = first +i;
		}
		for (i = 0; i < first; i++) {
			renum[n-first+i] = i;
		}
		double wrenum[][] = new double[n][2];
		double betarenum[] = new double[n];
		boolean atinfrenum[] = new boolean[n];
		for (i = 0; i < n; i++) {
			wrenum[i][0] = w[renum[i]][0];
			wrenum[i][1] = w[renum[i]][1];
			betarenum[i] = beta[renum[i]];
			atinfrenum[i] = atinf[renum[i]];
		}
		
		// First edge
		//double edgeh[] = new double[n];
		double lblh[][] = new double[n][2];
		for (i = 0; i < n; i++) {
			for (j = 0; j < 2; j++) {
				lblh[i][j] = Double.NaN;
			}
		}
		String lblhString[][] = new String[n][2];
		double lblhx[][] = new double[n][2];
		double lblhy[][] = new double[n][2];
		xPointArray[0] = (float)wrenum[0][0];
		yPointArray[0] = (float)wrenum[0][1];
		xPointArray[1] = (float)wrenum[1][0];
		yPointArray[1] = (float)wrenum[1][1];
		xPointArray[n] = xPointArray[0];
		yPointArray[n] = yPointArray[0];
		
		double ang = Math.atan2(wrenum[1][1] - wrenum[0][1], wrenum[1][0] - wrenum[0][0]);
		
		// Remaining edges
		j = 2;
		while (j <= n) {
		    int jp1 = (j%n)+1;
		    
		    // Draw marker/label
		    if (addMarkerLabel) {
		    	theta = ang;
		    	// May need to modify position of label
		    	if ((Math.abs(betarenum[j-2]-1) < 3.0*eps) ||
		    			(Math.abs(betarenum[jp1-1] - 1) < 3.0*eps)) {
		    		// Next to a slit, perturb label inside
		    		theta = theta + Math.PI - (betarenum[j-1]+ 1)*Math.PI/2.0;
		    	}
		    	else if (Math.abs(betarenum[j-1]) < 3.0*eps) {
		            // For a "trivial" vertex, put number outside
		    		theta = theta - Math.PI/2.0;
		    	}
		    	// Mark label; markers will be added last
		    	lblhx[j-1][1] = wrenum[j-1][0] + 0.035*R*Math.cos(theta);
		    	lblhy[j-1][1] = wrenum[j-1][1] + 0.035*R*Math.sin(theta);
		    	lblhString[j-1][1] =String.valueOf(renum[j-1]);
		    } // if (addMarkerLabel)
		    
		    // Next edge
		    if (!atinfrenum[jp1-1]) {
		    	// Bounded edge; straightforward
		    	xPointArray[j] = (float)wrenum[jp1-1][0];
		    	yPointArray[j] = (float)wrenum[jp1-1][1];
		    	ang = ang - Math.PI*betarenum[j-1];
		    	j = j+1;
		    } // if (!atinfrenum[jp1-1])
		    else {
		        // Unbounded edge (first of two consecutive)
		    	ang= ang - Math.PI*betarenum[j-1];
		    	xPointArray[j] = (float)(wrenum[j-1][0] + R*Math.cos(ang));
		    	yPointArray[j] = (float)(wrenum[j-1][1] + R*Math.sin(ang));
		    	
		    	
		    	// Make first label outside axes box
		    	if (addMarkerLabel) {
		    		theta = ang;
		    		costheta = Math.cos(theta);
		    		if (costheta == 0) {
		    			denom = eps;
		    		}
		    		else {
		    			denom = costheta;
		    		}
		    		Rx[0] = (lim[0] - wrenum[j-1][0])/denom;
		    		Rx[1] = (lim[1] - wrenum[j-1][0])/denom;
		    		sintheta = Math.sin(theta);
		    		if (sintheta == 0) {
		    			denom = eps;
		    		}
		    		else {
		    			denom = sintheta;
		    		}
		    		Ry[0] = (lim[2] - wrenum[j-1][1])/denom;
		    		Ry[1] = (lim[3] - wrenum[j-1][1])/denom;
		    		RR[0] = Rx[0];
		    		RR[1] = Rx[1];
		    		RR[2] = Ry[0];
		    		RR[3] = Ry[1];
		    		minRR = Double.MAX_VALUE;
		    		for (i = 0; i <= 3; i++) {
		    		    if ((RR[i] > 0)	&& (RR[i] < minRR)) {
		    		    	minRR = RR[i];
		    		    }   		
		    		}
		    		if (minRR == Double.MAX_VALUE) {
		    			minRR = 0.0;
		    		}
		    		lblhx[j][0] = wrenum[j-1][0] + (minRR + 0.07*R)*costheta;
		    		lblhy[j][0] = wrenum[j-1][1] + (minRR + 0.07*R)*sintheta;
		    		lblhString[j][0] = renum[j] + " (inf)";
		    	} // if (addMarkerLabel)
		    	
		    	// Second unbounded edge
		    	ang = ang - Math.PI*betarenum[jp1-1];
		    	xPointArray[j] = (float)(wrenum[(j+1)%n][0] - R*Math.cos(ang));
		    	yPointArray[j] = (float)(wrenum[(j+1)%n][1] - R*Math.sin(ang));
		    	xPointArray[j+1] = (float)(wrenum[(j+1)%n][0]);
		    	xPointArray[j+1] = (float)(wrenum[(j+1)%n][1]);
		    	if (addMarkerLabel) {
		    	    theta = ang + Math.PI;
		    	    costheta = Math.cos(theta);
		    		if (costheta == 0) {
		    			denom = eps;
		    		}
		    		else {
		    			denom = costheta;
		    		}
		    		Rx[0] = (lim[0] - wrenum[(j+1)%n][0])/denom;
		    		Rx[1] = (lim[1] - wrenum[(j+1)%n][0])/denom;
		    		sintheta = Math.sin(theta);
		    		if (sintheta == 0) {
		    			denom = eps;
		    		}
		    		else {
		    			denom = sintheta;
		    		}
		    		Ry[0] = (lim[2] - wrenum[(j+1)%n][1])/denom;
		    		Ry[1] = (lim[3] - wrenum[(j+1)%n][1])/denom;
		    		RR[0] = Rx[0];
		    		RR[1] = Rx[1];
		    		RR[2] = Ry[0];
		    		RR[3] = Ry[1];
		    		minRR = Double.MAX_VALUE;
		    		for (i = 0; i <= 3; i++) {
		    		    if ((RR[i] > 0)	&& (RR[i] < minRR)) {
		    		    	minRR = RR[i];
		    		    }   		
		    		}
		    		if (minRR == Double.MAX_VALUE) {
		    			minRR = 0.0;
		    		}
		    		lblhx[j][1] = wrenum[(j+1)%n][0] + (minRR + 0.07*R)*costheta;
		    		lblhy[j][1] = wrenum[(j+1)%n][1] + (minRR + 0.07*R)*sintheta;
		    		lblhString[j][1] = renum[j] + " (inf)";

		    	} // if (addMarkerLabel)
		    	
		    	// We've done tow
		    	j = j+2;
		    } // else
		} // while (j <= n)
		
		// Last item, label for first point
		if (addMarkerLabel) {
			theta = ang;
			if ((Math.abs(betarenum[n-1]-1) < 3.0*eps) || 
					(Math.abs(betarenum[0]) < 3.0*eps) || (Math.abs(betarenum[1]-1) < 3.0*eps)) {
				theta = theta + Math.PI - (betarenum[0] + 1)*Math.PI/2.0;
			}
			lblhx[0][1] = (int)Math.round(wrenum[0][0] + 0.035*R*Math.cos(theta));
			lblhy[0][1] = (int)Math.round(wrenum[0][1] + 0.035*R*Math.sin(theta));
			lblhString[0][1] = String.valueOf(renum[0]);
		} // if (addMarkerLabel)
		
		// Plot markers last, to keep them on top
		// Not needed with SHOW_POINTS_AND_LINES
		//if (addMarkerLabel) {
		   // for (j = 0; j < n; j++) {
		    	//if (!atinfrenum[j]) {
		    		//lblhx[j][0] = wrenum[j][0];
		    		//lblhy[j][0] = wrenum[j][1];
		    		//lblhString[j][0] = ".";
		    	//}
		   // }
		//} // if (addMarkerLabel)
		
		ViewJFrameGraph pointGraph = new ViewJFrameGraph(xPointArray, yPointArray,
				"title", "lablelX", "labelY", Color.BLUE);
		pointGraph.setVisible(true);
		ViewJComponentGraph graph = pointGraph.getGraph();
		if (addMarkerLabel) {
		    graph.setPointsAndLinesDisplay(ViewJComponentGraph.SHOW_POINTS_AND_LINES);
		}
		else {
			 graph.setPointsAndLinesDisplay(ViewJComponentGraph.SHOW_LINES_ONLY);	
		}
		Graphics g = pointGraph.getFrameGraphics();
		graph.paintComponent(g);
		Rectangle graphBounds = graph.getGraphBounds();
		double xScale = graphBounds.width / (maxrealwf - minrealwf);
        double yScale = graphBounds.height / (maximagwf - minimagwf);
        for (i = 0; i < n; i++) {
        	for (j = 0; j < 2; j++) {
        		if (lblhString[i][j] != null) {
        			int posX =  (int)Math.round(graphBounds.x + xScale*(lblhx[i][j] - minrealwf));
        			int posY =  (int)Math.round(graphBounds.y + yScale*(lblhy[i][j] - minimagwf));
        			posY = -posY + 2*graphBounds.y + graphBounds.height;
        			graph.drawString(g, lblhString[i][j], posX, posY);
        		}
        	}
        }
        
		return pointGraph;
	}
	
	private double rectAccuracy(scmap M) {
		// Apparent accuracy of Schwarz-Christoffel rectangle map.
		// accuracy estimates the accuracy of the Schwarz-Christoffel rectangle map M.
		// The technique used is to compare the differences between successive finite
		// vertices to the integral between the corresponding prevertices, and return
		// the maximum.
		// Original MATLAB routine copyright 1998 by Toby Driscoll.
		int i, j;
		double acc;
		double cr[] = new double[1];
		double ci[] = new double[1];
		// If an accuracy has been assigned, don't question it.
		if (!Double.isNaN(M.accuracy)) {
			return M.accuracy;
		}
		// Get data for low-level functions
		polygon p = M.poly;
		double w[][] = p.vertex;
		int n = w.length;
		double beta[] = new double[p.angle.length];
		for (i = 0; i < p.angle.length; i++) {
			beta[i] = p.angle[i] - 1;
		}
		double z[][] = M.prevertex;
		double c[] = new double[2];
		c[0] = M.constant[0];
		c[1] = M.constant[1];
		double L[] = M.stripL;
		double qdata[][] = M.qdata;
		
		// Remember to put first corner first
		int corner[] = new int[4];
		int renum[] = new int[n];
		rcorners(w, beta, z, corner, renum);
		
		// Map prevertices to strip
		double K = -Double.MAX_VALUE;
		for (i = 0; i < n; i++) {
			if (z[i][0] > K) {
				K = z[i][0];
			}
		}
		double Kp = -Double.MAX_VALUE;
		for (i = 0; i < n; i++) {
			if (z[i][1] > Kp) {
				Kp = z[i][1];
			}
		}
		double zs[][] = new double[z.length][2];
		double zsprime[][] = new double[z.length][2];
		r2strip(zs, zsprime, z, z, L[0]);
		for (i = 0; i < zs.length; i++) {
			// Put them *exactly* on edges
			zs[i][1] = Math.round(zs[i][1]);
		}
		
		// Integrate between consecutive finite pairs on bottom and top
		int numidxbot = 0;
		for (i = 0; i <=corner[2]-1; i++) {
			if (isFinite(w[i][0]) && isFinite(w[i][1])) {
				numidxbot++;
			}
		}
		int idxbot[] = new int[numidxbot];
		for (i = 0, j = 0; i <= corner[2] -1; i++) {
			if (isFinite(w[i][0]) && isFinite(w[i][1])) {
				idxbot[j++] = i;
			}
		}
		int numidxtop = 0;
		for (i = corner[2]; i <= n-1; i++) {
			if (isFinite(w[i][0]) && isFinite(w[i][1])) {
				numidxtop++;
			}	
		}
		int idxtop[] = new int[numidxtop];
		for (i = corner[2], j = 0; i <= n-1; i++) {
			if (isFinite(w[i][0]) && isFinite(w[i][1])) {
				idxtop[j++] = i;
			}	
		}
		// Two columns hold endpoint indices for integrations
		int idx[][] = new int[numidxbot+numidxtop-2][2];
		for (i = 0; i < numidxbot-1; i++) {
			idx[i][0] = idxbot[i];
			idx[i][1] = idxbot[i+1];
		}
		
		for (i = 0; i < numidxtop-1; i++) {
			idx[numidxbot-1+i][0] = idxtop[i];
			idx[numidxbot-1+i][1] = idxtop[i+1];
		}
		
		// Find midpoints.  Go into the interior of strip to avoid integrating
		// through skipped prevertices.
		double zz[][][] = new double[2][numidxbot+numidxtop-2][2];
		for (i = 0; i < numidxbot+numidxtop-2; i++) {
			for (j = 0; j < 2; j++) {
				zz[j][i][0] = zs[idx[i][j]][0];
				zz[j][i][1] = zs[idx[i][j]][1];
			}
		}
		double mid[][] = new double[numidxbot+numidxtop-2][2];
		for (j = 0; j < numidxbot+numidxtop-2; j++) {
			for (i = 0; i < 2; i++) {
				mid[j][0] += zz[i][j][0];
				mid[j][1] += zz[i][j][1];
			}
		}
		for (i = 0; i < numidxbot+numidxtop-2; i++) {
			mid[i][0] = mid[i][0]/2.0;
			mid[i][1] = 0.5;
		}
		
		// As a final check, integrate once across the strip
		double tmp = Double.MAX_VALUE;
		int k = -1;
		for (i = 0; i < numidxtop; i++) {
			if (Math.abs(zs[idxtop[i]][0] - zs[0][0]) < tmp) {
			    tmp = Math.abs(zs[idxtop[i]][0] - zs[0][0]);
			    k = i;
			}
		}
		int idx2[][] = new int[numidxbot+numidxtop-1][2];
		for (i = 0; i < numidxbot+numidxtop-2; i++) {
			for (j = 0; j < 2; j++) {
			    idx2[i][j] = idx[i][j];	
			}
		}
		idx2[numidxbot+numidxtop-2][0] = 0;
		idx2[numidxbot+numidxtop-2][1] = idxtop[k];
		double mid2[][] = new double[numidxbot+numidxtop-1][2];
		for (i = 0; i < numidxbot+numidxtop-2; i++) {
			mid2[i][0] = mid[i][0];
			mid2[i][1] = mid[i][1];
		}
		mid2[numidxbot+numidxtop-2][0] = (zs[0][0] + zs[idxtop[k]][0])/2.0;
		mid2[numidxbot+numidxtop-2][1] = (zs[0][1] + zs[idxtop[k]][1])/2.0;
		
		// Add in ends of strip
		int ends[] = new int[2];
		for (i = 0, j = 0; i < n-1 && j < 2; i++) {
			if (zs[i+1][1] != zs[i][1]) {
				ends[j++] = i;
			}
		}
		if (j < 2) {
			if (zs[n-1][1] != zs[0][1]) {
				ends[j++] = n-1;
			}
		}
		double zq[][] = new double[n+2][2];
		double bq[] = new double[n+2];
		double wq[][] = new double[n+2][2];
		for (i = 0; i <= ends[0]; i++) {
		    zq[i][0] = zs[i][0];
		    zq[i][1] = zs[i][1];
		    bq[i] = beta[i];
		    wq[i][0] = w[i][0];
		    wq[i][1] = w[i][1];
		}
		zq[ends[0]+1][0] = Double.POSITIVE_INFINITY;
		zq[ends[0]+1][1] = 0.0;
		bq[ends[0]+1] = 0.0;
		wq[ends[0]+1][0] = Double.NaN;
		wq[ends[0]+1][1] = 0.0;
		for (i = ends[0]+1; i <= ends[1]; i++) {
			zq[i+1][0] = zs[i][0];
			zq[i+1][1] = zs[i][1];
		    bq[i+1] = beta[i];
		    wq[i+1][0] = w[i][0];
		    wq[i+1][1] = w[i][1];
		}
		zq[ends[1]+2][0] = Double.NEGATIVE_INFINITY;
		zq[ends[1]+2][1] = 0.0;
		bq[ends[1]+2] = 0.0;
		wq[ends[1]+2][0] = Double.NaN;
		wq[ends[1]+2][1] = 0.0;
		for (i = ends[1]+1; i < n; i++) {
			zq[i+2][0] = zs[i][0];
			zq[i+2][1] = zs[i][1];
		    bq[i+2] = beta[i];
		    wq[i+2][0] = w[i][0];
		    wq[i+2][1] = w[i][1];	
		}
		// Extend qdat with useless columns at ends
		double qdata2[][] = new double[qdata.length][2*n+6];
		for (i = 0; i < qdata.length; i++) {
			for (j = 0; j <= ends[0]; j++) {
				qdata2[i][j] = qdata[i][j];
			}
			qdata2[i][ends[0]+1] = qdata[i][n];
			for (j = ends[0]+1; j <= ends[1]; j++) {
				qdata2[i][j+1] = qdata[i][j];
			}
			qdata2[i][ends[1]+2] = qdata[i][n];
		    for (j = ends[1]+1; j < n; j++) {
			   qdata2[i][j+2] = qdata[i][j];
		    }
		   qdata2[i][n+2] = qdata[i][n];
		   for (j = n+1; j <= ends[0]+n+1; j++) {
				qdata2[i][j+2] = qdata[i][j];
			}
		   qdata2[i][ends[0]+n+4] = qdata[i][2*n+1];
		   for (j = ends[0]+n+2; j <= ends[1]+n+1; j++) {
			   qdata2[i][j+3] = qdata[i][j];   
		   }
		   qdata2[i][ends[1]+n+5] = qdata[i][2*n+1];
		   for (j = ends[1]+n+2; j < 2*n+1; j++) {
			   qdata2[i][j+4] = qdata[i][j];   
		   }
		   qdata2[i][2*n+5] = qdata[i][2*n+1];
		}
		
		// Do the integrations
		double zleft[][] = new double[idx2.length][2];
		double zright[][] = new double[idx2.length][2];
		for (i = 0; i < idx2.length; i++) {
			zleft[i][0] = zs[idx2[i][0]][0];
			zleft[i][1] = zs[idx2[i][0]][1];
			zright[i][0] = zs[idx2[i][1]][0];
			zright[i][1] = zs[idx2[i][1]][1];
		}
		int idx3[][] = new int[idx2.length][2];
		for (i = 0; i < idx2.length; i++) {
			for (j = 0; j < 2; j++) {
				if (idx2[i][j] > ends[1]) {
					idx3[i][j] = idx2[i][j]+2;
				}
				else if (idx2[i][j] > ends[0]) {
					idx3[i][j] = idx2[i][j] +1;
				}
				else {
					idx3[i][j] = idx2[i][j];
				}
			}
		}
		int idxc0[] = new int[idx3.length];
		int idxc1[] = new int[idx3.length];
		for (i = 0; i < idx3.length; i++) {
			idxc0[i] = idx3[i][0];
			idxc1[i] = idx3[i][1];
		}
		
		double I1[][] = stquad(zleft, mid2, idxc0, zq, bq, qdata2);
		double I2[][] = stquad(zright, mid2, idxc1, zq, bq, qdata2);
		double I[][] = new double[I1.length][I1[0].length];
		for (i = 0; i < I1.length; i++) {
			for (j = 0; j < I1[0].length; j++) {
				I[i][j] = I1[i][j] - I2[i][j];
			}
		}
		//double diffw[][][] = new double[idx3[0].length-1][idx3.length][2];
		// idx3[0].length = 2
		double diffw[][] = new double[idx3.length][2];
		for (i = 0; i < idx3.length; i++) {
			diffw[i][0] = wq[idx3[i][1]][0] - wq[idx3[i][0]][0];
			diffw[i][1] = wq[idx3[i][1]][1] - wq[idx3[i][0]][1];
		}
		double absdiff[] = new double[idx3.length];
		for (i = 0; i < idx3.length; i++) {
			zmlt(c[0], c[1], I[i][0], I[i][1], cr, ci);
			absdiff[i] = zabs(cr[0] - diffw[i][0], ci[0] - diffw[i][1]);
		}
		acc = -Double.MAX_VALUE;
		for (i = 0; i < idx3.length; i++) {
			if (absdiff[i] > acc) {
				acc = absdiff[i];
			}
		}
		System.out.println("rectmap accuracy = " + acc);
		return acc;
	}
	
	public class scmap {
	    double prevertex[][];
	    double constant[] = new double[]{Double.NaN, Double.NaN};
	    double stripL[];
	    double qdata[][];
	    polygon poly;
	    double center[] = new double[]{Double.NaN, Double.NaN};
	    double accuracy = Double.NaN;
	    String className = "rectmap";
	    scmap() {
	    	
	    }
	}
	
	private void rcorners(double w[][], double beta[], double z[][], int corners[], int renum[]) {
		// w, beta, and z are input/output
		// w, beta, and z have the same length n
		// corners and renum are output
		// int corners[] = new int[4]
		// int renum[] = new int[w.length] = new int[n];
		// Find corners of rectangle whose map is represented by prevertices z
		// on the strip, then renumber w, beta, and z (and the corners) so that
		// corners[0] = 0.
		// Original MATLAB routine copyright 1998 by Toby Driscoll
		int left;
		int right;
		int top;
		int bot;
		int i, j;
		int offset = -1;
		int n = w.length;
		
		// Deduce corner locations
		double minrealz = Double.MAX_VALUE;
		double maxrealz = -Double.MAX_VALUE;
		double minimagz = Double.MAX_VALUE;
		double maximagz = -Double.MAX_VALUE;
		for (i = 0; i < n; i++) {
		    if (z[i][0] < minrealz) {
		    	minrealz = z[i][0];
		    }
		    if (z[i][0] > maxrealz) {
		    	maxrealz = z[i][0];
		    }
		    if (z[i][1] < minimagz) {
		    	minimagz = z[i][1];
		    }
		    if (z[i][1] > maximagz) {
		    	maximagz = z[i][1];
		    }
		} // for (i = 0; i < n; i++)
		for (i = 0, j = 0; i < n; i++) {
		    if (Math.abs(z[i][0] - minrealz) < eps) {
		    	left = 1;
		    }
		    else {
		    	left = 0;
		    }
		    if (Math.abs(z[i][0] - maxrealz) < eps) {
		    	right = 1;
		    }
		    else {
		    	right = 0;
		    }
		    if (Math.abs(z[i][1] - maximagz) < eps) {
		    	top = 1;
		    }
		    else {
		    	top = 0;
		    }
		    if (Math.abs(z[i][1] - minimagz) < eps) {
		    	bot = 1;
		    }
		    else {
		    	bot = 0;
		    }
		    if (left + right + top + bot == 2) {
		    	corners[j] = i;
		    	if (zabs(z[i][0] - maxrealz, z[i][1]) < eps) {
		    		offset = j;
		    	}
		    	j++;
		    }
		} // for (i = 0; i < n; i++)
		int tempcorners[] = new int[4];
		for (i = offset; i <= 3; i++) {
		    tempcorners[i-offset] = corners[i];	
		}
		for (i = 0; i <= offset-1; i++) {
			tempcorners[4-offset+i] = corners[i];
		}
		for (i = 0; i <= 3; i++) {
			corners[i] = tempcorners[i];
		}
		
		// Renumber vertices so that corners[0] = 0
		double wtemp[][] = new double[n][2];
		double betatemp[] = new double[n];
		double ztemp[][] = new double[n][2];
		for (i = corners[0]; i <= n-1; i++) {
			renum[i-corners[0]] = i;
			wtemp[i-corners[0]][0] = w[i][0];
			wtemp[i-corners[0]][1] = w[i][1];
			betatemp[i-corners[0]] = beta[i];
			ztemp[i-corners[0]][0] = z[i][0];
			ztemp[i-corners[0]][1] = z[i][1];
		}
		for (i = 0; i <= corners[0]-1; i++) {
			renum[n-corners[0]+i] = i;
			wtemp[n-corners[0]+i][0] = w[i][0];
			wtemp[n-corners[0]+i][1] = w[i][1];
			betatemp[n-corners[0]+i] = beta[i];
			ztemp[n-corners[0]+i][0] = z[i][0];
			ztemp[n-corners[0]+i][1] = z[i][1];
		}
		for (i = 0; i < n; i++) {
			w[i][0] = wtemp[i][0];
			w[i][1] = wtemp[i][1];
			beta[i] = betatemp[i];
			z[i][0] = ztemp[i][0];
			z[i][1] = ztemp[i][1];
		}
		for (i = 0; i < n; i++) {
			wtemp[i] = null;
			ztemp[i] = null;
		}
		wtemp = null;
		betatemp = null;
		ztemp = null;
		for (i = 0; i <= 3; i++) {
			corners[i] = ((tempcorners[i]-tempcorners[0] + n)%n);
		}
		tempcorners = null;
	}
	
	// Construct polygon object
	// x and y are 2 double[] vectors used to specify the vertices.
	// alpha if supplied manually specifies the interior angles at the vedrtices, divided by PI
	// polygon accepts unbounded polygons (vertices at infinity).  However, you must
	// supply alpha, and the vertices must be in counterclockwise order about the interior.
	// Ported from original MATLAB routine copyright 1998 by Toby Driscoll.
	private class polygon {
		double vertex[][];
		double angle[];
		String className = "polygon";
		
		
		polygon(double x[], double y[], double alpha[]) {
			int n;
			// Vertices passed as two real vectors
			// If first point is repeated at the end, delete the second copy
			double xdiff = x[x.length-1] - x[0];
			double ydiff = y[x.length-1] - y[0];
			if (zabs(xdiff, ydiff) < 3.0*eps) {
			    n = x.length-1;	
			} // if (zabs(xdiff, ydiff) < 3.0*eps)
			else {
				n = x.length;
			}
			vertex = new double[n][2];
			if ((alpha != null) && (alpha.length == x.length)) {
				angle = new double[n];
			}
			for (int i = 0; i < n; i++) {
				vertex[i][0] = x[i];
				vertex[i][1] = y[i];
				if ((alpha != null) && (alpha.length == x.length)) {
					angle[i] = alpha[i];
				}
			}
			
			// Now compute angles if needed
			if (n > 0) {
			    alpha = new double[n];
			    boolean isccw[] = new boolean[1];
			    int index[] = new int[1];
			    angle(alpha, isccw, index, vertex, angle);
			    if (!isccw[0]) {
			    	double vertextemp[][] = new double[n][2];
			    	for (int i = 0; i < n; i++) {
			    	    vertextemp[i][0] = vertex[n-1-i][0];
			    	    vertextemp[i][1] = vertex[n-1-i][1];
			    	}
			    	for (int i = 0; i < n; i++) {
			    		vertex[i][0] = vertextemp[i][0];
			    		vertex[i][1] = vertextemp[i][1];
			    	}
			    	for (int i = 0; i < n; i++) {
			    		vertextemp[i] = null;
			    	}
			    	vertextemp = null;
			    	double alphatemp[] = new double[n];
			    	for (int i = 0; i < n; i++) {
			    		alphatemp[i] = alpha[n-1-i];
			    	}
			    	for (int i = 0; i < n; i++) {
			    		alpha[i] = alphatemp[i];
			    	}
			    	alphatemp = null;
			    } // if (!isccw)
			    if (angle == null) {
			    	angle = new double[n];
			    }
			    for (int i = 0; i < n; i++) {
			    	angle[i] = alpha[i];
			    }
			    if (Math.abs(index[0]) > 1) {
			    	MipavUtil.displayWarning("Polygon is multiple-sheeted");
			    }
			} // if (n > 0)
		} // polygon(double x[], double y[])
	} // private class polygon
	
	// Normalized interior angles of a polygon.
	// alpha contains the returned interior angles, normalized by PI, of the polygon
	// 0 < alpha[j] <= 2 if vertex j is finite, and -2 <= alpha[j] <= 0 if j is an infinite vertex.
	// (This is consistent with the definition as the angle swept from the exiting side through
	// the interior to the incoming side, with the vertices in counterclockwise order.)  It is
	// impossible to compute angles for an unbounded polygon; they must be supplied to the
	// polygon constructor to be well-defined.
	// Original MATLAB routine copyright 1998 by Toby Driscoll
	private void angle(double alpha[], boolean[] isccw, int index[], double w[][], double angle[]) {
		int i, j, k;
		double cr[] = new double[1];
		double ci[] = new double[1];
	    int n = w.length;
	    boolean mask[] = new boolean[n];
	    
	    if ((angle != null) && (angle.length > 0)) {
	    	// If the angles have been assigned, return them
	    	for (i = 0; i < angle.length; i++) {
	    		alpha[i] = angle[i];
	    	}
	    } // if ((angle != null) && (angle.length > 0))
	    else {
	        if (n == 0) {
	        	alpha = null;
	        	isccw = null;
	        	index = null;
	        	return;
	        }
	        
	        for (i = 0; i < n; i++) {
	        	if (Double.isInfinite(w[i][0]) || Double.isInfinite(w[i][1])) {
	        		System.err.println("Cannot compute angles for unbounded polyhgons");
	        		return;
	        	}
	        } // for (i = 0; i < w.length; i++)
	        
	        // Compute angles
	        double incoming[][] = new double[n][2];
	        incoming[0][0] = w[0][0] - w[n-1][0];
	        incoming[0][1] = w[0][1] - w[n-1][1];
	        for (i = 1; i < n; i++) {
	        	incoming[i][0] = w[i][0] - w[i-1][0];
	        	incoming[i][1] = w[i][1] - w[i-1][1];
	        }
	        double outgoing[][] = new double[n][2];
	        for (i = 0; i < n-1; i++) {
	        	outgoing[i][0] = incoming[i+1][0];
	        	outgoing[i][1] = incoming[i+1][1];
	        }
	        outgoing[n-1][0] = incoming[0][0];
	        outgoing[n-1][1] = incoming[0][1];
	        for (i = 0; i < n; i++) {
	            zmlt(-incoming[i][0], -incoming[i][1], outgoing[i][0], -outgoing[i][1], cr, ci);
	            double theta = Math.atan2(ci[0], cr[0]);
	            double tdiv = theta/Math.PI;
	            alpha[i] = tdiv - 2 * Math.floor(tdiv/2);
	        } // for (i = 0; i < n; i++)
	        
	        // It's ill-posed to determine locally the slits (inward-pointing) from 
	        // points (outward-pointing).  Check suspicious cases at the tips to see if
	        // they are interior to the rest of the polygon
	        int nummask = 0;
	        for (i = 0; i < n; i++) {
	        	if ((alpha[i] < 100.0  *eps) || ((2 - alpha[i]) < 100.0 * eps)) {
	        		mask[i] = true;
	        		nummask++;
	        	}
	        } // for (i = 0; i < n; i++)
	        if (nummask == n) {
	        	// This can happen if all vertices are collinear.
	        	for (i = 0; i < alpha.length; i++) {
	        		alpha[i] = 0;
	        	}
	        	isccw[0] = true; // irrelevant
	        	index[0] = 1; // irrelevant
	        	return;
	        } // if (nummask == n)
	        int numnotmask = n - nummask;
	        double wmask[][] = new double[nummask][2];
	        double wnotmask[][] = new double[numnotmask][2];
	        for (i = 0, j = 0, k = 0; i < n; i++) {
	            if (mask[i]) {
	            	wmask[j][0] = w[i][0];
	            	wmask[j++][1] = w[i][1];
	            }
	            else {
	            	wnotmask[k][0] = w[i][0];
	            	wnotmask[k++][1] = w[i][1];
	            }
	        } // for (i = 0, j = 0, k = 0; i < n; i++)
	        boolean slit[] = new boolean[wmask.length];
	        boolean onvtx[][] = new boolean[wnotmask.length][wmask.length];
	        isinpoly(slit, onvtx, wmask, wnotmask, null, eps);
	        for (i = 0; i < alpha.length; i++) {
	        	if (mask[i]) {
	        		if (slit[i]) {
	        			alpha[i] = 2.0;
	        		}
	        		else {
	        			alpha[i] = 0.0;
	        		}
	        	} // if (mask[i])
	        } // for (i = 0; i < alpha.length; i++)
	    } // else
	    
	    // Now test--if incorrect, assume the orientation is clockwise
	    double sindex = 0.0;
	    for (i = 0; i < alpha.length; i++) {
	        sindex += (alpha[i] - 1.0);	
	    }
	    sindex = sindex/2.0; // sindex should be integer
	    if (Math.abs(sindex - Math.round(sindex)) > 100.0*Math.sqrt(n)*eps) {
	    	// Try reversing the interpretation of a crack
	    	sindex = 0.0;
	    	for (i = 0; i < alpha.length; i++) {
	    		if ((alpha[i] < 2.0*eps) || ((2.0 - alpha[i]) < 2.0*eps)) {
	    			mask[i] = true;
	    		}
	    		else {
	    			mask[i] = false;
	    			alpha[i] = 2.0 - alpha[i];
	    		}
	    		sindex += (alpha[i] - 1.0);
	    	} // for (i = 0; i < alpha.length; i++)
	    	sindex = sindex/2.0; // should be integer
	    	// If still not OK, something is wrong
	    	if (Math.abs(sindex - Math.round(sindex)) > 100.0*Math.sqrt(n)*eps) {
	    		System.err.println("Invalid polygon");
	    		return;
	    	}
	    } // if (Math.abs(sindex - Math.round(sindex)) > 100.0*Math.sqrt(n)*eps)
	    
	    index[0] = (int)Math.round(sindex);
	    isccw[0] = (index[0] < 0);
	}
	
	// isinpoly identifies points inside a polygon
	// isinpoly returns a vector the size of z such that each nonzero corresponds to a point inside
	// the polygon defined by w and beta.
	// ontvx = new boolean[w.length][z.length]
	// More precisely, the value returned for a point is the winding number of the polygon about
	// that point.
	// The problem becomes ill-defined for points very near an edge or vertex.  isinpoly considers
	// points within roughly tol of the boundary to be "inside", and computes winding number for such points
	// as the number of conformal images the point ought to have
	// Original MATLAB routine copyright 1998 by Toby Driscoll.
	// Uses the argument principle, with some gymnastics for boundary points.
	private void isinpoly(boolean indexout[], boolean onvtx[][], double z[][], double w[][], double beta[], double tol) {
		int i, j, k, p;
		double cr[] = new double[1];
		double ci[] = new double[1];
	    if (beta == null) {
	    	beta = scangle(w);
	    }
	    int n = w.length;
	    double index[] = new double[z.length];
	    double zcopy[][] = new double[z.length][2];
	    for (i = 0; i < z.length; i++) {
	    	zcopy[i][0] = z[i][0];
	    	zcopy[i][1] = z[i][1];
	    }
	    double wcopy[][] = new double[w.length][2];
	    for (i = 0; i < w.length; i++) {
	    	wcopy[i][0] = w[i][0];
	    	wcopy[i][1] = w[i][1];
	    }
	    
	    
	    // Rescale to make differences relative
	    double diffw[][] = new double[n][2];
	    for (i = 0; i < n-1; i++) {
	        diffw[i][0] = w[i+1][0] - w[i][0];
	        diffw[i][1] = w[i+1][1] - w[i][1];
	    }
	    diffw[n-1][0] = w[0][0] - w[n-1][0];
	    diffw[n-1][1] = w[0][1] - w[n-1][1];
	    double absdiffw[] = new double[n];
	    double sum = 0.0;
	    boolean anygt = false;
	    for (i = 0; i < n; i++) {
	    	absdiffw[i] = zabs(diffw[i][0], diffw[i][1]);
	    	if (absdiffw[i] > eps) {
	    		anygt = true;
	    	}
	    	sum += absdiffw[i];
	    }
	    double scale = sum/n;
	    // Trivial case (e.g., a single or repeated point)
	    if (!anygt) {
	    	return;
	    }
	    
	    for (i = 0; i < n; i++) {
	    	wcopy[i][0] = w[i][0]/scale;
	    	wcopy[i][1] = w[i][1]/scale;
	    }
	    
	    for (i = 0; i < z.length; i++) {
	    	zcopy[i][0] = z[i][0]/scale;
	    	zcopy[i][1] = z[i][1]/scale;
	    }
	    
	    // Array of differences between each z and each w
	    int np = z.length;
	    double d[][][] = new double[n][np][2];
	    for (i = 0; i < n; i++) {
	    	for (j = 0; j < np; j++) {
	    	    d[i][j][0] = wcopy[i][0] - zcopy[j][0];
	    	    d[i][j][1] = wcopy[i][1] - zcopy[j][1];
	    	}
	    } // for (i = 0; i < n; i++)
	    
	    // Avoid divides by zero
	    for (i = 0; i < n; i++) {
	    	for (j = 0; j < np; j++) {
	    		if (zabs(d[i][j][0], d[i][j][1]) < eps) {
	    			d[i][j][0] = eps;
	    			d[i][j][1] = 0.0;
	    		}
	    	}
	    } // for (i = 0; i < n; i++)
	    
	    // Diffs of imag(log(w-z)) around the polygon
	    double ang[][] = new double[n][np];
	    for (i = 0; i < n-1; i++) {
	    	for (j = 0; j < np; j++) {
	    		zdiv(d[i+1][j][0], d[i+1][j][1], d[i][j][0], d[i][j][1], cr, ci);
	    		ang[i][j] = Math.atan2(ci[0], cr[0])/Math.PI;
	    	}	
	    } // for (i = 0; i < n-1; i++)
	    for (j = 0; j < np; j++) {
	    	zdiv(d[0][j][0], d[0][j][1], d[n-1][j][0], d[n-1][j][1], cr, ci);
	    	ang[i][j] = Math.atan2(ci[0], cr[0])/Math.PI;
	    }
	    
	    // Find boundary points (edge and vertex)
	    double wdiff[][] = new double[n][2];
	    for (i = 0; i < n-1; i++) {
	    	wdiff[i][0] = wcopy[i+1][0] - wcopy[i][0];
	    	wdiff[i][1] = wcopy[i+1][1] - wcopy[i][1];
	    }
	    wdiff[n-1][0] = wcopy[0][0] - wcopy[n-1][0];
	    wdiff[n-1][1] = wcopy[0][1] - wcopy[n-1][1];
	    double tangents[][] = new double[n][2];
	    for (i = 0; i < n; i++) {
	    	zdiv(wdiff[i][0], wdiff[i][1], zabs(wdiff[i][0], wdiff[i][1]), 0, cr, ci);
	    	tangents[i][0] = cr[0];
	    	tangents[i][1] = ci[0];
	    } // for (i = 0; i < n; i++)
	    
	    // If points are repeated (e.g. crowding), skip to new point
	    for (p = 0; p < n; p++) {
	    	if ((tangents[p][0] == 0) && (tangents[p][1] == 0)) {
	    		double v[][] = new double[2*n-p-1][2];
	    		for (i = p+1; i < n; i++) {
	    		    v[i-p-1][0] = wcopy[i][0];
	    		    v[i-p-1][1] = wcopy[i][1];
	    		}
	    		for (i = 0; i < n; i++) {
	    		    v[n-p-1+i][0] = wcopy[i][0];
	    		    v[n-p-1+i][1] = wcopy[i][1];
	    		}
	    		boolean found = false;
	    		for (i = 0; i < v.length && (!found); i++) {
	    			if ((v[i][0] != wcopy[p][0]) || (v[i][1] != wcopy[p][1])) {
	    			    found = true;
	    			    double vdiffreal = v[i][0] - wcopy[p][0];
	    			    double vdiffimag = v[i][1] - wcopy[p][1];
	    			    zdiv(vdiffreal, vdiffimag, zabs(vdiffreal, vdiffimag), 0, cr, ci);
	    			    tangents[p][0] = cr[0];
	    			    tangents[p][1] = ci[0];
	    			}
	    		}
	    	} // if ((tangents[p][0] == 0) && (tangents[p][1] == 0))
	    } // for (p = 0; p < n; p++)
	    
	    // Points which are close to an edge
	    boolean onbdy[][] = new boolean[n][np];
	    for (i = 0; i < n; i++) {
	    	for (j = 0; j < np; j++) {
	    		zdiv(d[i][j][0], d[i][j][1],tangents[i][0], tangents[i][1], cr, ci);
	    		if (Math.abs(ci[0]) < 10.0 * tol) {
	    			onbdy[i][j] = true;
	    		}
	    	}
	    } // for (i = 0; i < n; i++)
	    // Points which are essentially vertices
	    for (i = 0; i < n; i++) {
	    	for (j = 0; j < np; j++) {
	    		if (zabs(d[i][j][0], d[i][j][1]) < tol) {
	    			onvtx[i][j] = true;
	    		}
	    	}
	    } // for (i = 0; i < n; i++)
	    // Correction: points must be closed, finite edge segment
	    boolean onvtxshift[][] = new boolean[n][np];
	    for (i = 0; i < n-1; i++) {
	    	for (j = 0; j < np; j++) {
	    		onvtxshift[i][j] = onvtx[i+1][j];
	    	}
	    }
	    for (j = 0; j < np; j++) {
	    	onvtxshift[n-1][j] = onvtx[0][j];
	    }
	    for (i = 0; i < n; i++) {
	    	for (j = 0; j < np; j++) {
	    		onbdy[i][j] = onbdy[i][j] && ((Math.abs(ang[i][j]) > 0.9)  || onvtx[i][j] || onvtxshift[i][j]);
	    	}
	    }
	    
	    // Truly interior points are easy: add up the args
	    boolean interior[] = new boolean[np];
	    boolean anyinterior = false;
	    for (j = 0; j < np; j++) {
	    	boolean found = false;
	    	for (i = 0; i < n && (!found); i++) {
	    		if (!onbdy[i][j]) {
	    			found = true;
	    			interior[j] = true;
	    			anyinterior = true;
	    		}
	    	}
	    } //  for (j = 0; j < np; j++)
	    
	    if (anyinterior) {
	        for (j = 0; j < np; j++) {
	        	if (interior[j]) {
		        	sum = 0.0;
		        	for (i = 0; i < n; i++) {
		        		sum += ang[i][j];
		        	}
		        	sum = sum/2.0;
		        	index[j] = (int)Math.round(sum);
	        	}
	        }
	    } // if (anyinterior) 
	    
	    // Boundary points are tricky
	    for (k = 0; k < np; k++) {
	        if (!interior[k]) {
	            // Index wrt other parts of polygon
	            double S = 0.0;
	        	for (i = 0; i < n; i++) {
	        		if (!onbdy[i][k]) {
	        			S += ang[i][k];
	        		}
	        	}
	        	// We pretend a vertex point is on either adjacent side
	        	int numb = 0;
	        	for (i = 0; i < n; i++) {
	        		if (onvtx[i][k]) {
	        			numb++;
	        		}
	        	}
	        	double b[] = new double[numb];
	        	for (i = 0, j = 0; i < n; i++) {
	        	    if (onvtx[i][k]) {
	        	    	b[j++] = beta[i];
	        	    }
	        	}
	        	// Each edge membership counts as 1/2 winding number (either side)
	        	double augment = 0.0;
	        	for (i = 0; i < n; i++) {
	        		if (onbdy[i][k]) {
	        			augment += 1.0;
	        		}
	        		if (onvtx[i][k]) {
	        			augment -= 1.0;
	        		}
	        	}
	        	for (i = 0; i < b.length; i++) {
	        		augment -= b[i];
	        	}
	        	index[k] = Math.round(augment*sign(S) + S)/2.0;
	        } // if (!interior[k])
	    } // for (k = 0; k < np; k++)
	    
	    for (i = 0; i < index.length; i++) {
	    	if (index[i] != 0) {
	    		indexout[i] = true;
	    	}
	    }
	}
	
	// scangle computes the turning angles of the polygon whose vertices are specified in the vector w.
	// The turning angle of a vertex measures how much the heading changes at that vertex from the
	// incoming to the outgoing edge, normalized by pi.  For a finite vertex, it is equal in absolute
	// value to (exterior angle)/pi, with a negative sign for left turns and positive for right turns.
	// Thus the turn at a finite vertex is in (-1,1], with 1 meaning a slit.  At a infinite vertex the
	// turning angle is in the range [-3, -1] and is equal to the exterior angle of the two sides 
	// extended back from infinity, minus 2.  scangle cannot determine the angle at an infinite vertex
	// or its neighbors, and will return NaN's in those positions.
	// Original code copyright 1998 by Toby Driscoll.
	private double[] scangle(double w[][]) {
		int i;
		double beta[];
		double realPart;
		double imagPart;
		int n = w.length;
		if (n == 0) {
			beta = null;
			return beta;
		}
		// Cannot determine if at an infinite vertex or next to an infinite vertex
		boolean mask[] = new boolean[n];
		for (i = 0; i < n; i++) {
		    mask[i] =true;	
		}
		for (i = 0; i < n; i++) {
			if (i == 0) {
				if ((Double.isInfinite(w[n-1][0])) || (Double.isInfinite(w[n-1][1])) ||
				    (Double.isInfinite(w[i][0])) || (Double.isInfinite(w[i][1])) ||
					(Double.isInfinite(w[i+1][0])) || (Double.isInfinite(w[i+1][1]))) {
					mask[i] = false;
				}
			} // if (i == 0)
			else if (i == n-1) {
				if ((Double.isInfinite(w[i-1][0])) || (Double.isInfinite(w[i-1][1])) ||
				    (Double.isInfinite(w[i][0])) || (Double.isInfinite(w[i][1])) ||
					(Double.isInfinite(w[0][0])) || (Double.isInfinite(w[0][1]))) {
					mask[i] = false;
				}	
			}
			else {
				if ((Double.isInfinite(w[i-1][0])) || (Double.isInfinite(w[i-1][1])) ||
				    (Double.isInfinite(w[i][0])) || (Double.isInfinite(w[i][1])) ||
					(Double.isInfinite(w[i+1][0])) || (Double.isInfinite(w[i+1][1]))) {
					mask[i] = false;
				}
			}
		}
		double dw[][] = new double[n][2];
		dw[0][0] = w[0][0] - w[n-1][0];
		dw[0][1] = w[0][1] - w[n-1][1];
		for (i = 0; i < n-1; i++) {
			dw[i+1][0] = w[i+1][0] - w[i][0];
			dw[i+1][1] = w[i+1][1] - w[i][1];
		}
		double dwshift[][] = new double[n][2];
		for (i = 0; i < n-1; i++) {
			dwshift[i][0] = dw[i+1][0];
			dwshift[i][1] = dw[i+1][1];
		}
		dwshift[n-1][0] = dw[0][0];
		dwshift[n-1][1] = dw[0][1];
		beta = new double[w.length];
		for (i = 0; i < beta.length; i++) {
			beta[i] = Double.NaN;
		}
		for (i = 0; i < n; i++) {
			if (mask[i]) {
				realPart = (dw[i][0]*dwshift[i][0] + dw[i][1] * dwshift[i][1]);
				imagPart = (-dw[i][0]*dwshift[i][1] + dw[i][1] * dwshift[i][0]);
				beta[i] = Math.atan2(imagPart, realPart)/Math.PI;
				// It's ill-posed to tell a point (outward) from a slit (inward).
				// Since the latter is much more common and important, we'll be generous in
				// giving it the tie.
				if (Math.abs(beta[i]+1) < 1.0E-12) {
					beta[i] = 1.0;
				}
			}
		}
		return beta;
	}
	
	// Schwarz-Crhistoffel disk parameter problem.
	// dparam soves the Schwarz-Christoffel mapping parameter problem with the disk
	// as fundamental domain and the polygon specified by w as the target. w must be
	// a vector of the vertices of the polygon, specified in counterclockwise order,
	// and beta should be a vector of the turning angles of the polygon, see scangle
	// for details.  If successful, dparam will return z, a vector of the pre-images of w;
	// c, the multiplicative constant of the conformal map; and qdat, an optional
	// matrix of quadrature data used by some of the other S-C routines. z0 is an initial
	// guess for z.  dparam attempts to find an answer within tolerance tol.
	// Original MATLAB dparam copyright 1998-2001 by Toby Driscoll.
	private void dparam(double z[][], double c[], double qdat[][], 
			double w[][], double beta[], double z0[][], double tol) {
		int i, j, k;
		double cr[] = new double[1];
		double ci[] = new double[1];
	    int n = w.length; // number of vertices
	    
	    // Check input data
	    int err = sccheck("d", w, beta, null);
	    if (err == -1) {
	    	return;
	    }
	    if (err == 1) {
	    	MipavUtil.displayError("Use scfix to make polygon obey requirements");
	    	return;
	    }
	    
	    int nqpts = (int)Math.max(Math.ceil(-Math.log10(tol)),4);
	    scqdata(qdat, beta, nqpts);  // quadrature data
	    
	    boolean atinf[] = new boolean[beta.length];
	    for (i = 0; i < beta.length; i++) {
	    	if (beta[i] <= -1) {
	    		atinf[i] = true;
	    	}
	    } // for (i = 0; i < beta.length; i++)
	    
	    if (n == 3) {
	    	// Trivial solution
	    	z[0][0] = 0.0;
	    	z[0][1] = -1.0;
	    	double sqrt2 = Math.sqrt(2.0);
	    	z[1][0] = 1.0/sqrt2;
	    	z[1][1] = -1.0/sqrt2;
	    	z[2][0] = 1.0;
	    	z[2][1] = 0.0;
	    } // if (n == 3)
	    else {
	        // Set up normalized lengths for nonlinear equations
	    	
	    	// indices of left and right integration endpoints
	    	int numleft = 0;
	    	for (i = 0; i < n-2; i++) {
	    		if (!atinf[i]) {
	    			numleft++;
	    		}
	    	} // for (i = 0; i < n-2; i++)
	    	int left[] = new int[numleft];
	    	for (i = 0, j = 0; i < n-2; i++) {
	    		if (!atinf[i]) {
	    			left[j++] = i;
	    		}
	    	} // for (i = 0, j = 0; i < n-2; i++)
	    	int numright = 0;
	    	for (i = 1; i < n-1; i++) {
	    		if (!atinf[i]) {
	    			numright++;
	    		}
	    	} // for (i = 1; i < n-1; i++)
	    	int right[] = new int[numright];
	    	for (i = 1, j = 0; i < n-1; i++) {
	    	    if (!atinf[i]) {
	    	    	right[j++] = i;
	    	    }
	    	} // for (i = 1, j = 0; i < n-1; i++)
	    	// Assumes numright == numleft
	    	boolean cmplx[] = new boolean[numright];
	    	int numcmplx = 0;
	    	for (i = 0; i < numright; i++) {
	    		if ((right[i] - left[i]) == 2) {
	    			cmplx[i] = true;
	    			numcmplx++;
	    		}
	    	} // for (i = 0; i < numright; i++)
	    	// Normalize lengths by w[1] - w[0]
	    	double diffw[] = new double[2];
	    	diffw[0] = w[1][0] - w[0][0];
	    	diffw[1] = w[1][1] - w[0][1];
	    	double nmlen[][] = new double[numright][2];
	    	for (i = 0; i < numright; i++) {
	    	    zdiv(w[right[i]][0] - w[left[i]][0], w[right[i]][1] - w[left[i]][1], 
	    	    		diffw[0], diffw[1], cr, ci);
	    	    nmlen[i][0] = cr[0];
	    	    nmlen[i][1] = ci[0];
	    	} // for (i = 0; i < numright; i++)
	    	// abs value for finite ones; Re/Im or Infinite ones
	    	int numnotcmplx = numright-numcmplx;
	    	double nmlen2[] = new double[numnotcmplx + 2*numcmplx];
	    	for (i = 0, j = 0, k = 0; i < numright; i++) {
	    	    if (!cmplx[i]) {
	    	    	nmlen2[j++] = zabs(nmlen[i][0], nmlen[i][1]);
	    	    }
	    	    else {
	    	    	nmlen2[numnotcmplx + k] = nmlen[i][0];
	    	    	nmlen2[numnotcmplx + numcmplx + k] = nmlen[i][1];
	    	    	k++;
	    	    }
	    	} // for (i = 0, j = 0, k = 0; i < numright; i++)
	    	// First entry is useless (= 0)
	    	double nmlen3[] = new double[nmlen2.length-1];
	    	for (i = 0; i < nmlen3.length; i++) {
	    		nmlen3[i] = nmlen2[i+1];
	    	}
	    	
	    	// Set up initial guess
	    	double y0[] = null;
	    	if ((z0 == null) || (z0.length == 0)) {
	    		y0 = new double[n-3];
	    	}
	    	else {
	    		double z02[][] = new double[z0.length][2];
	    		for (i = 0; i < z0.length; i++) {
	    			zdiv(z0[i][0], z0[i][1], zabs(z0[i][0], z0[i][1]), 0.0, cr, ci);
	    			z02[i][0] = cr[0];
	    			z02[i][1] = ci[0];
	    		} // for (i = 0; i < z0.length; i++)
	    		// Moebius to make th(n-3:n-1) = [1,1.5,2]*PI
	    		// The MATLAB lines are:
	    		// Am = moebius(z0(n-2:n),[-1;-i;1]));
	    		// z0 = Am(z0);
	    		// The line Z0 = Am(z0); generates the error message:
	    		// Subscript indices must be either real positive integers or logicals.
	    	} // else
	    	// Solve nonlinear system of equations
		    dpfun fm = new dpfun(y0, n, beta, nmlen3, left, right, cmplx, qdat);
		    fm.driver();
			fm.dumpResults();
		    int exitStatus = fm.getExitStatus();
		    if (exitStatus < 0 ) {
		    	printExitStatus(exitStatus);
		    	return;
		    }
			double y[] = fm.getParameters();
			
			// Convert y values to z
			double cumprod[] = new double[y.length+1];
			cumprod[0] = 1.0;
			for (i = 1; i <= y.length; i++) {
				cumprod[i] = cumprod[i-1] * Math.exp(-y[i-1]);
			}
			double cs[] = new double[y.length+1];
			cs[0] = cumprod[0];
			for (i = 1; i <= y.length; i++) {
				cs[i] = cs[i-1] + cumprod[i];
			}
			double theta[] = new double[n-3];
			for (i = 0; i < n-3; i++) {
				theta[i] = Math.PI * cs[i]/cs[n-3];
			}
			for (i = 0; i < n-3; i++) {
				z[i][0] = Math.cos(theta[i]);
				z[i][1] = Math.sin(theta[i]);
			}
			z[n-3][0] = -1.0;
			z[n-3][1] = 0.0;
			z[n-2][0] = 0.0;
			z[n-2][1] = -1.0;
			z[n-1][0] = 1.0;
			z[n-1][1] = 0.0;
	    } // else
	    	    
	    // Determine scaling constant
	    double mid[][] = new double[1][2];
	    mid[0][0] = (z[0][0] + z[1][0])/2.0;
	    mid[0][1] = (z[0][1] + z[1][1])/2.0;
	    double z1[][] = new double[1][2];
	    z1[0][0] = z[1][0];
	    z1[0][1] = z[1][1];
	    int sing1[] = new int[1];
	    sing1[0] = 1;
	    double z02[][] = new double[1][2];
	    z02[0][0] = z[0][0];
	    z02[0][1] = z[0][1];
	    int sing0[] = new int[1];
	    sing0[0] = 0;
	    double I1[][] = dquad(z1, mid, sing1, z, beta, qdat);
	    double I2[][] = dquad(z02, mid, sing0, z, beta, qdat);
	    double denom[] = new double[2];
	    denom[0] = I1[0][0] - I2[0][0];
	    denom[1] = I1[0][1] - I2[0][1];
	    double num[] = new double[2];
	    num[0] = w[0][0] - w[1][0];
	    num[1] = w[0][1] - w[1][1];
	    zdiv(num[0], num[1], denom[0], denom[1], cr, ci);
	    c[0] = cr[0];
	    c[1] = ci[0];
	    return;
	}
	
	
	// Schwarz-Christoffel rectangle parameter problem
	// rparam solves the Schwarz-Christoffel parameter problem with a rectangle as a fundamental domain
	// and the interior of the specified polygon as the target.  
	// w must be a vector of the vertices of the polygon, specified in a counterclockwise order.
	// w is a complex number in the original MATLAB.
	// beta is a vector of the turning angles; see scangle
	// The interior angles of the polygon are alpha1*PI,..., alphan*PI.
	// (1 - alphak)*PI = betak*PI.
	// The angle betak*PI is the turning angle at vertex k.
	// cnr is a 4-component vector specifying the indices of the vertices which are which are the
	// images of the corners of the rectangle.  ** Be sure ** the first two entries describe the LONG
	// sides of the rectangle, and go in counterclockwise order.  If cnr is omitted, the user is 
	// requested to select these vertices using the mouse.
	// If successful, rparam will return z, a vector of prevertices;
	// c, the multiplicative constant of the conformal map;
	// L, a parameter related to aspect ratio;
	// and qdat, an optional matrix of quadrature data used by some of the other sc routines.
	// rparam uses z0 as an initial guess for z.  In this case, z0 represents the image of 
	// prevertices on the strip 0 <= Im z <= 1.  You can use r2strip to transform from the rectangle to 
	// the strip.
	// rparam attempts to find an answer within the tolerance tol.  (Also see scparopt.)
	// rparam uses a vector of control parameters.  See scparopt.
	// Original MATLAB code copyright 1998 by Toby Driscoll.
	// NESolve has been replaced the the MIAPV implementation of ELSUNC
	private void rparam(double z[][], double c[], double L[], double qdat[][], 
			double w[][], double beta[], int cnr[], double z0[][], double tol) {
		int i, j, k, m, p;
		double diffR;
		double diffI;
		double sum;
		int num;
		double mean;
		double sum1;
		double sum2;
		double len;
		double wid;
		double modest;
		int npoints;
		double spacing;
		double dx;
		int upper;
		double cr[] = new double[1];
		double ci[] = new double[1];
	    int n = w.length;  // number of vertices
	    
	    // cnr should be set by initial MIPAV dialog, so this should never happen.
	    // The polygon as specified by w and beta is drawn on the image and the
	    // user selects 4 vertices using the mouse.
	    if ((cnr == null) || (cnr.length == 0)) {
	    	String msg[] = new String[2];
	        msg[0] = "Select the images of the corners of the rectangle";
	        msg[1] = "Go in counerclockwise order and select a long rectangle edge first";
	        cnr = scselect(w, beta, 4, "Select corners", msg);
	    } // if ((cnr == null) || (cnr.length == 0))
	
	    // Renumber the vertices so that cnr[0] = 0.
	    int renum[] = new int[n];
	    for (i = cnr[0]; i < n; i++) {
	    	renum[i-cnr[0]] = i;
	    }
	    for (i = 0; i <= cnr[0]-1; i++) {
	    	renum[n-cnr[0]+i] = i;
	    }
	    double wcopy[][] = new double[n][2];
	    double betacopy[] = new double[n];
	    for (i = cnr[0]; i < n; i++) {
	        wcopy[i-cnr[0]][0] = w[i][0];
	        wcopy[i-cnr[0]][1] = w[i][1];
	        betacopy[i-cnr[0]] = beta[i];
	    }
	    for (i = 0; i <= cnr[0]-1; i++) {
	    	wcopy[i+n-cnr[0]][0] = w[i][0];
	    	wcopy[i+n-cnr[0]][1] = w[i][1];
	    	betacopy[i+n-cnr[0]] = beta[i];
	    }
	    
	    int offset = n - cnr[0]; // cnr - cnr[0] so no need to add one
	    int cnrcopy[] = new int[cnr.length];
	    for (i = 0; i < cnr.length; i++) {
	    	cnrcopy[i] = ((cnr[i] + offset) % n);
	    }
	    double z0copy[][] = null;
	    if (z0 != null) {
		    z0copy = new double[z0.length][2];
		    for (i = 0; i < z0.length; i++) {
		    	z0copy[i][0] = z0[i][0];
		    	z0copy[i][1] = z0[i][1];
		    }
		    
		    if (z0copy.length == 1) {
		    	tol = z0copy[0][0];
		    	z0copy = null;
		    }
	    } // if (z0 != null)
	    int nqpts = Math.max((int)Math.ceil(-Math.log10(tol)), 4);
	    scqdata(qdat, betacopy, nqpts);  // quadrature data
	    
	    // Check input data
	    int err = sccheck("r", wcopy, betacopy, cnrcopy);
	    if (err == -1) {
	    	return;
	    }
	    if (err == 1) {
	    	MipavUtil.displayError("Use scfix to make polygon obey requirements");
	    	return;
	    }
	    
	    boolean atinf[] = new boolean[n];
	    for (i = 0; i < n; i++) {
	    	atinf[i] = (betacopy[i] <= -1);
	    }
	    
	    if (z0copy == null) {
	        // Try to find a reasonable initial guess
	    	// side lengths
	    	double dw[]= new double[n];
	    	for (i = 0; i < n-1; i++) {
	    		diffR = wcopy[i+1][0] - wcopy[i][0];
	    		diffI = wcopy[i+1][1] - wcopy[i][1];
	    		dw[i] = zabs(diffR,diffI);
	    	} // for (i = 0; i < n-1; i++)
	    	diffR = wcopy[0][0] - wcopy[n-1][0];
	    	diffI = wcopy[0][1] - wcopy[n-1][1];
	    	dw[n-1] = zabs(diffR,diffI);
	        sum = 0.0;
	        num = 0;
	        for (i = 0; i < n; i++) {
	        	if ((!Double.isInfinite(dw[i])) && (!Double.isNaN(dw[i]))) {
	        		sum += dw[i];
	        		num++;
	        	}
	        } // for (i = 0; i < n; i++)
	        mean = sum/num;
	        for (i = 0; i < n; i++) {
	        	if (Double.isInfinite(dw[i]) || Double.isNaN(dw[i])) {
	        		dw[i] = mean;
	        	}
	        } // for (i = 0; i < n; i++)
	        // Estimate length and width (conformal modulus)
	        sum1 = 0.0;
	        sum2 = 0.0;
	        for (i = cnrcopy[0]; i <= cnrcopy[1]-1; i++) {
	        	sum1 += dw[i];
	        }
	        for (i = cnrcopy[2]; i <= cnrcopy[3]-1; i++) {
	        	sum2 += dw[i];
	        }
	        len = (sum1 + sum2)/2.0;
	        sum1 = 0.0;
	        sum2 = 0.0;
	        for (i = cnrcopy[1]; i <= cnrcopy[2]-1; i++) {
	        	sum1 += dw[i];
	        }
	        for (i = cnrcopy[3]; i <= n-1; i++) {
	        	sum2 += dw[i];
	        }
	        for (i = 0; i <= cnrcopy[0]-1; i++) {
	        	sum2 += dw[i];
	        }
	        wid = (sum1 + sum2)/2.0;
	        modest = Math.min(len/wid, 100);
	        // Evenly space prevertices to match this conformal modulus.
	        // z0[0] to z0[cnr[2]-1] has an imaginary part = 0 with increasing positive numbers
	        // z0[cnr[2]] to z0[n-1] has an imaginary part = 1 with decreasing positive numbers.
	        // Only dz[cnr[2]-1] has an imaginary part and this is not used in forming y0
	        // Initially dz fron 0 to cnrcopy[2] -1 is positive and dz from concopy[2] to n-2 is negative
	        // However, dz from cnrcopy[2] to n-1 is multiplied by negative one to become positive
	        // Therefore, all the dz used in forming y0 are positive real numbers.
	        // y0 is all real
	        npoints = cnrcopy[1] - cnrcopy[0] +1;
	        spacing = modest/(npoints - 1.0);
	        z0copy = new double[n][2];
	        for (i = 0; i < npoints-1; i++) {
	            z0copy[cnrcopy[0]+i][0] = i*spacing;	
	        }
	        z0copy[cnrcopy[1]][0] = modest;
	        dx = z0copy[cnrcopy[0]+1][0] - z0copy[cnrcopy[0]][0];
	        for (i = 1; i <= cnrcopy[0]-1; i++) {
	        	z0copy[cnrcopy[0]-i][0] = z0copy[cnrcopy[0]][0] - dx * i;
	        }
	        upper = cnrcopy[2] - cnrcopy[1] - 1;
	        for (i = 1; i <= upper; i++) {
	        	z0copy[cnrcopy[1]+i][0] = z0copy[cnrcopy[1]][0] + dx * i;
	        }
	        npoints = cnrcopy[3] - cnrcopy[2] + 1;
	        spacing = modest/(npoints - 1.0);
	        for (i = 0; i < npoints-1; i++) {
	        	z0copy[cnrcopy[3]-i][0] = i*spacing;
	        	z0copy[cnrcopy[3]-i][1] = 1;
	        }
	        z0copy[cnrcopy[2]][0] = modest; 
	        z0copy[cnrcopy[2]][1] = 1;
	        dx = z0copy[cnrcopy[3]-1][0] - z0copy[cnrcopy[3]][0];
	        for (i = 1; i <= n-1-cnrcopy[3]; i++) {
	        	z0copy[cnrcopy[3]+i][0] = z0copy[cnrcopy[3]][0] - dx*i;
	        	z0copy[cnrcopy[3]+i][1] = 1;
	        }
	    } // if (z0copy == null)
	    else {
	    	if (z0copy.length != n) {
	    		MipavUtil.displayError("Initial guess has wrong number of prevertices");
	    		return;
	    	}
	        z0copy = new double[n][2];
	    	for (i = cnrcopy[0]; i < n; i++) {
		        z0copy[i-cnrcopy[0]][0] = z0[i][0];
		        z0copy[i-cnrcopy[0]][1] = z0[i][1];
		    }
		    for (i = 0; i <= cnrcopy[0]-1; i++) {
		    	z0copy[i+n-cnrcopy[0]][0] = z0[i][0];
		    	z0copy[i+n-cnrcopy[0]][1] = z0[i][1];
		    }
		    for (i = 0; i < n; i++) {
		    	z0copy[i][1] = Math.round(z0copy[i][1]);
		    }
		    
            for (i = cnrcopy[0]; i <= cnrcopy[1]; i++) {
            	if (z0copy[i][1] == 0) {
            		MipavUtil.displayError("Initial guess has prevertices on wrong side of strip");
            		return;
            	}
            } // for (i = cnrcopy[0]; i <= cnrcopy[1]; i++)
            for (i = cnrcopy[2]; i <= cnrcopy[3]; i++) {
            	if (z0copy[i][1] == 0) {
            		MipavUtil.displayError("Initial guess has prevertices on wrong side of strip");
            		return;
            	}
            } // for (i = cnrcopy[2]; i <= cnrcopy[3]; i++)
	    } // else
	    
	    // Convert z0 to unconstrained vars
	    double y0[] = new double[n-3];
	    double dz[][] = new double[n-1][2];
	    for (i = 0; i < n-1; i++) {
	    	dz[i][0] = z0copy[i+1][0] - z0copy[i][0];
	    	dz[i][1] = z0copy[i+1][1] - z0copy[i][1];
	    }
	    for (i = cnrcopy[2]; i < n-1; i++) {
	    	dz[i][0] = -dz[i][0];
	    }
	    for (i = 0; i <= cnrcopy[1]-2; i++) {
	    	y0[i] = Math.log(dz[i][0]);
	    }
	    for (i = cnrcopy[2]-1; i <= cnrcopy[3]-3; i++) {
	    	y0[i] = Math.log(dz[i+2][0]);	
	    }
	    y0[cnrcopy[1]-1] = (Math.log(dz[cnrcopy[1]-1][0]) + Math.log(dz[cnrcopy[2]][0]))/2.0;
	    
	    
	    // Vertices on the "short" edges are transformed into the interval [-1,1],
	    // and then the Trefethen-style transformation is used.
	    L[0] = z0copy[cnrcopy[1]][0] - z0copy[cnrcopy[0]][0];
	    L[1] = z0copy[cnrcopy[1]][1] - z0copy[cnrcopy[0]][1];
	    double x[] = new double[cnrcopy[2]-cnrcopy[1]-1];
	    for (i = cnrcopy[1]+1; i <= cnrcopy[2]-1; i++) {
	    	x[i-cnrcopy[1]-1] = Math.exp(Math.PI*(L[0] - z0copy[i][0]))*Math.cos(Math.PI*(L[1] + z0copy[i][1]));
	    }
	    double dx2[] = new double[x.length+1];
	    if (x.length >= 1) {
		    dx2[0] = 1 - x[0];
		    for (i = 0; i < x.length-1; i++) {
		    	dx2[i+1] = x[i] - x[i+1];
		    }
		    dx2[x.length] = x[x.length-1] + 1;
	    }
	    else {
	    	dx2[0] = 2.0;
	    }
	    for (i = 0; i <= dx2.length-2; i++) {
	        y0[i+cnrcopy[1]] = Math.log(dx2[i]/dx2[i+1]);
	    }
	    x = new double[n-1-cnrcopy[3]];
	    for (i = 0; i < n-1-cnrcopy[3]; i++) {
	    	x[i] = Math.exp(Math.PI*z0copy[cnrcopy[3]+1+i][0])*Math.cos(Math.PI*z0copy[cnrcopy[3]+1+i][1]);
	    }
	    dx2 = new double[x.length+1];
	    if (x.length >= 1) {
		    dx2[0] = x[0]+1;
		    for (i = 0; i < x.length-1; i++) {
		    	dx2[i+1] = x[i+1] - x[i];
		    }
		    dx2[x.length] = 1 - x[x.length-1];
	    }
	    else {
	    	dx2[0] = 2.0;
	    }
	    for (i = 0; i <= dx2.length-2; i++) {
	    	y0[cnrcopy[3]-2+i] = Math.log(dx2[i]/dx2[i+1]);
	    }
	    
	    // Find prevertices (solve param problem)
	    
	    // Set up normalized lengths for nonlinear equations:
	    // indices of left and right integration endpoints
	    // Delete indices corresponding to vertices at infinity
	    int numleft = 0;
	    int numright = 0;
	    for (i = 0; i < n-2; i++) {
	    	if (!atinf[i]) {
	    		numleft++;
	    	}
	    	if (!atinf[i+1]) {
	    		numright++;
	    	}
	    }
	    if (atinf[n-2]) {
	    	numright++;
	    }
	    int left[] = new int[numleft];
	    int right[] = new int[numright];
	    int jl = 0;
	    int jr = 0;
	    for (i = 0; i < n-2; i++) {
	    	if (!atinf[i]) {
	    		left[jl++] = i;
	    	}
		    if (!atinf[i+1]) {
		    	right[jr++] = i+1;
		    }
	    }
	    if (atinf[n-2]) {
	    	right[jr++] = n-1;
	    }
	    if (left.length != right.length) {
	    	MipavUtil.displayError("left.length != right.length");
	    	return;
	    }
	    boolean cmplx[] = new boolean[left.length];
	    int numcmplx = 0;
	    for (i = 0; i < left.length; i++) {
	    	if ((right[i] - left[i]) == 2) {
	    		cmplx[i] = true;
	    		numcmplx++;
	    	}
	    }
	    // It's possible we replaced the last single condition by a complex one
	    if ((cmplx.length + numcmplx) > n-2) {
	    	cmplx[cmplx.length-1] = false;
	    }
	    // Normalize lengths by w[1] - w[0]
	    double wdenomreal = wcopy[1][0] - wcopy[0][0];
	    double wdenomimag = wcopy[1][1] - wcopy[0][1];
	    
	    // First entry is useless = 1.
	    double nmlen[][] = new double[left.length-1][2];
	    for (i = 1; i < left.length; i++) {
	    	double realpart = wcopy[right[i]][0] - wcopy[left[i]][0];
	    	double imagpart = wcopy[right[i]][1] - wcopy[left[i]][1];
	    	zdiv(realpart, imagpart, wdenomreal, wdenomimag, cr, ci);
	    	nmlen[i-1][0] = cr[0];
	    	nmlen[i-1][1] = ci[0];
	    	// Absolute value for finite ones
	    	if (!cmplx[i]) {
	    		nmlen[i-1][0] = zabs(nmlen[i-1][0],nmlen[i-1][1]);
	    		nmlen[i-1][1] = 0;
	    	}
	    } // for (i = 1; i < left.length; i++)
	    
	    // Solve nonlinear system of equations
	    rpfun fm = new rpfun(y0, n, betacopy, nmlen, left, right, cmplx, qdat, cnrcopy);
	    fm.driver();
		fm.dumpResults();
	    int exitStatus = fm.getExitStatus();
	    if (exitStatus < 0 ) {
	    	printExitStatus(exitStatus);
	    	return;
	    }
		double y[] = fm.getParameters();
		
		// Convert y values to z on a strip
		rptrnsfm(z, y,cnrcopy);
		int ends[] = new int[2];
		for (i = 0, j = 0; i < n-1; i++) {
			if (z[i][1] != z[i+1][1]) {
				ends[j++] = i;
			}
		}
		if (z[n-1][1] != z[0][1]) {
			ends[j] = n-1;
		}
		double zs[][] = new double[n+2][2];
		for (i = 0; i <= ends[0]; i++) {
			zs[i][0] = z[i][0];
			zs[i][1] = z[i][1];
		}
		zs[ends[0]+1][0] = Double.POSITIVE_INFINITY;
		zs[ends[0]+1][1] = 0.0;
		for (i = ends[0]+1; i <= ends[1]; i++) {
			zs[i+1][0] = z[i][0];
			zs[i+1][1] = z[i][1];
		}
		zs[ends[1]+2][0] = Double.NEGATIVE_INFINITY;
		zs[ends[1]+2][1] = 0;
		for (i = ends[1]+1; i < n; i++) {
			zs[i+2][0] = z[i][0];
			zs[i+2][1] = z[i][1];
		}
	    double bs[] = new double[n+2];
	    for (i = 0; i <= ends[0]; i++) {
	    	bs[i] = betacopy[i];
	    }
	    bs[ends[0]+1] = 0;
	    for (i = ends[0]+1; i <= ends[1]; i++) {
	    	bs[i+1] = betacopy[i];
	    }
	    bs[ends[1]+2] = 0;
	    for (i = ends[1]+1; i < n; i++) {
	    	bs[i+2] = betacopy[i];
	    }
	    double qs[][] = new double[qdat.length][qdat[0].length+4];
		for (i = 0; i < qdat.length; i++) {
			for (j = 0; j <= ends[0]; j++) {
				qs[i][j] = qdat[i][j];
				qs[i][j+2+n+1] = qdat[i][j+n+1];
			}
			qs[i][ends[0]+1] = qdat[i][n];
			qs[i][ends[0]+3+n+1] = qdat[i][2*n+1];
			for (j = ends[0]+1; j <= ends[1]; j++) {
				qs[i][j+1] = qdat[i][j];
				qs[i][j+3+n+1] = qdat[i][j+n+1];
			}
			qs[i][ends[1]+2] = qdat[i][n];
			qs[i][ends[1]+4+n+1] = qdat[i][2*n+1];
			for (j = ends[1]+1; j <= n-1; j++) {
				qs[i][j+2] = qdat[i][j];
				qs[i][j+4+n+1] = qdat[i][j+n+1];
			}
			qs[i][n+2] = qdat[i][n];
			qs[i][2*n+5] = qdat[i][2*n+1];
		} // for (i = 0; i < qdat.length; i++)
	    
		// Determine multiplicative constant
		double mid[][] = new double[1][2];
		mid[0][0] = (zs[0][0] + zs[1][0])/2.0;
		mid[0][1] = (zs[0][1] + zs[1][1])/2.0;
		int sing1[] = new int[1];
		sing1[0] = 1;
		double z1[][] = new double[1][2];
		z1[0][0] = zs[1][0];
		z1[0][1] = zs[1][1];
		double I1[][] = stquad(z1, mid, sing1, zs, bs, qs);
		sing1[0] = 0;
		z1[0][0] = zs[0][0];
		z1[0][1] = zs[0][1];
		double I2[][] = stquad(z1, mid, sing1, zs, bs, qs);
		double g[] = new double[2];
		g[0] = I1[0][0] - I2[0][0];
		g[1] = I1[0][1] - I2[0][1];
		double ar = w[0][0] - w[1][0];
		double ai = w[0][1] - w[1][1];
		zdiv(ar, ai, g[0], g[1], cr, ci);
		c[0] = cr[0];
		c[1] = ci[0];
		
		// Find prevertices on the rectangle
		
		// Find corners of the rectangle
		zs = new double[z.length][2];
		for (i = 0; i < z.length; i++) {
			zs[i][0] = z[i][0];
			zs[i][1] = z[i][1];
		}
		// In rptrnsfm z[cnrcopy[0]] is never changed from its initial value of zero
		// so zs[cnrcopy[0]][0] = 0 and zs[cnrcopy[0]][1] = 0;
		// so L is real
		L[0] = Math.max(zs[cnrcopy[1]][0], zs[cnrcopy[2]][0]) - zs[cnrcopy[0]][0];
		L[1] = -zs[cnrcopy[0]][1];
		double K[] = new double[1];
		double Kp[] = new double[1];
		ellipkkp(K, Kp, L[0]);
		double rect[][] = new double[4][2];
		rect[0][0] = K[0];
		rect[1][0] = K[0];
		rect[1][1] = Kp[0];
		rect[2][0] = -K[0];
		rect[2][1] = Kp[0];
		rect[3][0] = -K[0];
		boolean l[] = new boolean[n]; // on left side
		for (i = cnrcopy[2]; i <= cnrcopy[3]; i++) {
			l[i] = true;
		}
		boolean r[] = new boolean[n]; // on right side
		for (i = cnrcopy[0]; i <= cnrcopy[1]; i++) {
			r[i] = true;
 		}
		int tllength = 0;
		int trlength = 0;
		int bllength = 0;
		int brlength = 0;
		for (i = 0; i < zs.length; i++) {
			if ((zs[i][0] > L[0]) && (zs[i][1] > 0.0)) {
				// top-left side
				tllength++;
			}
			else if ((zs[i][0] > L[0]) && (zs[i][1] == 0.0)) {
				// top-right side
				trlength++;
			}
			else if ((zs[i][0] < 0.0) && (zs[i][1] > 0.0)) {
				// bottom-left side
				bllength++;
			}
			else if ((zs[i][0] < 0.0) && (zs[i][1] == 0.0)) {
				// bottom-right side
				brlength++;
			}
		} // for (i = 0; i < zs.length; i++)
		int tl[] = new int[tllength];
		int tr[] = new int[trlength];
		int bl[] = new int[bllength];
		int br[] = new int[brlength];
		for (i = 0, j = 0, k = 0, m = 0, p = 0; i < zs.length; i++) {
			if ((zs[i][0] > L[0]) && (zs[i][1] > 0.0)) {
				// top-left side
				tl[j++] = i;
			}
			else if ((zs[i][0] > L[0]) && (zs[i][1] == 0.0)) {
				// top-right side
				tr[k++] = i;
			}
			else if ((zs[i][0] < 0.0) && (zs[i][1] > 0.0)) {
				// bottom-left side
				bl[m++] = i;
			}
			else if ((zs[i][0] < 0.0) && (zs[i][1] == 0.0)) {
				// bottom-right side
				br[p++] = i;
			}
		} // for (i = 0, j = 0, k = 0, m = 0, p = 0; i < zs.length; i++)
		
		// Initial guesses
		//z = new double[zs.length][2];
		for (i = 0; i < z.length; i++) {
			z[i][0] = 0;
			z[i][1] = 0;
		}
		// Corners
		for (i = 0; i < 4; i++) {
		    z[cnrcopy[i]][0] = rect[i][0];
		    z[cnrcopy[i]][1] = rect[i][1];
		}
		// Left and right sides are simple
		int denom = cnrcopy[3] - cnrcopy[2];
		double realDiff = (rect[3][0] - rect[2][0])/denom;
		double imagDiff = (rect[3][1] - rect[2][1])/denom;
		for (i = cnrcopy[2]; i <= cnrcopy[3]; i++) {
			z[i][0] = rect[2][0] + (i - cnrcopy[2]) * realDiff;
			z[i][1] = rect[2][1] + (i - cnrcopy[2]) * imagDiff;
		}
		denom = cnrcopy[1] - cnrcopy[0];
		realDiff = (rect[1][0] - rect[0][0])/denom;
		imagDiff = (rect[1][1] - rect[0][1])/denom;
		for (i = cnrcopy[0]; i <= cnrcopy[1]; i++) {
			z[i][0] = rect[0][0] + (i - cnrcopy[0]) * realDiff;
			z[i][1] = rect[0][1] + (i - cnrcopy[0]) * imagDiff;
		}
		// Cluster the top and bottom guesses near 0.
		double h = K[0]/20.0;
		for (i = 0; i < tl.length; i++) {
			z[tl[i]][0] = -(i+1)*h;
			z[tl[i]][1] = Kp[0];
		}
		for (i = 0; i < tr.length; i++) {
			z[tr[i]][0] = h*(tr.length - i);
			z[tr[i]][1] = Kp[0];
		}
		for (i = 0; i < bl.length; i++) {
			z[bl[i]][0] = -h * (bl.length - i);
			z[bl[i]][1] = 0.0;
		}
		for (i = 0; i < br.length; i++) {
			z[br[i]][0] = (i+1)*h;
			z[br[i]][1] = 0.0;
		}
		
		// Newton iteration on "r2strip() - zs = 0"
		double zn[][] = new double[z.length][2];
		for (i = 0; i < z.length; i++) {
			zn[i][0] = z[i][0];
			zn[i][1] = z[i][1];
		}
		int maxiter = 50;
		boolean done[] = new boolean[zn.length];
		for (i = 0; i < 4; i++) {;
			done[cnrcopy[i]] = true;
		}
		k = 0;
		boolean alldone = true;
		int numnotdone = 0;
		for (i = 0; i < done.length; i++) {
			if (!done[i]) {
				alldone = false;
				numnotdone++;
			}
		} // for (i = 0; i < done.length; i++)
		double znnotdone[][] = new double[numnotdone][2];
		double zsnotdone[][] = new double[numnotdone][2];
		boolean lr[] = new boolean[numnotdone];
		boolean tbl[] = new boolean[numnotdone];
		boolean tlnotdone;
		boolean blnotdone;
		boolean tbr[] = new boolean[numnotdone];
		boolean trnotdone;
		boolean brnotdone;
		for (i = 0, j = 0; i < done.length; i++) {
		    if (!done[i]) {
		    	znnotdone[j][0] = zn[i][0];
		    	znnotdone[j][1] = zn[i][1];
		    	zsnotdone[j][0] = zs[i][0];
		    	zsnotdone[j][1] = zs[i][1];
		    	lr[j] = r[i] || l[i];
		    	tlnotdone = (zs[i][0] > L[0]) && (zs[i][1] > 0);
		    	blnotdone = (zs[i][0] < 0) && (zs[i][1] > 0);
		    	tbl[j] = tlnotdone || blnotdone;
		    	trnotdone = (zs[i][0] > L[0]) && (zs[i][1] == 0);
		    	brnotdone = (zs[i][0] < 0) && (zs[i][1] == 0);
		    	tbr[j++] = trnotdone || brnotdone;
		    } 
		} // for (i = 0, j = 0; i < done.length; i++)
		double zcnr[][] = new double[4][2];
		for (i = 0; i < 4; i++) {
			zcnr[i][0] = z[cnrcopy[i]][0];
			zcnr[i][1] = z[cnrcopy[i]][1];
		}
		double F[][] = new double[numnotdone][2];
		double Fabs;
		double dF[][] = new double[numnotdone][2];
		double step[][] = new double[numnotdone][2];
		double znew[][] = new double[numnotdone][2];
		double xn;
		while ((!alldone) && (k < maxiter)) {
		    r2strip(F, dF, znnotdone, zcnr, L[0]);
		    for (i = 0; i < numnotdone; i++) {
		    	F[i][0] = zsnotdone[i][0] - F[i][0];
		    	F[i][1] = zsnotdone[i][1] - F[i][1];
		    }
		    	
		    	// Adjust Newton step to stay exactly on rectangle boundary
		    for (i = 0; i < numnotdone; i++) {
		    	zdiv(F[i][0], F[i][1], dF[i][0], dF[i][1], cr, ci);
		    	step[i][0] = cr[0];
		    	step[i][1] = ci[0];
		    }
		    
		    for (i = 0; i < numnotdone; i++) {
		    	if (lr[i]) {
		    	    step[i][0] = 0;	
		    	}
		    	else {
		    		step[i][1] = 0;
		    	}
		    }
		    
		    	
		    	// Newton step
		    for (i = 0; i < numnotdone; i++) {
		    	znew[i][0] = znnotdone[i][0] + step[i][0];
		    	znew[i][1] = znnotdone[i][1] + step[i][1];
		    }
		    
		    	// Keep prevertices from moving too far (past boundaries)
		    	// Left/right sides capped in Im direction
		    for (i = 0; i < numnotdone; i++) {
		        if (lr[i]) {
		        	xn = Math.min(Math.max(znew[i][1], 0), Kp[0]);
		        	znew[i][1] = xn;
		        } // if (lr[i])
		    }
		        // Top/bottom-left sides capped in Re direction
		    for (i = 0; i < numnotdone; i++) {
		        if (tbl[i]) {
		        	xn = Math.min(Math.max(znew[i][0], -K[0]), -eps);
		        	znew[i][0] = xn;
		        }
		    }
		    
		        // Top/bottom-right sides capped in Re direction
		    for (i = 0; i < numnotdone; i++) {
		        if (tbr[i]) {
		            xn = Math.min(Math.max(znew[i][0], eps), K[0]);
		            znew[i][0] = xn;
		        } 
		    }
		        
		        //  Update
		    for (i = 0, j = 0; i < done.length; i++) {
		    	if (!done[i]) {
		            znnotdone[j][0] = znew[j][0];
		            znnotdone[j][1] = znew[j][1];
		            zn[i][0] = znew[j][0];
		            zn[i][1] = znew[j][1];
		    	}
		    } // for (i = 0, j = 0; i < done.length; i++)
		    
		    
		    alldone = true;
		    double znnotdone2[][] = new double[numnotdone][2];
		    numnotdone = 0;
		    for (i = 0, j = 0, m = 0; i < done.length; i++) {
		    	if (!done[i]) {
		    		Fabs = zabs(F[j][0], F[j][1]);
		    		if (Fabs < tol) {
		    			done[i] = true;
		    		}
		    		else {
		    			numnotdone++;
		    			alldone = false;
		    			znnotdone2[m][0] = znnotdone[j][0];
		    			znnotdone2[m++][1] = znnotdone[j][1];
		    		}
		    		j++;
		    	}
		    } // for (i = 0, j = 0, m = 0; i < done.length; i++)
		    if (!alldone) {
			    znnotdone = new double[numnotdone][2];
				zsnotdone = new double[numnotdone][2];
				lr = new boolean[numnotdone];
				tbl = new boolean[numnotdone];
				tbr = new boolean[numnotdone];
				for (i = 0, j = 0; i < done.length; i++) {
				    if (!done[i]) {
				    	znnotdone[j][0] = znnotdone2[j][0];
				    	znnotdone[j][1] = znnotdone2[j][1];
				    	zsnotdone[j][0] = zs[i][0];
				    	zsnotdone[j][1] = zs[i][1];
				    	lr[j] = r[i] || l[i];
				    	tlnotdone = (zs[i][0] > L[0]) && (zs[i][1] > 0);
				    	blnotdone = (zs[i][0] < 0) && (zs[i][1] > 0);
				    	tbl[j] = tlnotdone || blnotdone;
				    	trnotdone = (zs[i][0] > L[0]) && (zs[i][1] == 0);
				    	brnotdone = (zs[i][0] < 0) && (zs[i][1] == 0);
				    	tbr[j++] = trnotdone || brnotdone;
				    } 
				} // for (i = 0, j = 0; i < done.length; i++)
				F = new double[numnotdone][2];
				dF = new double[numnotdone][2];
		    } // if (!alldone)
		    k++;
		} // while ((!alldone) && (k < maxiter))
		
		boolean warning = false;
		for (i = 0; i < F.length && !warning; i++) {
			if (zabs(F[i][0], F[i][1]) > tol) {
				warning = true;
				MipavUtil.displayWarning("Could not converge to the rectangle prevertices");
			}
		} // for (i = 0; i < F.length && !warning; i++)
		
		for (i = 0; i < z.length; i++) {
			z[i][0] = zn[i][0];
			z[i][1] = zn[i][1];
		}
		
		// Undo renumbering
		double temp[][] = new double[z.length][2];
		for (i = 0; i < z.length; i++) {
			temp[renum[i]][0] = z[i][0];
			temp[renum[i]][1] = z[i][1];
		}
		for (i = 0; i < z.length; i++) {
			z[i][0] = temp[i][0];
			z[i][1] = temp[i][1];
		}
		
		return;
    }
	
	// Select one or more vertices in a polygon
	// The polygon given by w and beta is in the source image window.
	// The user selects m vertices using the mouse. If m <= 0, m defaults to 1.
	// On exit K is a vector of indices into W.
	// scselect provides an instructional message
	private int[] scselect(double w[][], double beta[], int m,  String titl, String msg[]) {
		int i, j;
		int nearestIndex;
		double nearestDistance;
		double xDiff;
		double yDiff;
		double nearestXDiff;
		double nearestYDiff;
		double distance;
		if (m <= 0) {
			m = 1;
		}
		int K[] = new int[m];
		int n = w.length;
		for (i = 0; i < n-1; i++) {
		    if ((Double.isInfinite(w[i][0]) || Double.isInfinite(w[i][1])) &&
		    	(Double.isInfinite(w[i+1][0]) || Double.isInfinite(w[i+1][1]))) {
		    	MipavUtil.displayError("Infinite vertices must not be adjacent");
		    	System.exit(-1);
		    }
		}
		if ((Double.isInfinite(w[n-1][0]) || Double.isInfinite(w[n-1][1])) &&
			(Double.isInfinite(w[0][0]) || Double.isInfinite(w[0][1]))) {
			MipavUtil.displayError("Infinite vertices must not be adjacent");
	    	System.exit(-1);	
		}

        if (n < m) {
            MipavUtil.displayError("Number of vertices = " + n + " less than required " + m);

            System.exit(-1);
        }
		
		if (titl != null) {
		    System.out.println(titl);
		}
		if ((msg != null) && (msg.length > 0)) {
			for (i = 0; i < msg.length; i++) {
				System.out.println(msg[i]);
			}
		}
		
		// Begin selection(s)
		accessLock.lock();
		boolean addedPoint[] = new boolean[n];
		for (i = 0; i < m;) {
			try {
			    canProcessMouseClick.await();
			}
			catch (InterruptedException e) {
				e.printStackTrace();
			}
			nearestIndex = -1;
		    nearestDistance = Double.MAX_VALUE;
		    nearestXDiff = Double.MAX_VALUE;
		    nearestYDiff = Double.MAX_VALUE;
			for (j = 0; j < n; j++) {
			    xDiff = xClick - w[i][0];
			    yDiff = yClick - w[i][1];
			    distance = Math.sqrt(xDiff*xDiff + yDiff*yDiff);
			    if (distance < nearestDistance) {
			    	nearestIndex = j;
			    	nearestDistance = distance;
			    	nearestXDiff = Math.abs(xDiff);
			    	nearestYDiff = Math.abs(yDiff);
			    }
			} // for (j = 0; j < n; j++)
			if (nearestXDiff >= 3) {
				System.out.println("Cannot add vertex " + j + " when xDiff = " + nearestXDiff + " is >= 3");
				continue;
			}
			if (nearestYDiff >= 3) {
				System.out.println("Cannot add vertex " + j + " when yDiff = " + nearestYDiff + " is >= 3");
				continue;
			}
			if (addedPoint[j]) {
				System.out.println("Cannot add vertex " + j + " twice");
				continue;
			}
			else {
			    K[i] = nearestIndex;
			    addedPoint[nearestIndex] = true;
			    i++;
			}
		} // for (i = 0; i < m; i++)
		accessLock.unlock();
		return K;
	}
	
	
	
	public void mouseClicked(MouseEvent mouseEvent) {
		 int xS, yS;
	     ViewJComponentBase vBase= (ViewJComponentBase)srcImage.getParentFrame().getComponentImage();
		try {

			xS = Math.round((mouseEvent.getX() / (vBase.getZoomX() * vBase.getResolutionX())) - 0.5f);
            yS = Math.round((mouseEvent.getY() / (vBase.getZoomY() * vBase.getResolutionY())) - 0.5f);
            xClick = mouseEvent.getX();
            yClick = mouseEvent.getY();

            if ((xS < 0) || (xS >= srcImage.getExtents()[0]) || (yS < 0) || (yS >= srcImage.getExtents()[1])) {
                return;
            }

           
        } catch (OutOfMemoryError error) {
            System.gc();
            MipavUtil.displayError("Out of memory: SchwarzChristoffelMapping.mouseClicked+"
            		+ "");

            return;
        }
		
	    canProcessMouseClick.signalAll();
	}
	
	public void mousePressed(MouseEvent event) {
		
	}
	
	public void mouseReleased(MouseEvent event) {
		
	}
	
	public void mouseEntered(MouseEvent event) {
		
	}
	
	public void mouseExited(MouseEvent event) {
		
	}
	
	// Map from rectangle to strip.
	// r2strip maps from a rectangle to the strip 0 <= Im z <= 1, with the function log(sn(z|m))/pi,
	// where sn is a Jacobi elliptic function and m = exp(-2*PI*L).  The prevertices of the map 
	// (in the rectangle domain) are given by z; only the corners of the rectangle defined by z
	// are used.
	
	// The derivative of the map is returned as yprime.
	
	// Note: The functionality is not parallel to hp2disk and disk2hp.
	
	// Original MATLAB routine copyright 1998 by Toby Driscoll.
	
	// The built-in ellipj accepts only real arguments.  The standard identity is not helpful
	// when m is near zero, because (1-m) loses digits of accuracy.
	private void r2strip(double yp[][], double yprime[][], double zp[][], double z[][], double L) {
	    int i;
	    double cr[] = new double[1];
	    double ci[] = new double[1];
		double K = z[0][0];
		//double Kp = z[0][1];
		for (i = 1; i < z.length; i++) {
			if (z[i][0] > K) {
				K = z[i][0];
			}
			//if (z[i][1] > Kp) {
				//Kp = z[i][1];
			//}
		} // for (i = 1; i < z.length; i++) 
		for (i = 0; i < zp.length; i++) {
			yp[i][0] = zp[i][0];
			yp[i][1] = zp[i][1];
			yprime[i][0] = zp[i][0];
			yprime[i][1] = zp[i][1];
		}
		
		double sn[][] = new double[zp.length][2];
		double cn[][] = new double[zp.length][2];
		double dn[][] = new double[zp.length][2];
		ellipjc(sn, cn, dn, zp, L, false);
		
		// Make sure everything is in the upper half-plane (fix roundoff)
		for (i = 0; i < sn.length; i++) {
			sn[i][1] = Math.max(0, sn[i][1]);
			yp[i][0] = Math.log(zabs(sn[i][0], sn[i][1]))/Math.PI;
			yp[i][1] = Math.atan2(sn[i][1], sn[i][0])/Math.PI;
			zmlt(cn[i][0], cn[i][1], dn[i][0], dn[i][1], cr, ci);
			zdiv(cr[0], ci[0], sn[i][0], sn[i][1], cr, ci);
			yprime[i][0] = cr[0]/Math.PI;
			yprime[i][1] = ci[0]/Math.PI;
		}
		
		// Make sure everything is in the strip (roundoff could put it outside).
		for (i = 0; i < yp.length; i++) {
			yp[i][1] = Math.min(1, Math.max(0, yp[i][1]));
		}
	
	}
	
	// Jacobi elliptic functions for complex argument
	// ellipjc returns the values of the Jacobi elliptic functions evaluated at complex argument U and
	// parameter m = exp(-2*PI*L), 0 < L < Infinity.  Recall that m = k^2, where k is the elliptic modulus.
	
	// u may be a vector: L must be a scalar.  The entries of U are expected to lie within the rectangle
	// |Re u| < K, 0 < Im u < Kp, where K and Kp are returned by ellipkkp.
	
	// Original MATLAB routine copyright 1999 by Toby Driscoll.
	
	// The built-in ellipj can't handle complex arguments, and standard transformations to handle this
	// would require ellipj called with parameter 1-m.  When m < eps (or is even close), this can't be
	// done accurately.
	
	// The algorithm is the descending Landen transformation, described in L Howell's PhD thesis from MIT.
	// Additional formulas come from Gradshteyn & Ryzhik, 5th ed., and Abramowitz & Stegun.
	private void ellipjc(double sn[][], double cn[][], double dn[][], double u[][], double L, boolean flag) {
		int i, j;
		int numhigh = 0;
		int high[];
		double m;
		double sinu[] = new double[2];
		double cosu[] = new double[2];
		double sincos[] = new double[2];
		double cr[] = new double[1];
		double ci[] = new double[1];
		double realPart;
		double imagPart;
		double cosSq[] = new double[2];
		double sinSq[] = new double[2];
		double kappa;
		double sqrt1m;
		double md4;
		double md42;
		double md43;
		double md44;
		double md45;
		double md46;
		double mu;
		double v[][] = new double[u.length][2];
		double sn1[][] = new double[u.length][2];
		double cn1[][] = new double[u.length][2];
		double dn1[][] = new double[u.length][2];
		double denom[] = new double[2];
		double sn1Sq[] = new double[2];
		double ucopy[][] = new double[u.length][2];
		
		for (i = 0; i < u.length; i++) {
			ucopy[i][0] = u[i][0];
			ucopy[i][1] = u[i][1];
		}
		
	    if (!flag) {
	    	// flag = false indicates we must check for and transform u in the upper half of the rectangle.
	    	double K[] = new double[1];
	    	double Kp[] = new double[1];
	    	ellipkkp(K, Kp, L);
	    	for (i = 0; i < ucopy.length; i++) {
	    		if (ucopy[i][1] > Kp[0]/2.0) {
	    			numhigh++;
	    		}
	    	} // for (i = 0; i < ucopy.length; i++)
	    	high = new int[numhigh];
	    	for (i = 0, j = 0; i < ucopy.length; i++) {
	    		if (ucopy[i][1] > Kp[0]/2.0) {
	    			ucopy[i][0] = -ucopy[i][0];
	    			ucopy[i][1] = Kp[0] - ucopy[i][1];
	    			high[j++] = i;
	    		}
	    	} // for (i = 0, j = 0; i < ucopy.length; i++)
	    	m = Math.exp(-2.0*Math.PI*L);
	    } // if (!flag)
	    else {
	    	// Recursive call - L is actually m.
	    	high = null;
	    	m = L;
	    }
	    
	    if (m < 4.0 * eps) {
	        for (i = 0; i < u.length; i++) {
	            sinu[0] = Math.sin(ucopy[i][0])*Math.cosh(ucopy[i][1]);
	            sinu[1] = Math.cos(ucopy[i][0])*Math.sinh(ucopy[i][1]);
	            cosu[0] = Math.cos(ucopy[i][0])*Math.cosh(ucopy[i][1]);
	            cosu[1] = -Math.sin(ucopy[i][0])*Math.sinh(ucopy[i][1]);
	            zmlt(sinu[0], sinu[1], cosu[0], cosu[1], cr, ci);
	            sincos[0] = cr[0];
	            sincos[1] = ci[0];
	            realPart = sincos[0] - ucopy[i][0];
	            imagPart = sincos[1] - ucopy[i][1];
	            zmlt(realPart, imagPart, cosu[0], cosu[1], cr, ci);
	            sn[i][0] = sinu[0] + (m/4.0)*cr[0];
	            sn[i][1] = sinu[1] + (m/4.0)*ci[0];
	            realPart = -sincos[0] + ucopy[i][0];
	            imagPart = -sincos[1] + ucopy[i][1];
	            zmlt(realPart, imagPart, sinu[0], sinu[1], cr, ci);
	            cn[i][0] = cosu[0] + (m/4.0)*cr[0];
	            cn[i][1] = cosu[1] + (m/4.0)*ci[0];
	            zmlt(cosu[0], cosu[1], cosu[0], cosu[1], cr, ci);
	            cosSq[0] = cr[0];
	            cosSq[1] = ci[0];
	            zmlt(sinu[0], sinu[1], sinu[0], sinu[1], cr, ci);
	            sinSq[0] = cr[0];
	            sinSq[1] = ci[0];
	            realPart = cosSq[0] - sinSq[0] - 1;
	            imagPart = cosSq[1] - sinSq[1];
	            dn[i][0] = 1.0 + (m/4.0)*realPart;
	            dn[i][1] = (m/4.0)*imagPart;
	        } // for (i = 0; i < u.length; i++)
	    } // if (m < 4.0 * eps)
	    else {
	    	if (m > 1.0e-3) {
	    		sqrt1m = Math.sqrt(1.0 - m);
	    	    kappa = (1 - sqrt1m)/(1.0 + sqrt1m);	
	    	}
	    	else {
	    	    md4 = (m/4.0);
	    	    md42 = md4 * md4;
	    	    md43 = md42 * md4;
	    	    md44 = md43 * md4;
	    	    md45 = md44 * md4;
	    	    md46 = md45 * md4;
	    	    kappa = 132*md46 + 42*md45 + 14*md44 + 5*md43 + 2*md42 + md4;
	    	}
	    	mu = kappa * kappa;
	    	for (i = 0; i < ucopy.length; i++) {
	    	    v[i][0] = ucopy[i][0]/(1.0 + kappa);
	    	    v[i][1] = ucopy[i][1]/(1.0 + kappa);
	    	} // for (i = 0; i < ucopy.length; i++)
	    	ellipjc(sn1, cn1, dn1, v, mu, true);
	    	for (i = 0; i < ucopy.length; i++) {
	    	    zmlt(sn1[i][0], sn1[i][1], sn1[i][0], sn1[i][1], cr, ci);
	    	    sn1Sq[0] = cr[0];
	    	    sn1Sq[1] = ci[0];
	    	    denom[0] = 1.0 + kappa*sn1Sq[0];
	    	    denom[1] = kappa*sn1Sq[1];
	    	    zdiv((1.0 + kappa)*sn1[i][0], (1.0 + kappa)*sn1[i][1], denom[0], denom[1], cr, ci);
	    	    sn[i][0] = cr[0];
	    	    sn[i][1] = ci[0];
	    	    zmlt(cn1[i][0], cn1[i][1],dn1[i][0], dn1[i][1], cr, ci);
	    	    zdiv(cr[0], ci[0], denom[0], denom[1], cr, ci);
	    	    cn[i][0] = cr[0];
	    	    cn[i][1] = ci[0];
	    	    zdiv((1.0 - kappa*sn1Sq[0]), -kappa*sn1Sq[1], denom[0], denom[1], cr, ci);
	    	    dn[i][0] = cr[0];
	    	    dn[i][1] = ci[0];
	    	} // for (i = 0; i < ucopy.length; i++)
	    } // else
	    
	    if (numhigh > 0) {
	    	double snh[] = new double[2];
			double cnh[] = new double[2];
			double dnh[] = new double[2];
		    double sqrtm = Math.sqrt(m);
		    for (i = 0; i < numhigh; i++) {
		        snh[0] = sn[high[i]][0];
		        snh[1] = sn[high[i]][1];
		        cnh[0] = cn[high[i]][0];
		        cnh[1] = cn[high[i]][1];
		        dnh[0] = dn[high[i]][0];
		        dnh[1] = dn[high[i]][1];
		        zdiv(-1.0, 0.0, sqrtm*snh[0], sqrtm*snh[1], cr, ci);
		        sn[high[i]][0] = cr[0];
		        sn[high[i]][1] = ci[0];
		        zdiv(-dnh[1], dnh[0], sqrtm*snh[0], sqrtm*snh[1], cr, ci);
		        cn[high[i]][0] = cr[0];
		        cn[high[i]][1] = ci[0];
		        zdiv(-cnh[1], cnh[0], snh[0], snh[1], cr, ci);
		        dn[high[i]][0] = cr[0];
		        dn[high[i]][1] = ci[0];
		    } // for (i = 0; i < numhigh; i++) 
	    } // if (numhigh > 0)
	}
	
	// ellipkkp is the complete elliptic integral of the first kind, with complement.
	// K[0] returns the value of the complete elliptic integral of the first kind, evaluated at
	// M = exp(-2*PI*L), 0 < L < Infinity/
	// Kp[0] returns the result for the complementary parameter 1-m, which is useful when m < eps.
	// Even when m < 1.0e-6, the built-in ellipke can lose digits of accuracy for Kp.
	// Recall that the elliptic modulus k is related to the parameter m by m = k^2.
	// Original MATLAB routine copyright 1999 by Toby Driscoll.
	
	// elipkkp uses the method of the arithmetic-geometric mean described in 17.6 of M. Abramowitz
	// and I. A. Stegun, "Handbook of Mathematical Functions," Dover, 1965.  Same method as in 
	// ellipke, only interchanging 1 and1-m to find Kp.
	private void ellipkkp(double K[], double Kp[], double L) {
	    double m;
	    double a0;
	    double b0;
	    double s0;
	    int i1;
	    double mm;
	    double a1 = 0.0;
	    double b1;
	    double c1;
	    double w1;
	    // When m = exp(-2*PI*L) is extremely small, use O(m) approximations.
	    if (L > 10.0) {
	    	K[0] = Math.PI/2.0;
	    	Kp[0] = Math.PI*L + Math.log(4.0);
	    	return;
	    } // if (L > 10.0)
	    
	    m = Math.exp(-2.0*Math.PI*L);
	    a0 = 1;
	    b0 = Math.sqrt(1.0-m);
	    s0 = m;
	    i1 = 0;
	    mm = 1.0;
	    while (mm > eps) {
	        a1 = (a0 + b0)/2.0;
	        b1 = Math.sqrt(a0*b0);
	        c1 = (a0-b0)/2.0;
	        i1 = i1 + 1;
	        w1 = Math.pow(2, i1)*c1*c1;
	        mm = w1;
	        s0 = s0 + w1;
	        a0 = a1;
	        b0 = b1;
	    } // while (mm > eps)
	    
	    K[0] = Math.PI/(2.0 * a1);
	    if (m == 1.0) {
	    	K[0] = K[0] * Double.POSITIVE_INFINITY;
	    }
	    
	    a0 = 1.0;
	    b0 = Math.sqrt(m);
	    s0 = 1.0 - m;
	    i1 = 0;
	    mm = 1.0;
	    while (mm > eps) {
	    	a1 = (a0 + b0)/2.0;
	    	b1 = Math.sqrt(a0*b0);
	    	c1 = (a0-b0)/2.0;
	    	i1 = i1 + 1;
	    	w1 = Math.pow(2, i1)*c1*c1;
	    	mm = w1;
	    	s0 = s0 + w1;
	    	a0 = a1;
	    	b0 = b1;
	    } // while (mm > eps)
	    
	    Kp[0] = Math.PI/(2.0 * a1);
	    if (m == 0) {
	    	Kp[0] = Kp[0] * Double.POSITIVE_INFINITY;
	    }
 	}
	
	private void printExitStatus(int exitStatus) {
		if (exitStatus == -1) {
            System.err.println("Abnormal termination because m < n or n <= 0 or m <= 0 or mdc < m or mdw < n*n + 5*n + 3*m + 6 or");
            System.err.println("maxit <= 0 or epsrel < 0 or epsabs < 0 or epsx < 0 or invalid starting point on entry");
        } 
        else if (exitStatus == -2) {
        	System.err.println("Abnormal termination because the number of iterations has exceeded the maximum allowed iterations");
        }
        else if (exitStatus == -3) {
        	System.err.println("Abnormal termination because the Hessian emanating from the 2nd order method is not positive definite");
        }
        else if (exitStatus == -4) {
        	System.err.println("Abnormal termination because the algorithm would like to use 2nd derivatives but is not allowed to do that");
        }
        else if (exitStatus == -5) {
        	System.err.println("Abnormal termination because an undamped step with Newtons method is a failure");
        }
        else if (exitStatus == -6) {
        	System.err.println("Abnormal termination because the latest search direction computed using subspace minimization");
        	System.err.println("was not a descent direction (probably caused by a wrongly computed Jacobian)");
        }
        else if (exitStatus == -7) {
        	System.err.println("Abnormal termination because there is only one feasible point");
        	System.err.println("namely X(I) = BL(I) = BU(I), I = 1,2,...,N");
        }
        else if (exitStatus == -8) {
        	System.err.println("Abnormal termination due to driver error");
        }
        else {
        	System.err.println("Exit status = " + exitStatus);
        }
	}
	
	class dpfun extends NLConstrainedEngine {
		int n;
		double beta[];
		double nmlen[];
		int left[];
		int right[];
		boolean cmplx[];
		double qdat[][];
		
    	public dpfun (double y0[], int n, double beta[], double nmlen[], int left[],
    			int right[], boolean cmplx[], double qdat[][]) {
    		// nPoints, params
    		super(y0.length, y0.length);
    		this.n = n;
    		this.beta = beta;
    		this.nmlen = nmlen;
    		this.left = left;
    		this.right = right;
    		this.cmplx = cmplx;
    		this.qdat = qdat;
    		
    		bounds = 0; // bounds = 0 means unconstrained

			// bounds = 1 means same lower and upper bounds for
			// all parameters
			// bounds = 2 means different lower and upper bounds
			// for all parameters

			// The default is internalScaling = false
			// To make internalScaling = true and have the columns of the
			// Jacobian scaled to have unit length include the following line.
			// internalScaling = true;
			// Suppress diagnostic messages
			outputMes = false;
			for (int i = 0; i < y0.length; i++) {
				gues[i] = y0[i];
			}
    	}
    	
    	/**
		 * Starts the analysis.
		 */
		public void driver() {
			super.driver();
		}
		
		/**
		 * Display results of displaying exponential fitting parameters.
		 */
		public void dumpResults() {
			Preferences
					.debug(" ******* Fit Elsunc Schwarz-Christoffel dparam ********* \n\n",
							Preferences.DEBUG_ALGORITHM);
			Preferences.debug("Number of iterations: " + String.valueOf(iters)
					+ "\n", Preferences.DEBUG_ALGORITHM);
			Preferences.debug("Chi-squared: " + String.valueOf(getChiSquared())
					+ "\n", Preferences.DEBUG_ALGORITHM);
			for (int i = 0; i < a.length; i++) {
			Preferences.debug("a"+i+" " + String.valueOf(a[i]) + "\n",
					Preferences.DEBUG_ALGORITHM);
			}
		}
    	
    	public void fitToFunction(double[] a, double[] residuals, double[][] covarMat) {
    		int ctrl;
    		int i, j;
    		double z[][];
    		double I1[][];
    		double I2[][];
    		double intsnotcmplx[][];
    		double intscmplx[][] = null;
    		double cr[] = new double[1];
    		double ci[] = new double[1];
    		try {
				ctrl = ctrlMat[0];

				if ((ctrl == -1) || (ctrl == 1)) {
                    // Returns residual for solution of nonlinear equations
					
					// Convert a values to z (prevertices)
					double cumprod[] = new double[a.length+1];
					cumprod[0] = 1;
					for (i = 1; i <= a.length; i++) {
						cumprod[i] = cumprod[i-1]*Math.exp(-a[i-1]);
					}
					double cs[] = new double[a.length+1];
					cs[0] = cumprod[0];
					for (i = 1; i <= a.length; i++) {
						cs[i] = cs[i-1] + cumprod[i];
					}
					double theta[] = new double[n-3];
					for (i = 0; i < n-3; i++) {
						theta[i] = Math.PI*cs[i]/cs[cs.length-1];
					}
					z = new double[n][2];
					for (i = 0; i < n-3; i++) {
						z[i][0] = Math.cos(theta[i]);
						z[i][1] = Math.sin(theta[i]);
					}
					z[n-3][0] = -1;
					z[n-3][1] = 0;
					z[n-2][0] = 0;
					z[n-2][1] = -1;					
					z[n-1][0] = 1;
					z[n-1][1] = 0;
					
					// Compute the integrals
					double zleft[][] = new double[left.length][2];
					for (i = 0; i < left.length; i++) {
						zleft[i][0] = z[left[i]][0];
						zleft[i][1] = z[left[i]][1];	
					}
					double zright[][] = new double[right.length][2];
					for (i = 0; i < right.length; i++) {
						zright[i][0] = z[right[i]][0];
						zright[i][1] = z[right[i]][1];	
					}
					double angl[] = new double[left.length];
					for (i = 0; i < left.length; i++) {
						angl[i] = Math.atan2(zleft[i][1], zleft[i][0]);
					}
					double mid[][] = new double[left.length][2];
				    for (i = 0; i < left.length; i++) {
				    	zdiv(zright[i][0], zright[i][1], zleft[i][0], zleft[i][1], cr, ci);
				    	double ang = Math.atan2(ci[0],  cr[0]) + 2.0*Math.PI;
				    	double div = ang/(2.0*Math.PI);
				    	double fix;
				    	if (div >= 0) {
				    		fix = Math.floor(div);
				    	}
				    	else {
				    		fix = Math.ceil(div);
				    	}
				    	double rem = ang - 2.0*Math.PI*fix;
				    	double expvar = (angl[i] + rem/2.0);
				    	mid[i][0] = Math.cos(expvar);
				    	mid[i][1] = Math.sin(expvar);
				    }
				    // For integrals between nonadjacent singularities, choose 0
				    // as intermediate integration point.
				    int numcmplx = 0;
				    for (i = 0; i < cmplx.length; i++) {
				        if (cmplx[i]) {
				        	numcmplx++;
				        	mid[i][0] = 0;
				        	mid[i][1] = 0;
				        }
				    } // for (i = 0; i < cmplx.length; i++)
				    if (numcmplx > 0) {
				        cmplx[0] = true;	
				    }
				    double ints[][] = new double[left.length][2];
				    for (i = 0; i < left.length; i++) {
				    	ints[i][0] = Double.NaN;
				    }
				    numcmplx = 0;
				    for (i = 0; i < cmplx.length; i++) {
				        if (cmplx[i]) {
				        	numcmplx++;
				        }
				    } // for (i = 0; i < cmplx.length; i++)
				    int numnotcmplx = cmplx.length - numcmplx;
				    double zleftnotcmplx[][] = new double[numnotcmplx][2];
				    double midnotcmplx[][] = new double[numnotcmplx][2];
				    int leftnotcmplx[] = new int[numnotcmplx];
				    double zrightnotcmplx[][] = new double[numnotcmplx][2];
				    int rightnotcmplx[] = new int[numnotcmplx];
				    for (i = 0, j = 0; i < cmplx.length; i++) {
				        if (!cmplx[i]) {
				        	zleftnotcmplx[j][0] = zleft[i][0];
				        	zleftnotcmplx[j][1] = zleft[i][1];
				        	midnotcmplx[j][0] = mid[i][0];
				        	midnotcmplx[j][1] = mid[i][1];
				        	leftnotcmplx[j] = left[i];
				        	zrightnotcmplx[j][0] = zright[i][0];
				        	zrightnotcmplx[j][1] = zright[i][1];
				        	rightnotcmplx[j++] = right[i];
				        }
				    } // for (i = 0, j = 0; i < cmplx.length; i++)
				    I1 = dabsquad(zleftnotcmplx, midnotcmplx, leftnotcmplx, z, beta, qdat);
				    I2 = dabsquad(zrightnotcmplx, midnotcmplx, rightnotcmplx, z, beta, qdat);
				    intsnotcmplx = new double[I1.length][I1[0].length];
				    int numintszero = 0;
				    for (i = 0, j = 0; i < cmplx.length; i++) {
				    	if (!cmplx[i]) {
					        intsnotcmplx[j][0] = I1[j][0] + I2[j][0];
					        intsnotcmplx[j][1] = I1[j][1] + I2[j][1];
					        ints[i][0] = intsnotcmplx[j][0];
					        ints[i][1] = intsnotcmplx[j++][1];
					        if ((ints[i][0] == 0) && (ints[i][1] == 0)) {
					        	numintszero++;
					        }
				    	} 
				    }
				    if (numcmplx > 0) {
					    double zleftcmplx[][] = new double[numcmplx][2];
					    double midcmplx[][] = new double[numcmplx][2];
					    int leftcmplx[] = new int[numcmplx];
					    double zrightcmplx[][] = new double[numcmplx][2];
					    int rightcmplx[] = new int[numcmplx];
					    for (i = 0, j = 0; i < cmplx.length; i++) {
					        if (cmplx[i]) {
					        	zleftcmplx[j][0] = zleft[i][0];
					        	zleftcmplx[j][1] = zleft[i][1];
					        	midcmplx[j][0] = mid[i][0];
					        	midcmplx[j][1] = mid[i][1];
					        	leftcmplx[j] = left[i];
					        	zrightcmplx[j][0] = zright[i][0];
					        	zrightcmplx[j][1] = zright[i][1];
					        	rightcmplx[j++] = right[i];
					        }
					    } // for (i = 0, j = 0; i < cmplx.length; i++)
					    I1 = dquad(zleftcmplx, midcmplx, leftcmplx, z, beta, qdat);
					    I2 = dquad(zrightcmplx, midcmplx, rightcmplx, z, beta, qdat);
					    intscmplx = new double[I1.length][I1[0].length];
					    
					    for (i = 0, j = 0; i < cmplx.length; i++) {
					    	if (cmplx[i]) {
						        intscmplx[j][0] = I1[j][0] - I2[j][0];
						        intscmplx[j][1] = I1[j][1] - I2[j][1];
						        ints[i][0] = intscmplx[j][0];
						        ints[i][1] = intscmplx[j++][1];
						        if ((ints[i][0] == 0) && (ints[i][1] == 0)) {
						        	numintszero++;
						        }
					    	} 
					    } // for (i = 0, j = 0; i < cmplx.length; i++)
				    } // if (numcmplx > 0)
				    if (numintszero > 0) {
				    	// Singularities were too crowded in practice
				    	MipavUtil.displayWarning("Severe crowding");
				    }
				    
				    // Compute nonlinear equation residual values.
				    cmplx[0] = false;
				    numcmplx = 0;
				    numnotcmplx = 0;
				    for (i = 0; i < cmplx.length; i++) {
				        if (cmplx[i]) {
				        	numcmplx++;
				        }
				        else {
				        	numnotcmplx++;
				        }
				    }
				    double F1[] = new double[numnotcmplx-1];
				    double absints0 = zabs(ints[0][0], ints[0][1]);
				    for (i = 1, j = 0; i < cmplx.length; i++) {
				    	if (!cmplx[i]) {
				            F1[j++] = ints[i][0]/absints0;
				    	}
				    } // for (i = 1, j = 0; i < cmplx.length; i++) 
				    double F2[][] = new double[numcmplx][2];
				    for (i = 1, j = 0; i < cmplx.length; i++) {
				    	if (cmplx[i]) {
				    		zdiv(ints[i][0], ints[i][1], ints[0][0], ints[0][1], cr, ci);
				    		F2[j][0] = cr[0];
				    		F2[j++][1] = ci[0];
				    	}
				    }
				    double F[] = new double[F1.length + 2*F2.length];
				    for (i = 0; i < F1.length; i++) {
				    	F[i] = F1[i];
				    }
				    for (i = 0; i < F2.length; i++) {
				    	F[F1.length + i] = F2[i][0];
				    	F[F1.length + F2.length + i] = F2[i][1];
				    }
				    for (i = 0; i < F.length; i++) {
				    	residuals[i] = F[i] - nmlen[i];
				    }
				} // if ((ctrl == -1) || (ctrl == 1))

				// Calculate the Jacobian numerically
				else if (ctrl == 2) {
					ctrlMat[0] = 0;
				}
			} catch (Exception e) {
				Preferences.debug("function error: " + e.getMessage() + "\n",
						Preferences.DEBUG_ALGORITHM);
			}

			return;
    		
    	}
    }
	
	private double[][] dquad(double z1[][], double z2[][], int sing1[], double z[][],
			double beta[], double qdat[][]) {
		// Numerical quadrature for the disk map.
		// dquad performs the integration for the SC disk formula.  z1 and z2
		// are vectors of left and right endpoints.  If z1[k] = z[m], then 
		// sing1[k] should have the value m; otherwise, sing1[k] should be -1.
		
		// z and beta are prevertices and turning angles for the SC map.  qdat
		// is a matrix of quadrature data (see scqdata).
		
		// The integration is adaptive in the sense that members of z (with nonzero
		// beta) that are close to the left endpoints cause subdivision.  This is
		// NOT true of the singularities close to the right end.
		
		// Original MATLAB routine copyright 1998-2001 by Toby Driscoll.
		
		// z1,z2 are vectors of left and right endpoints.  sing1 is a vector of 
		// indices which label the singularities in z1.  So if sing1[5] = 3,
		// then z1[5] = z[3].  A -1 means no singularity.  z is the vector of
		// singularities; beta is the vector of associated turning angles. qdat
		// is quadrature data from scqdata.
		
		// Make sure z and beta are column vectors.
		
		// dquad integrates from a possible singularity at the left end to a
		// regular point at the right.  If both endpoints are singularities,
		// you must break the integral into two pieces and make two calls.
		
		// The integral is subdivided, if necessary, so that no singularity lies
		// closer  to the left endpoint than 1/2 the length of the integration
		// (sub) interval
		int i, j, k, m;
		double cr[] = new double[1];
		double ci[] = new double[1];
		int nqpts = qdat.length;
		int n = z.length;
		double bigz[][][] = new double[n][nqpts][2];
		for (i = 0; i < n; i++) {
		    for (j = 0; j < nqpts; j++) {
		    	bigz[i][j][0] = z[i][0];
		    	bigz[i][j][1] = z[i][1];
		    }
		} // for (i = 0; i < n; i++)
		double bigbeta[][] = new double[beta.length][nqpts];
		for (i = 0; i < beta.length; i++) {
			for (j = 0; j < nqpts; j++) {
				bigbeta[i][j] = beta[i];
			}
		} // for (i = 0; i < beta.length; i++)
		if ((sing1 == null) || (sing1.length == 0)) {
			sing1 = new int[z1.length];
			for (i = 0; i < z1.length; i++) {
				sing1[i] = -1;
			}
		} // if ((sing1 == null) || (sing1.length == 0))
		double I[][] = new double[z1.length][2];
		int numnontriv = 0;
		for (i = 0; i < z1.length; i++) {
			if ((z1[i][0] != z2[i][0]) || (z1[i][1] != z2[i][1])) {
				numnontriv++;
			}
		} // for (i = 0; i < z1.length; i++)
		int nontriv[] = new int[numnontriv];
		for (i = 0, j = 0; i < z1.length; i++) {
			if ((z1[i][0] != z2[i][0]) || (z1[i][1] != z2[i][1])) {
				nontriv[j++] = i;
			}
		} // for (i = 0, j = 0; i < z1.length; i++)
		
		double za[] = new double[2];
		double zb[] = new double[2];
		double nd[][] = new double[nqpts][2];
		double wt[][] = new double[nqpts][2];
		double terms[][][] = new double[n][nqpts][2];
		double zr[] = new double[2];
		double zl[] = new double[2];
		double logterms[] = new double[2];
		double prod[] = new double[2];
		double expSum[] = new double[2];
		double expTerm;
		double realExp;
		double imagExp;
		for (i = 0; i < nontriv.length; i++) {
			k = nontriv[i];
			za[0] = z1[k][0];
			za[1] = z1[k][1];
			zb[0] = z2[k][0];
			zb[1] = z2[k][1];
			int sng = sing1[k];
			
			// Allowable integration step, based on nearest singularity.
			double dist = 1.0;
			double denom = zabs(zb[0]-za[0],zb[1]-za[1]);
			double minVal = Double.MAX_VALUE;
			double absDiff;
			for (j = 0; j <=sng-1; j++) {
				absDiff = zabs(z[j][0] - za[0],z[j][1] - za[1]);
				if (absDiff < minVal) {
					minVal = absDiff;
				}
			} // for (j = 0; j <=sng-1; j++)
			for (j = sng+1; j <= n-1; j++) {
				absDiff = zabs(z[j][0] - za[0],z[j][1] - za[1]);
				if (absDiff < minVal) {
					minVal = absDiff;
				}	
			} // for (j = sng+1; j <= n-1; j++)
			minVal = 2.0*minVal/denom;
			if (minVal < dist) {
				dist = minVal;
			}
			zr[0] = za[0] + dist * (zb[0] - za[0]);
			zr[1] = za[1] + dist * (zb[1] - za[1]);
			// Adjust Gauss-Jacobi nodes and weights to interval.
			int ind = (sng+n+1)%(n+1);
			for (j = 0; j < nqpts; j++) {
				nd[j][0] = ((zr[0] - za[0])*qdat[j][ind] + zr[0] + za[0])/2.0; // G-J nodes
				nd[j][1] = ((zr[1] - za[1])*qdat[j][ind] + zr[1] + za[1])/2.0;
				wt[j][0] = ((zr[0]-za[0])/2.0) * qdat[j][ind+n+1];
				wt[j][1] = ((zr[1]-za[1])/2.0) * qdat[j][ind+n+1];// G-J weights
			} // for (j = 0; j < nqpts; j++)
			int zeroterms = 0;
			for (j = 0; j < n; j++) {
				for (m = 0; m < nqpts; m++) {
				    zdiv(nd[m][0], nd[m][1], bigz[j][m][0], bigz[j][m][1],cr,ci);
				    terms[j][m][0] = 1 - cr[0];
				    terms[j][m][1] = -ci[0];
				    if ((terms[j][m][0] == 0) && (terms[j][m][1] == 0)) {
				    	zeroterms++;
				    }
				}
			} // for (j = 0; j < n; j++)
			if (zeroterms > 0) {
				// Endpoints are practically coincident
				I[k][0] = 0;
				I[k][1] = 0;
			}
			else {
			    // Use Gauss-Jacobi on first subinterval, if necessary.
				if (sng >= 0) {
					double fac = zabs(zr[0]-za[0],zr[1]-za[1])/2.0;
				    double fac2 = Math.pow(fac, beta[sng]);
					for (m = 0; m < nqpts; m++) {
					    denom = zabs(terms[sng][m][0], terms[sng][m][1]);
					    terms[sng][m][0] = terms[sng][m][0]/denom;
					    terms[sng][m][1] = terms[sng][m][1]/denom;
					    wt[m][0] = wt[m][0] * fac2;
					    wt[m][1] = wt[m][1] * fac2;
					}
				} // if (sng >= 0)
				I[k][0] = 0;
				I[k][1] = 0;
			    for (m = 0; m < nqpts; m++) {
			    	expSum[0] = 0;
			    	expSum[1] = 0;
			    	for (j = 0; j < n; j++) {
						logterms[0] = Math.log(zabs(terms[j][m][0],terms[j][m][1]));
						logterms[1] = Math.atan2(terms[j][m][1], terms[j][m][0]);
						prod[0] = logterms[0] * bigbeta[j][m];
						prod[1] = logterms[1] * bigbeta[j][m];
						expSum[0] += prod[0];
						expSum[1] += prod[1];
					} // for (j = 0; j < n; j++)
			    	expTerm = Math.exp(expSum[0]);
			    	zmlt(expTerm*Math.cos(expSum[1]), expTerm*Math.sin(expSum[1]),
			    			wt[m][0], wt[m][1], cr, ci);
			    	I[k][0] += cr[0];
			    	I[k][1] += ci[0];
				} // for (m = 0; m < nqpts; m++)
			    while (dist < 1) {
			        // Do regular Gaussian quad on other subintervals.
			    	zl[0] = zr[0];
			    	zl[1] = zr[1];
			    	dist = 1.0;
			    	minVal = Double.MAX_VALUE;
			    	denom = zabs(zl[0] - zb[0], zl[1] - zb[1]);
			    	for (j = 0; j < n; j++) {
			    	    double num = zabs(z[j][0] - zl[0], z[j][1] - zl[1]);
			    	    if (num < minVal) {
			    	    	minVal = num;
			    	    }
			    	} // (j = 0; j < n; j++)
			    	minVal = 2.0*minVal/denom;
			    	if (minVal < dist) {
			    		dist = minVal;
			    	}
			    	zr[0] = zl[0] + dist * (zb[0] - zl[0]);
			    	zr[1] = zl[1] + dist * (zb[1] - zl[1]);
			    	for (j = 0; j < nqpts; j++) {
						nd[j][0] = ((zr[0] - zl[0])*qdat[j][n] + zr[0] + zl[0])/2.0;
						nd[j][1] = ((zr[1] - zl[1])*qdat[j][n] + zr[1] + zl[1])/2.0;
						wt[j][0] = ((zr[0]-zl[0])/2.0) * qdat[j][2*n+1];
						wt[j][1] = ((zr[1]-zl[1])/2.0) * qdat[j][2*n+1];
					} // for (j = 0; j < nqpts; j++)
			    	//I(k) = I(k) + exp(sum(log(1 - nd(ones(n,1),:)./bigz).*bigbeta)) * wt
			    	for (m = 0; m < nqpts; m++) {
			    		expSum[0] = 0;
			    		expSum[1] = 0;
						for (j = 0; j < n; j++) {
						    zdiv(nd[m][0], nd[m][1], bigz[j][m][0], bigz[j][m][1],cr,ci);
						    logterms[0] = Math.log(zabs(1 - cr[0],-ci[0]));
						    logterms[1] = Math.atan2(-ci[0],1-cr[0]);
						    prod[0] = logterms[0] * bigbeta[j][m];
						    prod[1] = logterms[1] * bigbeta[j][m];
						    expSum[0] += prod[0];
						    expSum[1] += prod[1];
						}
						expTerm = Math.exp(expSum[0]);
						zmlt(expTerm*Math.cos(expSum[1]), expTerm*Math.sin(expSum[1]), wt[m][0], wt[m][1], cr, ci);
						I[k][0] += cr[0];
						I[k][1] += ci[0];
			    	} // for (m = 0; m < nqpts; m++)
			    } // while (dist < 1)
			} // else
		} // for (i = 0; i < nontriv.length; i++)
		return I;
	}
	
	private double[][] dabsquad(double z1[][], double z2[][], int sing1[], double z[][],
			double beta[], double qdat[][]) {
		// Numerical quadrature for side lengths in the disk map.
		
		// dabsquad is similar to dquad, but the integrand is replaced by its
		// absolute value and the path of integration is along the unit circle
		// rather than the straight line between endpoints.  This allows the use
		// of real rather than complex logarithms in the computations of powers, 
		// which can result in substantial savings.
		
		// z1,z2 are vectors of left and right endpoints.  sing1 is a vector of 
		// indices which label the singularities in z1.  So if sing1[5] = 3,
		// then z1[5] = z[3].  A -1 means no singularity.  z is the vector of
		// singularities; beta is the vector of associated turning angles. qdat
		// is quadrature data from scqdata.
		
		// Make sure z and beta are column vectors.
		
		// dabsquad integrates from a possible singularity at the left end to a
		// regular point at the right.  If both endpoints are singularities,
		// you must break the integral into two pieces and make two calls.  The
		// integration always takes along the smaller arc of the circle between
		// the endpoints.
		
		// The integral is subdivided, if necessary, so that no singularity lies
		// closer  to the left endpoint than 1/2 the length of the integration
		// (sub) interval
		
		// Original MATLAB routine copyright 1997 by Toby Driscoll.
		// Last updated 05/08/97.
		int i, j, k, m;
		double cr[] = new double[1];
		double ci[] = new double[1];
		int nqpts = qdat.length;
		int n = z.length;
		double argz[] = new double[n];
		for (i = 0; i < n; i++) {
			argz[i] = Math.atan2(z[i][1], z[i][0]); 
		}
		double argz1[] = new double[z1.length];
		for (i = 0; i < z1.length; i++) {
			argz1[i] = Math.atan2(z1[i][1], z1[i][0]);
		}
		double argz2[] = new double[z2.length];
		for (i = 0; i < z2.length; i++) {
			argz2[i] = Math.atan2(z2[i][1], z2[i][0]);
		}
		double ang21[] = new double[z1.length];
		for (i = 0; i < z1.length; i++) {
			zdiv(z2[i][0], z2[i][1], z1[i][0], z1[i][1], cr, ci);
			ang21[i] = Math.atan2(ci[0], cr[0]);
		}
		// Modification needed if integration passes through -1
		for (i = 0; i < z1.length; i++) {
			if (((argz2[i] - argz1[i]) * ang21[i]) < 0.0) {
				argz2[i] = argz2[i] + 2.0*Math.PI*sign(ang21[i]);
			}
		} // for (i = 0; i < z1.length; i++)
		double bigargz[][] = new double[n][nqpts];
		for (i = 0; i < n; i++) {
		    for (j = 0; j < nqpts; j++) {
		    	bigargz[i][j] = argz[i];
		    }
		} // for (i = 0; i < n; i++)
		double bigbeta[][] = new double[beta.length][nqpts];
		for (i = 0; i < beta.length; i++) {
			for (j = 0; j < nqpts; j++) {
				bigbeta[i][j] = beta[i];
			}
		} // for (i = 0; i < beta.length; i++)
		if ((sing1 == null) || (sing1.length == 0)) {
			sing1 = new int[z1.length];
			for (i = 0; i < z1.length; i++) {
				sing1[i] = -1;
			}
		} // if ((sing1 == null) || (sing1.length == 0))
		
		
		double I[][] = new double[z1.length][2];
		int numnontriv = 0;
		for (i = 0; i < z1.length; i++) {
			if ((z1[i][0] != z2[i][0]) || (z1[i][1] != z2[i][1])) {
				numnontriv++;
			}
		} // for (i = 0; i < z1.length; i++)
		int nontriv[] = new int[numnontriv];
		for (i = 0, j = 0; i < z1.length; i++) {
			if ((z1[i][0] != z2[i][0]) || (z1[i][1] != z2[i][1])) {
				nontriv[j++] = i;
			}
		} // for (i = 0, j = 0; i < z1.length; i++)
		
		double za[] = new double[2];
		double zb[] = new double[2];
		double nd[] = new double[nqpts];
		double wt[] = new double[nqpts];
		double theta[][] = new double[n][nqpts];
		double terms[][] = new double[n][nqpts];
		double logterms[] = new double[2];
		double prod[] = new double[2];
		double expSum[] = new double[2];
		double zl[] = new double[2];
		for (i = 0; i < nontriv.length; i++) {
			k = nontriv[i];
			double arga = argz1[k];
			double argb = argz2[k];
			za[0] = z1[k][0];
			za[1] = z1[k][1];
			zb[0] = z2[k][0];
			zb[1] = z2[k][1];
			int sng = sing1[k];
			
			// Allowable integration step, based on nearest singularity.
			double dist = 1.0;
			double denom = zabs(zb[0]-za[0],zb[1]-za[1]);
			double minVal = Double.MAX_VALUE;
			double absDiff;
			for (j = 0; j <=sng-1; j++) {
				absDiff = zabs(z[j][0] - za[0],z[j][1] - za[1]);
				if (absDiff < minVal) {
					minVal = absDiff;
				}
			} // for (j = 0; j <=sng-1; j++)
			for (j = sng+1; j <= n-1; j++) {
				absDiff = zabs(z[j][0] - za[0],z[j][1] - za[1]);
				if (absDiff < minVal) {
					minVal = absDiff;
				}	
			} // for (j = sng+1; j <= n-1; j++)
			minVal = 2.0*minVal/denom;
			if (minVal < dist) {
				dist = minVal;
			}
			double argr = arga + dist * (argb-arga);
			// Adjust Gauss-Jacobi nodes and weights to interval.
			int ind = (sng+n+1)%(n+1);
			for (j = 0; j < nqpts; j++) {
				nd[j] = ((argr - arga)*qdat[j][ind] + argr + arga)/2.0; // G-J nodes
				wt[j] = (Math.abs(argr-arga)/2.0) * qdat[j][ind+n+1]; // G-J weights
			} // for (j = 0; j < nqpts; j++)
			int zeroterms = 0;
			for (j = 0; j < n; j++) {
				for (m = 0; m < nqpts; m++) {
					double ang = nd[m] - bigargz[j][m] + 2.0*Math.PI;
					double div = ang/(2.0*Math.PI);
			    	double fix;
			    	if (div >= 0) {
			    		fix = Math.floor(div);
			    	}
			    	else {
			    		fix = Math.ceil(div);
			    	}
			    	theta[j][m] = ang - 2.0*Math.PI*fix;
			    	if (theta[j][m] > Math.PI) {
			    		theta[j][m] = 2.0*Math.PI - theta[j][m];
			    	}
			    	terms[j][m] = 2.0 * Math.sin(theta[j][m]/2.0);
			    	if (terms[j][m] == 0) {
			    		zeroterms++;
			    	}
				}
			} // for (j = 0; j < n; j++)
			if (zeroterms > 0) {
				// Endpoints are practically coincident
				I[k][0] = 0;
				I[k][1] = 0;
			}
			else {
			    // Use Gauss-Jacobi on first subinterval, if necessary.
				if (sng >= 0) {
					double fac = Math.abs(argr-arga)/2.0;
				    double fac2 = Math.pow(fac, beta[sng]);
					for (m = 0; m < nqpts; m++) {
					    denom = Math.abs(nd[m] - arga);
					    terms[sng][m] = terms[sng][m]/denom;
					    wt[m] = wt[m] * fac2;
					}
				} // if (sng >= 0)
				I[k][0] = 0;
				I[k][1] = 0;
			    for (m = 0; m < nqpts; m++) {
			    	expSum[0] = 0;
			    	expSum[1] = 0;
			    	for (j = 0; j < n; j++) {
						logterms[0] = Math.log(Math.abs(terms[j][m]));
						logterms[1] = Math.atan2(0, terms[j][m]);
						prod[0] = logterms[0] * bigbeta[j][m];
						prod[1] = logterms[1] * bigbeta[j][m];
						expSum[0] += prod[0];
						expSum[1] += prod[1];
					} // for (j = 0; j < n; j++)
			    	double expTerm = Math.exp(expSum[0]);
			    	I[k][0] += expTerm*Math.cos(expSum[1]) * wt[m];
			    	I[k][1] += expTerm*Math.sin(expSum[1]) * wt[m];
				} // for (m = 0; m < nqpts; m++)
			    while (dist < 1) {
			        // Do regular Gaussian quad on other subintervals.
			    	double argl = argr;
			    	zl[0] = Math.cos(argl);
			    	zl[1] = Math.sin(argl);
			    	dist = 1.0;
			    	minVal = Double.MAX_VALUE;
			    	denom = zabs(zl[0] - zb[0], zl[1] - zb[1]);
			    	for (j = 0; j < n; j++) {
			    	    double num = zabs(z[j][0] - zl[0], z[j][1] - zl[1]);
			    	    if (num < minVal) {
			    	    	minVal = num;
			    	    }
			    	} // (j = 0; j < n; j++)
			    	minVal = 2.0*minVal/denom;
			    	if (minVal < dist) {
			    		dist = minVal;
			    	}
			    	argr = argl + dist*(argb-argl);
			    	for (j = 0; j < nqpts; j++) {
						nd[j] = ((argr - argl)*qdat[j][n] + argr + argl)/2.0; 
						wt[j] = (Math.abs(argr-argl)/2.0) * qdat[j][2*n+1];
					} // for (j = 0; j < nqpts; j++)
			    	for (j = 0; j < n; j++) {
						for (m = 0; m < nqpts; m++) {
							double ang = nd[m] - bigargz[j][m] + 2.0*Math.PI;
							double div = ang/(2.0*Math.PI);
					    	double fix;
					    	if (div >= 0) {
					    		fix = Math.floor(div);
					    	}
					    	else {
					    		fix = Math.ceil(div);
					    	}
					    	theta[j][m] = ang - 2.0*Math.PI*fix;
					    	if (theta[j][m] > Math.PI) {
					    		theta[j][m] = 2.0*Math.PI - theta[j][m];
					    	}
						}
					} // for (j = 0; j < n; j++)
			    	for (m = 0; m < nqpts; m++) {
				    	expSum[0] = 0;
				    	expSum[1] = 0;
				    	for (j = 0; j < n; j++) {
				    		double term = 2.0*Math.sin(theta[j][m]/2.0);
							logterms[0] = Math.log(Math.abs(term));
							logterms[1] = Math.atan2(0, term);
							prod[0] = logterms[0] * bigbeta[j][m];
							prod[1] = logterms[1] * bigbeta[j][m];
							expSum[0] += prod[0];
							expSum[1] += prod[1];
						} // for (j = 0; j < n; j++)
				    	double expTerm = Math.exp(expSum[0]);
				    	I[k][0] += expTerm*Math.cos(expSum[1]) * wt[m];
				    	I[k][1] += expTerm*Math.sin(expSum[1]) * wt[m];
					} // for (m = 0; m < nqpts; m++)
			    } // while (dist < 1)
			} // else
		} // for (i = 0; i < nontriv.length; i++)
		return I;
	}
	
	class rpfun extends NLConstrainedEngine {
		int n;
		double beta[];
		double nmlen[][];
		int left[];
		int right[];
		boolean cmplx[];
		double qdat[][];
		int corners[];
		
    	public rpfun (double y0[], int n, double beta[], double nmlen[][], int left[],
    			int right[], boolean cmplx[], double qdat[][], int corners[]) {
    		// nPoints, params
    		super(y0.length, y0.length);
    		this.n = n;
    		this.beta = beta;
    		this.nmlen = nmlen;
    		this.left = left;
    		this.right = right;
    		this.cmplx = cmplx;
    		this.qdat = qdat;
    		this.corners = corners;
    		
    		bounds = 0; // bounds = 0 means unconstrained

			// bounds = 1 means same lower and upper bounds for
			// all parameters
			// bounds = 2 means different lower and upper bounds
			// for all parameters

			// The default is internalScaling = false
			// To make internalScaling = true and have the columns of the
			// Jacobian scaled to have unit length include the following line.
			// internalScaling = true;
			// Suppress diagnostic messages
			outputMes = false;
			for (int i = 0; i < y0.length; i++) {
				gues[i] = y0[i];
			}
    	}
    	
    	/**
		 * Starts the analysis.
		 */
		public void driver() {
			super.driver();
		}
		
		/**
		 * Display results of displaying exponential fitting parameters.
		 */
		public void dumpResults() {
			Preferences
					.debug(" ******* Fit Elsunc Schwarz-Christoffel rparam ********* \n\n",
							Preferences.DEBUG_ALGORITHM);
			Preferences.debug("Number of iterations: " + String.valueOf(iters)
					+ "\n", Preferences.DEBUG_ALGORITHM);
			Preferences.debug("Chi-squared: " + String.valueOf(getChiSquared())
					+ "\n", Preferences.DEBUG_ALGORITHM);
			for (int i = 0; i < a.length; i++) {
			Preferences.debug("a"+i+" " + String.valueOf(a[i]) + "\n",
					Preferences.DEBUG_ALGORITHM);
			}
		}
    	
    	public void fitToFunction(double[] a, double[] residuals, double[][] covarMat) {
    		int ctrl;
    		int i, j, k;
    		double z[][];
    		double I1[][];
    		double I2[][];
    		double I3[][];
    		double cr[] = new double[1];
    		double ci[] = new double[1];
    		try {
				ctrl = ctrlMat[0];

				if ((ctrl == -1) || (ctrl == 1)) {
                    // Returns residual for solution of nonlinear equations
					
					// Transform a (unconstrained variables) to z (actual params)
					z = new double[a.length+3][2];
					rptrnsfm(z, a, corners);
					
					// Compute the integrals appearing in the nonlinear equations
					double zleft[][] = new double[left.length][2];
					for (i = 0; i < left.length; i++) {
						zleft[i][0] = z[left[i]][0];
						zleft[i][1] = z[left[i]][1];
					}
					double zright[][] = new double[right.length][2];
					for (i = 0; i < right.length; i++) {
						zright[i][0] = z[right[i]][0];
						zright[i][1] = z[right[i]][1];
					}
					
					// To use stquad, must put strip ends into z, beta
					double diffz[] = new double[z.length];
					int nonzerodiff = 0;
					for (i = 0; i < n-1; i++) {
						diffz[i] = z[i+1][1] - z[i][1];
						if (diffz[i] != 0) {
							nonzerodiff++;
						}
					}
					diffz[n-1] = z[0][1] - z[n-1][1];
					if (diffz[n-1] != 0) {
						nonzerodiff++;
					}
					int ends[] = new int[nonzerodiff];
					for (i = 0, j = 0; i < diffz.length; i++) {
						if (diffz[i] != 0) {
						    ends[j++] = i;	
						}
					}
					double z2[][] = new double[n+2][2];
					double beta2[] = new double[n+2];
					for (i = 0; i <= ends[0]; i++) {
						z2[i][0] = z[i][0];
						z2[i][1] = z[i][1];
						beta2[i] = beta[i];
					}
					z2[ends[0]+1][0] = Double.POSITIVE_INFINITY;
					z2[ends[0]+1][1] = 0;
					beta2[ends[0]+1] = 0;
					for (i = ends[0]+1; i <= ends[1]; i++) {
						z2[i+1][0] = z[i][0];
						z2[i+1][1] = z[i][1];
						beta2[i+1] = beta[i];
					}
					z2[ends[1]+2][0] = Double.NEGATIVE_INFINITY;
					z2[ends[1]+2][1] = 0;
					beta2[ends[1]+2] = 0;
					for (i = ends[1]+1; i < n; i++) {
						z2[i+2][0] = z[i][0];
						z2[i+2][1] = z[i][1];
						beta2[i+2] = beta[i];
					}
					// Put dummy columns into qdat after ends[0] and ends[1] columns
					// qdata is nqpts rows by 2*n+2 columns
					// so we are expanding it to 2*n+6 columns with the insertion
					// of 2 more of column n and 2 more of column 2*n+1.
					double qdat2[][] = new double[qdat.length][qdat[0].length+4];
					for (i = 0; i < qdat.length; i++) {
						for (j = 0; j <= ends[0]; j++) {
							qdat2[i][j] = qdat[i][j];
							qdat2[i][j+2+n+1] = qdat[i][j+n+1];
						}
						qdat2[i][ends[0]+1] = qdat[i][n];
						qdat2[i][ends[0]+3+n+1] = qdat[i][2*n+1];
						for (j = ends[0]+1; j <= ends[1]; j++) {
							qdat2[i][j+1] = qdat[i][j];
							qdat2[i][j+3+n+1] = qdat[i][j+n+1];
						}
						qdat2[i][ends[1]+2] = qdat[i][n];
						qdat2[i][ends[1]+4+n+1] = qdat[i][2*n+1];
						for (j = ends[1]+1; j <= n-1; j++) {
							qdat2[i][j+2] = qdat[i][j];
							qdat2[i][j+4+n+1] = qdat[i][j+n+1];
						}
						qdat2[i][n+2] = qdat[i][n];
						qdat2[i][2*n+5] = qdat[i][2*n+1];
					} // for (i = 0; i < qdat.length; i++)
					// Change singularity indices to reflect ends
					int left2[] = new int[left.length];
					for (i = 0; i < left.length; i++) {
						if (left[i] > ends[1]) {
							left2[i] = left[i] + 2;
						}
						else if (left[i] > ends[0]) {
							left2[i] = left[i] + 1;
						}
						else {
							left2[i] = left[i];
						}
					} // for (i = 0; i < left.length; i++)
					int right2[] = new int[right.length];
					for (i = 0; i < right.length; i++) {
						if (right[i] > ends[1]) {
							right2[i] = right[i] + 2;
						}
						else if (right[i] > ends[0]) {
							right2[i] = right[i] +1;
						}
						else {
							right2[i] = right[i];
						}
					} // for (i = 0; i < right.length; i++)
					
					double ints[][] = new double[zleft.length][2];
					int nums2 = 0;
					for (i = 0; i < left2.length; i++) {
						if ((right2[i] - left2[i] == 1) && (zleft[i][1] == zright[i][1])) {
							nums2++;
						}
					}
					int s2[] = new int[nums2];
					int nots2[] = new int[left2.length-nums2];
					for (i = 0, j = 0, k = 0; i < left2.length; i++) {
						if ((right2[i] - left2[i] == 1) && (zleft[i][1] == zright[i][1])) {
							s2[j++] = i;
						}
						else {
							nots2[k++] = i;
						}
					}
					double mid[][] = new double[s2.length][2];
					for (i = 0; i < s2.length; i++) {
						mid[i][0] = (zleft[s2[i]][0] + zright[s2[i]][0])/2.0;
						mid[i][1] = (zleft[s2[i]][1] + zright[s2[i]][1])/2.0;
					}
				    double zlefts2[][] = new double[s2.length][2];
				    int lefts2[] = new int[s2.length];
				    double zrights2[][] = new double[s2.length][2];
				    int rights2[] = new int[s2.length];
				    for (i = 0; i < s2.length; i++) {
				    	zlefts2[i][0] = zleft[s2[i]][0];
				    	zlefts2[i][1] = zleft[s2[i]][1];
				    	lefts2[i] = left2[s2[i]];
				    	zrights2[i][0] = zright[s2[i]][0];
				    	zrights2[i][1] = zright[s2[i]][1];
				    	rights2[i] = right2[s2[i]];
				    }
				    I1 = stquadh(zlefts2, mid, lefts2, z2, beta2, qdat2);
				    I2 = stquadh(zrights2, mid, rights2, z2, beta2, qdat2);
				    for (i = 0; i < s2.length; i++) {
				        ints[s2[i]][0] = I1[i][0] - I2[i][0];
				        ints[s2[i]][1] = I1[i][1] - I2[i][1];
				    }
				    
				    // Three-stage integrations
				    int lengthnots2 = zleft.length - s2.length;
				    double mid1[][] = new double[lengthnots2][2];
				    double mid2[][] = new double[lengthnots2][2];
				    for (i = 0; i < lengthnots2; i++) {
				    	mid1[i][0] = zleft[nots2[i]][0];
				    	mid1[i][1] = 0.5;
				    	mid2[i][0] = zright[nots2[i]][0];
				    	mid2[i][1] = 0.5;
				    }
				    double zleftns2[][] = new double[nots2.length][2];
				    int leftns2[] = new int[nots2.length];
				    double zrightns2[][] = new double[nots2.length][2];
				    int rightns2[] = new int[nots2.length];
				    for (i = 0; i < nots2.length; i++) {
				    	zleftns2[i][0] = zleft[nots2[i]][0];
				    	zleftns2[i][1] = zleft[nots2[i]][1];
				    	leftns2[i] = left2[nots2[i]];
				    	zrightns2[i][0] = zright[nots2[i]][0];
				    	zrightns2[i][1] = zright[nots2[i]][1];
				    	rightns2[i] = right2[nots2[i]];
				    }
				    int zero[] = new int[nots2.length];
				    I1 = stquad(zleftns2, mid1, leftns2, z2, beta2, qdat2);
				    I2 = stquadh(mid1, mid2, zero, z2, beta2, qdat2);
				    I3 = stquad(zrightns2, mid2, rightns2, z2, beta2, qdat2);
				    for (i = 0; i < nots2.length; i++) {
				    	ints[nots2[i]][0] = I1[i][0] + I2[i][0] - I3[i][0];
				    	ints[nots2[i]][1] = I1[i][1] + I2[i][1] - I3[i][1];
				    }
				    
				    boolean found = false;
				    for (i = 0; i < ints.length && (!found); i++) {
				    	if (((ints[i][0] == 0) && (ints[i][1] == 0)) || Double.isNaN(ints[i][0]) || Double.isNaN(ints[i][1])) {
				    		// Singularities were too crowded
				    		found = true;
				    		MipavUtil.displayWarning("Severe crowding");
				    	}
				    } // for (i = 0; i < ints.length && (!found); i++)
				    
				    // Compute nonlinear equation residual values
				    boolean cmplx2[] = new boolean[cmplx.length-1];
				    for (i = 0; i < cmplx.length-1; i++) {
				    	cmplx2[i] = cmplx[i+1];
				    }
				    int numcmplx = 0;
				    int numnotcmplx = 0;
				    for (i = 0; i < cmplx.length; i++) {
				    	if (cmplx[i]) {
				    		numcmplx++;
				    	}
				    	else {
				    		numnotcmplx++;
				    	}
				    } // for (i = 0; i < cmplx.length; i++)
				    double F[] = new double[numnotcmplx];
				    for (i = 0, j = 0; i < ints.length; i++) {
				    	if (!cmplx[i]) {
				    	    F[j++] = zabs(ints[i][0],ints[i][1]);	// F[0] = abs(ints[0])
				    	}
				    }
				    int numcmplx2 = 0;
				    int numnotcmplx2 = 0;
				    for (i = 0; i < cmplx2.length; i++) {
				    	if (cmplx2[i]) {
				    		numcmplx2++;
				    	}
				    	else {
				    		numnotcmplx2++;
				    	}
				    } // for (i = 0; i < cmplx2.length; i++)
				    double nmlennotcmplx2[] = new double[numnotcmplx2];
				    double nmlencmplx2[][] = new double[numcmplx2][2];
				    for (i = 0, j = 0, k = 0; i < nmlen.length; i++) {
				    	if (!cmplx2[i]) {
				    		nmlennotcmplx2[j++] = nmlen[i][0];
				    	}
				    	else {
				    		nmlencmplx2[k][0] = nmlen[i][0];
				    		nmlencmplx2[k++][1] = nmlen[i][1];
				    	}
				    } // for (i = 0, j = 0, k = 0; i < nmlen.length; i++)
				    double Fmod[] = new double[F.length-1];
				    for (i = 0; i < F.length-1; i++) {
				    	Fmod[i] = Math.log(F[i+1]/F[0]/nmlennotcmplx2[i]);
				    	residuals[i] = Fmod[i];
				    }
				    if (numcmplx > 0) {
				    	double intscmplx[][] = new double[numcmplx][2];
				    	for (i = 0, j = 0; i < cmplx.length; i++) {
					    	if (cmplx[i]) {
					    	    intscmplx[j][0] = ints[i][0];
					    	    intscmplx[j++][1] = ints[i][1];
					    	}
					    } // for (i = 0, j = 0; i < cmplx.length; i++)
				    	double realF2[] = new double[numcmplx];
				    	double imagF2[] = new double[numcmplx];
				    	for (i = 0; i < numcmplx; i++) {
				    	    zdiv(intscmplx[i][0], intscmplx[i][1], ints[0][0], ints[0][1], cr, ci);
				    	    zdiv(cr[0], ci[0], nmlencmplx2[i][0], nmlencmplx2[i][1], cr, ci);
				    	    realF2[i] = Math.log(zabs(cr[0],ci[0]));
				    	    imagF2[i] = Math.atan2(ci[0], cr[0]);
				    	} // for (i = 0; i < numcmplx; i++)
				    	for (i = 0; i < numcmplx; i++) {
			    	    	residuals[i+Fmod.length] = realF2[i];
			    	    	residuals[i+Fmod.length+numcmplx] = imagF2[i];
			    	    }
				    } // if (numcmplx > 0)
				    
				} // if ((ctrl == -1) || (ctrl == 1))

				// Calculate the Jacobian numerically
				else if (ctrl == 2) {
					ctrlMat[0] = 0;
				}
			} catch (Exception e) {
				Preferences.debug("function error: " + e.getMessage() + "\n",
						Preferences.DEBUG_ALGORITHM);
			}

			return;
    		
    	}
    }
	
			
	/**
     * complex divide c = a/b.
     * 
     * @param ar double
     * @param ai double
     * @param br double
     * @param bi double
     * @param cr double[]
     * @param ci double[]
     */
    private void zdiv(final double ar, final double ai, final double br, final double bi, final double[] cr,
            final double[] ci) {
        double bm, cc, cd, ca, cb;

        bm = 1.0 / zabs(br, bi);
        cc = br * bm;
        cd = bi * bm;
        ca = ( (ar * cc) + (ai * cd)) * bm;
        cb = ( (ai * cc) - (ar * cd)) * bm;
        cr[0] = ca;
        ci[0] = cb;

        return;
    }
    
    /**
     * zabs computes the absolute value or magnitude of a double precision complex variable zr + j*zi.
     * 
     * @param zr double
     * @param zi double
     * 
     * @return double
     */
    private double zabs(final double zr, final double zi) {
        double u, v, q, s;
        u = Math.abs(zr);
        v = Math.abs(zi);
        s = u + v;

        // s * 1.0 makes an unnormalized underflow on CDC machines into a true
        // floating zero
        s = s * 1.0;

        if (s == 0.0) {
            return 0.0;
        } else if (u > v) {
            q = v / u;

            return (u * Math.sqrt(1.0 + (q * q)));
        } else {
            q = u / v;

            return (v * Math.sqrt(1.0 + (q * q)));
        }
    }
    
    /**
     * complex multiply c = a * b.
     * 
     * @param ar double
     * @param ai double
     * @param br double
     * @param bi double
     * @param cr double[]
     * @param ci double[]
     */
    private void zmlt(final double ar, final double ai, final double br, final double bi, final double[] cr,
            final double[] ci) {
        double ca, cb;

        ca = (ar * br) - (ai * bi);
        cb = (ar * bi) + (ai * br);
        cr[0] = ca;
        ci[0] = cb;

        return;
    }
	
	// Original MATLAB stquadh routine copyright 1998 by Tody Driscoll.
	// stquad applies the "1/2 rule" by assuming that the distance from the integration interval to the nearest
	// singularity is equal to the distance from the left endpoint to the nearest singularity.  This is 
	// certainly true e.g. if one begins integration at the nearest singularity to the target point.  However,
	// it may be violated in important circumstances, such as when one integrates from the next-nearest
	// singularity (because the nearest maps to infinity), or when one is integrating between adjacent prevertices
	// (as in the param problem).  The main difficulty is the possibility of singularities from the "other side"
	// of the strip intruding.
	
	// Here we assume that the integration intervals are horizontal.  This function recursively subdivides the
	// interval until the "1/2 rule" is satisfied for singularities "between" the endpoints.  Actually, we use
	// a more stringent "alpha rule", for alpha > 1/2, as this seems to be necessary sometimes.
	
	// There must be no singularities *inside* the interval, of course.
	private double[][] stquadh(double z1[][], double z2[][], int sing1[], double z[][], double beta[], double qdat[][]) {
		int i, j, k, m;
		double za[][] = new double[1][2];
		double zb[][] = new double[1][2];
		int sng[] = new int[1];
		double alf;
		double alfSquared;
		double oneMinusAlfSquared;
		double d;
		int n = z.length;
		double dx[] = new double[n];
		double dy[] = new double[n];
		boolean toright[] = new boolean[n];
		boolean active[] = new boolean[n];
		int numactive;
		double x;
		double y;
		double L[];
		double Lmin;
		double zmid[][] = new double[1][2];
		double result[][] = new double[1][2];
		int zero[] = new int[1];
		double I[][] = null;
		
		if (sing1 == null) {
			sing1 = new int[z1.length];
			for (i = 0; i < z1.length; i++) {
				sing1[i] = -1;
			}
		}
		I = new double[z1.length][2];
		int numdiff = 0;
		for (i = 0; i < z1.length; i++) {
			if ((z1[i][0] != z2[i][0]) || (z1[i][1] != z2[i][1])) {
				numdiff++;
			}
		}
		int nontriv[] = new int[numdiff];
		for (i = 0, j = 0; i < z1.length; i++) {
			if ((z1[i][0] != z2[i][0]) || (z1[i][1] != z2[i][1])) {
			    nontriv[j++] = i;
			}
		}
		 // alf == 1/2 means the "1/2 rule."  Better to be more strict.
	    alf = 0.75;
	    alfSquared = alf * alf;
	    oneMinusAlfSquared = 1.0 - alfSquared;
		for (i = 0; i < nontriv.length; i++) {
		    k = nontriv[i];
		    za[0][0] = z1[k][0];
		    za[0][1] = z1[k][1];
		    zb[0][0] = z2[k][0];
		    zb[0][1] = z2[k][1];
		    sng[0] = sing1[k];
		   
		    // Given integration length
		    d = zb[0][0] - za[0][0];
		 // Compute horizontal position (signed) and vertical distance (positive) from the singularities to
		    // the left endpoint.  If we are going from right to left, reverse the sense of horizontal.
		    for (j = 0; j < n; j++) {
		        dx[j] = (z[j][0] - za[0][0]) * sign(d);
		        dy[j] = Math.abs(z[j][1] - za[0][1]);
		    }
		    
		    // We have to be concerned with singularities lying to the right (left if d < 0) of the left
		    // integration endpoint.
		    for (j = 0; j < n; j++) {
		    	if ((dx[j] > 0.0) && (!Double.isInfinite(z[j][0])) && (!Double.isInfinite(z[j][1])) ) {
		    	    toright[j] = true;	
		    	}
		    	else {
		    		toright[j] = false;
		    	}
		    } // for (j = 0; j < n; j++)
		    // For points with small enough dx, the limitation is purely due to dy.
		    // For others, it must be calculated.
		    for (j = 0; j < n; j++) {
		    	if ((dx[j] > dy[j]/alf) && toright[j]) {
		    		active[j] = true;
		    	}
		    	else {
		    		active[j] = false;
		    	}
		    } // for (j = 0; j < n; j++)
		    // Make sure the left endpoint won't be included.
		    if (sng[0] >= 0) {
		    	active[sng[0]] = false;
		    }
		    numactive = 0;
		    for (j = 0; j < active.length; j++) {
		    	if (active[j]) {
		    		numactive++;
		    	}
		    } //  for (j = 0; j < active.length; j++)
		    
		    // For those active, find the integration length constraint.
		    // This comes from making the sing/right-endpoint distance equal to alf*L.
		    L = new double[numactive];
		    for (j = 0, m = 0; j < n; j++) {
		    	if (active[j]) {
		    	    x = dx[j];
		    	    y = dy[j];
		    	    L[m++] = (x - Math.sqrt(alfSquared*x*x - oneMinusAlfSquared*y*y))/oneMinusAlfSquared;
		    	}
		    } // for (j = 0, m = 0; j < n; j++)
		    
		    // What is the maximum allowable integration length?
		    Lmin = Double.MAX_VALUE;
		    for (j = 0 ; j < L.length; j++) {
		        if (L[j] < Lmin) {
		        	Lmin = L[j];
		        }
		    } // for (j = 0 ; j < L.length; j++)
		    for (j = 0; j < n; j++) {
		    	if (toright[j] && (!active[j]) && (dy[j]/alf < Lmin)) {
		    		Lmin = dy[j]/alf;
		    	}
		    } // for (j = 0; j < n; j++)
		    
		    if (Lmin < Math.abs(d)) {
		    	// Apply stquad on the safe part and recurse on the rest
			    zmid[0][0] = za[0][0] + Lmin*sign(d);
			    zmid[0][1] = za[0][1];
			    result = stquad(za, zmid,sng,z,beta,qdat);
			    I[k][0] = result[0][0];
			    I[k][1] = result[0][1];
			    result = stquadh(zmid, zb, zero, z, beta, qdat);
			    I[k][0] = I[k][0] + result[0][0];
			    I[k][1] = I[k][1] + result[0][1];
			} // if (Lmin < Math.abs(d))
		    else {
		        // No restriction
		    	result = stquad(za, zb, sng, z, beta, qdat);
		    	I[k][0] = result[0][0];
		    	I[k][1] = result[0][1];
		    } // else
		} // for (i = 0; i < nontriv.length; i++)
		
		return I;
	}
	
	// Numerical quadrature for the strip map.
	// Original MATLAB routine copyright 1998 by Toby Driscoll
	// z1,z2 are vectors of left and right endpoints.  sing1 is a vector of integer indices which label the
	// singularities in z1.  So if sing1[5] = 3, then z1[5] = z[3].  -1 means no singularity.  z is the vector
	// of *all* singularities, including the "ends" of the strip at +-infinity.  beta is the vector of associated
	// turning angles.  qdat is the quadrature data from scqdata.  It should include all the beta values, even
	// though the ends are never used in this manner.
	
	// Make sure z and beta are column vectors.
	
	// stquad integrates from a possible singularity at the left end to a regular point on the right.  If both
	// endpoints are singularities, you must break the integral into two pieces and make two calls.
	
	// The integral is subdivided, if necessary, so that no singularity lies closer to the left endpoint than
	// 1/2 the length of the integration (sub)interval.
	private double[][] stquad(double z1[][], double z2[][], int sing1[], double z[][], double beta[], double qdat[][]) {
		double I[][] = null;
		int i, j, k;
		double za[] = new double[2];
		double zb[] = new double[2];
		int sng;
		double dist;
		double zreal;
		double zimag;
		double zabs;
		double num;
		double denom;
		double result;
		double zr[] = new double[2];
		int ind;
		double nd[][] = new double[qdat.length][2];
		double wt[][] = new double[qdat.length][2];
		boolean anydiffzero;
		double scale;
		double zl[] = new double[2];
		double cr[] = new double[1];
		double ci[] = new double[1];
		int n = z.length;
		
		if (sing1 == null) {
			sing1 = new int[z1.length];
			for (i = 0; i < z1.length; i++) {
				sing1[i] = -1;
			}
		} // if (sing1 == null)
		
		I = new double[z1.length][2];
		int numdiff = 0;
		for (i = 0; i < z1.length; i++) {
			if ((z1[i][0] != z2[i][0]) || (z1[i][1] != z2[i][1])) {
				numdiff++;
			}
		}
		int nontriv[] = new int[numdiff];
		for (i = 0, j = 0; i < z1.length; i++) {
			if ((z1[i][0] != z2[i][0]) || (z1[i][1] != z2[i][1])) {
			    nontriv[j++] = i;
			}
		}
		
		for (i = 0; i < nontriv.length; i++) {
		    k = nontriv[i];
		    za[0] = z1[k][0];
		    za[1] = z1[k][1];
		    zb[0] = z2[k][0];
		    zb[1] = z2[k][1];
		    sng = sing1[k];
		    
		    // Allowable integration step, based on nearest singularity
		    dist = 1.00;
		    for (j = 0; j <= sng-1; j++) {
		    	zreal = z[j][0] - za[0];
		    	zimag = z[j][1] - za[1];
		    	num = 2.0*zabs(zreal,zimag);
		    	zreal = zb[0] - za[0];
			    zimag = zb[1] - za[1];
			    denom = zabs(zreal,zimag);
			    result = num/denom;
		    	if (result < dist) {
		    		dist = result;
		    	}
		    } // for (j = 0; j <= sng-1; j++)
		    for (j = sng+1; j <= n-1; j++) {
		    	zreal = z[j][0] - za[0];
		    	zimag = z[j][1] - za[1];
		    	num = 2.0*zabs(zreal,zimag);
		    	zreal = zb[0] - za[0];
			    zimag = zb[1] - za[1];
			    denom = zabs(zreal,zimag);
			    result = num/denom;
		    	if (result < dist) {
		    		dist = result;
		    	}	
		    } // for (j = sng+1; j <= n-1; j++)
		    zr[0] = za[0] + dist*(zb[0] - za[0]);
		    zr[1] = za[1] + dist*(zb[1] - za[1]);
		    ind = ((sng+n+1) % (n+1));
		    // Adjust Gauss_jacobi nodes and weights to interval.
		    
		    for (j = 0; j < qdat.length; j++) {
		        nd[j][0] = ((zr[0]-za[0])*qdat[j][ind] + zr[0] + za[0])/2.0; // G-J nodes
		        nd[j][1] = ((zr[1]-za[1])*qdat[j][ind] + zr[1] + za[1])/2.0;
		        wt[j][0] = ((zr[0]-za[0])/2) * qdat[j][ind+n+1]; // G-J weights
		        wt[j][1] = ((zr[1]-za[1])/2) * qdat[j][ind+n+1];
		    } // for (j = 0; j < qdat.length; j++)
		    anydiffzero = false;
		    if ((nd[0][0] == za[0]) && (nd[0][1] == za[1])) {
		    	anydiffzero = true;
		    }
		    else if ((zr[0] == nd[nd.length-1][0]) && (zr[1] == nd[nd.length-1][1])) {
		    	anydiffzero = true;
		    }
		    else {
		    	for (j = 0; j < nd.length-1 && (!anydiffzero); j++) {
		    		if ((nd[j][0] == nd[j+1][0]) && (nd[j][1] == nd[j+1][1])) {
		    			anydiffzero = true;
		    		}
		    	}
		    } // else
		    if (anydiffzero) {
		    	// Endpoints are practically coincident.
		    	I[k][0] = 0;
		    	I[k][1] = 0;
		    }
		    else {
		    	// Use Gauss_Jacobi on first subinterval, if necessary.
		    	if (sng >= 0) {
		    		zreal = zr[0] - za[0];
	    			zimag = zr[1] - za[1];
	    			zabs = zabs(zreal,zimag)/2.0;
	                scale = Math.pow(zabs, beta[sng]);
		    		for (j = 0; j < wt.length; j++) {
		    			wt[j][0] = wt[j][0] * scale;
		    			wt[j][1] = wt[j][1] * scale;
		    		}
		    	} // if (sng >= 0)
		    	double out[][] = stderiv(nd, z, beta, 1, sng);
		    	I[k][0] = 0;
		    	I[k][1] = 0;
		    	for (j = 0; j < wt.length; j++) {
		    		zmlt(out[j][0], out[j][1], wt[j][0], wt[j][1], cr, ci);
		    	    I[k][0] += cr[0];
		    	    I[k][1] += ci[0];
		    	}
		    	while ((dist < 1)&& (!Double.isNaN(I[k][0])) && (!Double.isNaN(I[k][1]))) {
		    	    // Do regular Gaussian quad on other subintervals.
		    		zl[0] = zr[0];
		    		zl[1] = zr[1];
		    		dist = 1;
		    		for (j = 0; j < n; j++) {
		    			zreal = z[j][0] - zl[0];
		    			zimag = z[j][1] - zl[1];
		    			num = 2.0*zabs(zreal,zimag);
		    			zreal = zl[0] - zb[0];
			    		zimag = zl[1] - zb[1];
			    		denom = zabs(zreal,zimag);
			    		result = num/denom;
		    			if (result < dist) {
		    				dist = result;
		    			}
		    		} // for (j = 0; j < n; j++)
		    		
		    		zr[0] = zl[0] + dist*(zb[0]-zl[0]);
		    		zr[1] = zl[1] + dist*(zb[1]-zl[1]);
		    		for (j = 0; j < qdat.length; j++) {
		    			nd[j][0] = ((zr[0]-zl[0]) * qdat[j][n] + zr[0] + zl[0])/2.0;
		    			nd[j][1] = ((zr[1]-zl[1]) * qdat[j][n] + zr[1] + zl[1])/2.0;
		    			wt[j][0] = ((zr[0]-zl[0])/2.0) * qdat[j][2*n+1];
		    			wt[j][1] = ((zr[1]-zl[1])/2.0) * qdat[j][2*n+1];
		    		} // for (j = 0; j < qdat.length; j++)
		    		out = stderiv(nd,z,beta,1,-1);
	                for (j = 0; j < wt.length; j++) {
	                	zmlt(out[j][0], out[j][1], wt[j][0], wt[j][1], cr, ci);
	                	I[k][0] += cr[0];
	                	I[k][1] += ci[0];
	                }
		    	} // while ((dist < 1)&& (!Double.isNaN(I[k][0])) && (!Double.isNaN(I[k][1])))
		    } // else
		} // for (i = 0; i < nontriv.length; i++)
		return I;
	}
	
	// Derivative of the strip map
	// stderiv returns the derivative at the points of zp of the Schwarz-Christoffel
	// strip map defined by z, beta, and c.
	// See also stparam, stmap.
	// Original MATLAB routine copyright 1998 by Toby Driscoll.
	// If j >= 0, the terms corresponding to z[j] are normalized by abs(zp - z[j]).
	// This is for Gauss_jacobi quadrature.
	private double[][] stderiv(double zp[][], double z[][], double beta[], int c, int j) {
		int i, k, m;
	    double fprime[][] =  null;
	    double log2 = 0.69314718055994531;
	    fprime = new double[zp.length][2];
	    double zprow[][] = new double[zp.length][2];
	    double theta[] = null;
	    
	    for (i = 0; i < zp.length; i++) {
	        zprow[i][0] = zp[i][0];
	        zprow[i][1] = zp[i][1];
	    }
	    int npts = zprow.length;
	    // z and beta are passed call by value. Make sure they are not changed
	    double zcopy[][] = new double[z.length][2];
	    double betacopy[] = new double[beta.length];
	    double z2[][];
	    double beta2[];
	    for (i = 0; i < z.length; i++) {
	    	zcopy[i][0] = z[i][0];
	    	zcopy[i][1] = z[i][1];
	    }
	    for (i = 0; i < beta.length; i++) {
	    	betacopy[i] = beta[i];
	    }
	    // Strip out infinite prevertices
	    if (zcopy.length == betacopy.length) {
	        int numinfz = 0;
	        int numfinitez = 0;
	        for (i = 0; i < zcopy.length; i++) {
	        	if (Double.isInfinite(zcopy[i][0]) || Double.isInfinite(zcopy[i][1])) {
	        		numinfz++;
	        	}
	        	else {
	        		numfinitez++;
	        	}
	        } // for (i = 0; i < zcopy.length; i++)
	        int ends[] = new int[numinfz];
	        int notends[] = new int[numfinitez];
	        for (i = 0, k = 0, m = 0; i < zcopy.length; i++) {
	        	if (Double.isInfinite(zcopy[i][0]) || Double.isInfinite(zcopy[i][1])) {
	        	    ends[k++] = i;
	        	}
	        	else {
	        		notends[m++] = i;
	        	}
	        } // for (i = 0, k = 0, m = 0; i < zcopy.length; i++)
	        theta = new double[ends.length-1];
	        for (i = 0; i < ends.length-1; i++) {
	        	theta[i] = beta[ends[i+1]] - beta[ends[i]];
	        }
	        if (zcopy[ends[0]][0] < 0) {
	            for (i = 0; i < theta.length; i++) {
	            	theta[i] = -theta[i];
	            }
	        } // if (zcopy[ends[0]][0] < 0)
	        z2 = new double[numfinitez][2];
	        beta2 = new double[numfinitez];
	        for (i = 0; i < numfinitez; i++) {
	        	z2[i][0] = zcopy[notends[i]][0];
	        	z2[i][1] = zcopy[notends[i]][1];
	        	beta2[i] = betacopy[notends[i]];
	        }
	        for (i = 0; i < zcopy.length; i++) {
	        	zcopy[i] = null;
	        }
	        zcopy = null;
	        betacopy = null;
	        // Adjust singularity index if given
	        if (j >= 0) {
	        	if ((numinfz >= 3) && (j > ends[2])) {
	        		j = j-3;
	        	}
	        	else if (j > ends[1]) {
	        		j = j-2;
	        	}
	        	else if (j > ends[0]) {
	        		j--;
	        	}
	        } // if (j >= 0)
	    } // if (zcopy.length == betacopy.length)
	    else {
	    	MipavUtil.displayError("Vector of preveretices must include +/- Inf entries");
	    	return null;
	    }
	    double zcol[][] = new double[z2.length][2];
	    for (i = 0; i < zcol.length; i++) {
	    	zcol[i][0] = z2[i][0];
	    	zcol[i][1] = z2[i][1];
	    }
	    double bcol[] = new double[beta2.length];
	    for (i = 0; i < bcol.length; i++) {
	    	bcol[i] = beta2[i];
	    }
	    int n = z2.length;
	    
	    double terms[][][] = new double[n][npts][2];
	    for (i = 0; i < n; i++) {
	    	for (k = 0; k < npts; k++) {
	    		terms[i][k][0] = (-Math.PI/2.0) * (zprow[k][0] - zcol[i][0]);
	    		terms[i][k][1] = (-Math.PI/2.0) * (zprow[k][1] - zcol[i][1]);
	    	}
	    } // for (i = 0; i < n; i++)
	    for (i = 0; i < n; i++) {
	    	if (z2[i][1] == 0) {
	    		for (k = 0; k < npts; k++) {
	    			terms[i][k][0] = -terms[i][k][0];
	    			terms[i][k][1] = -terms[i][k][1];
	    		}
	    	}
	    } // for (i = 0; i < n; i++)
	    double rt[][] = new double[n][npts];
	    for (i = 0; i < n; i++) {
	    	for (k = 0; k < npts; k++) {
	    		rt[i][k] = terms[i][k][0];
	    	}
	    } // for (i = 0; i < n; i++)
	    boolean big[][] = new boolean[n][npts];
	    for (i = 0; i < n; i++) {
	    	for (k = 0; k < npts; k++) {
	    		if (Math.abs(rt[i][k]) > 40) {
	    			big[i][k] = true;
	    		}
	    	}
	    } // for (i = 0; i < n; i++)
    	// sinh(x + iy) = (sinhx*cosy) + i*(coshx*siny)
    	// logz = log|z| + i*theta
    	for (i = 0; i < n; i++) {
    		for (k = 0; k < npts; k++) {
    			if (!big[i][k]) {
    				double realsinh = Math.sinh(terms[i][k][0])*Math.cos(terms[i][k][1]);
    				double imagsinh = Math.cosh(terms[i][k][0])*Math.sin(terms[i][k][1]);
    				terms[i][k][0] = Math.log(zabs(realsinh,imagsinh));
    				terms[i][k][1] = Math.atan2(-realsinh, imagsinh);
    			}
    			else {
    			    terms[i][k][0] = sign(rt[i][k]) * (terms[i][k][0]) - log2;
    			    terms[i][k][1] = sign(rt[i][k]) * (terms[i][k][1] - Math.PI/2.0);
    			}
    		}
    	} // for (i = 0; i < n; i++)
    	if (j >= 0) {
    		for (k = 0; k < npts; k++) {
    			double zreal = zprow[k][0] - z2[j][0];
    			double zimag = zprow[k][1] - z2[j][1];
    			double zabs = zabs(zreal,zimag);
    			terms[j][k][0] = terms[j][k][0] - Math.log(zabs);
    		}
    	} // if (j >= 0)
    	double mult[][][] = new double[n][npts][2];
    	for (i = 0; i < n; i++) {
    		for (k = 0; k < npts; k++) {
    		    mult[i][k][0] = terms[i][k][0]*bcol[i];
    		    mult[i][k][1] = terms[i][k][1]*bcol[i];
    		}
    	}
    	double sum[][] = new double[npts][2];
    	for (k = 0; k < npts; k++) {
    		for (i = 0; i < n; i++) {
    			sum[k][0] += mult[i][k][0];
    			sum[k][1] += mult[i][k][1];
    		}
    	} // for (k = 0; k < npts; k++)
    	double sum2[][] = new double[npts][2];
    	for (i = 0; i < npts; i++) {
    		sum2[i][0] = (Math.PI/2.0)*theta[0]*zprow[i][0] + sum[i][0];
    		sum2[i][1] = (Math.PI/2.0)*theta[0]*zprow[i][1] + sum[i][1];
    	}
    	for (i = 0; i < npts; i++) {
    		fprime[i][0] = c*Math.exp(sum2[i][0])*Math.cos(sum2[i][1]);
    		fprime[i][1] = c*Math.exp(sum2[i][0])*Math.sin(sum2[i][1]);
    	}
	    return fprime;
	}
	
	private double sign(double d) {
		if (d > 0.0) {
			return 1.0;
		}
		else if (d == 0.0) {
			return 0.0;
		}
		else {
			return -1.0;
		}
	}
	
	private double[] sign(double dReal, double dImag) {
		// x/abs(x) for complex numbers
		double realResult[] = new double[1];
		double imagResult[] = new double[1];
		double absd = zabs(dReal, dImag);
		zdiv(dReal, dImag, absd, 0, realResult, imagResult);
		double result[] = new double[]{realResult[0], imagResult[0]};
		return result;
	}
	
	private void rptrnsfm(double z[][], double y[], int cnr[]) {
		// rptrnsfm not intended for calling directly by the user
		// Transformation optimization vars to prevertices for rectangle problem
		// Original MATLAB code copyright 1997 by Toby Driscoll.  Last updated 05/06/97
		int i, j;
		int n = y.length+3;
		//double z[][] = new double[n][2];
		for (i = 0; i < z.length; i++) {
			for (j = 0; j < 2; j++) {
				z[i][j] = 0.0;
			}
		}
		
		// Fill interior of long edges first
		double cumsum = 0.0;
		for (i = cnr[0]; i <= cnr[1] - 2; i++) {
		    cumsum += Math.exp(y[i]);
		    z[i+1][0] = cumsum;
		    z[i+1][1] = 0;
		}
		cumsum = 0.0;
		for (i = cnr[3]-3; i >= cnr[2]-1; i--) {
			cumsum += Math.exp(y[i]);
		    z[i+2][0] = cumsum;
		    z[i+2][1] = 1.0;	
		}
		
		// Find L
		double xr[] = new double[2];
		xr[0] = z[cnr[1]-1][0];
		xr[1] = z[cnr[2]+1][0];
		double meanxr = (xr[0] + xr[1])/2.0;
		double diff = (xr[1] - xr[0])/2.0;
		double diffSquared = diff * diff;
		double yexp = Math.exp(2*y[cnr[1]-1]);
		double mag = diffSquared + yexp;
		double sqrtmag = Math.sqrt(mag);
		z[cnr[1]][0] = meanxr + sqrtmag;
		z[cnr[1]][1] = 0.0;
		z[cnr[2]][0] = z[cnr[1]][0];
		z[cnr[2]][1] = 1 + z[cnr[1]][1];
		z[cnr[3]][0] = 0.0;
		z[cnr[3]][1] = 1.0;
		
		// Now fill in "short edges"
		double cp[] = new double[Math.max(0, 1 + cnr[2] - 2 - cnr[1]) + 1];
		cp[0] = 1;
		for (i = 1; i < cnr[2] - cnr[1]; i++) {
			cp[i] = cp[i-1] * Math.exp(-(y[cnr[1] + i-1]));
		}
		double flipudcp[] = new double[cp.length];
		for (i = 0; i < cp.length; i++) {
			flipudcp[i] = cp[cp.length-1-i];
		}
		double cumsumflip[] = new double[cp.length];
		cumsumflip[0] = flipudcp[0];
		for (i = 1; i < cp.length; i++) {
			cumsumflip[i] = cumsumflip[i-1] + flipudcp[i];
		}
		double flipcumsum[] = new double[cp.length];
		for (i = 0; i < cp.length; i++) {
			flipcumsum[i] = cumsumflip[cp.length-i-1];
		}
		double cumsumcp[] = new double[cp.length];
		cumsumcp[0] = cp[0];
		for (i = 1; i < cp.length; i++) {
			cumsumcp[i] = cumsumcp[i-1] + cp[i];
		}
		double x[] = new double[cp.length+1];
		x[0] = -flipcumsum[0];
		for (i = 1; i < cp.length; i++) {
			x[i] = cumsumcp[i-1] - flipcumsum[i];
		}
		x[cp.length] = cumsumcp[cp.length-1];
		double x2[] = new double[x.length-2];
		for (i = 0; i < x2.length; i++) {
			x2[i] = x[i+1]/x[cp.length];
		}
		boolean mask[] = new boolean[x2.length];
		for (i = 0; i < x2.length; i++) {
			if (Math.abs(x2[i]) < eps) {
				mask[i] = true;
			}
		}
		double u[][] = new double[x2.length][2];
		for (i = 0; i < x2.length; i++) {
			u[i][0] = x2[i];
			u[i][1] = 0;
		}
		for (i = 0; i < x2.length; i++) {
			if (!mask[i]) {
				u[i][0] = Math.log(Math.abs(x2[i]))/Math.PI;
				u[i][1] = Math.atan2(0, x2[i])/Math.PI;
			}
			else {
				u[i][0] = -z[cnr[1]][0]/eps;
				u[i][1] = -z[cnr[1]][1]/eps;
			}
		} // for (i = 0; i < x2.length; i++)
		for (i = cnr[1]+1; i <= cnr[2]-1; i++) {
			z[i][0] = z[cnr[1]][0] - u[i-(cnr[1]+1)][0];
			z[i][1] = u[i-(cnr[1]+1)][1];
		}
				 
		cp = new double[Math.max(0, 1 + n-4-(cnr[3]-2))+1+(cnr[0]-1)+1];
		cp[0] = 1;
		for (i = 1; i <= n - cnr[3] - 1; i++) {
			cp[i] = cp[i-1]*Math.exp(-y[i+cnr[3]-3]);
		}
		for (i = n - cnr[3]; i < n -cnr[3] + cnr[0]; i++) {
			cp[i] = cp[i-1]*Math.exp(-y[i-n+cnr[3]]);
		}
		flipudcp = new double[cp.length];
		for (i = 0; i < cp.length; i++) {
			flipudcp[i] = cp[cp.length-1-i];
		}
		cumsumflip = new double[cp.length];
		cumsumflip[0] = flipudcp[0];
		for (i = 1; i < cp.length; i++) {
			cumsumflip[i] = cumsumflip[i-1] + flipudcp[i];
		}
		flipcumsum = new double[cp.length];
		for (i = 0; i < cp.length; i++) {
			flipcumsum[i] = cumsumflip[cp.length-i-1];
		}
		cumsumcp = new double[cp.length];
		cumsumcp[0] = cp[0];
		for (i = 1; i < cp.length; i++) {
			cumsumcp[i] = cumsumcp[i-1] + cp[i];
		}
		x = new double[cp.length+1];
		x[0] = -flipcumsum[0];
		for (i = 1; i < cp.length; i++) {
			x[i] = cumsumcp[i-1] - flipcumsum[i];
		}
		x[cp.length] = cumsumcp[cp.length-1];
		x2 = new double[x.length-2];
		for (i = 0; i < x2.length; i++) {
			x2[i] = x[i+1]/x[cp.length];
		}
		mask = new boolean[x2.length];
		for (i = 0; i < x2.length; i++) {
			if (Math.abs(x2[i]) < eps) {
				mask[i] = true;
			}
		}
		u = new double[x2.length][2];
		for (i = 0; i < x2.length; i++) {
			u[i][0] = x2[i];
			u[i][1] = 0;
		}
		for (i = 0; i < x2.length; i++) {
			if (!mask[i]) {
				u[i][0] = Math.log(Math.abs(x2[i]))/Math.PI;
				u[i][1] = Math.atan2(0, x2[i])/Math.PI;
			}
			else {
				u[i][0] = -z[cnr[1]][0]/eps;
				u[i][1] = -z[cnr[1]][1]/eps;
			}
		} // for (i = 0; i < x2.length; i++)
		for (i = 0; i < n-cnr[3]-1; i++) {
			z[i+cnr[3]+1][0] = u[i][0];
			z[i+cnr[3]+1][1] = u[i][1];
		}
		for (i = n-cnr[3]; i < n - cnr[3] + cnr[0]; i++) {
			z[i-n+cnr[3]][0] = u[i-1][0];
			z[i-n+cnr[3]][1] = u[i-1][1];
		}
		
		return;
	}
	
	// Gauss-Jacobi quadrature data for SC Toolbox.
	// scqdata returns a matrix of quadrature data suitable for other SC routines.  
	// beta is a vector of turning angles corresponding to *finite* singularities (prevertices and,
	// for exterior map, the origin).
	// nqpts is the number of quadrature points per subinterval, roughly equal to -log10(error).
	
	// All the SC routines call this routine as needed, and the work required is small, so you will
	// probably never have to call this routine directly.
	// Original MATLAB code copyright 1998 by Toby Driscoll.
	private void scqdata(double qdat[][], double beta[], int nqpts) {
		int i, j;
	    int n = beta.length;
	    double qnode[][] = new double[nqpts][n+1];
	    double qwght[][] = new double[nqpts][n+1];
	    double z[] = new double[nqpts];
	    double w[] = new double[nqpts];
	    for (j = 0; j < n; j++) {
	    	if (beta[j] > -1.0) {
	    	    gaussj(z, w, nqpts, 0, beta[j]);
	    	    for (i = 0; i < nqpts; i++) {
	    	    	qnode[i][j] = z[i];
	    	    	qwght[i][j] = w[i];
	    	    }
	    	}
	    } // for (j = 0; j < n; j++)
	    gaussj(z, w, nqpts, 0, 0);
	    for (i = 0; i < nqpts; i++) {
	    	qnode[i][n] = z[i];
	    	qwght[i][n] = w[i];
	    }
	    //qdat = new double[nqpts][2*n+2];
	    for (i = 0; i < nqpts; i++) {
	    	for (j = 0; j < n+1; j++) {
	    		qdat[i][j] = qnode[i][j];
	    		qdat[i][j+n+1] = qwght[i][j];
	    	}
	    }
	    return;
	}
	
	// gaussj returns nodes and weights for Gauss-Jacobi integration.  z and w are n-vectors such that the integral from
	// x = -1 to x = +1 of f(x)*((1-x)^alf)*((1+x)^bet)dx
	// is approximated by sum(f(z) .* w).
	// Original MATLAB code copyright 1997 by Toby Driscoll and last updated 04/11/97.
	// Uses the Lanczos iteration connection to orthogonal polynomials.
	// Borrows heavily from gaussj out of scpack FORTRAN.
	// Calculate coeffs a, b of Lanczos recurrence relation (closed form is known).
	// Break out n = 1 specially to avoid possible divide by zero.
	private void gaussj(double z[], double w[], int n,  double alf, double bet) {
		int i;
		double a[] = new double[n];
		double b[] = new double[n-1];
	    double apb = alf + bet;
	    a[0] = (bet-alf)/(apb+2);
	    double var = apb+2.0;
	    b[0] = Math.sqrt(4.0*(1.0+alf)*(1.0+bet)/ ((apb+3.0)*var*var));
	    for (i = 2; i <= n; i++) {
	    	a[i-1] = apb * (bet-alf) /((apb+2*i)*(apb+2*i-2));
	    }
	    for (i=2; i <= n-1; i++) {
	    	var = apb+2*i;
	    	var = var * var;
	    	b[i-1] = Math.sqrt(4.0*i*(i+alf)*(i+bet)*(i+apb)/((var-1)*var));
	    }
	    // Find eigvals/eigvecs of tridiag "Ritz" matrix
	    double eigenvector[][] = new double[n][n];
        double eigenvalue[] = new double[n];
	    if (n > 1) {
	        double sum[][] = new double[n][n];
	        for (i = 0; i < n; i++) {
	        	sum[i][i] = a[i];
	        }
	        for (i = 0; i < n-1; i++) {
	        	sum[i][i+1] = b[i];
	        	sum[i+1][i] = b[i];
	        }
	        // MATLAB puts eigenvalues in increasing order.
	        // Eigenvalue.decompose also puts eigenvalues in increasing order.
	        Eigenvalue.decompose(sum, eigenvector, eigenvalue);
	    } // if (n > 1)
	    else {
	    	eigenvector[0][0] = 1;
	    	eigenvalue[0] = a[0];
	    }
	    
	    // Compute normalization (integral of w(x))
	    double result1[] = new double[1];
	    Gamma gamma1 = new Gamma(alf+1, result1);
	    gamma1.run();
	    double result2[] = new double[1];
	    Gamma gamma2 = new Gamma(bet+1, result2);
	    gamma2.run();
	    double result3[] = new double[1];
	    Gamma gamma3 = new Gamma(apb+2, result3);
	    gamma3.run();
	    double c = Math.pow(2.0, apb+1.0)*result1[0]*result2[0]/result3[0];
	    
	    // Return the values
	    double prew[] = new double[n];
	    for (i = 0; i < n; i++) {
	    	prew[i] = c * eigenvector[0][i] * eigenvector[0][i];
	    }
	    ArrayList<indexValueItem> valueList = new ArrayList<indexValueItem>();
	    for (i = 0; i < n; i++) {
	    	valueList.add(new indexValueItem(i, eigenvalue[i]));
	    }
	    Collections.sort(valueList, new indexValueComparator());
	    for (i = 0; i < n; i++) {
	    	z[i] = eigenvalue[valueList.get(i).getIndex()];
	    	w[i] = prew[valueList.get(i).getIndex()];
	    }
	}
	
	// sccheck checks polygon inputs to the Schwarz-Christoffel functions.
	// sccheck is used by the xxparam functions to check the validity of 
	// inputs describing the polygon to be mapped.  type is a string consisting
	// of the prefix to param ("d", "dp", etc.).  If errors are found, execution 
	// will terminate.  Sometimes the trouble has to do with how the parameter
	// problem is posed, which imposes a few nonobvious constraints.  The function
	// scfix is provided to automatically fix such difficulties, by renumbering or
	// perhaps adding vertices.  sccheck output is 1 if the problem is rectifiable
	// by scfix, 2 if warning status only.
	// Original MATLAB code copyright 1998 by Toby Driscoll.
	private int sccheck(String type, double w[][], double beta[], int aux[]) {
		int i;
	    int err;
	    int sumb;
	    double sumbeta;
	    int n = w.length;
	    boolean atinf[] = new boolean[beta.length];
	    for (i = 0; i < beta.length; i++) {
	    	if (beta[i] <= -1.0) {
	    		atinf[i] = true;
	    	}
	    }
	    err = 0;
	    
	    // Universal truths
	    if (beta.length != n) {
	    	MipavUtil.displayError("Mismatched angles and vertices");
	    	err = -1;
	    	return err;
	    }
	    for (i = 0; i < n; i++) {
	    	if ((beta[i] > 1) || (beta[i] <-3)) {
	    		MipavUtil.displayError("beta["+i+"] = " + beta[i] + " not in the required [-3,1] range");
	    		err = -1;
	    		return err;
	    	}
	    } // for (i = 0; i < n; i++)
	    
	    // Infinite vertices
	    if ((!type.equalsIgnoreCase("de")) && (!type.equalsIgnoreCase("cr"))) {
	        for (i = 0; i < n; i++) {
	        	if ((!atinf[i]) && ((Double.isInfinite(w[i][0])) || (Double.isInfinite(w[i][1])))) {
	        	    MipavUtil.displayError("Infinite vertex " + i + " illegally has an angle > -1");
	        	    err = -1;
	        	    return err;
	        	}
	            if (atinf[i] && (!Double.isInfinite(w[i][0])) && (!Double.isInfinite(w[i][1]))) {
	        	    MipavUtil.displayError("Finite vertex " + i + " illegally has an angle <= -1");	
	        	    err = -1;
	        	    return err;
	        	}
	        } // for (i = 0; i < n; i++)
	        for (i = 0; i < n-1; i++) {
	        	if (atinf[i] && atinf[i+1]) {
	        		MipavUtil.displayError(i + " and " + (i+1) + " are unallowed adjacent infinite vertices");
	        		err = -1;
	        		return err;
	        	}
	        } // for (i = 0; i < n-1; i++)
	    } // if ((!type.equalsIgnoreCase("de")) && (!type.equalsIgnoreCase("cr")))
	    else {
	    	for (i = 0; i < n; i++) {
	    		if (atinf[i]) {
	    			MipavUtil.displayError("vertex " + i + " illegally has a beta <= -1");
	    			err = -1;
	    			return err;
	    		}
	    		if (Double.isInfinite(w[i][0]) || Double.isInfinite(w[i][1])) {
	    			MipavUtil.displayError(i + " is an illegal infinite vertex");
	    			err = -1;
	    			return err;
	    		}
	    	} // for (i = 0; i < n; i++)
	    } // else
	    
	    if (type.equalsIgnoreCase("de")) {
	    	sumb = 2;
	    }
	    else {
	    	sumb = -2;
	    }
	    
	    // Orientation conventions
	    sumbeta = 0.0;
	    for (i = 0; i < n; i++) {
	    	sumbeta += beta[i];
	    }
	    if (Math.abs(sumbeta + sumb) < 1.0E-9) {
	    	MipavUtil.displayError("Vertices were probably given in the wrong order");
	    	err = 1;
	    	return err;
	    }
	    else if (Math.abs(sumbeta - sumb) > 1.0E-9) {
	    	MipavUtil.displayWarning("Angles do not sum to " + sumb);
	    	err = 2;
	    	return err;
	    }
	    
	    // Some finer points
	    if (type.equalsIgnoreCase("hp") || type.equalsIgnoreCase("d")) {
	        if (n < 3) {
	            MipavUtil.displayError("Polygon must have at least 3 vertices");
	            err = -1;
	            return err;
	        }
	        else if (Double.isInfinite(w[0][0]) || Double.isInfinite(w[0][1]) || Double.isInfinite(w[1][0]) ||
	        		Double.isInfinite(w[1][1]) || Double.isInfinite(w[n-2][0]) || Double.isInfinite(w[n-2][1])) {
	        	MipavUtil.displayError("Infinite vertices must not be at positions 0, 1, or n-2");
	        	err = 1;
	        	return err;
	        }
	        else if ((Math.abs(beta[n-1]) < eps) || (Math.abs(beta[n-1] - 1) < eps)) {
	        	MipavUtil.displayError("Sides adjacent to w[n-1] must not be collinear");
	        	err = 1;
	        	return err;
	        }
	    } // if (type.equalsIgnoreCase("hp") || type.equalsIgnoreCase("d"))
	    else if (type.equalsIgnoreCase("cr")) {
	        if (n < 4) {
	        	MipavUtil.displayError("Polygon must have at least 4 vertices");
	        	err = -1;
	        	return err;
	        }
	    } // else if (type.equalsIgnoreCase("cr"))
	    else if (type.equalsIgnoreCase("de")) {
	        if (n < 2) {
	        	MipavUtil.displayError("polygon must have at least 2 vertices");
	        	err = -1;
	        	return err;
	        }
	        else if (((beta[n-1] == 0) || (beta[n-1] == 1)) && (n > 2)) {
	        	MipavUtil.displayError("Sides adjacent to w[n-1] must not be collinear");
	        	err = 1;
	        	return err;
	        }
	    } // else if (type.equalsIgnoreCase("de"))
	    else if (type.equalsIgnoreCase("st")) {
	        if (n < 5) {
	        	MipavUtil.displayError("Polygon must have at least 5 vertices");
	        	err = -1;
	        	return err;
	        }
	        double wrenum[][] = new double[n][2];
	        double betarenum[] = new double[n];
	        for (i = aux[0]; i < n; i++) {
	        	wrenum[i - aux[0]][0] = w[i][0];
	        	wrenum[i - aux[0]][1] = w[i][1];
	        	betarenum[i - aux[0]] = beta[i];
	        }
	        for (i = 0; i <= aux[0]-1; i++) {
	        	wrenum[n - aux[0] + i][0] = w[i][0];
	        	wrenum[n - aux[0] + i][1] = w[i][1];
	        	betarenum[n- aux[0] + i] = beta[i];
	        }
	        int renum[] = new int[n];
	        for (i = aux[0]; i < n; i++) {
	        	renum[i-aux[0]] = i;
	        }
	        for (i = 0; i <= aux[0] - 1; i++) {
	        	renum[n - aux[0] + i] = i;
	        }
	        int k = -1;
	        for (i = 0; i < n; i++) {
	        	if (renum[i] == aux[1]) {
	        		k = i + 1;
	        	}
	        } // for (i = 0; i < n; i++)
	        renum = null;
	        if (atinf[1] || atinf[2] || atinf[n-1]) {
	        	MipavUtil.displayError("Vertices at (w[aux[0] + [1, 2, -1]) must be finite");
	        	err = 1;
	        	return err;
	        }
	        else if (k - 2 < 2) {
	        	MipavUtil.displayError("There must be at least 2 vertices between ends 0 and 1");
	        	err = 1;
	        	return err;
	        }
	        else if (k == n) {
	        	MipavUtil.displayError("There must be at least one vertex between ends 1 and 0");
	        	err = 1;
	        	return err;
	        }
	    } // else if (type.equalsIgnoreCase("st"))
	    else if (type.equalsIgnoreCase("r")) {
	        int corner[] = new int[aux.length];	
	        for (i = 0; i < aux.length; i++) {
	        	corner[i] = aux[i];
	        }
	        double wrenum[][] = new double[n][2];
	        double betarenum[] = new double[n];
	        for (i = aux[0]; i < n; i++) {
	        	wrenum[i - aux[0]][0] = w[i][0];
	        	wrenum[i - aux[0]][1] = w[i][1];
	        	betarenum[i - aux[0]] = beta[i];
	        }
	        for (i = 0; i <= aux[0]-1; i++) {
	        	wrenum[n - aux[0] + i][0] = w[i][0];
	        	wrenum[n - aux[0] + i][1] = w[i][1];
	        	betarenum[n- aux[0] + i] = beta[i];
	        }
	        int offset = n - corner[0]; // corner - corner[0] so no need to add one
	        int corner2[] = new int[corner.length];
		    for (i = 0; i < corner.length; i++) {
		    	corner2[i] = ((corner[i] + offset) % n);
		    }
		    if (n < 4) {
		    	MipavUtil.displayError("Polygon must have at least 4 vertices");
		    	err = -1;
		    	return err;
		    }
		    int sortcorner[] = new int[corner2.length];
		    for (i = 0; i < corner2.length; i++) {
		    	sortcorner[i] = corner2[i];
		    }
		    Arrays.sort(sortcorner);
		    for (i = 0; i < corner2.length; i++) {
		    	if (corner2[i] != sortcorner[i]) {
		    		MipavUtil.displayError("Corners must be specified in ccw order");
		    		err = -1;
		    		return err;
		    	}
		    } // for (i = 0; i < corner.length; i++)
		    if (Double.isInfinite(wrenum[0][0]) || Double.isInfinite(wrenum[0][1])) {
		    	MipavUtil.displayError("Corner[0] must be finite");
		    	err = -1;
		    	return err;
		    }
		    if (Double.isInfinite(wrenum[1][0]) || Double.isInfinite(wrenum[1][1])) {
		        MipavUtil.displayError("Vertex corner[0] + 1 must be finite");
		        err = 1;
		        return err;
		    }
		    if ((Math.abs(betarenum[n-1]) < eps) || (Math.abs(betarenum[n-1] - 1) < eps)) {
	        	MipavUtil.displayError("Sides adjacent to w[corner[0]-1] must not be collinear");
	        	err = 1;
	        	return err;
	        }
	    } // else if (type.equalsIgnoreCase("r"))
	    return err;
	}
	
	private void scfix(double wn[][], double betan[], int verticesAdded[], int auxn[], String type, double w[][], double beta[], int aux[]) {
		// wn,betan, and auxn are output
		// w, beta, and type are input
		// aux is input
		// Fix polygon to meet Schwarz-Christoffel toolbox constraints.
		// scfix attempts to fix a problem in the given polygon that arises from the
		// posing of the parameter problem.  scfix is used when a call to xxparam results
		// in an error and so advises.  In this case the polygon as given violates some
		// fairly arbitrary constraint.  scifx remedies the situation by renumbering the
		// vertices, or, if necessary, adding a trivial (zero-turn) vertex.  type is one
		// of {"hp", "d", "de", "st", "r"}.  if an additional aux input and output argument
		// is given, it represents the indices of the strip ends or the rectangle corners.
		
		// Original MATLAB routine copyright 1998 by Toby Driscoll
		
		// You may wonder, why not let the xxparam functions call scfix automatically?  The 
		// trouble with that approach is that since a function can't modify its inputs in
		// the calling workspace, either the xxparam functions would have to return more
		// arguments, or the mapping and plotting functions also would have to detect and 
		// correct the problem every time they're called.  The problem is rare enough that this
		// method seems adequate.
		int i, j, k;
		int n = w.length;
		int renum[] = new int[n];
		for (i = 0; i < n; i++) {
			renum[i] = i;
		}
		int renumtemp[] = new int[n];
		
		// Orientation conventions
		int sumb;
		if (type.equalsIgnoreCase("de")) {
			sumb = 2;
		}
		else {
			sumb = -2;
		}
		double sumbeta = 0.0;
		for (i = 0; i < beta.length; i++) {
			sumbeta += beta[i];
		}
		if (Math.abs(sumbeta + sumb) < 1.0E-9) {
			// reverse order
			wn[0][0] = w[0][0];
			wn[0][1] = w[0][1];
			for (i = 1; i <= n-1; i++) {
				wn[i][0] = w[n-i][0];
				wn[i][1] = w[n-1][1];
			}
			for (i = 0; i < n; i++) {
				w[i][0] = wn[i][0];
				w[i][1] = wn[i][1];
			}
			
			beta = scangle(w);
			renumtemp[0] = renum[0];
			for (i = 1; i <= n-1; i++) {
				renumtemp[i] = renum[n-i];
			}
			for (i = 0; i < n; i++) {
				renum[i] = renumtemp[i];
			}
			if (aux != null) {
				int auxtemp[] = new int[n];
				auxtemp[0] = aux[0];
				for (i = 1; i <= n-1; i++) {
					auxtemp[i] = aux[n-i];
				}
				for (i = 0; i < n; i++) {
					aux[i] = auxtemp[i];
				}
				auxtemp = null;
			}
		} // if (Math.abs(sumbeta + sumb) < 1.0E-9)
		
		// Less obvious restrictions
		if (type.equalsIgnoreCase("hp") || type.equalsIgnoreCase("d")) {
		    int shift[] = new int[n];
		    for (i = 0; i < n-1; i++) {
		    	shift[i] = i+1;
		    }
		    shift[n-1] = 0;
		    // Remember, if necessary, to meet requirements:
		    // w([0,1,n-2]) finite && sides at w[n-1] not collinear
		    while (true) {
		    	boolean anyisinf = false;
		    	if (Double.isInfinite(w[0][0]) || Double.isInfinite(w[0][1]) || Double.isInfinite(w[1][0]) ||
		    		Double.isInfinite(w[1][1]) || Double.isInfinite(w[n-2][0]) || Double.isInfinite(w[n-2][1])) {
		    		anyisinf = true;
		    	}
		    	if (!anyisinf && (Math.abs(beta[n-1]) >= eps) && (Math.abs(beta[n-1] - 1) >= eps)) {
		    		break;
		    	}
		        for (i = 0; i < n-1; i++) {
		        	renumtemp[i] = renum[i+1];
		        	wn[i][0] = w[i+1][0];
		        	wn[i][1] = w[i+1][1];
		        	betan[i] = beta[i+1];
		        }
		        renumtemp[n-1] = renum[0];
		        wn[n-1][0] = w[0][0];
		        wn[n-1][1] = w[0][1];
		        betan[n-1] = beta[0];
		        for (i = 0; i < n; i++) {
		        	renum[i] = renumtemp[i];
		        	w[i][0] = wn[i][0];
		        	w[i][1] = wn[i][1];
		        	beta[i] = betan[i];
		        }
		        if (renum[0] == 0) { // tried all orderings
		        	// First be sure beta[n-1] is no longer a problem.
		        	boolean betaproblem = true;
		        	for (i = 0; i < beta.length && betaproblem; i++) {
		        		if ((Math.abs(beta[i]-1.0) >= eps) && (Math.abs(beta[i]) >= eps)) {
		        			betaproblem = false;
		        		}
		        	} // for (i = 0; i < beta.length && betaproblem; i++)
		        	if (betaproblem) {
		        		System.err.println("Polygon has empty interior");
		        		return;
		        	}
		        	while ((Math.abs(beta[n-1]) < eps) || (Math.abs(beta[n-1] - 1) < eps)) {
		        		for (i = 0; i < n-1; i++) {
				        	wn[i][0] = w[i+1][0];
				        	wn[i][1] = w[i+1][1];
				        	betan[i] = beta[i+1];
				        }
				        wn[n-1][0] = w[0][0];
				        wn[n-1][1] = w[0][1];
				        betan[n-1] = beta[0];
				        for (i = 0; i < n; i++) {
				        	w[i][0] = wn[i][0];
				        	w[i][1] = wn[i][1];
				        	beta[i] = betan[i];
				        }	
		        	} // while ((Math.abs(beta[n-1]) < eps) || (Math.abs(beta[n-1] - 1) < eps))
		        	// Next, add one or two vertices as needed.
		        	if (Double.isInfinite(w[0][0]) || Double.isInfinite(w[0][1]) || Double.isInfinite(w[1][0]) ||
		        			Double.isInfinite(w[1][1])) { 
		        	    scaddvtx(wn,betan,w,beta,0,null);
		        	    verticesAdded[0] = verticesAdded[0]+1;
		        	    w = new double[wn.length][2];
		        	    beta = new double[wn.length];
		        	    n = n+1;
		        	    for (i = 0; i < n; i++) {
		        	    	w[i][0] = wn[i][0];
		        	    	w[i][1] = wn[i][1];
		        	    	beta[i] = betan[i];
		        	    }
		        	} // if (Double.isInfinite(w[0][0]) || Double.isInfinite(w[0][1]) || Double.isInfinite(w[1][0]) ||
		        	if (Double.isInfinite(w[n-2][0]) || Double.isInfinite(w[n-2][1])) {
		        		scaddvtx(wn,betan,w,beta,n-2,null);
		        		verticesAdded[0] = verticesAdded[0]+1;
		        		w = new double[wn.length][2];
			        	beta = new double[wn.length];
		        		n=n+1;
		        		for (i = 0; i < n; i++) {
		        	    	w[i][0] = wn[i][0];
		        	    	w[i][1] = wn[i][1];
		        	    	beta[i] = betan[i];
		        	    }
		        	} // if (Double.isInfinite(w[n-2][0]) || Double.isInfinite(w[n-2][1]))
		        	renum = new int[n];
		        	for (i = 0; i < n; i++) {
		        		renum[i] = i;
		        	}
		        	System.out.println("Warning: A vertex has been added");
		        	break;
		        } // if (renum[0] == 0)
		    } // while (true)
		} // if (type.equalsIgnoreCase("hp") || type.equalsIgnoreCase("d"))
		else if (type.equalsIgnoreCase("de")) {
			int shift[] = new int[n];
		    for (i = 0; i < n-1; i++) {
		    	shift[i] = i+1;
		    }
		    shift[n-1] = 0;
		    // Remember, if necessary, to ensure sides at w[n-1] not collinear
		    // (except if n==2, which is to be handled explicitly anyway)
		    while (true) {
		    	if (((Math.abs(beta[n-1]) >= eps) && (Math.abs(beta[n-1] - 1) >= eps)) || (n <= 2)) {
		    		break;
		    	}
		    	for (i = 0; i < n-1; i++) {
		        	renumtemp[i] = renum[i+1];
		        	wn[i][0] = w[i+1][0];
		        	wn[i][1] = w[i+1][1];
		        	betan[i] = beta[i+1];
		        }
		        renumtemp[n-1] = renum[0];
		        wn[n-1][0] = w[0][0];
		        wn[n-1][1] = w[0][1];
		        betan[n-1] = beta[0];
		        for (i = 0; i < n; i++) {
		        	renum[i] = renumtemp[i];
		        	w[i][0] = wn[i][0];
		        	w[i][1] = wn[i][1];
		        	beta[i] = betan[i];
		        }
		        if (renum[0] == 0) {
		        	wn = new double[2][2];
		        	betan = new double[2];
		        	for (i = 0, j = 0; i < beta.length; i++) {
		        		if (Math.abs(beta[i]) >= eps) {
		        		    wn[j][0] = w[i][0];
		        		    wn[j][1] = w[i][1];
		        		    betan[j++] = beta[i];
		        		}
		        	}
        		    renum = new int[2];
        		    renum[0] = 0;
        		    renum[1] = 1;
        		    n = 2;
        		    System.out.println("Polygon is a line segment); removing superfluous vertices");
        		    break;
		        } // if (renum[0] == 0)
		    } // while (true)
		} // else if (type.equalsIgnoreCase("de"))
		else if (type.equalsIgnoreCase("st")) {
			int ends[] = null;
			if ((aux != null) && (aux.length > 0)) {
			    ends = new int[aux.length];	
			    for (i = 0; i < aux.length; i++) {
			    	ends[i] = aux[i];
			    }
			} // if ((aux != null) && (aux.length > 0))
			else {
			    MipavUtil.displayInfo("Use mouse to select images of left and right ends of the strip.");
			    ends = scselect(w, beta, 2, null, null);
			}
			for (i = ends[0]; i < n; i++) {
				renum[i-ends[0]] = i;
				wn[i-ends[0]][0] = w[i][0];
				wn[i-ends[0]][1] = w[i][1];
				betan[i-ends[0]] = beta[i];
			}
			for (i = 0; i <= ends[0]-1; i++) {
				renum[n-ends[0]+i] = i;
				wn[n-ends[0]+i][0] = w[i][0];
				wn[n-ends[0]+i][1] = w[i][1];
				betan[n-ends[0]+i] = beta[i];
			}
			for (i = 0; i < n; i++) {
				w[i][0] = wn[i][0];
				w[i][1] = wn[i][1];
				beta[i] = betan[i];
			}
			for (k = 0; k < renum.length; k++) {
			    if (renum[k] == ends[1]) {
			    	break;
			    }
			} // for (k = 0; k < renum.length; k++)
			if (k < 3) {
				if (k < n-2) {
				    // Switch ends
					for (i = k; i < n; i++) {
						renum[i-k] = i;
						wn[i-k][0] = w[i][0];
						wn[i-k][1] = w[i][1];
						betan[i-k] = beta[i];
					}
					for (i = 0; i <= k-1; i++) {
						renum[n-k+i] = i;
						wn[n-k+i][0] = w[i][0];
						wn[n-k+i][1] = w[i][1];
						betan[n-k+i] = beta[i];
					}
					for (i = 0; i < n; i++) {
						w[i][0] = wn[i][0];
						w[i][1] = wn[i][1];
						beta[i] = betan[i];
					}
					for (k = 0; k < renum.length; k++) {
					    if (renum[k] == 0) {
					    	break;
					    }
					} // for (k = 0; k < renum.length; k++)
				} // if (k < n-2)
				else {
				    // Add one or two vertices.
					for (j = 1; j <= 4-(k+1); j++) {
					    scaddvtx(wn, betan, w, beta, j-1, null);
					    verticesAdded[0] = verticesAdded[0]+1;
					    w = new double[wn.length][2];
					    beta = new double[wn.length];
					    n = n+1;
					    for (i = 0; i < n; i++) {
					    	w[i][0] = wn[i][0];
					    	w[i][1] = wn[i][1];
					    	beta[i] = betan[i];
					    }
					    k = k+1;
					    System.out.println("Warning: A vertex has been added.");
					} // for (j = 1; j <= 4-(k+1); j++)
				} // else
			} // if (k < 3)
			
			if (k == n-1) {
				// Must add a vertex in any case
				 scaddvtx(wn, betan, w, beta, n-1, null);
				 verticesAdded[0] = verticesAdded[0]+1;
				 w = new double[wn.length][2];
			     beta = new double[wn.length];
			     n = n+1;
			     for (i = 0; i < n; i++) {
			    	 w[i][0] = wn[i][0];
			    	 w[i][1] = wn[i][1];
			    	 beta[i] = betan[i];
			     }
			     System.out.println("Warning: A vertex has been added.");
			} // if (k == n-1)
			
			if (Double.isInfinite(w[1][0]) || Double.isInfinite(w[1][1])) {
			    // Add two vertices
				for (j = 0; j <= 1; j++) {
					 scaddvtx(wn, betan, w, beta, j, null);
					 verticesAdded[0] = verticesAdded[0]+1;
					 w = new double[wn.length][2];
				     beta = new double[wn.length];
				     n = n+1;
				     k = k+1;
				     for (i = 0; i < n; i++) {
				    	 w[i][0] = wn[i][0];
				    	 w[i][1] = wn[i][1];
				    	 beta[i] = betan[i];
				     }
				     System.out.println("Warning: A vertex has been added.");	
				}
			} // if (Double.isInfinite(w[1][0]) || Double.isInfinite(w[1][1]))
			else if (Double.isInfinite(w[2][0]) || Double.isInfinite(w[2][1])) {
			    // Add one vertex.
				 scaddvtx(wn, betan, w, beta, 1, null);
				 verticesAdded[0] = verticesAdded[0]+1;
				 w = new double[wn.length][2];
			     beta = new double[wn.length];
			     n = n+1;
			     k = k+1;
			     for (i = 0; i < n; i++) {
			    	 w[i][0] = wn[i][0];
			    	 w[i][1] = wn[i][1];
			    	 beta[i] = betan[i];
			     }
			     System.out.println("Warning: A vertex has been added.");	
			} // else if (Double.isInfinite(w[2][0]) || Double.isInfinite(w[2][1]))
			else if (Double.isInfinite(w[n-1][0]) || Double.isInfinite(w[n-1][1])) {
			    // Add one vertex.
				 scaddvtx(wn, betan, w, beta, n-1, null);
				 verticesAdded[0] = verticesAdded[0]+1;
				 w = new double[wn.length][2];
			     beta = new double[wn.length];
			     n = n+1;
			     for (i = 0; i < n; i++) {
			    	 w[i][0] = wn[i][0];
			    	 w[i][1] = wn[i][1];
			    	 beta[i] = betan[i];
			     }
			     System.out.println("Warning: A vertex has been added.");	
			} // else if (Double.isInfinite(w[2][0]) || Double.isInfinite(w[2][1]))
			
			for (i = 0; i <= k; i++) {
				auxn[i] = i;
			}
		} // else if (type.equalsIgnoreCase("st"))
		else if (type.equalsIgnoreCase("r")) {
		    int corner[] = new int[aux.length];
		    for (i = 0; i < aux.length; i++) {
		    	corner[i] = aux[i];
		    }
		    for (i = corner[0]; i < n; i++) {
				renum[i-corner[0]] = i;
				wn[i-corner[0]][0] = w[i][0];
				wn[i-corner[0]][1] = w[i][1];
				betan[i-corner[0]] = beta[i];
			}
			for (i = 0; i <= corner[0]-1; i++) {
				renum[n-corner[0]+i] = i;
				wn[n-corner[0]+i][0] = w[i][0];
				wn[n-corner[0]+i][1] = w[i][1];
				betan[n-corner[0]+i] = beta[i];
			}
			for (i = 0; i < n; i++) {
				w[i][0] = wn[i][0];
				w[i][1] = wn[i][1];
				beta[i] = betan[i];
			}
			int cornertemp[] = new int[corner.length];
			for (i = 0; i < corner.length; i++) {
				cornertemp[i] = (corner[i]-corner[0]+n) % n;
			} 
			for (i = 0; i < corner.length; i++) {
				corner[i] = cornertemp[i];
			}
			// Note: These problems are pretty rare.
			if ((Math.abs(beta[n-1]) <eps) || (Math.abs(beta[n-1]-1.0) < eps)) {
			    // Try swapping sides 1-2 and 3-4.
				if ((Math.abs(beta[corner[2]-1]) >= eps) && (Math.abs(beta[corner[2]-1]-1) >= eps) && (!Double.isInfinite(w[corner[2]][0]))
						&& (!Double.isInfinite(w[corner[2]][1]))) {
					for (i = corner[2]; i < n; i++) {
						renum[i-corner[2]] = i;
						wn[i-corner[2]][0] = w[i][0];
						wn[i-corner[2]][1] = w[i][1];
						betan[i-corner[2]] = beta[i];
					}
					for (i = 0; i <= corner[2]-1; i++) {
						renum[n-corner[2]+i] = i;
						wn[n-corner[2]+i][0] = w[i][0];
						wn[n-corner[2]+i][1] = w[i][1];
						betan[n-corner[2]+i] = beta[i];
					}
					for (i = 0; i < n; i++) {
						w[i][0] = wn[i][0];
						w[i][1] = wn[i][1];
						beta[i] = betan[i];
					}
					for (i = 0; i < corner.length; i++) {
						cornertemp[i] = (corner[i]-corner[2]+n) % n;
					} 
					Arrays.sort(cornertemp);
					for (i = 0; i < corner.length; i++) {
						corner[i] = cornertemp[i];
					}
				}
				else {
					System.err.println("Collinear sides make posing problem impossible");
					return;
				}
			} // if ((Math.abs(beta[n-1]) <eps) || (Math.abs(beta[n-1]-1.0) < eps))
			if (Double.isInfinite(w[1][0]) || Double.isInfinite(w[1][1])) {
				scaddvtx(wn, betan, w, beta, 0, null);
				verticesAdded[0] = verticesAdded[0]+1;
			    n = n+1;
			    for (i = 1; i <= 3; i++) {
			    	corner[i] = corner[i] + 1;
			    }
			} // if (Double.isInfinite(w[1][0]) || Double.isInfinite(w[1][1]))
			
			for (i = 0; i < corner.length; i++) {
				auxn[i] = corner[i];
			}
		} // else if (type.equalsIgnoreCase("r"))
	}
	
	private void scaddvtx(double wn[][], double betan[], double w[][], double beta[], int pos, double window[]) {
		// wn and betan are outputs of length n+1
		// w, beta, pos, and window are inputs. w and beta are of length n
		// Add a vertex to a polygon
		// scaddvtx adds a new vertex to the polygon described by w and beta immediately
		// after vertex pos.  If w[pos:pos+1] are finite, the new vertex is at the midpoint of an
		// edge; otherwise, the new vertex is a reasonable distance from its finite neighbor
		// Original MATLAB routine copyright 1998 by Toby Driscoll.
		double newv[] = new double[2];
		//int numadj;
		int i, j;
		int base = -1;
		boolean found;
		int rbase;
		double wreal;
		double wimag;
		int jrem;
		if (window == null) {
			window = new double[]{Double.NEGATIVE_INFINITY, Double.POSITIVE_INFINITY, Double.NEGATIVE_INFINITY,
					Double.POSITIVE_INFINITY};
		}
		int n = w.length;
		double ang[] = new double[n];
		double ang2[] = new double[n];
		if (pos < 0) {
			pos = n-1;
		}
		int pos1 = (pos+1)%n;
		if (isFinite(w[pos][0]) && isFinite(w[pos][1]) && isFinite(w[pos1][0]) &&
				isFinite(w[pos1][1])) {
			// Easy case
		    newv[0] = (w[pos][0] + w[pos1][0])/2.0;	
		    newv[1] = (w[pos][1] + w[pos1][1])/2.0;
		}
		else {
			// Messy case
			// Find a pair of adjacent finite vertices as a basis.
			//numadj = 0;
			found = false;
			for (i = 0; i < n-1 && (!found); i++) {
				if (isFinite(w[i][0]) && isFinite(w[i][1]) && isFinite(w[i+1][0]) &&
						isFinite(w[i+1][1])) {	
				    found = true;
				    base = i;
				}
			}
			if (!found) {
				if (isFinite(w[n-1][0]) && isFinite(w[n-1][1]) && isFinite(w[0][0]) &&
						isFinite(w[0][1])) {	
				    found = true;
				    base = n-1;
				}
			} // if (!found)
			rbase = (base+1) % n;
			wreal = w[rbase][0] - w[base][0];
			wimag = w[rbase][1] - w[base][1];
			ang[base] = Math.atan2(wimag, wreal);
			
			// Determine the absolute angle of side pos->pos1.
			for (j = 0; j < n; j++) {
				ang2[j] = ang[j];
			}
			for (j = base+1; j < n; j++) {
			    jrem =( j-1 + n) % n;
			    ang[j] = ang2[jrem] - Math.PI * beta[j];
			    if (j == pos) {
			    	break;
			    }
			} // for (j = base+1; j < n; j++)
			
			// Find a nice side length
			double len[] = new double[n];
			for (i = 0; i < n-1; i++) {
				len[i] = zabs(w[i+1][0] - w[i][0], w[i+1][1] - w[i][1]);
			}
			len[n-1] = zabs(w[0][0] - w[n-1][0], w[0][1] - w[n-1][1]);
			double sum = 0.0;
			int numfinite = 0;
			for (i = 0; i < n; i++) {
				if (isFinite(len[i])) {
					numfinite++;
					sum += len[i];
				}
			} // for (i = 0; i < n; i++)
			double avglen = sum/numfinite;
			
			double wbase[] = new double[2];
			double dir[] = new double[2];
			if (Double.isInfinite(w[pos][0]) || Double.isInfinite(w[pos][1])) {
			    wbase[0] = w[pos1][0];
			    wbase[1] = w[pos1][1];
			    dir[0] = Math.cos(ang[pos] + Math.PI);
			    dir[1] = Math.sin(ang[pos] + Math.PI);
			}
			else {
				wbase[0] = w[pos][0];
				wbase[1] = w[pos][1];
				dir[0] = Math.cos(ang[pos]);
				dir[1] = Math.sin(ang[pos]);
			}
			
			// Restrict point to a window (to help out graphics)
			newv[0] = wbase[0] + avglen*dir[0];
			newv[1] = wbase[1] + avglen*dir[1];
			while ((newv[0] < window[0]) || (newv[0] > window[1]) || (newv[1] < window[2]) || (newv[1] > window[3])) {
				avglen = avglen/2;
				newv[0] = wbase[0] + avglen*dir[0];
				newv[1] = wbase[1] + avglen*dir[1];
			}
		} // else
		
		for (i = 0; i <= pos; i++) {
			wn[i][0] = w[i][0];
			wn[i][1] = w[i][1];
			betan[i] = beta[i];
		}
		wn[pos+1][0] = newv[0];
		wn[pos+1][1] = newv[1];
		betan[pos+1] = 0;
		for (i = pos+1; i <= n-1; i++) {
			wn[i+1][0] = w[i][0];
			wn[i+1][1] = w[i][1];
			betan[i+1] = beta[i];
		}
	}
	
	private class indexValueComparator implements Comparator<indexValueItem> {

        /**
         * DOCUMENT ME!
         * 
         * @param o1 DOCUMENT ME!
         * @param o2 DOCUMENT ME!
         * 
         * @return DOCUMENT ME!
         */
        public int compare(final indexValueItem o1, final indexValueItem o2) {
            final double a = o1.getValue();
            final double b = o2.getValue();
            final int c = o1.getIndex();
            final int d = o2.getIndex();

            if (a < b) {
                return -1;
            } else if (a > b) {
                return 1;
            } else if (c < d) {
            	return -1;
            }
            else if (c > d) {
            	return 1;
            }
            else {
            	return 0;
            }
        }

    }
	
	private class indexValueItem {
		private int index;
		private double value;
		
		public indexValueItem(int index, double value) {
			this.value = value;
			this.index = index;
		}
		
		public int getIndex() {
			return index;
		}
		
		public double getValue() {
			return value;
		}
	}
	
	private static final boolean isFinite(double d) {
	    return (!Double.isInfinite(d) && !Double.isNaN(d));
	}
	
}