package gov.nih.mipav.model.algorithms;

import java.awt.Color;
import java.awt.Graphics;
import java.awt.Rectangle;
import java.awt.event.MouseEvent;
import java.awt.event.MouseListener;
import java.io.IOException;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.Collections;
import java.util.Comparator;
import java.util.Vector;
import java.util.concurrent.locks.Condition;
import java.util.concurrent.locks.Lock;
import java.util.concurrent.locks.ReentrantLock;

import de.jtem.numericalMethods.algebra.linear.decompose.Eigenvalue;
import gov.nih.mipav.model.algorithms.AlgorithmFRAP.FitWholeNL2solInt2;
import gov.nih.mipav.model.algorithms.SchwarzChristoffelMapping.ODEModel;
import gov.nih.mipav.model.algorithms.SchwarzChristoffelMapping.dpfun;
import gov.nih.mipav.model.algorithms.SchwarzChristoffelMapping.polygon;
import gov.nih.mipav.model.algorithms.SchwarzChristoffelMapping.qlgraph;
import gov.nih.mipav.model.algorithms.SchwarzChristoffelMapping.scmap;
import gov.nih.mipav.model.structures.ModelImage;
import gov.nih.mipav.model.structures.jama.ComplexLinearEquations;
import gov.nih.mipav.model.structures.jama.GeneralizedEigenvalue;
import gov.nih.mipav.model.structures.jama.LinearEquations2;
import gov.nih.mipav.util.DoubleDouble;
import gov.nih.mipav.view.MipavUtil;
import gov.nih.mipav.view.Preferences;
import gov.nih.mipav.view.ViewJComponentBase;
import gov.nih.mipav.view.ViewJComponentGraph;
import gov.nih.mipav.view.ViewJFrameGraph;

public class SchwarzChristoffelMapping2 extends AlgorithmBase {

	// This is a port of portions of the Schwarz-Christoffel Toolbox from MATLAB
	// to Java
	// with the kind permission of Professor Toby Driscoll. The original code
	// is:
	// Version 2.3 January 15, 2003
	// Copyright (c) 1994-2003 by Toby Driscoll (driscoll@math.udel.edu).

	// How much progress information to show during and after the solution to
	// the parameter problem.
	// private boolean traceSolution = false;

	// Desired accuracy in the map. This may not be met exactly.
	private double tolerance = 1.0E-8;

	// eps returns the distance from 1.0 to the next larger double-precision
	// number, that is, eps = 2^-52.
	private double eps;

	private double w[][];

	private boolean testRoutine = true;

	private SchwarzChristoffelMapping scm = new SchwarzChristoffelMapping();

	public SchwarzChristoffelMapping2() {

	}

	public SchwarzChristoffelMapping2(ModelImage destImg, ModelImage srcImg, double w[][]) {
		super(destImg, srcImg);
		this.w = w;
	}

	public void runAlgorithm() {
		// eps returns the distance from 1.0 to the next larger double-precision
		// number, that is, eps = 2^-52.
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
		scm.setEps(eps);

		if (testRoutine) {
            //testHplmap1();
			//testHplmap2();
			//testHplmap3();
			testStripmap1();
			//testStripmap2();
			return;
		}

	}
	
	private void testHplmap1() {
		// Extra line is due to plotpoly going from +infinity on one side to
		// -infinity or the other side or more specifically going from:
		// xPointArray[jext] = (float)(wrenum[j-1][0] + R*Math.cos(ang));
    	// yPointArray[jext] = (float)(wrenum[j-1][1] + R*Math.sin(ang));
		// to:
		// xPointArray[jext+1] = (float)(wrenum[(j+1)%n][0] - R*Math.cos(ang));
    	// yPointArray[jext+1] = (float)(wrenum[(j+1)%n][1] - R*Math.sin(ang));
		int i;
		double re[];
		double im[];
		double x[] = new double[]{0.0,0.0,Double.POSITIVE_INFINITY};
		double y[] = new double[]{1.0, -1.0, 0.0};
		double alpha[] = new double[]{1.5, 0.5, -1.0};
		polygon p = scm.new polygon(x, y, alpha);
		scmap f = hplmap(p, null, null, null);
        re = new double[17];
		for (i = 0; i < 17; i++) {
			re[i] = (i-10)*0.7;
		}
		im = new double[12];
		for (i = 0; i < 12; i++) {
			im[i] = (i+1)*0.7;
		}
		double axis[] = new double[]{-3, 3, -1.5, 4.5};
		hplot(f, re, im, Integer.MIN_VALUE, false, axis);
	}
	
	private void testHplmap2() {
		// Map accuracies and ODE integrations are okay.
        // Accuracy = 4.7671978322256E-10
        // Accuracy = 1.4031304414909808E-9
        // In ODE normal return.  Integration reached tout
        // In ODE normal return.  Integration reached tout
        // In ODE normal return.  Integration reached tout
        // In ODE normal return.  Integration reached tout

		// In any event passing the evalinv points to f1 in hplmap does not seem to make sense.
		int i, j;
		double x[] = new double[]{0, 0, 2, 2};
		double y[] = new double[]{1, 0, 0, 1};
		polygon p = scm.new polygon(x, y, null);
		scmap f1 = hplmap(p, null, null, null);
		double theta[] = new double[]{0.5, 1.0/6.0, 0.5, 1.0/6.0};
		double thetas[] = new double[]{1.0/6.0, 0.5, 1.0/6.0, 0.5};
		// mod(a, m) = a - m*floor(a/m)
		double diff[] = new double[4];
		for (i = 0; i < 4; i++) {
			diff[i] = theta[i] - thetas[i];
		}
		double alf[] = new double[4];
		for (i = 0; i < 4; i++) {
			alf[i] = Math.floor(diff[i]) - diff[i] + 1;
		}
		double z[][] = f1.prevertex;
		double c[] = new double[]{0.0,1.0};
		scmap f2 = hplmap(null, z, alf, c);
		polygon Q = f2.poly;
		double w[][] = Q.vertex;
		double m = Double.MAX_VALUE;
		for (i = 0; i < w.length; i++) {
			if (w[i][1] < m) {
				m = w[i][1];
			}
		}
		double M = -Double.MAX_VALUE;
		for (i = 0; i < w.length; i++) {
			if (w[i][1] > M) {
				M = w[i][1];
			}
		}
		double a = -Double.MAX_VALUE;
		for (i = 0; i < w.length; i++) {
			if (w[i][0] > a) {
				a = w[i][0];
			}
		}
		double t[] = new double[101];
		t[0] = .01/100.0;
		for (i = 1; i <= 99; i++) {
			t[i] = (double)i/100.0;
		}
		t[100] = 99.99/100.0;
		double wt[][] = new double[4][2];
		for (i = 0; i < 4; i++) {
			wt[i][0] = x[i];
			wt[i][1] = y[i];
		}
		float xPointArray[] = new float[wt.length+1];
		float yPointArray[] = new float[wt.length+1];
		double beta[] = new double[]{-0.5, -0.5, -0.5, -0.5};
		double axlim[] = new double[4];
		// Minimum line segment length, as a proportion of the axes box
        double minlen = 0.005;
        // Maximum line segment length, as a proportion of the axes box
        double maxlen = 0.02;
		ViewJFrameGraph pointGraph = scm.plotpoly(xPointArray, yPointArray, wt, beta, false, axlim, Integer.MIN_VALUE, true, null);
		ViewJComponentGraph graph = pointGraph.getGraph();
		Rectangle graphBounds = graph.getGraphBounds();
		Graphics g = graph.getGraphics();
		double xScale = graphBounds.width / (axlim[1] - axlim[0]);
        double yScale = graphBounds.height / (axlim[3] - axlim[2]);
		double len = Math.max(axlim[1] - axlim[0], axlim[3] - axlim[2]);
		minlen = len * minlen;
		maxlen = len * maxlen;
		
		double X[][] = new double[101][15];
		double Y[][] = new double[101][15];
		double wp[][] = new double[101*15][2];
		double zp[][] = new double[101*15][2];
		for (i = 0; i < 101; i++) {
			for (j = 0; j < 15; j++) {
			    X[i][j] = (j+1)*a/16.0;
			    Y[i][j] = m*t[i];
			    wp[15*i + j][0] = X[i][j];
			    wp[15*i + j][1] = Y[i][j] + M * X[i][j]/a;
			}
		}
		hpevalinv(zp, f2, wp);
		Vector<Double>x1Vector = new Vector<Double>();
		Vector<Double>y1Vector = new Vector<Double>();
		Vector<Double>x2Vector = new Vector<Double>();
		Vector<Double>y2Vector = new Vector<Double>();
		for (i = 0; i < 15; i++) {
			for (j = 0; j < 100; j++) {
				double posx1 = zp[101*i + j][0];
	    		double posy1 = zp[101*i + j][1];
	    		double posx2 = zp[101*i + j + 1][0];
	    		double posy2 = zp[101*i + j + 1][1];
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
		X = new double[15][101];
		Y = new double[15][101];
		for (i = 0; i < 15; i++) {
			for (j = 0; j < 101; j++) {
				X[i][j] = t[j] * a;
				Y[i][j] = (i+1)*m/16;
			    wp[15*j + i][0] = X[i][j];
			    wp[15*j + i][1] = Y[i][j] + M * X[i][j]/a;
			}
		}
		hpevalinv(zp, f2, wp);
		for (i = 0; i < 15; i++) {
			for (j = 0; j < 100; j++) {
				double posx1 = zp[101*i + j][0];
	    		double posy1 = zp[101*i + j][1];
	    		double posx2 = zp[101*i + j + 1][0];
	    		double posy2 = zp[101*i + j + 1][1];
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
	
	private void testHplmap3() {
		int i;
	    double w[][] = new double[6][2];
	    w[0][0] = 4.0;
	    w[0][1] = 0.0;
	    w[1][0] = 0.0;
	    w[1][1] = 2.0;
	    w[2][0] = -2.0;
	    w[2][1] = 4.0;
	    w[3][0] = -3.0;
	    w[3][1] = 0.0;
	    w[4][0] = -3.0;
	    w[4][1] = -1.0;
	    w[5][0] = 2.0;
	    w[5][1] = -2.0;
	    double x[] = new double[w.length];
	    double y[] = new double[w.length];
	    for (i = 0; i < w.length; i++) {
	    	x[i] = w[i][0];
	    	y[i] = w[i][1];
	    }
	    polygon p = scm.new polygon(x, y, null);
	    scmap f = hplmap(p, null, null, null);
	    double qdata[][] = f.qdata;
		double tol = f.accuracy;
		p = f.poly;
		w = p.vertex;
		double beta[] = new double[p.angle.length];
		for (i = 0; i < p.angle.length; i++) {
			beta[i] = p.angle[i] - 1.0;
		}
		double z[][] = f.prevertex;
		double c[] = f.constant;
		double zp[][] = new double[4][2];
		zp[0][0] = -1.0;
		zp[0][1] = 0.01;
		zp[1][0] = 0.0;
		zp[1][1] = 2.0;
		zp[2][0] = 4.0;
		zp[2][1] = 0.5;
		zp[3][0] = Double.POSITIVE_INFINITY;
		zp[3][1] = 0.0;
		double wp[][] = new double[zp.length][2];
		for (i = 0; i < wp.length; i++) {
			wp[i][0] = Double.NaN;
		}
		scm.hpmap(wp, zp, w, beta, z, c, qdata);
		for (i = 0; i < wp.length; i++) {
			if (i == 0) {
				System.out.println("Expected forward result: 3.718839996085665 - 0.046084791699413i");
			}
			else if (i == 1) {
				System.out.println("Expected forward result: 1.734612962216089 - 0.777136490010106i");
			}
			else if (i == 2) {
				System.out.println("Expected forward result: 1.179285609480821 - 1.753573737693204i");
			}
			else if (i == 3) {
				System.out.println("Expected forward result: 2.0 - 2.0i");
			}
			System.out.println("Actual forward result: " + wp[i][0] + " " + wp[i][1] + "i");
		}
		double wpinverse[][] = new double[wp.length][2];
		hpevalinv(wpinverse, f, wp);
		for (i = 0; i < wpinverse.length; i++) {
			if (i == 0) {
				System.out.println("Expected inverse result: -1 + 0.01i");
			}
			else if (i == 1) {
				System.out.println("Expected inverse result: 2.0i");
			}
			else if (i == 2) {
				System.out.println("Expected inverse result: 4.0 + 0.5i");	
			}
			else if (i == 3) {
				System.out.println("Expected inverse result: Infinity + 0.0i");
			}
			System.out.println("Actual inverse result: " + wpinverse[i][0] + " " + wpinverse[i][1] + "i");
		}
	}
	
	private void testStripmap1() {
		// NLConstrainedEngine and NLConstrainedEP do not work.  Nl2sol does not work for f1, but it 
		// works for f2.  NESolve works for both f1 and f2.
		// Plots are identical to Figure 4.6 except for 2 extra lines from plotpoly
		 double x[] = new double[]{Double.POSITIVE_INFINITY, -1, -2.5, Double.POSITIVE_INFINITY, 2.4,
				                   Double.POSITIVE_INFINITY, 2.4, Double.POSITIVE_INFINITY, -1, -2.5};
		 double y[] = new double[]{0, -1, -1, 0, -3.3, 0, -1.3, 0, 1, 1};
		 double alf[] = new double[]{0,2,1,-.85, 2, 0,2,-1.15,2,1};
		 polygon p = scm.new polygon(x, y, alf);
		 int endidx[] = new int[]{5, 7};
		 scmap f1 = stripmap(p, endidx);
		 int endidx2[] = new int[]{3,7};
		 scmap f2 = stripmap(p, endidx2);
		 double re[] = new double[]{0};
		 double im[] = new double[]{8};
		 double axis[] = new double[]{-4.3, 5.7, -6.15, 3.85};
		 stripplot(f1, re, im, Integer.MIN_VALUE, false, axis);
		 stripplot(f2, re, im, Integer.MIN_VALUE, false, axis);
	}
	
	private void testStripmap2() {
		int i;
	    double w[][] = new double[6][2];
	    w[0][0] = 4.0;
	    w[0][1] = 0.0;
	    w[1][0] = 0.0;
	    w[1][1] = 2.0;
	    w[2][0] = -2.0;
	    w[2][1] = 4.0;
	    w[3][0] = -3.0;
	    w[3][1] = 0.0;
	    w[4][0] = -3.0;
	    w[4][1] = -1.0;
	    w[5][0] = 2.0;
	    w[5][1] = -2.0;
	    double x[] = new double[w.length];
	    double y[] = new double[w.length];
	    for (i = 0; i < w.length; i++) {
	    	x[i] = w[i][0];
	    	y[i] = w[i][1];
	    }
	    polygon p = scm.new polygon(x, y, null);
	    int endidx[] = new int[]{0,3};
	    scmap M = stripmap(p, endidx);
	    double zp[][] = new double[4][2];
	    zp[0][0] = 1.0;
	    zp[0][1] = 1.0;
	    zp[1][0] = Double.POSITIVE_INFINITY;
	    zp[1][1] = 0.0;
	    zp[2][0] = -2.0;
	    zp[2][1] = 0.5;
	    zp[3][0] = 0.0;
	    zp[3][1] = 0.0;
	    double wp[][] = stripeval(M, zp);
	    for (i = 0; i < zp.length; i++) {
	    	if (i == 0) {
	    		System.out.println("Expected forward result: -3.0 - 0.312856946533931i");
	    	}
	    	else if (i == 1) {
	    		System.out.println("Expected forward result: -3.0 + 0.0i");
	    	}
	    	else if (i == 2) {
	    		System.out.println("Expected forward result: 3.534971699300866 - 0.074755569347609i");
	    	}
	    	else if (i == 3) {
	    		System.out.println("Expected forward result: 0.0 + 2.0i");
	    	}
	    	System.out.println("Actual forward result: " + wp[i][0] + " " + wp[i][1] + "i");
	    }
	    zp = new double[3][2];
	    zp[0][0] = 4.0;
	    zp[0][1] = 0.0;
	    zp[1][0] = -2.0;
	    zp[1][1] = 0.5;
	    zp[2][0] = 1.0;
	    zp[2][1] = 0.5;
	    wp = stripeval(M, zp);
	    double z0[][] = null;
	    double wpinverse[][] = stripevalinv(M, wp, z0);
	    for (i = 0; i < zp.length; i++) {
	    	if (i == 0) {
	    		System.out.println("Expected inverse result: 4 + 0i");
	    	}
	    	else if (i == 1) {
	    		System.out.println("Expected inverse result: -2 + 0.5i");
	    	}
	    	else if (i == 2) {
	    		System.out.println("Expected inverse result: 1 + 0.5i");
	    	}
	    	System.out.println("Actual inverse result: " + wpinverse[i][0] + " " + wpinverse[i][1] + "i");
	    }
	}
	
	private scmap stripmap(polygon poly, int endidx[]) {
		// Schwarz-Christoffel strip map object.
		// stripmap constructs a Schwarz-Christoffel strip map object for the polygon poly.
		// endidx is a two-vector containing the indices of the vertices that are the 
		// images of the left and right ends of the strip.  The parameter problem is 
		// solved using default options for the prevertices and the multiplicative
		// constant.
		
		// Extracted from original MATLAB stripmap routine copyright 1998 by Toby Driscoll.
		int i;
		double z[][] = null;
		double z0[][] = null;
		double c[] = null;
		double qdata[][] = null;
		double wn[][] = null;
		double betan[] = null;
		polygon poly2 = null;
		
		// Get data for the low-level functions
		double worig[][] = poly.vertex;
		double w[][] = new double[worig.length][2];
		for (i = 0; i < worig.length; i++) {
			w[i][0] = worig[i][0];
			w[i][1] = worig[i][1];
		}
		int n = w.length;
		double beta[] = new double[poly.angle.length];
		for (i = 0; i < poly.angle.length; i++) {
			beta[i] = poly.angle[i] - 1.0;
		}
		
		// Find prevertices if necessary
		if ((z == null) || (z.length == 0)) {
			// Apply scfix to enforce solver rules
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
			int endidxn[] = new int[2];
			scm.scfix(wn, betan, verticesAdded, endidxn, "st", w, beta, endidx);
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
			double x[] = new double[wn2.length];
			double y[] = new double[wn2.length];
			for (i = 0; i < wn2.length; i++) {
				x[i] = wn2[i][0];
				y[i] = wn2[i][1];
			}
			double alpha[] = new double[betan2.length];
			for (i = 0; i < betan2.length; i++) {
				alpha[i] = betan2[i] + 1.0;
			}
			poly2 = scm.new polygon(x, y, alpha);
			z = new double[wn2.length][2];
			c = new double[2];
			int nqpts = (int)Math.max(Math.ceil(-Math.log10(tolerance)), 4);
			qdata = new double[nqpts][2*betan2.length+2];
			stparam(z, c, qdata, wn2, betan2, endidxn, z0, tolerance);
		} // if ((z == null) || (z.length == 0))
		else {
			poly2 = poly;
		}
	    scmap M = scm.new scmap();
	    M.poly = poly2;
	    M.prevertex = z;
	    M.constant = c;
	    M.qdata = qdata;
	    M.accuracy = stripAccuracy(M);
	    return M;
	}
	
	private double stripAccuracy(scmap M) {
		// Apparent accuracy of the Schwarz-Christoffel strip map.
		// stripAccuracy estimates the accuracy of the Schwarz-CZhristoffel strip
		// map M.  The technique used is to compare the differences between successive
		// finite vertices to the integral between the corresponding prevertices, and
		// return the maximum.
		
		// Original MATLAB accuracy routine copyright 1998 by Toby Driscoll.
		
		// If an accuracy has been assigned, don't question it.
		int i, j;
		double cre[] = new double[1];
		double cim[] = new double[1];
		double acc;
		if (!Double.isNaN(M.accuracy)) {
		    acc = M.accuracy;
		    return acc;
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
		double c[] = M.constant;
		double qdata[][] = M.qdata;
		
		// Find ends of strip and renumber to put left end first
		int endidx[] = new int[2];
		for (i = 0; i < n; i++) {
			if (Double.isInfinite(z[i][0]) || Double.isInfinite(z[i][1])) {
				if (z[i][0] < 0) {
					endidx[0] = i;
				}
				if (z[i][0] > 0) {
					endidx[1] = i;
				}
			}
		} // for (i = 0; i < n; i++)
		
		// Integrate between consecutive finite pairs along bottom and top
		boolean bot[] = new boolean[n];
		for (i = 0; i < n; i++) {
			bot[i] = (z[i][1] == 0.0);
		}
		bot[endidx[0]] = false;
		bot[endidx[1]] = false;
		boolean top[] = new boolean[n];
		for (i = 0; i < n; i++) {
			top[i] = (z[i][1] != 0.0);
		}
		top[endidx[0]] = false;
		top[endidx[1]] = false;
		int numidxbot = 0;
		for (i = 0; i < n; i++) {
			if (bot[i] && (!Double.isInfinite(w[i][0])) && (!Double.isInfinite(w[i][1]))) {
				numidxbot++;
			}
		}
		int idxbot[] = new int[numidxbot];
		for (i = 0, j = 0; i < n; i++) {
			if (bot[i] && (!Double.isInfinite(w[i][0])) && (!Double.isInfinite(w[i][1]))) {
				idxbot[j++] = i;
			}
		}
		int numidxtop = 0;
		for (i = 0; i < n; i++) {
			if (top[i] && (!Double.isInfinite(w[i][0])) && (!Double.isInfinite(w[i][1]))) {
				numidxtop++;
			}
		}
		int idxtop[] = new int[numidxtop];
		for (i = 0, j = 0; i < n; i++) {
			if (top[i] && (!Double.isInfinite(w[i][0])) && (!Double.isInfinite(w[i][1]))) {
				idxtop[j++] = i;
			}
		}
		
		// Two columns hold endpoint indices for integrations
		int idx[][] = new int[idxbot.length + idxtop.length - 2][2];
		for (i = 0; i < idxbot.length-1; i++) {
			idx[i][0] = idxbot[i];
			idx[i][1] = idxbot[i+1];
		}
		for (i = 0; i < idxtop.length - 1; i++) {
			idx[idxbot.length-1+i][0] = idxtop[i];
			idx[idxbot.length-1+i][1] = idxtop[i+1];
		}
		
		// As a final check, integrate once across the strip
		int k = -1;
		double minVal = Double.MAX_VALUE;
		for (i = 0; i < idxtop.length; i++) {
			double val = Math.abs(z[idxtop[i]][0] - z[(endidx[0]+1)%n][0]);
			if (val < minVal) {
				minVal = val;
				k = i;
			}
		}
		int idx2[][] = new int[idx.length+1][2];
		for (i = 0; i < idx.length; i++) {
			idx2[i][0] = idx[i][0];
			idx2[i][1] = idx[i][1];
		}
		idx2[idx2.length-1][0] = (endidx[0]+1)% n;
		idx2[idx2.length-1][1] = idxtop[k];
		
		double I[][] = new double[idx2.length][2];
		double zl[][] = new double[idx2.length][2];
		double zr[][] = new double[idx2.length][2];
		for (i = 0; i < idx2.length; i++) {
			zl[i][0] = z[idx2[i][0]][0];
			zl[i][1] = z[idx2[i][0]][1];
			zr[i][0] = z[idx2[i][1]][0];
			zr[i][1] = z[idx2[i][1]][1];
		}
		
		// Two-stage integrations (neighboring prevertices)
		boolean s2[] = new boolean[idx2.length];
		int nums2 = 0;
		int numnots2 = 0;
		for (i = 0; i < idx2.length; i++) {
			s2[i] = ((idx2[i][1] - idx2[i][0]) == 1);
			if (s2[i]) {
				nums2++;
			}
			else {
				numnots2++;
			}
		}
		double zls2[][] = new double[nums2][2];
		int idxs20[] = new int[nums2];
		double zrs2[][] = new double[nums2][2];
		int idxs21[] = new int[nums2];
		for (i = 0, j = 0; i < idx2.length; i++) {
			if (s2[i]) {
				zls2[j][0] = zl[i][0];
				zls2[j][1] = zl[i][1];
				idxs20[j] = idx2[i][0];
				zrs2[j][0] = zr[i][0];
				zrs2[j][1] = zr[i][1];
				idxs21[j++] = idx2[i][1];
			}
		}
		double mid[][] = new double[nums2][2];
		for (i = 0; i < nums2; i++) {
			mid[i][0] = (zls2[i][0] + zrs2[i][0])/2.0;
			mid[i][1] = (zls2[i][1] + zrs2[i][1])/2.0;
		}
		double I1[][] = scm.stquadh(zls2, mid, idxs20, z, beta, qdata);
		double I2[][] = scm.stquadh(zrs2, mid, idxs21, z, beta, qdata);
		for (i = 0, j = 0; i < idx2.length; i++) {
		    if (s2[i]) {
		    	I[i][0] = I1[j][0] - I2[j][0];
		    	I[i][1] = I1[j][1] - I2[j][1];
		    	j++;
		    }
		}
		
		// Three-stage integrations
		double zlnots2[][] = new double[numnots2][2];
		int idxnots20[] = new int[numnots2];
		double zrnots2[][] = new double[numnots2][2];
		int idxnots21[] = new int[numnots2];
		int sing1[] = new int[numnots2];
		for (i = 0, j = 0; i < idx2.length; i++) {
			if (!s2[i]) {
				zlnots2[j][0] = zl[i][0];
				zlnots2[j][1] = zl[i][1];
				idxnots20[j] = idx2[i][0];
				zrnots2[j][0] = zr[i][0];
				zrnots2[j][1] = zr[i][1];
				idxnots21[j] = idx2[i][1];
				sing1[j++] = -1;
			}
		}
		double mid1[][] = new double[numnots2][2];
		double mid2[][] = new double[numnots2][2];
		for (i = 0; i < numnots2; i++) {
			mid1[i][0] = zlnots2[i][0];
			mid1[i][1] = 0.5;
			mid2[i][0] = zrnots2[i][0];
			mid2[i][1] = 0.5;
		}
		double I3[][] = scm.stquad(zlnots2, mid1, idxnots20, z, beta, qdata);
		double I4[][] = scm.stquadh(mid1, mid2, sing1, z, beta, qdata);
		double I5[][] = scm.stquad(zrnots2, mid2, idxnots21, z, beta, qdata);
		for (i = 0, j = 0; i < idx2.length; i++) {
			if (!s2[i]) {
				I[i][0] = I3[j][0] + I4[j][0] - I5[j][0];
				I[i][1] = I3[j][1] + I4[j][1] - I5[j][1];
				j++;
			}
		}
		double cI[][] = new double[idx2.length][2];
		for (i = 0; i < idx2.length; i++) {
			scm.zmlt(c[0], c[1], I[i][0], I[i][1], cre, cim);
			cI[i][0] = cre[0];
			cI[i][1] = cim[0];
		}
		double diffW[][] = new double[idx2.length][2];
		for (i = 0; i < idx2.length; i++) {
			diffW[i][0] = w[idx2[i][1]][0] - w[idx2[i][0]][0];
			diffW[i][1] = w[idx2[i][1]][1] - w[idx2[i][0]][1];
		}
		double absVal[] = new double[idx2.length];
		for (i = 0; i < idx2.length; i++) {
			absVal[i] = scm.zabs(cI[i][0] - diffW[i][0], cI[i][1] - diffW[i][1]);
		}
		acc = 0.0;
		for (i = 0; i < idx2.length; i++) {
			if (absVal[i] > acc) {
				acc = absVal[i];
			}
		}
		System.out.println("Strip accuracy = " + acc);
		return acc;
	}
	
	private void stparam(double z[][], double c[], double qdat[][], double w[][],
			double beta[], int ends[], double z0[][], double tol) {
	    // Schwarz-Christoffel strip parameter problem.
		// stparam solves the Schwarz-Christoffel parameter problem with the infinite
		// strip as fundamental domain and interior of the specified polygon as target.
		// w must be a vector of the vertices of the polygon, specified in counterclockwise
		// order.  beta is a vector of turning angles; see scangles. ends is a 2-vector
		// whose entries are indices of the vertices which are the images of the left and
		// right ends of the strip.  If ends is omitted, the user is requested to select
		// these vertices using the mouse.
		
		// If successful, stparam will return z, a vector of the preimages of w; c, the 
		// multiplicative constant of the conformal map; and qdat, an optional matrix
		// of quadrature data required by some of the other sc routines. If z0 is supplied,
		// it will be used an initial guess for z.
		
		// Original MATLAB stparam routine copyright 1998 by Toby Driscoll.
		int i, j, k;
		double cr[] = new double[1];
		double ci[] = new double[1];
		double z03[][];
		double y[] = null;
		
		if ((ends == null) || (ends.length == 0)) {
			String msg[] = new String[1];
			msg[0] = "Select the vertices that map to the ends of the strip.";
			ends = scm.scselect(w, beta, 2, "Select ends", msg);
		}
		
		int N = w.length; // Number of vertices
		// Renumber vertices so that the ends of the strip map to w([0, k])
		int renum[] = new int[N];
		for (i = ends[0]; i < N; i++) {
			renum[i-ends[0]] = i;
		}
		for (i = 0 ; i <= ends[0]-1; i++) {
			renum[N-ends[0]+i] = i;
		}
		double w2[][] = new double[w.length][2];
		double beta2[] = new double[beta.length];
		for (i = 0; i < N; i++) {
			w2[i][0] = w[renum[i]][0];
			w2[i][1] = w[renum[i]][1];
			beta2[i] = beta[renum[i]];
		}
		for (k = 0; k < N; k++) {
			if (renum[k] == ends[1]) {
				break;
			}
		}
		// n: number of finite prevertices
		int n = N-2;
		// nb: number of prevertices on bottom edge of strip
		int nb = k-1;
		
		// Check input data
		int err = scm.sccheck("st", w2, beta2, ends);
	    if (err == -1) {
	    	return;
	    }
	    if (err == 1) {
	    	MipavUtil.displayError("Use scfix to make polygon obey requirements");
	    	return;
	    }
	    
	    int nqpts = (int)Math.max(Math.ceil(-Math.log10(tol)), 4);
	    scm.scqdata(qdat, beta2, nqpts);  // quadrature data
	    boolean atinf[] = new boolean[beta2.length];
	    for (i = 0; i < beta2.length; i++) {
	    	atinf[i] = (beta2[i] <= -1);
	    }
	    
	    // Ignore images of ends of strip.
	    double w3[][] = new double[w2.length-2][2];
	    for (i = 0, j = 0; i < w2.length; i++) {
	    	if ((i != 0) && (i != k)) {
	    		w3[j][0] = w2[i][0];
	    		w3[j++][1] = w2[i][1];
	    	}
	    }
	    boolean atinf2[] = new boolean[atinf.length-2];
	    int numinf = 0;
	    for (i = 0, j = 0; i < atinf.length; i++) {
	    	if ((i != 0) && (i != k)) {
	    		atinf2[j++] = atinf[i];
	    		if (atinf[i]) {
	    			numinf++;
	    		}
	    	}
	    } // for (i = 0, j = 0; i < atinf.length; i++)
	    
	    if ((z0 == null) || (z0.length == 0)) {
	        // Make initial guess based on polygon.
	    	z0 = new double[n][2];
	    	if (numinf > 0) {
	    		// Can't base it on relative side lengths.
	    		double abs1 = scm.zabs(w3[nb-1][0] - w3[0][0], w3[nb-1][1] - w3[0][1]);
	    		double abs2 = scm.zabs(w3[n-1][0] - w3[nb][0], w3[n-1][1] - w3[nb][1]);
	    		double scale = (abs1 + abs2)/2.0;
	    		double space = scale/(nb - 1.0);
	    		for (i = 0; i < nb; i++) {
	    			z0[i][0] = i * space;
	    			z0[i][1] = 0.0;
	    		}
	    		if ((n - nb - 1) == 0) {
	    			z0[nb][0] = scale;
	    			z0[nb][1] = 1.0;
	    		}
	    		else {
		    		space = scale/(n-nb-1);
		    		double spacearr[] = new double[n-nb];
		    		for (i = 0; i < n-nb; i++) {
		    			spacearr[i] = i * space;
		    		}
		    		double spacearrflip[] = new double[n-nb];
		    		for (i = 0; i < n-nb; i++) {
		    		    spacearrflip[i] = spacearr[n-nb-1-i];	
		    		}
		    		for (i = nb; i < n; i++) {
		    			z0[i][0] = spacearrflip[i-nb];
		    			z0[i][1] = 1.0;
		    		}
	    		}
	    	} // if (numinf > 0)
	    	else {
	    		// This is from Louis Howell's code.
	    		double abs1 = scm.zabs(w3[n-1][0]-w3[0][0], w3[n-1][1] - w3[0][1]);
	    		double abs2 = scm.zabs(w3[nb-1][0] - w3[nb][0], w3[nb-1][1] - w3[nb][1]);
	    		double scale = (abs1 + abs2)/2.0;
	    		z0[0][0] = 0;
	    		z0[0][1] = 0;
	    		for (i = 1; i <= nb-1; i++) {
	    			z0[i][0] = z0[i-1][0] + (scm.zabs(w3[i][0] - w3[i-1][0], w3[i][1] - w3[i-1][1]))/scale;
	    		}
	    		if ((nb+1) == n) {
	    			z0[n-1][0] = (z0[0][0] + z0[nb-1][0])/2.0;
	    			z0[n-1][1] = (z0[0][1] + z0[nb-1][1])/2.0;
	    		}
	    		else {
	    			z0[n-1][0] = 0.0;
	    			z0[n-1][1] = 0.0;
	    			for (i = n-2; i >= nb; i--) {
	    				z0[i][0] = z0[i+1][0] + scm.zabs(w3[i+1][0] - w3[i][0], w3[i+1][1] - w3[i][1])/scale;
	    			}
	    		} // else
	    		scale = Math.sqrt(z0[nb-1][0]/z0[nb][0]);
	    		for (i = 0; i < nb; i++) {
	    			z0[i][0] = z0[i][0]/scale;
	    		}
	    		for (i = nb; i < n; i++) {
	    			z0[i][0] = z0[i][0] * scale;
	    			z0[i][1] = 1.0;
	    		}
	    	}
	    	z03 = z0;
	    } // if ((z0 == null) || (z0.length == 0))
	    else { // z0.length > 0
	    	double z02[][] = new double[z0.length][2];
	    	for (i = 0; i < z0.length; i++) {
	    		z02[i][0] = z0[renum[i]][0];
	    		z02[i][1] = z0[renum[i]][1];
	    	}
	    	if (z02.length == N) {
	    		if (((!Double.isInfinite(z02[0][0])) && (!Double.isInfinite(z02[0][1]))) || ((!Double.isInfinite(z02[k][0])) && (!Double.isInfinite(z02[k][1])))) {
	    			MipavUtil.displayError("Starting guess does not match ends of strip");
	    			System.exit(-1);
	    		}
	    		z03 = new double[z02.length-2][2];
	    		for (i = 0, j = 0; i < z02.length; i++) {
	    		    if ((i != 0) && (i != k)) {
	    		    	z03[j][0] = z02[i][0];
	    		    	z03[j++][1] = z02[i][1];
	    		    }
	    		}
	    	} // if (z02.length == N)
	    	else if (z02.length == n-1) {
	    		z03 = new double[z02.length+1][2];
	    		z03[0][0] = 0;
	    		z03[0][1] = 0;
	    		for (i = 0; i < z02.length; i++) {
	    			z03[i+1][0] = z02[i][0];
	    			z03[i+1][1] = z02[i][1];
	    		}
	    	} // else if (z02.length == n-1)
	    	else {
	    		z03 = z02;
	    	}
	    } // else z0.length > 0
	    
	    double y0[] = new double[n-1];
	    for (i = 0; i < nb-1; i++) {
	    	y0[i] = Math.log(z03[i+1][0] - z03[i][0]);
	    }
	    y0[nb-1] = z03[nb][0];
	    for (i = nb; i < n-1; i++) {
	    	y0[i] = Math.log(z03[i][0] - z03[i+1][0]);
	    }
	    
	    
	    // Find prevertices  (solve param problem)
	    
	    // Set up normalized lengths for nonlinear equations:
	    // indices of left and right integration endpoints
	    int left[] = new int[n];
	    left[0] = 0;
	    for (i = 1; i < n; i++) {
	    	left[i] = i-1; 
	    }
	    int right[] = new int[n];
	    right[0] = n-1;
	    for (i = 1; i < n; i++) {
	    	right[i] = i;
	    }
	    // Delete indices corresponding to vertices at Infinity
	    int findinf[] = new int[numinf];
	    for (i = 0, j = 0; i < atinf2.length; i++) {
	    	if (atinf2[i]) {
	    		findinf[j++] = i;
	    	}
	    }
	    int numleftdelete = 0;
	   
	    boolean doleft;
	    for (i = 0; i < n; i++) {
	    	doleft = true;
	    	for (j = 0; j < numinf  && doleft; j++) {
	    		if ((findinf[j] + 1) == i) {
	    			doleft = false;
	    		}
	    	} // for (j = 0; j < numinf  && doleft; j++)
	    	if (i == nb) {
	    		doleft = false;
	    	}
	    	if (!doleft) {
	    	    numleftdelete++;	
	    	}
	    } // for (i = 0; i < n; i++)
	    int left2[] = new int[n-numleftdelete];
	    for (i = 0, k = 0; i < n; i++) {
	    	doleft = true;
	    	for (j = 0; j < numinf  && doleft; j++) {
	    		if ((findinf[j] + 1) == i) {
	    			doleft = false;
	    		}
	    	} // for (j = 0; j < numinf  && doleft; j++)
	    	if (i == nb) {
	    		doleft = false;
	    	}
	    	if (doleft) {
	    	    left2[k++] = left[i];	
	    	}
	    } // for (i = 0, k = 0; i < n; i++)
	    int numrightdelete = 0;
	    boolean doright;
	    for (i = 0; i < n; i++) {
	    	doright = true;
	    	for (j = 0; j < numinf  && doright; j++) {
	    		if (findinf[j] == i) {
	    			doright = false;
	    		}
	    	} // for (j = 0; j < numinf  && doright; j++)
	    	if (i == nb) {
	    		doright = false;
	    	}
	    	if (!doright) {
	    	    numrightdelete++;	
	    	}
	    } // for (i = 0; i < n; i++)
	    int right2[] = new int[n-numrightdelete];
	    for (i = 0, k = 0; i < n; i++) {
	    	doright= true;
	    	for (j = 0; j < numinf  && doright; j++) {
	    		if (findinf[j] == i) {
	    			doright = false;
	    		}
	    	} // for (j = 0; j < numinf  && doright; j++)
	    	if (i == nb) {
	    		doright = false;
	    	}
	    	if (doright) {
	    	    right2[k++] = right[i];	
	    	}
	    } // for (i = 0, k = 0; i < n; i++)
	    boolean cmplx[] = new boolean[n-numleftdelete];
	    for (i = 0; i < left2.length; i++) {
	    	cmplx[i] = ((right2[i] - left2[i]) == 2);
	    }
	    cmplx[0] = false;
	    cmplx[1] = true;
	    // Normalize lengths
	    double nmlen[][] = new double[left2.length][2];
	    double denom[] = new double[2];
	    denom[0] = w3[n-1][0] - w3[0][0];
	    denom[1] = w3[n-1][1] - w3[0][1];
	    for (i = 0; i < left2.length; i++) {
	    	scm.zdiv(w3[right2[i]][0] - w3[left2[i]][0], w3[right2[i]][1] - w3[left2[i]][1], denom[0], denom[1], cr, ci);
	    	nmlen[i][0] = cr[0];
	    	nmlen[i][1] = ci[0];
	    }
	    // absolute value for finite ones
	    for (i = 0; i < cmplx.length; i++) {
	    	if (!cmplx[i]) {
	    		double abs1 = scm.zabs(nmlen[i][0], nmlen[i][1]);
	    		nmlen[i][0] = abs1;
	    		nmlen[i][1] = 0.0;
	    	}
	    } // for (i = 0; i < cmplx.length; i++)
	    // First entry is useless (=1)
	    double nmlen2[][] = new double[nmlen.length-1][2];
	    for (i = 1; i < nmlen.length; i++) {
	    	nmlen2[i-1][0] = nmlen[i][0];
	    	nmlen2[i-1][1] = nmlen[i][1];
	    }
	    
	    // Solve nonlinear system of equations
	    boolean useNLConstrainedEngine = false;
	    boolean useNLConstrainedEngineEP = false;
	    boolean useNESolve = true;
	    if (useNLConstrainedEngine) {
		    stpfun fm = new stpfun(y0, n, nb, beta2, nmlen2, left2, right2, cmplx, qdat);
			fm.driver();
			fm.dumpResults();
			int exitStatus = fm.getExitStatus();
			if (exitStatus < 0) {
				System.out.println("Error in NLConstrainedEngine during stpparam call to stpfun");
				scm.printExitStatus(exitStatus);
				System.exit(-1);
			}
			y = fm.getParameters();
	    }
	    else if (useNLConstrainedEngineEP) {
	    	DoubleDouble y0EP[] = new DoubleDouble[y0.length];
	    	for (i = 0; i < y0.length; i++) {
	    		y0EP[i] = DoubleDouble.valueOf(y0[i]);
	    	}
	    	DoubleDouble beta2EP[] = new DoubleDouble[beta2.length];
	    	for (i = 0; i < beta2.length; i++) {
	    		beta2EP[i] = DoubleDouble.valueOf(beta2[i]);
	    	}
	    	DoubleDouble nmlen2EP[][] = new DoubleDouble[nmlen2.length][2];
	    	for (i = 0; i < nmlen2.length; i++) {
	    		nmlen2EP[i][0] = DoubleDouble.valueOf(nmlen2[i][0]);
	    		nmlen2EP[i][1] = DoubleDouble.valueOf(nmlen2[i][1]);
	    	}
	    	DoubleDouble qdatEP[][] = new DoubleDouble[qdat.length][qdat[0].length];
	    	for (i = 0; i < qdat.length; i++) {
	    		for (j = 0; j < qdat[0].length; j++) {
	    			qdatEP[i][j] = DoubleDouble.valueOf(qdat[i][j]);
	    		}
	    	}
		    stpfunEP fm = new stpfunEP(y0EP, n, nb, beta2EP, nmlen2EP, left2, right2, cmplx, qdatEP);
			fm.driver();
			fm.dumpResults();
			int exitStatus = fm.getExitStatus();
			if (exitStatus < 0) {
				System.out.println("Error in NLConstrainedEngineEP during stpparam call to stpfunEP");
				scm.printExitStatus(exitStatus);
				System.exit(-1);
			}
			y = fm.getParameters();
	    } 
	    else if (useNESolve) {
	    	boolean initialJacobianIdentity = false;
	    	double fparam[] = null;
	    	boolean analyticJacobian = false;
	    	double scale[][] = null;
	    	Vector<Double> path = null;
	    	double btrack[] = null;
	    	int trace = 0; // 2 => on, set btrack null; 1 =>, on, leave btrack alone; 0 = off
	    	int method = 2; // 1 = linesearch; 2 = trustregion
	    	int maxIterations = 100*(n-1);
	    	double fvectol = tol;
	    	double steptol = tol/10.0;
	    	double maxStepSize = 0;
	    	int details11 = nqpts;
	    	int scaling = 0; // 0 => no scaling; 1 => scaling without scale; 2 => scaling with scale
	        stpfun3 neModel = new stpfun3(initialJacobianIdentity, y0, fparam, 
		    		analyticJacobian, scale, path,
		    		btrack, trace, method, maxIterations, fvectol, 
		    		steptol, maxStepSize, details11, scaling,
		    		n, nb, beta2, nmlen2, left2, right2, cmplx, qdat);
	        neModel.driver();
	        neModel.dumpTestResults();
	        int exitStatus = neModel.getExitStatus();
	        if (exitStatus != 1) {
	        	System.out.println("Error in NESolve during stpparam call to stpfun3");
				neModel.printExitStatus(exitStatus);
				System.exit(-1);	
	        }
	        y = neModel.getParameters();
	    } // else if (useNESolve)
	    else { // Use NL2sol
	    	double x[] = new double[y0.length+1];
	    	for (i = 0; i < y0.length; i++) {
	    		x[i+1] = y0[i];
	    	}
			
			int iv[] = new int[61 + y0.length]; // 61 + number of parameters
			//int vLength = 94 + tValues.length * 2 + 3 * tValues.length + 2
					//* (3 * 2 + 33) / 2;
			int vLength = 1000;
			double v[] = new double[vLength];
			boolean useAnalyticJacobian = false;
			stpfun2 solModel = new stpfun2(x, n, iv, v, useAnalyticJacobian, nb, beta2, nmlen2, left2, right2, cmplx, qdat);
			solModel.driver();
			solModel.dumpResults();
			y = new double[y0.length];
			for (i = 0; i < y0.length; i++) {
				y[i] = x[i+1];
			}
	    }
		
		// Convert y values to z
		double z1[][] = new double[n][2];
		for (i = 1; i < nb; i++) {
			z1[i][0] = z1[i-1][0] + Math.exp(y[i-1]);
			z1[i][1] = 0;
		}
		z1[nb][0] = y[nb-1];
		z1[nb][1] = 1.0;
		for (i = nb+1; i < n; i++) {
			z1[i][0] = z1[i-1][0] - Math.exp(y[i-1]);
			z1[i][1] = 1.0;
		}
		double z2[][] = new double[z1.length+2][2];
		z2[0][0] = Double.NEGATIVE_INFINITY;
		z2[0][1] = 0.0;
		for (i = 0; i < nb; i++) {
			z2[i+1][0] = z1[i][0];
			z2[i+1][1] = z1[i][1];
		}
		z2[nb+1][0] = Double.POSITIVE_INFINITY;
		z2[nb+1][1] = 0.0;
		for (i = nb; i < n; i++) {
			z2[i+2][0] = z1[i][0];
			z2[i+2][1] = z1[i][1];
		}
		
		// Determine multiplicative constant
		double mid[][] = new double[1][2];
		mid[0][0] = (z2[1][0] + z2[2][0])/2.0;
		mid[0][1] = (z2[1][1] + z2[2][1])/2.0;
		double z2in[][] = new double[1][2];
		z2in[0][0] = z2[2][0];
		z2in[0][1] = z2[2][1];
		int sing2[] = new int[]{2};
		double I2[][] = scm.stquad(z2in, mid, sing2, z2, beta2, qdat);
		double z1in[][] = new double[1][2];
		z1in[0][0] = z2[1][0];
		z1in[0][1] = z2[1][1];
		int sing1[] = new int[]{1};
		double I1[][] = scm.stquad(z1in, mid, sing1, z2, beta2, qdat);
		double g[] = new double[2];
		g[0] = I2[0][0] - I1[0][0];
		g[1] = I2[0][1] - I1[0][1];
		scm.zdiv(w3[0][0] - w3[1][0], w3[0][1] - w3[1][1], g[0], g[1], cr, ci);
		c[0] = cr[0];
		c[1] = ci[0];
		
		// Undo renumbering
		for (i = 0; i < N; i++) {
			j = renum[i];
			z[j][0] = z2[i][0];
			z[j][1] = z2[i][1];
		}
		double qdat2[][] = new double[qdat.length][qdat[0].length];
		for (i = 0; i < qdat.length; i++) {
			for (j = 0; j < qdat[0].length; j++) {
				qdat2[i][j] = qdat[i][j];
			}
		}
		for (i = 0; i < qdat.length; i++) {
			for (j = 0; j < N; j++) {
				k = renum[j];
				qdat[i][k] = qdat2[i][j];
				qdat[i][N+1+k] = qdat2[i][N+1+j];
			}
		}
	}
	
	private double[][] stripeval(scmap M, double zp[][]) {
		// Evaluate Schwarz-Christoffel strip map at points.
		// stripeval evaluates the Schwarz-Christoffel map M at the points zp
		// in the strip 0 <= Im z <= 1.  
		// Original MATLAB eval routine copyright 1998 by Toby Driscoll.
		int i, j;
		polygon p = M.poly;
		double w[][] = p.vertex;
		double beta[] = new double[p.angle.length];
		for (i = 0; i < p.angle.length; i++) {
			beta[i] = p.angle[i] - 1.0;
		}
		double z[][] = M.prevertex;
		double c[] = M.constant;
		double qdata[][] = M.qdata;
		double wp[][] = new double[zp.length][2];
		for (i = 0; i < zp.length; i++) {
			wp[i][0] = Double.NaN;
		}
		boolean idx[] = new boolean[zp.length];
		int numidx = 0;
		for (i = 0; i < zp.length; i++) {
			idx[i] = (zp[i][1] > -eps) && (zp[i][1] < 1.0 + eps);
			if (idx[i]) {
			    numidx++;	
			}
		}
		double zpidx[][] = new double[numidx][2];
		for (i = 0, j = 0; i < zp.length; i++) {
		    if (idx[i]) {
		    	zpidx[j][0] = zp[i][0];
		    	zpidx[j++][1] = zp[i][1];
		    }
		}
		double wpidx[][] = scm.stmap(zpidx, w, beta, z, c, qdata);
		for (i = 0, j = 0; i < zp.length; i++) {
			if (idx[i]) {
				wp[i][0] = wpidx[j][0];
				wp[i][1] = wpidx[j++][1];
			}
		}
		return wp;
	}
	
	
	
	private double[][] stripevalinv(scmap M, double wp[][], double z0[][]) {
		// Invert Schwarz-Christoffel strip map at points.
		// stripevalinv evaluates the inverse of the Schwarz-Christoffel map M
		// at the points wp in the polygon.  The default tolerance of M is used.
		// From original MATLAB evalinv routine copyright 1998 by Toby Driscoll.
		int i;
		double qdata[][] = M.qdata;
		double tol = M.accuracy;
		polygon p = M.poly;
		double w[][] = p.vertex;
		int n = w.length;
		double beta[] = new double[p.angle.length];
		for (i = 0; i < p.angle.length; i++) {
			beta[i] = p.angle[i] - 1.0;
		}
		double z[][] = M.prevertex;
		double c[] = M.constant;
		boolean ode = true;
		boolean newton = true;
		int maxiter = 500;
		double z02[][] = null;
		
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
		double zp[][] = stinvmap(wp, w, beta, z, c, qdata, z02, ode, newton, tol, maxiter);
		return zp;
	}
	
	private double[][] stinvmap(double wp[][], double w[][], double beta[], double z[][],
			double c[], double qdat[][], double z0[][], boolean ode, boolean newton, double tol,
			int maxiter) {
		// Schwarz-Christoffel strip inverse map.
		// stinvmap computes the inverse of the Schwarz-Christoffel strip map (i.e., from the poloygon
		// to the strip) at the points fiven in the vector wp.  The other arguments are as in stparam.
		
		// The default algorithm is to solve an ODE in order to obtain a fair approximation for zp,
		// and then improve zp with Newton iterations.  The ODE solution at wp requires a vector z0
		// whose forward image w0 is such that for each j, the line segment connecting wp[j] and w0[j]
		// lies inside the polygon.  By default z0 is chosen by a fairly robust automatic process.
		// You can choose to use either an ODE solution or Newton iterations exclusively.
		
		// stinvmap has two interpretations.  If the ODE solution is being used,z0 overrides the
		// automatic selection of initial points.  (This can be handy in convex polygons, where the
		// choice of z0 is trivial.) Otherwise,z0 is taken as an initial guess to zp.  In either case,
		// if length(z0) == 1, the value z0 is used for all elements of wp;  otherwise, length(z0) 
		// should equal length(wp).
		
		// Original MATLAB stinvmap routine copyright 1998 by Toby Driscoll.
		
		int i, j, k;
		int m;
		int n = w.length;
		double zp[][] = new double[wp.length][2];
		int lenwp = wp.length;
		int nqpts;
		double qdat2[][] = null;
		double F[][] = null;
		double cr[] = new double[1];
		double ci[] = new double[1];
		
		if (qdat.length == 1) {
			nqpts = Math.max((int)Math.ceil(-Math.log10(qdat[0][0])), 2);
			qdat2 = new double[nqpts][2*beta.length+2];
			scm.scqdata(qdat2, beta, nqpts);
		}
		else {
			qdat2 = qdat;
		}
		boolean done[] = new boolean[lenwp];
		// First, trap all points indistinguishable from vertices, or they will cause trouble.
		// Modified 05/14/2007 to work around bug in matlab 2007a.
		for (j = 0; j < n; j++) {
		    for (i = 0; i < lenwp; i++) {
		    	if (scm.zabs(wp[i][0] - w[j][0], wp[i][1] - w[j][1]) < 3.0*eps) {
		    		zp[i][0] = z[j][0];
		    		zp[i][1] = z[j][1];
		    		done[i] = true;
		    	}
		    }
		} // for (j = 0; j < n; j++)
		int sumdone = 0;
		int numnotdone = 0;
		for (i = 0; i < lenwp; i++) {
			if (done[i]) {
				sumdone++;
			}
			else {
				numnotdone++;
			}
		}
		lenwp = lenwp - sumdone;
		if (lenwp == 0) {
			return null;
		}
		
		// ODE
		if (ode) {
			double wpnotdone[][] = new double[numnotdone][2];
			for (i = 0, j = 0; i < done.length; i++) {
	    		if (!done[i]) {
	    			wpnotdone[j][0] = wp[i][0];
	    			wpnotdone[j++][1] = wp[i][1];
	    		}
	    	}
			double w0[][] = null;
		    if ((z0 == null) || (z0.length == 0)) {
		        // Pick a value z0 (not a singularity) and compute the map there.	
		    	z0 = new double[numnotdone][2];
		    	w0 = new double[numnotdone][2];
		    	scm.scimapz0(z0, w0, "st", wpnotdone, w, beta, z, c, qdat2, null);
		    } // if ((z0 == null) || (z0.length == 0))
		    else {
		    	w0 = scm.stmap(z0, w, beta, z, c, qdat2);
		    	if ((z0.length == 1) && (lenwp > 1)) {
		    		double temp0 = z0[0][0];
		    		double temp1 = z0[0][1];
		    		z0 = new double[lenwp][2];
		    	    for (i = 0; i < lenwp; i++) {
		    	    	z0[i][0] = temp0;
		    	    	z0[i][1] = temp1;
		    	    }
		    	    temp0 = w0[0][0];
		    	    temp1 = w0[0][1];
		    	    w0 = new double[lenwp][2];
		    	    for (i = 0; i < lenwp; i++) {
		    	    	w0[i][0] = temp0;
		    	    	w0[i][1] = temp1;
		    	    }
		    	} // if ((z0.length == 1) && (lenwp > 1))
		    } // else
		    
		    // Use relaxed OD tol if improving with Newton.
		    double odetol = tol;
		    if (newton) {
		        odetol = Math.max(tol, 1.0E-2);	
		    }
		    double abstol = odetol;
			double reltol = odetol;
		    
		    // Rescale dependent coordinate.
		    double scale[][] = new double[numnotdone][2];
		    for (i = 0; i < numnotdone; i++) {
		    	scale[i][0] = wpnotdone[i][0] - w0[i][0];
		    	scale[i][1] = wpnotdone[i][1] - w0[i][1];
		    }
		    
		    // Solve ODE
		    double z02[] = new double[2*z0.length];
		    for (i = 0; i < z0.length; i++) {
		    	z02[i] = z0[i][0];
		    	z02[z0.length + i] = z0[i][1];
		    }
		    double yarr[][] = new double[3][z02.length];
			for (i = 0; i < z02.length; i++) {
				yarr[0][i] = z02[i];
			}
			double coef = 0.1;
			double t[] = new double[1];
			double relerr[] = new double[1];
			double abserr[] = new double[1];
			int iflag[] = new int[1];
			ODESTModel modODE;
			double tout;
			while (true) {
				t[0] = 0;
				tout = 0.5;
				relerr[0] = coef*reltol;
				abserr[0] = coef*abstol;
				iflag[0] = 1;
				for (i = 0; i < z02.length; i++) {
				    z02[i] = yarr[0][i];
				}
				modODE = new ODESTModel(z02.length, z02, t, tout, relerr,
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
			for (i = 0; i < z02.length; i++) {
				yarr[1][i] = z02[i];
			}
			coef = 0.1;
			while (true) {
				t[0] = 0.5;
				tout = 1.0;
				relerr[0] = coef*reltol;
				abserr[0] = coef*abstol;
				iflag[0] = 1;
				for (i = 0; i < z02.length; i++) {
				    z02[i] = yarr[1][i];	
				}
				modODE = new ODESTModel(z02.length, z02, t, tout, relerr,
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
			for (i = 0; i < z02.length; i++) {
				yarr[2][i] = z02[i];
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
		} // if (ode)
		
		// Newton iterations
		if (newton) {
			double zn[][];
		    if (!ode) {
		    	if ((z0.length == 1)&& (lenwp > 1)) {
		    		zn = new double[lenwp][2];
		    		for (i = 0; i < lenwp; i++) {
		    		    zn[i][0] = z0[0][0];
		    		    zn[i][1] = z0[0][1];
		    		}
		    	} // if ((z0.length == 1)&& (lenwp > 1))
		    	else {
		    		zn = new double[z0.length][2];
		    		for (i = 0; i < z0.length; i++) {
		    			zn[i][0] = z0[i][0];
		    			zn[i][1] = z0[i][1];
		    		}
		    	}
		    	for (i = 0; i < done.length; i++) {
	    			if (done[i]) {
	    				zn[i][0] = zp[i][0];
	    				zn[i][1] = zp[i][1];
	    			}
		    	}	
		    } // if (!ode)
		    else {
		    	zn = new double[zp.length][2];
			    for (i = 0; i < zp.length; i++) {
			        zn[i][0] = zp[i][0];
			        zn[i][1] = zp[i][1];
			    }	
		    } // else
		    
		    k = 0;
		    while ((numnotdone > 0) && (k < maxiter)) {
			    double wpnotdone[][] = new double[numnotdone][2];
			    double znnotdone[][] = new double[numnotdone][2];
			    for (i = 0, j = 0; i < done.length; i++) {
			    	if (!done[i]) {
			    		wpnotdone[j][0] = wp[i][0];
			    		wpnotdone[j][1] = wp[i][1];
			    		znnotdone[j][0] = zn[i][0];
			    		znnotdone[j++][1] = zn[i][1];
			    	}
			    }
			    double wpnotdone2[][] = scm.stmap(znnotdone, w, beta, z, c, qdat2);
			    F = new double[numnotdone][2];
			    for (i = 0; i < numnotdone; i++) {
			    	F[i][0] = wpnotdone[i][0] - wpnotdone2[i][0];
			    	F[i][1] = wpnotdone[i][1] - wpnotdone2[i][1];
			    }
			    double prec[] = new double[]{1.0,0.0};
			    double ds[][] = scm.stderiv(znnotdone, z, beta,prec, -1);
			    double dF[][] = new double[numnotdone][2];
			    for (i = 0; i < numnotdone; i++) {
			        scm.zmlt(c[0], c[1], ds[i][0], ds[i][1], cr, ci);
			        dF[i][0] = cr[0];
			        dF[i][1] = ci[0];
			    }
			    double Fdiv[][] = new double[numnotdone][2];
			    for (i = 0; i < numnotdone; i++) {
			    	scm.zdiv(F[i][0], F[i][1], dF[i][0], dF[i][1], cr, ci);
			    	Fdiv[i][0] = cr[0];
			    	Fdiv[i][1] = ci[0];
			    }
			    for (i = 0, j = 0; i < lenwp; i ++) {
			    	if (!done[i]) {
			    		zn[i][0] = zn[i][0] + Fdiv[j][0];
			    		zn[i][1] = zn[i][1] + Fdiv[j++][1];
			    	}
			    }
			    for (i = 0, j = 0; i < done.length; i++) {
			    	if (!done[i]) {
			    		done[i] = (scm.zabs(F[j][0], F[j][1]) < tol);
			    		j++;
			    	}
			    }
			    numnotdone = 0;
			    for (i = 0; i < done.length; i++) {
			        if (!done[i]) {
			        	numnotdone++;
			        }
			    }
			    k = k + 1;
		    } // while ((numnotdone > 0) && (k < maxiter))
		    double maxabsF = 0.0;
		    for (i = 0; i < F.length; i++) {
		    	double resid = scm.zabs(F[i][0], F[i][1]);
		    	//Preferences.debug("resid = " + resid + "\n", Preferences.DEBUG_ALGORITHM);
		    	if (resid > maxabsF) {
		    		maxabsF = resid;
		    	}
		    }
		    if (maxabsF > tol ) {
		    	MipavUtil.displayWarning("Check solution: maximum residual in stinvamp = " + maxabsF);
		    }
		    for (i = 0; i < zn.length; i++) {
		    	zp[i][0] = zn[i][0];
		    	zp[i][1] = zn[i][1];
		    }
		} // if (newton)
		
		return zp;
	}
	
	private void hpevalinv(double zp[][], scmap M, double wp[][]) {
		// Invert Schwarz-Christoffel half-plane at points.
		// hpevalinv evaluates the inverse of the Schwarz-Christoffel map M
		// at the points wp in the polygon.  The default tolerance of M is used.
		// From original MATLAB evalinv routine copyright 1998 by Toby Driscoll.
		int i;
		double qdata[][] = M.qdata;
		double tol = M.accuracy;
		polygon p = M.poly;
		double w[][] = p.vertex;
		int n = w.length;
		double beta[] = new double[p.angle.length];
		for (i = 0; i < p.angle.length; i++) {
			beta[i] = p.angle[i] - 1.0;
		}
		double z[][] = M.prevertex;
		double c[] = M.constant;
		for (i = 0; i < wp.length; i++) {
			zp[i][0] = Double.NaN;
		}
		boolean ode = true;
		boolean newton = true;
		int maxiter = 500;
		double z0[][] = null;
		hpinvmap(zp, wp, w, beta, z, c, qdata, z0, ode, newton, tol, maxiter);
	}
	
	private void hpinvmap(double zp[][], double wp[][], double w[][], double beta[], double z[][],
			double c[], double qdat[][], double z0[][], boolean ode, boolean newton, double tol,
			int maxiter) {
		// Schwarz-Christoffel half-plane inverse map.
		// hpivnmap computes the inverse of the upper half-plane map( i.e., from the polygon to the
		// upper half-plane) at the points given in the vector wp.  The arguments are as in hpparam.
		// tol is a scalar tolerance and qdat is a quadrature-data matrix as returned by scqdata.
		
		// The default algorithm is to solve an ode in order to obtain a fair approximation for zp,
		// and then improve zp with Newton iterations.  The ode solution at wp requires a vector z0
		// whose forward image w0 is such that for each j, the line segment connecting wp[j] and
		// w0[j] lies inside the polygon.  By default z0 is chosen by a fairly robust automatic
		// process.  You can choose to use either an ODE solution or Newton iterations exclusively.
		
		// hpinvmap has two interpretations.  If the ode solution is being used, z0 overrides the 
		// automatic selection of initial points.  (This can be handy in convex polygons, where the
		// choice of z0 is trivial.)  Otherwise, z0 is taken as an initial guess to zp.  In either 
		// case, if length((z0) == 1, the value z0 is used for all elements of wp; otherwise,
		// length(z0) should equal length(wp).
		
		// Original MATLAB hpinvmap routine copyright 1998 by Toby Driscoll.
		int i, j, k, m;
		double cr[] = new double[1];
		double ci[] = new double[1];
		//int nfin;
		double F[][] = null;
		int n = w.length;
		for (i = 0; i < wp.length; i++) {
			zp[i][0] = 0;
			zp[i][1] = 0;
		}
		int lenwp = wp.length;
		//if (Double.isInfinite(z[n-1][0]) || Double.isInfinite(z[n-1][1])) {
		    //nfin = n-1;	
		//}
		//else {
			//nfin = n;
		//}
		
		boolean done[] = new boolean[lenwp];
		// First, trap all points indistinguishable from vertices, or they will cause trouble.
		// Modified 05/14/2007 to work around bug in matlab 2007a.
		for (j = 0; j < n; j++) {
		    for (i = 0; i < lenwp; i++) {
		    	if (scm.zabs(wp[i][0] - w[j][0], wp[i][1] - w[j][1]) < 3.0*eps) {
		    		zp[i][0] = z[j][0];
		    		zp[i][1] = z[j][1];
		    		done[i] = true;
		    	}
		    }
		} // for (j = 0; j < n; j++)
		int sumdone = 0;
		int numnotdone = 0;
		for (i = 0; i < lenwp; i++) {
			if (done[i]) {
				sumdone++;
			}
			else {
				numnotdone++;
			}
		}
		lenwp = lenwp - sumdone;
		if (lenwp == 0) {
			return;
		}
		
		// ODE
		if (ode) {
			double wpnotdone[][] = new double[numnotdone][2];
			for (i = 0, j = 0; i < done.length; i++) {
	    		if (!done[i]) {
	    			wpnotdone[j][0] = wp[i][0];
	    			wpnotdone[j++][1] = wp[i][1];
	    		}
	    	}
			double w0[][] = null;
		    if ((z0 == null) || (z0.length == 0)) {
		        // Pick a value z0 (not a singularity) and compute the map there.	
		    	z0 = new double[numnotdone][2];
		    	w0 = new double[numnotdone][2];
		    	scm.scimapz0(z0, w0, "hp", wpnotdone, w, beta, z, c, qdat, null);
		    } // if ((z0 == null) || (z0.length == 0))
		    else {
		    	w0 = new double[z0.length][2];
		    	scm.hpmap(w0, z0, w, beta, z, c, qdat);
		    	if ((z0.length == 1) && (lenwp > 1)) {
		    		double temp0 = z0[0][0];
		    		double temp1 = z0[0][1];
		    		z0 = new double[lenwp][2];
		    	    for (i = 0; i < lenwp; i++) {
		    	    	z0[i][0] = temp0;
		    	    	z0[i][1] = temp1;
		    	    }
		    	    temp0 = w0[0][0];
		    	    temp1 = w0[0][1];
		    	    w0 = new double[lenwp][2];
		    	    for (i = 0; i < lenwp; i++) {
		    	    	w0[i][0] = temp0;
		    	    	w0[i][1] = temp1;
		    	    }
		    	} // if ((z0.length == 1) && (lenwp > 1))
		    } // else
		    
		    // Use relaxed OD tol if improving with Newton.
		    double odetol = tol;
		    if (newton) {
		        odetol = Math.max(tol, 1.0E-3);	
		    }
		    double abstol = odetol;
			double reltol = odetol;
		    
		    // Rescale dependent coordinate.
		    double scale[][] = new double[numnotdone][2];
		    for (i = 0; i < numnotdone; i++) {
		    	scale[i][0] = wpnotdone[i][0] - w0[i][0];
		    	scale[i][1] = wpnotdone[i][1] - w0[i][1];
		    }
		    
		    // Solve ODE
		    double z02[] = new double[2*z0.length];
		    for (i = 0; i < z0.length; i++) {
		    	z02[i] = z0[i][0];
		    	z02[z0.length + i] = z0[i][1];
		    }
		    double yarr[][] = new double[3][z02.length];
			for (i = 0; i < z02.length; i++) {
				yarr[0][i] = z02[i];
			}
			double coef = 0.1;
			double t[] = new double[1];
			double relerr[] = new double[1];
			double abserr[] = new double[1];
			int iflag[] = new int[1];
			ODEHPModel modODE;
			double tout;
			while (true) {
				t[0] = 0;
				tout = 0.5;
				relerr[0] = coef*reltol;
				abserr[0] = coef*abstol;
				iflag[0] = 1;
				for (i = 0; i < z02.length; i++) {
				    z02[i] = yarr[0][i];
				}
				modODE = new ODEHPModel(z02.length, z02, t, tout, relerr,
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
			for (i = 0; i < z02.length; i++) {
				yarr[1][i] = z02[i];
			}
			coef = 0.1;
			while (true) {
				t[0] = 0.5;
				tout = 1.0;
				relerr[0] = coef*reltol;
				abserr[0] = coef*abstol;
				iflag[0] = 1;
				for (i = 0; i < z02.length; i++) {
				    z02[i] = yarr[1][i];	
				}
				modODE = new ODEHPModel(z02.length, z02, t, tout, relerr,
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
			for (i = 0; i < z02.length; i++) {
				yarr[2][i] = z02[i];
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
			for (i = 0; i < zp.length; i++) {
				if (zp[i][1] < 0.0) {
					zp[i][1] = 0.0;
				}
			}
		} // if (ode)
		
		// Newton iterations
		if (newton) {
			double zn[][];
		    if (!ode) {
		    	if ((z0.length == 1)&& (lenwp > 1)) {
		    		zn = new double[lenwp][2];
		    		for (i = 0; i < lenwp; i++) {
		    		    zn[i][0] = z0[0][0];
		    		    zn[i][1] = z0[0][1];
		    		}
		    	} // if ((z0.length == 1)&& (lenwp > 1))
		    	else {
		    		zn = new double[z0.length][2];
		    		for (i = 0; i < z0.length; i++) {
		    			zn[i][0] = z0[i][0];
		    			zn[i][1] = z0[i][1];
		    		}
		    	}
		    	for (i = 0; i < done.length; i++) {
	    			if (done[i]) {
	    				zn[i][0] = zp[i][0];
	    				zn[i][1] = zp[i][1];
	    			}
		    	}	
		    } // if (!ode)
		    else {
		    	zn = new double[zp.length][2];
			    for (i = 0; i < zp.length; i++) {
			        zn[i][0] = zp[i][0];
			        zn[i][1] = zp[i][1];
			    }	
		    } // else
		    
		    k = 0;
		    while ((numnotdone > 0) && (k < maxiter)) {
			    double wpnotdone[][] = new double[numnotdone][2];
			    double wpnotdone2[][] = new double[numnotdone][2];
			    double znnotdone[][] = new double[numnotdone][2];
			    for (i = 0, j = 0; i < done.length; i++) {
			    	if (!done[i]) {
			    		wpnotdone[j][0] = wp[i][0];
			    		wpnotdone[j][1] = wp[i][1];
			    		znnotdone[j][0] = zn[i][0];
			    		znnotdone[j++][1] = zn[i][1];
			    	}
			    }
			    scm.hpmap(wpnotdone2, znnotdone, w, beta, z, c, qdat);
			    F = new double[numnotdone][2];
			    for (i = 0; i < numnotdone; i++) {
			    	F[i][0] = wpnotdone[i][0] - wpnotdone2[i][0];
			    	F[i][1] = wpnotdone[i][1] - wpnotdone2[i][1];
			    }
			    double dF[][] = hpderiv(znnotdone, z, beta, c);
			    double Fdiv[][] = new double[numnotdone][2];
			    for (i = 0; i < numnotdone; i++) {
			    	scm.zdiv(F[i][0], F[i][1], dF[i][0], dF[i][1], cr, ci);
			    	Fdiv[i][0] = cr[0];
			    	Fdiv[i][1] = ci[0];
			    }
			    double znew[][] = new double[numnotdone][2];
			    for (i = 0; i < numnotdone; i++) {
			    	znew[i][0] = znnotdone[i][0] + Fdiv[i][0];
			    	znew[i][1] = znnotdone[i][1] + Fdiv[i][1];
			    }
			    for (i = 0, j = 0; i < done.length; i++) {
			    	if (!done[i]) {
			    		zn[i][0] = znew[j][0];
			    		zn[i][1] = Math.max(0.0, znew[j][1]);
			    		j++;
			    	}
			    } // for (i = 0, j = 0; i < done.length; i++)
			    for (i = 0, j = 0; i < done.length; i++) {
			    	if (!done[i]) {
			    		done[i] = (scm.zabs(F[j][0], F[j][1]) < tol);
			    		j++;
			    	}
			    }
			    numnotdone = 0;
			    for (i = 0; i < done.length; i++) {
			        if (!done[i]) {
			        	numnotdone++;
			        }
			    }
			    k = k + 1;
		    } // while ((numnotdone > 0) && (k < maxiter))
		    double maxabsF = 0.0;
		    for (i = 0; i < F.length; i++) {
		    	double resid = scm.zabs(F[i][0], F[i][1]);
		    	//Preferences.debug("resid = " + resid + "\n", Preferences.DEBUG_ALGORITHM);
		    	if (resid > maxabsF) {
		    		maxabsF = resid;
		    	}
		    }
		    if (maxabsF > tol ) {
		    	MipavUtil.displayWarning("Check solution: maximum residual in hpinvamp = " + maxabsF);
		    }
		    for (i = 0; i < zn.length; i++) {
		    	zp[i][0] = zn[i][0];
		    	zp[i][1] = zn[i][1];
		    }
		} // if (newton)
	}
	
	class ODESTModel extends ODE {
		// Used by stinvmap for solution of an ODE.
		double scale[][];
		double z[][];
		double beta[];
		double c[];
		public ODESTModel(int neqn, double y[], double t[], double tout, double relerr[],
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
			
			double fprime[][] = scm.stderiv(zp, z, beta, c, -1);
			double f[][] = new double[scale.length][2];
			for (i = 0; i < scale.length; i++) {
				scm.zdiv(scale[i][0], scale[i][1], fprime[i][0], fprime[i][1], cr, ci);
				f[i][0] = cr[0];
				f[i][1] = ci[0];
			}
			
			for (i = 0; i < f.length; i++) {
				yp[i] = f[i][0];
				yp[f.length + i] = f[i][1];
			}	
		}
	}
	
	class ODEHPModel extends ODE {
		double scale[][];
		double z[][];
		double beta[];
		double c[];
		public ODEHPModel(int neqn, double y[], double t[], double tout, double relerr[],
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
			// Don't allow points in the lower half-plane.  This really messes up the
			// derivative calculation;
			double zp[][] = new double[lenzp][2];
			for (i = 0; i < lenzp; i++) {
				zp[i][0] = yy[i];
			}
			for (i = lenzp; i < lenyy; i++) {
				zp[i-lenzp][1] = Math.max(0.0, yy[i]);
			}
			
			double fprime[][] = hpderiv(zp, z, beta, c);
			double f[][] = new double[scale.length][2];
			for (i = 0; i < scale.length; i++) {
				scm.zdiv(scale[i][0], scale[i][1], fprime[i][0], fprime[i][1], cr, ci);
				f[i][0] = cr[0];
				f[i][1] = ci[0];
			}
			
			for (i = 0; i < f.length; i++) {
				yp[i] = f[i][0];
				yp[f.length + i] = f[i][1];
			}	
		}
	}
	
	private double[][] hpderiv(double zp[][], double z[][], double beta[], double c[]) {
		// Derivative of the half-plane map
		// hpderiv returns the derivative at the points of zp of the Schwarz-Christoffel
		// half-plane map defined by z, bet, and c
		
		// Original MATLAB hpderiv routine copyright 1998 by Toby Driscoll
		
		int i, j;
		double cr[] = new double[1];
		double ci[] = new double[1];
		int numnotinfz = 0;
		for (i = 0; i < z.length; i++) {
			if ((!Double.isInfinite(z[i][0])) && (!Double.isInfinite(z[i][1]))) {
				numnotinfz++;
			}
		}
		double zf[][] = new double[numnotinfz][2];
		double betaf[] = new double[numnotinfz];
		for (i = 0, j = 0; i < z.length; i++) {
			if ((!Double.isInfinite(z[i][0])) && (!Double.isInfinite(z[i][1]))) {
				zf[j][0] = z[i][0];
				zf[j][1] = z[i][1];
				betaf[j++] = beta[i];
			}
		}
		int npts = zp.length;
		double fprime[][] = new double[npts][2];
		double terms[][][] = new double[numnotinfz][npts][2];
		for (i = 0; i < numnotinfz; i++) {
			for (j = 0; j < npts; j++) {
				terms[i][j][0] = zp[j][0] - zf[i][0];
				terms[i][j][1] = zp[j][1] - zf[i][1];
			}
		}
		for (j = 0; j < npts; j++) {
			double sumr = 0.0;
			double sumi = 0.0;
			for (i = 0; i < numnotinfz; i++) {
				double logr = Math.log(scm.zabs(terms[i][j][0], terms[i][j][1]));
				double logi = Math.atan2(terms[i][j][1], terms[i][j][0]);
				double prodr = logr * betaf[i];
				double prodi = logi * betaf[i];
				sumr += prodr;
				sumi += prodi;
			} // for (i = 0; i < numnotinfz; i++)
			double expb = Math.exp(sumr);
			double expr = expb * Math.cos(sumi);
			double expi = expb * Math.sin(sumi);
			scm.zmlt(c[0], c[1], expr, expi, cr, ci);
			fprime[j][0] = cr[0];
			fprime[j][1] = ci[0];
		} // for (j = 0; j < npts; j++)
		
		return fprime;
	}
	
	public void stripplot(scmap M, double re[], double im[], int yInvert, boolean closed, double axis[]) {
		// Visualize a Schwarz-Christoffel strip map.
		// stripplot plots the polygon associated with the Schwarz-Christoffel
		// half-plane map M and the images of vertical lines whose real
		// parts are given in re and horizontal lines whose imaginary
		// parts are given in im under the S-C transformation.
		// From 1998 MATLAB plot routine copyright by Toby Driscoll.
		int i;
		polygon p = M.poly;
		double w[][] = p.vertex;
		double beta[] = new double[p.angle.length];
		for (i = 0; i < p.angle.length; i++) {
			beta[i] = p.angle[i] - 1;
		}
		double z[][] = M.prevertex;
		double c[] = M.constant;
		stplot(w, beta, z, c, re, im, yInvert, closed, axis);
	}
	
	public void stplot(double w[][], double beta[], double z[][], double c[],
			double re[], double im[], int yInvert, boolean closed, double axis[]) {
	    // Image of cartesian grid under Schwarz-Christoffel strip map.
		// stplot will adaptively plot the images under the Schwarz-Christoffel
		// exterior map of vertical lines whose real parts are given in re and
		// horizontal lines whose imaginary parts are given in im in the upper half-plane.
		//The abscissae of the vertical lines will bracket the finite extremes of real(Z).
		// From 1998 MATLAB stplot routine copyright by Toby Driscoll.
		int i, j, m;
		double tol;
		boolean haveInfiniteZ = false;
		double betan[];
		int n = w.length;
		double qdat[][] = null;
		double axlim[] = new double[4];
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
        Vector<Double>zpReal = new Vector<Double>();
		Vector<Double>zpImag = new Vector<Double>();
		Vector<Double>wpReal = new Vector<Double>();
		Vector<Double>wpImag = new Vector<Double>();
		Vector<Boolean>newlog = new Vector<Boolean>();
		Vector<Double> linhx[][] = null;
		Vector<Double> linhy[][] = null;
		Vector<Double>x1Vector = new Vector<Double>();
		Vector<Double>y1Vector = new Vector<Double>();
		Vector<Double>x2Vector = new Vector<Double>();
		Vector<Double>y2Vector = new Vector<Double>();
        
		// Zero arguments default to 10
		if (((re == null) || (re.length == 0)) && ((im == null) || (im.length == 0))) {
		    re = new double[]{10.0};
		    im = new double[]{10.0};
		}
		
		// Integer arguments must be converted to specific values
		double minre = Double.MAX_VALUE;
		double maxre = -Double.MAX_VALUE;
		for (i = 0; i < z.length; i++) {
			if (!Double.isInfinite(z[i][0])) {
				if (z[i][0] < minre) {
					minre = z[i][0];
				}
				if (z[i][0] > maxre) {
					maxre = z[i][0];
				}
			}
		}
		if ((re.length == 1) && (re[0] == Math.round(re[0]))) {
			if (re[0] < 1) {
			    re = null;
			}
			else if (re[0] < 2) {
				// Real parts are given in re.
				re[0] = (minre + maxre)/2.0;
			}
			else {
			    m = (int)re[0];	
			    double dre = (maxre - minre)/(m - 1.0);
			    double spacing = (maxre - minre + 2*dre)/(m - 1.0);
			    re = new double[m];
			    for (i = 0; i < m; i++) {
			    	re[i] = minre - dre + i*spacing;
			    }
			} // else 
		} // if ((re.length == 1) && (re[0] == Math.round(re[0])))
		if ((im.length == 1) & (im[0] == Math.round(im[0]))) {
			if (im[0] < 1) {
				im = null;
			}
			else {
				m = (int)im[0];
				double spacing = 1.0/(m + 1.0);
				im = new double[m];
				for (i = 1; i < m+1; i++) {
					im[i-1] = i*spacing;
				}
			}
		} // if ((im.length == 1) & (im[0] == Math.round(im[0])))
		boolean atinf[] = new boolean[w.length];
		int numinfinite = 0;
		int numfinite = 0;
		for (i = 0; i < w.length; i++) {
			if ((!Double.isInfinite(w[i][0])) && (!Double.isInfinite(w[i][1]))) {
				numfinite++;
			}
			else {
				numinfinite++;
				atinf[i] = true;
			}
		}
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
			return;
		}
		int renum[] = new int[n];
		for (i = 0; i < n - first; i++) {
			renum[i] = first +i;
		}
		for (i = 0; i < first; i++) {
			renum[n-first+i] = i;
		}
		double wrenum[][] = new double[n][2];
		for (i = 0; i < n; i++) {
			wrenum[i][0] = w[renum[i]][0];
			wrenum[i][1] = w[renum[i]][1];
		}
		float xPointArray[];
		float yPointArray[];
		
		// Put 2 finite points at every infinity in plotpoly
		// Unless last one in wrenum is infinite in which case add 3
		numinfinite = 0;
		for (i = 0; i < w.length; i++) {
		    if (Double.isInfinite(wrenum[i][0]) || Double.isInfinite(wrenum[i][1]))	{
		    	if (i == w.length-1) {
		    		numinfinite = numinfinite + 2;
		    	}
		    	else {
		            numinfinite++;	
		    	}
		    }
		}
		if (closed) {
		    xPointArray = new float[n+1+numinfinite];
		    yPointArray = new float[n+1+numinfinite];
		}
		else {
			xPointArray = new float[n+numinfinite];
			yPointArray = new float[n+numinfinite];
		}
		ViewJFrameGraph pointGraph = scm.plotpoly(xPointArray, yPointArray, w, beta, false, axlim, yInvert, closed, axis);
		ViewJComponentGraph graph = pointGraph.getGraph();
		Rectangle graphBounds = graph.getGraphBounds();
		Graphics g = graph.getGraphics();
		double xScale = graphBounds.width / (axlim[1] - axlim[0]);
        double yScale = graphBounds.height / (axlim[3] - axlim[2]);
        double len = Math.max(axlim[1] - axlim[0], axlim[3] - axlim[2]);
		minlen = len * minlen;
		maxlen = len * maxlen;
		tol = Math.pow(10.0, -nqpts);
		for (i = 0; i < z.length; i++) {
			if (Double.isInfinite(z[i][0]) || Double.isInfinite(z[i][1])) {
				haveInfiniteZ = true;
			}
		}
		qdat = new double[nqpts][2*beta.length+2];
		scm.scqdata(qdat, beta, nqpts);	
		
		
		// Plot vertical lines
		if (re != null) {
		linhx = new Vector[re.length][2];
		linhy = new Vector[re.length][2];
		for (i = 0; i < re.length; i++) {
			for (j = 0; j < 2; j++) {
				linhx[i][j] = new Vector<Double>();
				linhy[i][j] = new Vector<Double>();
			}
		}
		
		for (j = 0; j < re.length; j++) {
		    zpReal.clear();
		    zpImag.clear();
		    double spacing = 1.0/14.0;
		    for (i = 0; i < 15; i++) {
		    	zpReal.add(re[j]);
		    	zpImag.add(i*spacing);
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
				double neww[][] = scm.stmap(zpnew, w, beta, z, c, qdat);
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
				scm.scpadapt(zpReal, zpImag, wpReal, wpImag, newlog, minlen, maxlen, axlim);
		    } // while (iter < maxrefn)
		} // for (j = 0; j < re.length; j++)
		
		for (i = 0; i < re.length; i++) {
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
		} // if (re != null)
		
		// Plot horizontal lines
		if (im != null) {
		linhx = new Vector[im.length][2];
		linhy = new Vector[im.length][2];
		for (i = 0; i < im.length; i++) {
			for (j = 0; j < 2; j++) {
				linhx[i][j] = new Vector<Double>();
				linhy[i][j] = new Vector<Double>();
			}
		}
		
		double x1h = Math.min(-5, minre);
		double x2h = Math.max(5, maxre);
		double wl[] = new double[2]; // image of left end
		double wr[] = new double[2]; // image of right end
		for (i = 0; i < z.length; i++) {
			if (Double.isInfinite(z[i][0])) {
				if (z[i][0] < 0) {
					wl[0] = w[i][0];
					wl[1] = w[i][1];
				}
				else if (z[i][0] > 0) {
					wr[0] = w[i][0];
					wr[1] = w[i][1];
				}
			}
		}
		for (j = 0; j < im.length; j++) {
		    // Start evenly spaced
			zpReal.clear();
			zpImag.clear();
			//zpReal.add(Double.NEGATIVE_INFINITY);
			//zpImag.add(im[j]);
			for (i = 0; i < 15; i++) {
				zpReal.add(x1h + i*(x2h-x1h)/14.0);
				zpImag.add(im[j]);
			}
			//zpReal.add(Double.POSITIVE_INFINITY);
			//zpImag.add(im[j]);
			newlog.clear();
			//newlog.add(false);
		    for (i = 0; i < 15; i++) {
		    	newlog.add(true);
		    }
		    //newlog.add(false);
		    wpReal.clear();
		    wpImag.clear();
		    //wpReal.add(wl[0]);
		    //wpImag.add(wl[1]);
		    for (i = 0; i < 15; i++) {
		    	wpReal.add(Double.NaN);
		    	wpImag.add(0.0);
		    }
		    //wpReal.add(wr[0]);
		    //wpImag.add(wr[1]);
		    
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
 				double neww[][] = scm.stmap(zpnew, w, beta, z, c, qdat);
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
 				scm.scpadapt(zpReal, zpImag, wpReal, wpImag, newlog, minlen, maxlen, axlim);
 		    } // while (iter < maxrefn)
		} // for (j = 0; j < im.length; j++)
		
		for (i = 0; i < im.length; i++) {
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
		} // if (im != null)
		
		graph.setX1Vector(x1Vector);
		graph.setY1Vector(y1Vector);
		graph.setX2Vector(x2Vector);
		graph.setY2Vector(y2Vector);
		graph.setAddSchwarzChristoffelLines(true);
		graph.paintComponent(g);
	}
	
	public void hplot(scmap M, double re[], double im[], int yInvert, boolean closed, double axis[]) {
		// Visualize a Schwarz-Christoffel half-plane map.
		// hplot plots the polygon associated with the Schwarz-Christoffel
		// half-plane map M and the images of vertical lines whose real
		// parts are given in re and horizontal lines whose imaginary
		// parts are given in im under the S-C transformation.
		// From 1998 MATLAB plot routine copyright by Toby Driscoll.
		int i;
		polygon p = M.poly;
		double w[][] = p.vertex;
		double beta[] = new double[p.angle.length];
		for (i = 0; i < p.angle.length; i++) {
			beta[i] = p.angle[i] - 1;
		}
		double z[][] = M.prevertex;
		double c[] = M.constant;
		hpplot(w, beta, z, c, re, im, yInvert, closed, axis);
	}
	
	public void hpplot(double w[][], double beta[], double z[][], double c[],
			double re[], double im[], int yInvert, boolean closed, double axis[]) {
	    // Image of cartesian grid under Schwarz-Christoffel half-plane map.
		// hpplot will adaptively plot the images under the Schwarz-Christoffel
		// exterior map of vertical lines whose real parts are given in re and
		// horizontal lines whose imaginary parts are given in im.
		// From 1998 MATLAB hpplot routine copyright by Toby Driscoll.
		int i, j, m;
		double tol;
		boolean haveInfiniteZ = false;
		double betan[];
		int n = w.length;
		double qdat[][] = null;
		double axlim[] = new double[4];
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
        Vector<Double>zpReal = new Vector<Double>();
		Vector<Double>zpImag = new Vector<Double>();
		Vector<Double>wpReal = new Vector<Double>();
		Vector<Double>wpImag = new Vector<Double>();
		Vector<Boolean>newlog = new Vector<Boolean>();
        
		// Zero arguments default to 10
		if (((re == null) || (re.length == 0)) && ((im == null) || (im.length == 0))) {
		    re = new double[]{10.0};
		    im = new double[]{10.0};
		}
		
		// Integer arguments must be converted to specific values
		if ((re.length == 1) && (re[0] == Math.round(re[0]))) {
			if (re[0] < 1) {
			    re = null;
			}
			else if (re[0] < 2) {
				// Real parts are given in re.
				re[0] = (z[0][0] + z[n-2][0])/2.0;
			}
			else {
			    m = (int)re[0];	
			    double dre = (z[n-2][0] - z[0][0])/(m - 1.0);
			    double spacing = (z[n-2][0] - z[0][0] + 2*dre)/(m - 1.0);
			    re = new double[m];
			    for (i = 0; i < m; i++) {
			    	re[i] = z[0][0] - dre + i*spacing;
			    }
			} // else 
		} // if ((re.length == 1) && (re[0] == Math.round(re[0])))
		if ((im.length == 1) & (im[0] == Math.round(im[0]))) {
			if (re.length < 2) {
				double spacing = 4/im[0];
				int numlines = (int)im[0];
				im = new double[numlines];
				for (i = 0; i < numlines; i++) {
					im[i] = (i+1)*spacing;
				}
			} // if (re.length < 2)
			else {
				int numlines = (int)im[0];
				double diffre;
				double sumdiff = 0.0;
				for (i = 0; i < re.length-1; i++) {
					diffre = re[i+1] - re[i];
					sumdiff = sumdiff + diffre;
				}
				double meandiff = sumdiff/(re.length-1);
				im = new double[numlines];
				for (i = 1; i <= numlines; i++) {
					im[i-1] = i*meandiff;
				}
			} // else
		} // if ((im.length == 1) & (im[0] == Math.round(im[0])))
		float xPointArray[];
		float yPointArray[];
		int numinfinite = 0;
		// Put 2 finite points at every infinity in plotpoly
		// Unless last one is infinite in which case add 3
		for (i = 0; i < w.length; i++) {
		    if (Double.isInfinite(w[i][0]) || Double.isInfinite(w[i][1]))	{
		    	if (i == w.length-1) {
		    		numinfinite = numinfinite + 2;
		    	}
		    	else {
		            numinfinite++;	
		    	}
		    }
		}
		if (closed) {
		    xPointArray = new float[n+1+numinfinite];
		    yPointArray = new float[n+1+numinfinite];
		}
		else {
			xPointArray = new float[n+numinfinite];
			yPointArray = new float[n+numinfinite];
		}
		ViewJFrameGraph pointGraph = scm.plotpoly(xPointArray, yPointArray, w, beta, false, axlim, yInvert, closed, axis);
		ViewJComponentGraph graph = pointGraph.getGraph();
		Rectangle graphBounds = graph.getGraphBounds();
		Graphics g = graph.getGraphics();
		double xScale = graphBounds.width / (axlim[1] - axlim[0]);
        double yScale = graphBounds.height / (axlim[3] - axlim[2]);
        double len = Math.max(axlim[1] - axlim[0], axlim[3] - axlim[2]);
		minlen = len * minlen;
		maxlen = len * maxlen;
		tol = Math.pow(10.0, -nqpts);
		for (i = 0; i < z.length; i++) {
			if (Double.isInfinite(z[i][0]) || Double.isInfinite(z[i][1])) {
				haveInfiniteZ = true;
			}
		}
		if (haveInfiniteZ) {
		    betan = new double[n-1];
		    for (i = 0; i < n-1; i++) {
		    	betan[i] = beta[i];
		    }
		    qdat = new double[nqpts][2*betan.length+2];
			scm.scqdata(qdat, betan, nqpts);
		} // if (haveInfiniteZ)
		else {
			qdat = new double[nqpts][2*beta.length+2];
			scm.scqdata(qdat, beta, nqpts);	
		}
		
		// Plot vertical lines
		double y2d = Math.max(z[n-2][0], 10.0);
		Vector<Double> linhx[][] = new Vector[re.length][2];
		Vector<Double> linhy[][] = new Vector[re.length][2];
		Vector<Double>x1Vector = new Vector<Double>();
		Vector<Double>y1Vector = new Vector<Double>();
		Vector<Double>x2Vector = new Vector<Double>();
		Vector<Double>y2Vector = new Vector<Double>();
		for (i = 0; i < re.length; i++) {
			for (j = 0; j < 2; j++) {
				linhx[i][j] = new Vector<Double>();
				linhy[i][j] = new Vector<Double>();
			}
		}
		
		for (j = 0; j < re.length; j++) {
		    zpReal.clear();
		    zpImag.clear();
		    double spacing = y2d/14.0;
		    for (i = 0; i < 15; i++) {
		    	zpReal.add(re[j]);
		    	zpImag.add(i*spacing);
		    }
		    //zpReal.add(re[j]);
		    //zpImag.add(Double.POSITIVE_INFINITY);
		    newlog.clear();
		    for (i = 0; i < 15; i++) {
		    	newlog.add(true);
		    }
		    //newlog.add(false);
		    wpReal.clear();
		    wpImag.clear();
		    for (i = 0; i < 15; i++) {
		    	wpReal.add(Double.NaN);
		    
		    	wpImag.add(0.0);
		    }
		    //wpReal.add(w[n-1][0]);
		   // wpImag.add(w[n-1][1]);
		    
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
				scm.hpmap(neww, zpnew, w, beta, z, c, qdat);
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
				scm.scpadapt(zpReal, zpImag, wpReal, wpImag, newlog, minlen, maxlen, axlim);
		    } // while (iter < maxrefn)
		} // for (j = 0; j < re.length; j++)
		
		for (i = 0; i < re.length; i++) {
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
		linhx = new Vector[im.length][2];
		linhy = new Vector[im.length][2];
		for (i = 0; i < im.length; i++) {
			for (j = 0; j < 2; j++) {
				linhx[i][j] = new Vector<Double>();
				linhy[i][j] = new Vector<Double>();
			}
		}
		
		double z1 = Math.min(-10, z[0][0]);
		double z2 = Math.max(40.0, z[n-2][0]);
		for (j = 0; j < im.length; j++) {
		    // Start evenly spaced
			zpReal.clear();
			zpImag.clear();
			//zpReal.add(Double.NEGATIVE_INFINITY);
			//zpImag.add(im[j]);
			for (i = 0; i < 15; i++) {
				zpReal.add(z1 + i*(z2-z1)/14.0);
				zpImag.add(im[j]);
			}
			//zpReal.add(Double.POSITIVE_INFINITY);
			//zpImag.add(im[j]);
			newlog.clear();
			//newlog.add(false);
		    for (i = 0; i < 15; i++) {
		    	newlog.add(true);
		    }
		    //newlog.add(false);
		    wpReal.clear();
		    wpImag.clear();
		    //wpReal.add(w[n-1][0]);
		    //wpImag.add(w[n-1][1]);
		    for (i = 0; i < 15; i++) {
		    	wpReal.add(Double.NaN);
		    	wpImag.add(0.0);
		    }
		    //wpReal.add(w[n-1][0]);
		    //wpImag.add(w[n-1][1]);
		    
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
 				scm.hpmap(neww, zpnew, w, beta, z, c, qdat);
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
 				scm.scpadapt(zpReal, zpImag, wpReal, wpImag, newlog, minlen, maxlen, axlim);
 		    } // while (iter < maxrefn)
		} // for (j = 0; j < im.length; j++)
		
		for (i = 0; i < im.length; i++) {
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
	
	

	public scmap hplmap(polygon poly, double zin[][], double alphain[], double c[]) {
		// hplmap constructs a Schwarz-Christoffel half-plane object for the
		// polygon poly.
		// The parameter problem is solved using default options for the
		// prevertices and
		// the multiplicative constant.
		// Derived from original hlpmap MATLAB routine copyright 1998-2001 by
		// Toby Driscoll.
		int i, j;
		double wn[][];
		double betan[];
		double alpha2[];
		int nqpts;
		double qdata[][] = null;
		double z0[] = null;
		double z[][] = null;
		double alpha[] = null;
		double beta[] = null;
		double cr[] = new double[1];
		double ci[] = new double[1];
		if (zin != null) {
			z = new double[zin.length][2];
			for (i = 0; i < zin.length; i++) {
				z[i][0] = zin[i][0];
				z[i][1] = zin[i][1];
			}
		} // if (zin != null)
		if (alphain != null) {
			alpha = new double[alphain.length];
			for (i = 0; i < alphain.length; i++) {
				alpha[i] = alphain[i];
			}
		} // if (alphain != null)

		// Get data for the low-level functions

		scmap map = scm.new scmap();
		
		if ((z != null) && (z.length != 0) && (alpha != null) && (alpha.length != 0)) {
		    if ((!Double.isInfinite(z[z.length-1][0])) && (!Double.isInfinite(z[z.length-1][1]))) {
		    	double ztemp[][] = new double[z.length][2];
		    	for (i = 0; i < z.length; i++) {
		    		ztemp[i][0] = z[i][0];
		    		ztemp[i][1] = z[i][1];
		    	}
		    	z = null;
		    	z = new double[ztemp.length+1][2];
		    	for (i = 0; i < ztemp.length; i++) {
		    		z[i][0] = ztemp[i][0];
		    		z[i][1] = ztemp[i][1];
		    	}
		    	z[z.length-1][0] = Double.POSITIVE_INFINITY;
		    	z[z.length-1][1] = 0;
		    	double alphatemp[] = new double[alpha.length];
		    	for (i = 0; i < alpha.length; i++) {
		    		alphatemp[i] = alpha[i];
		    	}
		    	alpha = new double[alphatemp.length+1];
		    	for (i = 0; i < alphatemp.length; i++) {
		    		alpha[i] = alphatemp[i];
		    	}
		    	alpha[alpha.length-1] = 1;
		    } // if ((!Double.isInfinite(z[z.length-1][0])) && (!Double.isInfinite(z[z.length-1][1])))
		    // Nonsense vertices
		    double x[] = new double[alpha.length];
		    double y[] = new double[alpha.length];
		    for (i = 0; i < alpha.length; i++) {
		    	y[i] = Double.NaN;
		    }
		    poly = scm.new polygon(x, y, alpha);
		    if ((c == null) || (c.length == 0)) {
		    	c = new double[2];
		    	c[0] = 1;
		    	c[1] = 0;
		    }
		} // if ((z != null) && (z.length != 0) && (alpha != null) && (alpha.length != 0))

		if ((z == null) || (z.length == 0)) {
			double w[][] = poly.vertex;
			beta = new double[poly.angle.length];
			for (i = 0; i < poly.angle.length; i++) {
				beta[i] = poly.angle[i] - 1.0;
			}
			wn = new double[w.length + 2][2];
			betan = new double[w.length + 2];
			// Number of vertices added by scfix
			int verticesAdded[] = new int[1];
			int initialVertices = w.length;
			for (i = 0; i < w.length; i++) {
				wn[i][0] = w[i][0];
				wn[i][1] = w[i][1];
				betan[i] = beta[i];
			}
			scm.scfix(wn, betan, verticesAdded, null, "hp", w, beta, null);
			double wn2[][];
			double betan2[];
			if ((verticesAdded[0] == 0) || (verticesAdded[0] == 1)) {
				wn2 = new double[initialVertices + verticesAdded[0]][2];
				betan2 = new double[initialVertices + verticesAdded[0]];
				for (i = 0; i < initialVertices + verticesAdded[0]; i++) {
					wn2[i][0] = wn[i][0];
					wn2[i][1] = wn[i][1];
					betan2[i] = betan[i];
				}
			} else {
				wn2 = wn;
				betan2 = betan;
			}
			double xn[] = new double[wn2.length];
			double yn[] = new double[wn2.length];
			for (i = 0; i < wn2.length; i++) {
				xn[i] = wn2[i][0];
				yn[i] = wn2[i][1];
			}
			alpha2 = new double[betan2.length];
			for (i = 0; i < betan2.length; i++) {
				alpha2[i] = betan2[i] + 1.0;
			}
			poly = scm.new polygon(xn, yn, alpha2);
			c = new double[2];
			z = new double[wn2.length][2];
			nqpts = Math.max((int) Math.ceil(-Math.log10(tolerance)), 4);
			// last beta not used in this qdata
			qdata = new double[nqpts][2 * (wn2.length-1) + 2];
			hpparam(z, c, qdata, wn2, betan2, z0, tolerance);
		} // if ((z == null) || (z.length == 0))
		
		if ((qdata == null) || (qdata.length == 0)) {
		    nqpts = (int) Math.ceil(-Math.log10(tolerance));
		    alpha = poly.angle;
		    betan = new double[alpha.length-1];
		    for (i = 0; i < alpha.length-1; i++) {
		    	betan[i] = alpha[i] - 1;
		    }
		    qdata = new double[nqpts][2 * betan.length + 2];
		    scm.scqdata(qdata, betan, nqpts);
		} // if ((qdata == null) || (qdata.length == 0))
		
		if ((c == null) || (c.length == 0)) {
		    w = poly.vertex;
		    beta = new double[poly.angle.length];
		    for (i = 0; i < poly.angle.length; i++) {
		        beta[i] = poly.angle[i] - 1.0;
		    }
		    int idx = 0;
		    for (i = 1; (i < z.length) && (idx == 0); i++) {
		        if ((!Double.isInfinite(z[i][0])) && (!Double.isInfinite(z[i][1])))	{
		        	idx = i;
		        }
		    }
		    double mid[][] = new double[1][2];
		    mid[0][0] = (z[0][0] + z[idx][0])/2.0;
		    mid[0][1] = (z[idx][0] - z[0][0])/2.0;
		    double zn[][] = new double[z.length-1][2];
		    for (i = 0; i < z.length-1; i++) {
		    	zn[i][0] = z[i][0];
		    	zn[i][1] = z[i][1];
		    }
		    betan = new double[beta.length-1];
		    for (i = 0; i < beta.length-1; i++) {
		    	betan[i] = beta[i];
		    }
		    double z00[][] = new double[1][2];
		    z00[0][0] = z[0][0];
		    z00[0][1] = z[0][1];
		    int sing0[] = new int[]{0};
		    double zidx[][] = new double[1][2];
		    zidx[0][0] = z[idx][0];
		    zidx[0][1] = z[idx][1];
		    int singidx[] = new int[]{idx};
		    double I1[][] = scm.hpquad(z00, mid, sing0, zn, betan, qdata);
		    double I2[][] = scm.hpquad(zidx, mid, singidx, zn, betan, qdata);
		    double I[] = new double[2];
		    I[0] = I1[0][0] - I2[0][0];
		    I[1] = I1[0][1] - I2[0][1];
		    double diff[] = new double[2];
		    diff[0] = w[idx][0] - w[0][0];
		    diff[1] = w[idx][1] - w[0][1];
		    scm.zdiv(diff[0], diff[1], I[0], I[1], cr, ci);
		    c = new double[2];
		    c[0] = cr[0];
		    c[1] = ci[0];
		} // if ((c == null) || (c.length == 0))
		boolean unknownpoly = false;
		w = poly.vertex;
		for (i = 0; i < w.length; i++) {
		    if (Double.isNaN(w[i][0]) || Double.isNaN(w[i][1])) {
		    	unknownpoly = true;
		    }
		}
		
		map.poly = poly;
		map.prevertex = z;
		map.constant = c;
		map.qdata = qdata;
		if (unknownpoly) {
			// If the polygon was not known, find it from the map
			map.poly = forwardpoly(map);
		}

		// Now fill in apparent accuracy
		map.accuracy = hpaccuracy(map);
		return map;
	}
	
	private polygon forwardpoly(scmap map) {
		// Given a hplmap map, forwardpoly returns the polygon thet is 
		// formed using the prevertices, angles, and quadrature data of that
		// map.  If the prevertices were found from the solution of a
		// parameter problem, then the result should agree closely with the
		// original polygon that was supplied.
		// Original MATLAB forwardpoly routine copyright 1998 by Toby Driscoll.
		int i, j, k;
		double cr[] = new double[1];
		double ci[] = new double[1];
		polygon p = null;
		double qdata[][] = null;
		double z[][] = map.prevertex;
		double alpha[] = map.poly.angle;
		double c[] = map.constant;
		
		int n = z.length;
		
		// Since there is no parameter problem, use high accuracy in quadrature.
		double betan[] = new double[n-1];
		for (i = 0; i < n-1; i++) {
			betan[i] = alpha[i] - 1.0;
		}
		int nqpts = 16;
		qdata = new double[nqpts][2 * betan.length + 2];
		scm.scqdata(qdata, betan, nqpts);
		
		double w[][] = new double[n][2];
		boolean atinf[] = new boolean[n];
		int numinf = 0;
		for (i = 0; i < n; i++) {
			atinf[i] = (alpha[i] < eps);
			if (atinf[i] ) {
				w[i][0] = Double.POSITIVE_INFINITY;
				numinf++;
			}
		} // for (i = 0; i < n; i++)
		int numnotinf = n - numinf;
		
		// Endpoints of integrations.  Because the last preverrtes is at infinity, we
		// shouldn't try to integrate there.
		int idx[] = new int[numnotinf];
		for (i = 0, j = 0; i < n; i++) {
			if (!atinf[i]) {
			    idx[j++] = i;	
			}
		}
		if (idx[idx.length-1] == n-1) {
		    int idxtemp[] = new int[idx.length-1];
		    for (i = 0; i < idx.length-1; i++) {
		    	idxtemp[i] = idx[i];
		    }
		    idx = new int[idxtemp.length];
		    for (i = 0; i < idxtemp.length; i++) {
		    	idx[i] = idxtemp[i];
		    }
		} // if (idx[idx.length-1] == n-1)
		int endpt[][] = new int[idx.length-1][2];
		for (i = 0; i < idx.length-1; i++) {
			endpt[i][0] = idx[i];
			endpt[i][1] = idx[i+1];
		}
		
		// Edge endpoint prevertices.  z(endpt) will not work if endpt has just one
		// row, since shape will not be preserved.
		double ze[][][] = new double[endpt.length][2][2];
		for (i = 0; i < endpt.length; i++) {
			for (j = 0; j < 2; j++) {
			 for (k = 0; k < 2; k++) {
				 ze[i][j][k] = z[endpt[i][j]][k];
			 }
			}
		}
		
		// Midpoints are in upper half-plane.  Always make 45 degrees with 
		// real line.
		double mid[][] = new double[endpt.length][2];
		for (i = 0; i < endpt.length; i++) {
			mid[i][0] = (ze[i][0][0] + ze[i][1][0])/2.0;
			mid[i][1] = (ze[i][1][1] - ze[i][0][1])/2.0;
		}
		
		// Integrations
		double ze0[][] = new double[ze.length][2];
		double ze1[][] = new double[ze.length][2];
		for (i = 0; i < ze.length; i++) {
			for (j = 0; j < 2; j++) {
				ze0[i][j] = ze[i][0][j];
				ze1[i][j] = ze[i][1][j];
			}
		}
		int endpt0[] = new int[endpt.length];
		int endpt1[] = new int[endpt.length];
		for (i = 0; i < endpt.length; i++) {
			endpt0[i] = endpt[i][0];
			endpt1[i] = endpt[i][1];
		}
		double zn[][] = new double[n-1][2];
		for (i = 0; i < n-1; i++) {
		    zn[i][0] = z[i][0];
		    zn[i][1] = z[i][1];
		}
		double I1[][] = scm.hpquad(ze0, mid, endpt0, zn, betan, qdata);
		double I2[][] = scm.hpquad(ze1, mid, endpt1, zn, betan, qdata);
		double I[][] = new double[ze.length][2];
		for (i = 0; i < ze.length; i++) {
			I[i][0] = I1[i][0] - I2[i][0];
			I[i][1] = I1[i][1] - I2[i][1];
		}
		
		// Deduce vertices
		double cs[][] = new double[idx.length][2];
		cs[0][0] = 0;
		cs[0][1] = 0;
		for (i = 0; i < idx.length-1; i++) {
			cs[i+1][0] = cs[i][0] + I[i][0];
			cs[i+1][1] = cs[i][1] + I[i][1];
		}
		double ccs[][] = new double[idx.length][2];
		for (i = 0; i < idx.length; i++) {
			scm.zmlt(c[0], c[1], cs[i][0], cs[i][1], cr, ci);
			ccs[i][0] = cr[0];
			ccs[i][1] = ci[0];
		}
		for (i = 0; i < idx.length; i++) {
			w[idx[i]][0] = ccs[i][0];
			w[idx[i]][1] = ccs[i][1];
		}
		
		// Get the last vertex via intersection
		if (alpha[n-1] > 0) {
		    if ((Math.abs(alpha[n-1]-1) < 5.0*eps) || (Math.abs(alpha[n-1]-2) < 5.0*eps)) {
		    	MipavUtil.displayError("Cannot deduce last vertex when its adjacent sides are collinear");
		    	System.exit(-1);
		    }
		    else if (atinf[0] || atinf[1] || atinf[n-2]) {
		    	MipavUtil.displayError("Vertices 1, 2, and end-1 must be finite.");
		    	System.exit(-1);
		    }
		    else {
		    	// Here's the direction from w[0]
		    	double diff[] = new double[2];
		    	diff[0] = w[1][0] - w[0][0];
		    	diff[1] = w[1][1] - w[0][1];
		    	double evar[] = new double[2];
		    	evar[0] = Math.cos(Math.PI*alpha[0]);
		    	evar[1] = Math.sin(Math.PI*alpha[0]);
		    	double d1[] = new double[2];
		    	scm.zmlt(diff[0], diff[1], evar[0], evar[1], cr, ci);
		    	d1[0] = cr[0];
		    	d1[1] = ci[0];
		    	// Get the direction from w[n-2]
		    	double sum = 0.0;
		    	for (i = 1; i < n-1; i++) {
		    		sum += Math.PI*(1.0 - alpha[i]);
		    	}
		    	double d2[] = new double[2];
		    	d2[0] = Math.atan2(diff[1], diff[0]) + sum;
		    	d2[1] = Math.sin(d2[0]);
		    	d2[0] = Math.cos(d2[0]);
		    	double b[] = new double[2];
		    	b[0] = w[n-2][0] - w[0][0];
		    	b[1] = w[n-2][1] - w[0][1];
		    	// 2 x 2 matrix is:
		    	// d1[0]    -d2[0]
		    	// d1[1]    -d2[1]
		    	double det = -d1[0]*d2[1] + d1[1]*d2[0];
		    	// Inverse is:
		    	double a00 = -d2[1]/det;
		    	double a01 = d2[0]/det;
		    	//double a10 = -d1[1]/det;
		    	//double a11 = d1[0]/det;
		    	double s[] = new double[2];
		    	s[0] = a00*b[0] + a01*b[1];
		    	w[n-1][0] = w[0][0] + s[0]*d1[0];
		    	w[n-1][1] = w[0][1] + s[0]*d1[1];
		    }
		} // if (alpha[n-1] > 0)
		double x[] = new double[w.length];
		double y[] = new double[w.length];
		for (i = 0; i < w.length; i++) {
			x[i] = w[i][0];
			y[i] = w[i][1];
		}
		p = scm.new polygon(x, y, alpha);
		return p;
	}
	
	private double hpaccuracy(scmap M) {
		// Apparent accuracy of the Schwarz-Christoffel half-plane map.
		// hpaccuracy estimates the accuracy of the Schwarz-CZhristoffel half-plane
		// map M.  The technique used is to compare the differences between successive
		// finite vertices to the integral between the corresponding prevertices, and
		// return the maximum.
		
		// Original MATLAB accuracy routine copyright 1998 by Toby Driscoll.
		
		// If an accuracy has been assigned, don't question it.
		int i, j;
		double cre[] = new double[1];
		double cim[] = new double[1];
		double acc;
		if (!Double.isNaN(M.accuracy)) {
		    acc = M.accuracy;
		    return acc;
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
		double c[] = M.constant;
		double qdata[][] = M.qdata;
		
		// Test accuracy by integrating between consecutive finite prevertices, and
		// comparing to differences of vertices.
		// Exclude last prevertex at infinity.
		int numidx = 0;
		for(i = 0; i < n-1; i++) {
			if ((!Double.isInfinite(w[i][0]))  && (!Double.isInfinite(w[i][1]))) {
				numidx++;
			}
		}
		int idx[] = new int[numidx];
		for(i = 0, j = 0; i < n-1; i++) {
			if ((!Double.isInfinite(w[i][0]))  && (!Double.isInfinite(w[i][1]))) {
			    idx[j++] = i;
			}
		}
		
		// Two columns hold endpoint indices for integrations
		int idx2[][] = new int[numidx-1][2];
		for (i = 0; i < numidx-1; i++) {
			idx2[i][0] = idx[i];
			idx2[i][1] = idx[i+1];
		}
		
		// Find midpoints.  Go into upper half-plane to avoid integrating through
		// skipped prevertices.
		double zz[][][] = new double[2][numidx-1][2];
		for (i = 0; i < numidx-1; i++) {
			for (j = 0; j < 2; j++) {
				zz[j][i][0] = z[idx2[i][j]][0];
				zz[j][i][1] = z[idx2[i][j]][1];
			}
		}
		double mid[][] = new double[numidx-1][2];
		for (i = 0; i < numidx-1; i++) {
			mid[i][0] = (zz[0][i][0] + zz[1][i][0])/2.0;
			mid[i][1] = (zz[0][i][1] + zz[1][i][1])/2.0;
		}
		for (i = 0; i < numidx-1; i++) {
			mid[i][1] = mid[i][1] + scm.zabs(zz[1][i][0] - zz[0][i][0], zz[1][i][1] - zz[0][i][1])/2.0;
		}
		
		// Do the integrations
		double z0[][] = new double[numidx-1][2];
		double z1[][] = new double[numidx-1][2];
		for (i = 0; i < numidx-1; i++) {
			z0[i][0] = z[idx2[i][0]][0];
			z0[i][1] = z[idx2[i][0]][1];
			z1[i][0] = z[idx2[i][1]][0];
			z1[i][1] = z[idx2[i][1]][1];
		}
		int idx20[] = new int[numidx-1];
		int idx21[] = new int[numidx-1];
		for (i = 0; i < numidx-1; i++) {
			idx20[i] = idx2[i][0];
			idx21[i] = idx2[i][1];
		}
		double zn[][] = new double[n-1][2];
		double betan[] = new double[n-1];
		for (i = 0; i < n-1; i++) {
			zn[i][0] = z[i][0];
			zn[i][1] = z[i][1];
			betan[i] = beta[i];
		}
		double I1[][] = scm.hpquad(z0, mid, idx20, zn, betan, qdata);
		double I2[][] = scm.hpquad(z1, mid, idx21, zn, betan, qdata);
		double I[][] = new double[numidx-1][2];
		for (i = 0; i < numidx-1; i++) {
			I[i][0] = I1[i][0] - I2[i][0];
			I[i][1] = I1[i][1] - I2[i][1];
		}
		double cI[][] = new double[numidx-1][2];
		for (i = 0; i < numidx-1; i++) {
		    scm.zmlt(c[0], c[1], I[i][0], I[i][1], cre, cim);
		    cI[i][0] = cre[0];
		    cI[i][1] = cim[0];
		}
		double diffW[][] = new double[numidx-1][2];
		for (i = 0; i < numidx-1; i++) {
			diffW[i][0] = w[idx2[i][1]][0] - w[idx2[i][0]][0];
			diffW[i][1] = w[idx2[i][1]][1] - w[idx2[i][0]][1];
		}
		double val[][] = new double[numidx-1][2];
		for (i = 0; i < numidx-1; i++) {
			val[i][0] = cI[i][0] - diffW[i][0];
			val[i][1] = cI[i][1] - diffW[i][1];
		}
		double absVal[] = new double[numidx-1];
		for (i = 0; i < numidx-1; i++) {
			absVal[i] = scm.zabs(val[i][0], val[i][1]);
		}
		acc = 0.0;
		for (i = 0; i < numidx-1; i++) {
			if (absVal[i] > acc) {
				acc = absVal[i];
			}
		}
		System.out.println("Accuracy = " + acc);
		return acc;
	}

	private void hpparam(double z[][], double c[], double qdat[][], double w[][], double beta[], double z0[],
			double tol) {
		// hpparam solves the Schwarz-Christoffel parameter problem with the
		// upper half-plane
		// as fundamental domain and the interior of the polygon as the target.
		// w must be a
		// vector of the vertices of the polygon, specified in counterclockwise
		// order. beta
		// is a vector of turning angles; see scangles. If successful, hpparam
		// will return z,
		// a vector of pre-images of w; c, the multiplicative constant of the
		// conformal map;
		// and qdat, an optional matrix of quadrature data used by some of the
		// other routines.
		// If z0 is supplied, it is used as an initial guess for z. hpparam
		// attempts to find
		// an answer within tolerance tol.
		// Original hpparam MATLAB routine copyright 1998-2001 by Toby Driscoll.
		int i, j, k;
		double cr[] = new double[1];
		double ci[] = new double[1];
		int n = w.length; // number of vertices

		// Check input data
		int err = scm.sccheck("hp", w, beta, null);
		if (err == -1) {
			return;
		}
		if (err == 1) {
			MipavUtil.displayError("Use scfix to make polygon obey requirements");
			return;
		}
		double betan[] = new double[n - 1];
		for (i = 0; i < n - 1; i++) {
			betan[i] = beta[i];
		}

		int nqpts = (int) Math.max(Math.ceil(-Math.log10(tol)), 4);
		scm.scqdata(qdat, betan, nqpts); // quadrature data

		boolean atinf[] = new boolean[beta.length];
		for (i = 0; i < beta.length; i++) {
			if (beta[i] <= -1) {
				atinf[i] = true;
			}
		} // for (i = 0; i < beta.length; i++)

		// Find prevertices (solve param problem)
		if (n == 3) {
			z[0][0] = -1;
			z[0][1] = 0;
			z[1][0] = 1;
			z[1][1] = 0;
			z[2][0] = Double.POSITIVE_INFINITY;
			z[2][1] = 0;
		} // if (n == 3)
		else { // n != 3
				// Set up normalized lengths for nonlinear equations:
				// indices of left and right integration endpoints
			int numleft = 0;
			for (i = 0; i < n - 2; i++) {
				if (!atinf[i]) {
					numleft++;
				}
			}
			int left[] = new int[numleft];
			for (i = 0, j = 0; i < n - 2; i++) {
				if (!atinf[i]) {
					left[j++] = i;
				}
			}
			// numright should equal numleft
			int numright = 0;
			for (i = 1; i < n - 1; i++) {
				if (!atinf[i]) {
					numright++;
				}
			}
			int right[] = new int[numright];
			for (i = 1, j = 0; i < n - 1; i++) {
				if (!atinf[i]) {
					right[j++] = i;
				}
			}
			boolean cmplx[] = new boolean[numleft];
			int numcmplx = 0;
			for (i = 0; i < numleft; i++) {
				cmplx[i] = ((right[i] - left[i]) == 2);
				if (cmplx[i]) {
					numcmplx++;
				}
			}
			int numnotcmplx = numleft - numcmplx;
			// Normalize lengths by w[1]-w[0]
			double denomR = w[1][0] - w[0][0];
			double denomI = w[1][1] - w[0][1];
			double nmlen[][] = new double[numleft][2];
			for (i = 0; i < numleft; i++) {
				scm.zdiv(w[right[i]][0] - w[left[i]][0], w[right[i]][1] - w[left[i]][1], denomR, denomI, cr, ci);
				nmlen[i][0] = cr[0];
				nmlen[i][1] = ci[0];
			}
			double nmlen2[] = new double[numnotcmplx + 2 * numcmplx];
			for (i = 0, j = 0, k = 0; i < numleft; i++) {
				if (!cmplx[i]) {
					nmlen2[j++] = scm.zabs(nmlen[i][0], nmlen[i][1]);
				} else {
					nmlen2[numnotcmplx + k] = nmlen[i][0];
					nmlen2[numnotcmplx + numcmplx + k] = nmlen[i][1];
					k++;
				}
			}
			// First entry is useless (=1)
			double nmlen3[] = new double[nmlen2.length - 1];
			for (i = 0; i < nmlen3.length; i++) {
				nmlen3[i] = nmlen2[i + 1];
			}

			// Set up initial guess
			if ((z0 == null) || (z0.length == 0)) {
				z0 = new double[n - 1];
				z0[0] = -1;
				z0[n - 2] = 1;
				for (i = 1; i < n - 2; i++) {
					z0[i] = -1 + i * 2.0 / (n - 2.0);
				}
			} // if ((z0 == null) || (z0.length == 0))
			else {
				double denom = z0[n - 2] - z0[0];
				for (i = 0; i < n - 2; i++) {
					z0[i] = 2.0 * z0[i] / denom;
				}
				for (i = 0; i < n - 2; i++) {
					z0[i] = z0[i] - z0[0] - 1.0;
				}
			}
			double y0[] = new double[n - 3];
			for (i = 0; i < n - 3; i++) {
				y0[i] = Math.log((z0[i + 1] - z0[i]) / (z0[i + 2] - z0[i + 1]));
			}

			// Solve nonlinear system of equations
			
			hppfun fm = new hppfun(y0, n, betan, nmlen3, left, right, cmplx, qdat);
			fm.driver();
			fm.dumpResults();
			int exitStatus = fm.getExitStatus();
			if (exitStatus < 0) {
				System.out.println("Error in NLConstrainedEngine during hpparam call to hppfun");
				scm.printExitStatus(exitStatus);
				System.exit(-1);
			}
			double y[] = fm.getParameters();
			
			// Convert y values to z
			double cumprod[] = new double[n-2];
			cumprod[0] = 1;
			for (i = 1; i <= n-3; i++) {
				cumprod[i] = cumprod[i-1]*Math.exp(-y[i-1]);
			}
			double cs[] = new double[n-2];
			cs[0] = cumprod[0];
			for (i = 1; i <= n-3; i++) {
				cs[i] = cs[i-1] + cumprod[i];
			}
			double flipcs[] = new double[n-2];
			flipcs[0] = cumprod[n-3];
			for (i = 1; i <= n-3; i++) {
				flipcs[i] = flipcs[i-1] + cumprod[n-3-i];
			}
			double flipflipcs[] = new double[n-2];
			for (i = 0; i <= n-3; i++) {
				flipflipcs[i] = flipcs[n-3-i];
			}
			z[0][0] = -flipflipcs[0];
			for (i = 1; i <= n-3; i++) {
				z[i][0] = cs[i-1] - flipflipcs[i];
			}
			z[n-2][0] = cs[n-3];
			for (i = 0; i < n-1; i++) {
				z[i][0] = z[i][0]/z[n-2][0];
			}
			z[n-1][0] = Double.POSITIVE_INFINITY;
		} // else n != 3
		
		// Determine multiplicative constant
		double mid[][] = new double[1][2];
		mid[0][0] = (z[0][0] + z[1][0])/2.0;
		mid[0][1] = (z[0][1] + z[1][1])/2.0;
		double zn[][] = new double[n-1][2];
		for (i = 0; i < n-1; i++) {
			zn[i][0] = z[i][0];
			zn[i][1] = z[i][1];
		}
		double z1[][] = new double[1][2];
		z1[0][0] = z[1][0];
		z1[0][1] = z[1][1];
		double z00[][] = new double[1][2];
		z00[0][0] = z[0][0];
		z00[0][1] = z[0][1];
		int sing1[] = new int[]{1};
		int sing0[] = new int[]{0};
		double I1[][] = scm.hpquad(z1, mid, sing1, zn, betan, qdat);
		double I2[][] = scm.hpquad(z00, mid, sing0, zn, betan, qdat);
		double g[] = new double[2];
		g[0] = I1[0][0] - I2[0][0];
		g[1] = I1[0][1] - I2[0][1];
		scm.zdiv(w[0][0] - w[1][0], w[0][1] - w[1][1], g[0], g[1], cr, ci);
		c[0] = cr[0];
		c[1] = ci[0];
	}

	class hppfun extends NLConstrainedEngine {
		int n;
		double beta[];
		double nmlen[];
		int left[];
		int right[];
		boolean cmplx[];
		double qdat[][];

		public hppfun(double y0[], int n, double beta[], double nmlen[], int left[], int right[], boolean cmplx[],
				double qdat[][]) {
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
			Preferences.debug(" ******* Fit Elsunc Schwarz-Christoffel hpparam ********* \n\n",
					Preferences.DEBUG_ALGORITHM);
			Preferences.debug("Number of iterations: " + String.valueOf(iters) + "\n", Preferences.DEBUG_ALGORITHM);
			Preferences.debug("Chi-squared: " + String.valueOf(getChiSquared()) + "\n", Preferences.DEBUG_ALGORITHM);
			for (int i = 0; i < a.length; i++) {
				Preferences.debug("a" + i + " " + String.valueOf(a[i]) + "\n", Preferences.DEBUG_ALGORITHM);
			}
		}

		public void fitToFunction(double[] a, double[] residuals, double[][] covarMat) {
    		int ctrl;
    		int i, j;
    		double z[][];
    		double I1[][];
    		double I2[][];
    		double cr[] = new double[1];
    		double ci[] = new double[1];
    		try {
				ctrl = ctrlMat[0];

				if ((ctrl == -1) || (ctrl == 1)) {
                    // Returns residual for solution of nonlinear equations
					
					// Convert a values (unconstrained variables) to z (prevertices)
					// n-3 y0, so n-2 cumprod, so n-1 z
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
					double flipcs[] = new double[a.length+1];
					flipcs[0] = cumprod[a.length];
					for (i = 1; i <= a.length; i++) {
						flipcs[i] = flipcs[i-1] + cumprod[a.length-i];
					}
					double flipflipcs[] = new double[a.length+1];
					for (i = 0; i <= a.length; i++) {
						flipflipcs[i] = flipcs[a.length-i];
					}
					z = new double[a.length+2][2];
					z[0][0] = -flipflipcs[0];
					for (i = 1; i <= a.length; i++) {
						z[i][0] = cs[i-1] - flipflipcs[i];
					}
					z[a.length+1][0] = cs[a.length];
					for (i = 0; i < n-1; i++) {
						z[i][0] = z[i][0]/z[n-2][0];
					}
					
					// Compute the integrals
					double zleft[][] = new double[left.length][2];
					for (i = 0; i < left.length; i++) {
						zleft[i][0] = z[left[i]][0];	
					}
					double zright[][] = new double[right.length][2];
					for (i = 0; i < right.length; i++) {
						zright[i][0] = z[right[i]][0];	
					}
					double mid[][] = new double[left.length][2];
				    for (i = 0; i < left.length; i++) {
				    	mid[i][0] = (zleft[i][0] + zright[i][0])/2.0;
				    }
				    // For integrals between nonadjacent singularities, choose intermiediate
				    // points in the upper half-plane
				    int numcmplx = 0;
				    for (i = 0; i < cmplx.length; i++) {
				        if (cmplx[i]) {
				        	numcmplx++;
				        	mid[i][1] = (zright[i][0] - zleft[i][0])/2.0;
				        }
				    } // for (i = 0; i < cmplx.length; i++)
				    int numnotcmplx = cmplx.length - numcmplx;
				    I1 =scm. hpquad(zleft, mid, left, z, beta, qdat);
				    I2 = scm.hpquad(zright, mid, right, z, beta, qdat);
				    double ints[][] = new double[left.length][2];
				    int numintszero = 0;
				    for (i = 0; i < left.length; i++) {
				    	ints[i][0] = I1[i][0] - I2[i][0];
				    	ints[i][1] = I1[i][1] - I2[i][1];
				    	if ((ints[i][0] == 0) && (ints[i][1] == 0)) {
				        	numintszero++;
				        }
				    }
				    
				    if (numintszero > 0) {
				    	// Singularities were too crowded in practice
				    	MipavUtil.displayWarning("Severe crowding");
				    }
				    
				    // Compute nonlinear equation residual values.
				    double F1[] = new double[numnotcmplx];
				    for (i = 0, j = 0; i < cmplx.length; i++) {
				    	if (!cmplx[i]) {
				    		F1[j++] = scm.zabs(ints[i][0], ints[i][1]);
				    	}
				    }
				    double F11[] = new double[numnotcmplx-1];
				    for (i = 1; i < numnotcmplx; i++) {
				    	F11[i-1] = F1[i]/F1[0];
				    }
				    double F2[][] = new double[numcmplx][2];
				    double denomR = ints[0][0];
				    double denomI = ints[0][1];
				    for (i = 0, j = 0; i < cmplx.length; i++) {
				    	if (cmplx[i]) {
				    	    scm.zdiv(ints[i][0], ints[i][1], denomR, denomI, cr, ci);
				    	    F2[j][0] = cr[0];
				    	    F2[j++][1] = ci[0];
				    	}
				    }
				    double F[] = new double[numnotcmplx-1 + 2*numcmplx];
				    for (i = 0; i < numnotcmplx-1; i++) {
				    	F[i] = F11[i];
				    }
				    for (i = 0; i < numcmplx; i++) {
				    	F[numnotcmplx-1+i] = F2[i][0];
				    	F[numnotcmplx-1+numcmplx+i] = F2[i][1];
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

	
	
	class stpfun extends NLConstrainedEngine {
		int n;
		int nb;
		double beta[];
		double nmlen[][];
		int left[];
		int right[];
		boolean cmplx[];
		double qdat[][];

		public stpfun(double y0[], int n, int nb, double beta[], double nmlen[][], int left[], int right[], boolean cmplx[],
				double qdat[][]) {
			// nPoints, params
			super(y0.length, y0.length);
			this.n = n;
			this.nb = nb;
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
			Preferences.debug(" ******* Fit Elsunc Schwarz-Christoffel stparam ********* \n\n",
					Preferences.DEBUG_ALGORITHM);
			Preferences.debug("Number of iterations: " + String.valueOf(iters) + "\n", Preferences.DEBUG_ALGORITHM);
			Preferences.debug("Chi-squared: " + String.valueOf(getChiSquared()) + "\n", Preferences.DEBUG_ALGORITHM);
			for (int i = 0; i < a.length; i++) {
				Preferences.debug("a" + i + " " + String.valueOf(a[i]) + "\n", Preferences.DEBUG_ALGORITHM);
			}
		}

		public void fitToFunction(double[] a, double[] residuals, double[][] covarMat) {
    		int ctrl;
    		int i, j;
    		double z[][];
    		double I1[][];
    		double I2[][];
    		double rat1[] = null;
    		double rat2[][] = null;
    		double F1[] = null;
    		double F2[][] = null;
    		double cr[] = new double[1];
    		double ci[] = new double[1];
    		double ints[][] = null;
    		try {
				ctrl = ctrlMat[0];

				if ((ctrl == -1) || (ctrl == 1)) {
                    // Returns residual for solution of nonlinear equations
					
					// In this function, n refers to the number of finite prevertices.
					
					// Transform a (unconstrained variables) to z (actual parameters)
					z  = new double[n][2];
					for (i = 1; i < nb; i++) {
						z[i][0] = z[i-1][0] + Math.exp(a[i-1]);
					}
					z[nb][0] = a[nb-1];
					z[nb][1] = 1.0;
					for (i = nb+1; i < n; i++) {
						z[i][0] = z[i-1][0] -Math.exp(a[i-1]);
						z[i][1] = 1.0;
					}
					
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
					double mid[][] = new double[left.length][2];
					for (i = 0; i < left.length; i++) {
						mid[i][0] = (zleft[i][0] + zright[i][0])/2.0;
						mid[i][1] = (zleft[i][1] + zright[i][1])/2.0;
					}
					boolean c2[] = new boolean[cmplx.length];
					for (i = 0; i < cmplx.length; i++) {
						c2[i] = cmplx[i];
					}
					c2[1] = false;
					for (i = 0; i < c2.length; i++) {
						if (c2[i]) {
						    double sgn = scm.sign(left[i] - (nb-1));
						    mid[i][1] = mid[i][1] - sgn/2.0;
						}
					} // for (i = 0; i < c2.length; i++)
					
					// Add ends of strip to z, and modify singularity indices
					double zs[][] = new double[n+2][2];
					zs[0][0] = Double.NEGATIVE_INFINITY;
					for (i = 0; i < nb; i++) {
						zs[i+1][0] = z[i][0];
						zs[i+1][1] = z[i][1];
					}
					zs[nb+1][0] = Double.POSITIVE_INFINITY;
					for (i = nb; i < n; i++) {
						zs[i+2][0] = z[i][0];
						zs[i+2][1] = z[i][1];
					}
					int left2[] = new int[left.length];
					for (i = 0; i < left.length; i++) {
						left2[i] = left[i] + 1;
						if (left[i] > (nb-1)) {
							left2[i]++;
						}
					}
					int right2[] = new int[right.length];
					for (i = 0; i < right.length; i++) {
						right2[i] = right[i] + 1;
						if (right[i] > (nb-1)) {
							right2[i]++;
						}
					}
					
					// Do those staying on a side
					ints = new double[n-1][2];
					c2[0] = true;
					boolean id[] = new boolean[c2.length];
					int numid = 0;
					for (i = 0; i < c2.length; i++) {
						id[i] = !c2[i];
						if (id[i]) {
							numid++;
						}
					}
					double zleftid[][] = new double[numid][2];
					double midid[][] = new double[numid][2];
					int leftid[] = new int[numid];
					double zrightid[][] = new double[numid][2];
					int rightid[] = new int[numid];
					for (i = 0, j = 0; i < id.length; i++) {
					    if (id[i]) {
					    	zleftid[j][0] = zleft[i][0];
					    	zleftid[j][1] = zleft[i][1];
					    	midid[j][0] = mid[i][0];
					    	midid[j][1] = mid[i][1];
					    	leftid[j] = left2[i];
					    	zrightid[j][0] = zright[i][0];
					    	zrightid[j][1] = zright[i][1];
					    	rightid[j++] = right2[i];
					    }
					}
					I1 = scm.stquadh(zleftid, midid, leftid, zs, beta, qdat);
					I2 = scm.stquadh(zrightid, midid, rightid, zs, beta, qdat);
					for (i = 0, j = 0; i < id.length; i++) {
						if (id[i]) {
							ints[i][0] = I1[j][0] - I2[j][0];
							ints[i][1] = I1[j][1] - I2[j][1];
							j++;
						}
					}
					
					// For the rest, go to the strip middle, across, and back to the side
					int numc2 = 0;
					for (i = 0; i < c2.length; i++) {
						if (c2[i]) {
							numc2++;
						}
					}
					double z1[][] = new double[numc2][2];
					double z2[][] = new double[numc2][2];
					for (i = 0, j = 0; i < c2.length; i++) {
						if (c2[i]) {
							z1[j][0] = zleft[i][0];
							z1[j][1] = 0.5;
							z2[j][0] = zright[i][0];
							z2[j++][1] = 0.5;
						}
					}
					numid = 0;
					for (i = 0; i < id.length; i++) {
						id[i] = !id[i];
						if (id[i]) {
							numid++;
						}
					}
					zleftid = new double[numid][2];
					leftid = new int[numid];
					zrightid = new double[numid][2];
					rightid = new int[numid];
					for (i = 0, j = 0; i < id.length; i++) {
						if (id[i]) {
							zleftid[j][0] = zleft[i][0];
							zleftid[j][1] = zleft[i][1];
							leftid[j] = left2[i];
							zrightid[j][0] = zright[i][0];
							zrightid[j][1] = zright[i][1];
							rightid[j++] = right2[i]; 
						}
					}
					double I3[][]= scm.stquad(zleftid, z1, leftid, zs, beta, qdat);
					for (i = 0, j = 0; i < id.length; i++) {
						if (id[i]) {
							ints[i][0] = I3[j][0];
							ints[i][1] = I3[j++][1];
						}
					}
					int sing1[] = new int[z1.length];
					for (i = 0; i < z1.length; i++) {
						sing1[i] = -1;
					}
					double I4[][] = scm.stquadh(z1, z2, sing1, zs, beta, qdat);
					for (i = 0, j = 0; i < id.length; i++) {
						if (id[i]) {
							ints[i][0] = ints[i][0] + I4[j][0];
							ints[i][1] = ints[i][1] + I4[j++][1];
						}
					}
					double I5[][] = scm.stquad(zrightid, z2, rightid, zs, beta, qdat);
					for (i = 0, j = 0; i < id.length; i++) {
						if (id[i]) {
							ints[i][0] = ints[i][0] - I5[j][0];
							ints[i][1] = ints[i][1] - I5[j++][1];
						}
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
					}
					double absval[] = new double[numnotcmplx];  // absval[0] = abs(ints[0])
					for (i = 0, j = 0; i < cmplx.length; i++) {
						if (!cmplx[i]) {
							absval[j++] = scm.zabs(ints[i][0], ints[i][1]);
						}
					}
					int numrat1Zero = 0;
					int numrat2Zero = 0;
					int numrat1NaN = 0;
					int numrat2NaN = 0;
					int numrat1Inf = 0;
					int numrat2Inf = 0;
					if (absval[0] == 0) {
						rat1 = new double[1];
						rat1[0] = 0;
						rat2 = new double[1][2];
						rat2[0][0] = 0;
						rat2[0][1] = 0;
						numrat1Zero = 1;
						numrat2Zero = 1;
					}
					else {
						rat1 = new double[numnotcmplx-1];
						for (i = 1; i < absval.length; i++) {
							rat1[i-1] = absval[i]/absval[0];
							if (rat1[i-1] == 0) {
								numrat1Zero++;
							}
							else if (Double.isNaN(rat1[i-1])) {
							    numrat1NaN++;   	
							}
							else if (Double.isInfinite(rat1[i-1])) {
								numrat1Inf++;
							}
						}
						rat2 = new double[numcmplx][2];
						for (i = 0, j = 0; i < cmplx.length; i++) {
							if (cmplx[i]) {
								scm.zdiv(ints[i][0], ints[i][1], ints[0][0], ints[0][1], cr, ci);
								rat2[j][0] = cr[0];
								rat2[j++][1] = ci[0];
								if ((cr[0] == 0) && (ci[0] == 0)) {
									numrat2Zero++;
								}
								else if (Double.isNaN(cr[0]) || Double.isNaN(ci[0])) {
									numrat2NaN++;
								}
								else if (Double.isInfinite(cr[0]) || Double.isInfinite(ci[0])) {
									numrat2Inf++;
								}
							}
						}
					}
					if ((numrat1Zero > 0) || (numrat2Zero > 0) || (numrat1NaN > 0) || (numrat2NaN > 0) ||
							(numrat1Inf > 0) || (numrat2Inf > 0)) {
						// Singularities were too crowded.
						System.err.println("Severe crowding");
					}
					
					// Compute nonlinear equation residual values.
					boolean cmplx2[] = new boolean[cmplx.length-1];
					for (i = 1; i < cmplx.length; i++) {
						cmplx2[i-1] = cmplx[i];
					}
					if ((rat1 != null) && (rat1.length > 0)) {
						F1 = new double[rat1.length];
						for (i = 0, j = 0; i < cmplx2.length; i++) {
						    if (!cmplx2[i]) {
						    	F1[j] = Math.log(rat1[j]/nmlen[i][0]);
						    	j++;
						    }
						}
					}
					if ((rat2 != null) && (rat2.length > 0)) {
						F2 = new double[rat2.length][2];
						for (i = 0, j = 0; i < cmplx2.length; i++) {
						    if (cmplx2[i]) {
						    	scm.zdiv(rat2[j][0], rat2[j][1], nmlen[i][0], nmlen[i][1], cr, ci);
						    	F2[j][0] = Math.log(scm.zabs(cr[0], ci[0]));
						    	F2[j++][1] = Math.atan2(ci[0], cr[0]);
						    }
						}
					}
					int rindex = 0;
					if (F1 != null) {
					    for (rindex = 0; rindex < F1.length; rindex++)	{
					        residuals[rindex] = F1[rindex];	
					    }
					}
					if (F2 != null) {
						for (i = 0; i < F2.length; i++) {
							residuals[rindex + i] = F2[i][0];
							residuals[rindex + F2.length + i] = F2[i][1];
						}
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
	
	class stpfunEP extends NLConstrainedEngineEP {
		int n;
		int nb;
		DoubleDouble beta[];
		DoubleDouble nmlen[][];
		int left[];
		int right[];
		boolean cmplx[];
		DoubleDouble qdat[][];

		public stpfunEP(DoubleDouble y0[], int n, int nb, DoubleDouble beta[], DoubleDouble nmlen[][], int left[], int right[], 
				boolean cmplx[], DoubleDouble qdat[][]) {
			// nPoints, params
			super(y0.length, y0.length);
			this.n = n;
			this.nb = nb;
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
			Preferences.debug(" ******* Fit Elsunc Schwarz-Christoffel stparam ********* \n\n",
					Preferences.DEBUG_ALGORITHM);
			Preferences.debug("Number of iterations: " + String.valueOf(iters) + "\n", Preferences.DEBUG_ALGORITHM);
			Preferences.debug("Chi-squared: " + String.valueOf(getChiSquared()) + "\n", Preferences.DEBUG_ALGORITHM);
			for (int i = 0; i < a.length; i++) {
				Preferences.debug("a" + i + " " + String.valueOf(a[i]) + "\n", Preferences.DEBUG_ALGORITHM);
			}
		}

		public void fitToFunction(DoubleDouble[] a, DoubleDouble[] residuals, DoubleDouble[][] covarMat) {
    		int ctrl;
    		int i, j;
    		DoubleDouble z[][];
    		DoubleDouble I1[][];
    		DoubleDouble I2[][];
    		DoubleDouble rat1[] = null;
    		DoubleDouble rat2[][] = null;
    		DoubleDouble F1[] = null;
    		DoubleDouble F2[][] = null;
    		DoubleDouble cr[] = new DoubleDouble[1];
    		DoubleDouble ci[] = new DoubleDouble[1];
    		DoubleDouble ints[][] = null;
    		try {
				ctrl = ctrlMat[0];

				if ((ctrl == -1) || (ctrl == 1)) {
                    // Returns residual for solution of nonlinear equations
					
					// In this function, n refers to the number of finite prevertices.
					
					// Transform a (unconstrained variables) to z (actual parameters)
					z  = new DoubleDouble[n][2];
					z[0][0] = DoubleDouble.valueOf(0.0);
					z[0][1] = DoubleDouble.valueOf(0.0);
					for (i = 1; i < nb; i++) {
						z[i][0] = z[i-1][0].add((a[i-1]).exp());
						z[i][1] = DoubleDouble.valueOf(0.0);
					}
					z[nb][0] = a[nb-1];
					z[nb][1] = DoubleDouble.valueOf(1.0);
					for (i = nb+1; i < n; i++) {
						z[i][0] = z[i-1][0].subtract((a[i-1]).exp());
						z[i][1] = DoubleDouble.valueOf(1.0);
					}
					
					// Compute the integrals
					DoubleDouble zleft[][] = new DoubleDouble[left.length][2];
					for (i = 0; i < left.length; i++) {
						zleft[i][0] = z[left[i]][0];
						zleft[i][1] = z[left[i]][1];
					}
					DoubleDouble zright[][] = new DoubleDouble[right.length][2];
					for (i = 0; i < right.length; i++) {
						zright[i][0] = z[right[i]][0];
						zright[i][1] = z[right[i]][1];
					}
					DoubleDouble mid[][] = new DoubleDouble[left.length][2];
					for (i = 0; i < left.length; i++) {
						mid[i][0] = (zleft[i][0].add(zright[i][0])).divide(DoubleDouble.valueOf(2.0));
						mid[i][1] = (zleft[i][1].add(zright[i][1])).divide(DoubleDouble.valueOf(2.0));
					}
					boolean c2[] = new boolean[cmplx.length];
					for (i = 0; i < cmplx.length; i++) {
						c2[i] = cmplx[i];
					}
					c2[1] = false;
					for (i = 0; i < c2.length; i++) {
						if (c2[i]) {
						    DoubleDouble sgn = sign(DoubleDouble.valueOf(left[i] - (nb-1)));
						    mid[i][1] = mid[i][1].subtract(sgn.divide(DoubleDouble.valueOf(2.0)));
						}
					} // for (i = 0; i < c2.length; i++)
					
					// Add ends of strip to z, and modify singularity indices
					DoubleDouble zs[][] = new DoubleDouble[n+2][2];
					zs[0][0] = DoubleDouble.NEGATIVE_INFINITY;
					zs[0][1] = DoubleDouble.valueOf(0.0);
					for (i = 0; i < nb; i++) {
						zs[i+1][0] = z[i][0];
						zs[i+1][1] = z[i][1];
					}
					zs[nb+1][0] = DoubleDouble.POSITIVE_INFINITY;
					zs[nb+1][1] = DoubleDouble.valueOf(0.0);
					for (i = nb; i < n; i++) {
						zs[i+2][0] = z[i][0];
						zs[i+2][1] = z[i][1];
					}
					int left2[] = new int[left.length];
					for (i = 0; i < left.length; i++) {
						left2[i] = left[i] + 1;
						if (left[i] > (nb-1)) {
							left2[i]++;
						}
					}
					int right2[] = new int[right.length];
					for (i = 0; i < right.length; i++) {
						right2[i] = right[i] + 1;
						if (right[i] > (nb-1)) {
							right2[i]++;
						}
					}
					
					// Do those staying on a side
					ints = new DoubleDouble[n-1][2];
					c2[0] = true;
					boolean id[] = new boolean[c2.length];
					int numid = 0;
					for (i = 0; i < c2.length; i++) {
						id[i] = !c2[i];
						if (id[i]) {
							numid++;
						}
					}
					DoubleDouble zleftid[][] = new DoubleDouble[numid][2];
					DoubleDouble midid[][] = new DoubleDouble[numid][2];
					int leftid[] = new int[numid];
					DoubleDouble zrightid[][] = new DoubleDouble[numid][2];
					int rightid[] = new int[numid];
					for (i = 0, j = 0; i < id.length; i++) {
					    if (id[i]) {
					    	zleftid[j][0] = zleft[i][0];
					    	zleftid[j][1] = zleft[i][1];
					    	midid[j][0] = mid[i][0];
					    	midid[j][1] = mid[i][1];
					    	leftid[j] = left2[i];
					    	zrightid[j][0] = zright[i][0];
					    	zrightid[j][1] = zright[i][1];
					    	rightid[j++] = right2[i];
					    }
					}
					I1 = stquadhEP(zleftid, midid, leftid, zs, beta, qdat);
					I2 = stquadhEP(zrightid, midid, rightid, zs, beta, qdat);
					for (i = 0, j = 0; i < id.length; i++) {
						if (id[i]) {
							ints[i][0] = I1[j][0].subtract(I2[j][0]);
							ints[i][1] = I1[j][1].subtract(I2[j][1]);
							j++;
						}
					}
					
					// For the rest, go to the strip middle, across, and back to the side
					int numc2 = 0;
					for (i = 0; i < c2.length; i++) {
						if (c2[i]) {
							numc2++;
						}
					}
					DoubleDouble z1[][] = new DoubleDouble[numc2][2];
					DoubleDouble z2[][] = new DoubleDouble[numc2][2];
					for (i = 0, j = 0; i < c2.length; i++) {
						if (c2[i]) {
							z1[j][0] = zleft[i][0];
							z1[j][1] = DoubleDouble.valueOf(0.5);
							z2[j][0] = zright[i][0];
							z2[j++][1] = DoubleDouble.valueOf(0.5);
						}
					}
					numid = 0;
					for (i = 0; i < id.length; i++) {
						id[i] = !id[i];
						if (id[i]) {
							numid++;
						}
					}
					zleftid = new DoubleDouble[numid][2];
					leftid = new int[numid];
					zrightid = new DoubleDouble[numid][2];
					rightid = new int[numid];
					for (i = 0, j = 0; i < id.length; i++) {
						if (id[i]) {
							zleftid[j][0] = zleft[i][0];
							zleftid[j][1] = zleft[i][1];
							leftid[j] = left2[i];
							zrightid[j][0] = zright[i][0];
							zrightid[j][1] = zright[i][1];
							rightid[j++] = right2[i]; 
						}
					}
					DoubleDouble I3[][]= stquadEP(zleftid, z1, leftid, zs, beta, qdat);
					for (i = 0, j = 0; i < id.length; i++) {
						if (id[i]) {
							ints[i][0] = I3[j][0];
							ints[i][1] = I3[j++][1];
						}
					}
					int sing1[] = new int[z1.length];
					for (i = 0; i < z1.length; i++) {
						sing1[i] = -1;
					}
					DoubleDouble I4[][] = stquadhEP(z1, z2, sing1, zs, beta, qdat);
					for (i = 0, j = 0; i < id.length; i++) {
						if (id[i]) {
							ints[i][0] = ints[i][0].add(I4[j][0]);
							ints[i][1] = ints[i][1].add(I4[j++][1]);
						}
					}
					DoubleDouble I5[][] = stquadEP(zrightid, z2, rightid, zs, beta, qdat);
					for (i = 0, j = 0; i < id.length; i++) {
						if (id[i]) {
							ints[i][0] = ints[i][0].subtract(I5[j][0]);
							ints[i][1] = ints[i][1].subtract(I5[j++][1]);
						}
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
					}
					DoubleDouble absval[] = new DoubleDouble[numnotcmplx];  // absval[0] = abs(ints[0])
					for (i = 0, j = 0; i < cmplx.length; i++) {
						if (!cmplx[i]) {
							absval[j++] = zabs(ints[i][0], ints[i][1]);
						}
					}
					int numrat1Zero = 0;
					int numrat2Zero = 0;
					int numrat1NaN = 0;
					int numrat2NaN = 0;
					int numrat1Inf = 0;
					int numrat2Inf = 0;
					if (absval[0].isZero()) {
						rat1 = new DoubleDouble[1];
						rat1[0] = DoubleDouble.valueOf(0);
						rat2 = new DoubleDouble[1][2];
						rat2[0][0] = DoubleDouble.valueOf(0);
						rat2[0][1] = DoubleDouble.valueOf(0);
						numrat1Zero = 1;
						numrat2Zero = 1;
					}
					else {
						rat1 = new DoubleDouble[numnotcmplx-1];
						for (i = 1; i < absval.length; i++) {
							rat1[i-1] = absval[i].divide(absval[0]);
							if (rat1[i-1].isZero()) {
								numrat1Zero++;
							}
							else if (rat1[i-1].isNaN()) {
							    numrat1NaN++;   	
							}
							else if (rat1[i-1].isInfinite()) {
								numrat1Inf++;
							}
						}
						rat2 = new DoubleDouble[numcmplx][2];
						for (i = 0, j = 0; i < cmplx.length; i++) {
							if (cmplx[i]) {
								zdiv(ints[i][0], ints[i][1], ints[0][0], ints[0][1], cr, ci);
								rat2[j][0] = cr[0];
								rat2[j++][1] = ci[0];
								if ((cr[0].isZero()) && (ci[0].isZero())) {
									numrat2Zero++;
								}
								else if ((cr[0].isNaN()) || (ci[0].isNaN())) {
									numrat2NaN++;
								}
								else if ((cr[0].isInfinite()) || (ci[0].isInfinite())) {
									numrat2Inf++;
								}
							}
						}
					}
					if ((numrat1Zero > 0) || (numrat2Zero > 0) || (numrat1NaN > 0) || (numrat2NaN > 0) ||
							(numrat1Inf > 0) || (numrat2Inf > 0)) {
						// Singularities were too crowded.
					    System.err.println("Severe crowding");
					}
					
					// Compute nonlinear equation residual values.
					boolean cmplx2[] = new boolean[cmplx.length-1];
					for (i = 1; i < cmplx.length; i++) {
						cmplx2[i-1] = cmplx[i];
					}
					if ((rat1 != null) && (rat1.length > 0)) {
						F1 = new DoubleDouble[rat1.length];
						for (i = 0, j = 0; i < cmplx2.length; i++) {
						    if (!cmplx2[i]) {
						    	F1[j] = (rat1[j].divide(nmlen[i][0])).log();
						    	j++;
						    }
						}
					}
					if ((rat2 != null) && (rat2.length > 0)) {
						F2 = new DoubleDouble[rat2.length][2];
						for (i = 0, j = 0; i < cmplx2.length; i++) {
						    if (cmplx2[i]) {
						    	zdiv(rat2[j][0], rat2[j][1], nmlen[i][0], nmlen[i][1], cr, ci);
						    	F2[j][0] = (zabs(cr[0], ci[0])).log();
						    	F2[j++][1] = ci[0].atan2(cr[0]);
						    }
						}
					}
					int rindex = 0;
					if (F1 != null) {
					    for (rindex = 0; rindex < F1.length; rindex++)	{
					        residuals[rindex] = F1[rindex];	
					    }
					}
					if (F2 != null) {
						for (i = 0; i < F2.length; i++) {
							residuals[rindex + i] = F2[i][0];
							residuals[rindex + F2.length + i] = F2[i][1];
						}
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
	
	class stpfun2 extends NL2sol {
		int iv[];
		double v[];
		double x[];
		int n;
		int nb;
		double beta[];
		double nmlen[][];
		int left[];
		int right[];
		boolean cmplx[];
		double qdat[][];

		public stpfun2(double x[], int n, int iv[], double v[],
				boolean useAnalyticJacobian, int nb, double beta[], double nmlen[][], int left[], int right[], boolean cmplx[],
				double qdat[][]) {
			
			
			// nPoints data points
			// nPoints coefficients
			// x[] is a length 3 initial guess at input and best estimate at
			// output
			// data starts at x[1]
			// iv[] has length 61 + number of coefficients = 63
			// v[] has length at least 94 + n*p + 3*n + p*(3*p+33)/2
			// uiparm, integer parameter array = null
			// urparm, double parameter array = null
			super(x.length-1, x.length-1, x, iv, v, useAnalyticJacobian, null, null);
			this.n = n;
			this.x = x;
			this.iv = iv;
			this.v = v;
			this.nb = nb;
			this.beta = beta;
			this.nmlen = nmlen;
			this.left = left;
			this.right = right;
			this.cmplx = cmplx;
			this.qdat = qdat;
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
			Preferences.debug(" ******* Fit Nl2sol Schwarz-Christoffel stparam ********* \n\n",
					Preferences.DEBUG_ALGORITHM);
			Preferences.debug("Number of iterations: " + String.valueOf(iv[31]) + "\n", Preferences.DEBUG_ALGORITHM);
			Preferences.debug("Chi-squared: " + String.valueOf(getChiSquared()) + "\n", Preferences.DEBUG_ALGORITHM);
		}
		
		public void calcj(final int meqn, final int nvar, final double x[],
				final int nf, final double jac[][], final int uiparm[],
				final double urparm[]) {

		}

		public void calcr(final int meqn, final int nvar, final double x[],
				int nf[], final double r[], final int uiparm[],
				final double urparm[]) {
    		int i, j;
    		double z[][];
    		double I1[][];
    		double I2[][];
    		double rat1[] = null;
    		double rat2[][] = null;
    		double F1[] = null;
    		double F2[][] = null;
    		double cr[] = new double[1];
    		double ci[] = new double[1];
    		double ints[][] = null;
    		
            // Returns residual for solution of nonlinear equations
			
			// In this function, n refers to the number of finite prevertices.
			
			// Transform a (unconstrained variables) to z (actual parameters)
			z  = new double[n][2];
			for (i = 1; i < nb; i++) {
				z[i][0] = z[i-1][0] + Math.exp(x[i]);
			}
			z[nb][0] = x[nb];
			z[nb][1] = 1.0;
			for (i = nb+1; i < n; i++) {
				z[i][0] = z[i-1][0] -Math.exp(x[i]);
				z[i][1] = 1.0;
			}
			
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
			double mid[][] = new double[left.length][2];
			for (i = 0; i < left.length; i++) {
				mid[i][0] = (zleft[i][0] + zright[i][0])/2.0;
				mid[i][1] = (zleft[i][1] + zright[i][1])/2.0;
			}
			boolean c2[] = new boolean[cmplx.length];
			for (i = 0; i < cmplx.length; i++) {
				c2[i] = cmplx[i];
			}
			c2[1] = false;
			for (i = 0; i < c2.length; i++) {
				if (c2[i]) {
				    double sgn = scm.sign(left[i] - (nb-1));
				    mid[i][1] = mid[i][1] - sgn/2.0;
				}
			} // for (i = 0; i < c2.length; i++)
			
			// Add ends of strip to z, and modify singularity indices
			double zs[][] = new double[n+2][2];
			zs[0][0] = Double.NEGATIVE_INFINITY;
			for (i = 0; i < nb; i++) {
				zs[i+1][0] = z[i][0];
				zs[i+1][1] = z[i][1];
			}
			zs[nb+1][0] = Double.POSITIVE_INFINITY;
			for (i = nb; i < n; i++) {
				zs[i+2][0] = z[i][0];
				zs[i+2][1] = z[i][1];
			}
			int left2[] = new int[left.length];
			for (i = 0; i < left.length; i++) {
				left2[i] = left[i] + 1;
				if (left[i] > (nb-1)) {
					left2[i]++;
				}
			}
			int right2[] = new int[right.length];
			for (i = 0; i < right.length; i++) {
				right2[i] = right[i] + 1;
				if (right[i] > (nb-1)) {
					right2[i]++;
				}
			}
			
			// Do those staying on a side
			ints = new double[n-1][2];
			c2[0] = true;
			boolean id[] = new boolean[c2.length];
			int numid = 0;
			for (i = 0; i < c2.length; i++) {
				id[i] = !c2[i];
				if (id[i]) {
					numid++;
				}
			}
			double zleftid[][] = new double[numid][2];
			double midid[][] = new double[numid][2];
			int leftid[] = new int[numid];
			double zrightid[][] = new double[numid][2];
			int rightid[] = new int[numid];
			for (i = 0, j = 0; i < id.length; i++) {
			    if (id[i]) {
			    	zleftid[j][0] = zleft[i][0];
			    	zleftid[j][1] = zleft[i][1];
			    	midid[j][0] = mid[i][0];
			    	midid[j][1] = mid[i][1];
			    	leftid[j] = left2[i];
			    	zrightid[j][0] = zright[i][0];
			    	zrightid[j][1] = zright[i][1];
			    	rightid[j++] = right2[i];
			    }
			}
			I1 = scm.stquadh(zleftid, midid, leftid, zs, beta, qdat);
			I2 = scm.stquadh(zrightid, midid, rightid, zs, beta, qdat);
			for (i = 0, j = 0; i < id.length; i++) {
				if (id[i]) {
					ints[i][0] = I1[j][0] - I2[j][0];
					ints[i][1] = I1[j][1] - I2[j][1];
					j++;
				}
			}
			
			// For the rest, go to the strip middle, across, and back to the side
			int numc2 = 0;
			for (i = 0; i < c2.length; i++) {
				if (c2[i]) {
					numc2++;
				}
			}
			double z1[][] = new double[numc2][2];
			double z2[][] = new double[numc2][2];
			for (i = 0, j = 0; i < c2.length; i++) {
				if (c2[i]) {
					z1[j][0] = zleft[i][0];
					z1[j][1] = 0.5;
					z2[j][0] = zright[i][0];
					z2[j++][1] = 0.5;
				}
			}
			numid = 0;
			for (i = 0; i < id.length; i++) {
				id[i] = !id[i];
				if (id[i]) {
					numid++;
				}
			}
			zleftid = new double[numid][2];
			leftid = new int[numid];
			zrightid = new double[numid][2];
			rightid = new int[numid];
			for (i = 0, j = 0; i < id.length; i++) {
				if (id[i]) {
					zleftid[j][0] = zleft[i][0];
					zleftid[j][1] = zleft[i][1];
					leftid[j] = left2[i];
					zrightid[j][0] = zright[i][0];
					zrightid[j][1] = zright[i][1];
					rightid[j++] = right2[i]; 
				}
			}
			double I3[][]= scm.stquad(zleftid, z1, leftid, zs, beta, qdat);
			for (i = 0, j = 0; i < id.length; i++) {
				if (id[i]) {
					ints[i][0] = I3[j][0];
					ints[i][1] = I3[j++][1];
				}
			}
			int sing1[] = new int[z1.length];
			for (i = 0; i < z1.length; i++) {
				sing1[i] = -1;
			}
			double I4[][] = scm.stquadh(z1, z2, sing1, zs, beta, qdat);
			for (i = 0, j = 0; i < id.length; i++) {
				if (id[i]) {
					ints[i][0] = ints[i][0] + I4[j][0];
					ints[i][1] = ints[i][1] + I4[j++][1];
				}
			}
			double I5[][] = scm.stquad(zrightid, z2, rightid, zs, beta, qdat);
			for (i = 0, j = 0; i < id.length; i++) {
				if (id[i]) {
					ints[i][0] = ints[i][0] - I5[j][0];
					ints[i][1] = ints[i][1] - I5[j++][1];
				}
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
			}
			double absval[] = new double[numnotcmplx];  // absval[0] = abs(ints[0])
			for (i = 0, j = 0; i < cmplx.length; i++) {
				if (!cmplx[i]) {
					absval[j++] = scm.zabs(ints[i][0], ints[i][1]);
				}
			}
			int numrat1Zero = 0;
			int numrat2Zero = 0;
			int numrat1NaN = 0;
			int numrat2NaN = 0;
			int numrat1Inf = 0;
			int numrat2Inf = 0;
			if (absval[0] == 0) {
				rat1 = new double[1];
				rat1[0] = 0;
				rat2 = new double[1][2];
				rat2[0][0] = 0;
				rat2[0][1] = 0;
				numrat1Zero = 1;
				numrat2Zero = 1;
			}
			else {
				rat1 = new double[numnotcmplx-1];
				for (i = 1; i < absval.length; i++) {
					rat1[i-1] = absval[i]/absval[0];
					if (rat1[i-1] == 0) {
						numrat1Zero++;
					}
					else if (Double.isNaN(rat1[i-1])) {
					    numrat1NaN++;   	
					}
					else if (Double.isInfinite(rat1[i-1])) {
						numrat1Inf++;
					}
				}
				rat2 = new double[numcmplx][2];
				for (i = 0, j = 0; i < cmplx.length; i++) {
					if (cmplx[i]) {
						scm.zdiv(ints[i][0], ints[i][1], ints[0][0], ints[0][1], cr, ci);
						rat2[j][0] = cr[0];
						rat2[j++][1] = ci[0];
						if ((cr[0] == 0) && (ci[0] == 0)) {
							numrat2Zero++;
						}
						else if (Double.isNaN(cr[0]) || Double.isNaN(ci[0])) {
							numrat2NaN++;
						}
						else if (Double.isInfinite(cr[0]) || Double.isInfinite(ci[0])) {
							numrat2Inf++;
						}
					}
				}
			}
			if ((numrat1Zero > 0) || (numrat2Zero > 0) || (numrat1NaN > 0) || (numrat2NaN > 0) ||
					(numrat1Inf > 0) || (numrat2Inf > 0)) {
				// Singularities were too crowded.
				System.err.println("Severe crowding");
			}
			
			// Compute nonlinear equation residual values.
			boolean cmplx2[] = new boolean[cmplx.length-1];
			for (i = 1; i < cmplx.length; i++) {
				cmplx2[i-1] = cmplx[i];
			}
			if ((rat1 != null) && (rat1.length > 0)) {
				F1 = new double[rat1.length];
				for (i = 0, j = 0; i < cmplx2.length; i++) {
				    if (!cmplx2[i]) {
				    	F1[j] = Math.log(rat1[j]/nmlen[i][0]);
				    	j++;
				    }
				}
			}
			if ((rat2 != null) && (rat2.length > 0)) {
				F2 = new double[rat2.length][2];
				for (i = 0, j = 0; i < cmplx2.length; i++) {
				    if (cmplx2[i]) {
				    	scm.zdiv(rat2[j][0], rat2[j][1], nmlen[i][0], nmlen[i][1], cr, ci);
				    	F2[j][0] = Math.log(scm.zabs(cr[0], ci[0]));
				    	F2[j++][1] = Math.atan2(ci[0], cr[0]);
				    }
				}
			}
			int rindex = 0;
			if (F1 != null) {
			    for (rindex = 0; rindex < F1.length; rindex++)	{
			        r[rindex+1] = F1[rindex];	
			    }
			}
			if (F2 != null) {
				for (i = 0; i < F2.length; i++) {
					r[rindex + i + 1] = F2[i][0];
					r[rindex + F2.length + i + 1] = F2[i][1];
				}
			}
				

			return;
    		
    	}
	}
	
	// If NESolve, method will be be NESolve.TRUST_REGION (default) or NESolve.LINE_SEARCH.
	class stpfun3 extends NESolve {
		int n;
		int nb;
		double beta[];
		double nmlen[][];
		int left[];
		int right[];
		boolean cmplx[];
		double qdat[][];
	       
        public stpfun3(boolean initialJacobianIdentity, double x0[], double fparam[], 
	    		boolean analyticJacobian, double scale[][], Vector<Double> path,
	    		double btrack[], int trace, int method, int maxIterations, double fvectol, 
	    		double steptol, double maxStepSize, double details11, int scaling,
	    		int n, int nb, double beta[], double nmlen[][], int left[], int right[], boolean cmplx[],
				double qdat[][]) {

            super(initialJacobianIdentity, x0, fparam, 
    	    		analyticJacobian, scale, path,
    	    		btrack, trace, method, maxIterations, fvectol, 
    	    		steptol, maxStepSize, details11, scaling);
            this.n = n;
			this.nb = nb;
			this.beta = beta;
			this.nmlen = nmlen;
			this.left = left;
			this.right = right;
			this.cmplx = cmplx;
			this.qdat = qdat;
        }

        
        public void fitToFunction(double fvplus[], double xplus[], double fparam[]) {
        	int i, j;
    		double z[][];
    		double I1[][];
    		double I2[][];
    		double rat1[] = null;
    		double rat2[][] = null;
    		double F1[] = null;
    		double F2[][] = null;
    		double cr[] = new double[1];
    		double ci[] = new double[1];
    		double ints[][] = null;
        	// Returns residual for solution of nonlinear equations
			
			// In this function, n refers to the number of finite prevertices.
			
			// Transform a (unconstrained variables) to z (actual parameters)
			z  = new double[n][2];
			for (i = 1; i < nb; i++) {
				z[i][0] = z[i-1][0] + Math.exp(xplus[i-1]);
			}
			z[nb][0] = xplus[nb-1];
			z[nb][1] = 1.0;
			for (i = nb+1; i < n; i++) {
				z[i][0] = z[i-1][0] -Math.exp(xplus[i-1]);
				z[i][1] = 1.0;
			}
			
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
			double mid[][] = new double[left.length][2];
			for (i = 0; i < left.length; i++) {
				mid[i][0] = (zleft[i][0] + zright[i][0])/2.0;
				mid[i][1] = (zleft[i][1] + zright[i][1])/2.0;
			}
			boolean c2[] = new boolean[cmplx.length];
			for (i = 0; i < cmplx.length; i++) {
				c2[i] = cmplx[i];
			}
			c2[1] = false;
			for (i = 0; i < c2.length; i++) {
				if (c2[i]) {
				    double sgn = scm.sign(left[i] - (nb-1));
				    mid[i][1] = mid[i][1] - sgn/2.0;
				}
			} // for (i = 0; i < c2.length; i++)
			
			// Add ends of strip to z, and modify singularity indices
			double zs[][] = new double[n+2][2];
			zs[0][0] = Double.NEGATIVE_INFINITY;
			for (i = 0; i < nb; i++) {
				zs[i+1][0] = z[i][0];
				zs[i+1][1] = z[i][1];
			}
			zs[nb+1][0] = Double.POSITIVE_INFINITY;
			for (i = nb; i < n; i++) {
				zs[i+2][0] = z[i][0];
				zs[i+2][1] = z[i][1];
			}
			int left2[] = new int[left.length];
			for (i = 0; i < left.length; i++) {
				left2[i] = left[i] + 1;
				if (left[i] > (nb-1)) {
					left2[i]++;
				}
			}
			int right2[] = new int[right.length];
			for (i = 0; i < right.length; i++) {
				right2[i] = right[i] + 1;
				if (right[i] > (nb-1)) {
					right2[i]++;
				}
			}
			
			// Do those staying on a side
			ints = new double[n-1][2];
			c2[0] = true;
			boolean id[] = new boolean[c2.length];
			int numid = 0;
			for (i = 0; i < c2.length; i++) {
				id[i] = !c2[i];
				if (id[i]) {
					numid++;
				}
			}
			double zleftid[][] = new double[numid][2];
			double midid[][] = new double[numid][2];
			int leftid[] = new int[numid];
			double zrightid[][] = new double[numid][2];
			int rightid[] = new int[numid];
			for (i = 0, j = 0; i < id.length; i++) {
			    if (id[i]) {
			    	zleftid[j][0] = zleft[i][0];
			    	zleftid[j][1] = zleft[i][1];
			    	midid[j][0] = mid[i][0];
			    	midid[j][1] = mid[i][1];
			    	leftid[j] = left2[i];
			    	zrightid[j][0] = zright[i][0];
			    	zrightid[j][1] = zright[i][1];
			    	rightid[j++] = right2[i];
			    }
			}
			I1 = scm.stquadh(zleftid, midid, leftid, zs, beta, qdat);
			I2 = scm.stquadh(zrightid, midid, rightid, zs, beta, qdat);
			for (i = 0, j = 0; i < id.length; i++) {
				if (id[i]) {
					ints[i][0] = I1[j][0] - I2[j][0];
					ints[i][1] = I1[j][1] - I2[j][1];
					j++;
				}
			}
			
			// For the rest, go to the strip middle, across, and back to the side
			int numc2 = 0;
			for (i = 0; i < c2.length; i++) {
				if (c2[i]) {
					numc2++;
				}
			}
			double z1[][] = new double[numc2][2];
			double z2[][] = new double[numc2][2];
			for (i = 0, j = 0; i < c2.length; i++) {
				if (c2[i]) {
					z1[j][0] = zleft[i][0];
					z1[j][1] = 0.5;
					z2[j][0] = zright[i][0];
					z2[j++][1] = 0.5;
				}
			}
			numid = 0;
			for (i = 0; i < id.length; i++) {
				id[i] = !id[i];
				if (id[i]) {
					numid++;
				}
			}
			zleftid = new double[numid][2];
			leftid = new int[numid];
			zrightid = new double[numid][2];
			rightid = new int[numid];
			for (i = 0, j = 0; i < id.length; i++) {
				if (id[i]) {
					zleftid[j][0] = zleft[i][0];
					zleftid[j][1] = zleft[i][1];
					leftid[j] = left2[i];
					zrightid[j][0] = zright[i][0];
					zrightid[j][1] = zright[i][1];
					rightid[j++] = right2[i]; 
				}
			}
			double I3[][]= scm.stquad(zleftid, z1, leftid, zs, beta, qdat);
			for (i = 0, j = 0; i < id.length; i++) {
				if (id[i]) {
					ints[i][0] = I3[j][0];
					ints[i][1] = I3[j++][1];
				}
			}
			int sing1[] = new int[z1.length];
			for (i = 0; i < z1.length; i++) {
				sing1[i] = -1;
			}
			double I4[][] = scm.stquadh(z1, z2, sing1, zs, beta, qdat);
			for (i = 0, j = 0; i < id.length; i++) {
				if (id[i]) {
					ints[i][0] = ints[i][0] + I4[j][0];
					ints[i][1] = ints[i][1] + I4[j++][1];
				}
			}
			double I5[][] = scm.stquad(zrightid, z2, rightid, zs, beta, qdat);
			for (i = 0, j = 0; i < id.length; i++) {
				if (id[i]) {
					ints[i][0] = ints[i][0] - I5[j][0];
					ints[i][1] = ints[i][1] - I5[j++][1];
				}
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
			}
			double absval[] = new double[numnotcmplx];  // absval[0] = abs(ints[0])
			for (i = 0, j = 0; i < cmplx.length; i++) {
				if (!cmplx[i]) {
					absval[j++] = scm.zabs(ints[i][0], ints[i][1]);
				}
			}
			int numrat1Zero = 0;
			int numrat2Zero = 0;
			int numrat1NaN = 0;
			int numrat2NaN = 0;
			int numrat1Inf = 0;
			int numrat2Inf = 0;
			if (absval[0] == 0) {
				rat1 = new double[1];
				rat1[0] = 0;
				rat2 = new double[1][2];
				rat2[0][0] = 0;
				rat2[0][1] = 0;
				numrat1Zero = 1;
				numrat2Zero = 1;
			}
			else {
				rat1 = new double[numnotcmplx-1];
				for (i = 1; i < absval.length; i++) {
					rat1[i-1] = absval[i]/absval[0];
					if (rat1[i-1] == 0) {
						numrat1Zero++;
					}
					else if (Double.isNaN(rat1[i-1])) {
					    numrat1NaN++;   	
					}
					else if (Double.isInfinite(rat1[i-1])) {
						numrat1Inf++;
					}
				}
				rat2 = new double[numcmplx][2];
				for (i = 0, j = 0; i < cmplx.length; i++) {
					if (cmplx[i]) {
						scm.zdiv(ints[i][0], ints[i][1], ints[0][0], ints[0][1], cr, ci);
						rat2[j][0] = cr[0];
						rat2[j++][1] = ci[0];
						if ((cr[0] == 0) && (ci[0] == 0)) {
							numrat2Zero++;
						}
						else if (Double.isNaN(cr[0]) || Double.isNaN(ci[0])) {
							numrat2NaN++;
						}
						else if (Double.isInfinite(cr[0]) || Double.isInfinite(ci[0])) {
							numrat2Inf++;
						}
					}
				}
			}
			if ((numrat1Zero > 0) || (numrat2Zero > 0) || (numrat1NaN > 0) || (numrat2NaN > 0) ||
					(numrat1Inf > 0) || (numrat2Inf > 0)) {
				// Singularities were too crowded.
				System.err.println("Severe crowding");
			}
			
			// Compute nonlinear equation residual values.
			boolean cmplx2[] = new boolean[cmplx.length-1];
			for (i = 1; i < cmplx.length; i++) {
				cmplx2[i-1] = cmplx[i];
			}
			if ((rat1 != null) && (rat1.length > 0)) {
				F1 = new double[rat1.length];
				for (i = 0, j = 0; i < cmplx2.length; i++) {
				    if (!cmplx2[i]) {
				    	F1[j] = Math.log(rat1[j]/nmlen[i][0]);
				    	j++;
				    }
				}
			}
			if ((rat2 != null) && (rat2.length > 0)) {
				F2 = new double[rat2.length][2];
				for (i = 0, j = 0; i < cmplx2.length; i++) {
				    if (cmplx2[i]) {
				    	scm.zdiv(rat2[j][0], rat2[j][1], nmlen[i][0], nmlen[i][1], cr, ci);
				    	F2[j][0] = Math.log(scm.zabs(cr[0], ci[0]));
				    	F2[j++][1] = Math.atan2(ci[0], cr[0]);
				    }
				}
			}
			int rindex = 0;
			if (F1 != null) {
			    for (rindex = 0; rindex < F1.length; rindex++)	{
			        fvplus[rindex] = F1[rindex];	
			    }
			}
			if (F2 != null) {
				for (i = 0; i < F2.length; i++) {
					fvplus[rindex + i] = F2[i][0];
					fvplus[rindex + F2.length + i] = F2[i][1];
				}
			}	
        }

       
        public void fitToJacobian(double jc[][], int addfun[], double x0[], double fparam[]) {
        	
        }
        
        /**
		 * Starts the analysis.
		 */
		public void driver() {
			super.driver();
		}
    }
	
	
	
	public DoubleDouble sign(DoubleDouble d) {
		if (d.isPositive()) {
			return DoubleDouble.valueOf(1.0);
		}
		else if (d.isZero()) {
			return DoubleDouble.valueOf(0.0);
		}
		else {
			return DoubleDouble.valueOf(-1.0);
		}
	}
	
	/**
     * zabs computes the absolute value or magnitude of a DoubleDouble precision complex variable zr + j*zi.
     * 
     * @param zr DoubleDouble
     * @param zi DoubleDouble
     * 
     * @return DoubleDouble
     */
    private DoubleDouble zabs(final DoubleDouble zr, final DoubleDouble zi) {
        DoubleDouble u, v, q, s;
        u = zr.abs();
        v = zi.abs();
        s = u.add(v);

        // s * 1.0 makes an unnormalized underflow on CDC machines into a true
        // floating zero
        //s = s * 1.0;

        if (s.equals(DoubleDouble.valueOf(0.0))) {
            return DoubleDouble.valueOf(0.0);
        } else if (u.gt(v)) {
        	q = v.divide(u);
            return (u.multiply(((DoubleDouble.valueOf(1.0)).add(q.multiply(q))).sqrt()));
        } else {
        	q = u.divide(v);
            return (v.multiply(((DoubleDouble.valueOf(1.0)).add(q.multiply(q))).sqrt()));
        }
    }
    
    /**
     * complex multiply c = a * b.
     * 
     * @param ar DoubleDouble
     * @param ai DoubleDouble
     * @param br DoubleDouble
     * @param bi DoubleDouble
     * @param cr DoubleDouble[]
     * @param ci DoubleDouble[]
     */
    private void zmlt(final DoubleDouble ar, final DoubleDouble ai, final DoubleDouble br, final DoubleDouble bi, final DoubleDouble[] cr,
            final DoubleDouble[] ci) {
        DoubleDouble ca, cb;

        ca = (ar.multiply(br)).subtract(ai.multiply(bi));
        cb = (ar.multiply(bi)).add(ai.multiply(br));
        cr[0] = (DoubleDouble)ca.clone();
        ci[0] = (DoubleDouble)cb.clone();

        return;
    }
    
    /**
     * complex divide c = a/b.
     * 
     * @param ar DoubleDouble
     * @param ai DoubleDouble
     * @param br DoubleDouble
     * @param bi DoubleDouble
     * @param cr DoubleDouble[]
     * @param ci DoubleDouble[]
     */
    private void zdiv(final DoubleDouble ar, final DoubleDouble ai, final DoubleDouble br, final DoubleDouble bi, final DoubleDouble[] cr,
            final DoubleDouble[] ci) {
        DoubleDouble bm, cc, cd, ca, cb;

        bm = (zabs(br, bi)).reciprocal();
        cc = br.multiply(bm);
        cd = bi.multiply(bm);
        ca = ( (ar.multiply(cc)).add(ai.multiply(cd))).multiply(bm);
        cb = ( (ai.multiply(cc)).subtract(ar.multiply(cd))).multiply(bm);
        cr[0] = (DoubleDouble)ca.clone();
        ci[0] = (DoubleDouble)cb.clone();

        return;
    }
	
	private DoubleDouble[][] stquadhEP(DoubleDouble z1[][], DoubleDouble z2[][], int sing1[], DoubleDouble z[][],
			DoubleDouble beta[], DoubleDouble qdat[][]) {
		// Original MATLAB stquadh routine copyright 1998 by Toby Driscoll.
		
		// stquadh applies the "1/2 rule" by assuming that the distance from the
		// integration interval to the nearest singularity is equal to the 
		// distance from the left endpoint to the nearest singularity.  This is
		// certainly true e.g. if one begins integration at the nearest
		// singularity to the target point.  However, it may be violated in
		// important circumstances, such as when one integrates from the 
		// next-nearest singularity (because the nearest maps to infinity), or when
		// one is integrating between adjacent prevertices (as in the param
		// problem).  The main difficulty is the possibility of singularities
		// from the "other side" of the strip intruding.
		
		// Here we assume the integration intervals are horizontal.  This
		// function recursively subdivides the interval until the "1/2 rule" is
		// satisfied for singularities "between" the endpoints.  Actually, we
		// use a more stringent "alpha rule", for alpha > 1/2, as this seems to
		// be necessary sometimes.
		
		// There must be no singularities *inside* the interval, of course.
		int i, j, k, kk;
		DoubleDouble za[][] = new DoubleDouble[1][2];
		DoubleDouble zb[][] = new DoubleDouble[1][2];
		int sng;
		DoubleDouble alf;
		DoubleDouble zmid[][] = new DoubleDouble[1][2];
		int n = z.length;
		if ((sing1 == null) || (sing1.length == 0)) {
			sing1 = new int[z1.length];
			for (i = 0; i < z1.length; i++) {
				sing1[i] = -1;
			}
		}
		DoubleDouble I[][] = new DoubleDouble[z1.length][2];
		int numnontriv = 0;
		for (i = 0; i < z1.length; i++) {
			if ((z1[i][0].ne(z2[i][0])) || (z1[i][1].ne(z2[i][1]))) {
				numnontriv++;
			}
		}
		int nontriv[] = new int[numnontriv];
		for (i = 0, j = 0; i < z1.length; i++) {
			if ((z1[i][0].ne(z2[i][0])) || (z1[i][1].ne(z2[i][1]))) {
				nontriv[j++] = i;
			}	
		}
		
		
		for (kk = 0; kk < numnontriv; kk++) {
			k = nontriv[kk];
			za[0][0] = z1[k][0];
			za[0][1] = z1[k][1];
			zb[0][0] = z2[k][0];
			zb[0][1] = z2[k][1];
			sng = sing1[k];
			
			// alf == 1/2 means the "1/2 rule."  Better to be more strict.
			alf = DoubleDouble.valueOf(.75);
			// Given integration length
			DoubleDouble d = zb[0][0].subtract(za[0][0]);
			
			// Compute horizontal position (signed) and vertical distance (positive)
			// from the singularities to the left endpoint.  If we are going from right,
			// to left, reverse the sense of horizontal.
			DoubleDouble sgnd = sign(d);
			DoubleDouble dx[] = new DoubleDouble[n];
			DoubleDouble dy[] = new DoubleDouble[n];
			for (i = 0; i < n; i++) {
				dx[i] = (z[i][0].subtract(za[0][0])).multiply(sgnd);
				dy[i] = (z[i][1].subtract(za[0][1])).abs();
			}
			
			// We have to be concerned with singularities lying to the right (left if
			// d < 0) of the left integration endpoint.
			boolean toright[] = new boolean[n];
			for (i = 0; i < n; i++) {
				toright[i] = (dx[i].isPositive()) && (!(z[i][0].isInfinite())) && (!(z[i][1].isInfinite()));
			}
			// For points with small enough dx, the limitation is purely due to dy.  For
			// others it must be calculated.
			boolean active[] = new boolean[n];
			int numactive = 0;
			for (i = 0; i < n; i++) {
				active[i] = (dx[i].gt(dy[i].divide(alf))) && toright[i];
				if (active[i]) {
					numactive++;
				}
			}
			// Make sure that the left endpoint won't be included
			if (sng >= 0) {
				active[sng] = false;
			}
			
			// For those active, find the integration length constraint.  This comes
			// from making the sing/right-endpoint distance equal to alf*L.
			DoubleDouble x[] = new DoubleDouble[numactive];
			DoubleDouble y[] = new DoubleDouble[numactive];
			for (i = 0, j = 0; i < n; i++) {
				if (active[i]) {
					x[j] = dx[i];
					y[j++] = dy[i];
				}
			}
			DoubleDouble L[] = new DoubleDouble[numactive];
			DoubleDouble alfSquared = alf.multiply(alf);
			DoubleDouble denom = (DoubleDouble.valueOf(1.0)).subtract(alfSquared);
			for (i = 0; i < numactive; i++) {
				DoubleDouble num1 = (alfSquared.multiply(x[i])).multiply(x[i]);
				DoubleDouble num2 = (denom.multiply(y[i])).multiply(y[i]);
				L[i] = (x[i].subtract((num1.subtract(num2)).sqrt())).divide(denom);	
			}
			
			// What if the maximum allowable integration length?
			DoubleDouble Lmin = DoubleDouble.valueOf(Double.MAX_VALUE);
			for (i = 0; i < numactive; i++) {
				if (L[i].lt(Lmin)) {
					Lmin = L[i];
				}
			}
			for (i = 0; i < n; i++) {
				if (toright[i] && (!active[i])) {
					DoubleDouble div = dy[i].divide(alf);
					if (div.lt(Lmin)) {
						Lmin = div;
					}
				}
			}
			
			if (Lmin.lt(d.abs())) {
			    // Apply stquad on the safe part and recurse on the rest
			    sgnd = sign(d);
				zmid[0][0] = za[0][0].add(Lmin.multiply(sgnd));
				zmid[0][1] = za[0][1];
				int sng1[] = new int[]{sng};
				DoubleDouble I1[][] = stquadEP(za, zmid, sng1, z, beta, qdat);
				I[k][0] = I1[0][0];
				I[k][1] = I1[0][1];
				int sngm1[] = new int[]{-1};
				DoubleDouble I2[][] = stquadhEP(zmid, zb, sngm1, z, beta, qdat);
				I[k][0] = I[k][0].add(I2[0][0]);
				I[k][1] = I[k][1].add(I2[0][1]);
			} // if (Lmin < Math.abs(d))
			else {
				// No restriction
				int sng1[] = new int[]{sng};
				DoubleDouble I3[][] = stquadEP(za, zb, sng1, z, beta, qdat);
				I[k][0] = I3[0][0];
				I[k][1] = I3[0][1];
			}
		} // for (kk = 0; kk < numnontriv; kk++) 
		return I;
	}
	
	private DoubleDouble[][] stquadEP(DoubleDouble z1[][], DoubleDouble z2[][], int sing1[], DoubleDouble z[][],
			DoubleDouble beta[], DoubleDouble qdat[][]) {
		// z1, z2, are vectors of left and right endpoints.  sing1 is a vector of integer
		// indices which label the singularities in z1.  So if sing1[5] = 3, then z1[5] = 3.
		// A -1 means no singularity.  z is the vector of *all* singularities, including the
		// "ends" of the strip at +- Infinity.  beta is the vector of associated turning angles.
		// qdat is quadrature data from scqdata.  It should include all the beta values, even
		// though the ends are never used in this manner.
		
		// stquad integrates form a possible singularity at the left end to a regular point
		// at the right.  If both endpoints are singularities, you must break the integral into
		// two pieces and make two calls.
		
		// The integral is subdivided, if necessary, so that no singularity lies closer to the
		// left endpoint than 1/2 the length of the integration (sub)interval.
		
		// Original MATLAB stquad routine copyright 1998 by Toby Driscoll.
		
		int i, j, k, kk;
		DoubleDouble za[] = new DoubleDouble[2];
		DoubleDouble zb[] = new DoubleDouble[2];
		int sng;
		DoubleDouble zr[] = new DoubleDouble[2];
		int ind;
		DoubleDouble nd[][] = new DoubleDouble[qdat.length][2];
		DoubleDouble wt[][] = new DoubleDouble[qdat.length][2];
		DoubleDouble c[] = new DoubleDouble[]{DoubleDouble.valueOf(1),DoubleDouble.valueOf(0)};
		DoubleDouble cr[] = new DoubleDouble[1];
		DoubleDouble ci[] = new DoubleDouble[1];
		DoubleDouble zl[] = new DoubleDouble[2];
		int n = z.length;
		if ((sing1 == null) || (sing1.length == 0)) {
			sing1 = new int[z1.length];
			for (i = 0; i < z1.length; i++) {
				sing1[i] = -1;
			}
		}
		DoubleDouble I[][] = new DoubleDouble[z1.length][2];
		
		int numnontriv = 0;
		for (i = 0; i < z1.length; i++) {
			if ((z1[i][0].ne(z2[i][0])) || (z1[i][1].ne(z2[i][1]))) {
				numnontriv++;
			}
		}
		int nontriv[] = new int[numnontriv];
		for (i = 0, j = 0; i < z1.length; i++) {
			if ((z1[i][0].ne(z2[i][0])) || (z1[i][1].ne(z2[i][1]))) {
				nontriv[j++] = i;
			}	
		}
		
		
		for (kk = 0; kk < numnontriv; kk++) {
			k = nontriv[kk];
			za[0] = z1[k][0];
			za[1] = z1[k][1];
			zb[0] = z2[k][0];
			zb[1] = z2[k][1];
			sng = sing1[k];
			
			// Allowable integration step, based on nearest singularity.
			DoubleDouble dist;
			DoubleDouble denom = zabs(zb[0].subtract(za[0]), zb[1].subtract(za[1]));
			DoubleDouble minVal = DoubleDouble.valueOf(Double.MAX_VALUE);
			for (j = 0; j < n; j++) {
				if (j != sng) {
					DoubleDouble val = zabs(z[j][0].subtract(za[0]), z[j][1].subtract(za[1]));
					if (val.lt(minVal)) {
						minVal = val;
					}
				}
			}
			dist = (DoubleDouble.valueOf(1.0)).min(((DoubleDouble.valueOf(2.0)).multiply(minVal)).divide(denom));
			zr[0] = za[0].add(dist.multiply(zb[0].subtract(za[0])));
			zr[1] = za[1].add(dist.multiply(zb[1].subtract(za[1])));
			ind = (sng+n+1)%(n+1);
			// Adjust Gauss-Jacobi nodes and weights to interval.
			for (i = 0; i < qdat.length; i++) {
				for (j = 0; j < 2; j++) {
					DoubleDouble diff = zr[j].subtract(za[j]);
			        nd[i][j] = (((diff.multiply(qdat[i][ind])).add(zr[j])).add(za[j])).divide(DoubleDouble.valueOf(2.0));  // G-J nodes
			        wt[i][j] = (diff.divide(DoubleDouble.valueOf(2.0))).multiply(qdat[i][ind+n+1]);  // G-J weights
				}
			} // for (i = 0; i < qdat.length; i++)
			boolean anydiffzero = false;
			if ((nd[0][0].equals(za[0])) && (nd[0][1].equals(za[1]))) {
				anydiffzero = true;
			}
			for (i = 0; i < qdat.length-1 && (!anydiffzero); i++) {
				if ((nd[i+1][0].equals(nd[i][0])) && (nd[i+1][1].equals(nd[i][1]))) {
					anydiffzero = true;
				}
			}
			if ((zr[0].equals(nd[qdat.length-1][0])) && (zr[1].equals(nd[qdat.length-1][1]))) {
				anydiffzero = true;
			}
			if (anydiffzero) {
				// Endpoints are practically coincident
				I[k][0] = DoubleDouble.valueOf(0);
				I[k][1] = DoubleDouble.valueOf(0);
			}
			else {
				// Use Gauss-Jacobi on first subinterval, if necessary.
				if (sng >= 0) {
					DoubleDouble base = zabs(zr[0].subtract(za[0]), zr[1].subtract(za[1])).divide(DoubleDouble.valueOf(2.0));
					DoubleDouble val = base.pow(beta[sng]);
					for (i = 0; i < qdat.length; i++) {
						wt[i][0] = wt[i][0].multiply(val);
						wt[i][1] = wt[i][1].multiply(val);
					}
				} // if (sng >= 0)
				DoubleDouble sde[][] = stderivEP(nd, z, beta, c, sng);
				I[k][0] = DoubleDouble.valueOf(0);
				I[k][1] = DoubleDouble.valueOf(0);
				for (i = 0; i < qdat.length; i++) {
					zmlt(sde[i][0], sde[i][1], wt[i][0], wt[i][1], cr, ci);
					I[k][0] = I[k][0].add(cr[0]);
					I[k][1] = I[k][1].add(ci[0]);
				}
				while ((dist.lt(DoubleDouble.valueOf(1))) && (!(I[k][0].isNaN())) && (!(I[k][1].isNaN()))) {
					zl[0] = zr[0];
					zl[1] = zr[1];
					denom = zabs(zl[0].subtract(zb[0]), zl[1].subtract(zb[1]));
					minVal = DoubleDouble.valueOf(Double.MAX_VALUE);
					for (i = 0; i < n; i++) {
						DoubleDouble val = zabs(z[i][0].subtract(zl[0]), z[i][1].subtract(zl[1]));
						if (val.lt(minVal)) {
							minVal = val;
						}
					} // for (i = 0; i < n; i++)
					dist = (DoubleDouble.valueOf(1.0)).min(((DoubleDouble.valueOf(2.0)).multiply(minVal)).divide(denom));
					zr[0] = zl[0].add(dist.multiply(zb[0].subtract(zl[0])));
					zr[1] = zl[1].add(dist.multiply(zb[1].subtract(zl[1])));
					for (i = 0; i < qdat.length; i++) {
						for (j = 0; j < 2; j++) {
							DoubleDouble diff = zr[j].subtract(zl[j]);
					        nd[i][j] = (((diff.multiply(qdat[i][n])).add(zr[j])).add(zl[j])).divide(DoubleDouble.valueOf(2.0));  // G-J nodes
					        wt[i][j] = (diff.divide(DoubleDouble.valueOf(2.0))).multiply(qdat[i][2*n+1]);  // G-J weights
						}
					} // for (i = 0; i < qdat.length; i++)
					sde = stderivEP(nd, z, beta, c, -1);
					for (i = 0; i < qdat.length; i++) {
						zmlt(sde[i][0], sde[i][1], wt[i][0], wt[i][1], cr, ci);
						I[k][0] = I[k][0].add(cr[0]);
						I[k][1] = I[k][1].add(ci[0]);
					}
				} // while ((dist < 1) && (!Double.isNaN(I[k][0])) && (!Double.isNaN(I[k][1])))
			} // else
		} // for (kk = 0; kk < numnontriv; kk++)
		return I;
	}
	
	private DoubleDouble[][] stderivEP(DoubleDouble zp[][], DoubleDouble z[][], DoubleDouble beta[], DoubleDouble c[], int j) {
		// Derivative of the strip map
		// stderiv returns the derivative at the points of zp of the Schwarz-Christoffel
		// strip map defined by z, beta, and c.
		
		// Original MATLAB stderiv routine copyright 1998 by Toby Driscoll.
		
		// If the fifth argument j >= 0, the terms corresponding to z[j] are normalized
		// by abs(zp-z[j]).  This is for Gauss-Jacobi quadrature.
		int i, k, m;
		DoubleDouble theta = DoubleDouble.valueOf(0.0);
		DoubleDouble z2[][] = null;
		DoubleDouble beta2[] = null;
		int n;
		DoubleDouble terms[][][];
		DoubleDouble cr[] = new DoubleDouble[1];
		DoubleDouble ci[] = new DoubleDouble[1];
		
		DoubleDouble log2 = (DoubleDouble.valueOf(2.0)).log();
		int npts = zp.length;
		DoubleDouble fprime[][] = new DoubleDouble[npts][2];
		
		// Strip out infinite prevertices
		if (z.length == beta.length) {
		    int numinf = 0;
		    for (i  = 0; i < z.length; i++) {
		    	if ((z[i][0].isInfinite()) || (z[i][1].isInfinite())) {
		    		numinf++;
		    	}
		    }
		    int ends[] = new int[numinf];
		    for (i  = 0, k = 0; i < z.length; i++) {
		    	if ((z[i][0].isInfinite()) || (z[i][1].isInfinite())) {
		    		ends[k++] = i;
		    	}
		    }
		    theta = beta[ends[1]].subtract(beta[ends[0]]);
		    if (z[ends[0]][0].isNegative()) {
		    	theta = theta.negate();
		    }
		    z2 = new DoubleDouble[z.length - numinf][2];
		    beta2 = new DoubleDouble[beta.length - numinf];
		    for (i = 0, k = 0; i < z.length; i++) {
		    	boolean keep = true;
		    	for (m = 0; m < numinf  && keep; m++) {
		    	    if (ends[m] == i) {
		    	    	keep = false;
		    	    }
		    	} // for (m = 0; m < numinf  && keep; m++)
		    	if (keep) {
		    		z2[k][0] = z[i][0];
		    		z2[k][1] = z[i][1];
		    		beta2[k++] = beta[i];
		    	}
		    } // for (i = 0, k = 0; i < z.length; i++)
		    // Adjust singularity index if given 
		    int decrementj = 0;
		    if (j > ends[0]) {
		    	decrementj++;
		    }
		    if (j > ends[1]) {
		    	decrementj++;
		    }
		    j = j - decrementj;
		} // if (z.length == beta.length)
		else {
			MipavUtil.displayError("Vector of prevertices must include +/- Infinity entries in stderiv");
			System.exit(-1);
		}
		n = z2.length;
		
		terms = new DoubleDouble[n][npts][2];
		for (i = 0; i < n; i++) {
			for (k = 0; k < npts; k++) {
				terms[i][k][0] = (DoubleDouble.PI_2.negate()).multiply(zp[k][0].subtract(z2[i][0]));
				terms[i][k][1] = (DoubleDouble.PI_2.negate()).multiply(zp[k][1].subtract(z2[i][1]));
			}
		}
		boolean lower[] = new boolean[n];
		for (i = 0; i < n; i++) {
			lower[i] = (z2[i][1].isZero());
		}
		for (i = 0; i < n; i++) {
			if (lower[i]) {
				for (k = 0; k < npts; k++) {
					terms[i][k][0] = terms[i][k][0].negate();
					terms[i][k][1] = terms[i][k][1].negate();
				}
			}
		} // for (i = 0; i < n; i++)
		DoubleDouble rt[][] = new DoubleDouble[n][npts];
		for (i = 0; i < n; i++) {
			for (k = 0; k < npts; k++) {
				rt[i][k] = terms[i][k][0];			
			}
		}
		int numbig = 0;
		int numnotbig = 0;
		boolean big[][] = new boolean[n][npts];
		for (i = 0; i < n; i++) {
			for (k = 0; k < npts; k++) {
				big[i][k] = ((rt[i][k].abs()).gt(DoubleDouble.valueOf(40.0)));
				if (big[i][k]) {
					numbig++;
				}
				else {
					numnotbig++;
				}
			}
		} // for (i = 0; i < n; i++)
		if (numnotbig > 0) {
		    for (i = 0; i < n; i++) {
		    	for (k = 0; k < npts; k++) {
		    		if (!big[i][k]) {
		    			// sinh(x + iy) = (sinhx cosy) + i(coshx siny)
		    			DoubleDouble logargr = (terms[i][k][0].cosh()).multiply(terms[i][k][1].sin());
		    			DoubleDouble logargi = ((terms[i][k][0].sinh()).multiply(terms[i][k][1].cos())).negate();
		    			terms[i][k][0] = (zabs(logargr, logargi)).log();		    			
		    			terms[i][k][1] = logargi.atan2(logargr);
		    		}
		    	}
		    }
		} // if (numnotbig > 0)
		
		for (i = 0; i < n; i++) {
			for (k = 0; k < npts; k++) {
				if (big[i][k]) {
					DoubleDouble sgn = sign(rt[i][k]);
					terms[i][k][0] = (sgn.multiply(terms[i][k][0])).subtract(log2);
					terms[i][k][1] = sgn.multiply(terms[i][k][1].subtract(DoubleDouble.PI_2));
				}
			}
		} // for (i = 0; i < n; i++)
		if (j >= 0) {
			for (k = 0; k < npts; k++) {
				terms[j][k][0] = terms[j][k][0].subtract((zabs(zp[k][0].subtract(z2[j][0]), zp[k][1].subtract(z2[j][1]))).log());
			}
		} // if (j >= 0)
		DoubleDouble sum[][] = new DoubleDouble[npts][2];
		for (i = 0; i < npts; i++) {
			sum[i][0] = DoubleDouble.valueOf(0.0);
			sum[i][1] = DoubleDouble.valueOf(0.0);
		}
		for (k = 0; k < npts; k++) {
			for (i = 0; i < n; i++) {
				sum[k][0] = sum[k][0].add(terms[i][k][0].multiply(beta2[i]));
				sum[k][1] = sum[k][1].add(terms[i][k][1].multiply(beta2[i]));
			}
		}
		DoubleDouble argr[] = new DoubleDouble[npts];
		DoubleDouble argi[] = new DoubleDouble[npts];
		for (i = 0; i < npts; i++) {
			argr[i] = ((DoubleDouble.PI_2.multiply(theta)).multiply(zp[i][0])).add(sum[i][0]);
			argi[i] = ((DoubleDouble.PI_2.multiply(theta)).multiply(zp[i][1])).add(sum[i][1]);
		}
		for (i = 0; i < npts; i++) {
			DoubleDouble expb = argr[i].exp();
			DoubleDouble expr = expb.multiply(argi[i].cos());
			DoubleDouble expi = expb.multiply(argi[i].sin());
			zmlt(c[0], c[1], expr, expi, cr, ci);
			fprime[i][0] = cr[0];
			fprime[i][1] = ci[0];
		}
		return fprime;
	}

}