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
import gov.nih.mipav.model.algorithms.SchwarzChristoffelMapping.ODEModel;
import gov.nih.mipav.model.algorithms.SchwarzChristoffelMapping.dpfun;
import gov.nih.mipav.model.algorithms.SchwarzChristoffelMapping.polygon;
import gov.nih.mipav.model.algorithms.SchwarzChristoffelMapping.qlgraph;
import gov.nih.mipav.model.algorithms.SchwarzChristoffelMapping.scmap;
import gov.nih.mipav.model.structures.ModelImage;
import gov.nih.mipav.model.structures.jama.ComplexLinearEquations;
import gov.nih.mipav.model.structures.jama.GeneralizedEigenvalue;
import gov.nih.mipav.model.structures.jama.LinearEquations2;
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
			testHplmap2();
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

		// First hpevalinv works okay and only goes to k = 3 in the loop in if (newton) in hpinvmap.  The second hpevalinv
		// generates NaNs and infinities at k = 21 in the loop in if (newton) in hpinvmap.  At k = 21 the F/dF division has
		//  dF values like dF = 0 + 0i or dF = 0 + 3.95E-311i which generate NaNs and infinities in Fdiv.
		// If you try to use only the ODE and specify newton = false, the ODE finds the equation too stiff to succeed.
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
			    wp[101*j + i][0] = X[i][j];
			    wp[101*j + i][1] = Y[i][j] + M * X[i][j]/a;
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
		for (i = 0; i < 101; i++) {
			for (j = 0; j < 15; j++) {
			    wp[101*j + i][0] = Y[i][j] + M * X[i][j]/a;
			    wp[101*j + i][1] = X[i][j];
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
		    	hpmap(w0, z0, w, beta, z, c, qdat);
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
			    hpmap(wpnotdone2, znnotdone, w, beta, z, c, qdat);
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
				zp[i-lenzp][1] = Math.max(0.0, yy[i]);;
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
		// exterior map of vertical lines whose real parrts are given in re and
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
				hpmap(neww, zpnew, w, beta, z, c, qdat);
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
 				hpmap(neww, zpnew, w, beta, z, c, qdat);
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
	
	public void hpmap(double wp[][], double zp[][], double w[][], double beta[], double z[][], 
			double c[], double qdat[][]) {
		// Schwarz-Christoffel half-plane map
		// hpmap computes the values of the Schwarz-Christoffel half-plane map at the points in
		// vector zp.  The polygon's vertices should be given in w and the arguments z, c, and
		// qdat should be computed by hpparam.  hpmap returns a vector wp the same size as zp.
		// Original MATLAB hpmap routine copyright 1998 by Toby Driscoll.
		int i, j;
		double betan[];
		double zn[][];
		int nqpts;
		double tol;
		double qdat2[][] = null;
		int p;
		double mid[][] = null;
		double cre[] = new double[1];
		double cim[] = new double[1];
		
		if ((zp == null) || (zp.length == 0)) {
			wp = null;
			return;
		}
		int n = w.length;
		betan = new double[n-1];
		zn = new double[n-1][2];
	    for (i = 0; i < n-1; i++) {
	    	betan[i] = beta[i];
	    	zn[i][0] = z[i][0];
	    	zn[i][1] = z[i][1];
	    }
		
		// Quadrature data and error tolerance
		if ((qdat == null) || (qdat.length == 0)) {
		    tol = 1.0E-8;
		    nqpts = 8;
		    qdat2 = new double[nqpts][2*betan.length+2];
			scm.scqdata(qdat2, betan, nqpts);	
		}
		else if (qdat.length == 1) {
			tol = qdat[0][0];
			nqpts = Math.max((int)Math.ceil(-Math.log10(tol)), 8);
			qdat2 = new double[nqpts][2*betan.length+2];
			scm.scqdata(qdat2, betan, nqpts);
		}
		else {
			tol = Math.pow(10.0, -qdat.length);
			qdat2 = qdat;
		}
		
		p = zp.length;
		for (i = 0; i < p; i++) {
			wp[i][0] = 0.0;
			wp[i][1] = 0.0;
		}
		
		// For each point in zp, find nearest prevertex.
		double dist[] = new double[p];
		for (i = 0; i < p; i++) {
			dist[i] = Double.MAX_VALUE;
		}
		int sing[] = new int[p];  // indices of prevertices
		for (j = 0; j < p; j++) {
			for (i = 0; i < n; i++) {
				double currentDist = scm.zabs(zp[j][0]-z[i][0],zp[j][1]-z[i][1]);
				if (currentDist < dist[j]) {
				    dist[j] = currentDist;
				    sing[j] = i;
				}
			}
		}
		
		// Screen out images of prevertices
		boolean vertex[] = new boolean[p];
		for (i = 0; i < p; i++) {
			vertex[i] = (dist[i] < tol);
		}
		for (i = 0; i < p; i++) {
			if (vertex[i]) {
				wp[i][0] = w[sing[i]][0];
				wp[i][1] = w[sing[i]][1];
			}
		}
		boolean zpinf[] = new boolean[p];
		for (i = 0; i < p; i++) {
			zpinf[i] = (Double.isInfinite(zp[i][0]) || Double.isInfinite(zp[i][1]));
		}
		for (i = 0; i < p; i++) {
			if (zpinf[i]) {
				wp[i][0] = w[n-1][0];
				wp[i][1] = w[n-1][1];
			}
		}
		for (i = 0; i < p; i++) {
			vertex[i] = vertex[i] || zpinf[i];
		}
		
		// "Bad" points are closest to a prevertex of infinity.
		int numinf = 0;
		for (i = 0; i < w.length; i++) {
			if (Double.isInfinite(w[i][0]) || Double.isInfinite(w[i][1])) {
				numinf++;
			}
		}
		int atinf[] = new int[numinf];
		for (i = 0, j = 0; i < w.length; i++) {
			if (Double.isInfinite(w[i][0]) || Double.isInfinite(w[i][1])) {
				atinf[j++] = i;
			}
		}
		boolean member[] = new boolean[p];
		for (i = 0; i < p; i++) {
			for (j = 0; j < numinf; j++) {
				if (sing[i] == atinf[j]) {
					member[i] = true;
				}
			}
		}
		boolean bad[] = new boolean[p];
		int numbad = 0;
		for (i = 0; i < p; i++) {
		    bad[i] = member[i] && (!vertex[i]);
		    if (bad[i]) {
		    	numbad++;
		    }
		}
		
		if (numbad > 0) {
			// Can't integrate starting at pre-infinity: which neighboring
			// prevertex to use;
			double direcn;
			mid = new double[numbad][2];
			for (i = 0, j = 0; i < p; i++) {
				if (bad[i]) {
					direcn = zp[i][0] - z[sing[i]][0];
					if (direcn >= 0) {
						sing[i] = sing[i] + 1;
					}
					else {
						sing[i] = sing[i] - 1;
					}
					// Midpoints of these integrations
					mid[j][0] = (z[sing[i]][0] + zp[i][0])/2;
					mid[j++][1] = (z[sing[i]][1] + zp[i][1])/2;
				}
			}
		} // if (numbad > 0)
		
		// za = the starting singularities
		double zs[][] = new double[p][2];
		double ws[][] = new double[p][2];
		for (i = 0; i < p; i++) {
			j = sing[i];
			zs[i][0] = z[j][0];
			zs[i][1] = z[j][1];
			ws[i][0] = w[j][0];
			ws[i][1] = w[j][1];
		}
		
		// Compute the map directly at "normal" ppoints
		boolean normal[] = new boolean[p];
		int numnormal = 0;
		for (i = 0; i < p; i++) {
			normal[i] = (!bad[i]) && (!vertex[i]);
			if (normal[i]) {
				numnormal++;
			}
		}
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
			}
			double I[][] = hpquad(zsnormal, zpnormal, singnormal, zn, betan, qdat);
			double cI[][] = new double[numnormal][2];
			for (i = 0; i < numnormal; i++) {
				scm.zmlt(c[0], c[1], I[i][0], I[i][1], cre, cim);
				cI[i][0] = cre[0];
				cI[i][1] = cim[0];
			}
			for (i = 0, j = 0; i < p; i++) {
				if (normal[i]) {
					wp[i][0] = ws[i][0] + cI[j][0];
					wp[i][1] = ws[i][1] + cI[j++][1];
				}
			}
		} // if (numnormal > 0)
		
		// Compute map at "bad" points, in stages.  Stop at midpoint to avoid
		// integration where right endpoint is close to a singularity.
		if (numbad > 0) {
			double zsbad[][] = new double[numbad][2];
			int singbad[] = new int[numbad];
			double zpbad[][] = new double[numbad][2];
			int singnone[] = new int[numbad];
			for (i = 0; i < p; i++) {
				if (bad[i]) {
					zsbad[j][0] = zs[i][0];
					zsbad[j][1] = zs[i][1];
					singbad[j] = sing[i];
					zpbad[j][0] = zp[i][0];
					zpbad[j][1] = zp[i][1];
					singnone[j++] = -1;
				}
			}
			double I1[][] = hpquad(zsbad, mid, singbad, zn, betan, qdat);
			double I2[][] = hpquad(zpbad, mid, singnone, zn, betan, qdat);
			double I[][] = new double[numbad][2];
			for (i = 0; i < numbad; i++) {
				I[i][0] = I1[i][0] - I2[i][0];
				I[i][1] = I1[i][1] - I2[i][1];
			}
			double cI[][] = new double[numbad][2];
			for (i = 0; i < numbad; i++) {
				scm.zmlt(c[0], c[1], I[i][0], I[i][1], cre, cim);
				cI[i][0] = cre[0];
				cI[i][1] = cim[0];
			}
			for (i = 0, j = 0; i < p; i++) {
				if (bad[i]) {
					wp[i][0] = ws[i][0] + cI[j][0];
					wp[i][1] = ws[i][1] + cI[j++][1];
				}
			}
		} // if (numbad > 0)
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
		    double I1[][] = hpquad(z00, mid, sing0, zn, betan, qdata);
		    double I2[][] = hpquad(zidx, mid, singidx, zn, betan, qdata);
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
		double I1[][] = hpquad(ze0, mid, endpt0, zn, betan, qdata);
		double I2[][] = hpquad(ze1, mid, endpt1, zn, betan, qdata);
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
		double I1[][] = hpquad(z0, mid, idx20, zn, betan, qdata);
		double I2[][] = hpquad(z1, mid, idx21, zn, betan, qdata);
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
		double I1[][] = hpquad(z1, mid, sing1, zn, betan, qdat);
		double I2[][] = hpquad(z00, mid, sing0, zn, betan, qdat);
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
				    I1 = hpquad(zleft, mid, left, z, beta, qdat);
				    I2 = hpquad(zright, mid, right, z, beta, qdat);
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

	private double[][] hpquad(double z1[][], double z2[][], int sing1[], double z[][], double beta[], double qdat[][]) {
		// Numerical quadrature for the half-plane map.
		// z1, z2 are vectors of left and right endpoints. sing1 is a vector of
		// integer indices which label the singularities in z1. So if sing1[5] =
		// 3,
		// then z1[]5 = z[3],. A -1 means no singularity. A is the vector of
		// finite
		// singularities; beta is the vector of associated turning angles. qdata
		// is
		// quadrature data from scqdata.

		// hpquad integrates from a possible singularity at the left end to a
		// regular point at the right. If both endpoints are singularities,
		// you must break the integral into two pieces and make two calls, or
		// call hpquad(z1,z2,sing1,sing2,z,beta,qdat) and accept an automatic
		// choice.

		// The integral is subdivided, if necessary, so that no singularity
		// lies closer to the left endpoint than 1/2 the length of the
		// integration (sub)interval.

		// Original MATLAB routine copyright 1998 by Toby Driscoll.
		int i, j, k, m;
		double cr[] = new double[1];
		double ci[] = new double[1];
		int nqpts = qdat.length;
		// Note: Here n is the total number of *finite* singularities; i.e., the
		// number of terms in the product appearing in the integrand.
		int n = z.length;
		double bigz[][][] = new double[n][nqpts][2];
		for (i = 0; i < n; i++) {
			for (j = 0; j < nqpts; j++) {
				bigz[i][j][0] = z[i][0];
				bigz[i][j][1] = z[i][1];
			}
		}
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
		double termsr;
		double termsi;

		for (i = 0; i < nontriv.length; i++) {
			k = nontriv[i];
			za[0] = z1[k][0];
			za[1] = z1[k][1];
			zb[0] = z2[k][0];
			zb[1] = z2[k][1];
			int sng = sing1[k];

			// Allowable integration step, based on nearest singularity.
			double dist = 1.0;
			double denom = scm.zabs(zb[0] - za[0], zb[1] - za[1]);
			double minVal = Double.MAX_VALUE;
			double absDiff;
			for (j = 0; j <= sng - 1; j++) {
				absDiff = scm.zabs(z[j][0] - za[0], z[j][1] - za[1]);
				if (absDiff < minVal) {
					minVal = absDiff;
				}
			} // for (j = 0; j <=sng-1; j++)
			for (j = sng + 1; j <= n - 1; j++) {
				absDiff = scm.zabs(z[j][0] - za[0], z[j][1] - za[1]);
				if (absDiff < minVal) {
					minVal = absDiff;
				}
			} // for (j = sng+1; j <= n-1; j++)
			minVal = 2.0 * minVal / denom;
			if (minVal < dist) {
				dist = minVal;
			}
			zr[0] = za[0] + dist * (zb[0] - za[0]);
			zr[1] = za[1] + dist * (zb[1] - za[1]);
			// Adjust Gauss-Jacobi nodes and weights to interval.
			int ind = (sng + n + 1) % (n + 1);
			for (j = 0; j < nqpts; j++) {
				nd[j][0] = ((zr[0] - za[0]) * qdat[j][ind] + zr[0] + za[0]) / 2.0; // G-J
																					// nodes
				nd[j][1] = ((zr[1] - za[1]) * qdat[j][ind] + zr[1] + za[1]) / 2.0;
				wt[j][0] = ((zr[0] - za[0]) / 2.0) * qdat[j][ind + n + 1];
				wt[j][1] = ((zr[1] - za[1]) / 2.0) * qdat[j][ind + n + 1];// G-J
																			// weights
			} // for (j = 0; j < nqpts; j++)
			int zeroterms = 0;
			for (j = 0; j < n; j++) {
				for (m = 0; m < nqpts; m++) {
					terms[j][m][0] = nd[m][0] - bigz[j][m][0];
					terms[j][m][1] = nd[m][1] - bigz[j][m][1];
					if ((terms[j][m][0] == 0) && (terms[j][m][1] == 0)) {
						zeroterms++;
					}
				}
			} // for (j = 0; j < n; j++)
			if (zeroterms > 0) {
				// Endpoints are practically coincident
				I[k][0] = 0;
				I[k][1] = 0;
			} else {
				// Use Gauss-Jacobi on first subinterval, if necessary.
				if (sng >= 0) {
					double fac = scm.zabs(zr[0] - za[0], zr[1] - za[1]) / 2.0;
					double fac2 = Math.pow(fac, beta[sng]);
					for (m = 0; m < nqpts; m++) {
						denom = scm.zabs(terms[sng][m][0], terms[sng][m][1]);
						terms[sng][m][0] = terms[sng][m][0] / denom;
						terms[sng][m][1] = terms[sng][m][1] / denom;
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
						logterms[0] = Math.log(scm.zabs(terms[j][m][0], terms[j][m][1]));
						logterms[1] = Math.atan2(terms[j][m][1], terms[j][m][0]);
						prod[0] = logterms[0] * bigbeta[j][m];
						prod[1] = logterms[1] * bigbeta[j][m];
						expSum[0] += prod[0];
						expSum[1] += prod[1];
					} // for (j = 0; j < n; j++)
					expTerm = Math.exp(expSum[0]);
					scm.zmlt(expTerm * Math.cos(expSum[1]), expTerm * Math.sin(expSum[1]), wt[m][0], wt[m][1], cr, ci);
					I[k][0] += cr[0];
					I[k][1] += ci[0];
				} // for (m = 0; m < nqpts; m++)
				while (dist < 1) {
					// Do regular Gaussian quad on other subintervals.
					zl[0] = zr[0];
					zl[1] = zr[1];
					dist = 1.0;
					minVal = Double.MAX_VALUE;
					denom = scm.zabs(zl[0] - zb[0], zl[1] - zb[1]);
					for (j = 0; j < n; j++) {
						double num = scm.zabs(z[j][0] - zl[0], z[j][1] - zl[1]);
						if (num < minVal) {
							minVal = num;
						}
					} // (j = 0; j < n; j++)
					minVal = 2.0 * minVal / denom;
					if (minVal < dist) {
						dist = minVal;
					}
					zr[0] = zl[0] + dist * (zb[0] - zl[0]);
					zr[1] = zl[1] + dist * (zb[1] - zl[1]);
					for (j = 0; j < nqpts; j++) {
						nd[j][0] = ((zr[0] - zl[0]) * qdat[j][n] + zr[0] + zl[0]) / 2.0;
						nd[j][1] = ((zr[1] - zl[1]) * qdat[j][n] + zr[1] + zl[1]) / 2.0;
						wt[j][0] = ((zr[0] - zl[0]) / 2.0) * qdat[j][2 * n + 1];
						wt[j][1] = ((zr[1] - zl[1]) / 2.0) * qdat[j][2 * n + 1];
					} // for (j = 0; j < nqpts; j++)
					for (m = 0; m < nqpts; m++) {
						expSum[0] = 0;
						expSum[1] = 0;
						for (j = 0; j < n; j++) {
							termsr = nd[m][0] - bigz[j][m][0];
							termsi = nd[m][1] - bigz[j][m][1];
							logterms[0] = Math.log(scm.zabs(termsr, termsi));
							logterms[1] = Math.atan2(termsi, termsr);
							prod[0] = logterms[0] * bigbeta[j][m];
							prod[1] = logterms[1] * bigbeta[j][m];
							expSum[0] += prod[0];
							expSum[1] += prod[1];
						}
						expTerm = Math.exp(expSum[0]);
						scm.zmlt(expTerm * Math.cos(expSum[1]), expTerm * Math.sin(expSum[1]), wt[m][0], wt[m][1], cr,
								ci);
						I[k][0] += cr[0];
						I[k][1] += ci[0];
					} // for (m = 0; m < nqpts; m++)
				} // while (dist < 1)
			} // else
		} // for (i = 0; i < nontriv.length; i++)
		return I;
	}

}