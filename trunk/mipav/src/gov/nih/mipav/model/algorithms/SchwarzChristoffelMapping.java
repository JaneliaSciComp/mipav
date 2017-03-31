package gov.nih.mipav.model.algorithms;

import gov.nih.mipav.model.algorithms.AlgorithmFRAP.FitFullIntModel;
import gov.nih.mipav.model.structures.*;
import gov.nih.mipav.view.*;

import java.io.*;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.Collections;
import java.util.Comparator;

import de.jtem.numericalMethods.algebra.linear.decompose.Eigenvalue;
import de.jtem.numericalMethods.calculus.integration.RungeKuttaFehlbergIntegrator;

public class SchwarzChristoffelMapping extends AlgorithmBase {
	
	// This is a port of portions of the Schwarz-Christoffel Toolbox from MATLAB to Java
	// with the kind permission of Professor Toby Driscoll.  The original code is:
	// Version 2.3   January 15, 2003
	// Copyright (c) 1994-2003 by Toby Driscoll (driscoll@math.udel.edu).

	// How much progress information to show during and after the solution to the parameter problem.
	private boolean traceSolution = false;
	
	// Desired accuracy in the map.  This may not be met exactly.
	private double tolerance = 1.0E-8;
	
	// eps returns the distance from 1.0 to the next larger double-precision number, that is, eps = 2^-52.
	private double eps;
	
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
	// If NESolve, method will be be NESolve.TRUST_REGION (default) or NESolve.LINE_SEARCH.
	private void rparam(double z[][], double c[], double L[][], double qdat[][], 
			double w[][], double beta[], int cnr[], double z0[][], boolean trace, double tol,
			int method) {
		int i;
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
	    int n = w.length;  // number of vertices
	    
	    // cnr should be set by initial MIPAV dialog, so this should never happen.
	    // The polygon as specified by w and beta is drawn on the image and the
	    // user selects 4 vertices using the mouse.
	    //if ((cnr == null) || (cnr.length == 0)) {
	    	//String msg[] = new String[2];
	        //msg[0] = "Select the images of the corners of the rectangle";
	        //msg[1] = "Go in counerclockwise order and select a long rectangle edge first";
	        //cnr = scselect(w, beta, 4, "Select corners", msg);
	    // } // if ((cnr == null) || (cnr.length == 0))
	
	    // Renumber the vertices so that cnr[0] = 0.
	    double wtemp[][] = new double[n][2];
	    double betatemp[] = new double[n];
	    for (i = cnr[0]; i < n; i++) {
	        wtemp[i-cnr[0]][0] = w[i][0];
	        wtemp[i-cnr[0]][1] = w[i][1];
	        betatemp[i-cnr[0]] = beta[i];
	    }
	    for (i = 0; i <= cnr[0]-1; i++) {
	    	wtemp[i+n-cnr[0]][0] = w[i][0];
	    	wtemp[i+n-cnr[0]][1] = w[i][1];
	    	betatemp[i+n-cnr[0]] = beta[i];
	    }
	    for (i = 0; i < n; i++) {
	    	w[i][0] = wtemp[i][0];
	    	w[i][1] = wtemp[i][1];
	    	beta[i] = betatemp[i];
	    }
	    for (i = 0; i < n; i++) {
	    	wtemp[i] = null;
	    }
	    wtemp = null;
	    betatemp = null;
	    int offset = n - cnr[0]; // cnr - cnr[0] so no need to add one
	    for (i = 0; i < n; i++) {
	    	cnr[i] = ((cnr[i] + offset) % n);
	    }
	    
	    if (z0.length == 1) {
	    	tol = z0[0][0];
	    	z0 = null;
	    }
	    int nqpts = Math.max((int)Math.ceil(-Math.log10(tol)), 4);
	    qdat = scqdata(beta, nqpts);  // quadrature data
	    
	    // Check input data
	    int err = sccheck("r", w, beta, cnr);
	    if (err == -1) {
	    	return;
	    }
	    if (err == 1) {
	    	MipavUtil.displayError("Use scfix to make polygon obey requirements");
	    	return;
	    }
	    
	    boolean atinf[] = new boolean[n];
	    for (i = 0; i < n; i++) {
	    	atinf[i] = (beta[i] <= -1);
	    }
	    
	    if (z0 == null) {
	        // Try to find a reasonable initial guess
	    	// side lengths
	    	double dw[]= new double[n];
	    	for (i = 0; i < n-1; i++) {
	    		diffR = w[i+1][0] - w[i][0];
	    		diffI = w[i+1][1] - w[i][1];
	    		dw[i] = Math.sqrt(diffR*diffR + diffI*diffI);
	    	} // for (i = 0; i < n-1; i++)
	    	diffR = w[0][0] - w[n-1][0];
	    	diffI = w[0][1] - w[n-1][1];
	    	dw[n-1] = Math.sqrt(diffR*diffR + diffI*diffI);
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
	        for (i = cnr[0]; i <= cnr[1]-1; i++) {
	        	sum1 += dw[i];
	        }
	        for (i = cnr[2]; i <= cnr[3]-1; i++) {
	        	sum2 += dw[i];
	        }
	        len = (sum1 + sum2)/2.0;
	        sum1 = 0.0;
	        sum2 = 0.0;
	        for (i = cnr[1]; i <= cnr[2]-1; i++) {
	        	sum1 += dw[i];
	        }
	        for (i = cnr[3]; i <= n-1; i++) {
	        	sum2 += dw[i];
	        }
	        for (i = 0; i <= cnr[0]-1; i++) {
	        	sum2 += dw[i];
	        }
	        wid = (sum1 + sum2)/2.0;
	        modest = Math.min(len/wid, 100);
	        // Evenly space prevertices to match this conformal modulus.
	        npoints = cnr[1] - cnr[0] +1;
	        spacing = modest/(npoints - 1.0);
	        z0 = new double[n][2];
	        for (i = 0; i < npoints-1; i++) {
	            z0[cnr[0]+i][0] = i*spacing;	
	        }
	        z0[cnr[1]][0] = modest;
	        dx = z0[cnr[0]+1][0] - z0[cnr[0]][0];
	        for (i = 1; i <= cnr[0]-1; i++) {
	        	z0[cnr[0]-i][0] = z0[cnr[0]][0] - dx * i;
	        }
	        upper = cnr[2] - cnr[1] - 1;
	        for (i = 1; i <= upper; i++) {
	        	z0[cnr[1]+i][0] = z0[cnr[1]][0] + dx * i;
	        }
	        npoints = cnr[3] - cnr[2] + 1;
	        spacing = modest/(npoints - 1.0);
	        for (i = 0; i < npoints-1; i++) {
	        	z0[cnr[3]-i][0] = i*spacing;
	        	z0[cnr[3]-i][1] = 1;
	        }
	        z0[cnr[2]][0] = modest; 
	        z0[cnr[2]][1] = 1;
	        dx = z0[cnr[3]-1][0] - z0[cnr[3]][0];
	        for (i = 1; i <= n-1-cnr[3]; i++) {
	        	z0[cnr[3]+i][0] = z0[cnr[3]][0] - dx*i;
	        	z0[cnr[3]+i][1] = 1;
	        }
	    } // if (z0 == null)
	    else {
	    	if (z0.length != n) {
	    		MipavUtil.displayError("Initial guess has wrong number of prevertices");
	    		return;
	    	}
	    	double z0temp[][] = new double[n][2];
	    	for (i = cnr[0]; i < n; i++) {
		        z0temp[i-cnr[0]][0] = z0[i][0];
		        z0temp[i-cnr[0]][1] = z0[i][1];
		    }
		    for (i = 0; i <= cnr[0]-1; i++) {
		    	z0temp[i+n-cnr[0]][0] = z0[i][0];
		    	z0temp[i+n-cnr[0]][1] = z0[i][1];
		    }
		    for (i = 0; i < n; i++) {
		    	z0[i][0] = z0temp[i][0];
		    	z0[i][1] = Math.round(z0temp[i][1]);
		    }
		    for (i = 0; i < n; i++) {
		    	z0temp[i] = null;
		    }
		    z0temp = null;
            for (i = cnr[0]; i <= cnr[1]; i++) {
            	if (z0[i][1] == 0) {
            		MipavUtil.displayError("Initial guess has prevertices on wrong side of strip");
            		return;
            	}
            } // for (i = cnr[0]; i <= cnr[1]; i++)
            for (i = cnr[2]; i <= cnr[3]; i++) {
            	if (z0[i][1] == 0) {
            		MipavUtil.displayError("Initial guess has prevertices on wrong side of strip");
            		return;
            	}
            } // for (i = cnr[2]; i <= cnr[3]; i++)
	    } // else
	    
	    // Convert z0 to unconstrained vars
	    double y0[][] = new double[n-3][2];
	    double dz[][] = new double[n-1][2];
	    for (i = 0; i < n-1; i++) {
	    	dz[i][0] = z0[i+1][0] - z0[i][0];
	    	dz[i][1] = z0[i+1][1] - z0[i][1];
	    }
	    for (i = cnr[2]; i < n-1; i++) {
	    	dz[i][0] = -dz[i][0];
	    	dz[i][1] = -dz[i][1];
	    }
	    for (i = 0; i <= cnr[1]-2; i++) {
	    	y0[i][0] = Math.log(Math.sqrt(dz[i][0]*dz[i][0] + dz[i][1]*dz[i][1]));
	    	y0[i][1] = Math.atan2(dz[i][1], dz[i][0]);
	    }
	    for (i = cnr[2]-1; i <= cnr[3]-3; i++) {
	    	y0[i][0] = Math.log(Math.sqrt(dz[i+2][0]*dz[i+2][0] + dz[i+2][1]*dz[i+2][1]));
	    	y0[i][1] = Math.atan2(dz[i+2][1], dz[i+2][0]);	
	    }
	    y0[cnr[1]-1][0] = (Math.log(Math.sqrt(dz[cnr[1]-1][0]*dz[cnr[1]-1][0] + dz[cnr[1]-1][1]*dz[cnr[1]-1][1])) +
	    		(Math.log(Math.sqrt(dz[cnr[2]][0]*dz[cnr[2]][0] + dz[cnr[2]][1]*dz[cnr[2]][1])))/2.0);
	    y0[cnr[1]-1][1] = (Math.atan2(dz[cnr[1]-1][1],dz[cnr[1]-1][0]) + Math.atan2(dz[cnr[2]][1], dz[cnr[2]][0])/2.0);
	    
	    // Vertices on the "short" edges are transformed into the interval [-1,1],
	    // and then the Trefethen-style transformation is used.
	    L[0][0] = z0[cnr[1]][0] - z0[cnr[0]][0];
	    L[0][1] = z0[cnr[1]][1] - z0[cnr[0]][1];
	    double x[] = new double[cnr[2]-cnr[1]-1];
	    for (i = cnr[1]+1; i <= cnr[2]-1; i++) {
	    	x[i-cnr[1]-1] = Math.exp(Math.PI*(L[0][0] - z0[i][0]))*Math.cos(Math.PI*(L[0][1] + z0[i][1]));
	    }
	    double dx2[] = new double[x.length+1];
	    dx2[0] = 1 - x[0];
	    for (i = 0; i < x.length-1; i++) {
	    	dx2[i+1] = x[i] - x[i+1];
	    }
	    dx2[x.length] = x[x.length-1] + 1;
	    for (i = 0; i <= dx2.length-2; i++) {
	        y0[i+cnr[1]][0] = Math.log(dx2[i]/dx2[i+1]);
	        y0[i+cnr[1]][1] = 0;
	    }
	    x = new double[n-1-cnr[3]];
	    for (i = 0; i < n-1-cnr[3]; i++) {
	    	x[i] = Math.exp(Math.PI*z0[cnr[3]+1+i][0])*Math.cos(Math.PI*z0[cnr[3]+1+i][1]);
	    }
	    dx2[0] = x[0]+1;
	    for (i = 0; i < x.length-1; i++) {
	    	dx2[i+1] = x[i+1] - x[i];
	    }
	    dx2[x.length] = 1 - x[x.length-1];
	    for (i = 0; i <= dx2.length-2; i++) {
	        y0[i+cnr[3]-2][0] = Math.log(dx2[i]/dx2[i+1]);
	        y0[i+cnr[3]-2][1] = 0;
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
		    	right[jr++] = i;
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
	    double wdenomreal = w[1][0] - w[0][0];
	    double wdenomimag = w[1][1] - w[0][1];
	    // Multiply 1/(wdenomreal + i*wdenomimag) by (wdenomreal - i * wdenomimag)/(wdenomreal - i * wdenomimag)
	    // Giving (wdenomreal - i * wdenomimag)/(wdenomreal**2 + wdenomimag**2)
	    double denom = wdenomreal*wdenomreal + wdenomimag*wdenomimag;
	    // First entry is useless = 1.
	    double nmlen[][] = new double[left.length-1][2];
	    for (i = 1; i < left.length; i++) {
	    	double realpart = w[right[i]][0] - w[left[i]][0];
	    	double imagpart = w[right[i]][1] - w[left[i]][1];
	    	nmlen[i-1][0] = (realpart*wdenomreal + imagpart*wdenomimag)/denom;
	    	nmlen[i-1][1] = (imagpart*wdenomreal - realpart*wdenomimag)/denom;
	    	// Absolute value for finite ones
	    	if (!cmplx[i]) {
	    		nmlen[i-1][0] = Math.sqrt(nmlen[i-1][0]*nmlen[i-1][0] + nmlen[i-1][1]*nmlen[i-1][1]);
	    		nmlen[i-1][1] = 0;
	    	}
	    } // for (i = 1; i < left.length; i++)
	    
	    // Solve nonlinear system of equations
	    double y0sep[] = new double[2*y0.length];
	    for (i = 0; i < y0.length; i++) {
	    	y0sep[2*i] = y0[i][0];
	    	y0sep[2*i+1] = y0[i][1];
	    }
	    rpfun fm = new rpfun(y0sep, n, beta, nmlen, left, right, cmplx, qdat, cnr);
	    fm.driver();
		fm.dumpResults();
	    int exitStatus = fm.getExitStatus();
	    if (exitStatus < 0 ) {
	    	printExitStatus(exitStatus);
	    	return;
	    }
		double params[] = fm.getParameters();
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
	
	class rpfun extends NLConstrainedEngine {
		int n;
		double beta[];
		double nmlen[][];
		int left[];
		int right[];
		boolean cmplx[];
		double qdat[][];
		int corners[];
		
    	public rpfun (double y0sep[], int n, double beta[], double nmlen[][], int left[],
    			int right[], boolean cmplx[], double qdat[][], int corners[]) {
    		// nPoints, params
    		super(y0sep.length, y0sep.length);
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
			for (int i = 0; i < y0sep.length; i++) {
				gues[i] = y0sep[i];
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
    		int i, j;
    		double z[][];
    		double I1[][];
    		double I2[][];
    		try {
				ctrl = ctrlMat[0];

				if ((ctrl == -1) || (ctrl == 1)) {
                    // Returns residual for solution of nonlinear equations
					
					// Transform y (unconstrained variables) to z (actual params)
					double y[][] = new double[a.length/2][2];
					for (i = 0; i < a.length; i++) {
						if ((i % 2) == 0) {
							y[i/2][0] = a[i];
						}
						else {
							y[i/2][1] = a[i];
						}
					}
					z = rptrnsfm(y, corners);
					
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
						z2[i][0] = z[i][1];
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
					// Put dummy columns into qdat at ends
					double qdat2[][] = new double[qdat.length][qdat[0].length+2];
					for (i = 0; i <= ends[0]; i++) {
						qdat2[i][qdat[0].length] = i;
						qdat2[i][qdat[0].length+1] = i+n+1;
					}
					qdat2[ends[0]+1][qdat[0].length] = n;
					qdat2[ends[0]+1][qdat[0].length+1] = n+n+1;
					for (i = ends[0]+1; i <= ends[1]; i++) {
						qdat2[i+1][qdat[0].length] = i;
						qdat2[i+1][qdat[0].length+1] = i+n+1;	
					}
					qdat2[ends[1]+2][qdat[0].length] = n;
					qdat2[ends[1]+2][qdat[0].length+1] = n+n+1;
					for (i = ends[1]+1; i <= n-1; i++) {
						qdat2[i+2][qdat[0].length] = i;
						qdat2[i+2][qdat[0].length+1] = i+n+1;	
					}
					qdat2[n+2][qdat[0].length] = n;
					qdat2[n+2][qdat[0].length+1] = n+n+1;
					// Change singularity indices to reflect ends
					for (i = 0; i < left.length; i++) {
						if (left[i] > ends[0]) {
							left[i]++;
						}
						if (left[i] > ends[1]) {
							left[i]++;
						}
					} // for (i = 0; i < left.length; i++)
					for (i = 0; i < right.length; i++) {
						if (right[i] > ends[0]) {
							right[i]++;
						}
						if (right[i] > ends[1]) {
							right[i]++;
						}
					} // for (i = 0; i < right.length; i++)
					
					double ints[][] = new double[zleft.length][2];
					int nums2 = 0;
					for (i = 0; i < left.length; i++) {
						if ((right[i] - left[i] == 1) && (zleft[i][1] == zright[i][1])) {
							nums2++;
						}
					}
					int s2[] = new int[nums2];
					for (i = 0, j = 0; i < left.length; i++) {
						if ((right[i] - left[i] == 1) && (zleft[i][1] == zright[i][1])) {
							s2[j++] = i;
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
				    	lefts2[i] = left[s2[i]];
				    	zrights2[i][0] = zright[s2[i]][0];
				    	zrights2[i][1] = zright[s2[i]][1];
				    	rights2[i] = right[s2[i]];
				    }
				    I1 = stquadh(zlefts2, mid, lefts2, z, beta, qdat);
				    I2 = stquadh(zrights2, mid, rights2, z, beta, qdat);
				    for (i = 0, j = 0; i < s2.length; i++) {
				        ints[s2[j]][0] = I1[i][0] - I2[i][0];
				        ints[s2[j++]][1] = I1[i][1] - I2[i][1];
				    }
				    
				    // Three-stage integrations
				    int lengthnots2 = zleft.length - s2.length;
				    int nots2[] = new int[lengthnots2];
				    for (i = 0, j = 0; i < left.length; i++) {
				    	if ((right[i] - left[i] != 1) || (zleft[i][1] != zright[i][1])) {
				    	    nots2[j++] = i;	
				    	}
				    }
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
				    	leftns2[i] = left[nots2[i]];
				    	zrightns2[i][0] = zright[nots2[i]][0];
				    	zrightns2[i][1] = zright[nots2[i]][1];
				    	rightns2[i] = right[nots2[i]];
				    }
				    double zero[][] = new double[nots2.length][2];
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
	
	private double[][] stquadh(double z1[][], double z2[][], int sing1[], double z[][], double beta[], double qdat[][]) {
		double I[][] = null;
		return I;
	}
	
	private double[][] rptrnsfm(double y[][], int cnr[]) {
		// rptrnsfm not intended for calling directly by the user
		// Transformation optimization vars to prevertices for rectangle problem
		// Original MATLAB code copyright 1997 by Toby Driscoll.  Last updated 05/06/97
		int i;
		int n = y.length+3;
		double z[][] = new double[n][2];
		
		// Fill interior of long edges first
		double cumsumreal = 0.0;
		double cumsumimag = 0.0;
		for (i = cnr[0]; i <= cnr[1] - 2; i++) {
		    cumsumreal += Math.exp(y[i][0])*Math.cos(y[i][1]);
		    cumsumimag += Math.exp(y[i][0])*Math.sin(y[i][1]);
		    z[i+1][0] = cumsumreal;
		    z[i+1][1] = cumsumimag;
		}
		cumsumreal = 0.0;
		cumsumimag = 0.0;
		for (i = cnr[3]-3; i >= cnr[2]-1; i--) {
			cumsumreal += Math.exp(y[i][0])*Math.cos(y[i][1]);
		    cumsumimag += Math.exp(y[i][0])*Math.sin(y[i][1]);
		    z[i+2][0] = cumsumreal;
		    z[i+2][1] = 1.0 + cumsumimag;	
		}
		
		// Find L
		double xr[] = new double[2];
		xr[0] = z[cnr[1]-1][0];
		xr[1] = z[cnr[2]+1][0];
		double meanxr = (xr[0] + xr[1])/2.0;
		double diff = (xr[1] - xr[0])/2.0;
		double diffSquared = diff * diff;
		double yexpreal = Math.exp(2*y[cnr[1]-1][0])*Math.cos(2*y[cnr[1]-1][1]);
		double yexpimag = Math.exp(2*y[cnr[1]-1][0])*Math.sin(2*y[cnr[1]-1][1]);
		double realtotal = diffSquared + yexpreal;
		double mag = Math.sqrt(realtotal*realtotal + yexpimag*yexpimag);
		double sqrtmag = Math.sqrt(mag);
		double angle = Math.atan2(yexpimag,realtotal);
		double halfangle = angle/2.0;
		double realsqrt = sqrtmag * Math.cos(halfangle);
		double imagsqrt = sqrtmag * Math.sin(halfangle);
		z[cnr[1]][0] = meanxr + realsqrt;
		z[cnr[1]][1] = imagsqrt;
		z[cnr[2]][0] = z[cnr[1]][0];
		z[cnr[2]][1] = 1 + z[cnr[1]][1];
		z[cnr[3]][0] = 0.0;
		z[cnr[3]][1] = 1.0;
		
		// Now fill in "short edges"
		double cp[][] = new double[cnr[2] - cnr[1]][2];
		cp[0][0] = 1;
		cp[0][1] = 0;
		for (i = 1; i < cnr[2] - cnr[1]; i++) {
			cp[i][0] = cp[i-1][0] * Math.exp(-y[cnr[1] + i-1][0]) * Math.cos(-y[cnr[1] + i-1][1]) 
					- cp[i-1][1] * Math.exp(-y[cnr[1]+i-1][0]) * Math.sin(-y[cnr[1]+i-1][1]);
			cp[i][1] = cp[i-1][0] * Math.exp(-y[cnr[1]+i-1][0]) * Math.sin(-y[cnr[1]+i-1][1])
					+ cp[i-1][1] * Math.exp(-y[cnr[1]+i-1][0]) * Math.cos(-y[cnr[1]+i-1][1]);
		}
		double flipudcp[][] = new double[cp.length][2];
		for (i = 0; i < cp.length; i++) {
			flipudcp[i][0] = cp[cp.length-1-i][0];
			flipudcp[i][1] = cp[cp.length-1-i][1];
		}
		double cumsumflip[][] = new double[cp.length][2];
		cumsumflip[0][0] = flipudcp[0][0];
		cumsumflip[0][1] = flipudcp[0][1];
		for (i = 1; i < cp.length; i++) {
			cumsumflip[i][0] = cumsumflip[i-1][0] + flipudcp[i][0];
			cumsumflip[i][1] = cumsumflip[i-1][1] + flipudcp[i][1];
		}
		double flipcumsum[][] = new double[cp.length][2];
		for (i = 0; i < cp.length; i++) {
			flipcumsum[i][0] = cumsumflip[cp.length-i-1][0];
			flipcumsum[i][1] = cumsumflip[cp.length-i-1][1];
		}
		double cumsumcp[][] = new double[cp.length][2];
		cumsumcp[0][0] = cp[0][0];
		cumsumcp[0][1] = cp[0][1];
		for (i = 1; i < cp.length; i++) {
			cumsumcp[i][0] = cumsumcp[i-1][0] + cp[i][0];
			cumsumcp[i][1] = cumsumcp[i-1][1] + cp[i][1];
		}
		double x[][] = new double[cp.length+1][2];
		x[0][0] = -flipcumsum[0][0];
		x[0][1] = -flipcumsum[0][1];
		for (i = 1; i < cp.length; i++) {
			x[i][0] = cumsumcp[i-1][0] - flipcumsum[i][0];
			x[i][1] = cumsumcp[i-1][1] - flipcumsum[i][1];
		}
		x[cp.length][0] = cumsumcp[cp.length-1][0];
		x[cp.length][1] = cumsumcp[cp.length-1][1];
		// (1/(x[cp.length][0] + i * x[cp.length][1])) * (x[cp.length][0] - i * x[cp.length][1])/(x[cp.length][0] - i * x[cp.length][1])
		double denom = x[cp.length][0]*x[cp.length][0] + x[cp.length][1]*x[cp.length][1];
		double x2[][] = new double[x.length-2][2];
		for (i = 0; i < x2.length; i++) {
			x2[i][0] = (x[i+1][0]*x[cp.length][0] + x[i+1][1]*x[cp.length][1])/denom;
			x2[i][1] = (-x[i+1][0]*x[cp.length][1] + x[i+1][1]*x[cp.length][0])/denom;
		}
		boolean mask[] = new boolean[x2.length];
		for (i = 0; i < x2.length; i++) {
			double absx = Math.sqrt(x2[i][0]*x2[i][0] + x2[i][1]*x2[i][1]);
			if (absx < eps) {
				mask[i] = true;
			}
		}
		double u[][] = new double[x2.length][2];
		for (i = 0; i < x2.length; i++) {
			u[i][0] = x2[i][0];
			u[i][1] = x2[i][1];
		}
		for (i = 0; i < x2.length; i++) {
			if (!mask[i]) {
				mag = Math.sqrt(x2[i][0]*x2[i][0] + x2[i][1]*x2[i][1]);
				angle = Math.atan2(x2[i][1], x2[i][0]);
				u[i][0] = Math.log(mag)/Math.PI;
				u[i][1] = angle/Math.PI;
			}
			else {
				u[i][0] = -z[cnr[1]][0]/eps;
				u[i][1] = -z[cnr[1]][1]/eps;
			}
		} // for (i = 0; i < x2.length; i++)
		for (i = cnr[1]+1; i <= cnr[2]-1; i++) {
			z[i][0] = z[cnr[1]][0] - u[i][0];
			z[i][1] = u[i][1];
		}
		
		cp = new double[1 + n-4-(cnr[3]-2)+1+cnr[0]][2];
		cp[0][0] = 1;
		cp[0][1] = 0;
		for (i = 1; i <= n - cnr[3] - 1; i++) {
			cp[i][0] = cp[i-1][0]*Math.exp(-y[i+cnr[3]-3][0])*Math.cos(-y[i+cnr[3]-3][1]) 
					- cp[i-1][1]*Math.exp(-y[i+cnr[3]-3][0])*Math.sin(-y[i+cnr[3]-3][1]);
			cp[i][1] = cp[i-1][0]*Math.exp(-y[i+cnr[3]-3][0])*Math.sin(-y[i+cnr[3]-3][1])
					+ cp[i-1][1]*Math.exp(-y[i+cnr[3]-3][0])*Math.cos(-y[i+cnr[3]-3][1]);
		}
		for (i = n - cnr[3]; i < n -cnr[3] + cnr[0]; i++) {
			cp[i][0] = cp[i-1][0]*Math.exp(-y[i-n+cnr[3]][0])*Math.cos(-y[i-n+cnr[3]][1]) 
					- cp[i-1][1]*Math.exp(-y[i-n+cnr[3]][0])*Math.sin(-y[i-n+cnr[3]][1]);
			cp[i][1] = cp[i-1][0]*Math.exp(-y[i-n+cnr[3]][0])*Math.sin(-y[i-n+cnr[3]][1])
					+ cp[i-1][1]*Math.exp(-y[i-n+cnr[3]][0])*Math.cos(-y[i-n+cnr[3]][1]);	
		}
		flipudcp = new double[cp.length][2];
		for (i = 0; i < cp.length; i++) {
			flipudcp[i][0] = cp[cp.length-1-i][0];
			flipudcp[i][1] = cp[cp.length-1-i][1];
		}
		cumsumflip = new double[cp.length][2];
		cumsumflip[0][0] = flipudcp[0][0];
		cumsumflip[0][1] = flipudcp[0][1];
		for (i = 1; i < cp.length; i++) {
			cumsumflip[i][0] = cumsumflip[i-1][0] + flipudcp[i][0];
			cumsumflip[i][1] = cumsumflip[i-1][1] + flipudcp[i][1];
		}
		flipcumsum = new double[cp.length][2];
		for (i = 0; i < cp.length; i++) {
			flipcumsum[i][0] = cumsumflip[cp.length-i-1][0];
			flipcumsum[i][1] = cumsumflip[cp.length-i-1][1];
		}
		cumsumcp = new double[cp.length][2];
		cumsumcp[0][0] = cp[0][0];
		cumsumcp[0][1] = cp[0][1];
		for (i = 1; i < cp.length; i++) {
			cumsumcp[i][0] = cumsumcp[i-1][0] + cp[i][0];
			cumsumcp[i][1] = cumsumcp[i-1][1] + cp[i][1];
		}
		x = new double[cp.length+1][2];
		x[0][0] = -flipcumsum[0][0];
		x[0][1] = -flipcumsum[0][1];
		for (i = 1; i < cp.length; i++) {
			x[i][0] = cumsumcp[i-1][0] - flipcumsum[i][0];
			x[i][1] = cumsumcp[i-1][1] - flipcumsum[i][1];
		}
		x[cp.length][0] = cumsumcp[cp.length-1][0];
		x[cp.length][1] = cumsumcp[cp.length-1][1];
		// (1/(x[cp.length][0] + i * x[cp.length][1])) * (x[cp.length][0] - i * x[cp.length][1])/(x[cp.length][0] - i * x[cp.length][1])
		denom = x[cp.length][0]*x[cp.length][0] + x[cp.length][1]*x[cp.length][1];
		x2 = new double[x.length-2][2];
		for (i = 0; i < x2.length; i++) {
			x2[i][0] = (x[i+1][0]*x[cp.length][0] + x[i+1][1]*x[cp.length][1])/denom;
			x2[i][1] = (-x[i+1][0]*x[cp.length][1] + x[i+1][1]*x[cp.length][0])/denom;
		}
		mask = new boolean[x2.length];
		for (i = 0; i < x2.length; i++) {
			double absx = Math.sqrt(x2[i][0]*x2[i][0] + x2[i][1]*x2[i][1]);
			if (absx < eps) {
				mask[i] = true;
			}
		}
		u = new double[x2.length][2];
		for (i = 0; i < x2.length; i++) {
			u[i][0] = x2[i][0];
			u[i][1] = x2[i][1];
		}
		for (i = 0; i < x2.length; i++) {
			if (!mask[i]) {
				mag = Math.sqrt(x2[i][0]*x2[i][0] + x2[i][1]*x2[i][1]);
				angle = Math.atan2(x2[i][1], x2[i][0]);
				u[i][0] = Math.log(mag)/Math.PI;
				u[i][1] = angle/Math.PI;
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
			z[i-n+cnr[3]][0] = u[i][0];
			z[i-n+cnr[3]][1] = u[i][1];
		}
		return z;
	}
	
	// Gauss-Jacobi quadrature data for SC Toolbox.
	// scqdata returns a matrix of quadrature data suitable for other SC routines.  
	// beta is a vector of turning angles corresponding to *finite* singularities (prevertices and,
	// for exterior map, the origin).
	// nqpts is the number of quadrature points per subinterval, roughly equal to -log10(error).
	
	// All the SC routines call this routine as needed, and the work required is small, so you will
	// probably never have to call this routine directly.
	// Original MATLAB code copyright 1998 by Toby Driscoll.
	private double[][] scqdata(double beta[], int nqpts) {
		int i, j;
	    double qdat[][] = null;
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
	    qdat = new double[nqpts][2*n+2];
	    for (i = 0; i < nqpts; i++) {
	    	for (j = 0; j < n+1; j++) {
	    		qdat[i][j] = qnode[i][j];
	    		qdat[i][j+n+1] = qwght[i][j];
	    	}
	    }
	    return qdat;
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
	        for (i = 0; i < n; i++) {
	        	w[i][0] = wrenum[i][0];
	        	w[i][1] = wrenum[i][1];
	        	beta[i] = betarenum[i];
	        }
	        for (i = 0; i < n; i++) {
	        	wrenum[i] = null;
	        }
	        wrenum = null;
	        betarenum = null;
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
	        for (i = 0; i < n; i++) {
	        	w[i][0] = wrenum[i][0];
	        	w[i][1] = wrenum[i][1];
	        	beta[i] = betarenum[i];
	        }
	        for (i = 0; i < n; i++) {
	        	wrenum[i] = null;
	        }
	        wrenum = null;
	        betarenum = null;
	        int offset = n - corner[0]; // corner - corner[0] so no need to add one
		    for (i = 0; i < n; i++) {
		    	corner[i] = ((corner[i] + offset) % n);
		    }
		    if (n < 4) {
		    	MipavUtil.displayError("Polygon must have at least 4 vertices");
		    	err = -1;
		    	return err;
		    }
		    int sortcorner[] = new int[corner.length];
		    for (i = 0; i < corner.length; i++) {
		    	sortcorner[i] = corner[i];
		    }
		    Arrays.sort(sortcorner);
		    for (i = 0; i < corner.length; i++) {
		    	if (corner[i] != sortcorner[i]) {
		    		MipavUtil.displayError("Corners must be specified in ccw order");
		    		err = -1;
		    		return err;
		    	}
		    } // for (i = 0; i < corner.length; i++)
		    if (Double.isInfinite(w[0][0]) || Double.isInfinite(w[0][1])) {
		    	MipavUtil.displayError("Corner[0] must be finite");
		    	err = -1;
		    	return err;
		    }
		    if (Double.isInfinite(w[1][0]) || Double.isInfinite(w[1][1])) {
		        MipavUtil.displayError("Corner[0] + 1 must be finite");
		        err = 1;
		        return err;
		    }
		    if ((Math.abs(beta[n-1]) < eps) || (Math.abs(beta[n-1] - 1) < eps)) {
	        	MipavUtil.displayError("Sides adjacent to w[corner[0]-1] must not be collinear");
	        	err = 1;
	        	return err;
	        }
	    } // else if (type.equalsIgnoreCase("r"))
	    return err;
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
	
}