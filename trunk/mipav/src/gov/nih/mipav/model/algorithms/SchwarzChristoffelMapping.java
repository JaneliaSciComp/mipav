package gov.nih.mipav.model.algorithms;

import gov.nih.mipav.model.structures.*;
import gov.nih.mipav.view.*;

import java.io.*;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.Collections;
import java.util.Comparator;

import de.jtem.numericalMethods.algebra.linear.decompose.Eigenvalue;

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
	private void rparam(double z[][], double c[], double L[], double qdat[][], 
			double w[][], double beta[], int cnr[], double z0[][], boolean trace, double tol,
			int method) {
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
	    //if ((cnr == null) || (cnr.length == 0)) {
	    	//String msg[] = new String[2];
	        //msg[0] = "Select the images of the corners of the rectangle";
	        //msg[1] = "Go in counerclockwise order and select a long rectangle edge first";
	        //cnr = scselect(w, beta, 4, "Select corners", msg);
	    // } // if ((cnr == null) || (cnr.length == 0))
	
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
	    for (i = 0; i < n; i++) {
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
	    qdat = scqdata(betacopy, nqpts);  // quadrature data
	    
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
	        // z0[0] to z0[cnr[2]-1] has an imaginary part = 0.
	        // z0[cnr[2]] to z0[n-1] has an imaginary part = 1.
	        // Only dz[cnr[2]-1] has an imaginary part
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
	    	dz[i][1] = -dz[i][1];
	    }
	    for (i = 0; i <= cnrcopy[1]-2; i++) {
	    	y0[i] = Math.log(zabs(dz[i][0],dz[i][1]));
	    }
	    for (i = cnrcopy[2]-1; i <= cnrcopy[3]-3; i++) {
	    	y0[i] = Math.log(zabs(dz[i+2][0],dz[i+2][1]));	
	    }
	    y0[cnrcopy[1]-1] = (Math.log(zabs(dz[cnrcopy[1]-1][0],dz[cnrcopy[1]-1][1])) +
	    		(Math.log(zabs(dz[cnrcopy[2]][0],dz[cnrcopy[2]][1])))/2.0);
	    
	    
	    // Vertices on the "short" edges are transformed into the interval [-1,1],
	    // and then the Trefethen-style transformation is used.
	    L[0] = z0copy[cnrcopy[1]][0] - z0copy[cnrcopy[0]][0];
	    L[1] = z0copy[cnrcopy[1]][1] - z0copy[cnrcopy[0]][1];
	    double x[] = new double[cnrcopy[2]-cnrcopy[1]-1];
	    for (i = cnrcopy[1]+1; i <= cnrcopy[2]-1; i++) {
	    	x[i-cnrcopy[1]-1] = Math.exp(Math.PI*(L[0] - z0copy[i][0]))*Math.cos(Math.PI*(L[1] + z0copy[i][1]));
	    }
	    double dx2[] = new double[x.length+1];
	    dx2[0] = 1 - x[0];
	    for (i = 0; i < x.length-1; i++) {
	    	dx2[i+1] = x[i] - x[i+1];
	    }
	    dx2[x.length] = x[x.length-1] + 1;
	    for (i = 0; i <= dx2.length-2; i++) {
	        y0[i+cnrcopy[1]] = Math.log(dx2[i]/dx2[i+1]);
	    }
	    x = new double[n-1-cnrcopy[3]];
	    for (i = 0; i < n-1-cnrcopy[3]; i++) {
	    	x[i] = Math.exp(Math.PI*z0copy[cnrcopy[3]+1+i][0])*Math.cos(Math.PI*z0copy[cnrcopy[3]+1+i][1]);
	    }
	    dx2[0] = x[0]+1;
	    for (i = 0; i < x.length-1; i++) {
	    	dx2[i+1] = x[i+1] - x[i];
	    }
	    dx2[x.length] = 1 - x[x.length-1];
	    for (i = 0; i <= dx2.length-2; i++) {
	        y0[i+cnrcopy[3]-2] = Math.log(dx2[i]/dx2[i+1]);
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
		z = rptrnsfm(y,cnrcopy);
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
	    	bs[i] = beta[i];
	    }
	    bs[ends[0]+1] = 0;
	    for (i = ends[0]+1; i <= ends[1]; i++) {
	    	bs[i+1] = beta[i];
	    }
	    bs[ends[1]+2] = 0;
	    for (i = 0; i < n; i++) {
	    	bs[i+2] = beta[i];
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
		sing1[0] = 2;
		double z1[][] = new double[1][2];
		z1[0][0] = zs[1][0];
		z1[0][1] = zs[1][1];
		double I1[][] = stquad(z1, mid, sing1, zs, bs, qs);
		sing1[0] = 1;
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
		// In rptrnsfm z[cnr[0]] is never changed from its initial value of zero
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
		for (i = cnr[2]; i <= cnr[3]; i++) {
			l[i] = true;
		}
		boolean r[] = new boolean[n]; // on right side
		for (i = cnr[0]; i <= cnr[1]; i++) {
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
		z = new double[zs.length][2];
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
		double dF[][] = new double[numnotdone][2];
		double step[] = new double[2];
		double znew[] = new double[2];
		double xn;
		while ((!alldone) && (k < maxiter)) {
		    r2strip(F, dF, znnotdone, zcnr, L[0]);
		    for (i = 0; i < numnotdone; i++) {
		    	F[i][0] = zsnotdone[i][0] - F[i][0];
		    	F[i][1] = zsnotdone[i][1] - F[i][1];
		    	
		    	// Adjust Newton step to stay exactly on rectangle boundary
		    	zdiv(F[i][0], F[i][1], dF[i][0], dF[i][1], cr, ci);
		    	step[0] = cr[0];
		    	step[1] = ci[0];
		    	if (lr[i]) {
		    	    step[0] = 0;	
		    	}
		    	else {
		    		step[1] = 0;
		    	}
		    	
		    	// Newton step
		    	znew[0] = znnotdone[i][0] + step[0];
		    	znew[1] = znnotdone[i][1] + step[1];
		    	// Keep prevertices from moving too far (past boundaries)
		    	// Left/rigth sides capped in Im direction
		        if (lr[i]) {
		        	xn = Math.min(Math.max(znew[1], 0), Kp[0]);
		        	znew[1] = xn;
		        } // if (lr[i])
		        // Top/bottom-left sides capped in Re direction
		        if (tbl[i]) {
		        	xn = Math.min(Math.max(znew[0], -K[0]), -eps);
		        	znew[0] = xn;
		        }
		        // Top/bottom-right sides capped in Re direction
		        if (tbr[i]) {
		            xn = Math.min(Math.max(znew[0], eps), K[0]);
		            znew[0] = xn;
		        } 
		        
		        //  Update
		        znnotdone[i][0] = znew[0];
		        znnotdone[i][1] = znew[1];
		    } // for (i = 0; i < numnotdone; i++)
		    
		    numnotdone = 0;
		    for (i = 0, j = 0; i < done.length; i++) {
		    	if (!done[i]) {
		    		if (zabs(F[j][0], F[j][1]) < tol) {
		    			done[i] = true;
		    		}
		    		else {
		    			numnotdone++;
		    			alldone = false;
		    		}
		    	}
		    } // for (i = 0, j = 0; i < done.length; i++)
		    if (!alldone) {
			    znnotdone = new double[numnotdone][2];
				zsnotdone = new double[numnotdone][2];
				lr = new boolean[numnotdone];
				tbl = new boolean[numnotdone];
				tbr = new boolean[numnotdone];
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
				F = new double[numnotdone][2];
				dF = new double[numnotdone][2];
		    } // if (!alldone)
		    k++;
		} // while ((!alldone) && (k < maxiter))
		
		boolean warning = false;
		for (i = 0; i < F.length && !warning; i++) {
			if (zabs(F[i][0], F[i][1]) > tol) {
				warning = true;
				MipavUtil.displayWarning("Could not converge to the rectangel prevertices");
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
		for (i = 0; i < z.length; i++) {
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
	// ellipjc returns the values of the Jacobi elliptic functions evaluated at comples argument U and
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
		
	    if (!flag) {
	    	// flag = false indicates we must check for and transform u in the upper half of the rectangle.
	    	double K[] = new double[1];
	    	double Kp[] = new double[1];
	    	ellipkkp(K, Kp, L);
	    	for (i = 0; i < u.length; i++) {
	    		if (u[i][1] > Kp[0]/2.0) {
	    			numhigh++;
	    		}
	    	} // for (i = 0; i < u.length; i++)
	    	high = new int[numhigh];
	    	for (i = 0, j = 0; i < u.length; i++) {
	    		if (u[i][1] > Kp[0]/2.0) {
	    			u[i][0] = -u[i][0];
	    			u[i][1] = Kp[0] - u[i][1];
	    			high[j++] = i;
	    		}
	    	} // for (i = 0, j = 0; i < u.length; i++)
	    	m = Math.exp(-2.0*Math.PI*L);
	    } // if (!flag)
	    else {
	    	// Recursive call - L is actually m.
	    	high = null;
	    	m = L;
	    }
	    
	    if (m < 4.0 * eps) {
	        for (i = 0; i < u.length; i++) {
	            sinu[0] = Math.sin(u[i][0])*Math.cosh(u[i][1]);
	            sinu[1] = Math.cos(u[i][0])*Math.sinh(u[i][1]);
	            cosu[0] = Math.cos(u[i][0])*Math.cosh(u[i][1]);
	            cosu[1] = -Math.sin(u[i][0])*Math.sinh(u[i][1]);
	            zmlt(sinu[0], sinu[1], cosu[0], cosu[1], cr, ci);
	            sincos[0] = cr[0];
	            sincos[1] = ci[0];
	            realPart = sincos[0] - u[i][0];
	            imagPart = sincos[1] - u[i][1];
	            zmlt(realPart, imagPart, cosu[0], cosu[1], cr, ci);
	            sn[i][0] = sinu[0] + (m/4.0)*cr[0];
	            sn[i][1] = sinu[1] + (m/4.0)*ci[0];
	            realPart = -sincos[0] + u[i][0];
	            imagPart = -sincos[1] + u[i][1];
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
	    	for (i = 0; i < u.length; i++) {
	    	    v[i][0] = u[i][0]/(1.0 + kappa);
	    	    v[i][1] = u[i][1]/(1.0 + kappa);
	    	} // for (i = 0; i < u.length; i++)
	    	ellipjc(sn1, cn1, dn1, v, mu, true);
	    	for (i = 0; i < u.length; i++) {
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
	    	} // for (i = 0; i < u.length; i++)
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
	    
	    if (m == 1.0) {
	    	K[0] = Double.POSITIVE_INFINITY;
	    }
	    else {
	    	K[0] = Math.PI/(2.0 * a1);
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
	    
	    if (m == 0) {
	    	Kp[0] = Double.POSITIVE_INFINITY;
	    }
	    else {
	    	Kp[0] = Math.PI/(2.0 * a1);
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
					z = rptrnsfm(a, corners);
					
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
					// Put dummy columns into qdat after ends[0] and ends[1] columns
					// qdata ia nqpts rows by 2*n+2 columns
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
					int nots2[] = new int[left.length-nums2];
					for (i = 0, j = 0, k = 0; i < left.length; i++) {
						if ((right[i] - left[i] == 1) && (zleft[i][1] == zright[i][1])) {
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
				    	lefts2[i] = left[s2[i]];
				    	zrights2[i][0] = zright[s2[i]][0];
				    	zrights2[i][1] = zright[s2[i]][1];
				    	rights2[i] = right[s2[i]];
				    }
				    I1 = stquadh(zlefts2, mid, lefts2, z2, beta, qdat2);
				    I2 = stquadh(zrights2, mid, rights2, z2, beta, qdat2);
				    for (i = 0, j = 0; i < s2.length; i++) {
				        ints[s2[j]][0] = I1[i][0] - I2[i][0];
				        ints[s2[j++]][1] = I1[i][1] - I2[i][1];
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
				    	leftns2[i] = left[nots2[i]];
				    	zrightns2[i][0] = zright[nots2[i]][0];
				    	zrightns2[i][1] = zright[nots2[i]][1];
				    	rightns2[i] = right[nots2[i]];
				    }
				    int zero[] = new int[nots2.length];
				    I1 = stquad(zleftns2, mid1, leftns2, z, beta, qdat);
				    I2 = stquadh(mid1, mid2, zero, z, beta, qdat);
				    I3 = stquad(zrightns2, mid2, rightns2, z, beta, qdat);
				    for (i = 0, j = 0; i < nots2.length; i++) {
				    	ints[nots2[j]][0] = I1[i][0] + I2[i][0] - I3[i][0];
				    	ints[nots2[j++]][1] = I1[i][1] + I2[i][1] - I3[i][1];
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
				    	    for (i = 0; i < numcmplx; i++) {
				    	    	residuals[i+Fmod.length] = realF2[i];
				    	    	residuals[i+Fmod.length+numcmplx] = imagF2[i];
				    	    }
				    	} // for (i = 0; i < numcmplx; i++)
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
		    	if ((dx[j] > 0.0) && (Double.isInfinite(z[j][0]) || Double.isInfinite(z[j][1]))) {
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
	// z1,z2 are vectors of left and right endpoints.  sing1 is a vecotr of integer indices which label the
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
		double zr[] = new double[2];
		int ind;
		double nd[][] = new double[qdat.length][2];
		double wt[][] = new double[qdat.length][2];
		boolean anydiffzero;
		double scale;
		double zl[] = new double[2];
		double prod[] = new double[2];
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
		    dist = 1;
		    for (j = 0; j <= sng-1; j++) {
		    	zreal = z[j][0] - za[0];
		    	zimag = z[j][1] - za[1];
		    	zabs = 2.0*zabs(zreal,zimag);
		    	if (zabs < dist) {
		    		dist = zabs;
		    	}
		    } // for (j = 0; j <= sng-1; j++)
		    for (j = sng+1; j <= n-1; j++) {
		    	zreal = z[j][0] - za[0];
		    	zimag = z[j][1] - za[1];
		    	zabs = 2.0*zabs(zreal,zimag);
		    	if (zabs < dist) {
		    		dist = zabs;
		    	}
		    } // for (j = sng+1; j <= n-1; j++)
		    zreal = zb[0] - za[0];
		    zimag = zb[1] - za[1];
		    zabs = zabs(zreal,zimag);
		    dist = dist/zabs;
		    zr[0] = za[0] + dist*(zb[0] - za[0]);
		    zr[1] = za[1] + dist*(zb[1] - za[1]);
		    ind = ((sng+n+1) % (n+1));
		    // Adjust Gauss_jacobi nodes and weights to interval.
		    
		    for (j = 0; j < qdat.length; j++) {
		        nd[j][0] = ((zr[0]-za[0])*qdat[j][ind] + zr[0] + za[0])/2.0; // G-J nodes
		        nd[j][1] = ((zr[1]-za[1])*qdat[j][ind] + zr[1] + za[1])/2.0;
		        wt[j][0] = ((zr[0]-za[0])/2) * qdat[j][ind+n+1]; // G-J weights
		        wt[1][1] = ((zr[1]-za[1])/2) * qdat[j][ind+n+1];
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
		    	for (i = 0; i < wt.length; i++) {
		    		zmlt(out[i][0], out[i][1], wt[i][0], wt[i][1], cr, ci);
		    	    I[k][0] += cr[0];
		    	    I[k][1] += ci[0];
		    	}
		    	while ((dist < 1)&& (!Double.isNaN(I[k][0])) && (!Double.isNaN(I[k][1]))) {
		    	    // Do regular Gaussian quad on other subintervals.
		    		zl[0] = zr[0];
		    		zl[1] = zr[1];
		    		dist = 1;
		    		for (i = 0; i < n; i++) {
		    			zreal = z[i][0] - zl[0];
		    			zimag = z[i][1] - zl[1];
		    			zabs = 2.0*zabs(zreal,zimag);
		    			if (zabs < dist) {
		    				dist = zabs;
		    			}
		    		} // for (i = 0; i < n; i++)
		    		zreal = zl[0] - zb[0];
		    		zimag = zl[1] - zb[1];
		    		zabs = zabs(zreal,zimag);
		    		dist = dist/zabs;
		    		zr[0] = zl[0] + dist*(zb[0]-zl[0]);
		    		zr[1] = zl[1] + dist*(zb[1]-zl[1]);
		    		for (i = 0; i < qdat.length; i++) {
		    			nd[i][0] = ((zr[0]-zl[0]) * qdat[i][n] + zr[0] + zl[0])/2.0;
		    			nd[i][1] = ((zr[1]-zl[1]) * qdat[i][n] + zr[1] + zl[1])/2.0;
		    			wt[i][0] = ((zr[0]-zl[0])/2.0) * qdat[i][2*n+1];
		    			wt[i][1] = ((zr[1]-zl[1])/2.0) * qdat[i][2*n+1];
		    		} // for (i = 0; i < qdat.length; i++)
		    		out = stderiv(nd,z,beta,1,-1);
	                prod[0] = 0;
	                prod[1] = 0;
	                for (i = 0; i < wt.length; i++) {
	                	zmlt(out[i][0], out[i][1], wt[i][0], wt[i][1], cr, ci);
	                	prod[0] += cr[0];
	                	prod[1] += ci[0];
	                }
	                I[k][0] = I[k][0] + prod[0];
	                I[k][1] = I[k][1] + prod[1];
		    	} // while ((dist < 1)&& (!Double.isNaN(I[k][0])) && (!Double.isNaN(I[k][1])))
		    } // else
		} // for (i = 0; i < nontriv.length; i++)
		return I;
	}
	
	// Derivative of the strip map
	// stderiv returns the derivative at the points of zp of the Schwarz-Christoffel
	// strip map defined by z, beta, and c.
	// See also stparam, stmap.
	// Original MATLAB rotuine copyright 1998 by Toby Driscoll.
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
	        } // for (i = 0, k = 0; i < zcopy.length; i++)
	        theta = new double[ends.length-1];
	        for (i = 0; i < ends.length-1; i++) {
	        	theta[i] = beta[ends[i+1]] - beta[ends[i]];
	        }
	        if (zcopy[ends[0]][0] < 0) {
	            for (i = 0; i < theta.length; i++) {
	            	theta[i] = -theta[i];
	            }
	        } // if (zcopy[ends[0]][0] < 0)
	        double z2[][] = new double[numfinitez][2];
	        double beta2[] = new double[numfinitez];
	        for (i = 0; i < numfinitez; i++) {
	        	z2[i][0] = zcopy[notends[i]][0];
	        	z2[i][1] = zcopy[notends[i]][1];
	        	beta2[i] = betacopy[notends[i]];
	        }
	        zcopy = new double[numfinitez][2];
	        betacopy = new double[numfinitez];
	        for (i = 0; i < numfinitez; i++) {
	        	zcopy[i][0] = z2[i][0];
	        	zcopy[i][1] = z2[i][1];
	        	betacopy[i] = beta2[i];
	        }
	        for (i = 0; i < numfinitez; i++) {
	        	z2[i] = null;
	        }
	        z2 = null;
	        beta2 = null;
	        // Adjust singularity index if given
	        if (j >= 0) {
	        	if (j > ends[0]) {
	        		j--;
	        	}
	        	if (j > ends[1]) {
	        		j--;
	        	}
	        } // if (j >= 0)
	    } // if (zcopy.length == betacopy.length)
	    else {
	    	MipavUtil.displayError("Vector of preveretices must include +/- Inf entries");
	    	return null;
	    }
	    double zcol[][] = new double[zcopy.length][2];
	    for (i = 0; i < zcol.length; i++) {
	    	zcol[i][0] = zcopy[i][0];
	    	zcol[i][1] = zcopy[i][1];
	    }
	    double bcol[] = new double[betacopy.length];
	    for (i = 0; i < bcol.length; i++) {
	    	bcol[i] = betacopy[i];
	    }
	    int n = zcopy.length;
	    
	    double terms[][][] = new double[n][npts][2];
	    for (i = 0; i < n; i++) {
	    	for (k = 0; k < npts; k++) {
	    		terms[i][k][0] = (-Math.PI/2.0) * (zprow[k][0] - zcol[i][0]);
	    		terms[i][k][1] = (-Math.PI/2.0) * (zprow[k][1] - zcol[i][1]);
	    	}
	    } // for (i = 0; i < n; i++)
	    for (i = 0; i < n; i++) {
	    	if (zcopy[i][1] == 0) {
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
    			double zreal = zprow[k][0] - z[j][0];
    			double zimag = zprow[k][1] - z[j][1];
    			double zabs = zabs(zreal,zimag);
    			terms[j][k][0] = terms[j][k][0] - Math.log(zabs);
    		}
    	} // if (j >= 0)
    	double mult[][][] = new double[n][npts][2];
    	for (i = 0; i < n; i++) {
    		for (k = 0; k < npts; k++) {
    		    mult[i][k][0] = terms[i][k][0]*bcol[i];
    		    mult[i][k][1] = terms[i][k][0]*bcol[i];
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
    		sum2[i][1] = (Math.PI/2.0)*theta[0]*zprow[i][1] + sum[i][0];
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
	
	private double[][] rptrnsfm(double y[], int cnr[]) {
		// rptrnsfm not intended for calling directly by the user
		// Transformation optimization vars to prevertices for rectangle problem
		// Original MATLAB code copyright 1997 by Toby Driscoll.  Last updated 05/06/97
		int i;
		int n = y.length+3;
		double z[][] = new double[n][2];
		
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
		double cp[] = new double[cnr[2] - cnr[1]];
		cp[0] = 1;
		for (i = 1; i < cnr[2] - cnr[1]; i++) {
			cp[i] = cp[i-1] * Math.exp(-y[cnr[1] + i-1]);
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
				u[i][0] = Math.log(x2[i])/Math.PI;
				u[i][1] = 0;
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
		
		cp = new double[1 + n-4-(cnr[3]-2)+1+cnr[0]];
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
				u[i][0] = Math.log(x2[i])/Math.PI;
				u[i][1] = 0;
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
		    if (Double.isInfinite(wrenum[0][0]) || Double.isInfinite(wrenum[0][1])) {
		    	MipavUtil.displayError("Corner[0] must be finite");
		    	err = -1;
		    	return err;
		    }
		    if (Double.isInfinite(wrenum[1][0]) || Double.isInfinite(wrenum[1][1])) {
		        MipavUtil.displayError("Corner[0] + 1 must be finite");
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