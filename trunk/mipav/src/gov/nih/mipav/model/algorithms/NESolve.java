package gov.nih.mipav.model.algorithms;

import java.util.Vector;

import Jama.Matrix;
import gov.nih.mipav.view.*;

/**
    This is a port of nesolve.m and supporting files coded in MATLAB by Richard T. Behrens in 1988
    NESolve produces the solution to a system of nonlinear equations
    For example, find the x, y, and z that solve the simultaneous equations:
    sin(x) + y^2 + log(z) - 7 = 0
    3*x + 2^y - z^3 + 1 = 0;
    x + y + z - 5 = 0
    nesolve.m coded in MATLAB by Ricaard T. Behrens, April 1988.
    Revised 11/278/88 JNL
    Hookstep option added 8/21/90 RTB.
    Hello William, 

Thank you for contacting MathWorks Technical Support. My name is Stephen and I am writing in reference to your 
Technical Support Case #02445410 regarding 'Permission to port nesolve.m'. 

Thank you for contacting us about this. It appears that “nesolve” is a File Exchange function that is not part
of the MATLAB code base. The author included parts of a MATLAB function help header, but none of its source code.
Therefore, we neither grant nor deny permission to copy the contents of that file as it does not pertain to
the MATLAB code base.

If you have any more related questions, let me know and I would be happy to help.

Sincerely, 
Stephen Jue
MathWorks Technical Support Department 

 nesolve.m is based on Algorithm D6.1.3: Part of the modular software system from the 
 appendix of the book "Numerical Methods for Unconstrained Optimization and Nonlinear Equations"
 by Dennis & Schnabel, 1983.  Refer to that book for more detailed information about the
 algorithms.   
*/

    public abstract class NESolve {
    	
    	private int NO_SCALING = 0;
    	private int SCALING_WITHOUT_SCALE = 1;
    	private int SCALING_WITH_SCALE = 2;
        private int scaling = NO_SCALING;
    
	    // An initial guess of the solution (starting point for iterations)
    	protected double x0[];
    	
    	// (Optional) A vector whose elements select various algorithmic options
    	// and specify various tolerances
    	// details[0] is not used
    	protected double details[] = new double[17];
    	
    	// (Optional) A set of parameters (constants) which if nonemepty
    	// is paased on to the function and Jacobian
    	protected double fparam[] = null;
    	
    	protected boolean analyticJacobian = false;
    	
    	// Typical values of X (1st column) and F (second column)
    	protected double scale[][] = null;
    	
    	// Outputs
    	// The final approximation of the solution
    	protected double xf[];
    	
    	// Indicates the stopping reason (equals 1 for normal)
    	protected int termcode[] = new int[1];
    	
    	// (Optional) Returns the sequence of iterates
    	protected Vector<Double> path = null;
    	
    	protected double btrack[] = null;
    	
    	// Number of function evaluations
    	protected int nofun[] = new int[1];
    	
	    public NESolve(double x0[], double fparam[], boolean analyticJacobian, double scale[][], Vector<Double> path,
	    		double btrack[], int scaling) {
	    	this.x0 = x0;
	 	    this.fparam = fparam;
	 	    this.analyticJacobian = analyticJacobian;
	 	    this.scale = scale;
	 	    this.path = path;
	 	    this.btrack = null;
	 	    this.scaling = scaling;
	    }
	    
	    // fvplus is an output
	    // xplus and fparam are inputs
	    public abstract void fitToFunction(double fvplus[], double xplus[], double fparam[]);
	    
	    // jc and addfun are outputs addfun is the number of additional function evaluations
	    // x0 and fparam are inputs
	    public abstract void fitToJacobian(double jc[][], int addfun[], double x0[], double fparam[]);
	    
	    public void driver() {
	    	int i;
	    	int j;
	    	// Variables for trust region methods
	    	double trustvars[] = new double[4];
	    	double fvc[];
	    	double fvplus[];
	        double fc[] = new double[1];
	    	double sin[];
	    	double sx[];
	    	double sf[];
	    	int itncount;
	    	int consecmax;
	    	double prod;
	    	double maxProd;
	    	int addfun[] = new int[1];
	    	double jc[][];
	    	double gc[];
	    	double xc[];
	    	int restart;
	    	int norest;
	    	double normfv0 = 0.0;
	    	double fracdone;
	    	double maxfvc;
	    	double M[][];
	    	double hc[][];
	    	double sn[];
	    	// Initialization
	        if (fparam != null) {
	        	details[15] = 1;
	        }
	        if (analyticJacobian) {
	        	details[4] = 1;
	        }
	        if ((scale == null) && (scaling == SCALING_WITH_SCALE)) {
	            scaling = SCALING_WITHOUT_SCALE;	
	        }
	        details[16] = scaling;
	        if (path != null) {
	        	details[14] = 1;
	        }
	        if (details[14] == 1) {
	        	for (i = 0; i < x0.length; i++) {
	        		path.add(x0[i]);
	        	}
	        }
	        if (btrack == null) {
	        	details[1] = 2;
	        }
	        xf = new double[x0.length];
	        fvplus = new double[x0.length];
	        sin = new double[x0.length];
	        sx = new double[x0.length];
	        sf = new double[x0.length];
	        jc = new double[x0.length][x0.length];
	        gc = new double[x0.length];
	        fvc = new double[x0.length];
	        xc = new double[x0.length];
	        M = new double[x0.length][x0.length];
	        hc = new double[x0.length][x0.length];
	        sn = new double[x0.length];
	        // Algorithm step 2
	        if (details[16] > 0) { // Might need F(x0) for scaling
	           for (i = 0; i < x0.length; i++) {
	        	   sin[i] = 1.0;
	           }
	           nefn(fc, fvplus, nofun, x0, sin, fparam);
	        } // if (details[16] > 0)
	        neinck(details, sx, sf, termcode, x0, fvplus, details, scale);
	        
	        // Algorithm step 3
	        if (termcode[0] < 0) {
	        	for (i = 0; i < x0.length; i++) {
	        		xf[i] = x0[i];
	        	}
	        	if (details[14] > 0) {
	        		for (i = 0; i < xf.length; i++) {
	        			path.add(xf[i]);
	        		}
	        	}
	        } // if (termcode[0] < 0)
	        
	        // Algorithm step 4.
	        itncount = 0;
	        
	        // Algorithm step 5.
	        nefn(fc, fvplus, nofun, x0, sf, fparam);
	        
	        // Algorithm step 6.
	        consecmax = 0;
	        maxProd = 0.0;
	        for (i = 0; i < x0.length; i++) {
	            prod = sf[i] * Math.abs(fvplus[i]);
	            if (prod > maxProd) {
	            	maxProd = prod;
	            }
	        } // for (i = 0; i < x0.length; i++)
	        if (maxProd <= 1.0e-2 * details[8]) {
	        	termcode[0] = 1;
	        }
	        else {
	        	termcode[0] = 0;
	        }
	        
	        // Algorithm step 7.
	        if (termcode[0] > 0) {
	        	for (i = 0; i < x0.length; i++) {
	        		xf[i] = x0[i];
	            }
	        	if (details[14] > 0) {
	        		for (i = 0; i < xf.length; i++) {
	        			path.add(xf[i]);
	        	    }
	        	} // if (details[14] > 0)
	        } // if (termcode[0] > 0)
	        else { // termcode[0] <= 0
	            if (details[4] > 0) {
	            	fitToJacobian(jc, addfun, x0, fparam);
	            	nofun[0] = nofun[0] + addfun[0];
	            } // if (details[4] > 0)
	            else { // details[4] <= 0
	                nefdjac(jc, nofun, fvplus, x0, sx, details, fparam); 
	            } // else details[4] <= 0)
	            for (j = 0; j < x0.length; j++) {
	            	gc[j] = 0.0;
	            	for (i = 0; i < x0.length; i++) {
	            		gc[j] = gc[j] + jc[i][j] * (fvplus[i] * sf[i] * sf[i]);
	            	}
	            } // for (j = 0; j < x0.length; j++)
	            for (i = 0; i < x0.length; i++) {
	            	fvc[i] = fvplus[i];
	            }
	        } // else termcode[0] <= 0
	        
	        // Algorithm step 8.
	        for (i = 0; i < x0.length; i++) {
	        	xc[i] = x0[i];
	        }
	        
	        // Algorithm step 9.
	        restart = 1;
	        norest = 0;
	        
	        // Algorithm step 10 (iteration).
	        if (details[1] > 0) {
	            normfv0 = 0.0;
	            for (i = 0; i < x0.length; i++) {
	            	if (Math.abs(fvc[i]) > normfv0) {
	            		normfv0 = Math.abs(fvc[i]);
	            	}
	            }
	        } // if (details[1] > 0)
	        
	        while (termcode[0] == 0) {
	            if (details[1] > 0) {
	                maxfvc = 0.0;
	                for (i = 0; i < x0.length; i++) {
	                	if (Math.abs(fvc[i]) > maxfvc) {
	                		maxfvc = Math.abs(fvc[i]);
	                	}
	                } // for (i = 0; i < x0.length; i++)
	                Preferences.debug("itncount = " + itncount + "\n", Preferences.DEBUG_ALGORITHM);
	                Preferences.debug("nofun[0] = " + nofun[0] + "\n", Preferences.DEBUG_ALGORITHM);
	                Preferences.debug("max(abs(fvc)) = " + maxfvc + "\n", Preferences.DEBUG_ALGORITHM);
	                fracdone = Math.log(maxfvc/normfv0)/Math.log(details[8]/normfv0);
	                Preferences.debug("fracdone = " + fracdone + "\n", Preferences.DEBUG_ALGORITHM);
	            } // if (details[1] > 0)
	            itncount++;
	            if ((details[4] > 0) || (details[3] > 0) || ((1 - details[5]) > 0)) {
	                nemodel(M, hc, sn, fvc, jc, gc, sf, sx, details[2]);
	            } // if ((details[4] > 0) || (details[3] > 0) || ((1 - details[5]) > 0))
	        } // while (termcode[0] == 0)
	    } // driver
	    
	    private void nefn(double fplus[], double fvplus[], int nofun[], double xplus[], double sf[], double fparam[]) {
	    	// fplus and fplus are outputs
	    	// nofun is an input/output
	    	// xplus, sf, and fparam are inputs
	    	// This function is part of the Nonlinear Equations package, see nesolve.m
	    	// It evaluates the vector function and calculates the sum of squares for nonlinear equations
	    	// Part of the modular software system from the appendix of the book "Numerical Methods for 
	    	// Unconstrained Optimization and Nonlinear Equations" by Dennis & Schnable, 1983.
	    	// Originally coded in MATLAB by RIchard T. Behrens, April, 1988.
	    	int i;
	    	double prod;
	    	fitToFunction(fvplus, xplus, fparam);
	    	fplus[0] = 0.0;
	    	for (i= 0; i < sf.length; i++) {
	    	    prod = sf[i] * fvplus[i];
	    	    fvplus[0] += (prod * prod);
	    	}
	    	fvplus[0] = 0.5 * fvplus[0];
	    	nofun[0]++;
	    } // nefn
	    
	    private void neinck(double dout[], double sx[], double sf[], int termcode[], double x0[], double f0[], 
	    		double din[], double scale[][]) {
	    	// dout, sx, sf, and termcode are outputs
	    	// x0, f0, din, and scale are inputs
	    	// This function is part of the Nonlinear Equations package, see nesolve.m.
	    	// It checks the input arguments and sets the various tolerances and limits.
	    	// Part of the modular software system from the appendix of the book "Numerical
	    	// Methods for Unconstrained Optimization and Nonlinear Equations" by Dennis & Schnabel, 1983.
	    	// Originally coded in MATLAB by Richard T. Behrens, April, 1988.
	    	int i;
	    	termcode[0] = 0;
	    	for (i = 0; i < din.length; i++) {
	    		dout[i] = din[i];
	    	}
	    	
	    	// Step 1
	    	int n = x0.length;
	    	if (n < 1) {
	    		termcode[0] = -1;
	    		return;
	    	}
	    	
	    	// Steps 2 & 3
	    	int l = scale.length;
	    	int m = scale[0].length;
	    	if (dout[16] == 2) {
	    		if ((l == n) && (m == 2)) {
	    			for (i = 0; i < n; i++) {
	    				sx[i] = 1.0/Math.abs(scale[i][0]);
	    				sf[i] = 1.0/Math.abs(scale[i][1]);
	    			}
	    		}
	    		else {
	    			dout[16] = 1;
	    		}
	    	} // if (dout[16] == 2)
	    	if (dout[16] == 0) {
	    		for (i = 0; i < n; i++) {
	    			sx[i] = 1.0;
	    			sf[i] = 1.0;
	    		}
	    	} // if (dout[16] == 0)
	    	if (dout[16] == 1) {
	    	    for (i = 0; i < n; i++) {
	    	    	if (x0[i] == 0) {
	    	    		x0[i] = 1;
	    	    	}
	    	    	if (f0[i] == 0) {
	    	    		f0[i] = 1;
	    	    	}
	    	    	sx[i] = 1.0/Math.abs(x0[i]);
	    	    	sf[i] = 1.0/Math.abs(f0[i]);
	    	    } // for (i = 0; i < n; i++)
	    	} // if (dout[16] == 1)
	    	
	    	// Step 4
	    	// eps returns the distance from 1.0 to the next larger double-precision number, that is, eps = 2^-52.
	    	// epsilon = D1MACH(4)
	        // Machine epsilon is the smallest positive epsilon such that
	        // (1.0 + epsilon) != 1.0.
	        // epsilon = 2**(1 - doubleDigits) = 2**(1 - 53) = 2**(-52)
	        // epsilon = 2.2204460e-16
	        // epsilon is called the largest relative spacing
	        double eps = 1.0;
	        double neweps = 1.0;

	        while (true) {

	            if (1.0 == (1.0 + neweps)) {
	                break;
	            } else {
	                eps = neweps;
	                neweps = neweps / 2.0;
	            }
	        } // while(true)
	    	if (dout[12] <= 0) {
	    		dout[13] = eps;
	    	}
	    	else {
	    		dout[13] = Math.max(eps, Math.pow(10.0, -dout[12]));
	    	}
	    	if (dout[13] > 0.01) {
	    		termcode[0] = -2;
	    		return;
	    	}
	    	
	    	// Step 5
	    	if (dout[2] <= 0) {
	    		dout[2] = 1; // Default to linesearch
	    	}
	    	if (((dout[2] == 2) || (dout[2] == 3)) && (dout[7] <= 0)) {
	    		dout[7] = -1;
	    	}
	    	
	    	// Step 6
	    	if (dout[6] <= 1) {
	    		dout[6] = 100; // Default to 100 iteration limit.
	    	}
	    	if (dout[8] <= 0) {
	    		dout[8] = Math.pow(eps, (1.0/3.0)); // fvectol
	    	}
	    	if (dout[9] <= 0) {
	    		dout[9] = Math.pow(eps, (2.0/3.0)); // steptol
	    	}
	    	if (dout[10] <= 0) {
	    		dout[10] = Math.pow(eps, (2.0/3.0)); // mintol
	    	}
	    	if (dout[11] <= 0) {
	    		double normsxx0 = 0.0;
	    		for (i = 0; i < n; i++) {
	    			double prod = sx[i] * x0[i];
	    			normsxx0 += (prod * prod);
	    		}
	    		normsxx0 = Math.sqrt(normsxx0);
	    		double maxsx = -Double.MAX_VALUE;
	    		for (i = 0; i < n; i++) {
	    			if (sx[i] > maxsx) {
	    				maxsx = sx[i];
	    			}
	    		}
	    		dout[11] = 1000.0 * Math.max(normsxx0, maxsx); // maxstep
	    	} // if (dout[11] <= 0)
	    } // neinck
	    
	    private void nefdjac(double J[][], int nofun[], double fc[], double xc[], double sx[], double details[], double fparam[]) {
	        // J is an output
	    	// nofn is an input/output
	    	// fc, xc, sx, details, and fparam are inputs
	    	// This function is part of the Nonlinear Equations package.
	    	// This is a "Finite Difference Jacobian Approximation".  It calculates a finite 
	    	// difference approximation to J(xc), the Jacobian of F9X0 at x = xc.
	    	// Algorithm A5.4.1: Part of the modular software system from the appendix of the book
	    	// "Numerical Methods for Unconstrained Optimization and Nonlinear Equations" by
	    	// Dennis & Schnabel, 1983.
	    	// Coded in MATLAB by Sherkat Masoum M., March, 1988.
	    	// Edited by Richard T. Behrens, June, 1988.
	    	
	    	// Algorithm step 1.
	    	double fj[] = new double[xc.length];
	    	int n = fc.length;
	    	double sqrteta = Math.sqrt(details[13]);
	    	
	    	// Algorithm step 2.
	    	for (int j = 0; j < n; j++) {
	    	    double addOne = 0.0;
	    	    if (xc[j] == 0) {
	    	    	addOne = 1.0;
	    	    }
	    	    double stepsizej = sqrteta * Math.max(Math.abs(xc[j]), 1.0/sx[j]) * (sign(xc[j])) + addOne;
	    	    // To incorporate a different stepsize rule, change the previous line.
	    	    double tempj = xc[j];
	    	    xc[j] = xc[j] + stepsizej;
	    	    stepsizej = xc[j] - tempj;
	    	    // The privious line reduces finite precision error slightly, see section 5.4 of the book.
	    	    fitToFunction(fj, xc, fparam);
	    	    nofun[0]++;
	    	    for (int i = 0; i < n; i++) {
	    	    	J[i][j] = (fj[i] - fc[i])/stepsizej;
	    	    }
	    	    xc[j] = tempj;
	    	} // for (int j = 0; j < n; j++)
	    } // nefdjac
	    
	    private double sign(double x) {
	    	if (x > 0) {
	    		return 1;
	    	}
	    	else if (x == 0) {
	    		return 0;
	    	}
	    	else {
	    		return -1;
	    	}
	    } // sign
	    
	    private void nemodel(double m[][], double h[][], double sn[], double fc[], double J[][], double g[],
	    		double sf[], double sx[], double globmeth) {
	        // m, h, and sn are outputs
	    	// fc, J, g, sf, sx, and globmeth are inputs
	    	// This function is part of the Nonlinear Equations package
	    	// It forms the affine model for use in solving nonlinear equations.
	    	// Algorithm A6.5.1: Part of the modular software system from the appendix of the book
	    	// "Numerical Methods for Unconstrained Optimization and Nonlinear Equations" by
	    	// Dennis & Schnabel, 1983.
	    	// Coded in MATLAB by Sherkat Masoum M., March, 1988.
	    	// Edited by Richard T. Behrens
	    	
	    	// Algorithm step 1.
	    	int i;
	    	int j;
	    	int n = Math.max(J.length, J[0].length);
	    	for (i = 0; i < sf.length; i++) {
	    		for (j = 0; j < sf.length; j++) {
	    			if (i == j) {
	    				m[i][j] = sf[i];
	    			}
	    			else {
	    				m[i][j] = 0;
	    			}
	    		}
	    	}
	    	Matrix matM = new Matrix(m);
	    	Matrix matJ = new Matrix(J);
	    	m = (matM.times(matJ)).getArray();
	    	
	    	// Algorithm step 2.
	    } // nemodel
    }