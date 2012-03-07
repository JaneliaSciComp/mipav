package gov.nih.mipav.model.algorithms;

import gov.nih.mipav.view.*;

/**
 * This is a port of lmmin.c and lmmin.h from C into Java.
 * The port is performed by William Gandler.
 *
 */

/**
 * Project:  LevenbergMarquardtLeastSquaresFitting
 *
 * File:     lmmin.c
 *
 * Contents: Levenberg-Marquardt core implementation,
 *           and simplified user interface.
 *
 * Author:   Joachim Wuttke <j.wuttke@fz-juelich.de>
 *
 * Homepage: www.messen-und-deuten.de/lmfit
 */

/**
 * lmfit is released under the LMFIT-BEER-WARE licence:
 *
 * In writing this software, I borrowed heavily from the public domain,
 * especially from work by Burton S. Garbow, Kenneth E. Hillstrom,
 * Jorge J. Moré, Steve Moshier, and the authors of lapack. To avoid
 * unneccessary complications, I put my additions and amendments also
 * into the public domain. Please retain this notice. Otherwise feel
 * free to do whatever you want with this stuff. If we meet some day,
 * and you think this work is worth it, you can buy me a beer in return.
 * 
 *   The purpose of lmmin is to minimize the sum of the squares of
 *   m nonlinear functions in n variables by a modification of
 *   the levenberg-marquardt algorithm. The user must provide a
 *   subroutine evaluate which calculates the functions. The jacobian
 *   is then calculated by a forward-difference approximation.
 */
public abstract class Lmmin {

    private boolean testMode = false;
    
 // epsilon = D1MACH(4)
    // Machine epsilon is the smallest positive epsilon such that
    // (1.0 + epsilon) != 1.0.
    // epsilon = 2**(1 - doubleDigits) = 2**(1 - 53) = 2**(-52)
    // epsilon = 2.2204460e-16
    // epsilon is called the largest relative spacing
    private final double DBL_EPSILON = Math.pow(2, -52);
	
	// emin, the smallest exponent E for double precision, is I1MACH(15)
    // tiny = D1MACH(1) = 2**(emin - 1) = 2**(-1022) = 2.225073858507201E-308
    // D1MACH(1) is the smallest normalized number, which preserves the
    // full precision of the mantissa.
    // Double.MIN_VALUE = 2**(-1074) is the smallest denormalized number = 4.9E-324,
    // which preserves only a portion of the fraction's precision.
	/** 2**-1022 = D1MACH(1). */
    private final double DBL_MIN = Math.pow(2, -1022);
    
    // Double.MAX_VALUE
    // A constant holding the largest positive finite value of type double, (2-2**-52)·2**1023. 
    // It is equal to the value returned by: Double.longBitsToDouble(0x7fefffffffffffffL). 
    // 1.7976931348623157E308d
    private final double DBL_MAX = Double.MAX_VALUE;
    
    /** resolution of arithmetic */
    private final double LM_MACHEP = DBL_EPSILON;
    
    private final double LM_DWARF = DBL_MIN;
    
    /** Square should not underflow */
    private final double LM_SQRT_DWARF = Math.sqrt(DBL_MIN);
    
    /** Square should not overflow */
    private final double LM_SQRT_GIANT = Math.sqrt(DBL_MAX);
    
    private final double LM_USERTOL = 30.0 * LM_MACHEP;
    
    /** Collection of control (input) parameters */
    /** ftol is a nonnegative input variable. Termination occurs when
 *        both the actual and predicted relative reductions in the sum
 *        of squares are at most ftol. Therefore, ftol measures the
 *        relative error desired in the sum of squares. */
    protected double ftol = LM_USERTOL;
    
    /** xtol is a nonnegative input variable. Termination occurs when
 *        the relative error between two consecutive iterates is at
 *        most xtol. Therefore, xtol measures the relative error desired
 *        in the approximate solution. */
    protected double xtol = LM_USERTOL;
    
    /** gtol is a nonnegative input variable. Termination occurs when
 *        the cosine of the angle between fvec and any column of the
 *        jacobian is at most gtol in absolute value. Therefore, gtol
 *        measures the orthogonality desired between the function vector
 *        and the columns of the jacobian. */
    protected double gtol = LM_USERTOL;
    
    /** step used to calculate the jacobian
     * epsfcn is an input variable used in choosing a step length for
 *        the forward-difference approximation. The relative errors in
 *        the functions are assumed to be of the order of epsfcn. */
    protected double epsfcn = LM_USERTOL;
    
    /** initial bound to steps in the outer loop 
     * factor is a positive input variable used in determining the
 *	  initial step bound. This bound is set to the product of
 *	  factor and the euclidean norm of diag*x if nonzero, or else
 *	  to factor itself. In most cases factor should lie in the
 *	  interval (0.1,100.0). Generally, the value 100.0 is recommended.*/
    protected double factor = 100.0;
    
    /** maximum number of iterations */
    protected int maxcall = 100;
    
    /** maxfev is a positive integer input variable. Termination
 *	  occurs when the number of calls to lm_fcn is at least
 *	  maxfev by the end of an iteration. */
    private int maxfev;
    
    /** mode is an integer input variable. If mode = 1, the
 *	  variables will be scaled internally. If mode = 2,
 *	  the scaling is specified by the input diag. */
    protected int mode = 1;
    
    /** Bits ored to show where to output diagnostics */
    protected int printflags = 0;
    
    /** Set message texts (indexed by info) */
    private final String[] lm_infmsg = new String[] {
    		"success (sum of squares below underflow limit)",
    	    "success (the relative error in the sum of squares is at most tol)",
    	    "success (the relative error between x and the solution is at most tol)",
    	    "success (both errors are at most tol)",
    	    "trapped by degeneracy (fvec is orthogonal to the columns of the jacobian)",
    	    "timeout (number of calls to fcn has reached maxcall*(n+1))",
    	    "failure (ftol<tol: cannot reduce sum of squares any further)",
    	    "failure (xtol<tol: cannot improve approximate solution any further)",
    	    "failure (gtol<tol: cannot improve approximate solution any further)",
    	    "exception (not enough memory)",
    	    "fatal coding error (improper input parameters)",
    	    "exception (break requested within function evaluation)"
    	};
    
    private final String[] lm_shortmsg = new String[] {
    		"success (0)",
    	    "success (f)",
    	    "success (p)",
    	    "success (f,p)",
    	    "degenerate",
    	    "call limit",
    	    "failed (f)",
    	    "failed (p)",
    	    "failed (o)",
    	    "no memory",
    	    "invalid input",
    	    "user break"
    };
    
    protected boolean debugMessages = false;
    
    protected boolean debugMatrix = false;
    
    /** m is a positive integer input variable set to the number of functions. */
    private int m;
    
    /** n is a positive integer variable set to the number of variables; n must not exceed m.*/
    private int n;
    
    /** x is an array of length n.  On input x must contain an initial estimate of the
     * solution vector.  On output x contains the final estimate of the solution vector.
     */
    protected double x[];
    
    /** Collection of status (output) parameters. */
    
    /** norm of the residue vector fvec */
    protected double fnorm[] = new double[1];
    
    /** actual number of iterations */
    protected int nfev[] = new int[1];
    
    /** status of minimization 
    *	info is an integer OUTPUT variable that indicates the termination
    *        status of lm_lmdif as follows:
    *
    *        info < 0  termination requested by user-supplied routine *evaluate;
    *
    *	  info = 0  fnorm almost vanishing;
    *
    *	  info = 1  both actual and predicted relative reductions
    *		    in the sum of squares are at most ftol;
    *
    *	  info = 2  relative error between two consecutive iterates
    *		    is at most xtol;
    *
    *	  info = 3  conditions for info = 1 and info = 2 both hold;
    *
    *	  info = 4  the cosine of the angle between fvec and any
    *		    column of the jacobian is at most gtol in
    *		    absolute value;
    *
    *	  info = 5  number of calls to lm_fcn has reached or
    *		    exceeded maxfev;
    *
    *	  info = 6  ftol is too small: no further reduction in
    *		    the sum of squares is possible;
    *
    *	  info = 7  xtol is too small: no further improvement in
    *		    the approximate solution x is possible;
    *
    *	  info = 8  gtol is too small: fvec is orthogonal to the
    *		    columns of the jacobian to machine precision;
    *
    *	  info =10  improper input parameters; */
    protected int info[] = new int[1];
    
    /** fvec is an output array of length m which contains the functions evaluated at
     * the output x.
     */
    private double fvec[];
    
    /** diag is an array of length n. If mode = 1, diag is
 *        internally set. If mode = 2, diag must contain positive entries
 *        that serve as multiplicative scale factors for the variables. */
    protected double diag[];
    
    /** qtf is an OUTPUT array of length n which contains
 *	  the first n elements of the vector (q transpose)*fvec. */
    private double qtf[];
    
    /** fjac is an OUTPUT m by n array. The upper n by n submatrix
 *	  of fjac contains an upper triangular matrix r with
 *	  diagonal elements of nonincreasing magnitude such that
 *
 *		pT*(jacT*jac)*p = rT*r
 *
 *              (NOTE: T stands for matrix transposition),
 *
 *	  where p is a permutation matrix and jac is the final
 *	  calculated jacobian. Column j of p is column ipvt(j)
 *	  (see below) of the identity matrix. The lower trapezoidal
 *	  part of fjac contains information generated during
 *	  the computation of r. */
    private double fjac[];
    
   /** wa1, wa2, and wa3 are work arrays of length n. */
    private double wa1[];
    private double wa2[];
    private double wa3[];
    
    /** wa4 is a work array of length m, used among others to hold
 *        residuals from evaluate. */
    private double wa4[];
    
    /** ipvt is an integer OUTPUT array of length n. It defines a
 *        permutation matrix p such that jac*p = q*r, where jac is
 *        the final calculated jacobian, q is orthogonal (not stored),
 *        and r is upper triangular with diagonal elements of
 *        nonincreasing magnitude. Column j of p is column ipvt(j)
 *        of the identity matrix. */
    private int ipvt[];
    
    /**
     * 
     * @param m the number of functions
     * @param n the number of variables; n must not exceed m.
     */
    public Lmmin(int m, int n) {
    	this.m = m;
    	this.n = n;
    	
    	try {
    	    fvec = new double[m];
    	    diag = new double[n];
    	    qtf = new double[n];
    	    fjac = new double[m*n];
    	    wa1 = new double[n];
    	    wa2 = new double[n];
    	    wa3 = new double[n];
    	    wa4 = new double[m];
    	    ipvt = new int[n];
    	}
    	catch (OutOfMemoryError e) {
    	    info[0] = 9;	
    	}
    }
    
    /**
     * driver.
     */
    public void driver() {
    	int i;
    	int iter;
    	int j;
    	double actred;
    	double delta;
    	double dirder;
    	double eps;
    	double fnorm;
    	double fnorm1;
    	double gnorm;
    	double par;
    	double pnorm;
    	double prered;
    	double ratio;
    	double step;
    	double sum;
    	double temp;
    	double temp1;
    	double temp2;
    	double temp3;
    	double xnorm;
    	double p1 = 0.1;
    	double p0001 = 1.0e-4;
    	
    	if (mode == 1) {
    		for (j = 0; j < n; j++) {
    			diag[j] = 1.0;
    		}
    	}
    	
    	/** perform fit ***/
    	info[0] = 0;
    	maxfev = maxcall * (n + 1);
    	
    	nfev[0] = 0; /* function evaluation counter */
    	iter = 0;    /* outer loop counter */
    	par = 0;     /* levenberg-marquardt parameter */
    	delta = 0.0; /* to prevent a warning (initialization within if-clause) */
    	xnorm = 0.0; /* ditto */
    	temp = Math.max(epsfcn, LM_MACHEP);
    	eps = Math.sqrt(temp); /* for calculating the Jacobianby forward differences */
    	
    	/** Check input parameters for errors */
    	if ((n <= 0) || (m < n) || (ftol < 0.0) || (xtol < 0.0) || (gtol < 0.0) ||
    		(maxfev <= 0) || (factor <= 0.0)) {
    		info[0] = 10; // invalid parameter
    		return;
    	}
    	
    	if (mode == 2) {     /* scaling by diag[] */
    	    for (j = 0; j < n; j++) {    /* Check for nonpositive elements */
    	        if (diag[j] <= 0.0) {
    	        	info[0] = 10; // invalid parameter
    	        	return;
    	        }
    	    }
    	}  // if (mode == 2)
    	
    	/*** evaluate function at starting point and calculate norm. ***/
    	if (testMode) {
        	fitToTestFunction(x, fvec);
        }
        else {
            fitToFunction(x, fvec, info);
        }
    	nfev[0]++;
    	lm_printout_std(x, fvec, printflags, 0, 0, nfev[0]);
    	if (info[0] < 0) {
    		return;
    	}
    	fnorm = lm_enorm(m, fvec);
    	if (fnorm <= LM_DWARF) {
    		info[0] = 0;
    		return;
    	}
    	
    	/*** The outer loop */
    	if (debugMessages) {
    		Preferences.debug("outer loop\n");
    		Preferences.debug("iter = " + iter + "\n");
    		Preferences.debug("nfev[0] = " + nfev[0] + "\n");
    		Preferences.debug("fnorm = " + fnorm + "\n");
    	}
    	
    	/*** outer: calculate the jacobian ***/
    	for (j = 0; j < n; j++) {
    		temp = x[j];
    		step = Math.max(eps*eps, eps * Math.abs(temp));
    		x[j] = temp + step; /* replace temporarily */
    		info[0] = 0;
    		if (testMode) {
            	fitToTestFunction(x, wa4);
            }
            else {
                fitToFunction(x, wa4, info);
            }
            lm_printout_std(x, wa4, printflags, 1, iter, nfev[0]);
            if (info[0] < 0) {
            	return; /* user requested break */
            }
            for (i = 0; i < m; i++) {
            	fjac[j*m + i] = (wa4[i] - fvec[i])/step;
            }
            x[j] = temp; /* restore */
    	} // for (j = 0; j < n; j++)
    	
    	if (debugMatrix) {
    		/* print the entire matrix */
    		for (i = 0; i < m; i++) {
    			for (j = 0; j < n; j++) {
    				Preferences.debug(fjac[j*m + i] + " ");
    			}
    			Preferences.debug("\n");
    		} // for (i = 0; i < m; i++)
    	} // if (debugMatrix)
    	
    	/*** outer: compute the qr factorization of the Jacobian. ***/
    } // private void driver
    
    public abstract void fitToFunction(double x[], double fvec[], int info[]);
    
    /*****************************************************************************/
    /*  lm_printout_std (default monitoring routine)                             */
    /*****************************************************************************/

    private void lm_printout_std( final double []par,
                          final double[] fvec,
                          int printflags, int iflag, int iter, int nfev)
    /*
     *       iflag : 0 (init) 1 (outer loop) 2(inner loop) -1(terminated)
     *       iter  : outer loop counter
     *       nfev  : number of calls to *evaluate
     */
    {
        if( printflags == 0)
            return;

        int i;

        if( (printflags & 1) != 0 ){
            /* location of printout call within lmdif */
            if (iflag == 2) {
                Preferences.debug("trying step in gradient direction  \n");
            } else if (iflag == 1) {
                Preferences.debug("determining gradient iteration = " + iter + "\n");
            } else if (iflag == 0) {
                Preferences.debug("starting minimization              \n");
            } else if (iflag == -1) {
                Preferences.debug("terminated after " + nfev + " evaluations   \n");
            }
        }

        if( (printflags & 2) != 0 ){
            Preferences.debug("  par: \n");
            for (i = 0; i < n; ++i)
                Preferences.debug(par[i] + "\n");
            Preferences.debug(" norm = " +  lm_enorm(m, fvec) + "\n");
        }

        if( (printflags & 3) != 0 )
            Preferences.debug( "\n" );

        if ( ((printflags & 8)!= 0) || (((printflags & 4) != 0) && iflag == -1) ) {
    	Preferences.debug("  residuals:\n");
    	for (i = 0; i < m; ++i)
    	    Preferences.debug("    fvec[" + i + "] = " + fvec[i] + "\n");
        }
    }
    
    /*****************************************************************************/
    /*  lm_enorm (Euclidean norm)                                                */
    /*****************************************************************************/

    double lm_enorm(int n, final double []x)
    {
    /*     Given an n-vector x, this function calculates the
     *     euclidean norm of x.
     *
     *     The euclidean norm is computed by accumulating the sum of
     *     squares in three different sums. The sums of squares for the
     *     small and large components are scaled so that no overflows
     *     occur. Non-destructive underflows are permitted. Underflows
     *     and overflows do not occur in the computation of the unscaled
     *     sum of squares for the intermediate components.
     *     The definitions of small, intermediate and large components
     *     depend on two constants, LM_SQRT_DWARF and LM_SQRT_GIANT. The main
     *     restrictions on these constants are that LM_SQRT_DWARF**2 not
     *     underflow and LM_SQRT_GIANT**2 not overflow.
     *
     *     Parameters
     *
     *	n is a positive integer input variable.
     *
     *	x is an input array of length n.
     */
        int i;
        double agiant, s1, s2, s3, xabs, x1max, x3max, temp;

        s1 = 0;
        s2 = 0;
        s3 = 0;
        x1max = 0;
        x3max = 0;
        agiant = LM_SQRT_GIANT / n;

        /** sum squares. **/

        for (i = 0; i < n; i++) {
    	xabs = Math.abs(x[i]);
    	if (xabs > LM_SQRT_DWARF) {
                if ( xabs < agiant ) {
                    s2 += xabs * xabs;
                } else if ( xabs > x1max ) {
    		temp = x1max / xabs;
    		s1 = 1 + s1 * (temp * temp);
    		x1max = xabs;
    	    } else {
    		temp = xabs / x1max;
    		s1 += (temp * temp);
    	    }
    	} else if ( xabs > x3max ) {
    	    temp = x3max / xabs;
    	    s3 = 1 + s3 * (temp * temp);
    	    x3max = xabs;
    	} else if (xabs != 0.) {
                temp = xabs / x3max;
                s3 += (temp * temp);
    	}
        }

        /** calculation of norm. **/

        if (s1 != 0)
    	return x1max * Math.sqrt(s1 + (s2 / x1max) / x1max);
        else if (s2 != 0)
            if (s2 >= x3max)
                return Math.sqrt(s2 * (1 + (x3max / s2) * (x3max * s3)));
            else
                return Math.sqrt(x3max * ((s2 / x3max) + (x3max * s3)));
        else
            return x3max * Math.sqrt(s3);

    } /*** lm_enorm. ***/
    
    private void fitToTestFunction(final double par[], double fvec[]) {
    	
    }

    
}