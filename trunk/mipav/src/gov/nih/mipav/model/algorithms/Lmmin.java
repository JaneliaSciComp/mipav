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
 * Jorge J. More`, Steve Moshier, and the authors of lapack. To avoid
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

/**
 * To run self tests in another module put:
 *      if (testMode) {
    		new FitAll();
            setCompleted(false);
            return;
    	}
    	
    	class FitAll extends Lmmin {

        public FitAll() {

            super();

        }

        
        public void fitToFunction(double x[], double fvec[], int info[]) {

            return;
        }
    }
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
    // A constant holding the largest positive finite value of type double, (2-2**-52)*2**1023. 
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
    protected double fnorm;
    
    /** actual number of iterations */
    protected int nfev;
    
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
    private double par[] = new double[1];
    
    private int testCase;
    
    private double xSeries[];
    
    private double ySeries[];
    
    private final int DRAPER24D = 0;
    
    private final int ROSENBROCK = 1;
    
    private final int FREUDENSTEIN_AND_ROTH = 2;
    
    private final int JENNRICH_AND_SAMPSON = 6;
    
    private final int HELICAL_VALLEY = 7;
    
    private final int BARD = 8;
    
    private final int MEYER = 10;
    
    private final int BOX_3D = 12;
    
    private final int POWELL_SINGULAR = 13;
    
    private final int KOWALIK_AND_OSBORNE = 15;
    
    private final int BROWN_AND_DENNIS = 16;
    
    private final int OSBORNE1 = 17;
    
    private final int OSBORNE2 = 19;
    
    private final int WATSON = 20;
    
    private final int HOCK25 = 25;
    
    private final int BROWN_ALMOST_LINEAR = 27;
    
    private final int LINEAR_FULL_RANK = 32;
    
    private final int LINEAR_RANK1 = 33;
    
    private final int LINEAR_RANK1_WITH_ZERO_COLUMNS_AND_ROWS = 34;
    
    private final int CHEBYQUAD = 35;
    
    private final int LEVMAR_ROSENBROCK = 50;
    
    private final int MODIFIED_ROSENBROCK = 51;
    
    private final int POWELL_2_PARAMETER = 52;
    
    private final int WOOD = 53;
    
    private final int HOCK1 = 61;
    
    private final int HOCK21_MODIFIED = 62;
    
    private final int HATFLDB = 63;
    
    private final int HATFLDC = 64;
    
    private final int EQUILIBRIUM_COMBUSTION = 65;
    
    public Lmmin() {
    	int i;
    	// Below is an example used to fit y = a0 - a1*(a2**x)
    	// This example implements the solution of problem D of chapter 24 of Applied Regression Analysis, Third Edition by
    	// Norman R. Draper and Harry Smith */
    	// The correct answer is a0 = 72.4326,  a1 = 28.2519, a2 = 0.5968
    	Preferences.debug("Draper problem 24D y = a0 - a1*(a2**x)\n", Preferences.DEBUG_ALGORITHM);
    	Preferences.debug("Correct answer is a0 = 72.4326, a1 = 28.2519, a2 = 0.5968\n", Preferences.DEBUG_ALGORITHM);
    	//Run gives correct results:
    	//success (the relative error in the sum of squares is at most tol)
    	//Number of iterations: 21
    	//Residue vector: 1.8891280522613172
    	//x[0 ]= 72.43262770735069
    	//x[1 ]= 28.251874403060796
    	//x[2 ]= 0.5967899087159795
    	testMode = true;
    	testCase = DRAPER24D;
    	m = 5;
    	n = 3;
        xSeries = new double[m];
        ySeries = new double[m];
        x = new double[n];
        xSeries[0] = 0.0;
        xSeries[1] = 1.0;
        xSeries[2] = 2.0;
        xSeries[3] = 3.0;
        xSeries[4] = 4.0;
        ySeries[0] = 44.4;
        ySeries[1] = 54.6;
        ySeries[2] = 63.8;
        ySeries[3] = 65.7;
        ySeries[4] = 68.9;
        fitTestModel();
        x[0] = 0.0;
        x[1] = 10.0;
        x[2] = 0.2;
        driver();
        dumpTestResults();
        
        // Below is an example used to fit y = (a0 * log(0.01*i)**(a1) + a2
    	// where a0 = -50, a1 = 2.0/3.0, a2 = 25.0
    	// Variant of test example 25 from Hock and Schittkowski
        Preferences.debug("Test example 25 from Hock and Schittkowski\n", Preferences.DEBUG_ALGORITHM);
        Preferences.debug("y = (a0 * log(0.01*i)**(a1) + a2\n", Preferences.DEBUG_ALGORITHM);
        Preferences.debug("Correct answer is a0 = -50, a1 = 2.0/3.0, a3 = 25.0\n", Preferences.DEBUG_ALGORITHM);
        // Run gives correct results:
        // success (the relative error between x and the solution is at most tol)
        // Number of iterations: 18
        // Residue vector: 3.5704330589399866E-14
        // x[0 ]= -50.000000000000014
        // x[1 ]= 0.6666666666666666
        // x[2 ]= 25.0
        testMode = true;
        testCase = HOCK25;
        m = 99;
        n = 3;
    	xSeries = new double[m];
    	ySeries = new double[m];
    	x = new double[n];
    	for (i = 1; i <= m; i++) {
    		xSeries[i-1] = 0.01 * i;
    		ySeries[i-1] = Math.pow((-50.0 * Math.log(xSeries[i-1])),2.0/3.0) + 25.0;
    	}
    	fitTestModel();
    	x[0] = -100.0;
    	x[1] = 1.0/3.0;
    	x[2] = 12.5;
    	driver();
    	dumpTestResults();
    	
    	// Below is an example to fit y = a0 + x/(a1*(16 - x) + a2*min(x,16-x))
        // From Testing Unconstrained Optimization Software by More, Garbow, and Hillstrom
        Preferences.debug("Bard function standard starting point\n", Preferences.DEBUG_ALGORITHM);
        Preferences.debug("y = a0 + x/(a1*(16-x) + a2*min(x,16-x))\n", Preferences.DEBUG_ALGORITHM);
        Preferences.debug("Correct answer is a0 = 0.08241, a1 = 1.133, a2 = 2.344\n", Preferences.DEBUG_ALGORITHM);
        Preferences.debug("Correct answer has Chi-squared = 8.21487E-3\n", Preferences.DEBUG_ALGORITHM);
        // Run gives correct results:
        // success (the relative error in the sum of squares is at most tol)
        // Number of iterations: 8
        // Residue vector: 0.0906359603390342
        // x[0 ]= 0.08241055953800391
        // x[1 ]= 1.1330360851163723
        // x[2 ]= 2.343695185250993
        testMode = true;
        testCase = BARD;
        m = 15;
        n = 3;
        xSeries = new double[m];
        for (i = 1; i <= m; i++) {
        	xSeries[i-1] = i;
        }
        ySeries = new double[]{0.14, 0.18, 0.22, 0.25, 0.29, 0.32, 0.35, 0.39, 
        		               0.37, 0.58, 0.73, 0.96, 1.34, 2.10, 4.39};
        x = new double[n];
        fitTestModel();
        x[0] = 1.0;
        x[1] = 1.0;
        x[2] = 1.0;
        driver();
        dumpTestResults();
        
        // Below is an example to fit y = a0 + x/(a1*(16 - x) + a2*min(x,16-x))
        // From Testing Unconstrained Optimization Software by More, Garbow, and Hillstrom
        Preferences.debug("Bard function 10 * standard starting point\n", Preferences.DEBUG_ALGORITHM);
        Preferences.debug("y = a0 + x/(a1*(16-x) + a2*min(x,16-x))\n", Preferences.DEBUG_ALGORITHM);
        Preferences.debug("Correct answer is a0 = 0.08241, a1 = 1.133, a2 = 2.344\n", Preferences.DEBUG_ALGORITHM);
        Preferences.debug("Correct answer has Chi-squared = 8.21487E-3\n", Preferences.DEBUG_ALGORITHM);
        // Run gives incorrect results and does not spot that it has failed:
        //success (both errors are at most tol)
        //Number of iterations: 52
        //Residue vector: 4.174768658261074
        //x[0 ]= 0.8406666655903977
        //x[1 ]= -3.6341655556676335E9
        //x[2 ]= -3.673607341621811E9
        // The incorrect results are as expected because a1 is a root in the denominator and so with no constraints
        // a[1] = 0 will result in an inifinity. Note that unconstrained ELSUNC worked only at the standard starting
        // point, but not at 10 or 100 times the standard starting point.  ELSUNC constrained with a[1] >= 0.1 did 
        // work at 10.0 * standard starting point and 100.0 * standard starting point. NL2sol worked at the standard
        // starting point and 100.0 * standard starting point, but NL2sol did not work at 10.0 * standard starting
        // point.
        testMode = true;
        testCase = BARD;
        m = 15;
        n = 3;
        xSeries = new double[m];
        for (i = 1; i <= m; i++) {
        	xSeries[i-1] = i;
        }
        ySeries = new double[]{0.14, 0.18, 0.22, 0.25, 0.29, 0.32, 0.35, 0.39, 
        		               0.37, 0.58, 0.73, 0.96, 1.34, 2.10, 4.39};
        x = new double[n];
        fitTestModel();
        x[0] = 10.0;
        x[1] = 10.0;
        x[2] = 10.0;
        driver();
        dumpTestResults();
        
        // Below is an example to fit y = a0 + x/(a1*(16 - x) + a2*min(x,16-x))
        // From Testing Unconstrained Optimization Software by More, Garbow, and Hillstrom
        Preferences.debug("Bard function 100 * standard starting point\n", Preferences.DEBUG_ALGORITHM);
        Preferences.debug("y = a0 + x/(a1*(16-x) + a2*min(x,16-x))\n", Preferences.DEBUG_ALGORITHM);
        Preferences.debug("Correct answer is a0 = 0.08241, a1 = 1.133, a2 = 2.344\n", Preferences.DEBUG_ALGORITHM);
        Preferences.debug("Correct answer has Chi-squared = 8.21487E-3\n", Preferences.DEBUG_ALGORITHM);
        // Run gives incorrect results and does not spot that it has failed:
        //success (the relative error in the sum of squares is at most tol)
        //Number of iterations: 23
        //Residue vector: 4.17476866065162
        //x[0 ]= 0.8406666682140694
        //x[1 ]= -1.6587319041298513E9
        //x[2 ]= -1.6666323846381438E9
        // The incorrect results are as expected because a1 is a root in the denominator and so with no constraints
        // a[1] = 0 will result in an inifinity. Note that unconstrained ELSUNC worked only at the standard starting
        // point, but not at 10 or 100 times the standard starting point.  ELSUNC constrained with a[1] >= 0.1 did 
        // work at 10.0 * standard starting point and 100.0 * standard starting point. NL2sol worked at the standard
        // starting point and 100.0 * standard starting point, but NL2sol did not work at 10.0 * standard starting
        // point.
        testMode = true;
        testCase = BARD;
        m = 15;
        n = 3;
        xSeries = new double[m];
        for (i = 1; i <= m; i++) {
        	xSeries[i-1] = i;
        }
        ySeries = new double[]{0.14, 0.18, 0.22, 0.25, 0.29, 0.32, 0.35, 0.39, 
        		               0.37, 0.58, 0.73, 0.96, 1.34, 2.10, 4.39};
        x = new double[n];
        fitTestModel();
        x[0] = 100.0;
        x[1] = 100.0;
        x[2] = 100.0;
        driver();
        dumpTestResults();
        
        // Below is an example to fit y = a0*(x**2 + a1*x)/(x**2 + a2*x + a3)
        // From Testing Unconstrained Optimization Software by More, Garbow, and Hillstrom
        Preferences.debug("Kowalik and Osborne function standard starting point\n", Preferences.DEBUG_ALGORITHM);
        Preferences.debug("y = a0*(x**2 + a1*x)/(x**2 + a2*x + a3)\n", Preferences.DEBUG_ALGORITHM);
        Preferences.debug("Correct answer is a0 = 0.1928, a1 = 0.1913, a2 = 0.1231, a3 = 0.1361\n", 
        		Preferences.DEBUG_ALGORITHM);
        Preferences.debug("Correct answer has Chi-squared = 3.07505E-4\n", Preferences.DEBUG_ALGORITHM);
        //Run gives correct results:
        //success (the relative error in the sum of squares is at most tol)
        //Number of iterations: 34
        //Residue vector: 0.017535837700242276
        //x[0 ]= 0.19280693466293072
        //x[1 ]= 0.19128232778972068
        //x[2 ]= 0.12305650749330135
        //x[3 ]= 0.13606233002421278
        testMode = true;
        testCase = KOWALIK_AND_OSBORNE;
        m = 11;
        n = 4;
        xSeries = new double[]{4.0, 2.0, 1.0, 0.5, 0.25, 0.167, 0.125, 0.1, 0.0833, 0.0714, 0.0625};
        ySeries = new double[]{0.1957, 0.1947, 0.1735, 0.1600, 0.0844, 0.0627, 0.0456, 0.0342, 
        		               0.0323, 0.0235, 0.0246};
        x = new double[4];
        fitTestModel();
        x[0] = 0.25;
        x[1] = 0.39;
        x[2] = 0.415;
        x[3] = 0.39;
        driver();
        dumpTestResults();
        
        // Below is an example to fit y = a0*(x**2 + a1*x)/(x**2 + a2*x + a3)
        // From Testing Unconstrained Optimization Software by More, Garbow, and Hillstrom
        Preferences.debug("Kowalik and Osborne function 10 * standard starting point\n", Preferences.DEBUG_ALGORITHM);
        Preferences.debug("y = a0*(x**2 + a1*x)/(x**2 + a2*x + a3)\n", Preferences.DEBUG_ALGORITHM);
        Preferences.debug("Correct answer is a0 = 0.1928, a1 = 0.1913, a2 = 0.1231, a3 = 0.1361\n", 
        		Preferences.DEBUG_ALGORITHM);
        Preferences.debug("Correct answer has Chi-squared = 3.07505E-4\n", Preferences.DEBUG_ALGORITHM);
        // Run gives incorrect results and does not spot that it has failed:
        //success (the relative error in the sum of squares is at most tol)
        //Number of iterations: 158
        //Residue vector: 0.03205219300245665
        //x[0 ]= 585478.320868832
        //x[1 ]= -14.075878972638181
        //x[2 ]= -2.6497096463223256E7
        //x[3 ]= -1.6528924108883783E7
        testMode = true;
        testCase = KOWALIK_AND_OSBORNE;
        m = 11;
        n = 4;
        xSeries = new double[]{4.0, 2.0, 1.0, 0.5, 0.25, 0.167, 0.125, 0.1, 0.0833, 0.0714, 0.0625};
        ySeries = new double[]{0.1957, 0.1947, 0.1735, 0.1600, 0.0844, 0.0627, 0.0456, 0.0342, 
        		               0.0323, 0.0235, 0.0246};
        x = new double[4];
        fitTestModel();
        x[0] = 2.5;
        x[1] = 3.9;
        x[2] = 4.15;
        x[3] = 3.9;
        driver();
        dumpTestResults();
        
        // Below is an example to fit y = a0*(x**2 + a1*x)/(x**2 + a2*x + a3)
        // From Testing Unconstrained Optimization Software by More, Garbow, and Hillstrom
        Preferences.debug("Kowalik and Osborne function 100 * standard starting point\n", Preferences.DEBUG_ALGORITHM);
        Preferences.debug("y = a0*(x**2 + a1*x)/(x**2 + a2*x + a3)\n", Preferences.DEBUG_ALGORITHM);
        Preferences.debug("Correct answer is a0 = 0.1928, a1 = 0.1913, a2 = 0.1231, a3 = 0.1361\n", 
        		Preferences.DEBUG_ALGORITHM);
        Preferences.debug("Correct answer has Chi-squared = 3.07505E-4\n", Preferences.DEBUG_ALGORITHM);
        // Run gives correct results:
        //success (the relative error in the sum of squares is at most tol)
        //Number of iterations: 389
        //Residue vector: 0.01753583770024227
        //x[0 ]= 0.19280693501318796
        //x[1 ]= 0.19128230851839392
        //x[2 ]= 0.12305649773710475
        //x[3 ]= 0.13606232215470096
        testMode = true;
        testCase = KOWALIK_AND_OSBORNE;
        m = 11;
        n = 4;
        xSeries = new double[]{4.0, 2.0, 1.0, 0.5, 0.25, 0.167, 0.125, 0.1, 0.0833, 0.0714, 0.0625};
        ySeries = new double[]{0.1957, 0.1947, 0.1735, 0.1600, 0.0844, 0.0627, 0.0456, 0.0342, 
        		               0.0323, 0.0235, 0.0246};
        x = new double[4];
        fitTestModel();
        x[0] = 25.0;
        x[1] = 39.0;
        x[2] = 41.5;
        x[3] = 39.0;
        driver();
        dumpTestResults();
    }
    
    private void fitTestModel() {
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
    
    private void dumpTestResults() {
    	int i;
    	Preferences.debug("Number of iterations: " + String.valueOf(nfev) + "\n", Preferences.DEBUG_ALGORITHM);
        Preferences.debug("Residue vector: " + String.valueOf(fnorm) + "\n", Preferences.DEBUG_ALGORITHM);
        for (i = 0; i < n; i++) {
            Preferences.debug("x[" + i + " ]= " + String.valueOf(x[i]) + "\n", Preferences.DEBUG_ALGORITHM);
        }
    }
    
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
    	lm_lmdif();
    	lm_printout_std(par, fvec, printflags, -1, 0, nfev);
    	
        fnorm = lm_enorm(m, fvec, 0);
        if (info[0] < 0 )
    	info[0] = 11;
        Preferences.debug("\n" + lm_infmsg[info[0]] + "\n", Preferences.DEBUG_ALGORITHM);

    /*** clean up. ***/

        fvec = null;
        diag = null;
        qtf = null;
        fjac = null;
        wa1 = null;
        wa2 = null;
        wa3 = null;
        wa4 = null;
        ipvt = null;
    }
    
    private void lm_lmdif() {
    	int i;
    	int iter;
    	int j;
    	double actred;
    	double delta;
    	double dirder;
    	double eps;
    	double fnorm1;
    	double gnorm;
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
    	
    	nfev = 0; /* function evaluation counter */
    	iter = 0;    /* outer loop counter */
    	par[0] = 0;     /* levenberg-marquardt parameter */
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
    	
    	if (debugMessages) {
    		Preferences.debug("lm_lmdif\n", Preferences.DEBUG_ALGORITHM);
    	}
    	
    	/*** evaluate function at starting point and calculate norm. ***/
    	if (testMode) {
        	fitToTestFunction(x, fvec);
        }
        else {
            fitToFunction(x, fvec, info);
        }
    	nfev++;
    	lm_printout_std(x, fvec, printflags, 0, 0, nfev);
    	if (info[0] < 0) {
    		return;
    	}
    	fnorm = lm_enorm(m, fvec, 0);
    	if (fnorm <= LM_DWARF) {
    		info[0] = 0;
    		return;
    	}
    	
    	/*** The outer loop */
    	do {
    	if (debugMessages) {
    		Preferences.debug("outer loop\n", Preferences.DEBUG_ALGORITHM);
    		Preferences.debug("iter = " + iter + "\n", Preferences.DEBUG_ALGORITHM);
    		Preferences.debug("nfev = " + nfev + "\n", Preferences.DEBUG_ALGORITHM);
    		Preferences.debug("fnorm = " + fnorm + "\n", Preferences.DEBUG_ALGORITHM);
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
    		nfev++;
            lm_printout_std(x, wa4, printflags, 1, iter, nfev);
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
    				Preferences.debug(fjac[j*m + i] + " ", Preferences.DEBUG_ALGORITHM);
    			}
    			Preferences.debug("\n", Preferences.DEBUG_ALGORITHM);
    		} // for (i = 0; i < m; i++)
    	} // if (debugMatrix)
    	
    	/*** outer: compute the qr factorization of the Jacobian. ***/
    	lm_qrfac(m, n, fjac, 1, ipvt, wa1, wa2, wa3);
        /* return values are ipvt, wa1=rdiag, wa2=acnorm */

	if (iter == 0) { 
            /* first iteration only */
	    if (mode != 2) {
                /* diag := norms of the columns of the initial Jacobian */
		for (j = 0; j < n; j++) {
		    diag[j] = wa2[j];
		    if (wa2[j] == 0.)
			diag[j] = 1.;
		}
	    }
            /* use diag to scale x, then calculate the norm */
	    for (j = 0; j < n; j++)
		wa3[j] = diag[j] * x[j];
	    xnorm = lm_enorm(n, wa3, 0);
            /* initialize the step bound delta. */
	    delta = factor * xnorm;
	    if (delta == 0.)
		delta = factor;
	} else {
            if (mode != 2) {
                for (j = 0; j < n; j++)
                    diag[j] = Math.max( diag[j], wa2[j] );
            }
        }

/*** outer: form (q transpose)*fvec and store first n components in qtf. ***/

	for (i = 0; i < m; i++)
	    wa4[i] = fvec[i];

	for (j = 0; j < n; j++) {
	    temp3 = fjac[j*m+j];
	    if (temp3 != 0.) {
		sum = 0;
		for (i = j; i < m; i++)
		    sum += fjac[j*m+i] * wa4[i];
		temp = -sum / temp3;
		for (i = j; i < m; i++)
		    wa4[i] += fjac[j*m+i] * temp;
	    }
	    fjac[j*m+j] = wa1[j];
	    qtf[j] = wa4[j];
	}

/*** outer: compute norm of scaled gradient and test for convergence. ***/

	gnorm = 0;
        for (j = 0; j < n; j++) {
            if (wa2[ipvt[j]] == 0)
                continue;
            sum = 0.;
            for (i = 0; i <= j; i++)
                sum += fjac[j*m+i] * qtf[i];
            gnorm = Math.max( gnorm, Math.abs( sum / wa2[ipvt[j]] / fnorm ) );
        }

	if (gnorm <= gtol) {
	    info[0] = 4;
	    return;
	}

/*** the inner loop. ***/
	do {
		if (debugMessages) {
			    Preferences.debug("lm_lmdif/ inner loop iter = " + iter + " nfev[0] = " + nfev + "\n", 
			    		Preferences.DEBUG_ALGORITHM);
		}

		/*** inner: determine the levenberg-marquardt parameter. ***/

			    lm_lmpar( n, fjac, m, ipvt, diag, qtf, delta, par,
		                      wa1, wa2, wa4, wa3 );
		            /* used return values are fjac (partly), par, wa1=x, wa3=diag*x */

			    for (j = 0; j < n; j++)
				wa2[j] = x[j] - wa1[j]; /* new parameter vector ? */

			    pnorm = lm_enorm(n, wa3, 0);

		            /* at first call, adjust the initial step bound. */

			    if (nfev <= 1+n)
				delta = Math.min(delta, pnorm);

		/*** inner: evaluate the function at x + p and calculate its norm. ***/

			    info[0] = 0;
			    if (testMode) {
	            	fitToTestFunction(wa2, wa4);
	            }
	            else {
	                fitToFunction(wa4, wa4, info);
	            }
		            ++nfev;
		            lm_printout_std(wa2, wa4, printflags, 2, iter, nfev);
			    if (info[0] < 0)
				return; /* user requested break. */

			    fnorm1 = lm_enorm(m, wa4, 0);
		if (debugMessages) {
			    Preferences.debug("lm_lmdif/ pnorm = " + pnorm + " fnorm1 = " + fnorm1 + "\n",
			    		Preferences.DEBUG_ALGORITHM);
			    Preferences.debug("fnorm = " + fnorm + " delta = " + delta + " par[0] = " + par[0] + "\n",
			    		Preferences.DEBUG_ALGORITHM);
		}

		/*** inner: compute the scaled actual reduction. ***/

			    if (p1 * fnorm1 < fnorm) {
			    	temp = fnorm1 / fnorm;
				    actred = 1 - (temp * temp);
			    }
			    else
				actred = -1;

		/*** inner: compute the scaled predicted reduction and 
		     the scaled directional derivative. ***/

			    for (j = 0; j < n; j++) {
				wa3[j] = 0;
				for (i = 0; i <= j; i++)
				    wa3[i] -= fjac[j*m+i] * wa1[ipvt[j]];
			    }
			    temp1 = lm_enorm(n, wa3, 0) / fnorm;
			    temp2 = Math.sqrt(par[0]) * pnorm / fnorm;
			    prered = (temp1*temp1) + 2 * (temp2*temp2);
			    dirder = -((temp1*temp1) + (temp2*temp2));

		/*** inner: compute the ratio of the actual to the predicted reduction. ***/

			    ratio = prered != 0 ? actred / prered : 0;
		if (debugMessages) {
			    Preferences.debug("lm_lmdif/ actred = " + actred + " prered = " + prered + "\n",
			    		Preferences.DEBUG_ALGORITHM);
			    if (prered != 0.0) {
			    	Preferences.debug("ratio = " + ratio + "\n", Preferences.DEBUG_ALGORITHM);
			    }
			    else {
			    	Preferences.debug("ratio = 0.0\n", Preferences.DEBUG_ALGORITHM);
			    }
				Preferences.debug("temp1*temp1 = " + (temp1*temp1) + "\n", Preferences.DEBUG_ALGORITHM);
				Preferences.debug("temp2*temp2 = " + (temp2*temp2) + "\n", Preferences.DEBUG_ALGORITHM);
				Preferences.debug("dirder = " + dirder + "\n", Preferences.DEBUG_ALGORITHM);
		}

		/*** inner: update the step bound. ***/

			    if (ratio <= 0.25) {
				if (actred >= 0.)
				    temp = 0.5;
				else
				    temp = 0.5 * dirder / (dirder + 0.55 * actred);
				if (p1 * fnorm1 >= fnorm || temp < p1)
				    temp = p1;
				delta = temp * Math.min(delta, pnorm / p1);
				par[0] /= temp;
			    } else if (par[0] == 0. || ratio >= 0.75) {
				delta = pnorm / 0.5;
				par[0] *= 0.5;
			    }

		/*** inner: test for successful iteration. ***/

			    if (ratio >= p0001) {
		                /* yes, success: update x, fvec, and their norms. */
				for (j = 0; j < n; j++) {
				    x[j] = wa2[j];
				    wa2[j] = diag[j] * x[j];
				}
				for (i = 0; i < m; i++)
				    fvec[i] = wa4[i];
				xnorm = lm_enorm(n, wa2, 0);
				fnorm = fnorm1;
				iter++;
			    }
			    else {
			    	if (debugMessages) {
				        Preferences.debug("ATTN: iteration considered unsuccessful\n", Preferences.DEBUG_ALGORITHM);
			    	}
			    }

		/*** inner: test for convergence. ***/

		            if( fnorm<=LM_DWARF ){
		                info[0] = 0;
		                return;
		            }

			    info[0] = 0;
			    if (Math.abs(actred) <= ftol && prered <= ftol && 0.5 * ratio <= 1)
				info[0] = 1;
			    if (delta <= xtol * xnorm)
				info[0] += 2;
			    if (info[0] != 0)
				return;

		/*** inner: tests for termination and stringent tolerances. ***/

			    if (nfev >= maxfev){
				info[0] = 5;
		                return;
		            }
			    if (Math.abs(actred) <= LM_MACHEP &&
				prered <= LM_MACHEP && 0.5 * ratio <= 1){
				info[0] = 6;
		                return;
		            }
			    if (delta <= LM_MACHEP * xnorm){
				info[0] = 7;
		                return;
		            }
			    if (gnorm <= LM_MACHEP){
				info[0] = 8;
				return;
		            }

		/*** inner: end of the loop. repeat if iteration unsuccessful. ***/

			} while (ratio < p0001);

		/*** outer: end of the loop. ***/

		    } while (true);
    } // private void lm_lmdif
    
    /*****************************************************************************/
    /*  lm_qrfac (QR factorisation, from lapack)                                 */
    /*****************************************************************************/

    private void lm_qrfac(int m, int n, double[] a, int pivot, int[] ipvt,
    	      double[] rdiag, double[] acnorm, double[] wa)
    {
    /*
     *     This subroutine uses householder transformations with column
     *     pivoting (optional) to compute a qr factorization of the
     *     m by n matrix a. That is, qrfac determines an orthogonal
     *     matrix q, a permutation matrix p, and an upper trapezoidal
     *     matrix r with diagonal elements of nonincreasing magnitude,
     *     such that a*p = q*r. The householder transformation for
     *     column k, k = 1,2,...,min(m,n), is of the form
     *
     *	    i - (1/u(k))*u*uT
     *
     *     where u has zeroes in the first k-1 positions. The form of
     *     this transformation and the method of pivoting first
     *     appeared in the corresponding linpack subroutine.
     *
     *     Parameters:
     *
     *	m is a positive integer input variable set to the number
     *	  of rows of a.
     *
     *	n is a positive integer input variable set to the number
     *	  of columns of a.
     *
     *	a is an m by n array. On input a contains the matrix for
     *	  which the qr factorization is to be computed. On OUTPUT
     *	  the strict upper trapezoidal part of a contains the strict
     *	  upper trapezoidal part of r, and the lower trapezoidal
     *	  part of a contains a factored form of q (the non-trivial
     *	  elements of the u vectors described above).
     *
     *	pivot is a logical input variable. If pivot is set true,
     *	  then column pivoting is enforced. If pivot is set false,
     *	  then no column pivoting is done.
     *
     *	ipvt is an integer OUTPUT array of length lipvt. This array
     *	  defines the permutation matrix p such that a*p = q*r.
     *	  Column j of p is column ipvt(j) of the identity matrix.
     *	  If pivot is false, ipvt is not referenced.
     *
     *	rdiag is an OUTPUT array of length n which contains the
     *	  diagonal elements of r.
     *
     *	acnorm is an OUTPUT array of length n which contains the
     *	  norms of the corresponding columns of the input matrix a.
     *	  If this information is not needed, then acnorm can coincide
     *	  with rdiag.
     *
     *	wa is a work array of length n. If pivot is false, then wa
     *	  can coincide with rdiag.
     *
     */
        int i, j, k, kmax, minmn;
        double ajnorm, sum, temp;

    /*** qrfac: compute initial column norms and initialize several arrays. ***/

        for (j = 0; j < n; j++) {
	    	acnorm[j] = lm_enorm(m, a, j*m);
	    	rdiag[j] = acnorm[j];
	    	wa[j] = rdiag[j];
	    	if (pivot != 0) {
	    	    ipvt[j] = j;
	    	}
        }
    if (debugMessages) {
        Preferences.debug("qrfac\n", Preferences.DEBUG_ALGORITHM);
    }

    /*** qrfac: reduce a to r with householder transformations. ***/

        minmn = Math.min(m, n);
        for (j = 0; j < minmn; j++) {
    	if (pivot != 0) {

            /** bring the column of largest norm into the pivot position. **/

    	kmax = j;
    	for (k = j + 1; k < n; k++)
    	    if (rdiag[k] > rdiag[kmax])
    		kmax = k;
    	if (kmax != j) {

    	for (i = 0; i < m; i++) {
    	    temp = a[j*m+i];
    	    a[j*m+i] = a[kmax*m+i];
    	    a[kmax*m+i] = temp;
    	}
    	rdiag[kmax] = rdiag[j];
    	wa[kmax] = wa[j];
    	k = ipvt[j];
    	ipvt[j] = ipvt[kmax];
    	ipvt[kmax] = k;
    	} // if (kmax != j)
    	} // if (pivot != 0)

            /** compute the Householder transformation to reduce the
                j-th column of a to a multiple of the j-th unit vector. **/

    	ajnorm = lm_enorm(m-j, a, j*m + j);
    	if (ajnorm == 0.) {
    	    rdiag[j] = 0;
    	    continue;
    	}

    	if (a[j*m+j] < 0.)
    	    ajnorm = -ajnorm;
    	for (i = j; i < m; i++)
    	    a[j*m+i] /= ajnorm;
    	a[j*m+j] += 1;

            /** apply the transformation to the remaining columns
                and update the norms. **/

    	for (k = j + 1; k < n; k++) {
    	    sum = 0;

    	    for (i = j; i < m; i++)
    		sum += a[j*m+i] * a[k*m+i];

    	    temp = sum / a[j + m * j];

    	    for (i = j; i < m; i++)
    		a[k*m+i] -= temp * a[j*m+i];

    	    if ((pivot != 0) && rdiag[k] != 0.) {
    		temp = a[m * k + j] / rdiag[k];
    		temp = Math.max(0., 1 - temp * temp);
    		rdiag[k] *= Math.sqrt(temp);
    		temp = rdiag[k] / wa[k];
    		if ( 0.05 * (temp*temp) <= LM_MACHEP ) {
    		    rdiag[k] = lm_enorm(m-j-1, a, m*k+j+1);
    		    wa[k] = rdiag[k];
    		} // if ( 0.05 * (temp*temp) <= LM_MACHEP )
    	    } // for (k = j + 1; k < n; k++)
    	} // for (k = j + 1; k < n; k++)

    	rdiag[j] = -ajnorm;
        } // for (j = 0; j < minmn; j++)
    }

    
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
                Preferences.debug("trying step in gradient direction  \n", Preferences.DEBUG_ALGORITHM);
            } else if (iflag == 1) {
                Preferences.debug("determining gradient iteration = " + iter + "\n", Preferences.DEBUG_ALGORITHM);
            } else if (iflag == 0) {
                Preferences.debug("starting minimization              \n", Preferences.DEBUG_ALGORITHM);
            } else if (iflag == -1) {
                Preferences.debug("terminated after " + nfev + " evaluations   \n", Preferences.DEBUG_ALGORITHM);
            }
        }

        if( (printflags & 2) != 0 ){
            Preferences.debug("  par: \n", Preferences.DEBUG_ALGORITHM);
            for (i = 0; i < n; ++i)
                Preferences.debug(par[i] + "\n", Preferences.DEBUG_ALGORITHM);
            Preferences.debug(" norm = " +  lm_enorm(m, fvec, 0) + "\n", Preferences.DEBUG_ALGORITHM);
        }

        if( (printflags & 3) != 0 )
            Preferences.debug( "\n", Preferences.DEBUG_ALGORITHM );

        if ( ((printflags & 8)!= 0) || (((printflags & 4) != 0) && iflag == -1) ) {
    	Preferences.debug("  residuals:\n", Preferences.DEBUG_ALGORITHM);
    	for (i = 0; i < m; ++i)
    	    Preferences.debug("    fvec[" + i + "] = " + fvec[i] + "\n", Preferences.DEBUG_ALGORITHM);
        }
    }
    
    /*****************************************************************************/
    /*  lm_lmpar (determine Levenberg-Marquardt parameter)                       */
    /*****************************************************************************/

    private void lm_lmpar(int n, double[] r, int ldr, int[] ipvt, double[] diag,
    	      double[] qtb, double delta, double[] par, double[] x,
    	      double[] sdiag, double[] aux, double[] xdi)
    {
    /*     Given an m by n matrix a, an n by n nonsingular diagonal
     *     matrix d, an m-vector b, and a positive number delta,
     *     the problem is to determine a value for the parameter
     *     par such that if x solves the system
     *
     *	    a*x = b  and  sqrt(par)*d*x = 0
     *
     *     in the least squares sense, and dxnorm is the euclidean
     *     norm of d*x, then either par=0 and (dxnorm-delta) < 0.1*delta,
     *     or par>0 and abs(dxnorm-delta) < 0.1*delta.
     *
     *     Using lm_qrsolv, this subroutine completes the solution of the problem
     *     if it is provided with the necessary information from the
     *     qr factorization, with column pivoting, of a. That is, if
     *     a*p = q*r, where p is a permutation matrix, q has orthogonal
     *     columns, and r is an upper triangular matrix with diagonal
     *     elements of nonincreasing magnitude, then lmpar expects
     *     the full upper triangle of r, the permutation matrix p,
     *     and the first n components of qT*b. On output
     *     lmpar also provides an upper triangular matrix s such that
     *
     *	    pT*(aT*a + par*d*d)*p = sT*s.
     *
     *     s is employed within lmpar and may be of separate interest.
     *
     *     Only a few iterations are generally needed for convergence
     *     of the algorithm. If, however, the limit of 10 iterations
     *     is reached, then the output par will contain the best
     *     value obtained so far.
     *
     *     parameters:
     *
     *	n is a positive integer input variable set to the order of r.
     *
     *	r is an n by n array. on input the full upper triangle
     *	  must contain the full upper triangle of the matrix r.
     *	  on OUTPUT the full upper triangle is unaltered, and the
     *	  strict lower triangle contains the strict upper triangle
     *	  (transposed) of the upper triangular matrix s.
     *
     *	ldr is a positive integer input variable not less than n
     *	  which specifies the leading dimension of the array r.
     *
     *	ipvt is an integer input array of length n which defines the
     *	  permutation matrix p such that a*p = q*r. column j of p
     *	  is column ipvt(j) of the identity matrix.
     *
     *	diag is an input array of length n which must contain the
     *	  diagonal elements of the matrix d.
     *
     *	qtb is an input array of length n which must contain the first
     *	  n elements of the vector (q transpose)*b.
     *
     *	delta is a positive input variable which specifies an upper
     *	  bound on the euclidean norm of d*x.
     *
     *	par is a nonnegative variable. on input par contains an
     *	  initial estimate of the levenberg-marquardt parameter.
     *	  on OUTPUT par contains the final estimate.
     *
     *	x is an OUTPUT array of length n which contains the least
     *	  squares solution of the system a*x = b, sqrt(par)*d*x = 0,
     *	  for the output par.
     *
     *	sdiag is an array of length n which contains the
     *	  diagonal elements of the upper triangular matrix s.
     *
     *	aux is a multi-purpose work array of length n.
     *
     *	xdi is a work array of length n. On OUTPUT: diag[j] * x[j].
     *
     */
        int i, iter, j, nsing;
        double dxnorm, fp, fp_old, gnorm, parc, parl, paru;
        double sum, temp;
        double p1 = 0.1;

    if (debugMessages) {
        Preferences.debug("lmpar\n", Preferences.DEBUG_ALGORITHM);
    }

    /*** lmpar: compute and store in x the gauss-newton direction. if the
         jacobian is rank-deficient, obtain a least squares solution. ***/

        nsing = n;
        for (j = 0; j < n; j++) {
    	aux[j] = qtb[j];
    	if (r[j * ldr + j] == 0 && nsing == n)
    	    nsing = j;
    	if (nsing < n)
    	    aux[j] = 0;
        }
    if (debugMessages) {
        Preferences.debug("nsing = " + nsing + "\n", Preferences.DEBUG_ALGORITHM);
    }
        for (j = nsing - 1; j >= 0; j--) {
    	aux[j] = aux[j] / r[j + ldr * j];
    	temp = aux[j];
    	for (i = 0; i < j; i++)
    	    aux[i] -= r[j * ldr + i] * temp;
        }

        for (j = 0; j < n; j++)
    	x[ipvt[j]] = aux[j];

    /*** lmpar: initialize the iteration counter, evaluate the function at the
         origin, and test for acceptance of the gauss-newton direction. ***/

        iter = 0;
        for (j = 0; j < n; j++)
    	xdi[j] = diag[j] * x[j];
        dxnorm = lm_enorm(n, xdi, 0);
        fp = dxnorm - delta;
        if (fp <= p1 * delta) {
    if (debugMessages) {
    	Preferences.debug("lmpar/ terminate (fp<p1*delta)\n", Preferences.DEBUG_ALGORITHM);
    }
    	par[0] = 0;
    	return;
        }

    /*** lmpar: if the jacobian is not rank deficient, the newton
         step provides a lower bound, parl, for the 0. of
         the function. otherwise set this bound to 0.. ***/

        parl = 0;
        if (nsing >= n) {
    	for (j = 0; j < n; j++)
    	    aux[j] = diag[ipvt[j]] * xdi[ipvt[j]] / dxnorm;

    	for (j = 0; j < n; j++) {
    	    sum = 0.;
    	    for (i = 0; i < j; i++)
    		sum += r[j * ldr + i] * aux[i];
    	    aux[j] = (aux[j] - sum) / r[j + ldr * j];
    	}
    	temp = lm_enorm(n, aux, 0);
    	parl = fp / delta / temp / temp;
        }

    /*** lmpar: calculate an upper bound, paru, for the 0. of the function. ***/

        for (j = 0; j < n; j++) {
    	sum = 0;
    	for (i = 0; i <= j; i++)
    	    sum += r[j * ldr + i] * qtb[i];
    	aux[j] = sum / diag[ipvt[j]];
        }
        gnorm = lm_enorm(n, aux, 0);
        paru = gnorm / delta;
        if (paru == 0.)
    	paru = LM_DWARF / Math.min(delta, p1);

    /*** lmpar: if the input par lies outside of the interval (parl,paru),
         set par to the closer endpoint. ***/

        par[0] = Math.max(par[0], parl);
        par[0] = Math.min(par[0], paru);
        if (par[0] == 0.)
    	par[0] = gnorm / dxnorm;
    if (debugMessages) {
        Preferences.debug("lmpar/ parl = " + parl + " par = " + par + " paru = " + paru + "\n",
        		Preferences.DEBUG_ALGORITHM);
    }

    /*** lmpar: iterate. ***/

        for (;; iter++) {

            /** evaluate the function at the current value of par. **/

    	if (par[0] == 0.)
    	    par[0] = Math.max(LM_DWARF, 0.001 * paru);
    	temp = Math.sqrt(par[0]);
    	for (j = 0; j < n; j++)
    	    aux[j] = temp * diag[j];

    	lm_qrsolv( n, r, ldr, ipvt, aux, qtb, x, sdiag, xdi );
            /* return values are r, x, sdiag */

    	for (j = 0; j < n; j++)
    	    xdi[j] = diag[j] * x[j]; /* used as output */
    	dxnorm = lm_enorm(n, xdi, 0);
    	fp_old = fp;
    	fp = dxnorm - delta;
            
            /** if the function is small enough, accept the current value
                of par. Also test for the exceptional cases where parl
                is zero or the number of iterations has reached 10. **/

    	if (Math.abs(fp) <= p1 * delta
    	    || (parl == 0. && fp <= fp_old && fp_old < 0.)
    	    || iter == 10)
    	    break; /* the only exit from the iteration. */
            
            /** compute the Newton correction. **/

    	for (j = 0; j < n; j++)
    	    aux[j] = diag[ipvt[j]] * xdi[ipvt[j]] / dxnorm;

    	for (j = 0; j < n; j++) {
    	    aux[j] = aux[j] / sdiag[j];
    	    for (i = j + 1; i < n; i++)
    		aux[i] -= r[j * ldr + i] * aux[j];
    	}
    	temp = lm_enorm(n, aux, 0);
    	parc = fp / delta / temp / temp;

            /** depending on the sign of the function, update parl or paru. **/

    	if (fp > 0)
    	    parl = Math.max(parl, par[0]);
    	else if (fp < 0)
    	    paru = Math.min(paru, par[0]);
    	/* the case fp==0 is precluded by the break condition  */
            
            /** compute an improved estimate for par. **/
            
    	par[0] = Math.max(parl, par[0] + parc);
            
        }

    } /*** lm_lmpar. ***/


    
    /*****************************************************************************/
    /*  lm_qrsolv (linear least-squares)                                         */
    /*****************************************************************************/

    private void lm_qrsolv(int n, double []r, int ldr, int []ipvt, double []diag,
    	       double[] qtb, double[] x, double[] sdiag, double[] wa)
    {
    /*
     *     Given an m by n matrix a, an n by n diagonal matrix d,
     *     and an m-vector b, the problem is to determine an x which
     *     solves the system
     *
     *	    a*x = b  and  d*x = 0
     *
     *     in the least squares sense.
     *
     *     This subroutine completes the solution of the problem
     *     if it is provided with the necessary information from the
     *     qr factorization, with column pivoting, of a. That is, if
     *     a*p = q*r, where p is a permutation matrix, q has orthogonal
     *     columns, and r is an upper triangular matrix with diagonal
     *     elements of nonincreasing magnitude, then qrsolv expects
     *     the full upper triangle of r, the permutation matrix p,
     *     and the first n components of (q transpose)*b. The system
     *     a*x = b, d*x = 0, is then equivalent to
     *
     *	    r*z = qT*b,  pT*d*p*z = 0,
     *
     *     where x = p*z. If this system does not have full rank,
     *     then a least squares solution is obtained. On output qrsolv
     *     also provides an upper triangular matrix s such that
     *
     *	    pT *(aT *a + d*d)*p = sT *s.
     *
     *     s is computed within qrsolv and may be of separate interest.
     *
     *     Parameters
     *
     *	n is a positive integer input variable set to the order of r.
     *
     *	r is an n by n array. On input the full upper triangle
     *	  must contain the full upper triangle of the matrix r.
     *	  On OUTPUT the full upper triangle is unaltered, and the
     *	  strict lower triangle contains the strict upper triangle
     *	  (transposed) of the upper triangular matrix s.
     *
     *	ldr is a positive integer input variable not less than n
     *	  which specifies the leading dimension of the array r.
     *
     *	ipvt is an integer input array of length n which defines the
     *	  permutation matrix p such that a*p = q*r. Column j of p
     *	  is column ipvt(j) of the identity matrix.
     *
     *	diag is an input array of length n which must contain the
     *	  diagonal elements of the matrix d.
     *
     *	qtb is an input array of length n which must contain the first
     *	  n elements of the vector (q transpose)*b.
     *
     *	x is an OUTPUT array of length n which contains the least
     *	  squares solution of the system a*x = b, d*x = 0.
     *
     *	sdiag is an OUTPUT array of length n which contains the
     *	  diagonal elements of the upper triangular matrix s.
     *
     *	wa is a work array of length n.
     *
     */
        int i, kk, j, k, nsing;
        double qtbpj, sum, temp;
        double _sin, _cos, _tan, _cot; /* local variables, not functions */

    /*** qrsolv: copy r and (q transpose)*b to preserve input and initialize s.
         in particular, save the diagonal elements of r in x. ***/

        for (j = 0; j < n; j++) {
    	for (i = j; i < n; i++)
    	    r[j * ldr + i] = r[i * ldr + j];
    	x[j] = r[j * ldr + j];
    	wa[j] = qtb[j];
        }
    if (debugMessages) {
        Preferences.debug("qrsolv\n", Preferences.DEBUG_ALGORITHM);
    }

    /*** qrsolv: eliminate the diagonal matrix d using a Givens rotation. ***/

        for (j = 0; j < n; j++) {

    /*** qrsolv: prepare the row of d to be eliminated, locating the
         diagonal element using p from the qr factorization. ***/

    	if (diag[ipvt[j]] != 0.) {
    	for (k = j; k < n; k++)
    	    sdiag[k] = 0.;
    	sdiag[j] = diag[ipvt[j]];

    /*** qrsolv: the transformations to eliminate the row of d modify only 
         a single element of qT*b beyond the first n, which is initially 0. ***/

    	qtbpj = 0.;
    	for (k = j; k < n; k++) {

                /** determine a Givens rotation which eliminates the
                    appropriate element in the current row of d. **/

    	    if (sdiag[k] == 0.)
    		continue;
    	    kk = k + ldr * k;
    	    if (Math.abs(r[kk]) < Math.abs(sdiag[k])) {
    		_cot = r[kk] / sdiag[k];
    		_sin = 1 / Math.sqrt(1 + (_cot*_cot));
    		_cos = _sin * _cot;
    	    } else {
    		_tan = sdiag[k] / r[kk];
    		_cos = 1 / Math.sqrt(1 + (_tan*_tan));
    		_sin = _cos * _tan;
    	    }

                /** compute the modified diagonal element of r and
                    the modified element of ((q transpose)*b,0). **/

    	    r[kk] = _cos * r[kk] + _sin * sdiag[k];
    	    temp = _cos * wa[k] + _sin * qtbpj;
    	    qtbpj = -_sin * wa[k] + _cos * qtbpj;
    	    wa[k] = temp;

                /** accumulate the tranformation in the row of s. **/

    	    for (i = k + 1; i < n; i++) {
    		temp = _cos * r[k * ldr + i] + _sin * sdiag[i];
    		sdiag[i] = -_sin * r[k * ldr + i] + _cos * sdiag[i];
    		r[k * ldr + i] = temp;
    	    }
    	} // for (k = j; k < n; k++)
    	} // if (diag[ipvt[j]] != 0.)

            /** store the diagonal element of s and restore
                the corresponding diagonal element of r. **/

    	sdiag[j] = r[j * ldr + j];
    	r[j * ldr + j] = x[j];
        } // for (j = 0; j < n; j++)

    /*** qrsolv: solve the triangular system for z. if the system is
         singular, then obtain a least squares solution. ***/

        nsing = n;
        for (j = 0; j < n; j++) {
    	if (sdiag[j] == 0. && nsing == n)
    	    nsing = j;
    	if (nsing < n)
    	    wa[j] = 0;
        }

        for (j = nsing - 1; j >= 0; j--) {
    	sum = 0;
    	for (i = j + 1; i < nsing; i++)
    	    sum += r[j * ldr + i] * wa[i];
    	wa[j] = (wa[j] - sum) / sdiag[j];
        }

    /*** qrsolv: permute the components of z back to components of x. ***/

        for (j = 0; j < n; j++)
    	x[ipvt[j]] = wa[j];

    } /*** lm_qrsolv. ***/

    
    /*****************************************************************************/
    /*  lm_enorm (Euclidean norm)                                                */
    /*****************************************************************************/

    double lm_enorm(int n, final double []x, int offset)
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
    	xabs = Math.abs(x[i+offset]);
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
    	int i;
    	double ymodel = 0.0;
        try {
            switch (testCase) {
            case DRAPER24D:
            	// evaluate the residuals[i] = ymodel[i] - ySeries[i]
                for (i = 0; i < m; i++) {
                    ymodel = par[0] - (par[1] * Math.pow(par[2], xSeries[i]));
                    fvec[i] = ymodel - ySeries[i];
                }
            	break;
            	
            case HOCK25:
            	// evaluate the residuals[i] = ymodel[i] - ySeries[i]
                for (i = 0; i < m; i++) {
                    ymodel = Math.pow((par[0] * Math.log(xSeries[i])),par[1]) + par[2];
                    fvec[i] = ymodel - ySeries[i];
                }
                break;
            case BARD:
            	 // evaluate the residuals[i] = ymodel[i] - ySeries[i]
                for (i = 0; i < m; i++) {
                    ymodel = par[0] + xSeries[i]/(par[1]*(16.0 - xSeries[i]) 
                    		 + par[2]*Math.min(xSeries[i], 16.0 - xSeries[i]));
                    fvec[i] = ymodel - ySeries[i];
                }
                break;
            case KOWALIK_AND_OSBORNE:
            	 // evaluate the residuals[i] = ymodel[i] - ySeries[i]
                for (i = 0; i < m; i++) {
                    ymodel = par[0]*(xSeries[i]*xSeries[i] + par[1]*xSeries[i])/
                            (xSeries[i]*xSeries[i] + par[2]*xSeries[i] + par[3]);
                    fvec[i] = ymodel - ySeries[i];
                }
                break;
            }
            	
        }
        catch (Exception e) {
            Preferences.debug("function error: " + e.getMessage() + "\n", Preferences.DEBUG_ALGORITHM);
        }

        return;
    }

    
}