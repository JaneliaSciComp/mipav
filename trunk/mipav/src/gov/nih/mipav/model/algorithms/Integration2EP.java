package gov.nih.mipav.model.algorithms;


import javax.help.plaf.basic.BasicFavoritesNavigatorUI.AddAction;

import gov.nih.mipav.view.*;
import gov.nih.mipav.DoubleDouble;


/**
 * This is a port of FORTRAN numerical integration routines in QUADPACK found at http://www.netlib.org/quadpack
 * Reference: R. Piessens, E. deDoncker-Kapenga, C. Uberhuber, D. Kahaner Quadpack: a Subroutine Package for Automatic
 * Integration Springer Verlag, 1983. Series in Computational Mathematics v. 1
 * The original dqage, dqagie, dqagpe, and dqagse routines were written by Robert Piessens and Elise de Doncker.
 * The original dqng routine was written by Robert Piessens and Elise de Doncker and modified by David Kahaner.
 * The original dqelg, dqk15, dqk15i, dqk21, dqk31, dqk41, dqk51, dqk61, and dqpsrt routines were written
 * by Robert Piessens and Elise de Doncker.
 * Self tests 1 to 15 come from quadpack_prb.f90 by John Burkardt.
 * The names of the copyright holders or contributors may not be used to endorse
 * or promote any derived software without prior permission;
 * The Quadpack software is provided "as is", without warranties;
 * Quadpack and the authors deny any liability for situations resulting from the use of this software.

 * Porting was performed by William Gandler.
 *
 * <p>Title:</p>
 *
 * <p>Description:</p>
 *
 * <p>Copyright: Copyright (c) 2003</p>
 *
 * <p>Company:</p>
 *
 * @author   not attributable
 * @version  1.0
 */

// Example code is below:
/*private void runIntegrationTest2() {
      IntModel2 imod;
      double lower = 0.0;
      double upper = 1.0;
      int routine = Integration2.DQAGPE;
      double breakPoints[] = new double[4];
      breakPoints[0] = 1.0/7.0;
      breakPoints[1] = 2.0/3.0;
      double epsabs = 0.0;
      double epsrel = 1.0E-3;
      int limit = 100;
      double numInt;
      int errorStatus;
      double absError;
      int neval;
      imod = new IntModel2(lower, upper, routine, breakPoints, epsabs, epsrel,
                           limit);
      imod.driver();
      numInt = imod.getIntegral();
      errorStatus = imod.getErrorStatus();
      absError = imod.getAbserr();
      neval = imod.getNeval();
      Preferences.debug("Numerical Integral = " + numInt + " after " + neval +
                        " integrand evaluations used\n");
      Preferences.debug("Error status = " + errorStatus +
                        " with absolute error = " + absError + "\n");
    }*/

/*class IntModel2 extends Integration2 {
      public IntModel2(double lower, double upper, int routine,
                       double breakPoints[], double epsabs,
                       double epsrel, int limit) {
        super(lower, upper, routine, breakPoints, epsabs, epsrel, limit);
      }

      public double intFunc(double x) {
        double function = 0.0;
        if ((x != 1.0/7.0) && (x != 2.0/3.0)) {
          function = Math.pow(Math.abs(x - 1.0/7.0), -0.25) *
                     Math.pow(Math.abs(x - 2.0/3.0), -0.55);
        }
        return function;
      }

      public void driver() {
        super.driver();
      }
    }*/

/*private void runIntegrationTest2() {
      IntModel2 imod;
      double boun = 0.0;
      int routine = Integration2.DQAGIE;
      int inf = 1;
      double epsabs = 0.0;
      double epsrel = 1.0E-3;
      int limit = 100;
      double numInt;
      int errorStatus;
      double absError;
      int neval;
      imod = new IntModel2(boun, routine, inf, epsabs, epsrel, limit);
      imod.driver();
      numInt = imod.getIntegral();
      errorStatus = imod.getErrorStatus();
      absError = imod.getAbserr();
      neval = imod.getNeval();
      Preferences.debug("Numerical Integral = " + numInt + " after " + neval +
                        " integrand evaluations used\n");
      Preferences.debug("Error status = " + errorStatus +
                        " with absolute error = " + absError + "\n");
    }*/

/*class IntModel2 extends Integration2 {
      public IntModel2(double boun, int routine, int inf,
                       double epsabs, double epsrel, int limit) {
        super(boun, routine, inf, epsabs, epsrel, limit);
      }

      public double intFunc(double x) {
        double function = 0.0;
        if (x > 0.0) {
          function = Math.sqrt(x) * Math.log(x)/((x + 1.0) * (x + 2.0));
        }
        return function;
      }

      public void driver() {
        super.driver();
      }
    }*/


public abstract class Integration2EP {

    //~ Static fields/initializers -------------------------------------------------------------------------------------

    /** Integral over (lower, upper) with user specified break points. */
    protected static final int DQAGPE = 1;

    /** Integral over (bound, +infinity), (-infinity, bound), or (-infinity, +infinity). */
    protected static final int DQAGIE = 2;

    /** Integral over (lower, upper) which can handle end point singularities. */
    protected static final int DQAGSE = 3;

    /** Integral without singularities, discontinuities, or infinite bounds. */
    protected static final int DQAGE = 4;

    /** Non-adaptive integration. */
    protected static final int DQNG = 5;
    
    /** Gauss-Kronod quadrature rule */
    protected static final int DQK15 = 6;
    
    /** Gauss-Kronod quadrature rule */
    protected static final int DQK21 = 7;
    
    /** Gauss-Kronod quadrature rule */
    protected static final int DQK31 = 8;
    
    /** Gauss-Kronod quadrature rule */
    protected static final int DQK41 = 9;
    
    /** Gauss-Kronod quadrature rule */
    protected static final int DQK51 = 10;
    
    /** Gauss-Kronod quadrature rule */
    protected static final int DQK61 = 11;


    //~ Instance fields ------------------------------------------------------------------------------------------------

    /** Estimate of the absolute value of the error, which should equal or exceed abs(actual integral - result). */
    private DoubleDouble[] abserr = new DoubleDouble[1];

    /** The left end points of the subintervals in the partition of the given integration range (lower, upper). */
    private DoubleDouble[] alist;

    /** The right end points of the subintervals in the partition of the given integration range (lower, upper). */
    private DoubleDouble[] blist;

    /** finite bound of integration range used in dqagie (has no meaning if interval is doubly-infinite). */
    private DoubleDouble bound;

    /**
     * Used in dqagpe This array must always be >= 2 in length. The first breakPoints.length - 2 points are user
     * provided break points. If these points are not in an ascending sequence, they will automatically be sorted.
     */
    private DoubleDouble[] breakPoints;

    /** Estimates of the absolute errors on the subintervals. */
    private DoubleDouble[] elist;

    /**
     * epmach = D1MACH(4) Machine epmach is the smallest positive epmach such that (1.0 + epmach) != 1.0. epmach = 2**(1
     * - doubleDigits) = 2**(1 - 53) = 2**(-52) epmach = 2.224460e-16 epmach is called the largest relative spacing
     * for doubles
     * For DoubleDouble useEPS = 1.23259516440783e-32= 2^-106
     */
    private DoubleDouble epmach;

    /**
     * If epsabs <= 0.0 and epsrel < Math.max(50*relative machine accuracy, 5.0E-29), the routine will exit with an
     * error message.
     */
    private DoubleDouble epsabs;

    /** DOCUMENT ME! */
    private DoubleDouble epsrel;

    /**
     * errorStatus = 0 for normal and reliable termination of the routine. It is assumed that the requested accuracy has
     * been achieved. errorStatus > 0 for abnormal termination of the routine. The estimates for the integral and error
     * are less reliable. It is assumed that the requested accuracy has not been achieved. errorStatus = 1 maximum
     * number of subdivisions allowed has been achieved. One can allow more subdivisions by increasing the value of
     * limit (and taking the according dimension adjustments into account). However, if this yields no improvement, it
     * is advised to analyze the integrand in order to determine the integration difficulties. If the position of a
     * local difficulty can be determined( i.e. singularity, discontinuity within the interval), it should be supplied
     * to the routine as an element of the breakPoints array. If necessary, an appropriate special purpose integrator
     * must be used, which is designed for handling the type of difficulty involved. errorStatus = 2 The occurrence of
     * roundoff error is detected, which prevents the requested tolerance from being achieved. The error may be
     * under-estimated. errorStatus = 3 Extremely bad integrand behavior occurs at some points of the integration
     * interval. errorStatus = 4 The algorithm does not converge. Roundoff error is detected in the extrapolation table.
     * It is presumed that the requested tolerance cannot be achieved, and that the returned result is the best that can
     * be obtained. errorStatus = 5 The integral is probably divergent, or slowly convergent. It must be noted that
     * divergence can occur with any other value of errorStatus > 0. errorStatus = 6 The input is invalid because
     * (epsabs <= 0 and epsrel < max(50.0 * relative machine accuracy, 5.0E-29) or in the case of dqagpe can also happen
     * because npts2 < 2, the breakPoints are specified outside the integration range, or , or limit <= npts.
     */
    private int errorStatus;

    /**
     * In dqagie indicates the kind of integration range involved inf = 1 corresponds to (bound, +infinity) inf = -1
     * corresponds to (-infinity, bound) inf = 2 corresponds to (-infinity, +infinity).
     */
    private int inf;

    /**
     * The first k elements of iord are pointers to the error estimates over the subintervals, such that elist[iord[0]],
     * ..., elist[iord[k-1]] form a decreasing sequence, with k = last if last <= (limit/2 + 2) , and k = limit + 1 -
     * last otheriwse.
     */
    private int[] iord;

    /**
     * In dqage key selects the local integration rule A gauss-kronrod pair is used with 7 - 15 points if key < 2 10 -
     * 21 points if key = 2 15 - 31 points if key = 3 20 - 41 points if key = 4 25 - 51 points if key = 5 30 - 61 points
     * if key > 5.
     */
    private int key;

    /** The number of subintervals actually called in the subdivision process. */
    private int last;

    /**
     * level contains the subdivision levels of the subinterval, i.e., if (aa, bb) is a subinterval of (p1, p2) where p1
     * as well as p2 is a user-provided break point or integration limit, then (aa, bb) has level 1 if abs(bb - aa) =
     * abs(p2 - p1) * 2**(-1).
     */
    private int[] level;

    /**
     * Gives an upper bound on the number of subintervals in the partition of lower, upper. In dqagpe limit must be >=
     * npts2. If limit < npts2 in dqagpe, the routine exits with an error message.
     */
    private int limit;

    /** DOCUMENT ME! */
    private DoubleDouble lower;

    /**
     * After the first integration over the intervals (pts[i], pts[i+1]), i = 0, 1, ..., npts2 - 2, the error estimates
     * over some of the intervals may have been increased artificially, in order to put their subdivision forward. If
     * this happens for the subinterval numbered k, ndin[k] is put to 1, otherwise ndin[k] = 0.
     */
    private int[] ndin;

    /** Number of integrand evaluations. */
    private int neval;

    /** npts = npts2 - 2. */
    private int npts;

    /** npts2 = breakPoints.length. */
    private int npts2;

    /** D1MACH(2) = 2**(emax)*(1 - 2**(-doubleDigits)) = 2**1024*(1 - 2**-53) D1MACH(2) = Double.MAX_VALUE. */
    private DoubleDouble oflow = DoubleDouble.valueOf(Double.MAX_VALUE);

    /** Integration limits and the break points of the interval in ascending sequence. */
    private DoubleDouble[] pts;
    
    private DoubleDouble[] resabs = new DoubleDouble[1];
    
    private DoubleDouble[] resasc = new DoubleDouble[1];

    /** Approximation to the integral. */
    private DoubleDouble[] result = new DoubleDouble[1];

    /** The integral approximations of the subintervals. */
    private DoubleDouble[] rlist;

    /** DOCUMENT ME! */
    private int routine;

    /** 
     * For doubles uflow = 2**-1022, but a double has 53 bits of precision while
     * a DoubleDouble has 106 bits of precision, so uflow = 2**-(1022 - 53) = 2**-969
     */
    private DoubleDouble uflow = DoubleDouble.valueOf(2.0).pow(-969.0);

    /** DOCUMENT ME! */
    private DoubleDouble upper;
    
    private boolean selfTest = false;
    
    private int testCase = 1;
    
    private DoubleDouble actualAnswer;

    //~ Constructors ---------------------------------------------------------------------------------------------------
    
    /**
     * Constructor for running self tests.
     */
    public Integration2EP() {
    	int i;
    	//DoubleDouble neweps;
    	DoubleDouble tol;
        selfTest = true;
        
        //epmach = DoubleDouble.valueOf(1.0);
        //neweps = DoubleDouble.valueOf(1.0);

        //while (true) {
        	// Obtain x.hi = 2.0 x.lo = 0
        	// x.hi = 1.5 x.lo = 0 
        	// down to
        	// x.hi = 1.0000000000000002 x.lo = 0.0
        	// x.hi = 1.0 x.lo =  1.1102230246251565E-16
        	// down to 
        	// x.hi = 1.0 x.lo = 4.9E-324
        	//DoubleDouble x = (DoubleDouble.valueOf(1.0)).add(neweps);
        	//Preferences.debug("x.hi = " + x.hi + "\n");
        	//Preferences.debug("x.lo = " + x.lo + "\n");
            //if ((DoubleDouble.valueOf(1.0)).equals((DoubleDouble.valueOf(1.0)).add(neweps))) {
               // break;
            //} else {
                //epmach = neweps;
                //neweps = neweps.divide(DoubleDouble.valueOf(2.0));
            //}
        //}
        // DoubleDouble.EPS is the smallest representable relative difference between two {link @ DoubleDouble} values
        // EPS = 1.23259516440783e-32;  /* = 2^-106 */
        epmach = DoubleDouble.valueOf(DoubleDouble.EPS);

        tol = ((DoubleDouble.valueOf(50.0)).multiply(epmach)).max(DoubleDouble.valueOf(5.0E-29));
        
        testCase = 1;
        // dqage is an adaptive automatic integrator using a Gauss-Kronod rule.
        // Integrate cos(100*sin(x)) from 0 to PI.
        // The exact answer is PI * j0(100), or roughly 0.06278740.
        // key chooses the order of the integration rule from 1 to 6.
        // Numerical Integral = 0.062787400491492629703018938311878 after 2623 integrand evaluations used
        // Error status = 2 with estimated absolute error = 5.475357990914704230361215237912E-22
        // Actual answer = 0.062787400491492672011578833637816
        // Exact error = -4.2308559895325939633446582822463E-17
        double realArg = 100.0;
    	double imaginaryArg = 0.0;
    	double initialOrder = 0.0;
    	int sequenceNumber = 1; // Number of sequential Bessel function orders calculated
    	double realResult[] = new double[1];
    	double imagResult[] = new double[1];
    	int[] nz = new int[1]; // number of components set to zero due to underflow
        int[] errorFlag = new int[1]; // zero if no error
        
    	Bessel bes = new Bessel(Bessel.BESSEL_J, realArg, imaginaryArg, initialOrder, Bessel.UNSCALED_FUNCTION,
                sequenceNumber, realResult, imagResult, nz, errorFlag);
    	bes.run();
    	if (errorFlag[0] != 0) {
    	    Preferences.debug("Bessel_J error for realArg = " + realArg + "\n");
    	}
    	Preferences.debug("j0(100.0) = " + realResult[0] + "\n");
    	
        lower = DoubleDouble.valueOf(0.0);
        upper = DoubleDouble.PI;
        routine = Integration2EP.DQAGE;
        key = 6;
        epsabs = DoubleDouble.valueOf(0.0);
        epsrel = DoubleDouble.valueOf(1.0E-28);
        actualAnswer = (DoubleDouble.valueOf(realResult[0])).multiply(DoubleDouble.PI);
        limit = 1000;
        
        if ((epsabs.le(DoubleDouble.valueOf(0.0))) && (epsrel.lt(tol))) {
            MipavUtil.displayError("Must set epsabs > 0.0 or must set epsrel >= " + tol);
            errorStatus = 6;

            return;
        }
        
        if (limit <= 1) {
            MipavUtil.displayError("limit must be >= 1");
            errorStatus = 6;

            return;
        }
        
        driver();
        
        Preferences.debug("Test 1 testing dqage\n");
        Preferences.debug("Integrand is cos(100.0*sin(x))\n");
        Preferences.debug("Integrand lower endpoint = 0.0\n");
        Preferences.debug("Integrand upper endpoint = PI\n");
        Preferences.debug("Numerical Integral = " + result[0] + " after " + neval +
        " integrand evaluations used\n");
        Preferences.debug("Error status = " + errorStatus +
        " with estimated absolute error = " + abserr[0] + "\n");
        Preferences.debug("Actual answer = " + actualAnswer + "\n");
        Preferences.debug("Exact error = " + (result[0].subtract(actualAnswer)) + "\n");
        
        testCase = 2;
        // Test2 tests dqagie.
        // dqagie is an adaptive quadrature routine for infinite intervals.
        // Integrate log(x)/(1 + 100*x*x) from 0 to infinity
        // The exact answer is -PI*log(10)/20 = -0.3616892
        // Give the type of infinity
        // inf = 1 means bound to infinity
        // inf = -1 means -infinity to bound
        // inf = 2 means -infinity to infinity
        // Numerical Integral = -0.36168922062077324571528626847539 after 6285 integrand evaluations used
        // Error status = 2 with estimated absolute error = 2.7214832372768430856227469064293E-21
        // Actual answer = -0.36168922062077324062450232751309
        // Exact error = -5.0907839409623050673220957332826E-18
        bound = DoubleDouble.valueOf(0.0);
        inf = 1;
        routine = Integration2EP.DQAGIE;
        epsabs = DoubleDouble.valueOf(0.0);
        epsrel = DoubleDouble.valueOf(1.0E-28);
        limit = 1000;
        actualAnswer = (((DoubleDouble.PI).negate()).multiply((DoubleDouble.valueOf(10.0)).log())).divide(DoubleDouble.valueOf(20.0));
        
        if ((epsabs.le(DoubleDouble.valueOf(0.0))) && (epsrel.lt(tol))) {
            MipavUtil.displayError("Must set epsabs > 0.0 or must set epsrel >= " + tol);
            errorStatus = 6;

            return;
        }
        
        if (limit <= 1) {
            MipavUtil.displayError("limit must be >= 1");
            errorStatus = 6;

            return;
        }
        
        driver();
        
        Preferences.debug("Test 2 testing dqagie\n");
        Preferences.debug("Integrand is log(x)/(1 + 100*x*x)\n");
        Preferences.debug("Integrand lower endpoint = 0.0\n");
        Preferences.debug("Integrand upper endpoint = infinity\n");
        Preferences.debug("Numerical Integral = " + result[0] + " after " + neval +
        " integrand evaluations used\n");
        Preferences.debug("Error status = " + errorStatus +
        " with estimated absolute error = " + abserr[0] + "\n");
        Preferences.debug("Actual answer = " + actualAnswer + "\n");
        Preferences.debug("Exact error = " + (result[0].subtract(actualAnswer)) + "\n");
        
        testCase = 3;
        // Test3 tests dqagpe
        // dqagpe is an adaptive integrator that can handle singularities of the
        // integrand at user specified points.
        // Integrate x**3 * log(abs((x*x-1)*(x*x-2))) from 0 to 3.
        // The exact answer is 61*log(2)+77*log(7)/4 - 27.
        // Numerical Integral = 52.740748383471444913493269262508 after 11907 integrand evaluations used
        // Error status = 2 with estimated absolute error = 5.686261292209667242705102933715E-20
        // Actual answer = 52.74074838347144499772919972023
        // Exact error = -8.4235930457721460477450425680551E-17
        lower = DoubleDouble.valueOf(0.0);
        upper = DoubleDouble.valueOf(3.0);
        routine = Integration2EP.DQAGPE;
        breakPoints = new DoubleDouble[4];
        breakPoints[0] = DoubleDouble.valueOf(1.0);
        breakPoints[1] = (DoubleDouble.valueOf(2.0)).sqrt();
        epsabs = DoubleDouble.valueOf(0.0);
        epsrel = DoubleDouble.valueOf(1.0E-28);
        limit = 1000;
        npts2 = breakPoints.length;
        npts = npts2 - 2;
        actualAnswer = (((DoubleDouble.valueOf(61.0)).multiply((DoubleDouble.valueOf(2.0)).log())).add
        ((DoubleDouble.valueOf(77.0)).multiply(((DoubleDouble.valueOf(7.0)).log())).divide(DoubleDouble.valueOf(4.0)))).subtract(DoubleDouble.valueOf(27.0));
        
        if (npts2 < 2) {
            MipavUtil.displayError("breakPoints array length must be >= 2");
            errorStatus = 6;

            return;
        }

        for (i = 0; i < (npts2 - 2); i++) {

            if (breakPoints[i].lt(lower)) {
                MipavUtil.displayError("Break point " + breakPoints[i] + " is impossibly < lower");
                errorStatus = 6;

                return;
            }

            if (breakPoints[i].gt(upper)) {
                MipavUtil.displayError("Break point " + breakPoints[i] + " is impossibly > upper");
                errorStatus = 6;

                return;
            }
        } // for (int i = 0; i < npts2 - 2; i++)
        
        if ((epsabs.le(DoubleDouble.valueOf(0.0))) && (epsrel.lt(tol))) {
            MipavUtil.displayError("Must set epsabs > 0.0 or must set epsrel >= " + tol);
            errorStatus = 6;

            return;
        }
        
        if (limit <= npts) {
            MipavUtil.displayError("Must set limit = " + limit + " > " + npts);
            errorStatus = 6;

            return;
        }
        
        driver();
        
        Preferences.debug("Test 3 testing dqagpe\n");
        Preferences.debug("Integrand is x**3 * log(abs((x*x-1)*(x*x-2)))\n");
        Preferences.debug("Integrand lower endpoint = 0.0\n");
        Preferences.debug("Integrand upper endpoint = 3.0\n");
        Preferences.debug("Numerical Integral = " + result[0] + " after " + neval +
        " integrand evaluations used\n");
        Preferences.debug("Error status = " + errorStatus +
        " with estimated absolute error = " + abserr[0] + "\n");
        Preferences.debug("Actual answer = " + actualAnswer + "\n");
        Preferences.debug("Exact error = " + (result[0].subtract(actualAnswer)) + "\n");
        
        testCase = 4;
        // Test4 tests dqagse
        // dqagse is an adaptive integrator for endpoint singularities.
        // Integrate log(x)/sqrt(x) from 0 to 1.
        // The exact answer is -4.
        // Numerical Integral = -4.00000000000000000 after 8127 integrand evaluations used
        // Error status = 2 with estimated absolute error = 2.3876831159953403966422329946169E-21
        // Actual answer = -4.0
        // Exact error = 6.1021832024198197007231242132039E-18
        lower = DoubleDouble.valueOf(0.0);
        upper = DoubleDouble.valueOf(1.0);
        routine = Integration2EP.DQAGSE;
        epsabs = DoubleDouble.valueOf(0.0);
        epsrel = DoubleDouble.valueOf(1.0E-28);
        limit = 1000;
        actualAnswer = DoubleDouble.valueOf(-4.0);
        
        if ((epsabs.le(DoubleDouble.valueOf(0.0))) && (epsrel.lt(tol))) {
            MipavUtil.displayError("Must set epsabs > 0.0 or must set epsrel >= " + tol);
            errorStatus = 6;

            return;
        }

        if (limit < 1) {
            MipavUtil.displayError("limit must be >= 1");
            errorStatus = 6;

            return;
        }
        
        driver();
        
        Preferences.debug("Test 4 testing dqagse\n");
        Preferences.debug("Integrand is log(x)/sqrt(x)\n");
        Preferences.debug("Integrand lower endpoint = 0.0\n");
        Preferences.debug("Integrand upper endpoint = 1.0\n");
        Preferences.debug("Numerical Integral = " + result[0] + " after " + neval +
        " integrand evaluations used\n");
        Preferences.debug("Error status = " + errorStatus +
        " with estimated absolute error = " + abserr[0] + "\n");
        Preferences.debug("Actual answer = " + actualAnswer + "\n");
        Preferences.debug("Exact error = " + (result[0].subtract(actualAnswer)) + "\n");
        
        testCase = 9;
        // test9 tests dqk15
        // dqk15 is a Gauss-Kronod quadrature rule.
        lower = DoubleDouble.valueOf(0.0);
        upper = DoubleDouble.valueOf(1.0);
        routine = Integration2EP.DQK15;
        actualAnswer = (DoubleDouble.valueOf(-4.0)).divide(DoubleDouble.valueOf(9.0));
        // Numerical Integral = -0.44453824758327635083418354279228
        // Estimated absolute error = 0.2017677811047011722019991551744
        // Actual answer = -0.44444444444444444444444444444444
        // Exact error = -9.3803138831906389739098347836056E-5
        // resabs[0] = 0.44453824758327635083418354279228
        // resasc[0] = 0.2017677811047011722019991551744
        
        driver();
        
        Preferences.debug("Test 9 testing dqk15\n");
        Preferences.debug("Integrand is sqrt(x) * log(x)\n");
        Preferences.debug("Integrand lower endpoint = 0.0\n");
        Preferences.debug("Integrand upper endpoint = 1.0\n");
        Preferences.debug("Numerical Integral = " + result[0]+ "\n");
        Preferences.debug("Estimated absolute error = " + abserr[0] + "\n");
        Preferences.debug("Actual answer = " + actualAnswer + "\n");
        Preferences.debug("Exact error = " + (result[0].subtract(actualAnswer)) + "\n");
        Preferences.debug("resabs[0] = " + resabs[0] + "\n");
        Preferences.debug("resasc[0] = " + resasc[0] + "\n");
        
        testCase = 10;
        // test10 tests dqk21
        // dqk21 is a Gauss-Kronod quadrature rule.
        lower = DoubleDouble.valueOf(0.0);
        upper = DoubleDouble.valueOf(1.0);
        routine = Integration2EP.DQK21;
        actualAnswer = (DoubleDouble.valueOf(-4.0)).divide(DoubleDouble.valueOf(9.0));
        // Numerical Integral = -0.44448120174773075854749425655714
        // Estimated absolute error = 0.062137345692400246001066751930368
        // Actual answer = -0.44444444444444444444444444444444
        // Exact error = -3.6757303286314103049812112702108E-5
        // resabs[0] = 0.44448120174773075854749425655714
        // resasc[0] = 0.2010203179967191344102455883178
        
        driver();
        
        Preferences.debug("Test 10 testing dqk21\n");
        Preferences.debug("Integrand is sqrt(x) * log(x)\n");
        Preferences.debug("Integrand lower endpoint = 0.0\n");
        Preferences.debug("Integrand upper endpoint = 1.0\n");
        Preferences.debug("Numerical Integral = " + result[0]+ "\n");
        Preferences.debug("Estimated absolute error = " + abserr[0] + "\n");
        Preferences.debug("Actual answer = " + actualAnswer + "\n");
        Preferences.debug("Exact error = " + (result[0].subtract(actualAnswer)) + "\n");
        Preferences.debug("resabs[0] = " + resabs[0] + "\n");
        Preferences.debug("resasc[0] = " + resasc[0] + "\n");
        
        testCase = 11;
        // test11 tests dqk31
        // dqk31 is a Gauss-Kronod quadrature rule.
        lower = DoubleDouble.valueOf(0.0);
        upper = DoubleDouble.valueOf(1.0);
        routine = Integration2EP.DQK31;
        actualAnswer = (DoubleDouble.valueOf(-4.0)).divide(DoubleDouble.valueOf(9.0));
        // Numerical Integral = -0.44445711423810116344071890372747
        // Estimated absolute error = 0.013135187473737603241003449749653
        // Actual answer = -0.44444444444444444444444444444444
        // Exact error = -1.2669793656718996274459283032989E-5
        // resabs[0] = 0.44445711423810116344071890372747
        // resasc[0] = 0.20044662305437476729677475163959
        
        driver();
        
        Preferences.debug("Test 11 testing dqk31\n");
        Preferences.debug("Integrand is sqrt(x) * log(x)\n");
        Preferences.debug("Integrand lower endpoint = 0.0\n");
        Preferences.debug("Integrand upper endpoint = 1.0\n");
        Preferences.debug("Numerical Integral = " + result[0]+ "\n");
        Preferences.debug("Estimated absolute error = " + abserr[0] + "\n");
        Preferences.debug("Actual answer = " + actualAnswer + "\n");
        Preferences.debug("Exact error = " + (result[0].subtract(actualAnswer)) + "\n");
        Preferences.debug("resabs[0] = " + resabs[0] + "\n");
        Preferences.debug("resasc[0] = " + resasc[0] + "\n");
        
        testCase = 12;
        // test12 tests dqk41
        // dqk41 is a Gauss-Kronod quadrature rule.
        lower = DoubleDouble.valueOf(0.0);
        upper = DoubleDouble.valueOf(1.0);
        routine = Integration2EP.DQK41;
        actualAnswer = (DoubleDouble.valueOf(-4.0)).divide(DoubleDouble.valueOf(9.0));
        // Numerical Integral = -0.4444502553565425529317134939924
        // Estimated absolute error = 0.0042429656788425900820197450546617
        // Actual answer = -0.44444444444444444444444444444444
        // Exact error = -5.8109120981084872690495479589851E-6
        // resabs[0] = 0.4444502553565425529317134939924
        // resasc[0] = 0.20065010692415911355429312311755
        
        driver();
        
        Preferences.debug("Test 12 testing dqk41\n");
        Preferences.debug("Integrand is sqrt(x) * log(x)\n");
        Preferences.debug("Integrand lower endpoint = 0.0\n");
        Preferences.debug("Integrand upper endpoint = 1.0\n");
        Preferences.debug("Numerical Integral = " + result[0]+ "\n");
        Preferences.debug("Estimated absolute error = " + abserr[0] + "\n");
        Preferences.debug("Actual answer = " + actualAnswer + "\n");
        Preferences.debug("Exact error = " + (result[0].subtract(actualAnswer)) + "\n");
        Preferences.debug("resabs[0] = " + resabs[0] + "\n");
        Preferences.debug("resasc[0] = " + resasc[0] + "\n");
        
        testCase = 13;
        // test13 tests dqk51
        // dqk51 is a Gauss-Kronod quadrature rule.
        lower = DoubleDouble.valueOf(0.0);
        upper = DoubleDouble.valueOf(1.0);
        routine = Integration2EP.DQK51;
        actualAnswer = (DoubleDouble.valueOf(-4.0)).divide(DoubleDouble.valueOf(9.0));
        // Numerical Integral = -0.44444761693182600668948609006176
        // Estimated absolute error = 0.0017429443550292554005765269872151
        // Actual answer = -0.44444444444444444444444444444444
        // Exact error = -3.1724873815622450416456173276073E-6
        // resabs[0] = 0.44444761693182600668948609006176
        // resasc[0] = 0.20079987986805535202107362923376
        
        driver();
        
        Preferences.debug("Test 13 testing dqk51\n");
        Preferences.debug("Integrand is sqrt(x) * log(x)\n");
        Preferences.debug("Integrand lower endpoint = 0.0\n");
        Preferences.debug("Integrand upper endpoint = 1.0\n");
        Preferences.debug("Numerical Integral = " + result[0]+ "\n");
        Preferences.debug("Estimated absolute error = " + abserr[0] + "\n");
        Preferences.debug("Actual answer = " + actualAnswer + "\n");
        Preferences.debug("Exact error = " + (result[0].subtract(actualAnswer)) + "\n");
        Preferences.debug("resabs[0] = " + resabs[0] + "\n");
        Preferences.debug("resasc[0] = " + resasc[0] + "\n");
        
        testCase = 14;
        // test14 tests dqk61
        // dqk61 is a Gauss-Kronod quadrature rule.
        lower = DoubleDouble.valueOf(0.0);
        upper = DoubleDouble.valueOf(1.0);
        routine = Integration2EP.DQK61;
        actualAnswer = (DoubleDouble.valueOf(-4.0)).divide(DoubleDouble.valueOf(9.0));
        // Numerical Integral = -0.44444636518933710849952792078239
        // Estimated absolute error = 8.3764739340422666930861763803848E-4
        // Actual answer = -0.44444444444444444444444444444444
        // Exact error = -1.9207448926640550834763379470697E-6
        // resabs[0] = 0.44444636518933710849952792078239
        // resasc[0] = 0.20063345752687790135346389270898
        
        driver();
        
        Preferences.debug("Test 14 testing dqk61\n");
        Preferences.debug("Integrand is sqrt(x) * log(x)\n");
        Preferences.debug("Integrand lower endpoint = 0.0\n");
        Preferences.debug("Integrand upper endpoint = 1.0\n");
        Preferences.debug("Numerical Integral = " + result[0]+ "\n");
        Preferences.debug("Estimated absolute error = " + abserr[0] + "\n");
        Preferences.debug("Actual answer = " + actualAnswer + "\n");
        Preferences.debug("Exact error = " + (result[0].subtract(actualAnswer)) + "\n");
        Preferences.debug("resabs[0] = " + resabs[0] + "\n");
        Preferences.debug("resasc[0] = " + resasc[0] + "\n");
        
        testCase = 15;
        // Test15 tests dqng
        // dqng is a nonadaptive automatic integrator using a Gauss-Kronod
        // Patterson rule.
        // Integrate sqrt(x) * log(x) from 0 to 1.
        // The exact answer is -4.0/9.0.
        // Numerical Integral = -0.44444458538420454910066622043601 after 87 integrand evaluations used
        // Error status = 1 with estimated absolute error = 2.1889792398786871280557597028752E-5
        // Actual answer = -0.44444444444444444444444444444444
        // Exact error = -1.4093976010465622177599158059837E-7
        lower = DoubleDouble.valueOf(0.0);
        upper = DoubleDouble.valueOf(1.0);
        routine = Integration2EP.DQNG;
        epsabs = DoubleDouble.valueOf(0.0);
        epsrel = DoubleDouble.valueOf(1.0E-28);
        actualAnswer = (DoubleDouble.valueOf(-4.0)).divide(DoubleDouble.valueOf(9.0));
        
        if ((epsabs.le(DoubleDouble.valueOf(0.0))) && (epsrel.lt(tol))) {
            MipavUtil.displayError("Must set epsabs > 0.0 or must set epsrel >= " + tol);
            errorStatus = 6;

            return;
        }
        
        driver();
        
        Preferences.debug("Test 15 testing dqng\n");
        Preferences.debug("Integrand is sqrt(x) * log(x)\n");
        Preferences.debug("Integrand lower endpoint = 0.0\n");
        Preferences.debug("Integrand upper endpoint = 1.0\n");
        Preferences.debug("Numerical Integral = " + result[0] + " after " + neval +
        " integrand evaluations used\n");
        Preferences.debug("Error status = " + errorStatus +
        " with estimated absolute error = " + abserr[0] + "\n");
        Preferences.debug("Actual answer = " + actualAnswer + "\n");
        Preferences.debug("Exact error = " + (result[0].subtract(actualAnswer)) + "\n");
        
        testCase = 101;
        lower = DoubleDouble.valueOf(0.0);
        upper = DoubleDouble.valueOf(1.0);
        routine = Integration2EP.DQAGPE;
        breakPoints = new DoubleDouble[4];
        breakPoints[0] = (DoubleDouble.valueOf(1.0)).divide(DoubleDouble.valueOf(7.0));
        breakPoints[1] = (DoubleDouble.valueOf(2.0)).divide(DoubleDouble.valueOf(3.0));
        epsabs = DoubleDouble.valueOf(0.0);
        //epsrel = DoubleDouble.valueOf(1.0E-3);
        epsrel = DoubleDouble.valueOf(1.0E-28);
        //limit = 100;
        limit = 1000;
        
        npts2 = breakPoints.length;
        npts = npts2 - 2;

        if (npts2 < 2) {
            MipavUtil.displayError("breakPoints array length must be >= 2");
            errorStatus = 6;

            return;
        }

        for (i = 0; i < (npts2 - 2); i++) {

            if (breakPoints[i].lt(lower)) {
                MipavUtil.displayError("Break point " + breakPoints[i] + " is impossibly < lower");
                errorStatus = 6;

                return;
            }

            if (breakPoints[i].gt(upper)) {
                MipavUtil.displayError("Break point " + breakPoints[i] + " is impossibly > upper");
                errorStatus = 6;

                return;
            }
        } // for (int i = 0; i < npts2 - 2; i++)
        
        if ((epsabs.le(DoubleDouble.valueOf(0.0))) && (epsrel.lt(tol))) {
            MipavUtil.displayError("Must set epsabs > 0.0 or must set epsrel >= " + tol);
            errorStatus = 6;

            return;
        }
        
        if (limit <= npts) {
            MipavUtil.displayError("Must set limit = " + limit + " > " + npts);
            errorStatus = 6;

            return;
        }
        
        driver();
        
        Preferences.debug("Test 101\n");
        Preferences.debug("Numerical Integral = " + result[0] + " after " + neval +
        " integrand evaluations used\n");
        Preferences.debug("Error status = " + errorStatus +
        " with absolute error = " + abserr[0] + "\n");
        
        testCase = 102;
        bound = DoubleDouble.valueOf(0.0);
        routine = Integration2EP.DQAGIE;
        inf = 1;
        epsabs = DoubleDouble.valueOf(0.0);
        //epsrel = DoubleDouble.valueOf(1.0E-3);
        epsrel = DoubleDouble.valueOf(1.0E-28);
        //limit = 100;
        limit = 1000;
        
        if ((inf != 1) && (inf != -1) && (inf != 2)) {
            MipavUtil.displayError("inf must equal -1, 1, or 2");
            errorStatus = 6;

            return;
        }
        
        if ((epsabs.le(DoubleDouble.valueOf(0.0))) && (epsrel.lt(tol))) {
            MipavUtil.displayError("Must set epsabs > 0.0 or must set epsrel >= " + tol);
            errorStatus = 6;

            return;
        }
        
        if (limit < 1) {
            MipavUtil.displayError("limit must be >= 1");
            errorStatus = 6;

            return;
        }
        
        driver();
        
        Preferences.debug("Test 102\n");
        Preferences.debug("Numerical Integral = " + result[0] + " after " + neval +
        " integrand evaluations used\n");
        Preferences.debug("Error status = " + errorStatus +
        " with absolute error = " + abserr[0] + "\n");
    }

    /**
     * Constructor for Integration Used with routine = DQNG.
     *
     * @param  lower    DOCUMENT ME!
     * @param  upper    DOCUMENT ME!
     * @param  routine  DOCUMENT ME!
     * @param  epsabs   DOCUMENT ME!
     * @param  epsrel   DOCUMENT ME!
     */
    public Integration2EP(DoubleDouble lower, DoubleDouble upper, int routine, DoubleDouble epsabs, DoubleDouble epsrel) {
        DoubleDouble tol;

        this.lower = lower;
        this.upper = upper;
        this.routine = routine;

        if (routine != DQNG) {
            MipavUtil.displayError("routine must be DQAGSE with this constructor");

            return;
        }

        this.epsabs = epsabs;
        this.epsrel = epsrel;

        epmach = DoubleDouble.valueOf(DoubleDouble.EPS);
        tol = ((DoubleDouble.valueOf(50.0)).multiply(epmach)).max(DoubleDouble.valueOf(5.0E-29));

        if ((epsabs.le(DoubleDouble.valueOf(0.0))) && (epsrel.lt(tol))) {
            MipavUtil.displayError("Must set epsabs > 0.0 or must set epsrel >= " + tol);
            errorStatus = 6;

            return;
        }
    }

    /**
     * Constructor used with routine = DQAGIE.
     *
     * @param  bound    double
     * @param  routine  int
     * @param  inf      int
     * @param  epsabs   double
     * @param  epsrel   double
     * @param  limit    int
     */
    public Integration2EP(DoubleDouble bound, int routine, int inf, DoubleDouble epsabs, DoubleDouble epsrel, int limit) {
        DoubleDouble tol;
        this.bound = bound;
        this.routine = routine;

        if (routine != DQAGIE) {
            MipavUtil.displayError("routine must be DQAGIE with this constructor");

            return;
        }

        this.inf = inf;

        if ((inf != 1) && (inf != -1) && (inf != 2)) {
            MipavUtil.displayError("inf must equal -1, 1, or 2");
            errorStatus = 6;

            return;
        }

        this.epsabs = epsabs;
        this.epsrel = epsrel;

        epmach = DoubleDouble.valueOf(DoubleDouble.EPS);
        tol = ((DoubleDouble.valueOf(50.0)).multiply(epmach)).max(DoubleDouble.valueOf(5.0E-29));

        if ((epsabs.le(DoubleDouble.valueOf(0.0))) && (epsrel.lt(tol))) {
            MipavUtil.displayError("Must set epsabs > 0.0 or must set epsrel >= " + tol);
            errorStatus = 6;

            return;
        }

        this.limit = limit;

        if (limit < 1) {
            MipavUtil.displayError("limit must be >= 1");
            errorStatus = 6;

            return;
        }

    }

    /**
     * Constructor for Integration Used with routine = DQAGSE.
     *
     * @param  lower    DOCUMENT ME!
     * @param  upper    DOCUMENT ME!
     * @param  routine  DOCUMENT ME!
     * @param  epsabs   DOCUMENT ME!
     * @param  epsrel   DOCUMENT ME!
     * @param  limit    DOCUMENT ME!
     */
    public Integration2EP(DoubleDouble lower, DoubleDouble upper, int routine, DoubleDouble epsabs, DoubleDouble epsrel, int limit) {
        DoubleDouble tol;

        this.lower = lower;
        this.upper = upper;
        this.routine = routine;

        if (routine != DQAGSE) {
            MipavUtil.displayError("routine must be DQAGSE with this constructor");

            return;
        }

        this.epsabs = epsabs;
        this.epsrel = epsrel;

        epmach = DoubleDouble.valueOf(DoubleDouble.EPS);
        tol = ((DoubleDouble.valueOf(50.0)).multiply(epmach)).max(DoubleDouble.valueOf(5.0E-29));

        if ((epsabs.le(DoubleDouble.valueOf(0.0))) && (epsrel.lt(tol))) {
            MipavUtil.displayError("Must set epsabs > 0.0 or must set epsrel >= " + tol);
            errorStatus = 6;

            return;
        }

        this.limit = limit;

        if (limit < 1) {
            MipavUtil.displayError("limit must be >= 1");
            errorStatus = 6;

            return;
        }
    }


    /**
     * Constructor for Integration Used with routine = DQAGPE.
     *
     * @param  lower        DOCUMENT ME!
     * @param  upper        DOCUMENT ME!
     * @param  routine      DOCUMENT ME!
     * @param  breakPoints  DOCUMENT ME!
     * @param  epsabs       DOCUMENT ME!
     * @param  epsrel       DOCUMENT ME!
     * @param  limit        DOCUMENT ME!
     */
    public Integration2EP(DoubleDouble lower, DoubleDouble upper, int routine, DoubleDouble[] breakPoints, 
    		              DoubleDouble epsabs, DoubleDouble epsrel, int limit) {
        DoubleDouble tol;

        this.lower = lower;
        this.upper = upper;
        this.routine = routine;

        if (routine != DQAGPE) {
            MipavUtil.displayError("routine must be DQAGPE with this constructor");

            return;
        }

        this.breakPoints = breakPoints;
        npts2 = breakPoints.length;
        npts = npts2 - 2;

        if (npts2 < 2) {
            MipavUtil.displayError("breakPoints array length must be >= 2");
            errorStatus = 6;

            return;
        }

        for (int i = 0; i < (npts2 - 2); i++) {

            if (breakPoints[i].lt(lower)) {
                MipavUtil.displayError("Break point " + breakPoints[i] + " is impossibly < lower");
                errorStatus = 6;

                return;
            }

            if (breakPoints[i].gt(upper)) {
                MipavUtil.displayError("Break point " + breakPoints[i] + " is impossibly > upper");
                errorStatus = 6;

                return;
            }
        } // for (int i = 0; i < npts2 - 2; i++)

        this.epsabs = epsabs;
        this.epsrel = epsrel;

        epmach = DoubleDouble.valueOf(DoubleDouble.EPS);
        tol = ((DoubleDouble.valueOf(50.0)).multiply(epmach)).max(DoubleDouble.valueOf(5.0E-29));

        if ((epsabs.le(DoubleDouble.valueOf(0.0))) && (epsrel.lt(tol))) {
            MipavUtil.displayError("Must set epsabs > 0.0 or must set epsrel >= " + tol);
            errorStatus = 6;

            return;
        }

        this.limit = limit;

        if (limit <= npts) {
            MipavUtil.displayError("Must set limit = " + limit + " > " + npts);
            errorStatus = 6;

            return;
        }
    }

    /**
     * Constructor for Integration Used with routine = DQAGE.
     *
     * @param  lower    DOCUMENT ME!
     * @param  upper    DOCUMENT ME!
     * @param  routine  DOCUMENT ME!
     * @param  key      DOCUMENT ME!
     * @param  epsabs   DOCUMENT ME!
     * @param  epsrel   DOCUMENT ME!
     * @param  limit    DOCUMENT ME!
     */
    public Integration2EP(DoubleDouble lower, DoubleDouble upper, int routine, int key,
    		              DoubleDouble epsabs, DoubleDouble epsrel, int limit) {
        DoubleDouble tol;

        this.lower = lower;
        this.upper = upper;
        this.routine = routine;

        if (routine != DQAGE) {
            MipavUtil.displayError("routine must be DQAGE with this constructor");

            return;
        }

        this.key = key;
        this.epsabs = epsabs;
        this.epsrel = epsrel;

        epmach = DoubleDouble.valueOf(DoubleDouble.EPS);
        tol = ((DoubleDouble.valueOf(50.0)).multiply(epmach)).max(DoubleDouble.valueOf(5.0E-29));

        if ((epsabs.le(DoubleDouble.valueOf(0.0))) && (epsrel.lt(tol))) {
            MipavUtil.displayError("Must set epsabs > 0.0 or must set epsrel >= " + tol);
            errorStatus = 6;

            return;
        }

        this.limit = limit;

        if (limit <= 1) {
            MipavUtil.displayError("limit must be >= 1");
            errorStatus = 6;

            return;
        }
    }

    //~ Methods --------------------------------------------------------------------------------------------------------


    /**
     * DOCUMENT ME!
     *
     * @param   x  DOCUMENT ME!
     *
     * @return  DOCUMENT ME!
     */
    public abstract DoubleDouble intFunc(DoubleDouble x);
    
    private DoubleDouble intFuncTest(DoubleDouble x) {
    	DoubleDouble function = DoubleDouble.valueOf(0.0);
    	switch(testCase) {
    	case 1:
    		function = ((DoubleDouble.valueOf(100.0)).multiply(x.sin())).cos();
    		break;
    	case 2:
    		function = (x.log()).divide((DoubleDouble.valueOf(1.0)).add(((DoubleDouble.valueOf(100.0)).multiply(x)).multiply(x)));
    		break;
    	case 3:
    		function = ((x.multiply(x)).multiply(x)).multiply(((((x.multiply(x)).subtract(DoubleDouble.valueOf(1.0))).multiply
    				                                       ((x.multiply(x)).subtract(DoubleDouble.valueOf(2.0)))).abs()).log());
    		break;
    	case 4:
    		function = (x.log()).divide(x.sqrt());
    		break;
    	case 9:
    	case 10:
    	case 11:
    	case 12:
    	case 13:
    	case 14:
    	case 15:
    		if (x.le(DoubleDouble.valueOf(0.0))) {
    		    function = DoubleDouble.valueOf(0.0);	
    		}
    		else {
    			function = (x.sqrt()).multiply(x.log());
    		}
    		break;
    	case 101:
    		DoubleDouble oneSeventh = (DoubleDouble.valueOf(1.0)).divide(DoubleDouble.valueOf(7.0));
    		DoubleDouble twoThirds = (DoubleDouble.valueOf(2.0)).divide(DoubleDouble.valueOf(3.0));
    		if ((x.ne(oneSeventh)) && (x.ne(twoThirds))) {
    	          function = (((x.subtract(oneSeventh)).abs()).pow( -0.25)).multiply
    	                     (((x.subtract(twoThirds)).abs()).pow( -0.55));
    	        }
    		break;
    	case 102:
    		if (x.isPositive()) {
    	          function = ((x.sqrt()).multiply(x.log())).divide(((x.add(DoubleDouble.valueOf(1.0))).multiply(x.add(DoubleDouble.valueOf(2.0)))));
    	        }
    		break;
    	}
    	return function;
    }

    /**
     * This is a port of the orginal FORTRAN code whose header is given below:
     * c***begin prologue  dqagie
	 c***date written   800101   (yymmdd)
	 c***revision date  830518   (yymmdd)
	 c***category no.  h2a3a1,h2a4a1
	 c***keywords  automatic integrator, infinite intervals,
	 c             general-purpose, transformation, extrapolation,
	 c             globally adaptive
	 c***author  piessens,robert,appl. math & progr. div - k.u.leuven
	 c           de doncker,elise,appl. math & progr. div - k.u.leuven
	 c***purpose  the routine calculates an approximation result to a given
	 c            integral   i = integral of f over (bound,+infinity)
	 c            or i = integral of f over (-infinity,bound)
	 c            or i = integral of f over (-infinity,+infinity),
	 c            hopefully satisfying following claim for accuracy
	 c            abs(i-result).le.max(epsabs,epsrel*abs(i))
	 c***description
	 c
	 c integration over infinite intervals
     c standard fortran subroutine

     */
    public void dqagie() {

        // Variables ending in 1 denote left subinterval and variables
        // ending in 2 denote right subinterval
        // rlist2 should be length at least (limexp + 2)
        // rlist2 contains the part of the epsilon table still needed for
        // further computations
        DoubleDouble[] rlist2;

        // Index to the interval with the largest error estimate
        int[] maxErr = new int[1];

        // errMax[0] = elist[maxErr];
        DoubleDouble[] errMax = new DoubleDouble[1];

        // error on the interval currently subdivided (before that subdivision
        // has taken place)
        DoubleDouble erlast;

        // Sum of the integrals over the subintervals
        DoubleDouble area;

        // Sum of the errors over the subintervals
        DoubleDouble errSum;

        // Requested accuracy max(epsabs, epsrel * abs(result))
        DoubleDouble errBnd;

        // Number of calls to the extrapolation routine
        int[] nres = new int[1];

        // Number of elements in rlist2.  If an appropriate approximation
        // to the compounded integral has been obtained, it is put in
        // rlist2[numr12-1] after numr12 has been increased by one.
        int[] numr12 = new int[1];

        // Sum of the errors over the intervals larger than the smallest
        // interval considered up to now
        DoubleDouble erlarg;

        // boolean variable denoting that the routine is attempting to
        // perform extrapolation.  That is, before dividing the
        // interval we try to decrease the value of erlarg
        boolean extrap;

        // If noext is true, the extrapolation is no longer allowed.
        boolean noext;

        // length of the smallest interval considered up to now, multiplied
        // by 1.5
        DoubleDouble small;
        DoubleDouble[] res31a;
        DoubleDouble[] abseps = new DoubleDouble[1];
        DoubleDouble[] area1 = new DoubleDouble[1];
        DoubleDouble area12;
        DoubleDouble[] area2 = new DoubleDouble[1];
        DoubleDouble a1;
        DoubleDouble a2;
        DoubleDouble boun;
        DoubleDouble b1;
        DoubleDouble b2;
        DoubleDouble correc;
        DoubleDouble[] defabs = new DoubleDouble[1];
        DoubleDouble[] defab1 = new DoubleDouble[1];
        DoubleDouble[] defab2 = new DoubleDouble[1];
        DoubleDouble dres;
        DoubleDouble[] error1 = new DoubleDouble[1];
        DoubleDouble[] error2 = new DoubleDouble[1];
        DoubleDouble error12;
        DoubleDouble ertest;
        DoubleDouble[] resabs = new DoubleDouble[1];
        DoubleDouble[] reseps = new DoubleDouble[1];
        int id;
        int ierro;
        int iroff1;
        int iroff2;
        int iroff3;
        int jupbnd;
        int k;
        int ksgn;
        int ktmin;
        int[] nrmax = new int[1];

        try {
            alist = new DoubleDouble[limit];
            blist = new DoubleDouble[limit];
            rlist = new DoubleDouble[limit];
            elist = new DoubleDouble[limit];
            iord = new int[limit];
            res31a = new DoubleDouble[3];
            rlist2 = new DoubleDouble[52];

            errorStatus = 0;
            neval = 0;
            last = 0;
            result[0] = DoubleDouble.valueOf(0.0);
            abserr[0] = DoubleDouble.valueOf(0.0);
            alist[0] = DoubleDouble.valueOf(0.0);
            blist[0] = DoubleDouble.valueOf(1.0);
            rlist[0] = DoubleDouble.valueOf(0.0);
            elist[0] = DoubleDouble.valueOf(0.0);
            iord[0] = -1;

            // First approximation to the integral

            // Determine the integral to be mapped onto (0,1).
            // if inf = 2, the integral is computed as i = i1 + i2, where
            // i1 = integral of f over(-infinity, 0)
            // i2 = integral of f over (0, +infinity)

            boun = (DoubleDouble)bound.clone();

            if (inf == 2) {
                boun = DoubleDouble.valueOf(0.0);
            } // if (inf == 2)

            dqk15i(boun, inf, DoubleDouble.valueOf(0.0), DoubleDouble.valueOf(1.0), result, abserr, defabs, resabs);

            // Test on accuracy

            last = 1;
            rlist[0] = (DoubleDouble)result[0].clone();
            elist[0] = (DoubleDouble)abserr[0].clone();
            iord[0] = 0;
            dres = (result[0]).abs();
            errBnd = (epsabs).max(epsrel.multiply(dres));

            if ((abserr[0].le(((DoubleDouble.valueOf(100.0)).multiply(epmach)).multiply(defabs[0]))) && (abserr[0].gt(errBnd))) {
                errorStatus = 2;
            } // if ((abserr[0].le(((DoubleDouble.valueOf(100.0)).multiply(epmach)).multiply(defabs[0]))) && (abserr[0].gt(errBnd)))

            if (limit == 1) {
                errorStatus = 1;
            } // if (limit == 1)

            if ((errorStatus != 0) || ((abserr[0].le(errBnd)) && (abserr[0].ne(resabs[0]))) || (abserr[0].equals(DoubleDouble.valueOf(0.0)))) {
                neval = (30 * last) - 15;

                if (inf == 2) {
                    neval = 2 * neval;
                } // if (inf == 2)

                if (errorStatus > 2) {
                    errorStatus = errorStatus - 1;
                } // if (errorStatus > 2)

                return;
            } // if ((errorStatus != 0) ||

            // Initialization

            rlist2[0] = (DoubleDouble)result[0].clone();
            errMax[0] = (DoubleDouble)abserr[0].clone();
            maxErr[0] = 0;
            area = (DoubleDouble)result[0].clone();
            errSum = (DoubleDouble)abserr[0].clone();
            abserr[0] = (DoubleDouble)oflow.clone();
            nrmax[0] = 1;
            nres[0] = 0;
            ktmin = 0;
            numr12[0] = 2;
            extrap = false;
            noext = false;
            erlarg = (DoubleDouble)errSum.clone();
            correc = (DoubleDouble)erlarg.clone();
            small = DoubleDouble.valueOf(0.375);
            ertest = (DoubleDouble)errBnd.clone();
            ierro = 0;
            iroff1 = 0;
            iroff2 = 0;
            iroff3 = 0;
            ksgn = -1;

            if (dres.ge(((DoubleDouble.valueOf(1.0)).subtract((DoubleDouble.valueOf(50.0)).multiply(epmach))).multiply(defabs[0]))) {
                ksgn = 1;
            } // if (dres >= (1.0 - 50.0 * epmach)*defabs[0])

// main do-loop
loop:
            for (last = 2; last <= limit; last++) {

                // Bisect the interval with the nrmax-th largest error estimate.

                a1 = (DoubleDouble)alist[maxErr[0]].clone();
                b1 = (DoubleDouble.valueOf(0.5)).multiply(alist[maxErr[0]].add(blist[maxErr[0]]));
                a2 = (DoubleDouble)b1.clone();
                b2 = (DoubleDouble)blist[maxErr[0]].clone();
                erlast = (DoubleDouble)errMax[0].clone();
                dqk15i(boun, inf, a1, b1, area1, error1, resabs, defab1);
                dqk15i(boun, inf, a2, b2, area2, error2, resabs, defab2);

                // Improve previous approximations to integral and error
                // and test for accuracy

                area12 = area1[0].add(area2[0]);
                error12 = error1[0].add(error2[0]);
                errSum = (errSum.add(error12)).subtract(errMax[0]);
                area = (area.add(area12)).subtract(rlist[maxErr[0]]);

                if ((defab1[0].ne(error1[0])) && (defab2[0].ne(error2[0]))) {

                	if ((((rlist[maxErr[0]].subtract(area12)).abs()).le((DoubleDouble.valueOf(1.0E-5)).multiply(area12.abs()))) &&
                            (error12.ge((DoubleDouble.valueOf(0.99)).multiply(errMax[0])))) {

                        if (extrap) {
                            iroff2 = iroff2 + 1;
                        } else {
                            iroff1 = iroff1 + 1;
                        }
                    } // if ((Math.abs(rlist[maxErr[0]] - area12) <=

                    if ((last > 10) && (error12.gt(errMax[0]))) {
                        iroff3 = iroff3 + 1;
                    } // if ((last > 10) && (error12 > errMax[0]))
                } // if ((defab1[0] != error1[0]) && (defab2[0] != error2[0]))

                rlist[maxErr[0]] = (DoubleDouble)area1[0].clone();
                rlist[last - 1] = (DoubleDouble)area2[0].clone();
                errBnd = (epsabs).max(epsrel.multiply(area.abs()));

                // Test for roundoff error and eventually set error flag

                if (((iroff1 + iroff2) >= 10) || (iroff3 >= 20)) {
                    errorStatus = 2;
                } // if ((iroff1 + iroff2 >= 10) || (iroff3 >= 20))

                if (iroff2 >= 5) {
                    ierro = 3;
                } // if (iroff2 >= 5)

                // Set error flag in the case that the number of subintervals
                // equals limit

                if (last == limit) {
                    errorStatus = 1;
                } // if (last == limit)

                // Set error flag in the case of bad integrand behavior
                // at some points of the integration range
                if (((a1.abs()).max(b2.abs())).le
                        (((DoubleDouble.valueOf(1.0)).add((DoubleDouble.valueOf(100.0)).multiply(epmach))).multiply((a2.abs()).add((DoubleDouble.valueOf(1.0E3)).multiply(uflow))))) {
                    errorStatus = 4;
                } // if (Math.max(Math.abs(a1), Math.abs(b2)) <=

                // Append the newly-created intervals to the list.

                if (error2[0].le(error1[0])) {
                    alist[last - 1] = (DoubleDouble)a2.clone();
                    blist[maxErr[0]] = (DoubleDouble)b1.clone();
                    blist[last - 1] = (DoubleDouble)b2.clone();
                    elist[maxErr[0]] = (DoubleDouble)error1[0].clone();
                    elist[last - 1] = (DoubleDouble)error2[0].clone();
                } // if (error2[0] <= error1[0])
                else {
                    alist[maxErr[0]] = (DoubleDouble)a2.clone();
                    alist[last - 1] = (DoubleDouble)a1.clone();
                    blist[last - 1] = (DoubleDouble)b1.clone();
                    rlist[maxErr[0]] = (DoubleDouble)area2[0].clone();
                    rlist[last - 1] = (DoubleDouble)area1[0].clone();
                    elist[maxErr[0]] = (DoubleDouble)error2[0].clone();
                    elist[last - 1] = (DoubleDouble)error1[0].clone();
                } // else

                // Call the subroutine dqpsrt to maintain the descending ordering
                // in the list of error estimates and select the subinterval
                // with nrmax-th largest error estimate (to be bisected next).

                dqpsrt(limit, last, maxErr, errMax, elist, iord, nrmax);

                if (errSum.le(errBnd)) {
                    result[0] = DoubleDouble.valueOf(0.0);

                    for (k = 0; k < last; k++) {
                        result[0] = result[0].add(rlist[k]);
                    } // for (k = 0; k < last; k++)

                    abserr[0] = (DoubleDouble)errSum.clone();
                    neval = (30 * last) - 15;

                    if (inf == 2) {
                        neval = 2 * neval;
                    } // if (inf == 2)

                    if (errorStatus > 2) {
                        errorStatus = errorStatus - 1;
                    } // if (errorStatus > 2)

                    return;
                } // if (errSum <= errBnd)

                if (errorStatus != 0) {
                    break;
                } // if (errorStatus != 0)

                if (last == 2) {
                    small = DoubleDouble.valueOf(0.375);
                    erlarg = (DoubleDouble)errSum.clone();
                    ertest = (DoubleDouble)errBnd.clone();
                    rlist2[1] = (DoubleDouble)area.clone();

                    continue;
                } // if (last == 2)

                if (noext) {
                    continue;
                } // if (noext)

                erlarg = erlarg.subtract(erlast);

                if (((b1.subtract(a1)).abs()).gt(small)) {
                    erlarg = erlarg.add(error12);
                } // if (Math.abs(b1 - a1) > small)

                if (!extrap) {

                    // Test whether the interval to be bisected next is the
                    // smallest interval

                	if (((blist[maxErr[0]].subtract(alist[maxErr[0]])).abs()).gt(small)) {
                        continue;
                    } // if (Math.abs(blist[maxErr[0]] - alist[maxErr[0]]) > small)

                    extrap = true;
                    nrmax[0] = 2;
                } // if (!extrap)

                if ((ierro != 3) && (erlarg.gt(ertest))) {

                    // The smallest interval has the largest error.
                    // Before bisecting decrease the sum of the errors over the
                    // larger intervals (erlarg) and perform extrapolation.

                    id = nrmax[0];
                    jupbnd = last;

                    if (last > (2 + (limit / 2))) {
                        jupbnd = limit + 3 - last;
                    } // if (last > (2 + limit/2))

                    for (k = id; k <= jupbnd; k++) {
                        maxErr[0] = iord[nrmax[0] - 1];
                        errMax[0] = (DoubleDouble)elist[maxErr[0]].clone();

                        if (((blist[maxErr[0]].subtract(alist[maxErr[0]])).abs()).gt(small)) {
                            continue loop;
                        }

                        nrmax[0] = nrmax[0] + 1;
                    } // for (k = id; k <= jupbnd; k++)
                } // if ((ierro != 3) && (erlarg > ertest))

                // Perform extrapolation

                numr12[0] = numr12[0] + 1;
                rlist2[numr12[0] - 1] = area;
                dqelg(numr12, rlist2, reseps, abseps, res31a, nres);
                ktmin = ktmin + 1;

                if ((ktmin > 5) && (abserr[0].lt((DoubleDouble.valueOf(1.0E-3)).multiply(errSum)))) {
                    errorStatus = 5;
                } // if ((ktmin > 5) && (abserr[0] < 1.0E-3 * errSum))

                if (abseps[0].lt(abserr[0])) {
                    ktmin = 0;
                    abserr[0] = (DoubleDouble)abseps[0].clone();
                    result[0] = (DoubleDouble)reseps[0].clone();
                    correc = (DoubleDouble)erlarg.clone();
                    ertest = epsabs.max(epsrel.multiply(reseps[0].abs()));

                    if (abserr[0].le(ertest)) {
                        break;
                    } // if (abserr[0] <= ertest)
                } // if (absesp[0] < abserr[0])

                // Prepare bisection of the smallest interval
                if (numr12[0] == 1) {
                    noext = true;
                } // if (numr12[0] == 1)

                if (errorStatus == 5) {
                    break;
                } // if (errorStatus == 5)

                maxErr[0] = iord[0];
                errMax[0] = (DoubleDouble)elist[maxErr[0]].clone();
                nrmax[0] = 1;
                extrap = false;
                small = (DoubleDouble.valueOf(0.5)).multiply(small);
                erlarg = (DoubleDouble)errSum.clone();
            } // for (last = 2; last <= limit; last++)

            // Set final result and error estimate

            if (abserr[0].ne(oflow)) {

                if ((errorStatus + ierro) != 0) {

                    if (ierro == 3) {
                        abserr[0] = abserr[0].add(correc);
                    } // if (ierro == 3)

                    if (errorStatus == 0) {
                        errorStatus = 3;
                    } // if (errorStatus == 0)

                    if ((result[0].isZero()) || (area.isZero())) {

                        if (abserr[0].gt(errSum)) {
                            result[0] = DoubleDouble.valueOf(0.0);

                            for (k = 0; k < last; k++) {
                                result[0] = result[0].add(rlist[k]);
                            } // for (k = 0; k < last; k++)

                            abserr[0] = (DoubleDouble)errSum.clone();
                            neval = (30 * last) - 15;

                            if (inf == 2) {
                                neval = 2 * neval;
                            } // if (inf == 2)

                            if (errorStatus > 2) {
                                errorStatus = errorStatus - 1;
                            } // if (errorStatus > 2)

                            return;
                        } // if (abserr[0] > errSum)

                        if (area.isZero()) {
                            neval = (30 * last) - 15;

                            if (inf == 2) {
                                neval = 2 * neval;
                            } // if (inf == 2)

                            if (errorStatus > 2) {
                                errorStatus = errorStatus - 1;
                            } // if (errorStatus > 2)

                            return;
                        } // if (area == 0.0)

                        if ((ksgn == -1) && ((result[0].abs()).max(area.abs()).le((DoubleDouble.valueOf(1.0E-2)).multiply(defabs[0])))) {
                            neval = (30 * last) - 15;

                            if (inf == 2) {
                                neval = 2 * neval;
                            } // if (inf == 2)

                            if (errorStatus > 2) {
                                errorStatus = errorStatus - 1;
                            } // if (errorStatus > 2)

                            return;
                        } // if ((ksgn == -1) && (Math.max(Math.abs(result[0]),

                        if (((DoubleDouble.valueOf(1.0E-2)).gt(result[0].divide(area))) || ((result[0].divide(area)).gt(DoubleDouble.valueOf(100.0))) ||
                                (errSum.gt(area.abs()))) {
                            errorStatus = 6;
                        } // if ((1.0E-2 > (result[0]/area)) || (result[0]/area > 100.0) ||

                        neval = (30 * last) - 15;

                        if (inf == 2) {
                            neval = 2 * neval;
                        } // if (inf == 2)

                        if (errorStatus > 2) {
                            errorStatus = errorStatus - 1;
                        } // if (errorStatus > 2)

                        return;
                    } // if (result[0] == 0.0) || (area == 0.0))

                    if ((abserr[0].divide(result[0].abs())).gt(errSum.divide(area.abs()))) {
                        result[0] = DoubleDouble.valueOf(0.0);

                        for (k = 0; k < last; k++) {
                            result[0] = result[0].add(rlist[k]);
                        } // for (k = 0; k < last; k++)

                        abserr[0] = (DoubleDouble)errSum.clone();
                        neval = (30 * last) - 15;

                        if (inf == 2) {
                            neval = 2 * neval;
                        } // if (inf == 2)

                        if (errorStatus > 2) {
                            errorStatus = errorStatus - 1;
                        } // if (errorStatus > 2)

                        return;
                    } // if (abserr[0]/Math.abs(result[0]) > errSum/Math.abs(area))
                } // if ((errorStatus + ierro) != 0)

                if ((ksgn == -1) && (((result[0].abs()).max(area.abs())).le((DoubleDouble.valueOf(1.0E-2)).multiply(defabs[0])))) {
                    neval = (30 * last) - 15;

                    if (inf == 2) {
                        neval = 2 * neval;
                    } // if (inf == 2)

                    if (errorStatus > 2) {
                        errorStatus = errorStatus - 1;
                    } // if (errorStatus > 2)

                    return;
                } // if ((ksgn == -1) && (Math.max(Math.abs(result[0]),

                if (((DoubleDouble.valueOf(1.0E-2)).gt(result[0].divide(area))) || ((result[0].divide(area)).gt(DoubleDouble.valueOf(100.0))) ||
                		                  (errSum.gt(area.abs()))) {
                    errorStatus = 6;
                } // if ((1.0E-2 > (result[0]/area)) || (result[0]/area > 100.0) ||

                neval = (30 * last) - 15;

                if (inf == 2) {
                    neval = 2 * neval;
                } // if (inf == 2)

                if (errorStatus > 2) {
                    errorStatus = errorStatus - 1;
                } // if (errorStatus > 2)

                return;
            } // if (abserr[0] != oflow)

            result[0] = DoubleDouble.valueOf(0.0);

            for (k = 0; k < last; k++) {
                result[0] = result[0].add(rlist[k]);
            } // for (k = 0; k < last; k++)

            abserr[0] = (DoubleDouble)errSum.clone();
            neval = (30 * last) - 15;

            if (inf == 2) {
                neval = 2 * neval;
            } // if (inf == 2)

            if (errorStatus > 2) {
                errorStatus = errorStatus - 1;
            } // if (errorStatus > 2)

            return;

        } // try
        catch (Exception err) {
            Preferences.debug("dqagie error: " + err.getMessage());
        }

    } // dqagie


    /**
     * dqagpe.
     * This is a port of the original FORTRAN code whose header is given below:
     * c***begin prologue  dqagie
	 c***date written   800101   (yymmdd)
	 c***revision date  830518   (yymmdd)
	 c***category no.  h2a3a1,h2a4a1
	 c***keywords  automatic integrator, infinite intervals,
	 c             general-purpose, transformation, extrapolation,
	 c             globally adaptive
	 c***author  piessens,robert,appl. math & progr. div - k.u.leuven
	 c           de doncker,elise,appl. math & progr. div - k.u.leuven
	 c***purpose  the routine calculates an approximation result to a given
	 c            integral   i = integral of f over (bound,+infinity)
	 c            or i = integral of f over (-infinity,bound)
	 c            or i = integral of f over (-infinity,+infinity),
	 c            hopefully satisfying following claim for accuracy
	 c            abs(i-result).le.max(epsabs,epsrel*abs(i))
	 c***description
	 c
	 c integration over infinite intervals
	 c standard fortran subroutine

     */
    public void dqagpe() {
        DoubleDouble[] res31a;

        // rlist2 should be length at least (limexp + 2)
        // rlist2 contains the part of the epsilon table still needed for
        // further computations
        DoubleDouble[] rlist2;

        // Index to the interval with the largest error estimate
        int[] maxErr = new int[1];

        // errMax[0] = elist[maxErr];
        DoubleDouble[] errMax = new DoubleDouble[1];

        // error on the interval currently subdivided (before that subdivision
        // has taken place)
        DoubleDouble erlast;

        // Sum of the integrals over the subintervals
        DoubleDouble area;

        // Sum of the errors over the subintervals
        DoubleDouble errSum;

        // Requested accuracy max(epsabs, epsrel * abs(result))
        DoubleDouble errBnd;

        // Number of calls to the extrapolation routine
        int[] nres = new int[1];

        // Number of elements in rlist2.  If an appropriate approximation
        // to the compounded integral has been obtained, it is put in
        // rlist2[numr12-1] after numr12 has been increased by one.
        int[] numr12 = new int[1];

        // Sum of the errors over the intervals larger than the smallest
        // interval considered up to now
        DoubleDouble erlarg;

        // boolean variable denoting that the routine is attempting to
        // perform extrapolation.  That is, before dividing the
        // interval we try to decrease the value of erlarg
        boolean extrap;

        // If noext is true, the extrapolation is no longer allowed.
        boolean noext;

        // Variables ending in 1 denote left subinterval and variables
        // ending in 2 denote right subinterval
        DoubleDouble[] abseps = new DoubleDouble[1];
        DoubleDouble[] area1 = new DoubleDouble[1];
        DoubleDouble area12;
        DoubleDouble[] area2 = new DoubleDouble[1];
        DoubleDouble a1;
        DoubleDouble a2;
        DoubleDouble b1;
        DoubleDouble b2;
        DoubleDouble correc = DoubleDouble.valueOf(0.0);
        DoubleDouble[] defabs = new DoubleDouble[1];
        DoubleDouble[] defab1 = new DoubleDouble[1];
        DoubleDouble[] defab2 = new DoubleDouble[1];
        DoubleDouble dres;
        DoubleDouble[] error1 = new DoubleDouble[1];
        DoubleDouble error12;
        DoubleDouble[] error2 = new DoubleDouble[1];
        DoubleDouble ertest;
        DoubleDouble[] resa = new DoubleDouble[1];
        DoubleDouble resabs;
        DoubleDouble[] reseps = new DoubleDouble[1];
        DoubleDouble sign;
        DoubleDouble temp;
        int i;
        int id;
        int ierro;
        int ind1;
        int ind2;
        int ip1;
        int iroff1;
        int iroff2;
        int iroff3;
        int j;
        int jlow;
        int jupbnd;
        int k = 0;
        int ksgn;
        int ktmin;
        int levcur;
        int levmax;
        int nint;
        int nintp1;
        int[] nrmax = new int[1];

        try {
            alist = new DoubleDouble[limit];
            blist = new DoubleDouble[limit];
            rlist = new DoubleDouble[limit];
            elist = new DoubleDouble[limit];
            pts = new DoubleDouble[npts2];
            level = new int[limit];
            ndin = new int[npts2];
            iord = new int[limit];
            res31a = new DoubleDouble[3];
            rlist2 = new DoubleDouble[52];

            errorStatus = 0;
            neval = 0;
            last = 0;
            result[0] = DoubleDouble.valueOf(0.0);
            abserr[0] = DoubleDouble.valueOf(0.0);
            alist[0] = (DoubleDouble)lower.clone();
            blist[0] = (DoubleDouble)upper.clone();
            rlist[0] = DoubleDouble.valueOf(0.0);
            elist[0] = DoubleDouble.valueOf(0.0);
            iord[0] = -1;
            level[0] = 0;

            // If any break points are provided, sort them into an
            // ascending sequence.
            sign = DoubleDouble.valueOf(1.0);

            if (lower.gt(upper)) {
                sign = DoubleDouble.valueOf(-1.0);
            }

            pts[0] = lower.min(upper);

            if (npts != 0) {

                for (i = 0; i < npts; i++) {
                    pts[i + 1] = breakPoints[i];
                }
            } // if (npts != 0)

            pts[npts + 1] = lower.max(upper);
            nint = npts + 1;
            a1 = pts[0];

            if (npts != 0) {
                nintp1 = nint + 1;

                for (i = 0; i < nint; i++) {
                    ip1 = i + 1;

                    for (j = ip1; j < nintp1; j++) {

                        if (pts[i].gt(pts[j])) {
                            temp = (DoubleDouble)pts[i].clone();
                            pts[i] = (DoubleDouble)pts[j].clone();
                            pts[j] = (DoubleDouble)temp.clone();
                        } // if (pts[i] > pts[j])
                    } // for (j = ip1; j < nintp1; j++)
                } // for (i = 0; i < nint; i++)
            } // if (npts != 0)

            // Compute first integral and error approximations

            resabs = DoubleDouble.valueOf(0.0);

            for (i = 0; i < nint; i++) {
                b1 = (DoubleDouble)pts[i + 1].clone();
                dqk21(a1, b1, area1, error1, defabs, resa);
                abserr[0] = abserr[0].add(error1[0]);
                result[0] = result[0].add(area1[0]);
                ndin[i] = 0;

                if ((error1[0].equals(resa[0])) && (error1[0].ne(DoubleDouble.valueOf(0.0)))) {
                    ndin[i] = 1;
                }

                resabs = resabs.add(defabs[0]);
                level[i] = 0;
                elist[i] = (DoubleDouble)error1[0].clone();
                alist[i] = (DoubleDouble)a1.clone();
                blist[i] = (DoubleDouble)b1.clone();
                rlist[i] = (DoubleDouble)area1[0].clone();
                iord[i] = i;
                a1 = (DoubleDouble)b1.clone();
            } // for (i = 0; i < nint; i++)

            errSum = DoubleDouble.valueOf(0.0);

            for (i = 0; i < nint; i++) {

                if (ndin[i] == 1) {
                    elist[i] = (DoubleDouble)abserr[0].clone();
                } // if (ndin[i] == 1)

                errSum = errSum.add(elist[i]);
            } // for (i = 0; i < nint; i++)

            // Test on accuracy

            last = nint;
            neval = 21 * nint;
            dres = (result[0].abs());
            errBnd = epsabs.max(dres.multiply(epsrel));

            if ((abserr[0].le(((DoubleDouble.valueOf(100.0)).multiply(epmach)).multiply(resabs))) && (abserr[0].gt(errBnd))) {
                errorStatus = 2;
            }

            if (nint != 1) {

                for (i = 0; i < npts; i++) {
                    jlow = i + 1;
                    ind1 = iord[i];

                    for (j = jlow; j < nint; j++) {
                        ind2 = iord[j];

                        if (elist[ind1].lt(elist[ind2])) {
                            ind1 = ind2;
                            k = j;
                        } // if (elist[ind1] <= elist[ind2])
                    } // for (j = jlow; j < nint; j++)

                    if (ind1 != iord[i]) {
                        iord[k] = iord[i];
                        iord[i] = ind1;
                    } // if (ind1 != iord[i])
                } // for (i = 0; i < npts; i++)

                if (limit < npts2) {
                    errorStatus = 1;
                } // if (limit < npts2)
            } // if (nint != 1)

            if ((errorStatus != 0) || (abserr[0].le(errBnd))) {

                if (errorStatus > 2) {
                    errorStatus = errorStatus - 1;
                } // if (errorStatus > 2)

                result[0] = result[0].multiply(sign);

                return;
            } // if ((errorStatus != 0) || (abserr[0] <= errBnd))

            // Initialization

            rlist2[0] = (DoubleDouble)result[0].clone();
            maxErr[0] = iord[0];
            errMax[0] = (DoubleDouble)elist[maxErr[0]].clone();
            area = (DoubleDouble)result[0].clone();
            nrmax[0] = 1;
            nres[0] = 0;
            numr12[0] = 1;
            ktmin = 0;
            extrap = false;
            noext = false;
            erlarg = (DoubleDouble)errSum.clone();
            ertest = (DoubleDouble)errBnd.clone();
            levmax = 1;
            iroff1 = 0;
            iroff2 = 0;
            iroff3 = 0;
            ierro = 0;
            abserr[0] = (DoubleDouble)oflow.clone();
            ksgn = -1;

            if (dres.ge(((DoubleDouble.valueOf(1.0)).subtract((DoubleDouble.valueOf(50.0)).multiply(epmach))).multiply(resabs))) {
                ksgn = 1;
            }

// Main do-loop
loop:
            for (last = npts2; last <= limit; last++) {

                // Bisect the subinterval with the nrmax-th largest error
                // estimate
                levcur = level[maxErr[0]] + 1;
                a1 = (DoubleDouble)alist[maxErr[0]].clone();
                b1 = (DoubleDouble.valueOf(0.5)).multiply(alist[maxErr[0]].add(blist[maxErr[0]]));
                a2 = (DoubleDouble)b1.clone();
                b2 = (DoubleDouble)blist[maxErr[0]].clone();
                erlast = DoubleDouble.valueOf((double)maxErr[0]);
                dqk21(a1, b1, area1, error1, resa, defab1);
                dqk21(a2, b2, area2, error2, resa, defab2);

                // Improve previous approximations to integral and error
                // and test for accuracy.
                neval = neval + 42;
                area12 = area1[0].add(area2[0]);
                error12 = error1[0].add(error2[0]);
                errSum = (errSum.add(error12)).subtract(errMax[0]);
                area = (area.add(area12)).subtract(rlist[maxErr[0]]);

                if ((defab1[0].ne(error1[0])) && (defab2[0].ne(error2[0]))) {

                    if ((((rlist[maxErr[0]].subtract(area12)).abs()).le((DoubleDouble.valueOf(1.0E-5)).multiply(area12.abs()))) &&
                            (error12.ge((DoubleDouble.valueOf(0.99)).multiply(errMax[0])))) {

                        if (extrap) {
                            iroff2 = iroff2 + 1;
                        } else {
                            iroff1 = iroff1 + 1;
                        }
                    }

                    if ((last > 10) && (error12.ge(errMax[0]))) {
                        iroff3 = iroff3 + 1;
                    }
                } // if ((defab1[0] != error1[0]) && (defab2[0] != error2[0]))

                level[maxErr[0]] = levcur;
                level[last - 1] = levcur;
                rlist[maxErr[0]] = (DoubleDouble)area1[0].clone();
                rlist[last - 1] = (DoubleDouble)area2[0].clone();
                errBnd = epsabs.max( epsrel.multiply(area.abs()));

                // Test for roundoff error and eventually set error flag

                if (((iroff1 + iroff2) >= 10) || (iroff3 >= 20)) {
                    errorStatus = 2;
                }

                if (iroff2 >= 5) {
                    ierro = 3;
                }

                // Set error flag in the case that the number of
                // subintervals equals limit.

                if (last == limit) {
                    errorStatus = 1;
                }

                // Set error flag in the case of bad integrand behavior
                // at a point of the integration range

                DoubleDouble maxVar = (a1.abs()).max(b2.abs());
                DoubleDouble leftVar = (DoubleDouble.valueOf(1.0)).add((DoubleDouble.valueOf(100.0)).multiply(epmach));
                DoubleDouble rightVar = (a2.abs()).add((DoubleDouble.valueOf(1.0E3)).multiply(uflow));
                if (maxVar.le(leftVar.multiply(rightVar))) {
                    errorStatus = 4;
                }

                // Append the newly created intervals to the list

                if (error2[0].le(error1[0])) {
                    alist[last - 1] = (DoubleDouble)a2.clone();
                    blist[maxErr[0]] = (DoubleDouble)b1.clone();
                    blist[last - 1] = (DoubleDouble)b2.clone();
                    elist[maxErr[0]] = (DoubleDouble)error1[0].clone();
                    elist[last - 1] = (DoubleDouble)error2[0].clone();
                } else {
                    alist[maxErr[0]] = (DoubleDouble)a2.clone();
                    alist[last - 1] = (DoubleDouble)a1.clone();
                    blist[last - 1] = (DoubleDouble)b1.clone();
                    rlist[maxErr[0]] = (DoubleDouble)area2[0].clone();
                    rlist[last - 1] = (DoubleDouble)area1[0].clone();
                    elist[maxErr[0]] = (DoubleDouble)error2[0].clone();
                    elist[last - 1] = (DoubleDouble)error1[0].clone();
                }

                // Call subroutine dqpsrt to maintain the descending
                // ordering in the list of error estimates and select
                // the subinterval with nrmax-th largest error estimate
                // (to be bisected next).

                dqpsrt(limit, last, maxErr, errMax, elist, iord, nrmax);

                if (errSum.le(errBnd)) {
                    result[0] = DoubleDouble.valueOf(0.0);

                    for (k = 0; k < last; k++) {
                        result[0] = result[0].add(rlist[k]);
                    }

                    abserr[0] = (DoubleDouble)errSum.clone();

                    if (errorStatus > 2) {
                        errorStatus = errorStatus - 1;
                    }

                    result[0] = result[0].multiply(sign);

                    return;
                } // if (errSum <= errBnd)

                if (errorStatus != 0) {
                    break;
                } // if (errorStatus != 0)

                if (noext) {
                    continue;
                } // if (noext)

                erlarg = erlarg.subtract(erlast);

                if ((levcur + 1) <= levmax) {
                    erlarg = erlarg.add(error12);
                } // if ((levcur + 1) <= levmax)

                if (!extrap) {

                    // Test whether the interval to be bisected next is the
                    // smallest interval
                    if ((level[maxErr[0]] + 1) <= levmax) {
                        continue;
                    } // if ((level[maxErr[0]] + 1) <= levmax)

                    extrap = true;
                    nrmax[0] = 2;
                } // if (!extrap)

                if ((ierro != 3) && (erlarg.gt(ertest))) {
                    // The smallest interval has the largest error.
                    // Before bisecting decrease the sum of errors over
                    // the larger intervals (erlarg) and perform extrapolation

                    id = nrmax[0];
                    jupbnd = last;

                    if (last > (2 + (limit / 2))) {
                        jupbnd = limit + 3 - last;
                    } // if (last > (2 + limit/2))

                    for (k = id; k <= jupbnd; k++) {
                        maxErr[0] = iord[nrmax[0] - 1];
                        errMax[0] = (DoubleDouble)elist[maxErr[0]].clone();

                        if ((level[maxErr[0]] + 1) <= levmax) {
                            continue loop;
                        } // if ((level[maxErr[0]] + 1) <= levmax)

                        nrmax[0] = nrmax[0] + 1;
                    } // for (k = id; k <= jupbnd; k++)
                } // if ((ierro != 3) && (erlarg > ertest))

                // Perform extrapolation

                numr12[0] = numr12[0] + 1;
                rlist2[numr12[0] - 1] = (DoubleDouble)area.clone();

                if (numr12[0] > 2) {
                    dqelg(numr12, rlist2, reseps, abseps, res31a, nres);
                    ktmin = ktmin + 1;

                    if ((ktmin > 5) && (abserr[0].lt((DoubleDouble.valueOf(1.0E-3)).multiply(errSum)))) {
                        errorStatus = 5;
                    } // if ((ktmin > 5) && (abserr < 1.0E-3 * errSum))

                    if (abseps[0].lt(abserr[0])) {
                        ktmin = 0;
                        abserr[0] = (DoubleDouble)abseps[0].clone();
                        result[0] = (DoubleDouble)reseps[0].clone();
                        correc = (DoubleDouble)erlarg.clone();
                        ertest = epsabs.max(epsrel.multiply(reseps[0].abs()));

                        if (abserr[0].lt(ertest)) {
                            break;
                        } // if (abserr[0] < ertest)
                    } // if (abseps[0] < abserr)

                    // Prepare bisection of smallest interval
                    if (numr12[0] == 1) {
                        noext = true;
                    } // if (numr12[0] == 1)

                    if (errorStatus >= 5) {
                        break;
                    } // if (errorStatus >= 5)
                } // if (numr12[0] > 2)

                maxErr[0] = iord[0];
                errMax[0] = (DoubleDouble)elist[maxErr[0]].clone();
                nrmax[0] = 1;
                extrap = false;
                levmax = levmax + 1;
                erlarg = (DoubleDouble)errSum.clone();
            } // for (last = npts2; last <= limit; last++)

            // Set the final result.

            if (abserr[0].ne(oflow)) {

                if ((errorStatus + ierro) != 0) {

                    if (ierro == 3) {
                        abserr[0] = abserr[0].add(correc);
                    }

                    if (errorStatus == 0) {
                        errorStatus = 3;
                    } // if (errorStatus == 0)

                    if ((result[0].isZero()) || (area.isZero())) {

                        if (abserr[0].gt(errSum)) {
                            result[0] = DoubleDouble.valueOf(0.0);

                            for (k = 0; k < last; k++) {
                                result[0] = result[0].add(rlist[k]);
                            }

                            abserr[0] = (DoubleDouble)errSum.clone();

                            if (errorStatus > 2) {
                                errorStatus = errorStatus - 1;
                            }

                            result[0] = result[0].multiply(sign);

                            return;
                        } // if (abserr[0] > errSum)

                        if (area.isZero()) {

                            if (errorStatus > 2) {
                                errorStatus = errorStatus - 1;
                            }

                            result[0] = result[0].multiply(sign);

                            return;
                        } // if (area == 0.0)

                        if ((ksgn == -1) && (((result[0].abs()).max(area.abs())).le((DoubleDouble.valueOf(1.0E-2)).multiply(resabs)))) {
                        

                            if (errorStatus > 2) {
                                errorStatus = errorStatus - 1;
                            }

                            result[0] = result[0].multiply(sign);

                            return;
                        } // if ((ksgn == -1) && (Math.max(Math.abs(result[0]),

                        if (((DoubleDouble.valueOf(1.0E-2)).gt(result[0].divide(area))) || ((result[0].divide(area)) .gt(DoubleDouble.valueOf(100.0))) ||
                                (errSum.gt(area.abs()))) {
                            errorStatus = 6;
                        } // if ((1.0E-2 > (result[0]/area) || ((result[0]/area) >

                        if (errorStatus > 2) {
                            errorStatus = errorStatus - 1;
                        }

                        result[0] = result[0].multiply(sign);

                        return;
                    } // if ((result[0] == 0.0) || (area == 0.0))

                    if ((abserr[0].divide(result[0].abs())).gt(errSum.divide(area.abs()))) {
                        result[0] = DoubleDouble.valueOf(0.0);

                        for (k = 0; k < last; k++) {
                            result[0] = result[0].add(rlist[k]);
                        }

                        abserr[0] = (DoubleDouble)errSum.clone();

                        if (errorStatus > 2) {
                            errorStatus = errorStatus - 1;
                        }

                        result[0] = result[0].multiply(sign);

                        return;
                    } // if ((abserr[0]/Math.abs(result[0])) > (errSum/Math.abs(area)))
                } // if ((errorStatus + ierro) != 0)

                if ((ksgn == -1) && ((result[0].abs()).max((area.abs())).le((DoubleDouble.valueOf(1.0E-2)).multiply(resabs)))) {

                    if (errorStatus > 2) {
                        errorStatus = errorStatus - 1;
                    }

                    result[0] = result[0].multiply(sign);

                    return;
                } // if ((ksgn == -1) && (Math.max(Math.abs(result[0]),

                if (((DoubleDouble.valueOf(1.0E-2)).gt(result[0].divide(area))) || ((result[0].divide(area)).gt(DoubleDouble.valueOf(100.0))) ||
                   (errSum.gt(area.abs()))) {
                    errorStatus = 6;
                } // if ((1.0E-2 > (result[0]/area) || ((result[0]/area) >

                if (errorStatus > 2) {
                    errorStatus = errorStatus - 1;
                }

                result[0] = result[0].multiply(sign);

                return;
            } // if (abserr[0] != oflow)

            result[0] = DoubleDouble.valueOf(0.0);

            for (k = 0; k < last; k++) {
                result[0] = result[0].add(rlist[k]);
            }

            abserr[0] = (DoubleDouble)errSum.clone();

            if (errorStatus > 2) {
                errorStatus = errorStatus - 1;
            }

            result[0] = result[0].multiply(sign);

            return;

        } // try
        catch (Exception err) {
            Preferences.debug("dqagpe error: " + err.getMessage());
        }
    } // dqagpe

    /**
     * DOCUMENT ME!
     */
    public void driver() {

        switch (routine) {

            case DQAGPE:
                dqagpe();
                break;

            case DQAGIE:
                dqagie();
                break;

            case DQAGSE:
                dqagse();
                break;

            case DQAGE:
                dqage();
                break;
                
            case DQK15:
            	dqk15(lower, upper, result, abserr, resabs, resasc);
            	break;
            	
            case DQK21:
            	dqk21(lower, upper, result, abserr, resabs, resasc);
            	break;
            	
            case DQK31:
            	dqk31(lower, upper, result, abserr, resabs, resasc);
            	break;
            	
            case DQK41:
            	dqk41(lower, upper, result, abserr, resabs, resasc);
            	break;
            	
            case DQK51:
            	dqk51(lower, upper, result, abserr, resabs, resasc);
            	break;
            	
            case DQK61:
            	dqk61(lower, upper, result, abserr, resabs, resasc);
            	break;

            case DQNG:
                dqng();
                break;
        }
    }

    /**
     * DOCUMENT ME!
     *
     * @return  abserr
     */
    public DoubleDouble getAbserr() {
        return abserr[0];
    }

    /**
     * DOCUMENT ME!
     *
     * @return  errorStatus
     */
    public int getErrorStatus() {
        return errorStatus;
    }


    /**
     * DOCUMENT ME!
     *
     * @return  result
     */
    public DoubleDouble getIntegral() {
        return result[0];
    }

    /**
     * DOCUMENT ME!
     *
     * @return  neval
     */
    public int getNeval() {
        return neval;
    }

    /**
     * This is a port of the original FORTRAN routine whose header is given below:
     * c***begin prologue  dqage
	 c***date written   800101   (yymmdd)
	 c***revision date  830518   (yymmdd)
	 c***category no.  h2a1a1
	 c***keywords  automatic integrator, general-purpose,
	 c             integrand examinator, globally adaptive,
	 c             gauss-kronrod
	 c***author  piessens,robert,appl. math. & progr. div. - k.u.leuven
	 c           de doncker,elise,appl. math. & progr. div. - k.u.leuven
	 c***purpose  the routine calculates an approximation result to a given
	 c            definite integral   i = integral of f over (a,b),
	 c            hopefully satisfying following claim for accuracy
	 c            abs(i-reslt).le.max(epsabs,epsrel*abs(i)).
	 c***description
	 c
	 c        computation of a definite integral
	 c        standard fortran subroutine
	 c        DoubleDouble precision version

     * Calculates an integral over (lower, upper), hopefully satisfying the abs(actual integral - result) <= max(epsabs,
     * epsresl * abs(actual integral)).
     */
    private void dqage() {

        // Variables ending in 1 denote left subinterval and variables
        // ending in 2 denote right subinterval
        // Index to the interval with the largest error estimate
        int[] maxErr = new int[1];

        // errMax[0] = elist[maxErr];
        DoubleDouble[] errMax = new DoubleDouble[1];

        // Sum of the integrals over the subintervals
        DoubleDouble area;

        // Sum of the errors over the subintervals
        DoubleDouble errSum;

        // Requested accuracy max(epsabs, epsrel * abs(result))
        DoubleDouble errBnd;
        DoubleDouble[] area1 = new DoubleDouble[1];
        DoubleDouble area12;
        DoubleDouble[] area2 = new DoubleDouble[1];
        DoubleDouble a1;
        DoubleDouble a2;
        DoubleDouble b1;
        DoubleDouble b2;
        DoubleDouble[] defabs = new DoubleDouble[1];
        DoubleDouble[] defab1 = new DoubleDouble[1];
        DoubleDouble[] defab2 = new DoubleDouble[1];
        DoubleDouble[] error1 = new DoubleDouble[1];
        DoubleDouble[] error2 = new DoubleDouble[1];
        DoubleDouble error12;
        DoubleDouble[] resabs = new DoubleDouble[1];
        int iroff1;
        int iroff2;
        int k;
        int keyf;
        int[] nrmax = new int[1];

        try {
            alist = new DoubleDouble[limit];
            blist = new DoubleDouble[limit];
            rlist = new DoubleDouble[limit];
            elist = new DoubleDouble[limit];
            iord = new int[limit];

            errorStatus = 0;
            neval = 0;
            last = 0;
            result[0] = DoubleDouble.valueOf(0.0);
            abserr[0] = DoubleDouble.valueOf(0.0);
            alist[0] = (DoubleDouble)lower.clone();
            blist[0] = (DoubleDouble)upper.clone();
            rlist[0] = DoubleDouble.valueOf(0.0);
            elist[0] = DoubleDouble.valueOf(0.0);
            iord[0] = -1;

            // First approximation to the integral

            keyf = key;

            if (key <= 0) {
                keyf = 1;
            } // if (key <= 0)

            if (key >= 7) {
                keyf = 6;
            } // if (key >= 7)

            neval = 0;

            switch (keyf) {

                case 1:
                    dqk15(lower, upper, result, abserr, defabs, resabs);
                    break;

                case 2:
                    dqk21(lower, upper, result, abserr, defabs, resabs);
                    break;

                case 3:
                    dqk31(lower, upper, result, abserr, defabs, resabs);
                    break;

                case 4:
                    dqk41(lower, upper, result, abserr, defabs, resabs);
                    break;

                case 5:
                    dqk51(lower, upper, result, abserr, defabs, resabs);
                    break;

                case 6:
                    dqk61(lower, upper, result, abserr, defabs, resabs);
                    break;
            } // switch (keyf)

            last = 1;
            rlist[0] = (DoubleDouble)result[0].clone();
            elist[0] = (DoubleDouble)abserr[0].clone();
            iord[0] = 0;

            // Test on accuracy

            errBnd = epsabs.max(epsrel.multiply(result[0].abs()));

            if ((abserr[0].le(((DoubleDouble.valueOf(50.0)).multiply(epmach)).multiply(defabs[0]))) && (abserr[0].gt(errBnd))) {
                errorStatus = 2;
            } // if ((abserr[0] <= 50.0 * epmach * defabs[0]) &&

            if (limit == 1) {
                errorStatus = 1;
            } // if (limit == 1)

            if ((errorStatus != 0) || ((abserr[0].le(errBnd)) && (abserr[0].ne(resabs[0]))) || (abserr[0].isZero())) {

                if (keyf != 1) {
                    neval = ((10 * keyf) + 1) * ((2 * neval) + 1);
                } // if (keyf != 1)
                else {
                    neval = (30 * neval) + 15;
                } // else

                return;
            } // if ((errorStatus != 0) ||

            // Initialization

            errMax[0] = (DoubleDouble)abserr[0].clone();
            maxErr[0] = 0;
            area = (DoubleDouble)result[0].clone();
            errSum = (DoubleDouble)abserr[0].clone();
            nrmax[0] = 1;
            iroff1 = 0;
            iroff2 = 0;

            // Main do-loop

            for (last = 2; last <= limit; last++) {

                // Bisect the subinterval with the largest error estimate.

                a1 = (DoubleDouble)alist[maxErr[0]].clone();
                b1 = (DoubleDouble.valueOf(0.5)).multiply(alist[maxErr[0]].add(blist[maxErr[0]]));
                a2 = (DoubleDouble)b1.clone();
                b2 = (DoubleDouble)blist[maxErr[0]].clone();

                switch (keyf) {

                    case 1:
                        dqk15(a1, b1, area1, error1, resabs, defab1);
                        dqk15(a2, b2, area2, error2, resabs, defab2);
                        break;

                    case 2:
                        dqk21(a1, b1, area1, error1, resabs, defab1);
                        dqk21(a2, b2, area2, error2, resabs, defab2);
                        break;

                    case 3:
                        dqk31(a1, b1, area1, error1, resabs, defab1);
                        dqk31(a2, b2, area2, error2, resabs, defab2);
                        break;

                    case 4:
                        dqk41(a1, b1, area1, error1, resabs, defab1);
                        dqk41(a2, b2, area2, error2, resabs, defab2);
                        break;

                    case 5:
                        dqk51(a1, b1, area1, error1, resabs, defab1);
                        dqk51(a2, b2, area2, error2, resabs, defab2);
                        break;

                    case 6:
                        dqk61(a1, b1, area1, error1, resabs, defab1);
                        dqk61(a2, b2, area2, error2, resabs, defab2);
                        break;
                } // switch (keyf)

                // Improve previous approximations to integral and error
                // and test for accuracy.

                neval = neval + 1;
                area12 = area1[0].add(area2[0]);
                error12 = error1[0].add(error2[0]);
                errSum = (errSum.add(error12)).subtract(errMax[0]);
                area = (area.add(area12)).subtract(rlist[maxErr[0]]);

                if ((defab1[0].ne(error1[0])) && (defab2[0].ne(error2[0]))) {

                    if ((((rlist[maxErr[0]].subtract(area12)).abs()).le((DoubleDouble.valueOf(1.0E-5)).multiply(area12.abs()))) &&
                            (error12.ge((DoubleDouble.valueOf(0.99)).multiply(errMax[0])))) {
                        iroff1 = iroff1 + 1;
                    } // if ((Math.abs(rlist[maxErr[0]] - area12) <=

                    if ((last > 10) && (error12.gt(errMax[0]))) {
                        iroff2 = iroff2 + 1;
                    } // if ((last > 10) && (error12 > errMax[0]))
                } // if ((defab1[0] != error1[0]) && (defab2[0] != error2[0]))

                rlist[maxErr[0]] = (DoubleDouble)area1[0].clone();
                rlist[last - 1] = (DoubleDouble)area2[0].clone();
                errBnd = epsabs.max(epsrel.multiply(area.abs()));

                if (errSum.gt(errBnd)) {

                    // Test for roundoff error and eventually set error flag

                    if ((iroff1 >= 6) || (iroff2 >= 20)) {
                        errorStatus = 2;
                    } // if ((iroff1 >= 6) || (iroff2 >= 20))

                    // Set error flag in the case that the number of subintervals
                    // equals limit.

                    if (last == limit) {
                        errorStatus = 1;
                    } // if (last == limit)

                    // Set error flag in the case of bad integrand behavior
                    // at a point of the integration range.

                    DoubleDouble maxVal = (a1.abs()).max(b2.abs());
                    DoubleDouble leftVal = (DoubleDouble.valueOf(1.0)).add((DoubleDouble.valueOf(100.0)).multiply(epmach));
                    DoubleDouble rightVal = (a2.abs()).add((DoubleDouble.valueOf(1.0E3)).multiply(uflow));
                    if (maxVal.le(leftVal.multiply(rightVal))) {
                        errorStatus = 3;
                    } // if (Math.max(Math.abs(a1), Math.abs(b2)) <=
                } // if (errSum > errBnd)

                // Append the newly created intervals to the list

                if (error2[0].le(error1[0])) {
                    alist[last - 1] = (DoubleDouble)a2.clone();
                    blist[maxErr[0]] = (DoubleDouble)b1.clone();
                    blist[last - 1] = (DoubleDouble)b2.clone();
                    elist[maxErr[0]] = (DoubleDouble)error1[0].clone();
                    elist[last - 1] = (DoubleDouble)error2[0].clone();
                } // if (error2[0] <= error1[0])
                else {
                    alist[maxErr[0]] = (DoubleDouble)a2.clone();
                    alist[last - 1] = (DoubleDouble)a1.clone();
                    blist[last - 1] = (DoubleDouble)b1.clone();
                    rlist[maxErr[0]] = (DoubleDouble)area2[0].clone();
                    rlist[last - 1] = (DoubleDouble)area1[0].clone();
                    elist[maxErr[0]] = (DoubleDouble)error2[0].clone();
                    elist[last - 1] = (DoubleDouble)error1[0].clone();
                } // else

                // Call subroutine dqpsrt to maintain the descending ordering
                // in the list of error estimates and select the subinterval
                // with the largest error estimate (to be bisected next).

                dqpsrt(limit, last, maxErr, errMax, elist, iord, nrmax);

                if ((errorStatus != 0) || (errSum.le(errBnd))) {
                    break;
                } // if (errorStatus != 0) || (errSum <= errBnd))
            } // for (last = 2; last <= limit; last++)

            // Compute final result.

            result[0] = DoubleDouble.valueOf(0.0);

            for (k = 0; k < last; k++) {
                result[0] = result[0].add(rlist[k]);
            } // for (k = 0; k < last; k++)

            abserr[0] = (DoubleDouble)errSum.clone();

            if (keyf != 1) {
                neval = ((10 * keyf) + 1) * ((2 * neval) + 1);
            } // if (keyf != 1)
            else {
                neval = (30 * neval) + 15;
            } // else

            return;
        } // try
        catch (Exception err) {
            Preferences.debug("dqage error: " + err.getMessage());
        }

    }

    /**
     * This is a port of the original FORTRAN routine whose header is given below:
     * c***begin prologue  dqagse
	 c***date written   800101   (yymmdd)
	 c***revision date  830518   (yymmdd)
	 c***category no.  h2a1a1
	 c***keywords  automatic integrator, general-purpose,
	 c             (end point) singularities, extrapolation,
	 c             globally adaptive
	 c***author  piessens,robert,appl. math. & progr. div. - k.u.leuven
	 c           de doncker,elise,appl. math. & progr. div. - k.u.leuven
	 c***purpose  the routine calculates an approximation result to a given
	 c            definite integral i = integral of f over (a,b),
	 c            hopefully satisfying following claim for accuracy
	 c            abs(i-result).le.max(epsabs,epsrel*abs(i)).
	 c***description
	 c
	 c        computation of a definite integral
	 c        standard fortran subroutine
	 c        DoubleDouble precision version

     * This routine calculates the integral of intFunc over (lower, upper) and can handle end-point singularities at
     * lower and upper. It tries to satisfy the claim for accuracy abs(actual integral - result) <= max(epsabs, esprel *
     * abs(actual integral))
     */
    private void dqagse() {

        // Variables ending in 1 denote left subinterval and variables
        // ending in 2 denote right subinterval
        // rlist2 should be length at least (limexp + 2)
        // The value of limexp is determined in subroutine dqelg
        // rlist2 contains the part of the epsilon table still needed for
        // further computations
        DoubleDouble[] rlist2;

        // Index to the interval with the largest error estimate
        int[] maxErr = new int[1];

        // errMax[0] = elist[maxErr];
        DoubleDouble[] errMax = new DoubleDouble[1];

        // error on the interval currently subdivided (before that subdivision
        // has taken place)
        DoubleDouble erlast;

        // Sum of the integrals over the subintervals
        DoubleDouble area;

        // Sum of the errors over the subintervals
        DoubleDouble errSum;

        // Requested accuracy max(epsabs, epsrel * abs(result))
        DoubleDouble errBnd;

        // Number of calls to the extrapolation routine
        int[] nres = new int[1];

        // Number of elements in rlist2.  If an appropriate approximation
        // to the compounded integral has been obtained, it is put in
        // rlist2[numr12-1] after numr12 has been increased by one.
        int[] numr12 = new int[1];

        // Sum of the errors over the intervals larger than the smallest
        // interval considered up to now
        DoubleDouble erlarg;

        // boolean variable denoting that the routine is attempting to
        // perform extrapolation.  That is, before dividing the
        // interval we try to decrease the value of erlarg
        boolean extrap;

        // If noext is true, the extrapolation is no longer allowed.
        boolean noext;

        // length of the smallest interval considered up to now, multiplied
        // by 1.5
        DoubleDouble small;
        DoubleDouble[] res31a;
        DoubleDouble[] abseps = new DoubleDouble[1];
        DoubleDouble[] area1 = new DoubleDouble[1];
        DoubleDouble area12;
        DoubleDouble[] area2 = new DoubleDouble[1];
        DoubleDouble a1;
        DoubleDouble a2;
        DoubleDouble b1;
        DoubleDouble b2;
        DoubleDouble correc;
        DoubleDouble[] defabs = new DoubleDouble[1];
        DoubleDouble[] defab1 = new DoubleDouble[1];
        DoubleDouble[] defab2 = new DoubleDouble[1];
        DoubleDouble dres;
        DoubleDouble[] error1 = new DoubleDouble[1];
        DoubleDouble[] error2 = new DoubleDouble[1];
        DoubleDouble error12;
        DoubleDouble ertest;
        DoubleDouble[] resabs = new DoubleDouble[1];
        DoubleDouble[] reseps = new DoubleDouble[1];
        int id;
        int ierro;
        int iroff1;
        int iroff2;
        int iroff3;
        int jupbnd;
        int k;
        int ksgn;
        int ktmin;
        int[] nrmax = new int[1];


        try {
            alist = new DoubleDouble[limit];
            blist = new DoubleDouble[limit];
            rlist = new DoubleDouble[limit];
            elist = new DoubleDouble[limit];
            iord = new int[limit];
            rlist2 = new DoubleDouble[52];
            res31a = new DoubleDouble[3];

            errorStatus = 0;
            neval = 0;
            last = 0;
            result[0] = DoubleDouble.valueOf(0.0);
            abserr[0] = DoubleDouble.valueOf(0.0);
            alist[0] = (DoubleDouble)lower.clone();
            blist[0] = (DoubleDouble)upper.clone();
            rlist[0] = DoubleDouble.valueOf(0.0);
            elist[0] = DoubleDouble.valueOf(0.0);

            // First approximation to the integral

            ierro = 0;
            dqk21(lower, upper, result, abserr, defabs, resabs);

            // Test on accuracy.

            dres = (result[0].abs());
            errBnd = epsabs.max(epsrel.multiply(dres));
            last = 1;
            rlist[0] = (DoubleDouble)result[0].clone();
            elist[0] = (DoubleDouble)abserr[0].clone();
            iord[0] = 0;

            if ((abserr[0].le(((DoubleDouble.valueOf(100.0)).multiply(epmach)).multiply(defabs[0]))) && (abserr[0].gt(errBnd))) {
                errorStatus = 2;
            } // if ((abserr[0] <= 100.0 * epmach * defabs[0]) &&

            if (limit == 1) {
                errorStatus = 1;
            } // if (limit == 1)

            if ((errorStatus != 0) || ((abserr[0].le(errBnd)) && (abserr[0].ne(resabs[0]))) || (abserr[0].isZero())) {
                neval = (42 * last) - 21;

                return;
            } // if ((errorStatus != 0) || ((abserr[0] <= errBnd) && (abserr[0] !=

            // Initialization

            rlist2[0] = (DoubleDouble)result[0].clone();
            errMax[0] = (DoubleDouble)abserr[0].clone();
            maxErr[0] = 0;
            area = (DoubleDouble)result[0].clone();
            errSum = (DoubleDouble)abserr[0].clone();
            erlarg = (DoubleDouble)errSum.clone();
            correc = (DoubleDouble)erlarg.clone();
            small = (DoubleDouble.valueOf(0.375)).multiply((upper.subtract(lower)).abs());
            ertest = (DoubleDouble)errBnd.clone();
            abserr[0] = (DoubleDouble)oflow.clone();
            nrmax[0] = 1;
            nres[0] = 0;
            numr12[0] = 2;
            ktmin = 0;
            extrap = false;
            noext = false;
            iroff1 = 0;
            iroff2 = 0;
            iroff3 = 0;
            ksgn = -1;

            if (dres.ge(((DoubleDouble.valueOf(1.0)).subtract((DoubleDouble.valueOf(50.0)).multiply(epmach))).multiply(defabs[0]))) {
                ksgn = 1;
            } // if (dres >= (1.0 - 50.0 * epmach)*defabs[0])

// Main do-loop
loop:
            for (last = 2; last <= limit; last++) {

                // Bisect the subinterval with the nrmax-th largest error
                // estimate.

                a1 = (DoubleDouble)alist[maxErr[0]].clone();
                b1 = (DoubleDouble.valueOf(0.5)).multiply(alist[maxErr[0]].add(blist[maxErr[0]]));
                a2 = (DoubleDouble)b1.clone();
                b2 = (DoubleDouble)blist[maxErr[0]].clone();
                erlast = (DoubleDouble)errMax[0].clone();
                dqk21(a1, b1, area1, error1, resabs, defab1);
                dqk21(a2, b2, area2, error2, resabs, defab2);

                // Improve previous approximations to integral and error
                // and test for accuracy

                area12 = area1[0].add(area2[0]);
                error12 = error1[0].add(error2[0]);
                errSum = (errSum.add(error12)).subtract(errMax[0]);
                area = (area.add(area12)).subtract(rlist[maxErr[0]]);

                if ((defab1[0].ne(error1[0])) && (defab2[0].ne(error2[0]))) {

                    if ((((rlist[maxErr[0]].subtract(area12)).abs()).le((DoubleDouble.valueOf(1.0E-5)).multiply(area12.abs()))) &&
                            (error12.ge((DoubleDouble.valueOf(0.99)).multiply(errMax[0])))) {

                        if (extrap) {
                            iroff2 = iroff2 + 1;
                        } // if (extrap)
                        else {
                            iroff1 = iroff1 + 1;
                        }
                    } // if ((Math.abs(rlist[maxErr[0]] - area12) <=

                    if ((last > 10) && (error12.gt(errMax[0]))) {
                        iroff3 = iroff3 + 1;
                    } // if ((last > 10) && (error12 > errMax[0]))
                } // if ((defab1[0] != error1[0]) && (defab2[0] != error2[0]))

                rlist[maxErr[0]] = (DoubleDouble)area1[0].clone();
                rlist[last - 1] = (DoubleDouble)area2[0].clone();
                errBnd = epsabs.max(epsrel.multiply(area.abs()));

                // Test for roundoff error and eventually set error flag.

                if (((iroff1 + iroff2) >= 10) || (iroff3 >= 20)) {
                    errorStatus = 2;
                } // if (((iroff1 + iroff2) >= 10) || (iroff3 >= 20))

                if (iroff2 >= 5) {
                    ierro = 3;
                } // if (iroff2 >= 5)

                // Set error flag in the case that the number of subintervals
                // equals limit.

                if (last == limit) {
                    errorStatus = 1;
                } // if (last == limit)

                // Set error flag in the case of bad integrand behavior
                // at a point of the integration range.

                DoubleDouble maxVal = (a1.abs()).max(b2.abs());
                DoubleDouble leftVal = (DoubleDouble.valueOf(1.0)).add((DoubleDouble.valueOf(100.0)).multiply(epmach));
                DoubleDouble rightVal = (a2.abs()).add((DoubleDouble.valueOf(1.0E3)).multiply(uflow));
                if (maxVal.le(leftVal.multiply(rightVal))) {
                    errorStatus = 4;
                } // if (Math.max(Math.abs(a1), Math.abs(b2)) <=

                // Append the newly-created intervals to the list.

                if (error2[0].le(error1[0])) {
                    alist[last - 1] = (DoubleDouble)a2.clone();
                    blist[maxErr[0]] = (DoubleDouble)b1.clone();
                    blist[last - 1] = (DoubleDouble)b2.clone();
                    elist[maxErr[0]] = (DoubleDouble)error1[0].clone();
                    elist[last - 1] = (DoubleDouble)error2[0].clone();
                } // if (error2[0] <= error1[0])
                else {
                    alist[maxErr[0]] = (DoubleDouble)a2.clone();
                    alist[last - 1] = (DoubleDouble)a1.clone();
                    blist[last - 1] = (DoubleDouble)b1.clone();
                    rlist[maxErr[0]] = (DoubleDouble)area2[0].clone();
                    rlist[last - 1] = (DoubleDouble)area1[0].clone();
                    elist[maxErr[0]] = (DoubleDouble)error2[0].clone();
                    elist[last - 1] = (DoubleDouble)error1[0].clone();
                } // else

                // Call subroutine dqpsrt to maintain the descending ordering
                // in the list of error estimates and select the subinterval
                // with nrmax-th largest error estimate (to be bisected next).

                dqpsrt(limit, last, maxErr, errMax, elist, iord, nrmax);

                if (errSum.le(errBnd)) {
                    result[0] = DoubleDouble.valueOf(0.0);

                    for (k = 0; k < last; k++) {
                        result[0] = result[0].add(rlist[k]);
                    } // for (k = 0; k < last; k++)

                    abserr[0] = (DoubleDouble)errSum.clone();

                    if (errorStatus > 2) {
                        errorStatus = errorStatus - 1;
                    } // if (errorStatus > 2)

                    neval = (42 * last) - 21;

                    return;
                } // if (errSum <= errBnd)

                if (errorStatus != 0) {
                    break;
                } // if (errorStatus != 0)

                if (last == 2) {
                    small = (DoubleDouble.valueOf(0.375)).multiply((upper.subtract(lower)).abs());
                    erlarg = (DoubleDouble)errSum.clone();
                    ertest = (DoubleDouble)errBnd.clone();
                    rlist2[1] = (DoubleDouble)area.clone();

                    continue;
                } // if (last == 2)

                if (noext) {
                    continue;
                } // if (noext)

                erlarg = erlarg.subtract(erlast);

                if (((b1.subtract(a1)).abs()).gt(small)) {
                    erlarg = erlarg.add(error12);
                } // if (Math.abs(b1 - a1) > small)

                if (!extrap) {

                    // Test whether the interval to be bisected next is the
                    // smallest interval.

                    if (((blist[maxErr[0]].subtract(alist[maxErr[0]])).abs()).gt(small)) {
                        continue;
                    } // if (Math.abs(blist[maxErr[0]] - alist[maxErr[0]]) > small)

                    extrap = true;
                    nrmax[0] = 2;
                } // if (!extrap)

                if ((ierro != 3) && (erlarg.gt(ertest))) {

                    // The smallest interval has the largest error.
                    // Before bisecting decrease the sum of the errors over the
                    // larger intervals (erlarg) and perform extrapolation.

                    id = nrmax[0];
                    jupbnd = last;

                    if (last > (2 + (limit / 2))) {
                        jupbnd = limit + 3 - last;
                    } // if (last > (2 + limit/2))

                    for (k = id; k <= jupbnd; k++) {
                        maxErr[0] = iord[nrmax[0] - 1];
                        errMax[0] = (DoubleDouble)elist[maxErr[0]].clone();

                        if (((blist[maxErr[0]].subtract(alist[maxErr[0]])).abs()).gt(small)) {
                            continue loop;
                        } // if (Math.abs(blist[maxErr[0]] - alist[maxErr[0]]) > small)

                        nrmax[0] = nrmax[0] + 1;
                    } // for (k = id; k <= jupbnd; k++)
                } // if ((ierro != 3) && (erlarg > ertest))

                // Perform extrapolation

                numr12[0] = numr12[0] + 1;
                rlist2[numr12[0] - 1] = (DoubleDouble)area.clone();
                dqelg(numr12, rlist2, reseps, abseps, res31a, nres);
                ktmin = ktmin + 1;

                if ((ktmin > 5) && (abserr[0].lt((DoubleDouble.valueOf(1.0E-3)).multiply(errSum)))) {
                    errorStatus = 5;
                } // if ((ktmin > 5) && (abserr[0] < 1.0E-3 * errSum))

                if (abseps[0].lt(abserr[0])) {
                    ktmin = 0;
                    abserr[0] = (DoubleDouble)abseps[0].clone();
                    result[0] = (DoubleDouble)reseps[0].clone();
                    correc = (DoubleDouble)erlarg.clone();
                    ertest = epsabs.max(epsrel.multiply(reseps[0].abs()));

                    if (abserr[0].le(ertest)) {
                        break;
                    } // if (abserr[0] <= ertest)
                } // if (abseps[0] < abserr[0])

                // Prepare bisection of the smallest interval

                if (numr12[0] == 1) {
                    noext = true;
                } // if (numr12[0] == 1)

                if (errorStatus == 5) {
                    break;
                } // if (errorStatus == 5)

                maxErr[0] = iord[0];
                errMax[0] = (DoubleDouble)elist[maxErr[0]].clone();
                nrmax[0] = 1;
                extrap = false;
                small = (DoubleDouble.valueOf(0.5)).multiply(small);
                erlarg = (DoubleDouble)errSum.clone();
            } // for (last = 2; last <= limit; last++)

            // Set final result and error estimate

            if (abserr[0].ne(oflow)) {

                if ((errorStatus + ierro) != 0) {

                    if (ierro == 3) {
                        abserr[0] = abserr[0].add(correc);
                    } // if (ierro == 3)

                    if (errorStatus == 0) {
                        errorStatus = 3;
                    } // if (errorStatus == 0)

                    if ((result[0].isZero()) || (area.isZero())) {

                        if (abserr[0].gt(errSum)) {
                            result[0] = DoubleDouble.valueOf(0.0);

                            for (k = 0; k < last; k++) {
                                result[0] = result[0].add(rlist[k]);
                            } // for (k = 0; k < last; k++)

                            abserr[0] = (DoubleDouble)errSum.clone();

                            if (errorStatus > 2) {
                                errorStatus = errorStatus - 1;
                            } // if (errorStatus > 2)

                            neval = (42 * last) - 21;

                            return;
                        } // if (abserr[0] > errSum)

                        if (area.isZero()) {

                            if (errorStatus > 2) {
                                errorStatus = errorStatus - 1;
                            } // if (errorStatus > 2)

                            neval = (42 * last) - 21;

                            return;
                        } // if (area.isZero())

                        if ((ksgn == -1) && ((result[0].abs()).max( (area.abs())).le((DoubleDouble.valueOf(1.0E-2)).multiply(defabs[0])))) {

                            if (errorStatus > 2) {
                                errorStatus = errorStatus - 1;
                            } // if (errorStatus > 2)

                            neval = (42 * last) - 21;

                            return;
                        } // if ((ksgn == -1) && (Math.max(Math.abs(result[0]),

                        if (((DoubleDouble.valueOf(1.0E-2)).gt(result[0].divide(area))) || ((result[0].divide(area)).gt(DoubleDouble.valueOf(100.0))) ||
                                (errSum.gt(area.abs()))) {
                            errorStatus = 6;
                        } // if ((1.0E-2 > (result[0]/area)) || (result[0]/area > 100.0) ||

                        if (errorStatus > 2) {
                            errorStatus = errorStatus - 1;
                        } // if (errorStatus > 2)

                        neval = (42 * last) - 21;

                        return;
                    } // if ((result[0].isZero()0 || (area.isZero()))

                    if ((abserr[0].divide(result[0].abs())).gt(errSum.divide(area.abs()))) {
                        result[0] = DoubleDouble.valueOf(0.0);

                        for (k = 0; k < last; k++) {
                            result[0] = result[0].add(rlist[k]);
                        } // for (k = 0; k < last; k++)

                        abserr[0] = (DoubleDouble)errSum.clone();

                        if (errorStatus > 2) {
                            errorStatus = errorStatus - 1;
                        } // if (errorStatus > 2)

                        neval = (42 * last) - 21;

                        return;
                    } // if (abserr[0]/Math.abs(result[0]) > errSum/Math.abs(area))
                } // if ((errorStatus + ierro) != 0)

                if ((ksgn == -1) && ((result[0].abs()).max( (area.abs())).le((DoubleDouble.valueOf(1.0E-2)).multiply(defabs[0])))) {

                    if (errorStatus > 2) {
                        errorStatus = errorStatus - 1;
                    } // if (errorStatus > 2)

                    neval = (42 * last) - 21;

                    return;
                } // if ((ksgn == -1) && (Math.max(Math.abs(result[0]),

                if (((DoubleDouble.valueOf(1.0E-2)).gt(result[0].divide(area))) || ((result[0].divide(area)).gt(DoubleDouble.valueOf(100.0))) ||
                		(errSum.gt(area.abs()))) {
                    errorStatus = 6;
                } // if ((1.0E-2 > (result[0]/area)) || (result[0]/area > 100.0) ||

                if (errorStatus > 2) {
                    errorStatus = errorStatus - 1;
                } // if (errorStatus > 2)

                neval = (42 * last) - 21;

                return;
            } // if (abserr[0] != oflow)

            result[0] = DoubleDouble.valueOf(0.0);

            for (k = 0; k < last; k++) {
                result[0] = result[0].add(rlist[k]);
            } // for (k = 0; k < last; k++)

            abserr[0] = (DoubleDouble)errSum.clone();

            if (errorStatus > 2) {
                errorStatus = errorStatus - 1;
            } // if (errorStatus > 2)

            neval = (42 * last) - 21;

            return;
        } // try
        catch (Exception err) {
            Preferences.debug("dqagse error: " + err.getMessage());
        }

    }

    /**
     * This is a port of the original FORTRAN whose header is given below:
     * c***begin prologue  dqelg
	 c***refer to  dqagie,dqagoe,dqagpe,dqagse
	 c***routines called  d1mach
	 c***revision date  830518   (yymmdd)
	 c***keywords  epsilon algorithm, convergence acceleration,
	 c             extrapolation
	 c***author  piessens,robert,appl. math. & progr. div. - k.u.leuven
	 c           de doncker,elise,appl. math & progr. div. - k.u.leuven
	 c***purpose  the routine determines the limit of a given sequence of
	 c            approximations, by means of the epsilon algorithm of
	 c            p.wynn. an estimate of the absolute error is also given.
	 c            the condensed epsilon table is computed. only those
	 c            elements needed for the computation of the next diagonal
	 c            are preserved.
	 c***description
	 c
	 c           epsilon algorithm
	 c           standard fortran subroutine
	 c           DoubleDouble precision version

     * This routine determines the limit of a given sequence of approximations, by means of the epsilon algorithm of P.
     * Wynn. An estimate of the absolute error is also given. The condensed epsilon table is computed. Only those
     * elements needed for the computation of the next diagonal are preserved.
     *
     * @param  n       epstab[n[0]-1] contains the new element in the first column of the epsilon table
     * @param  epstab  52 element array containing the elements of the two lower diagonals of the triangular epsilon
     *                 table. The elements are numbered starting at the right-hand corner of the triangle
     * @param  result  resulting approximation to the integral The element in the new diagonal with the least value of
     *                 error.
     * @param  abserr  estimate of the absolute error computed from result and the 3 previous results
     * @param  res31a  array containing the last 3 results
     * @param  nres    number of calls to the routine (should be zero at first call)
     */
    private void dqelg(int[] n, DoubleDouble[] epstab, DoubleDouble[] result, DoubleDouble[] abserr, DoubleDouble[] res31a, int[] nres) {
        DoubleDouble delta1;
        DoubleDouble delta2;
        DoubleDouble delta3;
        DoubleDouble epsinf;
        DoubleDouble esptab;

        // error = abs(e1-e0) + abs(e2-e1) + abs(new-e2)
        DoubleDouble error;
        DoubleDouble err1;
        DoubleDouble err2;
        DoubleDouble err3;

        // The 4 elements on which the computation of a new element in the
        // epsilon table is based
        DoubleDouble e0;
        DoubleDouble e1;
        DoubleDouble e2;
        DoubleDouble e3;
        DoubleDouble e1abs;
        DoubleDouble res;
        DoubleDouble ss = DoubleDouble.valueOf(0.0);
        DoubleDouble tol1;
        DoubleDouble tol2;
        DoubleDouble tol3;
        int i;
        int ib;
        int ib2;
        int ie;
        int indx;
        int k1;
        int k2;
        int k3;

        // The maximum number of elements the epsilon table can contain.
        // If this number is reached, the upper diagonal of the epsilon
        // table is deleted.
        int limexp;

        // number of elements to be computed in the new diagonal
        int newelm;
        int num;
        boolean doSeg = true;

        nres[0] = nres[0] + 1;
        abserr[0] = (DoubleDouble)oflow.clone();
        result[0] = (DoubleDouble)epstab[n[0] - 1].clone();

        if (n[0] < 3) {
            abserr[0] = abserr[0].max( ((DoubleDouble.valueOf(5.0)).multiply(epmach)).multiply(result[0].abs()));

            return;
        } // if (n[0] < 3)

        limexp = 50;
        epstab[n[0] + 1] = (DoubleDouble)epstab[n[0] - 1].clone();
        newelm = (n[0] - 1) / 2;
        epstab[n[0] - 1] = (DoubleDouble)oflow.clone();
        num = n[0];
        k1 = n[0];

        for (i = 1; i <= newelm; i++) {
            k2 = k1 - 1;
            k3 = k1 - 2;
            res = (DoubleDouble)epstab[k1 + 1].clone();
            e0 = (DoubleDouble)epstab[k3 - 1].clone();
            e1 = (DoubleDouble)epstab[k2 - 1].clone();
            e2 = (DoubleDouble)res.clone();
            e1abs = (e1.abs());
            delta2 = e2.subtract(e1);
            err2 = delta2.abs();
            tol2 = ((e2.abs()).max( e1abs)).multiply(epmach);
            delta3 = e1.subtract(e0);
            err3 = delta3.abs();
            tol3 = (e1abs.max(e0.abs())).multiply(epmach);

            if ((err2.le(tol2)) && (err3.le(tol3))) {

                // if e0, e1, and e2 are equal to within machine accuracy,
                // convergence is assumed.
                result[0] = (DoubleDouble)res.clone();
                abserr[0] = err2.add(err3);
                abserr[0] = abserr[0].max( ((DoubleDouble.valueOf(5.0)).multiply(epmach)).multiply(result[0].abs()));

                return;
            } // if ((err2 <= tol2) && (err3 <= tol3))

            e3 = (DoubleDouble)epstab[k1 - 1].clone();
            epstab[k1 - 1] = (DoubleDouble)e1.clone();
            delta1 = e1.subtract(e3);
            err1 = delta1.abs();
            tol1 = (e1abs.max(e3.abs())).multiply(epmach);

            // If two elements are very close to each other, omit a part of
            // the table by adjusting the value of n

            if ((err1.gt(tol1)) && (err2.gt(tol2)) && (err3.gt(tol3))) {
                ss = ((delta1.reciprocal()).add(delta2.reciprocal())).subtract(delta2.reciprocal());
                epsinf = (ss.multiply(e1)).abs();

                // Test to detect irregular behavior in the table, and
                // eventually omit a part of the table adjusting the value of n.

                if (epsinf.gt(DoubleDouble.valueOf(1.0E-4))) {
                    doSeg = false;
                }
            } // ((err1 > tol1) && (err2 > tol2) && (err3 > tol3))

            if (doSeg) {
                n[0] = i + i - 1;

                break;
            } // if (doSeg)

            // Compute a new element and eventually adjust the value of result

            res = e1.add(ss.reciprocal());
            epstab[k1 - 1] = (DoubleDouble)res.clone();
            k1 = k1 - 2;
            error = (err2.add((res.subtract(e2)).abs())).add(err3);

            if (error.gt(abserr[0])) {
                continue;
            } // if (error > abserr[0])

            abserr[0] = (DoubleDouble)error.clone();
            result[0] = (DoubleDouble)res.clone();
        } // for (i = 1; i <= newelm; i++)

        // Shift the table
        if (n[0] == limexp) {
            n[0] = (2 * (limexp / 2)) - 1;
        } // if (n[0] == limexp)

        ib = 1;

        if (((num / 2) * 2) == num) {
            ib = 2;
        } // if ((num/2)*2 == num)

        ie = newelm + 1;

        for (i = 1; i <= ie; i++) {
            ib2 = ib + 2;
            epstab[ib - 1] = (DoubleDouble)epstab[ib2 - 1].clone();
            ib = ib2;
        } // for (i = 1; i <= ie; i++)

        if (num != n[0]) {
            indx = num - n[0];

            for (i = 0; i < n[0]; i++) {
                epstab[i] = (DoubleDouble)epstab[indx].clone();
                indx = indx + 1;
            } // for (i = 0; i < n[0]; i++)
        } // if (num != n[0])

        if (nres[0] < 4) {
            res31a[nres[0] - 1] = (DoubleDouble)result[0].clone();
            abserr[0] = (DoubleDouble)oflow.clone();
            abserr[0] = abserr[0].max( ((DoubleDouble.valueOf(5.0)).multiply(epmach)).multiply(result[0].abs()));

            return;
        } // if (nres[0] < 4)

        // Compute error estimate

        abserr[0] = (((result[0].subtract(res31a[2])).abs()).add((result[0].subtract(res31a[1])).abs())).add((result[0].subtract(res31a[0])).abs());
        res31a[0] = (DoubleDouble)res31a[1].clone();
        res31a[1] = (DoubleDouble)res31a[2].clone();
        res31a[2] = (DoubleDouble)result[0].clone();
        abserr[0] = abserr[0].max( ((DoubleDouble.valueOf(5.0)).multiply(epmach)).multiply(result[0].abs()));

        return;
    }

    /**
     * The is a port of the original FORTRAN whose header is given below:
     * c***begin prologue  dqk15
	 c***date written   800101   (yymmdd)
	 c***revision date  830518   (yymmdd)
	 c***category no.  h2a1a2
	 c***keywords  15-point gauss-kronrod rules
	 c***author  piessens,robert,appl. math. & progr. div. - k.u.leuven
	 c           de doncker,elise,appl. math. & progr. div - k.u.leuven
	 c***purpose  to compute i = integral of f over (a,b), with error
	 c                           estimate
	 c                       j = integral of abs(f) over (a,b)
	 c***description
	 c
	 c           integration rules
	 c           standard fortran subroutine
	 c           DoubleDouble precision version

     * This routine computes theintegral of the intFunc over (a, b) and the integral of the abs(intFunc) over (a, b).
     *
     * @param  a       lower limit for integration
     * @param  b       upper limit for integration
     * @param  result  Approximation to the integral. The result is computed by applying the 15-point kronrod rule
     *                 (resk) obtained by optimal addition of abscissae to the 7-point gauss rule (resg).
     * @param  abserr  Estimate of the modulus of the absolute error, which should equal or exceed abs(integral of
     *                 intFunc over (a,b) - result)
     * @param  resabs  Approximation to integral of abs(intFunc)
     * @param  resasc  Approximation to the integral of abs(intFunc - integral of intFunc/(b - a)) over (a, b)
     */
    private void dqk15(DoubleDouble a, DoubleDouble b, DoubleDouble[] result, DoubleDouble[] abserr, DoubleDouble[] resabs, DoubleDouble[] resasc) {
        // The abscissae and weights are supplied for the interval (-1, 1).
        // Because of symmetry only the positive abscissae and their
        // corresponding weights are given.

        // xgk - abscissae of the 15-point kronrod rule
        // xgk[1], xgk[3], ... abscissae of the 7-point gauss rule
        // xgk[0], xgk[2], ... abscissae which are optimally added to the
        // 7-point gauss rule.
        DoubleDouble[] xgk = new DoubleDouble[] {
                           DoubleDouble.valueOf(0.991455371120812639206854697526329),
                           DoubleDouble.valueOf(0.949107912342758524526189684047851),
                           DoubleDouble.valueOf(0.864864423359769072789712788640926),
                           DoubleDouble.valueOf(0.741531185599394439863864773280788),
                           DoubleDouble.valueOf(0.586087235467691130294144838258730),
                           DoubleDouble.valueOf(0.405845151377397166906606412076961),
                           DoubleDouble.valueOf(0.207784955007898467600689403773245),
                           DoubleDouble.valueOf(0.000000000000000000000000000000000)
                       };

        // wgk - weights of the 15-point kronrod rule
        DoubleDouble[] wgk = new DoubleDouble[] {
                           DoubleDouble.valueOf(0.022935322010529224963732008058970),
                           DoubleDouble.valueOf(0.063092092629978553290700663189204),
                           DoubleDouble.valueOf(0.104790010322250183839876322541518),
                           DoubleDouble.valueOf(0.140653259715525918745189590510238),
                           DoubleDouble.valueOf(0.169004726639267902826583426598550),
                           DoubleDouble.valueOf(0.190350578064785409913256402421014),
                           DoubleDouble.valueOf(0.204432940075298892414161999234649),
                           DoubleDouble.valueOf(0.209482141084727828012999174891714)
                       };

        // wg - weights of the 7-point gauss rule, corresponding to the
        // abscissae xgk[1], xgk[3], ...
        // wg[0], wg[2], ... are set to zero.
        DoubleDouble[] wg = new DoubleDouble[] {
                          DoubleDouble.valueOf(0.0),
                          DoubleDouble.valueOf(0.129484966168869693270611432679082),
                          DoubleDouble.valueOf(0.0),
                          DoubleDouble.valueOf(0.279705391489276667901467771423780),
                          DoubleDouble.valueOf(0.0),
                          DoubleDouble.valueOf(0.381830050505118944950369775488975),
                          DoubleDouble.valueOf(0.0),
                          DoubleDouble.valueOf(0.417959183673469387755102040816327)
                      };

        // abscissa
        DoubleDouble absc;

        // mid point of the interval
        DoubleDouble centr;
        DoubleDouble dhlgth;
        DoubleDouble fc;
        DoubleDouble fsum;

        // function value
        DoubleDouble fval1;
        DoubleDouble fval2;
        DoubleDouble[] fv1 = new DoubleDouble[7];
        DoubleDouble[] fv2 = new DoubleDouble[7];

        // half-length of the interval
        DoubleDouble hlgth;

        // result of the 7-point gauss formula
        DoubleDouble resg;

        // result of the 15-point kronrod formula
        DoubleDouble resk;

        // approximation to the mean value of intFunc over
        // (a, b), i.e. to (integral of intFunc)/(b - a) over (a,b)
        DoubleDouble reskh;
        int j;
        int jtw;
        int jtwm1;

        centr = (DoubleDouble.valueOf(0.5)).multiply(a.add(b));
        hlgth = (DoubleDouble.valueOf(0.5)).multiply(b.subtract(a));
        dhlgth = hlgth.abs();

        // Compute the 15-point kronrod approximation to the integral, and
        // estimate the absolute error.

        if (selfTest) {
            fc = intFuncTest(centr);	
        }
        else {
            fc = intFunc(centr);
        }
        resg = fc.multiply(wg[3]);
        resk = fc.multiply(wgk[7]);
        resabs[0] = resk.abs();

        for (j = 0; j < 3; j++) {
            jtw = (2 * j) + 1;
            absc = hlgth.multiply(xgk[jtw]);
            if (selfTest) {
            	fval1 = intFuncTest(centr.subtract(absc));
                fval2 = intFuncTest(centr.add(absc));	
            }
            else {
                fval1 = intFunc(centr.subtract(absc));
                fval2 = intFunc(centr.add(absc));
            }
            fv1[jtw] = (DoubleDouble)fval1.clone();
            fv2[jtw] = (DoubleDouble)fval2.clone();
            fsum = fval1.add(fval2);
            resg = resg.add(wg[j].multiply(fsum));
            resk = resk.add(wgk[jtw].multiply(fsum));
            resabs[0] = resabs[0].add(wgk[jtw].multiply((fval1.abs()).add(fval2.abs())));
        } // for (j = 0; j < 3; j++)

        for (j = 0; j < 4; j++) {
            jtwm1 = 2 * j;
            absc = hlgth.multiply(xgk[jtwm1]);
            if (selfTest) {
            	fval1 = intFuncTest(centr.subtract(absc));
                fval2 = intFuncTest(centr.add(absc));	
            }
            else {
                fval1 = intFunc(centr.subtract(absc));
                fval2 = intFunc(centr.add(absc));
            }
            fv1[jtwm1] = (DoubleDouble)fval1.clone();
            fv2[jtwm1] = (DoubleDouble)fval2.clone();
            fsum = fval1.add(fval2);
            resk = resk.add(wgk[jtwm1].multiply(fsum));
            resabs[0] = resabs[0].add(wgk[jtwm1].multiply((fval1.abs()).add(fval2.abs())));
        } // for (j = 0; j < 4; j++)

        reskh = (DoubleDouble.valueOf(0.5)).multiply(resk);
        resasc[0] = wgk[7].multiply((fc.subtract(reskh)).abs());

        for (j = 0; j < 7; j++) {
            resasc[0] = resasc[0].add(wgk[j].multiply(((fv1[j].subtract(reskh)).abs()).add((fv2[j].subtract(reskh)).abs())));
        } // for (j = 0; j < 7; j++)

        result[0] = resk.multiply(hlgth);
        resabs[0] = resabs[0].multiply(dhlgth);
        resasc[0] = resasc[0].multiply(dhlgth);
        abserr[0] = ((resk.subtract(resg)).multiply(hlgth)).abs();

        if ((resasc[0].ne(DoubleDouble.valueOf(0.0))) && (abserr[0].ne(DoubleDouble.valueOf(0.0)))) {
        	DoubleDouble base = ((DoubleDouble.valueOf(200.0)).multiply(abserr[0])).divide(resasc[0]);
            abserr[0] = resasc[0].multiply((DoubleDouble.valueOf(1.0)).min(base.pow( 1.5)));
        }

        if (resabs[0].gt(uflow.divide((DoubleDouble.valueOf(50.0)).multiply(epmach)))) {
            abserr[0] = (((DoubleDouble.valueOf(50.0)).multiply(epmach)).multiply(resabs[0])).max( abserr[0]);
        }

        return;
    }

    /**
     * This is a port of the original FORTRAN whose header is given below:
     * c***begin prologue  dqk15i
	 c***date written   800101   (yymmdd)
	 c***revision date  830518   (yymmdd)
	 c***category no.  h2a3a2,h2a4a2
	 c***keywords  15-point transformed gauss-kronrod rules
	 c***author  piessens,robert,appl. math. & progr. div. - k.u.leuven
	 c           de doncker,elise,appl. math. & progr. div. - k.u.leuven
	 c***purpose  the original (infinite integration range is mapped
	 c            onto the interval (0,1) and (a,b) is a part of (0,1).
	 c            it is the purpose to compute
	 c            i = integral of transformed integrand over (a,b),
	 c            j = integral of abs(transformed integrand) over (a,b).
	 c***description
	 c
	 c           integration rule
	 c           standard fortran subroutine
	 c           DoubleDouble precision version

     * The original infinite integration range is mapped onto the interval (0, 1) and (a, b) is a part of (0, 1). This
     * routine computes the integral of the transformed integrand over (a, b) and the integral of the abs(transformed
     * integrand) over (a, b).
     *
     * @param  boun    finite bound of original integration range (set to zero if inf = +2)
     * @param  inf     If inf = -1, the original interval is (-infinity, bound) If inf = +1, the original interval is
     *                 (bound, +infinity) If inf = +2, the original interval is (-infinity, +infinity) and the integral
     *                 is computed as the sum of two integrals, one over (-infinity, 0) and one over (0, +infinity)
     * @param  a       lower limit for integration over subrange of (0, 1)
     * @param  b       upper limit for integration over subrange of (0, 1)
     * @param  result  Approximation to the integral. The result is computed by applying the 15-point kronrod rule
     *                 (resk) obtained by optimal addition of abscissae to the 7-point gauss rule (resg).
     * @param  abserr  Estimate of the modulus of the absolute error, which should equal or exceed abs(integral of
     *                 transformed integrand - result)
     * @param  resabs  Approximation to integral of abs(transformed integrand)
     * @param  resasc  Approximation to the integral of abs(transformed integrand - integral of transformed integrand/(b
     *                 - a)) over (a, b)
     */
    private void dqk15i(DoubleDouble boun, int inf, DoubleDouble a, DoubleDouble b, DoubleDouble[] result, DoubleDouble[] abserr, DoubleDouble[] resabs,
                        DoubleDouble[] resasc) {
        // The abscissae and weights are supplied for the interval (-1, 1).
        // Because of symmetry only the positive abscissae and their
        // corresponding weights are given.

        // xgk - abscissae of the 15-point kronrod rule
        // xgk[1], xgk[3], ... abscissae of the 7-point gauss rule
        // xgk[0], xgk[2], ... abscissae which are optimally added to the
        // 7-point gauss rule.
        DoubleDouble[] xgk = new DoubleDouble[] {
                           DoubleDouble.valueOf(0.991455371120812639206854697526329),
                           DoubleDouble.valueOf(0.949107912342758524526189684047851),
                           DoubleDouble.valueOf(0.864864423359769072789712788640926),
                           DoubleDouble.valueOf(0.741531185599394439863864773280788),
                           DoubleDouble.valueOf(0.586087235467691130294144838258730),
                           DoubleDouble.valueOf(0.405845151377397166906606412076961),
                           DoubleDouble.valueOf(0.207784955007898467600689403773245),
                           DoubleDouble.valueOf(0.000000000000000000000000000000000)
                       };

        // wgk - weights of the 15-point kronrod rule
        DoubleDouble[] wgk = new DoubleDouble[] {
                           DoubleDouble.valueOf(0.022935322010529224963732008058970),
                           DoubleDouble.valueOf(0.063092092629978553290700663189204),
                           DoubleDouble.valueOf(0.104790010322250183839876322541518),
                           DoubleDouble.valueOf(0.140653259715525918745189590510238),
                           DoubleDouble.valueOf(0.169004726639267902826583426598550),
                           DoubleDouble.valueOf(0.190350578064785409913256402421014),
                           DoubleDouble.valueOf(0.204432940075298892414161999234649),
                           DoubleDouble.valueOf(0.209482141084727828012999174891714)
                       };

        // wg - weights of the 7-point gauss rule, corresponding to the
        // abscissae xgk[1], xgk[3], ...
        // wg[0], wg[2], ... are set to zero.
        DoubleDouble[] wg = new DoubleDouble[] {
                          DoubleDouble.valueOf(0.0),
                          DoubleDouble.valueOf(0.129484966168869693270611432679082),
                          DoubleDouble.valueOf(0.0), 
                          DoubleDouble.valueOf(0.279705391489276667901467771423780),
                          DoubleDouble.valueOf(0.0),
                          DoubleDouble.valueOf(0.381830050505118944950369775488975),
                          DoubleDouble.valueOf(0.0),
                          DoubleDouble.valueOf(0.417959183673469387755102040816327)
                      };

        // abscissa
        DoubleDouble absc;
        DoubleDouble absc1;
        DoubleDouble absc2;

        // mid point of the interval
        DoubleDouble centr;
        DoubleDouble dinf;
        DoubleDouble fc;
        DoubleDouble fsum;

        // function value
        DoubleDouble fval1;
        DoubleDouble fval2;
        DoubleDouble[] fv1 = new DoubleDouble[7];
        DoubleDouble[] fv2 = new DoubleDouble[7];

        // half-length of the interval
        DoubleDouble hlgth;

        // result of the 7-point gauss formula
        DoubleDouble resg;

        // result of the 15-point kronrod formula
        DoubleDouble resk;

        // approximation to the mean value of the transformed integrand over
        // (a, b), i.e. to (integral of transformed integrand)/(b - a)
        DoubleDouble reskh;

        // transformed abscissa
        DoubleDouble tabsc1;
        DoubleDouble tabsc2;
        int j;

        dinf = (DoubleDouble.valueOf(1.0)).min(DoubleDouble.valueOf((double)inf));

        centr = (DoubleDouble.valueOf(0.5)).multiply(a.add(b));
        hlgth = (DoubleDouble.valueOf(0.5)).multiply(b.subtract(a));
        tabsc1 = boun.add((dinf.multiply((DoubleDouble.valueOf(1.0)).subtract(centr))).divide(centr));
        if (selfTest) {
        	fval1 = intFuncTest(tabsc1);	
        }
        else {
            fval1 = intFunc(tabsc1);
        }

        if (inf == 2) {
        	if (selfTest) {
        		fval1 = fval1.add(intFuncTest(tabsc1.negate()));	
        	}
        	else {
                fval1 = fval1.add(intFunc(tabsc1.negate()));
        	}
        } // if (inf == 2)

        fc = (fval1.divide(centr)).divide(centr);

        // Compute the 15-point kronrod approximation to the integral, and
        // estimate the error.

        resg = wg[7].multiply(fc);
        resk = wgk[7].multiply(fc);
        resabs[0] = resk.abs();

        for (j = 0; j < 7; j++) {
            absc = hlgth.multiply(xgk[j]);
            absc1 = centr.subtract(absc);
            absc2 = centr.add(absc);
            tabsc1 = boun.add((dinf.multiply((DoubleDouble.valueOf(1.0)).subtract(absc1))).divide(absc1));
            tabsc2 = boun.add((dinf.multiply((DoubleDouble.valueOf(1.0)).subtract(absc2))).divide(absc2));
            if (selfTest) {
            	fval1 = intFuncTest(tabsc1);
                fval2 = intFuncTest(tabsc2);	
            }
            else {
                fval1 = intFunc(tabsc1);
                fval2 = intFunc(tabsc2);
            }

            if (inf == 2) {
            	if (selfTest) {
            		fval1 = fval1.add(intFuncTest(tabsc1.negate()));
                    fval2 = fval2.add(intFuncTest(tabsc2.negate()));	
            	}
            	else {
                    fval1 = fval1.add(intFunc(tabsc1.negate()));
                    fval2 = fval2.add(intFunc(tabsc2.negate()));
            	}
            } // if (inf == 2)

            fval1 = (fval1.divide(absc1)).divide(absc1);
            fval2 = (fval2.divide(absc2)).divide(absc2);
            fv1[j] = (DoubleDouble)fval1.clone();
            fv2[j] = (DoubleDouble)fval2.clone();
            fsum = fval1.add(fval2);
            resg = resg.add(wg[j].multiply(fsum));
            resk = resk.add(wgk[j].multiply(fsum));
            resabs[0] = resabs[0].add(wgk[j].multiply((fval1.abs()).add(fval2.abs())));
        } // for (j = 0; j < 7; j++)

        reskh = (DoubleDouble.valueOf(0.5)).multiply(resk);
        resasc[0] = wgk[7].multiply((fc.subtract(reskh)).abs());

        for (j = 0; j < 7; j++) {
            resasc[0] = resasc[0].add(wgk[j].multiply(((fv1[j].subtract(reskh)).abs()).add((fv2[j].subtract(reskh)).abs())));
        } // for (j = 0; j < 7; j++)

        result[0] = resk.multiply(hlgth);
        resasc[0] = resasc[0].multiply(hlgth);
        resabs[0] = resabs[0].multiply(hlgth);
        abserr[0] = ((resk.subtract(resg)).multiply(hlgth)).abs();

        if ((resasc[0].ne(DoubleDouble.valueOf(0.0))) && (abserr[0].ne(DoubleDouble.valueOf(0.0)))) {
        	DoubleDouble base = ((DoubleDouble.valueOf(200.0)).multiply(abserr[0])).divide(resasc[0]);
            abserr[0] = resasc[0].multiply(((DoubleDouble.valueOf(1.0)).min( (base.pow( 1.5)))));
        } // if ((resasc[0] != 0.0) && (abserr[0] != 0.0))

        if (resabs[0].gt(uflow.divide((DoubleDouble.valueOf(50.0)).multiply(epmach)))) {
            abserr[0] = (((DoubleDouble.valueOf(50.0)).multiply(epmach)).multiply(resabs[0])).max( abserr[0]);
        } // if (resabs[0] > uflow/(50.0 * epmach))

        return;
    }

    /**
     * This is a port of the original FORTRAN whose header is given below:
     * c***begin prologue  dqk21
	 c***date written   800101   (yymmdd)
	 c***revision date  830518   (yymmdd)
	 c***category no.  h2a1a2
	 c***keywords  21-point gauss-kronrod rules
	 c***author  piessens,robert,appl. math. & progr. div. - k.u.leuven
	 c           de doncker,elise,appl. math. & progr. div. - k.u.leuven
	 c***purpose  to compute i = integral of f over (a,b), with error
	 c                           estimate
	 c                       j = integral of abs(f) over (a,b)
	 c***description
	 c
	 c           integration rules
	 c           standard fortran subroutine
	 c           DoubleDouble precision version

     * Computes integral of function over (a,b), with error estimate and computes integral of abs(function) over (a,b).
     *
     * @param  a       lower limit of integration
     * @param  b       upper limit of integration
     * @param  result  Approximation to the integral. Result is computed by applying the 21-point kronrod rule (resk)
     *                 obtained by optimal addition of abscissae to the 10-point gauss rule (resg).
     * @param  abserr  Estimate of the modulus of the absolute error, which should not exceed abs(actual integral -
     *                 result)
     * @param  resabs  Approximation to the integral of abs(function) over (a,b).
     * @param  resasc  Approximation to the integral of abs(function - actual integral/(b-a)) over (a,b)
     */
    private void dqk21(DoubleDouble a, DoubleDouble b, DoubleDouble[] result, DoubleDouble[] abserr, DoubleDouble[] resabs, DoubleDouble[] resasc) {

        // The abscissae and weights are given for the interval (-1,1).
        // Because of symmetry only the positive abscissae and their
        // corresponding weights are given.
        // Gauss quadrature weights and kronron quadrature abscissae and
        // weights as evaluated by 80 digit decimal artihmetic by L. W.
        // Fullerton, Bell Labs, November, 1981.
        // xgk - The abscissae of the 21-point kronrod rule
        // xgk[1], xgk[3], ... abscissae of the 10-point gauss rule
        // xgk[0], xgk[2], ... abscissae which are optimally added to the
        // 10-point gauss rule
        DoubleDouble[] xgk = new DoubleDouble[] {
                           DoubleDouble.valueOf(0.995657163025808080735527280689003),
                           DoubleDouble.valueOf(0.973906528517171720077964012084452),
                           DoubleDouble.valueOf(0.930157491355708226001207180059508),
                           DoubleDouble.valueOf(0.865063366688984510732096688423493),
                           DoubleDouble.valueOf(0.780817726586416897063717578345042),
                           DoubleDouble.valueOf(0.679409568299024406234327365114874),
                           DoubleDouble.valueOf(0.562757134668604683339000099272694),
                           DoubleDouble.valueOf(0.433395394129247190799265943165784),
                           DoubleDouble.valueOf(0.294392862701460198131126603103866),
                           DoubleDouble.valueOf(0.148874338981631210884826001129720),
                           DoubleDouble.valueOf(0.000000000000000000000000000000000)
                       };

        // wgk - weights of the 21-point kronrod rule
        DoubleDouble[] wgk = new DoubleDouble[] {
                           DoubleDouble.valueOf(0.011694638867371874278064396062192),
                           DoubleDouble.valueOf(0.032558162307964727478818972459390),
                           DoubleDouble.valueOf(0.054755896574351996031381300244580),
                           DoubleDouble.valueOf(0.075039674810919952767043140916190),
                           DoubleDouble.valueOf(0.093125454583697605535065465083366),
                           DoubleDouble.valueOf(0.109387158802297641899210590325805),
                           DoubleDouble.valueOf(0.123491976262065851077958109831074),
                           DoubleDouble.valueOf(0.134709217311473325928054001771707),
                           DoubleDouble.valueOf(0.142775938577060080797094273138717),
                           DoubleDouble.valueOf(0.147739104901338491374841515972068),
                           DoubleDouble.valueOf(0.149445554002916905664936468389821)
                       };

        // wg - weights of the 10-point gauss rule
        DoubleDouble[] wg = new DoubleDouble[] {
                          DoubleDouble.valueOf(0.066671344308688137593568809893332),
                          DoubleDouble.valueOf(0.149451349150580593145776339657697),
                          DoubleDouble.valueOf(0.219086362515982043995534934228163),
                          DoubleDouble.valueOf(0.269266719309996355091226921569469),
                          DoubleDouble.valueOf(0.295524224714752870173892994651338)
                      };

        // mid-point of the interval
        DoubleDouble centr;

        // half-length of the interval
        DoubleDouble hlgth;

        // abscissa
        DoubleDouble absc;

        // function values
        DoubleDouble fval1;
        DoubleDouble fval2;

        // result of the 10-point gauss formula
        DoubleDouble resg;

        // result of the 21-point kronrod formula
        DoubleDouble resk;

        // approximation to the mean value of function over (a,b),
        // i.e. to actual integral/ (b-a)
        DoubleDouble reskh;
        DoubleDouble dhlgth;
        DoubleDouble fc;
        DoubleDouble fsum;
        DoubleDouble[] fv1 = new DoubleDouble[10];
        DoubleDouble[] fv2 = new DoubleDouble[10];
        int j;
        int jtw;
        int jtwm1;


        centr = (DoubleDouble.valueOf(0.5)).multiply(a.add(b));
        hlgth = (DoubleDouble.valueOf(0.5)).multiply(b.subtract(a));
        dhlgth = hlgth.abs();

        // Compute the 21-point kronrod approximation to the integral, and
        // estimate the absolute error

        resg = DoubleDouble.valueOf(0.0);
        if (selfTest) {
        	fc = intFuncTest(centr);	
        }
        else {
            fc = intFunc(centr);
        }
        resk = wgk[10].multiply(fc);
        resabs[0] = resk.abs();

        for (j = 0; j <= 4; j++) {
            jtw = (2 * j) + 1;
            absc = hlgth.multiply(xgk[jtw]);
            if (selfTest) {
            	fval1 = intFuncTest(centr.subtract(absc));
                fval2 = intFuncTest(centr.add(absc));	
            }
            else {
                fval1 = intFunc(centr.subtract(absc));
                fval2 = intFunc(centr.add(absc));
            }
            fv1[jtw] = (DoubleDouble)fval1.clone();
            fv2[jtw] = (DoubleDouble)fval2.clone();
            fsum = fval1.add(fval2);
            resg = resg.add(wg[j].multiply(fsum));
            resk = resk.add(wgk[jtw].multiply(fsum));
            resabs[0] = resabs[0].add(wgk[jtw].multiply((fval1.abs()).add(fval2.abs())));
        } // for (j = 0; j <= 4; j ++)

        for (j = 0; j <= 4; j++) {
            jtwm1 = 2 * j;
            absc = hlgth.multiply(xgk[jtwm1]);
            if (selfTest) {
            	fval1 = intFuncTest(centr.subtract(absc));
                fval2 = intFuncTest(centr.add(absc));	
            }
            else {
                fval1 = intFunc(centr.subtract(absc));
                fval2 = intFunc(centr.add(absc));
            }
            fv1[jtwm1] = (DoubleDouble)fval1.clone();
            fv2[jtwm1] = (DoubleDouble)fval2.clone();
            fsum = fval1.add(fval2);
            resk = resk.add(wgk[jtwm1].multiply(fsum));
            resabs[0] = resabs[0].add(wgk[jtwm1].multiply((fval1.abs()).add(fval2.abs())));
        } // for (j = 0; j <= 4; j ++)

        reskh = (DoubleDouble.valueOf(0.5)).multiply(resk);
        resasc[0] = wgk[10].multiply((fc.subtract(reskh)).abs());

        for (j = 0; j <= 9; j++) {
            resasc[0] = resasc[0].add(wgk[j].multiply(((fv1[j].subtract(reskh)).abs()).add((fv2[j].subtract(reskh)).abs())));
        } // for (j = 0; j <= 9; j++)

        result[0] = resk.multiply(hlgth);
        resabs[0] = resabs[0].multiply(dhlgth);
        resasc[0] = resasc[0].multiply(dhlgth);
        abserr[0] = ((resk.subtract(resg)).multiply(hlgth)).abs();

        if ((resasc[0].ne(DoubleDouble.valueOf(0.0))) && (abserr[0].ne(DoubleDouble.valueOf(0.0)))) {
        	DoubleDouble base = ((DoubleDouble.valueOf(200.0)).multiply(abserr[0])).divide(resasc[0]);
            abserr[0] = resasc[0].multiply((DoubleDouble.valueOf(1.0)).min( base.pow(DoubleDouble.valueOf( 1.5))));
        }

        if (resabs[0].gt(uflow.divide((DoubleDouble.valueOf(50.0)).multiply(epmach)))) {
            abserr[0] = (((DoubleDouble.valueOf(50.0)).multiply(epmach)).multiply(resabs[0])).max( abserr[0]);
        }

        return;
    }

    /**
     * This is a port of the original FORTRAN whose header is given below:
     * c***begin prologue  dqk31
	 c***date written   800101   (yymmdd)
	 c***revision date  830518   (yymmdd)
	 c***category no.  h2a1a2
	 c***keywords  31-point gauss-kronrod rules
	 c***author  piessens,robert,appl. math. & progr. div. - k.u.leuven
	 c           de doncker,elise,appl. math. & progr. div. - k.u.leuven
	 c***purpose  to compute i = integral of f over (a,b) with error
	 c                           estimate
	 c                       j = integral of abs(f) over (a,b)
	 c***description
	 c
	 c           integration rules
	 c           standard fortran subroutine
     c           DoubleDouble precision version

     * This routine computes theintegral of the intFunc over (a, b) and the integral of the abs(intFunc) over (a, b).
     *
     * @param  a       lower limit for integration
     * @param  b       upper limit for integration
     * @param  result  Approximation to the integral. The result is computed by applying the 31-point kronrod rule
     *                 (resk) obtained by optimal addition of abscissae to the 15-point gauss rule (resg).
     * @param  abserr  Estimate of the modulus of the absolute error, which should equal or exceed abs(integral of
     *                 intFunc over (a,b) - result)
     * @param  resabs  Approximation to integral of abs(intFunc)
     * @param  resasc  Approximation to the integral of abs(intFunc - integral of intFunc/(b - a)) over (a, b)
     */
    private void dqk31(DoubleDouble a, DoubleDouble b, DoubleDouble[] result, DoubleDouble[] abserr, DoubleDouble[] resabs, DoubleDouble[] resasc) {
        // The abscissae and weights are supplied for the interval (-1, 1).
        // Because of symmetry only the positive abscissae and their
        // corresponding weights are given.
        // Gauss quadrature weights and kronron quadrature abscissae and
        // weights as evaluated by 80 digit decimal artihmetic by L. W.
        // Fullerton, Bell Labs, November, 1981.


        // xgk - abscissae of the 31-point kronrod rule
        // xgk[1], xgk[3], ... abscissae of the 15-point gauss rule
        // xgk[0], xgk[2], ... abscissae which are optimally added to the
        // 15-point gauss rule.
        DoubleDouble[] xgk = new DoubleDouble[] {
                           DoubleDouble.valueOf(0.998002298693397060285172840152271),
                           DoubleDouble.valueOf(0.987992518020485428489565718586613),
                           DoubleDouble.valueOf(0.967739075679139134257347978784337),
                           DoubleDouble.valueOf(0.937273392400705904307758947710209),
                           DoubleDouble.valueOf(0.897264532344081900882509656454496), 
                           DoubleDouble.valueOf(0.848206583410427216200648320774217),
                           DoubleDouble.valueOf(0.790418501442465932967649294817947), 
                           DoubleDouble.valueOf(0.724417731360170047416186054613938),
                           DoubleDouble.valueOf(0.650996741297416970533735895313275), 
                           DoubleDouble.valueOf(0.570972172608538847537226737253911),
                           DoubleDouble.valueOf(0.485081863640239680693655740232351), 
                           DoubleDouble.valueOf(0.394151347077563369897207370981045),
                           DoubleDouble.valueOf(0.299180007153168812166780024266389), 
                           DoubleDouble.valueOf(0.201194093997434522300628303394596),
                           DoubleDouble.valueOf(0.101142066918717499027074231447392), 
                           DoubleDouble.valueOf(0.000000000000000000000000000000000)
                       };

        // wgk - weights of the 31-point kronrod rule
        DoubleDouble[] wgk = new DoubleDouble[] {
        		           DoubleDouble.valueOf(0.005377479872923348987792051430128), 
        		           DoubleDouble.valueOf(0.015007947329316122538374763075807),
        		           DoubleDouble.valueOf(0.025460847326715320186874001019653), 
        		           DoubleDouble.valueOf(0.035346360791375846222037948478360),
        		           DoubleDouble.valueOf(0.044589751324764876608227299373280), 
        		           DoubleDouble.valueOf(0.053481524690928087265343147239430),
        		           DoubleDouble.valueOf(0.062009567800670640285139230960803), 
        		           DoubleDouble.valueOf(0.069854121318728258709520077099147),
        		           DoubleDouble.valueOf(0.076849680757720378894432777482659), 
        		           DoubleDouble.valueOf(0.083080502823133021038289247286104),
        		           DoubleDouble.valueOf(0.088564443056211770647275443693774),
        		           DoubleDouble.valueOf(0.093126598170825321225486872747346),
        		           DoubleDouble.valueOf(0.096642726983623678505179907627589),
        		           DoubleDouble.valueOf(0.099173598721791959332393173484603),
        		           DoubleDouble.valueOf(0.100769845523875595044946662617570),
        		           DoubleDouble.valueOf(0.101330007014791549017374792767493)
                       };

        // wg - weights of the 15-point gauss rule
        DoubleDouble[] wg = new DoubleDouble[] {
        		          DoubleDouble.valueOf(0.030753241996117268354628393577204), 
        		          DoubleDouble.valueOf(0.070366047488108124709267416450667),
        		          DoubleDouble.valueOf(0.107159220467171935011869546685869), 
        		          DoubleDouble.valueOf(0.139570677926154314447804794511028),
        		          DoubleDouble.valueOf(0.166269205816993933553200860481209), 
        		          DoubleDouble.valueOf(0.186161000015562211026800561866423),
        		          DoubleDouble.valueOf(0.198431485327111576456118326443839), 
        		          DoubleDouble.valueOf(0.202578241925561272880620199967519)
                      };

        // abscissa
        DoubleDouble absc;

        // mid point of the interval
        DoubleDouble centr;
        DoubleDouble dhlgth;
        DoubleDouble fc;
        DoubleDouble fsum;

        // function value
        DoubleDouble fval1;
        DoubleDouble fval2;
        DoubleDouble[] fv1 = new DoubleDouble[15];
        DoubleDouble[] fv2 = new DoubleDouble[15];

        // half-length of the interval
        DoubleDouble hlgth;

        // result of the 15-point gauss formula
        DoubleDouble resg;

        // result of the 31-point kronrod formula
        DoubleDouble resk;

        // approximation to the mean value of intFunc over
        // (a, b), i.e. to (integral of intFunc)/(b - a) over (a,b)
        DoubleDouble reskh;
        int j;
        int jtw;
        int jtwm1;

        centr = (DoubleDouble.valueOf(0.5)).multiply(a.add(b));
        hlgth = (DoubleDouble.valueOf(0.5)).multiply(b.subtract(a));
        dhlgth = hlgth.abs();

        // Compute the 31-point kronrod approximation to the integral, and
        // estimate the absolute error.

        if (selfTest) {
        	fc = intFuncTest(centr);
        }
        else {
            fc = intFunc(centr);
        }
        resg = fc.multiply(wg[7]);
        resk = fc.multiply(wgk[15]);
        resabs[0] = resk.abs();

        for (j = 0; j < 7; j++) {
            jtw = (2 * j) + 1;
            absc = hlgth.multiply(xgk[jtw]);
            if (selfTest) {
            	fval1 = intFuncTest(centr.subtract(absc));
                fval2 = intFuncTest(centr.add(absc));	
            }
            else {
                fval1 = intFunc(centr.subtract(absc));
                fval2 = intFunc(centr.add(absc));
            }
            fv1[jtw] = (DoubleDouble)fval1.clone();
            fv2[jtw] = (DoubleDouble)fval2.clone();
            fsum = fval1.add(fval2);
            resg = resg.add(wg[j].multiply(fsum));
            resk = resk.add(wgk[jtw].multiply(fsum));
            resabs[0] = resabs[0].add(wgk[jtw].multiply((fval1.abs()).add(fval2.abs())));
        } // for (j = 0; j < 7; j++)

        for (j = 0; j < 8; j++) {
            jtwm1 = 2 * j;
            absc = hlgth.multiply(xgk[jtwm1]);
            if (selfTest) {
            	fval1 = intFuncTest(centr.subtract(absc));
                fval2 = intFuncTest(centr.add(absc));	
            }
            else {
                fval1 = intFunc(centr.subtract(absc));
                fval2 = intFunc(centr.add(absc));
            }
            fv1[jtwm1] = (DoubleDouble)fval1.clone();
            fv2[jtwm1] = (DoubleDouble)fval2.clone();
            fsum = fval1.add(fval2);
            resk = resk.add(wgk[jtwm1].multiply(fsum));
            resabs[0] = resabs[0].add(wgk[jtwm1].multiply((fval1.abs()).add(fval2.abs())));
        } // for (j = 0; j < 8; j++)

        reskh = (DoubleDouble.valueOf(0.5)).multiply(resk);
        resasc[0] = wgk[15].multiply((fc.subtract(reskh)).abs());

        for (j = 0; j < 15; j++) {
            resasc[0] = resasc[0].add(wgk[j].multiply(((fv1[j].subtract(reskh)).abs()).add((fv2[j].subtract(reskh)).abs())));
        } // for (j = 0; j < 15; j++)

        result[0] = resk.multiply(hlgth);
        resabs[0] = resabs[0].multiply(dhlgth);
        resasc[0] = resasc[0].multiply(dhlgth);
        abserr[0] = ((resk.subtract(resg)).multiply(hlgth)).abs();

        if ((resasc[0].ne(DoubleDouble.valueOf(0.0))) && (abserr[0].ne(DoubleDouble.valueOf(0.0)))) {
        	DoubleDouble base = ((DoubleDouble.valueOf(200.0)).multiply(abserr[0])).divide(resasc[0]);
            abserr[0] = resasc[0].multiply((DoubleDouble.valueOf(1.0)).min(base.pow( 1.5)));
        }

        if (resabs[0].gt(uflow.divide((DoubleDouble.valueOf(50.0)).multiply(epmach)))) {
            abserr[0] = (((DoubleDouble.valueOf(50.0)).multiply(epmach)).multiply(resabs[0])).max( abserr[0]);
        }

        return;
    }

    /**
     * This is a port of the original FORTRAN routine whose header is given below:
     * c***begin prologue  dqk41
	 c***date written   800101   (yymmdd)
	 c***revision date  830518   (yymmdd)
	 c***category no.  h2a1a2
	 c***keywords  41-point gauss-kronrod rules
	 c***author  piessens,robert,appl. math. & progr. div. - k.u.leuven
	 c           de doncker,elise,appl. math. & progr. div. - k.u.leuven
	 c***purpose  to compute i = integral of f over (a,b), with error
	 c                           estimate
	 c                       j = integral of abs(f) over (a,b)
	 c***description
	 c
	 c           integration rules
	 c           standard fortran subroutine
	 c           DoubleDouble precision version

     * This routine computes theintegral of the intFunc over (a, b) and the integral of the abs(intFunc) over (a, b).
     *
     * @param  a       lower limit for integration
     * @param  b       upper limit for integration
     * @param  result  Approximation to the integral. The result is computed by applying the 41-point kronrod rule
     *                 (resk) obtained by optimal addition of abscissae to the 20-point gauss rule (resg).
     * @param  abserr  Estimate of the modulus of the absolute error, which should equal or exceed abs(integral of
     *                 intFunc over (a,b) - result)
     * @param  resabs  Approximation to integral of abs(intFunc)
     * @param  resasc  Approximation to the integral of abs(intFunc - integral of intFunc/(b - a)) over (a, b)
     */
    private void dqk41(DoubleDouble a, DoubleDouble b, DoubleDouble[] result, DoubleDouble[] abserr, DoubleDouble[] resabs, DoubleDouble[] resasc) {
        // The abscissae and weights are supplied for the interval (-1, 1).
        // Because of symmetry only the positive abscissae and their
        // corresponding weights are given.
        // Gauss quadrature weights and kronron quadrature abscissae and
        // weights as evaluated by 80 digit decimal artihmetic by L. W.
        // Fullerton, Bell Labs, November, 1981.


        // xgk - abscissae of the 41-point kronrod rule
        // xgk[1], xgk[3], ... abscissae of the 20-point gauss rule
        // xgk[0], xgk[2], ... abscissae which are optimally added to the
        // 20-point gauss rule.
        DoubleDouble[] xgk = new DoubleDouble[] {
                           DoubleDouble.valueOf(0.998859031588277663838315576545863), 
                           DoubleDouble.valueOf(0.993128599185094924786122388471320),
                           DoubleDouble.valueOf(0.981507877450250259193342994720217), 
                           DoubleDouble.valueOf(0.963971927277913791267666131197277),
                           DoubleDouble.valueOf(0.940822633831754753519982722212443),
                           DoubleDouble.valueOf(0.912234428251325905867752441203298),
                           DoubleDouble.valueOf(0.878276811252281976077442995113078), 
                           DoubleDouble.valueOf(0.839116971822218823394529061701521),
                           DoubleDouble.valueOf(0.795041428837551198350638833272788), 
                           DoubleDouble.valueOf(0.746331906460150792614305070355642),
                           DoubleDouble.valueOf(0.693237656334751384805490711845932),
                           DoubleDouble.valueOf(0.636053680726515025452836696226286),
                           DoubleDouble.valueOf(0.575140446819710315342946036586425), 
                           DoubleDouble.valueOf(0.510867001950827098004364050955251),
                           DoubleDouble.valueOf(0.443593175238725103199992213492640), 
                           DoubleDouble.valueOf(0.373706088715419560672548177024927),
                           DoubleDouble.valueOf(0.301627868114913004320555356858592),
                           DoubleDouble.valueOf(0.227785851141645078080496195368575),
                           DoubleDouble.valueOf(0.152605465240922675505220241022678),
                           DoubleDouble.valueOf(0.076526521133497333754640409398838),
                           DoubleDouble.valueOf(0.000000000000000000000000000000000)
                       };

        // wgk - weights of the 41-point kronrod rule
        DoubleDouble[] wgk = new DoubleDouble[] {
        		           DoubleDouble.valueOf(0.003073583718520531501218293246031), 
        		           DoubleDouble.valueOf(0.008600269855642942198661787950102),
        		           DoubleDouble.valueOf(0.014626169256971252983787960308868),
        		           DoubleDouble.valueOf(0.020388373461266523598010231432755),
        		           DoubleDouble.valueOf(0.025882133604951158834505067096153), 
        		           DoubleDouble.valueOf(0.031287306777032798958543119323801),
        		           DoubleDouble.valueOf(0.036600169758200798030557240707211), 
        		           DoubleDouble.valueOf(0.041668873327973686263788305936895),
        		           DoubleDouble.valueOf(0.046434821867497674720231880926108),
        		           DoubleDouble.valueOf(0.050944573923728691932707670050345),
        		           DoubleDouble.valueOf(0.055195105348285994744832372419777), 
        		           DoubleDouble.valueOf(0.059111400880639572374967220648594),
        		           DoubleDouble.valueOf(0.062653237554781168025870122174255), 
        		           DoubleDouble.valueOf(0.065834597133618422111563556969398),
        		           DoubleDouble.valueOf(0.068648672928521619345623411885368), 
        		           DoubleDouble.valueOf(0.071054423553444068305790361723210),
        		           DoubleDouble.valueOf(0.073030690332786667495189417658913),
        		           DoubleDouble.valueOf(0.074582875400499188986581418362488),
        		           DoubleDouble.valueOf(0.075704497684556674659542775376617), 
        		           DoubleDouble.valueOf(0.076377867672080736705502835038061),
        		           DoubleDouble.valueOf(0.076600711917999656445049901530102)
                       };

        // wg - weights of the 20-point gauss rule
        DoubleDouble[] wg = new DoubleDouble[] {
        		          DoubleDouble.valueOf(0.017614007139152118311861962351853),
        		          DoubleDouble.valueOf(0.040601429800386941331039952274932),
        		          DoubleDouble.valueOf(0.062672048334109063569506535187042),
        		          DoubleDouble.valueOf(0.083276741576704748724758143222046),
        		          DoubleDouble.valueOf(0.101930119817240435036750135480350), 
        		          DoubleDouble.valueOf(0.118194531961518417312377377711382),
        		          DoubleDouble.valueOf(0.131688638449176626898494499748163), 
        		          DoubleDouble.valueOf(0.142096109318382051329298325067165),
        		          DoubleDouble.valueOf(0.149172986472603746787828737001969), 
        		          DoubleDouble.valueOf(0.152753387130725850698084331955098)
                      };

        // abscissa
        DoubleDouble absc;

        // mid point of the interval
        DoubleDouble centr;
        DoubleDouble dhlgth;
        DoubleDouble fc;
        DoubleDouble fsum;

        // function value
        DoubleDouble fval1;
        DoubleDouble fval2;
        DoubleDouble[] fv1 = new DoubleDouble[20];
        DoubleDouble[] fv2 = new DoubleDouble[20];

        // half-length of the interval
        DoubleDouble hlgth;

        // result of the 20-point gauss formula
        DoubleDouble resg;

        // result of the 41-point kronrod formula
        DoubleDouble resk;

        // approximation to the mean value of intFunc over
        // (a, b), i.e. to (integral of intFunc)/(b - a) over (a,b)
        DoubleDouble reskh;
        int j;
        int jtw;
        int jtwm1;

        centr = (DoubleDouble.valueOf(0.5)).multiply(a.add(b));
        hlgth = (DoubleDouble.valueOf(0.5)).multiply(b.subtract(a));
        dhlgth = hlgth.abs();

        // Compute the 41-point kronrod approximation to the integral, and
        // estimate the absolute error.

        resg = DoubleDouble.valueOf(0.0);
        if (selfTest) {
        	fc = intFuncTest(centr);	
        }
        else {
            fc = intFunc(centr);
        }
        resk = fc.multiply(wgk[20]);
        resabs[0] = resk.abs();

        for (j = 0; j < 10; j++) {
            jtw = (2 * j) + 1;
            absc = hlgth.multiply(xgk[jtw]);
            if (selfTest) {
            	fval1 = intFuncTest(centr.subtract(absc));
                fval2 = intFuncTest(centr.add(absc));	
            }
            else {
                fval1 = intFunc(centr.subtract(absc));
                fval2 = intFunc(centr.add(absc));
            }
            fv1[jtw] = (DoubleDouble)fval1.clone();
            fv2[jtw] = (DoubleDouble)fval2.clone();
            fsum = fval1.add(fval2);
            resg = resg.add(wg[j].multiply(fsum));
            resk = resk.add(wgk[jtw].multiply(fsum));
            resabs[0] = resabs[0].add(wgk[jtw].multiply((fval1.abs()).add(fval2.abs())));
        } // for (j = 0; j < 10; j++)

        for (j = 0; j < 10; j++) {
            jtwm1 = 2 * j;
            absc = hlgth.multiply(xgk[jtwm1]);
            if (selfTest) {
            	fval1 = intFuncTest(centr.subtract(absc));
                fval2 = intFuncTest(centr.add(absc));	
            }
            else {
                fval1 = intFunc(centr.subtract(absc));
                fval2 = intFunc(centr.add(absc));
            }
            fv1[jtwm1] = (DoubleDouble)fval1.clone();
            fv2[jtwm1] = (DoubleDouble)fval2.clone();
            fsum = fval1.add(fval2);
            resk = resk.add(wgk[jtwm1].multiply(fsum));
            resabs[0] = resabs[0].add(wgk[jtwm1].multiply((fval1.abs()).add(fval2.abs())));
        } // for (j = 0; j < 10; j++)

        reskh = (DoubleDouble.valueOf(0.5)).multiply(resk);
        resasc[0] = wgk[20].multiply((fc.subtract(reskh)).abs());

        for (j = 0; j < 20; j++) {
            resasc[0] = resasc[0].add(wgk[j].multiply(((fv1[j].subtract(reskh)).abs()).add((fv2[j].subtract(reskh)).abs())));
        } // for (j = 0; j < 20; j++)

        result[0] = resk.multiply(hlgth);
        resabs[0] = resabs[0].multiply(dhlgth);
        resasc[0] = resasc[0].multiply(dhlgth);
        abserr[0] = ((resk.subtract(resg)).multiply(hlgth)).abs();

        if ((resasc[0].ne(DoubleDouble.valueOf(0.0))) && (abserr[0].ne(DoubleDouble.valueOf(0.0)))) {
        	DoubleDouble base = ((DoubleDouble.valueOf(200.0)).multiply(abserr[0])).divide(resasc[0]);
            abserr[0] = resasc[0].multiply((DoubleDouble.valueOf(1.0)).min(base.pow( 1.5)));
        }

        if (resabs[0].gt(uflow.divide((DoubleDouble.valueOf(50.0)).multiply(epmach)))) {
            abserr[0] = (((DoubleDouble.valueOf(50.0)).multiply(epmach)).multiply(resabs[0])).max (abserr[0]);
        }

        return;
    }

    /**
     * This is a port of the original FORTRAN routine whose header is given below:
     * c***begin prologue  dqk51
	 c***date written   800101   (yymmdd)
	 c***revision date  830518   (yymmdd)
	 c***category no.  h2a1a2
	 c***keywords  51-point gauss-kronrod rules
	 c***author  piessens,robert,appl. math. & progr. div. - k.u.leuven
	 c           de doncker,elise,appl. math & progr. div. - k.u.leuven
	 c***purpose  to compute i = integral of f over (a,b) with error
	 c                           estimate
	 c                       j = integral of abs(f) over (a,b)
	 c***description
	 c
	 c           integration rules
	 c           standard fortran subroutine
	 c           DoubleDouble precision version

     * This routine computes theintegral of the intFunc over (a, b) and the integral of the abs(intFunc) over (a, b).
     *
     * @param  a       lower limit for integration
     * @param  b       upper limit for integration
     * @param  result  Approximation to the integral. The result is computed by applying the 51-point kronrod rule
     *                 (resk) obtained by optimal addition of abscissae to the 25-point gauss rule (resg).
     * @param  abserr  Estimate of the modulus of the absolute error, which should equal or exceed abs(integral of
     *                 intFunc over (a,b) - result)
     * @param  resabs  Approximation to integral of abs(intFunc)
     * @param  resasc  Approximation to the integral of abs(intFunc - integral of intFunc/(b - a)) over (a, b)
     */
    private void dqk51(DoubleDouble a, DoubleDouble b, DoubleDouble[] result, DoubleDouble[] abserr, DoubleDouble[] resabs, DoubleDouble[] resasc) {
        // The abscissae and weights are supplied for the interval (-1, 1).
        // Because of symmetry only the positive abscissae and their
        // corresponding weights are given.
        // Gauss quadrature weights and kronron quadrature abscissae and
        // weights as evaluated by 80 digit decimal artihmetic by L. W.
        // Fullerton, Bell Labs, November, 1981.


        // xgk - abscissae of the 51-point kronrod rule
        // xgk[1], xgk[3], ... abscissae of the 25-point gauss rule
        // xgk[0], xgk[2], ... abscissae which are optimally added to the
        // 25-point gauss rule.
        DoubleDouble[] xgk = new DoubleDouble[] {
                           DoubleDouble.valueOf(0.999262104992609834193457486540341), 
                           DoubleDouble.valueOf(0.995556969790498097908784946893902),
                           DoubleDouble.valueOf(0.988035794534077247637331014577406), 
                           DoubleDouble.valueOf(0.976663921459517511498315386479594),
                           DoubleDouble.valueOf(0.961614986425842512418130033660167), 
                           DoubleDouble.valueOf(0.942974571228974339414011169658471),
                           DoubleDouble.valueOf(0.920747115281701561746346084546331), 
                           DoubleDouble.valueOf(0.894991997878275368851042006782805),
                           DoubleDouble.valueOf(0.865847065293275595448996969588340), 
                           DoubleDouble.valueOf(0.833442628760834001421021108693570),
                           DoubleDouble.valueOf(0.797873797998500059410410904994307), 
                           DoubleDouble.valueOf(0.759259263037357630577282865204361),
                           DoubleDouble.valueOf(0.717766406813084388186654079773298), 
                           DoubleDouble.valueOf(0.673566368473468364485120633247622),
                           DoubleDouble.valueOf(0.626810099010317412788122681624518), 
                           DoubleDouble.valueOf(0.577662930241222967723689841612654),
                           DoubleDouble.valueOf(0.526325284334719182599623778158010), 
                           DoubleDouble.valueOf(0.473002731445714960522182115009192),
                           DoubleDouble.valueOf(0.417885382193037748851814394594572), 
                           DoubleDouble.valueOf(0.361172305809387837735821730127641),
                           DoubleDouble.valueOf(0.303089538931107830167478909980339), 
                           DoubleDouble.valueOf(0.243866883720988432045190362797452),
                           DoubleDouble.valueOf(0.183718939421048892015969888759528), 
                           DoubleDouble.valueOf(0.122864692610710396387359818808037),
                           DoubleDouble.valueOf(0.061544483005685078886546392366797), 
                           DoubleDouble.valueOf(0.000000000000000000000000000000000)
                       };

        // wgk - weights of the 51-point kronrod rule
        DoubleDouble[] wgk = new DoubleDouble[] {
        		           DoubleDouble.valueOf(0.001987383892330315926507851882843), 
        		           DoubleDouble.valueOf(0.005561932135356713758040236901066),
        		           DoubleDouble.valueOf(0.009473973386174151607207710523655), 
        		           DoubleDouble.valueOf(0.013236229195571674813656405846976),
        		           DoubleDouble.valueOf(0.016847817709128298231516667536336),
        		           DoubleDouble.valueOf(0.020435371145882835456568292235939),
        		           DoubleDouble.valueOf(0.024009945606953216220092489164881), 
        		           DoubleDouble.valueOf(0.027475317587851737802948455517811),
        		           DoubleDouble.valueOf(0.030792300167387488891109020215229), 
        		           DoubleDouble.valueOf(0.034002130274329337836748795229551),
        		           DoubleDouble.valueOf(0.037116271483415543560330625367620), 
        		           DoubleDouble.valueOf(0.040083825504032382074839284467076),
        		           DoubleDouble.valueOf(0.042872845020170049476895792439495), 
        		           DoubleDouble.valueOf(0.045502913049921788909870584752660),
        		           DoubleDouble.valueOf(0.047982537138836713906392255756915), 
        		           DoubleDouble.valueOf(0.050277679080715671963325259433440),
        		           DoubleDouble.valueOf(0.052362885806407475864366712137873), 
        		           DoubleDouble.valueOf(0.054251129888545490144543370459876),
        		           DoubleDouble.valueOf(0.055950811220412317308240686382747), 
        		           DoubleDouble.valueOf(0.057437116361567832853582693939506),
        		           DoubleDouble.valueOf(0.058689680022394207961974175856788),
        		           DoubleDouble.valueOf(0.059720340324174059979099291932562),
        		           DoubleDouble.valueOf(0.060539455376045862945360267517565),
        		           DoubleDouble.valueOf(0.061128509717053048305859030416293),
        		           DoubleDouble.valueOf(0.061471189871425316661544131965264),

                           // note: wgk[25] was calculated from the values of wgk[0..24]
        		           DoubleDouble.valueOf(0.061580818067832935078759824240066)
                       };

        // wg - weights of the 25-point gauss rule
        DoubleDouble[] wg = new DoubleDouble[] {
        		          DoubleDouble.valueOf(0.011393798501026287947902964113235), 
        		          DoubleDouble.valueOf(0.026354986615032137261901815295299),
        		          DoubleDouble.valueOf(0.040939156701306312655623487711646), 
        		          DoubleDouble.valueOf(0.054904695975835191925936891540473),
        		          DoubleDouble.valueOf(0.068038333812356917207187185656708), 
        		          DoubleDouble.valueOf(0.080140700335001018013234959669111),
        		          DoubleDouble.valueOf(0.091028261982963649811497220702892), 
        		          DoubleDouble.valueOf(0.100535949067050644202206890392686),
        		          DoubleDouble.valueOf(0.108519624474263653116093957050117), 
        		          DoubleDouble.valueOf(0.114858259145711648339325545869556),
        		          DoubleDouble.valueOf(0.119455763535784772228178126512901), 
        		          DoubleDouble.valueOf(0.122242442990310041688959518945852),
        		          DoubleDouble.valueOf(0.123176053726715451203902873079050)
                      };

        // abscissa
        DoubleDouble absc;

        // mid point of the interval
        DoubleDouble centr;
        DoubleDouble dhlgth;
        DoubleDouble fc;
        DoubleDouble fsum;

        // function value
        DoubleDouble fval1;
        DoubleDouble fval2;
        DoubleDouble[] fv1 = new DoubleDouble[25];
        DoubleDouble[] fv2 = new DoubleDouble[25];

        // half-length of the interval
        DoubleDouble hlgth;

        // result of the 25-point gauss formula
        DoubleDouble resg;

        // result of the 51-point kronrod formula
        DoubleDouble resk;

        // approximation to the mean value of intFunc over
        // (a, b), i.e. to (integral of intFunc)/(b - a) over (a,b)
        DoubleDouble reskh;
        int j;
        int jtw;
        int jtwm1;

        centr = (DoubleDouble.valueOf(0.5)).multiply(a.add(b));
        hlgth = (DoubleDouble.valueOf(0.50)).multiply(b.subtract(a));
        dhlgth = hlgth.abs();

        // Compute the 51-point kronrod approximation to the integral, and
        // estimate the absolute error.

        if (selfTest) {
        	fc = intFuncTest(centr);	
        }
        else {
            fc = intFunc(centr);
        }
        resg = fc.multiply(wg[12]);
        resk = fc.multiply(wgk[25]);
        resabs[0] = resk.abs();

        for (j = 0; j < 12; j++) {
            jtw = (2 * j) + 1;
            absc = hlgth.multiply(xgk[jtw]);
            if (selfTest) {
            	fval1 = intFuncTest(centr.subtract(absc));
                fval2 = intFuncTest(centr.add(absc));	
            }
            else {
                fval1 = intFunc(centr.subtract(absc));
                fval2 = intFunc(centr.add(absc));
            }
            fv1[jtw] = (DoubleDouble)fval1.clone();
            fv2[jtw] = (DoubleDouble)fval2.clone();
            fsum = fval1.add(fval2);
            resg = resg.add(wg[j].multiply(fsum));
            resk = resk.add(wgk[jtw].multiply(fsum));
            resabs[0] = resabs[0].add(wgk[jtw].multiply((fval1.abs()).add(fval2.abs())));
        } // for (j = 0; j < 12; j++)

        for (j = 0; j < 13; j++) {
            jtwm1 = 2 * j;
            absc = hlgth.multiply(xgk[jtwm1]);
            if (selfTest) {
            	fval1 = intFuncTest(centr.subtract(absc));
                fval2 = intFuncTest(centr.add(absc));	
            }
            else {
                fval1 = intFunc(centr.subtract(absc));
                fval2 = intFunc(centr.add(absc));
            }
            fv1[jtwm1] = (DoubleDouble)fval1.clone();
            fv2[jtwm1] = (DoubleDouble)fval2.clone();
            fsum = fval1.add(fval2);
            resk = resk.add(wgk[jtwm1].multiply(fsum));
            resabs[0] = resabs[0].add(wgk[jtwm1].multiply((fval1.abs()).add(fval2.abs())));
        } // for (j = 0; j < 13; j++)

        reskh = (DoubleDouble.valueOf(0.5)).multiply(resk);
        resasc[0] = wgk[25].multiply((fc.subtract(reskh)).abs());

        for (j = 0; j < 25; j++) {
            resasc[0] = resasc[0].add(wgk[j].multiply(((fv1[j].subtract(reskh)).abs()).add((fv2[j].subtract(reskh)).abs())));
        } // for (j = 0; j < 25; j++)

        result[0] = resk.multiply(hlgth);
        resabs[0] = resabs[0].multiply(dhlgth);
        resasc[0] = resasc[0].multiply(dhlgth);
        abserr[0] = ((resk.subtract(resg)).multiply(hlgth)).abs();

        if ((resasc[0].ne(DoubleDouble.valueOf(0.0))) && (abserr[0].ne(DoubleDouble.valueOf(0.0)))) {
        	DoubleDouble base = ((DoubleDouble.valueOf(200.0)).multiply(abserr[0])).divide(resasc[0]);
            abserr[0] = resasc[0].multiply((DoubleDouble.valueOf(1.0)).min(base.pow( 1.5)));
        }

        if (resabs[0].gt(uflow.divide((DoubleDouble.valueOf(50.0)).multiply(epmach)))) {
            abserr[0] = (((DoubleDouble.valueOf(50.0)).multiply(epmach)).multiply(resabs[0])).max( abserr[0]);
        }

        return;
    }

    /**
     * This is a port of the original FORTRAN routine whose header is given below:
     * c***begin prologue  dqk61
	 c***date written   800101   (yymmdd)
	 c***revision date  830518   (yymmdd)
	 c***category no.  h2a1a2
	 c***keywords  61-point gauss-kronrod rules
	 c***author  piessens,robert,appl. math. & progr. div. - k.u.leuven
	 c           de doncker,elise,appl. math. & progr. div. - k.u.leuven
	 c***purpose  to compute i = integral of f over (a,b) with error
	 c                           estimate
	 c                       j = integral of dabs(f) over (a,b)
	 c***description
	 c
	 c        integration rule
	 c        standard fortran subroutine
	 c        DoubleDouble precision version

     * This routine computes the integral of the intFunc over (a, b) and the integral of the abs(intFunc) over (a, b).
     *
     * @param  a       lower limit for integration
     * @param  b       upper limit for integration
     * @param  result  Approximation to the integral. The result is computed by applying the 61-point kronrod rule
     *                 (resk) obtained by optimal addition of abscissae to the 30-point gauss rule (resg).
     * @param  abserr  Estimate of the modulus of the absolute error, which should equal or exceed abs(integral of
     *                 intFunc over (a,b) - result)
     * @param  resabs  Approximation to integral of abs(intFunc)
     * @param  resasc  Approximation to the integral of abs(intFunc - integral of intFunc/(b - a)) over (a, b)
     */
    private void dqk61(DoubleDouble a, DoubleDouble b, DoubleDouble[] result, DoubleDouble[] abserr, DoubleDouble[] resabs, DoubleDouble[] resasc) {
        // The abscissae and weights are supplied for the interval (-1, 1).
        // Because of symmetry only the positive abscissae and their
        // corresponding weights are given.
        // Gauss quadrature weights and kronron quadrature abscissae and
        // weights as evaluated by 80 digit decimal artihmetic by L. W.
        // Fullerton, Bell Labs, November, 1981.


        // xgk - abscissae of the 61-point kronrod rule
        // xgk[1], xgk[3], ... abscissae of the 30-point gauss rule
        // xgk[0], xgk[2], ... abscissae which are optimally added to the
        // 30-point gauss rule.
        DoubleDouble[] xgk = new DoubleDouble[] {
                           DoubleDouble.valueOf(0.999484410050490637571325895705811), 
                           DoubleDouble.valueOf(0.996893484074649540271630050918695),
                           DoubleDouble.valueOf(0.991630996870404594858628366109486), 
                           DoubleDouble.valueOf(0.983668123279747209970032581605663),
                           DoubleDouble.valueOf(0.973116322501126268374693868423707), 
                           DoubleDouble.valueOf(0.960021864968307512216871025581798),
                           DoubleDouble.valueOf(0.944374444748559979415831324037439), 
                           DoubleDouble.valueOf(0.926200047429274325879324277080474),
                           DoubleDouble.valueOf(0.905573307699907798546522558925958), 
                           DoubleDouble.valueOf(0.882560535792052681543116462530226),
                           DoubleDouble.valueOf(0.857205233546061098958658510658944),
                           DoubleDouble.valueOf(0.829565762382768397442898119732502),
                           DoubleDouble.valueOf(0.799727835821839083013668942322683), 
                           DoubleDouble.valueOf(0.767777432104826194917977340974503),
                           DoubleDouble.valueOf(0.733790062453226804726171131369528), 
                           DoubleDouble.valueOf(0.697850494793315796932292388026640),
                           DoubleDouble.valueOf(0.660061064126626961370053668149271), 
                           DoubleDouble.valueOf(0.620526182989242861140477556431189),
                           DoubleDouble.valueOf(0.579345235826361691756024932172540), 
                           DoubleDouble.valueOf(0.536624148142019899264169793311073),
                           DoubleDouble.valueOf(0.492480467861778574993693061207709), 
                           DoubleDouble.valueOf(0.447033769538089176780609900322854),
                           DoubleDouble.valueOf(0.400401254830394392535476211542661), 
                           DoubleDouble.valueOf(0.352704725530878113471037207089374),
                           DoubleDouble.valueOf(0.304073202273625077372677107199257), 
                           DoubleDouble.valueOf(0.254636926167889846439805129817805),
                           DoubleDouble.valueOf(0.204525116682309891438957671002025), 
                           DoubleDouble.valueOf(0.153869913608583546963794672743256),
                           DoubleDouble.valueOf(0.102806937966737030147096751318001), 
                           DoubleDouble.valueOf(0.051471842555317695833025213166723),
                           DoubleDouble.valueOf(0.000000000000000000000000000000000)
                       };

        // wgk - weights of the 61-point kronrod rule
        DoubleDouble[] wgk = new DoubleDouble[] {
        		           DoubleDouble.valueOf(0.001389013698677007624551591226760), 
        		           DoubleDouble.valueOf(0.003890461127099884051267201844516),
        		           DoubleDouble.valueOf(0.006630703915931292173319826369750), 
        		           DoubleDouble.valueOf(0.009273279659517763428441146892024),
        		           DoubleDouble.valueOf(0.011823015253496341742232898853251), 
        		           DoubleDouble.valueOf(0.014369729507045804812451432443580),
        		           DoubleDouble.valueOf(0.016920889189053272627572289420322), 
        		           DoubleDouble.valueOf(0.019414141193942381173408951050128),
        		           DoubleDouble.valueOf(0.021828035821609192297167485738339), 
        		           DoubleDouble.valueOf(0.024191162078080601365686370725232),
        		           DoubleDouble.valueOf(0.026509954882333101610601709335075), 
        		           DoubleDouble.valueOf(0.028754048765041292843978785354334),
        		           DoubleDouble.valueOf(0.030907257562387762472884252943092), 
        		           DoubleDouble.valueOf(0.032981447057483726031814191016854),
        		           DoubleDouble.valueOf(0.034979338028060024137499670731468), 
        		           DoubleDouble.valueOf(0.036882364651821229223911065617136),
        		           DoubleDouble.valueOf(0.038678945624727592950348651532281), 
        		           DoubleDouble.valueOf(0.040374538951535959111995279752468),
        		           DoubleDouble.valueOf(0.041969810215164246147147541285970), 
        		           DoubleDouble.valueOf(0.043452539701356069316831728117073),
        		           DoubleDouble.valueOf(0.044814800133162663192355551616723), 
        		           DoubleDouble.valueOf(0.046059238271006988116271735559374),
        		           DoubleDouble.valueOf(0.047185546569299153945261478181099), 
        		           DoubleDouble.valueOf(0.048185861757087129140779492298305),
        		           DoubleDouble.valueOf(0.049055434555029778887528165367238), 
        		           DoubleDouble.valueOf(0.049795683427074206357811569379942),
        		           DoubleDouble.valueOf(0.050405921402782346840893085653585), 
        		           DoubleDouble.valueOf(0.050881795898749606492297473049805),
        		           DoubleDouble.valueOf(0.051221547849258772170656282604944), 
        		           DoubleDouble.valueOf(0.051426128537459025933862879215781),
        		           DoubleDouble.valueOf(0.051494729429451567558340433647099)
                       };

        // wg - weights of the 30-point gauss rule
        DoubleDouble[] wg = new DoubleDouble[] {
        		          DoubleDouble.valueOf(0.007968192496166605615465883474674), 
        		          DoubleDouble.valueOf(0.018466468311090959142302131912047),
        		          DoubleDouble.valueOf(0.028784707883323369349719179611292), 
        		          DoubleDouble.valueOf(0.038799192569627049596801936446348),
        		          DoubleDouble.valueOf(0.048402672830594052902938140422808), 
        		          DoubleDouble.valueOf(0.057493156217619066481721689402056),
        		          DoubleDouble.valueOf(0.065974229882180495128128515115962),
        		          DoubleDouble.valueOf(0.073755974737705206268243850022191),
        		          DoubleDouble.valueOf(0.080755895229420215354694938460530), 
        		          DoubleDouble.valueOf(0.086899787201082979802387530715126),
        		          DoubleDouble.valueOf(0.092122522237786128717632707087619), 
        		          DoubleDouble.valueOf(0.096368737174644259639468626351810),
        		          DoubleDouble.valueOf(0.099593420586795267062780282103569), 
        		          DoubleDouble.valueOf(0.101762389748405504596428952168554),
        		          DoubleDouble.valueOf(0.102852652893558840341285636705415)
                      };

        // abscissa
        DoubleDouble absc;

        // mid point of the interval
        DoubleDouble centr;
        DoubleDouble dhlgth;
        DoubleDouble fc;
        DoubleDouble fsum;

        // function value
        DoubleDouble fval1;
        DoubleDouble fval2;
        DoubleDouble[] fv1 = new DoubleDouble[30];
        DoubleDouble[] fv2 = new DoubleDouble[30];

        // half-length of the interval
        DoubleDouble hlgth;

        // result of the 30-point gauss formula
        DoubleDouble resg;

        // result of the 61-point kronrod formula
        DoubleDouble resk;

        // approximation to the mean value of intFunc over
        // (a, b), i.e. to (integral of intFunc)/(b - a) over (a,b)
        DoubleDouble reskh;
        int j;
        int jtw;
        int jtwm1;

        centr = (DoubleDouble.valueOf(0.5)).multiply(a.add(b));
        hlgth = (DoubleDouble.valueOf(0.5)).multiply(b.subtract(a));
        dhlgth = hlgth.abs();

        // Compute the 61-point kronrod approximation to the integral, and
        // estimate the absolute error.

        resg = DoubleDouble.valueOf(0.0);
        if (selfTest) {
        	fc = intFuncTest(centr);	
        }
        else {
            fc = intFunc(centr);
        }
        resk = fc.multiply(wgk[30]);
        resabs[0] = resk.abs();

        for (j = 0; j < 15; j++) {
            jtw = (2 * j) + 1;
            absc = hlgth.multiply(xgk[jtw]);
            if (selfTest) {
            	fval1 = intFuncTest(centr.subtract(absc));
                fval2 = intFuncTest(centr.add(absc));	
            }
            else {
                fval1 = intFunc(centr.subtract(absc));
                fval2 = intFunc(centr.add(absc));
            }
            fv1[jtw] = (DoubleDouble)fval1.clone();
            fv2[jtw] = (DoubleDouble)fval2.clone();
            fsum = fval1.add(fval2);
            resg = resg.add(wg[j].multiply(fsum));
            resk = resk.add(wgk[jtw].multiply(fsum));
            resabs[0] = resabs[0].add(wgk[jtw].multiply((fval1.abs()).add(fval2.abs())));
        } // for (j = 0; j < 15; j++)

        for (j = 0; j < 15; j++) {
            jtwm1 = 2 * j;
            absc = hlgth.multiply(xgk[jtwm1]);
            if (selfTest) {
            	fval1 = intFuncTest(centr.subtract(absc));
                fval2 = intFuncTest(centr.add(absc));	
            }
            else {
                fval1 = intFunc(centr.subtract(absc));
                fval2 = intFunc(centr.add(absc));
            }
            fv1[jtwm1] = (DoubleDouble)fval1.clone();
            fv2[jtwm1] = (DoubleDouble)fval2.clone();
            fsum = fval1.add(fval2);
            resk = resk.add(wgk[jtwm1].multiply(fsum));
            resabs[0] = resabs[0].add(wgk[jtwm1].multiply((fval1.abs()).add(fval2.abs())));
        } // for (j = 0; j < 15; j++)

        reskh = (DoubleDouble.valueOf(0.5)).multiply(resk);
        resasc[0] = wgk[30].multiply((fc.subtract(reskh)).abs());

        for (j = 0; j < 30; j++) {
            resasc[0] = resasc[0].add(wgk[j].multiply(((fv1[j].subtract(reskh)).abs()).add((fv2[j].subtract(reskh)).abs())));
        } // for (j = 0; j < 30; j++)

        result[0] = resk.multiply(hlgth);
        resabs[0] = resabs[0].multiply(dhlgth);
        resasc[0] = resasc[0].multiply(dhlgth);
        abserr[0] = ((resk.subtract(resg)).multiply(hlgth)).abs();

        if ((resasc[0].ne(DoubleDouble.valueOf(0.0))) && (abserr[0].ne(DoubleDouble.valueOf(0.0)))) {
        	DoubleDouble base = ((DoubleDouble.valueOf(200.0)).multiply(abserr[0])).divide(resasc[0]);
            abserr[0] = resasc[0].multiply((DoubleDouble.valueOf(1.0)).min(base.pow( 1.5)));
        }

        if (resabs[0].gt(uflow.divide((DoubleDouble.valueOf(50.0)).multiply(epmach)))) {
            abserr[0] = (((DoubleDouble.valueOf(50.0)).multiply(epmach)).multiply(resabs[0])).max( abserr[0]);
        }

        return;
    }


    /**
     * This is a port of the original FORTRAN routine whose header is given below:
     * c***begin prologue  dqng
	 c***date written   800101   (yymmdd)
	 c***revision date  810101   (yymmdd)
	 c***category no.  h2a1a1
	 c***keywords  automatic integrator, smooth integrand,
	 c             non-adaptive, gauss-kronrod(patterson)
	 c***author  piessens,robert,appl. math. & progr. div. - k.u.leuven
	 c           de doncker,elise,appl math & progr. div. - k.u.leuven
	 c           kahaner,david,nbs - modified (2/82)
	 c***purpose  the routine calculates an approximation result to a
	 c            given definite integral i = integral of f over (a,b),
	 c            hopefully satisfying following claim for accuracy
	 c            abs(i-result).le.max(epsabs,epsrel*abs(i)).
	 c***description
	 c
	 c non-adaptive integration
	 c standard fortran subroutine
	 c DoubleDouble precision version

     * Calculates an integral over (lower, upper), hopefully satisfying the abs(actual integral - result) <= max(epsabs,
     * epsresl * abs(actual integral)) Result is obtained by applying the 21-point gauss-kronrod rule (res21) obtained
     * by optimal addition of abscissae to the 10-point gauss rule (res10), or by applying the 43-point rule (res43)
     * obtained by optimal addition of abscissae to the 21-point gauss-kronrod rule, or by applying the 87-point rule
     * (res87) obtained by optimal addition of abscissae to the 43-point rule.
     */
    private void dqng() {
        // The following lines contain the abscissae and weights of the
        // integration rules used
        // x1 abscissae common to the 10-, 21-, 43-, and 87-point rule
        // x2 abscissae common to the 21-, 43-, and 87-point rule
        // x3 abscissae common to the 43- and 87- point rule
        // x4 abscissae of the 87-point rule
        // w10 weights fo the 10-point formula
        // w21a weights of the 21-point formula for abscissae x1
        // w21b weights of the 21-point formula for abscissae x2
        // w43a weights of the 43-point formula for abscissae x1, x3
        // w43b weights of the 43-point formula for abscissae x3
        // w87a weights of the 87-point formula for abscissae x1, x2, x3
        // w87b weights of the 87-point formula for abscissae x4

        // gauss-kronrod-patterson quadrature coefficients for use in
        // routine dqng.  These coefficients were calculated with
        // 101 decimal digit arithmetic by L. W. Fullerton, bell labs, nov 1981.

        DoubleDouble[] x1 = new DoubleDouble[] {
        		          DoubleDouble.valueOf(0.973906528517171720077964012084452),
        		          DoubleDouble.valueOf(0.865063366688984510732096688423493),
        		          DoubleDouble.valueOf(0.679409568299024406234327365114874),
        		          DoubleDouble.valueOf(0.433395394129247190799265943165784),
        		          DoubleDouble.valueOf(0.148874338981631210884826001129720)
                      };

        DoubleDouble[] w10 = new DoubleDouble[] {
        		           DoubleDouble.valueOf(0.066671344308688137593568809893332),
        		           DoubleDouble.valueOf(0.149451349150580593145776339657697),
        		           DoubleDouble.valueOf(0.219086362515982043995534934228163),
        		           DoubleDouble.valueOf(0.269266719309996355091226921569469),
        		           DoubleDouble.valueOf(0.295524224714752870173892994651338)
                       };

        DoubleDouble[] x2 = new DoubleDouble[] {
        		          DoubleDouble.valueOf(0.995657163025808080735527280689003),
        		          DoubleDouble.valueOf(0.930157491355708226001207180059508),
        		          DoubleDouble.valueOf(0.780817726586416897063717578345042), 
        		          DoubleDouble.valueOf(0.562757134668604683339000099272694),
        		          DoubleDouble.valueOf(0.294392862701460198131126603103866)
                      };

        DoubleDouble[] w21a = new DoubleDouble[] {
        		            DoubleDouble.valueOf(0.032558162307964727478818972459390), 
        		            DoubleDouble.valueOf(0.075039674810919952767043140916190),
        		            DoubleDouble.valueOf(0.109387158802297641899210590325805),
        		            DoubleDouble.valueOf(0.134709217311473325928054001771707),
        		            DoubleDouble.valueOf(0.147739104901338491374841515972068)
                        };

        DoubleDouble[] w21b = new DoubleDouble[] {
        		            DoubleDouble.valueOf(0.011694638867371874278064396062192),
        		            DoubleDouble.valueOf(0.054755896574351996031381300244580),
        		            DoubleDouble.valueOf(0.093125454583697605535065465083366),
        		            DoubleDouble.valueOf(0.123491976262065851077958109831074),
        		            DoubleDouble.valueOf(0.142775938577060080797094273138717),
        		            DoubleDouble.valueOf(0.149445554002916905664936468389821)
                        };

        DoubleDouble[] x3 = new DoubleDouble[] {
        		          DoubleDouble.valueOf(0.999333360901932081394099323919911), 
        		          DoubleDouble.valueOf(0.987433402908088869795961478381209),
        		          DoubleDouble.valueOf(0.954807934814266299257919200290473), 
        		          DoubleDouble.valueOf(0.900148695748328293625099494069092),
        		          DoubleDouble.valueOf(0.825198314983114150847066732588520),
        		          DoubleDouble.valueOf(0.732148388989304982612354848755461),
        		          DoubleDouble.valueOf(0.622847970537725238641159120344323), 
        		          DoubleDouble.valueOf(0.499479574071056499952214885499755),
        		          DoubleDouble.valueOf(0.364901661346580768043989548502644), 
        		          DoubleDouble.valueOf(0.222254919776601296498260928066212),
        		          DoubleDouble.valueOf(0.074650617461383322043914435796506)
                      };

        DoubleDouble[] w43a = new DoubleDouble[] {
        		            DoubleDouble.valueOf(0.016296734289666564924281974617663), 
        		            DoubleDouble.valueOf(0.037522876120869501461613795898115),
        		            DoubleDouble.valueOf(0.054694902058255442147212685465005), 
        		            DoubleDouble.valueOf(0.067355414609478086075553166302174),
        		            DoubleDouble.valueOf(0.073870199632393953432140695251367), 
        		            DoubleDouble.valueOf(0.005768556059769796184184327908655),
        		            DoubleDouble.valueOf(0.027371890593248842081276069289151), 
        		            DoubleDouble.valueOf(0.046560826910428830743339154433824),
        		            DoubleDouble.valueOf(0.061744995201442564496240336030883), 
        		            DoubleDouble.valueOf(0.071387267268693397768559114425516)
                        };

        DoubleDouble[] w43b = new DoubleDouble[] {
        		            DoubleDouble.valueOf(0.001844477640212414100389106552965), 
        		            DoubleDouble.valueOf(0.010798689585891651740465406741293),
        		            DoubleDouble.valueOf(0.021895363867795428102523123075149), 
        		            DoubleDouble.valueOf(0.032597463975345689443882222526137),
        		            DoubleDouble.valueOf(0.042163137935191811847627924327955), 
        		            DoubleDouble.valueOf(0.050741939600184577780189020092084),
        		            DoubleDouble.valueOf(0.058379395542619248375475369330206), 
        		            DoubleDouble.valueOf(0.064746404951445885544689259517511),
        		            DoubleDouble.valueOf(0.069566197912356484528633315038405), 
        		            DoubleDouble.valueOf(0.072824441471833208150939535192842),
        		            DoubleDouble.valueOf(0.074507751014175118273571813842889), 
        		            DoubleDouble.valueOf(0.074722147517403005594425168280423)
                        };

        DoubleDouble[] x4 = new DoubleDouble[] {
        		          DoubleDouble.valueOf(0.999902977262729234490529830591582), 
        		          DoubleDouble.valueOf(0.997989895986678745427496322365960),
        		          DoubleDouble.valueOf(0.992175497860687222808523352251425), 
        		          DoubleDouble.valueOf(0.981358163572712773571916941623894),
        		          DoubleDouble.valueOf(0.965057623858384619128284110607926), 
        		          DoubleDouble.valueOf(0.943167613133670596816416634507426),
        		          DoubleDouble.valueOf(0.915806414685507209591826430720050), 
        		          DoubleDouble.valueOf(0.883221657771316501372117548744163),
        		          DoubleDouble.valueOf(0.845710748462415666605902011504855), 
        		          DoubleDouble.valueOf(0.803557658035230982788739474980964),
        		          DoubleDouble.valueOf(0.757005730685495558328942793432020), 
        		          DoubleDouble.valueOf(0.706273209787321819824094274740840),
        		          DoubleDouble.valueOf(0.651589466501177922534422205016736), 
        		          DoubleDouble.valueOf(0.593223374057961088875273770349144),
        		          DoubleDouble.valueOf(0.531493605970831932285268948562671), 
        		          DoubleDouble.valueOf(0.466763623042022844871966781659270),
        		          DoubleDouble.valueOf(0.399424847859218804732101665817923), 
        		          DoubleDouble.valueOf(0.329874877106188288265053371824597),
        		          DoubleDouble.valueOf(0.258503559202161551802280975429025), 
        		          DoubleDouble.valueOf(0.185695396568346652015917141167606),
        		          DoubleDouble.valueOf(0.111842213179907468172398359241362), 
        		          DoubleDouble.valueOf(0.037352123394619870814998165437704)
                      };

        DoubleDouble[] w87a = new DoubleDouble[] {
        		            DoubleDouble.valueOf(0.008148377384149172900002878448190), 
        		            DoubleDouble.valueOf(0.018761438201562822243935059003794),
        		            DoubleDouble.valueOf(0.027347451050052286161582829741283), 
        		            DoubleDouble.valueOf(0.033677707311637930046581056957588),
        		            DoubleDouble.valueOf(0.036935099820427907614589586742499), 
        		            DoubleDouble.valueOf(0.002884872430211530501334156248695),
        		            DoubleDouble.valueOf(0.013685946022712701888950035273128), 
        		            DoubleDouble.valueOf(0.023280413502888311123409291030404),
        		            DoubleDouble.valueOf(0.030872497611713358675466394126442),
        		            DoubleDouble.valueOf(0.035693633639418770719351355457044),
        		            DoubleDouble.valueOf(0.000915283345202241360843392549948), 
        		            DoubleDouble.valueOf(0.005399280219300471367738743391053),
        		            DoubleDouble.valueOf(0.010947679601118931134327826856808), 
        		            DoubleDouble.valueOf(0.016298731696787335262665703223280),
        		            DoubleDouble.valueOf(0.021081568889203835112433060188190), 
        		            DoubleDouble.valueOf(0.025370969769253827243467999831710),
        		            DoubleDouble.valueOf(0.029189697756475752501446154084920), 
        		            DoubleDouble.valueOf(0.032373202467202789685788194889595),
        		            DoubleDouble.valueOf(0.034783098950365142750781997949596), 
        		            DoubleDouble.valueOf(0.036412220731351787562801163687577),
        		            DoubleDouble.valueOf(0.037253875503047708539592001191226)
                        };

        DoubleDouble[] w87b = new DoubleDouble[] {
        		            DoubleDouble.valueOf(0.000274145563762072350016527092881), 
        		            DoubleDouble.valueOf(0.001807124155057942948341311753254),
        		            DoubleDouble.valueOf(0.004096869282759164864458070683480), 
        		            DoubleDouble.valueOf(0.006758290051847378699816577897424),
        		            DoubleDouble.valueOf(0.009549957672201646536053581325377),
        		            DoubleDouble.valueOf(0.012329447652244853694626639963780),
        		            DoubleDouble.valueOf(0.015010447346388952376697286041943), 
        		            DoubleDouble.valueOf(0.017548967986243191099665352925900),
        		            DoubleDouble.valueOf(0.019938037786440888202278192730714), 
        		            DoubleDouble.valueOf(0.022194935961012286796332102959499),
        		            DoubleDouble.valueOf(0.024339147126000805470360647041454), 
        		            DoubleDouble.valueOf(0.026374505414839207241503786552615),
        		            DoubleDouble.valueOf(0.028286910788771200659968002987960), 
        		            DoubleDouble.valueOf(0.030052581128092695322521110347341),
        		            DoubleDouble.valueOf(0.031646751371439929404586051078883), 
        		            DoubleDouble.valueOf(0.033050413419978503290785944862689),
        		            DoubleDouble.valueOf(0.034255099704226061787082821046821), 
        		            DoubleDouble.valueOf(0.035262412660156681033782717998428),
        		            DoubleDouble.valueOf(0.036076989622888701185500318003895), 
        		            DoubleDouble.valueOf(0.036698604498456094498018047441094),
        		            DoubleDouble.valueOf(0.037120549269832576114119958413599), 
        		            DoubleDouble.valueOf(0.037334228751935040321235449094698),
        		            DoubleDouble.valueOf(0.037361073762679023410321241766599)
                        };

        // mid point of integration interval
        DoubleDouble centr;

        // half-length of integration interval
        DoubleDouble hlgth;

        // abscissa
        DoubleDouble asbc;

        // function value
        DoubleDouble fval;

        // array of function values which have already been computed
        DoubleDouble[] savfun = new DoubleDouble[21];

        // 10-point gauss result
        DoubleDouble res10;

        // 21-point kronrod result
        DoubleDouble res21;

        // 43-point result
        DoubleDouble res43;

        // 87-point result
        DoubleDouble res87;

        // approximation to integral of abs(intFunc)
        DoubleDouble resabs;

        // approximation to integral of
        // abs(intFunc - integral of intFunc over (lower, upper)/(upper - lower))
        DoubleDouble dhlgth;
        DoubleDouble fcentr;
        int k;
        DoubleDouble absc;
        DoubleDouble fval1;
        DoubleDouble fval2;
        DoubleDouble[] fv1 = new DoubleDouble[5];
        DoubleDouble[] fv2 = new DoubleDouble[5];
        DoubleDouble[] fv3 = new DoubleDouble[5];
        DoubleDouble[] fv4 = new DoubleDouble[5];
        int ipx;
        DoubleDouble reskh;
        DoubleDouble resasc;

        result[0] = DoubleDouble.valueOf(0.0);
        abserr[0] = DoubleDouble.valueOf(0.0);
        hlgth = (DoubleDouble.valueOf(0.5)).multiply(upper.subtract(lower));
        dhlgth = hlgth.abs();
        centr = (DoubleDouble.valueOf(0.5)).multiply(upper.add(lower));
        if (selfTest) {
        	fcentr = intFuncTest(centr);	
        }
        else {
            fcentr = intFunc(centr);
        }
        neval = 21;
        errorStatus = 1;

        // Compute the integral using the 10- and 21-point formula.

        res10 = DoubleDouble.valueOf(0.0);
        res21 = w21b[5].multiply(fcentr);
        resabs = w21b[5].multiply(fcentr.abs());

        for (k = 0; k < 5; k++) {
            absc = hlgth.multiply(x1[k]);
            if (selfTest) {
            	fval1 = intFuncTest(centr.add(absc));
                fval2 = intFuncTest(centr.subtract(absc));	
            }
            else {
                fval1 = intFunc(centr.add(absc));
                fval2 = intFunc(centr.subtract(absc));
            }
            fval = fval1.add(fval2);
            res10 = res10.add(w10[k].multiply(fval));
            res21 = res21.add(w21a[k].multiply(fval));
            resabs = resabs.add(w21a[k].multiply((fval1.abs()).add(fval2.abs())));
            savfun[k] = (DoubleDouble)fval.clone();
            fv1[k] = (DoubleDouble)fval1.clone();
            fv2[k] = (DoubleDouble)fval2.clone();
        } // for (k = 0; k < 5; k++)

        ipx = 4;

        for (k = 0; k < 5; k++) {
            ipx++;
            absc = hlgth.multiply(x2[k]);
            if (selfTest) {
            	fval1 = intFuncTest(centr.add(absc));
                fval2 = intFuncTest(centr.subtract(absc));	
            }
            else {
                fval1 = intFunc(centr.add(absc));
                fval2 = intFunc(centr.subtract(absc));
            }
            fval = fval1.add(fval2);
            res21 = res21.add(w21b[k].multiply(fval));
            resabs = resabs.add(w21b[k].multiply((fval1.abs()).add(fval2.abs())));
            savfun[ipx] = (DoubleDouble)fval.clone();
            fv3[k] = (DoubleDouble)fval1.clone();
            fv4[k] = (DoubleDouble)fval2.clone();
        } // for (k = 0; k < 5; k++)

        // Test for convergence

        result[0] = res21.multiply(hlgth);
        resabs = resabs.multiply(dhlgth);
        reskh = (DoubleDouble.valueOf(0.5)).multiply(res21);
        resasc = w21b[5].multiply((fcentr.subtract(reskh)).abs());

        for (k = 0; k < 5; k++) {
            resasc = resasc.add(w21a[k].multiply(((fv1[k].subtract(reskh)).abs()).add((fv2[k].subtract(reskh)).abs()))).add
                     (w21b[k].multiply(((fv3[k].subtract(reskh)).abs()).add((fv4[k].subtract(reskh)).abs())));
        } // for (k = 0; k < 5; k++)

        abserr[0] = ((res21.subtract(res10)).multiply(hlgth)).abs();
        resasc = resasc.multiply(dhlgth);

        if ((resasc.ne(DoubleDouble.valueOf(0.0))) && (abserr[0].ne(DoubleDouble.valueOf(0.0)))) {
        	DoubleDouble base = ((DoubleDouble.valueOf(200.0)).multiply(abserr[0])).divide(resasc);
            abserr[0] = resasc.multiply((DoubleDouble.valueOf(1.0)).min(base.pow(1.5)));
        } // if ((resasc != 0.0) && (abserr[0] != 0.0))

        if (resabs.gt(uflow.divide((DoubleDouble.valueOf(50.0)).multiply(epmach)))) {
            abserr[0] = ((epmach.multiply(DoubleDouble.valueOf(50.0))).multiply(resabs)).max( abserr[0]);
        } // if (resabs > uflow/(50.0 * epmach))

        if (abserr[0].le(epsabs.max( epsrel.multiply(result[0].abs())))) {
            errorStatus = 0;
        } // if (abserr[0] <= Math.max(epsabs, epsrel * Math.abs(result[0])))

        if (errorStatus == 0) {
            return;
        } // if (errorStatus == 0)

        // Compute the integral using the 43-point formula

        res43 = w43b[11].multiply(fcentr);
        neval = 43;

        for (k = 0; k < 10; k++) {
            res43 = res43.add(savfun[k].multiply(w43a[k]));
        } // for (k = 0; k < 10; k++)

        for (k = 0; k < 11; k++) {
            ipx++;
            absc = hlgth.multiply(x3[k]);
            if (selfTest) {
            	fval = intFuncTest(absc.add(centr)).add(intFuncTest(centr.subtract(absc)));	
            }
            else {
                fval = intFunc(absc.add(centr)).add(intFunc(centr.subtract(absc)));
            }
            res43 = res43.add(fval.multiply(w43b[k]));
            savfun[ipx] = (DoubleDouble)fval.clone();
        } // for (k = 0; k < 11; k++)

        // Test for convergence
        result[0] = res43.multiply(hlgth);
        abserr[0] = ((res43.subtract(res21)).multiply(hlgth)).abs();

        if ((resasc.ne(DoubleDouble.valueOf(0.0))) && (abserr[0].ne(DoubleDouble.valueOf(0.0)))) {
        	DoubleDouble base = ((DoubleDouble.valueOf(200.0)).multiply(abserr[0])).divide(resasc);
            abserr[0] = resasc.multiply((DoubleDouble.valueOf(1.0)).min(base.pow( 1.5)));
        } // if ((resasc != 0.0) && (abserr[0] != 0.0))

        if (resabs.gt(uflow.divide((DoubleDouble.valueOf(50.0)).multiply(epmach)))) {
            abserr[0] = ((epmach.multiply(DoubleDouble.valueOf(50.0))).multiply(resabs)).max( abserr[0]);
        } // if (resabs > uflow/(50.0 * epmach))

        if (abserr[0].le(epsabs.max( epsrel.multiply(result[0].abs())))) {
            errorStatus = 0;
        } // if (abserr[0] <= Math.max(epsabs, epsrel * Math.abs(result[0])))

        if (errorStatus == 0) {
            return;
        } // if (errorStatus == 0)

        // Compute the integral using the 87-point formula
        res87 = w87b[22].multiply(fcentr);
        neval = 87;

        for (k = 0; k < 21; k++) {
            res87 = res87.add(savfun[k].multiply(w87a[k]));
        } // for (k = 0; k < 21; k++)

        for (k = 0; k < 22; k++) {
            absc = hlgth.multiply(x4[k]);
            if (selfTest) {
            	res87 = res87.add(w87b[k].multiply(intFuncTest(absc.add(centr)).add(intFuncTest(centr.subtract(absc)))));	
            }
            else {
                res87 = res87.add(w87b[k].multiply(intFunc(absc.add(centr)).add(intFunc(centr.subtract(absc)))));
            }
        } // for (k = 0; k < 22; k++)

        result[0] = res87.multiply(hlgth);
        abserr[0] = ((res87.subtract(res43)).multiply(hlgth)).abs();

        if ((resasc.ne(DoubleDouble.valueOf(0.0))) && (abserr[0].ne(DoubleDouble.valueOf(0.0)))) {
        	DoubleDouble base = ((DoubleDouble.valueOf(200.0)).multiply(abserr[0])).divide(resasc);
            abserr[0] = resasc.multiply(((DoubleDouble.valueOf(1.0)).min(base.pow( 1.5))));
        } // if ((resasc != 0.0) && (abserr[0] != 0.0))

        if (resabs.gt(uflow.divide((DoubleDouble.valueOf(50.0)).multiply(epmach)))) {
            abserr[0] = ((epmach.multiply(DoubleDouble.valueOf(50.0))).multiply(resabs)).max( abserr[0]);
        } // if (resabs > uflow/(50.0 * epmach))

        if (abserr[0].le(epsabs.max( epsrel.multiply(result[0].abs())))) {
            errorStatus = 0;
        } // if (abserr[0] <= Math.max(epsabs, epsrel * Math.abs(result[0])))

        if (errorStatus == 0) {
            return;
        } // if (errorStatus == 0)

        Preferences.debug("Abnormal return from dqng\n");

        return;
    }

    /**
     * This is a port of the original FORTRAN routine whose header is given below:
     * c***begin prologue  dqpsrt
	 c***refer to  dqage,dqagie,dqagpe,dqawse
	 c***routines called  (none)
	 c***revision date  810101   (yymmdd)
	 c***keywords  sequential sorting
	 c***author  piessens,robert,appl. math. & progr. div. - k.u.leuven
	 c           de doncker,elise,appl. math. & progr. div. - k.u.leuven
	 c***purpose  this routine maintains the descending ordering in the
	 c            list of the local error estimated resulting from the
	 c            interval subdivision process. at each call two error
	 c            estimates are inserted using the sequential search
	 c            method, top-down for the largest error estimate and
	 c            bottom-up for the smallest error estimate.
	 c***description
	 c
	 c           ordering routine
	 c           standard fortran subroutine
	 c           DoubleDouble precision version

     * This routine maintains the descending ordering in the list of the local error estimated resulting from the
     * interval subdivision process. At each call two error estimates are inserted using the sequential search method,
     * top-down for the largest error estimate and bottom-up for the smallest error estimate.
     *
     * @param  limit   maximum number of error estimates the list can contain
     * @param  last    number of error estimates currently in the list
     * @param  maxErr  maxErr points to the nrmax-th largest estimate currently in the list
     * @param  ermax   nrmax-th largest error estimate ermax[0] = elist[maxErr[0]]
     * @param  elist   the error estimates
     * @param  iord    Array of size last , the first k elements of which contain pointers to the error estimates, such
     *                 that elist[iord[0]],...,elist[iord[k-1]] form a decreasing sequence, with k = last if last <=
     *                 (limit/2 + 2), and k = limit + 1 - last otherwise
     * @param  nrmax   maxErr[0] = iord[nrmax[0] - 1]
     */
    private void dqpsrt(int limit, int last, int[] maxErr, DoubleDouble[] ermax, DoubleDouble[] elist, int[] iord, int[] nrmax) {
        DoubleDouble errmin;
        DoubleDouble errmax;
        int i;
        int ibeg;
        int ido;
        int isucc;
        int j;
        int jbnd;
        int jupbn;
        int k;

        // Check whether the list contains more than two error estimates

        if (last <= 2) {
            iord[0] = 0;
            iord[1] = 1;
            maxErr[0] = iord[nrmax[0] - 1];
            ermax[0] = (DoubleDouble)elist[maxErr[0]].clone();

            return;
        } // if (last <= 2)

        // This part of the routine is only executed if, due to a difficult
        // integrand, subdivision increased the error estimate.  In the
        // normal case the insert procedure should start after the nrmax-th
        // largest error estimate

        errmax = (DoubleDouble)elist[maxErr[0]].clone();

        if (nrmax[0] != 1) {
            ido = nrmax[0] - 1;

            for (i = 1; i <= ido; i++) {
                isucc = iord[nrmax[0] - 2];

                if (errmax.le(elist[isucc])) {
                    break;
                } // if (errmax <= elist[isucc])

                iord[nrmax[0] - 1] = isucc;
                nrmax[0] = nrmax[0] - 1;
            } // for (i = 1; i <= ido; i++)
        } // if (nrmax[0] != 1)

        // Compute the number of elements in the list to be maintained in
        // descending order.  This number depends on the number of
        // subdivisions still allowed
        jupbn = last;

        if (last > ((limit / 2) + 2)) {
            jupbn = limit + 3 - last;
        } // if (last > (limit/2 + 2))

        errmin = (DoubleDouble)elist[last - 1].clone();

        // Insert errmax by traversing the list top-down,
        // starting comparison from the element elist(iord[nrmmax[0]])

        jbnd = jupbn - 1;
        ibeg = nrmax[0] + 1;

group:   {

            if (ibeg <= jbnd) {

                for (i = ibeg; i <= jbnd; i++) {
                    isucc = iord[i - 1];

                    if (errmax.ge(elist[isucc])) {
                        break group;
                    } // if (errmax >= elist[isucc])

                    iord[i - 2] = isucc;
                } // for (i = ibeg; i <= jbnd; i++)
            } // if (ibeg <= jbnd)

            iord[jbnd - 1] = maxErr[0];
            iord[jupbn - 1] = last - 1;
            maxErr[0] = iord[nrmax[0] - 1];
            ermax[0] = (DoubleDouble)elist[maxErr[0]].clone();

            return;
        } // group

        // Insert errmin by traversing the list from the bottom-up

        iord[i - 2] = maxErr[0];
        k = jbnd;

group2:  {

            for (j = i; j <= jbnd; j++) {
                isucc = iord[k - 1];

                if (errmin.lt(elist[isucc])) {
                    break group2;
                } // if (errmin < elist[isucc])

                iord[k] = isucc;
                k = k - 1;
            } // for (j = i; j <= jbnd; j++)

            iord[i - 1] = last - 1;
            maxErr[0] = iord[nrmax[0] - 1];
            ermax[0] = (DoubleDouble)elist[maxErr[0]].clone();

            return;
        } // group2

        iord[k] = last - 1;
        maxErr[0] = iord[nrmax[0] - 1];
        ermax[0] = (DoubleDouble)elist[maxErr[0]].clone();

        return;
    }
}
