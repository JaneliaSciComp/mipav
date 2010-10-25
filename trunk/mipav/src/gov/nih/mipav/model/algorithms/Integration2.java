package gov.nih.mipav.model.algorithms;


import gov.nih.mipav.DoubleDouble;
import gov.nih.mipav.view.*;


/**
 * This is a port of FORTRAN numerical integration routines in QUADPACK found at http://www.netlib.org/quadpack
 * Reference: R. Piessens, E. deDoncker-Kapenga, C. Uberhuber, D. Kahaner Quadpack: a Subroutine Package for Automatic
 * Integration Springer Verlag, 1983. Series in Computational Mathematics v. 1
 * The original dqage, dqagie, dqagpe, dqagse, and dqawce routines were written by Robert Piessens and Elise de Doncker.
 * The original dqng routine was written by Robert Piessens and Elise de Doncker and modified by David Kahaner.
 * The original dqc25c, dqcheb, dqelg, dqk15, dqk15i, dqk15w, dqk21, dqk31, dqk41, dqk51, dqk61, dqpsrt, and dqwgtc  
 * routines were written by Robert Piessens and Elise de Doncker.
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


public abstract class Integration2 {

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
    
    /** Cauchy principal value */
    protected static final int DQAWCE = 12;

    //~ Instance fields ------------------------------------------------------------------------------------------------

    /** Estimate of the absolute value of the error, which should equal or exceed abs(actual integral - result). */
    private double[] abserr = new double[1];

    /** The left end points of the subintervals in the partition of the given integration range (lower, upper). */
    private double[] alist;

    /** The right end points of the subintervals in the partition of the given integration range (lower, upper). */
    private double[] blist;

    /** finite bound of integration range used in dqagie (has no meaning if interval is doubly-infinite). */
    private double bound;
    
    /** Parameter in the weight function, c != lower, c != upper.
     * If c == a or c == b, the routine will end with errorStatus = 6
     */
    private double c;

    /**
     * Used in dqagpe This array must always be >= 2 in length. The first breakPoints.length - 2 points are user
     * provided break points. If these points are not in an ascending sequence, they will automatically be sorted.
     */
    private double[] breakPoints;

    /** Estimates of the absolute errors on the subintervals. */
    private double[] elist;

    /**
     * epmach = D1MACH(4) Machine epmach is the smallest positive epmach such that (1.0 + epmach) != 1.0. epmach = 2**(1
     * - doubleDigits) = 2**(1 - 53) = 2**(-52) epmach = 2.224460e-16 epmach is called the largest relative spacing
     */
    private double epmach;
    // 2**-1022 = D1MACH(1)

    /**
     * If epsabs <= 0.0 and epsrel < Math.max(50*relative machine accuracy, 5.0E-29), the routine will exit with an
     * error message.
     */
    private double epsabs;

    /** DOCUMENT ME! */
    private double epsrel;

    /**
     * errorStatus = 0 for normal and reliable termination of the routine. It is assumed that the requested accuracy has
     * been achieved. errorStatus > 0 for abnormal termination of the routine. The estimates for the integral and error
     * are less reliable. It is assumed that the requested accuracy has not been achieved.
     *  
     * errorStatus = 1 maximum number of subdivisions allowed has been achieved. 
     * One can allow more subdivisions by increasing the value of limit (and taking the according dimension adjustments
     * into account). However, if this yields no improvement, it is advised to analyze the integrand in order to
     * determine the integration difficulties. If the position of a local difficulty can be determined( i.e. singularity,
     * discontinuity within the interval), it should be supplied to the routine as an element of the breakPoints array.
     * If necessary, an appropriate special purpose integrator must be used, which is designed for handling the type
     * of difficulty involved. 
     * 
     * errorStatus = 2 The occurrence of roundoff error is detected, which prevents the requested tolerance from 
     * being achieved. The error may be under-estimated.
     *  
     * errorStatus = 3 Extremely bad integrand behavior occurs at some points of the integration interval.
     * 
     * errorStatus = 4 The algorithm does not converge. Roundoff error is detected in the extrapolation table.
     * It is presumed that the requested tolerance cannot be achieved, and that the returned result is the best that can
     * be obtained. 
     * 
     * errorStatus = 5 The integral is probably divergent, or slowly convergent. It must be noted that
     * divergence can occur with any other value of errorStatus > 0. 
     * 
     * errorStatus = 6 The input is invalid because (epsabs <= 0 and epsrel < max(50.0 * relative machine accuracy, 5.0E-29) 
     * or in the case of dqagpe can also happen because npts2 < 2, the breakPoints are specified outside the integration
     * range, or , or limit <= npts.
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
    private double lower;

    /**
     * After the first integration over the intervals (pts[i], pts[i+1]), i = 0, 1, ..., npts2 - 2, the error estimates
     * over some of the intervals may have been increased artificially, in order to put their subdivision forward. If
     * this happens for the subinterval numbered k, ndin[k] is put to 1, otherwise ndin[k] = 0.
     */
    private int[] ndin;

    /** Number of integrand evaluations. */
    private int neval[] = new int[1];

    /** npts = npts2 - 2. */
    private int npts;

    /** npts2 = breakPoints.length. */
    private int npts2;

    /** D1MACH(2) = 2**(emax)*(1 - 2**(-doubleDigits)) = 2**1024*(1 - 2**-53) D1MACH(2) = Double.MAX_VALUE. */
    private double oflow = Double.MAX_VALUE;

    /** Integration limits and the break points of the interval in ascending sequence. */
    private double[] pts;
    
    private double[] resabs = new double[1];
    
    private double[] resasc = new double[1];

    /** Approximation to the integral. */
    private double[] result = new double[1];

    /** The integral approximations of the subintervals. */
    private double[] rlist;

    /** DOCUMENT ME! */
    private int routine;

    /** DOCUMENT ME! */
    private double uflow = Math.pow(2, -1022);

    /** DOCUMENT ME! */
    private double upper;
    
    private boolean selfTest = false;
    
    private int testCase = 1;
    
    private double actualAnswer;

    //~ Constructors ---------------------------------------------------------------------------------------------------
    
    /**
     * Constructor for running self tests.
     */
    public Integration2() {
    	double neweps;
    	double tol;
        selfTest = true;
        
        epmach = 1.0;
        neweps = 1.0;
        
        while (true) {

            if (1.0 == (1.0 + neweps)) {
                break;
            } else {
                epmach = neweps;
                neweps = neweps / 2.0;
            }
        }
        Preferences.debug("epmach = " + epmach + "\n");

        tol = Math.max(50.0 * epmach, 5.0E-29);
        Preferences.debug("tol = " + tol + "\n");
        
        testCase = 1;
        // dqage is an adaptive automatic integrator using a Gauss-Kronod rule.
        // Integrate cos(100*sin(x)) from 0 to PI.
        // The exact answer is PI * j0(100), or roughly 0.06278740.
        // key chooses the order of the integration rule from 1 to 6.
        // Numerical Integral = 0.06278740049149303 after 427 integrand evaluations used
        // Error status = 0 with estimated absolute error = 9.163654279831235E-9
        // Actual answer = 0.06278740049149267
        // Exact error = 3.608224830031759E-16
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
    	
        lower = 0.0;
        upper = Math.PI;
        routine = Integration2.DQAGE;
        key = 6;
        epsabs = 0.0;
        epsrel = 1.0E-3;
        actualAnswer = Math.PI * realResult[0];
        limit = 100;
        
        if ((epsabs <= 0.0) && (epsrel < tol)) {
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
        Preferences.debug("Numerical Integral = " + result[0] + " after " + neval[0] +
        " integrand evaluations used\n");
        Preferences.debug("Error status = " + errorStatus +
        " with estimated absolute error = " + abserr[0] + "\n");
        Preferences.debug("Actual answer = " + actualAnswer + "\n");
        Preferences.debug("Exact error = " + (result[0] - actualAnswer) + "\n");
        
        testCase = 2;
        // Test2 tests dqagie.
        // dqagie is an adaptive quadrature routine for infinite intervals.
        // Integrate log(x)/(1 + 100*x*x) from 0 to infinity
        // The exact answer is -PI*log(10)/20 = -0.3616892
        // Give the type of infinity
        // inf = 1 means bound to infinity
        // inf = -1 means -infinity to bound
        // inf = 2 means -infinity to infinity
        // Numerical Integral = -0.3616892186127024 after 285 integrand evaluations used
        // Error status = 0 with estimated absolute error = 3.016716912995765E-6
        // Actual answer = -0.36168922062077324
        // Exact error = 2.008070820735952E-9
        bound = 0.0;
        inf = 1;
        routine = Integration2.DQAGIE;
        epsabs = 0.0;
        epsrel = 1.0E-3;
        limit = 100;
        actualAnswer = -Math.PI * Math.log(10.0)/20.0;
        
        if ((epsabs <= 0.0) && (epsrel < tol)) {
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
        Preferences.debug("Numerical Integral = " + result[0] + " after " + neval[0] +
        " integrand evaluations used\n");
        Preferences.debug("Error status = " + errorStatus +
        " with estimated absolute error = " + abserr[0] + "\n");
        Preferences.debug("Actual answer = " + actualAnswer + "\n");
        Preferences.debug("Exact error = " + (result[0] - actualAnswer) + "\n");
        
        testCase = 3;
        // Test3 tests dqagpe
        // dqagpe is an adaptive integrator that can handle singularities of the
        // integrand at user specified points.
        // Integrate x**3 * log(abs((x*x-1)*(x*x-2))) from 0 to 3.
        // The exact answer is 61*log(2)+77*log(7)/4 - 27.
        // Numerical Integral = 52.75627387910168 after 609 integrand evaluations used
        // Error status = 0 with estimated absolute error = 0.051715446356638495
        // Actual answer = 52.74074838347144
        // Exact error = 0.015525495630235753
        lower = 0.0;
        upper = 3.0;
        routine = Integration2.DQAGPE;
        breakPoints = new double[4];
        breakPoints[0] = 1.0;
        breakPoints[1] = Math.sqrt(2.0);
        epsabs = 0.0;
        epsrel = 1.0E-3;
        limit = 100;
        npts2 = breakPoints.length;
        npts = npts2 - 2;
        actualAnswer = 61.0*Math.log(2.0) + 77.0*Math.log(7.0)/4.0 - 27.0;
        
        if (npts2 < 2) {
            MipavUtil.displayError("breakPoints array length must be >= 2");
            errorStatus = 6;

            return;
        }

        for (int i = 0; i < (npts2 - 2); i++) {

            if (breakPoints[i] < lower) {
                MipavUtil.displayError("Break point " + breakPoints[i] + " is impossibly < lower");
                errorStatus = 6;

                return;
            }

            if (breakPoints[i] > upper) {
                MipavUtil.displayError("Break point " + breakPoints[i] + " is impossibly > upper");
                errorStatus = 6;

                return;
            }
        } // for (int i = 0; i < npts2 - 2; i++)
        
        if ((epsabs <= 0.0) && (epsrel < tol)) {
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
        Preferences.debug("Numerical Integral = " + result[0] + " after " + neval[0] +
        " integrand evaluations used\n");
        Preferences.debug("Error status = " + errorStatus +
        " with estimated absolute error = " + abserr[0] + "\n");
        Preferences.debug("Actual answer = " + actualAnswer + "\n");
        Preferences.debug("Exact error = " + (result[0] - actualAnswer) + "\n");
        
        testCase = 4;
        // Test4 tests dqagse
        // dqagse is an adaptive integrator for endpoint singularities.
        // Integrate log(x)/sqrt(x) from 0 to 1.
        // The exact answer is -4.
        // Numerical Integral = -4.000000000000085 after 315 integrand evaluations used
        // Error status = 0 with estimated absolute error = 1.354472090042691E-13
        // Actual answer = -4.0
        // Exact error = -8.526512829121202E-14
        lower = 0.0;
        upper = 1.0;
        routine = Integration2.DQAGSE;
        epsabs = 0.0;
        epsrel = 1.0E-3;
        limit = 100;
        actualAnswer = -4.0;
        
        if ((epsabs <= 0.0) && (epsrel < tol)) {
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
        Preferences.debug("Numerical Integral = " + result[0] + " after " + neval[0] +
        " integrand evaluations used\n");
        Preferences.debug("Error status = " + errorStatus +
        " with estimated absolute error = " + abserr[0] + "\n");
        Preferences.debug("Actual answer = " + actualAnswer + "\n");
        Preferences.debug("Exact error = " + (result[0] - actualAnswer) + "\n");
        
        testCase = 9;
        // test9 tests dqk15
        // dqk15 is a Gauss-Kronod quadrature rule.
        lower = 0.0;
        upper = 1.0;
        routine = Integration2.DQK15;
        actualAnswer = -4.0/9.0;
        // Numerical Integral = -0.44453824758327637
        // Estimated absolute error = 0.20176778110470117
        // Actual answer = -0.4444444444444444
        // Exact error = -9.380313883194935E-5
        // resabs[0] = 0.44453824758327637
        // resasc[0] = 0.20176778110470117
        
        driver();
        
        Preferences.debug("Test 9 testing dqk15\n");
        Preferences.debug("Integrand is sqrt(x) * log(x)\n");
        Preferences.debug("Integrand lower endpoint = 0.0\n");
        Preferences.debug("Integrand upper endpoint = 1.0\n");
        Preferences.debug("Numerical Integral = " + result[0]+ "\n");
        Preferences.debug("Estimated absolute error = " + abserr[0] + "\n");
        Preferences.debug("Actual answer = " + actualAnswer + "\n");
        Preferences.debug("Exact error = " + (result[0] - actualAnswer) + "\n");
        Preferences.debug("resabs[0] = " + resabs[0] + "\n");
        Preferences.debug("resasc[0] = " + resasc[0] + "\n");
        
        testCase = 10;
        // test10 tests dqk21
        // dqk21 is a Gauss-Kronod quadrature rule.
        lower = 0.0;
        upper = 1.0;
        routine = Integration2.DQK21;
        actualAnswer = -4.0/9.0;
        // Numerical Integral = -0.44448120174773076
        // Estimated absolute error = 0.06213734569239914
        // Actual answer = -0.4444444444444444
        // Exact error = -3.675730328633886E-5
        // resabs[0] = 0.44448120174773076
        // resasc[0] = 0.20102031799671913
        
        driver();
        
        Preferences.debug("Test 10 testing dqk21\n");
        Preferences.debug("Integrand is sqrt(x) * log(x)\n");
        Preferences.debug("Integrand lower endpoint = 0.0\n");
        Preferences.debug("Integrand upper endpoint = 1.0\n");
        Preferences.debug("Numerical Integral = " + result[0]+ "\n");
        Preferences.debug("Estimated absolute error = " + abserr[0] + "\n");
        Preferences.debug("Actual answer = " + actualAnswer + "\n");
        Preferences.debug("Exact error = " + (result[0] - actualAnswer) + "\n");
        Preferences.debug("resabs[0] = " + resabs[0] + "\n");
        Preferences.debug("resasc[0] = " + resasc[0] + "\n");
        
        testCase = 11;
        // test11 tests dqk31
        // dqk31 is a Gauss-Kronod quadrature rule.
        lower = 0.0;
        upper = 1.0;
        routine = Integration2.DQK31;
        actualAnswer = -4.0/9.0;
        // Numerical Integral = -0.4444571142381011
        // Estimated absolute error = 0.013135187473747374
        // Actual answer = -0.4444444444444444
        // Exact error = -1.2669793656661099E-5
        // resabs[0] = 0.4444571142381011
        // resasc[0] = 0.20044662305437475
        
        driver();
        
        Preferences.debug("Test 11 testing dqk31\n");
        Preferences.debug("Integrand is sqrt(x) * log(x)\n");
        Preferences.debug("Integrand lower endpoint = 0.0\n");
        Preferences.debug("Integrand upper endpoint = 1.0\n");
        Preferences.debug("Numerical Integral = " + result[0]+ "\n");
        Preferences.debug("Estimated absolute error = " + abserr[0] + "\n");
        Preferences.debug("Actual answer = " + actualAnswer + "\n");
        Preferences.debug("Exact error = " + (result[0] - actualAnswer) + "\n");
        Preferences.debug("resabs[0] = " + resabs[0] + "\n");
        Preferences.debug("resasc[0] = " + resasc[0] + "\n");
        
        testCase = 12;
        // test12 tests dqk41
        // dqk41 is a Gauss-Kronod quadrature rule.
        lower = 0.0;
        upper = 1.0;
        routine = Integration2.DQK41;
        actualAnswer = -4.0/9.0;
        // Numerical Integral = -0.4444502553565426
        // Estimated absolute error = 0.004242965678842284
        // Actual answer = -0.4444444444444444
        // Exact error = -5.8109120981697515E-6
        // resabs[0] = 0.4444502553565426
        // resasc[0] = 0.20065010692415908
        
        driver();
        
        Preferences.debug("Test 12 testing dqk41\n");
        Preferences.debug("Integrand is sqrt(x) * log(x)\n");
        Preferences.debug("Integrand lower endpoint = 0.0\n");
        Preferences.debug("Integrand upper endpoint = 1.0\n");
        Preferences.debug("Numerical Integral = " + result[0]+ "\n");
        Preferences.debug("Estimated absolute error = " + abserr[0] + "\n");
        Preferences.debug("Actual answer = " + actualAnswer + "\n");
        Preferences.debug("Exact error = " + (result[0] - actualAnswer) + "\n");
        Preferences.debug("resabs[0] = " + resabs[0] + "\n");
        Preferences.debug("resasc[0] = " + resasc[0] + "\n");
        
        testCase = 13;
        // test13 tests dqk51
        // dqk51 is a Gauss-Kronod quadrature rule.
        lower = 0.0;
        upper = 1.0;
        routine = Integration2.DQK51;
        actualAnswer = -4.0/9.0;
        // Numerical Integral = -0.4444476169318261
        // Estimated absolute error = 0.001742944355024998
        // Actual answer = -0.4444444444444444
        // Exact error = -3.1724873816862953E-6
        // resabs[0] = 0.4444476169318261
        // resasc[0] = 0.20079987986805534
        
        driver();
        
        Preferences.debug("Test 13 testing dqk51\n");
        Preferences.debug("Integrand is sqrt(x) * log(x)\n");
        Preferences.debug("Integrand lower endpoint = 0.0\n");
        Preferences.debug("Integrand upper endpoint = 1.0\n");
        Preferences.debug("Numerical Integral = " + result[0]+ "\n");
        Preferences.debug("Estimated absolute error = " + abserr[0] + "\n");
        Preferences.debug("Actual answer = " + actualAnswer + "\n");
        Preferences.debug("Exact error = " + (result[0] - actualAnswer) + "\n");
        Preferences.debug("resabs[0] = " + resabs[0] + "\n");
        Preferences.debug("resasc[0] = " + resasc[0] + "\n");
        
        testCase = 14;
        // test14 tests dqk61
        // dqk61 is a Gauss-Kronod quadrature rule.
        lower = 0.0;
        upper = 1.0;
        routine = Integration2.DQK61;
        actualAnswer = -4.0/9.0;
        // Numerical Integral = -0.4444463651893372
        // Estimated absolute error = 8.376473933975456E-4
        // Actual answer = -0.4444444444444444
        // Exact error = -1.920744892802695E-6
        // resabs[0] = 0.4444463651893372
        // resasc[0] = 0.2006334575268779
        
        driver();
        
        Preferences.debug("Test 14 testing dqk61\n");
        Preferences.debug("Integrand is sqrt(x) * log(x)\n");
        Preferences.debug("Integrand lower endpoint = 0.0\n");
        Preferences.debug("Integrand upper endpoint = 1.0\n");
        Preferences.debug("Numerical Integral = " + result[0]+ "\n");
        Preferences.debug("Estimated absolute error = " + abserr[0] + "\n");
        Preferences.debug("Actual answer = " + actualAnswer + "\n");
        Preferences.debug("Exact error = " + (result[0] - actualAnswer) + "\n");
        Preferences.debug("resabs[0] = " + resabs[0] + "\n");
        Preferences.debug("resasc[0] = " + resasc[0] + "\n");
        
        testCase = 15;
        // Test15 tests dqng
        // dqng is a nonadaptive automatic integrator using a Gauss-Kronod
        // Patterson rule.
        // Integrate sqrt(x) * log(x) from 0 to 1.
        // The exact answer is -4.0/9.0.
        // Numerical Integral = -0.44444458538420456 after 87 integrand evaluations used
        // Error status = 0 with estimated absolute error = 2.1889792399460097E-5
        // Actual answer = -0.4444444444444444
        // Exact error = -1.4093976014040166E-7
        lower = 0.0;
        upper = 1.0;
        routine = Integration2.DQNG;
        epsabs = 0.0;
        epsrel = 1.0E-3;
        actualAnswer = -4.0/9.0;
        
        if ((epsabs <= 0.0) && (epsrel < tol)) {
            MipavUtil.displayError("Must set epsabs > 0.0 or must set epsrel >= " + tol);
            errorStatus = 6;

            return;
        }
        
        driver();
        
        Preferences.debug("Test 15 testing dqng\n");
        Preferences.debug("Integrand is sqrt(x) * log(x)\n");
        Preferences.debug("Integrand lower endpoint = 0.0\n");
        Preferences.debug("Integrand upper endpoint = 1.0\n");
        Preferences.debug("Numerical Integral = " + result[0] + " after " + neval[0] +
        " integrand evaluations used\n");
        Preferences.debug("Error status = " + errorStatus +
        " with estimated absolute error = " + abserr[0] + "\n");
        Preferences.debug("Actual answer = " + actualAnswer + "\n");
        Preferences.debug("Exact error = " + (result[0] - actualAnswer) + "\n");
        
        testCase = 101;
        lower = 0.0;
        upper = 1.0;
        routine = Integration2.DQAGPE;
        breakPoints = new double[4];
        breakPoints[0] = 1.0/7.0;
        breakPoints[1] = 2.0/3.0;
        epsabs = 0.0;
        epsrel = 1.0E-3;
        limit = 100;
        
        npts2 = breakPoints.length;
        npts = npts2 - 2;

        if (npts2 < 2) {
            MipavUtil.displayError("breakPoints array length must be >= 2");
            errorStatus = 6;

            return;
        }

        for (int i = 0; i < (npts2 - 2); i++) {

            if (breakPoints[i] < lower) {
                MipavUtil.displayError("Break point " + breakPoints[i] + " is impossibly < lower");
                errorStatus = 6;

                return;
            }

            if (breakPoints[i] > upper) {
                MipavUtil.displayError("Break point " + breakPoints[i] + " is impossibly > upper");
                errorStatus = 6;

                return;
            }
        } // for (int i = 0; i < npts2 - 2; i++)
        
        if ((epsabs <= 0.0) && (epsrel < tol)) {
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
        Preferences.debug("Numerical Integral = " + result[0] + " after " + neval[0] +
        " integrand evaluations used\n");
        Preferences.debug("Error status = " + errorStatus +
        " with absolute error = " + abserr[0] + "\n");
        
        testCase = 102;
        bound = 0.0;
        routine = Integration2.DQAGIE;
        inf = 1;
        epsabs = 0.0;
        epsrel = 1.0E-3;
        limit = 100;
        
        if ((inf != 1) && (inf != -1) && (inf != 2)) {
            MipavUtil.displayError("inf must equal -1, 1, or 2");
            errorStatus = 6;

            return;
        }
        
        if ((epsabs <= 0.0) && (epsrel < tol)) {
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
        Preferences.debug("Numerical Integral = " + result[0] + " after " + neval[0] +
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
    public Integration2(double lower, double upper, int routine, double epsabs, double epsrel) {
        double neweps;
        double tol;

        this.lower = lower;
        this.upper = upper;
        this.routine = routine;

        if (routine != DQNG) {
            MipavUtil.displayError("routine must be DQAGSE with this constructor");

            return;
        }

        this.epsabs = epsabs;
        this.epsrel = epsrel;

        epmach = 1.0;
        neweps = 1.0;

        while (true) {

            if (1.0 == (1.0 + neweps)) {
                break;
            } else {
                epmach = neweps;
                neweps = neweps / 2.0;
            }
        }

        tol = Math.max(50.0 * epmach, 5.0E-29);

        if ((epsabs <= 0.0) && (epsrel < tol)) {
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
    public Integration2(double bound, int routine, int inf, double epsabs, double epsrel, int limit) {
        double neweps;
        double tol;
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

        epmach = 1.0;
        neweps = 1.0;

        while (true) {

            if (1.0 == (1.0 + neweps)) {
                break;
            } else {
                epmach = neweps;
                neweps = neweps / 2.0;
            }
        }

        tol = Math.max(50.0 * epmach, 5.0E-29);

        if ((epsabs <= 0.0) && (epsrel < tol)) {
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
    public Integration2(double lower, double upper, int routine, double epsabs, double epsrel, int limit) {
        double neweps;
        double tol;

        this.lower = lower;
        this.upper = upper;
        this.routine = routine;

        if (routine != DQAGSE) {
            MipavUtil.displayError("routine must be DQAGSE with this constructor");

            return;
        }

        this.epsabs = epsabs;
        this.epsrel = epsrel;

        epmach = 1.0;
        neweps = 1.0;

        while (true) {

            if (1.0 == (1.0 + neweps)) {
                break;
            } else {
                epmach = neweps;
                neweps = neweps / 2.0;
            }
        }

        tol = Math.max(50.0 * epmach, 5.0E-29);

        if ((epsabs <= 0.0) && (epsrel < tol)) {
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
     * Constructor for Integration Used with routine = DQAWCE.
     *
     * @param  lower    DOCUMENT ME!
     * @param  upper    DOCUMENT ME!
     * @param  c
     * @param  routine  DOCUMENT ME!
     * @param  epsabs   DOCUMENT ME!
     * @param  epsrel   DOCUMENT ME!
     * @param  limit    DOCUMENT ME!
     */
    public Integration2(double lower, double upper, double c, int routine, double epsabs, double epsrel, int limit) {
        double neweps;
        double tol;

        this.lower = lower;
        this.upper = upper;
        this.c = c;
        this.routine = routine;

        if (routine != DQAWCE) {
            MipavUtil.displayError("routine must be DQAWCE with this constructor");

            return;
        }
        
        if ((c == lower) || (c == upper)) {
            MipavUtil.displayError("Cannot have c == lower or c == upper");
            errorStatus = 6;
            
            return;
        }

        this.epsabs = epsabs;
        this.epsrel = epsrel;

        epmach = 1.0;
        neweps = 1.0;

        while (true) {

            if (1.0 == (1.0 + neweps)) {
                break;
            } else {
                epmach = neweps;
                neweps = neweps / 2.0;
            }
        }

        tol = Math.max(50.0 * epmach, 5.0E-29);

        if ((epsabs <= 0.0) && (epsrel < tol)) {
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
    public Integration2(double lower, double upper, int routine, double[] breakPoints, double epsabs, double epsrel,
                        int limit) {
        double neweps;
        double tol;

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

            if (breakPoints[i] < lower) {
                MipavUtil.displayError("Break point " + breakPoints[i] + " is impossibly < lower");
                errorStatus = 6;

                return;
            }

            if (breakPoints[i] > upper) {
                MipavUtil.displayError("Break point " + breakPoints[i] + " is impossibly > upper");
                errorStatus = 6;

                return;
            }
        } // for (int i = 0; i < npts2 - 2; i++)

        this.epsabs = epsabs;
        this.epsrel = epsrel;

        epmach = 1.0;
        neweps = 1.0;

        while (true) {

            if (1.0 == (1.0 + neweps)) {
                break;
            } else {
                epmach = neweps;
                neweps = neweps / 2.0;
            }
        }

        tol = Math.max(50.0 * epmach, 5.0E-29);

        if ((epsabs <= 0.0) && (epsrel < tol)) {
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
    public Integration2(double lower, double upper, int routine, int key, double epsabs, double epsrel, int limit) {
        double neweps;
        double tol;

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

        epmach = 1.0;
        neweps = 1.0;

        while (true) {

            if (1.0 == (1.0 + neweps)) {
                break;
            } else {
                epmach = neweps;
                neweps = neweps / 2.0;
            }
        }

        tol = Math.max(50.0 * epmach, 5.0E-29);

        if ((epsabs <= 0.0) && (epsrel < tol)) {
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
    public abstract double intFunc(double x);
    
    private double intFuncTest(double x) {
        double function = 0.0;
        switch (testCase) {
        case 1:
        	function = Math.cos(100.0 * Math.sin(x));
        	break;
        case 2:
        	function = Math.log(x)/(1.0 + 100.0*x*x);
        	break;
        case 3:
        	function = x*x*x*Math.log(Math.abs((x*x - 1.0)*(x*x - 2.0)));
        	break;
        case 4:
        	function = Math.log(x)/Math.sqrt(x);
        	break;
        case 9:
        case 10:
        case 11:
        case 12:
        case 13:
        case 14:
        case 15:
        	if (x <= 0.0) {
        		function = 0.0;
        	}
        	else {
        		function = Math.sqrt(x) * Math.log(x);
        	}
        	break;
        case 101:
        	if ((x != 1.0/7.0) && (x != 2.0/3.0)) {
                function = Math.pow(Math.abs(x - 1.0/7.0), -0.25) *
                           Math.pow(Math.abs(x - 2.0/3.0), -0.55);
              }
        	break;
        case 102:
        	if (x > 0.0) {
                function = Math.sqrt(x) * Math.log(x)/((x + 1.0) * (x + 2.0));
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
        double[] rlist2;

        // Index to the interval with the largest error estimate
        int[] maxErr = new int[1];

        // errMax[0] = elist[maxErr];
        double[] errMax = new double[1];

        // error on the interval currently subdivided (before that subdivision
        // has taken place)
        double erlast;

        // Sum of the integrals over the subintervals
        double area;

        // Sum of the errors over the subintervals
        double errSum;

        // Requested accuracy max(epsabs, epsrel * abs(result))
        double errBnd;

        // Number of calls to the extrapolation routine
        int[] nres = new int[1];

        // Number of elements in rlist2.  If an appropriate approximation
        // to the compounded integral has been obtained, it is put in
        // rlist2[numr12-1] after numr12 has been increased by one.
        int[] numr12 = new int[1];

        // Sum of the errors over the intervals larger than the smallest
        // interval considered up to now
        double erlarg;

        // boolean variable denoting that the routine is attempting to
        // perform extrapolation.  That is, before dividing the
        // interval we try to decrease the value of erlarg
        boolean extrap;

        // If noext is true, the extrapolation is no longer allowed.
        boolean noext;

        // length of the smallest interval considered up to now, multiplied
        // by 1.5
        double small;
        double[] res31a;
        double[] abseps = new double[1];
        double[] area1 = new double[1];
        double area12;
        double[] area2 = new double[1];
        double a1;
        double a2;
        double boun;
        double b1;
        double b2;
        double correc;
        double[] defabs = new double[1];
        double[] defab1 = new double[1];
        double[] defab2 = new double[1];
        double dres;
        double[] error1 = new double[1];
        double[] error2 = new double[1];
        double error12;
        double ertest;
        double[] resabs = new double[1];
        double[] reseps = new double[1];
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
            alist = new double[limit];
            blist = new double[limit];
            rlist = new double[limit];
            elist = new double[limit];
            iord = new int[limit];
            res31a = new double[3];
            rlist2 = new double[52];

            errorStatus = 0;
            neval[0] = 0;
            last = 0;
            result[0] = 0.0;
            abserr[0] = 0.0;
            alist[0] = 0.0;
            blist[0] = 1.0;
            rlist[0] = 0.0;
            elist[0] = 0.0;
            iord[0] = -1;

            // First approximation to the integral

            // Determine the integral to be mapped onto (0,1).
            // if inf = 2, the integral is computed as i = i1 + i2, where
            // i1 = integral of f over(-infinity, 0)
            // i2 = integral of f over (0, +infinity)

            boun = bound;

            if (inf == 2) {
                boun = 0.0;
            } // if (inf == 2)

            dqk15i(boun, inf, 0.0, 1.0, result, abserr, defabs, resabs);

            // Test on accuracy

            last = 1;
            rlist[0] = result[0];
            elist[0] = abserr[0];
            iord[0] = 0;
            dres = Math.abs(result[0]);
            errBnd = Math.max(epsabs, epsrel * dres);

            if ((abserr[0] <= (100.0 * epmach * defabs[0])) && (abserr[0] > errBnd)) {
                errorStatus = 2;
            } // if ((abserr[0] <= 100.0 * epmach * defabs[0]) &&

            if (limit == 1) {
                errorStatus = 1;
            } // if (limit == 1)

            if ((errorStatus != 0) || ((abserr[0] <= errBnd) && (abserr[0] != resabs[0])) || (abserr[0] == 0.0)) {
                neval[0] = (30 * last) - 15;

                if (inf == 2) {
                    neval[0] = 2 * neval[0];
                } // if (inf == 2)

                if (errorStatus > 2) {
                    errorStatus = errorStatus - 1;
                } // if (errorStatus > 2)

                return;
            } // if ((errorStatus != 0) ||

            // Initialization

            rlist2[0] = result[0];
            errMax[0] = abserr[0];
            maxErr[0] = 0;
            area = result[0];
            errSum = abserr[0];
            abserr[0] = oflow;
            nrmax[0] = 1;
            nres[0] = 0;
            ktmin = 0;
            numr12[0] = 2;
            extrap = false;
            noext = false;
            erlarg = errSum;
            correc = erlarg;
            small = 0.375;
            ertest = errBnd;
            ierro = 0;
            iroff1 = 0;
            iroff2 = 0;
            iroff3 = 0;
            ksgn = -1;

            if (dres >= ((1.0 - (50.0 * epmach)) * defabs[0])) {
                ksgn = 1;
            } // if (dres >= (1.0 - 50.0 * epmach)*defabs[0])

// main do-loop
loop:
            for (last = 2; last <= limit; last++) {

                // Bisect the interval with the nrmax-th largest error estimate.

                a1 = alist[maxErr[0]];
                b1 = 0.5 * (alist[maxErr[0]] + blist[maxErr[0]]);
                a2 = b1;
                b2 = blist[maxErr[0]];
                erlast = errMax[0];
                dqk15i(boun, inf, a1, b1, area1, error1, resabs, defab1);
                dqk15i(boun, inf, a2, b2, area2, error2, resabs, defab2);

                // Improve previous approximations to integral and error
                // and test for accuracy

                area12 = area1[0] + area2[0];
                error12 = error1[0] + error2[0];
                errSum = errSum + error12 - errMax[0];
                area = area + area12 - rlist[maxErr[0]];

                if ((defab1[0] != error1[0]) && (defab2[0] != error2[0])) {

                    if ((Math.abs(rlist[maxErr[0]] - area12) <= (1.0E-5 * Math.abs(area12))) &&
                            (error12 >= (0.99 * errMax[0]))) {

                        if (extrap) {
                            iroff2 = iroff2 + 1;
                        } else {
                            iroff1 = iroff1 + 1;
                        }
                    } // if ((Math.abs(rlist[maxErr[0]] - area12) <=

                    if ((last > 10) && (error12 > errMax[0])) {
                        iroff3 = iroff3 + 1;
                    } // if ((last > 10) && (error12 > errMax[0]))
                } // if ((defab1[0] != error1[0]) && (defab2[0] != error2[0]))

                rlist[maxErr[0]] = area1[0];
                rlist[last - 1] = area2[0];
                errBnd = Math.max(epsabs, epsrel * Math.abs(area));

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
                if (Math.max(Math.abs(a1), Math.abs(b2)) <=
                        ((1.0 + (100.0 * epmach)) * (Math.abs(a2) + (1.0E3 * uflow)))) {
                    errorStatus = 4;
                } // if (Math.max(Math.abs(a1), Math.abs(b2)) <=

                // Append the newly-created intervals to the list.

                if (error2[0] <= error1[0]) {
                    alist[last - 1] = a2;
                    blist[maxErr[0]] = b1;
                    blist[last - 1] = b2;
                    elist[maxErr[0]] = error1[0];
                    elist[last - 1] = error2[0];
                } // if (error2[0] <= error1[0])
                else {
                    alist[maxErr[0]] = a2;
                    alist[last - 1] = a1;
                    blist[last - 1] = b1;
                    rlist[maxErr[0]] = area2[0];
                    rlist[last - 1] = area1[0];
                    elist[maxErr[0]] = error2[0];
                    elist[last - 1] = error1[0];
                } // else

                // Call the subroutine dqpsrt to maintain the descending ordering
                // in the list of error estimates and select the subinterval
                // with nrmax-th largest error estimate (to be bisected next).

                dqpsrt(limit, last, maxErr, errMax, elist, iord, nrmax);

                if (errSum <= errBnd) {
                    result[0] = 0.0;

                    for (k = 0; k < last; k++) {
                        result[0] = result[0] + rlist[k];
                    } // for (k = 0; k < last; k++)

                    abserr[0] = errSum;
                    neval[0] = (30 * last) - 15;

                    if (inf == 2) {
                        neval[0] = 2 * neval[0];
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
                    small = 0.375;
                    erlarg = errSum;
                    ertest = errBnd;
                    rlist2[1] = area;

                    continue;
                } // if (last == 2)

                if (noext) {
                    continue;
                } // if (noext)

                erlarg = erlarg - erlast;

                if (Math.abs(b1 - a1) > small) {
                    erlarg = erlarg + error12;
                } // if (Math.abs(b1 - a1) > small)

                if (!extrap) {

                    // Test whether the interval to be bisected next is the
                    // smallest interval

                    if (Math.abs(blist[maxErr[0]] - alist[maxErr[0]]) > small) {
                        continue;
                    } // if (Math.abs(blist[maxErr[0]] - alist[maxErr[0]]) > small)

                    extrap = true;
                    nrmax[0] = 2;
                } // if (!extrap)

                if ((ierro != 3) && (erlarg > ertest)) {

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
                        errMax[0] = elist[maxErr[0]];

                        if (Math.abs(blist[maxErr[0]] - alist[maxErr[0]]) > small) {
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

                if ((ktmin > 5) && (abserr[0] < (1.0E-3 * errSum))) {
                    errorStatus = 5;
                } // if ((ktmin > 5) && (abserr[0] < 1.0E-3 * errSum))

                if (abseps[0] < abserr[0]) {
                    ktmin = 0;
                    abserr[0] = abseps[0];
                    result[0] = reseps[0];
                    correc = erlarg;
                    ertest = Math.max(epsabs, epsrel * Math.abs(reseps[0]));

                    if (abserr[0] <= ertest) {
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
                errMax[0] = elist[maxErr[0]];
                nrmax[0] = 1;
                extrap = false;
                small = 0.5 * small;
                erlarg = errSum;
            } // for (last = 2; last <= limit; last++)

            // Set final result and error estimate

            if (abserr[0] != oflow) {

                if ((errorStatus + ierro) != 0) {

                    if (ierro == 3) {
                        abserr[0] = abserr[0] + correc;
                    } // if (ierro == 3)

                    if (errorStatus == 0) {
                        errorStatus = 3;
                    } // if (errorStatus == 0)

                    if ((result[0] == 0.0) || (area == 0.0)) {

                        if (abserr[0] > errSum) {
                            result[0] = 0.0;

                            for (k = 0; k < last; k++) {
                                result[0] = result[0] + rlist[k];
                            } // for (k = 0; k < last; k++)

                            abserr[0] = errSum;
                            neval[0] = (30 * last) - 15;

                            if (inf == 2) {
                                neval[0] = 2 * neval[0];
                            } // if (inf == 2)

                            if (errorStatus > 2) {
                                errorStatus = errorStatus - 1;
                            } // if (errorStatus > 2)

                            return;
                        } // if (abserr[0] > errSum)

                        if (area == 0.0) {
                            neval[0] = (30 * last) - 15;

                            if (inf == 2) {
                                neval[0] = 2 * neval[0];
                            } // if (inf == 2)

                            if (errorStatus > 2) {
                                errorStatus = errorStatus - 1;
                            } // if (errorStatus > 2)

                            return;
                        } // if (area == 0.0)

                        if ((ksgn == -1) && (Math.max(Math.abs(result[0]), Math.abs(area)) <= (1.0E-2 * defabs[0]))) {
                            neval[0] = (30 * last) - 15;

                            if (inf == 2) {
                                neval[0] = 2 * neval[0];
                            } // if (inf == 2)

                            if (errorStatus > 2) {
                                errorStatus = errorStatus - 1;
                            } // if (errorStatus > 2)

                            return;
                        } // if ((ksgn == -1) && (Math.max(Math.abs(result[0]),

                        if ((1.0E-2 > (result[0] / area)) || ((result[0] / area) > 100.0) ||
                                (errSum > Math.abs(area))) {
                            errorStatus = 6;
                        } // if ((1.0E-2 > (result[0]/area)) || (result[0]/area > 100.0) ||

                        neval[0] = (30 * last) - 15;

                        if (inf == 2) {
                            neval[0] = 2 * neval[0];
                        } // if (inf == 2)

                        if (errorStatus > 2) {
                            errorStatus = errorStatus - 1;
                        } // if (errorStatus > 2)

                        return;
                    } // if (result[0] == 0.0) || (area == 0.0))

                    if ((abserr[0] / Math.abs(result[0])) > (errSum / Math.abs(area))) {
                        result[0] = 0.0;

                        for (k = 0; k < last; k++) {
                            result[0] = result[0] + rlist[k];
                        } // for (k = 0; k < last; k++)

                        abserr[0] = errSum;
                        neval[0] = (30 * last) - 15;

                        if (inf == 2) {
                            neval[0] = 2 * neval[0];
                        } // if (inf == 2)

                        if (errorStatus > 2) {
                            errorStatus = errorStatus - 1;
                        } // if (errorStatus > 2)

                        return;
                    } // if (abserr[0]/Math.abs(result[0]) > errSum/Math.abs(area))
                } // if ((errorStatus + ierro) != 0)

                if ((ksgn == -1) && (Math.max(Math.abs(result[0]), Math.abs(area)) <= (1.0E-2 * defabs[0]))) {
                    neval[0] = (30 * last) - 15;

                    if (inf == 2) {
                        neval[0] = 2 * neval[0];
                    } // if (inf == 2)

                    if (errorStatus > 2) {
                        errorStatus = errorStatus - 1;
                    } // if (errorStatus > 2)

                    return;
                } // if ((ksgn == -1) && (Math.max(Math.abs(result[0]),

                if ((1.0E-2 > (result[0] / area)) || ((result[0] / area) > 100.0) || (errSum > Math.abs(area))) {
                    errorStatus = 6;
                } // if ((1.0E-2 > (result[0]/area)) || (result[0]/area > 100.0) ||

                neval[0] = (30 * last) - 15;

                if (inf == 2) {
                    neval[0] = 2 * neval[0];
                } // if (inf == 2)

                if (errorStatus > 2) {
                    errorStatus = errorStatus - 1;
                } // if (errorStatus > 2)

                return;
            } // if (abserr[0] != oflow)

            result[0] = 0.0;

            for (k = 0; k < last; k++) {
                result[0] = result[0] + rlist[k];
            } // for (k = 0; k < last; k++)

            abserr[0] = errSum;
            neval[0] = (30 * last) - 15;

            if (inf == 2) {
                neval[0] = 2 * neval[0];
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
        double[] res31a;

        // rlist2 should be length at least (limexp + 2)
        // rlist2 contains the part of the epsilon table still needed for
        // further computations
        double[] rlist2;

        // Index to the interval with the largest error estimate
        int[] maxErr = new int[1];

        // errMax[0] = elist[maxErr];
        double[] errMax = new double[1];

        // error on the interval currently subdivided (before that subdivision
        // has taken place)
        double erlast;

        // Sum of the integrals over the subintervals
        double area;

        // Sum of the errors over the subintervals
        double errSum;

        // Requested accuracy max(epsabs, epsrel * abs(result))
        double errBnd;

        // Number of calls to the extrapolation routine
        int[] nres = new int[1];

        // Number of elements in rlist2.  If an appropriate approximation
        // to the compounded integral has been obtained, it is put in
        // rlist2[numr12-1] after numr12 has been increased by one.
        int[] numr12 = new int[1];

        // Sum of the errors over the intervals larger than the smallest
        // interval considered up to now
        double erlarg;

        // boolean variable denoting that the routine is attempting to
        // perform extrapolation.  That is, before dividing the
        // interval we try to decrease the value of erlarg
        boolean extrap;

        // If noext is true, the extrapolation is no longer allowed.
        boolean noext;

        // Variables ending in 1 denote left subinterval and variables
        // ending in 2 denote right subinterval
        double[] abseps = new double[1];
        double[] area1 = new double[1];
        double area12;
        double[] area2 = new double[1];
        double a1;
        double a2;
        double b1;
        double b2;
        double correc = 0.0;
        double[] defabs = new double[1];
        double[] defab1 = new double[1];
        double[] defab2 = new double[1];
        double dres;
        double[] error1 = new double[1];
        double error12;
        double[] error2 = new double[1];
        double ertest;
        double[] resa = new double[1];
        double resabs;
        double[] reseps = new double[1];
        double sign;
        double temp;
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
            alist = new double[limit];
            blist = new double[limit];
            rlist = new double[limit];
            elist = new double[limit];
            pts = new double[npts2];
            level = new int[limit];
            ndin = new int[npts2];
            iord = new int[limit];
            res31a = new double[3];
            rlist2 = new double[52];

            errorStatus = 0;
            neval[0] = 0;
            last = 0;
            result[0] = 0.0;
            abserr[0] = 0.0;
            alist[0] = lower;
            blist[0] = upper;
            rlist[0] = 0.0;
            elist[0] = 0.0;
            iord[0] = -1;
            level[0] = 0;

            // If any break points are provided, sort them into an
            // ascending sequence.
            sign = 1.0;

            if (lower > upper) {
                sign = -1.0;
            }

            pts[0] = Math.min(lower, upper);

            if (npts != 0) {

                for (i = 0; i < npts; i++) {
                    pts[i + 1] = breakPoints[i];
                }
            } // if (npts != 0)

            pts[npts + 1] = Math.max(lower, upper);
            nint = npts + 1;
            a1 = pts[0];

            if (npts != 0) {
                nintp1 = nint + 1;

                for (i = 0; i < nint; i++) {
                    ip1 = i + 1;

                    for (j = ip1; j < nintp1; j++) {

                        if (pts[i] > pts[j]) {
                            temp = pts[i];
                            pts[i] = pts[j];
                            pts[j] = temp;
                        } // if (pts[i] > pts[j])
                    } // for (j = ip1; j < nintp1; j++)
                } // for (i = 0; i < nint; i++)
            } // if (npts != 0)

            // Compute first integral and error approximations

            resabs = 0.0;

            for (i = 0; i < nint; i++) {
                b1 = pts[i + 1];
                dqk21(a1, b1, area1, error1, defabs, resa);
                abserr[0] = abserr[0] + error1[0];
                result[0] = result[0] + area1[0];
                ndin[i] = 0;

                if ((error1[0] == resa[0]) && (error1[0] != 0.0)) {
                    ndin[i] = 1;
                }

                resabs = resabs + defabs[0];
                level[i] = 0;
                elist[i] = error1[0];
                alist[i] = a1;
                blist[i] = b1;
                rlist[i] = area1[0];
                iord[i] = i;
                a1 = b1;
            } // for (i = 0; i < nint; i++)

            errSum = 0.0;

            for (i = 0; i < nint; i++) {

                if (ndin[i] == 1) {
                    elist[i] = abserr[0];
                } // if (ndin[i] == 1)

                errSum = errSum + elist[i];
            } // for (i = 0; i < nint; i++)

            // Test on accuracy

            last = nint;
            neval[0] = 21 * nint;
            dres = Math.abs(result[0]);
            errBnd = Math.max(epsabs, dres * epsrel);

            if ((abserr[0] <= (100.0 * epmach * resabs)) && (abserr[0] > errBnd)) {
                errorStatus = 2;
            }

            if (nint != 1) {

                for (i = 0; i < npts; i++) {
                    jlow = i + 1;
                    ind1 = iord[i];

                    for (j = jlow; j < nint; j++) {
                        ind2 = iord[j];

                        if (elist[ind1] <= elist[ind2]) {
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

            if ((errorStatus != 0) || (abserr[0] <= errBnd)) {

                if (errorStatus > 2) {
                    errorStatus = errorStatus - 1;
                } // if (errorStatus > 2)

                result[0] = result[0] * sign;

                return;
            } // if ((errorStatus != 0) || (abserr[0] <= errBnd))

            // Initialization

            rlist2[0] = result[0];
            maxErr[0] = iord[0];
            errMax[0] = elist[maxErr[0]];
            area = result[0];
            nrmax[0] = 1;
            nres[0] = 0;
            numr12[0] = 1;
            ktmin = 0;
            extrap = false;
            noext = false;
            erlarg = errSum;
            ertest = errBnd;
            levmax = 1;
            iroff1 = 0;
            iroff2 = 0;
            iroff3 = 0;
            ierro = 0;
            abserr[0] = oflow;
            ksgn = -1;

            if (dres >= ((1.0 - (50.0 * epmach)) * resabs)) {
                ksgn = 1;
            }

// Main do-loop
loop:
            for (last = npts2; last <= limit; last++) {

                // Bisect the subinterval with the nrmax-th largest error
                // estimate
                levcur = level[maxErr[0]] + 1;
                a1 = alist[maxErr[0]];
                b1 = 0.5 * (alist[maxErr[0]] + blist[maxErr[0]]);
                a2 = b1;
                b2 = blist[maxErr[0]];
                erlast = maxErr[0];
                dqk21(a1, b1, area1, error1, resa, defab1);
                dqk21(a2, b2, area2, error2, resa, defab2);

                // Improve previous approximations to integral and error
                // and test for accuracy.
                neval[0] = neval[0] + 42;
                area12 = area1[0] + area2[0];
                error12 = error1[0] + error2[0];
                errSum = errSum + error12 - errMax[0];
                area = area + area12 - rlist[maxErr[0]];

                if ((defab1[0] != error1[0]) && (defab2[0] != error2[0])) {

                    if ((Math.abs(rlist[maxErr[0]] - area12) <= (1.0E-5 * Math.abs(area12))) &&
                            (error12 >= (0.99 * errMax[0]))) {

                        if (extrap) {
                            iroff2 = iroff2 + 1;
                        } else {
                            iroff1 = iroff1 + 1;
                        }
                    }

                    if ((last > 10) && (error12 > errMax[0])) {
                        iroff3 = iroff3 + 1;
                    }
                } // if ((defab1[0] != error1[0]) && (defab2[0] != error2[0]))

                level[maxErr[0]] = levcur;
                level[last - 1] = levcur;
                rlist[maxErr[0]] = area1[0];
                rlist[last - 1] = area2[0];
                errBnd = Math.max(epsabs, epsrel * Math.abs(area));

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

                if (Math.max(Math.abs(a1), Math.abs(b2)) <=
                        ((1.0 + (100.0 * epmach)) * (Math.abs(a2) + (1.0E3 * uflow)))) {
                    errorStatus = 4;
                }

                // Append the newly created intervals to the list

                if (error2[0] <= error1[0]) {
                    alist[last - 1] = a2;
                    blist[maxErr[0]] = b1;
                    blist[last - 1] = b2;
                    elist[maxErr[0]] = error1[0];
                    elist[last - 1] = error2[0];
                } else {
                    alist[maxErr[0]] = a2;
                    alist[last - 1] = a1;
                    blist[last - 1] = b1;
                    rlist[maxErr[0]] = area2[0];
                    rlist[last - 1] = area1[0];
                    elist[maxErr[0]] = error2[0];
                    elist[last - 1] = error1[0];
                }

                // Call subroutine dqpsrt to maintain the descending
                // ordering in the list of error estimates and select
                // the subinterval with nrmax-th largest error estimate
                // (to be bisected next).

                dqpsrt(limit, last, maxErr, errMax, elist, iord, nrmax);

                if (errSum <= errBnd) {
                    result[0] = 0.0;

                    for (k = 0; k < last; k++) {
                        result[0] = result[0] + rlist[k];
                    }

                    abserr[0] = errSum;

                    if (errorStatus > 2) {
                        errorStatus = errorStatus - 1;
                    }

                    result[0] = result[0] * sign;

                    return;
                } // if (errSum <= errBnd)

                if (errorStatus != 0) {
                    break;
                } // if (errorStatus != 0)

                if (noext) {
                    continue;
                } // if (noext)

                erlarg = erlarg - erlast;

                if ((levcur + 1) <= levmax) {
                    erlarg = erlarg + error12;
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

                if ((ierro != 3) && (erlarg > ertest)) {
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
                        errMax[0] = elist[maxErr[0]];

                        if ((level[maxErr[0]] + 1) <= levmax) {
                            continue loop;
                        } // if ((level[maxErr[0]] + 1) <= levmax)

                        nrmax[0] = nrmax[0] + 1;
                    } // for (k = id; k <= jupbnd; k++)
                } // if ((ierro != 3) && (erlarg > ertest))

                // Perform extrapolation

                numr12[0] = numr12[0] + 1;
                rlist2[numr12[0] - 1] = area;

                if (numr12[0] > 2) {
                    dqelg(numr12, rlist2, reseps, abseps, res31a, nres);
                    ktmin = ktmin + 1;

                    if ((ktmin > 5) && (abserr[0] < (1.0E-3 * errSum))) {
                        errorStatus = 5;
                    } // if ((ktmin > 5) && (abserr < 1.0E-3 * errSum))

                    if (abseps[0] < abserr[0]) {
                        ktmin = 0;
                        abserr[0] = abseps[0];
                        result[0] = reseps[0];
                        correc = erlarg;
                        ertest = Math.max(epsabs, epsrel * Math.abs(reseps[0]));

                        if (abserr[0] < ertest) {
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
                errMax[0] = elist[maxErr[0]];
                nrmax[0] = 1;
                extrap = false;
                levmax = levmax + 1;
                erlarg = errSum;
            } // for (last = npts2; last <= limit; last++)

            // Set the final result.

            if (abserr[0] != oflow) {

                if ((errorStatus + ierro) != 0) {

                    if (ierro == 3) {
                        abserr[0] = abserr[0] + correc;
                    }

                    if (errorStatus == 0) {
                        errorStatus = 3;
                    } // if (errorStatus == 0)

                    if ((result[0] == 0.0) || (area == 0.0)) {

                        if (abserr[0] > errSum) {
                            result[0] = 0.0;

                            for (k = 0; k < last; k++) {
                                result[0] = result[0] + rlist[k];
                            }

                            abserr[0] = errSum;

                            if (errorStatus > 2) {
                                errorStatus = errorStatus - 1;
                            }

                            result[0] = result[0] * sign;

                            return;
                        } // if (abserr[0] > errSum)

                        if (area == 0.0) {

                            if (errorStatus > 2) {
                                errorStatus = errorStatus - 1;
                            }

                            result[0] = result[0] * sign;

                            return;
                        } // if (area == 0.0)

                        if ((ksgn == -1) && (Math.max(Math.abs(result[0]), Math.abs(area)) <= (1.0E-2 * resabs))) {

                            if (errorStatus > 2) {
                                errorStatus = errorStatus - 1;
                            }

                            result[0] = result[0] * sign;

                            return;
                        } // if ((ksgn == -1) && (Math.max(Math.abs(result[0]),

                        if ((1.0E-2 > (result[0] / area)) || ((result[0] / area) > 100.0) ||
                                (errSum > Math.abs(area))) {
                            errorStatus = 6;
                        } // if ((1.0E-2 > (result[0]/area) || ((result[0]/area) >

                        if (errorStatus > 2) {
                            errorStatus = errorStatus - 1;
                        }

                        result[0] = result[0] * sign;

                        return;
                    } // if ((result[0] == 0.0) || (area == 0.0))

                    if ((abserr[0] / Math.abs(result[0])) > (errSum / Math.abs(area))) {
                        result[0] = 0.0;

                        for (k = 0; k < last; k++) {
                            result[0] = result[0] + rlist[k];
                        }

                        abserr[0] = errSum;

                        if (errorStatus > 2) {
                            errorStatus = errorStatus - 1;
                        }

                        result[0] = result[0] * sign;

                        return;
                    } // if ((abserr[0]/Math.abs(result[0])) > (errSum/Math.abs(area)))
                } // if ((errorStatus + ierro) != 0)

                if ((ksgn == -1) && (Math.max(Math.abs(result[0]), Math.abs(area)) <= (1.0E-2 * resabs))) {

                    if (errorStatus > 2) {
                        errorStatus = errorStatus - 1;
                    }

                    result[0] = result[0] * sign;

                    return;
                } // if ((ksgn == -1) && (Math.max(Math.abs(result[0]),

                if ((1.0E-2 > (result[0] / area)) || ((result[0] / area) > 100.0) || (errSum > Math.abs(area))) {
                    errorStatus = 6;
                } // if ((1.0E-2 > (result[0]/area) || ((result[0]/area) >

                if (errorStatus > 2) {
                    errorStatus = errorStatus - 1;
                }

                result[0] = result[0] * sign;

                return;
            } // if (abserr[0] != oflow)

            result[0] = 0.0;

            for (k = 0; k < last; k++) {
                result[0] = result[0] + rlist[k];
            }

            abserr[0] = errSum;

            if (errorStatus > 2) {
                errorStatus = errorStatus - 1;
            }

            result[0] = result[0] * sign;

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
                
            case DQAWCE:
            	dqawce();
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
    public double getAbserr() {
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
    public double getIntegral() {
        return result[0];
    }

    /**
     * DOCUMENT ME!
     *
     * @return  neval
     */
    public int getNeval() {
        return neval[0];
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
	 c        double precision version

     * Calculates an integral over (lower, upper), hopefully satisfying the abs(actual integral - result) <= max(epsabs,
     * epsresl * abs(actual integral)).
     */
    private void dqage() {

        // Variables ending in 1 denote left subinterval and variables
        // ending in 2 denote right subinterval
        // Index to the interval with the largest error estimate
        int[] maxErr = new int[1];

        // errMax[0] = elist[maxErr];
        double[] errMax = new double[1];

        // Sum of the integrals over the subintervals
        double area;

        // Sum of the errors over the subintervals
        double errSum;

        // Requested accuracy max(epsabs, epsrel * abs(result))
        double errBnd;
        double[] area1 = new double[1];
        double area12;
        double[] area2 = new double[1];
        double a1;
        double a2;
        double b1;
        double b2;
        double[] defabs = new double[1];
        double[] defab1 = new double[1];
        double[] defab2 = new double[1];
        double[] error1 = new double[1];
        double[] error2 = new double[1];
        double error12;
        double[] resabs = new double[1];
        int iroff1;
        int iroff2;
        int k;
        int keyf;
        int[] nrmax = new int[1];

        try {
            alist = new double[limit];
            blist = new double[limit];
            rlist = new double[limit];
            elist = new double[limit];
            iord = new int[limit];

            errorStatus = 0;
            neval[0] = 0;
            last = 0;
            result[0] = 0.0;
            abserr[0] = 0.0;
            alist[0] = lower;
            blist[0] = upper;
            rlist[0] = 0.0;
            elist[0] = 0.0;
            iord[0] = -1;

            // First approximation to the integral

            keyf = key;

            if (key <= 0) {
                keyf = 1;
            } // if (key <= 0)

            if (key >= 7) {
                keyf = 6;
            } // if (key >= 7)

            neval[0] = 0;

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
            rlist[0] = result[0];
            elist[0] = abserr[0];
            iord[0] = 0;

            // Test on accuracy

            errBnd = Math.max(epsabs, epsrel * Math.abs(result[0]));

            if ((abserr[0] <= (50.0 * epmach * defabs[0])) && (abserr[0] > errBnd)) {
                errorStatus = 2;
            } // if ((abserr[0] <= 50.0 * epmach * defabs[0]) &&

            if (limit == 1) {
                errorStatus = 1;
            } // if (limit == 1)

            if ((errorStatus != 0) || ((abserr[0] <= errBnd) && (abserr[0] != resabs[0])) || (abserr[0] == 0.0)) {

                if (keyf != 1) {
                    neval[0] = ((10 * keyf) + 1) * ((2 * neval[0]) + 1);
                } // if (keyf != 1)
                else {
                    neval[0] = (30 * neval[0]) + 15;
                } // else

                return;
            } // if ((errorStatus != 0) ||

            // Initialization

            errMax[0] = abserr[0];
            maxErr[0] = 0;
            area = result[0];
            errSum = abserr[0];
            nrmax[0] = 1;
            iroff1 = 0;
            iroff2 = 0;

            // Main do-loop

            for (last = 2; last <= limit; last++) {

                // Bisect the subinterval with the largest error estimate.

                a1 = alist[maxErr[0]];
                b1 = 0.5 * (alist[maxErr[0]] + blist[maxErr[0]]);
                a2 = b1;
                b2 = blist[maxErr[0]];

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

                neval[0] = neval[0] + 1;
                area12 = area1[0] + area2[0];
                error12 = error1[0] + error2[0];
                errSum = errSum + error12 - errMax[0];
                area = area + area12 - rlist[maxErr[0]];

                if ((defab1[0] != error1[0]) && (defab2[0] != error2[0])) {

                    if ((Math.abs(rlist[maxErr[0]] - area12) <= (1.0E-5 * Math.abs(area12))) &&
                            (error12 >= (0.99 * errMax[0]))) {
                        iroff1 = iroff1 + 1;
                    } // if ((Math.abs(rlist[maxErr[0]] - area12) <=

                    if ((last > 10) && (error12 > errMax[0])) {
                        iroff2 = iroff2 + 1;
                    } // if ((last > 10) && (error12 > errMax[0]))
                } // if ((defab1[0] != error1[0]) && (defab2[0] != error2[0]))

                rlist[maxErr[0]] = area1[0];
                rlist[last - 1] = area2[0];
                errBnd = Math.max(epsabs, epsrel * Math.abs(area));

                if (errSum > errBnd) {

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

                    if (Math.max(Math.abs(a1), Math.abs(b2)) <=
                            ((1.0 + (100.0 * epmach)) * (Math.abs(a2) + (1.0E3 * uflow)))) {
                        errorStatus = 3;
                    } // if (Math.max(Math.abs(a1), Math.abs(b2)) <=
                } // if (errSum > errBnd)

                // Append the newly created intervals to the list

                if (error2[0] <= error1[0]) {
                    alist[last - 1] = a2;
                    blist[maxErr[0]] = b1;
                    blist[last - 1] = b2;
                    elist[maxErr[0]] = error1[0];
                    elist[last - 1] = error2[0];
                } // if (error2[0] <= error1[0])
                else {
                    alist[maxErr[0]] = a2;
                    alist[last - 1] = a1;
                    blist[last - 1] = b1;
                    rlist[maxErr[0]] = area2[0];
                    rlist[last - 1] = area1[0];
                    elist[maxErr[0]] = error2[0];
                    elist[last - 1] = error1[0];
                } // else

                // Call subroutine dqpsrt to maintain the descending ordering
                // in the list of error estimates and select the subinterval
                // with the largest error estimate (to be bisected next).

                dqpsrt(limit, last, maxErr, errMax, elist, iord, nrmax);

                if ((errorStatus != 0) || (errSum <= errBnd)) {
                    break;
                } // if (errorStatus != 0) || (errSum <= errBnd))
            } // for (last = 2; last <= limit; last++)

            // Compute final result.

            result[0] = 0.0;

            for (k = 0; k < last; k++) {
                result[0] = result[0] + rlist[k];
            } // for (k = 0; k < last; k++)

            abserr[0] = errSum;

            if (keyf != 1) {
                neval[0] = ((10 * keyf) + 1) * ((2 * neval[0]) + 1);
            } // if (keyf != 1)
            else {
                neval[0] = (30 * neval[0]) + 15;
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
	 c        double precision version

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
        double[] rlist2;

        // Index to the interval with the largest error estimate
        int[] maxErr = new int[1];

        // errMax[0] = elist[maxErr];
        double[] errMax = new double[1];

        // error on the interval currently subdivided (before that subdivision
        // has taken place)
        double erlast;

        // Sum of the integrals over the subintervals
        double area;

        // Sum of the errors over the subintervals
        double errSum;

        // Requested accuracy max(epsabs, epsrel * abs(result))
        double errBnd;

        // Number of calls to the extrapolation routine
        int[] nres = new int[1];

        // Number of elements in rlist2.  If an appropriate approximation
        // to the compounded integral has been obtained, it is put in
        // rlist2[numr12-1] after numr12 has been increased by one.
        int[] numr12 = new int[1];

        // Sum of the errors over the intervals larger than the smallest
        // interval considered up to now
        double erlarg;

        // boolean variable denoting that the routine is attempting to
        // perform extrapolation.  That is, before dividing the
        // interval we try to decrease the value of erlarg
        boolean extrap;

        // If noext is true, the extrapolation is no longer allowed.
        boolean noext;

        // length of the smallest interval considered up to now, multiplied
        // by 1.5
        double small;
        double[] res31a;
        double[] abseps = new double[1];
        double[] area1 = new double[1];
        double area12;
        double[] area2 = new double[1];
        double a1;
        double a2;
        double b1;
        double b2;
        double correc;
        double[] defabs = new double[1];
        double[] defab1 = new double[1];
        double[] defab2 = new double[1];
        double dres;
        double[] error1 = new double[1];
        double[] error2 = new double[1];
        double error12;
        double ertest;
        double[] resabs = new double[1];
        double[] reseps = new double[1];
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
            alist = new double[limit];
            blist = new double[limit];
            rlist = new double[limit];
            elist = new double[limit];
            iord = new int[limit];
            rlist2 = new double[52];
            res31a = new double[3];

            errorStatus = 0;
            neval[0] = 0;
            last = 0;
            result[0] = 0.0;
            abserr[0] = 0.0;
            alist[0] = lower;
            blist[0] = upper;
            rlist[0] = 0.0;
            elist[0] = 0.0;

            // First approximation to the integral

            ierro = 0;
            dqk21(lower, upper, result, abserr, defabs, resabs);

            // Test on accuracy.

            dres = Math.abs(result[0]);
            errBnd = Math.max(epsabs, epsrel * dres);
            last = 1;
            rlist[0] = result[0];
            elist[0] = abserr[0];
            iord[0] = 0;

            if ((abserr[0] <= (100.0 * epmach * defabs[0])) && (abserr[0] > errBnd)) {
                errorStatus = 2;
            } // if ((abserr[0] <= 100.0 * epmach * defabs[0]) &&

            if (limit == 1) {
                errorStatus = 1;
            } // if (limit == 1)

            if ((errorStatus != 0) || ((abserr[0] <= errBnd) && (abserr[0] != resabs[0])) || (abserr[0] == 0.0)) {
                neval[0] = (42 * last) - 21;

                return;
            } // if ((errorStatus != 0) || ((abserr[0] <= errBnd) && (abserr[0] !=

            // Initialization

            rlist2[0] = result[0];
            errMax[0] = abserr[0];
            maxErr[0] = 0;
            area = result[0];
            errSum = abserr[0];
            erlarg = errSum;
            correc = erlarg;
            small = 0.375 * Math.abs(upper - lower);
            ertest = errBnd;
            abserr[0] = oflow;
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

            if (dres >= ((1.0 - (50.0 * epmach)) * defabs[0])) {
                ksgn = 1;
            } // if (dres >= (1.0 - 50.0 * epmach)*defabs[0])

// Main do-loop
loop:
            for (last = 2; last <= limit; last++) {

                // Bisect the subinterval with the nrmax-th largest error
                // estimate.

                a1 = alist[maxErr[0]];
                b1 = 0.5 * (alist[maxErr[0]] + blist[maxErr[0]]);
                a2 = b1;
                b2 = blist[maxErr[0]];
                erlast = errMax[0];
                dqk21(a1, b1, area1, error1, resabs, defab1);
                dqk21(a2, b2, area2, error2, resabs, defab2);

                // Improve previous approximations to integral and error
                // and test for accuracy

                area12 = area1[0] + area2[0];
                error12 = error1[0] + error2[0];
                errSum = errSum + error12 - errMax[0];
                area = area + area12 - rlist[maxErr[0]];

                if ((defab1[0] != error1[0]) && (defab2[0] != error2[0])) {

                    if ((Math.abs(rlist[maxErr[0]] - area12) <= (1.0E-5 * Math.abs(area12))) &&
                            (error12 >= (0.99 * errMax[0]))) {

                        if (extrap) {
                            iroff2 = iroff2 + 1;
                        } // if (extrap)
                        else {
                            iroff1 = iroff1 + 1;
                        }
                    } // if ((Math.abs(rlist[maxErr[0]] - area12) <=

                    if ((last > 10) && (error12 > errMax[0])) {
                        iroff3 = iroff3 + 1;
                    } // if ((last > 10) && (error12 > errMax[0]))
                } // if ((defab1[0] != error1[0]) && (defab2[0] != error2[0]))

                rlist[maxErr[0]] = area1[0];
                rlist[last - 1] = area2[0];
                errBnd = Math.max(epsabs, epsrel * Math.abs(area));

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

                if (Math.max(Math.abs(a1), Math.abs(b2)) <=
                        ((1.0 + (100.0 * epmach)) * (Math.abs(a2) + (1.0E3 * uflow)))) {
                    errorStatus = 4;
                } // if (Math.max(Math.abs(a1), Math.abs(b2)) <=

                // Append the newly-created intervals to the list.

                if (error2[0] <= error1[0]) {
                    alist[last - 1] = a2;
                    blist[maxErr[0]] = b1;
                    blist[last - 1] = b2;
                    elist[maxErr[0]] = error1[0];
                    elist[last - 1] = error2[0];
                } // if (error2[0] <= error1[0])
                else {
                    alist[maxErr[0]] = a2;
                    alist[last - 1] = a1;
                    blist[last - 1] = b1;
                    rlist[maxErr[0]] = area2[0];
                    rlist[last - 1] = area1[0];
                    elist[maxErr[0]] = error2[0];
                    elist[last - 1] = error1[0];
                } // else

                // Call subroutine dqpsrt to maintain the descending ordering
                // in the list of error estimates and select the subinterval
                // with nrmax-th largest error estimate (to be bisected next).

                dqpsrt(limit, last, maxErr, errMax, elist, iord, nrmax);

                if (errSum <= errBnd) {
                    result[0] = 0.0;

                    for (k = 0; k < last; k++) {
                        result[0] = result[0] + rlist[k];
                    } // for (k = 0; k < last; k++)

                    abserr[0] = errSum;

                    if (errorStatus > 2) {
                        errorStatus = errorStatus - 1;
                    } // if (errorStatus > 2)

                    neval[0] = (42 * last) - 21;

                    return;
                } // if (errSum <= errBnd)

                if (errorStatus != 0) {
                    break;
                } // if (errorStatus != 0)

                if (last == 2) {
                    small = 0.375 * Math.abs(upper - lower);
                    erlarg = errSum;
                    ertest = errBnd;
                    rlist2[1] = area;

                    continue;
                } // if (last == 2)

                if (noext) {
                    continue;
                } // if (noext)

                erlarg = erlarg - erlast;

                if (Math.abs(b1 - a1) > small) {
                    erlarg = erlarg + error12;
                } // if (Math.abs(b1 - a1) > small)

                if (!extrap) {

                    // Test whether the interval to be bisected next is the
                    // smallest interval.

                    if (Math.abs(blist[maxErr[0]] - alist[maxErr[0]]) > small) {
                        continue;
                    } // if (Math.abs(blist[maxErr[0]] - alist[maxErr[0]]) > small)

                    extrap = true;
                    nrmax[0] = 2;
                } // if (!extrap)

                if ((ierro != 3) && (erlarg > ertest)) {

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
                        errMax[0] = elist[maxErr[0]];

                        if (Math.abs(blist[maxErr[0]] - alist[maxErr[0]]) > small) {
                            continue loop;
                        } // if (Math.abs(blist[maxErr[0]] - alist[maxErr[0]]) > small)

                        nrmax[0] = nrmax[0] + 1;
                    } // for (k = id; k <= jupbnd; k++)
                } // if ((ierro != 3) && (erlarg > ertest))

                // Perform extrapolation

                numr12[0] = numr12[0] + 1;
                rlist2[numr12[0] - 1] = area;
                dqelg(numr12, rlist2, reseps, abseps, res31a, nres);
                ktmin = ktmin + 1;

                if ((ktmin > 5) && (abserr[0] < (1.0E-3 * errSum))) {
                    errorStatus = 5;
                } // if ((ktmin > 5) && (abserr[0] < 1.0E-3 * errSum))

                if (abseps[0] < abserr[0]) {
                    ktmin = 0;
                    abserr[0] = abseps[0];
                    result[0] = reseps[0];
                    correc = erlarg;
                    ertest = Math.max(epsabs, epsrel * Math.abs(reseps[0]));

                    if (abserr[0] <= ertest) {
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
                errMax[0] = elist[maxErr[0]];
                nrmax[0] = 1;
                extrap = false;
                small = 0.5 * small;
                erlarg = errSum;
            } // for (last = 2; last <= limit; last++)

            // Set final result and error estimate

            if (abserr[0] != oflow) {

                if ((errorStatus + ierro) != 0) {

                    if (ierro == 3) {
                        abserr[0] = abserr[0] + correc;
                    } // if (ierro == 3)

                    if (errorStatus == 0) {
                        errorStatus = 3;
                    } // if (errorStatus == 0)

                    if ((result[0] == 0.0) || (area == 0.0)) {

                        if (abserr[0] > errSum) {
                            result[0] = 0.0;

                            for (k = 0; k < last; k++) {
                                result[0] = result[0] + rlist[k];
                            } // for (k = 0; k < last; k++)

                            abserr[0] = errSum;

                            if (errorStatus > 2) {
                                errorStatus = errorStatus - 1;
                            } // if (errorStatus > 2)

                            neval[0] = (42 * last) - 21;

                            return;
                        } // if (abserr[0] > errSum)

                        if (area == 0.0) {

                            if (errorStatus > 2) {
                                errorStatus = errorStatus - 1;
                            } // if (errorStatus > 2)

                            neval[0] = (42 * last) - 21;

                            return;
                        } // if (area == 0.0)

                        if ((ksgn == -1) && (Math.max(Math.abs(result[0]), Math.abs(area)) <= (1.0E-2 * defabs[0]))) {

                            if (errorStatus > 2) {
                                errorStatus = errorStatus - 1;
                            } // if (errorStatus > 2)

                            neval[0] = (42 * last) - 21;

                            return;
                        } // if ((ksgn == -1) && (Math.max(Math.abs(result[0]),

                        if ((1.0E-2 > (result[0] / area)) || ((result[0] / area) > 100.0) ||
                                (errSum > Math.abs(area))) {
                            errorStatus = 6;
                        } // if ((1.0E-2 > (result[0]/area)) || (result[0]/area > 100.0) ||

                        if (errorStatus > 2) {
                            errorStatus = errorStatus - 1;
                        } // if (errorStatus > 2)

                        neval[0] = (42 * last) - 21;

                        return;
                    } // if ((result[0] == 0.00 || (area == 0.0))

                    if ((abserr[0] / Math.abs(result[0])) > (errSum / Math.abs(area))) {
                        result[0] = 0.0;

                        for (k = 0; k < last; k++) {
                            result[0] = result[0] + rlist[k];
                        } // for (k = 0; k < last; k++)

                        abserr[0] = errSum;

                        if (errorStatus > 2) {
                            errorStatus = errorStatus - 1;
                        } // if (errorStatus > 2)

                        neval[0] = (42 * last) - 21;

                        return;
                    } // if (abserr[0]/Math.abs(result[0]) > errSum/Math.abs(area))
                } // if ((errorStatus + ierro) != 0)

                if ((ksgn == -1) && (Math.max(Math.abs(result[0]), Math.abs(area)) <= (1.0E-2 * defabs[0]))) {

                    if (errorStatus > 2) {
                        errorStatus = errorStatus - 1;
                    } // if (errorStatus > 2)

                    neval[0] = (42 * last) - 21;

                    return;
                } // if ((ksgn == -1) && (Math.max(Math.abs(result[0]),

                if ((1.0E-2 > (result[0] / area)) || ((result[0] / area) > 100.0) || (errSum > Math.abs(area))) {
                    errorStatus = 6;
                } // if ((1.0E-2 > (result[0]/area)) || (result[0]/area > 100.0) ||

                if (errorStatus > 2) {
                    errorStatus = errorStatus - 1;
                } // if (errorStatus > 2)

                neval[0] = (42 * last) - 21;

                return;
            } // if (abserr[0] != oflow)

            result[0] = 0.0;

            for (k = 0; k < last; k++) {
                result[0] = result[0] + rlist[k];
            } // for (k = 0; k < last; k++)

            abserr[0] = errSum;

            if (errorStatus > 2) {
                errorStatus = errorStatus - 1;
            } // if (errorStatus > 2)

            neval[0] = (42 * last) - 21;

            return;
        } // try
        catch (Exception err) {
            Preferences.debug("dqagse error: " + err.getMessage());
        }

    }
    
    /**
     * This is the port of the original FORTRAN routine whose header is given below:
     * c***begin prologue  dqawce
	 c***date written   800101   (yymmdd)
	 c***revision date  830518   (yymmdd)
	 c***category no.  h2a2a1,j4
	 c***keywords  automatic integrator, special-purpose,
	 c             cauchy principal value, clenshaw-curtis method
	 c***author  piessens,robert,appl. math. & progr. div. - k.u.leuven
	 c           de doncker,elise,appl. math. & progr. div. - k.u.leuven
	 c***  purpose  the routine calculates an approximation result to a
	 c              cauchy principal value i = integral of f*w over (a,b)
	 c              (w(x) = 1/(x-c), (c.ne.a, c.ne.b), hopefully satisfying
	 c              following claim for accuracy
	 c              abs(i-result).le.max(epsabs,epsrel*abs(i))
	 c***description
	 c
	 c        computation of a cauchy principal value
	 c        standard fortran subroutine
	 c        double precision version
     */
    private void dqawce() {
        // Routines called dqc25c, dqpsrt
    	// Variables end in 1 for the the left subinterval
    	// and variables end in 2 for the right subinterval
    	
    	// Index to the interval with the largest error estimate
        int[] maxErr = new int[1];

        // errMax[0] = elist[maxErr];
        double[] errMax = new double[1];

        // Sum of the integrals over the subintervals
        double area;

        // Sum of the errors over the subintervals
        double errSum;

        // Requested accuracy max(epsabs, epsrel * abs(result))
        double errBnd;
        
        double aa;
        double area1[] = new double[1];
        double area12;
        double area2[] = new double[1];
        double a1;
        double a2;
        double bb;
        double b1;
        double b2;
        double error1[] = new double[1];
        double erro12;
        double error2[] = new double[1];
        int iroff1;
        int iroff2;
        int k;
        int krule[] = new int[1];
        int nev[] = new int[1];
        int nrmax[] = new int[1];
        
	    try {
	    	alist = new double[limit];
            blist = new double[limit];
            rlist = new double[limit];
            elist = new double[limit];
            iord = new int[limit];
            
            errorStatus = 0;
            neval[0] = 0;
            last = 0;
            alist[0] = lower;
            blist[0] = upper;
            rlist[0] = 0.0;
            elist[0] = 0.0;
            iord[0] = -1;
            result[0] = 0.0;
            abserr[0] = 0.0;
            
            // First approximation to the integral
            aa = lower;
            bb = upper;
            if (lower > upper) {
            	aa = upper;
            	bb = lower;
            }
            errorStatus = 0;
            krule[0] = 1;
            dqc25c(aa, bb, c, result, abserr, krule, neval);
            last = 1;
            rlist[0] = result[0];
            elist[0] = abserr[0];
            iord[0] = 0;
            alist[0] = lower;
            blist[0] = upper;
            
            // Test on accuracy
            
            errBnd = Math.max(epsabs, epsrel * Math.abs(result[0]));
            if (limit == 1) {
            	errorStatus = 1;
            }
            if ((abserr[0] < Math.min(0.01 * Math.abs(result[0]), errBnd)) ||
            	(errorStatus == 1)) {
            	if (aa == upper) {
            		result[0] = -result[0];
            	}
            	return;
            }
            
            // Initialization
            
            alist[0] = aa;
            blist[0] = bb;
            rlist[0] = result[0];
            errMax[0] = abserr[0];
            maxErr[0] = 0;
            area = result[0];
            errSum = abserr[0];
            nrmax[0] = 1;
            iroff1 = 0;
            iroff2 = 0;
            
            // Main for loop
            
            for (last = 1; last < limit; last++) {
            	
            	// Biset the subinterval with the nrmax-th largest error estimate.
            	
            	a1 = alist[maxErr[0]];
            	b1 = 0.5*(alist[maxErr[0]] + blist[maxErr[0]]);
            	b2 = blist[maxErr[0]];
            	if ((c <= b1) && (c > a1)) {
            		b1 = 0.5*(c + b2);
            	}
            	if ((c > b1) && (c < b2)) {
            		b1 = 0.5*(a1 + c);
            	}
            	a2 = b1;
            	krule[0] = 2;
            	dqc25c(a1, b1, c, area1, error1, krule, nev);
            	neval[0] = neval[0] + nev[0];
            	dqc25c(a2, b2, c, area2, error2, krule, nev);
            	neval[0] = neval[0] + nev[0];
            	
            	// Improve previous approximations to integral
            	// and error and test for accuracy.
            	
            	area12 = area1[0] + area2[0];
            	erro12 = error1[0] + error2[0];
            	errSum = errSum + erro12 - errMax[0];
            	area = area + area12 - rlist[maxErr[0]];
            	if ((Math.abs(rlist[maxErr[0]] - area12) < 1.0E-5*Math.abs(area12)) &&
            		(erro12 >= 0.99*errMax[0]) && (krule[0] == 0)) {
            		iroff1 = iroff1 + 1;
            	}
            	if ((last >= 10) && (erro12 > errMax[0]) && (krule[0] == 0)) {
            		iroff2 = iroff2 + 1;
            	}
            	rlist[maxErr[0]] = area1[0];
            	rlist[last] = area2[0];
            	errBnd = Math.max(epsabs, epsrel * Math.abs(area));
            	if (errSum > errBnd) {
            		
            		// Test for roundoff error and eventually set error flag.
            		
            		if ((iroff1 >= 6) && (iroff2 > 20)) {
            			errorStatus = 2;
            		}
            		
            		// Set error flag in the case that number of interval
            		// bisections exceeds limit.
            		
            		if (last == (limit-1)) {
            			errorStatus = 1;
            		}
            		
            		// Set error flag in the case of bad integrand behavior
            		// at a point of the integration range
            		if (Math.max(Math.abs(a1), Math.abs(b2)) <= ((1.0 + 100.0*epmach)*(Math.abs(a2) + 1.0E3 * uflow))) {
            			errorStatus = 3;
            		}
            	} // if (errSum > errBnd)
            	
            	// Append the newly created intervals to the list
            	
            	if (error2[0] <= error1[0]) {
            		alist[last] = a2;
            		blist[maxErr[0]] = b1;
            		blist[last] = b2;
            		elist[maxErr[0]] = error1[0];
            		elist[last] = error2[0];
            	}
            	else {
            	    alist[maxErr[0]] = a2;
            	    alist[last] = a1;
            	    blist[last] = b1;
            	    rlist[maxErr[0]] = area2[0];
            	    rlist[last] = area1[0];
            	    elist[maxErr[0]] = error2[0];
            	    elist[last] = error1[0];
            	}
            	
            	// Call subroutine dqpsrt to maintain the descending ordering
            	// in the list of error estimates and select the subinterval
            	// with the nrmax-th largest error estimate (to be bisected next).
            	
            	dqpsrt(limit, last, maxErr, errMax, elist, iord, nrmax);
            	if ((errorStatus != 0) || (errSum <= errBnd)) {
            		break;
            	}
            } // for (last = 1; last < limit; last++)
            
            // Compute final result
            
            result[0] = 0.0;
            for (k = 0; k < last; k++) {
            	result[0] = result[0] + rlist[k];
            }
            abserr[0] = errSum;
            if (aa == upper) {
            	result[0] = -result[0];
            }
            return;
	    } // try
	    catch (Exception err) {
	        Preferences.debug("dqawce error: " + err.getMessage());
	    }
    } // dqawce
    
    /**
     * This is the port of the original FORTRAN routine whose header is given below:
     * c***begin prologue  dqc25c
	 c***date written   810101   (yymmdd)
	 c***revision date  830518   (yymmdd)
	 c***category no.  h2a2a2,j4
	 c***keywords  25-point clenshaw-curtis integration
	 c***author  piessens,robert,appl. math. & progr. div. - k.u.leuven
	 c           de doncker,elise,appl. math. & progr. div. - k.u.leuven
	 c***purpose  to compute i = integral of f*w over (a,b) with
	 c            error estimate, where w(x) = 1/(x-c)
	 c***description
	 c
	 c        integration rules for the computation of cauchy
	 c        principal value integrals
	 c        standard fortran subroutine
	 c        double precision version
	 c
	 c
	 c           a      - double precision
	 c                    left end point of the integration interval
	 c
	 c           b      - double precision
	 c                    right end point of the integration interval, b.gt.a
	 c
	 c           c      - double precision
	 c                    parameter in the weight function
	 c
	 c           result - double precision
	 c                    approximation to the integral
	 c                    result is computed by using a generalized
	 c                    clenshaw-curtis method if c lies within ten percent
	 c                    of the integration interval. in the other case the
	 c                    15-point kronrod rule obtained by optimal addition
	 c                    of abscissae to the 7-point gauss rule, is applied.
	 c
	 c           abserr - double precision
	 c                    estimate of the modulus of the absolute error,
	 c                    which should equal or exceed abs(i-result)
	 c
	 c           krul   - integer
	 c                    key which is decreased by 1 if the 15-point
	 c                    gauss-kronrod scheme has been used
	 c
	 c           neval  - integer
	 c                    number of integrand evaluations
	 c
	 c.......................................................................
	 c***references  (none)
	 c***routines called  dqcheb,dqk15w,dqwgtc
	 c***end prologue  dqc25c
	 c
     * 
     * @param a
     * @param b
     * @param c
     * @param result
     * @param abserr
     * @param krul
     * @param neval
     */
    private void dqc25c(double a, double b, double c, double result[], double abserr[],
    		            int krul[], int neval[]) {
    	
    } // dqc25c

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
	 c           double precision version

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
    private void dqelg(int[] n, double[] epstab, double[] result, double[] abserr, double[] res31a, int[] nres) {
        double delta1;
        double delta2;
        double delta3;
        double epsinf;
        double esptab;

        // error = abs(e1-e0) + abs(e2-e1) + abs(new-e2)
        double error;
        double err1;
        double err2;
        double err3;

        // The 4 elements on which the computation of a new element in the
        // epsilon table is based
        double e0;
        double e1;
        double e2;
        double e3;
        double e1abs;
        double res;
        double ss = 0.0;
        double tol1;
        double tol2;
        double tol3;
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
        abserr[0] = oflow;
        result[0] = epstab[n[0] - 1];

        if (n[0] < 3) {
            abserr[0] = Math.max(abserr[0], 5.0 * epmach * Math.abs(result[0]));

            return;
        } // if (n[0] < 3)

        limexp = 50;
        epstab[n[0] + 1] = epstab[n[0] - 1];
        newelm = (n[0] - 1) / 2;
        epstab[n[0] - 1] = oflow;
        num = n[0];
        k1 = n[0];

        for (i = 1; i <= newelm; i++) {
            k2 = k1 - 1;
            k3 = k1 - 2;
            res = epstab[k1 + 1];
            e0 = epstab[k3 - 1];
            e1 = epstab[k2 - 1];
            e2 = res;
            e1abs = Math.abs(e1);
            delta2 = e2 - e1;
            err2 = Math.abs(delta2);
            tol2 = Math.max(Math.abs(e2), e1abs) * epmach;
            delta3 = e1 - e0;
            err3 = Math.abs(delta3);
            tol3 = Math.max(e1abs, Math.abs(e0)) * epmach;

            if ((err2 <= tol2) && (err3 <= tol3)) {

                // if e0, e1, and e2 are equal to within machine accuracy,
                // convergence is assumed.
                result[0] = res;
                abserr[0] = err2 + err3;
                abserr[0] = Math.max(abserr[0], 5.0 * epmach * Math.abs(result[0]));

                return;
            } // if ((err2 <= tol2) && (err3 <= tol3))

            e3 = epstab[k1 - 1];
            epstab[k1 - 1] = e1;
            delta1 = e1 - e3;
            err1 = Math.abs(delta1);
            tol1 = Math.max(e1abs, Math.abs(e3)) * epmach;

            // If two elements are very close to each other, omit a part of
            // the table by adjusting the value of n

            if ((err1 > tol1) && (err2 > tol2) && (err3 > tol3)) {
                ss = (1.0 / delta1) + (1.0 / delta2) - (1.0 / delta3);
                epsinf = Math.abs(ss * e1);

                // Test to detect irregular behavior in the table, and
                // eventually omit a part of the table adjusting the value of n.

                if (epsinf > 1.0E-4) {
                    doSeg = false;
                }
            } // ((err1 > tol1) && (err2 > tol2) && (err3 > tol3))

            if (doSeg) {
                n[0] = i + i - 1;

                break;
            } // if (doSeg)

            // Compute a new element and eventually adjust the value of result

            res = e1 + (1.0 / ss);
            epstab[k1 - 1] = res;
            k1 = k1 - 2;
            error = err2 + Math.abs(res - e2) + err3;

            if (error > abserr[0]) {
                continue;
            } // if (error > abserr[0])

            abserr[0] = error;
            result[0] = res;
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
            epstab[ib - 1] = epstab[ib2 - 1];
            ib = ib2;
        } // for (i = 1; i <= ie; i++)

        if (num != n[0]) {
            indx = num - n[0];

            for (i = 0; i < n[0]; i++) {
                epstab[i] = epstab[indx];
                indx = indx + 1;
            } // for (i = 0; i < n[0]; i++)
        } // if (num != n[0])

        if (nres[0] < 4) {
            res31a[nres[0] - 1] = result[0];
            abserr[0] = oflow;
            abserr[0] = Math.max(abserr[0], 5.0 * epmach * Math.abs(result[0]));

            return;
        } // if (nres[0] < 4)

        // Compute error estimate

        abserr[0] = Math.abs(result[0] - res31a[2]) + Math.abs(result[0] - res31a[1]) + Math.abs(result[0] - res31a[0]);
        res31a[0] = res31a[1];
        res31a[1] = res31a[2];
        res31a[2] = result[0];
        abserr[0] = Math.max(abserr[0], 5.0 * epmach * Math.abs(result[0]));

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
	 c           double precision version

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
    private void dqk15(double a, double b, double[] result, double[] abserr, double[] resabs, double[] resasc) {
        // The abscissae and weights are supplied for the interval (-1, 1).
        // Because of symmetry only the positive abscissae and their
        // corresponding weights are given.

        // xgk - abscissae of the 15-point kronrod rule
        // xgk[1], xgk[3], ... abscissae of the 7-point gauss rule
        // xgk[0], xgk[2], ... abscissae which are optimally added to the
        // 7-point gauss rule.
        double[] xgk = new double[] {
                           0.991455371120812639206854697526329, 0.949107912342758524526189684047851,
                           0.864864423359769072789712788640926, 0.741531185599394439863864773280788,
                           0.586087235467691130294144838258730, 0.405845151377397166906606412076961,
                           0.207784955007898467600689403773245, 0.000000000000000000000000000000000
                       };

        // wgk - weights of the 15-point kronrod rule
        double[] wgk = new double[] {
                           0.022935322010529224963732008058970, 0.063092092629978553290700663189204,
                           0.104790010322250183839876322541518, 0.140653259715525918745189590510238,
                           0.169004726639267902826583426598550, 0.190350578064785409913256402421014,
                           0.204432940075298892414161999234649, 0.209482141084727828012999174891714
                       };

        // wg - weights of the 7-point gauss rule, corresponding to the
        // abscissae xgk[1], xgk[3], ...
        // wg[0], wg[2], ... are set to zero.
        double[] wg = new double[] {
                          0.0, 0.129484966168869693270611432679082, 0.0, 0.279705391489276667901467771423780, 0.0,
                          0.381830050505118944950369775488975, 0.0, 0.417959183673469387755102040816327
                      };

        // abscissa
        double absc;

        // mid point of the interval
        double centr;
        double dhlgth;
        double fc;
        double fsum;

        // function value
        double fval1;
        double fval2;
        double[] fv1 = new double[7];
        double[] fv2 = new double[7];

        // half-length of the interval
        double hlgth;

        // result of the 7-point gauss formula
        double resg;

        // result of the 15-point kronrod formula
        double resk;

        // approximation to the mean value of intFunc over
        // (a, b), i.e. to (integral of intFunc)/(b - a) over (a,b)
        double reskh;
        int j;
        int jtw;
        int jtwm1;

        centr = 0.5 * (a + b);
        hlgth = 0.5 * (b - a);
        dhlgth = Math.abs(hlgth);

        // Compute the 15-point kronrod approximation to the integral, and
        // estimate the absolute error.

        if (selfTest) {
        	fc = intFuncTest(centr);	
        }
        else {
            fc = intFunc(centr);
        }
        resg = fc * wg[3];
        resk = fc * wgk[7];
        resabs[0] = Math.abs(resk);

        for (j = 0; j < 3; j++) {
            jtw = (2 * j) + 1;
            absc = hlgth * xgk[jtw];
            if (selfTest) {
            	fval1 = intFuncTest(centr - absc);
                fval2 = intFuncTest(centr + absc);	
            }
            else {
                fval1 = intFunc(centr - absc);
                fval2 = intFunc(centr + absc);
            }
            fv1[jtw] = fval1;
            fv2[jtw] = fval2;
            fsum = fval1 + fval2;
            resg = resg + (wg[j] * fsum);
            resk = resk + (wgk[jtw] * fsum);
            resabs[0] = resabs[0] + (wgk[jtw] * (Math.abs(fval1) + Math.abs(fval2)));
        } // for (j = 0; j < 3; j++)

        for (j = 0; j < 4; j++) {
            jtwm1 = 2 * j;
            absc = hlgth * xgk[jtwm1];
            if (selfTest) {
            	fval1 = intFuncTest(centr - absc);
                fval2 = intFuncTest(centr + absc);	
            }
            else {
                fval1 = intFunc(centr - absc);
                fval2 = intFunc(centr + absc);
            }
            fv1[jtwm1] = fval1;
            fv2[jtwm1] = fval2;
            fsum = fval1 + fval2;
            resk = resk + (wgk[jtwm1] * fsum);
            resabs[0] = resabs[0] + (wgk[jtwm1] * (Math.abs(fval1) + Math.abs(fval2)));
        } // for (j = 0; j < 4; j++)

        reskh = 0.5 * resk;
        resasc[0] = wgk[7] * Math.abs(fc - reskh);

        for (j = 0; j < 7; j++) {
            resasc[0] = resasc[0] + (wgk[j] * (Math.abs(fv1[j] - reskh) + Math.abs(fv2[j] - reskh)));
        } // for (j = 0; j < 7; j++)

        result[0] = resk * hlgth;
        resabs[0] = resabs[0] * dhlgth;
        resasc[0] = resasc[0] * dhlgth;
        abserr[0] = Math.abs((resk - resg) * hlgth);

        if ((resasc[0] != 0.0) && (abserr[0] != 0.0)) {
            abserr[0] = resasc[0] * Math.min(1.0, Math.pow(200.0 * abserr[0] / resasc[0], 1.5));
        }

        if (resabs[0] > (uflow / (50.0 * epmach))) {
            abserr[0] = Math.max((50.0 * epmach) * resabs[0], abserr[0]);
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
	 c           double precision version

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
    private void dqk15i(double boun, int inf, double a, double b, double[] result, double[] abserr, double[] resabs,
                        double[] resasc) {
        // The abscissae and weights are supplied for the interval (-1, 1).
        // Because of symmetry only the positive abscissae and their
        // corresponding weights are given.

        // xgk - abscissae of the 15-point kronrod rule
        // xgk[1], xgk[3], ... abscissae of the 7-point gauss rule
        // xgk[0], xgk[2], ... abscissae which are optimally added to the
        // 7-point gauss rule.
        double[] xgk = new double[] {
                           0.991455371120812639206854697526329, 0.949107912342758524526189684047851,
                           0.864864423359769072789712788640926, 0.741531185599394439863864773280788,
                           0.586087235467691130294144838258730, 0.405845151377397166906606412076961,
                           0.207784955007898467600689403773245, 0.000000000000000000000000000000000
                       };

        // wgk - weights of the 15-point kronrod rule
        double[] wgk = new double[] {
                           0.022935322010529224963732008058970, 0.063092092629978553290700663189204,
                           0.104790010322250183839876322541518, 0.140653259715525918745189590510238,
                           0.169004726639267902826583426598550, 0.190350578064785409913256402421014,
                           0.204432940075298892414161999234649, 0.209482141084727828012999174891714
                       };

        // wg - weights of the 7-point gauss rule, corresponding to the
        // abscissae xgk[1], xgk[3], ...
        // wg[0], wg[2], ... are set to zero.
        double[] wg = new double[] {
                          0.0, 0.129484966168869693270611432679082, 0.0, 0.279705391489276667901467771423780, 0.0,
                          0.381830050505118944950369775488975, 0.0, 0.417959183673469387755102040816327
                      };

        // abscissa
        double absc;
        double absc1;
        double absc2;

        // mid point of the interval
        double centr;
        double dinf;
        double fc;
        double fsum;

        // function value
        double fval1;
        double fval2;
        double[] fv1 = new double[7];
        double[] fv2 = new double[7];

        // half-length of the interval
        double hlgth;

        // result of the 7-point gauss formula
        double resg;

        // result of the 15-point kronrod formula
        double resk;

        // approximation to the mean value of the transformed integrand over
        // (a, b), i.e. to (integral of transformed integrand)/(b - a)
        double reskh;

        // transformed abscissa
        double tabsc1;
        double tabsc2;
        int j;

        dinf = Math.min(1.0, inf);

        centr = 0.5 * (a + b);
        hlgth = 0.5 * (b - a);
        tabsc1 = boun + (dinf * (1.0 - centr) / centr);
        if (selfTest) {
        	fval1 = intFuncTest(tabsc1);	
        }
        else {
            fval1 = intFunc(tabsc1);
        }

        if (inf == 2) {
        	if (selfTest) {
        		fval1 = fval1 + intFuncTest(-tabsc1);
        	}
        	else {
                fval1 = fval1 + intFunc(-tabsc1);
        	}
        } // if (inf == 2)

        fc = (fval1 / centr) / centr;

        // Compute the 15-point kronrod approximation to the integral, and
        // estimate the error.

        resg = wg[7] * fc;
        resk = wgk[7] * fc;
        resabs[0] = Math.abs(resk);

        for (j = 0; j < 7; j++) {
            absc = hlgth * xgk[j];
            absc1 = centr - absc;
            absc2 = centr + absc;
            tabsc1 = boun + (dinf * (1.0 - absc1) / absc1);
            tabsc2 = boun + (dinf * (1.0 - absc2) / absc2);
            if (selfTest) {
	            fval1 = intFuncTest(tabsc1);
	            fval2 = intFuncTest(tabsc2);
	
	            if (inf == 2) {
	                fval1 = fval1 + intFuncTest(-tabsc1);
	                fval2 = fval2 + intFuncTest(-tabsc2);
	            } // if (inf == 2)
            } // if (selfTest)
            else {
            	fval1 = intFunc(tabsc1);
	            fval2 = intFunc(tabsc2);
	
	            if (inf == 2) {
	                fval1 = fval1 + intFunc(-tabsc1);
	                fval2 = fval2 + intFunc(-tabsc2);
	            } // if (inf == 2)	
            }

            fval1 = (fval1 / absc1) / absc1;
            fval2 = (fval2 / absc2) / absc2;
            fv1[j] = fval1;
            fv2[j] = fval2;
            fsum = fval1 + fval2;
            resg = resg + (wg[j] * fsum);
            resk = resk + (wgk[j] * fsum);
            resabs[0] = resabs[0] + (wgk[j] * (Math.abs(fval1) + Math.abs(fval2)));
        } // for (j = 0; j < 7; j++)

        reskh = 0.5 * resk;
        resasc[0] = wgk[7] * Math.abs(fc - reskh);

        for (j = 0; j < 7; j++) {
            resasc[0] = resasc[0] + (wgk[j] * (Math.abs(fv1[j] - reskh) + Math.abs(fv2[j] - reskh)));
        } // for (j = 0; j < 7; j++)

        result[0] = resk * hlgth;
        resasc[0] = resasc[0] * hlgth;
        resabs[0] = resabs[0] * hlgth;
        abserr[0] = Math.abs((resk - resg) * hlgth);

        if ((resasc[0] != 0.0) && (abserr[0] != 0.0)) {
            abserr[0] = resasc[0] * Math.min(1.0, Math.pow(200.0 * abserr[0] / resasc[0], 1.5));
        } // if ((resasc[0] != 0.0) && (abserr[0] != 0.0))

        if (resabs[0] > (uflow / (50.0 * epmach))) {
            abserr[0] = Math.max((50.0 * epmach) * resabs[0], abserr[0]);
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
	 c           double precision version

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
    private void dqk21(double a, double b, double[] result, double[] abserr, double[] resabs, double[] resasc) {

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
        double[] xgk = new double[] {
                           0.995657163025808080735527280689003, 0.973906528517171720077964012084452,
                           0.930157491355708226001207180059508, 0.865063366688984510732096688423493,
                           0.780817726586416897063717578345042, 0.679409568299024406234327365114874,
                           0.562757134668604683339000099272694, 0.433395394129247190799265943165784,
                           0.294392862701460198131126603103866, 0.148874338981631210884826001129720,
                           0.000000000000000000000000000000000
                       };

        // wgk - weights of the 21-point kronrod rule
        double[] wgk = new double[] {
                           0.011694638867371874278064396062192, 0.032558162307964727478818972459390,
                           0.054755896574351996031381300244580, 0.075039674810919952767043140916190,
                           0.093125454583697605535065465083366, 0.109387158802297641899210590325805,
                           0.123491976262065851077958109831074, 0.134709217311473325928054001771707,
                           0.142775938577060080797094273138717, 0.147739104901338491374841515972068,
                           0.149445554002916905664936468389821
                       };

        // wg - weights of the 10-point gauss rule
        double[] wg = new double[] {
                          0.066671344308688137593568809893332, 0.149451349150580593145776339657697,
                          0.219086362515982043995534934228163, 0.269266719309996355091226921569469,
                          0.295524224714752870173892994651338
                      };

        // mid-point of the interval
        double centr;

        // half-length of the interval
        double hlgth;

        // abscissa
        double absc;

        // function values
        double fval1;
        double fval2;

        // result of the 10-point gauss formula
        double resg;

        // result of the 21-point kronrod formula
        double resk;

        // approximation to the mean value of function over (a,b),
        // i.e. to actual integral/ (b-a)
        double reskh;
        double dhlgth;
        double fc;
        double fsum;
        double[] fv1 = new double[10];
        double[] fv2 = new double[10];
        int j;
        int jtw;
        int jtwm1;


        centr = 0.5 * (a + b);
        hlgth = 0.5 * (b - a);
        dhlgth = Math.abs(hlgth);

        // Compute the 21-point kronrod approximation to the integral, and
        // estimate the absolute error

        resg = 0.0;
        if (selfTest) {
        	fc = intFuncTest(centr);	
        }
        else {
            fc = intFunc(centr);
        }
        resk = wgk[10] * fc;
        resabs[0] = Math.abs(resk);

        for (j = 0; j <= 4; j++) {
            jtw = (2 * j) + 1;
            absc = hlgth * xgk[jtw];
            if (selfTest) {
            	fval1 = intFuncTest(centr - absc);
                fval2 = intFuncTest(centr + absc);	
            }
            else {
                fval1 = intFunc(centr - absc);
                fval2 = intFunc(centr + absc);
            }
            fv1[jtw] = fval1;
            fv2[jtw] = fval2;
            fsum = fval1 + fval2;
            resg = resg + (wg[j] * fsum);
            resk = resk + (wgk[jtw] * fsum);
            resabs[0] = resabs[0] + (wgk[jtw] * (Math.abs(fval1) + Math.abs(fval2)));
        } // for (j = 0; j <= 4; j ++)

        for (j = 0; j <= 4; j++) {
            jtwm1 = 2 * j;
            absc = hlgth * xgk[jtwm1];
            if (selfTest) {
            	fval1 = intFuncTest(centr - absc);
                fval2 = intFuncTest(centr + absc);	
            }
            else {
                fval1 = intFunc(centr - absc);
                fval2 = intFunc(centr + absc);
            }
            fv1[jtwm1] = fval1;
            fv2[jtwm1] = fval2;
            fsum = fval1 + fval2;
            resk = resk + (wgk[jtwm1] * fsum);
            resabs[0] = resabs[0] + (wgk[jtwm1] * (Math.abs(fval1) + Math.abs(fval2)));
        } // for (j = 0; j <= 4; j ++)

        reskh = 0.5 * resk;
        resasc[0] = wgk[10] * Math.abs(fc - reskh);

        for (j = 0; j <= 9; j++) {
            resasc[0] = resasc[0] + (wgk[j] * (Math.abs(fv1[j] - reskh) + Math.abs(fv2[j] - reskh)));
        } // for (j = 0; j <= 9; j++)

        result[0] = resk * hlgth;
        resabs[0] = resabs[0] * dhlgth;
        resasc[0] = resasc[0] * dhlgth;
        abserr[0] = Math.abs((resk - resg) * hlgth);

        if ((resasc[0] != 0.0) && (abserr[0] != 0.0)) {
            abserr[0] = resasc[0] * Math.min(1.0, Math.pow(200.0 * abserr[0] / resasc[0], 1.5));
        }

        if (resabs[0] > (uflow / (50.0 * epmach))) {
            abserr[0] = Math.max((50.0 * epmach) * resabs[0], abserr[0]);
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
     c           double precision version

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
    private void dqk31(double a, double b, double[] result, double[] abserr, double[] resabs, double[] resasc) {
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
        double[] xgk = new double[] {
                           0.998002298693397060285172840152271, 0.987992518020485428489565718586613,
                           0.967739075679139134257347978784337, 0.937273392400705904307758947710209,
                           0.897264532344081900882509656454496, 0.848206583410427216200648320774217,
                           0.790418501442465932967649294817947, 0.724417731360170047416186054613938,
                           0.650996741297416970533735895313275, 0.570972172608538847537226737253911,
                           0.485081863640239680693655740232351, 0.394151347077563369897207370981045,
                           0.299180007153168812166780024266389, 0.201194093997434522300628303394596,
                           0.101142066918717499027074231447392, 0.000000000000000000000000000000000
                       };

        // wgk - weights of the 31-point kronrod rule
        double[] wgk = new double[] {
                           0.005377479872923348987792051430128, 0.015007947329316122538374763075807,
                           0.025460847326715320186874001019653, 0.035346360791375846222037948478360,
                           0.044589751324764876608227299373280, 0.053481524690928087265343147239430,
                           0.062009567800670640285139230960803, 0.069854121318728258709520077099147,
                           0.076849680757720378894432777482659, 0.083080502823133021038289247286104,
                           0.088564443056211770647275443693774, 0.093126598170825321225486872747346,
                           0.096642726983623678505179907627589, 0.099173598721791959332393173484603,
                           0.100769845523875595044946662617570, 0.101330007014791549017374792767493
                       };

        // wg - weights of the 15-point gauss rule
        double[] wg = new double[] {
                          0.030753241996117268354628393577204, 0.070366047488108124709267416450667,
                          0.107159220467171935011869546685869, 0.139570677926154314447804794511028,
                          0.166269205816993933553200860481209, 0.186161000015562211026800561866423,
                          0.198431485327111576456118326443839, 0.202578241925561272880620199967519
                      };

        // abscissa
        double absc;

        // mid point of the interval
        double centr;
        double dhlgth;
        double fc;
        double fsum;

        // function value
        double fval1;
        double fval2;
        double[] fv1 = new double[15];
        double[] fv2 = new double[15];

        // half-length of the interval
        double hlgth;

        // result of the 15-point gauss formula
        double resg;

        // result of the 31-point kronrod formula
        double resk;

        // approximation to the mean value of intFunc over
        // (a, b), i.e. to (integral of intFunc)/(b - a) over (a,b)
        double reskh;
        int j;
        int jtw;
        int jtwm1;

        centr = 0.5 * (a + b);
        hlgth = 0.5 * (b - a);
        dhlgth = Math.abs(hlgth);

        // Compute the 31-point kronrod approximation to the integral, and
        // estimate the absolute error.

        if (selfTest) {
        	fc = intFuncTest(centr);	
        }
        else {
            fc = intFunc(centr);
        }
        resg = fc * wg[7];
        resk = fc * wgk[15];
        resabs[0] = Math.abs(resk);

        for (j = 0; j < 7; j++) {
            jtw = (2 * j) + 1;
            absc = hlgth * xgk[jtw];
            if (selfTest) {
            	fval1 = intFuncTest(centr - absc);
                fval2 = intFuncTest(centr + absc);	
            }
            else {
                fval1 = intFunc(centr - absc);
                fval2 = intFunc(centr + absc);
            }
            fv1[jtw] = fval1;
            fv2[jtw] = fval2;
            fsum = fval1 + fval2;
            resg = resg + (wg[j] * fsum);
            resk = resk + (wgk[jtw] * fsum);
            resabs[0] = resabs[0] + (wgk[jtw] * (Math.abs(fval1) + Math.abs(fval2)));
        } // for (j = 0; j < 7; j++)

        for (j = 0; j < 8; j++) {
            jtwm1 = 2 * j;
            absc = hlgth * xgk[jtwm1];
            if (selfTest) {
            	fval1 = intFuncTest(centr - absc);
                fval2 = intFuncTest(centr + absc);	
            }
            else {
                fval1 = intFunc(centr - absc);
                fval2 = intFunc(centr + absc);
            }
            fv1[jtwm1] = fval1;
            fv2[jtwm1] = fval2;
            fsum = fval1 + fval2;
            resk = resk + (wgk[jtwm1] * fsum);
            resabs[0] = resabs[0] + (wgk[jtwm1] * (Math.abs(fval1) + Math.abs(fval2)));
        } // for (j = 0; j < 8; j++)

        reskh = 0.5 * resk;
        resasc[0] = wgk[15] * Math.abs(fc - reskh);

        for (j = 0; j < 15; j++) {
            resasc[0] = resasc[0] + (wgk[j] * (Math.abs(fv1[j] - reskh) + Math.abs(fv2[j] - reskh)));
        } // for (j = 0; j < 15; j++)

        result[0] = resk * hlgth;
        resabs[0] = resabs[0] * dhlgth;
        resasc[0] = resasc[0] * dhlgth;
        abserr[0] = Math.abs((resk - resg) * hlgth);

        if ((resasc[0] != 0.0) && (abserr[0] != 0.0)) {
            abserr[0] = resasc[0] * Math.min(1.0, Math.pow(200.0 * abserr[0] / resasc[0], 1.5));
        }

        if (resabs[0] > (uflow / (50.0 * epmach))) {
            abserr[0] = Math.max((50.0 * epmach) * resabs[0], abserr[0]);
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
	 c           double precision version

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
    private void dqk41(double a, double b, double[] result, double[] abserr, double[] resabs, double[] resasc) {
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
        double[] xgk = new double[] {
                           0.998859031588277663838315576545863, 0.993128599185094924786122388471320,
                           0.981507877450250259193342994720217, 0.963971927277913791267666131197277,
                           0.940822633831754753519982722212443, 0.912234428251325905867752441203298,
                           0.878276811252281976077442995113078, 0.839116971822218823394529061701521,
                           0.795041428837551198350638833272788, 0.746331906460150792614305070355642,
                           0.693237656334751384805490711845932, 0.636053680726515025452836696226286,
                           0.575140446819710315342946036586425, 0.510867001950827098004364050955251,
                           0.443593175238725103199992213492640, 0.373706088715419560672548177024927,
                           0.301627868114913004320555356858592, 0.227785851141645078080496195368575,
                           0.152605465240922675505220241022678, 0.076526521133497333754640409398838,
                           0.000000000000000000000000000000000
                       };

        // wgk - weights of the 41-point kronrod rule
        double[] wgk = new double[] {
                           0.003073583718520531501218293246031, 0.008600269855642942198661787950102,
                           0.014626169256971252983787960308868, 0.020388373461266523598010231432755,
                           0.025882133604951158834505067096153, 0.031287306777032798958543119323801,
                           0.036600169758200798030557240707211, 0.041668873327973686263788305936895,
                           0.046434821867497674720231880926108, 0.050944573923728691932707670050345,
                           0.055195105348285994744832372419777, 0.059111400880639572374967220648594,
                           0.062653237554781168025870122174255, 0.065834597133618422111563556969398,
                           0.068648672928521619345623411885368, 0.071054423553444068305790361723210,
                           0.073030690332786667495189417658913, 0.074582875400499188986581418362488,
                           0.075704497684556674659542775376617, 0.076377867672080736705502835038061,
                           0.076600711917999656445049901530102
                       };

        // wg - weights of the 20-point gauss rule
        double[] wg = new double[] {
                          0.017614007139152118311861962351853, 0.040601429800386941331039952274932,
                          0.062672048334109063569506535187042, 0.083276741576704748724758143222046,
                          0.101930119817240435036750135480350, 0.118194531961518417312377377711382,
                          0.131688638449176626898494499748163, 0.142096109318382051329298325067165,
                          0.149172986472603746787828737001969, 0.152753387130725850698084331955098
                      };

        // abscissa
        double absc;

        // mid point of the interval
        double centr;
        double dhlgth;
        double fc;
        double fsum;

        // function value
        double fval1;
        double fval2;
        double[] fv1 = new double[20];
        double[] fv2 = new double[20];

        // half-length of the interval
        double hlgth;

        // result of the 20-point gauss formula
        double resg;

        // result of the 41-point kronrod formula
        double resk;

        // approximation to the mean value of intFunc over
        // (a, b), i.e. to (integral of intFunc)/(b - a) over (a,b)
        double reskh;
        int j;
        int jtw;
        int jtwm1;

        centr = 0.5 * (a + b);
        hlgth = 0.5 * (b - a);
        dhlgth = Math.abs(hlgth);

        // Compute the 41-point kronrod approximation to the integral, and
        // estimate the absolute error.

        resg = 0.0;
        if (selfTest) {
        	fc = intFuncTest(centr);	
        }
        else {
            fc = intFunc(centr);
        }
        resk = fc * wgk[20];
        resabs[0] = Math.abs(resk);

        for (j = 0; j < 10; j++) {
            jtw = (2 * j) + 1;
            absc = hlgth * xgk[jtw];
            if (selfTest) {
            	fval1 = intFuncTest(centr - absc);
                fval2 = intFuncTest(centr + absc);	
            }
            else {
                fval1 = intFunc(centr - absc);
                fval2 = intFunc(centr + absc);
            }
            fv1[jtw] = fval1;
            fv2[jtw] = fval2;
            fsum = fval1 + fval2;
            resg = resg + (wg[j] * fsum);
            resk = resk + (wgk[jtw] * fsum);
            resabs[0] = resabs[0] + (wgk[jtw] * (Math.abs(fval1) + Math.abs(fval2)));
        } // for (j = 0; j < 10; j++)

        for (j = 0; j < 10; j++) {
            jtwm1 = 2 * j;
            absc = hlgth * xgk[jtwm1];
            if (selfTest) {
            	fval1 = intFuncTest(centr - absc);
                fval2 = intFuncTest(centr + absc);	
            }
            else {
                fval1 = intFunc(centr - absc);
                fval2 = intFunc(centr + absc);
            }
            fv1[jtwm1] = fval1;
            fv2[jtwm1] = fval2;
            fsum = fval1 + fval2;
            resk = resk + (wgk[jtwm1] * fsum);
            resabs[0] = resabs[0] + (wgk[jtwm1] * (Math.abs(fval1) + Math.abs(fval2)));
        } // for (j = 0; j < 10; j++)

        reskh = 0.5 * resk;
        resasc[0] = wgk[20] * Math.abs(fc - reskh);

        for (j = 0; j < 20; j++) {
            resasc[0] = resasc[0] + (wgk[j] * (Math.abs(fv1[j] - reskh) + Math.abs(fv2[j] - reskh)));
        } // for (j = 0; j < 20; j++)

        result[0] = resk * hlgth;
        resabs[0] = resabs[0] * dhlgth;
        resasc[0] = resasc[0] * dhlgth;
        abserr[0] = Math.abs((resk - resg) * hlgth);

        if ((resasc[0] != 0.0) && (abserr[0] != 0.0)) {
            abserr[0] = resasc[0] * Math.min(1.0, Math.pow(200.0 * abserr[0] / resasc[0], 1.5));
        }

        if (resabs[0] > (uflow / (50.0 * epmach))) {
            abserr[0] = Math.max((50.0 * epmach) * resabs[0], abserr[0]);
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
	 c           double precision version

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
    private void dqk51(double a, double b, double[] result, double[] abserr, double[] resabs, double[] resasc) {
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
        double[] xgk = new double[] {
                           0.999262104992609834193457486540341, 0.995556969790498097908784946893902,
                           0.988035794534077247637331014577406, 0.976663921459517511498315386479594,
                           0.961614986425842512418130033660167, 0.942974571228974339414011169658471,
                           0.920747115281701561746346084546331, 0.894991997878275368851042006782805,
                           0.865847065293275595448996969588340, 0.833442628760834001421021108693570,
                           0.797873797998500059410410904994307, 0.759259263037357630577282865204361,
                           0.717766406813084388186654079773298, 0.673566368473468364485120633247622,
                           0.626810099010317412788122681624518, 0.577662930241222967723689841612654,
                           0.526325284334719182599623778158010, 0.473002731445714960522182115009192,
                           0.417885382193037748851814394594572, 0.361172305809387837735821730127641,
                           0.303089538931107830167478909980339, 0.243866883720988432045190362797452,
                           0.183718939421048892015969888759528, 0.122864692610710396387359818808037,
                           0.061544483005685078886546392366797, 0.000000000000000000000000000000000
                       };

        // wgk - weights of the 51-point kronrod rule
        double[] wgk = new double[] {
                           0.001987383892330315926507851882843, 0.005561932135356713758040236901066,
                           0.009473973386174151607207710523655, 0.013236229195571674813656405846976,
                           0.016847817709128298231516667536336, 0.020435371145882835456568292235939,
                           0.024009945606953216220092489164881, 0.027475317587851737802948455517811,
                           0.030792300167387488891109020215229, 0.034002130274329337836748795229551,
                           0.037116271483415543560330625367620, 0.040083825504032382074839284467076,
                           0.042872845020170049476895792439495, 0.045502913049921788909870584752660,
                           0.047982537138836713906392255756915, 0.050277679080715671963325259433440,
                           0.052362885806407475864366712137873, 0.054251129888545490144543370459876,
                           0.055950811220412317308240686382747, 0.057437116361567832853582693939506,
                           0.058689680022394207961974175856788, 0.059720340324174059979099291932562,
                           0.060539455376045862945360267517565, 0.061128509717053048305859030416293,
                           0.061471189871425316661544131965264,

                           // note: wgk[25] was calculated from the values of wgk[0..24]
                           0.061580818067832935078759824240066
                       };

        // wg - weights of the 25-point gauss rule
        double[] wg = new double[] {
                          0.011393798501026287947902964113235, 0.026354986615032137261901815295299,
                          0.040939156701306312655623487711646, 0.054904695975835191925936891540473,
                          0.068038333812356917207187185656708, 0.080140700335001018013234959669111,
                          0.091028261982963649811497220702892, 0.100535949067050644202206890392686,
                          0.108519624474263653116093957050117, 0.114858259145711648339325545869556,
                          0.119455763535784772228178126512901, 0.122242442990310041688959518945852,
                          0.123176053726715451203902873079050
                      };

        // abscissa
        double absc;

        // mid point of the interval
        double centr;
        double dhlgth;
        double fc;
        double fsum;

        // function value
        double fval1;
        double fval2;
        double[] fv1 = new double[25];
        double[] fv2 = new double[25];

        // half-length of the interval
        double hlgth;

        // result of the 25-point gauss formula
        double resg;

        // result of the 51-point kronrod formula
        double resk;

        // approximation to the mean value of intFunc over
        // (a, b), i.e. to (integral of intFunc)/(b - a) over (a,b)
        double reskh;
        int j;
        int jtw;
        int jtwm1;

        centr = 0.5 * (a + b);
        hlgth = 0.5 * (b - a);
        dhlgth = Math.abs(hlgth);

        // Compute the 51-point kronrod approximation to the integral, and
        // estimate the absolute error.

        if (selfTest) {
        	fc = intFuncTest(centr);	
        }
        else {
            fc = intFunc(centr);
        }
        resg = fc * wg[12];
        resk = fc * wgk[25];
        resabs[0] = Math.abs(resk);

        for (j = 0; j < 12; j++) {
            jtw = (2 * j) + 1;
            absc = hlgth * xgk[jtw];
            if (selfTest) {
            	fval1 = intFuncTest(centr - absc);
	            fval2 = intFuncTest(centr + absc);	
            }
            else {
	            fval1 = intFunc(centr - absc);
	            fval2 = intFunc(centr + absc);
            }
            fv1[jtw] = fval1;
            fv2[jtw] = fval2;
            fsum = fval1 + fval2;
            resg = resg + (wg[j] * fsum);
            resk = resk + (wgk[jtw] * fsum);
            resabs[0] = resabs[0] + (wgk[jtw] * (Math.abs(fval1) + Math.abs(fval2)));
        } // for (j = 0; j < 12; j++)

        for (j = 0; j < 13; j++) {
            jtwm1 = 2 * j;
            absc = hlgth * xgk[jtwm1];
            if (selfTest) {
            	fval1 = intFuncTest(centr - absc);
		        fval2 = intFuncTest(centr + absc);	
            }
            else {
		        fval1 = intFunc(centr - absc);
		        fval2 = intFunc(centr + absc);
            }
            fv1[jtwm1] = fval1;
            fv2[jtwm1] = fval2;
            fsum = fval1 + fval2;
            resk = resk + (wgk[jtwm1] * fsum);
            resabs[0] = resabs[0] + (wgk[jtwm1] * (Math.abs(fval1) + Math.abs(fval2)));
        } // for (j = 0; j < 13; j++)

        reskh = 0.5 * resk;
        resasc[0] = wgk[25] * Math.abs(fc - reskh);

        for (j = 0; j < 25; j++) {
            resasc[0] = resasc[0] + (wgk[j] * (Math.abs(fv1[j] - reskh) + Math.abs(fv2[j] - reskh)));
        } // for (j = 0; j < 25; j++)

        result[0] = resk * hlgth;
        resabs[0] = resabs[0] * dhlgth;
        resasc[0] = resasc[0] * dhlgth;
        abserr[0] = Math.abs((resk - resg) * hlgth);

        if ((resasc[0] != 0.0) && (abserr[0] != 0.0)) {
            abserr[0] = resasc[0] * Math.min(1.0, Math.pow(200.0 * abserr[0] / resasc[0], 1.5));
        }

        if (resabs[0] > (uflow / (50.0 * epmach))) {
            abserr[0] = Math.max((50.0 * epmach) * resabs[0], abserr[0]);
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
	 c        double precision version

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
    private void dqk61(double a, double b, double[] result, double[] abserr, double[] resabs, double[] resasc) {
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
        double[] xgk = new double[] {
                           0.999484410050490637571325895705811, 0.996893484074649540271630050918695,
                           0.991630996870404594858628366109486, 0.983668123279747209970032581605663,
                           0.973116322501126268374693868423707, 0.960021864968307512216871025581798,
                           0.944374444748559979415831324037439, 0.926200047429274325879324277080474,
                           0.905573307699907798546522558925958, 0.882560535792052681543116462530226,
                           0.857205233546061098958658510658944, 0.829565762382768397442898119732502,
                           0.799727835821839083013668942322683, 0.767777432104826194917977340974503,
                           0.733790062453226804726171131369528, 0.697850494793315796932292388026640,
                           0.660061064126626961370053668149271, 0.620526182989242861140477556431189,
                           0.579345235826361691756024932172540, 0.536624148142019899264169793311073,
                           0.492480467861778574993693061207709, 0.447033769538089176780609900322854,
                           0.400401254830394392535476211542661, 0.352704725530878113471037207089374,
                           0.304073202273625077372677107199257, 0.254636926167889846439805129817805,
                           0.204525116682309891438957671002025, 0.153869913608583546963794672743256,
                           0.102806937966737030147096751318001, 0.051471842555317695833025213166723,
                           0.000000000000000000000000000000000
                       };

        // wgk - weights of the 61-point kronrod rule
        double[] wgk = new double[] {
                           0.001389013698677007624551591226760, 0.003890461127099884051267201844516,
                           0.006630703915931292173319826369750, 0.009273279659517763428441146892024,
                           0.011823015253496341742232898853251, 0.014369729507045804812451432443580,
                           0.016920889189053272627572289420322, 0.019414141193942381173408951050128,
                           0.021828035821609192297167485738339, 0.024191162078080601365686370725232,
                           0.026509954882333101610601709335075, 0.028754048765041292843978785354334,
                           0.030907257562387762472884252943092, 0.032981447057483726031814191016854,
                           0.034979338028060024137499670731468, 0.036882364651821229223911065617136,
                           0.038678945624727592950348651532281, 0.040374538951535959111995279752468,
                           0.041969810215164246147147541285970, 0.043452539701356069316831728117073,
                           0.044814800133162663192355551616723, 0.046059238271006988116271735559374,
                           0.047185546569299153945261478181099, 0.048185861757087129140779492298305,
                           0.049055434555029778887528165367238, 0.049795683427074206357811569379942,
                           0.050405921402782346840893085653585, 0.050881795898749606492297473049805,
                           0.051221547849258772170656282604944, 0.051426128537459025933862879215781,
                           0.051494729429451567558340433647099
                       };

        // wg - weights of the 30-point gauss rule
        double[] wg = new double[] {
                          0.007968192496166605615465883474674, 0.018466468311090959142302131912047,
                          0.028784707883323369349719179611292, 0.038799192569627049596801936446348,
                          0.048402672830594052902938140422808, 0.057493156217619066481721689402056,
                          0.065974229882180495128128515115962, 0.073755974737705206268243850022191,
                          0.080755895229420215354694938460530, 0.086899787201082979802387530715126,
                          0.092122522237786128717632707087619, 0.096368737174644259639468626351810,
                          0.099593420586795267062780282103569, 0.101762389748405504596428952168554,
                          0.102852652893558840341285636705415
                      };

        // abscissa
        double absc;

        // mid point of the interval
        double centr;
        double dhlgth;
        double fc;
        double fsum;

        // function value
        double fval1;
        double fval2;
        double[] fv1 = new double[30];
        double[] fv2 = new double[30];

        // half-length of the interval
        double hlgth;

        // result of the 30-point gauss formula
        double resg;

        // result of the 61-point kronrod formula
        double resk;

        // approximation to the mean value of intFunc over
        // (a, b), i.e. to (integral of intFunc)/(b - a) over (a,b)
        double reskh;
        int j;
        int jtw;
        int jtwm1;

        centr = 0.5 * (a + b);
        hlgth = 0.5 * (b - a);
        dhlgth = Math.abs(hlgth);

        // Compute the 61-point kronrod approximation to the integral, and
        // estimate the absolute error.

        resg = 0.0;
        if (selfTest) {
        	fc = intFuncTest(centr);	
        }
        else {
            fc = intFunc(centr);
        }
        resk = fc * wgk[30];
        resabs[0] = Math.abs(resk);

        for (j = 0; j < 15; j++) {
            jtw = (2 * j) + 1;
            absc = hlgth * xgk[jtw];
            if (selfTest) {
            	fval1 = intFuncTest(centr - absc);
                fval2 = intFuncTest(centr + absc);	
            }
            else {
                fval1 = intFunc(centr - absc);
                fval2 = intFunc(centr + absc);
            }
            fv1[jtw] = fval1;
            fv2[jtw] = fval2;
            fsum = fval1 + fval2;
            resg = resg + (wg[j] * fsum);
            resk = resk + (wgk[jtw] * fsum);
            resabs[0] = resabs[0] + (wgk[jtw] * (Math.abs(fval1) + Math.abs(fval2)));
        } // for (j = 0; j < 15; j++)

        for (j = 0; j < 15; j++) {
            jtwm1 = 2 * j;
            absc = hlgth * xgk[jtwm1];
            if (selfTest) {
            	fval1 = intFuncTest(centr - absc);
                fval2 = intFuncTest(centr + absc);	
            }
            else {
                fval1 = intFunc(centr - absc);
                fval2 = intFunc(centr + absc);
            }
            fv1[jtwm1] = fval1;
            fv2[jtwm1] = fval2;
            fsum = fval1 + fval2;
            resk = resk + (wgk[jtwm1] * fsum);
            resabs[0] = resabs[0] + (wgk[jtwm1] * (Math.abs(fval1) + Math.abs(fval2)));
        } // for (j = 0; j < 15; j++)

        reskh = 0.5 * resk;
        resasc[0] = wgk[30] * Math.abs(fc - reskh);

        for (j = 0; j < 30; j++) {
            resasc[0] = resasc[0] + (wgk[j] * (Math.abs(fv1[j] - reskh) + Math.abs(fv2[j] - reskh)));
        } // for (j = 0; j < 30; j++)

        result[0] = resk * hlgth;
        resabs[0] = resabs[0] * dhlgth;
        resasc[0] = resasc[0] * dhlgth;
        abserr[0] = Math.abs((resk - resg) * hlgth);

        if ((resasc[0] != 0.0) && (abserr[0] != 0.0)) {
            abserr[0] = resasc[0] * Math.min(1.0, Math.pow(200.0 * abserr[0] / resasc[0], 1.5));
        }

        if (resabs[0] > (uflow / (50.0 * epmach))) {
            abserr[0] = Math.max((50.0 * epmach) * resabs[0], abserr[0]);
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
	 c double precision version

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

        double[] x1 = new double[] {
                          0.973906528517171720077964012084452, 0.865063366688984510732096688423493,
                          0.679409568299024406234327365114874, 0.433395394129247190799265943165784,
                          0.148874338981631210884826001129720
                      };

        double[] w10 = new double[] {
                           0.066671344308688137593568809893332, 0.149451349150580593145776339657697,
                           0.219086362515982043995534934228163, 0.269266719309996355091226921569469,
                           0.295524224714752870173892994651338
                       };

        double[] x2 = new double[] {
                          0.995657163025808080735527280689003, 0.930157491355708226001207180059508,
                          0.780817726586416897063717578345042, 0.562757134668604683339000099272694,
                          0.294392862701460198131126603103866
                      };

        double[] w21a = new double[] {
                            0.032558162307964727478818972459390, 0.075039674810919952767043140916190,
                            0.109387158802297641899210590325805, 0.134709217311473325928054001771707,
                            0.147739104901338491374841515972068
                        };

        double[] w21b = new double[] {
                            0.011694638867371874278064396062192, 0.054755896574351996031381300244580,
                            0.093125454583697605535065465083366, 0.123491976262065851077958109831074,
                            0.142775938577060080797094273138717, 0.149445554002916905664936468389821
                        };

        double[] x3 = new double[] {
                          0.999333360901932081394099323919911, 0.987433402908088869795961478381209,
                          0.954807934814266299257919200290473, 0.900148695748328293625099494069092,
                          0.825198314983114150847066732588520, 0.732148388989304982612354848755461,
                          0.622847970537725238641159120344323, 0.499479574071056499952214885499755,
                          0.364901661346580768043989548502644, 0.222254919776601296498260928066212,
                          0.074650617461383322043914435796506
                      };

        double[] w43a = new double[] {
                            0.016296734289666564924281974617663, 0.037522876120869501461613795898115,
                            0.054694902058255442147212685465005, 0.067355414609478086075553166302174,
                            0.073870199632393953432140695251367, 0.005768556059769796184184327908655,
                            0.027371890593248842081276069289151, 0.046560826910428830743339154433824,
                            0.061744995201442564496240336030883, 0.071387267268693397768559114425516
                        };

        double[] w43b = new double[] {
                            0.001844477640212414100389106552965, 0.010798689585891651740465406741293,
                            0.021895363867795428102523123075149, 0.032597463975345689443882222526137,
                            0.042163137935191811847627924327955, 0.050741939600184577780189020092084,
                            0.058379395542619248375475369330206, 0.064746404951445885544689259517511,
                            0.069566197912356484528633315038405, 0.072824441471833208150939535192842,
                            0.074507751014175118273571813842889, 0.074722147517403005594425168280423
                        };

        double[] x4 = new double[] {
                          0.999902977262729234490529830591582, 0.997989895986678745427496322365960,
                          0.992175497860687222808523352251425, 0.981358163572712773571916941623894,
                          0.965057623858384619128284110607926, 0.943167613133670596816416634507426,
                          0.915806414685507209591826430720050, 0.883221657771316501372117548744163,
                          0.845710748462415666605902011504855, 0.803557658035230982788739474980964,
                          0.757005730685495558328942793432020, 0.706273209787321819824094274740840,
                          0.651589466501177922534422205016736, 0.593223374057961088875273770349144,
                          0.531493605970831932285268948562671, 0.466763623042022844871966781659270,
                          0.399424847859218804732101665817923, 0.329874877106188288265053371824597,
                          0.258503559202161551802280975429025, 0.185695396568346652015917141167606,
                          0.111842213179907468172398359241362, 0.037352123394619870814998165437704
                      };

        double[] w87a = new double[] {
                            0.008148377384149172900002878448190, 0.018761438201562822243935059003794,
                            0.027347451050052286161582829741283, 0.033677707311637930046581056957588,
                            0.036935099820427907614589586742499, 0.002884872430211530501334156248695,
                            0.013685946022712701888950035273128, 0.023280413502888311123409291030404,
                            0.030872497611713358675466394126442, 0.035693633639418770719351355457044,
                            0.000915283345202241360843392549948, 0.005399280219300471367738743391053,
                            0.010947679601118931134327826856808, 0.016298731696787335262665703223280,
                            0.021081568889203835112433060188190, 0.025370969769253827243467999831710,
                            0.029189697756475752501446154084920, 0.032373202467202789685788194889595,
                            0.034783098950365142750781997949596, 0.036412220731351787562801163687577,
                            0.037253875503047708539592001191226
                        };

        double[] w87b = new double[] {
                            0.000274145563762072350016527092881, 0.001807124155057942948341311753254,
                            0.004096869282759164864458070683480, 0.006758290051847378699816577897424,
                            0.009549957672201646536053581325377, 0.012329447652244853694626639963780,
                            0.015010447346388952376697286041943, 0.017548967986243191099665352925900,
                            0.019938037786440888202278192730714, 0.022194935961012286796332102959499,
                            0.024339147126000805470360647041454, 0.026374505414839207241503786552615,
                            0.028286910788771200659968002987960, 0.030052581128092695322521110347341,
                            0.031646751371439929404586051078883, 0.033050413419978503290785944862689,
                            0.034255099704226061787082821046821, 0.035262412660156681033782717998428,
                            0.036076989622888701185500318003895, 0.036698604498456094498018047441094,
                            0.037120549269832576114119958413599, 0.037334228751935040321235449094698,
                            0.037361073762679023410321241766599
                        };

        // mid point of integration interval
        double centr;

        // half-length of integration interval
        double hlgth;

        // abscissa
        double asbc;

        // function value
        double fval;

        // array of function values which have already been computed
        double[] savfun = new double[21];

        // 10-point gauss result
        double res10;

        // 21-point kronrod result
        double res21;

        // 43-point result
        double res43;

        // 87-point result
        double res87;

        // approximation to integral of abs(intFunc)
        double resabs;

        // approximation to integral of
        // abs(intFunc - integral of intFunc over (lower, upper)/(upper - lower))
        double dhlgth;
        double fcentr;
        int k;
        double absc;
        double fval1;
        double fval2;
        double[] fv1 = new double[5];
        double[] fv2 = new double[5];
        double[] fv3 = new double[5];
        double[] fv4 = new double[5];
        int ipx;
        double reskh;
        double resasc;

        result[0] = 0.0;
        abserr[0] = 0.0;
        hlgth = 0.5 * (upper - lower);
        dhlgth = Math.abs(hlgth);
        centr = 0.5 * (upper + lower);
        if (selfTest) {
        	fcentr = intFuncTest(centr);	
        }
        else {
            fcentr = intFunc(centr);
        }
        neval[0] = 21;
        errorStatus = 1;

        // Compute the integral using the 10- and 21-point formula.

        res10 = 0.0;
        res21 = w21b[5] * fcentr;
        resabs = w21b[5] * Math.abs(fcentr);

        for (k = 0; k < 5; k++) {
            absc = hlgth * x1[k];
            if (selfTest) {
            	fval1 = intFuncTest(centr + absc);
                fval2 = intFuncTest(centr - absc);	
            }
            else {
                fval1 = intFunc(centr + absc);
                fval2 = intFunc(centr - absc);
            }
            fval = fval1 + fval2;
            res10 = res10 + (w10[k] * fval);
            res21 = res21 + (w21a[k] * fval);
            resabs = resabs + (w21a[k] * (Math.abs(fval1) + Math.abs(fval2)));
            savfun[k] = fval;
            fv1[k] = fval1;
            fv2[k] = fval2;
        } // for (k = 0; k < 5; k++)

        ipx = 4;

        for (k = 0; k < 5; k++) {
            ipx++;
            absc = hlgth * x2[k];
            if (selfTest) {
            	fval1 = intFuncTest(centr + absc);
                fval2 = intFuncTest(centr - absc);	
            }
            else {
                fval1 = intFunc(centr + absc);
                fval2 = intFunc(centr - absc);
            }
            fval = fval1 + fval2;
            res21 = res21 + (w21b[k] * fval);
            resabs = resabs + (w21b[k] * (Math.abs(fval1) + Math.abs(fval2)));
            savfun[ipx] = fval;
            fv3[k] = fval1;
            fv4[k] = fval2;
        } // for (k = 0; k < 5; k++)

        // Test for convergence

        result[0] = res21 * hlgth;
        resabs = resabs * dhlgth;
        reskh = 0.5 * res21;
        resasc = w21b[5] * Math.abs(fcentr - reskh);

        for (k = 0; k < 5; k++) {
            resasc = resasc + (w21a[k] * (Math.abs(fv1[k] - reskh) + Math.abs(fv2[k] - reskh))) +
                     (w21b[k] * (Math.abs(fv3[k] - reskh) + Math.abs(fv4[k] - reskh)));
        } // for (k = 0; k < 5; k++)

        abserr[0] = Math.abs((res21 - res10) * hlgth);
        resasc = resasc * dhlgth;

        if ((resasc != 0.0) && (abserr[0] != 0.0)) {
            abserr[0] = resasc * (Math.min(1.0, Math.pow((200.0 * abserr[0] / resasc), 1.5)));
        } // if ((resasc != 0.0) && (abserr[0] != 0.0))

        if (resabs > (uflow / (50.0 * epmach))) {
            abserr[0] = Math.max((epmach * 50.0) * resabs, abserr[0]);
        } // if (resabs > uflow/(50.0 * epmach))

        if (abserr[0] <= Math.max(epsabs, epsrel * Math.abs(result[0]))) {
            errorStatus = 0;
        } // if (abserr[0] <= Math.max(epsabs, epsrel * Math.abs(result[0])))

        if (errorStatus == 0) {
            return;
        } // if (errorStatus == 0)

        // Compute the integral using the 43-point formula

        res43 = w43b[11] * fcentr;
        neval[0] = 43;

        for (k = 0; k < 10; k++) {
            res43 = res43 + (savfun[k] * w43a[k]);
        } // for (k = 0; k < 10; k++)

        for (k = 0; k < 11; k++) {
            ipx++;
            absc = hlgth * x3[k];
            if (selfTest) {
            	fval = intFuncTest(absc + centr) + intFuncTest(centr - absc);	
            }
            else {
                fval = intFunc(absc + centr) + intFunc(centr - absc);
            }
            res43 = res43 + (fval * w43b[k]);
            savfun[ipx] = fval;
        } // for (k = 0; k < 11; k++)

        // Test for convergence
        result[0] = res43 * hlgth;
        abserr[0] = Math.abs((res43 - res21) * hlgth);

        if ((resasc != 0.0) && (abserr[0] != 0.0)) {
            abserr[0] = resasc * (Math.min(1.0, Math.pow((200.0 * abserr[0] / resasc), 1.5)));
        } // if ((resasc != 0.0) && (abserr[0] != 0.0))

        if (resabs > (uflow / (50.0 * epmach))) {
            abserr[0] = Math.max((epmach * 50.0) * resabs, abserr[0]);
        } // if (resabs > uflow/(50.0 * epmach))

        if (abserr[0] <= Math.max(epsabs, epsrel * Math.abs(result[0]))) {
            errorStatus = 0;
        } // if (abserr[0] <= Math.max(epsabs, epsrel * Math.abs(result[0])))

        if (errorStatus == 0) {
            return;
        } // if (errorStatus == 0)

        // Compute the integral using the 87-point formula
        res87 = w87b[22] * fcentr;
        neval[0] = 87;

        for (k = 0; k < 21; k++) {
            res87 = res87 + (savfun[k] * w87a[k]);
        } // for (k = 0; k < 21; k++)

        for (k = 0; k < 22; k++) {
            absc = hlgth * x4[k];
            if (selfTest) {
            	res87 = res87 + (w87b[k] * (intFuncTest(absc + centr) + intFuncTest(centr - absc)));	
            }
            else {
                res87 = res87 + (w87b[k] * (intFunc(absc + centr) + intFunc(centr - absc)));
            }
        } // for (k = 0; k < 22; k++)

        result[0] = res87 * hlgth;
        abserr[0] = Math.abs((res87 - res43) * hlgth);

        if ((resasc != 0.0) && (abserr[0] != 0.0)) {
            abserr[0] = resasc * (Math.min(1.0, Math.pow((200.0 * abserr[0] / resasc), 1.5)));
        } // if ((resasc != 0.0) && (abserr[0] != 0.0))

        if (resabs > (uflow / (50.0 * epmach))) {
            abserr[0] = Math.max((epmach * 50.0) * resabs, abserr[0]);
        } // if (resabs > uflow/(50.0 * epmach))

        if (abserr[0] <= Math.max(epsabs, epsrel * Math.abs(result[0]))) {
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
	 c           double precision version

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
    private void dqpsrt(int limit, int last, int[] maxErr, double[] ermax, double[] elist, int[] iord, int[] nrmax) {
        double errmin;
        double errmax;
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
            ermax[0] = elist[maxErr[0]];

            return;
        } // if (last <= 2)

        // This part of the routine is only executed if, due to a difficult
        // integrand, subdivision increased the error estimate.  In the
        // normal case the insert procedure should start after the nrmax-th
        // largest error estimate

        errmax = elist[maxErr[0]];

        if (nrmax[0] != 1) {
            ido = nrmax[0] - 1;

            for (i = 1; i <= ido; i++) {
                isucc = iord[nrmax[0] - 2];

                if (errmax <= elist[isucc]) {
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

        errmin = elist[last - 1];

        // Insert errmax by traversing the list top-down,
        // starting comparison from the element elist(iord[nrmmax[0]])

        jbnd = jupbn - 1;
        ibeg = nrmax[0] + 1;

group:   {

            if (ibeg <= jbnd) {

                for (i = ibeg; i <= jbnd; i++) {
                    isucc = iord[i - 1];

                    if (errmax >= elist[isucc]) {
                        break group;
                    } // if (errmax >= elist[isucc])

                    iord[i - 2] = isucc;
                } // for (i = ibeg; i <= jbnd; i++)
            } // if (ibeg <= jbnd)

            iord[jbnd - 1] = maxErr[0];
            iord[jupbn - 1] = last - 1;
            maxErr[0] = iord[nrmax[0] - 1];
            ermax[0] = elist[maxErr[0]];

            return;
        } // group

        // Insert errmin by traversing the list from the bottom-up

        iord[i - 2] = maxErr[0];
        k = jbnd;

group2:  {

            for (j = i; j <= jbnd; j++) {
                isucc = iord[k - 1];

                if (errmin < elist[isucc]) {
                    break group2;
                } // if (errmin < elist[isucc])

                iord[k] = isucc;
                k = k - 1;
            } // for (j = i; j <= jbnd; j++)

            iord[i - 1] = last - 1;
            maxErr[0] = iord[nrmax[0] - 1];
            ermax[0] = elist[maxErr[0]];

            return;
        } // group2

        iord[k] = last - 1;
        maxErr[0] = iord[nrmax[0] - 1];
        ermax[0] = elist[maxErr[0]];

        return;
    }
}
