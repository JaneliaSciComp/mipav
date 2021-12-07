package gov.nih.mipav.model.algorithms;


import gov.nih.mipav.view.*;


/**
 * This is a port of FORTRAN numerical integration routines in QUADPACK found at http://www.netlib.org/quadpack
 * 
 * Reference: R. Piessens, E. deDoncker-Kapenga, C. Uberhuber, D. Kahaner Quadpack: a Subroutine Package for Automatic
 * Integration Springer Verlag, 1983. Series in Computational Mathematics v. 1
 * 
 * The original dqage, dqagie, dqagpe, dqagse, dqawce, dqawfe, dqawoe, adn dqawse routines were written by Robert
 * Piessens and Elise de Doncker.
 * 
 * The original dqng routine was written by Robert Piessens and Elise de Doncker and modified by David Kahaner.
 * 
 * The original dqc25c, dqc25f, dqc25s, dqcheb, dqelg, dqk15, dqk15i, dqk15w, dqk21, dqk31, dqk41, dqk51, dqk61, dqmomo,
 * dqpsrt, dqwgtc, dqwgtf, and dqwgts routines were written by Robert Piessens and Elise de Doncker.
 * 
 * The simple contents of dqwgtc, dqwgtf, and dqwgts are incorporated into dqk15w.
 * 
 * The linpack routine dgtsl was written by Jack Dongarra of the Argonne National Laboratory.
 * 
 * Self tests 1 to 15 come from quadpack_prb.f90 by John Burkardt.
 * 
 * Porting was performed by William Gandler.
 * 
 * <hr>
 * The QUADPACK code is made freely available, with the following restrictions:
 * 
 * <pre>
 * Re-distributions of the source code should credit Quadpack and retain the notice with the names of the original authors;
 * the names of the copyright holders or contributors may not be used to endorse or promote any derived software without prior permission;
 * the Quadpack software is provided &quot;as is&quot;, without warranties;
 * Quadpack and the authors deny any liability for situations resulting from the use of this software.
 * </pre>
 */

// Example code is below:
/*
 * private void runIntegrationTest2() { IntModel2 imod; double lower = 0.0; double upper = 1.0; int routine =
 * Integration2.DQAGPE; double breakPoints[] = new double[4]; breakPoints[0] = 1.0/7.0; breakPoints[1] = 2.0/3.0; double
 * epsabs = 0.0; double epsrel = 1.0E-3; int limit = 100; double numInt; int errorStatus; double absError; int neval;
 * imod = new IntModel2(lower, upper, routine, breakPoints, epsabs, epsrel, limit); imod.driver(); numInt =
 * imod.getIntegral(); errorStatus = imod.getErrorStatus(); absError = imod.getAbserr(); neval = imod.getNeval();
 * Preferences.debug("Numerical Integral = " + numInt + " after " + neval + " integrand evaluations used\n");
 * Preferences.debug("Error status = " + errorStatus + " with absolute error = " + absError + "\n"); }
 */

/*
 * class IntModel2 extends Integration2 { public IntModel2(double lower, double upper, int routine, double
 * breakPoints[], double epsabs, double epsrel, int limit) { super(lower, upper, routine, breakPoints, epsabs, epsrel,
 * limit); }
 * 
 * public double intFunc(double x) { double function = 0.0; if ((x != 1.0/7.0) && (x != 2.0/3.0)) { function =
 * Math.pow(Math.abs(x - 1.0/7.0), -0.25) * Math.pow(Math.abs(x - 2.0/3.0), -0.55); } return function; }
 * 
 * public void driver() { super.driver(); } }
 */

/*
 * private void runIntegrationTest2() { IntModel2 imod; double boun = 0.0; int routine = Integration2.DQAGIE; int inf =
 * 1; double epsabs = 0.0; double epsrel = 1.0E-3; int limit = 100; double numInt; int errorStatus; double absError; int
 * neval; imod = new IntModel2(boun, routine, inf, epsabs, epsrel, limit); imod.driver(); numInt = imod.getIntegral();
 * errorStatus = imod.getErrorStatus(); absError = imod.getAbserr(); neval = imod.getNeval();
 * Preferences.debug("Numerical Integral = " + numInt + " after " + neval + " integrand evaluations used\n");
 * Preferences.debug("Error status = " + errorStatus + " with absolute error = " + absError + "\n"); }
 */

/*
 * class IntModel2 extends Integration2 { public IntModel2(double boun, int routine, int inf, double epsabs, double
 * epsrel, int limit) { super(boun, routine, inf, epsabs, epsrel, limit); }
 * 
 * public double intFunc(double x) { double function = 0.0; if (x > 0.0) { function = Math.sqrt(x) * Math.log(x)/((x +
 * 1.0) * (x + 2.0)); } return function; }
 * 
 * public void driver() { super.driver(); } }
 */

public abstract class Integration2 {

    // ~ Static fields/initializers
    // -------------------------------------------------------------------------------------

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

    /** Automatic integrator, integrand with oscillatory cosine or sine factor */
    protected static final int DQAWOE = 13;

    /** Adaptive integrator for integrands with algebraic or logarithmic singularities at endpoints. */
    protected static final int DQAWSE = 14;

    /** Automatic integrator, special-purpose, fourier integrals */
    protected static final int DQAWFE = 15;

    // ~ Instance fields
    // ------------------------------------------------------------------------------------------------

    /** Estimate of the absolute value of the error, which should equal or exceed abs(actual integral - result). */
    private final double[] abserr = new double[1];

    /**
     * parameter in the weight function, alfa.gt.(-1) if alfa.le.(-1), the routine will end with ier[0] = 6.
     */
    private double alfa;

    /** The left end points of the subintervals in the partition of the given integration range (lower, upper). */
    private double[] alist;

    /**
     * parameter in the weight function, beta.gt.(-1) if beta.le.(-1), the routine will end with ier[0] = 6.
     */
    private double beta;

    /** The right end points of the subintervals in the partition of the given integration range (lower, upper). */
    private double[] blist;

    /** finite bound of integration range used in dqagie (has no meaning if interval is doubly-infinite). */
    private double bound;

    /**
     * Used in dqagpe This array must always be >= 2 in length. The first breakPoints.length - 2 points are user
     * provided break points. If these points are not in an ascending sequence, they will automatically be sorted.
     */
    private double[] breakPoints;

    /**
     * Parameter in the weight function, c != lower, c != upper. If c == a or c == b, the routine will end with
     * errorStatus = 6
     */
    private double c;

    /**
     * array of dimension (maxp1,25) containing the chebyshev moments
     */
    private double[][] chebmo = null;

    /** Estimates of the absolute errors on the subintervals. */
    private double[] elist;

    /**
     * epmach = D1MACH(4) Machine epmach is the smallest positive epmach such that (1.0 + epmach) != 1.0. epmach = 2**(1 -
     * doubleDigits) = 2**(1 - 53) = 2**(-52) epmach = 2.224460e-16 epmach is called the largest relative spacing
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
     * In dqawfe vector of dimension at least limlst erlst[k] contains the error estimate corresponding wiht rstlst[k].
     */
    private double erlst[];

    private double gamma;

    /**
     * if dqawoe is to be used only once, icall must be set to 1. assume that during this call, the c chebyshev moments
     * (for clenshaw-curtis integration c of degree 24) have been computed for intervals of c lenghts
     * (abs(b-a))*2**(-l), l=0,1,2,...momcom-1. c if icall.gt.1 this means that dqawoe has been c called twice or more
     * on intervals of the same c length abs(b-a). the chebyshev moments already c computed are then re-used in
     * subsequent calls. c if icall.lt.1, the routine will end with ier[0] = 6.
     */
    private int icall;

    /**
     * ier[0] = 0 for normal and reliable termination of the routine. It is assumed that the requested accuracy has been
     * achieved. ier[0] > 0 for abnormal termination of the routine. The estimates for the integral and error are less
     * reliable. It is assumed that the requested accuracy has not been achieved.
     * 
     * ier[0] = 1 maximum number of subdivisions allowed has been achieved. One can allow more subdivisions by
     * increasing the value of limit (and taking the according dimension adjustments into account). However, if this
     * yields no improvement, it is advised to analyze the integrand in order to determine the integration difficulties.
     * If the position of a local difficulty can be determined( i.e. singularity, discontinuity within the interval), it
     * should be supplied to the routine as an element of the breakPoints array. If necessary, an appropriate special
     * purpose integrator must be used, which is designed for handling the type of difficulty involved.
     * 
     * ier[0] = 2 The occurrence of roundoff error is detected, which prevents the requested tolerance from being
     * achieved. The error may be under-estimated.
     * 
     * ier[0] = 3 Extremely bad integrand behavior occurs at some points of the integration interval.
     * 
     * ier[0] = 4 The algorithm does not converge. Roundoff error is detected in the extrapolation table. It is presumed
     * that the requested tolerance cannot be achieved, and that the returned result is the best that can be obtained.
     * 
     * ier[0] = 5 The integral is probably divergent, or slowly convergent. It must be noted that divergence can occur
     * with any other value of ier[0] > 0.
     * 
     * ier[0] = 6 The input is invalid because (epsabs <= 0 and epsrel < max(50.0 * relative machine accuracy, 5.0E-29)
     * or in the case of dqagpe can also happen because npts2 < 2, the breakPoints are specified outside the integration
     * range, or , or limit <= npts.
     */
    private final int ier[] = new int[1];

    /**
     * In dqawfe vector of dimension at least limlst ierlst[k] contains the error flag corresponding with rstlst[k]. For
     * the meaning of the local error flags see description of the output parameter ier.
     */
    private int ierlst[];

    /**
     * In dqagie indicates the kind of integration range involved inf = 1 corresponds to (bound, +infinity) inf = -1
     * corresponds to (-infinity, bound) inf = 2 corresponds to (-infinity, +infinity).
     */
    private int inf;

    /**
     * With dqawoe: indicates which of the weight functions is to be used integr = 1 w(x) = cos(omega*x) integr = 2 w(x) =
     * sin(omega*x) if integr.ne.1 and integr.ne.2, the routine will end with ier[0] = 6. With dqawse: indicates which
     * weight function is to be used = 1 (x-lower)**alfa*(upper-x)**beta = 2
     * (x-lower)**alfa*(upper-x)**beta*log(x-lower) = 3 (x-lower)**alfa*(upper-x)**beta*log(upper-x) = 4
     * (x-lower)**alfa*(upper-x)**beta*log(x-lower)*log(upper-x) if integr.lt.1 or integr.gt.4, the routine will end
     * with ier[0] = 6.
     */
    private int integr;

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
     * npts2. If limit < npts2 in dqagpe, the routine exits with an error message. In dqawse limit must be >= 2. If
     * limit < 2 in dqawse, the routine exits with ier[0] = 6.
     */
    private int limit;

    /**
     * In dqawfe limlst gives an upper bound on the number of cycles, limlst >= 1. If limlst < 3, the routine will end
     * with ier[0] = 6.
     */
    private int limlst;

    /** DOCUMENT ME! */
    private double lower;

    /**
     * In dqawfe number of subintervals needed for the integration. If omega = 0 then lst is set to 1.
     */
    private int lst;

    /**
     * gives an upper bound on the number of chebyshev c moments which can be stored, i.e. for the c intervals of
     * lenghts abs(b-a)*2**(-l), c l=0,1, ..., maxp1-2, maxp1.ge.1. c if maxp1.lt.1, the routine will end with ier[0] =
     * 6.
     */
    private int maxp1;

    /**
     * indicating that the chebyshev moments c have been computed for intervals of lengths c (abs(b-a))*2**(-l),
     * l=0,1,2, ..., momcom-1, c momcom.lt.maxp1
     */
    private int momcom;

    /**
     * After the first integration over the intervals (pts[i], pts[i+1]), i = 0, 1, ..., npts2 - 2, the error estimates
     * over some of the intervals may have been increased artificially, in order to put their subdivision forward. If
     * this happens for the subinterval numbered k, ndin[k] is put to 1, otherwise ndin[k] = 0.
     */
    private int[] ndin;

    /** Number of integrand evaluations. */
    private final int neval[] = new int[1];

    /**
     * vector of dimension at least limit, containing the c subdivision levels of the subintervals, i.e. c iwork(i) = l
     * means that the subinterval c numbered i is of length abs(b-a)*2**(1-l)
     */
    private int nnlog[];

    /** npts = npts2 - 2. */
    private int npts;

    /** npts2 = breakPoints.length. */
    private int npts2;

    /** D1MACH(2) = 2**(emax)*(1 - 2**(-doubleDigits)) = 2**1024*(1 - 2**-53) D1MACH(2) = Double.MAX_VALUE. */
    private final double oflow = Double.MAX_VALUE;

    /** parameter in the integrand weight function */
    private double omega;

    /** Integration limits and the break points of the interval in ascending sequence. */
    private double[] pts;

    private final double[] resabs = new double[1];

    private final double[] resasc = new double[1];

    /** Approximation to the integral. */
    private final double[] result = new double[1];

    /** The integral approximations of the subintervals. */
    private double[] rlist;

    /** DOCUMENT ME! */
    private int routine;

    /**
     * In dqawfe vector of dimension at least limlst. rslst[k-1] contains the integral contribution over the interval
     * (a+(k-1)c, a+kc), where c = (2*int(abs(omega))+1)*pi/abs(omega), k = 1, 2, ..., lst. Note that if omega = 0,
     * rslst[0] contains the value of the integral over (a, infinity).
     */
    private double rslst[];

    /** DOCUMENT ME! */
    private final double uflow = Math.pow(2, -1022);

    /** DOCUMENT ME! */
    private double upper;

    private boolean selfTest = false;

    private int testCase = 1;

    private double actualAnswer;

    // ~ Constructors
    // ---------------------------------------------------------------------------------------------------

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
        Preferences.debug("epmach = " + epmach + "\n", Preferences.DEBUG_ALGORITHM);

        tol = Math.max(50.0 * epmach, 5.0E-29);
        Preferences.debug("tol = " + tol + "\n", Preferences.DEBUG_ALGORITHM);

        testCase = 1;
        // dqage is an adaptive automatic integrator using a Gauss-Kronod rule.
        // Integrate cos(100*sin(x)) from 0 to PI.
        // The exact answer is PI * j0(100), or roughly 0.06278740.
        // key chooses the order of the integration rule from 1 to 6.
        // Numerical Integral = 0.06278740049149303 after 427 integrand evaluations used
        // Error status = 0 with estimated absolute error = 9.163654279831235E-9
        // Actual answer = 0.06278740049149267
        // Exact error = 3.608224830031759E-16
        final double realArg = 100.0;
        final double imaginaryArg = 0.0;
        final double initialOrder = 0.0;
        final int sequenceNumber = 1; // Number of sequential Bessel function orders calculated
        final double realResult[] = new double[1];
        final double imagResult[] = new double[1];
        final int[] nz = new int[1]; // number of components set to zero due to underflow
        final int[] errorFlag = new int[1]; // zero if no error

        final Bessel bes = new Bessel(Bessel.BESSEL_J, realArg, imaginaryArg, initialOrder, Bessel.UNSCALED_FUNCTION,
                sequenceNumber, realResult, imagResult, nz, errorFlag);
        bes.run();
        if (errorFlag[0] != 0) {
            Preferences.debug("Bessel_J error for realArg = " + realArg + "\n", Preferences.DEBUG_ALGORITHM);
        }
        Preferences.debug("j0(100.0) = " + realResult[0] + "\n", Preferences.DEBUG_ALGORITHM);

        lower = 0.0;
        upper = Math.PI;
        routine = Integration2.DQAGE;
        key = 6;
        epsabs = 0.0;
        epsrel = 1.0E-3;
        actualAnswer = Math.PI * realResult[0];
        limit = 100;

        if (upper <= lower) {
            MipavUtil.displayError("upper must be > than lower");
            ier[0] = 6;

            return;
        }

        if ( (epsabs <= 0.0) && (epsrel < tol)) {
            MipavUtil.displayError("Must set epsabs > 0.0 or must set epsrel >= " + tol);
            ier[0] = 6;

            return;
        }

        if (limit <= 1) {
            MipavUtil.displayError("limit must be >= 1");
            ier[0] = 6;

            return;
        }

        driver();

        Preferences.debug("Test 1 testing dqage\n", Preferences.DEBUG_ALGORITHM);
        Preferences.debug("Integrand is cos(100.0*sin(x))\n", Preferences.DEBUG_ALGORITHM);
        Preferences.debug("Integrand lower endpoint = 0.0\n", Preferences.DEBUG_ALGORITHM);
        Preferences.debug("Integrand upper endpoint = PI\n", Preferences.DEBUG_ALGORITHM);
        Preferences.debug("Numerical Integral = " + result[0] + " after " + neval[0] + " integrand evaluations used\n", 
        		Preferences.DEBUG_ALGORITHM);
        Preferences.debug("Error status = " + ier[0] + " with estimated absolute error = " + abserr[0] + "\n", 
        		Preferences.DEBUG_ALGORITHM);
        Preferences.debug("Actual answer = " + actualAnswer + "\n", Preferences.DEBUG_ALGORITHM);
        Preferences.debug("Exact error = " + (result[0] - actualAnswer) + "\n", Preferences.DEBUG_ALGORITHM);

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
        actualAnswer = -Math.PI * Math.log(10.0) / 20.0;

        if ( (epsabs <= 0.0) && (epsrel < tol)) {
            MipavUtil.displayError("Must set epsabs > 0.0 or must set epsrel >= " + tol);
            ier[0] = 6;

            return;
        }

        if (limit <= 1) {
            MipavUtil.displayError("limit must be >= 1");
            ier[0] = 6;

            return;
        }

        driver();

        Preferences.debug("Test 2 testing dqagie\n", Preferences.DEBUG_ALGORITHM);
        Preferences.debug("Integrand is log(x)/(1 + 100*x*x)\n", Preferences.DEBUG_ALGORITHM);
        Preferences.debug("Integrand lower endpoint = 0.0\n", Preferences.DEBUG_ALGORITHM);
        Preferences.debug("Integrand upper endpoint = infinity\n", Preferences.DEBUG_ALGORITHM);
        Preferences.debug("Numerical Integral = " + result[0] + " after " + neval[0] + " integrand evaluations used\n", 
        		Preferences.DEBUG_ALGORITHM);
        Preferences.debug("Error status = " + ier[0] + " with estimated absolute error = " + abserr[0] + "\n", 
        		Preferences.DEBUG_ALGORITHM);
        Preferences.debug("Actual answer = " + actualAnswer + "\n", Preferences.DEBUG_ALGORITHM);
        Preferences.debug("Exact error = " + (result[0] - actualAnswer) + "\n", Preferences.DEBUG_ALGORITHM);

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
        actualAnswer = 61.0 * Math.log(2.0) + 77.0 * Math.log(7.0) / 4.0 - 27.0;

        if (upper <= lower) {
            MipavUtil.displayError("upper must be > than lower");
            ier[0] = 6;

            return;
        }

        if (npts2 < 2) {
            MipavUtil.displayError("breakPoints array length must be >= 2");
            ier[0] = 6;

            return;
        }

        for (int i = 0; i < (npts2 - 2); i++) {

            if (breakPoints[i] < lower) {
                MipavUtil.displayError("Break point " + breakPoints[i] + " is impossibly < lower");
                ier[0] = 6;

                return;
            }

            if (breakPoints[i] > upper) {
                MipavUtil.displayError("Break point " + breakPoints[i] + " is impossibly > upper");
                ier[0] = 6;

                return;
            }
        } // for (int i = 0; i < npts2 - 2; i++)

        if ( (epsabs <= 0.0) && (epsrel < tol)) {
            MipavUtil.displayError("Must set epsabs > 0.0 or must set epsrel >= " + tol);
            ier[0] = 6;

            return;
        }

        if (limit <= npts) {
            MipavUtil.displayError("Must set limit = " + limit + " > " + npts);
            ier[0] = 6;

            return;
        }

        driver();

        Preferences.debug("Test 3 testing dqagpe\n", Preferences.DEBUG_ALGORITHM);
        Preferences.debug("Integrand is x**3 * log(abs((x*x-1)*(x*x-2)))\n", Preferences.DEBUG_ALGORITHM);
        Preferences.debug("Integrand lower endpoint = 0.0\n", Preferences.DEBUG_ALGORITHM);
        Preferences.debug("Integrand upper endpoint = 3.0\n", Preferences.DEBUG_ALGORITHM);
        Preferences.debug("Numerical Integral = " + result[0] + " after " + neval[0] + " integrand evaluations used\n", 
        		Preferences.DEBUG_ALGORITHM);
        Preferences.debug("Error status = " + ier[0] + " with estimated absolute error = " + abserr[0] + "\n", 
        		Preferences.DEBUG_ALGORITHM);
        Preferences.debug("Actual answer = " + actualAnswer + "\n", Preferences.DEBUG_ALGORITHM);
        Preferences.debug("Exact error = " + (result[0] - actualAnswer) + "\n", Preferences.DEBUG_ALGORITHM);

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

        if (upper <= lower) {
            MipavUtil.displayError("upper must be > than lower");
            ier[0] = 6;

            return;
        }

        if ( (epsabs <= 0.0) && (epsrel < tol)) {
            MipavUtil.displayError("Must set epsabs > 0.0 or must set epsrel >= " + tol);
            ier[0] = 6;

            return;
        }

        if (limit < 1) {
            MipavUtil.displayError("limit must be >= 1");
            ier[0] = 6;

            return;
        }

        driver();

        Preferences.debug("Test 4 testing dqagse\n", Preferences.DEBUG_ALGORITHM);
        Preferences.debug("Integrand is log(x)/sqrt(x)\n", Preferences.DEBUG_ALGORITHM);
        Preferences.debug("Integrand lower endpoint = 0.0\n", Preferences.DEBUG_ALGORITHM);
        Preferences.debug("Integrand upper endpoint = 1.0\n", Preferences.DEBUG_ALGORITHM);
        Preferences.debug("Numerical Integral = " + result[0] + " after " + neval[0] + " integrand evaluations used\n", 
        		Preferences.DEBUG_ALGORITHM);
        Preferences.debug("Error status = " + ier[0] + " with estimated absolute error = " + abserr[0] + "\n", 
        		Preferences.DEBUG_ALGORITHM);
        Preferences.debug("Actual answer = " + actualAnswer + "\n", Preferences.DEBUG_ALGORITHM);
        Preferences.debug("Exact error = " + (result[0] - actualAnswer) + "\n", Preferences.DEBUG_ALGORITHM);

        testCase = 5;
        // Test5 tests dqawce
        // dqawce is an adaptive integrator for finding the Cauchy
        // principal value of the integral of f(x)*w(x) over (lower,upper)
        // where w(x) = 1/(x-c), c between lower and upper.
        // Integrate 1/(x*(5*x*x*x+6)) from -1 to 5.
        // The exact answer is log(125/631)/18 = -0.0899401
        // Numerical Integral = -0.08994400695837006 after 215 integrand evaluations used
        // Error status = 0 with estimated absolute error = 1.1852900958199885E-6
        // Actual answer = -0.08994400695771734
        // Exact error = -6.527139939649373E-13
        lower = -1.0;
        upper = 5.0;
        c = 0.0;
        routine = Integration2.DQAWCE;
        epsabs = 0.0;
        epsrel = 1.0E-3;
        limit = 100;
        actualAnswer = Math.log(125.0 / 631.0) / 18.0;

        if (upper <= lower) {
            MipavUtil.displayError("upper must be > than lower");
            ier[0] = 6;

            return;
        }

        if ( (c == lower) || (c == upper)) {
            MipavUtil.displayError("Cannot have c == lower or c == upper");
            ier[0] = 6;

            return;
        }

        if ( (epsabs <= 0.0) && (epsrel < tol)) {
            MipavUtil.displayError("Must set epsabs > 0.0 or must set epsrel >= " + tol);
            ier[0] = 6;

            return;
        }

        if (limit < 1) {
            MipavUtil.displayError("limit must be >= 1");
            ier[0] = 6;

            return;
        }

        driver();

        Preferences.debug("Test 5 testing dqawce\n", Preferences.DEBUG_ALGORITHM);
        Preferences.debug("Integrand is 1/(x*(5*x*x*x+6))\n", Preferences.DEBUG_ALGORITHM);
        Preferences.debug("Integrand lower endpoint = -1.0\n", Preferences.DEBUG_ALGORITHM);
        Preferences.debug("Integrand upper endpoint = 5.0\n", Preferences.DEBUG_ALGORITHM);
        Preferences.debug("Point of singularity c = 0.0\n", Preferences.DEBUG_ALGORITHM);
        Preferences.debug("Numerical Integral = " + result[0] + " after " + neval[0] + " integrand evaluations used\n", 
        		Preferences.DEBUG_ALGORITHM);
        Preferences.debug("Error status = " + ier[0] + " with estimated absolute error = " + abserr[0] + "\n", 
        		Preferences.DEBUG_ALGORITHM);
        Preferences.debug("Actual answer = " + actualAnswer + "\n", Preferences.DEBUG_ALGORITHM);
        Preferences.debug("Exact error = " + (result[0] - actualAnswer) + "\n", Preferences.DEBUG_ALGORITHM);

        testCase = 6;
        // Test 6 tests dqawfe
        // dqawfe handles fourier integration of f(x)*w(x) from
        // lower to infinity, with w(x) = cos(omega*x) or sine(omega*x)
        // Integrate cos(pi*x/2)/sqrt(x) from 0 to infinity.
        // The exact answer is 1.0
        // Numerical Integral = 0.9999969531107412 after 380 integrand evaluations used
        // Error status = 0 with estimated absolute error = 5.923418719196993E-4
        // Actual answer = 1.0
        // Exact error = -3.046889258784269E-6

        lower = 0.0;
        routine = Integration2.DQAWFE;
        epsabs = 1.0E-3;
        // integr = 1 for cosine, = 2 for sine
        integr = 1;
        limlst = 100;
        limit = 100;
        maxp1 = 100;
        omega = 0.5 * Math.PI;
        actualAnswer = 1.0;

        if ( (integr != 1) && (integr != 2)) {
            MipavUtil.displayError("integr must equal 1 or 2");
            ier[0] = 6;

            return;
        }

        if (epsabs <= 0.0) {
            MipavUtil.displayError("Must set epsabs > 0.0");
            ier[0] = 6;

            return;
        }

        if (limit < 1) {
            MipavUtil.displayError("limit must be at least 1");
            ier[0] = 6;

            return;
        }

        if (limlst < 3) {
            MipavUtil.displayError("limlst must be at least 3");
            ier[0] = 6;

            return;
        }

        if (maxp1 < 1) {
            MipavUtil.displayError("maxp1 must be at least 1");
            ier[0] = 6;

            return;
        }

        driver();

        Preferences.debug("Test 6 testing dqawfe\n", Preferences.DEBUG_ALGORITHM);
        Preferences.debug("Integrand is cos(pi*x/2)/sqrt(x)\n", Preferences.DEBUG_ALGORITHM);
        Preferences.debug("Integrand lower endpoint = 0.0\n", Preferences.DEBUG_ALGORITHM);
        Preferences.debug("Integrand upper endpoint = infinity\n", Preferences.DEBUG_ALGORITHM);
        Preferences.debug("Numerical Integral = " + result[0] + " after " + neval[0] + " integrand evaluations used\n", 
        		Preferences.DEBUG_ALGORITHM);
        Preferences.debug("Error status = " + ier[0] + " with estimated absolute error = " + abserr[0] + "\n", 
        		Preferences.DEBUG_ALGORITHM);
        Preferences.debug("Actual answer = " + actualAnswer + "\n", Preferences.DEBUG_ALGORITHM);
        Preferences.debug("Exact error = " + (result[0] - actualAnswer) + "\n", Preferences.DEBUG_ALGORITHM);

        testCase = 7;
        // Test7 tests dqawoe
        // dqawoe integrates functions of the form f(x)*sin(omega*x)
        // or f(x)*cos(omega*x)
        // Integrate log(x)*sin(10*pi*x) from 0 to 1
        // The exact answer is -(gamma+log(10*pi)-ci(10*pi))/(10*pi) = -0.1281316.
        // gamma is euler's constant
        // ci is the cosine integral ci(x) = integral(x to infinity) -cos(v)/v dv.
        // cin(10.0*pi) = 4.0255378
        // ci(x*pi) = gamma + ln(pi) + ln(x) - cin(x*pi)
        // Specify integr = 1 for cosine integral, 2 for sine integral.
        // ci(10.0*pi) = -0.0010071562550209023
        // Numerical Integral = -0.12813689323080435 after 215 integrand evaluations used
        // Error status = 0 with estimated absolute error = 7.321363049983965E-5
        // Actual answer = -0.1281368478946547
        // Exact error = -4.533614963997401E-8

        lower = 0.0;
        upper = 1.0;
        routine = Integration2.DQAWOE;
        epsabs = 0.0;
        epsrel = 1.0E-3;
        limit = 100;
        omega = 10.0 * Math.PI;
        integr = 2;
        icall = 1;
        maxp1 = 100;
        gamma = 0.57721566490153286061;
        final double cin = 4.0255378;
        double ci = gamma + Math.log(Math.PI) + Math.log(10.0) - cin;
        actualAnswer = - (gamma + Math.log(10.0 * Math.PI) - ci) / (10.0 * Math.PI);

        if (upper <= lower) {
            MipavUtil.displayError("upper must be > than lower");
            ier[0] = 6;

            return;
        }

        if ( (epsabs <= 0.0) && (epsrel < tol)) {
            MipavUtil.displayError("Must set epsabs > 0.0 or must set epsrel >= " + tol);
            ier[0] = 6;

            return;
        }

        if (limit < 1) {
            MipavUtil.displayError("limit must be >= 1");
            ier[0] = 6;

            return;
        }

        if ( (integr != 1) && (integr != 2)) {
            MipavUtil.displayError("integr must equal 1 or 2");
            ier[0] = 6;

            return;
        }

        if (icall < 1) {
            MipavUtil.displayError("icall must be at least 1");
            ier[0] = 6;

            return;
        }

        if (maxp1 < 1) {
            MipavUtil.displayError("maxp1 must be at least 1");
            ier[0] = 6;

            return;
        }

        driver();

        Preferences.debug("Test 7 testing dqawoe\n", Preferences.DEBUG_ALGORITHM);
        Preferences.debug("Integrand is log(x)*sin(10*pi*x)\n", Preferences.DEBUG_ALGORITHM);
        Preferences.debug("Integrand lower endpoint = 0.0\n", Preferences.DEBUG_ALGORITHM);
        Preferences.debug("Integrand upper endpoint = 1.0\n", Preferences.DEBUG_ALGORITHM);
        Preferences.debug("ci(10.0*pi) = " + ci + "\n", Preferences.DEBUG_ALGORITHM);
        Preferences.debug("Numerical Integral = " + result[0] + " after " + neval[0] + " integrand evaluations used\n", 
        		Preferences.DEBUG_ALGORITHM);
        Preferences.debug("Error status = " + ier[0] + " with estimated absolute error = " + abserr[0] + "\n", 
        		Preferences.DEBUG_ALGORITHM);
        Preferences.debug("Actual answer = " + actualAnswer + "\n", Preferences.DEBUG_ALGORITHM);
        Preferences.debug("Exact error = " + (result[0] - actualAnswer) + "\n", Preferences.DEBUG_ALGORITHM);

        testCase = 8;
        // test8 tests dqawse
        // dqawse is an adaptive integrator for integrands wiht
        // algebraic or logarithmic singularities at the endpoints.
        // ci is the cosine integral ci(x) = integral (x to infinity) -cos(v)/v dv
        // Si is the sine integral Si(x) = integral (0 to x) sin(v)/v dv
        // Actual answer = 0.5*(ci(1.0)*sin(1.0) - si(1.0)*cos(1.0)) - 0.5
        // Actual answer = 0.5*(ci(1.0)*sin(1.0) - (Si(1.0) - pi/2.0)*cos(1.0)) - 0.5
        // Numerical Integral = -0.18927363338348974 after 40 integrand evaluations used
        // Error status = 0 with estimated absolute error = 1.1122097989861538E-6
        // Actual answer = -0.18927518789136621
        // Exact error = 1.5545078764778175E-6
        lower = 0.0;
        upper = 1.0;
        routine = Integration2.DQAWSE;
        epsabs = 0.0;
        epsrel = 1.0E-3;
        limit = 100;
        alfa = 0.0;
        beta = 0.0;
        // Describes the integrand weight function
        // integr = 2 means weight function is (x-lower)**alfa * (upper-x)**beta * log(x-lower)
        integr = 2;
        ci = 0.3374039229;
        final double Si = 0.9460830704;
        actualAnswer = 0.5 * (ci * Math.sin(1.0) - (Si - Math.PI / 2.0) * Math.cos(1.0)) - 0.5;

        if (upper <= lower) {
            MipavUtil.displayError("upper must be > than lower");
            ier[0] = 6;

            return;
        }

        if ( (epsabs <= 0.0) && (epsrel < tol)) {
            MipavUtil.displayError("Must set epsabs > 0.0 or must set epsrel >= " + tol);
            ier[0] = 6;

            return;
        }

        if (limit < 2) {
            MipavUtil.displayError("limit must be >= 2");
            ier[0] = 6;

            return;
        }

        if ( (integr != 1) && (integr != 2) && (integr != 3) && (integr != 4)) {
            MipavUtil.displayError("integr must equal 1, 2, 3, or 4");
            ier[0] = 6;

            return;
        }

        if (alfa <= -1.0) {
            MipavUtil.displayError("alfa must be > -1.0");
            ier[0] = 6;

            return;
        }

        if (beta <= -1.0) {
            MipavUtil.displayError("beta must be > -1.0");
            ier[0] = 6;

            return;
        }

        driver();

        Preferences.debug("Test 8 testing dqawse\n", Preferences.DEBUG_ALGORITHM);
        Preferences.debug("Integrand is log(x)/(1 + (log(x))**2)**2\n", Preferences.DEBUG_ALGORITHM);
        Preferences.debug("Integrand lower endpoint = 0.0\n", Preferences.DEBUG_ALGORITHM);
        Preferences.debug("Integrand upper endpoint = 1.0\n", Preferences.DEBUG_ALGORITHM);
        Preferences.debug("Numerical Integral = " + result[0] + " after " + neval[0] + " integrand evaluations used\n", 
        		Preferences.DEBUG_ALGORITHM);
        Preferences.debug("Error status = " + ier[0] + " with estimated absolute error = " + abserr[0] + "\n", 
        		Preferences.DEBUG_ALGORITHM);
        Preferences.debug("Actual answer = " + actualAnswer + "\n", Preferences.DEBUG_ALGORITHM);
        Preferences.debug("Exact error = " + (result[0] - actualAnswer) + "\n", Preferences.DEBUG_ALGORITHM);

        testCase = 9;
        // test9 tests dqk15
        // dqk15 is a Gauss-Kronod quadrature rule.
        lower = 0.0;
        upper = 1.0;
        routine = Integration2.DQK15;
        actualAnswer = -4.0 / 9.0;
        // Numerical Integral = -0.44453824758327637
        // Estimated absolute error = 0.20176778110470117
        // Actual answer = -0.4444444444444444
        // Exact error = -9.380313883194935E-5
        // resabs[0] = 0.44453824758327637
        // resasc[0] = 0.20176778110470117

        if (upper <= lower) {
            MipavUtil.displayError("upper must be > than lower");
            ier[0] = 6;

            return;
        }

        driver();

        Preferences.debug("Test 9 testing dqk15\n", Preferences.DEBUG_ALGORITHM);
        Preferences.debug("Integrand is sqrt(x) * log(x)\n", Preferences.DEBUG_ALGORITHM);
        Preferences.debug("Integrand lower endpoint = 0.0\n", Preferences.DEBUG_ALGORITHM);
        Preferences.debug("Integrand upper endpoint = 1.0\n", Preferences.DEBUG_ALGORITHM);
        Preferences.debug("Numerical Integral = " + result[0] + "\n", Preferences.DEBUG_ALGORITHM);
        Preferences.debug("Estimated absolute error = " + abserr[0] + "\n", Preferences.DEBUG_ALGORITHM);
        Preferences.debug("Actual answer = " + actualAnswer + "\n", Preferences.DEBUG_ALGORITHM);
        Preferences.debug("Exact error = " + (result[0] - actualAnswer) + "\n", Preferences.DEBUG_ALGORITHM);
        Preferences.debug("resabs[0] = " + resabs[0] + "\n", Preferences.DEBUG_ALGORITHM);
        Preferences.debug("resasc[0] = " + resasc[0] + "\n", Preferences.DEBUG_ALGORITHM);

        testCase = 10;
        // test10 tests dqk21
        // dqk21 is a Gauss-Kronod quadrature rule.
        lower = 0.0;
        upper = 1.0;
        routine = Integration2.DQK21;
        actualAnswer = -4.0 / 9.0;
        // Numerical Integral = -0.44448120174773076
        // Estimated absolute error = 0.06213734569239914
        // Actual answer = -0.4444444444444444
        // Exact error = -3.675730328633886E-5
        // resabs[0] = 0.44448120174773076
        // resasc[0] = 0.20102031799671913

        if (upper <= lower) {
            MipavUtil.displayError("upper must be > than lower");
            ier[0] = 6;

            return;
        }

        driver();

        Preferences.debug("Test 10 testing dqk21\n", Preferences.DEBUG_ALGORITHM);
        Preferences.debug("Integrand is sqrt(x) * log(x)\n", Preferences.DEBUG_ALGORITHM);
        Preferences.debug("Integrand lower endpoint = 0.0\n", Preferences.DEBUG_ALGORITHM);
        Preferences.debug("Integrand upper endpoint = 1.0\n", Preferences.DEBUG_ALGORITHM);
        Preferences.debug("Numerical Integral = " + result[0] + "\n", Preferences.DEBUG_ALGORITHM);
        Preferences.debug("Estimated absolute error = " + abserr[0] + "\n", Preferences.DEBUG_ALGORITHM);
        Preferences.debug("Actual answer = " + actualAnswer + "\n", Preferences.DEBUG_ALGORITHM);
        Preferences.debug("Exact error = " + (result[0] - actualAnswer) + "\n", Preferences.DEBUG_ALGORITHM);
        Preferences.debug("resabs[0] = " + resabs[0] + "\n", Preferences.DEBUG_ALGORITHM);
        Preferences.debug("resasc[0] = " + resasc[0] + "\n", Preferences.DEBUG_ALGORITHM);

        testCase = 11;
        // test11 tests dqk31
        // dqk31 is a Gauss-Kronod quadrature rule.
        lower = 0.0;
        upper = 1.0;
        routine = Integration2.DQK31;
        actualAnswer = -4.0 / 9.0;
        // Numerical Integral = -0.4444571142381011
        // Estimated absolute error = 0.013135187473747374
        // Actual answer = -0.4444444444444444
        // Exact error = -1.2669793656661099E-5
        // resabs[0] = 0.4444571142381011
        // resasc[0] = 0.20044662305437475

        if (upper <= lower) {
            MipavUtil.displayError("upper must be > than lower");
            ier[0] = 6;

            return;
        }

        driver();

        Preferences.debug("Test 11 testing dqk31\n", Preferences.DEBUG_ALGORITHM);
        Preferences.debug("Integrand is sqrt(x) * log(x)\n", Preferences.DEBUG_ALGORITHM);
        Preferences.debug("Integrand lower endpoint = 0.0\n", Preferences.DEBUG_ALGORITHM);
        Preferences.debug("Integrand upper endpoint = 1.0\n", Preferences.DEBUG_ALGORITHM);
        Preferences.debug("Numerical Integral = " + result[0] + "\n", Preferences.DEBUG_ALGORITHM);
        Preferences.debug("Estimated absolute error = " + abserr[0] + "\n", Preferences.DEBUG_ALGORITHM);
        Preferences.debug("Actual answer = " + actualAnswer + "\n", Preferences.DEBUG_ALGORITHM);
        Preferences.debug("Exact error = " + (result[0] - actualAnswer) + "\n", Preferences.DEBUG_ALGORITHM);
        Preferences.debug("resabs[0] = " + resabs[0] + "\n", Preferences.DEBUG_ALGORITHM);
        Preferences.debug("resasc[0] = " + resasc[0] + "\n", Preferences.DEBUG_ALGORITHM);

        testCase = 12;
        // test12 tests dqk41
        // dqk41 is a Gauss-Kronod quadrature rule.
        lower = 0.0;
        upper = 1.0;
        routine = Integration2.DQK41;
        actualAnswer = -4.0 / 9.0;
        // Numerical Integral = -0.4444502553565426
        // Estimated absolute error = 0.004242965678842284
        // Actual answer = -0.4444444444444444
        // Exact error = -5.8109120981697515E-6
        // resabs[0] = 0.4444502553565426
        // resasc[0] = 0.20065010692415908

        if (upper <= lower) {
            MipavUtil.displayError("upper must be > than lower");
            ier[0] = 6;

            return;
        }

        driver();

        Preferences.debug("Test 12 testing dqk41\n", Preferences.DEBUG_ALGORITHM);
        Preferences.debug("Integrand is sqrt(x) * log(x)\n", Preferences.DEBUG_ALGORITHM);
        Preferences.debug("Integrand lower endpoint = 0.0\n", Preferences.DEBUG_ALGORITHM);
        Preferences.debug("Integrand upper endpoint = 1.0\n", Preferences.DEBUG_ALGORITHM);
        Preferences.debug("Numerical Integral = " + result[0] + "\n", Preferences.DEBUG_ALGORITHM);
        Preferences.debug("Estimated absolute error = " + abserr[0] + "\n", Preferences.DEBUG_ALGORITHM);
        Preferences.debug("Actual answer = " + actualAnswer + "\n", Preferences.DEBUG_ALGORITHM);
        Preferences.debug("Exact error = " + (result[0] - actualAnswer) + "\n", Preferences.DEBUG_ALGORITHM);
        Preferences.debug("resabs[0] = " + resabs[0] + "\n", Preferences.DEBUG_ALGORITHM);
        Preferences.debug("resasc[0] = " + resasc[0] + "\n", Preferences.DEBUG_ALGORITHM);

        testCase = 13;
        // test13 tests dqk51
        // dqk51 is a Gauss-Kronod quadrature rule.
        lower = 0.0;
        upper = 1.0;
        routine = Integration2.DQK51;
        actualAnswer = -4.0 / 9.0;
        // Numerical Integral = -0.4444476169318261
        // Estimated absolute error = 0.001742944355024998
        // Actual answer = -0.4444444444444444
        // Exact error = -3.1724873816862953E-6
        // resabs[0] = 0.4444476169318261
        // resasc[0] = 0.20079987986805534

        if (upper <= lower) {
            MipavUtil.displayError("upper must be > than lower");
            ier[0] = 6;

            return;
        }

        driver();

        Preferences.debug("Test 13 testing dqk51\n", Preferences.DEBUG_ALGORITHM);
        Preferences.debug("Integrand is sqrt(x) * log(x)\n", Preferences.DEBUG_ALGORITHM);
        Preferences.debug("Integrand lower endpoint = 0.0\n", Preferences.DEBUG_ALGORITHM);
        Preferences.debug("Integrand upper endpoint = 1.0\n", Preferences.DEBUG_ALGORITHM);
        Preferences.debug("Numerical Integral = " + result[0] + "\n", Preferences.DEBUG_ALGORITHM);
        Preferences.debug("Estimated absolute error = " + abserr[0] + "\n", Preferences.DEBUG_ALGORITHM);
        Preferences.debug("Actual answer = " + actualAnswer + "\n", Preferences.DEBUG_ALGORITHM);
        Preferences.debug("Exact error = " + (result[0] - actualAnswer) + "\n", Preferences.DEBUG_ALGORITHM);
        Preferences.debug("resabs[0] = " + resabs[0] + "\n", Preferences.DEBUG_ALGORITHM);
        Preferences.debug("resasc[0] = " + resasc[0] + "\n", Preferences.DEBUG_ALGORITHM);

        testCase = 14;
        // test14 tests dqk61
        // dqk61 is a Gauss-Kronod quadrature rule.
        lower = 0.0;
        upper = 1.0;
        routine = Integration2.DQK61;
        actualAnswer = -4.0 / 9.0;
        // Numerical Integral = -0.4444463651893372
        // Estimated absolute error = 8.376473933975456E-4
        // Actual answer = -0.4444444444444444
        // Exact error = -1.920744892802695E-6
        // resabs[0] = 0.4444463651893372
        // resasc[0] = 0.2006334575268779

        if (upper <= lower) {
            MipavUtil.displayError("upper must be > than lower");
            ier[0] = 6;

            return;
        }

        driver();

        Preferences.debug("Test 14 testing dqk61\n", Preferences.DEBUG_ALGORITHM);
        Preferences.debug("Integrand is sqrt(x) * log(x)\n", Preferences.DEBUG_ALGORITHM);
        Preferences.debug("Integrand lower endpoint = 0.0\n", Preferences.DEBUG_ALGORITHM);
        Preferences.debug("Integrand upper endpoint = 1.0\n", Preferences.DEBUG_ALGORITHM);
        Preferences.debug("Numerical Integral = " + result[0] + "\n", Preferences.DEBUG_ALGORITHM);
        Preferences.debug("Estimated absolute error = " + abserr[0] + "\n", Preferences.DEBUG_ALGORITHM);
        Preferences.debug("Actual answer = " + actualAnswer + "\n", Preferences.DEBUG_ALGORITHM);
        Preferences.debug("Exact error = " + (result[0] - actualAnswer) + "\n", Preferences.DEBUG_ALGORITHM);
        Preferences.debug("resabs[0] = " + resabs[0] + "\n", Preferences.DEBUG_ALGORITHM);
        Preferences.debug("resasc[0] = " + resasc[0] + "\n", Preferences.DEBUG_ALGORITHM);

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
        actualAnswer = -4.0 / 9.0;

        if (upper <= lower) {
            MipavUtil.displayError("upper must be > than lower");
            ier[0] = 6;

            return;
        }

        if ( (epsabs <= 0.0) && (epsrel < tol)) {
            MipavUtil.displayError("Must set epsabs > 0.0 or must set epsrel >= " + tol);
            ier[0] = 6;

            return;
        }

        driver();

        Preferences.debug("Test 15 testing dqng\n", Preferences.DEBUG_ALGORITHM);
        Preferences.debug("Integrand is sqrt(x) * log(x)\n", Preferences.DEBUG_ALGORITHM);
        Preferences.debug("Integrand lower endpoint = 0.0\n", Preferences.DEBUG_ALGORITHM);
        Preferences.debug("Integrand upper endpoint = 1.0\n", Preferences.DEBUG_ALGORITHM);
        Preferences.debug("Numerical Integral = " + result[0] + " after " + neval[0] + " integrand evaluations used\n", 
        		Preferences.DEBUG_ALGORITHM);
        Preferences.debug("Error status = " + ier[0] + " with estimated absolute error = " + abserr[0] + "\n", 
        		Preferences.DEBUG_ALGORITHM);
        Preferences.debug("Actual answer = " + actualAnswer + "\n", Preferences.DEBUG_ALGORITHM);
        Preferences.debug("Exact error = " + (result[0] - actualAnswer) + "\n", Preferences.DEBUG_ALGORITHM);

        testCase = 101;
        lower = 0.0;
        upper = 1.0;
        routine = Integration2.DQAGPE;
        breakPoints = new double[4];
        breakPoints[0] = 1.0 / 7.0;
        breakPoints[1] = 2.0 / 3.0;
        epsabs = 0.0;
        epsrel = 1.0E-3;
        limit = 100;

        if (upper <= lower) {
            MipavUtil.displayError("upper must be > than lower");
            ier[0] = 6;

            return;
        }

        npts2 = breakPoints.length;
        npts = npts2 - 2;

        if (npts2 < 2) {
            MipavUtil.displayError("breakPoints array length must be >= 2");
            ier[0] = 6;

            return;
        }

        for (int i = 0; i < (npts2 - 2); i++) {

            if (breakPoints[i] < lower) {
                MipavUtil.displayError("Break point " + breakPoints[i] + " is impossibly < lower");
                ier[0] = 6;

                return;
            }

            if (breakPoints[i] > upper) {
                MipavUtil.displayError("Break point " + breakPoints[i] + " is impossibly > upper");
                ier[0] = 6;

                return;
            }
        } // for (int i = 0; i < npts2 - 2; i++)

        if ( (epsabs <= 0.0) && (epsrel < tol)) {
            MipavUtil.displayError("Must set epsabs > 0.0 or must set epsrel >= " + tol);
            ier[0] = 6;

            return;
        }

        if (limit <= npts) {
            MipavUtil.displayError("Must set limit = " + limit + " > " + npts);
            ier[0] = 6;

            return;
        }

        driver();

        Preferences.debug("Test 101\n", Preferences.DEBUG_ALGORITHM);
        Preferences.debug("Numerical Integral = " + result[0] + " after " + neval[0] + " integrand evaluations used\n", 
        		Preferences.DEBUG_ALGORITHM);
        Preferences.debug("Error status = " + ier[0] + " with absolute error = " + abserr[0] + "\n",
        		Preferences.DEBUG_ALGORITHM);

        testCase = 102;
        bound = 0.0;
        routine = Integration2.DQAGIE;
        inf = 1;
        epsabs = 0.0;
        epsrel = 1.0E-3;
        limit = 100;

        if ( (inf != 1) && (inf != -1) && (inf != 2)) {
            MipavUtil.displayError("inf must equal -1, 1, or 2");
            ier[0] = 6;

            return;
        }

        if ( (epsabs <= 0.0) && (epsrel < tol)) {
            MipavUtil.displayError("Must set epsabs > 0.0 or must set epsrel >= " + tol);
            ier[0] = 6;

            return;
        }

        if (limit < 1) {
            MipavUtil.displayError("limit must be >= 1");
            ier[0] = 6;

            return;
        }

        driver();

        Preferences.debug("Test 102\n", Preferences.DEBUG_ALGORITHM);
        Preferences.debug("Numerical Integral = " + result[0] + " after " + neval[0] + " integrand evaluations used\n", 
        		Preferences.DEBUG_ALGORITHM);
        Preferences.debug("Error status = " + ier[0] + " with absolute error = " + abserr[0] + "\n", 
        		Preferences.DEBUG_ALGORITHM);
    }

    /**
     * Constructor for Integration Used with routine = DQNG.
     * 
     * @param lower DOCUMENT ME!
     * @param upper DOCUMENT ME!
     * @param routine DOCUMENT ME!
     * @param epsabs DOCUMENT ME!
     * @param epsrel DOCUMENT ME!
     */
    public Integration2(final double lower, final double upper, final int routine, final double epsabs,
            final double epsrel) {
        double neweps;
        double tol;

        this.lower = lower;
        this.upper = upper;
        this.routine = routine;

        if (routine != Integration2.DQNG) {
            MipavUtil.displayError("routine must be DQAGSE with this constructor");

            return;
        }

        if (upper <= lower) {
            MipavUtil.displayError("upper must be > than lower");
            ier[0] = 6;

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

        if ( (epsabs <= 0.0) && (epsrel < tol)) {
            MipavUtil.displayError("Must set epsabs > 0.0 or must set epsrel >= " + tol);
            ier[0] = 6;

            return;
        }
    }

    /**
     * Constructor used with routine = DQAGIE.
     * 
     * @param bound double
     * @param routine int
     * @param inf int
     * @param epsabs double
     * @param epsrel double
     * @param limit int
     */
    public Integration2(final double bound, final int routine, final int inf, final double epsabs, final double epsrel,
            final int limit) {
        double neweps;
        double tol;
        this.bound = bound;
        this.routine = routine;

        if (routine != Integration2.DQAGIE) {
            MipavUtil.displayError("routine must be DQAGIE with this constructor");

            return;
        }

        this.inf = inf;

        if ( (inf != 1) && (inf != -1) && (inf != 2)) {
            MipavUtil.displayError("inf must equal -1, 1, or 2");
            ier[0] = 6;

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

        if ( (epsabs <= 0.0) && (epsrel < tol)) {
            MipavUtil.displayError("Must set epsabs > 0.0 or must set epsrel >= " + tol);
            ier[0] = 6;

            return;
        }

        this.limit = limit;

        if (limit < 1) {
            MipavUtil.displayError("limit must be >= 1");
            ier[0] = 6;

            return;
        }

    }

    /**
     * Constructor for Integration Used with routine = DQAGSE.
     * 
     * @param lower DOCUMENT ME!
     * @param upper DOCUMENT ME!
     * @param routine DOCUMENT ME!
     * @param epsabs DOCUMENT ME!
     * @param epsrel DOCUMENT ME!
     * @param limit DOCUMENT ME!
     */
    public Integration2(final double lower, final double upper, final int routine, final double epsabs,
            final double epsrel, final int limit) {
        double neweps;
        double tol;

        this.lower = lower;
        this.upper = upper;
        this.routine = routine;

        if (routine != Integration2.DQAGSE) {
            MipavUtil.displayError("routine must be DQAGSE with this constructor");

            return;
        }

        if (upper <= lower) {
            MipavUtil.displayError("upper must be > than lower");
            ier[0] = 6;

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

        if ( (epsabs <= 0.0) && (epsrel < tol)) {
            MipavUtil.displayError("Must set epsabs > 0.0 or must set epsrel >= " + tol);
            ier[0] = 6;

            return;
        }

        this.limit = limit;

        if (limit < 1) {
            MipavUtil.displayError("limit must be >= 1");
            ier[0] = 6;

            return;
        }
    }

    /**
     * Constructor for Integration2 Used with routine = DQAWCE.
     * 
     * @param lower DOCUMENT ME!
     * @param upper DOCUMENT ME!
     * @param c
     * @param routine DOCUMENT ME!
     * @param epsabs DOCUMENT ME!
     * @param epsrel DOCUMENT ME!
     * @param limit DOCUMENT ME!
     */
    public Integration2(final double lower, final double upper, final double c, final int routine, final double epsabs,
            final double epsrel, final int limit) {
        double neweps;
        double tol;

        this.lower = lower;
        this.upper = upper;
        this.c = c;
        this.routine = routine;

        if (routine != Integration2.DQAWCE) {
            MipavUtil.displayError("routine must be DQAWCE with this constructor");

            return;
        }

        if (upper <= lower) {
            MipavUtil.displayError("upper must be > than lower");
            ier[0] = 6;

            return;
        }

        if ( (c == lower) || (c == upper)) {
            MipavUtil.displayError("Cannot have c == lower or c == upper");
            ier[0] = 6;

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

        if ( (epsabs <= 0.0) && (epsrel < tol)) {
            MipavUtil.displayError("Must set epsabs > 0.0 or must set epsrel >= " + tol);
            ier[0] = 6;

            return;
        }

        this.limit = limit;

        if (limit < 1) {
            MipavUtil.displayError("limit must be >= 1");
            ier[0] = 6;

            return;
        }
    }

    /**
     * Constructor must be used with routine dqawfe
     * 
     * @param lower
     * @param routine
     * @param omega
     * @param integr
     * @param epsabs
     * @param limlst
     * @param limit
     * @param maxp1
     */
    public Integration2(final double lower, final int routine, final double omega, final int integr,
            final double epsabs, final int limlst, final int limit, final int maxp1) {

        this.lower = lower;
        this.routine = routine;

        if (routine != Integration2.DQAWFE) {
            MipavUtil.displayError("routine must be DQAWFE with this constructor");

            return;
        }

        this.omega = omega;

        this.integr = integr;
        if ( (integr != 1) && (integr != 2)) {
            MipavUtil.displayError("integr must equal 1 or 2");
            ier[0] = 6;

            return;
        }

        this.epsabs = epsabs;

        if (epsabs <= 0.0) {
            MipavUtil.displayError("Must set epsabs > 0.0");
            ier[0] = 6;

            return;
        }

        this.limit = limit;

        if (limit < 1) {
            MipavUtil.displayError("limit must be at least 1");
            ier[0] = 6;

            return;
        }

        this.limlst = limlst;
        if (limlst < 3) {
            MipavUtil.displayError("limlst must be at least 3");
            ier[0] = 6;

            return;
        }

        this.maxp1 = maxp1;

        if (maxp1 < 1) {
            MipavUtil.displayError("maxp1 must be at least 1");
            ier[0] = 6;

            return;
        }
    }

    /**
     * Constructor must be used with routine dqawoe
     * 
     * @param lower
     * @param upper
     * @param routine
     * @param omega
     * @param integr
     * @param epsabs
     * @param epsrel
     * @param limit
     * @param icall
     * @param maxp1
     */
    public Integration2(final double lower, final double upper, final int routine, final double omega,
            final int integr, final double epsabs, final double epsrel, final int limit, final int icall,
            final int maxp1) {
        double neweps;
        double tol;

        this.lower = lower;
        this.upper = upper;
        this.routine = routine;

        if (routine != Integration2.DQAWOE) {
            MipavUtil.displayError("routine must be DQAWOE with this constructor");

            return;
        }

        if (upper <= lower) {
            MipavUtil.displayError("upper must be > than lower");
            ier[0] = 6;

            return;
        }

        this.omega = omega;

        this.integr = integr;
        if ( (integr != 1) && (integr != 2)) {
            MipavUtil.displayError("integr must equal 1 or 2");
            ier[0] = 6;

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

        if ( (epsabs <= 0.0) && (epsrel < tol)) {
            MipavUtil.displayError("Must set epsabs > 0.0 or must set epsrel >= " + tol);
            ier[0] = 6;

            return;
        }

        this.limit = limit;

        if (limit < 1) {
            MipavUtil.displayError("limit must be at least 1");
            ier[0] = 6;

            return;
        }

        this.icall = icall;

        if (icall < 1) {
            MipavUtil.displayError("icall must be at least 1");
            ier[0] = 6;

            return;
        }

        this.maxp1 = maxp1;

        if (maxp1 < 1) {
            MipavUtil.displayError("maxp1 must be at least 1");
            ier[0] = 6;

            return;
        }
    }

    /**
     * This constructor must be used with routine DQAWSE.
     * 
     * @param lower
     * @param upper
     * @param routine
     * @param alfa
     * @param beta
     * @param integr
     * @param epsabs
     * @param epsrel
     * @param limit
     */
    public Integration2(final double lower, final double upper, final int routine, final double alfa,
            final double beta, final int integr, final double epsabs, final double epsrel, final int limit) {
        double neweps;
        double tol;

        this.lower = lower;
        this.upper = upper;
        this.routine = routine;

        if (routine != Integration2.DQAWSE) {
            MipavUtil.displayError("routine must be DQAWSE with this constructor");

            return;
        }

        if (upper <= lower) {
            MipavUtil.displayError("upper must be > than lower");
            ier[0] = 6;

            return;
        }

        this.alfa = alfa;
        if (alfa <= -1.0) {
            MipavUtil.displayError("alfa must be > -1.0");
            ier[0] = 6;

            return;
        }

        this.beta = beta;
        if (beta <= -1.0) {
            MipavUtil.displayError("beta must be > -1.0");
            ier[0] = 6;

            return;
        }

        this.integr = integr;
        if ( (integr != 1) && (integr != 2) && (integr != 3) && (integr != 4)) {
            MipavUtil.displayError("integr must equal 1, 2, 3, or 4");
            ier[0] = 6;

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

        if ( (epsabs <= 0.0) && (epsrel < tol)) {
            MipavUtil.displayError("Must set epsabs > 0.0 or must set epsrel >= " + tol);
            ier[0] = 6;

            return;
        }

        this.limit = limit;

        if (limit < 2) {
            MipavUtil.displayError("Must set limit >= 2");
            ier[0] = 6;

            return;
        }

    }

    /**
     * Constructor for Integration Used with routine = DQAGPE.
     * 
     * @param lower DOCUMENT ME!
     * @param upper DOCUMENT ME!
     * @param routine DOCUMENT ME!
     * @param breakPoints DOCUMENT ME!
     * @param epsabs DOCUMENT ME!
     * @param epsrel DOCUMENT ME!
     * @param limit DOCUMENT ME!
     */
    public Integration2(final double lower, final double upper, final int routine, final double[] breakPoints,
            final double epsabs, final double epsrel, final int limit) {
        double neweps;
        double tol;

        this.lower = lower;
        this.upper = upper;
        this.routine = routine;

        if (routine != Integration2.DQAGPE) {
            MipavUtil.displayError("routine must be DQAGPE with this constructor");

            return;
        }

        if (upper <= lower) {
            MipavUtil.displayError("upper must be > than lower");
            ier[0] = 6;

            return;
        }

        this.breakPoints = breakPoints;
        npts2 = breakPoints.length;
        npts = npts2 - 2;

        if (npts2 < 2) {
            MipavUtil.displayError("breakPoints array length must be >= 2");
            ier[0] = 6;

            return;
        }

        for (int i = 0; i < (npts2 - 2); i++) {

            if (breakPoints[i] < lower) {
                MipavUtil.displayError("Break point " + breakPoints[i] + " is impossibly < lower");
                ier[0] = 6;

                return;
            }

            if (breakPoints[i] > upper) {
                MipavUtil.displayError("Break point " + breakPoints[i] + " is impossibly > upper");
                ier[0] = 6;

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

        if ( (epsabs <= 0.0) && (epsrel < tol)) {
            MipavUtil.displayError("Must set epsabs > 0.0 or must set epsrel >= " + tol);
            ier[0] = 6;

            return;
        }

        this.limit = limit;

        if (limit <= npts) {
            MipavUtil.displayError("Must set limit = " + limit + " > " + npts);
            ier[0] = 6;

            return;
        }
    }

    /**
     * Constructor for Integration Used with routine = DQAGE.
     * 
     * @param lower DOCUMENT ME!
     * @param upper DOCUMENT ME!
     * @param routine DOCUMENT ME!
     * @param key DOCUMENT ME!
     * @param epsabs DOCUMENT ME!
     * @param epsrel DOCUMENT ME!
     * @param limit DOCUMENT ME!
     */
    public Integration2(final double lower, final double upper, final int routine, final int key, final double epsabs,
            final double epsrel, final int limit) {
        double neweps;
        double tol;

        this.lower = lower;
        this.upper = upper;
        this.routine = routine;

        if (routine != Integration2.DQAGE) {
            MipavUtil.displayError("routine must be DQAGE with this constructor");

            return;
        }

        if (upper <= lower) {
            MipavUtil.displayError("upper must be > than lower");
            ier[0] = 6;

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

        if ( (epsabs <= 0.0) && (epsrel < tol)) {
            MipavUtil.displayError("Must set epsabs > 0.0 or must set epsrel >= " + tol);
            ier[0] = 6;

            return;
        }

        this.limit = limit;

        if (limit <= 1) {
            MipavUtil.displayError("limit must be >= 1");
            ier[0] = 6;

            return;
        }
    }

    // ~ Methods
    // --------------------------------------------------------------------------------------------------------

    /**
     * DOCUMENT ME!
     * 
     * @param x DOCUMENT ME!
     * 
     * @return DOCUMENT ME!
     */
    public abstract double intFunc(double x);

    private double intFuncTest(final double x) {
        double function = 0.0;
        double lg;
        double var;
        switch (testCase) {
            case 1:
                function = Math.cos(100.0 * Math.sin(x));
                break;
            case 2:
                function = Math.log(x) / (1.0 + 100.0 * x * x);
                break;
            case 3:
                function = x * x * x * Math.log(Math.abs( (x * x - 1.0) * (x * x - 2.0)));
                break;
            case 4:
                function = Math.log(x) / Math.sqrt(x);
                break;
            case 5:
                function = 1.0 / (5.0 * x * x * x + 6.0);
                break;
            case 6:
                if (x <= 0.0) {
                    function = 0.0;
                } else {
                    function = 1.0 / Math.sqrt(x);
                }
                break;
            case 7:
                if (x <= 0.0) {
                    function = 0.0;
                } else {
                    function = Math.log(x);
                }
                break;
            case 8:
                if (x <= 0.0) {
                    function = 0.0;
                } else {
                    lg = Math.log(x);
                    var = 1.0 + lg * lg;
                    function = 1.0 / (var * var);
                }
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
                } else {
                    function = Math.sqrt(x) * Math.log(x);
                }
                break;
            case 101:
                if ( (x != 1.0 / 7.0) && (x != 2.0 / 3.0)) {
                    function = Math.pow(Math.abs(x - 1.0 / 7.0), -0.25) * Math.pow(Math.abs(x - 2.0 / 3.0), -0.55);
                }
                break;
            case 102:
                if (x > 0.0) {
                    function = Math.sqrt(x) * Math.log(x) / ( (x + 1.0) * (x + 2.0));
                }
                break;
        }
        return function;
    }

    /**
     * This is a port of the FORTRAN routine whose header is given below: c c dgtsl given a general tridiagonal matrix
     * and a right hand c side will find the solution. c c on entry c c n integer c is the order of the tridiagonal
     * matrix. c c c double precision(n) c is the subdiagonal of the tridiagonal matrix. c c(2) through c(n) should
     * contain the subdiagonal. c on output c is destroyed. c c d double precision(n) c is the diagonal of the
     * tridiagonal matrix. c on output d is destroyed. c c e double precision(n) c is the superdiagonal of the
     * tridiagonal matrix. c e(1) through e(n-1) should contain the superdiagonal. c on output e is destroyed. c c b
     * double precision(n) c is the right hand side vector. c c on return c c b is the solution vector. c c info integer
     * c = 0 normal value. c = k if the k-th element of the diagonal becomes c exactly zero. the subroutine returns when
     * c this is detected. c c linpack. this version dated 08/14/78 . c jack dongarra, argonne national laboratory.
     * 
     * @param n
     * @param c
     * @param d
     * @param e
     * @param b
     */
    private void dgtsl(final int n, final double c[], final double d[], final double e[], final double b[],
            final int info[]) {
        int k;
        int kb;
        int kp1;
        int nm1;
        int nm2;
        double t;

        info[0] = 0;
        c[0] = d[0];
        nm1 = n - 1;
        if (nm1 >= 1) {
            d[0] = e[0];
            e[0] = 0.0;
            e[n - 1] = 0.0;
            for (k = 0; k < nm1; k++) {
                kp1 = k + 1;

                // Find the largest of the two rows

                if (Math.abs(c[kp1]) >= Math.abs(c[k])) {

                    // Interchange row

                    t = c[kp1];
                    c[kp1] = c[k];
                    c[k] = t;
                    t = d[kp1];
                    d[kp1] = d[k];
                    d[k] = t;
                    t = e[kp1];
                    e[kp1] = e[k];
                    e[k] = t;
                    t = b[kp1];
                    b[kp1] = b[k];
                    b[k] = t;
                } // if (Math.abs(c[kp1]) >= Math.abs(c[k]))

                // Zero elements

                if (c[k] == 0.0) {
                    info[0] = k + 1;
                    return;
                } // if (c[k] == 0.0)

                t = -c[kp1] / c[k];
                c[kp1] = d[kp1] + t * d[k];
                d[kp1] = e[kp1] + t * e[k];
                e[kp1] = 0.0;
                b[kp1] = b[kp1] + t * b[k];
            } // for (k = 0; k < nm1; k++)
        } // if (nm1 >= 1)
        if (c[n - 1] == 0.0) {
            info[0] = n;
            return;
        } // if (c[n-1] == 0.0)

        // Back solve

        nm2 = n - 2;
        b[n - 1] = b[n - 1] / c[n - 1];
        if (n == 1) {
            return;
        }
        b[nm1 - 1] = (b[nm1 - 1] - d[nm1 - 1] * b[n - 1]) / c[nm1 - 1];
        if (nm2 < 1) {
            return;
        }
        for (kb = 1; kb <= nm2; kb++) {
            k = nm2 - kb + 1;
            b[k - 1] = (b[k - 1] - d[k - 1] * b[k] - e[k - 1] * b[k + 1]) / c[k - 1];
        } // for (kb = 1; kb <= nm2; kb++)
        return;
    } // dgtsl

    /**
     * This is a port of the orginal FORTRAN code whose header is given below: c***begin prologue dqagie c***date
     * written 800101 (yymmdd) c***revision date 830518 (yymmdd) c***category no. h2a3a1,h2a4a1 c***keywords automatic
     * integrator, infinite intervals, c general-purpose, transformation, extrapolation, c globally adaptive c***author
     * piessens,robert,appl. math & progr. div - k.u.leuven c de doncker,elise,appl. math & progr. div - k.u.leuven
     * c***purpose the routine calculates an approximation result to a given c integral i = integral of f over
     * (bound,+infinity) c or i = integral of f over (-infinity,bound) c or i = integral of f over
     * (-infinity,+infinity), c hopefully satisfying following claim for accuracy c
     * abs(i-result).le.max(epsabs,epsrel*abs(i)) c***description c c integration over infinite intervals c standard
     * fortran subroutine
     * 
     */
    public void dqagie(final double bound, final int inf, final double epsrel) {

        // Variables ending in 1 denote left subinterval and variables
        // ending in 2 denote right subinterval
        // rlist2 should be length at least (limexp + 2)
        // rlist2 contains the part of the epsilon table still needed for
        // further computations
        double[] rlist2;

        // Index to the interval with the largest error estimate
        final int[] maxErr = new int[1];

        // errMax[0] = elist[maxErr];
        final double[] errMax = new double[1];

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
        final int[] nres = new int[1];

        // Number of elements in rlist2. If an appropriate approximation
        // to the compounded integral has been obtained, it is put in
        // rlist2[numr12-1] after numr12 has been increased by one.
        final int[] numr12 = new int[1];

        // Sum of the errors over the intervals larger than the smallest
        // interval considered up to now
        double erlarg;

        // boolean variable denoting that the routine is attempting to
        // perform extrapolation. That is, before dividing the
        // interval we try to decrease the value of erlarg
        boolean extrap;

        // If noext is true, the extrapolation is no longer allowed.
        boolean noext;

        // length of the smallest interval considered up to now, multiplied
        // by 1.5
        double small;
        double[] res31a;
        final double[] abseps = new double[1];
        final double[] area1 = new double[1];
        double area12;
        final double[] area2 = new double[1];
        double a1;
        double a2;
        double boun;
        double b1;
        double b2;
        double correc;
        final double[] defabs = new double[1];
        final double[] defab1 = new double[1];
        final double[] defab2 = new double[1];
        double dres;
        final double[] error1 = new double[1];
        final double[] error2 = new double[1];
        double error12;
        double ertest;
        final double[] resabs = new double[1];
        final double[] reseps = new double[1];
        int id;
        int ierro;
        int iroff1;
        int iroff2;
        int iroff3;
        int jupbnd;
        int k;
        int ksgn;
        int ktmin;
        final int[] nrmax = new int[1];

        try {
            alist = new double[limit];
            blist = new double[limit];
            rlist = new double[limit];
            elist = new double[limit];
            iord = new int[limit];
            res31a = new double[3];
            rlist2 = new double[52];

            ier[0] = 0;
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

            if ( (abserr[0] <= (100.0 * epmach * defabs[0])) && (abserr[0] > errBnd)) {
                ier[0] = 2;
            } // if ((abserr[0] <= 100.0 * epmach * defabs[0]) &&

            if (limit == 1) {
                ier[0] = 1;
            } // if (limit == 1)

            if ( (ier[0] != 0) || ( (abserr[0] <= errBnd) && (abserr[0] != resabs[0])) || (abserr[0] == 0.0)) {
                neval[0] = (30 * last) - 15;

                if (inf == 2) {
                    neval[0] = 2 * neval[0];
                } // if (inf == 2)

                if (ier[0] > 2) {
                    ier[0] = ier[0] - 1;
                } // if (ier[0] > 2)

                return;
            } // if ((ier[0] != 0) ||

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

            if (dres >= ( (1.0 - (50.0 * epmach)) * defabs[0])) {
                ksgn = 1;
            } // if (dres >= (1.0 - 50.0 * epmach)*defabs[0])

            // main do-loop
            loop: for (last = 2; last <= limit; last++) {

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

                if ( (defab1[0] != error1[0]) && (defab2[0] != error2[0])) {

                    if ( (Math.abs(rlist[maxErr[0]] - area12) <= (1.0E-5 * Math.abs(area12)))
                            && (error12 >= (0.99 * errMax[0]))) {

                        if (extrap) {
                            iroff2 = iroff2 + 1;
                        } else {
                            iroff1 = iroff1 + 1;
                        }
                    } // if ((Math.abs(rlist[maxErr[0]] - area12) <=

                    if ( (last > 10) && (error12 > errMax[0])) {
                        iroff3 = iroff3 + 1;
                    } // if ((last > 10) && (error12 > errMax[0]))
                } // if ((defab1[0] != error1[0]) && (defab2[0] != error2[0]))

                rlist[maxErr[0]] = area1[0];
                rlist[last - 1] = area2[0];
                errBnd = Math.max(epsabs, epsrel * Math.abs(area));

                // Test for roundoff error and eventually set error flag

                if ( ( (iroff1 + iroff2) >= 10) || (iroff3 >= 20)) {
                    ier[0] = 2;
                } // if ((iroff1 + iroff2 >= 10) || (iroff3 >= 20))

                if (iroff2 >= 5) {
                    ierro = 3;
                } // if (iroff2 >= 5)

                // Set error flag in the case that the number of subintervals
                // equals limit

                if (last == limit) {
                    ier[0] = 1;
                } // if (last == limit)

                // Set error flag in the case of bad integrand behavior
                // at some points of the integration range
                if (Math.max(Math.abs(a1), Math.abs(b2)) <= ( (1.0 + (100.0 * epmach)) * (Math.abs(a2) + (1.0E3 * uflow)))) {
                    ier[0] = 4;
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

                    if (ier[0] > 2) {
                        ier[0] = ier[0] - 1;
                    } // if (ier[0] > 2)

                    return;
                } // if (errSum <= errBnd)

                if (ier[0] != 0) {
                    break;
                } // if (ier[0] != 0)

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

                if ( !extrap) {

                    // Test whether the interval to be bisected next is the
                    // smallest interval

                    if (Math.abs(blist[maxErr[0]] - alist[maxErr[0]]) > small) {
                        continue;
                    } // if (Math.abs(blist[maxErr[0]] - alist[maxErr[0]]) > small)

                    extrap = true;
                    nrmax[0] = 2;
                } // if (!extrap)

                if ( (ierro != 3) && (erlarg > ertest)) {

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

                if ( (ktmin > 5) && (abserr[0] < (1.0E-3 * errSum))) {
                    ier[0] = 5;
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

                if (ier[0] == 5) {
                    break;
                } // if (ier[0] == 5)

                maxErr[0] = iord[0];
                errMax[0] = elist[maxErr[0]];
                nrmax[0] = 1;
                extrap = false;
                small = 0.5 * small;
                erlarg = errSum;
            } // for (last = 2; last <= limit; last++)

            // Set final result and error estimate

            if (abserr[0] != oflow) {

                if ( (ier[0] + ierro) != 0) {

                    if (ierro == 3) {
                        abserr[0] = abserr[0] + correc;
                    } // if (ierro == 3)

                    if (ier[0] == 0) {
                        ier[0] = 3;
                    } // if (ier[0] == 0)

                    if ( (result[0] == 0.0) || (area == 0.0)) {

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

                            if (ier[0] > 2) {
                                ier[0] = ier[0] - 1;
                            } // if (ier[0] > 2)

                            return;
                        } // if (abserr[0] > errSum)

                        if (area == 0.0) {
                            neval[0] = (30 * last) - 15;

                            if (inf == 2) {
                                neval[0] = 2 * neval[0];
                            } // if (inf == 2)

                            if (ier[0] > 2) {
                                ier[0] = ier[0] - 1;
                            } // if (ier[0] > 2)

                            return;
                        } // if (area == 0.0)

                        if ( (ksgn == -1) && (Math.max(Math.abs(result[0]), Math.abs(area)) <= (1.0E-2 * defabs[0]))) {
                            neval[0] = (30 * last) - 15;

                            if (inf == 2) {
                                neval[0] = 2 * neval[0];
                            } // if (inf == 2)

                            if (ier[0] > 2) {
                                ier[0] = ier[0] - 1;
                            } // if (ier[0] > 2)

                            return;
                        } // if ((ksgn == -1) && (Math.max(Math.abs(result[0]),

                        if ( (1.0E-2 > (result[0] / area)) || ( (result[0] / area) > 100.0)
                                || (errSum > Math.abs(area))) {
                            ier[0] = 6;
                        } // if ((1.0E-2 > (result[0]/area)) || (result[0]/area > 100.0) ||

                        neval[0] = (30 * last) - 15;

                        if (inf == 2) {
                            neval[0] = 2 * neval[0];
                        } // if (inf == 2)

                        if (ier[0] > 2) {
                            ier[0] = ier[0] - 1;
                        } // if (ier[0] > 2)

                        return;
                    } // if (result[0] == 0.0) || (area == 0.0))

                    if ( (abserr[0] / Math.abs(result[0])) > (errSum / Math.abs(area))) {
                        result[0] = 0.0;

                        for (k = 0; k < last; k++) {
                            result[0] = result[0] + rlist[k];
                        } // for (k = 0; k < last; k++)

                        abserr[0] = errSum;
                        neval[0] = (30 * last) - 15;

                        if (inf == 2) {
                            neval[0] = 2 * neval[0];
                        } // if (inf == 2)

                        if (ier[0] > 2) {
                            ier[0] = ier[0] - 1;
                        } // if (ier[0] > 2)

                        return;
                    } // if (abserr[0]/Math.abs(result[0]) > errSum/Math.abs(area))
                } // if ((ier[0] + ierro) != 0)

                if ( (ksgn == -1) && (Math.max(Math.abs(result[0]), Math.abs(area)) <= (1.0E-2 * defabs[0]))) {
                    neval[0] = (30 * last) - 15;

                    if (inf == 2) {
                        neval[0] = 2 * neval[0];
                    } // if (inf == 2)

                    if (ier[0] > 2) {
                        ier[0] = ier[0] - 1;
                    } // if (ier[0] > 2)

                    return;
                } // if ((ksgn == -1) && (Math.max(Math.abs(result[0]),

                if ( (1.0E-2 > (result[0] / area)) || ( (result[0] / area) > 100.0) || (errSum > Math.abs(area))) {
                    ier[0] = 6;
                } // if ((1.0E-2 > (result[0]/area)) || (result[0]/area > 100.0) ||

                neval[0] = (30 * last) - 15;

                if (inf == 2) {
                    neval[0] = 2 * neval[0];
                } // if (inf == 2)

                if (ier[0] > 2) {
                    ier[0] = ier[0] - 1;
                } // if (ier[0] > 2)

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

            if (ier[0] > 2) {
                ier[0] = ier[0] - 1;
            } // if (ier[0] > 2)

            return;

        } // try
        catch (final Exception err) {
            Preferences.debug("dqagie error: " + err.getMessage(), Preferences.DEBUG_ALGORITHM);
        }

    } // dqagie

    /**
     * dqagpe. This is a port of the original FORTRAN code whose header is given below: c***begin prologue dqagie
     * c***date written 800101 (yymmdd) c***revision date 830518 (yymmdd) c***category no. h2a3a1,h2a4a1 c***keywords
     * automatic integrator, infinite intervals, c general-purpose, transformation, extrapolation, c globally adaptive
     * c***author piessens,robert,appl. math & progr. div - k.u.leuven c de doncker,elise,appl. math & progr. div -
     * k.u.leuven c***purpose the routine calculates an approximation result to a given c integral i = integral of f
     * over (bound,+infinity) c or i = integral of f over (-infinity,bound) c or i = integral of f over
     * (-infinity,+infinity), c hopefully satisfying following claim for accuracy c
     * abs(i-result).le.max(epsabs,epsrel*abs(i)) c***description c c integration over infinite intervals c standard
     * fortran subroutine
     * 
     */
    public void dqagpe() {
        double[] res31a;

        // rlist2 should be length at least (limexp + 2)
        // rlist2 contains the part of the epsilon table still needed for
        // further computations
        double[] rlist2;

        // Index to the interval with the largest error estimate
        final int[] maxErr = new int[1];

        // errMax[0] = elist[maxErr];
        final double[] errMax = new double[1];

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
        final int[] nres = new int[1];

        // Number of elements in rlist2. If an appropriate approximation
        // to the compounded integral has been obtained, it is put in
        // rlist2[numr12-1] after numr12 has been increased by one.
        final int[] numr12 = new int[1];

        // Sum of the errors over the intervals larger than the smallest
        // interval considered up to now
        double erlarg;

        // boolean variable denoting that the routine is attempting to
        // perform extrapolation. That is, before dividing the
        // interval we try to decrease the value of erlarg
        boolean extrap;

        // If noext is true, the extrapolation is no longer allowed.
        boolean noext;

        // Variables ending in 1 denote left subinterval and variables
        // ending in 2 denote right subinterval
        final double[] abseps = new double[1];
        final double[] area1 = new double[1];
        double area12;
        final double[] area2 = new double[1];
        double a1;
        double a2;
        double b1;
        double b2;
        double correc = 0.0;
        final double[] defabs = new double[1];
        final double[] defab1 = new double[1];
        final double[] defab2 = new double[1];
        double dres;
        final double[] error1 = new double[1];
        double error12;
        final double[] error2 = new double[1];
        double ertest;
        final double[] resa = new double[1];
        double resabs;
        final double[] reseps = new double[1];
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
        final int[] nrmax = new int[1];

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

            ier[0] = 0;
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

                if ( (error1[0] == resa[0]) && (error1[0] != 0.0)) {
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

            if ( (abserr[0] <= (100.0 * epmach * resabs)) && (abserr[0] > errBnd)) {
                ier[0] = 2;
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
                    ier[0] = 1;
                } // if (limit < npts2)
            } // if (nint != 1)

            if ( (ier[0] != 0) || (abserr[0] <= errBnd)) {

                if (ier[0] > 2) {
                    ier[0] = ier[0] - 1;
                } // if (ier[0] > 2)

                result[0] = result[0] * sign;

                return;
            } // if ((ier[0] != 0) || (abserr[0] <= errBnd))

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

            if (dres >= ( (1.0 - (50.0 * epmach)) * resabs)) {
                ksgn = 1;
            }

            // Main do-loop
            loop: for (last = npts2; last <= limit; last++) {

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

                if ( (defab1[0] != error1[0]) && (defab2[0] != error2[0])) {

                    if ( (Math.abs(rlist[maxErr[0]] - area12) <= (1.0E-5 * Math.abs(area12)))
                            && (error12 >= (0.99 * errMax[0]))) {

                        if (extrap) {
                            iroff2 = iroff2 + 1;
                        } else {
                            iroff1 = iroff1 + 1;
                        }
                    }

                    if ( (last > 10) && (error12 > errMax[0])) {
                        iroff3 = iroff3 + 1;
                    }
                } // if ((defab1[0] != error1[0]) && (defab2[0] != error2[0]))

                level[maxErr[0]] = levcur;
                level[last - 1] = levcur;
                rlist[maxErr[0]] = area1[0];
                rlist[last - 1] = area2[0];
                errBnd = Math.max(epsabs, epsrel * Math.abs(area));

                // Test for roundoff error and eventually set error flag

                if ( ( (iroff1 + iroff2) >= 10) || (iroff3 >= 20)) {
                    ier[0] = 2;
                }

                if (iroff2 >= 5) {
                    ierro = 3;
                }

                // Set error flag in the case that the number of
                // subintervals equals limit.

                if (last == limit) {
                    ier[0] = 1;
                }

                // Set error flag in the case of bad integrand behavior
                // at a point of the integration range

                if (Math.max(Math.abs(a1), Math.abs(b2)) <= ( (1.0 + (100.0 * epmach)) * (Math.abs(a2) + (1.0E3 * uflow)))) {
                    ier[0] = 4;
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

                    if (ier[0] > 2) {
                        ier[0] = ier[0] - 1;
                    }

                    result[0] = result[0] * sign;

                    return;
                } // if (errSum <= errBnd)

                if (ier[0] != 0) {
                    break;
                } // if (ier[0] != 0)

                if (noext) {
                    continue;
                } // if (noext)

                erlarg = erlarg - erlast;

                if ( (levcur + 1) <= levmax) {
                    erlarg = erlarg + error12;
                } // if ((levcur + 1) <= levmax)

                if ( !extrap) {

                    // Test whether the interval to be bisected next is the
                    // smallest interval
                    if ( (level[maxErr[0]] + 1) <= levmax) {
                        continue;
                    } // if ((level[maxErr[0]] + 1) <= levmax)

                    extrap = true;
                    nrmax[0] = 2;
                } // if (!extrap)

                if ( (ierro != 3) && (erlarg > ertest)) {
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

                        if ( (level[maxErr[0]] + 1) <= levmax) {
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

                    if ( (ktmin > 5) && (abserr[0] < (1.0E-3 * errSum))) {
                        ier[0] = 5;
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

                    if (ier[0] >= 5) {
                        break;
                    } // if (ier[0] >= 5)
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

                if ( (ier[0] + ierro) != 0) {

                    if (ierro == 3) {
                        abserr[0] = abserr[0] + correc;
                    }

                    if (ier[0] == 0) {
                        ier[0] = 3;
                    } // if (ier[0] == 0)

                    if ( (result[0] == 0.0) || (area == 0.0)) {

                        if (abserr[0] > errSum) {
                            result[0] = 0.0;

                            for (k = 0; k < last; k++) {
                                result[0] = result[0] + rlist[k];
                            }

                            abserr[0] = errSum;

                            if (ier[0] > 2) {
                                ier[0] = ier[0] - 1;
                            }

                            result[0] = result[0] * sign;

                            return;
                        } // if (abserr[0] > errSum)

                        if (area == 0.0) {

                            if (ier[0] > 2) {
                                ier[0] = ier[0] - 1;
                            }

                            result[0] = result[0] * sign;

                            return;
                        } // if (area == 0.0)

                        if ( (ksgn == -1) && (Math.max(Math.abs(result[0]), Math.abs(area)) <= (1.0E-2 * resabs))) {

                            if (ier[0] > 2) {
                                ier[0] = ier[0] - 1;
                            }

                            result[0] = result[0] * sign;

                            return;
                        } // if ((ksgn == -1) && (Math.max(Math.abs(result[0]),

                        if ( (1.0E-2 > (result[0] / area)) || ( (result[0] / area) > 100.0)
                                || (errSum > Math.abs(area))) {
                            ier[0] = 6;
                        } // if ((1.0E-2 > (result[0]/area) || ((result[0]/area) >

                        if (ier[0] > 2) {
                            ier[0] = ier[0] - 1;
                        }

                        result[0] = result[0] * sign;

                        return;
                    } // if ((result[0] == 0.0) || (area == 0.0))

                    if ( (abserr[0] / Math.abs(result[0])) > (errSum / Math.abs(area))) {
                        result[0] = 0.0;

                        for (k = 0; k < last; k++) {
                            result[0] = result[0] + rlist[k];
                        }

                        abserr[0] = errSum;

                        if (ier[0] > 2) {
                            ier[0] = ier[0] - 1;
                        }

                        result[0] = result[0] * sign;

                        return;
                    } // if ((abserr[0]/Math.abs(result[0])) > (errSum/Math.abs(area)))
                } // if ((ier[0] + ierro) != 0)

                if ( (ksgn == -1) && (Math.max(Math.abs(result[0]), Math.abs(area)) <= (1.0E-2 * resabs))) {

                    if (ier[0] > 2) {
                        ier[0] = ier[0] - 1;
                    }

                    result[0] = result[0] * sign;

                    return;
                } // if ((ksgn == -1) && (Math.max(Math.abs(result[0]),

                if ( (1.0E-2 > (result[0] / area)) || ( (result[0] / area) > 100.0) || (errSum > Math.abs(area))) {
                    ier[0] = 6;
                } // if ((1.0E-2 > (result[0]/area) || ((result[0]/area) >

                if (ier[0] > 2) {
                    ier[0] = ier[0] - 1;
                }

                result[0] = result[0] * sign;

                return;
            } // if (abserr[0] != oflow)

            result[0] = 0.0;

            for (k = 0; k < last; k++) {
                result[0] = result[0] + rlist[k];
            }

            abserr[0] = errSum;

            if (ier[0] > 2) {
                ier[0] = ier[0] - 1;
            }

            result[0] = result[0] * sign;

            return;

        } // try
        catch (final Exception err) {
            Preferences.debug("dqagpe error: " + err.getMessage(), Preferences.DEBUG_ALGORITHM);
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
                dqagie(bound, inf, epsrel);
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

            case DQAWFE:
                dqawfe();
                break;

            case DQAWOE:
                dqawoe(lower, upper, epsabs, epsrel, icall, result, abserr, neval, ier);
                break;

            case DQAWSE:
                dqawse();
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
     * @return abserr
     */
    public double getAbserr() {
        return abserr[0];
    }

    /**
     * DOCUMENT ME!
     * 
     * @return ier[0]
     */
    public int getErrorStatus() {
        return ier[0];
    }

    /**
     * DOCUMENT ME!
     * 
     * @return result
     */
    public double getIntegral() {
        return result[0];
    }

    /**
     * DOCUMENT ME!
     * 
     * @return neval
     */
    public int getNeval() {
        return neval[0];
    }

    /**
     * This is a port of the original FORTRAN routine whose header is given below: c***begin prologue dqage c***date
     * written 800101 (yymmdd) c***revision date 830518 (yymmdd) c***category no. h2a1a1 c***keywords automatic
     * integrator, general-purpose, c integrand examinator, globally adaptive, c gauss-kronrod c***author
     * piessens,robert,appl. math. & progr. div. - k.u.leuven c de doncker,elise,appl. math. & progr. div. - k.u.leuven
     * c***purpose the routine calculates an approximation result to a given c definite integral i = integral of f over
     * (a,b), c hopefully satisfying following claim for accuracy c abs(i-reslt).le.max(epsabs,epsrel*abs(i)).
     * c***description c c computation of a definite integral c standard fortran subroutine c double precision version
     * 
     * Calculates an integral over (lower, upper), hopefully satisfying the abs(actual integral - result) <= max(epsabs,
     * epsresl * abs(actual integral)).
     */
    private void dqage() {

        // Variables ending in 1 denote left subinterval and variables
        // ending in 2 denote right subinterval
        // Index to the interval with the largest error estimate
        final int[] maxErr = new int[1];

        // errMax[0] = elist[maxErr];
        final double[] errMax = new double[1];

        // Sum of the integrals over the subintervals
        double area;

        // Sum of the errors over the subintervals
        double errSum;

        // Requested accuracy max(epsabs, epsrel * abs(result))
        double errBnd;
        final double[] area1 = new double[1];
        double area12;
        final double[] area2 = new double[1];
        double a1;
        double a2;
        double b1;
        double b2;
        final double[] defabs = new double[1];
        final double[] defab1 = new double[1];
        final double[] defab2 = new double[1];
        final double[] error1 = new double[1];
        final double[] error2 = new double[1];
        double error12;
        final double[] resabs = new double[1];
        int iroff1;
        int iroff2;
        int k;
        int keyf;
        final int[] nrmax = new int[1];

        try {
            alist = new double[limit];
            blist = new double[limit];
            rlist = new double[limit];
            elist = new double[limit];
            iord = new int[limit];

            ier[0] = 0;
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

            if ( (abserr[0] <= (50.0 * epmach * defabs[0])) && (abserr[0] > errBnd)) {
                ier[0] = 2;
            } // if ((abserr[0] <= 50.0 * epmach * defabs[0]) &&

            if (limit == 1) {
                ier[0] = 1;
            } // if (limit == 1)

            if ( (ier[0] != 0) || ( (abserr[0] <= errBnd) && (abserr[0] != resabs[0])) || (abserr[0] == 0.0)) {

                if (keyf != 1) {
                    neval[0] = ( (10 * keyf) + 1) * ( (2 * neval[0]) + 1);
                } // if (keyf != 1)
                else {
                    neval[0] = (30 * neval[0]) + 15;
                } // else

                return;
            } // if ((ier[0] != 0) ||

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

                if ( (defab1[0] != error1[0]) && (defab2[0] != error2[0])) {

                    if ( (Math.abs(rlist[maxErr[0]] - area12) <= (1.0E-5 * Math.abs(area12)))
                            && (error12 >= (0.99 * errMax[0]))) {
                        iroff1 = iroff1 + 1;
                    } // if ((Math.abs(rlist[maxErr[0]] - area12) <=

                    if ( (last > 10) && (error12 > errMax[0])) {
                        iroff2 = iroff2 + 1;
                    } // if ((last > 10) && (error12 > errMax[0]))
                } // if ((defab1[0] != error1[0]) && (defab2[0] != error2[0]))

                rlist[maxErr[0]] = area1[0];
                rlist[last - 1] = area2[0];
                errBnd = Math.max(epsabs, epsrel * Math.abs(area));

                if (errSum > errBnd) {

                    // Test for roundoff error and eventually set error flag

                    if ( (iroff1 >= 6) || (iroff2 >= 20)) {
                        ier[0] = 2;
                    } // if ((iroff1 >= 6) || (iroff2 >= 20))

                    // Set error flag in the case that the number of subintervals
                    // equals limit.

                    if (last == limit) {
                        ier[0] = 1;
                    } // if (last == limit)

                    // Set error flag in the case of bad integrand behavior
                    // at a point of the integration range.

                    if (Math.max(Math.abs(a1), Math.abs(b2)) <= ( (1.0 + (100.0 * epmach)) * (Math.abs(a2) + (1.0E3 * uflow)))) {
                        ier[0] = 3;
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

                if ( (ier[0] != 0) || (errSum <= errBnd)) {
                    break;
                } // if (ier[0] != 0) || (errSum <= errBnd))
            } // for (last = 2; last <= limit; last++)

            // Compute final result.

            result[0] = 0.0;

            for (k = 0; k < last; k++) {
                result[0] = result[0] + rlist[k];
            } // for (k = 0; k < last; k++)

            abserr[0] = errSum;

            if (keyf != 1) {
                neval[0] = ( (10 * keyf) + 1) * ( (2 * neval[0]) + 1);
            } // if (keyf != 1)
            else {
                neval[0] = (30 * neval[0]) + 15;
            } // else

            return;
        } // try
        catch (final Exception err) {
            Preferences.debug("dqage error: " + err.getMessage(), Preferences.DEBUG_ALGORITHM);
        }

    }

    /**
     * This is a port of the original FORTRAN routine whose header is given below: c***begin prologue dqagse c***date
     * written 800101 (yymmdd) c***revision date 830518 (yymmdd) c***category no. h2a1a1 c***keywords automatic
     * integrator, general-purpose, c (end point) singularities, extrapolation, c globally adaptive c***author
     * piessens,robert,appl. math. & progr. div. - k.u.leuven c de doncker,elise,appl. math. & progr. div. - k.u.leuven
     * c***purpose the routine calculates an approximation result to a given c definite integral i = integral of f over
     * (a,b), c hopefully satisfying following claim for accuracy c abs(i-result).le.max(epsabs,epsrel*abs(i)).
     * c***description c c computation of a definite integral c standard fortran subroutine c double precision version
     * 
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
        final int[] maxErr = new int[1];

        // errMax[0] = elist[maxErr];
        final double[] errMax = new double[1];

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
        final int[] nres = new int[1];

        // Number of elements in rlist2. If an appropriate approximation
        // to the compounded integral has been obtained, it is put in
        // rlist2[numr12-1] after numr12 has been increased by one.
        final int[] numr12 = new int[1];

        // Sum of the errors over the intervals larger than the smallest
        // interval considered up to now
        double erlarg;

        // boolean variable denoting that the routine is attempting to
        // perform extrapolation. That is, before dividing the
        // interval we try to decrease the value of erlarg
        boolean extrap;

        // If noext is true, the extrapolation is no longer allowed.
        boolean noext;

        // length of the smallest interval considered up to now, multiplied
        // by 1.5
        double small;
        double[] res31a;
        final double[] abseps = new double[1];
        final double[] area1 = new double[1];
        double area12;
        final double[] area2 = new double[1];
        double a1;
        double a2;
        double b1;
        double b2;
        double correc;
        final double[] defabs = new double[1];
        final double[] defab1 = new double[1];
        final double[] defab2 = new double[1];
        double dres;
        final double[] error1 = new double[1];
        final double[] error2 = new double[1];
        double error12;
        double ertest;
        final double[] resabs = new double[1];
        final double[] reseps = new double[1];
        int id;
        int ierro;
        int iroff1;
        int iroff2;
        int iroff3;
        int jupbnd;
        int k;
        int ksgn;
        int ktmin;
        final int[] nrmax = new int[1];

        try {
            alist = new double[limit];
            blist = new double[limit];
            rlist = new double[limit];
            elist = new double[limit];
            iord = new int[limit];
            rlist2 = new double[52];
            res31a = new double[3];

            ier[0] = 0;
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

            if ( (abserr[0] <= (100.0 * epmach * defabs[0])) && (abserr[0] > errBnd)) {
                ier[0] = 2;
            } // if ((abserr[0] <= 100.0 * epmach * defabs[0]) &&

            if (limit == 1) {
                ier[0] = 1;
            } // if (limit == 1)

            if ( (ier[0] != 0) || ( (abserr[0] <= errBnd) && (abserr[0] != resabs[0])) || (abserr[0] == 0.0)) {
                neval[0] = (42 * last) - 21;

                return;
            } // if ((ier[0] != 0) || ((abserr[0] <= errBnd) && (abserr[0] !=

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

            if (dres >= ( (1.0 - (50.0 * epmach)) * defabs[0])) {
                ksgn = 1;
            } // if (dres >= (1.0 - 50.0 * epmach)*defabs[0])

            // Main do-loop
            loop: for (last = 2; last <= limit; last++) {

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

                if ( (defab1[0] != error1[0]) && (defab2[0] != error2[0])) {

                    if ( (Math.abs(rlist[maxErr[0]] - area12) <= (1.0E-5 * Math.abs(area12)))
                            && (error12 >= (0.99 * errMax[0]))) {

                        if (extrap) {
                            iroff2 = iroff2 + 1;
                        } // if (extrap)
                        else {
                            iroff1 = iroff1 + 1;
                        }
                    } // if ((Math.abs(rlist[maxErr[0]] - area12) <=

                    if ( (last > 10) && (error12 > errMax[0])) {
                        iroff3 = iroff3 + 1;
                    } // if ((last > 10) && (error12 > errMax[0]))
                } // if ((defab1[0] != error1[0]) && (defab2[0] != error2[0]))

                rlist[maxErr[0]] = area1[0];
                rlist[last - 1] = area2[0];
                errBnd = Math.max(epsabs, epsrel * Math.abs(area));

                // Test for roundoff error and eventually set error flag.

                if ( ( (iroff1 + iroff2) >= 10) || (iroff3 >= 20)) {
                    ier[0] = 2;
                } // if (((iroff1 + iroff2) >= 10) || (iroff3 >= 20))

                if (iroff2 >= 5) {
                    ierro = 3;
                } // if (iroff2 >= 5)

                // Set error flag in the case that the number of subintervals
                // equals limit.

                if (last == limit) {
                    ier[0] = 1;
                } // if (last == limit)

                // Set error flag in the case of bad integrand behavior
                // at a point of the integration range.

                if (Math.max(Math.abs(a1), Math.abs(b2)) <= ( (1.0 + (100.0 * epmach)) * (Math.abs(a2) + (1.0E3 * uflow)))) {
                    ier[0] = 4;
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

                    if (ier[0] > 2) {
                        ier[0] = ier[0] - 1;
                    } // if (ier[0] > 2)

                    neval[0] = (42 * last) - 21;

                    return;
                } // if (errSum <= errBnd)

                if (ier[0] != 0) {
                    break;
                } // if (ier[0] != 0)

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

                if ( !extrap) {

                    // Test whether the interval to be bisected next is the
                    // smallest interval.

                    if (Math.abs(blist[maxErr[0]] - alist[maxErr[0]]) > small) {
                        continue;
                    } // if (Math.abs(blist[maxErr[0]] - alist[maxErr[0]]) > small)

                    extrap = true;
                    nrmax[0] = 2;
                } // if (!extrap)

                if ( (ierro != 3) && (erlarg > ertest)) {

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

                if ( (ktmin > 5) && (abserr[0] < (1.0E-3 * errSum))) {
                    ier[0] = 5;
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

                if (ier[0] == 5) {
                    break;
                } // if (ier[0] == 5)

                maxErr[0] = iord[0];
                errMax[0] = elist[maxErr[0]];
                nrmax[0] = 1;
                extrap = false;
                small = 0.5 * small;
                erlarg = errSum;
            } // for (last = 2; last <= limit; last++)

            // Set final result and error estimate

            if (abserr[0] != oflow) {

                if ( (ier[0] + ierro) != 0) {

                    if (ierro == 3) {
                        abserr[0] = abserr[0] + correc;
                    } // if (ierro == 3)

                    if (ier[0] == 0) {
                        ier[0] = 3;
                    } // if (ier[0] == 0)

                    if ( (result[0] == 0.0) || (area == 0.0)) {

                        if (abserr[0] > errSum) {
                            result[0] = 0.0;

                            for (k = 0; k < last; k++) {
                                result[0] = result[0] + rlist[k];
                            } // for (k = 0; k < last; k++)

                            abserr[0] = errSum;

                            if (ier[0] > 2) {
                                ier[0] = ier[0] - 1;
                            } // if (ier[0] > 2)

                            neval[0] = (42 * last) - 21;

                            return;
                        } // if (abserr[0] > errSum)

                        if (area == 0.0) {

                            if (ier[0] > 2) {
                                ier[0] = ier[0] - 1;
                            } // if (ier[0] > 2)

                            neval[0] = (42 * last) - 21;

                            return;
                        } // if (area == 0.0)

                        if ( (ksgn == -1) && (Math.max(Math.abs(result[0]), Math.abs(area)) <= (1.0E-2 * defabs[0]))) {

                            if (ier[0] > 2) {
                                ier[0] = ier[0] - 1;
                            } // if (ier[0] > 2)

                            neval[0] = (42 * last) - 21;

                            return;
                        } // if ((ksgn == -1) && (Math.max(Math.abs(result[0]),

                        if ( (1.0E-2 > (result[0] / area)) || ( (result[0] / area) > 100.0)
                                || (errSum > Math.abs(area))) {
                            ier[0] = 6;
                        } // if ((1.0E-2 > (result[0]/area)) || (result[0]/area > 100.0) ||

                        if (ier[0] > 2) {
                            ier[0] = ier[0] - 1;
                        } // if (ier[0] > 2)

                        neval[0] = (42 * last) - 21;

                        return;
                    } // if ((result[0] == 0.00 || (area == 0.0))

                    if ( (abserr[0] / Math.abs(result[0])) > (errSum / Math.abs(area))) {
                        result[0] = 0.0;

                        for (k = 0; k < last; k++) {
                            result[0] = result[0] + rlist[k];
                        } // for (k = 0; k < last; k++)

                        abserr[0] = errSum;

                        if (ier[0] > 2) {
                            ier[0] = ier[0] - 1;
                        } // if (ier[0] > 2)

                        neval[0] = (42 * last) - 21;

                        return;
                    } // if (abserr[0]/Math.abs(result[0]) > errSum/Math.abs(area))
                } // if ((ier[0] + ierro) != 0)

                if ( (ksgn == -1) && (Math.max(Math.abs(result[0]), Math.abs(area)) <= (1.0E-2 * defabs[0]))) {

                    if (ier[0] > 2) {
                        ier[0] = ier[0] - 1;
                    } // if (ier[0] > 2)

                    neval[0] = (42 * last) - 21;

                    return;
                } // if ((ksgn == -1) && (Math.max(Math.abs(result[0]),

                if ( (1.0E-2 > (result[0] / area)) || ( (result[0] / area) > 100.0) || (errSum > Math.abs(area))) {
                    ier[0] = 6;
                } // if ((1.0E-2 > (result[0]/area)) || (result[0]/area > 100.0) ||

                if (ier[0] > 2) {
                    ier[0] = ier[0] - 1;
                } // if (ier[0] > 2)

                neval[0] = (42 * last) - 21;

                return;
            } // if (abserr[0] != oflow)

            result[0] = 0.0;

            for (k = 0; k < last; k++) {
                result[0] = result[0] + rlist[k];
            } // for (k = 0; k < last; k++)

            abserr[0] = errSum;

            if (ier[0] > 2) {
                ier[0] = ier[0] - 1;
            } // if (ier[0] > 2)

            neval[0] = (42 * last) - 21;

            return;
        } // try
        catch (final Exception err) {
            Preferences.debug("dqagse error: " + err.getMessage(), Preferences.DEBUG_ALGORITHM);
        }

    }

    /**
     * This is the port of the original FORTRAN routine whose header is given below: c***begin prologue dqawce c***date
     * written 800101 (yymmdd) c***revision date 830518 (yymmdd) c***category no. h2a2a1,j4 c***keywords automatic
     * integrator, special-purpose, c cauchy principal value, clenshaw-curtis method c***author piessens,robert,appl.
     * math. & progr. div. - k.u.leuven c de doncker,elise,appl. math. & progr. div. - k.u.leuven c*** purpose the
     * routine calculates an approximation result to a c cauchy principal value i = integral of f*w over (a,b) c (w(x) =
     * 1/(x-c), (c.ne.a, c.ne.b), hopefully satisfying c following claim for accuracy c
     * abs(i-result).le.max(epsabs,epsrel*abs(i)) c***description c c computation of a cauchy principal value c standard
     * fortran subroutine c double precision version
     */
    private void dqawce() {
        // Routines called dqc25c, dqpsrt
        // Variables end in 1 for the the left subinterval
        // and variables end in 2 for the right subinterval

        // Index to the interval with the largest error estimate
        final int[] maxErr = new int[1];

        // errMax[0] = elist[maxErr];
        final double[] errMax = new double[1];

        // Sum of the integrals over the subintervals
        double area;

        // Sum of the errors over the subintervals
        double errSum;

        // Requested accuracy max(epsabs, epsrel * abs(result))
        double errBnd;

        double aa;
        final double area1[] = new double[1];
        double area12;
        final double area2[] = new double[1];
        double a1;
        double a2;
        double bb;
        double b1;
        double b2;
        final double error1[] = new double[1];
        double erro12;
        final double error2[] = new double[1];
        int iroff1;
        int iroff2;
        int k;
        final int krule[] = new int[1];
        final int nev[] = new int[1];
        final int nrmax[] = new int[1];

        try {
            alist = new double[limit];
            blist = new double[limit];
            rlist = new double[limit];
            elist = new double[limit];
            iord = new int[limit];

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
            ier[0] = 0;
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
                ier[0] = 1;
            }
            if ( (abserr[0] < Math.min(0.01 * Math.abs(result[0]), errBnd)) || (ier[0] == 1)) {
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

            for (last = 2; last <= limit; last++) {

                // Biset the subinterval with the nrmax-th largest error estimate.

                a1 = alist[maxErr[0]];
                b1 = 0.5 * (alist[maxErr[0]] + blist[maxErr[0]]);
                b2 = blist[maxErr[0]];
                if ( (c <= b1) && (c > a1)) {
                    b1 = 0.5 * (c + b2);
                }
                if ( (c > b1) && (c < b2)) {
                    b1 = 0.5 * (a1 + c);
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
                if ( (Math.abs(rlist[maxErr[0]] - area12) < 1.0E-5 * Math.abs(area12)) && (erro12 >= 0.99 * errMax[0])
                        && (krule[0] == 0)) {
                    iroff1 = iroff1 + 1;
                }
                if ( (last > 10) && (erro12 > errMax[0]) && (krule[0] == 0)) {
                    iroff2 = iroff2 + 1;
                }
                rlist[maxErr[0]] = area1[0];
                rlist[last - 1] = area2[0];
                errBnd = Math.max(epsabs, epsrel * Math.abs(area));
                if (errSum > errBnd) {

                    // Test for roundoff error and eventually set error flag.

                    if ( (iroff1 >= 6) && (iroff2 > 20)) {
                        ier[0] = 2;
                    }

                    // Set error flag in the case that number of interval
                    // bisections exceeds limit.

                    if (last == limit) {
                        ier[0] = 1;
                    }

                    // Set error flag in the case of bad integrand behavior
                    // at a point of the integration range
                    if (Math.max(Math.abs(a1), Math.abs(b2)) <= ( (1.0 + 100.0 * epmach) * (Math.abs(a2) + 1.0E3 * uflow))) {
                        ier[0] = 3;
                    }
                } // if (errSum > errBnd)

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

                // Call subroutine dqpsrt to maintain the descending ordering
                // in the list of error estimates and select the subinterval
                // with the nrmax-th largest error estimate (to be bisected next).

                dqpsrt(limit, last, maxErr, errMax, elist, iord, nrmax);
                if ( (ier[0] != 0) || (errSum <= errBnd)) {
                    break;
                }
            } // for (last = 2; last <= limit; last++)

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
        catch (final Exception err) {
            Preferences.debug("dqawce error: " + err.getMessage(), Preferences.DEBUG_ALGORITHM);
        }
    } // dqawce

    /**
     * This is the port of the original FORTRAN routine whose header is given below: c***begin prologue dqawfe c***date
     * written 800101 (yymmdd) c***revision date 830518 (yymmdd) c***category no. h2a3a1 c***keywords automatic
     * integrator, special-purpose, c fourier integrals, c integration between zeros with dqawoe, c convergence
     * acceleration with dqelg c***author piessens,robert,appl. math. & progr. div. - k.u.leuven c dedoncker,elise,appl.
     * math. & progr. div. - k.u.leuven c***purpose the routine calculates an approximation result to a c given fourier
     * integal c i = integral of f(x)*w(x) over (a,infinity) c where w(x)=cos(omega*x) or w(x)=sin(omega*x), c hopefully
     * satisfying following claim for accuracy c abs(i-result).le.epsabs. c***description c c computation of fourier
     * integrals c standard fortran subroutine c double precision version c c parameters c on entry c f - double
     * precision c function subprogram defining the integrand c function f(x). the actual name for f needs to c be
     * declared e x t e r n a l in the driver program. c c a - double precision c lower limit of integration c c omega -
     * double precision c parameter in the weight function c c integr - integer c indicates which weight function is
     * used c integr = 1 w(x) = cos(omega*x) c integr = 2 w(x) = sin(omega*x) c if integr.ne.1.and.integr.ne.2, the
     * routine will c end with ier = 6. c c epsabs - double precision c absolute accuracy requested, epsabs.gt.0 c if
     * epsabs.le.0, the routine will end with ier = 6. c c limlst - integer c limlst gives an upper bound on the number
     * of c cycles, limlst.ge.1. c if limlst.lt.3, the routine will end with ier = 6. c c limit - integer c gives an
     * upper bound on the number of subintervals c allowed in the partition of each cycle, limit.ge.1 c each cycle,
     * limit.ge.1. c c maxp1 - integer c gives an upper bound on the number of c chebyshev moments which can be stored,
     * i.e. c for the intervals of lengths abs(b-a)*2**(-l), c l=0,1, ..., maxp1-2, maxp1.ge.1 c c on return c result -
     * double precision c approximation to the integral x c c abserr - double precision c estimate of the modulus of the
     * absolute error, c which should equal or exceed abs(i-result) c c neval - integer c number of integrand
     * evaluations c c ier - ier = 0 normal and reliable termination of c the routine. it is assumed that the c
     * requested accuracy has been achieved. c ier.gt.0 abnormal termination of the routine. the c estimates for
     * integral and error are less c reliable. it is assumed that the requested c accuracy has not been achieved. c
     * error messages c if omega.ne.0 c ier = 1 maximum number of cycles allowed c has been achieved., i.e. of
     * subintervals c (a+(k-1)c,a+kc) where c c = (2*int(abs(omega))+1)*pi/abs(omega), c for k = 1, 2, ..., lst. c one
     * can allow more cycles by increasing c the value of limlst (and taking the c according dimension adjustments into
     * c account). c examine the array iwork which contains c the error flags on the cycles, in order to c look for
     * eventual local integration c difficulties. if the position of a local c difficulty can be determined (e.g. c
     * singularity, discontinuity within the c interval) one will probably gain from c splitting up the interval at this
     * point c and calling appropriate integrators on c the subranges. c = 4 the extrapolation table constructed for c
     * convergence acceleration of the series c formed by the integral contributions over c the cycles, does not
     * converge to within c the requested accuracy. as in the case of c ier = 1, it is advised to examine the c array
     * iwork which contains the error c flags on the cycles. c = 6 the input is invalid because c (integr.ne.1 and
     * integr.ne.2) or c epsabs.le.0 or limlst.lt.3. c result, abserr, neval, lst are set c to zero. c = 7 bad integrand
     * behaviour occurs within one c or more of the cycles. location and type c of the difficulty involved can be c
     * determined from the vector ierlst. here c lst is the number of cycles actually c needed (see below). c ierlst(k) =
     * 1 the maximum number of c subdivisions (= limit) has c been achieved on the k th c cycle. c = 2 occurrence of
     * roundoff error c is detected and prevents the c tolerance imposed on the c k th cycle, from being c achieved. c =
     * 3 extremely bad integrand c behaviour occurs at some c points of the k th cycle. c = 4 the integration procedure
     * c over the k th cycle does c not converge (to within the c required accuracy) due to c roundoff in the c
     * extrapolation procedure c invoked on this cycle. it c is assumed that the result c on this interval is the c best
     * which can be obtained. c = 5 the integral over the k th c cycle is probably divergent c or slowly convergent. it
     * c must be noted that c divergence can occur with c any other value of c ierlst(k). c if omega = 0 and integr = 1,
     * c the integral is calculated by means of dqagie c and ier = ierlst(1) (with meaning as described c for ierlst(k),
     * k = 1). c c rslst - double precision c vector of dimension at least limlst c rslst(k) contains the integral
     * contribution c over the interval (a+(k-1)c,a+kc) where c c = (2*int(abs(omega))+1)*pi/abs(omega), c k = 1, 2,
     * ..., lst. c note that, if omega = 0, rslst(1) contains c the value of the integral over (a,infinity). c c erlst -
     * double precision c vector of dimension at least limlst c erlst(k) contains the error estimate corresponding c
     * with rslst(k). c c ierlst - integer c vector of dimension at least limlst c ierlst(k) contains the error flag
     * corresponding c with rslst(k). for the meaning of the local error c flags see description of output parameter
     * ier. c c lst - integer c number of subintervals needed for the integration c if omega = 0 then lst is set to 1. c
     * c alist, blist, rlist, elist - double precision c vector of dimension at least limit, c c iord, nnlog - integer c
     * vector of dimension at least limit, providing c space for the quantities needed in the subdivision c process of
     * each cycle c c chebmo - double precision c array of dimension at least (maxp1,25), providing c space for the
     * chebyshev moments needed within the c cycles c c***references (none) c***routines called
     * d1mach,dqagie,dqawoe,dqelg c***end prologue dqawfe c
     */
    private void dqawfe() {
        final double abseps[] = new double[1];
        double correc;
        // (2*int(abs(omega))+1)*pi/abs(omega)
        double cycle;
        // c1, c2 end points of subinterval (of length cycle)
        double c1;
        double c2;
        double dl;
        double drl = 0.0;
        double ep;
        double eps;
        // absolute tolerance requested over current subinterval
        double epsa;
        // sum of error estimates over subintervals, calculated cumulatively
        double errsum;
        double fact;
        final double p = 0.9;
        double p1;
        // The dimension of psum is determined by the value of limexp
        // in subroutine dqelg (psum must be of dimension (limexp+2) at least).
        // psum contains the part of the epsilon table which is still
        // needed for further computations. Each element of psum is a
        // partial sum of the series which should sum to the value of
        // the integral.
        double psum[];
        final double reseps[] = new double[1];
        double res3la[];
        final double rslstVar[] = new double[1];
        final double erlstVar[] = new double[1];
        final int ierlstVar[] = new int[1];

        int ktmin;
        int l;
        int ll = 0;
        final int nev[] = new int[1];
        final int nres[] = new int[1];
        final int numrl2[] = new int[1];

        try {
            alist = new double[limit];
            blist = new double[limit];
            chebmo = new double[maxp1][25];
            elist = new double[limit];
            erlst = new double[limlst];
            ierlst = new int[limlst];
            iord = new int[limit];
            nnlog = new int[limit];
            psum = new double[52];
            res3la = new double[3];
            rlist = new double[limit];
            rslst = new double[limlst];

            result[0] = 0.0;
            abserr[0] = 0.0;
            neval[0] = 0;
            lst = 0;
            ier[0] = 0;
            if (omega == 0.0) {

                // Integration by dqagie if omega is zero

                if (integr == 1) {
                    dqagie(0.0, 1, 0.0);
                }
                rslst[0] = result[0];
                erlst[0] = abserr[0];
                ierlst[0] = ier[0];
                lst = 1;
                return;
            } // if (omega == 0.0)

            // Initializations

            l = (int) Math.abs(omega);
            dl = 2 * l + 1;
            cycle = dl * Math.PI / Math.abs(omega);
            ier[0] = 0;
            ktmin = 0;
            neval[0] = 0;
            numrl2[0] = 0;
            nres[0] = 0;
            c1 = lower;
            c2 = cycle + lower;
            p1 = 1.0 - p;
            eps = epsabs;
            if (epsabs > uflow / p1) {
                eps = epsabs * p1;
            }
            ep = eps;
            fact = 1.0;
            correc = 0.0;
            abserr[0] = 0.0;
            errsum = 0.0;

            // main for loop
            for (lst = 1; lst <= limlst; lst++) {

                // integrate over current subinterval.

                epsa = eps * fact;
                rslstVar[0] = rslst[lst - 1];
                erlstVar[0] = erlst[lst - 1];
                ierlstVar[0] = ierlst[lst - 1];
                dqawoe(c1, c2, epsa, 0.0, lst, rslstVar, erlstVar, nev, ierlstVar);
                rslst[lst - 1] = rslstVar[0];
                erlst[lst - 1] = erlstVar[0];
                ierlst[lst - 1] = ierlstVar[0];
                neval[0] = neval[0] + nev[0];
                fact = fact * p;
                errsum = errsum + erlst[lst - 1];
                drl = 50.0 * Math.abs(rslst[lst - 1]);

                // test on accuracy with partial sum

                if ( (errsum + drl) <= epsabs && lst >= 6) {
                    result[0] = psum[numrl2[0] - 1];
                    abserr[0] = errsum + drl;
                    return;
                }
                correc = Math.max(correc, erlst[lst - 1]);
                if (ierlst[lst - 1] != 0) {
                    eps = Math.max(ep, correc * p1);
                }
                if (ierlst[lst - 1] != 0) {
                    ier[0] = 7;
                }
                if (ier[0] == 7 && (errsum + drl) <= correc * 10.0 && lst > 5) {
                    result[0] = psum[numrl2[0] - 1];
                    abserr[0] = errsum + drl;
                    return;
                }
                numrl2[0] = numrl2[0] + 1;
                if (lst <= 1) {
                    psum[0] = rslst[0];
                    ll = numrl2[0];
                    c1 = c2;
                    c2 = c2 + cycle;
                    continue;
                } // if (lst <= 1)
                psum[numrl2[0] - 1] = psum[ll - 1] + rslst[lst - 1];
                if (lst == 2) {
                    ll = numrl2[0];
                    c1 = c2;
                    c2 = c2 + cycle;
                    continue;
                } // if (lst == 2)

                // test on maximum number of subintervals

                if (lst == limlst) {
                    ier[0] = 1;
                }

                // perform new extrapolation

                dqelg(numrl2, psum, reseps, abseps, res3la, nres);

                // test whether extrapolated result is influenced by roundoff

                ktmin = ktmin + 1;
                if (ktmin >= 15 && abserr[0] <= 1.0E-3 * (errsum + drl)) {
                    ier[0] = 4;
                }
                if (abseps[0] <= abserr[0] || lst == 3) {
                    abserr[0] = abseps[0];
                    result[0] = reseps[0];
                    ktmin = 0;

                    // if ier[0] is not 0, check whether direct result (partial sum)
                    // or extrapolated result yields the best integral
                    // approximation

                    if ( (abserr[0] + 10.0 * correc) <= epsabs || (abserr[0] <= epsabs && 10.0 * correc >= epsabs)) {
                        break;
                    }

                } // if(abseps[0] <= abserr[0] || lst == 3)
                if (ier[0] != 0 && ier[0] != 7) {
                    break;
                }
                ll = numrl2[0];
                c1 = c2;
                c2 = c2 + cycle;
            } // for (lst = 1; lst <= limlst; lst++)

            // set final result and error estimate
            // -----------------------------------

            abserr[0] = abserr[0] + 10.0 * correc;
            if (ier[0] == 0) {
                return;
            }
            if (result[0] == 0.0 || psum[numrl2[0] - 1] == 0.0) {
                if (abserr[0] > errsum) {
                    result[0] = psum[numrl2[0] - 1];
                    abserr[0] = errsum + drl;
                    return;
                }
                if (psum[numrl2[0] - 1] == 0.0) {
                    return;
                }
            } // if(result[0] == 0.0 || psum[numrl2[0]-1] == 0.0)
            if (abserr[0] / Math.abs(result[0]) > (errsum + drl) / Math.abs(psum[numrl2[0] - 1])) {
                result[0] = psum[numrl2[0] - 1];
                abserr[0] = errsum + drl;
                return;
            }
            if (ier[0] >= 1 && ier[0] != 7) {
                abserr[0] = abserr[0] + drl;
            }
            return;
        } catch (final Exception err) {
            Preferences.debug("dqawfe error: " + err.getMessage(), Preferences.DEBUG_ALGORITHM);
        }
    } // dqawfe

    /**
     * This is the port of the original FORTRAN routine whose header is given below: c***begin prologue dqawoe c***date
     * written 800101 (yymmdd) c***revision date 830518 (yymmdd) c***category no. h2a2a1 c***keywords automatic
     * integrator, special-purpose, c integrand with oscillatory cos or sin factor, c clenshaw-curtis method, (end
     * point) singularities, c extrapolation, globally adaptive c***author piessens,robert,appl. math. & progr. div. -
     * k.u.leuven c de doncker,elise,appl. math. & progr. div. - k.u.leuven c***purpose the routine calculates an
     * approximation result to a given c definite integral c i = integral of f(x)*w(x) over (a,b) c where w(x) =
     * cos(omega*x) or w(x)=sin(omega*x), c hopefully satisfying following claim for accuracy c
     * abs(i-result).le.max(epsabs,epsrel*abs(i)). c***description c c computation of oscillatory integrals c standard
     * fortran subroutine c double precision version c c parameters c on entry c f - double precision c function
     * subprogram defining the integrand c function f(x). the actual name for f needs to be c declared e x t e r n a l
     * in the driver program. c c a - double precision c lower limit of integration c c b - double precision c upper
     * limit of integration c c omega - double precision c parameter in the integrand weight function c c integr -
     * integer c indicates which of the weight functions is to be c used c integr = 1 w(x) = cos(omega*x) c integr = 2
     * w(x) = sin(omega*x) c if integr.ne.1 and integr.ne.2, the routine c will end with ier = 6. c c epsabs - double
     * precision c absolute accuracy requested c epsrel - double precision c relative accuracy requested c if
     * epsabs.le.0 c and epsrel.lt.max(50*rel.mach.acc.,0.5d-28), c the routine will end with ier = 6. c c limit -
     * integer c gives an upper bound on the number of subdivisions c in the partition of (a,b), limit.ge.1. c c icall -
     * integer c if dqawoe is to be used only once, icall must c be set to 1. assume that during this call, the c
     * chebyshev moments (for clenshaw-curtis integration c of degree 24) have been computed for intervals of c lenghts
     * (abs(b-a))*2**(-l), l=0,1,2,...momcom-1. c if icall.gt.1 this means that dqawoe has been c called twice or more
     * on intervals of the same c length abs(b-a). the chebyshev moments already c computed are then re-used in
     * subsequent calls. c if icall.lt.1, the routine will end with ier = 6. c c maxp1 - integer c gives an upper bound
     * on the number of chebyshev c moments which can be stored, i.e. for the c intervals of lenghts abs(b-a)*2**(-l), c
     * l=0,1, ..., maxp1-2, maxp1.ge.1. c if maxp1.lt.1, the routine will end with ier = 6. c c on return c result -
     * double precision c approximation to the integral c c abserr - double precision c estimate of the modulus of the
     * absolute error, c which should equal or exceed abs(i-result) c c neval - integer c number of integrand
     * evaluations c c ier - integer c ier = 0 normal and reliable termination of the c routine. it is assumed that the
     * c requested accuracy has been achieved. c - ier.gt.0 abnormal termination of the routine. c the estimates for
     * integral and error are c less reliable. it is assumed that the c requested accuracy has not been achieved. c
     * error messages c ier = 1 maximum number of subdivisions allowed c has been achieved. one can allow more c
     * subdivisions by increasing the value of c limit (and taking according dimension c adjustments into account).
     * however, if c this yields no improvement it is advised c to analyze the integrand, in order to c determine the
     * integration difficulties. c if the position of a local difficulty can c be determined (e.g. singularity, c
     * discontinuity within the interval) one c will probably gain from splitting up the c interval at this point and
     * calling the c integrator on the subranges. if possible, c an appropriate special-purpose integrator c should be
     * used which is designed for c handling the type of difficulty involved. c = 2 the occurrence of roundoff error is
     * c detected, which prevents the requested c tolerance from being achieved. c the error may be under-estimated. c =
     * 3 extremely bad integrand behaviour occurs c at some points of the integration c interval. c = 4 the algorithm
     * does not converge. c roundoff error is detected in the c extrapolation table. c it is presumed that the requested
     * c tolerance cannot be achieved due to c roundoff in the extrapolation table, c and that the returned result is
     * the c best which can be obtained. c = 5 the integral is probably divergent, or c slowly convergent. it must be
     * noted that c divergence can occur with any other value c of ier.gt.0. c = 6 the input is invalid, because c
     * (epsabs.le.0 and c epsrel.lt.max(50*rel.mach.acc.,0.5d-28)) c or (integr.ne.1 and integr.ne.2) or c icall.lt.1 or
     * maxp1.lt.1. c result, abserr, neval, last, rlist(1), c elist(1), iord(1) and nnlog(1) are set c to zero. alist(1)
     * and blist(1) are set c to a and b respectively. c c last - integer c on return, last equals the number of c
     * subintervals produces in the subdivision c process, which determines the number of c significant elements
     * actually in the c work arrays. c alist - double precision c vector of dimension at least limit, the first c last
     * elements of which are the left c end points of the subintervals in the partition c of the given integration range
     * (a,b) c c blist - double precision c vector of dimension at least limit, the first c last elements of which are
     * the right c end points of the subintervals in the partition c of the given integration range (a,b) c c rlist -
     * double precision c vector of dimension at least limit, the first c last elements of which are the integral c
     * approximations on the subintervals c c elist - double precision c vector of dimension at least limit, the first c
     * last elements of which are the moduli of the c absolute error estimates on the subintervals c c iord - integer c
     * vector of dimension at least limit, the first k c elements of which are pointers to the error c estimates over
     * the subintervals, c such that elist(iord(1)), ..., c elist(iord(k)) form a decreasing sequence, with c k = last
     * if last.le.(limit/2+2), and c k = limit+1-last otherwise. c c nnlog - integer c vector of dimension at least
     * limit, containing the c subdivision levels of the subintervals, i.e. c iwork(i) = l means that the subinterval c
     * numbered i is of length abs(b-a)*2**(1-l) c c on entry and return c momcom - integer c indicating that the
     * chebyshev moments c have been computed for intervals of lengths c (abs(b-a))*2**(-l), l=0,1,2, ..., momcom-1, c
     * momcom.lt.maxp1 c c chebmo - double precision c array of dimension (maxp1,25) containing the c chebyshev moments
     * c c***references (none) c***routines called d1mach,dqc25f,dqelg,dqpsrt c***end prologue dqawoe c
     */
    private void dqawoe(final double a, final double b, final double epsabs, final double epsrel, final int icall,
            final double result[], final double abserr[], final int neval[], final int ier[]) {
        // Variables ending in 1 denote left subinterval and variables
        // ending in 2 denote right subinterval
        // rlist2 should be length at least (limexp + 2)
        // The value of limexp is determined in subroutine dqelg
        // rlist2 contains the part of the epsilon table still needed for
        // further computations
        double[] rlist2;

        // Index to the interval with the largest error estimate
        final int[] maxErr = new int[1];

        // errMax[0] = elist[maxErr];
        final double[] errMax = new double[1];

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
        final int[] nres = new int[1];

        // Number of elements in rlist2. If an appropriate approximation
        // to the compounded integral has been obtained, it is put in
        // rlist2[numr12-1] after numr12 has been increased by one.
        final int[] numr12 = new int[1];

        // Length of the smallest interval considered up to now,
        // multiplied by 1.5
        double small;

        // Sum of the errors over the intervals larger than the smallest
        // interval considered up to now
        double erlarg = 0.0;

        // boolean variable denoting that the routine is attempting to
        // perform extrapolation. That is, before dividing the
        // interval we try to decrease the value of erlarg
        boolean extrap;

        // If noext is true, the extrapolation is no longer allowed.
        boolean noext;

        final double abseps[] = new double[1];
        final double area1[] = new double[1];
        double area12;
        final double area2[] = new double[1];
        double a1;
        double a2;
        double b1;
        double b2;
        double correc = 0.0;
        final double defab1[] = new double[1];
        final double defab2[] = new double[1];
        final double defabs[] = new double[1];
        double domega;
        double dres;
        final double error1[] = new double[1];
        double erro12;
        final double error2[] = new double[1];
        double ertest = 0.0;
        final double reseps[] = new double[1];
        final double res31a[] = new double[3];
        double width;
        int id;
        int ierro;
        int iroff1;
        int iroff2;
        int iroff3;
        int jupbnd;
        int k;
        int ksgn;
        int ktmin;
        final int nev[] = new int[1];
        final int nrmax[] = new int[1];
        int nrmom;
        boolean extall;
        boolean do50 = false;
        boolean do70 = false;
        boolean do75 = false;
        boolean do90 = false;
        boolean do150 = false;
        boolean do160 = false;
        boolean do165 = false;
        boolean do170 = false;
        boolean do190 = false;

        try {
            alist = new double[limit];
            blist = new double[limit];
            rlist = new double[limit];
            elist = new double[limit];
            iord = new int[limit];
            rlist2 = new double[52];
            if ( (chebmo == null) || (chebmo.length != maxp1)) {
                chebmo = new double[maxp1][25];
            }
            nnlog = new int[limit];

            ier[0] = 0;
            neval[0] = 0;
            last = 0;
            result[0] = 0.0;
            abserr[0] = 0.0;
            alist[0] = a;
            blist[0] = b;
            rlist[0] = 0.0;
            elist[0] = 0.0;
            iord[0] = -1;
            nnlog[0] = 0;

            // First approximation to the integral
            domega = Math.abs(omega);
            nrmom = 0;
            if (icall == 1) {
                momcom = 0;
            }
            dqc25f(a, b, domega, nrmom, 0, result, abserr, neval, defabs, resabs);

            // Test on accuracy

            dres = Math.abs(result[0]);
            errBnd = Math.max(epsabs, epsrel * dres);
            rlist[0] = result[0];
            elist[0] = abserr[0];
            iord[0] = 0;
            if ( (abserr[0] <= 100.0 * epmach * defabs[0]) && (abserr[0] > errBnd)) {
                ier[0] = 2;
            }
            if (limit == 1) {
                ier[0] = 1;
            }
            if ( (ier[0] != 0) || (abserr[0] <= errBnd)) {
                if ( (integr == 2) && (omega < 0.0)) {
                    result[0] = -result[0];
                }
                return;
            }

            // Initializations
            errMax[0] = abserr[0];
            maxErr[0] = 0;
            area = result[0];
            errSum = abserr[0];
            abserr[0] = oflow;
            nrmax[0] = 1;
            extrap = false;
            noext = false;
            ierro = 0;
            iroff1 = 0;
            iroff2 = 0;
            iroff3 = 0;
            ktmin = 0;
            small = Math.abs(b - a) * 0.75;
            nres[0] = 0;
            numr12[0] = 0;
            extall = false;
            if (0.5 * Math.abs(b - a) * domega <= 2.0) {
                numr12[0] = 1;
                extall = true;
                rlist2[0] = result[0];
            }
            if (0.25 * Math.abs(b - a) * domega <= 2.0) {
                extall = true;
            }
            ksgn = -1;
            if (dres >= (1.0 - 50.0 * epmach) * defabs[0]) {
                ksgn = 1;
            }

            // Main for loop

            do150 = true;
            loop: for (last = 2; last <= limit; last++) {

                // Bisect the subinteval with the nrmax-th largest error estimate.

                nrmom = nnlog[maxErr[0]] + 1;
                a1 = alist[maxErr[0]];
                b1 = 0.5 * (alist[maxErr[0]] + blist[maxErr[0]]);
                a2 = b1;
                b2 = blist[maxErr[0]];
                erlast = errMax[0];
                dqc25f(a1, b1, domega, nrmom, 0, area1, error1, nev, resabs, defab1);
                neval[0] = neval[0] + nev[0];
                dqc25f(a2, b2, domega, nrmom, 1, area2, error2, nev, resabs, defab2);
                neval[0] = neval[0] + nev[0];

                // Improve precious approximations to integral
                // and error and test for accuracy.

                area12 = area1[0] + area2[0];
                erro12 = error1[0] + error2[0];
                errSum = errSum + erro12 - errMax[0];
                area = area + area12 - rlist[maxErr[0]];
                if ( (defab1[0] != error1[0]) && (defab2[0] != error2[0])) {
                    if ( (Math.abs(rlist[maxErr[0]] - area12) <= 1.0E-5 * Math.abs(area12))
                            && (erro12 >= 0.99 * errMax[0])) {
                        if (extrap) {
                            iroff2 = iroff2 + 1;
                        } else {
                            iroff1 = iroff1 + 1;
                        }
                    }
                    if ( (last > 10) && (erro12 > errMax[0])) {
                        iroff3 = iroff3 + 1;
                    }
                } // if ((defab1[0] != error1[0]) && (defab2[0] != error2[0]))
                rlist[maxErr[0]] = area1[0];
                rlist[last - 1] = area2[0];
                nnlog[maxErr[0]] = nrmom;
                nnlog[last - 1] = nrmom;
                errBnd = Math.max(epsabs, epsrel * Math.abs(area));

                // Test for roundoff error and eventually set error flag.

                if ( (iroff1 + iroff2 >= 10) || (iroff3 >= 20)) {
                    ier[0] = 2;
                }
                if (iroff2 >= 5) {
                    ierro = 3;
                }

                // Set error flag in the case that the number of
                // subintervals equals the limit.

                if (last == limit) {
                    ier[0] = 1;
                }

                // Set error flag in the case of bad integrand behavior
                // at a point of the integration range.

                if (Math.max(Math.abs(a1), Math.abs(b2)) <= (1.0 + 100.0 * epmach) * (Math.abs(a2) + 1.0E3 * uflow)) {
                    ier[0] = 4;
                }

                // Append the newly-created intervals to the list
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

                // Call the subroutine dqpsrt to maintain the descending ordering
                // in the list of error estimates and select the subinterval
                // with the nrmax-th largest error estimate (to bisected next).

                dqpsrt(limit, last, maxErr, errMax, elist, iord, nrmax);
                if (errSum <= errBnd) {
                    do150 = false;
                    do170 = true;
                    break;
                }
                if (ier[0] != 0) {
                    break;
                }
                if ( (last == 2) && extall) {
                    small = 0.5 * small;
                    numr12[0] = numr12[0] + 1;
                    rlist2[numr12[0] - 1] = area;
                    ertest = errBnd;
                    erlarg = errSum;
                    continue;
                } // if ((last == 2) && extall)
                if (noext) {
                    continue;
                }
                if (extall) {
                    erlarg = erlarg - erlast;
                    if (Math.abs(b1 - a1) > small) {
                        erlarg = erlarg + erro12;
                    }
                    if (extrap) {
                        do70 = true;
                    } else {
                        do50 = true;
                    }
                } // if (extall)
                else {
                    do50 = true;
                }

                // Test whether the interval to be bisected next is the
                // smallest interval.

                if (do50) {
                    do50 = false;
                    width = Math.abs(blist[maxErr[0]] - alist[maxErr[0]]);
                    if (width > small) {
                        continue;
                    } // if (width > small)
                    if ( !extall) {

                        // Test whether we can start with the extrapolation procedure
                        // (we do this if we integrate over the next interval with
                        // use of a gauss-kronrod rule - see subroutine dqc25f).

                        small = 0.5 * small;
                        if (0.25 * width * domega > 2.0) {
                            continue;
                        }
                        extall = true;
                        ertest = errBnd;
                        erlarg = errSum;
                        continue;
                    } // if (!extall)
                    extrap = true;
                    nrmax[0] = 2;
                    do70 = true;
                } // if (do50)
                if (do70) {
                    do70 = false;
                    if ( (ierro == 3) || (erlarg <= ertest)) {
                        do90 = true;
                    } else {
                        do75 = true;
                    }
                } // if (do70)

                if (do75) {
                    do75 = false;
                    // The smallest interval has the largest error.
                    // Before bisecting decrease the sum of the errors over
                    // the larger intervals (erlarg) and perform extrapolation.

                    jupbnd = last;
                    if (last > (limit / 2 + 2)) {
                        jupbnd = limit + 3 - last;
                    }
                    id = nrmax[0];
                    for (k = id; k <= jupbnd; k++) {
                        maxErr[0] = iord[nrmax[0] - 1];
                        errMax[0] = elist[maxErr[0]];
                        if (Math.abs(blist[maxErr[0]] - alist[maxErr[0]]) > small) {
                            continue loop;
                        }
                        nrmax[0] = nrmax[0] + 1;
                    } // for (k = id; k <= jupbnd; k++)
                    do90 = true;
                } // if (do75)
                if (do90) {
                    do90 = false;
                    // Perform extrapolation
                    numr12[0] = numr12[0] + 1;
                    rlist2[numr12[0] - 1] = area;
                    if (numr12[0] >= 3) {
                        dqelg(numr12, rlist2, reseps, abseps, res31a, nres);
                        ktmin = ktmin + 1;
                        if ( (ktmin > 5) && (abserr[0] < 1.0E-3 * errSum)) {
                            ier[0] = 5;
                        }
                        if (abseps[0] < abserr[0]) {
                            ktmin = 0;
                            abserr[0] = abseps[0];
                            result[0] = reseps[0];
                            correc = erlarg;
                            ertest = Math.max(epsabs, epsrel * Math.abs(reseps[0]));
                            if (abserr[0] <= ertest) {
                                break;
                            }
                        } // if (abseps[0] < abserr[0])
                        // Prepare bisection of the smallest interval
                        if (numr12[0] == 1) {
                            noext = true;
                        }
                        if (ier[0] == 5) {
                            break;
                        }
                    } // if (numr12[0] >= 3)
                    maxErr[0] = iord[0];
                    errMax[0] = elist[maxErr[0]];
                    nrmax[0] = 1;
                    extrap = false;
                    small = 0.5 * small;
                    erlarg = errSum;
                } // if (do90)
            } // for (last = 2; last <= limit; last++)

            // Set the final result.

            if (do150) {
                if ( (abserr[0] == oflow) || (nres[0] == 0)) {
                    do170 = true;
                } else if (ier[0] + ierro == 0) {
                    do165 = true;
                } else {
                    if (ierro == 3) {
                        abserr[0] = abserr[0] + correc;
                    }
                    if (ier[0] == 0) {
                        ier[0] = 3;
                    }
                    if ( (result[0] != 0.0) && (area != 0.0)) {
                        do160 = true;
                    } else if (abserr[0] > errSum) {
                        do170 = true;
                    } else if (area == 0.0) {
                        do190 = true;
                    } else {
                        do165 = true;
                    }
                }
            } // if (do150)

            if (do160) {
                if (abserr[0] / Math.abs(result[0]) > errSum / Math.abs(area)) {
                    do170 = true;
                } else {
                    do165 = true;
                }
            } // if (do160)

            if (do165) {
                // Test on divergence
                if ( (ksgn == -1) && (Math.max(Math.abs(result[0]), Math.abs(area)) <= 1.0E-2 * defabs[0])) {} else if ( (1.0E-2 > (result[0] / area))
                        || ( (result[0] / area) > 100.0) || (errSum >= Math.abs(area))) {
                    ier[0] = 6;
                }
                do190 = true;
            } // if (do165)

            if (do170) {
                result[0] = 0.0;
                for (k = 0; k < last; k++) {
                    result[0] = result[0] + rlist[k];
                }
                abserr[0] = errSum;
                do190 = true;
            } // if (do170)

            if (do190) {
                if (ier[0] > 2) {
                    ier[0] = ier[0] - 1;
                }
                if ( (integr == 2) && (omega < 0.0)) {
                    result[0] = -result[0];
                }
            } // if (do190)
            return;
        } // try
        catch (final Exception err) {
            Preferences.debug("dqawoe error: " + err.getMessage(), Preferences.DEBUG_ALGORITHM);
        }
    } // dqawoe

    /**
     * This is the port of the original FORTRAN routine whose header is given below: c***begin prologue dqawse c***date
     * written 800101 (yymmdd) c***revision date 830518 (yymmdd) c***category no. h2a2a1 c***keywords automatic
     * integrator, special-purpose, c algebraico-logarithmic end point singularities, c clenshaw-curtis method
     * c***author piessens,robert,appl. math. & progr. div. - k.u.leuven c de doncker,elise,appl. math. & progr. div. -
     * k.u.leuven c***purpose the routine calculates an approximation result to a given c definite integral i = integral
     * of f*w over (a,b), c (where w shows a singular behaviour at the end points, c see parameter integr). c hopefully
     * satisfying following claim for accuracy c abs(i-result).le.max(epsabs,epsrel*abs(i)). c***description c c
     * integration of functions having algebraico-logarithmic c end point singularities c standard fortran subroutine c
     * double precision version c c parameters c on entry c f - double precision c function subprogram defining the
     * integrand c function f(x). the actual name for f needs to be c declared e x t e r n a l in the driver program. c
     * c a - double precision c lower limit of integration c c b - double precision c upper limit of integration, b.gt.a
     * c if b.le.a, the routine will end with ier = 6. c c alfa - double precision c parameter in the weight function,
     * alfa.gt.(-1) c if alfa.le.(-1), the routine will end with c ier = 6. c c beta - double precision c parameter in
     * the weight function, beta.gt.(-1) c if beta.le.(-1), the routine will end with c ier = 6. c c integr - integer c
     * indicates which weight function is to be used c = 1 (x-a)**alfa*(b-x)**beta c = 2
     * (x-a)**alfa*(b-x)**beta*log(x-a) c = 3 (x-a)**alfa*(b-x)**beta*log(b-x) c = 4
     * (x-a)**alfa*(b-x)**beta*log(x-a)*log(b-x) c if integr.lt.1 or integr.gt.4, the routine c will end with ier = 6. c
     * c epsabs - double precision c absolute accuracy requested c epsrel - double precision c relative accuracy
     * requested c if epsabs.le.0 c and epsrel.lt.max(50*rel.mach.acc.,0.5d-28), c the routine will end with ier = 6. c
     * c limit - integer c gives an upper bound on the number of subintervals c in the partition of (a,b), limit.ge.2 c
     * if limit.lt.2, the routine will end with ier = 6. c c on return c result - double precision c approximation to
     * the integral c c abserr - double precision c estimate of the modulus of the absolute error, c which should equal
     * or exceed abs(i-result) c c neval - integer c number of integrand evaluations c c ier - integer c ier = 0 normal
     * and reliable termination of the c routine. it is assumed that the requested c accuracy has been achieved. c
     * ier.gt.0 abnormal termination of the routine c the estimates for the integral and error c are less reliable. it
     * is assumed that the c requested accuracy has not been achieved. c error messages c = 1 maximum number of
     * subdivisions allowed c has been achieved. one can allow more c subdivisions by increasing the value of c limit.
     * however, if this yields no c improvement, it is advised to analyze the c integrand in order to determine the c
     * integration difficulties which prevent the c requested tolerance from being achieved. c in case of a jump
     * discontinuity or a local c singularity of algebraico-logarithmic type c at one or more interior points of the c
     * integration range, one should proceed by c splitting up the interval at these c points and calling the integrator
     * on the c subranges. c = 2 the occurrence of roundoff error is c detected, which prevents the requested c
     * tolerance from being achieved. c = 3 extremely bad integrand behaviour occurs c at some points of the integration
     * c interval. c = 6 the input is invalid, because c b.le.a or alfa.le.(-1) or beta.le.(-1), or c integr.lt.1 or
     * integr.gt.4, or c (epsabs.le.0 and c epsrel.lt.max(50*rel.mach.acc.,0.5d-28), c or limit.lt.2. c result, abserr,
     * neval, rlist(1), elist(1), c iord(1) and last are set to zero. alist(1) c and blist(1) are set to a and b c
     * respectively. c c alist - double precision c vector of dimension at least limit, the first c last elements of
     * which are the left c end points of the subintervals in the partition c of the given integration range (a,b) c c
     * blist - double precision c vector of dimension at least limit, the first c last elements of which are the right c
     * end points of the subintervals in the partition c of the given integration range (a,b) c c rlist - double
     * precision c vector of dimension at least limit,the first c last elements of which are the integral c
     * approximations on the subintervals c c elist - double precision c vector of dimension at least limit, the first c
     * last elements of which are the moduli of the c absolute error estimates on the subintervals c c iord - integer c
     * vector of dimension at least limit, the first k c of which are pointers to the error c estimates over the
     * subintervals, so that c elist(iord(1)), ..., elist(iord(k)) with k = last c if last.le.(limit/2+2), and k =
     * limit+1-last c otherwise form a decreasing sequence c c last - integer c number of subintervals actually produced
     * in c the subdivision process c c***references (none) c***routines called d1mach,dqc25s,dqmomo,dqpsrt c***end
     * prologue dqawse c
     */
    private void dqawse() {
        // Index to the interval with the largest error estimate
        final int[] maxErr = new int[1];

        // errMax[0] = elist[maxErr];
        final double[] errMax = new double[1];

        // Sum of the integrals over the subintervals
        double area;

        // Sum of the errors over the subintervals
        double errSum;

        // Requested accuracy max(epsabs, epsrel * abs(result))
        double errBnd;

        final double area1[] = new double[1];
        double area12;
        final double area2[] = new double[1];
        double a1;
        double a2;
        double b1;
        double b2;
        double centre;
        final double error1[] = new double[1];
        double erro12;
        final double error2[] = new double[1];
        final double resas1[] = new double[1];
        final double resas2[] = new double[1];
        double rg[];
        double rh[];
        double ri[];
        double rj[];

        int iroff1;
        int iroff2;
        int k;
        final int nev[] = new int[1];
        final int nrmax[] = new int[1];

        try {
            alist = new double[limit];
            blist = new double[limit];
            rlist = new double[limit];
            elist = new double[limit];
            iord = new int[limit];
            rg = new double[25];
            rh = new double[25];
            ri = new double[25];
            rj = new double[25];

            neval[0] = 0;
            last = 0;
            rlist[0] = 0.0;
            elist[0] = 0.0;
            iord[0] = -1;
            result[0] = 0.0;
            abserr[0] = 0.0;
            ier[0] = 0;

            // Compute the modified chebyshev moments

            dqmomo(ri, rj, rg, rh);

            // Integrate over the intervals (lower, (lower+upper)/2) and ((lower+upper)/2, upper).

            centre = 0.5 * (lower + upper);
            dqc25s(lower, centre, ri, rj, rg, rh, area1, error1, resas1, nev);
            neval[0] = nev[0];
            dqc25s(centre, upper, ri, rj, rg, rh, area2, error2, resas2, nev);
            last = 2;
            neval[0] = neval[0] + nev[0];
            result[0] = area1[0] + area2[0];
            abserr[0] = error1[0] + error2[0];

            // Test on accuracy
            errBnd = Math.max(epsabs, epsrel * Math.abs(result[0]));

            // Initialization
            if (error2[0] <= error1[0]) {
                alist[0] = lower;
                alist[1] = centre;
                blist[0] = centre;
                blist[1] = upper;
                rlist[0] = area1[0];
                rlist[1] = area2[0];
                elist[0] = error1[0];
                elist[1] = error2[0];
            } else {
                alist[0] = centre;
                alist[1] = lower;
                blist[0] = upper;
                blist[1] = centre;
                rlist[0] = area2[0];
                rlist[1] = area1[0];
                elist[0] = error2[0];
                elist[1] = error1[0];
            }
            iord[0] = 0;
            iord[1] = 1;
            if (limit == 2) {
                ier[0] = 1;
            }
            if (abserr[0] <= errBnd || ier[0] == 1) {
                return;
            }
            errMax[0] = elist[0];
            maxErr[0] = 0;
            nrmax[0] = 1;
            area = result[0];
            errSum = abserr[0];
            iroff1 = 0;
            iroff2 = 0;

            // Main for loop
            for (last = 3; last <= limit; last++) {

                // bisect the subinterval with largest error estimate.

                a1 = alist[maxErr[0]];
                b1 = 0.5 * (alist[maxErr[0]] + blist[maxErr[0]]);
                a2 = b1;
                b2 = blist[maxErr[0]];

                dqc25s(a1, b1, ri, rj, rg, rh, area1, error1, resas1, nev);
                neval[0] = neval[0] + nev[0];
                dqc25s(a2, b2, ri, rj, rg, rh, area2, error2, resas2, nev);
                neval[0] = neval[0] + nev[0];

                // improve previous approximations integral and error
                // and test for accuracy.

                area12 = area1[0] + area2[0];
                erro12 = error1[0] + error2[0];
                errSum = errSum + erro12 - errMax[0];
                area = area + area12 - rlist[maxErr[0]];
                if ( (lower != a1) && (upper != b2) && (resas1[0] != error1[0]) && (resas2[0] != error2[0])) {

                    // test for roundoff error.

                    if (Math.abs(rlist[maxErr[0]] - area12) < 1.0E-5 * Math.abs(area12) && erro12 >= 0.99 * errMax[0]) {
                        iroff1 = iroff1 + 1;
                    }
                    if (last > 10 && erro12 > errMax[0]) {
                        iroff2 = iroff2 + 1;
                    }
                } // if ((lower != a1) && (upper != b2) &&
                rlist[maxErr[0]] = area1[0];
                rlist[last - 1] = area2[0];

                // test on accuracy.

                errBnd = Math.max(epsabs, epsrel * Math.abs(area));
                if (errSum > errBnd) {

                    // set error flag in the case that the number of interval
                    // bisections exceeds limit.

                    if (last == limit) {
                        ier[0] = 1;
                    }

                    // set error flag in the case of roundoff error.

                    if (iroff1 >= 6 || iroff2 >= 20) {
                        ier[0] = 2;
                    }

                    // set error flag in the case of bad integrand behaviour
                    // at interior points of integration range.

                    if (Math.max(Math.abs(a1), Math.abs(b2)) <= (1.0 + 100.0 * epmach) * (Math.abs(a2) + 1.0E3 * uflow)) {
                        ier[0] = 3;
                    }

                } // if (errSum > errBnd)

                // append the newly-created intervals to the list.

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

                // call subroutine dqpsrt to maintain the descending ordering
                // in the list of error estimates and select the subinterval
                // with largest error estimate (to be bisected next).

                dqpsrt(limit, last, maxErr, errMax, elist, iord, nrmax);
                if (ier[0] != 0 || errSum <= errBnd) {
                    break;
                }
            } // for (last = 3; last <= limit; last++)

            // compute final result.
            // ---------------------

            result[0] = 0.0;
            for (k = 0; k < last; k++) {
                result[0] = result[0] + rlist[k];
            }
            abserr[0] = errSum;
            return;
        } // try
        catch (final Exception err) {
            Preferences.debug("dqawse error: " + err.getMessage(), Preferences.DEBUG_ALGORITHM);
        }
    } // dqawse()

    /**
     * This is the port of the original FORTRAN routine whose header is given below: c***begin prologue dqc25c c***date
     * written 810101 (yymmdd) c***revision date 830518 (yymmdd) c***category no. h2a2a2,j4 c***keywords 25-point
     * clenshaw-curtis integration c***author piessens,robert,appl. math. & progr. div. - k.u.leuven c de
     * doncker,elise,appl. math. & progr. div. - k.u.leuven c***purpose to compute i = integral of f*w over (a,b) with c
     * error estimate, where w(x) = 1/(x-c) c***description c c integration rules for the computation of cauchy c
     * principal value integrals c standard fortran subroutine c double precision version c c c a - double precision c
     * left end point of the integration interval c c b - double precision c right end point of the integration
     * interval, b.gt.a c c c - double precision c parameter in the weight function c c result - double precision c
     * approximation to the integral c result is computed by using a generalized c clenshaw-curtis method if c lies
     * within ten percent c of the integration interval. in the other case the c 15-point kronrod rule obtained by
     * optimal addition c of abscissae to the 7-point gauss rule, is applied. c c abserr - double precision c estimate
     * of the modulus of the absolute error, c which should equal or exceed abs(i-result) c c krul - integer c key which
     * is decreased by 1 if the 15-point c gauss-kronrod scheme has been used c c neval - integer c number of integrand
     * evaluations c c....................................................................... c***references (none)
     * c***routines called dqcheb,dqk15w,dqwgtc c***end prologue dqc25c c
     * 
     * @param a
     * @param b
     * @param c
     * @param result
     * @param abserr
     * @param krul
     * @param neval
     */
    private void dqc25c(final double a, final double b, final double c, final double result[], final double abserr[],
            final int krul[], final int neval[]) {
        double ak22;
        double amom0;
        double amom1;
        double amom2;
        double cc;
        /** mid point of the interval */
        double centr;
        /**
         * chebyshev series expansion of coefficients, for the function f, of degree 12.
         */
        final double cheb12[] = new double[13];
        /**
         * chebyshev series expansion of coefficients, for the function f, of degree 24.
         */
        final double cheb24[] = new double[25];
        /** value of the function f at the points cos(k*pi/24), k = 0, ..., 24 */
        final double fval[] = new double[25];
        /** half-length of the interval */
        double hlgth;
        /** Approximation to the integral corresponding to the use of cheb12 */
        double res12;
        /** Approximation to the integral corresponding to the use of cheb24 */
        double res24;
        double u;
        /** To be used for the chebyshev series expansion of f */
        final double x[] = new double[11];
        int i;
        int isym;
        int k;

        for (k = 1; k <= 11; k++) {
            x[k - 1] = Math.cos(k * Math.PI / 24);
        }

        // Check the position of c
        cc = (2.0 * c - b - a) / (b - a);
        if (Math.abs(cc) >= 1.1) {

            // Apply the 15-point gauss-kronrod scheme.

            krul[0] = krul[0] - 1;
            dqk15w(c, a, b, result, abserr, resabs, resasc);
            neval[0] = 15;
            if (resasc[0] == abserr[0]) {
                krul[0] = krul[0] + 1;
            }
            return;
        } // if (Math.abs(cc) >= 1.1)

        // Use the generalized clenshaw-curtis method.

        hlgth = 0.5 * (b - a);
        centr = 0.5 * (b + a);
        neval[0] = 25;
        if (selfTest) {
            fval[0] = 0.5 * intFuncTest(hlgth + centr);
            fval[12] = intFuncTest(centr);
            fval[24] = 0.5 * intFuncTest(centr - hlgth);
            for (i = 2; i <= 12; i++) {
                u = hlgth * x[i - 2];
                isym = 26 - i;
                fval[i - 1] = intFuncTest(u + centr);
                fval[isym - 1] = intFuncTest(centr - u);
            }
        } // if (selfTest)
        else {
            fval[0] = 0.5 * intFunc(hlgth + centr);
            fval[12] = intFunc(centr);
            fval[24] = 0.5 * intFunc(centr - hlgth);
            for (i = 2; i <= 12; i++) {
                u = hlgth * x[i - 2];
                isym = 26 - i;
                fval[i - 1] = intFunc(u + centr);
                fval[isym - 1] = intFunc(centr - u);
            }
        }

        // Compute the chebyshev series expansion.

        dqcheb(x, fval, cheb12, cheb24);

        // The modified chebyshev moments sare computed by forward
        // recursion, using amom0 and amom1 as starting values.

        amom0 = Math.log(Math.abs( (1.0 - cc) / (1.0 + cc)));
        amom1 = 2.0 + cc * amom0;
        res12 = cheb12[0] * amom0 + cheb12[1] * amom1;
        res24 = cheb24[0] * amom0 + cheb24[1] * amom1;
        for (k = 3; k <= 13; k++) {
            amom2 = 2.0 * cc * amom1 - amom0;
            ak22 = (k - 2) * (k - 2);
            if ( (k / 2) * 2 == k) {
                amom2 = amom2 - 4.0 / (ak22 - 1.0);
            }
            res12 = res12 + cheb12[k - 1] * amom2;
            res24 = res24 + cheb24[k - 1] * amom2;
            amom0 = amom1;
            amom1 = amom2;
        } // for (k = 3; k <= 13; k++)
        for (k = 14; k <= 25; k++) {
            amom2 = 2.0 * cc * amom1 - amom0;
            ak22 = (k - 2) * (k - 2);
            if ( (k / 2) * 2 == k) {
                amom2 = amom2 - 4.0 / (ak22 - 1.0);
            }
            res24 = res24 + cheb24[k - 1] * amom2;
            amom0 = amom1;
            amom1 = amom2;
        } // for (k = 14; k <= 25; k++)
        result[0] = res24;
        abserr[0] = Math.abs(res24 - res12);
        return;
    } // dqc25c

    /**
     * This is a port of the original FORTRAN whose header is given below: c***begin prologue dqc25f c***date written
     * 810101 (yymmdd) c***revision date 830518 (yymmdd) c***category no. h2a2a2 c***keywords integration rules for
     * functions with cos or sin c factor, clenshaw-curtis, gauss-kronrod c***author piessens,robert,appl. math. &
     * progr. div. - k.u.leuven c de doncker,elise,appl. math. & progr. div. - k.u.leuven c***purpose to compute the
     * integral i=integral of f(x) over (a,b) c where w(x) = cos(omega*x) or w(x)=sin(omega*x) and to c compute j =
     * integral of abs(f) over (a,b). for small value c of omega or small intervals (a,b) the 15-point gauss-kronro c
     * rule is used. otherwise a generalized clenshaw-curtis c method is used. c***description c c integration rules for
     * functions with cos or sin factor c standard fortran subroutine c double precision version c c parameters c on
     * entry c f - double precision c function subprogram defining the integrand c function f(x). the actual name for f
     * needs to c be declared e x t e r n a l in the calling program. c c a - double precision c lower limit of
     * integration c c b - double precision c upper limit of integration c c omega - double precision c parameter in the
     * weight function c c integr - integer c indicates which weight function is to be used c integr = 1 w(x) =
     * cos(omega*x) c integr = 2 w(x) = sin(omega*x) c c nrmom - integer c the length of interval (a,b) is equal to the
     * length c of the original integration interval divided by c 2**nrmom (we suppose that the routine is used in an c
     * adaptive integration process, otherwise set c nrmom = 0). nrmom must be zero at the first call. c c maxp1 -
     * integer c gives an upper bound on the number of chebyshev c moments which can be stored, i.e. for the c intervals
     * of lengths abs(bb-aa)*2**(-l), c l = 0,1,2, ..., maxp1-2. c c ksave - integer c key which is one when the moments
     * for the c current interval have been computed c c on return c result - double precision c approximation to the
     * integral i c c abserr - double precision c estimate of the modulus of the absolute c error, which should equal or
     * exceed abs(i-result) c c neval - integer c number of integrand evaluations c c resabs - double precision c
     * approximation to the integral j c c resasc - double precision c approximation to the integral of abs(f-i/(b-a)) c
     * c on entry and return c momcom - integer c for each interval length we need to compute the c chebyshev moments.
     * momcom counts the number of c intervals for which these moments have already been c computed. if nrmom.lt.momcom
     * or ksave = 1, the c chebyshev moments for the interval (a,b) have c already been computed and stored, otherwise
     * we c compute them and we increase momcom. c c chebmo - double precision c array of dimension at least (maxp1,25)
     * containing c the modified chebyshev moments for the first momcom c momcom interval lengths c c
     * ...................................................................... c***references (none) c***routines called
     * d1mach,dgtsl,dqcheb,dqk15w,dqwgtf c***end prologue dqc25f c
     * 
     * @param a
     * @param b
     * @param omega
     * @param nrmom
     * @param ksave
     * @param result
     * @param abserr
     * @param neval
     * @param resabs
     * @param resasc
     */
    private void dqc25f(final double a, final double b, final double omega, final int nrmom, final int ksave,
            final double result[], final double abserr[], final int neval[], final double resabs[],
            final double resasc[]) {
        // mid point of the integration variable
        double centr;

        // half-length of the integration interval
        double hlgth;

        // value of the function f at the points
        // (b-a)*0.5*cos(k*PI/12) + (b+a)*0.5, k = 0, ..., 24.
        final double fval[] = new double[25];

        // coefficients of the chebyshev series expansion of degree 12,
        // for the function f, in the interval (a,b)
        final double cheb12[] = new double[13];

        // coefficients of the chebyshev series expansion of degree 24,
        // for the function f, in the interval (a,b)
        final double cheb24[] = new double[25];

        // approximation to the integral of
        // cos(0.5*(b-a)*omega*x)*f(0.5*(b-a)*x+0.5*(b+a))
        // over (-1,+1) using the chebyshev sereis expansion of degree 12
        double resc12;

        // approximation to the same integral, using the
        // chebyshev seies expansion of degree 24
        double resc24;

        // the analogue of resc12 for the sine
        double ress12;

        // the analogue of resc24 for the sine
        double ress24;

        /** Values cos(k*PI/24) k = 1, ..., 11 to be used for the chebyshev series expansion of f */
        final double x[] = new double[11];

        final double d[] = new double[25];
        final double d1[] = new double[25];
        final double d2[] = new double[25];
        final double v[] = new double[28];
        double vp[];

        double ac;
        double an;
        double an2;
        double as;
        double asap;
        double ass;
        double conc;
        double cons;
        double cospar;
        double estc;
        double ests;
        double parint;
        double par2;
        double par22;
        double sinpar;

        int i;
        final int iers[] = new int[1];
        int isym;
        int j;
        int k;
        int m = 0;
        int noequ = 0;
        int noeq1 = 0;

        for (k = 1; k <= 11; k++) {
            x[k - 1] = Math.cos(k * Math.PI / 24);
        }

        centr = 0.5 * (b + a);
        hlgth = 0.5 * (b - a);
        parint = omega * hlgth;

        // Compute the integral using the 15-point gauss-kronrod
        // formula if the value of the parameter in the integrand
        // is small

        if (Math.abs(parint) <= 2.0) {
            dqk15w(omega, a, b, result, abserr, resabs, resasc);
            neval[0] = 15;
            return;
        } // if (Math.abw(parint) <= 2.0)

        // Compute the integral using the generalized clenshaw-curtis method.

        conc = hlgth * Math.cos(centr * omega);
        cons = hlgth * Math.sin(centr * omega);
        neval[0] = 25;

        // Check whether the chebyshev moments for this interval
        // have already been computed.

        if ( (nrmom >= momcom) && (ksave != 1)) {

            // Compute a new set of chebyshev moments

            m = momcom + 1;
            par2 = parint * parint;
            par22 = par2 + 2.0;
            sinpar = Math.sin(parint);
            cospar = Math.cos(parint);

            // Compute the chebyshev moments with respect to cosine.

            v[0] = 2.0 * sinpar / parint;
            v[1] = (8.0 * cospar + (par2 + par2 - 8.0) * sinpar / parint) / par2;
            v[2] = (32.0 * (par2 - 12.0) * cospar + (2.0 * ( (par2 - 80.0) * par2 + 192.0) * sinpar) / parint)
                    / (par2 * par2);
            ac = 8.0 * cospar;
            as = 24.0 * parint * sinpar;
            if (Math.abs(parint) <= 24.0) {

                // compute the chebyshev moments as the solutions of a
                // boundary value problem with 1 initial value (v(3)) and 1
                // end value (computed using an asymptotic formula).

                noequ = 25;
                noeq1 = noequ - 1;
                an = 6.0;
                for (k = 0; k < noeq1; k++) {
                    an2 = an * an;
                    d[k] = -2.0 * (an2 - 4.0) * (par22 - an2 - an2);
                    d2[k] = (an - 1.0) * (an - 2.0) * par2;
                    d1[k + 1] = (an + 3.0) * (an + 4.0) * par2;
                    v[k + 3] = as - (an2 - 4.0) * ac;
                    an = an + 2.0;
                } // for (k = 0; k < noeq1; k++)
                an2 = an * an;
                d[noequ - 1] = -2.0 * (an2 - 4.0) * (par22 - an2 - an2);
                v[noequ + 2] = as - (an2 - 4.0) * ac;
                v[3] = v[3] - 56.0 * par2 * v[2];
                ass = parint * sinpar;
                asap = ( ( ( ( (210.0 * par2 - 1.0) * cospar - (105.0 * par2 - 63.0) * ass) / an2 - (1.0 - 15.0 * par2)
                        * cospar + 15.0 * ass)
                        / an2 - cospar + 3.0 * ass)
                        / an2 - cospar)
                        / an2;
                v[noequ + 2] = v[noequ + 2] - 2.0 * asap * par2 * (an - 1.0) * (an - 2.0);

                // solve the tridiagonal system by means of gaussian
                // elimination with partial pivoting.
                vp = new double[25];
                for (i = 0; i < 25; i++) {
                    vp[i] = v[i + 3];
                }
                dgtsl(noequ, d1, d, d2, vp, iers);
                for (i = 0; i < 25; i++) {
                    v[i + 3] = vp[i];
                }
            } // if (Math.abs(parint) <= 24.0)
            else { // Math.abs(parint) > 24.0

                // compute the chebyshev moments by means of forward recursion.

                an = 4.0;
                for (i = 3; i < 13; i++) {
                    an2 = an * an;
                    v[i] = ( (an2 - 4.0) * (2.0 * (par22 - an2 - an2) * v[i - 1] - ac) + as - par2 * (an + 1.0)
                            * (an + 2.0) * v[i - 2])
                            / (par2 * (an - 1.0) * (an - 2.0));
                    an = an + 2.0;
                } // for (i = 3; i < 13; i++)
            } // else Math.abs(parint) > 24.0)
            for (j = 1; j <= 13; j++) {
                chebmo[m - 1][2 * j - 2] = v[j - 1];
            }

            // Compute the chebyshev moments with respect to sine

            v[0] = 2.0 * (sinpar - parint * cospar) / par2;
            v[1] = (18.0 - 48.0 / par2) * sinpar / par2 + ( -2.0 + 48.0 / par2) * cospar / parint;
            ac = -24.0 * parint * cospar;
            as = -8.0 * sinpar;
            if (Math.abs(parint) <= 24.0) {

                // compute the chebyshev moments as the solutions of a boundary
                // value problem with 1 initial value (v[1]) and 1 end value
                // (computed using an asymptotic formula).

                an = 5.0;
                for (k = 0; k < noeq1; k++) {
                    an2 = an * an;
                    d[k] = -2.0 * (an2 - 4.0) * (par22 - an2 - an2);
                    d2[k] = (an - 1.0) * (an - 2.0) * par2;
                    d1[k + 1] = (an + 3.0) * (an + 4.0) * par2;
                    v[k + 2] = ac + (an2 - 4.0) * as;
                    an = an + 2.0;
                } // for (k = 0; k < noeq1; k++)
                an2 = an * an;
                d[noequ - 1] = -2.0 * (an2 - 4.0) * (par22 - an2 - an2);
                v[noequ + 1] = ac + (an2 - 4.0) * as;
                v[2] = v[2] - 42.0 * par2 * v[1];
                ass = parint * cospar;
                asap = ( ( ( ( (105.0 * par2 - 63.0) * ass + (210.0 * par2 - 1.0) * sinpar) / an2 + (15.0 * par2 - 1.0)
                        * sinpar - 15.0 * ass)
                        / an2 - 3.0 * ass - sinpar)
                        / an2 - sinpar)
                        / an2;
                v[noequ + 1] = v[noequ + 1] - 2.0 * asap * par2 * (an - 1.0) * (an - 2.0);

                // solve the tridiagonal system by means of gaussian
                // elimination with partial pivoting.

                vp = new double[26];
                for (i = 0; i < 26; i++) {
                    vp[i] = v[i + 2];
                }
                dgtsl(noequ, d1, d, d2, vp, iers);
                for (i = 0; i < 26; i++) {
                    v[i + 2] = vp[i];
                }
            } // if (Math.abs(parint) <= 24.0)
            else { // Math.abs(parint) > 24.0

                // compute the chebyshev moments by means of forward recursion.

                an = 3.0;
                for (i = 2; i < 12; i++) {
                    an2 = an * an;
                    v[i] = ( (an2 - 4.0) * (2.0 * (par22 - an2 - an2) * v[i - 1] + as) + ac - par2 * (an + 1.0)
                            * (an + 2.0) * v[i - 2])
                            / (par2 * (an - 1.0) * (an - 2.0));
                    an = an + 2.0;
                } // for (i = 2; i < 12; i++)
            } // else Math.abs(parint) > 24.0
            for (j = 1; j <= 12; j++) {
                chebmo[m - 1][2 * j - 1] = v[j - 1];
            }
        } // if ((nrmom >= momcom) && (ksave != 1)
        if (nrmom < momcom) {
            m = nrmom + 1;
        }
        if ( (momcom < (maxp1 - 1)) && (nrmom >= momcom)) {
            momcom = momcom + 1;
        }

        // compute the coefficients of the chebyshev expansions
        // of degrees 12 and 24 of the function f.

        if (selfTest) {
            fval[0] = 0.5 * intFuncTest(centr + hlgth);
            fval[12] = intFuncTest(centr);
            fval[24] = 0.5 * intFuncTest(centr - hlgth);
            for (i = 2; i <= 12; i++) {
                isym = 26 - i;
                fval[i - 1] = intFuncTest(hlgth * x[i - 2] + centr);
                fval[isym - 1] = intFuncTest(centr - hlgth * x[i - 2]);
            } // for (i = 2; i <= 12; i++)
        } // if (selfTest)
        else {
            fval[0] = 0.5 * intFunc(centr + hlgth);
            fval[12] = intFunc(centr);
            fval[24] = 0.5 * intFunc(centr - hlgth);
            for (i = 2; i <= 12; i++) {
                isym = 26 - i;
                fval[i - 1] = intFunc(hlgth * x[i - 2] + centr);
                fval[isym - 1] = intFunc(centr - hlgth * x[i - 2]);
            } // for (i = 2; i <= 12; i++)
        }
        dqcheb(x, fval, cheb12, cheb24);

        // compute the integral and error estimates.

        resc12 = cheb12[12] * chebmo[m - 1][12];
        ress12 = 0.0;
        k = 11;
        for (j = 1; j <= 6; j++) {
            resc12 = resc12 + cheb12[k - 1] * chebmo[m - 1][k - 1];
            ress12 = ress12 + cheb12[k] * chebmo[m - 1][k];
            k = k - 2;
        } // for (j = 1; j <= 6; j++)
        resc24 = cheb24[24] * chebmo[m - 1][24];
        ress24 = 0.0;
        resabs[0] = Math.abs(cheb24[24]);
        k = 23;
        for (j = 1; j <= 12; j++) {
            resc24 = resc24 + cheb24[k - 1] * chebmo[m - 1][k - 1];
            ress24 = ress24 + cheb24[k] * chebmo[m - 1][k];
            resabs[0] = Math.abs(cheb24[k - 1]) + Math.abs(cheb24[k]);
            k = k - 2;
        } // for (j = 1; j <= 12; j++)
        estc = Math.abs(resc24 - resc12);
        ests = Math.abs(ress24 - ress12);
        resabs[0] = resabs[0] * Math.abs(hlgth);
        if (integr == 1) {
            result[0] = conc * resc24 - cons * ress24;
            abserr[0] = Math.abs(conc * estc) + Math.abs(cons * ests);
        } else {
            result[0] = conc * ress24 + cons * resc24;
            abserr[0] = Math.abs(conc * ests) + Math.abs(cons * estc);
        }
        return;
    } // dqc25f

    /**
     * This is a port of the original FORTRAN whose header is given below: c***begin prologue dqc25s c***date written
     * 810101 (yymmdd) c***revision date 830518 (yymmdd) c***category no. h2a2a2 c***keywords 25-point clenshaw-curtis
     * integration c***author piessens,robert,appl. math. & progr. div. - k.u.leuven c de doncker,elise,appl. math. &
     * progr. div. - k.u.leuven c***purpose to compute i = integral of f*w over (bl,br), with error c estimate, where
     * the weight function w has a singular c behaviour of algebraico-logarithmic type at the points c a and/or b.
     * (bl,br) is a part of (a,b). c***description c c integration rules for integrands having algebraico-logarithmic c
     * end point singularities c standard fortran subroutine c double precision version c c parameters c f - double
     * precision c function subprogram defining the integrand c f(x). the actual name for f needs to be declared c e x t
     * e r n a l in the driver program. c c a - double precision c left end point of the original interval c c b -
     * double precision c right end point of the original interval, b.gt.a c c bl - double precision c lower limit of
     * integration, bl.ge.a c c br - double precision c upper limit of integration, br.le.b c c alfa - double precision
     * c parameter in the weight function c c beta - double precision c parameter in the weight function c c ri,rj,rg,rh -
     * double precision c modified chebyshev moments for the application c of the generalized clenshaw-curtis c method
     * (computed in subroutine dqmomo) c c result - double precision c approximation to the integral c result is
     * computed by using a generalized c clenshaw-curtis method if b1 = a or br = b. c in all other cases the 15-point
     * kronrod c rule is applied, obtained by optimal addition of c abscissae to the 7-point gauss rule. c c abserr -
     * double precision c estimate of the modulus of the absolute error, c which should equal or exceed abs(i-result) c
     * c resasc - double precision c approximation to the integral of abs(f*w-i/(b-a)) c c integr - integer c which
     * determines the weight function c = 1 w(x) = (x-a)**alfa*(b-x)**beta c = 2 w(x) = (x-a)**alfa*(b-x)**beta*log(x-a)
     * c = 3 w(x) = (x-a)**alfa*(b-x)**beta*log(b-x) c = 4 w(x) = (x-a)**alfa*(b-x)**beta*log(x-a)* c log(b-x) c c nev -
     * integer c number of integrand evaluations c***references (none) c***routines called dqcheb,dqk15w c***end
     * prologue dqc25s c
     * 
     * @param bl
     * @param br
     * @param ri
     * @param rj
     * @param rg
     * @param rh
     * @param result
     * @param abserr
     * @param resasc
     * @param nev
     */
    private void dqc25s(final double bl, final double br, final double ri[], final double rj[], final double rg[],
            final double rh[], final double result[], final double abserr[], final double resasc[], final int nev[]) {
        // mid point of the interval (bl, br)
        double centr;

        // half-length of the interval (bl, br)
        double hlgth = 0.0;

        // value of the function f at the points
        // (br-bl)*0.5*cos(k*PI/24) + (br+bl)*0.5, k = 0, ..., 24.
        final double fval[] = new double[25];

        // coefficients of the chebyshev series expansion of degree 12,
        // for the function f, in the interval (bl,br)
        final double cheb12[] = new double[13];

        // coefficients of the chebyshev series expansion of degree 24,
        // for the function f, in the interval (bl,br)
        final double cheb24[] = new double[25];

        // approximation to the integral obtained from cheb12
        double res12 = 0.0;

        // approximation to the integral obtained from cheb24
        double res24 = 0.0;

        double dc;
        double factor = 0.0;
        double fix = 0.0;
        double u;
        final double resabs[] = new double[1];

        /** Values cos(k*PI/24) k = 1, ..., 11 to be used for the chebyshev series expansion of f */
        final double x[] = new double[11];

        int i;
        int isym;
        int k;

        boolean do5 = false;
        boolean do7 = false;
        boolean do10 = false;
        boolean do25 = false;
        boolean do45 = false;
        boolean do70 = false;
        boolean do105 = false;
        boolean do130 = false;
        boolean do140 = false;
        boolean do155 = false;
        boolean do175 = false;
        boolean do200 = false;
        boolean do235 = false;
        boolean do260 = false;

        for (k = 1; k <= 11; k++) {
            x[k - 1] = Math.cos(k * Math.PI / 24);
        }

        nev[0] = 25;
        if (bl == lower && (alfa != 0.0 || integr == 2 || integr == 4)) {
            do10 = true;
        } else {
            do5 = true;
        }
        if (do5) {
            if (br == upper && (beta != 0.0 || integr == 3 || integr == 4)) {
                do140 = true;
            } else {
                do7 = true;
            }
        } // if (do5)
        if (do7) {
            // If lower > bl and upper < br, apply the 15-point gauss-kronrod scheme.

            dqk15w(lower, bl, br, result, abserr, resabs, resasc);
            nev[0] = 15;
            return;
        } // if (do7)
        if (do10) {

            // this part of the program is executed only if a = bl.
            // ----------------------------------------------------

            // compute the chebyshev series expansion of the following function
            // f1 = (0.5*(b+b-br-a)-0.5*(br-a)*x)**beta*f(0.5*(br-a)*x+0.5*(br+a))

            hlgth = 0.5 * (br - bl);
            centr = 0.5 * (br + bl);
            fix = upper - centr;
            if (selfTest) {
                fval[0] = 0.5 * intFuncTest(hlgth + centr) * Math.pow( (fix - hlgth), beta);
                fval[12] = intFuncTest(centr) * Math.pow(fix, beta);
                fval[24] = 0.5 * intFuncTest(centr - hlgth) * Math.pow( (fix + hlgth), beta);
                for (i = 2; i <= 12; i++) {
                    u = hlgth * x[i - 2];
                    isym = 26 - i;
                    fval[i - 1] = intFuncTest(u + centr) * Math.pow( (fix - u), beta);
                    fval[isym - 1] = intFuncTest(centr - u) * Math.pow( (fix + u), beta);
                } // for (i = 2; i <= 12; i++)
            } // if (selfTest)
            else {
                fval[0] = 0.5 * intFunc(hlgth + centr) * Math.pow( (fix - hlgth), beta);
                fval[12] = intFunc(centr) * Math.pow(fix, beta);
                fval[24] = 0.5 * intFunc(centr - hlgth) * Math.pow( (fix + hlgth), beta);
                for (i = 2; i <= 12; i++) {
                    u = hlgth * x[i - 2];
                    isym = 26 - i;
                    fval[i - 1] = intFunc(u + centr) * Math.pow( (fix - u), beta);
                    fval[isym - 1] = intFunc(centr - u) * Math.pow( (fix + u), beta);
                } // for (i = 2; i <= 12; i++)
            } // else
            factor = Math.pow(hlgth, (alfa + 1.0));
            result[0] = 0.0;
            abserr[0] = 0.0;
            res12 = 0.0;
            res24 = 0.0;
            if (integr > 2) {
                do70 = true;
            } else {
                do25 = true;
            }
        } // if (do10)
        if (do25) {
            dqcheb(x, fval, cheb12, cheb24);

            // integr = 1 (or 2)

            for (i = 0; i <= 12; i++) {
                res12 = res12 + cheb12[i] * ri[i];
                res24 = res24 + cheb24[i] * ri[i];
            } // for (i = 0; i <= 12; i++)
            for (i = 13; i <= 24; i++) {
                res24 = res24 + cheb24[i] * ri[i];
            }
            if (integr == 1) {
                do130 = true;
            } else {
                do45 = true;
            }
        } // if (do25)
        if (do45) {

            // integr = 2

            dc = Math.log(br - bl);
            result[0] = res24 * dc;
            abserr[0] = Math.abs( (res24 - res12) * dc);
            res12 = 0.0;
            res24 = 0.0;
            for (i = 0; i <= 12; i++) {
                res12 = res12 + cheb12[i] * rg[i];
                res24 = res12 + cheb24[i] * rg[i];
            } // for (i = 0; i <= 12; i++)
            for (i = 13; i <= 24; i++) {
                res24 = res24 + cheb24[i] * rg[i];
            }
            do130 = true;
        } // if (do45)

        if (do70) {

            // compute the chebyshev series expansion of the following function
            // f4 = f1*log(0.5*(b+b-br-a)-0.5*(br-a)*x)

            fval[0] = fval[0] * Math.log(fix - hlgth);
            fval[12] = fval[12] * Math.log(fix);
            fval[24] = fval[24] * Math.log(fix + hlgth);
            for (i = 2; i <= 12; i++) {
                u = hlgth * x[i - 2];
                isym = 26 - i;
                fval[i - 1] = fval[i - 1] * Math.log(fix - u);
                fval[isym - 1] = fval[isym - 1] * Math.log(fix + u);
            } // for (i = 2; i <= 12; i++)
            dqcheb(x, fval, cheb12, cheb24);

            // integr = 3 (or 4)

            for (i = 0; i <= 12; i++) {
                res12 = res12 + cheb12[i] * ri[i];
                res24 = res24 + cheb24[i] * ri[i];
            } // for (i = 0; i <= 12; i++)
            for (i = 13; i <= 24; i++) {
                res24 = res24 + cheb24[i] * ri[i];
            }
            if (integr == 3) {
                do130 = true;
            } else {
                do105 = true;
            }
        } // if (do70)

        if (do105) {

            // integr = 4

            dc = Math.log(br - bl);
            result[0] = res24 * dc;
            abserr[0] = Math.abs( (res24 - res12) * dc);
            res12 = 0.0;
            res24 = 0.0;
            for (i = 0; i <= 12; i++) {
                res12 = res12 + cheb12[i] * rg[i];
                res24 = res24 + cheb24[i] * rg[i];
            } // for (i = 0; i <= 12; i++)
            for (i = 13; i <= 24; i++) {
                res24 = res24 + cheb24[i] * rg[i];
            }
            do130 = true;
        } // if (do105
        if (do130) {
            result[0] = (result[0] + res24) * factor;
            abserr[0] = (abserr[0] + Math.abs(res24 - res12)) * factor;
            return;
        } // if (do130)

        if (do140) {

            // this part of the program is executed only if b = br.
            // ----------------------------------------------------

            // compute the chebyshev series expansion of the following function
            // f2 = (0.5*(b+bl-a-a)+0.5*(b-bl)*x)**alfa*f(0.5*(b-bl)*x+0.5*(b+bl))

            hlgth = 0.5 * (br - bl);
            centr = 0.5 * (br + bl);
            fix = centr - lower;
            if (selfTest) {
                fval[0] = 0.5 * intFuncTest(hlgth + centr) * Math.pow( (fix + hlgth), alfa);
                fval[12] = intFuncTest(centr) * Math.pow(fix, alfa);
                fval[24] = 0.5 * intFuncTest(centr - hlgth) * Math.pow( (fix - hlgth), alfa);
                for (i = 2; i <= 12; i++) {
                    u = hlgth * x[i - 2];
                    isym = 26 - i;
                    fval[i - 1] = intFuncTest(u + centr) * Math.pow( (fix + u), alfa);
                    fval[isym - 1] = intFuncTest(centr - u) * Math.pow( (fix - u), alfa);
                } // for (i = 2; i <= 12; i++)
            } // if (selfTest)
            else {
                fval[0] = 0.5 * intFunc(hlgth + centr) * Math.pow( (fix + hlgth), alfa);
                fval[12] = intFunc(centr) * Math.pow(fix, alfa);
                fval[24] = 0.5 * intFunc(centr - hlgth) * Math.pow( (fix - hlgth), alfa);
                for (i = 2; i <= 12; i++) {
                    u = hlgth * x[i - 2];
                    isym = 26 - i;
                    fval[i - 1] = intFunc(u + centr) * Math.pow( (fix + u), alfa);
                    fval[isym - 1] = intFunc(centr - u) * Math.pow( (fix - u), alfa);
                } // for (i = 2; i <= 12; i++)
            } // else
            factor = Math.pow(hlgth, (beta + 1.0));
            result[0] = 0.0;
            abserr[0] = 0.0;
            res12 = 0.0;
            res24 = 0.0;
            if (integr == 2 || integr == 4) {
                do200 = true;
            } else {
                do155 = true;
            }
        } // if (do140)

        if (do155) {

            // integr = 1 (or 3)

            dqcheb(x, fval, cheb12, cheb24);
            for (i = 0; i <= 12; i++) {
                res12 = res12 + cheb12[i] * rj[i];
                res24 = res24 + cheb24[i] * rj[i];
            } // for (i = 0; i <= 12; i++)
            for (i = 13; i <= 24; i++) {
                res24 = res24 + cheb24[i] * rj[i];
            }
            if (integr == 1) {
                do260 = true;
            } else {
                do175 = true;
            }
        } // if (do155)

        if (do175) {

            // integr = 3

            dc = Math.log(br - bl);
            result[0] = res24 * dc;
            abserr[0] = Math.abs( (res24 - res12) * dc);
            res12 = 0.0;
            res24 = 0.0;
            for (i = 0; i <= 12; i++) {
                res12 = res12 + cheb12[i] * rh[i];
                res24 = res24 + cheb24[i] * rh[i];
            } // for (i = 0; i <= 12; i++)
            for (i = 13; i <= 24; i++) {
                res24 = res24 + cheb24[i] * rh[i];
            }
            do260 = true;
        } // if (do175)

        if (do200) {

            // compute the chebyshev series expansion of the following function
            // f3 = f2*log(0.5*(b-bl)*x+0.5*(b+bl-a-a))

            fval[0] = fval[0] * Math.log(hlgth + fix);
            fval[12] = fval[12] * Math.log(fix);
            fval[24] = fval[24] * Math.log(fix - hlgth);
            for (i = 2; i <= 12; i++) {
                u = hlgth * x[i - 2];
                isym = 26 - i;
                fval[i - 1] = fval[i - 1] * Math.log(u + fix);
                fval[isym - 1] = fval[isym - 1] * Math.log(fix - u);
            } // for (i = 2; i <= 12; i++)
            dqcheb(x, fval, cheb12, cheb24);

            // integr = 2 (or 4)

            for (i = 0; i <= 12; i++) {
                res12 = res12 + cheb12[i] * rj[i];
                res24 = res24 + cheb24[i] * rj[i];
            } // for (i = 0; i <= 12; i++)
            for (i = 13; i <= 24; i++) {
                res24 = res24 + cheb24[i] * rj[i];
            }
            if (integr == 2) {
                do260 = true;
            } else {
                do235 = true;
            }
        } // if (do200)

        if (do235) {
            dc = Math.log(br - bl);
            result[0] = res24 * dc;
            abserr[0] = Math.abs( (res24 - res12) * dc);
            res12 = 0.0;
            res24 = 0.0;

            // integr = 4

            for (i = 0; i <= 12; i++) {
                res12 = res12 + cheb12[i] * rh[i];
                res24 = res24 + cheb24[i] * rh[i];
            } // for (i = 0; i <= 12; i++)
            for (i = 13; i <= 24; i++) {
                res24 = res24 + cheb24[i] * rh[i];
            }
            do260 = true;
        } // if (do235)

        if (do260) {
            result[0] = (result[0] + res24) * factor;
            abserr[0] = (abserr[0] + Math.abs(res24 - res12)) * factor;
        } // if (do260)
        return;
    } // dqc25s

    /**
     * This is a port of the original FORTRAN whose header is given below: c***begin prologue dqcheb c***refer to
     * dqc25c,dqc25f,dqc25s c***routines called (none) c***revision date 830518 (yymmdd) c***keywords chebyshev series
     * expansion, fast fourier transform c***author piessens,robert,appl. math. & progr. div. - k.u.leuven c de
     * doncker,elise,appl. math. & progr. div. - k.u.leuven c***purpose this routine computes the chebyshev series
     * expansion c of degrees 12 and 24 of a function using a c fast fourier transform method c f(x) = sum(k=1,..,13)
     * (cheb12(k)*t(k-1,x)), c f(x) = sum(k=1,..,25) (cheb24(k)*t(k-1,x)), c where t(k,x) is the chebyshev polynomial of
     * degree k. c***description c c chebyshev series expansion c standard fortran subroutine c double precision version
     * c c parameters c on entry c x - double precision c vector of dimension 11 containing the c values cos(k*pi/24), k =
     * 1, ..., 11 c c fval - double precision c vector of dimension 25 containing the c function values at the points c
     * (b+a+(b-a)*cos(k*pi/24))/2, k = 0, ...,24, c where (a,b) is the approximation interval. c fval(1) and fval(25)
     * are divided by two c (these values are destroyed at output). c c on return c cheb12 - double precision c vector
     * of dimension 13 containing the c chebyshev coefficients for degree 12 c c cheb24 - double precision c vector of
     * dimension 25 containing the c chebyshev coefficients for degree 24 c c***end prologue dqcheb c
     * 
     * @param x
     * @param fval
     * @param cheb12
     * @param cheb24
     */
    private void dqcheb(final double x[], final double fval[], final double cheb12[], final double cheb24[]) {
        double alam;
        double alam1;
        double alam2;
        double part1;
        double part2;
        double part3;
        final double v[] = new double[12];
        int i;
        int j;

        for (i = 1; i <= 12; i++) {
            j = 26 - i;
            v[i - 1] = fval[i - 1] - fval[j - 1];
            fval[i - 1] = fval[i - 1] + fval[j - 1];
        }
        alam1 = v[0] - v[8];
        alam2 = x[5] * (v[2] - v[6] - v[10]);
        cheb12[3] = alam1 + alam2;
        cheb12[9] = alam1 - alam2;
        alam1 = v[1] - v[7] - v[9];
        alam2 = v[3] - v[5] - v[11];
        alam = x[2] * alam1 + x[8] * alam2;
        cheb24[3] = cheb12[3] + alam;
        cheb24[21] = cheb12[3] - alam;
        alam = x[8] * alam1 - x[2] * alam2;
        cheb24[9] = cheb12[9] + alam;
        cheb24[15] = cheb12[9] - alam;
        part1 = x[3] * v[4];
        part2 = x[7] * v[8];
        part3 = x[5] * v[6];
        alam1 = v[0] + part1 + part2;
        alam2 = x[1] * v[2] + part3 + x[9] * v[10];
        cheb12[1] = alam1 + alam2;
        cheb12[11] = alam1 - alam2;
        alam = x[0] * v[1] + x[2] * v[3] + x[4] * v[5] + x[6] * v[7] + x[8] * v[9] + x[10] * v[11];
        cheb24[1] = cheb12[1] + alam;
        cheb24[23] = cheb12[1] - alam;
        alam = x[10] * v[1] - x[8] * v[3] + x[6] * v[5] - x[4] * v[7] + x[2] * v[9] - x[0] * v[11];
        cheb24[11] = cheb12[11] + alam;
        cheb24[13] = cheb12[11] - alam;
        alam1 = v[0] - part1 + part2;
        alam2 = x[9] * v[2] - part3 + x[1] * v[10];
        cheb12[5] = alam1 + alam2;
        cheb12[7] = alam1 - alam2;
        alam = x[4] * v[1] - x[8] * v[3] - x[0] * v[5] - x[10] * v[7] + x[2] * v[9] + x[6] * v[11];
        cheb24[5] = cheb12[5] + alam;
        cheb24[19] = cheb12[5] - alam;
        alam = x[6] * v[1] - x[2] * v[3] - x[10] * v[5] + x[0] * v[7] - x[8] * v[9] - x[4] * v[11];
        cheb24[7] = cheb12[7] + alam;
        cheb24[17] = cheb12[7] - alam;
        for (i = 1; i <= 6; i++) {
            j = 14 - i;
            v[i - 1] = fval[i - 1] - fval[j - 1];
            fval[i - 1] = fval[i - 1] + fval[j - 1];
        }
        alam1 = v[0] + x[7] * v[4];
        alam2 = x[3] * v[2];
        cheb12[2] = alam1 + alam2;
        cheb12[10] = alam1 - alam2;
        cheb12[6] = v[0] - v[4];
        alam = x[1] * v[1] + x[5] * v[3] + x[9] * v[5];
        cheb24[2] = cheb12[2] + alam;
        cheb24[22] = cheb12[2] - alam;
        alam = x[5] * (v[1] - v[3] - v[5]);
        cheb24[6] = cheb12[6] + alam;
        cheb24[18] = cheb12[6] - alam;
        alam = x[9] * v[1] - x[5] * v[3] + x[1] * v[5];
        cheb24[10] = cheb12[10] + alam;
        cheb24[14] = cheb12[10] - alam;
        for (i = 1; i <= 3; i++) {
            j = 8 - i;
            v[i - 1] = fval[i - 1] - fval[j - 1];
            fval[i - 1] = fval[i - 1] + fval[j - 1];
        }
        cheb12[4] = v[0] + x[7] * v[2];
        cheb12[8] = fval[0] - x[7] * fval[2];
        alam = x[3] * v[1];
        cheb24[4] = cheb12[4] + alam;
        cheb24[20] = cheb12[4] - alam;
        alam = x[7] * fval[1] - fval[3];
        cheb24[8] = cheb12[8] + alam;
        cheb24[16] = cheb12[8] - alam;
        cheb12[0] = fval[0] + fval[2];
        alam = fval[1] + fval[3];
        cheb24[0] = cheb12[0] + alam;
        cheb24[24] = cheb12[0] - alam;
        cheb12[12] = v[0] - v[2];
        cheb24[12] = cheb12[12];
        alam = 1.0 / 6.0;
        for (i = 1; i <= 11; i++) {
            cheb12[i] = cheb12[i] * alam;
        }
        alam = 0.5 * alam;
        cheb12[0] = cheb12[0] * alam;
        cheb12[12] = cheb12[12] * alam;
        for (i = 1; i <= 23; i++) {
            cheb24[i] = cheb24[i] * alam;
        }
        cheb24[0] = 0.5 * alam * cheb24[0];
        cheb24[24] = 0.5 * alam * cheb24[24];
        return;
    } // dqcheb

    /**
     * This is a port of the original FORTRAN whose header is given below: c***begin prologue dqelg c***refer to
     * dqagie,dqagoe,dqagpe,dqagse c***routines called d1mach c***revision date 830518 (yymmdd) c***keywords epsilon
     * algorithm, convergence acceleration, c extrapolation c***author piessens,robert,appl. math. & progr. div. -
     * k.u.leuven c de doncker,elise,appl. math & progr. div. - k.u.leuven c***purpose the routine determines the limit
     * of a given sequence of c approximations, by means of the epsilon algorithm of c p.wynn. an estimate of the
     * absolute error is also given. c the condensed epsilon table is computed. only those c elements needed for the
     * computation of the next diagonal c are preserved. c***description c c epsilon algorithm c standard fortran
     * subroutine c double precision version
     * 
     * This routine determines the limit of a given sequence of approximations, by means of the epsilon algorithm of P.
     * Wynn. An estimate of the absolute error is also given. The condensed epsilon table is computed. Only those
     * elements needed for the computation of the next diagonal are preserved.
     * 
     * @param n epstab[n[0]-1] contains the new element in the first column of the epsilon table
     * @param epstab 52 element array containing the elements of the two lower diagonals of the triangular epsilon
     *            table. The elements are numbered starting at the right-hand corner of the triangle
     * @param result resulting approximation to the integral The element in the new diagonal with the least value of
     *            error.
     * @param abserr estimate of the absolute error computed from result and the 3 previous results
     * @param res31a array containing the last 3 results
     * @param nres number of calls to the routine (should be zero at first call)
     */
    private void dqelg(final int[] n, final double[] epstab, final double[] result, final double[] abserr,
            final double[] res31a, final int[] nres) {
        double delta1;
        double delta2;
        double delta3;
        double epsinf;

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

            if ( (err2 <= tol2) && (err3 <= tol3)) {

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

            if ( (err1 > tol1) && (err2 > tol2) && (err3 > tol3)) {
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

        if ( ( (num / 2) * 2) == num) {
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
     * The is a port of the original FORTRAN whose header is given below: c***begin prologue dqk15 c***date written
     * 800101 (yymmdd) c***revision date 830518 (yymmdd) c***category no. h2a1a2 c***keywords 15-point gauss-kronrod
     * rules c***author piessens,robert,appl. math. & progr. div. - k.u.leuven c de doncker,elise,appl. math. & progr.
     * div - k.u.leuven c***purpose to compute i = integral of f over (a,b), with error c estimate c j = integral of
     * abs(f) over (a,b) c***description c c integration rules c standard fortran subroutine c double precision version
     * 
     * This routine computes theintegral of the intFunc over (a, b) and the integral of the abs(intFunc) over (a, b).
     * 
     * @param a lower limit for integration
     * @param b upper limit for integration
     * @param result Approximation to the integral. The result is computed by applying the 15-point kronrod rule (resk)
     *            obtained by optimal addition of abscissae to the 7-point gauss rule (resg).
     * @param abserr Estimate of the modulus of the absolute error, which should equal or exceed abs(integral of intFunc
     *            over (a,b) - result)
     * @param resabs Approximation to integral of abs(intFunc)
     * @param resasc Approximation to the integral of abs(intFunc - integral of intFunc/(b - a)) over (a, b)
     */
    private void dqk15(final double a, final double b, final double[] result, final double[] abserr,
            final double[] resabs, final double[] resasc) {
        // The abscissae and weights are supplied for the interval (-1, 1).
        // Because of symmetry only the positive abscissae and their
        // corresponding weights are given.

        // xgk - abscissae of the 15-point kronrod rule
        // xgk[1], xgk[3], ... abscissae of the 7-point gauss rule
        // xgk[0], xgk[2], ... abscissae which are optimally added to the
        // 7-point gauss rule.
        final double[] xgk = new double[] {0.991455371120812639206854697526329, 0.949107912342758524526189684047851,
                0.864864423359769072789712788640926, 0.741531185599394439863864773280788,
                0.586087235467691130294144838258730, 0.405845151377397166906606412076961,
                0.207784955007898467600689403773245, 0.000000000000000000000000000000000};

        // wgk - weights of the 15-point kronrod rule
        final double[] wgk = new double[] {0.022935322010529224963732008058970, 0.063092092629978553290700663189204,
                0.104790010322250183839876322541518, 0.140653259715525918745189590510238,
                0.169004726639267902826583426598550, 0.190350578064785409913256402421014,
                0.204432940075298892414161999234649, 0.209482141084727828012999174891714};

        // wg - weights of the 7-point gauss rule, corresponding to the
        // abscissae xgk[1], xgk[3], ...
        // wg[0], wg[2], ... are set to zero.
        final double[] wg = new double[] {0.0, 0.129484966168869693270611432679082, 0.0,
                0.279705391489276667901467771423780, 0.0, 0.381830050505118944950369775488975, 0.0,
                0.417959183673469387755102040816327};

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
        final double[] fv1 = new double[7];
        final double[] fv2 = new double[7];

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
        } else {
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
            } else {
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
            } else {
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
        abserr[0] = Math.abs( (resk - resg) * hlgth);

        if ( (resasc[0] != 0.0) && (abserr[0] != 0.0)) {
            abserr[0] = resasc[0] * Math.min(1.0, Math.pow(200.0 * abserr[0] / resasc[0], 1.5));
        }

        if (resabs[0] > (uflow / (50.0 * epmach))) {
            abserr[0] = Math.max( (50.0 * epmach) * resabs[0], abserr[0]);
        }

        return;
    }

    /**
     * This is a port of the original FORTRAN whose header is given below: c***begin prologue dqk15i c***date written
     * 800101 (yymmdd) c***revision date 830518 (yymmdd) c***category no. h2a3a2,h2a4a2 c***keywords 15-point
     * transformed gauss-kronrod rules c***author piessens,robert,appl. math. & progr. div. - k.u.leuven c de
     * doncker,elise,appl. math. & progr. div. - k.u.leuven c***purpose the original (infinite integration range is
     * mapped c onto the interval (0,1) and (a,b) is a part of (0,1). c it is the purpose to compute c i = integral of
     * transformed integrand over (a,b), c j = integral of abs(transformed integrand) over (a,b). c***description c c
     * integration rule c standard fortran subroutine c double precision version
     * 
     * The original infinite integration range is mapped onto the interval (0, 1) and (a, b) is a part of (0, 1). This
     * routine computes the integral of the transformed integrand over (a, b) and the integral of the abs(transformed
     * integrand) over (a, b).
     * 
     * @param boun finite bound of original integration range (set to zero if inf = +2)
     * @param inf If inf = -1, the original interval is (-infinity, bound) If inf = +1, the original interval is (bound,
     *            +infinity) If inf = +2, the original interval is (-infinity, +infinity) and the integral is computed
     *            as the sum of two integrals, one over (-infinity, 0) and one over (0, +infinity)
     * @param a lower limit for integration over subrange of (0, 1)
     * @param b upper limit for integration over subrange of (0, 1)
     * @param result Approximation to the integral. The result is computed by applying the 15-point kronrod rule (resk)
     *            obtained by optimal addition of abscissae to the 7-point gauss rule (resg).
     * @param abserr Estimate of the modulus of the absolute error, which should equal or exceed abs(integral of
     *            transformed integrand - result)
     * @param resabs Approximation to integral of abs(transformed integrand)
     * @param resasc Approximation to the integral of abs(transformed integrand - integral of transformed integrand/(b -
     *            a)) over (a, b)
     */
    private void dqk15i(final double boun, final int inf, final double a, final double b, final double[] result,
            final double[] abserr, final double[] resabs, final double[] resasc) {
        // The abscissae and weights are supplied for the interval (-1, 1).
        // Because of symmetry only the positive abscissae and their
        // corresponding weights are given.

        // xgk - abscissae of the 15-point kronrod rule
        // xgk[1], xgk[3], ... abscissae of the 7-point gauss rule
        // xgk[0], xgk[2], ... abscissae which are optimally added to the
        // 7-point gauss rule.
        final double[] xgk = new double[] {0.991455371120812639206854697526329, 0.949107912342758524526189684047851,
                0.864864423359769072789712788640926, 0.741531185599394439863864773280788,
                0.586087235467691130294144838258730, 0.405845151377397166906606412076961,
                0.207784955007898467600689403773245, 0.000000000000000000000000000000000};

        // wgk - weights of the 15-point kronrod rule
        final double[] wgk = new double[] {0.022935322010529224963732008058970, 0.063092092629978553290700663189204,
                0.104790010322250183839876322541518, 0.140653259715525918745189590510238,
                0.169004726639267902826583426598550, 0.190350578064785409913256402421014,
                0.204432940075298892414161999234649, 0.209482141084727828012999174891714};

        // wg - weights of the 7-point gauss rule, corresponding to the
        // abscissae xgk[1], xgk[3], ...
        // wg[0], wg[2], ... are set to zero.
        final double[] wg = new double[] {0.0, 0.129484966168869693270611432679082, 0.0,
                0.279705391489276667901467771423780, 0.0, 0.381830050505118944950369775488975, 0.0,
                0.417959183673469387755102040816327};

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
        final double[] fv1 = new double[7];
        final double[] fv2 = new double[7];

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
        } else {
            fval1 = intFunc(tabsc1);
        }

        if (inf == 2) {
            if (selfTest) {
                fval1 = fval1 + intFuncTest( -tabsc1);
            } else {
                fval1 = fval1 + intFunc( -tabsc1);
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
                    fval1 = fval1 + intFuncTest( -tabsc1);
                    fval2 = fval2 + intFuncTest( -tabsc2);
                } // if (inf == 2)
            } // if (selfTest)
            else {
                fval1 = intFunc(tabsc1);
                fval2 = intFunc(tabsc2);

                if (inf == 2) {
                    fval1 = fval1 + intFunc( -tabsc1);
                    fval2 = fval2 + intFunc( -tabsc2);
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
        abserr[0] = Math.abs( (resk - resg) * hlgth);

        if ( (resasc[0] != 0.0) && (abserr[0] != 0.0)) {
            abserr[0] = resasc[0] * Math.min(1.0, Math.pow(200.0 * abserr[0] / resasc[0], 1.5));
        } // if ((resasc[0] != 0.0) && (abserr[0] != 0.0))

        if (resabs[0] > (uflow / (50.0 * epmach))) {
            abserr[0] = Math.max( (50.0 * epmach) * resabs[0], abserr[0]);
        } // if (resabs[0] > uflow/(50.0 * epmach))

        return;
    }

    /**
     * This a a port of the original FORTRAN whose header is given below: c***begin prologue dqk15w c***date written
     * 810101 (yymmdd) c***revision date 830518 (mmddyy) c***category no. h2a2a2 c***keywords 15-point gauss-kronrod
     * rules c***author piessens,robert,appl. math. & progr. div. - k.u.leuven c de doncker,elise,appl. math. & progr.
     * div. - k.u.leuven c***purpose to compute i = integral of f*w over (a,b), with error c estimate c j = integral of
     * abs(f*w) over (a,b) c***description c c integration rules c standard fortran subroutine c double precision
     * version c c parameters c on entry c c p1, p2, p3, p4 - double precision c parameters in the weight function c c
     * kp - integer c key for indicating the type of weight function c c a - double precision c lower limit of
     * integration c c b - double precision c upper limit of integration c c on return c result - double precision c
     * approximation to the integral i c result is computed by applying the 15-point c kronrod rule (resk) obtained by
     * optimal addition c of abscissae to the 7-point gauss rule (resg). c c abserr - double precision c estimate of the
     * modulus of the absolute error, c which should equal or exceed abs(i-result) c c resabs - double precision c
     * approximation to the integral of abs(f) c c resasc - double precision c approximation to the integral of
     * abs(f-i/(b-a)) c
     * 
     * @param p1
     * @param kp
     * @param a
     * @param b
     * @param result
     * @param abserr
     * @param resabs
     * @param resasc
     */
    private void dqk15w(final double p1, final double a, final double b, final double[] result, final double[] abserr,
            final double[] resabs, final double[] resasc) {
        /**
         * The abscissa and weights are given for the interval (-1,1). Because of symmetry only the positive abscissae
         * and their corresponding weights are given.
         */
        /** abscissa */
        double absc;
        double absc1;
        double absc2;
        /** mid point of the inverval */
        double centr;
        double dhlgth;
        double fc;
        double fsum;
        /** function value */
        double fval1;
        double fval2;
        final double fv1[] = new double[7];
        final double fv2[] = new double[7];
        /** half_length of the interval */
        double hlgth;
        /** result of the 7-point gauss formula */
        double resg;
        /** result of the 15-point kronrod formula */
        double resk;
        /**
         * approximation to the mean value of f*w over (a,b), i.e. to i/(b-a)
         */
        double reskh;
        /** Weights of the 7-point gauss rule */
        final double wg[] = new double[] {0.1294849661688697, 0.2797053914892767, 0.3818300505051889,
                0.4179591836734694};
        /** Weights of the 15-point gauss-kronrod rule */
        final double wgk[] = new double[] {0.02293532201052922, 0.06309209262997855, 0.1047900103222502,
                0.1406532597155259, 0.1690047266392679, 0.1903505780647854, 0.2044329400752989, 0.2094821410847278};
        /**
         * abscissae of the 15-point gauss-kronrod rule xgk[1], xgk[3], ... abscissae of the 7-point gauss rule. xgk[0],
         * xgk[2], ... abscissae which are optimally added to the 7-point gauss rule.
         */
        final double xgk[] = new double[] {0.9914553711208126, 0.9491079123427585, 0.8648644233597691,
                0.7415311855993944, 0.5860872354676911, 0.4058451513773972, 0.2077849550078985, 0.0000000000000000};
        double xma;
        double bmx;

        int j;
        int jtw;
        int jtwm1;

        centr = 0.5 * (a + b);
        hlgth = 0.5 * (b - a);
        dhlgth = Math.abs(hlgth);

        // Compute the 15-point kronrod approximation to the
        // integral, and estimate the error.
        if (routine == Integration2.DQAWCE) {
            if (selfTest) {
                fc = intFuncTest(centr) / (centr - p1);
            } else {
                fc = intFunc(centr) / (centr - p1);
            }
        } // if (routine == DQAWCE)
        else if ( (routine == Integration2.DQAWFE) || (routine == Integration2.DQAWOE)) {
            if (integr == 1) {
                if (selfTest) {
                    fc = intFuncTest(centr) * Math.cos(centr * p1);
                } else {
                    fc = intFunc(centr) * Math.cos(centr * p1);
                }
            } // if (integr == 1)
            else { // integr == 2
                if (selfTest) {
                    fc = intFuncTest(centr) * Math.sin(centr * p1);
                } else {
                    fc = intFunc(centr) * Math.sin(centr * p1);
                }
            } // else integr == 2
        } // else if ((routine == DQAWFE) || (routine == DQAWOE))
        else { // routine == DQAWSE
            if (integr == 1) {
                if (selfTest) {
                    xma = centr - lower;
                    bmx = upper - centr;
                    fc = intFuncTest(centr) * Math.pow(xma, alfa) * Math.pow(bmx, beta);
                } else {
                    xma = centr - lower;
                    bmx = upper - centr;
                    fc = intFunc(centr) * Math.pow(xma, alfa) * Math.pow(bmx, beta);
                }
            } // if (integr == 1)
            else if (integr == 2) {
                if (selfTest) {
                    xma = centr - lower;
                    bmx = upper - centr;
                    fc = intFuncTest(centr) * Math.pow(xma, alfa) * Math.pow(bmx, beta) * Math.log(xma);
                } else {
                    xma = centr - lower;
                    bmx = upper - centr;
                    fc = intFunc(centr) * Math.pow(xma, alfa) * Math.pow(bmx, beta) * Math.log(xma);
                }
            } // else if (integr == 2)
            else if (integr == 3) {
                if (selfTest) {
                    xma = centr - lower;
                    bmx = upper - centr;
                    fc = intFuncTest(centr) * Math.pow(xma, alfa) * Math.pow(bmx, beta) * Math.log(bmx);
                } else {
                    xma = centr - lower;
                    bmx = upper - centr;
                    fc = intFunc(centr) * Math.pow(xma, alfa) * Math.pow(bmx, beta) * Math.log(bmx);
                }
            } // else if (integr == 3)
            else { // integr == 4
                if (selfTest) {
                    xma = centr - lower;
                    bmx = upper - centr;
                    fc = intFuncTest(centr) * Math.pow(xma, alfa) * Math.pow(bmx, beta) * Math.log(xma) * Math.log(bmx);
                } else {
                    xma = centr - lower;
                    bmx = upper - centr;
                    fc = intFunc(centr) * Math.pow(xma, alfa) * Math.pow(bmx, beta) * Math.log(xma) * Math.log(bmx);
                }
            } // integr == 4
        } // else routine == DQAWSE
        resg = wg[3] * fc;
        resk = wgk[7] * fc;
        resabs[0] = Math.abs(resk);
        for (j = 1; j <= 3; j++) {
            jtw = 2 * j;
            absc = hlgth * xgk[jtw - 1];
            absc1 = centr - absc;
            absc2 = centr + absc;
            if (routine == Integration2.DQAWCE) {
                if (selfTest) {
                    fval1 = intFuncTest(absc1) / (absc1 - p1);
                    fval2 = intFuncTest(absc2) / (absc2 - p1);
                } else {
                    fval1 = intFunc(absc1) / (absc1 - p1);
                    fval2 = intFunc(absc2) / (absc2 - p1);
                }
            } // if (routine == DQAWCE)
            else if ( (routine == Integration2.DQAWFE) || (routine == Integration2.DQAWOE)) {
                if (integr == 1) {
                    if (selfTest) {
                        fval1 = intFuncTest(absc1) * Math.cos(absc1 * p1);
                        fval2 = intFuncTest(absc2) * Math.cos(absc2 * p1);
                    } else {
                        fval1 = intFunc(absc1) * Math.cos(absc1 * p1);
                        fval2 = intFunc(absc2) * Math.cos(absc2 * p1);
                    }
                } // if (integr == 1)
                else { // integr == 2
                    if (selfTest) {
                        fval1 = intFuncTest(absc1) * Math.sin(absc1 * p1);
                        fval2 = intFuncTest(absc2) * Math.sin(absc2 * p1);
                    } else {
                        fval1 = intFunc(absc1) * Math.sin(absc1 * p1);
                        fval2 = intFunc(absc2) * Math.sin(absc2 * p1);
                    }
                }
            } // else if ((routine == DQAWFE) || (routine == DQAWOE))
            else { // routine == DQAWSE
                if (integr == 1) {
                    if (selfTest) {
                        xma = absc1 - lower;
                        bmx = upper - absc1;
                        fval1 = intFuncTest(absc1) * Math.pow(xma, alfa) * Math.pow(bmx, beta);
                        xma = absc2 - lower;
                        bmx = upper - absc2;
                        fval2 = intFuncTest(absc2) * Math.pow(xma, alfa) * Math.pow(bmx, beta);
                    } else {
                        xma = absc1 - lower;
                        bmx = upper - absc1;
                        fval1 = intFunc(absc1) * Math.pow(xma, alfa) * Math.pow(bmx, beta);
                        xma = absc2 - lower;
                        bmx = upper - absc2;
                        fval2 = intFunc(absc2) * Math.pow(xma, alfa) * Math.pow(bmx, beta);
                    }
                } // if (integr == 1)
                else if (integr == 2) {
                    if (selfTest) {
                        xma = absc1 - lower;
                        bmx = upper - absc1;
                        fval1 = intFuncTest(absc1) * Math.pow(xma, alfa) * Math.pow(bmx, beta) * Math.log(xma);
                        xma = absc2 - lower;
                        bmx = upper - absc2;
                        fval2 = intFuncTest(absc2) * Math.pow(xma, alfa) * Math.pow(bmx, beta) * Math.log(xma);
                    } else {
                        xma = absc1 - lower;
                        bmx = upper - absc1;
                        fval1 = intFunc(absc1) * Math.pow(xma, alfa) * Math.pow(bmx, beta) * Math.log(xma);
                        xma = absc2 - lower;
                        bmx = upper - absc2;
                        fval2 = intFunc(absc2) * Math.pow(xma, alfa) * Math.pow(bmx, beta) * Math.log(xma);
                    }
                } // else if (integr == 2)
                else if (integr == 3) {
                    if (selfTest) {
                        xma = absc1 - lower;
                        bmx = upper - absc1;
                        fval1 = intFuncTest(absc1) * Math.pow(xma, alfa) * Math.pow(bmx, beta) * Math.log(bmx);
                        xma = absc2 - lower;
                        bmx = upper - absc2;
                        fval2 = intFuncTest(absc2) * Math.pow(xma, alfa) * Math.pow(bmx, beta) * Math.log(bmx);
                    } else {
                        xma = absc1 - lower;
                        bmx = upper - absc1;
                        fval1 = intFunc(absc1) * Math.pow(xma, alfa) * Math.pow(bmx, beta) * Math.log(bmx);
                        xma = absc2 - lower;
                        bmx = upper - absc2;
                        fval2 = intFunc(absc2) * Math.pow(xma, alfa) * Math.pow(bmx, beta) * Math.log(bmx);
                    }
                } // else if (integr == 3)
                else { // integr == 4
                    if (selfTest) {
                        xma = absc1 - lower;
                        bmx = upper - absc1;
                        fval1 = intFuncTest(absc1) * Math.pow(xma, alfa) * Math.pow(bmx, beta) * Math.log(xma)
                                * Math.log(bmx);
                        xma = absc2 - lower;
                        bmx = upper - absc2;
                        fval2 = intFuncTest(absc2) * Math.pow(xma, alfa) * Math.pow(bmx, beta) * Math.log(xma)
                                * Math.log(bmx);
                    } else {
                        xma = absc1 - lower;
                        bmx = upper - absc1;
                        fval1 = intFunc(absc1) * Math.pow(xma, alfa) * Math.pow(bmx, beta) * Math.log(xma)
                                * Math.log(bmx);
                        xma = absc2 - lower;
                        bmx = upper - absc2;
                        fval2 = intFunc(absc2) * Math.pow(xma, alfa) * Math.pow(bmx, beta) * Math.log(xma)
                                * Math.log(bmx);
                    }
                } // integr == 4
            } // else routine == DQAWSE
            fv1[jtw - 1] = fval1;
            fv2[jtw - 1] = fval2;
            fsum = fval1 + fval2;
            resg = resg + wg[j - 1] * fsum;
            resk = resk + wgk[jtw - 1] * fsum;
            resabs[0] = resabs[0] + wgk[jtw - 1] * (Math.abs(fval1) + Math.abs(fval2));
        } // for (j = 1; j <= 3; j++)
        for (j = 1; j <= 4; j++) {
            jtwm1 = 2 * j - 1;
            absc = hlgth * xgk[jtwm1 - 1];
            absc1 = centr - absc;
            absc2 = centr + absc;
            if (routine == Integration2.DQAWCE) {
                if (selfTest) {
                    fval1 = intFuncTest(absc1) / (absc1 - p1);
                    fval2 = intFuncTest(absc2) / (absc2 - p1);
                } else {
                    fval1 = intFunc(absc1) / (absc1 - p1);
                    fval2 = intFunc(absc2) / (absc2 - p1);
                }
            } // if (routine == DQAWCE)
            else if ( (routine == Integration2.DQAWFE) || (routine == Integration2.DQAWOE)) {
                if (integr == 1) {
                    if (selfTest) {
                        fval1 = intFuncTest(absc1) * Math.cos(absc1 * p1);
                        fval2 = intFuncTest(absc2) * Math.cos(absc2 * p1);
                    } else {
                        fval1 = intFunc(absc1) * Math.cos(absc1 * p1);
                        fval2 = intFunc(absc2) * Math.cos(absc2 * p1);
                    }
                } // if (integr == 1)
                else { // integr == 2
                    if (selfTest) {
                        fval1 = intFuncTest(absc1) * Math.sin(absc1 * p1);
                        fval2 = intFuncTest(absc2) * Math.sin(absc2 * p1);
                    } else {
                        fval1 = intFunc(absc1) * Math.sin(absc1 * p1);
                        fval2 = intFunc(absc2) * Math.sin(absc2 * p1);
                    }
                }
            } // else if ((routine == DQAWFE) || (routine == DQAWOE))
            else { // routine == DQAWSE
                if (integr == 1) {
                    if (selfTest) {
                        xma = absc1 - lower;
                        bmx = upper - absc1;
                        fval1 = intFuncTest(absc1) * Math.pow(xma, alfa) * Math.pow(bmx, beta);
                        xma = absc2 - lower;
                        bmx = upper - absc2;
                        fval2 = intFuncTest(absc2) * Math.pow(xma, alfa) * Math.pow(bmx, beta);
                    } else {
                        xma = absc1 - lower;
                        bmx = upper - absc1;
                        fval1 = intFunc(absc1) * Math.pow(xma, alfa) * Math.pow(bmx, beta);
                        xma = absc2 - lower;
                        bmx = upper - absc2;
                        fval2 = intFunc(absc2) * Math.pow(xma, alfa) * Math.pow(bmx, beta);
                    }
                } // if (integr == 1)
                else if (integr == 2) {
                    if (selfTest) {
                        xma = absc1 - lower;
                        bmx = upper - absc1;
                        fval1 = intFuncTest(absc1) * Math.pow(xma, alfa) * Math.pow(bmx, beta) * Math.log(xma);
                        xma = absc2 - lower;
                        bmx = upper - absc2;
                        fval2 = intFuncTest(absc2) * Math.pow(xma, alfa) * Math.pow(bmx, beta) * Math.log(xma);
                    } else {
                        xma = absc1 - lower;
                        bmx = upper - absc1;
                        fval1 = intFunc(absc1) * Math.pow(xma, alfa) * Math.pow(bmx, beta) * Math.log(xma);
                        xma = absc2 - lower;
                        bmx = upper - absc2;
                        fval2 = intFunc(absc2) * Math.pow(xma, alfa) * Math.pow(bmx, beta) * Math.log(xma);
                    }
                } // else if (integr == 2)
                else if (integr == 3) {
                    if (selfTest) {
                        xma = absc1 - lower;
                        bmx = upper - absc1;
                        fval1 = intFuncTest(absc1) * Math.pow(xma, alfa) * Math.pow(bmx, beta) * Math.log(bmx);
                        xma = absc2 - lower;
                        bmx = upper - absc2;
                        fval2 = intFuncTest(absc2) * Math.pow(xma, alfa) * Math.pow(bmx, beta) * Math.log(bmx);
                    } else {
                        xma = absc1 - lower;
                        bmx = upper - absc1;
                        fval1 = intFunc(absc1) * Math.pow(xma, alfa) * Math.pow(bmx, beta) * Math.log(bmx);
                        xma = absc2 - lower;
                        bmx = upper - absc2;
                        fval2 = intFunc(absc2) * Math.pow(xma, alfa) * Math.pow(bmx, beta) * Math.log(bmx);
                    }
                } // else if (integr == 3)
                else { // integr == 4
                    if (selfTest) {
                        xma = absc1 - lower;
                        bmx = upper - absc1;
                        fval1 = intFuncTest(absc1) * Math.pow(xma, alfa) * Math.pow(bmx, beta) * Math.log(xma)
                                * Math.log(bmx);
                        xma = absc2 - lower;
                        bmx = upper - absc2;
                        fval2 = intFuncTest(absc2) * Math.pow(xma, alfa) * Math.pow(bmx, beta) * Math.log(xma)
                                * Math.log(bmx);
                    } else {
                        xma = absc1 - lower;
                        bmx = upper - absc1;
                        fval1 = intFunc(absc1) * Math.pow(xma, alfa) * Math.pow(bmx, beta) * Math.log(xma)
                                * Math.log(bmx);
                        xma = absc2 - lower;
                        bmx = upper - absc2;
                        fval2 = intFunc(absc2) * Math.pow(xma, alfa) * Math.pow(bmx, beta) * Math.log(xma)
                                * Math.log(bmx);
                    }
                } // integr == 4
            } // else routine == DQAWSE
            fv1[jtwm1 - 1] = fval1;
            fv2[jtwm1 - 1] = fval2;
            fsum = fval1 + fval2;
            resk = resk + wgk[jtwm1 - 1] * fsum;
            resabs[0] = resabs[0] + wgk[jtwm1 - 1] * (Math.abs(fval1) + Math.abs(fval2));
        } // for (j = 1; j <= 4; j++)
        reskh = 0.5 * resk;
        resasc[0] = wgk[7] * Math.abs(fc - reskh);
        for (j = 0; j < 7; j++) {
            resasc[0] = resasc[0] + wgk[j] * (Math.abs(fv1[j] - reskh) + Math.abs(fv2[j] - reskh));
        }
        result[0] = resk * hlgth;
        resabs[0] = resabs[0] * dhlgth;
        resasc[0] = resasc[0] * dhlgth;
        abserr[0] = Math.abs( (resk - resg) * hlgth);
        if ( (resasc[0] != 0.0) && (abserr[0] != 0.0)) {
            abserr[0] = resasc[0] * Math.min(1.0, Math.pow(200.0 * abserr[0] / resasc[0], 1.5));
        }
        if (resabs[0] > uflow / (50.0 * epmach)) {
            abserr[0] = Math.max( (50.0 * epmach) * resabs[0], abserr[0]);
        }
        return;
    } // dqk15w

    /**
     * This is a port of the original FORTRAN whose header is given below: c***begin prologue dqk21 c***date written
     * 800101 (yymmdd) c***revision date 830518 (yymmdd) c***category no. h2a1a2 c***keywords 21-point gauss-kronrod
     * rules c***author piessens,robert,appl. math. & progr. div. - k.u.leuven c de doncker,elise,appl. math. & progr.
     * div. - k.u.leuven c***purpose to compute i = integral of f over (a,b), with error c estimate c j = integral of
     * abs(f) over (a,b) c***description c c integration rules c standard fortran subroutine c double precision version
     * 
     * Computes integral of function over (a,b), with error estimate and computes integral of abs(function) over (a,b).
     * 
     * @param a lower limit of integration
     * @param b upper limit of integration
     * @param result Approximation to the integral. Result is computed by applying the 21-point kronrod rule (resk)
     *            obtained by optimal addition of abscissae to the 10-point gauss rule (resg).
     * @param abserr Estimate of the modulus of the absolute error, which should not exceed abs(actual integral -
     *            result)
     * @param resabs Approximation to the integral of abs(function) over (a,b).
     * @param resasc Approximation to the integral of abs(function - actual integral/(b-a)) over (a,b)
     */
    private void dqk21(final double a, final double b, final double[] result, final double[] abserr,
            final double[] resabs, final double[] resasc) {

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
        final double[] xgk = new double[] {0.995657163025808080735527280689003, 0.973906528517171720077964012084452,
                0.930157491355708226001207180059508, 0.865063366688984510732096688423493,
                0.780817726586416897063717578345042, 0.679409568299024406234327365114874,
                0.562757134668604683339000099272694, 0.433395394129247190799265943165784,
                0.294392862701460198131126603103866, 0.148874338981631210884826001129720,
                0.000000000000000000000000000000000};

        // wgk - weights of the 21-point kronrod rule
        final double[] wgk = new double[] {0.011694638867371874278064396062192, 0.032558162307964727478818972459390,
                0.054755896574351996031381300244580, 0.075039674810919952767043140916190,
                0.093125454583697605535065465083366, 0.109387158802297641899210590325805,
                0.123491976262065851077958109831074, 0.134709217311473325928054001771707,
                0.142775938577060080797094273138717, 0.147739104901338491374841515972068,
                0.149445554002916905664936468389821};

        // wg - weights of the 10-point gauss rule
        final double[] wg = new double[] {0.066671344308688137593568809893332, 0.149451349150580593145776339657697,
                0.219086362515982043995534934228163, 0.269266719309996355091226921569469,
                0.295524224714752870173892994651338};

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
        final double[] fv1 = new double[10];
        final double[] fv2 = new double[10];
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
        } else {
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
            } else {
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
            } else {
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
        abserr[0] = Math.abs( (resk - resg) * hlgth);

        if ( (resasc[0] != 0.0) && (abserr[0] != 0.0)) {
            abserr[0] = resasc[0] * Math.min(1.0, Math.pow(200.0 * abserr[0] / resasc[0], 1.5));
        }

        if (resabs[0] > (uflow / (50.0 * epmach))) {
            abserr[0] = Math.max( (50.0 * epmach) * resabs[0], abserr[0]);
        }

        return;
    }

    /**
     * This is a port of the original FORTRAN whose header is given below: c***begin prologue dqk31 c***date written
     * 800101 (yymmdd) c***revision date 830518 (yymmdd) c***category no. h2a1a2 c***keywords 31-point gauss-kronrod
     * rules c***author piessens,robert,appl. math. & progr. div. - k.u.leuven c de doncker,elise,appl. math. & progr.
     * div. - k.u.leuven c***purpose to compute i = integral of f over (a,b) with error c estimate c j = integral of
     * abs(f) over (a,b) c***description c c integration rules c standard fortran subroutine c double precision version
     * 
     * This routine computes theintegral of the intFunc over (a, b) and the integral of the abs(intFunc) over (a, b).
     * 
     * @param a lower limit for integration
     * @param b upper limit for integration
     * @param result Approximation to the integral. The result is computed by applying the 31-point kronrod rule (resk)
     *            obtained by optimal addition of abscissae to the 15-point gauss rule (resg).
     * @param abserr Estimate of the modulus of the absolute error, which should equal or exceed abs(integral of intFunc
     *            over (a,b) - result)
     * @param resabs Approximation to integral of abs(intFunc)
     * @param resasc Approximation to the integral of abs(intFunc - integral of intFunc/(b - a)) over (a, b)
     */
    private void dqk31(final double a, final double b, final double[] result, final double[] abserr,
            final double[] resabs, final double[] resasc) {
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
        final double[] xgk = new double[] {0.998002298693397060285172840152271, 0.987992518020485428489565718586613,
                0.967739075679139134257347978784337, 0.937273392400705904307758947710209,
                0.897264532344081900882509656454496, 0.848206583410427216200648320774217,
                0.790418501442465932967649294817947, 0.724417731360170047416186054613938,
                0.650996741297416970533735895313275, 0.570972172608538847537226737253911,
                0.485081863640239680693655740232351, 0.394151347077563369897207370981045,
                0.299180007153168812166780024266389, 0.201194093997434522300628303394596,
                0.101142066918717499027074231447392, 0.000000000000000000000000000000000};

        // wgk - weights of the 31-point kronrod rule
        final double[] wgk = new double[] {0.005377479872923348987792051430128, 0.015007947329316122538374763075807,
                0.025460847326715320186874001019653, 0.035346360791375846222037948478360,
                0.044589751324764876608227299373280, 0.053481524690928087265343147239430,
                0.062009567800670640285139230960803, 0.069854121318728258709520077099147,
                0.076849680757720378894432777482659, 0.083080502823133021038289247286104,
                0.088564443056211770647275443693774, 0.093126598170825321225486872747346,
                0.096642726983623678505179907627589, 0.099173598721791959332393173484603,
                0.100769845523875595044946662617570, 0.101330007014791549017374792767493};

        // wg - weights of the 15-point gauss rule
        final double[] wg = new double[] {0.030753241996117268354628393577204, 0.070366047488108124709267416450667,
                0.107159220467171935011869546685869, 0.139570677926154314447804794511028,
                0.166269205816993933553200860481209, 0.186161000015562211026800561866423,
                0.198431485327111576456118326443839, 0.202578241925561272880620199967519};

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
        final double[] fv1 = new double[15];
        final double[] fv2 = new double[15];

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
        } else {
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
            } else {
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
            } else {
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
        abserr[0] = Math.abs( (resk - resg) * hlgth);

        if ( (resasc[0] != 0.0) && (abserr[0] != 0.0)) {
            abserr[0] = resasc[0] * Math.min(1.0, Math.pow(200.0 * abserr[0] / resasc[0], 1.5));
        }

        if (resabs[0] > (uflow / (50.0 * epmach))) {
            abserr[0] = Math.max( (50.0 * epmach) * resabs[0], abserr[0]);
        }

        return;
    }

    /**
     * This is a port of the original FORTRAN routine whose header is given below: c***begin prologue dqk41 c***date
     * written 800101 (yymmdd) c***revision date 830518 (yymmdd) c***category no. h2a1a2 c***keywords 41-point
     * gauss-kronrod rules c***author piessens,robert,appl. math. & progr. div. - k.u.leuven c de doncker,elise,appl.
     * math. & progr. div. - k.u.leuven c***purpose to compute i = integral of f over (a,b), with error c estimate c j =
     * integral of abs(f) over (a,b) c***description c c integration rules c standard fortran subroutine c double
     * precision version
     * 
     * This routine computes theintegral of the intFunc over (a, b) and the integral of the abs(intFunc) over (a, b).
     * 
     * @param a lower limit for integration
     * @param b upper limit for integration
     * @param result Approximation to the integral. The result is computed by applying the 41-point kronrod rule (resk)
     *            obtained by optimal addition of abscissae to the 20-point gauss rule (resg).
     * @param abserr Estimate of the modulus of the absolute error, which should equal or exceed abs(integral of intFunc
     *            over (a,b) - result)
     * @param resabs Approximation to integral of abs(intFunc)
     * @param resasc Approximation to the integral of abs(intFunc - integral of intFunc/(b - a)) over (a, b)
     */
    private void dqk41(final double a, final double b, final double[] result, final double[] abserr,
            final double[] resabs, final double[] resasc) {
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
        final double[] xgk = new double[] {0.998859031588277663838315576545863, 0.993128599185094924786122388471320,
                0.981507877450250259193342994720217, 0.963971927277913791267666131197277,
                0.940822633831754753519982722212443, 0.912234428251325905867752441203298,
                0.878276811252281976077442995113078, 0.839116971822218823394529061701521,
                0.795041428837551198350638833272788, 0.746331906460150792614305070355642,
                0.693237656334751384805490711845932, 0.636053680726515025452836696226286,
                0.575140446819710315342946036586425, 0.510867001950827098004364050955251,
                0.443593175238725103199992213492640, 0.373706088715419560672548177024927,
                0.301627868114913004320555356858592, 0.227785851141645078080496195368575,
                0.152605465240922675505220241022678, 0.076526521133497333754640409398838,
                0.000000000000000000000000000000000};

        // wgk - weights of the 41-point kronrod rule
        final double[] wgk = new double[] {0.003073583718520531501218293246031, 0.008600269855642942198661787950102,
                0.014626169256971252983787960308868, 0.020388373461266523598010231432755,
                0.025882133604951158834505067096153, 0.031287306777032798958543119323801,
                0.036600169758200798030557240707211, 0.041668873327973686263788305936895,
                0.046434821867497674720231880926108, 0.050944573923728691932707670050345,
                0.055195105348285994744832372419777, 0.059111400880639572374967220648594,
                0.062653237554781168025870122174255, 0.065834597133618422111563556969398,
                0.068648672928521619345623411885368, 0.071054423553444068305790361723210,
                0.073030690332786667495189417658913, 0.074582875400499188986581418362488,
                0.075704497684556674659542775376617, 0.076377867672080736705502835038061,
                0.076600711917999656445049901530102};

        // wg - weights of the 20-point gauss rule
        final double[] wg = new double[] {0.017614007139152118311861962351853, 0.040601429800386941331039952274932,
                0.062672048334109063569506535187042, 0.083276741576704748724758143222046,
                0.101930119817240435036750135480350, 0.118194531961518417312377377711382,
                0.131688638449176626898494499748163, 0.142096109318382051329298325067165,
                0.149172986472603746787828737001969, 0.152753387130725850698084331955098};

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
        final double[] fv1 = new double[20];
        final double[] fv2 = new double[20];

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
        } else {
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
            } else {
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
            } else {
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
        abserr[0] = Math.abs( (resk - resg) * hlgth);

        if ( (resasc[0] != 0.0) && (abserr[0] != 0.0)) {
            abserr[0] = resasc[0] * Math.min(1.0, Math.pow(200.0 * abserr[0] / resasc[0], 1.5));
        }

        if (resabs[0] > (uflow / (50.0 * epmach))) {
            abserr[0] = Math.max( (50.0 * epmach) * resabs[0], abserr[0]);
        }

        return;
    }

    /**
     * This is a port of the original FORTRAN routine whose header is given below: c***begin prologue dqk51 c***date
     * written 800101 (yymmdd) c***revision date 830518 (yymmdd) c***category no. h2a1a2 c***keywords 51-point
     * gauss-kronrod rules c***author piessens,robert,appl. math. & progr. div. - k.u.leuven c de doncker,elise,appl.
     * math & progr. div. - k.u.leuven c***purpose to compute i = integral of f over (a,b) with error c estimate c j =
     * integral of abs(f) over (a,b) c***description c c integration rules c standard fortran subroutine c double
     * precision version
     * 
     * This routine computes theintegral of the intFunc over (a, b) and the integral of the abs(intFunc) over (a, b).
     * 
     * @param a lower limit for integration
     * @param b upper limit for integration
     * @param result Approximation to the integral. The result is computed by applying the 51-point kronrod rule (resk)
     *            obtained by optimal addition of abscissae to the 25-point gauss rule (resg).
     * @param abserr Estimate of the modulus of the absolute error, which should equal or exceed abs(integral of intFunc
     *            over (a,b) - result)
     * @param resabs Approximation to integral of abs(intFunc)
     * @param resasc Approximation to the integral of abs(intFunc - integral of intFunc/(b - a)) over (a, b)
     */
    private void dqk51(final double a, final double b, final double[] result, final double[] abserr,
            final double[] resabs, final double[] resasc) {
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
        final double[] xgk = new double[] {0.999262104992609834193457486540341, 0.995556969790498097908784946893902,
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
                0.061544483005685078886546392366797, 0.000000000000000000000000000000000};

        // wgk - weights of the 51-point kronrod rule
        final double[] wgk = new double[] {0.001987383892330315926507851882843, 0.005561932135356713758040236901066,
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
                0.061580818067832935078759824240066};

        // wg - weights of the 25-point gauss rule
        final double[] wg = new double[] {0.011393798501026287947902964113235, 0.026354986615032137261901815295299,
                0.040939156701306312655623487711646, 0.054904695975835191925936891540473,
                0.068038333812356917207187185656708, 0.080140700335001018013234959669111,
                0.091028261982963649811497220702892, 0.100535949067050644202206890392686,
                0.108519624474263653116093957050117, 0.114858259145711648339325545869556,
                0.119455763535784772228178126512901, 0.122242442990310041688959518945852,
                0.123176053726715451203902873079050};

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
        final double[] fv1 = new double[25];
        final double[] fv2 = new double[25];

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
        } else {
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
            } else {
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
            } else {
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
        abserr[0] = Math.abs( (resk - resg) * hlgth);

        if ( (resasc[0] != 0.0) && (abserr[0] != 0.0)) {
            abserr[0] = resasc[0] * Math.min(1.0, Math.pow(200.0 * abserr[0] / resasc[0], 1.5));
        }

        if (resabs[0] > (uflow / (50.0 * epmach))) {
            abserr[0] = Math.max( (50.0 * epmach) * resabs[0], abserr[0]);
        }

        return;
    }

    /**
     * This is a port of the original FORTRAN routine whose header is given below: c***begin prologue dqk61 c***date
     * written 800101 (yymmdd) c***revision date 830518 (yymmdd) c***category no. h2a1a2 c***keywords 61-point
     * gauss-kronrod rules c***author piessens,robert,appl. math. & progr. div. - k.u.leuven c de doncker,elise,appl.
     * math. & progr. div. - k.u.leuven c***purpose to compute i = integral of f over (a,b) with error c estimate c j =
     * integral of dabs(f) over (a,b) c***description c c integration rule c standard fortran subroutine c double
     * precision version
     * 
     * This routine computes the integral of the intFunc over (a, b) and the integral of the abs(intFunc) over (a, b).
     * 
     * @param a lower limit for integration
     * @param b upper limit for integration
     * @param result Approximation to the integral. The result is computed by applying the 61-point kronrod rule (resk)
     *            obtained by optimal addition of abscissae to the 30-point gauss rule (resg).
     * @param abserr Estimate of the modulus of the absolute error, which should equal or exceed abs(integral of intFunc
     *            over (a,b) - result)
     * @param resabs Approximation to integral of abs(intFunc)
     * @param resasc Approximation to the integral of abs(intFunc - integral of intFunc/(b - a)) over (a, b)
     */
    private void dqk61(final double a, final double b, final double[] result, final double[] abserr,
            final double[] resabs, final double[] resasc) {
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
        final double[] xgk = new double[] {0.999484410050490637571325895705811, 0.996893484074649540271630050918695,
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
                0.000000000000000000000000000000000};

        // wgk - weights of the 61-point kronrod rule
        final double[] wgk = new double[] {0.001389013698677007624551591226760, 0.003890461127099884051267201844516,
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
                0.051494729429451567558340433647099};

        // wg - weights of the 30-point gauss rule
        final double[] wg = new double[] {0.007968192496166605615465883474674, 0.018466468311090959142302131912047,
                0.028784707883323369349719179611292, 0.038799192569627049596801936446348,
                0.048402672830594052902938140422808, 0.057493156217619066481721689402056,
                0.065974229882180495128128515115962, 0.073755974737705206268243850022191,
                0.080755895229420215354694938460530, 0.086899787201082979802387530715126,
                0.092122522237786128717632707087619, 0.096368737174644259639468626351810,
                0.099593420586795267062780282103569, 0.101762389748405504596428952168554,
                0.102852652893558840341285636705415};

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
        final double[] fv1 = new double[30];
        final double[] fv2 = new double[30];

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
        } else {
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
            } else {
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
            } else {
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
        abserr[0] = Math.abs( (resk - resg) * hlgth);

        if ( (resasc[0] != 0.0) && (abserr[0] != 0.0)) {
            abserr[0] = resasc[0] * Math.min(1.0, Math.pow(200.0 * abserr[0] / resasc[0], 1.5));
        }

        if (resabs[0] > (uflow / (50.0 * epmach))) {
            abserr[0] = Math.max( (50.0 * epmach) * resabs[0], abserr[0]);
        }

        return;
    }

    /**
     * This is a port of the original FORTRAN routine whose header is given below: c***begin prologue dqmomo c***date
     * written 820101 (yymmdd) c***revision date 830518 (yymmdd) c***category no. h2a2a1,c3a2 c***keywords modified
     * chebyshev moments c***author piessens,robert,appl. math. & progr. div. - k.u.leuven c de doncker,elise,appl.
     * math. & progr. div. - k.u.leuven c***purpose this routine computes modified chebsyshev moments. the k-th c
     * modified chebyshev moment is defined as the integral over c (-1,1) of w(x)*t(k,x), where t(k,x) is the chebyshev
     * c polynomial of degree k. c***description c c modified chebyshev moments c standard fortran subroutine c double
     * precision version c c parameters c alfa - double precision c parameter in the weight function w(x), alfa.gt.(-1)
     * c c beta - double precision c parameter in the weight function w(x), beta.gt.(-1) c c ri - double precision c
     * vector of dimension 25 c ri(k) is the integral over (-1,1) of c (1+x)**alfa*t(k-1,x), k = 1, ..., 25. c c rj -
     * double precision c vector of dimension 25 c rj(k) is the integral over (-1,1) of c (1-x)**beta*t(k-1,x), k = 1,
     * ..., 25. c c rg - double precision c vector of dimension 25 c rg(k) is the integral over (-1,1) of c
     * (1+x)**alfa*log((1+x)/2)*t(k-1,x), k = 1, ..., 25. c c rh - double precision c vector of dimension 25 c rh(k) is
     * the integral over (-1,1) of c (1-x)**beta*log((1-x)/2)*t(k-1,x), k = 1, ..., 25. c c integr - integer c input
     * parameter indicating the modified c moments to be computed c integr = 1 compute ri, rj c = 2 compute ri, rj, rg c =
     * 3 compute ri, rj, rh c = 4 compute ri, rj, rg, rh c c***references (none) c***routines called (none) c***end
     * prologue dqmomo c
     * 
     * @param ri
     * @param rj
     * @param rg
     * @param rh
     */
    private void dqmomo(final double ri[], final double rj[], final double rg[], final double rh[]) {
        double alfp1;
        double alfp2;
        double an;
        double anm1;
        double betp1;
        double betp2;
        double ralf;
        double rbet;

        int i;
        int im1;

        alfp1 = alfa + 1.0;
        betp1 = beta + 1.0;
        alfp2 = alfa + 2.0;
        betp2 = beta + 2.0;
        ralf = Math.pow(2.0, alfp1);
        rbet = Math.pow(2.0, betp1);

        // Compute ri, rj using a forward recurrence relation

        ri[0] = ralf / alfp1;
        rj[0] = rbet / betp1;
        ri[1] = ri[0] * alfa / alfp2;
        rj[1] = rj[0] * beta / betp2;
        an = 2.0;
        anm1 = 1.0;
        for (i = 2; i <= 24; i++) {
            ri[i] = - (ralf + an * (an - alfp2) * ri[i - 1]) / (anm1 * (an + alfp1));
            rj[i] = - (rbet + an * (an - betp2) * rj[i - 1]) / (anm1 * (an + betp1));
            anm1 = an;
            an = an + 1.0;
        } // for (i = 2; i <= 24; i++)

        if ( (integr == 2) || (integr == 4)) {

            // compute rg using a forward recurrence relation.

            rg[0] = -ri[0] / alfp1;
            rg[1] = - (ralf + ralf) / (alfp2 * alfp2) - rg[0];
            an = 2.0;
            anm1 = 1.0;
            im1 = 2;
            for (i = 3; i <= 25; i++) {
                rg[i - 1] = - (an * (an - alfp2) * rg[im1 - 1] - an * ri[im1 - 1] + anm1 * ri[i - 1])
                        / (anm1 * (an + alfp1));
                anm1 = an;
                an = an + 1.0;
                im1 = i;
            } // for (i = 3 ; i <= 25; i++)
        } // if ((integr == 2) || (integr == 4)

        if ( (integr == 3) || (integr == 4)) {

            // compute rh using a forward recurrence relation.

            rh[0] = -rj[0] / betp1;
            rh[1] = - (rbet + rbet) / (betp2 * betp2) - rh[0];
            an = 2.0;
            anm1 = 1.0;
            im1 = 2;
            for (i = 3; i <= 25; i++) {
                rh[i - 1] = - (an * (an - betp2) * rh[im1 - 1] - an * rj[im1 - 1] + anm1 * rj[i - 1])
                        / (anm1 * (an + betp1));
                anm1 = an;
                an = an + 1.0;
                im1 = i;
            } // for (i = 3; i <= 25; i++)
            for (i = 2; i <= 25; i += 2) {
                rh[i - 1] = -rh[i - 1];
            } // for (i = 2; i <= 25; i += 2)
        } // if ((integr == 3) || (integr == 4))
        for (i = 2; i <= 25; i += 2) {
            rj[i - 1] = -rj[i - 1];
        }
        return;
    } // dqmomo

    /**
     * This is a port of the original FORTRAN routine whose header is given below: c***begin prologue dqng c***date
     * written 800101 (yymmdd) c***revision date 810101 (yymmdd) c***category no. h2a1a1 c***keywords automatic
     * integrator, smooth integrand, c non-adaptive, gauss-kronrod(patterson) c***author piessens,robert,appl. math. &
     * progr. div. - k.u.leuven c de doncker,elise,appl math & progr. div. - k.u.leuven c kahaner,david,nbs - modified
     * (2/82) c***purpose the routine calculates an approximation result to a c given definite integral i = integral of
     * f over (a,b), c hopefully satisfying following claim for accuracy c abs(i-result).le.max(epsabs,epsrel*abs(i)).
     * c***description c c non-adaptive integration c standard fortran subroutine c double precision version
     * 
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
        // routine dqng. These coefficients were calculated with
        // 101 decimal digit arithmetic by L. W. Fullerton, bell labs, nov 1981.

        final double[] x1 = new double[] {0.973906528517171720077964012084452, 0.865063366688984510732096688423493,
                0.679409568299024406234327365114874, 0.433395394129247190799265943165784,
                0.148874338981631210884826001129720};

        final double[] w10 = new double[] {0.066671344308688137593568809893332, 0.149451349150580593145776339657697,
                0.219086362515982043995534934228163, 0.269266719309996355091226921569469,
                0.295524224714752870173892994651338};

        final double[] x2 = new double[] {0.995657163025808080735527280689003, 0.930157491355708226001207180059508,
                0.780817726586416897063717578345042, 0.562757134668604683339000099272694,
                0.294392862701460198131126603103866};

        final double[] w21a = new double[] {0.032558162307964727478818972459390, 0.075039674810919952767043140916190,
                0.109387158802297641899210590325805, 0.134709217311473325928054001771707,
                0.147739104901338491374841515972068};

        final double[] w21b = new double[] {0.011694638867371874278064396062192, 0.054755896574351996031381300244580,
                0.093125454583697605535065465083366, 0.123491976262065851077958109831074,
                0.142775938577060080797094273138717, 0.149445554002916905664936468389821};

        final double[] x3 = new double[] {0.999333360901932081394099323919911, 0.987433402908088869795961478381209,
                0.954807934814266299257919200290473, 0.900148695748328293625099494069092,
                0.825198314983114150847066732588520, 0.732148388989304982612354848755461,
                0.622847970537725238641159120344323, 0.499479574071056499952214885499755,
                0.364901661346580768043989548502644, 0.222254919776601296498260928066212,
                0.074650617461383322043914435796506};

        final double[] w43a = new double[] {0.016296734289666564924281974617663, 0.037522876120869501461613795898115,
                0.054694902058255442147212685465005, 0.067355414609478086075553166302174,
                0.073870199632393953432140695251367, 0.005768556059769796184184327908655,
                0.027371890593248842081276069289151, 0.046560826910428830743339154433824,
                0.061744995201442564496240336030883, 0.071387267268693397768559114425516};

        final double[] w43b = new double[] {0.001844477640212414100389106552965, 0.010798689585891651740465406741293,
                0.021895363867795428102523123075149, 0.032597463975345689443882222526137,
                0.042163137935191811847627924327955, 0.050741939600184577780189020092084,
                0.058379395542619248375475369330206, 0.064746404951445885544689259517511,
                0.069566197912356484528633315038405, 0.072824441471833208150939535192842,
                0.074507751014175118273571813842889, 0.074722147517403005594425168280423};

        final double[] x4 = new double[] {0.999902977262729234490529830591582, 0.997989895986678745427496322365960,
                0.992175497860687222808523352251425, 0.981358163572712773571916941623894,
                0.965057623858384619128284110607926, 0.943167613133670596816416634507426,
                0.915806414685507209591826430720050, 0.883221657771316501372117548744163,
                0.845710748462415666605902011504855, 0.803557658035230982788739474980964,
                0.757005730685495558328942793432020, 0.706273209787321819824094274740840,
                0.651589466501177922534422205016736, 0.593223374057961088875273770349144,
                0.531493605970831932285268948562671, 0.466763623042022844871966781659270,
                0.399424847859218804732101665817923, 0.329874877106188288265053371824597,
                0.258503559202161551802280975429025, 0.185695396568346652015917141167606,
                0.111842213179907468172398359241362, 0.037352123394619870814998165437704};

        final double[] w87a = new double[] {0.008148377384149172900002878448190, 0.018761438201562822243935059003794,
                0.027347451050052286161582829741283, 0.033677707311637930046581056957588,
                0.036935099820427907614589586742499, 0.002884872430211530501334156248695,
                0.013685946022712701888950035273128, 0.023280413502888311123409291030404,
                0.030872497611713358675466394126442, 0.035693633639418770719351355457044,
                0.000915283345202241360843392549948, 0.005399280219300471367738743391053,
                0.010947679601118931134327826856808, 0.016298731696787335262665703223280,
                0.021081568889203835112433060188190, 0.025370969769253827243467999831710,
                0.029189697756475752501446154084920, 0.032373202467202789685788194889595,
                0.034783098950365142750781997949596, 0.036412220731351787562801163687577,
                0.037253875503047708539592001191226};

        final double[] w87b = new double[] {0.000274145563762072350016527092881, 0.001807124155057942948341311753254,
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
                0.037361073762679023410321241766599};

        // mid point of integration interval
        double centr;

        // half-length of integration interval
        double hlgth;

        // function value
        double fval;

        // array of function values which have already been computed
        final double[] savfun = new double[21];

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
        final double[] fv1 = new double[5];
        final double[] fv2 = new double[5];
        final double[] fv3 = new double[5];
        final double[] fv4 = new double[5];
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
        } else {
            fcentr = intFunc(centr);
        }
        neval[0] = 21;
        ier[0] = 1;

        // Compute the integral using the 10- and 21-point formula.

        res10 = 0.0;
        res21 = w21b[5] * fcentr;
        resabs = w21b[5] * Math.abs(fcentr);

        for (k = 0; k < 5; k++) {
            absc = hlgth * x1[k];
            if (selfTest) {
                fval1 = intFuncTest(centr + absc);
                fval2 = intFuncTest(centr - absc);
            } else {
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
            } else {
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
            resasc = resasc + (w21a[k] * (Math.abs(fv1[k] - reskh) + Math.abs(fv2[k] - reskh)))
                    + (w21b[k] * (Math.abs(fv3[k] - reskh) + Math.abs(fv4[k] - reskh)));
        } // for (k = 0; k < 5; k++)

        abserr[0] = Math.abs( (res21 - res10) * hlgth);
        resasc = resasc * dhlgth;

        if ( (resasc != 0.0) && (abserr[0] != 0.0)) {
            abserr[0] = resasc * (Math.min(1.0, Math.pow( (200.0 * abserr[0] / resasc), 1.5)));
        } // if ((resasc != 0.0) && (abserr[0] != 0.0))

        if (resabs > (uflow / (50.0 * epmach))) {
            abserr[0] = Math.max( (epmach * 50.0) * resabs, abserr[0]);
        } // if (resabs > uflow/(50.0 * epmach))

        if (abserr[0] <= Math.max(epsabs, epsrel * Math.abs(result[0]))) {
            ier[0] = 0;
        } // if (abserr[0] <= Math.max(epsabs, epsrel * Math.abs(result[0])))

        if (ier[0] == 0) {
            return;
        } // if (ier[0] == 0)

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
            } else {
                fval = intFunc(absc + centr) + intFunc(centr - absc);
            }
            res43 = res43 + (fval * w43b[k]);
            savfun[ipx] = fval;
        } // for (k = 0; k < 11; k++)

        // Test for convergence
        result[0] = res43 * hlgth;
        abserr[0] = Math.abs( (res43 - res21) * hlgth);

        if ( (resasc != 0.0) && (abserr[0] != 0.0)) {
            abserr[0] = resasc * (Math.min(1.0, Math.pow( (200.0 * abserr[0] / resasc), 1.5)));
        } // if ((resasc != 0.0) && (abserr[0] != 0.0))

        if (resabs > (uflow / (50.0 * epmach))) {
            abserr[0] = Math.max( (epmach * 50.0) * resabs, abserr[0]);
        } // if (resabs > uflow/(50.0 * epmach))

        if (abserr[0] <= Math.max(epsabs, epsrel * Math.abs(result[0]))) {
            ier[0] = 0;
        } // if (abserr[0] <= Math.max(epsabs, epsrel * Math.abs(result[0])))

        if (ier[0] == 0) {
            return;
        } // if (ier[0] == 0)

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
            } else {
                res87 = res87 + (w87b[k] * (intFunc(absc + centr) + intFunc(centr - absc)));
            }
        } // for (k = 0; k < 22; k++)

        result[0] = res87 * hlgth;
        abserr[0] = Math.abs( (res87 - res43) * hlgth);

        if ( (resasc != 0.0) && (abserr[0] != 0.0)) {
            abserr[0] = resasc * (Math.min(1.0, Math.pow( (200.0 * abserr[0] / resasc), 1.5)));
        } // if ((resasc != 0.0) && (abserr[0] != 0.0))

        if (resabs > (uflow / (50.0 * epmach))) {
            abserr[0] = Math.max( (epmach * 50.0) * resabs, abserr[0]);
        } // if (resabs > uflow/(50.0 * epmach))

        if (abserr[0] <= Math.max(epsabs, epsrel * Math.abs(result[0]))) {
            ier[0] = 0;
        } // if (abserr[0] <= Math.max(epsabs, epsrel * Math.abs(result[0])))

        if (ier[0] == 0) {
            return;
        } // if (ier[0] == 0)

        Preferences.debug("Abnormal return from dqng\n", Preferences.DEBUG_ALGORITHM);

        return;
    }

    /**
     * This is a port of the original FORTRAN routine whose header is given below: c***begin prologue dqpsrt c***refer
     * to dqage,dqagie,dqagpe,dqawse c***routines called (none) c***revision date 810101 (yymmdd) c***keywords
     * sequential sorting c***author piessens,robert,appl. math. & progr. div. - k.u.leuven c de doncker,elise,appl.
     * math. & progr. div. - k.u.leuven c***purpose this routine maintains the descending ordering in the c list of the
     * local error estimated resulting from the c interval subdivision process. at each call two error c estimates are
     * inserted using the sequential search c method, top-down for the largest error estimate and c bottom-up for the
     * smallest error estimate. c***description c c ordering routine c standard fortran subroutine c double precision
     * version
     * 
     * This routine maintains the descending ordering in the list of the local error estimated resulting from the
     * interval subdivision process. At each call two error estimates are inserted using the sequential search method,
     * top-down for the largest error estimate and bottom-up for the smallest error estimate.
     * 
     * @param limit maximum number of error estimates the list can contain
     * @param last number of error estimates currently in the list
     * @param maxErr maxErr points to the nrmax-th largest estimate currently in the list
     * @param ermax nrmax-th largest error estimate ermax[0] = elist[maxErr[0]]
     * @param elist the error estimates
     * @param iord Array of size last , the first k elements of which contain pointers to the error estimates, such that
     *            elist[iord[0]],...,elist[iord[k-1]] form a decreasing sequence, with k = last if last <= (limit/2 +
     *            2), and k = limit + 1 - last otherwise
     * @param nrmax maxErr[0] = iord[nrmax[0] - 1]
     */
    private void dqpsrt(final int limit, final int last, final int[] maxErr, final double[] ermax,
            final double[] elist, final int[] iord, final int[] nrmax) {
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
        // integrand, subdivision increased the error estimate. In the
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
        // descending order. This number depends on the number of
        // subdivisions still allowed
        jupbn = last;

        if (last > ( (limit / 2) + 2)) {
            jupbn = limit + 3 - last;
        } // if (last > (limit/2 + 2))

        errmin = elist[last - 1];

        // Insert errmax by traversing the list top-down,
        // starting comparison from the element elist(iord[nrmmax[0]])

        jbnd = jupbn - 1;
        ibeg = nrmax[0] + 1;

        group: {

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

        group2: {

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
