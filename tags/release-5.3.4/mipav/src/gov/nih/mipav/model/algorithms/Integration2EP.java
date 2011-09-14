package gov.nih.mipav.model.algorithms;


import gov.nih.mipav.util.DoubleDouble;

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

public abstract class Integration2EP {

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
    private final DoubleDouble[] abserr = new DoubleDouble[1];

    /**
     * parameter in the weight function, alfa.gt.(-1) if alfa.le.(-1), the routine will end with errorStatus = 6.
     */
    private DoubleDouble alfa;

    /** The left end points of the subintervals in the partition of the given integration range (lower, upper). */
    private DoubleDouble[] alist;

    /**
     * parameter in the weight function, beta.gt.(-1) if beta.le.(-1), the routine will end with ier[0] = 6.
     */
    private DoubleDouble beta;

    /** The right end points of the subintervals in the partition of the given integration range (lower, upper). */
    private DoubleDouble[] blist;

    /** finite bound of integration range used in dqagie (has no meaning if interval is doubly-infinite). */
    private DoubleDouble bound;

    /**
     * Used in dqagpe This array must always be >= 2 in length. The first breakPoints.length - 2 points are user
     * provided break points. If these points are not in an ascending sequence, they will automatically be sorted.
     */
    private DoubleDouble[] breakPoints;

    /**
     * Parameter in the weight function, c != lower, c != upper. If c == a or c == b, the routine will end with ier[0] =
     * 6
     */
    private DoubleDouble c;

    /**
     * array of dimension (maxp1,25) containing the chebyshev moments
     */
    private DoubleDouble[][] chebmo = null;

    /** Estimates of the absolute errors on the subintervals. */
    private DoubleDouble[] elist;

    /**
     * epmach = D1MACH(4) Machine epmach is the smallest positive epmach such that (1.0 + epmach) != 1.0. epmach = 2**(1 -
     * doubleDigits) = 2**(1 - 53) = 2**(-52) epmach = 2.224460e-16 epmach is called the largest relative spacing for
     * doubles For DoubleDouble useEPS = 1.23259516440783e-32= 2^-106
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
     * In dqawfe vector of dimension at least limlst erlst[k] contains the error estimate corresponding wiht rstlst[k].
     */
    private DoubleDouble erlst[];

    private DoubleDouble gamma;

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
     * achieved. errorStatus > 0 for abnormal termination of the routine. The estimates for the integral and error are
     * less reliable. It is assumed that the requested accuracy has not been achieved.
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
     * ier[0 = 4 The algorithm does not converge. Roundoff error is detected in the extrapolation table. It is presumed
     * that the requested tolerance cannot be achieved, and that the returned result is the best that can be obtained.
     * 
     * ier[0] = 5 The integral is probably divergent, or slowly convergent. It must be noted that divergence can occur
     * with any other value of errorStatus > 0.
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
    private DoubleDouble lower;

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
    private final DoubleDouble oflow = DoubleDouble.valueOf(Double.MAX_VALUE);

    /** parameter in the integrand weight function */
    private DoubleDouble omega;

    /** Integration limits and the break points of the interval in ascending sequence. */
    private DoubleDouble[] pts;

    private final DoubleDouble[] resabs = new DoubleDouble[1];

    private final DoubleDouble[] resasc = new DoubleDouble[1];

    /** Approximation to the integral. */
    private final DoubleDouble[] result = new DoubleDouble[1];

    /** The integral approximations of the subintervals. */
    private DoubleDouble[] rlist;

    /** DOCUMENT ME! */
    private int routine;

    /**
     * In dqawfe vector of dimension at least limlst. rslst[k-1] contains the integral contribution over the interval
     * (a+(k-1)c, a+kc), where c = (2*int(abs(omega))+1)*pi/abs(omega), k = 1, 2, ..., lst. Note that if omega = 0,
     * rslst[0] contains the value of the integral over (a, infinity).
     */
    private DoubleDouble rslst[];

    /**
     * For doubles uflow = 2**-1022, but a double has 53 bits of precision while a DoubleDouble has 106 bits of
     * precision, so uflow = 2**-(1022 - 53) = 2**-969
     */
    private final DoubleDouble uflow = DoubleDouble.valueOf(2.0).pow( -969.0);

    /** DOCUMENT ME! */
    private DoubleDouble upper;

    private boolean selfTest = false;

    private int testCase = 1;

    private DoubleDouble actualAnswer;

    // ~ Constructors
    // ---------------------------------------------------------------------------------------------------

    /**
     * Constructor for running self tests.
     */
    public Integration2EP() {
        int i;
        // DoubleDouble neweps;
        DoubleDouble tol;
        selfTest = true;

        // epmach = DoubleDouble.valueOf(1.0);
        // neweps = DoubleDouble.valueOf(1.0);

        // while (true) {
        // Obtain x.hi = 2.0 x.lo = 0
        // x.hi = 1.5 x.lo = 0
        // down to
        // x.hi = 1.0000000000000002 x.lo = 0.0
        // x.hi = 1.0 x.lo = 1.1102230246251565E-16
        // down to
        // x.hi = 1.0 x.lo = 4.9E-324
        // DoubleDouble x = (DoubleDouble.valueOf(1.0)).add(neweps);
        // Preferences.debug("x.hi = " + x.hi + "\n");
        // Preferences.debug("x.lo = " + x.lo + "\n");
        // if ((DoubleDouble.valueOf(1.0)).equals((DoubleDouble.valueOf(1.0)).add(neweps))) {
        // break;
        // } else {
        // epmach = neweps;
        // neweps = neweps.divide(DoubleDouble.valueOf(2.0));
        // }
        // }
        // DoubleDouble.EPS is the smallest representable relative difference between two {link @ DoubleDouble} values
        // EPS = 1.23259516440783e-32; /* = 2^-106 */
        epmach = DoubleDouble.valueOf(DoubleDouble.EPS);

        tol = ( (DoubleDouble.valueOf(50.0)).multiply(epmach)).max(DoubleDouble.valueOf(5.0E-29));

        testCase = 1;
        // dqage is an adaptive automatic integrator using a Gauss-Kronod rule.
        // Integrate cos(100*sin(x)) from 0 to PI.
        // The exact answer is PI * j0(100), or roughly 0.06278740.
        // key chooses the order of the integration rule from 1 to 6.
        // Numerical Integral = 0.062787400491492629703018938311878 after 2623 integrand evaluations used
        // Error status = 2 with estimated absolute error = 5.475357990914704230361215237912E-22
        // Actual answer = 0.062787400491492672011578833637816
        // Exact error = -4.2308559895325939633446582822463E-17
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

        lower = DoubleDouble.valueOf(0.0);
        upper = DoubleDouble.PI;
        routine = Integration2EP.DQAGE;
        key = 6;
        epsabs = DoubleDouble.valueOf(0.0);
        epsrel = DoubleDouble.valueOf(1.0E-28);
        actualAnswer = (DoubleDouble.valueOf(realResult[0])).multiply(DoubleDouble.PI);
        limit = 1000;

        if (upper.le(lower)) {
            MipavUtil.displayError("upper must be > than lower");
            ier[0] = 6;

            return;
        }

        if ( (epsabs.le(DoubleDouble.valueOf(0.0))) && (epsrel.lt(tol))) {
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
        Preferences.debug("Exact error = " + (result[0].subtract(actualAnswer)) + "\n", Preferences.DEBUG_ALGORITHM);

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
        actualAnswer = ( ( (DoubleDouble.PI).negate()).multiply( (DoubleDouble.valueOf(10.0)).log()))
                .divide(DoubleDouble.valueOf(20.0));

        if ( (epsabs.le(DoubleDouble.valueOf(0.0))) && (epsrel.lt(tol))) {
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
        Preferences.debug("Exact error = " + (result[0].subtract(actualAnswer)) + "\n", Preferences.DEBUG_ALGORITHM);

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
        actualAnswer = ( ( (DoubleDouble.valueOf(61.0)).multiply( (DoubleDouble.valueOf(2.0)).log()))
                .add( (DoubleDouble.valueOf(77.0)).multiply( ( (DoubleDouble.valueOf(7.0)).log())).divide(
                        DoubleDouble.valueOf(4.0)))).subtract(DoubleDouble.valueOf(27.0));

        if (upper.le(lower)) {
            MipavUtil.displayError("upper must be > than lower");
            ier[0] = 6;

            return;
        }

        if (npts2 < 2) {
            MipavUtil.displayError("breakPoints array length must be >= 2");
            ier[0] = 6;

            return;
        }

        for (i = 0; i < (npts2 - 2); i++) {

            if (breakPoints[i].lt(lower)) {
                MipavUtil.displayError("Break point " + breakPoints[i] + " is impossibly < lower");
                ier[0] = 6;

                return;
            }

            if (breakPoints[i].gt(upper)) {
                MipavUtil.displayError("Break point " + breakPoints[i] + " is impossibly > upper");
                ier[0] = 6;

                return;
            }
        } // for (int i = 0; i < npts2 - 2; i++)

        if ( (epsabs.le(DoubleDouble.valueOf(0.0))) && (epsrel.lt(tol))) {
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
        Preferences.debug("Exact error = " + (result[0].subtract(actualAnswer)) + "\n", Preferences.DEBUG_ALGORITHM);

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
        actualAnswer = DoubleDouble.valueOf( -4.0);

        if (upper.le(lower)) {
            MipavUtil.displayError("upper must be > than lower");
            ier[0] = 6;

            return;
        }

        if ( (epsabs.le(DoubleDouble.valueOf(0.0))) && (epsrel.lt(tol))) {
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
        Preferences.debug("Exact error = " + (result[0].subtract(actualAnswer)) + "\n", Preferences.DEBUG_ALGORITHM);

        testCase = 5;
        // Test5 tests dqawce
        // dqawce is an adaptive integrator for finding the Cauchy
        // principal value of the integral of f(x)*w(x) over (lower,upper)
        // where w(x) = 1/(x-c), c between lower and upper.
        // Integrate 1/(x*(5*x*x*x+6)) from -1 to 5.
        // The exact answer is log(125/631)/18 = -0.0899401
        // Numerical Integral = -0.089944006957717334997429859754904 after 1465 integrand evaluations used
        // Error status = 2 with estimated absolute error = 3.8465541596292214532062525632172E-16
        // Actual answer = -0.089944006957717335193136668554834
        // Exact error = 1.957068087999283248150260255781E-19
        lower = DoubleDouble.valueOf( -1.0);
        upper = DoubleDouble.valueOf(5.0);
        c = DoubleDouble.valueOf(0.0);
        routine = Integration2.DQAWCE;
        epsabs = DoubleDouble.valueOf(0.0);
        epsrel = DoubleDouble.valueOf(1.0E-28);
        limit = 1000;
        actualAnswer = ( ( (DoubleDouble.valueOf(125.0)).divide(DoubleDouble.valueOf(631.0))).log())
                .divide(DoubleDouble.valueOf(18.0));

        if (upper.le(lower)) {
            MipavUtil.displayError("upper must be > than lower");
            ier[0] = 6;

            return;
        }

        if ( (c.equals(lower)) || (c.equals(upper))) {
            MipavUtil.displayError("Cannot have c == lower or c == upper");
            ier[0] = 6;

            return;
        }

        if ( (epsabs.le(DoubleDouble.valueOf(0.0))) && (epsrel.lt(tol))) {
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
        Preferences.debug("Exact error = " + (result[0].subtract(actualAnswer)) + "\n", Preferences.DEBUG_ALGORITHM);

        testCase = 6;
        // Test 6 tests dqawfe
        // dqawfe handles fourier integration of f(x)*w(x) from
        // lower to infinity, with w(x) = cos(omega*x) or sine(omega*x)
        // Integrate cos(pi*x/2)/sqrt(x) from 0 to infinity.
        // The exact answer is 1.0

        lower = DoubleDouble.valueOf(0.0);
        routine = Integration2.DQAWFE;
        epsabs = DoubleDouble.valueOf(1.0E-28);
        // integr = 1 for cosine, = 2 for sine
        integr = 1;
        limlst = 1000;
        limit = 1000;
        maxp1 = 1000;
        omega = DoubleDouble.PI_2;
        actualAnswer = DoubleDouble.valueOf(1.0);
        // Numerical Integral = 1.0008077423247694706975922000344 after 29410 integrand evaluations used
        // Error status = 1 with estimated absolute error = 4.3642349749966654680431157178436E-5
        // Actual answer = 1.0
        // Exact error = 8.0774232476947069759220003443673E-4

        if ( (integr != 1) && (integr != 2)) {
            MipavUtil.displayError("integr must equal 1 or 2");
            ier[0] = 6;

            return;
        }

        if (epsabs.le(DoubleDouble.valueOf(0.0))) {
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
        Preferences.debug("Exact error = " + (result[0].subtract(actualAnswer)) + "\n", Preferences.DEBUG_ALGORITHM);

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
        // ci(10.0*pi) = -0.0010071562550216642568563602363948
        // Numerical Integral = -0.12813684839916732808773185269829 after 1585 integrand evaluations used
        // Error status = 2 with estimated absolute error = 1.7134290667214252583410074821439E-16
        // Actual answer = -0.12813684789465472190566225032523
        // Exact error = -5.0451260618206960237305589988034E-10

        lower = DoubleDouble.valueOf(0.0);
        upper = DoubleDouble.valueOf(1.0);
        routine = Integration2.DQAWOE;
        epsabs = DoubleDouble.valueOf(0.0);
        epsrel = DoubleDouble.valueOf(1.0E-28);
        limit = 100;
        omega = (DoubleDouble.valueOf(10.0)).multiply(DoubleDouble.PI);
        integr = 2;
        icall = 1;
        maxp1 = 100;
        gamma = DoubleDouble.valueOf(0.57721566490153286061);
        final DoubleDouble cin = DoubleDouble.valueOf(4.0255378);
        DoubleDouble ci = ( (gamma.add(DoubleDouble.PI.log())).add( (DoubleDouble.valueOf(10.0)).log())).subtract(cin);
        actualAnswer = ( ( (gamma.add( ( (DoubleDouble.valueOf(10.0)).multiply(DoubleDouble.PI)).log())).subtract(ci))
                .negate()).divide( (DoubleDouble.valueOf(10.0)).multiply(DoubleDouble.PI));

        if (upper.le(lower)) {
            MipavUtil.displayError("upper must be > than lower");
            ier[0] = 6;

            return;
        }

        if ( (epsabs.le(DoubleDouble.valueOf(0.0))) && (epsrel.lt(tol))) {
            MipavUtil.displayError("Must set epsabs > 0.0 or must set epsrel >= " + tol);
            ier[0] = 6;

            return;
        }

        if (limit < 1) {
            MipavUtil.displayError("limit must be >= 1");
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
        Preferences.debug("Exact error = " + (result[0].subtract(actualAnswer)) + "\n", Preferences.DEBUG_ALGORITHM);

        testCase = 8;
        // test8 tests dqawse
        // dqawse is an adaptive integrator for integrands wiht
        // algebraic or logarithmic singularities at the endpoints.
        // ci is the cosine integral ci(x) = integral (x to infinity) -cos(v)/v dv
        // Si is the sine integral Si(x) = integral (0 to x) sin(v)/v dv
        // Actual answer = 0.5*(ci(1.0)*sin(1.0) - si(1.0)*cos(1.0)) - 0.5
        // Actual answer = 0.5*(ci(1.0)*sin(1.0) - (Si(1.0) - pi/2.0)*cos(1.0)) - 0.5
        // Numerical Integral = -0.18927518788209331038635264953216 after 1530 integrand evaluations used
        // Error status = 2 with estimated absolute error = 9.5744005211108732062017105732718E-17
        // Actual answer = -0.18927518789136618790735779914497
        // Exact error = 9.2728775210051496128126555915408E-12

        lower = DoubleDouble.valueOf(0.0);
        upper = DoubleDouble.valueOf(1.0);
        routine = Integration2EP.DQAWSE;
        epsabs = DoubleDouble.valueOf(0.0);
        epsrel = DoubleDouble.valueOf(1.0E-28);
        limit = 1000;
        alfa = DoubleDouble.valueOf(0.0);
        beta = DoubleDouble.valueOf(0.0);
        // Describes the integrand weight function
        // integr = 2 means weight function is (x-lower)**alfa * (upper-x)**beta * log(x-lower)
        integr = 2;
        ci = DoubleDouble.valueOf(0.3374039229);
        final DoubleDouble Si = DoubleDouble.valueOf(0.9460830704);
        actualAnswer = ( (DoubleDouble.valueOf(0.5)).multiply( (ci.multiply( (DoubleDouble.valueOf(1.0)).sin()))
                .subtract( (Si.subtract(DoubleDouble.PI_2)).multiply( (DoubleDouble.valueOf(1.0)).cos()))))
                .subtract(DoubleDouble.valueOf(0.5));

        if (upper.le(lower)) {
            MipavUtil.displayError("upper must be > than lower");
            ier[0] = 6;

            return;
        }

        if ( (epsabs.le(DoubleDouble.valueOf(0.0))) && (epsrel.lt(tol))) {
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

        if (alfa.le(DoubleDouble.valueOf( -1.0))) {
            MipavUtil.displayError("alfa must be > -1.0");
            ier[0] = 6;

            return;
        }

        if (beta.le(DoubleDouble.valueOf( -1.0))) {
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
        Preferences.debug("Exact error = " + (result[0].subtract(actualAnswer)) + "\n", Preferences.DEBUG_ALGORITHM);

        testCase = 9;
        // test9 tests dqk15
        // dqk15 is a Gauss-Kronod quadrature rule.
        lower = DoubleDouble.valueOf(0.0);
        upper = DoubleDouble.valueOf(1.0);
        routine = Integration2EP.DQK15;
        actualAnswer = (DoubleDouble.valueOf( -4.0)).divide(DoubleDouble.valueOf(9.0));
        // Numerical Integral = -0.44453824758327635083418354279228
        // Estimated absolute error = 0.2017677811047011722019991551744
        // Actual answer = -0.44444444444444444444444444444444
        // Exact error = -9.3803138831906389739098347836056E-5
        // resabs[0] = 0.44453824758327635083418354279228
        // resasc[0] = 0.2017677811047011722019991551744

        driver();

        Preferences.debug("Test 9 testing dqk15\n", Preferences.DEBUG_ALGORITHM);
        Preferences.debug("Integrand is sqrt(x) * log(x)\n", Preferences.DEBUG_ALGORITHM);
        Preferences.debug("Integrand lower endpoint = 0.0\n", Preferences.DEBUG_ALGORITHM);
        Preferences.debug("Integrand upper endpoint = 1.0\n", Preferences.DEBUG_ALGORITHM);
        Preferences.debug("Numerical Integral = " + result[0] + "\n", Preferences.DEBUG_ALGORITHM);
        Preferences.debug("Estimated absolute error = " + abserr[0] + "\n", Preferences.DEBUG_ALGORITHM);
        Preferences.debug("Actual answer = " + actualAnswer + "\n", Preferences.DEBUG_ALGORITHM);
        Preferences.debug("Exact error = " + (result[0].subtract(actualAnswer)) + "\n", Preferences.DEBUG_ALGORITHM);
        Preferences.debug("resabs[0] = " + resabs[0] + "\n", Preferences.DEBUG_ALGORITHM);
        Preferences.debug("resasc[0] = " + resasc[0] + "\n", Preferences.DEBUG_ALGORITHM);

        testCase = 10;
        // test10 tests dqk21
        // dqk21 is a Gauss-Kronod quadrature rule.
        lower = DoubleDouble.valueOf(0.0);
        upper = DoubleDouble.valueOf(1.0);
        routine = Integration2EP.DQK21;
        actualAnswer = (DoubleDouble.valueOf( -4.0)).divide(DoubleDouble.valueOf(9.0));
        // Numerical Integral = -0.44448120174773075854749425655714
        // Estimated absolute error = 0.062137345692400246001066751930368
        // Actual answer = -0.44444444444444444444444444444444
        // Exact error = -3.6757303286314103049812112702108E-5
        // resabs[0] = 0.44448120174773075854749425655714
        // resasc[0] = 0.2010203179967191344102455883178

        driver();

        Preferences.debug("Test 10 testing dqk21\n", Preferences.DEBUG_ALGORITHM);
        Preferences.debug("Integrand is sqrt(x) * log(x)\n", Preferences.DEBUG_ALGORITHM);
        Preferences.debug("Integrand lower endpoint = 0.0\n", Preferences.DEBUG_ALGORITHM);
        Preferences.debug("Integrand upper endpoint = 1.0\n", Preferences.DEBUG_ALGORITHM);
        Preferences.debug("Numerical Integral = " + result[0] + "\n", Preferences.DEBUG_ALGORITHM);
        Preferences.debug("Estimated absolute error = " + abserr[0] + "\n", Preferences.DEBUG_ALGORITHM);
        Preferences.debug("Actual answer = " + actualAnswer + "\n", Preferences.DEBUG_ALGORITHM);
        Preferences.debug("Exact error = " + (result[0].subtract(actualAnswer)) + "\n", Preferences.DEBUG_ALGORITHM);
        Preferences.debug("resabs[0] = " + resabs[0] + "\n", Preferences.DEBUG_ALGORITHM);
        Preferences.debug("resasc[0] = " + resasc[0] + "\n", Preferences.DEBUG_ALGORITHM);

        testCase = 11;
        // test11 tests dqk31
        // dqk31 is a Gauss-Kronod quadrature rule.
        lower = DoubleDouble.valueOf(0.0);
        upper = DoubleDouble.valueOf(1.0);
        routine = Integration2EP.DQK31;
        actualAnswer = (DoubleDouble.valueOf( -4.0)).divide(DoubleDouble.valueOf(9.0));
        // Numerical Integral = -0.44445711423810116344071890372747
        // Estimated absolute error = 0.013135187473737603241003449749653
        // Actual answer = -0.44444444444444444444444444444444
        // Exact error = -1.2669793656718996274459283032989E-5
        // resabs[0] = 0.44445711423810116344071890372747
        // resasc[0] = 0.20044662305437476729677475163959

        driver();

        Preferences.debug("Test 11 testing dqk31\n", Preferences.DEBUG_ALGORITHM);
        Preferences.debug("Integrand is sqrt(x) * log(x)\n", Preferences.DEBUG_ALGORITHM);
        Preferences.debug("Integrand lower endpoint = 0.0\n", Preferences.DEBUG_ALGORITHM);
        Preferences.debug("Integrand upper endpoint = 1.0\n", Preferences.DEBUG_ALGORITHM);
        Preferences.debug("Numerical Integral = " + result[0] + "\n", Preferences.DEBUG_ALGORITHM);
        Preferences.debug("Estimated absolute error = " + abserr[0] + "\n", Preferences.DEBUG_ALGORITHM);
        Preferences.debug("Actual answer = " + actualAnswer + "\n", Preferences.DEBUG_ALGORITHM);
        Preferences.debug("Exact error = " + (result[0].subtract(actualAnswer)) + "\n", Preferences.DEBUG_ALGORITHM);
        Preferences.debug("resabs[0] = " + resabs[0] + "\n", Preferences.DEBUG_ALGORITHM);
        Preferences.debug("resasc[0] = " + resasc[0] + "\n", Preferences.DEBUG_ALGORITHM);

        testCase = 12;
        // test12 tests dqk41
        // dqk41 is a Gauss-Kronod quadrature rule.
        lower = DoubleDouble.valueOf(0.0);
        upper = DoubleDouble.valueOf(1.0);
        routine = Integration2EP.DQK41;
        actualAnswer = (DoubleDouble.valueOf( -4.0)).divide(DoubleDouble.valueOf(9.0));
        // Numerical Integral = -0.4444502553565425529317134939924
        // Estimated absolute error = 0.0042429656788425900820197450546617
        // Actual answer = -0.44444444444444444444444444444444
        // Exact error = -5.8109120981084872690495479589851E-6
        // resabs[0] = 0.4444502553565425529317134939924
        // resasc[0] = 0.20065010692415911355429312311755

        driver();

        Preferences.debug("Test 12 testing dqk41\n", Preferences.DEBUG_ALGORITHM);
        Preferences.debug("Integrand is sqrt(x) * log(x)\n", Preferences.DEBUG_ALGORITHM);
        Preferences.debug("Integrand lower endpoint = 0.0\n", Preferences.DEBUG_ALGORITHM);
        Preferences.debug("Integrand upper endpoint = 1.0\n", Preferences.DEBUG_ALGORITHM);
        Preferences.debug("Numerical Integral = " + result[0] + "\n", Preferences.DEBUG_ALGORITHM);
        Preferences.debug("Estimated absolute error = " + abserr[0] + "\n", Preferences.DEBUG_ALGORITHM);
        Preferences.debug("Actual answer = " + actualAnswer + "\n", Preferences.DEBUG_ALGORITHM);
        Preferences.debug("Exact error = " + (result[0].subtract(actualAnswer)) + "\n", Preferences.DEBUG_ALGORITHM);
        Preferences.debug("resabs[0] = " + resabs[0] + "\n", Preferences.DEBUG_ALGORITHM);
        Preferences.debug("resasc[0] = " + resasc[0] + "\n", Preferences.DEBUG_ALGORITHM);

        testCase = 13;
        // test13 tests dqk51
        // dqk51 is a Gauss-Kronod quadrature rule.
        lower = DoubleDouble.valueOf(0.0);
        upper = DoubleDouble.valueOf(1.0);
        routine = Integration2EP.DQK51;
        actualAnswer = (DoubleDouble.valueOf( -4.0)).divide(DoubleDouble.valueOf(9.0));
        // Numerical Integral = -0.44444761693182600668948609006176
        // Estimated absolute error = 0.0017429443550292554005765269872151
        // Actual answer = -0.44444444444444444444444444444444
        // Exact error = -3.1724873815622450416456173276073E-6
        // resabs[0] = 0.44444761693182600668948609006176
        // resasc[0] = 0.20079987986805535202107362923376

        driver();

        Preferences.debug("Test 13 testing dqk51\n", Preferences.DEBUG_ALGORITHM);
        Preferences.debug("Integrand is sqrt(x) * log(x)\n", Preferences.DEBUG_ALGORITHM);
        Preferences.debug("Integrand lower endpoint = 0.0\n", Preferences.DEBUG_ALGORITHM);
        Preferences.debug("Integrand upper endpoint = 1.0\n", Preferences.DEBUG_ALGORITHM);
        Preferences.debug("Numerical Integral = " + result[0] + "\n", Preferences.DEBUG_ALGORITHM);
        Preferences.debug("Estimated absolute error = " + abserr[0] + "\n", Preferences.DEBUG_ALGORITHM);
        Preferences.debug("Actual answer = " + actualAnswer + "\n", Preferences.DEBUG_ALGORITHM);
        Preferences.debug("Exact error = " + (result[0].subtract(actualAnswer)) + "\n", Preferences.DEBUG_ALGORITHM);
        Preferences.debug("resabs[0] = " + resabs[0] + "\n", Preferences.DEBUG_ALGORITHM);
        Preferences.debug("resasc[0] = " + resasc[0] + "\n", Preferences.DEBUG_ALGORITHM);

        testCase = 14;
        // test14 tests dqk61
        // dqk61 is a Gauss-Kronod quadrature rule.
        lower = DoubleDouble.valueOf(0.0);
        upper = DoubleDouble.valueOf(1.0);
        routine = Integration2EP.DQK61;
        actualAnswer = (DoubleDouble.valueOf( -4.0)).divide(DoubleDouble.valueOf(9.0));
        // Numerical Integral = -0.44444636518933710849952792078239
        // Estimated absolute error = 8.3764739340422666930861763803848E-4
        // Actual answer = -0.44444444444444444444444444444444
        // Exact error = -1.9207448926640550834763379470697E-6
        // resabs[0] = 0.44444636518933710849952792078239
        // resasc[0] = 0.20063345752687790135346389270898

        driver();

        Preferences.debug("Test 14 testing dqk61\n", Preferences.DEBUG_ALGORITHM);
        Preferences.debug("Integrand is sqrt(x) * log(x)\n", Preferences.DEBUG_ALGORITHM);
        Preferences.debug("Integrand lower endpoint = 0.0\n", Preferences.DEBUG_ALGORITHM);
        Preferences.debug("Integrand upper endpoint = 1.0\n", Preferences.DEBUG_ALGORITHM);
        Preferences.debug("Numerical Integral = " + result[0] + "\n", Preferences.DEBUG_ALGORITHM);
        Preferences.debug("Estimated absolute error = " + abserr[0] + "\n", Preferences.DEBUG_ALGORITHM);
        Preferences.debug("Actual answer = " + actualAnswer + "\n", Preferences.DEBUG_ALGORITHM);
        Preferences.debug("Exact error = " + (result[0].subtract(actualAnswer)) + "\n", Preferences.DEBUG_ALGORITHM);
        Preferences.debug("resabs[0] = " + resabs[0] + "\n", Preferences.DEBUG_ALGORITHM);
        Preferences.debug("resasc[0] = " + resasc[0] + "\n", Preferences.DEBUG_ALGORITHM);

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
        actualAnswer = (DoubleDouble.valueOf( -4.0)).divide(DoubleDouble.valueOf(9.0));

        if ( (epsabs.le(DoubleDouble.valueOf(0.0))) && (epsrel.lt(tol))) {
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
        Preferences.debug("Exact error = " + (result[0].subtract(actualAnswer)) + "\n", Preferences.DEBUG_ALGORITHM);

        testCase = 101;
        lower = DoubleDouble.valueOf(0.0);
        upper = DoubleDouble.valueOf(1.0);
        routine = Integration2EP.DQAGPE;
        breakPoints = new DoubleDouble[4];
        breakPoints[0] = (DoubleDouble.valueOf(1.0)).divide(DoubleDouble.valueOf(7.0));
        breakPoints[1] = (DoubleDouble.valueOf(2.0)).divide(DoubleDouble.valueOf(3.0));
        epsabs = DoubleDouble.valueOf(0.0);
        // epsrel = DoubleDouble.valueOf(1.0E-3);
        epsrel = DoubleDouble.valueOf(1.0E-28);
        // limit = 100;
        limit = 1000;

        npts2 = breakPoints.length;
        npts = npts2 - 2;

        if (npts2 < 2) {
            MipavUtil.displayError("breakPoints array length must be >= 2");
            ier[0] = 6;

            return;
        }

        for (i = 0; i < (npts2 - 2); i++) {

            if (breakPoints[i].lt(lower)) {
                MipavUtil.displayError("Break point " + breakPoints[i] + " is impossibly < lower");
                ier[0] = 6;

                return;
            }

            if (breakPoints[i].gt(upper)) {
                MipavUtil.displayError("Break point " + breakPoints[i] + " is impossibly > upper");
                ier[0] = 6;

                return;
            }
        } // for (int i = 0; i < npts2 - 2; i++)

        if ( (epsabs.le(DoubleDouble.valueOf(0.0))) && (epsrel.lt(tol))) {
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
        Preferences.debug("Error status = " + ier[0] + " with absolute error = " + abserr[0] + "\n", Preferences.DEBUG_ALGORITHM);

        testCase = 102;
        bound = DoubleDouble.valueOf(0.0);
        routine = Integration2EP.DQAGIE;
        inf = 1;
        epsabs = DoubleDouble.valueOf(0.0);
        // epsrel = DoubleDouble.valueOf(1.0E-3);
        epsrel = DoubleDouble.valueOf(1.0E-28);
        // limit = 100;
        limit = 1000;

        if ( (inf != 1) && (inf != -1) && (inf != 2)) {
            MipavUtil.displayError("inf must equal -1, 1, or 2");
            ier[0] = 6;

            return;
        }

        if ( (epsabs.le(DoubleDouble.valueOf(0.0))) && (epsrel.lt(tol))) {
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
    public Integration2EP(final DoubleDouble lower, final DoubleDouble upper, final int routine,
            final DoubleDouble epsabs, final DoubleDouble epsrel) {
        DoubleDouble tol;

        this.lower = lower;
        this.upper = upper;
        this.routine = routine;

        if (routine != Integration2EP.DQNG) {
            MipavUtil.displayError("routine must be DQAGSE with this constructor");

            return;
        }

        if (upper.le(lower)) {
            MipavUtil.displayError("upper must be > than lower");
            ier[0] = 6;

            return;
        }

        this.epsabs = epsabs;
        this.epsrel = epsrel;

        epmach = DoubleDouble.valueOf(DoubleDouble.EPS);
        tol = ( (DoubleDouble.valueOf(50.0)).multiply(epmach)).max(DoubleDouble.valueOf(5.0E-29));

        if ( (epsabs.le(DoubleDouble.valueOf(0.0))) && (epsrel.lt(tol))) {
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
    public Integration2EP(final DoubleDouble bound, final int routine, final int inf, final DoubleDouble epsabs,
            final DoubleDouble epsrel, final int limit) {
        DoubleDouble tol;
        this.bound = bound;
        this.routine = routine;

        if (routine != Integration2EP.DQAGIE) {
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

        epmach = DoubleDouble.valueOf(DoubleDouble.EPS);
        tol = ( (DoubleDouble.valueOf(50.0)).multiply(epmach)).max(DoubleDouble.valueOf(5.0E-29));

        if ( (epsabs.le(DoubleDouble.valueOf(0.0))) && (epsrel.lt(tol))) {
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
    public Integration2EP(final DoubleDouble lower, final DoubleDouble upper, final int routine,
            final DoubleDouble epsabs, final DoubleDouble epsrel, final int limit) {
        DoubleDouble tol;

        this.lower = lower;
        this.upper = upper;
        this.routine = routine;

        if (routine != Integration2EP.DQAGSE) {
            MipavUtil.displayError("routine must be DQAGSE with this constructor");

            return;
        }

        if (upper.le(lower)) {
            MipavUtil.displayError("upper must be > than lower");
            ier[0] = 6;

            return;
        }

        this.epsabs = epsabs;
        this.epsrel = epsrel;

        epmach = DoubleDouble.valueOf(DoubleDouble.EPS);
        tol = ( (DoubleDouble.valueOf(50.0)).multiply(epmach)).max(DoubleDouble.valueOf(5.0E-29));

        if ( (epsabs.le(DoubleDouble.valueOf(0.0))) && (epsrel.lt(tol))) {
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
    public Integration2EP(final DoubleDouble lower, final DoubleDouble upper, final DoubleDouble c, final int routine,
            final DoubleDouble epsabs, final DoubleDouble epsrel, final int limit) {
        DoubleDouble tol;

        this.lower = lower;
        this.upper = upper;
        this.c = c;
        this.routine = routine;

        if (routine != Integration2EP.DQAWCE) {
            MipavUtil.displayError("routine must be DQAWCE with this constructor");

            return;
        }

        if (upper.le(lower)) {
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

        epmach = DoubleDouble.valueOf(DoubleDouble.EPS);
        tol = ( (DoubleDouble.valueOf(50.0)).multiply(epmach)).max(DoubleDouble.valueOf(5.0E-29));

        if ( (epsabs.le(DoubleDouble.valueOf(0.0))) && (epsrel.lt(tol))) {
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
    public Integration2EP(final DoubleDouble lower, final int routine, final DoubleDouble omega, final int integr,
            final DoubleDouble epsabs, final int limlst, final int limit, final int maxp1) {

        this.lower = lower;
        this.routine = routine;

        if (routine != Integration2EP.DQAWFE) {
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

        if (epsabs.le(DoubleDouble.valueOf(0.0))) {
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
    public Integration2EP(final DoubleDouble lower, final DoubleDouble upper, final int routine,
            final DoubleDouble omega, final int integr, final DoubleDouble epsabs, final DoubleDouble epsrel,
            final int limit, final int icall, final int maxp1) {
        DoubleDouble tol;

        this.lower = lower;
        this.upper = upper;
        this.routine = routine;

        if (routine != Integration2EP.DQAWOE) {
            MipavUtil.displayError("routine must be DQAWOE with this constructor");

            return;
        }

        if (upper.le(lower)) {
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

        epmach = DoubleDouble.valueOf(DoubleDouble.EPS);
        tol = ( (DoubleDouble.valueOf(50.0)).multiply(epmach)).max(DoubleDouble.valueOf(5.0E-29));

        if ( (epsabs.le(DoubleDouble.valueOf(0.0))) && (epsrel.lt(tol))) {
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
    public Integration2EP(final DoubleDouble lower, final DoubleDouble upper, final int routine,
            final DoubleDouble alfa, final DoubleDouble beta, final int integr, final DoubleDouble epsabs,
            final DoubleDouble epsrel, final int limit) {
        DoubleDouble tol;

        this.lower = lower;
        this.upper = upper;
        this.routine = routine;

        if (routine != Integration2EP.DQAWSE) {
            MipavUtil.displayError("routine must be DQAWSE with this constructor");

            return;
        }

        if (upper.le(lower)) {
            MipavUtil.displayError("upper must be > than lower");
            ier[0] = 6;

            return;
        }

        this.alfa = alfa;
        if (alfa.le(DoubleDouble.valueOf( -1.0))) {
            MipavUtil.displayError("alfa must be > -1.0");
            ier[0] = 6;

            return;
        }

        this.beta = beta;
        if (beta.le(DoubleDouble.valueOf( -1.0))) {
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

        epmach = DoubleDouble.valueOf(DoubleDouble.EPS);
        tol = ( (DoubleDouble.valueOf(50.0)).multiply(epmach)).max(DoubleDouble.valueOf(5.0E-29));

        if ( (epsabs.le(DoubleDouble.valueOf(0.0))) && (epsrel.lt(tol))) {
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
    public Integration2EP(final DoubleDouble lower, final DoubleDouble upper, final int routine,
            final DoubleDouble[] breakPoints, final DoubleDouble epsabs, final DoubleDouble epsrel, final int limit) {
        DoubleDouble tol;

        this.lower = lower;
        this.upper = upper;
        this.routine = routine;

        if (routine != Integration2EP.DQAGPE) {
            MipavUtil.displayError("routine must be DQAGPE with this constructor");

            return;
        }

        if (upper.le(lower)) {
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

            if (breakPoints[i].lt(lower)) {
                MipavUtil.displayError("Break point " + breakPoints[i] + " is impossibly < lower");
                ier[0] = 6;

                return;
            }

            if (breakPoints[i].gt(upper)) {
                MipavUtil.displayError("Break point " + breakPoints[i] + " is impossibly > upper");
                ier[0] = 6;

                return;
            }
        } // for (int i = 0; i < npts2 - 2; i++)

        this.epsabs = epsabs;
        this.epsrel = epsrel;

        epmach = DoubleDouble.valueOf(DoubleDouble.EPS);
        tol = ( (DoubleDouble.valueOf(50.0)).multiply(epmach)).max(DoubleDouble.valueOf(5.0E-29));

        if ( (epsabs.le(DoubleDouble.valueOf(0.0))) && (epsrel.lt(tol))) {
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
    public Integration2EP(final DoubleDouble lower, final DoubleDouble upper, final int routine, final int key,
            final DoubleDouble epsabs, final DoubleDouble epsrel, final int limit) {
        DoubleDouble tol;

        this.lower = lower;
        this.upper = upper;
        this.routine = routine;

        if (routine != Integration2EP.DQAGE) {
            MipavUtil.displayError("routine must be DQAGE with this constructor");

            return;
        }

        if (upper.le(lower)) {
            MipavUtil.displayError("upper must be > than lower");
            ier[0] = 6;

            return;
        }

        this.key = key;
        this.epsabs = epsabs;
        this.epsrel = epsrel;

        epmach = DoubleDouble.valueOf(DoubleDouble.EPS);
        tol = ( (DoubleDouble.valueOf(50.0)).multiply(epmach)).max(DoubleDouble.valueOf(5.0E-29));

        if ( (epsabs.le(DoubleDouble.valueOf(0.0))) && (epsrel.lt(tol))) {
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
    public abstract DoubleDouble intFunc(DoubleDouble x);

    private DoubleDouble intFuncTest(final DoubleDouble x) {
        DoubleDouble function = DoubleDouble.valueOf(0.0);
        DoubleDouble lg;
        DoubleDouble var;
        switch (testCase) {
            case 1:
                function = ( (DoubleDouble.valueOf(100.0)).multiply(x.sin())).cos();
                break;
            case 2:
                function = (x.log()).divide( (DoubleDouble.valueOf(1.0)).add( ( (DoubleDouble.valueOf(100.0))
                        .multiply(x)).multiply(x)));
                break;
            case 3:
                function = ( (x.multiply(x)).multiply(x)).multiply( ( ( ( (x.multiply(x)).subtract(DoubleDouble
                        .valueOf(1.0))).multiply( (x.multiply(x)).subtract(DoubleDouble.valueOf(2.0)))).abs()).log());
                break;
            case 4:
                function = (x.log()).divide(x.sqrt());
                break;
            case 5:
                function = (DoubleDouble.valueOf(1.0)).divide( ( ( ( (DoubleDouble.valueOf(5.0)).multiply(x))
                        .multiply(x)).multiply(x)).add(DoubleDouble.valueOf(6.0)));
                break;
            case 6:
                if (x.le(DoubleDouble.valueOf(0.0))) {
                    function = DoubleDouble.valueOf(0.0);
                } else {
                    function = (DoubleDouble.valueOf(1.0)).divide(x.sqrt());
                }
                break;
            case 7:
                if (x.le(DoubleDouble.valueOf(0.0))) {
                    function = DoubleDouble.valueOf(0.0);
                } else {
                    function = x.log();
                }
                break;
            case 8:
                if (x.le(DoubleDouble.valueOf(0.0))) {
                    function = DoubleDouble.valueOf(0.0);
                } else {
                    lg = x.log();
                    var = (DoubleDouble.valueOf(1.0)).add(lg.multiply(lg));
                    function = (DoubleDouble.valueOf(1.0)).divide(var.multiply(var));
                }
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
                } else {
                    function = (x.sqrt()).multiply(x.log());
                }
                break;
            case 101:
                final DoubleDouble oneSeventh = (DoubleDouble.valueOf(1.0)).divide(DoubleDouble.valueOf(7.0));
                final DoubleDouble twoThirds = (DoubleDouble.valueOf(2.0)).divide(DoubleDouble.valueOf(3.0));
                if ( (x.ne(oneSeventh)) && (x.ne(twoThirds))) {
                    function = ( ( (x.subtract(oneSeventh)).abs()).pow( -0.25)).multiply( ( (x.subtract(twoThirds))
                            .abs()).pow( -0.55));
                }
                break;
            case 102:
                if (x.isPositive()) {
                    function = ( (x.sqrt()).multiply(x.log())).divide( ( (x.add(DoubleDouble.valueOf(1.0))).multiply(x
                            .add(DoubleDouble.valueOf(2.0)))));
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
    private void dgtsl(final int n, final DoubleDouble c[], final DoubleDouble d[], final DoubleDouble e[],
            final DoubleDouble b[], final int info[]) {
        int k;
        int kb;
        int kp1;
        int nm1;
        int nm2;
        DoubleDouble t;

        info[0] = 0;
        c[0] = d[0];
        nm1 = n - 1;
        if (nm1 >= 1) {
            d[0] = (DoubleDouble) e[0].clone();
            e[0] = DoubleDouble.valueOf(0.0);
            e[n - 1] = DoubleDouble.valueOf(0.0);
            for (k = 0; k < nm1; k++) {
                kp1 = k + 1;

                // Find the largest of the two rows

                if ( (c[kp1].abs()).ge(c[k].abs())) {

                    // Interchange row

                    t = (DoubleDouble) c[kp1].clone();
                    c[kp1] = (DoubleDouble) c[k].clone();
                    c[k] = (DoubleDouble) t.clone();
                    t = (DoubleDouble) d[kp1].clone();
                    d[kp1] = (DoubleDouble) d[k].clone();
                    d[k] = (DoubleDouble) t.clone();
                    t = (DoubleDouble) e[kp1].clone();
                    e[kp1] = (DoubleDouble) e[k].clone();
                    e[k] = (DoubleDouble) t.clone();
                    t = (DoubleDouble) b[kp1].clone();
                    b[kp1] = (DoubleDouble) b[k].clone();
                    b[k] = (DoubleDouble) t.clone();
                } // if (Math.abs(c[kp1]) >= Math.abs(c[k]))

                // Zero elements

                if (c[k].isZero()) {
                    info[0] = k + 1;
                    return;
                } // if (c[k] == 0.0)

                t = (c[kp1].divide(c[k])).negate();
                c[kp1] = d[kp1].add(t.multiply(d[k]));
                d[kp1] = e[kp1].add(t.multiply(e[k]));
                e[kp1] = DoubleDouble.valueOf(0.0);
                b[kp1] = b[kp1].add(t.multiply(b[k]));
            } // for (k = 0; k < nm1; k++)
        } // if (nm1 >= 1)
        if (c[n - 1].isZero()) {
            info[0] = n;
            return;
        } // if (c[n-1] == 0.0)

        // Back solve

        nm2 = n - 2;
        b[n - 1] = b[n - 1].divide(c[n - 1]);
        if (n == 1) {
            return;
        }
        b[nm1 - 1] = (b[nm1 - 1].subtract(d[nm1 - 1].multiply(b[n - 1]))).divide(c[nm1 - 1]);
        if (nm2 < 1) {
            return;
        }
        for (kb = 1; kb <= nm2; kb++) {
            k = nm2 - kb + 1;
            b[k - 1] = ( (b[k - 1].subtract(d[k - 1].multiply(b[k]))).subtract(e[k - 1].multiply(b[k + 1])))
                    .divide(c[k - 1]);
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
    public void dqagie(final DoubleDouble bound, final int inf, final DoubleDouble epsrel) {

        // Variables ending in 1 denote left subinterval and variables
        // ending in 2 denote right subinterval
        // rlist2 should be length at least (limexp + 2)
        // rlist2 contains the part of the epsilon table still needed for
        // further computations
        DoubleDouble[] rlist2;

        // Index to the interval with the largest error estimate
        final int[] maxErr = new int[1];

        // errMax[0] = elist[maxErr];
        final DoubleDouble[] errMax = new DoubleDouble[1];

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
        final int[] nres = new int[1];

        // Number of elements in rlist2. If an appropriate approximation
        // to the compounded integral has been obtained, it is put in
        // rlist2[numr12-1] after numr12 has been increased by one.
        final int[] numr12 = new int[1];

        // Sum of the errors over the intervals larger than the smallest
        // interval considered up to now
        DoubleDouble erlarg;

        // boolean variable denoting that the routine is attempting to
        // perform extrapolation. That is, before dividing the
        // interval we try to decrease the value of erlarg
        boolean extrap;

        // If noext is true, the extrapolation is no longer allowed.
        boolean noext;

        // length of the smallest interval considered up to now, multiplied
        // by 1.5
        DoubleDouble small;
        DoubleDouble[] res31a;
        final DoubleDouble[] abseps = new DoubleDouble[1];
        final DoubleDouble[] area1 = new DoubleDouble[1];
        DoubleDouble area12;
        final DoubleDouble[] area2 = new DoubleDouble[1];
        DoubleDouble a1;
        DoubleDouble a2;
        DoubleDouble boun;
        DoubleDouble b1;
        DoubleDouble b2;
        DoubleDouble correc;
        final DoubleDouble[] defabs = new DoubleDouble[1];
        final DoubleDouble[] defab1 = new DoubleDouble[1];
        final DoubleDouble[] defab2 = new DoubleDouble[1];
        DoubleDouble dres;
        final DoubleDouble[] error1 = new DoubleDouble[1];
        final DoubleDouble[] error2 = new DoubleDouble[1];
        DoubleDouble error12;
        DoubleDouble ertest;
        final DoubleDouble[] resabs = new DoubleDouble[1];
        final DoubleDouble[] reseps = new DoubleDouble[1];
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
            alist = new DoubleDouble[limit];
            blist = new DoubleDouble[limit];
            rlist = new DoubleDouble[limit];
            elist = new DoubleDouble[limit];
            iord = new int[limit];
            res31a = new DoubleDouble[3];
            rlist2 = new DoubleDouble[52];

            ier[0] = 0;
            neval[0] = 0;
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

            boun = (DoubleDouble) bound.clone();

            if (inf == 2) {
                boun = DoubleDouble.valueOf(0.0);
            } // if (inf == 2)

            dqk15i(boun, inf, DoubleDouble.valueOf(0.0), DoubleDouble.valueOf(1.0), result, abserr, defabs, resabs);

            // Test on accuracy

            last = 1;
            rlist[0] = (DoubleDouble) result[0].clone();
            elist[0] = (DoubleDouble) abserr[0].clone();
            iord[0] = 0;
            dres = (result[0]).abs();
            errBnd = (epsabs).max(epsrel.multiply(dres));

            if ( (abserr[0].le( ( (DoubleDouble.valueOf(100.0)).multiply(epmach)).multiply(defabs[0])))
                    && (abserr[0].gt(errBnd))) {
                ier[0] = 2;
            } // if ((abserr[0].le(((DoubleDouble.valueOf(100.0)).multiply(epmach)).multiply(defabs[0]))) &&
            // (abserr[0].gt(errBnd)))

            if (limit == 1) {
                ier[0] = 1;
            } // if (limit == 1)

            if ( (ier[0] != 0) || ( (abserr[0].le(errBnd)) && (abserr[0].ne(resabs[0])))
                    || (abserr[0].equals(DoubleDouble.valueOf(0.0)))) {
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

            rlist2[0] = (DoubleDouble) result[0].clone();
            errMax[0] = (DoubleDouble) abserr[0].clone();
            maxErr[0] = 0;
            area = (DoubleDouble) result[0].clone();
            errSum = (DoubleDouble) abserr[0].clone();
            abserr[0] = (DoubleDouble) oflow.clone();
            nrmax[0] = 1;
            nres[0] = 0;
            ktmin = 0;
            numr12[0] = 2;
            extrap = false;
            noext = false;
            erlarg = (DoubleDouble) errSum.clone();
            correc = (DoubleDouble) erlarg.clone();
            small = DoubleDouble.valueOf(0.375);
            ertest = (DoubleDouble) errBnd.clone();
            ierro = 0;
            iroff1 = 0;
            iroff2 = 0;
            iroff3 = 0;
            ksgn = -1;

            if (dres.ge( ( (DoubleDouble.valueOf(1.0)).subtract( (DoubleDouble.valueOf(50.0)).multiply(epmach)))
                    .multiply(defabs[0]))) {
                ksgn = 1;
            } // if (dres >= (1.0 - 50.0 * epmach)*defabs[0])

            // main do-loop
            loop: for (last = 2; last <= limit; last++) {

                // Bisect the interval with the nrmax-th largest error estimate.

                a1 = (DoubleDouble) alist[maxErr[0]].clone();
                b1 = (DoubleDouble.valueOf(0.5)).multiply(alist[maxErr[0]].add(blist[maxErr[0]]));
                a2 = (DoubleDouble) b1.clone();
                b2 = (DoubleDouble) blist[maxErr[0]].clone();
                erlast = (DoubleDouble) errMax[0].clone();
                dqk15i(boun, inf, a1, b1, area1, error1, resabs, defab1);
                dqk15i(boun, inf, a2, b2, area2, error2, resabs, defab2);

                // Improve previous approximations to integral and error
                // and test for accuracy

                area12 = area1[0].add(area2[0]);
                error12 = error1[0].add(error2[0]);
                errSum = (errSum.add(error12)).subtract(errMax[0]);
                area = (area.add(area12)).subtract(rlist[maxErr[0]]);

                if ( (defab1[0].ne(error1[0])) && (defab2[0].ne(error2[0]))) {

                    if ( ( ( (rlist[maxErr[0]].subtract(area12)).abs()).le( (DoubleDouble.valueOf(1.0E-5))
                            .multiply(area12.abs())))
                            && (error12.ge( (DoubleDouble.valueOf(0.99)).multiply(errMax[0])))) {

                        if (extrap) {
                            iroff2 = iroff2 + 1;
                        } else {
                            iroff1 = iroff1 + 1;
                        }
                    } // if ((Math.abs(rlist[maxErr[0]] - area12) <=

                    if ( (last > 10) && (error12.gt(errMax[0]))) {
                        iroff3 = iroff3 + 1;
                    } // if ((last > 10) && (error12 > errMax[0]))
                } // if ((defab1[0] != error1[0]) && (defab2[0] != error2[0]))

                rlist[maxErr[0]] = (DoubleDouble) area1[0].clone();
                rlist[last - 1] = (DoubleDouble) area2[0].clone();
                errBnd = (epsabs).max(epsrel.multiply(area.abs()));

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
                if ( ( (a1.abs()).max(b2.abs())).le( ( (DoubleDouble.valueOf(1.0)).add( (DoubleDouble.valueOf(100.0))
                        .multiply(epmach))).multiply( (a2.abs()).add( (DoubleDouble.valueOf(1.0E3)).multiply(uflow))))) {
                    ier[0] = 4;
                } // if (Math.max(Math.abs(a1), Math.abs(b2)) <=

                // Append the newly-created intervals to the list.

                if (error2[0].le(error1[0])) {
                    alist[last - 1] = (DoubleDouble) a2.clone();
                    blist[maxErr[0]] = (DoubleDouble) b1.clone();
                    blist[last - 1] = (DoubleDouble) b2.clone();
                    elist[maxErr[0]] = (DoubleDouble) error1[0].clone();
                    elist[last - 1] = (DoubleDouble) error2[0].clone();
                } // if (error2[0] <= error1[0])
                else {
                    alist[maxErr[0]] = (DoubleDouble) a2.clone();
                    alist[last - 1] = (DoubleDouble) a1.clone();
                    blist[last - 1] = (DoubleDouble) b1.clone();
                    rlist[maxErr[0]] = (DoubleDouble) area2[0].clone();
                    rlist[last - 1] = (DoubleDouble) area1[0].clone();
                    elist[maxErr[0]] = (DoubleDouble) error2[0].clone();
                    elist[last - 1] = (DoubleDouble) error1[0].clone();
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

                    abserr[0] = (DoubleDouble) errSum.clone();
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
                    small = DoubleDouble.valueOf(0.375);
                    erlarg = (DoubleDouble) errSum.clone();
                    ertest = (DoubleDouble) errBnd.clone();
                    rlist2[1] = (DoubleDouble) area.clone();

                    continue;
                } // if (last == 2)

                if (noext) {
                    continue;
                } // if (noext)

                erlarg = erlarg.subtract(erlast);

                if ( ( (b1.subtract(a1)).abs()).gt(small)) {
                    erlarg = erlarg.add(error12);
                } // if (Math.abs(b1 - a1) > small)

                if ( !extrap) {

                    // Test whether the interval to be bisected next is the
                    // smallest interval

                    if ( ( (blist[maxErr[0]].subtract(alist[maxErr[0]])).abs()).gt(small)) {
                        continue;
                    } // if (Math.abs(blist[maxErr[0]] - alist[maxErr[0]]) > small)

                    extrap = true;
                    nrmax[0] = 2;
                } // if (!extrap)

                if ( (ierro != 3) && (erlarg.gt(ertest))) {

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
                        errMax[0] = (DoubleDouble) elist[maxErr[0]].clone();

                        if ( ( (blist[maxErr[0]].subtract(alist[maxErr[0]])).abs()).gt(small)) {
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

                if ( (ktmin > 5) && (abserr[0].lt( (DoubleDouble.valueOf(1.0E-3)).multiply(errSum)))) {
                    ier[0] = 5;
                } // if ((ktmin > 5) && (abserr[0] < 1.0E-3 * errSum))

                if (abseps[0].lt(abserr[0])) {
                    ktmin = 0;
                    abserr[0] = (DoubleDouble) abseps[0].clone();
                    result[0] = (DoubleDouble) reseps[0].clone();
                    correc = (DoubleDouble) erlarg.clone();
                    ertest = epsabs.max(epsrel.multiply(reseps[0].abs()));

                    if (abserr[0].le(ertest)) {
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
                errMax[0] = (DoubleDouble) elist[maxErr[0]].clone();
                nrmax[0] = 1;
                extrap = false;
                small = (DoubleDouble.valueOf(0.5)).multiply(small);
                erlarg = (DoubleDouble) errSum.clone();
            } // for (last = 2; last <= limit; last++)

            // Set final result and error estimate

            if (abserr[0].ne(oflow)) {

                if ( (ier[0] + ierro) != 0) {

                    if (ierro == 3) {
                        abserr[0] = abserr[0].add(correc);
                    } // if (ierro == 3)

                    if (ier[0] == 0) {
                        ier[0] = 3;
                    } // if (ier[0] == 0)

                    if ( (result[0].isZero()) || (area.isZero())) {

                        if (abserr[0].gt(errSum)) {
                            result[0] = DoubleDouble.valueOf(0.0);

                            for (k = 0; k < last; k++) {
                                result[0] = result[0].add(rlist[k]);
                            } // for (k = 0; k < last; k++)

                            abserr[0] = (DoubleDouble) errSum.clone();
                            neval[0] = (30 * last) - 15;

                            if (inf == 2) {
                                neval[0] = 2 * neval[0];
                            } // if (inf == 2)

                            if (ier[0] > 2) {
                                ier[0] = ier[0] - 1;
                            } // if (ier[0] > 2)

                            return;
                        } // if (abserr[0] > errSum)

                        if (area.isZero()) {
                            neval[0] = (30 * last) - 15;

                            if (inf == 2) {
                                neval[0] = 2 * neval[0];
                            } // if (inf == 2)

                            if (ier[0] > 2) {
                                ier[0] = ier[0] - 1;
                            } // if (ier[0] > 2)

                            return;
                        } // if (area == 0.0)

                        if ( (ksgn == -1)
                                && ( (result[0].abs()).max(area.abs()).le( (DoubleDouble.valueOf(1.0E-2))
                                        .multiply(defabs[0])))) {
                            neval[0] = (30 * last) - 15;

                            if (inf == 2) {
                                neval[0] = 2 * neval[0];
                            } // if (inf == 2)

                            if (ier[0] > 2) {
                                ier[0] = ier[0] - 1;
                            } // if (ier[0] > 2)

                            return;
                        } // if ((ksgn == -1) && (Math.max(Math.abs(result[0]),

                        if ( ( (DoubleDouble.valueOf(1.0E-2)).gt(result[0].divide(area)))
                                || ( (result[0].divide(area)).gt(DoubleDouble.valueOf(100.0)))
                                || (errSum.gt(area.abs()))) {
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

                    if ( (abserr[0].divide(result[0].abs())).gt(errSum.divide(area.abs()))) {
                        result[0] = DoubleDouble.valueOf(0.0);

                        for (k = 0; k < last; k++) {
                            result[0] = result[0].add(rlist[k]);
                        } // for (k = 0; k < last; k++)

                        abserr[0] = (DoubleDouble) errSum.clone();
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

                if ( (ksgn == -1)
                        && ( ( (result[0].abs()).max(area.abs())).le( (DoubleDouble.valueOf(1.0E-2))
                                .multiply(defabs[0])))) {
                    neval[0] = (30 * last) - 15;

                    if (inf == 2) {
                        neval[0] = 2 * neval[0];
                    } // if (inf == 2)

                    if (ier[0] > 2) {
                        ier[0] = ier[0] - 1;
                    } // if (ier[0] > 2)

                    return;
                } // if ((ksgn == -1) && (Math.max(Math.abs(result[0]),

                if ( ( (DoubleDouble.valueOf(1.0E-2)).gt(result[0].divide(area)))
                        || ( (result[0].divide(area)).gt(DoubleDouble.valueOf(100.0))) || (errSum.gt(area.abs()))) {
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

            result[0] = DoubleDouble.valueOf(0.0);

            for (k = 0; k < last; k++) {
                result[0] = result[0].add(rlist[k]);
            } // for (k = 0; k < last; k++)

            abserr[0] = (DoubleDouble) errSum.clone();
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
        DoubleDouble[] res31a;

        // rlist2 should be length at least (limexp + 2)
        // rlist2 contains the part of the epsilon table still needed for
        // further computations
        DoubleDouble[] rlist2;

        // Index to the interval with the largest error estimate
        final int[] maxErr = new int[1];

        // errMax[0] = elist[maxErr];
        final DoubleDouble[] errMax = new DoubleDouble[1];

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
        final int[] nres = new int[1];

        // Number of elements in rlist2. If an appropriate approximation
        // to the compounded integral has been obtained, it is put in
        // rlist2[numr12-1] after numr12 has been increased by one.
        final int[] numr12 = new int[1];

        // Sum of the errors over the intervals larger than the smallest
        // interval considered up to now
        DoubleDouble erlarg;

        // boolean variable denoting that the routine is attempting to
        // perform extrapolation. That is, before dividing the
        // interval we try to decrease the value of erlarg
        boolean extrap;

        // If noext is true, the extrapolation is no longer allowed.
        boolean noext;

        // Variables ending in 1 denote left subinterval and variables
        // ending in 2 denote right subinterval
        final DoubleDouble[] abseps = new DoubleDouble[1];
        final DoubleDouble[] area1 = new DoubleDouble[1];
        DoubleDouble area12;
        final DoubleDouble[] area2 = new DoubleDouble[1];
        DoubleDouble a1;
        DoubleDouble a2;
        DoubleDouble b1;
        DoubleDouble b2;
        DoubleDouble correc = DoubleDouble.valueOf(0.0);
        final DoubleDouble[] defabs = new DoubleDouble[1];
        final DoubleDouble[] defab1 = new DoubleDouble[1];
        final DoubleDouble[] defab2 = new DoubleDouble[1];
        DoubleDouble dres;
        final DoubleDouble[] error1 = new DoubleDouble[1];
        DoubleDouble error12;
        final DoubleDouble[] error2 = new DoubleDouble[1];
        DoubleDouble ertest;
        final DoubleDouble[] resa = new DoubleDouble[1];
        DoubleDouble resabs;
        final DoubleDouble[] reseps = new DoubleDouble[1];
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
        final int[] nrmax = new int[1];

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

            ier[0] = 0;
            neval[0] = 0;
            last = 0;
            result[0] = DoubleDouble.valueOf(0.0);
            abserr[0] = DoubleDouble.valueOf(0.0);
            alist[0] = (DoubleDouble) lower.clone();
            blist[0] = (DoubleDouble) upper.clone();
            rlist[0] = DoubleDouble.valueOf(0.0);
            elist[0] = DoubleDouble.valueOf(0.0);
            iord[0] = -1;
            level[0] = 0;

            // If any break points are provided, sort them into an
            // ascending sequence.
            sign = DoubleDouble.valueOf(1.0);

            if (lower.gt(upper)) {
                sign = DoubleDouble.valueOf( -1.0);
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
                            temp = (DoubleDouble) pts[i].clone();
                            pts[i] = (DoubleDouble) pts[j].clone();
                            pts[j] = (DoubleDouble) temp.clone();
                        } // if (pts[i] > pts[j])
                    } // for (j = ip1; j < nintp1; j++)
                } // for (i = 0; i < nint; i++)
            } // if (npts != 0)

            // Compute first integral and error approximations

            resabs = DoubleDouble.valueOf(0.0);

            for (i = 0; i < nint; i++) {
                b1 = (DoubleDouble) pts[i + 1].clone();
                dqk21(a1, b1, area1, error1, defabs, resa);
                abserr[0] = abserr[0].add(error1[0]);
                result[0] = result[0].add(area1[0]);
                ndin[i] = 0;

                if ( (error1[0].equals(resa[0])) && (error1[0].ne(DoubleDouble.valueOf(0.0)))) {
                    ndin[i] = 1;
                }

                resabs = resabs.add(defabs[0]);
                level[i] = 0;
                elist[i] = (DoubleDouble) error1[0].clone();
                alist[i] = (DoubleDouble) a1.clone();
                blist[i] = (DoubleDouble) b1.clone();
                rlist[i] = (DoubleDouble) area1[0].clone();
                iord[i] = i;
                a1 = (DoubleDouble) b1.clone();
            } // for (i = 0; i < nint; i++)

            errSum = DoubleDouble.valueOf(0.0);

            for (i = 0; i < nint; i++) {

                if (ndin[i] == 1) {
                    elist[i] = (DoubleDouble) abserr[0].clone();
                } // if (ndin[i] == 1)

                errSum = errSum.add(elist[i]);
            } // for (i = 0; i < nint; i++)

            // Test on accuracy

            last = nint;
            neval[0] = 21 * nint;
            dres = (result[0].abs());
            errBnd = epsabs.max(dres.multiply(epsrel));

            if ( (abserr[0].le( ( (DoubleDouble.valueOf(100.0)).multiply(epmach)).multiply(resabs)))
                    && (abserr[0].gt(errBnd))) {
                ier[0] = 2;
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
                    ier[0] = 1;
                } // if (limit < npts2)
            } // if (nint != 1)

            if ( (ier[0] != 0) || (abserr[0].le(errBnd))) {

                if (ier[0] > 2) {
                    ier[0] = ier[0] - 1;
                } // if (ier[0] > 2)

                result[0] = result[0].multiply(sign);

                return;
            } // if ((ier[0] != 0) || (abserr[0] <= errBnd))

            // Initialization

            rlist2[0] = (DoubleDouble) result[0].clone();
            maxErr[0] = iord[0];
            errMax[0] = (DoubleDouble) elist[maxErr[0]].clone();
            area = (DoubleDouble) result[0].clone();
            nrmax[0] = 1;
            nres[0] = 0;
            numr12[0] = 1;
            ktmin = 0;
            extrap = false;
            noext = false;
            erlarg = (DoubleDouble) errSum.clone();
            ertest = (DoubleDouble) errBnd.clone();
            levmax = 1;
            iroff1 = 0;
            iroff2 = 0;
            iroff3 = 0;
            ierro = 0;
            abserr[0] = (DoubleDouble) oflow.clone();
            ksgn = -1;

            if (dres.ge( ( (DoubleDouble.valueOf(1.0)).subtract( (DoubleDouble.valueOf(50.0)).multiply(epmach)))
                    .multiply(resabs))) {
                ksgn = 1;
            }

            // Main do-loop
            loop: for (last = npts2; last <= limit; last++) {

                // Bisect the subinterval with the nrmax-th largest error
                // estimate
                levcur = level[maxErr[0]] + 1;
                a1 = (DoubleDouble) alist[maxErr[0]].clone();
                b1 = (DoubleDouble.valueOf(0.5)).multiply(alist[maxErr[0]].add(blist[maxErr[0]]));
                a2 = (DoubleDouble) b1.clone();
                b2 = (DoubleDouble) blist[maxErr[0]].clone();
                erlast = DoubleDouble.valueOf(maxErr[0]);
                dqk21(a1, b1, area1, error1, resa, defab1);
                dqk21(a2, b2, area2, error2, resa, defab2);

                // Improve previous approximations to integral and error
                // and test for accuracy.
                neval[0] = neval[0] + 42;
                area12 = area1[0].add(area2[0]);
                error12 = error1[0].add(error2[0]);
                errSum = (errSum.add(error12)).subtract(errMax[0]);
                area = (area.add(area12)).subtract(rlist[maxErr[0]]);

                if ( (defab1[0].ne(error1[0])) && (defab2[0].ne(error2[0]))) {

                    if ( ( ( (rlist[maxErr[0]].subtract(area12)).abs()).le( (DoubleDouble.valueOf(1.0E-5))
                            .multiply(area12.abs())))
                            && (error12.ge( (DoubleDouble.valueOf(0.99)).multiply(errMax[0])))) {

                        if (extrap) {
                            iroff2 = iroff2 + 1;
                        } else {
                            iroff1 = iroff1 + 1;
                        }
                    }

                    if ( (last > 10) && (error12.ge(errMax[0]))) {
                        iroff3 = iroff3 + 1;
                    }
                } // if ((defab1[0] != error1[0]) && (defab2[0] != error2[0]))

                level[maxErr[0]] = levcur;
                level[last - 1] = levcur;
                rlist[maxErr[0]] = (DoubleDouble) area1[0].clone();
                rlist[last - 1] = (DoubleDouble) area2[0].clone();
                errBnd = epsabs.max(epsrel.multiply(area.abs()));

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

                final DoubleDouble maxVar = (a1.abs()).max(b2.abs());
                final DoubleDouble leftVar = (DoubleDouble.valueOf(1.0)).add( (DoubleDouble.valueOf(100.0))
                        .multiply(epmach));
                final DoubleDouble rightVar = (a2.abs()).add( (DoubleDouble.valueOf(1.0E3)).multiply(uflow));
                if (maxVar.le(leftVar.multiply(rightVar))) {
                    ier[0] = 4;
                }

                // Append the newly created intervals to the list

                if (error2[0].le(error1[0])) {
                    alist[last - 1] = (DoubleDouble) a2.clone();
                    blist[maxErr[0]] = (DoubleDouble) b1.clone();
                    blist[last - 1] = (DoubleDouble) b2.clone();
                    elist[maxErr[0]] = (DoubleDouble) error1[0].clone();
                    elist[last - 1] = (DoubleDouble) error2[0].clone();
                } else {
                    alist[maxErr[0]] = (DoubleDouble) a2.clone();
                    alist[last - 1] = (DoubleDouble) a1.clone();
                    blist[last - 1] = (DoubleDouble) b1.clone();
                    rlist[maxErr[0]] = (DoubleDouble) area2[0].clone();
                    rlist[last - 1] = (DoubleDouble) area1[0].clone();
                    elist[maxErr[0]] = (DoubleDouble) error2[0].clone();
                    elist[last - 1] = (DoubleDouble) error1[0].clone();
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

                    abserr[0] = (DoubleDouble) errSum.clone();

                    if (ier[0] > 2) {
                        ier[0] = ier[0] - 1;
                    }

                    result[0] = result[0].multiply(sign);

                    return;
                } // if (errSum <= errBnd)

                if (ier[0] != 0) {
                    break;
                } // if (ier[0] != 0)

                if (noext) {
                    continue;
                } // if (noext)

                erlarg = erlarg.subtract(erlast);

                if ( (levcur + 1) <= levmax) {
                    erlarg = erlarg.add(error12);
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

                if ( (ierro != 3) && (erlarg.gt(ertest))) {
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
                        errMax[0] = (DoubleDouble) elist[maxErr[0]].clone();

                        if ( (level[maxErr[0]] + 1) <= levmax) {
                            continue loop;
                        } // if ((level[maxErr[0]] + 1) <= levmax)

                        nrmax[0] = nrmax[0] + 1;
                    } // for (k = id; k <= jupbnd; k++)
                } // if ((ierro != 3) && (erlarg > ertest))

                // Perform extrapolation

                numr12[0] = numr12[0] + 1;
                rlist2[numr12[0] - 1] = (DoubleDouble) area.clone();

                if (numr12[0] > 2) {
                    dqelg(numr12, rlist2, reseps, abseps, res31a, nres);
                    ktmin = ktmin + 1;

                    if ( (ktmin > 5) && (abserr[0].lt( (DoubleDouble.valueOf(1.0E-3)).multiply(errSum)))) {
                        ier[0] = 5;
                    } // if ((ktmin > 5) && (abserr < 1.0E-3 * errSum))

                    if (abseps[0].lt(abserr[0])) {
                        ktmin = 0;
                        abserr[0] = (DoubleDouble) abseps[0].clone();
                        result[0] = (DoubleDouble) reseps[0].clone();
                        correc = (DoubleDouble) erlarg.clone();
                        ertest = epsabs.max(epsrel.multiply(reseps[0].abs()));

                        if (abserr[0].lt(ertest)) {
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
                errMax[0] = (DoubleDouble) elist[maxErr[0]].clone();
                nrmax[0] = 1;
                extrap = false;
                levmax = levmax + 1;
                erlarg = (DoubleDouble) errSum.clone();
            } // for (last = npts2; last <= limit; last++)

            // Set the final result.

            if (abserr[0].ne(oflow)) {

                if ( (ier[0] + ierro) != 0) {

                    if (ierro == 3) {
                        abserr[0] = abserr[0].add(correc);
                    }

                    if (ier[0] == 0) {
                        ier[0] = 3;
                    } // if (ier[0] == 0)

                    if ( (result[0].isZero()) || (area.isZero())) {

                        if (abserr[0].gt(errSum)) {
                            result[0] = DoubleDouble.valueOf(0.0);

                            for (k = 0; k < last; k++) {
                                result[0] = result[0].add(rlist[k]);
                            }

                            abserr[0] = (DoubleDouble) errSum.clone();

                            if (ier[0] > 2) {
                                ier[0] = ier[0] - 1;
                            }

                            result[0] = result[0].multiply(sign);

                            return;
                        } // if (abserr[0] > errSum)

                        if (area.isZero()) {

                            if (ier[0] > 2) {
                                ier[0] = ier[0] - 1;
                            }

                            result[0] = result[0].multiply(sign);

                            return;
                        } // if (area == 0.0)

                        if ( (ksgn == -1)
                                && ( ( (result[0].abs()).max(area.abs())).le( (DoubleDouble.valueOf(1.0E-2))
                                        .multiply(resabs)))) {

                            if (ier[0] > 2) {
                                ier[0] = ier[0] - 1;
                            }

                            result[0] = result[0].multiply(sign);

                            return;
                        } // if ((ksgn == -1) && (Math.max(Math.abs(result[0]),

                        if ( ( (DoubleDouble.valueOf(1.0E-2)).gt(result[0].divide(area)))
                                || ( (result[0].divide(area)).gt(DoubleDouble.valueOf(100.0)))
                                || (errSum.gt(area.abs()))) {
                            ier[0] = 6;
                        } // if ((1.0E-2 > (result[0]/area) || ((result[0]/area) >

                        if (ier[0] > 2) {
                            ier[0] = ier[0] - 1;
                        }

                        result[0] = result[0].multiply(sign);

                        return;
                    } // if ((result[0] == 0.0) || (area == 0.0))

                    if ( (abserr[0].divide(result[0].abs())).gt(errSum.divide(area.abs()))) {
                        result[0] = DoubleDouble.valueOf(0.0);

                        for (k = 0; k < last; k++) {
                            result[0] = result[0].add(rlist[k]);
                        }

                        abserr[0] = (DoubleDouble) errSum.clone();

                        if (ier[0] > 2) {
                            ier[0] = ier[0] - 1;
                        }

                        result[0] = result[0].multiply(sign);

                        return;
                    } // if ((abserr[0]/Math.abs(result[0])) > (errSum/Math.abs(area)))
                } // if ((ier[0] + ierro) != 0)

                if ( (ksgn == -1)
                        && ( (result[0].abs()).max( (area.abs())).le( (DoubleDouble.valueOf(1.0E-2)).multiply(resabs)))) {

                    if (ier[0] > 2) {
                        ier[0] = ier[0] - 1;
                    }

                    result[0] = result[0].multiply(sign);

                    return;
                } // if ((ksgn == -1) && (Math.max(Math.abs(result[0]),

                if ( ( (DoubleDouble.valueOf(1.0E-2)).gt(result[0].divide(area)))
                        || ( (result[0].divide(area)).gt(DoubleDouble.valueOf(100.0))) || (errSum.gt(area.abs()))) {
                    ier[0] = 6;
                } // if ((1.0E-2 > (result[0]/area) || ((result[0]/area) >

                if (ier[0] > 2) {
                    ier[0] = ier[0] - 1;
                }

                result[0] = result[0].multiply(sign);

                return;
            } // if (abserr[0] != oflow)

            result[0] = DoubleDouble.valueOf(0.0);

            for (k = 0; k < last; k++) {
                result[0] = result[0].add(rlist[k]);
            }

            abserr[0] = (DoubleDouble) errSum.clone();

            if (ier[0] > 2) {
                ier[0] = ier[0] - 1;
            }

            result[0] = result[0].multiply(sign);

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
    public DoubleDouble getAbserr() {
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
    public DoubleDouble getIntegral() {
        return result[0];
    }

    /**
     * DOCUMENT ME!
     * 
     * @return neval[0
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
     * c***description c c computation of a definite integral c standard fortran subroutine c DoubleDouble precision
     * version
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
        final DoubleDouble[] errMax = new DoubleDouble[1];

        // Sum of the integrals over the subintervals
        DoubleDouble area;

        // Sum of the errors over the subintervals
        DoubleDouble errSum;

        // Requested accuracy max(epsabs, epsrel * abs(result))
        DoubleDouble errBnd;
        final DoubleDouble[] area1 = new DoubleDouble[1];
        DoubleDouble area12;
        final DoubleDouble[] area2 = new DoubleDouble[1];
        DoubleDouble a1;
        DoubleDouble a2;
        DoubleDouble b1;
        DoubleDouble b2;
        final DoubleDouble[] defabs = new DoubleDouble[1];
        final DoubleDouble[] defab1 = new DoubleDouble[1];
        final DoubleDouble[] defab2 = new DoubleDouble[1];
        final DoubleDouble[] error1 = new DoubleDouble[1];
        final DoubleDouble[] error2 = new DoubleDouble[1];
        DoubleDouble error12;
        final DoubleDouble[] resabs = new DoubleDouble[1];
        int iroff1;
        int iroff2;
        int k;
        int keyf;
        final int[] nrmax = new int[1];

        try {
            alist = new DoubleDouble[limit];
            blist = new DoubleDouble[limit];
            rlist = new DoubleDouble[limit];
            elist = new DoubleDouble[limit];
            iord = new int[limit];

            ier[0] = 0;
            neval[0] = 0;
            last = 0;
            result[0] = DoubleDouble.valueOf(0.0);
            abserr[0] = DoubleDouble.valueOf(0.0);
            alist[0] = (DoubleDouble) lower.clone();
            blist[0] = (DoubleDouble) upper.clone();
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
            rlist[0] = (DoubleDouble) result[0].clone();
            elist[0] = (DoubleDouble) abserr[0].clone();
            iord[0] = 0;

            // Test on accuracy

            errBnd = epsabs.max(epsrel.multiply(result[0].abs()));

            if ( (abserr[0].le( ( (DoubleDouble.valueOf(50.0)).multiply(epmach)).multiply(defabs[0])))
                    && (abserr[0].gt(errBnd))) {
                ier[0] = 2;
            } // if ((abserr[0] <= 50.0 * epmach * defabs[0]) &&

            if (limit == 1) {
                ier[0] = 1;
            } // if (limit == 1)

            if ( (ier[0] != 0) || ( (abserr[0].le(errBnd)) && (abserr[0].ne(resabs[0]))) || (abserr[0].isZero())) {

                if (keyf != 1) {
                    neval[0] = ( (10 * keyf) + 1) * ( (2 * neval[0]) + 1);
                } // if (keyf != 1)
                else {
                    neval[0] = (30 * neval[0]) + 15;
                } // else

                return;
            } // if ((ier[0] != 0) ||

            // Initialization

            errMax[0] = (DoubleDouble) abserr[0].clone();
            maxErr[0] = 0;
            area = (DoubleDouble) result[0].clone();
            errSum = (DoubleDouble) abserr[0].clone();
            nrmax[0] = 1;
            iroff1 = 0;
            iroff2 = 0;

            // Main do-loop

            for (last = 2; last <= limit; last++) {

                // Bisect the subinterval with the largest error estimate.

                a1 = (DoubleDouble) alist[maxErr[0]].clone();
                b1 = (DoubleDouble.valueOf(0.5)).multiply(alist[maxErr[0]].add(blist[maxErr[0]]));
                a2 = (DoubleDouble) b1.clone();
                b2 = (DoubleDouble) blist[maxErr[0]].clone();

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
                area12 = area1[0].add(area2[0]);
                error12 = error1[0].add(error2[0]);
                errSum = (errSum.add(error12)).subtract(errMax[0]);
                area = (area.add(area12)).subtract(rlist[maxErr[0]]);

                if ( (defab1[0].ne(error1[0])) && (defab2[0].ne(error2[0]))) {

                    if ( ( ( (rlist[maxErr[0]].subtract(area12)).abs()).le( (DoubleDouble.valueOf(1.0E-5))
                            .multiply(area12.abs())))
                            && (error12.ge( (DoubleDouble.valueOf(0.99)).multiply(errMax[0])))) {
                        iroff1 = iroff1 + 1;
                    } // if ((Math.abs(rlist[maxErr[0]] - area12) <=

                    if ( (last > 10) && (error12.gt(errMax[0]))) {
                        iroff2 = iroff2 + 1;
                    } // if ((last > 10) && (error12 > errMax[0]))
                } // if ((defab1[0] != error1[0]) && (defab2[0] != error2[0]))

                rlist[maxErr[0]] = (DoubleDouble) area1[0].clone();
                rlist[last - 1] = (DoubleDouble) area2[0].clone();
                errBnd = epsabs.max(epsrel.multiply(area.abs()));

                if (errSum.gt(errBnd)) {

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

                    final DoubleDouble maxVal = (a1.abs()).max(b2.abs());
                    final DoubleDouble leftVal = (DoubleDouble.valueOf(1.0)).add( (DoubleDouble.valueOf(100.0))
                            .multiply(epmach));
                    final DoubleDouble rightVal = (a2.abs()).add( (DoubleDouble.valueOf(1.0E3)).multiply(uflow));
                    if (maxVal.le(leftVal.multiply(rightVal))) {
                        ier[0] = 3;
                    } // if (Math.max(Math.abs(a1), Math.abs(b2)) <=
                } // if (errSum > errBnd)

                // Append the newly created intervals to the list

                if (error2[0].le(error1[0])) {
                    alist[last - 1] = (DoubleDouble) a2.clone();
                    blist[maxErr[0]] = (DoubleDouble) b1.clone();
                    blist[last - 1] = (DoubleDouble) b2.clone();
                    elist[maxErr[0]] = (DoubleDouble) error1[0].clone();
                    elist[last - 1] = (DoubleDouble) error2[0].clone();
                } // if (error2[0] <= error1[0])
                else {
                    alist[maxErr[0]] = (DoubleDouble) a2.clone();
                    alist[last - 1] = (DoubleDouble) a1.clone();
                    blist[last - 1] = (DoubleDouble) b1.clone();
                    rlist[maxErr[0]] = (DoubleDouble) area2[0].clone();
                    rlist[last - 1] = (DoubleDouble) area1[0].clone();
                    elist[maxErr[0]] = (DoubleDouble) error2[0].clone();
                    elist[last - 1] = (DoubleDouble) error1[0].clone();
                } // else

                // Call subroutine dqpsrt to maintain the descending ordering
                // in the list of error estimates and select the subinterval
                // with the largest error estimate (to be bisected next).

                dqpsrt(limit, last, maxErr, errMax, elist, iord, nrmax);

                if ( (ier[0] != 0) || (errSum.le(errBnd))) {
                    break;
                } // if (ier[0] != 0) || (errSum <= errBnd))
            } // for (last = 2; last <= limit; last++)

            // Compute final result.

            result[0] = DoubleDouble.valueOf(0.0);

            for (k = 0; k < last; k++) {
                result[0] = result[0].add(rlist[k]);
            } // for (k = 0; k < last; k++)

            abserr[0] = (DoubleDouble) errSum.clone();

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
     * c***description c c computation of a definite integral c standard fortran subroutine c DoubleDouble precision
     * version
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
        DoubleDouble[] rlist2;

        // Index to the interval with the largest error estimate
        final int[] maxErr = new int[1];

        // errMax[0] = elist[maxErr];
        final DoubleDouble[] errMax = new DoubleDouble[1];

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
        final int[] nres = new int[1];

        // Number of elements in rlist2. If an appropriate approximation
        // to the compounded integral has been obtained, it is put in
        // rlist2[numr12-1] after numr12 has been increased by one.
        final int[] numr12 = new int[1];

        // Sum of the errors over the intervals larger than the smallest
        // interval considered up to now
        DoubleDouble erlarg;

        // boolean variable denoting that the routine is attempting to
        // perform extrapolation. That is, before dividing the
        // interval we try to decrease the value of erlarg
        boolean extrap;

        // If noext is true, the extrapolation is no longer allowed.
        boolean noext;

        // length of the smallest interval considered up to now, multiplied
        // by 1.5
        DoubleDouble small;
        DoubleDouble[] res31a;
        final DoubleDouble[] abseps = new DoubleDouble[1];
        final DoubleDouble[] area1 = new DoubleDouble[1];
        DoubleDouble area12;
        final DoubleDouble[] area2 = new DoubleDouble[1];
        DoubleDouble a1;
        DoubleDouble a2;
        DoubleDouble b1;
        DoubleDouble b2;
        DoubleDouble correc;
        final DoubleDouble[] defabs = new DoubleDouble[1];
        final DoubleDouble[] defab1 = new DoubleDouble[1];
        final DoubleDouble[] defab2 = new DoubleDouble[1];
        DoubleDouble dres;
        final DoubleDouble[] error1 = new DoubleDouble[1];
        final DoubleDouble[] error2 = new DoubleDouble[1];
        DoubleDouble error12;
        DoubleDouble ertest;
        final DoubleDouble[] resabs = new DoubleDouble[1];
        final DoubleDouble[] reseps = new DoubleDouble[1];
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
            alist = new DoubleDouble[limit];
            blist = new DoubleDouble[limit];
            rlist = new DoubleDouble[limit];
            elist = new DoubleDouble[limit];
            iord = new int[limit];
            rlist2 = new DoubleDouble[52];
            res31a = new DoubleDouble[3];

            ier[0] = 0;
            neval[0] = 0;
            last = 0;
            result[0] = DoubleDouble.valueOf(0.0);
            abserr[0] = DoubleDouble.valueOf(0.0);
            alist[0] = (DoubleDouble) lower.clone();
            blist[0] = (DoubleDouble) upper.clone();
            rlist[0] = DoubleDouble.valueOf(0.0);
            elist[0] = DoubleDouble.valueOf(0.0);

            // First approximation to the integral

            ierro = 0;
            dqk21(lower, upper, result, abserr, defabs, resabs);

            // Test on accuracy.

            dres = (result[0].abs());
            errBnd = epsabs.max(epsrel.multiply(dres));
            last = 1;
            rlist[0] = (DoubleDouble) result[0].clone();
            elist[0] = (DoubleDouble) abserr[0].clone();
            iord[0] = 0;

            if ( (abserr[0].le( ( (DoubleDouble.valueOf(100.0)).multiply(epmach)).multiply(defabs[0])))
                    && (abserr[0].gt(errBnd))) {
                ier[0] = 2;
            } // if ((abserr[0] <= 100.0 * epmach * defabs[0]) &&

            if (limit == 1) {
                ier[0] = 1;
            } // if (limit == 1)

            if ( (ier[0] != 0) || ( (abserr[0].le(errBnd)) && (abserr[0].ne(resabs[0]))) || (abserr[0].isZero())) {
                neval[0] = (42 * last) - 21;

                return;
            } // if ((ier[0] != 0) || ((abserr[0] <= errBnd) && (abserr[0] !=

            // Initialization

            rlist2[0] = (DoubleDouble) result[0].clone();
            errMax[0] = (DoubleDouble) abserr[0].clone();
            maxErr[0] = 0;
            area = (DoubleDouble) result[0].clone();
            errSum = (DoubleDouble) abserr[0].clone();
            erlarg = (DoubleDouble) errSum.clone();
            correc = (DoubleDouble) erlarg.clone();
            small = (DoubleDouble.valueOf(0.375)).multiply( (upper.subtract(lower)).abs());
            ertest = (DoubleDouble) errBnd.clone();
            abserr[0] = (DoubleDouble) oflow.clone();
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

            if (dres.ge( ( (DoubleDouble.valueOf(1.0)).subtract( (DoubleDouble.valueOf(50.0)).multiply(epmach)))
                    .multiply(defabs[0]))) {
                ksgn = 1;
            } // if (dres >= (1.0 - 50.0 * epmach)*defabs[0])

            // Main do-loop
            loop: for (last = 2; last <= limit; last++) {

                // Bisect the subinterval with the nrmax-th largest error
                // estimate.

                a1 = (DoubleDouble) alist[maxErr[0]].clone();
                b1 = (DoubleDouble.valueOf(0.5)).multiply(alist[maxErr[0]].add(blist[maxErr[0]]));
                a2 = (DoubleDouble) b1.clone();
                b2 = (DoubleDouble) blist[maxErr[0]].clone();
                erlast = (DoubleDouble) errMax[0].clone();
                dqk21(a1, b1, area1, error1, resabs, defab1);
                dqk21(a2, b2, area2, error2, resabs, defab2);

                // Improve previous approximations to integral and error
                // and test for accuracy

                area12 = area1[0].add(area2[0]);
                error12 = error1[0].add(error2[0]);
                errSum = (errSum.add(error12)).subtract(errMax[0]);
                area = (area.add(area12)).subtract(rlist[maxErr[0]]);

                if ( (defab1[0].ne(error1[0])) && (defab2[0].ne(error2[0]))) {

                    if ( ( ( (rlist[maxErr[0]].subtract(area12)).abs()).le( (DoubleDouble.valueOf(1.0E-5))
                            .multiply(area12.abs())))
                            && (error12.ge( (DoubleDouble.valueOf(0.99)).multiply(errMax[0])))) {

                        if (extrap) {
                            iroff2 = iroff2 + 1;
                        } // if (extrap)
                        else {
                            iroff1 = iroff1 + 1;
                        }
                    } // if ((Math.abs(rlist[maxErr[0]] - area12) <=

                    if ( (last > 10) && (error12.gt(errMax[0]))) {
                        iroff3 = iroff3 + 1;
                    } // if ((last > 10) && (error12 > errMax[0]))
                } // if ((defab1[0] != error1[0]) && (defab2[0] != error2[0]))

                rlist[maxErr[0]] = (DoubleDouble) area1[0].clone();
                rlist[last - 1] = (DoubleDouble) area2[0].clone();
                errBnd = epsabs.max(epsrel.multiply(area.abs()));

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

                final DoubleDouble maxVal = (a1.abs()).max(b2.abs());
                final DoubleDouble leftVal = (DoubleDouble.valueOf(1.0)).add( (DoubleDouble.valueOf(100.0))
                        .multiply(epmach));
                final DoubleDouble rightVal = (a2.abs()).add( (DoubleDouble.valueOf(1.0E3)).multiply(uflow));
                if (maxVal.le(leftVal.multiply(rightVal))) {
                    ier[0] = 4;
                } // if (Math.max(Math.abs(a1), Math.abs(b2)) <=

                // Append the newly-created intervals to the list.

                if (error2[0].le(error1[0])) {
                    alist[last - 1] = (DoubleDouble) a2.clone();
                    blist[maxErr[0]] = (DoubleDouble) b1.clone();
                    blist[last - 1] = (DoubleDouble) b2.clone();
                    elist[maxErr[0]] = (DoubleDouble) error1[0].clone();
                    elist[last - 1] = (DoubleDouble) error2[0].clone();
                } // if (error2[0] <= error1[0])
                else {
                    alist[maxErr[0]] = (DoubleDouble) a2.clone();
                    alist[last - 1] = (DoubleDouble) a1.clone();
                    blist[last - 1] = (DoubleDouble) b1.clone();
                    rlist[maxErr[0]] = (DoubleDouble) area2[0].clone();
                    rlist[last - 1] = (DoubleDouble) area1[0].clone();
                    elist[maxErr[0]] = (DoubleDouble) error2[0].clone();
                    elist[last - 1] = (DoubleDouble) error1[0].clone();
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

                    abserr[0] = (DoubleDouble) errSum.clone();

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
                    small = (DoubleDouble.valueOf(0.375)).multiply( (upper.subtract(lower)).abs());
                    erlarg = (DoubleDouble) errSum.clone();
                    ertest = (DoubleDouble) errBnd.clone();
                    rlist2[1] = (DoubleDouble) area.clone();

                    continue;
                } // if (last == 2)

                if (noext) {
                    continue;
                } // if (noext)

                erlarg = erlarg.subtract(erlast);

                if ( ( (b1.subtract(a1)).abs()).gt(small)) {
                    erlarg = erlarg.add(error12);
                } // if (Math.abs(b1 - a1) > small)

                if ( !extrap) {

                    // Test whether the interval to be bisected next is the
                    // smallest interval.

                    if ( ( (blist[maxErr[0]].subtract(alist[maxErr[0]])).abs()).gt(small)) {
                        continue;
                    } // if (Math.abs(blist[maxErr[0]] - alist[maxErr[0]]) > small)

                    extrap = true;
                    nrmax[0] = 2;
                } // if (!extrap)

                if ( (ierro != 3) && (erlarg.gt(ertest))) {

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
                        errMax[0] = (DoubleDouble) elist[maxErr[0]].clone();

                        if ( ( (blist[maxErr[0]].subtract(alist[maxErr[0]])).abs()).gt(small)) {
                            continue loop;
                        } // if (Math.abs(blist[maxErr[0]] - alist[maxErr[0]]) > small)

                        nrmax[0] = nrmax[0] + 1;
                    } // for (k = id; k <= jupbnd; k++)
                } // if ((ierro != 3) && (erlarg > ertest))

                // Perform extrapolation

                numr12[0] = numr12[0] + 1;
                rlist2[numr12[0] - 1] = (DoubleDouble) area.clone();
                dqelg(numr12, rlist2, reseps, abseps, res31a, nres);
                ktmin = ktmin + 1;

                if ( (ktmin > 5) && (abserr[0].lt( (DoubleDouble.valueOf(1.0E-3)).multiply(errSum)))) {
                    ier[0] = 5;
                } // if ((ktmin > 5) && (abserr[0] < 1.0E-3 * errSum))

                if (abseps[0].lt(abserr[0])) {
                    ktmin = 0;
                    abserr[0] = (DoubleDouble) abseps[0].clone();
                    result[0] = (DoubleDouble) reseps[0].clone();
                    correc = (DoubleDouble) erlarg.clone();
                    ertest = epsabs.max(epsrel.multiply(reseps[0].abs()));

                    if (abserr[0].le(ertest)) {
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
                errMax[0] = (DoubleDouble) elist[maxErr[0]].clone();
                nrmax[0] = 1;
                extrap = false;
                small = (DoubleDouble.valueOf(0.5)).multiply(small);
                erlarg = (DoubleDouble) errSum.clone();
            } // for (last = 2; last <= limit; last++)

            // Set final result and error estimate

            if (abserr[0].ne(oflow)) {

                if ( (ier[0] + ierro) != 0) {

                    if (ierro == 3) {
                        abserr[0] = abserr[0].add(correc);
                    } // if (ierro == 3)

                    if (ier[0] == 0) {
                        ier[0] = 3;
                    } // if (ier[0] == 0)

                    if ( (result[0].isZero()) || (area.isZero())) {

                        if (abserr[0].gt(errSum)) {
                            result[0] = DoubleDouble.valueOf(0.0);

                            for (k = 0; k < last; k++) {
                                result[0] = result[0].add(rlist[k]);
                            } // for (k = 0; k < last; k++)

                            abserr[0] = (DoubleDouble) errSum.clone();

                            if (ier[0] > 2) {
                                ier[0] = ier[0] - 1;
                            } // if (ier[0] > 2)

                            neval[0] = (42 * last) - 21;

                            return;
                        } // if (abserr[0] > errSum)

                        if (area.isZero()) {

                            if (ier[0] > 2) {
                                ier[0] = ier[0] - 1;
                            } // if (ier[0] > 2)

                            neval[0] = (42 * last) - 21;

                            return;
                        } // if (area.isZero())

                        if ( (ksgn == -1)
                                && ( (result[0].abs()).max( (area.abs())).le( (DoubleDouble.valueOf(1.0E-2))
                                        .multiply(defabs[0])))) {

                            if (ier[0] > 2) {
                                ier[0] = ier[0] - 1;
                            } // if (ier[0] > 2)

                            neval[0] = (42 * last) - 21;

                            return;
                        } // if ((ksgn == -1) && (Math.max(Math.abs(result[0]),

                        if ( ( (DoubleDouble.valueOf(1.0E-2)).gt(result[0].divide(area)))
                                || ( (result[0].divide(area)).gt(DoubleDouble.valueOf(100.0)))
                                || (errSum.gt(area.abs()))) {
                            ier[0] = 6;
                        } // if ((1.0E-2 > (result[0]/area)) || (result[0]/area > 100.0) ||

                        if (ier[0] > 2) {
                            ier[0] = ier[0] - 1;
                        } // if (ier[0] > 2)

                        neval[0] = (42 * last) - 21;

                        return;
                    } // if ((result[0].isZero()0 || (area.isZero()))

                    if ( (abserr[0].divide(result[0].abs())).gt(errSum.divide(area.abs()))) {
                        result[0] = DoubleDouble.valueOf(0.0);

                        for (k = 0; k < last; k++) {
                            result[0] = result[0].add(rlist[k]);
                        } // for (k = 0; k < last; k++)

                        abserr[0] = (DoubleDouble) errSum.clone();

                        if (ier[0] > 2) {
                            ier[0] = ier[0] - 1;
                        } // if (ier[0] > 2)

                        neval[0] = (42 * last) - 21;

                        return;
                    } // if (abserr[0]/Math.abs(result[0]) > errSum/Math.abs(area))
                } // if ((ier[0] + ierro) != 0)

                if ( (ksgn == -1)
                        && ( (result[0].abs()).max( (area.abs())).le( (DoubleDouble.valueOf(1.0E-2))
                                .multiply(defabs[0])))) {

                    if (ier[0] > 2) {
                        ier[0] = ier[0] - 1;
                    } // if (ier[0] > 2)

                    neval[0] = (42 * last) - 21;

                    return;
                } // if ((ksgn == -1) && (Math.max(Math.abs(result[0]),

                if ( ( (DoubleDouble.valueOf(1.0E-2)).gt(result[0].divide(area)))
                        || ( (result[0].divide(area)).gt(DoubleDouble.valueOf(100.0))) || (errSum.gt(area.abs()))) {
                    ier[0] = 6;
                } // if ((1.0E-2 > (result[0]/area)) || (result[0]/area > 100.0) ||

                if (ier[0] > 2) {
                    ier[0] = ier[0] - 1;
                } // if (ier[0] > 2)

                neval[0] = (42 * last) - 21;

                return;
            } // if (abserr[0] != oflow)

            result[0] = DoubleDouble.valueOf(0.0);

            for (k = 0; k < last; k++) {
                result[0] = result[0].add(rlist[k]);
            } // for (k = 0; k < last; k++)

            abserr[0] = (DoubleDouble) errSum.clone();

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
        final DoubleDouble[] errMax = new DoubleDouble[1];

        // Sum of the integrals over the subintervals
        DoubleDouble area;

        // Sum of the errors over the subintervals
        DoubleDouble errSum;

        // Requested accuracy max(epsabs, epsrel * abs(result))
        DoubleDouble errBnd;

        DoubleDouble aa;
        final DoubleDouble area1[] = new DoubleDouble[1];
        DoubleDouble area12;
        final DoubleDouble area2[] = new DoubleDouble[1];
        DoubleDouble a1;
        DoubleDouble a2;
        DoubleDouble bb;
        DoubleDouble b1;
        DoubleDouble b2;
        final DoubleDouble error1[] = new DoubleDouble[1];
        DoubleDouble erro12;
        final DoubleDouble error2[] = new DoubleDouble[1];
        int iroff1;
        int iroff2;
        int k;
        final int krule[] = new int[1];
        final int nev[] = new int[1];
        final int nrmax[] = new int[1];

        try {
            alist = new DoubleDouble[limit];
            blist = new DoubleDouble[limit];
            rlist = new DoubleDouble[limit];
            elist = new DoubleDouble[limit];
            iord = new int[limit];

            neval[0] = 0;
            last = 0;
            alist[0] = (DoubleDouble) lower.clone();
            blist[0] = (DoubleDouble) upper.clone();
            rlist[0] = DoubleDouble.valueOf(0.0);
            elist[0] = DoubleDouble.valueOf(0.0);
            iord[0] = -1;
            result[0] = DoubleDouble.valueOf(0.0);
            abserr[0] = DoubleDouble.valueOf(0.0);

            // First approximation to the integral
            aa = (DoubleDouble) lower.clone();
            bb = (DoubleDouble) upper.clone();
            if (lower.gt(upper)) {
                aa = (DoubleDouble) upper.clone();
                bb = (DoubleDouble) lower.clone();
            }
            ier[0] = 0;
            krule[0] = 1;
            dqc25c(aa, bb, c, result, abserr, krule, neval);
            last = 1;
            rlist[0] = (DoubleDouble) result[0].clone();
            elist[0] = (DoubleDouble) abserr[0].clone();
            iord[0] = 0;
            alist[0] = (DoubleDouble) lower.clone();
            blist[0] = (DoubleDouble) upper.clone();

            // Test on accuracy

            errBnd = epsabs.max(epsrel.multiply(result[0].abs()));
            if (limit == 1) {
                ier[0] = 1;
            }
            if ( (abserr[0].lt( ( (DoubleDouble.valueOf(0.01)).multiply(result[0].abs())).min(errBnd)))
                    || (ier[0] == 1)) {
                if (aa.equals(upper)) {
                    result[0] = result[0].negate();
                }
                return;
            }

            // Initialization

            alist[0] = (DoubleDouble) aa.clone();
            blist[0] = (DoubleDouble) bb.clone();
            rlist[0] = (DoubleDouble) result[0].clone();
            errMax[0] = (DoubleDouble) abserr[0].clone();
            maxErr[0] = 0;
            area = (DoubleDouble) result[0].clone();
            errSum = (DoubleDouble) abserr[0].clone();
            nrmax[0] = 1;
            iroff1 = 0;
            iroff2 = 0;

            // Main for loop

            for (last = 2; last <= limit; last++) {

                // Biset the subinterval with the nrmax-th largest error estimate.

                a1 = (DoubleDouble) alist[maxErr[0]].clone();
                b1 = (DoubleDouble.valueOf(0.5)).multiply(alist[maxErr[0]].add(blist[maxErr[0]]));
                b2 = (DoubleDouble) blist[maxErr[0]].clone();
                if ( (c.le(b1)) && (c.gt(a1))) {
                    b1 = (DoubleDouble.valueOf(0.5)).multiply(c.add(b2));
                }
                if ( (c.gt(b1)) && (c.lt(b2))) {
                    b1 = (DoubleDouble.valueOf(0.5)).multiply(a1.add(c));
                }
                a2 = (DoubleDouble) b1.clone();
                krule[0] = 2;
                dqc25c(a1, b1, c, area1, error1, krule, nev);
                neval[0] = neval[0] + nev[0];
                dqc25c(a2, b2, c, area2, error2, krule, nev);
                neval[0] = neval[0] + nev[0];

                // Improve previous approximations to integral
                // and error and test for accuracy.

                area12 = area1[0].add(area2[0]);
                erro12 = error1[0].add(error2[0]);
                errSum = (errSum.add(erro12)).subtract(errMax[0]);
                area = (area.add(area12)).subtract(rlist[maxErr[0]]);
                if ( ( ( (rlist[maxErr[0]].subtract(area12)).abs()).lt( (DoubleDouble.valueOf(1.0E-5)).multiply(area12
                        .abs())))
                        && (erro12.ge( (DoubleDouble.valueOf(0.99)).multiply(errMax[0]))) && (krule[0] == 0)) {
                    iroff1 = iroff1 + 1;
                }
                if ( (last > 10) && (erro12.gt(errMax[0])) && (krule[0] == 0)) {
                    iroff2 = iroff2 + 1;
                }
                rlist[maxErr[0]] = (DoubleDouble) area1[0].clone();
                rlist[last - 1] = (DoubleDouble) area2[0].clone();
                errBnd = epsabs.max(epsrel.multiply(area.abs()));
                if (errSum.gt(errBnd)) {

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
                    if ( ( (a1.abs()).max(b2.abs())).le( ( (DoubleDouble.valueOf(1.0)).add( (DoubleDouble
                            .valueOf(100.0)).multiply(epmach))).multiply( (a2.abs()).add( (DoubleDouble.valueOf(1.0E3))
                            .multiply(uflow))))) {
                        ier[0] = 3;
                    }
                } // if (errSum > errBnd)

                // Append the newly created intervals to the list

                if (error2[0].le(error1[0])) {
                    alist[last - 1] = (DoubleDouble) a2.clone();
                    blist[maxErr[0]] = (DoubleDouble) b1.clone();
                    blist[last - 1] = (DoubleDouble) b2.clone();
                    elist[maxErr[0]] = (DoubleDouble) error1[0].clone();
                    elist[last - 1] = (DoubleDouble) error2[0].clone();
                } else {
                    alist[maxErr[0]] = (DoubleDouble) a2.clone();
                    alist[last - 1] = (DoubleDouble) a1.clone();
                    blist[last - 1] = (DoubleDouble) b1.clone();
                    rlist[maxErr[0]] = (DoubleDouble) area2[0].clone();
                    rlist[last - 1] = (DoubleDouble) area1[0].clone();
                    elist[maxErr[0]] = (DoubleDouble) error2[0].clone();
                    elist[last - 1] = (DoubleDouble) error1[0].clone();
                }

                // Call subroutine dqpsrt to maintain the descending ordering
                // in the list of error estimates and select the subinterval
                // with the nrmax-th largest error estimate (to be bisected next).

                dqpsrt(limit, last, maxErr, errMax, elist, iord, nrmax);
                if ( (ier[0] != 0) || (errSum.le(errBnd))) {
                    break;
                }
            } // for (last = 2; last <= limit; last++)

            // Compute final result

            result[0] = DoubleDouble.valueOf(0.0);
            for (k = 0; k < last; k++) {
                result[0] = result[0].add(rlist[k]);
            }
            abserr[0] = errSum;
            if (aa == upper) {
                result[0] = result[0].negate();
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
     * integrals c standard fortran subroutine c DoubleDouble precision version c c parameters c on entry c f -
     * DoubleDouble precision c function subprogram defining the integrand c function f(x). the actual name for f needs
     * to c be declared e x t e r n a l in the driver program. c c a - DoubleDouble precision c lower limit of
     * integration c c omega - DoubleDouble precision c parameter in the weight function c c integr - integer c
     * indicates which weight function is used c integr = 1 w(x) = cos(omega*x) c integr = 2 w(x) = sin(omega*x) c if
     * integr.ne.1.and.integr.ne.2, the routine will c end with ier = 6. c c epsabs - DoubleDouble precision c absolute
     * accuracy requested, epsabs.gt.0 c if epsabs.le.0, the routine will end with ier = 6. c c limlst - integer c
     * limlst gives an upper bound on the number of c cycles, limlst.ge.1. c if limlst.lt.3, the routine will end with
     * ier = 6. c c limit - integer c gives an upper bound on the number of subintervals c allowed in the partition of
     * each cycle, limit.ge.1 c each cycle, limit.ge.1. c c maxp1 - integer c gives an upper bound on the number of c
     * chebyshev moments which can be stored, i.e. c for the intervals of lengths abs(b-a)*2**(-l), c l=0,1, ...,
     * maxp1-2, maxp1.ge.1 c c on return c result - DoubleDouble precision c approximation to the integral x c c abserr -
     * DoubleDouble precision c estimate of the modulus of the absolute error, c which should equal or exceed
     * abs(i-result) c c neval - integer c number of integrand evaluations c c ier - ier = 0 normal and reliable
     * termination of c the routine. it is assumed that the c requested accuracy has been achieved. c ier.gt.0 abnormal
     * termination of the routine. the c estimates for integral and error are less c reliable. it is assumed that the
     * requested c accuracy has not been achieved. c error messages c if omega.ne.0 c ier = 1 maximum number of cycles
     * allowed c has been achieved., i.e. of subintervals c (a+(k-1)c,a+kc) where c c =
     * (2*int(abs(omega))+1)*pi/abs(omega), c for k = 1, 2, ..., lst. c one can allow more cycles by increasing c the
     * value of limlst (and taking the c according dimension adjustments into c account). c examine the array iwork
     * which contains c the error flags on the cycles, in order to c look for eventual local integration c difficulties.
     * if the position of a local c difficulty can be determined (e.g. c singularity, discontinuity within the c
     * interval) one will probably gain from c splitting up the interval at this point c and calling appropriate
     * integrators on c the subranges. c = 4 the extrapolation table constructed for c convergence acceleration of the
     * series c formed by the integral contributions over c the cycles, does not converge to within c the requested
     * accuracy. as in the case of c ier = 1, it is advised to examine the c array iwork which contains the error c
     * flags on the cycles. c = 6 the input is invalid because c (integr.ne.1 and integr.ne.2) or c epsabs.le.0 or
     * limlst.lt.3. c result, abserr, neval, lst are set c to zero. c = 7 bad integrand behaviour occurs within one c or
     * more of the cycles. location and type c of the difficulty involved can be c determined from the vector ierlst.
     * here c lst is the number of cycles actually c needed (see below). c ierlst(k) = 1 the maximum number of c
     * subdivisions (= limit) has c been achieved on the k th c cycle. c = 2 occurrence of roundoff error c is detected
     * and prevents the c tolerance imposed on the c k th cycle, from being c achieved. c = 3 extremely bad integrand c
     * behaviour occurs at some c points of the k th cycle. c = 4 the integration procedure c over the k th cycle does c
     * not converge (to within the c required accuracy) due to c roundoff in the c extrapolation procedure c invoked on
     * this cycle. it c is assumed that the result c on this interval is the c best which can be obtained. c = 5 the
     * integral over the k th c cycle is probably divergent c or slowly convergent. it c must be noted that c divergence
     * can occur with c any other value of c ierlst(k). c if omega = 0 and integr = 1, c the integral is calculated by
     * means of dqagie c and ier = ierlst(1) (with meaning as described c for ierlst(k), k = 1). c c rslst -
     * DoubleDouble precision c vector of dimension at least limlst c rslst(k) contains the integral contribution c over
     * the interval (a+(k-1)c,a+kc) where c c = (2*int(abs(omega))+1)*pi/abs(omega), c k = 1, 2, ..., lst. c note that,
     * if omega = 0, rslst(1) contains c the value of the integral over (a,infinity). c c erlst - DoubleDouble precision
     * c vector of dimension at least limlst c erlst(k) contains the error estimate corresponding c with rslst(k). c c
     * ierlst - integer c vector of dimension at least limlst c ierlst(k) contains the error flag corresponding c with
     * rslst(k). for the meaning of the local error c flags see description of output parameter ier. c c lst - integer c
     * number of subintervals needed for the integration c if omega = 0 then lst is set to 1. c c alist, blist, rlist,
     * elist - DoubleDouble precision c vector of dimension at least limit, c c iord, nnlog - integer c vector of
     * dimension at least limit, providing c space for the quantities needed in the subdivision c process of each cycle
     * c c chebmo - DoubleDouble precision c array of dimension at least (maxp1,25), providing c space for the chebyshev
     * moments needed within the c cycles c c***references (none) c***routines called d1mach,dqagie,dqawoe,dqelg c***end
     * prologue dqawfe c
     */
    private void dqawfe() {
        final DoubleDouble abseps[] = new DoubleDouble[1];
        DoubleDouble correc;
        // (2*int(abs(omega))+1)*pi/abs(omega)
        DoubleDouble cycle;
        // c1, c2 end points of subinterval (of length cycle)
        DoubleDouble c1;
        DoubleDouble c2;
        DoubleDouble dl;
        DoubleDouble drl = DoubleDouble.valueOf(0.0);
        DoubleDouble ep;
        DoubleDouble eps;
        // absolute tolerance requested over current subinterval
        DoubleDouble epsa;
        // sum of error estimates over subintervals, calculated cumulatively
        DoubleDouble errsum;
        DoubleDouble fact;
        final DoubleDouble p = DoubleDouble.valueOf(0.9);
        DoubleDouble p1;
        // The dimension of psum is determined by the value of limexp
        // in subroutine dqelg (psum must be of dimension (limexp+2) at least).
        // psum contains the part of the epsilon table which is still
        // needed for further computations. Each element of psum is a
        // partial sum of the series which should sum to the value of
        // the integral.
        DoubleDouble psum[];
        final DoubleDouble reseps[] = new DoubleDouble[1];
        DoubleDouble res3la[];
        final DoubleDouble rslstVar[] = new DoubleDouble[1];
        final DoubleDouble erlstVar[] = new DoubleDouble[1];
        final int ierlstVar[] = new int[1];

        int ktmin;
        int l;
        int ll = 0;
        final int nev[] = new int[1];
        final int nres[] = new int[1];
        final int numrl2[] = new int[1];

        try {
            alist = new DoubleDouble[limit];
            blist = new DoubleDouble[limit];
            chebmo = new DoubleDouble[maxp1][25];
            elist = new DoubleDouble[limit];
            erlst = new DoubleDouble[limlst];
            ierlst = new int[limlst];
            iord = new int[limit];
            nnlog = new int[limit];
            psum = new DoubleDouble[52];
            res3la = new DoubleDouble[3];
            rlist = new DoubleDouble[limit];
            rslst = new DoubleDouble[limlst];

            result[0] = DoubleDouble.valueOf(0.0);
            abserr[0] = DoubleDouble.valueOf(0.0);
            neval[0] = 0;
            lst = 0;
            ier[0] = 0;
            for (l = 0; l < limlst; l++) {
                erlst[l] = DoubleDouble.valueOf(0.0);
                rslst[l] = DoubleDouble.valueOf(0.0);
            }
            for (l = 0; l < 52; l++) {
                psum[l] = DoubleDouble.valueOf(0.0);
            }
            for (l = 0; l < 3; l++) {
                res3la[l] = DoubleDouble.valueOf(0.0);
            }
            reseps[0] = DoubleDouble.valueOf(0.0);
            abseps[0] = DoubleDouble.valueOf(0.0);
            if (omega.isZero()) {

                // Integration by dqagie if omega is zero

                if (integr == 1) {
                    dqagie(DoubleDouble.valueOf(0.0), 1, DoubleDouble.valueOf(0.0));
                }
                rslst[0] = (DoubleDouble) result[0].clone();
                erlst[0] = (DoubleDouble) abserr[0].clone();
                ierlst[0] = ier[0];
                lst = 1;
                return;
            } // if (omega == 0.0)

            // Initializations

            l = ( (omega.abs()).trunc()).intValue();
            dl = ( (DoubleDouble.valueOf(2.0)).multiply(DoubleDouble.valueOf(l))).add(DoubleDouble.valueOf(1.0));
            cycle = (dl.multiply(DoubleDouble.PI)).divide(omega.abs());
            ier[0] = 0;
            ktmin = 0;
            neval[0] = 0;
            numrl2[0] = 0;
            nres[0] = 0;
            c1 = (DoubleDouble) lower.clone();
            c2 = cycle.add(lower);
            p1 = (DoubleDouble.valueOf(1.0)).subtract(p);
            eps = (DoubleDouble) epsabs.clone();
            if (epsabs.gt(uflow.divide(p1))) {
                eps = epsabs.multiply(p1);
            }
            ep = (DoubleDouble) eps.clone();
            fact = DoubleDouble.valueOf(1.0);
            correc = DoubleDouble.valueOf(0.0);
            abserr[0] = DoubleDouble.valueOf(0.0);
            errsum = DoubleDouble.valueOf(0.0);
            // main for loop
            for (lst = 1; lst <= limlst; lst++) {

                // integrate over current subinterval.

                epsa = eps.multiply(fact);
                rslstVar[0] = (DoubleDouble) rslst[lst - 1].clone();
                erlstVar[0] = (DoubleDouble) erlst[lst - 1].clone();
                dqawoe(c1, c2, epsa, DoubleDouble.valueOf(0.0), lst, rslstVar, erlstVar, nev, ierlstVar);
                rslst[lst - 1] = (DoubleDouble) rslstVar[0].clone();
                erlst[lst - 1] = (DoubleDouble) erlstVar[0].clone();
                ierlst[lst - 1] = ierlstVar[0];
                neval[0] = neval[0] + nev[0];
                fact = fact.multiply(p);
                errsum = errsum.add(erlst[lst - 1]);
                drl = (DoubleDouble.valueOf(50.0)).multiply(rslst[lst - 1].abs());

                // test on accuracy with partial sum

                if ( (errsum.add(drl)).le(epsabs) && lst >= 6) {
                    result[0] = (DoubleDouble) psum[numrl2[0] - 1].clone();
                    abserr[0] = errsum.add(drl);
                    return;
                }
                correc = correc.max(erlst[lst - 1]);
                if (ierlst[lst - 1] != 0) {
                    eps = ep.max(correc.multiply(p1));
                }
                if (ierlst[lst - 1] != 0) {
                    ier[0] = 7;
                }
                if (ier[0] == 7 && (errsum.add(drl)).le(correc.multiply(DoubleDouble.valueOf(10.0))) && lst > 5) {
                    result[0] = (DoubleDouble) psum[numrl2[0] - 1].clone();
                    abserr[0] = errsum.add(drl);
                    return;
                }
                numrl2[0] = numrl2[0] + 1;
                if (lst <= 1) {
                    psum[0] = (DoubleDouble) rslst[0].clone();
                    ll = numrl2[0];
                    c1 = (DoubleDouble) c2.clone();
                    c2 = c2.add(cycle);
                    continue;
                } // if (lst <= 1)
                psum[numrl2[0] - 1] = psum[ll - 1].add(rslst[lst - 1]);
                if (lst == 2) {
                    ll = numrl2[0];
                    c1 = (DoubleDouble) c2.clone();
                    c2 = c2.add(cycle);
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
                if (ktmin >= 15 && abserr[0].le( (DoubleDouble.valueOf(1.0E-3)).multiply(errsum.add(drl)))) {
                    ier[0] = 4;
                }
                if (abseps[0].le(abserr[0]) || lst == 3) {
                    abserr[0] = (DoubleDouble) abseps[0].clone();
                    result[0] = (DoubleDouble) reseps[0].clone();
                    ktmin = 0;

                    // if ier[0] is not 0, check whether direct result (partial sum)
                    // or extrapolated result yields the best integral
                    // approximation

                    if ( (abserr[0].add( (DoubleDouble.valueOf(10.0)).multiply(correc))).le(epsabs)
                            || (abserr[0].le(epsabs) && ( (DoubleDouble.valueOf(10.0)).multiply(correc)).ge(epsabs))) {
                        break;
                    }

                } // if(abseps[0] <= abserr[0] || lst == 3)
                if (ier[0] != 0 && ier[0] != 7) {
                    break;
                }
                ll = numrl2[0];
                c1 = (DoubleDouble) c2.clone();
                c2 = c2.add(cycle);
            } // for (lst = 1; lst <= limlst; lst++)

            // set final result and error estimate
            // -----------------------------------

            abserr[0] = abserr[0].add( (DoubleDouble.valueOf(10.0)).multiply(correc));
            if (ier[0] == 0) {
                return;
            }
            if (result[0].isZero() || psum[numrl2[0] - 1].isZero()) {
                if (abserr[0].gt(errsum)) {
                    result[0] = (DoubleDouble) psum[numrl2[0] - 1].clone();
                    abserr[0] = errsum.add(drl);
                    return;
                }
                if (psum[numrl2[0] - 1].isZero()) {
                    return;
                }
            } // if(result[0] == 0.0 || psum[numrl2[0]-1] == 0.0)
            if ( (abserr[0].divide(result[0].abs())).gt( (errsum.add(drl)).divide(psum[numrl2[0] - 1].abs()))) {
                result[0] = (DoubleDouble) psum[numrl2[0] - 1].clone();
                abserr[0] = errsum.add(drl);
                return;
            }
            if (ier[0] >= 1 && ier[0] != 7) {
                abserr[0] = abserr[0].add(drl);
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
     * fortran subroutine c double precision version c c parameters c on entry c f - DoubleDouble precision c function
     * subprogram defining the integrand c function f(x). the actual name for f needs to be c declared e x t e r n a l
     * in the driver program. c c a - DoubleDouble precision c lower limit of integration c c b - DoubleDouble precision
     * c upper limit of integration c c omega - DoubleDouble precision c parameter in the integrand weight function c c
     * integr - integer c indicates which of the weight functions is to be c used c integr = 1 w(x) = cos(omega*x) c
     * integr = 2 w(x) = sin(omega*x) c if integr.ne.1 and integr.ne.2, the routine c will end with ier = 6. c c epsabs -
     * DoubleDouble precision c absolute accuracy requested c epsrel - DoubleDouble precision c relative accuracy
     * requested c if epsabs.le.0 c and epsrel.lt.max(50*rel.mach.acc.,0.5d-28), c the routine will end with ier = 6. c
     * c limit - integer c gives an upper bound on the number of subdivisions c in the partition of (a,b), limit.ge.1. c
     * c icall - integer c if dqawoe is to be used only once, icall must c be set to 1. assume that during this call,
     * the c chebyshev moments (for clenshaw-curtis integration c of degree 24) have been computed for intervals of c
     * lenghts (abs(b-a))*2**(-l), l=0,1,2,...momcom-1. c if icall.gt.1 this means that dqawoe has been c called twice
     * or more on intervals of the same c length abs(b-a). the chebyshev moments already c computed are then re-used in
     * subsequent calls. c if icall.lt.1, the routine will end with ier = 6. c c maxp1 - integer c gives an upper bound
     * on the number of chebyshev c moments which can be stored, i.e. for the c intervals of lenghts abs(b-a)*2**(-l), c
     * l=0,1, ..., maxp1-2, maxp1.ge.1. c if maxp1.lt.1, the routine will end with ier = 6. c c on return c result -
     * DoubleDouble precision c approximation to the integral c c abserr - DoubleDouble precision c estimate of the
     * modulus of the absolute error, c which should equal or exceed abs(i-result) c c neval - integer c number of
     * integrand evaluations c c ier - integer c ier = 0 normal and reliable termination of the c routine. it is assumed
     * that the c requested accuracy has been achieved. c - ier.gt.0 abnormal termination of the routine. c the
     * estimates for integral and error are c less reliable. it is assumed that the c requested accuracy has not been
     * achieved. c error messages c ier = 1 maximum number of subdivisions allowed c has been achieved. one can allow
     * more c subdivisions by increasing the value of c limit (and taking according dimension c adjustments into
     * account). however, if c this yields no improvement it is advised c to analyze the integrand, in order to c
     * determine the integration difficulties. c if the position of a local difficulty can c be determined (e.g.
     * singularity, c discontinuity within the interval) one c will probably gain from splitting up the c interval at
     * this point and calling the c integrator on the subranges. if possible, c an appropriate special-purpose
     * integrator c should be used which is designed for c handling the type of difficulty involved. c = 2 the
     * occurrence of roundoff error is c detected, which prevents the requested c tolerance from being achieved. c the
     * error may be under-estimated. c = 3 extremely bad integrand behaviour occurs c at some points of the integration
     * c interval. c = 4 the algorithm does not converge. c roundoff error is detected in the c extrapolation table. c
     * it is presumed that the requested c tolerance cannot be achieved due to c roundoff in the extrapolation table, c
     * and that the returned result is the c best which can be obtained. c = 5 the integral is probably divergent, or c
     * slowly convergent. it must be noted that c divergence can occur with any other value c of ier.gt.0. c = 6 the
     * input is invalid, because c (epsabs.le.0 and c epsrel.lt.max(50*rel.mach.acc.,0.5d-28)) c or (integr.ne.1 and
     * integr.ne.2) or c icall.lt.1 or maxp1.lt.1. c result, abserr, neval, last, rlist(1), c elist(1), iord(1) and
     * nnlog(1) are set c to zero. alist(1) and blist(1) are set c to a and b respectively. c c last - integer c on
     * return, last equals the number of c subintervals produces in the subdivision c process, which determines the
     * number of c significant elements actually in the c work arrays. c alist - DoubleDouble precision c vector of
     * dimension at least limit, the first c last elements of which are the left c end points of the subintervals in the
     * partition c of the given integration range (a,b) c c blist - DoubleDouble precision c vector of dimension at
     * least limit, the first c last elements of which are the right c end points of the subintervals in the partition c
     * of the given integration range (a,b) c c rlist - DoubleDouble precision c vector of dimension at least limit, the
     * first c last elements of which are the integral c approximations on the subintervals c c elist - DoubleDouble
     * precision c vector of dimension at least limit, the first c last elements of which are the moduli of the c
     * absolute error estimates on the subintervals c c iord - integer c vector of dimension at least limit, the first k
     * c elements of which are pointers to the error c estimates over the subintervals, c such that elist(iord(1)), ...,
     * c elist(iord(k)) form a decreasing sequence, with c k = last if last.le.(limit/2+2), and c k = limit+1-last
     * otherwise. c c nnlog - integer c vector of dimension at least limit, containing the c subdivision levels of the
     * subintervals, i.e. c iwork(i) = l means that the subinterval c numbered i is of length abs(b-a)*2**(1-l) c c on
     * entry and return c momcom - integer c indicating that the chebyshev moments c have been computed for intervals of
     * lengths c (abs(b-a))*2**(-l), l=0,1,2, ..., momcom-1, c momcom.lt.maxp1 c c chebmo - DoubleDouble precision c
     * array of dimension (maxp1,25) containing the c chebyshev moments c c***references (none) c***routines called
     * d1mach,dqc25f,dqelg,dqpsrt c***end prologue dqawoe c
     */
    private void dqawoe(final DoubleDouble a, final DoubleDouble b, final DoubleDouble epsabs,
            final DoubleDouble epsrel, final int icall, final DoubleDouble result[], final DoubleDouble abserr[],
            final int neval[], final int ier[]) {
        // Variables ending in 1 denote left subinterval and variables
        // ending in 2 denote right subinterval
        // rlist2 should be length at least (limexp + 2)
        // The value of limexp is determined in subroutine dqelg
        // rlist2 contains the part of the epsilon table still needed for
        // further computations
        DoubleDouble[] rlist2;

        // Index to the interval with the largest error estimate
        final int[] maxErr = new int[1];

        // errMax[0] = elist[maxErr];
        final DoubleDouble[] errMax = new DoubleDouble[1];

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
        final int[] nres = new int[1];

        // Number of elements in rlist2. If an appropriate approximation
        // to the compounded integral has been obtained, it is put in
        // rlist2[numr12-1] after numr12 has been increased by one.
        final int[] numr12 = new int[1];

        // Length of the smallest interval considered up to now,
        // multiplied by 1.5
        DoubleDouble small;

        // Sum of the errors over the intervals larger than the smallest
        // interval considered up to now
        DoubleDouble erlarg = DoubleDouble.valueOf(0.0);

        // boolean variable denoting that the routine is attempting to
        // perform extrapolation. That is, before dividing the
        // interval we try to decrease the value of erlarg
        boolean extrap;

        // If noext is true, the extrapolation is no longer allowed.
        boolean noext;

        final DoubleDouble abseps[] = new DoubleDouble[1];
        final DoubleDouble area1[] = new DoubleDouble[1];
        DoubleDouble area12;
        final DoubleDouble area2[] = new DoubleDouble[1];
        DoubleDouble a1;
        DoubleDouble a2;
        DoubleDouble b1;
        DoubleDouble b2;
        DoubleDouble correc = DoubleDouble.valueOf(0.0);
        final DoubleDouble defab1[] = new DoubleDouble[1];
        final DoubleDouble defab2[] = new DoubleDouble[1];
        final DoubleDouble defabs[] = new DoubleDouble[1];
        DoubleDouble domega;
        DoubleDouble dres;
        final DoubleDouble error1[] = new DoubleDouble[1];
        DoubleDouble erro12;
        final DoubleDouble error2[] = new DoubleDouble[1];
        DoubleDouble ertest = DoubleDouble.valueOf(0.0);
        final DoubleDouble reseps[] = new DoubleDouble[1];
        final DoubleDouble res31a[] = new DoubleDouble[3];
        DoubleDouble width;
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
            alist = new DoubleDouble[limit];
            blist = new DoubleDouble[limit];
            rlist = new DoubleDouble[limit];
            elist = new DoubleDouble[limit];
            defab1[0] = DoubleDouble.valueOf(0.0);
            defab2[0] = DoubleDouble.valueOf(0.0);
            iord = new int[limit];
            rlist2 = new DoubleDouble[52];
            if ( (chebmo == null) || (chebmo.length != maxp1)) {
                chebmo = new DoubleDouble[maxp1][25];
            }
            nnlog = new int[limit];

            ier[0] = 0;
            neval[0] = 0;
            last = 0;
            result[0] = DoubleDouble.valueOf(0.0);
            abserr[0] = DoubleDouble.valueOf(0.0);
            alist[0] = (DoubleDouble) a.clone();
            blist[0] = (DoubleDouble) b.clone();
            rlist[0] = DoubleDouble.valueOf(0.0);
            elist[0] = DoubleDouble.valueOf(0.0);
            iord[0] = -1;
            nnlog[0] = 0;

            // First approximation to the integral
            domega = omega.abs();
            nrmom = 0;
            if (icall == 1) {
                momcom = 0;
            }
            dqc25f(a, b, domega, nrmom, 0, result, abserr, neval, defabs, resabs);

            // Test on accuracy

            dres = result[0].abs();
            errBnd = epsabs.max(epsrel.multiply(dres));
            rlist[0] = (DoubleDouble) result[0].clone();
            elist[0] = (DoubleDouble) abserr[0].clone();
            iord[0] = 0;
            if ( (abserr[0].le( ( (DoubleDouble.valueOf(100.0)).multiply(epmach)).multiply(defabs[0])))
                    && (abserr[0].gt(errBnd))) {
                ier[0] = 2;
            }
            if (limit == 1) {
                ier[0] = 1;
            }
            if ( (ier[0] != 0) || (abserr[0].le(errBnd))) {
                if ( (integr == 2) && (omega.lt(DoubleDouble.valueOf(0.0)))) {
                    result[0] = result[0].negate();
                }
                return;
            }

            // Initializations
            errMax[0] = (DoubleDouble) abserr[0].clone();
            maxErr[0] = 0;
            area = (DoubleDouble) result[0].clone();
            errSum = (DoubleDouble) abserr[0].clone();
            abserr[0] = (DoubleDouble) oflow.clone();
            nrmax[0] = 1;
            extrap = false;
            noext = false;
            ierro = 0;
            iroff1 = 0;
            iroff2 = 0;
            iroff3 = 0;
            ktmin = 0;
            small = ( (b.subtract(a)).abs()).multiply(DoubleDouble.valueOf(0.75));
            nres[0] = 0;
            numr12[0] = 0;
            extall = false;
            if ( ( ( (DoubleDouble.valueOf(0.5)).multiply( (b.subtract(a)).abs())).multiply(domega)).le(DoubleDouble
                    .valueOf(2.0))) {
                numr12[0] = 1;
                extall = true;
                rlist2[0] = (DoubleDouble) result[0].clone();
            }
            if ( ( (DoubleDouble.valueOf(0.25)).multiply( (b.subtract(a)).abs()).multiply(domega)).le(DoubleDouble
                    .valueOf(2.0))) {
                extall = true;
            }
            ksgn = -1;
            if (dres.ge( ( (DoubleDouble.valueOf(1.0)).subtract( (DoubleDouble.valueOf(50.0)).multiply(epmach)))
                    .multiply(defabs[0]))) {
                ksgn = 1;
            }

            // Main for loop

            do150 = true;
            loop: for (last = 2; last <= limit; last++) {

                // Bisect the subinteval with the nrmax-th largest error estimate.

                nrmom = nnlog[maxErr[0]] + 1;
                a1 = (DoubleDouble) alist[maxErr[0]].clone();
                b1 = (DoubleDouble.valueOf(0.5)).multiply(alist[maxErr[0]].add(blist[maxErr[0]]));
                a2 = (DoubleDouble) b1.clone();
                b2 = (DoubleDouble) blist[maxErr[0]].clone();
                erlast = (DoubleDouble) errMax[0].clone();
                dqc25f(a1, b1, domega, nrmom, 0, area1, error1, nev, resabs, defab1);
                neval[0] = neval[0] + nev[0];
                dqc25f(a2, b2, domega, nrmom, 1, area2, error2, nev, resabs, defab2);
                neval[0] = neval[0] + nev[0];

                // Improve precious approximations to integral
                // and error and test for accuracy.

                area12 = area1[0].add(area2[0]);
                erro12 = error1[0].add(error2[0]);
                errSum = (errSum.add(erro12)).subtract(errMax[0]);
                area = (area.add(area12)).subtract(rlist[maxErr[0]]);
                if ( (defab1[0].ne(error1[0])) && (defab2[0].ne(error2[0]))) {
                    if ( ( ( (rlist[maxErr[0]].subtract(area12)).abs()).le( (DoubleDouble.valueOf(1.0E-5))
                            .multiply(area12.abs())))
                            && (erro12.ge( (DoubleDouble.valueOf(0.99)).multiply(errMax[0])))) {
                        if (extrap) {
                            iroff2 = iroff2 + 1;
                        } else {
                            iroff1 = iroff1 + 1;
                        }
                    }
                    if ( (last > 10) && (erro12.gt(errMax[0]))) {
                        iroff3 = iroff3 + 1;
                    }
                } // if ((defab1[0] != error1[0]) && (defab2[0] != error2[0]))
                rlist[maxErr[0]] = (DoubleDouble) area1[0].clone();
                rlist[last - 1] = (DoubleDouble) area2[0].clone();
                nnlog[maxErr[0]] = nrmom;
                nnlog[last - 1] = nrmom;
                errBnd = epsabs.max(epsrel.multiply(area.abs()));

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

                if ( ( (a1.abs()).max( (b2.abs()))).le( ( (DoubleDouble.valueOf(1.0))
                        .add( (DoubleDouble.valueOf(100.0)).multiply(epmach))).multiply( (a2.abs()).add( (DoubleDouble
                        .valueOf(1.0E3)).multiply(uflow))))) {
                    ier[0] = 4;
                }

                // Append the newly-created intervals to the list
                if (error2[0].le(error1[0])) {
                    alist[last - 1] = (DoubleDouble) a2.clone();
                    blist[maxErr[0]] = (DoubleDouble) b1.clone();
                    blist[last - 1] = (DoubleDouble) b2.clone();
                    elist[maxErr[0]] = (DoubleDouble) error1[0].clone();
                    elist[last - 1] = (DoubleDouble) error2[0].clone();
                } else {
                    alist[maxErr[0]] = (DoubleDouble) a2.clone();
                    alist[last - 1] = (DoubleDouble) a1.clone();
                    blist[last - 1] = (DoubleDouble) b1.clone();
                    rlist[maxErr[0]] = (DoubleDouble) area2[0].clone();
                    rlist[last - 1] = (DoubleDouble) area1[0].clone();
                    elist[maxErr[0]] = (DoubleDouble) error2[0].clone();
                    elist[last - 1] = (DoubleDouble) error1[0].clone();
                }

                // Call the subroutine dqpsrt to maintain the descending ordering
                // in the list of error estimates and select the subinterval
                // with the nrmax-th largest error estimate (to bisected next).
                dqpsrt(limit, last, maxErr, errMax, elist, iord, nrmax);
                if (errSum.le(errBnd)) {
                    do150 = false;
                    do170 = true;
                    break;
                }
                if (ier[0] != 0) {
                    break;
                }
                if ( (last == 2) && extall) {
                    small = (DoubleDouble.valueOf(0.5)).multiply(small);
                    numr12[0] = numr12[0] + 1;
                    rlist2[numr12[0] - 1] = (DoubleDouble) area.clone();
                    ertest = (DoubleDouble) errBnd.clone();
                    erlarg = (DoubleDouble) errSum.clone();
                    continue;
                } // if ((last == 2) && extall)
                if (noext) {
                    continue;
                }
                if (extall) {
                    erlarg = erlarg.subtract(erlast);
                    if ( ( (b1.subtract(a1)).abs()).gt(small)) {
                        erlarg = erlarg.add(erro12);
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
                    width = (blist[maxErr[0]].subtract(alist[maxErr[0]])).abs();
                    if (width.gt(small)) {
                        continue;
                    } // if (width > small)
                    if ( !extall) {

                        // Test whether we can start with the extrapolation procedure
                        // (we do this if we integrate over the next interval with
                        // use of a gauss-kronrod rule - see subroutine dqc25f).

                        small = (DoubleDouble.valueOf(0.5)).multiply(small);
                        if ( ( ( (DoubleDouble.valueOf(0.25)).multiply(width)).multiply(domega)).gt(DoubleDouble
                                .valueOf(2.0))) {
                            continue;
                        }
                        extall = true;
                        ertest = (DoubleDouble) errBnd.clone();
                        erlarg = (DoubleDouble) errSum.clone();
                        continue;
                    } // if (!extall)
                    extrap = true;
                    nrmax[0] = 2;
                    do70 = true;
                } // if (do50)
                if (do70) {
                    do70 = false;
                    if ( (ierro == 3) || (erlarg.le(ertest))) {
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
                        errMax[0] = (DoubleDouble) elist[maxErr[0]].clone();
                        if ( ( (blist[maxErr[0]].subtract(alist[maxErr[0]])).abs()).gt(small)) {
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
                    rlist2[numr12[0] - 1] = (DoubleDouble) area.clone();
                    if (numr12[0] >= 3) {
                        dqelg(numr12, rlist2, reseps, abseps, res31a, nres);
                        ktmin = ktmin + 1;
                        if ( (ktmin > 5) && (abserr[0].lt( (DoubleDouble.valueOf(1.0E-3)).multiply(errSum)))) {
                            ier[0] = 5;
                        }
                        if (abseps[0].lt(abserr[0])) {
                            ktmin = 0;
                            abserr[0] = (DoubleDouble) abseps[0].clone();
                            result[0] = (DoubleDouble) reseps[0].clone();
                            correc = (DoubleDouble) erlarg.clone();
                            ertest = epsabs.max(epsrel.multiply(reseps[0].abs()));
                            if (abserr[0].le(ertest)) {
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
                    errMax[0] = (DoubleDouble) elist[maxErr[0]].clone();
                    nrmax[0] = 1;
                    extrap = false;
                    small = (DoubleDouble.valueOf(0.5)).multiply(small);
                    erlarg = (DoubleDouble) errSum.clone();
                } // if (do90)
            } // for (last = 2; last <= limit; last++)

            // Set the final result.

            if (do150) {
                if ( (abserr[0].equals(oflow)) || (nres[0] == 0)) {
                    do170 = true;
                } else if (ier[0] + ierro == 0) {
                    do165 = true;
                } else {
                    if (ierro == 3) {
                        abserr[0] = abserr[0].add(correc);
                    }
                    if (ier[0] == 0) {
                        ier[0] = 3;
                    }
                    if ( (result[0].ne(DoubleDouble.valueOf(0.0))) && (area.ne(DoubleDouble.valueOf(0.0)))) {
                        do160 = true;
                    } else if (abserr[0].gt(errSum)) {
                        do170 = true;
                    } else if (area.isZero()) {
                        do190 = true;
                    } else {
                        do165 = true;
                    }
                }
            } // if (do150)

            if (do160) {
                if ( (abserr[0].divide(result[0].abs())).gt(errSum.divide(area.abs()))) {
                    do170 = true;
                } else {
                    do165 = true;
                }
            } // if (do160)

            if (do165) {
                // Test on divergence
                if ( (ksgn == -1)
                        && ( (result[0].abs()).max( (area.abs())).le( (DoubleDouble.valueOf(1.0E-2))
                                .multiply(defabs[0])))) {} else if ( ( (DoubleDouble.valueOf(1.0E-2)).gt(result[0]
                        .divide(area)))
                        || ( (result[0].divide(area)).gt(DoubleDouble.valueOf(100.0))) || (errSum.ge(area.abs()))) {
                    ier[0] = 6;
                }
                do190 = true;
            } // if (do165)

            if (do170) {
                result[0] = DoubleDouble.valueOf(0.0);
                for (k = 0; k < last; k++) {
                    result[0] = result[0].add(rlist[k]);
                }
                abserr[0] = errSum;
                do190 = true;
            } // if (do170)

            if (do190) {
                if (ier[0] > 2) {
                    ier[0] = ier[0] - 1;
                }
                if ( (integr == 2) && (omega.lt(DoubleDouble.valueOf(0.0)))) {
                    result[0] = result[0].negate();
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
     * DoubleDouble precision version c c parameters c on entry c f - DoubleDouble precision c function subprogram
     * defining the integrand c function f(x). the actual name for f needs to be c declared e x t e r n a l in the
     * driver program. c c a - DoubleDouble precision c lower limit of integration c c b - DoubleDouble precision c
     * upper limit of integration, b.gt.a c if b.le.a, the routine will end with ier = 6. c c alfa - DoubleDouble
     * precision c parameter in the weight function, alfa.gt.(-1) c if alfa.le.(-1), the routine will end with c ier =
     * 6. c c beta - DoubleDouble precision c parameter in the weight function, beta.gt.(-1) c if beta.le.(-1), the
     * routine will end with c ier = 6. c c integr - integer c indicates which weight function is to be used c = 1
     * (x-a)**alfa*(b-x)**beta c = 2 (x-a)**alfa*(b-x)**beta*log(x-a) c = 3 (x-a)**alfa*(b-x)**beta*log(b-x) c = 4
     * (x-a)**alfa*(b-x)**beta*log(x-a)*log(b-x) c if integr.lt.1 or integr.gt.4, the routine c will end with ier = 6. c
     * c epsabs - DoubleDouble precision c absolute accuracy requested c epsrel - DoubleDouble precision c relative
     * accuracy requested c if epsabs.le.0 c and epsrel.lt.max(50*rel.mach.acc.,0.5d-28), c the routine will end with
     * ier = 6. c c limit - integer c gives an upper bound on the number of subintervals c in the partition of (a,b),
     * limit.ge.2 c if limit.lt.2, the routine will end with ier = 6. c c on return c result - DoubleDouble precision c
     * approximation to the integral c c abserr - DoubleDouble precision c estimate of the modulus of the absolute
     * error, c which should equal or exceed abs(i-result) c c neval - integer c number of integrand evaluations c c ier -
     * integer c ier = 0 normal and reliable termination of the c routine. it is assumed that the requested c accuracy
     * has been achieved. c ier.gt.0 abnormal termination of the routine c the estimates for the integral and error c
     * are less reliable. it is assumed that the c requested accuracy has not been achieved. c error messages c = 1
     * maximum number of subdivisions allowed c has been achieved. one can allow more c subdivisions by increasing the
     * value of c limit. however, if this yields no c improvement, it is advised to analyze the c integrand in order to
     * determine the c integration difficulties which prevent the c requested tolerance from being achieved. c in case
     * of a jump discontinuity or a local c singularity of algebraico-logarithmic type c at one or more interior points
     * of the c integration range, one should proceed by c splitting up the interval at these c points and calling the
     * integrator on the c subranges. c = 2 the occurrence of roundoff error is c detected, which prevents the requested
     * c tolerance from being achieved. c = 3 extremely bad integrand behaviour occurs c at some points of the
     * integration c interval. c = 6 the input is invalid, because c b.le.a or alfa.le.(-1) or beta.le.(-1), or c
     * integr.lt.1 or integr.gt.4, or c (epsabs.le.0 and c epsrel.lt.max(50*rel.mach.acc.,0.5d-28), c or limit.lt.2. c
     * result, abserr, neval, rlist(1), elist(1), c iord(1) and last are set to zero. alist(1) c and blist(1) are set to
     * a and b c respectively. c c alist - DoubleDouble precision c vector of dimension at least limit, the first c last
     * elements of which are the left c end points of the subintervals in the partition c of the given integration range
     * (a,b) c c blist - DoubleDouble precision c vector of dimension at least limit, the first c last elements of which
     * are the right c end points of the subintervals in the partition c of the given integration range (a,b) c c rlist -
     * DoubleDouble precision c vector of dimension at least limit,the first c last elements of which are the integral c
     * approximations on the subintervals c c elist - DoubleDouble precision c vector of dimension at least limit, the
     * first c last elements of which are the moduli of the c absolute error estimates on the subintervals c c iord -
     * integer c vector of dimension at least limit, the first k c of which are pointers to the error c estimates over
     * the subintervals, so that c elist(iord(1)), ..., elist(iord(k)) with k = last c if last.le.(limit/2+2), and k =
     * limit+1-last c otherwise form a decreasing sequence c c last - integer c number of subintervals actually produced
     * in c the subdivision process c c***references (none) c***routines called d1mach,dqc25s,dqmomo,dqpsrt c***end
     * prologue dqawse c
     */
    private void dqawse() {
        // Index to the interval with the largest error estimate
        final int[] maxErr = new int[1];

        // errMax[0] = elist[maxErr];
        final DoubleDouble[] errMax = new DoubleDouble[1];

        // Sum of the integrals over the subintervals
        DoubleDouble area;

        // Sum of the errors over the subintervals
        DoubleDouble errSum;

        // Requested accuracy max(epsabs, epsrel * abs(result))
        DoubleDouble errBnd;

        final DoubleDouble area1[] = new DoubleDouble[1];
        DoubleDouble area12;
        final DoubleDouble area2[] = new DoubleDouble[1];
        DoubleDouble a1;
        DoubleDouble a2;
        DoubleDouble b1;
        DoubleDouble b2;
        DoubleDouble centre;
        final DoubleDouble error1[] = new DoubleDouble[1];
        DoubleDouble erro12;
        final DoubleDouble error2[] = new DoubleDouble[1];
        final DoubleDouble resas1[] = new DoubleDouble[1];
        final DoubleDouble resas2[] = new DoubleDouble[1];
        DoubleDouble rg[];
        DoubleDouble rh[];
        DoubleDouble ri[];
        DoubleDouble rj[];

        int iroff1;
        int iroff2;
        int k;
        final int nev[] = new int[1];
        final int nrmax[] = new int[1];

        try {
            alist = new DoubleDouble[limit];
            blist = new DoubleDouble[limit];
            rlist = new DoubleDouble[limit];
            elist = new DoubleDouble[limit];
            iord = new int[limit];
            rg = new DoubleDouble[25];
            rh = new DoubleDouble[25];
            ri = new DoubleDouble[25];
            rj = new DoubleDouble[25];

            neval[0] = 0;
            last = 0;
            rlist[0] = DoubleDouble.valueOf(0.0);
            elist[0] = DoubleDouble.valueOf(0.0);
            iord[0] = -1;
            result[0] = DoubleDouble.valueOf(0.0);
            abserr[0] = DoubleDouble.valueOf(0.0);
            ier[0] = 0;

            // Compute the modified chebyshev moments

            dqmomo(ri, rj, rg, rh);

            // Integrate over the intervals (lower, (lower+upper)/2) and ((lower+upper)/2, upper).

            centre = (DoubleDouble.valueOf(0.5)).multiply(lower.add(upper));
            dqc25s(lower, centre, ri, rj, rg, rh, area1, error1, resas1, nev);
            neval[0] = nev[0];
            dqc25s(centre, upper, ri, rj, rg, rh, area2, error2, resas2, nev);
            last = 2;
            neval[0] = neval[0] + nev[0];
            result[0] = area1[0].add(area2[0]);
            abserr[0] = error1[0].add(error2[0]);

            // Test on accuracy
            errBnd = epsabs.max(epsrel.multiply(result[0].abs()));

            // Initialization
            if (error2[0].le(error1[0])) {
                alist[0] = (DoubleDouble) lower.clone();
                alist[1] = (DoubleDouble) centre.clone();
                blist[0] = (DoubleDouble) centre.clone();
                blist[1] = (DoubleDouble) upper.clone();
                rlist[0] = (DoubleDouble) area1[0].clone();
                rlist[1] = (DoubleDouble) area2[0].clone();
                elist[0] = (DoubleDouble) error1[0].clone();
                elist[1] = (DoubleDouble) error2[0].clone();
            } else {
                alist[0] = (DoubleDouble) centre.clone();
                alist[1] = (DoubleDouble) lower.clone();
                blist[0] = (DoubleDouble) upper.clone();
                blist[1] = (DoubleDouble) centre.clone();
                rlist[0] = (DoubleDouble) area2[0].clone();
                rlist[1] = (DoubleDouble) area1[0].clone();
                elist[0] = (DoubleDouble) error2[0].clone();
                elist[1] = (DoubleDouble) error1[0].clone();
            }
            iord[0] = 0;
            iord[1] = 1;
            if (limit == 2) {
                ier[0] = 1;
            }
            if (abserr[0].le(errBnd) || ier[0] == 1) {
                return;
            }
            errMax[0] = (DoubleDouble) elist[0].clone();
            maxErr[0] = 0;
            nrmax[0] = 1;
            area = (DoubleDouble) result[0].clone();
            errSum = (DoubleDouble) abserr[0].clone();
            iroff1 = 0;
            iroff2 = 0;

            // Main for loop
            for (last = 3; last <= limit; last++) {

                // bisect the subinterval with largest error estimate.

                a1 = (DoubleDouble) alist[maxErr[0]].clone();
                b1 = (DoubleDouble.valueOf(0.5)).multiply(alist[maxErr[0]].add(blist[maxErr[0]]));
                a2 = (DoubleDouble) b1.clone();
                b2 = (DoubleDouble) blist[maxErr[0]].clone();

                dqc25s(a1, b1, ri, rj, rg, rh, area1, error1, resas1, nev);
                neval[0] = neval[0] + nev[0];
                dqc25s(a2, b2, ri, rj, rg, rh, area2, error2, resas2, nev);
                neval[0] = neval[0] + nev[0];

                // improve previous approximations integral and error
                // and test for accuracy.

                area12 = area1[0].add(area2[0]);
                erro12 = error1[0].add(error2[0]);
                errSum = (errSum.add(erro12)).subtract(errMax[0]);
                area = (area.add(area12)).subtract(rlist[maxErr[0]]);
                if ( (lower.ne(a1)) && (upper.ne(b2)) && (resas1[0].ne(error1[0])) && (resas2[0].ne(error2[0]))) {

                    // test for roundoff error.

                    if ( ( (rlist[maxErr[0]].subtract(area12)).abs()).lt( (DoubleDouble.valueOf(1.0E-5))
                            .multiply(area12.abs()))
                            && erro12.ge( (DoubleDouble.valueOf(0.99)).multiply(errMax[0]))) {
                        iroff1 = iroff1 + 1;
                    }
                    if (last > 10 && erro12.gt(errMax[0])) {
                        iroff2 = iroff2 + 1;
                    }
                } // if ((lower != a1) && (upper != b2) &&
                rlist[maxErr[0]] = (DoubleDouble) area1[0].clone();
                rlist[last - 1] = (DoubleDouble) area2[0].clone();

                // test on accuracy.

                errBnd = epsabs.max(epsrel.multiply(area.abs()));
                if (errSum.gt(errBnd)) {

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

                    if ( ( (a1.abs()).max(b2.abs())).le( ( (DoubleDouble.valueOf(1.0)).add( (DoubleDouble
                            .valueOf(100.0)).multiply(epmach))).multiply( (a2.abs()).add( (DoubleDouble.valueOf(1.0E3))
                            .multiply(uflow))))) {
                        ier[0] = 3;
                    }

                } // if (errSum > errBnd)

                // append the newly-created intervals to the list.

                if (error2[0].le(error1[0])) {
                    alist[last - 1] = (DoubleDouble) a2.clone();
                    blist[maxErr[0]] = (DoubleDouble) b1.clone();
                    blist[last - 1] = (DoubleDouble) b2.clone();
                    elist[maxErr[0]] = (DoubleDouble) error1[0].clone();
                    elist[last - 1] = (DoubleDouble) error2[0].clone();
                } else {
                    alist[maxErr[0]] = (DoubleDouble) a2.clone();
                    alist[last - 1] = (DoubleDouble) a1.clone();
                    blist[last - 1] = (DoubleDouble) b1.clone();
                    rlist[maxErr[0]] = (DoubleDouble) area2[0].clone();
                    rlist[last - 1] = (DoubleDouble) area1[0].clone();
                    elist[maxErr[0]] = (DoubleDouble) error2[0].clone();
                    elist[last - 1] = (DoubleDouble) error1[0].clone();
                }

                // call subroutine dqpsrt to maintain the descending ordering
                // in the list of error estimates and select the subinterval
                // with largest error estimate (to be bisected next).

                dqpsrt(limit, last, maxErr, errMax, elist, iord, nrmax);
                if (ier[0] != 0 || errSum.le(errBnd)) {
                    break;
                }
            } // for (last = 3; last <= limit; last++)

            // compute final result.
            // ---------------------

            result[0] = DoubleDouble.valueOf(0.0);
            for (k = 0; k < last; k++) {
                result[0] = result[0].add(rlist[k]);
            }
            abserr[0] = (DoubleDouble) errSum.clone();
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
     * principal value integrals c standard fortran subroutine c DoubleDouble precision version c c c a - DoubleDouble
     * precision c left end point of the integration interval c c b - DoubleDouble precision c right end point of the
     * integration interval, b.gt.a c c c - DoubleDouble precision c parameter in the weight function c c result -
     * DoubleDouble precision c approximation to the integral c result is computed by using a generalized c
     * clenshaw-curtis method if c lies within ten percent c of the integration interval. in the other case the c
     * 15-point kronrod rule obtained by optimal addition c of abscissae to the 7-point gauss rule, is applied. c c
     * abserr - DoubleDouble precision c estimate of the modulus of the absolute error, c which should equal or exceed
     * abs(i-result) c c krul - integer c key which is decreased by 1 if the 15-point c gauss-kronrod scheme has been
     * used c c neval - integer c number of integrand evaluations c
     * c....................................................................... c***references (none) c***routines
     * called dqcheb,dqk15w,dqwgtc c***end prologue dqc25c c
     * 
     * @param a
     * @param b
     * @param c
     * @param result
     * @param abserr
     * @param krul
     * @param neval
     */
    private void dqc25c(final DoubleDouble a, final DoubleDouble b, final DoubleDouble c, final DoubleDouble result[],
            final DoubleDouble abserr[], final int krul[], final int neval[]) {
        DoubleDouble ak22;
        DoubleDouble amom0;
        DoubleDouble amom1;
        DoubleDouble amom2;
        DoubleDouble cc;
        /** mid point of the interval */
        DoubleDouble centr;
        /**
         * chebyshev series expansion of coefficients, for the function f, of degree 12.
         */
        final DoubleDouble cheb12[] = new DoubleDouble[13];
        /**
         * chebyshev series expansion of coefficients, for the function f, of degree 24.
         */
        final DoubleDouble cheb24[] = new DoubleDouble[25];
        /** value of the function f at the points cos(k*pi/24), k = 0, ..., 24 */
        final DoubleDouble fval[] = new DoubleDouble[25];
        /** half-length of the interval */
        DoubleDouble hlgth;
        /** Approximation to the integral corresponding to the use of cheb12 */
        DoubleDouble res12;
        /** Approximation to the integral corresponding to the use of cheb24 */
        DoubleDouble res24;
        DoubleDouble u;
        // the vector x contains the values cos(k*pi/24),
        // k = 1, ..., 11, to be used for the chebyshev series
        // expansion of f
        /** To be used for the chebyshev series expansion of f */
        final DoubleDouble x[] = new DoubleDouble[11];
        int i;
        int isym;
        int k;

        for (k = 1; k <= 11; k++) {
            x[k - 1] = ( ( (DoubleDouble.valueOf(k)).multiply(DoubleDouble.PI)).divide(DoubleDouble.valueOf(24))).cos();
        }

        // Check the position of c
        cc = ( ( ( (DoubleDouble.valueOf(2.0)).multiply(c)).subtract(b)).subtract(a)).divide(b.subtract(a));
        if ( ( (cc).abs()).ge(DoubleDouble.valueOf(1.1))) {

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

        hlgth = (DoubleDouble.valueOf(0.5)).multiply(b.subtract(a));
        centr = (DoubleDouble.valueOf(0.5)).multiply(b.add(a));
        neval[0] = 25;
        if (selfTest) {
            fval[0] = (DoubleDouble.valueOf(0.5)).multiply(intFuncTest(hlgth.add(centr)));
            fval[12] = intFuncTest(centr);
            fval[24] = (DoubleDouble.valueOf(0.5)).multiply(intFuncTest(centr.subtract(hlgth)));
            for (i = 2; i <= 12; i++) {
                u = hlgth.multiply(x[i - 2]);
                isym = 26 - i;
                fval[i - 1] = intFuncTest(u.add(centr));
                fval[isym - 1] = intFuncTest(centr.subtract(u));
            }
        } // if (selfTest)
        else {
            fval[0] = (DoubleDouble.valueOf(0.5)).multiply(intFunc(hlgth.add(centr)));
            fval[12] = intFunc(centr);
            fval[24] = (DoubleDouble.valueOf(0.5)).multiply(intFunc(centr.subtract(hlgth)));
            for (i = 2; i <= 12; i++) {
                u = hlgth.multiply(x[i - 2]);
                isym = 26 - i;
                fval[i - 1] = intFunc(u.add(centr));
                fval[isym - 1] = intFunc(centr.subtract(u));
            }
        }

        // Compute the chebyshev series expansion.

        dqcheb(x, fval, cheb12, cheb24);

        // The modified chebyshev moments sare computed by forward
        // recursion, using amom0 and amom1 as starting values.

        amom0 = ( ( (DoubleDouble.valueOf(1.0)).subtract(cc)).divide( (DoubleDouble.valueOf(1.0)).add(cc)).abs()).log();
        amom1 = (DoubleDouble.valueOf(2.0)).add(cc.multiply(amom0));
        res12 = (cheb12[0].multiply(amom0)).add(cheb12[1].multiply(amom1));
        res24 = (cheb24[0].multiply(amom0)).add(cheb24[1].multiply(amom1));
        for (k = 3; k <= 13; k++) {
            amom2 = ( ( (DoubleDouble.valueOf(2.0)).multiply(cc)).multiply(amom1)).subtract(amom0);
            ak22 = DoubleDouble.valueOf( ( (k - 2) * (k - 2)));
            if ( (k / 2) * 2 == k) {
                amom2 = amom2.subtract( (DoubleDouble.valueOf(4.0)).divide(ak22.subtract(DoubleDouble.valueOf(1.0))));
            }
            res12 = res12.add(cheb12[k - 1].multiply(amom2));
            res24 = res24.add(cheb24[k - 1].multiply(amom2));
            amom0 = (DoubleDouble) amom1.clone();
            amom1 = (DoubleDouble) amom2.clone();
        } // for (k = 3; k <= 13; k++)
        for (k = 14; k <= 25; k++) {
            amom2 = ( ( (DoubleDouble.valueOf(2.0)).multiply(cc)).multiply(amom1)).subtract(amom0);
            ak22 = DoubleDouble.valueOf( ( (k - 2) * (k - 2)));
            if ( (k / 2) * 2 == k) {
                amom2 = amom2.subtract( (DoubleDouble.valueOf(4.0)).divide(ak22.subtract(DoubleDouble.valueOf(1.0))));
            }
            res24 = res24.add(cheb24[k - 1].multiply(amom2));
            amom0 = (DoubleDouble) amom1.clone();
            amom1 = (DoubleDouble) amom2.clone();
        } // for (k = 14; k <= 25; k++)
        result[0] = (DoubleDouble) res24.clone();
        abserr[0] = (res24.subtract(res12)).abs();
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
    private void dqc25f(final DoubleDouble a, final DoubleDouble b, final DoubleDouble omega, final int nrmom,
            final int ksave, final DoubleDouble result[], final DoubleDouble abserr[], final int neval[],
            final DoubleDouble resabs[], final DoubleDouble resasc[]) {
        // mid point of the integration variable
        DoubleDouble centr;

        // half-length of the integration interval
        DoubleDouble hlgth;

        // value of the function f at the points
        // (b-a)*0.5*cos(k*PI/12) + (b+a)*0.5, k = 0, ..., 24.
        final DoubleDouble fval[] = new DoubleDouble[25];

        // coefficients of the chebyshev series expansion of degree 12,
        // for the function f, in the interval (a,b)
        final DoubleDouble cheb12[] = new DoubleDouble[13];

        // coefficients of the chebyshev series expansion of degree 24,
        // for the function f, in the interval (a,b)
        final DoubleDouble cheb24[] = new DoubleDouble[25];

        // approximation to the integral of
        // cos(0.5*(b-a)*omega*x)*f(0.5*(b-a)*x+0.5*(b+a))
        // over (-1,+1) using the chebyshev sereis expansion of degree 12
        DoubleDouble resc12;

        // approximation to the same integral, using the
        // chebyshev seies expansion of degree 24
        DoubleDouble resc24;

        // the analogue of resc12 for the sine
        DoubleDouble ress12;

        // the analogue of resc24 for the sine
        DoubleDouble ress24;

        /** Values cos(k*PI/24) k = 1, ..., 11 to be used for the chebyshev series expansion of f */
        final DoubleDouble x[] = new DoubleDouble[11];

        final DoubleDouble d[] = new DoubleDouble[25];
        final DoubleDouble d1[] = new DoubleDouble[25];
        final DoubleDouble d2[] = new DoubleDouble[25];
        final DoubleDouble v[] = new DoubleDouble[28];
        DoubleDouble vp[];

        DoubleDouble ac;
        DoubleDouble an;
        DoubleDouble an2;
        DoubleDouble as;
        DoubleDouble asap;
        DoubleDouble ass;
        DoubleDouble conc;
        DoubleDouble cons;
        DoubleDouble cospar;
        DoubleDouble estc;
        DoubleDouble ests;
        DoubleDouble parint;
        DoubleDouble par2;
        DoubleDouble par22;
        DoubleDouble sinpar;

        int i;
        final int iers[] = new int[1];
        int isym;
        int j;
        int k;
        int m = 0;
        int noequ = 0;
        int noeq1 = 0;

        for (k = 1; k <= 11; k++) {
            x[k - 1] = ( ( (DoubleDouble.valueOf(k)).multiply(DoubleDouble.PI)).divide(DoubleDouble.valueOf(24.0)))
                    .cos();
        }

        centr = (DoubleDouble.valueOf(0.5)).multiply(b.add(a));
        hlgth = (DoubleDouble.valueOf(0.5)).multiply(b.subtract(a));
        parint = omega.multiply(hlgth);

        // Compute the integral using the 15-point gauss-kronrod
        // formula if the value of the parameter in the integrand
        // is small

        if ( (parint.abs()).le(DoubleDouble.valueOf(2.0))) {
            dqk15w(omega, a, b, result, abserr, resabs, resasc);
            neval[0] = 15;
            return;
        } // if (Math.abw(parint) <= 2.0)

        // Compute the integral using the generalized clenshaw-curtis method.

        conc = hlgth.multiply( (centr.multiply(omega)).cos());
        cons = hlgth.multiply( (centr.multiply(omega)).sin());
        neval[0] = 25;

        // Check whether the chebyshev moments for this interval
        // have already been computed.

        if ( (nrmom >= momcom) && (ksave != 1)) {

            // Compute a new set of chebyshev moments

            m = momcom + 1;
            par2 = parint.multiply(parint);
            par22 = par2.add(DoubleDouble.valueOf(2.0));
            sinpar = parint.sin();
            cospar = parint.cos();

            // Compute the chebyshev moments with respect to cosine.

            v[0] = ( (DoubleDouble.valueOf(2.0)).multiply(sinpar)).divide(parint);
            v[1] = ( ( (DoubleDouble.valueOf(8.0)).multiply(cospar)).add( ( (par2.add(par2)).subtract(DoubleDouble
                    .valueOf(8.0))).multiply(sinpar)).divide(parint)).divide(par2);
            v[2] = ( ( ( (DoubleDouble.valueOf(32.0)).multiply(par2.subtract(DoubleDouble.valueOf(12.0))))
                    .multiply(cospar)).add( (DoubleDouble.valueOf(2.0)).multiply(
                    ( (par2.subtract(DoubleDouble.valueOf(80.0))).multiply(par2)).add(DoubleDouble.valueOf(192.0)))
                    .multiply(sinpar)).divide(parint)).divide(par2.multiply(par2));
            ac = (DoubleDouble.valueOf(8.0)).multiply(cospar);
            as = ( (DoubleDouble.valueOf(24.0)).multiply(parint)).multiply(sinpar);
            if ( (parint.abs()).le(DoubleDouble.valueOf(24.0))) {

                // compute the chebyshev moments as the solutions of a
                // boundary value problem with 1 initial value (v(3)) and 1
                // end value (computed using an asymptotic formula).

                noequ = 25;
                noeq1 = noequ - 1;
                an = DoubleDouble.valueOf(6.0);
                for (k = 0; k < noeq1; k++) {
                    an2 = an.multiply(an);
                    d[k] = ( ( (DoubleDouble.valueOf(2.0)).multiply(an2.subtract(DoubleDouble.valueOf(4.0))))
                            .multiply( (par22.subtract(an2)).subtract(an2))).negate();
                    d2[k] = ( (an.subtract(DoubleDouble.valueOf(1.0))).multiply(an.subtract(DoubleDouble.valueOf(2.0))))
                            .multiply(par2);
                    d1[k + 1] = ( (an.add(DoubleDouble.valueOf(3.0))).multiply(an.add(DoubleDouble.valueOf(4.0))))
                            .multiply(par2);
                    v[k + 3] = as.subtract( (an2.subtract(DoubleDouble.valueOf(4.0))).multiply(ac));
                    an = an.add(DoubleDouble.valueOf(2.0));
                } // for (k = 0; k < noeq1; k++)
                an2 = an.multiply(an);
                d[noequ - 1] = ( ( (DoubleDouble.valueOf(2.0)).multiply(an2.subtract(DoubleDouble.valueOf(4.0))))
                        .multiply( (par22.subtract(an2)).subtract(an2))).negate();
                v[noequ + 2] = as.subtract( (an2.subtract(DoubleDouble.valueOf(4.0))).multiply(ac));
                v[3] = v[3].subtract( ( (DoubleDouble.valueOf(56.0)).multiply(par2)).multiply(v[2]));
                ass = parint.multiply(sinpar);
                asap = ( ( ( ( ( ( ( ( ( ( ( (DoubleDouble.valueOf(210.0)).multiply(par2)).subtract(DoubleDouble
                        .valueOf(1.0))).multiply(cospar)).subtract( (DoubleDouble.valueOf(105.0)).multiply(par2))
                        .subtract(DoubleDouble.valueOf(63.0))).multiply(ass)).divide(an2)).subtract( ( (DoubleDouble
                        .valueOf(1.0)).subtract( (DoubleDouble.valueOf(15.0)).multiply(par2))).multiply(cospar))
                        .add( (DoubleDouble.valueOf(15.0)).multiply(ass))).divide(an2)).subtract(cospar)
                        .add( (DoubleDouble.valueOf(3.0)).multiply(ass))).divide(an2)).subtract(cospar)).divide(an2);
                v[noequ + 2] = v[noequ + 2].subtract( ( ( ( ( (DoubleDouble.valueOf(2.0)).multiply(asap))
                        .multiply(par2)).multiply(an.subtract(DoubleDouble.valueOf(1.0))))).multiply(an
                        .subtract(DoubleDouble.valueOf(2.0))));

                // solve the tridiagonal system by means of gaussian
                // elimination with partial pivoting.
                vp = new DoubleDouble[25];
                for (i = 0; i < 25; i++) {
                    vp[i] = (DoubleDouble) v[i + 3].clone();
                }
                dgtsl(noequ, d1, d, d2, vp, iers);
                for (i = 0; i < 25; i++) {
                    v[i + 3] = (DoubleDouble) vp[i].clone();
                }
            } // if (Math.abs(parint) <= 24.0)
            else { // Math.abs(parint) > 24.0

                // compute the chebyshev moments by means of forward recursion.

                an = DoubleDouble.valueOf(4.0);
                for (i = 3; i < 13; i++) {
                    an2 = an.multiply(an);
                    v[i] = ( ( (an2.subtract(DoubleDouble.valueOf(4.0))).multiply( ( ( (DoubleDouble.valueOf(2.0))
                            .multiply( (par22.subtract(an2)).subtract(an2))).multiply(v[i - 1])).subtract(ac)).add(as))
                            .subtract( ( (par2.multiply(an.add(DoubleDouble.valueOf(1.0)))).multiply(an
                                    .add(DoubleDouble.valueOf(2.0)))).multiply(v[i - 2]))).divide( (par2.multiply(an
                            .subtract(DoubleDouble.valueOf(1.0)))).multiply(an.subtract(DoubleDouble.valueOf(2.0))));
                    an = an.add(DoubleDouble.valueOf(2.0));
                } // for (i = 3; i < 13; i++)
            } // else Math.abs(parint) > 24.0)
            for (j = 1; j <= 13; j++) {
                chebmo[m - 1][2 * j - 2] = v[j - 1];
            }

            // Compute the chebyshev moments with respect to sine

            v[0] = ( (DoubleDouble.valueOf(2.0)).multiply(sinpar.subtract(parint.multiply(cospar)))).divide(par2);
            v[1] = ( ( ( (DoubleDouble.valueOf(18.0)).subtract( (DoubleDouble.valueOf(48.0)).divide(par2)))
                    .multiply(sinpar)).divide(par2)).add( ( ( (DoubleDouble.valueOf( -2.0)).add( (DoubleDouble
                    .valueOf(48.0)).divide(par2))).multiply(cospar)).divide(parint));
            ac = ( (DoubleDouble.valueOf( -24.0)).multiply(parint)).multiply(cospar);
            as = (DoubleDouble.valueOf( -8.0)).multiply(sinpar);
            if ( (parint.abs()).le(DoubleDouble.valueOf(24.0))) {

                // compute the chebyshev moments as the solutions of a boundary
                // value problem with 1 initial value (v[1]) and 1 end value
                // (computed using an asymptotic formula).

                an = DoubleDouble.valueOf(5.0);
                for (k = 0; k < noeq1; k++) {
                    an2 = an.multiply(an);
                    d[k] = ( (DoubleDouble.valueOf( -2.0)).multiply(an2.subtract(DoubleDouble.valueOf(4.0))))
                            .multiply( (par22.subtract(an2)).subtract(an2));
                    d2[k] = ( (an.subtract(DoubleDouble.valueOf(1.0))).multiply(an.subtract(DoubleDouble.valueOf(2.0))))
                            .multiply(par2);
                    d1[k + 1] = ( (an.add(DoubleDouble.valueOf(3.0))).multiply(an.add(DoubleDouble.valueOf(4.0))))
                            .multiply(par2);
                    v[k + 2] = ac.add( (an2.subtract(DoubleDouble.valueOf(4.0))).multiply(as));
                    an = an.add(DoubleDouble.valueOf(2.0));
                } // for (k = 0; k < noeq1; k++)
                an2 = an.multiply(an);
                d[noequ - 1] = ( (DoubleDouble.valueOf( -2.0)).multiply(an2.subtract(DoubleDouble.valueOf(4.0))))
                        .multiply( (par22.subtract(an2)).subtract(an2));
                v[noequ + 1] = ac.add( (an2.subtract(DoubleDouble.valueOf(4.0))).multiply(as));
                v[2] = v[2].subtract( ( (DoubleDouble.valueOf(42.0)).multiply(par2)).multiply(v[1]));
                ass = parint.multiply(cospar);
                asap = ( ( ( ( ( ( ( ( ( ( (DoubleDouble.valueOf(105.0)).multiply(par2)).subtract(DoubleDouble
                        .valueOf(63.0))).multiply(ass)).add( ( (DoubleDouble.valueOf(210.0)).multiply(par2))
                        .subtract(DoubleDouble.valueOf(1.0))).multiply(sinpar)).divide(an2)).add(
                        ( (DoubleDouble.valueOf(15.0)).multiply(par2)).subtract(DoubleDouble.valueOf(1.0))).multiply(
                        sinpar).subtract( (DoubleDouble.valueOf(15.0)).multiply(ass))).divide(an2))
                        .subtract( (DoubleDouble.valueOf(3.0)).multiply(ass)).subtract(sinpar)).divide(an2))
                        .subtract(sinpar)).divide(an2);
                v[noequ + 1] = v[noequ + 1].subtract( ( ( ( (DoubleDouble.valueOf(2.0)).multiply(asap)).multiply(par2))
                        .multiply(an.subtract(DoubleDouble.valueOf(1.0)))).multiply(an.subtract(DoubleDouble
                        .valueOf(2.0))));

                // solve the tridiagonal system by means of gaussian
                // elimination with partial pivoting.

                vp = new DoubleDouble[26];
                for (i = 0; i < 26; i++) {
                    vp[i] = (DoubleDouble) v[i + 2].clone();
                }
                dgtsl(noequ, d1, d, d2, vp, iers);
                for (i = 0; i < 26; i++) {
                    v[i + 2] = (DoubleDouble) vp[i].clone();
                }
            } // if (Math.abs(parint) <= 24.0)
            else { // Math.abs(parint) > 24.0

                // compute the chebyshev moments by means of forward recursion.

                an = DoubleDouble.valueOf(3.0);
                for (i = 2; i < 12; i++) {
                    an2 = an.multiply(an);
                    v[i] = ( ( (an2.subtract(DoubleDouble.valueOf(4.0))).multiply( ( ( (DoubleDouble.valueOf(2.0))
                            .multiply( (par22.subtract(an2)).subtract(an2))).multiply(v[i - 1])).add(as)).add(ac))
                            .subtract( ( (par2.multiply(an.add(DoubleDouble.valueOf(1.0)))).multiply(an
                                    .add(DoubleDouble.valueOf(2.0)))).multiply(v[i - 2]))).divide( (par2.multiply(an
                            .subtract(DoubleDouble.valueOf(1.0)))).multiply(an.subtract(DoubleDouble.valueOf(2.0))));
                    an = an.add(DoubleDouble.valueOf(2.0));
                } // for (i = 2; i < 12; i++)
            } // else Math.abs(parint) > 24.0
            for (j = 1; j <= 12; j++) {
                chebmo[m - 1][2 * j - 1] = (DoubleDouble) v[j - 1].clone();
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
            fval[0] = (DoubleDouble.valueOf(0.5)).multiply(intFuncTest(centr.add(hlgth)));
            fval[12] = intFuncTest(centr);
            fval[24] = (DoubleDouble.valueOf(0.5)).multiply(intFuncTest(centr.subtract(hlgth)));
            for (i = 2; i <= 12; i++) {
                isym = 26 - i;
                fval[i - 1] = intFuncTest( (hlgth.multiply(x[i - 2])).add(centr));
                fval[isym - 1] = intFuncTest(centr.subtract(hlgth.multiply(x[i - 2])));
            } // for (i = 2; i <= 12; i++)
        } // if (selfTest)
        else {
            fval[0] = (DoubleDouble.valueOf(0.5)).multiply(intFunc(centr.add(hlgth)));
            fval[12] = intFunc(centr);
            fval[24] = (DoubleDouble.valueOf(0.5)).multiply(intFunc(centr.subtract(hlgth)));
            for (i = 2; i <= 12; i++) {
                isym = 26 - i;
                fval[i - 1] = intFunc( (hlgth.multiply(x[i - 2])).add(centr));
                fval[isym - 1] = intFunc(centr.subtract(hlgth.multiply(x[i - 2])));
            } // for (i = 2; i <= 12; i++)
        }
        dqcheb(x, fval, cheb12, cheb24);

        // compute the integral and error estimates.

        resc12 = cheb12[12].multiply(chebmo[m - 1][12]);
        ress12 = DoubleDouble.valueOf(0.0);
        k = 11;
        for (j = 1; j <= 6; j++) {
            resc12 = resc12.add(cheb12[k - 1].multiply(chebmo[m - 1][k - 1]));
            ress12 = ress12.add(cheb12[k].multiply(chebmo[m - 1][k]));
            k = k - 2;
        } // for (j = 1; j <= 6; j++)
        resc24 = cheb24[24].multiply(chebmo[m - 1][24]);
        ress24 = DoubleDouble.valueOf(0.0);
        resabs[0] = cheb24[24].abs();
        k = 23;
        for (j = 1; j <= 12; j++) {
            resc24 = resc24.add(cheb24[k - 1].multiply(chebmo[m - 1][k - 1]));
            ress24 = ress24.add(cheb24[k].multiply(chebmo[m - 1][k]));
            resabs[0] = (cheb24[k - 1].abs()).add(cheb24[k].abs());
            k = k - 2;
        } // for (j = 1; j <= 12; j++)
        estc = (resc24.subtract(resc12)).abs();
        ests = (ress24.subtract(ress12)).abs();
        resabs[0] = resabs[0].multiply(hlgth.abs());
        if (integr == 1) {
            result[0] = (conc.multiply(resc24)).subtract(cons.multiply(ress24));
            abserr[0] = ( (conc.multiply(estc)).abs()).add( (cons.multiply(ests)).abs());
        } else {
            result[0] = (conc.multiply(ress24)).add(cons.multiply(resc24));
            abserr[0] = ( (conc.multiply(ests)).abs()).add( (cons.multiply(estc)).abs());
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
     * end point singularities c standard fortran subroutine c DoubleDouble precision version c c parameters c f -
     * DoubleDouble precision c function subprogram defining the integrand c f(x). the actual name for f needs to be
     * declared c e x t e r n a l in the driver program. c c a - DoubleDouble precision c left end point of the original
     * interval c c b - DoubleDouble precision c right end point of the original interval, b.gt.a c c bl - DoubleDouble
     * precision c lower limit of integration, bl.ge.a c c br - DoubleDouble precision c upper limit of integration,
     * br.le.b c c alfa - DoubleDouble precision c parameter in the weight function c c beta - DoubleDouble precision c
     * parameter in the weight function c c ri,rj,rg,rh - DoubleDouble precision c modified chebyshev moments for the
     * application c of the generalized clenshaw-curtis c method (computed in subroutine dqmomo) c c result -
     * DoubleDouble precision c approximation to the integral c result is computed by using a generalized c
     * clenshaw-curtis method if b1 = a or br = b. c in all other cases the 15-point kronrod c rule is applied, obtained
     * by optimal addition of c abscissae to the 7-point gauss rule. c c abserr - DoubleDouble precision c estimate of
     * the modulus of the absolute error, c which should equal or exceed abs(i-result) c c resasc - DoubleDouble
     * precision c approximation to the integral of abs(f*w-i/(b-a)) c c integr - integer c which determines the weight
     * function c = 1 w(x) = (x-a)**alfa*(b-x)**beta c = 2 w(x) = (x-a)**alfa*(b-x)**beta*log(x-a) c = 3 w(x) =
     * (x-a)**alfa*(b-x)**beta*log(b-x) c = 4 w(x) = (x-a)**alfa*(b-x)**beta*log(x-a)* c log(b-x) c c nev - integer c
     * number of integrand evaluations c***references (none) c***routines called dqcheb,dqk15w c***end prologue dqc25s c
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
    private void dqc25s(final DoubleDouble bl, final DoubleDouble br, final DoubleDouble ri[], final DoubleDouble rj[],
            final DoubleDouble rg[], final DoubleDouble rh[], final DoubleDouble result[], final DoubleDouble abserr[],
            final DoubleDouble resasc[], final int nev[]) {
        // mid point of the interval (bl, br)
        DoubleDouble centr;

        // half-length of the interval (bl, br)
        DoubleDouble hlgth = DoubleDouble.valueOf(0.0);

        // value of the function f at the points
        // (br-bl)*0.5*cos(k*PI/24) + (br+bl)*0.5, k = 0, ..., 24.
        final DoubleDouble fval[] = new DoubleDouble[25];

        // coefficients of the chebyshev series expansion of degree 12,
        // for the function f, in the interval (bl,br)
        final DoubleDouble cheb12[] = new DoubleDouble[13];

        // coefficients of the chebyshev series expansion of degree 24,
        // for the function f, in the interval (bl,br)
        final DoubleDouble cheb24[] = new DoubleDouble[25];

        // approximation to the integral obtained from cheb12
        DoubleDouble res12 = DoubleDouble.valueOf(0.0);

        // approximation to the integral obtained from cheb24
        DoubleDouble res24 = DoubleDouble.valueOf(0.0);

        DoubleDouble dc;
        DoubleDouble factor = DoubleDouble.valueOf(0.0);
        DoubleDouble fix = DoubleDouble.valueOf(0.0);
        DoubleDouble u;
        final DoubleDouble resabs[] = new DoubleDouble[1];

        /** Values cos(k*PI/24) k = 1, ..., 11 to be used for the chebyshev series expansion of f */
        final DoubleDouble x[] = new DoubleDouble[11];

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
            x[k - 1] = ( ( (DoubleDouble.valueOf(k)).multiply(DoubleDouble.PI)).divide(DoubleDouble.valueOf(24.0)))
                    .cos();
        }

        nev[0] = 25;
        if (bl.equals(lower) && (alfa.ne(DoubleDouble.valueOf(0.0)) || integr == 2 || integr == 4)) {
            do10 = true;
        } else {
            do5 = true;
        }
        if (do5) {
            if (br.equals(upper) && (beta.ne(DoubleDouble.valueOf(0.0)) || integr == 3 || integr == 4)) {
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

            hlgth = (DoubleDouble.valueOf(0.5)).multiply(br.subtract(bl));
            centr = (DoubleDouble.valueOf(0.5)).multiply(br.add(bl));
            fix = upper.subtract(centr);
            if (selfTest) {
                fval[0] = ( (DoubleDouble.valueOf(0.5)).multiply(intFuncTest(hlgth.add(centr)))).multiply( (fix
                        .subtract(hlgth)).pow(beta));
                fval[12] = (intFuncTest(centr)).multiply(fix.pow(beta));
                fval[24] = ( (DoubleDouble.valueOf(0.5)).multiply(intFuncTest(centr.subtract(hlgth)))).multiply( (fix
                        .add(hlgth)).pow(beta));
                for (i = 2; i <= 12; i++) {
                    u = hlgth.multiply(x[i - 2]);
                    isym = 26 - i;
                    fval[i - 1] = (intFuncTest(u.add(centr))).multiply( (fix.subtract(u)).pow(beta));
                    fval[isym - 1] = (intFuncTest(centr.subtract(u))).multiply( (fix.add(u)).pow(beta));
                } // for (i = 2; i <= 12; i++)
            } // if (selfTest)
            else {
                fval[0] = ( (DoubleDouble.valueOf(0.5)).multiply(intFunc(hlgth.add(centr)))).multiply( (fix
                        .subtract(hlgth)).pow(beta));
                fval[12] = (intFunc(centr)).multiply(fix.pow(beta));
                fval[24] = ( (DoubleDouble.valueOf(0.5)).multiply(intFunc(centr.subtract(hlgth)))).multiply( (fix
                        .add(hlgth)).pow(beta));
                for (i = 2; i <= 12; i++) {
                    u = hlgth.multiply(x[i - 2]);
                    isym = 26 - i;
                    fval[i - 1] = (intFunc(u.add(centr))).multiply( (fix.subtract(u)).pow(beta));
                    fval[isym - 1] = (intFunc(centr.subtract(u))).multiply( (fix.add(u)).pow(beta));
                } // for (i = 2; i <= 12; i++)
            } // else
            factor = hlgth.pow(alfa.add(DoubleDouble.valueOf(1.0)));
            result[0] = DoubleDouble.valueOf(0.0);
            abserr[0] = DoubleDouble.valueOf(0.0);
            res12 = DoubleDouble.valueOf(0.0);
            res24 = DoubleDouble.valueOf(0.0);
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
                res12 = res12.add(cheb12[i].multiply(ri[i]));
                res24 = res24.add(cheb24[i].multiply(ri[i]));
            } // for (i = 0; i <= 12; i++)
            for (i = 13; i <= 24; i++) {
                res24 = res24.add(cheb24[i].multiply(ri[i]));
            }
            if (integr == 1) {
                do130 = true;
            } else {
                do45 = true;
            }
        } // if (do25)
        if (do45) {

            // integr = 2

            dc = (br.subtract(bl)).log();
            result[0] = res24.multiply(dc);
            abserr[0] = ( (res24.subtract(res12)).multiply(dc)).abs();
            res12 = DoubleDouble.valueOf(0.0);
            res24 = DoubleDouble.valueOf(0.0);
            for (i = 0; i <= 12; i++) {
                res12 = res12.add(cheb12[i].multiply(rg[i]));
                res24 = res12.add(cheb24[i].multiply(rg[i]));
            } // for (i = 0; i <= 12; i++)
            for (i = 13; i <= 24; i++) {
                res24 = res24.add(cheb24[i].multiply(rg[i]));
            }
            do130 = true;
        } // if (do45)

        if (do70) {

            // compute the chebyshev series expansion of the following function
            // f4 = f1*log(0.5*(b+b-br-a)-0.5*(br-a)*x)

            fval[0] = fval[0].multiply( (fix.subtract(hlgth)).log());
            fval[12] = fval[12].multiply(fix.log());
            fval[24] = fval[24].multiply( (fix.add(hlgth)).log());
            for (i = 2; i <= 12; i++) {
                u = hlgth.multiply(x[i - 2]);
                isym = 26 - i;
                fval[i - 1] = fval[i - 1].multiply( (fix.subtract(u)).log());
                fval[isym - 1] = fval[isym - 1].multiply( (fix.add(u)).log());
            } // for (i = 2; i <= 12; i++)
            dqcheb(x, fval, cheb12, cheb24);

            // integr = 3 (or 4)

            for (i = 0; i <= 12; i++) {
                res12 = res12.add(cheb12[i].multiply(ri[i]));
                res24 = res24.add(cheb24[i].multiply(ri[i]));
            } // for (i = 0; i <= 12; i++)
            for (i = 13; i <= 24; i++) {
                res24 = res24.add(cheb24[i].multiply(ri[i]));
            }
            if (integr == 3) {
                do130 = true;
            } else {
                do105 = true;
            }
        } // if (do70)

        if (do105) {

            // integr = 4

            dc = (br.subtract(bl)).log();
            result[0] = res24.multiply(dc);
            abserr[0] = ( (res24.subtract(res12)).multiply(dc)).abs();
            res12 = DoubleDouble.valueOf(0.0);
            res24 = DoubleDouble.valueOf(0.0);
            for (i = 0; i <= 12; i++) {
                res12 = res12.add(cheb12[i].multiply(rg[i]));
                res24 = res24.add(cheb24[i].multiply(rg[i]));
            } // for (i = 0; i <= 12; i++)
            for (i = 13; i <= 24; i++) {
                res24 = res24.add(cheb24[i].multiply(rg[i]));
            }
            do130 = true;
        } // if (do105
        if (do130) {
            result[0] = (result[0].add(res24)).multiply(factor);
            abserr[0] = (abserr[0].add( (res24.subtract(res12)).abs())).multiply(factor);
            return;
        } // if (do130)

        if (do140) {

            // this part of the program is executed only if b = br.
            // ----------------------------------------------------

            // compute the chebyshev series expansion of the following function
            // f2 = (0.5*(b+bl-a-a)+0.5*(b-bl)*x)**alfa*f(0.5*(b-bl)*x+0.5*(b+bl))

            hlgth = (DoubleDouble.valueOf(0.5)).multiply(br.subtract(bl));
            centr = (DoubleDouble.valueOf(0.5)).multiply(br.add(bl));
            fix = centr.subtract(lower);
            if (selfTest) {
                fval[0] = ( (DoubleDouble.valueOf(0.5)).multiply(intFuncTest(hlgth.add(centr)))).multiply( (fix
                        .add(hlgth)).pow(alfa));
                fval[12] = (intFuncTest(centr)).multiply(fix.pow(alfa));
                fval[24] = ( (DoubleDouble.valueOf(0.5)).multiply(intFuncTest(centr.subtract(hlgth)))).multiply( (fix
                        .subtract(hlgth)).pow(alfa));
                for (i = 2; i <= 12; i++) {
                    u = hlgth.multiply(x[i - 2]);
                    isym = 26 - i;
                    fval[i - 1] = (intFuncTest(u.add(centr))).multiply( (fix.add(u)).pow(alfa));
                    fval[isym - 1] = (intFuncTest(centr.subtract(u))).multiply( (fix.subtract(u)).pow(alfa));
                } // for (i = 2; i <= 12; i++)
            } // if (selfTest)
            else {
                fval[0] = ( (DoubleDouble.valueOf(0.5)).multiply(intFunc(hlgth.add(centr)))).multiply( (fix.add(hlgth))
                        .pow(alfa));
                fval[12] = (intFunc(centr)).multiply(fix.pow(alfa));
                fval[24] = ( (DoubleDouble.valueOf(0.5)).multiply(intFunc(centr.subtract(hlgth)))).multiply( (fix
                        .subtract(hlgth)).pow(alfa));
                for (i = 2; i <= 12; i++) {
                    u = hlgth.multiply(x[i - 2]);
                    isym = 26 - i;
                    fval[i - 1] = (intFunc(u.add(centr))).multiply( (fix.add(u)).pow(alfa));
                    fval[isym - 1] = (intFunc(centr.subtract(u))).multiply( (fix.subtract(u)).pow(alfa));
                } // for (i = 2; i <= 12; i++)
            } // else
            factor = hlgth.pow(beta.add(DoubleDouble.valueOf(1.0)));
            result[0] = DoubleDouble.valueOf(0.0);
            abserr[0] = DoubleDouble.valueOf(0.0);
            res12 = DoubleDouble.valueOf(0.0);
            res24 = DoubleDouble.valueOf(0.0);
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
                res12 = res12.add(cheb12[i].multiply(rj[i]));
                res24 = res24.add(cheb24[i].multiply(rj[i]));
            } // for (i = 0; i <= 12; i++)
            for (i = 13; i <= 24; i++) {
                res24 = res24.add(cheb24[i].multiply(rj[i]));
            }
            if (integr == 1) {
                do260 = true;
            } else {
                do175 = true;
            }
        } // if (do155)

        if (do175) {

            // integr = 3

            dc = (br.subtract(bl)).log();
            result[0] = res24.multiply(dc);
            abserr[0] = ( (res24.subtract(res12)).multiply(dc)).abs();
            res12 = DoubleDouble.valueOf(0.0);
            res24 = DoubleDouble.valueOf(0.0);
            for (i = 0; i <= 12; i++) {
                res12 = res12.add(cheb12[i].multiply(rh[i]));
                res24 = res24.add(cheb24[i].multiply(rh[i]));
            } // for (i = 0; i <= 12; i++)
            for (i = 13; i <= 24; i++) {
                res24 = res24.add(cheb24[i].multiply(rh[i]));
            }
            do260 = true;
        } // if (do175)

        if (do200) {

            // compute the chebyshev series expansion of the following function
            // f3 = f2*log(0.5*(b-bl)*x+0.5*(b+bl-a-a))

            fval[0] = fval[0].multiply( (hlgth.add(fix)).log());
            fval[12] = fval[12].multiply(fix.log());
            fval[24] = fval[24].multiply( (fix.subtract(hlgth)).log());
            for (i = 2; i <= 12; i++) {
                u = hlgth.multiply(x[i - 2]);
                isym = 26 - i;
                fval[i - 1] = fval[i - 1].multiply( (u.add(fix)).log());
                fval[isym - 1] = fval[isym - 1].multiply( (fix.subtract(u)).log());
            } // for (i = 2; i <= 12; i++)
            dqcheb(x, fval, cheb12, cheb24);

            // integr = 2 (or 4)

            for (i = 0; i <= 12; i++) {
                res12 = res12.add(cheb12[i].multiply(rj[i]));
                res24 = res24.add(cheb24[i].multiply(rj[i]));
            } // for (i = 0; i <= 12; i++)
            for (i = 13; i <= 24; i++) {
                res24 = res24.add(cheb24[i].multiply(rj[i]));
            }
            if (integr == 2) {
                do260 = true;
            } else {
                do235 = true;
            }
        } // if (do200)

        if (do235) {
            dc = (br.subtract(bl)).log();
            result[0] = res24.multiply(dc);
            abserr[0] = ( (res24.subtract(res12)).multiply(dc)).abs();
            res12 = DoubleDouble.valueOf(0.0);
            res24 = DoubleDouble.valueOf(0.0);

            // integr = 4

            for (i = 0; i <= 12; i++) {
                res12 = res12.add(cheb12[i].multiply(rh[i]));
                res24 = res24.add(cheb24[i].multiply(rh[i]));
            } // for (i = 0; i <= 12; i++)
            for (i = 13; i <= 24; i++) {
                res24 = res24.add(cheb24[i].multiply(rh[i]));
            }
            do260 = true;
        } // if (do235)

        if (do260) {
            result[0] = (result[0].add(res24)).multiply(factor);
            abserr[0] = (abserr[0].add( (res24.subtract(res12)).abs())).multiply(factor);
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
     * degree k. c***description c c chebyshev series expansion c standard fortran subroutine c DoubleDouble precision
     * version c c parameters c on entry c x - DoubleDouble precision c vector of dimension 11 containing the c values
     * cos(k*pi/24), k = 1, ..., 11 c c fval - DoubleDouble precision c vector of dimension 25 containing the c function
     * values at the points c (b+a+(b-a)*cos(k*pi/24))/2, k = 0, ...,24, c where (a,b) is the approximation interval. c
     * fval(1) and fval(25) are divided by two c (these values are destroyed at output). c c on return c cheb12 -
     * DoubleDouble precision c vector of dimension 13 containing the c chebyshev coefficients for degree 12 c c cheb24 -
     * DoubleDouble precision c vector of dimension 25 containing the c chebyshev coefficients for degree 24 c c***end
     * prologue dqcheb c
     * 
     * @param x
     * @param fval
     * @param cheb12
     * @param cheb24
     */
    private void dqcheb(final DoubleDouble x[], final DoubleDouble fval[], final DoubleDouble cheb12[],
            final DoubleDouble cheb24[]) {
        DoubleDouble alam;
        DoubleDouble alam1;
        DoubleDouble alam2;
        DoubleDouble part1;
        DoubleDouble part2;
        DoubleDouble part3;
        final DoubleDouble v[] = new DoubleDouble[12];
        int i;
        int j;

        for (i = 1; i <= 12; i++) {
            j = 26 - i;
            v[i - 1] = fval[i - 1].subtract(fval[j - 1]);
            fval[i - 1] = fval[i - 1].add(fval[j - 1]);
        }
        alam1 = v[0].subtract(v[8]);
        alam2 = x[5].multiply( (v[2].subtract(v[6])).subtract(v[10]));
        cheb12[3] = alam1.add(alam2);
        cheb12[9] = alam1.subtract(alam2);
        alam1 = (v[1].subtract(v[7])).subtract(v[9]);
        alam2 = (v[3].subtract(v[5])).subtract(v[11]);
        alam = (x[2].multiply(alam1)).add(x[8].multiply(alam2));
        cheb24[3] = cheb12[3].add(alam);
        cheb24[21] = cheb12[3].subtract(alam);
        alam = (x[8].multiply(alam1)).subtract(x[2].multiply(alam2));
        cheb24[9] = cheb12[9].add(alam);
        cheb24[15] = cheb12[9].subtract(alam);
        part1 = x[3].multiply(v[4]);
        part2 = x[7].multiply(v[8]);
        part3 = x[5].multiply(v[6]);
        alam1 = (v[0].add(part1)).add(part2);
        alam2 = ( (x[1].multiply(v[2])).add(part3)).add(x[9].multiply(v[10]));
        cheb12[1] = alam1.add(alam2);
        cheb12[11] = alam1.subtract(alam2);
        alam = ( ( ( ( (x[0].multiply(v[1])).add(x[2].multiply(v[3]))).add(x[4].multiply(v[5]))).add(x[6]
                .multiply(v[7]))).add(x[8].multiply(v[9]))).add(x[10].multiply(v[11]));
        cheb24[1] = cheb12[1].add(alam);
        cheb24[23] = cheb12[1].subtract(alam);
        alam = ( ( ( ( (x[10].multiply(v[1])).subtract(x[8].multiply(v[3]))).add(x[6].multiply(v[5]))).subtract(x[4]
                .multiply(v[7]))).add(x[2].multiply(v[9]))).subtract(x[0].multiply(v[11]));
        cheb24[11] = cheb12[11].add(alam);
        cheb24[13] = cheb12[11].subtract(alam);
        alam1 = (v[0].subtract(part1)).add(part2);
        alam2 = ( (x[9].multiply(v[2])).subtract(part3)).add(x[1].multiply(v[10]));
        cheb12[5] = alam1.add(alam2);
        cheb12[7] = alam1.subtract(alam2);
        alam = ( ( ( ( (x[4].multiply(v[1])).subtract(x[8].multiply(v[3]))).subtract(x[0].multiply(v[5])))
                .subtract(x[10].multiply(v[7]))).add(x[2].multiply(v[9]))).add(x[6].multiply(v[11]));
        cheb24[5] = cheb12[5].add(alam);
        cheb24[19] = cheb12[5].subtract(alam);
        alam = ( ( ( ( (x[6].multiply(v[1])).subtract(x[2].multiply(v[3]))).subtract(x[10].multiply(v[5]))).add(x[0]
                .multiply(v[7]))).subtract(x[8].multiply(v[9]))).subtract(x[4].multiply(v[11]));
        cheb24[7] = cheb12[7].add(alam);
        cheb24[17] = cheb12[7].subtract(alam);
        for (i = 1; i <= 6; i++) {
            j = 14 - i;
            v[i - 1] = fval[i - 1].subtract(fval[j - 1]);
            fval[i - 1] = fval[i - 1].add(fval[j - 1]);
        }
        alam1 = v[0].add(x[7].multiply(v[4]));
        alam2 = x[3].multiply(v[2]);
        cheb12[2] = alam1.add(alam2);
        cheb12[10] = alam1.subtract(alam2);
        cheb12[6] = v[0].subtract(v[4]);
        alam = ( (x[1].multiply(v[1])).add(x[5].multiply(v[3]))).add(x[9].multiply(v[5]));
        cheb24[2] = cheb12[2].add(alam);
        cheb24[22] = cheb12[2].subtract(alam);
        alam = x[5].multiply( (v[1].subtract(v[3])).subtract(v[5]));
        cheb24[6] = cheb12[6].add(alam);
        cheb24[18] = cheb12[6].subtract(alam);
        alam = ( (x[9].multiply(v[1])).subtract(x[5].multiply(v[3]))).add(x[1].multiply(v[5]));
        cheb24[10] = cheb12[10].add(alam);
        cheb24[14] = cheb12[10].subtract(alam);
        for (i = 1; i <= 3; i++) {
            j = 8 - i;
            v[i - 1] = fval[i - 1].subtract(fval[j - 1]);
            fval[i - 1] = fval[i - 1].add(fval[j - 1]);
        }
        cheb12[4] = v[0].add(x[7].multiply(v[2]));
        cheb12[8] = fval[0].subtract(x[7].multiply(fval[2]));
        alam = x[3].multiply(v[1]);
        cheb24[4] = cheb12[4].add(alam);
        cheb24[20] = cheb12[4].subtract(alam);
        alam = (x[7].multiply(fval[1])).subtract(fval[3]);
        cheb24[8] = cheb12[8].add(alam);
        cheb24[16] = cheb12[8].subtract(alam);
        cheb12[0] = fval[0].add(fval[2]);
        alam = fval[1].add(fval[3]);
        cheb24[0] = cheb12[0].add(alam);
        cheb24[24] = cheb12[0].subtract(alam);
        cheb12[12] = v[0].subtract(v[2]);
        cheb24[12] = (DoubleDouble) cheb12[12].clone();
        alam = (DoubleDouble.valueOf(1.0)).divide(DoubleDouble.valueOf(6.0));
        for (i = 1; i <= 11; i++) {
            cheb12[i] = cheb12[i].multiply(alam);
        }
        alam = (DoubleDouble.valueOf(0.5)).multiply(alam);
        cheb12[0] = cheb12[0].multiply(alam);
        cheb12[12] = cheb12[12].multiply(alam);
        for (i = 1; i <= 23; i++) {
            cheb24[i] = cheb24[i].multiply(alam);
        }
        cheb24[0] = ( (DoubleDouble.valueOf(0.5)).multiply(alam)).multiply(cheb24[0]);
        cheb24[24] = ( (DoubleDouble.valueOf(0.5)).multiply(alam)).multiply(cheb24[24]);
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
     * subroutine c DoubleDouble precision version
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
    private void dqelg(final int[] n, final DoubleDouble[] epstab, final DoubleDouble[] result,
            final DoubleDouble[] abserr, final DoubleDouble[] res31a, final int[] nres) {
        DoubleDouble delta1;
        DoubleDouble delta2;
        DoubleDouble delta3;
        DoubleDouble epsinf;

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
        abserr[0] = (DoubleDouble) oflow.clone();
        result[0] = (DoubleDouble) epstab[n[0] - 1].clone();

        if (n[0] < 3) {
            abserr[0] = abserr[0].max( ( (DoubleDouble.valueOf(5.0)).multiply(epmach)).multiply(result[0].abs()));

            return;
        } // if (n[0] < 3)

        limexp = 50;
        epstab[n[0] + 1] = (DoubleDouble) epstab[n[0] - 1].clone();
        newelm = (n[0] - 1) / 2;
        epstab[n[0] - 1] = (DoubleDouble) oflow.clone();
        num = n[0];
        k1 = n[0];

        for (i = 1; i <= newelm; i++) {
            k2 = k1 - 1;
            k3 = k1 - 2;
            res = (DoubleDouble) epstab[k1 + 1].clone();
            e0 = (DoubleDouble) epstab[k3 - 1].clone();
            e1 = (DoubleDouble) epstab[k2 - 1].clone();
            e2 = (DoubleDouble) res.clone();
            e1abs = (e1.abs());
            delta2 = e2.subtract(e1);
            err2 = delta2.abs();
            tol2 = ( (e2.abs()).max(e1abs)).multiply(epmach);
            delta3 = e1.subtract(e0);
            err3 = delta3.abs();
            tol3 = (e1abs.max(e0.abs())).multiply(epmach);

            if ( (err2.le(tol2)) && (err3.le(tol3))) {

                // if e0, e1, and e2 are equal to within machine accuracy,
                // convergence is assumed.
                result[0] = (DoubleDouble) res.clone();
                abserr[0] = err2.add(err3);
                abserr[0] = abserr[0].max( ( (DoubleDouble.valueOf(5.0)).multiply(epmach)).multiply(result[0].abs()));

                return;
            } // if ((err2 <= tol2) && (err3 <= tol3))

            e3 = (DoubleDouble) epstab[k1 - 1].clone();
            epstab[k1 - 1] = (DoubleDouble) e1.clone();
            delta1 = e1.subtract(e3);
            err1 = delta1.abs();
            tol1 = (e1abs.max(e3.abs())).multiply(epmach);

            // If two elements are very close to each other, omit a part of
            // the table by adjusting the value of n

            if ( (err1.gt(tol1)) && (err2.gt(tol2)) && (err3.gt(tol3))) {
                ss = ( (delta1.reciprocal()).add(delta2.reciprocal())).subtract(delta2.reciprocal());
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
            epstab[k1 - 1] = (DoubleDouble) res.clone();
            k1 = k1 - 2;
            error = (err2.add( (res.subtract(e2)).abs())).add(err3);

            if (error.gt(abserr[0])) {
                continue;
            } // if (error > abserr[0])

            abserr[0] = (DoubleDouble) error.clone();
            result[0] = (DoubleDouble) res.clone();
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
            epstab[ib - 1] = (DoubleDouble) epstab[ib2 - 1].clone();
            ib = ib2;
        } // for (i = 1; i <= ie; i++)

        if (num != n[0]) {
            indx = num - n[0];

            for (i = 0; i < n[0]; i++) {
                epstab[i] = (DoubleDouble) epstab[indx].clone();
                indx = indx + 1;
            } // for (i = 0; i < n[0]; i++)
        } // if (num != n[0])

        if (nres[0] < 4) {
            res31a[nres[0] - 1] = (DoubleDouble) result[0].clone();
            abserr[0] = (DoubleDouble) oflow.clone();
            abserr[0] = abserr[0].max( ( (DoubleDouble.valueOf(5.0)).multiply(epmach)).multiply(result[0].abs()));

            return;
        } // if (nres[0] < 4)

        // Compute error estimate

        abserr[0] = ( ( (result[0].subtract(res31a[2])).abs()).add( (result[0].subtract(res31a[1])).abs()))
                .add( (result[0].subtract(res31a[0])).abs());
        res31a[0] = (DoubleDouble) res31a[1].clone();
        res31a[1] = (DoubleDouble) res31a[2].clone();
        res31a[2] = (DoubleDouble) result[0].clone();
        abserr[0] = abserr[0].max( ( (DoubleDouble.valueOf(5.0)).multiply(epmach)).multiply(result[0].abs()));

        return;
    }

    /**
     * The is a port of the original FORTRAN whose header is given below: c***begin prologue dqk15 c***date written
     * 800101 (yymmdd) c***revision date 830518 (yymmdd) c***category no. h2a1a2 c***keywords 15-point gauss-kronrod
     * rules c***author piessens,robert,appl. math. & progr. div. - k.u.leuven c de doncker,elise,appl. math. & progr.
     * div - k.u.leuven c***purpose to compute i = integral of f over (a,b), with error c estimate c j = integral of
     * abs(f) over (a,b) c***description c c integration rules c standard fortran subroutine c DoubleDouble precision
     * version
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
    private void dqk15(final DoubleDouble a, final DoubleDouble b, final DoubleDouble[] result,
            final DoubleDouble[] abserr, final DoubleDouble[] resabs, final DoubleDouble[] resasc) {
        // The abscissae and weights are supplied for the interval (-1, 1).
        // Because of symmetry only the positive abscissae and their
        // corresponding weights are given.

        // xgk - abscissae of the 15-point kronrod rule
        // xgk[1], xgk[3], ... abscissae of the 7-point gauss rule
        // xgk[0], xgk[2], ... abscissae which are optimally added to the
        // 7-point gauss rule.
        final DoubleDouble[] xgk = new DoubleDouble[] {DoubleDouble.valueOf(0.991455371120812639206854697526329),
                DoubleDouble.valueOf(0.949107912342758524526189684047851),
                DoubleDouble.valueOf(0.864864423359769072789712788640926),
                DoubleDouble.valueOf(0.741531185599394439863864773280788),
                DoubleDouble.valueOf(0.586087235467691130294144838258730),
                DoubleDouble.valueOf(0.405845151377397166906606412076961),
                DoubleDouble.valueOf(0.207784955007898467600689403773245),
                DoubleDouble.valueOf(0.000000000000000000000000000000000)};

        // wgk - weights of the 15-point kronrod rule
        final DoubleDouble[] wgk = new DoubleDouble[] {DoubleDouble.valueOf(0.022935322010529224963732008058970),
                DoubleDouble.valueOf(0.063092092629978553290700663189204),
                DoubleDouble.valueOf(0.104790010322250183839876322541518),
                DoubleDouble.valueOf(0.140653259715525918745189590510238),
                DoubleDouble.valueOf(0.169004726639267902826583426598550),
                DoubleDouble.valueOf(0.190350578064785409913256402421014),
                DoubleDouble.valueOf(0.204432940075298892414161999234649),
                DoubleDouble.valueOf(0.209482141084727828012999174891714)};

        // wg - weights of the 7-point gauss rule, corresponding to the
        // abscissae xgk[1], xgk[3], ...
        // wg[0], wg[2], ... are set to zero.
        final DoubleDouble[] wg = new DoubleDouble[] {DoubleDouble.valueOf(0.0),
                DoubleDouble.valueOf(0.129484966168869693270611432679082), DoubleDouble.valueOf(0.0),
                DoubleDouble.valueOf(0.279705391489276667901467771423780), DoubleDouble.valueOf(0.0),
                DoubleDouble.valueOf(0.381830050505118944950369775488975), DoubleDouble.valueOf(0.0),
                DoubleDouble.valueOf(0.417959183673469387755102040816327)};

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
        final DoubleDouble[] fv1 = new DoubleDouble[7];
        final DoubleDouble[] fv2 = new DoubleDouble[7];

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
        } else {
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
            } else {
                fval1 = intFunc(centr.subtract(absc));
                fval2 = intFunc(centr.add(absc));
            }
            fv1[jtw] = (DoubleDouble) fval1.clone();
            fv2[jtw] = (DoubleDouble) fval2.clone();
            fsum = fval1.add(fval2);
            resg = resg.add(wg[j].multiply(fsum));
            resk = resk.add(wgk[jtw].multiply(fsum));
            resabs[0] = resabs[0].add(wgk[jtw].multiply( (fval1.abs()).add(fval2.abs())));
        } // for (j = 0; j < 3; j++)

        for (j = 0; j < 4; j++) {
            jtwm1 = 2 * j;
            absc = hlgth.multiply(xgk[jtwm1]);
            if (selfTest) {
                fval1 = intFuncTest(centr.subtract(absc));
                fval2 = intFuncTest(centr.add(absc));
            } else {
                fval1 = intFunc(centr.subtract(absc));
                fval2 = intFunc(centr.add(absc));
            }
            fv1[jtwm1] = (DoubleDouble) fval1.clone();
            fv2[jtwm1] = (DoubleDouble) fval2.clone();
            fsum = fval1.add(fval2);
            resk = resk.add(wgk[jtwm1].multiply(fsum));
            resabs[0] = resabs[0].add(wgk[jtwm1].multiply( (fval1.abs()).add(fval2.abs())));
        } // for (j = 0; j < 4; j++)

        reskh = (DoubleDouble.valueOf(0.5)).multiply(resk);
        resasc[0] = wgk[7].multiply( (fc.subtract(reskh)).abs());

        for (j = 0; j < 7; j++) {
            resasc[0] = resasc[0].add(wgk[j].multiply( ( (fv1[j].subtract(reskh)).abs()).add( (fv2[j].subtract(reskh))
                    .abs())));
        } // for (j = 0; j < 7; j++)

        result[0] = resk.multiply(hlgth);
        resabs[0] = resabs[0].multiply(dhlgth);
        resasc[0] = resasc[0].multiply(dhlgth);
        abserr[0] = ( (resk.subtract(resg)).multiply(hlgth)).abs();

        if ( (resasc[0].ne(DoubleDouble.valueOf(0.0))) && (abserr[0].ne(DoubleDouble.valueOf(0.0)))) {
            final DoubleDouble base = ( (DoubleDouble.valueOf(200.0)).multiply(abserr[0])).divide(resasc[0]);
            abserr[0] = resasc[0].multiply( (DoubleDouble.valueOf(1.0)).min(base.pow(1.5)));
        }

        if (resabs[0].gt(uflow.divide( (DoubleDouble.valueOf(50.0)).multiply(epmach)))) {
            abserr[0] = ( ( (DoubleDouble.valueOf(50.0)).multiply(epmach)).multiply(resabs[0])).max(abserr[0]);
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
     * integration rule c standard fortran subroutine c DoubleDouble precision version
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
    private void dqk15i(final DoubleDouble boun, final int inf, final DoubleDouble a, final DoubleDouble b,
            final DoubleDouble[] result, final DoubleDouble[] abserr, final DoubleDouble[] resabs,
            final DoubleDouble[] resasc) {
        // The abscissae and weights are supplied for the interval (-1, 1).
        // Because of symmetry only the positive abscissae and their
        // corresponding weights are given.

        // xgk - abscissae of the 15-point kronrod rule
        // xgk[1], xgk[3], ... abscissae of the 7-point gauss rule
        // xgk[0], xgk[2], ... abscissae which are optimally added to the
        // 7-point gauss rule.
        final DoubleDouble[] xgk = new DoubleDouble[] {DoubleDouble.valueOf(0.991455371120812639206854697526329),
                DoubleDouble.valueOf(0.949107912342758524526189684047851),
                DoubleDouble.valueOf(0.864864423359769072789712788640926),
                DoubleDouble.valueOf(0.741531185599394439863864773280788),
                DoubleDouble.valueOf(0.586087235467691130294144838258730),
                DoubleDouble.valueOf(0.405845151377397166906606412076961),
                DoubleDouble.valueOf(0.207784955007898467600689403773245),
                DoubleDouble.valueOf(0.000000000000000000000000000000000)};

        // wgk - weights of the 15-point kronrod rule
        final DoubleDouble[] wgk = new DoubleDouble[] {DoubleDouble.valueOf(0.022935322010529224963732008058970),
                DoubleDouble.valueOf(0.063092092629978553290700663189204),
                DoubleDouble.valueOf(0.104790010322250183839876322541518),
                DoubleDouble.valueOf(0.140653259715525918745189590510238),
                DoubleDouble.valueOf(0.169004726639267902826583426598550),
                DoubleDouble.valueOf(0.190350578064785409913256402421014),
                DoubleDouble.valueOf(0.204432940075298892414161999234649),
                DoubleDouble.valueOf(0.209482141084727828012999174891714)};

        // wg - weights of the 7-point gauss rule, corresponding to the
        // abscissae xgk[1], xgk[3], ...
        // wg[0], wg[2], ... are set to zero.
        final DoubleDouble[] wg = new DoubleDouble[] {DoubleDouble.valueOf(0.0),
                DoubleDouble.valueOf(0.129484966168869693270611432679082), DoubleDouble.valueOf(0.0),
                DoubleDouble.valueOf(0.279705391489276667901467771423780), DoubleDouble.valueOf(0.0),
                DoubleDouble.valueOf(0.381830050505118944950369775488975), DoubleDouble.valueOf(0.0),
                DoubleDouble.valueOf(0.417959183673469387755102040816327)};

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
        final DoubleDouble[] fv1 = new DoubleDouble[7];
        final DoubleDouble[] fv2 = new DoubleDouble[7];

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

        dinf = (DoubleDouble.valueOf(1.0)).min(DoubleDouble.valueOf(inf));

        centr = (DoubleDouble.valueOf(0.5)).multiply(a.add(b));
        hlgth = (DoubleDouble.valueOf(0.5)).multiply(b.subtract(a));
        tabsc1 = boun.add( (dinf.multiply( (DoubleDouble.valueOf(1.0)).subtract(centr))).divide(centr));
        if (selfTest) {
            fval1 = intFuncTest(tabsc1);
        } else {
            fval1 = intFunc(tabsc1);
        }

        if (inf == 2) {
            if (selfTest) {
                fval1 = fval1.add(intFuncTest(tabsc1.negate()));
            } else {
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
            tabsc1 = boun.add( (dinf.multiply( (DoubleDouble.valueOf(1.0)).subtract(absc1))).divide(absc1));
            tabsc2 = boun.add( (dinf.multiply( (DoubleDouble.valueOf(1.0)).subtract(absc2))).divide(absc2));
            if (selfTest) {
                fval1 = intFuncTest(tabsc1);
                fval2 = intFuncTest(tabsc2);
            } else {
                fval1 = intFunc(tabsc1);
                fval2 = intFunc(tabsc2);
            }

            if (inf == 2) {
                if (selfTest) {
                    fval1 = fval1.add(intFuncTest(tabsc1.negate()));
                    fval2 = fval2.add(intFuncTest(tabsc2.negate()));
                } else {
                    fval1 = fval1.add(intFunc(tabsc1.negate()));
                    fval2 = fval2.add(intFunc(tabsc2.negate()));
                }
            } // if (inf == 2)

            fval1 = (fval1.divide(absc1)).divide(absc1);
            fval2 = (fval2.divide(absc2)).divide(absc2);
            fv1[j] = (DoubleDouble) fval1.clone();
            fv2[j] = (DoubleDouble) fval2.clone();
            fsum = fval1.add(fval2);
            resg = resg.add(wg[j].multiply(fsum));
            resk = resk.add(wgk[j].multiply(fsum));
            resabs[0] = resabs[0].add(wgk[j].multiply( (fval1.abs()).add(fval2.abs())));
        } // for (j = 0; j < 7; j++)

        reskh = (DoubleDouble.valueOf(0.5)).multiply(resk);
        resasc[0] = wgk[7].multiply( (fc.subtract(reskh)).abs());

        for (j = 0; j < 7; j++) {
            resasc[0] = resasc[0].add(wgk[j].multiply( ( (fv1[j].subtract(reskh)).abs()).add( (fv2[j].subtract(reskh))
                    .abs())));
        } // for (j = 0; j < 7; j++)

        result[0] = resk.multiply(hlgth);
        resasc[0] = resasc[0].multiply(hlgth);
        resabs[0] = resabs[0].multiply(hlgth);
        abserr[0] = ( (resk.subtract(resg)).multiply(hlgth)).abs();

        if ( (resasc[0].ne(DoubleDouble.valueOf(0.0))) && (abserr[0].ne(DoubleDouble.valueOf(0.0)))) {
            final DoubleDouble base = ( (DoubleDouble.valueOf(200.0)).multiply(abserr[0])).divide(resasc[0]);
            abserr[0] = resasc[0].multiply( ( (DoubleDouble.valueOf(1.0)).min( (base.pow(1.5)))));
        } // if ((resasc[0] != 0.0) && (abserr[0] != 0.0))

        if (resabs[0].gt(uflow.divide( (DoubleDouble.valueOf(50.0)).multiply(epmach)))) {
            abserr[0] = ( ( (DoubleDouble.valueOf(50.0)).multiply(epmach)).multiply(resabs[0])).max(abserr[0]);
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
    private void dqk15w(final DoubleDouble p1, final DoubleDouble a, final DoubleDouble b, final DoubleDouble[] result,
            final DoubleDouble[] abserr, final DoubleDouble[] resabs, final DoubleDouble[] resasc) {
        /**
         * The abscissa and weights are given for the interval (-1,1). Because of symmetry only the positive abscissae
         * and their corresponding weights are given.
         */
        /** abscissa */
        DoubleDouble absc;
        DoubleDouble absc1;
        DoubleDouble absc2;
        /** mid point of the inverval */
        DoubleDouble centr;
        DoubleDouble dhlgth;
        DoubleDouble fc;
        DoubleDouble fsum;
        /** function value */
        DoubleDouble fval1;
        DoubleDouble fval2;
        final DoubleDouble fv1[] = new DoubleDouble[7];
        final DoubleDouble fv2[] = new DoubleDouble[7];
        /** half_length of the interval */
        DoubleDouble hlgth;
        /** result of the 7-point gauss formula */
        DoubleDouble resg;
        /** result of the 15-point kronrod formula */
        DoubleDouble resk;
        /**
         * approximation to the mean value of f*w over (a,b), i.e. to i/(b-a)
         */
        DoubleDouble reskh;
        /** Weights of the 7-point gauss rule */
        final DoubleDouble wg[] = new DoubleDouble[] {DoubleDouble.valueOf(0.1294849661688697),
                DoubleDouble.valueOf(0.2797053914892767), DoubleDouble.valueOf(0.3818300505051889),
                DoubleDouble.valueOf(0.4179591836734694)};
        /** Weights of the 15-point gauss-kronrod rule */
        final DoubleDouble wgk[] = new DoubleDouble[] {DoubleDouble.valueOf(0.02293532201052922),
                DoubleDouble.valueOf(0.06309209262997855), DoubleDouble.valueOf(0.1047900103222502),
                DoubleDouble.valueOf(0.1406532597155259), DoubleDouble.valueOf(0.1690047266392679),
                DoubleDouble.valueOf(0.1903505780647854), DoubleDouble.valueOf(0.2044329400752989),
                DoubleDouble.valueOf(0.2094821410847278)};
        /**
         * abscissae of the 15-point gauss-kronrod rule xgk[1], xgk[3], ... abscissae of the 7-point gauss rule. xgk[0],
         * xgk[2], ... abscissae which are optimally added to the 7-point gauss rule.
         */
        final DoubleDouble xgk[] = new DoubleDouble[] {DoubleDouble.valueOf(0.9914553711208126),
                DoubleDouble.valueOf(0.9491079123427585), DoubleDouble.valueOf(0.8648644233597691),
                DoubleDouble.valueOf(0.7415311855993944), DoubleDouble.valueOf(0.5860872354676911),
                DoubleDouble.valueOf(0.4058451513773972), DoubleDouble.valueOf(0.2077849550078985),
                DoubleDouble.valueOf(0.0000000000000000)};
        DoubleDouble xma;
        DoubleDouble bmx;

        int j;
        int jtw;
        int jtwm1;

        centr = (DoubleDouble.valueOf(0.5)).multiply(a.add(b));
        hlgth = (DoubleDouble.valueOf(0.5)).multiply(b.subtract(a));
        dhlgth = hlgth.abs();

        // Compute the 15-point kronrod approximation to the
        // integral, and estimate the error.
        if (routine == Integration2EP.DQAWCE) {
            if (selfTest) {
                fc = intFuncTest(centr).divide(centr.subtract(p1));
            } else {
                fc = intFunc(centr).divide(centr.subtract(p1));
            }
        } // if (routine == DQAWCE)
        else if ( (routine == Integration2EP.DQAWFE) || (routine == Integration2EP.DQAWOE)) {
            if (integr == 1) {
                if (selfTest) {
                    fc = intFuncTest(centr).multiply( (centr.multiply(p1)).cos());
                } else {
                    fc = intFunc(centr).multiply( (centr.multiply(p1)).cos());
                }
            } // if (integr == 1)
            else { // integr == 2
                if (selfTest) {
                    fc = intFuncTest(centr).multiply( (centr.multiply(p1)).sin());
                } else {
                    fc = intFunc(centr).multiply( (centr.multiply(p1)).sin());
                }
            } // else integr == 2
        } // else if ((routine == DQAWFE) || (routine == DQAWOE))
        else { // routine == DQAWSE
            if (integr == 1) {
                if (selfTest) {
                    xma = centr.subtract(lower);
                    bmx = upper.subtract(centr);
                    fc = (intFuncTest(centr).multiply(xma.pow(alfa))).multiply(bmx.pow(beta));
                } else {
                    xma = centr.subtract(lower);
                    bmx = upper.subtract(centr);
                    fc = (intFunc(centr).multiply(xma.pow(alfa))).multiply(bmx.pow(beta));
                }
            } // if (integr == 1)
            else if (integr == 2) {
                if (selfTest) {
                    xma = centr.subtract(lower);
                    bmx = upper.subtract(centr);
                    fc = ( (intFuncTest(centr).multiply(xma.pow(alfa))).multiply(bmx.pow(beta))).multiply(xma.log());
                } else {
                    xma = centr.subtract(lower);
                    bmx = upper.subtract(centr);
                    fc = ( (intFunc(centr).multiply(xma.pow(alfa))).multiply(bmx.pow(beta))).multiply(xma.log());
                }
            } // else if (integr == 2)
            else if (integr == 3) {
                if (selfTest) {
                    xma = centr.subtract(lower);
                    bmx = upper.subtract(centr);
                    fc = ( (intFuncTest(centr).multiply(xma.pow(alfa))).multiply(bmx.pow(beta))).multiply(bmx.log());
                } else {
                    xma = centr.subtract(lower);
                    bmx = upper.subtract(centr);
                    fc = ( (intFunc(centr).multiply(xma.pow(alfa))).multiply(bmx.pow(beta))).multiply(bmx.log());
                }
            } // else if (integr == 3)
            else { // integr == 4
                if (selfTest) {
                    xma = centr.subtract(lower);
                    bmx = upper.subtract(centr);
                    fc = ( ( (intFuncTest(centr).multiply(xma.pow(alfa))).multiply(bmx.pow(beta))).multiply(xma.log()))
                            .multiply(bmx.log());
                } else {
                    xma = centr.subtract(lower);
                    bmx = upper.subtract(centr);
                    fc = ( ( (intFunc(centr).multiply(xma.pow(alfa))).multiply(bmx.pow(beta))).multiply(xma.log()))
                            .multiply(bmx.log());
                }
            } // integr == 4
        } // else routine == DQAWSE
        resg = wg[3].multiply(fc);
        resk = wgk[7].multiply(fc);
        resabs[0] = resk.abs();
        for (j = 1; j <= 3; j++) {
            jtw = 2 * j;
            absc = hlgth.multiply(xgk[jtw - 1]);
            absc1 = centr.subtract(absc);
            absc2 = centr.add(absc);
            if (routine == Integration2EP.DQAWCE) {
                if (selfTest) {
                    fval1 = intFuncTest(absc1).divide(absc1.subtract(p1));
                    fval2 = intFuncTest(absc2).divide(absc2.subtract(p1));
                } else {
                    fval1 = intFunc(absc1).divide(absc1.subtract(p1));
                    fval2 = intFunc(absc2).divide(absc2.subtract(p1));
                }
            } // if (routine == DQAWCE)
            else if ( (routine == Integration2EP.DQAWFE) || (routine == Integration2EP.DQAWOE)) {
                if (integr == 1) {
                    if (selfTest) {
                        fval1 = intFuncTest(absc1).multiply( (absc1.multiply(p1)).cos());
                        fval2 = intFuncTest(absc2).multiply( (absc2.multiply(p1)).cos());
                    } else {
                        fval1 = intFunc(absc1).multiply( (absc1.multiply(p1)).cos());
                        fval2 = intFunc(absc2).multiply( (absc2.multiply(p1)).cos());
                    }
                } // if (integr == 1)
                else { // integr == 2
                    if (selfTest) {
                        fval1 = intFuncTest(absc1).multiply( (absc1.multiply(p1)).sin());
                        fval2 = intFuncTest(absc2).multiply( (absc2.multiply(p1)).sin());
                    } else {
                        fval1 = intFunc(absc1).multiply( (absc1.multiply(p1)).sin());
                        fval2 = intFunc(absc2).multiply( (absc2.multiply(p1)).sin());
                    }
                }
            } // else if ((routine == DQAWFE) || (routine == DQAWOE))
            else { // routine == DQAWSE
                if (integr == 1) {
                    if (selfTest) {
                        xma = absc1.subtract(lower);
                        bmx = upper.subtract(absc1);
                        fval1 = (intFuncTest(absc1).multiply(xma.pow(alfa))).multiply(bmx.pow(beta));
                        xma = absc2.subtract(lower);
                        bmx = upper.subtract(absc2);
                        fval2 = (intFuncTest(absc2).multiply(xma.pow(alfa))).multiply(bmx.pow(beta));
                    } else {
                        xma = absc1.subtract(lower);
                        bmx = upper.subtract(absc1);
                        fval1 = (intFunc(absc1).multiply(xma.pow(alfa))).multiply(bmx.pow(beta));
                        xma = absc2.subtract(lower);
                        bmx = upper.subtract(absc2);
                        fval2 = (intFunc(absc2).multiply(xma.pow(alfa))).multiply(bmx.pow(beta));
                    }
                } // if (integr == 1)
                else if (integr == 2) {
                    if (selfTest) {
                        xma = absc1.subtract(lower);
                        bmx = upper.subtract(absc1);
                        fval1 = ( (intFuncTest(absc1).multiply(xma.pow(alfa))).multiply(bmx.pow(beta))).multiply(xma
                                .log());
                        xma = absc2.subtract(lower);
                        bmx = upper.subtract(absc2);
                        fval2 = ( (intFuncTest(absc2).multiply(xma.pow(alfa))).multiply(bmx.pow(beta))).multiply(xma
                                .log());
                    } else {
                        xma = absc1.subtract(lower);
                        bmx = upper.subtract(absc1);
                        fval1 = ( (intFunc(absc1).multiply(xma.pow(alfa))).multiply(bmx.pow(beta))).multiply(xma.log());
                        xma = absc2.subtract(lower);
                        bmx = upper.subtract(absc2);
                        fval2 = ( (intFunc(absc2).multiply(xma.pow(alfa))).multiply(bmx.pow(beta))).multiply(xma.log());
                    }
                } // else if (integr == 2)
                else if (integr == 3) {
                    if (selfTest) {
                        xma = absc1.subtract(lower);
                        bmx = upper.subtract(absc1);
                        fval1 = ( (intFuncTest(absc1).multiply(xma.pow(alfa))).multiply(bmx.pow(beta))).multiply(bmx
                                .log());
                        xma = absc2.subtract(lower);
                        bmx = upper.subtract(absc2);
                        fval2 = ( (intFuncTest(absc2).multiply(xma.pow(alfa))).multiply(bmx.pow(beta))).multiply(bmx
                                .log());
                    } else {
                        xma = absc1.subtract(lower);
                        bmx = upper.subtract(absc1);
                        fval1 = ( (intFunc(absc1).multiply(xma.pow(alfa))).multiply(bmx.pow(beta))).multiply(bmx.log());
                        xma = absc2.subtract(lower);
                        bmx = upper.subtract(absc2);
                        fval2 = ( (intFunc(absc2).multiply(xma.pow(alfa))).multiply(bmx.pow(beta))).multiply(bmx.log());
                    }
                } // else if (integr == 3)
                else { // integr == 4
                    if (selfTest) {
                        xma = absc1.subtract(lower);
                        bmx = upper.subtract(absc1);
                        fval1 = ( ( (intFuncTest(absc1).multiply(xma.pow(alfa))).multiply(bmx.pow(beta))).multiply(xma
                                .log())).multiply(bmx.log());
                        xma = absc2.subtract(lower);
                        bmx = upper.subtract(absc2);
                        fval2 = ( ( (intFuncTest(absc2).multiply(xma.pow(alfa))).multiply(bmx.pow(beta))).multiply(xma
                                .log())).multiply(bmx.log());
                    } else {
                        xma = absc1.subtract(lower);
                        bmx = upper.subtract(absc1);
                        fval1 = ( ( (intFunc(absc1).multiply(xma.pow(alfa))).multiply(bmx.pow(beta))).multiply(xma
                                .log())).multiply(bmx.log());
                        xma = absc2.subtract(lower);
                        bmx = upper.subtract(absc2);
                        fval2 = ( ( (intFunc(absc2).multiply(xma.pow(alfa))).multiply(bmx.pow(beta))).multiply(xma
                                .log())).multiply(bmx.log());
                    }
                } // integr == 4
            } // else routine == DQAWSE
            fv1[jtw - 1] = (DoubleDouble) fval1.clone();
            fv2[jtw - 1] = (DoubleDouble) fval2.clone();
            fsum = fval1.add(fval2);
            resg = resg.add(wg[j - 1].multiply(fsum));
            resk = resk.add(wgk[jtw - 1].multiply(fsum));
            resabs[0] = resabs[0].add(wgk[jtw - 1].multiply( (fval1.abs()).add(fval2.abs())));
        } // for (j = 1; j <= 3; j++)
        for (j = 1; j <= 4; j++) {
            jtwm1 = 2 * j - 1;
            absc = hlgth.multiply(xgk[jtwm1 - 1]);
            absc1 = centr.subtract(absc);
            absc2 = centr.add(absc);
            if (routine == Integration2EP.DQAWCE) {
                if (selfTest) {
                    fval1 = intFuncTest(absc1).divide(absc1.subtract(p1));
                    fval2 = intFuncTest(absc2).divide(absc2.subtract(p1));
                } else {
                    fval1 = intFunc(absc1).divide(absc1.subtract(p1));
                    fval2 = intFunc(absc2).divide(absc2.subtract(p1));
                }
            } // if (routine == DQAWCE)
            else if ( (routine == Integration2EP.DQAWFE) || (routine == Integration2EP.DQAWOE)) {
                if (integr == 1) {
                    if (selfTest) {
                        fval1 = intFuncTest(absc1).multiply( (absc1.multiply(p1)).cos());
                        fval2 = intFuncTest(absc2).multiply( (absc2.multiply(p1)).cos());
                    } else {
                        fval1 = intFunc(absc1).multiply( (absc1.multiply(p1)).cos());
                        fval2 = intFunc(absc2).multiply( (absc2.multiply(p1)).cos());
                    }
                } // if (integr == 1)
                else { // integr == 2
                    if (selfTest) {
                        fval1 = intFuncTest(absc1).multiply( (absc1.multiply(p1)).sin());
                        fval2 = intFuncTest(absc2).multiply( (absc2.multiply(p1)).sin());
                    } else {
                        fval1 = intFunc(absc1).multiply( (absc1.multiply(p1)).sin());
                        fval2 = intFunc(absc2).multiply( (absc2.multiply(p1)).sin());
                    }
                }
            } // else if ((routine == DQAWFE) || (routine == DQAWOE))
            else { // routine == DQAWSE
                if (integr == 1) {
                    if (selfTest) {
                        xma = absc1.subtract(lower);
                        bmx = upper.subtract(absc1);
                        fval1 = (intFuncTest(absc1).multiply(xma.pow(alfa))).multiply(bmx.pow(beta));
                        xma = absc2.subtract(lower);
                        bmx = upper.subtract(absc2);
                        fval2 = (intFuncTest(absc2).multiply(xma.pow(alfa))).multiply(bmx.pow(beta));
                    } else {
                        xma = absc1.subtract(lower);
                        bmx = upper.subtract(absc1);
                        fval1 = (intFunc(absc1).multiply(xma.pow(alfa))).multiply(bmx.pow(beta));
                        xma = absc2.subtract(lower);
                        bmx = upper.subtract(absc2);
                        fval2 = (intFunc(absc2).multiply(xma.pow(alfa))).multiply(bmx.pow(beta));
                    }
                } // if (integr == 1)
                else if (integr == 2) {
                    if (selfTest) {
                        xma = absc1.subtract(lower);
                        bmx = upper.subtract(absc1);
                        fval1 = ( (intFuncTest(absc1).multiply(xma.pow(alfa))).multiply(bmx.pow(beta))).multiply(xma
                                .log());
                        xma = absc2.subtract(lower);
                        bmx = upper.subtract(absc2);
                        fval2 = ( (intFuncTest(absc2).multiply(xma.pow(alfa))).multiply(bmx.pow(beta))).multiply(xma
                                .log());
                    } else {
                        xma = absc1.subtract(lower);
                        bmx = upper.subtract(absc1);
                        fval1 = ( (intFunc(absc1).multiply(xma.pow(alfa))).multiply(bmx.pow(beta))).multiply(xma.log());
                        xma = absc2.subtract(lower);
                        bmx = upper.subtract(absc2);
                        fval2 = ( (intFunc(absc2).multiply(xma.pow(alfa))).multiply(bmx.pow(beta))).multiply(xma.log());
                    }
                } // else if (integr == 2)
                else if (integr == 3) {
                    if (selfTest) {
                        xma = absc1.subtract(lower);
                        bmx = upper.subtract(absc1);
                        fval1 = ( (intFuncTest(absc1).multiply(xma.pow(alfa))).multiply(bmx.pow(beta))).multiply(bmx
                                .log());
                        xma = absc2.subtract(lower);
                        bmx = upper.subtract(absc2);
                        fval2 = ( (intFuncTest(absc2).multiply(xma.pow(alfa))).multiply(bmx.pow(beta))).multiply(bmx
                                .log());
                    } else {
                        xma = absc1.subtract(lower);
                        bmx = upper.subtract(absc1);
                        fval1 = ( (intFunc(absc1).multiply(xma.pow(alfa))).multiply(bmx.pow(beta))).multiply(bmx.log());
                        xma = absc2.subtract(lower);
                        bmx = upper.subtract(absc2);
                        fval2 = ( (intFunc(absc2).multiply(xma.pow(alfa))).multiply(bmx.pow(beta))).multiply(bmx.log());
                    }
                } // else if (integr == 3)
                else { // integr == 4
                    if (selfTest) {
                        xma = absc1.subtract(lower);
                        bmx = upper.subtract(absc1);
                        fval1 = ( ( (intFuncTest(absc1).multiply(xma.pow(alfa))).multiply(bmx.pow(beta))).multiply(xma
                                .log())).multiply(bmx.log());
                        xma = absc2.subtract(lower);
                        bmx = upper.subtract(absc2);
                        fval2 = ( ( (intFuncTest(absc2).multiply(xma.pow(alfa))).multiply(bmx.pow(beta))).multiply(xma
                                .log())).multiply(bmx.log());
                    } else {
                        xma = absc1.subtract(lower);
                        bmx = upper.subtract(absc1);
                        fval1 = ( ( (intFunc(absc1).multiply(xma.pow(alfa))).multiply(bmx.pow(beta))).multiply(xma
                                .log())).multiply(bmx.log());
                        xma = absc2.subtract(lower);
                        bmx = upper.subtract(absc2);
                        fval2 = ( ( (intFunc(absc2).multiply(xma.pow(alfa))).multiply(bmx.pow(beta))).multiply(xma
                                .log())).multiply(bmx.log());
                    }
                } // integr == 4
            } // else routine == DQAWSE
            fv1[jtwm1 - 1] = (DoubleDouble) fval1.clone();
            fv2[jtwm1 - 1] = (DoubleDouble) fval2.clone();
            fsum = fval1.add(fval2);
            resk = resk.add(wgk[jtwm1 - 1].multiply(fsum));
            resabs[0] = resabs[0].add(wgk[jtwm1 - 1].multiply( (fval1.abs()).add(fval2.abs())));
        } // for (j = 1; j <= 4; j++)
        reskh = (DoubleDouble.valueOf(0.5)).multiply(resk);
        resasc[0] = wgk[7].multiply( (fc.subtract(reskh)).abs());
        for (j = 0; j < 7; j++) {
            resasc[0] = resasc[0].add(wgk[j].multiply( ( (fv1[j].subtract(reskh)).abs()).add( (fv2[j].subtract(reskh))
                    .abs())));
        }
        result[0] = resk.multiply(hlgth);
        resabs[0] = resabs[0].multiply(dhlgth);
        resasc[0] = resasc[0].multiply(dhlgth);
        abserr[0] = ( (resk.subtract(resg)).multiply(hlgth)).abs();
        if ( (resasc[0].ne(DoubleDouble.valueOf(0.0))) && (abserr[0].ne(DoubleDouble.valueOf(0.0)))) {
            final DoubleDouble base = ( (DoubleDouble.valueOf(200.0)).multiply(abserr[0])).divide(resasc[0]);
            abserr[0] = resasc[0].multiply( (DoubleDouble.valueOf(1.0)).min(base.pow(1.5)));
        }
        if (resabs[0].gt(uflow.divide( (DoubleDouble.valueOf(50.0)).multiply(epmach)))) {
            abserr[0] = ( ( (DoubleDouble.valueOf(50.0)).multiply(epmach)).multiply(resabs[0])).max(abserr[0]);
        }
        return;
    } // dqk15w

    /**
     * This is a port of the original FORTRAN whose header is given below: c***begin prologue dqk21 c***date written
     * 800101 (yymmdd) c***revision date 830518 (yymmdd) c***category no. h2a1a2 c***keywords 21-point gauss-kronrod
     * rules c***author piessens,robert,appl. math. & progr. div. - k.u.leuven c de doncker,elise,appl. math. & progr.
     * div. - k.u.leuven c***purpose to compute i = integral of f over (a,b), with error c estimate c j = integral of
     * abs(f) over (a,b) c***description c c integration rules c standard fortran subroutine c DoubleDouble precision
     * version
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
    private void dqk21(final DoubleDouble a, final DoubleDouble b, final DoubleDouble[] result,
            final DoubleDouble[] abserr, final DoubleDouble[] resabs, final DoubleDouble[] resasc) {

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
        final DoubleDouble[] xgk = new DoubleDouble[] {DoubleDouble.valueOf(0.995657163025808080735527280689003),
                DoubleDouble.valueOf(0.973906528517171720077964012084452),
                DoubleDouble.valueOf(0.930157491355708226001207180059508),
                DoubleDouble.valueOf(0.865063366688984510732096688423493),
                DoubleDouble.valueOf(0.780817726586416897063717578345042),
                DoubleDouble.valueOf(0.679409568299024406234327365114874),
                DoubleDouble.valueOf(0.562757134668604683339000099272694),
                DoubleDouble.valueOf(0.433395394129247190799265943165784),
                DoubleDouble.valueOf(0.294392862701460198131126603103866),
                DoubleDouble.valueOf(0.148874338981631210884826001129720),
                DoubleDouble.valueOf(0.000000000000000000000000000000000)};

        // wgk - weights of the 21-point kronrod rule
        final DoubleDouble[] wgk = new DoubleDouble[] {DoubleDouble.valueOf(0.011694638867371874278064396062192),
                DoubleDouble.valueOf(0.032558162307964727478818972459390),
                DoubleDouble.valueOf(0.054755896574351996031381300244580),
                DoubleDouble.valueOf(0.075039674810919952767043140916190),
                DoubleDouble.valueOf(0.093125454583697605535065465083366),
                DoubleDouble.valueOf(0.109387158802297641899210590325805),
                DoubleDouble.valueOf(0.123491976262065851077958109831074),
                DoubleDouble.valueOf(0.134709217311473325928054001771707),
                DoubleDouble.valueOf(0.142775938577060080797094273138717),
                DoubleDouble.valueOf(0.147739104901338491374841515972068),
                DoubleDouble.valueOf(0.149445554002916905664936468389821)};

        // wg - weights of the 10-point gauss rule
        final DoubleDouble[] wg = new DoubleDouble[] {DoubleDouble.valueOf(0.066671344308688137593568809893332),
                DoubleDouble.valueOf(0.149451349150580593145776339657697),
                DoubleDouble.valueOf(0.219086362515982043995534934228163),
                DoubleDouble.valueOf(0.269266719309996355091226921569469),
                DoubleDouble.valueOf(0.295524224714752870173892994651338)};

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
        final DoubleDouble[] fv1 = new DoubleDouble[10];
        final DoubleDouble[] fv2 = new DoubleDouble[10];
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
        } else {
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
            } else {
                fval1 = intFunc(centr.subtract(absc));
                fval2 = intFunc(centr.add(absc));
            }
            fv1[jtw] = (DoubleDouble) fval1.clone();
            fv2[jtw] = (DoubleDouble) fval2.clone();
            fsum = fval1.add(fval2);
            resg = resg.add(wg[j].multiply(fsum));
            resk = resk.add(wgk[jtw].multiply(fsum));
            resabs[0] = resabs[0].add(wgk[jtw].multiply( (fval1.abs()).add(fval2.abs())));
        } // for (j = 0; j <= 4; j ++)

        for (j = 0; j <= 4; j++) {
            jtwm1 = 2 * j;
            absc = hlgth.multiply(xgk[jtwm1]);
            if (selfTest) {
                fval1 = intFuncTest(centr.subtract(absc));
                fval2 = intFuncTest(centr.add(absc));
            } else {
                fval1 = intFunc(centr.subtract(absc));
                fval2 = intFunc(centr.add(absc));
            }
            fv1[jtwm1] = (DoubleDouble) fval1.clone();
            fv2[jtwm1] = (DoubleDouble) fval2.clone();
            fsum = fval1.add(fval2);
            resk = resk.add(wgk[jtwm1].multiply(fsum));
            resabs[0] = resabs[0].add(wgk[jtwm1].multiply( (fval1.abs()).add(fval2.abs())));
        } // for (j = 0; j <= 4; j ++)

        reskh = (DoubleDouble.valueOf(0.5)).multiply(resk);
        resasc[0] = wgk[10].multiply( (fc.subtract(reskh)).abs());

        for (j = 0; j <= 9; j++) {
            resasc[0] = resasc[0].add(wgk[j].multiply( ( (fv1[j].subtract(reskh)).abs()).add( (fv2[j].subtract(reskh))
                    .abs())));
        } // for (j = 0; j <= 9; j++)

        result[0] = resk.multiply(hlgth);
        resabs[0] = resabs[0].multiply(dhlgth);
        resasc[0] = resasc[0].multiply(dhlgth);
        abserr[0] = ( (resk.subtract(resg)).multiply(hlgth)).abs();

        if ( (resasc[0].ne(DoubleDouble.valueOf(0.0))) && (abserr[0].ne(DoubleDouble.valueOf(0.0)))) {
            final DoubleDouble base = ( (DoubleDouble.valueOf(200.0)).multiply(abserr[0])).divide(resasc[0]);
            abserr[0] = resasc[0].multiply( (DoubleDouble.valueOf(1.0)).min(base.pow(DoubleDouble.valueOf(1.5))));
        }

        if (resabs[0].gt(uflow.divide( (DoubleDouble.valueOf(50.0)).multiply(epmach)))) {
            abserr[0] = ( ( (DoubleDouble.valueOf(50.0)).multiply(epmach)).multiply(resabs[0])).max(abserr[0]);
        }

        return;
    }

    /**
     * This is a port of the original FORTRAN whose header is given below: c***begin prologue dqk31 c***date written
     * 800101 (yymmdd) c***revision date 830518 (yymmdd) c***category no. h2a1a2 c***keywords 31-point gauss-kronrod
     * rules c***author piessens,robert,appl. math. & progr. div. - k.u.leuven c de doncker,elise,appl. math. & progr.
     * div. - k.u.leuven c***purpose to compute i = integral of f over (a,b) with error c estimate c j = integral of
     * abs(f) over (a,b) c***description c c integration rules c standard fortran subroutine c DoubleDouble precision
     * version
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
    private void dqk31(final DoubleDouble a, final DoubleDouble b, final DoubleDouble[] result,
            final DoubleDouble[] abserr, final DoubleDouble[] resabs, final DoubleDouble[] resasc) {
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
        final DoubleDouble[] xgk = new DoubleDouble[] {DoubleDouble.valueOf(0.998002298693397060285172840152271),
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
                DoubleDouble.valueOf(0.000000000000000000000000000000000)};

        // wgk - weights of the 31-point kronrod rule
        final DoubleDouble[] wgk = new DoubleDouble[] {DoubleDouble.valueOf(0.005377479872923348987792051430128),
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
                DoubleDouble.valueOf(0.101330007014791549017374792767493)};

        // wg - weights of the 15-point gauss rule
        final DoubleDouble[] wg = new DoubleDouble[] {DoubleDouble.valueOf(0.030753241996117268354628393577204),
                DoubleDouble.valueOf(0.070366047488108124709267416450667),
                DoubleDouble.valueOf(0.107159220467171935011869546685869),
                DoubleDouble.valueOf(0.139570677926154314447804794511028),
                DoubleDouble.valueOf(0.166269205816993933553200860481209),
                DoubleDouble.valueOf(0.186161000015562211026800561866423),
                DoubleDouble.valueOf(0.198431485327111576456118326443839),
                DoubleDouble.valueOf(0.202578241925561272880620199967519)};

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
        final DoubleDouble[] fv1 = new DoubleDouble[15];
        final DoubleDouble[] fv2 = new DoubleDouble[15];

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
        } else {
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
            } else {
                fval1 = intFunc(centr.subtract(absc));
                fval2 = intFunc(centr.add(absc));
            }
            fv1[jtw] = (DoubleDouble) fval1.clone();
            fv2[jtw] = (DoubleDouble) fval2.clone();
            fsum = fval1.add(fval2);
            resg = resg.add(wg[j].multiply(fsum));
            resk = resk.add(wgk[jtw].multiply(fsum));
            resabs[0] = resabs[0].add(wgk[jtw].multiply( (fval1.abs()).add(fval2.abs())));
        } // for (j = 0; j < 7; j++)

        for (j = 0; j < 8; j++) {
            jtwm1 = 2 * j;
            absc = hlgth.multiply(xgk[jtwm1]);
            if (selfTest) {
                fval1 = intFuncTest(centr.subtract(absc));
                fval2 = intFuncTest(centr.add(absc));
            } else {
                fval1 = intFunc(centr.subtract(absc));
                fval2 = intFunc(centr.add(absc));
            }
            fv1[jtwm1] = (DoubleDouble) fval1.clone();
            fv2[jtwm1] = (DoubleDouble) fval2.clone();
            fsum = fval1.add(fval2);
            resk = resk.add(wgk[jtwm1].multiply(fsum));
            resabs[0] = resabs[0].add(wgk[jtwm1].multiply( (fval1.abs()).add(fval2.abs())));
        } // for (j = 0; j < 8; j++)

        reskh = (DoubleDouble.valueOf(0.5)).multiply(resk);
        resasc[0] = wgk[15].multiply( (fc.subtract(reskh)).abs());

        for (j = 0; j < 15; j++) {
            resasc[0] = resasc[0].add(wgk[j].multiply( ( (fv1[j].subtract(reskh)).abs()).add( (fv2[j].subtract(reskh))
                    .abs())));
        } // for (j = 0; j < 15; j++)

        result[0] = resk.multiply(hlgth);
        resabs[0] = resabs[0].multiply(dhlgth);
        resasc[0] = resasc[0].multiply(dhlgth);
        abserr[0] = ( (resk.subtract(resg)).multiply(hlgth)).abs();

        if ( (resasc[0].ne(DoubleDouble.valueOf(0.0))) && (abserr[0].ne(DoubleDouble.valueOf(0.0)))) {
            final DoubleDouble base = ( (DoubleDouble.valueOf(200.0)).multiply(abserr[0])).divide(resasc[0]);
            abserr[0] = resasc[0].multiply( (DoubleDouble.valueOf(1.0)).min(base.pow(1.5)));
        }

        if (resabs[0].gt(uflow.divide( (DoubleDouble.valueOf(50.0)).multiply(epmach)))) {
            abserr[0] = ( ( (DoubleDouble.valueOf(50.0)).multiply(epmach)).multiply(resabs[0])).max(abserr[0]);
        }

        return;
    }

    /**
     * This is a port of the original FORTRAN routine whose header is given below: c***begin prologue dqk41 c***date
     * written 800101 (yymmdd) c***revision date 830518 (yymmdd) c***category no. h2a1a2 c***keywords 41-point
     * gauss-kronrod rules c***author piessens,robert,appl. math. & progr. div. - k.u.leuven c de doncker,elise,appl.
     * math. & progr. div. - k.u.leuven c***purpose to compute i = integral of f over (a,b), with error c estimate c j =
     * integral of abs(f) over (a,b) c***description c c integration rules c standard fortran subroutine c DoubleDouble
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
    private void dqk41(final DoubleDouble a, final DoubleDouble b, final DoubleDouble[] result,
            final DoubleDouble[] abserr, final DoubleDouble[] resabs, final DoubleDouble[] resasc) {
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
        final DoubleDouble[] xgk = new DoubleDouble[] {DoubleDouble.valueOf(0.998859031588277663838315576545863),
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
                DoubleDouble.valueOf(0.000000000000000000000000000000000)};

        // wgk - weights of the 41-point kronrod rule
        final DoubleDouble[] wgk = new DoubleDouble[] {DoubleDouble.valueOf(0.003073583718520531501218293246031),
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
                DoubleDouble.valueOf(0.076600711917999656445049901530102)};

        // wg - weights of the 20-point gauss rule
        final DoubleDouble[] wg = new DoubleDouble[] {DoubleDouble.valueOf(0.017614007139152118311861962351853),
                DoubleDouble.valueOf(0.040601429800386941331039952274932),
                DoubleDouble.valueOf(0.062672048334109063569506535187042),
                DoubleDouble.valueOf(0.083276741576704748724758143222046),
                DoubleDouble.valueOf(0.101930119817240435036750135480350),
                DoubleDouble.valueOf(0.118194531961518417312377377711382),
                DoubleDouble.valueOf(0.131688638449176626898494499748163),
                DoubleDouble.valueOf(0.142096109318382051329298325067165),
                DoubleDouble.valueOf(0.149172986472603746787828737001969),
                DoubleDouble.valueOf(0.152753387130725850698084331955098)};

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
        final DoubleDouble[] fv1 = new DoubleDouble[20];
        final DoubleDouble[] fv2 = new DoubleDouble[20];

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
        } else {
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
            } else {
                fval1 = intFunc(centr.subtract(absc));
                fval2 = intFunc(centr.add(absc));
            }
            fv1[jtw] = (DoubleDouble) fval1.clone();
            fv2[jtw] = (DoubleDouble) fval2.clone();
            fsum = fval1.add(fval2);
            resg = resg.add(wg[j].multiply(fsum));
            resk = resk.add(wgk[jtw].multiply(fsum));
            resabs[0] = resabs[0].add(wgk[jtw].multiply( (fval1.abs()).add(fval2.abs())));
        } // for (j = 0; j < 10; j++)

        for (j = 0; j < 10; j++) {
            jtwm1 = 2 * j;
            absc = hlgth.multiply(xgk[jtwm1]);
            if (selfTest) {
                fval1 = intFuncTest(centr.subtract(absc));
                fval2 = intFuncTest(centr.add(absc));
            } else {
                fval1 = intFunc(centr.subtract(absc));
                fval2 = intFunc(centr.add(absc));
            }
            fv1[jtwm1] = (DoubleDouble) fval1.clone();
            fv2[jtwm1] = (DoubleDouble) fval2.clone();
            fsum = fval1.add(fval2);
            resk = resk.add(wgk[jtwm1].multiply(fsum));
            resabs[0] = resabs[0].add(wgk[jtwm1].multiply( (fval1.abs()).add(fval2.abs())));
        } // for (j = 0; j < 10; j++)

        reskh = (DoubleDouble.valueOf(0.5)).multiply(resk);
        resasc[0] = wgk[20].multiply( (fc.subtract(reskh)).abs());

        for (j = 0; j < 20; j++) {
            resasc[0] = resasc[0].add(wgk[j].multiply( ( (fv1[j].subtract(reskh)).abs()).add( (fv2[j].subtract(reskh))
                    .abs())));
        } // for (j = 0; j < 20; j++)

        result[0] = resk.multiply(hlgth);
        resabs[0] = resabs[0].multiply(dhlgth);
        resasc[0] = resasc[0].multiply(dhlgth);
        abserr[0] = ( (resk.subtract(resg)).multiply(hlgth)).abs();

        if ( (resasc[0].ne(DoubleDouble.valueOf(0.0))) && (abserr[0].ne(DoubleDouble.valueOf(0.0)))) {
            final DoubleDouble base = ( (DoubleDouble.valueOf(200.0)).multiply(abserr[0])).divide(resasc[0]);
            abserr[0] = resasc[0].multiply( (DoubleDouble.valueOf(1.0)).min(base.pow(1.5)));
        }

        if (resabs[0].gt(uflow.divide( (DoubleDouble.valueOf(50.0)).multiply(epmach)))) {
            abserr[0] = ( ( (DoubleDouble.valueOf(50.0)).multiply(epmach)).multiply(resabs[0])).max(abserr[0]);
        }

        return;
    }

    /**
     * This is a port of the original FORTRAN routine whose header is given below: c***begin prologue dqk51 c***date
     * written 800101 (yymmdd) c***revision date 830518 (yymmdd) c***category no. h2a1a2 c***keywords 51-point
     * gauss-kronrod rules c***author piessens,robert,appl. math. & progr. div. - k.u.leuven c de doncker,elise,appl.
     * math & progr. div. - k.u.leuven c***purpose to compute i = integral of f over (a,b) with error c estimate c j =
     * integral of abs(f) over (a,b) c***description c c integration rules c standard fortran subroutine c DoubleDouble
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
    private void dqk51(final DoubleDouble a, final DoubleDouble b, final DoubleDouble[] result,
            final DoubleDouble[] abserr, final DoubleDouble[] resabs, final DoubleDouble[] resasc) {
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
        final DoubleDouble[] xgk = new DoubleDouble[] {DoubleDouble.valueOf(0.999262104992609834193457486540341),
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
                DoubleDouble.valueOf(0.000000000000000000000000000000000)};

        // wgk - weights of the 51-point kronrod rule
        final DoubleDouble[] wgk = new DoubleDouble[] {DoubleDouble.valueOf(0.001987383892330315926507851882843),
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
                DoubleDouble.valueOf(0.061580818067832935078759824240066)};

        // wg - weights of the 25-point gauss rule
        final DoubleDouble[] wg = new DoubleDouble[] {DoubleDouble.valueOf(0.011393798501026287947902964113235),
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
                DoubleDouble.valueOf(0.123176053726715451203902873079050)};

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
        final DoubleDouble[] fv1 = new DoubleDouble[25];
        final DoubleDouble[] fv2 = new DoubleDouble[25];

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
        } else {
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
            } else {
                fval1 = intFunc(centr.subtract(absc));
                fval2 = intFunc(centr.add(absc));
            }
            fv1[jtw] = (DoubleDouble) fval1.clone();
            fv2[jtw] = (DoubleDouble) fval2.clone();
            fsum = fval1.add(fval2);
            resg = resg.add(wg[j].multiply(fsum));
            resk = resk.add(wgk[jtw].multiply(fsum));
            resabs[0] = resabs[0].add(wgk[jtw].multiply( (fval1.abs()).add(fval2.abs())));
        } // for (j = 0; j < 12; j++)

        for (j = 0; j < 13; j++) {
            jtwm1 = 2 * j;
            absc = hlgth.multiply(xgk[jtwm1]);
            if (selfTest) {
                fval1 = intFuncTest(centr.subtract(absc));
                fval2 = intFuncTest(centr.add(absc));
            } else {
                fval1 = intFunc(centr.subtract(absc));
                fval2 = intFunc(centr.add(absc));
            }
            fv1[jtwm1] = (DoubleDouble) fval1.clone();
            fv2[jtwm1] = (DoubleDouble) fval2.clone();
            fsum = fval1.add(fval2);
            resk = resk.add(wgk[jtwm1].multiply(fsum));
            resabs[0] = resabs[0].add(wgk[jtwm1].multiply( (fval1.abs()).add(fval2.abs())));
        } // for (j = 0; j < 13; j++)

        reskh = (DoubleDouble.valueOf(0.5)).multiply(resk);
        resasc[0] = wgk[25].multiply( (fc.subtract(reskh)).abs());

        for (j = 0; j < 25; j++) {
            resasc[0] = resasc[0].add(wgk[j].multiply( ( (fv1[j].subtract(reskh)).abs()).add( (fv2[j].subtract(reskh))
                    .abs())));
        } // for (j = 0; j < 25; j++)

        result[0] = resk.multiply(hlgth);
        resabs[0] = resabs[0].multiply(dhlgth);
        resasc[0] = resasc[0].multiply(dhlgth);
        abserr[0] = ( (resk.subtract(resg)).multiply(hlgth)).abs();

        if ( (resasc[0].ne(DoubleDouble.valueOf(0.0))) && (abserr[0].ne(DoubleDouble.valueOf(0.0)))) {
            final DoubleDouble base = ( (DoubleDouble.valueOf(200.0)).multiply(abserr[0])).divide(resasc[0]);
            abserr[0] = resasc[0].multiply( (DoubleDouble.valueOf(1.0)).min(base.pow(1.5)));
        }

        if (resabs[0].gt(uflow.divide( (DoubleDouble.valueOf(50.0)).multiply(epmach)))) {
            abserr[0] = ( ( (DoubleDouble.valueOf(50.0)).multiply(epmach)).multiply(resabs[0])).max(abserr[0]);
        }

        return;
    }

    /**
     * This is a port of the original FORTRAN routine whose header is given below: c***begin prologue dqk61 c***date
     * written 800101 (yymmdd) c***revision date 830518 (yymmdd) c***category no. h2a1a2 c***keywords 61-point
     * gauss-kronrod rules c***author piessens,robert,appl. math. & progr. div. - k.u.leuven c de doncker,elise,appl.
     * math. & progr. div. - k.u.leuven c***purpose to compute i = integral of f over (a,b) with error c estimate c j =
     * integral of dabs(f) over (a,b) c***description c c integration rule c standard fortran subroutine c DoubleDouble
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
    private void dqk61(final DoubleDouble a, final DoubleDouble b, final DoubleDouble[] result,
            final DoubleDouble[] abserr, final DoubleDouble[] resabs, final DoubleDouble[] resasc) {
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
        final DoubleDouble[] xgk = new DoubleDouble[] {DoubleDouble.valueOf(0.999484410050490637571325895705811),
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
                DoubleDouble.valueOf(0.000000000000000000000000000000000)};

        // wgk - weights of the 61-point kronrod rule
        final DoubleDouble[] wgk = new DoubleDouble[] {DoubleDouble.valueOf(0.001389013698677007624551591226760),
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
                DoubleDouble.valueOf(0.051494729429451567558340433647099)};

        // wg - weights of the 30-point gauss rule
        final DoubleDouble[] wg = new DoubleDouble[] {DoubleDouble.valueOf(0.007968192496166605615465883474674),
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
                DoubleDouble.valueOf(0.102852652893558840341285636705415)};

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
        final DoubleDouble[] fv1 = new DoubleDouble[30];
        final DoubleDouble[] fv2 = new DoubleDouble[30];

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
        } else {
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
            } else {
                fval1 = intFunc(centr.subtract(absc));
                fval2 = intFunc(centr.add(absc));
            }
            fv1[jtw] = (DoubleDouble) fval1.clone();
            fv2[jtw] = (DoubleDouble) fval2.clone();
            fsum = fval1.add(fval2);
            resg = resg.add(wg[j].multiply(fsum));
            resk = resk.add(wgk[jtw].multiply(fsum));
            resabs[0] = resabs[0].add(wgk[jtw].multiply( (fval1.abs()).add(fval2.abs())));
        } // for (j = 0; j < 15; j++)

        for (j = 0; j < 15; j++) {
            jtwm1 = 2 * j;
            absc = hlgth.multiply(xgk[jtwm1]);
            if (selfTest) {
                fval1 = intFuncTest(centr.subtract(absc));
                fval2 = intFuncTest(centr.add(absc));
            } else {
                fval1 = intFunc(centr.subtract(absc));
                fval2 = intFunc(centr.add(absc));
            }
            fv1[jtwm1] = (DoubleDouble) fval1.clone();
            fv2[jtwm1] = (DoubleDouble) fval2.clone();
            fsum = fval1.add(fval2);
            resk = resk.add(wgk[jtwm1].multiply(fsum));
            resabs[0] = resabs[0].add(wgk[jtwm1].multiply( (fval1.abs()).add(fval2.abs())));
        } // for (j = 0; j < 15; j++)

        reskh = (DoubleDouble.valueOf(0.5)).multiply(resk);
        resasc[0] = wgk[30].multiply( (fc.subtract(reskh)).abs());

        for (j = 0; j < 30; j++) {
            resasc[0] = resasc[0].add(wgk[j].multiply( ( (fv1[j].subtract(reskh)).abs()).add( (fv2[j].subtract(reskh))
                    .abs())));
        } // for (j = 0; j < 30; j++)

        result[0] = resk.multiply(hlgth);
        resabs[0] = resabs[0].multiply(dhlgth);
        resasc[0] = resasc[0].multiply(dhlgth);
        abserr[0] = ( (resk.subtract(resg)).multiply(hlgth)).abs();

        if ( (resasc[0].ne(DoubleDouble.valueOf(0.0))) && (abserr[0].ne(DoubleDouble.valueOf(0.0)))) {
            final DoubleDouble base = ( (DoubleDouble.valueOf(200.0)).multiply(abserr[0])).divide(resasc[0]);
            abserr[0] = resasc[0].multiply( (DoubleDouble.valueOf(1.0)).min(base.pow(1.5)));
        }

        if (resabs[0].gt(uflow.divide( (DoubleDouble.valueOf(50.0)).multiply(epmach)))) {
            abserr[0] = ( ( (DoubleDouble.valueOf(50.0)).multiply(epmach)).multiply(resabs[0])).max(abserr[0]);
        }

        return;
    }

    /**
     * This is a port of the original FORTRAN routine whose header is given below: c***begin prologue dqmomo c***date
     * written 820101 (yymmdd) c***revision date 830518 (yymmdd) c***category no. h2a2a1,c3a2 c***keywords modified
     * chebyshev moments c***author piessens,robert,appl. math. & progr. div. - k.u.leuven c de doncker,elise,appl.
     * math. & progr. div. - k.u.leuven c***purpose this routine computes modified chebsyshev moments. the k-th c
     * modified chebyshev moment is defined as the integral over c (-1,1) of w(x)*t(k,x), where t(k,x) is the chebyshev
     * c polynomial of degree k. c***description c c modified chebyshev moments c standard fortran subroutine c
     * DoubleDouble precision version c c parameters c alfa - DoubleDouble precision c parameter in the weight function
     * w(x), alfa.gt.(-1) c c beta - DoubleDouble precision c parameter in the weight function w(x), beta.gt.(-1) c c ri -
     * DoubleDouble precision c vector of dimension 25 c ri(k) is the integral over (-1,1) of c (1+x)**alfa*t(k-1,x), k =
     * 1, ..., 25. c c rj - DoubleDouble precision c vector of dimension 25 c rj(k) is the integral over (-1,1) of c
     * (1-x)**beta*t(k-1,x), k = 1, ..., 25. c c rg - DoubleDouble precision c vector of dimension 25 c rg(k) is the
     * integral over (-1,1) of c (1+x)**alfa*log((1+x)/2)*t(k-1,x), k = 1, ..., 25. c c rh - DoubleDouble precision c
     * vector of dimension 25 c rh(k) is the integral over (-1,1) of c (1-x)**beta*log((1-x)/2)*t(k-1,x), k = 1, ...,
     * 25. c c integr - integer c input parameter indicating the modified c moments to be computed c integr = 1 compute
     * ri, rj c = 2 compute ri, rj, rg c = 3 compute ri, rj, rh c = 4 compute ri, rj, rg, rh c c***references (none)
     * c***routines called (none) c***end prologue dqmomo c
     * 
     * @param ri
     * @param rj
     * @param rg
     * @param rh
     */
    private void dqmomo(final DoubleDouble ri[], final DoubleDouble rj[], final DoubleDouble rg[],
            final DoubleDouble rh[]) {
        DoubleDouble alfp1;
        DoubleDouble alfp2;
        DoubleDouble an;
        DoubleDouble anm1;
        DoubleDouble betp1;
        DoubleDouble betp2;
        DoubleDouble ralf;
        DoubleDouble rbet;

        int i;
        int im1;

        alfp1 = alfa.add(DoubleDouble.valueOf(1.0));
        betp1 = beta.add(DoubleDouble.valueOf(1.0));
        alfp2 = alfa.add(DoubleDouble.valueOf(2.0));
        betp2 = beta.add(DoubleDouble.valueOf(2.0));
        ralf = (DoubleDouble.valueOf(2.0)).pow(alfp1);
        rbet = (DoubleDouble.valueOf(2.0)).pow(betp1);

        // Compute ri, rj using a forward recurrence relation

        ri[0] = ralf.divide(alfp1);
        rj[0] = rbet.divide(betp1);
        ri[1] = (ri[0].multiply(alfa)).divide(alfp2);
        rj[1] = (rj[0].multiply(beta)).divide(betp2);
        an = DoubleDouble.valueOf(2.0);
        anm1 = DoubleDouble.valueOf(1.0);
        for (i = 2; i <= 24; i++) {
            ri[i] = ( (ralf.add( (an.multiply(an.subtract(alfp2))).multiply(ri[i - 1]))).negate()).divide(anm1
                    .multiply(an.add(alfp1)));
            rj[i] = ( (rbet.add( (an.multiply(an.subtract(betp2))).multiply(rj[i - 1]))).negate()).divide(anm1
                    .multiply(an.add(betp1)));
            anm1 = (DoubleDouble) an.clone();
            an = an.add(DoubleDouble.valueOf(1.0));
        } // for (i = 2; i <= 24; i++)

        if ( (integr == 2) || (integr == 4)) {

            // compute rg using a forward recurrence relation.

            rg[0] = (ri[0].divide(alfp1)).negate();
            rg[1] = ( ( (ralf.add(ralf)).negate()).divide(alfp2.multiply(alfp2))).subtract(rg[0]);
            an = DoubleDouble.valueOf(2.0);
            anm1 = DoubleDouble.valueOf(1.0);
            im1 = 2;
            for (i = 3; i <= 25; i++) {
                rg[i - 1] = ( ( ( (an.multiply(an.subtract(alfp2))).multiply(rg[im1 - 1])).subtract(an
                        .multiply(ri[im1 - 1])).add(anm1.multiply(ri[i - 1]))).negate()).divide(anm1.multiply(an
                        .add(alfp1)));
                anm1 = (DoubleDouble) an.clone();
                an = an.add(DoubleDouble.valueOf(1.0));
                im1 = i;
            } // for (i = 3 ; i <= 25; i++)
        } // if ((integr == 2) || (integr == 4)

        if ( (integr == 3) || (integr == 4)) {

            // compute rh using a forward recurrence relation.

            rh[0] = (rj[0].negate()).divide(betp1);
            rh[1] = ( ( (rbet.add(rbet)).negate()).divide(betp2.multiply(betp2))).subtract(rh[0]);
            an = DoubleDouble.valueOf(2.0);
            anm1 = DoubleDouble.valueOf(1.0);
            im1 = 2;
            for (i = 3; i <= 25; i++) {
                rh[i - 1] = ( ( ( (an.multiply(an.subtract(betp2))).multiply(rh[im1 - 1])).subtract(an
                        .multiply(rj[im1 - 1])).add(anm1.multiply(rj[i - 1]))).negate()).divide(anm1.multiply(an
                        .add(betp1)));
                anm1 = (DoubleDouble) an.clone();
                an = an.add(DoubleDouble.valueOf(1.0));
                im1 = i;
            } // for (i = 3; i <= 25; i++)
            for (i = 2; i <= 25; i += 2) {
                rh[i - 1] = rh[i - 1].negate();
            } // for (i = 2; i <= 25; i += 2)
        } // if ((integr == 3) || (integr == 4))
        for (i = 2; i <= 25; i += 2) {
            rj[i - 1] = rj[i - 1].negate();
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
     * c***description c c non-adaptive integration c standard fortran subroutine c DoubleDouble precision version
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

        final DoubleDouble[] x1 = new DoubleDouble[] {DoubleDouble.valueOf(0.973906528517171720077964012084452),
                DoubleDouble.valueOf(0.865063366688984510732096688423493),
                DoubleDouble.valueOf(0.679409568299024406234327365114874),
                DoubleDouble.valueOf(0.433395394129247190799265943165784),
                DoubleDouble.valueOf(0.148874338981631210884826001129720)};

        final DoubleDouble[] w10 = new DoubleDouble[] {DoubleDouble.valueOf(0.066671344308688137593568809893332),
                DoubleDouble.valueOf(0.149451349150580593145776339657697),
                DoubleDouble.valueOf(0.219086362515982043995534934228163),
                DoubleDouble.valueOf(0.269266719309996355091226921569469),
                DoubleDouble.valueOf(0.295524224714752870173892994651338)};

        final DoubleDouble[] x2 = new DoubleDouble[] {DoubleDouble.valueOf(0.995657163025808080735527280689003),
                DoubleDouble.valueOf(0.930157491355708226001207180059508),
                DoubleDouble.valueOf(0.780817726586416897063717578345042),
                DoubleDouble.valueOf(0.562757134668604683339000099272694),
                DoubleDouble.valueOf(0.294392862701460198131126603103866)};

        final DoubleDouble[] w21a = new DoubleDouble[] {DoubleDouble.valueOf(0.032558162307964727478818972459390),
                DoubleDouble.valueOf(0.075039674810919952767043140916190),
                DoubleDouble.valueOf(0.109387158802297641899210590325805),
                DoubleDouble.valueOf(0.134709217311473325928054001771707),
                DoubleDouble.valueOf(0.147739104901338491374841515972068)};

        final DoubleDouble[] w21b = new DoubleDouble[] {DoubleDouble.valueOf(0.011694638867371874278064396062192),
                DoubleDouble.valueOf(0.054755896574351996031381300244580),
                DoubleDouble.valueOf(0.093125454583697605535065465083366),
                DoubleDouble.valueOf(0.123491976262065851077958109831074),
                DoubleDouble.valueOf(0.142775938577060080797094273138717),
                DoubleDouble.valueOf(0.149445554002916905664936468389821)};

        final DoubleDouble[] x3 = new DoubleDouble[] {DoubleDouble.valueOf(0.999333360901932081394099323919911),
                DoubleDouble.valueOf(0.987433402908088869795961478381209),
                DoubleDouble.valueOf(0.954807934814266299257919200290473),
                DoubleDouble.valueOf(0.900148695748328293625099494069092),
                DoubleDouble.valueOf(0.825198314983114150847066732588520),
                DoubleDouble.valueOf(0.732148388989304982612354848755461),
                DoubleDouble.valueOf(0.622847970537725238641159120344323),
                DoubleDouble.valueOf(0.499479574071056499952214885499755),
                DoubleDouble.valueOf(0.364901661346580768043989548502644),
                DoubleDouble.valueOf(0.222254919776601296498260928066212),
                DoubleDouble.valueOf(0.074650617461383322043914435796506)};

        final DoubleDouble[] w43a = new DoubleDouble[] {DoubleDouble.valueOf(0.016296734289666564924281974617663),
                DoubleDouble.valueOf(0.037522876120869501461613795898115),
                DoubleDouble.valueOf(0.054694902058255442147212685465005),
                DoubleDouble.valueOf(0.067355414609478086075553166302174),
                DoubleDouble.valueOf(0.073870199632393953432140695251367),
                DoubleDouble.valueOf(0.005768556059769796184184327908655),
                DoubleDouble.valueOf(0.027371890593248842081276069289151),
                DoubleDouble.valueOf(0.046560826910428830743339154433824),
                DoubleDouble.valueOf(0.061744995201442564496240336030883),
                DoubleDouble.valueOf(0.071387267268693397768559114425516)};

        final DoubleDouble[] w43b = new DoubleDouble[] {DoubleDouble.valueOf(0.001844477640212414100389106552965),
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
                DoubleDouble.valueOf(0.074722147517403005594425168280423)};

        final DoubleDouble[] x4 = new DoubleDouble[] {DoubleDouble.valueOf(0.999902977262729234490529830591582),
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
                DoubleDouble.valueOf(0.037352123394619870814998165437704)};

        final DoubleDouble[] w87a = new DoubleDouble[] {DoubleDouble.valueOf(0.008148377384149172900002878448190),
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
                DoubleDouble.valueOf(0.037253875503047708539592001191226)};

        final DoubleDouble[] w87b = new DoubleDouble[] {DoubleDouble.valueOf(0.000274145563762072350016527092881),
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
                DoubleDouble.valueOf(0.037361073762679023410321241766599)};

        // mid point of integration interval
        DoubleDouble centr;

        // half-length of integration interval
        DoubleDouble hlgth;

        // function value
        DoubleDouble fval;

        // array of function values which have already been computed
        final DoubleDouble[] savfun = new DoubleDouble[21];

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
        final DoubleDouble[] fv1 = new DoubleDouble[5];
        final DoubleDouble[] fv2 = new DoubleDouble[5];
        final DoubleDouble[] fv3 = new DoubleDouble[5];
        final DoubleDouble[] fv4 = new DoubleDouble[5];
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
        } else {
            fcentr = intFunc(centr);
        }
        neval[0] = 21;
        ier[0] = 1;

        // Compute the integral using the 10- and 21-point formula.

        res10 = DoubleDouble.valueOf(0.0);
        res21 = w21b[5].multiply(fcentr);
        resabs = w21b[5].multiply(fcentr.abs());

        for (k = 0; k < 5; k++) {
            absc = hlgth.multiply(x1[k]);
            if (selfTest) {
                fval1 = intFuncTest(centr.add(absc));
                fval2 = intFuncTest(centr.subtract(absc));
            } else {
                fval1 = intFunc(centr.add(absc));
                fval2 = intFunc(centr.subtract(absc));
            }
            fval = fval1.add(fval2);
            res10 = res10.add(w10[k].multiply(fval));
            res21 = res21.add(w21a[k].multiply(fval));
            resabs = resabs.add(w21a[k].multiply( (fval1.abs()).add(fval2.abs())));
            savfun[k] = (DoubleDouble) fval.clone();
            fv1[k] = (DoubleDouble) fval1.clone();
            fv2[k] = (DoubleDouble) fval2.clone();
        } // for (k = 0; k < 5; k++)

        ipx = 4;

        for (k = 0; k < 5; k++) {
            ipx++;
            absc = hlgth.multiply(x2[k]);
            if (selfTest) {
                fval1 = intFuncTest(centr.add(absc));
                fval2 = intFuncTest(centr.subtract(absc));
            } else {
                fval1 = intFunc(centr.add(absc));
                fval2 = intFunc(centr.subtract(absc));
            }
            fval = fval1.add(fval2);
            res21 = res21.add(w21b[k].multiply(fval));
            resabs = resabs.add(w21b[k].multiply( (fval1.abs()).add(fval2.abs())));
            savfun[ipx] = (DoubleDouble) fval.clone();
            fv3[k] = (DoubleDouble) fval1.clone();
            fv4[k] = (DoubleDouble) fval2.clone();
        } // for (k = 0; k < 5; k++)

        // Test for convergence

        result[0] = res21.multiply(hlgth);
        resabs = resabs.multiply(dhlgth);
        reskh = (DoubleDouble.valueOf(0.5)).multiply(res21);
        resasc = w21b[5].multiply( (fcentr.subtract(reskh)).abs());

        for (k = 0; k < 5; k++) {
            resasc = resasc.add(
                    w21a[k].multiply( ( (fv1[k].subtract(reskh)).abs()).add( (fv2[k].subtract(reskh)).abs()))).add(
                    w21b[k].multiply( ( (fv3[k].subtract(reskh)).abs()).add( (fv4[k].subtract(reskh)).abs())));
        } // for (k = 0; k < 5; k++)

        abserr[0] = ( (res21.subtract(res10)).multiply(hlgth)).abs();
        resasc = resasc.multiply(dhlgth);

        if ( (resasc.ne(DoubleDouble.valueOf(0.0))) && (abserr[0].ne(DoubleDouble.valueOf(0.0)))) {
            final DoubleDouble base = ( (DoubleDouble.valueOf(200.0)).multiply(abserr[0])).divide(resasc);
            abserr[0] = resasc.multiply( (DoubleDouble.valueOf(1.0)).min(base.pow(1.5)));
        } // if ((resasc != 0.0) && (abserr[0] != 0.0))

        if (resabs.gt(uflow.divide( (DoubleDouble.valueOf(50.0)).multiply(epmach)))) {
            abserr[0] = ( (epmach.multiply(DoubleDouble.valueOf(50.0))).multiply(resabs)).max(abserr[0]);
        } // if (resabs > uflow/(50.0 * epmach))

        if (abserr[0].le(epsabs.max(epsrel.multiply(result[0].abs())))) {
            ier[0] = 0;
        } // if (abserr[0] <= Math.max(epsabs, epsrel * Math.abs(result[0])))

        if (ier[0] == 0) {
            return;
        } // if (ier[0] == 0)

        // Compute the integral using the 43-point formula

        res43 = w43b[11].multiply(fcentr);
        neval[0] = 43;

        for (k = 0; k < 10; k++) {
            res43 = res43.add(savfun[k].multiply(w43a[k]));
        } // for (k = 0; k < 10; k++)

        for (k = 0; k < 11; k++) {
            ipx++;
            absc = hlgth.multiply(x3[k]);
            if (selfTest) {
                fval = intFuncTest(absc.add(centr)).add(intFuncTest(centr.subtract(absc)));
            } else {
                fval = intFunc(absc.add(centr)).add(intFunc(centr.subtract(absc)));
            }
            res43 = res43.add(fval.multiply(w43b[k]));
            savfun[ipx] = (DoubleDouble) fval.clone();
        } // for (k = 0; k < 11; k++)

        // Test for convergence
        result[0] = res43.multiply(hlgth);
        abserr[0] = ( (res43.subtract(res21)).multiply(hlgth)).abs();

        if ( (resasc.ne(DoubleDouble.valueOf(0.0))) && (abserr[0].ne(DoubleDouble.valueOf(0.0)))) {
            final DoubleDouble base = ( (DoubleDouble.valueOf(200.0)).multiply(abserr[0])).divide(resasc);
            abserr[0] = resasc.multiply( (DoubleDouble.valueOf(1.0)).min(base.pow(1.5)));
        } // if ((resasc != 0.0) && (abserr[0] != 0.0))

        if (resabs.gt(uflow.divide( (DoubleDouble.valueOf(50.0)).multiply(epmach)))) {
            abserr[0] = ( (epmach.multiply(DoubleDouble.valueOf(50.0))).multiply(resabs)).max(abserr[0]);
        } // if (resabs > uflow/(50.0 * epmach))

        if (abserr[0].le(epsabs.max(epsrel.multiply(result[0].abs())))) {
            ier[0] = 0;
        } // if (abserr[0] <= Math.max(epsabs, epsrel * Math.abs(result[0])))

        if (ier[0] == 0) {
            return;
        } // if (ier[0] == 0)

        // Compute the integral using the 87-point formula
        res87 = w87b[22].multiply(fcentr);
        neval[0] = 87;

        for (k = 0; k < 21; k++) {
            res87 = res87.add(savfun[k].multiply(w87a[k]));
        } // for (k = 0; k < 21; k++)

        for (k = 0; k < 22; k++) {
            absc = hlgth.multiply(x4[k]);
            if (selfTest) {
                res87 = res87
                        .add(w87b[k].multiply(intFuncTest(absc.add(centr)).add(intFuncTest(centr.subtract(absc)))));
            } else {
                res87 = res87.add(w87b[k].multiply(intFunc(absc.add(centr)).add(intFunc(centr.subtract(absc)))));
            }
        } // for (k = 0; k < 22; k++)

        result[0] = res87.multiply(hlgth);
        abserr[0] = ( (res87.subtract(res43)).multiply(hlgth)).abs();

        if ( (resasc.ne(DoubleDouble.valueOf(0.0))) && (abserr[0].ne(DoubleDouble.valueOf(0.0)))) {
            final DoubleDouble base = ( (DoubleDouble.valueOf(200.0)).multiply(abserr[0])).divide(resasc);
            abserr[0] = resasc.multiply( ( (DoubleDouble.valueOf(1.0)).min(base.pow(1.5))));
        } // if ((resasc != 0.0) && (abserr[0] != 0.0))

        if (resabs.gt(uflow.divide( (DoubleDouble.valueOf(50.0)).multiply(epmach)))) {
            abserr[0] = ( (epmach.multiply(DoubleDouble.valueOf(50.0))).multiply(resabs)).max(abserr[0]);
        } // if (resabs > uflow/(50.0 * epmach))

        if (abserr[0].le(epsabs.max(epsrel.multiply(result[0].abs())))) {
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
     * smallest error estimate. c***description c c ordering routine c standard fortran subroutine c DoubleDouble
     * precision version
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
    private void dqpsrt(final int limit, final int last, final int[] maxErr, final DoubleDouble[] ermax,
            final DoubleDouble[] elist, final int[] iord, final int[] nrmax) {
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
            ermax[0] = (DoubleDouble) elist[maxErr[0]].clone();

            return;
        } // if (last <= 2)

        // This part of the routine is only executed if, due to a difficult
        // integrand, subdivision increased the error estimate. In the
        // normal case the insert procedure should start after the nrmax-th
        // largest error estimate

        errmax = (DoubleDouble) elist[maxErr[0]].clone();

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
        // descending order. This number depends on the number of
        // subdivisions still allowed
        jupbn = last;

        if (last > ( (limit / 2) + 2)) {
            jupbn = limit + 3 - last;
        } // if (last > (limit/2 + 2))

        errmin = (DoubleDouble) elist[last - 1].clone();

        // Insert errmax by traversing the list top-down,
        // starting comparison from the element elist(iord[nrmmax[0]])

        jbnd = jupbn - 1;
        ibeg = nrmax[0] + 1;

        group: {

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
            ermax[0] = (DoubleDouble) elist[maxErr[0]].clone();

            return;
        } // group

        // Insert errmin by traversing the list from the bottom-up

        iord[i - 2] = maxErr[0];
        k = jbnd;

        group2: {

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
            ermax[0] = (DoubleDouble) elist[maxErr[0]].clone();

            return;
        } // group2

        iord[k] = last - 1;
        maxErr[0] = iord[nrmax[0] - 1];
        ermax[0] = (DoubleDouble) elist[maxErr[0]].clone();

        return;
    }
}
