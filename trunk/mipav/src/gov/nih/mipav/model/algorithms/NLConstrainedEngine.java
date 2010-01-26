package gov.nih.mipav.model.algorithms;


import gov.nih.mipav.view.*;


/**
 * This is a port of the program ELSUNC for solving least squares problems where the unknowns are restricted by lower
 * and upper bounds. Reference: Gauss-Newton Based Algorithms For Constrained Nonlinear Least Squares Problems by Per
 * Lindstrom and Per-Ake Wedin, Institute of Information Processing, University of Umea, S-901 87 Umea, Sweden This can
 * be downleaded from http://www.cs.umu.se/~perl/reports/alg.ps.gz
 */

// BELOW IS AN EXAMPLE OF A DRIVER USED IN FITTING A 4 PARAMETER
// DOUBLE EXPONENTIAL ROUTINE

/*class FitDoubleExponentialNoWholeConstrainedModel extends NLConstrainedEngine {
 *
 * public FitDoubleExponentialNoWholeConstrainedModel(int nPoints, double xData[], float yData[],
 *   double initial[]) {     // nPoints data points, 4 coefficients, and exponential fitting
 * super(nPoints,4,xData,yData);     int i;
 *
 *   bounds = 2; // bounds = 0 means unconstrained                 // bounds = 1 means same lower and upper bounds for
 *               // all parameters                 // bounds = 2 means different lower and upper bounds
 * // for all parameters     // Constrain gamma to be between 0.0 and 1.0     bl[0] = 0.0;     bu[0] = 1.0;     //
 * Constrain alpha to be negative     bl[1] = -Double.MAX_VALUE;     bu[1] = -Double.MIN_VALUE;     // Constrain beta to
 * be negative     bl[2] = -Double.MAX_VALUE;     bu[2] = -Double.MIN_VALUE;     // Constrain bottom to be between 0.0
 * and 1.0     bl[3] = 0.0;     bu[3] = 1.0;
 *
 *   // The default is internalScaling = false     // To make internalScaling = true and have the columns of the     //
 * Jacobian scaled to have unit length include the following line.     internalScaling = true;
 *
 *
 *   gues[0] = initial[0];     gues[1] = initial[1];     gues[2] = initial[2];     gues[3] = initial[3]; }
 *
 * /**  Fit to function - a3 + (1-a3)*[1 - ao*exp(a1*x) - (1 - a0)*exp(a2*x)]  @param x1    The x value of the data point.
 *  @param atry  The best guess parameter values.  @param dyda  The derivative values of y with respect to fitting
 * parameters.  @return      The calculated y value.
 */
/*public void fitToFunction(double a[], double residuals[],
 *                          double covarMat[][]) { int ctrl; int i; double ymodel = 0.0; double e1, e2;
 *
 *         try {               ctrl = ctrlMat[0];               if ((ctrl == -1) || (ctrl == 1)) {                 //
 * evaluate the residuals[i] = ymodel[i] - ySeries[i]                 for (i = 0; i < nPts; i++) {
 * ymodel = a[3] + (1.0 - a[3])*(1 - a[0]*Math.exp(a[1]*xSeries[i])                                 - (1.0 -
 * a[0])*Math.exp(a[2]*xSeries[i]));                   residuals[i] = ymodel - ySeries[i];                 }
 *   } // if ((ctrl == -1) || (ctrl == 1))               else if (ctrl == 2) {                 // Calculate the Jacobian
 * analytically                 for (i = 0; i < nPts; i++) {                   e1 = Math.exp(a[1]*xSeries[i]);
 *         e2 = Math.exp(a[2]*xSeries[i]);                   covarMat[i][0] = (1.0 - a[3])*(-e1 + e2);
 * covarMat[i][1] = -(1.0 - a[3])*a[0]*xSeries[i]*e1;                   covarMat[i][2] = -(1.0 - a[3])*(1.0 -
 * a[0])*xSeries[i]*e2;                   covarMat[i][3] = a[0]*e1 + (1.0 - a[0])*e2;                 }               }
 * // else if (ctrl == 2)               // To calculate the Jacobian numerically just set               // ctrlMat[0] =
 * 0 and return               else if (ctrl == 2) {                 ctrlMat[0] = 0;               } // else if (ctrl ==
 * 2)         }         catch (Exception e) {                 Preferences.debug("function error: " + e.getMessage() +
 * "\n");         }         return;}*/


/**
 * Starts the analysis.
 */
/*public void driver() {
 *  super.driver();}*/


/**
 * Display results of displaying exponential fitting parameters.
 */
/*public void dumpResults() {
 *  Preferences.debug(" ******* FitDoubleExponential ********* \n\n"); Preferences.debug("Number of iterations: " +
 * String.valueOf(iters) + "\n"); Preferences.debug("Chi-squared: " + String.valueOf(getChiSquared()) + "\n");
 * Preferences.debug("a0 " + String.valueOf(a[0]) + "\n"); Preferences.debug("a1 " + String.valueOf(a[1]) + "\n");
 * Preferences.debug("a2 " + String.valueOf(a[2]) + "\n"); Preferences.debug("a3 " + String.valueOf(a[3]) + "\n"); }}*/

/* Below is an example used to fit y = alpha - beta*(rho**x)
 * This example implements the solution of problem D of chapter 24 of Applied Regression Analysis, Third Edition by
 * Norman R. Draper andHarry Smith */


/*public void runTest24D() {
 * // Answer is a0 = 72.4326, a1 = 28.2519, a2 = 0.5968 double xSeries[] = new double[5]; float ySeries[] = new
 * float[5]; double initial[] = new double[3]; int nPoints = 5; Fit24DModel fmod; xSeries[0] = 0.0; xSeries[1] = 1.0;
 * xSeries[2] = 2.0; xSeries[3] = 3.0; xSeries[4] = 4.0; ySeries[0] = 44.4f; ySeries[1] = 54.6f; ySeries[2] = 63.8f;
 * ySeries[3] = 65.7f; ySeries[4] = 68.9f; initial[0] = 0.0; initial[1] = 10.0; initial[2] = 0.2; fmod = new
 * Fit24DModel(nPoints,xSeries,ySeries,initial); fmod.driver(); fmod.dumpResults();}*/


/**
 * class Fit24DModel extends NLConstrainedEngine { public Fit24DModel(int nPoints, double xData[], float yData[], double
 * initial[]) { // nPoints data points, 3 coefficients, and exponential fitting super(nPoints,3,xData,yData); int i;
 * bounds = 2; // bounds = 0 means unconstrained // bounds = 1 means same lower and upper bounds for // all parameters
 * // bounds = 2 means different lower and upper bounds // for all parameters // Constrain alpha bl[0] = -1000.0; bu[0]
 * = 1000.0; // Constrain beta bl[1] = -1000.0; bu[1] = 1000.0; // Constrain rho bl[2] = 0.0; bu[2] = 1.0; // The
 * default is internalScaling = false // To make internalScaling = true and have the columns of the // Jacobian scaled
 * to have unit length include the following line. //internalScaling = true; gues[0] = initial[0]; gues[1] = initial[1];
 * gues[2] = initial[2]; } /** Fit to function - a0 - a1*(a2**x)
 *
 * @param   x1    The x value of the data point.
 * @param   atry  The best guess parameter values.
 * @param   dyda  The derivative values of y with respect to fitting parameters.
 *
 * @return  The calculated y value.
 */
/* public void fitToFunction(double a[], double residuals[],
 *                           double covarMat[][]) {  int ctrl;  int i;  double ymodel = 0.0;  double e1, e2;
 *
 *          try {                ctrl = ctrlMat[0];                if ((ctrl == -1) || (ctrl == 1)) {
 * // evaluate the residuals[i] = ymodel[i] - ySeries[i]                  for (i = 0; i < nPts; i++) {
 *  ymodel = a[0] - a[1]*Math.pow(a[2],xSeries[i]);                    residuals[i] = ymodel - ySeries[i];
 *    }                } // if ((ctrl == -1) || (ctrl == 1))                else if (ctrl == 2) {                  //
 * Calculate the Jacobian analytically                  for (i = 0; i < nPts; i++) {                    e1 =
 * Math.exp(a[1]*xSeries[i]);                    e2 = Math.exp(a[2]*xSeries[i]);                    covarMat[i][0] =
 * 1.0;                    covarMat[i][1] = -Math.pow(a[2],xSeries[i]);                    covarMat[i][2] =
 * -xSeries[i]*a[1]*Math.pow(a[2],xSeries[i] - 1.0);                  }                } // else if (ctrl == 2)
 *       // To calculate the Jacobian numerically just set                // ctrlMat[0] = 0 and return
 * else if (ctrl == 2) {                  ctrlMat[0] = 0;                } // else if (ctrl == 2)          }
 * catch (Exception e) {                  Preferences.debug("function error: " + e.getMessage() + "\n");          }
 *     return; }*/


/**
 * Starts the analysis.
 */
/*public void driver() {
 *  super.driver();}*/


/**
 * Display results of displaying exponential fitting parameters.
 */
/*public void dumpResults() {
 *   Preferences.debug(" ******* FitDoubleExponential ********* \n\n");  Preferences.debug("Number of iterations: " +
 * String.valueOf(iters) + "\n");  Preferences.debug("Chi-squared: " + String.valueOf(getChiSquared()) + "\n");
 * Preferences.debug("a0 " + String.valueOf(a[0]) + "\n");  Preferences.debug("a1 " + String.valueOf(a[1]) + "\n");
 * Preferences.debug("a2 " + String.valueOf(a[2]) + "\n"); }}*/

/* This code required a port of FORTRAN to JAVA.  FORTRAN always uses call by reference
   in its subroutines, while JAVA uses call by value for its primitives, such as
   doubles and integers, and call by reference for other structures.  There are
   2 ways to handle the conversion of primitive calls.  When a subroutine argument
   always uses the same variable, the variable can simply be made global.  If a
   subroutine argument uses different variables, then use a one element matrix
   for that argument.  Before the subroutine call assign the original variable value to
   the matrix argument.  If the argument is changed in the subroutine, make sure
   the matrix argument is assigned its value before return.  After the subroutine
   executes, assign the matrix value back to the original variable.

   FORTRAN also uses goto statements while JAVA does not.  This is handled with
   a combination of while, do while, and switch statements with continue and break.
   Note that the FORTRAN cycle is equivalent to the JAVA continue while the FORTRAN
   exit is equivalent to the JAVA break.

   The FORTRAN sign transfer function SIGN(X,Y) takes the sign of the second
   argument and puts it on the first argument, ABS(X) if Y >= 0 and -ABS(X) if
   Y < 0.

   FORTRAN used 1 based indexing for arrays while JAVA uses 0 based indexing
   for arrays.  Variables that are assigned a 0 in FORTRAN to indicate that no
   array variable has been selected must be assigned a -1 in JAVA.  Statements
   that assign a matrix rank equal to a matrix index in FORTRAN must assign the
   matrix rank equal to the matrix index + 1 in JAVA.

   The routine releps was the one routine that could not be derived from the
   FORTRAN 90 elsunc.f90 file.  This routine had to be obtained from the
   FORTRAN 77 dbluchelp.f file. */

public abstract class NLConstrainedEngine {

    //~ Static fields/initializers -------------------------------------------------------------------------------------

    /** DOCUMENT ME! */
    private static final double big = 1.0E32;

    //~ Instance fields ------------------------------------------------------------------------------------------------

    /** 1D array of length param containing the unknown parameters. */
    /** On completion if exitStatus >= 0 a[] contains the latest (best) estimate of the solution point. */
    protected double[] a;

    /**
     * absolute convergence constant Used to test for convergence by the sum of squares being less than the
     * absoluteConvergence squared.
     */
    protected double absoluteConvergence;

    /** DOCUMENT ME! */
    protected double[] bl;

    /**
     * integer scalar code for assessing the bounds bounds = 0 means an unconstrained problem bounds = 1 means the same
     * lower bounds for all unknowns and the same upper bounds for all unknowns. The lower and upper bound must be
     * stored in bl[0] and bu[0] respectively. bounds = 2 means that the bounds must be supplied by the user in bl[i]
     * and bu[i] for i = 0,1, param - 1.
     */
    protected int bounds;

    /** DOCUMENT ME! */
    protected double[] bu;

    /**
     * 2D array of doubles of dimension nPts by max(4,param) containing the main part of the covariance matrix
     * (Jtranspose * J)inverse, where J is the Jacobian matrix computed at a.
     */
    protected double[][] covarMat;

    /** Wrapper for ctrl or lctrl communication with driver. */
    protected int[] ctrlMat = new int[1];

    /** DOCUMENT ME! */
    protected double dyda[], stdv;

    /** DOCUMENT ME! */
    protected double[] gues;

    /**
     * If true, the columns of the Jacobian are scaled to have unit length. If false, internal scaling is not used to
     * compute the Gauss-Newton search direction.
     */
    protected boolean internalScaling = false;

    /** integer scalar containing the number of iterations until termination. */
    protected int iters;

    /** DOCUMENT ME! */
    protected int maxIterations;

    /** integer scalar containing the number of data points. */
    protected int nPts;

    /** variables integer scalar containing the number of unknowns. */
    protected int param;

    /**
     * parameter convergence constant Used to test for convergence by the relative change in a[] being less than the
     * parameterConvergence.
     */
    protected double parameterConvergence;

    /**
     * relative convergence constant Used to test for convergence by reduction in objective function being less than
     * relativeConvergence squared.
     */
    protected double relativeConvergence;

    /** 1D array of doubles of dimension nPts containing the value of the residuals at the termination point a;. */
    protected double[] residuals;

    /**
     * The method of Newton is allowed at the end of the iteration in some situations (large residuals). If true, the
     * use of second derivatives is permitted
     */
    protected boolean secondAllowed = true;

    /** pseudo rank tolerance constant used to determine the pseudo rank of the Jacobian J. */
    protected double tolerance;

    /** DOCUMENT ME! */
    protected double[] xSeries;

    /** DOCUMENT ME! */
    protected double[] ySeries;

    /** DOCUMENT ME! */
    private double aDiff;

    /** DOCUMENT ME! */
    private double alfk;

    /**
     * The orginal code introduces a alfkm1 and alfkm2 in the common block preunc and another alfkm1 and alfkm2 in the
     * subroutine lineuc. Here the alfkm1 and alfkm2 in lineuc are called alfkm1a and alfkm2a.
     */
    private double alfkm1a;

    /** DOCUMENT ME! */
    private double alfkm2a;

    /** DOCUMENT ME! */
    private double alfkp1;

    /** DOCUMENT ME! */
    private double alfmax;

    /** DOCUMENT ME! */
    private double alfmin;

    /** DOCUMENT ME! */
    private double alfnoi;

    /** DOCUMENT ME! */
    private double alpha;

    /** DOCUMENT ME! */
    private double alphup;

    /**
     * 1D integer array of length param containing a code which indicates whether an unknown is active or not aset[i] =
     * 0 when a[i] is free = +1 when a[i] == bl[i] = -1 when a[i] == bu[i].
     */
    private int[] aset;

    /**
     * scalar containing the norm of the orthogonal projection of residuals onto the space spanned by the columns of the
     * Jacobian.
     */
    private double beta;

    /**
     * The original code introudces a beta in lsunc and another beta in subroutine lineuc. Here the beta in lineuc is
     * called beta2
     */
    private double beta2;

    /** DOCUMENT ME! */
    private double betkm2, d1km2, fsqkm2, dxnkm2, alfkm2, aupkm2, betkm1, d1km1, fsqkm1, dxnkm1, alfkm1, aupkm1;

    /** DOCUMENT ME! */
    private double bn;

    /** DOCUMENT ME! */
    private double cnorm;

    /**
     * ctrl can have 3 different values on entry. If ctrl = 1 or ctrl = -1, compute the residuals for all nPts at a
     * certain point a and return the residuals = yfit - yseries in the array residuals if the functions are computable.
     * If not successful and ctrl was set to 1, set ctrl = -1. If not successful and ctrl was set to -1, set ctrl < -10.
     * If ctrl = 2 and the Jacobian is supplied analytically, calculate the Jacobian = Jij = dymodi/daj. The Jacobian is
     * placed in matrix covarMat. If the Jacobian cannot be calculated, set ctrl < -10. If you wish to compute the
     * Jacobian numerically on return, set ctrl = 0.
     */
    private int ctrl;

    /** scalar containing the predicted reduction in the objective function if gn-direction is used. */
    private double d1sqs;

    /**
     * 1D double array of length param conatiaing the diagonal elements of the diagonal matrix D if internalScaling is
     * true. Otherwise, undefined.
     */
    private double[] diag;

    /** DOCUMENT ME! */
    private double diff;

    /** double working areas 1D array of length param containing the solution of J * dx = -residuals. */
    private double[] dx;

    /** DOCUMENT ME! */
    private double dxnorm;

    /** DOCUMENT ME! */
    private double eps;

    /** DOCUMENT ME! */
    private double epsl;

    /** DOCUMENT ME! */
    private int errorStatus;

    /** DOCUMENT ME! */
    private int eval;

    /**
     * integer scalar that indicates why the return was taken. If exitStatus != -1 and exitStatus >= -10, then a[]
     * contains the latest (best) estimate of the solution point. = 1000X convergence because relative predicted
     * reduction in the objective function = (0.5 * chisquare) is less than (relativeConvergence)**2 = 200X convergence
     * because sum of squares is less than (absoluteConvergence)**2 = 30X convergence because the relative change in a
     * is less than parameterConvergence = 4X convergence because we are computing at noise level X, the last digit in
     * the convergence code, indicates how the last steps were computed X = 0 no trouble (Gauss-Newton the last 3 steps)
     * = 1 pseudoRank <> param at termination point = 2 The method of Newton was used (at least) in the last step = 3
     * The (last - 2) step was subspace minimization, but the last 2 steps were Gauss-Newton steps = 4 The steplength
     * was not unit in both the last 2 steps < 0 indicates that no convergence criterion is fulfilled, but some abnormal
     * termination criterion is satisfied = -1 Invalid starting condition: nPts < param or param <= 0 or nPts <= 0 or
     * maxIterations <= 0 or relativeConvergence < 0 or relativeConvergence < 0 or absoluteConvergence < 0 or tolerance
     * <= 0 or parameterConvergence < 0 or invalid starting point on entry = -2 Number of iterations has exceeded
     * maximum allowed iterations = -3 The Hessian emanating from the second order method is not positive definite = -4
     * The algorithm would like to use second derivatives but is not allowed to do that = -5 An undamped step with
     * Newton's method is a failure = -6 The latest search direction computed using subspace minimization was not a
     * descent direction (probably caused by wrongly computed Jacobian) = -7 There is only one feasible point, name a[i]
     * = bl[i] = bu[i]; i = 0,1,...,param-1 < -10 Termination due to user stop indicator
     */
    private int exitStatus;

    /** DOCUMENT ME! */
    private double fsum;

    /** integer scalar containing the total number of function evaluations done inside this routine. */
    private int funcEval;

    /** 1D array of length param containing the gradient of the objective at the current point. */
    private double[] g;

    /**
     * param by param array If secondAllowed if false, gmat can be used as a singly subscripted array of dimension param
     * by setting all second indices to 0.
     */
    private double[][] gmat;

    /** DOCUMENT ME! */
    private boolean gndok;

    /** scalar containing norm of the gradient divided by norm of Jacobian. */
    private double gnorm;

    /** DOCUMENT ME! */
    private int i, j, k;

    /** Variables used for restart information. */
    private int icount, lattry, itotal, irank, imax, ival;

    /** DOCUMENT ME! */
    private int iev;

    /** DOCUMENT ME! */
    private int ifree, indic;

    /** DOCUMENT ME! */
    private int info;

    /**
     * integer scalar containing the number of function evaluations caused by computing Jacobians with difference
     * methods.
     */
    private int jacobianEval;

    /** DOCUMENT ME! */
    private int k2;

    /** DOCUMENT ME! */
    private int kod;

    /** integer scalar containing the number of function evaluations caused by the linesearch algorithm. */
    private int lineEval;

    /** Leading dimension of array gmat mdg must be >= param if secondAllowed is true. */
    private int mdg;

    /** DOCUMENT ME! */
    private double noise;

    /** Number of active constraints. */
    private int paramAct;

    /** DOCUMENT ME! */
    private double pbeta;

    /** DOCUMENT ME! */
    private double pgb1;

    /** DOCUMENT ME! */
    private double pgb2;

    /** double scalar containging the value of the objective function = (0.5 * chisquare) at the termination point a. */
    private double phi;

    /** DOCUMENT ME! */
    private double phik;

    /** DOCUMENT ME! */
    private double phikm1;

    /** DOCUMENT ME! */
    private double phikp1;

    /** DOCUMENT ME! */
    private double philat, bestpg;

    /** DOCUMENT ME! */
    private double[] phiMat = new double[1];

    /**
     * array of integers of dimension param containing the permutation matrix E in the decomposition QTranspose * J * D
     * E = (R) (0) pivit[i] contains the index of the column that was moved into column i.
     */
    private int[] pivit;

    /** DOCUMENT ME! */
    private double pk;

    /** DOCUMENT ME! */
    private double pMinrm, qMinrm, deltaMinrm, a1div3;

    /** DOCUMENT ME! */
    private double predb;

    /** scalar containing the predicted reduction in the objective if pseudoRank from previous step is used. */
    private double prekm1;

    /**
     * integer scalar containing the pseudo rank of the matrix jhat at the termination point a. jhat is the Jacobian
     * matrix with columns corresponding to active constraints deleted
     */
    private int pseudoRank;

    /** Create wrapper for routine gndunc. */
    private int[] pseudoRankMat = new int[1];

    /** DOCUMENT ME! */
    private double rabs;

    /** DOCUMENT ME! */
    private int rank;

    /** DOCUMENT ME! */
    private boolean reduce;

    /** DOCUMENT ME! */
    private boolean restart;

    /** DOCUMENT ME! */
    private double rlenb;

    /** DOCUMENT ME! */
    private double rmin;

    /**
     * The suffices km2 and km1 in the names of the variables represent time step k-2 and k-1 respectively. These
     * variables are updated only inside the routine evreuc.
     */
    private int rngkm2, kodkm2, rngkm1, kodkm1;

    /** DOCUMENT ME! */
    private double rootsp, bl0, bu0;

    /** DOCUMENT ME! */
    private int secind;

    /**
     * integer scalar containing the number of function evaluations caused by using second derivatives in the method of
     * Newton.
     */
    private int secondEval;

    /** DOCUMENT ME! */
    private double smax;

    /** DOCUMENT ME! */
    private double sn;

    /** double scalar containing an estimate of the linear convergence factor. */
    private double speed;

    /** single relative precision. */
    private double srelpr;

    /** DOCUMENT ME! */
    private double stepb;

    /**
     * 1D array of length nPts containing the orthogonal projection of residuals onto space spanned by the frist
     * pseudoRank linear independent columns of the Jacobian.
     */
    private double[] v;

    /** DOCUMENT ME! */
    private double v0norm, v1norm, v2norm, scv0v1, scv0v2, scv1v2;

    /** DOCUMENT ME! */
    private double[] w0;

    /** DOCUMENT ME! */
    private double[] w1;

    /** DOCUMENT ME! */
    private double[] w2;

    /** DOCUMENT ME! */
    private double[] w3;

    /** DOCUMENT ME! */
    private double x1Minrm, x2Minrm, x3Minrm;
    
    protected boolean outputMes = true;

    //~ Constructors ---------------------------------------------------------------------------------------------------

    /**
     * NLConstrainedEngine - non-linear fit to a function.
     *
     * @param  nPts     number of points to fit to the function to
     * @param  param    number of parameters of function
     * @param  xSeries  DOCUMENT ME!
     * @param  yData    DOCUMENT ME!
     */
    public NLConstrainedEngine(int nPts, int param, double[] xSeries, float[] yData) {

        try {
            this.nPts = nPts;
            this.param = param;
            this.xSeries = xSeries;
            ySeries = new double[nPts];

            for (int i = 0; i < nPts; i++) {
                ySeries[i] = (double) yData[i];
            }

            maxIterations = 20 * param;
            bl = new double[param];
            bu = new double[param];
            residuals = new double[nPts];
            pivit = new int[param];
            aset = new int[param];
            dx = new double[param];
            g = new double[param];
            w0 = new double[param];
            w1 = new double[nPts];
            w2 = new double[nPts];
            w3 = new double[param];
            v = new double[nPts];
            gmat = new double[param][param];

            // compute srelpr single relative precision
            releps();
            rootsp = Math.sqrt(srelpr);

            mdg = param;
            tolerance = rootsp;
            relativeConvergence = 10.0 * rootsp;
            absoluteConvergence = srelpr;
            parameterConvergence = 10.0 * rootsp;

            a = new double[param];
            gues = new double[param];

            int covar2 = Math.max(4, param);
            covarMat = new double[nPts][covar2];
        } catch (OutOfMemoryError error) { }
    }
    
    /**
     * NLConstrainedEngine - non-linear fit to a function.
     *
     * @param  nPts     number of points to fit to the function to
     * @param  param    number of parameters of function
     * @param  xSeries  DOCUMENT ME!
     * @param  yData    DOCUMENT ME!
     */
    public NLConstrainedEngine(int nPts, int param, double[] xSeries, double[] yData) {

        try {
            this.nPts = nPts;
            this.param = param;
            this.xSeries = xSeries;
            ySeries = new double[nPts];

            for (int i = 0; i < nPts; i++) {
                ySeries[i] = yData[i];
            }

            maxIterations = 20 * param;
            bl = new double[param];
            bu = new double[param];
            residuals = new double[nPts];
            pivit = new int[param];
            aset = new int[param];
            dx = new double[param];
            g = new double[param];
            w0 = new double[param];
            w1 = new double[nPts];
            w2 = new double[nPts];
            w3 = new double[param];
            v = new double[nPts];
            gmat = new double[param][param];

            // compute srelpr single relative precision
            releps();
            rootsp = Math.sqrt(srelpr);

            mdg = param;
            tolerance = rootsp;
            relativeConvergence = 10.0 * rootsp;
            absoluteConvergence = srelpr;
            parameterConvergence = 10.0 * rootsp;

            a = new double[param];
            gues = new double[param];

            int covar2 = Math.max(4, param);
            covarMat = new double[nPts][covar2];
        } catch (OutOfMemoryError error) { }
    }

    //~ Methods --------------------------------------------------------------------------------------------------------

    /**
     * fitToFunction communicates with 3 protected variables param, nPts, and ctrlMat ctrlMat is used as a wrapper for
     * ctrl or lctrl. Evaluates the residuals or the Jacobian at a certain a[]
     *
     * @param  a          DOCUMENT ME!
     * @param  residuals  DOCUMENT ME!
     * @param  covarMat   DOCUMENT ME!
     */
    public abstract void fitToFunction(double[] a, double[] residuals, double[][] covarMat);

    /**
     * driver.
     */
    public void driver() {

        try {

            for (i = 0; i < param; i++) {
                a[i] = gues[i];
            }

            if (internalScaling) {
                diag = new double[param];
            }

            exitStatus = 0;

            // Initialize the bl and bu arrays
            bl0 = -big;
            bu0 = big;

            if (bounds == 1) {
                bl0 = bl[0];
                bu0 = bu[0];
            }

            if ((bounds == 0) || (bounds == 1)) {

                for (i = 0; i < param; i++) {
                    bl[i] = bl0;
                    bu[i] = bu0;
                }
            }

            // call lsunc to minimize
            lsunc();

            if (exitStatus < 0) {
                Preferences.debug("exitStatus after lsunc() call = " + exitStatus + "\n");

                return;
            }

            if (((exitStatus % 10) == 1) && outputMes) {
                System.out.println("Pseudo-rank of the Jacobian is not full\n");

                if (!internalScaling) {
                    System.out.println("A run with internalScaling = true might " + "give another solution\n");
                }

                Preferences.debug("Pseudo-rank of the Jacobian is not full\n");

                if (!internalScaling) {
                    Preferences.debug("A run with internalScaling = true might " + "give another solution\n");
                }
            }

            covar();

            if (!internalScaling) {
                return;
            }

            // COMPENSATE FOR THE INTERNAL SCALING BY FORMING
            // covarMat = D*covarMat*D
            // WHERE D IS A DIAGONAL MATRIX

            for (j = 0; j < param; j++) {

                for (k = 0; k < param; k++) {
                    covarMat[k][j] = covarMat[k][j] * diag[j];
                } // for (k = 0; k < param; k++)
            } // for (j = 0; j < param; j++)

            for (i = 0; i < param; i++) {

                for (k = 0; k < param; k++) {
                    covarMat[i][k] = covarMat[i][k] * diag[i];
                } // for (k = 0; k < param; k++)
            } // for (i = 0; i < param; i++)

            return;
        } catch (Exception e) {
            Preferences.debug("driver error: " + e.getMessage());
        }
    }

    /**
     * getChiSquared - accessor to chi-squared value (goodness-of-fit measure).
     *
     * @return  the value of chi squared
     */
    public double getChiSquared() {
        return 2.0 * phi;
    }

    /**
     * getParameters accessor to function parameters.
     *
     * @return  the function parameters determined by the algorithm
     */
    public double[] getParameters() {
        return a;
    }

    /**
     * DOCUMENT ME!
     *
     * @return  double[] residuals
     */
    public double[] getResiduals() {
        return residuals;
    }

    /**
     * DOCUMENT ME!
     */
    private void analuc() {
        // CHECK IF PREVIOUS STEP WAS SUFFICIENTLY GOOD AND DECIDE IF THE SEARCH
        // DIRECTION (DX=GAUSS-NEWTON DIRECTION) FOR CURRENT STEP SHALL BE RECOMPUTED

        // ON ENTRY

        // iters  INTEGER SCALAR CONTAINING ITERATION COUNT
        // restart  LOGICAL SCALAR = FALSE IF THIS IS NOT A RESTART STEP
        // = TRUE IF THIS IS A RESTART STEP
        // kod     INTEGER SCALAR CONTAINING A CODE THAT SAYS HOW THE
        // PREVIOUS SEARCH DIRECTION WAS COMPUTED
        // = 1 IF GAUSS-NEWTON DIRECTION
        // = -1 IF SUBSPACE DIRECTION
        // = -2 IF NEWTON DIRECTION
        // fsum    double SCALAR CONTAINING SUM OF SQUARES AT CURRENT POINT
        // disqs   double SCALAR CONTAINING PREDICTED REDUCTION IN OBJECTIVE
        // FUNCTION IF GAUSS-NEWTON DIRECTION IS USED
        // beta    double SCALAR CONTAINING THE NORM OF THE ORTHOGONAL
        // PROJECTION OF residuals ONTO THE COLUMN SPACE OF THE JACOBIAN J
        // PREKM1  double SCALAR CONTAINING
        // THE SAME AS BETA IF ABS(KOD)=2
        // IF ABS(KOD)=1 IT CONTAINS THE PROJECTION MENTIONED
        // IN THE EXPLANATION OF BETA BUT NOT NECESSARILY ONTO THE
        // FULL COLUMN SPACE OF THE JACOBIAN. THE DIMENSION OF THE
        // SPACE IS EQUAL TO 1 LESS THE DIMENSION OF THE SPACE WHERE
        // MINIMIZATION WAS DONE IN THE PREVIOUS STEP
        // fitToFunction SUBROUTINE NAME USED TO EVALUATE THE VECTOR OF RESIDUALS
        // AT A CERTAIN POINT
        // a[]     double 1D ARRAY OF DIMENSION param CONTAINING
        // THE CURRENT POINT
        // dx[]    DOUBLE 1D ARRAY OF DIMENSION param CONTAINING
        // CURRENT GAUSS-NEWTON SEARCH DIRECTION
        // diag[]  REAL 1D ARRAY OF DIMENSION param CONTAINING THE
        // DIAGONAL ELEMENTS OF THE DIAGONAL MATRIX D FROM (1) BELOW
        // w0      DOUBLE 1D ARRAY OF DIMENSION param CONTAINING
        // INFORMATION NEEDED TO FORM MATRIX Q IN THE QR-
        // DECOMPOSITION OF THE JACOBIAN OF residuals(a)
        // pivit   INTEGER SINGLY SUBSCRIPTED ARRAY OF DIMENSION param CONTAINING
        // THE PERMUTATION MATRIX E FROM (1) BELOW
        // FOR MORE EXACT EXPLANATION OF THE ARRAYS w0 AND pivit SEE THE
        // EXPLANATION OF ARRAYS w0 AND JPVT ON RETURN FROM THE
        // LINPACK ROUTINE SQRDC WHICH HAS BEEN USED TO MAKE THE
        // QR-DECOMPOSITION OF MATRIX J*D

        // param  INTEGER SCALAR CONTAINING LENGTH OF ARRAYS a,dx,diag,and pivit
        // pseudoRank INTEGER SCALAR CONTAINING THE PSEUDO RANK USED WHEN
        // CURRENT GAUSS-NEWTON SEARCH DIRECTION WAS COMPUTED
        // internalScaling
        // = false IF SCALING OF JACOBIAN J WAS DONE BEFORE QR-DECOMP.
        // = true IF SCALING WAS DONE
        // residuals[] DOUBLE 1D ARRAY OF DIMENSION nPts CONTAINING
        // THE VECTOR OF RESIDUALS AT CURRENT POINT
        // v[]    DOUBLE 1D ARRAY OF DIMENSION nPts CONTAINING
        // THE ORTHOGONAL PROJECTION OF residuals ONTO THE SPACE SPANNED BY
        // THE COLUMNS OF THE JACOBIAN J
        // covarMat DOUBLE 2D ARRAY OF DIMENSION nPts*param
        // CONTAINING THE MATRIX R FROM THE
        // DECOMPOSITION    T
        // Q *J*D*E = (R)
        // (0)           (1)
        // IN THE UPPER TRIANGLE OF covarMat
        // WHERE
        // Q IS ORTHOGONAL (nPts*nPts)
        // J IS THE JACOBIAN (nPts*param) AT THE TERMINATING POINT
        // D IS A DIAGONAL MATRIX (param*param)
        // E IS A PERMUTATION MATRIX (param*param)
        // R IS AN UPPER TRIANGULAR MATRIX (param*param)
        // nPts     INTEGER SCALAR CONTAINING THE LENGTH OF THE ARRAYS residuals,v,w1
        // mdg      INTEGER SCALAR CONTAINING LEADING DIMENSION OF ARRAY gmat.
        // MDG MUST BE >= N
        // secondAllowed LOGICAL SCALAR = TRUE IF THE USER HAS ALLOWED USE OF
        // 2:ND DERIVATES. FALSE OTHERWISE
        // paramAct INTEGER SCALAR CONTAINING THE NO. OF ACTIVE CONSTRAINTS
        // (POSITIVE OR NEGATIVE)
        // aset[] INTEGER 1D ARRAY OF DIMENSION param
        // CONTAINING +1 OR -1 TO INDICATE AN ACTIVE BOUND (OTHERWISE ZERO)

        // ON RETURN

        // restart  IS SET TO FALSE
        // kod      CONTAINS A CODE THAT INDICATES HOW THE SEARCH DIRECTION
        // CONTAINED IN ARRAY dx ON RETURN IS COMPUTED
        // = 1 IF THE DIRECTION IN dx ON ENTRY IS ACCEPTED
        // = -1 IF MINIMIZATION IN SUBSPACE HAS BEEN USED
        // TO COMPUTE DX
        // = -2 IF THE METHOD OF NEWTON HAS BEEN USED TO COMPUTE dx
        // paramAct IS PUT EQUAL TO -paramAct-1 IF paramAct < 0 ON ENTRY
        // errorStatus INTEGER SCALAR CONTAINING -3 IF COEFFICIENT MATRIX IN
        // SYSTEM ARISING FROM SECOND DERIVATIVE METHOD IS NOT
        // POSITIVE DEFINITE
        // = -4 IF THE ALGORITHM WOULD LIKE TO USE SECOND DERIVATIVES
        // BUT IS NOT ALLOWED TO DO THAT
        // < -10 AS A USER STOP INDICATOR
        // OTHERWISE errorStatus = 0 ON RETURN
        // eval       INTEGER SCALAR CONTAINING NO. OF FUNCTION EVALUATIONS
        // DONE INSIDE THIS ROUTINE

        // IF KOD=1 ON RETURN ALL THE OTHER INPUT PARAMETERS ARE UNCHANGED
        // IF KOD=-1 OR ABS(KOD)=2 THE FOLLOWING PARAMATERS ARE
        // CHANGED ON RETURN

        // dx[]    CONTAINS THE SUGGESTED SEARCH DIRECTION
        // d1sqs   CONTAINS PREDICTED REDUCTION IN OBJECTIVE IF SUGGESTED
        // SEARCH DIRECTION IS USED
        // dxnorm  CONTAINS THE NORM OF THE SUGGESTED SEARCH DIRECTION
        // pseudoRank CONTAINS THE DIMENSION OF THE SUBSPACE USED TO COMPUTE
        // SEARCH DIRECTION OR (IF ABS(kod)=2 ON RETURN) pseudoRank = -param
        // v[]     CONTAINS THE ORTHOGONAL PROJECTION OF residuals ONTO THE SPACE
        // SPANNED BY THE pseudoRank FIRST LINEAR INDEPENDENT COLUMNS
        // OF THE JACOBIAN J  (IF kod = -1 ON RETURN)
        // UNCHANGED IF kod = 2 ON RETURN
        // CONTAINS THE VECTOR OF RESIDUALS IF kod = -2 ON RETURN
        // covarMat[][] UNCHANGED IF kod = -1 ON RETURN
        // COMPLETELY DESTROYED IF ABS(kod)=2 ON RETURN
        // w0      UNCHANGED IF kod=-1 ON RETURN
        // COMPLETELY DESTROYED IF ABS(kod)=2 ON RETURN

        eval = 0;

        // SET gndok = true IF SEARCH DIRECTION CONTAINED IN dx ON ENTRY IS ACCEPTED

        // SET secind =
        // -2 IF NEWTON METHOD
        // -1 IF SUBSPACE MINIMIZATION IS THE ALTERNATIVE
        // FOR RECOMPUTATION OF dx

        gnavuc();

        if (gndok) {

            // SEARCH DIRECTION CONTAINED IN dx ON ENTRY IS ACCEPTED
            kod = 1;
            errorStatus = 0;
            restart = false;

            return;
        } // if (gndok)

        // RECOMPUTE SEARCH DIRECTION

        if (Math.abs(secind) != 2) {
            // USE MINIMIZATION IN SUBSPACE TO RECOMPUTE

            subuc();
            kod = -1;

            if (rank == pseudoRank) {
                kod = 1;
            }

            pseudoRank = rank;
            errorStatus = 0;
            restart = false;

            return;
        } // if (Math.abs(secind) != 2)

        // USE 2:ND DERIVATIVES TO RECOMPUTE IF WE ARE ALLOWED TO

        if (!secondAllowed) {
            errorStatus = -4;
            kod = secind;
        } else {
            secuc();
            kod = secind;
            pseudoRank = -(param - paramAct);
            dxnorm = dnrm2(param, dx, 1);
        } // else

        restart = false;

        return;
    }

    /**
     * DOCUMENT ME!
     *
     * @param  internalScaling  DOCUMENT ME!
     * @param  work             DOCUMENT ME!
     */
    private void btrunc(boolean internalScaling, double[] work) {
        // BACKTRANSFORM SO THAT   DY:=D*E*DX
        // WHERE DX IS A param-VECTOR
        // E IS A PERMUTATION MATRIX param*param
        // D IS A DIAGONAL MATRIX param*param

        // ON ENTRY

        // DX[] CONTAINS THE VECTOR TO BE TRANSFORMED IN THE ELEMENTS
        // DX[I] I=0,1...,pseudoRank-1
        // IF pseudoRank < param  THEN DX[pseudoRank+J] :=0
        // J=0,1,...(Nparam-pseudoRank-1) BEFORE TRANSFORMATION
        // DIAG[] CONTAINS THE DIAGONAL ELEMENTS OF DIAGONAL MATRIX D
        // IF internalScaling = true. OTHERWISE DIAG[] IS UNDEFINED
        // pivit[] CONTAINS THE PERMUTATION MATRIX E STORED IN A SPECIAL
        // WAY (SEE LINPACK ROUTINE SQRDC)
        // internalScaling = false IF MATRIX D IS THE UNIT MATRIX
        // = true IF SCALING WAS DONE BY MATRIX D

        // ON RETURN

        // DX[] CONTAINS THE TRANSFORMED VECTOR DY ABOVE

        int i;

        if (param == 0) {
            return;
        }

        if (pseudoRank != param) {

            for (i = pseudoRank; i < param; i++) {
                dx[i] = 0.0;
            }
        }

        // DO THE PIVOTING

        pivec(work);

        // EVENTUALLY RESCALE

        if (!internalScaling) {
            return;
        }

        for (i = 0; i < param; i++) {
            dx[i] = diag[i] * dx[i];
        }

        return;
    }

    /**
     * DOCUMENT ME!
     *
     * @param  dphize  DOCUMENT ME!
     */
    private void chder(double dphize) {
        // MAKE A CONSISTENCY CHECK OF THE DERIVATIVE APPROXIMATION
        // BASED ON THE JACOBIAN MATRIX
        // THE FUNCTION UNDER CONCERN IS
        // PHI(ALPHA) = F(a+ALPHA*dx)

        // ON ENTRY

        // DPHIZE THE DERIVATIVE OF PHI(ALPHA) AT ALPHA=0 COMPUTED
        // BY dx(TR)*G(a)    WHERE G(a) IS THE GRADIENT OF residuals[a]
        // PHIZER PHI(0)
        // a[] CONTAINS THE STARTING POINT OF THE LINESEARCH
        // dx[] CONTAINS THE SEARCH DIRECTION
        // nPts NO. OF RESIDUALS IN THE VECTOR VALUED FUNCTION residuals[a]
        // param NO. OF UNKNOWNS
        // k2 NO. OF FUNCTION EVALUATIONS DONE SO FAR IN THE LINESEARCH
        // errorStatus = -2
        // alfk THE alpha VALUE FOR WHICH THE DIFFERENCES ARE COMPUTED
        // phik phi(alfk)

        // ON RETURN

        // k2 INCREASED BY 1 OR SET TO < -10 AS A USER STOP INDICATOR
        // errorStatus SET = -1 IF INCONSISTENCY IS DETECTED

        // WORKING AREAS

        // xnew[param]
        // fnew[nPts]

        double phimk, dphifo, dphiba, dphice, maxdif;
        double[] xnew = new double[param];
        double[] fnew = new double[nPts];

        // COMPUTE phi(-alfk)

        ctrl = -1;
        fsumsq(-alfk, xnew, fnew, phiMat);
        phimk = phiMat[0];

        if (ctrl < -10) {
            k2 = ctrl;
        }

        if (k2 < 0) {
            return;
        }

        k2++;

        // COMPUTE APPROXIMATIONS OF THE DERIVATIVE BY USING FORWARD,
        // BACKWARD AND CENTRAL DIFFERENCES

        if (alfk <= 0.0) {
            errorStatus = -1;

            return;
        }

        dphifo = (phik - phi) / alfk;
        dphiba = (phi - phimk) / alfk;
        dphice = (phik - phimk) / 2.0 / alfk;
        maxdif = Math.abs(dphifo - dphiba);
        maxdif = Math.max(maxdif, Math.abs(dphifo - dphice));
        maxdif = Math.max(maxdif, Math.abs(dphiba - dphice));

        if ((Math.abs(dphifo - dphize) > maxdif) && (Math.abs(dphice - dphize) > maxdif)) {
            errorStatus = -1;
        }

        return;
    }

    /**
     * DOCUMENT ME!
     *
     * @param  xmin  DOCUMENT ME!
     * @param  v2    DOCUMENT ME!
     */
    private void choose(double xmin, double[] v2) {
        // X1Minrm, X2Minrm AND X3Minrm ARE THREE REAL ROOTS OF A 3:RD DEGREE POLYNOMIAL WHICH TENDS TO MINUS INFINITY
        // WHEN X TENDS TO MINUS INFINITY. CHOOSE ONE OF THE OUTER ROOTS FOR alfkp1. beta2 = THE OTHER OUTER ONE. pbeta
        // = THE VALUE OF THE CORRESPONDING 4:TH DEGREE POLYNOMIAL AT beta2.

        double x, y, z;

        y = x1Minrm;
        x = Math.min(x1Minrm, Math.min(x2Minrm, x3Minrm));
        z = Math.max(x1Minrm, Math.max(x2Minrm, x3Minrm));

        if ((x1Minrm <= x2Minrm) && (x1Minrm <= x3Minrm)) {
            y = Math.min(x2Minrm, x3Minrm);
        }

        if ((x2Minrm <= x1Minrm) && (x2Minrm <= x3Minrm)) {
            y = Math.min(x1Minrm, x3Minrm);
        }

        if ((x3Minrm <= x1Minrm) && (x3Minrm <= x2Minrm)) {
            y = Math.min(x1Minrm, x2Minrm);
        }

        if (xmin <= y) {
            alfkp1 = x;
            beta2 = z;
        } else {
            alfkp1 = z;
            beta2 = x;
        }

        pbeta = pol4(v2, beta2);

        return;
    }

    /**
     * DOCUMENT ME!
     */
    private void covar() {
        int i, ii, j, jj, k, km1, l;
        boolean sing;
        double temp, tolr;

        // FORM THE INVERSE OF covarMat IN THE FULL UPPER TRIANGLE OF covarMat.

        tolr = tolerance * Math.abs(covarMat[0][0]);
        l = 0;

        for (k = 0; k < param; k++) {

            if (Math.abs(covarMat[k][k]) <= tolr) {
                break;
            }

            covarMat[k][k] = 1.0 / covarMat[k][k];
            km1 = k - 1;

            for (j = 0; j <= km1; j++) {
                temp = covarMat[k][k] * covarMat[j][k];
                covarMat[j][k] = 0.0;

                for (i = 0; i <= j; i++) {
                    covarMat[i][k] = covarMat[i][k] - (temp * covarMat[i][j]);
                } // for (i = 0; i <= j; i++)
            } // for (j = 0; j <= km1; j++)

            l = k;
        } // for (k = 0; k < param; k++)

        // FORM THE FULL UPPER TRIANGLE OF THE INVERSE OF
        // (covarMat TRANSPOSE)* covarMat
        // IN THE FULL UPPER TRIANGLE OF covarMat.

        for (k = 0; k <= l; k++) {
            km1 = k - 1;

            for (j = 0; j <= km1; j++) {
                temp = covarMat[j][k];

                for (i = 0; i <= j; i++) {
                    covarMat[i][j] = covarMat[i][j] + (temp * covarMat[i][k]);
                } // for (i = 0; i <= j; i++)
            } // for (j = 0; j <= km1; j++)

            temp = covarMat[k][k];

            for (i = 0; i <= k; i++) {
                covarMat[i][k] = temp * covarMat[i][k];
            } // for (i = 0; i <= k; i++)
        } // for (k = 0; k <= l; k++)

        // FORM THE FULL LOWER TRIANGLE OF THE COVARIANCE MATRIX
        // IN THE STRICT LOWER TRIANGLE OF covarMat AND IN dx

        for (j = 0; j < param; j++) {
            jj = pivit[j];
            sing = (j > l);

            for (i = 0; i <= j; i++) {

                if (sing) {
                    covarMat[i][j] = 0.0;
                }

                ii = pivit[i];

                if (ii > jj) {
                    covarMat[ii][jj] = covarMat[i][j];
                }

                if (ii < jj) {
                    covarMat[jj][ii] = covarMat[i][j];
                }
            } // for (i = 0; i <= j; i++)

            dx[jj] = covarMat[j][j];
        } // for (j = 0; j < param; j++)

        // SYMMETRIZE THE COVARIANCE MATRIX IN covarMat.

        for (j = 0; j < param; j++) {

            for (i = 0; i <= j; i++) {
                covarMat[i][j] = covarMat[j][i];
            } // for (i = 0; i <= j; i++)

            covarMat[j][j] = dx[j];
        } // for (j = 0; j < param; j++)

        return;
    }

    /**
     * DOCUMENT ME!
     */
    private void dimsub() {
        // FIND APPROPRIATE DIMENSION OF SUBSPACE WHERE MINIMIZATION SHALL BE DONE

        // ON ENTRY

        // restart = FALSE IF THIS STEP IS NOT A RESTART STEP
        // = TRUE IF IT IS
        // kod CONTAINS A CODE THAT SAYS HOW THE PREVIOUS SEARCH
        // DIRECTION WAS COMPUTED
        // = 1 IF GN-DIRECTION
        // = -1 IF SUBSPACE DIRECTION
        // = -2 IF NEWTON DIRECTION
        // fsum CONTAINS THE SUM OF SQUARES AT CURRENT POINT
        // residuals CONTAINS THE VECTOR OF RESIDUALS AT CURRENT POINT
        // nPts CONTAINS LENGTH OF THE ARRAYS residuals, v, and w1
        // covarMat[][] double  2D ARRAY OF DIMENSION nPts*param
        // CONTAINING THE MATRIX R FROM THE
        // DECOMPOSITION    T
        // Q *J*D*E = (R)
        // (0)
        // IN THE UPPER TRIANGLE OF covarMat
        // WHERE
        // Q IS ORTHOGONAL (nPts*nPts)
        // J IS THE JACOBIAN (nPts*param) AT THE TERMINATING POINT
        // D IS A DIAGONAL MATRIX (param*param)
        // E IS A PERMUTATION MATRIX (param*param)
        // R IS AN UPPER TRIANGULAR MATRIX (param*param)
        // param CONTAINS LENGTH OF THE ARRAYS w0,dx,pivit,diag AND work
        // internalScaling = false IF NO SCALING OF THE JACOBIAN J IS DONE
        // = true IF COLUMN SCALING IS DONE
        // diag[] CONTAINS THE DIAGONAL ELEMENTS OF THE DIAGONAL MATRIX
        // D ABOVE (IF SCALING IS DONE, OTHERWISE UNDEFINED)
        // pivit[] CONTAINS THE PERMUTATION MATRIX E FROM ABOVE
        // w0[] CONTAINS INFO. NEEDED TO FORM MATRIX Q ABOVE
        // pseudoRank CONTAINS PSEUDO RANK USED TO COMPUTE GN-DIRECTION IN dx
        // dx[] CONTAINS GN-DIRECTION
        // v[] CONTAINS   J*dx

        // ON RETURN

        // dx[] COMPLETELY DESTROYED
        // rank CONTAINS SUGGESTED DIMENSION OF SUBSPACE WHERE THE
        // MINIMIZATION SHALL BE DONE

        // WORKING AREAS

        // work[] double 1D ARRAY OF DIMENSION param
        // w1[] double 1D ARRAY OF DIMENSION nPts

        int i, j;
        int mindim = 0;
        double[] dummy = new double[1];
        double pgress;
        double[] work = new double[param];
        double[] temp = new double[2];

        rabs = 0.1;

        // CHECK IF A RESTART STEP

        if (!restart) {

            // IN THIS POSITION WE KNOW THAT PREVIOUS SEARCH DIRECTION WAS
            // COMPUTED BY MINIMIZING IN A SUBSPACE OR WAS THE GN - DIRECTION

            // TO DETERMINE A SUITABLE SUBSPACE WE TRY TO ESTIMATE THE
            // BEST REDUCTION IN THE OBJECTIVE FUNCTION BY INVESTIGATING
            // THE RIGHT HAND SIDE OF THE UPPER TRIANGULAR
            // SYSTEM              T
            // R * dx = -Q * residuals               (1)

            // RNGKM1 = PSEUDO RANK USED TO COMPUTE PREVIOUS SEARCH DIRECTION

            // FORM RIGHT HAND SIDE OF(1) AND STORE IN W1

            for (i = 0; i < nPts; i++) {
                w1[i] = -residuals[i];
            }

            sqrsl(covarMat, nPts, pseudoRank, w0, w1, dummy, w1, dummy, dummy, dummy, 1000);

            // COMPUTE ESTIMATES OF STEPLENGTHS w1[i] AND PROGRESS WORK[i]

            work[0] = w1[0];
            j = pivit[0];

            if (!internalScaling) {
                w1[0] = w1[0] / covarMat[0][0];
            } else {
                w1[0] = w1[0] * diag[j] / covarMat[0][0];
            }

            for (i = 1; i < pseudoRank; i++) {
                work[i] = w1[i];

                if (internalScaling) {
                    j = pivit[i];
                    w1[i] = w1[i] * diag[j] / covarMat[i][i];
                } else {
                    w1[i] = w1[i] / covarMat[i][i];
                }

                temp[0] = w1[i - 1];
                temp[1] = w1[i];
                w1[i] = dnrm2(2, temp, 1);
                temp[0] = work[i - 1];
                temp[1] = work[i];
                work[i] = dnrm2(2, temp, 1);
            } // for (i = 1; i < pseudoRank; i++)

            sn = w1[pseudoRank - 1];
            bn = work[pseudoRank - 1];

            // DETERMINE THE LOWEST POSSIBLE DIMENSION

            for (i = 0; i < pseudoRank; i++) {
                mindim = i + 1;

                if (work[i] > (rabs * bn)) {
                    break;
                }
            } // for (i = 0; i <  pseduoRank; i++)

            if (kod == 1) {
                pregn(work, mindim);
            }

            if (kod == -1) {
                presub(work);
            }

            return;
        } // if (!restart)

        // SUGGEST NEW PSEUDO RANK = LATEST PSEUDO RANK -1
        // ALSO SAVE PSEUDO RANK THAT REDUCES THE OBJECTIVE BEST

        if (icount <= 1) {
            bestpg = (0.5 * fsum) - philat;
            irank = lattry;
        }

        pgress = (0.5 * fsum) - philat;

        if (pgress > bestpg) {
            bestpg = (0.5 * fsum) - philat;
            irank = lattry;
        }

        rank = lattry - 1;

        if (lattry <= 1) {
            rank = irank;
            iters++;
            lattry = 0;
        }

        return;
    }

    /**
     * DOCUMENT ME!
     *
     * @param   n     DOCUMENT ME!
     * @param   x     DOCUMENT ME!
     * @param   incx  DOCUMENT ME!
     *
     * @return  DOCUMENT ME!
     */
    private double dnrm2(int n, double[] x, int incx) {

        // Euclidean norm of the n-vector stored in x[] with storage increment incx .
        // if n <= 0 return with result = 0.
        // if n >= 1 then incx must be >= 1

        // four phase method using two built-in constants that are
        // hopefully applicable to all machines.
        // cutlo = maximum of  SQRT(u/eps)  over all known machines.
        // cuthi = minimum of  SQRT(v)      over all known machines.
        // where
        // eps = smallest no. such that eps + 1. > 1.
        // u   = smallest positive no.   (underflow limit)
        // v   = largest  no.            (overflow  limit)

        // brief outline of algorithm..

        // phase 1    scans zero components.
        // move to phase 2 when a component is nonzero and <= cutlo
        // move to phase 3 when a component is > cutlo
        // move to phase 4 when a component is >= cuthi/m
        // where m = n for x[] double and m = 2*n for complex.

        double fn_val = 0.0;
        int i, ix, j, next;
        double cuthi, cutlo, hitest, sum, xmax;
        double term;
        boolean doSwitch, loop, do85, do100, do105, do110, do115, do200;

        if ((n <= 0) || (incx <= 0)) {
            fn_val = 0.0;

            return fn_val;
        }

        // Set machine-dependent constants

        cutlo = Math.sqrt(Double.MIN_VALUE / srelpr);
        cuthi = Math.sqrt(Double.MAX_VALUE);

        next = 1;
        sum = 0.0;
        xmax = 0.0;
        i = 0;
        j = 0;
        ix = 1;
        loop = true;
        doSwitch = true;
        do85 = false;
        do100 = false;
        do105 = false;
        do110 = false;
        do115 = false;
        do200 = false;

// begin main loop
mainLoop:
        while (loop) {

            if (doSwitch) {

                switch (next) {

                    case 1:
                        if (Math.abs(x[i]) > cutlo) {
                            do85 = true;
                        } else {
                            next = 2;
                            xmax = 0.0;

                            continue mainLoop;
                        }

                        break;

                    case 2:

                        // phase 1. sum is zero
                        if (x[i] == 0.0) {
                            do200 = true;
                        } else if (Math.abs(x[i]) > cutlo) {
                            do85 = true;
                        } else {
                            next = 3;
                            do105 = true;
                        }

                        break;

                    case 3:
                        // phase 2.  sum is small.
                        // scale to avoid destructive underflow.

                        if (Math.abs(x[i]) > cutlo) {
                            // prepare for phase 3.

                            sum = (sum * xmax) * xmax;
                            do85 = true;
                        } // if (Math.abs(x[i]) > cutlo )
                        else {
                            do110 = true;
                        }

                        break;

                    case 4:
                        do110 = true;
                        break;
                } // switch (next)
            } // if (doSwitch)

            if (do85) {
                // phase 3.  sum is mid-range.  no scaling.

                // for real or d.p. set hitest = cuthi/n
                // for complex      set hitest = cuthi/(2*n)

                do85 = false;
                hitest = cuthi / n;

                for (j = ix; j <= n; j++) {

                    if (Math.abs(x[i]) >= hitest) {
                        do100 = true;
                        doSwitch = false;

                        continue mainLoop;
                    }

                    sum = sum + (x[i] * x[i]);
                    i = i + incx;
                } // for (j = ix; j <= n; j++)

                fn_val = Math.sqrt(sum);

                return fn_val;
            } // if (do85)

            if (do100) {
                do100 = false;

                // prepare for phase 4.
                // ABS(x(i)) is very large
                ix = j;
                next = 4;
                doSwitch = true;
                sum = (sum / x[i]) / x[i];

                // Set xmax; large if next = 4, small if next = 3
                xmax = Math.abs(x[i]);
                term = x[i] / xmax;
                sum = sum + (term * term);

                ix = ix + 1;
                i = i + incx;

                if (ix <= n) {
                    continue mainLoop;
                }

                // end of main loop.

                // compute square root and adjust for scaling.

                fn_val = xmax * Math.sqrt(sum);

                return fn_val;
            } // if (do100)

            if (do200) {
                do200 = false;
                doSwitch = true;
                ix = ix + 1;
                i = i + incx;

                if (ix <= n) {
                    continue mainLoop;
                }

                // end of main loop.

                // compute square root and adjust for scaling.

                fn_val = xmax * Math.sqrt(sum);

                return fn_val;
            } // if (do200)

            if (do105) {
                do105 = false;
                doSwitch = true;
                xmax = Math.abs(x[i]);
                term = x[i] / xmax;
                sum = sum + (term * term);

                ix = ix + 1;
                i = i + incx;

                if (ix <= n) {
                    continue mainLoop;
                }

                // end of main loop.

                // compute square root and adjust for scaling.

                fn_val = xmax * Math.sqrt(sum);

                return fn_val;
            } // if (do105)

            if (do110) {
                do110 = false;
                // common code for phases 2 and 4.
                // in phase 4 sum is large.  scale to avoid overflow.

                if (Math.abs(x[i]) <= xmax) {
                    do115 = true;
                    doSwitch = false;

                    continue mainLoop;
                }

                term = xmax / x[i];
                sum = 1.0 + (sum * term * term);
                xmax = Math.abs(x[i]);
                do200 = true;
                doSwitch = false;

                continue mainLoop;
            } // if (do110)

            if (do115) {
                do115 = false;
                doSwitch = true;
                term = x[i] / xmax;
                sum = sum + (term * term);

                ix = ix + 1;
                i = i + incx;

                if (ix <= n) {
                    continue mainLoop;
                }

                // end of main loop.

                // compute square root and adjust for scaling.

                fn_val = xmax * Math.sqrt(sum);

                return fn_val;
            } // if (do115)
        } // while (loop)

        // Should never reach here
        return fn_val;

    } // dnrm2

    /**
     * DOCUMENT ME!
     */
    private void evreuc() {
        // CHECK IF PREVIOUS STEP WAS A COMPLETE FAILURE
        // I.E.  IF THE VALUE OF THE OBJECTIVE FUNCTION INCREASED
        // IN CURRENT STEP WE RESTART AT PREVIOUS POINT
        // IF NO RESTART IS DONE WE UPDATE CERTAIN COMMON-VARIABLES
        // THAT HOLD INFORMATION OF THE TWO LATEST POINTS IN THE
        // ITERATION      iters+1       iters
        // a          := a      +alpha*dx

        // ON ENTRY

        // w3 double 1D ARRAY OF DIMENSION param CONTAINING
        // THE PREVIOUS POINT (TIME STEP iters)
        // param INTEGER SCALAR CONTAINING THE LENGTH OF THE ARRAY w3, a
        // nPts INTEGER SCALAR CONTAINING THE LENGTH OF THE ARRAY residuals
        // iters INTEGER SCALAR CONTAINING ITERATION NUMBER
        // funcEval INTEGER SCALAR CONTAINING NO. OF FUNCTION EVALUATIONS
        // DONE SO FAR
        // alpha double SCALAR CONTAINING THE STEPSIZE alpha USED IN
        // PREVIOUS STEP
        // alphup double SCALAR CONTAINING THE UPPER BOUND OF STEP LENGTH
        // USED IN PREVIOUS STEP
        // d1sqs double SCALAR CONTAINING PREDICTED REDUCTION IN THE
        // OBJECTIVE FUNCTION FOR LATEST STEP
        // beta double SCALAR CONTAINING THE NORM OF ORTHOGONAL PROJECTION
        // OF residuals ONTO COLUMN SPACE OF THE JACOBIAN
        // fsum double SCALAR CONTAINING THE SUM OF SQUARES AT PREVIOUS POINT
        // (TIME STEP iters)
        // dxnorm double SCALAR CONTAINING THE NORM OF THE SEARCH
        // DIRECTION (dx)
        // kod INTEGER SCALAR CONTAINING A CODE THAT SAYS HOW THE SEARCH
        // DIRECTION dx WAS COMPUTED
        // = 1 GAUSS-NEWTON DIRECTION
        // = -1 SUBSPACE DIRECTION
        // = -2 IF NEWTON DIRECTION
        // pseudoRank INTEGER SCALAR CONTAINING A CODE DEPENDING ON kod
        // IF ABS(kod)=1
        // THEN pseudoRank EQUALS PSEUDO RANK USED TO COMPUTE dx
        // ELSE pseudoRank EQUALS -param
        // phi double SCALAR CONTAINING THE VALUE OF OBJECTIVE FUNCTION
        // AT THE CURRENT POINT (TIME STEP iters+1)
        // errorStatus INTEGER SCALAR CONTAINING
        // -1 IF dx WAS NOT A DESCENT DIRECTION
        // -2 IF alpha*II dx II < SQRT(srelpr)
        // -3 IF NOT POSITIVE DEFINITE MATRIX FROM SECOND DERIVATIVE
        // SYSTEM
        // -4 IF THE ALGORITHM WANTS TO USE SECOND DERIVATIVES BUT
        // IS NOT ALLOWED TO DO THAT
        // < -10 AS A USER STOP INDICATOR
        // 0 OTHERWISE

        // ON RETURN

        // restart LOGICAL SCALAR SET TO TRUE IF RESTART IS RECOMMENDED
        // SET TO FALSE OTHERWISE
        // IF restart is false, THE FOLLOWING ARE CHANGED:

        // iters IS INCREASED BY 1
        // errorStatus IS SET TO -5 IF THE OBJECTIVE IS INCREASED IN THE
        // LATEST STEP

        // IF restart is true, THE FOLLOWING ARE CHANGED:
        // funcEval IS INCREASED BY 1
        // a[] double 1D ARRAY OF DIMENSION param TAKES
        // THE VALUE OF THE PREVIOUS POINT (w3)
        // residuals double 1D ARRAY OF DIMENSION nPts CONTAINS
        // THE VECTOR OF RESIDUALS AT THE PREVIOUS POINT

        int i;
        double c2, dsqrel;
        double[][] dummy = new double[1][1];
        double temp;

        restart = false;
        dsqrel = Math.sqrt(srelpr);

        // CALL RESTART ADVISOR ROUTINE

        reavuc();

        if (restart) {
        	if (outputMes) {
                Preferences.debug("In evreuc: restart\n");
        	}
            itotal++;
            icount++;
            lattry = pseudoRank;
            philat = phi;
        } // if restart
        else if (errorStatus == -5) {
        	if (outputMes) {
                Preferences.debug("Undamped Newton  does not work: Try GN\n");
        	}
            ifree = 5;
            indic = 3;
            iters++;
        } // else if (errorStatus == -5)
        else {
            // NO RESTART.UPDATE HISTORY VARIABLES

            icount = 0;
            lattry = pseudoRank;
            betkm2 = betkm1;
            d1km2 = d1km1;
            fsqkm2 = fsqkm1;
            dxnkm2 = dxnkm1;
            alfkm2 = alfkm1;
            aupkm2 = aupkm1;
            rngkm2 = rngkm1;
            kodkm2 = kodkm1;
            betkm1 = beta;
            d1km1 = d1sqs;
            fsqkm1 = fsum;
            dxnkm1 = dxnorm;
            alfkm1 = alpha;
            aupkm1 = alphup;
            rngkm1 = pseudoRank;
            kodkm1 = kod;

            // CHECK IF ANY BOUND IS CROSSED.IF SO, MAKE THE
            // CORRESPONDING CONSTRAINT ACTIVE

            for (i = 0; i < param; i++) {

                if (aset[i] != 0) {
                    continue;
                }

                c2 = 0.0;

                if (a[i] < (bl[i] + c2 + dsqrel)) {
                    aset[i] = 1;
                    a[i] = bl[i];
                    paramAct++;

                    continue;
                }

                c2 = 0.0;

                if (a[i] <= (bu[i] - c2 - dsqrel)) {
                    continue;
                }

                aset[i] = -1;
                a[i] = bu[i];
                paramAct++;
            } // for (i = 0; i < param; i++)

            // INCREASE ITERATION COUNT

            iters++;

            return;
        }

        // RESTART FROM PREVIOUS POINT
        errorStatus = 0;

        for (i = 0; i < param; i++) {
            a[i] = w3[i];
        }

        ctrl = -1;

        // EVALUATE THE FUNCTIONS AT PREVIOUS POINT
        ctrlMat[0] = ctrl;
        fitToFunction(a, residuals, dummy);
        ctrl = ctrlMat[0];

        if (ctrl < -10) {
            errorStatus = ctrl;

            return;
        }

        alpha = alfkm1;
        alphup = aupkm1;
        temp = dnrm2(nPts, residuals, 1);
        phi = 0.5 * temp * temp;
        funcEval++;

        return;
    }

    /**
     * DOCUMENT ME!
     *
     * @param  alfk    DOCUMENT ME!
     * @param  xnew    DOCUMENT ME!
     * @param  fnew    DOCUMENT ME!
     * @param  phiMat  DOCUMENT ME!
     */
    private void fsumsq(double alfk, double[] xnew, double[] fnew, double[] phiMat) {
        // EVALUATE FUNCTION VALUES AT THE POINT a+alfk*dx . POSSIBLY THE USER CAN SIGNAL UNCOMPUTABILTY ON RETURN FROM
        // THE USER WRITTEN ROUTINE fitToFunction

        double fn_val;
        double[][] dummy = new double[1][1];
        int lctrl;
        int i;
        double temp;

        fn_val = 0.0;

        for (i = 0; i < param; i++) {
            xnew[i] = a[i] + (alfk * dx[i]);
        }

        lctrl = ctrl;
        ctrlMat[0] = lctrl;
        fitToFunction(xnew, fnew, dummy);
        lctrl = ctrlMat[0];
        if (outputMes) {
	        Preferences.debug("xnew inside fsumsq\n");
	
	        for (i = 0; i < param; i++) {
	            Preferences.debug(xnew[i] + "\n");
            }
        }

        /*Preferences.debug("fnew inside fsumsq\n");
         * for (i = 0; i < nPts; i++) { Preferences.debug(fnew[i] + "\n");}*/
        if (ctrl != 1) {

            if (lctrl == -1) {
                temp = dnrm2(nPts, fnew, 1);
                fn_val = 0.5 * temp * temp;
            }

            if (outputMes) {
                Preferences.debug("fn_val when ctrl = -1 = " + fn_val + "\n");
            }

            if (lctrl < -10) {
                ctrl = lctrl;
            }

            phiMat[0] = fn_val;

            return;
        } // if (ctrl != 1)

        if (lctrl == 1) {
            temp = dnrm2(nPts, fnew, 1);
            fn_val = 0.5 * temp * temp;
        }
      
        if (outputMes) {
            Preferences.debug("fn_val when ctrl= 1 = " + fn_val + "\n");
        }
        ctrl = lctrl;
        phiMat[0] = fn_val;

        return;
    }

    /**
     * DOCUMENT ME!
     *
     * @param  gmod    DOCUMENT ME!
     * @param  dphize  DOCUMENT ME!
     * @param  tau     DOCUMENT ME!
     * @param  pmax    DOCUMENT ME!
     */
    private void gauc(double[] gmod, double dphize, double tau, double pmax) {
        // THIS IS A ROUTINE FOR UNCONSTRAINED LEAST SQUARES PROBLEMS THAT HALVES THE VALUE OF alfk UNTIL A
        // GOLDSTEIN-ARMIJO CONDITION IS SATISFIED OR UNTIL THE NORM OF THE SEARCH DIRECTION TIMES THE THE STEPLENGTH IS
        // REDUCED BELOW SQRT(RELATIVE PRECISION) OR UNTIL THE STEPLENGTH IS SMALLER THAN ALFMIN

        // PHI(ALPHA)=0.5*(IIF(a+ALPHA*dx)II**2)
        // CHOOSE ALPHA=a SO THAT
        // PHI(x)<=PHI(0)+TAU*x*DPHI(0)   (1)
        // WE KNOW THAT PHI(U)>PHI(0)+TAU*alfk*DPHI(0)
        // THE SIMPLEST WE CAN DO IS TO SET   alfk=alfk*0.5 AND
        // TEST IF CONDITION  (1) IS SATISFIED FOR x=alfk

        double phix, sqreps, x;

        sqreps = Math.sqrt(srelpr);
        phix = phi;
        x = alfk;

        do {

            if (((pmax * x) < sqreps) || (x <= alfmin)) {
                errorStatus = -2;
            }

            if (errorStatus == -2) {
                alfk = x;
                phik = phix;

                return;
            }

            x = 0.5 * x;
            ctrl = -1;
            fsumsq(x, gmod, residuals, phiMat);
            phix = phiMat[0];
            if (outputMes) {
                Preferences.debug("In GAUC: phix = " + phix + " x = " + x + "\n");
            }

            // POSSIBLY THE USER CAN TERMINATE

            if (ctrl < -10) {
                k2 = ctrl;
            }

            if (k2 < 0) {
                return;
            }

            k2++;
        } while (phix > (phi + (tau * x * dphize)));

        alfk = x;
        phik = phix;

        return;
    }

    /**
     * DOCUMENT ME!
     */
    private void gnavuc() {
        // THIS IS THE GAUSS-NEWTON DIRECTION ADVISOR ROUTINE
        // IT ACCEPTS THE GN-DIRECTION AS SEARCH DIRECTION FOR CURRENT
        // STEP IF CONDITION 1) AND ONE OF CONDITION 2)-3) ARE FULLFILLED

        // CONDITIONS

        // 1) A PRIMARY DEMAND FOR ACCEPTING THE GN-DIRECTION IS THAT A GN-DIRECTION
        // WAS USED IN THE PREVIOUS STEP AND NO RESTART WAS DONE IN THIS STEP
        // 2) THE DECREASE IN OBJECTIVE FUNCTION VALUE IN LATEST STEP WAS GOOD ENOUGH
        // AND WE ARE OUTSIDE THE UMBRELLA
        // 3) A TOLERABLE VALUE OF CONVERGENCE FACTOR IS OBSERVED

        // ON ENTRY

        // iters CONTAINS THE ITERATION COUNT
        // restart = true IF THIS STEP IS A RESTART STEP
        // = false IF THIS STEP IS NOT A RESTART STEP
        // kod   CONTAINS A CODE THAT SAYS HOW THE PREVIOUS SEARCH
        // DIRECTION WAS COMPUTED
        // = 1 IF GN-DIRECTION
        // = -1 IF SUBSPACE DIRECTION
        // = -2 IF NEWTON DIRECTION
        // fsum  CONTAINS THE SUM OF SQUARES AT CURRENT POINT
        // beta  CONTAINS THE NORM OF THE ORTHOGONAL PROJECTION OF residuals
        // ONTO THE COLUMN SPACE OF THE JACOBIAN
        // prekm1  SEE EXPLANATION IN SUBROUTINE ANALUC
        // paramAct INTEGER SCALAR CONTAINING THE NO.OF ACTIVE CONSTRAINTS
        // (POSITIVE OR NEGATIVE)
        // aset[] INTEGER 1D ARRAY OF DIMENSION param
        // CONTAINING +1 OR -1 TO INDICTATE AN ACTIVE BOUND (OTHERWISE ZERO)

        // ON RETURN

        // secind  INDICATES THE ALTERNATIVE IF GN-DIRECTION IS NOT ACCEPTED
        // = -1 IF SUBSPACE MINIMIZATION
        // = -2 IF NEWTONS METHOD
        // paramAct IS PUT EQUAL TO -paramAct-1 IF paramAct IS <0 ON ENTRY
        // gndok = true IF CURRENT GN-DIRECTION IS ACCEPTED
        // = false IF RECOMPUTATION OF SEARCH DIRECTION IS NEEDED

        double pgress, fnorm, ac;
        final double c1 = 0.5, c2 = 0.1, c3 = 4.0, c4 = 10.0, c5 = 0.016;

        gndok = true;
        pgress = -fsum;
        fnorm = Math.sqrt(fsum);

        // CONDITION 1

        if ((!restart) && ((k == 0) || (imax != -1))) {
            return;
        }

        if (ifree > 0) {
            ifree--;

            if (restart) {
                secind = -1;
                gndok = false;
            } else {
                gndok = true;
            }

            return;
        } // if (ifree > 0)

        gndok = false;
        pgress = fsqkm1 - fsum;

        if (Math.abs(kod) == 2) {
            secind = kod;

            return;
        }

        if ((kod != -1) && (!restart)) {
            gndok = true;

            // CONDITION 2

            ac = Math.min(1.0, aupkm1);

            if ((pgress > (c2 * ac * (2.0 - ac) * d1km1)) && (fnorm < (c3 * beta))) {
                return;
            }

            // CONDITION 3

            if (beta < (c1 * betkm1)) {
                return;
            }

            gndok = false;

            if (fnorm <= (c4 * beta)) {
                secind = -1;
            } else {
                secind = -2;
            }

            return;
        } // if ((kod != -1) && (!restart))

        secind = -1;

        if (!restart) {

            if ((alfkm1 < (c5 * aupkm1)) && (prekm1 < (c2 * beta))) {
                secind = -2;
            }

            return;
        } // if (!restart)

        if (prekm1 < (c2 * beta)) {
            secind = -2;
        }

        if (icount != 1) {
            return;
        }

        if (imax == -1) {
            return;
        }

        aset[imax] = ival;
        imax = -1;
        paramAct++;

        return;
    }

    /**
     * DOCUMENT ME!
     *
     * @param   internalScaling  DOCUMENT ME!
     * @param   pseudoRankMat    DOCUMENT ME!
     * @param   work             DOCUMENT ME!
     *
     * @return  DOCUMENT ME!
     */
    private double gndunc(boolean internalScaling, int[] pseudoRankMat, double[] work) {
        // COMPUTE THE SOLUTION (DX) OF THE SYSTEM              T R*D*E*DX = -Q *residuals WHERE R IS param*param UPPER
        // TRIANGULAR D IS param*param DIAGONAL MATRIX E IS param*param PERMUTATION MATRIX Q IS nPts*nPts ORTHOGONAL
        // MATRIX residuals IS AN nPts-VECTOR THE SOLUTION WILL BE COMPUTED BY ONLY USING THE FIRST PRANK <= param
        // COLUMNS OF R

        // ON ENTRY

        // residuals  CONTAINS THE VECTOR OF RESIDUALS AT CURRENT POINT
        // nPts IS THE LENGTH OF THE ARRAYS residuals,V AND w1
        // covarMat CONTAINS THE MATRIX R IN THE UPPER TRIANGLE AND
        // INFORMATION NEEDED TO FORM MATRIX Q IN THE LOWER PART
        // param IS THE LENGTH OF THE ARRAYS DIAG,pivit,w0 AND DX
        // internalScaling = false IF DIAGONAL MATRIX D IS THE UNIT MATRIX
        // = true IF SCALING IS DONE
        // diag[] CONTAINS THE DIAGONAL ELEMENTS OF MATRIX D IF internalScaling = true
        // IF internalScaling = false THEN DIAG[K] K=0,1,....,param-1 IS UNDEFINED
        // pivit[],w0[] CONTAIN INFORMATION RETURNED FROM THE LINPACK
        // ROUTINE SQRDC AS JPVT AND w0
        // pseudoRank IS THE SUGGESTED PSEUDO RANK OF MATRIX R

        // ON RETURN

        // DX[] THE PSEUDO INVERSE SOLUTION (GAUSS-NEWTON SEARCH DIRECTION)
        // D1SQS IS THE PREDICTED REDUCTION IN THE OBJECTIVE
        // V[]   IS THE OTHOGONAL PROJECTION OF residuals ONTO THE SPACE SPANNED
        // BY THE COLUMNS OF THE JACOBIAN J FOR WHICH
        // T
        // Q *J*D*E = (R)
        // (0)           HOLDS.

        // w1[]  double 1D ARRAY OF DIMENSION nPts  !!!!!
        // CONTAINS THE FIRST param ELEMENTS OF    T
        // -Q *F

        double d1sqs;
        int pseudoRank;
        int i, j;
        double[] dummy = new double[1];
        double temp;

        pseudoRank = pseudoRankMat[0];
        d1sqs = 0.0;

        for (j = 0; j < nPts; j++) {
            w1[j] = -residuals[j];
        }

        if ((param == 0) || (pseudoRank <= 0)) {
            return d1sqs;
        }
        // T
        // COMPUTE THE SOLUTION DX,THE PROJECTION  -V  AND w1 = -Q * residuals

        sqrsl(covarMat, nPts, pseudoRank, w0, w1, dummy, w1, dx, dummy, v, 1101);
        temp = dnrm2(pseudoRank, w1, 1);
        d1sqs = temp * temp;

        if (info == -1) {
            btrunc(internalScaling, work);
            pseudoRankMat[0] = pseudoRank;

            return d1sqs;
        }

        // SQRSL HAS DETECTED EXACT SINGULARITY OF MATRIX R
        // INFO = INDEX OF THE FIRST ZERO DIAGONAL ELEMENT OF R
        // SOLVE UPPER TRIANGULAR SYSTEM

        pseudoRank = info;

        for (j = 0; j < param; j++) {
            dx[j] = w1[j];
        }

        if (pseudoRank <= 0) {
            btrunc(internalScaling, work);
            pseudoRankMat[0] = pseudoRank;

            return d1sqs;
        }

        strsl(w1);

        // MOVE SOLUTION OF TRIANGULAR SYSTEM TO DX

        for (i = 0; i < param; i++) {
            temp = dx[i];
            dx[i] = w1[i];
            w1[i] = temp;
        } // for (i = 0; i < param; i++)

        // DO BACKTRANSFORMATIONS   DX:=D*E*DX

        btrunc(internalScaling, work);
        pseudoRankMat[0] = pseudoRank;

        return d1sqs;
    }

    /**
     * DOCUMENT ME!
     */
    private void hess() {
        // COMPUTE THE param*param MATRIX            nPts-1
        // gmat :=   SIGMA(residuals *G )
        // K=0          K  K
        // WHERE G   IS THE HESSIAN OF residuals (a)
        // K                             K

        double[] c1Mat = new double[param];
        double[] c2Mat = new double[param];
        double[] c3Mat = new double[param];
        double[] c4Mat = new double[param];
        double[][] dummy = new double[1][1];
        int j, k, ll;
        double athird, eps1, eps2, epsk, epsj, ak, aj, term, sum;

        ctrl = -1;
        athird = 1.0 / 3.0;
        eps2 = Math.pow(srelpr, athird);
        eps1 = eps2;

        for (k = 0; k < param; k++) {
            ak = a[k];
            epsk = Math.max(Math.abs(ak), 1.0) * eps2;

            for (j = 0; j <= k; j++) {
                aj = a[j];
                epsj = Math.max(Math.abs(aj), 1.0) * eps1;
                a[k] = ak + epsk;
                a[j] = a[j] + epsj;
                ctrlMat[0] = ctrl;
                fitToFunction(a, c1Mat, dummy);
                ctrl = ctrlMat[0];

                if (ctrl < -10) {
                    errorStatus = ctrl;

                    return;
                }

                a[k] = ak;
                a[j] = aj;
                a[k] = ak + epsk;
                a[j] = a[j] - epsj;
                ctrlMat[0] = ctrl;
                fitToFunction(a, c2Mat, dummy);
                ctrl = ctrlMat[0];

                if (ctrl < -10) {
                    errorStatus = ctrl;

                    return;
                }

                a[k] = ak;
                a[j] = aj;
                a[k] = ak - epsk;
                a[j] = a[j] + epsj;
                ctrlMat[0] = ctrl;
                fitToFunction(a, c3Mat, dummy);
                ctrl = ctrlMat[0];

                if (ctrl < -10) {
                    errorStatus = ctrl;

                    return;
                }

                a[k] = ak;
                a[j] = aj;
                a[k] = ak - epsk;
                a[j] = a[j] - epsj;
                ctrlMat[0] = ctrl;
                fitToFunction(a, c4Mat, dummy);
                ctrl = ctrlMat[0];

                if (ctrl < -10) {
                    errorStatus = ctrl;

                    return;
                }

                a[k] = ak;
                a[j] = aj;
                sum = 0.0;

                for (ll = 0; ll < nPts; ll++) {
                    term = c1Mat[ll] - c2Mat[ll] - c3Mat[ll] + c4Mat[ll];
                    sum = sum + (term / (4.0 * epsk * epsj) * residuals[ll]);
                } // for (ll = 0; ll < nPts; ll++)

                gmat[k][j] = sum;

                if (k == j) {
                    continue;
                }

                gmat[j][k] = sum;
            } // for (k = 0; j <= k; j++)
        } // for (k = 0; k < param; k++)

        return;
    }

    /**
     * DOCUMENT ME!
     */
    private void jacdif() {
        // COMPUTE THE nPts*param JACOBIAN OF residuals[a] AT THE CURRENT POINT BY
        // USING FORWARD DIFFERENCES

        // ON ENTRY

        // a[]  CONTAINS THE CURRENT POINT
        // param IS THE LENGTH OF THE ARRAY a
        // residuals[] CONTAINS THE VECTOR OF RESIDUALS AT CURRENT POINT
        // nPts IS THE LENGTH OF THE ARRAYS residuals AND W1

        // ON RETURN

        // covarMat CONTAINS THE APPROXIMATE JACOBIAN IN THE nPts*param UPPER PART
        // errorStatus CONTAINS A USER STOP INDICATOR OR ZERO

        int i, j;
        double delta, xtemp, deltaj;

        errorStatus = 0;
        delta = Math.sqrt(srelpr);

        for (j = 0; j < param; j++) {
            xtemp = a[j];
            deltaj = Math.max(Math.abs(xtemp), 1.0) * delta;
            a[j] = xtemp + deltaj;
            ctrl = -1;
            ctrlMat[0] = ctrl;
            fitToFunction(a, w1, covarMat);
            ctrl = ctrlMat[0];

            if (ctrl < -10) {
                errorStatus = ctrl;

                return;
            }

            for (i = 0; i < nPts; i++) {
                covarMat[i][j] = (w1[i] - residuals[i]) / deltaj;
            } // for (i = 0; i < nPts; i++)

            a[j] = xtemp;
        } // for (j = 0; j < param; j++)

        return;
    }

    /**
     * DOCUMENT ME!
     *
     * @param  nn  DOCUMENT ME!
     */
    private void jtrj(int nn) {
        // T
        // FORM THE nn by nn SYMMETRIC MATRIX  covarMat *covarMat
        // AND STORE IN covarMat

        // T
        // FIRST FORM THE LOWER PART OF covarMat *covarMat
        // AND STORE IN THE LOWER PART OF covarMat

        int i, j, k;

        for (j = 0; j < nn; j++) {

            for (i = 0; i < nn; i++) {
                w0[i] = covarMat[i][j];
            }

            for (k = j; k < nn; k++) {
                covarMat[k][j] = 0.0;

                for (i = 0; i <= j; i++) {
                    covarMat[k][j] = covarMat[k][j] + (covarMat[i][k] * w0[i]);
                }
            } // for (k = j; k < nn; k++)
        } // for (j = 0; j < nn; j++)

        // MOVE THE LOWER PART OF covarMat TO THE UPPER PART OF covarMat

        if (nn == 1) {
            return;
        }

        for (i = 1; i < nn; i++) {
            k = i - 1;

            for (j = 0; j <= k; j++) {
                covarMat[j][i] = covarMat[i][j];
            } // for (j = 0; j <= k; j++)
        } // for (i = 1; i < nn; i++)

        return;
    }

    /**
     * DOCUMENT ME!
     *
     * @param  gmod    DOCUMENT ME!
     * @param  dphize  DOCUMENT ME!
     * @param  alplow  DOCUMENT ME!
     */
    private void lineuc(double[] gmod, double dphize, double alplow) {
        // THIS IS A LINE SEARCH ROUTINE FOR UNCONSTRAINED LEAST SQUARES PROBLEMS

        // COMPUTE THE STEPLENGTH alpha FOR THE ITERATION
        // aNew := a + alpha * dx
        // WHERE   a IS THE CURRENT POINT
        // dx    IS THE SEARCH DIRECTION
        // alpha IS CLOSE TO THE SOLUTION OF THE PROBLEM
        // MINIMIZE  phi(alpha)
        // WITH THE RESTRICTION  0<alplow<=alpha<=alphup
        // HOWEVER, IF WE ARE FORCED TO TAKE A PURE GOLDSTEIN-ARMIJO STEP
        // THE STEPLENGTH CAN BE SLIGHTLY LOWER THAN alplow

        // phi[alpha]=0.5*(IIresiduals(a+alpha*dx)II**2)
        // residuals(a)= (residuals0[a],residuals1[a],.........,
        // residualsnPts-1[a]) TRANSPOSE

        // ON ENTRY:

        // a[param] double ARRAY OF LENGTH param-THE CURRENT POINT
        // (CHANGED ON RETURN)
        // gmod[param] double ARRAY OF LENGTH param-GRADIENT OF PHI AT ALPHA=0
        // (CHANGED ON RETURN)
        // dx[param] double ARRAY OF LENGTH param-SEARCH DIRECTION
        // residuals[nPts] double ARRAY OF LENGTH nPts -
        // residuals[0]....residuals[nPts-1] CONTAIN THE
        // VALUE OF residuals(a) AT THE CURRENT POINT.
        // (CHANGED ON RETURN)
        // v[nPts] double ARRAY 0F LENGTH nPts: THE VECTOR C*dx WHERE
        // C IS THE JACOBIAN AND dx THE SEARCH DIRECTION
        // nPts INTEGER-NO. OF FUNCTIONS IN residuals[a] =
        // residuals0[a].......,resdiualsnPts-1[a]
        // param  INTEGER-NO. OF UNKNOWNS
        // alpha double -A FIRST GUESS OF STEPLENGTH
        // phi double -phi(alpha) AT alpha=0
        // dphize double -THE DERIVATIVE OF phi(alpha) AT alpha=0
        // alplow double -FIX LOWER BOUND OF THE STEPLENGTH

        // ctrl CAN HAVE 3 DIFFERENT VALUES ON ENTRY
        // ctrl = 1  MEANS EVALUATE THE FUNCTIONS AT THE POINT a AND
        // RETURN THIS VECTOR IN THE ARRAY residuals IF THE FUNCTIONS
        // ARE COMPUTABLE.
        // ON RETURN THE USER CAN INDICATE UNCOMPUTABILITY BY
        // SETTING ctrl = -1
        // DO NOT ALTER ARRAY a.
        // ctrl = -1  MEANS EVALUATE THE FUNCTIONS AT THE POINT a AND
        // RETURN THIS VECTOR IN THE ARRAY residuals IF THE FUNCTIONS
        // ARE COMPUTABLE. DO NOT ALTER ARRAY a.
        // POSSIBLE UNCOMPUTABILITY OF THE FUNCTIONS MUST BE
        // INDICATED BY SETTING ctrl TO A VALUE < -10 ON RETURN
        // ctrl = 2  MEANS CALCULATE THE JACOBIAN OF residuals[a] AT THE POINT a
        // AND RETURN THIS MATRIX IN THE ARRAY C IF THE JACOBIAN
        // IS SUPPLIED ANALYTICALLY.
        // POSSIBLE UNCOMPUTABILITY OF THE JACOBIAN MUST BE
        // INDICATED BY SETTING ctrl TO A VALUE < -10 ON RETURN.
        // IF THE USER WANTS THE JACOBIAN BEING COMPUTED
        // NUMERICALLY THAT SHOULD BE INDICATED BY SETTING
        // CTRL=0 ON RETURN.
        // DO NOT ALTER ARRAYS a AND residuals.
        // alphup double -FIX UPPER BOUND OF THE STEPLENGTH

        // ON RETURN:     AND errorStatus = 0

        // a[param] THE NEW POINT
        // residuals[nPts] THE VALUE OF residuals(a) AT THE NEW POINT
        // alpha THE COMPUTED STEPLENGTH
        // phikp1 double -phi(alpha) AT THE NEW POINT
        // aDiff double -THE 2-NORM  II aOld-aNew II
        // errorStatus  INTEGER  = -1 IF THE SEARCH DIRECTION IS NOT A
        // DESCENT DIRECTION.
        // = -2 IF alpha*2NORM(dx) < SQRT(srelpr)
        // OR IF alpha <= alplow
        // = -3 IF THE FIRST GUESS OF STEPLENGTH
        // MAKES THE FUNCTIONS UNCOMPUTABLE AT a
        // < -10 AS A USER STOP INDICATOR
        // = 0 OTHERWISE
        // iev INTEGER- =NO. OF FUNCTION EVALUATIONS DONE
        // INSIDE THIS ROUTINE

        // WORKING AREAS:
        // fnew[nPts]  double ARRAY OF LENGTH nPts
        // v2[nPts] double ARRAY OF LENGTH nPts

        int i;
        double eta, tau, gamma, pmax, phikm2, xmin;
        double[] fnew = new double[nPts];
        double[] v2 = new double[nPts];
        boolean loop;
        boolean doMinrn = true;

        k2 = 0;
        aDiff = 0.0;
        alfkm1a = 0.0;
        phikm1 = phi;

        // SET VALUES OF THE CONSTANTS ETA,TAU AND GAMMA.
        // COMPUTE ALFMIN,ALFMAX,ALFK AND PMAX= THE EUCLIDEAN NORM OF dx

        eta = 0.3;
        tau = 0.25;
        gamma = 0.4;
        alfmax = alphup;
        alfmin = alplow;
        alfk = Math.min(alpha, alfmax);
        alfk = Math.max(alfk, alfmin);
        pmax = dnrm2(param, dx, 1);

        // dx MUST BE A DESCENT DIRECTION

        errorStatus = -1;

        if (dphize >= 0.0) {
            alpha = alfkm1a;
            phikp1 = phikm1;
            iev = k2;

            return;
        }

        errorStatus = 0;

        // COMPUTE PHIK=PHI(ALF0) AND TEST UNCOMPUTABILITY AT aOld

        ctrl = 1;
        fsumsq(alfk, gmod, fnew, phiMat);
        phik = phiMat[0];
        if (outputMes) {
            Preferences.debug("In lineuc no. 1: phik = " + phik + " alfk = " + alfk + "\n");
        }
        k2++;
        iev = k2;

        if (ctrl == -1) {
            errorStatus = -3;
        }

        if (alfk <= 0.0) {
            errorStatus = -2;
        }

        if (ctrl < -10) {
            errorStatus = ctrl;
        }

        if (errorStatus < 0) {
            return;
        }

        // COMPUTE THE VECTOR v2 SO THAT A ONE DIMENSIONAL
        // MINIMIZATION IN R[nPts] CAN BE DONE

        linuc2(fnew, v2);
        diff = phi - phik;

        // SET XMIN = THE BEST OF THE POINTS 0 AND ALF0

        if (diff >= 0.0) {
            xmin = alfk;
        } else {
            xmin = 0.0;
        }

        // MINIMIZE IN R[nPts]. USE TWO POINTS : 0 AND ALF0
        // NEW SUGGESTION OF STEPLENGTH IS ALFKP1
        // PK IS THE VALUE OF THE APPROXIMATING FUNCTION AT ALFKP1

        minrm(v2, xmin);

        // POSSIBLY THE OTHER ROOT IS CHOSEN

        if ((alfkp1 != beta2) && (pk > pbeta) && (beta2 <= alfk)) {
            alfkp1 = beta2;
            pk = pbeta;
        }

        alfkm1a = 0.0;
        phikm1 = phi;
        alfkm2a = alfkm1a;
        phikm2 = phikm1;
        alfkm1a = alfk;
        phikm1 = phik;
        alfk = alfkp1;

        // TEST TERMINATION CONDITION AT alpha = alf0
        loop = true;

        if (!((-diff <= (tau * dphize * alfkm1a)) || (phikm1 < (gamma * phi)))) {
            loop = false;
        }

        // TERMINATION CONDITION SATISFIED AT alpha = alf0
        while (loop) {
            diff = phi - phik;

            // CHECK IF ESSENTIAL REDUCTION IS LIKELY

            reduc(pk, eta, gmod, fnew);

            if (k2 < -10) {
                errorStatus = k2;

                return;
            }

            if (!reduce) {

                for (i = 0; i < param; i++) {
                    gmod[i] = a[i];
                    a[i] = a[i] + (alfkm1a * dx[i]);
                    gmod[i] = gmod[i] - a[i];
                }

                aDiff = dnrm2(param, gmod, 1);
                alpha = alfkm1a;
                phikp1 = phikm1;
                iev = k2;

                return;
            } // if (!reduce)

            // THE VALUE OF THE OBJECTIVE FUNCTION CAN MOST LIKELY BE REDUCED

            // MINIMIZE IN R[param].USE THREE POINTS:alfkm2a, alfkm1a, alfk
            // NEW SUGGESTION OF THE STEPLENGTH IS alfkp1
            // pk IS THE VALUE OF THE APPROXIMATING FUNCTION AT alfkp1

            minrn(phikm2, pmax);
            alfkm2a = alfkm1a;
            phikm2 = phikm1;
            alfkm1a = alfk;
            phikm1 = phik;
            alfk = alfkp1;
        } // while (loop)

        // TERMINATION CONDITION NOT SATISFIED AT alpha = alf0

        // COMPUTE phik = phi(alf1)

        ctrl = -1;
        fsumsq(alfk, gmod, fnew, phiMat);
        phik = phiMat[0];
        if (outputMes) {
            Preferences.debug("In LINEUC no.2: phik = " + phik + " alfk = " + alfk + "\n");
        }

        if (ctrl < -10) {
            k2 = ctrl;
        }

        if (k2 < 0) {
            errorStatus = k2;

            return;
        }

        // TEST TERMINATION CONDITION AT ALPHA = ALF1

        diff = phi - phik;

        if ((-diff <= (tau * dphize * alfk)) || (phik < (gamma * phi))) {
            // TERMINATION CONDITION SATISFIED AT ALPHA = ALF1

            // CHECK IF ALF0 IS SOMEWHAT GOOD

            if (phi <= phikm1) {

                // SINCE PHI[0] <= PHI[ALF0], ALF0 IS NO GOOD GUESS AND WE TRY
                // AN OTHER MINIMIZATION IN R[nPts].USE TWO POINTS:0 AND ALF1.
                // THE NEW SUGGESTION OF STEPLENGTH IS ALFKP1.
                // PK IS THE VALUE OF THE APPROXIMATING FUNCTION AT ALFKP1

                xmin = alfk;
                linuc2(fnew, v2);
                minrm(v2, xmin);

                // POSSIBLY THE OTHER ROOT IS CHOSEN

                if ((alfkp1 != beta2) && (pk > pbeta) && (beta2 <= alfk)) {
                    alfkp1 = beta2;
                    pk = pbeta;
                }

                alfkm1a = 0.0;
                phikm1 = phi;
                doMinrn = false;
            } // if (phi <= phikm1)

            if (doMinrn) {
                // MINIMIZE IN R[param].USE THREE POINTS:0, ALF0 AND ALF1.
                // THE NEW SUGGESTION OF THE STEPLENGTH IS ALFKP1.
                // PK IS THE VALUE OF THE APPROXIMATING FUNCTION AT ALFKP1

                minrn(phikm2, pmax);
            } // if (doMinrn)

            k2++;
            loop = true;

            while (loop) {
                diff = phi - phik;
                alfkm2a = alfkm1a;
                phikm2 = phikm1;
                alfkm1a = alfk;
                phikm1 = phik;
                alfk = alfkp1;

                // CHECK IF ESSENTIAL REDUCTION IS LIKELY

                reduc(pk, eta, gmod, fnew);

                if (k2 < -10) {
                    errorStatus = k2;

                    return;
                }

                if (!reduce) {

                    for (i = 0; i < param; i++) {
                        gmod[i] = a[i];
                        a[i] = a[i] + (alfkm1a * dx[i]);
                        gmod[i] = gmod[i] - a[i];
                    }

                    aDiff = dnrm2(param, gmod, 1);
                    alpha = alfkm1a;
                    phikp1 = phikm1;
                    iev = k2;

                    return;
                } // if (!reduce)

                // MINIMIZE IN R[param].USE THREE POINTS:ALFKM2a, ALFKM1a AND ALFK.
                // THE NEW SUGGESTION OF STEPLENGTH IS ALFKP1.
                // PK IS THE VALUE OF THE APPROXIMATING FUNCTION AT ALFKP1

                minrn(phikm2, pmax);
            } // while (loop)
        } // if ((-diff <= tau*dphize*alfk) || (phik < gamma*phi))

        // TAKE A PURE GOLDSTEIN-ARMIJO STEP

        k2++;
        gauc(gmod, dphize, tau, pmax);

        if (k2 < -10) {
            errorStatus = k2;

            return;
        }

        // COMPARE TWO EXPRESSIONS FOR THE DERIVATIVE OF PHI(a+alpha*dx)
        // AT ALPHA=0. IF INCONSISTENCY IS DETECTED K2 IS SET=-1 ON RETURN.

        if (errorStatus == -2) {
            chder(dphize);
        }

        if (k2 < -10) {
            errorStatus = k2;

            return;
        }

        alfkm1a = alfk;
        phikm1 = phik;

        // COMPUTE THE NEW POINT AND THE DIFFERENCE II aOld-aNew II

        for (i = 0; i < param; i++) {
            gmod[i] = a[i];
            a[i] = a[i] + (alfkm1a * dx[i]);
            gmod[i] = gmod[i] - a[i];
        }

        aDiff = dnrm2(param, gmod, 1);
        alpha = alfkm1a;
        phikp1 = phikm1;
        iev = k2;

        return;
    }

    /**
     * DOCUMENT ME!
     *
     * @param  fnew  DOCUMENT ME!
     * @param  v2    DOCUMENT ME!
     */
    private void linuc2(double[] fnew, double[] v2) {

        // Form the vector v2 as the divided difference vector of
        // second order
        double f1, f2, f3, alf;
        int i;

        alf = alfk;

        for (i = 0; i < nPts; i++) {
            f1 = fnew[i];
            f2 = residuals[i];
            f3 = v[i];
            v2[i] = (((f1 - f2) / alf) - f3) / alf;
        } // for (i = 0; i < nPts; i++)

        return;
    }

    /**
     * DOCUMENT ME!
     */
    private void lsunc() {
        // covarMat[][] 2D double ARRAY OF DIMENSION nPts *MAX(4,param)
        // IF exitStatus < 0 ON RETURN ARRAY covarMat IS UNDEFINED
        // OTHERWISE covarMat CONTAINS THE MATRIX R FROM THE
        // DECOMPOSITION    T
        // Q *J*D*E = (R)
        // (0)
        // IN THE UPPER TRIANGLE OF covarMat
        // WHERE
        // Q IS ORTHOGONAL (nPts*nPts)
        // J IS THE JACOBIAN (nPts*param) AT THE TERMINATING POINT
        // D IS A DIAGONAL MATRIX (param*param)
        // E IS A PERMUTATION MATRIX (param*param)
        // R IS AN UPPER TRIANGULAR MATRIX (param*param)
        // I.E.           T  -1
        // J = Q*(R)*E *D
        // (0)
        // AND
        // T      -1    T    T  -1     T   -1        -1   T -1  T
        // J *J = D  *E*R *R*E *D     (J *J)   = D*E*R  *(R )  *E *D

        // WHICH IS THE MAIN PART OF THE COVARIANCE MATRIX
        int i, paramCons;

        // a small positive value used to determine the pseudo rank
        // of the Jacobian
        double tau;
        double aNorm = 1.0;
        double norm;
        boolean loop;
        boolean doSoliuc;
        double c1 = 0.0;

        // validate input values
        if ((nPts < param) || (param <= 0) || (nPts <= 0) || (mdg < param) || (maxIterations <= 0) ||
                (tolerance <= 0.0) || (relativeConvergence < 0.0) || (absoluteConvergence < 0.0) ||
                (parameterConvergence < 0.0) ||
                ((relativeConvergence + absoluteConvergence + parameterConvergence) <= 0.0)) {
            exitStatus = -1;

            return;
        }

        // Initialize aset[i] and check values in bl[i] and bu[i]
        paramAct = 0;
        paramCons = 0;

        for (i = 0; i < param; i++) {

            if (bl[i] > bu[i]) {
                exitStatus = -1;

                return;
            }

            aset[i] = 0;

            if (a[i] < (bl[i] + c1)) {
                a[i] = bl[i];
            }

            if (a[i] > (bu[i] - c1)) {
                a[i] = bu[i];
            }

            if (a[i] == bu[i]) {
                aset[i] = -1;
            }

            if ((a[i] == bl[i]) || (bl[i] == bu[i])) {
                aset[i] = 1;
            }

            if (aset[i] != 0) {
                paramAct = paramAct + 1;
            }

            if (bl[i] == bu[i]) {
                paramCons = paramCons + 1;
            }
        } // for (i = 0; i < param; i++)

        // initialize variables
        iters = 0;
        ifree = 0;
        indic = 0;
        errorStatus = 0;
        funcEval = 0;
        jacobianEval = 0;
        secondEval = 0;
        lineEval = 0;
        restart = false;
        icount = 0;
        itotal = 0;
        tau = tolerance;
        aDiff = dnrm2(param, a, 1) * 2.0 * parameterConvergence;
        betkm1 = 0.0;
        alfkm1 = 1.0;
        alpha = 0.0;
        alfkm2 = 1.0;
        kodkm2 = 1;
        kod = 1;
        rngkm1 = Math.max(1, param - paramAct);
        lattry = rngkm1;
        kodkm1 = 1;
        bestpg = 0.0;
        aupkm1 = 0.0;

        // Evaluate at user supplied point
        ctrl = 1;
        ctrlMat[0] = ctrl;
        fitToFunction(a, residuals, covarMat);
        ctrl = ctrlMat[0];

        if (ctrl == -1) {
            exitStatus = -1;

            return;
        }

        funcEval++;
        norm = dnrm2(nPts, residuals, 1);
        phi = 0.5 * norm * norm;
        fsqkm1 = 2.0 * phi;

        if (paramCons == param) {
            exitStatus = -7;

            return;
        }

        // Main loop of iteration starts here
        loop = true;
        doSoliuc = true;

        while (loop) {

            if (doSoliuc) {
                soliuc(tau);

                if (errorStatus < -10) {
                    exitStatus = errorStatus;

                    return;
                }

                // ON RETURN BETA  IS THE NORM OF THE ORTHOGONAL PROJECTION OF
                // residuals ONTO THE SPACE SPANNED BY THE COLUMNS OF THE
                // JACOBIAN
                // PREKM1 IS PREDICTED REDUCTION IF PSEUDORANK-1
                // FROM PREVIOUS STEP IS USED
                // D1SQS IS PREDICTED REDUCTION IN THE OBJECTIVE
                // FUNCTION IF DX IS USED AS SEARCH DIRECTION
                // GNORM IS THE NORM OF THE GRADIENT DIVIDED BY THE
                // LENGTH OF THE LONGEST COLUMN OF THE JACOBIAN
                // PSEUDORANK IS PSEUDO RANK USED TO COMPUTE THE DIRECTION DX

                // COMPUTE THE NORM OF CURRENT POINT
                // THE NORM OF THE SEARCH DIRECTION
                // THE SUM OF SQUARES AT THE CURRENT POINT
                // A NOISE LEVEL INDICATOR

                aNorm = dnrm2(param, a, 1);
                dxnorm = dnrm2(param, dx, 1);
                norm = dnrm2(nPts, residuals, 1);
                fsum = norm * norm;

                if (dxnorm < srelpr) {
                    alfnoi = 1.0;
                } else {
                    alfnoi = Math.sqrt(srelpr) / dxnorm;
                }
            } // if (doSoliuc)

            doSoliuc = true;

            // Check main termination criteria
            termuc(aNorm);

            // If exitStatus <> 0, the iteration is finished
            if (exitStatus != 0) {
                speed = 0.0;

                if (betkm1 != 0.0) {
                    speed = beta / betkm1;
                }

                return;
            }

            // Analyze the past and sometimes recompute the search direction
            analuc();
            if (outputMes) {
	            Preferences.debug("POINT\n");
	
	            for (i = 0; i < param; i++) {
	                Preferences.debug(a[i] + "\n");
	            }
	
	            Preferences.debug("pseudorank = " + pseudoRank + "\n");
	            Preferences.debug("DIRECTION\n");
	
	            for (i = 0; i < param; i++) {
	                Preferences.debug(dx[i] + "\n");
	            }
            }

            if (errorStatus < -10) {
                exitStatus = errorStatus;

                return;
            }

            // ACCUMULATE FUNCTION EVALUATIONS
            funcEval = funcEval + eval;
            secondEval = secondEval + eval;

            // Check some abnormal termination criteria
            // ERROR = -3 if not positive definite Hessian
            if (errorStatus == -3) {
            	if (outputMes) {
                    System.out.println("Hessian matrix not positive definite\n");
                    System.out.println("Try Gauss-Newton\n");
                    Preferences.debug("Hessian matrix not positive definite\n");
                    Preferences.debug("Try Gauss-Newton\n");
            	}
                ifree = 5;
                indic = -3;

                continue;
            } // if (errorStatus == -3)

            // Save current point
            for (i = 0; i < param; i++) {
                w3[i] = a[i];
            }

            // Compute step length and take a step
            stepuc();

            if (errorStatus < -10) {
                exitStatus = errorStatus;

                return;
            }

            // Accumulate function evaluations
            funcEval = funcEval + eval;
            lineEval = lineEval + eval;

            // Possibly a restart is done
            // If no restart is done, variables representing the past
            // are updated
            evreuc();

            if (errorStatus < -10) {
                exitStatus = errorStatus;

                return;
            }

            // Output some information
            outuc();

            if (ifree > 0) {
                continue;
            }

            if (restart || (errorStatus < 0)) {
                doSoliuc = false;
            }
        } // while loop
    }

    /**
     * DOCUMENT ME!
     *
     * @param  v2    DOCUMENT ME!
     * @param  xmin  DOCUMENT ME!
     */
    private void minrm(double[] v2, double xmin) {
        // A SUBROUTINE WHICH FINDS THE POINT a WHERE THE FUNCTION P(X)= 0.5*II residuals+V*X+V2*X**2 II**2  IS
        // MINIMIZED. residuals, v AND v2 BELONG TO R[nPts] AND X IS A SCALAR. THE VALUES OF residuals, v AND v2 ARE
        // BASED ON TW0 FUNCTION VALUES :  F(0) AND F(ALPHA). THE FUNCTION P(X) IS ALWAYS >=0 AND IT IS A POLYNOMIAL OF
        // 4:TH DEGREE. THE MINIMUM VALUE OF P(X) IS ATTAINED AT A POINT X WHERE THE FIRST DERIVATIVE OF P(X)=DP(X) IS
        // ZERO. DP(X) IS A POLYNOMIAL OF 3:RD DEGREE. IN CASE OF THREE ROOTS (X1<X2<X3), X2 CORRESPONDS TO A LOCAL
        // MAXIMUM. CHOOSE THE ONE (OF X1 AND X3) THAT IS AT THE SAME SIDE OF THE MAXIMUM AS XMIN IS PROVIDED NO
        // EXTRAPOLATION IS DONE. HOWEVER,THE MINIMUM POINT X MUST LIE IN THE INTERVAL (ALFMIN,ALFMAX) . PX IS SET TO
        // P(X) AT THE MINIMUM POINT WHERE P(X) IS THE POLYNOMIAL ABOVE. beta2 IS SET TO THE OTHER MINIMIZER OF P(X)
        // (IF TWO ARE DETERMINED) OTHERWISE beta2 = X. pbeta = P(beta2)

        int k;
        double pprim, pbiss;
        double d, errorVar, h0, dm, hm, x0, eps;

        eps = 1.E-04;

        // COMPUTE NORMS AND SCALAR PRODUCTS

        minrm1(v2);
        pprim = pol3(xmin);
        pbiss = (6.0 * xmin * xmin * v2norm * v2norm) + (6.0 * xmin * scv1v2) + (2.0 * scv0v2) + (v1norm * v1norm);
        h0 = Math.abs(pprim / pbiss);
        dm = Math.abs((6.0 * scv1v2) + (12.0 * xmin * v2norm * v2norm)) + (24.0 * h0 * v2norm * v2norm);

        // DETERMINE IF DP(X)=0 SHOULD BE SOLVED BY USING NEWTONS METHOD

        hm = Math.max(h0, 1.0);

        if (pbiss <= (20.0 * hm * dm)) {

            // COMPUTE QUANTITIES P, Q, DELTA AND A1DIV3 SO THAT THE SOLUTION OF
            // X * * 3 + A1 * X * * 2 + A2 * X + A3 = 0 IS X = T - A1 / 3
            // WHERE T SOLVES T * * 3 + P * T + Q = 0

            minrm2();

            // MATHEMATICALLY WE DESTINGUISH THREE DIFFERENT CASES:
            // IF deltaMinrm > 0 THEN DF(X) = 0 HAS ONE REAL ROOT
            // IF deltaMinrm = 0 THEN DF(X) = 0 HAS ONE SINGLE AND ONE DOUBLE REAL ROOT
            // IF deltaMinrm < 0 THEN DF(X) = 0 HAS THREE DIFFERENT REAL ROOTS

            // IF deltaMinrm = 0 THE ONLY ROOT OF INTEREST IS THE SINGLE ONE, SO
            // NUMERICALLY WE DISTINGUISH TWO CASES:deltaMinrm >= 0
            // AND deltaMinrm < 0

            if (deltaMinrm >= 0.0) {

                // DELTA >= 0 ONE INTERESTING ROOT.X

                oner();
                beta2 = alfkp1;
                // MAKE THE MINIMUM POINT alfkp1 LIE IN THE INTERVAL
                // (ALFMIN,ALFMAX) AND EVALUATE F(alfkp1) AT THE MINIMUM POINT

                alfkp1 = Math.min(alfkp1, alfmax);
                alfkp1 = Math.max(alfkp1, alfmin);
                pk = pol4(v2, alfkp1);
                beta2 = alfkp1;
                pbeta = pk;

                return;
            } // if (deltaMinrm >= 0.0)

            // DELTA < 0 TWO INTERESTING ROOTS.Y AND Z, Y < Z
            twor();

            // CHOOSE alfkp1 = x1Minrm OR x2Minrm OR x3Minrm

            choose(xmin, v2);
            // MAKE THE MINIMUM POINT alfkp1 LIE IN THE INTERVALL
            // (ALFMIN,ALFMAX) AND EVALUATE F(alfkp1) AT THE MINIMUM POINT

            alfkp1 = Math.min(alfkp1, alfmax);
            alfkp1 = Math.max(alfkp1, alfmin);
            pk = pol4(v2, alfkp1);
            beta2 = Math.min(beta2, alfmax);
            beta2 = Math.max(beta2, alfmin);

            if (deltaMinrm >= 0.0) {
                beta2 = alfkp1;
                pbeta = pk;
            }

            return;
        } // if (pbiss <= 20.0*hm*dm)

        deltaMinrm = 1.0;

        // ITERATE USING NEWTONS METHOD

        k = 0;
        x0 = xmin;

        do {
            pprim = pol3(x0);
            pbiss = (6.0 * x0 * x0 * v2norm * v2norm) + (6.0 * x0 * scv1v2) + (2.0 * scv0v2) + (v1norm * v1norm);
            d = -pprim / pbiss;
            alfkp1 = x0 + d;
            errorVar = 2.0 * dm * d * d / Math.abs(pbiss);
            x0 = alfkp1;
            k++;
        } while ((errorVar > eps) && (k < 3));

        beta2 = alfkp1;

        // MAKE THE MINIMUM POINT alfkp1 LIE IN THE INTERVALL
        // (ALFMIN,ALFMAX) AND EVALUATE F(alfkp1) AT THE MINIMUM POINT

        alfkp1 = Math.min(alfkp1, alfmax);
        alfkp1 = Math.max(alfkp1, alfmin);
        pk = pol4(v2, alfkp1);
        beta2 = Math.min(beta2, alfmax);
        beta2 = Math.max(beta2, alfmin);

        if (deltaMinrm >= 0.0) {
            beta2 = alfkp1;
            pbeta = pk;
        }

        return;
    }

    /**
     * DOCUMENT ME!
     *
     * @param  v2  DOCUMENT ME!
     */
    private void minrm1(double[] v2) {
        // COMPUTE THE EUCLIDEAN NORM OF THE nPts-DIMENSIONAL VECTORS
        // residuals, v, and v2

        double sc1, sc2, sc3;
        int i;

        v0norm = dnrm2(nPts, residuals, 1);
        v1norm = dnrm2(nPts, v, 1);
        v2norm = dnrm2(nPts, v2, 1);

        // SCALE THE VECTORS

        if (v0norm != 0.0) {
            scalv(residuals, v0norm, nPts);
        }

        if (v1norm != 0.0) {
            scalv(v, v1norm, nPts);
        }

        if (v2norm != 0.0) {
            scalv(v2, v2norm, nPts);
        }

        // COMPUTE THE SCALAR PRODUCTS residuals(T)*V, residuals(T)*v2,
        // v(T)*v2

        sc1 = 0.0;
        sc2 = 0.0;
        sc3 = 0.0;

        for (i = 0; i < nPts; i++) {
            sc1 = sc1 + (residuals[i] * v[i]);
            sc2 = sc2 + (residuals[i] * v2[i]);
            sc3 = sc3 + (v[i] * v2[i]);
        }

        scv0v1 = sc1 * v0norm * v1norm;
        scv0v2 = sc2 * v0norm * v2norm;
        scv1v2 = sc3 * v1norm * v2norm;

        // RESCALE THE VECTORS

        if (v0norm != 0.0) {
            scalv(residuals, 1.0 / v0norm, nPts);
        }

        if (v1norm != 0.0) {
            scalv(v, 1.0 / v1norm, nPts);
        }

        if (v2norm != 0.0) {
            scalv(v2, 1.0 / v2norm, nPts);
        }

        return;
    }

    /**
     * DOCUMENT ME!
     */
    private void minrm2() {
        // SUPPOSE WE HAVE A FOURTH DEGREE POLYNOMIAL II V0+V1*X+V2*X**2 II**2.
        // THE DERIVATIVE IS A THIRD DEGREE POLYNOMIAL WHICH CAN BE
        // WRITTEN    T**3+P*T+Q
        // WHERE T=X+A1 AND P,Q DEFINED BELOW.

        double a1, a2, a3;
        double temp, temp2;

        a1 = 1.5 * scv1v2 / v2norm / v2norm;
        temp = v1norm / v2norm;
        a2 = 0.5 * ((temp * temp) + (scv0v2 / v2norm / v2norm * 2.0));
        a3 = 0.5 * scv0v1 / v2norm / v2norm;
        pMinrm = a2 - (a1 * a1 / 3.0);
        qMinrm = a3 - (a1 * a2 / 3.0) + (2.0 * a1 * a1 * a1 / 27.0);
        temp = qMinrm / 2.0;
        temp2 = pMinrm / 3.0;
        deltaMinrm = (temp * temp) + (temp2 * temp2 * temp2);
        a1div3 = a1 / 3.0;

        return;
    }

    /**
     * DOCUMENT ME!
     *
     * @param  phikm2  DOCUMENT ME!
     * @param  pmax    DOCUMENT ME!
     */
    private void minrn(double phikm2, double pmax) {
        // PROVIDED THE POINTS alfk, alfkm2a AND alfkm1a ARE NOT TOO CLOSE, THE QUADRATIC PASSING THROUGH (alfk,phik),
        // (alfkm2a,phikm2) AND (alfkm1a,phikm1) IS DETERMINED. alfkp1 = THE MINIMUM POINT OF THIS QUADRATIC. pk = THE
        // VALUE OF THE QUADRATIC AT alfkp1

        double eps, t1, t2, t3;

        eps = Math.sqrt(srelpr) / pmax;
        alfkp1 = alfk;
        pk = phik;

        if ((Math.abs(alfkm2a - alfk) < eps) || (Math.abs(alfkm1a - alfk) < eps) ||
                (Math.abs(alfkm1a - alfkm2a) < eps)) {
            return;
        }

        quamin(phikm2);
        alfkp1 = Math.min(alfkp1, alfmax);
        alfkp1 = Math.max(alfkp1, alfmin);
        t1 = (alfkp1 - alfk) * (alfkp1 - alfkm2a) * phikm1 / (alfkm1a - alfk) / (alfkm1a - alfkm2a);
        t2 = (alfkp1 - alfkm1a) * (alfkp1 - alfkm2a) * phik / (alfk - alfkm1a) / (alfk - alfkm2a);
        t3 = (alfkp1 - alfkm1a) * (alfkp1 - alfk) * phikm2 / (alfkm2a - alfk) / (alfkm2a - alfkm1a);
        pk = t1 + t2 + t3;

        return;
    }

    /**
     * DOCUMENT ME!
     */
    private void newunc() {
        // COMPUTE THE JACOBIAN OF residuals(a) AT THE CURRENT POINT AND
        // STORE IN ARRAY covarMat

        // ON ENTRY

        // a[]  CONTAINS THE CURRENT POINT
        // residuals  CONTAINS THE VECTOR OF RESIDUALS AT THE CURRENT POINT
        // param, nPts  THE LENGTH OF a AND residuals RESPECTIVELY
        // errorStatus  EQUALS ZERO

        // ON RETURN

        // covarMat CONTAINS THE nPts*param JACOBIAN MATRIX
        // funcEval CONTAINS THE TOTAL NO.OF FUNCTION EVALUATIONS DONE SO FAR
        // jacobianEval CONTAINS NO. OF FUNCTION EVALUATIONS CAUSED BY COMPUTING
        // THE JACOBIAN WITH DIFFERENCE METHODS
        // errorStatus CONTAINS A USER STOP INDICATION <-10 OR UNCHANGED


        ctrl = 2;

        // IF CTRL=2 ON ENTRY TO fitToFunction THE JACOBIAN MATRIX IS REQUESTED.
        // HOWEVER, IF THE ANALYTICAL JACOBIAN IS NOT AVAILABLE THE USER
        // SIGNALS THAT BY SETTING CTRL TO ZERO ON RETURN

        ctrlMat[0] = ctrl;
        fitToFunction(a, residuals, covarMat);
        ctrl = ctrlMat[0];

        if (ctrl == 2) {
            return;
        }

        if (ctrl < -10) {
            errorStatus = ctrl;

            return;
        }

        // COMPUTE THE JACOBIAN USING FORWARD DIFFERENCES

        if (ctrl == 0) {
            jacdif();
        }

        if (ctrl < -10) {
            errorStatus = ctrl;

            return;
        }

        funcEval = funcEval + param;
        jacobianEval = jacobianEval + param;

        return;
    }

    /**
     * DOCUMENT ME!
     */
    private void oner() {
        // COMPUTE THE ROOT OF A THIRD DEGREE POLYNOMIAL WHEN THERE
        // IS ONLY ONE REAL ROOT

        double arg1, arg2, a3rd, sqd, s1, s2, t;

        sqd = Math.sqrt(deltaMinrm);
        arg1 = (-qMinrm / 2.0) + sqd;

        if (arg1 >= 0.0) {
            s1 = 1.0;
        } else {
            s1 = -1.0;
        }

        arg2 = (-qMinrm / 2.0) - sqd;

        if (arg2 >= 0.0) {
            s2 = 1.0;
        } else {
            s2 = -1.0;
        }

        a3rd = 1.0 / 3.0;
        t = (s1 * Math.pow(Math.abs(arg1), a3rd)) + (s2 * Math.pow(Math.abs(arg2), a3rd));
        alfkp1 = t - a1div3;

        return;
    }

    /**
     * DOCUMENT ME!
     */
    private void outuc() {
        // ON ENTRY

        // iters INTEGER SCALAR CONTAINING ITERATION COUNT
        // restart LOGICAL SCALAR = TRUE IF RESTART IS DONE
        // = FALSE IF NO RESTART IS DONE
        // eval INTEGER SCALAR CONTAINING NO. OF FUNCTION EVALUATIONS
        // DONE INSIDE THE LINESEARCH ROUTINE FOR LATEST STEP
        // phi double SCALAR CONTAINING THE VALUE OF THE OBJECTIVE
        // FUNCTION AT CURRENT POINT
        // gnorm double SCALAR CONTAINING THE NORM OF THE GRADIENT OF
        // THE OBJECTIVE FUNCTION AT THE PREVIOUS POINT DIVIDED BY
        // THE LONGEST COLUMN OF THE JACOBIAN

        // param INTEGER SCALAR CONTAINING THE NO. OF UNKNOWNS
        // paramAct INTEGER SCALAR CONTAINING THE NO. OF ACTIVE CONSTRAINTS
        // aset[] INTEGER 1D ARRAY OF DIMENSION param
        // CONTAINING A CODE WHICH INDICATES WHETHER AN UNKNOWN IS
        // ACTIVE OR NOT.
        // aset[i] = 0 WHEN a[i] IS FREE
        // = +1 WHEN a[i] EQUALS bl[i]
        // = -1 WHEN a[i] EQUALS bu[i]
        // ON RETURN:

        // speed double SCALAR CONTAINING AN ESTIMATE OF THE LINEAR
        // CONVERGENCE FACTOR

        int itno;
        double reduc, ac, pred;

        if (restart) {
            return;
        }

        if (ifree == 5) {
            return;
        }

        speed = 0.0;
        itno = iters - 1;

        if ((itno != 0) && (betkm2 != 0.0)) {
            speed = betkm1 / betkm2;
        }

        reduc = fsqkm1 - (2.0 * phi);
        ac = Math.min(1.0, aupkm1);
        pred = ac * (2.0 - ac) * d1km1;
        if (outputMes) {
	        Preferences.debug("Collected information for iteration steps\n");
	        Preferences.debug("itno = " + itno + "\n");
	        Preferences.debug("fsqkm1 = " + fsqkm1 + "\n");
	        Preferences.debug("gnorm = " + gnorm + "\n");
	        Preferences.debug("dxnkm1 = " + dxnkm1 + "\n");
	        Preferences.debug("kodkm1 = " + kodkm1 + "\n");
	        Preferences.debug("rngkm1 = " + rngkm1 + "\n");
	        Preferences.debug("alfkm1 = " + alfkm1 + "\n");
	        Preferences.debug("eval = " + eval + "\n");
	        Preferences.debug("pred = " + pred + "\n");
	        Preferences.debug("speed = " + speed + "\n");
	        Preferences.debug("phi = " + phi + "\n");
	        Preferences.debug("reduc = " + reduc + "\n");
	
	        if (paramAct > 0) {
	
	            for (i = 0; i < param; i++) {
	                Preferences.debug("aset[" + i + "] = " + aset[i] + "\n");
	            }
	        }
        }

        return;
    }

    /**
     * DOCUMENT ME!
     *
     * @param  work  DOCUMENT ME!
     */
    private void pivec(double[] work) {
        // COMPUTE W:= pivit * dx  WHERE pivit REPRESENTS A param*param PERMUTATION
        // MATRIX AND dx IS A param-VECTOR
        // pivit[j] CONTAINS THE INDEX OF THE ELEMENT THAT WAS MOVED INTO POSITION J

        // ON RETURN

        // dx CONTAINS THE TRANSFORMED VECTOR W ABOVE

        int i, k;

        if (param == 0) {
            return;
        }

        for (i = 0; i < param; i++) {
            k = pivit[i];
            work[k] = dx[i];
        } // for (i = 0; k < param; i++)

        for (i = 0; i < param; i++) {
            dx[i] = work[i];
        }

        return;
    }

    /**
     * DOCUMENT ME!
     *
     * @param   x  DOCUMENT ME!
     *
     * @return  DOCUMENT ME!
     */
    private double pol3(double x) {
        // EVALUATE A 3:RD DEGREE POLYNOMIAL
        // A3*X**3+A2*X**2+A1*X+A0   AT X
        // WHERE  A0 = SCV0V1
        // A1 = 2*SCV0V2+V1NORM**2
        // A2 = 3*SCV1V2
        // A3 = 2*V2NORM**2

        double fn_val;
        fn_val = scv0v1 + (x * v1norm * v1norm) + (2.00 * x * scv0v2) + (x * x * 3.00 * scv1v2) +
                 (x * x * x * v2norm * v2norm * 2.0);

        return fn_val;
    }

    /**
     * DOCUMENT ME!
     *
     * @param   v2  DOCUMENT ME!
     * @param   x   DOCUMENT ME!
     *
     * @return  DOCUMENT ME!
     */
    private double pol4(double[] v2, double x) {
        double p, s;
        int i;
        double fn_val;

        // EVALUATE THE 4:TH DEGREE POLYNOMIAL || V2*X**2+V*X+residuals ||**2 AT X
        s = 0.0;

        for (i = 0; i < nPts; i++) {
            p = residuals[i] + (x * v[i]) + (x * x * v2[i]);
            s = s + (p * p);
        }

        fn_val = 0.5 * s;

        return fn_val;
    }

    /**
     * DOCUMENT ME!
     *
     * @param  work    DOCUMENT ME!
     * @param  mindim  DOCUMENT ME!
     */
    private void pregn(double[] work, int mindim) {
        // GN-STEP IN PREVIOUS STEP
        // TAKE rank AS THE LARGEST k (mindim<=k<=pseudoRank-1) FOR WHICH
        // w1[k-1] < smax*sn AND work[-1k] > rmin*bn
        // IF NO SUCH k EXISTS TAKE rank =pseudoRank-1 PROVIDED
        // (pseudoRank-1) >= mindim

        int i, k, m1;

        smax = 0.2;
        rmin = 0.5;
        m1 = pseudoRank - 1;
        k = pseudoRank;

        if (mindim > m1) {
            rank = k;

            return;
        }

        for (i = mindim; i <= m1; i++) {
            k = m1 - i + mindim;

            if ((w1[k - 1] < (smax * sn)) && (work[k - 1] > (rmin * bn))) {
                rank = k;

                return;
            }
        } // for (i = mindim; i <= m1; i++)

        rank = Math.max(mindim, pseudoRank - 1);

        return;
    }

    /**
     * DOCUMENT ME!
     *
     * @param  work  DOCUMENT ME!
     */
    private void presub(double[] work) {
        // SUBSPACE MINIMIZATION IN LATEST STEP

        int i, i1;
        double pgress;

        stepb = 0.2;
        pgb1 = 0.3;
        pgb2 = 0.1;
        predb = 0.7;
        rlenb = 2.0;

        // IF THE LATEST STEP WAS FAIRLY GOOD THE DIMENSION MUST NOT BE DECREASED

        pgress = Math.sqrt(fsqkm1 - fsum);

        if ((alfkm1 < stepb) && (pgress <= (pgb1 * Math.sqrt(d1km1))) && (pgress <= (pgb2 * betkm1))) {

            // A BAD STEP

            rank = Math.max(1, rngkm1 - 1);

            if ((rngkm1 > 1) && (work[rank - 1] > (rabs * bn))) {
                return;
            }
        }

        rank = rngkm1;

        if ((work[rank - 1] > (predb * bn)) && ((rlenb * w1[rank - 1]) < w1[rank])) {
            return;
        }

        i1 = rngkm1 + 1;

        for (i = i1; i <= pseudoRank; i++) {
            rank = i;

            if (work[i - 1] > (predb * bn)) {
                break;
            }
        } // for (i = i1; i <= pseudoRank; i++)

        return;
    }

    /**
     * DOCUMENT ME!
     *
     * @param  phikm2  DOCUMENT ME!
     */
    private void quamin(double phikm2) {
        // COMPUTE THE MINIMUM POINT U OF A QUADRATIC POLYNOMIAL PASSING
        // THROUGH (alfkm2a,phikm2), (alfkm1a,phikm1), and (alfk,phik)

        double d1, d2, s, q;

        d1 = phikm2 - phik;
        d2 = phikm1 - phik;
        s = ((alfkm1a - alfk) * (alfkm1a - alfk) * d1) - ((alfkm2a - alfk) * (alfkm2a - alfk) * d2);
        q = 2.0 * (((alfkm2a - alfk) * d2) - ((alfkm1a - alfk) * d1));
        alfkp1 = alfk - (s / q);

        return;
    }

    /**
     * DOCUMENT ME!
     */
    private void reavuc() {
        // THIS IS THE RESTART ADVISOR ROUTINE FOR UNCONSTRAINED PROBLEMS

        // ON ENTRY

        // errorStatus INTEGER SCALAR CONTAINING
        // -1 IF NO DESCENT DIRECTION
        // -2 IF LATEST alpha*II dx II < SQRT(srelpr) OR
        // IF alpha <= A LOWER BOUND ON THE STEPLENGTH
        // -3 IF NOT POS. DEF. MATRIX FROM 2:ND ORDER METHOD
        // -4 IF NOT ALLOWED TO USE 2:ND DERIVATIVES
        // 0 OTHERWISE
        // alpha double SCALAR CONTAINING THE LATEST STEP LENGTH
        // alphup double SCALAR CONTAINING THE LATEST UPPER BOUND
        // FOR THE STEPLENGTH
        // fsum double SCALAR CONTAINING THE SUM OF SQUARES
        // AT THE PREVIOUS POINT
        // phi double SCALAR CONTAINING THE VALUE OF THE OBJECTIVE
        // FUNCTION AT THE CURRENT POINT
        // restart LOGICAL SCALAR EQUAL TO FALSE
        // lattry INTEGER SCALAR STORED EQUAL TO ZERO
        // IF NO CHECK SHALL BE DONE

        // ON RETURN

        // errorStatus  IS SET =-5 IF THE OBJECTIVE INCREASES
        // restart IS SET = TRUE IF THE LATEST STEPLENGTH IS TOO SHORT

        if (outputMes) {
    	    Preferences.debug("In reavuc: alpha = " + alpha + "\n");
            Preferences.debug("In reavuc: alphup = " + alphup + "\n");
        }

        if ((lattry == 0) && (bestpg > 0.0)) {
            return;
        }

        if ((lattry == 0) && (bestpg <= 0.0)) {
            errorStatus = -5;

            return;
        }

        if ((errorStatus == -1) || (errorStatus <= -3)) {
            return;
        }

        if ((errorStatus != -2) && (phi >= (0.5 * fsum))) {
            errorStatus = -5;

            return;
        }

        if (alpha <= (alphup / 3000.0)) {
            restart = true;
        }

        return;
    }

    /**
     * DOCUMENT ME!
     *
     * @param  pk    DOCUMENT ME!
     * @param  eta   DOCUMENT ME!
     * @param  gmod  DOCUMENT ME!
     * @param  fnew  DOCUMENT ME!
     */
    private void reduc(double pk, double eta, double[] gmod, double[] fnew) {
        // REDUCE IS SET TO TRUE IF ESSENTIAL REDUCTION OF THE OBJECTIVE FUNCTION IS LIKELY. OTHERWISE REDUCE IS SET TO
        // FALSE

        double c1, delta;
        int i;
        delta = 0.2;

        c1 = nPts * Math.sqrt(srelpr);

        if (!(((phikm1 - pk) > (eta * diff)) || ((pk + c1) < (delta * phikm1)))) {

            for (i = 0; i < nPts; i++) {
                residuals[i] = fnew[i];
            }

            reduce = false;

            return;
        } // if (!((phikm1-pk > eta*diff) || (pk+c1 < delta*phikm1)))

        for (i = 0; i < nPts; i++) {
            residuals[i] = fnew[i];
        }

        ctrl = -1;
        fsumsq(alfk, gmod, fnew, phiMat);
        phik = phiMat[0];
        if (outputMes) {
            Preferences.debug("In REDUC: phik = " + phik + " alfk = " + alfk + "\n");
        }

        if (ctrl < -10) {
            k2 = ctrl;
        }

        if (k2 < 0) {
            return;
        }

        k2++;
        reduce = true;

        if (((phikm1 - phik) > (eta * diff)) || (phik < (delta * phikm1))) {
            return;
        }

        // TERMINATE BUT CHOOSE THE BEST POINT OUT OF alfkm1a AND ALFK

        if (phikm1 <= phik) {
            reduce = false;

            return;
        }

        alfkm1a = alfk;
        phikm1 = phik;

        for (i = 0; i < nPts; i++) {
            residuals[i] = fnew[i];
        }

        reduce = false;

        return;
    }

    /**
     * DOCUMENT ME!
     */
    private void releps() {
        // COMPUTE SRELPR = DOUBLE RELATIVE PRECISION FOR A BINARY
        // MACHINE   I.E.
        // DETERMINE THE SMALLEST POSITIVE NUMBER 0.5**K FOR WHICH
        // (1.0+0.5**K) > 1.0  AND  (1.0+0.5**(K+1)) = 1.0
        // WHERE K IS A POSITIVE INTEGER

        double temp, frac;
        boolean loop = true;
        frac = 1.0;

        while (loop) {
            frac = 0.5 * frac;
            temp = frac + 1.0;

            if (temp == 1.0) {
                break;
            }
        } // while (loop)

        srelpr = 2.0 * frac;

        return;
    }

    /**
     * DOCUMENT ME!
     *
     * @param  nn  DOCUMENT ME!
     */
    private void rtrw1(int nn) {
        // T
        // FORM  dx:= -(R :0)*w1
        // THE nn by nn UPPER TRIANGULAR MATRIX R IS CONTAINED IN THE UPPER
        // LEFT TRIANGLE OF ARRAY covarMat

        int i, j;

        for (j = 0; j < nn; j++) {
            dx[j] = 0.0;

            for (i = 0; i <= j; i++) {
                dx[j] = dx[j] - (covarMat[i][j] * w1[i]);
            }
        } // for (j = 0; j < nn; j++)

        return;
    }

    /**
     * DOCUMENT ME!
     *
     * @param  v       DOCUMENT ME!
     * @param  factor  DOCUMENT ME!
     * @param  n       DOCUMENT ME!
     */
    private void scalv(double[] v, double factor, int n) {

        // Form the new contents of vector v as
        // v[i] = v[i]/factor for i = 0,...,n-1
        int i;

        for (i = 0; i < n; i++) {
            v[i] = v[i] / factor;
        }

        return;
    }

    /**
     * DOCUMENT ME!
     */
    private void scaunc() {
        // If internalScaling is false,
        // THEN NO SCALING IS DONE
        // ELSE SCALE THE nPts*param MATRIX covarMat
        // SO THAT EACH COLUMN HAS UNIT LENGTH

        // ON RETURN

        // CNORM SET TO THE MAXIMUM COLUMN LENGTH OF THE MATRIX covarMat
        // IF SCALING IS DONE  covarMat := covarMat*D WHERE D IS A DIAGONAL MATRIX
        // WITH DIAGONAL ELEMENTS D(I)=1/LENGTH(I) WHERE LENGTH(I)
        // IS THE LENGTH OF COLUMN NO. I UNLESS LENGTH(I)=0
        // WHEN D(I) IS SET TO 1.0
        // DIAG() IF SCALING IS DONE DIAG(I) HOLDS THE DIAGONAL ELEMENTS
        // OF MATRIX D ABOVE  (I=0,1,......,param-1)

        int j, k;
        double colj;
        double[] covarPart = new double[nPts];

        cnorm = 0.0;

        for (j = 0; j < param; j++) {

            for (k = 0; k < nPts; k++) {
                covarPart[k] = covarMat[k][j];
            }

            colj = dnrm2(nPts, covarPart, 1);

            if (colj > cnorm) {
                cnorm = colj;
            }

            if (!internalScaling) {
                continue;
            }

            if (colj == 0.0) {
                colj = 1.0;
            }

            for (k = 0; k < nPts; k++) {
                covarMat[k][j] = covarMat[k][j] / colj;
            }

            diag[j] = 1.0 / colj;
        } // for (j = 0; j < param; j++)

        if (cnorm == 0.0) {
            cnorm = 1.0;
        }

        return;
    }

    /**
     * DOCUMENT ME!
     *
     * @param  nn    DOCUMENT ME!
     * @param  jpvt  DOCUMENT ME!
     * @param  job   DOCUMENT ME!
     */
    private void schdc(int nn, int[] jpvt, int job) {

        /** SCHDC COMPUTES THE CHOLESKY DECOMPOSITION OF A POSITIVE DEFINITE
         *  MATRIX.  A PIVOTING OPTION ALLOWS THE USER TO ESTIMATE THE CONDITION OF A POSITIVE DEFINITE MATRIX OR
         * DETERMINE THE RANK OF A POSITIVE SEMIDEFINITE MATRIX.
         *
         * ON ENTRY
         *
         * covarMat    CONTAINS THE MATRIX WHOSE DECOMPOSITION IS TO    BE COMPUTED.  ONLT THE UPPER HALF OF covarMat NEED
         * BE STORED.    THE LOWER PART OF THE ARRAY covarMat IS NOT REFERENCED.
         *
         * nn  INTEGER.    nn IS THE ORDER OF THE MATRIX.
         *
         * w0 double    w0 IS A WORK ARRAY.
         *
         * jpvt   INTEGER(nn).       jpvt CONTAINS INTEGERS THAT CONTROL THE SELECTION       OF THE PIVOT ELEMENTS, IF
         * PIVOTING HAS BEEN REQUESTED.       EACH DIAGONAL ELEMENT covarMat(K,K)       IS PLACED IN ONE OF THREE
         * CLASSES ACCORDING TO THE       VALUE OF JPVT(K).
         *
         *     IF JPVT(K) .GT. 0, THEN X(K) IS AN INITIAL                               ELEMENT.
         *
         *     IF JPVT(K) .EQ. 0, THEN X(K) IS A FREE ELEMENT.
         *
         *     IF JPVT(K) .LT. 0, THEN X(K) IS A FINAL ELEMENT.
         *
         *  BEFORE THE DECOMPOSITION IS COMPUTED, INITIAL ELEMENTS    ARE MOVED BY SYMMETRIC ROW AND COLUMN INTERCHANGES
         * TO    THE BEGINNING OF THE ARRAY covarMat AND FINAL    ELEMENTS TO THE END.  BOTH INITIAL AND FINAL ELEMENTS
         *   ARE FROZEN IN PLACE DURING THE COMPUTATION AND ONLY    FREE ELEMENTS ARE MOVED.  AT THE K-TH STAGE OF THE
         *  REDUCTION, IF covarMat(K,K) IS OCCUPIED BY A FREE ELEMENT    IT IS INTERCHANGED WITH THE LARGEST FREE
         * ELEMENT    covarMat(L,L) WITH L .GE. K.  JPVT IS NOT REFERENCED IF    JOB .EQ. 0.
         *
         * JOB     INTEGER.    JOB IS AN INTEGER THAT INITIATES COLUMN PIVOTING.    IF JOB .EQ. 0, NO PIVOTING IS DONE.
         * IF JOB .NE. 0, PIVOTING IS DONE.
         *
         * ON RETURN
         *
         * covarMat      covarMat CONTAINS IN ITS UPPER HALF THE CHOLESKY FACTOR    OF THE MATRIX covarMat AS IT HAS BEEN
         * PERMUTED BY PIVOTING.
         *
         * JPVT   JPVT(J) CONTAINS THE INDEX OF THE DIAGONAL ELEMENT    OF covarMat THAT WAS MOVED INTO THE J-TH POSITION,
         *    PROVIDED PIVOTING WAS REQUESTED.
         *
         * INFO   CONTAINS THE INDEX OF THE LAST POSITIVE DIAGONAL    ELEMENT OF THE CHOLESKY FACTOR.
         *
         * FOR POSITIVE DEFINITE MATRICES INFO = nn-1 IS THE NORMAL RETURN. FOR PIVOTING WITH POSITIVE SEMIDEFINITE
         * MATRICES INFO WILL IN GENERAL BE LESS THAN nn-1.  HOWEVER, INFO MAY BE GREATER THAN THE RANK OF covarMat-1,
         * SINCE ROUNDING ERROR CAN CAUSE AN OTHERWISE ZERO ELEMENT TO BE POSITIVE. INDEFINITE SYSTEMS WILL ALWAYS
         * CAUSEINFO TO BE LESS THAN nn-1. */

        int pu, pl, plp1, j, jp, jt, k, kb, km1, kp1, l, maxl;
        double temp;
        double maxdia;
        boolean swapk, negk;
        boolean[] negkMat = new boolean[nn];

        for (k = 0; k < nn; k++) {
            negkMat[k] = false;
        }

        pl = 0;
        pu = -1;
        info = nn - 1;

        if (job != 0) {

            // PIVOTING HAS BEEN REQUESTED.
            // REARRANGE THE THE ELEMENTS ACCORDING TO JPVT.

            for (k = 0; k < nn; k++) {
                swapk = jpvt[k] > 0;
                negk = jpvt[k] < 0;
                jpvt[k] = k;

                if (negk) {
                    negkMat[k] = true;
                }

                if (!swapk) {
                    continue;
                }

                if (k == pl) {
                    pl++;

                    continue;
                }

                for (i = 0; i <= (pl - 1); i++) {
                    temp = covarMat[i][k];
                    covarMat[i][k] = covarMat[i][pl];
                    covarMat[i][pl] = temp;
                }

                temp = covarMat[k][k];
                covarMat[k][k] = covarMat[pl][pl];
                covarMat[pl][pl] = temp;
                plp1 = pl + 1;

                for (j = plp1; j < nn; j++) {

                    if (j < k) {
                        temp = covarMat[pl][j];
                        covarMat[pl][j] = covarMat[j][k];
                        covarMat[j][k] = temp;
                    } else {

                        if (j == k) {
                            continue;
                        }

                        temp = covarMat[k][j];
                        covarMat[k][j] = covarMat[pl][j];
                        covarMat[pl][j] = temp;
                    }
                } // for (j = plp1; j < nn; j++)

                jpvt[k] = jpvt[pl];
                negkMat[k] = negkMat[pl];
                jpvt[pl] = k;
                negkMat[pl] = false;
                pl++;
            } // for (k = 0; k < nn; k++)

            pu = nn - 1;

            for (kb = pl; kb < nn; kb++) {
                k = nn - 1 - kb + pl;

                if (!negkMat[k]) {
                    continue;
                }

                negkMat[k] = false;

                if (pu == k) {
                    pu--;

                    continue;
                }

                for (i = 0; i <= (k - 1); i++) {
                    temp = covarMat[i][k];
                    covarMat[i][k] = covarMat[i][pu];
                    covarMat[i][pu] = temp;
                }

                temp = covarMat[k][k];
                covarMat[k][k] = covarMat[pu][pu];
                covarMat[pu][pu] = temp;
                kp1 = k + 1;

                for (j = kp1; j < nn; j++) {

                    if (j < pu) {
                        temp = covarMat[k][j];
                        covarMat[k][j] = covarMat[j][pu];
                        covarMat[j][pu] = temp;
                    } else {

                        if (j == pu) {
                            continue;
                        }

                        temp = covarMat[k][j];
                        covarMat[k][j] = covarMat[pu][j];
                        covarMat[pu][j] = temp;
                    }
                } // for (j = kp1; j < nn; j++) {

                jt = jpvt[k];
                negk = negkMat[k];
                jpvt[k] = jpvt[pu];
                negkMat[k] = negkMat[pu];
                jpvt[pu] = jt;
                negkMat[pu] = negk;

                pu--;
            } // for (kb = pl; kb < nn; kb++)

        } // if (job != 0)

        for (k = 0; k < nn; k++) {

            // REDUCTION LOOP.

            maxdia = covarMat[k][k];
            kp1 = k + 1;
            maxl = k;

            // DETERMINE THE PIVOT ELEMENT.

            if ((k >= pl) && (k < pu)) {

                for (l = kp1; l <= pu; l++) {

                    if (covarMat[l][l] > maxdia) {
                        maxdia = covarMat[l][l];
                        maxl = l;
                    } // if (covarMat[l][l] > maxdia)
                } // for (l = kp1; l <= pu; l++)
            } // if ((k >= pl) && (k < pu))

            // QUIT IF THE PIVOT ELEMENT IS NOT POSITIVE.

            if (maxdia <= 0.0) {
                info = k - 1;

                break;
            } // if (maxdia <= 0.0)

            if (k != maxl) {

                // START THE PIVOTING AND UPDATE JPVT.

                km1 = k - 1;

                for (i = 0; i <= km1; i++) {
                    temp = covarMat[i][k];
                    covarMat[i][k] = covarMat[i][maxl];
                    covarMat[i][maxl] = temp;
                }

                covarMat[maxl][maxl] = covarMat[k][k];
                covarMat[k][k] = maxdia;
                jp = jpvt[maxl];
                jpvt[maxl] = jpvt[k];
                jpvt[k] = jp;
            } // if (k != maxl)

            // REDUCTION STEP. PIVOTING IS CONTAINED ACROSS THE ROWS.

            w0[k] = Math.sqrt(covarMat[k][k]);
            covarMat[k][k] = w0[k];

            for (j = kp1; j < nn; j++) {

                if (k != maxl) {

                    if (j < maxl) {
                        temp = covarMat[k][j];
                        covarMat[k][j] = covarMat[j][maxl];
                        covarMat[j][maxl] = temp;
                    } // if (j < maxl)
                    else if (j != maxl) {
                        temp = covarMat[k][j];
                        covarMat[k][j] = covarMat[maxl][j];
                        covarMat[maxl][j] = temp;
                    } // else if (j != maxl)
                } // if (k != maxl)

                covarMat[k][j] = covarMat[k][j] / w0[k];
                w0[j] = covarMat[k][j];
                temp = -covarMat[k][j];

                for (i = kp1; i <= j; i++) {
                    covarMat[i][j] = covarMat[i][j] + (temp * w0[i]);
                } // for (i = kp1; i <= j; i++)
            } // for (j = kp1; j < nn; j++)
        } // for (k = 0; k < nn; k++)

        return;
    }

    /**
     * DOCUMENT ME!
     */
    private void secuc() {
        // COMPUTE THE SOLUTION (dx) OF THE param*param SYSTEM
        // T
        // gmat*dx =  -J * residuals               (1)
        // WHERE
        // T       nPts-1
        // gmat = J *J + SIGMA(residuals *G )
        // K=0           K  K
        // AND

        // J IS THE nPts*param JACOBIAN OF residuals(a) AT THE CURRENT POINT
        // G  (param*param) IS THE K:TH HESSIAN OF residuals(a) AT THE CURRENT POINT
        // K
        // WE KNOW THE QR-DECOMPOSITION OF THE JACOBIAN J ON ENTRY

        // ON ENTRY

        // fitTOFunction SUBROUTINE NAME USED TO EVALUATE THE FUNCTIONS AT A CERTAIN POINT
        // a[]   CONTAINS THE CURRENT POINT
        // diag[] CONTAINS THE DIAGONAL ELEMENTS OF THE DIAGONAL MATRIX
        // D BELOW (IF SCALING IS DONE. OTHERWISE UNDEFINED)
        // pivit[] CONTAINS THE PERMUTATION MATRIX E BELOW
        // w0 CONTAINS INFO. NEEDED TO FORM MATRIX Q BELOW
        // param CONTAINS LENGTH OF THE ARRAYS a,dx,diag,pivit,w0,w3
        // internalScaling = false IF NO SCALING OF THE JACOBIAN IS DONE
        // = true IF SCALING IS DONE
        // residuals[] CONTAINS THE VECTOR OF RESIDUALS AT CURRENT POINT
        // v[] CONTAINS THE ORTHOGONAL PROJECTION OF -residuals ONTO THE
        // SPACE SPANNED BY THE COLUMNS THE JACOBIAN AT CURRENT POINT
        // covarMat[][] double 2D array OF DIMENSION nPts*param
        // CONTAINING THE MATRIX R FROM THE
        // DECOMPOSITION    T
        // Q *J*D*E = (R)
        // (0)           (2)
        // IN THE UPPER TRIANGLE OF covarMat
        // WHERE
        // Q IS ORTHOGONAL (nPts*nPts)
        // J IS THE JACOBIAN (nPts*param) AT THE CURRENT POINT
        // D IS A DIAGONAL MATRIX (param*param)
        // E IS A PERMUTATION MATRIX (param*param)
        // R IS AN UPPER TRIANGULAR MATRIX (param*param)
        // nPts INTEGER SCALAR CONTAINING THE LENGTH OF THE ARRAYS residuals,v,w1
        // mdg INTEGER SCALAR CONTAINING LEADING DIMENSION OF ARRAY gmat.
        // mdg MUST BE >= param
        // paramAct INTEGER SCALAR CONTAINING NO. OF ACTIVE CONSTRAINTS

        // ON RETURN

        // THE COMPUTED SOLUTION dx OF SYSTEM (1)
        // w0[] COMPLETELY DESTROYED
        // v[] COMPLETELY DESTROYED
        // covarMat[][] COMPLETELY DESTROYED
        // errorStatus CONTAINS = -3 IF THE MATRIX gmat IN SYSTEM (1)
        // IS NOT POSITIVE DEFINITE
        // < -10 IF A USER STOP IS INDICATED
        // =  0 IF gmat IS POSITIVE DEFINITE
        // eval CONTAINS NO. OF FUNCTION EVALUATIONS DONE INSIDE THIS ROUTINE

        // WORKING AREAS

        // w1[] double 1D ARRAY OF DIMENSION nPts
        // w3[] double 1D ARRAY OF DIMENSION param
        // gmat[][] double 2D ARRAY OF DIMENSION mdg*param

        int i, j, k, l, nn;
        int[] idummy = new int[1];
        double[] dummy = new double[param];

        // INSTEAD OF SOLVING (1) WE SOLVE A TRANSFORMED SYSTEM
        // WHICH HAS param-paramAct UNKNOWNS DY

        // I.E. SOLVE  T    T      nPts-1                           T     T
        // (R *R+E *D*(SIGMA(residuals *G ))*D*E)*DY = -(R :0)*Q *residuals  (2)
        // K=0           K  K

        // dx = D*E*DY  (THE LAST paramAct ELEMENTS OF DY EQUAL TO ZERO)

        // FORM RIGHT HAND SIDE OF SYSTEM (2)
        // I.E.          T     T
        // FORM  -(R :0)*Q *residuals   AND STORE IN dx
        // T
        // FIRST FORM Q *residuals BY USING SQRSL AND STORE IN w1

        nn = param - paramAct;
        sqrsl(covarMat, nPts, nn, w0, residuals, dummy, w1, dummy, dummy, dummy, 1000);

        // T
        // FORM  -(R :0)*w1 AND STORE IN dx

        rtrw1(nn);
        // T
        // FORM  covarMat = R *R

        jtrj(nn);
        /*Preferences.debug("R(tr)*R\n");
         * for (i = 0; i < nn; i++) { for (j = 0; j < nn; j++) {     Preferences.debug(covarMat[i][j] + " "); }
         * Preferences.debug("\n");}*/

        // COMPUTE HESSIANS G   AND FORM FINAL MATRIX gmat OF SYSTEM (2)
        // K

        // THE 4 FIRST COLUMNS OF ARRAY covarMat ARE USED AS WORKING STORAGE
        // INSIDE THE ROUTINE HESS

        errorStatus = 0;
        hess();

        if (errorStatus < -10) {
            return;
        }

        eval = 2 * param * (param + 1);

        // T
        // FORM E *D*GMAT*D*E

        if (internalScaling) {

            for (i = 0; i < param; i++) {

                for (j = 0; j < param; j++) {
                    gmat[i][j] = diag[i] * gmat[i][j];
                    gmat[j][i] = gmat[j][i] * diag[i];
                }
            }

            if (outputMes) {
	            Preferences.debug("The nonlinear part.Only the upper triangular.\n");
	            Preferences.debug("Row - wize written.I.e.the first 4values make\n");
	            Preferences.debug("up the 1st row.The following 3 values make up\n");
	            Preferences.debug("the 2nd row starting at the diagonal elem.\n");
            }

        } // if (internalScaling)

        // DO THE PIVOTING OF COLUMNS AND ROWS IN MATRIX gmat
        // AND ADD TO MATRIX covarMat
        for (i = 0; i < nn; i++) {
            l = pivit[i];

            for (j = i; j < nn; j++) {
                k = pivit[j];
                covarMat[i][j] = covarMat[i][j] + gmat[l][k];
                // write(10,*) GMAT(L,K)
            } // for (j = i; j < nn; j++)
        } // for (i = 0; i < nn; i++)

        // PERFORM CHOLESKY DECOMPOSITION OF MATRIX covarMat

        schdc(nn, idummy, 0);
        errorStatus = 0;

        if (nn != info) {

            // MATRIX covarMat IS NOT POSITIVE DEFINITE
            errorStatus = -3;

            return;
        } // if (nn != info)

        // T    T
        // SOLVE SYSTEM   covarMat*DY = -(R 0)*Q *residuals

        sposl(nn);

        // FORM  dx = D*E*DY

        if (paramAct != 0) {

            for (i = 0; i < paramAct; i++) {
                j = nn + i;
                dx[j] = 0.0;
            }
        } // if (paramAct != 0)

        pivec(w0);

        if (!internalScaling) {
            return;
        }

        for (i = 0; i < param; i++) {
            dx[i] = diag[i] * dx[i];
        }

        return;
    }

    /**
     * DOCUMENT ME!
     *
     * @param  tau  DOCUMENT ME!
     */
    private void soliuc(double tau) {

        /**
         * @param tau double scalar containing a small positive value used           to determine the pseudo rank of the
         * Jacobian
         */
        // Compute the pseudo inverse solution (dx) of the system
        // J * dx = -residuals
        // where
        // J is the Jacobian of resiudals(a) at the current point
        // residuals is the vector of residuals
        // on entry

        // paramAct number of active constraints
        // Equals -number of active constraints when one is deleted in soliuc

        // pseudoRank scalar containing the pseudo rank of the matrix J
        // that was used to compute dx
        int i, j, jj, k, inds, kk, ii, lprank, mr, kmax;
        int[] ip = new int[1];
        double tol, d1max, d1new;
        double ymax = 0.0;
        double factor = 2.0;
        double[] scaqr = new double[1];
        double[] dummy = new double[1];
        double[] dykk = new double[1];
        double[] work = new double[param];
        double[][] work2 = new double[param][1];
        double[] w2Part;

        if (outputMes) {
	        Preferences.debug("Current point in soliuc\n");
	
	        for (i = 0; i < param; i++) {
	            Preferences.debug(a[i] + "\n");
	        }
        }

        // Compute Jacobian at the current point and store in covarMat
        imax = -1;
        errorStatus = 0;
        newunc();

        if (errorStatus != 0) {
            return;
        }

        // Compute the gradient of the objective at the current point
        for (i = 0; i < param; i++) {
            g[i] = 0.0;

            for (j = 0; j < nPts; j++) {
                g[i] = g[i] + (covarMat[j][i] * residuals[j]);
            }
        }

        // Scale the Jacobian if internalScaling is true
        // cnorm := length of the longest column in the Jacobian
        scaunc();
        gnorm = dnrm2(param, g, 1) / cnorm;
        if (outputMes) {
            Preferences.debug("cnorm = " + cnorm + "\n");
            Preferences.debug("gnorm = " + gnorm + "\n");
        }

        // MAKE A QR-DECOMPOSITION OF (POSSIBLY SCALED) JACOBIAN
        // I.E.                T
        // Q *J*D*E = (R)
        // (0)
        // ALSO DETERMINE PSEUDO RANK OF MATRIX J*D

        tol = Math.sqrt(param) * tau;
        pseudoRank = triunc(tol, paramAct);

        // COMPUTE THE "PSEUDO INVERSE" SOLUTION (DX) BY USING pseudoRank AS
        // PSEUDO RANK OF MATRIX J*D
        // ALSO COMPUTE  V= J*DX
        // AND       D1SQS= PREDICTED REDUCTION IN THE OBJECTIVE
        // BETA = THE NORM OF ORTH. PROJ. OF residuals ONTO COLUMN
        // SPACE OF THE JACOBIAN J
        pseudoRankMat[0] = pseudoRank;
        d1sqs = gndunc(internalScaling, pseudoRankMat, work);
        pseudoRank = pseudoRankMat[0];
        if (outputMes) {
	        Preferences.debug("The right hand side\n");
	
	        for (i = 0; i < param; i++) {
	            Preferences.debug(w1[i] + "\n");
	        }
        }

        beta = dnrm2(param - paramAct, w1, 1);
        info = rngkm1 - 1;

        if (alfkm1 == aupkm1) {
            info = rngkm1 - 2;
        }

        info = Math.max(1, info);
        prekm1 = beta;

        if (Math.abs(kodkm1) == 1) {
            prekm1 = dnrm2(info, w1, 1);
        }

        // Delete the constraint that causes the largest predicted
        // reduction in the objective
        if (outputMes) {
	        Preferences.debug("paramAct = " + paramAct + "\n");
	        Preferences.debug("pseudoRank = " + pseudoRank + "\n");
	        Preferences.debug("imax = " + imax + "\n");
        }

        if ((paramAct == 0) || (imax != -1)) {
            return;
        }

        // Initialize
        ii = 0;
        kk = param - paramAct;
        mr = param - kk;
        d1max = d1sqs;
        kmax = -1;

        for (i = 0; i < nPts; i++) {
            w1[i] = -residuals[i];
        }

        sqrsl(covarMat, nPts, param, w0, w1, dummy, w1, w2, dummy, dummy, 1000);

        // Try each bounded column to see what predicted reduction it gives

        inds = 0;

        for (i = 0; i < param; i++) {

            if (aset[i] == 0) {
                continue;
            }

            if (pseudoRank < (param - paramAct)) {
                k = 0;
                kk = 0;
                if (outputMes) {
                    Preferences.debug("Not full rank: pseudoRank = " + pseudoRank + "\n");
                }
                inds = aset[i];
                aset[i] = 0;
                newunc();

                if (errorStatus != 0) {
                    System.exit(errorStatus);
                }

                lprank = triunc(tol, paramAct - 1);
                if (outputMes) {
                    Preferences.debug("lprank = " + lprank + "\n");
                }
                pseudoRankMat[0] = lprank;
                d1new = gndunc(false, pseudoRankMat, work);
                lprank = pseudoRankMat[0];
                if (outputMes) {
	                Preferences.debug("New search direction\n");
	
	                for (jj = 0; jj < param; jj++) {
	                    Preferences.debug(dx[jj] + "\n");
	                }
                }

                aset[i] = inds;

                if ((dx[i] * inds * (bu[i] - bl[i])) <= 0.0) {
                    continue;
                }
            } // if (pseudoRank < (param - paramAct))
            else {
                k = param - paramAct + ii;
                ii++;

                // T
                // Put w2 equal to -Q * residuals
                for (jj = 0; jj < nPts; jj++) {
                    w2[jj] = w1[jj];
                }

                // Zero the kth column below its (param - paramAct)+1 element
                for (j = 0; j < param; j++) {
                    work[j] = covarMat[j][k];

                    if (j > k) {
                        work[j] = 0.0;
                    }
                } // for (j = 0; j < param; j++)

                if (k != kk) {
                    ip[0] = 1;

                    for (jj = 0; jj < mr; jj++) {
                        work2[jj][0] = work[kk + jj];
                    }

                    sqrdc(work2, mr, 1, scaqr, ip, 0);

                    // Transform w2 in the same way
                    w2Part = new double[w2.length - kk];

                    for (jj = 0; jj < (w2.length - kk); jj++) {
                        w2Part[jj] = w2[jj + kk];
                    }

                    sqrsl(work2, mr, 1, scaqr, w2Part, dummy, w2Part, dykk, dummy, dummy, 100);

                    for (jj = 0; jj < mr; jj++) {
                        work[kk + jj] = work2[jj][0];
                    }
                } // if (k != kk)

                // Determine the kkth element in dy
                if (outputMes) {
	                Preferences.debug("SOLIUC: work[kk] = " + work[kk] + "\n");
	                Preferences.debug("SOLIUC:tol = " + tol + "\n");
	                Preferences.debug("SOLIUC:covarMat[0][0] = " + covarMat[0][0] + "\n");
                }

                if (Math.abs(work[kk]) <= (tol * Math.abs(covarMat[0][0]))) {
                    continue;
                }

                dykk[0] = w2[kk] / work[kk];
                if (outputMes) {
	                Preferences.debug("In soliuc: dykk[0] = " + dykk[0] + "\n");
	                Preferences.debug("i = " + i + "\n");
	                Preferences.debug("aset[i] = " + aset[i] + "\n");
	                Preferences.debug("bu[i] = " + bu[i] + "\n");
	                Preferences.debug("bl[i] = " + bl[i] + "\n");
                }

                if ((dykk[0] * aset[i] * (bu[i] - bl[i])) <= 0.0) {
                    continue;
                }

                d1new = d1sqs + (w2[kk] * w2[kk]);
            } // else

            if (outputMes) {
                Preferences.debug("In soliuc: d1new = " + d1new + "\n");
                Preferences.debug("In soliuc: d1sqs = " + d1sqs + "\n");
            }

            if (d1new < (factor * d1sqs)) {
                continue;
            }

            if (outputMes) {
                Preferences.debug("Delete a fixed variable\n");
            }

            if (d1new < d1max) {
                continue;
            }

            d1max = d1new;
            ymax = w2[kk];
            kmax = k;
            imax = i;
        } // for (i = 0; i < param; i++)

        if (pseudoRank < (param - paramAct)) {

            if (imax != -1) {
                ival = aset[imax];
                aset[imax] = 0;
                paramAct--;
            } // if (imax != -1)

            newunc();
            pseudoRank = triunc(tol, paramAct);
            pseudoRankMat[0] = pseudoRank;
            d1sqs = gndunc(false, pseudoRankMat, work);
            pseudoRank = pseudoRankMat[0];
            beta = dnrm2(param - paramAct, w1, 1);
            prekm1 = beta;
        } // if (pseudoRank < (param - paramAct))
        else if (kmax != -1) {

            // FORM J*DX WHERE DX IS THE AUGMENTED GN-DIRECTION.
            // J = Q*H*(D1)
            // ( 0)
            // T
            // FIRST COMPUTE W2:= (D1)= -Q *residuaos
            // (D2)
            for (jj = 0; jj < nPts; jj++) {
                w2[jj] = w1[jj];
            }

            if (kmax != kk) {

                for (j = 0; j < param; j++) {
                    work[j] = covarMat[j][kmax];

                    if (j > kmax) {
                        work[j] = 0.0;
                    }
                } // for (j = 0; j < param; j++)

                for (jj = 0; jj < mr; jj++) {
                    work2[jj][0] = work[jj + kk];
                }

                sqrdc(work2, mr, 1, scaqr, ip, 0);
                w2[kk] = ymax;
                // w2 := H*(D1)
                // ( 0)
            } // if (kmax != kk)

            ip[0] = kk + 1;

            for (jj = ip[0]; jj < nPts; jj++) {
                w2[jj] = 0.0;
            }

            if (kmax != kk) {
                w2Part = new double[w2.length - kk];

                for (jj = 0; jj < (w2.length - kk); jj++) {
                    w2Part[jj] = w2[jj + kk];
                }

                sqrsl(work2, mr, 1, scaqr, w2Part, w2Part, dummy, dummy, dummy, dummy, 10000);
            } // if (kmax != kk)

            for (jj = 0; jj < mr; jj++) {
                work[jj + kk] = work2[jj][0];
            }

            // V := Q*H*(D1)
            // ( 0)
            sqrsl(covarMat, nPts, param, w0, w2, v, dummy, dummy, dummy, dummy, 10000);

            // Move column kmax to column kk
            if (kmax != kk) {

                for (jj = 0; jj <= kk; jj++) {
                    covarMat[jj][kk] = work[jj];
                }
            } // if (kmax != kk)

            // Change in permutation vector
            ip[0] = pivit[kk];
            pivit[kk] = pivit[kmax];
            pivit[kmax] = ip[0];

            // Update active set variables

            ival = aset[imax];
            aset[imax] = 0;
            pseudoRank++;
            paramAct--;
            beta = dnrm2(param - paramAct, w1, 1);
            prekm1 = beta;
            d1sqs = d1max;

            // Compute the augmented GN-direction

            for (jj = 0; jj <= kk; jj++) {
                dx[jj] = w1[jj];
            }

            dx[kk] = ymax;
            strsl(dx);

            // back transform
            btrunc(internalScaling, w2);
            if (outputMes) {
	            Preferences.debug("Full rank new search direction\n");
	
	            for (i = 0; i < param; i++) {
	                Preferences.debug(dx[i] + "\n");
	            }
            }
        } // else if (kmax != 0)

        return;
    }

    /**
     * DOCUMENT ME!
     *
     * @param  nn  DOCUMENT ME!
     */
    private void sposl(int nn) {
        // SPOSL SOLVES THE REAL SYMMETRIC POSITIVE DEFINITE SYSTEM
        // covarMat * X = dx

        // ON ENTRY

        // covarMat double[nn][nn]

        // dx double[nn]
        // THE RIGHT HAND SIDE VECTOR.

        // ON RETURN

        // dx  THE SOLUTION VECTOR  X .

        // ERROR CONDITION

        // A DIVISION BY ZERO WILL OCCUR IF THE INPUT FACTOR CONTAINS
        // A ZERO ON THE DIAGONAL.  TECHNICALLY THIS INDICATES
        // SINGULARITY BUT IT IS USUALLY CAUSED BY IMPROPER SUBROUTINE
        // ARGUMENTS.  IT WILL NOT OCCUR IF THE SUBROUTINES ARE CALLED
        // CORRECTLY AND  INFO .EQ. 0 .

        double t;
        int j, k, kb;

        // SOLVE TRANS(R)*Y = dx

        for (k = 0; k < nn; k++) {
            t = 0.0;

            for (j = 0; j <= (k - 1); j++) {
                t = t + covarMat[j][k] + dx[j];
            }

            dx[k] = (dx[k] - t) / covarMat[k][k];
        } // for (k = 0; k < nn; k++)

        // SOLVE R*X = Y

        for (kb = 1; kb <= nn; kb++) {
            k = nn - kb;
            dx[k] = dx[k] / covarMat[k][k];
            t = -dx[k];

            for (j = 0; j <= (k - 1); j++) {
                dx[j] = dx[j] + (t * covarMat[j][k]);
            }
        } // for (kb = 1; kb <= nn; kb++)

        return;
    }

    /**
     * DOCUMENT ME!
     *
     * @param  x      DOCUMENT ME!
     * @param  n      DOCUMENT ME!
     * @param  p      DOCUMENT ME!
     * @param  qraux  DOCUMENT ME!
     * @param  jpvt   DOCUMENT ME!
     * @param  job    DOCUMENT ME!
     */
    private void sqrdc(double[][] x, int n, int p, double[] qraux, int[] jpvt, int job) {
        // SQRDC USES HOUSEHOLDER TRANSFORMATIONS TO COMPUTE THE QR FACTORIZATION OF AN N BY P MATRIX X.  COLUMN
        // PIVOTING BASED ON THE 2-NORMS OF THE REDUCED COLUMNS MAY BE PERFORMED AT THE USERS OPTION.

        // ON ENTRY

        // X       DOUBLE(N,P)
        // X CONTAINS THE MATRIX WHOSE DECOMPOSITION IS TO BE COMPUTED.

        // N       INTEGER.
        // N IS THE NUMBER OF ROWS OF THE MATRIX X.

        // P       INTEGER.
        // P IS THE NUMBER OF COLUMNS OF THE MATRIX X.

        // JPVT    INTEGER(P).
        // JPVT CONTAINS INTEGERS THAT CONTROL THE SELECTION OF THE
        // PIVOT COLUMNS.  THE K-TH COLUMN X(K) OF X IS PLACED IN ONE OF
        // THREE CLASSES ACCORDING TO THE VALUE OF JPVT(K).

        // IF JPVT(K) > 0, THEN X(K) IS AN INITIAL COLUMN.

        // IF JPVT(K) .EQ. 0, THEN X(K) IS A FREE COLUMN.

        // IF JPVT(K) .LT. 0, THEN X(K) IS A FINAL COLUMN.

        // BEFORE THE DECOMPOSITION IS COMPUTED, INITIAL COLUMNS
        // ARE MOVED TO THE BEGINNING OF THE ARRAY X AND FINAL
        // COLUMNS TO THE END.  BOTH INITIAL AND FINAL COLUMNS
        // ARE FROZEN IN PLACE DURING THE COMPUTATION AND ONLY
        // FREE COLUMNS ARE MOVED.  AT THE K-TH STAGE OF THE
        // REDUCTION, IF X(K) IS OCCUPIED BY A FREE COLUMN
        // IT IS INTERCHANGED WITH THE FREE COLUMN OF LARGEST
        // REDUCED NORM.  JPVT IS NOT REFERENCED IF
        // JOB .EQ. 0.

        // JOB     INTEGER.
        // JOB IS AN INTEGER THAT INITIATES COLUMN PIVOTING.
        // IF JOB .EQ. 0, NO PIVOTING IS DONE.
        // IF JOB .NE. 0, PIVOTING IS DONE.

        // ON RETURN

        // X       X CONTAINS IN ITS UPPER TRIANGLE THE UPPER TRIANGULAR MATRIX R
        // OF THE QR FACTORIZATION.   BELOW ITS DIAGONAL X CONTAINS
        // INFORMATION FROM WHICH THE ORTHOGONAL PART OF THE
        // DECOMPOSITION CAN BE RECOVERED.
        // NOTE THAT IF PIVOTING HAS BEEN REQUESTED, THE DECOMPOSITION IS
        // NOT THAT OF THE ORIGINAL MATRIX X BUT THAT OF X
        // WITH ITS COLUMNS PERMUTED AS DESCRIBED BY JPVT.

        // QRAUX   DOUBLE(P).
        // QRAUX CONTAINS FURTHER INFORMATION REQUIRED TO RECOVER
        // THE ORTHOGONAL PART OF THE DECOMPOSITION.

        // JPVT    JPVT(K) CONTAINS THE INDEX OF THE COLUMN OF THE
        // ORIGINAL MATRIX THAT HAS BEEN INTERCHANGED INTO
        // THE K-TH COLUMN, IF PIVOTING WAS REQUESTED.

        int i, j, jj, jp, l, lp1, lup, maxj, pl, pu;
        double maxnrm, tt;
        double nrmxl, t;
        double[] work = new double[p];
        double temp;
        boolean negj, swapj;
        boolean[] negjMat = new boolean[p];
        double[] tempMat = new double[n];

        for (j = 0; j < p; j++) {
            negjMat[j] = false;
        }

        pl = 0;
        pu = -1;

        if (job != 0) {

            // PIVOTING HAS BEEN REQUESTED.REARRANGE THE COLUMNS ACCORDING TO JPVT.

            for (j = 0; j < p; j++) {
                swapj = jpvt[j] > 0;
                negj = jpvt[j] < 0;
                jpvt[j] = j;

                if (negj) {
                    negjMat[j] = true;
                }

                if (!swapj) {
                    continue;
                }

                if (j != pl) {

                    for (jj = 0; jj < n; jj++) {
                        temp = x[jj][pl];
                        x[jj][pl] = x[jj][j];
                        x[jj][j] = temp;
                    }
                }

                jpvt[j] = jpvt[pl];
                negjMat[j] = negjMat[pl];
                jpvt[pl] = j;
                negjMat[pl] = false;
                pl++;
            } // for (j = 0; j < p; j++)

            pu = p - 1;

            for (jj = 1; jj <= p; jj++) {
                j = p - jj;

                if (!negjMat[j]) {
                    continue;
                }

                negjMat[j] = false;

                if (j != pu) {

                    for (i = 0; i < n; i++) {
                        temp = x[i][pu];
                        x[i][pu] = x[i][j];
                        x[i][j] = temp;
                    }

                    jp = jpvt[pu];
                    negj = negjMat[pu];
                    jpvt[pu] = jpvt[j];
                    negjMat[pu] = negjMat[j];
                    jpvt[j] = jp;
                    negjMat[j] = negj;
                } // if (j != pu)

                pu--;
            } // for (jj = 1; jj <= p; jj++)

            // COMPUTE THE NORMS OF THE FREE COLUMNS.

            for (j = pl; j <= pu; j++) {

                for (i = 0; i < n; i++) {
                    tempMat[i] = x[i][j];
                }

                qraux[j] = dnrm2(n, tempMat, 1);
                work[j] = qraux[j];
            } // for (j = pl; j <= pu; j++)
        } // if (job != 0)

        // PERFORM THE HOUSEHOLDER REDUCTION OF X.

        lup = Math.min(n, p);

        for (l = 0; l < lup; l++) {

            if ((l >= pl) && (l < pu)) {

                // LOCATE THE COLUMN OF LARGEST NORM AND BRING IT INTO THE PIVOT POSITION.

                maxnrm = 0.0;
                maxj = l;

                for (j = l; j <= pu; j++) {

                    if (qraux[j] <= maxnrm) {
                        continue;
                    }

                    maxnrm = qraux[j];
                    maxj = j;
                } // for (j = l; j <= pu; j++)

                if (maxj != l) {

                    for (i = 0; i < n; i++) {
                        temp = x[i][l];
                        x[i][l] = x[i][maxj];
                        x[i][maxj] = temp;
                    }

                    qraux[maxj] = qraux[l];
                    work[maxj] = work[l];
                    jp = jpvt[maxj];
                    jpvt[maxj] = jpvt[l];
                    jpvt[l] = jp;
                } // if (maxj != l)
            } // if ((l >= pl) && (l < pu))

            qraux[l] = 0.0;

            if (l == (n - 1)) {
                continue;
            }

            // COMPUTE THE HOUSEHOLDER TRANSFORMATION FOR COLUMN L.
            for (i = 0; i < (n - l); i++) {
                tempMat[i] = x[l + i][l];
            }

            nrmxl = dnrm2(n - l, tempMat, 1);

            if (nrmxl == 0.0) {
                continue;
            }

            if (x[l][l] != 0.0) {

                if (x[l][l] >= 0.0) {
                    nrmxl = Math.abs(nrmxl);
                } else {
                    nrmxl = -Math.abs(nrmxl);
                }
            }

            for (i = l; i < n; i++) {
                x[i][l] = x[i][l] / nrmxl;
            }

            x[l][l] = x[l][l] + 1.0;

            // APPLY THE TRANSFORMATION TO THE REMAINING COLUMNS,
            // UPDATING THE NORMS.

            lp1 = l + 1;

            for (j = lp1; j < p; j++) {
                t = 0.0;

                for (i = l; i < n; i++) {
                    t = t - (x[i][l] * x[i][j] / x[l][l]);
                }

                for (i = l; i < n; i++) {
                    x[i][j] = x[i][j] + (t * x[i][l]);
                }

                if ((j < pl) || (j > pu)) {
                    continue;
                }

                if (qraux[j] == 0.0) {
                    continue;
                }

                temp = Math.abs(x[l][j]) / qraux[j];
                tt = 1.0 - (temp * temp);
                tt = Math.max(tt, 0.0);
                t = tt;
                temp = qraux[j] / work[j];
                tt = 1.0 + (0.05 * tt * temp * temp);

                if (tt != 1.0) {
                    qraux[j] = qraux[j] * Math.sqrt(t);
                } else {

                    for (i = 0; i < (n - l - 1); i++) {
                        tempMat[i] = x[l + 1 + i][j];
                    }

                    qraux[j] = dnrm2(n - l - 1, tempMat, 1);
                    work[j] = qraux[j];
                }
            } // for (j = lp1; j < p; j++)

            // SAVE THE TRANSFORMATION.

            qraux[l] = x[l][l];
            x[l][l] = -nrmxl;
        } // for (l = 0; l < lup; l++)

        return;
    }

    /**
     * DOCUMENT ME!
     *
     * @param  x      DOCUMENT ME!
     * @param  n      DOCUMENT ME!
     * @param  k      DOCUMENT ME!
     * @param  qraux  DOCUMENT ME!
     * @param  y      DOCUMENT ME!
     * @param  qy     DOCUMENT ME!
     * @param  qty    DOCUMENT ME!
     * @param  b      DOCUMENT ME!
     * @param  rsd    DOCUMENT ME!
     * @param  xb     DOCUMENT ME!
     * @param  job    DOCUMENT ME!
     */
    private void sqrsl(double[][] x, int n, int k, double[] qraux, double[] y, double[] qy, double[] qty, double[] b,
                       double[] rsd, double[] xb, int job) {
        // SQRSL APPLIES THE OUTPUT OF SQRDC TO COMPUTE COORDINATE TRANSFORMATIONS, PROJECTIONS, AND LEAST SQUARES
        // SOLUTIONS. FOR K <= MIN(N,P), LET XK BE THE MATRIX

        // XK = (X(JPVT(0)),X(JPVT(1)), ... ,X(JPVT(K)))

        // FORMED FROM COLUMNNS JPVT(0), ... ,JPVT(K) OF THE ORIGINAL
        // N X P MATRIX X THAT WAS INPUT TO SQRDC (IF NO PIVOTING WAS
        // DONE, XK CONSISTS OF THE FIRST K COLUMNS OF X IN THEIR
        // ORIGINAL ORDER).  SQRDC PRODUCES A FACTORED ORTHOGONAL MATRIX Q
        // AND AN UPPER TRIANGULAR MATRIX R SUCH THAT

        // XK = Q * (R)
        // (0)

        // THIS INFORMATION IS CONTAINED IN CODED FORM IN THE ARRAYS X AND QRAUX.

        // ON ENTRY

        // X      DOUBLE(N,P).
        // X CONTAINS THE OUTPUT OF SQRDC.

        // N      INTEGER.
        // N IS THE NUMBER OF ROWS OF THE MATRIX XK.  IT MUST
        // HAVE THE SAME VALUE AS N IN SQRDC.

        // K      INTEGER.
        // K IS THE NUMBER OF COLUMNS OF THE MATRIX XK.  K
        // MUST NOT BE GREATER THAN MIN(N,P), WHERE P IS THE
        // SAME AS IN THE CALLING SEQUENCE TO SQRDC.

        // QRAUX  DOUBLE(P).
        // QRAUX CONTAINS THE AUXILIARY OUTPUT FROM SQRDC.

        // Y      DOUBLE(N)
        // Y CONTAINS AN N-VECTOR THAT IS TO BE MANIPULATED
        // BY SQRSL.

        // JOB    INTEGER.
        // JOB SPECIFIES WHAT IS TO BE COMPUTED.  JOB HAS
        // THE DECIMAL EXPANSION ABCDE, WITH THE FOLLOWING MEANING.

        // IF A.NE.0, COMPUTE QY.
        // IF B,C,D, OR E .NE. 0, COMPUTE QTY.
        // IF C.NE.0, COMPUTE B.
        // IF D.NE.0, COMPUTE RSD.
        // IF E.NE.0, COMPUTE XB.

        // NOTE THAT A REQUEST TO COMPUTE B, RSD, OR XB
        // AUTOMATICALLY TRIGGERS THE COMPUTATION OF QTY, FOR
        // WHICH AN ARRAY MUST BE PROVIDED IN THE CALLING SEQUENCE.

        // ON RETURN

        // QY     DOUBLE(N).
        // QY CONTAINS Q*Y, IF ITS COMPUTATION HAS BEEN REQUESTED.

        // QTY    DOUBLE(N).
        // QTY CONTAINS TRANS(Q)*Y, IF ITS COMPUTATION HAS BEEN REQUESTED.
        // HERE TRANS(Q) IS THE TRANSPOSE OF THE MATRIX Q.

        // B      DOUBLE(K)
        // B CONTAINS THE SOLUTION OF THE LEAST SQUARES PROBLEM

        // MINIMIZE NORM2(Y - XK*B),

        // IF ITS COMPUTATION HAS BEEN REQUESTED.  (NOTE THAT
        // IF PIVOTING WAS REQUESTED IN SQRDC, THE J-TH
        // COMPONENT OF B WILL BE ASSOCIATED WITH COLUMN JPVT(J)
        // OF THE ORIGINAL MATRIX X THAT WAS INPUT INTO SQRDC.)

        // RSD    DOUBLE(N).
        // RSD CONTAINS THE LEAST SQUARES RESIDUAL Y - XK*B,
        // IF ITS COMPUTATION HAS BEEN REQUESTED.  RSD IS
        // ALSO THE ORTHOGONAL PROJECTION OF Y ONTO THE
        // ORTHOGONAL COMPLEMENT OF THE COLUMN SPACE OF XK.

        // XB     DOUBLE(N).
        // XB CONTAINS THE LEAST SQUARES APPROXIMATION XK*B,
        // IF ITS COMPUTATION HAS BEEN REQUESTED.  XB IS ALSO
        // THE ORTHOGONAL PROJECTION OF Y ONTO THE COLUMN SPACE OF X.

        // INFO   INTEGER.
        // INFO IS -1 UNLESS THE COMPUTATION OF B HAS BEEN REQUESTED AND
        // R IS EXACTLY SINGULAR.  IN THIS CASE, INFO IS THE INDEX OF THE
        // THE FIRST ZERO DIAGONAL ELEMENT OF R AND B IS LEFT UNALTERED.

        // THE PARAMETERS QY, QTY, B, RSD, AND XB ARE NOT REFERENCED IF THEIR
        // COMPUTATION IS NOT REQUESTED AND IN THIS CASE CAN BE REPLACED BY DUMMY
        // VARIABLES IN THE CALLING PROGRAM.
        // TO SAVE STORAGE, THE USER MAY IN SOME CASES USE THE SAME ARRAY FOR
        // DIFFERENT PARAMETERS IN THE CALLING SEQUENCE.  A FREQUENTLY OCCURRING
        // EXAMPLE IS WHEN ONE WISHES TO COMPUTE ANY OF B, RSD, OR XB AND DOES NOT
        // NEED Y OR QTY.  IN THIS CASE ONE MAY IDENTIFY Y, QTY, AND ONE OF B, RSD,
        // OR XB, WHILE PROVIDING SEPARATE ARRAYS FOR ANYTHING ELSE THAT IS TO BE
        // COMPUTED.  THUS THE CALLING SEQUENCE

        // CALL SQRSL(X,LDX,N,K,QRAUX,Y,DUM,Y,B,Y,DUM,110,INFO)

        // WILL RESULT IN THE COMPUTATION OF B AND RSD, WITH RSD OVERWRITING Y.
        // MORE GENERALLY, EACH ITEM IN THE FOLLOWING LIST CONTAINS GROUPS OF
        // PERMISSIBLE IDENTIFICATIONS FOR A SINGLE CALLINNG SEQUENCE.

        // 1. (Y,QTY,B) (RSD) (XB) (QY)

        // 2. (Y,QTY,RSD) (B) (XB) (QY)

        // 3. (Y,QTY,XB) (B) (RSD) (QY)

        // 4. (Y,QY) (QTY,B) (RSD) (XB)

        // 5. (Y,QY) (QTY,RSD) (B) (XB)

        // 6. (Y,QY) (QTY,XB) (B) (RSD)

        // IN ANY GROUP THE VALUE RETURNED IN THE ARRAY ALLOCATED TO
        // THE GROUP CORRESPONDS TO THE LAST MEMBER OF THE GROUP.

        int i, j, jj, ju, kp1;
        double t, temp;
        boolean cb, cqy, cqty, cr, cxb;

        // SET INFO FLAG.

        info = -1;

        // DETERMINE WHAT IS TO BE COMPUTED.

        cqy = (job / 10000) != 0;
        cqty = (job % 10000) != 0;
        cb = ((job % 1000) / 100) != 0;
        cr = ((job % 100) / 10) != 0;
        cxb = (job % 10) != 0;
        ju = Math.min(k, n - 1);

        // SPECIAL ACTION WHEN N=1.

        if (ju == 0) {

            if (cqy) {
                qy[0] = y[0];
            }

            if (cqty) {
                qty[0] = y[0];
            }

            if (cxb) {
                xb[0] = y[0];
            }

            if (cb) {

                if (x[0][0] == 0.0) {
                    info = 0;
                } else {
                    b[0] = y[0] / x[0][0];
                }
            } // if (cb)

            if (cr) {
                rsd[0] = 0.0;
            }

            return;
        } // if (ju == 0)

        // SET UP TO COMPUTE QY OR QTY.

        if (cqy) {

            for (i = 0; i < n; i++) {
                qy[i] = y[i];
            }
        }

        if (cqty) {

            for (i = 0; i < n; i++) {
                qty[i] = y[i];
            }
        }

        if (cqy) {

            // COMPUTE QY.
            for (jj = 1; jj <= ju; jj++) {
                j = ju - jj;

                if (qraux[j] == 0.0) {
                    continue;
                }

                temp = x[j][j];
                x[j][j] = qraux[j];
                t = 0.0;

                for (i = j; i < n; i++) {
                    t = t - (x[i][j] * qy[i] / x[j][j]);
                }

                for (i = j; i < n; i++) {
                    qy[i] = qy[i] + (t * x[i][j]);
                }

                x[j][j] = temp;
            } // for (jj = 1; jj <= ju; jj++)
        } // if (cqy)

        if (cqty) {

            // COMPUTE TRANS(Q)*Y.

            for (j = 0; j < ju; j++) {

                if (qraux[j] == 0.0) {
                    continue;
                }

                temp = x[j][j];
                x[j][j] = qraux[j];
                t = 0.0;

                for (i = j; i < n; i++) {
                    t = t - (x[i][j] * qty[i] / x[j][j]);
                }

                for (i = j; i < n; i++) {
                    qty[i] = qty[i] + (t * x[i][j]);
                }

                x[j][j] = temp;
            } // for (j = 0; j < ju; j++)
        } // if (cqty)

        // SET UP TO COMPUTE B, RSD, OR XB.

        if (cb) {

            for (i = 0; i < k; i++) {
                b[i] = qty[i];
            }
        } // if (cb)

        kp1 = k + 1;

        if (cxb) {

            for (i = 0; i < k; i++) {
                xb[i] = qty[i];
            }
        } // if (cxb)

        if (cr && (k < n)) {

            for (i = kp1 - 1; i < n; i++) {
                rsd[i] = qty[i];
            }
        } // if (cr && (k < n))

        if (cxb && (kp1 <= n)) {

            for (i = kp1 - 1; i < n; i++) {
                xb[i] = 0.0;
            }
        } // if (cxb && (kp1 <= n))

        if (cr) {

            for (i = 0; i < k; i++) {
                rsd[i] = 0.0;
            }
        } // if (cr)

        if (cb) {

            // COMPUTE B.

            for (jj = 1; jj <= k; jj++) {
                j = k - jj;

                if (x[j][j] == 0.0) {
                    info = j;

                    break;
                }

                b[j] = b[j] / x[j][j];

                if (j == 0) {
                    continue;
                }

                t = -b[j];

                for (i = 0; i < j; i++) {
                    b[i] = b[i] + (t * x[i][j]);
                }
            } // for (jj = 1; jj <= k; jj++)
        } // if (cb)

        if ((!cr) && (!cxb)) {
            return;
        }

        // COMPUTE RSD OR XB AS REQUIRED.

        for (jj = 1; jj <= ju; jj++) {
            j = ju - jj;

            if (qraux[j] == 0.0) {
                continue;
            }

            temp = x[j][j];
            x[j][j] = qraux[j];

            if (cr) {
                t = 0.0;

                for (i = j; i < n; i++) {
                    t = t - (x[i][j] * rsd[i] / x[j][j]);
                }

                for (i = j; i < n; i++) {
                    rsd[i] = rsd[i] + (t * x[i][j]);
                }
            } // if (cr)

            if (cxb) {
                t = 0.0;

                for (i = j; i < n; i++) {
                    t = t - (x[i][j] * xb[i] / x[j][j]);
                }

                for (i = j; i < n; i++) {
                    xb[i] = xb[i] + (t * x[i][j]);
                }
            } // if (cxb)

            x[j][j] = temp;
        } // for (jj = 1; jj <= ju; jj++)

        return;
    }

    /**
     * DOCUMENT ME!
     */
    private void stepuc() {
        // COMPUTE THE STEPLENGTH ALPHA IN THE ITERATION
        // aNew := aOld + alpha*dx           (1)

        // ON ENTRY
        // PARAMETERS FLAGGED WITH (*) ARE ONLY DEFINED IF ABS(KOD)=1

        // a[] double 1D ARRAY OF DIMENSION  param CONTAINING
        // THE CURRENT POINT
        // *  g[] double 1D ARRAY OF DIMENSION param CONTAINING
        // GRADIENT OF THE OBJECTIVE FUNCTION AT CURRENT POINT
        // dx[] double 1D ARRAY OF DIMENSION param CONTAINING
        // THE SEARCH DIRECTION
        // param INTEGER SCALAR CONTAINING THE LENGTH OF THE ARRAYS a, g, dx
        // residuals double 1D ARRAY OF DIMENSION nPts CONTAINING
        // THE VECTOR OF RESIDUALS AT CURRENT POINT
        // *  V[] double 1D ARRAY OF DIMENSION nPts CONTAINING
        // THE DERIVATIVE OF residuals(aOld+alpha*dx) WITH RESPECT TO alpha
        // AT alpha = 0
        // nPts  INTEGER SCALAR CONTAINING THE LENGTH OF THE ARRAYS residuals,v
        // phi double SCALAR CONTAINING THE VALUE OF THE OBJECTIVE
        // FUNCTION AT THE CURRENT POINT
        // kod  INTEGER SCALAR CONTAINING A CODE THAT SAYS HOW THE
        // SEARCH DIRECTION dx HAS BEEN COMPUTED
        // = 1 IF GAUSS-NEWTON DIRECTION
        // = -1 IF SUBSPACE DIRECTION
        // = -2 IF NEWTON DIRECTION
        // pseudoRank INTEGER SCALAR CONTAINING PSEUDO RANK OF MATRIX covarMat
        // bl[] double 1D ARRAY OF DIMENSION param CONTAINING
        // THE LOWER BOUNDS OF THE UNKNOWNS
        // bu[] double 1D ARRAY OF DIMENSION param CONTAINING
        // THE UPPER BOUNDS OF THE UNKNOWNS
        // paramAct INTEGER SCALAR CONTAINING THE NO. OF ACTIVE CONSTRAINTS
        // aset[] INTEGER 1D ARRAY OF DIMENSION param
        // CONTAINING A CODE WHICH INDICATES WHETHER AN UNKNOWN IS
        // ACTIVE OR NOT.
        // aset[i] = 0 WHEN a[i] IS FREE
        // = +1 WHEN a[i] EQUALS bl[i]
        // = -1 WHEN a[i] EQUALS bu[i]

        // ON RETURN

        // a[]  THE NEW POINT aNew IN  (1)
        // residuals[] THE VALUE OF THE RESIDUALS AT THE NEW POINT
        // phi  THE VALUE OF THE OBJECTIVE AT THE NEW POINT
        // aset[] MODIFIED IN ONE ELEMENT IF A BOUND IS CROSSED
        // paramAct  THE CURRENT NO. OF ACTIVE CONSTRAINTS
        // eval INTEGER SCALAR CONTAINING NO. OF FUNCTION EVALUATIONS
        // DONE INSIDE THE LINESEARCH ROUTINE
        // alpha double SCALAR CONTAINING THE STEPSIZE
        // alphup double SCALAR CONTAINING THE UPPER BOUND OF STEPZISE
        // aDiff double SCALAR CONTAINING THE EUCLIDEAN DISTANCE BETWEEN
        // THE TWO POINTS aOld AND aNew  FROM  (1)
        // errorStatus INTEGER SCALAR CONTAINING
        // = -1 IF dx IS NOT A DESCENT DIRECTION
        // = -2 IF aDiff < SQRT(srelpr) OR IF alpha <= A LOWER BOUND
        // < -10 AS A USER STOP INDICATOR
        // = 0 OTHERWISE

        // WORKING AREAS

        // gmod[] double 1D ARRAY OF DIMENSION param

        double[] gmod = new double[param];

        int i;
        double alplow, ael, magfy, dphize;
        double[][] dummy = new double[1][1];
        double temp;

        // IF ABS(kod)=2
        // THEN TAKE AN UNDAMPED STEP
        // ELSE USE lineuc TO COMPUTE STEPSIZE alpha

        // DETERMINE LOWER AND UPPER BOUND OF STEP LENGTH AND
        // SUGGEST alpha AS STEPSIZE

        eval = 0;
        alphup = 3.0;

        if ((ifree > 0) && (indic < 0)) {
            alphup = 10.0 * alphup;
        }

        if (bounds != 0) {
        	if (outputMes) {
	            Preferences.debug("aset\n");
	
	            for (i = 0; i < param; i++) {
	                Preferences.debug(aset[i] + "\n");
	            }
	
	            Preferences.debug("dx\n");
	
	            for (i = 0; i < param; i++) {
	                Preferences.debug(dx[i] + "\n");
	            }
        	}

            for (i = 0; i < param; i++) {

                if (aset[i] != 0) {
                    continue;
                }

                if (dx[i] > 0.0) {

                    if ((alphup * dx[i]) < (bu[i] - a[i])) {
                        continue;
                    }

                    alphup = (bu[i] - a[i]) / dx[i];

                    continue;
                } // if (dx[i] > 0.0)

                if ((-alphup * dx[i]) <= (a[i] - bl[i])) {
                    continue;
                }

                alphup = -(a[i] - bl[i]) / dx[i];
            } // for (i = 0; i < param; i++)
        } // if (bounds != 0)

        alplow = alphup / 3000.0;

        if (Math.abs(kod) != 2) {
            magfy = 3.0;

            if (pseudoRank < rngkm1) {
                magfy = 6.0;
            }

            alpha = Math.min(1.0, Math.min(magfy * alfkm1, alphup));
            if (outputMes) {
	            Preferences.debug("In stepuc: alpha = " + alpha + "\n");
	            Preferences.debug("In stepuc: alfkm1 = " + alfkm1 + "\n");
	            Preferences.debug("In stepuc: alphup = " + alphup + "\n");
            }

            if ((ifree > 0) && (indic < 0)) {
                alpha = Math.min(10.0, alphup);
            }

            alpha = Math.max(alpha, alplow) * 2.0;

            // COMPUTE THE DERIVATIVE OF phi(a + alpha * dx) AT alpha = 0
            dphize = 0.0;

            for (i = 0; i < param; i++) {
                dphize = dphize + (g[i] * dx[i]);
            }

            do {
                alpha = alpha * 0.5;

                // COMPUTE A STEP LENGTH

                lineuc(gmod, dphize, alplow);

                if (errorStatus < -10) {
                    return;
                }

                eval = eval + iev;
            } while (errorStatus == -3);

            phi = phikp1;

            return;
        } // if (Math.abs(kod) != 2)

        // TAKE AN UNDAMPED STEP

        alpha = Math.min(1.0, alphup);
        errorStatus = 0;
        aDiff = 0.0;

        for (i = 0; i < param; i++) {
            ael = a[i];
            a[i] = ael + (alpha * dx[i]);
            aDiff = aDiff + ((a[i] - ael) * (a[i] - ael));
        } // for (i = 0; i < param; i++)

        aDiff = Math.sqrt(aDiff);

        // COMPUTE THE VECTOR OF RESIDUALS AT THE NEW POINT

        ctrl = 1;
        ctrlMat[0] = ctrl;
        fitToFunction(a, residuals, dummy);
        ctrl = ctrlMat[0];
        eval = 1;

        // INDICATE A LARGER VALUE OF THE OBJECTIVE IF THE RESIDUALS ARE
        // UNCOMPUTABLE  (I.E. CTRL=-1 )

        if (ctrl == -1) {
            phi = 2.0 * phi;
        }

        if (ctrl == 1) {
            temp = dnrm2(nPts, residuals, 1);
            phi = 0.5 * temp * temp;
        }

        if (ctrl < -10) {
            errorStatus = ctrl;
        }

        return;
    }

    /**
     * DOCUMENT ME!
     *
     * @param  b  DOCUMENT ME!
     */
    private void strsl(double[] b) {
        // STRSL SOLVES SYSTEMS OF THE FORM

        // covarMat * X = B

        // WHERE covarMat IS AN UPPER TRIANGULAR MATRIX OF ORDER pseudoRank

        // ON RETURN

        // B         B CONTAINS THE SOLUTION, IF INFO .EQ. -1.
        // OTHERWISE B IS UNALTERED.

        // INFO      INTEGER
        // INFO CONTAINS -1 IF THE SYSTEM IS NONSINGULAR.
        // OTHERWISE INFO CONTAINS THE INDEX OF
        // THE FIRST ZERO DIAGONAL ELEMENT OF T.

        double temp;
        int j, jj, k;

        for (info = 0; info < pseudoRank; info++) {

            if (covarMat[info][info] == 0.0) {
                return;
            }
        }

        info = -1;


        // SOLVE covarMat*X=B FOR covarMat UPPER TRIANGULAR.

        b[pseudoRank - 1] = b[pseudoRank - 1] / covarMat[pseudoRank - 1][pseudoRank - 1];

        for (jj = 2; jj <= pseudoRank; jj++) {
            j = pseudoRank - jj;
            temp = -b[j + 1];

            for (k = 0; k <= j; k++) {
                b[k] = b[k] + (temp * covarMat[k][j + 1]);
            }

            b[j] = b[j] / covarMat[j][j];
        }

        return;
    }

    /**
     * DOCUMENT ME!
     */
    private void subuc() {
        // COMPUTE A SEARCH DIRECTION dx BY MINIMIZING IN A SUBSPACE
        // I.E.  SOLVE THE UPPER TRIANGULAR SYSTEM            T
        // R*dx = -Q *residuals

        // BY ONLY USING THE rank (WILL BE DETERMINED ) FIRST COLUMNS IN MATRIX R
        // AND SETTING THE REST OF THE ELEMENTS IN dx TO ZERO

        // ON ENTRY

        // restart = FALSE IF CURRENT STEP IS NOT A RESTART STEP
        // = TRUE IF IT IS
        // kod  CONTAINS A CODE THAT SAYS HOW THE PREVIOUS SEARCH DIRECTION WAS
        // COMPUTED
        // = 1 IF GN-DIRECTION
        // = -1 IF SUBSPACE DIRECTION
        // = -2 IF NEWTON DIRECTION
        // fsum CONTAINS THE SUM OF SQUARES AT CURRENT POINT
        // d1sqs CONTAINS PREDICTED REDUCTION IN THE OBJECTIVE FUNCTION
        // IF GN-DIRECTION IS USED AS SEARCH DIRECTION
        // dxnorm CONTAINS THE NORM OF THE GN-DIRECTION CONTAINED IN dx
        // residuals CONTAINS THE VECTOR OF RESIDUALS AT CURRENT POINT
        // nPts CONTAINS LENGTH OF THE ARRAYS residuals, v, and w1
        // covarMat[][] DOUBLE 2D ARRAY OF DIMENSION nPts*param
        // CONTAINING THE MATRIX R FROM THE
        // DECOMPOSITION    T
        // Q *J*D*E = (R)
        // (0)
        // IN THE UPPER TRIANGLE OF covarMat
        // WHERE
        // Q IS ORTHOGONAL (nPts*nPts)
        // J IS THE JACOBIAN (nPts*param) AT THE TERMINATING POINT
        // D IS A DIAGONAL MATRIX (param*param)
        // E IS A PERMUTATION MATRIX (param*param)
        // R IS AN UPPER TRIANGULAR MATRIX (param*param)
        // param CONTAINS LENGTH OF THE ARRAYS diag,pivit,w0,dx AND WORK
        // internalScaling = false IF NO SCALING OF THE JACOBIAN J IS DONE
        // = true IF SCALING IS DONE
        // diag CONTAINS THE DIAGONAL ELEMENTS OF THE DIAGONAL MATRIX
        // D ABOVE (IF SCALING IS DONE OTHERWISE UNDEFINED)
        // pivit CONTAINS THE PERMUTATION MATRIX E ABOVE
        // w0 CONTAINS INFO. NEEDED TO FORM MATRIX Q ABOVE
        // pseduoRank CONTAINS PSEUDO RANK USED TO COMPUTE GN-DIRECTION IN dx
        // dx CONTAINS GN-DIRECTION
        // v[] CONTAINS THE ORTHOGONAL PROJECTION OF residuals ONTO THE SPACE
        // SPANNED BY THE COLUMNS OF THE JACOBIAN OF residuals(a)

        // ON RETURN

        // d1sqs CONTAINS PREDICTED REDUCTION IN THE OBJECTIVE FUNCTION
        // IF SUGGESTED SEARCH DIRECTION IS USED
        // dxnorm CONTAINS THE NORM OF THE SUGGESTED SEARCH DIRECTION
        // dx CONTAINS THE SUGGESTED SEARCH DIRECTION
        // v[] CONTAINS THE ORTHOGONAL PROJECTION OF residuals ONTO THE SPACE
        // SPANNED BY rank LINEARLY INDEPENDENT COLUMNS OF THE
        // JACOBIAN OF residuals(a)
        // RANK  CONTAINS DIMENSION OF THE SUBSPACE USED TO COMPUTE
        // THE SUGGESTED SEARCH DIRECTION

        // WORKING AREAS

        // work[] double 1D ARRAY OF DIMENSION param
        // w1[]   double 1D ARRAY OF DIMENSION nPts

        double[] work = new double[param];

        // FIND APPROPRIATE SUBSPACE

        dimsub();

        // COMPUTE SEARCH DIRECTION BY MINIMIZING IN A SUBSPACE OF DIMENSION RANK

        pseudoRankMat[0] = rank;
        d1sqs = gndunc(internalScaling, pseudoRankMat, work);
        rank = pseudoRankMat[0];
        dxnorm = dnrm2(param, dx, 1);

        return;
    }

    /**
     * DOCUMENT ME!
     *
     * @param  aNorm  DOCUMENT ME!
     */
    private void termuc(double aNorm) {
        // CHECK IF ANY OF THE TERMINATION CRITERIA ARE SATISFIED
        // THE CONVERGENCE CRITERIA ARE ONLY CHECKED IF THE LATEST SEARCH
        // DIRECTION WAS COMPUTED USING THE GAUSS-NEWTON WITH FULL PSEUDO
        // RANK OR IF THE METHOD OF NEWTON WAS USED AND PROVIDED THAT NO
        // RESTART WAS DONE


        // THE CONVERGENCE CRITERIA ARE

        // 1) RELATIVE PREDICTED REDUCTION IN THE OBJECTIVE FUNCTION
        // IS LESS THAN relativeConvergence**2
        // 2) THE SUM OF SQUARES IS LESS THAN absoluteConvergence**2
        // 3) THE RELATIVE CHANGE IN a[] IS LESS THAN parameterConvergence
        // 4) WE ARE COMPUTING AT NOISE LEVEL
        // THE LAST DIGIT IN THE CONVERGENCE CODE (SEE BELOW) INDICATES
        // HOW THE LAST STEPS WERE COMPUTED
        // = 0 NO TROBLE (GAUSS-NEWTON THE LAST 3 STEPS)
        // = 1 pseudoRank <> param AT THE TERMINATION POINT
        // = 2 THE METHOD OF NEWTON WAS USED (AT LEAST) IN THE LAST STEP
        // = 3 THE 2:ND BUT LAST STEP WAS SUBSPACE MINIMIZATION BUT THE
        // LAST TWO WERE GAUSS-NEWTON STEPS
        // = 4 THE STEPLENGTH WAS NOT UNIT IN BOTH THE LAST TWO STEPS


        // THE ABNORMAL TERMINATION CRITERIA ARE

        // 5) NO. OF ITERATIONS HAS EXCEEDED MAXIMUM ALLOWED ITERATIONS
        // 6) THE HESSIAN EMANATING FROM 2.ND ORDER METHOD IS NOT POS. DEF.
        // 7) THE ALGORITHM WOULD LIKE TO USE 2:ND DERIVATIVES BUT IS
        // NOT ALLOWED TO DO THAT
        // 8) AN UNDAMPED STEP WITH NEWTONS METHOD IS A FAILURE
        // 9) THE LATEST SEARCH DIRECTION COMPUTED USING SUBSPACE
        // MINIMIZATION WAS NOT A DESCENT DIRECTION (PROBABLY CAUSED
        // BY WRONGLY COMPUTED JACOBIAN)

        // ON ENTRY

        // pseudoRank   INTEGER SCALAR CONTAINING PSEUDO RANK USED TO COMPUTE
        // THE SEARCH DIRECTION IF SUBSPACE MINIMIZATION IS USED
        // = -param IF SECOND ORDER METHOD IS USED
        // errorStatus   INTEGER SCALAR CONTAINING
        // = -1 IF PREVIOUS DIRECTION WAS NOT A DESCENT DIRECTION
        // = -2 IF THE CHANGE IN a < SQRT(SRELPR)
        // = -3 IF NOT POS. DEF. HESSIAN FROM SECOND ORDER METHOD
        // = -4 IF THE ALGORITHM WANTS TO USE SECOND DERIVATIVES
        // BUT IS NOT ALLOWED TO DO THAT
        // = -5 IF AN UNDAMPED NEWTON STEP FAILS
        // =  0 OTHERWISE
        // restart  LOGICAL SCALAR = TRUE IF A RESTART STEP
        // = FALSE IF THIS STEP IS NOT A RESTART STEP
        // maxIterations INTEGER SCALAR CONTAINING MAXIMUM ALLOWED ITERATIONS
        // iters INTEGER SCALAR CONTAINING ITERATION NO.
        // fsum double SCALAR CONTAINING SUM OF SQUARES AT CURRENT POINT
        // d1sqs double SCALAR CONTAINING PREDICTED REDUCTION IN THE
        // OBJECTIVE FUNCTION IF ONE MORE GAUSS-NEWTON STEP
        // IS TAKEN
        // param INTEGER SCALAR CONTAINING NUMBER OF PARAMETERS
        // aNorm double SCALAR CONTAINING NORM OF CURRENT POINT
        // alfnoi double SCALAR CONTAINING A NOISE LEVEL
        // aDiff double SCALAR CONTAINING CHANGE IN LATEST SUCCESIVE POINTS
        // absoluteConvergence  double SCALARS CONTAINING SMALL POSITIVE VALUES
        // relativeConvergence  USED IN CHECKING THE CONVERGENCE CRITERIA
        // parameterConvergence
        // g[] double 1D ARRAY OF DIMENSION param CONTAINING
        // THE GRADIENT OF THE OBJECTIVE AT THE CURRENT POINT
        // bl[] double 1D ARRAY OF DIMENSION param CONTAINING
        // THE LOWER BOUNDS OF THE UNKNOWNS
        // bu[] double 1D ARRAY OF DIMENSION param CONTAINING
        // THE UPPER BOUNDS OF THE UNKNOWNS
        // paramAct INTEGER SCALAR CONTAINING THE NO. OF ACTIVE CONSTRAINTS
        // exitStatus INTEGER 1D ARRAY OF DIMENSION param
        // CONTAINING A CODE WHICH INDICATES WHETHER AN UNKNOWN IS
        // ACTIVE OR NOT.
        // aset[i] = 0 WHEN a[i] IS FREE
        // = +1 WHEN a[i] EQUALS bl[i]
        // = -1 WHEN a[i] EQUALS bu[i]

        // ON RETURN

        // exitStatus INTEGER SCALAR CONTAINING
        // = 0 IF NO TERMINATION CRITERIUM IS SATISFIED
        // = 10000 CRITERIUM 1) IS SATISFIED
        // =  2000     #     2)      #
        // =   300     #     3)      #
        // =    40     #     4)      #
        // =     X  WHERE X EQUALS 0,1,2,3 OR 4
        // =    -2     #     5)      #
        // =    -3     #     6)      #
        // =    -4     #     7)      #
        // =    -5     #     8)      #
        // =    -6     #     9)      #

        double rlmax;
        int i;
        noise = 0.1;
        eps = 0.01;
        epsl = 0.001;

        exitStatus = 0;
        rlmax = 0.0;

        // THE CONVERGENCE CRITERIA ARE NOT CHECKED IF THE LATEST STEP
        // WAS A RESTART STEP OR WAS NOT A FULL PSEUDO RANK STEP

        if ((imax != -1) || (restart) || ((kodkm1 == -1) && (alfnoi <= noise)) ||
                ((errorStatus < 0) && (errorStatus != -2))) {

            // criterium no. 5
            if (iters > maxIterations) {
                exitStatus = -2;
            }

            // criterium no. 9
            if (errorStatus == -1) {
                exitStatus = -6;
            }

            // criterium no. 6-8
            if ((errorStatus >= -5) && (errorStatus <= -3)) {
                exitStatus = errorStatus;
            }

            return;
        }

        Preferences.debug("In TERMUC:\n");

        // CRITERIUM NO. 1

        if (d1sqs <= (fsum * relativeConvergence * relativeConvergence)) {
            exitStatus = exitStatus + 10000;
        }

        // CRITERIUM NO. 2

        if (fsum <= (absoluteConvergence * absoluteConvergence)) {
            exitStatus = exitStatus + 2000;
        }

        // CRITERIUM NO. 3

        if (aDiff < (aNorm * parameterConvergence)) {
            exitStatus = exitStatus + 300;
        }

        // CRITERIUM NO. 4

        if ((alfnoi > noise) || (errorStatus == -2)) {
            exitStatus = exitStatus + 40;
        }

        if (exitStatus == 0) {

            // criterium no. 5
            if (iters > maxIterations) {
                exitStatus = -2;
            }

            // criterium no. 9
            if (errorStatus == -1) {
                exitStatus = -6;
            }

            // criterium no. 6-8
            if ((errorStatus >= -5) && (errorStatus <= -3)) {
                exitStatus = errorStatus;
            }

            return;
        }

        // CHECK LAGRANGE MULTIPLIERS

        Preferences.debug("Checking Lagrange mult. in TERMUC\n");

        for (i = 0; i < param; i++) {
            Preferences.debug(g[i] + "\n");
        }

        if (paramAct != 0) {

            for (i = 0; i < param; i++) {

                if (Math.abs(g[i]) < epsl) {
                    continue;
                }

                if ((g[i] * aset[i] * (bu[i] - bl[i])) >= 0.0) {
                    continue;
                }

                if (rlmax > Math.abs(g[i])) {
                    continue;
                }

                rlmax = Math.abs(g[i]);
                exitStatus = 0;
            } // for (i = 0; i < param; i++)
        } // if (paramAct != 0)

        if (exitStatus == 0) {

            // criterium no. 5
            if (iters > maxIterations) {
                exitStatus = -2;
            }

            // criterium no. 9
            if (errorStatus == -1) {
                exitStatus = -6;
            }

            // criterium no. 6-8
            if ((errorStatus >= -5) && (errorStatus <= -3)) {
                exitStatus = errorStatus;
            }

            return;
        }

        // DETERMINE THE LAST DIGIT IN THE CONVERGENCE CODE

        if (Math.abs(kodkm1) == 2) {
            exitStatus = exitStatus + 2;

            return;
        }

        if (pseudoRank != (param - paramAct)) {
            exitStatus = exitStatus + 1;

            return;
        }

        if ((kodkm2 != 1) && (iters >= 2)) {
            exitStatus = exitStatus + 3;

            return;
        }

        if ((Math.abs(alfkm2 - 1.0) <= eps) && (Math.abs(alfkm1 - 1.0) <= eps)) {
            return;
        }

        exitStatus = exitStatus + 4;

        return;
    }

    /**
     * DOCUMENT ME!
     *
     * @param   tol       DOCUMENT ME!
     * @param   paramAct  DOCUMENT ME!
     *
     * @return  DOCUMENT ME!
     */
    private int triunc(double tol, int paramAct) {
        // MAKE A QR-DECOMPOSITION OF THE nPts*param MATRIX covarMat BY USING THE ROUTINE SQRDC FROM LINPACK I.E.
        // DETEMINE MATRICES Q,E,R SO THAT      T                                          Q *covarMat*E = (R)
        //                                                (0) WHERE  Q IS nPts*nPts ORTHOGONAL E IS param*param
        // PERMUTATION MATRIX R IS param*param UPPER TRIANGULAR

        // ON ENTRY

        // residuals[]  contains the right hand side in covarMat*dx = residuals
        // covarMat[][] CONTAINS THE nPts*param MATRIX TO DECOMPOSE
        // nPts    NO. OF ROWS IN MATRIX C
        // param   NO. OF COLUMNS IN MATRIX C
        // tol  A SMALL POSITIVE VALUE USED TO DETERMINE PSEUDO RANK OF
        // MATRIX covarMat  (SEE pseudoRank)
        // paramAct INTEGER SCALAR CONTAINING THE NO. OF ACTIVE CONSTRAINTS
        // aset[] INTEGER 1D ARRAY OF DIMENSION param CONTAINING A CODE
        // WHICH INDICATES WHETHER AN UNKNOWN IS ACTIVE OR NOT.
        // ASET(I) = 0 WHEN a[I] IS FREE
        // = +1 WHEN a[I] EQUALS BL[I]
        // = -1 WHEN a[I] EQUALS BU[I]

        // ON RETURN

        // THE ARRAYS covarMat,pivit AND w0 CONTAIN THE CORRESPONDING OUTPUT FROM
        // LINPACK ROUTINE SQRDC
        // ABS(R[0,0]) >= ABS(R[1,1]) >=.......>=ABS(R[param-1,param-1])

        int i, j, k, nn;
        double r11;

        if (param == 0) {
            return 0;
        }

        // INITIATE PIVOT VECTOR SO THAT ALL COLUMNS CORRESPONDING TO
        // ACTIVE BOUNDS ARE CONSIDERED AS FINAL COLUMNS

        for (i = 0; i < param; i++) {
            pivit[i] = 0;

            if (aset[i] != 0) {
                pivit[i] = -1;
            }
        } // for (i = 0; i < param; i++)

        // DECOMPOSE MATRIX covarMat

        sqrdc(covarMat, nPts, param, w0, pivit, 1);

        // DETERMINE PSEUDO RANK

        k = -1;
        r11 = Math.abs(covarMat[0][0]);
        nn = param - paramAct;

        if (nn == 0) {
            return 0;
        }

        for (i = 0; i < nn; i++) {

            if (Math.abs(covarMat[i][i]) >= tol) {
                k = i;
            }
        } // for (i = 0; i < nn; i++)

        if (k == -1) {

            for (j = 0; j < nn; j++) {

                if (Math.abs(covarMat[j][j]) <= (tol * r11)) {
                    break;
                }

                k = j;
            } // for (j = 0; j < nn; j++)
        } // if (k == -1)

        return (k + 1);
    }

    /**
     * DOCUMENT ME!
     */
    private void twor() {
        // COMPUTE THE THREE ROOTS OF A THIRD DEGREE POLYNOMIAL WHEN
        // THERE ARE 3 REAL ROOTS

        double eps = 1.0E-8;
        double fi, sqd, t, tanfi;

        sqd = Math.sqrt(-deltaMinrm);

        if (Math.abs(qMinrm) <= (2.0 * eps * sqd)) {
            fi = Math.PI / 2.0;
        } else {
            tanfi = Math.abs(2.00 * sqd / qMinrm);
            fi = Math.atan(tanfi);
        }

        t = 2.0 * Math.sqrt(-pMinrm / 3.0);

        if (qMinrm > 0.0) {
            t = -t;
        }

        x1Minrm = (t * Math.cos(fi / 3.0)) - a1div3;
        x2Minrm = (t * Math.cos((fi + (2.0 * Math.PI)) / 3.0)) - a1div3;
        x3Minrm = (t * Math.cos((fi + (4.0 * Math.PI)) / 3.0)) - a1div3;

        return;
    }

}
