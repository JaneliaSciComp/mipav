package gov.nih.mipav.model.algorithms;

import gov.nih.mipav.view.*;
import gov.nih.mipav.util.DoubleDouble;


/**
 * This is a port of the program ELSUNC for solving least squares problems where the unknowns are restricted by lower
 * and upper bounds. Reference: Gauss-Newton Based Algorithms For Constrained Nonlinear Least Squares Problems by Per
 * Lindstrom and Per-Ake Wedin, Institute of Information Processing, University of Umea, S-901 87 Umea, Sweden This can
 * be downleaded from http://www.cs.umu.se/~perl/reports/alg.ps.gz
 * 
 * The original code could incorrectly set the number of active constraints 1 greater than the number of parameters in 
 * gnavuc.  The code for setting constraintAct was improved in 2 places to prevent this problem.  First, in evreuc:
 * Before the loop for (i = 0; i < param; i++) put constraintAct = 0; and in the loop change
 *  if (aset[i] != 0) {
 *      continue;
 *  }
 *  to:
 *  aset[i] = 0;
 *  so that aset would be evaluated for each parameter.
 *  In gnavuc changed constraintAct++; to 
 *  if ((aset[imax] == 0) && (ival != 0)) {
 *      constraintAct++;
 *  }
 *  
 *  For the self tests:
 *  DRAPER24D OK.
 *  HOCK25 OK.
 *  BARD Unconstrained only worked at the standard  starting point.  This is as expected
 *  because a[1] is a root in the denominator and so with unconstrained for large
 *  ranges a[1] = 0 will result in an infinity.  Constrained with a[1] >= 0.1
 *  works at 10.0 * starting point and at 100.0 * starting point.
 *  KOWALIK_AND_OSBORNE at standard initial starting point OK.  At 10 * standard initial starting point only
 *  constrained OK.  At 100 * starting point does not work
 *  Note that the article Gauss-Newton Based Algorithms for Constrained Nonlinear Least Squares Problems by 
 *  Per Lindstrom and Per-ake Wedin says, "However, on the problems BARD and KOWALIK_AND_OSBORNE commented ELSUNC
 *  started at 10*Xs and 100*Xs, converges towards a reported unbounded local minimizer.
 *  I found the following statement about SolvOpt at http://www.uni-graz.at/imawww/kuntsevich/solvopt/results/moreset.html
 *  in Something about More set of test functions.
 *  Function #15 - Kowalik and Osborne
    This function has the global minimum f0=0.000307506
    at the point [0.192807; 0.191282; 0.123056; 0.136062].
    Additionally, it takes a local minimum f=0.00179453906640 on a big flat plateau. The following are the minimum points found on this plateau by SolvOpt:
    [0.816961; 2045080438; 7189803671; 2927176299],
    [0.734588; 2648629959; 8372807699; 3408733052],
    [0.537138; 1852002979; 4280874360; 1742904647].
    These particular points appear to be "false" local minima on a flat plateau, the fact caused by the finite accuracy of calculations. 
 *  MEYER starting point okay.  At 10 * standard starting point constrained does not work.
 *  Remember that Java is limited to 64 bits whereas the FORTRAN used by the ELSUNC program has access to the full 80 processor bits.
 *  >There are basically 2 problems with Java precision. 1.) The Intel Pentium uses 80 bit numbers in floating point
 * registers. However, Java must round each number back to 64 bits whenever a Java variable is assigned. There used to
 * be a proposal to introduce a special keyword extendedfp to fully use whatever math the platform had, but it didn't
 * get thru. Apparently the Java designers felt for Java being consistent is more important than being successful. 2.)
 * Java also forbids the use of fused multiply-add (FMA) operations. This operation computes the quantity ax + y as a
 * single floating-point operation. Operations of this type are found in many compute-intensive applications,
 * particularly matrix operations. With this instruction, only a single rounding occurs for the two arithmetic
 * operations, yielding a more accurate result in less time than would be required for two separate operations. Java's
 * strict language definition does not permit use of FMAs and thus sacrifices up to 50% of performance on some
 * platforms.  
 * The smaller number of bits on Java would be particularly prevalent for the big flat plateau of the Kowalik and
 * Osborne function.  With the Kowalik and Osborne run from 10 * the standard starting point, all 4 runs terminate
 * normally purely because we are computing at the noise level.
 * 
 *  >The only obvious cure for this problem would be to use the Java BigDecimal class. However, the use of BigDecimal
 * would involve much more work than just changing doubles to BigDecimals. For example, with doubles I would write: f =
 * f/g; With BigDecimal I would write: f = f.divide(g, mc); where mc is the mathematical context settings. Likewise,
 * instead of the ordinary arithmetic operators I would have to use add, compareTo, equals, negate, plus, remainder, and
 * subtract. Using BigDecimal would undoubtedly slow the program down considerably.  In addition, BigDecimal cannot do
 * many functions, such as log, exp, and trigonometric functions.
 * 
 * While Meyer with 10 * the original starting point works for the original ELSUNC program, it requires far more
 * iterations than any of the other test problems - 770 for unconstrained and 782 for constrained.  Since it barely
 * works for 80 bit FORTRAN, one would not expect it to work for 64 bit Java.
 *  OSBRONE1 at starting point unconstrained okay.
 *  OSBORNE2 at starting point unconstrained okay.
 *   
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

public abstract class NLConstrainedEngineEP {

    //~ Static fields/initializers -------------------------------------------------------------------------------------

    /** DOCUMENT ME! */
    private static final double big = 1.0E32;

    //~ Instance fields ------------------------------------------------------------------------------------------------

    /** 1D array of length param containing the unknown parameters. */
    /** On completion if exitStatus >= 0 a[] contains the latest (best) estimate of the solution point. */
    protected DoubleDouble[] a;

    /**
     * absolute convergence constant Used to test for convergence by the sum of squares being less than the
     * absoluteConvergence squared.
     */
    protected DoubleDouble absoluteConvergence;

    /** DOCUMENT ME! */
    protected DoubleDouble[] bl;

    /**
     * integer scalar code for assessing the bounds bounds = 0 means an unconstrained problem bounds = 1 means the same
     * lower bounds for all unknowns and the same upper bounds for all unknowns. The lower and upper bound must be
     * stored in bl[0] and bu[0] respectively. bounds = 2 means that the bounds must be supplied by the user in bl[i]
     * and bu[i] for i = 0,1, param - 1.
     */
    protected int bounds;

    /** DOCUMENT ME! */
    protected DoubleDouble[] bu;

    /**
     * 2D array of doubles of dimension nPts by max(4,param) containing the main part of the covariance matrix
     * (Jtranspose * J)inverse, where J is the Jacobian matrix computed at a.
     */
    protected DoubleDouble[][] covarMat;

    /** Wrapper for ctrl or lctrl communication with driver. */
    protected int[] ctrlMat = new int[1];

    /** DOCUMENT ME! */
    protected DoubleDouble dyda[], stdv;

    /** DOCUMENT ME! */
    protected DoubleDouble[] gues;

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
    protected DoubleDouble parameterConvergence;

    /**
     * relative convergence constant Used to test for convergence by reduction in objective function being less than
     * relativeConvergence squared.
     */
    protected DoubleDouble relativeConvergence;

    /** 1D array of doubles of dimension nPts containing the value of the residuals at the termination point a;. */
    protected DoubleDouble[] residuals;

    /**
     * The method of Newton is allowed at the end of the iteration in some situations (large residuals). If true, the
     * use of second derivatives is permitted
     */
    protected boolean secondAllowed = true;

    /** pseudo rank tolerance constant used to determine the pseudo rank of the Jacobian J. */
    protected DoubleDouble tolerance;

    /** DOCUMENT ME! */
    private DoubleDouble[] xSeries;

    /** DOCUMENT ME! */
    private DoubleDouble[] ySeries;

    /** DOCUMENT ME! */
    private DoubleDouble aDiff;

    /** DOCUMENT ME! */
    private DoubleDouble alfnoi;

    /** DOCUMENT ME! */
    private DoubleDouble alpha;

    /** DOCUMENT ME! */
    private DoubleDouble alphup;

    /**
     * 1D integer array of length param containing a code which indicates whether an unknown is active or not aset[i] =
     * 0 when a[i] is free = +1 when a[i] == bl[i] = -1 when a[i] == bu[i].
     */
    private int[] aset;

    /**
     * scalar containing the norm of the orthogonal projection of residuals onto the space spanned by the columns of the
     * Jacobian.
     */
    private DoubleDouble beta;

    /** DOCUMENT ME! */
    private DoubleDouble betkm2, alfkm2, betkm1, d1km1, fsqkm1, dxnkm1, alfkm1, aupkm1;

    /** DOCUMENT ME! */
    private DoubleDouble bn;

    /** DOCUMENT ME! */
    private DoubleDouble cnorm;

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
    private DoubleDouble d1sqs;

    /**
     * 1D double array of length param conatiaing the diagonal elements of the diagonal matrix D if internalScaling is
     * true. Otherwise, undefined.
     */
    private DoubleDouble[] diag;

    /** double working areas 1D array of length param containing the solution of J * dx = -residuals. */
    private DoubleDouble[] dx;

    /** DOCUMENT ME! */
    private DoubleDouble dxnorm;

    /** DOCUMENT ME! */
    private DoubleDouble eps;

    /** DOCUMENT ME! */
    private DoubleDouble epsl;

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
    private DoubleDouble fsum;

    /** integer scalar containing the total number of function evaluations done inside this routine. */
    private int funcEval;

    /** 1D array of length param containing the gradient of the objective at the current point. */
    private DoubleDouble[] g;

    /**
     * param by param array If secondAllowed if false, gmat can be used as a singly subscripted array of dimension param
     * by setting all second indices to 0.
     */
    private DoubleDouble[][] gmat;

    /** DOCUMENT ME! */
    private boolean gndok;

    /** scalar containing norm of the gradient divided by norm of Jacobian. */
    private DoubleDouble gnorm;

    /** Variables used for restart information. */
    private int icount, lattry, itotal, irank, imax, ival;

    /** DOCUMENT ME! */
    private int iev;

    /** DOCUMENT ME! */
    private int ifree, indic;

    /** Consistently use 1 base indexing rather than 0 based indexing */
    private int info;

    /**
     * integer scalar containing the number of function evaluations caused by computing Jacobians with difference
     * methods.
     */
    private int jacobianEval;

    /** DOCUMENT ME! */
    private int kod;

    /** integer scalar containing the number of function evaluations caused by the linesearch algorithm. */
    private int lineEval;

    /** Leading dimension of array gmat mdg must be >= param if secondAllowed is true. */
    private int mdg;

    /** DOCUMENT ME! */
    private DoubleDouble noise;

    /** Number of active constraints. */
    private int constraintAct;

    /** DOCUMENT ME! */
    private DoubleDouble pgb1;

    /** DOCUMENT ME! */
    private DoubleDouble pgb2;

    /** double scalar containging the value of the objective function = (0.5 * chisquare) at the termination point a. */
    private DoubleDouble phi;

    /** DOCUMENT ME! */
    private DoubleDouble phikp1;

    /** DOCUMENT ME! */
    private DoubleDouble philat, bestpg;

    /**
     * array of integers of dimension param containing the permutation matrix E in the decomposition QTranspose * J * D
     * E = (R) (0) pivit[i] contains the index of the column that was moved into column i.
     */
    private int[] pivit;

    /** DOCUMENT ME! */
    private DoubleDouble pMinrm, qMinrm, deltaMinrm, a1div3;

    /** DOCUMENT ME! */
    private DoubleDouble predb;

    /** scalar containing the predicted reduction in the objective if pseudoRank from previous step is used. */
    private DoubleDouble prekm1;

    /** DOCUMENT ME! */
    private DoubleDouble rabs;

    /** DOCUMENT ME! */
    private boolean restart;

    /** DOCUMENT ME! */
    private DoubleDouble rlenb;

    /** DOCUMENT ME! */
    private DoubleDouble rmin;

    /**
     * The suffices km2 and km1 in the names of the variables represent time step k-2 and k-1 respectively. These
     * variables are updated only inside the routine evreuc.
     */
    private int kodkm2, rngkm1, kodkm1;

    /** DOCUMENT ME! */
    private DoubleDouble rootsp, bl0, bu0;

    /** DOCUMENT ME! */
    private int secind;

    /**
     * integer scalar containing the number of function evaluations caused by using second derivatives in the method of
     * Newton.
     */
    private int secondEval;

    /** DOCUMENT ME! */
    private DoubleDouble smax;

    /** DOCUMENT ME! */
    private DoubleDouble sn;

    /** double scalar containing an estimate of the linear convergence factor. */
    private DoubleDouble speed;

    /** single relative precision. */
    private DoubleDouble srelpr;

    /** DOCUMENT ME! */
    private DoubleDouble stepb;

    /**
     * 1D array of length nPts containing the orthogonal projection of residuals onto space spanned by the first
     * pseudoRank linear independent columns of the Jacobian.
     */
    private DoubleDouble[] v;

    /** DOCUMENT ME! */
    private DoubleDouble v0norm, v1norm, v2norm, scv0v1, scv0v2, scv1v2;

    /** DOCUMENT ME! */
    private DoubleDouble[] w0;

    /** DOCUMENT ME! */
    private DoubleDouble[] w1;

    /** DOCUMENT ME! */
    private DoubleDouble[] w2;

    /** DOCUMENT ME! */
    private DoubleDouble[] w3;

    /** DOCUMENT ME! */
    private DoubleDouble x1Minrm, x2Minrm, x3Minrm;
    
    protected boolean outputMes = true;
    
    private boolean testMode = false; 
    
    private boolean  analyticalJacobian = true;
    
    private int testCase;
    
    private final int DRAPER24D = 0;
    
    private final int JENNRICH_AND_SAMPSON = 6;
    
    private final int BARD = 8;
    
    private final int MEYER = 10;
    
    private final int KOWALIK_AND_OSBORNE = 15;
    
    private final int OSBORNE1 = 17;
    
    private final int OSBORNE2 = 19;
    
    private final int WATSON = 20;
    
    private final int HOCK25 = 25;

    //~ Constructors ---------------------------------------------------------------------------------------------------

    public NLConstrainedEngineEP() {
    	int i;
    	// Below is an example used to fit y = a0 - a1*(a2**x)
    	// This example implements the solution of problem D of chapter 24 of Applied Regression Analysis, Third Edition by
    	// Norman R. Draper and Harry Smith */
    	// The correct answer is a0 = 72.4326,  a1 = 28.2519, a2 = 0.5968
    	// Works for 4 possibilities of internalScaling = true, false; Jacobian calculation = numerical, analytical
  
    	Preferences.debug("Draper problem 24D y = a0 - a1*(a2**x) constrained\n", Preferences.DEBUG_ALGORITHM);
    	Preferences.debug("Correct answer is a0 = 72.4326, a1 = 28.2519, a2 = 0.5968\n", Preferences.DEBUG_ALGORITHM);
    	testMode = true;
    	testCase = DRAPER24D;
    	nPts = 5;
    	param = 3;
        xSeries = new DoubleDouble[5];
        ySeries = new DoubleDouble[nPts];
        gues = new DoubleDouble[param];
        xSeries[0] = DoubleDouble.valueOf(0.0);
        xSeries[1] = DoubleDouble.valueOf(1.0);
        xSeries[2] = DoubleDouble.valueOf(2.0);
        xSeries[3] = DoubleDouble.valueOf(3.0);
        xSeries[4] = DoubleDouble.valueOf(4.0);
        ySeries[0] = DoubleDouble.valueOf(44.4);
        ySeries[1] = DoubleDouble.valueOf(54.6);
        ySeries[2] = DoubleDouble.valueOf(63.8);
        ySeries[3] = DoubleDouble.valueOf(65.7);
        ySeries[4] = DoubleDouble.valueOf(68.9);
        fitTestModel();
        gues[0] = DoubleDouble.valueOf(0.0);
        gues[1] = DoubleDouble.valueOf(10.0);
        gues[2] = DoubleDouble.valueOf(0.2);
        bounds = 2; // bounds = 0 means unconstrained

        // bounds = 1 means same lower and upper bounds for
        // all parameters
        // bounds = 2 means different lower and upper bounds
        // for all parameters
        // Constrain a0
        bl = new DoubleDouble[param];
        bu = new DoubleDouble[param];
        bl[0] = DoubleDouble.valueOf(-1000.0);
        bu[0] = DoubleDouble.valueOf(1000.0);

        // Constrain a1
        bl[1] = DoubleDouble.valueOf(-1000.0);
        bu[1] = DoubleDouble.valueOf(1000.0);

        // Constrain a2
        bl[2] = DoubleDouble.valueOf(0.0);
        bu[2] = DoubleDouble.valueOf(1.0);
        driverCalls();
        
        // Below is an example used to fit y = 100 * (a2 - a1)**2 + 
        
        // Below is an example used to fit y = (a0 * log(0.01*i)**(a1) + a2
    	// where a0 = -50, a1 = 2.0/3.0, a2 = 25.0
    	// Variant of test example 25 from Hock and Schittkowski
    	// Works for 4 possibilities of internalScaling = true, false; Jacobian calculation = numerical, analytical
        Preferences.debug("Test example 25 from Hock and Schittkowski constrained\n", Preferences.DEBUG_ALGORITHM);
        Preferences.debug("y = (a0 * log(0.01*i)**(a1) + a2\n", Preferences.DEBUG_ALGORITHM);
        Preferences.debug("Correct answer is a0 = -50, a1 = 2.0/3.0, a3 = 25.0\n", Preferences.DEBUG_ALGORITHM);
        testMode = true;
        testCase = HOCK25;
        nPts = 99;
        param = 3;
    	xSeries = new DoubleDouble[99];
    	ySeries = new DoubleDouble[nPts];
    	gues = new DoubleDouble[param];
    	DoubleDouble exp;
    	for (i = 1; i <= 99; i++) {
    		xSeries[i-1] = (DoubleDouble.valueOf(0.01)).multiply(DoubleDouble.valueOf(i));
    		ySeries[i-1] = (DoubleDouble.valueOf(-50.0)).multiply(xSeries[i-1].log());
    		exp = (DoubleDouble.valueOf(2.0)).divide(DoubleDouble.valueOf(3.0));
    		ySeries[i-1] = ySeries[i-1].pow(exp);
    		ySeries[i-1] = ySeries[i-1].add(DoubleDouble.valueOf(25.0));
    	}
    	fitTestModel();
    	gues[0] = DoubleDouble.valueOf(-100.0);
    	gues[1] = (DoubleDouble.valueOf(1.0)).divide(DoubleDouble.valueOf(3.0));
    	gues[2] = DoubleDouble.valueOf(12.5);
    	bounds = 2; // bounds = 0 means unconstrained

        // bounds = 1 means same lower and upper bounds for
        // all parameters
        // bounds = 2 means different lower and upper bounds
        // for all parameters
        bl = new DoubleDouble[param];
        bu = new DoubleDouble[param];
        // Constrain a[0]
        bl[0] = DoubleDouble.valueOf(-200.0);
        bu[0] = DoubleDouble.valueOf(-0.1);

        // Constrain a[1]
        bl[1] = DoubleDouble.valueOf(0.0);
        bu[1] = DoubleDouble.valueOf(5.0);

        // Constrain a[2]
        bl[2] = DoubleDouble.valueOf(0.0);
        bu[2] = DoubleDouble.valueOf(25.6);
        driverCalls();
        // Below is an example to fit y = a0 + x/(a1*(16 - x) + a2*min(x,16-x))
        // From Testing Unconstrained Optimization Software by More, Garbow, and Hillstrom
        // Unconstrained only worked at the standard  starting point.  This is as expected
        // because a[1] is a root in the denominator and so with unconstrained for large
        // ranges a[1] = 0 will result in an infinity.  Constrained with a[1] >= 0.1
        // works at 10.0 * starting point and at 100.0 * starting point.
        Preferences.debug("Bard function standard starting point unconstrained\n", Preferences.DEBUG_ALGORITHM);
        Preferences.debug("y = a0 + x/(a1*(16-x) + a2*min(x,16-x))\n", Preferences.DEBUG_ALGORITHM);
        Preferences.debug("Correct answer is a0 = 0.08241, a1 = 1.133, a2 = 2.344\n", Preferences.DEBUG_ALGORITHM);
        Preferences.debug("Correct answer has Chi-squared = 8.21487E-3\n", Preferences.DEBUG_ALGORITHM);
        testMode = true;
        testCase = BARD;
        nPts = 15;
        param = 3;
        xSeries = new DoubleDouble[15];
        for (i = 1; i <= 15; i++) {
        	xSeries[i-1] = DoubleDouble.valueOf(i);
        }
        ySeries = new DoubleDouble[]{DoubleDouble.valueOf(0.14), DoubleDouble.valueOf(0.18),
        		                     DoubleDouble.valueOf(0.22), DoubleDouble.valueOf(0.25),
        		                     DoubleDouble.valueOf(0.29), DoubleDouble.valueOf(0.32),
        		                     DoubleDouble.valueOf(0.35), DoubleDouble.valueOf(0.39), 
        		                     DoubleDouble.valueOf(0.37), DoubleDouble.valueOf(0.58),
        		                     DoubleDouble.valueOf(0.73), DoubleDouble.valueOf(0.96),
        		                     DoubleDouble.valueOf(1.34), DoubleDouble.valueOf(2.10),
        		                     DoubleDouble.valueOf(4.39)};
        gues = new DoubleDouble[param];
        fitTestModel();
        gues[0] = DoubleDouble.valueOf(1.0);
        gues[1] = DoubleDouble.valueOf(1.0);
        gues[2] = DoubleDouble.valueOf(1.0);
        
        bounds = 0;  // bounds = 0 means unconstrained

        // bounds = 1 means same lower and upper bounds for
        // all parameters
        // bounds = 2 means different lower and upper bounds
        // for all parameters
        bl = new DoubleDouble[param];
        bu = new DoubleDouble[param];
        driverCalls();
        // Below is an example to fit y = a0 + x/(a1*(16 - x) + a2*min(x,16-x))
        // From Testing Unconstrained Optimization Software by More, Garbow, and Hillstrom
        // Unconstrained only worked at the standard  starting point.  This is as expected
        // because a[1] is a root in the denominator and so with unconstrained for large
        // ranges a[1] = 0 will result in an infinity.  Constrained with a[1] >= 0.1
        // works at 10.0 * starting point and at 100.0 * starting point.
        Preferences.debug("Bard function 10 * standard starting point constrained\n", Preferences.DEBUG_ALGORITHM);
        Preferences.debug("y = a0 + x/(a1*(16-x) + a2*min(x,16-x))\n", Preferences.DEBUG_ALGORITHM);
        Preferences.debug("Correct answer is a0 = 0.08241, a1 = 1.133, a2 = 2.344\n", Preferences.DEBUG_ALGORITHM);
        Preferences.debug("Correct answer has Chi-squared = 8.21487E-3\n", Preferences.DEBUG_ALGORITHM);
        testMode = true;
        testCase = BARD;
        nPts = 15;
        param = 3;
        xSeries = new DoubleDouble[15];
        for (i = 1; i <= 15; i++) {
        	xSeries[i-1] = DoubleDouble.valueOf(i);
        }
        ySeries = new DoubleDouble[]{DoubleDouble.valueOf(0.14), DoubleDouble.valueOf(0.18),
        		                     DoubleDouble.valueOf(0.22), DoubleDouble.valueOf(0.25),
        		                     DoubleDouble.valueOf(0.29), DoubleDouble.valueOf(0.32),
        		                     DoubleDouble.valueOf(0.35), DoubleDouble.valueOf(0.39),
        		                     DoubleDouble.valueOf(0.37), DoubleDouble.valueOf(0.58),
        		                     DoubleDouble.valueOf(0.73), DoubleDouble.valueOf(0.96),
        		                     DoubleDouble.valueOf(1.34), DoubleDouble.valueOf(2.10),
        		                     DoubleDouble.valueOf(4.39)};
        gues = new DoubleDouble[param];
        fitTestModel();
        gues[0] = DoubleDouble.valueOf(10.0);
        gues[1] = DoubleDouble.valueOf(10.0);
        gues[2] = DoubleDouble.valueOf(10.0);
        
        bounds = 2;  // bounds = 0 means unconstrained

        // bounds = 1 means same lower and upper bounds for
        // all parameters
        // bounds = 2 means different lower and upper bounds
        // for all parameters
        bl = new DoubleDouble[param];
        bu = new DoubleDouble[param];
        bl[0] = DoubleDouble.valueOf(0.0);
        bu[0] = DoubleDouble.valueOf(20.0);
        bl[1] = DoubleDouble.valueOf(0.1);
        bu[1] = DoubleDouble.valueOf(20.0);
        bl[2] = DoubleDouble.valueOf(0.0);
        bu[2] = DoubleDouble.valueOf(20.0);
        driverCalls();
        // Below is an example to fit y = a0 + x/(a1*(16 - x) + a2*min(x,16-x))
        // From Testing Unconstrained Optimization Software by More, Garbow, and Hillstrom
        // Unconstrained only worked at the standard  starting point.  This is as expected
        // because a[1] is a root in the denominator and so with unconstrained for large
        // ranges a[1] = 0 will result in an infinity.  Constrained with a[1] >= 0.1
        // works at 10.0 * starting point and at 100.0 * starting point.
        Preferences.debug("Bard function 100 * standard starting point constrained\n", Preferences.DEBUG_ALGORITHM);
        Preferences.debug("y = a0 + x/(a1*(16-x) + a2*min(x,16-x))\n", Preferences.DEBUG_ALGORITHM);
        Preferences.debug("Correct answer is a0 = 0.08241, a1 = 1.133, a2 = 2.344\n", Preferences.DEBUG_ALGORITHM);
        Preferences.debug("Correct answer has Chi-squared = 8.21487E-3\n", Preferences.DEBUG_ALGORITHM);
        testMode = true;
        testCase = BARD;
        nPts = 15;
        param = 3;
        xSeries = new DoubleDouble[15];
        for (i = 1; i <= 15; i++) {
        	xSeries[i-1] = DoubleDouble.valueOf(i);
        }
        ySeries = new DoubleDouble[]{DoubleDouble.valueOf(0.14), DoubleDouble.valueOf(0.18),
        		               DoubleDouble.valueOf(0.22), DoubleDouble.valueOf(0.25),
        		               DoubleDouble.valueOf(0.29), DoubleDouble.valueOf(0.32),
        		               DoubleDouble.valueOf(0.35), DoubleDouble.valueOf(0.39), 
        		               DoubleDouble.valueOf(0.37), DoubleDouble.valueOf(0.58),
        		               DoubleDouble.valueOf(0.73), DoubleDouble.valueOf(0.96),
        		               DoubleDouble.valueOf(1.34), DoubleDouble.valueOf(2.10),
        		               DoubleDouble.valueOf(4.39)};
        gues = new DoubleDouble[param];
        fitTestModel();
        gues[0] = DoubleDouble.valueOf(100.0);
        gues[1] = DoubleDouble.valueOf(100.0);
        gues[2] = DoubleDouble.valueOf(100.0);
        
        bounds = 2;  // bounds = 0 means unconstrained

        // bounds = 1 means same lower and upper bounds for
        // all parameters
        // bounds = 2 means different lower and upper bounds
        // for all parameters
        bl = new DoubleDouble[param];
        bu = new DoubleDouble[param];
        bl[0] = DoubleDouble.valueOf(0.0);
        bu[0] = DoubleDouble.valueOf(200.0);
        bl[1] = DoubleDouble.valueOf(0.1);
        bu[1] = DoubleDouble.valueOf(200.0);
        bl[2] = DoubleDouble.valueOf(0.0);
        bu[2] = DoubleDouble.valueOf(200.0);
        driverCalls();
        // Below is an example to fit y = a0*(x**2 + a1*x)/(x**2 + a2*x + a3)
        // From Testing Unconstrained Optimization Software by More, Garbow, and Hillstrom
        // According to the article if started at 10 * standard starting point or 
        // 100 * standard point ELSUNC running unconstrained converges towards a reported
        // unbounded local minimizer.  In fact unconstrained only worked at the standard 
        // starting point and constrained would work at 10 * standard starting point 
        // but not 100 * standard starting point.
        Preferences.debug("Kowalik and Osborne function standard starting point unconstrained\n", Preferences.DEBUG_ALGORITHM);
        Preferences.debug("y = a0*(x**2 + a1*x)/(x**2 + a2*x + a3)\n", Preferences.DEBUG_ALGORITHM);
        Preferences.debug("Correct answer is a0 = 0.1928, a1 = 0.1913, a2 = 0.1231, a3 = 0.1361\n", 
        		Preferences.DEBUG_ALGORITHM);
        Preferences.debug("Correct answer has Chi-squared = 3.07505E-4\n", Preferences.DEBUG_ALGORITHM);
        testMode = true;
        testCase = KOWALIK_AND_OSBORNE;
        nPts = 11;
        param = 4;
        xSeries = new DoubleDouble[]{DoubleDouble.valueOf(4.0), DoubleDouble.valueOf(2.0),
        		                     DoubleDouble.valueOf(1.0), DoubleDouble.valueOf(0.5),
        		                     DoubleDouble.valueOf(0.25), DoubleDouble.valueOf(0.167),
        		                     DoubleDouble.valueOf(0.125), DoubleDouble.valueOf(0.1),
        		                     DoubleDouble.valueOf(0.0833), DoubleDouble.valueOf(0.0714),
        		                     DoubleDouble.valueOf(0.0625)};
        ySeries = new DoubleDouble[]{DoubleDouble.valueOf(0.1957), DoubleDouble.valueOf(0.1947),
        		                     DoubleDouble.valueOf(0.1735), DoubleDouble.valueOf(0.1600),
        		                     DoubleDouble.valueOf(0.0844), DoubleDouble.valueOf(0.0627),
        		                     DoubleDouble.valueOf(0.0456), DoubleDouble.valueOf(0.0342), 
        		                     DoubleDouble.valueOf(0.0323), DoubleDouble.valueOf(0.0235),
        		                     DoubleDouble.valueOf(0.0246)};
        gues = new DoubleDouble[param];
        fitTestModel();
        gues[0] = DoubleDouble.valueOf(0.25);
        gues[1] = DoubleDouble.valueOf(0.39);
        gues[2] = DoubleDouble.valueOf(0.415);
        gues[3] = DoubleDouble.valueOf(0.39);
        
        bounds = 0;  // bounds = 0 means unconstrained

        // bounds = 1 means same lower and upper bounds for
        // all parameters
        // bounds = 2 means different lower and upper bounds
        // for all parameters
        bl = new DoubleDouble[param];
        bu = new DoubleDouble[param];
        driverCalls();
        // Below is an example to fit y = a0*(x**2 + a1*x)/(x**2 + a2*x + a3)
        // From Testing Unconstrained Optimization Software by More, Garbow, and Hillstrom
        // According to the article if started at 10 * standard starting point or 
        // 100 * standard point ELSUNC running unconstrained converges towards a reported
        // unbounded local minimizer.  In fact unconstrained only worked at the standard 
        // starting point and constrained would work at 10 * standard starting point 
        // but not 100 * standard starting point.
        Preferences.debug("Kowalik and Osborne function 10 * standard starting point constrained\n", 
        		Preferences.DEBUG_ALGORITHM);
        Preferences.debug("y = a0*(x**2 + a1*x)/(x**2 + a2*x + a3)\n", Preferences.DEBUG_ALGORITHM);
        Preferences.debug("Correct answer is a0 = 0.1928, a1 = 0.1913, a2 = 0.1231, a3 = 0.1361\n", 
        		Preferences.DEBUG_ALGORITHM);
        Preferences.debug("Correct answer has Chi-squared = 3.07505E-4\n", Preferences.DEBUG_ALGORITHM);
        testMode = true;
        testCase = KOWALIK_AND_OSBORNE;
        nPts = 11;
        param = 4;
        xSeries = new DoubleDouble[]{DoubleDouble.valueOf(4.0), DoubleDouble.valueOf(2.0),
        		                     DoubleDouble.valueOf(1.0), DoubleDouble.valueOf(0.5),
        		                     DoubleDouble.valueOf(0.25), DoubleDouble.valueOf(0.167),
        		                     DoubleDouble.valueOf(0.125), DoubleDouble.valueOf(0.1),
        		                     DoubleDouble.valueOf(0.0833), DoubleDouble.valueOf(0.0714),
        		                     DoubleDouble.valueOf(0.0625)};
        ySeries = new DoubleDouble[]{DoubleDouble.valueOf(0.1957), DoubleDouble.valueOf(0.1947),
        		                     DoubleDouble.valueOf(0.1735), DoubleDouble.valueOf(0.1600),
        		                     DoubleDouble.valueOf(0.0844), DoubleDouble.valueOf(0.0627),
        		                     DoubleDouble.valueOf(0.0456), DoubleDouble.valueOf(0.0342), 
        		                     DoubleDouble.valueOf(0.0323), DoubleDouble.valueOf(0.0235),
        		                     DoubleDouble.valueOf(0.0246)};
        gues = new DoubleDouble[param];
        fitTestModel();
        gues[0] = DoubleDouble.valueOf(2.5);
        gues[1] = DoubleDouble.valueOf(3.9);
        gues[2] = DoubleDouble.valueOf(4.15);
        gues[3] = DoubleDouble.valueOf(3.9);
        
        bounds = 2;  // bounds = 0 means unconstrained

        // bounds = 1 means same lower and upper bounds for
        // all parameters
        // bounds = 2 means different lower and upper bounds
        // for all parameters
        bl = new DoubleDouble[param];
        bu = new DoubleDouble[param];
        bl[0] = DoubleDouble.valueOf(0.0);
        bu[0] = DoubleDouble.valueOf(10.0);
        bl[1] = DoubleDouble.valueOf(0.0);
        bu[1] = DoubleDouble.valueOf(10.0);
        bl[2] = DoubleDouble.valueOf(0.0);
        bu[2] = DoubleDouble.valueOf(10.0);
        bl[3] = DoubleDouble.valueOf(0.0);
        bu[3] = DoubleDouble.valueOf(10.0);
        driverCalls();
        // Below is an example to fit y = a0*(x**2 + a1*x)/(x**2 + a2*x + a3)
        // From Testing Unconstrained Optimization Software by More, Garbow, and Hillstrom
        // According to the article if started at 10 * standard starting point or 
        // 100 * standard point ELSUNC running unconstrained converges towards a reported
        // unbounded local minimizer.  In fact unconstrained only worked at the standard 
        // starting point and constrained would work at 10 * standard starting point 
        // but not 100 * standard starting point.
        Preferences.debug("Kowalik and Osborne function 100 * standard starting point constrained\n", 
        		Preferences.DEBUG_ALGORITHM);
        Preferences.debug("y = a0*(x**2 + a1*x)/(x**2 + a2*x + a3)\n", Preferences.DEBUG_ALGORITHM);
        Preferences.debug("Correct answer is a0 = 0.1928, a1 = 0.1913, a2 = 0.1231, a3 = 0.1361\n", Preferences.DEBUG_ALGORITHM);
        Preferences.debug("Correct answer has Chi-squared = 3.07505E-4\n", Preferences.DEBUG_ALGORITHM);
        testMode = true;
        testCase = KOWALIK_AND_OSBORNE;
        nPts = 11;
        param = 4;
        xSeries = new DoubleDouble[]{DoubleDouble.valueOf(4.0), DoubleDouble.valueOf(2.0),
        		                     DoubleDouble.valueOf(1.0), DoubleDouble.valueOf(0.5),
        		                     DoubleDouble.valueOf(0.25), DoubleDouble.valueOf(0.167),
        		                     DoubleDouble.valueOf(0.125), DoubleDouble.valueOf(0.1),
        		                     DoubleDouble.valueOf(0.0833), DoubleDouble.valueOf(0.0714), 
        		                     DoubleDouble.valueOf(0.0625)};
        ySeries = new DoubleDouble[]{DoubleDouble.valueOf(0.1957), DoubleDouble.valueOf(0.1947),
        		                     DoubleDouble.valueOf(0.1735), DoubleDouble.valueOf(0.1600),
        		                     DoubleDouble.valueOf(0.0844), DoubleDouble.valueOf(0.0627),
        		                     DoubleDouble.valueOf(0.0456), DoubleDouble.valueOf(0.0342), 
        		                     DoubleDouble.valueOf(0.0323), DoubleDouble.valueOf(0.0235),
        		                     DoubleDouble.valueOf(0.0246)};
        gues = new DoubleDouble[param];
        fitTestModel();
        gues[0] = DoubleDouble.valueOf(25.0);
        gues[1] = DoubleDouble.valueOf(39.0);
        gues[2] = DoubleDouble.valueOf(41.5);
        gues[3] = DoubleDouble.valueOf(39.0);
        
        bounds = 2;  // bounds = 0 means unconstrained

        // bounds = 1 means same lower and upper bounds for
        // all parameters
        // bounds = 2 means different lower and upper bounds
        // for all parameters
        bl = new DoubleDouble[param];
        bu = new DoubleDouble[param];
        bl[0] = DoubleDouble.valueOf(0.0);
        bu[0] = DoubleDouble.valueOf(100.0);
        bl[1] = DoubleDouble.valueOf(0.0);
        bu[1] = DoubleDouble.valueOf(100.0);
        bl[2] = DoubleDouble.valueOf(0.0);
        bu[2] = DoubleDouble.valueOf(100.0);
        bl[3] = DoubleDouble.valueOf(0.0);
        bu[3] = DoubleDouble.valueOf(100.0);
        driverCalls();
        // Below is an example to fit y = a0*exp[a1/(x + a2)]
        // From Testing Unconstrained Optimization Software by More, Garbow, and Hillstrom
        Preferences.debug("Meyer function standard starting point unconstrained\n", Preferences.DEBUG_ALGORITHM);
        Preferences.debug("Y = a0*exp[a1/(x + a2)]\n", Preferences.DEBUG_ALGORITHM);
        Preferences.debug("Correct answers are a0 = 0.0056096, a1 = 6181.3, a2 = 345.22\n", Preferences.DEBUG_ALGORITHM);
        Preferences.debug("Correct answer has Chi-squared = 87.9458\n", Preferences.DEBUG_ALGORITHM);
        testMode = true;
        testCase = MEYER;
        nPts = 16;
        param = 3;
        xSeries = new DoubleDouble[]{DoubleDouble.valueOf(50.0), DoubleDouble.valueOf(55.0),
        		                     DoubleDouble.valueOf(60.0), DoubleDouble.valueOf(65.0),
        		                     DoubleDouble.valueOf(70.0), DoubleDouble.valueOf(75.0),
        		                     DoubleDouble.valueOf(80.0), DoubleDouble.valueOf(85.0),
        		                     DoubleDouble.valueOf(90.0), DoubleDouble.valueOf(95.0),
        		                     DoubleDouble.valueOf(100.0), DoubleDouble.valueOf(105.0), 
        		                     DoubleDouble.valueOf(110.0), DoubleDouble.valueOf(115.0),
        		                     DoubleDouble.valueOf(120.0), DoubleDouble.valueOf(125.0)};
        ySeries = new DoubleDouble[]{DoubleDouble.valueOf(34780.0), DoubleDouble.valueOf(28610.0),
        		                     DoubleDouble.valueOf(23650.0), DoubleDouble.valueOf(19630.0),
        		                     DoubleDouble.valueOf(16370.0), DoubleDouble.valueOf(13720.0),
        		                     DoubleDouble.valueOf(11540.0), DoubleDouble.valueOf(9744.0),
        		                     DoubleDouble.valueOf(8261.0), DoubleDouble.valueOf(7030.0),
        		                     DoubleDouble.valueOf(6005.0), DoubleDouble.valueOf(5147.0),
        		                     DoubleDouble.valueOf(4427.0), DoubleDouble.valueOf(3820.0),
        		                     DoubleDouble.valueOf(3307.0), DoubleDouble.valueOf(2872.0)};
        gues = new DoubleDouble[param];
        fitTestModel();
        gues[0] = DoubleDouble.valueOf(0.02);
        gues[1] = DoubleDouble.valueOf(4000.0);
        gues[2] = DoubleDouble.valueOf(250.0);
        bounds = 0;  // bounds = 0 means unconstrained

        // bounds = 1 means same lower and upper bounds for
        // all parameters
        // bounds = 2 means different lower and upper bounds
        // for all parameters
        bl = new DoubleDouble[param];
        bu = new DoubleDouble[param];
        driverCalls();
        // Below is an example to fit y = a0*exp[a1/(x + a2)]
        // From Testing Unconstrained Optimization Software by More, Garbow, and Hillstrom
        Preferences.debug("Meyer function 10 * standard starting point constrained\n", Preferences.DEBUG_ALGORITHM);
        Preferences.debug("Y = a0*exp[a1/(x + a2)]\n", Preferences.DEBUG_ALGORITHM);
        Preferences.debug("Correct answers are a0 = 0.0056096, a1 = 6181.3, a2 = 345.22\n", Preferences.DEBUG_ALGORITHM);
        Preferences.debug("Correct answer has Chi-squared = 87.9458\n", Preferences.DEBUG_ALGORITHM);
        testMode = true;
        testCase = MEYER;
        nPts = 16;
        param = 3;
        xSeries = new DoubleDouble[]{DoubleDouble.valueOf(50.0), DoubleDouble.valueOf(55.0),
        		                     DoubleDouble.valueOf(60.0), DoubleDouble.valueOf(65.0),
        		                     DoubleDouble.valueOf(70.0), DoubleDouble.valueOf(75.0),
        		                     DoubleDouble.valueOf(80.0), DoubleDouble.valueOf(85.0),
        		                     DoubleDouble.valueOf(90.0), DoubleDouble.valueOf(95.0),
        		                     DoubleDouble.valueOf(100.0), DoubleDouble.valueOf(105.0),
        		                     DoubleDouble.valueOf(110.0), DoubleDouble.valueOf(115.0),
        		                     DoubleDouble.valueOf(120.0), DoubleDouble.valueOf(125.0)};
        ySeries = new DoubleDouble[]{DoubleDouble.valueOf(34780.0), DoubleDouble.valueOf(28610.0),
        		                     DoubleDouble.valueOf(23650.0), DoubleDouble.valueOf(19630.0),
        		                     DoubleDouble.valueOf(16370.0), DoubleDouble.valueOf(13720.0),
        		                     DoubleDouble.valueOf(11540.0), DoubleDouble.valueOf(9744.0),
        		                     DoubleDouble.valueOf(8261.0), DoubleDouble.valueOf(7030.0),
        		                     DoubleDouble.valueOf(6005.0), DoubleDouble.valueOf(5147.0),
        		                     DoubleDouble.valueOf(4427.0), DoubleDouble.valueOf(3820.0),
        		                     DoubleDouble.valueOf(3307.0), DoubleDouble.valueOf(2872.0)};
        gues = new DoubleDouble[param];
        fitTestModel();
        gues[0] = DoubleDouble.valueOf(0.2);
        gues[1] = DoubleDouble.valueOf(40000.0);
        gues[2] = DoubleDouble.valueOf(2500.0);
        bounds = 2;  // bounds = 0 means unconstrained

        // bounds = 1 means same lower and upper bounds for
        // all parameters
        // bounds = 2 means different lower and upper bounds
        // for all parameters
        bl = new DoubleDouble[param];
        bu = new DoubleDouble[param];
        bl[0] = DoubleDouble.valueOf(1.0E-3);
        bu[0] = DoubleDouble.valueOf(1.0);
        bl[1] = DoubleDouble.valueOf(100.0);
        bu[1] = DoubleDouble.valueOf(100000.0);
        bl[2] = DoubleDouble.valueOf(100.0);
        bu[2] = DoubleDouble.valueOf(3000.0);                                             
        driverCalls();
        // Below is an example to fit y(i-1) = 2 + 2*i -(exp(i*a0) + exp(i*a1))
        // for i = 1 to 10
        // Don't test Jennrich and Sampson - after 2/12 days of computer time not even the first of
        // the 4 Jennrich and Sampson runs had completed.
        /*Preferences.debug("Jennrich and Sampson function at standard starting point unconstrained\n", 
           Preferences.DEBUG_ALGORITHM);
        Preferences.debug("y(i-1) = 2 + 2*i - (exp(i*a0) + exp(i*a1)\n", Preferences.DEBUG_ALGORITHM);
        Preferences.debug("for i = 1 to 10\n", Preferences.DEBUG_ALGORITHM);
        Preferences.debug("Correct answer has chi-squared = 124.362 at a0 = 0.257825, a1 = 0.257825\n", 
        Preferences.DEBUG_ALGORITHM);
        testMode = true;
        testCase = JENNRICH_AND_SAMPSON;
        nPts = 10;
        param = 2;
        gues = new DoubleDouble[param];
        fitTestModel();
        gues[0] = DoubleDouble.valueOf(0.3);
        gues[1] = DoubleDouble.valueOf(0.4);
        bounds = 0; // bounds = 0 means unconstrained
        // bounds = 1 means same lower and upper bounds for
        // all parameters
        // bounds = 2 means different lower and upper bounds
        // for all parameters
        bl = new DoubleDouble[param];
        bu = new DoubleDouble[param];
        driverCalls();*/
        // Below is an example to fit y = a0 + a1*exp(-a3*x) + a2*exp(-a4*x)
        // From Testing Unconstrained Optimization Software by More, Garbow, and Hillstrom
        Preferences.debug("Osborne 1 function unconstrained\n", Preferences.DEBUG_ALGORITHM);
        Preferences.debug("y = a0 + a1*exp(-a3*x) + a2*exp(-a4*x)\n", Preferences.DEBUG_ALGORITHM);
        Preferences.debug("Correct answer is a0 = 0.37541, a1 = 1.9358, a2 = -1.4647, a3 = 0.012868, a4 = 0.022123\n", 
        		Preferences.DEBUG_ALGORITHM);
        Preferences.debug("Correct answer has Chi-squared = 5.46489E-5\n", Preferences.DEBUG_ALGORITHM);
        testMode = true;
        testCase = OSBORNE1;
        nPts = 33;
        param = 5;
        xSeries = new DoubleDouble[33];
        for (i = 1; i <= 33; i++) {
        	xSeries[i-1] = (DoubleDouble.valueOf(10.0)).multiply(DoubleDouble.valueOf(i-1));
        }
        ySeries = new DoubleDouble[]{DoubleDouble.valueOf(0.844), DoubleDouble.valueOf(0.908),
        		                     DoubleDouble.valueOf(0.932), DoubleDouble.valueOf(0.936),
        		                     DoubleDouble.valueOf(0.925), DoubleDouble.valueOf(0.908),
        		                     DoubleDouble.valueOf(0.881), DoubleDouble.valueOf(0.850),
        		                     DoubleDouble.valueOf(0.818), DoubleDouble.valueOf(0.784),
        		                     DoubleDouble.valueOf(0.751), DoubleDouble.valueOf(0.718),
        		                     DoubleDouble.valueOf(0.685), DoubleDouble.valueOf(0.658),
        		                     DoubleDouble.valueOf(0.628), DoubleDouble.valueOf(0.603),
        		                     DoubleDouble.valueOf(0.580), DoubleDouble.valueOf(0.558),
        		                     DoubleDouble.valueOf(0.538), DoubleDouble.valueOf(0.522),
        		                     DoubleDouble.valueOf(0.506), DoubleDouble.valueOf(0.490),
        		                     DoubleDouble.valueOf(0.478), DoubleDouble.valueOf(0.467),
        		                     DoubleDouble.valueOf(0.457), DoubleDouble.valueOf(0.448),
        		                     DoubleDouble.valueOf(0.438), DoubleDouble.valueOf(0.431),
        		                     DoubleDouble.valueOf(0.424), DoubleDouble.valueOf(0.420),
        		                     DoubleDouble.valueOf(0.414), DoubleDouble.valueOf(0.411),
        		                     DoubleDouble.valueOf(0.406)};
        gues = new DoubleDouble[param];
        fitTestModel();
        gues[0] = DoubleDouble.valueOf(0.5);
        gues[1] = DoubleDouble.valueOf(1.5);
        gues[2] = DoubleDouble.valueOf(-1.0);
        gues[3] = DoubleDouble.valueOf(0.01);
        gues[4] = DoubleDouble.valueOf(0.02);
        bounds = 0;  // bounds = 0 means unconstrained

        // bounds = 1 means same lower and upper bounds for
        // all parameters
        // bounds = 2 means different lower and upper bounds
        // for all parameters
        bl = new DoubleDouble[param];
        bu = new DoubleDouble[param];
        driverCalls();
        // Below is an example to fit y = a0*exp(-a4*x) + a1*exp(-a5*(x-a8)**2) 
        // + a2*exp(-a6*(x-a9)**2) + a3*exp(-a7*(x-a10)**2)
        // From Testing Unconstrained Optimization Software by More, Garbow, and Hillstrom
        Preferences.debug("Osborne 2 function unconstrained\n", Preferences.DEBUG_ALGORITHM);
        Preferences.debug("y = a0*exp(-a4*x) + a1*exp(-a5*(x-a8)**2) \n", Preferences.DEBUG_ALGORITHM);
        Preferences.debug("    + a2*exp(-a6*(x-a9)**2) + a3*exp(-a7*(x-a10)**2)\n", Preferences.DEBUG_ALGORITHM);
        Preferences.debug("Correct answer has Chi-squared = 4.01377E-2\n", Preferences.DEBUG_ALGORITHM);
        testMode = true;
        testCase = OSBORNE2;
        nPts = 65;
        param = 11;
        xSeries = new DoubleDouble[65];
        for (i = 1; i <= 65; i++) {
        	xSeries[i-1] = (DoubleDouble.valueOf(i-1)).divide(DoubleDouble.valueOf(10.0));
        }
        ySeries = new DoubleDouble[]{DoubleDouble.valueOf(1.366), DoubleDouble.valueOf(1.191),
        		                     DoubleDouble.valueOf(1.112), DoubleDouble.valueOf(1.013),
        		                     DoubleDouble.valueOf(0.991), DoubleDouble.valueOf(0.885),
        		                     DoubleDouble.valueOf(0.831), DoubleDouble.valueOf(0.847),
        		                     DoubleDouble.valueOf(0.786), DoubleDouble.valueOf(0.725),
        		                     DoubleDouble.valueOf(0.746), DoubleDouble.valueOf(0.679),
        		                     DoubleDouble.valueOf(0.608), DoubleDouble.valueOf(0.655),
        		                     DoubleDouble.valueOf(0.616), DoubleDouble.valueOf(0.606),
        		                     DoubleDouble.valueOf(0.602), DoubleDouble.valueOf(0.626),
        		                     DoubleDouble.valueOf(0.651), DoubleDouble.valueOf(0.724),
        		                     DoubleDouble.valueOf(0.649), DoubleDouble.valueOf(0.649),
        		                     DoubleDouble.valueOf(0.694), DoubleDouble.valueOf(0.644),
        		                     DoubleDouble.valueOf(0.624), DoubleDouble.valueOf(0.661),
        		                     DoubleDouble.valueOf(0.612), DoubleDouble.valueOf(0.558),
        		                     DoubleDouble.valueOf(0.533), DoubleDouble.valueOf(0.495),
        		                     DoubleDouble.valueOf(0.500), DoubleDouble.valueOf(0.423),
        		                     DoubleDouble.valueOf(0.395), DoubleDouble.valueOf(0.375),
        		                     DoubleDouble.valueOf(0.372), DoubleDouble.valueOf(0.391),
        		                     DoubleDouble.valueOf(0.396), DoubleDouble.valueOf(0.405),
        		                     DoubleDouble.valueOf(0.428), DoubleDouble.valueOf(0.429),
        		                     DoubleDouble.valueOf(0.523), DoubleDouble.valueOf(0.562),
        		                     DoubleDouble.valueOf(0.607), DoubleDouble.valueOf(0.653),
        		                     DoubleDouble.valueOf(0.672), DoubleDouble.valueOf(0.708),
        		                     DoubleDouble.valueOf(0.633), DoubleDouble.valueOf(0.668),
        		                     DoubleDouble.valueOf(0.645), DoubleDouble.valueOf(0.632),
        		                     DoubleDouble.valueOf(0.591), DoubleDouble.valueOf(0.559),
        		                     DoubleDouble.valueOf(0.597), DoubleDouble.valueOf(0.625),
        		                     DoubleDouble.valueOf(0.739), DoubleDouble.valueOf(0.710),
        		                     DoubleDouble.valueOf(0.729), DoubleDouble.valueOf(0.720),
        		                     DoubleDouble.valueOf(0.636), DoubleDouble.valueOf(0.581),
        		                     DoubleDouble.valueOf(0.428), DoubleDouble.valueOf(0.292),
        		                     DoubleDouble.valueOf(0.162), DoubleDouble.valueOf(0.098),
        		                     DoubleDouble.valueOf(0.054)};
        gues = new DoubleDouble[param];
        fitTestModel();
        gues[0] = DoubleDouble.valueOf(1.3);
        gues[1] = DoubleDouble.valueOf(0.65);
        gues[2] = DoubleDouble.valueOf(0.65);
        gues[3] = DoubleDouble.valueOf(0.7);
        gues[4] = DoubleDouble.valueOf(0.6);
        gues[5] = DoubleDouble.valueOf(3.0);
        gues[6] = DoubleDouble.valueOf(5.0);
        gues[7] = DoubleDouble.valueOf(7.0);
        gues[8] = DoubleDouble.valueOf(2.0);
        gues[9] = DoubleDouble.valueOf(4.5);
        gues[10] = DoubleDouble.valueOf(5.5);
        bounds = 0;  // bounds = 0 means unconstrained

        // bounds = 1 means same lower and upper bounds for
        // all parameters
        // bounds = 2 means different lower and upper bounds
        // for all parameters
        bl = new DoubleDouble[param];
        bu = new DoubleDouble[param];
        driverCalls();
        
        // Below is an example to fit the Watson function with 12 parameters
        // From Testing Unconstrained Optimization Software by More, Garbow, and Hillstrom
        Preferences.debug("Watson with 12 parameters at standard starting point unconstrained\n", Preferences.DEBUG_ALGORITHM);
        Preferences.debug("Correct chi-squared = 4.72238E-10\n", Preferences.DEBUG_ALGORITHM);
        testMode = true;
        testCase = WATSON;
        nPts = 31;
        param = 12;
        // Guess all parameters are 0.0.
        gues = new DoubleDouble[param];
        for (i = 0; i < param; i++) {
        	gues[i] = DoubleDouble.valueOf(0.0);
        }
        fitTestModel();
        bounds = 0; // bounds = 0 means unconstrained
        // bounds = 1 means same lower and upper bounds for
        // all parameters
        // bounds = 2 means different lower and upper bounds
        // for all parameters
        bl = new DoubleDouble[param];
        bu = new DoubleDouble[param];
        driverCalls();
    }
    
    
    
    /**
     * NLConstrainedEngine - non-linear fit to a function.
     *
     * @param  nPts     number of points to fit to the function to
     * @param  param    number of parameters of function
     */
    public NLConstrainedEngineEP(int nPts, int param) {
    	int i,j;

        try {
            this.nPts = nPts;
            this.param = param;

            maxIterations = 2000 * param;
            bl = new DoubleDouble[param];
            bu = new DoubleDouble[param];
            residuals = new DoubleDouble[nPts];
            pivit = new int[param];
            aset = new int[param];
            dx = new DoubleDouble[param];
            g = new DoubleDouble[param];
            w0 = new DoubleDouble[param];
            w1 = new DoubleDouble[nPts];
            w2 = new DoubleDouble[nPts];
            w3 = new DoubleDouble[param];
            v = new DoubleDouble[nPts];
            gmat = new DoubleDouble[param][param];

            // compute srelpr single relative precision
            releps();
            rootsp = (srelpr).sqrt();

            mdg = param;
            tolerance = rootsp;
            relativeConvergence = (DoubleDouble.valueOf(10.0)).multiply(rootsp);
            absoluteConvergence = srelpr;
            parameterConvergence = (DoubleDouble.valueOf(10.0)).multiply(rootsp);

            a = new DoubleDouble[param];
            gues = new DoubleDouble[param];

            int covar2 = Math.max(4, param);
            covarMat = new DoubleDouble[nPts][covar2];
            
            for (i = 0; i < param; i++) {
            	dx[i] = DoubleDouble.valueOf(0.0);
            }
            
            for (i = 0; i < nPts; i++) {
            	for (j = 0; j < covar2; j++) {
            		covarMat[i][j] = DoubleDouble.valueOf(0.0);
            	}
            }
        } catch (OutOfMemoryError error) { }
    }

    //~ Methods --------------------------------------------------------------------------------------------------------

    /*if (fullTest) {
    	FitAll fa = new FitAll();
    	return;
    }*/
    
    /*class FitAll extends NLConstrainedEngine {

       
        public FitAll() {

            // nPoints data points, 3 coefficients, and exponential fitting
            super();

            
        }

        
        public void driver() {
            super.driver();
        }

        
        public void dumpResults() {
            
        }

       
        public void fitToFunction(double[] a, double[] residuals, double[][] covarMat) {
            

            return;
        }
    }*/

        /**
         * Creates a new Fit24DModel object.
         *
         * @param  nPoints  DOCUMENT ME!
         * @param  xData    DOCUMENT ME!
         * @param  yData    DOCUMENT ME!
         * @param  initial  DOCUMENT ME!
         */
        private void fitTestModel() {
        	int i,j;

            // nPoints data points, 3 coefficients, and exponential fitting
        	try {
                

                maxIterations = 2000 * param;
                residuals = new DoubleDouble[nPts];
                pivit = new int[param];
                aset = new int[param];
                dx = new DoubleDouble[param];
                g = new DoubleDouble[param];
                w0 = new DoubleDouble[param];
                w1 = new DoubleDouble[nPts];
                w2 = new DoubleDouble[nPts];
                w3 = new DoubleDouble[param];
                v = new DoubleDouble[nPts];
                gmat = new DoubleDouble[param][param];

                // compute srelpr single relative precision
                releps();
                rootsp = (srelpr).sqrt();

                mdg = param;
                tolerance = rootsp;
                relativeConvergence = (DoubleDouble.valueOf(10.0)).multiply(rootsp);
                absoluteConvergence = srelpr;
                parameterConvergence = DoubleDouble.valueOf(10.0).multiply(rootsp);

                a = new DoubleDouble[param];

                int covar2 = Math.max(4, param);
                covarMat = new DoubleDouble[nPts][covar2];
                
                for (i = 0; i < param; i++) {
                	dx[i] = DoubleDouble.valueOf(0.0);
                }
                
                for (i = 0; i < nPts; i++) {
                	for (j = 0; j < covar2; j++) {
                		covarMat[i][j] = DoubleDouble.valueOf(0.0);
                	}
                }
            } catch (OutOfMemoryError error) { }

            
        }
    
        
        public void fitToTestFunction(DoubleDouble[] a, DoubleDouble[] residuals, DoubleDouble[][] covarMat) {
            int ctrl;
            int i;
            DoubleDouble logx;
        	DoubleDouble a0logx;
        	DoubleDouble a1m1;
        	DoubleDouble temp;
        	DoubleDouble temp2;
        	DoubleDouble sixteenmx;
        	DoubleDouble d1;
        	DoubleDouble d2;
        	DoubleDouble denom;
        	DoubleDouble denomSqr;
        	DoubleDouble xSqr;
        	DoubleDouble a1x;
        	DoubleDouble a2x;
        	DoubleDouble a3x;
        	DoubleDouble a4x;
        	DoubleDouble a5x;
        	DoubleDouble a6x;
        	DoubleDouble a7x;
        	DoubleDouble a8x;
        	DoubleDouble a9x;
        	DoubleDouble a10x;
        	DoubleDouble num;
        	DoubleDouble top;
        	DoubleDouble exponent;
            DoubleDouble ymodel = DoubleDouble.valueOf(0.0);

            try {
                ctrl = ctrlMat[0];
                switch (testCase) {
                case DRAPER24D:
	                if ((ctrl == -1) || (ctrl == 1)) {
	
	                    // evaluate the residuals[i] = ymodel[i] - ySeries[i]
	                    for (i = 0; i < nPts; i++) {
	                    	ymodel = a[2].pow(xSeries[i]);
	                    	ymodel = a[1].multiply(ymodel);
	                    	ymodel = a[0].subtract(ymodel);
	                        //ymodel = a[0] - (a[1] * Math.pow(a[2], xSeries[i]));
	                        residuals[i] = ymodel.subtract(ySeries[i]);
	                    }
	                } // if ((ctrl == -1) || (ctrl == 1))
	                else if (ctrl == 2) {
	                    if (analyticalJacobian) {
		                    // Calculate the Jacobian analytically
		                    for (i = 0; i < nPts; i++) {
		                        covarMat[i][0] = DoubleDouble.valueOf(1.0);
		                        covarMat[i][1] = (a[2].pow(xSeries[i])).negate();
		                        //covarMat[i][1] = -Math.pow(a[2], xSeries[i]);
		                        if (i == 0) {
		                        	covarMat[i][2] = DoubleDouble.valueOf(0.0);
		                        }
		                        else {
			                        covarMat[i][2] = xSeries[i].subtract(DoubleDouble.valueOf(1.0));
			                        covarMat[i][2] = a[2].pow(covarMat[i][2]);
			                        covarMat[i][2] = a[1].multiply(covarMat[i][2]);
			                        covarMat[i][2] = (xSeries[i].multiply(covarMat[i][2])).negate();
			                        //covarMat[i][2] = -xSeries[i] * a[1] * Math.pow(a[2], xSeries[i] - 1.0);
		                        }
		                    }
	                    }
	                    else {
	                    	// If the user wishes to calculate the Jacobian numerically
	                         ctrlMat[0] = 0; 	
	                    }
	                } // else if (ctrl == 2)
	                break;
                case HOCK25:
                	if ((ctrl == -1) || (ctrl == 1)) {

                        // evaluate the residuals[i] = ymodel[i] - ySeries[i]
                        for (i = 0; i < nPts; i++) {
                        	ymodel = a[0].multiply(xSeries[i].log());
                        	ymodel = ymodel.pow(a[1]);
                        	ymodel = ymodel.add(a[2]);
                            //ymodel = Math.pow((a[0] * Math.log(xSeries[i])),a[1]) + a[2];
                            residuals[i] = ymodel.subtract(ySeries[i]);
                        }
                    } // if ((ctrl == -1) || (ctrl == 1))
                    else if (ctrl == 2) {
                        if (analyticalJacobian) {
	                        // Calculate the Jacobian analytically
	                        for (i = 0; i < nPts; i++) {
	                        	logx = xSeries[i].log();
	                        	a0logx = a[0].multiply(logx);
	                        	a1m1 = a[1].subtract(DoubleDouble.valueOf(1.0));
	                        	covarMat[i][0] = a0logx.pow(a1m1);
	                        	covarMat[i][0] = a[1].multiply(covarMat[i][0]);
	                        	covarMat[i][0] = covarMat[i][0].multiply(logx);
	                            //covarMat[i][0] = a[1]*Math.pow((a[0] * Math.log(xSeries[i])),a[1]-1.0) * Math.log(xSeries[i]);
	                        	covarMat[i][1] = a0logx.log();
	                        	temp = a0logx.pow(a[1]);
	                        	covarMat[i][1] = covarMat[i][1].multiply(temp); 
	                            //covarMat[i][1] = Math.log(a[0] * Math.log(xSeries[i])) * Math.pow((a[0] * Math.log(xSeries[i])),a[1]);
	                            covarMat[i][2] = DoubleDouble.valueOf(1.0);
	                        }
                        }
                        else {
	                    	// If the user wishes to calculate the Jacobian numerically
	                         ctrlMat[0] = 0; 	
	                    }
	                } // else if (ctrl == 2)
                	break;
                case BARD:
                	if ((ctrl == -1) || (ctrl == 1)) {

                        // evaluate the residuals[i] = ymodel[i] - ySeries[i]
                        for (i = 0; i < nPts; i++) {
                        	sixteenmx = (DoubleDouble.valueOf(16.0)).subtract(xSeries[i]);
                        	d1 = a[1].multiply(sixteenmx);
                        	d2 = xSeries[i].min(sixteenmx);
                        	d2 = a[2].multiply(d2);
                        	denom = d1.add(d2);
                        	ymodel = xSeries[i].divide(denom);
                        	ymodel = ymodel.add(a[0]);
                            //ymodel = a[0] + xSeries[i]/(a[1]*(16.0 - xSeries[i]) 
                            		 //+ a[2]*Math.min(xSeries[i], 16.0 - xSeries[i]));
                            residuals[i] = ymodel.subtract(ySeries[i]);
                        }
                    } // if ((ctrl == -1) || (ctrl == 1))
                    else if (ctrl == 2) {
                        if (analyticalJacobian) {
	                        // Calculate the Jacobian analytically
	                        for (i = 0; i < nPts; i++) {
	                        	sixteenmx = (DoubleDouble.valueOf(16.0)).subtract(xSeries[i]);
	                        	d1 = a[1].multiply(sixteenmx);
	                        	d2 = xSeries[i].min(sixteenmx);
	                        	d2 = a[2].multiply(d2);
	                        	denom = d1.add(d2);
	                        	denomSqr = denom.multiply(denom);
	                        	//denom = (a[1]*(16.0 - xSeries[i]) + a[2]*Math.min(xSeries[i], 16.0 - xSeries[i]));
	                            covarMat[i][0] = DoubleDouble.valueOf(1.0);
	                            covarMat[i][1] = xSeries[i].negate();
	                            covarMat[i][1] = covarMat[i][1].multiply(sixteenmx);
	                            covarMat[i][1] = covarMat[i][1].divide(denomSqr);
	                            //covarMat[i][1] = -xSeries[i]*(16.0 - xSeries[i])/(denom*denom);
	                            covarMat[i][2] = xSeries[i].min(sixteenmx);
	                            covarMat[i][2] = xSeries[i].multiply(covarMat[i][2]);
	                            covarMat[i][2] = covarMat[i][2].negate();
	                            covarMat[i][2] = covarMat[i][2].divide(denomSqr);
	                            //covarMat[i][2] = -xSeries[i]*Math.min(xSeries[i], 16.0 - xSeries[i])/(denom*denom);
	                        }
                        }
                        else {
	                    	// If the user wishes to calculate the Jacobian numerically
	                         ctrlMat[0] = 0; 	
	                    }
	                } // else if (ctrl == 2)
                	break;
                case KOWALIK_AND_OSBORNE:
                	if ((ctrl == -1) || (ctrl == 1)) {

                        // evaluate the residuals[i] = ymodel[i] - ySeries[i]
                        for (i = 0; i < nPts; i++) {
                        	xSqr = xSeries[i].multiply(xSeries[i]);
                        	a1x = a[1].multiply(xSeries[i]);
                        	a2x = a[2].multiply(xSeries[i]);
                        	num = xSqr.add(a1x);
                        	num = a[0].multiply(num);
                        	denom = xSqr.add(a2x);
                        	denom = denom.add(a[3]);
                        	ymodel = num.divide(denom);
                            //ymodel = a[0]*(xSeries[i]*xSeries[i] + a[1]*xSeries[i])/
                                    //(xSeries[i]*xSeries[i] + a[2]*xSeries[i] + a[3]);
                            residuals[i] = ymodel.subtract(ySeries[i]);
                        }
                    } // if ((ctrl == -1) || (ctrl == 1))
                    else if (ctrl == 2) {
                        if (analyticalJacobian) {
	                        // Calculate the Jacobian analytically
	                        for (i = 0; i < nPts; i++) {
	                        	xSqr = xSeries[i].multiply(xSeries[i]);
	                        	a1x = a[1].multiply(xSeries[i]);
	                        	a2x = a[2].multiply(xSeries[i]);
	                        	denom = xSqr.add(a2x);
	                        	denom = denom.add(a[3]);
	                        	//denom = (xSeries[i]*xSeries[i] + a[2]*xSeries[i] + a[3]);
	                        	top = xSqr.add(a1x);
	                        	//top = (xSeries[i]*xSeries[i] + a[1]*xSeries[i]);
	                            covarMat[i][0] = top.divide(denom);
	                            covarMat[i][1] = a[0].multiply(xSeries[i]);
	                            covarMat[i][1] = covarMat[i][1].divide(denom);
	                            //covarMat[i][1] = a[0]*xSeries[i]/denom;
	                            denomSqr = denom.multiply(denom);
	                            covarMat[i][3] = a[0].negate();
	                            covarMat[i][3] = covarMat[i][3].multiply(top);
	                            covarMat[i][3] = covarMat[i][3].divide(denomSqr);
	                            covarMat[i][2] = covarMat[i][3].multiply(xSeries[i]);
	                            //covarMat[i][2] = -a[0]*xSeries[i]*top/(denom*denom);
	                            //covarMat[i][3] = -a[0]*top/(denom*denom);
	                        }
                        }
                        else {
	                    	// If the user wishes to calculate the Jacobian numerically
	                         ctrlMat[0] = 0; 	
	                    }
	                } // else if (ctrl == 2)
                	break;
                    case JENNRICH_AND_SAMPSON:
                	if ((ctrl == -1) || (ctrl == 1)) {
                		DoubleDouble exp1;
                		DoubleDouble exp2;
                	     for (i = 0; i < 10; i++) {
                	    	 residuals[i] = DoubleDouble.valueOf(2.0 + 2.0*(i + 1.0));
                	    	 exp1 = a[0].multiply(DoubleDouble.valueOf(i+1.0));
                	    	 exp1 = exp1.exp();
                	    	 exp2 = a[1].multiply(DoubleDouble.valueOf(i+1.0));
                	    	 exp2 = exp2.exp();
                	    	 residuals[i] = residuals[i].subtract(exp1);
                	    	 residuals[i] = residuals[i].subtract(exp2);
                	    	 //residuals[i] = 2.0 + 2.0*(i+1.0) - (Math.exp((i+1.0)*a[0]) + Math.exp((i+1.0)*a[1]));
                	     }
                	} // if ((ctrl == -1) || (ctrl == 1))
                	else if (ctrl == 2) {
                	    if (analyticalJacobian) {
                	    	DoubleDouble exp1;
                	    	DoubleDouble exp2;
                	        for (i = 0; i < 10; i++) {
                	        	exp1 = a[0].multiply(DoubleDouble.valueOf(i+1.0));
                   	    	    exp1 = exp1.exp();
                   	    	    exp2 = a[1].multiply(DoubleDouble.valueOf(i+1.0));
                   	    	    exp2 = exp2.exp();
                   	    	    covarMat[i][0] = exp1.multiply(DoubleDouble.valueOf(i+1.0));
                   	    	    covarMat[i][0] = covarMat[i][0].negate();
                   	    	    covarMat[i][1] = exp2.multiply(DoubleDouble.valueOf(i+1.0));
                   	    	    covarMat[i][1] = covarMat[i][1].negate();
                	    	    //covarMat[i][0] = -Math.exp((i+1.0)*a[0])*(i + 1.0);
                	    	    //covarMat[i][1] = -Math.exp((i+1.0)*a[1])*(i + 1.0);
                	        }
                	    }
                	    else {
                	    	// If the user wishes to calculate the Jacobian numerically
                	    	ctrlMat[0] = 0;
                	    }
                	} // else if (ctrl == 2)
                	break;
                case MEYER:
                	if ((ctrl == -1) || (ctrl == 1)) {

                        // evaluate the residuals[i] = ymodel[i] - ySeries[i]
                        for (i = 0; i < nPts; i++) {
                        	ymodel = xSeries[i].add(a[2]);
                        	ymodel = a[1].divide(ymodel);
                        	ymodel = ymodel.exp();
                        	ymodel = a[0].multiply(ymodel);
                            //ymodel = a[0]*Math.exp(a[1]/(xSeries[i] + a[2]));
                            residuals[i] = ymodel.subtract(ySeries[i]);
                        }
                    } // if ((ctrl == -1) || (ctrl == 1))
                    else if (ctrl == 2) {
                        if (analyticalJacobian) {
	                        // Calculate the Jacobian analytically
	                        for (i = 0; i < nPts; i++) {
	                        	a2x = xSeries[i].add(a[2]);
	                        	exponent = a[1].divide(a2x);
	                        	exponent = exponent.exp();
	                        	//exponent = Math.exp(a[1]/(xSeries[i] + a[2]));
	                            covarMat[i][0] = exponent;
	                            covarMat[i][1] = a[0].divide(a2x);
	                            covarMat[i][1] = covarMat[i][1].multiply(exponent);
	                            //covarMat[i][1] = (a[0]/(xSeries[i] + a[2]))* exponent;
	                            covarMat[i][2] = covarMat[i][1].negate();
	                            covarMat[i][2] = covarMat[i][2].multiply(a[1]);
	                            covarMat[i][2] = covarMat[i][2].divide(a2x);
	                            //covarMat[i][2] = -(a[0]*a[1]/((xSeries[i] + a[2])*(xSeries[i] + a[2]))) * exponent;
	                        }
                        }
                        else {
	                    	// If the user wishes to calculate the Jacobian numerically
	                         ctrlMat[0] = 0; 	
	                    }
	                } // else if (ctrl == 2)
                	break;
                case OSBORNE1:
                	if ((ctrl == -1) || (ctrl == 1)) {

                        // evaluate the residuals[i] = ymodel[i] - ySeries[i]
                        for (i = 0; i < nPts; i++) {
                        	a3x = a[3].negate();
                        	a3x = a3x.multiply(xSeries[i]);
                        	a3x = a3x.exp();
                        	a3x = a[1].multiply(a3x);
                        	a4x = a[4].negate();
                        	a4x = a4x.multiply(xSeries[i]);
                        	a4x = a4x.exp();
                        	a4x = a[2].multiply(a4x);
                        	ymodel = a[0].add(a3x);
                        	ymodel = ymodel.add(a4x);
                            //ymodel = a[0] + a[1]*Math.exp(-a[3]*xSeries[i]) + a[2]*Math.exp(-a[4]*xSeries[i]);
                            residuals[i] = ymodel.subtract(ySeries[i]);
                        }
                    } // if ((ctrl == -1) || (ctrl == 1))
                    else if (ctrl == 2) {
                        if (analyticalJacobian) {
	                        // Calculate the Jacobian analytically
	                        for (i = 0; i < nPts; i++) {
	                            covarMat[i][0] = DoubleDouble.valueOf(1.0);
	                            covarMat[i][1] = a[3].negate();
	                            covarMat[i][1] = covarMat[i][1].multiply(xSeries[i]);
	                            covarMat[i][1] = covarMat[i][1].exp();
	                            //covarMat[i][1] = Math.exp(-a[3]*xSeries[i]);
	                            covarMat[i][2] = a[4].negate();
	                            covarMat[i][2] = covarMat[i][2].multiply(xSeries[i]);
	                            covarMat[i][2] = covarMat[i][2].exp();
	                            //covarMat[i][2] = Math.exp(-a[4]*xSeries[i]);
	                            covarMat[i][3] = a[1].negate();
	                            covarMat[i][3] = covarMat[i][3].multiply(xSeries[i]);
	                            covarMat[i][3] = covarMat[i][3].multiply(covarMat[i][1]);
	                            //covarMat[i][3] = -a[1]*xSeries[i]*Math.exp(-a[3]*xSeries[i]);
	                            covarMat[i][4] = a[2].negate();
	                            covarMat[i][4] = covarMat[i][4].multiply(xSeries[i]);
	                            covarMat[i][4] = covarMat[i][4].multiply(covarMat[i][2]);
	                            //covarMat[i][4] = -a[2]*xSeries[i]*Math.exp(-a[4]*xSeries[i]);
	                        }
                        }
                        else {
	                    	// If the user wishes to calculate the Jacobian numerically
	                         ctrlMat[0] = 0; 	
	                    }
	                } // else if (ctrl == 2)
                	break;
                case OSBORNE2:
                	if ((ctrl == -1) || (ctrl == 1)) {

                        // evaluate the residuals[i] = ymodel[i] - ySeries[i]
                        for (i = 0; i < nPts; i++) {
                        	a4x = a[4].negate();
                        	a4x = a4x.multiply(xSeries[i]);
                        	a4x = a4x.exp();
                        	a4x = a[0].multiply(a4x);
                        	a5x = xSeries[i].subtract(a[8]);
                        	a5x = a5x.multiply(a5x);
                        	a5x = a5x.multiply(a[5]);
                        	a5x = a5x.negate();
                        	a5x = a5x.exp();
                        	a5x = a[1].multiply(a5x);
                        	a6x = xSeries[i].subtract(a[9]);
                        	a6x = a6x.multiply(a6x);
                        	a6x = a6x.multiply(a[6]);
                        	a6x = a6x.negate();
                        	a6x = a6x.exp();
                        	a6x = a[2].multiply(a6x);
                        	a7x = xSeries[i].subtract(a[10]);
                        	a7x = a7x.multiply(a7x);
                        	a7x = a7x.multiply(a[7]);
                        	a7x = a7x.negate();
                        	a7x = a7x.exp();
                        	a7x = a[3].multiply(a7x);
                        	ymodel = a4x.add(a5x);
                        	ymodel = ymodel.add(a6x);
                        	ymodel = ymodel.add(a7x);
                            //ymodel = a[0]*Math.exp(-a[4]*xSeries[i]) 
                                   //+ a[1]*Math.exp(-a[5]*(xSeries[i] - a[8])*(xSeries[i] - a[8]))
                                   //+ a[2]*Math.exp(-a[6]*(xSeries[i] - a[9])*(xSeries[i] - a[9]))
                                   //+ a[3]*Math.exp(-a[7]*(xSeries[i] - a[10])*(xSeries[i] - a[10]));
                            residuals[i] = ymodel.subtract(ySeries[i]);
                        }
                    } // if ((ctrl == -1) || (ctrl == 1))
                    else if (ctrl == 2) {
                        if (analyticalJacobian) {
	                        // Calculate the Jacobian analytically
	                        for (i = 0; i < nPts; i++) {
	                        	a4x = a[4].negate();
	                        	a4x = a4x.multiply(xSeries[i]);
	                        	a4x = a4x.exp();
	                        	covarMat[i][0] = a4x;
	                            //covarMat[i][0] = Math.exp(-a[4]*xSeries[i]);
	                        	a8x = xSeries[i].subtract(a[8]);
	                        	a5x = a8x.multiply(a8x);
	                        	a5x = a5x.multiply(a[5]);
	                        	a5x = a5x.negate();
	                        	a5x = a5x.exp();
	                        	covarMat[i][1] = a5x;
	                            //covarMat[i][1] = Math.exp(-a[5]*(xSeries[i] - a[8])*(xSeries[i] - a[8]));
	                        	a9x = xSeries[i].subtract(a[9]);
	                        	a6x = a9x.multiply(a9x);
	                        	a6x = a6x.multiply(a[6]);
	                        	a6x = a6x.negate();
	                        	a6x = a6x.exp();
	                        	covarMat[i][2] = a6x;
	                            //covarMat[i][2] = Math.exp(-a[6]*(xSeries[i] - a[9])*(xSeries[i] - a[9]));
	                        	a10x = xSeries[i].subtract(a[10]);
	                        	a7x = a10x.multiply(a10x);
	                        	a7x = a7x.multiply(a[7]);
	                        	a7x = a7x.negate();
	                        	a7x = a7x.exp();
	                        	covarMat[i][3] = a7x;
	                            //covarMat[i][3] = Math.exp(-a[7]*(xSeries[i] - a[10])*(xSeries[i] - a[10]));
	                        	covarMat[i][4] = a[0].negate();
	                        	covarMat[i][4] = covarMat[i][4].multiply(xSeries[i]);
	                        	covarMat[i][4] = covarMat[i][4].multiply(a4x);
	                            //covarMat[i][4] = -a[0]*xSeries[i]*Math.exp(-a[4]*xSeries[i]) ;
	                        	covarMat[i][5] = a[1].negate();
	                        	covarMat[i][5] = covarMat[i][5].multiply(a8x);
	                        	covarMat[i][5] = covarMat[i][5].multiply(a8x);
	                        	covarMat[i][5] = covarMat[i][5].multiply(a5x);
	                            //covarMat[i][5] = -a[1]*(xSeries[i] - a[8])*(xSeries[i] - a[8])
	                                             //*Math.exp(-a[5]*(xSeries[i] - a[8])*(xSeries[i] - a[8]));
	                        	covarMat[i][6] = a[2].negate();
	                        	covarMat[i][6] = covarMat[i][6].multiply(a9x);
	                        	covarMat[i][6] = covarMat[i][6].multiply(a9x);
	                        	covarMat[i][6] = covarMat[i][6].multiply(a6x);
	                            //covarMat[i][6] = -a[2]*(xSeries[i] - a[9])*(xSeries[i] - a[9])
	                                             //*Math.exp(-a[6]*(xSeries[i] - a[9])*(xSeries[i] - a[9]));
	                        	covarMat[i][7] = a[3].negate();
	                        	covarMat[i][7] = covarMat[i][7].multiply(a10x);
	                        	covarMat[i][7] = covarMat[i][7].multiply(a10x);
	                        	covarMat[i][7] = covarMat[i][7].multiply(a7x);
	                            //covarMat[i][7] = -a[3]*(xSeries[i] - a[10])*(xSeries[i] - a[10])
	                                             //*Math.exp(-a[7]*(xSeries[i] - a[10])*(xSeries[i] - a[10]));
	                        	covarMat[i][8] = (DoubleDouble.valueOf(2.0)).multiply(a[1]);
	                        	covarMat[i][8] = covarMat[i][8].multiply(a[5]);
	                        	covarMat[i][8] = covarMat[i][8].multiply(a8x);
	                        	covarMat[i][8] = covarMat[i][8].multiply(a5x);
	                            //covarMat[i][8] = 2.0*a[1]*a[5]*(xSeries[i] - a[8])
	                                             //*Math.exp(-a[5]*(xSeries[i] - a[8])*(xSeries[i] - a[8]));
	                        	covarMat[i][9] = (DoubleDouble.valueOf(2.0)).multiply(a[2]);
	                        	covarMat[i][9] = covarMat[i][9].multiply(a[6]);
	                        	covarMat[i][9] = covarMat[i][9].multiply(a9x);
	                        	covarMat[i][9] = covarMat[i][9].multiply(a6x);
	                            //covarMat[i][9] = 2.0*a[2]*a[6]*(xSeries[i] - a[9])
	                                             //*Math.exp(-a[6]*(xSeries[i] - a[9])*(xSeries[i] - a[9]));
	                        	covarMat[i][10] = (DoubleDouble.valueOf(2.0)).multiply(a[3]);
	                        	covarMat[i][10] = covarMat[i][10].multiply(a[7]);
	                        	covarMat[i][10] = covarMat[i][10].multiply(a10x);
	                        	covarMat[i][10] = covarMat[i][10].multiply(a7x);
	                            //covarMat[i][10] = 2.0*a[3]*a[7]*(xSeries[i] - a[10])
	                                              //*Math.exp(-a[7]*(xSeries[i] - a[10])*(xSeries[i] - a[10]));
	                        }
                        }
                        else {
	                    	// If the user wishes to calculate the Jacobian numerically
	                         ctrlMat[0] = 0; 	
	                    }
	                } // else if (ctrl == 2)
                	break;
                case WATSON:
                	if ((ctrl == -1 ) || (ctrl == 1)) {
                		DoubleDouble sum1;
                		DoubleDouble sum2;
                		DoubleDouble t[] = new DoubleDouble[29];
                		int j;
                	    for (i = 0; i < 31; i++) {
                	    	if (i < 29) {
                	            //t[i] = (i+1.0)/29.0;
                	    		temp = (DoubleDouble.valueOf(i)).add(DoubleDouble.valueOf(1.0));
                	    		t[i] = temp.divide(DoubleDouble.valueOf(29.0));
	                	        sum1 = DoubleDouble.valueOf(0.0);
	                	        for (j = 2; j <= param; j++) {
	                	            //sum1 += (j - 1.0)*a[j-1]*Math.pow(t[i], j-2.0);
	                	        	temp = (DoubleDouble.valueOf(j)).subtract(DoubleDouble.valueOf(1.0));
	                	        	temp = temp.multiply(a[j-1]);
	                	        	temp2 = (DoubleDouble.valueOf(j)).subtract(DoubleDouble.valueOf(2.0));
	                	        	temp2 = t[i].pow(temp2);
	                	        	temp = temp.multiply(temp2);
	                	        	sum1 = sum1.add(temp);
	                	        }
	                	        sum2 = DoubleDouble.valueOf(0.0);
	                	        for (j = 1; j <= param; j++) {
	                	            //sum2 += a[j-1]*Math.pow(t[i], j-1.0);	
	                	        	temp = DoubleDouble.valueOf(j).subtract(DoubleDouble.valueOf(1.0));
	                	        	temp = t[i].pow(temp);
	                	        	temp = a[j-1].multiply(temp);
	                	        	sum2 = sum2.add(temp);
	                	        }
                	        	//residuals[i] = sum1 - sum2*sum2 - 1.0;
	                	        temp = sum2.multiply(sum2);
	                	        residuals[i] = sum1.subtract(temp);
	                	        residuals[i] = residuals[i].subtract(DoubleDouble.valueOf(1.0));
                	        }
                	        else if (i == 29) {
                	        	residuals[i] = a[0];
                	        }
                	        else if (i == 30) {
                	        	//residuals[i] = a[1] - a[0]*a[0] - 1.0; 
                	        	temp = a[0].multiply(a[0]);
                	        	residuals[i] = a[1].subtract(temp);
                	        	residuals[i] = residuals[i].subtract(DoubleDouble.valueOf(1.0));
                	        }
                	    }
                	} // if ((ctrl == -1) || (ctrl == 1))
                	else if (ctrl == 2) {
                		if (analyticalJacobian) {
                    		DoubleDouble sum2;
                    		DoubleDouble t[] = new DoubleDouble[29];
                    		int j;
                    	    for (i = 0; i < 31; i++) {
                    	    	if (i < 29) {
                    	            //t[i] = (i+1.0)/29.0;
                    	            temp = (DoubleDouble.valueOf(i)).add(DoubleDouble.valueOf(1.0));
                    	    		t[i] = temp.divide(DoubleDouble.valueOf(29.0));
                    	            sum2 = DoubleDouble.valueOf(0.0);
    	                	        for (j = 1; j <= param; j++) {
    	                	            //sum2 += a[j-1]*Math.pow(t[i], j-1.0);
    	                	            temp = DoubleDouble.valueOf(j).subtract(DoubleDouble.valueOf(1.0));
    	                	        	temp = t[i].pow(temp);
    	                	        	temp = a[j-1].multiply(temp);
    	                	        	sum2 = sum2.add(temp);
    	                	        }
                    	            covarMat[i][0] = DoubleDouble.valueOf(-2.0).multiply(sum2);
                    	            for (j = 2; j <= param; j++) {
                    	            	//covarMat[i][j-1] = (j-1.0)*Math.pow(t[i],j-2.0) - 2.0*sum2*Math.pow(t[i],j-1.0);
                    	            	temp = DoubleDouble.valueOf(j).subtract(DoubleDouble.valueOf(2.0));
                    	            	temp = t[i].pow(temp);
                    	            	temp2 = (DoubleDouble.valueOf(j)).subtract(DoubleDouble.valueOf(1.0));
                    	            	temp = temp2.multiply(temp);
                    	            	temp2 = t[i].pow(temp2);
                    	            	temp2 = sum2.multiply(temp2);
                    	            	temp2 = (DoubleDouble.valueOf(2.0)).multiply(temp2);
                    	            	covarMat[i][j-1] = temp.subtract(temp2);
                    	            }
                    	    	} // if (i < 29)
                    	    	else if (i == 29) {
                    	    		covarMat[i][0] = DoubleDouble.valueOf(1.0);
                    	    		for (j = 1; j < param; j++) {
                    	    			covarMat[i][j] = DoubleDouble.valueOf(0.0);
                    	    		}
                    	    	}
                    	    	else if (i == 30) {
                    	    		covarMat[i][0] = DoubleDouble.valueOf(-2.0).multiply(a[0]);
                    	    		covarMat[i][1] = DoubleDouble.valueOf(1.0);
                    	    		for (j = 2; j < param; j++) {
                    	    			covarMat[i][j] = DoubleDouble.valueOf(0.0);
                    	    		}
                    	    	}
                    	    } // for (i = 0; i < 31; i++)
                		} // if (analyticalJacobian)
                		else {
                			// If the user wishes to calculate the Jacobian numerically
                			ctrlMat[0] = 0;
                		}
                	} // else if (ctrl == 2)
                    break;
                } // switch (testCase)
            } catch (Exception e) {
                Preferences.debug("function error: " + e.getMessage() + "\n", Preferences.DEBUG_ALGORITHM);
            }

            return;
        }
        
        private void dumpTestResults() {
        	int i;
        	if (analyticalJacobian) {
        	    Preferences.debug("Internal scaling = " + internalScaling + " Analytical Jacobian used\n", 
        	    		Preferences.DEBUG_ALGORITHM);
        	}
        	else {
        		Preferences.debug("Internal scaling = " + internalScaling + " Numerical Jacobian used\n", 
        				Preferences.DEBUG_ALGORITHM);
        	}
            Preferences.debug("Number of iterations: " + String.valueOf(iters) + "\n", Preferences.DEBUG_ALGORITHM);
            Preferences.debug("Chi-squared: " + String.valueOf(getChiSquared()) + "\n", Preferences.DEBUG_ALGORITHM);
            for (i = 0; i < param; i++) {
                Preferences.debug("a" + i + " = " + String.valueOf(a[i]) + "\n", Preferences.DEBUG_ALGORITHM);
            }
            if (exitStatus == 12340) { 	
            	Preferences.debug("Normal termination because the relative predicted reduction in the objective function is less than epsrel**2 and\n", 
            			Preferences.DEBUG_ALGORITHM);
            	Preferences.debug("because the sum of squares is less than epsabs**2 and\n", Preferences.DEBUG_ALGORITHM);
            	Preferences.debug("because the relative change in x is less than epsx and\n", Preferences.DEBUG_ALGORITHM);
                Preferences.debug("because we are computing at noise level\n", Preferences.DEBUG_ALGORITHM);
                Preferences.debug("The last steps were computed with no trouble (Gauss-Newton the last 3 steps)\n", 
                		Preferences.DEBUG_ALGORITHM);
            }
            else if (exitStatus == 12341) {
            	Preferences.debug("Normal termination because the relative predicted reduction in the objective function is less than epsrel**2 and\n", 
            			Preferences.DEBUG_ALGORITHM);
            	Preferences.debug("because the sum of squares is less than epsabs**2 and\n", Preferences.DEBUG_ALGORITHM);
            	Preferences.debug("because the relative change in x is less than epsx and\n", Preferences.DEBUG_ALGORITHM);
                Preferences.debug("because we are computing at noise level\n", Preferences.DEBUG_ALGORITHM);
                Preferences.debug("The last steps were computed with prank <> n at the termination point\n", 
                		Preferences.DEBUG_ALGORITHM);	
            }
            else if (exitStatus == 12342) {
            	Preferences.debug("Normal termination because the relative predicted reduction in the objective function is less than epsrel**2 and\n", 
            			Preferences.DEBUG_ALGORITHM);
            	Preferences.debug("because the sum of squares is less than epsabs**2 and\n", Preferences.DEBUG_ALGORITHM);
            	Preferences.debug("because the relative change in x is less than epsx and\n", Preferences.DEBUG_ALGORITHM);
                Preferences.debug("because we are computing at noise level\n", Preferences.DEBUG_ALGORITHM);
                Preferences.debug("The method of Newton was used (at least) in the last step\n", Preferences.DEBUG_ALGORITHM);	
            }
            else if (exitStatus == 12343) {
            	Preferences.debug("Normal termination because the relative predicted reduction in the objective function is less than epsrel**2 and\n", 
            			Preferences.DEBUG_ALGORITHM);
            	Preferences.debug("because the sum of squares is less than epsabs**2 and\n", Preferences.DEBUG_ALGORITHM);
            	Preferences.debug("because the relative change in x is less than epsx and\n", Preferences.DEBUG_ALGORITHM);
            	Preferences.debug("because we are computing at noise level\n", Preferences.DEBUG_ALGORITHM);
            	Preferences.debug("The 2nd but last step was subspace minimization but\n", Preferences.DEBUG_ALGORITHM);
                Preferences.debug("the last two were Gauss-Newton steps \n", Preferences.DEBUG_ALGORITHM);
            }
            else if (exitStatus == 12344) {
            	Preferences.debug("Normal termination because the relative predicted reduction in the objective function is less than epsrel**2 and\n", 
            			Preferences.DEBUG_ALGORITHM);
            	Preferences.debug("because the sum of squares is less than epsabs**2 and\n", Preferences.DEBUG_ALGORITHM);
            	Preferences.debug("because the relative change in x is less than epsx and\n", Preferences.DEBUG_ALGORITHM);
                Preferences.debug("because we are computing at noise level\n", Preferences.DEBUG_ALGORITHM);
                Preferences.debug("The steplength was not unit in both the last two steps\n", Preferences.DEBUG_ALGORITHM);	
            }
            else if (exitStatus == 12300) {
            	Preferences.debug("Normal termination because the relative predicted reduction in the objective function is less than epsrel**2 and\n", 
            			Preferences.DEBUG_ALGORITHM);
            	Preferences.debug("because the sum of squares is less than epsabs**2 and\n", Preferences.DEBUG_ALGORITHM);
            	Preferences.debug("because the relative change in x is less than epsx\n", Preferences.DEBUG_ALGORITHM);
            	Preferences.debug("The last steps were computed with no trouble (Gauss-Newton the last 3 steps)\n", 
            			Preferences.DEBUG_ALGORITHM);
            }
            else if (exitStatus == 12301) {
            	Preferences.debug("Normal termination because the relative predicted reduction in the objective function is less than epsrel**2 and\n", 
            			Preferences.DEBUG_ALGORITHM);
            	Preferences.debug("because the sum of squares is less than epsabs**2 and\n", Preferences.DEBUG_ALGORITHM);
            	Preferences.debug("because the relative change in x is less than epsx\n", Preferences.DEBUG_ALGORITHM);
            	Preferences.debug("The last steps were computed with prank <> n at the termination point\n", 
            			Preferences.DEBUG_ALGORITHM);	
            }
            else if (exitStatus == 12302) {
            	Preferences.debug("Normal termination because the relative predicted reduction in the objective function is less than epsrel**2 and\n", 
            			Preferences.DEBUG_ALGORITHM);
            	Preferences.debug("because the sum of squares is less than epsabs**2 and\n", Preferences.DEBUG_ALGORITHM);
            	Preferences.debug("because the relative change in x is less than epsx\n", Preferences.DEBUG_ALGORITHM);
            	Preferences.debug("The method of Newton was used (at least) in the last step\n", Preferences.DEBUG_ALGORITHM);	
            }
            else if (exitStatus == 12303) {
            	Preferences.debug("Normal termination because the relative predicted reduction in the objective function is less than epsrel**2 and\n", 
            			Preferences.DEBUG_ALGORITHM);
            	Preferences.debug("because the sum of squares is less than epsabs**2 and\n", Preferences.DEBUG_ALGORITHM);
            	Preferences.debug("because the relative change in x is less than epsx\n", Preferences.DEBUG_ALGORITHM);
            	Preferences.debug("The 2nd but last step was subspace minimization but\n", Preferences.DEBUG_ALGORITHM);
                Preferences.debug("the last two were Gauss-Newton steps \n", Preferences.DEBUG_ALGORITHM);
            }
            else if (exitStatus == 12304) {
            	Preferences.debug("Normal termination because the relative predicted reduction in the objective function is less than epsrel**2 and\n", 
            			Preferences.DEBUG_ALGORITHM);
            	Preferences.debug("because the sum of squares is less than epsabs**2 and\n", Preferences.DEBUG_ALGORITHM);
            	Preferences.debug("because the relative change in x is less than epsx\n", Preferences.DEBUG_ALGORITHM);	
            	Preferences.debug("The steplength was not unit in both the last two steps\n", Preferences.DEBUG_ALGORITHM);	
            }
            else if (exitStatus == 12040) {
            	Preferences.debug("Normal termination because the relative predicted reduction in the objective function is less than epsrel**2 and\n", 
            			Preferences.DEBUG_ALGORITHM);
            	Preferences.debug("because the sum of squares is less than epsabs**2 and\n", Preferences.DEBUG_ALGORITHM);
                Preferences.debug("because we are computing at noise level\n", Preferences.DEBUG_ALGORITHM);
                Preferences.debug("The last steps were computed with no trouble (Gauss-Newton the last 3 steps)\n", 
                		Preferences.DEBUG_ALGORITHM);	
            }
            else if (exitStatus == 12041) {
            	Preferences.debug("Normal termination because the relative predicted reduction in the objective function is less than epsrel**2 and\n", 
            			Preferences.DEBUG_ALGORITHM);
            	Preferences.debug("because the sum of squares is less than epsabs**2 and\n", Preferences.DEBUG_ALGORITHM);
                Preferences.debug("because we are computing at noise level\n", Preferences.DEBUG_ALGORITHM);
                Preferences.debug("The last steps were computed with prank <> n at the termination point\n", 
                		Preferences.DEBUG_ALGORITHM);	
            }
            else if (exitStatus == 12042) {
            	Preferences.debug("Normal termination because the relative predicted reduction in the objective function is less than epsrel**2 and\n", 
            			Preferences.DEBUG_ALGORITHM);
            	Preferences.debug("because the sum of squares is less than epsabs**2 and\n", Preferences.DEBUG_ALGORITHM);
                Preferences.debug("because we are computing at noise level\n", Preferences.DEBUG_ALGORITHM);	
                Preferences.debug("The method of Newton was used (at least) in the last step\n", Preferences.DEBUG_ALGORITHM);
            }
            else if (exitStatus == 12043) {
            	Preferences.debug("Normal termination because the relative predicted reduction in the objective function is less than epsrel**2 and\n", 
            			Preferences.DEBUG_ALGORITHM);
            	Preferences.debug("because the sum of squares is less than epsabs**2 and\n", Preferences.DEBUG_ALGORITHM);
                Preferences.debug("because we are computing at noise level\n", Preferences.DEBUG_ALGORITHM);	
                Preferences.debug("The 2nd but last step was subspace minimization but\n", Preferences.DEBUG_ALGORITHM);
                Preferences.debug("the last two were Gauss-Newton steps \n", Preferences.DEBUG_ALGORITHM);
            }
            else if (exitStatus == 12044) {
            	Preferences.debug("Normal termination because the relative predicted reduction in the objective function is less than epsrel**2 and\n", 
            			Preferences.DEBUG_ALGORITHM);
            	Preferences.debug("because the sum of squares is less than epsabs**2 and\n", Preferences.DEBUG_ALGORITHM);
                Preferences.debug("because we are computing at noise level\n", Preferences.DEBUG_ALGORITHM);
                Preferences.debug("The steplength was not unit in both the last two steps\n", Preferences.DEBUG_ALGORITHM);	
            }
            else if (exitStatus == 12000) {
            	Preferences.debug("Normal termination because the relative predicted reduction in the objective function is less than epsrel**2 and\n", 
            			Preferences.DEBUG_ALGORITHM);
            	Preferences.debug("because the sum of squares is less than epsabs**2\n", Preferences.DEBUG_ALGORITHM);
            	Preferences.debug("The last steps were computed with no trouble (Gauss-Newton the last 3 steps)\n", 
            			Preferences.DEBUG_ALGORITHM);
            }
            else if (exitStatus == 12001) {
            	Preferences.debug("Normal termination because the relative predicted reduction in the objective function is less than epsrel**2 and\n", 
            			Preferences.DEBUG_ALGORITHM);
            	Preferences.debug("because the sum of squares is less than epsabs**2\n", Preferences.DEBUG_ALGORITHM);
            	Preferences.debug("The last steps were computed with prank <> n at the termination point\n", 
            			Preferences.DEBUG_ALGORITHM);
            }
            else if (exitStatus == 12002) {
            	Preferences.debug("Normal termination because the relative predicted reduction in the objective function is less than epsrel**2 and\n", 
            			Preferences.DEBUG_ALGORITHM);
            	Preferences.debug("because the sum of squares is less than epsabs**2\n", Preferences.DEBUG_ALGORITHM);
            	Preferences.debug("The method of Newton was used (at least) in the last step\n", Preferences.DEBUG_ALGORITHM);
            }
            else if (exitStatus == 12003) {
            	Preferences.debug("Normal termination because the relative predicted reduction in the objective function is less than epsrel**2 and\n", 
            			Preferences.DEBUG_ALGORITHM);
            	Preferences.debug("because the sum of squares is less than epsabs**2\n", Preferences.DEBUG_ALGORITHM);
            	Preferences.debug("The 2nd but last step was subspace minimization but\n", Preferences.DEBUG_ALGORITHM);
                Preferences.debug("the last two were Gauss-Newton steps \n", Preferences.DEBUG_ALGORITHM);
            }
            else if (exitStatus == 12004) {
            	Preferences.debug("Normal termination because the relative predicted reduction in the objective function is less than epsrel**2 and\n", 
            			Preferences.DEBUG_ALGORITHM);
            	Preferences.debug("because the sum of squares is less than epsabs**2\n", Preferences.DEBUG_ALGORITHM);
            	Preferences.debug("The steplength was not unit in both the last two steps\n", Preferences.DEBUG_ALGORITHM);	
            }
            else if (exitStatus == 10340) {
            	Preferences.debug("Normal termination because the relative predicted reduction in the objective function is less than epsrel**2 and\n", 
            			Preferences.DEBUG_ALGORITHM);
            	Preferences.debug("because the relative change in x is less than epsx and\n", Preferences.DEBUG_ALGORITHM);
                Preferences.debug("because we are computing at noise level\n", Preferences.DEBUG_ALGORITHM);
                Preferences.debug("The last steps were computed with no trouble (Gauss-Newton the last 3 steps)\n", 
                		Preferences.DEBUG_ALGORITHM);
            }
            else if (exitStatus == 10341) {
            	Preferences.debug("Normal termination because the relative predicted reduction in the objective function is less than epsrel**2 and\n", 
            			Preferences.DEBUG_ALGORITHM);
            	Preferences.debug("because the relative change in x is less than epsx and\n", Preferences.DEBUG_ALGORITHM);
                Preferences.debug("because we are computing at noise level\n", Preferences.DEBUG_ALGORITHM);	
                Preferences.debug("The last steps were computed with prank <> n at the termination point\n", 
                		Preferences.DEBUG_ALGORITHM);
            }
            else if (exitStatus == 10342) {
            	Preferences.debug("Normal termination because the relative predicted reduction in the objective function is less than epsrel**2 and\n", 
            			Preferences.DEBUG_ALGORITHM);
            	Preferences.debug("because the relative change in x is less than epsx and\n", Preferences.DEBUG_ALGORITHM);
                Preferences.debug("because we are computing at noise level\n", Preferences.DEBUG_ALGORITHM);	
                Preferences.debug("The method of Newton was used (at least) in the last step\n", Preferences.DEBUG_ALGORITHM);
            }
            else if (exitStatus == 10343) {
            	Preferences.debug("Normal termination because the relative predicted reduction in the objective function is less than epsrel**2 and\n", 
            			Preferences.DEBUG_ALGORITHM);
            	Preferences.debug("because the relative change in x is less than epsx and\n", Preferences.DEBUG_ALGORITHM);
            	Preferences.debug("because we are computing at noise level\n", Preferences.DEBUG_ALGORITHM);
            	Preferences.debug("The 2nd but last step was subspace minimization but\n", Preferences.DEBUG_ALGORITHM);
                Preferences.debug("the last two were Gauss-Newton steps \n", Preferences.DEBUG_ALGORITHM);
            } 
            else if (exitStatus == 10344) {
            	Preferences.debug("Normal termination because the relative predicted reduction in the objective function is less than epsrel**2 and\n", 
            			Preferences.DEBUG_ALGORITHM);
            	Preferences.debug("because the relative change in x is less than epsx and\n", Preferences.DEBUG_ALGORITHM);
                Preferences.debug("because we are computing at noise level\n", Preferences.DEBUG_ALGORITHM);	
                Preferences.debug("The steplength was not unit in both the last two steps\n", Preferences.DEBUG_ALGORITHM);
            }
            else if (exitStatus == 10300) {
            	Preferences.debug("Normal termination because the relative predicted reduction in the objective function is less than epsrel**2 and\n", 
            			Preferences.DEBUG_ALGORITHM);
            	Preferences.debug("because the relative change in x is less than epsx\n", Preferences.DEBUG_ALGORITHM);
            	Preferences.debug("The last steps were computed with no trouble (Gauss-Newton the last 3 steps)\n", 
            			Preferences.DEBUG_ALGORITHM);
            }
            else if (exitStatus == 10301) {
            	Preferences.debug("Normal termination because the relative predicted reduction in the objective function is less than epsrel**2 and\n", 
            			Preferences.DEBUG_ALGORITHM);
            	Preferences.debug("because the relative change in x is less than epsx\n", Preferences.DEBUG_ALGORITHM);
            	Preferences.debug("The last steps were computed with prank <> n at the termination point\n", 
            			Preferences.DEBUG_ALGORITHM);
            }
            else if (exitStatus == 10302) {
            	Preferences.debug("Normal termination because the relative predicted reduction in the objective function is less than epsrel**2 and\n", 
            			Preferences.DEBUG_ALGORITHM);
            	Preferences.debug("because the relative change in x is less than epsx\n", Preferences.DEBUG_ALGORITHM);
            	Preferences.debug("The method of Newton was used (at least) in the last step\n", Preferences.DEBUG_ALGORITHM);
            }
            else if (exitStatus == 10303) {
            	Preferences.debug("Normal termination because the relative predicted reduction in the objective function is less than epsrel**2 and\n", 
            			Preferences.DEBUG_ALGORITHM);
            	Preferences.debug("because the relative change in x is less than epsx\n", Preferences.DEBUG_ALGORITHM);
            	Preferences.debug("The 2nd but last step was subspace minimization but\n", Preferences.DEBUG_ALGORITHM);
                Preferences.debug("the last two were Gauss-Newton steps \n", Preferences.DEBUG_ALGORITHM);
            }
            else if (exitStatus == 10304) {
            	Preferences.debug("Normal termination because the relative predicted reduction in the objective function is less than epsrel**2 and\n", 
            			Preferences.DEBUG_ALGORITHM);
            	Preferences.debug("because the relative change in x is less than epsx\n", Preferences.DEBUG_ALGORITHM);
            	Preferences.debug("The steplength was not unit in both the last two steps\n", Preferences.DEBUG_ALGORITHM);
            }
            else if (exitStatus == 10040) {
            	Preferences.debug("Normal termination because the relative predicted reduction in the objective function is less than epsrel**2 and\n", 
            			Preferences.DEBUG_ALGORITHM);
                Preferences.debug("because we are computing at noise level\n", Preferences.DEBUG_ALGORITHM);
                Preferences.debug("The last steps were computed with no trouble (Gauss-Newton the last 3 steps)\n", 
                		Preferences.DEBUG_ALGORITHM);
            } 
            else if (exitStatus == 10041) {
            	Preferences.debug("Normal termination because the relative predicted reduction in the objective function is less than epsrel**2 and\n", 
            			Preferences.DEBUG_ALGORITHM);
                Preferences.debug("because we are computing at noise level\n", Preferences.DEBUG_ALGORITHM);	
                Preferences.debug("The last steps were computed with prank <> n at the termination point\n", 
                		Preferences.DEBUG_ALGORITHM);
            }
            else if (exitStatus == 10042) {
            	Preferences.debug("Normal termination because the relative predicted reduction in the objective function is less than epsrel**2 and\n", 
            			Preferences.DEBUG_ALGORITHM);
                Preferences.debug("because we are computing at noise level\n", Preferences.DEBUG_ALGORITHM);	
                Preferences.debug("The method of Newton was used (at least) in the last step\n", Preferences.DEBUG_ALGORITHM);
            }
            else if (exitStatus == 10043) {
            	Preferences.debug("Normal termination because the relative predicted reduction in the objective function is less than epsrel**2 and\n", 
            			Preferences.DEBUG_ALGORITHM);
            	Preferences.debug("because we are computing at noise level\n", Preferences.DEBUG_ALGORITHM);
            	Preferences.debug("The 2nd but last step was subspace minimization but\n", Preferences.DEBUG_ALGORITHM);
                Preferences.debug("the last two were Gauss-Newton steps \n", Preferences.DEBUG_ALGORITHM);
            }  
            else if (exitStatus == 10044) {
            	Preferences.debug("Normal termination because the relative predicted reduction in the objective function is less than epsrel**2 and\n", 
            			Preferences.DEBUG_ALGORITHM);
                Preferences.debug("because we are computing at noise level\n", Preferences.DEBUG_ALGORITHM);	
                Preferences.debug("The steplength was not unit in both the last two steps\n", Preferences.DEBUG_ALGORITHM);
            }
            else if (exitStatus == 10000) {
            	Preferences.debug("Normal termination because the relative predicted reduction in the objective function is less than epsrel**2\n", 
            			Preferences.DEBUG_ALGORITHM);
            	Preferences.debug("The last steps were computed with no trouble (Gauss-Newton the last 3 steps)\n", 
            			Preferences.DEBUG_ALGORITHM);
            }
            else if (exitStatus == 10001) {
            	Preferences.debug("Normal termination because the relative predicted reduction in the objective function is less than epsrel**2\n", 
            			Preferences.DEBUG_ALGORITHM);
            	Preferences.debug("The last steps were computed with prank <> n at the termination point\n", 
            			Preferences.DEBUG_ALGORITHM);
            }
            else if (exitStatus == 10002) {
            	Preferences.debug("Normal termination because the relative predicted reduction in the objective function is less than epsrel**2\n", 
            			Preferences.DEBUG_ALGORITHM);
            	Preferences.debug("The method of Newton was used (at least) in the last step\n", Preferences.DEBUG_ALGORITHM);
            }
            else if (exitStatus == 10003) {
            	Preferences.debug("Normal termination because the relative predicted reduction in the objective function is less than epsrel**2\n", 
            			Preferences.DEBUG_ALGORITHM);
            	Preferences.debug("The 2nd but last step was subspace minimization but\n", Preferences.DEBUG_ALGORITHM);
                Preferences.debug("the last two were Gauss-Newton steps \n", Preferences.DEBUG_ALGORITHM);
            }
            else if (exitStatus == 10004) {
            	Preferences.debug("Normal termination because the relative predicted reduction in the objective function is less than epsrel**2\n", 
            			Preferences.DEBUG_ALGORITHM);
            	Preferences.debug("The steplength was not unit in both the last two steps\n", Preferences.DEBUG_ALGORITHM);
            }
            else if (exitStatus == 2340) {
            	Preferences.debug("Normal termination because the sum of squares is less than epsabs**2 and\n", 
            			Preferences.DEBUG_ALGORITHM);
            	Preferences.debug("because the relative change in x is less than epsx and\n", Preferences.DEBUG_ALGORITHM);
                Preferences.debug("because we are computing at noise level\n", Preferences.DEBUG_ALGORITHM);
                Preferences.debug("The last steps were computed with no trouble (Gauss-Newton the last 3 steps)\n", 
                		Preferences.DEBUG_ALGORITHM);
            }
            else if (exitStatus == 2341) {
            	Preferences.debug("Normal termination because the sum of squares is less than epsabs**2 and\n", 
            			Preferences.DEBUG_ALGORITHM);
            	Preferences.debug("because the relative change in x is less than epsx and\n", Preferences.DEBUG_ALGORITHM);
                Preferences.debug("because we are computing at noise level\n", Preferences.DEBUG_ALGORITHM);	
                Preferences.debug("The last steps were computed with prank <> n at the termination point\n", 
                		Preferences.DEBUG_ALGORITHM);
            }  
            else if (exitStatus == 2342) {
            	Preferences.debug("Normal termination because the sum of squares is less than epsabs**2 and\n", 
            			Preferences.DEBUG_ALGORITHM);
            	Preferences.debug("because the relative change in x is less than epsx and\n", Preferences.DEBUG_ALGORITHM);
                Preferences.debug("because we are computing at noise level\n", Preferences.DEBUG_ALGORITHM);	
                Preferences.debug("The method of Newton was used (at least) in the last step\n", Preferences.DEBUG_ALGORITHM);
            }  
            else if (exitStatus == 2343) {
            	Preferences.debug("Normal termination because the sum of squares is less than epsabs**2 and\n", 
            			Preferences.DEBUG_ALGORITHM);
            	Preferences.debug("because the relative change in x is less than epsx and\n", Preferences.DEBUG_ALGORITHM);
            	Preferences.debug("because we are computing at noise level\n", Preferences.DEBUG_ALGORITHM);
            	Preferences.debug("The 2nd but last step was subspace minimization but\n", Preferences.DEBUG_ALGORITHM);
                Preferences.debug("the last two were Gauss-Newton steps \n", Preferences.DEBUG_ALGORITHM);
            }
            else if (exitStatus == 2344) {
            	Preferences.debug("Normal termination because the sum of squares is less than epsabs**2 and\n", 
            			Preferences.DEBUG_ALGORITHM);
            	Preferences.debug("because the relative change in x is less than epsx and\n", Preferences.DEBUG_ALGORITHM);
                Preferences.debug("because we are computing at noise level\n", Preferences.DEBUG_ALGORITHM);	
                Preferences.debug("The steplength was not unit in both the last two steps\n", Preferences.DEBUG_ALGORITHM);
            } 
            else if (exitStatus == 2300) {
            	Preferences.debug("Normal termination because the sum of squares is less than epsabs**2 and\n", 
            			Preferences.DEBUG_ALGORITHM);
            	Preferences.debug("because the relative change in x is less than epsx\n", Preferences.DEBUG_ALGORITHM);	
            	Preferences.debug("The last steps were computed with no trouble (Gauss-Newton the last 3 steps)\n", 
            			Preferences.DEBUG_ALGORITHM);
            }
            else if (exitStatus == 2301) {
            	Preferences.debug("Normal termination because the sum of squares is less than epsabs**2 and\n", 
            			Preferences.DEBUG_ALGORITHM);
            	Preferences.debug("because the relative change in x is less than epsx\n", Preferences.DEBUG_ALGORITHM);
            	Preferences.debug("The last steps were computed with prank <> n at the termination point\n", 
            			Preferences.DEBUG_ALGORITHM);
            }
            else if (exitStatus == 2302) {
            	Preferences.debug("Normal termination because the sum of squares is less than epsabs**2 and\n", 
            			Preferences.DEBUG_ALGORITHM);
            	Preferences.debug("because the relative change in x is less than epsx\n", Preferences.DEBUG_ALGORITHM);
            	Preferences.debug("The method of Newton was used (at least) in the last step\n", Preferences.DEBUG_ALGORITHM);
            }
            else if (exitStatus == 2303) {
            	Preferences.debug("Normal termination because the sum of squares is less than epsabs**2 and\n", 
            			Preferences.DEBUG_ALGORITHM);
            	Preferences.debug("because the relative change in x is less than epsx\n", Preferences.DEBUG_ALGORITHM);
            	Preferences.debug("The 2nd but last step was subspace minimization but\n", Preferences.DEBUG_ALGORITHM);
                Preferences.debug("the last two were Gauss-Newton steps \n", Preferences.DEBUG_ALGORITHM);
            }
            else if (exitStatus == 2304) {
            	Preferences.debug("Normal termination because the sum of squares is less than epsabs**2 and\n", 
            			Preferences.DEBUG_ALGORITHM);
            	Preferences.debug("because the relative change in x is less than epsx\n", Preferences.DEBUG_ALGORITHM);	
            	Preferences.debug("The steplength was not unit in both the last two steps\n", Preferences.DEBUG_ALGORITHM);
            }
            else if (exitStatus == 2040) {
            	Preferences.debug("Normal termination because the sum of squares is less than epsabs**2 and\n", 
            			Preferences.DEBUG_ALGORITHM);
                Preferences.debug("because we are computing at noise level\n", Preferences.DEBUG_ALGORITHM);
                Preferences.debug("The last steps were computed with no trouble (Gauss-Newton the last 3 steps)\n", 
                		Preferences.DEBUG_ALGORITHM);
            }   
            else if (exitStatus == 2041) {
            	Preferences.debug("Normal termination because the sum of squares is less than epsabs**2  and\n", 
            			Preferences.DEBUG_ALGORITHM);
                Preferences.debug("because we are computing at noise level\n", Preferences.DEBUG_ALGORITHM);
                Preferences.debug("The last steps were computed with prank <> n at the termination point\n", 
                		Preferences.DEBUG_ALGORITHM);
            } 
            else if (exitStatus == 2042) {
            	Preferences.debug("Normal termination because the sum of squares is less than epsabs**2 and\n", 
            			Preferences.DEBUG_ALGORITHM);
                Preferences.debug("because we are computing at noise level\n", Preferences.DEBUG_ALGORITHM);	
                Preferences.debug("The method of Newton was used (at least) in the last step\n", Preferences.DEBUG_ALGORITHM);
            }
            else if (exitStatus == 2043) {
            	Preferences.debug("Normal termination because the sum of squares is less than epsabs**2 and\n", 
            			Preferences.DEBUG_ALGORITHM);
            	Preferences.debug("because we are computing at noise level\n", Preferences.DEBUG_ALGORITHM);
            	Preferences.debug("The 2nd but last step was subspace minimization but\n", Preferences.DEBUG_ALGORITHM);
                Preferences.debug("the last two were Gauss-Newton steps \n", Preferences.DEBUG_ALGORITHM);
            }
            else if (exitStatus == 2044) {
            	Preferences.debug("Normal termination because the sum of squares is less than epsabs**2 and\n", 
            			Preferences.DEBUG_ALGORITHM);
                Preferences.debug("because we are computing at noise level\n", Preferences.DEBUG_ALGORITHM);
                Preferences.debug("The steplength was not unit in both the last two steps\n", Preferences.DEBUG_ALGORITHM);
            }
            else if (exitStatus == 2000) {
            	Preferences.debug("Normal termination because the sum of squares is less than epsabs**2\n", 
            			Preferences.DEBUG_ALGORITHM);
            	Preferences.debug("The last steps were computed with no trouble (Gauss-Newton the last 3 steps)\n", 
            			Preferences.DEBUG_ALGORITHM);
            } 
            else if (exitStatus == 2001) {
            	Preferences.debug("Normal termination because the sum of squares is less than epsabs**2\n", 
            			Preferences.DEBUG_ALGORITHM);
            	Preferences.debug("The last steps were computed with prank <> n at the termination point\n", 
            			Preferences.DEBUG_ALGORITHM);
            }
            else if (exitStatus == 2002) {
            	Preferences.debug("Normal termination because the sum of squares is less than epsabs**2\n", 
            			Preferences.DEBUG_ALGORITHM);
            	Preferences.debug("The method of Newton was used (at least) in the last step\n", Preferences.DEBUG_ALGORITHM);
            }
            else if (exitStatus == 2003) {
            	Preferences.debug("Normal termination because the sum of squares is less than epsabs**2\n", 
            			Preferences.DEBUG_ALGORITHM);	
            	Preferences.debug("The 2nd but last step was subspace minimization but\n", Preferences.DEBUG_ALGORITHM);
                Preferences.debug("the last two were Gauss-Newton steps \n", Preferences.DEBUG_ALGORITHM);
            }
            else if (exitStatus == 2004) {
            	Preferences.debug("Normal termination because the sum of squares is less than epsabs**2\n", 
            			Preferences.DEBUG_ALGORITHM);
            	Preferences.debug("The steplength was not unit in both the last two steps\n", Preferences.DEBUG_ALGORITHM);
            }
            else if (exitStatus == 340) {
            	Preferences.debug("Normal termination because the relative change in x is less than epsx and\n", 
            			Preferences.DEBUG_ALGORITHM);
                Preferences.debug("because we are computing at noise level\n", Preferences.DEBUG_ALGORITHM);
                Preferences.debug("The last steps were computed with no trouble (Gauss-Newton the last 3 steps)\n", 
                		Preferences.DEBUG_ALGORITHM);
            }  
            else if (exitStatus == 341) {
            	Preferences.debug("Normal termination because the relative change in x is less than epsx and\n", 
            			Preferences.DEBUG_ALGORITHM);
                Preferences.debug("because we are computing at noise level\n", Preferences.DEBUG_ALGORITHM);	
                Preferences.debug("The last steps were computed with prank <> n at the termination point\n", 
                		Preferences.DEBUG_ALGORITHM);
            }
            else if (exitStatus == 342) {
            	Preferences.debug("Normal termination because the relative change in x is less than epsx and\n", 
            			Preferences.DEBUG_ALGORITHM);
                Preferences.debug("because we are computing at noise level\n", Preferences.DEBUG_ALGORITHM);
                Preferences.debug("The method of Newton was used (at least) in the last step\n", Preferences.DEBUG_ALGORITHM);
            }
            else if (exitStatus == 343) {
            	Preferences.debug("Normal termination because the relative change in x is less than epsx and\n", 
            			Preferences.DEBUG_ALGORITHM);
                Preferences.debug("because we are computing at noise level\n", Preferences.DEBUG_ALGORITHM);
                Preferences.debug("The 2nd but last step was subspace minimization but\n", Preferences.DEBUG_ALGORITHM);
                Preferences.debug("the last two were Gauss-Newton steps \n", Preferences.DEBUG_ALGORITHM);  
            }
            else if (exitStatus == 344) {
            	Preferences.debug("Normal termination because the relative change in x is less than epsx and\n", 
            			Preferences.DEBUG_ALGORITHM);
                Preferences.debug("because we are computing at noise level\n", Preferences.DEBUG_ALGORITHM);
                Preferences.debug("The steplength was not unit in both the last two steps\n", Preferences.DEBUG_ALGORITHM);
            }
            else if (exitStatus == 300) {
            	Preferences.debug("Normal termination because the relative change in x is less than epsx\n", 
            			Preferences.DEBUG_ALGORITHM);
            	Preferences.debug("The last steps were computed with no trouble (Gauss-Newton the last 3 steps)\n", 
            			Preferences.DEBUG_ALGORITHM);
            } 
            else if (exitStatus == 301) {
            	Preferences.debug("Normal termination because the relative change in x is less than epsx\n", 
            			Preferences.DEBUG_ALGORITHM);
            	Preferences.debug("The last steps were computed with prank <> n at the termination point\n", 
            			Preferences.DEBUG_ALGORITHM);
            }
            else if (exitStatus == 302) {
            	Preferences.debug("Normal termination because the relative change in x is less than epsx\n", 
            			Preferences.DEBUG_ALGORITHM);
            	Preferences.debug("The method of Newton was used (at least) in the last step\n", Preferences.DEBUG_ALGORITHM);
            }
            else if (exitStatus == 303) {
            	Preferences.debug("Normal termination because the relative change in x is less than epsx\n", 
            			Preferences.DEBUG_ALGORITHM);	
            	Preferences.debug("The 2nd but last step was subspace minimization but\n", Preferences.DEBUG_ALGORITHM);
                Preferences.debug("the last two were Gauss-Newton steps \n", Preferences.DEBUG_ALGORITHM);  
            }
            else if (exitStatus == 304) {
            	Preferences.debug("Normal termination because the relative change in x is less than epsx\n", 
            			Preferences.DEBUG_ALGORITHM);
                Preferences.debug("The steplength was not unit in both the last two steps\n", Preferences.DEBUG_ALGORITHM);
            }
            else if (exitStatus == 40) {
            	Preferences.debug("Normal termination because we are computing at noise level\n", Preferences.DEBUG_ALGORITHM);
            	Preferences.debug("The last steps were computed with no trouble (Gauss-Newton the last 3 steps)\n", 
            			Preferences.DEBUG_ALGORITHM);
            } 
            else if (exitStatus == 41) {
            	Preferences.debug("Normal termination because we are computing at noise level\n", Preferences.DEBUG_ALGORITHM);
            	Preferences.debug("The last steps were computed with prank <> n at the termination point\n", 
            			Preferences.DEBUG_ALGORITHM);
            }
            else if (exitStatus == 42) {
            	Preferences.debug("Normal termination because we are computing at noise level\n", Preferences.DEBUG_ALGORITHM);
            	Preferences.debug("The method of Newton was used (at least) in the last step\n", Preferences.DEBUG_ALGORITHM);
            }
            else if (exitStatus == 43) {
            	Preferences.debug("Normal termination because we are computing at noise level\n", Preferences.DEBUG_ALGORITHM);
            	Preferences.debug("The 2nd but last step was subspace minimization but\n", Preferences.DEBUG_ALGORITHM);
                Preferences.debug("the last two were Gauss-Newton steps \n", Preferences.DEBUG_ALGORITHM);  
            }
            else if (exitStatus == 44) {
            	Preferences.debug("Normal termination because we are computing at noise level\n", Preferences.DEBUG_ALGORITHM);
            	Preferences.debug("The steplength was not unit in both the last two steps\n", Preferences.DEBUG_ALGORITHM);
            } 
            else if (exitStatus == -1) {
                Preferences.debug("Abnormal termination because m < n or n <= 0 or m <= 0 or mdc < m or mdw < n*n + 5*n + 3*m + 6 or\n", 
                		Preferences.DEBUG_ALGORITHM);
                Preferences.debug("maxit <= 0 or epsrel < 0 or epsabs < 0 or epsx < 0 or invalid starting point on entry\n", 
                		Preferences.DEBUG_ALGORITHM);
            } 
            else if (exitStatus == -2) {
            	Preferences.debug("Abnormal termination because the number of iterations has exceeded the maximum allowed iterations\n", 
            			Preferences.DEBUG_ALGORITHM);
            }
            else if (exitStatus == -3) {
            	Preferences.debug("Abnormal termination because the Hessian emanating from the 2nd order method is not positive definite\n", 
            			Preferences.DEBUG_ALGORITHM);
            }
            else if (exitStatus == -4) {
            	Preferences.debug("Abnormal termination because the algorithm would like to use 2nd derivatives but is not allowed to do that\n", 
            			Preferences.DEBUG_ALGORITHM);
            }
            else if (exitStatus == -5) {
            	Preferences.debug("Abnormal termination because an undamped step with Newtons method is a failure\n", 
            			Preferences.DEBUG_ALGORITHM);
            }
            else if (exitStatus == -6) {
            	Preferences.debug("Abnormal termination because the latest search direction computed using subspace minimization\n", 
            			Preferences.DEBUG_ALGORITHM);
            	Preferences.debug("was not a descent direction (probably caused by a wrongly computed Jacobian)\n", 
            			Preferences.DEBUG_ALGORITHM);
            }
            else if (exitStatus == -7) {
            	Preferences.debug("Abnormal termination because there is only one feasible point,\n", 
            			Preferences.DEBUG_ALGORITHM);
            	Preferences.debug("namely X(I) = BL(I) = BU(I), I = 1,2,...,N\n", Preferences.DEBUG_ALGORITHM);
            }
            else if (exitStatus == -8) {
            	Preferences.debug("Abnormal termination due to driver error\n", Preferences.DEBUG_ALGORITHM);
            }
            else {
            	Preferences.debug("Exit status = " + exitStatus + "\n", Preferences.DEBUG_ALGORITHM);
            }
            Preferences.debug("\n", Preferences.DEBUG_ALGORITHM);
        }
        
        
        
        private void driverCalls() {
        	// The default is internalScaling = false
            internalScaling = false;
            analyticalJacobian = false;
            fitTestModel();
            driver();
            dumpTestResults();
            internalScaling = false;
            analyticalJacobian = true;
            fitTestModel();
            driver();
            dumpTestResults();
            internalScaling = true;
            analyticalJacobian = false;
            fitTestModel();
            driver();
            dumpTestResults();
            internalScaling = true;
            analyticalJacobian = true;
            fitTestModel();
            driver();
            dumpTestResults();
            Preferences.debug("\n", Preferences.DEBUG_ALGORITHM);	
        }
    /**
     * fitToFunction communicates with 3 protected variables param, nPts, and ctrlMat ctrlMat is used as a wrapper for
     * ctrl or lctrl. Evaluates the residuals or the Jacobian at a certain a[]
     *
     * @param  a          DOCUMENT ME!
     * @param  residuals  DOCUMENT ME!
     * @param  covarMat   DOCUMENT ME!
     */
    public abstract void fitToFunction(DoubleDouble[] a, DoubleDouble[] residuals, DoubleDouble[][] covarMat);

    /**
     * driver.
     */
    public void driver() {
    	int i;
    	int j;
    	int k;
        try {
            outputMes = false;
            for (i = 0; i < param; i++) {
                a[i] = gues[i];
            }

            if (internalScaling) {
                diag = new DoubleDouble[param];
            }

            exitStatus = 0;

            // Initialize the bl and bu arrays
            bl0 = DoubleDouble.valueOf(-big);
            bu0 = DoubleDouble.valueOf(big);

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
            	if (outputMes) {
                    Preferences.debug("exitStatus after lsunc() call = " + exitStatus + "\n", Preferences.DEBUG_ALGORITHM);
            	}

                return;
            }

            if (((exitStatus % 10) == 1) && outputMes) {
                System.out.println("Pseudo-rank of the Jacobian is not full < (param - constraintAct)\n");

                if (!internalScaling) {
                    System.out.println("A run with internalScaling = true might " + "give another solution\n");
                }

                Preferences.debug("Pseudo-rank of the Jacobian is not full < (param - constraintAct)\n", 
                		Preferences.DEBUG_ALGORITHM);

                if (!internalScaling) {
                    Preferences.debug("A run with internalScaling = true might " + "give another solution\n", 
                    		Preferences.DEBUG_ALGORITHM);
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
                    covarMat[k][j] = covarMat[k][j].multiply(diag[j]);
                } // for (k = 0; k < param; k++)
            } // for (j = 0; j < param; j++)

            for (i = 0; i < param; i++) {

                for (k = 0; k < param; k++) {
                    covarMat[i][k] = covarMat[i][k].multiply(diag[i]);
                } // for (k = 0; k < param; k++)
            } // for (i = 0; i < param; i++)

            return;
        } catch (Exception e) {
            Preferences.debug("driver error: " + e.getMessage() + "\n", Preferences.DEBUG_ALGORITHM);
        }
    }

    /**
     * getChiSquared - accessor to chi-squared value (goodness-of-fit measure).
     *
     * @return  the value of chi squared
     */
    public double getChiSquared() {
    	if (phi == null) {
    		Preferences.debug("phi is null in getChiSquared\n", Preferences.DEBUG_ALGORITHM);
    		return Double.NaN;
    	}
        return ((DoubleDouble.valueOf(2.0)).multiply(phi)).doubleValue();
    }

    /**
     * getParameters accessor to function parameters.
     *
     * @return  the function parameters determined by the algorithm
     */
    public double[] getParameters() {
    	double par[] = new double[a.length];
    	for (int i = 0; i < a.length; i++) {
    	    par[i] = a[i].doubleValue();	
    	}
        return par;
    }

    /**
     * DOCUMENT ME!
     *
     * @return  double[] residuals
     */
    public double[] getResiduals() {
    	double resid[] = new double[residuals.length];
    	for (int i = 0; i < residuals.length; i++) {
    		resid[i] = residuals[i].doubleValue();
    	}
        return resid;
    }
    
    public int getExitStatus() {
    	return exitStatus;
    }

    /**
     * DOCUMENT ME!
     */
    private void analuc(int prank[]) {
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
        // constraintAct INTEGER SCALAR CONTAINING THE NO. OF ACTIVE CONSTRAINTS
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
        // constraintAct IS PUT EQUAL TO -constraintAct-1 IF constraintAct < 0 ON ENTRY
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
        int rank[] = new int[1];

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

            subuc(prank, rank);
            kod = -1;

            if (rank[0] == prank[0]) {
                kod = 1;
            }

            prank[0] = rank[0];
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
            prank[0] = -(param - constraintAct);
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
    private void btrunc(int prank[], boolean internalScaling, DoubleDouble[] work) {
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

        if (prank[0] != param) {

            for (i = prank[0]; i < param; i++) {
                dx[i] = DoubleDouble.valueOf(0.0);
            }
        }

        // DO THE PIVOTING

        pivec(work);

        // EVENTUALLY RESCALE

        if (!internalScaling) {
            return;
        }

        for (i = 0; i < param; i++) {
            dx[i] = diag[i].multiply(dx[i]);
        }

        return;
    }

    /**
     * DOCUMENT ME!
     *
     * @param  dphize  DOCUMENT ME!
     */
    private void chder(DoubleDouble dphize, int[] k, DoubleDouble[] alfk, DoubleDouble[] phik) {
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

        DoubleDouble phimk[] = new DoubleDouble[1]; 
        DoubleDouble dphifo, dphiba, dphice, maxdif;
        DoubleDouble[] xnew = new DoubleDouble[param];
        DoubleDouble[] fnew = new DoubleDouble[nPts];
        int chderCtrl[] = new int[1];

        // COMPUTE phi(-alfk)

        chderCtrl[0] = -1;
        fsumsq(alfk[0].negate(), xnew, fnew, phimk, chderCtrl);

        if (chderCtrl[0] < -10) {
            k[0] = chderCtrl[0];
        }

        if (k[0] < 0) {
            return;
        }

        k[0]++;

        // COMPUTE APPROXIMATIONS OF THE DERIVATIVE BY USING FORWARD,
        // BACKWARD AND CENTRAL DIFFERENCES

        if (alfk[0].le(DoubleDouble.valueOf(0.0))) {
            errorStatus = -1;

            return;
        }

        dphifo = (phik[0].subtract(phi)).divide(alfk[0]);
        dphiba = (phi.subtract(phimk[0])).divide(alfk[0]);
        dphice = ((phik[0].subtract(phimk[0])).divide(DoubleDouble.valueOf(2.0))).divide(alfk[0]);
        maxdif = (dphifo.subtract(dphiba)).abs();
        maxdif = maxdif.max((dphifo.subtract(dphice)).abs());
        maxdif = maxdif.max((dphiba.subtract(dphice)).abs());

        if ((((dphifo.subtract(dphize)).abs()).gt(maxdif)) && (((dphice.subtract(dphize)).abs()).gt(maxdif))) {
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
    private void choose(DoubleDouble xmin, DoubleDouble[] v2, DoubleDouble[] root1,
    		            DoubleDouble[] root2, DoubleDouble[] proot2) {
        // X1Minrm, X2Minrm AND X3Minrm ARE THREE REAL ROOTS OF A 3:RD DEGREE POLYNOMIAL WHICH TENDS TO MINUS INFINITY
        // WHEN X TENDS TO MINUS INFINITY. CHOOSE ONE OF THE OUTER ROOTS FOR alfkp1. beta2 = THE OTHER OUTER ONE. pbeta
        // = THE VALUE OF THE CORRESPONDING 4:TH DEGREE POLYNOMIAL AT beta2.

        DoubleDouble x, y, z;

        y = x1Minrm;
        x = x1Minrm.min(x2Minrm.min(x3Minrm));
        z = x1Minrm.max(x2Minrm.max(x3Minrm));

        if ((x1Minrm.le(x2Minrm)) && (x1Minrm.le(x3Minrm))) {
            y = x2Minrm.min(x3Minrm);
        }

        if ((x2Minrm.le(x1Minrm)) && (x2Minrm.le(x3Minrm))) {
            y = x1Minrm.min(x3Minrm);
        }

        if ((x3Minrm.le(x1Minrm)) && (x3Minrm.le(x2Minrm))) {
            y = x1Minrm.min(x2Minrm);
        }

        if (xmin.le(y)) {
            root1[0] = x;
            root2[0] = z;
        } else {
            root1[0] = z;
            root2[0] = x;
        }

        proot2[0] = pol4(v2, root2[0]);

        return;
    }

    /**
     * DOCUMENT ME!
     */
    private void covar() {
        int i, ii, j, jj, k, km1, l;
        boolean sing;
        DoubleDouble temp, tolr;

        // FORM THE INVERSE OF covarMat IN THE FULL UPPER TRIANGLE OF covarMat.

        tolr = tolerance.multiply((covarMat[0][0]).abs());
        l = -1;

        for (k = 0; k < param; k++) {

            if ((covarMat[k][k].abs()).le(tolr)) {
                break;
            }

            covarMat[k][k] = (DoubleDouble.valueOf(1.0)).divide(covarMat[k][k]);
            km1 = k - 1;

            for (j = 0; j <= km1; j++) {
                temp = covarMat[k][k].multiply(covarMat[j][k]);
                covarMat[j][k] = DoubleDouble.valueOf(0.0);

                for (i = 0; i <= j; i++) {
                    covarMat[i][k] = covarMat[i][k].subtract(temp.multiply(covarMat[i][j]));
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
                    covarMat[i][j] = covarMat[i][j].add(temp.multiply(covarMat[i][k]));
                } // for (i = 0; i <= j; i++)
            } // for (j = 0; j <= km1; j++)

            temp = covarMat[k][k];

            for (i = 0; i <= k; i++) {
                covarMat[i][k] = temp.multiply(covarMat[i][k]);
            } // for (i = 0; i <= k; i++)
        } // for (k = 0; k <= l; k++)

        // FORM THE FULL LOWER TRIANGLE OF THE COVARIANCE MATRIX
        // IN THE STRICT LOWER TRIANGLE OF covarMat AND IN dx

        for (j = 0; j < param; j++) {
            jj = pivit[j];
            sing = (j > l);

            for (i = 0; i <= j; i++) {

                if (sing) {
                    covarMat[i][j] = DoubleDouble.valueOf(0.0);
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
    private void dimsub(int prank[], int rank[]) {
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
        DoubleDouble[] dummy = new DoubleDouble[1];
        DoubleDouble pgress;
        DoubleDouble[] work = new DoubleDouble[param];
        DoubleDouble[] temp = new DoubleDouble[2];

        rabs = DoubleDouble.valueOf(0.1);

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
                w1[i] = residuals[i].negate();
            }

            sqrsl(covarMat, nPts, prank[0], w0, w1, dummy, w1, dummy, dummy, dummy, 1000);

            // COMPUTE ESTIMATES OF STEPLENGTHS w1[i] AND PROGRESS WORK[i]

            work[0] = w1[0];
            j = pivit[0];

            if (!internalScaling) {
                w1[0] = w1[0].divide(covarMat[0][0]);
            } else {
                w1[0] = (w1[0].multiply(diag[j])).divide(covarMat[0][0]);
            }

            for (i = 1; i < prank[0]; i++) {
                work[i] = w1[i];

                if (internalScaling) {
                    j = pivit[i];
                    w1[i] = (w1[i].multiply(diag[j])).divide(covarMat[i][i]);
                } else {
                    w1[i] = w1[i].divide(covarMat[i][i]);
                }

                temp[0] = w1[i - 1];
                temp[1] = w1[i];
                w1[i] = dnrm2(2, temp, 1);
                temp[0] = work[i - 1];
                temp[1] = work[i];
                work[i] = dnrm2(2, temp, 1);
            } // for (i = 1; i < pseudoRank; i++)

            sn = w1[prank[0] - 1];
            bn = work[prank[0] - 1];

            // DETERMINE THE LOWEST POSSIBLE DIMENSION

            for (i = 0; i < prank[0]; i++) {
                mindim = i + 1;

                if (work[i].gt(rabs.multiply(bn))) {
                    break;
                }
            } // for (i = 0; i <  pseduoRank; i++)

            if (kod == 1) {
                pregn(work, mindim, prank, rank);
            }

            if (kod == -1) {
                presub(work, prank, rank);
            }

            return;
        } // if (!restart)

        // SUGGEST NEW PSEUDO RANK = LATEST PSEUDO RANK -1
        // ALSO SAVE PSEUDO RANK THAT REDUCES THE OBJECTIVE BEST

        if (icount <= 1) {
            bestpg = ((DoubleDouble.valueOf(0.5)).multiply(fsum)).subtract(philat);
            irank = lattry;
        }

        pgress = ((DoubleDouble.valueOf(0.5)).multiply(fsum)).subtract(philat);

        if (pgress.gt(bestpg)) {
            bestpg = ((DoubleDouble.valueOf(0.5)).multiply(fsum)).subtract(philat);
            irank = lattry;
        }

        rank[0] = lattry - 1;

        if (lattry <= 1) {
            rank[0] = irank;
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
    private DoubleDouble dnrm2(int n, DoubleDouble[] x, int incx) {

        // Euclidean norm of the n-vector stored in x[] with storage increment incx .
        // if n <= 0 return with result = 0.
        // if n >= 1 then incx must be >= 1

        // four phase method using two built-in constants that are
        // hopefully applicable to all machines.
        // cutlo = maximum of  SQRT(u/eps)  over all known machines.
    	// u = eps = 4.9E-324 so cutlo = 1
    	// For regular 64 bit operation cutlo = 1.491E-154
        // cuthi = minimum of  SQRT(v)      over all known machines.
    	// cuthi = 1.34078E154, the same as for regular 64 bit operation.
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

        DoubleDouble fn_val = DoubleDouble.valueOf(0.0);
        int i, j, nn, next;
        DoubleDouble cuthi, cutlo, hitest, sum, xmax;
        DoubleDouble term;
        boolean doSwitch, loop, do85, do100, do105, do110, do115, do200;

        if ((n <= 0) || (incx <= 0)) {
            fn_val = DoubleDouble.valueOf(0.0);

            return fn_val;
        }

        // Set machine-dependent constants

        cutlo = (new DoubleDouble(Double.MIN_VALUE, 0.0));
        cutlo = (cutlo.divide(srelpr)).sqrt();
        cuthi = (new DoubleDouble(Math.sqrt(Double.MAX_VALUE),0.0));

        next = 1;
        sum = DoubleDouble.valueOf(0.0);
        nn = n * incx;
        xmax = DoubleDouble.valueOf(0.0);
        i = 1;
        j = 0;
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
                        if ((x[i-1].abs()).gt(cutlo)) {
                            do85 = true;
                        } else {
                            next = 2;
                            xmax = DoubleDouble.valueOf(0.0);

                            continue mainLoop;
                        }

                        break;

                    case 2:

                        // phase 1. sum is zero
                        if (x[i-1].equals(DoubleDouble.valueOf(0.0))) {
                            do200 = true;
                        } else if ((x[i-1].abs()).gt(cutlo)) {
                            do85 = true;
                        } else {
                            next = 3;
                            do105 = true;
                        }

                        break;

                    case 3:
                        // phase 2.  sum is small.
                        // scale to avoid destructive underflow.

                        if ((x[i-1].abs()).gt(cutlo)) {
                            // prepare for phase 3.

                            sum = (sum.multiply(xmax)).multiply(xmax);
                            do85 = true;
                        } // if (Math.abs(x[i-1]) > cutlo )
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
                hitest = cuthi.divide(DoubleDouble.valueOf(n));

                for (j = i; j <= nn; j+=incx) {

                    if ((x[j-1].abs()).ge(hitest)) {
                        do100 = true;
                        doSwitch = false;

                        continue mainLoop;
                    }

                    sum = sum.add(x[j-1].multiply(x[j-1]));
                } // for (j = i; j <= nn; j+=incx)

                fn_val = sum.sqrt();

                return fn_val;
            } // if (do85)

            if (do100) {
                do100 = false;

                // prepare for phase 4.
                // ABS(x(i)) is very large
                i = j;
                next = 4;
                doSwitch = true;
                sum = (sum.divide(x[i-1])).divide(x[i-1]);

                // Set xmax; large if next = 4, small if next = 3
                xmax = x[i-1].abs();
                term = x[i-1].divide(xmax);
                sum = sum.add(term.multiply(term));

                i = i + incx;

                if (i <= nn) {
                    continue mainLoop;
                }

                // end of main loop.

                // compute square root and adjust for scaling.

                fn_val = xmax.multiply(sum.sqrt());

                return fn_val;
            } // if (do100)

            if (do200) {
                do200 = false;
                doSwitch = true;
                i = i + incx;

                if (i <= nn) {
                    continue mainLoop;
                }

                // end of main loop.

                // compute square root and adjust for scaling.

                fn_val = xmax.multiply(sum.sqrt());

                return fn_val;
            } // if (do200)

            if (do105) {
                do105 = false;
                doSwitch = true;
                xmax = x[i-1].abs();
                term = x[i-1].divide(xmax);
                sum = sum.add(term.multiply(term));

                i = i + incx;

                if (i <= nn) {
                    continue mainLoop;
                }

                // end of main loop.

                // compute square root and adjust for scaling.

                fn_val = xmax.multiply(sum.sqrt());

                return fn_val;
            } // if (do105)

            if (do110) {
                do110 = false;
                // common code for phases 2 and 4.
                // in phase 4 sum is large.  scale to avoid overflow.

                if ((x[i-1].abs()).le(xmax)) {
                	doSwitch = true;
                    term = x[i-1].divide(xmax);
                    sum = sum.add(term.multiply(term));

                    i = i + incx;

                    if (i <= nn) {
                        continue mainLoop;
                    }

                    // end of main loop.

                    // compute square root and adjust for scaling.

                    fn_val = xmax.multiply(sum.sqrt());

                    return fn_val;
                }

                term = xmax.divide(x[i-1]);
                sum = (DoubleDouble.valueOf(1.0)).add(sum.multiply(term.multiply(term)));
                xmax = x[i-1].abs();
                doSwitch = true;

                i = i + incx;

                if (i <= nn) {
                    continue mainLoop;
                }

                // end of main loop.

                // compute square root and adjust for scaling.

                fn_val = xmax.multiply(sum.sqrt());

                return fn_val;
            } // if (do110)

            if (do115) {
                do115 = false;
                doSwitch = true;
                term = x[i-1].divide(xmax);
                sum = sum.add(term.multiply(term));

                i = i + incx;

                if (i <= nn) {
                    continue mainLoop;
                }

                // end of main loop.

                // compute square root and adjust for scaling.

                fn_val = xmax.multiply(sum.sqrt());

                return fn_val;
            } // if (do115)
        } // while (loop)

        // Should never reach here
        return fn_val;

    } // dnrm2

    /**
     * DOCUMENT ME!
     */
    private void evreuc(int prank[]) {
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
        DoubleDouble c2, dsqrel;
        DoubleDouble[][] dummy = new DoubleDouble[1][1];
        DoubleDouble temp;
        int evreucCtrl;

        restart = false;
        dsqrel = srelpr.sqrt();

        // CALL RESTART ADVISOR ROUTINE

        reavuc();

        if (restart) {
        	if (outputMes) {
                Preferences.debug("In evreuc: restart\n", Preferences.DEBUG_ALGORITHM);
        	}
            itotal++;
            icount++;
            lattry = prank[0];
            philat = phi;
        } // if restart
        else if (errorStatus == -5) {
        	if (outputMes) {
                Preferences.debug("Undamped Newton  does not work: Try GN\n", Preferences.DEBUG_ALGORITHM);
        	}
            ifree = 5;
            indic = 3;
            iters++;
        } // else if (errorStatus == -5)
        else {
            // NO RESTART.UPDATE HISTORY VARIABLES

            icount = 0;
            lattry = prank[0];
            betkm2 = betkm1;
            alfkm2 = alfkm1;
            kodkm2 = kodkm1;
            betkm1 = beta;
            d1km1 = d1sqs;
            fsqkm1 = fsum;
            dxnkm1 = dxnorm;
            alfkm1 = alpha;
            aupkm1 = alphup;
            rngkm1 = prank[0];
            kodkm1 = kod;

            // CHECK IF ANY BOUND IS CROSSED.IF SO, MAKE THE
            // CORRESPONDING CONSTRAINT ACTIVE

            constraintAct = 0;
            for (i = 0; i < param; i++) {

                aset[i] = 0;

                c2 = DoubleDouble.valueOf(0.0);
                
                if (a[i].lt((bl[i].add(c2)).add(dsqrel))) {
                    aset[i] = 1;
                    a[i] = bl[i];
                    constraintAct++;

                    continue;
                }

                c2 = DoubleDouble.valueOf(0.0);

                if (a[i].le((bu[i].subtract(c2)).subtract(dsqrel))) {
                    continue;
                }

                aset[i] = -1;
                a[i] = bu[i];
                constraintAct++;
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

        evreucCtrl = -1;

        // EVALUATE THE FUNCTIONS AT PREVIOUS POINT
        ctrlMat[0] = evreucCtrl;
        if (testMode) {
        	fitToTestFunction(a, residuals, dummy);
        }
        else {
            fitToFunction(a, residuals, dummy);
        }
        evreucCtrl = ctrlMat[0];

        if (evreucCtrl < -10) {
            errorStatus = evreucCtrl;

            return;
        }

        alpha = alfkm1;
        alphup = aupkm1;
        temp = dnrm2(nPts, residuals, 1);
        phi = ((DoubleDouble.valueOf(0.5)).multiply(temp)).multiply(temp);
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
    private void fsumsq(DoubleDouble alfk, DoubleDouble[] xnew, DoubleDouble[] fnew, DoubleDouble[] fn_val, int ctrl[]) {
        // EVALUATE FUNCTION VALUES AT THE POINT a+alfk*dx . POSSIBLY THE USER CAN SIGNAL UNCOMPUTABILTY ON RETURN FROM
        // THE USER WRITTEN ROUTINE fitToFunction

        DoubleDouble[][] dummy = new DoubleDouble[1][1];
        int lctrl;
        int i;
        DoubleDouble temp;

        fn_val[0] = DoubleDouble.valueOf(0.0);

        for (i = 0; i < param; i++) {
            xnew[i] = a[i].add(alfk.multiply(dx[i]));
        }

        lctrl = ctrl[0];
        ctrlMat[0] = lctrl;
        if (testMode) {
        	fitToTestFunction(xnew, fnew, dummy);
        }
        else {
            fitToFunction(xnew, fnew, dummy);
        }
        lctrl = ctrlMat[0];
        if (outputMes) {
	        Preferences.debug("xnew and fnew inside fsumsq\n", Preferences.DEBUG_ALGORITHM);
	
	        for (i = 0; i < param; i++) {
	            Preferences.debug("xnew[" + i + "] = " + xnew[i] + "\n", Preferences.DEBUG_ALGORITHM);
            }
	        
	        for (i = 0; i < nPts; i++) {
	        	Preferences.debug("fnew[" + i + "] = " + fnew[i] + "\n", Preferences.DEBUG_ALGORITHM);
	        }
        }

        if (ctrl[0] != 1) {

            if (lctrl == -1) {
                temp = dnrm2(nPts, fnew, 1);
                fn_val[0] = ((DoubleDouble.valueOf(0.5)).multiply(temp)).multiply(temp);
            }
            
            if (outputMes) {
                Preferences.debug("fn_val[0] when ctrl[0] != 1 = " + fn_val[0] + "\n", Preferences.DEBUG_ALGORITHM);
            }

            if (lctrl < -10) {
                ctrl[0] = lctrl;
            }

            return;
        } // if (ctrl != 1)

        if (lctrl == 1) {
            temp = dnrm2(nPts, fnew, 1);
            fn_val[0] = ((DoubleDouble.valueOf(0.5)).multiply(temp)).multiply(temp);
        }
      
        if (outputMes) {
            Preferences.debug("fn_val[0] when ctrl[0]= 1 = " + fn_val[0] + "\n", Preferences.DEBUG_ALGORITHM);
        }
        ctrl[0] = lctrl;

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
    private void gauc(int k[], DoubleDouble alfmin, DoubleDouble[] gmod, DoubleDouble dphize, DoubleDouble u[],
    		          DoubleDouble[] phiu, DoubleDouble tau, DoubleDouble pmax) {
        // THIS IS A ROUTINE FOR UNCONSTRAINED LEAST SQUARES PROBLEMS THAT HALVES THE VALUE OF alfk UNTIL A
        // GOLDSTEIN-ARMIJO CONDITION IS SATISFIED OR UNTIL THE NORM OF THE SEARCH DIRECTION TIMES THE THE STEPLENGTH IS
        // REDUCED BELOW SQRT(RELATIVE PRECISION) OR UNTIL THE STEPLENGTH IS SMALLER THAN ALFMIN

        // PHI(ALPHA)=0.5*(IIF(a+ALPHA*dx)II**2)
        // CHOOSE ALPHA=a SO THAT
        // PHI(x)<=PHI(0)+TAU*x*DPHI(0)   (1)
        // WE KNOW THAT PHI(U)>PHI(0)+TAU*alfk*DPHI(0)
        // THE SIMPLEST WE CAN DO IS TO SET   alfk=alfk*0.5 AND
        // TEST IF CONDITION  (1) IS SATISFIED FOR x=alfk

        DoubleDouble phix[] = new DoubleDouble[1];
        DoubleDouble sqreps, x;
        int gaucCtrl[] = new int[1];

        sqreps = srelpr.sqrt();
        phix[0] = phi;
        x = u[0];

        do {

            if (((pmax.multiply(x)).lt(sqreps)) || (x.le(alfmin))) {
                errorStatus = -2;
            }

            if (errorStatus == -2) {
                u[0] = x;
                phiu[0] = phix[0];

                return;
            }

            x = (DoubleDouble.valueOf(0.5)).multiply(x);
            gaucCtrl[0] = -1;
            fsumsq(x, gmod, residuals, phix, gaucCtrl);
            if (outputMes) {
                Preferences.debug("In GAUC: phix[0] = " + phix[0] + " x = " + x + "\n", Preferences.DEBUG_ALGORITHM);
            }

            // POSSIBLY THE USER CAN TERMINATE

            if (gaucCtrl[0] < -10) {
                k[0] = gaucCtrl[0];
            }

            if (k[0] < 0) {
                return;
            }

            k[0]++;
        } while (phix[0].gt(phi.add((tau.multiply(x)).multiply(dphize))));

        u[0] = x;
        phiu[0] = phix[0];

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
        // constraintAct INTEGER SCALAR CONTAINING THE NO.OF ACTIVE CONSTRAINTS
        // (POSITIVE OR NEGATIVE)
        // aset[] INTEGER 1D ARRAY OF DIMENSION param
        // CONTAINING +1 OR -1 TO INDICTATE AN ACTIVE BOUND (OTHERWISE ZERO)

        // ON RETURN

        // secind  INDICATES THE ALTERNATIVE IF GN-DIRECTION IS NOT ACCEPTED
        // = -1 IF SUBSPACE MINIMIZATION
        // = -2 IF NEWTONS METHOD
        // constraintAct IS PUT EQUAL TO -constraintAct-1 IF constraintAct IS <0 ON ENTRY
        // gndok = true IF CURRENT GN-DIRECTION IS ACCEPTED
        // = false IF RECOMPUTATION OF SEARCH DIRECTION IS NEEDED

        DoubleDouble pgress, fnorm, ac;
        final DoubleDouble c1 = DoubleDouble.valueOf(0.5), c2 = DoubleDouble.valueOf(0.1), 
                           c3 = DoubleDouble.valueOf(4.0), c4 = DoubleDouble.valueOf(10.0),
                           c5 = DoubleDouble.valueOf(0.016);

        gndok = true;
        pgress = fsum.negate();
        fnorm = fsum.sqrt();

        // CONDITION 1

        if ((!restart) && ((iters == 0) || (imax != -1))) {
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
        pgress = fsqkm1.subtract(fsum);

        if (Math.abs(kod) == 2) {
            secind = kod;

            return;
        }

        if ((kod != -1) && (!restart)) {
            gndok = true;

            // CONDITION 2

            ac = (DoubleDouble.valueOf(1.0)).min(aupkm1);

            if ((pgress.gt(((c2.multiply(ac)).multiply((DoubleDouble.valueOf(2.0)).subtract(ac))).multiply(d1km1))) &&
                (fnorm.lt(c3.multiply(beta)))) {
                return;
            }

            // CONDITION 3

            if (beta.lt(c1.multiply(betkm1))) {
                return;
            }

            gndok = false;

            if (fnorm.le(c4.multiply(beta))) {
                secind = -1;
            } else {
                secind = -2;
            }

            return;
        } // if ((kod != -1) && (!restart))

        secind = -1;

        if (!restart) {

            if ((alfkm1.lt(c5.multiply(aupkm1))) && (prekm1.lt(c2.multiply(beta)))) {
                secind = -2;
            }

            return;
        } // if (!restart)

        if (prekm1.lt(c2.multiply(beta))) {
            secind = -2;
        }

        if (icount != 1) {
            return;
        }

        if (imax == -1) {
            return;
        }

        if ((aset[imax] == 0) && (ival != 0)) {
        	constraintAct++;
        }
        aset[imax] = ival;
        imax = -1;

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
    private DoubleDouble gndunc(boolean internalScaling, int[] prank, DoubleDouble[] work) {
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

        DoubleDouble d1sqs;
        int i, j;
        DoubleDouble[] dummy = new DoubleDouble[1];
        DoubleDouble temp;

        d1sqs = DoubleDouble.valueOf(0.0);

        for (j = 0; j < nPts; j++) {
            w1[j] = residuals[j].negate();
        }

        if ((param == 0) || (prank[0] <= 0)) {
            return d1sqs;
        }
        // T
        // COMPUTE THE SOLUTION DX,THE PROJECTION  -V  AND w1 = -Q * residuals

        sqrsl(covarMat, nPts, prank[0], w0, w1, dummy, w1, dx, dummy, v, 1101);
        temp = dnrm2(prank[0], w1, 1);
        d1sqs = temp.multiply(temp);

        if (info == 0) {
            btrunc(prank,internalScaling, work);

            return d1sqs;
        }

        // SQRSL HAS DETECTED EXACT SINGULARITY OF MATRIX R
        // INFO = INDEX OF THE FIRST ZERO DIAGONAL ELEMENT OF R+1
        // SOLVE UPPER TRIANGULAR SYSTEM

        prank[0] = info-1;

        for (j = 0; j < param; j++) {
            dx[j] = w1[j];
        }

        if (prank[0] <= 0) {
            btrunc(prank,internalScaling, work);

            return d1sqs;
        }

        strsl(prank,w1);

        // MOVE SOLUTION OF TRIANGULAR SYSTEM TO DX

        for (i = 0; i < param; i++) {
            temp = dx[i];
            dx[i] = w1[i];
            w1[i] = temp;
        } // for (i = 0; i < param; i++)

        // DO BACKTRANSFORMATIONS   DX:=D*E*DX

        btrunc(prank,internalScaling, work);

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

        DoubleDouble[] c1Mat = new DoubleDouble[nPts];
        DoubleDouble[] c2Mat = new DoubleDouble[nPts];
        DoubleDouble[] c3Mat = new DoubleDouble[nPts];
        DoubleDouble[] c4Mat = new DoubleDouble[nPts];
        DoubleDouble[][] dummy = new DoubleDouble[1][1];
        int j, k, ll;
        DoubleDouble athird, eps1, eps2, epsk, epsj, ak, aj, term, sum;
        int hessCtrl;

        hessCtrl = -1;
        athird = (DoubleDouble.valueOf(1.0)).divide(DoubleDouble.valueOf(3.0));
        eps2 = srelpr.pow(athird);
        
        eps1 = eps2;

        for (k = 0; k < param; k++) {
            ak = a[k];
            epsk = ((ak.abs()).max(DoubleDouble.valueOf(1.0))).multiply(eps2);

            for (j = 0; j <= k; j++) {
                aj = a[j];
                epsj = ((aj.abs()).max(DoubleDouble.valueOf(1.0))).multiply(eps1);
                a[k] = ak.add(epsk);
                a[j] = a[j].add(epsj);
                ctrlMat[0] = hessCtrl;
                if (testMode) {
                	fitToTestFunction(a, c1Mat, dummy);
                }
                else {
                    fitToFunction(a, c1Mat, dummy);
                }
                hessCtrl = ctrlMat[0];

                if (hessCtrl < -10) {
                    errorStatus = hessCtrl;

                    return;
                }

                a[k] = ak;
                a[j] = aj;
                a[k] = ak.add(epsk);
                a[j] = a[j].subtract(epsj);
                ctrlMat[0] = hessCtrl;
                if (testMode) {
                	fitToTestFunction(a, c2Mat, dummy);
                }
                else {
                    fitToFunction(a, c2Mat, dummy);
                }
                hessCtrl = ctrlMat[0];

                if (hessCtrl < -10) {
                    errorStatus = hessCtrl;

                    return;
                }

                a[k] = ak;
                a[j] = aj;
                a[k] = ak.subtract(epsk);
                a[j] = a[j].add(epsj);
                ctrlMat[0] = hessCtrl;
                if (testMode) {
                	fitToTestFunction(a, c3Mat, dummy);
                }
                else {
                    fitToFunction(a, c3Mat, dummy);
                }
                hessCtrl = ctrlMat[0];

                if (hessCtrl < -10) {
                    errorStatus = hessCtrl;

                    return;
                }

                a[k] = ak;
                a[j] = aj;
                a[k] = ak.subtract(epsk);
                a[j] = a[j].subtract(epsj);
                ctrlMat[0] = hessCtrl;
                if (testMode) {
                	fitToTestFunction(a, c4Mat, dummy);
                }
                else {
                    fitToFunction(a, c4Mat, dummy);
                }
                hessCtrl = ctrlMat[0];

                if (hessCtrl < -10) {
                    errorStatus = hessCtrl;

                    return;
                }

                a[k] = ak;
                a[j] = aj;
                sum = DoubleDouble.valueOf(0.0);

                for (ll = 0; ll < nPts; ll++) {
                    term = ((c1Mat[ll].subtract(c2Mat[ll])).subtract(c3Mat[ll])).add(c4Mat[ll]);
                    sum = sum.add((term.divide((DoubleDouble.valueOf(4.0).multiply(epsk)).multiply(epsj))).multiply(residuals[ll]));
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
    private void jacdif(int ctrl[]) {
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
        DoubleDouble delta, xtemp, deltaj;
        int jacCtrl[] = new int[1];

        ctrl[0] = 0;
        //delta = srelpr.sqrt();
        // For DoubleDouble srelpr = 4.9E-324, Double.MIN_VALUE, while for standard
        // 64 bit machines srelpr = 2.220446E-16.
        // Using the srelpr from extended precision always results in w1[i] = residuals[i],
        // so covarMat[i][j] is always set equal to zero.
        // Therefore, use the srelpr value obtained from 64 bit machines.
        delta = (DoubleDouble.valueOf(2.220446E-16)).sqrt();

        for (j = 0; j < param; j++) {
            xtemp = a[j];
            deltaj = ((xtemp.abs()).max(DoubleDouble.valueOf(1.0))).multiply(delta);
            a[j] = xtemp.add(deltaj);
            jacCtrl[0] = -1;
            ctrlMat[0] = jacCtrl[0];
            if (testMode) {
            	fitToTestFunction(a, w1, covarMat);
            }
            else {
                fitToFunction(a, w1, covarMat);
            }
            jacCtrl[0] = ctrlMat[0];

            if (jacCtrl[0] < -10) {
                ctrl[0] = jacCtrl[0];
                return;
            }

            for (i = 0; i < nPts; i++) {
                covarMat[i][j] = (w1[i].subtract(residuals[i])).divide(deltaj);
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
        DoubleDouble sum;

        for (j = 0; j < nn; j++) {

            for (i = 0; i < nn; i++) {
                w0[i] = covarMat[i][j];
            }

            for (k = j; k < nn; k++) {
                sum = DoubleDouble.valueOf(0.0);

                for (i = 0; i <= j; i++) {
                    sum = sum.add(covarMat[i][k].multiply(w0[i]));
                }
                covarMat[k][j] = sum;
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
    private void lineuc(DoubleDouble[] gmod, DoubleDouble dphize, DoubleDouble alplow) {
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
        int k[] = new int[1];
        DoubleDouble eta, tau, gamma, pmax, phikm2, xmin;
        DoubleDouble alfmax;
        DoubleDouble alfmin;
        DoubleDouble alfk[] = new DoubleDouble[1];
        DoubleDouble alfkm1[] = new DoubleDouble[1];
        DoubleDouble alfkm2;
        DoubleDouble alfkp1[] = new DoubleDouble[1];
        DoubleDouble beta[] = new DoubleDouble[1];
        DoubleDouble diff;
        DoubleDouble pbeta[] = new DoubleDouble[1];
        DoubleDouble phik[] = new DoubleDouble[1];
        DoubleDouble phikm1[] = new DoubleDouble[1];
        DoubleDouble pk[] = new DoubleDouble[1];
        DoubleDouble[] fnew = new DoubleDouble[nPts];
        DoubleDouble[] v2 = new DoubleDouble[nPts];
        boolean loop;
        int lineucCtrl[] = new int[1];
        boolean reduce[] = new boolean[1];

        k[0] = 0;
        aDiff = DoubleDouble.valueOf(0.0);
        alfkm1[0] = DoubleDouble.valueOf(0.0);
        phikm1[0] = phi;

        // SET VALUES OF THE CONSTANTS ETA,TAU AND GAMMA.
        // COMPUTE ALFMIN,ALFMAX,ALFK AND PMAX= THE EUCLIDEAN NORM OF dx

        eta = DoubleDouble.valueOf(0.3);
        tau = DoubleDouble.valueOf(0.25);
        gamma = DoubleDouble.valueOf(0.4);
        alfmax = alphup;
        alfmin = alplow;
        alfk[0] = alpha.min(alfmax);
        alfk[0] = alfk[0].max(alfmin);
        pmax = dnrm2(param, dx, 1);

        // dx MUST BE A DESCENT DIRECTION

        errorStatus = -1;

        if (dphize.ge(DoubleDouble.valueOf(0.0))) {
            alpha = alfkm1[0];
            phikp1 = phikm1[0];
            iev = k[0];

            return;
        }

        errorStatus = 0;

        // COMPUTE PHIK=PHI(ALF0) AND TEST UNCOMPUTABILITY AT aOld

        lineucCtrl[0] = 1;
        fsumsq(alfk[0], gmod, fnew, phik, lineucCtrl);
        if (outputMes) {
            Preferences.debug("In lineuc no. 1: phik[0] = " + phik[0] + " alfk[0] = " + alfk[0] + "\n", 
            		Preferences.DEBUG_ALGORITHM);
        }
        k[0]++;
        iev = k[0];

        if (lineucCtrl[0] == -1) {
            errorStatus = -3;
        }

        if (alfk[0].le(DoubleDouble.valueOf(0.0))) {
            errorStatus = -2;
        }

        if (lineucCtrl[0] < -10) {
            errorStatus = lineucCtrl[0];
        }

        if (errorStatus < 0) {
            return;
        }

        // COMPUTE THE VECTOR v2 SO THAT A ONE DIMENSIONAL
        // MINIMIZATION IN R[nPts] CAN BE DONE

        linuc2(fnew, alfk[0], v2);
        diff = phi.subtract(phik[0]);

        // SET XMIN = THE BEST OF THE POINTS 0 AND ALF0

        if (diff.ge(DoubleDouble.valueOf(0.0))) {
            xmin = alfk[0];
        } else {
            xmin = DoubleDouble.valueOf(0.0);
        }

        // MINIMIZE IN R[nPts]. USE TWO POINTS : 0 AND ALF0
        // NEW SUGGESTION OF STEPLENGTH IS ALFKP1
        // PK IS THE VALUE OF THE APPROXIMATING FUNCTION AT ALFKP1

        minrm(v2, alfmin, alfmax, xmin, alfkp1, pk, beta, pbeta);

        // POSSIBLY THE OTHER ROOT IS CHOSEN

        if ((!(alfkp1[0].equals(beta[0]))) && (pk[0].gt(pbeta[0])) && (beta[0].le(alfk[0]))) {
            alfkp1[0] = beta[0];
            pk[0] = pbeta[0];
        }

        alfkm1[0] = DoubleDouble.valueOf(0.0);
        phikm1[0] = phi;
        alfkm2 = alfkm1[0];
        phikm2 = phikm1[0];
        alfkm1[0] = alfk[0];
        phikm1[0] = phik[0];
        alfk[0] = alfkp1[0];

        // TEST TERMINATION CONDITION AT alpha = alf0
        loop = true;

        if (!(((diff.negate()).le((tau.multiply(dphize)).multiply(alfkm1[0]))) || (phikm1[0].lt(gamma.multiply(phi))))) {
            loop = false;
        }

        // TERMINATION CONDITION SATISFIED AT alpha = alf0
        while (loop) {
            diff = phi.subtract(phik[0]);

            // CHECK IF ESSENTIAL REDUCTION IS LIKELY

            reduc(alfkm1, phikm1, alfk[0], pk[0], diff, eta, gmod, fnew, k, phik, reduce);

            if (k[0] < -10) {
                errorStatus = k[0];

                return;
            }

            if (!reduce[0]) {

                for (i = 0; i < param; i++) {
                    gmod[i] = a[i];
                    a[i] = a[i].add(alfkm1[0].multiply(dx[i]));
                    gmod[i] = gmod[i].subtract(a[i]);
                }

                aDiff = dnrm2(param, gmod, 1);
                alpha = alfkm1[0];
                phikp1 = phikm1[0];
                iev = k[0];

                return;
            } // if (!reduce[0])

            // THE VALUE OF THE OBJECTIVE FUNCTION CAN MOST LIKELY BE REDUCED

            // MINIMIZE IN R[param].USE THREE POINTS:alfkm2a, alfkm1a, alfk
            // NEW SUGGESTION OF THE STEPLENGTH IS alfkp1
            // pk IS THE VALUE OF THE APPROXIMATING FUNCTION AT alfkp1

            minrn(alfk[0], phik[0], alfkm1[0], phikm1[0], alfkm2, phikm2, alfmin, alfmax, pmax, alfkp1, pk);
            alfkm2 = alfkm1[0];
            phikm2 = phikm1[0];
            alfkm1[0] = alfk[0];
            phikm1[0] = phik[0];
            alfk[0] = alfkp1[0];
        } // while (loop)

        // TERMINATION CONDITION NOT SATISFIED AT alpha = alf0

        // COMPUTE phik = phi(alf1)

        lineucCtrl[0] = -1;
        fsumsq(alfk[0], gmod, fnew, phik,lineucCtrl);
        if (outputMes) {
            Preferences.debug("In LINEUC no.2: phik[0] = " + phik[0] + " alfk[0] = " + alfk[0] + "\n", 
            		Preferences.DEBUG_ALGORITHM);
        }

        if (lineucCtrl[0] < -10) {
            k[0] = lineucCtrl[0];
        }

        if (k[0] < 0) {
            errorStatus = k[0];

            return;
        }

        // TEST TERMINATION CONDITION AT ALPHA = ALF1

        diff = phi.subtract(phik[0]);

        if (((diff.negate()).le((tau.multiply(dphize)).multiply(alfk[0]))) || (phik[0].lt(gamma.multiply(phi)))) {
            // TERMINATION CONDITION SATISFIED AT ALPHA = ALF1

            // CHECK IF ALF0 IS SOMEWHAT GOOD

            if (phi.le(phikm1[0])) {

                // SINCE PHI[0] <= PHI[ALF0], ALF0 IS NO GOOD GUESS AND WE TRY
                // AN OTHER MINIMIZATION IN R[nPts].USE TWO POINTS:0 AND ALF1.
                // THE NEW SUGGESTION OF STEPLENGTH IS ALFKP1.
                // PK IS THE VALUE OF THE APPROXIMATING FUNCTION AT ALFKP1

                xmin = alfk[0];
                linuc2(fnew, alfk[0], v2);
                minrm(v2, alfmin, alfmax, xmin, alfkp1, pk, beta, pbeta);

                // POSSIBLY THE OTHER ROOT IS CHOSEN

                if ((!(alfkp1[0].equals(beta[0]))) && (pk[0].gt(pbeta[0])) && (beta[0].le(alfk[0]))) {
                    alfkp1[0] = beta[0];
                    pk[0] = pbeta[0];
                }

                alfkm1[0] = DoubleDouble.valueOf(0.0);
                phikm1[0] = phi;
            } // if (phi <= phikm1)
            else {
                // MINIMIZE IN R[param].USE THREE POINTS:0, ALF0 AND ALF1.
                // THE NEW SUGGESTION OF THE STEPLENGTH IS ALFKP1.
                // PK IS THE VALUE OF THE APPROXIMATING FUNCTION AT ALFKP1

            	minrn(alfk[0], phik[0], alfkm1[0], phikm1[0], alfkm2, phikm2, alfmin, alfmax, pmax, alfkp1, pk);
            } // else

            k[0]++;
            loop = true;

            while (loop) {
                diff = phi.subtract(phik[0]);
                alfkm2 = alfkm1[0];
                phikm2 = phikm1[0];
                alfkm1[0] = alfk[0];
                phikm1[0] = phik[0];
                alfk[0] = alfkp1[0];

                // CHECK IF ESSENTIAL REDUCTION IS LIKELY

                reduc(alfkm1, phikm1, alfk[0], pk[0], diff, eta, gmod, fnew, k, phik, reduce);

                if (k[0] < -10) {
                    errorStatus = k[0];

                    return;
                }

                if (!reduce[0]) {

                    for (i = 0; i < param; i++) {
                        gmod[i] = a[i];
                        a[i] = a[i].add(alfkm1[0].multiply(dx[i]));
                        gmod[i] = gmod[i].subtract(a[i]);
                    }

                    aDiff = dnrm2(param, gmod, 1);
                    alpha = alfkm1[0];
                    phikp1 = phikm1[0];
                    iev = k[0];

                    return;
                } // if (!reduce[0])

                // MINIMIZE IN R[param].USE THREE POINTS:ALFKM2a, ALFKM1a AND ALFK.
                // THE NEW SUGGESTION OF STEPLENGTH IS ALFKP1.
                // PK IS THE VALUE OF THE APPROXIMATING FUNCTION AT ALFKP1

                minrn(alfk[0], phik[0], alfkm1[0], phikm1[0], alfkm2, phikm2, alfmin, alfmax, pmax, alfkp1, pk);
            } // while (loop)
        } // if ((-diff <= tau*dphize*alfk) || (phik < gamma*phi))

        // TAKE A PURE GOLDSTEIN-ARMIJO STEP

        k[0]++;
        gauc(k, alfmin, gmod, dphize, alfk, phik, tau, pmax);

        if (k[0] < -10) {
            errorStatus = k[0];

            return;
        }

        // COMPARE TWO EXPRESSIONS FOR THE DERIVATIVE OF PHI(a+alpha*dx)
        // AT ALPHA=0. IF INCONSISTENCY IS DETECTED K2 IS SET=-1 ON RETURN.

        if (errorStatus == -2) {
            chder(dphize, k, alfk, phik);
        }

        if (k[0] < -10) {
            errorStatus = k[0];

            return;
        }

        alfkm1[0] = alfk[0];
        phikm1[0] = phik[0];

        // COMPUTE THE NEW POINT AND THE DIFFERENCE II aOld-aNew II

        for (i = 0; i < param; i++) {
            gmod[i] = a[i];
            a[i] = a[i].add(alfkm1[0].multiply(dx[i]));
            gmod[i] = gmod[i].subtract(a[i]);
        }

        aDiff = dnrm2(param, gmod, 1);
        alpha = alfkm1[0];
        phikp1 = phikm1[0];
        iev = k[0];

        return;
    }

    /**
     * DOCUMENT ME!
     *
     * @param  fnew  DOCUMENT ME!
     * @param  v2    DOCUMENT ME!
     */
    private void linuc2(DoubleDouble[] fnew, DoubleDouble alfk, DoubleDouble[] v2) {

        // Form the vector v2 as the divided difference vector of
        // second order
        DoubleDouble f1, f2, f3, alf;
        int i;

        alf = alfk;

        for (i = 0; i < nPts; i++) {
            f1 = fnew[i];
            f2 = residuals[i];
            f3 = v[i];
            v2[i] = (((f1.subtract(f2)).divide(alf)).subtract(f3)).divide(alf);
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
    	int prank[] = new int[1];
        int i, paramCons;

        // a small positive value used to determine the pseudo rank
        // of the Jacobian
        DoubleDouble tau;
        DoubleDouble aNorm = DoubleDouble.valueOf(1.0);
        DoubleDouble norm;
        boolean loop;
        boolean doSoliuc;
        DoubleDouble c1 = DoubleDouble.valueOf(0.0);

        // validate input values
        if ((nPts < param) || (param <= 0) || (nPts <= 0) || (mdg < param) || (maxIterations <= 0) ||
                (tolerance.le(DoubleDouble.valueOf(0.0))) || (relativeConvergence.lt(DoubleDouble.valueOf(0.0))) || 
                (absoluteConvergence.lt(DoubleDouble.valueOf(0.0))) ||
                (parameterConvergence.lt(DoubleDouble.valueOf(0.0))) ||
                (((relativeConvergence.add(absoluteConvergence)).add(parameterConvergence)).le(DoubleDouble.valueOf(0.0)))) {
            exitStatus = -1;
            return;
        }

        // Initialize aset[i] and check values in bl[i] and bu[i]
        constraintAct = 0;
        paramCons = 0;

        for (i = 0; i < param; i++) {

            if (bl[i].gt(bu[i])) {
                exitStatus = -1;

                return;
            }

            aset[i] = 0;

            if (a[i].lt(bl[i].add(c1))) {
                a[i] = bl[i];
            }

            if (a[i].gt(bu[i].subtract(c1))) {
                a[i] = bu[i];
            }

            if (a[i].equals(bu[i])) {
                aset[i] = -1;
            }

            if ((a[i].equals(bl[i])) || (bl[i].equals(bu[i]))) {
                aset[i] = 1;
            }

            if (aset[i] != 0) {
                constraintAct = constraintAct + 1;
            }

            if (bl[i].equals(bu[i])) {
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
        aDiff = (dnrm2(param, a, 1).multiply(DoubleDouble.valueOf(2.0))).multiply(parameterConvergence);
        betkm1 = DoubleDouble.valueOf(0.0);
        alfkm1 = DoubleDouble.valueOf(1.0);
        alpha = DoubleDouble.valueOf(0.0);
        alfkm2 = DoubleDouble.valueOf(1.0);
        kodkm2 = 1;
        kod = 1;
        rngkm1 = Math.max(1, param - constraintAct);
        lattry = rngkm1;
        kodkm1 = 1;
        bestpg = DoubleDouble.valueOf(0.0);
        aupkm1 = DoubleDouble.valueOf(0.0);

        // Evaluate at user supplied point
        ctrl = 1;
        ctrlMat[0] = ctrl;
        if (testMode) {
        	fitToTestFunction(a, residuals, covarMat);
        }
        else {
            fitToFunction(a, residuals, covarMat);
        }
        ctrl = ctrlMat[0];

        if (ctrl == -1) {
            exitStatus = -1;

            return;
        }

        funcEval++;
        norm = dnrm2(nPts, residuals, 1);
        phi = ((DoubleDouble.valueOf(0.5)).multiply(norm)).multiply(norm);
        fsqkm1 = (DoubleDouble.valueOf(2.0)).multiply(phi);

        if (paramCons == param) {
            exitStatus = -7;

            return;
        }

        // Main loop of iteration starts here
        loop = true;
        doSoliuc = true;

        while (loop) {

            if (doSoliuc) {
                soliuc(tau,prank);

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
                fsum = norm.multiply(norm);

                if (dxnorm.lt(srelpr)) {
                    alfnoi = DoubleDouble.valueOf(1.0);
                } else {
                    alfnoi = (srelpr.sqrt()).divide(dxnorm);
                }
            } // if (doSoliuc)

            doSoliuc = true;

            // Check main termination criteria
            termuc(prank,aNorm);

            // If exitStatus <> 0, the iteration is finished
            if (exitStatus != 0) {
                speed = DoubleDouble.valueOf(0.0);

                if (!(betkm1.equals(DoubleDouble.valueOf(0.0)))) {
                    speed = beta.divide(betkm1);
                }

                return;
            }

            // Analyze the past and sometimes recompute the search direction
            analuc(prank);
            if (outputMes) {
	            Preferences.debug("POINT\n", Preferences.DEBUG_ALGORITHM);
	
	            for (i = 0; i < param; i++) {
	                Preferences.debug(a[i] + "\n", Preferences.DEBUG_ALGORITHM);
	            }
	
	            Preferences.debug("prank[0] = " + prank[0] + "\n", Preferences.DEBUG_ALGORITHM);
	            Preferences.debug("DIRECTION\n", Preferences.DEBUG_ALGORITHM);
	
	            for (i = 0; i < param; i++) {
	                Preferences.debug(dx[i] + "\n", Preferences.DEBUG_ALGORITHM);
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
                    Preferences.debug("Hessian matrix not positive definite\n", Preferences.DEBUG_ALGORITHM);
                    Preferences.debug("Try Gauss-Newton\n", Preferences.DEBUG_ALGORITHM);
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
            stepuc(prank);

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
            evreuc(prank);

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
    private void minrm(DoubleDouble[] v2, DoubleDouble alfmin, DoubleDouble alfmax, DoubleDouble xmin,
    		           DoubleDouble x[], DoubleDouble px[], DoubleDouble y[], DoubleDouble py[]) {
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
        DoubleDouble pprim, pbiss;
        DoubleDouble d, errorVar, h0, dm, hm, x0, eps;

        eps = DoubleDouble.valueOf(1.E-04);

        // COMPUTE NORMS AND SCALAR PRODUCTS

        minrm1(v2);
        pprim = pol3(xmin);
        pbiss = (((((DoubleDouble.valueOf(6.0)).multiply(xmin)).multiply(xmin)).multiply(v2norm)).multiply(v2norm));
        pbiss = pbiss.add(((DoubleDouble.valueOf(6.0)).multiply(xmin)).multiply(scv1v2));
        pbiss = pbiss.add((DoubleDouble.valueOf(2.0)).multiply(scv0v2));
        pbiss = pbiss.add(v1norm.multiply(v1norm));
        h0 = (pprim.divide(pbiss)).abs();
        dm = (DoubleDouble.valueOf(6.0)).multiply(scv1v2);
        dm = dm.add((((DoubleDouble.valueOf(12.0)).multiply(xmin)).multiply(v2norm)).multiply(v2norm));
        dm = dm.abs();
        dm = dm.add((((DoubleDouble.valueOf(24.0)).multiply(h0)).multiply(v2norm)).multiply(v2norm));

        // DETERMINE IF DP(X)=0 SHOULD BE SOLVED BY USING NEWTONS METHOD

        hm = h0.max(DoubleDouble.valueOf(1.0));

        if (pbiss.le(((DoubleDouble.valueOf(20.0)).multiply(hm)).multiply(dm))) {

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

            if (deltaMinrm.ge(DoubleDouble.valueOf(0.0))) {

                // DELTA >= 0 ONE INTERESTING ROOT.X

                oner(x);
                y[0] = x[0];
                // MAKE THE MINIMUM POINT alfkp1 LIE IN THE INTERVAL
                // (ALFMIN,ALFMAX) AND EVALUATE F(alfkp1) AT THE MINIMUM POINT

                x[0] = x[0].min(alfmax);
                x[0] = x[0].max(alfmin);
                px[0] = pol4(v2, x[0]);
                y[0] = y[0].min(alfmax);
                y[0] = y[0].max(alfmin);
                if (deltaMinrm.ge(DoubleDouble.valueOf(0.0))) {
                    y[0] = x[0];
                    py[0] = px[0];
                }

                return;
            } // if (deltaMinrm >= 0.0)

            // DELTA < 0 TWO INTERESTING ROOTS.Y AND Z, Y < Z
            twor();

            // CHOOSE alfkp1 = x1Minrm OR x2Minrm OR x3Minrm

            choose(xmin, v2, x, y, py);
            // MAKE THE MINIMUM POINT alfkp1 LIE IN THE INTERVALL
            // (ALFMIN,ALFMAX) AND EVALUATE F(alfkp1) AT THE MINIMUM POINT

            x[0] = x[0].min(alfmax);
            x[0] = x[0].max(alfmin);
            px[0] = pol4(v2, x[0]);
            y[0] = y[0].min(alfmax);
            y[0] = y[0].max(alfmin);

            if (deltaMinrm.ge(DoubleDouble.valueOf(0.0))) {
                y[0] = x[0];
                py[0] = px[0];
            }

            return;
        } // if (pbiss <= 20.0*hm*dm)

        deltaMinrm = DoubleDouble.valueOf(1.0);

        // ITERATE USING NEWTONS METHOD

        k = 0;
        x0 = xmin;

        do {
            pprim = pol3(x0);
            pbiss = (((((DoubleDouble.valueOf(6.0)).multiply(x0)).multiply(x0)).multiply(v2norm)).multiply(v2norm)).add
                      ((((DoubleDouble.valueOf(6.0)).multiply(x0)).multiply(scv1v2)).add
                         (((DoubleDouble.valueOf(2.0)).multiply(scv0v2)).add
                         (v1norm.multiply(v1norm))));
            d = (pprim.negate()).divide(pbiss);
            x[0] = x0.add(d);
            errorVar = ((((DoubleDouble.valueOf(2.0)).multiply(dm)).multiply(d)).multiply(d)).divide(pbiss.abs());
            x0 = x[0];
            k++;
        } while ((errorVar.gt(eps)) && (k < 3));

        y[0] = x[0];

        // MAKE THE MINIMUM POINT alfkp1 LIE IN THE INTERVALL
        // (ALFMIN,ALFMAX) AND EVALUATE F(alfkp1) AT THE MINIMUM POINT

        x[0] = x[0].min(alfmax);
        x[0] = x[0].max(alfmin);
        px[0] = pol4(v2, x[0]);
        y[0] = y[0].min(alfmax);
        y[0] = y[0].max(alfmin);

        if (deltaMinrm.ge(DoubleDouble.valueOf(0.0))) {
            y[0] = x[0];
            py[0] = px[0];
        }

        return;
    }

    /**
     * DOCUMENT ME!
     *
     * @param  v2  DOCUMENT ME!
     */
    private void minrm1(DoubleDouble[] v2) {
        // COMPUTE THE EUCLIDEAN NORM OF THE nPts-DIMENSIONAL VECTORS
        // residuals, v, and v2

        DoubleDouble sc1, sc2, sc3;
        int i;

        v0norm = dnrm2(nPts, residuals, 1);
        v1norm = dnrm2(nPts, v, 1);
        v2norm = dnrm2(nPts, v2, 1);

        // SCALE THE VECTORS

        if (!(v0norm.equals(DoubleDouble.valueOf(0.0)))) {
            scalv(residuals, v0norm, nPts);
        }

        if (!(v1norm.equals(DoubleDouble.valueOf(0.0)))) {
            scalv(v, v1norm, nPts);
        }

        if (!(v2norm.equals(DoubleDouble.valueOf(0.0)))) {
            scalv(v2, v2norm, nPts);
        }

        // COMPUTE THE SCALAR PRODUCTS residuals(T)*V, residuals(T)*v2,
        // v(T)*v2

        sc1 = DoubleDouble.valueOf(0.0);
        sc2 = DoubleDouble.valueOf(0.0);
        sc3 = DoubleDouble.valueOf(0.0);

        for (i = 0; i < nPts; i++) {
            sc1 = sc1.add(residuals[i].multiply(v[i]));
            sc2 = sc2.add(residuals[i].multiply(v2[i]));
            sc3 = sc3.add(v[i].multiply(v2[i]));
        }

        scv0v1 = (sc1.multiply(v0norm)).multiply(v1norm);
        scv0v2 = (sc2.multiply(v0norm)).multiply(v2norm);
        scv1v2 = (sc3.multiply(v1norm)).multiply(v2norm);

        // RESCALE THE VECTORS

        if (!(v0norm.equals(DoubleDouble.valueOf(0.0)))) {
            scalv(residuals, v0norm.reciprocal(), nPts);
        }

        if (!(v1norm.equals(DoubleDouble.valueOf(0.0)))) {
            scalv(v, v1norm.reciprocal(), nPts);
        }

        if (!(v2norm.equals(DoubleDouble.valueOf(0.0)))) {
            scalv(v2, v2norm.reciprocal(), nPts);
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

        DoubleDouble a1, a2, a3;
        DoubleDouble temp, temp2;

        a1 = (((DoubleDouble.valueOf(1.5)).multiply(scv1v2)).divide(v2norm)).divide(v2norm);
        temp = v1norm.divide(v2norm);
        a2 = (DoubleDouble.valueOf(0.5)).multiply((temp.multiply(temp)).add
        		(((scv0v2.divide(v2norm)).divide(v2norm)).multiply(DoubleDouble.valueOf(2.0))));
        a3 = (((DoubleDouble.valueOf(0.5)).multiply(scv0v1)).divide(v2norm)).divide(v2norm);
        pMinrm = a2.subtract((a1.multiply(a1)).divide(DoubleDouble.valueOf(3.0)));
        qMinrm = a3.subtract((a1.multiply(a2)).divide(DoubleDouble.valueOf(3.0))).add
                    (((((DoubleDouble.valueOf(2.0)).multiply(a1)).multiply(a1)).multiply(a1)).divide(DoubleDouble.valueOf(27.0)));
        temp = qMinrm.divide(DoubleDouble.valueOf(2.0));
        temp2 = pMinrm.divide(DoubleDouble.valueOf(3.0));
        deltaMinrm = (temp.multiply(temp)).add((temp2.multiply(temp2)).multiply(temp2));
        a1div3 = a1.divide(DoubleDouble.valueOf(3.0));

        return;
    }

    /**
     * DOCUMENT ME!
     *
     * @param  phikm2  DOCUMENT ME!
     * @param  pmax    DOCUMENT ME!
     */
    private void minrn(DoubleDouble x, DoubleDouble fx, DoubleDouble w, DoubleDouble fw, DoubleDouble v,
    		           DoubleDouble fv, DoubleDouble alfmin, DoubleDouble alfmax, 
    		           DoubleDouble pmax, DoubleDouble[] u , DoubleDouble[] pu) {
        // PROVIDED THE POINTS alfk, alfkm2a AND alfkm1a ARE NOT TOO CLOSE, THE QUADRATIC PASSING THROUGH (alfk,phik),
        // (alfkm2a,phikm2) AND (alfkm1a,phikm1) IS DETERMINED. alfkp1 = THE MINIMUM POINT OF THIS QUADRATIC. pk = THE
        // VALUE OF THE QUADRATIC AT alfkp1

        DoubleDouble eps, t1, t2, t3;

        eps = (srelpr.sqrt()).divide(pmax);
        u[0] = x;
        pu[0] = fx;

        if ((((v.subtract(x)).abs()).lt(eps)) || (((w.subtract(x)).abs()).lt(eps)) ||
                (((w.subtract(v)).abs()).lt(eps))) {
            return;
        }

        quamin(x, fx, w, fw, v, fv, u);
        u[0] = u[0].min(alfmax);
        u[0] = u[0].max(alfmin);
        t1 = ((((u[0].subtract(x)).multiply(u[0].subtract(v))).multiply(fw)).divide(w.subtract(x))).divide(w.subtract(v));
        t2 = ((((u[0].subtract(w)).multiply(u[0].subtract(v))).multiply(fx)).divide(x.subtract(w))).divide(x.subtract(v));
        t3 = ((((u[0].subtract(w)).multiply(u[0].subtract(x))).multiply(fv)).divide(v.subtract(x))).divide(v.subtract(w));
        pu[0] = (t1.add(t2)).add(t3);

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
        int newuncCtrl[] = new int[1];;

        newuncCtrl[0] = 2;

        // IF CTRL=2 ON ENTRY TO fitToFunction THE JACOBIAN MATRIX IS REQUESTED.
        // HOWEVER, IF THE ANALYTICAL JACOBIAN IS NOT AVAILABLE THE USER
        // SIGNALS THAT BY SETTING CTRL TO ZERO ON RETURN

        ctrlMat[0] = newuncCtrl[0];
        if (testMode) {
        	fitToTestFunction(a, residuals, covarMat);
        }
        else {
            fitToFunction(a, residuals, covarMat);
        }
        newuncCtrl[0] = ctrlMat[0];

        if (newuncCtrl[0] == 2) {
            return;
        }

        if (newuncCtrl[0] < -10) {
            errorStatus = newuncCtrl[0];

            return;
        }

        // COMPUTE THE JACOBIAN USING FORWARD DIFFERENCES

        if (newuncCtrl[0] == 0) {
            jacdif(newuncCtrl);
        }

        if (newuncCtrl[0] < -10) {
            errorStatus = newuncCtrl[0];

            return;
        }

        funcEval = funcEval + param;
        jacobianEval = jacobianEval + param;

        return;
    }

    /**
     * DOCUMENT ME!
     */
    private void oner(DoubleDouble x[]) {
        // COMPUTE THE ROOT OF A THIRD DEGREE POLYNOMIAL WHEN THERE
        // IS ONLY ONE REAL ROOT

        DoubleDouble arg1, arg2, a3rd, sqd, s1, s2, t;

        sqd = deltaMinrm.sqrt();
        arg1 = ((qMinrm.negate()).divide(DoubleDouble.valueOf(2.0))).add(sqd);

        if (arg1.ge(DoubleDouble.valueOf(0.0))) {
            s1 = DoubleDouble.valueOf(1.0);
        } else {
            s1 = DoubleDouble.valueOf(-1.0);
        }

        arg2 = ((qMinrm.negate()).divide(DoubleDouble.valueOf(2.0))).subtract(sqd);

        if (arg2.ge(DoubleDouble.valueOf(0.0))) {
            s2 = DoubleDouble.valueOf(1.0);
        } else {
            s2 = DoubleDouble.valueOf(-1.0);
        }

        a3rd = (DoubleDouble.valueOf(1.0)).divide(DoubleDouble.valueOf(3.0));
        t = (s1.multiply((arg1.abs()).pow(a3rd))).add(s2.multiply((arg2.abs()).pow(a3rd)));
        x[0] = t.subtract(a1div3);

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
        // constraintAct INTEGER SCALAR CONTAINING THE NO. OF ACTIVE CONSTRAINTS
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
        DoubleDouble reduc, ac, pred;
        int i;

        if (restart) {
            return;
        }

        if (ifree == 5) {
            return;
        }

        speed = DoubleDouble.valueOf(0.0);
        itno = iters - 1;

        if ((itno != 0) && (!(betkm2.equals(DoubleDouble.valueOf(0.0))))) {
            speed = betkm1.divide(betkm2);
        }

        reduc = fsqkm1.subtract((DoubleDouble.valueOf(2.0)).multiply(phi));
        ac = (DoubleDouble.valueOf(1.0)).min(aupkm1);
        pred = (ac.multiply((DoubleDouble.valueOf(2.0)).subtract(ac))).multiply(d1km1);
        if (outputMes) {
	        Preferences.debug("Collected information for iteration steps\n", Preferences.DEBUG_ALGORITHM);
	        Preferences.debug("itno = " + itno + "\n", Preferences.DEBUG_ALGORITHM);
	        Preferences.debug("fsqkm1 = " + fsqkm1 + "\n", Preferences.DEBUG_ALGORITHM);
	        Preferences.debug("gnorm = " + gnorm + "\n", Preferences.DEBUG_ALGORITHM);
	        Preferences.debug("dxnkm1 = " + dxnkm1 + "\n", Preferences.DEBUG_ALGORITHM);
	        Preferences.debug("kodkm1 = " + kodkm1 + "\n", Preferences.DEBUG_ALGORITHM);
	        Preferences.debug("rngkm1 = " + rngkm1 + "\n", Preferences.DEBUG_ALGORITHM);
	        Preferences.debug("alfkm1 = " + alfkm1 + "\n", Preferences.DEBUG_ALGORITHM);
	        Preferences.debug("eval = " + eval + "\n", Preferences.DEBUG_ALGORITHM);
	        Preferences.debug("pred = " + pred + "\n", Preferences.DEBUG_ALGORITHM);
	        Preferences.debug("speed = " + speed + "\n", Preferences.DEBUG_ALGORITHM);
	        Preferences.debug("phi = " + phi + "\n", Preferences.DEBUG_ALGORITHM);
	        Preferences.debug("reduc = " + reduc + "\n", Preferences.DEBUG_ALGORITHM);
	
	        if (constraintAct > 0) {
	
	            for (i = 0; i < param; i++) {
	                Preferences.debug("aset[" + i + "] = " + aset[i] + "\n", Preferences.DEBUG_ALGORITHM);
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
    private void pivec(DoubleDouble[] work) {
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
    private DoubleDouble pol3(DoubleDouble x) {
        // EVALUATE A 3:RD DEGREE POLYNOMIAL
        // A3*X**3+A2*X**2+A1*X+A0   AT X
        // WHERE  A0 = SCV0V1
        // A1 = 2*SCV0V2+V1NORM**2
        // A2 = 3*SCV1V2
        // A3 = 2*V2NORM**2

        DoubleDouble fn_val;
        fn_val = scv0v1;
        fn_val = fn_val.add((x.multiply(v1norm)).multiply(v1norm));
        fn_val = fn_val.add(((DoubleDouble.valueOf(2.00)).multiply(x)).multiply(scv0v2));
        fn_val = fn_val.add(((x.multiply(x)).multiply(DoubleDouble.valueOf(3.00))).multiply(scv1v2));
        fn_val = fn_val.add(((((x.multiply(x)).multiply(x)).multiply(v2norm)).multiply(v2norm)).multiply(DoubleDouble.valueOf(2.0)));
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
    private DoubleDouble pol4(DoubleDouble[] v2, DoubleDouble x) {
        DoubleDouble p, s;
        int i;
        DoubleDouble fn_val;

        // EVALUATE THE 4:TH DEGREE POLYNOMIAL || V2*X**2+V*X+residuals ||**2 AT X
        s = DoubleDouble.valueOf(0.0);

        for (i = 0; i < nPts; i++) {
            p = residuals[i].add(x.multiply(v[i]));
            p = p.add((x.multiply(x)).multiply(v2[i]));
            s = s.add(p.multiply(p));
        }

        fn_val = (DoubleDouble.valueOf(0.5)).multiply(s);

        return fn_val;
    }

    /**
     * DOCUMENT ME!
     *
     * @param  work    DOCUMENT ME!
     * @param  mindim  DOCUMENT ME!
     */
    private void pregn(DoubleDouble[] work, int mindim, int prank[], int dim[]) {
        // GN-STEP IN PREVIOUS STEP
        // TAKE rank AS THE LARGEST k (mindim<=k<=pseudoRank-1) FOR WHICH
        // w1[k-1] < smax*sn AND work[-1k] > rmin*bn
        // IF NO SUCH k EXISTS TAKE rank =pseudoRank-1 PROVIDED
        // (pseudoRank-1) >= mindim

        int i, k, m1;

        smax = DoubleDouble.valueOf(0.2);
        rmin = DoubleDouble.valueOf(0.5);
        m1 = prank[0] - 1;
        k = prank[0];

        if (mindim > m1) {
            dim[0] = k;

            return;
        }

        for (i = mindim; i <= m1; i++) {
            k = m1 - i + mindim;

            if ((w1[k - 1].lt(smax.multiply(sn))) && (work[k - 1].gt(rmin.multiply(bn)))) {
                dim[0] = k;

                return;
            }
        } // for (i = mindim; i <= m1; i++)

        dim[0] = Math.max(mindim, prank[0] - 1);

        return;
    }

    /**
     * DOCUMENT ME!
     *
     * @param  work  DOCUMENT ME!
     */
    private void presub(DoubleDouble[] work, int prank[], int rank[]) {
        // SUBSPACE MINIMIZATION IN LATEST STEP

        int i, i1;
        DoubleDouble pgress;

        stepb = DoubleDouble.valueOf(0.2);
        pgb1 = DoubleDouble.valueOf(0.3);
        pgb2 = DoubleDouble.valueOf(0.1);
        predb = DoubleDouble.valueOf(0.7);
        rlenb = DoubleDouble.valueOf(2.0);

        // IF THE LATEST STEP WAS FAIRLY GOOD THE DIMENSION MUST NOT BE DECREASED

        pgress = (fsqkm1.subtract(fsum)).sqrt();

        if ((alfkm1.lt(stepb)) && (pgress.le(pgb1.multiply(d1km1.sqrt()))) && (pgress.le(pgb2.multiply(betkm1)))) {

            // A BAD STEP

            rank[0] = Math.max(1, rngkm1 - 1);

            if ((rngkm1 > 1) && (work[rank[0] - 1].gt(rabs.multiply(bn)))) {
                return;
            }
        }

        rank[0] = rngkm1;

        if ((work[rank[0] - 1].gt(predb.multiply(bn))) && ((rlenb.multiply(w1[rank[0] - 1])).lt(w1[rank[0]]))) {
            return;
        }

        i1 = rngkm1 + 1;

        for (i = i1; i <= prank[0]; i++) {
            rank[0] = i;

            if (work[i - 1].gt(predb.multiply(bn))) {
                break;
            }
        } // for (i = i1; i <= prank[0]; i++)

        return;
    }

    /**
     * DOCUMENT ME!
     *
     * @param  phikm2  DOCUMENT ME!
     */
    private void quamin(DoubleDouble x, DoubleDouble fx, DoubleDouble w, DoubleDouble fw,
    		            DoubleDouble v, DoubleDouble fv, DoubleDouble u[]) {
        // COMPUTE THE MINIMUM POINT U OF A QUADRATIC POLYNOMIAL PASSING
        // THROUGH (alfkm2a,phikm2), (alfkm1a,phikm1), and (alfk,phik)

        DoubleDouble d1, d2, s, q;

        d1 = fv.subtract(fx);
        d2 = fw.subtract(fx);
        s = (((w.subtract(x)).multiply(w.subtract(x))).multiply(d1));
        s = s.subtract(((v.subtract(x)).multiply(v.subtract(x))).multiply(d2));
        q = (DoubleDouble.valueOf(2.0)).multiply(((v.subtract(x)).multiply(d2)).subtract((w.subtract(x)).multiply(d1)));
        u[0] = x.subtract(s.divide(q));

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
    	    Preferences.debug("In reavuc: alpha = " + alpha + "\n", Preferences.DEBUG_ALGORITHM);
            Preferences.debug("In reavuc: alphup = " + alphup + "\n", Preferences.DEBUG_ALGORITHM);
        }

        if ((lattry == 0) && (bestpg.gt(DoubleDouble.valueOf(0.0)))) {
            return;
        }

        if ((lattry == 0) && (bestpg.le(DoubleDouble.valueOf(0.0)))) {
            errorStatus = -5;

            return;
        }

        if ((errorStatus == -1) || (errorStatus <= -3)) {
            return;
        }

        if ((errorStatus != -2) && (phi.ge((DoubleDouble.valueOf(0.5)).multiply(fsum)))) {
            errorStatus = -5;

            return;
        }

        if (alpha.le(alphup.divide(DoubleDouble.valueOf(3000.0)))) {
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
    private void reduc(DoubleDouble alf[], DoubleDouble phialf[], DoubleDouble alfk, DoubleDouble pk,
    		           DoubleDouble diff, DoubleDouble eta, DoubleDouble[] gmod, DoubleDouble[] fnew,
    		           int k[], DoubleDouble phik[], boolean reduce[]) {
        // REDUCE IS SET TO TRUE IF ESSENTIAL REDUCTION OF THE OBJECTIVE FUNCTION IS LIKELY. OTHERWISE REDUCE IS SET TO
        // FALSE

        DoubleDouble c1, delta;
        int i;
        delta = DoubleDouble.valueOf(0.2);
        int reducCtrl[] = new int[1];

        c1 = (DoubleDouble.valueOf(nPts)).multiply(srelpr.sqrt());

        if (!(((phialf[0].subtract(pk)).gt(eta.multiply(diff))) ||
        	  ((pk.add(c1)).lt(delta.multiply(phialf[0]))))) {

            for (i = 0; i < nPts; i++) {
                residuals[i] = fnew[i];
            }

            reduce[0] = false;

            return;
        } // if (!((phikm1-pk > eta*diff) || (pk+c1 < delta*phikm1)))

        for (i = 0; i < nPts; i++) {
            residuals[i] = fnew[i];
        }

        reducCtrl[0] = -1;
        fsumsq(alfk, gmod, fnew, phik, reducCtrl);
        if (outputMes) {
            Preferences.debug("In REDUC: phik[0] = " + phik[0] + " alfk = " + alfk + "\n", Preferences.DEBUG_ALGORITHM);
        }

        if (reducCtrl[0] < -10) {
            k[0] = reducCtrl[0];
        }

        if (k[0] < 0) {
            return;
        }

        k[0]++;
        reduce[0] = true;

        if (((phialf[0].subtract(phik[0])).gt(eta.multiply(diff))) ||
             ((phik[0]).lt(delta.multiply(phialf[0])))) {
            return;
        }

        // TERMINATE BUT CHOOSE THE BEST POINT OUT OF alfkm1a AND ALFK

        if ((phialf[0]).le(phik[0])) {
            reduce[0] = false;

            return;
        }

        alf[0] = alfk;
        phialf[0] = phik[0];

        for (i = 0; i < nPts; i++) {
            residuals[i] = fnew[i];
        }

        reduce[0] = false;

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

        DoubleDouble temp, frac, oldFrac;
        boolean loop = true;
        frac = DoubleDouble.valueOf(1.0);

        while (loop) {
        	oldFrac = frac;
            frac = (DoubleDouble.valueOf(0.5)).multiply(frac);
            if (frac.equals(DoubleDouble.valueOf(0.0))) {
            	srelpr = oldFrac;
            	return;
            }
            temp = frac.add(DoubleDouble.valueOf(1.0));

            if (temp.equals(DoubleDouble.valueOf(1.0))) {
                break;
            }
        } // while (loop)

        srelpr = (DoubleDouble.valueOf(2.0)).multiply(frac);

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
            dx[j] = DoubleDouble.valueOf(0.0);

            for (i = 0; i <= j; i++) {
                dx[j] = dx[j].subtract(covarMat[i][j].multiply(w1[i]));
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
    private void scalv(DoubleDouble[] v, DoubleDouble factor, int n) {

        // Form the new contents of vector v as
        // v[i] = v[i]/factor for i = 0,...,n-1
        int i;

        for (i = 0; i < n; i++) {
            v[i] = v[i].divide(factor);
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
        DoubleDouble colj;
        DoubleDouble[] covarPart = new DoubleDouble[nPts];

        cnorm = DoubleDouble.valueOf(0.0);

        for (j = 0; j < param; j++) {

            for (k = 0; k < nPts; k++) {
                covarPart[k] = covarMat[k][j];
            }

            colj = dnrm2(nPts, covarPart, 1);

            if (colj.gt(cnorm)) {
                cnorm = colj;
            }

            if (!internalScaling) {
                continue;
            }

            if (colj.equals(DoubleDouble.valueOf(0.0))) {
                colj = DoubleDouble.valueOf(1.0);
            }

            for (k = 0; k < nPts; k++) {
                covarMat[k][j] = covarMat[k][j].divide(colj);
            }

            diag[j] = (DoubleDouble.valueOf(1.0)).divide(colj);
        } // for (j = 0; j < param; j++)

        if (cnorm.equals(DoubleDouble.valueOf(0.0))) {
            cnorm = DoubleDouble.valueOf(1.0);
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
         * INFO   CONTAINS THE (INDEX+1) OF THE LAST POSITIVE DIAGONAL    ELEMENT OF THE CHOLESKY FACTOR.
         *
         * FOR POSITIVE DEFINITE MATRICES INFO = nn IS THE NORMAL RETURN. FOR PIVOTING WITH POSITIVE SEMIDEFINITE
         * MATRICES INFO WILL IN GENERAL BE LESS THAN nn.  HOWEVER, INFO MAY BE GREATER THAN THE RANK OF covarMat,
         * SINCE ROUNDING ERROR CAN CAUSE AN OTHERWISE ZERO ELEMENT TO BE POSITIVE. INDEFINITE SYSTEMS WILL ALWAYS
         * CAUSE INFO TO BE LESS THAN nn. */

        int pu, pl, plp1, j, jp, jt, k, kb, km1, kp1, l, maxl;
        int i;
        DoubleDouble temp;
        DoubleDouble maxdia;
        boolean swapk, negk;

        pl = 0;
        pu = -1;
        info = nn;

        if (job != 0) {

            // PIVOTING HAS BEEN REQUESTED.
            // REARRANGE THE THE ELEMENTS ACCORDING TO JPVT.

            for (k = 0; k < nn; k++) {
                swapk = jpvt[k] > 0;
                negk = jpvt[k] < 0;
                jpvt[k] = k;

                if (negk) {
                    jpvt[k] = -jpvt[k];
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
                jpvt[pl] = k;
                pl++;
            } // for (k = 0; k < nn; k++)

            pu = nn - 1;

            for (kb = pl; kb < nn; kb++) {
                k = nn - 1 - kb + pl;

                if (jpvt[k] >= 0) {
                    continue;
                }

                jpvt[k] = -jpvt[k];

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
                jpvt[k] = jpvt[pu];
                jpvt[pu] = jt;

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

                    if (covarMat[l][l].gt(maxdia)) {
                        maxdia = covarMat[l][l];
                        maxl = l;
                    } // if (covarMat[l][l] > maxdia)
                } // for (l = kp1; l <= pu; l++)
            } // if ((k >= pl) && (k < pu))

            // QUIT IF THE PIVOT ELEMENT IS NOT POSITIVE.

            if (maxdia.le(DoubleDouble.valueOf(0.0))) {
                info = k;

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

            w0[k] = covarMat[k][k].sqrt();
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

                covarMat[k][j] = covarMat[k][j].divide(w0[k]);
                w0[j] = covarMat[k][j];
                temp = covarMat[k][j].negate();

                for (i = 0; i <= j-k-1; i++) {
                    covarMat[kp1+i][j] = covarMat[kp1+i][j].add(temp.multiply(w0[kp1+i]));
                } // for (i = 0; i <= j-k-1; i++)
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
        // constraintAct INTEGER SCALAR CONTAINING NO. OF ACTIVE CONSTRAINTS

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
        DoubleDouble[] dummy = new DoubleDouble[param];

        // INSTEAD OF SOLVING (1) WE SOLVE A TRANSFORMED SYSTEM
        // WHICH HAS param-constraintAct UNKNOWNS DY

        // I.E. SOLVE  T    T      nPts-1                           T     T
        // (R *R+E *D*(SIGMA(residuals *G ))*D*E)*DY = -(R :0)*Q *residuals  (2)
        // K=0           K  K

        // dx = D*E*DY  (THE LAST constraintAct ELEMENTS OF DY EQUAL TO ZERO)

        // FORM RIGHT HAND SIDE OF SYSTEM (2)
        // I.E.          T     T
        // FORM  -(R :0)*Q *residuals   AND STORE IN dx
        // T
        // FIRST FORM Q *residuals BY USING SQRSL AND STORE IN w1

        nn = param - constraintAct;
        sqrsl(covarMat, nPts, nn, w0, residuals, dummy, w1, dummy, dummy, dummy, 1000);

        // T
        // FORM  -(R :0)*w1 AND STORE IN dx

        rtrw1(nn);
        // T
        // FORM  covarMat = R *R

        jtrj(nn);
        /*Preferences.debug("R(tr)*R\n", Preferences.DEBUG_ALGORITHM);
         * for (i = 0; i < nn; i++) { for (j = 0; j < nn; j++) {     Preferences.debug(covarMat[i][j] + " ", 
           Preferences.DEBUG_ALGORITHM); }
         * Preferences.debug("\n", Preferences.DEBUG_ALGORITHM);}*/

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
                    gmat[i][j] = diag[i].multiply(gmat[i][j]);
                    gmat[j][i] = gmat[j][i].multiply(diag[i]);
                }
            }

            if (outputMes) {
	            Preferences.debug("The nonlinear part.Only the upper triangular.\n", Preferences.DEBUG_ALGORITHM);
	            Preferences.debug("Row - wize written.I.e.the first 4values make\n", Preferences.DEBUG_ALGORITHM);
	            Preferences.debug("up the 1st row.The following 3 values make up\n", Preferences.DEBUG_ALGORITHM);
	            Preferences.debug("the 2nd row starting at the diagonal elem.\n", Preferences.DEBUG_ALGORITHM);
            }

        } // if (internalScaling)

        // DO THE PIVOTING OF COLUMNS AND ROWS IN MATRIX gmat
        // AND ADD TO MATRIX covarMat
        for (i = 0; i < nn; i++) {
            l = pivit[i];

            for (j = i; j < nn; j++) {
                k = pivit[j];
                covarMat[i][j] = covarMat[i][j].add(gmat[l][k]);
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

        if (constraintAct != 0) {

            for (i = 0; i < constraintAct; i++) {
                j = nn + i;
                dx[j] = DoubleDouble.valueOf(0.0);
            }
        } // if (constraintAct != 0)

        pivec(w0);

        if (!internalScaling) {
            return;
        }

        for (i = 0; i < param; i++) {
            dx[i] = diag[i].multiply(dx[i]);
        }

        return;
    }

    /**
     * DOCUMENT ME!
     *
     * @param  tau  DOCUMENT ME!
     */
    private void soliuc(DoubleDouble tau, int prank[]) {

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

        // constraintAct number of active constraints
        // Equals -number of active constraints when one is deleted in soliuc

        // pseudoRank scalar containing the pseudo rank of the matrix J
        // that was used to compute dx
        int i, j, jj, k, inds, kk, ii, mr, kmax;
        int lprank[] = new int[1];
        int[] ip = new int[1];
        DoubleDouble tol, d1max, d1new;
        DoubleDouble ymax = DoubleDouble.valueOf(0.0);
        DoubleDouble factor = DoubleDouble.valueOf(2.0);
        DoubleDouble[] scaqr = new DoubleDouble[1];
        DoubleDouble[] dummy = new DoubleDouble[1];
        DoubleDouble[] dykk = new DoubleDouble[1];
        DoubleDouble[] work = new DoubleDouble[param];
        DoubleDouble[][] work2 = new DoubleDouble[param][1];
        DoubleDouble[] w2Part;

        if (outputMes) {
	        Preferences.debug("Current point in soliuc\n", Preferences.DEBUG_ALGORITHM);
	
	        for (i = 0; i < param; i++) {
	            Preferences.debug(a[i] + "\n", Preferences.DEBUG_ALGORITHM);
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
            g[i] = DoubleDouble.valueOf(0.0);

            for (j = 0; j < nPts; j++) {
                g[i] = g[i].add(covarMat[j][i].multiply(residuals[j]));
            }
        }

        // Scale the Jacobian if internalScaling is true
        // cnorm := length of the longest column in the Jacobian
        scaunc();
        gnorm = dnrm2(param, g, 1).divide(cnorm);
        if (outputMes) {
            Preferences.debug("cnorm = " + cnorm + "\n", Preferences.DEBUG_ALGORITHM);
            Preferences.debug("gnorm = " + gnorm + "\n", Preferences.DEBUG_ALGORITHM);
        }

        // MAKE A QR-DECOMPOSITION OF (POSSIBLY SCALED) JACOBIAN
        // I.E.                T
        // Q *J*D*E = (R)
        // (0)
        // ALSO DETERMINE PSEUDO RANK OF MATRIX J*D

        tol = ((DoubleDouble.valueOf(param)).sqrt()).multiply(tau);
        triunc(tol, constraintAct,prank);

        // COMPUTE THE "PSEUDO INVERSE" SOLUTION (DX) BY USING pseudoRank AS
        // PSEUDO RANK OF MATRIX J*D
        // ALSO COMPUTE  V= J*DX
        // AND       D1SQS= PREDICTED REDUCTION IN THE OBJECTIVE
        // BETA = THE NORM OF ORTH. PROJ. OF residuals ONTO COLUMN
        // SPACE OF THE JACOBIAN J
        d1sqs = gndunc(internalScaling, prank, work);
        if (outputMes) {
	        Preferences.debug("The right hand side\n", Preferences.DEBUG_ALGORITHM);
	
	        for (i = 0; i < nPts; i++) {
	            Preferences.debug(w1[i] + "\n", Preferences.DEBUG_ALGORITHM);
	        }
        }

        beta = dnrm2(param - constraintAct, w1, 1);
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
	        Preferences.debug("constraintAct = " + constraintAct + "\n", Preferences.DEBUG_ALGORITHM);
	        Preferences.debug("prank[0] = " + prank[0] + "\n", Preferences.DEBUG_ALGORITHM);
	        Preferences.debug("imax = " + imax + "\n", Preferences.DEBUG_ALGORITHM);
        }

        if ((constraintAct == 0) || (imax != -1)) {
            return;
        }

        // Initialize
        ii = 0;
        kk = param - constraintAct;
        mr = param - kk;
        d1max = d1sqs;
        kmax = -1;

        for (i = 0; i < nPts; i++) {
            w1[i] = residuals[i].negate();
        }

        sqrsl(covarMat, nPts, param, w0, w1, dummy, w1, w2, dummy, dummy, 1000);

        // Try each bounded column to see what predicted reduction it gives

        inds = 0;

        for (i = 0; i < param; i++) {

            if (aset[i] == 0) {
                continue;
            }

            if (prank[0] < (param - constraintAct)) {
                k = 0;
                kk = 0;
                if (outputMes) {
                    Preferences.debug("Not full rank: prank[0] = " + prank[0] + "\n", Preferences.DEBUG_ALGORITHM);
                }
                inds = aset[i];
                aset[i] = 0;
                newunc();

                if (errorStatus != 0) {
                    System.exit(errorStatus);
                }

                triunc(tol, constraintAct - 1,lprank);
                if (outputMes) {
                    Preferences.debug("lprank[0] = " + lprank[0] + "\n", Preferences.DEBUG_ALGORITHM);
                }
                d1new = gndunc(false, lprank, work);
                if (outputMes) {
	                Preferences.debug("New search direction\n", Preferences.DEBUG_ALGORITHM);
	
	                for (jj = 0; jj < param; jj++) {
	                    Preferences.debug(dx[jj] + "\n", Preferences.DEBUG_ALGORITHM);
	                }
                }

                aset[i] = inds;

                if (((dx[i].multiply(DoubleDouble.valueOf(inds))).multiply(bu[i].subtract(bl[i]))).le(DoubleDouble.valueOf(0.0))) {
                    continue;
                }
            } // if (prank[0] < (param - constraintAct))
            else {
                k = param - constraintAct + ii;
                ii++;

                // T
                // Put w2 equal to -Q * residuals
                for (jj = 0; jj < nPts; jj++) {
                    w2[jj] = w1[jj];
                }

                // Zero the kth column below its (param - constraintAct)+1 element
                for (j = 0; j < param; j++) {
                    work[j] = covarMat[j][k];

                    if (j > k) {
                        work[j] = DoubleDouble.valueOf(0.0);
                    }
                } // for (j = 0; j < param; j++)

                if (k != kk) {
                    ip[0] = 1;

                    for (jj = 0; jj < mr; jj++) {
                        work2[jj][0] = work[kk + jj];
                    }

                    sqrdc(work2, mr, 1, scaqr, ip, 0);

                    // Transform w2 in the same way
                    w2Part = new DoubleDouble[w2.length - kk];

                    for (jj = 0; jj < (w2.length - kk); jj++) {
                        w2Part[jj] = w2[jj + kk];
                    }

                    sqrsl(work2, mr, 1, scaqr, w2Part, dummy, w2Part, dykk, dummy, dummy, 100);

                    for (jj = 0; jj < mr; jj++) {
                        work[kk + jj] = work2[jj][0];
                    }
                    
                    for (jj = 0; jj < (w2.length - kk); jj++) {
                        w2[jj + kk] = w2Part[jj];
                    }
                } // if (k != kk)

                // Determine the kkth element in dy
                if (outputMes) {
	                Preferences.debug("SOLIUC: work[kk] = " + work[kk] + "\n", Preferences.DEBUG_ALGORITHM);
	                Preferences.debug("SOLIUC:tol = " + tol + "\n", Preferences.DEBUG_ALGORITHM);
	                Preferences.debug("SOLIUC:covarMat[0][0] = " + covarMat[0][0] + "\n", Preferences.DEBUG_ALGORITHM);
                }

                if ((work[kk].abs()).le(tol.multiply(covarMat[0][0].abs()))) {
                    continue;
                }

                dykk[0] = w2[kk].divide(work[kk]);
                if (outputMes) {
	                Preferences.debug("In soliuc: dykk[0] = " + dykk[0] + "\n", Preferences.DEBUG_ALGORITHM);
	                Preferences.debug("i = " + i + "\n", Preferences.DEBUG_ALGORITHM);
	                Preferences.debug("aset[i] = " + aset[i] + "\n", Preferences.DEBUG_ALGORITHM);
	                Preferences.debug("bu[i] = " + bu[i] + "\n", Preferences.DEBUG_ALGORITHM);
	                Preferences.debug("bl[i] = " + bl[i] + "\n", Preferences.DEBUG_ALGORITHM);
                }

                if (((dykk[0].multiply(DoubleDouble.valueOf(aset[i]))).multiply(bu[i].subtract(bl[i]))).le(DoubleDouble.valueOf(0.0))) {
                    continue;
                }

                d1new = d1sqs.add(w2[kk].multiply(w2[kk]));
            } // else

            if (outputMes) {
                Preferences.debug("In soliuc: d1new = " + d1new + "\n", Preferences.DEBUG_ALGORITHM);
                Preferences.debug("In soliuc: d1sqs = " + d1sqs + "\n", Preferences.DEBUG_ALGORITHM);
            }

            if (d1new.lt(factor.multiply(d1sqs))) {
                continue;
            }

            if (outputMes) {
                Preferences.debug("Delete a fixed variable\n", Preferences.DEBUG_ALGORITHM);
            }

            if (d1new.lt(d1max)) {
                continue;
            }

            d1max = d1new;
            ymax = w2[kk];
            kmax = k;
            imax = i;
        } // for (i = 0; i < param; i++)

        if (prank[0] < (param - constraintAct)) {

            if (imax != -1) {
                ival = aset[imax];
                aset[imax] = 0;
                constraintAct--;
            } // if (imax != -1)

            newunc();
            triunc(tol, constraintAct,prank);
            d1sqs = gndunc(false, prank, work);
            beta = dnrm2(param - constraintAct, w1, 1);
            prekm1 = beta;
        } // if (prank[0] < (param - constraintAct))
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
                        work[j] = DoubleDouble.valueOf(0.0);
                    }
                } // for (j = 0; j < param; j++)

                for (jj = 0; jj < mr; jj++) {
                    work2[jj][0] = work[jj + kk];
                }

                sqrdc(work2, mr, 1, scaqr, ip, 0);
                w2[kk] = ymax;
            } // if (kmax != kk)
            // w2 := H*(D1)
            // ( 0)

            ip[0] = kk + 1;

            for (jj = ip[0]; jj < nPts; jj++) {
                w2[jj] = DoubleDouble.valueOf(0.0);
            }

            if (kmax != kk) {
                w2Part = new DoubleDouble[w2.length - kk];

                for (jj = 0; jj < (w2.length - kk); jj++) {
                    w2Part[jj] = w2[jj + kk];
                }

                sqrsl(work2, mr, 1, scaqr, w2Part, w2Part, dummy, dummy, dummy, dummy, 10000);
                
                for (jj = 0; jj < (w2.length - kk); jj++) {
                    w2[jj + kk] = w2Part[jj];
                }
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
            prank[0]++;
            constraintAct--;
            beta = dnrm2(param - constraintAct, w1, 1);
            prekm1 = beta;
            d1sqs = d1max;

            // Compute the augmented GN-direction

            for (jj = 0; jj <= kk; jj++) {
                dx[jj] = w1[jj];
            }

            dx[kk] = ymax;
            strsl(prank,dx);

            // back transform
            btrunc(prank,internalScaling, w2);
            if (outputMes) {
	            Preferences.debug("Full rank new search direction\n", Preferences.DEBUG_ALGORITHM);
	
	            for (i = 0; i < param; i++) {
	                Preferences.debug(dx[i] + "\n", Preferences.DEBUG_ALGORITHM);
	            }
            }
        } // else if (kmax != -1)

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

        DoubleDouble t;
        int j, k, kb;

        // SOLVE TRANS(R)*Y = dx

        for (k = 0; k < nn; k++) {
            t = DoubleDouble.valueOf(0.0);

            for (j = 0; j <= (k - 1); j++) {
                t = t.add(covarMat[j][k].multiply(dx[j]));
            }

            dx[k] = (dx[k].subtract(t)).divide(covarMat[k][k]);
        } // for (k = 0; k < nn; k++)

        // SOLVE R*X = Y

        for (kb = 1; kb <= nn; kb++) {
            k = nn - kb;
            dx[k] = dx[k].divide(covarMat[k][k]);
            t = dx[k].negate();

            for (j = 0; j <= (k - 1); j++) {
                dx[j] = dx[j].add(t.multiply(covarMat[j][k]));
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
    private void sqrdc(DoubleDouble[][] x, int n, int p, DoubleDouble[] qraux, int[] jpvt, int job) {
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
        DoubleDouble maxnrm, tt;
        DoubleDouble nrmxl, t;
        DoubleDouble[] work = new DoubleDouble[p];
        DoubleDouble temp;
        boolean negj, swapj;
        DoubleDouble[] tempMat = new DoubleDouble[n];

        pl = 1;
        pu = 0;

        if (job != 0) {

            // PIVOTING HAS BEEN REQUESTED.REARRANGE THE COLUMNS ACCORDING TO JPVT.

            for (j = 1; j <= p; j++) {
                swapj = jpvt[j-1] > 0;
                negj = jpvt[j-1] < 0;
                jpvt[j-1] = j;

                if (negj) {
                	jpvt[j-1] = -j;
                }

                if (!swapj) {
                    continue;
                }

                if (j != pl) {

                    for (jj = 0; jj < n; jj++) {
                        temp = x[jj][pl-1];
                        x[jj][pl-1] = x[jj][j-1];
                        x[jj][j-1] = temp;
                    }
                }

                jpvt[j-1] = jpvt[pl-1];
                jpvt[pl-1] = j;
                pl++;
            } // for (j = 0; j < p; j++)

            pu = p;

            for (jj = 1; jj <= p; jj++) {
                j = p - jj + 1;

                if (jpvt[j-1] >= 0) {
                	continue;
                }
                jpvt[j-1] = -jpvt[j-1];

                if (j != pu) {

                    for (i = 0; i < n; i++) {
                        temp = x[i][pu-1];
                        x[i][pu-1] = x[i][j-1];
                        x[i][j-1] = temp;
                    }

                    jp = jpvt[pu-1];
                    jpvt[pu-1] = jpvt[j-1];
                    jpvt[j-1] = jp;
                } // if (j != pu)

                pu--;
            } // for (jj = 1; jj <= p; jj++)

            for (j = 0; j < p; j++) {
            	jpvt[j] = jpvt[j] - 1;
            }

        } // if (job != 0)
        
        // COMPUTE THE NORMS OF THE FREE COLUMNS.
        
        for (j = pl; j <= pu; j++) {

	        for (i = 0; i < n; i++) {
	            tempMat[i] = x[i][j-1];
	        }
	
	        qraux[j-1] = dnrm2(n, tempMat, 1);
	        work[j-1] = qraux[j-1];
        } // for (j = pl; j <= pu; j++)

        // PERFORM THE HOUSEHOLDER REDUCTION OF X.

        lup = Math.min(n, p);

        for (l = 1; l <= lup; l++) {

            if ((l >= pl) && (l < pu)) {

                // LOCATE THE COLUMN OF LARGEST NORM AND BRING IT INTO THE PIVOT POSITION.

                maxnrm = DoubleDouble.valueOf(0.0);
                maxj = l;

                for (j = l; j <= pu; j++) {

                    if (qraux[j-1].le(maxnrm)) {
                        continue;
                    }

                    maxnrm = qraux[j-1];
                    maxj = j;
                } // for (j = l; j <= pu; j++)

                if (maxj != l) {

                    for (i = 0; i < n; i++) {
                        temp = x[i][l-1];
                        x[i][l-1] = x[i][maxj-1];
                        x[i][maxj-1] = temp;
                    }

                    qraux[maxj-1] = qraux[l-1];
                    work[maxj-1] = work[l-1];
                    jp = jpvt[maxj-1];
                    jpvt[maxj-1] = jpvt[l-1];
                    jpvt[l-1] = jp;
                } // if (maxj != l)
            } // if ((l >= pl) && (l < pu))

            qraux[l-1] = DoubleDouble.valueOf(0.0);

            if (l == n) {
                continue;
            }

            // COMPUTE THE HOUSEHOLDER TRANSFORMATION FOR COLUMN L.
            for (i = 0; i < (n - l + 1); i++) {
                tempMat[i] = x[l + i - 1][l - 1];
            }

            nrmxl = dnrm2(n - l + 1, tempMat, 1);

            if (nrmxl.equals(DoubleDouble.valueOf(0.0))) {
                continue;
            }

            if (!(x[l-1][l-1].equals(DoubleDouble.valueOf(0.0)))) {

                if (x[l-1][l-1].ge(DoubleDouble.valueOf(0.0))) {
                    nrmxl = nrmxl.abs();
                } else {
                    nrmxl = (nrmxl.abs()).negate();
                }
            }

            for (i = l; i <= n; i++) {
                x[i-1][l-1] = x[i-1][l-1].divide(nrmxl);
            }

            x[l-1][l-1] = x[l-1][l-1].add(DoubleDouble.valueOf(1.0));

            // APPLY THE TRANSFORMATION TO THE REMAINING COLUMNS,
            // UPDATING THE NORMS.

            lp1 = l + 1;

            for (j = lp1; j <= p; j++) {
                t = DoubleDouble.valueOf(0.0);

                for (i = l; i <= n; i++) {
                    t = t.subtract(x[i-1][l-1].multiply(x[i-1][j-1]));
                }
                t = t.divide(x[l-1][l-1]);

                for (i = l; i <= n; i++) {
                    x[i-1][j-1] = x[i-1][j-1].add(t.multiply(x[i-1][l-1]));
                }

                if ((j < pl) || (j > pu)) {
                    continue;
                }

                if (qraux[j-1].equals(DoubleDouble.valueOf(0.0))) {
                    continue;
                }

                temp = (x[l-1][j-1].abs()).divide(qraux[j-1]);
                tt = (DoubleDouble.valueOf(1.0)).subtract(temp.multiply(temp));
                tt = tt.max(DoubleDouble.valueOf(0.0));
                t = tt;
                temp = qraux[j-1].divide(work[j-1]);
                tt = (DoubleDouble.valueOf(1.0)).add((((DoubleDouble.valueOf(0.05)).multiply(tt)).multiply(temp)).multiply(temp));

                if (!(tt.equals(DoubleDouble.valueOf(1.0)))) {
                    qraux[j-1] = qraux[j-1].multiply(t.sqrt());
                } else {

                    for (i = 0; i < (n - l); i++) {
                        tempMat[i] = x[l + i][j-1];
                    }

                    qraux[j-1] = dnrm2(n - l, tempMat, 1);
                    work[j-1] = qraux[j-1];
                }
            } // for (j = lp1; j <= p; j++)

            // SAVE THE TRANSFORMATION.

            qraux[l-1] = x[l-1][l-1];
            x[l-1][l-1] = nrmxl.negate();
        } // for (l = 1; l <= lup; l++)

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
    private void sqrsl(DoubleDouble[][] x, int n, int k, DoubleDouble[] qraux, DoubleDouble[] y,
    		           DoubleDouble[] qy, DoubleDouble[] qty, DoubleDouble[] b,
                       DoubleDouble[] rsd, DoubleDouble[] xb, int job) {
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
        // INFO IS 0 UNLESS THE COMPUTATION OF B HAS BEEN REQUESTED AND
        // R IS EXACTLY SINGULAR.  IN THIS CASE, INFO IS THE (INDEX+1) OF THE
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
        DoubleDouble t, temp;
        boolean cb, cqy, cqty, cr, cxb;

        // SET INFO FLAG.

        info = 0;

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

                if (x[0][0].equals(DoubleDouble.valueOf(0.0))) {
                    info = 1;
                } else {
                    b[0] = y[0].divide(x[0][0]);
                }
            } // if (cb)

            if (cr) {
                rsd[0] = DoubleDouble.valueOf(0.0);
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

                if (qraux[j].equals(DoubleDouble.valueOf(0.0))) {
                    continue;
                }

                temp = x[j][j];
                x[j][j] = qraux[j];
                t = DoubleDouble.valueOf(0.0);

                for (i = j; i < n; i++) {
                    t = t.subtract(x[i][j].multiply(qy[i]));
                }
                t = t.divide(x[j][j]);

                for (i = j; i < n; i++) {
                    qy[i] = qy[i].add(t.multiply(x[i][j]));
                }

                x[j][j] = temp;
            } // for (jj = 1; jj <= ju; jj++)
        } // if (cqy)

        if (cqty) {

            // COMPUTE TRANS(Q)*Y.

            for (j = 0; j < ju; j++) {

                if (qraux[j].equals(DoubleDouble.valueOf(0.0))) {
                    continue;
                }

                temp = x[j][j];
                x[j][j] = qraux[j];
                t = DoubleDouble.valueOf(0.0);

                for (i = j; i < n; i++) {
                    t = t.subtract(x[i][j].multiply(qty[i]));
                }
                t = t.divide(x[j][j]);

                for (i = j; i < n; i++) {
                    qty[i] = qty[i].add(t.multiply(x[i][j]));
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

            for (i = 0; i < n-k-1; i++) {
                rsd[kp1-1+i] = qty[kp1-1+i];
            }
        } // if (cr && (k < n))

        if (cxb && (kp1 <= n)) {

            for (i = kp1 - 1; i < n; i++) {
                xb[i] = DoubleDouble.valueOf(0.0);
            }
        } // if (cxb && (kp1 <= n))

        if (cr) {

            for (i = 0; i < k; i++) {
                rsd[i] = DoubleDouble.valueOf(0.0);
            }
        } // if (cr)

        if (cb) {

            // COMPUTE B.

            for (jj = 1; jj <= k; jj++) {
                j = k - jj;

                if (x[j][j].equals(DoubleDouble.valueOf(0.0))) {
                    info = j+1;

                    break;
                }

                b[j] = b[j].divide(x[j][j]);

                if (j == 0) {
                    continue;
                }

                t = b[j].negate();

                for (i = 0; i < j; i++) {
                    b[i] = b[i].add(t.multiply(x[i][j]));
                }
            } // for (jj = 1; jj <= k; jj++)
        } // if (cb)

        if ((!cr) && (!cxb)) {
            return;
        }

        // COMPUTE RSD OR XB AS REQUIRED.

        for (jj = 1; jj <= ju; jj++) {
            j = ju - jj;

            if (qraux[j].equals(DoubleDouble.valueOf(0.0))) {
                continue;
            }

            temp = x[j][j];
            x[j][j] = qraux[j];

            if (cr) {
                t = DoubleDouble.valueOf(0.0);

                for (i = j; i < n; i++) {
                    t = t.subtract(x[i][j].multiply(rsd[i]));
                }
                t = t.divide(x[j][j]);

                for (i = j; i < n; i++) {
                    rsd[i] = rsd[i].add(t.multiply(x[i][j]));
                }
            } // if (cr)

            if (cxb) {
                t = DoubleDouble.valueOf(0.0);

                for (i = j; i < n; i++) {
                    t = t.subtract(x[i][j].multiply(xb[i]));
                }
                t = t.divide(x[j][j]);

                for (i = j; i < n; i++) {
                    xb[i] = xb[i].add(t.multiply(x[i][j]));
                }
            } // if (cxb)

            x[j][j] = temp;
        } // for (jj = 1; jj <= ju; jj++)

        return;
    }

    /**
     * DOCUMENT ME!
     */
    private void stepuc(int prank[]) {
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
        // constraintAct INTEGER SCALAR CONTAINING THE NO. OF ACTIVE CONSTRAINTS
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
        // constraintAct  THE CURRENT NO. OF ACTIVE CONSTRAINTS
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

        DoubleDouble[] gmod = new DoubleDouble[param];

        int i;
        DoubleDouble alplow, ael, magfy, dphize;
        DoubleDouble[][] dummy = new DoubleDouble[1][1];
        DoubleDouble temp;
        int stepucCtrl;

        // IF ABS(kod)=2
        // THEN TAKE AN UNDAMPED STEP
        // ELSE USE lineuc TO COMPUTE STEPSIZE alpha

        // DETERMINE LOWER AND UPPER BOUND OF STEP LENGTH AND
        // SUGGEST alpha AS STEPSIZE

        eval = 0;
        alphup = DoubleDouble.valueOf(3.0);

        if ((ifree > 0) && (indic < 0)) {
            alphup = (DoubleDouble.valueOf(10.0)).multiply(alphup);
        }

        if (bounds != 0) {
        	if (outputMes) {
	            Preferences.debug("aset\n", Preferences.DEBUG_ALGORITHM);
	
	            for (i = 0; i < param; i++) {
	                Preferences.debug(aset[i] + "\n", Preferences.DEBUG_ALGORITHM);
	            }
	
	            Preferences.debug("dx\n", Preferences.DEBUG_ALGORITHM);
	
	            for (i = 0; i < param; i++) {
	                Preferences.debug(dx[i] + "\n", Preferences.DEBUG_ALGORITHM);
	            }
        	}

            for (i = 0; i < param; i++) {

                if (aset[i] != 0) {
                    continue;
                }

                if (dx[i].gt(DoubleDouble.valueOf(0.0))) {

                    if ((alphup.multiply(dx[i])).lt(bu[i].subtract(a[i]))) {
                        continue;
                    }

                    alphup = (bu[i].subtract(a[i])).divide(dx[i]);

                    continue;
                } // if (dx[i] > 0.0)

                if (((alphup.negate()).multiply(dx[i])).le(a[i].subtract(bl[i]))) {
                    continue;
                }

                alphup = ((a[i].subtract(bl[i])).negate()).divide(dx[i]);
            } // for (i = 0; i < param; i++)
        } // if (bounds != 0)

        alplow = alphup.divide(DoubleDouble.valueOf(3000.0));

        if (Math.abs(kod) != 2) {
            magfy = DoubleDouble.valueOf(3.0);

            if (prank[0] < rngkm1) {
                magfy = DoubleDouble.valueOf(6.0);
            }

            alpha = (DoubleDouble.valueOf(1.0)).min((magfy.multiply(alfkm1)).min(alphup));
            if (outputMes) {
	            Preferences.debug("In stepuc: alpha = " + alpha + "\n", Preferences.DEBUG_ALGORITHM);
	            Preferences.debug("In stepuc: alfkm1 = " + alfkm1 + "\n", Preferences.DEBUG_ALGORITHM);
	            Preferences.debug("In stepuc: alphup = " + alphup + "\n", Preferences.DEBUG_ALGORITHM);
            }

            if ((ifree > 0) && (indic < 0)) {
                alpha = (DoubleDouble.valueOf(10.0)).min(alphup);
            }

            alpha = (alpha.max(alplow)).multiply(DoubleDouble.valueOf(2.0));

            // COMPUTE THE DERIVATIVE OF phi(a + alpha * dx) AT alpha = 0
            dphize = DoubleDouble.valueOf(0.0);

            for (i = 0; i < param; i++) {
                dphize = dphize.add(g[i].multiply(dx[i]));
            }

            do {
                alpha = alpha.multiply(DoubleDouble.valueOf(0.5));

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

        alpha = (DoubleDouble.valueOf(1.0)).min(alphup);
        errorStatus = 0;
        aDiff = DoubleDouble.valueOf(0.0);

        for (i = 0; i < param; i++) {
            ael = a[i];
            a[i] = ael.add(alpha.multiply(dx[i]));
            aDiff = aDiff.add((a[i].subtract(ael)).multiply(a[i].subtract(ael)));
        } // for (i = 0; i < param; i++)

        aDiff = aDiff.sqrt();

        // COMPUTE THE VECTOR OF RESIDUALS AT THE NEW POINT

        stepucCtrl = 1;
        ctrlMat[0] = stepucCtrl;
        if (testMode) {
        	fitToTestFunction(a, residuals, dummy);
        }
        else {
            fitToFunction(a, residuals, dummy);
        }
        stepucCtrl = ctrlMat[0];
        eval = 1;

        // INDICATE A LARGER VALUE OF THE OBJECTIVE IF THE RESIDUALS ARE
        // UNCOMPUTABLE  (I.E. CTRL=-1 )

        if (stepucCtrl == -1) {
            phi = (DoubleDouble.valueOf(2.0)).multiply(phi);
        }

        if (stepucCtrl == 1) {
            temp = dnrm2(nPts, residuals, 1);
            phi = ((DoubleDouble.valueOf(0.5)).multiply(temp)).multiply(temp);
        }

        if (stepucCtrl < -10) {
            errorStatus = stepucCtrl;
        }

        return;
    }

    /**
     * DOCUMENT ME!
     *
     * @param  b  DOCUMENT ME!
     */
    private void strsl(int prank[], DoubleDouble[] b) {
        // STRSL SOLVES SYSTEMS OF THE FORM

        // covarMat * X = B

        // WHERE covarMat IS AN UPPER TRIANGULAR MATRIX OF ORDER pseudoRank

        // ON RETURN

        // B         B CONTAINS THE SOLUTION, IF INFO .EQ. 0.
        // OTHERWISE B IS UNALTERED.

        // INFO      INTEGER
        // INFO CONTAINS 0 IF THE SYSTEM IS NONSINGULAR.
        // OTHERWISE INFO CONTAINS THE (INDEX+1) OF
        // THE FIRST ZERO DIAGONAL ELEMENT OF T.

        DoubleDouble temp;
        int j, jj, k;

        for (info = 0; info < prank[0]; info++) {

            if (covarMat[info][info].equals(DoubleDouble.valueOf(0.0))) {
            	info++;
                return;
            }
        }

        info = 0;


        // SOLVE covarMat*X=B FOR covarMat UPPER TRIANGULAR.

        b[prank[0] - 1] = b[prank[0] - 1].divide(covarMat[prank[0] - 1][prank[0] - 1]);

        for (jj = 2; jj <= prank[0]; jj++) {
            j = prank[0] - jj;
            temp = b[j + 1].negate();

            for (k = 0; k <= j; k++) {
                b[k] = b[k].add(temp.multiply(covarMat[k][j + 1]));
            }

            b[j] = b[j].divide(covarMat[j][j]);
        }

        return;
    }

    /**
     * DOCUMENT ME!
     */
    private void subuc(int prank[], int rank[]) {
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

        DoubleDouble[] work = new DoubleDouble[param];

        // FIND APPROPRIATE SUBSPACE

        dimsub(prank, rank);

        // COMPUTE SEARCH DIRECTION BY MINIMIZING IN A SUBSPACE OF DIMENSION RANK

        d1sqs = gndunc(internalScaling, rank, work);
        dxnorm = dnrm2(param, dx, 1);

        return;
    }

    /**
     * DOCUMENT ME!
     *
     * @param  aNorm  DOCUMENT ME!
     */
    private void termuc(int prank[], DoubleDouble aNorm) {
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
        // constraintAct INTEGER SCALAR CONTAINING THE NO. OF ACTIVE CONSTRAINTS
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

        DoubleDouble rlmax;
        int i;
        noise = DoubleDouble.valueOf(0.1);
        eps = DoubleDouble.valueOf(0.01);
        epsl = DoubleDouble.valueOf(0.001);

        exitStatus = 0;
        rlmax = DoubleDouble.valueOf(0.0);

        // THE CONVERGENCE CRITERIA ARE NOT CHECKED IF THE LATEST STEP
        // WAS A RESTART STEP OR WAS NOT A FULL PSEUDO RANK STEP

        if ((imax != -1) || (restart) || ((kodkm1 == -1) && (alfnoi.le(noise))) ||
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

        if (outputMes) {
            Preferences.debug("In TERMUC:\n", Preferences.DEBUG_ALGORITHM);
        }

        // CRITERIUM NO. 1

        if (d1sqs.le((fsum.multiply(relativeConvergence)).multiply(relativeConvergence))) {
            exitStatus = exitStatus + 10000;
        }

        // CRITERIUM NO. 2

        if (fsum.le(absoluteConvergence.multiply(absoluteConvergence))) {
            exitStatus = exitStatus + 2000;
        }

        // CRITERIUM NO. 3

        if (aDiff.lt(aNorm.multiply(parameterConvergence))) {
            exitStatus = exitStatus + 300;
        }

        // CRITERIUM NO. 4

        if ((alfnoi.gt(noise)) || (errorStatus == -2)) {
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

        if (outputMes) {
	        Preferences.debug("Checking Lagrange mult. in TERMUC\n", Preferences.DEBUG_ALGORITHM);
	
	        for (i = 0; i < param; i++) {
	            Preferences.debug(g[i] + "\n", Preferences.DEBUG_ALGORITHM);
	        }
        }

        if (constraintAct != 0) {

            for (i = 0; i < param; i++) {

                if ((g[i].abs()).lt(epsl)) {
                    continue;
                }

                if (((g[i].multiply(DoubleDouble.valueOf(aset[i]))).multiply(bu[i].subtract(bl[i]))).ge(DoubleDouble.valueOf(0.0))) {
                    continue;
                }

                if (rlmax.gt(g[i].abs())) {
                    continue;
                }

                rlmax = g[i].abs();
                exitStatus = 0;
            } // for (i = 0; i < param; i++)
        } // if (constraintAct != 0)

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

        if (prank[0] != (param - constraintAct)) {
            exitStatus = exitStatus + 1;

            return;
        }

        if ((kodkm2 != 1) && (iters >= 2)) {
            exitStatus = exitStatus + 3;

            return;
        }

        if ((((alfkm2.subtract(DoubleDouble.valueOf(1.0))).abs()).le(eps)) &&
        	 (((alfkm1.subtract(DoubleDouble.valueOf(1.0))).abs()).le(eps))) {
            return;
        }

        exitStatus = exitStatus + 4;

        return;
    }

    /**
     * DOCUMENT ME!
     *
     * @param   tol       DOCUMENT ME!
     * @param   constraintAct  DOCUMENT ME!
     *
     * @return  DOCUMENT ME!
     */
    private void triunc(DoubleDouble tol, int constraintAct, int prank[]) {
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
        // constraintAct INTEGER SCALAR CONTAINING THE NO. OF ACTIVE CONSTRAINTS
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
        DoubleDouble r11;

        prank[0] = param;
        if (param == 0) {
            return;
        }

        // INITIATE PIVOT VECTOR SO THAT ALL COLUMNS CORRESPONDING TO
        // ACTIVE BOUNDS ARE CONSIDERED AS FINAL COLUMNS
        
        // In regular 64 bit operation tol = 2.5809E-8.
        // In extended precision tol = 3.8499E-162.
        // In DRAPER24D internal scaling = false, Analytical Jacobian saw a 
        // wrong result from a covarMat[1][1] = 1.8488E-32 in extended precision
        // The covarMat[1][1] was 0.0 in 64 bit mode
        // Don't let the minimum aciting tolerance go below 1.0E-20.
        DoubleDouble minTol = tol.max(DoubleDouble.valueOf(1.0E-20));

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
        r11 = covarMat[0][0].abs();
        nn = param - constraintAct;

        if (nn == 0) {
            prank[0] = 0;
            return;
        }

        for (i = 0; i < nn; i++) {

            if ((covarMat[i][i].abs()).ge(minTol)) {
                k = i;
            }
        } // for (i = 0; i < nn; i++)

        if (k == -1) {

            for (j = 0; j < nn; j++) {

                if ((covarMat[j][j].abs()).le(minTol.multiply(r11))) {
                    break;
                }

                k = j;
            } // for (j = 0; j < nn; j++)
        } // if (k == -1)

        prank[0] =  k + 1;
        return;
    }

    /**
     * DOCUMENT ME!
     */
    private void twor() {
        // COMPUTE THE THREE ROOTS OF A THIRD DEGREE POLYNOMIAL WHEN
        // THERE ARE 3 REAL ROOTS

        DoubleDouble eps = DoubleDouble.valueOf(1.0E-8);
        DoubleDouble fi, sqd, t, tanfi;
        DoubleDouble num;
        DoubleDouble cosarg;

        sqd = (deltaMinrm.negate()).sqrt();

        if ((qMinrm.abs()).le(((DoubleDouble.valueOf(2.0)).multiply(eps)).multiply(sqd))) {
            fi = DoubleDouble.PI_2;
        } else {
            tanfi = (((DoubleDouble.valueOf(2.00)).multiply(sqd)).divide(qMinrm)).abs();
            fi = tanfi.atan();
        }

        t = (DoubleDouble.valueOf(2.0)).multiply(((pMinrm.negate()).divide(DoubleDouble.valueOf(3.0))).sqrt());

        if (qMinrm.gt(DoubleDouble.valueOf(0.0))) {
            t = t.negate();
        }

        cosarg = fi.divide(DoubleDouble.valueOf(3.0));
        x1Minrm = (t.multiply(cosarg.cos())).subtract(a1div3);
        num = fi.add(DoubleDouble.TWO_PI);
        cosarg = num.divide(DoubleDouble.valueOf(3.0));
        x2Minrm = (t.multiply(cosarg.cos())).subtract(a1div3);
        num = fi.add((DoubleDouble.valueOf(4.0)).multiply(DoubleDouble.PI));
        cosarg = num.divide(DoubleDouble.valueOf(3.0));
        x3Minrm = (t.multiply(cosarg.cos())).subtract(a1div3);

        return;
    }

}
