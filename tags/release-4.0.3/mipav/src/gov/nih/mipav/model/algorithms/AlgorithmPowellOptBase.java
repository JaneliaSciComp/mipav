package gov.nih.mipav.model.algorithms;


import gov.nih.mipav.model.structures.*;

import gov.nih.mipav.view.*;


/**
 * Powell's Method<br>
 *
 * <p>Runs Powell's method. Powell's method is a way to find minimums without finding derivatives. Basically, it starts
 * at some point P in N-dimensional space, proceeds in a direction, and minimizes along that line using a 1-dimensional
 * minimization method. It continues like this until the point has moved by less than the tolerance. It starts with the
 * initial point defined in the constructor and initial directions of (1 0 ... 0) (0 1 0 ... ) ..., the basis vectors.
 * At the end, "point" is the best point found and functionAtBest is the value at "point".</p>
 *
 * <p>This is an abstract class which contains many of the basic functions needed to run Powell's method. 2D and 3D
 * versions extend this version.</p>
 *
 * <p>This optimization technique is based on FLIRT. FLIRT stands for FMRIB's Linear Image Registration Tool. For more
 * information on FLIRT, visit their homepage at <a href="http://www.fmrib.ox.ac.uk/fsl/flirt/">
 * http://www.fmrib.ox.ac.uk/fsl/flirt/</a>. Their main paper is:</p>
 *
 * <p>Jenkinson, M. and Smith, S. (2001a).<br>
 * A global optimisation method for robust affine registration of brain images.<br>
 * <i>Medical Image Analysis</i>, 5(2):143-156.<br>
 * </p>
 *
 * <p>For further information about Powell's method consult: <u>Numerical Recipes in C: The Art of Scientific Computing
 * Second Edition</u><br>
 * Press, William H., Teukolsky, Saul A., Vettering, William T., and Flannery, Brian P. Cambridge Unversity Press:
 * Cambridge, 1988.</p>
 *
 * @version  0.1 Oct 1, 2001
 * @author   Neva Cherniavsky
 */

public abstract class AlgorithmPowellOptBase extends AlgorithmBase {

    //~ Static fields/initializers -------------------------------------------------------------------------------------

    /**
     * Golden ratio is .38197 and .61803. Use this second part to magnify successive intervals in the bracketing
     * function. The "1" is for the magnification.
     */
    private static final double GOLD = 1.618034;

    /** Golden ratio is .38197 and .61803. */
    private static final double RGOLD = 0.618034;

    /** Golden ratio - second part. */
    private static final double CGOLD = 0.3819660;

    /** Used to prevent division by zero. */
    private static final double TINY = 1.0 * Math.pow(10, -20);

    //~ Instance fields ------------------------------------------------------------------------------------------------

    /** The initial bracket size for first iteration of Powell. */
    protected int bracketBound;

    /** Cost function called to measure cost - 1D. */
    protected AlgorithmOptimizeFunctionBase costFunction;

    /** Final point when optimization is complete. */
    protected double[] finalPoint;

    /** The transformation matrix from the origin of the input image. */
    protected TransMatrix fromOrigin;

    /** The cost of the function at the best minimum. */
    protected double functionAtBest;

    /** The maximum number of iterations the optimization allows. */
    protected int maxIterations;

    /** Degress of freedom. */
    protected int nDims;

    /** Parent algorithm that called this optimization. */
    protected AlgorithmBase parent;

    /** Point that is currently being optimized. */
    protected volatile double[] point;

       /** Point that was initially passed into function. */
    protected double[] start;

    /** Array of tolerances for each dimension. */
    protected double[] tolerance;

    /** The transformation matrix to the origin of the input image. */
    protected TransMatrix toOrigin;

    //~ Constructors ---------------------------------------------------------------------------------------------------

    /**
     * Constructs a new algorithm with the given centers of mass (needed for setting the transformations), the given
     * cost function (which was constructed with the proper images), the initial point we're looking at, and some
     * tolerance within that point to look for the minimum. The initial point contains rotations, translations, scales,
     * and skews.
     *
     * @param  parentAlgo       DOCUMENT ME!
     * @param  degreeOfFreedom  Degree of freedom for transformation (must be 3, 4, 6, 7, 9, or 12).
     * @param  costFunc         Cost function to use.
     * @param  initial          Initial point to start from, length of 12.
     * @param  tols             Tolerance for each dimension (tols.length == degreeOfFreedom).
     * @param  maxIter          The maximum iterations.
     * @param  coords           2 or 3 for 2D or 3D
     * @param  bracket          DOCUMENT ME!
     */
    public AlgorithmPowellOptBase(AlgorithmBase parentAlgo, int degreeOfFreedom, AlgorithmOptimizeFunctionBase costFunc,
                                  double[] initial, double[] tols, int maxIter, int coords, int bracket) {
        nDims = degreeOfFreedom;
        costFunction = costFunc;
        tolerance = tols;
        maxIterations = maxIter;
        bracketBound = bracket;
        parent = parentAlgo;
        point = new double[nDims];
        start = initial;
    }

    //~ Methods --------------------------------------------------------------------------------------------------------

    /**
     * Helper method that converts a vector with certain values set to a matrix. For values not set, uses those in start
     * variable.
     *
     * @param   vector  Point with possible rotations, translations, etc. set.
     *
     * @return  Matrix representation of that point.
     */
    public abstract TransMatrix convertToMatrix(double[] vector);

    /**
     * Accessor that returns the final point with translations, rotations, scales, and skews representing the best
     * tranformation.
     *
     * @return  A vector representing the best transformation in terms of translations, rotations, scales, and skews.
     */
    public abstract double[] getFinal();

    /**
     * Accessor that returns the matrix representing the best tranformation.
     *
     * @return  A matrix representing the best transformation.
     */
    public abstract TransMatrix getMatrix();

    /**
     * Accessor that returns the matrix representing the best tranformation. The passed in parameter represents the
     * resolution (same in all directions and for both input and reference images, since resampled isotropically). Since
     * the optimization was done in pixel space, not millimeter space, the translation parameters need to be scaled by
     * the sample value.
     *
     * @param   sample  Sample size.
     *
     * @return  A matrix representing the best transformation.
     */
    public abstract TransMatrix getMatrix(float sample);

    /**
     * Calls cost function with inputted point and saves result in functionAtBest.
     */
    public abstract void measureCost();

    /**
     * Sets the initial point to the value passed in.
     *
     * @param  initial  Initial point.
     */
    public abstract void setInitialPoint(double[] initial);

    /**
     * Sets everything to null and prepares this class for destruction.
     */
    public void disposeLocal() {
        costFunction = null;
        start = null;
        point = null;
        tolerance = null;
        finalPoint = null;
        toOrigin = null;
        fromOrigin = null;
    }

    /**
     * Accessor the returns the cost of the best transformation.
     *
     * @return  The cost of the best transformation.
     */
    public double getCost() {
        return functionAtBest;
    }

    /**
     * Accessor that returns the optimized point, with length == degrees of freedom.
     *
     * @return  The optimized point.
     */
    public double[] getPoint() {
        double[] pt = new double[point.length];

        for (int i = 0; i < pt.length; i++) {
            pt[i] = point[i];
        }

        return pt;
    }

    /**
     * Accessor that sets the maximum number of iterations.
     *
     * @param  max  The max number of iterations.
     */
    public void setMaxIterations(int max) {
        maxIterations = max;
    }

    /**
     * Minimizes the point along the given vector direction. Given an initial point and a guess as to how far from that
     * point we should go to look for a minimum.
     *
     * @param   initial     Initial point to look for minimum at.
     * @param   boundguess  Guess as to how far from that point we should go to look.
     * @param   directions  Direction to find minimum along.
     *
     * @return  Minimum of the point along the given vector.
     */
    protected void lineMinimization(double[] pt, double initial, double boundguess, double[] directions) {
        // Set up tolerances in direction of line minimization.
        // "unit_directions" is a unit vector in the direction of "directions"
        double tol = 0, sum = 0;
        double[] unit_directions = new double[nDims];

        for (int i = 0; i < nDims; i++) {
            sum += directions[i] * directions[i];
        }

        for (int i = 0; i < nDims; i++) {
            unit_directions[i] = directions[i] / (Math.sqrt(sum));

            if (tolerance[i] > TINY) {
                tol += Math.abs(unit_directions[i] / tolerance[i]);
            }
        }

        double unit_tolerance = Math.abs(1 / tol);

        // Create new Bracket.  Set a and b and find functionAtA and functionAtB.
        Bracket bracket = new Bracket();

        bracket.a = boundguess * unit_tolerance;
        bracket.functionAtA = oneDimension(pt, bracket.a, unit_directions);

        bracket.b = 0;

        if (initial == 0) { // for first call to lineMinimization within PowellOpt3D
            bracket.functionAtB = oneDimension(pt, bracket.b, unit_directions);
        } else {
            bracket.functionAtB = initial;
        }

        // minimumBracket is called and will set bracket.c and functionAtC.
        minimumBracket(pt, bracket, unit_directions);
        // if (initial == 0)  Preferences.debug("Initial bracket: \n" +bracket);

        double minDist = 0.1 * unit_tolerance;
        double xNew, yNew;
        int count = 0;

        while (((++count) < 100) && (Math.abs(bracket.c - bracket.a) > unit_tolerance) && !parent.isThreadStopped()) {

            if (count > 0) {
                xNew = nextPoint(bracket);
            } else {
                xNew = extrapolatePoint(bracket);
            }

            double directionN = 1.0;

            if (bracket.c < bracket.a) {
                directionN = -1.0;
            }

            if (Math.abs(xNew - bracket.a) < minDist) {
                xNew = bracket.a + (directionN * minDist);
            }

            if (Math.abs(xNew - bracket.c) < minDist) {
                xNew = bracket.c - (directionN * minDist);
            }

            if (Math.abs(xNew - bracket.b) < minDist) {
                xNew = extrapolatePoint(bracket);
            }

            if (Math.abs(bracket.b - bracket.a) < (4 * minDist)) {
                xNew = bracket.b + (directionN * 5 * minDist);
            }

            if (Math.abs(bracket.b - bracket.c) < (4 * minDist)) {
                xNew = bracket.b - (directionN * 5 * minDist);
            }

            yNew = oneDimension(pt, xNew, unit_directions);

            if (((xNew - bracket.b) * (bracket.c - bracket.b)) > 0) { // is xnew between bracket.c and bracket.b ?

                // swap bracket.a and bracket.c so that xnew is between bracket.a and bracket.b
                double xtemp = bracket.a;

                bracket.a = bracket.c;
                bracket.c = xtemp;

                double ytemp = bracket.functionAtA;

                bracket.functionAtA = bracket.functionAtC;
                bracket.functionAtC = ytemp;
            }

            if (yNew < bracket.functionAtB) {

                // new interval is [bracket.b,bracket.a] with xNew as best point in the middle
                bracket.c = bracket.b;
                bracket.functionAtC = bracket.functionAtB;
                bracket.b = xNew;
                bracket.functionAtB = yNew;
            } else {

                // new interval is  [bracket.c,xnew] with bracket.b as best point still
                bracket.a = xNew;
                bracket.functionAtA = yNew;
            }
        }

        for (int i = 0; i < nDims; i++) {
            pt[i] = (bracket.b * unit_directions[i]) + pt[i];
        }
    }

    /**
     * Finds the estimated quadratic minimum's position.
     *
     * @param   bracket  Bracketed minimum, with a < b < c and functionAtB < functionAtA and functionAtB < functionAtC.
     *
     * @return  The estimated quadratic minimum's position.
     */
    private double estimateMinimum(Bracket bracket) {
        double ad = 0.0, bd = 0.0, det = 0.0;
        double xNew;

        ad = ((bracket.b - bracket.c) * (bracket.functionAtB - bracket.functionAtA)) -
             ((bracket.b - bracket.a) * (bracket.functionAtB - bracket.functionAtC));
        bd = (-((bracket.b * bracket.b) - (bracket.c * bracket.c)) * (bracket.functionAtB - bracket.functionAtA)) +
             (((bracket.b * bracket.b) - (bracket.a * bracket.a)) * (bracket.functionAtB - bracket.functionAtC));
        det = (bracket.b - bracket.c) * (bracket.c - bracket.a) * (bracket.a - bracket.b);

        if ((Math.abs(det) > TINY) && ((ad / det) < 0)) { // quadratic only has a maxima
            xNew = Double.NaN;

            return xNew;
        }

        if (Math.abs(ad) > TINY) {
            xNew = -bd / (2 * ad);

            return xNew;
        } else { // near linear condition -> get closer to an end point
            xNew = Double.NaN;

            return xNew;
        }
    }

    /**
     * Comes up with a new point to test based on the golden ratio.
     *
     * @param   bracket  Original bracket - set of a, b, c and f(a), f(b) and f(c).
     *
     * @return  New point to test.
     */
    private double extrapolatePoint(Bracket bracket) {

        // bracket.b must be between bracket.a and bracket.c
        // use the golden ratio (scale similar result)
        double xNew;

        if (Math.abs(bracket.c - bracket.b) > Math.abs(bracket.a - bracket.b)) {
            xNew = (CGOLD * bracket.c) + (RGOLD * bracket.b);
        } else {
            xNew = (CGOLD * bracket.a) + (RGOLD * bracket.b);
        }

        return xNew;
    }

    /**
     * Creates a good "bracket" for the minimum value. A "bracket" is a set of three points such that a < b < c and f(a)
     * > f(b) and f(c) > f(b). a and c are said to "bracket" b if this condition holds.
     *
     * @param   bracket          Starting value for bracket - we will change so that f(b)< f(a).
     * @param   unit_directions  Starting value for bracket - we will change so that f(b)< f(a).
     *
     * @return  The class Bracket which contains a, b, c, and f(a), f(b), and f(c).
     */
    private Bracket minimumBracket(double[] pt, Bracket bracket, double[] unit_directions) {

        if (bracket.functionAtA == 0) {
            bracket.functionAtA = oneDimension(pt, bracket.a, unit_directions);
        }

        if (bracket.functionAtB == 0) {
            bracket.functionAtB = oneDimension(pt, bracket.b, unit_directions);
        }

        if (bracket.functionAtA < bracket.functionAtB) { // f(a) should be > f(b).  if not, swap a and b

            double tempx = bracket.a, tempy = bracket.functionAtA;

            bracket.a = bracket.b;
            bracket.functionAtA = bracket.functionAtB;
            bracket.b = tempx;
            bracket.functionAtB = tempy;
        }

        double newX2 = 0.0, newY2 = 0.0, maxX2 = 0.0;
        double dir = 1.0;

        if (bracket.b < bracket.a) {
            dir = -1.0;
        }

        bracket.c = bracket.b + (GOLD * (bracket.b - bracket.a));
        bracket.functionAtC = oneDimension(pt, bracket.c, unit_directions);

        while (bracket.functionAtB > bracket.functionAtC) { // note: must maintain bracket.functionAtA >=
                                                            // bracket.functionAtB
            maxX2 = bracket.b + (2 * GOLD * (bracket.c - bracket.b));
            newX2 = estimateMinimum(bracket);

            if ((Double.isNaN(newX2)) || (((newX2 - bracket.a) * dir) < 0) || (((newX2 - maxX2) * dir) > 0)) {
                newX2 = bracket.b + (GOLD * (bracket.c - bracket.a));
            }

            newY2 = oneDimension(pt, newX2, unit_directions);

            if (((newX2 - bracket.b) * (newX2 - bracket.a)) < 0) { // newx2 is between bracket.a and bracket.b

                if (newY2 < bracket.functionAtB) { // found a bracket!
                    bracket.c = bracket.b;
                    bracket.functionAtC = bracket.functionAtB;
                    bracket.b = newX2;
                    bracket.functionAtB = newY2;

                    break;
                } else { // can use newx2 as a new value for bracket.a (as newY2 >= bracket.functionAtB)
                    bracket.a = newX2;
                    bracket.functionAtA = newY2;
                }
            } else { // newx2 is between bracket.b and maxx2

                if (newY2 > bracket.functionAtB) { // found a bracket!
                    bracket.c = newX2;
                    bracket.functionAtC = newY2;

                    break;
                } else if (((newX2 - bracket.c) * dir) < 0) { // newx2 closer to bracket.b than old bracket.c
                    bracket.a = bracket.b;
                    bracket.functionAtA = bracket.functionAtB;
                    bracket.b = newX2;
                    bracket.functionAtB = newY2;
                } else {
                    bracket.a = bracket.b;
                    bracket.functionAtA = bracket.functionAtB;
                    bracket.b = bracket.c;
                    bracket.functionAtB = bracket.functionAtC;
                    bracket.c = newX2;
                    bracket.functionAtC = newY2;
                }
            }
        }

        if ((bracket.functionAtC < bracket.functionAtB) || (bracket.functionAtA < bracket.functionAtB)) {
            System.err.println("findinitialbound failed to bracket: current triplet is " + bracket);
        }

        return bracket;
    }

    /**
     * Takes the estimated minimum of the quadratic, checks to see if it is valid; if so, returns it, if not, returns
     * extrapolated point instead.
     *
     * @param   bracket  DOCUMENT ME!
     *
     * @return  Next point to test.
     */
    private double nextPoint(Bracket bracket) {
        // bracket.a and bracket.c are the bounds, bracket.b is between them

        double xNew, min, max;

        xNew = estimateMinimum(bracket);
        min = (bracket.a < bracket.c) ? bracket.a : bracket.c;
        max = (bracket.c < bracket.a) ? bracket.a : bracket.c;

        // check to see that the quadratic result is in the range
        if ((Double.isNaN(xNew)) || (xNew < min) || (xNew > max)) {
            xNew = extrapolatePoint(bracket);
        }

        return xNew;
    }

    /**
     * Measures the cost of the vector point at x.
     *
     * @param   x                DOCUMENT ME!
     * @param   unit_directions  DOCUMENT ME!
     *
     * @return  Cost of function at x.
     */
    private double oneDimension(double[] pt, double x, double[] unit_directions) {
        double f;
        double[] xt = new double[pt.length];

        for (int i = 0; i < pt.length; i++) {
            xt[i] = pt[i] + (x * unit_directions[i]);
        }

        f = costFunction.cost(convertToMatrix(xt));

        return f;
    }

    //~ Inner Classes --------------------------------------------------------------------------------------------------

    /**
     * Helper class for storing the six variables that come out of the minimumBracket function.
     */
    private class Bracket {

        /** DOCUMENT ME! */
        protected double a, b, c, functionAtA, functionAtB, functionAtC;

        /**
         * Default constructor initializes nothing.
         */
        protected Bracket() { }

        /**
         * Returns this class's elements as a string.
         *
         * @return  Representation of this class as a string.
         */
        public String toString() {
            return new String("a: " + a + " f(a): " + functionAtA + "\n" + "b: " + b + " f(b): " + functionAtB + "\n" +
                              "c: " + c + " f(c): " + functionAtC + "\n");
        }
    }

}
