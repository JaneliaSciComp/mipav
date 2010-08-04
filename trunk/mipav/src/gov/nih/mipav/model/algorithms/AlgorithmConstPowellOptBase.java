package gov.nih.mipav.model.algorithms;


import WildMagic.LibFoundation.NumericalAnalysis.function.RealFunctionOfSeveralVariables;
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

public abstract class AlgorithmConstPowellOptBase extends AlgorithmBase implements RealFunctionOfSeveralVariables {

    //~ Static fields/initializers -------------------------------------------------------------------------------------

    /**
     * Golden ratio is .38197 and .61803. Use this second part to magnify successive intervals in the bracketing
     * function. The "1" is for the magnification.
     */
    private static final double GOLD = 1.618034;

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

    /** Progress bar that may be set for long optimization runs. */
    protected ViewJProgressBar myProgressBar = null;

    /** Degress of freedom. */
    protected int nDims;

    /** Parent algorithm that called this optimization. */
    protected AlgorithmBase parent;

    /** Point that is currently being optimized. */
    protected double[] point;

    /** Where progress is when sent in. */
    protected int progressBegin;

    /** The max the progress can go to. */
    protected int progressMax;

    /** Point that was initially passed into function. */
    protected double[] start;

    /** Indicates whether the Powell algorithm succeeded in finding a minimum. */
    protected boolean success = true;

    /** Array of tolerances for each dimension. */
    protected double[] tolerance;

    /** The transformation matrix to the origin of the input image. */
    protected TransMatrix toOrigin;

    /** Array of translation and rotation limits for each dimension. */
    protected float[][] trLimits;

    /** DOCUMENT ME! */
    private int direction;

    /** DOCUMENT ME! */
    private double margin = Math.pow(10, -4);

    /** DOCUMENT ME! */
    private double maxLimit1D;

    /** DOCUMENT ME! */
    private double minLimit1D;

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
     * @param  bracket          the bracket bound for the first iteration of powell
     */
    public AlgorithmConstPowellOptBase(AlgorithmBase parentAlgo, int degreeOfFreedom,
                                       AlgorithmOptimizeFunctionBase costFunc, double[] initial, double[] tols,
                                       int maxIter, int coords, int bracket) {
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
     * Returns the final point with translations, rotations, scales, and skews representing the best
     * Transformation.
     *
     * @return  A vector representing the best transformation in terms of translations, rotations, scales, and skews.
     */
    public abstract double[] getFinal();

    /**
     * Returns the matrix representing the best transformation.
     *
     * @return  A matrix representing the best transformation.
     */
    public abstract TransMatrix getMatrix();

    /**
     * Returns the matrix representing the best transformation. The passed in parameter represents the
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
     * Calls cost function with inputed point and saves result in functionAtBest.
     */
    public abstract void measureCost();

    /**
     * Sets the initial point to the value passed in.
     *
     * @param  initial  Initial point.
     */
    public abstract void setInitialPoint(double[] initial);

    /**
     * Sets the limits on rotation and translation.
     *
     * @param  limits  limits
     */
    public abstract void setLimits(float[][] limits);

    /**
     * Returns whether or not a minimum was found.
     *
     * @return  whether or not a minimum was found.
     */
    public boolean didSucceed() {
        return success;
    }

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
     * Returns the cost of the best transformation.
     *
     * @return  The cost of the best transformation.
     */
    public double getCost() {
        return functionAtBest;
    }

    /**
     * Returns the optimized point, with length == degrees of freedom.
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
     * Sets the maximum number of iterations.
     *
     * @param  max  The max number of iterations.
     */
    public void setMaxIterations(int max) {
        maxIterations = max;
    }

    /**
     * Method to display current point values.
     *
     * @param  point    DOCUMENT ME!
     * @param  message  DOCUMENT ME!
     */
    protected void debugPoint(double[] point, String message) {
        Preferences.debug(message + "\n");

        for (int i = 0; i < point.length; i++) {
            Preferences.debug("point[" + i + "]: " + point[i] + "\n");
        }

        Preferences.debug("\n");
    }

    /**
     * Minimizes the point along the given vector direction. Given an initial point and a guess as to how far from that
     * point we should go to look for a minimum.
     *
     * <p>Changed from lineMinimization in AlgorithmPowellOptBase because direction is now assumed ot be one of the
     * original orthogonal directions and can be represented by one integer instead of a vector.</p>
     *
     * @param   boundguess  Guess as to how far from that point we should go to look.
     * @param   direct      Direction to find minimum along.
     *
     * @return  Minimum of the point along the given vector.
     */

    protected double lineMinimization(double boundguess, int direct) {
        double unit_tolerance = tolerance[direct];
        double a, b;
        double xNew;
        Bracket bracket;

        // Initialize limits for the current direction.
        minLimit1D = trLimits[0][direct];
        maxLimit1D = trLimits[1][direct];

        // Set points a and b (where a<b<c) using boundguess as guide, but limiting to prescribed range
        a = boundguess * unit_tolerance;

        while (testBounds1D(a) && (boundguess > 1)) {
            boundguess--;
            a = boundguess * unit_tolerance;
        }

        // Preferences.debug("boundguess: " + boundguess + ", unit tolerance: " +unit_tolerance + ".\n");
        b = 0;

        // minimumBracket is called and will set bracket.c and functionAtC.
        bracket = minimumBracket(a, b, direct, unit_tolerance);

        if (bracket.a == bracket.b) {

            // This will happen when minimumBracket couldn't even go "unit_tolerance" away from bracket.b
            // withouth going out of bounds.
            // Preferences.debug("We've gone out of bounds in trying to bracket the minimum.\n");
            return oneDimension(bracket.b, direct);
        } else {

            // Preferences.debug("Starting with bracket: \n" + bracket);
            xNew = estimateMinimum(bracket);
            
            if (Double.isNaN(xNew)) {
                // Quadratic only has a maximum or near linear condtion
                return oneDimension(bracket.b, direct);
            }

            if (testBounds1D(xNew)) {
                Preferences.debug("Minimum that was found is out of bounds. \n");

                return oneDimension(bracket.b, direct);
            }

            bracket.b = xNew;
            point[direct] = point[direct] + bracket.b;

            return bracket.functionAtB;
        }
    }

    /**
     * DOCUMENT ME!
     *
     * @param   testPt  DOCUMENT ME!
     *
     * @return  DOCUMENT ME!
     */
    protected boolean testBounds1D(double testPt) {
        boolean outOfBounds = false;
        double xt = 0.d;
        double min = minLimit1D;
        double max = maxLimit1D;
        int id = direction;

        xt = point[id] + testPt;

        if ((xt < (min - margin)) || (xt > (max + margin))) {
            outOfBounds = true;
        }

        return outOfBounds;
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
            Preferences.debug("Estimate minimum Quadratic only has a maximum\n");
            xNew = Double.NaN;

            return xNew;
        }

        if (Math.abs(ad) > TINY) {
            xNew = -bd / (2 * ad);

            return xNew;
        } else { // near linear condition -> get closer to an end point
            Preferences.debug("Estimate minimum Near linear condition -> get closer to end point\n");
            xNew = Double.NaN;

            return xNew;
        }
    }

    /**
     * Finds the bracket for the minimum value. A "bracket" is a set of three points such that a < b < c and f(a) > f(b)
     * and f(c) > f(b). a and c are said to "bracket" b if this condition holds. Original was based on the function
     * mnbrak in Numerical Recipes. I have completely changed it so that it doesn't even attempt to use parabolic
     * extrapolation. -Z Cohen
     *
     * @param   a     Starting value for bracket - we will change so that f(b) < f(a).
     * @param   b     Starting value for bracket - we will change so that f(b) < f(a).
     * @param   id    DOCUMENT ME!
     * @param   uTol  DOCUMENT ME!
     *
     * @return  The newly created bracket. Set a=b if bracket not found.
     */
    private Bracket minimumBracket(double a, double b, int id, double uTol) {
        // Create new Bracket.  Set a and b and find functionAtA and functionAtB.

        Bracket bracket = new Bracket();
        bracket.a = a;
        bracket.b = b;
        bracket.functionAtA = oneDimension(bracket.a, id);
        bracket.functionAtB = oneDimension(bracket.b, id);

        if (bracket.functionAtA < bracket.functionAtB) { // f(a) should be > f(b).  if not, swap a and b

            double tempx = bracket.a;
            double tempy = bracket.functionAtA;
            bracket.a = bracket.b;
            bracket.functionAtA = bracket.functionAtB;
            bracket.b = tempx;
            bracket.functionAtB = tempy;
        }

        // Find the maximum allowable value for bracket.c
        double maxC = 0.0;

        if ((bracket.b - bracket.a) > 0.0) {

            // Going in positive direction.
            maxC = maxLimit1D - point[id];
        } else {
            maxC = minLimit1D - point[id];
        }
        // Preferences.debug("maxC is " +maxC +".\n");

        // Find initial bracket.c
        double fraction = 1.0;
        bracket.c = bracket.b + (GOLD * fraction * (bracket.b - bracket.a));

        while ((bracket.c > maxC) && ((GOLD * fraction) > uTol)) {
            bracket.c = bracket.b + (GOLD * fraction * (bracket.b - bracket.a));
            fraction *= 0.95;
        }

        // Preferences.debug("Fraction of GOLD is: " +fraction +".\n");
        if ((GOLD * fraction) <= uTol) {
            Preferences.debug("Could not even create an initial bracket.  Even uTol, " + uTol +
                              " puts us out of bounds.\n");
            bracket.a = bracket.b;

            return bracket;
        }

        bracket.functionAtC = oneDimension(bracket.c, id);

        double newX2 = 0.0, newY2 = 0.0, maxX2 = 0.0;
        double dir = 1.0;

        if (bracket.b < bracket.a) {
            dir = -1.0;
        }

        int count = 1;

        while (bracket.functionAtB > bracket.functionAtC) {
            count++;
            maxX2 = bracket.b + (2 * GOLD * (bracket.c - bracket.b));
            newX2 = estimateMinimum(bracket);

            if ((Double.isNaN(newX2)) || (((newX2 - bracket.a) * dir) < 0) || (((newX2 - maxX2) * dir) > 0)) {
                newX2 = bracket.b + (GOLD * (bracket.c - bracket.a));
            }

            newY2 = oneDimension(newX2, id);

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

        // test that we haven't gone out of bounds
        if (testBounds1D(bracket.c)) {
            bracket.a = bracket.b;
        } else {

            if (count > 1) {
                Preferences.debug("It took " + count + " iterations to bracket the minimum.\n");
                // If we never get this message, that means that the entire while loop above isn't necessary, b/c we're
                // never going into it.
            }
        }

        return bracket;
    }


    /**
     * Measures the cost of the vector point at x.
     *
     * @param   x   DOCUMENT ME!
     * @param   id  DOCUMENT ME!
     *
     * @return  Cost of function at x.
     */
    private double oneDimension(double x, int id) {
        double f;
        double[] xt = new double[nDims];

        for (int i = 0; i < nDims; i++) {
            xt[i] = point[i];
        }

        xt[id] = point[id] + x;

        f = costFunction.cost(convertToMatrix(xt));

        //for ( int i = 0; i < xt.length; i++ )
        //{
        //	System.err.print( xt[i] + " " );
        //}
        
        //System.err.println(f);

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
                              "c: " + c + " f(c): " + functionAtC + ".\n");
        }
    }


    @Override
    public double eval(double[] x) {
        double r = costFunction.cost(convertToMatrix(x));
        
        //for ( int i = 0; i < x.length; i++ )
        //{
        //	System.err.print( x[i] + " " );
        //}
        
        //System.err.println(r);
        return r;
    }


    @Override
    public int getNumberOfVariables() {
        return nDims;
    }
    
    
}
