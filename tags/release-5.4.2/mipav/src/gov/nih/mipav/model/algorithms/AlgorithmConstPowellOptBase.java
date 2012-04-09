package gov.nih.mipav.model.algorithms;


import de.jtem.numericalMethods.calculus.function.RealFunctionOfSeveralVariables;
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
 * @version  0.1 Oct 1, 2001
 * @author   Neva Cherniavsky
 */

public abstract class AlgorithmConstPowellOptBase extends AlgorithmBase implements RealFunctionOfSeveralVariables {

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
     * Returns the final point with translations, rotations, scales, and skews representing the best
     * Transformation.
     *
     * @return  A vector representing the best transformation in terms of translations, rotations, scales, and skews.
     */
    public abstract double[] getFinal(double[] point);

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
        Preferences.debug(message + "\n", Preferences.DEBUG_ALGORITHM);

        for (int i = 0; i < point.length; i++) {
            Preferences.debug("point[" + i + "]: " + point[i] + "\n", Preferences.DEBUG_ALGORITHM);
        }

        Preferences.debug("\n", Preferences.DEBUG_ALGORITHM);
    }

    @Override
    public double eval(double[] x) {
        // The different searches change the number of variables in the
        // transformation matrix that change. The savedStartPoint
        // fills in the rest of the 'constant' portion of the matrix.
        double[]fullPoint = getFinal(x);
        double r = costFunction.cost(convertToMatrix(fullPoint));
        return r;
    }


    @Override
    public int getNumberOfVariables() {
        return nDims;
    }
    
    
}
