package gov.nih.mipav.model.algorithms;

import WildMagic.LibFoundation.Mathematics.*;
import gov.nih.mipav.model.structures.TransMatrix;
import gov.nih.mipav.util.MipavUtil;

import java.util.Hashtable;
import java.util.Vector;
import java.util.concurrent.CountDownLatch;
import com.mentorgen.tools.profile.runtime.Profile;


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
 * @version  0.2 March 27, 2008
 * @author   Hailong Wang, Ph.D
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
//    protected double[] finalPoint;

    /** The transformation matrix from the origin of the input image. */
    protected TransMatrix fromOrigin;

    /** The cost of the function at the best minimum. */
//    protected double functionAtBest;

    /** The maximum number of iterations the optimization allows. */
    protected int maxIterations;

    /** Degrees of freedom. */
    protected int dof;

    /** Parent algorithm that called this optimization. */
    protected AlgorithmBase parent;

    /** Point that is currently being optimized. */
//    protected volatile double[] point;

       /** Point that was initially passed into function. */
//    protected double[] start;

    /** Array of tolerances for each dimension. */
    protected double[] tolerance;

    /** The transformation matrix to the origin of the input image. */
    protected TransMatrix toOrigin;

    /**
     * Array used to hold the initial points, final points and costs
     */
    protected Vectornd[] points;

    /**
     * The flag for parallel powell's method
     */
    protected boolean parallelPowell = false;

    /**
     * Store the paths for every thread.
     */
    protected Hashtable<Long, Vector<Vector<Vector3f>>> paths = new Hashtable<Long, Vector<Vector<Vector3f>>>();

    /**
     * The flag to indicate whether the searching path need to be recorded.
     */
    protected boolean pathRecorded = false;

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
     * @param  tols             Tolerance for each dimension (tols.length == degreeOfFreedom).
     * @param  maxIter          The maximum iterations.
     * @param  coords           2 or 3 for 2D or 3D
     * @param  bracket          DOCUMENT ME!
     */
    public AlgorithmPowellOptBase(AlgorithmBase parentAlgo, int degreeOfFreedom, AlgorithmOptimizeFunctionBase costFunc,
                                  double[] tols, int maxIter, int coords, int bracket) {
        dof = degreeOfFreedom;
        costFunction = costFunc;
        tolerance = tols;
        maxIterations = maxIter;
        bracketBound = bracket;
        parent = parentAlgo;
    }

    //~ Methods --------------------------------------------------------------------------------------------------------

    /**
     * Convert a transformation vector to a transformation matrix.
     * 
     * @param vector    a transformation vector.
     * @return          a transformation matrix
     */
    public TransMatrix convertToMatrix(double[] vector){
        return convertToMatrix(getToOrigin(), getFromOrigin(), vector);
    }

    /**
     * Convert a transformation vector to a transformation matrix about the origin.
     * @param toOrigin          the matrix translating the origin to some specified point
     * @param fromOrigin        the matrix translating the origin back.
     * @param vector            a transformation vector.
     * @return                  a transformation matrix.
     */
    public abstract TransMatrix convertToMatrix(TransMatrix toOrigin, TransMatrix fromOrigin, double[] vector);
    
    /**
     * Construct a full transformation vector from the partial transformation vector.
     * For missing values in point, the values in defaultPoint will be used.
     * 
     * @param defaultPoint  a default full  transformation vector.
     * @param point         a partial or full transformation vector.
     * @return              a full transformation vector.
     */
    public abstract double[] constructPoint(double[] defaultPoint, double[] point);
    
    /**
     * Convert a transformation vector to a transformation matrix. For missing values,
     * use the values from defaultPoint.
     * 
     * @param defaultPoint  a start transformation vector.
     * @param point         a transformation vector.
     * @return              a transformation matrix.
     */
    public final TransMatrix convertToMatrix(double[] defaultPoint, double[] point){
        double[] fullPoint = constructPoint(defaultPoint, point);
        return convertToMatrix(fullPoint);
    }
    
    /**
     * Extract the partial or full transformation vector from the start transformation vector,
     * which will be optimized.
     * 
     * @param startPoint    the start full transformation vector.
     * @return              the partial or full transformation vector which will be optimized. 
     */
    public abstract double[] extractPoint(double[] startPoint);
    
    /**
     * Return the full transformation vector. 
     * @param index     the index of the transformation vector.
     * @return          the full transformation vector.
     */
    public double[] getPoint(int index){
        if(points == null){
            gov.nih.mipav.view.MipavUtil.displayError("There is no transformation vector.");
            return null;
        }
        
        if(index < 0 || index >= points.length){
            gov.nih.mipav.view.MipavUtil.displayError("The index is out of the boundary: " + index + "[" + 0 + "," + points.length + ")");
            return null;
        }
        
        double[] arHolder = points[index].getPoint();
        double[] pointsCopy = new double[arHolder.length];
        for(int i=0; i<pointsCopy.length; i++) {
        	pointsCopy[i] = arHolder[i];
        }
        
        return pointsCopy;
    }

    /**
     * Obtain the transformation vector and adjust its translation by sample parameters.
     * @param index     the index of transformation vector.
     * @param sample    the translation scaling parameter.
     * @return          the translation scaled transformation vector.
     */
    public double[] getPoint(int index, float sample){
        double[] point = getPoint(index);
        TransMatrix mat = convertToMatrix(point);

        point[3] = mat.get(0, 3) * sample;
        point[4] = mat.get(1, 3) * sample;
        point[5] = mat.get(2, 3) * sample;

        return point;
    }
    
    /**
     * Returns the cost for the transformation vector.
     * @param index     the index of transformation vector.
     * @return          the cost for the transformation vector.
     */
    public final double getCost(int index){
        if(points == null){
            gov.nih.mipav.view.MipavUtil.displayError("There is no transformation vector.");
            return Double.MAX_VALUE;
        }
        
        if(index < 0 || index >= points.length){
            gov.nih.mipav.view.MipavUtil.displayError("The index is out of the boundary: " + index + "[" + 0 + "," + points.length + ")");
            return Double.MAX_VALUE;
        }
        return points[index].getCost();
    }
    
    /**
     * Obtain the transformation vector and convert to the matrix representation.
     * @param index     the index of transformation vector.
     * @return          the transformation matrix
     */
    public final TransMatrix getMatrix(int index){
        double[] point = getPoint(index);
        return convertToMatrix(point);
    }

    /**
     * Obtain the transformation vector and convert to the matrix representation,
     * then scale the translation by sample.
     * 
     * @param index     the index of transformation vector.
     * @param sample    the translation scaling parameter.
     * @return          the scaled transformation matrix.
     */
    public abstract TransMatrix getMatrix(int index, float sample);
    
    /**
     * Adjust the translation of the transformation matrix by the sample pararmeter.
     * @param mat       the transformation matrix
     * @param sample    the resolution adjusting parameter.
     */
    public abstract void adjustTranslation(TransMatrix mat, float sample);
    
    /**
     * Measure the cost value for the given transformation vector.
     * 
     * @param point     a transformation vector.
     * @return          the cost value.
     */
    public final double measureCost(double[] point){
        if(costFunction == null){
            gov.nih.mipav.view.MipavUtil.displayError("The cost function is null.");
            return Double.MAX_VALUE;
        }
        return costFunction.cost(convertToMatrix(point));
    }

    /**
     * Measure the cost value for the given transformation matrix.
     * 
     * @param m     a transformation matrix.
     * @return      the cost value.
     */
    public final double measureCost(TransMatrix m){
        if(costFunction == null){
            gov.nih.mipav.view.MipavUtil.displayError("The cost function is null.");
            return Double.MAX_VALUE;
        }

        return costFunction.cost(m);
    }
    
    /**
     * Make a copy of the transformation vector 
     * @param point     a transformation vector.
     * @return          the copy of the transformation vector.
     */
    public final static double[] copyPoint(double[] point){
        if(point == null){
            gov.nih.mipav.view.MipavUtil.displayError("The transformation vector is null.");
            return null;
        }
        double[] copy = new double[point.length];
        System.arraycopy(point, 0, copy, 0, point.length);
        return copy;
    }
    
    /**
     * Scale the point by scale parameter and store it into another vector.
     * @param point     the transformation vector.
     * @param scale     the scale parameter.
     * @return          the scaled transformation vector
     */
    public final static double[] scalePoint(double[] point, double scale){
        if(point == null){
            gov.nih.mipav.view.MipavUtil.displayError("The transformation vector is null.");
            return null;
        }
        double[] copy = new double[point.length];
        for(int i = 0; i < point.length; i++){
            copy[i] = point[i] * scale;
        }
        return copy;
    }
    
    /**
     * Obtain the transformation vector and make a copy, then scale it by scale parameter.
     * @param index     the index of transformation vector
     * @param scale     the scale parameter
     * @return          the new scaled transformation vector.
     */
    public final double[] scalePoint(int index, double scale){
        double[] point = getPoint(index);
        return AlgorithmPowellOptBase.scalePoint(point, scale);    
    }
    
    /**
     * Sets everything to null and prepares this class for destruction.
     */
    public void disposeLocal() {
        costFunction = null;
        tolerance = null;
        toOrigin = null;
        fromOrigin = null;
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
    public void lineMinimization(double[] startPoint, double[] pt, double initial, double boundguess, double[] directions, double[]bestCost) {

        // Set up tolerances in direction of line minimization.
        // "unit_directions" is a unit vector in the direction of "directions"
        double tol = 0, sum = 0;
        double[] unit_directions = new double[dof];

        for (int i = 0; i < dof; i++) {
            sum += directions[i] * directions[i];
        }

        for (int i = 0; i < dof; i++) {
            unit_directions[i] = directions[i] / (Math.sqrt(sum));

            if (tolerance[i] > TINY) {
                tol += Math.abs(unit_directions[i] / tolerance[i]);
            }
        }

        double unit_tolerance = Math.abs(1 / tol);

        // Create new Bracket.  Set a and b and find functionAtA and functionAtB.
        Bracket bracket = new Bracket();

        bracket.a = boundguess * unit_tolerance;
        bracket.functionAtA = oneDimension(startPoint, pt, bracket.a, unit_directions);

        bracket.b = 0;

        if (initial == 0) { // for first call to lineMinimization within PowellOpt3D
            bracket.functionAtB = oneDimension(startPoint, pt, bracket.b, unit_directions);
        } else {
            bracket.functionAtB = initial;
        }

        // minimumBracket is called and will set bracket.c and functionAtC.
        minimumBracket(startPoint, pt, bracket, unit_directions);
        // if (initial == 0)  Preferences.debug("Initial bracket: \n" +bracket);

        double minDist = 0.1 * unit_tolerance;
        double xNew, yNew;
        int count = 0;

        //Profile.clear();
        //Profile.start();
        boolean bLineMinDone = false;
        if ( costFunction instanceof AlgorithmCostFunctions2D )
        {
            if ( ((AlgorithmCostFunctions2D)costFunction).isGPULineMin() )
            {
                
                float[] bracketB = ((AlgorithmCostFunctions2D)costFunction).lineMin( toOrigin, fromOrigin, 
                        0f, 2f, startPoint, pt, pt.length, unit_directions, unit_tolerance, (float)minDist,
                        bracket.a, bracket.functionAtA, bracket.b, bracket.functionAtB, 
                        bracket.c, bracket.functionAtC );
                bracket.b = bracketB[0];
                bracket.functionAtB = bracketB[1];
                bLineMinDone = true;
            }
        }
        else if ( costFunction instanceof AlgorithmCostFunctions )
        {
            if ( ((AlgorithmCostFunctions)costFunction).isGPULineMin() )
            {
                
                float[] bracketB = ((AlgorithmCostFunctions)costFunction).lineMin( toOrigin, fromOrigin, 
                        0f, 3f, startPoint, pt, pt.length, unit_directions, unit_tolerance, (float)minDist,
                        bracket.a, bracket.functionAtA, bracket.b, bracket.functionAtB, 
                        bracket.c, bracket.functionAtC );
                bracket.b = bracketB[0];
                bracket.functionAtB = bracketB[1];
                bLineMinDone = true;
            }
        }

        //Profile.stop();
        //Profile.setFileName( "profile_out"  );
        //Profile.shutdown();
        if ( !bLineMinDone )
        {

        while (((++count) < 100) && (Math.abs(bracket.c - bracket.a) > unit_tolerance) && ((parent == null)?true:!parent.isThreadStopped())) {

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

            yNew = oneDimension(startPoint, pt, xNew, unit_directions);
            //System.err.println( yNew );
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
        //System.err.println( "CPU BracketA = " + bracket.a + " " + bracket.functionAtA );
        //System.err.println( "BracketB = " + bracket.b + " " + bracket.functionAtB );
        //System.err.println( "BracketC = " + bracket.c + " " + bracket.functionAtC );
        }
        for (int i = 0; i < dof; i++) {
            pt[i] = (bracket.b * unit_directions[i]) + pt[i];
        }

        bestCost[0] = bracket.functionAtB;
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
    private Bracket minimumBracket(double[] startPoint, double[] pt, Bracket bracket, double[] unit_directions) {

        if (bracket.functionAtA == 0) {
            bracket.functionAtA = oneDimension(startPoint, pt, bracket.a, unit_directions);
        }

        if (bracket.functionAtB == 0) {
            bracket.functionAtB = oneDimension(startPoint, pt, bracket.b, unit_directions);
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
        bracket.functionAtC = oneDimension(startPoint,pt, bracket.c, unit_directions);

        while (bracket.functionAtB > bracket.functionAtC) { // note: must maintain bracket.functionAtA >=
                                                            // bracket.functionAtB
            maxX2 = bracket.b + (2 * GOLD * (bracket.c - bracket.b));
            newX2 = estimateMinimum(bracket);

            if ((Double.isNaN(newX2)) || (((newX2 - bracket.a) * dir) < 0) || (((newX2 - maxX2) * dir) > 0)) {
                newX2 = bracket.b + (GOLD * (bracket.c - bracket.a));
            }

            newY2 = oneDimension(startPoint, pt, newX2, unit_directions);

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
    private double oneDimension(double[] startPoint, double[] pt, double x, double[] unit_directions) {
        double f;
        double[] xt = new double[pt.length];

        for (int i = 0; i < pt.length; i++) {
            xt[i] = pt[i] + (x * unit_directions[i]);
        }
        double[]fullPoint = constructPoint(startPoint, xt);
        f = costFunction.cost(convertToMatrix(fullPoint));

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

    /**
     * Return an array of transformation vector. The meanings of those 
     * transformation vector are as following:
     *      the initial transformation vector:  before algorithm is performed
     *      the final transformation vector:    after algorithm was performed.
     * @return
     */
    public Vectornd[] getPoints() {
        return points;
    }

    /**
     * Sets the transformation vectors.
     * @param points    the transformation vectors.
     */
    public void setPoints(Vectornd[] points) {
        this.points = points;
    }

    /**
     * The utility function used to print information.
     * @param pt        a double array.
     * @param message   the message
     */
    public void print(double[] pt, String message) {
    	if(pt == null){
    		return;
    	}
    	System.out.println(message + "....");
    	StringBuffer sb = new StringBuffer("");
    	for(int i = 0; i < pt.length; i++){
    		sb.append(pt[i]);
    		sb.append(",");
    	}
    	System.out.println(sb.toString());
    }

    /**
     * Finds optimal point for the given initial point
     * @param v		the initial point
     */
    public void optimize(final Vectornd v) {
        if(v == null){
            return;
        }
        /**
         * Number of iterations.
         */
        int niters = 0;

        /**
         * The minimum cost.
         */
        double min = Double.MAX_VALUE;

        /**
         * Obtain the working point from initial point.
         */
        double[] point = extractPoint(v.getPoint());

        /**
         * Prepare for recording the search path.
         */
        Vector<Vector3f> path = null;
        if (pathRecorded && (dof == 3)) {
            path = new Vector<Vector3f>();
        }

        if (parallelPowell) {
            final double[][] pts = new double[dof][dof];
            final double[][] costs = new double[dof][1];
            boolean keepGoing = true;

            while ((niters < maxIterations) && keepGoing
                    && ((parent == null)?true:!parent.isThreadStopped())) {

                progress++;
                if((progress % progressModulus) == 0){
                    fireProgressStateChanged((int) (progress/progressModulus));
                }

                for (int i = 0; (i < dof) && keepGoing
                        && ((parent == null)?true:!parent.isThreadStopped()); i++) {
                    keepGoing = false;
                    for (int j = 0; j < dof; j++) {
                        System.arraycopy(point, 0, pts[j], 0, dof);
                    }
                    final CountDownLatch doneSignal = new CountDownLatch(dof);

                    for (int k = 0; k < dof; k++) {
                        final int boundGuess = bracketBound;
                        final double initialGuess = 0;
                        final double[] directions = new double[dof];

                        // directions should hold "1" for i and "0" for all
                        // other
                        // dimensions.
                        for (int j = 0; j < dof; j++) {
                            directions[j] = 0;
                        }
                        directions[k] = 1;
                        final int index = k;
                        // Preferences.debug("Calling lineMinimization for
                        // dimension
                        // "+i +".\n");
                        Runnable task = new Runnable() {
                            public void run() {
                                lineMinimization(v.getPoint(), pts[index],
                                        initialGuess, boundGuess, directions,
                                        costs[index]);
                                doneSignal.countDown();
                            }
                        };
                        MipavUtil.mipavThreadPool.execute(task);
                    }
                    try {
                        doneSignal.await();
                    } catch (InterruptedException e) {
                        e.printStackTrace();
                    }
                    // double max = Math.abs(point[0]-pts[0][0]);
                    // int index = 0;
                    // for(int k = 1; k < nDims; k++){
                    // if(max < Math.abs(point[k] - pts[k][k])){
                    // max = Math.abs(point[k] - pts[k][k]);
                    // index = k;
                    // }
                    // if (Math.abs(point[k] - pts[k][k]) > tolerance[k]){
                    // keepGoing = true;
                    // }
                    // }
                    min = costs[0][0];
                    int index = 0;
                    for (int k = 1; k < dof; k++) {
                        if (min > costs[k][0]) {
                            min = costs[k][0];
                            index = k;
                        }
                        if (Math.abs(point[k] - pts[k][k]) > tolerance[k]) {
                            keepGoing = true;
                        }
                    }
                    // if (Math.abs(point[index] - pts[index][index]) >
                    // tolerance[index]){
                    // keepGoing = true;
                    // }

                    point[index] = pts[index][index];
                    if (pathRecorded && dof == 3) {
                        path.add(new Vector3f((float)point[0], (float)point[1], (float)point[2]));
                    }
                } // end of nDims loop

                // print(point, "Initial Point");

                if ((parent == null)?false:parent.isThreadStopped()) {
                    return;
                }
                niters++;
            }

        } else {
            int boundGuess;
            double initialGuess;
            double[] originalPoint = new double[dof];
            double[] directions = new double[dof];
            boolean keepGoing = true;
            final double[] cost = new double[1];

            while ((niters < maxIterations) && keepGoing) {

                // Initialize data for testing tolerance.
                System.arraycopy(point, 0, originalPoint, 0, dof);

                /**
                 * Only terminate loop when point changes for ALL directions are
                 * less than their respective tolerances. Will only stay false
                 * if ALL are false.
                 */
                keepGoing = false;

                progress++;
                if ((progress % progressModulus) == 0) {
                    fireProgressStateChanged((int) (progress / progressModulus));
                }
                for (int i = 0; (i < dof) && ((parent == null)?true:!parent.isThreadStopped()); i++) {
                    boundGuess = bracketBound;
                    initialGuess = 0;

                    // directions should hold "1" for i and "0" for all other
                    // dimensions.
                    for (int j = 0; j < dof; j++) {
                        directions[j] = 0;
                    }

                    directions[i] = 1;

                    // Preferences.debug("Calling lineMinimization for dimension
                    // "+i +".\n");
                    lineMinimization(v.getPoint(), point, initialGuess,
                            boundGuess, directions, cost);
                    min = cost[0];
                    if (Math.abs(point[i] - originalPoint[i]) > tolerance[i]) {
                        keepGoing = true;
                    }
                    if (pathRecorded && dof == 3) {
                        path.add(new Vector3f((float)point[0], (float)point[1], (float)point[2]));
                    }
                } // end of nDims loop

                if ((parent == null)?false:parent.isThreadStopped()) {
                    return;
                }

                niters++;
            }
        }
        /**
         * Store the minimum cost and corresponding vector to v.
         */
        updatePoint(point, min, v);

        if (pathRecorded && dof == 3) {
            long threadId = Thread.currentThread().getId();
            Vector<Vector<Vector3f>> pathList = paths.get(threadId);
            if (pathList == null) {
                pathList = new Vector<Vector<Vector3f>>();
                paths.put(threadId, pathList);
            }
            pathList.add(path);
        }
    }

    /**
     * Optimize several initial points.
     * @param start
     * @param end
     */
    public void optimizeBlock(int start, int end) {
    	for(int i = start; i < end; i++){
    		Vectornd v = points[i];
    		optimize(v);
    	}
    
    }

    /**
     * Runs Powell's method. Powell's method is a way to find minimums without finding derivatives. Basically, it starts
     * at some point P in N-dimensional space, proceeds in a direction, and minimizes along that line using golden
     * search. It then resets the point and minimizes again, until the point moves by less than the tolerance. This
     * method starts with the initial point defined in the constructor and initial directions of (1 0 ... 0) (0 1 0 ...
     * ) ..., the basis vectors. At the end, "point" is the best point found and functionAtBest is the value at "point".
     */
    public void runAlgorithm() {
    	if(this.multiThreadingEnabled){
        	final CountDownLatch doneSignal = new CountDownLatch(nthreads);
    		float step = (float)(points.length)/nthreads;
    		for(int i = 0; i < nthreads; i++){
    			final int fstart = (int)(i*step);
    			int end = (int)(step*(i+1));    			
    			if(i == nthreads-1){
    				end = points.length;
    			}
    			final int fend = end;
    			Runnable task = new Runnable(){
    				public void run(){
    					optimizeBlock(fstart, fend);
    					doneSignal.countDown();
    				}
    			};
    			MipavUtil.mipavThreadPool.execute(task);
    		}
            try {
                doneSignal.await();
            } catch (InterruptedException e) {
                e.printStackTrace();
            }
    	
    	}else{
    		optimizeBlock(0, points.length);
    	}
    }

    public boolean isParallelPowell() {
    	return parallelPowell;
    }

    public void setParallelPowell(boolean parallelPowell) {
        this.parallelPowell = parallelPowell;
    }

    /**
     * Store the optimal point and cost to v 
     * @param point		the optimal point.
     * @param cost		the optimal cost.
     * @param v			the Vectornd variable used by upper level algorithm.
     */
    public abstract void updatePoint(double[] point, double cost, Vectornd v);

    public float[] createTerrain(final float transXFrom, final float transXTo,
            final float transXStep, final float transYFrom,
            final float transYTo, final float transYStep, final float rotFrom,
            final float rotTo, final float rotStep) {
        final int xdim = (int) ((transXTo - transXFrom) / transXStep);
        final int ydim = (int) ((transYTo - transYFrom) / transYStep);
        final int zdim = (int) ((rotTo - rotFrom) / rotStep);
        final float[] terrain = new float[xdim * ydim * zdim];
        final CountDownLatch doneSignal = new CountDownLatch(8);
        for (int i = 0; i < 2; i++) {
            final float zFrom = rotFrom + i * (rotTo - rotFrom) / 2;
            final float zTo = rotFrom + (i + 1) * (rotTo - rotFrom) / 2;
            final int zstart = i * zdim / 2;
            for (int j = 0; j < 2; j++) {
                final float yFrom = transYFrom + j * (transYTo - transYFrom)
                        / 2;
                final float yTo = transYFrom + (j + 1)
                        * (transYTo - transYFrom) / 2;
                final int ystart = j * ydim / 2;
                for (int k = 0; k < 2; k++) {
                    final float xFrom = transXFrom + k
                            * (transXTo - transXFrom) / 2;
                    final float xTo = transXFrom + (k + 1)
                            * (transXTo - transXFrom) / 2;
                    final int xstart = k * xdim / 2;
                    Runnable task = new Runnable() {
                        public void run() {
                            createTerrain(terrain, xFrom, xTo, transXStep,
                                    xstart, xdim, yFrom, yTo, transYStep,
                                    ystart, ydim, zFrom, zTo, rotStep, zstart,
                                    zdim);
                            doneSignal.countDown();
                        }
                    };
                    MipavUtil.mipavThreadPool.execute(task);
                }
            }
        }

        try {
            doneSignal.await();
        } catch (InterruptedException e) {
            e.printStackTrace();
        }
        return terrain;
    }

    private void createTerrain(float[] terrain, float transXFrom,
            float transXTo, float transXStep, int xstart, int xdim,
            float transYFrom, float transYTo, float transYStep, int ystart,
            int ydim, float rotFrom, float rotTo, float rotStep, int zstart,
            int zdim) {
        int nxs = (int) ((transXTo - transXFrom) / transXStep);
        int nys = (int) ((transYTo - transYFrom) / transYStep);
        int nzs = (int) ((rotTo - rotFrom) / rotStep);
        double[] pt = new double[3];
        for (int i = 0; i < nzs; i++) {
            pt[0] = rotFrom + i * rotStep;
            for (int j = 0; j < nys; j++) {
                pt[2] = transYFrom + j * transYStep;
                for (int k = 0; k < nxs; k++) {
                    pt[1] = transXFrom + k * transXStep;
                    terrain[(i + zstart) * xdim * ydim + (j + ystart) * xdim
                            + k + xstart] = (float) (255 * (1.0 - costFunction
                            .cost(convertToMatrix(pt))));
                }
            }
        }
    }


    public boolean isPathRecorded() {
    	return pathRecorded;
    }

    public void setPathRecorded(boolean pathRecorded) {
    	this.pathRecorded = pathRecorded;
    }

    public TransMatrix getFromOrigin() {
        return fromOrigin;
    }

    public TransMatrix getToOrigin() {
        return toOrigin;
    }

    public int getMaxIterations() {
        return maxIterations;
    }


}
