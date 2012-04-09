package gov.nih.mipav.model.algorithms;

import WildMagic.LibFoundation.Mathematics.*;

import de.jtem.numericalMethods.calculus.minimizing.Powell;
import de.jtem.numericalMethods.calculus.function.RealFunctionOfSeveralVariables;
import de.jtem.numericalMethods.calculus.minimizing.BrentOnLine;
import gov.nih.mipav.model.structures.TransMatrix;
import gov.nih.mipav.util.ThreadUtil;
import gov.nih.mipav.view.Preferences;

import java.util.Hashtable;
import java.util.Vector;
import java.util.concurrent.CountDownLatch;
//import com.mentorgen.tools.profile.runtime.Profile;


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
 * @version  0.1 Oct 1, 2001
 * @author   Neva Cherniavsky
 * @version  0.2 March 27, 2008
 * @author   Hailong Wang, Ph.D
 */

public abstract class AlgorithmPowellOptBase extends AlgorithmBase implements RealFunctionOfSeveralVariables{

    //~ Static fields/initializers -------------------------------------------------------------------------------------

    /** Used to prevent division by zero. */
    private static final double TINY = 1.0 * Math.pow(10, -20);

    //~ Instance fields ------------------------------------------------------------------------------------------------


    /** Cost function called to measure cost - 1D. */
    protected AlgorithmOptimizeFunctionBase costFunction;

    /** The transformation matrix from the origin of the input image. */
    protected TransMatrix fromOrigin;

    /** The maximum number of iterations the optimization allows. */
    protected int maxIterations;

    /** Degrees of freedom. */
    protected int dof;

    /** Parent algorithm that called this optimization. */
    protected AlgorithmBase parent;

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

    /** When true, JTEM Powell.search is used, otherwise, JTEM BrentOnline.search is used. */
    protected boolean useJTEM = false;

    /**
     * Store the paths for every thread.
     */
    protected Hashtable<Long, Vector<Vector<Vector3f>>> paths = new Hashtable<Long, Vector<Vector<Vector3f>>>();

    /**
     * The flag to indicate whether the searching path need to be recorded.
     */
    protected boolean pathRecorded = false;
    
    /** The different searches change the number of variables in the transformation matrix that change. This
      fills in the rest of the 'constant' portion of the matrix.*/
    double[] savedStartPoint;

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
     */
    public AlgorithmPowellOptBase(AlgorithmBase parentAlgo, int degreeOfFreedom, AlgorithmOptimizeFunctionBase costFunc,
                                  double[] tols, int maxIter) {
        dof = degreeOfFreedom;
        costFunction = costFunc;
        tolerance = tols;
        maxIterations = maxIter;
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
     * @param pt
     * @param directions
     * @param bestCost
     */
    public void lineMinimization(double[] pt, double[] directions, double[]bestCost) { 
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
        BrentOnLine brentOnLine = new BrentOnLine(pt, directions, this);
        bestCost[0] = brentOnLine.search(unit_tolerance);
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
        
        if ( useJTEM ) {
        	//System.err.println( "using JTEM" );
            double[][] xi = new double[dof][dof];
            for (int i = 0; i < dof; i++) {
                xi[i][i] = 1.0;
                //System.err.print(" " + tolerance[i]);
            }
            //System.err.println(" ");
            
            savedStartPoint = v.getPoint();
            Powell.search( point, xi, tolerance, this, maxIterations, null );
        }
        else if (parallelPowell) {
        	//System.err.println( "using Parallel Powell" );
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
                    savedStartPoint = new double[v.getPoint().length];
                    for ( int j = 0; j < v.getPoint().length; j++ )
                    {
                    	savedStartPoint[j] = v.getPoint()[j];
                    }
                    final CountDownLatch doneSignal = new CountDownLatch(dof);

                    for (int k = 0; k < dof; k++) {
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
                        // "+i +".\n", Preferences.DEBUG_ALGORITHM);
                        Runnable task = new Runnable() {
                            public void run() {
                                lineMinimization(pts[index], directions,
                                        costs[index]);
                                doneSignal.countDown();
                            }
                        };
                        ThreadUtil.mipavThreadPool.execute(task);
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

        } 
        else {
        	//System.err.println( "using MIPAV Powell" );
            double[] originalPoint = new double[dof];
            double[] directions = new double[dof];
            boolean keepGoing = true;
            final double[] cost = new double[1];

            while ((niters < maxIterations) && keepGoing) {

                // Initialize data for testing tolerance.
                System.arraycopy(point, 0, originalPoint, 0, dof);

                //
                // Only terminate loop when point changes for ALL directions are
                // less than their respective tolerances. Will only stay false
                // if ALL are false.
                //
                keepGoing = false;

                progress++;
                if ((progress % progressModulus) == 0) {
                    fireProgressStateChanged((int) (progress / progressModulus));
                }
                savedStartPoint = new double[v.getPoint().length];
                for ( int i = 0; i < v.getPoint().length; i++ )
                {
                	savedStartPoint[i] = v.getPoint()[i];
                }
                for (int i = 0; (i < dof) && ((parent == null)?true:!parent.isThreadStopped()); i++) {
                    // directions should hold "1" for i and "0" for all other
                    // dimensions.
                    for (int j = 0; j < dof; j++) {
                        directions[j] = 0;
                    }

                    directions[i] = 1;

                    // Preferences.debug("Calling lineMinimization for dimension
                    // "+i +".\n", Preferences.DEBUG_ALGORITHM);
                    lineMinimization(point, directions, cost);
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
    	// Cannot use multiThreading in runAlgorithm because this would result in multiple copies of optimizeBlock and
    	// optimize running at the same time.  But each version of optimize would use the same global savedStartPoint,
    	// leading to a conflict.
    	optimizeBlock(0, points.length);
    }

    public boolean isParallelPowell() {
    	return parallelPowell;
    }

    public void setParallelPowell(boolean parallelPowell) {
        this.parallelPowell = parallelPowell;
    }

    public void setUseJTEM(boolean bOn) {
        this.useJTEM = bOn;
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
                    ThreadUtil.mipavThreadPool.execute(task);
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
    

    @Override
    public double eval(double[] x) {
        // The different searches change the number of variables in the
        // transformation matrix that change. The savedStartPoint
        // fills in the rest of the 'constant' portion of the matrix.
        double[]fullPoint = constructPoint(savedStartPoint, x);
        double r = costFunction.cost(convertToMatrix(fullPoint));
        //System.err.println( fullPoint[0] + " " + r );
        return r;
    }


    @Override
    public int getNumberOfVariables() {
        return dof;
    }


}
