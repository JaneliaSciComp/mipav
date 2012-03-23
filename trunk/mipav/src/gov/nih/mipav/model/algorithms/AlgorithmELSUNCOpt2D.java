package gov.nih.mipav.model.algorithms;


import java.util.Hashtable;
import java.util.Vector;
import java.util.concurrent.CountDownLatch;

import WildMagic.LibFoundation.Mathematics.Vector2f;
import WildMagic.LibFoundation.Mathematics.Vector3f;

import gov.nih.mipav.model.algorithms.AlgorithmConstELSUNCOpt3D.FitOAR3DConstrainedModel;
import gov.nih.mipav.model.structures.TransMatrix;
import gov.nih.mipav.util.ThreadUtil;
import gov.nih.mipav.view.Preferences;


/**
 * Runs ELSUNC for a 2D image. Check the parent class comment for more detailed information.
 *
 * @version  0.1 March 22, 2012
 * @author William Gandler
 */
public class AlgorithmELSUNCOpt2D extends AlgorithmBase {

    //~ Instance fields ------------------------------------------------------------------------------------------------

    /** Flag indicating this is a rigid transformation. */
    private boolean rigid = false;
    
    /** Degress of freedom. */
    private int nDims;
    
    /** Cost function called to measure cost - 1D. */
    private AlgorithmOptimizeFunctionBase costFunction;
    
    /** Array of tolerances for each dimension. */
    private double[] tolerance;
    
    /** The maximum number of iterations the optimization allows. */
    private int maxIterations;
    
    /** Parent algorithm that called this optimization. */
    private AlgorithmBase parent;
    
    /** The transformation matrix to the origin of the input image. */
    private TransMatrix toOrigin;
    
    /** The transformation matrix from the origin of the input image. */
    private TransMatrix fromOrigin;
    
    /**
     * Array used to hold the initial points, final points and costs
     */
    private Vectornd[] points;
    
    /** The cost of the function at the best minimum. */
    private double functionAtBest;
    
    private double minFunctionAtBest;
    
    FitOAR2DModel dModel;
    
    private int status;
    
    /** Point that was initially passed into function. */
    private double[] start;
    
    /**
     * The flag to indicate whether the searching path need to be recorded.
     */
    private boolean pathRecorded = false;
    
    /**
     * Store the paths for every thread.
     */
    protected Hashtable<Long, Vector<Vector<Vector3f>>> paths = new Hashtable<Long, Vector<Vector<Vector3f>>>();

    /**
     * Constructs a new algorithm with the given centers of mass (needed for setting the transformations), the given
     * cost function (which was constructed with the proper images), the initial point we're looking at, some tolerance
     * within that point to look for the minimum, and the maximum number of iterations.
     *
     * @param  parent           Algorithm that called this optimization.
     * @param  com              Center of Mass of the input image.
     * @param  degreeOfFreedom  Degree of freedom for transformation (must be 2, 3, 4, 5, 7).
     * @param  costFunc         Cost function to use.
     * @param  tols             Tolerance for each dimension (tols.length == degreeOfFreedom).
     * @param  maxIter          Maximum number of iterations.
     * @param  _rigid           <code>true</code> means this was a rigid transformation
     */
    public AlgorithmELSUNCOpt2D(AlgorithmBase parent, Vector2f com, int degreeOfFreedom,
                                AlgorithmOptimizeFunctionBase costFunc, double[] tols, int maxIter,
                                boolean _rigid) {
    	nDims = degreeOfFreedom;
        costFunction = costFunc;
        tolerance = tols;
        maxIterations = maxIter;
        this.parent = parent;

        this.rigid = _rigid;
        toOrigin = new TransMatrix(3);
        toOrigin.setTranslate(com.X, com.Y);

        fromOrigin = new TransMatrix(3);
        fromOrigin.setTranslate(-com.X, -com.Y);
    }

    //~ Methods --------------------------------------------------------------------------------------------------------
    
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
     * Runs ELSUNC along one dimension at a time as long as the costFunction improves during one cycle
     * of runs along every dimension.
     */
    public void runAlgorithm() {
    	int i, j;
    	boolean anotherCycle = true;
    	double originalI;
        // Initialize data.
        functionAtBest = Double.MAX_VALUE;
        minFunctionAtBest = Double.MAX_VALUE;
        
        for(i = 0; i < points.length; i++){
        	if (points[i] == null) {
        		continue;
        	}
        	start = points[i].getPoint();
        	double[] point = extractPoint(points[i].getPoint());
        	
        	/**
             * Prepare for recording the search path.
             */
            Vector<Vector3f> path = null;
            if (pathRecorded && (nDims == 3)) {
                path = new Vector<Vector3f>();
            }

	        while (anotherCycle) {
	        	anotherCycle = false;
		        for (j = 0; j < nDims; j++) {
			        dModel = new FitOAR2DModel(j,point);
			        dModel.driver();
			        status = dModel.getExitStatus();
			        //dModel.statusMessage(status);
			        if (status > 0) {
			        	originalI = point[j];
				        double params[] = dModel.getParameters();
				        point[j] = params[0];
				        double[]fullPoint = getFinal(point);
				        functionAtBest = costFunction.cost(convertToMatrix(fullPoint));
				        if (functionAtBest < minFunctionAtBest) {
				        	minFunctionAtBest = functionAtBest;
				        	anotherCycle = true;
				        	if (pathRecorded && nDims == 3) {
		                        path.add(new Vector3f((float)point[0], (float)point[1], (float)point[2]));
		                    }
				        }
				        else {
				        	point[j] = originalI;
				        }
			        } // if (status > 0)
		        } // for (j = 0; j < nDims; j++)
	        } // while (anotherCycle)
	        /**
	         * Store the minimum cost and corresponding vector to v.
	         */
	        updatePoint(point, minFunctionAtBest, points[i]);

	        if (pathRecorded && nDims == 3) {
	            long threadId = Thread.currentThread().getId();
	            Vector<Vector<Vector3f>> pathList = paths.get(threadId);
	            if (pathList == null) {
	                pathList = new Vector<Vector<Vector3f>>();
	                paths.put(threadId, pathList);
	            }
	            pathList.add(path);
	        }
        } // for i = 0; i < points.length; i++)
        
    }
    
    public boolean isPathRecorded() {
    	return pathRecorded;
    }

    public void setPathRecorded(boolean pathRecorded) {
    	this.pathRecorded = pathRecorded;
    }
    
    /**
     * Accessor that returns the final point with translations, rotations, scales, and skews representing the best
     * tranformation.
     *
     * @return  vector representing the best transformation in terms of translations, rotations, scales, and skews.
     */
    public double[] getFinal(double[] point) {

        double []finalPoint = new double[start.length];
    	for (int i = 0; i < start.length; i++) {
            finalPoint[i] = start[i];
        }
        
        if(nDims == 1){
            /**
             * rotation only.
             */
            finalPoint[0] = point[0];
        } else if(nDims == 2){
            /**
             * x and y translations.
             */
            finalPoint[1] = point[0];
            finalPoint[2] = point[1];
        } else if (nDims == 3 && rigid) {
            /**
             * rotation, and x, y translations.
             */
            finalPoint[0] = point[0];
            finalPoint[1] = point[1];
            finalPoint[2] = point[2];
        } else if (nDims == 3) {
            /**
             * global scaling and x, y translations.
             */
            finalPoint[3] = finalPoint[4] = point[0];
            finalPoint[1] = point[1];
            finalPoint[2] = point[2];
        } else if (nDims == 4) {
            /**
             * Global scaling factor and x, y  translations.
             */
            finalPoint[0] = point[0];
            finalPoint[1] = point[1];
            finalPoint[2] = point[2];
            finalPoint[3] = finalPoint[4] = point[3];
        } else if (nDims > 4) {
            /**
             * rotation, x, y translations, scalings and skews.
             */
            for (int j = 0; j < point.length; j++) {
                finalPoint[j] = point[j];
            }
        }
        
        return finalPoint;
    }

    /**
     * Construct a full 7-dimension transformation vector from the partial transformation vector.
     * For missing values in point, the values in defaultPoint will be used.
     * 
     * Different degree of freedom has different meanings:
     *      2: only 2 translations
     *      3: rigid:      1 rotation and 2 translations
     *         non-rigid:    global scaling and 2 translations
     *      4: 1 rotation, 2 translations and global scaling
     *      5: 1 rotation, 2 translations and scalings
     *      7: 1 rotation, 2 translations, scalings and skews
     *      
     * @param defaultPoint  a default full 7-dimension transformation vector.
     * @param point         a partial or full transformation vector.
     * @return              a full transformation vector.
     */
    public double[] constructPoint(double[] defaultPoint, double[] point) {
    	if(point == null){
    	    gov.nih.mipav.view.MipavUtil.displayError("The transformation vector either is null!");
    		return null;
    	}
    	
    	if(defaultPoint == null || defaultPoint.length != 7){
    	    gov.nih.mipav.view.MipavUtil.displayError("The default transformation vector either is null or the length is not 7!");
    		return null;
    	}
    	double[] workingPoint = new double[defaultPoint.length];
    	System.arraycopy(defaultPoint, 0, workingPoint, 0, defaultPoint.length);
    	
    	//for ( int i = 0; i < point.length; i++ )
    	//{
    	//    System.err.print( point[i] + " " );
    	//}
    	//System.err.println("");
    	
        // set up parts of transform properly
    	if (point.length == 1) {
            workingPoint[0] = point[0];
        } else if (point.length == 2) {
            workingPoint[1] = point[0];
            workingPoint[2] = point[1];
        } else if ((point.length == 3) && (rigid == true)) {
            workingPoint[0] = point[0];
            workingPoint[1] = point[1];
            workingPoint[2] = point[2];
        } else if ((point.length == 3) && (rigid == false)) {
            workingPoint[3] = workingPoint[4] = point[0];
            workingPoint[1] = point[1];
            workingPoint[2] = point[2];
        } else if (point.length == 4) {
            workingPoint[0] = point[0];
            workingPoint[1] = point[1];
            workingPoint[2] = point[2];
            workingPoint[3] = workingPoint[4] = point[3];
        } else if (point.length > 4) {
            System.arraycopy(point, 0, workingPoint, 0, point.length);
        }
        return workingPoint;
    }
    
    /**
     * Convert a 7-dimension transformation vector to a 3x3 transformation matrix.
     * 
     * @param vector	a 7-dimension transformation vector including 1 rotation, 2 translations, 2 scalings and 2 skews.
     * @return			a 3x3 transformation matrix
     */
    public TransMatrix convertToMatrix(TransMatrix toOrigin, TransMatrix fromOrigin, double[] vector) {

		if (vector == null || vector.length != 7) {
			return null;
		}
		TransMatrix matrix = new TransMatrix(3);

		//System.err.println( vector[0] + " " + vector[1] + " " + vector[2] + " " + 0.0 + " " +
		//        vector[3] + " " + vector[4] + " " + vector[5] + " " + vector[6] );
		
		
		matrix.setTransform(vector[1], vector[2], vector[0]);
		matrix.setSkew(vector[5], vector[6]);
		matrix.setZoom(vector[3], vector[4]);

        //System.err.println( matrix.ToString() );
        matrix.MultLeft(toOrigin);
        matrix.Mult(fromOrigin);

        //System.err.println( matrix.ToString() );
        return matrix;
	}
    
    /**
     * Extract the partial or full transformation vector from the start transformation vector,
     * which will be optimized.
     * 
     * @param startPoint    the start full 7-dimension transformation vector.
     * @return              the partial or full transformation vector which will be optimized. 
     */
    public double[] extractPoint(double[] startPoint){
        if(startPoint == null || startPoint.length != 7){
            gov.nih.mipav.view.MipavUtil.displayError("The start transformation vector either is null or the length is not 7!");
            return null;
        }
        
        double[] point = new double[nDims];
        
        // set up initial point properly
        if (nDims == 1) {
            point[0] = startPoint[0]; // rotation
        }
        if (nDims == 2) {
            point[0] = startPoint[1];
            point[1] = startPoint[2];
        }

        if ((nDims == 3) && rigid) {
            point[0] = startPoint[0]; // rotation
            point[1] = startPoint[1]; // translation x
            point[2] = startPoint[2]; // translation y
        } else if (nDims == 3) {
            point[0] = startPoint[3]; // global scaling factor
            point[1] = startPoint[1]; // translation x
            point[2] = startPoint[2]; // translation y
        } else if (nDims >= 4) {
            for (int i = 0; i < nDims; i++) {
                point[i] = startPoint[i]; // 1 rotation, then 2 translations, then 2 scalings, then 2 skews
            }
        }
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
     * Accessor that sets the maximum number of iterations.
     *
     * @param  max  The max number of iterations.
     */
    public void setMaxIterations(int max) {
        maxIterations = max;
    }

    /**
     * @see AlgorithmELSUNCOptBase#adjustTranslation(TransMatrix, float)
     */
    public void adjustTranslation(TransMatrix mat, float sample){
        float transX = mat.get(0, 2) * sample;
        float transY = mat.get(1, 2) * sample;

        mat.set(0, 2, transX);
        mat.set(1, 2, transY);
    }
    
    /**
     * @see AlgorithmELSUNCOptBase#getMatrix(int, float)
     */
    public TransMatrix getMatrix(int index,float sample) {
        TransMatrix mat = getMatrix(index);
        adjustTranslation(mat, sample);
        return mat;
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
     * Convert a transformation vector to a transformation matrix.
     * 
     * @param vector    a transformation vector.
     * @return          a transformation matrix
     */
    public TransMatrix convertToMatrix(double[] vector){
        return convertToMatrix(toOrigin, fromOrigin, vector);
    }

    /**
     * @see AlgorithmELSUNCOptBase#updatePoint(double[], double, Vectornd).
     */
    public void updatePoint(double[] point, double cost, Vectornd v) {
        double[] finalPoint = v.getPoint();
        if(nDims == 1){
            /**
             * rotation only.
             */
            finalPoint[0] = point[0];
        } else if(nDims == 2){
            /**
             * x and y translations.
             */
            finalPoint[1] = point[0];
            finalPoint[2] = point[1];
        } else if (nDims == 3 && rigid) {
            /**
             * rotation, and x, y translations.
             */
            finalPoint[0] = point[0];
            finalPoint[1] = point[1];
            finalPoint[2] = point[2];
        } else if (nDims == 3) {
            /**
             * global scaling and x, y translations.
             */
            finalPoint[3] = finalPoint[4] = point[0];
            finalPoint[1] = point[1];
            finalPoint[2] = point[2];
        } else if (nDims == 4) {
            /**
             * Global scaling factor and x, y  translations.
             */
            finalPoint[0] = point[0];
            finalPoint[1] = point[1];
            finalPoint[2] = point[2];
            finalPoint[3] = finalPoint[4] = point[3];
        } else if (nDims > 4) {
            /**
             * rotation, x, y translations, scalings and skews.
             */
            for (int j = 0; j < point.length; j++) {
                finalPoint[j] = point[j];
            }
        }
        v.setCost(cost);
    }
    
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
    
    class FitOAR2DModel extends NLConstrainedEngine {
        private int currentDim;
        private double point[];
        /**
         * Creates a new FitOAR3DConstrainedModel object.
         * 
         * @param currentDim
         * Only optimize along 1 dimension at a time
         */
        public FitOAR2DModel(int currentDim, double point[]) {

            super(1, 1);
            this.currentDim = currentDim;
            this.point = point;

            bounds = 0; // bounds = 0 means unconstrained

            // bounds = 1 means same lower and upper bounds for
            // all parameters
            // bounds = 2 means different lower and upper bounds
            // for all parameters
            gues[0] = point[currentDim];
            
            // The default is internalScaling = false
            // To make internalScaling = true and have the columns of the
            // Jacobian scaled to have unit length include the following line.
            // internalScaling = true;
            // Suppress diagnostic messages
            outputMes = false;
        }

        /**
         * Starts the analysis.
         */
        public void driver() {
            super.driver();
        }

        /**
         * Display results of displaying OAR2D fitting parameters.
         */
        public void dumpResults() {
            Preferences.debug(" ******* FitOAR2DModel ********* \n\n", Preferences.DEBUG_ALGORITHM);
            Preferences.debug("Number of iterations: " + String.valueOf(iters) + "\n", Preferences.DEBUG_ALGORITHM);
            Preferences.debug("Chi-squared: " + String.valueOf(getChiSquared()) + "\n", Preferences.DEBUG_ALGORITHM);
            Preferences.debug("a0 " + String.valueOf(a[0]) + "\n", Preferences.DEBUG_ALGORITHM);
        }

        /**
         * 
         * 
         * @param a The best guess parameter values.
         * @param residuals ymodel - yData.
         * @param covarMat The derivative values of y with respect to fitting parameters.
         */
        public void fitToFunction(final double[] a, final double[] residuals, final double[][] covarMat) {
            int ctrl;
            double tempI;
            try {
                ctrl = ctrlMat[0];
                if ( (ctrl == -1) || (ctrl == 1)) {
                	tempI = point[currentDim];
                	point[currentDim] = a[0];
                	double[]fullPoint = getFinal(point);
                	point[currentDim] = tempI;
                    residuals[0] = costFunction.cost(convertToMatrix(fullPoint));
                } // if ((ctrl == -1) || (ctrl == 1))
                
                // Calculate the Jacobian numerically
                else if (ctrl == 2) {
                    ctrlMat[0] = 0;
                }
            } catch (final Exception exc) {
                Preferences.debug("function error: " + exc.getMessage() + "\n", Preferences.DEBUG_ALGORITHM);
            }

            return;
        }
    }
}
