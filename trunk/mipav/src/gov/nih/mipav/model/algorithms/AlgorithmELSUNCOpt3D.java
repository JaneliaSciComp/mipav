package gov.nih.mipav.model.algorithms;


import WildMagic.LibFoundation.Mathematics.Vector3f;
import gov.nih.mipav.model.structures.TransMatrixd;
import gov.nih.mipav.view.Preferences;



/**
 * Runs ELSUNC for a 3D image.
 *
 * @version  0.1 March 23, 2012
 * @author   William Gandler
 */
public class AlgorithmELSUNCOpt3D extends AlgorithmBase {
	
	//~ Instance fields ------------------------------------------------------------------------------------------------
    
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
    private TransMatrixd toOrigin;
    
    /** The transformation matrix from the origin of the input image. */
    private TransMatrixd fromOrigin;
    
    /**
     * Array used to hold the initial points, final points and costs
     */
    private Vectornd[] points;
    
    /** The cost of the function at the best minimum. */
    private double functionAtBest;
    
    private double minFunctionAtBest;
    
    FitOAR3DModel dModel;
    
    private int status;
    
    /** Point that was initially passed into function. */
    private double[] start;
    
	/**
     * Constructs a new algorithm with the given centers of mass (needed for setting the transformations), the given
     * cost function (which was constructed with the proper images), the initial point we're looking at, some tolerance
     * within that point to look for the minimum, and the maximum number of iterations.
     *
     * @param  parent           Algorithm that called this optimization.
     * @param  com              Center of Mass of the input image.
     * @param  degreeOfFreedom  Degree of freedom for transformation (must be 3, 4, 6, 7, 9, or 12).
     * @param  costFunc         Cost function to use.
     * @param  tols             Tolerance for each dimension (tols.length == degreeOfFreedom).
     * @param  maxIter          Maximum number of iterations.
     */
    public AlgorithmELSUNCOpt3D(AlgorithmBase parent, Vector3f com, int degreeOfFreedom,
                                AlgorithmOptimizeFunctionBase costFunc, double[] tols, int maxIter) {
    	nDims = degreeOfFreedom;
        costFunction = costFunc;
        tolerance = tols;
        maxIterations = maxIter;
        this.parent = parent;

        if (degreeOfFreedom <= 12) {
            toOrigin = new TransMatrixd(4);
            toOrigin.setTranslate(com.X, com.Y, com.Z);

            fromOrigin = new TransMatrixd(4);
            fromOrigin.setTranslate(-com.X, -com.Y, -com.Z);
        }
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
    	double originalJ;
        // Initialize data.
        functionAtBest = Double.MAX_VALUE;
        minFunctionAtBest = Double.MAX_VALUE;
        int cycles;
        int maxCycles = 6;
        
        for(i = 0; i < points.length; i++){
        	if (points[i] == null) {
        		continue;
        	}
        	anotherCycle = true;
        	start = points[i].getPoint();
        	double[] point = extractPoint(points[i].getPoint());

        	cycles = 0;
        	while (anotherCycle) {
        		cycles++;
        		if (cycles > maxCycles) {
        			break;
        		}
	        	anotherCycle = false;
		        for (j = 0; j < nDims; j++) {
		        	originalJ = point[j];
			        dModel = new FitOAR3DModel(j,point);
			        dModel.driver();
			        status = dModel.getExitStatus();
			        //dModel.statusMessage(status);
			        // status == -2 if maxAIterations reached
			        if ((status > 0) || (status == -2)){
				        double params[] = dModel.getParameters();
				        point[j] = params[0];
				        double[]fullPoint = getFinal(point);
				        functionAtBest = costFunction.cost(convertToMatrix(fullPoint));
				        if (functionAtBest < minFunctionAtBest) {
				        	minFunctionAtBest = functionAtBest;
				        	anotherCycle = true;
				        }
				        else {
				        	point[j] = originalJ;
				        }
			        } // if (status > 0)
			        else {
			        	point[j] = originalJ;
			        }
		        } // for (j = 0; j < nDims; j++)
	        } // while (anotherCycle)
	        /**
	         * Store the minimum cost and corresponding vector to v.
	         */
	        updatePoint(point, minFunctionAtBest, points[i]);

        } // for i = 0; i < points.length; i++)
        
    }

    /**
     * Construct a full 12-dimension transformation vector from the partial transformation vector.
     * For missing values in point, the values in defaultPoint will be used.
     * 
     * Different degree of freedom has different meanings:
     *      3: only 3 translations
     *      4: 3 translation and global scaling
     *      6: 3 rotations and translations
     *      7: 3 rotations and translations, and global scaling
     *      9: 3 rotations, translations and scalings
     *      12: 3 rotations, translations, scalings and skewings.
     *      
     * @param defaultPoint  a default full 12-dimension transformation vector.
     * @param point         a partial or full transformation vector.
     * @return              a full transformation vector.
     */
    public double[] constructPoint(double[] defaultPoint, double[] point){
        if(defaultPoint == null || defaultPoint.length != 12){
            gov.nih.mipav.view.MipavUtil.displayError("The default vector either is null or has incorrect dimension.");
            return null;
        }

        
        if(point == null || (point.length != 3 && point.length != 4
                && point.length != 6 && point.length != 7 && point.length != 9
                && point.length != 12)){
            gov.nih.mipav.view.MipavUtil.displayError("The vector either is null or has incorrect dimension.");
            return null;
        }
        // 3 rotations, then 3 translations, then 3 scalings, then 3 skews
        // = 6       + 1 scale = 7   + 3 scales = 9   + 3 skews = 12
        double[] fullPoint = new double[defaultPoint.length];
        System.arraycopy(defaultPoint, 0, fullPoint, 0, fullPoint.length);

        //System.err.print( "DefaultPoint = "  );
        //for ( int i = 0; i < defaultPoint.length; i++ )
        //{
         //   System.err.print( defaultPoint[i] + " " );
        //}
        //System.err.println("");
        
        // set up parts of transform properly
        if (point.length == 3) {
            fullPoint[3] = point[0];
            fullPoint[4] = point[1];
            fullPoint[5] = point[2];
        } else if (point.length == 4) {
            fullPoint[3] = point[1];
            fullPoint[4] = point[2];
            fullPoint[5] = point[3];
            fullPoint[6] = fullPoint[7] = fullPoint[8] = point[0];
        } else if ((point.length == 6) || (point.length == 7) 
                || (point.length == 9) || (point.length == 12)) {
            fullPoint[0] = point[0];
            fullPoint[1] = point[1];
            fullPoint[2] = point[2];
            fullPoint[3] = point[3];
            fullPoint[4] = point[4];
            fullPoint[5] = point[5];

            if (point.length == 7) {
                fullPoint[6] = fullPoint[7] = fullPoint[8] = point[6];
            } else if (point.length > 7) {
                fullPoint[6] = point[6];
                fullPoint[7] = point[7];
                fullPoint[8] = point[8];

                if (point.length > 9) {
                    fullPoint[9] = point[9];
                    fullPoint[10] = point[10];
                    fullPoint[11] = point[11];
                }
            }
        }
        return fullPoint;
    }
    
    /**
     * Accessor that returns the final point with translations, rotations, scales, and skews representing the best
     * tranformation.
     *
     * @return  vector representing the best transformation in terms of translations, rotations, scales, and skews.
     */
    public double[] getFinal(double[] point) {

        double[] finalPoint = new double[start.length];
    	for (int i = 0; i < start.length; i++) {
            finalPoint[i] = start[i];
        }
        
    	if (point.length == 3) {
            finalPoint[3] = point[0];
            finalPoint[4] = point[1];
            finalPoint[5] = point[2];
        } else if (point.length == 4) {
            finalPoint[3] = point[1];
            finalPoint[4] = point[2];
            finalPoint[5] = point[3];
            finalPoint[6] = finalPoint[7] = finalPoint[8] = point[0];
        } else if ((point.length == 6) || (point.length == 7) 
                || (point.length == 9) || (point.length == 12)) {
            finalPoint[0] = point[0];
            finalPoint[1] = point[1];
            finalPoint[2] = point[2];
            finalPoint[3] = point[3];
            finalPoint[4] = point[4];
            finalPoint[5] = point[5];

            if (point.length == 7) {
                finalPoint[6] = finalPoint[7] = finalPoint[8] = point[6];
            } else if (point.length > 7) {
                finalPoint[6] = point[6];
                finalPoint[7] = point[7];
                finalPoint[8] = point[8];

                if (point.length > 9) {
                    finalPoint[9] = point[9];
                    finalPoint[10] = point[10];
                    finalPoint[11] = point[11];
                }
            }
        }
        
        return finalPoint;
    }

    /**
     * Convert a 12-dimension transformation vector to a 4x4 transformation matrix.
     * 
     * @param vector 	a 12-dimension transformation vector including 3
     * rotations, translations, scalings and skews.
     * @return			a 4x4 transformation matrix
     */
    public TransMatrixd convertToMatrix(TransMatrixd toOrigin, TransMatrixd fromOrigin, 
                                       double[] vector) {
        if (vector == null || vector.length != 12) {
            return null;
        }
        TransMatrixd matrix = new TransMatrixd(4);
        
        matrix.setTransform(vector[3], vector[4], vector[5], vector[0],
                            vector[1], vector[2], vector[6], vector[7], vector[8],
                            vector[9], vector[10], vector[11]);

        
        
        matrix.MultLeft(toOrigin);
        matrix.Mult(fromOrigin);
        //System.err.println( "Matrix = " + matrix.ToString() );
        
        return matrix;
        
    } 

    /**
     * Extract the partial or full transformation vector from the start transformation vector,
     * which will be optimized.
     * 
     * @param startPoint    the start full 12-dimension transformation vector.
     * @return              the partial or full transformation vector which will be optimized. 
     */
    public double[] extractPoint(double[] startPoint){
        if(startPoint == null || startPoint.length != 12){
            gov.nih.mipav.view.MipavUtil.displayError("The start transformation vector either is null or the length is not 12!");
            return null;
        }
        
        double[] point = new double[nDims];
        if (nDims == 3) {
        	/**
        	 * x, y and z translation.
        	 */
            point[0] = startPoint[3];
            point[1] = startPoint[4];
            point[2] = startPoint[5];
        } else if (nDims == 4) {
        	/**
        	 * Global scaling factor and x, y and z translation.
        	 */
            point[0] = startPoint[6]; // global scaling factor
            point[1] = startPoint[3]; // translation x
            point[2] = startPoint[4]; // translation y
            point[3] = startPoint[5]; // translation z
        } else if (nDims >= 6) {
        	/**
        	 * x, y and z rotation, translation, scalings and skews.
        	 */
            for (int i = 0; i < nDims; i++) {
                point[i] = startPoint[i];
            }
        }
        return point;
    }
    
    /**
     * @see AlgorithmPowellOptBase#getMatrix(int, float)
     */
    public TransMatrixd getMatrix(int index, float sample) {
        TransMatrixd mat = getMatrix(index);
        adjustTranslation(mat, sample);
        return mat;
    }
    
    /**
     * Obtain the transformation vector and convert to the matrix representation.
     * @param index     the index of transformation vector.
     * @return          the transformation matrix
     */
    public final TransMatrixd getMatrix(int index){
        double[] point = getPoint(index);
        return convertToMatrix(point);
    }
    
    /**
     * Convert a transformation vector to a transformation matrix.
     * 
     * @param vector    a transformation vector.
     * @return          a transformation matrix
     */
    public TransMatrixd convertToMatrix(double[] vector){
        return convertToMatrix(toOrigin, fromOrigin, vector);
    }
    
    public final double measureCost(double[] point){
        if(costFunction == null){
            gov.nih.mipav.view.MipavUtil.displayError("The cost function is null.");
            return Double.MAX_VALUE;
        }
        return costFunction.cost(convertToMatrix(point));
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
        TransMatrixd mat = convertToMatrix(point);

        point[3] = mat.get(0, 3) * sample;
        point[4] = mat.get(1, 3) * sample;
        point[5] = mat.get(2, 3) * sample;

        return point;
    }

    /**
     * @see AlgorithmPowellOptBase#getMatrix(int, float)
     */
    public TransMatrixd getMatrix(double[] point, float sample){
        TransMatrixd mat = convertToMatrix(point);
        adjustTranslation(mat, sample);
        return mat;
    }
    
    /**
     * @see AlgorithmPowellOptBase#adjustTranslation(TransMatrix, float)
     */
    public void adjustTranslation(TransMatrixd mat, float sample){
        double transX = mat.get(0, 3) * sample;
        double transY = mat.get(1, 3) * sample;
        double transZ = mat.get(2, 3) * sample;

        mat.set(0, 3, transX);
        mat.set(1, 3, transY);
        mat.set(2, 3, transZ);
    }
    /**
     * Obtain the transformation vector, scale it by 0.5, then convert it to transformation
     * matrix.
     * @param index     the index of transformation vector.
     * @return          the scaled transformation matrix.
     */
    public TransMatrixd getMatrixHalf(int index) {
        double [] point = scalePoint(index, 0.5);
        return convertToMatrix(point);
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
     * Obtain the transformation vector, scale it by 0.5, then convert it to transformation
     * matrix and scale the translations by sample parameter.
     * @param index     the index of transformation vector.
     * @param sample    the translation scaling parameter.
     * @return          the scaled transformation matrix.
     */
    public TransMatrixd getMatrixHalf(int index, float sample) {
        double[] point = scalePoint(index, 0.5);
        TransMatrixd mat = convertToMatrix(point);
        double transX = mat.get(0, 3) * sample;
        double transY = mat.get(1, 3) * sample;
        double transZ = mat.get(2, 3) * sample;

        mat.set(0, 3, transX);
        mat.set(1, 3, transY);
        mat.set(2, 3, transZ);
        return mat;
    }

    /**
     * Converts a full transformation vector into a midsagittal transformation vector.
     * @param point     the full 12-dimension transformation vector.
     * @return          the transformation vector complying with midsagittal alignment algorithm.
     */
    public double[] convertToMidsagittal(double[] point){
        if(point == null || point.length != 12){
            gov.nih.mipav.view.MipavUtil.displayError("The start transformation vector either is null or the length is not 12!");
            return null;
        }
        double[] copy = copyPoint(point);
        
        /**
         * 3 rotations
         */
        copy[0] /= 2;
        copy[1] /= 2;
        copy[2] /= 2;
        
        /**
         * 3 translations
         */
        copy[3] /= 2;
        copy[4] /= 2;
        copy[5] = 0;
        
        /**
         * 3 scalings
         */
        copy[6] = 1;
        copy[7] = 1;
        copy[8] = 1;
        
        /**
         * 3 skews
         */
        copy[9] = 0;
        copy[10] = 0;
        copy[11] = 0;
        return copy;
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
     * Accessor that returns the matrix representing the best tranformation. This transformation contains only the z
     * rotation and the x and y translation, to be used in the midsagittal alignment algorithm.
     *
     * @return  matrix representing the best transformation's z rot and x and y trans.
     */
    public TransMatrixd getMatrixMidsagittal(int index) {
        double[] point = getPoint(index);
        double[] copy = convertToMidsagittal(point);       
        return convertToMatrix(copy);
    }

    /**
     * Accessor that returns the matrix representing the best tranformation. The passed in parameter represents the
     * resolution (same in all directions and for both input and reference images, since resampled isotropically). Since
     * the optimization was done in pixel space, not millimeter space, the translation parameters need to be scaled by
     * the sample value. This transformation contains only the z rotation and the x and y translation, to be used in the
     * midsagittal alignment algorithm.
     *
     * @param   sample  the voxel resolution
     *
     * @return  matrix representing the best transformation's z rot and x and y trans.
     */
    public TransMatrixd getMatrixMidsagittal(int index, float sample) {
        double[] point = getPoint(index);
        double[] copy = convertToMidsagittal(point);       
        TransMatrixd mat = convertToMatrix(copy);
        adjustTranslation(mat, sample);
        return mat;
    }

    /**
     * @see AlgorithmPowellOptBase#updatePoint(double[], double, Vectornd)
     */
    public void updatePoint(double[] point, double cost, Vectornd v) {
        double[] fullPoint = v.getPoint();

        if (point.length == 3) {
            fullPoint[3] = point[0];
            fullPoint[4] = point[1];
            fullPoint[5] = point[2];
        } else if (point.length == 4) {
            fullPoint[3] = point[1];
            fullPoint[4] = point[2];
            fullPoint[5] = point[3];
            fullPoint[6] = fullPoint[7] = fullPoint[8] = point[0];
        } else if ((point.length == 6) || (point.length == 7) 
                || (point.length == 9) || (point.length == 12)) {
            fullPoint[0] = point[0];
            fullPoint[1] = point[1];
            fullPoint[2] = point[2];
            fullPoint[3] = point[3];
            fullPoint[4] = point[4];
            fullPoint[5] = point[5];

            if (point.length == 7) {
                fullPoint[6] = fullPoint[7] = fullPoint[8] = point[6];
            } else if (point.length > 7) {
                fullPoint[6] = point[6];
                fullPoint[7] = point[7];
                fullPoint[8] = point[8];

                if (point.length > 9) {
                    fullPoint[9] = point[9];
                    fullPoint[10] = point[10];
                    fullPoint[11] = point[11];
                }
            }
        }
        v.setCost(cost);
    }
    
    /**
     * Accessor that sets the maximum number of iterations.
     *
     * @param  max  The max number of iterations.
     */
    public void setMaxIterations(int max) {
        maxIterations = max;
    }
    
    public int getMaxIterations() {
        return maxIterations;
    }
    
    class FitOAR3DModel extends NLConstrainedEngine {
        private int currentDim;
        private double point[];
        /**
         * Creates a new FitOAR3DConstrainedModel object.
         * 
         * @param currentDim
         * Only optimize along 1 dimension at a time
         */
        public FitOAR3DModel(int currentDim, double point[]) {

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
            maxIterations = 20;
        }

        /**
         * Starts the analysis.
         */
        public void driver() {
            super.driver();
        }

        /**
         * Display results of displaying OAR3D fitting parameters.
         */
        public void dumpResults() {
            Preferences.debug(" ******* FitOAR3DModel ********* \n\n", Preferences.DEBUG_ALGORITHM);
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
            try {
                ctrl = ctrlMat[0];
                if ( (ctrl == -1) || (ctrl == 1)) {
                	point[currentDim] = a[0];
                	double[]fullPoint = getFinal(point);
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
