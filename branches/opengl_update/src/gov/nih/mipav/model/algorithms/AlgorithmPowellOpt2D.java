package gov.nih.mipav.model.algorithms;


import WildMagic.LibFoundation.Mathematics.Vector2f;

import gov.nih.mipav.model.structures.TransMatrix;


/**
 * Runs Powell's method for a 2D image. Check the parent class comment for more detailed information.
 *
 * @version  0.1 Oct 1, 2001
 * @author   Neva Cherniavsky
 * @author   Matthew McAuliffe
 * @version  0.2 March 27, 2008
 * @author   Hailong Wang, Ph.D
 */
public class AlgorithmPowellOpt2D extends AlgorithmPowellOptBase {

    //~ Instance fields ------------------------------------------------------------------------------------------------

    /** Flag indicating this is a rigid transformation. */
    private boolean rigid = false;

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
    public AlgorithmPowellOpt2D(AlgorithmBase parent, Vector2f com, int degreeOfFreedom,
                                AlgorithmOptimizeFunctionBase costFunc, double[] tols, int maxIter,
                                boolean _rigid) {
        super(parent, degreeOfFreedom, costFunc, tols, maxIter);

        this.rigid = _rigid;
        toOrigin = new TransMatrix(3);
        toOrigin.setTranslate(com.X, com.Y);

        fromOrigin = new TransMatrix(3);
        fromOrigin.setTranslate(-com.X, -com.Y);
    }

    //~ Methods --------------------------------------------------------------------------------------------------------

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
        matrix.multLeft(toOrigin).mult(fromOrigin);

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
        
        double[] point = new double[dof];
        
        // set up initial point properly
        if (dof == 1) {
            point[0] = startPoint[0]; // rotation
        }
        if (dof == 2) {
            point[0] = startPoint[1];
            point[1] = startPoint[2];
        }

        if ((dof == 3) && rigid) {
            point[0] = startPoint[0]; // rotation
            point[1] = startPoint[1]; // translation x
            point[2] = startPoint[2]; // translation y
        } else if (dof == 3) {
            point[0] = startPoint[3]; // global scaling factor
            point[1] = startPoint[1]; // translation x
            point[2] = startPoint[2]; // translation y
        } else if (dof >= 4) {
            for (int i = 0; i < dof; i++) {
                point[i] = startPoint[i]; // 1 rotation, then 2 translations, then 2 scalings, then 2 skews
            }
        }
        return point;
    }

    /**
     * @see AlgorithmPowellOptBase#adjustTranslation(TransMatrix, float)
     */
    public void adjustTranslation(TransMatrix mat, float sample){
        float transX = mat.get(0, 2) * sample;
        float transY = mat.get(1, 2) * sample;

        mat.set(0, 2, transX);
        mat.set(1, 2, transY);
    }
    
    /**
     * @see AlgorithmPowellOptBase#getMatrix(int, float)
     */
    public TransMatrix getMatrix(int index,float sample) {
        TransMatrix mat = getMatrix(index);
        adjustTranslation(mat, sample);
        return mat;
    }

    /**
     * @see AlgorithmPowellOptBase#updatePoint(double[], double, Vectornd).
     */
    public void updatePoint(double[] point, double cost, Vectornd v) {
        double[] finalPoint = v.getPoint();
        if(dof == 1){
            /**
             * rotation only.
             */
            finalPoint[0] = point[0];
        } else if(dof == 2){
            /**
             * x and y translations.
             */
            finalPoint[1] = point[0];
            finalPoint[2] = point[1];
        } else if (dof == 3 && rigid) {
            /**
             * rotation, and x, y translations.
             */
            finalPoint[0] = point[0];
            finalPoint[1] = point[1];
            finalPoint[2] = point[2];
        } else if (dof == 3) {
            /**
             * global scaling and x, y translations.
             */
            finalPoint[3] = finalPoint[4] = point[0];
            finalPoint[1] = point[1];
            finalPoint[2] = point[2];
        } else if (dof == 4) {
            /**
             * Global scaling factor and x, y  translations.
             */
            finalPoint[0] = point[0];
            finalPoint[1] = point[1];
            finalPoint[2] = point[2];
            finalPoint[3] = finalPoint[4] = point[3];
        } else if (dof > 4) {
            /**
             * rotation, x, y translations, scalings and skews.
             */
            for (int j = 0; j < point.length; j++) {
                finalPoint[j] = point[j];
            }
        }
        v.setCost(cost);
    }
}
