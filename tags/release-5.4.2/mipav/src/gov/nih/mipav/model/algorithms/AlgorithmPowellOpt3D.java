package gov.nih.mipav.model.algorithms;


import WildMagic.LibFoundation.Mathematics.Vector3f;
import gov.nih.mipav.model.structures.TransMatrix;



/**
 * Runs Powell's method for a 3D image. Check the parent class comment for more detailed information.
 *
 * @version  0.1 Oct 1, 2001
 * @author   Neva Cherniavsky
 * @version  0.2 March 27, 2008
 * @author   Hailong Wang, Ph.D
 */
public class AlgorithmPowellOpt3D extends AlgorithmPowellOptBase {
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
    public AlgorithmPowellOpt3D(AlgorithmBase parent, Vector3f com, int degreeOfFreedom,
                                AlgorithmOptimizeFunctionBase costFunc, double[] tols, int maxIter) {
        super(parent, degreeOfFreedom, costFunc, tols, maxIter);

        if (degreeOfFreedom <= 12) {
            toOrigin = new TransMatrix(4);
            toOrigin.setTranslate(com.X, com.Y, com.Z);

            fromOrigin = new TransMatrix(4);
            fromOrigin.setTranslate(-com.X, -com.Y, -com.Z);
        }
    }

    //~ Methods --------------------------------------------------------------------------------------------------------

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
     * Convert a 12-dimension transformation vector to a 4x4 transformation matrix.
     * 
     * @param vector 	a 12-dimension transformation vector including 3
     * rotations, translations, scalings and skews.
     * @return			a 4x4 transformation matrix
     */
    public TransMatrix convertToMatrix(TransMatrix toOrigin, TransMatrix fromOrigin, 
                                       double[] vector) {
        if (vector == null || vector.length != 12) {
            return null;
        }
        TransMatrix matrix = new TransMatrix(4);
        
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
        
        double[] point = new double[dof];
        if (dof == 3) {
        	/**
        	 * x, y and z translation.
        	 */
            point[0] = startPoint[3];
            point[1] = startPoint[4];
            point[2] = startPoint[5];
        } else if (dof == 4) {
        	/**
        	 * Global scaling factor and x, y and z translation.
        	 */
            point[0] = startPoint[6]; // global scaling factor
            point[1] = startPoint[3]; // translation x
            point[2] = startPoint[4]; // translation y
            point[3] = startPoint[5]; // translation z
        } else if (dof >= 6) {
        	/**
        	 * x, y and z rotation, translation, scalings and skews.
        	 */
            for (int i = 0; i < dof; i++) {
                point[i] = startPoint[i];
            }
        }
        return point;
    }
    
    /**
     * @see AlgorithmPowellOptBase#getMatrix(int, float)
     */
    public TransMatrix getMatrix(int index, float sample) {
        TransMatrix mat = getMatrix(index);
        adjustTranslation(mat, sample);
        return mat;
    }

    /**
     * @see AlgorithmPowellOptBase#getMatrix(int, float)
     */
    public TransMatrix getMatrix(double[] point, float sample){
        TransMatrix mat = convertToMatrix(point);
        adjustTranslation(mat, sample);
        return mat;
    }
    
    /**
     * @see AlgorithmPowellOptBase#adjustTranslation(TransMatrix, float)
     */
    public void adjustTranslation(TransMatrix mat, float sample){
        float transX = mat.get(0, 3) * sample;
        float transY = mat.get(1, 3) * sample;
        float transZ = mat.get(2, 3) * sample;

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
    public TransMatrix getMatrixHalf(int index) {
        double [] point = scalePoint(index, 0.5);
        return convertToMatrix(point);
    }

    /**
     * Obtain the transformation vector, scale it by 0.5, then convert it to transformation
     * matrix and scale the translations by sample parameter.
     * @param index     the index of transformation vector.
     * @param sample    the translation scaling parameter.
     * @return          the scaled transformation matrix.
     */
    public TransMatrix getMatrixHalf(int index, float sample) {
        double[] point = scalePoint(index, 0.5);
        TransMatrix mat = convertToMatrix(point);
        float transX = mat.get(0, 3) * sample;
        float transY = mat.get(1, 3) * sample;
        float transZ = mat.get(2, 3) * sample;

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
     * Accessor that returns the matrix representing the best tranformation. This transformation contains only the z
     * rotation and the x and y translation, to be used in the midsagittal alignment algorithm.
     *
     * @return  matrix representing the best transformation's z rot and x and y trans.
     */
    public TransMatrix getMatrixMidsagittal(int index) {
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
    public TransMatrix getMatrixMidsagittal(int index, float sample) {
        double[] point = getPoint(index);
        double[] copy = convertToMidsagittal(point);       
        TransMatrix mat = convertToMatrix(copy);
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
}
