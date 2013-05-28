package gov.nih.mipav.model.algorithms;


import gov.nih.mipav.model.algorithms.filters.*;
import gov.nih.mipav.model.structures.*;

import java.io.*;

import de.jtem.numericalMethods.calculus.function.RealFunctionOfSeveralVariables;
import de.jtem.numericalMethods.calculus.minimizing.NelderMead;


/**
 * Perform Simplex Optimization on a VOI that has been propagated to a slice to find a transformation which will make an
 * affine "fit" a structure which the VOI fit on the previous slice (used in RFASegTool when propagating liver
 * segmentation VOIs). Minimize cost = -sum of Gradient magnitude under propagated ROI in match image
 *
 * @version  1.0 July, 2003
 * @author   Evan McCreedy
 */
public class AlgorithmVOISimplexOpt extends AlgorithmBase implements RealFunctionOfSeveralVariables  {

    //~ Static fields/initializers -------------------------------------------------------------------------------------

    /** Indicates that the minimum difference cost function should be used (MAXSUM is perferred). */
    public static final int MINDIFF = 20;

    /** Indicates that the maximum summation cost function should be used (this is the perferred cost function). */
    public static final int MAXSUM = 30;

    //~ Instance fields ------------------------------------------------------------------------------------------------

    /** The sum of the gradient magnitude along the polygon. */
    protected float baseVOIGradMagSum;

    /** Which cost function to use (MINDIFF or MAXSUM). */
    protected int costFunc;

    /** The slice number the VOI is in. */
    private int currentSlice = 0;

    /** The gradient magnitude of the slice. */
    protected float[] gradMagBuf = null;

    /** The source image. */
    private ModelImage image;

    /** The polygon of the VOI we are fitting. */
    protected VOIContour inputGon;

    /** the kernel size to use in calculating the gradient magnitude image. */
    private float[] sigmas;

    /** Size of the simplex vertex array (5 == tx, ty, rot, zoomx, zoomy). */
    private int simplexDim;

    /** The image slice that holds the VOI we are working on. */
    private float[] sliceBuf;

    /** The slice size in the x dimension. */
    protected int xDim;

    /** The slice size in the y dimension. */
    protected int yDim;

    private CostFunction func = null;
    
    //~ Constructors ---------------------------------------------------------------------------------------------------

    /**
     * Set up the algorithm to perform optimizations on an image. The individual Polygons to optimize will be provided
     * later (use <code>goOptimize()</code>)
     *
     * @param  image     the image containing the VOI(s) to run the SimplexOpt algorithm on
     * @param  sigmas    the kernel size to use in calculating the gradient magnitude image
     * @param  costFunc  which cost function to use (MINDIFF or MAXSUM)
     */
    public AlgorithmVOISimplexOpt(ModelImage image, float[] sigmas, int costFunc) {
        this.image = image;
        this.sigmas = sigmas;
        this.costFunc = costFunc;
        xDim = image.getExtents()[0];
        yDim = image.getExtents()[1];
        baseVOIGradMagSum = 0f;

        simplexDim = 5;
    }

    /**
     * Set up the algorithm to perform optimizations on an image. To be used in conjunction with the <code>run</code>
     * method.
     *
     * @param  image     the image containing the VOI(s) to run the SimplexOpt algorithm on
     * @param  inputGon  the Polygon to optimize the fit of
     * @param  sigmas    the kernel size to use in calculating the gradient magnitude image
     * @param  costFunc  which cost function to use (MINDIFF or MAXSUM)
     */
    public AlgorithmVOISimplexOpt(ModelImage image, VOIContour inputGon, float[] sigmas, int costFunc) {
        this.image = image;
        this.sigmas = sigmas;
        this.costFunc = costFunc;
        this.inputGon = inputGon;
        xDim = image.getExtents()[0];
        yDim = image.getExtents()[1];
        baseVOIGradMagSum = 0f;

        simplexDim = 5;
    }

    //~ Methods --------------------------------------------------------------------------------------------------------

    /**
     * Prepares this class for destruction.
     */
    public void finalize() {
        image = null;

        gradMagBuf = null;

        super.finalize();
    }

    /**
     * Run the simplex opt algorithm on a polygon (VOI) in a slice of an image.
     *
     * @param   sliceBuf  the slice the polygon resides in
     * @param   inputGon  the polygon to optimize the fit of
     *
     * @return  the optimized polygon
     */
    public VOIContour goOptimize(float[] sliceBuf, VOIContour inputGon) {
        int i;
        this.inputGon = inputGon;


        try {
            AlgorithmGradientMagnitude gradientMagAlgo = new AlgorithmGradientMagnitude(null, sigmas, true, false);
            gradMagBuf = gradientMagAlgo.calcInBuffer2D(sliceBuf, image.getExtents(), image.isColorImage(), null, null);
        } catch (OutOfMemoryError e) {
            System.gc();
            displayError("Algorithm VOISimplexOpt: Out of memory");
            setCompleted(false);

            return null;
        }

        baseVOIGradMagSum = 0;

        for (i = 0; i < inputGon.size(); i++) {
            baseVOIGradMagSum += gradMagBuf[(int) (inputGon.elementAt(i).X + (inputGon.elementAt(i).Y * xDim))];
        }

        return search();
    }

    /**
     * Run the algorithm on a polygon already specified through the constructor.
     */
    public void runAlgorithm() {

        try {
            sliceBuf = new float[xDim * yDim];
            image.exportSliceXY(currentSlice, sliceBuf);
        } catch (IOException error) {
            displayError("Algorithm: Image(s) locked");
            setCompleted(false);

            return;
        } catch (OutOfMemoryError e) {
            System.gc();
            displayError("Algorithm: Out of memory");
            setCompleted(false);

            return;
        }

        goOptimize(sliceBuf, inputGon);
        search();
    }

    /**
     * Specify the slice containing the VOI to optimize.
     *
     * @param  currentSlice  the slice with the VOI to optimize
     */
    public void setCurrentSlice(int currentSlice) {
        this.currentSlice = currentSlice;
    }

    /**
     * Spefify the VOI polygon to optimize.
     *
     * @param  poly  the polygon
     */
    public void setPolygon(VOIContour poly) {
        inputGon = poly;
    }

    /**
     * Converts a row of p into 3x3 transformation matrix.
     *
     * @param   x  row of p[][] (one of the simplex vertices)
     *
     * @return  the transformation matrix corresponding to <code>x</code>
     */
    protected TransMatrix getTransform(double[] x) {
        TransMatrix xfrm = new TransMatrix(3);
        xfrm.setTransform(x[0], x[1], x[2]);
        xfrm.setZoom(x[3], x[4]);

        return xfrm;
    }


    /**
     * Find the best fit for the VOI polygon we have loaded into this algorithm.
     *
     * @return  the optimized polygon
     */
    private VOIContour search() {
        func = new CostFunction();
        double[] initialPoint = new double[]{0,0,1,1,1};
        NelderMead.search(initialPoint, NelderMead.getStandardBasis(initialPoint.length), 
                0.0000001, this, 50000, null);
        TransMatrix xfrm = getTransform(initialPoint);
        xfrm.Inverse();
        
        return new VOIContour( inputGon, xfrm );
    }

    //~ Inner Classes --------------------------------------------------------------------------------------------------

    /**
     * Class for specifying optimization function for the simplex opt algorithm.
     */
    private class CostFunction implements AlgorithmOptimizeFunctionBase {

        /**
         * Empty constructor.
         */
        public CostFunction() { }
        
        public int getCostFunction()
        {
            return 0;
        }
        /**
         * Filler to implement cost functions from AlgorithmOptimizeFunctionBase.
         *
         * @param       tMatrix  the translation matrix
         *
         * @return      the value of the cost function
         *
         * @deprecated  DOCUMENT ME!
         */
        public double cost(TransMatrix tMatrix) {
            return -1;
        }
        
        /**
         * Filler to implement cost functions from AlgorithmOptimizeFunctionBase.
         *
         * @param       tMatrix  the translation matrix
         *
         * @return      the value of the cost function
         *
         * @deprecated  DOCUMENT ME!
         */
        public double cost(TransMatrixd tMatrix) {
            return -1;
        }

        /**
         * Calcuated the cost function for a given simplex vertex. cost = intensity sum of errors = sum_i(I[i] - I'[i])
         *
         * @return  the value of the cost function
         *
         * @param   x  row of p[][] (a simplex vertex)
         */
        public double cost(double[] x) {
            int i;
            int X, Y;
            float value;
            double cost = 0.0f;
            TransMatrix xfrm;
            xfrm = getTransform(x);

            VOIContour tmpGon = new VOIContour( inputGon, xfrm );

            for (i = 0; i < tmpGon.size(); i++) {
                value = 0;
                X = (int)tmpGon.elementAt(i).X;

                if ((X >= 0) && (X < (xDim - 1))) {
                    Y = (int)tmpGon.elementAt(i).Y;

                    if ((Y >= 0) && (Y < (yDim - 1))) {
                        value = gradMagBuf[X + (Y * xDim)];
                    }
                }

                cost -= value; // cost = -(sum of Grad. Mag. intensities under transformed VOI)
            }

            if (costFunc == MINDIFF) {
                cost = Math.abs(baseVOIGradMagSum + cost); // minimize difference between sums under VOI
            }

            return cost;
        }
    }

    @Override
    public double eval(double[] x) {
        return func.cost(x);
    }

    @Override
    public int getNumberOfVariables() {
        return simplexDim;
    }
}
