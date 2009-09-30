package gov.nih.mipav.model.algorithms;


import gov.nih.mipav.model.algorithms.filters.*;
import gov.nih.mipav.model.structures.*;

import java.awt.*;

import java.io.*;


/**
 * Perform Simplex Optimization on a VOI that has been propagated to a slice to find a transformation which will make an
 * affine "fit" a structure which the VOI fit on the previous slice (used in RFASegTool when propagating liver
 * segmentation VOIs). Minimize cost = -sum of Gradient magnitude under propagated ROI in match image
 *
 * @version  1.0 July, 2003
 * @author   Evan McCreedy
 */
public class AlgorithmVOISimplexOpt extends AlgorithmBase {

    //~ Static fields/initializers -------------------------------------------------------------------------------------

    /** Indicates that the minimum difference cost function should be used (MAXSUM is perferred). */
    public static final int MINDIFF = 20;

    /** Indicates that the maximum summation cost function should be used (this is the perferred cost function). */
    public static final int MAXSUM = 30;

    //~ Instance fields ------------------------------------------------------------------------------------------------

    /** The sum of the gradient magnitude along the polygon. */
    private float baseVOIGradMagSum;

    /** Which cost function to use (MINDIFF or MAXSUM). */
    private int costFunc;

    /** The slice number the VOI is in. */
    private int currentSlice = 0;

    /** The gradient magnitude of the slice. */
    private float[] gradMagBuf = null;

    /** The source image. */
    private ModelImage image;

    /** The polygon of the VOI we are fitting. */
    private Polygon inputGon;

    /** the kernel size to use in calculating the gradient magnitude image. */
    private float[] sigmas;

    /** The algorithm which takes our cost function and other parameters and performs the optimization. */
    private AlgorithmSimplexOpt simplex;

    /** Size of the simplex vertex array (5 == tx, ty, rot, zoomx, zoomy). */
    private int simplexDim;

    /** The image slice that holds the VOI we are working on. */
    private float[] sliceBuf;

    /** The slice size in the x dimension. */
    private int xDim;

    /** The slice size in the y dimension. */
    private int yDim;

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
    public AlgorithmVOISimplexOpt(ModelImage image, Polygon inputGon, float[] sigmas, int costFunc) {
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

        if (simplex != null) {
            simplex.finalize();
        }

        simplex = null;
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
    public Polygon goOptimize(float[] sliceBuf, Polygon inputGon) {
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

        for (i = 0; i < inputGon.npoints; i++) {
            baseVOIGradMagSum += gradMagBuf[(int) (inputGon.xpoints[i] + (inputGon.ypoints[i] * xDim))];
        }

        return Search();
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
        Search();
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
    public void setPolygon(Polygon poly) {
        inputGon = poly;
    }

    /**
     * Converts a row of p into 3x3 transformation matrix.
     *
     * @param   x  row of p[][] (one of the simplex vertices)
     *
     * @return  the transformation matrix corresponding to <code>x</code>
     */
    private TransMatrix getTransform(double[] x) {
        TransMatrix xfrm = new TransMatrix(3);
        xfrm.setTransform(x[0], x[1], x[2]);
        xfrm.setZoom(x[3], x[4]);

        return xfrm;
    }

    /**
     * Set up the array of simplex vertices and the cost array.
     *
     * @param  p     the array of simplex vertices p[i][j] where each i is a simplex vertex and each j is a parameter
     *               tx, ty, rot, zoomx, zoomy
     * @param  y     the cost array
     * @param  func  the cost function to use to set up the cost array
     */
    private void InitializePandY(double[][] p, double[] y, CostFunction func) {
        int i, j;

        for (i = 0; i < (simplexDim + 1); i++) {

            for (j = 0; j < simplexDim; j++) {
                p[i][j] = 0f;
            }
        }

        p[0][0] = 5f; // Tx
        p[0][1] = 5f; // Ty
        p[0][2] = 0f; // Rot about z-axis (degrees)
        p[0][3] = 1f; // ZoomX
        p[0][4] = 1f; // ZoomY

        p[1][0] = -5f;
        p[1][1] = -5f;
        p[1][2] = 0f;
        p[1][3] = 1f;
        p[1][4] = 1f;

        p[2][0] = 0f;
        p[2][1] = 0f;
        p[2][2] = 1f;
        p[2][3] = 0.95f;
        p[2][4] = 0.95f;

        p[3][0] = 1f;
        p[3][1] = 1f;
        p[3][2] = 1f;
        p[3][3] = 1.05f;
        p[3][4] = 1.05f;

        p[4][0] = 0f;
        p[4][1] = 0f;
        p[4][2] = 0f;
        p[4][3] = 1f;
        p[4][4] = 1f;

        for (i = 0; i < simplexDim; i++) {
            y[i] = func.cost(p[i]);
        }
    }

    /**
     * Find the best fit for the VOI polygon we have loaded into this algorithm.
     *
     * @return  the optimized polygon
     */
    private Polygon Search() {
        TransMatrix xfrm;
        CostFunction func = null;
        double[][] p = null;
        double[] y = null;

        try {
            func = new CostFunction();
            p = new double[simplexDim + 1][simplexDim];
            y = new double[simplexDim + 1];
            simplex = new AlgorithmSimplexOpt(p, y, simplexDim, func);
        } catch (OutOfMemoryError e) {
            displayError("Algorithm VOI Simplex:  Out of Memory");
            setCompleted(false);

            return null;
        }

        InitializePandY(p, y, func);
        simplex.setP(p);
        simplex.setY(y);
        simplex.setRunningInSeparateThread(runningInSeparateThread);
        simplex.run();
        xfrm = getTransform(p[0]); // lowest cost row
        xfrm.Inverse();

        return xfrm.transform(inputGon);
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

            Polygon tmpGon = xfrm.transform(inputGon);

            for (i = 0; i < tmpGon.npoints; i++) {
                value = 0;
                X = tmpGon.xpoints[i];

                if ((X >= 0) && (X < (xDim - 1))) {
                    Y = tmpGon.ypoints[i];

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
}
