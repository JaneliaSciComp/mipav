package gov.nih.mipav.model.algorithms;


import gov.nih.mipav.model.structures.ModelImage;
import gov.nih.mipav.view.MipavUtil;

import java.io.IOException;


/**
 * Calculates an n-dimensional gaussian volume at a given scale, kernel size, and normalized derivative (through the
 * fourth order) in any of the dimensions. Explanation of normalized derivative: The derivative of n =
 * (1/sqrt(2*PI*sigma*sigma))*exp(-x*x/(2*sigma*sigma)) with respect to x = -(x/(sigma*sigma))*n. 1/2 the integral of
 * the derivative from -infinity to infinity with respect to x or the integral of the derivative from 0 to infinity with
 * respect to x = 1/(sigma * sqrt(2*PI)). Hence, the derivative is normalized by multiplying by the normalizing constant
 * sigma. GenerateGaussian returns the Gaussian, sigma times the first derivative of the Gaussian, sigma squared times
 * the second derivative of the Gaussian, sigma cubed times the third derivative of the Gaussian.
 *
 * @version  0.8 Aug 4, 1998
 * @author   Matthew J. McAuliffe, Ph.D.
 */


public class GenerateGaussian {

    //~ Instance fields ------------------------------------------------------------------------------------------------

    /** denominator of gaussian coeff. */
    private double denom;

    /** derivative order in each dimension. */
    private int[] derivOrder;

    /** length of function in pixel in each dim. */
    private int[] dimLengths;

    /** data storage buffer. */
    private float[] gaussianData;

    /** DOCUMENT ME! */
    private boolean invert = false;

    /** number of dimensions. */
    private int nDims;

    /** temp point storage. */
    private int[] pt;

    /** standard dev. in each direction */
    private float[] sigmas;

    //~ Constructors ---------------------------------------------------------------------------------------------------

    /**
     * Initializes class that calculates an n-dim Gaussian.
     *
     * @param  data        storage area of Gaussian function
     * @param  dimLengths  length of function in pixels in each dimension
     * @param  sigmas      standard dev. in each direction
     * @param  derivOrder  derivative order in each dimension
     */
    public GenerateGaussian(float[] data, int[] dimLengths, float[] sigmas, int[] derivOrder) {
        this(data, dimLengths, sigmas, derivOrder, Double.MIN_VALUE);
    }

    /**
     * Initializes class that calculates an n-dim Gaussian.
     *
     * @param  data        storage area of Gaussian function
     * @param  dimLengths  length of function in pixels in each dimension
     * @param  sigmas      standard dev. in each direction
     * @param  derivOrder  derivative order in each dimension
     * @param  denom       denominator of the gaussian scale coeffient
     */
    public GenerateGaussian(float[] data, int[] dimLengths, float[] sigmas, int[] derivOrder, double denom) {

        this.gaussianData = data;
        this.sigmas = sigmas;
        this.derivOrder = derivOrder;
        this.dimLengths = dimLengths;
        this.nDims = dimLengths.length;
        this.pt = new int[nDims];
        this.denom = denom;
    }

    //~ Methods --------------------------------------------------------------------------------------------------------

    /**
     * Starts the Gaussian calculation.
     *
     * @param  invert  Flag indicating inversion
     */
    public void calc(boolean invert) {

        this.invert = invert;

        int dims = dimLengths.length - 1;

        recursiveCalc(0, dims);
    }

    /**
     * Prepares this class for destruction.
     */
    public void finalize() {

        gaussianData = null;
        System.gc();
    }

    /**
     * Converts gaussian float buffer to image model object.
     *
     * @param   UI  pointer to user interface
     *
     * @return  image model object containing the gaussian data
     */
    public ModelImage makeImage() {

        ModelImage image;

        try {
            image = new ModelImage(ModelImage.FLOAT, dimLengths, "Gaussian");
        } catch (OutOfMemoryError error) {
            MipavUtil.displayError("Generate gaussian: unable to allocate enough memory");

            return (image = null);
        }

        try {
            image.importData(0, gaussianData, true);
        } catch (IOException error) {
            MipavUtil.displayError("Gaussian: Image(s) locked");

            return (image = null);
        }

        return (image);
    }

    /**
     * Calculates the Gaussian function at a point.
     *
     * @param   pt  array of ints representing a point where the Gaussian should be calculated.
     *
     * @return  DOCUMENT ME!
     */
    private float gaussPt(int[] pt) {
        int i;
        double gValue = 1;
        double factor = 0;
        double hFactor = 1;
        double temp = 1;

        for (i = 0; i < nDims; i++) {

            if (sigmas[i] != 0.0) { // If a sigma in any dimension is zero - return.
                factor = (-pt[i] * pt[i]) / (2.0 * sigmas[i] * sigmas[i]);

                //if (denom == Double.MIN_VALUE) {
                    denom = Math.sqrt(2.0 * Math.PI * sigmas[i] * sigmas[i]);
                //}

                temp = Math.exp(factor) / denom;
                hFactor = hermite(derivOrder[i], pt[i] / sigmas[i]);
                gValue *= (temp * hFactor);
            }
        }

        return (float) gValue;
    }

    /**
     * Factor to include when calculating derivatives.
     *
     * @param   order  derivative order
     * @param   num    input factor to correct
     *
     * @return  hermite factor
     */
    private double hermite(int order, double num) {

        switch (order) {

            case 0:
                return 1.0;

            case 1:
                return -num;

            case 2:
                return (num * num) - 1.0;

            case 3:
                return (-num * num * num) + (3.0 * num);

            case 4:
                return (num * num * num * num) - (6.0 * num * num) + 3.0;

            default:
                return 0.0;
        }
    }

    /**
     * Starts the Gaussian calculation.
     *
     * @param  offset  offset in data
     * @param  dim     present dimension
     */
    private void recursiveCalc(int offset, int dim) {
        int i, j;
        int length;
        int index;

        length = dimLengths[dim];
        index = 1;

        for (j = dim - 1; j >= 0; j--) {
            index *= dimLengths[j];
        }

        if (dim > 0) {

            for (i = 0; i < length; i++) {
                pt[dim] = (int) (i - (length / 2));
                recursiveCalc((i * index) + offset, dim - 1);
            }
        } else {

            for (i = 0; i < length; i++) {
                pt[dim] = (int) (i - (length / 2));

                if (invert) {
                    gaussianData[offset + i] = -gaussPt(pt);
                } else {
                    gaussianData[offset + i] = gaussPt(pt);
                }
            }
        }
    }

}
