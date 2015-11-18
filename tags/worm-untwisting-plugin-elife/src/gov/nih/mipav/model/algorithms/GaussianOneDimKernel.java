package gov.nih.mipav.model.algorithms;


/**
 * DOCUMENT ME!
 */
public class GaussianOneDimKernel {

    //~ Constructors ---------------------------------------------------------------------------------------------------

    /**
     * Creates a new GaussianOneDimKernel object.
     */
    public GaussianOneDimKernel() { } // end GaussianOneDimKernel()

    //~ Methods --------------------------------------------------------------------------------------------------------


    /**
     * DOCUMENT ME!
     *
     * @param   sigma  DOCUMENT ME!
     *
     * @return  DOCUMENT ME!
     */
    public float[] make(float sigma) {
        // function uses the definition of sigma used in MIPAV.  That is, the kernel length is 3 sigma, not the kernel
        // radius.

        double sigma2 = sigma * sigma;

        int kernelLength = (int) Math.ceil(6 * sigma);

        if ((kernelLength % 2) == 0) {
            kernelLength++;
        }

        int kernelRadius = (kernelLength - 1) / 2;
        float[] kernel = new float[kernelLength];

        float sum = 0.0f;
        double constantFactor = 1.0 / (Math.sqrt(2.0 * Math.PI) * sigma);

        for (int idx = -kernelRadius; idx <= kernelRadius; idx++) {
            kernel[idx + kernelRadius] = (float) (constantFactor * Math.exp(-((idx * idx) / (2.0 * sigma2))));
            sum += kernel[idx + kernelRadius];
        }

        // normalize the kernel so elements sum to one
        for (int idx = 0; idx < kernel.length; idx++) {
            kernel[idx] /= sum;
        }

        return kernel;
    } // end make(...)


    /**
     * DOCUMENT ME!
     *
     * @param   sigma  DOCUMENT ME!
     *
     * @return  DOCUMENT ME!
     */
    public float[] makeFirstDerivative(float sigma) {
        double sigma2 = sigma * sigma;
        int kernelRadius = (int) ((3.0 * sigma * Math.sqrt(3.0 - (1.15 * Math.pow(3, 1.0 / 3.0)))) + 1.0);
        int kernelLength = (2 * kernelRadius) + 1;
        float[] kernel = new float[kernelLength];

        double sum = 0.0, val = 0.0;
        double constantFactor = 1.0 / (Math.sqrt(2.0 * Math.PI) * sigma);

        for (int idx = -kernelRadius; idx <= kernelRadius; idx++) {
            val = constantFactor * Math.exp(-((idx * idx) / (2.0 * sigma2)));
            kernel[idx + kernelRadius] = (float) (val * (-idx / sigma2));
        }

        // normalize the kernel
        sum = 0.0f;

        for (int idx = -kernelRadius; idx <= kernelRadius; idx++) {
            sum += idx * kernel[idx + kernelRadius];
        }

        for (int idx = -kernelRadius; idx <= kernelRadius; idx++) {
            kernel[idx + kernelRadius] = kernel[idx + kernelRadius] / (float) sum;
        }

        return kernel;
    } // end makeFirstDerivative(...)


    /**
     * DOCUMENT ME!
     *
     * @param   sigma  DOCUMENT ME!
     *
     * @return  DOCUMENT ME!
     */
    public float[] makeSecondDerivative(float sigma) {
        double sigma2 = sigma * sigma;
        int kernelRadius = (int) ((3.0 * sigma * Math.sqrt(3.0 - (1.15 * Math.pow(3, 1.0 / 3.0)))) + 1.0);
        int kernelLength = (2 * kernelRadius) + 1;
        float[] kernel = new float[kernelLength];

        double sum = 0.0, val;
        double constantFactor = 1.0 / (Math.sqrt(2.0 * Math.PI) * sigma);

        for (int idx = -kernelRadius; idx <= kernelRadius; idx++) {

            // compute kernel value with doubles
            val = constantFactor * Math.exp(-((idx * idx) / (2.0 * sigma2)));
            val *= (((idx * idx) - sigma2) / (sigma2 * sigma2));
            kernel[idx + kernelRadius] = (float) val;

            // compute normalization factor
            sum += ((idx * idx) / 2.0) * val;
        }

        // normalize the kernel so elements sum to zero
        for (int idx = -kernelRadius; idx <= kernelRadius; idx++) {
            kernel[idx + kernelRadius] /= sum;

            // flip the kernel so it matches the mathematical defn of a derivative
            kernel[idx + kernelRadius] = -kernel[idx + kernelRadius];
        }

        return kernel;
    } // end makeSecondDerivative(...)


} // end class GaussianOneDimKernel
