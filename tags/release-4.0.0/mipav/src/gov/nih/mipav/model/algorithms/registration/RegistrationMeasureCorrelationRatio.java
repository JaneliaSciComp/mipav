package gov.nih.mipav.model.algorithms.registration;


import gov.nih.mipav.model.structures.*;


/**
 * Concrete implementation of the RegistrationMeasure class based on the correlation ratio computed as follows:
 *
 * <p>sum_k { variance(Y_k) * n_k / N } / variance(Y)</p>
 *
 * <p>where - X is the target image - Y is the registered source image - N is the total number of samples - k is in the
 * range [0,nbins) - variance(Y) is the variances for every sample in the image Y - variance(Y_k) is the variance of the
 * k-th iso-set defined as the set of intensities in image Y at positions where the intensity in image X is in the k-th
 * intensity bin - n_k is the number of samples in the k-th iso-set</p>
 *
 * <p>The cummulative summations used to compute the variances are maintained along with the index of the corresponding
 * bin for each sample.</p>
 */
public class RegistrationMeasureCorrelationRatio extends RegistrationMeasure {

    //~ Instance fields ------------------------------------------------------------------------------------------------

    /** Sum of the intensities in bin. */
    protected double[] m_adBinSum1;

    /** Sum of the squared intensities in bin. */
    protected double[] m_adBinSum2;

    /** Bins used to hold cummulative summations for computing the variances. */
    protected int[] m_aiBinSum0; // number of samples in bin

    /** Image corresponding to the samples in the target image which identifies to which bin the sample belongs. */
    protected int[] m_aiImageBin;

    /** DOCUMENT ME! */
    protected double m_dSum1;

    /** DOCUMENT ME! */
    protected double m_dSum2;

    /** Number of bins to use for computation. */
    protected int m_iNumBins;

    /** Cummulative summations for computing the total variance. */
    protected int m_iSum0;

    //~ Constructors ---------------------------------------------------------------------------------------------------

    /**
     * Constructor which sets up for a default number of bins.
     */
    public RegistrationMeasureCorrelationRatio() {
        this(256);
    }

    /**
     * Constructor.
     *
     * @param  iNumBins  int Number of bins to use for computation.
     */
    public RegistrationMeasureCorrelationRatio(int iNumBins) {

        m_iNumBins = iNumBins;
        m_aiBinSum0 = new int[iNumBins];
        m_adBinSum1 = new double[iNumBins];
        m_adBinSum2 = new double[iNumBins];

        setImages(createEmptyImage(), createEmptyImage());
    }

    //~ Methods --------------------------------------------------------------------------------------------------------

    /**
     * Return the name of the particular registration measure without having an instance.
     *
     * @return  String Contains the name of the particular registration measure.
     */
    public static String getStaticName() {
        return "Correlation Ratio";
    }

    /**
     * Create a new instance of this measure. That is, the same type and having the same properties. But initially, the
     * target is not defined.
     *
     * @return  RegistrationMeasure Newly created instance.
     */
    public RegistrationMeasure createNew() {
        return new RegistrationMeasureCorrelationRatio(m_iNumBins);
    }

    /**
     * Return the current error measure between the target image and the registered source image.
     *
     * @return  double Error measure value. Minimum value is zero. Maximum value depends on the range of values in the
     *          target and registered source images.
     */
    public double getError() {

        // Compute the weighted sum of the variances for each bin.
        double dWeightedVarianceSum = 0.0;

        for (int i = 0; i < m_iNumBins; i++) {
            dWeightedVarianceSum += getVariance(m_aiBinSum0[i], m_adBinSum1[i], m_adBinSum2[i]) *
                                        (double) m_aiBinSum0[i] / (double) m_iSum0;
        }

        // Compute the overall variance.
        double dVariance = getVariance(m_iSum0, m_dSum1, m_dSum2);

        if (0.0 == dVariance) {
            return 0.0;
        } else {
            return dWeightedVarianceSum / dVariance;
        }
    }

    /**
     * Return the name of the particular registration measure.
     *
     * @return  String Contains the name of the particular registration measure.
     */
    public String getName() {
        return getStaticName();
    }

    /**
     * Setup the measure to use information from the target image and the original source image. Each derived class
     * override of this method must call this method.
     *
     * @param  kImageSrc  ModelSimpleImage Contains information and data about the original source image. This array may
     *                    be empty but not null.
     * @param  kImageTrg  ModelSimpleImage Contains information and data about yhe target image. This array may be empty
     *                    but not null.
     */
    public void setImages(ModelSimpleImage kImageSrc, ModelSimpleImage kImageTrg) {

        super.setImages(kImageSrc, kImageTrg);

        // Initialize all cummulative sums to zero.
        m_iSum0 = m_iNumSamples;
        m_dSum1 = 0;
        m_dSum2 = 0;

        for (int i = 0; i < m_iNumBins; i++) {
            m_aiBinSum0[i] = 0;
            m_adBinSum1[i] = 0;
            m_adBinSum2[i] = 0;
        }

        // Determine to which bin each sample in the target belongs.
        // The bins are evenly distributed over the range of sample
        // values in the target image.
        m_aiImageBin = new int[m_iNumSamples];

        float fTrgMin = 0;
        float fTrgMax = 0;

        if (m_iNumSamples > 0) {

            // Compute the range of values in the target image.
            fTrgMin = kImageTrg.data[0];
            fTrgMax = kImageTrg.data[0];

            for (int i = 1; i < m_iNumSamples; i++) {

                if (fTrgMin > kImageTrg.data[i]) {
                    fTrgMin = kImageTrg.data[i];
                } else if (fTrgMax < kImageTrg.data[i]) {
                    fTrgMax = kImageTrg.data[i];
                }
            }
        }

        // Compute the mapping of the values in the target image
        // into the one of the bins where each bin is identified
        // by its index in the range [0,nbins).  If constant target
        // image, then everything gets mapped into the first bin.
        float fScale = (fTrgMax == fTrgMin) ? 0.0f : (m_iNumBins / (fTrgMax - fTrgMin));

        for (int i = 0; i < m_iNumSamples; i++) {
            int iBin = (int) ((kImageTrg.data[i] - fTrgMin) * fScale);

            if (iBin >= m_iNumBins) {
                iBin = m_iNumBins - 1;
            }

            m_aiImageBin[i] = iBin;
            m_aiBinSum0[iBin]++;
        }
    }

    /**
     * Update the specified value in the registered source image. Causes a change in the computed error measure.
     *
     * @param  iIndex     int Linear array index for the value to be updated.
     * @param  fNewValue  float New registered source image value to store at the specified index.
     */
    public void updateRegistration(int iIndex, float fNewValue) {

        // Store the new value but note the old value.
        float fOldValue = m_kImageReg.data[iIndex];
        m_kImageReg.data[iIndex] = fNewValue;

        // Determine the bin.
        int iBin = m_aiImageBin[iIndex];

        // Remove the old value.
        double dOldValue2 = (double) fOldValue * (double) fOldValue;
        m_adBinSum1[iBin] -= fOldValue;
        m_adBinSum2[iBin] -= dOldValue2;
        m_dSum1 -= fOldValue;
        m_dSum2 -= dOldValue2;

        // Add the new value.
        double dNewValue2 = (double) fNewValue * (double) fNewValue;
        m_adBinSum1[iBin] += fNewValue;
        m_adBinSum2[iBin] += dNewValue2;
        m_dSum1 += fNewValue;
        m_dSum2 += dNewValue2;
    }

    /**
     * Compute the variance given the total number of samples along with the sum of values and sum of squared values.
     *
     * @param   N      int Number of samples.
     * @param   dSum1  double Sum of sample values.
     * @param   dSum2  double Sum of sample squared values.
     *
     * @return  double Computed variance. Zero is returned if there are not at least two samples.
     */
    protected static final double getVariance(int N, double dSum1, double dSum2) {

        if (N > 1) {
            return ((N * dSum2) - (dSum1 * dSum1)) / ((double) N * (N - 1));
        } else {
            return 0.0;
        }
    }
}
