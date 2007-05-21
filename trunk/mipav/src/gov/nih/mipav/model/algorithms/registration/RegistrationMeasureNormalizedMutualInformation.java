package gov.nih.mipav.model.algorithms.registration;


import gov.nih.mipav.model.structures.*;


/**
 * Concrete implementation of the RegistrationMeasure class based on the normalized mutual information computed as
 * follows:
 *
 * <p>H(X,Y) / (H(X) + H(Y))</p>
 *
 * <p>where</p>
 *
 * <p>- X and Y are the target and registered source images, respectively, or vice versa - H(X) and H(Y) are the
 * standard marginal entropy definition where H(X) = - sum_i { p_i * log(p_i) } for probability p_i in histogram bin i -
 * H(X,Y) is the joint entropy definition where H(X,Y) = - sum_ij { p_ij * log(p_ij) } for joint probability p_ij in
 * histogram bin (i,j).</p>
 */
public class RegistrationMeasureNormalizedMutualInformation extends RegistrationMeasure {

    //~ Static fields/initializers -------------------------------------------------------------------------------------

    /** Table with precalculated logarithms. */
    protected static double[] s_adIntLog = null;

    //~ Instance fields ------------------------------------------------------------------------------------------------

    /** Bins used to hold the histograms. */
    protected int[] m_aiBinReg; // for registered source image

    /** DOCUMENT ME! */
    protected int[] m_aiBinRegTrg; // for joint registered source and target

    /**
     * Image corresponding to the samples in the registered source image which identifies to which bin the sample
     * belongs. This is a linear storage for a 2D array where iBinReg and iBinTrg map to the 1D index by computing
     * iBinReg*m_iNumBins+iBinTrg.
     */
    protected int[] m_aiImageBinReg;

    /** Image corresponding to the samples in the target image which identifies to which bin the sample belongs. */
    protected int[] m_aiImageBinTrg;

    /** Precomputed entropy for the target image. */
    protected double m_dEntropyImageTrg;

    /**
     * Given a value in the registered source image, these values are used in this equation to determine the actual
     * histogram bin: int iBinReg = (int)((value - m_fSrcMin) * m_fSrcScale);.
     */
    protected float m_fSrcMin;

    /** DOCUMENT ME! */
    protected float m_fSrcScale;

    /** Number of bins to use for computation. */
    protected int m_iNumBins;

    //~ Constructors ---------------------------------------------------------------------------------------------------

    /**
     * Constructor which sets up for a default number of bins.
     */
    public RegistrationMeasureNormalizedMutualInformation() {
        this(256);
    }

    /**
     * Constructor.
     *
     * @param  iNumBins  int Number of bins to use for computation.
     */
    public RegistrationMeasureNormalizedMutualInformation(int iNumBins) {

        m_iNumBins = iNumBins;
        m_aiBinReg = new int[iNumBins];
        m_aiBinRegTrg = new int[iNumBins * iNumBins];

        setImages(createEmptyImage(), createEmptyImage());
    }

    //~ Methods --------------------------------------------------------------------------------------------------------

    /**
     * Return the name of the particular registration measure without having an instance.
     *
     * @return  String Contains the name of the particular registration measure.
     */
    public static String getStaticName() {
        return "Normalized Mutual Information";
    }

    /**
     * Create a new instance of this measure. That is, the same type and having the same properties. But initially, the
     * target is not defined.
     *
     * @return  RegistrationMeasure Newly created instance.
     */
    public RegistrationMeasure createNew() {
        return new RegistrationMeasureNormalizedMutualInformation(m_iNumBins);
    }

    /**
     * Return the current error measure between the target image and the registered source image.
     *
     * @return  double Error measure value. Minimum value is zero. Maximum value depends on the range of values in the
     *          target and registered source images.
     */
    public double getError() {

        double Hxy = getEntropy(m_aiBinRegTrg, (double) m_iNumSamples * (double) m_iNumSamples);
        double Hx = getEntropy(m_aiBinReg, m_iNumSamples);
        double Hy = m_dEntropyImageTrg;

        return Hxy / (Hx + Hy);
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

        // The marginal entropy for the target image is fixed and computed once.
        int[] aiBinTrg = new int[m_iNumBins];

        // Initialize all histograms to zero.
        for (int i = 0; i < m_iNumBins; i++) {
            m_aiBinReg[i] = 0;
            aiBinTrg[i] = 0;

            for (int j = 0; j < m_iNumBins; j++) {
                m_aiBinRegTrg[(i * m_iNumBins) + j] = 0;
            }
        }

        // Determine the range of samples for the original source
        // image.  This is used when mapping the values in the
        // registered source image into the appropriate histogram
        // bins which span the range of values.
        float fSrcMin = 0;
        float fSrcMax = 0;

        if (kImageSrc.data.length > 0) {
            fSrcMin = kImageSrc.data[0];
            fSrcMax = kImageSrc.data[0];

            for (int i = 1; i < kImageSrc.data.length; i++) {

                if (fSrcMin > kImageSrc.data[i]) {
                    fSrcMin = kImageSrc.data[i];
                } else if (fSrcMax < kImageSrc.data[i]) {
                    fSrcMax = kImageSrc.data[i];
                }
            }
        }

        m_fSrcMin = fSrcMin;
        m_fSrcScale = (fSrcMin == fSrcMax) ? 0.0f : (m_iNumBins / (fSrcMax - fSrcMin));

        // Initially, all of the values of the registered source image
        // are in the 0th histogram bin.
        m_aiImageBinReg = new int[m_iNumSamples];

        for (int i = 0; i < m_iNumSamples; i++) {
            m_aiImageBinReg[i] = 0;
        }

        m_aiBinReg[0] = m_iNumSamples;


        // Compute the range of values in the target image.
        // Determine to which bin each sample in the target belongs.
        // The bins are evenly distributed over the range of sample
        // values in the target image.
        m_aiImageBinTrg = new int[m_iNumSamples];

        float fTrgMin = 0;
        float fTrgMax = 0;

        if (m_iNumSamples > 0) {
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
        int iBinReg = 0;

        for (int i = 0; i < m_iNumSamples; i++) {
            int iBinTrg = (int) ((kImageTrg.data[i] - fTrgMin) * fScale);

            if (iBinTrg >= m_iNumBins) {
                iBinTrg = m_iNumBins - 1;
            }

            m_aiImageBinTrg[i] = iBinTrg;
            aiBinTrg[iBinTrg]++;
            m_aiBinRegTrg[(iBinReg * m_iNumBins) + iBinTrg]++;
        }

        // Precompute the entropy associated with the target image.
        m_dEntropyImageTrg = getEntropy(aiBinTrg, (double) m_iNumSamples);

        // Cleanup.
        aiBinTrg = null;
    }

    /**
     * Update the specified value in the registered source image. Causes a change in the computed error measure.
     *
     * @param  iIndex     int Linear array index for the value to be updated.
     * @param  fNewValue  float New registered source image value to store at the specified index.
     */
    public void updateRegistration(int iIndex, float fNewValue) {

        // Store the new value but note the old value.
        //float fOldValue = m_kImageReg.data[iIndex];
        m_kImageReg.data[iIndex] = fNewValue;

        // Determine the bin associated with the target.
        int iBinTrg = m_aiImageBinTrg[iIndex];

        // Determine the bin associated with the registered source.
        int iBinRegNew = (int) ((fNewValue - m_fSrcMin) * m_fSrcScale);

        if (iBinRegNew >= m_iNumBins) {
            iBinRegNew = m_iNumBins - 1;
        }

        int iBinRegOld = m_aiImageBinReg[iIndex];
        m_aiImageBinReg[iIndex] = iBinRegNew;

        // Update the histogram for the marginal entropy related to
        // the registered source image.
        m_aiBinReg[iBinRegOld]--;
        m_aiBinReg[iBinRegNew]++;

        // Update the histogram for the joint entropy.
        m_aiBinRegTrg[(iBinRegOld * m_iNumBins) + iBinTrg]--;
        m_aiBinRegTrg[(iBinRegNew * m_iNumBins) + iBinTrg]++;
    }

    /**
     * Standard entropy calculation.
     *
     * @param   aiHistBin    int[] Array of histogram bins. The sum of the the values in each of the array positions
     *                       should equal the values passed in iNumSamples.
     * @param   dNumSamples  double Total number of samples in the distribution. Must be a double-precision floating
     *                       point number because the number can be quite large in the case of the joint entropy.
     *
     * @return  double Computed entropy value.
     */
    protected static double getEntropy(int[] aiHistBin, double dNumSamples) {

        // The standard formula is to computed:
        //
        // sum_i { log (bin_i / N) * bin_i / N}
        //
        // which can be simplified to:
        // sum_i { bin_i * ( log(bin_i) - log(N) ) } / N
        //
        // since N is a contant, the log(N) can be precomputed.
        // Also, the log(bin_i) is the logarithm of integral values
        // which we already have stored in a table.
        double[] afIntLog = getTableIntLog();
        double dLogN = Math.log(dNumSamples);
        double dSum = 0.0;

        for (int iBin = 0; iBin < aiHistBin.length; iBin++) {

            if (aiHistBin[iBin] > 0) {
                int iHistBin = aiHistBin[iBin];

                if (iHistBin < afIntLog.length) {
                    dSum += (double) iHistBin * (afIntLog[iHistBin] - dLogN);
                } else {
                    dSum += (double) iHistBin * (Math.log((double) iHistBin) - dLogN);
                }
            }
        }

        return -dSum / dNumSamples;
    }

    /**
     * DOCUMENT ME!
     *
     * @return  DOCUMENT ME!
     */
    protected static double[] getTableIntLog() {

        // Define it not already defined.
        if (null == s_adIntLog) {
            s_adIntLog = new double[1 << 14];

            for (int i = 0; i < s_adIntLog.length; i++) {
                s_adIntLog[i] = Math.log((double) i);
            }
        }

        return s_adIntLog;
    }
}
