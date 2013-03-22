package gov.nih.mipav.model.algorithms.registration;


import gov.nih.mipav.model.structures.ModelSimpleImage;


/**
 * Concrete implementation of the RegistrationMeasure class based on sum of the squared differences between the target
 * image and the registered source image. The error value is normalized by computing the root mean squared error. The
 * differences between corresponding target image and registered source image are maintained in a separate image and a
 * cummulative sum of squared differences is maintained.
 */
public class RegistrationMeasureLeastSquares extends RegistrationMeasure {

    //~ Instance fields ------------------------------------------------------------------------------------------------

    /**
     * Current error at each sample (registered-target). Has the same number of samples as the target and registered
     * images.
     */
    protected float[] m_afImageErr;

    /** Current sum of squared error measure. */
    protected double m_dSumSquaredError;

    //~ Constructors ---------------------------------------------------------------------------------------------------

    /**
     * Constructor.
     */
    public RegistrationMeasureLeastSquares() {

        m_afImageErr = null;
        m_dSumSquaredError = 0.0;

        setImages(createEmptyImage(), createEmptyImage());
    }

    //~ Methods --------------------------------------------------------------------------------------------------------

    /**
     * Return the name of the particular registration measure without having an instance.
     *
     * @return  String Contains the name of the particular registration measure.
     */
    public static String getStaticName() {
        return "Least Squares";
    }

    /**
     * Create a new instance of this measure. That is, the same type and having the same properties. But initially, the
     * target is not defined.
     *
     * @return  RegistrationMeasure Newly created instance.
     */
    public RegistrationMeasure createNew() {
        return new RegistrationMeasureLeastSquares();
    }

    /**
     * Return the current error measure between the target image and the registered source image.
     *
     * @return  double Error measure value. Minimum value is zero. Maximum value depends on the range of values in the
     *          target and registered source images.
     */
    public double getError() {
        return Math.sqrt(m_dSumSquaredError / m_iNumSamples);
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

        // Allocate image of signed differences between the target and
        // the registered source images.  Initially zero.
        m_afImageErr = new float[m_iNumSamples];

        for (int i = 0; i < m_iNumSamples; i++) {
            m_afImageErr[i] = 0.0f;
        }

        m_dSumSquaredError = 0.0;
    }

    /**
     * Update the specified value in the registered source image. Causes a change in the computed error measure.
     *
     * @param  iIndex     int Linear array index for the value to be updated.
     * @param  fNewValue  float New registered source image value to store at the specified index.
     */
    public void updateRegistration(int iIndex, float fNewValue) {
        m_kImageReg.data[iIndex] = fNewValue;

        float fOldError = m_afImageErr[iIndex];
        float fNewError = m_kImageReg.data[iIndex] - m_kImageTrg.data[iIndex];
        m_afImageErr[iIndex] = Math.abs(fNewError);
        m_dSumSquaredError += (fNewError * fNewError) - (fOldError * fOldError);
    }
}
