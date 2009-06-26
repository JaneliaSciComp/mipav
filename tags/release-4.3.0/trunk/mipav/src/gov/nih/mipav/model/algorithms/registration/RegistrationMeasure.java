package gov.nih.mipav.model.algorithms.registration;


import gov.nih.mipav.model.structures.*;


/**
 * This is an abstract class used to compute the measure of registration between the specified target image and a
 * registered source image. This class create the registered source image to have the same dimensions as the input
 * target image. The measure calculations do not require the specification of the number of dimensions.
 */
public abstract class RegistrationMeasure {

    //~ Instance fields ------------------------------------------------------------------------------------------------

    /** Total number of samples in the target image. */
    protected int m_iNumSamples;

    /** Information and data for the registered image. Has the same number of samples as does the target image. */
    protected ModelSimpleImage m_kImageReg;

    /** Information and data for the target (reference) image. */
    protected ModelSimpleImage m_kImageTrg;

    //~ Constructors ---------------------------------------------------------------------------------------------------

    /**
     * Constructor.
     */
    protected RegistrationMeasure() {

        m_iNumSamples = 0;
        m_kImageTrg = null;
        m_kImageReg = null;
    }

    //~ Methods --------------------------------------------------------------------------------------------------------

    /**
     * Create a new instance of this measure. That is, the same type and having the same properties. But initially, the
     * target is not defined.
     *
     * @return  RegistrationMeasure Newly created instance.
     */
    public abstract RegistrationMeasure createNew();

    /**
     * Return the current error measure between the target image and the registered source image.
     *
     * @return  double Error measure value. Range of value depends on paricular implementation based on type of error
     *          measure.
     */
    public abstract double getError();

    /**
     * Return the name of the particular registration measure.
     *
     * @return  String Contains the name of the particular registration measure.
     */
    public abstract String getName();

    /**
     * Update the specified value in the registered source image. Causes a change in the computed error measure.
     *
     * @param  iIndex     int Linear array index for the value to be updated.
     * @param  fNewValue  float New registered source image value to store at the specified index.
     */
    public abstract void updateRegistration(int iIndex, float fNewValue);

    /**
     * Access the image containing the registered source values.
     *
     * @return  ModelSimpleImage Information and data for the registered source image values. The size of this array is
     *          the same properties as the target image.
     */
    public ModelSimpleImage getImageRegistered() {
        return m_kImageReg;
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

        // Save access to the target image.
        m_kImageTrg = kImageTrg;
        m_iNumSamples = m_kImageTrg.data.length;

        // Allocate registered source image.  Initially zero.
        m_kImageReg = new ModelSimpleImage(kImageTrg.extents, kImageTrg.resolutions);

        for (int i = 0; i < m_iNumSamples; i++) {
            m_kImageReg.data[i] = 0.0f;
        }
    }

    /**
     * Can be used by constructors to create an empty 3D image.
     *
     * @return  ModelSimpleImage Reference to a newly created empty image.
     */
    protected static ModelSimpleImage createEmptyImage() {

        int[] aiExtents = new int[3];
        float[] afResolutions = new float[3];

        for (int i = 0; i < 3; i++) {
            aiExtents[i] = 0;
            afResolutions[i] = 1.0f;
        }

        return new ModelSimpleImage(aiExtents, afResolutions);
    }
}
