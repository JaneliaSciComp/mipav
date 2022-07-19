package gov.nih.mipav.view.renderer.J3D.volumeview;


/**
 * A helper class to map RGB color image values to an intensity value.
 */

public class RendererMapColor {

    //~ Instance fields ------------------------------------------------------------------------------------------------

    /** Scale factors to apply to each RGB channel. Sum of scale factors must add up to one. */
    private float m_fScaleR, m_fScaleG, m_fScaleB;

    //~ Constructors ---------------------------------------------------------------------------------------------------

    /**
     * Constructor. Defaults equal weight to each of the RGB channels.
     */
    public RendererMapColor() {
        setScales(1.0f, 1.0f, 1.0f);
    }

    //~ Methods --------------------------------------------------------------------------------------------------------

    /**
     * Disposes of image memory and associated objects.
     */
    public void dispose() { }

    /**
     * Map input RGB values to an intensity value.
     *
     * @param   fValueR  float Input red channel value.
     * @param   fValueG  float Input green channel value.
     * @param   fValueB  float Input blue channel value.
     *
     * @return  float Output intensity value.
     */
    public final float mapValue(float fValueR, float fValueG, float fValueB) {
        return (fValueR * m_fScaleR) + (fValueG * m_fScaleG) + (fValueB * m_fScaleB);
    }

    /**
     * Specify the non-negative weights to apply to the RGB channels. The weights will be scaled so that they add up to
     * one.
     *
     * @param  fScaleR  float Scale factor to apply to the red channel.
     * @param  fScaleG  float Scale factor to apply to the green channel.
     * @param  fScaleB  float Scale factor to apply to the blue channel.
     */
    public void setScales(float fScaleR, float fScaleG, float fScaleB) {

        // Make sure all the weights are positive.
        m_fScaleR = Math.max(fScaleR, 0.0f);
        m_fScaleG = Math.max(fScaleG, 0.0f);
        m_fScaleB = Math.max(fScaleB, 0.0f);

        // Adjust so that the sum of weights is 1.
        float fScaleSum = m_fScaleR + m_fScaleG + m_fScaleB;
        m_fScaleR /= fScaleSum;
        m_fScaleG /= fScaleSum;
        m_fScaleB /= fScaleSum;
    }

    /**
     * Calls dispose.
     *
     * @throws  Throwable  DOCUMENT ME!
     */
    protected void finalize() throws Throwable {
        dispose();
    }
}
