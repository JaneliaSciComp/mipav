package gov.nih.mipav.view.renderer.J3D.volumeview;

import WildMagic.LibFoundation.Mathematics.Vector2f;

import gov.nih.mipav.model.structures.*;

import gov.nih.mipav.view.*;


/**
 * A helper class to map image values to RBB colors.
 */

public class RendererMapIntensity {

    //~ Instance fields ------------------------------------------------------------------------------------------------

    /** DOCUMENT ME! */
    private float[] m_afLutIn;

    /** DOCUMENT ME! */
    private float[] m_afLutOut;

    /** DOCUMENT ME! */
    private float[] m_afLutSlope;

    /** DOCUMENT ME! */
    private int[] m_aiColorMap;

    //~ Constructors ---------------------------------------------------------------------------------------------------

    /**
     * Constructor.
     *
     * @param  kLUT  ModelLUT Contains the color map and transfer function.
     */
    public RendererMapIntensity(ModelLUT kLUT) {

        try {
            m_aiColorMap = new int[kLUT.getExtents()[1]];
        } catch (OutOfMemoryError error) {
            System.gc();
            MipavUtil.displayError("Out of memory: ComponentEditImage.show");
            throw error;
        }

        kLUT.makeIndexedLUT(null);
        kLUT.exportIndexedLUT(m_aiColorMap);

        // get the transfer function for the line
        // note that the y coordinate needs to be inverted
        int iNumPoints = kLUT.getTransferFunction().size();
        m_afLutIn = new float[iNumPoints];
        m_afLutOut = new float[iNumPoints];

        for (int i = 0; i < iNumPoints; i++) {
            m_afLutIn[i] = ((Vector2f) (kLUT.getTransferFunction().getPoint(i))).X;
            m_afLutOut[i] = 255.0f - ((Vector2f) (kLUT.getTransferFunction().getPoint(i))).Y;
        }

        // adjust the transfer function so that the input is rescaled to
        // the range of [0,255].
        float fLutInRange = m_afLutIn[iNumPoints - 1] - m_afLutIn[0];
        float fLutInMin = m_afLutIn[0];

        for (int i = 0; i < iNumPoints; i++) {
            m_afLutIn[i] = (m_afLutIn[i] - fLutInMin) * 255.0f / fLutInRange;
        }

        m_afLutSlope = new float[iNumPoints - 1];

        for (int i = 1; i < iNumPoints; i++) {

            if (m_afLutIn[i] == -m_afLutIn[i - 1]) {
                m_afLutSlope[i - 1] = 0.0f;
            } else {
                m_afLutSlope[i - 1] = (m_afLutOut[i] - m_afLutOut[i - 1]) / (m_afLutIn[i] - m_afLutIn[i - 1]);
            }
        }
    }

    //~ Methods --------------------------------------------------------------------------------------------------------

    /**
     * Disposes of image memory and associated objects.
     */
    public void dispose() {
        m_aiColorMap = null;
        m_afLutIn = null;
        m_afLutOut = null;
        m_afLutSlope = null;
    }

    /**
     * Return an ARGB value stored as an integer which represents the color corresponding to the input value.
     *
     * @param   fInput  float Input value to map.
     *
     * @return  int ARGB color value where bits 24-31 store the alpha, bits 16-23 store the red, bits 8-15 store the
     *          green, and bits 0-7 store the blue.
     */
    public final int mapValue(float fInput) {
        int iValue = 0;

        for (int i = 0; i < m_afLutSlope.length; i++) {

            if ((fInput >= m_afLutIn[i]) && (fInput <= m_afLutIn[i + 1])) {
                iValue = (int) (m_afLutOut[i] + (m_afLutSlope[i] * (fInput - m_afLutIn[i])) + 0.5f);

                break;
            }
        }

        return m_aiColorMap[iValue];
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
