package gov.nih.mipav.view.renderer.flythroughview;

import gov.nih.mipav.model.structures.*;

import gov.nih.mipav.view.*;


/**
 * A helper class to map image values to RGB colors.
 */

public class ModelLUTHelper {

    //~ Instance fields ------------------------------------------------------------------------------------------------

    /** DOCUMENT ME! */
    private float[] m_afLutIn;

    /** DOCUMENT ME! */
    private float[] m_afLutOut;

    /** DOCUMENT ME! */
    private float[] m_afLutSlope;

    /** DOCUMENT ME! */
    private int[] m_aiColorMap;

    /** DOCUMENT ME! */
    private ModelLUT m_kModelLUT;

    //~ Constructors ---------------------------------------------------------------------------------------------------

    /**
     * Constructor.
     *
     * @param  kLUT  ModelLUT Contains the color map and transfer function.
     */
    public ModelLUTHelper(ModelLUT kLUT) {

        // Keep track of the input ModelLUT reference.
        m_kModelLUT = kLUT;

        // Allocate the color map table.
        try {
            m_aiColorMap = new int[kLUT.getExtents()[1]];
        } catch (OutOfMemoryError error) {
            System.gc();
            MipavUtil.displayError("Out of memory: ModelLUTHelper constructor");
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
            m_afLutIn[i] = kLUT.getTransferFunction().getPoint(i).X;
            m_afLutOut[i] = 255.0f - kLUT.getTransferFunction().getPoint(i).Y;
        }

        // Create the mapping for the input transfer function to
        // color map entries.
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
        m_kModelLUT = null;
        m_aiColorMap = null;
        m_afLutIn = null;
        m_afLutOut = null;
        m_afLutSlope = null;
    }

    /**
     * Get access to the associated ModelLUT instance.
     *
     * @return  ModelLUT Reference to ModelLUT passed in the constructor.
     */
    public ModelLUT getModelLUT() {
        return m_kModelLUT;
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

        if (fInput < m_afLutIn[0]) {
            return m_aiColorMap[0];
        }

        for (int i = 0; i < m_afLutSlope.length; i++) {

            if ((fInput >= m_afLutIn[i]) && (fInput <= m_afLutIn[i + 1])) {
                int iValue = (int) (m_afLutOut[i] + (m_afLutSlope[i] * (fInput - m_afLutIn[i])) + 0.5f);

                return m_aiColorMap[iValue];
            }
        }

        return m_aiColorMap[m_aiColorMap.length - 1];
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
