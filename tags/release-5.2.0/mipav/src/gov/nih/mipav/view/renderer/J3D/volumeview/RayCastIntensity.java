package gov.nih.mipav.view.renderer.J3D.volumeview;


import gov.nih.mipav.model.structures.*;


/**
 * A ray tracer for 3D intensity images.
 */

public abstract class RayCastIntensity extends RayCastRenderer implements RendererInterfaceIntensity {

    //~ Instance fields ------------------------------------------------------------------------------------------------

    /** DOCUMENT ME! */
    protected byte[] m_acImageA = null;

    /** DOCUMENT ME! */
    protected byte[] m_acImageB = null;

    /** DOCUMENT ME! */
    protected RendererMapIntensity m_kMap = null;

    //~ Constructors ---------------------------------------------------------------------------------------------------

    /**
     * The constructor for the ray tracer.
     *
     * @param  kImage    the 3D image
     * @param  iRBound   the dimension of the square 2D renderer image
     * @param  aiRImage  The rendered image data stored in row-major order. Each integer pixel represents an RGB color
     *                   in the format B | (G << 8) | (R << 16).
     */
    protected RayCastIntensity(ModelImage kImage, int iRBound, int[] aiRImage) {
        super(kImage, iRBound, aiRImage);
    }

    //~ Methods --------------------------------------------------------------------------------------------------------

    /**
     * Clean memory.
     *
     * @param  flag  is true call the super.disposeLocal
     */
    public void disposeLocal(boolean flag) {
        System.out.println("RayCastIntensity.disposeLocal");
        m_acImageB = null;
        m_acImageA = null;
        m_kMap = null;

        if (flag == true) {
            super.disposeLocal();
        }
    }

    /**
     * Return indication as to whether or not the input image data has been specified yet.
     *
     * @return  boolean True if the input image data has been specified.
     */
    public boolean hasInputData() {
        return null != m_acImageB;
    }

    /**
     * Return indication as to whether or not a LUT to use for mapping input intensity values to colors has been
     * specified.
     *
     * @return  boolean True if such a LUT has been defined.
     */
    public boolean hasInputMap() {
        return null != m_kMap;
    }

    /**
     * Specify the input volume to use for rendering. The image data stored in order of slice indices, each slice stored
     * in row-major order. That is, slice z=0 is stored first, slice z=1 is stored next, and so on. In slice z=0, the
     * y=0 row is stored first, the y=1 row is stored next, and so on.
     *
     * @param  acImageB  byte[] Array of byte intensity values for volume.
     * @param  acImageA  byte[] Array of byte alpha values for volume.
     */
    public void setInput(byte[] acImageB, byte[] acImageA) {

        // use the input alpha values directly, but allocate array for
        // modulated intensity values upon first access
        m_acImageA = acImageA;

        if (null == m_acImageB) {
            m_acImageB = new byte[acImageB.length];
        }

        // input intensity values are scaled by alpha values
        // representing opacity
        for (int i = 0; i < acImageB.length; i++) {
            m_acImageB[i] = (byte) (((int) (acImageB[i] & 0x0ff)) * ((int) (acImageA[i] & 0x0ff)) / 255);
        }
    }

    /**
     * Specify the lookup table to use for mapping input intensity values to colors.
     *
     * @param  kMap  Look up table for mapping the intensity values to colors.
     */
    public void setInputMap(RendererMapIntensity kMap) {
        m_kMap = kMap;
    }

    /**
     * Calls dispose.
     *
     * @throws  Throwable  DOCUMENT ME!
     */
    protected void finalize() throws Throwable {
        disposeLocal(false);
        super.finalize();
    }
}
