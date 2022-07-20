package gov.nih.mipav.view.renderer.J3D.volumeview;


import gov.nih.mipav.model.structures.*;

import gov.nih.mipav.view.renderer.J3D.*;


/**
 * The base class renderer to support shear-warp rendering of intensity images (gray scale). The intensity is stored in
 * the blue channel of the base class renderer. See the document ShearWarpRendering.pdf for a detailed description of
 * the renderer architecture.
 */

public abstract class ShearWarpIntensity extends ShearWarpRenderer implements RendererInterfaceIntensity {

    //~ Instance fields ------------------------------------------------------------------------------------------------

    /** DOCUMENT ME! */
    protected byte[] m_acImageA = null;

    /** byte and alpha channels for the image. */
    protected byte[] m_acImageB = null;

    /** intermediate 2D image. */
    protected float[] m_afInterB;

    /** Factor used to scale the intermediate values during the mapping to the final image. */
    protected float m_fNormalize;

    /** Map to convert intensity values to colors. */
    protected RendererMapIntensity m_kMap = null;

    //~ Constructors ---------------------------------------------------------------------------------------------------

    /**
     * Create an intensity renderer.
     *
     * @param  kImage       the 3D image
     * @param  iFinalBound  the dimension of the square 2D renderer image
     * @param  aiFinal      The 2D rendered image. The image is 24-bit RGB color, each pixel stored as a 32-bit integer.
     *                      The 8 most significant bits are ignored. The blue channel is stored in the 8 least
     *                      significant bits. The green channel is in the next 8 bits adjacent to the blue bits. The red
     *                      channel is in the next 8 bits adjacent to the green channel.
     */
    public ShearWarpIntensity(ModelImage kImage, int iFinalBound, int[] aiFinal) {
        super(kImage, iFinalBound, aiFinal);

        m_afInterB = new float[m_iInterQuantity];
    }

    //~ Methods --------------------------------------------------------------------------------------------------------

    /**
     * Clean memory.
     */
    public void disposeLocal() {
        m_acImageB = null;
        m_acImageA = null;
        m_afInterB = null;
        m_kMap = null;
        super.disposeLocal();
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

        initializeEncodeSkip(m_acImageA);

        // By default, intermediate result values are scaled by 1.
        m_fNormalize = 1.0f;
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
     * Calls disposeLocal to clean up memory, and then call super.finalize();
     *
     * @throws  Throwable  DOCUMENT ME!
     */
    protected void finalize() throws Throwable {
        disposeLocal();
        super.finalize();
    }

    /**
     * Map the intermediate images to the final color image.
     */
    protected void mapIntermediateToFinal() {
        float halfInterBound = 0.5f * m_iInterBound;
        int oneLessInterBound = m_iInterBound - 1;

        int iFIndex = 0;
        int mod = m_iRBound / 50;

        for (int i1 = 0; i1 < m_iRBound; i1++) {

            if (((i1) % mod) == 0) {
                ViewJFrameVolumeView.getRendererProgressBar().setValue(Math.round((float) (i1) / (m_iRBound - 1) * 50) +
                                                                       50);
                ViewJFrameVolumeView.getRendererProgressBar().update(ViewJFrameVolumeView.getRendererProgressBar().getGraphics());
            }

            float fT1 = (m_iInterBound * ((m_fFMult * i1) - 0.5f)) - m_fYTrn;

            for (int i0 = 0; i0 < m_iRBound; i0++, iFIndex++) {
                float fT0 = (m_iInterBound * ((m_fFMult * i0) - 0.5f)) - m_fXTrn;
                float fJ0 = (m_fZoom * ((m_aafM[0][0] * fT0) + (m_aafM[0][1] * fT1))) + halfInterBound;

                // int j0 = (int)Math.floor(fJ0);
                int j0 = (int) (fJ0);

                if ((0 <= j0) && (j0 < oneLessInterBound)) {
                    float fJ1 = (m_fZoom * ((m_aafM[1][0] * fT0) + (m_aafM[1][1] * fT1))) + halfInterBound;

                    // int j1 = (int)Math.floor(fJ1);
                    int j1 = (int) (fJ1);

                    if ((0 <= j1) && (j1 < oneLessInterBound)) {
                        m_afB[0] = fJ0 - (float) j0;
                        m_afA[0] = 1.0f - m_afB[0];
                        m_afB[1] = fJ1 - (float) j1;
                        m_afA[1] = 1.0f - m_afB[1];

                        // look up intermediate values
                        boolean bTransparent = false;
                        int j = j0 + (m_iInterBound * j1);
                        float fI00 = m_afInterB[j];

                        bTransparent |= (0 == m_aiInterC[j]);
                        j++;

                        float fI10 = m_afInterB[j];

                        bTransparent |= (0 == m_aiInterC[j]);
                        j += m_iInterBound;

                        float fI11 = m_afInterB[j];

                        bTransparent |= (0 == m_aiInterC[j]);
                        j--;

                        float fI01 = m_afInterB[j];

                        bTransparent |= (0 == m_aiInterC[j]);

                        if (!bTransparent) {

                            // bilerp the color channels
                            float fTmp0 = m_afA[0] * ((m_afA[1] * fI00) + (m_afB[1] * fI01));
                            float fTmp1 = m_afB[0] * ((m_afA[1] * fI10) + (m_afB[1] * fI11));
                            int iValue = (int) (m_fNormalize * (fTmp0 + fTmp1));

                            m_aiRImage[iFIndex] = m_kMap.mapValue(iValue);
                        }
                    }
                }
            }
        }
    }

    /**
     * Resample all the slices for the current permuted volume data.
     *
     * @param  iDS  The number of slices to increment during the resampling phase. The value should be one or larger. If
     *              one, all slices of the volume data are resampled. If two, only every other slice is resampled. An
     *              input larger than one is used to allow fast rendering during rotation of the volume data. Once the
     *              rotation terminates, a composite with input of one should be called.
     */
    protected void resampleAll(int iDS) {
        int mod = (m_aiSliceBound[2] - 1) / 50;

        for (m_iSlice = m_aiSliceMin[2]; m_iSlice <= m_aiSliceMax[2]; m_iSlice += iDS) {

            // if ( ( m_iSlice ) % mod == 0 ) {
            ViewJFrameVolumeView.getRendererProgressBar().setValue(Math.round((float) (m_iSlice) /
                                                                                  (m_aiSliceBound[2] - 2) * 50));
            ViewJFrameVolumeView.getRendererProgressBar().update(ViewJFrameVolumeView.getRendererProgressBar().getGraphics());
            // }

            beforeResampleSingle();
            resampleSingle();
        }
    }
}
