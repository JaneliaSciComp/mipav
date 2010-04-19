package gov.nih.mipav.view.renderer.J3D.volumeview;


import gov.nih.mipav.util.MipavMath;

import gov.nih.mipav.model.structures.*;

import gov.nih.mipav.view.renderer.J3D.*;


/**
 * The base class renderer to support shear-warp rendering of color images. See the document ShearWarpRendering.pdf for
 * a detailed description of the renderer architecture.
 */

public abstract class ShearWarpColor extends ShearWarpRenderer implements RendererInterfaceColor {

    //~ Instance fields ------------------------------------------------------------------------------------------------

    /** DOCUMENT ME! */
    protected byte[] m_acImageA = null;

    /** DOCUMENT ME! */
    protected byte[] m_acImageB = null;

    /** DOCUMENT ME! */
    protected byte[] m_acImageG = null;

    /** color channels of the image (R = red, G = green, B = blue, A = alpha). */
    protected byte[] m_acImageR = null;

    /** DOCUMENT ME! */
    protected float[] m_afInterB;

    /** DOCUMENT ME! */
    protected float[] m_afInterG;

    /** intermediate 2D images. */
    protected float[] m_afInterR;

    /** DOCUMENT ME! */
    protected Float m_kNormalizeB = null;

    /** DOCUMENT ME! */
    protected Float m_kNormalizeG = null;

    /** Factor used to scale the computed sum along each ray for the purpose of normalizing the integral. */
    protected Float m_kNormalizeR = null;

    /** map to convert RGB channel values to intensity. */
    RendererMapColor m_kMap = null;

    //~ Constructors ---------------------------------------------------------------------------------------------------

    /**
     * Create a color renderer. The input color channel type is 'byte', a signed 8-bit quantity, but the data represents
     * unsigned 8-bit values.
     *
     * @param  kImage       the 3D image
     * @param  iFinalBound  the dimension of the square 2D renderer image
     * @param  aiFinal      The 2D rendered image. The image is 24-bit RGB color, each pixel stored as a 32-bit integer.
     *                      The 8 most significant bits are ignored. The blue channel is stored in the 8 least
     *                      significant bits. The green channel is in the next 8 bits adjacent to the blue bits. The red
     *                      channel is in the next 8 bits adjacent to the green channel.
     */
    public ShearWarpColor(ModelImage kImage, int iFinalBound, int[] aiFinal) {
        super(kImage, iFinalBound, aiFinal);

        // intermediate storage for color channels
        m_afInterR = new float[m_iInterQuantity];
        m_afInterG = new float[m_iInterQuantity];
        m_afInterB = new float[m_iInterQuantity];
    }

    //~ Methods --------------------------------------------------------------------------------------------------------


    /**
     * DOCUMENT ME!
     */
    public void disposeLocal() {
        m_acImageR = null;
        m_acImageG = null;
        m_acImageB = null;
        m_acImageA = null;
        m_afInterR = null;
        m_afInterG = null;
        m_afInterB = null;
        m_kMap = null;
        m_kNormalizeR = null;
        m_kNormalizeG = null;
        m_kNormalizeB = null;

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
     * Return indication as to whether or not a map has been defined for mapping input colors to intensity.
     *
     * @return  boolean True if such a map has been defined.
     */
    public boolean hasInputMap() {
        return null != m_kMap;
    }

    /**
     * Specify the input volume to use for rendering. The image data stored in order of slice indices, each slice stored
     * in row-major order. That is, slice z=0 is stored first, slice z=1 is stored next, and so on. In slice z=0, the
     * y=0 row is stored first, the y=1 row is stored next, and so on.
     *
     * @param  acImageR  byte[] Array of byte red values for volume.
     * @param  acImageG  byte[] Array of byte green values for volume.
     * @param  acImageB  byte[] Array of byte blue values for volume.
     * @param  acImageA  byte[] Array of byte alpha values for volume.
     */
    public void setInput(byte[] acImageR, byte[] acImageG, byte[] acImageB, byte[] acImageA) {

        // Don't just keep pointers to the input arrays.  Actually make a
        // copy of the arrays in case we need to change their values.
        m_acImageR = (byte[]) acImageR.clone();
        m_acImageG = (byte[]) acImageG.clone();
        m_acImageB = (byte[]) acImageB.clone();
        m_acImageA = (byte[]) acImageA.clone();

        initializeEncodeSkip(m_acImageA);

        // Reset the integral normalization factor so that it can be recomputed.
        m_kNormalizeR = null;
        m_kNormalizeG = null;
        m_kNormalizeB = null;
    }

    /**
     * Specify the lookup table to use for mapping input RGB colors to intensity.
     *
     * @param  kMap  Look up table for mapping the color values to intensity.
     */
    public void setInputMap(RendererMapColor kMap) {
        m_kMap = kMap;

        // Reset the intensity image which corresponds to the RGB channels
        // so that it can be recomputed.
        m_kNormalizeR = null;
        m_kNormalizeG = null;
        m_kNormalizeB = null;
    }

    /**
     * Calls dispose.
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
        float fNormalizeR = m_kNormalizeR.floatValue();
        float fNormalizeG = m_kNormalizeG.floatValue();
        float fNormalizeB = m_kNormalizeB.floatValue();

        int iFIndex = 0;
        int mod = m_iRBound / 50;

        for (int i1 = 0; i1 < m_iRBound; i1++) {

            if (((i1) % mod) == 0) {
                ViewJFrameVolumeView.getRendererProgressBar().setValue(MipavMath.round((float) (i1) / (m_iRBound - 1) *
                                                                                           50) + 50);
                ViewJFrameVolumeView.getRendererProgressBar().update(ViewJFrameVolumeView.getRendererProgressBar().getGraphics());
            }

            float fT1 = (m_iInterBound * ((m_fFMult * i1) - 0.5f)) - m_fYTrn;

            for (int i0 = 0; i0 < m_iRBound; i0++, iFIndex++) {
                float fT0 = (m_iInterBound * ((m_fFMult * i0) - 0.5f)) - m_fXTrn;

                float fJ0 = (m_fZoom * ((m_aafM[0][0] * fT0) + (m_aafM[0][1] * fT1))) + (0.5f * m_iInterBound);
                int j0 = (int) Math.floor(fJ0);

                if ((0 <= j0) && (j0 < (m_iInterBound - 1))) {
                    float fJ1 = (m_fZoom * ((m_aafM[1][0] * fT0) + (m_aafM[1][1] * fT1))) + (0.5f * m_iInterBound);
                    int j1 = (int) Math.floor(fJ1);

                    if ((0 <= j1) && (j1 < (m_iInterBound - 1))) {
                        m_afB[0] = fJ0 - (float) j0;
                        m_afA[0] = 1.0f - m_afB[0];
                        m_afB[1] = fJ1 - (float) j1;
                        m_afA[1] = 1.0f - m_afB[1];

                        // look up intermediate values
                        boolean bTransparent = false;
                        int j = j0 + (m_iInterBound * j1);
                        float fR00 = m_afInterR[j];
                        float fG00 = m_afInterG[j];
                        float fB00 = m_afInterB[j];

                        bTransparent |= (0 == m_aiInterC[j]);
                        j++;

                        float fR10 = m_afInterR[j];
                        float fG10 = m_afInterG[j];
                        float fB10 = m_afInterB[j];

                        bTransparent |= (0 == m_aiInterC[j]);
                        j += m_iInterBound;

                        float fR11 = m_afInterR[j];
                        float fG11 = m_afInterG[j];
                        float fB11 = m_afInterB[j];

                        bTransparent |= (0 == m_aiInterC[j]);
                        j--;

                        float fR01 = m_afInterR[j];
                        float fG01 = m_afInterG[j];
                        float fB01 = m_afInterB[j];

                        bTransparent |= (0 == m_aiInterC[j]);

                        if (!bTransparent) {

                            // bilerp the color channels
                            float fTmp0 = m_afA[0] * ((m_afA[1] * fR00) + (m_afB[1] * fR01));
                            float fTmp1 = m_afB[0] * ((m_afA[1] * fR10) + (m_afB[1] * fR11));
                            float fRValue = (fTmp0 + fTmp1) * fNormalizeR;

                            fTmp0 = m_afA[0] * ((m_afA[1] * fG00) + (m_afB[1] * fG01));
                            fTmp1 = m_afB[0] * ((m_afA[1] * fG10) + (m_afB[1] * fG11));

                            float fGValue = (fTmp0 + fTmp1) * fNormalizeG;

                            fTmp0 = m_afA[0] * ((m_afA[1] * fB00) + (m_afB[1] * fB01));
                            fTmp1 = m_afB[0] * ((m_afA[1] * fB10) + (m_afB[1] * fB11));

                            float fBValue = (fTmp0 + fTmp1) * fNormalizeB;

                            // apply gamma correction
                            // fRValue = 255.0f*(float)Math.pow(fRValue/255.0f, m_fGamma);
                            // fGValue = 255.0f*(float)Math.pow(fGValue/255.0f, m_fGamma);
                            // fBValue = 255.0f*(float)Math.pow(fBValue/255.0f, m_fGamma);

                            // convert to integer in the range 0,255
                            int iR = MipavMath.round(fRValue);

                            if (iR < 0) {
                                iR = 0;
                            } else if (iR > 255) {
                                iR = 255;
                            }

                            int iG = MipavMath.round(fGValue);

                            if (iG < 0.0f) {
                                iG = 0;
                            } else if (iG > 255) {
                                iG = 255;
                            }

                            int iB = MipavMath.round(fBValue);

                            if (iB < 0) {
                                iB = 0;
                            } else if (iB > 255) {
                                iB = 255;
                            }

                            // convert to 24-bit color
                            m_aiRImage[iFIndex] = 0xff000000 | iB | (iG << 8) | (iR << 16);
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

        for (m_iSlice = m_aiSliceMin[2]; m_iSlice <= m_aiSliceMax[2]; m_iSlice += iDS) {
            beforeResampleSingle();
            resampleSingle();
        }
    }
}
