package gov.nih.mipav.view.renderer.J3D.volumeview;


import gov.nih.mipav.model.structures.*;

import java.util.*;


/**
 * A renderer for color volume data. The composited values are based on sums of intensity along rays parallel to
 * coordinate axes. See the document ShearWarpRendering.pdf for a detailed description of the renderer architecture.
 */

public class ShearWarpColorMIP extends ShearWarpColor {

    //~ Instance fields ------------------------------------------------------------------------------------------------

    /** intensity channel computed for combination of RGB channels. */
    protected byte[] m_acImageI = null;

    /** DOCUMENT ME! */
    protected float[] m_afInterI = null;

    //~ Constructors ---------------------------------------------------------------------------------------------------

    /**
     * Create an summed intensity renderer.
     *
     * @param  kImage       the 3D image
     * @param  iFinalBound  the dimension of the square 2D renderer image
     * @param  aiFinal      The 2D rendered image. The image is 24-bit RGB color, each pixel stored as a 32-bit integer.
     *                      The 8 most significant bits are ignored. The blue channel is stored in the 8 least
     *                      significant bits. The green channel is in the next 8 bits adjacent to the blue bits. The red
     *                      channel is in the next 8 bits adjacent to the green channel.
     */
    public ShearWarpColorMIP(ModelImage kImage, int iFinalBound, int[] aiFinal) {
        super(kImage, iFinalBound, aiFinal);

        // intermediate storage for intensity
        m_afInterI = new float[m_iInterQuantity];
    }

    //~ Methods --------------------------------------------------------------------------------------------------------

    /**
     * Clean memory.
     */
    public void disposeLocal() {
        m_acImageI = null;
        m_afInterI = null;

        super.disposeLocal();
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

        super.setInput(acImageR, acImageG, acImageB, acImageA);

        // Reset the intensity image which corresponds to the RGB channels
        // so that it can be recomputed.
        m_acImageI = null;

        // Input color values are scaled by alpha values representing opacity.
        for (int i = 0; i < acImageA.length; i++) {
            m_acImageR[i] = (byte) (((int) (m_acImageR[i] & 0x0ff)) * ((int) (m_acImageA[i] & 0x0ff)) / 255);
            m_acImageG[i] = (byte) (((int) (m_acImageG[i] & 0x0ff)) * ((int) (m_acImageA[i] & 0x0ff)) / 255);
            m_acImageB[i] = (byte) (((int) (m_acImageB[i] & 0x0ff)) * ((int) (m_acImageA[i] & 0x0ff)) / 255);
        }
    }

    /**
     * Specify the lookup table to use for mapping input RGB colors to intensity.
     *
     * @param  kMap  Look up table for mapping the color values to intensity.
     */
    public void setInputMap(RendererMapColor kMap) {

        super.setInputMap(kMap);

        // Reset the intensity image which corresponds to the RGB channels
        // so that it can be recomputed.
        m_acImageI = null;
    }

    /**
     * Return indication as to whether or not the particular renderer uses normal vectors as part of its implementation.
     *
     * @return  boolean True if the implementation uses normals.
     */
    public boolean usesNormals() {
        return false;
    }

    /**
     * This is a callback to be executed before resampleAll is executed. The base class initializes various primitive
     * quantities. This class needs its intermediate array corresponding to the composited intensity to be set to zero.
     *
     * @param  k0  a permutation, one of (0,1,2), (1,2,0), (2,0,1)
     * @param  k1  DOCUMENT ME!
     * @param  k2  DOCUMENT ME!
     */
    protected void beforeResampleAll(int k0, int k1, int k2) {
        super.beforeResampleAll(k0, k1, k2);

        // clear the intermediate image with minimum possible value
        Arrays.fill(m_afInterI, Float.MIN_VALUE);
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
     * Resample a single slice of the permuted volume data.
     */
    protected void resampleSingle() {
        m_iInterOffset += m_iInterBound * m_aiSliceMin[1];

        for (int i1 = m_aiSliceMin[1]; i1 < m_aiSliceMax[1]; i1++) {
            short[] asIndex = m_aasSliceEncode[i1];

            for (m_iSIndex = 0; asIndex[m_iSIndex] >= 0; m_iSIndex++) {

                // Find the next index of a nontransparent voxel-block and a
                // non-opaque intermediate pixel.
                if (!skipToNonopaque(i1, asIndex)) {
                    break; // finished with scan line
                }

                // Skip pixels not within the clipping bounds.
                if ((m_iI0 < m_aiSliceMin[0]) || (m_iI0 > m_aiSliceMax[0])) {
                    continue;
                }

                // look up image values
                int iI00 = m_aiCurrentI[m_iPixel];
                m_iPixel++;

                int iI10 = m_aiCurrentI[m_iPixel];
                m_iPixel += m_aiSliceBound[0];

                int iI11 = m_aiCurrentI[m_iPixel];
                m_iPixel--;

                int iI01 = m_aiCurrentI[m_iPixel];

                float fI00 = (float) (m_acImageI[iI00] & 0x0ff);
                float fI01 = (float) (m_acImageI[iI01] & 0x0ff);
                float fI10 = (float) (m_acImageI[iI10] & 0x0ff);
                float fI11 = (float) (m_acImageI[iI11] & 0x0ff);

                // bilerp the intensity values
                float fTmp0 = m_afA[0] * ((m_afA[1] * fI00) + (m_afB[1] * fI01));
                float fTmp1 = m_afB[0] * ((m_afA[1] * fI10) + (m_afB[1] * fI11));
                float fICurrent = (fTmp0 + fTmp1);

                // if maximum intensity found, then track the corresponding
                // interpolated RGB values
                if (fICurrent > m_afInterI[m_iInterIndex]) {

                    m_afInterI[m_iInterIndex] = fICurrent;
                    m_aiInterC[m_iInterIndex]++;

                    // red channel
                    fI00 = (float) (m_acImageR[iI00] & 0x0ff);
                    fI01 = (float) (m_acImageR[iI01] & 0x0ff);
                    fI10 = (float) (m_acImageR[iI10] & 0x0ff);
                    fI11 = (float) (m_acImageR[iI11] & 0x0ff);
                    fTmp0 = m_afA[0] * ((m_afA[1] * fI00) + (m_afB[1] * fI01));
                    fTmp1 = m_afB[0] * ((m_afA[1] * fI10) + (m_afB[1] * fI11));
                    m_afInterR[m_iInterIndex] = (fTmp0 + fTmp1);

                    // green channel
                    fI00 = (float) (m_acImageG[iI00] & 0x0ff);
                    fI01 = (float) (m_acImageG[iI01] & 0x0ff);
                    fI10 = (float) (m_acImageG[iI10] & 0x0ff);
                    fI11 = (float) (m_acImageG[iI11] & 0x0ff);
                    fTmp0 = m_afA[0] * ((m_afA[1] * fI00) + (m_afB[1] * fI01));
                    fTmp1 = m_afB[0] * ((m_afA[1] * fI10) + (m_afB[1] * fI11));
                    m_afInterG[m_iInterIndex] = (fTmp0 + fTmp1);

                    // blue channel
                    fI00 = (float) (m_acImageB[iI00] & 0x0ff);
                    fI01 = (float) (m_acImageB[iI01] & 0x0ff);
                    fI10 = (float) (m_acImageB[iI10] & 0x0ff);
                    fI11 = (float) (m_acImageB[iI11] & 0x0ff);
                    fTmp0 = m_afA[0] * ((m_afA[1] * fI00) + (m_afB[1] * fI01));
                    fTmp1 = m_afB[0] * ((m_afA[1] * fI10) + (m_afB[1] * fI11));
                    m_afInterB[m_iInterIndex] = (fTmp0 + fTmp1);
                }
            }

            m_iInterOffset += m_iInterBound;
        }
    }

    /**
     * Called at the beginning of the trace methods.
     */
    protected void traceInit() {

        super.traceInit();

        // If the intensity volume is not defined, create it now by
        // using the map to convert RGB color values to intensity values.
        if (null == m_acImageI) {
            m_acImageI = new byte[m_acImageR.length];

            for (int i = 0; i < m_acImageI.length; i++) {
                m_acImageI[i] = (byte) (m_kMap.mapValue((float) (m_acImageR[i] & 0x0ff),
                                                        (float) (m_acImageG[i] & 0x0ff),
                                                        (float) (m_acImageB[i] & 0x0ff)));
            }
        }

        // Set the default intermediate normalization factor for each channel.
        if (null == m_kNormalizeR) {
            m_kNormalizeR = new Float(1.0f);
        }

        if (null == m_kNormalizeG) {
            m_kNormalizeG = new Float(1.0f);
        }

        if (null == m_kNormalizeB) {
            m_kNormalizeB = new Float(1.0f);
        }
    }
}
