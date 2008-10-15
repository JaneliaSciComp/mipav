package gov.nih.mipav.view.renderer.J3D.volumeview;


import gov.nih.mipav.model.structures.*;

import java.util.*;


/**
 * A renderer for color volume data. The composited values are based on sums of intensity along rays parallel to
 * coordinate axes. See the document ShearWarpRendering.pdf for a detailed description of the renderer architecture.
 */

public class ShearWarpColorDRR extends ShearWarpColor {

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
    public ShearWarpColorDRR(ModelImage kImage, int iFinalBound, int[] aiFinal) {
        super(kImage, iFinalBound, aiFinal);
    }

    //~ Methods --------------------------------------------------------------------------------------------------------

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

        // clear the intermediate images for integral sum
        Arrays.fill(m_afInterR, 0.0f);
        Arrays.fill(m_afInterG, 0.0f);
        Arrays.fill(m_afInterB, 0.0f);
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

                    // finished with scan line
                    break;
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

                // red channel
                float fI00 = (float) (m_acImageR[iI00] & 0x0ff);
                float fI10 = (float) (m_acImageR[iI10] & 0x0ff);
                float fI11 = (float) (m_acImageR[iI11] & 0x0ff);
                float fI01 = (float) (m_acImageR[iI01] & 0x0ff);
                float fTmp0 = m_afA[0] * ((m_afA[1] * fI00) + (m_afB[1] * fI01));
                float fTmp1 = m_afB[0] * ((m_afA[1] * fI10) + (m_afB[1] * fI11));
                float fValueR = fTmp0 + fTmp1;

                // green channel
                fI00 = (float) (m_acImageG[iI00] & 0x0ff);
                fI10 = (float) (m_acImageG[iI10] & 0x0ff);
                fI11 = (float) (m_acImageG[iI11] & 0x0ff);
                fI01 = (float) (m_acImageG[iI01] & 0x0ff);
                fTmp0 = m_afA[0] * ((m_afA[1] * fI00) + (m_afB[1] * fI01));
                fTmp1 = m_afB[0] * ((m_afA[1] * fI10) + (m_afB[1] * fI11));

                float fValueG = fTmp0 + fTmp1;

                // blue channel
                fI00 = (float) (m_acImageB[iI00] & 0x0ff);
                fI10 = (float) (m_acImageB[iI10] & 0x0ff);
                fI11 = (float) (m_acImageB[iI11] & 0x0ff);
                fI01 = (float) (m_acImageB[iI01] & 0x0ff);
                fTmp0 = m_afA[0] * ((m_afA[1] * fI00) + (m_afB[1] * fI01));
                fTmp1 = m_afB[0] * ((m_afA[1] * fI10) + (m_afB[1] * fI11));

                float fValueB = fTmp0 + fTmp1;

                // m_afInter{RGB} is the running average
                // m_aiInterC is the number of points used to compute the average
                // so that the previous total can be computed before computing
                // the new average
                float fSumR = fValueR + (m_afInterR[m_iInterIndex] * m_aiInterC[m_iInterIndex]);
                float fSumG = fValueG + (m_afInterG[m_iInterIndex] * m_aiInterC[m_iInterIndex]);
                float fSumB = fValueB + (m_afInterB[m_iInterIndex] * m_aiInterC[m_iInterIndex]);
                m_aiInterC[m_iInterIndex]++;
                m_afInterR[m_iInterIndex] = fSumR / m_aiInterC[m_iInterIndex];
                m_afInterG[m_iInterIndex] = fSumG / m_aiInterC[m_iInterIndex];
                m_afInterB[m_iInterIndex] = fSumB / m_aiInterC[m_iInterIndex];
            }

            asIndex = null;
            m_iInterOffset += m_iInterBound;
        }
    }

    /**
     * Called at the beginning of the trace methods.
     */
    protected void traceInit() {

        super.traceInit();

        /**
         * In order to map line integrals of image intensity to RGB colors where each color channel is 8 bits, it is
         * necessary to make sure that the integrals are in [0,255].  Producing a theoretical maximum value of a line
         * integral is not tractable in an application.  This method constructs an approximate maximum by integrating
         * along each line of voxels in the image with line directions parallel to the coordinate axes.  The
         * 'processRay' call adjusts the line integrals using the estimate, but still clamps the integrals to 255 since
         * the estimate might not be the true maximum.
         */
        // If the intensity volume is not defined, create it now by
        // using the map to convert RGB color values to intensity values.
        if (null == m_kNormalizeR) {
            m_kNormalizeR = new Float(255.0f * computeIntegralNormalizationFactor(m_acImageR));
        }

        if (null == m_kNormalizeG) {
            m_kNormalizeG = new Float(255.0f * computeIntegralNormalizationFactor(m_acImageG));
        }

        if (null == m_kNormalizeB) {
            m_kNormalizeB = new Float(255.0f * computeIntegralNormalizationFactor(m_acImageB));
        }
    }
}
