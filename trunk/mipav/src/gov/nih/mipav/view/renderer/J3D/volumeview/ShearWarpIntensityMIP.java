package gov.nih.mipav.view.renderer.J3D.volumeview;


import gov.nih.mipav.model.structures.*;

import java.util.*;


/**
 * A renderer for intensity volume data. The composited values are based on sums of intensity along rays parallel to
 * coordinate axes. See the document ShearWarpRendering.pdf for a detailed description of the renderer architecture.
 */

public class ShearWarpIntensityMIP extends ShearWarpIntensity {

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
    public ShearWarpIntensityMIP(ModelImage kImage, int iFinalBound, int[] aiFinal) {
        super(kImage, iFinalBound, aiFinal);
    }

    //~ Methods --------------------------------------------------------------------------------------------------------

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

        // clear the intermediate image with smallest possible value
        Arrays.fill(m_afInterB, Float.MIN_VALUE);
    }

    /**
     * Resample a single slice of the permuted volume data.
     */
    protected void resampleSingle() {
        float fICurrent = 0;
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
                int iC = m_aiCurrentI[m_iPixel];
                float fI00 = (float) (m_acImageB[iC] & 0x0ff);

                if (fI00 != 0) {

                    m_iPixel++;
                    iC = m_aiCurrentI[m_iPixel];

                    float fI10 = (float) (m_acImageB[iC] & 0x0ff);

                    m_iPixel += m_aiSliceBound[0];
                    iC = m_aiCurrentI[m_iPixel];

                    float fI11 = (float) (m_acImageB[iC] & 0x0ff);

                    m_iPixel--;
                    iC = m_aiCurrentI[m_iPixel];

                    float fI01 = (float) (m_acImageB[iC] & 0x0ff);

                    // bilerp the image values
                    float fTmp0 = m_afA[0] * ((m_afA[1] * fI00) + (m_afB[1] * fI01));
                    float fTmp1 = m_afB[0] * ((m_afA[1] * fI10) + (m_afB[1] * fI11));
                    fICurrent = (fTmp0 + fTmp1);
                } else {
                    fICurrent = 0;
                }

                if (fICurrent > m_afInterB[m_iInterIndex]) {
                    m_afInterB[m_iInterIndex] = fICurrent;
                    m_aiInterC[m_iInterIndex]++;
                }
            }

            m_iInterOffset += m_iInterBound;
        }
    }
}
