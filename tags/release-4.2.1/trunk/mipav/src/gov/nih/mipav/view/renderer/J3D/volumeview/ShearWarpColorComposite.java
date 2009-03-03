package gov.nih.mipav.view.renderer.J3D.volumeview;


import gov.nih.mipav.model.structures.*;

import gov.nih.mipav.view.renderer.J3D.*;
import java.util.*;

import javax.vecmath.*;


/**
 * A renderer for composition of voxel RGB color values with alpha blending and no lighting.
 */
public class ShearWarpColorComposite extends ShearWarpColor {

    //~ Instance fields ------------------------------------------------------------------------------------------------

    /** The actual order of compositing - true if front-to-back. */
    protected boolean m_bCompositeForward;

    /** DOCUMENT ME! */
    protected Color3f m_kColor = new Color3f();

    /**
     * To avoid memory reallocations. These are used in the bilinearly interpolation of a 2x2 block of voxels in the
     * current slice.
     */
    protected Vector3f m_kNormal = new Vector3f();

    //~ Constructors ---------------------------------------------------------------------------------------------------

    /**
     * Create an RGBA+lighting color renderer. The input color channel type is 'byte', a signed 8-bit quantity, but the
     * data represents unsigned 8-bit values.
     *
     * @param  kImage       the 3D image
     * @param  iFinalBound  the dimension of the square 2D renderer image
     * @param  aiFinal      The 2D rendered image. The image is 24-bit RGB color, each pixel stored as a 32-bit integer.
     *                      The 8 most significant bits are ignored. The blue channel is stored in the 8 least
     *                      significant bits. The green channel is in the next 8 bits adjacent to the blue bits. The red
     *                      channel is in the next 8 bits adjacent to the green channel.
     */
    public ShearWarpColorComposite(ModelImage kImage, int iFinalBound, int[] aiFinal) {
        super(kImage, iFinalBound, aiFinal);

        m_bCompositeForward = false;
    }

    //~ Methods --------------------------------------------------------------------------------------------------------

    /**
     * Deallocate memory.
     */
    public void disposeLocal() {
        m_kNormal = null;
        m_kColor = null;
        super.disposeLocal();
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
     * quantities. This class sets the current eye position to always be in front of the box along the current renderer
     * view direction. This function also saves the current permutation for use in computing voxel positions for dynamic
     * lighting.
     *
     * @param  k0  a permutation, one of (0,1,2), (1,2,0), (2,0,1)
     * @param  k1  DOCUMENT ME!
     * @param  k2  DOCUMENT ME!
     */
    protected void beforeResampleAll(int k0, int k1, int k2) {
        super.beforeResampleAll(k0, k1, k2);

        // direction of compositing
        m_bCompositeForward = (m_aafBox[2][k2] < 0.0f);

        // clear the intermediate colors
        Arrays.fill(m_afInterR, m_kBackgroundColor.getRed());
        Arrays.fill(m_afInterG, m_kBackgroundColor.getGreen());
        Arrays.fill(m_afInterB, m_kBackgroundColor.getBlue());
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
     * Resample all the slices for the current permuted volume data.
     *
     * @param  iDS  The number of slices to increment during the resampling phase. The value should be one or larger. If
     *              one, all slices of the volume data are resampled. If two, only every other slice is resampled. An
     *              input larger than one is used to allow fast rendering during rotation of the volume data. Once the
     *              rotation terminates, a composite with input of one should be called.
     */
    protected void resampleAll(int iDS) {
        int mod = (m_aiSliceBound[2] - 1) / 50;

        if (m_bCompositeForward) { // front to back
            m_iSlice = m_aiSliceMin[2];

            for ( /**/; m_iSlice <= m_aiSliceMax[2]; m_iSlice += iDS) {

                if (((m_iSlice) % mod) == 0) {
                    ViewJFrameVolumeView.getRendererProgressBar().setValue(Math.round((float) (m_iSlice) /
                                                                                          (m_aiSliceBound[2] - 2) *
                                                                                          50));
                    ViewJFrameVolumeView.getRendererProgressBar().update(ViewJFrameVolumeView.getRendererProgressBar().getGraphics());
                }

                beforeResampleSingle();
                resampleSingle();
            }
        } else { // back to front
            m_iSlice = m_aiSliceMax[2];

            for ( /**/; m_iSlice >= m_aiSliceMin[2]; m_iSlice -= iDS) {

                if (((m_aiSliceBound[2] - 1 - m_iSlice) % mod) == 0) {
                    ViewJFrameVolumeView.getRendererProgressBar().setValue(Math.round((float) (m_aiSliceBound[2] - 1 -
                                                                                               m_iSlice) /
                                                                                          (m_aiSliceBound[2] - 2) *
                                                                                          50));
                    ViewJFrameVolumeView.getRendererProgressBar().update(ViewJFrameVolumeView.getRendererProgressBar().getGraphics());
                }

                beforeResampleSingle();
                resampleSingle();
            }
        }
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

                // compute the vertex positions for the voxels
                computePositions(m_iI0, i1, m_iSlice);

                // indexes for the four neighbors
                int iI00 = m_aiCurrentI[m_iPixel];
                m_iPixel++;

                int iI10 = m_aiCurrentI[m_iPixel];
                m_iPixel += m_aiSliceBound[0];

                int iI11 = m_aiCurrentI[m_iPixel];
                m_iPixel--;

                int iI01 = m_aiCurrentI[m_iPixel];

                // scale factors for the four neighbors
                float fS00 = m_afA[0] * m_afA[1];
                float fS01 = m_afA[0] * m_afB[1];
                float fS10 = m_afB[0] * m_afA[1];
                float fS11 = m_afB[0] * m_afB[1];

                // bilerp position
                m_kPosition.x = (fS00 * m_kP00.x) + (fS01 * m_kP01.x) + (fS10 * m_kP10.x) + (fS11 * m_kP11.x);
                m_kPosition.y = (fS00 * m_kP00.y) + (fS01 * m_kP01.y) + (fS10 * m_kP10.y) + (fS11 * m_kP11.y);
                m_kPosition.z = (fS00 * m_kP00.z) + (fS01 * m_kP01.z) + (fS10 * m_kP10.z) + (fS11 * m_kP11.z);

                // bilerp RGBA channels
                float fR00 = (float) (m_acImageR[iI00] & 0x0ff);
                float fR01 = (float) (m_acImageR[iI01] & 0x0ff);
                float fR10 = (float) (m_acImageR[iI10] & 0x0ff);
                float fR11 = (float) (m_acImageR[iI11] & 0x0ff);
                float fG00 = (float) (m_acImageG[iI00] & 0x0ff);
                float fG01 = (float) (m_acImageG[iI01] & 0x0ff);
                float fG10 = (float) (m_acImageG[iI10] & 0x0ff);
                float fG11 = (float) (m_acImageG[iI11] & 0x0ff);
                float fB00 = (float) (m_acImageB[iI00] & 0x0ff);
                float fB01 = (float) (m_acImageB[iI01] & 0x0ff);
                float fB10 = (float) (m_acImageB[iI10] & 0x0ff);
                float fB11 = (float) (m_acImageB[iI11] & 0x0ff);
                float fA00 = (float) (m_acImageA[iI00] & 0x0ff);
                float fA01 = (float) (m_acImageA[iI01] & 0x0ff);
                float fA10 = (float) (m_acImageA[iI10] & 0x0ff);
                float fA11 = (float) (m_acImageA[iI11] & 0x0ff);
                m_kColor.x = (fS00 * fR00) + (fS01 * fR01) + (fS10 * fR10) + (fS11 * fR11);
                m_kColor.y = (fS00 * fG00) + (fS01 * fG01) + (fS10 * fG10) + (fS11 * fG11);
                m_kColor.z = (fS00 * fB00) + (fS01 * fB01) + (fS10 * fB10) + (fS11 * fB11);

                float fSrcA = ((fS00 * fA00) + (fS01 * fA01) + (fS10 * fA10) + (fS11 * fA11)) / 255.0f;

                // only proceed if not fully transparent and if there is a normal
                if (fSrcA > 0.0f) {

                    float fSrcR = m_kColor.x;
                    float fSrcG = m_kColor.y;
                    float fSrcB = m_kColor.z;

                    // voxel is opaque
                    if (fSrcA == 1.0f) {
                        m_afInterR[m_iInterIndex] = fSrcR;
                        m_afInterG[m_iInterIndex] = fSrcG;
                        m_afInterB[m_iInterIndex] = fSrcB;
                    } // voxel is semitransparent so need to blend
                    else {
                        float fTrgA = 1.0f - fSrcA;

                        m_afInterR[m_iInterIndex] = (fSrcA * fSrcR) + (fTrgA * m_afInterR[m_iInterIndex]);
                        m_afInterG[m_iInterIndex] = (fSrcA * fSrcG) + (fTrgA * m_afInterG[m_iInterIndex]);
                        m_afInterB[m_iInterIndex] = (fSrcA * fSrcB) + (fTrgA * m_afInterB[m_iInterIndex]);
                    }

                    m_aiInterC[m_iInterIndex]++;
                }
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
