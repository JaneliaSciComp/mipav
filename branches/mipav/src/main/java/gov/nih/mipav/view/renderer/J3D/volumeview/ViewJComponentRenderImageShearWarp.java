package gov.nih.mipav.view.renderer.J3D.volumeview;


import gov.nih.mipav.model.structures.*;

import java.awt.event.*;


/**
 * Need to add.
 *
 * @version  0.1 Nov 18, 1997
 * @author   Matthew J. McAuliffe, Ph.D. (primary)
 */
public class ViewJComponentRenderImageShearWarp extends ViewJComponentRenderImage implements MouseMotionListener {

    //~ Instance fields ------------------------------------------------------------------------------------------------

    /**
     * opacity value used by the paint brush.
     *
     * <ul>
     *   <li>value = 1.0 - opaque</li>
     *   <li>value = 0.25 - default (mostly see through)</li>
     * </ul>
     */
    public float opacity = 0.25f;

    //~ Constructors ---------------------------------------------------------------------------------------------------

    /**
     * Constructor: ImageA and ImageB are expected to be of the same dimensionality !!
     *
     * @param  kVolumeRenderer  Access to renderer of which this is an instance.
     * @param  _imageA          Model of the image that will be displayed
     * @param  _LUTa            LUT used to display imageA
     * @param  _imageB          Model of the image that will be displayed
     * @param  _LUTb            LUT used to display imageB
     * @param  rvolBufferA      storage buffer used to build a displayable image
     * @param  rvolBufferB      DOCUMENT ME!
     * @param  extents          image dimension extents
     * @param  renderMode       rendering mode, MIP, DRR and SURFACE
     * @param  maxExtent        maximium extent value.
     */
    public ViewJComponentRenderImageShearWarp(VolumeRenderer kVolumeRenderer, ModelImage _imageA, ModelLUT _LUTa,
                                              ModelImage _imageB, ModelLUT _LUTb, int[] rvolBufferA, int[] rvolBufferB,
                                              int[] extents, int renderMode, int maxExtent) {

        super(kVolumeRenderer, _imageA, _LUTa, _imageB, _LUTb, rvolBufferA, rvolBufferB, extents, renderMode,
              maxExtent);

        rayStepSize = 7;
    }

    //~ Methods --------------------------------------------------------------------------------------------------------

    /**
     * Change the camera model.
     *
     * @param  renderMode  rendering mode.
     */
    public void setRenderMode(int renderMode) {
        renderingMode = renderMode;

        if (renderingMode == ModeMIP) {

            if (m_kImageA.isColorImage()) {
                rayTracerA = new ShearWarpColorMIP(m_kImageA, m_kImageDim.width, renBufferA);
            } else {
                rayTracerA = new ShearWarpIntensityMIP(m_kImageA, m_kImageDim.width, renBufferA);
            }

            rayTracerB = null;

            if (m_kImageB != null) {

                if (m_kImageB.isColorImage()) {
                    rayTracerB = new ShearWarpColorMIP(m_kImageB, m_kImageDim.width, renBufferB);
                } else {
                    rayTracerB = new ShearWarpIntensityMIP(m_kImageB, m_kImageDim.width, renBufferB);
                }
            }
        } else if (renderingMode == ModeXRAY) {

            if (m_kImageA.isColorImage()) {
                rayTracerA = new ShearWarpColorDRR(m_kImageA, m_kImageDim.width, renBufferA);
            } else {
                rayTracerA = new ShearWarpIntensityDRR(m_kImageA, m_kImageDim.width, renBufferA);
            }

            rayTracerB = null;

            if (m_kImageB != null) {

                if (m_kImageB.isColorImage()) {
                    rayTracerB = new ShearWarpColorDRR(m_kImageB, m_kImageDim.width, renBufferB);
                } else {
                    rayTracerB = new ShearWarpIntensityDRR(m_kImageB, m_kImageDim.width, renBufferB);
                }
            }
        } else if (renderingMode == ModeSURFACE) {
            rayTracerA = new ShearWarpColorLighting(m_kImageA, m_kImageDim.width, renBufferA);
            rayTracerB = null;
        } else if (renderingMode == ModeCOMPOSITE) {
            rayTracerA = new ShearWarpColorComposite(m_kImageA, m_kImageDim.width, renBufferA);
            rayTracerB = null;
        }

        if (null != rayTracerA) {
            rayTracerA.setBackgroundColor(m_kBackgroundColor);
            rayTracerA.setParallel(true);
            rayTracerA.setEyeDist(-1.75f * imageExtentMax);
            rayTracerA.setZoom(0.31f);
            rayTracerA.setGamma(0.45f);
            rayTracerA.rotateFrameBy(transformBU);
        }

        if (null != rayTracerB) {
            rayTracerB.setBackgroundColor(m_kBackgroundColor);
            rayTracerB.setParallel(true);
            rayTracerB.setEyeDist(-1.75f * imageExtentMax);
            rayTracerB.setZoom(0.31f);
            rayTracerB.setGamma(0.45f);
            rayTracerB.rotateFrameBy(transformBU);
        }
    }

    /**
     * Ray trace image volume by the step size and the space size.
     *
     * @param  rayStepSize   ray trace interp step size
     * @param  raySpaceSize  ray trace two point space size ( not used )
     */
    protected void trace(int rayStepSize, int raySpaceSize) {

        if (null != rayTracerA) {
            ((ShearWarpRenderer) rayTracerA).composite(rayStepSize);
        }

        if (null != rayTracerB) {
            ((ShearWarpRenderer) rayTracerB).composite(rayStepSize);
        }
    }
}
