package gov.nih.mipav.view.renderer.J3D.volumeview;


import gov.nih.mipav.model.structures.*;

import java.awt.*;
import java.awt.event.*;


/**
 * Need to add.
 *
 * @version  0.1 Nov 18, 1997
 * @author   Matthew J. McAuliffe, Ph.D. (primary)
 */
public class ViewJComponentRenderImageRayCast extends ViewJComponentRenderImage implements MouseMotionListener {

    //~ Constructors ---------------------------------------------------------------------------------------------------

    /**
     * ImageA and ImageB are expected to be of the same dimensionality !!
     *
     * @param  kVolumeRenderer  Access to renderer of which this is an instance.
     * @param  _imageA          Model of the image that will be displayed
     * @param  _LUTa            LUT used to display imageA
     * @param  _imageB          Model of the image that will be displayed
     * @param  _LUTb            LUT used to display imageB
     * @param  rvolBufferA      rendering buffer A
     * @param  rvolBufferB      rendering buffer B // Tagged for CHECKING DELETING
     * @param  extents          image dimension extents
     * @param  renderMode       rendering mode, MIP, DRR and SURFACE
     * @param  maxExtent        maximium extent value.
     */
    public ViewJComponentRenderImageRayCast(VolumeRenderer kVolumeRenderer, ModelImage _imageA, ModelLUT _LUTa,
                                            ModelImage _imageB, ModelLUT _LUTb, int[] rvolBufferA, int[] rvolBufferB,
                                            int[] extents, int renderMode, int maxExtent) {

        super(kVolumeRenderer, _imageA, _LUTa, _imageB, _LUTb, rvolBufferA, rvolBufferB, extents, renderMode,
              maxExtent);
    }

    //~ Methods --------------------------------------------------------------------------------------------------------

    /**
     * Blur the result ray traced image or not.
     *
     * @param  flag  true bluring to reduce contract, false not bluring
     */
    public void setBlurFlag(boolean flag) {

        if (null != rayTracerA) {
            ((RayCastRenderer) rayTracerA).setBlurFlag(flag);
        }

        if (null != rayTracerB) {
            ((RayCastRenderer) rayTracerB).setBlurFlag(flag);
        }
    }

    /**
     * Change the camera model.
     *
     * @param  renderMode  rendering mode.
     */
    public void setRenderMode(int renderMode) {
        renderingMode = renderMode;

        if (renderingMode == ModeMIP) {

            if (m_kImageA.isColorImage()) {
                rayTracerA = new RayCastColorMIP(m_kImageA, m_kImageDim.width, renBufferA);
            } else {
                rayTracerA = new RayCastIntensityMIP(m_kImageA, m_kImageDim.width, renBufferA);
            }

            if (m_kImageB != null) {

                if (m_kImageB.isColorImage()) {
                    rayTracerB = new RayCastColorMIP(m_kImageB, m_kImageDim.width, renBufferB);
                } else {
                    rayTracerB = new RayCastIntensityMIP(m_kImageB, m_kImageDim.width, renBufferB);
                }
            }
        } else if (renderingMode == ModeXRAY) {

            if (m_kImageA.isColorImage()) {
                rayTracerA = new RayCastColorDRR(m_kImageA, m_kImageDim.width, renBufferA);
            } else {
                rayTracerA = new RayCastIntensityDRR(m_kImageA, m_kImageDim.width, renBufferA);
            }

            if (m_kImageB != null) {

                if (m_kImageB.isColorImage()) {
                    rayTracerB = new RayCastColorDRR(m_kImageB, m_kImageDim.width, renBufferB);
                } else {
                    rayTracerB = new RayCastIntensityDRR(m_kImageB, m_kImageDim.width, renBufferB);
                }
            }
        } else if (renderingMode == ModeSURFACE) {
            rayTracerA = new RayCastColorLighting(m_kImageA, m_kImageDim.width, renBufferA);
            rayTracerB = null;
        } else if (renderingMode == ModeSURFACEFAST) {
            rayTracerA = new RayCastColorReflection(m_kImageA, m_kImageDim.width, renBufferA);
            rayTracerB = null;
        } else if (renderingMode == ModeCOMPOSITE) {
            rayTracerA = new RayCastColorComposite(m_kImageA, m_kImageDim.width, renBufferA);
            rayTracerB = null;
        }

        if (null != rayTracerA) {
            rayTracerA.setBackgroundColor(m_kBackgroundColor);
            rayTracerA.setParallel(false);
            rayTracerA.setEyeDist(-1.75f * imageExtentMax);
            rayTracerA.rotateFrameBy(transformBU);
        }

        if (null != rayTracerB) {
            rayTracerB.setBackgroundColor(m_kBackgroundColor);
            rayTracerB.setParallel(false);
            rayTracerB.setEyeDist(-1.75f * imageExtentMax);
            rayTracerB.rotateFrameBy(transformBU);
        }
    }


    /**
     * Set the vertex material diffuse color.
     *
     * @param  color  diffuse color
     */
    public void setVertexDiffuseColor(Color color) {

        if (null != rayTracerA) {
            ((RayCastRenderer) rayTracerA).setDiffuse(color);
        }

        if (null != rayTracerB) {
            ((RayCastRenderer) rayTracerB).setDiffuse(color);
        }
    }

    /**
     * Set the vertex material specular color.
     *
     * @param  color  specular color
     */
    public void setVertexSpecularColor(Color color) {

        if (null != rayTracerA) {
            ((RayCastRenderer) rayTracerA).setSpecular(color);
        }

        if (null != rayTracerB) {
            ((RayCastRenderer) rayTracerB).setSpecular(color);
        }
    }

    /**
     * Ray trace by step size.
     *
     * @param  rayStepSize   interp step size
     * @param  raySpaceSize  DOCUMENT ME!
     */
    protected void trace(int rayStepSize, int raySpaceSize) {

        if (null != rayTracerA) {
            ((RayCastRenderer) rayTracerA).trace(rayStepSize, raySpaceSize);
        }

        if (null != rayTracerB) {
            ((RayCastRenderer) rayTracerA).trace(rayStepSize, raySpaceSize);
        }
    }
}
