package gov.nih.mipav.view.renderer.J3D.volumeview;


import javax.vecmath.Point3f;
import javax.vecmath.Vector3f;

import gov.nih.mipav.model.structures.*;


/**
 * Maximum intensity projection volume rendering for color-based volumes.
 */

public class RayCastColorMIP extends RayCastColor {

    //~ Instance fields ------------------------------------------------------------------------------------------------

    /** intensity channel computed for combination of RGB channels. */
    protected byte[] m_acImageI = null;

    //~ Constructors ---------------------------------------------------------------------------------------------------

    /**
     * The constructor for the ray tracer. Currently, the only client of this class is VolumeRenderer.
     *
     * @param  kImage    the 3D image
     * @param  iRBound   the dimension of the square 2D renderer image
     * @param  aiRImage  The rendered image data stored in row-major order. Each integer pixel represents an RGB color
     *                   in the format B | (G << 8) | (R << 16).
     */
    public RayCastColorMIP(ModelImage kImage, int iRBound, int[] aiRImage) {
        super(kImage, iRBound, aiRImage);
    }

    //~ Methods --------------------------------------------------------------------------------------------------------

    /**
     * Clean memory.
     */
    public void dispose() {
        m_acImageI = null;

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
     * Calls dispose.
     *
     * @throws  Throwable  DOCUMENT ME!
     */
    protected void finalize() throws Throwable {
        disposeLocal();
        super.finalize();
    }

    /**
     * Process a ray that has intersected the oriented bounding box of the 3D image. The method is only called if there
     * is a line segment of intersection. The 'intersectsBox' stores the end points of the line segment in the class
     * members P0 and P1 in image coordinates.  P0 and P1 are used in the multi-thread rendering. 
     * 
     * @param  p0                ray trace starting point
     * @param  p1                ray trace stopping point
     * @param  iIndex            index of the pixel corresponding to the processed ray
     * @param  rayTraceStepSize  DOCUMENT ME!
     */
    protected synchronized void processRay(Point3f p0, Point3f p1, int iIndex, int rayTraceStepSize) {

        // Compute the number of steps to use given the step size of the ray.
        // The number of steps is proportional to the length of the line segment.
    	Point3f m_kP = new Point3f();
    	
    	Vector3f lineSeg = new Vector3f();
        lineSeg.sub(p1, p0);

        float fLength = lineSeg.length();
        int iSteps = (int) (1.0f / rayTraceStepSize * fLength);

        // find the maximum value along the ray
        if (iSteps > 1) {
            int iMaxIntensityColor = getBackgroundColor().getRGB();
            float fMax = -1.0f;
            boolean bHitModel = false;
            float fTStep = 1.0f / iSteps;

            for (int i = 1; i < iSteps; i++) {
                m_kP.scaleAdd(i * fTStep, lineSeg, p0);

                if (interpolate(m_kP, m_acImageA) > 0) {
                    bHitModel = true;

                    float fValue = interpolate(m_kP, m_acImageI);

                    if (fValue > fMax) {
                        fMax = fValue;
                        iMaxIntensityColor = interpolateColor(m_kP);
                    }
                }
            }

            if (bHitModel) {
                m_aiRImage[iIndex] = iMaxIntensityColor;
            }
        }
    }

    
    /**
     * Process a ray that has intersected the oriented bounding box of the 3D image. The method is only called if there
     * is a line segment of intersection. The 'intersectsBox' stores the end points of the line segment in the class
     * members m_kP0 and m_kP1 in image coordinates.
     *
     * @param  iIndex            index of the pixel corresponding to the processed ray
     * @param  rayTraceStepSize  DOCUMENT ME!
     */
    protected synchronized void processRay(int iIndex, int rayTraceStepSize) {

        // Compute the number of steps to use given the step size of the ray.
        // The number of steps is proportional to the length of the line segment.
    	Point3f m_kP = new Point3f();
    	
    	m_kPDiff.sub(m_kP1, m_kP0);

        float fLength = m_kPDiff.length();
        int iSteps = (int) (1.0f / rayTraceStepSize * fLength);

        // find the maximum value along the ray
        if (iSteps > 1) {
            int iMaxIntensityColor = getBackgroundColor().getRGB();
            float fMax = -1.0f;
            boolean bHitModel = false;
            float fTStep = 1.0f / iSteps;

            for (int i = 1; i < iSteps; i++) {
                m_kP.scaleAdd(i * fTStep, m_kPDiff, m_kP0);

                if (interpolate(m_kP, m_acImageA) > 0) {
                    bHitModel = true;

                    float fValue = interpolate(m_kP, m_acImageI);

                    if (fValue > fMax) {
                        fMax = fValue;
                        iMaxIntensityColor = interpolateColor(m_kP);
                    }
                }
            }

            if (bHitModel) {
                m_aiRImage[iIndex] = iMaxIntensityColor;
            }
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
    }
}
