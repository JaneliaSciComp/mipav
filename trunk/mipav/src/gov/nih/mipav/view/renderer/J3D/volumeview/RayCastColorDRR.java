package gov.nih.mipav.view.renderer.J3D.volumeview;


import gov.nih.mipav.util.MipavMath;

import gov.nih.mipav.model.structures.*;

import javax.vecmath.Point3f;
import javax.vecmath.Vector3f;


/**
 * A ray tracer for 3D images. Either a parallel or perspective camera model can be selected to form the rays. In the
 * parallel case, zooming is accomplished by changing the size of the viewport. In the perspective case, zooming is
 * accomplished by moving the eye point. The line segment of intersection (if it exists) of a ray with the bounding box
 * of the image is computed. An integration of the image values along the segment is computed and used as the gray scale
 * value for the volume rendering. The image is trilinearly interpolated to allow subvoxel evaluations.
 *
 * <p>The camera coordinate system has eye point is E = (0,0,z). The direction vector is D = (0,0,1), the up vector is U
 * = (0,1,0), and the right vector is R = (1,0,0). Only the eye point is allowed to change. Since the 3D image can be
 * arbitrarily rotated (via a virtual trackball), any portion of the image can be viewed either close up or far away.
 * </p>
 *
 * <p>The view plane has normal D and origin at E+n*D (n=near). The view frustum is orthogonal and has no far plane. For
 * a perspective camera, the field of view is given as an angle A subtended at the eye point. In camera coordinates, the
 * view port is the square [-e,e]^2 where e = n*tan(A/2). In world coordinates, the corners of the square are
 * E+n*D+s*e*U+t*e*R where |s| = |t| = 1 (four choices on sign). For a parallel camera, there is no field of view.</p>
 *
 * <p>The mapping between the viewport [-e,e]^2 and the B-by-B render image is the following. If (i,j) is a pixel in the
 * image, then the corresponding viewport point is (r,u) = (-e+2*e*i/(B-1),-e+2*e*j/(B-1)).</p>
 */

public class RayCastColorDRR extends RayCastColor {

    //~ Instance fields ------------------------------------------------------------------------------------------------

    /** DOCUMENT ME! */
    protected Float m_kNormalizeB = null;

    /** DOCUMENT ME! */
    protected Float m_kNormalizeG = null;

    /** Factor used to scale the computed sum along each ray for the purpose of normalizing the integral. */
    protected Float m_kNormalizeR = null;

    //~ Constructors ---------------------------------------------------------------------------------------------------

    /**
     * The constructor for the ray tracer.
     *
     * @param  kImage    the 3D image
     * @param  iRBound   the dimension of the square 2D renderer image
     * @param  aiRImage  The rendered image data stored in row-major order. Each integer pixel represents an RGB color
     *                   in the format B | (G << 8) | (R << 16).
     */
    public RayCastColorDRR(ModelImage kImage, int iRBound, int[] aiRImage) {
        super(kImage, iRBound, aiRImage);
    }

    //~ Methods --------------------------------------------------------------------------------------------------------

    /**
     * Clean memory.
     */
    public void disposeLocal() {
        m_kNormalizeR = null;
        m_kNormalizeG = null;
        m_kNormalizeB = null;

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

        // Reset the integral normalization factor so that it can be recomputed.
        m_kNormalizeR = null;
        m_kNormalizeG = null;
        m_kNormalizeB = null;

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
        m_kNormalizeR = null;
        m_kNormalizeG = null;
        m_kNormalizeB = null;
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
     * members P0 and P1 in image coordinates. This method uses the Trapezoid Rule to numerically integrates the
     * image along the line segment. The number of integration samples is chosen to be proportional to the length of the
     * line segment.   P0 and P1 are used for multi-thread rendering. 
     *
     * <p>The function sets the color of the pixel corresponding to the processed ray. The RGB value is stored as an
     * integer in the format B | (G << 8) | (R << 16). This method returns a yellowish value if the ray intersects a
     * region bounded by the level surface indicated by the level value class member. Otherwise a gray scale value is
     * returned.</p>
     *
     * @param  p0                ray trace starting point
     * @param  p1                ray trace stopping point
     * @param  iIndex            int the index of the pixel corresponding to the processed ray
     * @param  rayTraceStepSize  int size of steps to take along ray being traced
     */
    protected synchronized void processRay(Point3f p0, Point3f p1, int iIndex, int rayTraceStepSize) {

        // Compute the number of integration steps to use.  The number of
        // steps is proportional to the length of the line segment.
    	Point3f m_kP = new Point3f();
    	
    	Vector3f lineSeg = new Vector3f();
        lineSeg.sub(p1, p0);

        float fLength = lineSeg.length();

        // int iSteps = (int)(m_iMaxSamples*fLength*m_fMaxInvLength);
        int iSteps = (int) (1.0f / rayTraceStepSize * fLength);

        if (iSteps > 1) {

            // integrate along the line segment using the Trapezoid Rule
            float fIntegralR = 0.5f * (interpolate(p0, m_acImageR) + interpolate(p1, m_acImageR));
            float fIntegralG = 0.5f * (interpolate(p0, m_acImageG) + interpolate(p1, m_acImageG));
            float fIntegralB = 0.5f * (interpolate(p0, m_acImageB) + interpolate(p1, m_acImageB));

            boolean bHitModel = false;
            float fTStep = 1.0f / iSteps;

            for (int i = 1; i < iSteps; i++) {
                m_kP.scaleAdd(i * fTStep, lineSeg, p0);

                if (interpolate(m_kP, m_acImageA) > 0) {
                    bHitModel = true;

                    int iRGB = interpolateColor(m_kP);
                    fIntegralR += (float) ((iRGB & 0x0ff0000) >> 16);
                    fIntegralG += (float) ((iRGB & 0x0ff00) >> 8);
                    fIntegralB += (float) ((iRGB & 0x0ff));
                }
            }

            fIntegralR *= fTStep;
            fIntegralG *= fTStep;
            fIntegralB *= fTStep;

            if (bHitModel) {

                // Normalize to be in [0,1].  The normalization factor is estimated
                // from sampling, so the clamping after normalization is necessary.
                int iR = MipavMath.round(fIntegralR * m_kNormalizeR.floatValue());

                if (iR < 0) {
                    iR = 0;
                } else if (iR > 255) {
                    iR = 255;
                }

                int iG = MipavMath.round(fIntegralG * m_kNormalizeG.floatValue());

                if (iG < 0) {
                    iG = 0;
                } else if (iG > 255) {
                    iG = 255;
                }

                int iB = MipavMath.round(fIntegralB * m_kNormalizeB.floatValue());

                if (iB < 0) {
                    iB = 0;
                } else if (iB > 255) {
                    iB = 255;
                }

                m_aiRImage[iIndex] = (iR << 16) | (iG << 8) | iB;
            }
        }
    }
    
    
    /**
     * Process a ray that has intersected the oriented bounding box of the 3D image. The method is only called if there
     * is a line segment of intersection. The 'intersectsBox' stores the end points of the line segment in the class
     * members m_kP0 and m_kP1 in image coordinates. This method uses the Trapezoid Rule to numerically integrates the
     * image along the line segment. The number of integration samples is chosen to be proportional to the length of the
     * line segment.
     *
     * <p>The function sets the color of the pixel corresponding to the processed ray. The RGB value is stored as an
     * integer in the format B | (G << 8) | (R << 16). This method always returns a gray scale value (B = G = R).
     * However, the function can be overridden in a subclass to produce other rendering effects. For example, the color
     * can be set to a non-gray value if the ray intersects a level region bounded by a level surface. See
     * SurfaceRayTrace.java for an example.</p>
     *
     * @param  iIndex            index of the pixel corresponding to the processed ray
     * @param  rayTraceStepSize  DOCUMENT ME!
     */
    protected synchronized void processRay(int iIndex, int rayTraceStepSize) {

        // Compute the number of integration steps to use.  The number of
        // steps is proportional to the length of the line segment.
    	Point3f m_kP = new Point3f();
    	
    	m_kPDiff.sub(m_kP1, m_kP0);

        float fLength = m_kPDiff.length();

        // int iSteps = (int)(m_iMaxSamples*fLength*m_fMaxInvLength);
        int iSteps = (int) (1.0f / rayTraceStepSize * fLength);

        if (iSteps > 1) {

            // integrate along the line segment using the Trapezoid Rule
            float fIntegralR = 0.5f * (interpolate(m_kP0, m_acImageR) + interpolate(m_kP1, m_acImageR));
            float fIntegralG = 0.5f * (interpolate(m_kP0, m_acImageG) + interpolate(m_kP1, m_acImageG));
            float fIntegralB = 0.5f * (interpolate(m_kP0, m_acImageB) + interpolate(m_kP1, m_acImageB));

            boolean bHitModel = false;
            float fTStep = 1.0f / iSteps;

            for (int i = 1; i < iSteps; i++) {
                m_kP.scaleAdd(i * fTStep, m_kPDiff, m_kP0);

                if (interpolate(m_kP, m_acImageA) > 0) {
                    bHitModel = true;

                    int iRGB = interpolateColor(m_kP);
                    fIntegralR += (float) ((iRGB & 0x0ff0000) >> 16);
                    fIntegralG += (float) ((iRGB & 0x0ff00) >> 8);
                    fIntegralB += (float) ((iRGB & 0x0ff));
                }
            }

            fIntegralR *= fTStep;
            fIntegralG *= fTStep;
            fIntegralB *= fTStep;

            if (bHitModel) {

                // Normalize to be in [0,1].  The normalization factor is estimated
                // from sampling, so the clamping after normalization is necessary.
                int iR = MipavMath.round(fIntegralR * m_kNormalizeR.floatValue());

                if (iR < 0) {
                    iR = 0;
                } else if (iR > 255) {
                    iR = 255;
                }

                int iG = MipavMath.round(fIntegralG * m_kNormalizeG.floatValue());

                if (iG < 0) {
                    iG = 0;
                } else if (iG > 255) {
                    iG = 255;
                }

                int iB = MipavMath.round(fIntegralB * m_kNormalizeB.floatValue());

                if (iB < 0) {
                    iB = 0;
                } else if (iB > 255) {
                    iB = 255;
                }

                m_aiRImage[iIndex] = (iR << 16) | (iG << 8) | iB;
            }
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
