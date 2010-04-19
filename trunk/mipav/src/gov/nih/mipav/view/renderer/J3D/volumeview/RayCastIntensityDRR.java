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

public class RayCastIntensityDRR extends RayCastIntensity {

    //~ Instance fields ------------------------------------------------------------------------------------------------

    /** Factor used to scale the computed sum along each ray for the purpose of normalizing the integral. */
    protected float m_fNormalize;

    //~ Constructors ---------------------------------------------------------------------------------------------------

    /**
     * The constructor for the ray tracer.
     *
     * @param  kImage    the 3D image
     * @param  iRBound   the dimension of the square 2D renderer image
     * @param  aiRImage  The rendered image data stored in row-major order. Each integer pixel represents an RGB color
     *                   in the format B | (G << 8) | (R << 16).
     */
    public RayCastIntensityDRR(ModelImage kImage, int iRBound, int[] aiRImage) {
        super(kImage, iRBound, aiRImage);
    }

    //~ Methods --------------------------------------------------------------------------------------------------------

    /**
     * Specify the input volume to use for rendering. The image data stored in order of slice indices, each slice stored
     * in row-major order. That is, slice z=0 is stored first, slice z=1 is stored next, and so on. In slice z=0, the
     * y=0 row is stored first, the y=1 row is stored next, and so on.
     *
     * @param  acImageB  byte[] Array of byte intensity values for volume.
     * @param  acImageA  byte[] Array of byte alpha values for volume.
     */
    public void setInput(byte[] acImageB, byte[] acImageA) {

        super.setInput(acImageB, acImageA);

        // compute image normalization factor
        m_fNormalize = 255.0f * computeIntegralNormalizationFactor(m_acImageB);
    }

    /**
     * Return indication as to whether or not the particular renderer uses normal vectors as part of its implementation.
     *
     * @return  boolean True if the implementation uses normals.
     */
    public boolean usesNormals() {
        return false;
    }

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
            float fIntegral = 0.5f * (interpolate(p0, m_acImageB) + interpolate(p1, m_acImageB));
            boolean bHitModel = false;
            float fTStep = 1.0f / iSteps;
            for (int i = 1; i < iSteps; i++) {
                m_kP.scaleAdd(i * fTStep, lineSeg, p0);

                if (interpolate(m_kP, m_acImageA) > 0) {
                    bHitModel = true;
                    fIntegral += interpolate(m_kP, m_acImageB);
                }
            }

            fIntegral *= fTStep;

            if (bHitModel) {

                // Normalize to be in [0,1].  The normalization factor is estimated
                // from sampling, so the clamping after normalization is necessary.
                int iValue = MipavMath.round(fIntegral * m_fNormalize);

                if (iValue < 0) {
                    iValue = 0;
                } else if (iValue > 255) {
                    iValue = 255;
                }

                m_aiRImage[iIndex] = m_kMap.mapValue(iValue);
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
            float fIntegral = 0.5f * (interpolate(m_kP0, m_acImageB) + interpolate(m_kP1, m_acImageB));
            boolean bHitModel = false;
            float fTStep = 1.0f / iSteps;

            for (int i = 1; i < iSteps; i++) {
                m_kP.scaleAdd(i * fTStep, m_kPDiff, m_kP0);

                if (interpolate(m_kP, m_acImageA) > 0) {
                    bHitModel = true;
                    fIntegral += interpolate(m_kP, m_acImageB);
                }
            }

            fIntegral *= fTStep;

            if (bHitModel) {

                // Normalize to be in [0,1].  The normalization factor is estimated
                // from sampling, so the clamping after normalization is necessary.
                int iValue = MipavMath.round(fIntegral * m_fNormalize);

                if (iValue < 0) {
                    iValue = 0;
                } else if (iValue > 255) {
                    iValue = 255;
                }

                m_aiRImage[iIndex] = m_kMap.mapValue(iValue);
            }
        }
    }
}
