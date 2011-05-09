package gov.nih.mipav.view.renderer.J3D.volumeview;


import javax.vecmath.Point3f;
import javax.vecmath.Vector3f;

import gov.nih.mipav.model.structures.*;


/**
 * A sample ray tracer that extends RayTrace. The only two necessary member functions are a constructor and an override
 * of 'processRay'. This sample shows how to change the color of the trace whenever a ray intersects a region bounded by
 * a level surface of a specified yellow. In this sample, the color is changed to a yellow hue.
 */

public class RayCastIntensityMIP extends RayCastIntensity {

    //~ Constructors ---------------------------------------------------------------------------------------------------

    /**
     * The constructor for the ray tracer. Currently, the only client of this class is VolumeRenderer.
     *
     * @param  kImage    the 3D image
     * @param  iRBound   the dimension of the square 2D renderer image
     * @param  aiRImage  The rendered image data stored in row-major order. Each integer pixel represents an RGB color
     *                   in the format B | (G << 8) | (R << 16).
     */
    public RayCastIntensityMIP(ModelImage kImage, int iRBound, int[] aiRImage) {
        super(kImage, iRBound, aiRImage);
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
     * Process a ray that has intersected the oriented bounding box of the 3D image. The method is only called if there
     * is a line segment of intersection. The 'intersectsBox' stores the end points of the line segment in the class
     * members P0 and P1 in image coordinates.  P0 and P1 are used in the multi-thread rendering. 
     * 
     * @param  p0                ray trace starting point
     * @param  p1                ray trace stopping point
     * @param  iIndex            index of the pixel corresponding to the processed ray
     * @param  rayTraceStepSize  DOCUMENT ME!
     */
    protected final void processRay(Point3f p0, Point3f p1, int iIndex, int rayTraceStepSize) {

        // Compute the number of integration steps to use.  The number of
        // steps is proportional to the length of the line segment.
    	Point3f m_kP = new Point3f();
    	
    	Vector3f lineSeg = new Vector3f();
        lineSeg.sub(p1, p0);

        float fLength = lineSeg.length();

        // int iSteps = (int)(m_iMaxSamples*fLength*m_fMaxInvLength);
        int iSteps = (int) ((1.0f / rayTraceStepSize * fLength) + 0.5f);

        if (iSteps == 0) {
            m_aiRImage[iIndex] = 0;

            return;
        }
       
        // find the maximum value along the ray
        float fMax = 0.0f;
        boolean bHitModel = false;

        if (iSteps >= 1) {
            float fTStep = 1.0f / iSteps;

            for (int i = 1; i < iSteps; i++) {
                int iX = (int) m_kP.x;

                if ((iX < 0) || (iX > m_iXBoundM2)) {
                    continue;
                }

                int iY = (int) m_kP.y;

                if ((iY < 0) || (iY > m_iYBoundM2)) {
                    continue;
                }

                int iZ = (int) m_kP.z;

                if ((iZ < 0) || (iZ > m_iZBoundM2)) {
                    continue;
                }

                int i000 = iX + (m_iXBound * iY) + (m_iXYProduct * iZ);
                int i100 = i000 + 1;
                int i010 = i000 + m_iXBound;
                int i110 = i100 + m_iXBound;
                int i001 = i000 + m_iXYProduct;
                int i101 = i100 + m_iXYProduct;
                int i011 = i010 + m_iXYProduct;
                int i111 = i110 + m_iXYProduct;
                float fF000 = (m_acImageA[i000] & 0x0ff);
                float fF100 = (m_acImageA[i100] & 0x0ff);
                float fF010 = (m_acImageA[i010] & 0x0ff);
                float fF110 = (m_acImageA[i110] & 0x0ff);
                float fF001 = (m_acImageA[i001] & 0x0ff);
                float fF101 = (m_acImageA[i101] & 0x0ff);
                float fF011 = (m_acImageA[i011] & 0x0ff);
                float fF111 = (m_acImageA[i111] & 0x0ff);

                float fDX = m_kP.x - iX, fDY = m_kP.y - iY, fDZ = m_kP.z - iZ;
                float fOmDX = 1.0f - fDX, fOmDY = 1.0f - fDY, fOmDZ = 1.0f - fDZ;

                float fInterp = (fOmDZ *
                                     ((fOmDY * ((fOmDX * fF000) + (fDX * fF100))) +
                                          (fDY * ((fOmDX * fF010) + (fDX * fF110))))) +
                                (fDZ *
                                     ((fOmDY * ((fOmDX * fF001) + (fDX * fF101))) +
                                          (fDY * ((fOmDX * fF011) + (fDX * fF111)))));

                m_kP.scaleAdd(i * fTStep, lineSeg, p0);

                // if ( ((int)(fInterp + 0.5f)) > 0) {
                if (fInterp > 0.001) {
                    bHitModel = true;
                    fF000 = (m_acImageB[i000] & 0x0ff);
                    fF100 = (m_acImageB[i100] & 0x0ff);
                    fF010 = (m_acImageB[i010] & 0x0ff);
                    fF110 = (m_acImageB[i110] & 0x0ff);
                    fF001 = (m_acImageB[i001] & 0x0ff);
                    fF101 = (m_acImageB[i101] & 0x0ff);
                    fF011 = (m_acImageB[i011] & 0x0ff);
                    fF111 = (m_acImageB[i111] & 0x0ff);

                    fInterp = (fOmDZ *
                                   ((fOmDY * ((fOmDX * fF000) + (fDX * fF100))) +
                                        (fDY * ((fOmDX * fF010) + (fDX * fF110))))) +
                              (fDZ *
                                   ((fOmDY * ((fOmDX * fF001) + (fDX * fF101))) +
                                        (fDY * ((fOmDX * fF011) + (fDX * fF111)))));

                    ///float fValue = (int)(fInterp + 0.5f);
                    float fValue = (fInterp);

                    // float fValue = interpolate(m_kP, m_acImageB);
                    if (fValue > fMax) {
                        fMax = fValue;
                    }
                }
            }
        }

        if (bHitModel) {

            // m_aiRImage[iIndex] = m_kMap.mapValue((int)(fMax+0.5f));
            m_aiRImage[iIndex] = m_kMap.mapValue(fMax);
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
    protected final void processRay(int iIndex, int rayTraceStepSize) {

        // Compute the number of integration steps to use.  The number of
        // steps is proportional to the length of the line segment.
    	Point3f m_kP = new Point3f();
    	
    	m_kPDiff.sub(m_kP1, m_kP0);

        float fLength = m_kPDiff.length();

        // int iSteps = (int)(m_iMaxSamples*fLength*m_fMaxInvLength);
        int iSteps = (int) ((1.0f / rayTraceStepSize * fLength) + 0.5f);

        if (iSteps == 0) {
            m_aiRImage[iIndex] = 0;

            return;
        }

        // find the maximum value along the ray
        float fMax = 0.0f;
        boolean bHitModel = false;

        if (iSteps >= 1) {
            float fTStep = 1.0f / iSteps;

            for (int i = 1; i < iSteps; i++) {
                int iX = (int) m_kP.x;

                if ((iX < 0) || (iX > m_iXBoundM2)) {
                    continue;
                }

                int iY = (int) m_kP.y;

                if ((iY < 0) || (iY > m_iYBoundM2)) {
                    continue;
                }

                int iZ = (int) m_kP.z;

                if ((iZ < 0) || (iZ > m_iZBoundM2)) {
                    continue;
                }

                int i000 = iX + (m_iXBound * iY) + (m_iXYProduct * iZ);
                int i100 = i000 + 1;
                int i010 = i000 + m_iXBound;
                int i110 = i100 + m_iXBound;
                int i001 = i000 + m_iXYProduct;
                int i101 = i100 + m_iXYProduct;
                int i011 = i010 + m_iXYProduct;
                int i111 = i110 + m_iXYProduct;
                float fF000 = (m_acImageA[i000] & 0x0ff);
                float fF100 = (m_acImageA[i100] & 0x0ff);
                float fF010 = (m_acImageA[i010] & 0x0ff);
                float fF110 = (m_acImageA[i110] & 0x0ff);
                float fF001 = (m_acImageA[i001] & 0x0ff);
                float fF101 = (m_acImageA[i101] & 0x0ff);
                float fF011 = (m_acImageA[i011] & 0x0ff);
                float fF111 = (m_acImageA[i111] & 0x0ff);

                float fDX = m_kP.x - iX, fDY = m_kP.y - iY, fDZ = m_kP.z - iZ;
                float fOmDX = 1.0f - fDX, fOmDY = 1.0f - fDY, fOmDZ = 1.0f - fDZ;

                float fInterp = (fOmDZ *
                                     ((fOmDY * ((fOmDX * fF000) + (fDX * fF100))) +
                                          (fDY * ((fOmDX * fF010) + (fDX * fF110))))) +
                                (fDZ *
                                     ((fOmDY * ((fOmDX * fF001) + (fDX * fF101))) +
                                          (fDY * ((fOmDX * fF011) + (fDX * fF111)))));

                m_kP.scaleAdd(i * fTStep, m_kPDiff, m_kP0);

                // if ( ((int)(fInterp + 0.5f)) > 0) {
                if (fInterp > 0.001) {
                    bHitModel = true;
                    fF000 = (m_acImageB[i000] & 0x0ff);
                    fF100 = (m_acImageB[i100] & 0x0ff);
                    fF010 = (m_acImageB[i010] & 0x0ff);
                    fF110 = (m_acImageB[i110] & 0x0ff);
                    fF001 = (m_acImageB[i001] & 0x0ff);
                    fF101 = (m_acImageB[i101] & 0x0ff);
                    fF011 = (m_acImageB[i011] & 0x0ff);
                    fF111 = (m_acImageB[i111] & 0x0ff);

                    fInterp = (fOmDZ *
                                   ((fOmDY * ((fOmDX * fF000) + (fDX * fF100))) +
                                        (fDY * ((fOmDX * fF010) + (fDX * fF110))))) +
                              (fDZ *
                                   ((fOmDY * ((fOmDX * fF001) + (fDX * fF101))) +
                                        (fDY * ((fOmDX * fF011) + (fDX * fF111)))));

                    ///float fValue = (int)(fInterp + 0.5f);
                    float fValue = (fInterp);

                    // float fValue = interpolate(m_kP, m_acImageB);
                    if (fValue > fMax) {
                        fMax = fValue;
                    }
                }
            }
        }

        if (bHitModel) {

            // m_aiRImage[iIndex] = m_kMap.mapValue((int)(fMax+0.5f));
            m_aiRImage[iIndex] = m_kMap.mapValue(fMax);
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
    protected synchronized void processRaySlower(int iIndex, int rayTraceStepSize) {

        // Compute the number of integration steps to use.  The number of
        // steps is proportional to the length of the line segment.
    	Point3f m_kP = new Point3f();
    	
    	m_kPDiff.sub(m_kP1, m_kP0);

        float fLength = m_kPDiff.length();

        // int iSteps = (int)(m_iMaxSamples*fLength*m_fMaxInvLength);
        int iSteps = (int) (1.0f / rayTraceStepSize * fLength);

        if (iSteps > 1) {

            // find the maximum value along the ray
            float fMax = 0.0f;
            boolean bHitModel = false;
            float fTStep = 1.0f / iSteps;

            for (int i = 1; i < iSteps; i++) {
                m_kP.scaleAdd(i * fTStep, m_kPDiff, m_kP0);

                if (interpolate(m_kP, m_acImageA) > 0) {
                    bHitModel = true;

                    float fValue = interpolate(m_kP, m_acImageB);

                    if (fValue > fMax) {
                        fMax = fValue;
                    }
                }
            }

            if (bHitModel) {
                m_aiRImage[iIndex] = m_kMap.mapValue((int) (fMax + 0.5f));
            }
        }
    }


}
