package gov.nih.mipav.view.renderer.J3D.volumeview;


import gov.nih.mipav.util.MipavMath;

import gov.nih.mipav.model.structures.*;


/**
 * A sample ray tracer that extends RayTrace. The only two necessary member functions are a constructor and an override
 * of 'processRay'. This sample shows how to do a very inexpensive ray trace by computing the nearest voxel locations
 * for the line segment ray-box intersection and using Bresenham's line drawing algorithm to traverse only voxel
 * locations and accumulate image values.
 */

public class RayCastIntensityDRRBresenham extends RayCastIntensityDRR {

    //~ Constructors ---------------------------------------------------------------------------------------------------

    /**
     * The constructor for the ray tracer. Currently, the only client of this class is VolumeRenderer.
     *
     * @param  kImage    the 3D image
     * @param  iRBound   the rendered image dimension (image is square)
     * @param  aiRImage  The rendered image data stored in row-major order. Each integer pixel represents an RGB color
     *                   in the format B | (G << 8) | (R << 16).
     */
    public RayCastIntensityDRRBresenham(ModelImage kImage, int iRBound, int[] aiRImage) {
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
     * members m_kP0 and m_kP1 in image coordinates. This method uses Bresenham's line drawing algorithm to numerically
     * integrate the image (at a coarse level) along the line segment.
     *
     * <p>The function sets the color of the pixel corresponding to the processed ray. The RGB value is stored as an
     * integer in the format B | (G << 8) | (R << 16). This method always returns a gray scale value (B = G = R).</p>
     *
     * @param  iIndex            index of the pixel corresponding to the processed ray
     * @param  rayTraceStepSize  DOCUMENT ME!
     */
    protected void processRay(int iIndex, int rayTraceStepSize) {

        // Round the end points to the nearest voxel locations.
        int iX0 = (int) m_kP0.x;
        int iY0 = (int) m_kP0.y;
        int iZ0 = (int) m_kP0.z;
        int iX1 = (int) m_kP1.x;
        int iY1 = (int) m_kP1.y;
        int iZ1 = (int) m_kP1.z;

        // starting point of line
        int iX = iX0, iY = iY0, iZ = iZ0;

        // direction of line
        int iDx = iX1 - iX0, iDy = iY1 - iY0, iDz = iZ1 - iZ0;

        // increment or decrement depending on direction of line
        int iSx = ((iDx > 0) ? 1 : ((iDx < 0) ? -1 : 0));
        int iSy = ((iDy > 0) ? 1 : ((iDy < 0) ? -1 : 0));
        int iSz = ((iDz > 0) ? 1 : ((iDz < 0) ? -1 : 0));

        // decision parameters for voxel selection
        iDx = Math.abs(iDx);
        iDy = Math.abs(iDy);
        iDz = Math.abs(iDz);

        int iAx = 2 * iDx, iAy = 2 * iDy, iAz = 2 * iDz;
        int iDecX, iDecY, iDecZ;

        // determine largest direction component, single-step related variable
        int iMax = iDx, iVar = 0;

        if (iDy > iMax) {
            iMax = iDy;
            iVar = 1;
        }

        if (iDz > iMax) {
            iMax = iDz;
            iVar = 2;
        }

        // traverse Bresenham line and accumulate image values
        int iIntegral = 0, iSteps = 0;

        switch (iVar) {

            case 0: // single-step in x-direction
                iDecY = iAy - iDx;
                iDecZ = iAz - iDx;
                for ( /**/; /**/; iX += iSx, iDecY += iAy, iDecZ += iAz) {
                    iIntegral += (m_acImageB[iX + (m_iXBound * (iY + (m_iYBound * iZ)))] & 0x0ff);
                    iSteps++;

                    // take Bresenham step
                    if (iX == iX1) {
                        break;
                    }

                    if (iDecY >= 0) {
                        iDecY -= iAx;
                        iY += iSy;
                    }

                    if (iDecZ >= 0) {
                        iDecZ -= iAx;
                        iZ += iSz;
                    }
                }

                break;

            case 1: // single-step in y-direction
                iDecX = iAx - iDy;
                iDecZ = iAz - iDy;
                for ( /**/; /**/; iY += iSy, iDecX += iAx, iDecZ += iAz) {
                    iIntegral += (m_acImageB[iX + (m_iXBound * (iY + (m_iYBound * iZ)))] & 0x0ff);
                    iSteps++;

                    // take Bresenham step
                    if (iY == iY1) {
                        break;
                    }

                    if (iDecX >= 0) {
                        iDecX -= iAy;
                        iX += iSx;
                    }

                    if (iDecZ >= 0) {
                        iDecZ -= iAy;
                        iZ += iSz;
                    }
                }

                break;

            case 2: // single-step in z-direction
                iDecX = iAx - iDz;
                iDecY = iAy - iDz;
                for ( /**/; /**/; iZ += iSz, iDecX += iAx, iDecY += iAy) {
                    iIntegral += (m_acImageB[iX + (m_iXBound * (iY + (m_iYBound * iZ)))] & 0x0ff);
                    iSteps++;

                    // take Bresenham step
                    if (iZ == iZ1) {
                        break;
                    }

                    if (iDecX >= 0) {
                        iDecX -= iAz;
                        iX += iSx;
                    }

                    if (iDecY >= 0) {
                        iDecY -= iAz;
                        iY += iSy;
                    }
                }

                break;
        }

        if (iSteps == 0) {
            m_aiRImage[iIndex] = 0;

            return;
        }

        float fIntegral = ((float) iIntegral) / ((float) iSteps);

        // Normalize to be in [0,1].  The normalization factor is estimated
        // from sampling, so the clamping after normalization is necessary.
        fIntegral *= m_fNormalize;

        if (fIntegral > 1.0f) {
            fIntegral = 1.0f;
        }

        m_aiRImage[iIndex] = m_kMap.mapValue(MipavMath.round(fIntegral));
    }
}
