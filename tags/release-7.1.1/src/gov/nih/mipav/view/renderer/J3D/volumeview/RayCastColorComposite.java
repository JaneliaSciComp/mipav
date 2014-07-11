package gov.nih.mipav.view.renderer.J3D.volumeview;


import gov.nih.mipav.model.structures.*;

import javax.vecmath.*;


/**
 * Ray traced rendering of the volume using composition of colors and alpha values at each voxel.
 */

public class RayCastColorComposite extends RayCastColor {

    //~ Instance fields ------------------------------------------------------------------------------------------------


    //~ Constructors ---------------------------------------------------------------------------------------------------

    /**
     * The constructor for the ray tracer. Currently, the only client of this class is MjVolumeRenderer.
     *
     * @param  kImage    the 3D image
     * @param  iRBound   the dimension of the square 2D renderer image
     * @param  aiRImage  The rendered image data stored in row-major order. Each integer pixel represents an RGB color
     *                   in the format B | (G << 8) | (R << 16).
     */
    public RayCastColorComposite(ModelImage kImage, int iRBound, int[] aiRImage) {
        super(kImage, iRBound, aiRImage);
    }

    //~ Methods --------------------------------------------------------------------------------------------------------

    /**
     * Clean memory.
     *
     * @param  flag  is true call the super.disposeLocal
     */

    public void disposeLocal(boolean flag) {
        System.out.println(" RayCastColorLighting.disposeLocal");

        if (flag == true) {
            super.disposeLocal();
        }
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
        disposeLocal(false);
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
    protected void processRay(Point3f p0, Point3f p1, int iIndex, int rayTraceStepSize) {
         
        // Compute the number of steps to use.  The number of
        // steps is proportional to the length of the line segment.
        Vector3f lineSeg = new Vector3f();
        lineSeg.sub(p1, p0);

        float fLength = lineSeg.length();
        int iNumSteps = 1 + (int) ((fLength / rayTraceStepSize) * 1f);

        /** alpha array. */
        float[] afAlphasFrontToBack = new float[iNumSteps];

        /** Allocate arrays to store interpolated RGBA values along the ray in order from front-to-back. */
        Color3f[] akColorsFrontToBack = new Color3f[iNumSteps];
        for(int i = 0; i < akColorsFrontToBack.length; i++){
        	akColorsFrontToBack[i] = new Color3f();
        }

        int iNumStepsFrontToBack = 0;

        // Compute the step increment for front to back.
        float fStepX = (p1.x - p0.x) / iNumSteps;
        float fStepY = (p1.y - p0.y) / iNumSteps;
        float fStepZ = (p1.z - p0.z) / iNumSteps;

        // Start at the front and move toward the back.
        float fX = p0.x;
        float fY = p0.y;
        float fZ = p0.z;

        // Iterate from the front to back.
        for (int iStep = 0; iStep < iNumSteps; iStep++) {

            // compute the "min" corner of the nearest voxel.
            int iX = (int) fX;
            int iY = (int) fY;
            int iZ = (int) fZ;

            // make sure we are not trying to access voxels not in the volume
            if (((iX >= 0) && (iX <= m_iXBoundM2)) && ((iY >= 0) && (iY <= m_iYBoundM2)) &&
                    ((iZ >= 0) && (iZ <= m_iZBoundM2))) {

                // compute linear array index for the 8 corner samples
                int i000 = iX + (m_iXBound * iY) + (m_iXYProduct * iZ);
                int i100 = i000 + 1;
                int i010 = i000 + m_iXBound;
                int i110 = i100 + m_iXBound;
                int i001 = i000 + m_iXYProduct;
                int i101 = i100 + m_iXYProduct;
                int i011 = i010 + m_iXYProduct;
                int i111 = i110 + m_iXYProduct;

                // compute weight to apply to each of the 8 corner samples
                // determine scale to apply to each coordinate
                float fX1 = fX - iX;
                float fY1 = fY - iY;
                float fZ1 = fZ - iZ;
                float fX0 = 1.0f - fX1;
                float fY0 = 1.0f - fY1;
                float fZ0 = 1.0f - fZ1;
                float fS000 = fX0 * fY0 * fZ0;
                float fS001 = fX0 * fY0 * fZ1;
                float fS010 = fX0 * fY1 * fZ0;
                float fS011 = fX0 * fY1 * fZ1;
                float fS100 = fX1 * fY0 * fZ0;
                float fS101 = fX1 * fY0 * fZ1;
                float fS110 = fX1 * fY1 * fZ0;
                float fS111 = fX1 * fY1 * fZ1;

                // Interpolate material RGB colors.
                Color3f kColor = akColorsFrontToBack[iNumStepsFrontToBack];
                kColor.x = (fS000 * (m_acImageR[i000] & 0x0ff)) + (fS001 * (m_acImageR[i001] & 0x0ff)) +
                           (fS010 * (m_acImageR[i010] & 0x0ff)) + (fS011 * (m_acImageR[i011] & 0x0ff)) +
                           (fS100 * (m_acImageR[i100] & 0x0ff)) + (fS101 * (m_acImageR[i101] & 0x0ff)) +
                           (fS110 * (m_acImageR[i110] & 0x0ff)) + (fS111 * (m_acImageR[i111] & 0x0ff));
                kColor.y = (fS000 * (m_acImageG[i000] & 0x0ff)) + (fS001 * (m_acImageG[i001] & 0x0ff)) +
                           (fS010 * (m_acImageG[i010] & 0x0ff)) + (fS011 * (m_acImageG[i011] & 0x0ff)) +
                           (fS100 * (m_acImageG[i100] & 0x0ff)) + (fS101 * (m_acImageG[i101] & 0x0ff)) +
                           (fS110 * (m_acImageG[i110] & 0x0ff)) + (fS111 * (m_acImageG[i111] & 0x0ff));
                kColor.z = (fS000 * (m_acImageB[i000] & 0x0ff)) + (fS001 * (m_acImageB[i001] & 0x0ff)) +
                           (fS010 * (m_acImageB[i010] & 0x0ff)) + (fS011 * (m_acImageB[i011] & 0x0ff)) +
                           (fS100 * (m_acImageB[i100] & 0x0ff)) + (fS101 * (m_acImageB[i101] & 0x0ff)) +
                           (fS110 * (m_acImageB[i110] & 0x0ff)) + (fS111 * (m_acImageB[i111] & 0x0ff));

                // Interpolate alpha
                float fSrcA = (fS000 * (m_acImageA[i000] & 0x0ff)) + (fS001 * (m_acImageA[i001] & 0x0ff)) +
                              (fS010 * (m_acImageA[i010] & 0x0ff)) + (fS011 * (m_acImageA[i011] & 0x0ff)) +
                              (fS100 * (m_acImageA[i100] & 0x0ff)) + (fS101 * (m_acImageA[i101] & 0x0ff)) +
                              (fS110 * (m_acImageA[i110] & 0x0ff)) + (fS111 * (m_acImageA[i111] & 0x0ff));

                // Save the information along the ray.
                afAlphasFrontToBack[iNumStepsFrontToBack] = fSrcA;
                iNumStepsFrontToBack++;

                // Stop when fully opaque since nothing behind will affect
                // the blended result.
                if (fSrcA >= 255.0f) {
                    break;
                }
            }

            // step
            fX += fStepX;
            fY += fStepY;
            fZ += fStepZ;
        }


        // Iterate through the ray samples in reverse order.
        for (int iStep = iNumStepsFrontToBack - 1; iStep >= 0; --iStep) {

            // Access position, normal, material, and alpha current
            // sample along ray.
            Color3f kColor = akColorsFrontToBack[iStep];
            float fSrcA = afAlphasFrontToBack[iStep];

            // Scale so that alpha is in [0,1] range
            // fSrcA /= 255.0f;
            fSrcA *= 0.003922f;

            float fSrcR = kColor.x;
            float fSrcG = kColor.y;
            float fSrcB = kColor.z;

            // voxel is opaque
            if (fSrcA >= 1.0f) {
                m_aiRImage[iIndex] = (((int) fSrcR & 0xff) << 16) | (((int) fSrcG & 0xff) << 8) | ((int) fSrcB & 0xff);
            }

            // voxel is semitransparent so need to blend
            else {

                // get the RGBA values from the destination
                float fTrgR = ((m_aiRImage[iIndex] >> 16) & 0xff);
                float fTrgG = ((m_aiRImage[iIndex] >> 8) & 0xff);
                float fTrgB = ((m_aiRImage[iIndex]) & 0xff);
                float fTrgA = 1.0f - fSrcA;

                // composite the color values
                float fResR = (fSrcR * fSrcA) + (fTrgR * fTrgA);
                float fResG = (fSrcG * fSrcA) + (fTrgG * fTrgA);
                float fResB = (fSrcB * fSrcA) + (fTrgB * fTrgA);
                m_aiRImage[iIndex] = 
                                     // (((int)255   & 0xff) << 24) |
                                     (((int) fResR & 0xff) << 16) | (((int) fResG & 0xff) << 8) | ((int) fResB & 0xff);
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
     * integer in the format B | (G << 8) | (R << 16). This method returns a yellowish value if the ray intersects a
     * region bounded by the level surface indicated by the level value class member. Otherwise a gray scale value is
     * returned.</p>
     *
     * @param  iIndex            int the index of the pixel corresponding to the processed ray
     * @param  rayTraceStepSize  int size of steps to take along ray being traced
     */
    protected void processRay(int iIndex, int rayTraceStepSize) {
    	Color3f[] akColorsFrontToBack = new Color3f[3000];
        float[] afAlphasFrontToBack = new float[3000];

        for (int i = 0; i < 3000; i++) {
            akColorsFrontToBack[i] = new Color3f();
        }
        
        // Compute the number of steps to use.  The number of
        // steps is proportional to the length of the line segment.
        m_kPDiff.sub(m_kP1, m_kP0);

        float fLength = m_kPDiff.length();
        int iNumSteps = 1 + (int) ((fLength / rayTraceStepSize) * 1f);

        int iNumStepsFrontToBack = 0;

        // Compute the step increment for front to back.
        float fStepX = (m_kP1.x - m_kP0.x) / iNumSteps;
        float fStepY = (m_kP1.y - m_kP0.y) / iNumSteps;
        float fStepZ = (m_kP1.z - m_kP0.z) / iNumSteps;

        // Start at the front and move toward the back.
        float fX = m_kP0.x;
        float fY = m_kP0.y;
        float fZ = m_kP0.z;

        // Iterate from the front to back.
        for (int iStep = 0; iStep < iNumSteps; iStep++) {

            // compute the "min" corner of the nearest voxel.
            int iX = (int) fX;
            int iY = (int) fY;
            int iZ = (int) fZ;

            // make sure we are not trying to access voxels not in the volume
            if (((iX >= 0) && (iX <= m_iXBoundM2)) && ((iY >= 0) && (iY <= m_iYBoundM2)) &&
                    ((iZ >= 0) && (iZ <= m_iZBoundM2))) {

                // compute linear array index for the 8 corner samples
                int i000 = iX + (m_iXBound * iY) + (m_iXYProduct * iZ);
                int i100 = i000 + 1;
                int i010 = i000 + m_iXBound;
                int i110 = i100 + m_iXBound;
                int i001 = i000 + m_iXYProduct;
                int i101 = i100 + m_iXYProduct;
                int i011 = i010 + m_iXYProduct;
                int i111 = i110 + m_iXYProduct;

                // compute weight to apply to each of the 8 corner samples
                // determine scale to apply to each coordinate
                float fX1 = fX - iX;
                float fY1 = fY - iY;
                float fZ1 = fZ - iZ;
                float fX0 = 1.0f - fX1;
                float fY0 = 1.0f - fY1;
                float fZ0 = 1.0f - fZ1;
                float fS000 = fX0 * fY0 * fZ0;
                float fS001 = fX0 * fY0 * fZ1;
                float fS010 = fX0 * fY1 * fZ0;
                float fS011 = fX0 * fY1 * fZ1;
                float fS100 = fX1 * fY0 * fZ0;
                float fS101 = fX1 * fY0 * fZ1;
                float fS110 = fX1 * fY1 * fZ0;
                float fS111 = fX1 * fY1 * fZ1;

                // Interpolate material RGB colors.
                Color3f kColor = akColorsFrontToBack[iNumStepsFrontToBack];
                kColor.x = (fS000 * (m_acImageR[i000] & 0x0ff)) + (fS001 * (m_acImageR[i001] & 0x0ff)) +
                           (fS010 * (m_acImageR[i010] & 0x0ff)) + (fS011 * (m_acImageR[i011] & 0x0ff)) +
                           (fS100 * (m_acImageR[i100] & 0x0ff)) + (fS101 * (m_acImageR[i101] & 0x0ff)) +
                           (fS110 * (m_acImageR[i110] & 0x0ff)) + (fS111 * (m_acImageR[i111] & 0x0ff));
                kColor.y = (fS000 * (m_acImageG[i000] & 0x0ff)) + (fS001 * (m_acImageG[i001] & 0x0ff)) +
                           (fS010 * (m_acImageG[i010] & 0x0ff)) + (fS011 * (m_acImageG[i011] & 0x0ff)) +
                           (fS100 * (m_acImageG[i100] & 0x0ff)) + (fS101 * (m_acImageG[i101] & 0x0ff)) +
                           (fS110 * (m_acImageG[i110] & 0x0ff)) + (fS111 * (m_acImageG[i111] & 0x0ff));
                kColor.z = (fS000 * (m_acImageB[i000] & 0x0ff)) + (fS001 * (m_acImageB[i001] & 0x0ff)) +
                           (fS010 * (m_acImageB[i010] & 0x0ff)) + (fS011 * (m_acImageB[i011] & 0x0ff)) +
                           (fS100 * (m_acImageB[i100] & 0x0ff)) + (fS101 * (m_acImageB[i101] & 0x0ff)) +
                           (fS110 * (m_acImageB[i110] & 0x0ff)) + (fS111 * (m_acImageB[i111] & 0x0ff));

                // Interpolate alpha
                float fSrcA = (fS000 * (m_acImageA[i000] & 0x0ff)) + (fS001 * (m_acImageA[i001] & 0x0ff)) +
                              (fS010 * (m_acImageA[i010] & 0x0ff)) + (fS011 * (m_acImageA[i011] & 0x0ff)) +
                              (fS100 * (m_acImageA[i100] & 0x0ff)) + (fS101 * (m_acImageA[i101] & 0x0ff)) +
                              (fS110 * (m_acImageA[i110] & 0x0ff)) + (fS111 * (m_acImageA[i111] & 0x0ff));

                // Save the information along the ray.
                afAlphasFrontToBack[iNumStepsFrontToBack] = fSrcA;
                iNumStepsFrontToBack++;

                // Stop when fully opaque since nothing behind will affect
                // the blended result.
                if (fSrcA >= 255.0f) {
                    break;
                }
            }

            // step
            fX += fStepX;
            fY += fStepY;
            fZ += fStepZ;
        }


        // Iterate through the ray samples in reverse order.
        for (int iStep = iNumStepsFrontToBack - 1; iStep >= 0; --iStep) {

            // Access position, normal, material, and alpha current
            // sample along ray.
            Color3f kColor = akColorsFrontToBack[iStep];
            float fSrcA = afAlphasFrontToBack[iStep];

            // Scale so that alpha is in [0,1] range
            // fSrcA /= 255.0f;
            fSrcA *= 0.003922f;

            float fSrcR = kColor.x;
            float fSrcG = kColor.y;
            float fSrcB = kColor.z;

            // voxel is opaque
            if (fSrcA >= 1.0f) {
                m_aiRImage[iIndex] = (((int) fSrcR & 0xff) << 16) | (((int) fSrcG & 0xff) << 8) | ((int) fSrcB & 0xff);
            }

            // voxel is semitransparent so need to blend
            else {

                // get the RGBA values from the destination
                float fTrgR = ((m_aiRImage[iIndex] >> 16) & 0xff);
                float fTrgG = ((m_aiRImage[iIndex] >> 8) & 0xff);
                float fTrgB = ((m_aiRImage[iIndex]) & 0xff);
                float fTrgA = 1.0f - fSrcA;

                // composite the color values
                float fResR = (fSrcR * fSrcA) + (fTrgR * fTrgA);
                float fResG = (fSrcG * fSrcA) + (fTrgG * fTrgA);
                float fResB = (fSrcB * fSrcA) + (fTrgB * fTrgA);
                m_aiRImage[iIndex] = 
                                     // (((int)255   & 0xff) << 24) |
                                     (((int) fResR & 0xff) << 16) | (((int) fResG & 0xff) << 8) | ((int) fResB & 0xff);
            }
        }
    }

    /**
     * Called at the beginning of the trace methods.
     */
    protected void traceInit() {
        super.traceInit();
    }
}
