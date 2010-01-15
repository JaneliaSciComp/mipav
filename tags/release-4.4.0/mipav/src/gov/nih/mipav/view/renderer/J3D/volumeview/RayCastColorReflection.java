package gov.nih.mipav.view.renderer.J3D.volumeview;


import gov.nih.mipav.model.structures.*;

import gov.nih.mipav.view.renderer.J3D.*;

import javax.vecmath.*;


/**
 * Ray traced rendering of the level surface within the voxel for the fixed level value of 0.0.
 */

public class RayCastColorReflection extends RayCastColor {

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
    public RayCastColorReflection(ModelImage kImage, int iRBound, int[] aiRImage) {
        super(kImage, iRBound, aiRImage);
    }

    //~ Methods --------------------------------------------------------------------------------------------------------

    /**
     * Clean memory.
     *
     * @param  flag  is true call the super.disposeLocal
     */

    public void disposeLocal(boolean flag) {

        if (flag == true) {
            super.disposeLocal();
        }
    }

    /**
     * Setup the specified set of lights to use for rendering.
     *
     * @param  kLightSet   SoftwareLightSet Set of world/model lights.
     * @param  fShininess  float Non-negative shininess value to use for specular lighting. If this value is zero, then
     *                     specular lighting is not computed. The larger the value, the "sharper" will be the specular
     *                     highlights.
     */
    public void setLighting(SoftwareLightSet kLightSet, float fShininess) {
        m_kLightSet = kLightSet;
        m_kMaterial.shininess = fShininess;
    }

    /**
     * Return indication as to whether or not the particular renderer uses normal vectors as part of its implementation.
     *
     * @return  boolean True if the implementation uses normals.
     */
    public boolean usesNormals() {
        return true;
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
     * members P0 and P1 in image coordinates, which are used for multithreading rendering. This method uses the Trapezoid Rule to numerically integrates the
     * image along the line segment. The number of integration samples is chosen to be proportional to the length of the
     * line segment.
     *
     * <p>The function sets the color of the pixel corresponding to the processed ray. The RGB value is stored as an
     * integer in the format B | (G << 8) | (R << 16). This method returns a yellowish value if the ray intersects a
     * region bounded by the level surface indicated by the level value class member. Otherwise a gray scale value is
     * returned.</p>
     *
     * @param  p0                ray trace starting point
     * @param  p1                ray trace ending point
     * @param  iIndex            int the index of the pixel corresponding to the processed ray
     * @param  rayTraceStepSize  int size of steps to take along ray being traced
     */
    protected synchronized void processRay(Point3f p0, Point3f p1, int iIndex, int rayTraceStepSize) {

        // Compute the number of steps to use.  The number of
        // steps is proportional to the length of the line segment.
    	// Allocate arrays to store interpolated positions, normal vectors.
        SoftwareVertexProperty kVertexProperty = new SoftwareVertexProperty();
        kVertexProperty.enableDiffuse(true);
        kVertexProperty.enableSpecular(true);
        
        
        // RGBA values along the ray in order from front-to-back.
        float afAlphasFrontToBack = 0;
        
        Vector3f lineSeg = new Vector3f();
        lineSeg.sub(p1, p0);
        
        float fLength = lineSeg.length();

        int iNumSteps = 1 + (int) ((fLength / rayTraceStepSize) * 1f);
        // int iNumStepsFrontToBack = 0;

        // Compute the step increment for front to back.
        float fStepX = (p1.x - p0.x) / iNumSteps;
        float fStepY = (p1.y - p0.y) / iNumSteps;
        float fStepZ = (p1.z - p0.z) / iNumSteps;

        // Start at the front and move toward the back.
        float fX = p0.x;
        float fY = p0.y;
        float fZ = p0.z;
        int iStep;

        // Iterate from the front to back.
        for (iStep = 0; iStep < iNumSteps; iStep++) {

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

                // Interpolated position along ray.
                kVertexProperty.setPosition(fX, fY, fZ);

                // Interpolate alpha
                float fSrcA = (fS000 * (m_acImageA[i000] & 0x0ff)) + (fS001 * (m_acImageA[i001] & 0x0ff)) +
                              (fS010 * (m_acImageA[i010] & 0x0ff)) + (fS011 * (m_acImageA[i011] & 0x0ff)) +
                              (fS100 * (m_acImageA[i100] & 0x0ff)) + (fS101 * (m_acImageA[i101] & 0x0ff)) +
                              (fS110 * (m_acImageA[i110] & 0x0ff)) + (fS111 * (m_acImageA[i111] & 0x0ff));


                if (fSrcA > 0) {

                    afAlphasFrontToBack = fSrcA;

                    // Interpolate normal vector.
                    Vector3f kNormal000 = m_akNormal[i000];
                    Vector3f kNormal100 = m_akNormal[i100];
                    Vector3f kNormal010 = m_akNormal[i010];
                    Vector3f kNormal110 = m_akNormal[i110];
                    Vector3f kNormal001 = m_akNormal[i001];
                    Vector3f kNormal101 = m_akNormal[i101];
                    Vector3f kNormal011 = m_akNormal[i011];
                    Vector3f kNormal111 = m_akNormal[i111];

                    float fNx = (fS000 * kNormal000.x) + (fS001 * kNormal001.x) + (fS010 * kNormal010.x) +
                                (fS011 * kNormal011.x) + (fS100 * kNormal100.x) + (fS101 * kNormal101.x) +
                                (fS110 * kNormal110.x) + (fS111 * kNormal111.x);
                    float fNy = (fS000 * kNormal000.y) + (fS001 * kNormal001.y) + (fS010 * kNormal010.y) +
                                (fS011 * kNormal011.y) + (fS100 * kNormal100.y) + (fS101 * kNormal101.y) +
                                (fS110 * kNormal110.y) + (fS111 * kNormal111.y);
                    float fNz = (fS000 * kNormal000.z) + (fS001 * kNormal001.z) + (fS010 * kNormal010.z) +
                                (fS011 * kNormal011.z) + (fS100 * kNormal100.z) + (fS101 * kNormal101.z) +
                                (fS110 * kNormal110.z) + (fS111 * kNormal111.z);
                    kVertexProperty.setNormal(fNx, fNy, fNz);
                    kVertexProperty.setDiffuse(vertexDiffuse);
                    kVertexProperty.setSpecular(vertexSpecular);

                    iStep = iNumSteps;
                } else {
                    kVertexProperty.setNormal(0f, 0f, 0f);
                    kVertexProperty.setDiffuse(0f, 0f, 0f);
                    kVertexProperty.setSpecular(0f, 0f, 0f);
                    afAlphasFrontToBack = 0;
                }

                // Stop when fully opaque since nothing behind will affect the blended result.
                if (fSrcA >= 255.0f) {
                    break;
                }
            }

            // step
            fX += fStepX;
            fY += fStepY;
            fZ += fStepZ;
        }

        // Access alpha along the ray.
        // Scale it so that it is in [0,1] range.

        float fSrcA = afAlphasFrontToBack;

        if (fSrcA == 0) {
            return;
        }

        
        Color3f m_kColor = m_kLightSet.getCellColor(m_kMaterial, kVertexProperty, m_kEyeModel);
        
        float fSrcR = m_kColor.x * 255.0f;
        float fSrcG = m_kColor.y * 255.0f;
        float fSrcB = m_kColor.z * 255.0f;
        
        m_aiRImage[iIndex] = 
                             // (((int)255   & 0xff) << 24) |
                             (((int) (fSrcR) & 0xff) << 16) | (((int) (fSrcG) & 0xff) << 8) | ((int) (fSrcB) & 0xff);
        kVertexProperty = null;
        
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

        // Compute the number of steps to use.  The number of
        // steps is proportional to the length of the line segment.
    	// Allocate arrays to store interpolated positions, normal vectors.
        SoftwareVertexProperty kVertexProperty = new SoftwareVertexProperty();
        kVertexProperty.enableDiffuse(true);
        kVertexProperty.enableSpecular(true);
        // RGBA values along the ray in order from front-to-back.
        float afAlphasFrontToBack = 0;
        
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
        int iStep;

        // Iterate from the front to back.
        for (iStep = 0; iStep < iNumSteps; iStep++) {

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

                // Interpolated position along ray.
                kVertexProperty.setPosition(fX, fY, fZ);

                // Interpolate alpha
                float fSrcA = (fS000 * (m_acImageA[i000] & 0x0ff)) + (fS001 * (m_acImageA[i001] & 0x0ff)) +
                              (fS010 * (m_acImageA[i010] & 0x0ff)) + (fS011 * (m_acImageA[i011] & 0x0ff)) +
                              (fS100 * (m_acImageA[i100] & 0x0ff)) + (fS101 * (m_acImageA[i101] & 0x0ff)) +
                              (fS110 * (m_acImageA[i110] & 0x0ff)) + (fS111 * (m_acImageA[i111] & 0x0ff));


                if (fSrcA > 0) {

                    afAlphasFrontToBack = fSrcA;

                    // Interpolate normal vector.
                    Vector3f kNormal000 = m_akNormal[i000];
                    Vector3f kNormal100 = m_akNormal[i100];
                    Vector3f kNormal010 = m_akNormal[i010];
                    Vector3f kNormal110 = m_akNormal[i110];
                    Vector3f kNormal001 = m_akNormal[i001];
                    Vector3f kNormal101 = m_akNormal[i101];
                    Vector3f kNormal011 = m_akNormal[i011];
                    Vector3f kNormal111 = m_akNormal[i111];

                    float fNx = (fS000 * kNormal000.x) + (fS001 * kNormal001.x) + (fS010 * kNormal010.x) +
                                (fS011 * kNormal011.x) + (fS100 * kNormal100.x) + (fS101 * kNormal101.x) +
                                (fS110 * kNormal110.x) + (fS111 * kNormal111.x);
                    float fNy = (fS000 * kNormal000.y) + (fS001 * kNormal001.y) + (fS010 * kNormal010.y) +
                                (fS011 * kNormal011.y) + (fS100 * kNormal100.y) + (fS101 * kNormal101.y) +
                                (fS110 * kNormal110.y) + (fS111 * kNormal111.y);
                    float fNz = (fS000 * kNormal000.z) + (fS001 * kNormal001.z) + (fS010 * kNormal010.z) +
                                (fS011 * kNormal011.z) + (fS100 * kNormal100.z) + (fS101 * kNormal101.z) +
                                (fS110 * kNormal110.z) + (fS111 * kNormal111.z);
                    kVertexProperty.setNormal(fNx, fNy, fNz);
                    kVertexProperty.setDiffuse(vertexDiffuse);
                    kVertexProperty.setSpecular(vertexSpecular);

                    iStep = iNumSteps;
                } else {
                    kVertexProperty.setNormal(0f, 0f, 0f);
                    kVertexProperty.setDiffuse(0f, 0f, 0f);
                    kVertexProperty.setSpecular(0f, 0f, 0f);
                    afAlphasFrontToBack = 0;
                }

                // Stop when fully opaque since nothing behind will affect the blended result.
                if (fSrcA >= 255.0f) {
                    break;
                }
            }

            // step
            fX += fStepX;
            fY += fStepY;
            fZ += fStepZ;
        }

        // Access alpha along the ray.
        // Scale it so that it is in [0,1] range.

        float fSrcA = afAlphasFrontToBack;

        if (fSrcA == 0) {
            return;
        }

        // apply the lighting to determine the color
        Color3f m_kColor = m_kLightSet.getCellColor(m_kMaterial, kVertexProperty, m_kEyeModel);

        float fSrcR = m_kColor.x * 255.0f;
        float fSrcG = m_kColor.y * 255.0f;
        float fSrcB = m_kColor.z * 255.0f;

        m_aiRImage[iIndex] = 
                             // (((int)255   & 0xff) << 24) |
                             (((int) (fSrcR) & 0xff) << 16) | (((int) (fSrcG) & 0xff) << 8) | ((int) (fSrcB) & 0xff);
    
        kVertexProperty = null;
    }

    /**
     * Called at the beginning of the trace methods.
     */
    protected void traceInit() {
        super.traceInit();

        // convert each world space light to model space coordinates
        m_kLightSet.applyWorldToModelTransform(m_akAxis[0], m_akAxis[1], m_akAxis[2]);
    }
}
