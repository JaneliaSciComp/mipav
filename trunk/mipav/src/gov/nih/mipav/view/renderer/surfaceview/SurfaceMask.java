package gov.nih.mipav.view.renderer.surfaceview;

import java.util.*;
import javax.vecmath.*;
import javax.media.j3d.*;

import gov.nih.mipav.model.structures.*;

/**
 * SurfaceMask. Calculates the surface volume mask (BitSet) for a triangle
 * mesh surface displayed in the SurfaceRender class. Both boundary masks and
 * filled-volume masks are calculated by the maskInsideVoxels() function.  <p>
 * The boundary-mask is stored in the ModelStorageBase class and is used to
 * display the triangle mesh intersection with the 2D plane in the PlaneRender
 * class. The volume-mask is stored in the SurfaceAttributes class and is
 * accessed through the SurfaceAttributes.getMask() member function. The
 * volume mask is used in the BurnBaseView.calcTotalVolume() function.
 *
 *
 * @see JPanelSurface.java
 * @see SurfaceRender.java
 * @see SurfaceAttributes.java
 * @see ModelStorageBase.java
 * @see BurnBaseView.java
 *
 */
public class SurfaceMask
{

    /** Volume-Filled TriangleMesh mask. */
    private BitSet mVolumeMask;

    /** Dimensions of imageA. */
    private int m_iXBound, m_iYBound, m_iZBound;

    /** Product of the image dimensions. */
    private int m_iQuantity;

    /** The plane coordinate x,y dimensions: */
    private float m_fX0, m_fY0, m_fZ0, m_fX1, m_fY1, m_fZ1;

    /** ModelImage image array dimensions: */
    private int xDim, yDim, zDim;

    /** Default Constructor: */
    SurfaceMask() {}

    /**
     * Mask the mesh surface volume in voxels.
     *
     * @param index Mask index in the ModelImage imageA
     * @param imageA ModelImage for storing the surface-mask
     * @param kMesh ModelTriangleMesh[] surfaces
     * @param bHasVertexColor use per-vertex color from the ModelTriangleMesh,
     * otherwise use the surColor parameter
     * @param fOpacity Mask opacity
     * @param surColor default Mask color
     * @param xBox normalized SurfaceRender x-dimension
     * @param yBox normalized SurfaceRender y-dimension
     * @param zBox normalized SurfaceRender z-dimension
     */
    public void maskInsideVoxels(int index, ModelImage imageA,
                                 ModelTriangleMesh[] kMesh,
                                 boolean bHasVertexColor, boolean bCreateVertexColors, boolean bUseImageMask,
                                 float fOpacity, Color4f surColor )
    {

        float iX, iY, iZ;

        int[] extents = imageA.getExtents();
        xDim = extents[0];
        yDim = extents[1];
        zDim = extents[2];

        m_iQuantity = xDim * yDim * zDim;

        BitSet kImageMask = null;
        Color4f kImageMaskColor = null;
        if ( bUseImageMask )
        {
            kImageMask = imageA.getSurfaceMask( index );
            kImageMaskColor = imageA.getSurfaceMaskColor( index );
        }
        int[] iterFactors = imageA.getVolumeIterationFactors( );

        BitSet kMask = new BitSet(m_iQuantity);
        Color4f[] akMaskColor = new Color4f[m_iQuantity];

        // initialize the surface mask
        mVolumeMask = new BitSet(m_iQuantity);

        m_iXBound = xDim;
        m_iYBound = yDim;
        m_iZBound = zDim;

        float[] resols = imageA.getFileInfo()[0].getResolutions();

        // local x, y, z box viarables.  Those local viarables make sure that xBox, yBox
        // and zBox not changed in local method.
        float xB, yB, zB, maxB;
        xB = (xDim - 1) * resols[0];
        yB = (yDim - 1) * resols[1];
        zB = (zDim - 1) * resols[2];
        maxB = xB;
        if (yB > maxB) {
            maxB = yB;
        }
        if (zB > maxB) {
            maxB = zB;
        }
        // Normalize the size
        // xBox range between 0 - 1.
        xB = xB / maxB;
        yB = yB / maxB;
        zB = zB / maxB;

        m_fX0 = -xB;
        m_fY0 = -yB;
        m_fX1 = xB;
        m_fY1 = yB;
        m_fZ0 = -zB;
        m_fZ1 = zB;

        maxB = xB;

        if (yB > maxB) {
            maxB = yB;
        }

        if (zB > maxB) {
            maxB = zB;
        }

        if (zB > maxB) {
            m_fZ0 = -1f;
            m_fZ1 = 1f;
        }

        if (kMesh == null) {
            return;
        }

        for ( int mIndex = 0; mIndex < kMesh.length; mIndex++ ) {
            // Get the non-resampled surface volume voxels.
            Point3f[] akVertex = kMesh[mIndex].getVertexCopy();
            int[] aiConnect = kMesh[mIndex].getIndexCopy();
            int iTQuantity = (int) (aiConnect.length / 3);
            TexCoord3f[] akTexCoords = new TexCoord3f[ akVertex.length ];

            
            int iVQuantity = kMesh[mIndex].getVertexCount();
            Color4f[] kTriColors = new Color4f[iVQuantity];
            
            if (bHasVertexColor && kMesh[mIndex].getCapability(GeometryArray.ALLOW_COLOR_READ)) {
                
                for (int iC = 0; iC < iVQuantity; iC++) {
                    kTriColors[iC] = new Color4f();
                }
                
                kMesh[mIndex].getColors(0, kTriColors);
            }
            else if (bCreateVertexColors && kMesh[mIndex].getCapability(GeometryArray.ALLOW_COLOR_WRITE)) {
                for (int iC = 0; iC < iVQuantity; iC++) {
                    kTriColors[iC] = new Color4f( 1f, 1f, 1f, 1f );
                }
                bHasVertexColor = true;
            }
            
            Point3f tV0 = new Point3f();
            Point3f tV1 = new Point3f();
            Point3f tV2 = new Point3f();
            
            Point3f kV0 = new Point3f();
            Point3f kV1 = new Point3f();
            Point3f kV2 = new Point3f();
            
            Color4f kC0, kC1, kC2;
            
            for (int iT = 0; iT < iTQuantity; iT++) {
                
                // get the vertices of the triangle
                tV0 = akVertex[aiConnect[3 * iT]];
                tV1 = akVertex[aiConnect[ (3 * iT) + 1]];
                tV2 = akVertex[aiConnect[ (3 * iT) + 2]];

                kV0.x = tV0.x;
                kV0.y = tV0.y;
                kV0.z = tV0.z;
                kV1.x = tV1.x;
                kV1.y = tV1.y;
                kV1.z = tV1.z;
                kV2.x = tV2.x;
                kV2.y = tV2.y;
                kV2.z = tV2.z;

                Color4f kTriColor = null;

                if ( bCreateVertexColors )
                {
                    Point3f mV0 = getModelImagePoint( tV0 );
                    Point3f mV1 = getModelImagePoint( tV1 );
                    Point3f mV2 = getModelImagePoint( tV2 );
                    
                    if ( !imageA.isColorImage() )
                    {
                        float value = imageA.getFloat( (int)mV0.x, (int)mV0.y, (int)mV0.z ) / 255.0f;
                        kTriColors[aiConnect[3 * iT]] = new Color4f( value, value, value, 1 - fOpacity );

                        value = imageA.getFloat( (int)mV1.x, (int)mV1.y, (int)mV1.z ) / 255.0f;
                        kTriColors[aiConnect[ (3 * iT) + 1]] = new Color4f( value, value, value, 1 - fOpacity );

                        value = imageA.getFloat( (int)mV2.x, (int)mV2.y, (int)mV2.z ) / 255.0f;
                        kTriColors[aiConnect[ (3 * iT) + 2]] = new Color4f( value, value, value, 1 - fOpacity );
                    }

                    if ( bUseImageMask && (kImageMask != null) && (kImageMaskColor != null) )
                    {
                        int maskIndex =  (int)((iterFactors[0] * mV0.x) +
                                           (iterFactors[1] * mV0.y) +
                                           (iterFactors[2] * mV0.z)   );
                        if ( kImageMask.get( maskIndex ) )
                        {
                            kTriColors[aiConnect[3 * iT]] = new Color4f( kImageMaskColor.x,
                                                                         kImageMaskColor.y,
                                                                         kImageMaskColor.z,
                                                                         kImageMaskColor.w );
                        }
                        maskIndex =  (int)((iterFactors[0] * mV1.x) +
                                       (iterFactors[1] * mV1.y) +
                                       (iterFactors[2] * mV1.z)   );
                        if ( kImageMask.get( maskIndex ) )
                        {
                            kTriColors[aiConnect[(3 * iT) + 1]] = new Color4f( kImageMaskColor.x,
                                                                               kImageMaskColor.y,
                                                                               kImageMaskColor.z,
                                                                               kImageMaskColor.w );
                        }
                        maskIndex =  (int)((iterFactors[0] * mV2.x) +
                                       (iterFactors[1] * mV2.y) +
                                       (iterFactors[2] * mV2.z)   );
                        if ( kImageMask.get( maskIndex ) )
                        {
                            kTriColors[aiConnect[(3 * iT) + 2]] = new Color4f( kImageMaskColor.x,
                                                                               kImageMaskColor.y,
                                                                               kImageMaskColor.z,
                                                                               kImageMaskColor.w );
                        }
                    }
                }

                kC0 = kTriColors[aiConnect[3 * iT]];
                kC1 = kTriColors[aiConnect[ (3 * iT) + 1]];
                kC2 = kTriColors[aiConnect[ (3 * iT) + 2]];

                if ( (kC0 != null) && (kC1 != null) && (kC2 != null)) {
                    kTriColor = new Color4f( (kC0.x + kC1.x + kC2.x) / 3.0f,
                                             (kC0.y + kC1.y + kC2.y) / 3.0f,
                                             (kC0.z + kC1.z + kC2.z) / 3.0f,
                                             1 - fOpacity );
                }

                kV0.x = ( (kV0.x - m_fX0) / (m_fX1 - m_fX0)) * (xDim - 1);
                kV0.y = ( (kV0.y - m_fY0) / (m_fY1 - m_fY0)) * (yDim - 1);
                kV0.z = ( (kV0.z - m_fZ0) / (m_fZ1 - m_fZ0)) * (zDim - 1);

                kV1.x = ( (kV1.x - m_fX0) / (m_fX1 - m_fX0)) * (xDim - 1);
                kV1.y = ( (kV1.y - m_fY0) / (m_fY1 - m_fY0)) * (yDim - 1);
                kV1.z = ( (kV1.z - m_fZ0) / (m_fZ1 - m_fZ0)) * (zDim - 1);

                kV2.x = ( (kV2.x - m_fX0) / (m_fX1 - m_fX0)) * (xDim - 1);
                kV2.y = ( (kV2.y - m_fY0) / (m_fY1 - m_fY0)) * (yDim - 1);
                kV2.z = ( (kV2.z - m_fZ0) / (m_fZ1 - m_fZ0)) * (zDim - 1);

                kV0.z = zDim - 1 - kV0.z;
                kV1.z = zDim - 1 - kV1.z;
                kV2.z = zDim - 1 - kV2.z;

                // Sets the texture coordinates for the ModelTriangleMesh
                // based on the correspondence with the ModelImage voxel
                // location.
                akTexCoords[aiConnect[3 * iT]] = new TexCoord3f( kV0.x/(float)(xDim-1),
                                                                 kV0.y/(float)(yDim-1),
                                                                 kV0.z/(float)(zDim-1) );

                akTexCoords[aiConnect[3 * iT + 1]] = new TexCoord3f( kV1.x/(float)(xDim-1),
                                                                    kV1.y/(float)(yDim-1),
                                                                    kV1.z/(float)(zDim-1) );
                akTexCoords[aiConnect[3 * iT + 2]] = new TexCoord3f( kV2.x/(float)(xDim-1),
                                                                     kV2.y/(float)(yDim-1),
                                                                     kV2.z/(float)(zDim-1) );


                kV0.y = yDim - 1 - kV0.y;
                kV1.y = yDim - 1 - kV1.y;
                kV2.y = yDim - 1 - kV2.y;


                // compute the axis-aligned bounding box of the triangle
                float fXMin = kV0.x, fXMax = fXMin;
                float fYMin = kV0.y, fYMax = fYMin;
                float fZMin = kV0.z, fZMax = fZMin;

                if (kV1.x < fXMin) {
                    fXMin = kV1.x;
                }
                else if (kV1.x > fXMax) {
                    fXMax = kV1.x;
                }

                if (kV1.y < fYMin) {
                    fYMin = kV1.y;
                }
                else if (kV1.y > fYMax) {
                    fYMax = kV1.y;
                }

                if (kV1.z < fZMin) {
                    fZMin = kV1.z;
                }
                else if (kV1.z > fZMax) {
                    fZMax = kV1.z;
                }

                if (kV2.x < fXMin) {
                    fXMin = kV2.x;
                }
                else if (kV2.x > fXMax) {
                    fXMax = kV2.x;
                }

                if (kV2.y < fYMin) {
                    fYMin = kV2.y;
                }
                else if (kV2.y > fYMax) {
                    fYMax = kV2.y;
                }

                if (kV2.z < fZMin) {
                    fZMin = kV2.z;
                }
                else if (kV2.z > fZMax) {
                    fZMax = kV2.z;
                }

                // Rasterize the triangle.  The rasterization is repeated in all
                // three coordinate directions to make sure that floating point
                // round-off errors do not cause any holes in the rasterized
                // surface.
                float iXMin = fXMin, iXMax = fXMax;
                float iYMin = fYMin, iYMax = fYMax;
                float iZMin = fZMin, iZMax = fZMax;
                int ptr;
                int end = kMask.size();

                for (iY = iYMin; iY < iYMax; iY = iY + 0.1f) {

                    for (iZ = iZMin; iZ < iZMax; iZ = iZ + 0.1f) {
                        iX = getIntersectX(kV0, kV1, kV2, iY, iZ);

                        if (iX != -1) {
                            ptr = getIndex( (int) Math.round(iX), (int) Math.round(iY),
                                            (int) Math.round(iZ));

                            if ( (ptr >= 0) && (ptr < end)) {
                                kMask.set(ptr);
                                akMaskColor[ptr] = kTriColor;
                                mVolumeMask.set(ptr);
                            }

                        }
                    }
                }

                for (iX = iXMin; iX < iXMax; iX = iX + 0.1f) {

                    for (iZ = iZMin; iZ < iZMax; iZ = iZ + 0.1f) {
                        iY = getIntersectY(kV0, kV1, kV2, iX, iZ);

                        if (iY != -1) {
                            ptr = getIndex( (int) Math.round(iX), (int) Math.round(iY),
                                            (int) Math.round(iZ));

                            if ( (ptr >= 0) && (ptr < end)) {
                                kMask.set(ptr);
                                akMaskColor[ptr] = kTriColor;
                                mVolumeMask.set(ptr);
                            }
                        }
                    }
                }

                for (iX = iXMin; iX < iXMax; iX = iX + 0.1f) {

                    for (iY = iYMin; iY < iYMax; iY = iY + 0.1f) {
                        iZ = getIntersectZ(kV0, kV1, kV2, iX, iY);

                        if (iZ != -1) {
                            ptr = getIndex( (int) Math.round(iX), (int) Math.round(iY),
                                            (int) Math.round(iZ));

                            if ( (ptr >= 0) && (ptr < end)) {
                                kMask.set(ptr);
                                akMaskColor[ptr] = kTriColor;
                                mVolumeMask.set(ptr);
                            }
                        }
                    }
                }
            }

            // compute centroid of the surface voxels to act as flood fill seed
            float fXC = 0.0f, fYC = 0.0f, fZC = 0.0f;
            int iCount = 0;

            for (iZ = 1; iZ < (m_iZBound - 1); iZ++) {

                for (iY = 1; iY < (m_iYBound - 1); iY++) {

                    for (iX = 1; iX < (m_iXBound - 1); iX++) {

                        if (kMask.get(getIndex( (int) iX, (int) iY, (int) iZ))) {
                            fXC += (float) iX;
                            fYC += (float) iY;
                            fZC += (float) iZ;
                            iCount++;
                        }
                    }
                }
            }

            float fInvCount = 1.0f / iCount;

            fXC *= fInvCount;
            fYC *= fInvCount;
            fZC *= fInvCount;

            floodFill( (int) fXC, (int) fYC, (int) fZC);

            kMesh[mIndex].setTextureCoordinates( 0, 0, akTexCoords );
            kMesh[mIndex].setTextureCoordinateIndices( 0, 0, aiConnect );
            
            if (bCreateVertexColors && kMesh[mIndex].getCapability(GeometryArray.ALLOW_COLOR_WRITE))
            {
                kMesh[mIndex].setColors( 0, kTriColors );
            }
        }
        int v = 0;

        for (int i = 0; i < mVolumeMask.size(); i++) {

            if (mVolumeMask.get(i)) {
                v++;
            }
        }

        System.err.println("voxels = " + v);
        System.err.println("volume = " + (v * resols[0] * resols[1] * resols[2]));

        if ( !bUseImageMask )
        {
            Color4f color = new Color4f( surColor.x, surColor.y, surColor.z, 1 - fOpacity );
            imageA.addSurfaceMask( index, kMask, akMaskColor, color );
        }
    }

    /**
     * Given a triangle vertex point from a ModelTriangleMesh, determines the
     * corresponding ModelImage index point.
     * @param kTrianglePoint ModelTriangleMesh point in mesh coordinates
     * @return ModelImage index point.
     */
    public Point3f getModelImagePoint( Point3f kTrianglePoint )
    {
        Point3f kV0 = new Point3f( kTrianglePoint.x, kTrianglePoint.y, kTrianglePoint.z );

        kV0.x = ( (kV0.x - m_fX0) / (m_fX1 - m_fX0)) * (xDim - 1);
        kV0.y = ( (kV0.y - m_fY0) / (m_fY1 - m_fY0)) * (yDim - 1);
        kV0.z = ( (kV0.z - m_fZ0) / (m_fZ1 - m_fZ0)) * (zDim - 1);

        kV0.z = zDim - 1 - kV0.z;
        kV0.y = yDim - 1 - kV0.y;

        return kV0;
    }


    /**
     * Get the current added surface bit mask.
     *
     * @return  BitSet surface volume bit set mask.
     */
    public BitSet getVolumeMask() {
        return (BitSet) (mVolumeMask.clone());
    }


    /**
     * Identify voxels enclosed by the brain surface by using a flood
     * fill. The flood fill is nonrecursive to avoid overflowing the program
     * stack.
     *
     * @param  iX  the x-value of the seed point for the fill
     * @param  iY  the y-value of the seed point for the fill
     * @param  iZ  the z-value of the seed point for the fill
     */
    protected void floodFill(int iX, int iY, int iZ) {

        // Allocate the maximum amount of space needed.   An empty stack has
        // iTop == -1.
        int[] aiXStack = new int[m_iQuantity];
        int[] aiYStack = new int[m_iQuantity];
        int[] aiZStack = new int[m_iQuantity];

        // An empty stack has iTop = -1.  Push seed point onto stack.  All
        // points pushed onto stack have background color zero.
        int iTop = 0;

        aiXStack[iTop] = iX;
        aiYStack[iTop] = iY;
        aiZStack[iTop] = iZ;

        while (iTop >= 0) { // stack is not empty

            // Read top of stack.  Do not pop since we need to return to this
            // top value later to restart the fill in a different direction.
            iX = aiXStack[iTop];
            iY = aiYStack[iTop];
            iZ = aiZStack[iTop];

            // fill the pixel
            mVolumeMask.set(getIndex(iX, iY, iZ));

            int iXp1 = iX + 1;

            if ((iXp1 < m_iXBound) && !mVolumeMask.get(getIndex(iXp1, iY, iZ))) {

                // push pixel with background color
                iTop++;
                aiXStack[iTop] = iXp1;
                aiYStack[iTop] = iY;
                aiZStack[iTop] = iZ;

                continue;
            }

            int iXm1 = iX - 1;

            if ((0 <= iXm1) && !mVolumeMask.get(getIndex(iXm1, iY, iZ))) {

                // push pixel with background color
                iTop++;
                aiXStack[iTop] = iXm1;
                aiYStack[iTop] = iY;
                aiZStack[iTop] = iZ;

                continue;
            }

            int iYp1 = iY + 1;

            if ((iYp1 < m_iYBound) && !mVolumeMask.get(getIndex(iX, iYp1, iZ))) {

                // push pixel with background color
                iTop++;
                aiXStack[iTop] = iX;
                aiYStack[iTop] = iYp1;
                aiZStack[iTop] = iZ;

                continue;
            }

            int iYm1 = iY - 1;

            if ((0 <= iYm1) && !mVolumeMask.get(getIndex(iX, iYm1, iZ))) {

                // push pixel with background color
                iTop++;
                aiXStack[iTop] = iX;
                aiYStack[iTop] = iYm1;
                aiZStack[iTop] = iZ;

                continue;
            }

            int iZp1 = iZ + 1;

            if ((iZp1 < m_iZBound) && !mVolumeMask.get(getIndex(iX, iY, iZp1))) {

                // push pixel with background color
                iTop++;
                aiXStack[iTop] = iX;
                aiYStack[iTop] = iY;
                aiZStack[iTop] = iZp1;

                continue;
            }

            int iZm1 = iZ - 1;

            if ((0 <= iZm1) && !mVolumeMask.get(getIndex(iX, iY, iZm1))) {

                // push pixel with background color
                iTop++;
                aiXStack[iTop] = iX;
                aiYStack[iTop] = iY;
                aiZStack[iTop] = iZm1;

                continue;
            }

            // Done in all directions, pop and return to search other
            // directions.
            iTop--;

        }

        aiXStack = null;
        aiYStack = null;
        aiZStack = null;
    }

    /**
     * A convenience function for mapping the 3D voxel position (iX,iY,iZ) to
     * a 1D array index. The images are stored as 1D arrays, so this function
     * is used frequently.
     *
     * @param   iX  the x-value of the voxel position
     * @param   iY  the y-value of the voxel position
     * @param   iZ  the z-value of the voxel position
     *
     * @return  the 1D array index corresponding to (iX,iY,iZ)
     */
    protected final int getIndex(int iX, int iY, int iZ) {
        return iX + (m_iXBound * (iY + (m_iYBound * iZ)));

    }

    /**
     * Compute the point of intersection between a line (0,iY,iZ)+t(1,0,0) and
     * the triangle defined by the three input points. All calculations are in
     * voxel coordinates and the x-value of the intersection point is
     * truncated to an integer.
     *
     * @param   kV0  a 3D vertex of the triangle
     * @param   kV1  a 3D vertex of the triangle
     * @param   kV2  a 3D vertex of the triangle
     * @param   iY   the y-value of the origin of the line
     * @param   iZ   the z-value of the origin of the line
     *
     * @return  the x-value of the intersection
     */
    protected float getIntersectX(Point3f kV0, Point3f kV1, Point3f kV2,
                                  float iY, float iZ) {

        // Compute the intersection, if any, by calculating barycentric
        // coordinates of the intersection of the line with the plane of
        // the triangle.  The barycentric coordinates are K0 = fC0/fDet,
        // K1 = fC1/fDet, and K2 = fC2/fDet with K0+K1+K2=1.  The intersection
        // point with the plane is K0*V0+K1*V1+K2*V2.  The point is inside
        // the triangle whenever K0, K1, and K2 are all in the interval [0,1].
        float fPu = iY - kV0.y, fPv = iZ - kV0.z;
        float fE1u = kV1.y - kV0.y, fE1v = kV1.z - kV0.z;
        float fE2u = kV2.y - kV0.y, fE2v = kV2.z - kV0.z;
        float fE1dP = (fE1u * fPu) + (fE1v * fPv);
        float fE2dP = (fE2u * fPu) + (fE2v * fPv);
        float fE1dE1 = (fE1u * fE1u) + (fE1v * fE1v);
        float fE1dE2 = (fE1u * fE2u) + (fE1v * fE2v);
        float fE2dE2 = (fE2u * fE2u) + (fE2v * fE2v);
        float fDet = (float) Math.abs((fE1dE1 * fE2dE2) - (fE1dE2 * fE1dE2));

        float fC1 = (fE2dE2 * fE1dP) - (fE1dE2 * fE2dP);

        if ((fC1 < 0.0f) || (fC1 > fDet)) {

            // ray does not intersect triangle
            return -1;
        }

        float fC2 = (fE1dE1 * fE2dP) - (fE1dE2 * fE1dP);

        if ((fC2 < 0.0f) || (fC2 > fDet)) {

            // ray does not intersect triangle
            return -1;
        }

        float fC0 = fDet - fC1 - fC2;

        if ((fC0 < 0.0f) || (fC0 > fDet)) {

            // ray does not intersect triangle
            return -1;
        }

        if (fDet == 0) {
            return -1;
        }

        return ((fC0 * kV0.x) + (fC1 * kV1.x) + (fC2 * kV2.x)) / fDet;
    }

    /**
     * Compute the point of intersection between a line (iX,0,iZ)+t(0,1,0) and
     * the triangle defined by the three input points. All calculations are in
     * voxel coordinates and the y-value of the intersection point is
     * truncated to an integer.
     *
     * @param   kV0  a 3D vertex of the triangle
     * @param   kV1  a 3D vertex of the triangle
     * @param   kV2  a 3D vertex of the triangle
     * @param   iX   the x-value of the origin of the line
     * @param   iZ   the z-value of the origin of the line
     *
     * @return  the y-value of the intersection
     */
    protected float getIntersectY(Point3f kV0, Point3f kV1, Point3f kV2,
                                  float iX, float iZ) {

        // Compute the intersection, if any, by calculating barycentric
        // coordinates of the intersection of the line with the plane of
        // the triangle.  The barycentric coordinates are K0 = fC0/fDet,
        // K1 = fC1/fDet, and K2 = fC2/fDet with K0+K1+K2=1.  The intersection
        // point with the plane is K0*V0+K1*V1+K2*V2.  The point is inside
        // the triangle whenever K0, K1, and K2 are all in the interval [0,1].
        float fPu = iX - kV0.x, fPv = iZ - kV0.z;
        float fE1u = kV1.x - kV0.x, fE1v = kV1.z - kV0.z;
        float fE2u = kV2.x - kV0.x, fE2v = kV2.z - kV0.z;
        float fE1dP = (fE1u * fPu) + (fE1v * fPv);
        float fE2dP = (fE2u * fPu) + (fE2v * fPv);
        float fE1dE1 = (fE1u * fE1u) + (fE1v * fE1v);
        float fE1dE2 = (fE1u * fE2u) + (fE1v * fE2v);
        float fE2dE2 = (fE2u * fE2u) + (fE2v * fE2v);
        float fDet = (float) Math.abs((fE1dE1 * fE2dE2) - (fE1dE2 * fE1dE2));

        float fC1 = (fE2dE2 * fE1dP) - (fE1dE2 * fE2dP);

        if ((fC1 < 0.0f) || (fC1 > fDet)) {

            // ray does not intersect triangle
            return -1;
        }

        float fC2 = (fE1dE1 * fE2dP) - (fE1dE2 * fE1dP);

        if ((fC2 < 0.0f) || (fC2 > fDet)) {

            // ray does not intersect triangle
            return -1;
        }

        float fC0 = fDet - fC1 - fC2;

        if ((fC0 < 0.0f) || (fC0 > fDet)) {

            // ray does not intersect triangle
            return -1;
        }

        if (fDet == 0) {
            return -1;
        }

        return ((fC0 * kV0.y) + (fC1 * kV1.y) + (fC2 * kV2.y)) / fDet;
    }

    /**
     * Compute the point of intersection between a line (iX,iY,0)+t(0,0,1) and
     * the triangle defined by the three input points. All calculations are in
     * voxel coordinates and the z-value of the intersection point is
     * truncated to an integer.
     *
     * @param   kV0  a 3D vertex of the triangle
     * @param   kV1  a 3D vertex of the triangle
     * @param   kV2  a 3D vertex of the triangle
     * @param   iX   the x-value of the origin of the line
     * @param   iY   the y-value of the origin of the line
     *
     * @return  the z-value of the intersection
     */
    protected float getIntersectZ(Point3f kV0, Point3f kV1, Point3f kV2,
                                  float iX, float iY) {

        // Compute the intersection, if any, by calculating barycentric
        // coordinates of the intersection of the line with the plane of
        // the triangle.  The barycentric coordinates are K0 = fC0/fDet,
        // K1 = fC1/fDet, and K2 = fC2/fDet with K0+K1+K2=1.  The intersection
        // point with the plane is K0*V0+K1*V1+K2*V2.  The point is inside
        // the triangle whenever K0, K1, and K2 are all in the interval [0,1].
        float fPu = iX - kV0.x, fPv = iY - kV0.y;
        float fE1u = kV1.x - kV0.x, fE1v = kV1.y - kV0.y;
        float fE2u = kV2.x - kV0.x, fE2v = kV2.y - kV0.y;
        float fE1dP = (fE1u * fPu) + (fE1v * fPv);
        float fE2dP = (fE2u * fPu) + (fE2v * fPv);
        float fE1dE1 = (fE1u * fE1u) + (fE1v * fE1v);
        float fE1dE2 = (fE1u * fE2u) + (fE1v * fE2v);
        float fE2dE2 = (fE2u * fE2u) + (fE2v * fE2v);
        float fDet = (float) Math.abs((fE1dE1 * fE2dE2) - (fE1dE2 * fE1dE2));

        float fC1 = (fE2dE2 * fE1dP) - (fE1dE2 * fE2dP);

        if ((fC1 < 0.0f) || (fC1 > fDet)) {

            // ray does not intersect triangle
            return -1;
        }

        float fC2 = (fE1dE1 * fE2dP) - (fE1dE2 * fE1dP);

        if ((fC2 < 0.0f) || (fC2 > fDet)) {

            // ray does not intersect triangle
            return -1;
        }

        float fC0 = fDet - fC1 - fC2;

        if ((fC0 < 0.0f) || (fC0 > fDet)) {

            // ray does not intersect triangle
            return -1;
        }

        if (fDet == 0) {
            return -1;
        }

        return ((fC0 * kV0.z) + (fC1 * kV1.z) + (fC2 * kV2.z)) / fDet;
    }


}
