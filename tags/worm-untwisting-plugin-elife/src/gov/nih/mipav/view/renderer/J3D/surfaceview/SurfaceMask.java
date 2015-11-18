package gov.nih.mipav.view.renderer.J3D.surfaceview;

import WildMagic.LibFoundation.Mathematics.*;
import java.util.*;
import javax.vecmath.*;
import javax.media.j3d.*;

import gov.nih.mipav.model.structures.*;
import gov.nih.mipav.view.renderer.J3D.model.structures.*;

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

    /** ModelLUT for getModelImageColor w/LUT changes: */
    private ModelLUT m_kLUT = null;

    /** ModelRGB for getModelImageColor w/RGB LUT changes:  */
    private ModelRGB m_kRGBT = null;

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
     * @param bCreateVertexColor create the per-vertex color from the imageA ModleImage,
     * @param bUseImageMask create the per-vertex color from the imageA ModleImage, with the ModelImage mask
     * @param fOpacity Mask opacity
     * @param surColor default Mask color
     */
    public void maskInsideVoxels(int index, ModelImage imageA,
                                 ModelTriangleMesh[] kMesh,
                                 boolean bHasVertexColor,
                                 boolean bCreateVertexColors,
                                 boolean bUseImageMask,
                                 float fOpacity, ColorRGBA surColor )
    {

        float iX, iY, iZ;

        int[] extents = imageA.getExtents();
        xDim = extents[0];
        yDim = extents[1];
        zDim = extents[2];

        m_iQuantity = xDim * yDim * zDim;

        BitSet kImageMask = null;
        ColorRGBA kImageMaskColor = null;
        if ( bUseImageMask )
        {
            kImageMask = imageA.getSurfaceMask( index );
            kImageMaskColor = imageA.getSurfaceMaskColor( index );
        }
        int[] iterFactors = imageA.getVolumeIterationFactors( );

        BitSet kMask = new BitSet(m_iQuantity);
        ColorRGBA[] akMaskColor = new ColorRGBA[m_iQuantity];

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


                Color4f kTriColor = null;
                if ( bCreateVertexColors )
                {
                    kTriColors[aiConnect[3 * iT]] = getModelImageColor( imageA, 1 - fOpacity, kV0 );
                    kTriColors[aiConnect[ (3 * iT) + 1]] = getModelImageColor( imageA, 1 - fOpacity, kV1 );
                    kTriColors[aiConnect[ (3 * iT) + 2]] = getModelImageColor( imageA, 1 - fOpacity, kV2 );

                    if ( bUseImageMask && (kImageMask != null) && (kImageMaskColor != null) )
                    {
                        int maskIndex =  (int)((iterFactors[0] * kV0.x) +
                                               (iterFactors[1] * kV0.y) +
                                               (iterFactors[2] * kV0.z)   );
                        if ( kImageMask.get( maskIndex ) )
                        {
                            kTriColors[aiConnect[3 * iT]] = new Color4f( kImageMaskColor.R,
                                                                         kImageMaskColor.G,
                                                                         kImageMaskColor.B,
                                                                         kImageMaskColor.A );
                        }
                        maskIndex =  (int)((iterFactors[0] * kV1.x) +
                                       (iterFactors[1] * kV1.y) +
                                       (iterFactors[2] * kV1.z)   );
                        if ( kImageMask.get( maskIndex ) )
                        {
                            kTriColors[aiConnect[(3 * iT) + 1]] = new Color4f( kImageMaskColor.R,
                                    kImageMaskColor.G,
                                    kImageMaskColor.B,
                                    kImageMaskColor.A );
                        }
                        maskIndex =  (int)((iterFactors[0] * kV2.x) +
                                       (iterFactors[1] * kV2.y) +
                                       (iterFactors[2] * kV2.z)   );
                        if ( kImageMask.get( maskIndex ) )
                        {
                            kTriColors[aiConnect[(3 * iT) + 2]] = new Color4f( kImageMaskColor.R,
                                    kImageMaskColor.G,
                                    kImageMaskColor.B,
                                    kImageMaskColor.A );
                        }
                    }
                    
                    if (kMesh[mIndex].getCapability(GeometryArray.ALLOW_COLOR_WRITE) )
                    {
                        // Sets the color, but does not update the geometry yet (faster):
                        kMesh[mIndex].setColorDelay( aiConnect[ (3 * iT) + 0], kTriColors[aiConnect[ (3 * iT) + 0]] );
                        kMesh[mIndex].setColorDelay( aiConnect[ (3 * iT) + 1], kTriColors[aiConnect[ (3 * iT) + 1]] );
                        kMesh[mIndex].setColorDelay( aiConnect[ (3 * iT) + 2], kTriColors[aiConnect[ (3 * iT) + 2]] );
                    }
                }
                
                if ( !bUseImageMask )
                {
                    Color4f kC0, kC1, kC2;
                    kC0 = kTriColors[aiConnect[3 * iT]];
                    kC1 = kTriColors[aiConnect[ (3 * iT) + 1]];
                    kC2 = kTriColors[aiConnect[ (3 * iT) + 2]];
                    
                    if ( (kC0 != null) && (kC1 != null) && (kC2 != null)) {
                        kTriColor = new Color4f( (kC0.x + kC1.x + kC2.x) / 3.0f,
                                                 (kC0.y + kC1.y + kC2.y) / 3.0f,
                                                 (kC0.z + kC1.z + kC2.z) / 3.0f,
                                                 1 - fOpacity );
                    }
                    
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
                                    if ( kTriColor != null )
                                    {
                                    	akMaskColor[ptr] = new ColorRGBA(kTriColor.x, 
                                    			kTriColor.y, kTriColor.z, kTriColor.w );
                                    }
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
                                    if ( kTriColor != null )
                                    {
                                    	akMaskColor[ptr] = new ColorRGBA(kTriColor.x, 
                                    			kTriColor.y, kTriColor.z, kTriColor.w );
                                    }
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
                                    if ( kTriColor != null )
                                    {
                                    	akMaskColor[ptr] = new ColorRGBA(kTriColor.x, 
                                    			kTriColor.y, kTriColor.z, kTriColor.w );
                                    }
                                    mVolumeMask.set(ptr);
                                }
                            }
                        }
                    }
                }
            }
            if ( !bUseImageMask )
            {
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
            }
            kMesh[mIndex].setTextureCoordinates( 0, 0, akTexCoords );
            kMesh[mIndex].setTextureCoordinateIndices( 0, 0, aiConnect );           
            if ( bCreateVertexColors && kMesh[mIndex].getCapability(GeometryArray.ALLOW_COLOR_WRITE) )
            {
                // updates the geometry all at once (faster):
                kMesh[mIndex].setColorUpdate();
            }
        }

        if ( !bUseImageMask )
        {
            int v = 0;
            for (int i = 0; i < mVolumeMask.size(); i++) {
                if (mVolumeMask.get(i)) {
                    v++;
                }
            }
            System.err.println("voxels = " + v);
            System.err.println("volume = " + (v * resols[0] * resols[1] * resols[2]));

            ColorRGBA color = new ColorRGBA( surColor.R, surColor.G, surColor.B, 1 - fOpacity );
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

    public void setRGBT( ModelRGB kRGBT )
    {
        m_kRGBT = kRGBT;
    }

    public void setLUT( ModelLUT kLUT )
    {
        m_kLUT = kLUT;
    }

    /** Get the color from the RGB lookup table:
     * @param RGBIndexBuffer the color lookup table index buffer
     * @param kOffset the color offset
     * @param fNorm the color normalization
     * @param currentColor the current color (changed)
     */
    private void getColorMapped( int[] RGBIndexBuffer, Color3f kOffset, float fNorm,
                                 Color4f currentColor )
    {
        if ( m_kRGBT.getROn() )
        {
            currentColor.x =
                (RGBIndexBuffer[(int)((currentColor.x + kOffset.x) * fNorm)] & 0x00ff0000) >> 16;
        }
        if ( m_kRGBT.getGOn() )
        {
            currentColor.y =
                (RGBIndexBuffer[(int)((currentColor.y + kOffset.y) * fNorm)] &  0x0000ff00) >> 8;
        }
        if ( m_kRGBT.getBOn() )
        {
            currentColor.z =
                (RGBIndexBuffer[(int)((currentColor.z + kOffset.z) * fNorm)] & 0x000000ff);
        }
    }


    /**
     * calculates the color normalization factors
     * @param kImage the model image from which the normalization factors are
     * calculated
     * @param kOffset the calculated color offset
     * @return the color normalization factor.
     */
    private float calcMaxNormColors( ModelImage kImage, Color3f kOffset )
    {
        float fMaxColor = 255;
        if (kImage.getType() == ModelStorageBase.ARGB_USHORT) {
            fMaxColor = (float) kImage.getMaxR();
            fMaxColor = Math.max((float) kImage.getMaxG(), fMaxColor);
            fMaxColor = Math.max((float) kImage.getMaxB(), fMaxColor);
        } else if (kImage.getType() == ModelStorageBase.ARGB_FLOAT) {

            if (kImage.getMinR() < 0.0) {
                fMaxColor = (float) (kImage.getMaxR() - kImage.getMinR());
                kOffset.x = (float) (-kImage.getMinR());
            } else {
                fMaxColor = (float) kImage.getMaxR();
            }

            if (kImage.getMinG() < 0.0) {
                fMaxColor = Math.max((float)(kImage.getMaxG() - kImage.getMinG()),
                                     fMaxColor);
                kOffset.y= (float) (-kImage.getMinG());
            } else {
                fMaxColor = Math.max((float) kImage.getMaxG(), fMaxColor);
            }

            if (kImage.getMinB() < 0.0) {
                fMaxColor = Math.max((float)(kImage.getMaxB() - kImage.getMinB()),
                                     fMaxColor);
                kOffset.z = (float) (-kImage.getMinB());
            } else {
                fMaxColor = Math.max((float) kImage.getMaxB(), fMaxColor);
            }
        }

        return 255 / fMaxColor;
    }


    /**
     * Given a triangle vertex point from a ModelTriangleMesh, determines the
     * corresponding ModelImage index point, and recreates the color that
     * would have originally been assigned to the vertex.
     * @param kTrianglePoint ModelTriangleMesh point in ModelImage coordinates
     * @return original color of the triangle point.
     */
    public Color4f getModelImageColor( ModelImage imageA, float fTransparency, Point3f kModelPoint )
    {
        Color4f kColor = new Color4f();
        if ( !imageA.isColorImage() )
        {
            float value = imageA.getFloatTriLinearBounds( kModelPoint.x,
                                                          kModelPoint.y,
                                                          kModelPoint.z );

            value /= 255.0f;
            kColor.x = value;
            kColor.y = value;
            kColor.z = value;
            kColor.w = fTransparency;

            if ( m_kLUT != null )
            {
                float[][] RGB_LUT = m_kLUT.exportRGB_LUT(true);
                TransferFunction tf_imgA = m_kLUT.getTransferFunction();
                int index = (int)(tf_imgA.getRemappedValue(value * 255.0f, 256) + 0.5f);
                kColor.x = RGB_LUT[0][index]/255.0f;
                kColor.y = RGB_LUT[1][index]/255.0f;
                kColor.z = RGB_LUT[2][index]/255.0f;
            }
        }
        else
        {
                                        
            kColor.w = fTransparency;

            kColor.x = imageA.getFloatTriLinearBounds( kModelPoint.x,
                                                       kModelPoint.y,
                                                       kModelPoint.z, 1 );
            kColor.y = imageA.getFloatTriLinearBounds( kModelPoint.x,
                                                       kModelPoint.y,
                                                       kModelPoint.z, 2 );
            kColor.z = imageA.getFloatTriLinearBounds( kModelPoint.x,
                                                       kModelPoint.y,
                                                       kModelPoint.z, 3 );

            if ( m_kRGBT != null )
            {
                int[] aiRGBIndexBuffer = m_kRGBT.exportIndexedRGB();
                Color3f kOffset = new Color3f();
                float fNorm = calcMaxNormColors( imageA, kOffset );
                getColorMapped( aiRGBIndexBuffer, kOffset, fNorm, kColor );
            }
            kColor.x /= 255.0f;
            kColor.y /= 255.0f;
            kColor.z /= 255.0f;

        }
        return kColor;
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
    private void floodFill(int iX, int iY, int iZ) {

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
    private final int getIndex(int iX, int iY, int iZ) {
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
    private float getIntersectX(Point3f kV0, Point3f kV1, Point3f kV2,
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
    private float getIntersectY(Point3f kV0, Point3f kV1, Point3f kV2,
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
    private float getIntersectZ(Point3f kV0, Point3f kV1, Point3f kV2,
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
