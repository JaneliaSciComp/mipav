package gov.nih.mipav.model.algorithms.asc;

import java.util.*;

/**
 * A class for extracting multiresolution level surfaces from 3D images.  The
 * algorithm is partially based on the paper:
 * <pre>
 *   Multiresolution Isosurface Extraction with Adaptive Skeleton Climbing<br>
 *   Tim Poston, Tien-Tsin Wong, and Pheng-Ann Heng<br>
 *   Eurographics '98<br>, vol. 17, no. 3
 *   N. Ferreira and M. Gobel, editors
 *   13 pages, 1998.
 * </pre>
 *
 *  @author  David H. Eberly, Ph.D.
 * The algorithm used is an improvement over the one described in this paper.
 * The merging is done with a simple divide-and-conquer scheme by representing
 * the image as an octree.  The isosurface extraction in this code does not
 * suffer the ambiguitiy problems of the one in the paper.  See the document
 * AdaptiveSkeletonClimbing.pdf for a detailed description of the algorithms
 * as implemented in this class.
 */

public class ASC_Climb3D
{

    // Indices for the vertex array in MjVertexTable.  EI = edge index,
    // FI = face index.
    private static final int EI_XMIN_YMIN =  0;
    private static final int EI_XMIN_YMAX =  1;
    private static final int EI_XMAX_YMIN =  2;
    private static final int EI_XMAX_YMAX =  3;
    private static final int EI_XMIN_ZMIN =  4;
    private static final int EI_XMIN_ZMAX =  5;
    private static final int EI_XMAX_ZMIN =  6;
    private static final int EI_XMAX_ZMAX =  7;
    private static final int EI_YMIN_ZMIN =  8;
    private static final int EI_YMIN_ZMAX =  9;
    private static final int EI_YMAX_ZMIN = 10;
    private static final int EI_YMAX_ZMAX = 11;
    private static final int FI_XMIN      = 12;
    private static final int FI_XMAX      = 13;
    private static final int FI_YMIN      = 14;
    private static final int FI_YMAX      = 15;
    private static final int FI_ZMIN      = 16;
    private static final int FI_ZMAX      = 17;

    // Bit-flags for use by getVertices and the functions that connect
    // the vertices by edges.  EB = edge bit, FB = face bit.
    private static final int EB_XMIN_YMIN = 1 << EI_XMIN_YMIN;
    private static final int EB_XMIN_YMAX = 1 << EI_XMIN_YMAX;
    private static final int EB_XMAX_YMIN = 1 << EI_XMAX_YMIN;
    private static final int EB_XMAX_YMAX = 1 << EI_XMAX_YMAX;
    private static final int EB_XMIN_ZMIN = 1 << EI_XMIN_ZMIN;
    private static final int EB_XMIN_ZMAX = 1 << EI_XMIN_ZMAX;
    private static final int EB_XMAX_ZMIN = 1 << EI_XMAX_ZMIN;
    private static final int EB_XMAX_ZMAX = 1 << EI_XMAX_ZMAX;
    private static final int EB_YMIN_ZMIN = 1 << EI_YMIN_ZMIN;
    private static final int EB_YMIN_ZMAX = 1 << EI_YMIN_ZMAX;
    private static final int EB_YMAX_ZMIN = 1 << EI_YMAX_ZMIN;
    private static final int EB_YMAX_ZMAX = 1 << EI_YMAX_ZMAX;
    private static final int FB_XMIN      = 1 << FI_XMIN;
    private static final int FB_XMAX      = 1 << FI_XMAX;
    private static final int FB_YMIN      = 1 << FI_YMIN;
    private static final int FB_YMAX      = 1 << FI_YMAX;
    private static final int FB_ZMIN      = 1 << FI_ZMIN;
    private static final int FB_ZMAX      = 1 << FI_ZMAX;

    // image data
    private int m_iN, m_iTwoPowerN, m_iSize, m_iSizeSqr;
    private float[] m_afData;
    private float m_fLevel;

    // linear merging (binary trees for lines of the image)
    private ASC_LinearMergeTree[][] m_aakXMerge;
    private ASC_LinearMergeTree[][] m_aakYMerge;
    private ASC_LinearMergeTree[][] m_aakZMerge;

    // monoregions of the image
    private Vector m_kBoxes;  // Vector of MjOctBox

    // If set to 'true', image boundary voxels are not allowed to merge with
    // any other voxels.  This forces highest level of detail on the boundary.
    // The idea is that an image too large to process by itself can be
    // partitioned into smaller subimages and the adaptive skeleton climbing
    // applied to each subimage.  By forcing highest resolution on the
    // boundary, adjacent subimages will not have any cracking problems.
    private boolean m_bFixBoundary;

    // output of getPoint and getGradient
    private float m_fPointX, m_fPointY, m_fPointZ;

    // output of extractContour
    //public ASC_Vertex3[]   m_akVertex;
    //public ASC_Triangle3[] m_akTriangle;
    public Vector m_kVArray;  // Vector of MjVertex3
    public Vector m_kTArray;  // Vector of MjTriangle3


     /** The level value for the desired isosurface.  This value
     *   should not be an image value.  The restriction allows for a large
     *   amount of algorithmic simplification.  A good choice for fLevel is a
     *   non--integral value, perhaps a value of the form v.5 where v is an
     *   integer.
     */
    private float   fLevel;

     /** This parameter controls the resolution of the extracted
     *   isosurface.  If its value is -1, the coarsest resolution surface is
     *   constructed.  If its value is n where the image size is
     *   m-by-m-by-m with m = power(2,n)+1, the finest resolution surface is
     *   constructed.  This surface corresponds to an extraction at the voxel
     *   level.  Any value of iDepth between -1 and n may be used.
     *   The smaller the value, the coarser the resolution the extracted
     *   surface is.
     */
    private int     iDepth;

    /** For the fastest extraction, set this flag to 'false'.
     *   The vertices and triangles are extracted for each monoregion without
     *   worrying about the duplication of vertices that occurs on edges
     *   shared by voxels.  To eliminate duplicate vertices, set this flag
     *   to 'true'.
     */
    private boolean bUnique;  // should be removed

     /** For the fastest extraction, set this flag to
     *   zero.  The triangles are extracted for each monoregion without
     *   worrying about the orientation of the triangles.  In this mode, the
     *   display of triangles should have back-face culling disabled.  If
     *   you need the triangles to be consistently ordered, you have a choice
     *   of their normals being in the direction of the image gradients at
     *   their centroids (set flag to +1) or in the opposite direction of
     *   the image gradients (set flag to -1).
     */
    private int     iOrientTriangles;

    /**
     * Create a multiresolution extractor for isosurfaces that is based on
     * adaptive skeleton climbing.
     *
     * @param iN The input data must contain M-by-M-by-M data values where
     *   M = power(2,iN)+1.  Also, 0 <= iN <= 7.  The upper bound is
     *   necessary to guarantee that the memory requirements for the octree
     *   are within reason.  The input data organization is lexicographic
     *   order for (x,y,z).
     * @param afData the image array of the size specified by iN
     *   If the image must be partitioned into smaller subimages, use
     *   ASC_Climb3DDecompose.  That class will set bFixBoundary to 'true'.
     */
	public ASC_Climb3D (int iN, float afData[]) {

        m_afData = afData;
	    m_iN = iN;
	    initialize();
	}

    //public AlgorithmClimb3D (int iN, int[] aiData)
    private void initialize () {

        //m_aiData = aiData;
        m_iTwoPowerN = (1 << m_iN);
        m_iSize      = m_iTwoPowerN + 1;
        m_iSizeSqr   = m_iSize * m_iSize;

        // The binary trees that represent linear merging.  These are used
        // to help decide if monoregions in the octree can be merged into
        // larger monoregions.
        m_aakXMerge = new ASC_LinearMergeTree[m_iSize][];
        m_aakYMerge = new ASC_LinearMergeTree[m_iSize][];
        m_aakZMerge = new ASC_LinearMergeTree[m_iSize][];
        for (int i = 0; i < m_iSize; i++)
        {
            m_aakXMerge[i] = new ASC_LinearMergeTree[m_iSize];
            m_aakYMerge[i] = new ASC_LinearMergeTree[m_iSize];
            m_aakZMerge[i] = new ASC_LinearMergeTree[m_iSize];
            for (int j = 0; j < m_iSize; j++)
            {
                m_aakXMerge[i][j] = new ASC_LinearMergeTree(m_iN);
                m_aakYMerge[i][j] = new ASC_LinearMergeTree(m_iN);
                m_aakZMerge[i][j] = new ASC_LinearMergeTree(m_iN);
            }
        }

        // Volume merging.  This is the octree that is used to merge regions
        // based on a divide-and-conquer approach.
        // old m_kXYZMerge = new ASC_VolumeMergeTree(m_iN, m_aakXMerge, m_aakYMerge, m_aakZMerge);
        m_bFixBoundary = false;
        m_kBoxes = new Vector();
    }

    /**
     * Get the image data object.  This is used by ASC_Climb3DDecompose so
     * that it needs only one ASC_Climb3D object to compute level surfaces in
     * a piecewise manner.
     *
     * @return the image data object
     */
    public float[] getData() { return m_afData; }

    public void setData(float afData[] ) { m_afData = afData; }

    /**
     * Specify whether or not image boundary voxels are allowed to merge.
     * The only client of this call is ASC_Climb3DDecompose.  Boundary voxels
     * are not allowed to merge to prevent cracking on shared subimage
     * boundaries.
     *
     * @param bFixBoundary set to 'true' if the boundary voxels are not
     *    allowed to merge, ''false' if they are allowed to merge
     */
    public void setFixedBoundary (boolean bFixBoundary)
    {
        m_bFixBoundary = bFixBoundary;
    }

    /**
     *   Sets the level value for the desired isosurface.  This value
     *   should not be an image value.  The restriction allows for a large
     *   amount of algorithmic simplification.  A good choice for fLevel is a
     *   non--integral value, perhaps a value of the form v.5 where v is an
     *   integer.
     */
    public void setFLevel (float fLevel) { this.fLevel = fLevel; }

    /** This parameter controls the resolution of the extracted
     *   isosurface.  If its value is -1, the coarsest resolution surface is
     *   constructed.  If its value is n where the image size is
     *   m-by-m-by-m with m = power(2,n)+1, the finest resolution surface is
     *   constructed.  This surface corresponds to an extraction at the voxel
     *   level.  Any value of iDepth between -1 and n may be used.
     *   The smaller the value, the coarser the resolution the extracted
     *   surface is.
     */
    public void setIDepth (int _iDepth) { this.iDepth = _iDepth; }

    /** For the fastest extraction, set this flag to 'false'.
     *   The vertices and triangles are extracted for each monoregion without
     *   worrying about the duplication of vertices that occurs on edges
     *   shared by voxels.  To eliminate duplicate vertices, set this flag
     *   to 'true'.
     */
     public void setBUnique (boolean bUnique) { this.bUnique = bUnique; }


     /** For the fastest extracction, set this flag to
     *   zero.  The triangles are extracted for each monoregion without
     *   worrying about the orientation of the triangles.  In this mode, the
     *   display of triangles should have back-face culling disabled.  If
     *   you need the triangles to be consistently ordered, you have a choice
     *   of their normals being in the direction of the image gradients at
     *   their centroids (set flag to +1) or in the opposite direction of
     *   the image gradients (set flag to -1).
     */
    public void setIOrientTriangles(int iOrientTriangles) { this.iOrientTriangles = iOrientTriangles; }


    public void extractContour (float fLevel, int _iDepth, int iOrientTriangles)
    {
        m_kVArray = null;  // Vector of ASC_Vertex3
        m_kTArray = null;  // Vector of ASC_Triangle3
        this.iDepth = _iDepth;
        int i;

        try {
            // Storage for the boxes, vertices, and triangles during the
            // extraction process.
            m_kVArray = new Vector();  // Vector of ASC_Vertex3
            m_kTArray = new Vector();  // Vector of ASC_Triangle3
            m_kBoxes.clear();
        }
        catch (OutOfMemoryError e){
            System.gc();
            return;
        }


        // Recursively traverse the octree and compute the monoregions.
        m_fLevel = fLevel;
        System.out.println("extractContour1 Level = " + iDepth);
        merge(iDepth);
        System.out.println("extractContour2 Level = " + iDepth);

        // Build the triangle mesh from the monoregions.
        tessellate(m_kVArray, m_kTArray);

        if ( m_kVArray.size() == 0 || m_kTArray.size() == 0 )
        {
            // No contours of this level.  Application must test m_akVertex
            // and m_akTriangle for null before using.
            return;
        }

        // Remove duplicate vertices and remap the triangle connectivity
        // accordingly.
        makeUnique(m_kVArray, m_kTArray);

        // orient the triangles, if requested
        if ( iOrientTriangles > 0 )
            orientTriangles(true);
        else if ( iOrientTriangles < 0 )
            orientTriangles(false);

        //m_kVArray = null;
        //m_kTArray = null;

        //System.gc();
        return;
    }

    /**
     * A normal vector at a vertex is computed by averaging the triangle
     * normals for all triangles sharing the vertex.  This routine computes
     * all vertex normals.
     *
     * @param akVertex  the array of vertices for the mesh
     * @param kTArray   the array of triangles for the mesh
     * @return the array of vertex normals
     */
    static public ASC_Vertex3[] computeNormals (ASC_Vertex3[] akVertex,
        Vector kTArray)
    {
        // maintain a running sum of triangle normals at each vertex
        ASC_Vertex3[] akNormal = new ASC_Vertex3[akVertex.length];
        int i, j;
        for (i = 0; i < akVertex.length; i++)
            akNormal[i] = new ASC_Vertex3(0.0f,0.0f,0.0f);

        for (i = 0, j = 0; i < kTArray.size(); i++)
        {
            ASC_Triangle3 kT = (ASC_Triangle3)kTArray.get(i);
            ASC_Vertex3 kV0 = akVertex[kT.m_i0];
            ASC_Vertex3 kV1 = akVertex[kT.m_i1];
            ASC_Vertex3 kV2 = akVertex[kT.m_i2];

            // construct triangle normal
            float fEX1 = kV1.m_fX - kV0.m_fX;
            float fEY1 = kV1.m_fY - kV0.m_fY;
            float fEZ1 = kV1.m_fZ - kV0.m_fZ;
            float fEX2 = kV2.m_fX - kV0.m_fX;
            float fEY2 = kV2.m_fY - kV0.m_fY;
            float fEZ2 = kV2.m_fZ - kV0.m_fZ;
            float fNX = fEY1*fEZ2 - fEY2*fEZ1;
            float fNY = fEZ1*fEX2 - fEZ2*fEX1;
            float fNZ = fEX1*fEY2 - fEX2*fEY1;

            // maintain the sum of normals at each vertex
            akNormal[kT.m_i0].m_fX += fNX;
            akNormal[kT.m_i0].m_fY += fNY;
            akNormal[kT.m_i0].m_fZ += fNZ;
            akNormal[kT.m_i1].m_fX += fNX;
            akNormal[kT.m_i1].m_fY += fNY;
            akNormal[kT.m_i1].m_fZ += fNZ;
            akNormal[kT.m_i2].m_fX += fNX;
            akNormal[kT.m_i2].m_fY += fNY;
            akNormal[kT.m_i2].m_fZ += fNZ;
        }

        // The normal vector storage was used to accumulate the sum of
        // triangle normals.  Now these vectors must be rescaled to be
        // unit length.
        for (i = 0; i < akNormal.length; i++)
        {
            ASC_Vertex3 kV = akNormal[i];
            float fLength = (float)Math.sqrt(kV.m_fX*kV.m_fX +
                kV.m_fY*kV.m_fY + kV.m_fZ*kV.m_fZ);
            if ( fLength > 1e-08f )
            {
                float fInvLength = 1.0f/fLength;
                kV.m_fX *= fInvLength;
                kV.m_fY *= fInvLength;
                kV.m_fZ *= fInvLength;
            }
            else
            {
                kV.m_fX = 0.0f;
                kV.m_fY = 0.0f;
                kV.m_fZ = 0.0f;
            }
        }

        return akNormal;
    }


    /**
     * Interpolation of the image values.
     *
     * @param fX  the x component of point to be interpolated
     * @param fY  the y component of point to be interpolated
     * @param fZ  the z component of point to be interpolated
     * @return the interpolated function value
     */
    public float getFunction (float fX, float fY, float fZ)
    {
        int iX = (int) fX;
        if ( iX < 0 || iX >= m_iTwoPowerN )
            return 0.0f;

        int iY = (int) fY;
        if ( iY < 0 || iY >= m_iTwoPowerN )
            return 0.0f;

        int iZ = (int) fZ;
        if ( iZ < 0 || iZ >= m_iTwoPowerN )
            return 0.0f;

        int i000 = iX + m_iSize*(iY + m_iSize*iZ);
        int i100 = i000 + 1;
        int i010 = i000 + m_iSize;
        int i110 = i100 + m_iSize;
        int i001 = i000 + m_iSizeSqr;
        int i101 = i100 + m_iSizeSqr;
        int i011 = i010 + m_iSizeSqr;
        int i111 = i110 + m_iSizeSqr;
        float fF000 = m_afData[i000];
        float fF100 = m_afData[i100];
        float fF010 = m_afData[i010];
        float fF110 = m_afData[i110];
        float fF001 = m_afData[i001];
        float fF101 = m_afData[i101];
        float fF011 = m_afData[i011];
        float fF111 = m_afData[i111];

        fX -= iX;
        fY -= iY;
        fZ -= iZ;
        float fOmX = 1.0f - fX, fOmY = 1.0f - fY, fOmZ = 1.0f - fZ;
        float fTmp0 = fOmY*(fOmX*fF000+fX*fF100)+fY*(fOmX*fF010+fX*fF110);
        float fTmp1 = fOmY*(fOmX*fF001+fX*fF101)+fY*(fOmX*fF011+fX*fF111);
        float fResult = fOmZ*fTmp0+fZ*fTmp1;
        return fResult;
    }

    /**
     * Interpolation of the image gradient values.  The resulting gradient
     * is stored in m_fPointX, m_fPointy, m_fPointZ.
     *
     * @param fX the x component of the point to be interpolated
     * @param fY the y component of the point to be interpolated
     * @param fZ the z component of the point to be interpolated
     */
    public void getGradient (float fX, float fY, float fZ)
    {
        int iX = (int) fX;
        if ( iX < 0 || iX >= m_iTwoPowerN )
        {
            m_fPointX = 0.0f;
            m_fPointY = 0.0f;
            m_fPointZ = 0.0f;
            return;
        }

        int iY = (int) fY;
        if ( iY < 0 || iY >= m_iTwoPowerN )
        {
            m_fPointX = 0.0f;
            m_fPointY = 0.0f;
            m_fPointZ = 0.0f;
            return;
        }

        int iZ = (int) fZ;
        if ( iZ < 0 || iZ >= m_iTwoPowerN )
        {
            m_fPointX = 0.0f;
            m_fPointY = 0.0f;
            m_fPointZ = 0.0f;
            return;
        }

        int i000 = iX + m_iSize*(iY + m_iSize*iZ);
        int i100 = i000 + 1;
        int i010 = i000 + m_iSize;
        int i110 = i100 + m_iSize;
        int i001 = i000 + m_iSizeSqr;
        int i101 = i100 + m_iSizeSqr;
        int i011 = i010 + m_iSizeSqr;
        int i111 = i110 + m_iSizeSqr;
        float fF000 = m_afData[i000];
        float fF100 = m_afData[i100];
        float fF010 = m_afData[i010];
        float fF110 = m_afData[i110];
        float fF001 = m_afData[i001];
        float fF101 = m_afData[i101];
        float fF011 = m_afData[i011];
        float fF111 = m_afData[i111];

        fX -= iX;
        fY -= iY;
        fZ -= iZ;
        float fOmX = 1.0f - fX, fOmY = 1.0f - fY, fOmZ = 1.0f - fZ;
        float fTmp0, fTmp1;

        fTmp0 = fOmY*(fF100-fF000) + fY*(fF110-fF010);
        fTmp1 = fOmY*(fF101-fF001) + fY*(fF111-fF011);
        m_fPointX = fOmZ*fTmp0 + fZ*fTmp1;

        fTmp0 = fOmX*(fF010-fF000) + fX*(fF110-fF100);
        fTmp1 = fOmX*(fF011-fF001) + fX*(fF111-fF101);
        m_fPointY = fOmZ*fTmp0 + fZ*fTmp1;

        fTmp0 = fOmX*(fF001-fF000) + fX*(fF101-fF100);
        fTmp1 = fOmX*(fF011-fF010) + fX*(fF111-fF110);
        m_fPointZ = fOmY*fTmp0 + fY*fTmp1;
    }

    /**
     * Orient the triangle vertices so the triangle normal is in the
     * direction of the image gradient at its centroid or in the opposite
     * direction depending on the value of bSameDirection.
     *
     * @param kTri the triangle to orient
     * @param bSameDirection 'true' if the normal should be in the direction
     *    of the gradient, 'false' if the normal should be in the opposite
     *    direction of the gradient
     */
    public void orientTriangle (ASC_Triangle3 kTri, boolean bSameDirection)
    {
        // get triangle vertices
        ASC_Vertex3 kV0 = (ASC_Vertex3)m_kVArray.get(kTri.m_i0);
        ASC_Vertex3 kV1 = (ASC_Vertex3)m_kVArray.get(kTri.m_i1);
        ASC_Vertex3 kV2 = (ASC_Vertex3)m_kVArray.get(kTri.m_i2);

        // construct triangle normal based on current orientation
        float fEX1 = kV1.m_fX - kV0.m_fX;
        float fEY1 = kV1.m_fY - kV0.m_fY;
        float fEZ1 = kV1.m_fZ - kV0.m_fZ;
        float fEX2 = kV2.m_fX - kV0.m_fX;
        float fEY2 = kV2.m_fY - kV0.m_fY;
        float fEZ2 = kV2.m_fZ - kV0.m_fZ;
        float fNX  = fEY1*fEZ2 - fEY2*fEZ1;
        float fNY  = fEZ1*fEX2 - fEZ2*fEX1;
        float fNZ  = fEX1*fEY2 - fEX2*fEY1;

        // compute the triangle centroid
        float fOneThird = 1.0f/3.0f;
        float fCX = (kV0.m_fX + kV1.m_fX + kV2.m_fX)*fOneThird;
        float fCY = (kV0.m_fY + kV1.m_fY + kV2.m_fY)*fOneThird;
        float fCZ = (kV0.m_fZ + kV1.m_fZ + kV2.m_fZ)*fOneThird;

        // get the image gradient at the centroid
        getGradient(fCX,fCY,fCZ);

        // compute the dot product of normal and gradient
        float fDot = m_fPointX*fNX + m_fPointY*fNY + m_fPointZ*fNZ;

        // choose triangle orientation based on gradient direction
        int iSave;
        if ( bSameDirection )
        {
            if ( fDot < 0.0f )
            {
                // wrong orientation, reorder it
                iSave = kTri.m_i1;
                kTri.m_i1 = kTri.m_i2;
                kTri.m_i2 = iSave;
            }
        }
        else
        {
            if ( fDot > 0.0f )
            {
                // wrong orientation, reorder it
                iSave = kTri.m_i1;
                kTri.m_i1 = kTri.m_i2;
                kTri.m_i2 = iSave;
            }
        }
    }

    /**
     * Orient all the triangles so that the triangle normals are in the
     * direction of the image gradient at its centroid or in the opposite
     * direction depending on the value of bSameDirection.
     *
     * @param bSameDirection 'true' if the normal should be in the direction
     *    of the gradient, 'false' if the normal should be in the opposite
     *    direction of the gradient
     */
    public void orientTriangles (boolean bSameDirection)
    {
        for (int i = 0; i < m_kTArray.size(); i++)
            orientTriangle((ASC_Triangle3)m_kTArray.get(i), bSameDirection);
    }

    /**
     * Generate a unique set of vertices and triangles from the input
     * vertices and triangles.  The output vertices and output triangles are
     * stored in kVArray and kTArray.
     *
     * @param kVArray The input set of vertices, potentially with duplicates.
     *    On output, the set consists of unique vertices.
     * @param kTArray The input set of triangles, potentially with
     *    duplicates.  On output, the set consists of unique triangles.
     */
    public static void makeUnique (Vector kVArray, Vector kTArray)
    {
        // use a hash table to generate unique storage
        HashMap kVMap = new HashMap();  // map of <ASC_Vertex3,Integer>
        int iNextVertex = 0;
        Integer kNextVertex = new Integer(iNextVertex);
        for (int iV = 0; iV < kVArray.size(); iV++)
        {
            Object kKey = kVArray.get(iV);
            if ( !kVMap.containsKey(kKey) )
            {
                kVMap.put(kKey,kNextVertex);
                iNextVertex++;
                kNextVertex = new Integer(iNextVertex);
            }
        }

        // use a hash table to generate unique storage
        HashMap kTMap = null;  // map of <ASC_Triangle3,Integer>
        int iT;
        if ( kTArray.size() > 0 )
        {
            kTMap = new HashMap();
            int iNextTriangle = 0;
            Integer kNextTriangle = new Integer(iNextTriangle);
            for (iT = 0; iT < kTArray.size(); iT++)
            {
                ASC_Triangle3 kT = (ASC_Triangle3)kTArray.get(iT);

                // replace old vertex indices by new ones
                Integer kV = (Integer)kVMap.get(kVArray.get(kT.m_i0));
                kT.m_i0 = kV.intValue();
                kV = (Integer)kVMap.get(kVArray.get(kT.m_i1));
                kT.m_i1 = kV.intValue();
                kV = (Integer)kVMap.get(kVArray.get(kT.m_i2));
                kT.m_i2 = kV.intValue();

                // keep only unique triangles
                if ( !kTMap.containsKey(kT) )
                {
                    kTMap.put(kT,kNextTriangle);
                    iNextTriangle++;
                    kNextTriangle = new Integer(iNextTriangle);
                }
            }
        }

        // pack vertices into an array
        kVArray.setSize(kVMap.size());
        Iterator kVIter = kVMap.entrySet().iterator();
        while ( kVIter.hasNext() )
        {
            Map.Entry kEntry = (Map.Entry) kVIter.next();
            Integer kV = (Integer)kEntry.getValue();
            kVArray.set(kV.intValue(),kEntry.getKey());
        }

        // pack triangles into an array
        kTArray.setSize(kTMap.size());
        Iterator kTIter = kTMap.entrySet().iterator();
        while ( kTIter.hasNext() )
        {
            Map.Entry kEntry = (Map.Entry) kTIter.next();
            Integer kT = (Integer)kEntry.getValue();
            kTArray.set(kT.intValue(),kEntry.getKey());
        }

        kVMap = null;
        kTMap = null;
    }

    /**
     * This function constructs the binary trees for the lines of the image
     * in all three coordinate directions.  Afterwards, the octree is
     * traversed to merge the monoregions.
     *
     * @param iDepth This parameter controls the resolution of the extracted
     *   isosurface.  If its value is -1, the coarsest resolution surface is
     *   constructed.  If its value is n where the image size is
     *   m-by-m-by-m with m = power(2,n)+1, the finest resolution surface is
     *   constructed.  This surface corresponds to an extraction at the voxel
     *   level.  Any value of iDepth between -1 and n may be used.
     *   The smaller the value, the coarser the resolution the extracted
     *   surface is.
     */
    private void merge (int iDepth)
    {
        int iX, iY, iZ, iOffset, iStride;

        // construct the binary trees for the x-direction
        for (iY = 0; iY < m_iSize; iY++)
        {
            for (iZ = 0; iZ < m_iSize; iZ++)
            {
                iOffset = m_iSize*(iY+m_iSize*iZ);
                iStride = 1;
                m_aakXMerge[iY][iZ].setLevel(m_fLevel,m_afData,iOffset,
                    iStride);
            }
        }

        // construct the binary trees for the y-direction
        for (iX = 0; iX < m_iSize; iX++)
        {
            for (iZ = 0; iZ < m_iSize; iZ++)
            {
                iOffset = iX + m_iSizeSqr*iZ;
                iStride = m_iSize;
                m_aakYMerge[iX][iZ].setLevel(m_fLevel,m_afData,iOffset,
                    iStride);
            }
        }

        // construct the binary trees for the z-direction
        for (iX = 0; iX < m_iSize; iX++)
        {
            for (iY = 0; iY < m_iSize; iY++)
            {
                iOffset = iX + m_iSize*iY;
                iStride = m_iSizeSqr;
                m_aakZMerge[iX][iY].setLevel(m_fLevel,m_afData,iOffset,
                    iStride);
            }
        }

        // recursively traverse the octree and merge monoregions
        merge(0,0,0,0,0,0,0,m_iTwoPowerN,iDepth);
    }

    /**
     * Traverse the image as an octree and merge adjacent monoregions into
     * larger monoregions (if possible).
     *
     * @param iV the index of the octree node stored as a complete tree in
     *    a 1D array
     * @param iLX, iLY, iLZ the indices of the binary trees corresponding
     *    to the edges of the cube representing the octree node
     * @param iX0, iY0, iZ0 the origin of the cube representing the octree
     *    node
     * @param iS the edge length of the cube representing the octree node
     * @param iDepth This parameter controls the resolution of the extracted
     *   isosurface.  If its value is -1, the coarsest resolution surface is
     *   constructed.  If its value is n where the image size is
     *   m-by-m-by-m with m = power(2,n)+1, the finest resolution surface is
     *   constructed.  This surface corresponds to an extraction at the voxel
     *   level.  Any value of iDepth between -1 and n may be used.
     *   The smaller the value, the coarser the resolution the extracted
     *   surface is.
     */
    private boolean merge (int iV, int iLX, int iLY, int iLZ, int iX0,
        int iY0, int iZ0, int iS, int iDepth)
    {
        //System.out.println("extractContourMerge2 Level = " + iDepth);
        if ( iS > 1 )  // internal nodes
        {
            int iHS = iS/2;
            int iVB = 8*iV;
            int iV000 = iVB+1, iV100 = iVB+2, iV010 = iVB+3, iV110 = iVB+4;
            int iV001 = iVB+5, iV101 = iVB+6, iV011 = iVB+7, iV111 = iVB+8;
            int iLX0 = 2*iLX+1, iLX1 = iLX0+1;
            int iLY0 = 2*iLY+1, iLY1 = iLY0+1;
            int iLZ0 = 2*iLZ+1, iLZ1 = iLZ0+1;
            int iX1 = iX0+iHS, iY1 = iY0+iHS, iZ1 = iZ0+iHS;

            int iDM = iDepth - 1;
            boolean bM000 = merge(iV000,iLX0,iLY0,iLZ0,iX0,iY0,iZ0,iHS,iDM);
            boolean bM100 = merge(iV100,iLX1,iLY0,iLZ0,iX1,iY0,iZ0,iHS,iDM);
            boolean bM010 = merge(iV010,iLX0,iLY1,iLZ0,iX0,iY1,iZ0,iHS,iDM);
            boolean bM110 = merge(iV110,iLX1,iLY1,iLZ0,iX1,iY1,iZ0,iHS,iDM);
            boolean bM001 = merge(iV001,iLX0,iLY0,iLZ1,iX0,iY0,iZ1,iHS,iDM);
            boolean bM101 = merge(iV101,iLX1,iLY0,iLZ1,iX1,iY0,iZ1,iHS,iDM);
            boolean bM011 = merge(iV011,iLX0,iLY1,iLZ1,iX0,iY1,iZ1,iHS,iDM);
            boolean bM111 = merge(iV111,iLX1,iLY1,iLZ1,iX1,iY1,iZ1,iHS,iDM);

            // Attempt to merge monoregions, first in z-direction, next in
            //  y-direction, last in x-direction.
            ASC_MergeBox kR000 = new ASC_MergeBox(iHS);
            ASC_MergeBox kR100 = new ASC_MergeBox(iHS);
            ASC_MergeBox kR010 = new ASC_MergeBox(iHS);
            ASC_MergeBox kR110 = new ASC_MergeBox(iHS);
            ASC_MergeBox kR001 = new ASC_MergeBox(iHS);
            ASC_MergeBox kR101 = new ASC_MergeBox(iHS);
            ASC_MergeBox kR011 = new ASC_MergeBox(iHS);
            ASC_MergeBox kR111 = new ASC_MergeBox(iHS);

            if ( iDepth <= 0 )
            {
                if ( bM000 && bM001 ) doZMerge(kR000,kR001,iX0,iY0,iLZ);
                if ( bM100 && bM101 ) doZMerge(kR100,kR101,iX1,iY0,iLZ);
                if ( bM010 && bM011 ) doZMerge(kR010,kR011,iX0,iY1,iLZ);
                if ( bM110 && bM111 ) doZMerge(kR110,kR111,iX1,iY1,iLZ);
                if ( bM000 && bM010 ) doYMerge(kR000,kR010,iX0,iLY,iZ0);
                if ( bM100 && bM110 ) doYMerge(kR100,kR110,iX1,iLY,iZ0);
                if ( bM001 && bM011 ) doYMerge(kR001,kR011,iX0,iLY,iZ1);
                if ( bM101 && bM111 ) doYMerge(kR101,kR111,iX1,iLY,iZ1);
                if ( bM000 && bM100 ) doXMerge(kR000,kR100,iLX,iY0,iZ0);
                if ( bM010 && bM110 ) doXMerge(kR010,kR110,iLX,iY1,iZ0);
                if ( bM001 && bM101 ) doXMerge(kR001,kR101,iLX,iY0,iZ1);
                if ( bM011 && bM111 ) doXMerge(kR011,kR111,iLX,iY1,iZ1);
            }

            if ( iDepth <= 1 )
            {
                if ( kR000.m_bValid )
                {
                    if ( kR000.m_iXStride == iS )
                    {
                        if ( kR000.m_iYStride == iS )
                        {
                            if ( kR000.m_iZStride == iS )
                                return true;
                            else
                                addBox(iX0,iY0,iZ0,iS,iS,iHS,iLX,iLY,iLZ0);
                        }
                        else
                        {
                            if ( kR000.m_iZStride == iS )
                                addBox(iX0,iY0,iZ0,iS,iHS,iS,iLX,iLY0,iLZ);
                            else
                                addBox(iX0,iY0,iZ0,iS,iHS,iHS,iLX,iLY0,iLZ0);
                        }
                    }
                    else
                    {
                        if ( kR000.m_iYStride == iS )
                        {
                            if ( kR000.m_iZStride == iS )
                                addBox(iX0,iY0,iZ0,iHS,iS,iS,iLX0,iLY,iLZ);
                            else
                                addBox(iX0,iY0,iZ0,iHS,iS,iHS,iLX0,iLY,iLZ0);
                        }
                        else
                        {
                            if ( kR000.m_iZStride == iS )
                                addBox(iX0,iY0,iZ0,iHS,iHS,iS,iLX0,iLY0,iLZ);
                            else if ( bM000 )
                                addBox(iX0,iY0,iZ0,iHS,iHS,iHS,iLX0,iLY0,iLZ0);
                        }
                    }
                }

                if ( kR100.m_bValid )
                {
                    if ( kR100.m_iYStride == iS )
                    {
                        if ( kR100.m_iZStride == iS )
                            addBox(iX1,iY0,iZ0,iHS,iS,iS,iLX1,iLY,iLZ);
                        else
                            addBox(iX1,iY0,iZ0,iHS,iS,iHS,iLX1,iLY,iLZ0);
                    }
                    else
                    {
                        if ( kR100.m_iZStride == iS )
                            addBox(iX1,iY0,iZ0,iHS,iHS,iS,iLX1,iLY0,iLZ);
                        else if ( bM100 )
                            addBox(iX1,iY0,iZ0,iHS,iHS,iHS,iLX1,iLY0,iLZ0);
                    }
                }

                if ( kR010.m_bValid )
                {
                    if ( kR010.m_iXStride == iS )
                    {
                        if ( kR010.m_iZStride == iS )
                            addBox(iX0,iY1,iZ0,iS,iHS,iS,iLX,iLY1,iLZ);
                        else
                            addBox(iX0,iY1,iZ0,iS,iHS,iHS,iLX,iLY1,iLZ0);
                    }
                    else
                    {
                        if ( kR010.m_iZStride == iS )
                            addBox(iX0,iY1,iZ0,iHS,iHS,iS,iLX0,iLY1,iLZ);
                        else if ( bM010 )
                            addBox(iX0,iY1,iZ0,iHS,iHS,iHS,iLX0,iLY1,iLZ0);
                    }
                }

                if ( kR001.m_bValid )
                {
                    if ( kR001.m_iXStride == iS )
                    {
                        if ( kR001.m_iYStride == iS )
                            addBox(iX0,iY0,iZ1,iS,iS,iHS,iLX,iLY,iLZ1);
                        else
                            addBox(iX0,iY0,iZ1,iS,iHS,iHS,iLX,iLY0,iLZ1);
                    }
                    else
                    {
                        if ( kR001.m_iYStride == iS )
                            addBox(iX0,iY0,iZ1,iHS,iS,iHS,iLX0,iLY,iLZ1);
                        else if ( bM001 )
                            addBox(iX0,iY0,iZ1,iHS,iHS,iHS,iLX0,iLY0,iLZ1);
                    }
                }

                if ( kR110.m_bValid )
                {
                    if ( kR110.m_iZStride == iS )
                        addBox(iX1,iY1,iZ0,iHS,iHS,iS,iLX1,iLY1,iLZ);
                    else if ( bM110 )
                        addBox(iX1,iY1,iZ0,iHS,iHS,iHS,iLX1,iLY1,iLZ0);
                }

                if ( kR101.m_bValid )
                {
                    if ( kR101.m_iYStride == iS )
                        addBox(iX1,iY0,iZ1,iHS,iS,iHS,iLX1,iLY,iLZ1);
                    else if ( bM101 )
                        addBox(iX1,iY0,iZ1,iHS,iHS,iHS,iLX1,iLY0,iLZ1);
                }

                if ( kR011.m_bValid )
                {
                    if ( kR011.m_iXStride == iS )
                        addBox(iX0,iY1,iZ1,iS,iHS,iHS,iLX,iLY1,iLZ1);
                    else if ( bM011 )
                        addBox(iX0,iY1,iZ1,iHS,iHS,iHS,iLX0,iLY1,iLZ1);
                }

                if ( kR111.m_bValid && bM111 )
                    addBox(iX1,iY1,iZ1,iHS,iHS,iHS,iLX1,iLY1,iLZ1);
            }
            return false;
        }
        else  // leaf nodes
        {
            if ( m_bFixBoundary )
            {
                // Do not allow boundary voxels to merge with other voxels.
                addBox(iX0,iY0,iZ0,1,1,1,iLX,iLY,iLZ);
                return false;
            }

            // A leaf box is mergeable with neighbors as long as all its faces
            // have 0 or 2 sign changes on the edges.  That is, a face may not
            // have sign changes on all four edges.  If it does, the resulting
            // box for tessellating is 1x1x1 and is handled separately from
            // boxes of larger dimensions.

            // xmin face
            int iZ1 = iZ0+1;
            int iRT0 = m_aakYMerge[iX0][iZ0].getRootType(iLY);
            int iRT1 = m_aakYMerge[iX0][iZ1].getRootType(iLY);
            if ( (iRT0 | iRT1) == ASC_LinearMergeTree.CFG_MULT )
            {
                addBox(iX0,iY0,iZ0,1,1,1,iLX,iLY,iLZ);
                return false;
            }

            // xmax face
            int iX1 = iX0+1;
            iRT0 = m_aakYMerge[iX1][iZ0].getRootType(iLY);
            iRT1 = m_aakYMerge[iX1][iZ1].getRootType(iLY);
            if ( (iRT0 | iRT1) == ASC_LinearMergeTree.CFG_MULT )
            {
                addBox(iX0,iY0,iZ0,1,1,1,iLX,iLY,iLZ);
                return false;
            }

            // ymin face
            iRT0 = m_aakZMerge[iX0][iY0].getRootType(iLZ);
            iRT1 = m_aakZMerge[iX1][iY0].getRootType(iLZ);
            if ( (iRT0 | iRT1) == ASC_LinearMergeTree.CFG_MULT )
            {
                addBox(iX0,iY0,iZ0,1,1,1,iLX,iLY,iLZ);
                return false;
            }

            // ymax face
            int iY1 = iY0+1;
            iRT0 = m_aakZMerge[iX0][iY1].getRootType(iLZ);
            iRT1 = m_aakZMerge[iX1][iY1].getRootType(iLZ);
            if ( (iRT0 | iRT1) == ASC_LinearMergeTree.CFG_MULT )
            {
                addBox(iX0,iY0,iZ0,1,1,1,iLX,iLY,iLZ);
                return false;
            }

            // zmin face
            iRT0 = m_aakXMerge[iY0][iZ0].getRootType(iLX);
            iRT1 = m_aakXMerge[iY1][iZ0].getRootType(iLX);
            if ( (iRT0 | iRT1) == ASC_LinearMergeTree.CFG_MULT )
            {
                addBox(iX0,iY0,iZ0,1,1,1,iLX,iLY,iLZ);
                return false;
            }

            // zmax face
            iRT0 = m_aakXMerge[iY0][iZ1].getRootType(iLX);
            iRT1 = m_aakXMerge[iY1][iZ1].getRootType(iLX);
            if ( (iRT0 | iRT1) == ASC_LinearMergeTree.CFG_MULT )
            {
                addBox(iX0,iY0,iZ0,1,1,1,iLX,iLY,iLZ);
                return false;
            }

            return true;
        }
    }

    /**
     * The function for merging two boxes in the x-direction.  See
     * AdaptiveSkeletonClimbing.pdf for details.
     *
     * @param kR0, kR1 the boxes to attempt merging
     * @param iLX the index of the x-direction binary tree corresponding to
     *    the x-line containing iXOrigin
     * @param iYOrigin the common y-value for the origins of the rectangles
     * @param iZOrigin the common z-value for the origins of the rectangles
     */
    private boolean doXMerge (ASC_MergeBox kR0, ASC_MergeBox kR1, int iLX,
        int iY0, int iZ0)
    {
        if ( !kR0.m_bValid || !kR1.m_bValid
        ||   kR0.m_iYStride != kR1.m_iYStride
        ||   kR0.m_iZStride != kR1.m_iZStride )
        {
            return false;
        }

        // boxes are potentially x-mergeable
        int iY1 = iY0 + kR0.m_iYStride, iZ1 = iZ0 + kR0.m_iZStride;
        int iIncr = 0, iDecr = 0;
        for (int iY = iY0; iY <= iY1; iY++)
        {
            for (int iZ = iZ0; iZ <= iZ1; iZ++)
            {
                switch ( m_aakXMerge[iY][iZ].getRootType(iLX) )
                {
                case ASC_LinearMergeTree.CFG_MULT:  return false;
                case ASC_LinearMergeTree.CFG_INCR:  iIncr++;  break;
                case ASC_LinearMergeTree.CFG_DECR:  iDecr++;  break;
                }
            }
        }

        if ( iIncr != 0 && iDecr != 0 )
            return false;

        // strongly mono, x-merge the boxes
        kR0.m_iXStride *= 2;
        kR1.m_bValid = false;
        return true;
    }

    /**
     * The function for merging two boxes in the y-direction.  See
     * AdaptiveSkeletonClimbing.pdf for details.
     *
     * @param kR0, kR1 the boxes to attempt merging
     * @param iXOrigin the common x-value for the origins of the rectangles
     * @param iLY the index of the y-direction binary tree corresponding to
     *    the y-line containing iYOrigin
     * @param iZOrigin the common z-value for the origins of the rectangles
     */
    private boolean doYMerge (ASC_MergeBox kR0, ASC_MergeBox kR1, int iX0,
        int iLY, int iZ0)
    {
        if ( !kR0.m_bValid || !kR1.m_bValid
        ||   kR0.m_iXStride != kR1.m_iXStride
        ||   kR0.m_iZStride != kR1.m_iZStride )
        {
            return false;
        }

        // boxes are potentially y-mergeable
        int iX1 = iX0 + kR0.m_iXStride, iZ1 = iZ0 + kR0.m_iZStride;
        int iIncr = 0, iDecr = 0;
        for (int iX = iX0; iX <= iX1; iX++)
        {
            for (int iZ = iZ0; iZ <= iZ1; iZ++)
            {
                switch ( m_aakYMerge[iX][iZ].getRootType(iLY) )
                {
                case ASC_LinearMergeTree.CFG_MULT:  return false;
                case ASC_LinearMergeTree.CFG_INCR:  iIncr++;  break;
                case ASC_LinearMergeTree.CFG_DECR:  iDecr++;  break;
                }
            }
        }

        if ( iIncr != 0 && iDecr != 0 )
            return false;

        // strongly mono, y-merge the boxes
        kR0.m_iYStride *= 2;
        kR1.m_bValid = false;
        return true;
    }

    /**
     * The function for merging two boxes in the z-direction.  See
     * AdaptiveSkeletonClimbing.pdf for details.
     *
     * @param kR0, kR1 the boxes to attempt merging
     * @param iXOrigin the common x-value for the origins of the rectangles
     * @param iYOrigin the common y-value for the origins of the rectangles
     * @param iLZ the index of the z-direction binary tree corresponding to
     *    the z-line containing iZOrigin
     */
    private boolean doZMerge (ASC_MergeBox kR0, ASC_MergeBox kR1, int iX0,
        int iY0, int iLZ)
    {
        if ( !kR0.m_bValid || !kR1.m_bValid
        ||   kR0.m_iXStride != kR1.m_iXStride
        ||   kR0.m_iYStride != kR1.m_iYStride )
        {
            return false;
        }

        // boxes are potentially z-mergeable
        int iX1 = iX0 + kR0.m_iXStride, iY1 = iY0 + kR0.m_iYStride;
        int iIncr = 0, iDecr = 0;
        for (int iX = iX0; iX <= iX1; iX++)
        {
            for (int iY = iY0; iY <= iY1; iY++)
            {
                switch ( m_aakZMerge[iX][iY].getRootType(iLZ) )
                {
                case ASC_LinearMergeTree.CFG_MULT:  return false;
                case ASC_LinearMergeTree.CFG_INCR:  iIncr++;  break;
                case ASC_LinearMergeTree.CFG_DECR:  iDecr++;  break;
                }
            }
        }

        if ( iIncr != 0 && iDecr != 0 )
            return false;

        // strongly mono, z-merge the boxes
        kR0.m_iZStride *= 2;
        kR1.m_bValid = false;
        return true;
    }


    /**
     * Add the largest possible monoregion as defined by the input values.
     *
     * @param iX0, iY0, iZ0 the origin of the box
     * @param iDX, iDY, iDZ the edge lengths of the box
     * @param iLX, iLY, iLZ the indices for the binary trees corresponding
     *    to the edges of the box
     */
    private void addBox (int iX0, int iY0, int iZ0, int iDX, int iDY,
        int iDZ, int iLX, int iLY, int iLZ)
    {
        ASC_OctBox kBox = new ASC_OctBox(iX0,iY0,iZ0,iDX,iDY,iDZ,iLX,iLY,iLZ);
        m_kBoxes.add(kBox);

        // Mark box edges in linear merge trees.  This information will be
        // used in the crack avoidance.
        m_aakXMerge[kBox.m_iY0][kBox.m_iZ0].setEdge(kBox.m_iLX);
        m_aakXMerge[kBox.m_iY0][kBox.m_iZ1].setEdge(kBox.m_iLX);
        m_aakXMerge[kBox.m_iY1][kBox.m_iZ0].setEdge(kBox.m_iLX);
        m_aakXMerge[kBox.m_iY1][kBox.m_iZ1].setEdge(kBox.m_iLX);
        m_aakYMerge[kBox.m_iX0][kBox.m_iZ0].setEdge(kBox.m_iLY);
        m_aakYMerge[kBox.m_iX0][kBox.m_iZ1].setEdge(kBox.m_iLY);
        m_aakYMerge[kBox.m_iX1][kBox.m_iZ0].setEdge(kBox.m_iLY);
        m_aakYMerge[kBox.m_iX1][kBox.m_iZ1].setEdge(kBox.m_iLY);
        m_aakZMerge[kBox.m_iX0][kBox.m_iY0].setEdge(kBox.m_iLZ);
        m_aakZMerge[kBox.m_iX0][kBox.m_iY1].setEdge(kBox.m_iLZ);
        m_aakZMerge[kBox.m_iX1][kBox.m_iY0].setEdge(kBox.m_iLZ);
        m_aakZMerge[kBox.m_iX1][kBox.m_iY1].setEdge(kBox.m_iLZ);
    }

    /**
     * Construct the triangle meshes that represent the intersection of the
     * level surface with the monoregion boxes.
     *
     * @param kVArray the vertices of the full mesh (output)
     * @param kTArray the triangles of the full mesh (output)
     */
    private void tessellate (Vector kVArray, Vector kTArray)
    {
        int i;
        for (i = 0; i < m_kBoxes.size(); i++)
        {
            ASC_OctBox kBox = (ASC_OctBox)m_kBoxes.get(i);

            // get vertices on edges of box
            ASC_VertexTable kTable = new ASC_VertexTable();
            int iType = getVertices(kBox,kTable);
            if ( iType == 0 )
                continue;

            // add wireframe edges to table, add face-vertices if necessary
            if ( kBox.m_iDX > 1 || kBox.m_iDY > 1 || kBox.m_iDZ > 1 )
            {
                // box is larger than voxel, each face has at most one edge
                getXMinEdgesM(kBox,iType,kTable);
                getXMaxEdgesM(kBox,iType,kTable);
                getYMinEdgesM(kBox,iType,kTable);
                getYMaxEdgesM(kBox,iType,kTable);
                getZMinEdgesM(kBox,iType,kTable);
                getZMaxEdgesM(kBox,iType,kTable);

                if ( kTable.getVertexQuantity() > 18 )
                    kTable.removeTrianglesSE(kVArray,kTArray);
                else
                    kTable.removeTrianglesEC(kVArray,kTArray);
            }
            else
            {
                // 1x1x1 voxel, do full edge analysis, no splitting required
                getXMinEdgesS(kBox,iType,kTable);
                getXMaxEdgesS(kBox,iType,kTable);
                getYMinEdgesS(kBox,iType,kTable);
                getYMaxEdgesS(kBox,iType,kTable);
                getZMinEdgesS(kBox,iType,kTable);
                getZMaxEdgesS(kBox,iType,kTable);
                kTable.removeTrianglesEC(kVArray,kTArray);
            }
        }

        ASC_Vertex3 kVMat[] = new ASC_Vertex3[kVArray.size()];
        for (i = 0; i < kVArray.size(); i++) {
            kVMat[i] = (ASC_Vertex3)kVArray.get(i);
        }
        for (i = kVArray.size() - 1; i >= 0; i--) {
            kVArray.remove(i);
        }
        for (i = 0; i < kVMat.length; i++) {
            kVArray.add(new ASC_Vertex3(kVMat[i].m_fX,kVMat[i].m_fY,kVMat[i].m_fZ));
        }
        // restore sorting to default
        ASC_Vertex3.ms_iSortOn = 0;
    }

    /**
     * Interpolate the voxel edge (x,y,z) to (x+1,y,z).  The caller of this
     * method guarantees that m_fLevel is between the image values I(x,y,z)
     * and I(x+1,y,z).
     *
     * @param iX, iY, iZ the starting point of the voxel edge to be
     *    interpolated
     * @return The x-value at which the isosurface intersects the edge.  It
     *    is guaranteed that x < xIntersect < x+1.
     */
    private float getXInterp (int iX, int iY, int iZ)
    {
        int iIndex = iX + m_iSize*(iY + m_iSize*iZ);
        float fF0 = m_afData[iIndex];
        iIndex++;
        float fF1 = m_afData[iIndex];
        return (float)iX + (m_fLevel - fF0)/(fF1 - fF0);
    }

    /**
     * Interpolate the voxel edge (x,y,z) to (x,y+1,z).  The caller of this
     * method guarantees that m_fLevel is between the image values I(x,y,z)
     * and I(x,y+1,z).
     *
     * @param fLevel the desired level value for the isosurface
     * @param iX, iY, iZ the starting point of the voxel edge to be
     *    interpolated
     * @return The y-value at which the isosurface intersects the edge.  It
     *    is guaranteed that y < yIntersect < y+1.
     */
    private float getYInterp (int iX, int iY, int iZ)
    {
        int iIndex = iX + m_iSize*(iY + m_iSize*iZ);
        float fF0 = m_afData[iIndex];
        iIndex += m_iSize;
        float fF1 = m_afData[iIndex];
        return (float)iY + (m_fLevel - fF0)/(fF1 - fF0);
    }

    /**
     * Interpolate the voxel edge (x,y,z) to (x,y,z+1).  The caller of this
     * method guarantees that m_fLevel is between the image values I(x,y,z)
     * and I(x,y,z+1).
     *
     * @param fLevel the desired level value for the isosurface
     * @param iX, iY, iZ the starting point of the voxel edge to be
     *    interpolated
     * @return The z-value at which the isosurface intersects the edge.  It
     *    is guaranteed that z < zIntersect < z+1.
     */
    private float getZInterp (int iX, int iY, int iZ)
    {
        int iIndex = iX + m_iSize*(iY + m_iSize*iZ);
        float fF0 = m_afData[iIndex];
        iIndex += m_iSizeSqr;
        float fF1 = m_afData[iIndex];
        return (float)iZ + (m_fLevel - fF0)/(fF1 - fF0);
    }

    /**
     * Compute the points of intersection of the level surface with the
     * edges of the specified monoregion box.
     *
     * @param kBox the monoregion box
     * @param kTable the vertex-edge table representing the surface-box
     *    intersection as a wireframe
     * @return a bit-flag whose bits store information about whether or not
     *    a box edge is intersected by the level surface
     */
    private int getVertices (ASC_OctBox kBox, ASC_VertexTable kTable)
    {
        int iType = 0, iRoot;

        // xmin-ymin edge
        iRoot = m_aakZMerge[kBox.m_iX0][kBox.m_iY0].getZeroBase(kBox.m_iLZ);
        if ( iRoot != -1 )
        {
            iType |= EB_XMIN_YMIN;
            kTable.insert(EI_XMIN_YMIN,
                (float)kBox.m_iX0,
                (float)kBox.m_iY0,
                getZInterp(kBox.m_iX0,kBox.m_iY0,iRoot));
        }

        // xmin-ymax edge
        iRoot = m_aakZMerge[kBox.m_iX0][kBox.m_iY1].getZeroBase(kBox.m_iLZ);
        if ( iRoot != -1 )
        {
            iType |= EB_XMIN_YMAX;
            kTable.insert(EI_XMIN_YMAX,
                (float)kBox.m_iX0,
                (float)kBox.m_iY1,
                getZInterp(kBox.m_iX0,kBox.m_iY1,iRoot));
        }

        // xmax-ymin edge
        iRoot = m_aakZMerge[kBox.m_iX1][kBox.m_iY0].getZeroBase(kBox.m_iLZ);
        if ( iRoot != -1 )
        {
            iType |= EB_XMAX_YMIN;
            kTable.insert(EI_XMAX_YMIN,
                (float)kBox.m_iX1,
                (float)kBox.m_iY0,
                getZInterp(kBox.m_iX1,kBox.m_iY0,iRoot));
        }

        // xmax-ymax edge
        iRoot = m_aakZMerge[kBox.m_iX1][kBox.m_iY1].getZeroBase(kBox.m_iLZ);
        if ( iRoot != -1 )
        {
            iType |= EB_XMAX_YMAX;
            kTable.insert(EI_XMAX_YMAX,
                (float)kBox.m_iX1,
                (float)kBox.m_iY1,
                getZInterp(kBox.m_iX1,kBox.m_iY1,iRoot));
        }

        // xmin-zmin edge
        iRoot = m_aakYMerge[kBox.m_iX0][kBox.m_iZ0].getZeroBase(kBox.m_iLY);
        if ( iRoot != -1 )
        {
            iType |= EB_XMIN_ZMIN;
            kTable.insert(EI_XMIN_ZMIN,
                (float)kBox.m_iX0,
                getYInterp(kBox.m_iX0,iRoot,kBox.m_iZ0),
                (float)kBox.m_iZ0);
        }

        // xmin-zmax edge
        iRoot = m_aakYMerge[kBox.m_iX0][kBox.m_iZ1].getZeroBase(kBox.m_iLY);
        if ( iRoot != -1 )
        {
            iType |= EB_XMIN_ZMAX;
            kTable.insert(EI_XMIN_ZMAX,
                (float)kBox.m_iX0,
                getYInterp(kBox.m_iX0,iRoot,kBox.m_iZ1),
                (float)kBox.m_iZ1);
        }

        // xmax-zmin edge
        iRoot = m_aakYMerge[kBox.m_iX1][kBox.m_iZ0].getZeroBase(kBox.m_iLY);
        if ( iRoot != -1 )
        {
            iType |= EB_XMAX_ZMIN;
            kTable.insert(EI_XMAX_ZMIN,
                (float)kBox.m_iX1,
                getYInterp(kBox.m_iX1,iRoot,kBox.m_iZ0),
                (float)kBox.m_iZ0);
        }

        // xmax-zmax edge
        iRoot = m_aakYMerge[kBox.m_iX1][kBox.m_iZ1].getZeroBase(kBox.m_iLY);
        if ( iRoot != -1 )
        {
            iType |= EB_XMAX_ZMAX;
            kTable.insert(EI_XMAX_ZMAX,
                (float)kBox.m_iX1,
                getYInterp(kBox.m_iX1,iRoot,kBox.m_iZ1),
                (float)kBox.m_iZ1);
        }

        // ymin-zmin edge
        iRoot = m_aakXMerge[kBox.m_iY0][kBox.m_iZ0].getZeroBase(kBox.m_iLX);
        if ( iRoot != -1 )
        {
            iType |= EB_YMIN_ZMIN;
            kTable.insert(EI_YMIN_ZMIN,
                getXInterp(iRoot,kBox.m_iY0,kBox.m_iZ0),
                (float)kBox.m_iY0,
                (float)kBox.m_iZ0);
        }

        // ymin-zmax edge
        iRoot = m_aakXMerge[kBox.m_iY0][kBox.m_iZ1].getZeroBase(kBox.m_iLX);
        if ( iRoot != -1 )
        {
            iType |= EB_YMIN_ZMAX;
            kTable.insert(EI_YMIN_ZMAX,
                getXInterp(iRoot,kBox.m_iY0,kBox.m_iZ1),
                (float)kBox.m_iY0,
                (float)kBox.m_iZ1);
        }

        // ymax-zmin edge
        iRoot = m_aakXMerge[kBox.m_iY1][kBox.m_iZ0].getZeroBase(kBox.m_iLX);
        if ( iRoot != -1 )
        {
            iType |= EB_YMAX_ZMIN;
            kTable.insert(EI_YMAX_ZMIN,
                getXInterp(iRoot,kBox.m_iY1,kBox.m_iZ0),
                (float)kBox.m_iY1,
                (float)kBox.m_iZ0);
        }

        // ymax-zmax edge
        iRoot = m_aakXMerge[kBox.m_iY1][kBox.m_iZ1].getZeroBase(kBox.m_iLX);
        if ( iRoot != -1 )
        {
            iType |= EB_YMAX_ZMAX;
            kTable.insert(EI_YMAX_ZMAX,
                getXInterp(iRoot,kBox.m_iY1,kBox.m_iZ1),
                (float)kBox.m_iY1,
                (float)kBox.m_iZ1);
        }

        return iType;
    }

    /**
     * Compute the edges that connect the vertices on the xmin face of a
     * 1-by-1-by-1 voxel that are found by getVertices.  An additional vertex
     * is added in the case when the face intersects the level surface in a
     * plus-sign configuration.  That vertex is the branch point of the plus.
     *
     * @param kBox the monoregion box
     * @param iType the bit-flag whose bits indicate which edges of the box
     *    are intersected by the level surface
     * @param kTable the vertex-edge table representing the surface-box
     *    intersection as a wireframe
     */
    private void getXMinEdgesS (ASC_OctBox kBox, int iType,
        ASC_VertexTable kTable)
    {
        int iFaceType = 0;
        if ( (iType & EB_XMIN_YMIN) != 0 ) iFaceType |= 0x01;
        if ( (iType & EB_XMIN_YMAX) != 0 ) iFaceType |= 0x02;
        if ( (iType & EB_XMIN_ZMIN) != 0 ) iFaceType |= 0x04;
        if ( (iType & EB_XMIN_ZMAX) != 0 ) iFaceType |= 0x08;

        switch ( iFaceType )
        {
        case  0: return;
        case  3: kTable.insert(EI_XMIN_YMIN,EI_XMIN_YMAX); break;
        case  5: kTable.insert(EI_XMIN_YMIN,EI_XMIN_ZMIN); break;
        case  6: kTable.insert(EI_XMIN_YMAX,EI_XMIN_ZMIN); break;
        case  9: kTable.insert(EI_XMIN_YMIN,EI_XMIN_ZMAX); break;
        case 10: kTable.insert(EI_XMIN_YMAX,EI_XMIN_ZMAX); break;
        case 12: kTable.insert(EI_XMIN_ZMIN,EI_XMIN_ZMAX); break;
        case 15:
        {
            // four vertices, one per edge, need to disambiguate
            int i = kBox.m_iX0 + m_iSize*(kBox.m_iY0 + m_iSize*kBox.m_iZ0);
            float fF00 = m_afData[i];  // F(x,y,z)
            i += m_iSize;
            float fF10 = m_afData[i];  // F(x,y+1,z)
            i += m_iSizeSqr;
            float fF11 = m_afData[i];  // F(x,y+1,z+1)
            i -= m_iSize;
            float fF01 = m_afData[i];  // F(x,y,z+1)
            float fDet = fF00*fF11 - fF01*fF10;

            if ( fDet > 0.0f )
            {
                // disjoint hyperbolic segments, pair <P0,P2>, <P1,P3>
                kTable.insert(EI_XMIN_YMIN,EI_XMIN_ZMIN);
                kTable.insert(EI_XMIN_YMAX,EI_XMIN_ZMAX);
            }
            else if ( fDet < 0.0f )
            {
                // disjoint hyperbolic segments, pair <P0,P3>, <P1,P2>
                kTable.insert(EI_XMIN_YMIN,EI_XMIN_ZMAX);
                kTable.insert(EI_XMIN_YMAX,EI_XMIN_ZMIN);
            }
            else
            {
                // plus-sign configuration, add branch point to tessellation
                kTable.insert(FI_XMIN,
                    kTable.getVertex(EI_XMIN_ZMIN).m_fX,
                    kTable.getVertex(EI_XMIN_ZMIN).m_fY,
                    kTable.getVertex(EI_XMIN_YMIN).m_fZ);

                // add edges sharing the branch point
                kTable.insert(EI_XMIN_YMIN,FI_XMIN);
                kTable.insert(EI_XMIN_YMAX,FI_XMIN);
                kTable.insert(EI_XMIN_ZMIN,FI_XMIN);
                kTable.insert(EI_XMIN_ZMAX,FI_XMIN);
            }
            break;
        }
        }
    }

    /**
     * Compute the edges that connect the vertices on the xmax face of a
     * 1-by-1-by-1 voxel that are found by getVertices.  An additional vertex
     * is added in the case when the face intersects the level surface in a
     * plus-sign configuration.  That vertex is the branch point of the plus.
     *
     * @param kBox the monoregion box
     * @param iType the bit-flag whose bits indicate which edges of the box
     *    are intersected by the level surface
     * @param kTable the vertex-edge table representing the surface-box
     *    intersection as a wireframe
     */
    private void getXMaxEdgesS (ASC_OctBox kBox, int iType,
        ASC_VertexTable kTable)
    {
        int iFaceType = 0;
        if ( (iType & EB_XMAX_YMIN) != 0 ) iFaceType |= 0x01;
        if ( (iType & EB_XMAX_YMAX) != 0 ) iFaceType |= 0x02;
        if ( (iType & EB_XMAX_ZMIN) != 0 ) iFaceType |= 0x04;
        if ( (iType & EB_XMAX_ZMAX) != 0 ) iFaceType |= 0x08;

        switch ( iFaceType )
        {
        case  0: return;
        case  3: kTable.insert(EI_XMAX_YMIN,EI_XMAX_YMAX); break;
        case  5: kTable.insert(EI_XMAX_YMIN,EI_XMAX_ZMIN); break;
        case  6: kTable.insert(EI_XMAX_YMAX,EI_XMAX_ZMIN); break;
        case  9: kTable.insert(EI_XMAX_YMIN,EI_XMAX_ZMAX); break;
        case 10: kTable.insert(EI_XMAX_YMAX,EI_XMAX_ZMAX); break;
        case 12: kTable.insert(EI_XMAX_ZMIN,EI_XMAX_ZMAX); break;
        case 15:
        {
            // four vertices, one per edge, need to disambiguate
            int i = kBox.m_iX1 + m_iSize*(kBox.m_iY0 + m_iSize*kBox.m_iZ0);
            float fF00 = m_afData[i];  // F(x,y,z)
            i += m_iSize;
            float fF10 = m_afData[i];  // F(x,y+1,z)
            i += m_iSizeSqr;
            float fF11 = m_afData[i];  // F(x,y+1,z+1)
            i -= m_iSize;
            float fF01 = m_afData[i];  // F(x,y,z+1)
            float fDet = fF00*fF11 - fF01*fF10;

            if ( fDet > 0.0f )
            {
                // disjoint hyperbolic segments, pair <P0,P2>, <P1,P3>
                kTable.insert(EI_XMAX_YMIN,EI_XMAX_ZMIN);
                kTable.insert(EI_XMAX_YMAX,EI_XMAX_ZMAX);
            }
            else if ( fDet < 0.0f )
            {
                // disjoint hyperbolic segments, pair <P0,P3>, <P1,P2>
                kTable.insert(EI_XMAX_YMIN,EI_XMAX_ZMAX);
                kTable.insert(EI_XMAX_YMAX,EI_XMAX_ZMIN);
            }
            else
            {
                // plus-sign configuration, add branch point to tessellation
                kTable.insert(FI_XMAX,
                    kTable.getVertex(EI_XMAX_ZMIN).m_fX,
                    kTable.getVertex(EI_XMAX_ZMIN).m_fY,
                    kTable.getVertex(EI_XMAX_YMIN).m_fZ);

                // add edges sharing the branch point
                kTable.insert(EI_XMAX_YMIN,FI_XMAX);
                kTable.insert(EI_XMAX_YMAX,FI_XMAX);
                kTable.insert(EI_XMAX_ZMIN,FI_XMAX);
                kTable.insert(EI_XMAX_ZMAX,FI_XMAX);
            }
            break;
        }
        }
    }

    /**
     * Compute the edges that connect the vertices on the ymin face of a
     * 1-by-1-by-1 voxel that are found by getVertices.  An additional vertex
     * is added in the case when the face intersects the level surface in a
     * plus-sign configuration.  That vertex is the branch point of the plus.
     *
     * @param kBox the monoregion box
     * @param iType the bit-flag whose bits indicate which edges of the box
     *    are intersected by the level surface
     * @param kTable the vertex-edge table representing the surface-box
     *    intersection as a wireframe
     */
    private void getYMinEdgesS (ASC_OctBox kBox, int iType,
        ASC_VertexTable kTable)
    {
        int iFaceType = 0;
        if ( (iType & EB_XMIN_YMIN) != 0 ) iFaceType |= 0x01;
        if ( (iType & EB_XMAX_YMIN) != 0 ) iFaceType |= 0x02;
        if ( (iType & EB_YMIN_ZMIN) != 0 ) iFaceType |= 0x04;
        if ( (iType & EB_YMIN_ZMAX) != 0 ) iFaceType |= 0x08;

        switch ( iFaceType )
        {
        case  0: return;
        case  3: kTable.insert(EI_XMIN_YMIN,EI_XMAX_YMIN); break;
        case  5: kTable.insert(EI_XMIN_YMIN,EI_YMIN_ZMIN); break;
        case  6: kTable.insert(EI_XMAX_YMIN,EI_YMIN_ZMIN); break;
        case  9: kTable.insert(EI_XMIN_YMIN,EI_YMIN_ZMAX); break;
        case 10: kTable.insert(EI_XMAX_YMIN,EI_YMIN_ZMAX); break;
        case 12: kTable.insert(EI_YMIN_ZMIN,EI_YMIN_ZMAX); break;
        case 15:
        {
            // four vertices, one per edge, need to disambiguate
            int i = kBox.m_iX0 + m_iSize*(kBox.m_iY0 + m_iSize*kBox.m_iZ0);
            float fF00 = m_afData[i];  // F(x,y,z)
            i++;
            float fF10 = m_afData[i];  // F(x+1,y,z)
            i += m_iSizeSqr;
            float fF11 = m_afData[i];  // F(x+1,y,z+1)
            i--;
            float fF01 = m_afData[i];  // F(x,y,z+1)
            float fDet = fF00*fF11 - fF01*fF10;

            if ( fDet > 0.0f )
            {
                // disjoint hyperbolic segments, pair <P0,P2>, <P1,P3>
                kTable.insert(EI_XMIN_YMIN,EI_YMIN_ZMIN);
                kTable.insert(EI_XMAX_YMIN,EI_YMIN_ZMAX);
            }
            else if ( fDet < 0.0f )
            {
                // disjoint hyperbolic segments, pair <P0,P3>, <P1,P2>
                kTable.insert(EI_XMIN_YMIN,EI_YMIN_ZMAX);
                kTable.insert(EI_XMAX_YMIN,EI_YMIN_ZMIN);
            }
            else
            {
                // plus-sign configuration, add branch point to tessellation
                kTable.insert(FI_YMIN,
                    kTable.getVertex(EI_YMIN_ZMIN).m_fX,
                    kTable.getVertex(EI_XMIN_YMIN).m_fY,
                    kTable.getVertex(EI_XMIN_YMIN).m_fZ);

                // add edges sharing the branch point
                kTable.insert(EI_XMIN_YMIN,FI_YMIN);
                kTable.insert(EI_XMAX_YMIN,FI_YMIN);
                kTable.insert(EI_YMIN_ZMIN,FI_YMIN);
                kTable.insert(EI_YMIN_ZMAX,FI_YMIN);
            }
            break;
        }
        }
    }

    /**
     * Compute the edges that connect the vertices on the ymax face of a
     * 1-by-1-by-1 voxel that are found by getVertices.  An additional vertex
     * is added in the case when the face intersects the level surface in a
     * plus-sign configuration.  That vertex is the branch point of the plus.
     *
     * @param kBox the monoregion box
     * @param iType the bit-flag whose bits indicate which edges of the box
     *    are intersected by the level surface
     * @param kTable the vertex-edge table representing the surface-box
     *    intersection as a wireframe
     */
    private void getYMaxEdgesS (ASC_OctBox kBox, int iType,
        ASC_VertexTable kTable)
    {
        int iFaceType = 0;
        if ( (iType & EB_XMIN_YMAX) != 0 ) iFaceType |= 0x01;
        if ( (iType & EB_XMAX_YMAX) != 0 ) iFaceType |= 0x02;
        if ( (iType & EB_YMAX_ZMIN) != 0 ) iFaceType |= 0x04;
        if ( (iType & EB_YMAX_ZMAX) != 0 ) iFaceType |= 0x08;

        switch ( iFaceType )
        {
        case  0: return;
        case  3: kTable.insert(EI_XMIN_YMAX,EI_XMAX_YMAX); break;
        case  5: kTable.insert(EI_XMIN_YMAX,EI_YMAX_ZMIN); break;
        case  6: kTable.insert(EI_XMAX_YMAX,EI_YMAX_ZMIN); break;
        case  9: kTable.insert(EI_XMIN_YMAX,EI_YMAX_ZMAX); break;
        case 10: kTable.insert(EI_XMAX_YMAX,EI_YMAX_ZMAX); break;
        case 12: kTable.insert(EI_YMAX_ZMIN,EI_YMAX_ZMAX); break;
        case 15:
        {
            // four vertices, one per edge, need to disambiguate
            int i = kBox.m_iX0 + m_iSize*(kBox.m_iY1 + m_iSize*kBox.m_iZ0);
            float fF00 = m_afData[i];  // F(x,y,z)
            i++;
            float fF10 = m_afData[i];  // F(x+1,y,z)
            i += m_iSizeSqr;
            float fF11 = m_afData[i];  // F(x+1,y,z+1)
            i--;
            float fF01 = m_afData[i];  // F(x,y,z+1)
            float fDet = fF00*fF11 - fF01*fF10;

            if ( fDet > 0.0f )
            {
                // disjoint hyperbolic segments, pair <P0,P2>, <P1,P3>
                kTable.insert(EI_XMIN_YMAX,EI_YMAX_ZMIN);
                kTable.insert(EI_XMAX_YMAX,EI_YMAX_ZMAX);
            }
            else if ( fDet < 0.0f )
            {
                // disjoint hyperbolic segments, pair <P0,P3>, <P1,P2>
                kTable.insert(EI_XMIN_YMAX,EI_YMAX_ZMAX);
                kTable.insert(EI_XMAX_YMAX,EI_YMAX_ZMIN);
            }
            else
            {
                // plus-sign configuration, add branch point to tessellation
                kTable.insert(FI_YMAX,
                    kTable.getVertex(EI_YMAX_ZMIN).m_fX,
                    kTable.getVertex(EI_XMIN_YMAX).m_fY,
                    kTable.getVertex(EI_XMIN_YMAX).m_fZ);

                // add edges sharing the branch point
                kTable.insert(EI_XMIN_YMAX,FI_YMAX);
                kTable.insert(EI_XMAX_YMAX,FI_YMAX);
                kTable.insert(EI_YMAX_ZMIN,FI_YMAX);
                kTable.insert(EI_YMAX_ZMAX,FI_YMAX);
            }
            break;
        }
        }
    }

    /**
     * Compute the edges that connect the vertices on the zmin face of a
     * 1-by-1-by-1 voxel that are found by getVertices.  An additional vertex
     * is added in the case when the face intersects the level surface in a
     * plus-sign configuration.  That vertex is the branch point of the plus.
     *
     * @param kBox the monoregion box
     * @param iType the bit-flag whose bits indicate which edges of the box
     *    are intersected by the level surface
     * @param kTable the vertex-edge table representing the surface-box
     *    intersection as a wireframe
     */
    private void getZMinEdgesS (ASC_OctBox kBox, int iType,
        ASC_VertexTable kTable)
    {
        int iFaceType = 0;
        if ( (iType & EB_XMIN_ZMIN) != 0 ) iFaceType |= 0x01;
        if ( (iType & EB_XMAX_ZMIN) != 0 ) iFaceType |= 0x02;
        if ( (iType & EB_YMIN_ZMIN) != 0 ) iFaceType |= 0x04;
        if ( (iType & EB_YMAX_ZMIN) != 0 ) iFaceType |= 0x08;

        switch ( iFaceType )
        {
        case  0: return;
        case  3: kTable.insert(EI_XMIN_ZMIN,EI_XMAX_ZMIN); break;
        case  5: kTable.insert(EI_XMIN_ZMIN,EI_YMIN_ZMIN); break;
        case  6: kTable.insert(EI_XMAX_ZMIN,EI_YMIN_ZMIN); break;
        case  9: kTable.insert(EI_XMIN_ZMIN,EI_YMAX_ZMIN); break;
        case 10: kTable.insert(EI_XMAX_ZMIN,EI_YMAX_ZMIN); break;
        case 12: kTable.insert(EI_YMIN_ZMIN,EI_YMAX_ZMIN); break;
        case 15:
        {
            // four vertices, one per edge, need to disambiguate
            int i = kBox.m_iX0 + m_iSize*(kBox.m_iY0 + m_iSize*kBox.m_iZ0);
            float fF00 = m_afData[i];  // F(x,y,z)
            i++;
            float fF10 = m_afData[i];  // F(x+1,y,z)
            i += m_iSize;
            float fF11 = m_afData[i];  // F(x+1,y+1,z)
            i--;
            float fF01 = m_afData[i];  // F(x,y+1,z)
            float fDet = fF00*fF11 - fF01*fF10;

            if ( fDet > 0.0f )
            {
                // disjoint hyperbolic segments, pair <P0,P2>, <P1,P3>
                kTable.insert(EI_XMIN_ZMIN,EI_YMIN_ZMIN);
                kTable.insert(EI_XMAX_ZMIN,EI_YMAX_ZMIN);
            }
            else if ( fDet < 0.0f )
            {
                // disjoint hyperbolic segments, pair <P0,P3>, <P1,P2>
                kTable.insert(EI_XMIN_ZMIN,EI_YMAX_ZMIN);
                kTable.insert(EI_XMAX_ZMIN,EI_YMIN_ZMIN);
            }
            else
            {
                // plus-sign configuration, add branch point to tessellation
                kTable.insert(FI_ZMIN,
                    kTable.getVertex(EI_YMIN_ZMIN).m_fX,
                    kTable.getVertex(EI_XMIN_ZMIN).m_fY,
                    kTable.getVertex(EI_XMIN_ZMIN).m_fZ);

                // add edges sharing the branch point
                kTable.insert(EI_XMIN_ZMIN,FI_ZMIN);
                kTable.insert(EI_XMAX_ZMIN,FI_ZMIN);
                kTable.insert(EI_YMIN_ZMIN,FI_ZMIN);
                kTable.insert(EI_YMAX_ZMIN,FI_ZMIN);
            }
            break;
        }
        }
    }

    /**
     * Compute the edges that connect the vertices on the zmax face of a
     * 1-by-1-by-1 voxel that are found by getVertices.  An additional vertex
     * is added in the case when the face intersects the level surface in a
     * plus-sign configuration.  That vertex is the branch point of the plus.
     *
     * @param kBox the monoregion box
     * @param iType the bit-flag whose bits indicate which edges of the box
     *    are intersected by the level surface
     * @param kTable the vertex-edge table representing the surface-box
     *    intersection as a wireframe
     */
    private void getZMaxEdgesS (ASC_OctBox kBox, int iType,
        ASC_VertexTable kTable)
    {
        int iFaceType = 0;
        if ( (iType & EB_XMIN_ZMAX) != 0 ) iFaceType |= 0x01;
        if ( (iType & EB_XMAX_ZMAX) != 0 ) iFaceType |= 0x02;
        if ( (iType & EB_YMIN_ZMAX) != 0 ) iFaceType |= 0x04;
        if ( (iType & EB_YMAX_ZMAX) != 0 ) iFaceType |= 0x08;

        switch ( iFaceType )
        {
        case  0: return;
        case  3: kTable.insert(EI_XMIN_ZMAX,EI_XMAX_ZMAX); break;
        case  5: kTable.insert(EI_XMIN_ZMAX,EI_YMIN_ZMAX); break;
        case  6: kTable.insert(EI_XMAX_ZMAX,EI_YMIN_ZMAX); break;
        case  9: kTable.insert(EI_XMIN_ZMAX,EI_YMAX_ZMAX); break;
        case 10: kTable.insert(EI_XMAX_ZMAX,EI_YMAX_ZMAX); break;
        case 12: kTable.insert(EI_YMIN_ZMAX,EI_YMAX_ZMAX); break;
        case 15:
        {
            // four vertices, one per edge, need to disambiguate
            int i = kBox.m_iX0 + m_iSize*(kBox.m_iY0 + m_iSize*kBox.m_iZ1);
            float fF00 = m_afData[i];  // F(x,y,z)
            i++;
            float fF10 = m_afData[i];  // F(x+1,y,z)
            i += m_iSize;
            float fF11 = m_afData[i];  // F(x+1,y+1,z)
            i--;
            float fF01 = m_afData[i];  // F(x,y+1,z)
            float fDet = fF00*fF11 - fF01*fF10;

            if ( fDet > 0.0f )
            {
                // disjoint hyperbolic segments, pair <P0,P2>, <P1,P3>
                kTable.insert(EI_XMIN_ZMAX,EI_YMIN_ZMAX);
                kTable.insert(EI_XMAX_ZMAX,EI_YMAX_ZMAX);
            }
            else if ( fDet < 0.0f )
            {
                // disjoint hyperbolic segments, pair <P0,P3>, <P1,P2>
                kTable.insert(EI_XMIN_ZMAX,EI_YMAX_ZMAX);
                kTable.insert(EI_XMAX_ZMAX,EI_YMIN_ZMAX);
            }
            else
            {
                // plus-sign configuration, add branch point to tessellation
                kTable.insert(FI_ZMAX,
                    kTable.getVertex(EI_YMIN_ZMAX).m_fX,
                    kTable.getVertex(EI_XMIN_ZMAX).m_fY,
                    kTable.getVertex(EI_XMIN_ZMAX).m_fZ);

                // add edges sharing the branch point
                kTable.insert(EI_XMIN_ZMAX,FI_ZMAX);
                kTable.insert(EI_XMAX_ZMAX,FI_ZMAX);
                kTable.insert(EI_YMIN_ZMAX,FI_ZMAX);
                kTable.insert(EI_YMAX_ZMAX,FI_ZMAX);
            }
            break;
        }
        }
    }

    /**
     * Compute the edges that connect the vertices on the xmin face of a
     * nonvoxel box that are found by getVertices.  An additional vertex
     * is added in the case when the face intersects the level surface in a
     * plus-sign configuration.  That vertex is the branch point of the plus.
     *
     * @param kBox the monoregion box
     * @param iType the bit-flag whose bits indicate which edges of the box
     *    are intersected by the level surface
     * @param kTable the vertex-edge table representing the surface-box
     *    intersection as a wireframe
     */
    private void getXMinEdgesM (ASC_OctBox kBox, int iType,
        ASC_VertexTable kTable)
    {
        int iFaceType = 0;
        if ( (iType & EB_XMIN_YMIN) != 0 ) iFaceType |= 0x01;
        if ( (iType & EB_XMIN_YMAX) != 0 ) iFaceType |= 0x02;
        if ( (iType & EB_XMIN_ZMIN) != 0 ) iFaceType |= 0x04;
        if ( (iType & EB_XMIN_ZMAX) != 0 ) iFaceType |= 0x08;

        int iEnd0, iEnd1;
        switch ( iFaceType )
        {
        case  0: return;
        case  3: iEnd0 = EI_XMIN_YMIN; iEnd1 = EI_XMIN_YMAX; break;
        case  5: iEnd0 = EI_XMIN_YMIN; iEnd1 = EI_XMIN_ZMIN; break;
        case  6: iEnd0 = EI_XMIN_ZMIN; iEnd1 = EI_XMIN_YMAX; break;
        case  9: iEnd0 = EI_XMIN_YMIN; iEnd1 = EI_XMIN_ZMAX; break;
        case 10: iEnd0 = EI_XMIN_ZMAX; iEnd1 = EI_XMIN_YMAX; break;
        case 12: iEnd0 = EI_XMIN_ZMIN; iEnd1 = EI_XMIN_ZMAX; break;
        // should not get here
        default: iEnd0 = -1;  iEnd1 = -1; break;
        }

        ASC_Vertex3.ms_iSortOn = 2;
        TreeSet kVSet = new TreeSet();
        ASC_LinearMergeTree kMerge;
        int iRoot;

        for (int iZ = kBox.m_iZ0+1; iZ < kBox.m_iZ1; iZ++)
        {
            kMerge = m_aakYMerge[kBox.m_iX0][iZ];
            if ( kMerge.isZeroEdge(kBox.m_iLY)
            ||   kMerge.hasZeroSubedge(kBox.m_iLY) )
            {
                iRoot = kMerge.getZeroBase(kBox.m_iLY);
                // assert( iRoot != -1 );
                kVSet.add(new ASC_Vertex3(
                    (float)kBox.m_iX0,
                    getYInterp(kBox.m_iX0,iRoot,iZ),
                    (float)iZ));
            }
        }

        for (int iY = kBox.m_iY0+1; iY < kBox.m_iY1; iY++)
        {
            kMerge = m_aakZMerge[kBox.m_iX0][iY];
            if ( kMerge.isZeroEdge(kBox.m_iLZ)
            ||   kMerge.hasZeroSubedge(kBox.m_iLZ) )
            {
                iRoot = kMerge.getZeroBase(kBox.m_iLZ);
                // assert( iRoot != -1 );
                kVSet.add(new ASC_Vertex3(
                    (float)kBox.m_iX0,
                    (float)iY,
                    getZInterp(kBox.m_iX0,iY,iRoot)));
            }
        }

        // add subdivision
        if ( kVSet.size() == 0 )
        {
            kTable.insert(iEnd0,iEnd1);
            return;
        }

        double dV0y = Math.floor(((ASC_Vertex3)kVSet.first()).m_fY);
        double dV1y = Math.floor(((ASC_Vertex3)kVSet.last()).m_fY);
        double dE0y = Math.floor(kTable.getVertex(iEnd0).m_fY);
        double dE1y = Math.floor(kTable.getVertex(iEnd1).m_fY);
        if ( dE1y <= dV0y && dV1y <= dE0y )
        {
            int iSave = iEnd0;
            iEnd0 = iEnd1;
            iEnd1 = iSave;
        }
        // else:  assert( dE0y <= dV0y && dV1y <= dE1y );

        int iV0 = kTable.getVertexQuantity(), iV1 = iV0;

        // add vertices
        Iterator kIter = kVSet.iterator();
        while ( kIter.hasNext() )
        {
            ASC_Vertex3 kV = (ASC_Vertex3)kIter.next();
            kTable.insert(kV.m_fX,kV.m_fY,kV.m_fZ);
        }

        // add edges
        kTable.insert(iEnd0,iV1++);
        int iMax = kVSet.size()-1;
        for (int i = 1; i <= iMax; i++)
            kTable.insert(iV0++,iV1++);
        kTable.insert(iV0,iEnd1);
    }

    /**
     * Compute the edges that connect the vertices on the xmax face of a
     * nonvoxel box that are found by getVertices.  An additional vertex
     * is added in the case when the face intersects the level surface in a
     * plus-sign configuration.  That vertex is the branch point of the plus.
     *
     * @param kBox the monoregion box
     * @param iType the bit-flag whose bits indicate which edges of the box
     *    are intersected by the level surface
     * @param kTable the vertex-edge table representing the surface-box
     *    intersection as a wireframe
     */
    private void getXMaxEdgesM (ASC_OctBox kBox, int iType,
        ASC_VertexTable kTable)
    {
        int iFaceType = 0;
        if ( (iType & EB_XMAX_YMIN) != 0 ) iFaceType |= 0x01;
        if ( (iType & EB_XMAX_YMAX) != 0 ) iFaceType |= 0x02;
        if ( (iType & EB_XMAX_ZMIN) != 0 ) iFaceType |= 0x04;
        if ( (iType & EB_XMAX_ZMAX) != 0 ) iFaceType |= 0x08;

        int iEnd0, iEnd1;
        switch ( iFaceType )
        {
        case  0: return;
        case  3: iEnd0 = EI_XMAX_YMIN; iEnd1 = EI_XMAX_YMAX; break;
        case  5: iEnd0 = EI_XMAX_YMIN; iEnd1 = EI_XMAX_ZMIN; break;
        case  6: iEnd0 = EI_XMAX_ZMIN; iEnd1 = EI_XMAX_YMAX; break;
        case  9: iEnd0 = EI_XMAX_YMIN; iEnd1 = EI_XMAX_ZMAX; break;
        case 10: iEnd0 = EI_XMAX_ZMAX; iEnd1 = EI_XMAX_YMAX; break;
        case 12: iEnd0 = EI_XMAX_ZMIN; iEnd1 = EI_XMAX_ZMAX; break;
        // should not get here
        default: iEnd0 = -1;  iEnd1 = -1; break;
        }

        ASC_Vertex3.ms_iSortOn = 2;
        TreeSet kVSet = new TreeSet();
        ASC_LinearMergeTree kMerge;
        int iRoot;

        for (int iZ = kBox.m_iZ0+1; iZ < kBox.m_iZ1; iZ++)
        {
            kMerge = m_aakYMerge[kBox.m_iX1][iZ];
            if ( kMerge.isZeroEdge(kBox.m_iLY)
            ||   kMerge.hasZeroSubedge(kBox.m_iLY) )
            {
                iRoot = kMerge.getZeroBase(kBox.m_iLY);
                // assert( iRoot != -1 );
                kVSet.add(new ASC_Vertex3(
                    (float)kBox.m_iX1,
                    getYInterp(kBox.m_iX1,iRoot,iZ),
                    (float)iZ));
            }
        }

        for (int iY = kBox.m_iY0+1; iY < kBox.m_iY1; iY++)
        {
            kMerge = m_aakZMerge[kBox.m_iX1][iY];
            if ( kMerge.isZeroEdge(kBox.m_iLZ)
            ||   kMerge.hasZeroSubedge(kBox.m_iLZ) )
            {
                iRoot = kMerge.getZeroBase(kBox.m_iLZ);
                // assert( iRoot != -1 );
                kVSet.add(new ASC_Vertex3(
                    (float)kBox.m_iX1,
                    (float)iY,
                    getZInterp(kBox.m_iX1,iY,iRoot)));
            }
        }

        // add subdivision
        if ( kVSet.size() == 0 )
        {
            kTable.insert(iEnd0,iEnd1);
            return;
        }

        double dV0y = Math.floor(((ASC_Vertex3)kVSet.first()).m_fY);
        double dV1y = Math.floor(((ASC_Vertex3)kVSet.last()).m_fY);
        double dE0y = Math.floor(kTable.getVertex(iEnd0).m_fY);
        double dE1y = Math.floor(kTable.getVertex(iEnd1).m_fY);
        if ( dE1y <= dV0y && dV1y <= dE0y )
        {
            int iSave = iEnd0;
            iEnd0 = iEnd1;
            iEnd1 = iSave;
        }
        // else:  assert( dE0y <= dV0y && dV1y <= dE1y );

        int iV0 = kTable.getVertexQuantity(), iV1 = iV0;

        // add vertices
        Iterator kIter = kVSet.iterator();
        while ( kIter.hasNext() )
        {
            ASC_Vertex3 kV = (ASC_Vertex3)kIter.next();
            kTable.insert(kV.m_fX,kV.m_fY,kV.m_fZ);
        }

        // add edges
        kTable.insert(iEnd0,iV1++);
        int iMax = kVSet.size()-1;
        for (int i = 1; i <= iMax; i++)
            kTable.insert(iV0++,iV1++);
        kTable.insert(iV0,iEnd1);
    }

    /**
     * Compute the edges that connect the vertices on the ymin face of a
     * nonvoxel box that are found by getVertices.  An additional vertex
     * is added in the case when the face intersects the level surface in a
     * plus-sign configuration.  That vertex is the branch point of the plus.
     *
     * @param kBox the monoregion box
     * @param iType the bit-flag whose bits indicate which edges of the box
     *    are intersected by the level surface
     * @param kTable the vertex-edge table representing the surface-box
     *    intersection as a wireframe
     */
    private void getYMinEdgesM (ASC_OctBox kBox, int iType,
        ASC_VertexTable kTable)
    {
        int iFaceType = 0;
        if ( (iType & EB_XMIN_YMIN) != 0 ) iFaceType |= 0x01;
        if ( (iType & EB_XMAX_YMIN) != 0 ) iFaceType |= 0x02;
        if ( (iType & EB_YMIN_ZMIN) != 0 ) iFaceType |= 0x04;
        if ( (iType & EB_YMIN_ZMAX) != 0 ) iFaceType |= 0x08;

        int iEnd0, iEnd1;
        switch ( iFaceType )
        {
        case  0: return;
        case  3: iEnd0 = EI_XMIN_YMIN; iEnd1 = EI_XMAX_YMIN; break;
        case  5: iEnd0 = EI_XMIN_YMIN; iEnd1 = EI_YMIN_ZMIN; break;
        case  6: iEnd0 = EI_YMIN_ZMIN; iEnd1 = EI_XMAX_YMIN; break;
        case  9: iEnd0 = EI_XMIN_YMIN; iEnd1 = EI_YMIN_ZMAX; break;
        case 10: iEnd0 = EI_YMIN_ZMAX; iEnd1 = EI_XMAX_YMIN; break;
        case 12: iEnd0 = EI_YMIN_ZMIN; iEnd1 = EI_YMIN_ZMAX; break;
        // should not get here
        default: iEnd0 = -1;  iEnd1 = -1; break;
        }

        ASC_Vertex3.ms_iSortOn = 1;
        TreeSet kVSet = new TreeSet();
        ASC_LinearMergeTree kMerge;
        int iRoot;

        for (int iX = kBox.m_iX0+1; iX < kBox.m_iX1; iX++)
        {
            kMerge = m_aakZMerge[iX][kBox.m_iY0];
            if ( kMerge.isZeroEdge(kBox.m_iLZ)
            ||   kMerge.hasZeroSubedge(kBox.m_iLZ) )
            {
                iRoot = kMerge.getZeroBase(kBox.m_iLZ);
                // assert( iRoot != -1 );
                kVSet.add(new ASC_Vertex3(
                    (float)iX,
                    (float)kBox.m_iY0,
                    getZInterp(iX,kBox.m_iY0,iRoot)));
            }
        }

        for (int iZ = kBox.m_iZ0+1; iZ < kBox.m_iZ1; iZ++)
        {
            kMerge = m_aakXMerge[kBox.m_iY0][iZ];
            if ( kMerge.isZeroEdge(kBox.m_iLX)
            ||   kMerge.hasZeroSubedge(kBox.m_iLX) )
            {
                iRoot = kMerge.getZeroBase(kBox.m_iLX);
                // assert( iRoot != -1 );
                kVSet.add(new ASC_Vertex3(
                    getXInterp(iRoot,kBox.m_iY0,iZ),
                    (float)kBox.m_iY0,
                    (float)iZ));
            }
        }

        // add subdivision
        if ( kVSet.size() == 0 )
        {
            kTable.insert(iEnd0,iEnd1);
            return;
        }

        double dV0z = Math.floor(((ASC_Vertex3)kVSet.first()).m_fZ);
        double dV1z = Math.floor(((ASC_Vertex3)kVSet.last()).m_fZ);
        double dE0z = Math.floor(kTable.getVertex(iEnd0).m_fZ);
        double dE1z = Math.floor(kTable.getVertex(iEnd1).m_fZ);
        if ( dE1z <= dV0z && dV1z <= dE0z )
        {
            int iSave = iEnd0;
            iEnd0 = iEnd1;
            iEnd1 = iSave;
        }
        // else:  assert( dE0z <= dV0z && dV1z <= dE1z );

        int iV0 = kTable.getVertexQuantity(), iV1 = iV0;

        // add vertices
        Iterator kIter = kVSet.iterator();
        while ( kIter.hasNext() )
        {
            ASC_Vertex3 kV = (ASC_Vertex3)kIter.next();
            kTable.insert(kV.m_fX,kV.m_fY,kV.m_fZ);
        }

        // add edges
        kTable.insert(iEnd0,iV1++);
        int iMax = kVSet.size()-1;
        for (int i = 1; i <= iMax; i++)
            kTable.insert(iV0++,iV1++);
        kTable.insert(iV0,iEnd1);
    }

    /**
     * Compute the edges that connect the vertices on the ymax face of a
     * nonvoxel box that are found by getVertices.  An additional vertex
     * is added in the case when the face intersects the level surface in a
     * plus-sign configuration.  That vertex is the branch point of the plus.
     *
     * @param kBox the monoregion box
     * @param iType the bit-flag whose bits indicate which edges of the box
     *    are intersected by the level surface
     * @param kTable the vertex-edge table representing the surface-box
     *    intersection as a wireframe
     */
    private void getYMaxEdgesM (ASC_OctBox kBox, int iType,
        ASC_VertexTable kTable)
    {
        int iFaceType = 0;
        if ( (iType & EB_XMIN_YMAX) != 0 ) iFaceType |= 0x01;
        if ( (iType & EB_XMAX_YMAX) != 0 ) iFaceType |= 0x02;
        if ( (iType & EB_YMAX_ZMIN) != 0 ) iFaceType |= 0x04;
        if ( (iType & EB_YMAX_ZMAX) != 0 ) iFaceType |= 0x08;

        int iEnd0, iEnd1;
        switch ( iFaceType )
        {
        case  0: return;
        case  3: iEnd0 = EI_XMIN_YMAX; iEnd1 = EI_XMAX_YMAX; break;
        case  5: iEnd0 = EI_XMIN_YMAX; iEnd1 = EI_YMAX_ZMIN; break;
        case  6: iEnd0 = EI_YMAX_ZMIN; iEnd1 = EI_XMAX_YMAX; break;
        case  9: iEnd0 = EI_XMIN_YMAX; iEnd1 = EI_YMAX_ZMAX; break;
        case 10: iEnd0 = EI_YMAX_ZMAX; iEnd1 = EI_XMAX_YMAX; break;
        case 12: iEnd0 = EI_YMAX_ZMIN; iEnd1 = EI_YMAX_ZMAX; break;
        // should not get here
        default: iEnd0 = -1;  iEnd1 = -1; break;
        }

        ASC_Vertex3.ms_iSortOn = 1;
        TreeSet kVSet = new TreeSet();
        ASC_LinearMergeTree kMerge;
        int iRoot;

        for (int iX = kBox.m_iX0+1; iX < kBox.m_iX1; iX++)
        {
            kMerge = m_aakZMerge[iX][kBox.m_iY1];
            if ( kMerge.isZeroEdge(kBox.m_iLZ)
            ||   kMerge.hasZeroSubedge(kBox.m_iLZ) )
            {
                iRoot = kMerge.getZeroBase(kBox.m_iLZ);
                // assert( iRoot != -1 );
                kVSet.add(new ASC_Vertex3(
                    (float)iX,
                    (float)kBox.m_iY1,
                    getZInterp(iX,kBox.m_iY1,iRoot)));
            }
        }

        for (int iZ = kBox.m_iZ0+1; iZ < kBox.m_iZ1; iZ++)
        {
            kMerge = m_aakXMerge[kBox.m_iY1][iZ];
            if ( kMerge.isZeroEdge(kBox.m_iLX)
            ||   kMerge.hasZeroSubedge(kBox.m_iLX) )
            {
                iRoot = kMerge.getZeroBase(kBox.m_iLX);
                // assert( iRoot != -1 );
                kVSet.add(new ASC_Vertex3(
                    getXInterp(iRoot,kBox.m_iY1,iZ),
                    (float)kBox.m_iY1,
                    (float)iZ));
            }
        }

        // add subdivision
        if ( kVSet.size() == 0 )
        {
            kTable.insert(iEnd0,iEnd1);
            return;
        }

        double dV0z = Math.floor(((ASC_Vertex3)kVSet.first()).m_fZ);
        double dV1z = Math.floor(((ASC_Vertex3)kVSet.last()).m_fZ);
        double dE0z = Math.floor(kTable.getVertex(iEnd0).m_fZ);
        double dE1z = Math.floor(kTable.getVertex(iEnd1).m_fZ);
        if ( dE1z <= dV0z && dV1z <= dE0z )
        {
            int iSave = iEnd0;
            iEnd0 = iEnd1;
            iEnd1 = iSave;
        }
        // else:  assert( dE0z <= dV0z && dV1z <= dE1z );

        int iV0 = kTable.getVertexQuantity(), iV1 = iV0;

        // add vertices
        Iterator kIter = kVSet.iterator();
        while ( kIter.hasNext() )
        {
            ASC_Vertex3 kV = (ASC_Vertex3)kIter.next();
            kTable.insert(kV.m_fX,kV.m_fY,kV.m_fZ);
        }

        // add edges
        kTable.insert(iEnd0,iV1++);
        int iMax = kVSet.size()-1;
        for (int i = 1; i <= iMax; i++)
            kTable.insert(iV0++,iV1++);
        kTable.insert(iV0,iEnd1);
    }

    /**
     * Compute the edges that connect the vertices on the zmin face of a
     * nonvoxel box that are found by getVertices.  An additional vertex
     * is added in the case when the face intersects the level surface in a
     * plus-sign configuration.  That vertex is the branch point of the plus.
     *
     * @param kBox the monoregion box
     * @param iType the bit-flag whose bits indicate which edges of the box
     *    are intersected by the level surface
     * @param kTable the vertex-edge table representing the surface-box
     *    intersection as a wireframe
     */
    private void getZMinEdgesM (ASC_OctBox kBox, int iType,
        ASC_VertexTable kTable)
    {
        int iFaceType = 0;
        if ( (iType & EB_XMIN_ZMIN) != 0 ) iFaceType |= 0x01;
        if ( (iType & EB_XMAX_ZMIN) != 0 ) iFaceType |= 0x02;
        if ( (iType & EB_YMIN_ZMIN) != 0 ) iFaceType |= 0x04;
        if ( (iType & EB_YMAX_ZMIN) != 0 ) iFaceType |= 0x08;

        int iEnd0, iEnd1;
        switch ( iFaceType )
        {
        case  0: return;
        case  3: iEnd0 = EI_XMIN_ZMIN; iEnd1 = EI_XMAX_ZMIN; break;
        case  5: iEnd0 = EI_XMIN_ZMIN; iEnd1 = EI_YMIN_ZMIN; break;
        case  6: iEnd0 = EI_YMIN_ZMIN; iEnd1 = EI_XMAX_ZMIN; break;
        case  9: iEnd0 = EI_XMIN_ZMIN; iEnd1 = EI_YMAX_ZMIN; break;
        case 10: iEnd0 = EI_YMAX_ZMIN; iEnd1 = EI_XMAX_ZMIN; break;
        case 12: iEnd0 = EI_YMIN_ZMIN; iEnd1 = EI_YMAX_ZMIN; break;
        // should not get here
        default: iEnd0 = -1;  iEnd1 = -1; break;
        }

        ASC_Vertex3.ms_iSortOn = 0;
        TreeSet kVSet = new TreeSet();
        ASC_LinearMergeTree kMerge;
        int iRoot;

        for (int iX = kBox.m_iX0+1; iX < kBox.m_iX1; iX++)
        {
            kMerge = m_aakYMerge[iX][kBox.m_iZ0];
            if ( kMerge.isZeroEdge(kBox.m_iLY)
            ||   kMerge.hasZeroSubedge(kBox.m_iLY) )
            {
                iRoot = kMerge.getZeroBase(kBox.m_iLY);
                // assert( iRoot != -1 );
                kVSet.add(new ASC_Vertex3(
                    (float)iX,
                    getYInterp(iX,iRoot,kBox.m_iZ0),
                    (float)kBox.m_iZ0));
            }
        }

        for (int iY = kBox.m_iY0+1; iY < kBox.m_iY1; iY++)
        {
            kMerge = m_aakXMerge[iY][kBox.m_iZ0];
            if ( kMerge.isZeroEdge(kBox.m_iLX)
            ||   kMerge.hasZeroSubedge(kBox.m_iLX) )
            {
                iRoot = kMerge.getZeroBase(kBox.m_iLX);
                // assert( iRoot != -1 );
                kVSet.add(new ASC_Vertex3(
                    getXInterp(iRoot,iY,kBox.m_iZ0),
                    (float)iY,
                    (float)kBox.m_iZ0));
            }
        }

        // add subdivision
        if ( kVSet.size() == 0 )
        {
            kTable.insert(iEnd0,iEnd1);
            return;
        }

        double dV0x = Math.floor(((ASC_Vertex3)kVSet.first()).m_fX);
        double dV1x = Math.floor(((ASC_Vertex3)kVSet.last()).m_fX);
        double dE0x = Math.floor(kTable.getVertex(iEnd0).m_fX);
        double dE1x = Math.floor(kTable.getVertex(iEnd1).m_fX);
        if ( dE1x <= dV0x && dV1x <= dE0x )
        {
            int iSave = iEnd0;
            iEnd0 = iEnd1;
            iEnd1 = iSave;
        }
        // else:  assert( dE0x <= dV0x && dV1x <= dE1x );

        int iV0 = kTable.getVertexQuantity(), iV1 = iV0;

        // add vertices
        Iterator kIter = kVSet.iterator();
        while ( kIter.hasNext() )
        {
            ASC_Vertex3 kV = (ASC_Vertex3)kIter.next();
            kTable.insert(kV.m_fX,kV.m_fY,kV.m_fZ);
        }

        // add edges
        kTable.insert(iEnd0,iV1++);
        int iMax = kVSet.size()-1;
        for (int i = 1; i <= iMax; i++)
            kTable.insert(iV0++,iV1++);
        kTable.insert(iV0,iEnd1);
    }

    /**
     * Compute the edges that connect the vertices on the zmax face of a
     * nonvoxel box that are found by getVertices.  An additional vertex
     * is added in the case when the face intersects the level surface in a
     * plus-sign configuration.  That vertex is the branch point of the plus.
     *
     * @param kBox the monoregion box
     * @param iType the bit-flag whose bits indicate which edges of the box
     *    are intersected by the level surface
     * @param kTable the vertex-edge table representing the surface-box
     *    intersection as a wireframe
     */
    private void getZMaxEdgesM (ASC_OctBox kBox, int iType,
        ASC_VertexTable kTable)
    {
        int iFaceType = 0;
        if ( (iType & EB_XMIN_ZMAX) != 0 ) iFaceType |= 0x01;
        if ( (iType & EB_XMAX_ZMAX) != 0 ) iFaceType |= 0x02;
        if ( (iType & EB_YMIN_ZMAX) != 0 ) iFaceType |= 0x04;
        if ( (iType & EB_YMAX_ZMAX) != 0 ) iFaceType |= 0x08;

        int iEnd0, iEnd1;
        switch ( iFaceType )
        {
        case  0: return;
        case  3: iEnd0 = EI_XMIN_ZMAX; iEnd1 = EI_XMAX_ZMAX; break;
        case  5: iEnd0 = EI_XMIN_ZMAX; iEnd1 = EI_YMIN_ZMAX; break;
        case  6: iEnd0 = EI_YMIN_ZMAX; iEnd1 = EI_XMAX_ZMAX; break;
        case  9: iEnd0 = EI_XMIN_ZMAX; iEnd1 = EI_YMAX_ZMAX; break;
        case 10: iEnd0 = EI_YMAX_ZMAX; iEnd1 = EI_XMAX_ZMAX; break;
        case 12: iEnd0 = EI_YMIN_ZMAX; iEnd1 = EI_YMAX_ZMAX; break;
        // should not get here
        default: iEnd0 = -1; iEnd1 = -1; break;
        }

        ASC_Vertex3.ms_iSortOn = 0;
        TreeSet kVSet = new TreeSet();
        ASC_LinearMergeTree kMerge;
        int iRoot;

        for (int iX = kBox.m_iX0+1; iX < kBox.m_iX1; iX++)
        {
            kMerge = m_aakYMerge[iX][kBox.m_iZ1];
            if ( kMerge.isZeroEdge(kBox.m_iLY)
            ||   kMerge.hasZeroSubedge(kBox.m_iLY) )
            {
                iRoot = kMerge.getZeroBase(kBox.m_iLY);
                // assert( iRoot != -1 );
                kVSet.add(new ASC_Vertex3(
                    (float)iX,
                    getYInterp(iX,iRoot,kBox.m_iZ1),
                    (float)kBox.m_iZ1));
            }
        }

        for (int iY = kBox.m_iY0+1; iY < kBox.m_iY1; iY++)
        {
            kMerge = m_aakXMerge[iY][kBox.m_iZ1];
            if ( kMerge.isZeroEdge(kBox.m_iLX)
            ||   kMerge.hasZeroSubedge(kBox.m_iLX) )
            {
                iRoot = kMerge.getZeroBase(kBox.m_iLX);
                // assert( iRoot != -1 );
                kVSet.add(new ASC_Vertex3(
                    getXInterp(iRoot,iY,kBox.m_iZ1),
                    (float)iY,
                    (float)kBox.m_iZ1));
            }
        }

        // add subdivision
        if ( kVSet.size() == 0 )
        {
            kTable.insert(iEnd0,iEnd1);
            return;
        }

        double dV0x = Math.floor(((ASC_Vertex3)kVSet.first()).m_fX);
        double dV1x = Math.floor(((ASC_Vertex3)kVSet.last()).m_fX);
        double dE0x = Math.floor(kTable.getVertex(iEnd0).m_fX);
        double dE1x = Math.floor(kTable.getVertex(iEnd1).m_fX);
        if ( dE1x <= dV0x && dV1x <= dE0x )
        {
            int iSave = iEnd0;
            iEnd0 = iEnd1;
            iEnd1 = iSave;
        }
        // else:  assert( dE0x <= dV0x && dV1x <= dE1x );

        int iV0 = kTable.getVertexQuantity(), iV1 = iV0;

        // add vertices
        Iterator kIter = kVSet.iterator();
        while ( kIter.hasNext() )
        {
            ASC_Vertex3 kV = (ASC_Vertex3)kIter.next();
            kTable.insert(kV.m_fX,kV.m_fY,kV.m_fZ);
        }

        // add edges
        kTable.insert(iEnd0,iV1++);
        int iMax = kVSet.size()-1;
        for (int i = 1; i <= iMax; i++)
            kTable.insert(iV0++,iV1++);
        kTable.insert(iV0,iEnd1);
    }


}
