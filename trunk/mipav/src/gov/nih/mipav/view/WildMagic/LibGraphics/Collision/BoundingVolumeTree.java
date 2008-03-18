//Wild Magic Source Code
//David Eberly
//http://www.geometrictools.com
//Copyright (c) 1998-2007

//This library is free software; you can redistribute it and/or modify it
//under the terms of the GNU Lesser General Public License as published by
//the Free Software Foundation; either version 2.1 of the License, or (at
//your option) any later version.  The license is available for reading at
//either of the locations:
//http://www.gnu.org/copyleft/lgpl.html
//http://www.geometrictools.com/License/WildMagicLicense.pdf

//Version: 4.0.0 (2006/06/28)

package gov.nih.mipav.view.WildMagic.LibGraphics.Collision;

import java.util.Arrays;
import java.util.Comparator;
import java.util.Vector;

import gov.nih.mipav.view.WildMagic.LibFoundation.Intersection.*;
import gov.nih.mipav.view.WildMagic.LibFoundation.Mathematics.*;
import gov.nih.mipav.view.WildMagic.LibGraphics.SceneGraph.*;

public abstract class BoundingVolumeTree implements Comparator<BoundingVolumeTree.ProjectionInfo>
{
    // abstract base class.
    public void disposeLocal ()
    {
        m_kTriangles = null;
        m_pkLChild = null;
        m_pkRChild = null;
    }

    // tree topology
    public BoundingVolumeTree GetLChild ()
    {
        return m_pkLChild;
    }

    public BoundingVolumeTree GetRChild ()
    {
        return m_pkRChild;
    }

    public boolean IsInteriorNode ()
    {
        return ((m_pkLChild != null) || (m_pkRChild != null));
    }

    public boolean IsLeafNode ()
    {
        return ((m_pkLChild == null) && (m_pkRChild == null));
    }

    // member access
    public final TriMesh GetMesh ()
    {
        return m_pkMesh;
    }
    public final BoundingVolume GetWorldBound ()
    {
        return m_spkWorldBound;
    }

    public final BoundingVolume GetModelBound ()
    {
        return m_spkModelBound;
    }
    public int GetTriangleQuantity ()
    {
        return m_iTriangleQuantity;
    }
    public int GetTriangle (int i)
    {
        return m_kTriangles.get(i);
    }
    public final Vector<Integer> GetTriangles ()
    {
        return m_kTriangles;
    }

    public void UpdateWorldBound ()
    {
        m_spkModelBound.TransformBy(m_pkMesh.World,m_spkWorldBound);
    }


    protected BoundingVolumeTree ()
    {
        m_pkMesh = null;
        m_pkLChild = null;
        m_pkRChild = null;
        m_iTriangleQuantity = 0;
        m_kTriangles = null;
    }    

    protected BoundingVolumeTree (final TriMesh pkMesh)
    {
        m_pkMesh = pkMesh;
        m_pkLChild = null;
        m_pkRChild = null;
        m_iTriangleQuantity = 0;
        m_kTriangles = null;
    }

    protected BoundingVolumeTree (BoundingVolume.BVType eBVType, final TriMesh pkMesh )
    {
        this( eBVType, pkMesh, 1, false );
    }

    protected BoundingVolumeTree (BoundingVolume.BVType eBVType, final TriMesh pkMesh,
            int iMaxTrisPerLeaf, boolean bStoreInteriorTris)
    {
        m_pkMesh = pkMesh;
        // Centroids of triangles are used for splitting a mesh.  The centroids
        // are projected onto a splitting axis and sorted.  The split is based
        // on the median of the projections.
        int iTQuantity = m_pkMesh.IBuffer.GetIndexQuantity()/3;
        int[] aiIndex = m_pkMesh.IBuffer.GetData();
        Vector3f[] akCentroid = new Vector3f[iTQuantity];
        float fOneThird = 1.0f/3.0f;
        int iT, i;
        for (iT = 0, i = 0; iT < iTQuantity; iT++)
        {
            int i0 = aiIndex[i++];
            int i1 = aiIndex[i++];
            int i2 = aiIndex[i++];
//          akCentroid[iT] = fOneThird*(
//          pkMesh.VBuffer.Position3(i0) +
//          pkMesh.VBuffer.Position3(i1) +
//          pkMesh.VBuffer.Position3(i2));
            akCentroid[iT] = pkMesh.VBuffer.GetPosition3(i0);
            akCentroid[iT].addEquals(pkMesh.VBuffer.GetPosition3(i1));
            akCentroid[iT].addEquals(pkMesh.VBuffer.GetPosition3(i2));
            akCentroid[iT].scaleEquals(fOneThird);
        }

        // Initialize binary-tree arrays for storing triangle indices.  These
        // are used to store the indices when the mesh is split.
        int[] aiISplit = new int[iTQuantity];
        int[] aiOSplit = new int[iTQuantity];
        for (iT = 0; iT < iTQuantity; iT++)
        {
            aiISplit[iT] = iT;
        }

        BuildTree(eBVType,iMaxTrisPerLeaf,bStoreInteriorTris,akCentroid,0,
                iTQuantity-1,aiISplit,aiOSplit);

        akCentroid = null;
        aiISplit = null;
        aiOSplit = null;
/*
        if (bStoreInteriorTris)
        {
            float fEpsilon = 1e-05f;
            boolean bSuccess = ContainsLeafData(pkMesh.VBuffer,aiIndex,fEpsilon);
            if ( !bSuccess )
            {
                System.err.println("ContainsLeafData failed");
            }
        }
        */
    }


    protected void BuildTree (BoundingVolume.BVType eBVType, int iMaxTrisPerLeaf, boolean bStoreInteriorTris,
            final Vector3f[] akCentroid, int i0, int i1, int[] aiISplit,
            int[] aiOSplit)
    {
        assert(i0 <= i1);
        Line3f kLine = new Line3f();
        try
        {
            if ( eBVType == BoundingVolume.BVType.BV_SPHERE )
            {
                return;
            }
            else if ( eBVType == BoundingVolume.BVType.BV_BOX )
            {
                m_spkModelBound = BoxBVTree.CreateModelBound(m_pkMesh,i0,i1,aiISplit, kLine);
                //System.err.println("CreateModelBound " + (i1 - i0 + 1) + " " + aiISplit[i0] + " " + aiISplit[i1] );
                m_spkWorldBound = new BoxBV();
            }
        } catch (OutOfMemoryError x) {
            System.err.println("out of memory CreateModelBound");
        }

        if (i1 - i0 < iMaxTrisPerLeaf)
        { 
            try
            {
                // leaf node
                m_iTriangleQuantity = i1 - i0 + 1;
                m_kTriangles = new Vector<Integer>();
                for ( int i = 0; i < m_iTriangleQuantity; i++ )
                {
                    m_kTriangles.add(i, aiISplit[i0+i]);
                }
                m_pkLChild = null;
                m_pkRChild = null;

                //System.err.println("Leaf " + (i1 - i0 + 1) + " " + aiISplit[i0] + " " + aiISplit[m_iTriangleQuantity-1] );
            } catch (OutOfMemoryError x) {
                System.err.println("out of memory iMaxTrisPerLeaf");
            }
        }
        else
        {            

            int[] j01 = new int[2];
            try
            {

                // interior node
                if (bStoreInteriorTris)
                {
                    m_iTriangleQuantity = i1 - i0 + 1;
                    m_kTriangles = new Vector<Integer>();
                    for ( int i = 0; i < m_iTriangleQuantity; i++ )
                    {
                        m_kTriangles.add(i, aiISplit[i0+i]);
                    }

                    //System.err.println("Interior "  + (i1 - i0 + 1) + " " + aiISplit[i0] + " " + aiISplit[m_iTriangleQuantity-1] );
                }
                else
                {
                    m_iTriangleQuantity = 0;
                    m_kTriangles = null;
                }
                SplitTriangles(akCentroid,i0,i1,aiISplit,j01,aiOSplit,kLine);
                if ( eBVType == BoundingVolume.BVType.BV_BOX )
                {
                    m_pkLChild = new BoxBVTree(m_pkMesh);
                    m_pkLChild.BuildTree(eBVType,iMaxTrisPerLeaf,bStoreInteriorTris,
                            akCentroid,i0,j01[0],aiOSplit,aiISplit);

                    m_pkRChild = new BoxBVTree(m_pkMesh);
                    m_pkRChild.BuildTree(eBVType,iMaxTrisPerLeaf,bStoreInteriorTris,
                            akCentroid,j01[1],i1,aiOSplit,aiISplit);
                }
            } catch (OutOfMemoryError x) {
                System.err.println("out of memory else " + i0 + " " + j01[0] + " " + j01[1] + " " + i1 );
            }
        }

    }

    protected static void SplitTriangles (final Vector3f[] akCentroid, int i0, int i1,
            int[] aiISplit, int[] rj01, int[] aiOSplit,
            final Line3f rkLine)
    {
        BoundingVolumeTree kTree = new BoxBVTree();
        // project onto specified line
        int iQuantity = i1 - i0 + 1;
        ProjectionInfo[] akInfo = new ProjectionInfo[iQuantity];
        int i, j;
        for (i = i0, j = 0; i <= i1; i++, j++)
        {
            int iTriangle = aiISplit[i];
            Vector3f kDiff = akCentroid[iTriangle].sub( rkLine.Origin );
            akInfo[j] = kTree.new ProjectionInfo();
            akInfo[j].m_iTriangle = iTriangle;
            akInfo[j].m_fProjection = rkLine.Direction.Dot(kDiff);
        }
        // find median of projections by sorting
        try
        {
            Arrays.sort( akInfo, kTree);
        } catch (OutOfMemoryError x) {
            System.err.println("out of memory sort");
        }
        int iMedian = (iQuantity-1)/2;

        // partition the triangles by the median
        for (j = 0, rj01[0] = i0-1; j <= iMedian; j++)
        {
            aiOSplit[++rj01[0]] = akInfo[j].m_iTriangle;
        }
        for (rj01[1] = i1+1; j < iQuantity; j++)
        {
            aiOSplit[--rj01[1]] = akInfo[j].m_iTriangle;
        }

        akInfo = null;
    }


    // for quick-sort of centroid projections on axes
    public class ProjectionInfo
    {
        public ProjectionInfo() {}
        public int m_iTriangle;
        public float m_fProjection;
    };

    public int compare ( ProjectionInfo pInfo0, ProjectionInfo pInfo1)
    {
        if (pInfo0.m_fProjection < pInfo1.m_fProjection)
        {
            return -1;
        }

        if (pInfo0.m_fProjection > pInfo1.m_fProjection)
        {
            return +1;
        }

        return 0;
    }


    // model bounding volume factory
    //typedef BoundingVolume* (*CreatorM)(const TriMesh*,int,int,int*,Line3f&);
    //static CreatorM ms_aoCreateModelBound[BoundingVolume::BV_QUANTITY];

    // world bounding volume factory
    //typedef BoundingVolume* (*CreatorW)(void);
    //static CreatorW ms_aoCreateWorldBound[BoundingVolume::BV_QUANTITY];

    // mesh and bounds
    protected TriMesh m_pkMesh;
    protected BoundingVolume m_spkModelBound;
    protected BoundingVolume m_spkWorldBound;

    // binary tree representation
    protected BoundingVolumeTree m_pkLChild = null;
    protected BoundingVolumeTree m_pkRChild = null;

    // If bStoreInteriorTris is set to 'false' in the constructor, the
    // interior nodes set the triangle quantity to zero and the array to null.
    // Leaf nodes set the quantity to the number of triangles at that node (1
    // if iMaxTrianglesPerLeaf was set to 1) and allocate an array of
    // triangle indices that are relative to the input mesh of the top level
    // constructor.
    //
    // If bStoreInteriorTris is set to 'true', the interior nodes also save
    // the triangle quantity and array of triangle indices for the mesh that
    // the node represents.
    protected int m_iTriangleQuantity;
    protected Vector<Integer> m_kTriangles;

    // Checks to see if the vertices corresponding to the triangle mesh at
    // at each tree node are contained by the model space bounding volume.
    // The call is only made when _DEBUG_TEST has been defined *and* when
    // bStoreInteriorTris is set to 'true'.
    protected boolean ContainsLeafData (final VertexBuffer pkVBuffer, final int[] aiIndex,
            float fEpsilon)
    {
        if (m_pkLChild != null)
        {
            if (!m_pkLChild.ContainsLeafData(pkVBuffer,aiIndex,fEpsilon))
            {
                return false;
            }
        }

        if (m_pkRChild != null)
        {
            if (!m_pkRChild.ContainsLeafData(pkVBuffer,aiIndex,fEpsilon))
            {
                return false;
            }
        }

        //System.err.println("Test Leaf " + m_iTriangleQuantity +  " " + m_aiTriangle[0] + " " + m_aiTriangle[m_iTriangleQuantity-1]);
        for (int iT = 0; iT < m_iTriangleQuantity; iT++)
        {
            int j = 3*m_kTriangles.get(iT);
            for (int i = 0; i < 3; i++)
            {
                Vector3f kPoint = pkVBuffer.GetPosition3(aiIndex[j++]);
                if (!m_spkModelBound.Contains(kPoint))
                {
                    return false;
                }
            }
        }

        return true;
    }
}
