// Wild Magic Source Code
// David Eberly
// http://www.geometrictools.com
// Copyright (c) 1998-2007
//
// This library is free software; you can redistribute it and/or modify it
// under the terms of the GNU Lesser General Public License as published by
// the Free Software Foundation; either version 2.1 of the License, or (at
// your option) any later version.  The license is available for reading at
// either of the locations:
//     http://www.gnu.org/copyleft/lgpl.html
//     http://www.geometrictools.com/License/WildMagicLicense.pdf
//
// Version: 4.0.0 (2006/06/28)
//
// Ported to Java by Alexandra Bokinsky, PhD, Geometric Tools, Inc. (July 2007)
//

package gov.nih.mipav.view.WildMagic.LibGraphics.Collision;

import java.util.Vector;
import gov.nih.mipav.view.WildMagic.LibFoundation.Intersection.*;
import gov.nih.mipav.view.WildMagic.LibFoundation.Mathematics.*;
import gov.nih.mipav.view.WildMagic.LibGraphics.SceneGraph.*;

public class HierarchicalTriMesh extends TriMesh
{
    /** Construction.
     * @param pkVBuffer, vertex buffer.
     * @param pkIBuffer, index buffer.
     */
    public HierarchicalTriMesh (VertexBuffer pkVBuffer, IndexBuffer pkIBuffer)
    {
        super(pkVBuffer,pkIBuffer);
    }

    /** Construction.
     * @param pkVBuffer, vertex buffer.
     * @param pkIBuffer, index buffer.
     */
    public HierarchicalTriMesh (VertexBuffer pkVBuffer, IndexBuffer pkIBuffer, BoundingVolume kBounds)
    {
        super(pkVBuffer,pkIBuffer,kBounds);
    }
    
    /** Construction.
     * @param pkVBuffer, vertex buffer.
     * @param pkIBuffer, index buffer.
     */
    public HierarchicalTriMesh (TriMesh kMesh )
    {
        super(kMesh);
    }
    
    public Vector<PickRecord> TestIntersection (Vector3f rkOrigin,
                                                Vector3f rkDirection, float fTMin, float fTMax)
    {
        TriMesh pkMesh = m_kBoundingTree.GetMesh();

        // Convert the linear component to model-space coordinates.
        Vector3f kOrigin = pkMesh.World.ApplyInverse(rkOrigin);
        Vector3f kDirection = pkMesh.World.InvertVector(rkDirection);
        
        BoundingVolume kBV = m_kBoundingTree.GetModelBound();
        if ( kBV.TestIntersection( kOrigin, kDirection, fTMin, fTMax ) )
        {
            return TestIntersection( m_kBoundingTree, kOrigin, kDirection, fTMin, fTMax );
        }
        return null;
    }

    private Vector<PickRecord> TestIntersection (BoundingVolumeTree kTree,
                                                 Vector3f kOrigin,
                                                 Vector3f kDirection, float fTMin, float fTMax)
    {

        if (kTree.IsInteriorNode())
        {
            Vector<PickRecord> kRecords = new Vector<PickRecord>();
            BoundingVolumeTree kLeft = kTree.GetLChild();
            BoundingVolume kBV = kLeft.GetModelBound();
            if ( kBV.TestIntersection( kOrigin, kDirection, fTMin, fTMax ) )
            {
                Vector<PickRecord> kAddRecords = TestIntersection( kLeft, kOrigin, kDirection, fTMin, fTMax );
                if ( kAddRecords != null )
                {
                    kRecords.addAll( kAddRecords );
                }
            }
            BoundingVolumeTree kRight = kTree.GetRChild();
            kBV = kRight.GetModelBound();
            if ( kBV.TestIntersection( kOrigin, kDirection, fTMin, fTMax ) )
            {
                Vector<PickRecord> kAddRecords = TestIntersection( kRight, kOrigin, kDirection, fTMin, fTMax );
                if ( kAddRecords != null )
                {
                    kRecords.addAll( kAddRecords );
                }
            }
            return kRecords;
        }
        else
        {
            TriMesh pkMesh = kTree.GetMesh();
            Line3f kLine = new Line3f( kOrigin,kDirection);

            Vector<PickRecord> kRecords = null;
            // Compute intersections with the model-space triangles.
            int iTQuantity = kTree.GetTriangleQuantity();
            //System.err.println(iTQuantity);
            Vector<Integer> kTriangles = kTree.GetTriangles();
            for (int i = 0; i < iTQuantity; i++)
            {
                int iV0, iV1, iV2;
                int[] aiTris = new int[3];
                if (!pkMesh.GetTriangle(kTriangles.get(i),aiTris) )
                {
                    continue;
                }

                iV0 = aiTris[0];            iV1 = aiTris[1];            iV2 = aiTris[2];
                Triangle3f kTriangle = new Triangle3f(
                                                      pkMesh.VBuffer.GetPosition3(iV0),
                                                      pkMesh.VBuffer.GetPosition3(iV1),
                                                      pkMesh.VBuffer.GetPosition3(iV2));

                IntrLine3Triangle3f kIntr = new IntrLine3Triangle3f(kLine,kTriangle);
                if (kIntr.Find() && fTMin <= kIntr.GetLineT()
                    &&  kIntr.GetLineT() <= fTMax)
                {
                    PickRecord kRecord = new PickRecord();
                    kRecord.Intersected = pkMesh;
                    kRecord.T = kIntr.GetLineT();
                    kRecord.Triangle = i;
                    kRecord.iV0 = iV0;
                    kRecord.iV1 = iV1;
                    kRecord.iV2 = iV2;
                    kRecord.B0 = kIntr.GetTriB0();
                    kRecord.B1 = kIntr.GetTriB1();
                    kRecord.B2 = kIntr.GetTriB2();
                    if ( kRecords == null )
                    {
                        kRecords = new Vector<PickRecord>();
                    }
                    kRecords.add(kRecord);
                }
            }
            return kRecords;
        }
    }

    public BoundingVolumeTree GetBoundingVolumeTree()
    {
        return m_kBoundingTree;
    }
    
    public void SetBoundingVolumeTree( BoundingVolumeTree kTree )
    {
        m_kBoundingTree = kTree;
    }

    private BoundingVolumeTree m_kBoundingTree = null;

}
