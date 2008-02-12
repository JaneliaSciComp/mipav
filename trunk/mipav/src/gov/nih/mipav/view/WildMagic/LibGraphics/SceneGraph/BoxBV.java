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
// Version: 4.0.1 (2006/08/07)

package gov.nih.mipav.view.WildMagic.LibGraphics.SceneGraph;

import gov.nih.mipav.view.WildMagic.LibFoundation.Containment.*;
import gov.nih.mipav.view.WildMagic.LibFoundation.Mathematics.*;
import gov.nih.mipav.view.WildMagic.LibFoundation.Intersection.*;
import gov.nih.mipav.view.WildMagic.LibGraphics.ObjectSystem.*;

public class BoxBV extends BoundingVolume
                           //implements StreamInterface
{
    // Construction and destruction.
    public BoxBV ()  // center (0,0,0), axes (1,0,0),(0,1,0),(0,0,1), extents 1,1,1
    {
        m_kBox = new Box3f(Vector3f.ZERO,Vector3f.UNIT_X,Vector3f.UNIT_Y,Vector3f.UNIT_Z,
                           1.0f,1.0f,1.0f);
    }

    public BoxBV (final Box3f rkBox)
    {
        m_kBox = rkBox;
    }

    public void dispose () {}

    public BoundingVolume.BVType GetBVType ()
    {
        return BoundingVolume.BVType.BV_BOX;
    }

    // all bounding volumes must define a center and radius
    public void SetCenter (final Vector3f rkCenter)
    {
        m_kBox.Center = new Vector3f(rkCenter);
    }

    public void SetRadius (float fRadius)
    {
        m_kBox.Extent[0] = fRadius;
        m_kBox.Extent[1] = fRadius;
        m_kBox.Extent[2] = fRadius;
    }

    public Vector3f GetCenter ()
    {
        return m_kBox.Center;
    }

    public float GetRadius ()
    {
        float fRadius = m_kBox.Extent[0];
        if (fRadius < m_kBox.Extent[1])
        {
            fRadius = m_kBox.Extent[1];
        }
        if (fRadius < m_kBox.Extent[2])
        {
            fRadius = m_kBox.Extent[2];
        }

        return fRadius;
    }

    public void SetBox (Box3f kBox)
    {
        m_kBox = kBox;
    }
    
    public Box3f GetBox ()
    {
        return m_kBox;
    }

    // Compute a box that contains all the points.
    public void ComputeFromData (final Vector3f[] pkVertices)
    {
        if (pkVertices != null)
        {
            int iVQuantity = pkVertices.length;
            m_kBox = ContBox3f.ContOrientedBox(iVQuantity,pkVertices);
        }
    }

    public void ComputeFromData (final VertexBuffer pkVBuffer) {}

    // Transform the box (model-to-world conversion).
    public void TransformBy (final Transformation rkTransform,
                             BoundingVolume pkResult)
    {
        Box3f rkTarget = ((BoxBV)pkResult).m_kBox;
        rkTarget.Center = rkTransform.ApplyForward(m_kBox.Center);
        for (int i = 0; i < 3; i++)
        {
            rkTransform.GetRotate().mult(m_kBox.Axis[i], rkTarget.Axis[i]);
            rkTarget.Extent[i] = rkTransform.GetNorm()*m_kBox.Extent[i];
        }
    }


    // Determine if the bounding volume is one side of the plane, the other
    // side, or straddles the plane.  If it is on the positive side (the
    // side to which the normal points), the return value is +1.  If it is
    // on the negative side, the return value is -1.  If it straddles the
    // plane, the return value is 0.
    public int WhichSide (final Plane3f rkPlane)
    {
        float fProjCenter = rkPlane.Normal.Dot(m_kBox.Center) - rkPlane.Constant;
        float fAbs0 = Math.abs(rkPlane.Normal.Dot(m_kBox.Axis[0]));
        float fAbs1 = Math.abs(rkPlane.Normal.Dot(m_kBox.Axis[1]));
        float fAbs2 = Math.abs(rkPlane.Normal.Dot(m_kBox.Axis[2]));
        float fProjRadius = m_kBox.Extent[0]*fAbs0 + m_kBox.Extent[1]*fAbs1 +
            m_kBox.Extent[2]*fAbs2;
        
        if (fProjCenter - fProjRadius >= 0.0f)
        {
            return +1;
        }

        if (fProjCenter + fProjRadius <= 0.0f)
        {
            return -1;
        }

        return 0;
    }

    // Test for intersection of linear component and bound (points of
    // intersection not computed).  The linear component is parameterized by
    // P + t*D, where P is a point on the component and D is a unit-length
    // direction.  The interval [tmin,tmax] is
    //   line:     tmin = -Mathf::MAX_REAL, tmax = Mathf::MAX_REAL
    //   ray:      tmin = 0.0f, tmax = Mathf::MAX_REAL
    //   segment:  tmin = 0.0f, tmax > 0.0f
    public boolean TestIntersection (final Vector3f rkOrigin,
                                     final Vector3f rkDirection, float fTMin, float fTMax)
    {
        if (fTMin == -Float.MAX_VALUE)
        {
            Line3f kLine = new Line3f(rkOrigin,rkDirection);
            IntrLine3Box3f kTestLB = new IntrLine3Box3f(kLine,m_kBox);
            return kTestLB.Test();
        }

        assert(fTMin == 0.0f);
        if (fTMax == Float.MAX_VALUE)
        {
            Ray3f kRay = new Ray3f(rkOrigin,rkDirection);
            IntrRay3Box3f kTestRB = new IntrRay3Box3f(kRay,m_kBox);
            return kTestRB.Test();
        }

        assert(fTMax > 0.0f);
        Segment3f kSegment = new Segment3f();
        kSegment.Extent = 0.5f*fTMax;
        kSegment.Origin = rkOrigin.add( rkDirection.scale(kSegment.Extent) );
        kSegment.Direction = rkDirection;
        IntrSegment3Box3f kTestSB = new IntrSegment3Box3f(kSegment,m_kBox,true);
        return kTestSB.Test();
    }

    // Test for intersection of the two bounds.
    public boolean TestIntersection (final BoundingVolume pkInput)
    {
        IntrBox3Box3f kTestBB = new IntrBox3Box3f(m_kBox,((BoxBV)pkInput).m_kBox);
        return kTestBB.Test();
    }

    // Make a copy of the bounding volume.
    public void CopyFrom (final BoundingVolume pkInput)
    {
        m_kBox = ((BoxBV)pkInput).m_kBox;
    }

    // Change the current box so that it contains itself and the input.
    public void GrowToContain (final BoundingVolume pkInput)
    {
        m_kBox = ContBox3f.MergeBoxes(m_kBox,((BoxBV)pkInput).m_kBox);
    }

    // test for containment of a point
    public boolean Contains (final Vector3f rkPoint)
    {
        return ContBox3f.InBox(rkPoint,m_kBox);
    }

    protected Box3f m_kBox;
}
