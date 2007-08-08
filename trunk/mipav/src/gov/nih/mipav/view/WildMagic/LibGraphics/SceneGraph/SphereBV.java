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

package gov.nih.mipav.view.WildMagic.LibGraphics.SceneGraph;

import gov.nih.mipav.view.WildMagic.LibFoundation.Mathematics.*;
import gov.nih.mipav.view.WildMagic.LibFoundation.Intersection.*;
import gov.nih.mipav.view.WildMagic.LibGraphics.ObjectSystem.*;

public class SphereBV extends BoundingVolume
    implements StreamInterface
{

   // construction
    public SphereBV ()  // center (0,0,0), radius 0
    {
        m_kSphere = new Sphere3f(Vector3f.ZERO,0.0f);
    }

    public SphereBV (Sphere3f rkSphere)
    {
        m_kSphere = rkSphere;
    }

    public void finalize()
    {
        if ( m_kSphere != null )
        {
            m_kSphere.finalize();
            m_kSphere = null;
        }
        super.finalize();
    }

    public BVType GetBVType ()
    {
        return BoundingVolume.BVType.BV_SPHERE;
    }

    // all bounding volumes must define a center and radius
    public void SetCenter (Vector3f rkCenter)
    {
        m_kSphere.Center = rkCenter;
    }

    public void SetRadius (float fRadius)
    {
        m_kSphere.Radius = fRadius;
    }

    public Vector3f GetCenter ()
    {
        return m_kSphere.Center;
    }

    public float GetRadius ()
    {
        return m_kSphere.Radius;
    }

    public Sphere3f Sphere ()
    {
        return m_kSphere;
    }

    public Sphere3f GetSphere ()
    {
        return m_kSphere;
    }


    // Compute a sphere that contains all the points.
    //public void ComputeFromData (const Vector3fArray* pkVertices)
    public void ComputeFromData (final Vector3f[] pkVertices)
    {
        if (pkVertices != null)
        {
            int iVQuantity = pkVertices.length;
            m_kSphere = Sphere3f.ContSphereAverage(iVQuantity,pkVertices);
        }
    }

    public void ComputeFromData (final VertexBuffer pkVBuffer)
    {
        // TO DO.  This is a hack for now.  What to do if positions are (x,y,z,w)
        // with w not equal to 1?
        if (pkVBuffer != null)
        {
            int iPQuantity = pkVBuffer.GetVertexQuantity();

            m_kSphere.Center = new Vector3f(Vector3f.ZERO);
            m_kSphere.Radius = 0.0f;
            int i;
            for (i = 0; i < iPQuantity; i++)
            {
                //m_kSphere.Center += pkVBuffer->Position3(i);
                m_kSphere.Center.addEquals( pkVBuffer.Position3(i) );
            }
            //m_kSphere.Center /= (float)iPQuantity;
            m_kSphere.Center.divEquals( (float)iPQuantity );

            for (i = 0; i < iPQuantity; i++)
            {
                Vector3f kDiff = pkVBuffer.Position3(i).sub( m_kSphere.Center );
                float fRadiusSqr = kDiff.SquaredLength();
                if (fRadiusSqr > m_kSphere.Radius)
                {
                    m_kSphere.Radius = fRadiusSqr;
                }
            }

            m_kSphere.Radius = (float)Math.sqrt(m_kSphere.Radius);
        }
    }

    // Transform the sphere (model-to-world conversion).
    public void TransformBy ( Transformation rkTransform,
                              BoundingVolume pkResult)
    {
        Sphere3f rkTarget = ((SphereBV)pkResult).m_kSphere;
        rkTarget.Center = rkTransform.ApplyForward(m_kSphere.Center);
        rkTarget.Radius = rkTransform.GetNorm()*m_kSphere.Radius;
    }


    // Determine if the bounding volume is one side of the plane, the other
    // side, or straddles the plane.  If it is on the positive side (the
    // side to which the normal points), the return value is +1.  If it is
    // on the negative side, the return value is -1.  If it straddles the
    // plane, the return value is 0.
    public int WhichSide (Plane3f rkPlane)
    {
        float fDistance = rkPlane.DistanceTo(m_kSphere.Center);

        if (fDistance <= -m_kSphere.Radius)
        {
            return -1;
        }

        if (fDistance >= m_kSphere.Radius)
        {
            return +1;
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
    public boolean TestIntersection ( Vector3f rkOrigin,
                                      Vector3f rkDirection, float fTMin, float fTMax)
    {
        if (fTMin == Float.MIN_VALUE)
        {
            Line3f kLine = new Line3f(rkOrigin,rkDirection);
            IntrLine3Sphere3f kIntrLS = new IntrLine3Sphere3f(kLine,m_kSphere);
            return kIntrLS.Test();
        }

        assert(fTMin == 0.0f);
        if (fTMax == Float.MAX_VALUE)
        {
            Ray3f kRay = new Ray3f(rkOrigin,rkDirection);
            IntrRay3Sphere3f kIntrRS = new IntrRay3Sphere3f(kRay,m_kSphere);
            return kIntrRS.Test();
        }

        assert(fTMax > fTMin);
        Segment3f kSegment = new Segment3f();
        kSegment.Extent = 0.5f*fTMax;
        //kSegment.Origin = rkOrigin + kSegment.Extent*rkDirection;
        kSegment.Origin = rkOrigin.add( rkDirection.scale(kSegment.Extent) );
        kSegment.Direction = rkDirection;
        IntrSegment3Sphere3f kIntrSegS = new IntrSegment3Sphere3f(kSegment,m_kSphere);
        return kIntrSegS.Test();
    }

    // Test for intersection of the two bounds.
    public boolean TestIntersection ( BoundingVolume pkInput)
    {
        Sphere3f kSphere = ((SphereBV)pkInput).m_kSphere;
        IntrSphere3Sphere3f kIntrSS = new IntrSphere3Sphere3f(m_kSphere, kSphere);
        return kIntrSS.Test();
    }

    // Make a copy of the bounding volume.
    public void CopyFrom (BoundingVolume pkInput)
    {
        m_kSphere = ((SphereBV)pkInput).m_kSphere;
    }

    // Change the current sphere so that it is the minimum volume sphere that
    // contains the input sphere as well as its old sphere.
    public void GrowToContain ( BoundingVolume pkInput)
    {
        m_kSphere = Sphere3f.MergeSpheres(m_kSphere,((SphereBV)pkInput).m_kSphere);
    }

    // test for containment of a point
    public boolean Contains (Vector3f rkPoint)
    {
        return Sphere3f.InSphere(rkPoint,m_kSphere);
    }

    protected Sphere3f m_kSphere;

    public void Load (Stream rkStream, Stream.Link pkLink)
    {
        super.Load(rkStream,pkLink);

        // native data
        rkStream.Read(m_kSphere.Center);
        m_kSphere.Radius = rkStream.ReadFloat();
    }

    public void Link (Stream rkStream, Stream.Link pkLink)
    {
        super.Link(rkStream,pkLink);
    }

    public boolean Register (Stream rkStream)
    {
        return super.Register(rkStream);
    }

    public void Save (Stream rkStream)
    {
        super.Save(rkStream);

        // native data
        rkStream.Write(m_kSphere.Center);
        rkStream.Write(m_kSphere.Radius);
    }

    public int GetDiskUsed (StreamVersion rkVersion)
    {
        return super.GetDiskUsed(rkVersion) +
            3*Stream.SIZEOF_FLOAT + //sizeof(m_kSphere);
            Stream.SIZEOF_FLOAT;
    }

    public StringTree SaveStrings (final String acTitle)
    {
        StringTree pkTree = new StringTree();
        // strings
        pkTree.Append(StringTree.Format("SphereBV",GetName()));
        pkTree.Append(super.SaveStrings(null));
        pkTree.Append(StringTree.Format("center =",m_kSphere.Center));
        pkTree.Append(StringTree.Format("radius =",m_kSphere.Radius));
        return pkTree;
    }
}
