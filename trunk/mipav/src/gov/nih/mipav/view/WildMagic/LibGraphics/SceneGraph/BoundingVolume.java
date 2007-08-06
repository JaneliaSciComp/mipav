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
import gov.nih.mipav.view.WildMagic.LibGraphics.ObjectSystem.*;

public abstract class BoundingVolume extends GraphicsObject
    implements StreamInterface
{
    // run-time type information
    public enum BVType
    {
        BV_SPHERE,
        BV_BOX,
        BV_QUANTITY
    };
    public abstract BVType GetBVType ();

    // all bounding volumes must define a center and radius
    public abstract void SetCenter (Vector3f rkCenter);
    public abstract void SetRadius (float fRadius);
    public abstract Vector3f GetCenter ();
    public abstract float GetRadius ();

    // Compute a bounding volume that contains all the points.
    //public abstract void ComputeFromData ( Vector3fArray* pkVertices);
    public abstract void ComputeFromData ( final Vector3f[] pkVertices);
    public abstract void ComputeFromData ( VertexBuffer pkVBuffer);

    // Transform the bounding volume (model-to-world conversion).
    public abstract void TransformBy ( Transformation rkTransform,
                                       BoundingVolume pkResult);

    // Determine if the bounding volume is one side of the plane, the other
    // side, or straddles the plane.  If it is on the positive side (the
    // side to which the normal points), the return value is +1.  If it is
    // on the negative side, the return value is -1.  If it straddles the
    // plane, the return value is 0.
    public abstract int WhichSide ( Plane3f rkPlane);

    // Test for intersection of linear component and bound (points of
    // intersection not computed).  The linear component is parameterized by
    // P + t*D, where P is a point on the component and D is a unit-length
    // direction.  The interval [tmin,tmax] is
    //   line:     tmin = -Mathf::MAX_REAL, tmax = Mathf::MAX_REAL
    //   ray:      tmin = 0.0f, tmax = Mathf::MAX_REAL
    //   segment:  tmin = 0.0f, tmax > 0.0f
    public abstract boolean TestIntersection (Vector3f rkOrigin,
                                              Vector3f rkDirection, float fTMin, float fTMax);

    // Test for intersection of the two bounds.
    public abstract boolean TestIntersection (BoundingVolume pkInput);

    // Make a copy of the bounding volume.
    public abstract void CopyFrom (BoundingVolume pkInput);

    // Change the current bounding volume so that it contains the input
    // bounding volume as well as its old bounding volume.
    public abstract void GrowToContain (BoundingVolume pkInput);

    // test for containment of a point
    public abstract boolean Contains (Vector3f rkPoint);

    // streaming
    public BoundingVolume () {}

    public void Load (Stream rkStream, Stream.Link pkLink)
    {
        super.Load(rkStream,pkLink);
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
    }

    public int GetDiskUsed (StreamVersion rkVersion)
    {
        return super.GetDiskUsed(rkVersion);
    }

    public StringTree SaveStrings (final String acTitle)
    {
        StringTree pkTree = new StringTree();
        pkTree.Append(StringTree.Format("BoundingVolume",GetName()));
        pkTree.Append(super.SaveStrings(null));
        return pkTree;
    }


}
