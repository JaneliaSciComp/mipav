// Geometric Tools, Inc.
// http://www.geometrictools.com
// Copyright (c) 1998-2006.  All Rights Reserved
//
// The Wild Magic Version 4 Foundation Library source code is supplied
// under the terms of the license agreement
//     http://www.geometrictools.com/License/Wm4FoundationLicense.pdf
// and may not be copied or disclosed except in accordance with the terms
// of that agreement.
//
// Version: 4.0.0 (2006/06/28)

package gov.nih.mipav.view.WildMagic.LibFoundation.Mathematics;

/** The segment is represented as P+t*D, where P is the segment origin,
 * D is a unit-length direction vector and |t| <= e.  The value e is
 * referred to as the extent of the segment.  The end points of the
 * segment are P-e*D and P+e*D.  The user must ensure that the direction
 * vector is unit-length.  The representation for a segment is analogous
 * to that for an oriented bounding box.  P is the center, D is the
 * axis direction, and e is the extent.
 */
public class Segment3f
{
    // construction
    public Segment3f () {}  // uninitialized
    public Segment3f (Vector3f rkOrigin, Vector3f rkDirection, float fExtent)
    {
        Origin = new Vector3f(rkOrigin);
        Direction = new Vector3f(rkDirection);
        Extent = fExtent;
    }
    public Segment3f (Segment3f rkSegment)
    {
        Origin = new Vector3f(rkSegment.Origin);
        Direction = new Vector3f(rkSegment.Direction);
        Extent = rkSegment.Extent;
    }

    // end points
    public Vector3f GetPosEnd ()  // P+e*D
    {
        return Origin.add( Direction.scale(Extent) );
    }

    public Vector3f GetNegEnd ()  // P-e*D
    {
        return Origin.sub( Direction.scale(Extent) );
    }

    public Vector3f Origin, Direction;
    public float Extent;
}
