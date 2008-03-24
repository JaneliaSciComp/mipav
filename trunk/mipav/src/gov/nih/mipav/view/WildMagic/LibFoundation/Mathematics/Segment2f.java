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
// Ported to Java by Alexandra Bokinsky, PhD, Geometric Tools, Inc. (July 2007)
//

package gov.nih.mipav.view.WildMagic.LibFoundation.Mathematics;

public class Segment2f
{
    // The segment is represented as P+t*D, where P is the segment origin,
    // D is a unit-length direction vector and |t| <= e.  The value e is
    // referred to as the extent of the segment.  The end points of the
    // segment are P-e*D and P+e*D.  The user must ensure that the direction
    // vector is unit-length.  The representation for a segment is analogous
    // to that for an oriented bounding box.  P is the center, D is the
    // axis direction, and e is the extent.

    // construction
    public Segment2f () {}  // uninitialized
    public Segment2f (final Vector2f rkOrigin, final Vector2f rkDirection,
                      float fExtent)
    {
        Origin.SetData(rkOrigin);
        Direction.SetData(rkDirection);
        Extent = fExtent;
    }

    // end points
    public Vector2f GetPosEnd ()  // P+e*D
    {
        return Origin.add( Direction.scale(Extent) );
    }

    public Vector2f GetNegEnd ()  // P-e*D
    {
        return Origin.sub( Direction.scale(Extent) );
    }

    public Vector2f Origin = new Vector2f();
    public Vector2f Direction= new Vector2f();
    public float Extent;
}
