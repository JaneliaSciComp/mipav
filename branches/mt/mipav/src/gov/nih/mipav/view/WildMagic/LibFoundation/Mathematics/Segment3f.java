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
    /** construction -- uninitialized */
    public Segment3f () {}  // uninitialized
    /** construction 
     * @param rkOrigin, segment origin
     * @param rkDirection, segment direction, unit-length
     * @param fExtent, segment extent
     */
    public Segment3f (Vector3f rkOrigin, Vector3f rkDirection, float fExtent)
    {
        Origin.SetData(rkOrigin);
        Direction.SetData(rkDirection);
        Extent = fExtent;
    }

    /**
     * Copy constructor.
     * @param rkSegment, the segment to copy.
     */
    public Segment3f (Segment3f rkSegment)
    {
        Origin.SetData(rkSegment.Origin);
        Direction.SetData(rkSegment.Direction);
        Extent = rkSegment.Extent;
    }

    /**
     * delete memory
     */
    public void finalize()
    {
        Origin.finalize();
        Origin = null;
        Direction.finalize();
        Direction = null;
    }

    /** Returns the positive end point: P+e*D
     * @return P+e*D
     */
    public final void GetPosEnd ( Vector3f kResult )
    {
        Vector3f kDir = new Vector3f(Direction);
        kDir.scaleEquals(Extent);
        Origin.add( kDir, kResult );
        kDir = null;
    }

    /** Returns the negative end point: P-e*D
     * @return P-e*D
     */
    public final void GetNegEnd ( Vector3f kResult )
    {
        Vector3f kDir = new Vector3f(Direction);
        kDir.scaleEquals(Extent);
        Origin.sub( kDir, kResult );
        kDir = null;
    }

    /** Segment origin, and unit-length direction. */
    public Vector3f Origin = new Vector3f(), Direction = new Vector3f();
    /** Segment extent: */
    public float Extent;
}
