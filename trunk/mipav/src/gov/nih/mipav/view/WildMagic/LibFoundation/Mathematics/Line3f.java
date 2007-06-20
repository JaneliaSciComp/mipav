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

/** The line is represented as P+t*D where P is the line origin and D is
 * a unit-length direction vector.  The user must ensure that the
 * direction vector satisfies this condition.
 */
public class Line3f
{
    /** construction */
    public Line3f () {}  // uninitialized
    public Line3f ( Vector3f rkOrigin, Vector3f rkDirection)
    {
        Origin = new Vector3f(rkOrigin);
        Direction = new Vector3f(rkDirection);
    }
    public Line3f ( Line3f rkLine)
    {
        Origin = new Vector3f(rkLine.Origin);
        Direction = new Vector3f(rkLine.Direction);
    }

    public Vector3f Origin, Direction;
}
