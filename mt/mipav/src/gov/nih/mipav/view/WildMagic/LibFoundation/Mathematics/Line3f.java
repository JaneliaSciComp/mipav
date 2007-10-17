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

/** The line is represented as P+t*D where P is the line origin and D is
 * a unit-length direction vector.  The user must ensure that the
 * direction vector satisfies this condition.
 */
public class Line3f
{
    /** construction */
    public Line3f () {}  // uninitialized

    /** construction 
     * @param krOrigin, line origin
     * @param rkDirection, line direction unit-length
     */
    public Line3f ( Vector3f rkOrigin, Vector3f rkDirection)
    {
        Origin = new Vector3f(rkOrigin);
        Direction = new Vector3f(rkDirection);
    }

    /** copy construction 
     * @param rkLine, line 
     */
    public Line3f ( Line3f rkLine)
    {
        Origin = new Vector3f(rkLine.Origin);
        Direction = new Vector3f(rkLine.Direction);
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

    /** Line Origin and Direction vectors The line is represented as P+t*D
    * where P is the line origin and D is a unit-length direction vector.  The
    * user must ensure that the direction vector satisfies this condition.
    */
    public Vector3f Origin, Direction;
}
