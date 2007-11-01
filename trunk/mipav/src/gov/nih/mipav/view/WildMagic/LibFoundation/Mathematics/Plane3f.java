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

/** The plane is represented as Dot(N,X) = c where N is a unit-length
 * normal vector, c is the plane constant, and X is any point on the
 * plane.  The user must ensure that the normal vector satisfies this
 * condition.
 */
public class Plane3f
{

    /** Default constructor, uninitialized.
     */
    public Plane3f () {}

    /** Copy constructor.
     * @param rkPlane, plane to copy.
     */
    public Plane3f ( Plane3f rkPlane)
    {
        Normal.SetData( rkPlane.Normal );
        Constant = rkPlane.Constant;
    }

    /** specify N and c directly
     * @param rkNormal, normal vector
     * @param fConstant, plane constant
     */
    public Plane3f (Vector3f rkNormal, float fConstant)
    {
        Normal.SetData( rkNormal );
        Constant = fConstant;
    }

    /** N is specified, c = Dot(N,P) where P is on the plane
     * @param rkNormal, normal vector
     * @param rkP, point on the plane
     */
    public Plane3f (Vector3f rkNormal, Vector3f rkP)
    {
        Normal.SetData( rkNormal );
        Constant = rkNormal.Dot(rkP);
    }

    /** N = Cross(P1-P0,P2-P0)/Length(Cross(P1-P0,P2-P0)), c = Dot(N,P0) where
     * P0, P1, P2 are points on the plane.
     * @param rkP0, point0
     * @param rkP1, point1
     * @param rkP2, point2
     */
    public Plane3f (Vector3f rkP0, Vector3f rkP1,
                    Vector3f rkP2)
    {
        Vector3f kEdge1 = new Vector3f();
        rkP1.sub( rkP0, kEdge1 );
        Vector3f kEdge2 = new Vector3f();
        rkP2.sub( rkP0, kEdge2 );
        Normal.SetData(kEdge1);
        Normal.UnitCrossEquals(kEdge2);
        Constant = Normal.Dot(rkP0);
        kEdge1 = null;
        kEdge2 = null;
    }

    /**
     * delete memory
     */
    public void finalize()
    {
        if ( Normal != null )
        {
            Normal.finalize();
            Normal = null;
        }
    }

    /** The "positive side" of the plane is the half space to which the plane
     * normal points.  The "negative side" is the other half space.  The
     * function returns +1 for the positive side, -1 for the negative side,
     * and 0 for the point being on the plane.
     * @param rkQ, point
     * @return +1 for the positive side, -1 for the negative side, and 0 for
     * the point being on the plane.
     */
    public int WhichSide (Vector3f rkQ)
    {
        float fDistance = DistanceTo(rkQ);

        if (fDistance < (float)0.0)
        {
            return -1;
        }

        if (fDistance > (float)0.0)
        {
            return +1;
        }

        return 0;
    }

    /** Compute d = Dot(N,Q)-c where N is the plane normal and c is the plane
     * constant.  This is a signed distance.  The sign of the return value is
     * positive if the point is on the positive side of the plane, negative if
     * the point is on the negative side, and zero if the point is on the
     * plane.
     * @param rkP, point
     * @return distance.
     */
    public float DistanceTo (Vector3f rkP)
    {
        return Normal.Dot(rkP) - Constant;
    }

    /** Plane normal: */
    public Vector3f Normal = new Vector3f();
    /** Plane constant: */
    public float Constant;
}
