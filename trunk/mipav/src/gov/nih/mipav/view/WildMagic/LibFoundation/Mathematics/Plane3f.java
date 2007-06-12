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

/** The plane is represented as Dot(N,X) = c where N is a unit-length
 * normal vector, c is the plane constant, and X is any point on the
 * plane.  The user must ensure that the normal vector satisfies this
 * condition.
 */
public class Plane3f
{

    public Plane3f () {}  // uninitialized
    public Plane3f ( Plane3f rkPlane)
    {
        Normal = new Vector3f( rkPlane.Normal );
        Constant = rkPlane.Constant;
    }

    /** specify N and c directly */
    public Plane3f (Vector3f rkNormal, float fConstant)
    {
        Normal = new Vector3f( rkNormal );
        Constant = fConstant;
    }

    /** N is specified, c = Dot(N,P) where P is on the plane */
    public Plane3f (Vector3f rkNormal, Vector3f rkP)
    {
        Normal = new Vector3f( rkNormal );
        Constant = rkNormal.Dot(rkP);
    }

    /** N = Cross(P1-P0,P2-P0)/Length(Cross(P1-P0,P2-P0)), c = Dot(N,P0) where
     * P0, P1, P2 are points on the plane.
     */
    public Plane3f (Vector3f rkP0, Vector3f rkP1,
                    Vector3f rkP2)
    {
        Vector3f kEdge1 = rkP1.sub( rkP0 );
        Vector3f kEdge2 = rkP2.sub( rkP0 );
        Normal = kEdge1.UnitCross(kEdge2);
        Constant = Normal.Dot(rkP0);
    }

    /** The "positive side" of the plane is the half space to which the plane
     * normal points.  The "negative side" is the other half space.  The
     * function returns +1 for the positive side, -1 for the negative side,
     * and 0 for the point being on the plane.
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
     */
    public float DistanceTo (Vector3f rkP)
    {
        return Normal.Dot(rkP) - Constant;
    }

    public Vector3f Normal;
    public float Constant;
}
