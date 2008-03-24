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

package gov.nih.mipav.view.WildMagic.LibFoundation.Intersection;

import gov.nih.mipav.view.WildMagic.LibFoundation.Mathematics.*;

public class IntrRay2Segment2f
{
    /**
     * The return quantity is 0 if the objects do not intersect, or is 1 or 2 if they do intersect. If 2, then the
     * returned values are the end points of an interval of intersection. If an intersection occurs, the returned values
     * are the parameter values for the point(s) of intersection with respect to the first object parameterized by P+t*D
     * (t in [-inf,+inf] for line, t in [0,+inf] for ray, t in [0,1] for segment). The t parameter for the points of
     * intersection are returned in the afT array which must have length 2.
     *
     * @param   kRay      DOCUMENT ME!
     * @param   kSegment  DOCUMENT ME!
     * @param   afT       DOCUMENT ME!
     *
     * @return  DOCUMENT ME!
     */
    public static int FindIntersection(Ray2f kRay, Segment2f kSegment, float[] afT) {
        Vector2f kDiff = new Vector2f();
        float fD0SqrLen = kRay.Direction.SquaredLength();
        int iQuantity = Find(kRay.Origin, kRay.Direction, kSegment.Origin, kSegment.Direction, kDiff, afT);

        // intersects?
        if (0 != iQuantity) {

            if (iQuantity == 1) {

                if ((afT[0] < 0.0f) || (afT[1] < 0.0f) || (afT[1] > 1.0f)) {

                    // lines intersect, but ray and segment do not
                    iQuantity = 0;
                }
            } else {

                // ray and segment are on the same line
                float fDotRS = kRay.Direction.Dot(kSegment.Direction);
                float fDot0;
                float fDot1;

                if (fDotRS > 0.0f) {
                    fDot0 = kDiff.Dot(kRay.Direction);
                    fDot1 = fDot0 + fDotRS;
                } else {
                    fDot1 = kDiff.Dot(kRay.Direction);
                    fDot0 = fDot1 + fDotRS;
                }

                // compute intersection of [t0,t1] and [0,+infinity]
                if (fDot0 >= 0.0f) {

                    // complete overlap
                    float fInvLen = 1.0f / fD0SqrLen;
                    afT[0] = fDot0 * fInvLen;
                    afT[1] = fDot1 * fInvLen;
                } else if (fDot1 > 0.0f) {

                    // partial overlap
                    afT[0] = 0.0f;
                    afT[1] = fDot1 / fD0SqrLen;
                } else if (fDot1 < 0.0f) {

                    // no overlap
                    iQuantity = 0;
                } else {

                    // overlap at a single end point
                    iQuantity = 1;
                    afT[0] = 0.0f;
                }
            }
        }

        return iQuantity;
    }
    /**
     * Intersection is a solution to P0+s*D0 = P1+t*D1. Rewrite as s*D0 - t*D1 = P1 - P0, a 2x2 system of equations. If
     * D0 = (x0,y0) and D1 = (x1,y1) and P1 - P0 = (c0,c1), then the system is x0*s - x1*t = c0 and y0*s - y1*t = c1.
     * The error tests are relative to the size of the direction vectors, |Cross(D0,D1)| >= e*|D0|*|D1| rather than
     * absolute tests |Cross(D0,D1)| >= e. The quantities P1-P0, |D0|^2, and |D1|^2 are returned for use by calling
     * functions.
     *
     * @param   kP0    DOCUMENT ME!
     * @param   kD0    DOCUMENT ME!
     * @param   kP1    DOCUMENT ME!
     * @param   kD1    DOCUMENT ME!
     * @param   kDiff  DOCUMENT ME!
     * @param   afT    DOCUMENT ME!
     *
     * @return  DOCUMENT ME!
     */
    private static int Find(Vector2f kP0, Vector2f kD0, Vector2f kP1, Vector2f kD1, Vector2f kDiff, float[] afT) {
        float fDet = kD1.Cross(kD0);
        kP1.sub(kP0, kDiff);

        float fD0SqrLen = kD0.SquaredLength();
        int iQuantity;

        if ((fDet * fDet) > (Mathf.EPSILON * fD0SqrLen * kD1.SquaredLength())) {

            // Lines intersect in a single point.  Return both s and t values for
            // use by calling functions.
            float fInvDet = 1.0f / fDet;
            afT[0] = kD1.Cross(kDiff) * fInvDet;
            afT[1] = kD0.Cross(kDiff) * fInvDet;
            iQuantity = 1;
        } else {

            // lines are parallel
            fDet = kD0.Cross(kDiff);

            float fRHS = Mathf.EPSILON * fD0SqrLen * kDiff.SquaredLength();

            if ((fDet * fDet) > fRHS) {

                // lines are disjoint
                iQuantity = 0;
            } else {

                // lines are the same
                iQuantity = 2;
            }
        }

        return iQuantity;
    }


}
