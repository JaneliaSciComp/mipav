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

public class IntrSegment2Circle2f
{
    /**
     * The value is 0 if the objects do not intersect. Otherwise, the quantity is 1 or 2 and is the number of valid
     * points in The points of intersection are returned in the akPoint array which must have length 2.
     *
     * @param   kSegment  DOCUMENT ME!
     * @param   kCircle   DOCUMENT ME!
     * @param   akPoint   DOCUMENT ME!
     *
     * @return  DOCUMENT ME!
     */
    public static int FindIntersection(Segment2f kSegment, Circle2f kCircle, Vector2f[] akPoint) {
        float[] afT = new float[2];
        int iQuantity = Find(kSegment.Origin, kSegment.Direction, kCircle.Center, kCircle.Radius, afT);

        // intersects?
        if (0 != iQuantity) {

            // reduce root count if line-circle intersections are not on segment
            if (iQuantity == 1) {

                if ((afT[0] < 0.0f) || (afT[0] > 1.0f)) {
                    iQuantity = 0;
                }
            } else {

                if ((afT[1] < 0.0f) || (afT[0] > 1.0f)) {
                    iQuantity = 0;
                } else {

                    if (afT[1] <= 1.0f) {

                        if (afT[0] < 0.0f) {
                            iQuantity = 1;
                            afT[0] = afT[1];
                        }
                    } else {
                        iQuantity = ((afT[0] >= 0.0f) ? 1 : 0);
                    }
                }
            }

            // construct ray-circle points of intersection
            for (int i = 0; i < iQuantity; i++) {
                akPoint[i] = kSegment.Direction.scale(afT[i]);
                akPoint[i].addEquals(kSegment.Origin);
            }
        }

        return iQuantity;
    }

    /**
     * Intersection of a the line P+t*D and the circle |X-C| = R. The t value is a root to the quadratic equation: 0 =
     * |t*D+P-C|^2 - R^2 = |D|^2*t^2 + 2*D.Dot(P-C)*t + |P-C|^2-R^2 = a2*t^2 + 2*a1*t + a0 If two roots are returned,
     * the order is T[0] < T[1]. Hopefully the application will be kind and provide line directions D that are not so
     * small that a2 is nearly zero and potentially creates numerical problems. The t parameter for the points of
     * intersection are returned in the afT array which must have length 2.
     *
     * @param   kP   DOCUMENT ME!
     * @param   kD   DOCUMENT ME!
     * @param   kC   DOCUMENT ME!
     * @param   fR   DOCUMENT ME!
     * @param   afT  DOCUMENT ME!
     *
     * @return  DOCUMENT ME!
     */
    private static int Find(Vector2f kP, Vector2f kD, Vector2f kC, float fR, float[] afT) {
        Vector2f kDiff = new Vector2f();
        kDiff.sub(kP, kC);

        float fA0 = kDiff.SquaredLength() - (fR * fR);
        float fA1 = kD.Dot(kDiff);
        float fA2 = kD.SquaredLength();
        float fDiscr = (fA1 * fA1) - (fA0 * fA2);
        int iRootCount;

        if (fDiscr > 0.0f) {
            iRootCount = 2;

            float fInvA2 = 1.0f / fA2;
            fDiscr = (float) Math.sqrt(fDiscr);
            afT[0] = (-fA1 - fDiscr) * fInvA2;
            afT[1] = (-fA1 + fDiscr) * fInvA2;
        } else if (fDiscr < 0.0f) {
            iRootCount = 0;
        } else // fDiscr == 0
        {
            iRootCount = 1;
            afT[0] = -fA1 / fA2;
        }

        return iRootCount;
    }
}
