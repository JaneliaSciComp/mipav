package gov.nih.mipav.view.renderer.J3D.surfaceview.brainflattenerview;


import javax.vecmath.*;


/**
 * DOCUMENT ME!
 */
public class MjPrimitive {

    //~ Methods --------------------------------------------------------------------------------------------------------

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
    public static int findIntersection(Ray2f kRay, Segment2f kSegment, float[] afT) {
        MjVector2f kDiff = new MjVector2f();
        float fD0SqrLen = kRay.m_kDirection.lengthSquared();
        int iQuantity = find(kRay.m_kOrigin, kRay.m_kDirection, kSegment.m_kOrigin, kSegment.m_kDirection, kDiff, afT);

        // intersects?
        if (0 != iQuantity) {

            if (iQuantity == 1) {

                if ((afT[0] < 0.0f) || (afT[1] < 0.0f) || (afT[1] > 1.0f)) {

                    // lines intersect, but ray and segment do not
                    iQuantity = 0;
                }
            } else {

                // ray and segment are on the same line
                float fDotRS = kRay.m_kDirection.dot(kSegment.m_kDirection);
                float fDot0;
                float fDot1;

                if (fDotRS > 0.0f) {
                    fDot0 = kDiff.dot(kRay.m_kDirection);
                    fDot1 = fDot0 + fDotRS;
                } else {
                    fDot1 = kDiff.dot(kRay.m_kDirection);
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
     * The value is 0 if the objects do not intersect. Otherwise, the quantity is 1 or 2 and is the number of valid
     * points in The points of intersection are returned in the akPoint array which must have length 2.
     *
     * @param   kSegment  DOCUMENT ME!
     * @param   kCircle   DOCUMENT ME!
     * @param   akPoint   DOCUMENT ME!
     *
     * @return  DOCUMENT ME!
     */
    public static int findIntersection(Segment2f kSegment, Circle2f kCircle, Point2f[] akPoint) {
        float[] afT = new float[2];
        int iQuantity = find(kSegment.m_kOrigin, kSegment.m_kDirection, kCircle.m_kCenter, kCircle.m_fRadius, afT);

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
                akPoint[i] = new Point2f();
                akPoint[i].scaleAdd(afT[i], kSegment.m_kDirection, kSegment.m_kOrigin);
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
    private static int find(Point2f kP, MjVector2f kD, Point2f kC, float fR, float[] afT) {
        MjVector2f kDiff = new MjVector2f();
        kDiff.sub(kP, kC);

        float fA0 = kDiff.lengthSquared() - (fR * fR);
        float fA1 = kD.dot(kDiff);
        float fA2 = kD.lengthSquared();
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
    private static int find(Point2f kP0, MjVector2f kD0, Point2f kP1, MjVector2f kD1, MjVector2f kDiff, float[] afT) {
        float fDet = kD1.kross(kD0);
        kDiff.sub(kP1, kP0);

        float fD0SqrLen = kD0.lengthSquared();
        int iQuantity;

        if ((fDet * fDet) > (MjMathf.EPSILON * fD0SqrLen * kD1.lengthSquared())) {

            // Lines intersect in a single point.  Return both s and t values for
            // use by calling functions.
            float fInvDet = 1.0f / fDet;
            afT[0] = kD1.kross(kDiff) * fInvDet;
            afT[1] = kD0.kross(kDiff) * fInvDet;
            iQuantity = 1;
        } else {

            // lines are parallel
            fDet = kD0.kross(kDiff);

            float fRHS = MjMathf.EPSILON * fD0SqrLen * kDiff.lengthSquared();

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

    //~ Inner Classes --------------------------------------------------------------------------------------------------

    /**
     * DOCUMENT ME!
     */
    public static class Circle2f {

        /** DOCUMENT ME! */
        public float m_fRadius = 0.0f;

        /** DOCUMENT ME! */
        public Point2f m_kCenter = new Point2f(0.0f, 0.0f);
    }

    /**
     * Ray is R(t) = P+t*D for t >= 0. D is not necessarily unit length.
     */
    public static class Ray2f {

        /** DOCUMENT ME! */
        public MjVector2f m_kDirection = new MjVector2f(0.0f, 0.0f); // D

        /** DOCUMENT ME! */
        public Point2f m_kOrigin = new Point2f(0.0f, 0.0f); // P
    }

    /**
     * Segment is S(t) = P+t*D for 0 <= t <= 1. D is not necessarily unit length. The end points are P and P+D.
     */
    public static class Segment2f {

        /** DOCUMENT ME! */
        public MjVector2f m_kDirection = new MjVector2f(0.0f, 0.0f); // D

        /** DOCUMENT ME! */
        public Point2f m_kOrigin = new Point2f(0.0f, 0.0f); // P
    }
}
