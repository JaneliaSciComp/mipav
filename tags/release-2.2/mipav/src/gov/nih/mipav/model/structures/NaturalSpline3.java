package gov.nih.mipav.model.structures;

import javax.vecmath.*;

/**
 * This is an implementation for a curve in 3D defined by a natural spline.
 * The spline is defined to pass through an input set of control points with
 * an associated set of time values (any range).
 */
public class NaturalSpline3
    extends MultipleCurve3
{
    /**
     * Create a natural spline curve in 3D through the set of input points.
     * Automatically compute the "times" associated with each point
     * based on the accumulated distance between the points.
     * @param akPoint Point3f[] Array of 3D points along curve.
     */
    public NaturalSpline3(Point3f[] akPoint) {
        this (getAccumDistance(akPoint), akPoint);
    }
    /**
     * Create a natural spline curve in 3D through the set of input points.
     * @param afTime float[] Array of input time values.
     * @param akPoint Point3f[] Array of 3D points.  The size of this
     * array must match the size of the input afTime array.
     */
    public NaturalSpline3(float[] afTime, Point3f[] akPoint) {
        super(afTime);
        m_akA = akPoint;

        int iSegments = getNumSegments();
        float[] afDt = new float[iSegments];
        int i;
        for (i = 0; i < iSegments; i++)
            afDt[i] = m_afTime[i + 1] - m_afTime[i];

        float[] afD2t = new float[iSegments];
        for (i = 1; i < iSegments; i++)
            afD2t[i] = m_afTime[i + 1] - m_afTime[i - 1];

        Point3f[] akAlpha = new Point3f[iSegments];
        for (i = 1; i < iSegments; i++) {
            Point3f kNumer = new Point3f();
            kNumer.x = 3.0f * (afDt[i - 1] * m_akA[i + 1].x - afD2t[i] * m_akA[i].x +
                               afDt[i] * m_akA[i - 1].x);
            kNumer.y = 3.0f * (afDt[i - 1] * m_akA[i + 1].y - afD2t[i] * m_akA[i].y +
                               afDt[i] * m_akA[i - 1].y);
            kNumer.z = 3.0f * (afDt[i - 1] * m_akA[i + 1].z - afD2t[i] * m_akA[i].z +
                               afDt[i] * m_akA[i - 1].z);
            float fInvDenom = 1.0f / (afDt[i - 1] * afDt[i]);
            kNumer.scale(fInvDenom);
            akAlpha[i] = kNumer;
        }

        float[] afEll = new float[iSegments + 1];
        float[] afMu = new float[iSegments];
        Point3f[] akZ = new Point3f[iSegments + 1];
        float fInv;

        afEll[0] = 1.0f;
        afMu[0] = 0.0f;
        akZ[0] = new Point3f(0.0f, 0.0f, 0.0f);
        for (i = 1; i < iSegments; i++) {
            afEll[i] = 2.0f * afD2t[i] - afDt[i - 1] * afMu[i - 1];
            fInv = 1.0f / afEll[i];
            afMu[i] = fInv * afDt[i];
            Point3f kPoint = new Point3f();
            kPoint.x = fInv * (akAlpha[i].x - afDt[i - 1] * akZ[i - 1].x);
            kPoint.y = fInv * (akAlpha[i].y - afDt[i - 1] * akZ[i - 1].y);
            kPoint.z = fInv * (akAlpha[i].z - afDt[i - 1] * akZ[i - 1].z);
            akZ[i] = kPoint;
        }
        afEll[iSegments] = 1.0f;
        akZ[iSegments] = new Point3f(0.0f, 0.0f, 0.0f);

        m_akB = new Point3f[iSegments];
        m_akC = new Point3f[iSegments + 1];
        m_akD = new Point3f[iSegments];

        m_akC[iSegments] = new Point3f(0.0f, 0.0f, 0.0f);

        final float fOneThird = 1.0f / 3.0f;
        for (i = iSegments - 1; i >= 0; i--) {
            Point3f kPoint;

            kPoint = new Point3f();
            kPoint.x = akZ[i].x - afMu[i] * m_akC[i + 1].x;
            kPoint.y = akZ[i].y - afMu[i] * m_akC[i + 1].y;
            kPoint.z = akZ[i].z - afMu[i] * m_akC[i + 1].z;
            m_akC[i] = kPoint;

            kPoint = new Point3f();
            fInv = 1.0f / afDt[i];
            kPoint.x = fInv * (m_akA[i + 1].x - m_akA[i].x) -
                fOneThird * afDt[i] * (m_akC[i + 1].x + 2.0f * m_akC[i].x);
            kPoint.y = fInv * (m_akA[i + 1].y - m_akA[i].y) -
                fOneThird * afDt[i] * (m_akC[i + 1].y + 2.0f * m_akC[i].y);
            kPoint.z = fInv * (m_akA[i + 1].z - m_akA[i].z) -
                fOneThird * afDt[i] * (m_akC[i + 1].z + 2.0f * m_akC[i].z);
            m_akB[i] = kPoint;

            kPoint = new Point3f();
            kPoint.x = fOneThird * fInv * (m_akC[i + 1].x - m_akC[i].x);
            kPoint.y = fOneThird * fInv * (m_akC[i + 1].y - m_akC[i].y);
            kPoint.z = fOneThird * fInv * (m_akC[i + 1].z - m_akC[i].z);
            m_akD[i] = kPoint;
        }
    }

    /**
     * Get the 3D coordinates of the position along the curve evaluated
     * for the input time value.  The input time is expessed in terms
     * of its associated segment.
     * @param iSegment int Index of segment containing the input time.
     * @param fDt float Segment-relative time.
     * @return Point3f New instance created to contain the 3D coordinates
     * of the computed position.  If the fDt value is less than 0 then the
     * coordinates of the point at the start of the segment is returned.
     * If the fDt value is greater than the segment's width in time, then the
     * coordinates of the point at the end of the segment is returned.
     */
    public Point3f getPosition(int iSegment, float fDt) {
        Point3f kResult = new Point3f();
        kResult.x =
            m_akA[iSegment].x + fDt * (m_akB[iSegment].x + fDt * (m_akC[iSegment].x +
            fDt * m_akD[iSegment].x));
        kResult.y =
            m_akA[iSegment].y + fDt * (m_akB[iSegment].y + fDt * (m_akC[iSegment].y +
            fDt * m_akD[iSegment].y));
        kResult.z =
            m_akA[iSegment].z + fDt * (m_akB[iSegment].z + fDt * (m_akC[iSegment].z +
            fDt * m_akD[iSegment].z));

        return kResult;
    }

    /**
     * Get the 3D coordinates of the 1st derivative vector at the point
     * along the curve evaluated for the input time value.  The input time
     * is expessed in terms of its associated segment.
     * @param iSegment int Index of segment containing the input time.
     * @param fDt float Segment-relative time.
     * @return Vector3f New instance created to contain the 3D coordinates
     * of the computed 1st derivative vector.  If the fDt value is less than 0
     * then the 1st derivative vector of the point at the start of the segment
     * is returned.  If the fDt value is greater than the segment's width in
     * time, then the 1st derivative vector of the point at the end of the
     * segment is returned.
     */
    protected Vector3f getFirstDerivative(int iSegment, float fDt) {
        Vector3f kResult = new Vector3f();
        kResult.x = m_akB[iSegment].x + fDt * (2.0f * m_akC[iSegment].x + 3.0f * fDt *
                                           m_akD[iSegment].x);
        kResult.y = m_akB[iSegment].y + fDt * (2.0f * m_akC[iSegment].y + 3.0f * fDt *
                                           m_akD[iSegment].y);
        kResult.z = m_akB[iSegment].z + fDt * (2.0f * m_akC[iSegment].z + 3.0f * fDt *
                                           m_akD[iSegment].z);

        return kResult;
    }

    /**
     * Get the 3D coordinates of the 2nd derivative vector at the point
     * along the curve evaluated for the input time value.  The input time
     * is expessed in terms of its associated segment.
     * @param iSegment int Index of segment containing the input time.
     * @param fDt float Segment-relative time.
     * @return Vector3f New instance created to contain the 3D coordinates
     * of the computed 2nd derivative vector.  If the fDt value is less than 0
     * then the 2nd derivative vector of the point at the start of the segment
     * is returned.  If the fDt value is greater than the segment's width in
     * time, then the 2nd derivative vector of the point at the end of the
     * segment is returned.
     */
    protected Vector3f getSecondDerivative(int iSegment, float fDt) {
        Vector3f kResult = new Vector3f();
        kResult.x = 2.0f * m_akC[iSegment].x + 6.0f * fDt * m_akD[iSegment].x;
        kResult.y = 2.0f * m_akC[iSegment].y + 6.0f * fDt * m_akD[iSegment].y;
        kResult.z = 2.0f * m_akC[iSegment].z + 6.0f * fDt * m_akD[iSegment].z;
        return kResult;
    }

    /**
     * Get the arc length of the curve for the specified interval of times
     * within the specified segment.
     * @param iSegment int Index of the segment.
     * @param fDt0 float Minimum time of the specified interval.
     * This time is relative to the starting time of the specified segment.
     * @param fDt1 float Maximum time of the specified interval.
     * This time is relative to the starting time of the specified segment.
     * @return float Computed arc length.
     */
    protected float getLength(int iSegment, float fDt0, float fDt1) {
        // Use Romberg integration.
        final int iOrder = 5;
        float[] apfRom0 = new float[iOrder];
        float[] apfRom1 = new float[iOrder];
        float fH = fDt1 - fDt0;
        apfRom0[0] = 0.5f * fH * (getSpeed(iSegment, fDt0) + getSpeed(iSegment, fDt1));
        for (int i0 = 2, iP0 = 1; i0 <= iOrder; i0++, iP0 *= 2, fH *= 0.5f) {
            // approximations via the trapezoid rule
            float fSum = 0.0f;
            int i1;
            for (i1 = 1; i1 <= iP0; i1++)
                fSum += getSpeed(iSegment, fDt0 + fH * (i1 - 0.5f));

                // Richardson extrapolation
            apfRom1[0] = 0.5f * (apfRom0[0] + fH * fSum);
            for (int i2 = 1, iP2 = 4; i2 < i0; i2++, iP2 *= 4) {
                apfRom1[i2] =
                    (iP2 * apfRom1[i2 - 1] - apfRom0[i2 - 1]) / (iP2 - 1);
            }

            for (i1 = 0; i1 < i0; i1++)
                apfRom0[i1] = apfRom1[i1];
        }

        return apfRom0[iOrder - 1];
    }

    protected final Point3f[] m_akA;
    protected final Point3f[] m_akB;
    protected final Point3f[] m_akC;
    protected final Point3f[] m_akD;
}
