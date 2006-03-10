package gov.nih.mipav.model.structures;

import javax.vecmath.*;

/**
 * This is an implementation for a curve in 3D defined by a set of straight
 * line segments for any range of time values.
 */
public class PolylineCurve3
    extends MultipleCurve3
{
    /**
     * Create a polyline curve in 3D through the set of input points.
     * Automatically compute the "times" associated with each point
     * based on the accumulated distance between the points.
     * @param akPoint Point3f[] Array of 3D points along curve.
     */
    public PolylineCurve3(Point3f[] akPoint) {
        this (getAccumDistance(akPoint), akPoint);
    }

    /**
     * Create a polyline curve in 3D through the set of input points.
     * @param afTime float[] Array of input time values.
     * @param akPoint Point3f[] Array of 3D points.  The size of this
     * array must match the size of the input afTime array.
     */
    public PolylineCurve3(float[] afTime, Point3f[] akPoint) {
        super(afTime);
        m_akP = akPoint;
        m_akV = new Vector3f[getNumSegments()];

        for (int i = 0; i < getNumSegments(); i++) {
            m_akV[i] = new Vector3f();
            m_akV[i].sub(m_akP[i + 1], m_akP[i]);
        }
    }

    /**
     * Access the array of points which define this polyline curve.
     * @return Point3f[] Array of 3D point coordinates.
     */
    public Point3f[] getPoints() {
        return m_akP;
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
    protected Point3f getPosition(int iSegment, float fDt) {
        Point3f kResult = new Point3f(m_akP[iSegment]);
        if (m_afDeltaTime[iSegment] > 0.0f) {
            kResult.scaleAdd(fDt / m_afDeltaTime[iSegment], m_akV[iSegment], kResult);
        }
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
        return new Vector3f(m_akV[iSegment]);
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
        Vector3f kDeriv2 = new Vector3f(0.0f, 0.0f, 0.0f);

        if (0.0f == fDt && iSegment > 0) {
            kDeriv2.sub(m_akV[iSegment], m_akV[iSegment - 1]);
        }

        return kDeriv2;
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
        return getPosition(iSegment, fDt0).distance(getPosition(iSegment, fDt1));
    }

    protected final Point3f[] m_akP; // point samples
    protected final Vector3f[] m_akV; // m_akV[i] = m_akP[i+1] - m_akP[i]
}
