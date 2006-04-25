package gov.nih.mipav.model.structures;


import javax.vecmath.*;


/**
 * This is an abstract interface for a curve in 3D. This interface builds upon that of the IntervalLengthCurve3 abstract
 * interface by adding support for curves defined by segments. Each segment is defined by time values, the same ones
 * used to define the length intervals used in the IntervalLengthCurve3 superclass.
 */
public abstract class MultipleCurve3 extends IntervalLengthCurve3 {

    //~ Instance fields ------------------------------------------------------------------------------------------------

    /** DOCUMENT ME! */
    protected final float[] m_afDeltaTime; // [i] = m_afTime[i+1]-m_afTime

    /** DOCUMENT ME! */
    protected final float[] m_afTime;

    //~ Constructors ---------------------------------------------------------------------------------------------------

    /**
     * Create a segment-based 3D curve.
     *
     * @param  afTime  float[] Array of time values associated with the segments. These times values must be increasing
     *                 where the first and last values in the array define the minimum and maximum input time values,
     *                 respectively.
     */
    protected MultipleCurve3(float[] afTime) {
        super(afTime);

        m_afTime = afTime;
        m_afDeltaTime = new float[getNumSegments()];

        for (int i = 0; i < getNumSegments(); i++) {
            m_afDeltaTime[i] = m_afTime[i + 1] - m_afTime[i];
        }
    }

    //~ Methods --------------------------------------------------------------------------------------------------------

    /**
     * Get the 3D coordinates of the 1st derivative vector at the point along the curve evaluated for the input time
     * value.
     *
     * @param   fTime  float Input time value which must be within the range for which this curve is defined.
     *
     * @return  Vector3f New instance created to contain the 3D coordinates of the computed 1st derivative vector. If
     *          the time value is less than getMinTime() then the 1st derivative vector at the start of the curve is
     *          returned. If the time value is greater than getMaxTime() then the 1st derivative vector at the end of
     *          the curve is returned.
     */
    public Vector3f getFirstDerivative(float fTime) {
        return getFirstDerivative(getSegment(fTime), getSegmentTime(fTime));
    }

    /**
     * Get the arc length of the curve for the specified interval of times within the specified time interval.
     *
     * @param   iInterval  int Index of the time interval.
     * @param   fDt0       float Minimum time of the specified interval. This time is relative to the starting time of
     *                     the specified interval.
     * @param   fDt1       float Maximum time of the specified interval. This time is relative to the starting time of
     *                     the specified interval.
     *
     * @return  float Computed arc length.
     */
    public float getIntervalLength(int iInterval, float fDt0, float fDt1) {

        // Note that the times used for our segments is the same
        // as that use for the length intervals.
        return getLength(iInterval, fDt0, fDt1);
    }

    /**
     * Get the number of segments defined for this curve.
     *
     * @return  int Number of curve segments.
     */
    public int getNumSegments() {
        return getNumLengthIntervals();
    }

    /**
     * Get the 3D coordinates of the position along the curve evaluated for the input time value.
     *
     * @param   fTime  float Input time value which must be within the range for which this curve is defined.
     *
     * @return  Point3f New instance created to contain the 3D coordinates of the computed position. If the time value
     *          is less than getMinTime() then the coordinates of the point at the start of the curve is returned. If
     *          the time value is greater than getMaxTime() then the coordinates of the point at the end of the curve is
     *          returned.
     */
    public Point3f getPosition(float fTime) {
        return getPosition(getSegment(fTime), getSegmentTime(fTime));

    }

    /**
     * Get the 3D coordinates of the 2nd derivative vector at the point along the curve evaluated for the input time
     * value.
     *
     * @param   fTime  float Input time value which must be within the range for which this curve is defined.
     *
     * @return  Vector3f New instance created to contain the 3D coordinates of the computed 1st derivative vector. If
     *          the time value is less than getMinTime() then the 2nd derivative vector at the start of the curve is
     *          returned. If the time value is greater than getMaxTime() then the 2nd derivative vector at the end of
     *          the curve is returned.
     */
    public Vector3f getSecondDerivative(float fTime) {
        return getSecondDerivative(getSegment(fTime), getSegmentTime(fTime));
    }

    /**
     * Get the 3D coordinates of the 1st derivative vector at the point along the curve evaluated for the input time
     * value. The input time is expessed in terms of its associated segment.
     *
     * @param   iSegment  int Index of segment containing the input time.
     * @param   fDt       float Segment-relative time.
     *
     * @return  Vector3f New instance created to contain the 3D coordinates of the computed 1st derivative vector. If
     *          the fDt value is less than 0 then the 1st derivative vector of the point at the start of the segment is
     *          returned. If the fDt value is greater than the segment's width in time, then the 1st derivative vector
     *          of the point at the end of the segment is returned.
     */
    protected abstract Vector3f getFirstDerivative(int iSegment, float fDt);

    /**
     * Get the arc length of the curve for the specified interval of times within the specified segment.
     *
     * @param   iSegment  int Index of the segment.
     * @param   fDt0      float Minimum time of the specified interval. This time is relative to the starting time of
     *                    the specified segment.
     * @param   fDt1      float Maximum time of the specified interval. This time is relative to the starting time of
     *                    the specified segment.
     *
     * @return  float Computed arc length.
     */
    protected abstract float getLength(int iSegment, float fDt0, float fDt1);

    /**
     * Get the 3D coordinates of the position along the curve evaluated for the input time value. The input time is
     * expessed in terms of its associated segment.
     *
     * @param   iSegment  int Index of segment containing the input time.
     * @param   fDt       float Segment-relative time.
     *
     * @return  Point3f New instance created to contain the 3D coordinates of the computed position. If the fDt value is
     *          less than 0 then the coordinates of the point at the start of the segment is returned. If the fDt value
     *          is greater than the segment's width in time, then the coordinates of the point at the end of the segment
     *          is returned.
     */
    protected abstract Point3f getPosition(int iSegment, float fDt);

    /**
     * Get the 3D coordinates of the 2nd derivative vector at the point along the curve evaluated for the input time
     * value. The input time is expessed in terms of its associated segment.
     *
     * @param   iSegment  int Index of segment containing the input time.
     * @param   fDt       float Segment-relative time.
     *
     * @return  Vector3f New instance created to contain the 3D coordinates of the computed 2nd derivative vector. If
     *          the fDt value is less than 0 then the 2nd derivative vector of the point at the start of the segment is
     *          returned. If the fDt value is greater than the segment's width in time, then the 2nd derivative vector
     *          of the point at the end of the segment is returned.
     */
    protected abstract Vector3f getSecondDerivative(int iSegment, float fDt);

    /**
     * Compute the accumulated distance between the specified sequence of 3D points.
     *
     * @param   akPoint  Point3f[] Input sequence of 3D points.
     *
     * @return  float[] Array containing accumulated distance between the points.
     */
    protected static float[] getAccumDistance(Point3f[] akPoint) {
        float[] afTime = new float[akPoint.length];

        if (afTime.length > 0) {
            afTime[0] = 0.0f;

            for (int i = 1; i < afTime.length; i++) {
                afTime[i] = afTime[i - 1] + akPoint[i].distance(akPoint[i - 1]);
            }
        }

        return afTime;
    }

    /**
     * Get the index of the segment which contains the specified time value.
     *
     * @param   fTime  float Input time value.
     *
     * @return  int Index of associated time segment.
     */
    protected int getSegment(float fTime) {

        if (fTime <= m_afTime[0]) {
            return 0;
        } else if (fTime >= m_afTime[getNumSegments()]) {
            return getNumSegments() - 1;
        } else {

            for (int i = 0; i < getNumSegments(); i++) {

                if (fTime < m_afTime[i + 1]) {
                    return i;
                }
            }

            return 0; // should never get here
        }
    }

    /**
     * Get the offset in time for the input time relative to the starting time for the segment which contains the input
     * time.
     *
     * @param   fTime  float Input time value.
     *
     * @return  float Segment-relative time.
     */
    protected float getSegmentTime(float fTime) {

        if (fTime <= m_afTime[0]) {
            return 0.0f;
        } else if (fTime >= m_afTime[getNumSegments()]) {
            return m_afTime[getNumSegments()] - m_afTime[getNumSegments() - 1];
        } else {

            for (int i = 0; i < getNumSegments(); i++) {

                if (fTime < m_afTime[i + 1]) {
                    return fTime - m_afTime[i];
                }
            }

            return 0.0f; // should never get here
        }
    }

    /**
     * Get the speed at the point along the curve evaluated for the input time value. The speed is the length of the 1st
     * derivative vector. The input time is expessed in terms of its associated segment.
     *
     * @param   iSegment  int Index of segment containing the input time.
     * @param   fDt       float Segment-relative time.
     *
     * @return  float The computed speed. If the fDt value is less than 0 then the speed at the start of the segment is
     *          returned. If the fDt value is greater than the segment's width in time, then the speed at the end of the
     *          segment is returned.
     */
    protected float getSpeed(int iSegment, float fDt) {
        return getFirstDerivative(iSegment, fDt).length();
    }
}
