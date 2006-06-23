package gov.nih.mipav.model.structures;


import javax.vecmath.*;


/**
 * This is an abstract interface for a curve in 3D. The curve is defined as a function of time over an interval. The
 * position, 1st derivative, and 2nd derivative is computed for any point along the curve as selected by an input value
 * in the time domain. The tanget at any point is computed as direction of the 1st derivative vector and the speed is
 * computed as the length of the 1st derivative vector. The arc length of any subinterval of the curve can be computed.
 * The curve can be reparameterized as function of arc length where the length at the minimum interval time is zero and
 * the length at the maximum interval time is the total length of the curve. This means that given an arc length, the
 * corresponding input time within the curve's time interval is determined. Extensions of this class provide the
 * implementation based on the particular type of curve.
 */
public abstract class Curve3 extends Object {

    //~ Instance fields ------------------------------------------------------------------------------------------------

    /** DOCUMENT ME! */
    protected final float m_fTMax;

    /** curve parameter is t where tmin <= t <= tmax. */
    protected final float m_fTMin;

    /** total length of curve so that it is computed once; initially set to invalid number to indicate uninitialized. */
    private float m_fTotalLength = -1.0f;

    //~ Constructors ---------------------------------------------------------------------------------------------------

    /**
     * Create a generalized curve over the specified time interval.
     *
     * @param  fTMin  float Minimum time value for the interval.
     * @param  fTMax  float Maximum time value for the interval.
     */
    protected Curve3(float fTMin, float fTMax) {
        m_fTMin = fTMin;
        m_fTMax = fTMax;
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
    public abstract Vector3f getFirstDerivative(float fTime);

    /**
     * Get the length of the curve for the specified time subinterval.
     *
     * @param   fT0  float Minimum time of the subinterval. Clipped to getMinTime() if less than it.
     * @param   fT1  float Minimum time of the subinterval. Clipped to getMaxTime() if greater than it.
     *
     * @return  float Computed length curve for the specified time subinterval.
     */
    public abstract float getLength(float fT0, float fT1);

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
    public abstract Point3f getPosition(float fTime);

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
    public abstract Vector3f getSecondDerivative(float fTime);

    /**
     * Get the maximum time value for which this curve is defined.
     *
     * @return  float Maximum time value for curve.
     */
    public float getMaxTime() {
        return m_fTMax;
    }

    /**
     * Get the minimum time value for which this curve is defined.
     *
     * @return  float Minimum time value for curve.
     */
    public float getMinTime() {
        return m_fTMin;
    }

    /**
     * Get the speed at the point along the curve evaluated for the input time value. The speed is the length of the 1st
     * derivative vector.
     *
     * @param   fTime  float Input time value which must be within the range for which this curve is defined.
     *
     * @return  float The computed speed. If the time value is less than getMinTime() then the speed at the start of the
     *          curve is returned. If the time value is greater than getMaxTime() then the speed at the end of the curve
     *          is returned.
     */
    public float getSpeed(float fTime) {
        Vector3f kVelocity = getFirstDerivative(fTime);
        float fSpeed = kVelocity.length();

        return fSpeed;
    }

    /**
     * Get the 3D coordinates of the tanget vector at the point along the curve evaluated for the input time value. The
     * tanget vector is the normalized direction of the 1st derivative vector.
     *
     * @param   fTime  float Input time value which must be within the range for which this curve is defined.
     *
     * @return  Vector3f New instance created to contain the 3D coordinates of the computed tanget vector. If the time
     *          value is less than getMinTime() then the tanget vector at the start of the curve is returned. If the
     *          time value is greater than getMaxTime() then the tanget vector at the end of the curve is returned.
     */
    public Vector3f getTangent(float fTime) {
        Vector3f kVelocity = getFirstDerivative(fTime);

        if (kVelocity.lengthSquared() > 0.0f) {
            kVelocity.normalize();
        }

        return kVelocity;
    }

    /**
     * Reparameterize the curve where the time value is defined as a function of arc length. Location the time value
     * associated with this arc length along the curve.
     *
     * @param   fLength           float Input arc length for which the associated time is to be determined. This length
     *                            is measured from the starting point of the curve which is at the getMinTime().
     * @param   iIterations       int Maximum number of iterations for determining the associated time value. A
     *                            bisection method is used to perform the search over the entire range of time values
     *                            defined for this curve. If this maximum number of iterations are executed, then the
     *                            resolution of the time interval is (getMaxTime()-getMinTime()) divided by
     *                            2^{iIterations}.
     * @param   fLengthTolerance  float If the length of the current subinterval in the bisection method is smaller than
     *                            this value, then the search terminates.
     *
     * @return  float Time value associated with this arc length along the curve. The returned value is always within
     *          the range of getMinTime() and getMaxTime().
     */
    public float getTime(float fLength, int iIterations, float fLengthTolerance) {
        return getTime(m_fTMin, m_fTMax, fLength, iIterations, fLengthTolerance);
    }

    /**
     * Get the length total length of the curve from start to end.
     *
     * @return  float Length of the entire curve.
     */
    public float getTotalLength() {

        if (m_fTotalLength < 0.0f) {
            m_fTotalLength = getLength(m_fTMin, m_fTMax);
        }

        return m_fTotalLength;
    }

    /**
     * Reparameterize the curve where the time value is defined as a function of arc length. Locate the time value
     * within the specified time subinterval that is associated with this arc length along the curve.
     *
     * @param   fTimeMin          float Minimum time defined for this curve subinterval.
     * @param   fTimeMax          float Maximum time defined for this curve subinterval.
     * @param   fLength           float Input arc length for which the associated time is to be determined. This length
     *                            is measured from the starting point of the curve which is at fTimeMin.
     * @param   iIterations       int Maximum number of iterations for determining the associated time value. A
     *                            bisection method is used to perform the search over the entire range of time values
     *                            defined for this curve. If this maximum number of iterations are executed, then the
     *                            resolution of the time interval is (fTimeMax-fTimeMin) divided by 2^{iIterations}.
     * @param   fLengthTolerance  float If the length of the current subinterval in the bisection method is smaller than
     *                            this value, then the search terminates.
     *
     * @return  float Time value associated with this arc length along the curve. The returned value is always within
     *          the range of fTimeMin and fTimeMax.
     */
    protected float getTime(float fTimeMin, float fTimeMax, float fLength, int iIterations, float fLengthTolerance) {
        float fLengthSegment = getLength(fTimeMin, fTimeMax);

        if (fLength <= 0.0f) {
            return fTimeMin;
        }

        if (fLength >= fLengthSegment) {
            return fTimeMax;
        }

        // Use bisection method computing arc length over subintervals
        // of the curve.  Stop when the maximum number of iterations
        // specified is reached which should give a precision of
        // 2^{-iIterations} precision.
        float fTime = 0.5f * (fTimeMin + fTimeMax);

        while ((fLengthSegment > fLengthTolerance) && (iIterations-- > 0)) {
            float fLengthLo = getLength(fTimeMin, fTime);
            float fLengthHi = getLength(fTime, fTimeMax);
            float fLengthSegmentPrev = fLengthSegment;
            fLengthSegment = fLengthLo + fLengthHi;

            // As the interval is bisected, each time we get a more
            // precise measurement.  Adjust the fLength value on
            // the chosen subinterval so that it is relative to
            // the new computed length of the subinterval.
            fLength = fLength * fLengthSegment / fLengthSegmentPrev;

            if (fLength < fLengthLo) {
                fTimeMax = fTime;
                fLengthSegment = fLengthLo;
            } else {
                fTimeMin = fTime;
                fLength -= fLengthLo;
                fLengthSegment = fLengthHi;
            }

            fTime = 0.5f * (fTimeMin + fTimeMax);
        }

        return fTime;
    }
}
