package gov.nih.mipav.model.structures;

import javax.vecmath.*;

/**
 * This is an abstract interface for a curve in 3D.  This interface builds
 * upon that of the Curve3 abstract interface by adding support for computing
 * arc lengths over subintervals of the entire curve.  This capability
 * can be used to speed up the reparameterization of the curve for the
 * time value as a function of arc length.
 */
public abstract class IntervalLengthCurve3
    extends Curve3
{
    /**
     * Create a generalized curve over the specified time interval and
     * having the specified number of intervals for computing arc length.
     * Note that the arc lengths cannot be computed until the derived
     * class is fully constructed.  This is done by requiring that derived
     * classes call the initializeIntervalLengths method within the body
     * of their constructor.  It is at this time that the time values
     * associated with the intervals is specified.
     * @param afTime float[] Array of input time values associated with
     * the intervals.  These times values must be increasing and must be
     * within the getMinTime() and getMaxTime() range.
     */
    protected IntervalLengthCurve3(float[] afTime) {
        super(afTime[0], afTime[afTime.length - 1]);
        m_afIntervalTime = afTime;
        m_iNumLengthIntervals = afTime.length-1;
    }

    /**
     * Get the number of length intervals defined for this curve.
     * @return int Number of length intervals.
     */
    public int getNumLengthIntervals() {
        return m_iNumLengthIntervals;
    }

    /**
     * Get the arc length of the curve for the specified interval of times
     * within the specified time interval.
     * @param iInterval int Index of the time interval.
     * @param fDt0 float Minimum time of the specified interval.
     * This time is relative to the starting time of the specified interval.
     * @param fDt1 float Maximum time of the specified interval.
     * This time is relative to the starting time of the specified interval.
     * @return float Computed arc length.
     */
    public abstract float getIntervalLength(int iInterval, float fDt0, float fDt1);

    /**
     * Return the total length of the specified interval.
     * @param iInterval int Index which indentifies the interval.
     * @return float Total length of the specified interval.
     */
    public float getIntervalLength(int iInterval) {

        // Compute if not already done so.
        if (null == m_afLength) {
            m_afLength = new float[m_iNumLengthIntervals];

            // arc lengths and accumulated arc lengths of the intervals
            for (int i = 0; i < m_iNumLengthIntervals; i++) {
                float fDt = m_afIntervalTime[i + 1] - m_afIntervalTime[i];
                m_afLength[i] = getIntervalLength(i, 0.0f, fDt);
            }
        }

        return m_afLength[iInterval];
    }

    /**
     * Return the accumulated length of the curve for all intervals up to
     * and including the specified interval.
     * @param iInterval int Index which indentifies the interval.
     * @return float Accmulated length of curve through specified interval.
     */
    public float getIntervalAccumLength(int iInterval) {

        // Compute if not already done so.
        if (null == m_afAccumLength) {
            m_afAccumLength = new float[m_iNumLengthIntervals];

            // accumulated arc length of the intervals
            m_afAccumLength[0] = getIntervalLength(0);
            for (int i = 1; i < m_iNumLengthIntervals; i++) {
                m_afAccumLength[i] = m_afAccumLength[i - 1] +
                    getIntervalLength(i);
            }
        }

        return m_afAccumLength[iInterval];
    }

    /**
     * Get the length of the curve for the specified time subinterval.
     * @param fT0 float Minimum time of the subinterval.  Clipped to
     * getMinTime() if less than it.
     * @param fT1 float Minimum time of the subinterval.  Clipped to
     * getMaxTime() if greater than it.
     * @return float Computed length curve for the specified time subinterval.
     */
    public float getLength(float fT0, float fT1) {

        // Find the intervals for the two specified times.
        int iInterval0 = getInterval(fT0);
        int iInterval1 = getInterval(fT1);
        float fDt0 = fT0 - m_afIntervalTime[iInterval0];
        float fDt1 = fT1 - m_afIntervalTime[iInterval1];

        // Length measured across at least two intervals.
        if (iInterval0 < iInterval1) {
            // accumulate full-segment lengths
            float fLength = 0.0f;
            for (int i = iInterval0 + 1; i < iInterval1; i++) {
                fLength += getIntervalLength(i);
            }

            // add on partial first segment
            fLength +=
                getIntervalLength(iInterval0, fDt0,
                                  m_afIntervalTime[iInterval0 + 1] - m_afIntervalTime[iInterval0]);

            // add on partial last segment
            fLength += getIntervalLength(iInterval1, 0.0f, fDt1);
            return fLength;
        }

        // Length measured within one interval.
        else {
            return getIntervalLength(iInterval0, fDt0, fDt1);
        }
    }

    /**
     * Get the length total length of the curve from start to end.
     * @return float Length of the entire curve.
     */
    public float getTotalLength() {
        return getIntervalAccumLength(m_iNumLengthIntervals - 1);
    }

    /**
     * Reparameterize the curve where the time value is defined as a function
     * of arc length.  Location the time value associated with this
     * arc length along the curve.
     * @param fLength float Input arc length for which the associated time
     * is to be determined.  This length is measured from the starting
     * point of the curve which is at the getMinTime().
     * @param iIterations int Maximum number of iterations for determining
     * the associated time value.  A bisection method is used to perform
     * the search over the entire range of time values defined for this
     * curve.  If this maximum number of iterations are executed, then
     * the resolution of the time interval is (getMaxTime()-getMinTime())
     * divided by 2^{iIterations}.
     * @param fLengthTolerance float If the length of the current subinterval
     * in the bisection method is smaller than this value, then the search
     * terminates.
     * @return float Time value associated with this arc length along
     * the curve.  The returned value is always within the range of
     * getMinTime() and getMaxTime().
     */
    public float getTime(float fLength, int iIterations, float fLengthTolerance) {

        // Find the interval which contains this length.
        if (fLength < 0.0f) {
            return m_fTMin;
        }
        else if (fLength > getIntervalAccumLength(m_iNumLengthIntervals - 1)) {
            return m_fTMax;
        }
        int iInterval = 0;
        while ( (iInterval < m_iNumLengthIntervals) &&
               (fLength >= getIntervalAccumLength(iInterval))) {
            iInterval++;
        }

        // At the end of the curve, i.e. last interval?
        if (iInterval == m_iNumLengthIntervals) {
            return m_fTMax;
        }

        // Adjust the input arc length value so that it is defined
        // relative to the accumulated arc length before the interval
        // into which it belongs.
        if (iInterval > 0) {
            fLength -= getIntervalAccumLength(iInterval - 1);
        }

        // Search for the time in the subinterval that is associated
        // with this relative length (within the subinterval).
        return getTime(m_afIntervalTime[iInterval],
                       m_afIntervalTime[iInterval + 1],
                       fLength,
                       iIterations,
                       fLengthTolerance);
    }

    /**
     * Get the index of the interval which contains the specified time value.
     * @param fTime float Input time value.
     * @return int Index of associated time interval.
     */
    protected int getInterval(float fTime) {
        if (fTime <= m_afIntervalTime[0]) {
            return 0;
        }
        else if (fTime >= m_afIntervalTime[m_iNumLengthIntervals]) {
            return m_iNumLengthIntervals - 1;
        }
        else {
            for (int i = 0; i < m_iNumLengthIntervals; i++) {
                if (fTime < m_afIntervalTime[i + 1]) {
                    return i;
                }
            }
            return 0; // should never get here
        }
    }

    protected final int m_iNumLengthIntervals;

    // These are only created if initializeIntervalLengths is called.
    protected float[] m_afIntervalTime = null;
    private float[] m_afLength = null;
    private float[] m_afAccumLength = null;
}
