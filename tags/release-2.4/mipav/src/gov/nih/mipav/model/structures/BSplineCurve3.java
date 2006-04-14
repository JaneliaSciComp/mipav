package gov.nih.mipav.model.structures;

import gov.nih.mipav.model.structures.*;
import gov.nih.mipav.model.structures.jama.*;

import javax.vecmath.*;

/**
 * This is an implementation for a curve in 3D defined by a set of BSpline
 * basis functions.  The basis is defined by a set of 3D control points
 * and a degree.  A function is provided to a BSpline-based 3D curve which
 * best fits an input set of 3D data points.  Only curves based
 * on open uniform BSplines basis functions are supported.  The time
 * values are fixed to be within the [0,1] range.
 */
public class BSplineCurve3
    extends IntervalLengthCurve3
{
    /**
     * Create an open uniform BSpline curve defined on the time interval
     * of [0,1].
     * @param akCtrlPoint Point3f[] Array of 3D control points.
     * @param iDegree int Degree of the BSpline to use.  This must be a
     * positive value less than the number of control points.
     */
    public BSplineCurve3(Point3f[] akCtrlPoint, int iDegree) {
        super(createUniformTimes(akCtrlPoint.length));
        m_akCtrlPoint = akCtrlPoint;
        m_kBasis = new BSplineBasisf(m_akCtrlPoint.length, iDegree);
        m_afBD0 = new float[m_kBasis.getNumKnots()];
        m_afBD1 = new float[m_kBasis.getNumKnots()];
        m_afBD2 = new float[m_kBasis.getNumKnots()];
    }

    /**
     * Called to create an array of equally spaced time intervals over
     * the range of [0,1].
     * @param iNumControlPoints int One less than this number is the number
     * of intervals.
     * @return float[] Array with the the equally spaced times.  The size
     * of this array is the number of input control points.
     */
    protected static float[] createUniformTimes(int iNumControlPoints) {

        float[] afIntervalTimes = new float[iNumControlPoints];
        float fSpacing = 1.0f/(iNumControlPoints-1);
        for (int iControl = 0; iControl < iNumControlPoints; iControl++) {
            afIntervalTimes[iControl] = fSpacing * iControl;
        }

        return afIntervalTimes;
    }

    /**
     * Get the number of control points defined for the BSpline curve.
     * @return int Number of BSpline control points.
     */
    public int getNumControlPoints() {
        return m_kBasis.getNumControlPoints();
    }

    /**
     * Get the degree of the basis functions defined for the BSpline curve.
     * @return int Degree of BSpline basis functions.
     */
    public int getDegree() {
        return m_kBasis.getDegree();
    }

    /**
     * Get the 3D coordinates of the position along the curve evaluated
     * for the input time value.
     * @param fTime float Input time value which must be within the
     * range for which this curve is defined.
     * @return Point3f New instance created to contain the 3D coordinates
     * of the computed position.  If the time value is less than getMinTime()
     * then the coordinates of the point at the start of the curve is returned.
     * If the time value is greater than getMaxTime() then the coordinates of
     * the point at the end of the curve is returned.
     */
    public Point3f getPosition(float fTime) {
        int iMax = m_kBasis.compute(fTime, m_afBD0, null, null);
        int iMin = iMax - m_kBasis.getDegree();

        Point3f kPos = new Point3f(0.0f, 0.0f, 0.0f);
        for (int i = iMin; i <= iMax; i++) {
            kPos.scaleAdd(m_afBD0[i], m_akCtrlPoint[i], kPos);
        }

        return kPos;
    }

    /**
     * Get the 3D coordinates of the 1st derivative vector at the point
     * along the curve evaluated for the input time value.
     * @param fTime float Input time value which must be within the range
     * for which this curve is defined.
     * @return Vector3f New instance created to contain the 3D coordinates
     * of the computed 1st derivative vector.  If the time value is less than
     * getMinTime() then the 1st derivative vector at the start of the curve
     * is returned.  If the time value is greater than getMaxTime() then the
     * 1st derivative vector at the end of the curve is returned.
     */
    public Vector3f getFirstDerivative(float fTime) {
        int iMax = m_kBasis.compute(fTime, m_afBD0, m_afBD1, null);
        int iMin = iMax - m_kBasis.getDegree();

        Vector3f kDer1 = new Vector3f(0.0f, 0.0f, 0.0f);
        for (int i = iMin; i <= iMax; i++) {
            kDer1.scaleAdd(m_afBD1[i], m_akCtrlPoint[i], kDer1);
        }

        return kDer1;
    }

    /**
     * Get the 3D coordinates of the 2nd derivative vector at the point
     * along the curve evaluated for the input time value.
     * @param fTime float Input time value which must be within the range
     * for which this curve is defined.
     * @return Vector3f New instance created to contain the 3D coordinates
     * of the computed 1st derivative vector.  If the time value is less than
     * getMinTime() then the 2nd derivative vector at the start of the curve
     * is returned.  If the time value is greater than getMaxTime() then the
     * 2nd derivative vector at the end of the curve is returned.
     */
    public Vector3f getSecondDerivative(float fTime) {
        int iMax = m_kBasis.compute(fTime, m_afBD0, m_afBD1, m_afBD2);
        int iMin = iMax - m_kBasis.getDegree();

        Vector3f kDer2 = new Vector3f(0.0f, 0.0f, 0.0f);
        for (int i = iMin; i <= iMax; i++) {
            kDer2.scaleAdd(m_afBD2[i], m_akCtrlPoint[i], kDer2);
        }

        return kDer2;
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
    public float getIntervalLength(int iInterval, float fDt0, float fDt1) {
        float fA = m_afIntervalTime[iInterval] + fDt0;
        float fB = m_afIntervalTime[iInterval] + fDt1;

        // Use Romberg integration.
        final int iOrder = 5;
        float[] apfRom0 = new float[iOrder];
        float[] apfRom1 = new float[iOrder];
        float fH = fB - fA;
        apfRom0[0] = 0.5f * fH * (getSpeed(fA) + getSpeed(fB));
        for (int i0 = 2, iP0 = 1; i0 <= iOrder; i0++, iP0 *= 2, fH *= 0.5f) {
            // approximations via the trapezoid rule
            float fSum = 0.0f;
            int i1;
            for (i1 = 1; i1 <= iP0; i1++)
                fSum += getSpeed(fA + fH * (i1 - 0.5f));

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

    /**
     * Create a BSpline curve which best fits a set of 3D points.
     * @param akPoint Point3f[] Array of 3D points.  The order of the
     * points in the array defines a path.
     * @param iNumControlPoints int Number of 3D control points to use
     * when creating the BSpline curve.  The location of the 3D control
     * points are determined to provide a least squares best fit.
     * @param iDegree int Degree of the basis functions to use when
     * creating the BSpline curve.
     * @return BSplineCurve3 New instance of the best fit BSpline curve.
     */
    public static BSplineCurve3 createApproximation(Point3f[] akPoint,
        int iNumControlPoints, int iDegree) {

        // How many samples.
        int iNumSamples = akPoint.length;

        // Create a discretized Bspline basis given the number of
        // samples, number of control points, and specified degree.
        BSplineBasisDiscretef kBasis =
            new BSplineBasisDiscretef(iNumControlPoints, iDegree, iNumSamples);

        // Setup linear systems in form of a matrix equation.  The matrix
        // A contains the contribution of each control point for each
        // sample taken on the [0,1] interval.  The vector of values B
        // are the sample taken on the [0,1] interval.  The problem is
        // to find the X in A * X = B where X contains the positions
        // of the control points.  Since the solution is actually
        // X = (A^T * A)^{-1} * A^T * B, we will precompute the
        // values (A^T * A) and (A^T * B) which will save some storage.
        // and then solve (A^T * A) * X = A^T * B.
        // Fill both A and B with zeros as we will compute their
        // entries by summing into them.
        Matrix kA = new Matrix(iNumControlPoints, iNumControlPoints, 0.0);
        Point3f[] kB = new Point3f[iNumControlPoints];
        for (int iControl = 0; iControl < iNumControlPoints; iControl++) {
            kB[iControl] = new Point3f(0.0f, 0.0f, 0.0f);
        }
        for (int iSample = 0; iSample < iNumSamples; iSample++) {
            for (int iRow = 0; iRow < iNumControlPoints; iRow++) {
                kB[iRow].scaleAdd(kBasis.getD0(iRow, iSample), akPoint[iSample], kB[iRow]);

                for (int iCol = 0; iCol < iNumControlPoints; iCol++) {
                    double dAij = kA.get(iRow, iCol) +
                        kBasis.getD0(iRow, iSample) * kBasis.getD0(iCol, iSample);
                    kA.set(iRow, iCol, dAij);
                }
            }
        }

        // Use the inverse of A to find the solution which tells us
        // the location of each of the control points.
        kA.invert();
        Point3f[] akControl = new Point3f[iNumControlPoints];
        for (int iControl = 0; iControl < iNumControlPoints; iControl++) {
            akControl[iControl] = new Point3f(0.0f, 0.0f, 0.0f);

            for (int i = 0; i < iNumControlPoints; i++) {
                akControl[iControl].scaleAdd( (float) kA.get(iControl, i),
                                             kB[i], akControl[iControl]);
            }
        }

        return new BSplineCurve3(akControl, iDegree);
    }

    /**
     * Create a BSpline curve which best fits a set of 3D points.
     * @param akValue float[] Array of values.  The order of the
     * points in the array defines a path.
     * @param iNumControlPoints int Number of control points to use
     * when creating the BSpline curve.  The location of the control
     * points are determined to provide a least squares best fit.
     * @param iDegree int Degree of the basis functions to use when
     * creating the BSpline curve.
     * @return BSplineCurve3 New instance of the best fit BSpline curve.
     * Even though the fit is for a single set of values, the curve that
     * is created is for 3D set of values in which the x coordinates
     * are used to representing the single value curve.  The y and z
     * coordinates are undefined.
     */
    public static BSplineCurve3 createApproximation(float[] akValue,
        int iNumControlPoints, int iDegree) {

        // How many samples.
        int iNumSamples = akValue.length;

        // Create a discretized Bspline basis given the number of
        // samples, number of control points, and specified degree.
        BSplineBasisDiscretef kBasis =
            new BSplineBasisDiscretef(iNumControlPoints, iDegree, iNumSamples);

        // Setup linear systems in form of a matrix equation.  The matrix
        // A contains the contribution of each control point for each
        // sample taken on the [0,1] interval.  The vector of values B
        // are the sample taken on the [0,1] interval.  The problem is
        // to find the X in A * X = B where X contains the positions
        // of the control points.  Since the solution is actually
        // X = (A^T * A)^{-1} * A^T * B, we will precompute the
        // values (A^T * A) and (A^T * B) which will save some storage.
        // and then solve (A^T * A) * X = A^T * B.
        // Fill both A and B with zeros as we will compute their
        // entries by summing into them.
        Matrix kA = new Matrix(iNumControlPoints, iNumControlPoints, 0.0);
        float[] kB = new float[iNumControlPoints];
        for (int iControl = 0; iControl < iNumControlPoints; iControl++) {
            kB[iControl] = 0.0f;
        }
        for (int iSample = 0; iSample < iNumSamples; iSample++) {
            for (int iRow = 0; iRow < iNumControlPoints; iRow++) {
                kB[iRow] += kBasis.getD0(iRow, iSample) * akValue[iSample];

                for (int iCol = 0; iCol < iNumControlPoints; iCol++) {
                    double dAij = kA.get(iRow, iCol) +
                        kBasis.getD0(iRow, iSample) * kBasis.getD0(iCol, iSample);
                    kA.set(iRow, iCol, dAij);
                }
            }
        }

        // Use the inverse of A to find the solution which tells us
        // the location of each of the control points.
        kA.invert();
        Point3f[] akControl = new Point3f[iNumControlPoints];
        for (int iControl = 0; iControl < iNumControlPoints; iControl++) {
            akControl[iControl] = new Point3f(0.0f, 0.0f, 0.0f);

            for (int i = 0; i < iNumControlPoints; i++) {
                akControl[iControl].x += (float) kA.get(iControl, i) * kB[i];
            }
        }

        return new BSplineCurve3(akControl, iDegree);
    }

    protected Point3f[] m_akCtrlPoint; // ctrl[n+1]
    protected float[] m_afBD0;
    protected float[] m_afBD1;
    protected float[] m_afBD2;
    protected BSplineBasisf m_kBasis;
}
