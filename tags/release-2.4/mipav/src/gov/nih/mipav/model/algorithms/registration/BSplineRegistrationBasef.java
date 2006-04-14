package gov.nih.mipav.model.algorithms.registration;

import gov.nih.mipav.model.structures.*;
import gov.nih.mipav.model.structures.jama.*;

/**
 * This is a common base class for all BSpline-based registrations.
 */
public class BSplineRegistrationBasef {

    /**
     * Create instance to be used for registration.  Creates the memory
     * for the registered source image and the error image given the
     * total number of samples in the image.
     * @param kImageSrc ModelSimpleImage Input source image which contains
     * properties and data values.  Must have either 2 or 3 dimensions
     * where the number of dimensions matches that of the target image.
     * @param kImageTrg ModelSimpleImage Input target image which contains
     * properties and data values.  Must have either 2 or 3 dimensions
     * where the number of dimensions matches that of the source image.
     * @param kRegMeasure RegistrationMeasure Defines the cost measure
     * for comparing the target image with the registered source image.
     */
    protected BSplineRegistrationBasef(ModelSimpleImage kImageSrc,
                                       ModelSimpleImage kImageTrg,
                                       RegistrationMeasure kRegMeasure) {

        // Save input parameters.
        m_kImageTrg = kImageTrg;
        m_kImageSrc = kImageSrc;
        m_iNumSamplesTrg = kImageTrg.data.length;
        m_kRegMeasure = kRegMeasure;

        // Pass the target information to the registration measure.
        kRegMeasure.setImages(kImageSrc, kImageTrg);
    }

    /**
     * Cleanup memory
     * @throws Throwable
     */
    public void finalize() throws Throwable  {
        m_kImageTrg = null;
        m_kImageSrc = null;

        super.finalize();
    }

    /**
     * Return access to input target image data values which is the same
     * as was passed to the constructor.
     * @return ModelSimpleImage Image which contains properties and
     * data values.
     */
    public ModelSimpleImage getImageTarget() {
        return m_kImageTrg;
    }

    /**
     * Return access to input source image data values which is the same
     * as was passed to the constructor.
     * @return ModelSimpleImage Image which contains properties and
     * data values.
     */
    public ModelSimpleImage getImageSource() {
        return m_kImageSrc;
    }

    /**
     * Return the current error measure computeas the root mean
     * squared error.
     * @return double Current cummulative error measure between target
     * and registered source.
     */
    public double getError() {
        return m_kRegMeasure.getError();
    }

    /**
     * Create optimal placement of control points for each dimension
     * which yields the identity mapping of the source image, i.e.,
     * identity in that the output value to the Bspline function is
     * the same as the input value.
     * @param kBasis BSplineBasisDiscretef Input BSpline-basis
     * for a single dimension.
     * @return float[] Array of control point positions for this single
     * dimension input B-spline basis These positions are in the [0,1] range
     * and are monotonically increasing.  The size of this array is determined
     * by the number of control points specified for the input BSpline basis.
     */
    protected float[] createIdentityMapControlPoints(BSplineBasisDiscretef kBasis) {

        // Create the array to store the control point positions.
        int iNumControlPoints = kBasis.getNumControlPoints();
        float[] afControlPoint = new float[iNumControlPoints];

        // In the case of degree 1, evenly spacing the control points
        // always yields the identity map.
        if (1 == kBasis.getDegree()) {
            for (int i = 0; i < iNumControlPoints; i++) {
                afControlPoint[i] = (float)i / (float)(iNumControlPoints-1);
            }
        }

        // In all the other cases, sample the [0,1] BSpline function input
        // range and compute the position of the each control point so
        // that the same value is output as was input.  Setup a linear
        // system of equations and solve by method of least squares.
        else {

            // How many samples are defined for discretized BSpline already?
            int iNumSamples = kBasis.getNumSamples();

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
            Matrix kB = new Matrix(iNumControlPoints, 1, 0.0);
            for (int iSample = 0; iSample < iNumSamples; iSample++) {
                float fT = iSample / (float)(iNumSamples-1);

                for (int iRow = 0; iRow < iNumControlPoints; iRow++) {
                    double dBi = kB.get(iRow,0) + fT * kBasis.getD0(iRow, iSample);
                    kB.set(iRow, 0, dBi);

                    for (int iCol = 0; iCol < iNumControlPoints; iCol++) {
                        double dAij = kA.get(iRow,iCol) +
                            kBasis.getD0(iRow, iSample) * kBasis.getD0(iCol, iSample);
                        kA.set(iRow, iCol, dAij);
                    }
                }
            }

            // Find the solution.
            Matrix kX = kA.solve(kB);

            // Copy the control point positions.
            for (int i = 0; i < iNumControlPoints; i++) {
                afControlPoint[i] = (float)kX.get(i,0);
                if (afControlPoint[i] < 0.0f) {
                  afControlPoint[i] = 0.0f;
                }
                else if (afControlPoint[i] > 1.0f) {
                  afControlPoint[i] = 1.0f;
                }
            }
        }

        return afControlPoint;
    }

    protected ModelSimpleImage m_kImageTrg; // input target image
    protected ModelSimpleImage m_kImageSrc; // input source image

    /**
     * Defines the cost measure for comparing the target image with
     * the registered image.
     */
    protected RegistrationMeasure m_kRegMeasure;

    /**
     * Total number of samples in target, registered source, and error images.
     */
    protected int m_iNumSamplesTrg;
}
