package gov.nih.mipav.model.algorithms;

import WildMagic.LibFoundation.Mathematics.*;
import gov.nih.mipav.model.structures.*;
import Jama.*;

import java.awt.*;



/**
 * Active Contour class optimized for Cell Tacking, based on the following paper:
 *
 * <p>"Active Contours for Cell Tracking", by Nilanjan Ray and Scott T. Acton. 5th IEEE Southwest Symposium on Image
 * Analysis and Interpretation (SSIAI'02), 2002.</p>
 *
 * <p>Most functionality is inherited from AlgorithmAGVF, the runSnake function is optimized with the following
 * constraints for cell-tracking:</p>
 *
 * <p>1). Constrained Gradient Vector Flow: using a Dirichlet boundary condition, the gradient inside the initial
 * user-defined snake is set to a user-defined velocity vector.</p>
 *
 * <p>2). Shape-size constraint: the shape and size of the snake is constrained to be circular with a user-defined cell
 * radius.</p>
 *
 * <p>3). Implicit Resampling: the points along the snake curve are minimized such that they maintain an equal distance
 * between points.</p>
 *
 * <p>The gradient vector flow is calculated for each frame in the image sequence. Once the GVF is calculated, the
 * points inside the initial snake (either the user-defined snake for frame 0, or the snake for the previous frame) are
 * set to the velocity vector. runSnake then minimizes the shape-size and resampling constraints for the new GVF, based
 * on the user-defined constraint contributions (see the Lambda member variable). The output snake is used as the
 * initial guess for the next frame in the image sequence.</p>
 *
 * @see  AlgorithmAGVF
 * @see  GenerateGaussian
 */
public class AlgorithmCellTrackingAGVF extends AlgorithmAGVF {

    //~ Instance fields ------------------------------------------------------------------------------------------------

    /** Previous snake results (X-position):. */
    private float[] m_afX = null;

    /** Previous snake results (Y-position):. */
    private float[] m_afY = null;

    /**
     * whether or not to dilate the cell contour before solving for the cell on the next frame (dilation helps capture
     * cells that are moving fast).
     */
    private boolean m_bDilate = true;

    /** boolean when true, use the velocity vector:. */
    private boolean m_bUseVelocity = true;

    /** Amount to dilate the cell (multiple of the cell radius). */
    private float m_fDilationFactor = 2.0f;

    /** expected cell radius:. */
    private float m_fK = 110;

    /** Constraint factor for the cell shape:. */
    private float m_fLambda1 = 1;

    /** Constraint factor for the cell size:. */
    private float m_fLambda2 = 1;

    /** Constraint factor for sampling:. */
    private float m_fLambda3 = 1;

    /** tolerance factor for snake evolution:. */
    private float m_fTolerance = 0.02f;

    /**
     * matrices that are initialized on the first frame, constant across frames. n is the number of points in the voi
     * (snake) contour:
     */
    /** Cos matrix: = cos( 2*pi*i/n). */
    private Matrix m_kC = null;

    /** G Matrix, equation 19:. */
    private Matrix m_kG = null;

    /** H Matrix, equation 19:. */
    private Matrix m_kH = null;

    /** Previous snake polygon:. */
    private Polygon m_kPreviousSnake = null;

    /** Q Matrix, equation 24 (inverse calculated once per image sequence on the inital frame):. */
    private Matrix m_kQ = null;

    /** Sin matrix: = sin( 2*pi*i/n). */
    private Matrix m_kS = null;

    /**
     * Velocity vector (either the initial user-defined guess, or the calculated velocity (new snake center - previous
     * snake center):.
     */
    private Vector2f m_kVelocity = null;

    //~ Constructors ---------------------------------------------------------------------------------------------------

    /**
     * Creates a new AlgorithmCellTrackingAGVF object.
     *
     * @param  resultImage           image of GVF field magnitude
     * @param  srcImg                2D or 3D source image
     * @param  sigmas                describe the scale of the gaussian in each dimension
     * @param  gvfIterations         iterations in calculating GVF field
     * @param  boundaryIterations    iterations in calculating boundary
     * @param  k                     GVF constant
     * @param  smoothness            factor for smoothing points on contour (see AlgorithmAGVF)
     * @param  srcVOI                VOI that is to be evolved
     * @param  do25D                 only applies to 3D, if true do slice by slice
     * @param  radiusConstraint      The user-defined cell radius
     * @param  shapeConstraint       The contribution of the shape constraint in the minimization
     * @param  sizeConstraint        The contribution of the size constraint in the minimization
     * @param  resamplingConstraint  The contribution of the resampling constraint in the minimization
     * @param  bDilate               Whether to dilate the cell radius before optimizing
     * @param  fDilation             dilation factor (multiple of the cell radius)
     * @param  fDx                   The initial cell velocity (x-component)
     * @param  fDy                   The initial cell velocity (y-component)
     */
    public AlgorithmCellTrackingAGVF(ModelImage resultImage, ModelImage srcImg, float[] sigmas, int gvfIterations,
                                     int boundaryIterations, float k, float smoothness, VOI srcVOI, boolean do25D,
                                     float radiusConstraint, float shapeConstraint, float sizeConstraint,
                                     float resamplingConstraint, boolean bDilate, float fDilation, float fDx,
                                     float fDy) {
        super(resultImage, srcImg, sigmas, gvfIterations, boundaryIterations, k, smoothness, srcVOI, do25D);

        /* set the constraint variables: */
        m_fLambda1 = shapeConstraint;
        m_fLambda2 = sizeConstraint;
        m_fLambda3 = resamplingConstraint;
        m_fK = radiusConstraint;
        m_bDilate = bDilate;
        m_fDilationFactor = fDilation;
        m_kVelocity = new Vector2f(fDx, fDy);

        if ((fDx == 0) && (fDy == 0)) {
            m_bUseVelocity = false;
        } else {
            m_kVelocity.Normalize();
        }
    }

    //~ Methods --------------------------------------------------------------------------------------------------------

    /**
     * Prepares this class for destruction.
     */
    public void finalize() {
        m_afX = null;
        m_afY = null;
        m_kVelocity = null;
        m_kPreviousSnake.reset();
        m_kPreviousSnake = null;
        super.finalize();
    }

    /**
     * Actual function that evolves the boundary.
     *
     * @param  xPoints    x coordinates that describe the contour
     * @param  yPoints    y coordinates that describe the contour
     * @param  u          x component of the GVF
     * @param  v          y component of the GVF
     * @param  resultGon  resultant polygon
     */
    protected void runSnake(float[] xPoints, float[] yPoints, float[] u, float[] v, Polygon resultGon) {

        /* Get the number of points in the snake, iNPoints: */
        int iNPoints = xPoints.length - 2;

        if ((m_afX == null) && (m_afY == null)) {
            m_kPreviousSnake = new Polygon();
            initSnakeConstants(iNPoints);
        }

        /* Create matrix-representations (Nx1) for the snake x-position,
         * y-position, x-component of the GVF, y-component of the GVF, Cosine
         * and Sine functions of the number of points: */
        Matrix kX = new Matrix(iNPoints, 1);
        Matrix kY = new Matrix(iNPoints, 1);
        Matrix kFx = new Matrix(iNPoints, 1);
        Matrix kFy = new Matrix(iNPoints, 1);
        int position;
        float fX, fY;

        for (int i = 0; i < iNPoints; i++) {

            /* kX and kY store the x and y coordinates of the snake: */
            if ((m_afX != null) && (m_afY != null)) {

                /* Not first snake, use snake from previous frame: */
                kX.set(i, 0, m_afX[i]);
                kY.set(i, 0, m_afY[i]);
            } else {

                /* First snake (user-defined): */
                kX.set(i, 0, xPoints[i + 1]);
                kY.set(i, 0, yPoints[i + 1]);

                /* Set initial snake values: */
                m_kPreviousSnake.addPoint(Math.round((float) kX.get(i, 0)), Math.round((float) kY.get(i, 0)));
            }

            /* Constrained Gradient Vector Flow: Check if the new point is
             * inside the initial snake, if so set the value of the Fx and Fy to be the velocity vector, otherwise use
             * the calculated
             * GVF : */
            if (m_bUseVelocity && m_kPreviousSnake.contains(kX.get(i, 0), kY.get(i, 0))) {
                kFx.set(i, 0, m_kVelocity.X);
                kFy.set(i, 0, m_kVelocity.Y);
            } else {
                fX = (float) Math.min(xDim - 1, Math.max(kX.get(i, 0), 0));
                fY = (float) Math.min(yDim - 1, Math.max(kY.get(i, 0), 0));

                /* kFx and kFy store the GVF for each of the points on the snake
                 * (updated each time the snake points move) */
                position = (int) fX + (xDim * (int) fY);
                kFx.set(i, 0, getBilinear(position, (float) (fX - (int) fX), (float) (fY - (int) fY), extents, u));
                kFy.set(i, 0, getBilinear(position, (float) (fX - (int) fX), (float) (fY - (int) fY), extents, v));
            }
        }

        /* fXBar = the average x-position over all points on the snake: */
        float fXBar = (float) (kX.norm1() / (float) iNPoints);

        /* fYBar = the average y-position over all points on the snake: */
        float fYBar = (float) (kY.norm1() / (float) iNPoints);

        /* Store the center point of the current snake: */
        Vector2f kCenter = new Vector2f(fXBar, fYBar);

        /* Equation 11: */
        /* kR used to compute the average radius: */
        Matrix kR = new Matrix(iNPoints, 1);

        for (int i = 0; i < iNPoints; i++) {

            /* Equation 11: */
            kR.set(i, 0,
                   (float)
                   Math.sqrt(((kX.get(i, 0) - fXBar) * (kX.get(i, 0) - fXBar)) +
                             ((kY.get(i, 0) - fYBar) * (kY.get(i, 0) - fYBar))));
        }

        /* Equation 11: */
        /* dRBar is the averate radius over all points on the snake: */
        float fRBar = (float) (kR.norm1() / (float) iNPoints);

        /* Equations 22, 23, 24: */
        /* kXr and kYr: */
        Matrix kXr = new Matrix(iNPoints, 1);
        Matrix kYr = new Matrix(iNPoints, 1);

        for (int i = 0; i < iNPoints; i++) {
            kXr.set(i, 0, (kX.get(i, 0) - fXBar) / kR.get(i, 0));
            kYr.set(i, 0, (kY.get(i, 0) - fYBar) / kR.get(i, 0));
        }

        /* Equations 15, 16: */
        /* kDx and kDy represent the distance between points on the
         * contour: */
        Matrix kDx = new Matrix(iNPoints, 1);
        Matrix kDy = new Matrix(iNPoints, 1);

        for (int i = 0; i < iNPoints; i++) {
            int iIndexI = ((i + 1) >= iNPoints) ? 0 : (i + 1);
            kDx.set(i, 0, fRBar * (m_kC.get(i, 0) - m_kC.get(iIndexI, 0)));
            kDy.set(i, 0, fRBar * (m_kS.get(i, 0) - m_kS.get(iIndexI, 0)));
        }

        float fDeltaT = 1.0f / ((float) boundaryIterations);

        /* Temporary variables, declared outside the loop:*/
        Matrix kXInc, kYInc, kDiffX, kDiffY;
        Matrix kXTemp = new Matrix(iNPoints, 1);
        Matrix kYTemp = new Matrix(iNPoints, 1);

        /* Iterate and minimize the constraints to find the optimal snake: */
        for (int t = 0; (t < boundaryIterations) && (!threadStopped); t++) {
            /* Xt+1 = Q-1 ( 1/dt * Xt + fx + lambda1 * (xbar + rbar * C) -
             * lambda2 (rbar - k) * xr + lambda3 * G * dx ) */
            /* Yt+1 = Q-1 ( 1/dt * Yt + fy + lambda1 * (ybar + rbar * S) -
             * lambda2 (rbar - k) * yr + lambda3 * G * dy ) */

            /* Calculate lambda3 * G * dx: */
            kDx = m_kG.times(kDx);

            /* Calculate lambda3 * G * dy: */
            kDy = m_kG.times(kDy);

            for (int i = 0; i < iNPoints; i++) {

                /* 1 / dt * xt + fx +
                 * lambda1 * ( xbar + rbar * C ) - lambda2 * ( rbar - K ) * xr + lambda3 * G * dx */
                kXTemp.set(i, 0,
                           (kX.get(i, 0) * fDeltaT) + kFx.get(i, 0) +
                           (m_fLambda1 * (fXBar + (fRBar * m_kC.get(i, 0)))) -
                           (m_fLambda2 * (fRBar - m_fK) * kXr.get(i, 0)) + kDx.get(i, 0));

                /* 1 / dt * yt + fy +
                 * lambda1 * ( ybar + rbar * S ) - lambda2 * ( rbar - K ) * yr +lambda3 * G * dy */
                kYTemp.set(i, 0,
                           (kY.get(i, 0) * fDeltaT) + kFy.get(i, 0) +
                           (m_fLambda1 * (fYBar + (fRBar * m_kS.get(i, 0)))) -
                           (m_fLambda2 * (fRBar - m_fK) * kYr.get(i, 0)) + kDy.get(i, 0));
            }

            kXInc = m_kQ.times(kXTemp);
            kYInc = m_kQ.times(kYTemp);

            /* Calculate difference vector, to see if the snake has moved <
             * tolerance: */
            kDiffX = kXInc.minus(kX);
            kDiffY = kYInc.minus(kY);

            if (((kDiffX.norm1() / (float) iNPoints) < m_fTolerance) &&
                    ((kDiffY.norm1() / (float) iNPoints) < m_fTolerance)) {
                break;
            }

            /* Update vars: */
            fXBar = (float) (kXInc.norm1() / (float) iNPoints);
            fYBar = (float) (kYInc.norm1() / (float) iNPoints);

            for (int i = 0; i < iNPoints; i++) {

                /* Copy into X, Y: */
                kX.set(i, 0, kXInc.get(i, 0));
                kY.set(i, 0, kYInc.get(i, 0));

                /* Constrained Gradient Vector Flow: Check if the new point is
                 * inside the initial snake, if so set the value of the Fx and Fy to be the velocity vector, otherwise
                 * use the calculated
                 * GVF : */
                if (m_bUseVelocity && m_kPreviousSnake.contains(kX.get(i, 0), kY.get(i, 0))) {
                    kFx.set(i, 0, m_kVelocity.X);
                    kFy.set(i, 0, m_kVelocity.Y);
                } else {
                    fX = (float) Math.min(xDim - 1, Math.max(kX.get(i, 0), 0));
                    fY = (float) Math.min(yDim - 1, Math.max(kY.get(i, 0), 0));

                    /* kFx and kFy store the GVF for each of the points on the snake
                     * (updated each time the snake points move) */
                    position = (int) fX + (xDim * (int) fY);
                    kFx.set(i, 0, getBilinear(position, (float) (fX - (int) fX), (float) (fY - (int) fY), extents, u));
                    kFy.set(i, 0, getBilinear(position, (float) (fX - (int) fX), (float) (fY - (int) fY), extents, v));
                    kR.set(i, 0,
                           (float)
                           Math.sqrt(((kX.get(i, 0) - fXBar) * (kX.get(i, 0) - fXBar)) +
                                     ((kY.get(i, 0) - fYBar) * (kY.get(i, 0) - fYBar))));

                    kXr.set(i, 0, (kX.get(i, 0) - fXBar) / kR.get(i, 0));
                    kYr.set(i, 0, (kY.get(i, 0) - fYBar) / kR.get(i, 0));
                }
            }

            fRBar = (float) (kR.norm1() / (float) iNPoints);

            for (int i = 0; i < iNPoints; i++) {
                int iIndexI = ((i + 1) >= iNPoints) ? 0 : (i + 1);
                kDx.set(i, 0, fRBar * (m_kC.get(i, 0) - m_kC.get(iIndexI, 0)));
                kDy.set(i, 0, fRBar * (m_kS.get(i, 0) - m_kS.get(iIndexI, 0)));
            }
        }

        /* Calculate new centers: */
        fXBar = (float) (kX.norm1() / (float) iNPoints);
        fYBar = (float) (kY.norm1() / (float) iNPoints);

        /* Store the final results to initialize snake for the next frame: */
        if (m_afX == null) {
            m_afX = new float[iNPoints];
            m_afY = new float[iNPoints];
        }

        /* Copy the final results into the snake: */
        resultGon.reset();
        m_kPreviousSnake.reset();

        Vector2f kVec = new Vector2f();
        float fLength;

        for (int i = 0; i < iNPoints; i++) {
            resultGon.addPoint(Math.round((float) kX.get(i, 0)), Math.round((float) kY.get(i, 0)));
            m_kPreviousSnake.addPoint(Math.round((float) kX.get(i, 0)), Math.round((float) kY.get(i, 0)));

            if (m_bDilate) {
                kVec.X = (float) kX.get(i, 0) - fXBar;
                kVec.Y = (float) kY.get(i, 0) - fYBar;
                fLength = kVec.Length();
                kVec.Normalize();

                m_afX[i] = fXBar + (kVec.X * fLength * m_fDilationFactor);
                m_afY[i] = fYBar + (kVec.Y * fLength * m_fDilationFactor);
            } else {
                m_afX[i] = (float) kX.get(i, 0);
                m_afY[i] = (float) kY.get(i, 0);
            }
        }

        /* update velocity: */
        m_kVelocity.X = fXBar - kCenter.X;
        m_kVelocity.Y = fYBar - kCenter.Y;
        m_kVelocity.Normalize();

        return;
    }

    /**
     * Initializes the H, G, and Q matrices once per image sequence on the first frame. The matrix inverse only has to
     * be computed once.
     *
     * @param  iNPoints  the number of points in the initial VOI (snake)
     */
    private void initSnakeConstants(int iNPoints) {

        /* The cosine and sine matrices:  */
        m_kC = new Matrix(iNPoints, 1);
        m_kS = new Matrix(iNPoints, 1);

        /* Equations 17, 18, 19: */
        /* Matrix m_kH and m_kG are NxN matrices: */
        m_kH = new Matrix(iNPoints, iNPoints, 0);
        m_kG = new Matrix(iNPoints, iNPoints, 0);

        for (int i = 0; i < iNPoints; i++) {

            /* m_kC and m_kS are the cosine and sine functions of the snake
             * point(i), calculated once and stored: */
            m_kC.set(i, 0, (float) Math.cos((2 * Math.PI * i) / (float) iNPoints));
            m_kS.set(i, 0, (float) Math.sin((2 * Math.PI * i) / (float) iNPoints));

            for (int j = 0; j < iNPoints; j++) {

                if (i == j) {
                    m_kH.set(i, j, 2);

                    int iIndexI = ((i - 1) < 0) ? (iNPoints - 1) : (i - 1);
                    int iIndexJ = ((j - 1) < 0) ? (iNPoints - 1) : (j - 1);
                    m_kH.set(iIndexI, j, -1);
                    m_kH.set(i, iIndexJ, -1);

                    m_kG.set(i, j, 1);
                    m_kG.set(i, iIndexJ, -1);
                }
            }
        }

        /* pre-multiply: lambda3 * G: */
        m_kG.timesEquals(m_fLambda3);

        /* Equation 24: Calculate and store Q matrix: */
        float fDeltaT = 1.0f / ((float) boundaryIterations);
        m_kQ = Matrix.identity(iNPoints, iNPoints);
        m_kQ.timesEquals(fDeltaT);

        Matrix kLambda1 = Matrix.identity(iNPoints, iNPoints);
        kLambda1.timesEquals(m_fLambda1);
        m_kQ.plusEquals(kLambda1);
        m_kH.timesEquals(m_fLambda3);
        m_kQ.plusEquals(m_kH);
        m_kQ = m_kQ.inverse();
    }

}
