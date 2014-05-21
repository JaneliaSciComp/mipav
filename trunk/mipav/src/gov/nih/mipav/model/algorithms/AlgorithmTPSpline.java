package gov.nih.mipav.model.algorithms;


import gov.nih.mipav.model.file.*;
import gov.nih.mipav.model.structures.*;
import Jama.*;

import gov.nih.mipav.view.*;

import java.io.*;
import java.util.BitSet;


/**
 * Thin plate spline Warning: If the supplied (x,y) data set in setupTPSpline2D is nearly collinear, singular matrices
 * will result and a matrix inversion will fail. This code calculating the function f at interpolated points is a port
 * of software from Geometric Tools at www.geometrictools.com. Reference: Thin Plate Splines by David Eberly of
 * Geometric Tools at http://www.geometrictools.com/SampleMathematics/ThinPlateSplines/ThinPlateSplines.html.
 *
 * <p>The Eberly Thin plate splines 2D is a physically based 2D interpolation scheme for arbitrarily spaced tabulated
 * data (xi,yi,f(xi,yi)). These splines are the generalization of the natural cubic splines in 1D. The spline surface
 * represents a thin metal sheet that is constrained not to move at the grid points. The Eberly Thin plate spines 3D
 * operates on data of the form (xi,yi,zi,f(xi,yi,zi).</p>
 *
 * <p>Also given is thin plate spline code for warping from one x,y source set to another x',y' target set. This code is
 * based on the paper Principal Warps: Thin-Plate Splines and the Decompositions of Deformations by Fred L. BookStein,
 * IEEE Transactions on Pattern Analysis and Machine Intelligence, Vol. 11, No. 6, June, 1989, pp. 567 - 585.</p>
 */

public class AlgorithmTPSpline extends AlgorithmBase {

    //~ Instance fields ------------------------------------------------------------------------------------------------

    /** DOCUMENT ME! */
    private ModelImage baseImage;

    /** DOCUMENT ME! */
    private float[][] C;

    /** DOCUMENT ME! */
    private float[] mA;

    /** DOCUMENT ME! */
    private ModelImage matchImage;

    /** DOCUMENT ME! */
    private float[] mB;

    /** DOCUMENT ME! */
    private float[] mX;

    /** DOCUMENT ME! */
    private float[] mY;

    /** DOCUMENT ME! */
    private float[] mZ;

    /** DOCUMENT ME! */
    private int N;

    /** DOCUMENT ME! */
    private ModelImage resultImage;

    /** DOCUMENT ME! */
    private float smooth;

    /** DOCUMENT ME! */
    private double[] x;

    /** DOCUMENT ME! */
    private float xInvRange;

    /** DOCUMENT ME! */
    private float xMin;

    /** DOCUMENT ME! */
    private double[] xTar;

    /** DOCUMENT ME! */
    private double[] y;

    /** DOCUMENT ME! */
    private float yInvRange;

    /** DOCUMENT ME! */
    private float yMin;

    /** DOCUMENT ME! */
    private double[] yTar;

    /** DOCUMENT ME! */
    private double[] z;

    /** DOCUMENT ME! */
    private float zInvRange;

    /** DOCUMENT ME! */
    private float zMin;

    /** DOCUMENT ME! */
    private double[] zTar;
    
    private boolean setupRequired = true;
    
    private boolean run2D = true;
    
    private int xDimA;
    
    private int yDimA;
    
    private int zDimA;
    
    private boolean doSetupOnly = false;

    //~ Constructors ---------------------------------------------------------------------------------------------------

    /**
     * AlgorithmTPSpline - default constructor.
     */
    public AlgorithmTPSpline() { }

    /**
     * AlgorithmTPSpline - constructor for 2D case.
     *
     * @param  xSource     array with x coordinates of source data
     * @param  ySource     array with y coordinates of source data
     * @param  xTar        array with x coordinates of target data
     * @param  yTar        array with y coordinates of target data
     * @param  smooth      cannot be negative. If the smoothing term is nonzero, the interpolation is not exact
     * @param  baseImage   DOCUMENT ME!
     * @param  matchImage  DOCUMENT ME!
     */
    public AlgorithmTPSpline(double[] xSource, double[] ySource, double[] xTar, double[] yTar, float smooth,
                             ModelImage baseImage, ModelImage matchImage) {
        double xSum;
        double xSqSum;
        double ySum;
        double ySqSum;
        double xySum;
        double xStd;
        double yStd;
        double corr; // correlation coefficient
        int i;

        this.x = xSource;
        this.y = ySource;
        this.z = null;
        this.xTar = xTar;
        this.yTar = yTar;
        this.zTar = null;
        this.smooth = smooth;
        this.baseImage = baseImage;
        this.matchImage = matchImage;

        N = x.length;

        if (N < 3) {
            MipavUtil.displayError("AlgorithmTPSpline: x source array length cannot be less than 3");

            return;
        }

        if (N != y.length) {
            MipavUtil.displayError("AlgorithmTPSpline: x source array length does not equal y source array length");

            return;
        }

        if (N != xTar.length) {
            MipavUtil.displayError("AlgorithmTPSpline: x source array length does not equal x target array length");

            return;
        }

        if (N != yTar.length) {
            MipavUtil.displayError("AlgorithmTPSpline: x source array length does not equal y target array length");

            return;
        }

        if (smooth < 0.0f) {
            MipavUtil.displayError("AlgorithmTPSpline: smooth cannot be negative");

            return;
        }

        xSum = x[0];
        ySum = y[0];
        xSqSum = x[0] * x[0];
        ySqSum = y[0] * y[0];
        xySum = x[0] * y[0];

        for (i = 1; i < N; i++) {
            xSum += x[i];
            ySum += y[i];
            xSqSum += x[i] * x[i];
            ySqSum += y[i] * y[i];
            xySum += x[i] * y[i];
        } // for (i = 0; i < N; i++)

        xStd = (float) Math.sqrt((xSqSum - (xSum * xSum / N)) / (N - 1));
        yStd = (float) Math.sqrt((ySqSum - (ySum * ySum / N)) / (N - 1));
        corr = (xySum - (xSum * ySum / N)) / ((N - 1) * xStd * yStd);

        if ((corr > 0.95f) || (corr < -0.95f)) {
            MipavUtil.displayWarning("AlgorithmTPSpline: May fail because points too collinear Correlation coefficient = " +
                                     corr);
        } // if (corr > 0.95f) || (corr < -0.95f))
    }

    /**
     * AlgorithmTPSpline - constructor for 3D case.
     *
     * @param  xSource     array with x coordinates of source data
     * @param  ySource     array with y coordinates of source data
     * @param  zSource     array with z coordinates of source data
     * @param  xTar        array with x coordinates of target data
     * @param  yTar        array with y coordinates of target data
     * @param  zTar        array with z coordinates of target data
     * @param  smooth      cannot be negative. * If the smoothing term is nonzero, the interpolation is not exact
     * @param  baseImage   DOCUMENT ME!
     * @param  matchImage  DOCUMENT ME!
     */
    public AlgorithmTPSpline(double[] xSource, double[] ySource, double[] zSource, double[] xTar, double[] yTar,
                             double[] zTar, float smooth, ModelImage baseImage, ModelImage matchImage) {
        this.x = xSource;
        this.y = ySource;
        this.z = zSource;
        this.xTar = xTar;
        this.yTar = yTar;
        this.zTar = zTar;
        this.smooth = smooth;
        this.baseImage = baseImage;
        this.matchImage = matchImage;

        N = x.length;

        if (N < 4) {
            MipavUtil.displayError("AlgorithmTPSpline: x source array length cannot be less than 4");

            return;
        }

        if (N != y.length) {
            MipavUtil.displayError("AlgorithmTPSpline: x source array length does not equal y source array length");

            return;
        }

        if (N != z.length) {
            MipavUtil.displayError("AlgorithmTPSpline: x source array length does not equal z source array length");

            return;
        }

        if (N != xTar.length) {
            MipavUtil.displayError("AlgorithmTPSpline: x source array length does not equal x target array length");

            return;
        }

        if (N != yTar.length) {
            MipavUtil.displayError("AlgorithmTPSpline: x source array length does not equal y target array length");

            return;
        }

        if (N != zTar.length) {
            MipavUtil.displayError("AlgorithmTPSpline: x source array length does not equal z target array length");

            return;
        }

        if (smooth < 0.0f) {
            MipavUtil.displayError("AlgorithmTPSPline: smooth cannot be negative");

            return;
        }
    }
    
    
    
    
    /**
     * AlgorithmTPSpline - constructor for 3D case.
     *
     * @param  xSource     array with x coordinates of source data
     * @param  ySource     array with y coordinates of source data
     * @param  zSource     array with z coordinates of source data
     * @param  xTar        array with x coordinates of target data
     * @param  yTar        array with y coordinates of target data
     * @param  zTar        array with z coordinates of target data
     * @param  smooth      cannot be negative. * If the smoothing term is nonzero, the interpolation is not exact
     * @param  baseImage   DOCUMENT ME!
     * @param  matchImage  DOCUMENT ME!
     */
    public AlgorithmTPSpline(double[] xSource, double[] ySource, double[] zSource, double[] xTar, double[] yTar,
                             double[] zTar, float smooth, ModelImage baseImage, ModelImage matchImage, boolean doSetupOnly) {
        this.x = xSource;
        this.y = ySource;
        this.z = zSource;
        this.xTar = xTar;
        this.yTar = yTar;
        this.zTar = zTar;
        this.smooth = smooth;
        this.baseImage = baseImage;
        this.matchImage = matchImage;
        this.doSetupOnly = doSetupOnly;

        N = x.length;

        if (N < 4) {
            MipavUtil.displayError("AlgorithmTPSpline: x source array length cannot be less than 4");

            return;
        }

        if (N != y.length) {
            MipavUtil.displayError("AlgorithmTPSpline: x source array length does not equal y source array length");

            return;
        }

        if (N != z.length) {
            MipavUtil.displayError("AlgorithmTPSpline: x source array length does not equal z source array length");

            return;
        }

        if (N != xTar.length) {
            MipavUtil.displayError("AlgorithmTPSpline: x source array length does not equal x target array length");

            return;
        }

        if (N != yTar.length) {
            MipavUtil.displayError("AlgorithmTPSpline: x source array length does not equal y target array length");

            return;
        }

        if (N != zTar.length) {
            MipavUtil.displayError("AlgorithmTPSpline: x source array length does not equal z target array length");

            return;
        }

        if (smooth < 0.0f) {
            MipavUtil.displayError("AlgorithmTPSPline: smooth cannot be negative");

            return;
        }
    }
    
    /**
     * Constructor used when N, xDimA, yDimA, zDimA, x[], y[], z[], and C[][] are read from a file
     * @param matchImage
     */
    public AlgorithmTPSpline(ModelImage matchImage) {
        this.matchImage = matchImage;
    }

    //~ Methods --------------------------------------------------------------------------------------------------------

    /**
     * finalize -
     */
    public void finalize() {
        mX = null;
        mY = null;
        mZ = null;
        mA = null;
        mB = null;

        if (C != null) {

            for (int i = 0; i < C.length; i++) {
                C[i] = null;
            }

            C = null;
        }

        super.finalize();
    }

    /**
     * DOCUMENT ME!
     *
     * @return  resultImage
     */
    public ModelImage getResultImage() {
        return resultImage;
    }

    /**
     * DOCUMENT ME!
     *
     * @param   fT  DOCUMENT ME!
     *
     * @return  DOCUMENT ME!
     */
    public final double kernel(double fT) {
        double fT2;

        if (fT > 0.0) {
            fT2 = fT * fT;

            return fT2 * Math.log(fT2);
        } else {
            return 0.0;
        }

    }

    /**
     * run - starts the program.
     */
    public void runAlgorithm() {

        try {

            if (setupRequired) {
                fireProgressStateChanged(matchImage.getImageName(), "Calculating spline coefficients ...");
                if (z == null) {
                    setupTPSpline2D();
                    tpSpline2D();
                } else {
                	if(doSetupOnly) {
                		setupTPSpline3D();
                	}else {
                		setupTPSpline3D();
                        tpSpline3D();
                	}
                    
                }
            } // if (setupRequired)
            else {
                if (run2D) {
                    tpSpline2D();
                }
                else {
                    tpSpline3D();
                }
            }
        } catch (OutOfMemoryError error) {
            System.gc();
            MipavUtil.displayError("JDialogRegistrationTPSpline: Out of memory on progressBar" + error);
            finalize();
            setCompleted(false);

            return;
        } finally { }
    }

    /**
     * setupTPSpline2D - computes the thin plate spline coefficients.
     */
    public void setupTPSpline2D() {
        int iRow, iCol;
        double dx, dy;
        double fT;
        double[][] L;
        double[][] V;

        // compute matrix L
        L = new double[N + 3][N + 3];

        for (iRow = 0; iRow < N; iRow++) {

            for (iCol = 0; iCol < N; iCol++) {

                if (iRow == iCol) {
                    L[iRow][iCol] = smooth;
                } else {
                    dx = x[iRow] - x[iCol];
                    dy = y[iRow] - y[iCol];
                    fT = Math.sqrt((dx * dx) + (dy * dy));
                    L[iRow][iCol] = kernel(fT);
                }
            } // for (iCol = 0; iCol < N; iCol++)
        } // for (iRow = 0; iRow < N; iRow++)

        for (iCol = 0; iCol < N; iCol++) {
            L[N][iCol] = 1.0;
            L[N + 1][iCol] = x[iCol];
            L[N + 2][iCol] = y[iCol];
        }

        for (iRow = 0; iRow < N; iRow++) {
            L[iRow][N] = 1.0;
            L[iRow][N + 1] = x[iRow];
            L[iRow][N + 2] = y[iRow];
        }

        for (iRow = N; iRow < (N + 3); iRow++) {

            for (iCol = N; iCol < (N + 3); iCol++) {
                L[iRow][iCol] = 0.0;
            }
        }

        // Take the inverse of L, store result in L
        L = ((new Matrix(L)).inverse()).getArray();

        V = new double[N + 3][2];

        for (iRow = 0; iRow < N; iRow++) {
            V[iRow][0] = xTar[iRow];
            V[iRow][1] = yTar[iRow];
        }

        for (iRow = N; iRow < (N + 3); iRow++) {
            V[iRow][0] = 0.0;
            V[iRow][1] = 0.0;
        }

        // C contains the coefficents of W, a1, ax, and ay
        C = new float[N + 3][2];

        for (iRow = 0; iRow < (N + 3); iRow++) {

            for (iCol = 0; iCol < 2; iCol++) {
                C[iRow][iCol] = 0.0f;
            }
        }

        for (iRow = 0; iRow < (N + 3); iRow++) {

            for (iCol = 0; iCol < (N + 3); iCol++) {
                C[iRow][0] += (float) (L[iRow][iCol] * V[iCol][0]);
                C[iRow][1] += (float) (L[iRow][iCol] * V[iCol][1]);
            }
        }

        // clean up
        L = null;
        V = null;

    }

    /**
     * setupTPSpline2D computes the thin plate spline coefficients.
     *
     * @param  x       array with x coordinates of data
     * @param  y       array with y coordinates of data
     * @param  f       array with values of data at (x,y) locations
     * @param  smooth  nonnegative smoothing parameter
     */
    public void setupTPSpline2D(float[] x, float[] y, float[] f, float smooth) {
        int i, iRow, iCol;
        float xMax;
        float yMax;
        float dx, dy;
        double fT;
        double[][] A;
        float[][] B;
        float[][] P;
        double[][] Q;
        float[] prod;
        float[] tmp;
        float xSum;
        float xSqSum;
        float ySum;
        float ySqSum;
        float xySum;
        float xStd;
        float yStd;
        float corr; // correlation coefficient

        N = x.length;

        if (N < 3) {
            MipavUtil.displayError("setupTPSpline2D: x array length cannot be less than 3");

            return;
        }

        if (N != y.length) {
            MipavUtil.displayError("setupTPSpline2D: x array length does not equal y array length");

            return;
        }

        if (N != f.length) {
            MipavUtil.displayError("setupTPSpline2D: x array length does not equal f array length");

            return;
        }

        if (smooth < 0.0f) {
            MipavUtil.displayError("setupTPSpline2D: smooth cannot be negative");

            return;
        }

        xSum = x[0];
        ySum = y[0];
        xSqSum = x[0] * x[0];
        ySqSum = y[0] * y[0];
        xySum = x[0] * y[0];

        for (i = 1; i < N; i++) {
            xSum += x[i];
            ySum += y[i];
            xSqSum += x[i] * x[i];
            ySqSum += y[i] * y[i];
            xySum += x[i] * y[i];
        } // for (i = 0; i < N; i++)

        xStd = (float) Math.sqrt((xSqSum - (xSum * xSum / N)) / (N - 1));
        yStd = (float) Math.sqrt((ySqSum - (ySum * ySum / N)) / (N - 1));
        corr = (xySum - (xSum * ySum / N)) / ((N - 1) * xStd * yStd);

        if ((corr > 0.95f) || (corr < -0.95f)) {
            MipavUtil.displayWarning("setupTPSpline2D: May fail because points too collinear Correlation coefficient = " +
                                     corr);
        } // if (corr > 0.95f) || (corr < -0.95f))

        mX = new float[N];
        mY = new float[N];
        mA = new float[N];

        // map input x,y to unit square
        xMin = x[0];
        xMax = xMin;

        for (i = 1; i < N; i++) {

            if (x[i] < xMin) {
                xMin = x[i];
            }

            if (x[i] > xMax) {
                xMax = x[i];
            }
        } // for (i = 1; i < N; i++)

        if (xMax == xMin) {
            MipavUtil.displayError("setupTPSpline2D: x array cannot be single valued");

            return;
        }

        xInvRange = 1.0f / (xMax - xMin);

        for (i = 0; i < N; i++) {
            mX[i] = (x[i] - xMin) * xInvRange;
        }

        yMin = y[0];
        yMax = yMin;

        for (i = 1; i < N; i++) {

            if (y[i] < yMin) {
                yMin = y[i];
            }

            if (y[i] > yMax) {
                yMax = y[i];
            }
        } // for (i = 1; i < N; i++)

        if (yMax == yMin) {
            MipavUtil.displayError("setupTPSpline2D: y array cannot be single valued");

            return;
        }

        yInvRange = 1.0f / (yMax - yMin);

        for (i = 0; i < N; i++) {
            mY[i] = (y[i] - yMin) * yInvRange;
        }

        // compute matrix A = E + smooth * I[N X N matrix]
        A = new double[N][N];

        for (iRow = 0; iRow < N; iRow++) {

            for (iCol = 0; iCol < N; iCol++) {

                if (iRow == iCol) {
                    A[iRow][iCol] = smooth;
                } else {
                    dx = mX[iRow] - mX[iCol];
                    dy = mY[iRow] - mY[iCol];
                    fT = Math.sqrt((dx * dx) + (dy * dy));
                    A[iRow][iCol] = kernel(fT);
                }
            } // for (iCol = 0; iCol < N; iCol++)
        } // for (iRow = 0; iRow < N; iRow++)

        // compute matrix B[N X 3 matrix]
        B = new float[N][3];

        for (iRow = 0; iRow < N; iRow++) {
            B[iRow][0] = 1.0f;
            B[iRow][1] = mX[iRow];
            B[iRow][2] = mY[iRow];
        } // for (iRow = 0; iRow < N; iRow++)

        // Take the inverse of A, store result in A
        A = ((new Matrix(A)).inverse()).getArray();

        // compute P = B^tA^{-1} [3 X N matrix]
        P = new float[3][N];

        for (iRow = 0; iRow < 3; iRow++) {

            for (iCol = 0; iCol < N; iCol++) {
                P[iRow][iCol] = 0.0f;

                for (i = 0; i < N; i++) {
                    P[iRow][iCol] += B[i][iRow] * A[i][iCol];
                }
            }
        } // for (iRow = 0; iRow < 3; iRow++)

        // compute Q = PB = B^t A^{-1} B [3 X 3 matrix]
        Q = new double[3][3];

        for (iRow = 0; iRow < 3; iRow++) {

            for (iCol = 0; iCol < 3; iCol++) {
                Q[iRow][iCol] = 0.0;

                for (i = 0; i < N; i++) {
                    Q[iRow][iCol] += P[iRow][i] * B[i][iCol];
                }
            }
        } // for (iRow = 0; iRow < 3; iRow++)

        // Take the inverse of Q, store result in Q
        Q = ((new Matrix(Q)).inverse()).getArray();

        // compute P*f
        prod = new float[3];

        for (iRow = 0; iRow < 3; iRow++) {
            prod[iRow] = 0.0f;

            for (i = 0; i < N; i++) {
                prod[iRow] += P[iRow][i] * f[i];
            }
        } // for (iRow = 0; iRow < 3; iRow++)

        // compute 'b' vector for smooth thin plate spline
        mB = new float[3];

        for (iRow = 0; iRow < 3; iRow++) {
            mB[iRow] = 0.0f;

            for (i = 0; i < 3; i++) {
                mB[iRow] += Q[iRow][i] * prod[i];
            }
        } // for (iRow = 0; iRow < 3; iRow++)

        // compute f-B*b
        tmp = new float[N];

        for (iRow = 0; iRow < N; iRow++) {
            tmp[iRow] = f[iRow];

            for (i = 0; i < 3; i++) {
                tmp[iRow] -= B[iRow][i] * mB[i];
            }
        } // for (iRow = 0; iRow < N; iRow++)

        // compute 'a' vector for smooth thin plate spline
        for (iRow = 0; iRow < N; iRow++) {
            mA[iRow] = 0.0f;

            for (i = 0; i < N; i++) {
                mA[iRow] += A[iRow][i] * tmp[i];
            }
        } // for (iRow = 0; iRow < N; iRow++)

        // clean up
        tmp = null;
        A = null;
        B = null;
        P = null;
        Q = null;

    }

    /**
     * setupTPSpline2D computes the thin plate spline coefficients.
     *
     * @param  xSource  array with x coordinates of source data
     * @param  ySource  array with y coordinates of source data
     * @param  xTar     array with x coordinates of target data
     * @param  yTar     array with y coordinates of target data
     * @param  smooth   cannot be negative If the smoothing term is nonzero, the interpolation is not exact
     */
    public void setupTPSpline2D(double[] xSource, double[] ySource, double[] xTar, double[] yTar, float smooth) {
        int i, iRow, iCol;
        double dx, dy;
        double fT;
        double[][] L;
        double[][] V;
        double xSum;
        double xSqSum;
        double ySum;
        double ySqSum;
        double xySum;
        double xStd;
        double yStd;
        double corr; // correlation coefficient

        this.x = xSource;
        this.y = ySource;

        N = x.length;

        if (N < 3) {
            MipavUtil.displayError("setupTPSpline2D: x source array length cannot be less than 3");

            return;
        }

        if (N != y.length) {
            MipavUtil.displayError("setupTPSpline2D: x source array length does not equal y source array length");

            return;
        }

        if (N != xTar.length) {
            MipavUtil.displayError("setupTPSpline2D: x source array length does not equal x target array length");

            return;
        }

        if (N != yTar.length) {
            MipavUtil.displayError("setupTPSpline2D: x source array length does not equal y target array length");

            return;
        }

        if (smooth < 0.0f) {
            MipavUtil.displayError("setupTPSpline2D: smooth cannot be negative");

            return;
        }

        xSum = x[0];
        ySum = y[0];
        xSqSum = x[0] * x[0];
        ySqSum = y[0] * y[0];
        xySum = x[0] * y[0];

        for (i = 1; i < N; i++) {
            xSum += x[i];
            ySum += y[i];
            xSqSum += x[i] * x[i];
            ySqSum += y[i] * y[i];
            xySum += x[i] * y[i];
        } // for (i = 0; i < N; i++)

        xStd = (float) Math.sqrt((xSqSum - (xSum * xSum / N)) / (N - 1));
        yStd = (float) Math.sqrt((ySqSum - (ySum * ySum / N)) / (N - 1));
        corr = (xySum - (xSum * ySum / N)) / ((N - 1) * xStd * yStd);

        if ((corr > 0.95f) || (corr < -0.95f)) {
            MipavUtil.displayWarning("setupTPSpline2D: May fail because points too collinear Correlation coefficient = " +
                                     corr);
        } // if (corr > 0.95f) || (corr < -0.95f))

        // compute matrix L
        L = new double[N + 3][N + 3];

        for (iRow = 0; iRow < N; iRow++) {

            for (iCol = 0; iCol < N; iCol++) {

                if (iRow == iCol) {
                    L[iRow][iCol] = smooth;
                } else {
                    dx = x[iRow] - x[iCol];
                    dy = y[iRow] - y[iCol];
                    fT = Math.sqrt((dx * dx) + (dy * dy));
                    L[iRow][iCol] = kernel(fT);
                }
            } // for (iCol = 0; iCol < N; iCol++)
        } // for (iRow = 0; iRow < N; iRow++)

        for (iCol = 0; iCol < N; iCol++) {
            L[N][iCol] = 1.0;
            L[N + 1][iCol] = x[iCol];
            L[N + 2][iCol] = y[iCol];
        }

        for (iRow = 0; iRow < N; iRow++) {
            L[iRow][N] = 1.0;
            L[iRow][N + 1] = x[iRow];
            L[iRow][N + 2] = y[iRow];
        }

        for (iRow = N; iRow < (N + 3); iRow++) {

            for (iCol = N; iCol < (N + 3); iCol++) {
                L[iRow][iCol] = 0.0;
            }
        }

        // Take the inverse of L, store result in L
        L = ((new Matrix(L)).inverse()).getArray();
        
        V = new double[N + 3][2];

        for (iRow = 0; iRow < N; iRow++) {
            V[iRow][0] = xTar[iRow];
            V[iRow][1] = yTar[iRow];
        }

        for (iRow = N; iRow < (N + 3); iRow++) {
            V[iRow][0] = 0.0;
            V[iRow][1] = 0.0;
        }

        // C contains the coefficents of W, a1, ax, and ay
        C = new float[N + 3][2];

        for (iRow = 0; iRow < (N + 3); iRow++) {

            for (iCol = 0; iCol < 2; iCol++) {
                C[iRow][iCol] = 0.0f;
            }
        }

        for (iRow = 0; iRow < (N + 3); iRow++) {

            for (iCol = 0; iCol < (N + 3); iCol++) {
                C[iRow][0] += (float) (L[iRow][iCol] * V[iCol][0]);
                C[iRow][1] += (float) (L[iRow][iCol] * V[iCol][1]);
            }
        }

        // clean up
        L = null;
        V = null;

    }

    /**
     * setupTPSpline3D - computes the thin plate spline coefficients.
     */
    public void setupTPSpline3D() {
        int iRow, iCol;
        double dx, dy, dz;
        double fT;
        double[][] L;
        double[][] V;

        // compute matrix L
        L = new double[N + 4][N + 4];

        for (iRow = 0; iRow < N; iRow++) {

            for (iCol = 0; iCol < N; iCol++) {

                if (iRow == iCol) {
                    L[iRow][iCol] = smooth;
                } else {
                    dx = x[iRow] - x[iCol];
                    dy = y[iRow] - y[iCol];
                    dz = z[iRow] - z[iCol];
                    fT = Math.sqrt((dx * dx) + (dy * dy) + (dz * dz));
                    L[iRow][iCol] = fT;
                }
            } // for (iCol = 0; iCol < N; iCol++)
        } // for (iRow = 0; iRow < N; iRow++)

        for (iCol = 0; iCol < N; iCol++) {
            L[N][iCol] = 1.0;
            L[N + 1][iCol] = x[iCol];
            L[N + 2][iCol] = y[iCol];
            L[N + 3][iCol] = z[iCol];
        }

        for (iRow = 0; iRow < N; iRow++) {
            L[iRow][N] = 1.0;
            L[iRow][N + 1] = x[iRow];
            L[iRow][N + 2] = y[iRow];
            L[iRow][N + 3] = z[iRow];
        }

        for (iRow = N; iRow < (N + 4); iRow++) {

            for (iCol = N; iCol < (N + 4); iCol++) {
                L[iRow][iCol] = 0.0;
            }
        }

        // Take the inverse of L, store result in L
        L = ((new Matrix(L)).inverse()).getArray();

        V = new double[N + 4][3];

        for (iRow = 0; iRow < N; iRow++) {
            V[iRow][0] = xTar[iRow];
            V[iRow][1] = yTar[iRow];
            V[iRow][2] = zTar[iRow];
        }

        for (iRow = N; iRow < (N + 4); iRow++) {
            V[iRow][0] = 0.0;
            V[iRow][1] = 0.0;
            V[iRow][2] = 0.0;
        }

        // C contains the coefficents of W, a1, ax, ay, and az
        C = new float[N + 4][3];

        for (iRow = 0; iRow < (N + 4); iRow++) {

            for (iCol = 0; iCol < 3; iCol++) {
                C[iRow][iCol] = 0.0f;
            }
        }

        for (iRow = 0; iRow < (N + 4); iRow++) {

            for (iCol = 0; iCol < (N + 4); iCol++) {
                C[iRow][0] += (float) (L[iRow][iCol] * V[iCol][0]);
                C[iRow][1] += (float) (L[iRow][iCol] * V[iCol][1]);
                C[iRow][2] += (float) (L[iRow][iCol] * V[iCol][2]);
            }
        }

        // clean up
        L = null;
        V = null;

    }

    /**
     * setupTPSpline3D computes the thin plate spline coefficients.
     *
     * @param  x       array with x coordinates of data
     * @param  y       array with y coordinates of data
     * @param  z       array with z coordinates of data
     * @param  f       array with values of data at (x,y,z) locations
     * @param  smooth  nonnegative smoothing parameter
     */
    public void setupTPSpline3D(float[] x, float[] y, float[] z, float[] f, float smooth) {
        int i, iRow, iCol;
        float xMax;
        float yMax;
        float zMax;
        float dx, dy, dz;
        double fT;
        double[][] A;
        float[][] B;
        float[][] P;
        double[][] Q;
        float[] prod;
        float[] tmp;


        N = x.length;

        if (N < 4) {
            MipavUtil.displayError("setupTPSpline3D: x array length cannot be less than 4");

            return;
        }

        if (N != y.length) {
            MipavUtil.displayError("setupTPSpline3D: x array length does not equal y array length");

            return;
        }

        if (N != z.length) {
            MipavUtil.displayError("SetupTPSpline3D: x array length does not equal z array length");

            return;
        }

        if (N != f.length) {
            MipavUtil.displayError("setupTPSpline3D: x array length does not equal f array length");

            return;
        }

        if (smooth < 0.0f) {
            MipavUtil.displayError("setupTPSpline3D: smooth cannot be negative");

            return;
        }

        mX = new float[N];
        mY = new float[N];
        mZ = new float[N];
        mA = new float[N];

        // map input x,y,z to unit cube
        xMin = x[0];
        xMax = xMin;

        for (i = 1; i < N; i++) {

            if (x[i] < xMin) {
                xMin = x[i];
            }

            if (x[i] > xMax) {
                xMax = x[i];
            }
        } // for (i = 1; i < N; i++)

        if (xMax == xMin) {
            MipavUtil.displayError("setupTPSpline3D: x array cannot be single valued");

            return;
        }

        xInvRange = 1.0f / (xMax - xMin);

        for (i = 0; i < N; i++) {
            mX[i] = (x[i] - xMin) * xInvRange;
        }

        yMin = y[0];
        yMax = yMin;

        for (i = 1; i < N; i++) {

            if (y[i] < yMin) {
                yMin = y[i];
            }

            if (y[i] > yMax) {
                yMax = y[i];
            }
        } // for (i = 1; i < N; i++)

        if (yMax == yMin) {
            MipavUtil.displayError("setupTPSpline3D: y array cannot be single valued");

            return;
        }

        yInvRange = 1.0f / (yMax - yMin);

        for (i = 0; i < N; i++) {
            mY[i] = (y[i] - yMin) * yInvRange;
        }

        zMin = z[0];
        zMax = zMin;

        for (i = 1; i < N; i++) {

            if (z[i] < zMin) {
                zMin = z[i];
            }

            if (z[i] > zMax) {
                zMax = z[i];
            }
        } // for (i = 1; i < N; i++)

        if (zMax == zMin) {
            MipavUtil.displayError("setupTPSpline3D: z array cannot be single valued");

            return;
        }

        zInvRange = 1.0f / (zMax - zMin);

        for (i = 0; i < N; i++) {
            mZ[i] = (z[i] - zMin) * zInvRange;
        }

        // compute matrix A = E + smooth * I[N X N matrix]
        A = new double[N][N];

        for (iRow = 0; iRow < N; iRow++) {

            for (iCol = 0; iCol < N; iCol++) {

                if (iRow == iCol) {
                    A[iRow][iCol] = smooth;
                } else {
                    dx = mX[iRow] - mX[iCol];
                    dy = mY[iRow] - mY[iCol];
                    dz = mZ[iRow] - mZ[iCol];
                    fT = Math.sqrt((dx * dx) + (dy * dy) + (dz * dz));
                    A[iRow][iCol] = fT;
                }
            } // for (iCol = 0; iCol < N; iCol++)
        } // for (iRow = 0; iRow < N; iRow++)

        // compute matrix B[N X 4 matrix]
        B = new float[N][4];

        for (iRow = 0; iRow < N; iRow++) {
            B[iRow][0] = 1.0f;
            B[iRow][1] = mX[iRow];
            B[iRow][2] = mY[iRow];
            B[iRow][3] = mZ[iRow];
        } // for (iRow = 0; iRow < N; iRow++)

        // Take the inverse of A, store result in A
        A = ((new Matrix(A)).inverse()).getArray();

        // compute P = B^tA^{-1} [4 X N matrix]
        P = new float[4][N];

        for (iRow = 0; iRow < 4; iRow++) {

            for (iCol = 0; iCol < N; iCol++) {
                P[iRow][iCol] = 0.0f;

                for (i = 0; i < N; i++) {
                    P[iRow][iCol] += B[i][iRow] * A[i][iCol];
                }
            }
        } // for (iRow = 0; iRow < 4; iRow++)

        // compute Q = PB = B^t A^{-1} B [4 X 4 matrix]
        Q = new double[4][4];

        for (iRow = 0; iRow < 4; iRow++) {

            for (iCol = 0; iCol < 4; iCol++) {
                Q[iRow][iCol] = 0.0;

                for (i = 0; i < N; i++) {
                    Q[iRow][iCol] += P[iRow][i] * B[i][iCol];
                }
            }
        } // for (iRow = 0; iRow < 4; iRow++)

        // Take the inverse of Q, store result in Q
        Q = ((new Matrix(Q)).inverse()).getArray();

        // compute P*f
        prod = new float[4];

        for (iRow = 0; iRow < 4; iRow++) {
            prod[iRow] = 0.0f;

            for (i = 0; i < N; i++) {
                prod[iRow] += P[iRow][i] * f[i];
            }
        } // for (iRow = 0; iRow < 4; iRow++)

        // compute 'b' vector for smooth thin plate spline
        mB = new float[4];

        for (iRow = 0; iRow < 4; iRow++) {
            mB[iRow] = 0.0f;

            for (i = 0; i < 4; i++) {
                mB[iRow] += Q[iRow][i] * prod[i];
            }
        } // for (iRow = 0; iRow < 4; iRow++)

        // compute f-B*b
        tmp = new float[N];

        for (iRow = 0; iRow < N; iRow++) {
            tmp[iRow] = f[iRow];

            for (i = 0; i < 4; i++) {
                tmp[iRow] -= B[iRow][i] * mB[i];
            }
        } // for (iRow = 0; iRow < N; iRow++)

        // compute 'a' vector for smooth thin plate spline
        for (iRow = 0; iRow < N; iRow++) {
            mA[iRow] = 0.0f;

            for (i = 0; i < N; i++) {
                mA[iRow] += A[iRow][i] * tmp[i];
            }
        } // for (iRow = 0; iRow < N; iRow++)

        // clean up
        tmp = null;
        A = null;
        B = null;
        P = null;
        Q = null;

    }

    /**
     * setupTPSpline3D computes the thin plate spline coefficients.
     *
     * @param  xSource  array with x coordinates of source data
     * @param  ySource  array with y coordinates of source data
     * @param  zSource  array with z coordinates of source data
     * @param  xTar     array with x coordinates of target data
     * @param  yTar     array with y coordinates of target data
     * @param  zTar     array with z coordinates of target data
     * @param  smooth   cannot be negative If the smoothing term is nonzero, the interpolation is not exact
     */
    public void setupTPSpline3D(double[] xSource, double[] ySource, double[] zSource, double[] xTar, double[] yTar,
                                double[] zTar, float smooth) {
        int iRow, iCol;
        double dx, dy, dz;
        double fT;
        double[][] L;
        double[][] V;

        this.x = xSource;
        this.y = ySource;
        this.z = zSource;

        N = x.length;

        if (N < 4) {
            MipavUtil.displayError("setupTPSpline3D: x source array length cannot be less than 4");

            return;
        }

        if (N != y.length) {
            MipavUtil.displayError("setupTPSpline3D: x source array length does not equal y source array length");

            return;
        }

        if (N != z.length) {
            MipavUtil.displayError("setupTPSpline3D: x source array length does not equal z source array length");

            return;
        }

        if (N != xTar.length) {
            MipavUtil.displayError("setupTPSpline3D: x source array length does not equal x target array length");

            return;
        }

        if (N != yTar.length) {
            MipavUtil.displayError("setupTPSpline3D: x source array length does not equal y target array length");

            return;
        }

        if (N != zTar.length) {
            MipavUtil.displayError("setupTPSpline3D: x source array length does not equal z target array length");

            return;
        }

        if (smooth < 0.0f) {
            MipavUtil.displayError("setupTPSpline3D: smooth cannot be negative");

            return;
        }


        // compute matrix L
        L = new double[N + 4][N + 4];

        for (iRow = 0; iRow < N; iRow++) {

            for (iCol = 0; iCol < N; iCol++) {

                if (iRow == iCol) {
                    L[iRow][iCol] = smooth;
                } else {
                    dx = x[iRow] - x[iCol];
                    dy = y[iRow] - y[iCol];
                    dz = z[iRow] - z[iCol];
                    fT = Math.sqrt((dx * dx) + (dy * dy) + (dz * dz));
                    L[iRow][iCol] = fT;
                }
            } // for (iCol = 0; iCol < N; iCol++)
        } // for (iRow = 0; iRow < N; iRow++)

        for (iCol = 0; iCol < N; iCol++) {
            L[N][iCol] = 1.0;
            L[N + 1][iCol] = x[iCol];
            L[N + 2][iCol] = y[iCol];
            L[N + 3][iCol] = z[iCol];
        }

        for (iRow = 0; iRow < N; iRow++) {
            L[iRow][N] = 1.0;
            L[iRow][N + 1] = x[iRow];
            L[iRow][N + 2] = y[iRow];
            L[iRow][N + 3] = z[iRow];
        }

        for (iRow = N; iRow < (N + 4); iRow++) {

            for (iCol = N; iCol < (N + 4); iCol++) {
                L[iRow][iCol] = 0.0;
            }
        }

        // Take the inverse of L, store result in L
        L = ((new Matrix(L)).inverse()).getArray();
        
        V = new double[N + 4][3];

        for (iRow = 0; iRow < N; iRow++) {
            V[iRow][0] = xTar[iRow];
            V[iRow][1] = yTar[iRow];
            V[iRow][2] = zTar[iRow];
        }

        for (iRow = N; iRow < (N + 4); iRow++) {
            V[iRow][0] = 0.0;
            V[iRow][1] = 0.0;
            V[iRow][2] = 0.0;
        }

        // C contains the coefficents of W, a1, ax, ay, and az
        C = new float[N + 4][3];

        for (iRow = 0; iRow < (N + 4); iRow++) {

            for (iCol = 0; iCol < 3; iCol++) {
                C[iRow][iCol] = 0.0f;
            }
        }

        for (iRow = 0; iRow < (N + 4); iRow++) {

            for (iCol = 0; iCol < (N + 4); iCol++) {
                C[iRow][0] += (float) (L[iRow][iCol] * V[iCol][0]);
                C[iRow][1] += (float) (L[iRow][iCol] * V[iCol][1]);
                C[iRow][2] += (float) (L[iRow][iCol] * V[iCol][2]);
            }
        }

        // clean up
        L = null;
        V = null;

    }

    /**
     * DOCUMENT ME!
     *
     * @param   xWarp  DOCUMENT ME!
     * @param   yWarp  DOCUMENT ME!
     *
     * @return  DOCUMENT ME!
     */
    public float[][] tpSpline2D(float[] xWarp, float[] yWarp) {
        float[][] result;
        int i, j;
        double dx, dy;
        double fT;
        double U;
        int mod;

        fireProgressStateChanged("Thin plate spline", "Performing base to match grid transformation...");


        if (xWarp.length != yWarp.length) {
            MipavUtil.displayError("tpSpline2D: xWarp array length does not equal yWarp array length");

            return null;
        }

        result = new float[2][xWarp.length];
        mod = Math.max(xWarp.length / 100, 1);

        for (i = 0; i < xWarp.length; i++) {

            if ((i % mod) == 0) {
                fireProgressStateChanged((i + 1) * 100 / xWarp.length);
            }

            result[0][i] = C[N][0] + (C[N + 1][0] * xWarp[i]) + (C[N + 2][0] * yWarp[i]);
            result[1][i] = C[N][1] + (C[N + 1][1] * xWarp[i]) + (C[N + 2][1] * yWarp[i]);

            for (j = 0; j < N; j++) {
                dx = x[j] - xWarp[i];
                dy = y[j] - yWarp[i];
                fT = Math.sqrt((dx * dx) + (dy * dy));
                U = kernel(fT);
                result[0][i] += C[j][0] * U;
                result[1][i] += C[j][1] * U;
            }
        }


        return result;
    }

    /**
     * Copy important file information to resultant image structure.
     */
    public void updateFileInfo() {
        FileInfoBase[] fileInfo;

        if (resultImage.getNDims() == 2) {
            fileInfo = resultImage.getFileInfo();

            // fileInfo[0].setModality(baseImage.getFileInfo()[0].getModality());
            // fileInfo[0].setFileDirectory(baseImage.getFileInfo()[0].getFileDirectory());
            // fileInfo[0].setDataType(matchImage.getFileInfo()[0].getDataType());
            fileInfo[0].setEndianess(baseImage.getFileInfo()[0].getEndianess());
            fileInfo[0].setUnitsOfMeasure(baseImage.getFileInfo()[0].getUnitsOfMeasure());
            fileInfo[0].setResolutions(baseImage.getFileInfo()[0].getResolutions());

            // fileInfo[0].setExtents(resultImage.getExtents());
            // fileInfo[0].setMax(resultImage.getMax());
            // fileInfo[0].setMin(resultImage.getMin());
            fileInfo[0].setImageOrientation(baseImage.getImageOrientation());
            fileInfo[0].setAxisOrientation(baseImage.getFileInfo()[0].getAxisOrientation());
            fileInfo[0].setOrigin(baseImage.getFileInfo()[0].getOrigin());
            // fileInfo[0].setPixelPadValue(baseImage.getFileInfo()[0].getPixelPadValue());
            // fileInfo[0].setPhotometric(baseImage.getFileInfo()[0].getPhotometric());
        } else if (resultImage.getNDims() == 3) {
            fileInfo = resultImage.getFileInfo();

            for (int i = 0; i < resultImage.getExtents()[2]; i++) {
                int j = Math.min(i, baseImage.getExtents()[2] - 1);

                // fileInfo[i].setModality(baseImage.getFileInfo()[j].getModality());
                // fileInfo[i].setFileDirectory(baseImage.getFileInfo()[j].getFileDirectory());
                // fileInfo[i].setDataType(matchImage.getFileInfo()[j].getDataType());
                fileInfo[i].setEndianess(baseImage.getFileInfo()[j].getEndianess());
                fileInfo[i].setUnitsOfMeasure(baseImage.getFileInfo()[j].getUnitsOfMeasure());
                fileInfo[i].setResolutions(baseImage.getFileInfo()[j].getResolutions());

                // fileInfo[i].setExtents(resultImage.getExtents());
                // fileInfo[i].setMax(resultImage.getMax());
                // fileInfo[i].setMin(resultImage.getMin());
                fileInfo[i].setImageOrientation(baseImage.getImageOrientation());
                fileInfo[i].setAxisOrientation(baseImage.getFileInfo()[j].getAxisOrientation());
                fileInfo[i].setOrigin(baseImage.getFileInfo()[j].getOrigin());
                // fileInfo[i].setPixelPadValue(baseImage.getFileInfo()[j].getPixelPadValue());
                // fileInfo[i].setPhotometric(baseImage.getFileInfo()[j].getPhotometric());

            }
        }

    }

    /**
     * DOCUMENT ME!
     */
    private void tpSpline2D() {

        // Now that this handles both transformations for 2D images
        // and transformations for 3D images where the corresponding
        // landmarks have equal z values.
        float[] result = new float[2];
        int i, j;
        double dx, dy;
        double fT;
        double U;
        int xDimB, yDimB, zDimB;
        int[] extents;
        int lengthA, lengthB;
        int zNum;
        int m;
        int mod;
        float[] imgBuf;
        float[] resultBuf;
        int yPos;
        int pos;
        float X;
        float Y;
        int X0pos, X1pos;
        int Y0pos, Y1pos;
        float x0, x1, y0, y1;
        int resultIndex;
        int tmpb1, tmpb2, tmpb3, tmpb4;
        float tmpa1, tmpa2, tmpa3, tmpa4;
        VOIVector voiVector;
        VOI presentVOI;
        boolean nonPointVOIPresent = false;

        fireProgressStateChanged(0);
        fireProgressStateChanged("Performing base to match grid transformation...");

        if (setupRequired) {
            xDimA = baseImage.getExtents()[0];
            yDimA = baseImage.getExtents()[1];
        }

        xDimB = matchImage.getExtents()[0];
        yDimB = matchImage.getExtents()[1];

        if (matchImage.getNDims() == 3) {
            zDimB = matchImage.getExtents()[2];
        } else {
            zDimB = 1;
        }

        String name = matchImage.getImageName() + "_regTPSpline";

        try {

            if (matchImage.getNDims() == 3) {
                extents = new int[] { xDimA, yDimA, zDimB };
            } else {
                extents = new int[] { xDimA, yDimA };
            }

            resultImage = new ModelImage(matchImage.getType(), extents, name);
            if (baseImage != null) {
                updateFileInfo();
            }

        } catch (OutOfMemoryError error) {
            extents = null;

            if (resultImage != null) {
                resultImage.disposeLocal();
            }

            resultImage = null;
            System.gc();
            MipavUtil.displayError("AlgorithmTPSpline: Out of memory on extents");
            finalize();
            setCompleted(false);

            return;
        }

        lengthA = xDimA * yDimA;

        fireProgressStateChanged(0);
        fireProgressStateChanged("Performing interpolation into result buffer...");

        if (matchImage.isColorImage() == false) {
            lengthB = xDimB * yDimB;

            try {
                imgBuf = new float[lengthB];
                resultBuf = new float[lengthA * zDimB];
            } catch (OutOfMemoryError error) {
                imgBuf = null;
                resultBuf = null;
                System.gc();
                MipavUtil.displayError("AlgorithmTPSpline: Out of memory on imgBuf" + error);

                finalize();
                setCompleted(false);

                return;
            }

            for (zNum = 0; zNum < zDimB; zNum++) {

                try {
                    matchImage.exportData(zNum * lengthB, lengthB, imgBuf);
                } catch (IOException error) {

                    MipavUtil.displayError("AlgorithmTPSpline: matchImage locked" + error);
                    finalize();
                    setCompleted(false);

                    return;
                }

                float imgMin = (float) matchImage.getMin();

                mod = Math.max(yDimA / 100, 1);

                for (i = 0; i < yDimA; i++) {

                    if ((i % mod) == 0) {
                        fireProgressStateChanged(((i * 100) + (zNum * yDimA * 100)) / (zDimB * yDimA));
                    }

                    yPos = i * xDimA;

                    for (j = 0; j < xDimA; j++) {
                        pos = yPos + j;
                        result[0] = C[N][0] + (C[N + 1][0] * j) + (C[N + 2][0] * i);
                        result[1] = C[N][1] + (C[N + 1][1] * j) + (C[N + 2][1] * i);

                        for (m = 0; m < N; m++) {
                            dx = x[m] - j;
                            dy = y[m] - i;
                            fT = Math.sqrt((dx * dx) + (dy * dy));
                            U = kernel(fT);
                            result[0] += C[m][0] * U;
                            result[1] += C[m][1] * U;
                        }

                        resultBuf[(zNum * lengthA) + pos] = imgMin; // remains zero if transformed out of bounds
                        X = result[0];

                        if ((X >= 0) && (X <= (xDimB - 1))) {
                            Y = result[1];

                            if ((Y >= 0) && (Y <= (yDimB - 1))) {
                                // bi-linear interp. set intensity of i,j to new transformed coordinates if x,y is
                                // within dimensions of image

                                X0pos = (int) (X);
                                Y0pos = (int) (Y) * xDimB;
                                x0 = X - X0pos;
                                y0 = Y - (int) (Y);
                                x1 = 1 - x0;
                                y1 = 1 - y0;

                                if ((X >= 0) && (X < (xDimB - 1))) {
                                    X1pos = X0pos + 1;
                                } else {
                                    X1pos = X0pos;
                                }

                                if ((Y >= 0) && (Y < (yDimB - 1))) {
                                    Y1pos = Y0pos + xDimB;
                                } else {
                                    Y1pos = Y0pos;
                                }

                                resultBuf[(zNum * lengthA) + pos] = (x1 * y1 * imgBuf[Y0pos + X0pos]) +
                                                                    (x0 * y1 * imgBuf[Y0pos + X1pos]) +
                                                                    (x1 * y0 * imgBuf[Y1pos + X0pos]) +
                                                                    (x0 * y0 * imgBuf[Y1pos + X1pos]);

                            } // if ((Y >= 0) && (Y < (yDimB-1)))
                        } // if ((X >= 0) && (X < (xDimB-1)))
                    } // for (j = 0; j < xDimA; j++)
                } // for (i = 0; i < yDimA; i++)
            } // for (zNum = 0; z < zDimB; z++)
        } // matchImage.isColorImage() == false
        else { // color
            lengthB = 4 * xDimB * yDimB;

            try {
                imgBuf = new float[lengthB];
                resultBuf = new float[4 * lengthA * zDimB];
            } catch (OutOfMemoryError error) {
                imgBuf = null;
                resultBuf = null;
                System.gc();
                MipavUtil.displayError("AlgorithmTPSpline: Out of memory error on imgBuf" + error);

                finalize();
                setCompleted(false);

                return;
            }

            for (zNum = 0; zNum < zDimB; zNum++) {

                try {
                    matchImage.exportData(zNum * lengthB, lengthB, imgBuf);
                } catch (IOException error) {

                    MipavUtil.displayError("AlgorithmTPSpline: matchImage locked" + error);
                    finalize();
                    setCompleted(false);

                    return;
                }

                mod = Math.max(yDimA / 100, 1);

                for (i = 0; i < yDimA; i++) {

                    if ((i % mod) == 0) {
                        fireProgressStateChanged(((i * 100) + (zNum * yDimA * 100)) / (zDimB * yDimA));
                    }

                    yPos = i * xDimA;

                    for (j = 0; j < xDimA; j++) {
                        pos = yPos + j;
                        result[0] = C[N][0] + (C[N + 1][0] * j) + (C[N + 2][0] * i);
                        result[1] = C[N][1] + (C[N + 1][1] * j) + (C[N + 2][1] * i);

                        for (m = 0; m < N; m++) {
                            dx = x[m] - j;
                            dy = y[m] - i;
                            fT = Math.sqrt((dx * dx) + (dy * dy));
                            U = kernel(fT);
                            result[0] += C[m][0] * U;
                            result[1] += C[m][1] * U;
                        }

                        resultIndex = 4 * pos;
                        resultBuf[(4 * zNum * lengthA) + resultIndex] = 255; // remains 255 always
                        resultBuf[(4 * zNum * lengthA) + resultIndex + 1] = 0; // R, G, and B remain zero if
                                                                               // transformed out of bounds
                        resultBuf[(4 * zNum * lengthA) + resultIndex + 2] = 0;
                        resultBuf[(4 * zNum * lengthA) + resultIndex + 3] = 0;
                        X = result[0];

                        if ((X >= 0) && (X <= (xDimB - 1))) {
                            Y = result[1];

                            if ((Y >= 0) && (Y <= (yDimB - 1))) {

                                // bi-linear interp.
                                // set intensity of i,j to new transformed coordinates if
                                // x,y is within dimensions of image
                                X0pos = (int) (X);
                                Y0pos = (int) (Y) * xDimB;
                                x0 = X - X0pos;
                                y0 = Y - (int) (Y);
                                x1 = 1 - x0;
                                y1 = 1 - y0;

                                if ((X >= 0) && (X < (xDimB - 1))) {
                                    X1pos = X0pos + 1;
                                } else {
                                    X1pos = X0pos;
                                }

                                if ((Y >= 0) && (Y < (yDimB - 1))) {
                                    Y1pos = Y0pos + xDimB;
                                } else {
                                    Y1pos = Y0pos;
                                }

                                tmpb1 = 4 * (Y0pos + X0pos);
                                tmpb2 = 4 * (Y0pos + X1pos);
                                tmpb3 = 4 * (Y1pos + X0pos);
                                tmpb4 = 4 * (Y1pos + X1pos);

                                tmpa1 = x1 * y1;
                                tmpa2 = x0 * y1;
                                tmpa3 = x1 * y0;
                                tmpa4 = x0 * y0;

                                resultBuf[(4 * zNum * lengthA) + resultIndex + 1] = (tmpa1 * imgBuf[tmpb1 + 1]) +
                                                                                    (tmpa2 * imgBuf[tmpb2 + 1]) +
                                                                                    (tmpa3 * imgBuf[tmpb3 + 1]) +
                                                                                    (tmpa4 * imgBuf[tmpb4 + 1]);
                                resultBuf[(4 * zNum * lengthA) + resultIndex + 2] = (tmpa1 * imgBuf[tmpb1 + 2]) +
                                                                                    (tmpa2 * imgBuf[tmpb2 + 2]) +
                                                                                    (tmpa3 * imgBuf[tmpb3 + 2]) +
                                                                                    (tmpa4 * imgBuf[tmpb4 + 2]);
                                resultBuf[(4 * zNum * lengthA) + resultIndex + 3] = (tmpa1 * imgBuf[tmpb1 + 3]) +
                                                                                    (tmpa2 * imgBuf[tmpb2 + 3]) +
                                                                                    (tmpa3 * imgBuf[tmpb3 + 3]) +
                                                                                    (tmpa4 * imgBuf[tmpb4 + 3]);
                            } // if ((Y >= 0) && (Y < (yDimB-1)))
                        } // if ((X >= 0) && (X < (xDimB-1)))
                    } // for (j = 0; j < xDimA; j++)
                } // for (i = 0; i < yDimA; i++)
            } // for (zNum = 0; zNum < zDimB; zNum++)
        } // else color
        
        voiVector = matchImage.getVOIs();
        for (i = 0; i < voiVector.size(); i++) {
            presentVOI = matchImage.getVOIs().VOIAt(i);
            if (presentVOI.getCurveType() != VOI.POINT) {
                nonPointVOIPresent = true;	
            }
        }
        
        if (nonPointVOIPresent) {
            transform25DVOI(matchImage, resultImage, imgBuf);	
        }

        try {
            resultImage.importData(0, resultBuf, true);
        } catch (IOException error) {

            MipavUtil.displayError("AlgorithmTPSPline: IOException Error on importData into resultImage" + error);
            finalize();
            setCompleted(false);

            return;
        }

        setCompleted(true);

        return;
    }
    
    private void transform25DVOI(final ModelImage image, ModelImage destImage, final float[] imgBuffer) {
    	int i, j, k;
        int X0pos, Y0pos;
        float X, Y;
        float value;
        int index;
        int index2;
        int indexC;
        int iXdim = image.getExtents()[0];
        int iYdim = image.getExtents()[1];
        int sliceSize = iXdim * iYdim;
        int iZdim = 1;
        if (image.getNDims() > 2) {
        	iZdim = image.getExtents()[2];
        }
        ModelImage maskImage;
        float fillValue = 0.0f;
        int index2Size;
        VOIBaseVector curves = null;
        int xBounds[] = new int[2];
        int yBounds[] = new int[2];
        int zBounds[] = new int[2];
        int zFound[] = new int[iZdim];
        boolean duplicateZ = false;
        int oXdim = destImage.getExtents()[0];
        int oYdim = destImage.getExtents()[1];
        int oZdim = 1;
        if (destImage.getNDims() > 2) {
        	oZdim = destImage.getExtents()[2];
        }
        float[] result = new float[2];
        int m;
        double dx, dy;
        double fT;
        double U;


        ModelImage tmpMask = null;
        VOIVector voiVector;
        
        voiVector = image.getVOIs();
        
        if (voiVector.size() == 0) {
            return;
        }

        indexC = 0;
        try {
            maskImage = new ModelImage(ModelStorageBase.SHORT, image.getExtents(), "Short Image");
            tmpMask = new ModelImage(ModelStorageBase.SHORT, destImage.getExtents(), null);
        } catch (final OutOfMemoryError error) {
            throw error;
        }
        for (index = 0; index < voiVector.size(); index++) {
        	VOI presentVOI = voiVector.elementAt(index);
        	if (presentVOI.getCurveType() == VOI.CONTOUR) {
        		curves = presentVOI.getCurves();	
        		index2Size = curves.size();
        	}
        	else if (presentVOI.getCurveType() == VOI.POINT) {
        		continue;
        	}
        	else {
        		index2Size = 1;
        	}
            for (i = 0; i < iZdim; i++) {
            	zFound[i] = 0;
            }
        	for (index2 = 0; index2 < index2Size; index2++) {
        		if (presentVOI.getCurveType() == VOI.CONTOUR) {
        		    curves.get(index2).getBounds(xBounds, yBounds, zBounds);	
        		}
        		else {
        			presentVOI.getBounds(xBounds, yBounds, zBounds);
        		}
        		duplicateZ = false;
        		for (i = zBounds[0]; i <= zBounds[1]; i++) {
        			zFound[i]++;
        			if (zFound[i] >= 2) {
        				duplicateZ = true;
        			}
        		}
        		if (duplicateZ) {
        			indexC++;
		        	duplicateZ = false;
		        	for (i = 0; i < iZdim; i++) {
		        		zFound[i] = 0;
		        	}
			        tmpMask.calcMinMax();
			
			        AlgorithmVOIExtraction VOIExtAlgo = new AlgorithmVOIExtraction(tmpMask);
			
			        VOIExtAlgo.setRunningInSeparateThread(runningInSeparateThread);
			        VOIExtAlgo.run();
			        VOIExtAlgo.finalize();
			        VOIExtAlgo = null;
			        destImage.addVOIs(tmpMask.getVOIs());
			        tmpMask.resetVOIs();
			        for (k = 0; k < oZdim; k++) {
			        	for (j = 0; j < oYdim; j++) {
			        		for (i = 0; i < oXdim; i++) {
			        			tmpMask.set(i, j, k, fillValue);
			        		}
			        	}
			        }
			        index2--;
			        continue;
		        }
		        
		        maskImage.clearMask();
		        
		        (voiVector.elementAt(index)).createOneElementBinaryMask3D(maskImage.getMask(), iXdim, iYdim, false, false, index2);

				BitSet mask = maskImage.getMask();

				
				for (i = zBounds[0]*sliceSize; i < (zBounds[1]+1)*sliceSize; i++) {

					if (mask.get(i)) {
						maskImage.set(i, indexC + 1);
					}
					else {
						maskImage.set(i, 0);
					}
				}
		  
		
		        for (k = zBounds[0]; k <= zBounds[1]; k++) {
		            
		            try {
		                maskImage.exportData(k * sliceSize, sliceSize, imgBuffer); // locks and releases lock
		            } catch (final IOException error) {
		                displayError("AlgorithmTPSpline: Image(s) locked");
		                setCompleted(false);

		                return;
		            }
		            
		            for (i = 0; i < oYdim; i++) {

	                    for (j = 0; j < oXdim; j++) {
	                        result[0] = C[N][0] + (C[N + 1][0] * j) + (C[N + 2][0] * i);
	                        result[1] = C[N][1] + (C[N + 1][1] * j) + (C[N + 2][1] * i);

	                        for (m = 0; m < N; m++) {
	                            dx = x[m] - j;
	                            dy = y[m] - i;
	                            fT = Math.sqrt((dx * dx) + (dy * dy));
	                            U = kernel(fT);
	                            result[0] += C[m][0] * U;
	                            result[1] += C[m][1] * U;
	                        }

	                        value = fillValue; // remains zero if transformed out of bounds
	                        X = result[0];

	                        if ((X >= 0) && (X <= (iXdim - 1))) {
	                            Y = result[1];

	                            if ((Y >= 0) && (Y <= (iYdim - 1))) {
	                                // bi-linear interp. set intensity of i,j to new transformed coordinates if x,y is
	                                // within dimensions of image

	                                X0pos = (int) (X + 0.5f);
	                                X0pos = Math.min(X0pos, iXdim-1);
	                                Y0pos = ((int) (Y + 0.5f)) * iXdim;
	                                Y0pos = Math.min(Y0pos, (iYdim-1)*iXdim);
	                                value = imgBuffer[Y0pos + X0pos];
	                                

	                            } // if ((Y >= 0) && (Y < (iYdim-1)))
	                        } // if ((X >= 0) && (X < (iXdim-1)))
	                        tmpMask.set(j, i, k, value);
	                    } // for (j = 0; j < oXdim; j++)
	                } // for (i = 0; i < oYdim; i++)
		        
		
			       
			
			        if (threadStopped) {
			            return;
			        }
		        } // for (k = zBounds[0]; k <= zBounds[1]; k++)
		
		        // ******* Make algorithm for VOI extraction.
		        if (index2 == curves.size()-1) {
		        	indexC++;
		        	duplicateZ = false;
		        	for (i = 0; i < iZdim; i++) {
		        		zFound[i] = 0;
		        	}
			        tmpMask.calcMinMax();
			
			        AlgorithmVOIExtraction VOIExtAlgo = new AlgorithmVOIExtraction(tmpMask);
			
			        VOIExtAlgo.setRunningInSeparateThread(runningInSeparateThread);
			        VOIExtAlgo.run();
			        VOIExtAlgo.finalize();
			        VOIExtAlgo = null;
			        destImage.addVOIs(tmpMask.getVOIs());
			        tmpMask.resetVOIs();
			        for (k = 0; k < oZdim; k++) {
			        	for (j = 0; j < oYdim; j++) {
			        		for (i = 0; i < oXdim; i++) {
			        			tmpMask.set(i, j, k, fillValue);
			        		}
			        	}
			        }
		        }
        	} // for (index2 = 0; index2 < curves.size(); index2++)
        } // for (index = 0; index < voiVector.size(); index++)
        maskImage.disposeLocal();
        maskImage = null;
        tmpMask.disposeLocal();
        tmpMask = null;
       
    }
    
    
    
    public float[] getCorrespondingPoint(int xTar, int yTar, int zTar) {
    	float[] srcPoints = new float[3];
    	double dx, dy, dz;
        double fT;
        double U;
    	
    	
    	srcPoints[0] = C[N][0] + (C[N + 1][0] * xTar) + (C[N + 2][0] * yTar) + (C[N + 3][0] * zTar);
    	srcPoints[1] = C[N][1] + (C[N + 1][1] * xTar) + (C[N + 2][1] * yTar) + (C[N + 3][1] * zTar);
    	srcPoints[2] = C[N][2] + (C[N + 1][2] * xTar) + (C[N + 2][2] * yTar) + (C[N + 3][2] * zTar);
    	
    	
    	for (int m = 0; m < N; m++) {
            dx = x[m] - xTar;
            dy = y[m] - yTar;
            dz = z[m] - zTar;
            fT = Math.sqrt((dx * dx) + (dy * dy) + (dz * dz));
            U = fT;
            srcPoints[0] += C[m][0] * U;
            srcPoints[1] += C[m][1] * U;
            srcPoints[2] += C[m][2] * U;
        }
    	
    	
    	return srcPoints;
    }
    
    
    public final float[] getCorrespondingPoint(float xTar, float yTar, float zTar) {
    	float[] srcPoints = new float[3];
    	double dx, dy, dz;
        double fT;
        double U;
    	
    	
    	srcPoints[0] = C[N][0] + (C[N + 1][0] * xTar) + (C[N + 2][0] * yTar) + (C[N + 3][0] * zTar);
    	srcPoints[1] = C[N][1] + (C[N + 1][1] * xTar) + (C[N + 2][1] * yTar) + (C[N + 3][1] * zTar);
    	srcPoints[2] = C[N][2] + (C[N + 1][2] * xTar) + (C[N + 2][2] * yTar) + (C[N + 3][2] * zTar);
    	
    	
    	for (int m = 0; m < N; m++) {
            dx = x[m] - xTar;
            dy = y[m] - yTar;
            dz = z[m] - zTar;
            fT = Math.sqrt((dx * dx) + (dy * dy) + (dz * dz));
            U = fT;
            srcPoints[0] += C[m][0] * U;
            srcPoints[1] += C[m][1] * U;
            srcPoints[2] += C[m][2] * U;
        }
    	
    	//System.out.println("getCorrespondingPt : " + xTar + "," + yTar + "," + zTar + " ---> " + srcPoints[0] + "," + srcPoints[1] + "," + srcPoints[2]);
    	return srcPoints;
    	
    }

    /**
     * DOCUMENT ME!
     */
    private void tpSpline3D() {
        float[] result = new float[3];
        int i, j;
        double dx, dy, dz;
        double fT;
        double U;
        int xDimB, yDimB, zDimB;
        int[] extents;
        int sliceSizeA;
        int sliceSizeB;
        int lengthB;
        int k;
        int m;
        int mod;
        float[] resultBuf;
        float[] imgBuf;
        short[]imgBuf_short;
        int pos, yPos, zPos;
        float X, Y, Z;
        float x0, y0, z0;
        float x1, y1, z1;
        int X0pos, Y0pos, Z0pos;
        int X1pos, Y1pos, Z1pos;
        float temp4, temp5, temp6, temp7;
        int resultIndex;
        float tmpa1, tmpa2, tmpa3, tmpa4, tmpa5, tmpa6, tmpa7, tmpa8;
        int index000, index001, index010, index011, index100, index101, index110, index111;
        VOIVector voiVector;
        VOI presentVOI;
        boolean nonPointVOIPresent = false;

        fireProgressStateChanged(0);
        fireProgressStateChanged("Performing base to match grid transformation...");

        if (setupRequired) {
            xDimA = baseImage.getExtents()[0];
            yDimA = baseImage.getExtents()[1];
            zDimA = baseImage.getExtents()[2];
        }

        xDimB = matchImage.getExtents()[0];
        yDimB = matchImage.getExtents()[1];
        zDimB = matchImage.getExtents()[2];

        String name = matchImage.getImageName() + "_regTPSpline";

        try {
            extents = new int[] { xDimA, yDimA, zDimA };
            resultImage = new ModelImage(matchImage.getType(), extents, name);
            float[] resols;
            if(baseImage == null) {
            	resols = matchImage.getResolutions(0);
            }else {
            	resols = baseImage.getResolutions(0);
            }
            for(int w=0;w<resultImage.getExtents()[2];w++) {
            	resultImage.setResolutions(w, resols);
   		 	}
        } catch (OutOfMemoryError error) {
            extents = null;

            if (resultImage != null) {
                resultImage.disposeLocal();
            }

            resultImage = null;
            System.gc();
            MipavUtil.displayError("AlgorithmTPSpline: Out of memory on extents" + error);
            finalize();
            setCompleted(false);

            return;
        }

        if (baseImage != null) {
            updateFileInfo();
        }

        sliceSizeA = xDimA * yDimA;

        fireProgressStateChanged(0);
        fireProgressStateChanged("Performing interpolation into result buffer...");

        if (matchImage.isColorImage() == false) {
            sliceSizeB = xDimB * yDimB;
            lengthB = sliceSizeB * zDimB;

            try {
                imgBuf = new float[lengthB];
                resultBuf = new float[sliceSizeA];
            } catch (OutOfMemoryError error) {
                imgBuf = null;
                resultBuf = null;
                System.gc();
                MipavUtil.displayError("AlgorithmTPSpline: Out of memory on image buffers." + error);

                finalize();
                setCompleted(false);

                return;
            }

            try {
                matchImage.exportData(0, lengthB, imgBuf);
            } catch (IOException error) {

                MipavUtil.displayError("AlgorithmTPSpline: matchImage locked" + error);
                finalize();
                setCompleted(false);

                return;
            }

            float imgMin = (float) matchImage.getMin();

            mod = Math.max(zDimA / 100, 1);

            for (i = 0; i < zDimA; i++) {

                if ((i % mod) == 0) {
                    fireProgressStateChanged((i + 1) * 100 / zDimA);
                }

                zPos = i * sliceSizeA;

                for (j = 0; j < yDimA; j++) {
                    yPos = (j * xDimA);

                    for (k = 0; k < xDimA; k++) {
                        pos = yPos + k;
                        result[0] = C[N][0] + (C[N + 1][0] * k) + (C[N + 2][0] * j) + (C[N + 3][0] * i);
                        result[1] = C[N][1] + (C[N + 1][1] * k) + (C[N + 2][1] * j) + (C[N + 3][1] * i);
                        result[2] = C[N][2] + (C[N + 1][2] * k) + (C[N + 2][2] * j) + (C[N + 3][2] * i);

                        for (m = 0; m < N; m++) {
                            dx = x[m] - k;
                            dy = y[m] - j;
                            dz = z[m] - i;
                            fT = Math.sqrt((dx * dx) + (dy * dy) + (dz * dz));
                            U = fT;
                            result[0] += C[m][0] * U;
                            result[1] += C[m][1] * U;
                            result[2] += C[m][2] * U;
                        }

                        resultBuf[pos] = imgMin; // remains zero if transformed out of bounds
                        X = result[0];

                        if ((X >= 0) && (X <= (xDimB - 1))) {
                            Y = result[1];

                            if ((Y >= 0) && (Y <= (yDimB - 1))) {
                                Z = result[2];

                                if ((Z >= 0) && (Z <= (zDimB - 1))) {

                                    // Preferences.debug(X+", "+Y+", "+Z, Preferences.DEBUG_ALGORITHM);
                                    // set intensity of i,j,k to new transformed coordinate if
                                    // x,y,z is w/in dimensions of image
                                    x0 = X - (int) (X);
                                    y0 = Y - (int) (Y);
                                    z0 = Z - (int) (Z);
                                    x1 = 1 - x0;
                                    y1 = 1 - y0;
                                    z1 = 1 - z0;
                                    X0pos = (int) (X);
                                    Y0pos = (int) (Y) * xDimB;
                                    Z0pos = (int) (Z) * sliceSizeB;

                                    if ((X >= 0) && (X < (xDimB - 1))) {
                                        X1pos = X0pos + 1;
                                    } else {
                                        X1pos = X0pos;
                                    }

                                    if ((Y >= 0) && (Y < (yDimB - 1))) {
                                        Y1pos = Y0pos + xDimB;
                                    } else {
                                        Y1pos = Y0pos;
                                    }

                                    if ((Z >= 0) && (Z < (zDimB - 1))) {
                                        Z1pos = Z0pos + sliceSizeB;
                                    } else {
                                        Z1pos = Z0pos;
                                    }

                                    temp4 = y1 * z1;
                                    temp5 = y0 * z1;
                                    temp6 = y1 * z0;
                                    temp7 = y0 * z0;

                                    resultBuf[pos] = (x1 * temp4 * imgBuf[Z0pos + Y0pos + X0pos]) +
                                                     (x0 * temp4 * imgBuf[Z0pos + Y0pos + X1pos]) +
                                                     (x1 * temp5 * imgBuf[Z0pos + Y1pos + X0pos]) +
                                                     (x0 * temp5 * imgBuf[Z0pos + Y1pos + X1pos]) +
                                                     (x1 * temp6 * imgBuf[Z1pos + Y0pos + X0pos]) +
                                                     (x0 * temp6 * imgBuf[Z1pos + Y0pos + X1pos]) +
                                                     (x1 * temp7 * imgBuf[Z1pos + Y1pos + X0pos]) +
                                                     (x0 * temp7 * imgBuf[Z1pos + Y1pos + X1pos]);
                                } // if Z in bounds
                            } // if Y in bounds
                        } // if X in bounds
                    } // for k
                } // for j
                try {
                    resultImage.importData(zPos, resultBuf, false);
                } catch (IOException error) {

                    MipavUtil.displayError("AlgorithmTPSPline: IOException Error on importData into resultImage" + error);
                    finalize();
                    setCompleted(false);

                    return;
                }
            } // for i
        } // matchImage.isColorImage() == false
        else { // color
            sliceSizeB = xDimB * yDimB;
            lengthB = 4 * sliceSizeB * zDimB;
            if(matchImage.getType() == ModelStorageBase.ARGB_FLOAT) { //ARGB FLOAT
            	try {
                    imgBuf = new float[lengthB];
                    resultBuf = new float[4 * sliceSizeA];
                } catch (OutOfMemoryError error) {
                    imgBuf = null;
                    resultBuf = null;
                    System.gc();
                    MipavUtil.displayError("AlgorithmTPSpline: Out of memory on buffers" + error);

                    finalize();
                    setCompleted(false);

                    return;
                }

                try {
                    matchImage.exportData(0, lengthB, imgBuf);
                } catch (IOException error) {

                    MipavUtil.displayError("AlgorithmTPSpline: matchImage locked" + error);
                    finalize();
                    setCompleted(false);

                    return;
                }

                mod = Math.max(zDimA / 100, 1);

                for (i = 0; i < zDimA; i++) {

                    if ((i % mod) == 0) {
                        fireProgressStateChanged((i + 1) * 100 / zDimA);
                    }

                    zPos = i * sliceSizeA;

                    for (j = 0; j < yDimA; j++) {
                        yPos = (j * xDimA);

                        for (k = 0; k < xDimA; k++) {
                            pos = yPos + k;
                            result[0] = C[N][0] + (C[N + 1][0] * k) + (C[N + 2][0] * j) + (C[N + 3][0] * i);
                            result[1] = C[N][1] + (C[N + 1][1] * k) + (C[N + 2][1] * j) + (C[N + 3][1] * i);
                            result[2] = C[N][2] + (C[N + 1][2] * k) + (C[N + 2][2] * j) + (C[N + 3][2] * i);

                            for (m = 0; m < N; m++) {
                                dx = x[m] - k;
                                dy = y[m] - j;
                                dz = z[m] - i;
                                fT = Math.sqrt((dx * dx) + (dy * dy) + (dz * dz));
                                U = fT;
                                result[0] += C[m][0] * U;
                                result[1] += C[m][1] * U;
                                result[2] += C[m][2] * U;
                            }

                            resultIndex = 4 * pos;
                            resultBuf[resultIndex] = 255; // remains 255 always
                            resultBuf[resultIndex + 1] = 0; // R, G, and B remain zero if
                            resultBuf[resultIndex + 2] = 0; // transformed out of bounds
                            resultBuf[resultIndex + 3] = 0;
                            X = result[0];

                            if ((X >= 0) && (X <= (xDimB - 1))) {
                                Y = result[1];

                                if ((Y >= 0) && (Y <= (yDimB - 1))) {
                                    Z = result[2];

                                    if ((Z >= 0) && (Z <= (zDimB - 1))) {

                                        // Preferences.debug(X+", "+Y+", "+Z, Preferences.DEBUG_ALGORITHM);
                                        // set intensity of i,j,k to new transformed coordinate if
                                        // x,y,z is w/in dimensions of image
                                        x0 = X - (int) (X);
                                        y0 = Y - (int) (Y);
                                        z0 = Z - (int) (Z);
                                        x1 = 1 - x0;
                                        y1 = 1 - y0;
                                        z1 = 1 - z0;
                                        X0pos = (int) (X);
                                        Y0pos = (int) (Y) * xDimB;
                                        Z0pos = (int) (Z) * sliceSizeB;

                                        if ((X >= 0) && (X < (xDimB - 1))) {
                                            X1pos = X0pos + 1;
                                        } else {
                                            X1pos = X0pos;
                                        }

                                        if ((Y >= 0) && (Y < (yDimB - 1))) {
                                            Y1pos = Y0pos + xDimB;
                                        } else {
                                            Y1pos = Y0pos;
                                        }

                                        if ((Z >= 0) && (Z < (zDimB - 1))) {
                                            Z1pos = Z0pos + sliceSizeB;
                                        } else {
                                            Z1pos = Z0pos;
                                        }

                                        temp4 = y1 * z1;
                                        temp5 = y0 * z1;
                                        temp6 = y1 * z0;
                                        temp7 = y0 * z0;

                                        tmpa1 = x1 * temp4;
                                        tmpa2 = x0 * temp4;
                                        tmpa3 = x1 * temp5;
                                        tmpa4 = x0 * temp5;
                                        tmpa5 = x1 * temp6;
                                        tmpa6 = x0 * temp6;
                                        tmpa7 = x1 * temp7;
                                        tmpa8 = x0 * temp7;

                                        index000 = 4 * (Z0pos + Y0pos + X0pos);
                                        index001 = 4 * (Z0pos + Y0pos + X1pos);
                                        index010 = 4 * (Z0pos + Y1pos + X0pos);
                                        index011 = 4 * (Z0pos + Y1pos + X1pos);
                                        index100 = 4 * (Z1pos + Y0pos + X0pos);
                                        index101 = 4 * (Z1pos + Y0pos + X1pos);
                                        index110 = 4 * (Z1pos + Y1pos + X0pos);
                                        index111 = 4 * (Z1pos + Y1pos + X1pos);


                                        resultBuf[resultIndex + 1] = (tmpa1 * imgBuf[index000 + 1]) +
                                                                     (tmpa2 * imgBuf[index001 + 1]) +
                                                                     (tmpa3 * imgBuf[index010 + 1]) +
                                                                     (tmpa4 * imgBuf[index011 + 1]) +
                                                                     (tmpa5 * imgBuf[index100 + 1]) +
                                                                     (tmpa6 * imgBuf[index101 + 1]) +
                                                                     (tmpa7 * imgBuf[index110 + 1]) +
                                                                     (tmpa8 * imgBuf[index111 + 1]);

                                        resultBuf[resultIndex + 2] = (tmpa1 * imgBuf[index000 + 2]) +
                                                                     (tmpa2 * imgBuf[index001 + 2]) +
                                                                     (tmpa3 * imgBuf[index010 + 2]) +
                                                                     (tmpa4 * imgBuf[index011 + 2]) +
                                                                     (tmpa5 * imgBuf[index100 + 2]) +
                                                                     (tmpa6 * imgBuf[index101 + 2]) +
                                                                     (tmpa7 * imgBuf[index110 + 2]) +
                                                                     (tmpa8 * imgBuf[index111 + 2]);

                                        resultBuf[resultIndex + 3] = (tmpa1 * imgBuf[index000 + 3]) +
                                                                     (tmpa2 * imgBuf[index001 + 3]) +
                                                                     (tmpa3 * imgBuf[index010 + 3]) +
                                                                     (tmpa4 * imgBuf[index011 + 3]) +
                                                                     (tmpa5 * imgBuf[index100 + 3]) +
                                                                     (tmpa6 * imgBuf[index101 + 3]) +
                                                                     (tmpa7 * imgBuf[index110 + 3]) +
                                                                     (tmpa8 * imgBuf[index111 + 3]);
                                    } // if Z in bounds
                                } // if Y in bounds
                            } // if X in bounds
                        } // for k
                    } // for j
                    try {
                        resultImage.importData(4 * zPos, resultBuf, false);
                    } catch (IOException error) {

                        MipavUtil.displayError("AlgorithmTPSPline: IOException Error on importData into resultImage" + error);
                        finalize();
                        setCompleted(false);

                        return;
                    }
                } // for i
            }else { //ARGB UBYTE OR SHORT
            	System.out.println("argb ubyte/short");
            	try {
                    imgBuf_short = new short[lengthB];
                    resultBuf = new float[4 * sliceSizeA];
                } catch (OutOfMemoryError error) {
                    imgBuf = null;
                    resultBuf = null;
                    System.gc();
                    MipavUtil.displayError("AlgorithmTPSpline: Out of memory on buffers" + error);

                    finalize();
                    setCompleted(false);

                    return;
                }

                try {
                    matchImage.exportData(0, lengthB, imgBuf_short);
                } catch (IOException error) {

                    MipavUtil.displayError("AlgorithmTPSpline: matchImage locked" + error);
                    finalize();
                    setCompleted(false);

                    return;
                }

                mod = Math.max(zDimA / 100, 1);

                for (i = 0; i < zDimA; i++) {

                    if ((i % mod) == 0) {
                        fireProgressStateChanged((i + 1) * 100 / zDimA);
                    }

                    zPos = i * sliceSizeA;

                    for (j = 0; j < yDimA; j++) {
                        yPos = (j * xDimA);

                        for (k = 0; k < xDimA; k++) {
                            pos = yPos + k;
                            result[0] = C[N][0] + (C[N + 1][0] * k) + (C[N + 2][0] * j) + (C[N + 3][0] * i);
                            result[1] = C[N][1] + (C[N + 1][1] * k) + (C[N + 2][1] * j) + (C[N + 3][1] * i);
                            result[2] = C[N][2] + (C[N + 1][2] * k) + (C[N + 2][2] * j) + (C[N + 3][2] * i);

                            for (m = 0; m < N; m++) {
                                dx = x[m] - k;
                                dy = y[m] - j;
                                dz = z[m] - i;
                                fT = Math.sqrt((dx * dx) + (dy * dy) + (dz * dz));
                                U = fT;
                                result[0] += C[m][0] * U;
                                result[1] += C[m][1] * U;
                                result[2] += C[m][2] * U;
                            }

                            resultIndex = 4 * pos;
                            resultBuf[resultIndex] = 255; // remains 255 always
                            resultBuf[resultIndex + 1] = 0; // R, G, and B remain zero if
                            resultBuf[resultIndex + 2] = 0; // transformed out of bounds
                            resultBuf[resultIndex + 3] = 0;
                            X = result[0];

                            if ((X >= 0) && (X <= (xDimB - 1))) {
                                Y = result[1];

                                if ((Y >= 0) && (Y <= (yDimB - 1))) {
                                    Z = result[2];

                                    if ((Z >= 0) && (Z <= (zDimB - 1))) {

                                        // Preferences.debug(X+", "+Y+", "+Z, Preferences.DEBUG_ALGORITHM);
                                        // set intensity of i,j,k to new transformed coordinate if
                                        // x,y,z is w/in dimensions of image
                                        x0 = X - (int) (X);
                                        y0 = Y - (int) (Y);
                                        z0 = Z - (int) (Z);
                                        x1 = 1 - x0;
                                        y1 = 1 - y0;
                                        z1 = 1 - z0;
                                        X0pos = (int) (X);
                                        Y0pos = (int) (Y) * xDimB;
                                        Z0pos = (int) (Z) * sliceSizeB;

                                        if ((X >= 0) && (X < (xDimB - 1))) {
                                            X1pos = X0pos + 1;
                                        } else {
                                            X1pos = X0pos;
                                        }

                                        if ((Y >= 0) && (Y < (yDimB - 1))) {
                                            Y1pos = Y0pos + xDimB;
                                        } else {
                                            Y1pos = Y0pos;
                                        }

                                        if ((Z >= 0) && (Z < (zDimB - 1))) {
                                            Z1pos = Z0pos + sliceSizeB;
                                        } else {
                                            Z1pos = Z0pos;
                                        }

                                        temp4 = y1 * z1;
                                        temp5 = y0 * z1;
                                        temp6 = y1 * z0;
                                        temp7 = y0 * z0;

                                        tmpa1 = x1 * temp4;
                                        tmpa2 = x0 * temp4;
                                        tmpa3 = x1 * temp5;
                                        tmpa4 = x0 * temp5;
                                        tmpa5 = x1 * temp6;
                                        tmpa6 = x0 * temp6;
                                        tmpa7 = x1 * temp7;
                                        tmpa8 = x0 * temp7;

                                        index000 = 4 * (Z0pos + Y0pos + X0pos);
                                        index001 = 4 * (Z0pos + Y0pos + X1pos);
                                        index010 = 4 * (Z0pos + Y1pos + X0pos);
                                        index011 = 4 * (Z0pos + Y1pos + X1pos);
                                        index100 = 4 * (Z1pos + Y0pos + X0pos);
                                        index101 = 4 * (Z1pos + Y0pos + X1pos);
                                        index110 = 4 * (Z1pos + Y1pos + X0pos);
                                        index111 = 4 * (Z1pos + Y1pos + X1pos);


                                        resultBuf[resultIndex + 1] = (tmpa1 * imgBuf_short[index000 + 1]) +
                                                                     (tmpa2 * imgBuf_short[index001 + 1]) +
                                                                     (tmpa3 * imgBuf_short[index010 + 1]) +
                                                                     (tmpa4 * imgBuf_short[index011 + 1]) +
                                                                     (tmpa5 * imgBuf_short[index100 + 1]) +
                                                                     (tmpa6 * imgBuf_short[index101 + 1]) +
                                                                     (tmpa7 * imgBuf_short[index110 + 1]) +
                                                                     (tmpa8 * imgBuf_short[index111 + 1]);

                                        resultBuf[resultIndex + 2] = (tmpa1 * imgBuf_short[index000 + 2]) +
                                                                     (tmpa2 * imgBuf_short[index001 + 2]) +
                                                                     (tmpa3 * imgBuf_short[index010 + 2]) +
                                                                     (tmpa4 * imgBuf_short[index011 + 2]) +
                                                                     (tmpa5 * imgBuf_short[index100 + 2]) +
                                                                     (tmpa6 * imgBuf_short[index101 + 2]) +
                                                                     (tmpa7 * imgBuf_short[index110 + 2]) +
                                                                     (tmpa8 * imgBuf_short[index111 + 2]);

                                        resultBuf[resultIndex + 3] = (tmpa1 * imgBuf_short[index000 + 3]) +
                                                                     (tmpa2 * imgBuf_short[index001 + 3]) +
                                                                     (tmpa3 * imgBuf_short[index010 + 3]) +
                                                                     (tmpa4 * imgBuf_short[index011 + 3]) +
                                                                     (tmpa5 * imgBuf_short[index100 + 3]) +
                                                                     (tmpa6 * imgBuf_short[index101 + 3]) +
                                                                     (tmpa7 * imgBuf_short[index110 + 3]) +
                                                                     (tmpa8 * imgBuf_short[index111 + 3]);
                                    } // if Z in bounds
                                } // if Y in bounds
                            } // if X in bounds
                        } // for k
                    } // for j
                    try {
                        resultImage.importData(4 * zPos, resultBuf, false);
                    } catch (IOException error) {

                        MipavUtil.displayError("AlgorithmTPSPline: IOException Error on importData into resultImage" + error);
                        finalize();
                        setCompleted(false);

                        return;
                    }
                } // for i
            }
            
        } // else color

        resultImage.calcMinMax();
        
        voiVector = matchImage.getVOIs();
        for (i = 0; i < voiVector.size(); i++) {
            presentVOI = matchImage.getVOIs().VOIAt(i);
            if (presentVOI.getCurveType() != VOI.POINT) {
                nonPointVOIPresent = true;	
            }
        }
        
        if (nonPointVOIPresent) {
        	imgBuf = new float[xDimB*yDimB*zDimB];
            transform3DVOI(matchImage, resultImage, imgBuf);	
        }

        setCompleted(true);

        return;
    }
    
    private void transform3DVOI(final ModelImage image, ModelImage destImage, final float[] imgBuffer) {
    	int i, j, k;
        int X0pos, Y0pos, Z0pos;
        float X, Y, Z;
        float value;
        int index;
        int index2;
        int indexC;
        int iXdim = image.getExtents()[0];
        int iYdim = image.getExtents()[1];
        int iZdim = image.getExtents()[2];
        int sliceSize = iXdim * iYdim;
        int length = sliceSize * iZdim;
        ModelImage maskImage;
        float fillValue = 0.0f;
        int index2Size;
        VOIBaseVector curves = null;
        int xBounds[] = new int[2];
        int yBounds[] = new int[2];
        int zBounds[] = new int[2];
        int zFound[] = new int[iZdim];
        boolean duplicateZ = false;
        int oXdim = destImage.getExtents()[0];
        int oYdim = destImage.getExtents()[1];
        int oZdim = destImage.getExtents()[2];
        float[] result = new float[3];
        int m;
        double dx, dy, dz;
        double fT;
        double U;

        ModelImage tmpMask = null;
        VOIVector voiVector;
        
        voiVector = image.getVOIs();
        
        if (voiVector.size() == 0) {
            return;
        }

        indexC = 0;

        try {
            maskImage = new ModelImage(ModelStorageBase.SHORT, image.getExtents(), "Short Image");
            tmpMask = new ModelImage(ModelStorageBase.SHORT, destImage.getExtents(), null);
        } catch (final OutOfMemoryError error) {
            throw error;
        }
        for (k = 0; k < oZdim; k++) {
        	for (j = 0; j < oYdim; j++) {
        		for (i = 0; i < oXdim; i++) {
        			tmpMask.set(i, j, k, fillValue);
        		}
        	}
        }
      
        for (index = 0; index < voiVector.size(); index++) {
        	VOI presentVOI = voiVector.elementAt(index);
        	if (presentVOI.getCurveType() == VOI.CONTOUR) {
        		curves = presentVOI.getCurves();	
        		index2Size = curves.size();
        	}
        	else if (presentVOI.getCurveType() == VOI.POINT) {
        		continue;
        	}
        	else {
        		index2Size = 1;
        	}
            for (i = 0; i < iZdim; i++) {
            	zFound[i] = 0;
            }
        	for (index2 = 0; index2 < index2Size; index2++) {
        		if (presentVOI.getCurveType() == VOI.CONTOUR) {
        		    curves.get(index2).getBounds(xBounds, yBounds, zBounds);	
        		}
        		else {
        			presentVOI.getBounds(xBounds, yBounds, zBounds);
        		}
        		duplicateZ = false;
        		for (i = zBounds[0]; i <= zBounds[1]; i++) {
        			zFound[i]++;
        			if (zFound[i] >= 2) {
        				duplicateZ = true;
        			}
        		}
        		if (duplicateZ) {
        			indexC++;
		        	duplicateZ = false;
		        	for (i = 0; i < iZdim; i++) {
		        		zFound[i] = 0;
		        	}
			        tmpMask.calcMinMax();
			
			        AlgorithmVOIExtraction VOIExtAlgo = new AlgorithmVOIExtraction(tmpMask);
			
			        VOIExtAlgo.setRunningInSeparateThread(runningInSeparateThread);
			        VOIExtAlgo.run();
			        VOIExtAlgo.finalize();
			        VOIExtAlgo = null;
			        destImage.addVOIs(tmpMask.getVOIs());
			        tmpMask.resetVOIs();
			        for (k = 0; k < oZdim; k++) {
			        	for (j = 0; j < oYdim; j++) {
			        		for (i = 0; i < oXdim; i++) {
			        			tmpMask.set(i, j, k, fillValue);
			        		}
			        	}
			        }
			        index2--;
			        continue;
		        }
		        
		        maskImage.clearMask();
		        
		        (voiVector.elementAt(index)).createOneElementBinaryMask3D(maskImage.getMask(), iXdim, iYdim, false, false, index2);

				BitSet mask = maskImage.getMask();

				
				for (i = 0; i < length; i++) {

					if (mask.get(i)) {
						maskImage.set(i, indexC + 1);
					}
					else {
						maskImage.set(i, 0);
					}
				}
		  
		
	            try {
	                maskImage.exportData(0, length, imgBuffer); // locks and releases lock
	            } catch (final IOException error) {
	                displayError("Algorithm VOI transform: Image(s) locked");
	                setCompleted(false);

	                return;
	            }
	            
	            for (i = 0; i < oZdim; i++) {

	                for (j = 0; j < oYdim; j++) {

	                    for (k = 0; k < oXdim; k++) {
	                        result[0] = C[N][0] + (C[N + 1][0] * k) + (C[N + 2][0] * j) + (C[N + 3][0] * i);
	                        result[1] = C[N][1] + (C[N + 1][1] * k) + (C[N + 2][1] * j) + (C[N + 3][1] * i);
	                        result[2] = C[N][2] + (C[N + 1][2] * k) + (C[N + 2][2] * j) + (C[N + 3][2] * i);

	                        for (m = 0; m < N; m++) {
	                            dx = x[m] - k;
	                            dy = y[m] - j;
	                            dz = z[m] - i;
	                            fT = Math.sqrt((dx * dx) + (dy * dy) + (dz * dz));
	                            U = fT;
	                            result[0] += C[m][0] * U;
	                            result[1] += C[m][1] * U;
	                            result[2] += C[m][2] * U;
	                        }

	                        value = fillValue; // remains zero if transformed out of bounds
	                        X = result[0];

	                        if ((X >= 0) && (X <= (iXdim - 1))) {
	                            Y = result[1];

	                            if ((Y >= 0) && (Y <= (iYdim - 1))) {
	                                Z = result[2];

	                                if ((Z >= 0) && (Z <= (iZdim - 1))) {

	                                    // Preferences.debug(X+", "+Y+", "+Z, Preferences.DEBUG_ALGORITHM);
	                                    // set intensity of i,j,k to new transformed coordinate if
	                                    // x,y,z is w/in dimensions of image
	                                	X0pos = (int) (X + 0.5f);
		                                X0pos = Math.min(X0pos, iXdim-1);
		                                Y0pos = ((int) (Y + 0.5f)) * iXdim;
		                                Y0pos = Math.min(Y0pos, (iYdim-1)*iXdim);
		                                Z0pos = ((int) (Z + 0.5f)) * sliceSize;
		                                Z0pos = Math.min(Z0pos, (iZdim-1)*sliceSize);
		                                value = imgBuffer[Z0pos + Y0pos + X0pos];
	                                    
	                                } // if Z in bounds
	                            } // if Y in bounds
	                        } // if X in bounds
	                        tmpMask.set(k, j, i, value);
	                    } // for k
	                } // for j
	            } // for i
		        
		       
		        if (threadStopped) {
		            return;
		        }
		
		        // ******* Make algorithm for VOI extraction.
		        if (index2 == curves.size()-1) {
		        	indexC++;
		        	duplicateZ = false;
		        	for (i = 0; i < iZdim; i++) {
		        		zFound[i] = 0;
		        	}
			        tmpMask.calcMinMax();
			
			        AlgorithmVOIExtraction VOIExtAlgo = new AlgorithmVOIExtraction(tmpMask);
			
			        VOIExtAlgo.setRunningInSeparateThread(runningInSeparateThread);
			        VOIExtAlgo.run();
			        VOIExtAlgo.finalize();
			        VOIExtAlgo = null;
			        destImage.addVOIs(tmpMask.getVOIs());
			        tmpMask.resetVOIs();
			        for (k = 0; k < oZdim; k++) {
			        	for (j = 0; j < oYdim; j++) {
			        		for (i = 0; i < oXdim; i++) {
			        			tmpMask.set(i, j, k, fillValue);
			        		}
			        	}
			        }
		        }
        	} // for (index2 = 0; index2 < curves.size(); index2++)
        } // for (index = 0; index < voiVector.size(); index++)
        maskImage.disposeLocal();
        maskImage = null;
        tmpMask.disposeLocal();
        tmpMask = null;

        
    }
    
    /**
     * Saves:
     * 1.) N, the number of matching control points in baseImage and matchImage
     * 2.) Number of rows in C matrix
     * 3.) Number of columns in C matrix
     * 4.) xDim of baseImage
     * 5.) yDim of baseImage
     * 6.) If noncoplanar 3D thin plate spline is used, saves zDim of baseImage
     * 7.) x coordinates of baseImage control points
     * 8.) y coordinates of baseImage control points
     * 9.) If noncoplanar 3D thin plate spline is used, saves z coordinates of baseImage control points
     * 10.) If 2D or coplanar 3D, saves n+3 by 2 C[][] float matrix from setupTPSpline2D
     *      If noncoplanar 3D, saves n+4 by 3 C[][] float matrix from setupTPSpline3D
     * 11.) Saves optional message.
     * @param  fileName  - file name, including the path
     * @param  message optionally write a message at the file end
     */
    public void saveMatrix(String fileName, String message) {
        int row;
        int col;
        int r;
        int c;
        try {
            File file = new File(fileName);
            RandomAccessFile raFile = new RandomAccessFile(file, "rw");
            if (raFile == null) return;

            try {
                raFile.writeBytes(Integer.toString(N) + "\n"); // write N number of points used
                xDimA = baseImage.getExtents()[0];
                yDimA = baseImage.getExtents()[1];
                if (z == null) {
                    row = N +3;
                    col = 2;
                }
                else {
                    row = N + 4;
                    col = 3;
                    zDimA = baseImage.getExtents()[2];
                }
                raFile.writeBytes(Integer.toString(row) + "\n"); // write number of rows in C matrix
                raFile.writeBytes(Integer.toString(col) + "\n"); // write number of columns in C matrix
                raFile.writeBytes(Integer.toString(xDimA) + "\n");
                raFile.writeBytes(Integer.toString(yDimA) + "\n");
                if (col == 3) {
                    raFile.writeBytes(Integer.toString(zDimA) + "\n");
                }
                for (c = 0; c < N; c++) {
                    raFile.writeBytes(Double.toString(x[c]) + " ");
                }
                raFile.writeBytes("\n");
                for (c = 0; c < N; c++) {
                    raFile.writeBytes(Double.toString(y[c]) + " ");
                }
                raFile.writeBytes("\n");
                if (col == 3) {
                    for (c = 0; c < N; c++) {
                        raFile.writeBytes(Double.toString(z[c]) + " ");
                    }
                    raFile.writeBytes("\n");
                } // if (col == 3)
                for (r = 0; r < row; r++) {
                    for (c = 0; c < col; c++) {
                        raFile.writeBytes(Float.toString(C[r][c]) + " ");
                    }
                        
                    raFile.writeBytes("\n");
                }
                raFile.writeBytes("\n");
                if (message != null) {
                    raFile.writeBytes(message);
                }
            } catch (IOException error) {
                MipavUtil.displayError("Matrix save error " + error);
                
                return;
            }
            raFile.close();
        } catch (IOException error) {
            MipavUtil.displayError("Matrix save error " + error);

            return;
        }
    }
    
    /**
     * Reads:
     * 1.) N, the number of matching control points in baseImage and matchImage
     * 2.) Number of rows in C matrix
     * 3.) Number of columns in C matrix
     * 4.) xDim of baseImage
     * 5.) yDim of baseImage
     * 6.) If noncoplanar 3D thin plate spline is used, reads zDim of baseImage
     * 7.) x coordinates of baseImage control points
     * 8.) y coordinates of baseImage control points
     * 9.) If noncoplanar 3D thin plate spline is used, reads z coordinates of baseImage control points
     * 10.) If 2D or coplanar 3D, reads n+3 by 2 C[][] float matrix from setupTPSpline2D
     *      If noncoplanar 3D, reads n+4 by 3 C[][] float matrix from setupTPSpline3D
     * Does not read optional message at end of file.
     * @param raFile file to be read
     */
    public void readMatrix(RandomAccessFile raFile) {
        int row;
        int col;
        int r;
        String str;
        int index;
        int c;
        int nextIndex;
        String tmpStr;
        try {
            raFile.seek(0);
            N = Integer.valueOf(raFile.readLine().trim()).intValue();
            row = Integer.valueOf(raFile.readLine().trim()).intValue();
            col = Integer.valueOf(raFile.readLine().trim()).intValue();
            xDimA = Integer.valueOf(raFile.readLine().trim()).intValue();
            yDimA = Integer.valueOf(raFile.readLine().trim()).intValue();
            if (col == 3) {
                zDimA = Integer.valueOf(raFile.readLine().trim()).intValue();
            }
            x = new double[N];
            y = new double[N];
            if (col == 3) {
                z = new double[N];
            }
            C = new float[row][col];
            // Don't run setupTPSpline2D() or setupTPSpline3D()
            setupRequired = false;
            if (col == 2) {
                run2D = true;
            }
            else {
                run2D = false;
            }
            
            // Read x[]
            str = raFile.readLine().trim();
            index = 0;
           
            for (c = 0; c < N; c++) {

                nextIndex = str.indexOf(" ", index);

                if (nextIndex != -1) {
                    tmpStr = str.substring(index, nextIndex).trim();
                    index = nextIndex + 1;
                } else { // spaces trimmed from end
                    tmpStr = str.substring(index, str.length()).trim();
                    index = nextIndex;
                }
                if (tmpStr.indexOf(".") != -1) {
                    x[c] = Double.valueOf(tmpStr).doubleValue();
                } else {
                    x[c] = Integer.valueOf(tmpStr).doubleValue();
                }
            } // for (c = 0; c < N; c++)
            
            // Read y[]
            str = raFile.readLine().trim();
            index = 0;

            for (c = 0; c < N; c++) {

                nextIndex = str.indexOf(" ", index);

                if (nextIndex != -1) {
                    tmpStr = str.substring(index, nextIndex).trim();
                    index = nextIndex + 1;
                } else { // spaces trimmed from end
                    tmpStr = str.substring(index, str.length()).trim();
                    index = nextIndex;
                }
                if (tmpStr.indexOf(".") != -1) {
                    y[c] = Double.valueOf(tmpStr).doubleValue();
                } else {
                    y[c] = Integer.valueOf(tmpStr).doubleValue();
                }
            } // for (c = 0; c < N; c++)
            
            if (col == 3) {
                // Read z[]
                str = raFile.readLine().trim();
                index = 0;

                for (c = 0; c < N; c++) {

                    nextIndex = str.indexOf(" ", index);

                    if (nextIndex != -1) {
                        tmpStr = str.substring(index, nextIndex).trim();
                        index = nextIndex + 1;
                    } else { // spaces trimmed from end
                        tmpStr = str.substring(index, str.length()).trim();
                        index = nextIndex;
                    }
                    if (tmpStr.indexOf(".") != -1) {
                        z[c] = Double.valueOf(tmpStr).doubleValue();
                    } else {
                        z[c] = Integer.valueOf(tmpStr).doubleValue();
                    }
                } // for (c = 0; c < N; c++)    
            } // if (col == 3)
            
            // Read C[][]
            for (r = 0; r < row; r++) {
                str = raFile.readLine().trim();
                index = 0;

                for (c = 0; c < col; c++) {

                    nextIndex = str.indexOf(" ", index);

                    if (nextIndex != -1) {
                        tmpStr = str.substring(index, nextIndex).trim();
                        index = nextIndex + 1;
                    } else { // spaces trimmed from end
                        tmpStr = str.substring(index, str.length()).trim();
                        index = nextIndex;
                    }
                    if (tmpStr.indexOf(".") != -1) {
                        C[r][c] = Float.valueOf(tmpStr).floatValue();
                    } else {
                        C[r][c] = Integer.valueOf(tmpStr).floatValue();
                    }
                } // for (c = 0; c < col; c++)

            } // for (r = 0; r < row; r++)
        } catch (IOException error) {
            MipavUtil.displayError("Matrix read error " + error);
            
            return;
        }
    }

}
