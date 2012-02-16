package gov.nih.mipav.model.algorithms;

import javax.vecmath.Vector3d;

import WildMagic.LibFoundation.Mathematics.Vector2f;
import WildMagic.LibFoundation.Mathematics.Vector3f;


/**
 * Fourth-order Bspline for 1-3D lines and 2D surface. This class is based on code from
 * Dave Eberly's MAGIC C++ library.
 *
 */
public class AlgorithmBSpline extends AlgorithmBase {

    //~ Static fields/initializers -------------------------------------------------------------------------------------

    /** Blending matrix for a 4th degree Bspline. */
    private static float[][] mat4 = {
        { 0.04166667f, -0.16666667f, 0.25f, -0.16666667f, 0.04166667f },
        { 0.45833333f, -0.50000000f, -0.25f, 0.50000000f, -0.16666667f },
        { 0.45833333f, 0.50000000f, -0.25f, -0.50000000f, 0.25000000f },
        { 0.04166667f, 0.16666667f, 0.25f, 0.16666667f, -0.16666667f },
        { 0f, 0f, 0f, 0, 0.04166667f }
    };

    /** Blending matrix for 3rd degree B spline. */
    private static float[][] mat3 = {
        { 0.16666667f, -0.5f, 0.5f, -0.16666667f },
        { 0.66666667f, 0, -1, 0.5f },
        { 0.16666667f, 0.5f, 0.5f, -0.5f },
        { 0, 0, 0, 0.16666667f }
    };

    //~ Instance fields ------------------------------------------------------------------------------------------------

    /** DOCUMENT ME! */
    private float[][] coeff = null;

    /** DOCUMENT ME! */
    private int degree = -1;

    /** DOCUMENT ME! */
    private float[] dt = new float[5];
    
    /** DOCUMENT ME! */
    private double[] dt_double = new double[5];

    /** DOCUMENT ME! */
    private float[] dx = new float[5];

    /** DOCUMENT ME! */
    private float[] dy = new float[5];

    /** DOCUMENT ME! */
    private float[] dz = new float[5];

    /** DOCUMENT ME! */
    private float[] geoms = new float[5];

    /** DOCUMENT ME! */
    private float[] geomx = new float[5];

    /** DOCUMENT ME! */
    private float[] geomy = new float[5];

    /** DOCUMENT ME! */
    private float[] geomz = new float[5];

    /** Global variables used for 2D and 3D Bspline. */
    private float[] inter = null; // new float[xdim*ydim*zdim];

    /** DOCUMENT ME! */
    private float[] interA = null;

    /** DOCUMENT ME! */
    private float[] interB = null;

    /** DOCUMENT ME! */
    private float[] interG = null;

    /** DOCUMENT ME! */
    private float[] interR = null;

    /** DOCUMENT ME! */
    private float[][] mat = null; // new float[degree+1][degree+1];

    /** DOCUMENT ME! */
    private int sliceSize;

    /** DOCUMENT ME! */
    private float[] volume = null;

    /** DOCUMENT ME! */
    private int xBaseOld = -1;

    /** DOCUMENT ME! */
    private int xdim, ydim, zdim;

    /** DOCUMENT ME! */
    private int xold_tbase = -1;

    /** DOCUMENT ME! */
    private int xyold_tbase = -1;

    /** DOCUMENT ME! */
    private int yBaseOld = -1;

    /** DOCUMENT ME! */
    private int zBaseOld = -1;

    //~ Constructors ---------------------------------------------------------------------------------------------------

    /**
     * default constructor.
     */
    public AlgorithmBSpline() { }

    //~ Methods --------------------------------------------------------------------------------------------------------

    /**
     * This method can also be used to calculate derivatives of the Bspline.
     *
     * @param   orderDx  derivative order in x direction (n <= 4 )
     * @param   orderDy  derivative order in y direction (n <= 4 )
     * @param   x        float point index along the Bspline indicating point of interest
     * @param   y        float point index along the Bspline indicating point of interest
     *
     * @return  the Bspline interpolated data point
     */
    public float bSpline2D(int orderDx, int orderDy, float x, float y) {

        int i, j;
        int i0, i1, j0, j1;
        int k0, k1;
        int l0, l1;
        int xbase, ybase;
        float xdiff, ydiff;
        float result;
        float sum;
        int iindex;
        float matj0i0;
        int kx, ky;

        // set base indices and compute partial tensor product (if necessary)
        xbase = (int) x;
        ybase = (int) y;

        if ((xbase != xBaseOld) || (ybase != yBaseOld)) {
            xBaseOld = xbase;
            yBaseOld = ybase;
            kx = xbase - 1;
            ky = ybase - 1;

            // compute_geometry_tensor
            iindex = 0;

            for (i0 = 0; i0 <= degree; i0++) {

                for (i1 = 0; i1 <= degree; i1++) {
                    sum = 0;

                    for (j0 = 0, k0 = kx; j0 <= degree; j0++, k0++) {

                        // l0 = Math.max(Math.min(k0,xdim - 1),0);
                        l0 = xdim - 1;

                        if (k0 < l0) {
                            l0 = k0;
                        }

                        if (l0 < 0) {
                            l0 = 0;
                        }

                        matj0i0 = mat[j0][i0];

                        for (j1 = 0, k1 = ky; j1 <= degree; j1++, k1++) {

                            // l1 = Math.max(Math.min(k1,ydim - 1), 0);
                            l1 = ydim - 1;

                            if (k1 < l1) {
                                l1 = k1;
                            }

                            if (l1 < 0) {
                                l1 = 0;
                            }

                            sum += matj0i0 * mat[j1][i1] * volume[(l1 * xdim) + l0];
                        } // j1
                    } // j0

                    inter[iindex++] = sum;
                } // i1
            } // i0
        }

        // construct relative offsets from base pixel
        xdiff = x - xbase;
        ydiff = y - ybase;

        // compute polynomial terms
        float temp = 1;

        for (int d = 0; d < orderDx; d++) {
            dx[d] = 0;
        }

        for (int d = orderDx; d <= degree; d++) {
            dx[d] = coeff[orderDx][d] * temp;
            temp *= xdiff;
        }

        temp = 1;

        for (int d = 0; d < orderDy; d++) {
            dy[d] = 0;
        }

        for (int d = orderDy; d <= degree; d++) {
            dy[d] = coeff[orderDy][d] * temp;
            temp *= ydiff;
        }

        result = 0;
        iindex = 0;

        for (i = 0; i <= degree; i++) {

            for (j = 0; j <= degree; j++) {
                result += dx[i] * dy[j] * inter[iindex++];
            }
        }

        return result;

    }

    /**
     * This method can also be used to calculate derivatives of the Bspline.
     *
     * @param   orderDx  derivative order in x direction (n <= 4 )
     * @param   orderDy  derivative order in y direction (n <= 4 )
     * @param   x        float point index along the Bspline indicating point of interest
     * @param   y        float point index along the Bspline indicating point of interest
     *
     * @return  the Bspline interpolated data point
     */
    public float[] bSpline2DC(int orderDx, int orderDy, float x, float y) {

        int i, j;
        int i0, i1, j0, j1;
        int k0, k1;
        int l0, l1;
        int xbase, ybase;
        float xdiff, ydiff;
        float[] result = new float[4];
        float sumA, sumR, sumG, sumB;
        int iindexA, iindexR, iindexG, iindexB;
        int tmpIndex;
        float matj0i0;
        int kx, ky;

        // set base indices and compute partial tensor product (if necessary)
        xbase = (int) x;
        ybase = (int) y;

        if ((xbase != xBaseOld) || (ybase != yBaseOld)) {
            xBaseOld = xbase;
            yBaseOld = ybase;
            kx = xbase - 1;
            ky = ybase - 1;

            // compute_geometry_tensor
            iindexA = 0;
            iindexR = 0;
            iindexG = 0;
            iindexB = 0;

            for (i0 = 0; i0 <= degree; i0++) {

                for (i1 = 0; i1 <= degree; i1++) {
                    sumA = 0;
                    sumR = 0;
                    sumG = 0;
                    sumB = 0;

                    for (j0 = 0, k0 = kx; j0 <= degree; j0++, k0++) {

                        // l0 = Math.max(Math.min(k0,xdim - 1), 0);
                        l0 = xdim - 1;

                        if (k0 < l0) {
                            l0 = k0;
                        }

                        if (l0 < 0) {
                            l0 = 0;
                        }

                        matj0i0 = mat[j0][i0];

                        for (j1 = 0, k1 = ky; j1 <= degree; j1++, k1++) {

                            // l1 = Math.max(Math.min(k1,ydim - 1), 0);
                            l1 = ydim - 1;

                            if (k1 < l1) {
                                l1 = k1;
                            }

                            if (l1 < 0) {
                                l1 = 0;
                            }

                            tmpIndex = 4 * ((l1 * xdim) + l0);
                            sumA += matj0i0 * mat[j1][i1] * volume[tmpIndex];
                            sumR += matj0i0 * mat[j1][i1] * volume[tmpIndex + 1];
                            sumG += matj0i0 * mat[j1][i1] * volume[tmpIndex + 2];
                            sumB += matj0i0 * mat[j1][i1] * volume[tmpIndex + 3];
                        } // j1
                    } // j0

                    interA[iindexA++] = sumA;
                    interR[iindexR++] = sumR;
                    interG[iindexG++] = sumG;
                    interB[iindexB++] = sumB;
                } // i1
            } // i0
        }

        // construct relative offsets from base pixel
        xdiff = x - xbase;
        ydiff = y - ybase;

        // compute polynomial terms
        float temp = 1;

        for (int d = 0; d < orderDx; d++) {
            dx[d] = 0;
        }

        for (int d = orderDx; d <= degree; d++) {
            dx[d] = coeff[orderDx][d] * temp;
            temp *= xdiff;
        }

        temp = 1;

        for (int d = 0; d < orderDy; d++) {
            dy[d] = 0;
        }

        for (int d = orderDy; d <= degree; d++) {
            dy[d] = coeff[orderDy][d] * temp;
            temp *= ydiff;
        }

        result[0] = 0;
        result[1] = 0;
        result[2] = 0;
        result[3] = 0;
        iindexA = 0;
        iindexR = 0;
        iindexG = 0;
        iindexB = 0;

        for (i = 0; i <= degree; i++) {

            for (j = 0; j <= degree; j++) {
                result[0] += dx[i] * dy[j] * interA[iindexA++];
                result[1] += dx[i] * dy[j] * interR[iindexR++];
                result[2] += dx[i] * dy[j] * interG[iindexG++];
                result[3] += dx[i] * dy[j] * interB[iindexB++];
            }
        }

        return result;

    }

    /**
     * 3D graph Bspline for black and white. This method can also be used to calculate derivatives of the Bspline.
     * WARNING - programmer must call setup3DBSpline before using this function!!!
     *
     * @param   orderDx  derivative order in x direction (n <= 4 )
     * @param   orderDy  derivative order in y direction (n <= 4 )
     * @param   orderDz  derivative order in z direction (n <= 4 )
     * @param   x        float point index along the Bspline indicating point of interest
     * @param   y        float point index along the Bspline indicating point of interest
     * @param   z        float point index along the Bspline indicating pointof interest
     *
     * @return  the Bspline interpolated data point
     */

    public float bSpline3D(int orderDx, int orderDy, int orderDz, float x, float y, float z) {
        int i, j, k;
        int iindex;
        int i0, i1, i2, j0, j1, j2, k0, k1, k2, l0, l1, l2;

        int xbase, ybase, zbase;
        int kx, ky, kz; // optimizers
        float xdiff, ydiff, zdiff;
        float result;
        float sum;

        int yOffset;
        float matj0i0, matj1i1; // optimizers

        // set base indices and compute partial tensor product (if necessary)
        xbase = (int) x;
        ybase = (int) y;
        zbase = (int) z;

        // do only when int(x, y, or z) changes
        if ((xbase != xBaseOld) || (ybase != yBaseOld) || (zbase != zBaseOld)) {
            xBaseOld = xbase;
            yBaseOld = ybase;
            zBaseOld = zbase;
            kz = zbase - 1;
            ky = ybase - 1;
            kx = xbase - 1;
            iindex = 0;

            for (i0 = 0; i0 <= degree; i0++) {

                for (i1 = 0; i1 <= degree; i1++) {

                    for (i2 = 0; i2 <= degree; i2++) {
                        sum = 0;

                        for (j0 = 0, k0 = kx; j0 <= degree; j0++, k0++) {

                            // l0 = Math.max(Math.min(k0,xdim - 1), 0);
                            l0 = xdim - 1;

                            if (k0 < l0) {
                                l0 = k0;
                            }

                            if (l0 < 0) {
                                l0 = 0;
                            }

                            matj0i0 = mat[j0][i0];

                            for (j1 = 0, k1 = ky; j1 <= degree; j1++, k1++) {

                                // l1 = Math.max(Math.min(k1, ydim - 1), 0);
                                l1 = ydim - 1;

                                if (k1 < l1) {
                                    l1 = k1;
                                }

                                if (l1 < 0) {
                                    l1 = 0;
                                }

                                matj1i1 = mat[j1][i1] * matj0i0;
                                yOffset = (xdim * l1) + l0;

                                for (j2 = 0, k2 = kz; j2 <= degree; j2++, k2++) {

                                    // l2 = Math.max(Math.min(k2, zdim - 1), 0);
                                    l2 = zdim - 1;

                                    if (k2 < l2) {
                                        l2 = k2;
                                    }

                                    if (l2 < 0) {
                                        l2 = 0;
                                    }

                                    sum += matj1i1 * mat[j2][i2] * volume[yOffset + (l2 * sliceSize)];
                                } // j2
                            } // j1
                        } // j0

                        inter[iindex++] = sum;
                    } // i2
                } // i1
            } // i0
        }

        // construct relative offsets from base pixel
        xdiff = x - xbase;
        ydiff = y - ybase;
        zdiff = z - zbase;

        // compute polynomial terms
        float temp = 1;
        int d;

        for (d = 0; d < orderDx; d++) {
            dx[d] = 0;
        }

        for (d = orderDx; d <= degree; d++) {
            dx[d] = coeff[orderDx][d] * temp;
            temp *= xdiff;
        }

        temp = 1;

        for (d = 0; d < orderDy; d++) {
            dy[d] = 0;
        }

        for (d = orderDy; d <= degree; d++) {
            dy[d] = coeff[orderDy][d] * temp;
            temp *= ydiff;
        }

        temp = 1;

        for (d = 0; d < orderDz; d++) {
            dz[d] = 0;
        }

        for (d = orderDz; d <= degree; d++) {
            dz[d] = coeff[orderDz][d] * temp;
            temp *= zdiff;
        }

        result = 0;
        iindex = 0;

        for (i = 0; i <= degree; i++) {

            for (j = 0; j <= degree; j++) {

                for (k = 0; k <= degree; k++) {
                    result += dx[i] * dy[j] * dz[k] * inter[iindex++];
                }
            }
        }

        return result;
    }

    /**
     * 3D graph Bspline for color. This method can also be used to calculate derivatives of the Bspline. WARNING -
     * programmer must call setup3DBSpline before using this function!!!
     *
     * @param   orderDx  derivative order in x direction (n <= 4 )
     * @param   orderDy  derivative order in y direction (n <= 4 )
     * @param   orderDz  derivative order in z direction (n <= 4 )
     * @param   x        float point index along the Bspline indicating point of interest
     * @param   y        float point index along the Bspline indicating point of interest
     * @param   z        float point index along the Bspline indicating point of interest
     *
     * @return  the Bspline interpolated data point
     */

    public float[] bSpline3DC(int orderDx, int orderDy, int orderDz, float x, float y, float z) {
        int i, j, k;
        int iindexA, iindexR, iindexG, iindexB;
        int i0, i1, i2, j0, j1, j2, k0, k1, k2, l0, l1, l2;

        int xbase, ybase, zbase;
        int kx, ky, kz; // optimizers
        float xdiff, ydiff, zdiff;
        float[] result = new float[4];
        float sumA, sumR, sumG, sumB;
        int tmpIndex;

        int yOffset;
        float matj0i0, matj1i1; // optimizers

        // set base indices and compute partial tensor product (if necessary)
        xbase = (int) x;
        ybase = (int) y;
        zbase = (int) z;

        // do only when int(x, y, or z) changes
        if ((xbase != xBaseOld) || (ybase != yBaseOld) || (zbase != zBaseOld)) {
            xBaseOld = xbase;
            yBaseOld = ybase;
            zBaseOld = zbase;
            kz = zbase - 1;
            ky = ybase - 1;
            kx = xbase - 1;
            iindexA = 0;
            iindexR = 0;
            iindexG = 0;
            iindexB = 0;

            for (i0 = 0; i0 <= degree; i0++) {

                for (i1 = 0; i1 <= degree; i1++) {

                    for (i2 = 0; i2 <= degree; i2++) {
                        sumA = 0;
                        sumR = 0;
                        sumG = 0;
                        sumB = 0;

                        for (j0 = 0, k0 = kx; j0 <= degree; j0++, k0++) {

                            // l0 = Math.max(Math.min(k0,xdim - 1), 0);
                            l0 = xdim - 1;

                            if (k0 < l0) {
                                l0 = k0;
                            }

                            if (l0 < 0) {
                                l0 = 0;
                            }

                            matj0i0 = mat[j0][i0];

                            for (j1 = 0, k1 = ky; j1 <= degree; j1++, k1++) {

                                // l1 = Math.max(Math.min(k1, ydim - 1), 0);
                                l1 = ydim - 1;

                                if (k1 < l1) {
                                    l1 = k1;
                                }

                                if (l1 < 0) {
                                    l1 = 0;
                                }

                                matj1i1 = mat[j1][i1] * matj0i0;
                                yOffset = (xdim * l1) + l0;

                                for (j2 = 0, k2 = kz; j2 <= degree; j2++, k2++) {

                                    // l2 = Math.max(Math.min(k2, zdim - 1), 0);
                                    l2 = zdim - 1;

                                    if (k2 < l2) {
                                        l2 = k2;
                                    }

                                    if (l2 < 0) {
                                        l2 = 0;
                                    }

                                    tmpIndex = 4 * (yOffset + (l2 * sliceSize));
                                    sumA += matj1i1 * mat[j2][i2] * volume[tmpIndex];
                                    sumR += matj1i1 * mat[j2][i2] * volume[tmpIndex + 1];
                                    sumG += matj1i1 * mat[j2][i2] * volume[tmpIndex + 2];
                                    sumB += matj1i1 * mat[j2][i2] * volume[tmpIndex + 3];
                                } // j2
                            } // j1
                        } // j0

                        interA[iindexA++] = sumA;
                        interR[iindexR++] = sumR;
                        interG[iindexG++] = sumG;
                        interB[iindexB++] = sumB;
                    } // i2
                } // i1
            } // i0
        }

        // construct relative offsets from base pixel
        xdiff = x - xbase;
        ydiff = y - ybase;
        zdiff = z - zbase;

        // compute polynomial terms
        float temp = 1;
        int d;

        for (d = 0; d < orderDx; d++) {
            dx[d] = 0;
        }

        for (d = orderDx; d <= degree; d++) {
            dx[d] = coeff[orderDx][d] * temp;
            temp *= xdiff;
        }

        temp = 1;

        for (d = 0; d < orderDy; d++) {
            dy[d] = 0;
        }

        for (d = orderDy; d <= degree; d++) {
            dy[d] = coeff[orderDy][d] * temp;
            temp *= ydiff;
        }

        temp = 1;

        for (d = 0; d < orderDz; d++) {
            dz[d] = 0;
        }

        for (d = orderDz; d <= degree; d++) {
            dz[d] = coeff[orderDz][d] * temp;
            temp *= zdiff;
        }

        result[0] = 0;
        result[1] = 0;
        result[2] = 0;
        result[3] = 0;
        iindexA = 0;
        iindexR = 0;
        iindexG = 0;
        iindexB = 0;

        for (i = 0; i <= degree; i++) {

            for (j = 0; j <= degree; j++) {

                for (k = 0; k <= degree; k++) {
                    result[0] += dx[i] * dy[j] * dz[k] * interA[iindexA++];
                    result[1] += dx[i] * dy[j] * dz[k] * interR[iindexR++];
                    result[2] += dx[i] * dy[j] * dz[k] * interG[iindexG++];
                    result[3] += dx[i] * dy[j] * dz[k] * interB[iindexB++];
                }
            }
        }

        return result;
    }

    /**
     * This method can also be used to calculate derivatives of the Bspline.
     *
     * @param   derivOrder  derivative order (n <= 4 )
     * @param   t           float point index along the Bspline indicating point of interest
     * @param   dataX       control points for the Bspline
     *
     * @return  the Bspline interpolated data point
     */
    public float bSplineJet1D(byte derivOrder, float t, float[] dataX) {

        float tdiff, tdiff2;
        int tbase;
        float x;
        int i;
        int i0, j0, k0;

        tbase = (int) t;

        if (tbase != xold_tbase) {
            xold_tbase = tbase;

            for (i0 = 0; i0 <= 4; i0++) {
                geoms[i0] = 0;

                for (j0 = 0, k0 = tbase - 1; j0 <= 4; j0++, k0++) {
                    geoms[i0] += mat4[j0][i0] * dataX[k0];
                }
            }
        }

        tdiff = t - tbase;

        switch (derivOrder) {

            case 0:
                dt[0] = 1;
                dt[1] = tdiff;
                dt[2] = tdiff * dt[1];
                dt[3] = tdiff * dt[2];
                dt[4] = tdiff * dt[3];
                break;

            case 1:
                tdiff2 = tdiff * tdiff;
                dt[1] = 1;
                dt[2] = 2 * tdiff;
                dt[3] = 3 * tdiff2;
                dt[4] = 4 * tdiff * tdiff2;
                break;

            case 2:
                dt[2] = 2;
                dt[3] = 6 * tdiff;
                dt[4] = 12 * tdiff * tdiff;
                break;

            case 3:
                dt[3] = 6;
                dt[4] = 24 * tdiff;
                break;

            case 4:
                dt[4] = 24;
                break;
        }

        x = 0;

        for (i = derivOrder; i <= 4; i++) {
            x += dt[i] * geoms[i];
        }

        return x;
    }

    /**
     * This method can also be used to calculate derivatives of the Bspline.
     *
     * @param   derivOrder  derivative order (n <= 4 )
     * @param   t           float point index along the Bspline indicating point of interest
     * @param   dataX       control points for the Bspline
     * @param   dataY       control points for the Bspline
     *
     * @return  the Bspline interpolated data point
     */
    public Vector2f bSplineJetXY(int derivOrder, float t, float[] dataX, float[] dataY) {
        float tdiff, tdiff2;
        int tbase;
        int i;
        int i0, j0, k0;
        float x, y;

        tbase = (int) t;

        if (tbase != xyold_tbase) {
            xyold_tbase = tbase;

            for (i0 = 0; i0 <= 4; i0++) {
                geomx[i0] = 0;
                geomy[i0] = 0;

                for (j0 = 0, k0 = tbase - 2; j0 <= 4; j0++, k0++) {
                    geomx[i0] += mat4[j0][i0] * dataX[k0];
                    geomy[i0] += mat4[j0][i0] * dataY[k0];
                }
            }
        }

        tdiff = t - tbase;

        switch (derivOrder) {

            case 0:
                dt[0] = 1;
                dt[1] = tdiff;
                dt[2] = tdiff * dt[1];
                dt[3] = tdiff * dt[2];
                dt[4] = tdiff * dt[3];
                break;

            case 1:
                tdiff2 = tdiff * tdiff;
                dt[1] = 1;
                dt[2] = 2 * tdiff;
                dt[3] = 3 * tdiff2;
                dt[4] = 4 * tdiff * tdiff2;
                break;

            case 2:
                dt[2] = 2;
                dt[3] = 6 * tdiff;
                dt[4] = 12 * tdiff * tdiff;
                break;

            case 3:
                dt[3] = 6;
                dt[4] = 24 * tdiff;
                break;

            case 4:
                dt[4] = 24;
                break;
        }

        x = 0;
        y = 0;

        for (i = derivOrder; i <= 4; i++) {
            x += dt[i] * geomx[i];
            y += dt[i] * geomy[i];
        }

        return (new Vector2f(x, y));
    }

    /**
     * This method can also be used to calculate derivatives of the Bspline.
     *
     * @param   derivOrder  derivative order (n <= 4 )
     * @param   t           float point index along the Bspline indicating point of interest
     * @param   dataX       control points for the Bspline
     * @param   dataY       control points for the Bspline
     * @param   dataZ       control points for the Bspline
     *
     * @return  the Bspline interpolated data point
     */
    public Vector3f bSplineJetXYZ(int derivOrder, float t, float[] dataX, float[] dataY, float[] dataZ) {
        float tdiff, tdiff2;
        int tbase;
        int i;
        int i0, j0, k0;
        float x, y, z;

        tbase = (int) t;

        if (tbase != xyold_tbase) {
            xyold_tbase = tbase;

            for (i0 = 0; i0 <= 4; i0++) {
                geomx[i0] = 0;
                geomy[i0] = 0;
                geomz[i0] = 0;

                for (j0 = 0, k0 = tbase - 2; j0 <= 4; j0++, k0++) {
                    geomx[i0] += mat4[j0][i0] * dataX[k0];
                    geomy[i0] += mat4[j0][i0] * dataY[k0];
                    geomz[i0] += mat4[j0][i0] * dataZ[k0];
                }
            }
        }

        tdiff = t - tbase;

        switch (derivOrder) {

            case 0:
                dt[0] = 1;
                dt[1] = tdiff;
                dt[2] = tdiff * dt[1];
                dt[3] = tdiff * dt[2];
                dt[4] = tdiff * dt[3];
                break;

            case 1:
                tdiff2 = tdiff * tdiff;
                dt[1] = 1;
                dt[2] = 2 * tdiff;
                dt[3] = 3 * tdiff2;
                dt[4] = 4 * tdiff * tdiff2;
                break;

            case 2:
                dt[2] = 2;
                dt[3] = 6 * tdiff;
                dt[4] = 12 * tdiff * tdiff;
                break;

            case 3:
                dt[3] = 6;
                dt[4] = 24 * tdiff;
                break;

            case 4:
                dt[4] = 24;
                break;
        }

        x = 0;
        y = 0;
        z = 0;

        for (i = derivOrder; i <= 4; i++) {
            x += dt[i] * geomx[i];
            y += dt[i] * geomy[i];
            z += dt[i] * geomz[i];
        }

        return (new Vector3f(x, y, z));
    }
    
    
    
    
    
    
    
    
    
    
    /**
     * This method can also be used to calculate derivatives of the Bspline.
     *
     * @param   derivOrder  derivative order (n <= 4 )
     * @param   t           float point index along the Bspline indicating point of interest
     * @param   dataX       control points for the Bspline
     * @param   dataY       control points for the Bspline
     * @param   dataZ       control points for the Bspline
     *
     * @return  the Bspline interpolated data point
     */
    public Vector3d bSplineJetXYZ_double(int derivOrder, float t, float[] dataX, float[] dataY, float[] dataZ) {
        double tdiff, tdiff2;
        int tbase;
        int i;
        int i0, j0, k0;
        double x, y, z;

        tbase = (int) t;
        
        double[] geomx = new double[5];
        double[] geomy = new double[5];
        double[] geomz = new double[5];
        
        double[][] mat4 = {
                { 0.04166666667, -0.16666666667, 0.25, -0.16666666667, 0.04166666667 },
                { 0.45833333, -0.50000000, -0.25, 0.50000000, -0.16666666667 },
                { 0.45833333, 0.50000000, -0.25, -0.50000000, 0.25000000 },
                { 0.04166666667, 0.16666666667, 0.25, 0.16666666667, -0.16666666667 },
                { 0, 0, 0, 0, 0.04166666667 }
            };

        if (tbase != xyold_tbase) {
            xyold_tbase = tbase;

            for (i0 = 0; i0 <= 4; i0++) {
                geomx[i0] = 0;
                geomy[i0] = 0;
                geomz[i0] = 0;

                for (j0 = 0, k0 = tbase - 2; j0 <= 4; j0++, k0++) {
                    geomx[i0] += mat4[j0][i0] * (double)dataX[k0];
                    geomy[i0] += mat4[j0][i0] * (double)dataY[k0];
                    geomz[i0] += mat4[j0][i0] * (double)dataZ[k0];
                }
            }
        }

        tdiff = t - tbase;

        switch (derivOrder) {

            case 0:
            	dt_double[0] = 1;
            	dt_double[1] = tdiff;
            	dt_double[2] = tdiff * dt_double[1];
            	dt_double[3] = tdiff * dt_double[2];
            	dt_double[4] = tdiff * dt_double[3];
                break;

            case 1:
                tdiff2 = tdiff * tdiff;
                dt_double[1] = 1;
                dt_double[2] = 2 * tdiff;
                dt_double[3] = 3 * tdiff2;
                dt_double[4] = 4 * tdiff * tdiff2;
                break;

            case 2:
            	dt_double[2] = 2;
            	dt_double[3] = 6 * tdiff;
            	dt_double[4] = 12 * tdiff * tdiff;
                break;

            case 3:
            	dt_double[3] = 6;
            	dt_double[4] = 24 * tdiff;
                break;

            case 4:
            	dt_double[4] = 24;
                break;
        }

        x = 0;
        y = 0;
        z = 0;

        for (i = derivOrder; i <= 4; i++) {
            x += dt[i] * geomx[i];
            y += dt[i] * geomy[i];
            z += dt[i] * geomz[i];
        }

        return (new Vector3d(x, y, z));
    }
    
    
    
    
    
    
    
    

    /**
     * Clean up memeory.
     */
    public void finalize() {
        geoms = null;
        geomx = null;
        geomy = null;
        geomz = null;
        dt = null;
        dx = null;
        dy = null;
        dz = null;

        volume = null;
        inter = null;
        interA = null;
        interR = null;
        interG = null;
        interB = null;
        mat = null;
        coeff = null;

        super.finalize();
    }

    /**
     * Resets parameters used by the Bsplines.
     */
    public void resetBspline() {
        xold_tbase = -1;
        xyold_tbase = -1;
        xBaseOld = -1;
    }

    /**
     * Default method that is not really appropriate for this class but must be defined because this class extends
     * AlgorithmBase.
     */
    public void runAlgorithm() { }

    /**
     * Allocates memory and constructs arrays needed for BSpline.
     *
     * @param  vol      volume comprising control points for the Bspline
     * @param  extents  vol extents (xDim,yDim)
     * @param  _degree  degree of spline (3 or 4)
     */
    public void setup2DBSpline(float[] vol, int[] extents, int _degree) {
        volume = vol;
        xdim = extents[0];
        ydim = extents[1];
        degree = _degree;
        inter = new float[(degree + 1) * (degree + 1)];
        coeff = new float[degree + 1][degree + 1];
        mat = new float[degree + 1][degree + 1];

        // construct array of polynomial coefficients
        for (int r = 0; r <= degree; r++) {

            for (int c = 0; c < r; c++) {
                coeff[r][c] = 0;
            }

            for (int c = r; c <= degree; c++) {
                coeff[r][c] = 1;

                for (int i = 0; i < r; i++) {
                    coeff[r][c] *= c - i;
                }
            }
        }

        if (degree == 3) {
            mat = mat3;
        } else if (degree == 4) {
            mat = mat4;
        }

        return;
    }

    /**
     * Allocates memory and constructs arrays needed for BSpline.
     *
     * @param  vol      volume comprising control points for the Bspline
     * @param  extents  vol extents (xDim,yDim)
     * @param  _degree  degree of spline (3 or 4)
     */
    public void setup2DBSplineC(float[] vol, int[] extents, int _degree) {
        volume = vol;
        xdim = extents[0];
        ydim = extents[1];
        degree = _degree;
        interA = new float[(degree + 1) * (degree + 1)];
        interR = new float[(degree + 1) * (degree + 1)];
        interG = new float[(degree + 1) * (degree + 1)];
        interB = new float[(degree + 1) * (degree + 1)];
        coeff = new float[degree + 1][degree + 1];
        mat = new float[degree + 1][degree + 1];

        // construct array of polynomial coefficients
        for (int r = 0; r <= degree; r++) {

            for (int c = 0; c < r; c++) {
                coeff[r][c] = 0;
            }

            for (int c = r; c <= degree; c++) {
                coeff[r][c] = 1;

                for (int i = 0; i < r; i++) {
                    coeff[r][c] *= c - i;
                }
            }
        }

        if (degree == 3) {
            mat = mat3;
        } else if (degree == 4) {
            mat = mat4;
        }

        return;
    }

    /**
     * setup3DBSpline - setup 3D Bspline for black and white. Allocates memory and constructs arrays needed for BSpline
     *
     * @param  vol      volume comprising control points for the Bspline
     * @param  extents  vol extents (xDim,yDim, zDim)
     * @param  _degree  degree of spline (3 or 4)
     */
    public void setup3DBSpline(float[] vol, int[] extents, int _degree) {
        volume = vol;
        xdim = extents[0];
        ydim = extents[1];
        zdim = extents[2];
        sliceSize = xdim * ydim;
        degree = _degree;
        inter = new float[(degree + 1) * (degree + 1) * (degree + 1)];
        coeff = new float[degree + 1][degree + 1];
        mat = new float[degree + 1][degree + 1];

        // construct array of polynomial coefficients
        for (int r = 0; r <= degree; r++) {

            for (int c = 0; c < r; c++) {
                coeff[r][c] = 0;
            }

            for (int c = r; c <= degree; c++) {
                coeff[r][c] = 1;

                for (int i = 0; i < r; i++) {
                    coeff[r][c] *= c - i;
                }
            }
        }

        if (degree == 3) {
            mat = mat3;
        } else if (degree == 4) {
            mat = mat4;
        }

        return;
    }

    /**
     * Allocates memory and constructs arrays needed for BSpline.
     *
     * @param  vol      volume comprising control points for the Bspline
     * @param  extents  vol extents (xDim,yDim, zDim)
     * @param  _degree  degree of spline (3 or 4)
     */
    public void setup3DBSplineC(float[] vol, int[] extents, int _degree) {
        volume = vol;
        xdim = extents[0];
        ydim = extents[1];
        zdim = extents[2];
        sliceSize = xdim * ydim;
        degree = _degree;
        interA = new float[(degree + 1) * (degree + 1) * (degree + 1)];
        interR = new float[(degree + 1) * (degree + 1) * (degree + 1)];
        interG = new float[(degree + 1) * (degree + 1) * (degree + 1)];
        interB = new float[(degree + 1) * (degree + 1) * (degree + 1)];
        coeff = new float[degree + 1][degree + 1];
        mat = new float[degree + 1][degree + 1];

        // construct array of polynomial coefficients
        for (int r = 0; r <= degree; r++) {

            for (int c = 0; c < r; c++) {
                coeff[r][c] = 0;
            }

            for (int c = r; c <= degree; c++) {
                coeff[r][c] = 1;

                for (int i = 0; i < r; i++) {
                    coeff[r][c] *= c - i;
                }
            }
        }

        if (degree == 3) {
            mat = mat3;
        } else if (degree == 4) {
            mat = mat4;
        }

        return;
    }
}
