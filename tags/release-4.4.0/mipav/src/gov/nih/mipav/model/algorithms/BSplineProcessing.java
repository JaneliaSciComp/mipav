package gov.nih.mipav.model.algorithms;


/**
 * This is a port of code from a C language spline interpolation program by Philippe Thevenaz found at
 * http://bigwww.epfl.ch/thevenaz/interpolation. The C program is based on the paper: Philippe Thevenaz, Thierry
 * Blu, and Michael Unser, "Interpolation Revisited", IEEE Transactions on Medical Imaging, Vol. 19, No. 7, pp.
 * 739-785, July, 2000.
 */
public class BSplineProcessing {

    /** ?? */
    private static final double DBL_EPSILON = 2.220446e-16;

    /**
     * 2D Spline interpolation routine.
     * 
     * @param coeff input B-spline array of coefficients
     * @param x point to interpolate
     * @param y point to interpolate
     * @param nx image dimensions
     * @param ny image dimensions
     * @param SplineDegree degree of the spline model
     * 
     * @return DOCUMENT ME!
     */
    public final float interpolatedValue(final float[][] coeff, final double x, final double y, final int nx,
            final int ny, final int SplineDegree) {
        final double[] xWeight = new double[6], yWeight = new double[6];
        double interpolated;
        double w, w2, w4, t, t0, t1;
        final int[] xIndex = new int[6], yIndex = new int[6];
        final int nx2 = (2 * nx) - 2, ny2 = (2 * ny) - 2;
        int i, j, k;

        // compute the interpolation indexes
        if ( (SplineDegree % 2) == 1) {

            // System.out.print("spline degree type 1: "+SplineDegree+"\n");
            // odd degree
            i = (int) Math.floor(x) - (SplineDegree / 2);
            j = (int) Math.floor(y) - (SplineDegree / 2);

            for (k = 0; k <= SplineDegree; k++) {
                xIndex[k] = i++;
                yIndex[k] = j++;
            }
        } else {

            // System.out.print("spline degree type 2: "+SplineDegree+"\n");
            // even degree
            i = (int) Math.floor(x + 0.5) - (SplineDegree / 2);
            j = (int) Math.floor(y + 0.5) - (SplineDegree / 2);

            for (k = 0; k <= SplineDegree; k++) {
                xIndex[k] = i++;
                yIndex[k] = j++;
            }
        }

        // compute the interpolation weights
        switch (SplineDegree) {

            case 2:

                // x
                w = x - xIndex[1];
                xWeight[1] = (3.0 / 4.0) - (w * w);
                xWeight[2] = (1.0 / 2.0) * (w - xWeight[1] + 1.0);
                xWeight[0] = 1.0 - xWeight[1] - xWeight[2];

                // y
                w = y - yIndex[1];
                yWeight[1] = (3.0 / 4.0) - (w * w);
                yWeight[2] = (1.0 / 2.0) * (w - yWeight[1] + 1.0);
                yWeight[0] = 1.0 - yWeight[1] - yWeight[2];
                break;

            case 3:

                // x
                w = x - xIndex[1];
                xWeight[3] = (1.0 / 6.0) * w * w * w;
                xWeight[0] = (1.0 / 6.0) + ( (1.0 / 2.0) * w * (w - 1.0)) - xWeight[3];
                xWeight[2] = w + xWeight[0] - (2.0 * xWeight[3]);
                xWeight[1] = 1.0 - xWeight[0] - xWeight[2] - xWeight[3];

                // y
                w = y - yIndex[1];
                yWeight[3] = (1.0 / 6.0) * w * w * w;
                yWeight[0] = (1.0 / 6.0) + ( (1.0 / 2.0) * w * (w - 1.0)) - yWeight[3];
                yWeight[2] = w + yWeight[0] - (2.0 * yWeight[3]);
                yWeight[1] = 1.0 - yWeight[0] - yWeight[2] - yWeight[3];
                break;

            case 4:

                // x
                w = x - xIndex[2];
                w2 = w * w;
                t = (1.0 / 6.0) * w2;
                xWeight[0] = (1.0 / 2.0) - w;
                xWeight[0] *= xWeight[0];
                xWeight[0] *= (1.0 / 24.0) * xWeight[0];
                t0 = w * (t - (11.0 / 24.0));
                t1 = (19.0 / 96.0) + (w2 * ( (1.0 / 4.0) - t));
                xWeight[1] = t1 + t0;
                xWeight[3] = t1 - t0;
                xWeight[4] = xWeight[0] + t0 + ( (1.0 / 2.0) * w);
                xWeight[2] = 1.0 - xWeight[0] - xWeight[1] - xWeight[3] - xWeight[4];

                // y
                w = y - yIndex[2];
                w2 = w * w;
                t = (1.0 / 6.0) * w2;
                yWeight[0] = (1.0 / 2.0) - w;
                yWeight[0] *= yWeight[0];
                yWeight[0] *= (1.0 / 24.0) * yWeight[0];
                t0 = w * (t - (11.0 / 24.0));
                t1 = (19.0 / 96.0) + (w2 * ( (1.0 / 4.0) - t));
                yWeight[1] = t1 + t0;
                yWeight[3] = t1 - t0;
                yWeight[4] = yWeight[0] + t0 + ( (1.0 / 2.0) * w);
                yWeight[2] = 1.0 - yWeight[0] - yWeight[1] - yWeight[3] - yWeight[4];
                break;

            case 5:

                // x
                w = x - xIndex[2];
                w2 = w * w;
                xWeight[5] = (1.0 / 120.0) * w * w2 * w2;
                w2 -= w;
                w4 = w2 * w2;
                w -= 1.0 / 2.0;
                t = w2 * (w2 - 3.0);
                xWeight[0] = ( (1.0 / 24.0) * ( (1.0 / 5.0) + w2 + w4)) - xWeight[5];
                t0 = (1.0 / 24.0) * ( (w2 * (w2 - 5.0)) + (46.0 / 5.0));
                t1 = ( -1.0 / 12.0) * w * (t + 4.0);
                xWeight[2] = t0 + t1;
                xWeight[3] = t0 - t1;
                t0 = (1.0 / 16.0) * ( (9.0 / 5.0) - t);
                t1 = (1.0 / 24.0) * w * (w4 - w2 - 5.0);
                xWeight[1] = t0 + t1;
                xWeight[4] = t0 - t1;

                // y
                w = y - yIndex[2];
                w2 = w * w;
                yWeight[5] = (1.0 / 120.0) * w * w2 * w2;
                w2 -= w;
                w4 = w2 * w2;
                w -= 1.0 / 2.0;
                t = w2 * (w2 - 3.0);
                yWeight[0] = ( (1.0 / 24.0) * ( (1.0 / 5.0) + w2 + w4)) - yWeight[5];
                t0 = (1.0 / 24.0) * ( (w2 * (w2 - 5.0)) + (46.0 / 5.0));
                t1 = ( -1.0 / 12.0) * w * (t + 4.0);
                yWeight[2] = t0 + t1;
                yWeight[3] = t0 - t1;
                t0 = (1.0 / 16.0) * ( (9.0 / 5.0) - t);
                t1 = (1.0 / 24.0) * w * (w4 - w2 - 5.0);
                yWeight[1] = t0 + t1;
                yWeight[4] = t0 - t1;
                break;

            default:
                System.out.print("Invalid spline degree\n");

                return (0.0f);
        }

        // apply the mirror boundary conditions
        for (k = 0; k <= SplineDegree; k++) {
            xIndex[k] = (nx == 1) ? (0) : ( (xIndex[k] < 0) ? ( -xIndex[k] - (nx2 * ( ( -xIndex[k]) / nx2)))
                    : (xIndex[k] - (nx2 * (xIndex[k] / nx2))));

            if (nx <= xIndex[k]) {
                xIndex[k] = nx2 - xIndex[k];
            }

            yIndex[k] = (ny == 1) ? (0) : ( (yIndex[k] < 0) ? ( -yIndex[k] - (ny2 * ( ( -yIndex[k]) / ny2)))
                    : (yIndex[k] - (ny2 * (yIndex[k] / ny2))));

            if (ny <= yIndex[k]) {
                yIndex[k] = ny2 - yIndex[k];
            }
        }

        // perform interpolation
        // System.out.print("["+x+","+y+"] ");
        interpolated = 0.0;

        for (j = 0; j <= SplineDegree; j++) {
            w = 0.0;

            for (i = 0; i <= SplineDegree; i++) {
                w += xWeight[i] * coeff[xIndex[i]][yIndex[j]];
                // System.out.print("("+xIndex[i]+","+yIndex[j]+"|"+coeff[xIndex[i]][yIndex[j]]+")");
            }

            interpolated += yWeight[j] * w;
        }
        // System.out.println("->"+interpolated);

        return (float) (interpolated);
    } // end InterpolatedValue

    /**
     * 3D Spline interpolation routine.
     * 
     * @param coeff input B-spline array of coefficients
     * @param x point to interpolate
     * @param y point to interpolate
     * @param z point to interpolate
     * @param nx image dimensions
     * @param ny image dimensions
     * @param nz image dimensions
     * @param SplineDegree degree of the spline model
     * 
     * @return DOCUMENT ME!
     */
    public final float interpolatedValue(final float[][][] coeff, final double x, final double y, final double z,
            final int nx, final int ny, final int nz, final int SplineDegree) {
        final double[] xWeight = new double[6], yWeight = new double[6], zWeight = new double[6];
        double interpolated;
        double w, w2, w4, t, t0, t1;
        final int[] xIndex = new int[6], yIndex = new int[6], zIndex = new int[6];
        final int nx2 = (2 * nx) - 2, ny2 = (2 * ny) - 2, nz2 = (2 * nz) - 2;
        int i, j, k, l;

        // compute the interpolation indexes
        if ( (SplineDegree % 2) == 1) {
            i = (int) Math.floor(x) - (SplineDegree / 2);
            j = (int) Math.floor(y) - (SplineDegree / 2);
            l = (int) Math.floor(z) - (SplineDegree / 2);

            for (k = 0; k <= SplineDegree; k++) {
                xIndex[k] = i++;
                yIndex[k] = j++;
                zIndex[k] = l++;
            }
        } else {
            i = (int) Math.floor(x + 0.5) - (SplineDegree / 2);
            j = (int) Math.floor(y + 0.5) - (SplineDegree / 2);
            l = (int) Math.floor(z + 0.5) - (SplineDegree / 2);

            for (k = 0; k <= SplineDegree; k++) {
                xIndex[k] = i++;
                yIndex[k] = j++;
                zIndex[k] = l++;
            }
        }

        // compute the interpolation weights
        switch (SplineDegree) {

            case 2:

                // x
                w = x - xIndex[1];
                xWeight[1] = (3.0 / 4.0) - (w * w);
                xWeight[2] = (1.0 / 2.0) * (w - xWeight[1] + 1.0);
                xWeight[0] = 1.0 - xWeight[1] - xWeight[2];

                // y
                w = y - yIndex[1];
                yWeight[1] = (3.0 / 4.0) - (w * w);
                yWeight[2] = (1.0 / 2.0) * (w - yWeight[1] + 1.0);
                yWeight[0] = 1.0 - yWeight[1] - yWeight[2];

                // z
                w = z - zIndex[1];
                zWeight[1] = (3.0 / 4.0) - (w * w);
                zWeight[2] = (1.0 / 2.0) * (w - zWeight[1] + 1.0);
                zWeight[0] = 1.0 - zWeight[1] - zWeight[2];
                break;

            case 3:

                // x
                w = x - xIndex[1];
                xWeight[3] = (1.0 / 6.0) * w * w * w;
                xWeight[0] = (1.0 / 6.0) + ( (1.0 / 2.0) * w * (w - 1.0)) - xWeight[3];
                xWeight[2] = w + xWeight[0] - (2.0 * xWeight[3]);
                xWeight[1] = 1.0 - xWeight[0] - xWeight[2] - xWeight[3];

                // y
                w = y - yIndex[1];
                yWeight[3] = (1.0 / 6.0) * w * w * w;
                yWeight[0] = (1.0 / 6.0) + ( (1.0 / 2.0) * w * (w - 1.0)) - yWeight[3];
                yWeight[2] = w + yWeight[0] - (2.0 * yWeight[3]);
                yWeight[1] = 1.0 - yWeight[0] - yWeight[2] - yWeight[3];

                // z
                w = z - zIndex[1];
                zWeight[3] = (1.0 / 6.0) * w * w * w;
                zWeight[0] = (1.0 / 6.0) + ( (1.0 / 2.0) * w * (w - 1.0)) - zWeight[3];
                zWeight[2] = w + zWeight[0] - (2.0 * zWeight[3]);
                zWeight[1] = 1.0 - zWeight[0] - zWeight[2] - zWeight[3];
                break;

            case 4:

                // x
                w = x - xIndex[2];
                w2 = w * w;
                t = (1.0 / 6.0) * w2;
                xWeight[0] = (1.0 / 2.0) - w;
                xWeight[0] *= xWeight[0];
                xWeight[0] *= (1.0 / 24.0) * xWeight[0];
                t0 = w * (t - (11.0 / 24.0));
                t1 = (19.0 / 96.0) + (w2 * ( (1.0 / 4.0) - t));
                xWeight[1] = t1 + t0;
                xWeight[3] = t1 - t0;
                xWeight[4] = xWeight[0] + t0 + ( (1.0 / 2.0) * w);
                xWeight[2] = 1.0 - xWeight[0] - xWeight[1] - xWeight[3] - xWeight[4];

                // y
                w = y - yIndex[2];
                w2 = w * w;
                t = (1.0 / 6.0) * w2;
                yWeight[0] = (1.0 / 2.0) - w;
                yWeight[0] *= yWeight[0];
                yWeight[0] *= (1.0 / 24.0) * yWeight[0];
                t0 = w * (t - (11.0 / 24.0));
                t1 = (19.0 / 96.0) + (w2 * ( (1.0 / 4.0) - t));
                yWeight[1] = t1 + t0;
                yWeight[3] = t1 - t0;
                yWeight[4] = yWeight[0] + t0 + ( (1.0 / 2.0) * w);
                yWeight[2] = 1.0 - yWeight[0] - yWeight[1] - yWeight[3] - yWeight[4];

                // z
                w = z - zIndex[2];
                w2 = w * w;
                t = (1.0 / 6.0) * w2;
                zWeight[0] = (1.0 / 2.0) - w;
                zWeight[0] *= zWeight[0];
                zWeight[0] *= (1.0 / 24.0) * zWeight[0];
                t0 = w * (t - (11.0 / 24.0));
                t1 = (19.0 / 96.0) + (w2 * ( (1.0 / 4.0) - t));
                zWeight[1] = t1 + t0;
                zWeight[3] = t1 - t0;
                zWeight[4] = zWeight[0] + t0 + ( (1.0 / 2.0) * w);
                zWeight[2] = 1.0 - zWeight[0] - zWeight[1] - zWeight[3] - zWeight[4];
                break;

            case 5:

                // x
                w = x - xIndex[2];
                w2 = w * w;
                xWeight[5] = (1.0 / 120.0) * w * w2 * w2;
                w2 -= w;
                w4 = w2 * w2;
                w -= 1.0 / 2.0;
                t = w2 * (w2 - 3.0);
                xWeight[0] = ( (1.0 / 24.0) * ( (1.0 / 5.0) + w2 + w4)) - xWeight[5];
                t0 = (1.0 / 24.0) * ( (w2 * (w2 - 5.0)) + (46.0 / 5.0));
                t1 = ( -1.0 / 12.0) * w * (t + 4.0);
                xWeight[2] = t0 + t1;
                xWeight[3] = t0 - t1;
                t0 = (1.0 / 16.0) * ( (9.0 / 5.0) - t);
                t1 = (1.0 / 24.0) * w * (w4 - w2 - 5.0);
                xWeight[1] = t0 + t1;
                xWeight[4] = t0 - t1;

                // y
                w = y - yIndex[2];
                w2 = w * w;
                yWeight[5] = (1.0 / 120.0) * w * w2 * w2;
                w2 -= w;
                w4 = w2 * w2;
                w -= 1.0 / 2.0;
                t = w2 * (w2 - 3.0);
                yWeight[0] = ( (1.0 / 24.0) * ( (1.0 / 5.0) + w2 + w4)) - yWeight[5];
                t0 = (1.0 / 24.0) * ( (w2 * (w2 - 5.0)) + (46.0 / 5.0));
                t1 = ( -1.0 / 12.0) * w * (t + 4.0);
                yWeight[2] = t0 + t1;
                yWeight[3] = t0 - t1;
                t0 = (1.0 / 16.0) * ( (9.0 / 5.0) - t);
                t1 = (1.0 / 24.0) * w * (w4 - w2 - 5.0);
                yWeight[1] = t0 + t1;
                yWeight[4] = t0 - t1;

                // z
                w = z - zIndex[2];
                w2 = w * w;
                zWeight[5] = (1.0 / 120.0) * w * w2 * w2;
                w2 -= w;
                w4 = w2 * w2;
                w -= 1.0 / 2.0;
                t = w2 * (w2 - 3.0);
                zWeight[0] = ( (1.0 / 24.0) * ( (1.0 / 5.0) + w2 + w4)) - zWeight[5];
                t0 = (1.0 / 24.0) * ( (w2 * (w2 - 5.0)) + (46.0 / 5.0));
                t1 = ( -1.0 / 12.0) * w * (t + 4.0);
                zWeight[2] = t0 + t1;
                zWeight[3] = t0 - t1;
                t0 = (1.0 / 16.0) * ( (9.0 / 5.0) - t);
                t1 = (1.0 / 24.0) * w * (w4 - w2 - 5.0);
                zWeight[1] = t0 + t1;
                zWeight[4] = t0 - t1;
                break;

            default:
                System.out.print("Invalid spline degree\n");

                return (0.0f);
        }

        // apply the mirror boundary conditions
        for (k = 0; k <= SplineDegree; k++) {
            xIndex[k] = (nx == 1) ? (0) : ( (xIndex[k] < 0) ? ( -xIndex[k] - (nx2 * ( ( -xIndex[k]) / nx2)))
                    : (xIndex[k] - (nx2 * (xIndex[k] / nx2))));

            if (nx <= xIndex[k]) {
                xIndex[k] = nx2 - xIndex[k];
            }

            yIndex[k] = (ny == 1) ? (0) : ( (yIndex[k] < 0) ? ( -yIndex[k] - (ny2 * ( ( -yIndex[k]) / ny2)))
                    : (yIndex[k] - (ny2 * (yIndex[k] / ny2))));

            if (ny <= yIndex[k]) {
                yIndex[k] = ny2 - yIndex[k];
            }

            zIndex[k] = (nz == 1) ? (0) : ( (zIndex[k] < 0) ? ( -zIndex[k] - (nz2 * ( ( -zIndex[k]) / nz2)))
                    : (zIndex[k] - (nz2 * (zIndex[k] / nz2))));

            if (nz <= zIndex[k]) {
                zIndex[k] = nz2 - zIndex[k];
            }
        }

        // perform interpolation
        interpolated = 0.0;

        for (i = 0; i <= SplineDegree; i++) {

            for (j = 0; j <= SplineDegree; j++) {

                for (l = 0; l <= SplineDegree; l++) {
                    interpolated += xWeight[i] * yWeight[j] * zWeight[l] * coeff[xIndex[i]][yIndex[j]][zIndex[l]];
                }
            }
        }

        return (float) (interpolated);
    } // end InterpolatedValue

    /**
     * main function for transferring 2D image samples into spline coefficients.
     * 
     * @param Image in-place processing
     * @param nx image dimensions
     * @param ny image dimensions
     * @param SplineDegree degree of the spline model
     * 
     * @return DOCUMENT ME!
     */
    public final int samplesToCoefficients(final float[][] Image, final int nx, final int ny, final int SplineDegree) {
        double[] Line;
        final double[] pole = new double[2];
        int Npoles;

        // recover the poles from a lookup table
        switch (SplineDegree) {

            case 2:
                Npoles = 1;
                pole[0] = Math.sqrt(8.0) - 3.0;
                break;

            case 3:
                Npoles = 1;
                pole[0] = Math.sqrt(3.0) - 2.0;
                break;

            case 4:
                Npoles = 2;
                pole[0] = Math.sqrt(664.0 - Math.sqrt(438976.0)) + Math.sqrt(304.0) - 19.0;
                pole[1] = Math.sqrt(664.0 + Math.sqrt(438976.0)) - Math.sqrt(304.0) - 19.0;
                break;

            case 5:
                Npoles = 2;
                pole[0] = Math.sqrt( (135.0 / 2.0) - Math.sqrt(17745.0 / 4.0)) + Math.sqrt(105.0 / 4.0)
                        - (13.0 / 2.0);
                pole[1] = Math.sqrt( (135.0 / 2.0) + Math.sqrt(17745.0 / 4.0)) - Math.sqrt(105.0 / 4.0)
                        - (13.0 / 2.0);
                break;

            default:
                System.out.print("Invalid spline degree\n");

                return (1);
        }

        // convert the image samples into interpolation coefficients
        // in-place separable process, along x
        Line = new double[nx];

        for (int y = 0; y < ny; y++) {
            getRow(Image, y, nx, Line);
            convertToInterpolationCoefficients(Line, nx, pole, Npoles, BSplineProcessing.DBL_EPSILON);
            putRow(Image, y, nx, Line);
        }

        Line = null;

        // in-place separable process, along y
        Line = new double[ny];

        for (int x = 0; x < nx; x++) {
            getColumn(Image, x, ny, Line);
            convertToInterpolationCoefficients(Line, ny, pole, Npoles, BSplineProcessing.DBL_EPSILON);
            putColumn(Image, x, ny, Line);
        }

        Line = null;

        return (0);
    } // samplesToCoefficients

    /**
     * main function for transferring 3D image samples into spline coefficients.
     * 
     * @param Image in-place processing
     * @param nx image dimensions
     * @param ny image dimensions
     * @param nz image dimensions
     * @param SplineDegree degree of the spline model
     * 
     * @return DOCUMENT ME!
     */
    public final int samplesToCoefficients(final float[][][] Image, final int nx, final int ny, final int nz,
            final int SplineDegree) {
        double[] Line;
        final double[] pole = new double[2];
        int Npoles;

        // recover the poles from a lookup table
        switch (SplineDegree) {

            case 2:
                Npoles = 1;
                pole[0] = Math.sqrt(8.0) - 3.0;
                break;

            case 3:
                Npoles = 1;
                pole[0] = Math.sqrt(3.0) - 2.0;
                break;

            case 4:
                Npoles = 2;
                pole[0] = Math.sqrt(664.0 - Math.sqrt(438976.0)) + Math.sqrt(304.0) - 19.0;
                pole[1] = Math.sqrt(664.0 + Math.sqrt(438976.0)) - Math.sqrt(304.0) - 19.0;
                break;

            case 5:
                Npoles = 2;
                pole[0] = Math.sqrt( (135.0 / 2.0) - Math.sqrt(17745.0 / 4.0)) + Math.sqrt(105.0 / 4.0)
                        - (13.0 / 2.0);
                pole[1] = Math.sqrt( (135.0 / 2.0) + Math.sqrt(17745.0 / 4.0)) - Math.sqrt(105.0 / 4.0)
                        - (13.0 / 2.0);
                break;

            default:
                System.out.print("Invalid spline degree\n");

                return (1);
        }

        // convert the image samples into interpolation coefficients
        // in-place separable process, along x
        Line = new double[nx];

        for (int y = 0; y < ny; y++) {

            for (int z = 0; z < nz; z++) {
                getRow(Image, y, z, nx, Line);
                convertToInterpolationCoefficients(Line, nx, pole, Npoles, BSplineProcessing.DBL_EPSILON);
                putRow(Image, y, z, nx, Line);
            }
        }

        Line = null;

        // in-place separable process, along y
        Line = new double[ny];

        for (int x = 0; x < nx; x++) {

            for (int z = 0; z < nz; z++) {
                getColumn(Image, x, z, ny, Line);
                convertToInterpolationCoefficients(Line, ny, pole, Npoles, BSplineProcessing.DBL_EPSILON);
                putColumn(Image, x, z, ny, Line);
            }
        }

        Line = null;

        // in-place separable process, along z
        Line = new double[nz];

        for (int x = 0; x < nx; x++) {

            for (int y = 0; y < ny; y++) {
                getStack(Image, x, y, nz, Line);
                convertToInterpolationCoefficients(Line, nz, pole, Npoles, BSplineProcessing.DBL_EPSILON);
                putStack(Image, x, y, nz, Line);
            }
        }

        Line = null;

        return (0);
    } // samplesToCoefficients

    /**
     * to convert data points to spline coefficients.
     * 
     * @param coeff : input samples --> output coefficients
     * @param Ndata : number of samples or coefficients
     * @param pole : poles
     * @param Npoles : number of poles
     * @param Tolerance : admissible relative error
     */
    private void convertToInterpolationCoefficients(final double[] coeff, final int Ndata, final double[] pole,
            final int Npoles, final double Tolerance) {
        double Lambda = 1.0;

        // special case required by mirror boundaries
        if (Ndata == 1) {
            return;
        }

        // compute the overall gain
        for (int k = 0; k < Npoles; k++) {
            Lambda = Lambda * (1.0 - pole[k]) * (1.0 - (1.0 / pole[k]));
        }

        // apply the gain
        for (int n = 0; n < Ndata; n++) {
            coeff[n] *= Lambda;
        }

        // loop over all poles
        for (int k = 0; k < Npoles; k++) {

            // causal initialization
            coeff[0] = initialCausalCoefficient(coeff, Ndata, pole[k], Tolerance);

            // causal recursion
            for (int n = 1; n < Ndata; n++) {
                coeff[n] += pole[k] * coeff[n - 1];
            }

            // anticausal initialization
            coeff[Ndata - 1] = initialAntiCausalCoefficient(coeff, Ndata, pole[k]);

            // anticausal recursion
            for (int n = Ndata - 2; 0 <= n; n--) {
                coeff[n] = pole[k] * (coeff[n + 1] - coeff[n]);
            }
        }
    } // convertToInterpolationCoefficients

    /**
     * extract a column from a 2D image.
     * 
     * @param Image DOCUMENT ME!
     * @param x DOCUMENT ME!
     * @param ny DOCUMENT ME!
     * @param Line DOCUMENT ME!
     */
    private void getColumn(final float[][] Image, final int x, final int ny, final double[] Line) {

        for (int y = 0; y < ny; y++) {
            Line[y] = Image[x][y];
        }
    } // getColumn

    /**
     * extract a column from a 3D image.
     * 
     * @param Image DOCUMENT ME!
     * @param x DOCUMENT ME!
     * @param z DOCUMENT ME!
     * @param ny DOCUMENT ME!
     * @param Line DOCUMENT ME!
     */
    private void getColumn(final float[][][] Image, final int x, final int z, final int ny, final double[] Line) {

        for (int y = 0; y < ny; y++) {
            Line[y] = Image[x][y][z];
        }
    } // getColumn

    /**
     * extract a row from a 2D image.
     * 
     * @param Image DOCUMENT ME!
     * @param y DOCUMENT ME!
     * @param nx DOCUMENT ME!
     * @param Line DOCUMENT ME!
     */
    private void getRow(final float[][] Image, final int y, final int nx, final double[] Line) {

        for (int x = 0; x < nx; x++) {
            Line[x] = Image[x][y];
        }
    } // getRow

    /**
     * extract a row from a 3D image.
     * 
     * @param Image DOCUMENT ME!
     * @param y DOCUMENT ME!
     * @param z DOCUMENT ME!
     * @param nx DOCUMENT ME!
     * @param Line DOCUMENT ME!
     */
    private void getRow(final float[][][] Image, final int y, final int z, final int nx, final double[] Line) {

        for (int x = 0; x < nx; x++) {
            Line[x] = Image[x][y][z];
        }
    } // getRow

    /**
     * extract a stack (Z direction) from a 3D image.
     * 
     * @param Image DOCUMENT ME!
     * @param x DOCUMENT ME!
     * @param y DOCUMENT ME!
     * @param nz DOCUMENT ME!
     * @param Line DOCUMENT ME!
     */
    private void getStack(final float[][][] Image, final int x, final int y, final int nz, final double[] Line) {

        for (int z = 0; z < nz; z++) {
            Line[z] = Image[x][y][z];
        }
    } // getStack

    /**
     * spline subroutine.
     * 
     * @param coeff : coefficients
     * @param Ndata : number of coefficients
     * @param pole : actual pole
     * 
     * @return DOCUMENT ME!
     */
    private double initialAntiCausalCoefficient(final double[] coeff, final int Ndata, final double pole) {

        // this initialization corresponds to mirror boundaries
        return ( (pole / ( (pole * pole) - 1.0)) * ( (pole * coeff[Ndata - 2]) + coeff[Ndata - 1]));
    } // initialAntiCausalCoefficient

    /**
     * spline subroutine.
     * 
     * @param coeff : coefficients
     * @param Ndata : number of coefficients
     * @param pole : actual pole
     * @param Tolerance : admissible relative error
     * 
     * @return DOCUMENT ME!
     */
    private double initialCausalCoefficient(final double[] coeff, final int Ndata, final double pole,
            final double Tolerance) {
        double Sum, zn, z2n, iz;
        int Horizon;

        // this initialization corresponds to mirror boundaries
        Horizon = Ndata;

        if (Tolerance > 0.0) {
            Horizon = (int) Math.ceil(Math.log(Tolerance) / Math.log(Math.abs(pole)));
        }

        if (Horizon < Ndata) {

            // accelerated loop
            zn = pole;
            Sum = coeff[0];

            for (int n = 1; n < Horizon; n++) {
                Sum += zn * coeff[n];
                zn *= pole;
            }

            return (Sum);
        } else {

            // full loop
            zn = pole;
            iz = 1.0f / pole;
            z2n = Math.pow(pole, (Ndata - 1));
            Sum = coeff[0] + (z2n * coeff[Ndata - 1]);
            z2n *= z2n * iz;

            for (int n = 1; n <= (Ndata - 2); n++) {
                Sum += (zn + z2n) * coeff[n];
                zn *= pole;
                z2n *= iz;
            }

            return (Sum / (1.0f - (zn * zn)));
        }
    } // initialCausalCoefficient

    /**
     * write a column in a 2D image.
     * 
     * @param Image DOCUMENT ME!
     * @param x DOCUMENT ME!
     * @param ny DOCUMENT ME!
     * @param Line DOCUMENT ME!
     */
    private void putColumn(final float[][] Image, final int x, final int ny, final double[] Line) {

        for (int y = 0; y < ny; y++) {
            Image[x][y] = (float) Line[y];
        }
    } // putColumn

    /**
     * write a column in a 3D image.
     * 
     * @param Image DOCUMENT ME!
     * @param x DOCUMENT ME!
     * @param z DOCUMENT ME!
     * @param ny DOCUMENT ME!
     * @param Line DOCUMENT ME!
     */
    private void putColumn(final float[][][] Image, final int x, final int z, final int ny, final double[] Line) {

        for (int y = 0; y < ny; y++) {
            Image[x][y][z] = (float) Line[y];
        }
    } // putColumn

    /**
     * write a row in a 2D image.
     * 
     * @param Image DOCUMENT ME!
     * @param y DOCUMENT ME!
     * @param nx DOCUMENT ME!
     * @param Line DOCUMENT ME!
     */
    private void putRow(final float[][] Image, final int y, final int nx, final double[] Line) {

        for (int x = 0; x < nx; x++) {
            Image[x][y] = (float) Line[x];
        }
    } // putRow

    /**
     * write a row in a 3D image.
     * 
     * @param Image DOCUMENT ME!
     * @param y DOCUMENT ME!
     * @param z DOCUMENT ME!
     * @param nx DOCUMENT ME!
     * @param Line DOCUMENT ME!
     */
    private void putRow(final float[][][] Image, final int y, final int z, final int nx, final double[] Line) {

        for (int x = 0; x < nx; x++) {
            Image[x][y][z] = (float) Line[x];
        }
    } // putRow

    /**
     * write a stack in a 3D image.
     * 
     * @param Image DOCUMENT ME!
     * @param x DOCUMENT ME!
     * @param y DOCUMENT ME!
     * @param nz DOCUMENT ME!
     * @param Line DOCUMENT ME!
     */
    private void putStack(final float[][][] Image, final int x, final int y, final int nz, final double[] Line) {

        for (int z = 0; z < nz; z++) {
            Image[x][y][z] = (float) Line[z];
        }
    } // putStack

} // BSplineProcessing
