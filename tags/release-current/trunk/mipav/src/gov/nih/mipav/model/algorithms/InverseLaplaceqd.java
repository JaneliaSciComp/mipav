package gov.nih.mipav.model.algorithms;


import gov.nih.mipav.model.algorithms.filters.FFTUtility;


/**
 * This is a port of niltqd.m, a fast numerical inverse Laplace transform based on FFT and quotient-difference algorithm
 * by Lubomir Brancik, 2001, Brno University of Technology.
 * 
 * <hr>
 * 
 * In doing fits be sure to exclude the t = 0.0 point values as the error associated with this point is far greater than
 * the error associated with the other points. Increasing p above 3 did not have any appreciable effect on the rms
 * error. For the exp(t) example the effect of tol was: tol = 1.0E-8 rms error = 6.29E-5 tol = 1.0E-9 rms error =
 * 7.26E-6 tol = 1.0E-10 rms error = 4.40E-6 tol = 1.0E-11 rms error = 1.13E-5 tol = 1.0E-12 rms error = 3.27E-5
 */
public abstract class InverseLaplaceqd {

    // ~ Instance fields
    // ------------------------------------------------------------------------------------------------

    /** DOCUMENT ME! */
    double endTime;

    /**
     * largest pole of Laplace space function (default zero) Note that InverseLaplace may give incorrect results if the
     * default value of largestPole is used. For example, the function exp(t) with the Laplace transform 1/(s-1) will
     * give correct results if largestPole is set to 1.0, but will return incorrect negative values for time >= 1.0 if
     * the largestPole default of 0.0 is used.
     */
    private double largestPole = 0.0;

    /** DOCUMENT ME! */
    private int p = 3;

    // An example of exp(at) with the Laplace 1/(s - a) is given below.

    /*
     * private void runLapTestqd() { // This routine tests the numerical inverse laplace transform of a // function in
     * InverseLaplace with an analytically known inverse. The // function to be transformed is exp(a*t) // parameter a
     * in exp(a*t) double a = 1.0; // An estimate for the maximum of the real parts of the singularities // of F. If
     * unknown, set largestPole = 0.0 double largestPole = 1.0; // numerical tolerance of approaching pole (default =
     * 1.0e-9) double tol = 1.0e-9; // number of times to invert for int n = 103; int matrixSizeParameter = 3; // vector
     * of times to invert for double[] t = new double[n]; // true value of the function exp(a*t) double[] ftrue = new
     * double[n]; int i; FitExpModelqd lmod; double sse = 0; double diff; double rms;
     * 
     * double[] timeFunction = null;
     * 
     * 
     * for (i = 0; i < n; i++) { t[i] = i*0.1; ftrue[i] = Math.exp(a*t[i]); }
     * 
     * Preferences.debug("matrixSizeParameter = " + matrixSizeParameter + "\n"); Preferences.debug("tol = " + tol +
     * "\n"); lmod = new FitExpModelqd( n, t[n-1],largestPole, tol, matrixSizeParameter ); lmod.driver(); timeFunction =
     * lmod.getTimeFunction(); for ( i = 0; i < n; i++ ) { Preferences.debug( "time = " + t[i] + " routineFunction = " +
     * timeFunction[i] + " trueFunction = " + ftrue[i] + "\n" ); }
     * 
     * sse = 0.0; for (i = 1; i < n; i++) { diff = timeFunction[i] - ftrue[i]; sse = sse + diff*diff; } rms =
     * Math.sqrt(sse/(n-1)); Preferences.debug("rms error = " + rms + "\n");
     *  }
     */

    /*
     * class FitExpModelqd extends InverseLaplaceqd {
     * 
     * public FitExpModelqd( int timePoints, double endTime, double largestPole, double tol, int matrixSizeParameter ) {
     * super( timePoints, endTime, largestPole, tol, matrixSizeParameter ); }
     * 
     * public double[][] fitToLaplace( double realS, double[] imagS ) { // The Laplace transform of exp(at) = 1/(p-a)
     * double[][] ans = new double[imagS.length][2]; double a = 1.0; int i; double denom; double realDenom; realDenom = (
     * realS - a ) * ( realS - a ); for ( i = 0; i < imagS.length; i++ ) { denom = realDenom + imagS[i] * imagS[i]; //
     * real part ans[i][0] = ( realS - a ) / denom; ans[i][1] = -imagS[i] / denom; } return ans; }
     *  }
     */

    /** resulting vector of times. */
    private double[] time = null;

    /** resulting vector of real-space values. */
    private double[] timeFunction = null;

    /** numerical tolerance of approaching pole (default 1.0e-9). */
    private double tol = 1.0e-9;

    /** DOCUMENT ME! */
    private final int tPts;

    // ~ Constructors
    // ---------------------------------------------------------------------------------------------------

    /**
     * Constructor for InverseLaplaceqd.
     * 
     * @param timePoints int
     * @param endTime double
     */
    public InverseLaplaceqd(final int timePoints, final double endTime) {
        this.tPts = timePoints;
        this.endTime = endTime;
    }

    /**
     * Constructor for InverseLaplaceqd.
     * 
     * @param timePoints int
     * @param endTime double
     * @param largestPole double
     * @param tol double
     * @param matrixSizeParameter int
     */
    public InverseLaplaceqd(final int timePoints, final double endTime, final double largestPole, final double tol,
            final int matrixSizeParameter) {
        this.tPts = timePoints;
        this.endTime = endTime;
        this.largestPole = largestPole;
        this.tol = tol;
        this.p = matrixSizeParameter;
    }

    // ~ Methods
    // --------------------------------------------------------------------------------------------------------

    /**
     * The second index is 0 for the real part and 1 for the imaginary part.
     * 
     * @param realS DOCUMENT ME!
     * @param imagS DOCUMENT ME!
     * 
     * @return DOCUMENT ME!
     */
    public abstract double[][] fitToLaplace(double realS, double[] imagS);

    /**
     * driver.
     */
    public void driver() {

        // For variables with 2 indices the second index is 0 for the real part
        // and 1 for the imaginary part
        time = new double[tPts];
        timeFunction = new double[tPts];

        final int N = 2 * tPts;
        final int qd = (2 * p) + 1;
        int i, n;
        double NT;
        double omega;
        double realS;
        final double[] imagS = new double[N + qd];
        double[][] Fsc = new double[N + qd][2];
        final double[] fftR = new double[N];
        final double[] fftI = new double[N];
        final double[][] d = new double[qd][2];
        final double[][] e = new double[qd][2];
        final double[][] q = new double[qd - 1][2];
        final double[][] A2 = new double[tPts][2];
        final double[][] B2 = new double[tPts][2];
        final double[][] A1 = new double[tPts][2];
        final double[][] B1 = new double[tPts][2];
        final double[][] z = new double[tPts][2];
        final double[][] A = new double[tPts][2];
        final double[][] B = new double[tPts][2];
        double denom;
        int r;
        int w;
        double realProd;
        double imagProd;
        FFTUtility fft;

        for (i = 0; i < tPts; i++) {
            time[i] = (i * endTime) / (tPts - 1);
        }

        NT = (2.0 * endTime * N) / (N - 2.0);
        omega = 2.0 * Math.PI / NT;
        realS = largestPole - (Math.log(tol) / NT);

        for (i = 0; i < (N + qd); i++) {
            imagS[i] = -i * omega;
        }

        Fsc = fitToLaplace(realS, imagS);

        for (i = 0; i < N; i++) {
            fftR[i] = Fsc[i][0];
            fftI[i] = Fsc[i][1];
        }

        fft = new FFTUtility(fftR, fftI, 1, N, 1, -1, FFTUtility.FFT);
        fft.run();
        fft.finalize();
        fft = null;

        for (i = 0; i < (qd - 1); i++) {
            denom = (Fsc[N + i][0] * Fsc[N + i][0]) + (Fsc[N + i][1] * Fsc[N + i][1]);
            q[i][0] = ( (Fsc[N + 1 + i][0] * Fsc[N + i][0]) + (Fsc[N + 1 + i][1] * Fsc[N + i][1])) / denom;
            q[i][1] = ( (Fsc[N + 1 + i][1] * Fsc[N + i][0]) - (Fsc[N + 1 + i][0] * Fsc[N + i][1])) / denom;
        }

        d[0][0] = Fsc[N][0];
        d[0][1] = Fsc[N][1];
        d[1][0] = -q[0][0];
        d[1][1] = -q[0][1];

        for (r = 2; r <= (qd - 1); r += 2) {
            w = qd - r;

            for (i = 0; i < w; i++) {
                e[i][0] = q[i + 1][0] - q[i][0] + e[i + 1][0];
                e[i][1] = q[i + 1][1] - q[i][1] + e[i + 1][1];
            } // for (i = 0; i < w; i++)

            d[r][0] = -e[0][0];
            d[r][1] = -e[0][1];

            if (r > 2) {

                for (i = 0; i < (w - 1); i++) {
                    realProd = (q[i + 1][0] * e[i + 1][0]) - (q[i + 1][1] * e[i + 1][1]);
                    imagProd = (q[i + 1][0] * e[i + 1][1]) + (q[i + 1][1] * e[i + 1][0]);
                    denom = (e[i][0] * e[i][0]) + (e[i][1] * e[i][1]);
                    q[i][0] = ( (realProd * e[i][0]) + (imagProd * e[i][1])) / denom;
                    q[i][1] = ( (imagProd * e[i][0]) - (realProd * e[i][1])) / denom;
                } // for (i = 0; i < w-1; i++)

                d[r - 1][0] = -q[0][0];
                d[r - 1][1] = -q[0][1];
            } // if (r > 2)
        } // for (r = 2; r <= qd - 1; r+=2)

        for (i = 0; i < tPts; i++) {
            B2[i][0] = 1.0;
            A1[i][0] = d[0][0];
            A1[i][1] = d[0][1];
            B1[i][0] = 1.0;
            z[i][0] = Math.cos(omega * time[i]);
            z[i][1] = -Math.sin(omega * time[i]);
        } // for (i = 0; i < tPts; i++)

        for (n = 1; n < (qd - 1); n++) {

            for (i = 0; i < tPts; i++) {
                realProd = (d[n][0] * z[i][0]) - (d[n][1] * z[i][1]);
                imagProd = (d[n][0] * z[i][1]) + (d[n][1] * z[i][0]);
                A[i][0] = A1[i][0] + (realProd * A2[i][0]) - (imagProd * A2[i][1]);
                A[i][1] = A1[i][1] + (realProd * A2[i][1]) + (imagProd * A2[i][0]);
                B[i][0] = B1[i][0] + (realProd * B2[i][0]) - (imagProd * B2[i][1]);
                B[i][1] = B1[i][1] + (realProd * B2[i][1]) + (imagProd * B2[i][0]);
                A2[i][0] = A1[i][0];
                A2[i][1] = A1[i][1];
                B2[i][0] = B1[i][0];
                B2[i][1] = B1[i][1];
                A1[i][0] = A[i][0];
                A1[i][1] = A[i][1];
                B1[i][0] = B[i][0];
                B1[i][1] = B[i][1];
            } // for (i = 0; i < tPts; i++)
        } // for (n = 1; n < qd - 1; n++)

        for (i = 0; i < tPts; i++) {
            denom = (B[i][0] * B[i][0]) + (B[i][1] * B[i][1]);
            timeFunction[i] = fftR[i] + ( ( (A[i][0] * B[i][0]) + (A[i][1] * B[i][1])) / denom);
            timeFunction[i] = (2 * timeFunction[i]) - Fsc[0][0];
            timeFunction[i] = Math.exp(realS * time[i]) / NT * timeFunction[i];
        } // for (i = 0; i < tPts; i++)

        timeFunction[0] = 2 * timeFunction[0];
    }

    /**
     * getTime.
     * 
     * @return time
     */
    public double[] getTime() {
        return time;
    }

    /**
     * getTimeFunction.
     * 
     * @return timeFunction
     */
    public double[] getTimeFunction() {
        return timeFunction;
    }

}
