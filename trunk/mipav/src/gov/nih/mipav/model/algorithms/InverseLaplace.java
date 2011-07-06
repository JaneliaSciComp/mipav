package gov.nih.mipav.model.algorithms;


import gov.nih.mipav.view.Preferences;


/**
 * This is a port of the MATLAB INVLAP.M, a numerical inverse Laplace transform using the de Hoog algorithm, copyright
 * by Karl Hollenbeck on November 22, 1996, Department of Hydrodynamics and Water Resources, Technical University of
 * Denmark, DK-2800 Lyngby email:karl@isv16.isva.dtu.dk Downloaded from MATLAB central in ECP1 Software.zip.
 * 
 * <p>
 * The algorithm used is deHoog et al's quotient difference method with accelerated convergence for the continued
 * fraction expansion published in "An improved method for numerical inversion of Laplace transforms" by F. R. de Hoog,
 * J. H. Knight, and A. N. Stokes, SIAM Journal on Scientific and Statistical Computing, Vol. 3, No. 3, September, 1982,
 * pp. 357-366.
 * </p>
 * 
 * <p>
 * Hollenbeck algorithm modification: The time vector is split in segments of equal magnitude which are inverted
 * individually. This gives better overall accuracy.
 * </p>
 * 
 * <hr>
 * 
 * <p>
 * From the original MATLAB source code:
 * </p>
 * 
 * <pre>
 * Copyright: Karl Hollenbeck
 * Department of Hydrodynamics and Water Resources
 * Technical University of Denmark, DK-2800 Lyngby
 * email: karl@isv16.isva.dtu.dk
 * 
 * 22 Nov 1996, MATLAB 5 version 27 Jun 1997 updated 1 Oct 1998
 * 
 * IF YOU PUBLISH WORK BENEFITING FROM THIS M-FILE, PLEASE CITE IT AS:
 * Hollenbeck, K. J. (1998) INVLAP.M: A matlab function for numerical 
 * inversion of Laplace transforms by the de Hoog algorithm, 
 * http://www.isva.dtu.dk/staff/karl/invlap.htm
 * </pre>
 */

// An example of exp(at) with the Laplace 1/(s - a) is given below.
/*
 * private void runLapTest() { // This routine tests the numerical inverse laplace transform of a // function in
 * InverseLaplace with an analytically known inverse. The // function to be transformed is exp(at) // parameter a in
 * exp(at) double a = 1.0; // An estimate for the maximum of the real parts of the singularities // of F. If unknown,
 * set largestPole = 0.0 double largestPole = 1.0; // numerical tolerance of approaching pole (default = 1.0e-9) double
 * tol = 1.0e-9; // number of times to invert for int n = 30; // vector of times to invert for double t[] = new
 * double[n]; // true value of the function exp(at) double ftrue[] = new double[n]; int i; FitExpModel lmod; double
 * timeFunction[] = null; for (i = 1; i <= n; i++) { t[i-1] = Math.pow(10.0,0.5(double)i - 13.0); ftrue[i-1] =
 * Math.exp(at[i-1]); } lmod = new FitExpModel(t, largestPole, tol); lmod.driver(); timeFunction =
 * lmod.getTimeFunction(); for (i = 0; i < n; i++) { Preferences.debug("time = " + t[i] + " routineFunction = " +
 * timeFunction[i] + " trueFunction = " + ftrue[i] + "\n"); } } class FitExpModel extends InverseLaplace { public
 * FitExpModel(double time[], double largestPole, double tol) { super(time, largestPole, tol); } public double[][]
 * fitToLaplace(double realS, double[] imagS) { // The Laplace transform of exp(at) = 1/(p-a) double ans[][] = new
 * double [imagS.length][2]; double a = 1.0; int i; double denom; double realDenom; realDenom = (realS - a) (realS - a);
 * for (i = 0; i < imagS.length; i++) { denom = realDenom + imagS[i] imagS[i]; // real part ans[i][0] = (realS -
 * a)/denom; ans[i][1] = -imagS[i]/denom; } return ans; } }
 */

public abstract class InverseLaplace {

    // ~ Instance fields
    // ------------------------------------------------------------------------------------------------

    /**
     * Don't include a time == 0. This will give a -inifinity when the log of the time is taken. largest pole of Laplace
     * space function (default zero) Note that InverseLaplace may give incorrect results if the default value of
     * largestPole is used. For example, the function exp(t) with the Laplace transform 1/(s-1) will give correct
     * results if largestPole is set to 1.0, but will return incorrect negative values for time >= 1.0 if the
     * largestPole default of 0.0 is used.
     */
    private double largestPole = 0.0;

    /** DOCUMENT ME! */
    private double[] logT = null;

    /** DOCUMENT ME! */
    private double[] time;

    /** resulting vector of real-space values. */
    private double[] timeFunction = null;

    /** numerical tolerance of approaching pole (default 1.0e-9). */
    private double tol = 1.0e-9;

    /** DOCUMENT ME! */
    private int tPts;

    // ~ Constructors
    // ---------------------------------------------------------------------------------------------------

    /**
     * Constructor for InverseLaplace.
     * 
     * @param time DOCUMENT ME!
     */
    public InverseLaplace(final double[] time) {

        try {
            this.time = time;
            tPts = time.length;
            logT = new double[tPts];
            timeFunction = new double[tPts];
        } catch (final OutOfMemoryError error) {}
    }

    /**
     * Constructor for InverseLaplace.
     * 
     * @param time DOCUMENT ME!
     * @param largestPole DOCUMENT ME!
     * @param tol DOCUMENT ME!
     */
    public InverseLaplace(final double[] time, final double largestPole, final double tol) {

        try {
            this.time = time;
            this.largestPole = largestPole;
            this.tol = tol;
            tPts = time.length;
            logT = new double[tPts];
            timeFunction = new double[tPts];
        } catch (final OutOfMemoryError error) {}
    }

    // ~ Methods
    // --------------------------------------------------------------------------------------------------------

    /**
     * Returns 'a' coefficients in power series where the second index is 0 for the real part and 1 for the imaginary
     * part.
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
        int i, j;
        double minLogT = Double.MAX_VALUE;
        double maxLogT = -Double.MAX_VALUE;
        int iMinLogT;
        int iMaxLogT;
        int iLogT;
        int segPoints = 0;
        double[] segT = null;
        double T = 0.0;
        double realS;
        final int M = 50;
        final double[] imagS = new double[ (2 * M) + 1];
        final double[][] realE = new double[ (2 * M) + 1][M + 1];
        final double[][] imagE = new double[ (2 * M) + 1][M + 1];
        final double[][] realQ = new double[2 * M][M + 1];
        final double[][] imagQ = new double[2 * M][M + 1];

        final double[] realD = new double[ (2 * M) + 1];
        final double[] imagD = new double[ (2 * M) + 1];
        double[][] realA = null;
        double[][] imagA = null;
        double[][] realB = null;
        double[][] imagB = null;
        double[] realZ = null;
        double[] imagZ = null;
        double denom;
        double realProd;
        double imagProd;
        double realProd2;
        double imagProd2;
        int n;

        // The second index in a is for real = 0, imag = 1
        double[][] a = null;
        int r;
        int rq;
        double realH2M;
        double imagH2M;
        double realH2MSq;
        double imagH2MSq;
        double realR2Mz;
        double imagR2Mz;
        double realDiv;
        double imagDiv;
        double magnitude;
        double phase;
        double realRoot;
        double imagRoot;
        int timeIndex = 0;

        try {

            // Split up time vector in pieces of same order of magnitude,
            // invert one piece at a time. Simultaneous inversion for
            // times covering several orders of magnitude gives inaccurate
            // results for the small times.
            for (i = 0; i < tPts; i++) {
                logT[i] = 0.434294481903251 * Math.log(time[i]);

                if (logT[i] < minLogT) {
                    minLogT = logT[i];
                }

                if (logT[i] > maxLogT) {
                    maxLogT = logT[i];
                }
            } // for (i = 0; i < tPts; i++)

            iMinLogT = (int) Math.floor(minLogT);
            iMaxLogT = (int) Math.ceil(maxLogT);

            for (iLogT = iMinLogT; iLogT <= iMaxLogT; iLogT++) {
                segPoints = 0;

                for (i = 0; i < tPts; i++) {

                    if ( (logT[i] >= iLogT) && (logT[i] < (iLogT + 1))) {
                        segPoints++;
                    }
                } // for (i = 0; i < tPts; i++)

                if (segPoints > 0) {
                    segT = new double[segPoints];
                    T = -Double.MAX_VALUE;

                    for (i = 0, j = 0; i < tPts; i++) {

                        if ( (logT[i] >= iLogT) && (logT[i] < (iLogT + 1))) {
                            segT[j++] = time[i];

                            if (time[i] > T) {
                                T = time[i];
                            }
                        }
                    } // for (i = 0; i < tPts; i++)

                    T = 2.0 * T;
                    realS = largestPole - (Math.log(tol) / (2.0 * T));

                    for (i = 0; i < ( (2 * M) + 1); i++) {
                        imagS[i] = Math.PI * i / T;
                    }

                    // Call the Laplace function
                    // Get the 'a' coefficients in power series
                    a = fitToLaplace(realS, imagS);

                    // Zero term real part is halved
                    a[0][0] = a[0][0] / 2.0;

                    // Zero term imaginary part is halved
                    a[0][1] = a[0][1] / 2.0;

                    // Build up e and q columns
                    for (i = 0; i < ( (2 * M) + 1); i++) {

                        for (j = 0; j < (M + 1); j++) {
                            realE[i][j] = 0.0;
                            imagE[i][j] = 0.0;
                        }
                    }

                    for (i = 0; i < (2 * M); i++) {

                        for (j = 0; j < (M + 1); j++) {
                            realQ[i][j] = 0.0;
                            imagQ[i][j] = 0.0;
                        }
                    }

                    for (i = 1; i <= (2 * M); i++) {

                        // q[i-1][1] = a[i]/a[i-1];
                        denom = (a[i - 1][0] * a[i - 1][0]) + (a[i - 1][1] * a[i - 1][1]);
                        realQ[i - 1][1] = ( (a[i][0] * a[i - 1][0]) + (a[i][1] * a[i - 1][1])) / denom;
                        imagQ[i - 1][1] = ( (a[i][1] * a[i - 1][0]) - (a[i][0] * a[i - 1][1])) / denom;
                    }

                    for (r = 2; r <= (M + 1); r++) {

                        for (j = 0; j <= (2 * (M - r + 1)); j++) {
                            realE[j][r - 1] = realQ[j + 1][r - 1] - realQ[j][r - 1] + realE[j + 1][r - 2];
                            imagE[j][r - 1] = imagQ[j + 1][r - 1] - imagQ[j][r - 1] + imagE[j + 1][r - 2];
                        }

                        if (r < (M + 1)) {
                            rq = r + 1;

                            for (j = 0; j <= ( (2 * (M - rq + 1)) + 1); j++) {

                                // q[j][rq-1] = q[j+1][rq-2]*e[j+1][rq-2]/e[j][rq-2];
                                realProd = (realQ[j + 1][rq - 2] * realE[j + 1][rq - 2])
                                        - (imagQ[j + 1][rq - 2] * imagE[j + 1][rq - 2]);
                                imagProd = (realQ[j + 1][rq - 2] * imagE[j + 1][rq - 2])
                                        + (imagQ[j + 1][rq - 2] * realE[j + 1][rq - 2]);
                                denom = (realE[j][rq - 2] * realE[j][rq - 2]) + (imagE[j][rq - 2] * imagE[j][rq - 2]);
                                realQ[j][rq - 1] = ( (realProd * realE[j][rq - 2]) + (imagProd * imagE[j][rq - 2]))
                                        / denom;
                                imagQ[j][rq - 1] = ( (imagProd * realE[j][rq - 2]) - (realProd * imagE[j][rq - 2]))
                                        / denom;
                            } // for (j = 0; j <= 2*(M-rq+1)+1; j++)
                        } // if (r < (M+1))
                    } // for (r = 2; r <= M+1; r++)

                    // build up d vector
                    for (i = 0; i < ( (2 * M) + 1); i++) {
                        realD[i] = 0.0;
                        imagD[i] = 0.0;
                    }

                    realD[0] = a[0][0];
                    imagD[0] = a[0][1];

                    for (i = 1; i <= M; i++) {
                        realD[ (2 * i) - 1] = -realQ[0][i];
                        imagD[ (2 * i) - 1] = -imagQ[0][i];
                        realD[2 * i] = -realE[0][i];
                        imagD[2 * i] = -imagE[0][i];
                    }

                    // new does the required zeroing of A and B
                    realA = new double[ (2 * M) + 2][segPoints];
                    imagA = new double[ (2 * M) + 2][segPoints];
                    realB = new double[ (2 * M) + 2][segPoints];
                    imagB = new double[ (2 * M) + 2][segPoints];
                    realZ = new double[segPoints];
                    imagZ = new double[segPoints];

                    for (i = 0; i < segPoints; i++) {
                        realA[1][i] = realD[0];
                        imagA[1][i] = imagD[0];
                        realB[0][i] = 1.0;
                        imagB[0][i] = 0.0;
                        realB[1][i] = 1.0;
                        imagB[1][i] = 0.0;
                        realZ[i] = Math.cos(Math.PI * segT[i] / T);
                        imagZ[i] = Math.sin(Math.PI * segT[i] / T);
                    }

                    for (n = 2; n <= ( (2 * M) + 1); n++) {

                        for (j = 0; j < segPoints; j++) {
                            realProd = (realD[n - 1] * realZ[j]) - (imagD[n - 1] * imagZ[j]);
                            imagProd = (realD[n - 1] * imagZ[j]) + (imagD[n - 1] * realZ[j]);
                            realProd2 = (realProd * realA[n - 2][j]) - (imagProd * imagA[n - 2][j]);
                            imagProd2 = (realProd * imagA[n - 2][j]) + (imagProd * realA[n - 2][j]);
                            realA[n][j] = realA[n - 1][j] + realProd2;
                            imagA[n][j] = imagA[n - 1][j] + imagProd2;
                            realProd2 = (realProd * realB[n - 2][j]) - (imagProd * imagB[n - 2][j]);
                            imagProd2 = (realProd * imagB[n - 2][j]) + (imagProd * realB[n - 2][j]);
                            realB[n][j] = realB[n - 1][j] + realProd2;
                            imagB[n][j] = imagB[n - 1][j] + imagProd2;
                        }
                    } // for (n = 2; n <= 2*M+1; n++)

                    // double acceleration
                    for (i = 0; i < segPoints; i++) {
                        realH2M = .5 * (1.0 + ( (realD[ (2 * M) - 1] - realD[2 * M]) * realZ[i]) - ( (imagD[ (2 * M) - 1] - imagD[2 * M]) * imagZ[i]));
                        imagH2M = .5 * ( ( (realD[ (2 * M) - 1] - realD[2 * M]) * imagZ[i]) + ( (imagD[ (2 * M) - 1] - imagD[2 * M]) * realZ[i]));

                        // Square each element of h2M
                        realH2MSq = (realH2M * realH2M) - (imagH2M * imagH2M);
                        imagH2MSq = 2.0 * realH2M * imagH2M;

                        // z/(h2M).^2
                        denom = (realH2MSq * realH2MSq) + (imagH2MSq * imagH2MSq);
                        realDiv = ( (realZ[i] * realH2MSq) + (imagZ[i] * imagH2MSq)) / denom;
                        imagDiv = ( (imagZ[i] * realH2MSq) - (realZ[i] * imagH2MSq)) / denom;
                        realProd = 1.0 + (realD[2 * M] * realDiv) - (imagD[2 * M] * imagDiv);
                        imagProd = (realD[2 * M] * imagDiv) + (imagD[2 * M] * realDiv);
                        magnitude = Math.sqrt( (realProd * realProd) + (imagProd * imagProd));
                        phase = Math.atan2(imagProd, realProd);
                        realRoot = Math.sqrt(magnitude) * Math.cos(0.5 * phase);
                        imagRoot = Math.sqrt(magnitude) * Math.sin(0.5 * phase);
                        realR2Mz = ( -realH2M * (1.0 - realRoot)) - (imagH2M * imagRoot);
                        imagR2Mz = ( -imagH2M * (1.0 - realRoot)) + (realH2M * imagRoot);
                        realA[ (2 * M) + 1][i] = realA[2 * M][i] + (realR2Mz * realA[ (2 * M) - 1][i])
                                - (imagR2Mz * imagA[ (2 * M) - 1][i]);
                        imagA[ (2 * M) + 1][i] = imagA[2 * M][i] + (realR2Mz * imagA[ (2 * M) - 1][i])
                                + (imagR2Mz * realA[ (2 * M) - 1][i]);
                        realB[ (2 * M) + 1][i] = realB[2 * M][i] + (realR2Mz * realB[ (2 * M) - 1][i])
                                - (imagR2Mz * imagB[ (2 * M) - 1][i]);
                        imagB[ (2 * M) + 1][i] = imagB[2 * M][i] + (realR2Mz * imagB[ (2 * M) - 1][i])
                                + (imagR2Mz * realB[ (2 * M) - 1][i]);
                        denom = (realB[ (2 * M) + 1][i] * realB[ (2 * M) + 1][i])
                                + (imagB[ (2 * M) + 1][i] * imagB[ (2 * M) + 1][i]);
                        realDiv = ( (realA[ (2 * M) + 1][i] * realB[ (2 * M) + 1][i]) + (imagA[ (2 * M) + 1][i] * imagB[ (2 * M) + 1][i]))
                                / denom;
                        timeFunction[timeIndex++] = Math.exp(realS * segT[i]) * realDiv / T;
                    } // for (i = 0; i < segPoints; i++)
                } // if (segPoints > 0)
            } // for (iLogT = iMinLogT; iLogT <= iMaxLogT; iLogT++)
        } // try
        catch (final Exception err) {
            Preferences.debug("driver error: " + err.getMessage(), Preferences.DEBUG_ALGORITHM);
        }
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
