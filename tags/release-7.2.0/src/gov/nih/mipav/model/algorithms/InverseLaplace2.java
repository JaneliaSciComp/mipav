package gov.nih.mipav.model.algorithms;


import gov.nih.mipav.view.MipavUtil;


/**
 * This is a port of the numerical inverse Laplace transform found as the FORTRAN implementation of Algorithm 619 from
 * the collected algorithms from the ACM found at http://www.netlib.org/toms/619. Ths inversion of the Laplace transform
 * is done using the Durbin formula in combination with the Epsilon Algorithm. This algorithm by R. Piessens and R.
 * Huysmans appeared in ACM Trans. Math. Software, Vol. 10, No. 3, Sep., 1984, pp. 348-353.
 * 
 * <hr>
 * 
 * <p>
 * From <a href="http://www.acm.org/publications/policies/softwarecrnotice/">the ACM website</a>:
 * 
 * <pre>
 * ACM Software License Agreement
 * 
 *  All software, both binary and source published by the Association for Computing Machinery (hereafter, Software) is copyrighted by the Association (hereafter, ACM) and ownership of all right, title and interest in and to the Software remains with ACM. By using or copying the Software, User agrees to abide by the terms of this Agreement.
 *  Noncommercial Use
 * 
 *  The ACM grants to you (hereafter, User) a royalty-free, nonexclusive right to execute, copy, modify and distribute both the binary and source code solely for academic, research and other similar noncommercial uses, subject to the following conditions:
 * 
 *  1. User acknowledges that the Software is still in the development stage and that it is being supplied &quot;as is,&quot; without any support services from ACM. Neither ACM nor the author makes any representations or warranties, express or implied, including, without limitation, any representations or warranties of the merchantability or fitness for any particular purpose, or that the application of the software, will not infringe on any patents or other proprietary rights of others.
 *  2. ACM shall not be held liable for direct, indirect, incidental or consequential damages arising from any claim by User or any third party with respect to uses allowed under this Agreement, or from any use of the Software.
 *  3. User agrees to fully indemnify and hold harmless ACM and/or the author(s) of the original work from and against any and all claims, demands, suits, losses, damages, costs and expenses arising out of the User's use of the Software, including, without limitation, arising out of the User's modification of the Software.
 *  4. User may modify the Software and distribute that modified work to third parties provided that: (a) if posted separately, it clearly acknowledges that it contains material copyrighted by ACM (b) no charge is associated with such copies, (c) User agrees to notify ACM and the Author(s) of the distribution, and (d) User clearly notifies secondary users that such modified work is not the original Software.
 *  5. User agrees that ACM, the authors of the original work and others may enjoy a royalty-free, non-exclusive license to use, copy, modify and redistribute these modifications to the Software made by the User and distributed to third parties as a derivative work under this agreement.
 *  6. This agreement will terminate immediately upon User's breach of, or non-compliance with, any of its terms. User may be held liable for any copyright infringement or the infringement of any other proprietary rights in the Software that is caused or facilitated by the User's failure to abide by the terms of this agreement.
 *  7. This agreement will be construed and enforced in accordance with the law of the state of New York applicable to contracts performed entirely within the State. The parties irrevocably consent to the exclusive jurisdiction of the state or federal courts located in the City of New York for all disputes concerning this agreement. 
 * 
 *  Commercial Use
 * 
 *  Any User wishing to make a commercial use of the Software must contact ACM at permissions@acm.org to arrange an appropriate license. Commercial use includes (1) integrating or incorporating all or part of the source code into a product for sale or license by, or on behalf of, User to third parties, or (2) distribution of the binary or source code to third parties for use with a commercial product sold or licensed by, or on behalf of, User.
 * 
 *  Revised 6/98
 * </pre>
 * 
 * </p>
 */

// An example of sin(t) with the Laplace 1/(s**2 + 1) is given below.
/*
 * private void runLapTest2() { // This routine tests the numerical inverese Laplace transform of a // function in
 * InverseLaplace2 with an analytically known inverse. The // function to be transformed is sin(t) double time[] = new
 * double[] {0.1,1.0,2.0,3.0,4.0,5.0,1.0E1,2.0E1,3.0E1, 4.0E1,5.0E1,6.0E1,7.0E1,8.0E1,9.0E1,1.0E2}; double abscissa =
 * 0.0; double relEps = 1.0E-12; double absEps = 1.0E-12; double result[] = new double[time.length]; double estErr[] =
 * new double[time.length]; int evaluations[] = new int[time.length]; int errStatus[] = new int[time.length];
 * FitSineModel smod; int i;
 * 
 * smod = new FitSineModel(time,abscissa,relEps,absEps,result,estErr, evaluations,errStatus); smod.driver();
 * 
 * for (i = 0; i < time.length; i++) { if (errStatus[i] == 2) { Preferences.debug("time[" + i +
 * "] is illegally <= 0\n"); } else if (errStatus[i] == 1) { Preferences.debug("time[" + i +
 * "] had computations terminated\n"); Preferences.debug("The maximum bound on Laplace evaluations was reached\n"); }
 * else { Preferences.debug("time = " + time[i] + " routineFunction = " + result[i] + " trueFunction = " +
 * Math.sin(time[i]) + "\n");
 * 
 * } } }
 */

/*
 * class FitSineModel extends InverseLaplace2 {
 * 
 * public FitSineModel(double time[], double abscissa, double relEps, double absEps, double[] result, double [] estErr,
 * int evaluations[], int errStatus[]) { super(time, abscissa, relEps, absEps, result, estErr, evaluations, errStatus);
 * 
 * }
 * 
 * public void fitToLaplace(double realIn, double imagIn, double realOut[], double imagOut[]) { // 1/(s2 + 1) is the
 * Laplace transform of sin(t) double c, d; c = realInrealIn - imagInimagIn + 1.0; d = cc + 4.0realInrealInimagInimagIn;
 * realOut[0] = c/d; imagOut[0] = -2.0realInimagIn/d; return; } }
 */

public abstract class InverseLaplace2 {

    // ~ Instance fields
    // ------------------------------------------------------------------------------------------------

    /** abscissa of convergence of the Laplace transform. */
    private final double abscissa;

    /** absolute accuracy requested. */
    private final double absEps;

    /** D1MACH(4). */
    private double epsilon;

    /**
     * Parameter giving information on the termination of the algorithm errStatus[0] = 0 normal and reliable termination
     * of the routine errStatus[0] = 1 The computations are terminated because the bound on the number of evaluations of
     * the user supplied Laplace function has been achieved. This bound is equal to 8*maxBound+ 5. One can allow more
     * Laplace function evalutions by increasing the value of maxBound. errStatus[0] = 2 An input time value is less
     * than or equal to zero
     */
    private final int[] errStatus;

    /** Estimate of the absolute error abs(f(t) - result[0]). */
    private final double[] estErr;

    /** Number of evaluations of user supplied Laplace function. */
    private final int[] evaluations;

    /** index of the time variable being used. */
    private int it;

    /** maxBound is a bound on the number of terms used in the Durbin formula. */
    private final int maxBound = 20000;

    /** DOCUMENT ME! */
    private int nex;

    /** DOCUMENT ME! */
    private int nres;

    /**
     * emax = 1024, the largest exponent E for double precision, is I1MACH(16) D1MACH(2) = 2**(emax)*(1 -
     * 2**(-doubleDigits)) = 2**1024*(1 - 2**-53) D1MACH(2) = Double.MAX_VALUE.
     */
    private final double oflow = Double.MAX_VALUE;

    /** relative accuracy requested. */
    private final double relEps;

    /** DOCUMENT ME! */
    private final double[] res3la = new double[3];

    /**
     * The routine tries to satisfy the least stringent of both accuracy requirements. That is, the routine tries to
     * satisfy: ABS(f(t) - result[0]) <= max(absEps, relEps*ABS(f(t))) where f(t) is the actual time function Inverse
     * Laplace transform
     */
    private final double[] result;

    /** DOCUMENT ME! */
    private final double[] rex = new double[52];

    /**
     * Independent variable for which the inverse Laplace transform has to be computed. time value should be greater
     * than zero.
     */
    private final double[] time;

    // ~ Constructors
    // ---------------------------------------------------------------------------------------------------

    /**
     * Creates a new InverseLaplace2 object.
     * 
     * @param time double[]
     * @param abscissa double
     * @param relEps double
     * @param absEps double
     * @param result double[]
     * @param estErr double[]
     * @param evaluations int[]
     * @param errStatus int[]
     */
    public InverseLaplace2(final double[] time, final double abscissa, final double relEps, final double absEps,
            final double[] result, final double[] estErr, final int[] evaluations, final int[] errStatus) {
        this.time = time;
        this.abscissa = abscissa;
        this.relEps = relEps;
        this.absEps = absEps;
        this.result = result;
        this.estErr = estErr;
        this.evaluations = evaluations;
        this.errStatus = errStatus;
    }

    // ~ Methods
    // --------------------------------------------------------------------------------------------------------

    /**
     * DOCUMENT ME!
     * 
     * @param realIn double real part of independent variable of Laplace transform
     * @param imagIn double imag part of independent varialbe of Laplace transform
     * @param realOut double[] real part of Laplace transform output
     * @param imagOut double[] imag part of Laplace transform output
     */
    public abstract void fitToLaplace(double realIn, double imagIn, double[] realOut, double[] imagOut);

    /**
     * driver.
     */
    public void driver() {

        // The array si contains the values of the sine and cosine functions
        // required in the Durbin formula.
        final double[] si = new double[32];
        int tPts;
        final double pid16 = Math.PI / 16.0;
        double ak;
        int k;
        int kk;
        double t;
        double arg;
        double are;
        double aim;
        double bb;
        final double[] fre = new double[1];
        final double[] fim = new double[1];
        double r;
        int kc;
        int ks;
        int i;
        int m;
        double neweps;

        tPts = time.length;

        if (result.length != tPts) {
            MipavUtil.displayError("time.length = " + time.length + " but result.length = " + result.length);

            return;
        }

        if (estErr.length != tPts) {
            MipavUtil.displayError("time.length = " + time.length + " but estErr.length = " + estErr.length);

            return;
        }

        if (evaluations.length != tPts) {
            MipavUtil.displayError("time.length = " + time.length + " but evaluations.length = " + evaluations.length);

            return;
        }

        if (errStatus.length != tPts) {
            MipavUtil.displayError("time.length = " + time.length + " but errStatus.length = " + errStatus.length);

            return;
        }

        // epsilon = D1MACH(4)
        // Machine epsilon is the smallest positive epsilon such that
        // (1.0 + epsilon) != 1.0.
        // epsilon = 2**(1 - doubleDigits) = 2**(1 - 53) = 2**(-52)
        // epsilon = 2.224460e-16
        // epsilon is called the largest relative spacing
        epsilon = 1.0;
        neweps = 1.0;

        while (true) {

            if (1.0 == (1.0 + neweps)) {
                break;
            } else {
                epsilon = neweps;
                neweps = neweps / 2.0;
            }
        }

        // Compute elements of si
        si[7] = 1.0;
        si[15] = 0.0;
        ak = 1.0;

        for (k = 1; k <= 7; k++) {
            si[k - 1] = Math.sin(ak * pid16);
            ak = ak + 1.0;
            kk = 16 - k;
            si[kk - 1] = si[k - 1];
        } // for (k = 1; k <= 7; k++)

        for (k = 17; k <= 32; k++) {
            si[k - 1] = -si[k - 17];
        } // for (k = 17; k <= 32; k++)

        group: for (it = 0; it < tPts; it++) {
            t = time[it];

            if (t <= 0.0) {
                errStatus[it] = 2;
                result[it] = 0.0;
                estErr[it] = 1.0;
                evaluations[it] = 0;

                continue;
            } // if (t <= 0.0)

            errStatus[it] = 0;
            nres = 0;

            // Initialization of the summation of the Durbin formula
            arg = pid16 / t;
            are = abscissa + (2.0 / t);
            aim = 0.0;
            bb = Math.exp(are * t) / (16.0 * t);
            fitToLaplace(are, aim, fre, fim);
            evaluations[it] = 5;
            r = 0.5 * fre[0];
            nex = 0;
            kc = 8;
            ks = 0;

            // Main loop for the summation

            for (i = 1; i <= maxBound; i++) {
                m = 8;

                if (i == 1) {
                    m = 12;
                } // if (i == 1)

                for (k = 1; k <= m; k++) {
                    aim = aim + arg;
                    kc++;
                    ks++;

                    if (kc > 32) {
                        kc = 1;
                    } // if (kc > 32)

                    if (ks > 32) {
                        ks = 1;
                    } // if (ks > 32)

                    fitToLaplace(are, aim, fre, fim);
                    r = r + (fre[0] * si[kc - 1]) - (fim[0] * si[ks - 1]);
                } // for (k = 1; k <= m; k++)

                evaluations[it] = evaluations[it] + 8;
                nex++;
                rex[nex - 1] = r;

                // Extrapolation using the Epsilon algorithm
                if (nex >= 3) {
                    dqext();
                } // if (nex >= 3)

                if (nres >= 4) {

                    // Computation of intermediate result and estimate of the absolute
                    // error
                    result[it] = result[it] * bb;
                    estErr[it] = estErr[it] * bb;

                    if ( (estErr[it] < Math.max(absEps, relEps * Math.abs(result[it])))
                            && (Math.abs( (r * bb) - result[it]) < (0.5 * Math.abs(result[it])))) {
                        continue group;
                    }
                } // if (nres >= 4)
            } // for (i = 1; i <= maxBound; i++)

            // Set error flag in the case that the number of terms in the summation
            // is equal to maxBound
            errStatus[it] = 1;
        } // for (it = 0; it < tPts; it++)

        return;

    } // driver

    /**
     * DOCUMENT ME!
     */
    private void dqext() {
        // Epsilon algorithm
        // The routine determines the limit of a given sequence of approximations,
        // by means of the Epsilon algorithm of P. Wynn
        // An estimate of the absolute error is also given. The condensed Epsilon
        // table is computed. Only those elements needed for the computation of
        // the next diagonal are preserved.

        // Parameters
        // nex rex[nex] contains the new element in the first column of the
        // Epsilon table
        // rex 52 element vector containing the elements of the two lower
        // diagonals of the triangular Epsilon table. The elements are
        // numbered starting at the right-hand corner of the triangle
        // result Resulting approximation to the integral
        // estErr Estimate of the absolute error computed from the result and
        // the 3 previous results
        // res3la Vector of dimension 3 containing the last 3 results
        // nres Number of calls to the routine (should be zero at first call)
        // e0, e1, e2, e3 the 4 elements on which the computation of a new element
        // in the Epsilon table is based
        // newelm Number of elements to be computed in the new diagonal
        // error - error = ABS(e1-e0) + ABS(e2-e1) + ABS(new-e2)
        // result The element in the new diagonal with least value of error
        // epsilon The largest relative spacing.
        // oflow The largest positive magnitude.
        // limexp The maximum number of elements the Epsilon table can contain.
        // If this number is reached, the upper diagonal of the Epsilon
        // table is deleted.
        int limexp;
        int newelm;
        int num;
        int k1;
        int i;
        int k2;
        int k3;
        double res;
        double e0;
        double e1;
        double e2;
        double e3;
        double e1abs;
        double delta2;
        double err2;
        double tol2;
        double delta3;
        double err3;
        double tol3;
        double delta1;
        double err1;
        double tol1;
        double ss;
        double epsinf;
        double error;
        int ib;
        int ie;
        int ib2;
        int indx;

        nres++;
        estErr[it] = oflow;
        result[it] = rex[nex - 1];

        if (nex < 3) {
            estErr[it] = Math.max(estErr[it], 5.0 * epsilon * Math.abs(result[it]));

            return;
        } // if (nex < 3)

        limexp = 50;
        rex[nex + 1] = rex[nex - 1];
        newelm = (nex - 1) / 2;
        rex[nex - 1] = oflow;
        num = nex;
        k1 = nex;

        for (i = 1; i <= newelm; i++) {
            k2 = k1 - 1;
            k3 = k1 - 2;
            res = rex[k1 + 1];
            e0 = rex[k3 - 1];
            e1 = rex[k2 - 1];
            e2 = res;
            e1abs = Math.abs(e1);
            delta2 = e2 - e1;
            err2 = Math.abs(delta2);
            tol2 = Math.max(Math.abs(e2), e1abs) * epsilon;
            delta3 = e1 - e0;
            err3 = Math.abs(delta3);
            tol3 = Math.max(e1abs, Math.abs(e0)) * epsilon;

            if ( (err2 <= tol2) && (err3 <= tol3)) {

                // If e0, e1, and e2 are equal to within machine accuracy,
                // convergence is assumed.
                result[it] = res;
                estErr[it] = err2 + err3;
                estErr[it] = Math.max(estErr[it], 5.0 * epsilon * Math.abs(result[it]));

                return;
            } // if ((err2 <= tol2) && (err3 <= tol3))

            e3 = rex[k1 - 1];
            rex[k1 - 1] = e1;
            delta1 = e1 - e3;
            err1 = Math.abs(delta1);
            tol1 = Math.max(e1abs, Math.abs(e3)) * epsilon;

            // If two elements are very close to each other, omit a part of the
            // table by adjusting the value of nex
            if ( (err1 <= tol1) || (err2 <= tol2) || (err3 <= tol3)) {
                nex = i + i - 1;

                break;
            } // if ((err1 <= tol1) || (err2 <= tol2) || (err3 <= tol3))

            ss = (1.0 / delta1) + (1.0 / delta2) - (1.0 / delta3);
            epsinf = Math.abs(ss * e1);

            // Test to detect irregular behavior in the table, and eventually to
            // omit a part of the table adjusting the value of nex
            if (epsinf <= 1.0E-4) {
                nex = i + i - 1;

                break;
            } // if (epsinf <= 1.0E-4)

            // Compute a new element and eventually adjust the value of the result
            res = e1 + (1.0 / ss);
            rex[k1 - 1] = res;
            k1 = k1 - 2;
            error = err2 + Math.abs(res - e2) + err3;

            if (error <= estErr[it]) {
                estErr[it] = error;
                result[it] = res;
            } // if (error <= estErr[it])
        } // for (i = 1; i <= newelm; i++)

        // Shift the table
        if (nex == limexp) {
            nex = (2 * (limexp / 2)) - 1;
        } // if (nex == limexp)

        ib = 1;

        if ( ( (num / 2) * 2) == num) {
            ib = 2;
        } // if ((num/2)*2 == num)

        ie = newelm + 1;

        for (i = 1; i <= ie; i++) {
            ib2 = ib + 2;
            rex[ib - 1] = rex[ib2 - 1];
            ib = ib2;
        } // for (i = 1; i <= ie; i++)

        if (num != nex) {
            indx = num - nex + 1;

            for (i = 1; i <= nex; i++) {
                rex[i - 1] = rex[indx - 1];
                indx++;
            } // for (i = 1; i <= nex; i++)
        } // if (num != nex)

        if (nres < 4) {
            res3la[nres - 1] = result[it];
            estErr[it] = oflow;
            estErr[it] = Math.max(estErr[it], 5.0 * epsilon * Math.abs(result[it]));

            return;
        } // if (nres < 4)

        // Compute error estimate
        estErr[it] = Math.abs(result[it] - res3la[2]) + Math.abs(result[it] - res3la[1])
                + Math.abs(result[it] - res3la[0]);
        res3la[0] = res3la[1];
        res3la[1] = res3la[2];
        res3la[2] = result[it];
        estErr[it] = Math.max(estErr[it], 5.0 * epsilon * Math.abs(result[it]));

        return;
    }
}
