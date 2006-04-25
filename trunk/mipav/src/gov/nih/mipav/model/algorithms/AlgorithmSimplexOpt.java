package gov.nih.mipav.model.algorithms;


import gov.nih.mipav.view.*;


/**
 * Downhill Simplex Method, (Nelder and Mead) Numerical Recipes in C
 *
 * @version  0.1 May 4, 1998
 * @author   Matthew J. McAuliffe, Ph.D. March 2000, Delia McGarry
 */

public class AlgorithmSimplexOpt extends AlgorithmBase {

    //~ Static fields/initializers -------------------------------------------------------------------------------------

    /** DOCUMENT ME! */
    public static final int MAX_ITER = 5000;

    /** DOCUMENT ME! */
    public static final double TOL = (double) 0.0000001;

    //~ Instance fields ------------------------------------------------------------------------------------------------


    /** DOCUMENT ME! */
    private int nDim;

    /** DOCUMENT ME! */
    private int nPts;

    /** DOCUMENT ME! */
    private AlgorithmOptimizeFunctionBase optFunction;

    /** DOCUMENT ME! */
    private double[][] p;

    /** DOCUMENT ME! */
    private double[] pSum;

    /** DOCUMENT ME! */
    private double[] y;

    //~ Constructors ---------------------------------------------------------------------------------------------------

    /**
     * Creates a new AlgorithmSimplexOpt object.
     *
     * @param  p           DOCUMENT ME!
     * @param  functEvals  DOCUMENT ME!
     * @param  nDim        DOCUMENT ME!
     * @param  func        DOCUMENT ME!
     */
    public AlgorithmSimplexOpt(double[][] p, double[] functEvals, int nDim, AlgorithmOptimizeFunctionBase func) {

        this.nDim = nDim;
        nPts = nDim + 1;
        pSum = new double[nPts];
        y = functEvals;
        this.p = p;
        optFunction = func;
    }

    //~ Methods --------------------------------------------------------------------------------------------------------

    /**
     * DOCUMENT ME!
     */
    public void runAlgorithm() {
        Preferences.debug("Calling ameoba...\n");
        ameoba();
        Preferences.debug("Ameoba finished.\n");
    }

    /**
     * DOCUMENT ME!
     *
     * @param  p  DOCUMENT ME!
     */
    public void setP(double[][] p) {
        this.p = p;
    }

    /**
     * DOCUMENT ME!
     *
     * @param  y  DOCUMENT ME!
     */
    public void setY(double[] y) {
        this.y = y;
    }

    /**
     * ameoba.
     */
    private void ameoba() {
        int i, j;
        int ihi, ilo, inhi;
        double ySave, yTry;
        double rtol;
        int nFunctionCalls = 0;
        double denom;

        get_pSum();

        while (true) {
            ilo = 0;

            // Determine which pt is highest (worst), next-highest, and lowest (best) by
            // looping over pts in the simplex.
            if (y[0] > y[1]) { // convert ihi = y[0] > y[1] ? (inhi=1,0) : (inhi=0,1);
                ihi = 0;
                inhi = 1;
            } else {
                ihi = 1;
                inhi = 0;
            } // end of conversion

            for (i = 0; i < nPts; i++) {

                if (y[i] <= y[ilo]) {
                    ilo = i;
                }

                if (y[i] > y[ihi]) {
                    inhi = ihi;
                    ihi = i;
                } else if ((y[i] > y[inhi]) && (i != ihi)) {
                    inhi = i;
                }
            }

            denom = (double) (Math.abs(y[ihi]) + Math.abs(y[ilo]));

            if (denom <= 0.0001) { // prevent divide by zero
                denom = 0.0001;
            }

            rtol = (double) 2.0 * Math.abs(y[ihi] - y[ilo]) / denom;
            // rtol = (double)1000.0*(double)2.0*Math.abs(y[ihi] - y[ilo]) /denom;

            // Compute fractional range from highest to lowest and return if satisfactory.
            if (rtol < TOL) { // If returning, put best pt and value in slot 1.

                // if (rtol < (1000*TOL)) {//If returning, put best pt and value in slot 1.
                swap(y[0], y[ilo]);

                for (i = 0; i < nDim; i++) {
                    swap(p[0][i], p[ilo][i]);
                }

                Preferences.debug("Breaking out of amoeba b/c rtol = " + rtol + "\n");
                Preferences.debug("nFunctionCalls = " + nFunctionCalls + "\n");

                break;
            }

            if (nFunctionCalls >= MAX_ITER) {
                displayError("AlgorithmSimplexOpt: MAX_ITER exceeded");
            }

            nFunctionCalls += 2;

            // Begin new iteration. First extrapolate by factor -1 through face of
            // simplex across from high point, i.e., reflect simplex from high pt.
            yTry = amotry(ihi, (double) -1.0);

            if (yTry <= y[ilo]) { // Gives better result than best pt, so try an addition
                yTry = amotry(ihi, (double) 2.0); // extrapolation by factor 2.
            } else if (yTry >= y[inhi]) { // reflected pt is worse than 2nd-highest,
                ySave = y[ihi]; // so look for intermediate lower pt
                yTry = amotry(ihi, (double) 0.5); // by 1D contraction.

                if (yTry >= ySave) { // Havent gotten rid of high pt, so

                    for (i = 0; i < nPts; i++) { // contract around lowest pt.

                        if (i != ilo) {

                            for (j = 0; j < nDim; j++) {
                                p[i][j] = (double) (0.5 * (p[i][j] + p[ilo][j]));
                                pSum[j] = p[i][j];
                            }

                            y[i] = optFunction.cost(pSum);
                        }
                    }

                    nFunctionCalls += nDim;
                    get_pSum();
                }
            } else {
                --nFunctionCalls; // Correct the evaluation count.
            }
        }
    }

    /**
     * amotry extrapolates by factor fac through face of simplex across from high point, tries it, and replaces high
     * point if new point is better.
     *
     * @param   ihi  DOCUMENT ME!
     * @param   fac  DOCUMENT ME!
     *
     * @return  DOCUMENT ME!
     */
    private double amotry(int ihi, double fac) {

        int j;
        double yTry;
        double fac1, fac2;
        double[] pTry = new double[nPts];
        fac1 = (double) (1.0 - fac) / nDim;
        fac2 = fac1 - fac;

        for (j = 0; j < nDim; j++) {
            pTry[j] = (pSum[j] * fac1) - (p[ihi][j] * fac2);
        }

        yTry = optFunction.cost(pTry); // Evaluate function at trial point

        if (yTry < y[ihi]) { // If its better than highest,
            y[ihi] = yTry; // then replace highest

            for (j = 0; j < nDim; j++) {
                pSum[j] += pTry[j] - p[ihi][j];
                p[ihi][j] = pTry[j];
            }
        }

        return yTry;
    }

    /**
     * DOCUMENT ME!
     */
    private void get_pSum() {
        int i, j;
        double sum;

        for (j = 0; j < nDim; j++) {
            sum = (double) 0.0;

            for (i = 0; i < nPts; i++) {
                sum += p[i][j];
            }

            pSum[j] = sum;
        }
    }

    /**
     * DOCUMENT ME!
     *
     * @param  a  DOCUMENT ME!
     * @param  b  DOCUMENT ME!
     */
    private void swap(double a, double b) {
        double temp;

        temp = a;
        a = b;
        b = temp;
    }

}
