package gov.nih.mipav.model.algorithms;


import gov.nih.mipav.view.*;


/**
 * This is a port of code from the companion website to the book Data Reduction and Error Analysis for the Physical
 * Sciences Third Edition by Philip R. Bevington and D. Keith Robinson, McGraw-Hill, 2003. The code covers Chapter 8
 * Least-Squares Fit to an Arbitrary Function. The website is www.mhhe.com/bevington.
 */

/*Sample usage folows:
 * class FitReferenceModel extends NLEngine2 {
 *
 * public FitReferenceModel(int nPoints, double xData[], float yData[],                          double initial[]) {   //
 * nPoints data points, 3 coefficients, and exponential fitting   super(nPoints,3,NLEngine2.GRADIENT_SEARCH);   int i;
 *
 * for (i = 0; i < nPoints; i++) {       xseries[i]  = xData[i];       yseries[i]  = (double)yData[i];   }
 *
 * // Assign the same standard deviation to all data points   stdv = 1;   for (i = 0; i < nPoints; i++) {       sig[i] =
 * stdv;   }
 *
 * gues[0] = initial[0];   gues[1] = initial[1];   gues[2] = initial[2];
 *
 *} */

/**
 * Fit to function - a0 + a1*exp(a2*x).
 *
 * @param   x1    The x value of the data point.
 * @param   atry  The best guess parameter values.
 * @param   dyda  The derivative values of y with respect to fitting parameters.
 *
 * @return  The calculated y value.
 */
/*public double fitToFunction(double x1, double atry[], double dyda[]) {
 *  // mrqcof calls function // mrqcof supplies x1 and best guess parameters atry[] // function returns the partial
 * derivatives dyda and the calculated ymod double ymod = 0;           try {               ymod = atry[0] +
 * atry[1]*Math.exp(atry[2] * x1);                 dyda[0] = 1;     // a0 partial derivative                 dyda[1] =
 * Math.exp(atry[2] * x1); // a1 partial derivative                 // a2 partial derivative                 dyda[2] =
 * atry[1]*atry[2]*Math.exp(atry[2] * x1);         }         catch (Exception e) {
 * Preferences.debug("function error: " + e.getMessage() + "\n");         }         return ymod;}*/


/**
 * Starts the analysis.
 */
/*public void driver() {
 *  super.driver();}*/


/**
 * Display results of displaying exponential fitting parameters.
 */
/*public void dumpResults() {
 *  Preferences.debug(" ******* FitReferenceExponential ********* \n\n"); Preferences.debug("Number of iterations: " +
 * String.valueOf(kk) + "\n"); Preferences.debug("Chi-squared: " + String.valueOf(chisq) + "\n"); Preferences.debug("a0
 * " + String.valueOf(a[0]) + "\n");// + " +/- " + String.valueOf(Math.sqrt(covar[0][0]))); Preferences.debug("a1 " +
 * String.valueOf(a[1]) + "\n");// + " +/- " + String.valueOf(Math.sqrt(covar[1][1]))); Preferences.debug("a2 " +
 * String.valueOf(a[2]) + "\n"); }}*/


/*class FitDoubleExponentialModel extends NLEngine2 {

    public FitDoubleExponentialModel(int nPoints, double xData[], float yData[],
                               double initial[]) {
        // nPoints data points, 3 coefficients, and exponential fitting
        super(nPoints,3,NLEngine2.GRADIENT_SEARCH);
        int i;

        for (i = 0; i < nPoints; i++) {
            xseries[i]  = xData[i];
            yseries[i]  = (double)yData[i];
        }

        // Assign the same standard deviation to all data points
        stdv = 1;
        for (i = 0; i < nPoints; i++) {
            sig[i] = stdv;
        }

        gues[0] = initial[0];
        gues[1] = initial[1];
        gues[2] = initial[2];

    }*/

/**
 * Fit to function - 1 - ao*exp(a1*x) - (1 - a0)*exp(a2*x).
 *
 * @param   x1    The x value of the data point.
 * @param   atry  The best guess parameter values.
 * @param   dyda  The derivative values of y with respect to fitting parameters.
 *
 * @return  The calculated y value.
 */
/*public double fitToFunction(double x1, double atry[], double dyda[]) {
 *  // mrqcof calls function // mrqcof supplies x1 and best guess parameters atry[] // function returns the partial
 * derivatives dyda and the calculated ymod double ymod = 0;           try {               ymod = 1 -
 * atry[0]*Math.exp(atry[1]*x1)                        - (1 - atry[0])*Math.exp(atry[2] * x1);               // a0
 * partial derivative                 dyda[0] = -Math.exp(atry[1]*x1) + Math.exp(atry[2]*x1);                 // a1
 * partial derivative                 dyda[1] = -atry[0]*atry[1]*Math.exp(atry[1] * x1);                 // a2 partial
 * derivative                 dyda[2] = -(1 - atry[0])*atry[2]*Math.exp(atry[2] * x1);         }         catch
 * (Exception e) {                 Preferences.debug("function error: " + e.getMessage() + "\n");         }
 * return ymod;}*/


/**
 * Starts the analysis.
 */
/* public void driver() {
 *   super.driver(); }*/


/**
 * Display results of displaying exponential fitting parameters.
 */
/* public void dumpResults() {
 *   Preferences.debug(" ******* FitDoubleExponential ********* \n\n");  Preferences.debug("Number of iterations: " +
 * String.valueOf(kk) + "\n");  Preferences.debug("Chi-squared: " + String.valueOf(chisq) + "\n");
 * Preferences.debug("a0 " + String.valueOf(a[0]) +                    " +- " + String.valueOf(siga[0]) + "\n");
 * Preferences.debug("a1 " + String.valueOf(a[1]) +                    " +- " + String.valueOf(siga[1]) + "\n");
 * Preferences.debug("a2 " + String.valueOf(a[2]) +                    " +- " + String.valueOf(siga[2]) + "\n"); }}*/


public abstract class NLEngine2 {

    //~ Static fields/initializers -------------------------------------------------------------------------------------


    /** DOCUMENT ME! */
    public static final int GRID_SEARCH = 1;

    /** DOCUMENT ME! */
    public static final int GRADIENT_SEARCH = 2;

    /** DOCUMENT ME! */
    public static final int CHISQ_EXPANSION = 3;

    /** DOCUMENT ME! */
    public static final int MARQUARDT = 4;

    /** DOCUMENT ME! */
    protected static double[] beta, da, atry;

    //~ Instance fields ------------------------------------------------------------------------------------------------

    /** DOCUMENT ME! */
    protected double a[], dyda[], stdv, xseries[], yseries[];

    /** DOCUMENT ME! */
    protected double chisq;

    /** variables. */
    protected double[][] covar, alpha;

    /** DOCUMENT ME! */
    protected double[] gues;

    /** DOCUMENT ME! */
    protected int nParams, nPts, kk;

    /** DOCUMENT ME! */
    protected double[] sig;

    /** DOCUMENT ME! */
    protected double[] siga;

    /** DOCUMENT ME! */
    protected double ymod;

    /** DOCUMENT ME! */
    private double chiSq1, chiSq2, chiSq3;

    /** DOCUMENT ME! */
    private double[] deltaa;

    /** DOCUMENT ME! */
    private double[][] dYda;

    /** DOCUMENT ME! */
    private int fitMethod;

    /** DOCUMENT ME! */
    private double[] grad;

    /** DOCUMENT ME! */
    private double stepScale;

    /** DOCUMENT ME! */
    private double XiSq0;

    /** DOCUMENT ME! */
    private double[] y_0;

    //~ Constructors ---------------------------------------------------------------------------------------------------

    /**
     * NLEngine2 - non-linear fit to a function.
     *
     * @param  nPts       number of points to fit to the function to
     * @param  nParams    number of parameters of function
     * @param  fitMethod  nonlinear fitting method used
     */
    public NLEngine2(int nPts, int nParams, int fitMethod) {

        try {
            this.nPts = nPts;
            this.nParams = nParams;
            this.fitMethod = fitMethod;

            xseries = new double[nPts];
            yseries = new double[nPts];
            sig = new double[nPts];
            y_0 = new double[nPts];

            a = new double[nParams];
            gues = new double[nParams];
            covar = new double[nParams][nParams];
            alpha = new double[nParams][nParams];
            atry = new double[nParams];
            da = new double[nParams];
            beta = new double[nParams];
            deltaa = new double[nParams];
            siga = new double[nParams];
            grad = new double[10];
            dyda = new double[nParams];
            dYda = new double[nPts][nParams];
        } catch (OutOfMemoryError error) { }
    }

    //~ Methods --------------------------------------------------------------------------------------------------------


    /**
     * DOCUMENT ME!
     *
     * @param   x1    DOCUMENT ME!
     * @param   atry  DOCUMENT ME!
     * @param   dyda  DOCUMENT ME!
     *
     * @return  DOCUMENT ME!
     */
    public abstract double fitToFunction(double x1, double[] atry, double[] dyda);

    /**
     * driver.
     */
    public void driver() {
        int i, j;
        double chiOld;
        int nFree;
        double chiCut = 0.01;
        double stepDown = 0.1;
        double lambda = 0.001;
        double x2Prob;
        double chi2PerDof;

        try {

            switch (fitMethod) {

                case GRID_SEARCH:
                    stepScale = 0.49999;
                    break;

                case GRADIENT_SEARCH:
                    stepScale = 0.99999;
                    break;

                case CHISQ_EXPANSION:
                    stepScale = 0.001;
                    break;

                case MARQUARDT:
                    stepScale = 0.001;
                    break;
            }

            for (i = 0; i < nParams; i++) {
                a[i] = gues[i];
                deltaa[i] = a[i] * stepScale;
            }

            kk = 0;
            chisq = calcChiSqr();

            do {
                chiOld = chisq;
                Preferences.debug("iteration = " + kk + " chisq = " + chisq + "\n");

                for (j = 0; j < nParams; j++) {
                    Preferences.debug("a[" + j + "] = " + a[j] + "\n");
                }

                switch (fitMethod) {

                    case GRID_SEARCH:
                        gridls();
                        break;

                    case GRADIENT_SEARCH:
                        gradls(stepDown);
                        break;

                    case CHISQ_EXPANSION:
                        chiFit();
                        break;

                    case MARQUARDT:
                        marquardt(chiCut, lambda);
                        break;
                }

                kk++;
            } while (Math.abs(chiOld - chisq) >= chiCut);

            chisq = calcChiSqr();
            nFree = nPts - nParams;
            chi2PerDof = chisq / nFree;
            x2Prob = 100 * chiProb(nFree, chisq);

            switch (fitMethod) {

                case GRID_SEARCH:
                    sigParab();
                    break;

                case GRADIENT_SEARCH:
                    sigParab();
                    break;

                case CHISQ_EXPANSION:
                    sigMatrix();
                    break;

                case MARQUARDT:
                    sigMatrix();
                    break;
            }

            /*System.out.println("chisq = " + chisq);
             * System.out.println("degrees of freedom = " + nFree); System.out.println("chisq/dof = " + chi2PerDof);
             * System.out.println("prob = " + x2Prob); System.out.println("Fitted parameters: a[i] +- siga[i]"); for (i
             * = 0; i < nParams; i++) { System.out.println("a[" + i + "] = " + a[i] + " +- " + siga[i]); } if
             * ((fitMethod == CHISQ_EXPANSION) || (fitMethod == MARQUARDT)) { System.out.println("Error matrix"); for (i
             * = 0; i < nParams; i++) {     String varLine = "";     for (j = 0; j < nParams; j++) {         varLine =
             * varLine + " " + alpha[i][j];     }     System.out.println(varLine); }}*/
        } catch (Exception e) {
            Preferences.debug("driver error: " + e.getMessage());
        }
    }

    /**
     * getChiSquared - accessor to chi-squared value (goodness-of-fit measure).
     *
     * @return  the value of chi squared
     */
    public double getChiSquared() {
        return chisq;
    }

    /**
     * getParameters accessor to function parameters.
     *
     * @return  the function parameters determined by the algorithm
     */
    public double[] getParameters() {
        return a;
    }

    /**
     * d2XiSq_da2.
     *
     * @param   j  DOCUMENT ME!
     *
     * @return  DOCUMENT ME!
     */
    double d2XiSq_da2(int j) // See Eq. 8.35  - this sums over nPts
    {
        double tem;
        int i;

        if (j == 0) {

            for (i = 0; i < nPts; i++) {
                y_0[i] = fitToFunction(xseries[i], a, dyda); // Starting point-calculate it once
            }
        }

        a[j] = a[j] + deltaa[j];
        tem = 0.0;

        for (i = 0; i < nPts; i++) {
            dYda[i][j] = (fitToFunction(xseries[i], a, dyda) - y_0[i]) / deltaa[j] / sig[i];
            tem = tem + (dYda[i][j] * dYda[i][j]);
        }

        a[j] = a[j] - deltaa[j];

        return (2 * tem);
    }

    /**
     * d2XiSq_dajk.
     *
     * @param   j  DOCUMENT ME!
     * @param   k  DOCUMENT ME!
     *
     * @return  DOCUMENT ME!
     */
    double d2XiSq_dajk(int j, int k) // See Eq. 8.35
    {
        double tem = 0.0;
        int i;

        for (i = 0; i < nPts; i++) {
            tem = tem + (dYda[i][j] * dYda[i][k]);
        }

        return (2 * tem);
    }

    // ------------------------------------------------- Gaussian -----------------------------------------------
    /**
     * gauss_Dens.
     *
     * @param   x       DOCUMENT ME!
     * @param   dummy1  DOCUMENT ME!
     * @param   dummy2  DOCUMENT ME!
     * @param   dummy3  DOCUMENT ME!
     *
     * @return  DOCUMENT ME!
     */
    double gauss_Dens(double x, double dummy1, double dummy2, double dummy3) // Standard Gaussian with mean = 0 and
                                                                             // st.dev = 1
    {
        return (Math.exp(-x * x / 2) / Math.sqrt(2 * Math.PI));
    }

    /**
     * sigMatrix - standard deviations as sqrt of diagonal elements of error matrix.
     */
    void sigMatrix() {
        int j;

        for (j = 0; j < nParams; j++) {

            siga[j] = Math.sqrt(alpha[j][j]);
        }
    }

    /**
     * sigParab - Standard deviations calc'd from chiSq change of 1 (parabola fit at Xi2 minimum).
     */
    void sigParab() {
        int j;

        for (j = 0; j < nParams; j++) {
            chiSq2 = calcChiSqr();
            a[j] = a[j] + deltaa[j];
            chiSq3 = calcChiSqr();
            a[j] = a[j] - (2 * deltaa[j]);
            chiSq1 = calcChiSqr();
            a[j] = a[j] + deltaa[j];
            siga[j] = Math.abs(deltaa[j] * Math.sqrt(2 / (chiSq1 - (2 * chiSq2) + chiSq3)));
        }
    }

    /**
     * calcChiSqr - calculates chisq.
     *
     * @return  DOCUMENT ME!
     */
    private double calcChiSqr() {
        int i;
        double xi1;
        double xi2 = 0.0;

        for (i = 0; i < nPts; i++) {
            ymod = fitToFunction(xseries[i], a, dyda);
            xi1 = (yseries[i] - ymod) / sig[i];
            xi2 = xi2 + (xi1 * xi1);
        }

        return xi2;
    }

    /**
     * calcGrad.
     */
    private void calcGrad() {
        double fract = 0.001;
        int j;
        double dA, sum;

        sum = 0.0;
        chiSq2 = calcChiSqr();

        for (j = 0; j < nParams; j++) {
            dA = fract * deltaa[j]; // differential element for gradent
            a[j] = a[j] + dA;
            chiSq1 = calcChiSqr();
            a[j] = a[j] - dA;
            grad[j] = chiSq2 - chiSq1; // 2*da*grad
            sum = sum + (grad[j] * grad[j]);
        }

        for (j = 0; j < nParams; j++) {
            grad[j] = deltaa[j] * grad[j] / Math.sqrt(sum); // step * grad
        }
    }


    //-------------------------------- Chi^2 Density and Probability ----------------------------

    /**
     * chi2_Dens.
     *
     * @param   x       DOCUMENT ME!
     * @param   h       DOCUMENT ME!
     * @param   g       DOCUMENT ME!
     * @param   dummy1  DOCUMENT ME!
     *
     * @return  DOCUMENT ME!
     */
    private double chi2_Dens(double x, double h, double g, double dummy1) // See ChiProb for g.
    {
        double chiDens;

        if (x == 0.) {
            chiDens = 0.;
        } else {
            chiDens = power(x, h - 1) * Math.exp(-x / 2.) / g; // x^(h-1) e^(-x/2);
        }

        return (chiDens);
    }


    /**
     * Program 8.3: Non-linear least-squares fit by expansion of the fitting function.
     */

    private void chiFit() // double  det, chiSq1;
    {
        int j;

        makeBeta();
        makeAlpha();
        matInv(); // Invert matrix
        squareByRow(); // Evalulate parameter increments

        for (j = 0; j < nParams; j++) {
            a[j] = a[j] + da[j]; // Increment to next solution.
        }

        chisq = calcChiSqr();

        return;
    }

    /**
     * chiProb.
     *
     * @param   nFree  DOCUMENT ME!
     * @param   chi2   DOCUMENT ME!
     *
     * @return  DOCUMENT ME!
     */
    private double chiProb(double nFree, double chi2) {
        double probTem, cLim = 2, // expansion limit for nFree = 1
               intFromLim = 0.157, // integral from cLim to infInity for nFree = 1
               dx = 0.2, g; // determines accuracy of integration}
        int nIntervals = (int) (chi2 / dx); // number  of intervals for Simpson integral.  2 calculations per interval.

        if (nIntervals == 0) {
            nIntervals = 1;
        }

        double h = (double) nFree / 2.;

        g = gamma(h) * power(2, h);

        if (chi2 > (15 * Math.sqrt(nFree))) {
            return (0.0); // quick cutout
        }

        if (nFree == 1) {

            if (chi2 < cLim) { // Integrate expansion of the function
                probTem = 1.0 -
                          (Math.sqrt(chi2 / 2. / Math.PI) *
                               (2. -
                                    (chi2 *
                                         ((1. / 3.) -
                                              (chi2 * ((1.0 / 20.) - (chi2 * ((1. / 168.) - (chi2 / 1728.)))))))));
            } else {

                // Subtract numerical integral from analytic intFromLim
                probTem = intFromLim - simpson('X', nIntervals, cLim, chi2, h, g, 0.);
            }

            return (probTem);
        }

        if (nFree == 2) {
            probTem = Math.exp(-chi2 / 2); // Integrable for nFree = 2;
        } else { // I.e., nFree > 2
            probTem = 1 - simpson('X', nIntervals, 0, chi2, h, g, 0.);
        }

        return (probTem);
    }

    //Numerical Derivatives------------------------------
    // Can be replaced by analytic derivatives, if they can be calculated.
    // However, numerical calculation is general, and convenient.

    /**
     * dXiSq_da.
     *
     * @param   j  DOCUMENT ME!
     *
     * @return  DOCUMENT ME!
     */
    private double dXiSq_da(int j) // See Eq. 8.26 - this sums over nPts
    {
        double XiSqPlus, dXiSqDa;

        if (j == 0) {
            XiSq0 = calcChiSqr(); // starting point-calculate it once
        }

        a[j] = a[j] + deltaa[j];
        XiSqPlus = calcChiSqr();
        a[j] = a[j] - deltaa[j]; // restore
        dXiSqDa = (XiSqPlus - XiSq0) / (deltaa[j]);

        return (dXiSqDa);
    }


    //-------------------------------------------- F Distribution and Integral Probability ------------------

    /**
     * f_Dens.
     *
     * @param   f    DOCUMENT ME!
     * @param   nu1  DOCUMENT ME!
     * @param   nu2  DOCUMENT ME!
     * @param   g    DOCUMENT ME!
     *
     * @return  DOCUMENT ME!
     */
    private double f_Dens(double f, double nu1, double nu2, double g) // f Probability density. See F_Prob for g.
    {
        double tem; // f = chi1*nu2/(chi2*nu1)

        if (f == 0) {
            tem = 0;
        } else {
            tem = g * power(f, 0.5 * (nu1 - 2.)) / power(1. + (f * nu1 / nu2), 0.5 * (nu1 + nu2));
        }

        return (tem);
    }

    /**
     * f_Prob.
     *
     * @param   f    DOCUMENT ME!
     * @param   nu1  DOCUMENT ME!
     * @param   nu2  DOCUMENT ME!
     *
     * @return  DOCUMENT ME!
     */
    private double f_Prob(double f, double nu1, double nu2) // returns integral from f to infinity
    {
        double tem, g;
        int nIntervals = 1000;

        g = gamma(0.5 * (nu1 + nu2)) / (gamma(0.5 * nu1) * gamma(0.5 * nu2)) * power(nu1 / nu2, 0.5 * nu1);
        tem = 1.0 - simpson('F', nIntervals, 0, f, nu1, nu2, g);

        return (tem);
    }

    /**
     * gamma - Gamma Function.
     *
     * @param   h  DOCUMENT ME!
     *
     * @return  DOCUMENT ME!
     */

    private double gamma(double h) // Approximate Gamma function for integers and half-integers
    {
        double gamma; // gamma  = sqrt(2*PI)*[1 + 0.0833/h]*exp(-h)*[h^(h-1/2)] }
        gamma = Math.sqrt(2. * Math.PI) * (1 + (0.0833 / h)) * Math.exp(-h) * power(h, h - 0.5);

        return (gamma);
    }


    /**
     * Program 8.2 Non-linear least-squares fit by gradient-search method}.
     *
     * @param  stepDown  DOCUMENT ME!
     */

    private void gradls(double stepDown) {
        double stepSum, step1;
        double fract = 0.001;
        int j;

        calcGrad(); // calculate the gradient

        //-Evaluate chiSqr at new point and make sure chiSqr decreases-
        do {

            for (j = 0; j < nParams; j++) {
                a[j] = a[j] + (stepDown * grad[j]); // slide down
            }

            chiSq3 = calcChiSqr();

            if (chiSq3 >= chiSq2) { // must have overshot minimum

                for (j = 0; j < nParams; j++) {
                    a[j] = a[j] - (stepDown * grad[j]); // restore
                }

                stepDown = stepDown / 2; // decrease stepSize
            }
        } while (chiSq3 > chiSq2);

        stepSum = 0;
        // -- Increment parameters until chiSqr starts to increase --
        do {
            stepSum = stepSum + stepDown; // counts total increment
            chiSq1 = chiSq2;
            chiSq2 = chiSq3;

            for (j = 0; j < nParams; j++) {
                a[j] = a[j] + (stepDown * grad[j]);
            }

            chiSq3 = calcChiSqr();
        } while (chiSq3 <= chiSq2);
        // -- Find minimum of parabola defined by last three points --

        step1 = stepDown * (((chiSq3 - chiSq2) / (chiSq1 - (2 * chiSq2) + chiSq3)) + 0.5);

        for (j = 0; j < nParams; j++) {
            a[j] = a[j] - (step1 * grad[j]); // move to minimum
        }

        chisq = calcChiSqr();
        stepDown = stepSum; // start with this next time
    }

    /**
     * Program 8.1:Non-linear least-squares fit by the grid-search method.
     */

    private void gridls() {
        //
        double delta;
        double save, delta1, del1, del2, aa, bb, cc, disc, alpha, x1, x2;
        int j;

        // cout << "enter Grids, x[1], y[1] " <<x[1] <<"  "<<y[1]<<"  ";  cin >> j;

        chiSq2 = calcChiSqr();
        // -find local minimum for each parameter-
        for (j = 0; j < nParams; j++) {
            delta = deltaa[j];
            a[j] = a[j] + delta;
            chiSq3 = calcChiSqr();

            if (chiSq3 > chiSq2) { // started in wrong direction
                delta = -delta;
                a[j] = a[j] + delta;
                save = chiSq2; // interchange 2 and 3 so 3 is lower
                chiSq2 = chiSq3;
                chiSq3 = save;
            }
            // -Increment or decrement a[j] until chi squared increases-
            do {
                chiSq1 = chiSq2; // move back to prepare for quad fit
                chiSq2 = chiSq3;
                a[j] = a[j] + delta;
                chiSq3 = calcChiSqr();
            } while (chiSq3 < chiSq2);

            // -Find minimum of parabola defined by last three points  -
            del1 = chiSq2 - chiSq1;
            del2 = chiSq3 - (2 * chiSq2) + chiSq1;
            delta1 = delta * ((del1 / del2) + 1.5);
            a[j] = a[j] - delta1;
            chiSq2 = calcChiSqr(); // at new local minimum

            // -Adjust delta for change of 2 from chiSq at minimum  -
            aa = del2 / 2; // chiSq = aa*sqr(a[j] + bb*a[j] + cc
            bb = del1 - (del2 / 2);
            cc = chiSq1 - chiSq2;
            disc = (bb * bb) - (4 * aa * (cc - 2)); // chiSqr difference=2

            if (disc > 0) // if not true, then probably not parabolic yet
            {
                disc = Math.sqrt(disc);
                alpha = (-bb - disc) / (2 * aa);
                x1 = (alpha * delta) + a[0] - (2 * delta); // a[j] at chiSq minimum+2
                disc = (bb * bb) - (4 * aa * cc);

                if (disc > 0) {
                    disc = Math.sqrt(disc);
                } else {
                    disc = 0; // elim round err
                }

                alpha = (-bb - disc) / (2 * aa);
                x2 = (alpha * delta) + a[0] - (2 * delta); // at chiSq minimum
                delta = x1 - x2;
                deltaa[j] = delta;
            }
        } // for j = 0 to nParams-1}

        chisq = chiSq2;
    }

    /**
     * linCorProb.
     *
     * @param   r      DOCUMENT ME!
     * @param   nFree  DOCUMENT ME!
     *
     * @return  DOCUMENT ME!
     */
    private double linCorProb(double r, double nFree) {
        double dx = 0.01;
        double h, tem;
        int nIntervals;

        h = nFree;
        nIntervals = (int) (r / dx);
        tem = 1. - (2. * simpson('C', nIntervals, 0., r, h, 0, 0.));

        return (tem);
    }

    /**
     * linCorrel_Dens.
     *
     * @param   r       DOCUMENT ME!
     * @param   nFree   DOCUMENT ME!
     * @param   dummy   DOCUMENT ME!
     * @param   dummy1  DOCUMENT ME!
     *
     * @return  DOCUMENT ME!
     */
    private double linCorrel_Dens(double r, double nFree, double dummy, double dummy1) {
        double tem;
        tem = gamma((nFree + 1.) / 2.) / gamma(nFree / 2.) * Math.exp((nFree - 2.) / 2. * Math.log(1. - (r * r))) /
                  Math.sqrt(Math.PI);

        return (tem);
    }

    /**
     * makeAlpha.
     */
    private void makeAlpha() // Make the alpha matrices
    {
        int j, k;

        //--------------------- Set up the alpha matrices for linear fitting

        try {

            for (j = 0; j < nParams; j++) {
                alpha[j][j] = 0.5 * d2XiSq_da2(j);

                if (alpha[j][j] == 0) {
                    throw new Exception("Diagonal element is zero");
                }

                if (j > 0) {

                    for (k = 0; k < j; k++) {
                        alpha[j][k] = 0.5 * d2XiSq_dajk(j, k);
                        alpha[k][j] = alpha[j][k];
                    } // for k
                } // if j
            } // for j

            for (j = 0; j < nParams; j++) {

                if (alpha[j][j] < 0.0) {
                    alpha[j][j] = -alpha[j][j];

                    if (j > 0) {

                        for (k = 0; k < j; k++) {
                            alpha[j][k] = 0;
                            alpha[k][j] = 0;
                        } // for k
                    } // if j
                } // if alpha
            } // for j
        } catch (Exception e) {
            Preferences.debug("makeAlpha error: " + e.getMessage());
        }

    }

    /**
     * makeBeta.
     */
    private void makeBeta() {
        int j;

        for (j = 0; j < nParams; j++) {
            beta[j] = -dXiSq_da(j) / 2.;
        }

    }

    /**
     * Program 8.4: Non-linear least-squares fit by the gradient-expansion (Marquardt) method.
     *
     * @param  chiCut  DOCUMENT ME!
     * @param  lambda  DOCUMENT ME!
     */

    private void marquardt(double chiCut, double lambda) {
        int j;
        double chiSq1;
        boolean again;

        again = true;

        while (again) {
            again = false;
            makeBeta();
            makeAlpha();

            for (j = 0; j < nParams; j++) {
                alpha[j][j] = (1 + lambda) * alpha[j][j];
            }

            matInv(); // Invert matrix

            if (lambda > 0) // On final call, enter with lambda = 0 to get the error matrix
            {
                squareByRow(); // Evaluate parameter increments
                chiSq1 = chisq;

                for (j = 0; j < nParams; j++) {
                    a[j] = a[j] + da[j]; // Incr to next solution
                }

                chisq = calcChiSqr();

                if (chisq > (chiSq1 + chiCut)) {

                    for (j = 0; j < nParams; j++) {
                        a[j] = a[j] - da[j]; // Return to prev solution
                    }

                    chisq = calcChiSqr();
                    lambda = 10 * lambda; // and repeat the calc, with larger lambda
                    again = true;
                } else {
                    lambda = 0.1 * lambda;

                    return;
                }
            } else {
                return;
            }
        } // while again()
    }

    /**
     * matInv invert a matrix.
     *
     * @return  DOCUMENT ME!
     */
    private double matInv() {
        int i, j, k, L;
        int[] ik = new int[10];
        int[] jk = new int[10];
        double aMax, save, det;
        boolean again;

        det = 0;
        //------------------------------------  find largest element
        for (k = 0; k < nParams; k++) {
            aMax = 0;

FIND_AMAX:
            again = true;

            while (again) {
                again = false;

                for (i = k; i < nParams; i++) {

                    for (j = k; j < nParams; j++) {

                        if (Math.abs(alpha[i][j]) > Math.abs(aMax)) {
                            aMax = alpha[i][j];
                            ik[k] = i;
                            jk[k] = j;
                        } // if
                    } // for j
                } // for i

                if (aMax == 0) {
                    return (det); // with 0 determinant as signal
                }

                det = 1;

                // -------------------------------------- interchange rows and columns to put aMax in alpha[k,k]---
                i = ik[k];

                if (i < k) {
                    again = true;
                } else { // if (i >= k)

                    if (i > k) {

                        for (j = 0; j < nParams; j++) {
                            save = alpha[k][j];
                            alpha[k][j] = alpha[i][j];
                            alpha[i][j] = -save;
                        } // for j
                    } // if (i > k)

                    j = jk[k];

                    if (j < k) {
                        again = true;
                    } else if (j > k) {

                        for (i = 0; i < nParams; i++) {
                            save = alpha[i][k];
                            alpha[i][k] = alpha[i][j];
                            alpha[i][j] = -save;
                        } // for i
                    } // else if (j > k)
                } // else if (i >= k)
            } // while (again)

            // ---------------------------------------- accumulate elements of inverse matrix
            for (i = 0; i < nParams; i++) {

                if (i != k) {
                    alpha[i][k] = -alpha[i][k] / aMax;
                }
            } // for i

            for (i = 0; i < nParams; i++) {

                for (j = 0; j < nParams; j++) {

                    if ((i != k) && (j != k)) {
                        alpha[i][j] = alpha[i][j] + (alpha[i][k] * alpha[k][j]);
                    }
                } // for j
            } // for i

            for (j = 0; j < nParams; j++) {

                if (j != k) {
                    alpha[k][j] = alpha[k][j] / aMax;
                }
            } // for j

            alpha[k][k] = 1 / aMax;
            det = det * aMax;
        } // for k

        // ------------------------------------------ restore ordering of matrix
        for (L = 0; L < nParams; L++) {
            k = nParams - 1 - L;
            j = ik[k];

            if (j > k) {

                for (i = 0; i < nParams; i++) {
                    save = alpha[i][k];
                    alpha[i][k] = -alpha[i][j];
                    alpha[i][j] = save;
                } // for i
            } // if j

            i = jk[k];

            if (i > k) {

                for (j = 0; j < nParams; j++) {
                    save = alpha[k][j];
                    alpha[k][j] = -alpha[i][j];
                    alpha[i][j] = save;
                } // for j
            } // if i
        } // for L

        return (det);
    }

    /**
     * power.
     *
     * @param   x  DOCUMENT ME!
     * @param   a  DOCUMENT ME!
     *
     * @return  DOCUMENT ME!
     */
    private double power(double x, double a) // x^a
    {
        double tem;

        tem = Math.exp(a * Math.log(x));

        return (tem);
    }

    /**
     * simpson Simpson's rule.
     *
     * @param   functCode   selects the function
     * @param   nIntervals  DOCUMENT ME!
     * @param   loLim       DOCUMENT ME!
     * @param   hiLim       DOCUMENT ME!
     * @param   p1          DOCUMENT ME!
     * @param   p2          DOCUMENT ME!
     * @param   p3          DOCUMENT ME!
     *
     * @return  DOCUMENT ME!
     */

    private double simpson(char functCode, int nIntervals, double loLim, double hiLim, double p1, double p2,
                           double p3) // 2 calcs/interval
    {
        double sum = 0.0;
        double x, dx, t1, t2;
        int i;

        x = loLim;
        dx = (hiLim - loLim) / (2 * nIntervals);
        // --------------- Select function- must be in this list
        switch (functCode) {

            case 'X':
                sum = chi2_Dens(loLim, p1, p2, p3) - chi2_Dens(hiLim, p1, p2, p3);
                for (i = 1; i <= nIntervals; i++) {
                    x = x + (2 * dx);

                    t1 = chi2_Dens(x - dx, p1, p2, p3);
                    t2 = chi2_Dens(x, p1, p2, p3);
                    sum = sum + (4 * t1) + (2 * t2);

                    // cout << x << "   " << t2 << "  " << sum <<  endl;
                }

                break;

            case 'C':
                sum = linCorrel_Dens(loLim, p1, p2, p3) - linCorrel_Dens(hiLim, p1, p2, p3);
                for (i = 1; i <= nIntervals; i++) {
                    x = x + (2 * dx);

                    t1 = linCorrel_Dens(x - dx, p1, p2, p3);
                    t2 = linCorrel_Dens(x, p1, p2, p3);
                    sum = sum + (4 * t1) + (2 * t2);

                    // cout << x << "   " << t2 << "  " << sum <<  endl;
                }

                break;

            case 'T':
                sum = studentT_Dens(loLim, p1, p2, p3) - studentT_Dens(hiLim, p1, p2, p3);
                for (i = 1; i <= nIntervals; i++) {
                    x = x + (2 * dx);

                    t1 = studentT_Dens(x - dx, p1, p2, p3);
                    t2 = studentT_Dens(x, p1, p2, p3);
                    sum = sum + (4 * t1) + (2 * t2);

                    // cout << x << "   " << t2 << "  " << sum <<  endl;
                }

                break;

            case 'F':
                sum = f_Dens(loLim, p1, p2, p3) - f_Dens(hiLim, p1, p2, p3);
                for (i = 1; i <= nIntervals; i++) {
                    x = x + (2 * dx);

                    t1 = f_Dens(x - dx, p1, p2, p3);
                    t2 = f_Dens(x, p1, p2, p3);
                    sum = sum + (4 * t1) + (2 * t2);

                    // cout << x << "   " << t2 << "  " << sum <<  endl;
                }

                break;

            case 'G':
                sum = gauss_Dens(loLim, p1, p2, p3) - gauss_Dens(hiLim, p1, p2, p3);
                for (i = 1; i <= nIntervals; i++) {
                    x = x + (2 * dx);

                    t1 = gauss_Dens(x - dx, p1, p2, p3);
                    t2 = gauss_Dens(x, p1, p2, p3);
                    sum = sum + (4 * t1) + (2 * t2);

                    // cout << x << "   " << t2 << "  " << sum <<  endl;
                }

                break;
        }

        sum = sum * dx / 3;

        return (sum);
    }


    /**
     * squareByRow.
     */
    private void squareByRow() // multiply square matrix by row matrix
    {
        int i, j;

        for (i = 0; i < nParams; i++) {
            da[i] = 0.0;

            for (j = 0; j < nParams; j++) {
                da[i] = da[i] + (beta[j] * alpha[i][j]);
            }
        }
    }

    // ---------------------------- Students t Probability
    /**
     * stud_tProb.
     *
     * @param   t  DOCUMENT ME!
     * @param   h  DOCUMENT ME!
     *
     * @return  DOCUMENT ME!
     */
    private double stud_tProb(double t, double h) // returns integral from -t to +t
    {
        double g, dt, tem;
        int nIntervals;

        g = (gamma((h + 1) / 2) / gamma(h / 2)) / Math.sqrt(h * Math.PI); // indep of t - calculate once
        dt = 0.01; // integration step
        nIntervals = (int) (t / dt);
        tem = 2. * simpson('T', nIntervals, 0, t, h, g, 0.);

        return (tem);
    }


    // ---------------------------------Integral Probability for Student's t ----------------------
    /**
     * studentT_Dens.
     *
     * @param   t      DOCUMENT ME!
     * @param   nu     DOCUMENT ME!
     * @param   g      DOCUMENT ME!
     * @param   dummy  DOCUMENT ME!
     *
     * @return  DOCUMENT ME!
     */
    private double studentT_Dens(double t, double nu, double g, double dummy) // Student's t function. See Stud_Prob
                                                                              // for g.
    {
        double x;

        x = g * Math.exp((-0.5 * (nu + 1.)) * Math.log(1 + (t * t / nu)));

        return (x);
    }

}
