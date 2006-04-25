package gov.nih.mipav.model.algorithms;


import gov.nih.mipav.view.*;


/**
 * NLEngine&#59 ap dagher, 8-1-97.
 *
 * <p>Methods for Non-linear curve fitting in Java single class to hold all functions&#59 portable to any calling class
 * with function to be fitted defined in the subclass See &quotNumerical Recipes&quot, WH Press et al, Cambridge
 * University Press, 1988. Levenberg-Marquardt Method, Chap 15, section 5 p. 683 and Marquardt D, 1963, J Soc Ind Appl
 * Math, 11:431-441.</p>
 */

public abstract class NLEngine {

    //~ Static fields/initializers -------------------------------------------------------------------------------------

    /** DOCUMENT ME! */
    protected static final double ITMAX = 100;

    /** DOCUMENT ME! */
    protected static final double EPS = 0.0000003;

    /** DOCUMENT ME! */
    protected static final double ALAMDA = 0.001; // initial alamda

    /** DOCUMENT ME! */
    protected static final double FPMIN = 1.0E-30;
    //  public static final int LINE                = 1;
    //  public static final int EXPONENTIAL         = 2;

    /** DOCUMENT ME! */
    protected static double beta[], da[], atry[], oneda[][], ochisq2;

    //~ Instance fields ------------------------------------------------------------------------------------------------

    /** DOCUMENT ME! */
    protected double a[], dyda[], stdv, xseries[], yseries[];

    /** DOCUMENT ME! */
    protected double chisq, ochisq, alamda;

    /** variables. */
    protected double[][] covar, alpha;

    /** DOCUMENT ME! */
    protected double[] gues;

    /** DOCUMENT ME! */
    protected int ma, ia[], mfit, ndata, kk;

    /** DOCUMENT ME! */
    protected double[] sig;

    /** DOCUMENT ME! */
    protected double ymod, flamda;

    /** pq variables. */
    private double gamser, gammcf, gln;

    //~ Constructors ---------------------------------------------------------------------------------------------------

    /**
     * NLEngine - non-linear fit to a function.
     *
     * @param  nPts  number of points to fit to the function to
     * @param  _ma   number of parameters of function
     */
    public NLEngine(int nPts, int _ma) {

        try {
            ndata = nPts;
            ma = _ma;

            xseries = new double[nPts];
            yseries = new double[nPts];
            sig = new double[nPts];

            ia = new int[ma];
            a = new double[ma];
            gues = new double[ma];
            covar = new double[ma][ma];
            alpha = new double[ma][ma];
            atry = new double[ma];
            da = new double[ma];
            beta = new double[ma];
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
        int j;

        try {

            /* mrqmin driver */
            /* Nonzero entries of ia give those parameters that are to be fitted.
             * Zero entries of ia give those parameters that are to be held constant. mfit of the ma parameters are to
             * be fitted.  ma - mfit of the parameters are to be held constant. */
            for (mfit = 0, j = 0; j < ma; j++) {

                if (ia[j] != 0) {
                    mfit++;
                }
            }

            oneda = new double[mfit][1];

            alamda = -1; /* initialize */

            // provide an initial guess for the parameters a.
            for (int i = 0; i < ma; i++) {
                a[i] = gues[i];
            }

            mrqmin();

            /* begin iterations */
            kk = 1;

            int itst = 0;
            int counter = 0;

            while (itst <= 10) { /* itst is the number of loops tolerated
                                  * without any significant change in chisq */
                kk++; // iteration
                ochisq = chisq;
                mrqmin();

                if (chisq > ochisq) {
                    itst = 0;
                } else if (Math.abs(ochisq - chisq) < 0.01) {
                    itst++;
                }

                if (counter == 2000) {
                    break;
                }

                counter++;
            } /* end of iterative loop */

            flamda = alamda; // final lamda

            /* compute the covariance and curvature matrices */
            alamda = 0.0;
            mrqmin();
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


    /*
     *   fitLine -  @param x1    the x value of the data point  @param atry  the best guess parameter values  @param
     * dyda  the derivative values of y with respect to fitting parameters  @return      the calculated y value
     */
    /*  public double fitLine(double x1, double atry[], double dyda[]) {
            // called by mrqcof
            // mrqcof supplies x1 and atry[]
            // function returns partial derivatives dyda[] and the calculated ymod
            double fac, ymod = 0;
            try {
                fac     = atry[1] * x1;
                ymod    = atry[0] + fac;
                dyda[0] = 1;            // a0 partial derivative (y intecept)
                dyda[1] = x1;           // a1 partial derivative (slope)
            }
            catch (Exception e) {
                if (Preferences.isDebug()) System.err.println("function error: " + e.getMessage());
            }
            return ymod;
        }
      */
    /*
     *   fitExponential - a0 + a1*exp(a2*x)  @param x1    the x value of the data point  @param atry  the best guess
     * parameter values  @param dyda  the derivative values of y with respect to fitting parameters  @return      the
     * calculated y value
     */
    /*public double fitExponential(double x1, double atry[], double dyda[]) {
     *  // mrqcof calls function // mrqcof supplies x1 and best guess parameters atry[] // function returns the partial
     * derivatives dyda and the calculated ymod double ymod = 0; try {     ymod = atry[0] + atry[1]*Math.exp(atry[2] *
     * x1);         dyda[0] = 1;                                        // a0 partial derivative     dyda[1] =
     * Math.exp(atry[2] * x1);                   // a1 partial derivative     dyda[2] = atry[1]*x1*Math.exp(atry[2] *
     * x1);        // a2 partial derivative }  catch (Exception e) {     if (Preferences.isDebug())
     * System.err.println("function error: " + e.getMessage()); }  return ymod;}*/

    /**
     * covsrt - used to rearrange the alpha and covariance matrices into the order of all ma parameters.
     *
     * @param  mat  DOCUMENT ME!
     */
    private void covsrt(double[][] mat) {
        int i, j, k;
        double temp;

        try {

            for (i = mfit; i < ma; i++) {

                for (j = 0; j <= i; j++) {
                    mat[i][j] = mat[j][i] = 0.0;
                }
            }

            k = mfit - 1;

            for (j = ma - 1; j >= 0; j--) {

                if (ia[j] != 0) {

                    for (i = 0; i < ma; i++) {
                        temp = mat[i][k];
                        mat[i][k] = mat[i][j];
                        mat[i][j] = temp;
                    }

                    for (i = 0; i < ma; i++) {
                        temp = mat[k][i];
                        mat[k][i] = mat[j][i];
                        mat[j][i] = temp;
                    }

                    k--;
                }
            }
        } catch (Exception e) {
            Preferences.debug("covsrt error: " + e.getMessage());
        }
    }

    /**
     * gammln -.
     *
     * @param   xx  DOCUMENT ME!
     *
     * @return  value ln(T(xx)) for xx &gt 0.
     */
    private double gammln(double xx) {
        double x = 1, y, tmp = 0, ser = 0;
        double[] cof = {
            76.18009172947146, -86.50532032941677, 24.01409824083091, -1.231739572450155, 0.1208650973866179e-2,
            -0.5395239384953e-5
        };

        try {
            int j;
            y = x = xx;
            tmp = x + 5.5;
            tmp -= (x + 0.5) * Math.log(tmp);
            ser = 1.000000000190015;

            for (j = 0; j <= 5; j++) {
                ser += cof[j] / ++y;
            }
        } catch (Exception e) {
            Preferences.debug("gammln error: " + e.getMessage());
        }

        return -tmp + Math.log(2.5066282746310005 * ser / x);
    }


    /**
     * gammq - pq functions. See Numerical Recipes Section 6.1-6.2
     *
     * @param   a  DOCUMENT ME!
     * @param   x  DOCUMENT ME!
     *
     * @return  DOCUMENT ME!
     */
    private double gammq(double a, double x) { // returns the incomplete gamma function Q(a,x,) = 1 - P(a,x)

        try {

            if ((x < 0.0) || (a <= 0.0)) {
                throw new Exception("Invalid arguments in routine gammq()");
            }
        } catch (Exception e) {
            Preferences.debug("gammq error: " + e.getMessage());
        }

        if (x < (a + 1.0)) { // use the series representation
            gser(a, x);

            return 1.0 - gamser; // return complement
        } else { // use the continued fraction representation
            gcf(a, x); // changes gammcf;

            return gammcf;
        }
    }

    /**
     * gaussj - method for Gauss-Jordan Elimination to invert a matrix.
     *
     * @param  a  DOCUMENT ME!
     * @param  n  DOCUMENT ME!
     * @param  b  DOCUMENT ME!
     * @param  m  DOCUMENT ME!
     */
    private void gaussj(double[][] a, int n, double[][] b, int m) {

        try {
            int[] indxc, indxr, ipiv;
            int icol = 0, irow = 0;
            double big, dum, pivinv;

            indxc = new int[n];
            indxr = new int[n];
            ipiv = new int[n];

            for (int j = 0; j < n; j++) {
                ipiv[j] = 0;
            }

            for (int i = 0; i < n; i++) {
                big = 0.0;

                for (int j = 0; j < n; j++) {

                    if (ipiv[j] != 1) {

                        for (int k = 0; k < n; k++) {

                            if (ipiv[k] == 0) {

                                if (Math.abs(a[j][k]) >= big) {
                                    big = Math.abs(a[j][k]);
                                    irow = j;
                                    icol = k;
                                }
                            } else if (ipiv[k] > 1) {
                                throw new Exception("singular matrix");
                            }
                        }
                    }
                }

                ++(ipiv[icol]);

                if (irow != icol) {

                    for (int l = 0; l < n; l++) {
                        double temp1 = a[irow][l];
                        double temp2 = a[icol][l];
                        a[irow][l] = temp2;
                        a[icol][l] = temp1;
                    }

                    for (int l = 0; l < m; l++) {
                        double temp1 = b[irow][l];
                        double temp2 = b[icol][l];
                        b[irow][l] = temp2;
                        b[icol][l] = temp1;
                    }
                }

                indxr[i] = irow;
                indxc[i] = icol;

                if (a[icol][icol] == 0.0) {
                    throw new Exception("singular matrix");
                }

                pivinv = 1.0 / a[icol][icol];
                a[icol][icol] = 1.0;

                for (int l = 0; l < n; l++) {
                    a[icol][l] *= pivinv;
                }

                for (int l = 0; l < m; l++) {
                    b[icol][l] *= pivinv;
                }

                for (int ll = 0; ll < n; ll++) {

                    if (ll != icol) {
                        dum = a[ll][icol];
                        a[ll][icol] = 0.0;

                        for (int l = 0; l < n; l++) {
                            a[ll][l] -= a[icol][l] * dum;
                        }

                        for (int l = 0; l < m; l++) {
                            b[ll][l] -= b[icol][l] * dum;
                        }
                    }
                }
            }

            for (int l = (n - 1); l >= 0; l--) {

                if (indxr[l] != indxc[l]) {

                    for (int k = 0; k < n; k++) {
                        double temp1 = a[k][indxr[l]];
                        double temp2 = a[k][indxc[l]];
                        a[k][indxr[l]] = temp2;
                        a[k][indxc[l]] = temp1;
                    }
                }
            }
        } catch (Exception e) {
            Preferences.debug("gaussj error: " + e.getMessage());
        }
    }


    /**
     * gcf - returns the incomplete gamma function P(a,x) evaluated in its series representation as gamser. T(a) is also
     * returned
     *
     * @param  a  DOCUMENT ME!
     * @param  x  DOCUMENT ME!
     */
    private void gcf(double a, double x) {

        try {
            int i;
            double an, b, c, d, del, h;

            gln = gammln(a);
            b = x + 1.0 - a; // Set up for evaluating continued fraction by modified
            c = 1.0 / FPMIN; // Lentz's method
            d = 1.0 / b;
            h = d;

            for (i = 1; i <= ITMAX; i++) { // Iterate to convergence
                an = -i * (i - a);
                b += 2.0;
                d = (an * d) + b;

                if (Math.abs(d) < FPMIN) {
                    d = FPMIN;
                }

                c = b + (an / c);

                if (Math.abs(c) < FPMIN) {
                    c = FPMIN;
                }

                d = 1.0 / d;
                del = d * c;
                h *= del;

                if (Math.abs(del - 1.0) < EPS) {
                    break;
                }
            }

            if (i > ITMAX) {
                throw new Exception("a too large, ITMAX too small in routine gcf()");
            }

            gammcf = Math.exp(-x + (a * Math.log(x)) - gln) * h; // Put factors in front
        } catch (Exception e) {
            Preferences.debug("gcf error: " + e.getMessage());
        }
    }

    /**
     * gser - returns the incomplete gamma function P(a,x) evaluated in its series representation as gamser. T(a) is
     * also returned
     *
     * @param  a  DOCUMENT ME!
     * @param  x  DOCUMENT ME!
     */
    private void gser(double a, double x) {

        try {
            int n;
            double sum, del, ap;

            gln = gammln(a);

            if (x <= 0.0) {

                if (x < 0.0) {
                    throw new Exception("x < 0 in routine gser()");
                }

                gamser = 0.0;

                return;
            } else {
                ap = a;
                del = sum = 1.0 / a;

                for (n = 1; n <= ITMAX; n++) {
                    ap += 1.0;
                    del *= x / ap;
                    sum += del;

                    if (Math.abs(del) < (Math.abs(sum) * EPS)) {
                        gamser = sum * Math.exp(-x + (a * Math.log(x)) - (gln));

                        return;
                    }
                }

                throw new Exception("a too large, ITMAX too small in routine gser()");
            }
        } catch (Exception e) {
            Preferences.debug("gamser error: " + e.getMessage());
        }
    }

    /**
     * mrqcof - method to adjust parameter estimates based on partial derivatives.
     *
     * @param  atry   the best guess parameter values
     * @param  alpha  DOCUMENT ME!
     * @param  beta   DOCUMENT ME!
     */
    private void mrqcof(double[] atry, double[][] alpha, double[] beta) {
        double sig2i, dy, wt;
        int i, j, k, l, m;

        try {
            dyda = new double[ma];

            for (j = 0; j < mfit; j++) {

                for (k = 0; k <= j; k++) {
                    alpha[j][k] = 0.0;
                }

                beta[j] = 0.0;
            }

            chisq = 0.0;

            for (i = 0; i < ndata; i++) {

                ymod = fitToFunction(xseries[i], atry, dyda);

                sig2i = 1.0 / (sig[i] * sig[i]);
                dy = yseries[i] - ymod;

                for (j = 0, l = 0; l < ma; l++) {

                    if (ia[l] != 0) {
                        wt = dyda[l] * sig2i;

                        for (k = 0, m = 0; m <= l; m++, k++) {

                            if (ia[m] != 0) {
                                alpha[j][k] += wt * dyda[m];
                            }
                        }

                        beta[j] += dy * wt;
                        j++;
                    }
                }

                chisq += dy * dy * sig2i;
            }

            for (j = 1; j < mfit; j++) {

                for (k = 0; k < j; k++) {
                    alpha[k][j] = alpha[j][k];
                }
            }
        } catch (Exception e) {
            Preferences.debug("mrqcof error: " + e.getMessage());
        }
    }

    /**
     * mrqmin - see page 685 in Numerical Recipes in C.
     */
    private void mrqmin() { // mrqmin; see page 685 in Numerical Recipes in C

        /* This routine calls mrqcof and covsrt.
         * Levenberg-Marquardt method, attempting to reduce the value chisquare of a fit between a set of data points
         * x[1..ndata], y[1..ndata] with individual standard deviations sig[1..ndata], and a nonlinear function
         * dependent on ma coefficients a[1..ma].  The input array ia[1..ma] indicates by nozero entries those
         * components of a that should be fitted for, and by zero entries those components that should be held fixed at
         * their input values.  The program returns the current best-fit values for the parameters a[1..ma] and chisq.
         * The arrays covar[1..ma][1..ma], alpha[1..ma][1..ma] are used as working space during most iterations.  mrqcof
         * calls fitLine(x1,atry,dyda), fitExponential(x1,atry,dyda), or some other fitting function. mrqcof suplies the
         * x value x1 and the matrix with the current best guess of the parameters atry[].  function calulates the
         * derivatives dyda[1..ma] with respect to the fitting parameters atry at x.  function also returns the y value
         * ymod generated by these parameters.    The first call to mrqmin sets alamda equal to -1 for initialization.
         * The routine then sets alamda = .001.  If a step succeeds chisq becomes smaller and alamda decreases by a
         * factor of 10.  If a step fails alamda grows by a factor of 10.  The routine is called repeatedly until
         * convergence is achieved.  Then, a final call with alamda = 0 is made to generate the covariance matrix
         * covar[1..ma][1..ma] and the curvature matrix alpha[1..ma][1..ma]. */

        int j, k, l;

        try {

            if (alamda < 0.0) {

                // System.out.println("start");
                alamda = ALAMDA;
                mrqcof(a, alpha, beta);
                ochisq2 = chisq;

                for (j = 0; j < ma; j++) {
                    atry[j] = a[j];
                }
            } // end of alambda < 0 condition

            for (j = 0; j < mfit; j++) {

                for (k = 0; k < mfit; k++) {
                    covar[j][k] = alpha[j][k];
                }

                covar[j][j] = alpha[j][j] * (1.0 + (alamda));
                oneda[j][0] = beta[j];
            }

            gaussj(covar, mfit, oneda, 1);

            for (j = 0; j < mfit; j++) {
                da[j] = oneda[j][0];
            }

            if (alamda == 0.0) {
                covsrt(covar);
                covsrt(alpha);

                return;
            }

            for (j = 0, l = 0; l < ma; l++) {

                if (ia[l] != 0) {
                    atry[l] = a[l] + da[j++];
                }
            }

            // System.out.println("atry[0] = " + atry[0]);
            // System.out.println("atry[1] = " + atry[1]);
            // System.out.println("atry[2] = " + atry[2]);
            mrqcof(atry, covar, da);

            if (chisq < ochisq2) {
                alamda *= 0.1;
                ochisq2 = chisq;

                for (j = 0; j < mfit; j++) {

                    for (k = 0; k < mfit; k++) {
                        alpha[j][k] = covar[j][k];
                    }

                    beta[j] = da[j];
                }

                for (l = 0; l < ma; l++) {
                    a[l] = atry[l];
                }
            } else {
                alamda *= 10.0;
                chisq = ochisq2;
            }

            return;
        } catch (Exception e) {
            Preferences.debug("mrqmin error: " + e.getMessage());
        }
    }

}
