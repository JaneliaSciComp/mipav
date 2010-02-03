package gov.nih.mipav.model.algorithms;


import gov.nih.mipav.view.*;


/**
 * DOCUMENT ME!
 */
public abstract class Integration {

    //~ Static fields/initializers -------------------------------------------------------------------------------------

    /**
     * TRAPZD is for sufficiently smooth(analytic) integrands, integrated over intervals which contain no singularities,
     * and where the endpoints are also nonsingular.
     */
    protected static final int TRAPZD = 1;

    /**
     * The 5 choices below are used for improper integrals MIDPNT was found to have poorer convergence than the other
     * improper integral routines. Note that the Numerical Recipes Example Book uses 1.0E20 as infinity in its sample
     * programs and the Numercial Recipes Book uses 1.0E30 as infinity. If you need to integrate from a negative lower
     * limit to positive infinity, you do this by breaking the integral into two pieces at some positive value. Where
     * should you choose the breakpoint? At a sufficiently large positive value so that the integrand is at least
     * beginning to approach its asymptotic decrease to zero value at infinity. MIDPNT does not require the integrand to
     * be evaluated at the endpoints An example is the integral of the Bessel function Y0(x) from x = 0.0 to x = 2.0
     */
    protected static final int MIDPNT = 2;

    /**
     * MIDINF can be used either if: 1.) The lower limit is positive and the upper limit approaches positive infinity.
     * For example, the integral of sin(x)/x**2 from PI/2 to infinity. It is quite slowly convergent. Another example is
     * the integral of exp(-x)/sqrt(x) from PI/2 to infinity. OR: 2.) The lower limit approaches negative infinity and
     * the upper limit is negative. For example, the integral of sin(x)/x**2 from -infinity to -PI/2. Use very large
     * numbers rather than infinities on your machine
     */
    protected static final int MIDINF = 3;

    /**
     * MIDSQL can be used if there is an inverse square root singularity of the integrand at the lower limit of
     * integration. For example, the integral of sqrt(x)/sin(x) from 0.0 to PI/2 has a 1/sqrt(x) singularity at x = 0.
     * Another example is the integral of exp(-x)/sqrt(x) from 0.0 to PI/2. It has a singularity at x = 0.0.
     */
    protected static final int MIDSQL = 4;

    /**
     * MIDSQU can be used if there is an inverse square root singularity of the integrand at the upper limit of
     * integration. For example, the integral of sqrt(PI - x)/sin(x) from PI/2 to PI has a 1/sqrt(x) singularity at the
     * upper limit x = PI
     */
    protected static final int MIDSQU = 5;

    /**
     * MIDEXP can be used when the upper limit of integration is infinite and the integrand decreases exponentially at
     * infinity. For example, the integral of exp(-x)/sqrt(x) from PI/2 to infinity.
     */
    protected static final int MIDEXP = 6;

    //~ Instance fields ------------------------------------------------------------------------------------------------

    /** DOCUMENT ME! */
    private double eps = 1.0e-8;

    /** DOCUMENT ME! */
    private double[] h;

    /** DOCUMENT ME! */
    private int j;

    /** DOCUMENT ME! */
    private int jmax;

    /** DOCUMENT ME! */
    private int jmaxp;

    /** DOCUMENT ME! */
    private int k = 5;

    /** DOCUMENT ME! */
    private double lower;

    /** DOCUMENT ME! */
    private int routine;

    /** DOCUMENT ME! */
    private double[] s;

    /** DOCUMENT ME! */
    private double ss;

    /** DOCUMENT ME! */
    protected double upper;

    //~ Constructors ---------------------------------------------------------------------------------------------------

    /**
     * This is a port of numerical integration routines found in Chapter 4 Integration of Functions of the book
     * Numerical Recipes in C The Art of Scientific Computing Second Edition by William H. Press, Saul A. Teukolsky,
     * William T. Vetterling, and Brian P. Flannery, 1997. Code was also ported from the routine polint in Chapter 3.1
     * Polynomial Interpolation and Extrapolation.
     *
     * @param  lower    DOCUMENT ME!
     * @param  upper    DOCUMENT ME!
     * @param  routine  DOCUMENT ME!
     */

    /**
     * Example used to test TRAPZD: private void runIntegrationTest() { // Run test integrating x*x*(x*x -
     * 2.0)*Math.sin(x) from 0.0 to Math.PI/2.0 IntModel imod; int steps; double numInt; double actualInt; double eps =
     * 1.0e-10; imod = new IntModel(0.0,Math.PI/2.0,Integration.TRAPZD,eps); imod.driver(); steps = imod.getStepsUsed();
     * numInt = imod.getIntegral(); Preferences.debug("Numerical Integral = " + numInt + " after " + steps + " steps
     * used\n"); actualInt = actInt(Math.PI/2.0) - actInt(0.0); Preferences.debug("Actual Integral = " + actualInt); }
     * private double actInt(double x) { return (4.0*x*(x*x - 7.0)*Math.sin(x) - (x*x*x*x - 14.0*x*x +
     * 28.0)*Math.cos(x)); } class IntModel extends Integration { public IntModel(double lower, double upper, int
     * routine, double eps) { super(lower, upper, routine, eps); } public double intFunc(double x) { return (x*x*(x*x -
     * 2.0)*Math.sin(x)); } public void driver() { super.driver(); } }.
     *
     * @param  lower    DOCUMENT ME!
     * @param  upper    DOCUMENT ME!
     * @param  routine  DOCUMENT ME!
     */


    /**
     * Constructor for Integration.
     *
     * @param  lower    DOCUMENT ME!
     * @param  upper    DOCUMENT ME!
     * @param  routine  DOCUMENT ME!
     */
    public Integration(double lower, double upper, int routine) {

        try {
            this.lower = lower;
            this.upper = upper;
            this.routine = routine;
        } catch (OutOfMemoryError error) { }
    }

    /**
     * Constructor for Integration.
     *
     * @param  lower    DOCUMENT ME!
     * @param  upper    DOCUMENT ME!
     * @param  routine  DOCUMENT ME!
     * @param  eps      DOCUMENT ME!
     */
    public Integration(double lower, double upper, int routine, double eps) {

        try {
            this.lower = lower;
            this.upper = upper;
            this.routine = routine;
            this.eps = eps;
        } catch (OutOfMemoryError error) { }
    }

    //~ Methods --------------------------------------------------------------------------------------------------------

    /**
     * DOCUMENT ME!
     *
     * @param   x  DOCUMENT ME!
     *
     * @return  DOCUMENT ME!
     */
    public abstract double intFunc(double x);


    /**
     * driver.
     */
    public void driver() {
        double dif;
        int ns;
        int i;
        double dift;
        int m;
        double ho;
        double hp;
        double w;
        double den;
        double dss;

        try {

            if (routine == TRAPZD) {
                jmax = 100;
            } // if (routine == TRAPZD)
            else {
                jmax = 100;
            }

            jmaxp = jmax + 1;
            h = new double[jmaxp + 1];
            s = new double[jmaxp];

            double[] c = new double[k];
            double[] d = new double[k];

            h[0] = 1.0;

            for (j = 1; j <= jmax; j++) {

                switch (routine) {

                    case TRAPZD:
                        trapzd();
                        break;

                    case MIDPNT:
                        midpnt();
                        break;

                    case MIDINF:
                        midinf();
                        break;

                    case MIDSQL:
                        midsql();
                        break;

                    case MIDSQU:
                        midsqu();
                        break;

                    case MIDEXP:
                        midexp();
                        break;
                } // switch

                if (j >= k) {
                    ns = 1;
                    dss = 0.0;
                    dif = Math.abs(-h[j - k]);

                    for (i = 0; i < k; i++) {
                        c[i] = 0.0;
                        d[i] = 0.0;
                    }

                    for (i = 1; i <= k; i++) {
                        dift = Math.abs(-h[j - k - 1 + i]);

                        if (dift < dif) {
                            ns = i;
                            dif = dift;
                        } // if (dift < dif)

                        c[i - 1] = s[j - k - 1 + i];
                        d[i - 1] = s[j - k - 1 + i];
                    } // for (i = 1; i <= k; i++)

                    ss = s[j - k + ns - 1];
                    ns--;

                    for (m = 1; m < k; m++) {

                        for (i = 1; i <= (k - m); i++) {
                            ho = h[j - k - 1 + i];
                            hp = h[j - k - 1 + i + m];
                            w = c[i] - d[i - 1];
                            den = ho - hp;

                            if (den == 0.0) {
                                MipavUtil.displayError("Integration driver error - 2 input h's are identical");

                                return;
                            } // if (den == 0.0)

                            den = w / den;
                            d[i - 1] = hp * den;
                            c[i - 1] = ho * den;
                        } // for (i = 1; i <= k-m; i++)

                        if ((2 * ns) < (k - m)) {
                            dss = c[ns];
                        } else {
                            dss = d[ns - 1];
                            ns--;
                        }

                        ss += dss;
                    } // for (m = 1; m < k; m++)

                    // Preferences.debug("ss = " + ss + " dss = " + dss + "\n");
                    if (Math.abs(dss) <= (eps * Math.abs(ss))) {
                        return;
                    }
                } // if (j >= k)

                if (routine == TRAPZD) {
                    h[j] = 0.25 * h[j - 1];
                } // if (routine == TRAPZD)
                else {
                    h[j] = h[j - 1] / 9.0;
                }
            } // for (j = 1; j <= jmax; j++)

            MipavUtil.displayError("More than maximum " + jmax + "steps used");
            ss = 0.0;

            return;
        } // try
        catch (Exception err) {
            Preferences.debug("driver error: " + err.getMessage());
        }
    } // driver

    /**
     * DOCUMENT ME!
     *
     * @return  DOCUMENT ME!
     */
    public double getIntegral() {
        return ss;
    }

    /**
     * DOCUMENT ME!
     *
     * @return  DOCUMENT ME!
     */
    public int getStepsUsed() {
        return j;
    }

    /**
     * DOCUMENT ME!
     */
    private void midexp() {
        int it;
        int m;
        double tnm;
        double del;
        double ddel;
        double x;
        double sum;
        double a;
        double b;

        b = Math.exp(-lower);

        if (j == 1) {
            s[j - 1] = 2.0 * intFunc(-Math.log(0.5 * b));
        } // if (j == 1)
        else {

            for (it = 1, m = 1; m < (j - 1); m++) {
                it *= 3;
            } // (for it = 1, m = 1; m < j-1; m++)

            tnm = it;
            del = b / (3.0 * tnm);
            ddel = del + del;
            x = 0.5 * del;
            sum = 0.0;

            for (m = 1; m <= it; m++) {
                sum += intFunc(-Math.log(x)) / x;
                x += ddel;
                sum += intFunc(-Math.log(x)) / x;
                x += del;
            } // for (m = 1; m <= it; m++)

            s[j - 1] = (s[j - 2] + (b * sum / tnm)) / 3.0;
        }

        return;

    }

    /**
     * DOCUMENT ME!
     */
    private void midinf() {
        int it;
        int m;
        double tnm;
        double del;
        double ddel;
        double x;
        double sum;
        double a;
        double b;
        double arg;

        b = 1.0 / lower;
        a = 1.0 / upper;

        if (j == 1) {
            arg = 0.5 * (a + b);
            s[j - 1] = (b - a) * intFunc(1.0 / arg) / (arg * arg);
        } // if (j == 1)
        else {

            for (it = 1, m = 1; m < (j - 1); m++) {
                it *= 3;
            } // (for it = 1, m = 1; m < j-1; m++)

            tnm = it;
            del = (b - a) / (3.0 * tnm);
            ddel = del + del;
            x = a + (0.5 * del);
            sum = 0.0;

            for (m = 1; m <= it; m++) {
                sum += intFunc(1.0 / x) / (x * x);
                x += ddel;
                sum += intFunc(1.0 / x) / (x * x);
                x += del;
            } // for (m = 1; m <= it; m++)

            s[j - 1] = (s[j - 2] + ((b - a) * sum / tnm)) / 3.0;
        }

        return;
    }

    /**
     * DOCUMENT ME!
     */
    private void midpnt() {
        int it;
        int m;
        double tnm;
        double del;
        double ddel;
        double x;
        double sum;

        if (j == 1) {
            s[j - 1] = (upper - lower) * intFunc(0.5 * (lower + upper));
        } // if (j == 1)
        else {

            for (it = 1, m = 1; m < (j - 1); m++) {
                it *= 3;
            } // (for it = 1, m = 1; m < j-1; m++)

            tnm = it;
            del = (upper - lower) / (3.0 * tnm);
            ddel = del + del;
            x = lower + (0.5 * del);
            sum = 0.0;

            for (m = 1; m <= it; m++) {
                sum += intFunc(x);
                x += ddel;
                sum += intFunc(x);
                x += del;
            } // for (m = 1; m <= it; m++)

            s[j - 1] = (s[j - 2] + ((upper - lower) * sum / tnm)) / 3.0;
        } // else

        return;
    }

    /**
     * DOCUMENT ME!
     */
    private void midsql() {
        int it;
        int m;
        double tnm;
        double del;
        double ddel;
        double x;
        double sum;
        double a;
        double b;

        b = Math.sqrt(upper - lower);

        if (j == 1) {
            s[j - 1] = b * b * intFunc(lower + (0.25 * b * b));
        } // if (j == 1)
        else {

            for (it = 1, m = 1; m < (j - 1); m++) {
                it *= 3;
            } // (for it = 1, m = 1; m < j-1; m++)

            tnm = it;
            del = b / (3.0 * tnm);
            ddel = del + del;
            x = 0.5 * del;
            sum = 0.0;

            for (m = 1; m <= it; m++) {
                sum += 2.0 * x * intFunc(lower + (x * x));
                x += ddel;
                sum += 2.0 * x * intFunc(lower + (x * x));
                x += del;
            } // for (m = 1; m <= it; m++)

            s[j - 1] = (s[j - 2] + (b * sum / tnm)) / 3.0;
        }

        return;
    }

    /**
     * DOCUMENT ME!
     */
    private void midsqu() {
        int it;
        int m;
        double tnm;
        double del;
        double ddel;
        double x;
        double sum;
        double a;
        double b;

        b = Math.sqrt(upper - lower);

        if (j == 1) {
            s[j - 1] = b * b * intFunc(upper - (0.25 * b * b));
        } // if (j == 1)
        else {

            for (it = 1, m = 1; m < (j - 1); m++) {
                it *= 3;
            } // (for it = 1, m = 1; m < j-1; m++)

            tnm = it;
            del = b / (3.0 * tnm);
            ddel = del + del;
            x = 0.5 * del;
            sum = 0.0;

            for (m = 1; m <= it; m++) {
                sum += 2.0 * x * intFunc(upper - (x * x));
                x += ddel;
                sum += 2.0 * x * intFunc(upper - (x * x));
                x += del;
            } // for (m = 1; m <= it; m++)

            s[j - 1] = (s[j - 2] + (b * sum / tnm)) / 3.0;
        }

        return;
    }

    /**
     * DOCUMENT ME!
     */
    private void trapzd() {
        int it;
        int m;
        double tnm;
        double del;
        double ddel;
        double x;
        double sum;

        if (j == 1) {
            s[j - 1] = 0.5 * (upper - lower) * (intFunc(lower) + intFunc(upper));
        } // if (j == 1)
        else {

            for (it = 1, m = 1; m < (j - 1); m++) {
                it *= 2;
            } // (for it = 1, m = 1; m < j-1; m++)

            tnm = it;
            del = (upper - lower) / tnm;
            x = lower + (0.5 * del);
            sum = 0.0;

            for (m = 1; m <= it; m++) {
                sum += intFunc(x);
                x += del;
            } // for (m = 1; m <= it; m++)

            s[j - 1] = 0.5 * (s[j - 2] + ((upper - lower) * sum / tnm));
        } // else

        return;

    }
}
