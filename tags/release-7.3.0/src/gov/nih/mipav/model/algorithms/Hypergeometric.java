package gov.nih.mipav.model.algorithms;


import gov.nih.mipav.view.MipavUtil;


/**
 * <p>
 * The function <code>2F1(a,b,c,x)</code> is the hypergeometric function or Gauss's hypergeometric function.
 * </p>
 * 
 * <p>
 * The <code>Pochhammer(a,b) = Gamma(a+b)/Gamma(a)</code>
 * <code>2F1(a,b,c,x) = sum</code> from <code>k = 0</code>
 * to infinity of <code>Poch(a,k)*Poch(b,k)*z**k/(k! * Poch(c,k))</code>
 * </p>
 * 
 * <p>
 * It is the solution of the hypergeometric differential equation: <code>z(1-z)y" + [c-(a+b+1)z]y' - aby = 0</code>
 * </p>
 * 
 * <hr>
 * 
 * <p>
 * The FORTRAN code this class is based upon is from Computation of Special Functions by Shanjie Zhang and Jianming Jin
 * and copyright 1996 John Wiley &amp; Sons, Inc.
 * </p>
 * 
 * <p>
 * From the diskette that the FORTRAN code came on: <blockquote>
 * 
 * <pre>
 * DISCLAIMER OF WARRANTY
 * 
 * Although we have made a great effort to test and validate the 
 * computer programs, we make no warranties, express or implied, that 
 * these  programs  are  free  of  error,  or are consistent with any 
 * particular  standard  of  merchantability,  or that they will meet 
 * your requirements for any particular application.  They should not 
 * be relied on for  solving problems  whose incorrect solution could 
 * result in  injury to  a person or loss of property.  If you do use 
 * the programs in such a manner, it is at your own risk. The authors 
 * and publisher  disclaim all liability for  direct or consequential 
 * damages resulting from your use of the programs.
 * </pre>
 * 
 * </blockquote>
 * </p>
 */
public class Hypergeometric {
    // ~ Static fields/initializers
    // -------------------------------------------------------------------------------------

    public static final int REAL_VERSION = 1;

    public static final int COMPLEX_VERSION = 2;

    /** Tells whether real number or complex number version */
    private final int version;

    /** Input parameter */
    private double a;

    /** Input parameter */
    private double b;

    /** Input argument */
    private double c;

    /** Outputted result */
    private double result[];

    /** Input argument */
    private double x;

    /** Input argument */
    private double realZ;

    private double imagZ;

    /** outputted result */
    private double realResult[];

    private double imagResult[];

    // ~ Constructors
    // ---------------------------------------------------------------------------------------------------

    /**
     * @param a input parameter
     * @param b input parameter
     * @param c input parameter c must not equal 0, -1, -2, ...
     * @param x input argument
     * @param result outputted result
     */
    public Hypergeometric(final double a, final double b, final double c, final double x, final double result[]) {
        this.a = a;
        this.b = b;
        this.c = c;
        this.x = x;
        this.result = result;
        this.version = Hypergeometric.REAL_VERSION;
    }

    /**
     * @param a input real parameter
     * @param b input real parameter
     * @param c input real parameter c must not equal 0, -1, -2, ...
     * @param realZ input real part of argument
     * @param imagZ input imaginary part of argument
     * @param realResult real part of outputted result
     * @param imagResult imaginary part of outputted result
     */
    public Hypergeometric(final double a, final double b, final double c, final double realZ, final double imagZ,
            final double realResult[], final double imagResult[]) {
        this.a = a;
        this.b = b;
        this.c = c;
        this.realZ = realZ;
        this.imagZ = imagZ;
        this.realResult = realResult;
        this.imagResult = imagResult;
        this.version = Hypergeometric.COMPLEX_VERSION;
    }

    // ~ Methods
    // --------------------------------------------------------------------------------------------------------

    /**
     * Cleanup memory.
     * 
     * @throws Throwable DOCUMENT ME!
     */
    public void finalize() throws Throwable {

        super.finalize();
    }

    public void run() {
        if (version == Hypergeometric.REAL_VERSION) {
            realArgument();
        }

        else if (version == Hypergeometric.COMPLEX_VERSION) {
            complexArgument();
        } else {
            MipavUtil.displayError("Illegal argument");
            return;
        }
    } // run()

    /**
     * This is a port of subroutine HYGFX from Computation of Special Functions by Shanjie Zhang and Jianming Jin, pp.
     * 376-379.
     * 
     */
    private void realArgument() {
        boolean L0, L1, L2, L3, L4, L5;
        final double el = .5772156649015329;
        double eps;
        final double gc[] = new double[1];
        final double gcab[] = new double[1];
        final double gca[] = new double[1];
        final double gcb[] = new double[1];
        double g0;
        final double g1[] = new double[1];
        final double g2[] = new double[1];
        final double g3[] = new double[1];
        int nm = 0;
        double r;
        int k;
        double aa;
        double bb;
        double x1;
        double gm;
        int m;
        final double ga[] = new double[1];
        final double gb[] = new double[1];
        final double gam[] = new double[1];
        final double gbm[] = new double[1];
        final double pa[] = new double[1];
        final double pb[] = new double[1];
        int j;
        double rm;
        double f0;
        double r0;
        double r1;
        double sp0;
        double sp;
        double c0;
        double c1;
        double f1;
        double sm;
        double rp;
        double hw = 0.0;
        final double gabc[] = new double[1];
        double a0;
        Gamma gamm;
        Psi psi;

        L0 = ( (c == (int) c) && (c < 0.0));
        L1 = ( ( (1.0 - x) < 1.0E-15) && (c - a - b <= 0.0));
        L2 = ( (a == (int) a) && (a < 0.0));
        L3 = ( (b == (int) b) && (b < 0.0));
        L4 = ( ( (c - a) == (int) (c - a)) && (c - a <= 0.0));
        L5 = ( ( (c - b) == (int) (c - b)) && (c - b <= 0.0));
        if (L0 || L1) {
            MipavUtil.displayError("The hypergeometric series is divergent");
            return;
        }
        eps = 1.0E-15;
        if (x > 0.95) {
            eps = 1.0E-8;
        }
        if ( (x == 0.0) || (a == 0.0) || (b == 0.0)) {
            result[0] = 1.0;
            return;
        } // if ((x == 0.0) || (a == 0.0) || (b == 0.0))
        else if ( ( (1.0 - x) == eps) && (c - a - b > 0.0)) {
            // For c > a + b calculate F(a,b,c,1) using
            // F(a,b,c,1) = Gamma(c)*Gamma(c-a-b)/(Gamma(c-a)*Gamma(c-b))
            gamm = new Gamma(c, gc);
            gamm.run();
            gamm = new Gamma(c - a - b, gcab);
            gamm.run();
            gamm = new Gamma(c - a, gca);
            gamm.run();
            gamm = new Gamma(c - b, gcb);
            gamm.run();
            result[0] = gc[0] * gcab[0] / (gca[0] * gcb[0]);
            return;
        } // else if (((1.0-x) == eps) && (c-a-b > 0.0))
        else if ( ( (1.0 + x) <= eps) && (Math.abs(c - a + b - 1.0) <= eps)) {
            // For c = a + 1 - b calculate F(a,b,c,-1) using
            // F(a,b,a+1-b,-1) = Gamma(a-b+1)*Gamma(a/2 + 1)/(Gamma(a+1)*Gamma(1 - b + a/2))
            g0 = Math.sqrt(Math.PI) * Math.pow(2.0, -a);
            gamm = new Gamma(c, g1);
            gamm.run();
            gamm = new Gamma(1.0 + a / 2.0 - b, g2);
            gamm.run();
            gamm = new Gamma(0.5 + 0.5 * a, g3);
            gamm.run();
            result[0] = g0 * g1[0] / (g2[0] * g3[0]);
            return;
        } // else if (((1.0 + x) <= eps) && (Math.abs(c-a+b-1.0) <= eps))
        else if (L2 || L3) {
            if (L2) {
                nm = (int) Math.abs(a);
            }
            if (L3) {
                nm = (int) Math.abs(b);
            }
            result[0] = 1.0;
            r = 1.0;
            for (k = 1; k <= nm; k++) {
                r = r * (a + k - 1.0) * (b + k - 1.0) / (k * (c + k - 1.0)) * x;
                result[0] = result[0] + r;
            }
            return;
        } // else if (L2 || L3)
        else if (L4 || L5) {
            if (L4) {
                nm = (int) Math.abs(c - a);
            }
            if (L5) {
                nm = (int) Math.abs(c - b);
            }
            result[0] = 1.0;
            r = 1.0;
            for (k = 1; k <= nm; k++) {
                r = r * (c - a + k - 1.0) * (c - b + k - 1.0) / (k * (c + k - 1.0)) * x;
                result[0] = result[0] + r;
            }
            result[0] = Math.pow( (1.0 - x), (c - a - b)) * result[0];
            return;
        } // else if (L4 || L5)
        aa = a;
        bb = b;
        x1 = x;
        if (x < 0.0) {
            x = x / (x - 1.0);
            if ( (c > a) && (b < a) && (b > 0.0)) {
                a = bb;
                b = aa;
            }
            b = c - b;
        } // if (x < 0.0)
        if (x >= 0.75) {
            gm = 0.0;
            if (Math.abs(c - a - b - (int) (c - a - b)) < 1.0E-15) {
                m = (int) (c - a - b);
                gamm = new Gamma(a, ga);
                gamm.run();
                gamm = new Gamma(b, gb);
                gamm.run();
                gamm = new Gamma(c, gc);
                gamm.run();
                gamm = new Gamma(a + m, gam);
                gamm.run();
                gamm = new Gamma(b + m, gbm);
                gamm.run();
                psi = new Psi(a, pa);
                psi.run();
                psi = new Psi(b, pb);
                psi.run();
                if (m != 0) {
                    gm = 1.0;
                }
                for (j = 1; j <= Math.abs(m) - 1; j++) {
                    gm = gm * j;
                }
                rm = 1.0;
                for (j = 1; j <= Math.abs(m); j++) {
                    rm = rm * j;
                }
                f0 = 1.0;
                r0 = 1.0;
                r1 = 1.0;
                sp0 = 0.0;
                sp = 0.0;
                if (m >= 0) {
                    c0 = gm * gc[0] / (gam[0] * gbm[0]);
                    c1 = -gc[0] * Math.pow( (x - 1.0), m) / (ga[0] * gb[0] * rm);
                    for (k = 1; k <= m - 1; k++) {
                        r0 = r0 * (a + k - 1) * (b + k - 1) / (k * (k - m)) * (1.0 - x);
                        f0 = f0 + r0;
                    } // for (k = 1; k <= m-1; k++)
                    for (k = 1; k <= m; k++) {
                        sp0 = sp0 + 1.0 / (a + k - 1) + 1.0 / (b + k - 1) - 1.0 / k;
                    } // for (k = 1; k <= m; k++)
                    f1 = pa[0] + pb[0] + sp0 + 2.0 * el + Math.log(1.0 - x);
                    for (k = 1; k <= 250; k++) {
                        sp = sp + (1 - a) / (k * (a + k - 1)) + (1 - b) / (k * (b + k - 1));
                        sm = 0.0;
                        for (j = 1; j <= m; j++) {
                            sm = sm + (1 - a) / ( (j + k) * (a + j + k - 1)) + 1 / (b + j + k - 1);
                        } // for (j = 1; j <= m; j++)
                        rp = pa[0] + pb[0] + 2.0 * el + sp + sm + Math.log(1.0 - x);
                        r1 = r1 * (a + m + k - 1) * (b + m + k - 1) / (k * (m + k)) * (1.0 - x);
                        f1 = f1 + r1 * rp;
                        if (Math.abs(f1 - hw) < Math.abs(f1) * eps) {
                            break;
                        }
                        hw = f1;
                    } // for (k = 1; k <= 250; k++)
                    result[0] = f0 * c0 + f1 * c1;
                } // if (m >= 0)
                else { // m < 0
                    m = -m;
                    c0 = gm * gc[0] / (ga[0] * gb[0] * Math.pow( (1.0 - x), m));
                    c1 = -Math.pow( -1, m) * gc[0] / (gam[0] * gbm[0] * rm);
                    for (k = 1; k <= m - 1; k++) {
                        r0 = r0 * (a - m + k - 1) * (b - m + k - 1) / (k * (k - m)) * (1.0 - x);
                        f0 = f0 + r0;
                    } // for (k = 1; k <= m-1; k++)
                    for (k = 1; k <= m; k++) {
                        sp0 = sp0 + 1.0 / k;
                    } // for (k = 1; k <= m; k++)
                    f1 = pa[0] + pb[0] - sp0 + 2.0 * el + Math.log(1.0 - x);
                    for (k = 1; k <= 250; k++) {
                        sp = sp + (1 - a) / (k * (a + k - 1)) + (1 - b) / (k * (b + k - 1));
                        sm = 0.0;
                        for (j = 1; j <= m; j++) {
                            sm = sm + 1.0 / (j + k);
                        } // for (j = 1; j <= m; j++)
                        rp = pa[0] + pb[0] + 2.0 * el + sp - sm + Math.log(1.0 - x);
                        r1 = r1 * (a + k - 1) * (b + k - 1) / (k * (m + k)) * (1.0 - x);
                        f1 = f1 + r1 * rp;
                        if (Math.abs(f1 - hw) < Math.abs(f1) * eps) {
                            break;
                        }
                        hw = f1;
                    } // for (k = 1; k <= 250; k++)
                    result[0] = f0 * c0 + f1 * c1;
                } // else m < 0
            } // if (Math.abs(c - a - b - (int)(c - a - b)) < 1.0E-15)
            else { // c - a - b is not an integer
                gamm = new Gamma(a, ga);
                gamm.run();
                gamm = new Gamma(b, gb);
                gamm.run();
                gamm = new Gamma(c, gc);
                gamm.run();
                gamm = new Gamma(c - a, gca);
                gamm.run();
                gamm = new Gamma(c - b, gcb);
                gamm.run();
                gamm = new Gamma(c - a - b, gcab);
                gamm.run();
                gamm = new Gamma(a + b - c, gabc);
                gamm.run();
                c0 = gc[0] * gcab[0] / (gca[0] * gcb[0]);
                c1 = gc[0] * gabc[0] / (ga[0] * gb[0]) * Math.pow( (1.0 - x), (c - a - b));
                result[0] = 0.0;
                r0 = c0;
                r1 = c1;
                for (k = 1; k <= 250; k++) {
                    r0 = r0 * (a + k - 1) * (b + k - 1) / (k * (a + b - c + k)) * (1.0 - x);
                    r1 = r1 * (c - a + k - 1) * (c - b + k - 1) / (k * (c - a - b + k)) * (1.0 - x);
                    result[0] = result[0] + r0 + r1;
                    if (Math.abs(result[0] - hw) < Math.abs(result[0]) * eps) {
                        break;
                    }
                    hw = result[0];
                } // for (k = 1; k <= 250; k++)
                result[0] = result[0] + c0 + c1;
            } // else c - a - b is not an integer
        } // if (x >= 0.75)
        else { // 0.0 <= x < 0.75
            a0 = 1.0;
            if ( ( (c - a) < a) && ( (c - b) < b)) {
                a0 = Math.pow( (1.0 - x), (c - a - b));
                a = c - a;
                b = c - b;
            } // if (((c-a) < a) && ((c-b) < b))
            result[0] = 1.0;
            r = 1.0;
            for (k = 1; k <= 250; k++) {
                r = r * (a + k - 1.0) * (b + k - 1.0) / (k * (c + k - 1.0)) * x;
                result[0] = result[0] + r;
                if (Math.abs(result[0] - hw) <= Math.abs(result[0]) * eps) {
                    break;
                }
                hw = result[0];
            } // for (k = 1; k <= 250; k++)
            result[0] = a0 * result[0];
        } // else 0.0 <= x < 0.75
        if (x1 < 0.0) {
            x = x1;
            c0 = 1.0 / Math.pow( (1.0 - x), aa);
            result[0] = c0 * result[0];
        } // if (x1 < 0.0)
        a = aa;
        b = bb;
        if (k > 120) {
            // When convergence is slow
            MipavUtil.displayWarning("Hypergeometric warning! Check the accuracy!");
        }
        return;
    } // realArgument

    /**
     * This is a port of subroutine HYGFZ from Computation of Special Functions by Shanjie Zhang and Jianming Jin, pp.
     * 380-385.
     * 
     */
    private void complexArgument() {
        boolean L0, L1, L2, L3, L4, L5, L6;
        double x;
        double y;
        double eps = 1.0E-15;
        double aa;
        double bb;
        double a0;
        final double el = .5772156649015329;
        Gamma gamm;
        final double gc[] = new double[1];
        final double gcab[] = new double[1];
        final double gca[] = new double[1];
        final double gcb[] = new double[1];
        final double g0[] = new double[1];
        final double g1[] = new double[1];
        final double g2[] = new double[1];
        final double g3[] = new double[1];
        int nm = 0;
        final double realZR[] = new double[1];
        final double imagZR[] = new double[1];
        int k = 0;
        final double realZ1[] = new double[1];
        final double imagZ1[] = new double[1];
        final double realZC0[] = new double[1];
        final double imagZC0[] = new double[1];
        final double realZR0[] = new double[1];
        final double imagZR0[] = new double[1];
        double realZW = 0.0;
        double imagZW = 0.0;
        final double gm[] = new double[1];
        int mcab;
        int m;
        final double gam[] = new double[1];
        final double gbm[] = new double[1];
        Psi psi;
        final double pa[] = new double[1];
        final double pb[] = new double[1];
        int j;
        double rm;
        double realZF0;
        double imagZF0;
        final double realZR1[] = new double[1];
        final double imagZR1[] = new double[1];
        double sp0;
        double sp;
        final double realZC1[] = new double[1];
        final double imagZC1[] = new double[1];
        final double realZF1[] = new double[1];
        final double imagZF1[] = new double[1];
        double realZP;
        double imagZP;
        final double ga[] = new double[1];
        final double gb[] = new double[1];
        final double gabc[] = new double[1];
        final double realZ00[] = new double[1];
        final double imagZ00[] = new double[1];
        int mab;
        final double gab[] = new double[1];
        final double gba[] = new double[1];
        double ca;
        double cb;
        int nca;
        int ncb;
        final double pca[] = new double[1];
        final double pac[] = new double[1];
        double t0;
        final double gcbk[] = new double[1];
        double sq;
        double rk1;
        double sj1;
        double rk2;
        double sj2;
        double ws;
        double w0 = 0.0;
        final int ierr[] = new int[1];
        final double realTemp[] = new double[1];
        final double imagTemp[] = new double[1];
        final double realTemp2[] = new double[1];
        final double imagTemp2[] = new double[1];
        double sm;
        double realZP0;
        double imagZP0;

        x = realZ;
        y = imagZ;
        L0 = ( (c == (int) c) && (c < 0.0));
        L1 = ( (Math.abs(1.0 - x) < eps) && (y == 0.0) && ( (c - a - b) <= 0.0));
        L2 = ( (zabs(realZ + 1.0, imagZ) < eps) && (Math.abs(c - a + b - 1.0) < eps));
        L3 = ( (a == (int) a) && (a < 0.0));
        L4 = ( (b == (int) b) && (b < 0.0));
        L5 = ( ( (c - a) == (int) (c - a)) && ( (c - a) <= 0.0));
        L6 = ( ( (c - b) == (int) (c - b)) && ( (c - b) <= 0.0));
        aa = a;
        bb = b;
        a0 = zabs(realZ, imagZ);
        if (a0 > 0.95) {
            eps = 1.0E-8;
        }
        if (L0 || L1) {
            MipavUtil.displayError("The hypergeometric series is divergent");
            return;
        } // if (L0 || L1)
        if ( (a0 == 0.0) || (a == 0.0) || (b == 0.0)) {
            realResult[0] = 1.0;
            imagResult[0] = 0.0;
        } // if ((a0 == 0.0) || (a == 0.0) || (b == 0.0))
        else if ( (realZ == 1.0) && (imagZ == 0.0) && ( (c - a - b) > 0.0)) {
            gamm = new Gamma(c, gc);
            gamm.run();
            gamm = new Gamma(c - a - b, gcab);
            gamm.run();
            gamm = new Gamma(c - a, gca);
            gamm.run();
            gamm = new Gamma(c - b, gcb);
            gamm.run();
            realResult[0] = gc[0] * gcab[0] / (gca[0] * gcb[0]);
            imagResult[0] = 0.0;
        } // else if ((realZ == 1.0) && (imagZ == 0.0) && ((c-a-b) > 0.0))
        else if (L2) {
            g0[0] = Math.sqrt(Math.PI) * Math.pow(2.0, -a);
            gamm = new Gamma(c, g1);
            gamm.run();
            gamm = new Gamma(1.0 + a / 2.0 - b, g2);
            gamm.run();
            gamm = new Gamma(0.5 + 0.5 * a, g3);
            gamm.run();
            realResult[0] = g0[0] * g1[0] / (g2[0] * g3[0]);
            imagResult[0] = 0.0;
        } // else if (L2)
        else if (L3 || L4) {
            if (L3) {
                nm = (int) Math.abs(a);
            }
            if (L4) {
                nm = (int) Math.abs(b);
            }
            realResult[0] = 1.0;
            imagResult[0] = 0.0;
            realZR[0] = 1.0;
            imagZR[0] = 0.0;
            for (k = 1; k <= nm; k++) {
                zmlt(realZR[0], imagZR[0], (a + k - 1.0) * (b + k - 1.0) / (k * (c + k - 1.0)), 0.0, realZR, imagZR);
                zmlt(realZR[0], imagZR[0], realZ, imagZ, realZR, imagZR);
                realResult[0] = realResult[0] + realZR[0];
                imagResult[0] = imagResult[0] + imagZR[0];
            } // for (k = 1; k <= nm; k++)
        } // else if (L3 || L4)
        else if (L5 || L6) {
            if (L5) {
                nm = (int) Math.abs(c - a);
            }
            if (L6) {
                nm = (int) Math.abs(c - b);
            }
            realResult[0] = 1.0;
            imagResult[0] = 0.0;
            realZR[0] = 1.0;
            imagZR[0] = 0.0;
            for (k = 1; k <= nm; k++) {
                zmlt(realZR[0], imagZR[0], (c - a + k - 1.0) * (c - b + k - 1.0) / (k * (c + k - 1.0)), 0.0, realZR,
                        imagZR);
                zmlt(realZR[0], imagZR[0], realZ, imagZ, realZR, imagZR);
                realResult[0] = realResult[0] + realZR[0];
                imagResult[0] = imagResult[0] + imagZR[0];
            } // for (k = 1; k <= nm; k++)
            zpow(1.0 - realZ, -imagZ, c - a - b, realTemp, imagTemp, ierr);
            if (ierr[0] == 1) {
                return;
            }
            zmlt(realTemp[0], imagTemp[0], realResult[0], imagResult[0], realResult, imagResult);
        } // else if (L5 || L6)
        else if (a0 <= 1.0) {
            if (x < 0.0) {
                zdiv(realZ, imagZ, realZ - 1.0, imagZ, realZ1, imagZ1);
                if ( (c > a) && (b < a) && (b > 0.0)) {
                    a = bb;
                    b = aa;
                } // if ((c > a) && (b < a) && (b > 0.0))
                // (1.0-z)**a = exp(a*log(1.0-z))
                zpow(1.0 - realZ, -imagZ, a, realTemp, imagTemp, ierr);
                if (ierr[0] == 1) {
                    return;
                }
                zdiv(1.0, 0.0, realTemp[0], imagTemp[0], realZC0, imagZC0);
                realResult[0] = 1.0;
                imagResult[0] = 0.0;
                realZR0[0] = 1.0;
                imagZR0[0] = 0.0;
                for (k = 1; k <= 500; k++) {
                    zmlt(realZR0[0], imagZR0[0], (a + k - 1.0) * (c - b + k - 1.0) / (k * (c + k - 1.0)), 0.0, realZR0,
                            imagZR0);
                    zmlt(realZR0[0], imagZR0[0], realZ1[0], imagZ1[0], realZR0, imagZR0);
                    realResult[0] = realResult[0] + realZR0[0];
                    imagResult[0] = imagResult[0] + imagZR0[0];
                    if (zabs(realResult[0] - realZW, imagResult[0] - imagZW) < zabs(realResult[0], imagResult[0]) * eps) {
                        break;
                    }
                    realZW = realResult[0];
                    imagZW = imagResult[0];
                } // for (k = 1; k <= 500; k++)
                zmlt(realZC0[0], imagZC0[0], realResult[0], imagResult[0], realResult, imagResult);
            } // if (x < 0.0)
            else if (a0 >= 0.9) {
                gm[0] = 0.0;
                if ( (c - a - b) >= 0) {
                    mcab = (int) (c - a - b + eps);
                } else {
                    mcab = (int) (c - a - b - eps);
                }
                if (Math.abs(c - a - b - mcab) < eps) {
                    m = (int) (c - a - b);
                    gamm = new Gamma(a, ga);
                    gamm.run();
                    gamm = new Gamma(b, gb);
                    gamm.run();
                    gamm = new Gamma(c, gc);
                    gamm.run();
                    gamm = new Gamma(a + m, gam);
                    gamm.run();
                    gamm = new Gamma(b + m, gbm);
                    gamm.run();
                    psi = new Psi(a, pa);
                    psi.run();
                    psi = new Psi(b, pb);
                    psi.run();
                    if (m != 0) {
                        gm[0] = 1.0;
                    }
                    for (j = 1; j <= Math.abs(m) - 1; j++) {
                        gm[0] = gm[0] * j;
                    } // for (j = 1; j <= Math.abs(m) - 1; j++)
                    rm = 1.0;
                    for (j = 1; j <= Math.abs(m); j++) {
                        rm = rm * j;
                    } // for (j = 1; j <= Math.abs(m); j++)
                    realZF0 = 1.0;
                    imagZF0 = 0.0;
                    realZR0[0] = 1.0;
                    imagZR0[0] = 0.0;
                    realZR1[0] = 1.0;
                    imagZR1[0] = 0.0;
                    sp0 = 0.0;
                    sp = 0.0;
                    if (m >= 0) {
                        realZC0[0] = gm[0] * gc[0] / (gam[0] * gbm[0]);
                        imagZC0[0] = 0.0;
                        zpow(realZ - 1.0, imagZ, m, realTemp, imagTemp, ierr);
                        if (ierr[0] == 1) {
                            return;
                        }
                        realZC1[0] = -gc[0] * realTemp[0] / (ga[0] * gb[0] * rm);
                        imagZC1[0] = -gc[0] * imagTemp[0] / (ga[0] * gb[0] * rm);
                        for (k = 1; k <= m - 1; k++) {
                            zmlt(realZR0[0], imagZR0[0], (a + k - 1) * (b + k - 1) / (k * (k - m)), 0, realZR0, imagZR0);
                            zmlt(realZR0[0], imagZR0[0], 1.0 - realZ, -imagZ, realZR0, imagZR0);
                            realZF0 = realZF0 + realZR0[0];
                            imagZF0 = imagZF0 + imagZR0[0];
                        } // for (k = 1; k <= m-1; k++)
                        for (k = 1; k <= m; k++) {
                            sp0 = sp0 + 1.0 / (a + k - 1) + 1.0 / (b + k - 1) - 1.0 / k;
                        } // for (k = 1; k <= m; k++)
                        zlog(1.0 - realZ, -imagZ, realTemp, imagTemp, ierr);
                        if (ierr[0] == 1) {
                            MipavUtil.displayError("Hypergeometric has error in zlog");
                            return;
                        }
                        realZF1[0] = pa[0] + pb[0] + sp0 + 2.0 * el + realTemp[0];
                        imagZF1[0] = imagTemp[0];
                        for (k = 1; k <= 500; k++) {
                            sp = sp + (1 - a) / (k * (a + k - 1)) + (1 - b) / (k * (b + k - 1));
                            sm = 0.0;
                            for (j = 1; j <= m; j++) {
                                sm = sm + (1 - a) / ( (j + k) * (a + j + k - 1)) + 1 / (b + j + k - 1);
                            } // for (j = 1; j <= m; j++)
                            zlog(1.0 - realZ, -imagZ, realTemp, imagTemp, ierr);
                            if (ierr[0] == 1) {
                                MipavUtil.displayError("Hypergeometric has error in zlog");
                            }
                            realZP = pa[0] + pb[0] + 2.0 * el + sp + sm + realTemp[0];
                            imagZP = imagTemp[0];
                            zmlt(realZR1[0], imagZR1[0], (a + m + k - 1) * (b + m + k - 1) / (k * (m + k)), 0, realZR1,
                                    imagZR1);
                            zmlt(realZR1[0], imagZR1[0], 1.0 - realZ, -imagZ, realZR1, imagZR1);
                            zmlt(realZR1[0], imagZR1[0], realZP, imagZP, realTemp, imagTemp);
                            realZF1[0] = realZF1[0] + realTemp[0];
                            imagZF1[0] = imagZF1[0] + imagTemp[0];
                            if (zabs(realZF1[0] - realZW, imagZF1[0] - imagZW) < zabs(realZF1[0], imagZF1[0]) * eps) {
                                break;
                            }
                            realZW = realZF1[0];
                            imagZW = imagZF1[0];
                        } // for (k = 1; k <= 500; k++)
                        zmlt(realZF0, imagZF0, realZC0[0], imagZC0[0], realTemp, imagTemp);
                        zmlt(realZF1[0], imagZF1[0], realZC1[0], imagZC1[0], realTemp2, imagTemp2);
                        realResult[0] = realTemp[0] + realTemp2[0];
                        imagResult[0] = imagTemp[0] + imagTemp2[0];
                    } // if (m >= 0)
                    else { // m < 0
                        m = -m;
                        zpow(1.0 - realZ, -imagZ, m, realTemp, imagTemp, ierr);
                        if (ierr[0] == 1) {
                            return;
                        }
                        zdiv(gm[0] * gc[0], 0.0, ga[0] * gb[0] * realTemp[0], ga[0] * gb[0] * imagTemp[0], realZC0,
                                imagZC0);
                        realZC1[0] = -Math.pow( -1, m) * gc[0] / (gam[0] * gbm[0] * rm);
                        imagZC1[0] = 0.0;
                        for (k = 1; k <= m - 1; k++) {
                            zmlt(realZR0[0], imagZR0[0], (a - m + k - 1) * (b - m + k - 1) / (k * (k - m)), 0.0,
                                    realZR0, imagZR0);
                            zmlt(realZR0[0], imagZR0[0], 1.0 - realZ, -imagZ, realZR0, imagZR0);
                            realZF0 = realZF0 + realZR0[0];
                            imagZF0 = imagZF0 + imagZR0[0];
                        } // for (k = 1; k <= m-1; k++)
                        for (k = 1; k <= m; k++) {
                            sp0 = sp0 + 1.0 / k;
                        } // for (k = 1; k <= m; k++)
                        zlog(1.0 - realZ, -imagZ, realTemp, imagTemp, ierr);
                        if (ierr[0] == 1) {
                            MipavUtil.displayError("zlog error in Hypergeometric");
                            return;
                        }
                        realZF1[0] = pa[0] + pb[0] - sp0 + 2.0 * el + realTemp[0];
                        imagZF1[0] = imagTemp[0];
                        for (k = 1; k <= 500; k++) {
                            sp = sp + (1 - a) / (k * (a + k - 1)) + (1 - b) / (k * (b + k - 1));
                            sm = 0.0;
                            for (j = 1; j <= m; j++) {
                                sm = sm + 1.0 / (j + k);
                            } // for (j = 1; j <= m; j++)
                            zlog(1.0 - realZ, -imagZ, realTemp, imagTemp, ierr);
                            if (ierr[0] == 1) {
                                MipavUtil.displayError("Hypergeometric has zlog error");
                                return;
                            }
                            realZP = pa[0] + pb[0] + 2.0 * el + sp - sm + realTemp[0];
                            imagZP = imagTemp[0];
                            zmlt(realZR1[0], imagZR1[0], (a + k - 1) * (b + k - 1) / (k * (m + k)), 0.0, realZR1,
                                    imagZR1);
                            zmlt(realZR1[0], imagZR1[0], 1.0 - realZ, -imagZ, realZR1, imagZR1);
                            zmlt(realZR1[0], imagZR1[0], realZP, imagZP, realTemp, imagTemp);
                            realZF1[0] = realZF1[0] + realTemp[0];
                            imagZF1[0] = imagZF1[0] + imagTemp[0];
                            if (zabs(realZF1[0] - realZW, imagZF1[0] - imagZW) < zabs(realZF1[0], imagZF1[0]) * eps) {
                                break;
                            }
                            realZW = realZF1[0];
                            imagZW = imagZF1[0];
                        } // for (k = 1; k <= 500; k++)
                        zmlt(realZF0, imagZF0, realZC0[0], imagZC0[0], realTemp, imagTemp);
                        zmlt(realZF1[0], imagZF1[0], realZC1[0], imagZC1[0], realTemp2, imagTemp2);
                        realResult[0] = realTemp[0] + realTemp2[0];
                        imagResult[0] = imagTemp[0] + imagTemp2[0];
                    } // else m < 0
                } // if (Math.abs(c-a-b-mcab) < eps)
                else { // c - a - b is not an integer
                    gamm = new Gamma(a, ga);
                    gamm.run();
                    gamm = new Gamma(b, gb);
                    gamm.run();
                    gamm = new Gamma(c, gc);
                    gamm.run();
                    gamm = new Gamma(c - a, gca);
                    gamm.run();
                    gamm = new Gamma(c - b, gcb);
                    gamm.run();
                    gamm = new Gamma(c - a - b, gcab);
                    gamm.run();
                    gamm = new Gamma(a + b - c, gabc);
                    gamm.run();
                    realZC0[0] = gc[0] * gcab[0] / (gca[0] * gcb[0]);
                    imagZC0[0] = 0.0;
                    zpow(1.0 - realZ, -imagZ, c - a - b, realTemp, imagTemp, ierr);
                    if (ierr[0] == 1) {
                        return;
                    }
                    realZC1[0] = gc[0] * gabc[0] / (ga[0] * gb[0]) * realTemp[0];
                    imagZC1[0] = gc[0] * gabc[0] / (ga[0] * gb[0]) * imagTemp[0];
                    realResult[0] = 0.0;
                    imagResult[0] = 0.0;
                    realZR0[0] = realZC0[0];
                    imagZR0[0] = imagZC0[0];
                    realZR1[0] = realZC1[0];
                    imagZR1[0] = imagZC1[0];
                    for (k = 1; k <= 500; k++) {
                        zmlt(realZR0[0], imagZR0[0], (a + k - 1) * (b + k - 1) / (k * (a + b - c + k)), 0.0, realZR0,
                                imagZR0);
                        zmlt(realZR0[0], imagZR0[0], 1.0 - realZ, -imagZ, realZR0, imagZR0);
                        zmlt(realZR1[0], imagZR1[0], (c - a + k - 1) * (c - b + k - 1) / (k * (c - a - b + k)), 0.0,
                                realZR1, imagZR1);
                        zmlt(realZR1[0], imagZR1[0], 1.0 - realZ, -imagZ, realZR1, imagZR1);
                        realResult[0] = realResult[0] + realZR0[0] + realZR1[0];
                        imagResult[0] = imagResult[0] + imagZR0[0] + imagZR1[0];
                        if (zabs(realResult[0] - realZW, imagResult[0] - imagZW) < zabs(realResult[0], imagResult[0])
                                * eps) {
                            break;
                        }
                        realZW = realResult[0];
                        imagZW = imagResult[0];
                    } // for (k = 1; k <= 500; k++)
                    realResult[0] = realResult[0] + realZC0[0] + realZC1[0];
                    imagResult[0] = imagResult[0] + imagZC0[0] + imagZC1[0];
                } // else c - a - b is not an integer
            } // else if (a0 >= 0.9)
            else { // a0 < 0.9
                realZ00[0] = 1.0;
                imagZ00[0] = 0.0;
                if ( ( (c - a) < a) && ( (c - b) < b)) {
                    zpow(1.0 - realZ, -imagZ, c - a - b, realZ00, imagZ00, ierr);
                    if (ierr[0] == 1) {
                        return;
                    }
                    a = c - a;
                    b = c - b;
                } // if (((c-a) < a) && ((c-b) < b))
                realResult[0] = 1.0;
                imagResult[0] = 0.0;
                realZR[0] = 1.0;
                imagZR[0] = 0.0;
                for (k = 1; k <= 1500; k++) {
                    realZR[0] = realZR[0] * (a + k - 1.0) * (b + k - 1.0) / (k * (c + k - 1.0));
                    imagZR[0] = imagZR[0] * (a + k - 1.0) * (b + k - 1.0) / (k * (c + k - 1.0));
                    zmlt(realZR[0], imagZR[0], realZ, imagZ, realZR, imagZR);
                    realResult[0] = realResult[0] + realZR[0];
                    imagResult[0] = imagResult[0] + imagZR[0];
                    if (zabs(realResult[0] - realZW, imagResult[0] - imagZW) <= zabs(realResult[0], imagResult[0])
                            * eps) {
                        break;
                    }
                    realZW = realResult[0];
                    imagZW = imagResult[0];
                } // for (k = 1; k <= 1500; k++)
                zmlt(realZ00[0], imagZ00[0], realResult[0], imagResult[0], realResult, imagResult);
            } // else a0 < 0.9
        } // else if (a0 <= 1.0)
        else { // a0 > 1.0
            if ( (a - b) >= 0.0) {
                mab = (int) (a - b + eps);
            } else {
                mab = (int) (a - b - eps);
            }
            if ( (Math.abs(a - b - mab) < eps) && (a0 <= 1.1)) {
                b = b + eps;
            }
            if (Math.abs(a - b - mab) > eps) {
                gamm = new Gamma(a, ga);
                gamm.run();
                gamm = new Gamma(b, gb);
                gamm.run();
                gamm = new Gamma(c, gc);
                gamm.run();
                gamm = new Gamma(a - b, gab);
                gamm.run();
                gamm = new Gamma(b - a, gba);
                gamm.run();
                gamm = new Gamma(c - a, gca);
                gamm.run();
                gamm = new Gamma(c - b, gcb);
                gamm.run();
                zpow( -realZ, -imagZ, a, realTemp, imagTemp, ierr);
                if (ierr[0] == 1) {
                    return;
                }
                zdiv(gc[0] * gba[0], 0.0, gca[0] * gb[0] * realTemp[0], gca[0] * gb[0] * imagTemp[0], realZC0, imagZC0);
                zpow( -realZ, -imagZ, b, realTemp, imagTemp, ierr);
                if (ierr[0] == 1) {
                    return;
                }
                zdiv(gc[0] * gab[0], 0.0, gcb[0] * ga[0] * realTemp[0], gcb[0] * ga[0] * imagTemp[0], realZC1, imagZC1);
                realZR0[0] = realZC0[0];
                imagZR0[0] = imagZC0[0];
                realZR1[0] = realZC1[0];
                imagZR1[0] = imagZC1[0];
                realResult[0] = 0.0;
                imagResult[0] = 0.0;
                for (k = 1; k <= 500; k++) {
                    zdiv(realZR0[0] * (a + k - 1.0) * (a - c + k), imagZR0[0] * (a + k - 1.0) * (a - c + k),
                            (a - b + k) * k * realZ, (a - b + k) * k * imagZ, realZR0, imagZR0);
                    zdiv(realZR1[0] * (b + k - 1.0) * (b - c + k), imagZR1[0] * (b + k - 1.0) * (b - c + k),
                            (b - a + k) * k * realZ, (b - a + k) * k * imagZ, realZR1, imagZR1);
                    realResult[0] = realResult[0] + realZR0[0] + realZR1[0];
                    imagResult[0] = imagResult[0] + imagZR0[0] + imagZR1[0];
                    zdiv(realResult[0] - realZW, imagResult[0] - imagZW, realResult[0], imagResult[0], realTemp,
                            imagTemp);
                    if (zabs(realTemp[0], imagTemp[0]) <= eps) {
                        break;
                    }
                    realZW = realResult[0];
                    imagZW = imagResult[0];
                } // for (k = 1; k <= 500; k++)
                realResult[0] = realResult[0] + realZC0[0] + realZC1[0];
                imagResult[0] = imagResult[0] + imagZC0[0] + imagZC1[0];
            } // if (Math.abs(a-b-mab) > eps)
            else { // a - b = 0, +-1, +-2,...
                if ( (a - b) < 0.0) {
                    a = bb;
                    b = aa;
                } // if ((a-b) < 0.0)
                ca = c - a;
                cb = c - b;
                if (ca >= 0.0) {
                    nca = (int) (ca + eps);
                } else {
                    nca = (int) (ca - eps);
                }
                if (cb >= 0.0) {
                    ncb = (int) (cb + eps);
                } else {
                    ncb = (int) (cb - eps);
                }
                if ( (Math.abs(ca - nca) < eps) || (Math.abs(cb - ncb) < eps)) {
                    c = c + eps;
                }
                gamm = new Gamma(a, ga);
                gamm.run();
                gamm = new Gamma(c, gc);
                gamm.run();
                gamm = new Gamma(cb, gcb);
                gamm.run();
                psi = new Psi(a, pa);
                psi.run();
                psi = new Psi(ca, pca);
                psi.run();
                psi = new Psi(a - c, pac);
                psi.run();
                mab = (int) (a - b + eps);
                zpow( -realZ, -imagZ, b, realTemp, imagTemp, ierr);
                if (ierr[0] == 1) {
                    return;
                }
                zdiv(gc[0], 0, ga[0] * realTemp[0], ga[0] * imagTemp[0], realZC0, imagZC0);
                gamm = new Gamma(a - b, gm);
                gamm.run();
                realZF0 = gm[0] / gcb[0] * realZC0[0];
                imagZF0 = gm[0] / gcb[0] * imagZC0[0];
                realZR[0] = realZC0[0];
                imagZR[0] = imagZC0[0];
                for (k = 1; k <= mab - 1; k++) {
                    zdiv(realZR[0] * (b + k - 1.0), imagZR[0] * (b + k - 1.0), k * realZ, k * imagZ, realZR, imagZR);
                    t0 = a - b - k;
                    gamm = new Gamma(t0, g0);
                    gamm.run();
                    gamm = new Gamma(c - b - k, gcbk);
                    gamm.run();
                    realZF0 = realZF0 + realZR[0] * g0[0] / gcbk[0];
                    imagZF0 = imagZF0 + imagZR[0] * g0[0] / gcbk[0];
                } // for (k = 1; k <= mab-1; k++)
                if (mab == 0) {
                    realZF0 = 0.0;
                    imagZF0 = 0.0;
                } // if (mab == 0.0)
                zpow( -realZ, -imagZ, a, realTemp, imagTemp, ierr);
                if (ierr[0] == 1) {
                    return;
                }
                zdiv(gc[0], 0.0, ga[0] * gcb[0] * realTemp[0], ga[0] * gcb[0] * imagTemp[0], realZC1, imagZC1);
                sp = -2.0 * el - pa[0] - pca[0];
                for (j = 1; j <= mab; j++) {
                    sp = sp + 1.0 / j;
                }
                zlog( -realZ, -imagZ, realTemp, imagTemp, ierr);
                if (ierr[0] == 1) {
                    MipavUtil.displayError("Hypergeometric has error in zlog");
                    return;
                }
                realZP0 = sp + realTemp[0];
                imagZP0 = imagTemp[0];
                sq = 1.0;
                for (j = 1; j <= mab; j++) {
                    sq = sq * (b + j - 1.0) * (b - c + j) / j;
                }
                zmlt(sq * realZP0, sq * imagZP0, realZC1[0], imagZC1[0], realZF1, imagZF1);
                realZR[0] = realZC1[0];
                imagZR[0] = imagZC1[0];
                rk1 = 1.0;
                sj1 = 0.0;
                for (k = 1; k <= 10000; k++) {
                    zdiv(realZR[0], imagZR[0], realZ, imagZ, realZR, imagZR);
                    rk1 = rk1 * (b + k - 1.0) * (b - c + k) / (k * k);
                    rk2 = rk1;
                    for (j = k + 1; j <= k + mab; j++) {
                        rk2 = rk2 * (b + j - 1.0) * (b - c + j) / j;
                    }
                    sj1 = sj1 + (a - 1) / (k * (a + k - 1)) + (a - c - 1) / (k * (a - c + k - 1));
                    sj2 = sj1;
                    for (j = k + 1; j <= k + mab; j++) {
                        sj2 = sj2 + 1.0 / j;
                    }
                    zlog( -realZ, -imagZ, realTemp, imagTemp, ierr);
                    if (ierr[0] == 1) {
                        MipavUtil.displayError("Hypergeometric has error in zlog");
                        return;
                    }
                    realZP = -2.0 * el - pa[0] - pac[0] + sj2 - 1.0 / (k + a - c) - Math.PI
                            / Math.tan(Math.PI * (k + a - c)) + realTemp[0];
                    imagZP = imagTemp[0];
                    zmlt(rk2 * realZR[0], rk2 * imagZR[0], realZP, imagZP, realTemp, imagTemp);
                    realZF1[0] = realZF1[0] + realTemp[0];
                    imagZF1[0] = imagZF1[0] + imagTemp[0];
                    ws = zabs(realZF1[0], imagZF1[0]);
                    if (Math.abs( (ws - w0) / ws) < eps) {
                        break;
                    }
                    w0 = ws;
                } // for (k = 1; k <= 10000; k++)
                realResult[0] = realZF0 + realZF1[0];
                imagResult[0] = imagZF0 + imagZF1[0];
            } // else a - b = 0, +-1, +-2,...
        } // else a0 > 1.0
        a = aa;
        b = bb;
        if (k > 150) {
            MipavUtil.displayWarning("Hypergeometric warning! Check the accuracy!");
        }
        return;
    } // complexArgument

    /**
     * b = z**a = exp(a*log(z))
     * 
     * @param zr
     * @param zi
     * @param a
     * @param br
     * @param bi
     * @param ierr
     */
    private void zpow(final double zr, final double zi, final double a, final double br[], final double bi[],
            final int ierr[]) {
        zlog(zr, zi, br, bi, ierr);
        if (ierr[0] == 1) {
            MipavUtil.displayError("Hypergeometric has error in zlog in zpow");
            return;
        }
        br[0] = a * br[0];
        bi[0] = a * bi[0];
        zexp(br[0], bi[0], br, bi);
        return;
    }

    /**
     * zabs computes the absolute value or magnitude of a double precision complex variable zr + j*zi.
     * 
     * @param zr double
     * @param zi double
     * 
     * @return double
     */
    private double zabs(final double zr, final double zi) {
        double u, v, q, s;
        u = Math.abs(zr);
        v = Math.abs(zi);
        s = u + v;

        // s * 1.0 makes an unnormalized underflow on CDC machines into a true
        // floating zero
        s = s * 1.0;

        if (s == 0.0) {
            return 0.0;
        } else if (u > v) {
            q = v / u;

            return (u * Math.sqrt(1.0 + (q * q)));
        } else {
            q = u / v;

            return (v * Math.sqrt(1.0 + (q * q)));
        }
    }

    /**
     * complex divide c = a/b.
     * 
     * @param ar double
     * @param ai double
     * @param br double
     * @param bi double
     * @param cr double[]
     * @param ci double[]
     */
    private void zdiv(final double ar, final double ai, final double br, final double bi, final double[] cr,
            final double[] ci) {
        double bm, cc, cd, ca, cb;

        bm = 1.0 / zabs(br, bi);
        cc = br * bm;
        cd = bi * bm;
        ca = ( (ar * cc) + (ai * cd)) * bm;
        cb = ( (ai * cc) - (ar * cd)) * bm;
        cr[0] = ca;
        ci[0] = cb;

        return;
    }

    /**
     * complex exponential function b = exp(a).
     * 
     * @param ar double
     * @param ai double
     * @param br double[]
     * @param bi double[]
     */
    private void zexp(final double ar, final double ai, final double[] br, final double[] bi) {
        double zm, ca, cb;
        zm = Math.exp(ar);
        ca = zm * Math.cos(ai);
        cb = zm * Math.sin(ai);
        br[0] = ca;
        bi[0] = cb;

        return;
    }

    /**
     * complex logarithm b = clog(a).
     * 
     * @param ar double
     * @param ai double
     * @param br double[]
     * @param bi double[]
     * @param ierr int[] ierr = 0, normal return ierr = 1, z = cmplx(0.0, 0.0)
     */
    private void zlog(final double ar, final double ai, final double[] br, final double[] bi, final int[] ierr) {
        double theta;
        double zm;
        ierr[0] = 0;

        if (ar == 0.0) {

            if (ai == 0.0) {
                ierr[0] = 1;

                return;
            } // if (ai == 0.0)
            else {

                if (ai > 0.0) {
                    bi[0] = Math.PI / 2.0;
                } else {
                    bi[0] = -Math.PI / 2.0;
                }

                br[0] = Math.log(Math.abs(ai));

                return;
            }
        } // if (ar == 0.0)
        else if (ai == 0.0) {

            if (ar > 0.0) {
                br[0] = Math.log(ar);
                bi[0] = 0.0;

                return;
            } else {
                br[0] = Math.log(Math.abs(ar));
                bi[0] = Math.PI;

                return;
            }
        } // else if (ai == 0.0)

        theta = Math.atan(ai / ar);

        if ( (theta <= 0.0) && (ar < 0.0)) {
            theta = theta + Math.PI;
        } else if (ar < 0.0) {
            theta = theta - Math.PI;
        }

        zm = zabs(ar, ai);
        br[0] = Math.log(zm);
        bi[0] = theta;

        return;
    }

    /**
     * complex multiply c = a * b.
     * 
     * @param ar double
     * @param ai double
     * @param br double
     * @param bi double
     * @param cr double[]
     * @param ci double[]
     */
    private void zmlt(final double ar, final double ai, final double br, final double bi, final double[] cr,
            final double[] ci) {
        double ca, cb;

        ca = (ar * br) - (ai * bi);
        cb = (ar * bi) + (ai * br);
        cr[0] = ca;
        ci[0] = cb;

        return;
    }

    /**
     * zshch computes the complex hyperbolic functions csh = sinh(x+i*y) and cch = cosh(x+i*y).
     * 
     * @param zr double
     * @param zi double
     * @param cshr double[]
     * @param cshi double[]
     * @param cchr double[]
     * @param cchi double[]
     */
    @SuppressWarnings("unused")
    private void zshch(double zr, final double zi, final double[] cshr, final double[] cshi, final double[] cchr,
            final double[] cchi) {
        double pexp;
        double mexp;
        double sh;
        double ch;
        double sn;
        double cn;

        pexp = Math.exp(zr);
        mexp = Math.exp( -zr);
        sh = 0.5 * (pexp - mexp);
        ch = 0.5 * (pexp + mexp);
        sn = Math.sin(zi);
        cn = Math.cos(zi);
        cshr[0] = sh * cn;
        cshi[0] = ch * sn;
        cchr[0] = ch * cn;
        cchi[0] = sh * sn;

        return;
    }

    /**
     * complex square root b = csqrt(a).
     * 
     * @param ar double
     * @param ai double
     * @param br double[]
     * @param bi double[]
     */
    @SuppressWarnings("unused")
    private void zsqrt(final double ar, final double ai, final double[] br, final double[] bi) {
        final double drt = 1.0 / Math.sqrt(2.0);
        double zm;
        double theta;

        zm = zabs(ar, ai);
        zm = Math.sqrt(zm);

        if (ar == 0.0) {

            if (ai == 0.0) {
                br[0] = 0.0;
                bi[0] = 0.0;

                return;
            } // if (ai == 0.0)
            else if (ai > 0.0) {
                br[0] = zm * drt;
                bi[0] = zm * drt;

                return;
            } // else if (ai > 0.0)
            else { // ai < 0.0
                br[0] = zm * drt;
                bi[0] = -zm * drt;

                return;
            } // else ai < 0.0
        } // if (ar == 0.0)
        else if (ai == 0.0) {

            if (ar > 0.0) {
                br[0] = Math.sqrt(ar);
                bi[0] = 0.0;

                return;
            } // if (ar > 0.0)
            else { // ar < 0.0
                br[0] = 0.0;
                bi[0] = Math.sqrt(Math.abs(ar));

                return;
            } // ar < 0.0
        } // else if (ai == 0.0)

        theta = Math.atan(ai / ar);

        if (theta <= 0.0) {

            if (ar < 0.0) {
                theta = theta + Math.PI;
            }
        } else if (ar < 0.0) {
            theta = theta - Math.PI;
        }

        theta = 0.5 * theta;
        br[0] = zm * Math.cos(theta);
        bi[0] = zm * Math.sin(theta);

        return;
    }
}
