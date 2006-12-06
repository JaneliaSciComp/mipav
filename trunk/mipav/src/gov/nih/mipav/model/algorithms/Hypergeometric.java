package gov.nih.mipav.model.algorithms;

import gov.nih.mipav.view.*;

/**  
*/

public class Hypergeometric {
//  ~ Static fields/initializers -------------------------------------------------------------------------------------
    
    public static final int REAL_VERSION = 1;
    
    public static final int COMPLEX_VERSION = 2;
    
    /** Tells whether real number or complex number version */
    private int version;
    
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
    
    
//  ~ Constructors ---------------------------------------------------------------------------------------------------

    /**
     * @param a input parameter
     * @param b input parameter
     * @param c input parameter c must not equal 0, -1, -2, ...
     * @param x input argument
     * @param result outputted result
     */
    public Hypergeometric(double a, double b, double c, double x, double result[]) {
        this.a = a;
        this.b = b;
        this.c = c;
        this.x = x;
        this.result = result;
        this.version = REAL_VERSION;
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
    public Hypergeometric(double a, double b, double c,
            double realZ, double imagZ, double realResult[], double imagResult[]) {
        this.a = a;
        this.b = b;
        this.c = c;
        this.realZ = realZ;
        this.imagZ = imagZ;
        this.realResult = realResult;
        this.imagResult = imagResult;
        this.version = COMPLEX_VERSION;
    }
    
    
//  ~ Methods --------------------------------------------------------------------------------------------------------

    /**
     * Cleanup memory.
     *
     * @throws  Throwable  DOCUMENT ME!
     */
    public void finalize() throws Throwable {

        super.finalize();
    }

    
    public void run() {
        if (version == REAL_VERSION) {
            realArgument();
        }
        
        else if (version == COMPLEX_VERSION) {
            complexArgument();
        }
        else {
            MipavUtil.displayError("Illegal argument");
            return;
        }
    } // run()
    
    /**
     * This is a port of subroutine HYGFX from Computation of Special Functions
     * by Shanjie Zhang and Jianming Jin, pp. 376-379.
     *
     */
    private void realArgument() {
        boolean L0, L1, L2, L3, L4, L5;
        double el = .5772156649015329;
        double eps;
        double gc[] = new double[1];
        double gcab[] = new double[1];
        double gca[] = new double[1];
        double gcb[] = new double[1];
        double g0;
        double g1[] = new double[1];
        double g2[] = new double[1];
        double g3[] = new double[1];
        int nm = 0;
        double r;
        int k;
        double aa;
        double bb;
        double x1;
        double gm;
        int m;
        double ga[] = new double[1];
        double gb[] = new double[1];
        double gam[] = new double[1];
        double gbm[] = new double[1];
        double pa[] = new double[1];
        double pb[] = new double[1];
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
        double gabc[] = new double[1];
        double a0;
        Gamma gamm;
        Psi psi;
        
        
        L0 = ((c == (int)c) && (c < 0.0));
        L1 = (((1.0-x) < 1.0E-15) && (c - a - b <= 0.0));
        L2 = ((a == (int)a) && (a < 0.0));
        L3 = ((b == (int)b) && (b < 0.0));
        L4 = (((c-a) == (int)(c-a)) && (c-a <= 0.0));
        L5 = (((c-b) == (int)(c-b)) && (c-b <= 0.0));
        if (L0 || L1) {
            MipavUtil.displayError("The hypergeometric series is divergent");
            return;
        }
        eps = 1.0E-15;
        if (x > 0.95) {
            eps = 1.0E-8;
        }
        if ((x == 0.0) || (a == 0.0) || (b == 0.0)) {
            result[0] = 1.0;
            return;
        } // if ((x == 0.0) || (a == 0.0) || (b == 0.0))
        else if (((1.0-x) == eps) && (c-a-b > 0.0)) {
            // For c > a + b calculate F(a,b,c,1) using
            // F(a,b,c,1) = Gamma(c)*Gamma(c-a-b)/(Gamma(c-a)*Gamma(c-b))
            gamm = new Gamma(c,gc);
            gamm.run();
            gamm = new Gamma(c-a-b,gcab);
            gamm.run();
            gamm = new Gamma(c-a,gca);
            gamm.run();
            gamm = new Gamma(c-b,gcb);
            gamm.run();
            result[0] = gc[0]*gcab[0]/(gca[0]*gcb[0]);
            return;
        } // else if (((1.0-x) == eps) && (c-a-b > 0.0))
        else if (((1.0 + x) <= eps) && (Math.abs(c-a+b-1.0) <= eps)) {
            // For c = a + 1 - b calculate F(a,b,c,-1) using
            // F(a,b,a+1-b,-1) = Gamma(a-b+1)*Gamma(a/2 + 1)/(Gamma(a+1)*Gamma(1 - b + a/2))
            g0 = Math.sqrt(Math.PI) * Math.pow(2.0,-a);
            gamm = new Gamma(c,g1);
            gamm.run();
            gamm = new Gamma(1.0+a/2.0-b,g2);
            gamm.run();
            gamm = new Gamma(0.5 + 0.5*a, g3);
            gamm.run();
            result[0] = g0*g1[0]/(g2[0]*g3[0]);
            return;
        } // else if (((1.0 + x) <= eps) && (Math.abs(c-a+b-1.0) <= eps))
        else if (L2 || L3) {
            if (L2) {
                nm = (int)Math.abs(a);
            }
            if (L3) {
                nm = (int)Math.abs(b);
            }
            result[0] = 1.0;
            r = 1.0;
            for (k = 1; k <= nm; k++) {
                r = r * (a + k - 1.0) * (b + k - 1.0)/(k * (c + k - 1.0)) * x;
                result[0] = result[0] + r;
            }
            return;
        } // else if (L2 || L3)
        else if (L4 || L5) {
            if (L4) {
                nm = (int)Math.abs(c-a);
            }
            if (L5) {
                nm = (int)Math.abs(c-b);
            }
            result[0] = 1.0;
            r = 1.0;
            for (k = 1; k <= nm; k++) {
                r = r * (c-a+k-1.0)*(c-b+k-1.0)/(k*(c+k-1.0)) * x;
                result[0] = result[0] + r;
            }
            result[0] = Math.pow((1.0-x),(c-a-b))*result[0];
            return;
        } // else if (L4 || L5)
        aa = a;
        bb = b;
        x1 = x;
        if (x < 0.0) {
            x = x/(x-1.0);
            if ((c > a) && (b < a) && (b > 0.0)) {
                a = bb;
                b = aa;
            }
            b = c - b;
        } // if (x < 0.0)
        if (x >= 0.75) {
            gm = 0.0;
            if (Math.abs(c - a - b - (int)(c - a - b)) < 1.0E-15) {
                m = (int)(c - a - b);
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
                for (j = 1; j <= Math.abs(m)-1; j++) {
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
                    c0 = gm*gc[0]/(gam[0]*gbm[0]);
                    c1 = -gc[0]*Math.pow((x-1.0),m)/(ga[0]*gb[0]*rm);
                    for (k = 1; k <= m-1; k++) {
                        r0 = r0*(a+k-1)*(b+k-1)/(k*(k-m))*(1.0-x);
                        f0 = f0 + r0;
                    } // for (k = 1; k <= m-1; k++)
                    for (k = 1; k <= m; k++) {
                        sp0 = sp0 + 1.0/(a+k-1) + 1.0/(b+k-1) - 1.0/k;
                    } // for (k = 1; k <= m; k++)
                    f1 = pa[0] + pb[0] + sp0 + 2.0*el + Math.log(1.0-x);
                    for (k = 1; k <= 250; k++) {
                        sp = sp + (1 - a)/(k*(a+k-1)) + (1-b)/(k*(b+k-1));
                        sm = 0.0;
                        for (j = 1; j <= m; j++) {
                            sm = sm + (1-a)/((j+k)*(a+j+k-1)) + 1/(b+j+k-1);
                        } // for (j = 1; j <= m; j++)
                        rp = pa[0] + pb[0] + 2.0*el + sp + sm + Math.log(1.0 - x);
                        r1 = r1*(a+m+k-1)*(b+m+k-1)/(k*(m+k))*(1.0-x);
                        f1 = f1 + r1 * rp;
                        if (Math.abs(f1-hw) < Math.abs(f1)*eps) {
                            break;    
                        }
                        hw = f1;
                    } // for (k = 1; k <= 250; k++)
                    result[0] = f0*c0 + f1*c1;
                } // if (m >= 0)
                else { // m < 0
                    m = -m;
                    c0 = gm*gc[0]/(ga[0]*gb[0]*Math.pow((1.0-x),m));
                    c1 = -Math.pow(-1,m)*gc[0]/(gam[0]*gbm[0]*rm);
                    for (k = 1; k <= m-1; k++) {
                        r0 = r0*(a-m+k-1)*(b-m+k-1)/(k*(k-m))*(1.0-x);
                        f0 = f0 + r0;
                    } // for (k = 1; k <= m-1; k++)
                    for (k = 1; k <= m; k++) {
                        sp0 = sp0 + 1.0/k;
                    } // for (k = 1; k <= m; k++)
                    f1 = pa[0] + pb[0] - sp0 + 2.0*el + Math.log(1.0-x);
                    for (k = 1; k <= 250; k++) {
                        sp = sp + (1-a)/(k*(a+k-1)) + (1-b)/(k*(b+k-1));
                        sm = 0.0;
                        for (j = 1; j <= m; j++) {
                            sm = sm + 1.0/(j+k);
                        } // for (j = 1; j <= m; j++)
                        rp = pa[0] + pb[0] + 2.0*el + sp - sm + Math.log(1.0-x);
                        r1 = r1*(a+k-1)*(b+k-1)/(k*(m+k))*(1.0-x);
                        f1 = f1 + r1*rp;
                        if (Math.abs(f1-hw) < Math.abs(f1)*eps) {
                            break;
                        }
                        hw = f1;
                    } // for (k = 1; k <= 250; k++)
                    result[0] = f0*c0 + f1*c1;
                } // else m < 0
            } // if (Math.abs(c - a - b - (int)(c - a - b)) < 1.0E-15)
            else { // c - a - b is not an integer
                gamm = new Gamma(a, ga);
                gamm.run();
                gamm = new Gamma(b, gb);
                gamm.run();
                gamm = new Gamma(c, gc);
                gamm.run();
                gamm = new Gamma(c-a, gca);
                gamm.run();
                gamm = new Gamma(c-b, gcb);
                gamm.run();
                gamm = new Gamma(c-a-b, gcab);
                gamm.run();
                gamm = new Gamma(a+b-c, gabc);
                gamm.run();
                c0 = gc[0]*gcab[0]/(gca[0]*gcb[0]);
                c1 = gc[0]*gabc[0]/(ga[0]*gb[0])*Math.pow((1.0-x), (c-a-b));
                result[0] = 0.0;
                r0 = c0;
                r1 = c1;
                for (k = 1; k <= 250; k++) {
                    r0 = r0*(a+k-1)*(b+k-1)/(k*(a+b-c+k))*(1.0-x);
                    r1 = r1*(c-a+k-1)*(c-b+k-1)/(k*(c-a-b+k))*(1.0-x);
                    result[0] = result[0] + r0 + r1;
                    if (Math.abs(result[0]-hw) < Math.abs(result[0])*eps) {
                        break;
                    }
                    hw = result[0];
                } // for (k = 1; k <= 250; k++)
                result[0] = result[0] + c0 + c1;
            } // else c - a - b is not an integer
        } // if (x >= 0.75)
        else { // 0.0 <= x < 0.75
            a0 = 1.0;
            if (((c-a) < a) && ((c-b) < b)) {
                a0 = Math.pow((1.0-x),(c-a-b));
                a = c-a;
                b = c-b;
            } // if (((c-a) < a) && ((c-b) < b))
            result[0] = 1.0;
            r = 1.0;
            for (k = 1; k <= 250; k++) {
                r = r*(a+k-1.0)*(b+k-1.0)/(k*(c+k-1.0))*x;
                result[0] = result[0] + r;
                if (Math.abs(result[0]-hw) <= Math.abs(result[0])*eps) {
                    break;
                }
                hw = result[0];
            } // for (k = 1; k <= 250; k++)
            result[0] = a0 * result[0];
        } // else 0.0 <= x < 0.75
        if (x1 < 0.0) {
            x = x1;
            c0 = 1.0/Math.pow((1.0-x),aa);
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
    
    private void complexArgument() {
        boolean L0, L1, L2, L3, L4, L5, L6;
        double x;
        double y;
        double eps = 1.0E-15;
        double aa;
        double bb;
        double a0;
        double el = .5772156649015329;
        Gamma gamm;
        double gc[] = new double[1];
        double gcab[] = new double[1];
        double gca[] = new double[1];
        double gcb[] = new double[1];
        double g0[] = new double[1];
        double g1[] = new double[1];
        double g2[] = new double[1];
        double g3[] = new double[1];
        int nm;
        double realZR;
        double imagZR;
        int k;
        double realZ1;
        double imagZ1;
        double realZC0;
        double imagZC0;
        double realZR0;
        double imagZR0;
        double realZW;
        double imagZW;
        double gm;
        int mcab;
        int m;
        double gam[] = new double[1];
        double gbm[] = new double[1];
        Psi psi;
        double pa[] = new double[1];
        double pb[] = new double[1];
        int j;
        double rm;
        double realZF0;
        double imagZF0;
        double realZR1;
        double imagZR1;
        double sp0;
        double sp;
        double realZC1;
        double imagZC1;
        double realZF1;
        double imagZF1;
        double realZP;
        double imagZP;
        double ga[] = new double[1];
        double gb[] = new double[1];
        double gabc[] = new double[1];
        double realZ00;
        double imagZ00;
        int mab;
        double gab[] = new double[1];
        double gba[] = new double[1];
        double ca;
        double cb;
        int nca;
        int ncb;
        double pca[] = new double[1];
        double pac[] = new double[1];
        double t0;
        double gcbk[] = new double[1];
        double sq;
        double rk1;
        double sj1;
        double rk2;
        double sj2;
        double ws;
        double w0 = 0.0;
        
        x = realZ;
        y = imagZ;
        L0 = ((c == (int)c) && (c < 0.0));
        L1 = ((Math.abs(1.0-x) < eps) && (y == 0.0) && ((c-a-b) <= 0.0));
    } // complexArgument
}