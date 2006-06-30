package gov.nih.mipav.model.algorithms;

import gov.nih.mipav.view.*;

/**  This code calculates the confluent hypergeometric function of input parameters
 *   a and b and input argument x
 *   A typcial usage would be:
 *   double result[] = new double[1];
 *   ConfluentHypergeometric ch = new ConfluentHypergeometric(CONFLUENT_HYPERGEOMETRIC_FIRST_KIND,
 *                                                            -0.5, 1, 1.0, result);
 *   ch.run();
 *   Preferences.debug("Confluent hypergeomtric result = " + result[0] + "\n");
 *   UI.setDataText("Confluent hypergeometric result = " + result[0] + "\n");
*/

public class ConfluentHypergeometric {
//  ~ Static fields/initializers -------------------------------------------------------------------------------------

    /** Confluent Hypergeometric Function of the First Kind. */
    public static final int CONFLUENT_HYPERGEOMETRIC_FIRST_KIND = 1;
    
    /** Confluent Hypergeometric Function of the Second Kind */
    public static final int CONFLUENT_HYPERGEOMETRIC_SECOND_KIND = 2;
    
    /** Tells whether confluent hypergeometric function is of first or second kind */
    private int kind;
    
    /** Input parameter */
    private double a;
    
    /** Input parameter */
    private double b;
    
    /** Input argument */
    private double x;
    
    /** Outputted result */
    private double result[];
    
    
//  ~ Constructors ---------------------------------------------------------------------------------------------------

    /**
     * @param kind Tells whether confluent hypergeometric function is of first or second kind
     * @param a input parameter
     * @param b input parameter
     * @param x input argument
     * @param result outputted result
     */
    public ConfluentHypergeometric(int kind, double a, double b, double x, double result[]) {
        this.kind = kind;
        this.a = a;
        this.b = b;
        this.x = x;
        this.result = result;
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
        if (kind == CONFLUENT_HYPERGEOMETRIC_FIRST_KIND) {
            firstKindRealArgument();
        }
        else if (kind == CONFLUENT_HYPERGEOMETRIC_SECOND_KIND) {
            MipavUtil.displayError("Second kind not implemented");
            return;
        }
        else {
            MipavUtil.displayError("Illegal kind argument");
            return;
        }
    } // run()
    
    /**
     * This code is a port of the FORTRAN routine CHGM from the book Computation of
     * Special Functions by Shanjie Zhang and Jianming Jin, John Wiley & Sons, Inc.,
     * 1996, pp. 398-400.  It computes confluent hypergeometric functions of the first
     * kind for real parameters and argument.  It works well except for the case of 
     * a << -1 when x > 0 and a >> 1 when x < 0. In this exceptional case, the evaluation
     * involves the summation of a partially alternating series which results in a loss
     * of significant digits.
     * Note that with a = -0.5, b = 1, x = 798.2, this code resulted in 
     * Math.exp(x) == infinite.  So this code cannot be used for large x.
     */
    private void firstKindRealArgument() {
        double a0;
        double a1;
        double x0;
        int m;
        double r;
        int k;
        int nL;
        int L = 0;
        double rg;
        int j;
        double xg;
        double sum1;
        double sum2;
        double r1;
        double r2;
        double hg1;
        double hg2;
        int L0;
        double y0 = 0.0;
        double y1 = 0.0;
        double ta[] = new double[1];
        double tb[] = new double[1];
        double tba[] = new double[1];
        int i;
        
        a0 = a;
        a1 = a;
        x0 = x;
        result[0] = 0.0;
        if ((b == 0.0) || (b == -Math.abs((int)b))) {
            result[0] = Double.POSITIVE_INFINITY;
        }
        else if ((a == 0.0) || (x == 0.0)) {
            result[0] = 1.0;
        }
        else if (a == -1.0) {
            result[0] = 1.0 - x/b;
        }
        else if (a == b) {
            result[0] = Math.exp(x);
        }
        else if ((a-b) == 1.0) {
            result[0] = (1.0 + x/b)*Math.exp(x);
        }
        else if ((a == 1.0) && (b == 2.0)) {
            result[0] = (Math.exp(x) - 1.0)/x;
        }
        else if ((a == (int)a) && (a < 0.0)) {
            m = (int)(-a);
            r = 1.0;
            result[0] = 1.0;
            for (k = 1; k <= m; k++) {
                r = r * (a + k - 1.0)/k/(b+k-1.0)*x;
                result[0] = result[0] + r;
            }
        } // else if ((a == (int)a) && (a < 0.0))
        if (result[0] != 0.0) {
            return;
        }
        if (x < 0.0) {
            // 1F1(a, b, z) = exp(z)1F1(b-a, b, -z);
            a = b - a;
            a0 = a;
            x = Math.abs(x);
        } // if (x < 0.0)
        if (a < 2.0) {
            nL = 0;
        }
        else {
            // (2a - b + z)1F1(a, b, z) = a*1F1(a+1, b, z) - (b - a)*1F1(a-1, b, z)
            nL = 1;
            L = (int)a;
            a = a - L - 1.0;
        } // else (a >= 2.0)
        for (L0 = 0; L0 <= nL; L0++) {
            if (a0 >= 2.0) {
                a = a + 1.0;
            }
            if ((x <= 30.0 + Math.abs(b)) || (a < 0.0)) {
                // 1F1(a, b, z) = sum from k = 0 to infinity of
                // Pochhammer(a,k)*(z**k)/((k!)*Pochhammer(b,k))
                result[0] = 1.0;
                rg = 1.0;
                for (j = 1; j <= 500; j++) {
                    rg = rg * (a+j-1.0)/(j*(b+j-1.0))*x;
                    result[0] = result[0] + rg;
                    if (Math.abs(rg/result[0]) < 1.0E-15) {
                        break;
                    }
                } // for (j = 1; j <= 500; j++)
            } //if ((x <= 30.0 + Math.abs(b)) || (a < 0.0))
            else {
               Gamma gam = new Gamma(a, ta);
               gam.run();
               gam = new Gamma(b, tb);
               gam.run();
               xg = b - a;
               gam = new Gamma(xg, tba);
               gam.run();
               sum1= 1.0;
               sum2 = 1.0;
               r1 = 1.0;
               r2 = 1.0;
               for (i = 1; i <= 8; i++) {
                   r1 = -r1 * (a + i - 1.0) * (a - b + i)/ (x * i);
                   r2 = -r2 * (b - a + i - 1.0) * (a - i)/ (x * i);
                   sum1 = sum1 + r1;
                   sum2 = sum2 + r2;
               } // for (i = 1; i <= 8; i++)
               hg1 = tb[0]/tba[0]*Math.pow(x,-a)*Math.cos(Math.PI*a)*sum1;
               hg2 = tb[0]/ta[0]*Math.exp(x)*Math.pow(x,a-b)*sum2;
               result[0] = hg1 + hg2;        
            } // else 
            if (L0 == 0) {
                y0 = result[0];
            }
            if (L0 == 1) {
                y1 = result[0];
            }
        } // for (L0 = 0; L0 <= nL; L0++)
        if (a0 >= 2.0) {
            // (2a - b + z)1F1(a, b, z) = a*1F1(a+1, b, z) - (b - a)*1F1(a-1, b, z)
            for (i = 1; i <= L-1; i++) {
                result[0] = ((2.0*a - b + x)*y1 + (b-a)*y0)/a;
                y0 = y1;
                y1 = result[0];
                a = a + 1.0;
            } // for (i = 1; i <= L-1; i++)
        } // if (a0 >= 2.0)
        if (x0 < 0.0) {
            // 1F1(a, b, z) = exp(z)1F1(b-a, b, -z);
            result[0] = result[0] * Math.exp(x0);
            a0 = a1;
        } // if (x0 < 0.0)
        a = a0;
        x = x0;
        return;
    } // firstKindRealArgument
}