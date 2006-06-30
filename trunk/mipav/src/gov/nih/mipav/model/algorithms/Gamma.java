package gov.nih.mipav.model.algorithms;


/**  This code calculates the gamma function of an input argument x
 *   A typcial usage would be:
 *   double result[] = new double[1];
 *   Gamma gammaTest = new Gamma(5.0, result);
 *   gammaTest.run();
 *   Preferences.debug("Gamma(5.0) = " + result[0] + "\n");
 *   UI.setDataText("Gamma(5.0) = " + result[0] + "\n");
*/

public class Gamma {
    /** input argument 
     *  Note that the gamma function is not defined for x equal to zero or a
     *  negative integer */
    double x;
    
    /** output result */
    double result[];
    
    public Gamma(double x, double result[]) {
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

    /**
     * This code is a port of the FORTRAN routine GAMMA from the book Computation of
     * Special Functions by Shanjie Zhang and Jianming Jin, John Wiley & Sons, Inc.,
     * 1996, pp. 49-50.
     */
    public void run() {
        int m1;
        int k;
        double z;
        int m;
        double r = 1.0;
        double g[] = new double[]{1.0, 0.5772156649015329, -0.6558780715202538,
                     -0.420026350340952E-1, 0.1665386113822915, -0.421977345555443E-1,
                     -0.96219715278770E-2, 0.72189432466630E-2, -0.11651675918591E-2,
                     -0.2152416741149E-3, 0.1280502823882E-3, -0.201348547807E-4,
                     -0.12504934821E-5, 0.11330272320E-5, -0.2056338417E-6, 
                     0.61160950E-8, 0.50020075E-8, -0.11812746E-8,
                     0.1043427E-9, 0.77823E-11, -0.36968E-11,
                     0.51E-12, -0.206E-13, -0.54E-14, 
                     0.14E-14};
        double gr;
        if (x == (int)x) {
            if (x > 0.0) { // Use gamma(x) == (-1)! for x a positive integer
                result[0] = 1.0;
                m1 = (int)(x) - 1;
                for (k = 2; k <= m1; k++) {
                    result[0] = result[0] * k;
                }
            } // if (x > 0.0)
            else {
                result[0] = Double.POSITIVE_INFINITY; // for x 0 or a negative integer
            }
        }  // if (x == (int)x)
        else {
            if (Math.abs(x) > 1.0) {
                // When abs(x) > 1, use gamma(z) = (z-1)gamma(z-1) =
                // (z-1)(z-2)...(z-n)gamma(z-n)
                z = Math.abs(x);
                m = (int)z;
                r = 1.0;
                for (k = 1; k <= m; k++) {
                    r = r * (z-k);
                }
                z = z - m;
            } // if (Math.abs(x) > 1.0)
            else {
                z = x;
            }
            gr = g[24];
            // Calculate 1/gamma(x) = sum from k = 1 to 25 of g[k-1]*z**k
            for (k = 23; k >= 0; k--) {
                gr = gr*z + g[k];
            }
            result[0] = 1.0/(gr*z);
            if (Math.abs(x) > 1.0) {
                // When abs(x) > 1, use gamma(z) = (z-1)gamma(z-1) =
                // (z-1)(z-2)...(z-n)gamma(z-n) 
                result[0] = result[0] * r;
                if (x < 0.0) {
                    // gamma(-z) = -PI/(z*gamma(z)*sin(PI*z)) for z not equal to an integer
                    result[0] = -Math.PI/(x*result[0]*Math.sin(Math.PI*x));
                } // if (x < 0.0)
            } // if (Math.abs(x) > 1.0)
        } // else
        return;
    } // runAlgorithm
   
}