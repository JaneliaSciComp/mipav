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
    
    /** Compute gamma function for real x, x is not equal to 0 or a negative integer */
    public static final int GAMMA = 1;
    
    /** Compute gamma function or log of gamma function for real x > 0 */
    public static final int LGAMMA = 2;
    
    /** Compute gamma function or log of gamma function for complex arguments */
    public static final int CGAMMA = 3;
    /** input argument 
     *  Note that the gamma function is not defined for x equal to zero or a
     *  negative integer */
    private double x;
    
    /** Real part of input argument */
    private double realX;
    
    /** Imaginary part of input argument */
    private double imagX;
    
    
    /** In lgamma and cgamma calculate gamma(x) for functionCode = 1 and
     *  calulate the ln(gamma(x)) for functionCode = 0
     */
    private int functionCode;
    
    /** output result */
    private double result[];
    
    /** Real part of outputted result */
    private double realResult[];
    
    /** Imaginary part of outputted result */
    private double imagResult[];
    
    /** GAMMA, LGAMMA, or CGAMMA */
    private int version;
    
    /**
     * 
     * @param x Input argument
     * @param result outputted gamma(x)
     */
    public Gamma(double x, double result[]) {
        this.x = x;
        this.result = result;
        this.version = GAMMA;
    }
    
    /**
     * 
     * @param x input argument, must have x > 0
     * @param functionCode 1 for gamma(x), 0 for ln(gamma(x))
     * @param result outputted gamma(x) or ln(gamma(x))
     */
    public Gamma(double x, int functionCode, double result[]) {
        this.x = x;
        this.functionCode = functionCode;
        this.result = result;
        this.version = LGAMMA;
    }
    
    /**
     * 
     * @param realX real part of input argument
     * @param imagX imaginary part of input argument
     * @param functionCode 1 for gamma(x),0 for ln(gamma(x))
     * @param realResult real part of outputted gamma(x) or ln(gamma(x))
     * @param imagResult imaginary part of outputted gamma(x) or ln(gamma(x))
     */
    public Gamma(double realX, double imagX, int functionCode, 
                 double realResult[], double imagResult[]) {
        this.realX = realX;
        this.imagX = imagX;
        this.functionCode = functionCode;
        this.realResult = realResult;
        this.imagResult = imagResult;
        this.version = CGAMMA;
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
        if (version == GAMMA) {
            gamma();
        }
        else if (version == LGAMMA) {
            lgamma();
        }
        else if (version == CGAMMA) {
            cgamma();
        }
    }

    /**
     * This code is a port of the FORTRAN routine GAMMA from the book Computation of
     * Special Functions by Shanjie Zhang and Jianming Jin, John Wiley & Sons, Inc.,
     * 1996, pp. 49-50.
     */
    private void gamma() {
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
    } // gamma
    
    /**
     * This code is a port of the FORTRAN routine LGAMA from the book Computation of
     * Special Functions by Shanjie Zhang and Jianming Jin, John Wiley & Sons, Inc.,
     * 1996, pp. 50-51.  This routine calculates the gamma(x) for functionCode = 1 or
     * the ln(gamma(x)) for the function code = 0.  x must be greater than zero.
     */
    private void lgamma() {
        double a[] = new double[]{8.333333333333333E-2, -2.777777777777778E-3,
                                  7.936507936507937E-4, -5.952380952380952E-4,
                                  8.417508417508418E-4, -1.917526917526918E-3,
                                  6.410256410256410E-3, -2.955065359477124E-2,
                                  1.796443723688307E-1, -1.392432216905900};
        double x0;
        int n = 0;
        double x2;
        double xp;
        double gl0;
        int k;
        
        x0 = x;
        if ((x == 1.0) || (x == 2.0)) {
            // Treat special case of x = 1 and 2
            result[0] = 0.0;
            if (functionCode == 1) {
                // Calculate gamma(x)
                result[0] = Math.exp(result[0]);
            }
            return;
        } // if ((x == 1.0) || (x == 2.0))
        else if (x <= 7.0) {
            // When x <= 7, add an integer to x such that x + n > 7
            n = (int)(7 - x);
            x0 = x + n;
        } // else if (x <= 7.0)
        x2 = 1.0/(x0*x0);
        // Calculate ln(gamma(x+n)) using
        // ln(gamma(z)), when |z| -> infinity and |arg z| < PI, approaches:
        // (z - 1/2)ln(z) - z + (1/2)ln(2*PI) + 1/(12*z) - 1/(360*z**3) + 1/(1260*z**5) 
        // - 1/(1680*z**7) + 1/(1188*z**9) - 691/(360360*z**11) + 7/(1092*z**13)
        // - 3617/(122400*z**15) + ...
        xp = 2.0*Math.PI;
        gl0 = a[9];
        for (k = 8; k >= 0; k--) {
            gl0 = gl0*x2 + a[k];
        }
        result[0] = gl0/x0 + 0.5*Math.log(xp) + (x0 - 0.5)*Math.log(x0) - x0;
        if (x <= 7.0) {
            // Calculate ln(gamma(x)) using
            // gamma(z) = (z-1)gamma(z-1) = (z-1)*(z-2)...*(z-n)*gamma(z-n)
            for (k = 1; k <= n; k++) {
                result[0] = result[0] - Math.log(x0 - 1.0);
                x0 = x0 - 1.0;
            } // for (k = 1; k <= n; k++)
        } // if (x <= 7.0)
        if (functionCode == 1) {
            // Calculate gamma(x)
            result[0] = Math.exp(result[0]);
        } // if (functionCode == 1)
        return;
    } // lgamma
    
    /**
     * This code is a port of the FORTRAN routine CGAMA from the book Computation of
     * Special Functions by Shanjie Zhang and Jianming Jin, John Wiley & Sons, Inc.,
     * 1996, pp. 51-52.  This routine calculates the gamma(x) for functionCode = 1 or
     * the ln(gamma(x)) for the function code = 0.  x is complex and the output is
     * complex.
     */
    private void cgamma() {
        double a[] = new double[]{8.333333333333333E-2, -2.777777777777778E-3,
                                  7.936507936507937E-4, -5.952380952380952E-4,
                                  8.417508417508418E-4, -1.917526917526918E-3,
                                  6.410256410256410E-3, -2.955065359477124E-2,
                                  1.796443723688307E-1, -1.392432216905900};
        double x1 = 0.0;
        double y1 = 0.0;
        double x0;
        int na = 0;
        double z1;
        double th;
        int k;
        double t;
        double gr1;
        double gi1;
        int j;
        double th1;
        double sr;
        double si;
        double th2;
        double g0;
        double z2;
        
        if (imagX == 0.0 && realX == (int)realX && realX <= 0.0) {
            realResult[0] = Double.POSITIVE_INFINITY;
            imagResult[0] = 0.0;
            return;
        } // if (imagX == 0.0 && realX == (int)realX && realX <= 0.0)
        else if (realX < 0.0) {
            // When realX < 0.0, let x = -x
            x1 = realX;
            y1 = imagX;
            realX = -realX;
            imagX = -imagX;
        } // else if (realX < 0.0)
        x0 = realX;
        if (realX <= 7.0) {
            // When realX <= 7, add an integer to realX such that realX + na > 7
            na = (int)(7 - realX);
            x0 = realX + na;
        } // if (realX <= 7.0)
        // Calculate ln(gamma(x+n)) using
        // ln(gamma(z)), when |z| -> infinity and |arg z| < PI, approaches:
        // (z - 1/2)ln(z) - z + (1/2)ln(2*PI) + 1/(12*z) - 1/(360*z**3) + 1/(1260*z**5) 
        // - 1/(1680*z**7) + 1/(1188*z**9) - 691/(360360*z**11) + 7/(1092*z**13)
        // - 3617/(122400*z**15) + ...
        z1 = Math.sqrt(x0*x0 + imagX*imagX);
        th = Math.atan(imagX/x0);
        realResult[0] = (x0-0.5)*Math.log(z1) - th*imagX - x0 + 0.5*Math.log(2.0*Math.PI);
        imagResult[0] = th*(x0-0.5) + imagX*Math.log(z1) - imagX;
        for (k = 1; k <= 10; k++) {
            t = Math.pow(z1,(1-2*k));
            realResult[0] = realResult[0] + a[k-1]*t*Math.cos((2.0*k-1.0)*th);
            imagResult[0] = imagResult[0] - a[k-1]*t*Math.sin((2.0*k-1.0)*th);
        } // for (k = 1; k <= 10; k++)
        if (realX <= 7.0) {
            // Calculate ln(gamma(x)) using
            // gamma(z) = (z-1)gamma(z-1) = (z-1)*(z-2)...*(z-n)*gamma(z-n)  
            gr1 = 0.0;
            gi1 = 0.0;
            for (j = 0; j <= na-1; j++) {
                gr1 = gr1 + 0.5*Math.log((realX+j)*(realX+j) + imagX*imagX);
                gi1 = gi1 + Math.atan(imagX/(realX+j));
            } // for (j = 0; j <= na-1; j++)
            realResult[0] = realResult[0] - gr1;
            imagResult[0] = imagResult[0] - gi1;
        } // if (realX <= 7.0)
        if (x1 < 0.0) {
            // When realX < 0, use gamma(-z) = -pi/(z*gamma(z)*sin(PI*z)) for 
            // z not equal to 0, +-1, +-2, ...
            z1 = Math.sqrt(realX*realX + imagX*imagX);
            th1 = Math.atan(imagX/realX);
            sr = -Math.sin(Math.PI*realX)*Math.cosh(Math.PI*imagX);
            si = -Math.cos(Math.PI*realX)*Math.sinh(Math.PI*imagX);
            z2 = Math.sqrt(sr*sr + si*si);
            th2 = Math.atan(si/sr);
            if (sr < 0.0) {
                th2 = Math.PI + th2;
            }
            realResult[0] = Math.log(Math.PI/(z1*z2)) - realResult[0];
            imagResult[0] = -th1 - th2 - imagResult[0];
            realX = x1;
            imagX = y1;
        } // if (x1 < 0.0)
        if (functionCode == 1) {
            // Calculate gamma(x)
            g0 = Math.exp(realResult[0]);
            realResult[0] = g0*Math.cos(imagResult[0]);
            imagResult[0] = g0*Math.sin(imagResult[0]);
        } // if (functionCode == 1)
        return;
    } // cgamma
   
}