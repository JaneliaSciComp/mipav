package gov.nih.mipav.model.algorithms;


/**  This code calculates the gamma function of an input argument x
 *   A typical real variable usage would be:
 *   double result[] = new double[1];
 *   Psi psiTest = new Psi(5.0, result);
 *   psiTest.run();
 *   Preferences.debug("Psi(5.0) = " + result[0] + "\n");
 *   UI.setDataText("Psi(5.0) = " + result[0] + "\n");
 *   
 *   A typical complex variable usage would be:
 *   double realResult[] = new double[1];
 *   double imagResult[] = new double[1];
 *   Psi psiTest = new Psi(5.0,-1.0,realResult,imagResult);
 *   psiTest.run();
 *   Preferences.debug("Psi(5.0,-1.0) = " + realResult[0] + ", " + imagResult[0] + "\n"); 
*/

public class Psi {
    
   public static final int REAL_VERSION = 1;
    
    public static final int COMPLEX_VERSION = 2;
    
    
    /** input argument  */
    private double x;
    
    /** output result */
    private double result[];
    
    /** REAL_VERSION or COMPLEX_VERSION */
    private int version;
    
    /** Real part of input argument */
    private double realX;
    
    /** Imaginary part of input argument */
    private double imagX;
    
    /** Real part of output result */
    private double realResult[];
   
    /** Imaginary part of output result */
    private double imagResult[];
    
    /**
     * 
     * @param x Input argument
     * @param result outputted psi(x)
     */
    public Psi(double x, double result[]) {
        this.x = x;
        this.result = result;
        version = REAL_VERSION;
    }
    
    /**
     * 
     * @param realX real part of input argument
     * @param imagX imaginary part of input argument
     * @param realResult real part of outputted psi(x)
     * @param imagResult imaginary part of outputted psi(x)
     */
    public Psi(double realX, double imagX, double realResult[], double imagResult[]) {
        this.realX = realX;
        this.imagX = imagX;
        this.realResult = realResult;
        this.imagResult = imagResult;
        version = COMPLEX_VERSION;
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
            realPsi();
        }
        else if (version == COMPLEX_VERSION) {
            complexPsi();
        }
    }

    /**
     * This code is a port of the FORTRAN routine PSI from the book Computation of
     * Special Functions by Shanjie Zhang and Jianming Jin, John Wiley & Sons, Inc.,
     * 1996, pp. 58-59.
     */
    public void realPsi() {
        double xa;
        double eL;
        double s;
        int n;
        int k;
        double x2;
        double a1 = -8.333333333333E-2;
        double a2 = 8.333333333333E-3;
        double a3 = -3.9682539682539683E-3;
        double a4 = 4.1666666666666667E-3;
        double a5 = -7.5757575757575758E-3;
        double a6 = 2.1092796092796093E-2;
        double a7 = -8.3333333333333333E-2;
        double a8 = 0.4432598039215686;
        
        xa = Math.abs(x);
        eL = 0.5772156649015329;
        s = 0.0;
        if ((x == (int)x) && (x <= 0.0)) {
            result[0] = Double.POSITIVE_INFINITY;
            return;
        }
        else if (xa == (int)xa) {
            n = (int)xa;
            for (k = 1; k <= n-1; k++) {
                s = s + 1.0/k;
            }
            result[0] = -eL + s;
        } // else if (xa == (int)xa)
        else if ((xa + 0.5) == (int)(xa + 0.5)) {
            n = (int)(xa - 0.5);
            for (k = 1; k <= n; k++) {
                s = s + 1.0/(2.0*k-1.0);
            }
            result[0] = -eL + 2.0*s-1.386294361119891;
        } // else if ((xa + 0.5) == (int)(xa + 0.5))
        else {
            if (xa < 10.0) {
                // When |x| < 10, add an integer to |x| such that |x| + n > 10
                n = 10 - (int)xa;
                for (k = 0; k <= n-1; k++) {
                   // psi(z+n) = psi(z) + sum from k = 0 to n-1 of (1/(z+k)) for n = 1, 2, 3,...
                   s = s + 1.0/(xa+k); 
                }
                xa = xa + n;
            } // if (xa < 10.0)
            x2 = 1.0/(xa*xa);
            // Calculate psi(|x| + n) using
            // When |z| -> infinity and |arg z| < PI, 
            // psi(z) equal approximately ln(z) - 1/(2z) - 1/(12Z**2) + 1/(120z**4) 
            // - 1/(252z**6) + 1/(240z**8) - 1/(132z**10) + 691/(32760z**12) 
            // - 1/(12z**14) + 3617/(8160z**16)
            result[0] = Math.log(xa)-0.5/xa+x2*(((((((a8*x2+a7)*x2+a6)*x2+a5)
                                                *x2+a4)*x2+a3)*x2+a2)*x2+a1);
            // Calculate psi|x| using 
//          psi(z+n) = psi(z) + sum from k = 0 to n-1 of (1/(z+k)) for n = 1, 2, 3,...
            result[0] = result[0] - s;
        } // else
        if (x < 0.0) {
            result[0] = result[0] - Math.PI*Math.cos(Math.PI*x)/Math.sin(Math.PI*x)-1.0/x;
        }
        return;
    } // realPsi
    
    /**
     * This code is a port of the FORTRAN routine CPSI from the book Computation of
     * Special Functions by Shanjie Zhang and Jianming Jin, John Wiley & Sons, Inc.,
     * 1996, pp. 59-60.
     */
    public void complexPsi() {
        double a[] = new double [] {-8.333333333333E-2, 8.333333333333E-3,
                                    -3.9682539682539683E-3, 4.1666666666666667E-3,
                                    -7.5757575757575758E-3, 2.1092796092796093E-2,
                                    -8.3333333333333333E-2, 0.4432598039215686};
        double x1 = 0.0;
        double y1 = 0.0;
        double x0;
        int n = 0;
        double th = 0.0;
        double z2;
        double z0;
        int k;
        double rr;
        double ri;
        double tn;
        double tm;
        double ct2;
        
        if (imagX == 0.0 && realX == (int)realX && realX <= 0.0) {
            realResult[0] = Double.POSITIVE_INFINITY;
            imagResult[0] = 0.0;
        }
        else {
            if (realX < 0.0) {
                // When Re(z) < 0, let z = -z
                x1 = realX;
                y1 = imagX;
                realX = -realX;
                imagX = -imagX;
            } // if (realX < 0.0)
            x0 = realX;
            if (realX < 8.0) {
                // When realX < 8, add an integer to realX such that realX + n > 8
                n = 8 - (int)realX;
                x0 = realX + n;
            } // if (realX < 8.0)
            if (x0 == 0.0 && imagX != 0.0) {
                th = 0.5*Math.PI;
            }
            if (x0 != 0.0) {
                th = Math.atan(imagX/x0);
            }
            z2 = x0*x0 + imagX*imagX;
            z0 = Math.sqrt(z2);
            // Calculate psi(|x| + n) using
            // When |z| -> infinity and |arg z| < PI, 
            // psi(z) equal approximately ln(z) - 1/(2z) - 1/(12Z**2) + 1/(120z**4) 
            // - 1/(252z**6) + 1/(240z**8) - 1/(132z**10) + 691/(32760z**12) 
            // - 1/(12z**14) + 3617/(8160z**16)
            realResult[0] = Math.log(z0)-0.5*x0/z2;
            imagResult[0] = th + 0.5*imagX/z2;
            for (k = 1; k <= 8; k++) {
                realResult[0] = realResult[0] + a[k-1]*Math.pow(z2,-k)*Math.cos(2.0*k*th);
                imagResult[0] = imagResult[0] - a[k-1]*Math.pow(z2,-k)*Math.sin(2.0*k*th);
            }
            if (realX < 8.0) {
                // Calculate psi(|x| + n) using
                // When |z| -> infinity and |arg z| < PI, 
                // psi(z) equal approximately ln(z) - 1/(2z) - 1/(12Z**2) + 1/(120z**4) 
                // - 1/(252z**6) + 1/(240z**8) - 1/(132z**10) + 691/(32760z**12) 
                // - 1/(12z**14) + 3617/(8160z**16)
                rr = 0.0;
                ri = 0.0;
                for (k = 1; k <= n; k++) {
                    rr = rr + (x0-k)/((x0-k)*(x0-k) + imagX*imagX);
                    ri = ri + imagX/((x0-k)*(x0-k) + imagX*imagX);
                }
                realResult[0] = realResult[0] - rr;
                imagResult[0] = imagResult[0] + ri;
            } // if (realX < 8.0)
            if (x1 < 0.0) {
                // When realX < 0, apply
                // psi(-z) = psi(z) + 1/2 + pi*cot(pi*z)
                tn = Math.atan(Math.PI*realX);
                tm = Math.tanh(Math.PI*imagX);
                ct2 = tn*tn + tm*tm;
                realResult[0] = realResult[0] + realX/(realX*realX + imagX*imagX)
                                + Math.PI*(tn - tn*tm*tm)/ct2;
                imagResult[0] = imagResult[0] - imagX/(realX*realX + imagX*imagX)
                                - Math.PI*tm*(1.0+tn*tn)/ct2;
                realX = x1;
                imagX = y1;
            } // if (x1 < 0.0)
        } // else
        return;
    } // complexPsi
   
}