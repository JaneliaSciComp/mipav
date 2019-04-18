package gov.nih.mipav.model.algorithms.filters;


import gov.nih.mipav.model.algorithms.AlgorithmBase;
import gov.nih.mipav.model.algorithms.AlgorithmConvolver;
import gov.nih.mipav.model.algorithms.AlgorithmInterface;
import gov.nih.mipav.model.algorithms.GenerateGaussian;
import gov.nih.mipav.model.structures.ModelImage;

import java.io.IOException;

import gov.nih.mipav.view.Preferences;
import gov.nih.mipav.view.ViewJProgressBar;


/**
 *  This module contains a port from FORTRAN to Java of the FORTRAN program for designing elliptic-function
 *  filters found in "Elliptic Functions for Filter Design" by H. J. Orchard and Alan N. Wilson, Jr. in 
 *  IEEE Transactions on Circuits and Systems-I: Fundamental Theory and Applications, Vol. 44. No. 4,
 *  April, 1997, pp. 273-287.
 */
public class AlgorithmEllipticFilter extends AlgorithmBase {

    //~ Instance fields ------------------------------------------------------------------------------------------------
    private int n1; // degree of filter, any positive integer > 1
    private double apd; // passband ripple in dB = 10*log(1 + e[0]**2)
    private double k; // elliptic modulus 0.0 <= k <= 1.0
    // Poles
    private double cafreal[];  // declared as double[20]
    private double cafimag[];  // declared as double[20]
    private double df[]; // declared as double[20];
    private boolean displayOutput;
    
   /**
    * The example in the article of n = 7, apd = 0.1dB, k = 0.8 gives in Table VI:
    *  Zeros:
    *  -0.0455944342 + j1.026557002
    *  -0.1713670100 + j0.918389608
    *  -0.3689660125 + j0.603979789
    *  -0.4980421689 + j0.0
    *  
    *  Poles:
    *  1.268831784
    *  1.467798747
    *  2.384834232
    * 
    */
    

    //~ Constructors ---------------------------------------------------------------------------------------------------

    /**
     * 
     * @param n1
     * @param apd
     * @param k
     * @param cafreal
     * @param cafimag
     * @param df
     * @param displayOutput
     */
    public AlgorithmEllipticFilter(int n1, double apd, double k, double cafreal[], double cafimag[], double df[], boolean displayOutput) {
        this.n1 = n1;
        this.apd = apd;
        this.k = k;
        this.cafreal = cafreal;
        this.cafimag = cafimag;
        this.df = df;
        this.displayOutput = displayOutput;
    }
    //~ Methods --------------------------------------------------------------------------------------------------------

    /**
     * Prepares this class for destruction.
     */
    public void finalize() {
        
        super.finalize();
    }

    /**
     * Starts the program.
     */
    public void runAlgorithm() {
        int no;
        int n2;
        int n3;
        double dbn = Math.log(10.0)/20.0;
        double apn;
        double e[] = new double[10];
        double ek[] = new double[10];
        double v;
        int m1;
        double vsqrt;
        int i;
        int m2 = 1;
        double g[] = new double[10];
        int n;
        double var;
        double asd;
        double a;
        double u2;
        double u1;
        double pi2 = Math.PI/2.0;
        double cosu1;
        double coshu2;
        double sinu1;
        double sinhu2;
        double denom;
        double creal;
        double cimag;
        double d;
        double denom2;
        
        no = n1 % 2;
        n2 = (n1 + no)/2;
        n3 = (n1 - no)/2;
        apn = dbn * apd;
        e[0] = Math.sqrt(2.0 * Math.exp(apn) * Math.sinh(apn));
        ek[0] = k;
        
        // Compute Landen transformations, forwards on k and backwards on g;
        v = ek[0];
        m1 = 0;
        do  {
            vsqrt = v/(1.0 + Math.sqrt(1.0 - v*v));
            v = vsqrt*vsqrt;
            m1 = m1 + 1;
            ek[m1] = v;
        } while (v > 1.0E-15);
        for (i = 0; i <= 10; i++) {
            m2 = m1 - i;
            g[m2] = 4.0 * Math.pow((ek[m1]/4.0), (n1/Math.pow(2.0, i)));
            if (g[m2] > 1.0E-305) {
            	break;
            }
        } // for (i = 0; i <= 10; i++)
        for (n = m2; n>= 1; n--) {
        	g[n-1] = 2.0*Math.sqrt(g[n])/(1.0 + g[n]);
        }
        var = e[0]/g[0];
        asd = 10.0*Math.log10(1.0 + var*var);
        if (displayOutput) {
            System.out.println("Stopband minimum loss = " + asd + " dB");
        }
        Preferences.debug("Stopband minimm loss = " + asd + " dB\n", Preferences.DEBUG_ALGORITHM);
        
        // Compute poles and zeros
        for (n = 1; n <= m2; n++) {
            a = (1.0 + g[n])*e[n-1]/2.0;
            e[n] = a + Math.sqrt(a*a + g[n]);
        }
        u2 = Math.log((1.0 + Math.sqrt(1.0 + e[m2]*e[m2]))/e[m2])/n1;
        for (i = 1; i <= n3; i++) {
            u1 = (2*i - 1)*pi2/n1;	
            cosu1 = Math.cos(u1);
            coshu2 = Math.cosh(u2);
            sinu1 = Math.sin(u1);
            sinhu2 = Math.sinh(u2);
            denom = cosu1*cosu1*coshu2*coshu2 + sinu1*sinu1*sinhu2*sinhu2;
            creal = -sinu1*sinhu2/denom;
            cimag = -cosu1*coshu2/denom;
            d = 1.0/cosu1;
            for (n = m1; n >= 1; n--) {
            	denom = creal*creal + cimag*cimag;
            	denom2 = (1.0 + ek[n]);
            	creal = (creal - ek[n]*creal/denom)/denom2;
            	cimag = (cimag + ek[n]*cimag/denom)/denom2;
            	d = (d + ek[n]/d)/denom2;
            } // for (n = m1; n >= 1; n--)
            denom = creal*creal + cimag*cimag;
            cafreal[i-1] = creal/denom;
            cafimag[i-1] = -cimag/denom;
            df[i-1] = d/ek[0];
        } // for (i = 1; i <= n3; i++)
        if (no == 1) {
        	a = 1.0/Math.sinh(u2);
        	for (n = m1; n >= 1; n--) {
        		a = (a - ek[n]/a)/(1.0 + ek[n]);
        	} // for (n = m1; n >= 1; n--)
        	cafreal[n2-1] = -1.0/a;
        	cafimag[n2-1] = 0.0;
        } // if (no == 1)
        
        // Write output data
        if (displayOutput) {
            System.out.println("Zeros:");
        }
        Preferences.debug("Zeros:\n", Preferences.DEBUG_ALGORITHM);
        for (i = 0; i < n2; i++) {
        	if (displayOutput) {
        	    System.out.println(cafreal[i] + "  +  j * " + cafimag[i]);
        	}
        	Preferences.debug(cafreal[i] + "  +  j * " + cafimag[i], Preferences.DEBUG_ALGORITHM);
        }
        if (displayOutput) {
            System.out.println("Poles:");
        }
        Preferences.debug("Poles:\n", Preferences.DEBUG_ALGORITHM);
        for (i = 0; i < n3; i++) {
        	if (displayOutput) {
        	    System.out.println(String.valueOf(df[i]));
        	}
        	Preferences.debug(String.valueOf(df[i]) + "\n", Preferences.DEBUG_ALGORITHM);
        }
        return;
    }

    
}

