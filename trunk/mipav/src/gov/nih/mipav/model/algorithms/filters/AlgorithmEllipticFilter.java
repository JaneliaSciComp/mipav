package gov.nih.mipav.model.algorithms.filters;


import gov.nih.mipav.model.algorithms.AlgorithmBase;
import gov.nih.mipav.view.MipavUtil;
import gov.nih.mipav.view.Preferences;


/**
 *  This module contains a port from FORTRAN to Java of the FORTRAN program for designing elliptic-function
 *  filters and a port from MATLAB to Java of the MATLAB program ellipap1 for designing an elliptic
 *  analog lowpass filter prototype found in "Elliptic Functions for Filter Design" by H. J. Orchard and Alan N. Willson, Jr. in 
 *  IEEE Transactions on Circuits and Systems-I: Fundamental Theory and Applications, Vol. 44. No. 4,
 *  April, 1997, pp. 273-287.  Permission to port code kindly granted by Alan N. Willson, Jr.
 */
public class AlgorithmEllipticFilter extends AlgorithmBase {

    //~ Instance fields ------------------------------------------------------------------------------------------------\
	// FORTRAN variables
	// inputs
    private int n; // degree of filter, any positive integer > 1
    private double rp; // passband ripple in dB = 10*log(1 + e[0]**2)
    private double k; // elliptic modulus 0.0 <= k <= 1.0
    private boolean displayOutput;
    
    private int no; //  n % 2
    private int n2; // (n + no)/2;
    private int n3; // (n - no)/2
    
    
    private boolean runFORTRAN;
    
    // MATLAB variables
    // inputs
    // Order of an normalized elliptic analog lowpass filter
    //int n;
    // decibels of ripple in the passband
    // double rp;
    // decibels stopband is down
    private double rs;
    
    // outputs
    private double zimag[]; // if n == 1, zimag = null, else declared as new double[2*n3]
    private double preal[]; // if n == 1, preal = new double[1], else declared as new double[2*n3 + no]; if odd last pole is purely real
    private double pimag[]; // if n == 1, pimag = null, else declared as new double[2*n3]
    private double gain[]; // declared as new double[1]
    // Polynomial coefficients for low pass gain
    private double N2[];
    private double N0[];
    private double D2[];
    private double D1[];
    private double D0[];
    private boolean test = false;
    
   /**
    * The  FORTRAN example in the article of n = 7, apd = 0.1dB, k = 0.8 gives in Table VI:
    * Make FORTRAN zeros poles and FORTRAN poles zeros
    *  Poles:
    *  -0.0455944342 + j1.026557002
    *  -0.1713670100 + j0.918389608
    *  -0.3689660125 + j0.603979789
    *  -0.4980421689 + j0.0
    *  
    *  Zeros:
    *  1.268831784
    *  1.467798747
    *  2.384834232
    * 
    * For the MATLAB example rs = 55.43dB.
    * ellipap1 PORT gives:
     Zeros:
	j * -2.384764161974976
	j * 2.384764161974976
	j * -1.4677649005263902
	j * 1.4677649005263902
	j * -1.268807714942811
	j * 1.268807714942811
	Poles:
	-0.368967058832794  + j * -0.6039890555820511
	-0.368967058832794  + j * 0.6039890555820511
	-0.17136300802103874  + j * -0.9183938396880713
	-0.17136300802103874  + j * 0.9183938396880713
	-0.04559255065683074  + j * -1.026556156803157
	-0.04559255065683074  + j * 1.026556156803157
	-0.498050705720998 + j * 0.0
	Gain = 0.011657502783825976
    * 
    */
    

    //~ Constructors ---------------------------------------------------------------------------------------------------
    
    public AlgorithmEllipticFilter() {
    	
    }

    /**
     * 
     * @param n
     * @param apd
     * @param k
     * @param preal
     * @param pimag
     * @param zimag
     * @param displayOutput
     */
    public AlgorithmEllipticFilter(int n, double rp, double k, double preal[], double pimag[], double zimag[], boolean displayOutput) {
        this.n = n;
        this.rp = rp;
        this.k = k;
        this.preal = preal;
        this.pimag = pimag;
        this.zimag = zimag;
        this.displayOutput = displayOutput;
        this.runFORTRAN = true;
    }
    
    public AlgorithmEllipticFilter(int n, double rp, double rs, double zimag[], double preal[], double pimag[], double gain[], boolean displayOutput) {
    	this.n = n;
    	this.rp = rp;
    	this.rs = rs;
    	this.zimag = zimag;
    	this.preal = preal;
    	this.pimag = pimag;
    	this.gain = gain;
    	this.displayOutput = displayOutput;
    	this.runFORTRAN = false;
    }
    //~ Methods --------------------------------------------------------------------------------------------------------

    /**
     * Prepares this class for destruction.
     */
    public void finalize() {
        
        super.finalize();
    }
    
    public void runAlgorithm() {
    	if (test) {
    		runTest();
    	}
    	else if (runFORTRAN) {
    	    runFORTRAN();	
    	}
    	else {
    		ellipap1();
    	}
    }
    
    public void runTest() {
    	double wc;
    	n = 7;
    	rp = 0.1;
    	rs = 55.43;
    	no = n % 2;
    	n3 = (n - no)/2;
    	zimag = new double[2*n3];
    	preal = new double[2*n3 + no];
    	pimag = new double[2*n3];
    	gain = new double[1];
        new AlgorithmEllipticFilter(n, rp, rs, zimag, preal, pimag, gain, true);
        ellipap1();
    	generatePoly();
    	wc = find3dBfrequency();
    	System.out.println("3dB frequency = " + wc);
    	return;
    }

    /**
     * Starts the program.
     */
    public void runFORTRAN() {
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
        int j;
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
        
        no = n % 2;
        n2 = (n + no)/2;
        n3 = (n - no)/2;
        apn = dbn * rp;
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
            g[m2] = 4.0 * Math.pow((ek[m1]/4.0), (n/Math.pow(2.0, i)));
            if (g[m2] > 1.0E-305) {
            	break;
            }
        } // for (i = 0; i <= 10; i++)
        for (j = m2; j >= 1; j--) {
        	g[j-1] = 2.0*Math.sqrt(g[j])/(1.0 + g[j]);
        }
        var = e[0]/g[0];
        asd = 10.0*Math.log10(1.0 + var*var);
        if (displayOutput) {
            System.out.println("Stopband minimum loss = " + asd + " dB");
        }
        Preferences.debug("Stopband minimm loss = " + asd + " dB\n", Preferences.DEBUG_ALGORITHM);
        
        // Compute poles and zeros
        for (j = 1; j <= m2; j++) {
            a = (1.0 + g[j])*e[j-1]/2.0;
            e[j] = a + Math.sqrt(a*a + g[j]);
        }
        u2 = Math.log((1.0 + Math.sqrt(1.0 + e[m2]*e[m2]))/e[m2])/n;
        for (i = 1; i <= n3; i++) {
            u1 = (2*i - 1)*pi2/n;	
            cosu1 = Math.cos(u1);
            coshu2 = Math.cosh(u2);
            sinu1 = Math.sin(u1);
            sinhu2 = Math.sinh(u2);
            denom = cosu1*cosu1*coshu2*coshu2 + sinu1*sinu1*sinhu2*sinhu2;
            creal = -sinu1*sinhu2/denom;
            cimag = -cosu1*coshu2/denom;
            d = 1.0/cosu1;
            for (j = m1; j >= 1; j--) {
            	denom = creal*creal + cimag*cimag;
            	denom2 = (1.0 + ek[j]);
            	creal = (creal - ek[j]*creal/denom)/denom2;
            	cimag = (cimag + ek[j]*cimag/denom)/denom2;
            	d = (d + ek[j]/d)/denom2;
            } // for (j = m1; j >= 1; j--)
            denom = creal*creal + cimag*cimag;
            preal[i-1] = creal/denom;
            pimag[i-1] = -cimag/denom;
            zimag[i-1] = d/ek[0];
        } // for (i = 1; i <= n3; i++)
        if (no == 1) {
        	a = 1.0/Math.sinh(u2);
        	for (j = m1; j >= 1; j--) {
        		a = (a - ek[j]/a)/(1.0 + ek[j]);
        	} // for (j = m1; j >= 1; j--)
        	preal[n2-1] = -1.0/a;
        } // if (no == 1)
        
        // Write output data
        if (displayOutput) {
            System.out.println("Poles:");
        }
        Preferences.debug("Poles:\n", Preferences.DEBUG_ALGORITHM);
        for (i = 0; i < n2; i++) {
        	if (displayOutput) {
        	    System.out.println(preal[i] + "  +  j * " + pimag[i]);
        	}
        	Preferences.debug(preal[i] + "  +  j * " + pimag[i], Preferences.DEBUG_ALGORITHM);
        }
        if (displayOutput) {
            System.out.println("Zero:");
        }
        Preferences.debug("Zero:\n", Preferences.DEBUG_ALGORITHM);
        for (i = 0; i < n3; i++) {
        	if (displayOutput) {
        	    System.out.println(String.valueOf(zimag[i]));
        	}
        	Preferences.debug(String.valueOf(zimag[i]) + "\n", Preferences.DEBUG_ALGORITHM);
        }
        return;
    }
    
    public void ellipap1() {
    	// For n even, n poles and n zeros
    	// For n odd, n poles, 1 of which is real, and n - 1 zeros
    	// As the stop band attenuation goes to infinity, the Elliptic goes to Chebyshev
    	double dbn;
    	double apn;
    	double asn;
    	double e[] = new double[10];
    	double g[] = new double[10];
    	double v;
    	int m2;
    	int index;
    	double ek[] = new double[20];
    	int m1 = 0;
    	int en;
    	double u2;
    	double a;
    	double u1;
    	double cosu1;
        double coshu2;
        double sinu1;
        double sinhu2;
        double denom;
        double creal;
        double cimag;
        double d;
        double denom2;
        double afreal[];
        double afimag[];
        double df[];
        double cr[] = new double[1];
        double ci[] = new double[1];
        double cr2[] = new double[1];
        double ci2[] = new double[1];
        int i;
    	
        if (n == 1) {
        	// Special case; for n == 1, reduces to Chebyshev type 1
        	zimag = null;
        	preal[0] = -Math.sqrt(1.0/(Math.pow(10, (rp/10))-1));
        	pimag = null;
        	gain[0] = -preal[0];
        	return;
        }
        
        dbn = Math.log(10.0)/20.0;
        
        no = n % 2;
        n3 = (n - no)/2;
        apn = dbn * rp;
        asn = dbn * rs;
        e[0] = Math.sqrt(2.0 * Math.exp(apn) * Math.sinh(apn));
        g[0] = e[0]/Math.sqrt(Math.exp(2.0 * asn) - 1.0);
        
        v = g[0];
        m2 = 1;
        while (v > 1.0E-150) {
        	v = v/(1.0 + Math.sqrt(1 - v*v));
        	v = v * v;
        	m2 = m2 + 1;
        	g[m2-1] = v;
        }
        for (index = 0; index <= 10; index++) {
        	m1 = m2 + index;
        	ek[m1-1] = 4.0 * Math.pow(g[m2-1]/4.0, Math.pow(2, index)/n);
        	if (ek[m1-1] < 1.0e-14) {
        		break;
        	}
        }
        for (en = m1-1; en >= 1; en--) {
            ek[en-1] = 2.0 * Math.sqrt(ek[en])/(1.0 + ek[en]);
        }
        
        // Compute poles and zeros
        for (en = 1; en <= m2-1; en++) {
        	a = (1.0 + g[en]) * e[en-1]/2.0;
        	e[en] = a + Math.sqrt(a*a + g[en]);
        }    
        u2 = Math.log((1 + Math.sqrt(1.0 + e[m2-1]*e[m2-1]))/e[m2-1])/n;
        afreal = new double[n3];
        afimag = new double[n3];
        df = new double[n3];
        for (index = 1; index <= n3; index++) {
            u1 = (2*index -1)*Math.PI/(2*n);
            cosu1 = Math.cos(u1);
            coshu2 = Math.cosh(u2);
            sinu1 = Math.sin(u1);
            sinhu2 = Math.sinh(u2);
            denom = cosu1*cosu1*coshu2*coshu2 + sinu1*sinu1*sinhu2*sinhu2;
            creal = -sinu1*sinhu2/denom;
            cimag = -cosu1*coshu2/denom;
            d = 1.0/cosu1;
            for (en = m1-1; en >= 1; en--) {
            	denom = creal*creal + cimag*cimag;
            	denom2 = (1.0 + ek[en]);
            	creal = (creal - ek[en]*creal/denom)/denom2;
            	cimag = (cimag + ek[en]*cimag/denom)/denom2;
            	d = (d + ek[en]/d)/denom2;	
            } // for (en = m1-1; en >= 1; en--) 
            denom = creal*creal + cimag*cimag;
            afreal[index-1] = creal/denom;
            afimag[index-1] = -cimag/denom;
            df[index-1] = d/ek[0];
            preal[2*n3-2*index] = afreal[index-1];
            pimag[2*n3 - 2*index] = -afimag[index-1];
            preal[2*n3 - 2*index + 1] = afreal[index-1];
            pimag[2*n3 - 2*index + 1] = afimag[index-1];
            zimag[2*n3 - 2*index] = -df[index-1];
            zimag[2*n3 - 2*index + 1] = df[index-1];
        } // for (index = 1; index <= n3; index++)
        if (no == 1) {
        	// The real pole for odd pole counts
            a = 1.0/Math.sinh(u2);
            for (en = m1-1; en >= 1; en--) {
            	a = (a - ek[en]/a)/(1.0 + ek[en]);	
            }
            preal[2*n3] = -1.0/a;
        } // if (no == 1)
        // gain
        zmlt(-preal[0],-pimag[0],-preal[1],-pimag[1],cr,ci);
        for (i = 2; i < 2*n3; i++) {
            zmlt(cr[0],ci[0],-preal[i],-pimag[i],cr,ci);	
        }
        if (no == 1) {
        	zmlt(cr[0],ci[0],-preal[2*n3],0.0,cr,ci);
        }
        zmlt(0.0,-zimag[0],0.0,-zimag[1],cr2,ci2);
        for (i = 2; i < 2*n3; i++) {
        	zmlt(cr2[0],ci2[0],0.0,-zimag[i],cr2,ci2);
        }
        zdiv(cr[0],ci[0],cr2[0],ci2[0],cr,ci);
        gain[0] = cr[0];
        //if (no == 0) {
        	// n is even order so patch gain
            // gain[0] = gain[0]/Math.sqrt(1.0 + epsilon*epsilon);
            // Since epsilon is the smallest number we can add to one that will yield an number distinct from one,
            // adding epsilon * epsilon to one will yield one.
        //}
        
        if (displayOutput) {
            System.out.println("Zeros:");
        }
        Preferences.debug("Zeros:\n", Preferences.DEBUG_ALGORITHM);
        for (i = 0; i < 2*n3; i++) {
        	if (displayOutput) {
        	    System.out.println("j * " + zimag[i]);
        	}
        	Preferences.debug("j * " + zimag[i] + "\n", Preferences.DEBUG_ALGORITHM);
        }
        if (displayOutput) {
            System.out.println("Poles:");
        }
        Preferences.debug("Poles:\n", Preferences.DEBUG_ALGORITHM);
        for (i = 0; i < 2*n3; i++) {
        	if (displayOutput) {
        	    System.out.println(preal[i] + "  + j * " + pimag[i]);
        	}
        	Preferences.debug(preal[i] + "  + j * " + pimag[i] + "\n", Preferences.DEBUG_ALGORITHM);
        }
        
        if (no == 1) {
        	if (displayOutput) {
        		System.out.println(preal[2*n3] + " + j * 0.0");
        	}
        	Preferences.debug(preal[2*n3] + " + j * 0.0" + "\n", Preferences.DEBUG_ALGORITHM);
        }
        
        if (displayOutput) {
        	System.out.println("Gain = " + gain[0]);
        }
        Preferences.debug("Gain = " + gain[0] + "\n", Preferences.DEBUG_ALGORITHM);
        return;
    }
    
    public void generatePoly() {
    	int i;
        N2 = new double[n3];
        N0 = new double[n3];
        D2 = new double[n3];
        D1 = new double[n3+no];
        D0 = new double[n3+no];
        for (i = 0; i < n3; i++) {
        	N2[i] = 1.0;
        	D2[i] = 1.0;
        }
        if (runFORTRAN) {
        	for (i = 0; i < n3; i++) {
        		N0[i] = zimag[i]*zimag[i];
        		D1[i] = -2.0*preal[i];
        		D0[i] = preal[i]*preal[i] + pimag[i]*pimag[i];
        	}
        	if (no == 1) {
        		D1[n3] = 1.0;
        		D0[n3] = -preal[n3];
        	}
        }
        else {
        	for (i = 0; i < n3; i++) {
        		N0[i] = zimag[2*i]*zimag[2*i];
        		D1[i] = -2.0*preal[2*i];
        		D2[i] = preal[2*i]*preal[2*i] + pimag[2*i]*pimag[2*i];
        	}
        	if (no == 1) {
        		D1[n3] = 1.0;
        		D0[n3] = -preal[2*n3];
        	}
        }
    }
    
    
    public double findGain(double w) {
    	int i;
    	double gain = 1.0;
    	for (i = 0; i < n3; i++) {
    		gain *= (D2[i]*w*w + D1[i]*w + D0[i]);
    	}
    	if (no == 1) {
    		gain *= (D1[n3]*w + D0[n3]);
    	}
    	for (i = 0; i < n3; i++) {
    		gain /= (N2[i]*w*w + N0[i]);
    	}
    	
    	return gain;
    	
    }
    
    public double find3dBfrequency() {
    	// Find frequency where gain = 1/sqrt(2)
    	double desiredGain = 1.0/Math.sqrt(2.0);
    	double epsilon = 1.0E-10;
    	double wc = 1.0;
    	double wlow = 1.0;
    	double whigh = 1.0;
    	double gain;
    	gain = findGain(wc);
    	if (Math.abs(gain-desiredGain) < epsilon) {
    		return wc;
    	}
    	if (gain < desiredGain) {
    		while (gain < desiredGain) {
    			wlow = wc;
    			wc *= 10.0;
    			gain = findGain(wc);
    		}
    		whigh = wc;
    	}
    	else {
    		while (gain > desiredGain) {
    			whigh = wc;
    			wc /= 10.0;
    			gain = findGain(wc);
    			if (wc == 0) {
    				MipavUtil.displayError("Gain at wc = 0 reaches a minimum of " + gain + " Try increasing order of filter");
    				System.exit(0);
    			}
    		}
    		wlow = wc;
    	}
    	while (Math.abs(gain - desiredGain) >= epsilon) {
    		wc = Math.sqrt(wlow * whigh);
    		gain = findGain(wc);
    		if (gain < desiredGain) {
    			wlow = wc;
    		}
    		else {
    			whigh = wc;
    		}
    	}
    	return wc;
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


    
}

