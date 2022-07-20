package gov.nih.mipav.model.algorithms;


import gov.nih.mipav.view.MipavUtil;
import gov.nih.mipav.view.Preferences;


/**
 * <p>
 * Calculate Jacobian Elliptic functions.
 * </p>
 * 
 * <p>
 * hk is a modulus between 0.0 and 1.0.
 * </p>
 * 
 * <p>
 * The elliptic function sn is defined as the inverse function of the elliptic integral of the first kind.
 * </p>
 * 
 * <p>
 * The elliptic integral of the first kind is the integral from 0 to x of:<br>
 * <code>dt/sqrt((1 - t*t)*(1 - hk*hk*t*t))</code><br>
 * <code>cn (u, hk) = sqrt(1 - sn(u, hk)**2)</code><br>
 * <code>dn (u, hk) = sqrt(1 - hk*hk*sn(u, hk)**2)</code><br>
 * <code>sd (u, hk) = sn(u, hk)/dn(u, hk)</code>
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
public class JacobianElliptic {
    // ~ Static fields/initializers
    // -------------------------------------------------------------------------------------

    public static final int REAL_VERSION = 1;

    public static final int COMPLEX_VERSION = 2;

    private int kind;

    public static final int SN = 1;

    public static final int CN = 2;

    public static final int DN = 3;

    public static final int SD = 4;

    /** Tells whether real number or complex number version */
    private int version;

    /** Outputted results */
    /** sn (u, hk) */
    private double esn[];

    /** cn (u, hk) */
    private double ecn[];

    /** dn (u, hk) */
    private double edn[];

    /** sd (u, hk) = sn (u, hk)/ dn(u, hk) */
    private double esd[];

    /** phi (in degrees) */
    private double dph[];

    /** Input argument */
    private double u;

    /** Input modulus */
    private double hk;

    /** Input argument */
    private double realZ;

    private double imagZ;

    /** outputted result */
    private double realResult[];

    private double imagResult[];

    // ~ Constructors
    // ---------------------------------------------------------------------------------------------------
    
    public JacobianElliptic() {
    	
    }

    /**
     * @param u input argument
     * @param hk input modulus 0 <= hk <= 1
     * @param sn outputted sn function
     * @param cn outputted cn function
     * @param dn outputted dn function
     * @param sd outputted sd function
     * @param dphi outputted phi in degrees
     */
    public JacobianElliptic(final double u, final double hk, final double esn[], final double ecn[],
            final double edn[], final double esd[], final double dph[]) {
        this.u = u;
        this.hk = hk;
        this.esn = esn;
        this.ecn = ecn;
        this.edn = edn;
        this.esd = esd;
        this.dph = dph;
        this.version = JacobianElliptic.REAL_VERSION;
    }

    /**
     * @param realZ input real part of argument
     * @param imagZ input imaginary part of argument
     * @param hk input modulus 0 <= hk <= 1
     * @param kind Type of function to return
     * @param realResult real part of outputted result
     * @param imagResult imaginary part of outputted result
     */
    public JacobianElliptic(final double realZ, final double imagZ, final double hk, final int kind,
            final double realResult[], final double imagResult[]) {
        this.realZ = realZ;
        this.imagZ = imagZ;
        this.hk = hk;
        this.kind = kind;
        this.realResult = realResult;
        this.imagResult = imagResult;
        this.version = JacobianElliptic.COMPLEX_VERSION;
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
        if (version == JacobianElliptic.REAL_VERSION) {
            realArgument();
        }

        else if (version == JacobianElliptic.COMPLEX_VERSION) {
            complexArgument();
        } else {
            MipavUtil.displayError("Illegal argument");
            return;
        }
    } // run()
    
    public void testrealArgument() {
    	dph = new double[1];
    	esn = new double[1];
    	ecn = new double[1];
    	edn = new double[1];
    	esd = new double[1];
    	int errorsDetected = 0;
    	boolean errorFound;
    	int i;
    	double u0;
    	double K = 1.596242222;
    	double testu0[] = new double[]{0.0,0.1,0.2,0.3,0.4,0.5,0.6,0.7,0.8,0.9,1.0,1.1,1.2,1.3,1.4,1.5,1.6,1.7,1.8,1.9,
    			2.0,2.1,2.2,2.3,2.4,2.5,2.6,2.7,2.8,2.9,3.0,3.1,3.2,3.3,3.4,3.5,3.6,3.7,3.8,3.9,4.0,
    			0.0,0.1,0.2,0.3,0.4,0.5,0.6,0.7,0.8,0.9,1.0,1.1,1.2,1.3,1.4,1.5,1.6,1.7,1.8,1.9,
        			2.0,2.1,2.2,2.3,2.4,2.5,2.6,2.7,2.8,2.9,3.0,3.1,3.2,3.3,3.4,3.5,3.6,3.7,3.8,3.9,4.0};
    	double testhk[] = new double[]{0.25,0.25,0.25,0.25,0.25,0.25,0.25,0.25,0.25,0.25,0.25,0.25,0.25,0.25,0.25,0.25,
    			0.25,0.25,0.25,0.25,0.25,0.25,0.25,0.25,0.25,0.25,0.25,0.25,0.25,0.25,0.25,0.25,0.25,0.25,0.25,0.25,
    			0.25,0.25,0.25,0.25,0.25,0.5,0.5,0.5,0.5,0.5,0.5,0.5,0.5,0.5,0.5,0.5,0.5,0.5,0.5,0.5,0.5,0.5,0.5,0.5,
    			0.5,0.5,0.5,0.5,0.5,0.5,0.5,0.5,0.5,0.5,0.5,0.5,0.5,0.5,0.5,0.5,0.5,0.5,0.5,0.5,0.5,0.5};
    	// For k = 0.25, K = K(k) = 1.596242222
    	// For k - 0.50, K = K(k) = 1.685750355
    	double anssn[] = new double[]{0.0000000,.1589056,.3135378,.4598096,.5939825,.7127879,.8134972,.8939435,
    			.9525064,.9880738,1.0000000,.9880738,.9525064,.8939435,.8134972,.7127879,.5939825,.4598096,.3135378,
    			.1589056,.0000000,-.1589056,-.3135378,-.4598096,-.5939825,-.7127879,-.8134972,-.8939435,-.9525064,
    			-.9880738,-1.0000000,-.9880738,-.9525064,-.8939435,-.8134972,-.7127879,-.5939825,-.4598096,-.3135378,
    			-.1589056,.0000000,.0000000,.1675822,.3293274,.4799676,.6152323,.7320508,.8285204,.9036950,.9572829,
    			.9893373,1.0000000,.9893373,.9572829,.9036950,.8285204,.7320508,.6152323,.4799676,.3293274,.1675822,
    			.0000000,-.1675822,-.3293274,-.4799676,-.6152323,-.7320508,-.8285204,-.9036950,-.9572829,-.9893373,
    			-1.0000000,-.9893373,-.9572829,-.9036950,-.8285204,-.7320508,-.6152323,-.4799676,-.3293274,
    			-.1675822,.0000000};
    	double anscn[] = new double[]{1.0000000,.9872938,.9495757,.8880175,.8044780,.7013796,.5815689,.4481797,
    			.3045186,.1539812,.0000000,-.1539812,-.3045186,-.4481797,-.5815689,-.7013796,-.8044780,-.8880175,
    			-.9495757,-.9872938,-1.0000000,-.9872938,-.9495757,-.8880175,-.8044780,-.7013796,-.5815689,-.4481797,
    			-.3045186,-.1539812,.0000000,.1539812,.3045186,.4481797,.5815689,.7013796,.8044780,.8880175,
    			.9495757,.9872938,1.0000000,1.0000000,.9858581,.9442158,.8772862,.7883459,.6812500,.5599589,
    			.4281768,.2891529,.1456426,.0000000,-.1456426,-.2891529,-.4281768,-.5599589,-.6812500,-.7883459,
    			-.8772862,-.9442158,-.9858581,-1.0000000,-.9858581,-.9442158,-.8772862,-.7883459,-.6812500,-.5599589,
    			-.4281768,-.2891529,-.1456426,.0000000,.1456426,.2891529,.4281768,.5599589,.6812500,.7883459,
    			.8772862,.9442158,.9858581,1.0000000};
    	double ansdn[] = new double[]{1.0000000,.9992106,.9969232,.9933710,.9889131,.9839948,.9791011,.9747072,.9712341,
    			.9690108,.9682458,.9690108,.9712341,.9747072,.9791011,.9839948,.9889131,.9933710,.9969232,.9992106,
    			1.0000000,.9992106,.9969232,.9933710,.9889131,.9839948,.9791011,.9747072,.9712341,.9690108,
    			.9682458,.9690108,.9712341,.9747072,.9791011,.9839948,.9889131,.9933710,.9969232,.9992106,1.0000000,
    			1.0000000,.9964833,.9863498,.9707769,.9515105,.9306049,.9101585,.8920952,.8780105,.8690817,
    			.8660254,.8690817,.8780105,.8920952,.9101585,.9306049,.9515105,.9707769,.9863498,.9964833,
    			1.0000000,.9964833,.9863498,.9707769,.9515105,.9306049,.9101585,.8920952,.8780105,.8690817,
    			.8660254,.8690817,.8780105,.8920952,.9101585,.9306049,.9515105,.9707769,.9863498,.9964833,1.0000000};
    	for (i = 0; i < testu0.length; i++) {
    		errorFound = false;
    		u0 = testu0[i];
    		hk = testhk[i];
    		if (hk == 0.25) {
    			K = 1.596242222;
    		}
    		else if (hk == 0.5) {
    			K = 1.685750355;
    		}
    		u = u0 * K;
    		realArgument();
    		Preferences.debug("u0 = " + u0 + " hk = " + hk + " sn u result = " + esn[0] + " sn u answer = " + anssn[i] +
    				" cn u result = " + ecn[0] + " cn u answer = " + anscn[i] + " dn u result = " + edn[0] +
    				" dn u answer = " + ansdn[i] + "\n", Preferences.DEBUG_ALGORITHM);
    		if (Double.isNaN(esn[0])) {
    			errorFound = true;
    			Preferences.debug("Error found sn u is NaN\n", Preferences.DEBUG_ALGORITHM);
    		}
    		else if (anssn[i] != 0.0) {
		        if ((esn[0]/anssn[i] < 1-1.0E-6) || (esn[0]/anssn[i] > 1+1.0E-6)) {
		     		   Preferences.debug("Error detected in sn u\n",Preferences.DEBUG_ALGORITHM);
		     		   errorFound = true;
		        }
	        }
    		 else if (Math.abs(esn[0]) > 1.0E-6) {
         		Preferences.debug("Error detected in sn u\n",Preferences.DEBUG_ALGORITHM);
 	     		errorFound = true;
 	        }
    		if (Double.isNaN(ecn[0])) {
    			errorFound = true;
    			Preferences.debug("Error found cn u is NaN\n", Preferences.DEBUG_ALGORITHM);
    		}
    		else if (anscn[i] != 0.0) {
		        if ((ecn[0]/anscn[i] < 1-1.0E-6) || (ecn[0]/anscn[i] > 1+1.0E-6)) {
		     		   Preferences.debug("Error detected in cn u\n",Preferences.DEBUG_ALGORITHM);
		     		   errorFound = true;
		        }
	        }
    		 else if (Math.abs(ecn[0]) > 1.0E-6) {
         		Preferences.debug("Error detected in cn u\n",Preferences.DEBUG_ALGORITHM);
 	     		errorFound = true;
 	        }
    		if (Double.isNaN(edn[0])) {
    			errorFound = true;
    			Preferences.debug("Error found dn u is NaN \n", Preferences.DEBUG_ALGORITHM);
    		}
    		else if (ansdn[i] != 0.0) {
		        if ((edn[0]/ansdn[i] < 1-1.0E-6) || (edn[0]/ansdn[i] > 1+1.0E-6)) {
		     		   Preferences.debug("Error detected in dn u\n",Preferences.DEBUG_ALGORITHM);
		     		   errorFound = true;
		        }
	        }
    		 else if (Math.abs(edn[0]) > 1.0E-6) {
         		Preferences.debug("Error detected in dn u\n",Preferences.DEBUG_ALGORITHM);
 	     		errorFound = true;
 	        }
    		if (errorFound) {
    			errorsDetected++;
    		}
    	}
    	System.out.println(errorsDetected + " errors detected in " + testu0.length + " tests of realArgument");
    	Preferences.debug(errorsDetected + " erors detected in " + testu0.length + " tests of realArgument\n",Preferences.DEBUG_ALGORITHM);
    }

    /**
     * This is a port of subroutine JELP from Computation of Special Functions by Shanjie Zhang and Jianming Jin, pp.
     * 671-672. Computes Jacobian elliptic functions sn(u, hk), cn(u, hk), dn(u,hk) and sd(u, hk)
     * 
     */
    private void realArgument() {
        double a0;
        double b0;
        int n;
        double a = 1.0;
        double b;
        double c;
        final double r[] = new double[40];
        double dn;
        int k;
        double t;
        double sa;
        double d = 0.0;

        a0 = 1.0;
        b0 = Math.sqrt(1.0 - hk * hk);
        for (n = 1; n <= 40; n++) {
            a = (a0 + b0) / 2.0;
            b = Math.sqrt(a0 * b0);
            c = (a0 - b0) / 2.0;
            r[n - 1] = c / a;
            if (c < 1.0E-7) {
                break;
            }
            a0 = a;
            b0 = b;
        } // for (n = 1; n <= 40; n++)
        dn = Math.pow(2.0, n) * a * u;
        for (k = n - 1; k >= 0; k--) {
            t = r[k] * Math.sin(dn);
            sa = Math.atan(t / Math.sqrt(Math.abs(1.0 - t * t)));
            d = 0.5 * (dn + sa);
            dn = d;
        } // for (k = n-1; k >= 0; k--)
        dph[0] = d * 180/Math.PI;
        esn[0] = Math.sin(d);
        ecn[0] = Math.cos(d);
        edn[0] = Math.sqrt(1.0 - hk * hk * esn[0] * esn[0]);
        esd[0] = esn[0] / edn[0];
        return;
    } // realArgument

    /**
     * From Handbook of Mathematical Functions with Formulas, Graphs, and Mathematical Tables edited by Milton
     * Abramowitz and Irene A. Stegun, Chapter 16 Jacobian Elliptic Functions and Theta Functions, Section 16.21 Complex
     * Arguments s = sn(realZ|hk), c = cn(realZ|hk), d = dn(realZ|hk) hkp = sqrt(1 - hk*hk) s1 = sn(imagZ|hkp), c1 =
     * cn(imagZ|hkp), d1 = dn(imagZ|hkp) sn(realZ + i*imagZ|hk) = (s*d1 + i*c*d*s1*c1)/(c1*c1 + m*s*s*s1*s1) cn(realZ +
     * i*imagZ|hk) = (c*c1 - i*s*d*s1*d1)/(c1*c1 + m*s*s*s1*s1) dn(realZ + i*imagZ|hk) = (d*c1*d1 - i*m*s*c*s1)/(c1*c1 +
     * m*s*s*s1*s1)
     */
    private void complexArgument() {
        JacobianElliptic je;
        final double esnr[] = new double[1];
        final double ecnr[] = new double[1];
        final double ednr[] = new double[1];
        final double esdr[] = new double[1];
        final double dphr[] = new double[1];
        final double esni[] = new double[1];
        final double ecni[] = new double[1];
        final double edni[] = new double[1];
        final double esdi[] = new double[1];
        final double dphi[] = new double[1];
        double denom;
        double hkp;
        hkp = Math.sqrt(1.0 - hk * hk);
        je = new JacobianElliptic(realZ, hk, esnr, ecnr, ednr, esdr, dphr);
        je.run();
        je = new JacobianElliptic(imagZ, hkp, esni, ecni, edni, esdi, dphi);
        je.run();
        denom = ecni[0] * ecni[0] + hk * hk * esnr[0] * esnr[0] * esni[0] * esni[0];
        if (kind == JacobianElliptic.SN) {
            realResult[0] = esnr[0] * edni[0] / denom;
            imagResult[0] = ecnr[0] * ednr[0] * esni[0] * ecni[0] / denom;
        } // if (kind == SN)
        else if (kind == JacobianElliptic.CN) {
            realResult[0] = ecnr[0] * ecni[0] / denom;
            imagResult[0] = -esnr[0] * ednr[0] * esni[0] * edni[0] / denom;
        } // else if (kind == CN)
        else if (kind == JacobianElliptic.DN) {
            realResult[0] = ednr[0] * ecni[0] * edni[0] / denom;
            imagResult[0] = -hk * hk * esnr[0] * ecnr[0] * esni[0] / denom;
        } // else if kind == DN)
        else if (kind == JacobianElliptic.SD) {
            zdiv(esnr[0] * edni[0], ecnr[0] * ednr[0] * esni[0] * ecni[0], ednr[0] * ecni[0] * edni[0], -hk * hk
                    * esnr[0] * ecnr[0] * esni[0], realResult, imagResult);
        } // else if (kind == SD)
        return;
    } // complexArgument

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

}
