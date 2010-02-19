package gov.nih.mipav.model.algorithms;

import gov.nih.mipav.view.*;

/** 
 Calculate Jacobian Elliptic funtions
 hk is a modulus between 0.0 and 1.0
 The elliptic function sn is defined as the inverse function of the elliptic integral of the first kind.
 The elliptic integral of the first kind is the integral from 0 to x of:
 dt/sqrt((1 - t*t)*(1 - hk*hk*t*t))
 cn (u, hk) = sqrt(1 - sn(u, hk)**2)
 dn (u, hk) = sqrt(1 - hk*hk*sn(u, hk)**2)
 sd (u, hk) = sn(u, hk)/dn(u, hk)
*/

public class JacobianElliptic {
//  ~ Static fields/initializers -------------------------------------------------------------------------------------
    
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
    
    
//  ~ Constructors ---------------------------------------------------------------------------------------------------

    /**
     * @param u input argument
     * @param hk input modulus 0 <= hk <= 1
     * @param sn outputted sn function
     * @param cn outputted cn function
     * @param dn outputted dn function
     * @param sd outputted sd function
     * @param dphi outputted phi in degrees
     */
    public JacobianElliptic(double u, double hk, double esn[], double ecn[], double edn[], 
                            double esd[], double dph[]) {
        this.u = u;
        this.hk = hk;
        this.esn = esn;
        this.ecn = ecn;
        this.edn = edn;
        this.esd = esd;
        this.dph = dph;
        this.version = REAL_VERSION;
    }
    
    /**
     * @param realZ input real part of argument
     * @param imagZ input imaginary part of argument
     * @param hk    input modulus 0 <= hk <= 1
     * @param kind  Type of function to return
     * @param realResult real part of outputted result
     * @param imagResult imaginary part of outputted result
     */
    public JacobianElliptic(double realZ, double imagZ, double hk, int kind, 
                            double realResult[], double imagResult[]) {
        this.realZ = realZ;
        this.imagZ = imagZ;
        this.hk = hk;
        this.kind = kind;
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
     * This is a port of subroutine JELP from Computation of Special Functions
     * by Shanjie Zhang and Jianming Jin, pp. 671-672.
     * Computes Jacobian elliptic functions sn(u, hk), cn(u, hk), dn(u,hk) and sd(u, hk)
     *
     */
    private void realArgument() {
        double a0;
        double b0;
        int n;
        double a = 1.0;
        double b;
        double c;
        double r[]= new double[40];
        double dn;
        int k;
        double t;
        double sa;
        double d = 0.0;
        
        a0 = 1.0;
        b0 = Math.sqrt(1.0 - hk*hk);
        for (n = 1; n <= 40; n++) {
            a = (a0 + b0)/2.0;
            b = Math.sqrt(a0*b0);
            c = (a0 - b0)/2.0;
            r[n-1] = c/a;
            if (c < 1.0E-7) {
                break;
            }
            a0 = a;
            b0 = b;
        } // for (n = 1; n <= 40; n++)
        dn = Math.pow(2.0,n)*a*u;
        for (k = n-1; k >= 0; k--) {
            t = r[k]*Math.sin(dn);
            sa = Math.atan(t/Math.sqrt(Math.abs(1.0-t*t)));
            d = 0.5*(dn+sa);
            dn = d;
        } // for (k = n-1; k >= 0; k--)
        dph[0] = d * 180.0/Math.PI;
        esn[0] = Math.sin(d);
        ecn[0] = Math.cos(d);
        edn[0] = Math.sqrt(1.0 - hk*hk*esn[0]*esn[0]);
        esd[0] = esn[0]/edn[0];
        return;
    } // realArgument
    
    /**
     *  From Handbook of Mathematical Functions with Formulas, Graphs, and Mathematical
     *  Tables edited by Milton Abramowitz and Irene A. Stegun, Chapter 16 Jacobian 
     *  Elliptic Functions and Theta Functions, Section 16.21 Complex Arguments
     *  s = sn(realZ|hk), c = cn(realZ|hk), d = dn(realZ|hk)
     *  hkp = sqrt(1 - hk*hk)
     *  s1 = sn(imagZ|hkp), c1 = cn(imagZ|hkp), d1 = dn(imagZ|hkp)
     *  sn(realZ + i*imagZ|hk) = (s*d1 + i*c*d*s1*c1)/(c1*c1 + m*s*s*s1*s1)
     *  cn(realZ + i*imagZ|hk) = (c*c1 - i*s*d*s1*d1)/(c1*c1 + m*s*s*s1*s1)
     *  dn(realZ + i*imagZ|hk) = (d*c1*d1 - i*m*s*c*s1)/(c1*c1 + m*s*s*s1*s1)
     */
    private void complexArgument() {
        JacobianElliptic je;
        double esnr[] = new double[1];
        double ecnr[] = new double[1];
        double ednr[] = new double[1];
        double esdr[] = new double[1];
        double dphr[] = new double[1];
        double esni[] = new double[1];
        double ecni[] = new double[1];
        double edni[] = new double[1];
        double esdi[] = new double[1];
        double dphi[] = new double[1];
        double denom;
        double hkp;
        hkp = Math.sqrt(1.0 - hk*hk);
        je = new JacobianElliptic(realZ, hk, esnr, ecnr, ednr, esdr, dphr);
        je.run();
        je = new JacobianElliptic(imagZ, hkp, esni, ecni, edni, esdi, dphi);
        je.run();
        denom = ecni[0]*ecni[0] + hk*hk*esnr[0]*esnr[0]*esni[0]*esni[0];
        if (kind == SN) {
            realResult[0] = esnr[0]*edni[0]/denom;
            imagResult[0] = ecnr[0]*ednr[0]*esni[0]*ecni[0]/denom;
        } // if (kind == SN)
        else if (kind == CN) {
            realResult[0] = ecnr[0]*ecni[0]/denom;
            imagResult[0] = -esnr[0]*ednr[0]*esni[0]*edni[0]/denom;
        } // else if (kind == CN)
        else if (kind == DN) {
            realResult[0] = ednr[0]*ecni[0]*edni[0]/denom;
            imagResult[0] = -hk*hk*esnr[0]*ecnr[0]*esni[0]/denom;
        } // else if kind == DN)
        else if (kind == SD) {
            zdiv(esnr[0]*edni[0],ecnr[0]*ednr[0]*esni[0]*ecni[0],ednr[0]*ecni[0]*edni[0],-hk*hk*esnr[0]*ecnr[0]*esni[0],
                 realResult, imagResult);
        } // else if (kind == SD)
        return;
    } // complexArgument
    
    
    
    /**
     * zabs computes the absolute value or magnitude of a double precision complex variable zr + j*zi.
     *
     * @param   zr  double
     * @param   zi  double
     *
     * @return  double
     */
    private double zabs(double zr, double zi) {
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
     * @param  ar  double
     * @param  ai  double
     * @param  br  double
     * @param  bi  double
     * @param  cr  double[]
     * @param  ci  double[]
     */
    private void zdiv(double ar, double ai, double br, double bi, double[] cr, double[] ci) {
        double bm, cc, cd, ca, cb;

        bm = 1.0 / zabs(br, bi);
        cc = br * bm;
        cd = bi * bm;
        ca = ((ar * cc) + (ai * cd)) * bm;
        cb = ((ai * cc) - (ar * cd)) * bm;
        cr[0] = ca;
        ci[0] = cb;

        return;
    }

    
}