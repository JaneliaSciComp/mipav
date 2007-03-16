package gov.nih.mipav.model.algorithms;


import gov.nih.mipav.view.*;
import gov.nih.mipav.view.*;


/**
 * 
 * @author ilb
 * The code for EllipticIntegralType = COMPLETE1, COMPLETE2, INCOMPLETE1, and INCOMPLETE2 is ported from a C++ program in 
 * Figures 1 and 2 of "Numerical Calculation of the Elliptic Integrals of the First and Second Kinds with Complex Modulus"
 * by Tohru Morita, Interdisciplinary Information Sciences, Vol. 6, No. 1, 2000, pp. 67-74.
 */

public class EllipticIntegral {

    //~ Static fields/initializers -------------------------------------------------------------------------------------

    // K(k) COMPLETE1
    
    // E(k) COMPLETE2
    
    // F(phi, k) INCOMPLETE1
    
    // E(phi, k) INCOMPLETE2
    
    //  ~ Instance fields ------------------------------------------------------------------------------------------------
    
    // Number of repetitions = N - 1
    private int N = 12; // 6
    
    private double TINY = 3.0E-138;  // 3.0E-38
    
    private double BIG = 3.0E137; // 3.0E37
    
    private double C1 = 1.0/24.0;
    
    private double C2 = 0.1;
    
    private double C3 = 3.0/44.0;
    
    private double C4 = 1.0/14.0;
    
    private double TINYd = 1.0E-125; // 1.0E-25
    
    private double BIGd = 4.5E121; // 4.5E21
    
    private double C1d = 3.0/14.0;
    
    private double C2d = 1.0/6.0;
    
    private double C3d = 9.0/22.0;
    
    private double C4d = 3.0/26.0;
    
    private double C5d = 0.25 * C3d;
    
    private double C6d = 1.5 * C4d;
    
    private double pihf = 0.5 * Math.PI;
    
    private double RERR = 1.0E-12; // 1.0E-6
    
    private double ERRTOL = 8.0E-3; // 8.0E-2
    
    private double ERRTOLb = 1.5E-6; // 1.5E-3
    
    private boolean complete;
    
    private double firstr[];
    
    private double firsti[];
    
    private double secondr[];
    
    private double secondi[];
    
    private int[] errorFlag;
    
    // k is the modulus
    // k', the complementary modulus = sqrt(1 - k*k)
    private boolean complementaryModulusUsed;
    
    // If false, function used
    // If true, analytic continuation of function used
    private boolean analyticContinuationUsed;
    
    // Real part of modulus or complementary modulus
    private double modr;
    
    // Imaginary part of modulus or complementary modulus
    private double modi;
    
    private double ckr[] = new double[1];
    
    private double cki[] = new double[1];
    
    private double akr[] = new double[1];
    
    private double aki[] = new double[1];
    
    // For complete integrals phir = PI/2, phii = 0
    private double phir;
    
    private double phii;
    
    private int sgnck;
    
    private int nrep;
    
    private boolean runcel12;
    
    private boolean useStandardMethod;
    
    private double rfr[] = new double[1];
    private double rfi[] = new double[1];
    private double rdr[] = new double[1];
    private double rdi[] = new double[1];
    
    //  ~ Constructors ---------------------------------------------------------------------------------------------------
    
    // Constructor only for complete integrals with phi = PI/2.
    // The method called is based on the arithmetic-geometric mean procedure
    // If only the complete integrals are calculated, the arithmetic-geometric mean
    // procdure is superior to the 2 methods based on Carlson's algorithm
    public EllipticIntegral(double modr, double modi, boolean complementaryModulusUsed, 
                            boolean analyticContinuationUsed, double firstr[], double firsti[],
                            double secondr[], double secondi[], int[] errorFlag) {
        complete = true;
        runcel12 = true;
        this.modr = modr;
        this.modi = modi;
        this.complementaryModulusUsed = complementaryModulusUsed;
        this.analyticContinuationUsed = analyticContinuationUsed;
        this.firstr = firstr;
        this.firsti = firsti;
        this.secondr = secondr;
        this.secondi = secondi;
        this.errorFlag = errorFlag;
        phir = pihf;
        phii = 0.0;
    }
    
    // When calculating the complete integrals K(k) and E(K), we can make complete true or false, except
    // when abs(k') <= 1.0E-10, in which case complete must be true and the complemetary modulus must be
    // used
    // The only merit of the alternative method over the standard method is that the code is simpler
    // Both methods are based on modifications of Carlson's algortihm
    public EllipticIntegral(boolean complete, double modr, double modi, boolean complementaryModulusUsed, 
            boolean analyticContinuationUsed, double phir, double phii, double firstr[], 
            double firsti[], double secondr[], double secondi[], boolean useStandardMethod, int[] errorFlag) {
        this.complete = complete;
        this.modr = modr;
        this.modi = modi;
        this.complementaryModulusUsed = complementaryModulusUsed;
        this.analyticContinuationUsed = analyticContinuationUsed;
        this.phir = phir;
        this.phii = phii;
        this.firstr = firstr;
        this.firsti = firsti;
        this.secondr = secondr;
        this.secondi = secondi;
        this.useStandardMethod = useStandardMethod;
        this.errorFlag = errorFlag;
    }
    
    public void run() {
        
        if (errorFlag == null) {
            MipavUtil.displayError("Array errorFlag must not be null");

            return;
        }
        
        if (complete && ((phir != pihf) || (phii != 0.0))) {
            MipavUtil.displayError("Complete integrals must have phir = PI/2 and phii = 0");
            errorFlag[0] = 1;
            return;
        }
        
        if (complementaryModulusUsed) {
            ckr[0] = modr;
            cki[0] = modi;
            sqrtc(1.0 - ckr[0]*ckr[0] + cki[0]*cki[0], -2.0*ckr[0]*cki[0], akr, aki);
            if (modr < 0.0) {
                sgnck = -1;    
            }
            else {
                sgnck = 1;
            }
        } // if (complementaryModulusUsed)
        else { // modulus used
            akr[0] = modr;
            aki[0] = modi;
            sqrtc(1.0 - akr[0]*akr[0] + aki[0]*aki[0], -2.0*akr[0]*aki[0], ckr, cki);
            if (analyticContinuationUsed) {
                sgnck = -1;
                ckr[0] = -ckr[0];
                cki[0] = -cki[0];
            }
            else {
                sgnck = 1;
            }
        } // modulus used
        
        if (runcel12) {
            cel12(ckr[0], cki[0]);
            return;
        }
        else {
            if (complete) {
                sgnck = 0;
            }
            ell12(phir, phii, akr[0], aki[0], ckr[0], cki[0], sgnck);
        }
        
    } // public void run()
    
    // Routine cel12 only calculates complete integrals
    private void cel12(double ckr, double cki) {
        double kcr[] = new double[1];
        double kci[] = new double[1];
        double tar[] = new double[1];
        double tai[] = new double[1];
        double air[] = new double[1];
        double aii[] = new double[1];
        double sbr;
        double sbi;
        double aar[] = new double[1];
        double aai[] = new double[1];
        double sar[] = new double[1];
        double sai[] = new double[1];
        double quotr[] = new double[1];
        double quoti[] = new double[1];
        double tr[] = new double[1];
        double ti[] = new double[1];
        
        kcr[0] = ckr;
        kci[0] = cki;
        zmlt(ckr, cki, ckr, cki, tar, tai);
        air[0] = 1.0;
        aii[0] = 0.0;
        sbr = 1.0;
        sbi = 0.0;
        nrep = 0;
        while (true) {
            zdiv(2.0, 0.0, 1.0+kcr[0], kci[0], aar, aai);
            zmlt(air[0], aii[0], aar[0], aai[0], air, aii);
            zmlt((sbr + tar[0]), (sbi + tai[0]), aar[0]/2.0, aai[0]/2.0, sar, sai);
            zdiv(sar[0], sai[0], sbr, sbi, quotr, quoti);
            if ((zabs(1.0 - aar[0], -aai[0]) <RERR) && (zabs(1.0 - quotr[0], -quoti[0])< RERR)) {
                break;
            }
            zmlt(kcr[0], kci[0], sbr, sbi, tr, ti);
            tar[0] = tar[0] + tr[0];
            tai[0] = tai[0] + ti[0];
            zmlt(tar[0], tai[0], aar[0], aai[0], tar, tai);
            zmlt(tar[0], tai[0], aar[0]/2.0, aai[0]/2.0, tar, tai);
            sbr = sar[0];
            sbi = sai[0];
            sqrtc(kcr[0], kci[0], kcr, kci);
            zmlt(kcr[0], kci[0], aar[0], aai[0], kcr, kci);
            nrep++;
        } // while (true)
        firstr[0] = pihf * air[0];
        firsti[0] = pihf * aii[0];
        secondr[0] = pihf * sar[0];
        secondi[0] = pihf * sai[0];
        return;
    } // cel12
    
    private void ell12(double phir, double phii, double akr, double aki, double ckr,
                       double cki, int sgnck) {
        double cr[] = new double[1];
        double ci[] = new double[1];
        double qrtr[] = new double[1];
        double qrti[] = new double[1];
        double sr[] = new double[1];
        double si[] = new double[1];
        double sk2r[] = new double[1];
        double sk2i[] = new double[1];
        double qr;
        double qi;
        
        if (sgnck == 0) {
            cr[0] = 0.0;
            ci[0] = 0.0;
            qrtr[0] = ckr;
            qrti[0] = cki;
        } // if (sgnck == 0)
        else {
            zsin(phir, phii, sr, si);
            zcos(phir, phii, cr, ci);
            zmlt(sr[0], si[0], akr, aki, sk2r, sk2i);
            zmlt(sk2r[0], sk2i[0], sk2r[0], sk2i[0], sk2r, sk2i);
            qr = 1.0 - sk2r[0];
            qi = -sk2i[0];
            sqrtc(qr, qi, qrtr, qrti);
            if (sgnck < 0) {
                qrtr[0] = -qrtr[0];
                qrti[0] = -qrti[0];
            }
        }
        if (useStandardMethod) {
            rfrd(cr[0], ci[0], qrtr[0], qrti[0]);    
        }
        else {
            rfrdb(cr[0], ci[0], qrtr[0], qrti[0]);    
        }
        zmlt(sr[0], si[0], rfr[0], rfi[0], firstr, firsti);
        zmlt(sr[0], si[0], sk2r[0], sk2i[0], secondr, secondi);
        zmlt(secondr[0], secondi[0], rdr[0]/3.0, rdi[0]/3.0, secondr, secondi);
        secondr[0] = firstr[0] - secondr[0];
        secondi[0] = firsti[0] - secondi[0];
    } // ell12
    
    private void rfrd(double xrtr, double xrti, double yrtr, double yrti) {
       double sqrtxr[] = new double[1];
       double sqrtxi[] = new double[1];
       double sqrtyr[] = new double[1];
       double sqrtyi[] = new double[1];
       double xtr[] = new double[1];
       double xti[] = new double[1];
       double ytr[] = new double[1];
       double yti[] = new double[1];
       double facr[] = new double[1];
       double faci[] = new double[1];
       double sumdr;
       double sumdi;
       double facdr[] = new double[1];
       double facdi[] = new double[1];
       double alambr;
       double alambi;
       double tr1[] = new double[1];
       double ti1[] = new double[1];
       double tr2[] = new double[1];
       double ti2[] = new double[1];
       double zir[] = new double[1];
       double zii[] = new double[1];
       double sqrtzir[] = new double[1];
       double sqrtzii[] = new double[1];
       double delxr;
       double delxi;
       double delyr;
       double delyi;
       double delzr;
       double delzi;
       double avir[] = new double[1];
       double avii[] = new double[1];
       double delxdr;
       double delxdi;
       double delydr;
       double delydi;
       double delzdr;
       double delzdi;
       double e2r;
       double e2i;
       double e3r[] = new double[1];
       double e3i[] = new double[1];
       double avidr[] = new double[1];
       double avidi[] = new double[1];
       double ear[] = new double[1];
       double eai[] = new double[1];
       double ebr[] = new double[1];
       double ebi[] = new double[1];
       double ecr;
       double eci;
       double edr;
       double edi;
       double eer;
       double eei;
       
       sqrtxr[0] = xrtr;
       sqrtxi[0] = xrti;
       sqrtyr[0] = yrtr;
       sqrtyi[0] = yrti;
       zmlt(xrtr, xrti, xrtr, xrti, xtr, xti);
       zmlt(yrtr, yrti, yrtr, yrti, ytr, yti);
       facr[0] = 1.0;
       faci[0] = 0.0;
       sumdr = 0.0;
       sumdi = 0.0;
       facdr[0] = 1.0;
       facdi[0] = 0.0;
       nrep = 0;
       while (true) {
           zmlt(sqrtxr[0], sqrtxi[0], sqrtyr[0], sqrtyi[0], tr1, ti1);
           alambr = tr1[0] + sqrtxr[0] + sqrtyr[0];
           alambi = ti1[0] + sqrtxi[0] + sqrtyi[0];
           zdiv(1.0, 0.0, 1.0 + alambr, alambi, zir, zii);
           xtr[0] = xtr[0] + alambr;
           xti[0] = xti[0] + alambi;
           ytr[0] = ytr[0] + alambr;
           yti[0] = yti[0] + alambi;
           zmlt(xtr[0], xti[0], zir[0], zii[0], xtr, xti);
           zmlt(ytr[0], yti[0], zir[0], zii[0], ytr, yti);
           sqrtc(zir[0], zii[0], sqrtzir, sqrtzii);
           sqrtzir[0] *= 2.0;
           sqrtzii[0] *= 2.0;
           zmlt(facr[0], faci[0], sqrtzir[0], sqrtzii[0], facr, faci);
           zmlt(facdr[0], facdi[0], zir[0], zii[0], tr1, ti1);
           sumdr = sumdr + tr1[0];
           sumdi = sumdi + ti1[0];
           zmlt(facdr[0], facdi[0], zir[0], zii[0], facdr, facdi);
           zmlt(facdr[0], facdi[0], sqrtzir[0], sqrtzii[0], facdr, facdi);
           delxr = 1.0 - xtr[0];
           delxi = -xti[0];
           delyr = 1.0 - ytr[0];
           delyi = - yti[0];
           if ((zabs(delxr, delxi) < ERRTOL) && (zabs(delyr, delyi) < ERRTOL)) {
               break;    
           }
           sqrtc(xtr[0], xti[0], sqrtxr, sqrtxi);
           sqrtc(ytr[0], yti[0], sqrtyr, sqrtyi);
           nrep++;
       } // while (true)
       zdiv(3.0, 0.0, xtr[0] + ytr[0] + 1.0, xti[0] + yti[0], avir, avii);
       zmlt(xtr[0], xti[0], avir[0], avii[0], tr1, ti1);
       delxr = 1.0 - tr1[0];
       delxi = -ti1[0];
       zmlt(ytr[0], yti[0], avir[0], avii[0], tr1, ti1);
       delyr = 1.0 - tr1[0];
       delyi = -ti1[0];
       delzr = 1.0 - avir[0];
       delzi = -avii[0];
       zmlt(delxr, delxi, delyr, delyi, tr1, ti1);
       zmlt(delzr, delzi, delzr, delzi, tr2, ti2);
       e2r = tr1[0] - tr2[0];
       e2i = ti1[0] - ti2[0];
       zmlt(delxr, delxi, delyr, delyi, e3r, e3i);
       zmlt(e3r[0], e3i[0], delzr, delzi, e3r, e3i);
       zmlt(C1*e2r - C2 - C3*e3r[0], C1*e2i - C3*e3i[0], e2r, e2i, tr1, ti1);
       tr1[0] = 1.0 + tr1[0] + C4*e3r[0];
       ti1[0] = ti1[0] + C4*e3i[0];
       sqrtc(avir[0], avii[0], tr2, ti2);
       zmlt(facr[0], faci[0], tr1[0], ti1[0], rfr, rfi);
       zmlt(rfr[0], rfi[0], tr2[0], ti2[0], rfr, rfi);
       zdiv(5.0, 0.0, xtr[0] + ytr[0] + 3.0, xti[0] + yti[0], avidr, avidi);
       zmlt(xtr[0], xti[0], avidr[0], avidi[0], tr1, ti1);
       delxdr = 1.0 - tr1[0];
       delxdi = -ti1[0];
       zmlt(ytr[0], yti[0], avidr[0], avidi[0], tr1, ti1);
       delydr = 1.0 - tr1[0];
       delydi = -ti1[0];
       delzdr = 1.0 - avidr[0];
       delzdi = -avidi[0];
       zmlt(delxdr, delxdi, delydr, delydi, ear, eai);
       zmlt(delzdr, delzdi, delzdr, delzdi, ebr, ebi);
       ecr = ear[0] - ebr[0];
       eci = eai[0] - ebi[0];
       edr = ear[0] - 6.0*ebr[0];
       edi = eai[0] - 6.0*ebi[0];
       eer = edr + 2.0*ecr;
       eei = edi + 2.0*eci;
       zmlt(delzdr, delzdi, C4d * ear[0], C4d * eai[0], tr1, ti1);
       tr1[0] = -C3d*ecr + tr1[0];
       ti1[0] = -C3d*eci + ti1[0];
       zmlt(delzdr, delzdi, tr1[0], ti1[0], tr1, ti1);
       tr1[0] = C2d*eer + tr1[0];
       ti1[0] = C2d*eei + ti1[0];
       zmlt(delzdr, delzdi, tr1[0], ti1[0], tr1, ti1);
       zmlt(C6d*delzr, C6d*delzi, eer, eei, tr2, ti2);
       tr2[0] = -C1d + C5d*edr - tr2[0];
       ti2[0] = C5d*edi - ti2[0];
       zmlt(edr, edi, tr2[0], ti2[0], tr2, ti2);
       tr1[0] = 1.0 + tr2[0] + tr1[0];
       ti1[0] = ti2[0] + ti1[0];
       zmlt(facdr[0], facdi[0], tr1[0], ti1[0], tr1, ti1);
       zmlt(tr1[0], ti1[0], avidr[0], avidi[0], tr1, ti1);
       sqrtc(avidr[0], avidi[0], tr2, ti2);
       zmlt(tr1[0], ti1[0], tr2[0], ti2[0], tr1, ti1);
       rdr[0] = 3.0*sumdr + tr1[0];
       rdi[0] = 3.0*sumdi + ti1[0];
       return;
    } // rfrd
    
    private void rfrdb(double xrtr, double xrti, double yrtr, double yrti) {
        
    } // rfrdb
    
    private void zsin(double inr, double ini, double outr[], double outi[]) {
        outr[0] = Math.sin(inr)*Math.cosh(ini);
        outi[0] = Math.cos(inr)*Math.sinh(ini);
        return;
    } // private void zsin
    
    private void zcos(double inr, double ini, double outr[], double outi[]) {
        outr[0] = Math.cos(inr)*Math.cosh(ini);
        outi[0] = -Math.sin(inr)*Math.sinh(ini);
        return;
    } // private void zcos
    
    /**
     * complex multiply c = a * b.
     *
     * @param  ar  double
     * @param  ai  double
     * @param  br  double
     * @param  bi  double
     * @param  cr  double[]
     * @param  ci  double[]
     */
    private void zmlt(double ar, double ai, double br, double bi, double[] cr, double[] ci) {
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
    
    private void sqrtc(double zinr, double zini, double zsqr[], double zsqi[]) {
       double a;
       double th;
       
       a = zabs(zinr, zini);
       a = Math.sqrt(a);
       th = Math.atan2(zini, zinr);
       th *= 0.5;
       zsqr[0] = a * Math.cos(th);
       zsqi[0] = a * Math.sin(th);
       return;
    } // private void sqrtc
    
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

}
