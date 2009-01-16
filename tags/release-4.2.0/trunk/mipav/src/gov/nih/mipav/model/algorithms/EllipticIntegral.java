package gov.nih.mipav.model.algorithms;


import gov.nih.mipav.view.*;


/**
 * DOCUMENT ME!
 *
 * @author  ilb The elliptic integral of the first kind is defined by F(k, phi) = Integral from 0 to phi of
 *          d(theta)/sqrt(1 - k*k*sin(theta)*sin(theta)) where k is the modulus Letting t = sin(theta), the elliptic
 *          integral of the first kind can be written as F(k, x) = Integral from 0 to x of dt/sqrt((1-t*t)(1-k*k*t*t))
 *          where x = sin(phi)
 *
 *          <p>The elliptic integral of the second kind is defined by E(k, phi) = Integral from 0 to phi of sqrt(1 -
 *          k*k*sin(theta)*sin(theta))d(theta) where k is the modulus Letting t = sin(theta), the elliptic integral of
 *          the second kind can be written as E(k, x) = Integral from 0 to x of (sqrt(1 - k*k*t*t)/sqrt(1 - t*t))dt
 *          where x = sin(phi)</p>
 *
 *          <p>The elliptic integral of the third kind is defined by Pi(k, c, phi) = Integral from 0 to phi of
 *          d(theta)/((1 - c*sin(theta)*sin(theta))*sqrt(1 - k*k*sin(theta)*sin(theta))) Letting t = sin(theta), the
 *          elliptic integral of the third kind can be written as Pi(k, c, x) = Integral from 0 to x of dt/((1 -
 *          c*t*t)*sqrt((1 - t*t)*(1 - k*k*t*t))) where x = sin(phi)</p>
 *
 *          <p>When phi = PI/2 (x = 1), the elliptic integrals defined above are called complete elliptic integrals When
 *          phi is not equal to PI/2, the elliptic integrals are often referred to as incomplete elliptic integrals The
 *          complete elliptic integral of the first kind is: K(k) = Integral from 0 to PI/2 of d(theta)/sqrt(1 -
 *          k*k*sin(theta)*sin(theta)) where k is the modulus = Integral from 0 to 1 of dt/sqrt((1-t*t)(1-k*k*t*t)) The
 *          complete elliptic integral of the second kind is: E(k, phi) = Integral from 0 to PI/2 of sqrt(1 -
 *          k*k*sin(theta)*sin(theta))d(theta) where k is the modulus = Integral from 0 to 1 of (sqrt(1 -
 *          k*k*t*t)/sqrt(1 - t*t))dt The comlplete elliptic integral of the third kind is: Pi(k, c, phi) = Integral
 *          from 0 to PI/2 of d(theta)/((1 - c*sin(theta)*sin(theta))*sqrt(1 - k*k*sin(theta)*sin(theta))) = Integral
 *          from 0 to 1 of dt/((1 - c*t*t)*sqrt((1 - t*t)*(1 - k*k*t*t)))</p>
 *
 *          <p>The code for EllipticIntegralType = COMPLETE1, COMPLETE2, INCOMPLETE1, and INCOMPLETE2 is ported from a
 *          C++ program in Figures 1 and 2 of "Numerical Calculation of the Elliptic Integrals of the First and Second
 *          Kinds with Complex Modulus" by Tohru Morita, Interdisciplinary Information Sciences, Vol. 6, No. 1, 2000,
 *          pp. 67-74.</p>
 *
 *          <p>How to doublecheck? Use a combination of 2 sources: 1.) Port of FORTRAN code for complete and incomplete
 *          elliptic integrals of the first kind with real arguments from Computation of Special Functions by Shanjie
 *          Zhang and Jianming Jin, John Wiley & Sons, Inc., 1996, Chapter 18, Elliptic Integrals and Jacobian Elliptic
 *          Functions, pp. 654-679. 2.) PORT of FORTRAN code
 *          http://wwwasd.web.cern.ch/wwwasd/lhc++/requirements/special_functions/celint64.f revision 1.1.1.1 1996/04/01
 *          by mclareni. This code is a translation of the Algol procedure elco2(x,y,kc,a,b,u,v) in Ronald Bulirsch,
 *          Handbook Series Special Functions Numerical Calculation of Elliptic Integrals and Elliptic Functions,
 *          Numerische Mathematik, Volume 7, 1965, pp. 78 - 90. elco2 calculates a general elliptic integral of the
 *          second kind. It calculates w = u + i*v = Integral from 0 to z = x + i*y of (a + b*e*e)/((1 + e*e)*sqrt((1 +
 *          e*e)*(1 + kc*kc*e*e)))de kc is the complementary modulus = sqrt(1 - k*k), where k is the modulus The
 *          original algol elco2 function fails if kc = 0 or x < 0. However, the FORTRAN code appears to work for kc =
 *          0. The elco2 may have reduced accuracy at the branchpoints x = 0, y = +-i, +-i/kc.</p>
 *
 *          <p>F(k, arctan(w)) = elco2(w, kc, a = 1, b = 1) Let arctan(w) = z. Then w = tan(z) and for the incomplete
 *          elliptic integral of the first kind: F(k, z) = elco2(tan(z), kc, a = 1, b = 1) So elco2 cannot be used to
 *          check complex k (modulus), but it can be used as a partial check on complex z. It can check on complex z as
 *          long as 2 conditons are met: 1.) Real part of tan(z) >= 0 and 2.) kc > 0</p>
 *
 *          <p>E(k, arctan(w)) = elco2(w, kc, a = 1, b = kc*kc) Let arctan(w) = z. Then w = tan(z) and for the
 *          incomplete elliptic integral of the second kind: E(k, z) = elco2(tan(z), kc, a = 1, b = kc*kc) and it can
 *          check the incomplete elliptic integral of the second kind for the same conditions that the incomplete
 *          elliptic integral of the first kind can be checked.</p>
 *
 *          <p>Test results: For complete elliptic integrals of the first and second kind with real arguments, the
 *          Morita arithmetic-geometric mean, Morita Carlson's, ZHANG comelp, and ZHANG elit all give the same answer.
 *          </p>
 *
 *          <p>For incomplete elliptic integrals of the first and second kind with real arguemnts, both Morita
 *          Carlson's, ZHANG elit and CERN elco2 give the same answer. The problem with the CERN code is that is fails
 *          if the real part of z = x + iy is less than zero. However, from the Wolfram EllipticF documentation, the
 *          F(z|m) is an odd function with respect to z. F(-z|m) = -F(z|m), so there is no need to use elco2 for values
 *          of x < 0. Note that the Wolfram m = k*k, where k is the modulus used in Computation of Special Functions.
 *          </p>
 *
 *          <p>For incomplete elliptic integrals of the first and second kind with complex phi and real modulus, the
 *          Morita standard Carlson's and the CERN elco2 give the same answer.</p>
 */

public class EllipticIntegral {

    //~ Static fields/initializers -------------------------------------------------------------------------------------

    /**
     * F(k, phi) INCOMPLETE1 E(k, phi) INCOMPLETE2 K(k) COMPLETE1 E(k) COMPLETE2 Complex arguments for first and second
     * complete and incomplete elliptic integrals.
     */
    private static final int MORITA_SOURCE = 1;

    /** Real arguments for first and second complete and incomplete elliptic integrals. */
    private static final int ZHANG_SOURCE = 2;

    /** Complex argument for phi only in first and second complete and incomplete elliptic integrals. */
    private static final int CERN_SOURCE = 3;

    /** Compare outputs of different modules for self-testing. */
    private static final int TEST_SOURCE = 4;
    // ~ Instance fields ------------------------------------------------------------------------------------------------

    //~ Instance fields ------------------------------------------------------------------------------------------------

    /** DOCUMENT ME! */
    private double a;

    /** DOCUMENT ME! */
    private double[] aki = new double[1];

    /** DOCUMENT ME! */
    private double[] akr = new double[1];

    /** If false, function used If true, analytic continuation of function used. */
    private boolean analyticContinuationUsed;

    /** DOCUMENT ME! */
    private double b;

    /** DOCUMENT ME! */
    private double BIGd = 4.5E121; // 4.5E21

    /** DOCUMENT ME! */
    private double c;

    /** private double BIG = 3.0E137; // 3.0E37. */
    private double C1 = 1.0 / 24.0;

    /** DOCUMENT ME! */
    private double C1d = 3.0 / 14.0;

    /** DOCUMENT ME! */
    private double C2 = 0.1;

    /** DOCUMENT ME! */
    private double C2d = 1.0 / 6.0;

    /** DOCUMENT ME! */
    private double C3 = 3.0 / 44.0;

    /** DOCUMENT ME! */
    private double C3d = 9.0 / 22.0;

    /** DOCUMENT ME! */
    private double C4 = 1.0 / 14.0;

    /** DOCUMENT ME! */
    private double C4d = 3.0 / 26.0;

    /** DOCUMENT ME! */
    private double C5d = 0.25 * C3d;

    /** DOCUMENT ME! */
    private double C6d = 1.5 * C4d;

    /** DOCUMENT ME! */
    private double[] cki = new double[1];

    /** DOCUMENT ME! */
    private double[] ckr = new double[1];

    /** k is the modulus k', the complementary modulus = sqrt(1 - k*k). */
    private boolean complementaryModulusUsed;

    /** DOCUMENT ME! */
    private boolean complete;

    /** DOCUMENT ME! */
    private int[] errorFlag;

    /** DOCUMENT ME! */
    private double ERRTOL = 8.0E-3; // 8.0E-2

    /** DOCUMENT ME! */
    private double ERRTOLb = 1.5E-6; // 1.5E-3

    /** DOCUMENT ME! */
    private double[] first;

    /** DOCUMENT ME! */
    private double[] firsti;

    /** DOCUMENT ME! */
    private double[] firstr;

    /** DOCUMENT ME! */
    private double kc;

    /** modulus, 0 <= mod <= 1. */
    private double mod;

    /** Imaginary part of modulus or complementary modulus. */
    private double modi;

    /** Real part of modulus or complementary modulus. */
    private double modr;

    /** Number of repetitions = N - 1. */
    private int N = 12; // 6

    /** DOCUMENT ME! */
    private int nrep;

    /** DOCUMENT ME! */
    private double phi; // argument in radians

    /** DOCUMENT ME! */
    private double phii;

    /** For complete integrals phir = PI/2, phii = 0. */
    private double phir;

    /** DOCUMENT ME! */
    private double pihf = 0.5 * Math.PI;

    /** DOCUMENT ME! */
    private double[] rdi = new double[1];

    /** DOCUMENT ME! */
    private double[] rdr = new double[1];

    /** DOCUMENT ME! */
    private double RERR = 1.0E-12; // 1.0E-6

    /** DOCUMENT ME! */
    private double[] rfi = new double[1];

    /** DOCUMENT ME! */
    private double[] rfr = new double[1];

    /** DOCUMENT ME! */
    private boolean runcel12;

    /** DOCUMENT ME! */
    private boolean runcomelp = false;

    /** DOCUMENT ME! */
    private boolean runelit = false;

    /** DOCUMENT ME! */
    private double[] second;

    /** DOCUMENT ME! */
    private double[] secondi;

    /** DOCUMENT ME! */
    private double[] secondr;

    /** DOCUMENT ME! */
    private int sgnck;

    /** DOCUMENT ME! */
    private int source;

    /** DOCUMENT ME! */
    private double THIRD = 1.0 / 3.0;

    /** DOCUMENT ME! */
    private double[] third;

    /** DOCUMENT ME! */
    private double TINY = 3.0E-138; // 3.0E-38

    /** DOCUMENT ME! */
    private double TINYd = 1.0E-125; // 1.0E-25

    /** DOCUMENT ME! */
    private double[] u;

    /** DOCUMENT ME! */
    private ViewUserInterface UI = ViewUserInterface.getReference();

    /** DOCUMENT ME! */
    private boolean useStandardMethod;

    /** DOCUMENT ME! */
    private double[] v;

    /** DOCUMENT ME! */
    private double x;

    /** DOCUMENT ME! */
    private double y;

    //~ Constructors ---------------------------------------------------------------------------------------------------

    /**
     * ~ Constructors
     * ---------------------------------------------------------------------------------------------------
     */
    public EllipticIntegral() {
        source = TEST_SOURCE;
    }

    /**
     * Used for complete elliptic integrals of first and second kind with real modulus 0 <= mod <= 1, mod is the
     * modulus.
     *
     * @param  mod     DOCUMENT ME!
     * @param  first   DOCUMENT ME!
     * @param  second  DOCUMENT ME!
     */
    public EllipticIntegral(double mod, double[] first, double[] second) {
        source = ZHANG_SOURCE;
        runcomelp = true;
        this.mod = mod;
        this.first = first;
        this.second = second;
    }

    /**
     * Used for complete and incomplete elliptic integrals of first and second kind with real modulus and argument 0 <=
     * mod <= 1, mod is the modulus phi, argument in radians.
     *
     * @param  mod     DOCUMENT ME!
     * @param  phi     DOCUMENT ME!
     * @param  first   DOCUMENT ME!
     * @param  second  DOCUMENT ME!
     */
    public EllipticIntegral(double mod, double phi, double[] first, double[] second) {
        source = ZHANG_SOURCE;
        runelit = true;
        this.mod = mod;
        this.phi = phi;
        this.first = first;
        this.second = second;
    }

    /**
     * Used for complete and incomplete elliptic integrals of the third kind with real modulus, real argument phi, and
     * real parameter c 0 <= mod <= 1 phi, argument in radians c parameter, 0 <= c <=1.
     *
     * @param  mod    DOCUMENT ME!
     * @param  phi    DOCUMENT ME!
     * @param  c      DOCUMENT ME!
     * @param  third  DOCUMENT ME!
     */
    public EllipticIntegral(double mod, double phi, double c, double[] third) {
        source = ZHANG_SOURCE;
        this.mod = mod;
        this.phi = phi;
        this.c = c;
        this.third = third;
    }

    /**
     * Creates a new EllipticIntegral object.
     *
     * @param  x   DOCUMENT ME!
     * @param  y   DOCUMENT ME!
     * @param  kc  DOCUMENT ME!
     * @param  a   DOCUMENT ME!
     * @param  b   DOCUMENT ME!
     * @param  u   DOCUMENT ME!
     * @param  v   DOCUMENT ME!
     */
    public EllipticIntegral(double x, double y, double kc, double a, double b, double[] u, double[] v) {
        source = CERN_SOURCE;
        this.x = x;
        this.y = y;
        this.kc = kc;
        this.a = a;
        this.b = b;
        this.u = u;
        this.v = v;
    }

    /**
     * Constructor only for complete integrals with phi = PI/2. The method called is based on the arithmetic-geometric
     * mean procedure If only the complete integrals are calculated, the arithmetic-geometric mean procdure is superior
     * to the 2 methods based on Carlson's algorithm
     *
     * @param  modr                      DOCUMENT ME!
     * @param  modi                      DOCUMENT ME!
     * @param  complementaryModulusUsed  DOCUMENT ME!
     * @param  analyticContinuationUsed  DOCUMENT ME!
     * @param  firstr                    DOCUMENT ME!
     * @param  firsti                    DOCUMENT ME!
     * @param  secondr                   DOCUMENT ME!
     * @param  secondi                   DOCUMENT ME!
     * @param  errorFlag                 DOCUMENT ME!
     */
    public EllipticIntegral(double modr, double modi, boolean complementaryModulusUsed,
                            boolean analyticContinuationUsed, double[] firstr, double[] firsti, double[] secondr,
                            double[] secondi, int[] errorFlag) {
        source = MORITA_SOURCE;
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

    /**
     * When calculating the complete integrals K(k) and E(K), we can make complete true or false, except when abs(k') <=
     * 1.0E-10, in which case complete must be true and the complementary modulus must be used The only merit of the
     * alternative method over the standard method is that the code is simpler Both methods are based on modifications
     * of Carlson's algortihm.
     *
     * @param  complete                  DOCUMENT ME!
     * @param  modr                      DOCUMENT ME!
     * @param  modi                      DOCUMENT ME!
     * @param  complementaryModulusUsed  DOCUMENT ME!
     * @param  analyticContinuationUsed  DOCUMENT ME!
     * @param  phir                      DOCUMENT ME!
     * @param  phii                      DOCUMENT ME!
     * @param  firstr                    DOCUMENT ME!
     * @param  firsti                    DOCUMENT ME!
     * @param  secondr                   DOCUMENT ME!
     * @param  secondi                   DOCUMENT ME!
     * @param  useStandardMethod         DOCUMENT ME!
     * @param  errorFlag                 DOCUMENT ME!
     */
    public EllipticIntegral(boolean complete, double modr, double modi, boolean complementaryModulusUsed,
                            boolean analyticContinuationUsed, double phir, double phii, double[] firstr,
                            double[] firsti, double[] secondr, double[] secondi, boolean useStandardMethod,
                            int[] errorFlag) {
        source = MORITA_SOURCE;
        this.complete = complete;
        runcel12 = false;
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

    //~ Methods --------------------------------------------------------------------------------------------------------

    /**
     * DOCUMENT ME!
     */
    public void run() {
        double ckabs;

        switch (source) {

            case TEST_SOURCE:
                runTest();
                break;

            case MORITA_SOURCE:
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
                    ckabs = zabs(ckr[0], cki[0]);

                    if ((ckabs < 1.0E-10) && (Math.abs(phir - pihf) < TINY) && (Math.abs(phii) < TINY)) {
                        complete = true;
                        phir = pihf;
                        phii = 0.0;
                    }

                    sqrtc(1.0 - (ckr[0] * ckr[0]) + (cki[0] * cki[0]), -2.0 * ckr[0] * cki[0], akr, aki);

                    if (modr < 0.0) {
                        sgnck = -1;
                    } else {
                        sgnck = 1;
                    }
                } // if (complementaryModulusUsed)
                else { // modulus used
                    akr[0] = modr;
                    aki[0] = modi;
                    sqrtc(1.0 - (akr[0] * akr[0]) + (aki[0] * aki[0]), -2.0 * akr[0] * aki[0], ckr, cki);
                    ckabs = zabs(ckr[0], cki[0]);

                    if ((ckabs < 1.0E-10) && (Math.abs(phir - pihf) < TINY) && (Math.abs(phii) < TINY)) {
                        complete = true;
                        complementaryModulusUsed = true;
                        phir = pihf;
                        phii = 0.0;

                        if (ckr[0] < 0.0) {
                            sgnck = -1;
                        } else {
                            sgnck = 1;
                        }
                    } else if (analyticContinuationUsed) {
                        sgnck = -1;
                        ckr[0] = -ckr[0];
                        cki[0] = -cki[0];
                    } else {
                        sgnck = 1;
                    }
                } // modulus used

                if (runcel12) {
                    cel12(ckr[0], cki[0]);

                    return;
                } else {

                    if (complete) {
                        sgnck = 0;
                    }

                    ell12(phir, phii, akr[0], aki[0], ckr[0], cki[0], sgnck);
                }

                break;

            case ZHANG_SOURCE:
                if (runcomelp) {
                    comelp(mod);
                } else if (runelit) {
                    elit(mod, phi);
                } else {
                    elit3(mod, phi, c);
                }

                break;

            case CERN_SOURCE:
                elco2(x, y, kc, a, b);
                break;
        } // switch (source)

    } // public void run()

    /**
     * Routine cel12 only calculates complete integrals of the first and second kind with complex arguments Based on the
     * arithmetic-geometric mean procedure Ported from Tohru Morita article.
     *
     * @param  ckr  DOCUMENT ME!
     * @param  cki  DOCUMENT ME!
     */
    private void cel12(double ckr, double cki) {
        double[] kcr = new double[1];
        double[] kci = new double[1];
        double[] tar = new double[1];
        double[] tai = new double[1];
        double[] air = new double[1];
        double[] aii = new double[1];
        double sbr;
        double sbi;
        double[] aar = new double[1];
        double[] aai = new double[1];
        double[] sar = new double[1];
        double[] sai = new double[1];
        double[] quotr = new double[1];
        double[] quoti = new double[1];
        double[] tr = new double[1];
        double[] ti = new double[1];

        if (zabs(ckr, cki) < TINY) {
            firstr[0] = Double.POSITIVE_INFINITY;
            firsti[0] = 0.0;
            secondr[0] = 1.0;
            secondi[0] = 0.0;

            return;
        }

        kcr[0] = ckr;
        kci[0] = cki;
        zmlt(ckr, cki, ckr, cki, tar, tai);
        air[0] = 1.0;
        aii[0] = 0.0;
        sbr = 1.0;
        sbi = 0.0;
        nrep = 0;

        while (true) {
            zdiv(2.0, 0.0, 1.0 + kcr[0], kci[0], aar, aai);
            zmlt(air[0], aii[0], aar[0], aai[0], air, aii);
            zmlt((sbr + tar[0]), (sbi + tai[0]), aar[0] / 2.0, aai[0] / 2.0, sar, sai);
            zdiv(sar[0], sai[0], sbr, sbi, quotr, quoti);

            if ((zabs(1.0 - aar[0], -aai[0]) < RERR) && (zabs(1.0 - quotr[0], -quoti[0]) < RERR)) {
                break;
            }

            zmlt(kcr[0], kci[0], sbr, sbi, tr, ti);
            tar[0] = tar[0] + tr[0];
            tai[0] = tai[0] + ti[0];
            zmlt(tar[0], tai[0], aar[0], aai[0], tar, tai);
            zmlt(tar[0], tai[0], aar[0] / 2.0, aai[0] / 2.0, tar, tai);
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

    /**
     * Ported from Computation of Special Functions by Shanjie Zhang and Jianming Jin Computes complete first and second
     * elliptic integrals with a real modulus 0 <= mod <= 1, mod is the modulus.
     *
     * @param  mod  DOCUMENT ME!
     */
    private void comelp(double mod) {
        double pk;
        double ak;
        double bk;
        double ae;
        double be;

        pk = 1.0 - (mod * mod);

        if (Math.abs(mod - 1.0) < TINY) {
            first[0] = Double.POSITIVE_INFINITY;
            second[0] = 1.0;
        } else {
            ak = (((((((.01451196212 * pk) + .03742563713) * pk) + .03590092383) * pk) + .09666344259) * pk) +
                 1.38629436112;
            bk = (((((((.00441787012 * pk) + .03328355346) * pk) + .06880248576) * pk) + .12498593597) * pk) + 0.5;
            first[0] = ak - (bk * Math.log(pk));
            ae = (((((((.01736506451 * pk) + .04757383546) * pk) + .0626060122) * pk) + .44325141463) * pk) + 1.0;
            be = ((((((.00526449639 * pk) + .04069697526) * pk) + .09200180037) * pk) + .2499836831) * pk;
            second[0] = ae - (be * Math.log(pk));
        }

        return;
    }

    /**
     * DOCUMENT ME!
     *
     * @param   x  DOCUMENT ME!
     *
     * @return  DOCUMENT ME!
     */
    private double cosh(double x) {
        double var;
        var = (Math.exp(x) + Math.exp(-x)) / 2.0;

        return var;
    }

    /**
     * PORT of FORTRAN code http://wwwasd.web.cern.ch/wwwasd/lhc++/requirements/special_functions/celint64.f revision
     * 1.1.1.1 1996/04/01 by mclareni. Output is u[0] + i*v[0]
     *
     * @param  x   DOCUMENT ME!
     * @param  y   DOCUMENT ME!
     * @param  kc  DOCUMENT ME!
     * @param  a   DOCUMENT ME!
     * @param  b   DOCUMENT ME!
     */
    private void elco2(double x, double y, double kc, double a, double b) {
        int[] ierr;
        double[] tr;
        double[] ti;
        double aa;
        double bb;
        double sy;
        double c;
        double e2;
        double d;
        double xk;
        double e1;
        double f2;
        double f1;
        double dn1;
        double dn2;
        double[] d1 = new double[13];
        double[] d2 = new double[13];
        double h;
        int n;
        double xm;
        double f;
        double ykc;
        double e;
        int L;
        double xm1;
        double xm2;
        int j;
        boolean loop;
        double cc;

        cc = Math.pow(10.0, -16);

        if (((x == 0.0) && (y == 0.0)) || (kc == 0.0)) {
            zmlt(x, y, x, y, u, v);
            u[0] = u[0] + 1.0;
            sqrtc(u[0], v[0], u, v);
            ierr = new int[1];
            tr = new double[1];
            ti = new double[1];
            zlog(x + u[0], y + v[0], tr, ti, ierr);
            zdiv((a - b) * x, (a - b) * y, u[0], v[0], u, v);
            u[0] = (b * tr[0]) + u[0];
            v[0] = (b * ti[0]) + v[0];
        } else if (x < 0.0) {

            // Cannot calculate for (x < 0.0)
            u[0] = Double.NaN;
            v[0] = Double.NaN;
            Preferences.debug("Cannot calculate for x < 0.0");
        } else {
            aa = a;
            bb = b;

            if (y >= 0.0) {
                sy = 1.0;
            } else {
                sy = -1.0;
            }

            y = Math.abs(y);
            c = (x * x) - (y * y);
            e2 = 2.0 * x * y;
            d = kc * kc;
            xk = 1.0 - d;
            e1 = 1.0 + c;
            f2 = 1.0 / ((e1 * e1) + (e2 * e2));
            f1 = (((1.0 + (d * c)) * e1) + (d * e2 * e2)) * f2;
            f2 = -2.0 * xk * x * y * f2;
            dn1 = Math.sqrt(0.5 * (Math.sqrt((f1 * f1) + (f2 * f2)) + Math.abs(f1)));
            dn2 = 0.5 * f2 / dn1;

            if (f1 < 0.0) {
                f1 = dn1;
                dn1 = -dn2;
                dn2 = -f1;
            }

            if (xk < 0.0) {
                dn1 = Math.abs(dn1);
                dn2 = Math.abs(dn2);
            }

            c = 1.0 + dn1;
            f1 = (e1 * c) - (e2 * dn2);
            f2 = (e1 * dn2) + (e2 * c);
            d2[0] = 1.0 / ((f1 * f1) + (f2 * f2));
            d1[0] = ((x * f1) + (y * f2)) * d2[0];
            d2[0] = ((y * f1) - (x * f2)) * d2[0];
            h = aa - bb;
            n = 1;
            xm = 1.0;
            f = 1.0;
            d = 1.0;
            ykc = Math.abs(kc);
            e = aa;
            aa = bb + aa;
            L = 4;

            do {
                loop = false;
                xm1 = 0.5 * (ykc + xm);
                xm2 = xm1 * xm1;
                xk = f * xk / (4.0 * xm2);
                bb = (e * ykc) + bb;
                e = aa;
                f2 = 1.0 / ((c * c) + (dn2 * dn2));
                f1 = (((ykc + (xm * dn1)) * c) + (xm * dn2 * dn2)) * f2;
                e1 = f1 / xm1;
                e2 = xk * dn2 * f2;
                dn1 = Math.sqrt(0.5 * (Math.sqrt((e1 * e1) + (4.0 * e2 * e2)) + Math.abs(e1)));
                dn2 = e2 / dn1;
                f1 = (dn1 * x) - (dn2 * y);
                f2 = (dn1 * y) + (dn2 * x);
                x = Math.abs(f1);
                y = Math.abs(f2);
                aa = (bb / xm1) + aa;
                L = 2 * L;
                c = 1.0 + dn1;
                d = 0.5 * xk * d;
                e1 = 1.0 + (((x * x) - (y * y)) * xm2);
                e2 = 2.0 * x * y * xm2;
                f1 = (c * e1) - (dn2 * e2);
                f2 = (c * e2) + (dn2 * e1);
                e1 = d / ((f1 * f1) + (f2 * f2));
                d1[n] = ((x * f1) + (y * f2)) * e1;
                d2[n] = ((y * f1) - (x * f2)) * e1;
                xk = xk * xk;

                if (xk > cc) {
                    ykc = Math.sqrt(xm * ykc);
                    f = xm2;
                    xm = xm1;
                    n = n + 1;
                    loop = true;
                }
            } while (loop);

            f2 = 0.0;
            f1 = 0.0;

            for (j = n; j >= 0; j--) {
                f1 = d1[j] + f1;
                f2 = d2[j] + f2;
            }

            x = xm1 * x;
            y = xm1 * y;
            c = (x * x) + (y * y);
            e2 = 1.0 / (1.0 + (2.0 * y) + c);
            e1 = (1.0 - c) * e2;
            e2 = 2.0 * x * e2;
            d = aa / (xm1 * L);
            u[0] = (d * Math.atan2(e2, e1)) + (h * f1);
            v[0] = sy * ((h * f2) - (Math.log((e1 * e1) + (e2 * e2)) * 0.5 * d));
        }

        return;
    } // elco2

    /**
     * Ported from Computation of Special Functions by Shanjie Zhang and Jianming Jin Computes complete and incomplete
     * first and second elliptic integrals with a real modulus and real argument 0 <= mod <= 1, mod is the modulus arg
     * is the argument in radians.
     *
     * @param  mod  DOCUMENT ME!
     * @param  phi  DOCUMENT ME!
     */
    private void elit(double mod, double phi) {
        double g;
        double a0;
        double b0;
        double d0;
        double r;
        double fac;
        int n;
        double a = 0.0;
        double b;
        double c;
        double d = 0.0;
        double ck;
        double ce;

        g = 0.0;
        a0 = 1.0;
        b0 = Math.sqrt(1.0 - (mod * mod));
        d0 = phi;
        r = mod * mod;

        if ((Math.abs(mod - 1.0) < TINY) && (Math.abs(phi - pihf) < TINY)) {
            first[0] = Double.POSITIVE_INFINITY;
            second[0] = 1.0;
        } else if (Math.abs(mod - 1.0) < TINY) {
            first[0] = Math.log((1.0 + Math.sin(d0)) / Math.cos(d0));
            second[0] = Math.sin(d0);
        } else {
            fac = 1.0;

            for (n = 1; n <= 40; n++) {
                a = (a0 + b0) / 2.0;
                b = Math.sqrt(a0 * b0);
                c = (a0 - b0) / 2.0;
                fac = 2.0 * fac;
                r = r + (fac * c * c);

                if (phi != pihf) {
                    d = d0 + Math.atan((b0 / a0) * Math.tan(d0));
                    g = g + (c * Math.sin(d));
                    d0 = d + (Math.PI * (int) ((d / Math.PI) + 0.5));
                }

                a0 = a;
                b0 = b;

                if (c < 1.0E-7) {
                    break;
                }
            } // for (n = 1; n <= 40; n++)

            ck = Math.PI / (2.0 * a);
            ce = Math.PI * (2.0 - r) / (4.0 * a);

            if (phi == pihf) {
                first[0] = ck;
                second[0] = ce;
            } else {
                first[0] = d / (fac * a);
                second[0] = (first[0] * ce / ck) + g;
            }
        }

        return;
    }

    /**
     * Ported from Computation of Special Functions by Shanjie Zhang and Jianming Jin Computes complete and incomplete
     * elliptic integrals of the third kind with a real modulus real argument, and real parameter 0 <= mod <= 1, mod is
     * the modulus arg is the argument in radians c is the parameter, 0 <= c <= 1.
     *
     * @param  mod  DOCUMENT ME!
     * @param  phi  DOCUMENT ME!
     * @param  c    DOCUMENT ME!
     */
    private void elit3(double mod, double phi, double c) {
        double[] t = new double[] {
                         .9931285991850949, .9639719272779138, .9122344282513259, .8391169718222188, .7463319064601508,
                         .6360536807265150, .5108670019508271, .3737060887154195, .2277858511416451, .07652652113349734
                     };
        double[] w = new double[] {
                         .01761400713915212, .04060142980038694, .06267204833410907, .08327674157670475,
                         .1019301198172404, .1181945319615184, .1316886384491766, .1420961093183820, .1491729864726037,
                         .1527533871307258
                     };
        boolean lb1;
        boolean lb2;
        double c1;
        double c2;
        int i;
        double c0;
        double t1;
        double t2;
        double f1;
        double f2;
        double st;

        // Convert radians to degrees
        phi = phi * (180.0 / Math.PI);
        lb1 = ((mod == 1.0) && (Math.abs(phi - 90.0) <= 1.0E-8));
        lb2 = ((c == 1.0) && (Math.abs(phi - 90.0) <= 1.0E-8));

        if (lb1 || lb2) {
            third[0] = Double.MAX_VALUE;

            return;
        }

        // Evaluate elliptic integral using Gauss-Legendre quadrature
        c1 = 0.87266462599716E-12 * phi;
        c2 = c1;
        third[0] = 0.0;

        for (i = 1; i <= 10; i++) {
            c0 = c2 * t[i - 1];
            t1 = c1 + c0;
            t2 = c1 - c0;
            st = Math.sin(t1);
            f1 = 1.0 / ((1.0 - (c * st * st)) * Math.sqrt(1.0 - (mod * mod * st * st)));
            st = Math.sin(t2);
            f2 = 1.0 / ((1.0 - (c * st * st)) * Math.sqrt(1.0 - (mod * mod * st * st)));
            third[0] = third[0] + (w[i - 1] * (f1 + f2));
        } // for (i = 1; i <= 10; i++)

        third[0] = c1 * third[0];

        return;
    } // elit3

    /**
     * Calculates complete and incomplete elliptic integrals of the first and second kind with complex arguments Ported
     * from the Tohru Morita article.
     *
     * @param  phir   DOCUMENT ME!
     * @param  phii   DOCUMENT ME!
     * @param  akr    DOCUMENT ME!
     * @param  aki    DOCUMENT ME!
     * @param  ckr    DOCUMENT ME!
     * @param  cki    DOCUMENT ME!
     * @param  sgnck  DOCUMENT ME!
     */
    private void ell12(double phir, double phii, double akr, double aki, double ckr, double cki, int sgnck) {
        double[] cr = new double[1];
        double[] ci = new double[1];
        double[] qrtr = new double[1];
        double[] qrti = new double[1];
        double[] sr = new double[1];
        double[] si = new double[1];
        double[] sk2r = new double[1];
        double[] sk2i = new double[1];
        double qr;
        double qi;


        if ((Math.abs(phir - pihf) < TINY) && (Math.abs(phii) < TINY) && (Math.abs(akr - 1.0) < TINY) &&
                (Math.abs(aki) < TINY)) {
            firstr[0] = Double.POSITIVE_INFINITY;
            firsti[0] = 0.0;
            secondr[0] = 1.0;
            secondi[0] = 0.0;

            return;
        }

        zsin(phir, phii, sr, si);
        zmlt(sr[0], si[0], akr, aki, sk2r, sk2i);
        zmlt(sk2r[0], sk2i[0], sk2r[0], sk2i[0], sk2r, sk2i);

        if (sgnck == 0) {
            cr[0] = 0.0;
            ci[0] = 0.0;
            qrtr[0] = ckr;
            qrti[0] = cki;
        } // if (sgnck == 0)
        else {
            zcos(phir, phii, cr, ci);
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
            // rfcomplex(cr[0], ci[0], qrtr[0], qrti[0]);
        } else {
            rfrdb(cr[0], ci[0], qrtr[0], qrti[0]);
        }

        if (Double.isNaN(rdr[0])) {
            firstr[0] = Double.NaN;
            firsti[0] = 0.0;
            secondr[0] = Double.NaN;
            secondi[0] = 0.0;

            return;
        }

        zmlt(sr[0], si[0], rfr[0], rfi[0], firstr, firsti);
        zmlt(sr[0], si[0], sk2r[0], sk2i[0], secondr, secondi);
        zmlt(secondr[0], secondi[0], rdr[0] / 3.0, rdi[0] / 3.0, secondr, secondi);
        secondr[0] = firstr[0] - secondr[0];
        secondi[0] = firsti[0] - secondi[0];

        return;
    } // ell12

    /**
     * DOCUMENT ME!
     *
     * @param  xrtr  DOCUMENT ME!
     * @param  xrti  DOCUMENT ME!
     * @param  yrtr  DOCUMENT ME!
     * @param  yrti  DOCUMENT ME!
     */
    private void rfcomplex(double xrtr, double xrti, double yrtr, double yrti) {
        double[] sqrtxr = new double[1];
        double[] sqrtxi = new double[1];
        double[] sqrtyr = new double[1];
        double[] sqrtyi = new double[1];
        double[] sqrtzr = new double[1];
        double[] sqrtzi = new double[1];
        double[] xtr = new double[1];
        double[] xti = new double[1];
        double[] ytr = new double[1];
        double[] yti = new double[1];
        double[] ztr = new double[1];
        double[] zti = new double[1];
        double alambr;
        double alambi;
        double aver;
        double avei;
        double[] tr1 = new double[1];
        double[] ti1 = new double[1];
        double[] tr2 = new double[1];
        double[] ti2 = new double[1];
        double[] delxr = new double[1];
        double[] delxi = new double[1];
        double[] delyr = new double[1];
        double[] delyi = new double[1];
        double[] delzr = new double[1];
        double[] delzi = new double[1];
        double e2r;
        double e2i;
        double[] e3r = new double[1];
        double[] e3i = new double[1];

        sqrtxr[0] = xrtr;
        sqrtxi[0] = xrti;
        sqrtyr[0] = yrtr;
        sqrtyi[0] = yrti;
        ztr[0] = 1.0;
        zti[0] = 0.0;
        sqrtzr[0] = 1.0;
        sqrtzi[0] = 0.0;
        zmlt(xrtr, xrti, xrtr, xrti, xtr, xti);
        zmlt(yrtr, yrti, yrtr, yrti, ytr, yti);

        if ((zabs(xtr[0] + ytr[0], xti[0] + yti[0]) < TINYd) || (zabs(xtr[0] + ztr[0], xti[0] + zti[0]) < TINYd) ||
                (zabs(ytr[0] + ztr[0], yti[0] + zti[0]) < TINYd) || (zabs(xtr[0], xti[0]) > BIGd) ||
                (zabs(ytr[0], yti[0]) > BIGd)) {
            rfr[0] = Double.NaN;

            return;
        }

        nrep = 0;

        while (true) {
            zmlt(sqrtxr[0], sqrtxi[0], sqrtyr[0], sqrtyi[0], tr1, ti1);
            alambr = tr1[0];
            alambi = ti1[0];
            zmlt(sqrtxr[0], sqrtxi[0], sqrtzr[0], sqrtzi[0], tr1, ti1);
            alambr = alambr + tr1[0];
            alambi = alambi + ti1[0];
            zmlt(sqrtyr[0], sqrtyi[0], sqrtzr[0], sqrtzi[0], tr1, ti1);
            alambr = alambr + tr1[0];
            alambi = alambi + ti1[0];
            xtr[0] = 0.25 * (xtr[0] + alambr);
            xti[0] = 0.25 * (xti[0] + alambi);
            ytr[0] = 0.25 * (ytr[0] + alambr);
            yti[0] = 0.25 * (yti[0] + alambi);
            ztr[0] = 0.25 * (ztr[0] + alambr);
            zti[0] = 0.25 * (zti[0] + alambi);
            aver = THIRD * (xtr[0] + ytr[0] + ztr[0]);
            avei = THIRD * (xti[0] + yti[0] + zti[0]);
            zdiv(aver - xtr[0], avei - xti[0], aver, avei, delxr, delxi);
            zdiv(aver - ytr[0], avei - yti[0], aver, avei, delyr, delyi);
            zdiv(aver - ztr[0], avei - zti[0], aver, avei, delzr, delzi);

            if ((zabs(delxr[0], delxi[0]) < ERRTOL) && (zabs(delyr[0], delyi[0]) < ERRTOL) &&
                    (zabs(delzr[0], delzi[0]) < ERRTOL)) {
                break;
            }

            sqrtc(xtr[0], xti[0], sqrtxr, sqrtxi);
            sqrtc(ytr[0], yti[0], sqrtyr, sqrtyi);
            sqrtc(ztr[0], zti[0], sqrtzr, sqrtzi);
            nrep++;
        } // while (true)

        zmlt(delxr[0], delxi[0], delyr[0], delyi[0], tr1, ti1);
        zmlt(delzr[0], delzi[0], delzr[0], delzi[0], tr2, ti2);
        e2r = tr1[0] - tr2[0];
        e2i = ti1[0] - ti2[0];
        zmlt(delxr[0], delxi[0], delyr[0], delyi[0], e3r, e3i);
        zmlt(e3r[0], e3i[0], delzr[0], delzi[0], e3r, e3i);
        zmlt((C1 * e2r) - C2 - (C3 * e3r[0]), (C1 * e2i) - (C3 * e3i[0]), e2r, e2i, tr1, ti1);
        tr1[0] = 1.0 + tr1[0] + (C4 * e3r[0]);
        ti1[0] = ti1[0] + (C4 * e3i[0]);
        sqrtc(aver, avei, tr2, ti2);
        zdiv(tr1[0], ti1[0], tr2[0], ti2[0], rfr, rfi);

        return;
    } // rfcomplex

    /**
     * Ported from Tohru Morita article.
     *
     * @param  xrtr  DOCUMENT ME!
     * @param  xrti  DOCUMENT ME!
     * @param  yrtr  DOCUMENT ME!
     * @param  yrti  DOCUMENT ME!
     */
    private void rfrd(double xrtr, double xrti, double yrtr, double yrti) {
        double[] sqrtxr = new double[1];
        double[] sqrtxi = new double[1];
        double[] sqrtyr = new double[1];
        double[] sqrtyi = new double[1];
        double[] xtr = new double[1];
        double[] xti = new double[1];
        double[] ytr = new double[1];
        double[] yti = new double[1];
        double[] facr = new double[1];
        double[] faci = new double[1];
        double sumdr;
        double sumdi;
        double[] facdr = new double[1];
        double[] facdi = new double[1];
        double alambr;
        double alambi;
        double[] tr1 = new double[1];
        double[] ti1 = new double[1];
        double[] tr2 = new double[1];
        double[] ti2 = new double[1];
        double[] zir = new double[1];
        double[] zii = new double[1];
        double[] sqrtzir = new double[1];
        double[] sqrtzii = new double[1];
        double delxr;
        double delxi;
        double delyr;
        double delyi;
        double delzr;
        double delzi;
        double[] avir = new double[1];
        double[] avii = new double[1];
        double delxdr;
        double delxdi;
        double delydr;
        double delydi;
        double delzdr;
        double delzdi;
        double e2r;
        double e2i;
        double[] e3r = new double[1];
        double[] e3i = new double[1];
        double[] avidr = new double[1];
        double[] avidi = new double[1];
        double[] ear = new double[1];
        double[] eai = new double[1];
        double[] ebr = new double[1];
        double[] ebi = new double[1];
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

        if ((zabs(xtr[0] + ytr[0], xti[0] + yti[0]) < TINYd) || (zabs(xtr[0], xti[0]) > BIGd) ||
                (zabs(ytr[0], yti[0]) > BIGd)) {
            rdr[0] = Double.NaN;

            return;
        }

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
            delyi = -yti[0];

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
        zmlt((C1 * e2r) - C2 - (C3 * e3r[0]), (C1 * e2i) - (C3 * e3i[0]), e2r, e2i, tr1, ti1);
        tr1[0] = 1.0 + tr1[0] + (C4 * e3r[0]);
        ti1[0] = ti1[0] + (C4 * e3i[0]);
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
        edr = ear[0] - (6.0 * ebr[0]);
        edi = eai[0] - (6.0 * ebi[0]);
        eer = edr + (2.0 * ecr);
        eei = edi + (2.0 * eci);
        zmlt(delzdr, delzdi, C4d * ear[0], C4d * eai[0], tr1, ti1);
        tr1[0] = (-C3d * ecr) + tr1[0];
        ti1[0] = (-C3d * eci) + ti1[0];
        zmlt(delzdr, delzdi, tr1[0], ti1[0], tr1, ti1);
        tr1[0] = (C2d * eer) + tr1[0];
        ti1[0] = (C2d * eei) + ti1[0];
        zmlt(delzdr, delzdi, tr1[0], ti1[0], tr1, ti1);
        zmlt(C6d * delzr, C6d * delzi, eer, eei, tr2, ti2);
        tr2[0] = -C1d + (C5d * edr) - tr2[0];
        ti2[0] = (C5d * edi) - ti2[0];
        zmlt(edr, edi, tr2[0], ti2[0], tr2, ti2);
        tr1[0] = 1.0 + tr2[0] + tr1[0];
        ti1[0] = ti2[0] + ti1[0];
        zmlt(facdr[0], facdi[0], tr1[0], ti1[0], tr1, ti1);
        zmlt(tr1[0], ti1[0], avidr[0], avidi[0], tr1, ti1);
        sqrtc(avidr[0], avidi[0], tr2, ti2);
        zmlt(tr1[0], ti1[0], tr2[0], ti2[0], tr1, ti1);
        rdr[0] = (3.0 * sumdr) + tr1[0];
        rdi[0] = (3.0 * sumdi) + ti1[0];

        return;
    } // rfrd

    /**
     * Ported from Tohru Morita article.
     *
     * @param  xrtr  DOCUMENT ME!
     * @param  xrti  DOCUMENT ME!
     * @param  yrtr  DOCUMENT ME!
     * @param  yrti  DOCUMENT ME!
     */
    private void rfrdb(double xrtr, double xrti, double yrtr, double yrti) {
        double[] sqrtxr = new double[1];
        double[] sqrtxi = new double[1];
        double[] sqrtyr = new double[1];
        double[] sqrtyi = new double[1];
        double[] xtr = new double[1];
        double[] xti = new double[1];
        double[] ytr = new double[1];
        double[] yti = new double[1];
        double[] facr = new double[1];
        double[] faci = new double[1];
        double sumdr;
        double sumdi;
        double[] facdr = new double[1];
        double[] facdi = new double[1];
        double alambr;
        double alambi;
        double[] tr1 = new double[1];
        double[] ti1 = new double[1];
        double[] zir = new double[1];
        double[] zii = new double[1];
        double[] sqrtzir = new double[1];
        double[] sqrtzii = new double[1];
        double delxr;
        double delxi;
        double delyr;
        double delyi;
        double xytr;
        double xyti;

        sqrtxr[0] = xrtr;
        sqrtxi[0] = xrti;
        sqrtyr[0] = yrtr;
        sqrtyi[0] = yrti;
        zmlt(xrtr, xrti, xrtr, xrti, xtr, xti);
        zmlt(yrtr, yrti, yrtr, yrti, ytr, yti);

        if ((zabs(xtr[0] + ytr[0], xti[0] + yti[0]) < TINYd) || (zabs(xtr[0], xti[0]) > BIGd) ||
                (zabs(ytr[0], yti[0]) > BIGd)) {
            rdr[0] = Double.NaN;

            return;
        }

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
            delyi = -yti[0];

            if ((zabs(delxr, delxi) < ERRTOLb) && (zabs(delyr, delyi) < ERRTOLb)) {
                break;
            }

            sqrtc(xtr[0], xti[0], sqrtxr, sqrtxi);
            sqrtc(ytr[0], yti[0], sqrtyr, sqrtyi);
            nrep++;
        } // while (true)

        xytr = xtr[0] + ytr[0];
        xyti = xti[0] + yti[0];
        zmlt(facr[0], faci[0], (8.0 - xytr) / 6.0, -xyti / 6.0, rfr, rfi);
        zmlt(facdr[0], facdi[0], 1.6 - (0.3 * xytr), -0.3 * xyti, rdr, rdi);
        rdr[0] = (3 * sumdr) + rdr[0];
        rdi[0] = (3 * sumdi) + rdi[0];

        return;
    } // rfrdb

    /**
     * DOCUMENT ME!
     */
    private void runTest() {
        int count;
        firstr = new double[1];
        firsti = new double[1];
        secondr = new double[1];
        secondi = new double[1];
        first = new double[1];
        second = new double[1];
        u = new double[1];
        v = new double[1];
        errorFlag = new int[1];

        double firstr1 = 0.0;
        double firsti1 = 0.0;
        double secondr1 = 0.0;
        double secondi1 = 0.0;
        double firstr2 = 0.0;
        double firsti2 = 0.0;
        double secondr2 = 0.0;
        double secondi2 = 0.0;
        double first1 = 0.0;
        double second1 = 0.0;
        double first2 = 0.0;
        double second2 = 0.0;
        double firstr3 = 0.0;
        double firsti3 = 0.0;
        double secondr3 = 0.0;
        double secondi3 = 0.0;
        int errorsDetected = 0;
        double tol = 1.0E-5;
        int angle;
        int angler;
        int anglei;
        double[] numr = new double[1];
        double[] numi = new double[1];
        double[] denr = new double[1];
        double[] deni = new double[1];
        double[] outr = new double[1];
        double[] outi = new double[1];
        double firstAbs1 = 0.0;
        double secondAbs1 = 0.0;
        double firstAbs3 = 0.0;
        double secondAbs3 = 0.0;
        double firstAng1 = 0.0;
        double secondAng1 = 0.0;
        double firstAng3 = 0.0;
        double secondAng3 = 0.0;

        // Check complete Elliptic Integrals of the First and Second Kinds for real arguments
        UI.setDataText("Complete Elliptic Integrals of the First and Second Kinds for real arguments\n");
        modi = 0.0;
        complementaryModulusUsed = false;
        analyticContinuationUsed = false;
        phir = pihf;
        phii = 0.0;
        phi = pihf;

        for (count = 0; count <= 100; count++) {
            modr = count * 0.01;
            UI.setDataText("modr = " + modr + "\n");
            errorFlag[0] = 0;
            source = MORITA_SOURCE;
            complementaryModulusUsed = false;
            complete = true;
            runcel12 = true;
            run();

            if (errorFlag[0] != 0) {
                UI.setDataText("Morita arithmetic-geometric mean gave error\n");
            } else {
                firstr1 = firstr[0];
                firsti1 = firsti[0];
                secondr1 = secondr[0];
                secondi1 = secondi[0];
                UI.setDataText("Morita arithmetic-geometric mean: K(" + modr + ") = " + firstr1 + "  i * " + firsti1 +
                               "  E(" + modr + ") = " + secondr1 + "  i * " + secondi1 + "\n");
            }

            errorFlag[0] = 0;
            source = MORITA_SOURCE;
            complementaryModulusUsed = false;
            complete = true;
            runcel12 = false;
            useStandardMethod = true;
            run();

            if (errorFlag[0] != 0) {
                UI.setDataText("Morita Carlson's gave error\n");
            } else {
                firstr2 = firstr[0];
                firsti2 = firsti[0];
                secondr2 = secondr[0];
                secondi2 = secondi[0];
                UI.setDataText("Morita Carlson's: K(" + modr + ") = " + firstr2 + "  i * " + firsti2 + "  E(" + modr +
                               ") = " + secondr2 + "  i * " + secondi2 + "\n");
            }

            errorFlag[0] = 0;
            comelp(modr);

            if (errorFlag[0] != 0) {
                UI.setDataText("ZHANG comelp gave error\n");
            } else {
                first1 = first[0];
                second1 = second[0];
                UI.setDataText("ZHANG comelp: K(" + modr + ") = " + first1 + "  E(" + modr + ") = " + second1 + "\n");
            }

            errorFlag[0] = 0;
            elit(modr, phir);

            if (errorFlag[0] != 0) {
                UI.setDataText("ZHANG elit gave error\n");
            } else {
                first2 = first[0];
                second2 = second[0];
                UI.setDataText("ZHANG elit: K(" + modr + ") = " + first2 + "  E(" + modr + ") = " + second2 + "\n");
            }

            if ((Math.abs((firstr2 - firstr1) / firstr1) > tol) || (Math.abs((first1 - firstr1) / firstr1) > tol) ||
                    (Math.abs((first2 - firstr1) / firstr1) > tol) ||
                    (Math.abs((secondr2 - secondr1) / secondr1) > tol) ||
                    (Math.abs((second1 - secondr1) / secondr1) > tol) ||
                    (Math.abs((second2 - secondr1) / secondr1) > tol)) {
                UI.setDataText("Error detected\n");
                errorsDetected++;
            }
        } // for (count = 0; count <= 100; count++)

        // Check incomplete Elliptic Integrals of the First and Second Kinds for real arguments
        UI.setDataText("Incomplete Elliptic Integrals of the First and Second Kinds for real arguments\n");

        for (count = 0; count <= 100; count += 10) {
            modr = count * 0.01;
            UI.setDataText("modr = " + modr + "\n");
            kc = Math.sqrt(1.0 - (modr * modr));

            for (angle = 0; angle < 100; angle += 10) {
                phir = pihf * angle / 100.0;
                UI.setDataText("phir = " + phir + "\n");

                errorFlag[0] = 0;
                source = MORITA_SOURCE;
                complementaryModulusUsed = false;
                complete = false;
                runcel12 = false;
                useStandardMethod = true;
                run();

                if (errorFlag[0] != 0) {
                    UI.setDataText("Morita Carlson's gave error\n");
                } else {
                    UI.setDataText("Morita standard Carlson's: F(" + modr + ", " + phir + ") = " + firstr[0] +
                                   "  i * " + firsti[0] + "  E(" + modr + ", " + phir + ") = " + secondr[0] + "  i * " +
                                   secondi[0] + "\n");
                }

                errorFlag[0] = 0;
                phir = pihf * angle / 100.0;
                source = MORITA_SOURCE;
                complementaryModulusUsed = false;
                complete = false;
                runcel12 = false;
                useStandardMethod = false;
                run();

                if (errorFlag[0] != 0) {
                    UI.setDataText("Morita short Carlson's gave error\n");
                } else {
                    UI.setDataText("Morita short Carlson's: F(" + modr + ", " + phir + ") = " + firstr[0] + "  i * " +
                                   firsti[0] + "  E(" + modr + ", " + phir + ") = " + secondr[0] + "  i * " +
                                   secondi[0] + "\n");
                }

                errorFlag[0] = 0;
                phir = pihf * angle / 100.0;
                elit(modr, phir);

                if (errorFlag[0] != 0) {
                    UI.setDataText("ZHANG elit gave error\n");
                } else {
                    UI.setDataText("ZHANG elit: F(" + modr + ", " + phir + ") = " + first[0] + "  E(" + modr + ", " +
                                   phir + ") = " + second[0] + "\n");
                }

                errorFlag[0] = 0;

                // Calculate F(modr, phir)
                elco2(Math.tan(phir), 0.0, kc, 1.0, 1.0);

                if (errorFlag[0] != 0) {
                    UI.setDataText("CERN elco2 gave error\n");
                } else {
                    firstr3 = u[0];
                    firsti3 = v[0];
                    UI.setDataText("CERN elco2: F(" + modr + ", " + phir + ") = " + firstr3 + " i * " + firsti3 + "\n");
                }

                errorFlag[0] = 0;

                // Calculate E(modr, phir)
                phir = pihf * angle / 100.0;
                elco2(Math.tan(phir), 0.0, kc, 1.0, kc * kc);

                if (errorFlag[0] != 0) {
                    UI.setDataText("CERN elco2 gave error\n");
                } else {
                    secondr3 = u[0];
                    secondi3 = v[0];
                    UI.setDataText("CERN elco2: E(" + modr + ", " + phir + ") = " + secondr3 + " i * " + secondi3 +
                                   "\n");
                }

                if ((Math.abs((first[0] - firstr[0]) / firstr[0]) > tol) ||
                        (Math.abs((firstr3 - firstr[0]) / firstr[0]) > tol) ||
                        (Math.abs((second[0] - secondr[0]) / secondr[0]) > tol) ||
                        (Math.abs((secondr3 - secondr[0]) / secondr[0]) > tol)) {
                    UI.setDataText("Error detected\n");
                    errorsDetected++;
                }
            } // for (angle = 0; angle < 100; angle += 10)
        } // for (count = 0; count <= 100; count += 10)

        // Check incomplete Elliptic Integrals of the First and Second Kinds for real argument mod and complex argument
        // phi
        UI.setDataText("Incomplete Elliptic Integrals of the First and Second Kinds for real argument mod and complex argument phi\n");

        for (count = 0; count <= 100; count += 10) {
            modr = count * 0.01;
            UI.setDataText("modr = " + modr + "\n");
            kc = Math.sqrt(1.0 - (modr * modr));

            for (angler = 0; angler < 100; angler += 20) {
                phir = pihf * angler / 100.0;
                UI.setDataText("phir = " + phir + "\n");

                for (anglei = 0; anglei < 100; anglei += 20) {
                    phii = pihf * anglei / 100.0;
                    UI.setDataText("phii = " + phii + "\n");

                    errorFlag[0] = 0;
                    source = MORITA_SOURCE;
                    complementaryModulusUsed = false;
                    complete = false;
                    runcel12 = false;
                    useStandardMethod = true;
                    run();

                    if (errorFlag[0] != 0) {
                        UI.setDataText("Morita Carlson's gave error\n");
                    } else {
                        UI.setDataText("Morita Carlson's: F(" + modr + ", " + phir + ", " + phii + ") = " + firstr[0] +
                                       "  i * " + firsti[0] + "  E(" + modr + ", " + phir + ", " + phii + ") = " +
                                       secondr[0] + "  i * " + secondi[0] + "\n");
                        firstAbs1 = zabs(firstr[0], firsti[0]);
                        secondAbs1 = zabs(secondr[0], secondi[0]);
                        firstAng1 = Math.atan2(firsti[0], firstr[0]);
                        secondAng1 = Math.atan2(secondi[0], secondr[0]);
                    }

                    phir = pihf * angler / 100.0;
                    phii = pihf * anglei / 100.0;
                    zsin(phir, phii, numr, numi);
                    zcos(phir, phii, denr, deni);
                    zdiv(numr[0], numi[0], denr[0], deni[0], outr, outi);

                    if (outr[0] >= 0.0) {
                        errorFlag[0] = 0;

                        // Calculate F(modr, phir)
                        elco2(outr[0], outi[0], kc, 1.0, 1.0);

                        if (errorFlag[0] != 0) {
                            UI.setDataText("CERN elco2 gave error\n");
                        } else {
                            firstr3 = u[0];
                            firsti3 = v[0];
                            firstAbs3 = zabs(firstr3, firsti3);
                            firstAng3 = Math.atan2(firsti3, firstr3);
                            UI.setDataText("CERN elco2: F(" + modr + ", " + phir + ", " + phii + ") = " + firstr3 +
                                           " i * " + firsti3 + "\n");
                        }

                        errorFlag[0] = 0;

                        // Calculate E(modr, phir)
                        elco2(outr[0], outi[0], kc, 1.0, kc * kc);

                        if (errorFlag[0] != 0) {
                            UI.setDataText("CERN elco2 gave error\n");
                        } else {
                            secondr3 = u[0];
                            secondi3 = v[0];
                            secondAbs3 = zabs(secondr3, secondi3);
                            secondAng3 = Math.atan2(secondi3, secondr3);
                            UI.setDataText("CERN elco2: E(" + modr + ", " + phir + ", " + phii + ") = " + secondr3 +
                                           " i * " + secondi3 + "\n");
                        }

                        if ((Math.abs((firstAbs3 - firstAbs1) / firstAbs1) > tol) ||
                                (Math.abs((secondAbs3 - secondAbs1) / secondAbs1) > tol) ||
                                ((firstAng1 != 0.0) && (Math.abs((firstAng3 - firstAng1) / firstAng1) > tol)) ||
                                ((secondAng1 != 0.0) && (Math.abs((secondAng3 - secondAng1) / secondAng1) > tol))) {
                            UI.setDataText("Error detected\n");
                            errorsDetected++;
                        }
                    } // if (outr[0] >= 0.0)
                } // for (anglei = 0; anglei < 100; anglei += 20)
            } // for (angler = 0; angler < 100; angler += 20)
        } // for (count = 0; count <= 100; count += 10)

        UI.setDataText("Errors detected = " + errorsDetected + "\n");
    } // runTest()

    /**
     * DOCUMENT ME!
     *
     * @param   x  DOCUMENT ME!
     *
     * @return  DOCUMENT ME!
     */
    private double sinh(double x) {
        double var;
        var = (Math.exp(x) - Math.exp(-x)) / 2.0;

        return var;
    }

    /**
     * Ported from Tohru Morita article Performs square root of complex argument.
     *
     * @param  zinr  DOCUMENT ME!
     * @param  zini  DOCUMENT ME!
     * @param  zsqr  DOCUMENT ME!
     * @param  zsqi  DOCUMENT ME!
     */
    private void sqrtc(double zinr, double zini, double[] zsqr, double[] zsqi) {
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

    /**
     * DOCUMENT ME!
     *
     * @param  inr   DOCUMENT ME!
     * @param  ini   DOCUMENT ME!
     * @param  outr  DOCUMENT ME!
     * @param  outi  DOCUMENT ME!
     */
    private void zcos(double inr, double ini, double[] outr, double[] outi) {
        outr[0] = Math.cos(inr) * cosh(ini);
        outi[0] = -Math.sin(inr) * sinh(ini);

        return;
    } // private void zcos

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

    /**
     * complex logarithm b = clog(a).
     *
     * @param  ar    double
     * @param  ai    double
     * @param  br    double[]
     * @param  bi    double[]
     * @param  ierr  int[] ierr = 0, normal return ierr = 1, z = cmplx(0.0, 0.0)
     */
    private void zlog(double ar, double ai, double[] br, double[] bi, int[] ierr) {
        double theta;
        double zm;
        ierr[0] = 0;

        if (ar == 0.0) {

            if (ai == 0.0) {
                ierr[0] = 1;

                return;
            } // if (ai == 0.0)
            else {

                if (ai > 0.0) {
                    bi[0] = Math.PI / 2.0;
                } else {
                    bi[0] = -Math.PI / 2.0;
                }

                br[0] = Math.log(Math.abs(ai));

                return;
            }
        } // if (ar == 0.0)
        else if (ai == 0.0) {

            if (ar > 0.0) {
                br[0] = Math.log(ar);
                bi[0] = 0.0;

                return;
            } else {
                br[0] = Math.log(Math.abs(ar));
                bi[0] = Math.PI;

                return;
            }
        } // else if (ai == 0.0)

        theta = Math.atan(ai / ar);

        if ((theta <= 0.0) && (ar < 0.0)) {
            theta = theta + Math.PI;
        } else if (ar < 0.0) {
            theta = theta - Math.PI;
        }

        zm = zabs(ar, ai);
        br[0] = Math.log(zm);
        bi[0] = theta;

        return;
    }

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
     * DOCUMENT ME!
     *
     * @param  inr   DOCUMENT ME!
     * @param  ini   DOCUMENT ME!
     * @param  outr  DOCUMENT ME!
     * @param  outi  DOCUMENT ME!
     */
    private void zsin(double inr, double ini, double[] outr, double[] outi) {
        outr[0] = Math.sin(inr) * cosh(ini);
        outi[0] = Math.cos(inr) * sinh(ini);

        return;
    } // private void zsin

}
