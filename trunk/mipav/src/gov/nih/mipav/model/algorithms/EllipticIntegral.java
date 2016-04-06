package gov.nih.mipav.model.algorithms;



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
 *          <p>Source: 1.) Port of FORTRAN code for complete and incomplete
 *          elliptic integrals of the first kind with real arguments from Computation of Special Functions by Shanjie
 *          Zhang and Jianming Jin, John Wiley & Sons, Inc., 1996, Chapter 18, Elliptic Integrals and Jacobian Elliptic
 *          Functions, pp. 654-679. 
 */

public class EllipticIntegral {

    //~ Static fields/initializers -------------------------------------------------------------------------------------

    

    /** Real arguments for first and second complete and incomplete elliptic integrals. */

    // ~ Instance fields ------------------------------------------------------------------------------------------------

    //~ Instance fields ------------------------------------------------------------------------------------------------

    /** DOCUMENT ME! */
    private double c;

    /** DOCUMENT ME! */
    private double[] first;

    /** modulus, 0 <= mod <= 1. */
    private double mod;

    /** DOCUMENT ME! */
    private double phi; // argument in radians

    /** DOCUMENT ME! */
    private double pihf = 0.5 * Math.PI;

    /** DOCUMENT ME! */
    private boolean runcomelp = false;

    /** DOCUMENT ME! */
    private boolean runelit = false;

    /** DOCUMENT ME! */
    private double[] second;

    /** DOCUMENT ME! */
    private double[] third;

    /** DOCUMENT ME! */
    private double TINY = 3.0E-138; // 3.0E-38

    //~ Constructors ---------------------------------------------------------------------------------------------------

    /**
     * ~ Constructors
     * ---------------------------------------------------------------------------------------------------
     */
    public EllipticIntegral() {
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
        this.mod = mod;
        this.phi = phi;
        this.c = c;
        this.third = third;
    }

    

    //~ Methods --------------------------------------------------------------------------------------------------------

    /**
     * DOCUMENT ME!
     */
    public void run() {
        if (runcomelp) {
            comelp(mod);
        } else if (runelit) {
            elit(mod, phi);
        } else {
            elit3(mod, phi, c);
        }
    } // public void run()

    

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

    }
