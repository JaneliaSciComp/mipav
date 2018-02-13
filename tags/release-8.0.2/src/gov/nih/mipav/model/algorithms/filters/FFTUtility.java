package gov.nih.mipav.model.algorithms.filters;


import gov.nih.mipav.model.algorithms.AlgorithmBase;
import gov.nih.mipav.model.algorithms.RandomNumberGen;
import gov.nih.mipav.view.MipavUtil;
import gov.nih.mipav.view.Preferences;


/**
 * This FFT should only be used when it is necessary to perform a N-dimensional FFT on non-power of two data without
 * adding padding. Otherwise, developers should use AlgorithmFFT (which has multithreading/jocl optimizations).
 * 
 * For forward transforms arrays a and b originally hold the real and imaginary components of the data, and return the
 * real and imaginary components of the resulting fourier coefficients. The roles reverse for the inverse transform.
 * multivariate data is indexed according to Java indexing conventions in which the last dimension is contiguous in
 * memory, without limit on the number of implied multiple subscripts. the subroutine is called once for each variate.
 * the calls for a multivariate transform may be in any order. n is the dimension of the current variable. If n has more
 * than 15 "factors", FFT terminates with an error message. The smallest number with 16 "factors" is 12,754,584, so this
 * error condition should not occur very often. nspn is the spacing of consecutive data values while indexing the
 * current variable. nseg*n*nspn is the total number of complex data values. The sign of isn determines the sign of the
 * complex exponential, and the magnitude of isn is normally one. isn is -1 for the forward transform and +1 for the
 * inverse transform. The magnitude of isn determines the indexing increment for a & b. If fft is called twice, with
 * opposite signs on isn, an identity transformation is done. Calls can be in either order. The results are scaled by
 * 1/n when the sign of isn is positive. a tri-variate transform with a(n1,n2,n3), b(n1,n2,n3) is computed by FFTUtility
 * fft = new FFTUtility(a,b,n1*n2,n3,1,-1,FFTUtility.FFT); fft.run(); fft = new
 * FFTUtility(a,b,n1,n2,n3,-1,FFTUtility.FFT); fft.run(); fft = new FFTUtility(a,b,1,n1,n2*n3,-1,FFTUtility.FFT);
 * fft.run(); for a single-variate transform, FFTUtility fft = new FFTUtility(a,b,1,n,1,-1,FFTUtility.FFT); fft.run();
 * 
 * @see AlgorithmFFT
 * 
 * <hr>
 * 
 * This class is based on the Fortran/C code for R. C. Singleton's mixed radix FFT algorithm, released under the below
 * license by Mark Olesen on NETLIB (netlib.org).
 * 
 * References: R. C. Singleton, An algorithm for computing the mixed radix fast Fourier transform, IEEE Transactions on
 * Audio and Electroacoustics, AU-17, no. 2, June, 1969, pp. 93-103.
 * 
 * multivariate complex fourier transform, computed in place using mixed-radix fast fourier transform algorithm. by r.
 * c. singleton, stanford research institute, sept. 1968
 * 
 * <pre>
 * Copyright(c)1995,97 Mark Olesen &lt;olesen@me.QueensU.CA&gt;
 *      Queen's Univ at Kingston (Canada)
 * 
 * Permission to use, copy, modify, and distribute this software for
 * any purpose without fee is hereby granted, provided that this
 * entire notice is included in all copies of any software which is
 * or includes a copy or modification of this software and in all
 * copies of the supporting documentation for such software.
 * 
 * THIS SOFTWARE IS BEING PROVIDED &quot;AS IS&quot;, WITHOUT ANY EXPRESS OR
 * IMPLIED WARRANTY.  IN PARTICULAR, NEITHER THE AUTHOR NOR QUEEN'S
 * UNIVERSITY AT KINGSTON MAKES ANY REPRESENTATION OR WARRANTY OF ANY
 * KIND CONCERNING THE MERCHANTABILITY OF THIS SOFTWARE OR ITS
 * FITNESS FOR ANY PARTICULAR PURPOSE.
 * 
 * All of which is to say that you can do what you like with this
 * source code provided you don't try to sell it as your own and you
 * include an unaltered copy of this message (including the
 * copyright).
 * 
 * It is also implicitly understood that bug fixes and improvements
 * should make their way back to the general Internet community so
 * that everyone benefits.
 * </pre>
 */
public class FFTUtility extends AlgorithmBase {
    // ~ Static fields/initializers
    // -------------------------------------------------------------------------------------

    /** DOCUMENT ME! */
    public static final int FFT = 1;

    /** DOCUMENT ME! */
    public static final int REALS = 2;

    /** DOCUMENT ME! */
    public static final int REALT = 3;

    /** DOCUMENT ME! */
    public static final int SELF_TEST = 4;

    // ~ Instance fields
    // ------------------------------------------------------------------------------------------------

    /** DOCUMENT ME! */
    private double[] a;

    /** DOCUMENT ME! */
    private double[] b;

    /** DOCUMENT ME! */
    private int functionType;

    /** DOCUMENT ME! */
    private int isn;

    /** DOCUMENT ME! */
    private int n;

    /** DOCUMENT ME! */
    private int nseg;

    /** DOCUMENT ME! */
    private int nspn;
    
    private boolean showProgress = true;

    // ~ Constructors
    // ---------------------------------------------------------------------------------------------------

    /**
     * This constructor should only be used for functionType == SELF_TEST.
     * 
     * @param functionType DOCUMENT ME!
     */
    public FFTUtility(int functionType) {
        this.functionType = functionType;

        if (functionType != SELF_TEST) {
            MipavUtil.displayError("functionType in this constructor must be SELF_TEST");
        }
    }

    /**
     * Creates a new FFTUtility object.
     * 
     * @param a DOCUMENT ME!
     * @param b DOCUMENT ME!
     * @param nseg DOCUMENT ME!
     * @param n DOCUMENT ME!
     * @param nspn DOCUMENT ME!
     * @param isn DOCUMENT ME!
     * @param functionType DOCUMENT ME!
     */
    public FFTUtility(double[] a, double[] b, int nseg, int n, int nspn, int isn, int functionType) {
        this.a = a;
        this.b = b;
        this.nseg = nseg;
        this.n = n;
        this.nspn = nspn;
        this.isn = isn;
        this.functionType = functionType;
    }

    // ~ Methods
    // --------------------------------------------------------------------------------------------------------

    /**
     * DOCUMENT ME!
     */
    public void finalize() {
        a = null;
        b = null;
        super.finalize();
    }
    
    /**
     *    
     * @param showProgress
     */
    public void setShowProgress(boolean showProgress) {
    	this.showProgress = showProgress;
    }

    /**
     * Starts the program.
     */
    public void runAlgorithm() {

        if (functionType == FFT) {
            fft();
        } else if (functionType == REALS) {
            reals();
        } else if (functionType == REALT) {
            realt();
        } else if (functionType == SELF_TEST) {
            selfTest();
        }
    }

    /**
     * DOCUMENT ME!
     * 
     * @return DOCUMENT ME!
     */
    double[] normal() {

        // Generates an independent pair of random normal deviates
        // Method due to G. Marsaglia and T. A. Bray,
        // SIAM Review, VOl. 6, No. 3, July, 1964., pp. 260-264.
        // output[] independent pair of random normal deviates
        double[] output = new double[2];
        RandomNumberGen randomGen;
        double rx, ry;
        double r;
        randomGen = new RandomNumberGen();

        do { // while (r >= 1.0)
            rx = randomGen.genUniformRandomNum( -1.0f, 1.0f);
            ry = randomGen.genUniformRandomNum( -1.0f, 1.0f);
            r = (rx * rx) + (ry * ry);
        } while (r >= 1.0);

        r = Math.sqrt( -2.0 * Math.log(r) / r);
        output[0] = rx * r;
        output[1] = ry * r;

        return output;
    }

    /**
     * DOCUMENT ME!
     * 
     * @param a DOCUMENT ME!
     * @param b DOCUMENT ME!
     * @param c DOCUMENT ME!
     * @param d DOCUMENT ME!
     * @param n DOCUMENT ME!
     * 
     * @return DOCUMENT ME!
     */
    double[] rms(double[] a, double[] b, double[] c, double[] d, int n) {
        // Computes rms error for transform-inverse pair arrays a,b = transform, inverse results arrays c,d = original
        // data n = dimension of arrays a, b, c, and d

        // rms errors for a and b arrays
        double[] error = new double[2];
        double ssa = 0.0;
        double ssb = 0.0;
        int j;
        double amc;
        double bmd;

        for (j = 0; j < n; j++) {
            amc = a[j] - c[j];
            ssa = (amc * amc) + ssa;
            bmd = b[j] - d[j];
            ssb = (bmd * bmd) + ssb;
        }

        error[0] = Math.sqrt(ssa / (double) n);
        error[1] = Math.sqrt(ssb / (double) n);

        return error;
    }

    /**
     * DOCUMENT ME!
     */
    private void fft() {

        // ARRAY NFAC IS WORKING STORAGE FOR FACTORING N. THE SMALLEST
        // NUMBER EXCEEDING THE 15 LOCATIONS PROVIDED IS 12,754,584.
        int[] nfac = new int[15];
        int m;
        int nf;
        int k;
        int nspan;
        int ntot;
        int j;
        int jj;
        int kt;
        int maxp = 0;
        int maxf;
        double[] at;
        double[] ck;
        double[] bt;
        double[] sk;
        int[] np;
        int inc;
        int nt;
        int ks;
        double rad;
        double s72;
        double c72;
        double s120;
        double ak;
        int kspan;
        int nn;
        int jc;
        int lim;
        int klim;
        int i;
        int jf;
        double dr = 0.0;
        double cd = 0.0;
        double sd = 0.0;
        int kk = 0;
        int k1 = 0;
        int k2 = 0;
        double bk;
        double c1 = 0.0;
        double s1 = 0.0;
        int mm = 0;
        double aj;
        double bj;
        int kspnn = 0;
        double c2 = 0.0;
        double c3 = 0.0;
        double s2 = 0.0;
        double s3 = 0.0;
        int k3 = 0;
        double akp = 0.0;
        double akm = 0.0;
        double ajp = 0.0;
        double ajm = 0.0;
        double bkp = 0.0;
        double bkm = 0.0;
        double bjp = 0.0;
        double bjm = 0.0;
        int k4;
        double aa;
        double bb;
        double sindrrad;
        boolean seg40ato110 = true;
        boolean seg60to70 = true;
        boolean seg70to80 = false;
        boolean seg80ato90 = true;
        boolean seg110to290 = true;
        boolean seg110to120 = true;
        boolean seg110ato230 = true;
        boolean seg120to130 = true;
        boolean seg130to150 = true;
        boolean seg130to140 = true;
        boolean seg150ato180 = true;
        boolean seg160to180 = true;
        boolean seg180to190 = true;
        boolean seg150to150a = true;
        boolean seg150ato160 = true;
        boolean seg160to170 = true;
        boolean seg170ato180 = true;
        boolean seg190to200 = true;
        boolean seg40to40a = true;
        boolean seg40ato100 = true;
        boolean seg230to230a = true;
        boolean seg230ato290 = true;
        boolean seg200to210 = true;
        boolean seg300to310 = true;
        boolean seg310to320 = true;
        boolean seg460to470 = true;
        boolean seg470to480 = true;
        boolean seg490to500 = false;
        boolean seg510to570 = false;

        boolean goBack = false;
        boolean goBack2 = false;
        boolean goBack3 = false;
        boolean goBack4 = false;
        boolean goBack5 = false;
        boolean goBack6 = false;
        boolean goBack7 = false;
        boolean goBack8 = false;

        if (showProgress) {
            fireProgressStateChanged("FFT", "Performing FFT...");
        }

        // DETERMINE THE FACTORS OF n
        m = 0;
        nf = Math.abs(n);
        k = nf;

        if (nf == 1) {
            MipavUtil.displayError("Error! Dimension length less than 2");

            setCompleted(false);

            return;
        }

        nspan = Math.abs(nf * nspn);
        ntot = Math.abs(nspan * nseg);

        if ( (isn * ntot) == 0) {
            MipavUtil.displayError("Error! Zero in FFT parameters");
            Preferences.debug("nseg = " + nseg + " n = " + n + "\n", Preferences.DEBUG_ALGORITHM);
            Preferences.debug("nspn = " + nspn + " isn = " + isn + "\n", Preferences.DEBUG_ALGORITHM);

            setCompleted(false);

            return;
        } // if ((isn*ntot) == 0)

        while ( (k - ( (k / 16) * 16)) == 0) {
            m = m + 1;
            nfac[m - 1] = 4;
            k = k / 16;
        } // while (k - (k/16)*16 == 0)

        j = 3;
        jj = 9;

        do { // while (jj <= k)

            while ( (k % jj) == 0) {
                m = m + 1;
                nfac[m - 1] = j;
                k = k / jj;
            } // while ((k%jj) == 0)

            j = j + 2;
            jj = j * j;
        } while (jj <= k);

        if (k <= 4) {
            kt = m;
            nfac[m] = k;

            if (k != 1) {
                m = m + 1;
            }
        } // if (k <= 4)
        else { // k > 4

            if ( (k - ( (k / 4) * 4)) == 0) {
                m = m + 1;
                nfac[m - 1] = 2;
                k = k / 4;
            } // if ((k - (k / 4) * 4) == 0)

            // ALL SQUARE FACTORS OUT NOW, BUT K >= 5 STILL
            kt = m;
            maxp = Math.max(kt + kt + 2, k - 1);
            j = 2;

            do { // while (j <= k)

                if ( (k % j) == 0) {
                    m = m + 1;
                    nfac[m - 1] = j;
                    k = k / j;
                } // if ((k%j) == 0)

                j = ( ( (j + 1) / 2) * 2) + 1;
            } while (j <= k);
        } // else k > 4

        if (m <= (kt + 1)) {
            maxp = m + kt + 1;
        } // if (m <= (kt+1))

        if ( (m + kt) > 15) {
            MipavUtil.displayError("Error! FFT parameter n = " + n + " has more than 15 factors");

            setCompleted(false);

            return;
        } // if ((m+kt) > 15)

        if (kt != 0) {
            j = kt;

            do { // while (j != 0)
                m = m + 1;
                nfac[m - 1] = nfac[j - 1];
                j = j - 1;
            } while (j != 0);
        } // if (kt != 0)

        maxf = m - kt;
        maxf = nfac[maxf - 1];

        if (kt > 0) {
            maxf = Math.max(nfac[kt - 1], maxf);
        } // if (kt > 0)

        at = new double[maxf];
        ck = new double[maxf];
        bt = new double[maxf];
        sk = new double[maxf];
        np = new int[maxp];

        inc = Math.abs(isn);
        nt = inc * ntot;
        ks = inc * nspan;
        rad = Math.PI / 4.0;
        s72 = rad / 0.625;
        c72 = Math.cos(s72);
        s72 = Math.sin(s72);
        s120 = Math.sqrt(0.75);

        if (isn <= 0) {
            s72 = -s72;
            s120 = -s120;
            rad = -rad;
        } // if (isn <= 0)
        else { // isn > 0

            // SCALE BY 1 / nf FOR ISN.GT.0
            ak = 1.0 / (double) (nf);

            for (j = 0; j < nt; j += inc) {
                a[j] = a[j] * ak;
                b[j] = b[j] * ak;
            } // for (j = 0; j < nt; j += inc)
        } // else isn > 0

        kspan = ks;
        nn = nt - inc;
        jc = ks / nf;

        // SIN, COS VALUES ARE RE-INITIALIZED EACH LIM STEPS

        lim = 32;
        klim = lim * jc;
        i = 0;
        jf = 0;
        maxf = m - kt;
        maxf = nfac[maxf - 1];

        if (kt > 0) {
            maxf = Math.max(nfac[kt - 1], maxf);
        } // if (kt > 0)

        // COMPUTE FOURIER TRANSFORM

        outer: do { // while (goBack)
            goBack = false;

            if (seg40to40a) {
                dr = 8.0 * (double) (jc) / (double) (kspan);
                sindrrad = Math.sin(0.5 * dr * rad);
                cd = 2.0 * sindrrad * sindrrad;
                sd = Math.sin(dr * rad);
                kk = 1;
                i = i + 1;

                if (nfac[i - 1] != 2) {
                    seg40ato110 = false;
                } // if (nfac[i-1] != 2)
            } // if (seg40to40a)

            seg40to40a = true;

            if (seg40ato110) {

                if (seg40ato100) {

                    // TRANSFORM FOR FACTOR OF 2 (INCLUDING ROTATION FACTOR)

                    kspan = kspan / 2;
                    k1 = kspan + 2;

                    do { // while (kk <= jc)

                        do { // while (kk <= nn)
                            k2 = kk + kspan;
                            ak = a[k2 - 1];
                            bk = b[k2 - 1];
                            a[k2 - 1] = a[kk - 1] - ak;
                            b[k2 - 1] = b[kk - 1] - bk;
                            a[kk - 1] = a[kk - 1] + ak;
                            b[kk - 1] = b[kk - 1] + bk;
                            kk = k2 + kspan;
                        } while (kk <= nn);

                        kk = kk - nn;
                    } while (kk <= jc);

                    if (kk > kspan) {
                        break;
                    }

                    do { // while goBack2
                        goBack2 = false;

                        do { // while (kk <= (jc+jc))

                            if (seg60to70) {
                                c1 = 1.0 - cd;
                                s1 = sd;
                                mm = Math.min(k1 / 2, klim);
                            } // if (seg60to70)

                            seg60to70 = true;
                            seg70to80 = false;

                            do { // while kk <= mm

                                if (seg70to80) {
                                    ak = c1 - ( (cd * c1) + (sd * s1));
                                    s1 = ( (sd * c1) - (cd * s1)) + s1;

                                    // THE FOLLOWING THREE STATEMENTS COMPENSATE FOR
                                    // TRUNCATION ERROR.IF ROUNDED ARITHMETIC IS USED, SUBSTITUTE
                                    // C1 = AK

                                    c1 = (0.5 / ( (ak * ak) + (s1 * s1))) + 0.5;
                                    s1 = c1 * s1;
                                    c1 = c1 * ak;
                                } // if (seg70to80)

                                seg70to80 = true;

                                do { // while kk > k2

                                    do { // while kk < nt
                                        k2 = kk + kspan;
                                        ak = a[kk - 1] - a[k2 - 1];
                                        bk = b[kk - 1] - b[k2 - 1];
                                        a[kk - 1] = a[kk - 1] + a[k2 - 1];
                                        b[kk - 1] = b[kk - 1] + b[k2 - 1];
                                        a[k2 - 1] = (c1 * ak) - (s1 * bk);
                                        b[k2 - 1] = (s1 * ak) + (c1 * bk);
                                        kk = k2 + kspan;
                                    } while (kk < nt);

                                    k2 = kk - nt;
                                    c1 = -c1;
                                    kk = k1 - k2;
                                } while (kk > k2);

                                kk = kk + jc;
                            } while (kk <= mm);

                            if (kk < k2) {
                                seg80ato90 = false;

                                break;
                            }

                            k1 = k1 + inc + inc;
                            kk = ( (k1 - kspan) / 2) + jc;
                        } while (kk <= (jc + jc));

                        if (seg80ato90) {
                            goBack = true;

                            continue outer;
                        } // if (seg80ato90)

                        seg80ato90 = true;
                        s1 = (double) ( (kk - 1) / jc) * dr * rad;
                        c1 = Math.cos(s1);
                        s1 = Math.sin(s1);
                        mm = Math.min(k1 / 2, mm + klim);
                        seg60to70 = false;
                        goBack2 = true;
                    } while (goBack2);
                } // if (seg40ato100)

                seg40ato100 = true;

                // TRANSFORM FOR FACTOR OF 3 (OPTIONAL CODE)
                do { // while (kk <= kspan)

                    do { // while (kk < nn)
                        k1 = kk + kspan;
                        k2 = k1 + kspan;
                        ak = a[kk - 1];
                        bk = b[kk - 1];
                        aj = a[k1 - 1] + a[k2 - 1];
                        bj = b[k1 - 1] + b[k2 - 1];
                        a[kk - 1] = ak + aj;
                        b[kk - 1] = bk + bj;
                        ak = ( -0.5 * aj) + ak;
                        bk = ( -0.5 * bj) + bk;
                        aj = (a[k1 - 1] - a[k2 - 1]) * s120;
                        bj = (b[k1 - 1] - b[k2 - 1]) * s120;
                        a[k1 - 1] = ak - bj;
                        b[k1 - 1] = bk + aj;
                        a[k2 - 1] = ak + bj;
                        b[k2 - 1] = bk - aj;
                        kk = k2 + kspan;
                    } while (kk < nn);

                    kk = kk - nn;
                } while (kk <= kspan);

                seg110to290 = false;
            } // if (seg40ato110)

            seg40ato110 = true;

            if (seg110to290) {

                if (seg110to120) {

                    // TRANSFORM FOR FACTOR OF 4

                    if (nfac[i - 1] != 4) {
                        seg110ato230 = false;
                    }
                } // if (seg110to120)

                if (seg110ato230) {

                    if (seg110to120) {
                        kspnn = kspan;
                        kspan = kspan / 4;
                    } // if (seg110to120)

                    seg110to120 = true;

                    do { // while (goBack3)
                        goBack3 = false;

                        if (seg120to130) {
                            c1 = 1.0;
                            s1 = 0;
                            mm = Math.min(kspan, klim);
                            seg130to150 = false;
                        } // if (seg120to130)

                        seg120to130 = true;

                        if (seg130to150) {

                            if (seg130to140) {
                                c2 = c1 - ( (cd * c1) + (sd * s1));
                                s1 = ( (sd * c1) - (cd * s1)) + s1;

                                // THE FOLLOWING THREE STATEMENTS COMPENSATE FOR TRUNCATION
                                // ERROR.IF ROUNDED ARITHMETIC IS USED, SUBSTITUTE C1 = C2

                                c1 = (0.5 / ( (c2 * c2) + (s1 * s1))) + 0.5;
                                s1 = c1 * s1;
                                c1 = c1 * c2;
                            } // if (seg130to140)

                            seg130to140 = true;
                            c2 = (c1 * c1) - (s1 * s1);
                            s2 = c1 * s1 * 2.0;
                            c3 = (c2 * c1) - (s2 * s1);
                            s3 = (c2 * s1) + (s2 * c1);
                        } // if (seg130to150)

                        seg130to150 = true;

                        if (seg150to150a) {
                            k1 = kk + kspan;
                            k2 = k1 + kspan;
                            k3 = k2 + kspan;
                            akp = a[kk - 1] + a[k2 - 1];
                            akm = a[kk - 1] - a[k2 - 1];
                            ajp = a[k1 - 1] + a[k3 - 1];
                            ajm = a[k1 - 1] - a[k3 - 1];
                            a[kk - 1] = akp + ajp;
                            ajp = akp - ajp;
                            bkp = b[kk - 1] + b[k2 - 1];
                            bkm = b[kk - 1] - b[k2 - 1];
                            bjp = b[k1 - 1] + b[k3 - 1];
                            bjm = b[k1 - 1] - b[k3 - 1];
                            b[kk - 1] = bkp + bjp;
                            bjp = bkp - bjp;

                            if (isn < 0) {
                                seg150ato180 = false;
                            }
                        } // if (seg150to150a)

                        seg150to150a = true;

                        if (seg150ato180) {

                            if (seg150ato160) {
                                akp = akm - bjm;
                                akm = akm + bjm;
                                bkp = bkm + ajm;
                                bkm = bkm - ajm;

                                if (s1 == 0.0) {
                                    seg160to180 = false;
                                    seg180to190 = false;
                                }
                            } // if (seg150ato160)

                            seg150ato160 = true;

                            if (seg160to180) {

                                if (seg160to170) {
                                    a[k1 - 1] = (akp * c1) - (bkp * s1);
                                    b[k1 - 1] = (akp * s1) + (bkp * c1);
                                    a[k2 - 1] = (ajp * c2) - (bjp * s2);
                                    b[k2 - 1] = (ajp * s2) + (bjp * c2);
                                    a[k3 - 1] = (akm * c3) - (bkm * s3);
                                    b[k3 - 1] = (akm * s3) + (bkm * c3);
                                    kk = k3 + kspan;

                                    if (kk <= nt) {
                                        seg120to130 = false;
                                        seg130to150 = false;
                                        goBack3 = true;

                                        continue;
                                    }
                                } // if (seg160to170)

                                seg160to170 = true;
                                kk = kk - nt + jc;

                                if (kk <= mm) {
                                    seg120to130 = false;
                                    goBack3 = true;

                                    continue;
                                }

                                if (kk < kspan) {
                                    seg170ato180 = false;
                                    seg180to190 = false;
                                    seg190to200 = false;
                                }

                                if (seg170ato180) {
                                    kk = kk - kspan + inc;

                                    if (kk <= jc) {
                                        goBack3 = true;

                                        continue;
                                    }

                                    if (kspan == jc) {
                                        break outer;
                                    }

                                    goBack = true;

                                    continue outer;
                                } // if (seg170ato180)
                            } // if (seg160to180)
                        } // if (seg150ato180)

                        seg150ato180 = true;
                        seg160to180 = true;
                        seg170ato180 = true;

                        if (seg180to190) {
                            akp = akm + bjm;
                            akm = akm - bjm;
                            bkp = bkm - ajm;
                            bkm = bkm + ajm;

                            if (s1 != 0.0) {
                                seg120to130 = false;
                                seg130to150 = false;
                                seg150to150a = false;
                                seg150ato160 = false;
                                goBack3 = true;

                                continue;
                            }
                        } // if (seg180to190)

                        seg180to190 = true;

                        if (seg190to200) {
                            a[k1 - 1] = akp;
                            b[k1 - 1] = bkp;
                            a[k2 - 1] = ajp;
                            b[k2 - 1] = bjp;
                            a[k3 - 1] = akm;
                            b[k3 - 1] = bkm;
                            kk = k3 + kspan;

                            if (kk <= nt) {
                                seg120to130 = false;
                                seg130to150 = false;
                                goBack3 = true;

                                continue;
                            }

                            seg120to130 = false;
                            seg130to150 = false;
                            seg150to150a = false;
                            seg150ato160 = false;
                            seg160to170 = false;
                            goBack3 = true;

                            continue;
                        } // if (seg190to200)

                        seg190to200 = true;

                        if (seg200to210) {
                            s1 = (double) ( (kk - 1) / jc) * dr * rad;
                            c1 = Math.cos(s1);
                            s1 = Math.sin(s1);
                            mm = Math.min(kspan, mm + klim);
                            seg120to130 = false;
                            seg130to140 = false;
                            goBack3 = true;

                            continue;
                        } // if (seg200to210)

                        seg200to210 = true;
                    } while (goBack3);

                    // TRANSFORM FOR FACTOR OF 5 (OPTIONAL CODE)
                    c2 = (c72 * c72) - (s72 * s72);
                    s2 = 2.0 * c72 * s72;

                    do { // while (kk <= kspan)

                        do { // while (kk < nn)
                            k1 = kk + kspan;
                            k2 = k1 + kspan;
                            k3 = k2 + kspan;
                            k4 = k3 + kspan;
                            akp = a[k1 - 1] + a[k4 - 1];
                            akm = a[k1 - 1] - a[k4 - 1];
                            bkp = b[k1 - 1] + b[k4 - 1];
                            bkm = b[k1 - 1] - b[k4 - 1];
                            ajp = a[k2 - 1] + a[k3 - 1];
                            ajm = a[k2 - 1] - a[k3 - 1];
                            bjp = b[k2 - 1] + b[k3 - 1];
                            bjm = b[k2 - 1] - b[k3 - 1];
                            aa = a[kk - 1];
                            bb = b[kk - 1];
                            a[kk - 1] = aa + akp + ajp;
                            b[kk - 1] = bb + bkp + bjp;
                            ak = (akp * c72) + (ajp * c2) + aa;
                            bk = (bkp * c72) + (bjp * c2) + bb;
                            aj = (akm * s72) + (ajm * s2);
                            bj = (bkm * s72) + (bjm * s2);
                            a[k1 - 1] = ak - bj;
                            a[k4 - 1] = ak + bj;
                            b[k1 - 1] = bk + aj;
                            b[k4 - 1] = bk - aj;
                            ak = (akp * c2) + (ajp * c72) + aa;
                            bk = (bkp * c2) + (bjp * c72) + bb;
                            aj = (akm * s2) - (ajm * s72);
                            bj = (bkm * s2) - (bjm * s72);
                            a[k2 - 1] = ak - bj;
                            a[k3 - 1] = ak + bj;
                            b[k2 - 1] = bk + aj;
                            b[k3 - 1] = bk - aj;
                            kk = k4 + kspan;
                        } while (kk < nn);

                        kk = kk - nn;
                    } while (kk <= kspan);

                    seg230to230a = false;
                    seg230ato290 = false;
                } // if (seg110ato230)

                seg110ato230 = true;

                if (seg230to230a) {

                    // TRANSFORM FOR ODD FACTORS

                    k = nfac[i - 1];
                    kspnn = kspan;
                    kspan = kspan / k;

                    if (k == 3) {
                        seg40to40a = false;
                        seg40ato100 = false;
                        goBack = true;

                        continue;
                    }

                    if (k == 5) {
                        seg40to40a = false;
                        ;
                        seg40ato110 = false;
                        seg110to120 = false;
                        seg120to130 = false;
                        seg130to150 = false;
                        seg150to150a = false;
                        seg150ato180 = false;
                        seg180to190 = false;
                        seg190to200 = false;
                        seg200to210 = false;
                        goBack = true;

                        continue;
                    }
                } // if (seg230to230a)

                seg230to230a = true;

                if (seg230ato290) {

                    if (k != jf) {
                        jf = k;
                        s1 = rad / ((double) (k) / 8.0);
                        c1 = Math.cos(s1);
                        s1 = Math.sin(s1);
                        ck[jf - 1] = 1.0;
                        sk[jf - 1] = 0.0;
                        j = 1;

                        do { // while (j < k)
                            ck[j - 1] = (ck[k - 1] * c1) + (sk[k - 1] * s1);
                            sk[j - 1] = (ck[k - 1] * s1) - (sk[k - 1] * c1);
                            k = k - 1;
                            ck[k - 1] = ck[j - 1];
                            sk[k - 1] = -sk[j - 1];
                            j = j + 1;
                        } while (j < k);
                    } // if (k != jf)

                    do { // while (kk <= kspan)

                        do { // while (kk <= nn)
                            k1 = kk;
                            k2 = kk + kspnn;
                            aa = a[kk - 1];
                            bb = b[kk - 1];
                            ak = aa;
                            bk = bb;
                            j = 1;
                            k1 = k1 + kspan;

                            do { // while (k1 < k2)
                                k2 = k2 - kspan;
                                j = j + 1;
                                at[j - 1] = a[k1 - 1] + a[k2 - 1];
                                ak = at[j - 1] + ak;
                                bt[j - 1] = b[k1 - 1] + b[k2 - 1];
                                bk = bt[j - 1] + bk;
                                j = j + 1;
                                at[j - 1] = a[k1 - 1] - a[k2 - 1];
                                bt[j - 1] = b[k1 - 1] - b[k2 - 1];
                                k1 = k1 + kspan;
                            } while (k1 < k2);

                            a[kk - 1] = ak;
                            b[kk - 1] = bk;
                            k1 = kk;
                            k2 = kk + kspnn;
                            j = 1;

                            do { // while (j < k)
                                k1 = k1 + kspan;
                                k2 = k2 - kspan;
                                jj = j;
                                ak = aa;
                                bk = bb;
                                aj = 0.0;
                                bj = 0.0;
                                k = 1;

                                do { // while (k < jf)
                                    k = k + 1;
                                    ak = (at[k - 1] * ck[jj - 1]) + ak;
                                    bk = (bt[k - 1] * ck[jj - 1]) + bk;
                                    k = k + 1;
                                    aj = (at[k - 1] * sk[jj - 1]) + aj;
                                    bj = (bt[k - 1] * sk[jj - 1]) + bj;
                                    jj = jj + j;

                                    if (jj > jf) {
                                        jj = jj - jf;
                                    }
                                } while (k < jf);

                                k = jf - j;
                                a[k1 - 1] = ak - bj;
                                b[k1 - 1] = bk + aj;
                                a[k2 - 1] = ak + bj;
                                b[k2 - 1] = bk - aj;
                                j = j + 1;
                            } while (j < k);

                            kk = kk + kspnn;
                        } while (kk <= nn);

                        kk = kk - nn;
                    } while (kk <= kspan);
                } // if (seg230ato290)
            } // if (seg110to290)

            seg110to290 = true;
            seg230ato290 = true;

            // MULTIPLY BY ROTATION FACTOR(EXCEPT FOR FACTORS OF 2 AND 4)

            if (i == m) {
                break;
            }

            kk = jc + 1;

            do { // while (goBack4)
                goBack4 = false;

                if (seg300to310) {
                    c2 = 1.0 - cd;
                    s1 = sd;
                    mm = Math.min(kspan, klim);
                } // if (seg300to310)

                seg300to310 = true;
                seg310to320 = false;

                do { // while (kk <= mm)

                    if (seg310to320) {
                        c2 = c1 - ( (cd * c1) + (sd * s1));
                        s1 = s1 + ( (sd * c1) - (cd * s1));

                        // THE FOLLOWING THREE STATEMENTS COMPENSATE FOR TRUNCATION
                        // ERROR.IF ROUNDED ARITHMETIC IS USED, THEY MAY BE DELETED.

                        c1 = (0.5 / ( (c2 * c2) + (s1 * s1))) + 0.5;
                        s1 = c1 * s1;
                        c2 = c1 * c2;
                    } // if (seg310to320)

                    seg310to320 = true;
                    c1 = c2;
                    s2 = s1;
                    kk = kk + kspan;

                    do { // while (kk <= kspnn)

                        do { // while (kk <= nt)
                            ak = a[kk - 1];
                            a[kk - 1] = (c2 * ak) - (s2 * b[kk - 1]);
                            b[kk - 1] = (s2 * ak) + (c2 * b[kk - 1]);
                            kk = kk + kspnn;
                        } while (kk <= nt);

                        ak = s1 * s2;
                        s2 = (s1 * c2) + (c1 * s2);
                        c2 = (c1 * c2) - ak;
                        kk = kk - nt + kspan;
                    } while (kk <= kspnn);

                    kk = kk - kspnn + jc;
                } while (kk <= mm);

                if (kk >= kspan) {
                    kk = kk - kspan + jc + inc;

                    if (kk <= (jc + jc)) {
                        goBack4 = true;

                        continue;
                    }

                    goBack = true;

                    continue outer;
                } // if (kk >= kspan)

                s1 = (double) ( (kk - 1) / jc) * dr * rad;
                c2 = Math.cos(s1);
                s1 = Math.sin(s1);
                mm = Math.min(kspan, mm + klim);
                seg300to310 = false;
                goBack4 = true;

                continue;
            } while (goBack4);
        } while (goBack);

        // PERMUTE THE RESULTS TO NORMAL ORDER---DONE IN TWO STAGES
        // PERMUTATION FOR SQUARE FACTORS OF N

        np[0] = ks;

        if (kt != 0) {
            k = kt + kt + 1;

            if (m < k) {
                k = k - 1;
            }

            j = 1;
            np[k] = jc;

            do { // while (j < k)
                np[j] = np[j - 1] / nfac[j - 1];
                np[k - 1] = np[k] * nfac[j - 1];
                j = j + 1;
                k = k - 1;
            } while (j < k);

            k3 = np[k];
            kspan = np[1];
            kk = jc + 1;
            k2 = kspan + 1;
            j = 1;

            if (nf == ntot) {

                // PERMUTATION FOR SINGLE - VARIATE TRANSFORM(OPTIONAL CODE)
                outer5: do { // while (goBack5)
                    goBack5 = false;
                    ak = a[kk - 1];
                    a[kk - 1] = a[k2 - 1];
                    a[k2 - 1] = ak;
                    bk = b[kk - 1];
                    b[kk - 1] = b[k2 - 1];
                    b[k2 - 1] = bk;
                    kk = kk + inc;
                    k2 = kspan + k2;

                    if (k2 < ks) {
                        goBack5 = true;

                        continue;
                    }

                    do { // while (kk < ks)

                        do { // while (k2 > np[j-1])
                            k2 = k2 - np[j - 1];
                            j = j + 1;
                            k2 = np[j] + k2;
                        } while (k2 > np[j - 1]);

                        j = 1;

                        do { // while (k2 < ks)

                            if (kk < k2) {
                                goBack5 = true;

                                continue outer5;
                            }

                            kk = kk + inc;
                            k2 = kspan + k2;
                        } while (k2 < ks);
                    } while (kk < ks);
                } while (goBack5);

                jc = k3;
            } // if (nf == ntot)
            else { // nf != ntot

                // PERMUTATION FOR MULTIVARIATE TRANSFORM
                outer6: do { // while (goBack6)
                    goBack6 = false;
                    k = kk + jc;

                    do { // while (kk < k)
                        ak = a[kk - 1];
                        a[kk - 1] = a[k2 - 1];
                        a[k2 - 1] = ak;
                        bk = b[kk - 1];
                        b[kk - 1] = b[k2 - 1];
                        b[k2 - 1] = bk;
                        kk = kk + inc;
                        k2 = k2 + inc;
                    } while (kk < k);

                    kk = kk + ks - jc;
                    k2 = k2 + ks - jc;

                    if (kk < nt) {
                        goBack6 = true;

                        continue;
                    }

                    k2 = k2 - nt + kspan;
                    kk = kk - nt + jc;

                    if (k2 < ks) {
                        goBack6 = true;

                        continue;
                    }

                    do { // while (kk < ks)

                        do { // while (k2 > np[j-1])
                            k2 = k2 - np[j - 1];
                            j = j + 1;
                            k2 = np[j] + k2;
                        } while (k2 > np[j - 1]);

                        j = 1;

                        do { // while (k2 < ks)

                            if (kk < k2) {
                                goBack6 = true;

                                continue outer6;
                            }

                            kk = kk + jc;
                            k2 = kspan + k2;
                        } while (k2 < ks);
                    } while (kk < ks);
                } while (goBack6);

                jc = k3;
            } // else nf != ntot
        } // if (kt != 0)

        if ( ( (2 * kt) + 1) >= m) {

            setCompleted(true);

            return;
        }

        kspnn = np[kt];

        // PERMUTATION FOR SQUARE-FREE FACTORS OF N

        j = m - kt;
        nfac[j] = 1;

        do { // while (j != kt)
            nfac[j - 1] = nfac[j - 1] * nfac[j];
            j = j - 1;
        } while (j != kt);

        kt = kt + 1;
        nn = nfac[kt - 1] - 1;
        jj = 0;
        j = 0;
        seg460to470 = false;
        seg470to480 = false;

        outer7: do { // while (goBack7)
            goBack7 = false;

            if (seg460to470) {
                jj = jj - k2;
                k2 = kk;
                k = k + 1;
                kk = nfac[k - 1];
            } // if (seg460to470)

            seg460to470 = true;

            do { // while (j <= nn)

                if (seg470to480) {
                    jj = kk + jj;

                    if (jj >= k2) {
                        goBack7 = true;

                        continue outer7;
                    }

                    np[j - 1] = jj;
                } // if (seg470to480)

                seg470to480 = true;
                k2 = nfac[kt - 1];
                k = kt + 1;
                kk = nfac[k - 1];
                j = j + 1;
            } while (j <= nn);
        } while (goBack7);

        // DETERMINE THE PERMUTATION CYCLES OF LENGTH GREATER THAN 1

        j = 0;
        seg490to500 = false;

        outer8: do { // while (goBack8)
            goBack8 = false;

            if (seg490to500) {
                k = kk;
                kk = np[k - 1];
                np[k - 1] = -kk;

                if (kk != j) {
                    goBack8 = true;

                    continue;
                }

                k3 = kk;
            } // if (seg490to500)

            seg490to500 = true;

            do { // while (j != nn)

                do { // while (kk < 0)
                    j = j + 1;
                    kk = np[j - 1];
                } while (kk < 0);

                if (kk != j) {
                    goBack8 = true;

                    continue outer8;
                }

                np[j - 1] = -j;
            } while (j != nn);
        } while (goBack8);

        maxf = inc * maxf;

        // REORDER A AND B, FOLLOWING THE PERMUTATION CYCLES

        seg510to570 = false;

        do { // while (nt >= 0)

            if (seg510to570) {

                do { // while (j != 1)

                    do { // while (np[j-1] < 0)
                        j = j - 1;
                    } while (np[j - 1] < 0);

                    jj = jc;

                    do { // while (jj != 0)
                        kspan = jj;

                        if (jj > maxf) {
                            kspan = maxf;
                        }

                        jj = jj - kspan;
                        k = np[j - 1];
                        kk = (jc * k) + i + jj;
                        k1 = kk + kspan;
                        k2 = 0;

                        do { // while (k1 != kk)
                            k2 = k2 + 1;
                            at[k2 - 1] = a[k1 - 1];
                            bt[k2 - 1] = b[k1 - 1];
                            k1 = k1 - inc;
                        } while (k1 != kk);

                        do { // while (k != j)
                            k1 = kk + kspan;
                            k2 = k1 - (jc * (k + np[k - 1]));
                            k = -np[k - 1];

                            do { // while (k1 != kk)
                                a[k1 - 1] = a[k2 - 1];
                                b[k1 - 1] = b[k2 - 1];
                                k1 = k1 - inc;
                                k2 = k2 - inc;
                            } while (k1 != kk);

                            kk = k2;
                        } while (k != j);

                        k1 = kk + kspan;
                        k2 = 0;

                        do { // while (k1 != kk)
                            k2 = k2 + 1;
                            a[k1 - 1] = at[k2 - 1];
                            b[k1 - 1] = bt[k2 - 1];
                            k1 = k1 - inc;
                        } while (k1 != kk);
                    } while (jj != 0);
                } while (j != 1);
            } // if (seg510to570)

            seg510to570 = true;
            j = k3 + 1;
            nt = nt - kspnn;
            i = nt - inc + 1;
        } while (nt >= 0);

        setCompleted(true);

        return;

    }

    /**
     * DOCUMENT ME!
     */
    private void reals() {
        // Used with 'FFT' to compute fourier transform or inverse for real data

        // If isn = -1, this subroutine completes the Fourier transform
        // of 2*n real data values, where the original data values are
        // stored alternately in arrays a and b, and are first
        // transformed by a complex Fourier transform of dimension n.
        // The cosine coefficients are in a[0],a[1],...a[n-1],a[n]
        // and the sine coefficients are in b[0],b[1],...b[n-1],b[n].
        // Note that the arrays a and b must have dimension n+1.
        // A typical calling sequence is
        // FFTUtility fft = new FFTUtility(a,b,n,n,n,-1,FFTUtility.FFT);
        // fft.run();
        // fft.setFunctionType(FFTUtility.REALS);
        // fft.run();

        // If isn = 1, the inverse transformation is done, the first
        // step in evaluating a real Fourier series.
        // A typical calling sequence is
        // FFTUtility fft = new FFTUtility(a,b,n,n,n,1,FFTUtility.REALS);
        // fft.run();
        // fft.setFunctionType(FFTUtility.FFT);
        // fft.run();

        // The time domain results alternate in arrays a and b,
        // i.e. a[0],b[0],a[1],b[1],...a[n-1],b[n-1].

        int inc;
        int nf;
        int nk;
        int nh;
        double rad;
        double dr;
        double cd;
        double sd;
        double sindrrad;
        int lim;
        int mm;
        int ml;
        double sn;
        double cn;
        int k;
        double aa;
        double ab;
        double ba;
        double bb;
        double re;
        double em;
        int j;

        fireProgressStateChanged("FFT", "Performing FFT REALS...");

        inc = Math.abs(isn);
        nf = Math.abs(n);

        if ( (nf * isn) == 0) {
            MipavUtil.displayError("Error! Zero in REALS parameters");
            Preferences.debug("n = " + n + " isn = " + isn + "\n", Preferences.DEBUG_ALGORITHM);

            setCompleted(false);

            return;
        } // if ((nf*isn) == 0)

        nk = (nf * inc) + 2;
        nh = nk / 2;
        rad = Math.PI / 4.0;
        dr = -4.0 / (double) (nf);
        sindrrad = Math.sin(0.5 * dr * rad);
        cd = 2.0 * sindrrad * sindrrad;
        sd = Math.sin(dr * rad);

        // SIN,COS VALUES ARE RE-INITIALIZED EACH LIM STEPS

        lim = 32;
        mm = lim;
        ml = 0;
        sn = 0.0;

        if (isn > 0) {
            cn = -1.0;
            sd = -sd;
        } // if (isn > 0)
        else {
            cn = 1.0;
            a[nk - 2] = a[0];
            b[nk - 2] = b[0];
        } // else

        for (j = 0; j < nh; j += inc) {
            k = nk - j - 2;
            aa = a[j] + a[k];
            ab = a[j] - a[k];
            ba = b[j] + b[k];
            bb = b[j] - b[k];
            re = (cn * ba) + (sn * ab);
            em = (sn * ba) - (cn * ab);
            b[k] = (em - bb) * 0.5;
            b[j] = (em + bb) * 0.5;
            a[k] = (aa - re) * 0.5;
            a[j] = (aa + re) * 0.5;
            ml = ml + 1;

            if (ml == mm) {
                mm = mm + lim;
                sn = (double) (ml) * dr * rad;
                cn = Math.cos(sn);

                if (isn > 0) {
                    cn = -cn;
                }

                sn = Math.sin(sn);
            } // if (ml == mm)
            else {
                aa = cn - ( (cd * cn) + (sd * sn));
                sn = ( (sd * cn) - (cd * sn)) + sn;

                // THE FOLLOWING THREE STATEMENTS COMPENSATE FOR TRUNCATION
                // ERROR.IF ROUNDED ARITHMETIC IS USED, SUBSTITUTE cn = aa

                cn = (0.5 / ( (aa * aa) + (sn * sn))) + 0.5;
                sn = cn * sn;
                cn = cn * aa;
            } // else
        } // for (j = 0; j < nh; j += inc)

        setCompleted(true);

        return;
    }

    /**
     * DOCUMENT ME!
     */
    private void realt() {
        // Used with 'FFT' or any other complex Fourier transform to compute
        // transform or inverse for real data.
        // The data may be either single-variate or multi-variate.

        // If isn = -1, this subroutine completes the Fourier transform
        // of 2*n real data values, where the original data values are
        // stored alternately in arrays a and b, and are first
        // transformed by a complex Fourier transform of dimension n.
        // The cosine coefficients are in a[0],a[1],...a[n-1],a[n]
        // and the sine coefficients are in b[0],b[1],...b[n-1],b[n].
        // Note that the arrays a and b must have dimension n+1.
        // A typical calling sequence is
        // FFTUtility fft = new FFTUtility(a,b,1,n,1,-1,FFTUtility.FFT);
        // fft.run();
        // fft.setFunctionType(FFTUtility.REALT);
        // fft.run();

        // If isn = 1, the inverse transformation is done, the first
        // step in evaluating a real Fourier series.
        // A typical calling sequence is
        // FFTUtility fft = new FFTUtility(a,b,1,n,1,1,FFTUtility.REALT);
        // fft.run();
        // fft.setFunctionType(FFTUtility.FFT);
        // fft.run();
        // The time domain results alternate in arrays a and b,
        // i.e. a[0],b[0],a[1],b[1],...a[n-1],b[n-1].

        // This subroutine is set up to do the above-described operation on
        // all sub-vectors within any dimension of a multi-dimensional
        // Fourier transform. The parameters nseg, n, nspn, and inc
        // should agree with those used in the associated call of 'FFT'.
        // The folding frequency cosine coefficients are stored at the end
        // of array a (with zeros in corresponding locations in array b),
        // in a sub-matrix of dimension one less than the main array. The
        // deleted dimension is that corresponding to the parameter n in
        // the call of realt. Thus arrays a and b must have dimension
        // nseg*nspn*(n+1).

        int inc;
        int ks;
        int nf;
        int nt;
        int ns;
        int jc;
        int k2;
        int kd;
        int nh;
        int nn;
        int kk;
        double rad;
        double dr;
        double cd;
        double sindrrad;
        double sd;
        int lim;
        int klim;
        int mm;
        double sn;
        double aa;
        double ba;
        double cn;
        double ab;
        double bb;
        double re;
        double em;
        boolean goBack = false;

        fireProgressStateChanged("FFT", "Performing FFT REALT...");

        inc = Math.abs(isn);
        ks = Math.abs(nspn) * inc;
        nf = Math.abs(n);
        ns = ks * nf;
        nt = Math.abs(ns * nseg);

        if ( (isn * nt) == 0) {
            MipavUtil.displayError("Error! Zero in REALT parameters");
            Preferences.debug("nseg = " + nseg + " n = " + n + "\n", Preferences.DEBUG_ALGORITHM);
            Preferences.debug("nspn = " + nspn + " isn = " + isn + "\n", Preferences.DEBUG_ALGORITHM);

            setCompleted(false);

            return;
        } // if ((isn*nt) == 0)

        jc = ks;
        k2 = Math.abs(ks * nseg) - inc;
        kd = ns;
        nh = (ns / 2) + 1;
        nn = nt - inc;
        nt = nt + 1;
        kk = 1;
        rad = Math.PI / 4.0;
        dr = -4.0 / (double) (nf);
        sindrrad = Math.sin(0.5 * dr * rad);
        cd = 2.0 * sindrrad * sindrrad;
        sd = Math.sin(dr * rad);

        // SIN,COS VALUES ARE RE-INITIALIZED EACH LIM STEPS

        lim = 32;
        klim = lim * ks;
        mm = Math.min(nh, klim);
        sn = 0.0;

        if (isn > 0) {

            do { // while (kk <= jc)

                do { // while (kk <= nn)
                    aa = a[kk - 1];
                    ba = a[nt - 1];
                    a[kk - 1] = (aa + ba) * 0.5;
                    b[kk - 1] = (aa - ba) * 0.5;
                    nt = nt + jc;
                    kk = kk + ns;
                } while (kk <= nn);

                nt = nt - k2;
                kk = kk - nn;
            } while (kk <= jc);

            cn = -1.0;
            sd = -sd;
        } // if (isn > 0)
        else { // isn <= 0

            do { // while (kk <= jc)

                do { // while (kk <= nn)
                    aa = a[kk - 1];
                    ba = b[kk - 1];
                    b[kk - 1] = 0;
                    a[kk - 1] = aa + ba;
                    a[nt - 1] = aa - ba;
                    b[nt - 1] = 0;
                    nt = nt + jc;
                    kk = kk + ns;
                } while (kk <= nn);

                nt = nt - k2;
                kk = kk - nn;
            } while (kk <= jc);

            cn = 1.0;
        } // else isn <= 0

        if (nf == 1) {

            setCompleted(true);

            return;
        }

        outer: do { // while (goBack)
            goBack = false;
            aa = cn - ( (cd * cn) + (sd * sn));
            sn = ( (sd * cn) - (cd * sn)) + sn;

            // THE FOLLOWING THREE STATEMENTS COMPENSATE FOR TRUNCATION
            // ERROR. IF ROUNDED ARITHMETIC IS USED, SUBSTITUTE cn = aa

            cn = (0.5 / ( (aa * aa) + (sn * sn))) + 0.5;
            sn = cn * sn;
            cn = cn * aa;

            do { // while (true)
                jc = jc + ks;
                kd = kd - ks - ks;

                do { // while (kk <= jc)

                    do { // while (kk <= nn)
                        k2 = kk + kd;
                        aa = a[kk - 1] + a[k2 - 1];
                        ab = a[kk - 1] - a[k2 - 1];
                        ba = b[kk - 1] + b[k2 - 1];
                        bb = b[kk - 1] - b[k2 - 1];
                        re = (cn * ba) + (sn * ab);
                        em = (sn * ba) - (cn * ab);
                        b[k2 - 1] = (em - bb) * 0.5;
                        b[kk - 1] = (em + bb) * 0.5;
                        a[k2 - 1] = (aa - re) * 0.5;
                        a[kk - 1] = (aa + re) * 0.5;
                        kk = kk + ns;
                    } while (kk <= nn);

                    kk = kk - nn;
                } while (kk <= jc);

                if (kk <= mm) {
                    goBack = true;

                    continue outer;
                }

                if (kk > nh) {

                    setCompleted(true);

                    return;
                }

                sn = (double) (jc / ks) * dr * rad;
                cn = Math.cos(sn);

                if (isn > 0) {
                    cn = -cn;
                }

                sn = Math.sin(sn);
                mm = Math.min(nh, mm + klim);
            } while (true);
        } while (goBack);

    }

    /**
     * DOCUMENT ME!
     */
    private void selfTest() {
        a = new double[4097];
        b = new double[4097];

        double[] c = new double[4097];
        double[] d = new double[4097];
        int[] nc = new int[] {2, 3, 5, 210, 1000, 2000, 1024, 2048, 4096, 2187, 3125, 2401, 1331, 2197, 289, 361, 529};
        double[] ra = new double[64];
        double[] rb = new double[64];
        RandomNumberGen randomGen;
        int j;
        double[] output;
        int i;
        double ss1;
        double ss2;
        int n1, n2, n3;

        n = 16;
        randomGen = new RandomNumberGen();

        for (j = 0; j < n; j++) {
            c[j] = randomGen.genUniformRandomNum(0.0f, 1.0f);
            a[j] = c[j];
            d[j] = randomGen.genUniformRandomNum(0.0f, 1.0f);
            b[j] = d[j];
        } // for (j = 0; j < n; j++)

        Preferences.debug("Test of FFT, REALS, and REALT for real values\n", Preferences.DEBUG_ALGORITHM);
        Preferences.debug("Real input sequence\n", Preferences.DEBUG_ALGORITHM);

        for (j = 0; j < n; j++) {
            Preferences.debug("a[" + j + "] = " + a[j] + " b[" + j + "] = " + b[j] + "\n", Preferences.DEBUG_ALGORITHM);
        } // for (j = 0; j < n; j++)

        nseg = 1;
        nspn = 1;
        isn = -1;
        fft();
        reals();
        Preferences.debug("Fourier cosine and sine coefficients\n", Preferences.DEBUG_ALGORITHM);

        for (j = 0; j <= n; j++) {
            Preferences.debug("a[" + j + "] = " + a[j] + " b[" + j + "] = " + b[j] + "\n", Preferences.DEBUG_ALGORITHM);
        } // for (j = 0; j <= n; j++)

        // The next call on realt does the same thing as reals(a,b,n,1);

        isn = 1;
        realt();
        fft();
        Preferences.debug("Transform-inverse result\n", Preferences.DEBUG_ALGORITHM);

        for (j = 0; j < n; j++) {
            Preferences.debug("a[" + j + "] = " + a[j] + " b[" + j + "] = " + b[j] + "\n", Preferences.DEBUG_ALGORITHM);
        } // for (j = 0; j < n; j++)

        for (j = 0; j < n; j++) {
            a[j] = a[j] - c[j];
            b[j] = b[j] - d[j];
        } // for (j = 0; j < n; j++)

        sortg(a, n, ra);
        sortg(b, n, rb);
        Preferences.debug("Sorted error values\n", Preferences.DEBUG_ALGORITHM);

        for (j = 0; j < n; j++) {
            Preferences.debug("a[" + j + "] = " + a[j] + " b[" + j + "] = " + b[j] + "\n", Preferences.DEBUG_ALGORITHM);
        } // for (j = 0; j < n; j++)

        for (j = 0; j < n; j++) {
            a[2 * j] = c[j];
            a[ (2 * j) + 1] = d[j];
            b[2 * j] = 0.0;
            b[ (2 * j) + 1] = 0.0;
        } // for (j = 0; j < n; j++)

        Preferences.debug("Test of FFT for real values\n", Preferences.DEBUG_ALGORITHM);

        n = n + n;
        isn = -1;
        fft();
        Preferences.debug("Fourier cosine and sine coefficients\n", Preferences.DEBUG_ALGORITHM);

        for (j = 0; j < n; j++) {
            Preferences.debug("a[" + j + "] = " + a[j] + " b[" + j + "] = " + b[j] + "\n", Preferences.DEBUG_ALGORITHM);
        } // for (j = 0; j < n; j++)

        isn = 1;
        fft();
        Preferences.debug("Transform-inverse result\n", Preferences.DEBUG_ALGORITHM);

        for (j = 0; j < n; j++) {
            Preferences.debug("a[" + j + "] = " + a[j] + " b[" + j + "] = " + b[j] + "\n", Preferences.DEBUG_ALGORITHM);
        } // for (j = 0; j < n; j++)

        // Generate test data for transform of maximum size 4096
        for (j = 0; j < 4096; j++) {
            output = normal();
            c[j] = output[0];
            d[j] = output[1];
        } // for (j = 0; j < 4096; j++)

        Preferences.debug("Single-variate tests of subroutines\n", Preferences.DEBUG_ALGORITHM);

        for (i = 0; i <= 16; i++) {
            n = nc[i];

            for (j = 0; j < n; j++) {
                a[j] = c[j];
                b[j] = d[j];
            } // for (j = 0; j < n; j++)

            isn = -1;
            fft();
            isn = 1;
            fft();
            output = rms(a, b, c, d, n);
            ss1 = output[0];
            ss2 = output[1];
            Preferences.debug("FFT n = " + n + " ss1 = " + ss1 + " ss2 = " + ss2 + "\n", Preferences.DEBUG_ALGORITHM);

            for (j = 0; j < n; j++) {
                a[j] = c[j];
                b[j] = d[j];
            } // for (j = 0; j < n; j++)

            isn = -1;
            reals();
            isn = 1;
            reals();
            output = rms(a, b, c, d, n);
            ss1 = output[0];
            ss2 = output[1];
            Preferences.debug("REALS n = " + n + " ss1 = " + ss1 + " ss2 = " + ss2 + "\n", Preferences.DEBUG_ALGORITHM);

            for (j = 0; j < n; j++) {
                a[j] = c[j];
                b[j] = d[j];
            } // for (j = 0; j < n; j++)

            isn = -1;
            realt();
            isn = 1;
            realt();
            output = rms(a, b, c, d, n);
            ss1 = output[0];
            ss2 = output[1];
            Preferences.debug("REALT n = " + n + " ss1 = " + ss1 + " ss2 = " + ss2 + "\n", Preferences.DEBUG_ALGORITHM);

        } // for (i = 0; i <= 16; i++)

        Preferences.debug("Multi-variate tests of subroutines\n", Preferences.DEBUG_ALGORITHM);
        n1 = 4;
        n2 = 30;
        n3 = 9;
        n = n1 * n2 * n3;

        for (j = 0; j < n; j++) {
            a[j] = c[j];
            b[j] = d[j];
        } // for (j = 0; j < n; j++)

        isn = -1;
        nseg = n1 * n2;
        n = n3;
        nspn = 1;
        fft();
        nseg = n1;
        n = n2;
        nspn = n3;
        fft();
        nseg = 1;
        n = n1;
        nspn = n2 * n3;
        fft();
        isn = 1;
        nseg = 1;
        n = n1;
        nspn = n2 * n3;
        fft();
        nseg = n1;
        n = n2;
        nspn = n3;
        fft();
        nseg = n1 * n2;
        n = n3;
        nspn = 1;
        fft();
        n = n1 * n2 * n3;
        output = rms(a, b, c, d, n);
        ss1 = output[0];
        ss2 = output[1];
        Preferences.debug("FFT n = " + n + " ss1 = " + ss1 + " ss2 = " + ss2 + "\n", Preferences.DEBUG_ALGORITHM);

        for (j = 0; j < n; j++) {
            a[j] = c[j];
            b[j] = d[j];
        } // for (j = 0; j < n; j++)

        nseg = n1;
        n = n2;
        nspn = n3;
        isn = -1;
        realt();
        isn = 1;
        realt();
        n = n1 * n2 * n3;
        output = rms(a, b, c, d, n);
        ss1 = output[0];
        ss2 = output[1];
        Preferences.debug("REALT n = " + n + " ss1 = " + ss1 + " ss2 = " + ss2 + "\n", Preferences.DEBUG_ALGORITHM);
    }

    /**
     * DOCUMENT ME!
     * 
     * @param a DOCUMENT ME!
     * @param n DOCUMENT ME!
     * @param tag DOCUMENT ME!
     */
    public void sortg(double[] a, int n, double[] tag) {
        // Sorts array a into increasing order, from a[0] TO a[n-1]
        // The array tag is permuted the same as array a

        // TO SORT N ELEMENTS STARTING WITH A(K), CALL WITH A(K) AND TAG(K).
        // AN EARLIER VERSION OF THIS ALGORITHM, WITHOUT THE TAG ARRAY, WAS
        // PUBLISHED BY R.C. SINGLETON AS ACM ALGORITHM 347,
        // COMM. ACM 12 (MARCH 1969), 1865-1866. THE CURRENT VERSION
        // SOLVES A MACHINE-DEPENDENT PROBLEM PRESENT IN THE EARLIER
        // VERSION AND ALMOST ALL OTHER SORT SUBROUTINES. ON MANY
        // COMPUTERS, COMPARING A VERY LARGE NEGATIVE NUMBER WITH A
        // VERY LARGE POSITIVE NUMBER GIVES A WRONG RESULT AND A BAD SORT.
        // THIS PROBLEM WAS NOTED BY R. GRIFFIN AND K.A. REDISH, "REMARK
        // ON ALGORITHM 347,", COMM. ACM 13 (JANUARY 1970), 54.
        // THE PROBLEM IS AVOIDED HERE BY AN INITIAL SPLIT ON ZERO.
        // TIME IS PROPORTIONAL TO N*LOG(N)
        // AS FAR AS THE AUTHOR IS AWARE, NO FASTER IN-PLACE SORT METHOD HAS
        // BEEN PUBLISHED SINCE THE ORIGINAL APPEARANCE OF THIS ALGORITHM.

        // WORKING STORAGE ARRAYS IL AND IU SHOULD HAVE DIMENSION
        // INT(ALOG(FLOAT(N))/ALOG(2.0))
        // A DIMENSION OF 20 ALLOWS VALUES OF N UP TO 2**21-1
        int[] iu = new int[20];
        int[] il = new int[20];
        int m;
        int i;
        int j;
        int k;
        int l;
        double t;
        int ij = 0;
        double tg = 0.0;
        double tt = 0.0;
        boolean seg10 = true;
        boolean seg20 = true;
        boolean seg30 = true;
        boolean seg40 = true;
        boolean seg50 = true;
        boolean seg60 = true;
        boolean seg70 = true;
        boolean seg80 = true;
        boolean seg90 = true;
        boolean seg100 = true;
        boolean seg110 = true;
        boolean seg120 = true;
        boolean seg130 = true;
        boolean seg140 = true;
        boolean seg150 = true;

        m = 1;
        i = 1;
        j = n;
        k = i;
        l = j;

        if (i >= j) {
            return;
        }

        t = 0.0;

        if (a[i - 1] <= 0) {
            seg10 = false;
            seg20 = false;
        }

        outer: do { // while (true)

            if (seg10) {

                if (a[l - 1] <= 0) {
                    seg20 = false;
                    seg30 = false;
                    seg40 = false;
                    seg50 = false;
                    seg60 = false;
                    seg70 = false;
                    seg80 = false;
                }
            } // if (seg10)

            seg10 = true;

            if (seg20) {
                l = l - 1;

                if ( (l - i) <= 0) {
                    seg30 = false;
                    seg40 = false;
                    seg50 = false;
                    seg60 = false;
                } else {
                    continue;
                }
            } // if (seg20)

            seg20 = true;

            if (seg30) {

                if (a[j - 1] >= 0) {
                    seg40 = false;
                    seg50 = false;
                    seg60 = false;
                    seg70 = false;
                    seg80 = false;
                    seg90 = false;
                    seg100 = false;
                }
            } // if (seg30)

            seg30 = true;

            if (seg40) {

                if (a[k - 1] >= 0) {
                    seg50 = false;
                    seg60 = false;
                    seg70 = false;
                    seg80 = false;
                }
            } // if (seg40)

            seg40 = true;

            if (seg50) {
                k = k + 1;

                if ( (j - k) <= 0) {
                    seg60 = false;
                } else {
                    seg10 = false;
                    seg20 = false;
                    seg30 = false;

                    continue;
                }
            } // if (seg50)

            seg50 = true;

            if (seg60) {

                if (i >= j) {
                    seg70 = false;
                    seg80 = false;
                    seg90 = false;
                    seg100 = false;
                    seg110 = false;
                    seg120 = false;
                    seg130 = false;
                }
            } // if (seg60)

            seg60 = true;

            if (seg70) {
                k = i;
                ij = (j + i) / 2;
                t = a[ij - 1];

                if (a[i - 1] > t) {
                    a[ij - 1] = a[i - 1];
                    a[i - 1] = t;
                    t = a[ij - 1];
                    tg = tag[ij - 1];
                    tag[ij - 1] = tag[i - 1];
                    tag[i - 1] = tg;
                } // if (a[i-1] > t)
            } // if (seg70)

            seg70 = true;

            if (seg80) {
                seg90 = false;
                seg100 = false;
                l = j;

                if (a[j - 1] < t) {
                    a[ij - 1] = a[j - 1];
                    a[j - 1] = t;
                    t = a[ij - 1];
                    tg = tag[ij - 1];
                    tag[ij - 1] = tag[j - 1];
                    tag[j - 1] = tg;

                    if (a[i - 1] > t) {
                        a[ij - 1] = a[i - 1];
                        a[i - 1] = t;
                        t = a[ij - 1];
                        tg = tag[ij - 1];
                        tag[ij - 1] = tag[i - 1];
                        tag[i - 1] = tg;
                    } // if (a[i-1] > t)
                } // if (a[j-1] < t)
            } // if (seg80)

            seg80 = true;

            if (seg90) {
                tt = a[l - 1];
            } // if (seg90)

            seg90 = true;

            if (seg100) {
                a[l - 1] = a[k - 1];
                a[k - 1] = tt;
                tg = tag[l - 1];
                tag[l - 1] = tag[k - 1];
                tag[k - 1] = tg;
            } // if (seg100)

            seg100 = true;

            if (seg110) {

                do { // while (a[l-1] > t)
                    l = l - 1;
                } while (a[l - 1] > t);

                tt = a[l - 1];
            } // if (seg110)

            seg110 = true;

            if (seg120) {

                do { // while (a[k-1] < t)
                    k = k + 1;
                } while (a[k - 1] < t);

                if (k <= l) {
                    seg10 = false;
                    seg20 = false;
                    seg30 = false;
                    seg40 = false;
                    seg50 = false;
                    seg60 = false;
                    seg70 = false;
                    seg80 = false;
                    seg90 = false;

                    continue;
                }

                if ( (l - i) > (j - k)) {
                    il[m - 1] = i;
                    iu[m - 1] = l;
                    i = k;
                    m = m + 1;
                    seg130 = false;
                    seg140 = false;
                } // if ((l-i) > (j-k))
            } // if (seg120)

            seg120 = true;

            if (seg130) {
                il[m - 1] = k;
                iu[m - 1] = j;
                j = l;
                m = m + 1;
                seg140 = false;
            } // if (seg130)

            seg130 = true;

            if (seg140) {
                m = m - 1;

                if (m == 0) {
                    return;
                }

                i = il[m - 1];
                j = iu[m - 1];
            } // if (seg140)

            seg140 = true;

            if (seg150) {

                if ( (j - i) > 10) {
                    seg10 = false;
                    seg20 = false;
                    seg30 = false;
                    seg40 = false;
                    seg50 = false;
                    seg60 = false;

                    continue;
                } // if ((j-i) > 10)

                if (i == 1) {
                    seg10 = false;
                    seg20 = false;
                    seg30 = false;
                    seg40 = false;
                    seg50 = false;

                    continue;
                } // if (i == 1)

                i = i - 1;
            } // if (seg150)

            seg150 = true;

            do { // while (a[i-1] <= t)
                i = i + 1;

                if (i == j) {
                    seg10 = false;
                    seg20 = false;
                    seg30 = false;
                    seg40 = false;
                    seg50 = false;
                    seg60 = false;
                    seg70 = false;
                    seg80 = false;
                    seg90 = false;
                    seg100 = false;
                    seg110 = false;
                    seg120 = false;
                    seg130 = false;

                    continue outer;
                }

                t = a[i];
            } while (a[i - 1] <= t);

            tg = tag[i];
            k = i;

            do { // while (t < a[k-1])
                a[k] = a[k - 1];
                tag[k] = tag[k - 1];
                k = k - 1;
            } while (t < a[k - 1]);

            a[k] = t;
            tag[k] = tg;
            seg10 = false;
            seg20 = false;
            seg30 = false;
            seg40 = false;
            seg50 = false;
            seg60 = false;
            seg70 = false;
            seg80 = false;
            seg90 = false;
            seg100 = false;
            seg110 = false;
            seg120 = false;
            seg130 = false;
            seg140 = false;
            seg150 = false;
        } while (true);

    }

}
