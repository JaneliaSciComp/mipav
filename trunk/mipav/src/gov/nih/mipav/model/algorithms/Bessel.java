package gov.nih.mipav.model.algorithms;


import gov.nih.mipav.view.*;


/**
 * <p>
 * This module computes Bessel functions of complex arguments and a nonnegative order. This work is the port of FORTRAN
 * code written by Donald E. Amos at the Sandia National Laboratories. This is an installation of his Algorithm 644,
 * Collected Algorithms form ACM. The original FORTRAN code was downloaded from http://www.netlib.org/toms/644. The
 * decision to port this code was prompted by the fact that the MATLAB links to this Amos FORTRAN package. For
 * doublechecking this code 2 possibilities exist: 1.) The 6 self tests included in the package - zqcbh, zqcbi, zqcbj,
 * zqcbk, zqcby, and zqcai. The code has passed all 6 self tests. 2.) Bessel function tables for real arguments as given
 * in Handbook of Mathematical Functions with Formulas, Graphs, and Mathematical Tables Edited by Milton Abramowitz and
 * Irene A. Stegun. Some values for Bessel functions of complex arguments can be found in Computation of Special
 * Functions by Shanjie Zhang and Jianming Jin, copyright 1966 by John Wiley & Sons, Inc. The I and K Bessel functions
 * obtained in this code were found to match the values in these 2 sources. Note that the Zhang and Jin book also
 * contains code for Bessel functions of complex arguments.
 * </p>
 * 
 * <p>
 * REFERENCES:
 * <ol>
 * <li>Abramowitz, M. and Stegun, I. A., Handbook of Mathematical Functions, NBS Applied Math Series 55, U.S. Dept. of
 * Commerce, Washington, D.C., 1955</li>
 * <li>Amos, D. E., Algorithm 644, A Portable Package For Bessel Functions of a Complex Argument and Nonnegative Order,
 * ACM Transactions on Mathematical Software, Vol. 12, No. 3, September 1986, Pages 265-273</li>
 * <li>Amos, D. E., Remark on Algorithm 644, ACM Transactions on Mathematical Software, Vol. 16, No. 4, December 1990,
 * Page 404</li>
 * <li>4. Amos, D. E., Remark on Algorithm 644 (Improvements in Algorithm 644), ACM Transactions on Mathematical
 * Software, Vol. 21, No. 4, December 1995, Pages 388-393.</li>
 * <li>5. Cody, W. J., Algorithm 665, MACHAR: A Subroutine to Dynamically Determine Machine Parameters, ACM
 * Transactions on Mathematical Software, Vol. 14, No. 4, December 1988, Pages 303-311</li>
 * </ol>
 * </p>
 * 
 * <hr>
 * 
 * <p>
 * From <a href="http://www.acm.org/publications/policies/softwarecrnotice/">the ACM website</a>:
 * 
 * <pre>
 * ACM Software License Agreement
 * 
 *  All software, both binary and source published by the Association for Computing Machinery (hereafter, Software) is copyrighted by the Association (hereafter, ACM) and ownership of all right, title and interest in and to the Software remains with ACM. By using or copying the Software, User agrees to abide by the terms of this Agreement.
 *  Noncommercial Use
 * 
 *  The ACM grants to you (hereafter, User) a royalty-free, nonexclusive right to execute, copy, modify and distribute both the binary and source code solely for academic, research and other similar noncommercial uses, subject to the following conditions:
 * 
 *  1. User acknowledges that the Software is still in the development stage and that it is being supplied &quot;as is,&quot; without any support services from ACM. Neither ACM nor the author makes any representations or warranties, express or implied, including, without limitation, any representations or warranties of the merchantability or fitness for any particular purpose, or that the application of the software, will not infringe on any patents or other proprietary rights of others.
 *  2. ACM shall not be held liable for direct, indirect, incidental or consequential damages arising from any claim by User or any third party with respect to uses allowed under this Agreement, or from any use of the Software.
 *  3. User agrees to fully indemnify and hold harmless ACM and/or the author(s) of the original work from and against any and all claims, demands, suits, losses, damages, costs and expenses arising out of the User's use of the Software, including, without limitation, arising out of the User's modification of the Software.
 *  4. User may modify the Software and distribute that modified work to third parties provided that: (a) if posted separately, it clearly acknowledges that it contains material copyrighted by ACM (b) no charge is associated with such copies, (c) User agrees to notify ACM and the Author(s) of the distribution, and (d) User clearly notifies secondary users that such modified work is not the original Software.
 *  5. User agrees that ACM, the authors of the original work and others may enjoy a royalty-free, non-exclusive license to use, copy, modify and redistribute these modifications to the Software made by the User and distributed to third parties as a derivative work under this agreement.
 *  6. This agreement will terminate immediately upon User's breach of, or non-compliance with, any of its terms. User may be held liable for any copyright infringement or the infringement of any other proprietary rights in the Software that is caused or facilitated by the User's failure to abide by the terms of this agreement.
 *  7. This agreement will be construed and enforced in accordance with the law of the state of New York applicable to contracts performed entirely within the State. The parties irrevocably consent to the exclusive jurisdiction of the state or federal courts located in the City of New York for all disputes concerning this agreement. 
 * 
 *  Commercial Use
 * 
 *  Any User wishing to make a commercial use of the Software must contact ACM at permissions@acm.org to arrange an appropriate license. Commercial use includes (1) integrating or incorporating all or part of the source code into a product for sale or license by, or on behalf of, User to third parties, or (2) distribution of the binary or source code to third parties for use with a commercial product sold or licensed by, or on behalf of, User.
 * 
 *  Revised 6/98
 * </pre>
 * 
 * </p>
 */
public class Bessel {

    // ~ Static fields/initializers
    // -------------------------------------------------------------------------------------

    /** Bessel functions of the third kind, Hankel functions. */
    public static final int BESSEL_H = 1;

    /** modified Bessel function of the first kind. */
    public static final int BESSEL_I = 2;

    /** Bessel function of the first kind. */
    public static final int BESSEL_J = 3;

    /** modified Bessel function of the second kind. */
    public static final int BESSEL_K = 4;

    /** Bessel function of the second kind. */
    public static final int BESSEL_Y = 5;

    /** Airy function Ai. */
    public static final int AIRY_AI = 6;

    /** Airy function Bi. */
    public static final int AIRY_BI = 7;

    /** DOCUMENT ME! */
    public static final int UNSCALED_FUNCTION = 1;

    /** multiplied by exp(-abs(Real(z))). */
    public static final int SCALED_FUNCTION = 2;

    // ~ Instance fields
    // ------------------------------------------------------------------------------------------------

    /** DOCUMENT ME! */
    private double alim;

    /** DOCUMENT ME! */
    private final int BesselType;

    /** DOCUMENT ME! */
    private double[] cyi;

    /** DOCUMENT ME! */
    private double[] cyr;

    /** order of derivative, 0 or 1. */
    private int derivativeOrder;

    /** DOCUMENT ME! */
    private boolean doTest = false;

    /** I1MACH(14) = 53. */
    private int doubleDigits = 53;

    /** DOCUMENT ME! */
    private double elim;

    /** I1MACH(16) = 1024 The largest exponent for double precision. */
    private int emax = 1024;

    /** I1MACH(15) = -1021; The smallest exponent for double precision. */
    private int emin = -1021;

    /** D1MACH(4). */
    private double epsilon;

    /**
     * errorFlag = 0, normal return - computation completed errorFlag = 1, input error - no computation errorFlag = 2,
     * overflow - no computation, Real(z) large on UNSCALED_FUNCTION errorFlag = 3, ABS(Z) or initialOrder +
     * sequenceNumber - 1 large, computation but losses of significance by arguemnt reduction produce less than half of
     * machine accuracy errorFlag = 4, ABS(Z) or initialOrder + sequenceNumber - 1 too large, No computation because of
     * complete losses of significance by argument reduction errorFlag = 5, Error, no computation, argument termination
     * condition not met Length of array passed in must be 1.
     */
    private int[] errorFlag;

    /** DOCUMENT ME! */
    private double fnul;

    /**
     * real and imaginary outputs Length of arrays passed in must be sequenceNumber Kind of Kankel function = 1 or 2.
     */
    private int HankelKind;

    /** order of initial function. */
    private double initialOrder;

    /**
     * Number of components set to zero due to underflow nz[0] = 0, normal return nz[0] > 0, Last nz components of cy
     * set to zero due to underflow, cy[j] = 0.0 + i*0.0 for j = sequenceNumber - nz, ..., sequenceNumber-1 Length of
     * array passed in must be 1.
     */
    private int[] nz;

    /** DOCUMENT ME! */
    private boolean overflowTest = false;

    /** D1MACH(5) = log10(2). */
    private double r1m5 = 0.434294481903251 * Math.log(2.0);

    /** DOCUMENT ME! */
    private double rl;

    /**
     * UNSCALED_FUNCTION returns cy[j-1] = besselFunction(initialOrder+j-1,z), j = 1,...,sequenceNumber SCALED_FUNCTION
     * returns cy[j-1] = besselFunction(initialOrder+j-1,z)*exp(-abs(Real(z))), j = 1,...,sequenceNumber.
     */
    private int scalingOption;

    /** number of members of the sequence. */
    private int sequenceNumber;

    /** 2**-1022 = D1MACH(1). */
    private double tiny = Math.pow(2, -1022);

    /** DOCUMENT ME! */
    private double tol;

    /** DOCUMENT ME! */
    private double zi;

    /** zr and zi are the real and imaginary parts of the complex argument. */
    private double zr;

    // ~ Constructors
    // ---------------------------------------------------------------------------------------------------

    /**
     * Creates a new Bessel object.
     * 
     * @param BesselType DOCUMENT ME!
     * @param overflowTest DOCUMENT ME!
     */
    public Bessel(final int BesselType, final boolean overflowTest) {
        doTest = true;
        this.BesselType = BesselType;
        this.overflowTest = overflowTest;
    }

    /**
     * This constructor is used for Ai and Bi Airy functions.
     * 
     * @param BesselType int
     * @param zr double
     * @param zi double
     * @param derivativeOrder int
     * @param scalingOption int
     * @param cyr double[]
     * @param cyi double[]
     * @param nz int[]
     * @param errorFlag int[]
     */
    public Bessel(final int BesselType, final double zr, final double zi, final int derivativeOrder,
            final int scalingOption, final double[] cyr, final double[] cyi, final int[] nz, final int[] errorFlag) {
        this.BesselType = BesselType;
        this.zr = zr;
        this.zi = zi;
        this.derivativeOrder = derivativeOrder;
        this.scalingOption = scalingOption;
        this.cyr = cyr;
        this.cyi = cyi;
        this.nz = nz;
        this.errorFlag = errorFlag;
        sequenceNumber = 1;
    }

    /**
     * This construtor used for I, J, K, and Y Bessel functions.
     * 
     * @param BesselType int
     * @param zr double
     * @param zi double
     * @param initialOrder double
     * @param scalingOption int
     * @param sequenceNumber int
     * @param cyr double[]
     * @param cyi double[]
     * @param nz int[]
     * @param errorFlag int[]
     */
    public Bessel(final int BesselType, final double zr, final double zi, final double initialOrder,
            final int scalingOption, final int sequenceNumber, final double[] cyr, final double[] cyi, final int[] nz,
            final int[] errorFlag) {
        this.BesselType = BesselType;
        this.zr = zr;
        this.zi = zi;
        this.initialOrder = initialOrder;
        this.scalingOption = scalingOption;
        this.sequenceNumber = sequenceNumber;
        this.cyr = cyr;
        this.cyi = cyi;
        this.nz = nz;
        this.errorFlag = errorFlag;
    }

    /**
     * This constructor used for H functions of kind 1 and 2.
     * 
     * @param BesselType int
     * @param zr double
     * @param zi double
     * @param initialOrder double
     * @param scalingOption int
     * @param HankelKind int
     * @param sequenceNumber int
     * @param cyr double[]
     * @param cyi double[]
     * @param nz int[]
     * @param errorFlag int[]
     */
    public Bessel(final int BesselType, final double zr, final double zi, final double initialOrder,
            final int scalingOption, final int HankelKind, final int sequenceNumber, final double[] cyr,
            final double[] cyi, final int[] nz, final int[] errorFlag) {
        this.BesselType = BesselType;
        this.zr = zr;
        this.zi = zi;
        this.initialOrder = initialOrder;
        this.scalingOption = scalingOption;
        this.HankelKind = HankelKind;
        this.sequenceNumber = sequenceNumber;
        this.cyr = cyr;
        this.cyi = cyi;
        this.nz = nz;
        this.errorFlag = errorFlag;
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

    /**
     * DOCUMENT ME!
     */
    public void run() {
        double[] cwrkr;
        double[] cwrki;

        if (doTest) {
            runTest();

            return;
        }

        if (errorFlag == null) {
            MipavUtil.displayError("Array errorFlag must not be null");

            return;
        }

        if ( (BesselType != Bessel.BESSEL_H) && (BesselType != Bessel.BESSEL_I) && (BesselType != Bessel.BESSEL_J)
                && (BesselType != Bessel.BESSEL_K) && (BesselType != Bessel.BESSEL_Y) && (BesselType != Bessel.AIRY_AI)
                && (BesselType != Bessel.AIRY_BI)) {
            MipavUtil
                    .displayError("Bessel type must be Airy Ai, Airy BI, BESSEL_H, BESSEL_I, BESSEL_J, BESSEL_K, or BESSEL_Y");
            errorFlag[0] = 1;

            return;
        }

        if ( (BesselType != Bessel.AIRY_AI) && (BesselType != Bessel.AIRY_BI)) {

            if (initialOrder < 0) {
                MipavUtil.displayError("initialOrder must be >= 0.0");
                errorFlag[0] = 1;

                return;
            }
        }

        if ( (BesselType == Bessel.AIRY_AI) || (BesselType == Bessel.AIRY_BI)) {

            if ( (derivativeOrder < 0) || (derivativeOrder > 1)) {
                MipavUtil.displayError("Derivative order can only be 0 or 1");
                errorFlag[0] = 1;

                return;
            }
        }

        if ( (scalingOption != Bessel.UNSCALED_FUNCTION) && (scalingOption != Bessel.SCALED_FUNCTION)) {
            MipavUtil.displayError("scalingOption must be UNSCALED_FUNCTION or " + "SCALED_FUNCTION");
            errorFlag[0] = 1;

            return;
        }

        if (sequenceNumber < 1) {
            MipavUtil.displayError("sequence number must be >= 1");
            errorFlag[0] = 1;

            return;
        }

        if (cyr == null) {
            MipavUtil.displayError("Array cyr must not be null");
            errorFlag[0] = 1;

            return;
        }

        if (cyr.length != sequenceNumber) {
            MipavUtil.displayError("Array cyr must be of length " + sequenceNumber);
            errorFlag[0] = 1;

            return;
        }

        if (cyi == null) {
            MipavUtil.displayError("Array cyi must not be null");
            errorFlag[0] = 1;

            return;
        }

        if (cyi.length != sequenceNumber) {
            MipavUtil.displayError("Array cyi must be of length " + sequenceNumber);
            errorFlag[0] = 1;

            return;
        }

        if (BesselType != Bessel.AIRY_BI) {

            if (nz == null) {
                MipavUtil.displayError("Array nz must not be null");
                errorFlag[0] = 1;

                return;
            }
        }

        errorFlag[0] = 0;

        if (BesselType != Bessel.AIRY_BI) {
            nz[0] = 0;
        }

        switch (BesselType) {

            case BESSEL_H:
                if ( (zr == 0.0) && (zi == 0.0)) {
                    MipavUtil.displayError("BesselH must not have both zr and zi = 0.0");
                    errorFlag[0] = 1;

                    return;
                } // if ((zr == 0.00 && (zi == 0.0))

                if ( (HankelKind < 1) || (HankelKind > 2)) {
                    MipavUtil.displayError("BesselH must not have Hankel function kind"
                            + " less than 1 or greater than 2");
                    errorFlag[0] = 1;

                    return;
                } // if ((HankelKind < 1) || (HankelKind > 2))

                zbesh(zr, zi, initialOrder, scalingOption, HankelKind, sequenceNumber, cyr, cyi, nz, errorFlag);
                break;

            case BESSEL_I:
                zbesi(zr, zi, initialOrder, scalingOption, sequenceNumber, cyr, cyi, nz, errorFlag);
                break;

            case BESSEL_J:
                zbesj(zr, zi, initialOrder, scalingOption, sequenceNumber, cyr, cyi, nz, errorFlag);
                break;

            case BESSEL_K:
                if ( (zr == 0.0) && (zi == 0.0)) {
                    MipavUtil.displayError("BesselK must not have both zr and zi = 0.0");
                    errorFlag[0] = 1;

                    return;
                } // if ((zr == 0.00 && (zi == 0.0))

                zbesk(zr, zi, initialOrder, scalingOption, sequenceNumber, cyr, cyi, nz, errorFlag);
                break;

            case BESSEL_Y:
                cwrkr = new double[sequenceNumber];
                cwrki = new double[sequenceNumber];
                if ( (zr == 0.0) && (zi == 0.0)) {
                    MipavUtil.displayError("BesselY must not have both zr and zi = 0.0");
                    errorFlag[0] = 1;

                    return;
                } // if ((zr == 0.00 && (zi == 0.0))

                zbesy(zr, zi, initialOrder, scalingOption, sequenceNumber, cyr, cyi, nz, cwrkr, cwrki, errorFlag);
                break;

            case AIRY_AI:
                zairy(zr, zi, derivativeOrder, scalingOption, cyr, cyi, nz, errorFlag);
                break;

            case AIRY_BI:
                zbiry(zr, zi, derivativeOrder, scalingOption, cyr, cyi, errorFlag);
                break;

        }
    }

    /**
     * DGAMLN COMPUTES THE NATURAL LOG OF THE GAMMA FUNCTION FOR Z.GT.0. THE ASYMPTOTIC EXPANSION IS USED TO GENERATE
     * VALUES GREATER THAN ZMIN WHICH ARE ADJUSTED BY THE RECURSION G(Z+1)=Z*G(Z) FOR Z.LE.ZMIN. THE FUNCTION WAS MADE
     * AS PORTABLE AS POSSIBLE BY COMPUTIMG ZMIN FROM THE NUMBER OF BASE 10 DIGITS IN A WORD,
     * RLN=AMAX1(-ALOG10(R1MACH(4)),0.5E-18) LIMITED TO 18 DIGITS OF (RELATIVE) ACCURACY.
     * 
     * <p>
     * SINCE INTEGER ARGUMENTS ARE COMMON, A TABLE LOOK UP ON 100 VALUES IS USED FOR SPEED OF EXECUTION.
     * </p>
     * 
     * @param z double, z > 0.0
     * @param ierr int[] error flag IERR=0, NORMAL RETURN, COMPUTATION COMPLETED IERR=1, Z.LE.0.0D0, NO COMPUTATION
     * 
     * @return double NATURAL LOG OF THE GAMMA FUNCTION AT Z.NE.0.0D0
     */
    private double dgamln(final double z, final int[] ierr) {
        final double[] gln = new double[] {0.00000000000000000E+00, 0.00000000000000000E+00, 6.93147180559945309E-01,
                1.79175946922805500E+00, 3.17805383034794562E+00, 4.78749174278204599E+00, 6.57925121201010100E+00,
                8.52516136106541430E+00, 1.06046029027452502E+01, 1.28018274800814696E+01, 1.51044125730755153E+01,
                1.75023078458738858E+01, 1.99872144956618861E+01, 2.25521638531234229E+01, 2.51912211827386815E+01,
                2.78992713838408916E+01, 3.06718601060806728E+01, 3.35050734501368889E+01, 3.63954452080330536E+01,
                3.93398841871994940E+01, 4.23356164607534850E+01, 4.53801388984769080E+01, 4.84711813518352239E+01,
                5.16066755677643736E+01, 5.47847293981123192E+01, 5.80036052229805199E+01, 6.12617017610020020E+01,
                6.45575386270063311E+01, 6.78897431371815350E+01, 7.12570389671680090E+01, 7.46582363488301644E+01,
                7.80922235533153106E+01, 8.15579594561150372E+01, 8.50544670175815174E+01, 8.85808275421976788E+01,
                9.21361756036870925E+01, 9.57196945421432025E+01, 9.93306124547874269E+01, 1.02968198614513813E+02,
                1.06631760260643459E+02, 1.10320639714757395E+02, 1.14034211781461703E+02, 1.17771881399745072E+02,
                1.21533081515438634E+02, 1.25317271149356895E+02, 1.29123933639127215E+02, 1.32952575035616310E+02,
                1.36802722637326368E+02, 1.40673923648234259E+02, 1.44565743946344886E+02, 1.48477766951773032E+02,
                1.52409592584497358E+02, 1.56360836303078785E+02, 1.60331128216630907E+02, 1.64320112263195181E+02,
                1.68327445448427652E+02, 1.72352797139162802E+02, 1.76395848406997352E+02, 1.80456291417543771E+02,
                1.84533828861449491E+02, 1.88628173423671591E+02, 1.92739047287844902E+02, 1.96866181672889994E+02,
                2.01009316399281527E+02, 2.05168199482641199E+02, 2.09342586752536836E+02, 2.13532241494563261E+02,
                2.17736934113954227E+02, 2.21956441819130334E+02, 2.26190548323727593E+02, 2.30439043565776952E+02,
                2.34701723442818268E+02, 2.38978389561834323E+02, 2.43268849002982714E+02, 2.47572914096186884E+02,
                2.51890402209723194E+02, 2.56221135550009525E+02, 2.60564940971863209E+02, 2.64921649798552801E+02,
                2.69291097651019823E+02, 2.73673124285693704E+02, 2.78067573440366143E+02, 2.82474292687630396E+02,
                2.86893133295426994E+02, 2.91323950094270308E+02, 2.95766601350760624E+02, 3.00220948647014132E+02,
                3.04686856765668715E+02, 3.09164193580146922E+02, 3.13652829949879062E+02, 3.18152639620209327E+02,
                3.22663499126726177E+02, 3.27185287703775217E+02, 3.31717887196928473E+02, 3.36261181979198477E+02,
                3.40815058870799018E+02, 3.45379407062266854E+02, 3.49954118040770237E+02, 3.54539085519440809E+02,
                3.59134205369575399E+02};
        final double[] cf = new double[] {8.33333333333333333E-02, -2.77777777777777778E-03, 7.93650793650793651E-04,
                -5.95238095238095238E-04, 8.41750841750841751E-04, -1.91752691752691753E-03, 6.41025641025641026E-03,
                -2.95506535947712418E-02, 1.79644372368830573E-01, -1.39243221690590112E+00, 1.34028640441683920E+01,
                -1.56848284626002017E+02, 2.19310333333333333E+03, -3.61087712537249894E+04, 6.91472268851313067E+05,
                -1.52382215394074162E+07, 3.82900751391414141E+08, -1.08822660357843911E+10, 3.47320283765002252E+11,
                -1.23696021422692745E+13, 4.88788064793079335E+14, -2.13203339609193739E+16};

        // ln(2*PI)
        final double con = 1.83787706640934548E+00;
        int nz;
        double fz;
        double wdtol;
        int i1m;
        double rln;
        double fln;
        double zm;
        int mz;
        double zmin;
        double zdmy;
        double zinc;
        double zp;
        double t1;
        double s;
        double zsq;
        double tst;
        double trm;
        double tlg;
        int k;
        int i;
        double gamln = 0.0;
        ierr[0] = 0;

        if (z <= 0.0) {
            ierr[0] = 1;

            return 0.0;
        }

        nz = (int) z;

        if (z <= 101.0) {
            fz = z - nz;

            if ( (fz <= 0.0) && (nz <= 100)) {
                return gln[nz - 1];
            }
        } // if (z <= 101.0)

        wdtol = epsilon;
        wdtol = Math.max(wdtol, 0.5e-18);
        i1m = doubleDigits;
        rln = r1m5 * i1m;
        fln = Math.min(rln, 20.0);
        fln = Math.max(fln, 3.0);
        fln = fln - 3.0;
        zm = 1.8 + (0.3875 * fln);
        mz = (int) (zm) + 1;
        zmin = mz;
        zdmy = z;
        zinc = 0.0;

        if (z < zmin) {
            zinc = zmin - nz;
            zdmy = z + zinc;
        } // if (z < zmin)

        zp = 1.0 / zdmy;
        t1 = cf[0] * zp;
        s = t1;

        if (zp >= wdtol) {
            zsq = zp * zp;
            tst = t1 * wdtol;

            for (k = 1; k <= 21; k++) {
                zp = zp * zsq;
                trm = cf[k] * zp;

                if (Math.abs(trm) < tst) {
                    break;
                }

                s = s + trm;
            } // for (k = 1; k <= 21; k++)
        } // if (zp >= wdtol)

        if (zinc == 0.0) {
            tlg = Math.log(z);
            gamln = (z * (tlg - 1.0)) + (0.5 * (con - tlg)) + s;

            return gamln;
        } // if (zinc == 0.0)

        zp = 1.0;
        nz = (int) (zinc);

        for (i = 1; i <= nz; i++) {
            zp = zp * (z + i - 1.0);
        }

        tlg = Math.log(zdmy);
        gamln = (zdmy * (tlg - 1.0)) - Math.log(zp) + (0.5 * (con - tlg)) + s;

        return gamln;
    }

    /**
     * DOCUMENT ME!
     */
    private void runTest() {

        if (BesselType == Bessel.BESSEL_H) {

            if (overflowTest) {
                zqcbh(2);
            } else {
                zqcbh(1);
            }
        } // if (BesselType == BESSEL_H)
        else if (BesselType == Bessel.BESSEL_I) {

            if (overflowTest) {
                zqcbi(2);
            } else {
                zqcbi(1);
            }
        } // else if (BesselType == BESSEL_I);
        else if (BesselType == Bessel.BESSEL_J) {

            if (overflowTest) {
                zqcbj(2);
            } else {
                zqcbj(1);
            }
        } // else if (BesselType == BESSEL_J);
        else if (BesselType == Bessel.BESSEL_K) {

            if (overflowTest) {
                zqcbk(2);
            } else {
                zqcbk(1);
            }
        } // else if (BesselType == BESSEL_K)
        else if (BesselType == Bessel.BESSEL_Y) {

            if (overflowTest) {
                zqcby(2);
            } else {
                zqcby(1);
            }
        } // else if (BesselType == BESSEL_Y)
        else if ( (BesselType == Bessel.AIRY_AI) || (BesselType == Bessel.AIRY_BI)) {

            if (overflowTest) {
                zqcai(2);
            } else {
                zqcai(1);
            }
        } // else if ((BesselType == AIRY_AI)|| (BesselType == AIRY_BI))

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

    /**
     * ZACAI APPLIES THE ANALYTIC CONTINUATION FORMULA.
     * 
     * <p>
     * K(FNU,ZN*EXP(MP))=K(FNU,ZN)*EXP(-MP*FNU) - MP*I(FNU,ZN) MP=PI*MR*CMPLX(0.0,1.0)
     * </p>
     * 
     * <p>
     * TO CONTINUE THE K FUNCTION FROM THE RIGHT HALF TO THE LEFT HALF Z PLANE FOR USE WITH ZAIRY WHERE FNU=1/3 OR 2/3
     * AND N=1. ZACAI IS THE SAME AS ZACON WITH THE PARTS FOR LARGER ORDERS AND RECURRENCE REMOVED. A RECURSIVE CALL TO
     * ZACON CAN RESULT IF ZACON IS CALLED FROM ZAIRY.
     * </p>
     * 
     * @param zr double
     * @param zi double
     * @param fnu double
     * @param kode int
     * @param mr int
     * @param n int
     * @param yr double[]
     * @param yi double[]
     * @param nz int[]
     */
    private void zacai(double zr, double zi, final double fnu, final int kode, final int mr, final int n,
            final double[] yr, final double[] yi, final int[] nz) {
        final double[] cyr = new double[2];
        final double[] cyi = new double[2];
        double znr;
        double zni;
        double az;
        int nn;
        double dfnu;
        final int[] nw = new int[1];
        double fmr;
        double sgn;
        double csgnr;
        double csgni;
        double yy;
        int inu;
        double arg;
        double cspnr;
        double cspni;
        final double[] c1r = new double[1];
        final double[] c1i = new double[1];
        final double[] c2r = new double[1];
        final double[] c2i = new double[1];
        final int[] iuf = new int[1];
        double ascle;

        nz[0] = 0;
        znr = -zr;
        zni = -zi;
        az = zabs(zr, zi);
        nn = n;
        dfnu = fnu + n - 1.0;

        if ( (az <= 2.0) || ( (az > 2.0) && ( (az * az * 0.25) <= (dfnu + 1.0)))) {

            // Power series for the I function
            zseri(znr, zni, fnu, kode, nn, yr, yi, nw);
        } // if (az <= 2.0)
        else {

            if (az >= rl) {

                // Asymptotic expansion for large z for the I function
                zasyi(znr, zni, fnu, kode, nn, yr, yi, nw);

                if (nw[0] < 0) {
                    nz[0] = -1;

                    if (nw[0] == -2) {
                        nz[0] = -2;
                    }

                    return;
                } // if (nw[0] < 0)
            } // if (az >= rl)
            else { // az < rl

                // Miller algorithm normalized by the series for the I function
                zmlri(znr, zni, fnu, kode, nn, yr, yi, nw);

                if (nw[0] < 0) {
                    nz[0] = -1;

                    if (nw[0] == -2) {
                        nz[0] = -2;
                    }

                    return;
                } // if (nw[0] < 0)
            } // else az < rl
        } // else

        // Analytic continuation to the left half plane for the K function
        zbknu(znr, zni, fnu, kode, 1, cyr, cyi, nw);

        if (nw[0] != 0) {
            nz[0] = -1;

            if (nw[0] == -2) {
                nz[0] = -2;
            }

            return;
        } // if (nw[0] != 0)

        fmr = mr;

        if (fmr >= 0.0) {
            sgn = -Math.PI;
        } else {
            sgn = Math.PI;
        }

        csgnr = 0.0;
        csgni = sgn;

        if (kode != 1) {
            yy = -zni;
            csgnr = -csgni * Math.sin(yy);
            csgni = csgni * Math.cos(yy);
        } // if (kode != 1)

        // Calculate cspn = exp(fnu*PI*i) to minimize losses of significance
        // when fnu is large
        inu = (int) (fnu);
        arg = (fnu - inu) * sgn;
        cspnr = Math.cos(arg);
        cspni = Math.sin(arg);

        if ( (inu % 2) != 0) {
            cspnr = -cspnr;
            cspni = -cspni;
        } // if ((inu%2) != 0)

        c1r[0] = cyr[0];
        c1i[0] = cyi[0];
        c2r[0] = yr[0];
        c2i[0] = yi[0];

        if (kode != 1) {
            iuf[0] = 0;
            ascle = 1.0E3 * tiny / tol;
            zs1s2(znr, zni, c1r, c1i, c2r, c2i, nw, ascle, iuf);
            nz[0] = nz[0] + nw[0];
        } // if (kode != 1)

        yr[0] = (cspnr * c1r[0]) - (cspni * c1i[0]) + (csgnr * c2r[0]) - (csgni * c2i[0]);
        yi[0] = (cspnr * c1i[0]) + (cspni * c1r[0]) + (csgnr * c2i[0]) + (csgni * c2r[0]);

        return;
    }

    /**
     * ZACON APPLIES THE ANALYTIC CONTINUATION FORMULA.
     * 
     * <p>
     * K(FNU,ZN*EXP(MP))=K(FNU,ZN)*EXP(-MP*FNU) - MP*I(FNU,ZN) MP=PI*MR*CMPLX(0.0,1.0)
     * </p>
     * 
     * <p>
     * TO CONTINUE THE K FUNCTION FROM THE RIGHT HALF TO THE LEFT HALF Z PLANE
     * </p>
     * 
     * @param zr double
     * @param zi double
     * @param fnu double
     * @param kode int
     * @param mr int
     * @param n int
     * @param yr double[]
     * @param yi double[]
     * @param nz int[]
     */
    private void zacon(double zr, double zi, final double fnu, final int kode, final int mr, final int n,
            final double[] yr, final double[] yi, final int[] nz) {
        final double[] cyr = new double[2];
        final double[] cyi = new double[2];
        final double[] cssr = new double[3];
        final double[] csrr = new double[3];
        final double[] bry = new double[3];
        double znr;
        double zni;
        int nn;
        final int[] nw = new int[1];
        double s1r;
        double s1i;
        double s2r;
        double s2i;
        double fmr;
        double sgn;
        final double[] csgnr = new double[1];
        final double[] csgni = new double[1];
        double yy;
        double cpn;
        double spn;
        int inu;
        double arg;
        double cspnr;
        double cspni;
        final int[] iuf = new int[1];
        final double[] c1r = new double[1];
        final double[] c1i = new double[1];
        final double[] c2r = new double[1];
        final double[] c2i = new double[1];
        double ascle;
        double sc1r;
        double sc1i;
        final double[] str = new double[1];
        final double[] sti = new double[1];
        final double[] ptr = new double[1];
        final double[] pti = new double[1];
        double sc2r = 0.0;
        double sc2i = 0.0;
        double azn;
        double razn;
        double rzr;
        double rzi;
        double fn;
        double ckr;
        double cki;
        double cscl;
        double cscr;
        double as2;
        int kflag;
        double bscle;
        double csr;
        int i;
        double c1m;

        nz[0] = 0;
        znr = -zr;
        zni = -zi;
        nn = n;
        zbinu(znr, zni, fnu, kode, nn, yr, yi, nw);

        if (nw[0] == -2) {
            nz[0] = -2;

            return;
        } else if (nw[0] < 0) {
            nz[0] = -1;

            return;
        }

        // Analytic continuation to the left half plane for the K function
        nn = Math.min(2, n);
        zbknu(znr, zni, fnu, kode, nn, cyr, cyi, nw);

        if (nw[0] == -2) {
            nz[0] = -2;

            return;
        } else if (nw[0] != 0) {
            nz[0] = -1;

            return;
        }

        s1r = cyr[0];
        s1i = cyi[0];
        fmr = mr;

        if (fmr >= 0.0) {
            sgn = -Math.PI;
        } else {
            sgn = Math.PI;
        }

        csgnr[0] = 0.0;
        csgni[0] = sgn;

        if (kode != 1) {
            yy = -zni;
            cpn = Math.cos(yy);
            spn = Math.sin(yy);
            zmlt(csgnr[0], csgni[0], cpn, spn, csgnr, csgni);
        } // if (kode != 1)

        // Calculate cspn = exp(fnu*PI*i) to minimize losses of significance
        // when fnu is large
        inu = (int) (fnu);
        arg = (fnu - inu) * sgn;
        cpn = Math.cos(arg);
        spn = Math.sin(arg);
        cspnr = cpn;
        cspni = spn;

        if ( (inu % 2) != 0) {
            cspnr = -cspnr;
            cspni = -cspni;
        } // if ((inu%2) != 0)

        iuf[0] = 0;
        c1r[0] = s1r;
        c1i[0] = s1i;
        c2r[0] = yr[0];
        c2i[0] = yi[0];
        ascle = 1.0E3 * tiny / tol;

        if (kode != 1) {
            zs1s2(znr, zni, c1r, c1i, c2r, c2i, nw, ascle, iuf);
            nz[0] = nz[0] + nw[0];
            sc1r = c1r[0];
            sc1i = c1i[0];
        } // if (kode != 1)

        zmlt(cspnr, cspni, c1r[0], c1i[0], str, sti);
        zmlt(csgnr[0], csgni[0], c2r[0], c2i[0], ptr, pti);
        yr[0] = str[0] + ptr[0];
        yi[0] = sti[0] + pti[0];

        if (n == 1) {
            return;
        } // if (n == 1)

        cspnr = -cspnr;
        cspni = -cspni;
        s2r = cyr[1];
        s2i = cyi[1];
        c1r[0] = s2r;
        c1i[0] = s2i;
        c2r[0] = yr[1];
        c2i[0] = yi[1];

        if (kode != 1) {
            zs1s2(znr, zni, c1r, c1i, c2r, c2i, nw, ascle, iuf);
            nz[0] = nz[0] + nw[0];
            sc2r = c1r[0];
            sc2i = c1i[0];
        } // if (kode != 1)

        zmlt(cspnr, cspni, c1r[0], c1i[0], str, sti);
        zmlt(csgnr[0], csgni[0], c2r[0], c2i[0], ptr, pti);
        yr[1] = str[0] + ptr[0];
        yi[1] = sti[0] + pti[0];

        if (n == 2) {
            return;
        } // if (n == 2)

        cspnr = -cspnr;
        cspni = -cspni;
        azn = zabs(znr, zni);
        razn = 1.0 / azn;
        str[0] = znr * razn;
        sti[0] = -zni * razn;
        rzr = (str[0] + str[0]) * razn;
        rzi = (sti[0] + sti[0]) * razn;
        fn = fnu + 1.0;
        ckr = fn * rzr;
        cki = fn * rzi;

        // Scale near exponent extremes during recurrence on K functions
        cscl = 1.0 / tol;
        cscr = tol;
        cssr[0] = cscl;
        cssr[1] = 1.0;
        cssr[2] = cscr;
        csrr[0] = cscr;
        csrr[1] = 1.0;
        csrr[2] = cscl;
        bry[0] = ascle;
        bry[1] = 1.0 / ascle;
        bry[2] = Double.MAX_VALUE;
        as2 = zabs(s2r, s2i);
        kflag = 2;

        if (as2 <= bry[0]) {
            kflag = 1;
        } // if (as2 <= bry[0])
        else if ( (as2 > bry[0]) && (as2 >= bry[1])) {
            kflag = 3;
        } // else if ((as2 > bry[0]) && (as2 >= bry[1]))

        bscle = bry[kflag - 1];
        s1r = s1r * cssr[kflag - 1];
        s1i = s1i * cssr[kflag - 1];
        s2r = s2r * cssr[kflag - 1];
        s2i = s2i * cssr[kflag - 1];
        csr = csrr[kflag - 1];

        for (i = 3; i <= n; i++) {
            str[0] = s2r;
            sti[0] = s2i;
            s2r = (ckr * str[0]) - (cki * sti[0]) + s1r;
            s2i = (ckr * sti[0]) + (cki * str[0]) + s1i;
            s1r = str[0];
            s1i = sti[0];
            c1r[0] = s2r * csr;
            c1i[0] = s2i * csr;
            str[0] = c1r[0];
            sti[0] = c1i[0];
            c2r[0] = yr[i - 1];
            c2i[0] = yi[i - 1];

            if ( (kode != 1) && (iuf[0] >= 0)) {
                zs1s2(znr, zni, c1r, c1i, c2r, c2i, nw, ascle, iuf);
                nz[0] = nz[0] + nw[0];
                sc1r = sc2r;
                sc1i = sc2i;
                sc2r = c1r[0];
                sc2i = c1i[0];

                if (iuf[0] == 3) {
                    iuf[0] = -4;
                    s1r = sc1r * cssr[kflag - 1];
                    s1i = sc1i * cssr[kflag - 1];
                    s2r = sc2r * cssr[kflag - 1];
                    s2i = sc2i * cssr[kflag - 1];
                    str[0] = sc2r;
                    sti[0] = sc2i;
                } // if (iuf[0] == 3)
            } // if ((kode != 1) && (iuf[0] >= 0))

            ptr[0] = (cspnr * c1r[0]) - (cspni * c1i[0]);
            pti[0] = (cspnr * c1i[0]) + (cspni * c1r[0]);
            yr[i - 1] = ptr[0] + (csgnr[0] * c2r[0]) - (csgni[0] * c2i[0]);
            yi[i - 1] = pti[0] + (csgnr[0] * c2i[0]) + (csgni[0] * c2r[0]);
            ckr = ckr + rzr;
            cki = cki + rzi;
            cspnr = -cspnr;
            cspni = -cspni;

            if (kflag >= 3) {
                continue;
            } // if (kflag >= 3)

            ptr[0] = Math.abs(c1r[0]);
            pti[0] = Math.abs(c1i[0]);
            c1m = Math.max(ptr[0], pti[0]);

            if (c1m <= bscle) {
                continue;
            } // if (c1m <= bscle)

            kflag = kflag + 1;
            bscle = bry[kflag - 1];
            s1r = s1r * csr;
            s1i = s1i * csr;
            s2r = str[0];
            s2i = sti[0];
            s1r = s1r * cssr[kflag - 1];
            s1i = s1i * cssr[kflag - 1];
            s2r = s2r * cssr[kflag - 1];
            s2i = s2i * cssr[kflag - 1];
            csr = csrr[kflag - 1];
        } // for (i = 3; i <= n; i++)

        return;
    }

    /**
     * ON KODE=1, ZAIRY COMPUTES THE COMPLEX AIRY FUNCTION AI(Z) OR ITS DERIVATIVE DAI(Z)/DZ ON ID=0 OR ID=1
     * RESPECTIVELY. ON KODE=2, A SCALING OPTION CEXP(ZTA)*AI(Z) OR CEXP(ZTA)* DAI(Z)/DZ IS PROVIDED TO REMOVE THE
     * EXPONENTIAL DECAY IN -PI/3.LT.ARG(Z).LT.PI/3 AND THE EXPONENTIAL GROWTH IN PI/3.LT.ABS(ARG(Z)).LT.PI WHERE
     * ZTA=(2/3)*Z*CSQRT(Z).
     * 
     * <p>
     * WHILE THE AIRY FUNCTIONS AI(Z) AND DAI(Z)/DZ ARE ANALYTIC IN THE WHOLE Z PLANE, THE CORRESPONDING SCALED
     * FUNCTIONS DEFINED FOR KODE=2 HAVE A CUT ALONG THE NEGATIVE REAL AXIS. DEFINTIONS AND NOTATION ARE FOUND IN THE
     * NBS HANDBOOK OF C MATHEMATICAL FUNCTIONS (REF. 1). C C INPUT ZR,ZI ARE DOUBLE PRECISION C ZR,ZI - Z=CMPLX(ZR,ZI)
     * C ID - ORDER OF DERIVATIVE, ID=0 OR ID=1 C KODE - A PARAMETER TO INDICATE THE SCALING OPTION C KODE= 1 RETURNS C
     * AI=AI(Z) ON ID=0 OR C AI=DAI(Z)/DZ ON ID=1 C = 2 RETURNS C AI=CEXP(ZTA)*AI(Z) ON ID=0 OR C AI=CEXP(ZTA)*DAI(Z)/DZ
     * ON ID=1 WHERE C ZTA=(2/3)*Z*CSQRT(Z) C C OUTPUT AIR,AII ARE DOUBLE PRECISION C AIR,AII- COMPLEX ANSWER DEPENDING
     * ON THE CHOICES FOR ID AND C KODE C NZ - UNDERFLOW INDICATOR C NZ= 0 , NORMAL RETURN C NZ= 1 ,
     * AI=CMPLX(0.0D0,0.0D0) DUE TO UNDERFLOW IN C -PI/3.LT.ARG(Z).LT.PI/3 ON KODE=1 C IERR - ERROR FLAG C IERR=0,
     * NORMAL RETURN - COMPUTATION COMPLETED C IERR=1, INPUT ERROR - NO COMPUTATION C IERR=2, OVERFLOW - NO COMPUTATION,
     * REAL(ZTA) C TOO LARGE ON KODE=1 C IERR=3, CABS(Z) LARGE - COMPUTATION COMPLETED C LOSSES OF SIGNIFCANCE BY
     * ARGUMENT REDUCTION C PRODUCE LESS THAN HALF OF MACHINE ACCURACY C IERR=4, CABS(Z) TOO LARGE - NO COMPUTATION C
     * COMPLETE LOSS OF ACCURACY BY ARGUMENT C REDUCTION C IERR=5, ERROR - NO COMPUTATION, C ALGORITHM TERMINATION
     * CONDITION NOT MET C C***LONG DESCRIPTION C C AI AND DAI ARE COMPUTED FOR CABS(Z).GT.1.0 FROM THE K BESSEL C
     * FUNCTIONS BY C C AI(Z)=C*SQRT(Z)*K(1/3,ZTA) , DAI(Z)=-C*Z*K(2/3,ZTA) C C=1.0/(PI*SQRT(3.0)) C ZTA=(2/3)*Z**(3/2)
     * C C WITH THE POWER SERIES FOR CABS(Z).LE.1.0. C C IN MOST COMPLEX VARIABLE COMPUTATION, ONE MUST EVALUATE ELE- C
     * MENTARY FUNCTIONS. WHEN THE MAGNITUDE OF Z IS LARGE, LOSSES C OF SIGNIFICANCE BY ARGUMENT REDUCTION OCCUR.
     * CONSEQUENTLY, IF C THE MAGNITUDE OF ZETA=(2/3)*Z**1.5 EXCEEDS U1=SQRT(0.5/UR), C THEN LOSSES EXCEEDING HALF
     * PRECISION ARE LIKELY AND AN ERROR C FLAG IERR=3 IS TRIGGERED WHERE UR=DMAX1(D1MACH(4),1.0D-18) IS C DOUBLE
     * PRECISION UNIT ROUNDOFF LIMITED TO 18 DIGITS PRECISION. C ALSO, IF THE MAGNITUDE OF ZETA IS LARGER THAN
     * U2=0.5/UR, THEN C ALL SIGNIFICANCE IS LOST AND IERR=4. IN ORDER TO USE THE INT C FUNCTION, ZETA MUST BE FURTHER
     * RESTRICTED NOT TO EXCEED THE C LARGEST INTEGER, U3=I1MACH(9). THUS, THE MAGNITUDE OF ZETA C MUST BE RESTRICTED BY
     * MIN(U2,U3). ON 32 BIT MACHINES, U1,U2, C AND U3 ARE APPROXIMATELY 2.0E+3, 4.2E+6, 2.1E+9 IN SINGLE C PRECISION
     * ARITHMETIC AND 1.3E+8, 1.8E+16, 2.1E+9 IN DOUBLE C PRECISION ARITHMETIC RESPECTIVELY. THIS MAKES U2 AND U3 LIMIT-
     * C ING IN THEIR RESPECTIVE ARITHMETICS. THIS MEANS THAT THE MAG- C NITUDE OF Z CANNOT EXCEED 3.1E+4 IN SINGLE AND
     * 2.1E+6 IN C DOUBLE PRECISION ARITHMETIC. THIS ALSO MEANS THAT ONE CAN C EXPECT TO RETAIN, IN THE WORST CASES ON
     * 32 BIT MACHINES, C NO DIGITS IN SINGLE PRECISION AND ONLY 7 DIGITS IN DOUBLE C PRECISION ARITHMETIC. SIMILAR
     * CONSIDERATIONS HOLD FOR OTHER C MACHINES. C C THE APPROXIMATE RELATIVE ERROR IN THE MAGNITUDE OF A COMPLEX C
     * BESSEL FUNCTION CAN BE EXPRESSED BY P*10**S WHERE P=MAX(UNIT C ROUNDOFF,1.0E-18) IS THE NOMINAL PRECISION AND
     * 10**S REPRE- C SENTS THE INCREASE IN ERROR DUE TO ARGUMENT REDUCTION IN THE C ELEMENTARY FUNCTIONS. HERE,
     * S=MAX(1,ABS(LOG10(CABS(Z))), C ABS(LOG10(FNU))) APPROXIMATELY (I.E. S=MAX(1,ABS(EXPONENT OF C
     * CABS(Z),ABS(EXPONENT OF FNU)) ). HOWEVER, THE PHASE ANGLE MAY C HAVE ONLY ABSOLUTE ACCURACY. THIS IS MOST LIKELY
     * TO OCCUR WHEN C ONE COMPONENT (IN ABSOLUTE VALUE) IS LARGER THAN THE OTHER BY C SEVERAL ORDERS OF MAGNITUDE. IF
     * ONE COMPONENT IS 10**K LARGER C THAN THE OTHER, THEN ONE CAN EXPECT ONLY MAX(ABS(LOG10(P))-K, C 0) SIGNIFICANT
     * DIGITS; OR, STATED ANOTHER WAY, WHEN K EXCEEDS C THE EXPONENT OF P, NO SIGNIFICANT DIGITS REMAIN IN THE SMALLER C
     * COMPONENT. HOWEVER, THE PHASE ANGLE RETAINS ABSOLUTE ACCURACY C BECAUSE, IN COMPLEX ARITHMETIC WITH PRECISION P,
     * THE SMALLER C COMPONENT WILL NOT (AS A RULE) DECREASE BELOW P TIMES THE C MAGNITUDE OF THE LARGER COMPONENT. IN
     * THESE EXTREME CASES, C THE PRINCIPAL PHASE ANGLE IS ON THE ORDER OF +P, -P, PI/2-P, C OR -PI/2+P. C
     * C***REFERENCES HANDBOOK OF MATHEMATICAL FUNCTIONS BY M. ABRAMOWITZ C AND I. A. STEGUN, NBS AMS SERIES 55, U.S.
     * DEPT. OF C COMMERCE, 1955. C C COMPUTATION OF BESSEL FUNCTIONS OF COMPLEX ARGUMENT C AND LARGE ORDER BY D. E.
     * AMOS, SAND83-0643, MAY, 1983 C C A SUBROUTINE PACKAGE FOR BESSEL FUNCTIONS OF A COMPLEX C ARGUMENT AND
     * NONNEGATIVE ORDER BY D. E. AMOS, SAND85- C 1018, MAY, 1985 C C A PORTABLE PACKAGE FOR BESSEL FUNCTIONS OF A
     * COMPLEX C ARGUMENT AND NONNEGATIVE ORDER BY D. E. AMOS, ACM C TRANS. MATH. SOFTWARE, VOL. 12, NO. 3, SEPTEMBER
     * 1986, C PP 265-273.
     * </p>
     * 
     * @param zr double
     * @param zi double
     * @param id int
     * @param kode int
     * @param air double[]
     * @param aii double[]
     * @param nz int[]
     * @param ierr int[]
     */
    private void zairy(final double zr, final double zi, final int id, final int kode, final double[] air,
            final double[] aii, final int[] nz, final int[] ierr) {
        final double[] cyr = new double[1];
        final double[] cyi = new double[1];
        final double tth = 2.0 / 3.0;
        final double c1 = 3.55028053887817240E-01;
        double c2 = 2.58819403792806799E-01;
        final double coef = 1.83776298473930683E-01;
        final double[] str = new double[1];
        final double[] sti = new double[1];
        final double[] csqr = new double[1];
        final double[] csqi = new double[1];
        final int[] nn = new int[1];
        double az;
        double fid;
        double s1r;
        double s1i;
        double s2r;
        double s2i;
        double aa;
        double trm1r;
        double trm1i;
        double trm2r;
        double trm2i;
        double atrm;
        double z3r;
        double z3i;
        double az3;
        double ak;
        double bk;
        double ck;
        double dk;
        double d1;
        double d2;
        double ad;
        int k;
        double ztar;
        double ztai;
        double ptr;
        double cc;
        double fnu;
        int k1;
        double dig;
        double alaz;
        double bb;
        int iflag;
        double sfac;
        int mr;
        double neweps;

        ierr[0] = 0;
        nz[0] = 0;

        if ( (id < 0) || (id > 1) || (kode < 1) || (kode > 2)) {
            ierr[0] = 1;

            return;
        } // if ((id < 0) || (id > 1) || (kode < 1) || (kode > 2))

        az = zabs(zr, zi);

        // epsilon = D1MACH(4)
        // Machine epsilon is the smallest positive epsilon such that
        // (1.0 + epsilon) != 1.0.
        // epsilon = 2**(1 - doubleDigits) = 2**(1 - 53) = 2**(-52)
        // epsilon = 2.2204460e-16
        // epsilon is called the largest relative spacing
        epsilon = 1.0;
        neweps = 1.0;

        while (true) {

            if (1.0 == (1.0 + neweps)) {
                break;
            } else {
                epsilon = neweps;
                neweps = neweps / 2.0;
            }
        } // while(true)

        tol = Math.max(epsilon, 1.0E-18);
        tiny = Math.pow(2, -1022);
        fid = id;

        if (az <= 1.0) {

            // Power series for cabs(z) <= 1
            s1r = 1.0;
            s1i = 0.0;
            s2r = 1.0;
            s2i = 0.0;

            if (az < tol) {
                aa = 1.0E3 * tiny;
                s1r = 0.0;
                s1i = 0.0;

                if (id == 1) {
                    air[0] = -c2;
                    aii[0] = 0.0;
                    aa = Math.sqrt(aa);

                    if (az > aa) {
                        s1r = 0.5 * ( (zr * zr) - (zi * zi));
                        s1i = zr * zi;
                    } // if (az > aa)

                    air[0] = air[0] + (c1 * s1r);
                    aii[0] = aii[0] + (c1 * s1i);

                    return;
                } // if (id == 1)
                else { // id != 1

                    if (az > aa) {
                        s1r = c2 * zr;
                        s1i = c2 * zi;
                    } // if (az > aa)

                    air[0] = c1 - s1r;
                    aii[0] = -s1i;

                    return;
                } // else id != 1
            } // if (az < tol)

            aa = az * az;

            if (aa >= (tol / az)) {
                trm1r = 1.0;
                trm1i = 0.0;
                trm2r = 1.0;
                trm2i = 0.0;
                atrm = 1.0;
                str[0] = (zr * zr) - (zi * zi);
                sti[0] = 2.0 * zr * zi;
                z3r = (str[0] * zr) - (sti[0] * zi);
                z3i = (str[0] * zi) + (sti[0] * zr);
                az3 = az * aa;
                ak = 2.0 + fid;
                bk = 3.0 - fid - fid;
                ck = 4.0 - fid;
                dk = 3.0 + fid + fid;
                d1 = ak * dk;
                d2 = bk * ck;
                ad = Math.min(d1, d2);
                ak = 24.0 + (9.0 * fid);
                bk = 30.0 - (9.0 * fid);

                for (k = 1; k <= 25; k++) {
                    str[0] = ( (trm1r * z3r) - (trm1i * z3i)) / d1;
                    trm1i = ( (trm1r * z3i) + (trm1i * z3r)) / d1;
                    trm1r = str[0];
                    s1r = s1r + trm1r;
                    s1i = s1i + trm1i;
                    str[0] = ( (trm2r * z3r) - (trm2i * z3i)) / d2;
                    trm2i = ( (trm2r * z3i) + (trm2i * z3r)) / d2;
                    trm2r = str[0];
                    s2r = s2r + trm2r;
                    s2i = s2i + trm2i;
                    atrm = atrm * az3 / ad;
                    d1 = d1 + ak;
                    d2 = d2 + bk;

                    if (atrm < (tol * ad)) {
                        break;
                    } // if (atrm < (tol*ad))

                    ak = ak + 18.0;
                    bk = bk + 18.0;
                } // for (k = 1; k <= 25; k++)
            } // if (aa >= (tol/az))

            if (id != 1) {
                air[0] = (s1r * c1) - (c2 * ( (zr * s2r) - (zi * s2i)));
                aii[0] = (s1i * c1) - (c2 * ( (zr * s2i) + (zi * s2r)));

                if (kode == 1) {
                    return;
                } // if (kode == 1)

                zsqrt(zr, zi, str, sti);
                ztar = tth * ( (zr * str[0]) - (zi * sti[0]));
                ztai = tth * ( (zr * sti[0]) + (zi * str[0]));
                zexp(ztar, ztai, str, sti);
                ptr = (air[0] * str[0]) - (aii[0] * sti[0]);
                aii[0] = (air[0] * sti[0]) + (aii[0] * str[0]);
                air[0] = ptr;

                return;
            } // if (id != 1)

            air[0] = -s2r * c2;
            aii[0] = -s2i * c2;

            if (az > tol) {
                str[0] = (zr * s1r) - (zi * s1i);
                sti[0] = (zr * s1i) + (zi * s1r);
                cc = c1 / (1.0 + fid);
                air[0] = air[0] + (cc * ( (str[0] * zr) - (sti[0] * zi)));
                aii[0] = aii[0] + (cc * ( (str[0] * zi) + (sti[0] * zr)));
            } // if (az > tol)

            if (kode == 1) {
                return;
            } // if (kode == 1)

            zsqrt(zr, zi, str, sti);
            ztar = tth * ( (zr * str[0]) - (zi * sti[0]));
            ztai = tth * ( (zr * sti[0]) + (zi * str[0]));
            zexp(ztar, ztai, str, sti);
            ptr = (str[0] * air[0]) - (sti[0] * aii[0]);
            aii[0] = (str[0] * aii[0]) + (sti[0] * air[0]);
            air[0] = ptr;

            return;
        } // if (az <= 1.0)

        // Case for CABS(z) > 1.0
        fnu = (1.0 + fid) / 3.0;

        // SET PARAMETERS RELATED TO MACHINE CONSTANTS.
        // TOL IS THE APPROXIMATE UNIT ROUNDOFF LIMITED TO 1.0E-18.
        // ELIM IS THE APPROXIMATE EXPONENTIAL OVER- AND UNDERFLOW LIMIT.
        // EXP(-ELIM).LT.EXP(-ALIM)=EXP(-ELIM)/TOL AND
        // EXP(ELIM).GT.EXP(ALIM)=EXP(ELIM)*TOL ARE INTERVALS NEAR
        // UNDERFLOW AND OVERFLOW LIMITS WHERE SCALED ARITHMETIC IS DONE.
        // RL IS THE LOWER BOUNDARY OF THE ASYMPTOTIC EXPANSION FOR LARGE Z.
        // DIG = NUMBER OF BASE 10 DIGITS IN TOL = 10**(-DIG).
        // emin, the smallest exponent E for double precision, is I1MACH(15)
        // tiny = D1MACH(1) = 2**(emin - 1) = 2**(-1022)
        // D1MACH(1) is the smallest normalized number, which preserves the
        // full precision of the mantissa.
        // Double.MIN_VALUE = 2**(-1074) is the smallest denormalized number,
        // which preseerves only a portion of the fraction's precision.
        emin = -1021;

        // emax, the largest exponent E for double precision, is I1MACH(16)
        // D1MACH(2) = 2**(emax)*(1 - 2**(-doubleDigits)) = 2**1024*(1 - 2**-53)
        // D1MACH(2) = Double.MAX_VALUE
        emax = 1024;

        // r1m5 = log10(2), which is D1MACH(5)
        r1m5 = 0.434294481903251 * Math.log(2.0);
        k = Math.min(Math.abs(emin), Math.abs(emax));
        elim = 2.303 * ( (k * r1m5) - 3.0);

        // doubleDigits is I1MACH(14), the number of base-B digits for double
        // precision. IEEE 754 double precision has 64 bits with 1 sign bit,
        // 11 exponent bits, and 52 fraction, mantissa, or signficand bits.
        // Floating-point numbers are typically stored in normalized form,
        // with the radix point after the first non-zero digit. In base two,
        // the only possible non-zero digit is 1. Thus, a leading digit of 1
        // can always be assumed and need not be explicitly represented.
        // Therefore, the mantissa effectively has 53 bits of resolution.
        doubleDigits = 53;
        k1 = doubleDigits - 1;
        aa = r1m5 * k1;
        dig = Math.min(aa, 18.0);
        aa = 2.303 * aa;
        alim = elim + Math.max( -aa, -41.45);
        rl = (1.2 * dig) + 3.0;
        alaz = Math.log(az);

        // Test for proper range
        aa = 0.5 / tol;
        bb = 0.5 * Integer.MAX_VALUE;
        aa = Math.min(aa, bb);
        aa = Math.pow(aa, tth);

        if (az > aa) {
            ierr[0] = 4;
            nz[0] = 0;

            return;
        } // if (az > aa)

        aa = Math.sqrt(aa);

        if (az > aa) {
            ierr[0] = 3;
        } // if (az > aa)

        zsqrt(zr, zi, csqr, csqi);
        ztar = tth * ( (zr * csqr[0]) - (zi * csqi[0]));
        ztai = tth * ( (zr * csqi[0]) + (zi * csqr[0]));

        // Re(zta) <= 0 when Re(z) < 0, especially when Im(z) is small
        iflag = 0;
        sfac = 1.0;
        ak = ztai;

        if (zr < 0.0) {
            bk = ztar;
            ck = -Math.abs(bk);
            ztar = ck;
            ztai = ak;
        } // if (zr < 0.0)

        if ( (zi == 0.0) && (zr <= 0.0)) {
            ztar = 0.0;
            ztai = ak;
        } // if ((zi == 0.0) && (zr <= 0.0))

        aa = ztar;

        if ( (aa >= 0.0) && (zr > 0.0)) {

            if ( (kode != 2) && (aa >= alim)) {
                aa = -aa - (0.25 * alaz);
                iflag = 2;
                sfac = 1.0 / tol;

                if (aa < ( -elim)) {
                    nz[0] = 1;
                    air[0] = 0.0;
                    aii[0] = 0.0;

                    return;
                } // if (aa < (-elim))
            } // if ((kode != 2) && (aa >= alim))

            zbknu(ztar, ztai, fnu, kode, 1, cyr, cyi, nz);
            s1r = cyr[0] * coef;
            s1i = cyi[0] * coef;

            if (iflag != 0) {
                s1r = s1r * sfac;
                s1i = s1i * sfac;

                if (id == 1) {
                    str[0] = - ( (s1r * zr) - (s1i * zi));
                    s1i = - ( (s1r * zi) + (s1i * zr));
                    s1r = str[0];
                    air[0] = s1r / sfac;
                    aii[0] = s1i / sfac;

                    return;
                } // if (id == 1)
                else { // id != 1
                    str[0] = (s1r * csqr[0]) - (s1i * csqi[0]);
                    s1i = (s1r * csqi[0]) + (s1i * csqr[0]);
                    s1r = str[0];
                    air[0] = s1r / sfac;
                    aii[0] = s1i / sfac;

                    return;
                } // else id != 1
            } // if (iflag != 0)
            else if (id == 1) {
                air[0] = - ( (zr * s1r) - (zi * s1i));
                aii[0] = - ( (zr * s1i) + (zi * s1r));

                return;
            } // else if (id == 1)
            else {
                air[0] = (csqr[0] * s1r) - (csqi[0] * s1i);
                aii[0] = (csqr[0] * s1i) + (csqi[0] * s1r);

                return;
            } // else
        } // if ((aa >= 0.0) && (zr > 0.0))

        if ( (kode != 2) && (aa <= ( -alim))) {
            aa = -aa + (0.25 * alaz);
            iflag = 1;
            sfac = tol;

            if (aa > elim) {
                nz[0] = 0;
                ierr[0] = 2;

                return;
            } // if (aa > elim)
        } // if ((kode != 2) && (aa <= (-alim)))

        // CBKNU and CACON return exp(zta)*K(fnu,zta) on kode = 2
        mr = 1;

        if (zi < 0.0) {
            mr = -1;
        } // if (zi < 0.0)

        zacai(ztar, ztai, fnu, kode, mr, 1, cyr, cyi, nn);

        if (nn[0] < 0) {

            if (nn[0] == -1) {
                nz[0] = 0;
                ierr[0] = 2;

                return;
            } // if (nn[0] == -1)

            nz[0] = 0;
            ierr[0] = 5;

            return;
        } // if (nn[0] < 0)

        nz[0] = nz[0] + nn[0];
        s1r = cyr[0] * coef;
        s1i = cyi[0] * coef;

        if (iflag != 0) {
            s1r = s1r * sfac;
            s1i = s1i * sfac;

            if (id == 1) {
                str[0] = - ( (s1r * zr) - (s1i * zi));
                s1i = - ( (s1r * zi) + (s1i * zr));
                s1r = str[0];
                air[0] = s1r / sfac;
                aii[0] = s1i / sfac;

                return;
            } // if (id == 1)
            else { // id != 1
                str[0] = (s1r * csqr[0]) - (s1i * csqi[0]);
                s1i = (s1r * csqi[0]) + (s1i * csqr[0]);
                s1r = str[0];
                air[0] = s1r / sfac;
                aii[0] = s1i / sfac;

                return;
            } // else id != 1
        } // if (iflag != 0)
        else if (id == 1) {
            air[0] = - ( (zr * s1r) - (zi * s1i));
            aii[0] = - ( (zr * s1i) + (zi * s1r));

            return;
        } // else if (id == 1)
        else {
            air[0] = (csqr[0] * s1r) - (csqi[0] * s1i);
            aii[0] = (csqr[0] * s1i) + (csqi[0] * s1r);

            return;
        } // else
    }

    /**
     * ZASYI COMPUTES THE I BESSEL FUNCTION FOR REAL(Z).GE.0.0 BY MEANS OF THE ASYMPTOTIC EXPANSION FOR LARGE CABS(Z) IN
     * THE REGION CABS(Z).GT.MAX(RL,fnu*fnu/2). NZ=0 IS A NORMAL RETURN. NZ.LT.0 INDICATES AN OVERFLOW ON
     * kode=UNSCALED_FUNCTION.
     * 
     * @param zr double
     * @param zi double
     * @param fnu double
     * @param kode int
     * @param n int
     * @param yr double[]
     * @param yi double[]
     * @param nz int[]
     */
    private void zasyi(final double zr, double zi, final double fnu, final int kode, final int n, final double[] yr,
            final double[] yi, final int[] nz) {
        final double rtpi = 0.159154943091895336;
        double az;
        double arm;
        double rtr1;
        int il;
        double dfnu;
        double raz;
        final double[] str = new double[1];
        final double[] sti = new double[1];
        final double[] ak1r = new double[1];
        final double[] ak1i = new double[1];
        double czr;
        double czi;
        double dnu2;
        int koded;
        double fdn;
        double ezr;
        double ezi;
        double aez;
        double s;
        int jl;
        double p1r;
        double p1i;
        int inu;
        double arg;
        double ak;
        double bk;
        int k;
        double sqk;
        double atol;
        double sgn;
        double cs1r;
        double cs1i;
        double cs2r;
        double cs2i;
        final double[] ckr = new double[1];
        final double[] cki = new double[1];
        double aa;
        double bb;
        double dkr;
        double dki;
        int j;
        double s2r;
        double s2i;
        double tzr;
        double tzi;
        int m;
        int nn;
        double rzr;
        double rzi;
        int ib;
        int i;

        nz[0] = 0;
        az = zabs(zr, zi);
        arm = 1.0E3 * tiny;
        rtr1 = Math.sqrt(arm);
        il = Math.min(2, n);
        dfnu = fnu + n - il;

        // Overflow test
        raz = 1.0 / az;
        str[0] = zr * raz;
        sti[0] = -zi * raz;
        ak1r[0] = rtpi * str[0] * raz;
        ak1i[0] = rtpi * sti[0] * raz;
        zsqrt(ak1r[0], ak1i[0], ak1r, ak1i);
        czr = zr;
        czi = zi;

        if (kode == 2) {
            czr = 0.0;
        }

        if (Math.abs(czr) > elim) {
            nz[0] = -1;

            return;
        }

        dnu2 = dfnu + dfnu;
        koded = 1;

        if ( (Math.abs(czr) <= alim) || (n <= 2)) {
            koded = 0;
            zexp(czr, czi, str, sti);
            zmlt(ak1r[0], ak1i[0], str[0], sti[0], ak1r, ak1i);
        }

        fdn = 0.0;

        if (dnu2 > rtr1) {
            fdn = dnu2 * dnu2;
        }

        ezr = 8.0 * zr;
        ezi = 8.0 * zi;

        // When z is imaginary, the error test must be made relative to the
        // first reciprocal power since this is the leading term of the
        // expansion for the imaginary part
        aez = 8.0 * az;
        s = tol / aez;
        jl = (int) (rl + rl) + 2;
        p1r = 0.0;
        p1i = 0.0;

        if (zi != 0.0) {

            // Calculate exp(PI*(0.5+fnu+n-il)*i) to minimize losses of
            // significance when fnu of n is large
            inu = (int) (fnu);
            arg = (fnu - inu) * Math.PI;
            inu = inu + n - il;
            ak = -Math.sin(arg);
            bk = Math.cos(arg);

            if (zi < 0.0) {
                bk = -bk;
            }

            p1r = ak;
            p1i = bk;

            if ( (inu % 2) == 1) {
                p1r = -p1r;
                p1i = -p1i;
            }
        } // if (zi != 0.0)

        for (k = 1; k <= il; k++) {
            sqk = fdn - 1.0;
            atol = s * Math.abs(sqk);
            sgn = 1.0;
            cs1r = 1.0;
            cs1i = 0.0;
            cs2r = 1.0;
            cs2i = 0.0;
            ckr[0] = 1.0;
            cki[0] = 0.0;
            ak = 0.0;
            aa = 1.0;
            bb = aez;
            dkr = ezr;
            dki = ezi;

            group: {

                for (j = 1; j <= jl; j++) {
                    zdiv(ckr[0], cki[0], dkr, dki, str, sti);
                    ckr[0] = str[0] * sqk;
                    cki[0] = sti[0] * sqk;
                    cs2r = cs2r + ckr[0];
                    cs2i = cs2i + cki[0];
                    sgn = -sgn;
                    cs1r = cs1r + (ckr[0] * sgn);
                    cs1i = cs1i + (cki[0] * sgn);
                    dkr = dkr + ezr;
                    dki = dki + ezi;
                    aa = aa * Math.abs(sqk) / bb;
                    bb = bb + aez;
                    ak = ak + 8.0;
                    sqk = sqk - ak;

                    if (aa <= atol) {
                        break group;
                    }
                } // for (j = 1; j <= jl; j++)

                nz[0] = -2;

                return;
            } // group

            s2r = cs1r;
            s2i = cs1i;

            if ( (zr + zr) < elim) {
                tzr = zr + zr;
                tzi = zi + zi;
                zexp( -tzr, -tzi, str, sti);
                zmlt(str[0], sti[0], p1r, p1i, str, sti);
                zmlt(str[0], sti[0], cs2r, cs2i, str, sti);
                s2r = s2r + str[0];
                s2i = s2i + sti[0];
            } // if ((zr+zr) < elim)

            fdn = fdn + (8.0 * dfnu) + 4.0;
            p1r = -p1r;
            p1i = -p1i;
            m = n - il + k;
            yr[m - 1] = (s2r * ak1r[0]) - (s2i * ak1i[0]);
            yi[m - 1] = (s2r * ak1i[0]) + (s2i * ak1r[0]);
        } // for (k = 1; k <= il; k++)

        if (n <= 2) {
            return;
        }

        nn = n;
        k = nn - 2;
        ak = k;
        str[0] = zr * raz;
        sti[0] = -zi * raz;
        rzr = (str[0] + str[0]) * raz;
        rzi = (sti[0] + sti[0]) * raz;
        ib = 3;

        for (i = ib; i <= nn; i++) {
            yr[k - 1] = ( (ak + fnu) * ( (rzr * yr[k]) - (rzi * yi[k]))) + yr[k + 1];
            yi[k - 1] = ( (ak + fnu) * ( (rzr * yi[k]) + (rzi * yr[k]))) + yi[k + 1];
            ak = ak - 1.0;
            k = k - 1;
        } // for (i = ib; i <= nn; i++)

        if (koded == 0) {
            return;
        }

        zexp(czr, czi, ckr, cki);

        for (i = 0; i < nn; i++) {
            str[0] = (yr[i] * ckr[0]) - (yi[i] * cki[0]);
            yi[i] = (yr[i] * cki[0]) + (yi[i] * ckr[0]);
            yr[i] = str[0];
        }

        return;
    }

    /**
     * DOCUMENT ME!
     * 
     * @param zr double
     * @param zi double
     * @param fnu double
     * @param kode int
     * @param m int
     * @param n int
     * @param cyr double[]
     * @param cyi double[]
     * @param nz int[]
     * @param ierr int[]
     */
    private void zbesh(final double zr, final double zi, final double fnu, final int kode, final int m, final int n,
            final double[] cyr, final double[] cyi, final int[] nz, final int[] ierr) {

        /**
         * ON KODE=1, ZBESH COMPUTES AN N MEMBER SEQUENCE OF COMPLEX C HANKEL (BESSEL) FUNCTIONS CY(J)=H(M,FNU+J-1,Z)
         * FOR KINDS M=1 C OR 2, REAL, NONNEGATIVE ORDERS FNU+J-1, J=1,...,N, AND COMPLEX C Z.NE.CMPLX(0.0,0.0) IN THE
         * CUT PLANE -PI.LT.ARG(Z).LE.PI. C ON KODE=2, ZBESH RETURNS THE SCALED HANKEL FUNCTIONS C C
         * CY(I)=EXP(-MM*Z*I)*H(M,FNU+J-1,Z) MM=3-2*M, I**2=-1. C C WHICH REMOVES THE EXPONENTIAL BEHAVIOR IN BOTH THE
         * UPPER AND C LOWER HALF PLANES. DEFINITIONS AND NOTATION ARE FOUND IN THE C NBS HANDBOOK OF MATHEMATICAL
         * FUNCTIONS (REF. 1). C C INPUT ZR,ZI,FNU ARE DOUBLE PRECISION C ZR,ZI - Z=CMPLX(ZR,ZI),
         * Z.NE.CMPLX(0.0D0,0.0D0), C -PT.LT.ARG(Z).LE.PI C FNU - ORDER OF INITIAL H FUNCTION, FNU.GE.0.0D0 C KODE - A
         * PARAMETER TO INDICATE THE SCALING OPTION C KODE= 1 RETURNS C CY(J)=H(M,FNU+J-1,Z), J=1,...,N C = 2 RETURNS C
         * CY(J)=H(M,FNU+J-1,Z)*EXP(-I*Z*(3-2M)) C J=1,...,N , I**2=-1 C M - KIND OF HANKEL FUNCTION, M=1 OR 2 C N -
         * NUMBER OF MEMBERS IN THE SEQUENCE, N.GE.1 C C OUTPUT CYR,CYI ARE DOUBLE PRECISION C CYR,CYI- DOUBLE PRECISION
         * VECTORS WHOSE FIRST N COMPONENTS C CONTAIN REAL AND IMAGINARY PARTS FOR THE SEQUENCE C CY(J)=H(M,FNU+J-1,Z)
         * OR C CY(J)=H(M,FNU+J-1,Z)*EXP(-I*Z*(3-2M)) J=1,...,N C DEPENDING ON KODE, I**2=-1. C NZ - NUMBER OF
         * COMPONENTS SET TO ZERO DUE TO UNDERFLOW, C NZ= 0 , NORMAL RETURN C NZ.GT.0 , FIRST NZ COMPONENTS OF CY SET TO
         * ZERO DUE C TO UNDERFLOW, CY(J)=CMPLX(0.0D0,0.0D0) C J=1,...,NZ WHEN Y.GT.0.0 AND M=1 OR C Y.LT.0.0 AND M=2.
         * FOR THE COMPLMENTARY C HALF PLANES, NZ STATES ONLY THE NUMBER C OF UNDERFLOWS. C IERR - ERROR FLAG C IERR=0,
         * NORMAL RETURN - COMPUTATION COMPLETED C IERR=1, INPUT ERROR - NO COMPUTATION C IERR=2, OVERFLOW - NO
         * COMPUTATION, FNU TOO C LARGE OR CABS(Z) TOO SMALL OR BOTH C IERR=3, CABS(Z) OR FNU+N-1 LARGE - COMPUTATION
         * DONE C BUT LOSSES OF SIGNIFCANCE BY ARGUMENT C REDUCTION PRODUCE LESS THAN HALF OF MACHINE C ACCURACY C
         * IERR=4, CABS(Z) OR FNU+N-1 TOO LARGE - NO COMPUTA- C TION BECAUSE OF COMPLETE LOSSES OF SIGNIFI- C CANCE BY
         * ARGUMENT REDUCTION C IERR=5, ERROR - NO COMPUTATION, C ALGORITHM TERMINATION CONDITION NOT MET C C***LONG
         * DESCRIPTION C C THE COMPUTATION IS CARRIED OUT BY THE RELATION C C
         * H(M,FNU,Z)=(1/MP)*EXP(-MP*FNU)*K(FNU,Z*EXP(-MP)) C MP=MM*HPI*I, MM=3-2*M, HPI=PI/2, I**2=-1 C C FOR M=1 OR 2
         * WHERE THE K BESSEL FUNCTION IS COMPUTED FOR THE C RIGHT HALF PLANE RE(Z).GE.0.0. THE K FUNCTION IS CONTINUED
         * C TO THE LEFT HALF PLANE BY THE RELATION C C K(FNU,Z*EXP(MP)) = EXP(-MP*FNU)*K(FNU,Z)-MP*I(FNU,Z) C
         * MP=MR*PI*I, MR=+1 OR -1, RE(Z).GT.0, I**2=-1 C C WHERE I(FNU,Z) IS THE I BESSEL FUNCTION. C C EXPONENTIAL
         * DECAY OF H(M,FNU,Z) OCCURS IN THE UPPER HALF Z C PLANE FOR M=1 AND THE LOWER HALF Z PLANE FOR M=2.
         * EXPONENTIAL C GROWTH OCCURS IN THE COMPLEMENTARY HALF PLANES. SCALING C BY EXP(-MM*Z*I) REMOVES THE
         * EXPONENTIAL BEHAVIOR IN THE C WHOLE Z PLANE FOR Z TO INFINITY. C C FOR NEGATIVE ORDERS,THE FORMULAE C C
         * H(1,-FNU,Z) = H(1,FNU,Z)*CEXP( PI*FNU*I) C H(2,-FNU,Z) = H(2,FNU,Z)*CEXP(-PI*FNU*I) C I**2=-1 C C CAN BE
         * USED. C C IN MOST COMPLEX VARIABLE COMPUTATION, ONE MUST EVALUATE ELE- C MENTARY FUNCTIONS. WHEN THE
         * MAGNITUDE OF Z OR FNU+N-1 IS C LARGE, LOSSES OF SIGNIFICANCE BY ARGUMENT REDUCTION OCCUR. C CONSEQUENTLY, IF
         * EITHER ONE EXCEEDS U1=SQRT(0.5/UR), THEN C LOSSES EXCEEDING HALF PRECISION ARE LIKELY AND AN ERROR FLAG C
         * IERR=3 IS TRIGGERED WHERE UR=DMAX1(D1MACH(4),1.0D-18) IS C DOUBLE PRECISION UNIT ROUNDOFF LIMITED TO 18
         * DIGITS PRECISION. C IF EITHER IS LARGER THAN U2=0.5/UR, THEN ALL SIGNIFICANCE IS C LOST AND IERR=4. IN ORDER
         * TO USE THE INT FUNCTION, ARGUMENTS C MUST BE FURTHER RESTRICTED NOT TO EXCEED THE LARGEST MACHINE C INTEGER,
         * U3=I1MACH(9). THUS, THE MAGNITUDE OF Z AND FNU+N-1 IS C RESTRICTED BY MIN(U2,U3). ON 32 BIT MACHINES, U1,U2,
         * AND U3 C ARE APPROXIMATELY 2.0E+3, 4.2E+6, 2.1E+9 IN SINGLE PRECISION C ARITHMETIC AND 1.3E+8, 1.8E+16,
         * 2.1E+9 IN DOUBLE PRECISION C ARITHMETIC RESPECTIVELY. THIS MAKES U2 AND U3 LIMITING IN C THEIR RESPECTIVE
         * ARITHMETICS. THIS MEANS THAT ONE CAN EXPECT C TO RETAIN, IN THE WORST CASES ON 32 BIT MACHINES, NO DIGITS C
         * IN SINGLE AND ONLY 7 DIGITS IN DOUBLE PRECISION ARITHMETIC. C SIMILAR CONSIDERATIONS HOLD FOR OTHER MACHINES.
         * C C THE APPROXIMATE RELATIVE ERROR IN THE MAGNITUDE OF A COMPLEX C BESSEL FUNCTION CAN BE EXPRESSED BY
         * P*10**S WHERE P=MAX(UNIT C ROUNDOFF,1.0D-18) IS THE NOMINAL PRECISION AND 10**S REPRE- C SENTS THE INCREASE
         * IN ERROR DUE TO ARGUMENT REDUCTION IN THE C ELEMENTARY FUNCTIONS. HERE, S=MAX(1,ABS(LOG10(CABS(Z))), C
         * ABS(LOG10(FNU))) APPROXIMATELY (I.E. S=MAX(1,ABS(EXPONENT OF C CABS(Z),ABS(EXPONENT OF FNU)) ). HOWEVER, THE
         * PHASE ANGLE MAY C HAVE ONLY ABSOLUTE ACCURACY. THIS IS MOST LIKELY TO OCCUR WHEN C ONE COMPONENT (IN ABSOLUTE
         * VALUE) IS LARGER THAN THE OTHER BY C SEVERAL ORDERS OF MAGNITUDE. IF ONE COMPONENT IS 10**K LARGER C THAN THE
         * OTHER, THEN ONE CAN EXPECT ONLY MAX(ABS(LOG10(P))-K, C 0) SIGNIFICANT DIGITS; OR, STATED ANOTHER WAY, WHEN K
         * EXCEEDS C THE EXPONENT OF P, NO SIGNIFICANT DIGITS REMAIN IN THE SMALLER C COMPONENT. HOWEVER, THE PHASE
         * ANGLE RETAINS ABSOLUTE ACCURACY C BECAUSE, IN COMPLEX ARITHMETIC WITH PRECISION P, THE SMALLER C COMPONENT
         * WILL NOT (AS A RULE) DECREASE BELOW P TIMES THE C MAGNITUDE OF THE LARGER COMPONENT. IN THESE EXTREME CASES,
         * C THE PRINCIPAL PHASE ANGLE IS ON THE ORDER OF +P, -P, PI/2-P, C OR -PI/2+P. C C***REFERENCES HANDBOOK OF
         * MATHEMATICAL FUNCTIONS BY M. ABRAMOWITZ C AND I. A. STEGUN, NBS AMS SERIES 55, U.S. DEPT. OF C COMMERCE,
         * 1955. C C COMPUTATION OF BESSEL FUNCTIONS OF COMPLEX ARGUMENT C BY D. E. AMOS, SAND83-0083, MAY, 1983. C C
         * COMPUTATION OF BESSEL FUNCTIONS OF COMPLEX ARGUMENT C AND LARGE ORDER BY D. E. AMOS, SAND83-0643, MAY, 1983 C
         * C A SUBROUTINE PACKAGE FOR BESSEL FUNCTIONS OF A COMPLEX C ARGUMENT AND NONNEGATIVE ORDER BY D. E. AMOS,
         * SAND85- C 1018, MAY, 1985 C C A PORTABLE PACKAGE FOR BESSEL FUNCTIONS OF A COMPLEX C ARGUMENT AND NONNEGATIVE
         * ORDER BY D. E. AMOS, ACM C TRANS. MATH. SOFTWARE, VOL. 12, NO. 3, SEPTEMBER 1986,C PP 265-273.
         */
        int nn;
        double neweps;
        int k;
        int k1;
        double aa;
        double dig;
        double hpi = Math.PI / 2.0;
        double fn;
        int mm;
        double znr;
        double zni;
        double az;
        double bb;
        double ufl;
        double arg;
        double aln;
        final int[] nuf = new int[1];
        int mr;
        final int[] nw = new int[1];
        double sgn;
        int inu;
        int inuh;
        int ir;
        double rhpi;
        double csgnr;
        double csgni;
        double zti;
        double rtol;
        double ascle;
        double atol;
        double str;
        double sti;
        double fmm;
        int i;

        nn = n;

        /**
         * SET PARAMETERS RELATED TO MACHINE CONSTANTS. C TOL IS THE APPROXIMATE UNIT ROUNDOFF LIMITED TO 1.0E-18. C
         * ELIM IS THE APPROXIMATE EXPONENTIAL OVER- AND UNDERFLOW LIMIT. C EXP(-ELIM).LT.EXP(-ALIM)=EXP(-ELIM)/TOL AND
         * C EXP(ELIM).GT.EXP(ALIM)=EXP(ELIM)*TOL ARE INTERVALS NEAR C UNDERFLOW AND OVERFLOW LIMITS WHERE SCALED
         * ARITHMETIC IS DONE. C RL IS THE LOWER BOUNDARY OF THE ASYMPTOTIC EXPANSION FOR LARGE Z. C DIG = NUMBER OF
         * BASE 10 DIGITS IN TOL = 10**(-DIG).C FNUL IS THE LOWER BOUNDARY OF THE ASYMPTOTIC SERIES FOR LARGE FNU.
         */
        // epsilon = D1MACH(4)
        // Machine epsilon is the smallest positive epsilon such that
        // (1.0 + epsilon) != 1.0.
        // epsilon = 2**(1 - doubleDigits) = 2**(1 - 53) = 2**(-52)
        // epsilon = 2.224460e-16
        // epsilon is called the largest relative spacing
        epsilon = 1.0;
        neweps = 1.0;

        while (true) {

            if (1.0 == (1.0 + neweps)) {
                break;
            } else {
                epsilon = neweps;
                neweps = neweps / 2.0;
            }
        }

        tol = Math.max(epsilon, 1.0e-18);

        // emin, the smallest exponent E for double precision, is I1MACH(15)
        // tiny = D1MACH(1) = 2**(emin - 1) = 2**(-1022)
        // D1MACH(1) is the smallest normalized number, which preserves the
        // full precision of the mantissa.
        // Double.MIN_VALUE = 2**(-1074) is the smallest denormalized number,
        // which preseerves only a portion of the fraction's precision.
        emin = -1021;
        tiny = Math.pow(2, -1022);

        // emax, the largest exponent E for double precision, is I1MACH(16)
        // D1MACH(2) = 2**(emax)*(1 - 2**(-doubleDigits)) = 2**1024*(1 - 2**-53)
        // D1MACH(2) = Double.MAX_VALUE
        emax = 1024;

        // r1m5 = log10(2), which is D1MACH(5)
        r1m5 = 0.434294481903251 * Math.log(2.0);
        k = Math.min(Math.abs(emin), Math.abs(emax));
        elim = 2.303 * ( (k * r1m5) - 3.0);

        // doubleDigits is I1MACH(14), the number of base-B digits for double
        // precision. IEEE 754 double precision has 64 bits with 1 sign bit,
        // 11 exponent bits, and 52 fraction, mantissa, or signficand bits.
        // Floating-point numbers are typically stored in normalized form,
        // with the radix point after the first non-zero digit. In base two,
        // the only possible non-zero digit is 1. Thus, a leading digit of 1
        // can always be assumed and need not be explicitly represented.
        // Therefore, the mantissa effectively has 53 bits of resolution.
        doubleDigits = 53;
        k1 = doubleDigits - 1;
        aa = r1m5 * k1;
        dig = Math.min(aa, 18.0);
        aa = 2.303 * aa;
        alim = elim + Math.max( -aa, -41.45);
        fnul = 10.0 + (6.0 * (dig - 3.0));
        rl = (1.2 * dig) + 3.0;
        fn = fnu + nn - 1.0;
        mm = 3 - m - m;
        fmm = mm;
        znr = fmm * zi;
        zni = -fmm * zr;

        // Test for proper range
        az = zabs(zr, zi);
        aa = 0.5 / tol;
        bb = 0.5 * Integer.MAX_VALUE;
        aa = Math.min(aa, bb);

        if ( (az > aa) || (fn > aa)) {
            nz[0] = 0;
            ierr[0] = 4;

            return;
        } // if ((az > aa) || (fn > aa))

        aa = Math.sqrt(aa);

        if ( (az > aa) || (fn > aa)) {
            ierr[0] = 3;
        } // if ((az > aa) || (fn > aa))

        // Overflow test on the last member of the sequence
        ufl = 1.0E3 * tiny;

        if (az < ufl) {
            nz[0] = 0;
            ierr[0] = 2;

            return;
        } // if (az < ufl)

        if (fnu <= fnul) {

            if ( (fn > 1.0) && (fn <= 2.0) && (az <= tol)) {
                arg = 0.5 * az;
                aln = -fn * Math.log(arg);

                if (aln > elim) {
                    nz[0] = 0;
                    ierr[0] = 2;

                    return;
                } // if (aln > elim)
            } // if ((fn > 1.0) && (fn <= 2.0) && (az <= tol))
            else if (fn > 2.0) {
                zuoik(znr, zni, fnu, kode, 2, nn, cyr, cyi, nuf);

                if (nuf[0] < 0) {
                    nz[0] = 0;
                    ierr[0] = 2;

                    return;
                } // if (nuf[0] < 0)

                nz[0] = nz[0] + nuf[0];
                nn = nn - nuf[0];

                // Here nn = n or nn = 0 since nuf = 0, nn, or -1 on return from
                // zuoik. If nuf =nn, then cy[i] = czero for all i
                if (nn == 0) {

                    if (znr < 0.0) {
                        nz[0] = 0;
                        ierr[0] = 2;
                    } // if (znr < 0.0)

                    return;
                } // if (nn == 0)
            } // else if (fn > 2.0)

            if ( (znr >= 0.0) && ( (znr != 0.0) || (zni >= 0.0) || (m != 2))) {

                // Right half plane computation
                zbknu(znr, zni, fnu, kode, nn, cyr, cyi, nz);
            } // if ((znr >= 0.0) && ((znr != 0.0) || (zni >= 0.0) || (m != 2)))
            else {

                // Left half plane computation
                mr = -mm;
                zacon(znr, zni, fnu, kode, mr, nn, cyr, cyi, nw);

                if (nw[0] == -1) {
                    nz[0] = 0;
                    ierr[0] = 2;

                    return;
                } // if (nw[0] == -1)
                else if (nw[0] < 0) {
                    nz[0] = 0;
                    ierr[0] = 5;

                    return;
                } // else if (nw[0] < 0)

                nz[0] = nw[0];
            } // else
        } // if (fnu <= fnul)
        else { // fnu > fnul

            // Uniform asymptotic expansions for fnu > fnul
            mr = 0;

            if ( (znr < 0.0) || ( (znr == 0.0) && (zni < 0.0) && (m == 2))) {
                mr = -mm;

                if ( (znr == 0.0) && (zni < 0.0)) {
                    znr = -znr;
                    zni = -zni;
                } // if ((znr == 0.0) && (zni < 0.0)
            } // if ((znr < 0.0) || ((znr == 0.0) && (zni < 0.0) && (m == 2)))

            zbunk(znr, zni, fnu, kode, mr, nn, cyr, cyi, nw);

            if (nw[0] == -1) {
                nz[0] = 0;
                ierr[0] = 2;

                return;
            } // if (nw[0] == -1)
            else if (nw[0] < 0) {
                nz[0] = 0;
                ierr[0] = 5;

                return;
            } // else if (nw[0] < 0)

            nz[0] = nz[0] + nw[0];
        } // else fnu > fnul

        // H(m,fnu,z) = -fmm*(i/hpi)*(zt**fnu)*K(fnu,-z*zt)
        // zt = exp(-fmm*hpi*i) = cmplx(0.0,-fmm), fmm = 3 - 2*m, m = 1,2
        if ( -fmm >= 0.0) {
            sgn = hpi;
        } else {
            sgn = -hpi;
        }

        // Caclulate exp(fnu*hpi*i) to minimize losses of significance when
        // fnu is large
        inu = (int) (fnu);
        inuh = inu / 2;
        ir = inu - (2 * inuh);
        arg = (fnu - (inu - ir)) * sgn;
        rhpi = 1.0 / sgn;
        csgni = rhpi * Math.cos(arg);
        csgnr = -rhpi * Math.sin(arg);

        if ( (inuh % 2) != 0) {
            csgnr = -csgnr;
            csgni = -csgni;
        } // if ((inuh%2) != 0)

        zti = -fmm;
        rtol = 1.0 / tol;
        ascle = ufl * rtol;

        for (i = 1; i <= nn; i++) {
            aa = cyr[i - 1];
            bb = cyi[i - 1];
            atol = 1.0;

            if (Math.max(Math.abs(aa), Math.abs(bb)) <= ascle) {
                aa = aa * rtol;
                bb = bb * rtol;
                atol = tol;
            } // if (Math.max(Math.abs(aa),Math.abs(bb)) <= ascle)

            str = (aa * csgnr) - (bb * csgni);
            sti = (aa * csgni) + (bb * csgnr);
            cyr[i - 1] = str * atol;
            cyi[i - 1] = sti * atol;
            str = -csgni * zti;
            csgni = csgnr * zti;
            csgnr = str;
        } // for (i = 1; i <= nn; i++)

        return;
    }

    /**
     * DOCUMENT ME!
     * 
     * @param zr double
     * @param zi double
     * @param fnu double
     * @param kode int
     * @param n int
     * @param cyr double[]
     * @param cyi double[]
     * @param nz int[]
     * @param ierr int[]
     */
    private void zbesi(double zr, double zi, final double fnu, final int kode, final int n, final double[] cyr,
            final double[] cyi, final int[] nz, final int[] ierr) {

        /**
         * LONG DESCRIPTION C C THE COMPUTATION IS CARRIED OUT BY THE POWER SERIES FOR C SMALL CABS(Z), THE ASYMPTOTIC
         * EXPANSION FOR LARGE CABS(Z), C THE MILLER ALGORITHM NORMALIZED BY THE WRONSKIAN AND A C NEUMANN SERIES FOR
         * IMTERMEDIATE MAGNITUDES, AND THE C UNIFORM ASYMPTOTIC EXPANSIONS FOR I(FNU,Z) AND J(FNU,Z) C FOR LARGE
         * ORDERS. BACKWARD RECURRENCE IS USED TO GENERATE C SEQUENCES OR REDUCE ORDERS WHEN NECESSARY. C C THE
         * CALCULATIONS ABOVE ARE DONE IN THE RIGHT HALF PLANE AND C CONTINUED INTO THE LEFT HALF PLANE BY THE FORMULA C
         * C I(FNU,Z*EXP(M*PI)) = EXP(M*PI*FNU)*I(FNU,Z) REAL(Z).GT.0.0 C M = +I OR -I, I**2=-1 C C FOR NEGATIVE
         * ORDERS,THE FORMULA C C I(-FNU,Z) = I(FNU,Z) + (2/PI)*SIN(PI*FNU)*K(FNU,Z) C C CAN BE USED. HOWEVER,FOR LARGE
         * ORDERS CLOSE TO INTEGERS, THE C THE FUNCTION CHANGES RADICALLY. WHEN FNU IS A LARGE POSITIVE C INTEGER,THE
         * MAGNITUDE OF I(-FNU,Z)=I(FNU,Z) IS A LARGE C NEGATIVE POWER OF TEN. BUT WHEN FNU IS NOT AN INTEGER, C
         * K(FNU,Z) DOMINATES IN MAGNITUDE WITH A LARGE POSITIVE POWER OF C TEN AND THE MOST THAT THE SECOND TERM CAN BE
         * REDUCED IS BY C UNIT ROUNDOFF FROM THE COEFFICIENT. THUS, WIDE CHANGES CAN C OCCUR WITHIN UNIT ROUNDOFF OF A
         * LARGE INTEGER FOR FNU. HERE, C LARGE MEANS FNU.GT.CABS(Z). C C IN MOST COMPLEX VARIABLE COMPUTATION, ONE MUST
         * EVALUATE ELE- C MENTARY FUNCTIONS. WHEN THE MAGNITUDE OF Z OR FNU+N-1 IS C LARGE, LOSSES OF SIGNIFICANCE BY
         * ARGUMENT REDUCTION OCCUR. C CONSEQUENTLY, IF EITHER ONE EXCEEDS U1=SQRT(0.5/UR), THEN C LOSSES EXCEEDING HALF
         * PRECISION ARE LIKELY AND AN ERROR FLAG C IERR=3 IS TRIGGERED WHERE UR=DMAX1(D1MACH(4),1.0D-18) IS C DOUBLE
         * PRECISION UNIT ROUNDOFF LIMITED TO 18 DIGITS PRECISION. C IF EITHER IS LARGER THAN U2=0.5/UR, THEN ALL
         * SIGNIFICANCE IS C LOST AND IERR=4. IN ORDER TO USE THE INT FUNCTION, ARGUMENTS C MUST BE FURTHER RESTRICTED
         * NOT TO EXCEED THE LARGEST MACHINE C INTEGER, U3=I1MACH(9). THUS, THE MAGNITUDE OF Z AND FNU+N-1 IS C
         * RESTRICTED BY MIN(U2,U3). ON 32 BIT MACHINES, U1,U2, AND U3 C ARE APPROXIMATELY 2.0E+3, 4.2E+6, 2.1E+9 IN
         * SINGLE PRECISION C ARITHMETIC AND 1.3E+8, 1.8E+16, 2.1E+9 IN DOUBLE PRECISION C ARITHMETIC RESPECTIVELY. THIS
         * MAKES U2 AND U3 LIMITING IN C THEIR RESPECTIVE ARITHMETICS. THIS MEANS THAT ONE CAN EXPECT C TO RETAIN, IN
         * THE WORST CASES ON 32 BIT MACHINES, NO DIGITS C IN SINGLE AND ONLY 7 DIGITS IN DOUBLE PRECISION ARITHMETIC. C
         * SIMILAR CONSIDERATIONS HOLD FOR OTHER MACHINES. C C THE APPROXIMATE RELATIVE ERROR IN THE MAGNITUDE OF A
         * COMPLEX C BESSEL FUNCTION CAN BE EXPRESSED BY P*10**S WHERE P=MAX(UNIT C ROUNDOFF,1.0E-18) IS THE NOMINAL
         * PRECISION AND 10**S REPRE- C SENTS THE INCREASE IN ERROR DUE TO ARGUMENT REDUCTION IN THE C ELEMENTARY
         * FUNCTIONS. HERE, S=MAX(1,ABS(LOG10(CABS(Z))), C ABS(LOG10(FNU))) APPROXIMATELY (I.E. S=MAX(1,ABS(EXPONENT OF
         * C CABS(Z),ABS(EXPONENT OF FNU)) ). HOWEVER, THE PHASE ANGLE MAY C HAVE ONLY ABSOLUTE ACCURACY. THIS IS MOST
         * LIKELY TO OCCUR WHEN C ONE COMPONENT (IN ABSOLUTE VALUE) IS LARGER THAN THE OTHER BY C SEVERAL ORDERS OF
         * MAGNITUDE. IF ONE COMPONENT IS 10**K LARGER C THAN THE OTHER, THEN ONE CAN EXPECT ONLY MAX(ABS(LOG10(P))-K, C
         * 0) SIGNIFICANT DIGITS; OR, STATED ANOTHER WAY, WHEN K EXCEEDS C THE EXPONENT OF P, NO SIGNIFICANT DIGITS
         * REMAIN IN THE SMALLER C COMPONENT. HOWEVER, THE PHASE ANGLE RETAINS ABSOLUTE ACCURACY C BECAUSE, IN COMPLEX
         * ARITHMETIC WITH PRECISION P, THE SMALLER C COMPONENT WILL NOT (AS A RULE) DECREASE BELOW P TIMES THE C
         * MAGNITUDE OF THE LARGER COMPONENT. IN THESE EXTREME CASES, C THE PRINCIPAL PHASE ANGLE IS ON THE ORDER OF +P,
         * -P, PI/2-P,C OR -PI/2+P.
         */
        double neweps;
        int k;
        int k1;
        double aa;
        double dig;
        double az;
        double fn;
        double bb;
        double znr;
        double zni;
        double csgnr;
        double csgni;
        int inu;
        double arg;
        int nn;
        double rtol;
        double ascle;
        double atol;
        double str;
        double sti;
        int i;

        /**
         * SET PARAMETERS RELATED TO MACHINE CONSTANTS. C TOL IS THE APPROXIMATE UNIT ROUNDOFF LIMITED TO 1.0E-18. C
         * ELIM IS THE APPROXIMATE EXPONENTIAL OVER- AND UNDERFLOW LIMIT. C EXP(-ELIM).LT.EXP(-ALIM)=EXP(-ELIM)/TOL AND
         * C EXP(ELIM).GT.EXP(ALIM)=EXP(ELIM)*TOL ARE INTERVALS NEAR C UNDERFLOW AND OVERFLOW LIMITS WHERE SCALED
         * ARITHMETIC IS DONE. C RL IS THE LOWER BOUNDARY OF THE ASYMPTOTIC EXPANSION FOR LARGE Z. C DIG = NUMBER OF
         * BASE 10 DIGITS IN TOL = 10**(-DIG). C FNUL IS THE LOWER BOUNDARY OF THE ASYMPTOTIC SERIES FOR LARGE FNU.
         */
        // epsilon = D1MACH(4)
        // Machine epsilon is the smallest positive epsilon such that
        // (1.0 + epsilon) != 1.0.
        // epsilon = 2**(1 - doubleDigits) = 2**(1 - 53) = 2**(-52)
        // epsilon = 2.224460e-16
        // epsilon is called the largest relative spacing
        epsilon = 1.0;
        neweps = 1.0;

        while (true) {

            if (1.0 == (1.0 + neweps)) {
                break;
            } else {
                epsilon = neweps;
                neweps = neweps / 2.0;
            }
        }

        tol = Math.max(epsilon, 1.0e-18);

        // emin, the smallest exponent E for double precision, is I1MACH(15)
        // tiny = D1MACH(1) = 2**(emin - 1) = 2**(-1022)
        // D1MACH(1) is the smallest normalized number, which preserves the
        // full precision of the mantissa.
        // Double.MIN_VALUE = 2**(-1074) is the smallest denormalized number,
        // which preseerves only a portion of the fraction's precision.
        emin = -1021;
        tiny = Math.pow(2, -1022);

        // emax, the largest exponent E for double precision, is I1MACH(16)
        // D1MACH(2) = 2**(emax)*(1 - 2**(-doubleDigits)) = 2**1024*(1 - 2**-53)
        // D1MACH(2) = Double.MAX_VALUE
        emax = 1024;

        // r1m5 = log10(2), which is D1MACH(5)
        r1m5 = 0.434294481903251 * Math.log(2.0);
        k = Math.min(Math.abs(emin), Math.abs(emax));
        elim = 2.303 * ( (k * r1m5) - 3.0);

        // doubleDigits is I1MACH(14), the number of base-B digits for double
        // precision. IEEE 754 double precision has 64 bits with 1 sign bit,
        // 11 exponent bits, and 52 fraction, mantissa, or signficand bits.
        // Floating-point numbers are typically stored in normalized form,
        // with the radix point after the first non-zero digit. In base two,
        // the only possible non-zero digit is 1. Thus, a leading digit of 1
        // can always be assumed and need not be explicitly represented.
        // Therefore, the mantissa effectively has 53 bits of resolution.
        doubleDigits = 53;
        k1 = doubleDigits - 1;
        aa = r1m5 * k1;
        dig = Math.min(aa, 18.0);
        aa = 2.303 * aa;
        alim = elim + Math.max( -aa, -41.45);
        rl = (1.2 * dig) + 3.0;
        fnul = 10.0 + (6.0 * (dig - 3.0));

        // test for proper range
        az = zabs(zr, zi);
        fn = fnu + n - 1.0;
        aa = 0.5 / tol;

        // I1MACH(9) = Integer.MAX_VALUE;
        bb = 0.5 * Integer.MAX_VALUE;
        aa = Math.min(aa, bb);

        if ( (az > aa) || (fn > aa)) {
            nz[0] = 0;
            ierr[0] = 4;

            return;
        }

        aa = Math.sqrt(aa);

        if ( (az > aa) || (fn > aa)) {
            ierr[0] = 3;
        }

        znr = zr;
        zni = zi;
        csgnr = 1.0;
        csgni = 0.0;

        if (zr < 0.0) {
            znr = -zr;
            zni = -zi;

            // Calculate csgn = exp(fnu * PI* i) to minimize losses
            // of significance when fnu is large
            inu = (int) fnu;
            arg = Math.PI * (fnu - inu);

            if (zi < 0.0) {
                arg = -arg;
            }

            csgnr = Math.cos(arg);
            csgni = Math.sin(arg);

            if ( (inu % 2) != 0) {
                csgnr = -csgnr;
                csgni = -csgni;
            }
        } // if (zr < 0.0)

        zbinu(znr, zni, fnu, kode, n, cyr, cyi, nz);

        if (nz[0] == -2) {
            nz[0] = 0;
            ierr[0] = 5;

            return;
        } else if (nz[0] < 0) {
            nz[0] = 0;
            ierr[0] = 2;

            return;
        } else if (zr >= 0.0) {
            return;
        }

        // Analytic continuation to the left half plane
        nn = n - nz[0];

        if (nn == 0) {
            return;
        }

        rtol = 1.0 / tol;
        ascle = tiny * rtol * 1.0e3;

        for (i = 0; i < nn; i++) {
            aa = cyr[i];
            bb = cyi[i];
            atol = 1.0;

            if (Math.max(Math.abs(aa), Math.abs(bb)) <= ascle) {
                aa = aa * rtol;
                bb = bb * rtol;
                atol = tol;
            }

            str = (aa * csgnr) - (bb * csgni);
            sti = (aa * csgni) + (bb * csgnr);
            cyr[i] = str * atol;
            cyi[i] = sti * atol;
            csgnr = -csgnr;
            csgni = -csgni;
        } // for (i = 0; i < nn; i++)

        return;
    }

    /**
     * DOCUMENT ME!
     * 
     * @param zr double
     * @param zi double
     * @param fnu double
     * @param kode int
     * @param n int
     * @param cyr double[]
     * @param cyi double[]
     * @param nz int[]
     * @param ierr int[]
     */
    private void zbesj(double zr, final double zi, final double fnu, final int kode, final int n, final double[] cyr,
            final double[] cyi, final int[] nz, final int[] ierr) {

        /**
         * *BEGIN PROLOGUE ZBESJ C***DATE WRITTEN 830501 (YYMMDD) C***REVISION DATE 890801, 930101 (YYMMDD) C***CATEGORY
         * NO. B5K C***KEYWORDS J-BESSEL FUNCTION,BESSEL FUNCTION OF COMPLEX ARGUMENT, C BESSEL FUNCTION OF FIRST KIND
         * C***AUTHOR AMOS, DONALD E., SANDIA NATIONAL LABORATORIES C***PURPOSE TO COMPUTE THE J-BESSEL FUNCTION OF A
         * COMPLEX ARGUMENT C***DESCRIPTION C C ***A DOUBLE PRECISION ROUTINE*** C ON KODE=1, ZBESJ COMPUTES AN N MEMBER
         * SEQUENCE OF COMPLEX C BESSEL FUNCTIONS CY(I)=J(FNU+I-1,Z) FOR REAL, NONNEGATIVE C ORDERS FNU+I-1, I=1,...,N
         * AND COMPLEX Z IN THE CUT PLANE C -PI.LT.ARG(Z).LE.PI. ON KODE=2, ZBESJ RETURNS THE SCALED C FUNCTIONS C C
         * CY(I)=EXP(-ABS(Y))*J(FNU+I-1,Z) I = 1,...,N , Y=AIMAG(Z) C C WHICH REMOVE THE EXPONENTIAL GROWTH IN BOTH THE
         * UPPER AND C LOWER HALF PLANES FOR Z TO INFINITY. DEFINITIONS AND NOTATION C ARE FOUND IN THE NBS HANDBOOK OF
         * MATHEMATICAL FUNCTIONS C (REF. 1). C C INPUT ZR,ZI,FNU ARE DOUBLE PRECISION C ZR,ZI - Z=CMPLX(ZR,ZI),
         * -PI.LT.ARG(Z).LE.PI C FNU - ORDER OF INITIAL J FUNCTION, FNU.GE.0.0D0 C KODE - A PARAMETER TO INDICATE THE
         * SCALING OPTION C KODE= 1 RETURNS C CY(I)=J(FNU+I-1,Z), I=1,...,N C = 2 RETURNS C
         * CY(I)=J(FNU+I-1,Z)EXP(-ABS(Y)), I=1,...,N C N - NUMBER OF MEMBERS OF THE SEQUENCE, N.GE.1 C C OUTPUT CYR,CYI
         * ARE DOUBLE PRECISION C CYR,CYI- DOUBLE PRECISION VECTORS WHOSE FIRST N COMPONENTS C CONTAIN REAL AND
         * IMAGINARY PARTS FOR THE SEQUENCE C CY(I)=J(FNU+I-1,Z) OR C CY(I)=J(FNU+I-1,Z)EXP(-ABS(Y)) I=1,...,N C
         * DEPENDING ON KODE, Y=AIMAG(Z). C NZ - NUMBER OF COMPONENTS SET TO ZERO DUE TO UNDERFLOW, C NZ= 0 , NORMAL
         * RETURN C NZ.GT.0 , LAST NZ COMPONENTS OF CY SET ZERO DUE C TO UNDERFLOW, CY(I)=CMPLX(0.0D0,0.0D0), C I =
         * N-NZ+1,...,N C IERR - ERROR FLAG C IERR=0, NORMAL RETURN - COMPUTATION COMPLETED C IERR=1, INPUT ERROR - NO
         * COMPUTATION C IERR=2, OVERFLOW - NO COMPUTATION, AIMAG(Z) C TOO LARGE ON KODE=1 C IERR=3, CABS(Z) OR FNU+N-1
         * LARGE - COMPUTATION DONE C BUT LOSSES OF SIGNIFCANCE BY ARGUMENT C REDUCTION PRODUCE LESS THAN HALF OF
         * MACHINE C ACCURACY C IERR=4, CABS(Z) OR FNU+N-1 TOO LARGE - NO COMPUTA- C TION BECAUSE OF COMPLETE LOSSES OF
         * SIGNIFI- C CANCE BY ARGUMENT REDUCTION C IERR=5, ERROR - NO COMPUTATION, C ALGORITHM TERMINATION CONDITION
         * NOT MET C C***LONG DESCRIPTION C C THE COMPUTATION IS CARRIED OUT BY THE FORMULA C C J(FNU,Z)=EXP(
         * FNU*PI*I/2)*I(FNU,-I*Z) AIMAG(Z).GE.0.0 C C J(FNU,Z)=EXP(-FNU*PI*I/2)*I(FNU, I*Z) AIMAG(Z).LT.0.0 C C WHERE
         * I**2 = -1 AND I(FNU,Z) IS THE I BESSEL FUNCTION. C C FOR NEGATIVE ORDERS,THE FORMULA C C J(-FNU,Z) =
         * J(FNU,Z)*COS(PI*FNU) - Y(FNU,Z)*SIN(PI*FNU) C C CAN BE USED. HOWEVER,FOR LARGE ORDERS CLOSE TO INTEGERS, THE
         * C THE FUNCTION CHANGES RADICALLY. WHEN FNU IS A LARGE POSITIVE C INTEGER,THE MAGNITUDE OF
         * J(-FNU,Z)=J(FNU,Z)*COS(PI*FNU) IS A C LARGE NEGATIVE POWER OF TEN. BUT WHEN FNU IS NOT AN INTEGER, C Y(FNU,Z)
         * DOMINATES IN MAGNITUDE WITH A LARGE POSITIVE POWER OF C TEN AND THE MOST THAT THE SECOND TERM CAN BE REDUCED
         * IS BY C UNIT ROUNDOFF FROM THE COEFFICIENT. THUS, WIDE CHANGES CAN C OCCUR WITHIN UNIT ROUNDOFF OF A LARGE
         * INTEGER FOR FNU. HERE, C LARGE MEANS FNU.GT.CABS(Z). C C IN MOST COMPLEX VARIABLE COMPUTATION, ONE MUST
         * EVALUATE ELE- C MENTARY FUNCTIONS. WHEN THE MAGNITUDE OF Z OR FNU+N-1 IS C LARGE, LOSSES OF SIGNIFICANCE BY
         * ARGUMENT REDUCTION OCCUR. C CONSEQUENTLY, IF EITHER ONE EXCEEDS U1=SQRT(0.5/UR), THEN C LOSSES EXCEEDING HALF
         * PRECISION ARE LIKELY AND AN ERROR FLAG C IERR=3 IS TRIGGERED WHERE UR=DMAX1(D1MACH(4),1.0D-18) IS C DOUBLE
         * PRECISION UNIT ROUNDOFF LIMITED TO 18 DIGITS PRECISION. C IF EITHER IS LARGER THAN U2=0.5/UR, THEN ALL
         * SIGNIFICANCE IS C LOST AND IERR=4. IN ORDER TO USE THE INT FUNCTION, ARGUMENTS C MUST BE FURTHER RESTRICTED
         * NOT TO EXCEED THE LARGEST MACHINE C INTEGER, U3=I1MACH(9). THUS, THE MAGNITUDE OF Z AND FNU+N-1 IS C
         * RESTRICTED BY MIN(U2,U3). ON 32 BIT MACHINES, U1,U2, AND U3 C ARE APPROXIMATELY 2.0E+3, 4.2E+6, 2.1E+9 IN
         * SINGLE PRECISION C ARITHMETIC AND 1.3E+8, 1.8E+16, 2.1E+9 IN DOUBLE PRECISION C ARITHMETIC RESPECTIVELY. THIS
         * MAKES U2 AND U3 LIMITING IN C THEIR RESPECTIVE ARITHMETICS. THIS MEANS THAT ONE CAN EXPECT C TO RETAIN, IN
         * THE WORST CASES ON 32 BIT MACHINES, NO DIGITS C IN SINGLE AND ONLY 7 DIGITS IN DOUBLE PRECISION ARITHMETIC. C
         * SIMILAR CONSIDERATIONS HOLD FOR OTHER MACHINES. C C THE APPROXIMATE RELATIVE ERROR IN THE MAGNITUDE OF A
         * COMPLEX C BESSEL FUNCTION CAN BE EXPRESSED BY P*10**S WHERE P=MAX(UNIT C ROUNDOFF,1.0E-18) IS THE NOMINAL
         * PRECISION AND 10**S REPRE- C SENTS THE INCREASE IN ERROR DUE TO ARGUMENT REDUCTION IN THE C ELEMENTARY
         * FUNCTIONS. HERE, S=MAX(1,ABS(LOG10(CABS(Z))), C ABS(LOG10(FNU))) APPROXIMATELY (I.E. S=MAX(1,ABS(EXPONENT OF
         * C CABS(Z),ABS(EXPONENT OF FNU)) ). HOWEVER, THE PHASE ANGLE MAY C HAVE ONLY ABSOLUTE ACCURACY. THIS IS MOST
         * LIKELY TO OCCUR WHEN C ONE COMPONENT (IN ABSOLUTE VALUE) IS LARGER THAN THE OTHER BY C SEVERAL ORDERS OF
         * MAGNITUDE. IF ONE COMPONENT IS 10**K LARGER C THAN THE OTHER, THEN ONE CAN EXPECT ONLY MAX(ABS(LOG10(P))-K, C
         * 0) SIGNIFICANT DIGITS; OR, STATED ANOTHER WAY, WHEN K EXCEEDS C THE EXPONENT OF P, NO SIGNIFICANT DIGITS
         * REMAIN IN THE SMALLER C COMPONENT. HOWEVER, THE PHASE ANGLE RETAINS ABSOLUTE ACCURACY C BECAUSE, IN COMPLEX
         * ARITHMETIC WITH PRECISION P, THE SMALLER C COMPONENT WILL NOT (AS A RULE) DECREASE BELOW P TIMES THE C
         * MAGNITUDE OF THE LARGER COMPONENT. IN THESE EXTREME CASES, C THE PRINCIPAL PHASE ANGLE IS ON THE ORDER OF +P,
         * -P, PI/2-P, C OR -PI/2+P. C C***REFERENCES HANDBOOK OF MATHEMATICAL FUNCTIONS BY M. ABRAMOWITZ C AND I. A.
         * STEGUN, NBS AMS SERIES 55, U.S. DEPT. OF C COMMERCE, 1955. C C COMPUTATION OF BESSEL FUNCTIONS OF COMPLEX
         * ARGUMENT C BY D. E. AMOS, SAND83-0083, MAY, 1983. C C COMPUTATION OF BESSEL FUNCTIONS OF COMPLEX ARGUMENT C
         * AND LARGE ORDER BY D. E. AMOS, SAND83-0643, MAY, 1983 C C A SUBROUTINE PACKAGE FOR BESSEL FUNCTIONS OF A
         * COMPLEX C ARGUMENT AND NONNEGATIVE ORDER BY D. E. AMOS, SAND85- C 1018, MAY, 1985 C C A PORTABLE PACKAGE FOR
         * BESSEL FUNCTIONS OF A COMPLEX C ARGUMENT AND NONNEGATIVE ORDER BY D. E. AMOS, ACM C TRANS. MATH. SOFTWARE,
         * VOL. 12, NO. 3, SEPTEMBER 1986, C PP 265-273.
         */
        double neweps;
        int k;
        int k1;
        double aa;
        double dig;
        double az;
        double fn;
        double bb;
        double znr;
        double zni;
        double csgnr;
        double csgni;
        int inu;
        double arg;
        double cii;
        int inuh;
        int ir;
        int nl;
        double rtol;
        double ascle;
        double str;
        double sti;
        final double hpi = Math.PI / 2.0;
        int i;
        double atol;

        /**
         * SET PARAMETERS RELATED TO MACHINE CONSTANTS. C TOL IS THE APPROXIMATE UNIT ROUNDOFF LIMITED TO 1.0E-18. C
         * ELIM IS THE APPROXIMATE EXPONENTIAL OVER- AND UNDERFLOW LIMIT. C EXP(-ELIM).LT.EXP(-ALIM)=EXP(-ELIM)/TOL AND
         * C EXP(ELIM).GT.EXP(ALIM)=EXP(ELIM)*TOL ARE INTERVALS NEAR C UNDERFLOW AND OVERFLOW LIMITS WHERE SCALED
         * ARITHMETIC IS DONE. C RL IS THE LOWER BOUNDARY OF THE ASYMPTOTIC EXPANSION FOR LARGE Z. C DIG = NUMBER OF
         * BASE 10 DIGITS IN TOL = 10**(-DIG).C FNUL IS THE LOWER BOUNDARY OF THE ASYMPTOTIC SERIES FOR LARGE FNU.
         */
        // epsilon = D1MACH(4)
        // Machine epsilon is the smallest positive epsilon such that
        // (1.0 + epsilon) != 1.0.
        // epsilon = 2**(1 - doubleDigits) = 2**(1 - 53) = 2**(-52)
        // epsilon = 2.224460e-16
        // epsilon is called the largest relative spacing
        epsilon = 1.0;
        neweps = 1.0;

        while (true) {

            if (1.0 == (1.0 + neweps)) {
                break;
            } else {
                epsilon = neweps;
                neweps = neweps / 2.0;
            }
        }

        tol = Math.max(epsilon, 1.0e-18);

        // emin, the smallest exponent E for double precision, is I1MACH(15)
        // tiny = D1MACH(1) = 2**(emin - 1) = 2**(-1022)
        // D1MACH(1) is the smallest normalized number, which preserves the
        // full precision of the mantissa.
        // Double.MIN_VALUE = 2**(-1074) is the smallest denormalized number,
        // which preseerves only a portion of the fraction's precision.
        emin = -1021;
        tiny = Math.pow(2, -1022);

        // emax, the largest exponent E for double precision, is I1MACH(16)
        // D1MACH(2) = 2**(emax)*(1 - 2**(-doubleDigits)) = 2**1024*(1 - 2**-53)
        // D1MACH(2) = Double.MAX_VALUE
        emax = 1024;

        // r1m5 = log10(2), which is D1MACH(5)
        r1m5 = 0.434294481903251 * Math.log(2.0);
        k = Math.min(Math.abs(emin), Math.abs(emax));
        elim = 2.303 * ( (k * r1m5) - 3.0);

        // doubleDigits is I1MACH(14), the number of base-B digits for double
        // precision. IEEE 754 double precision has 64 bits with 1 sign bit,
        // 11 exponent bits, and 52 fraction, mantissa, or signficand bits.
        // Floating-point numbers are typically stored in normalized form,
        // with the radix point after the first non-zero digit. In base two,
        // the only possible non-zero digit is 1. Thus, a leading digit of 1
        // can always be assumed and need not be explicitly represented.
        // Therefore, the mantissa effectively has 53 bits of resolution.
        doubleDigits = 53;
        k1 = doubleDigits - 1;
        aa = r1m5 * k1;
        dig = Math.min(aa, 18.0);
        aa = 2.303 * aa;
        alim = elim + Math.max( -aa, -41.45);
        rl = (1.2 * dig) + 3.0;
        fnul = 10.0 + (6.0 * (dig - 3.0));

        // test for proper range
        az = zabs(zr, zi);
        fn = fnu + n - 1.0;
        aa = 0.5 / tol;

        // I1MACH(9) = Integer.MAX_VALUE;
        bb = 0.5 * Integer.MAX_VALUE;
        aa = Math.min(aa, bb);

        if ( (az > aa) || (fn > aa)) {
            nz[0] = 0;
            ierr[0] = 4;

            return;
        }

        aa = Math.sqrt(aa);

        if ( (az > aa) || (fn > aa)) {
            ierr[0] = 3;
        }

        // Calculate csgn = exp(fnu*hpi*i) to minimize losses of significance
        // when fnu is large
        cii = 1.0;
        inu = (int) fnu;
        inuh = inu / 2;
        ir = inu - (2 * inuh);
        arg = (fnu - (inu - ir)) * hpi;
        csgnr = Math.cos(arg);
        csgni = Math.sin(arg);

        if ( (inuh % 2) != 0) {
            csgnr = -csgnr;
            csgni = -csgni;
        } // if ((inuh%2) != 0)

        // zn is in the right half plane
        znr = zi;
        zni = -zr;

        if (zi < 0.0) {
            znr = -znr;
            zni = -zni;
            csgni = -csgni;
            cii = -cii;
        } // if (zi < 0.0)

        zbinu(znr, zni, fnu, kode, n, cyr, cyi, nz);

        if (nz[0] == -2) {
            nz[0] = 0;
            ierr[0] = 5;

            return;
        } // if (nz[0] == -2)
        else if (nz[0] < 0) {
            nz[0] = 0;
            ierr[0] = 2;

            return;
        } // else if (nz[0] < 0)

        nl = n - nz[0];

        if (nl == 0) {
            return;
        } // if (nl == 0)

        rtol = 1.0 / tol;
        ascle = 1.0E3 * tiny * rtol;

        for (i = 1; i <= nl; i++) {
            aa = cyr[i - 1];
            bb = cyi[i - 1];
            atol = 1.0;

            if (Math.max(Math.abs(aa), Math.abs(bb)) <= ascle) {
                aa = aa * rtol;
                bb = bb * rtol;
                atol = tol;
            } // if (Math.max(Math.abs(aa),Math.abs(bb)) <= ascle)

            str = (aa * csgnr) - (bb * csgni);
            sti = (aa * csgni) + (bb * csgnr);
            cyr[i - 1] = str * atol;
            cyi[i - 1] = sti * atol;
            str = -csgni * cii;
            csgni = csgnr * cii;
            csgnr = str;
        } // for (i = 1; i <= nl; i++)

        return;
    }

    /**
     * DOCUMENT ME!
     * 
     * @param zr double
     * @param zi double
     * @param fnu double
     * @param kode int
     * @param n int
     * @param cyr double[]
     * @param cyi double[]
     * @param nz int[]
     * @param ierr int[]
     */
    private void zbesk(final double zr, final double zi, final double fnu, final int kode, final int n,
            final double[] cyr, final double[] cyi, final int[] nz, final int[] ierr) {

        /**
         * *DATE WRITTEN 830501 (YYMMDD) C***REVISION DATE 890801, 930101 (YYMMDD) C***CATEGORY NO. B5K C***KEYWORDS
         * K-BESSEL FUNCTION,COMPLEX BESSEL FUNCTION, C MODIFIED BESSEL FUNCTION OF THE SECOND KIND, C BESSEL FUNCTION
         * OF THE THIRD KIND C***AUTHOR AMOS, DONALD E., SANDIA NATIONAL LABORATORIES C***PURPOSE TO COMPUTE K-BESSEL
         * FUNCTIONS OF COMPLEX ARGUMENT C***DESCRIPTION C C ***A DOUBLE PRECISION ROUTINE*** C C ON KODE=1, ZBESK
         * COMPUTES AN N MEMBER SEQUENCE OF COMPLEX C BESSEL FUNCTIONS CY(J)=K(FNU+J-1,Z) FOR REAL, NONNEGATIVE C ORDERS
         * FNU+J-1, J=1,...,N AND COMPLEX Z.NE.CMPLX(0.0,0.0) C IN THE CUT PLANE -PI.LT.ARG(Z).LE.PI. ON KODE=2, ZBESK C
         * RETURNS THE SCALED K FUNCTIONS, C C CY(J)=EXP(Z)*K(FNU+J-1,Z) , J=1,...,N, C C WHICH REMOVE THE EXPONENTIAL
         * BEHAVIOR IN BOTH THE LEFT AND C RIGHT HALF PLANES FOR Z TO INFINITY. DEFINITIONS AND C NOTATION ARE FOUND IN
         * THE NBS HANDBOOK OF MATHEMATICAL C FUNCTIONS (REF. 1). C C INPUT ZR,ZI,FNU ARE DOUBLE PRECISION C ZR,ZI -
         * Z=CMPLX(ZR,ZI), Z.NE.CMPLX(0.0D0,0.0D0), C -PI.LT.ARG(Z).LE.PI C FNU - ORDER OF INITIAL K FUNCTION,
         * FNU.GE.0.0D0 C N - NUMBER OF MEMBERS OF THE SEQUENCE, N.GE.1 C KODE - A PARAMETER TO INDICATE THE SCALING
         * OPTION C KODE= 1 RETURNS C CY(I)=K(FNU+I-1,Z), I=1,...,N C = 2 RETURNS C CY(I)=K(FNU+I-1,Z)*EXP(Z), I=1,...,N
         * C C OUTPUT CYR,CYI ARE DOUBLE PRECISION C CYR,CYI- DOUBLE PRECISION VECTORS WHOSE FIRST N COMPONENTS C
         * CONTAIN REAL AND IMAGINARY PARTS FOR THE SEQUENCE C CY(I)=K(FNU+I-1,Z), I=1,...,N OR C
         * CY(I)=K(FNU+I-1,Z)*EXP(Z), I=1,...,N C DEPENDING ON KODE C NZ - NUMBER OF COMPONENTS SET TO ZERO DUE TO
         * UNDERFLOW. C NZ= 0 , NORMAL RETURN C NZ.GT.0 , FIRST NZ COMPONENTS OF CY SET TO ZERO DUE C TO UNDERFLOW,
         * CY(I)=CMPLX(0.0D0,0.0D0), C I=1,...,N WHEN X.GE.0.0. WHEN X.LT.0.0 C NZ STATES ONLY THE NUMBER OF UNDERFLOWS
         * C IN THE SEQUENCE. C C IERR - ERROR FLAG C IERR=0, NORMAL RETURN - COMPUTATION COMPLETED C IERR=1, INPUT
         * ERROR - NO COMPUTATION C IERR=2, OVERFLOW - NO COMPUTATION, FNU IS C TOO LARGE OR CABS(Z) IS TOO SMALL OR
         * BOTH C IERR=3, CABS(Z) OR FNU+N-1 LARGE - COMPUTATION DONE C BUT LOSSES OF SIGNIFCANCE BY ARGUMENT C
         * REDUCTION PRODUCE LESS THAN HALF OF MACHINE C ACCURACY C IERR=4, CABS(Z) OR FNU+N-1 TOO LARGE - NO COMPUTA- C
         * TION BECAUSE OF COMPLETE LOSSES OF SIGNIFI- C CANCE BY ARGUMENT REDUCTION C IERR=5, ERROR - NO COMPUTATION, C
         * ALGORITHM TERMINATION CONDITION NOT MET C C***LONG DESCRIPTION C C EQUATIONS OF THE REFERENCE ARE IMPLEMENTED
         * FOR SMALL ORDERS C DNU AND DNU+1.0 IN THE RIGHT HALF PLANE X.GE.0.0. FORWARD C RECURRENCE GENERATES HIGHER
         * ORDERS. K IS CONTINUED TO THE LEFT C HALF PLANE BY THE RELATION C C K(FNU,Z*EXP(MP)) =
         * EXP(-MP*FNU)*K(FNU,Z)-MP*I(FNU,Z) C MP=MR*PI*I, MR=+1 OR -1, RE(Z).GT.0, I**2=-1 C C WHERE I(FNU,Z) IS THE I
         * BESSEL FUNCTION. C C FOR LARGE ORDERS, FNU.GT.FNUL, THE K FUNCTION IS COMPUTED C BY MEANS OF ITS UNIFORM
         * ASYMPTOTIC EXPANSIONS. C C FOR NEGATIVE ORDERS, THE FORMULA C C K(-FNU,Z) = K(FNU,Z) C C CAN BE USED. C C
         * ZBESK ASSUMES THAT A SIGNIFICANT DIGIT SINH(X) FUNCTION IS C AVAILABLE. C C IN MOST COMPLEX VARIABLE
         * COMPUTATION, ONE MUST EVALUATE ELE- C MENTARY FUNCTIONS. WHEN THE MAGNITUDE OF Z OR FNU+N-1 IS C LARGE,
         * LOSSES OF SIGNIFICANCE BY ARGUMENT REDUCTION OCCUR. C CONSEQUENTLY, IF EITHER ONE EXCEEDS U1=SQRT(0.5/UR),
         * THEN C LOSSES EXCEEDING HALF PRECISION ARE LIKELY AND AN ERROR FLAG C IERR=3 IS TRIGGERED WHERE
         * UR=DMAX1(D1MACH(4),1.0D-18) IS C DOUBLE PRECISION UNIT ROUNDOFF LIMITED TO 18 DIGITS PRECISION. C IF EITHER
         * IS LARGER THAN U2=0.5/UR, THEN ALL SIGNIFICANCE IS C LOST AND IERR=4. IN ORDER TO USE THE INT FUNCTION,
         * ARGUMENTS C MUST BE FURTHER RESTRICTED NOT TO EXCEED THE LARGEST MACHINE C INTEGER, U3=I1MACH(9). THUS, THE
         * MAGNITUDE OF Z AND FNU+N-1 IS C RESTRICTED BY MIN(U2,U3). ON 32 BIT MACHINES, U1,U2, AND U3 C ARE
         * APPROXIMATELY 2.0E+3, 4.2E+6, 2.1E+9 IN SINGLE PRECISION C ARITHMETIC AND 1.3E+8, 1.8E+16, 2.1E+9 IN DOUBLE
         * PRECISION C ARITHMETIC RESPECTIVELY. THIS MAKES U2 AND U3 LIMITING IN C THEIR RESPECTIVE ARITHMETICS. THIS
         * MEANS THAT ONE CAN EXPECT C TO RETAIN, IN THE WORST CASES ON 32 BIT MACHINES, NO DIGITS C IN SINGLE AND ONLY
         * 7 DIGITS IN DOUBLE PRECISION ARITHMETIC. C SIMILAR CONSIDERATIONS HOLD FOR OTHER MACHINES. C C THE
         * APPROXIMATE RELATIVE ERROR IN THE MAGNITUDE OF A COMPLEX C BESSEL FUNCTION CAN BE EXPRESSED BY P*10**S WHERE
         * P=MAX(UNIT C ROUNDOFF,1.0E-18) IS THE NOMINAL PRECISION AND 10**S REPRE- C SENTS THE INCREASE IN ERROR DUE TO
         * ARGUMENT REDUCTION IN THE C ELEMENTARY FUNCTIONS. HERE, S=MAX(1,ABS(LOG10(CABS(Z))), C ABS(LOG10(FNU)))
         * APPROXIMATELY (I.E. S=MAX(1,ABS(EXPONENT OF C CABS(Z),ABS(EXPONENT OF FNU)) ). HOWEVER, THE PHASE ANGLE MAY C
         * HAVE ONLY ABSOLUTE ACCURACY. THIS IS MOST LIKELY TO OCCUR WHEN C ONE COMPONENT (IN ABSOLUTE VALUE) IS LARGER
         * THAN THE OTHER BY C SEVERAL ORDERS OF MAGNITUDE. IF ONE COMPONENT IS 10**K LARGER C THAN THE OTHER, THEN ONE
         * CAN EXPECT ONLY MAX(ABS(LOG10(P))-K, C 0) SIGNIFICANT DIGITS; OR, STATED ANOTHER WAY, WHEN K EXCEEDS C THE
         * EXPONENT OF P, NO SIGNIFICANT DIGITS REMAIN IN THE SMALLER C COMPONENT. HOWEVER, THE PHASE ANGLE RETAINS
         * ABSOLUTE ACCURACY C BECAUSE, IN COMPLEX ARITHMETIC WITH PRECISION P, THE SMALLER C COMPONENT WILL NOT (AS A
         * RULE) DECREASE BELOW P TIMES THE C MAGNITUDE OF THE LARGER COMPONENT. IN THESE EXTREME CASES, C THE PRINCIPAL
         * PHASE ANGLE IS ON THE ORDER OF +P, -P, PI/2-P, C OR -PI/2+P. C C***REFERENCES HANDBOOK OF MATHEMATICAL
         * FUNCTIONS BY M. ABRAMOWITZ C AND I. A. STEGUN, NBS AMS SERIES 55, U.S. DEPT. OF C COMMERCE, 1955. C C
         * COMPUTATION OF BESSEL FUNCTIONS OF COMPLEX ARGUMENT C BY D. E. AMOS, SAND83-0083, MAY, 1983. C C COMPUTATION
         * OF BESSEL FUNCTIONS OF COMPLEX ARGUMENT C AND LARGE ORDER BY D. E. AMOS, SAND83-0643, MAY, 1983. C C A
         * SUBROUTINE PACKAGE FOR BESSEL FUNCTIONS OF A COMPLEX C ARGUMENT AND NONNEGATIVE ORDER BY D. E. AMOS, SAND85-
         * C 1018, MAY, 1985 C C A PORTABLE PACKAGE FOR BESSEL FUNCTIONS OF A COMPLEX C ARGUMENT AND NONNEGATIVE ORDER
         * BY D. E. AMOS, ACM C TRANS. MATH. SOFTWARE, VOL. 12, NO. 3, SEPTEMBER 1986, C PP 265-273.
         */
        int nn;
        double neweps;
        int k;
        int k1;
        double aa;
        double bb;
        double dig;
        double az;
        double fn;
        double ufl;
        double arg;
        double aln;
        final int[] nuf = new int[1];
        final int[] nw = new int[1];
        int mr;

        nn = n;

        /**
         * SET PARAMETERS RELATED TO MACHINE CONSTANTS. C TOL IS THE APPROXIMATE UNIT ROUNDOFF LIMITED TO 1.0E-18. C
         * ELIM IS THE APPROXIMATE EXPONENTIAL OVER- AND UNDERFLOW LIMIT. C EXP(-ELIM).LT.EXP(-ALIM)=EXP(-ELIM)/TOL AND
         * C EXP(ELIM).GT.EXP(ALIM)=EXP(ELIM)*TOL ARE INTERVALS NEAR C UNDERFLOW AND OVERFLOW LIMITS WHERE SCALED
         * ARITHMETIC IS DONE. C RL IS THE LOWER BOUNDARY OF THE ASYMPTOTIC EXPANSION FOR LARGE Z. C DIG = NUMBER OF
         * BASE 10 DIGITS IN TOL = 10**(-DIG).C FNUL IS THE LOWER BOUNDARY OF THE ASYMPTOTIC SERIES FOR LARGE FNU.
         */
        // epsilon = D1MACH(4)
        // Machine epsilon is the smallest positive epsilon such that
        // (1.0 + epsilon) != 1.0.
        // epsilon = 2**(1 - doubleDigits) = 2**(1 - 53) = 2**(-52)
        // epsilon = 2.224460e-16
        // epsilon is called the largest relative spacing
        epsilon = 1.0;
        neweps = 1.0;

        while (true) {

            if (1.0 == (1.0 + neweps)) {
                break;
            } else {
                epsilon = neweps;
                neweps = neweps / 2.0;
            }
        }

        tol = Math.max(epsilon, 1.0e-18);

        // emin, the smallest exponent E for double precision, is I1MACH(15)
        // tiny = D1MACH(1) = 2**(emin - 1) = 2**(-1022)
        // D1MACH(1) is the smallest normalized number, which preserves the
        // full precision of the mantissa.
        // Double.MIN_VALUE = 2**(-1074) is the smallest denormalized number,
        // which preseerves only a portion of the fraction's precision.
        emin = -1021;
        tiny = Math.pow(2, -1022);

        // emax, the largest exponent E for double precision, is I1MACH(16)
        // D1MACH(2) = 2**(emax)*(1 - 2**(-doubleDigits)) = 2**1024*(1 - 2**-53)
        // D1MACH(2) = Double.MAX_VALUE
        emax = 1024;

        // r1m5 = log10(2), which is D1MACH(5)
        r1m5 = 0.434294481903251 * Math.log(2.0);
        k = Math.min(Math.abs(emin), Math.abs(emax));
        elim = 2.303 * ( (k * r1m5) - 3.0);

        // doubleDigits is I1MACH(14), the number of base-B digits for double
        // precision. IEEE 754 double precision has 64 bits with 1 sign bit,
        // 11 exponent bits, and 52 fraction, mantissa, or signficand bits.
        // Floating-point numbers are typically stored in normalized form,
        // with the radix point after the first non-zero digit. In base two,
        // the only possible non-zero digit is 1. Thus, a leading digit of 1
        // can always be assumed and need not be explicitly represented.
        // Therefore, the mantissa effectively has 53 bits of resolution.
        doubleDigits = 53;
        k1 = doubleDigits - 1;
        aa = r1m5 * k1;
        dig = Math.min(aa, 18.0);
        aa = 2.303 * aa;
        alim = elim + Math.max( -aa, -41.45);
        rl = (1.2 * dig) + 3.0;
        fnul = 10.0 + (6.0 * (dig - 3.0));

        // test for proper range
        az = zabs(zr, zi);
        fn = fnu + nn - 1.0;
        aa = 0.5 / tol;

        // I1MACH(9) = Integer.MAX_VALUE;
        bb = 0.5 * Integer.MAX_VALUE;
        aa = Math.min(aa, bb);

        if ( (az > aa) || (fn > aa)) {
            nz[0] = 0;
            ierr[0] = 4;

            return;
        }

        aa = Math.sqrt(aa);

        if ( (az > aa) || (fn > aa)) {
            ierr[0] = 3;
        }

        // Overflow test on the last member of the sequence
        ufl = 1.0E3 * tiny;

        if (az < ufl) {
            nz[0] = 0;
            ierr[0] = 2;

            return;
        } // if (az < ufl)

        if (fnu > fnul) {

            // Uniform asymptotic expansions for fnu > fnul
            mr = 0;

            if (zr < 0.0) {
                mr = 1;

                if (zi < 0.0) {
                    mr = -1;
                } // if (zi < 0.0)
            } // if (zr < 0.0)

            zbunk(zr, zi, fnu, kode, mr, nn, cyr, cyi, nw);

            if (nw[0] == -1) {
                nz[0] = 0;
                ierr[0] = 2;

                return;
            } // if (nw[0] == -1)
            else if (nw[0] < 0) {
                nz[0] = 0;
                ierr[0] = 5;

                return;
            } // else if (nw[0] < 0)

            nz[0] = nz[0] + nw[0];

            return;
        } // if (fnu > fnul)

        if (fn <= 1.0) {

            if (zr < 0.0) {

                // Left half plane computation
                // PI/2 < arg(z) <= PI and -PI < arg(z) < -PI/2
                if (nz[0] != 0) {
                    nz[0] = 0;
                    ierr[0] = 2;

                    return;
                } // if (nz[0] != 0)

                mr = 1;

                if (zi < 0.0) {
                    mr = -1;
                } // if (zi < 0.0)

                zacon(zr, zi, fnu, kode, mr, nn, cyr, cyi, nw);

                if (nw[0] == -1) {
                    nz[0] = 0;
                    ierr[0] = 2;

                    return;
                } // if (nw[0] == -1)
                else if (nw[0] < 0) {
                    nz[0] = 0;
                    ierr[0] = 5;

                    return;
                } // else if (nw[0] < 0)

                nz[0] = nw[0];

                return;
            } // if (zr < 0.0)

            // Right half plane computation, real(z) >= 0
            zbknu(zr, zi, fnu, kode, nn, cyr, cyi, nw);

            if (nw[0] == -1) {
                nz[0] = 0;
                ierr[0] = 2;

                return;
            } // if (nw[0] == -1)
            else if (nw[0] < 0) {
                nz[0] = 0;
                ierr[0] = 5;

                return;
            } // else if (nw[0] < 0)

            nz[0] = nw[0];

            return;
        } // if (fn <= 1.0)

        if (fn > 2.0) {
            zuoik(zr, zi, fnu, kode, 2, nn, cyr, cyi, nuf);

            if (nuf[0] < 0) {
                nz[0] = 0;
                ierr[0] = 2;

                return;
            } // if (nuf[0] < 0)

            nz[0] = nz[0] + nuf[0];
            nn = nn - nuf[0];

            // Here nn = n or nn = 0 since nuf = 0,nn, or -1 on return from zuoik
            // If nuf = nn, then cy(i) = 0 for all i
            if (nn == 0) {

                if (zr < 0.0) {
                    nz[0] = 0;
                    ierr[0] = 2;

                    return;
                } // if (zr < 0.0)

                return;
            } // if (nn == 0)

            if (zr < 0.0) {

                // Left half plane computation
                // PI/2 < arg(z) <= PI and -PI < arg(z) < -PI/2
                if (nz[0] != 0) {
                    nz[0] = 0;
                    ierr[0] = 2;

                    return;
                } // if (nz[0] != 0)

                mr = 1;

                if (zi < 0.0) {
                    mr = -1;
                } // if (zi < 0.0)

                zacon(zr, zi, fnu, kode, mr, nn, cyr, cyi, nw);

                if (nw[0] == -1) {
                    nz[0] = 0;
                    ierr[0] = 2;

                    return;
                } // if (nw[0] == -1)
                else if (nw[0] < 0) {
                    nz[0] = 0;
                    ierr[0] = 5;

                    return;
                } // else if (nw[0] < 0)

                nz[0] = nw[0];

                return;
            } // if (zr < 0.0)

            // Right half plane computation, real(z) >= 0
            zbknu(zr, zi, fnu, kode, nn, cyr, cyi, nw);

            if (nw[0] == -1) {
                nz[0] = 0;
                ierr[0] = 2;

                return;
            } // if (nw[0] == -1)
            else if (nw[0] < 0) {
                nz[0] = 0;
                ierr[0] = 5;

                return;
            } // else if (nw[0] < 0)

            nz[0] = nw[0];

            return;
        } // if (fn > 2.0)

        if (az > tol) {

            if (zr < 0.0) {

                // Left half plane computation
                // PI/2 < arg(z) <= PI and -PI < arg(z) < -PI/2
                if (nz[0] != 0) {
                    nz[0] = 0;
                    ierr[0] = 2;

                    return;
                } // if (nz[0] != 0)

                mr = 1;

                if (zi < 0.0) {
                    mr = -1;
                } // if (zi < 0.0)

                zacon(zr, zi, fnu, kode, mr, nn, cyr, cyi, nw);

                if (nw[0] == -1) {
                    nz[0] = 0;
                    ierr[0] = 2;

                    return;
                } // if (nw[0] == -1)
                else if (nw[0] < 0) {
                    nz[0] = 0;
                    ierr[0] = 5;

                    return;
                } // else if (nw[0] < 0)

                nz[0] = nw[0];

                return;
            } // if (zr < 0.0)

            // Right half plane computation, real(z) >= 0
            zbknu(zr, zi, fnu, kode, nn, cyr, cyi, nw);

            if (nw[0] == -1) {
                nz[0] = 0;
                ierr[0] = 2;

                return;
            } // if (nw[0] == -1)
            else if (nw[0] < 0) {
                nz[0] = 0;
                ierr[0] = 5;

                return;
            } // else if (nw[0] < 0)

            nz[0] = nw[0];

            return;
        } // if (az > tol)

        arg = 0.5 * az;
        aln = -fn * Math.log(arg);

        if (aln > elim) {
            nz[0] = 0;
            ierr[0] = 2;

            return;
        } // if (aln > elim)

        if (zr < 0.0) {

            // Left half plane computation
            // PI/2 < arg(z) <= PI and -PI < arg(z) < -PI/2
            if (nz[0] != 0) {
                nz[0] = 0;
                ierr[0] = 2;

                return;
            } // if (nz[0] != 0)

            mr = 1;

            if (zi < 0.0) {
                mr = -1;
            } // if (zi < 0.0)

            zacon(zr, zi, fnu, kode, mr, nn, cyr, cyi, nw);

            if (nw[0] == -1) {
                nz[0] = 0;
                ierr[0] = 2;

                return;
            } // if (nw[0] == -1)
            else if (nw[0] < 0) {
                nz[0] = 0;
                ierr[0] = 5;

                return;
            } // else if (nw[0] < 0)

            nz[0] = nw[0];

            return;
        } // if (zr < 0.0)

        // Right half plane computation, real(z) >= 0
        zbknu(zr, zi, fnu, kode, nn, cyr, cyi, nw);

        if (nw[0] == -1) {
            nz[0] = 0;
            ierr[0] = 2;

            return;
        } // if (nw[0] == -1)
        else if (nw[0] < 0) {
            nz[0] = 0;
            ierr[0] = 5;

            return;
        } // else if (nw[0] < 0)

        nz[0] = nw[0];

        return;
    }

    /**
     * DOCUMENT ME!
     * 
     * @param zr double
     * @param zi double
     * @param fnu double
     * @param kode int
     * @param n int
     * @param cyr double[]
     * @param cyi double[]
     * @param nz int[]
     * @param cwrkr double[]
     * @param cwrki double[]
     * @param ierr int[]
     */
    private void zbesy(final double zr, final double zi, final double fnu, final int kode, final int n,
            final double[] cyr, final double[] cyi, final int[] nz, final double[] cwrkr, final double[] cwrki,
            final int[] ierr) {

        /**
         * *BEGIN PROLOGUE ZBESY C***DATE WRITTEN 830501 (YYMMDD) C***REVISION DATE 890801, 930101 (YYMMDD) C***CATEGORY
         * NO. B5K C***KEYWORDS Y-BESSEL FUNCTION,BESSEL FUNCTION OF COMPLEX ARGUMENT, C BESSEL FUNCTION OF SECOND KIND
         * C***AUTHOR AMOS, DONALD E., SANDIA NATIONAL LABORATORIES C***PURPOSE TO COMPUTE THE Y-BESSEL FUNCTION OF A
         * COMPLEX ARGUMENT C***DESCRIPTION C C ***A DOUBLE PRECISION ROUTINE*** C C ON KODE=1, ZBESY COMPUTES AN N
         * MEMBER SEQUENCE OF COMPLEX C BESSEL FUNCTIONS CY(I)=Y(FNU+I-1,Z) FOR REAL, NONNEGATIVE C ORDERS FNU+I-1,
         * I=1,...,N AND COMPLEX Z IN THE CUT PLANE C -PI.LT.ARG(Z).LE.PI. ON KODE=2, ZBESY RETURNS THE SCALED C
         * FUNCTIONS C C CY(I)=EXP(-ABS(Y))*Y(FNU+I-1,Z) I = 1,...,N , Y=AIMAG(Z) C C WHICH REMOVE THE EXPONENTIAL
         * GROWTH IN BOTH THE UPPER AND C LOWER HALF PLANES FOR Z TO INFINITY. DEFINITIONS AND NOTATION C ARE FOUND IN
         * THE NBS HANDBOOK OF MATHEMATICAL FUNCTIONS C (REF. 1). C C INPUT ZR,ZI,FNU ARE DOUBLE PRECISION C ZR,ZI -
         * Z=CMPLX(ZR,ZI), Z.NE.CMPLX(0.0D0,0.0D0), C -PI.LT.ARG(Z).LE.PI C FNU - ORDER OF INITIAL Y FUNCTION,
         * FNU.GE.0.0D0 C KODE - A PARAMETER TO INDICATE THE SCALING OPTION C KODE= 1 RETURNS C CY(I)=Y(FNU+I-1,Z),
         * I=1,...,N C = 2 RETURNS C CY(I)=Y(FNU+I-1,Z)*EXP(-ABS(Y)), I=1,...,N C WHERE Y=AIMAG(Z) C N - NUMBER OF
         * MEMBERS OF THE SEQUENCE, N.GE.1 C CWRKR, - DOUBLE PRECISION WORK VECTORS OF DIMENSION AT C CWRKI AT LEAST N C
         * C OUTPUT CYR,CYI ARE DOUBLE PRECISION C CYR,CYI- DOUBLE PRECISION VECTORS WHOSE FIRST N COMPONENTS C CONTAIN
         * REAL AND IMAGINARY PARTS FOR THE SEQUENCE C CY(I)=Y(FNU+I-1,Z) OR C CY(I)=Y(FNU+I-1,Z)*EXP(-ABS(Y)) I=1,...,N
         * C DEPENDING ON KODE. C NZ - NZ=0 , A NORMAL RETURN C NZ.GT.0 , NZ COMPONENTS OF CY SET TO ZERO DUE TO C
         * UNDERFLOW (GENERALLY ON KODE=2) C IERR - ERROR FLAG C IERR=0, NORMAL RETURN - COMPUTATION COMPLETED C IERR=1,
         * INPUT ERROR - NO COMPUTATION C IERR=2, OVERFLOW - NO COMPUTATION, FNU IS C TOO LARGE OR CABS(Z) IS TOO SMALL
         * OR BOTH C IERR=3, CABS(Z) OR FNU+N-1 LARGE - COMPUTATION DONE C BUT LOSSES OF SIGNIFCANCE BY ARGUMENT C
         * REDUCTION PRODUCE LESS THAN HALF OF MACHINE C ACCURACY C IERR=4, CABS(Z) OR FNU+N-1 TOO LARGE - NO COMPUTA- C
         * TION BECAUSE OF COMPLETE LOSSES OF SIGNIFI- C CANCE BY ARGUMENT REDUCTION C IERR=5, ERROR - NO COMPUTATION, C
         * ALGORITHM TERMINATION CONDITION NOT MET C C***LONG DESCRIPTION C C THE COMPUTATION IS CARRIED OUT IN TERMS OF
         * THE I(FNU,Z) AND C K(FNU,Z) BESSEL FUNCTIONS IN THE RIGHT HALF PLANE BY C C Y(FNU,Z) = I*CC*I(FNU,ARG) -
         * (2/PI)*CONJG(CC)*K(FNU,ARG) C C Y(FNU,Z) = CONJG(Y(FNU,CONJG(Z))) C C FOR AIMAG(Z).GE.0 AND AIMAG(Z).LT.0
         * RESPECTIVELY, WHERE C CC=EXP(I*PI*FNU/2), ARG=Z*EXP(-I*PI/2) AND I**2=-1. C C FOR NEGATIVE ORDERS,THE FORMULA
         * C C Y(-FNU,Z) = Y(FNU,Z)*COS(PI*FNU) + J(FNU,Z)*SIN(PI*FNU) C C CAN BE USED. HOWEVER,FOR LARGE ORDERS CLOSE
         * TO HALF ODD C INTEGERS THE FUNCTION CHANGES RADICALLY. WHEN FNU IS A LARGE C POSITIVE HALF ODD INTEGER,THE
         * MAGNITUDE OF Y(-FNU,Z)=J(FNU,Z)* C SIN(PI*FNU) IS A LARGE NEGATIVE POWER OF TEN. BUT WHEN FNU IS C NOT A HALF
         * ODD INTEGER, Y(FNU,Z) DOMINATES IN MAGNITUDE WITH A C LARGE POSITIVE POWER OF TEN AND THE MOST THAT THE
         * SECOND TERM C CAN BE REDUCED IS BY UNIT ROUNDOFF FROM THE COEFFICIENT. THUS, C WIDE CHANGES CAN OCCUR WITHIN
         * UNIT ROUNDOFF OF A LARGE HALF C ODD INTEGER. HERE, LARGE MEANS FNU.GT.CABS(Z). C C IN MOST COMPLEX VARIABLE
         * COMPUTATION, ONE MUST EVALUATE ELE- C MENTARY FUNCTIONS. WHEN THE MAGNITUDE OF Z OR FNU+N-1 IS C LARGE,
         * LOSSES OF SIGNIFICANCE BY ARGUMENT REDUCTION OCCUR. C CONSEQUENTLY, IF EITHER ONE EXCEEDS U1=SQRT(0.5/UR),
         * THEN C LOSSES EXCEEDING HALF PRECISION ARE LIKELY AND AN ERROR FLAG C IERR=3 IS TRIGGERED WHERE
         * UR=DMAX1(D1MACH(4),1.0D-18) IS C DOUBLE PRECISION UNIT ROUNDOFF LIMITED TO 18 DIGITS PRECISION. C IF EITHER
         * IS LARGER THAN U2=0.5/UR, THEN ALL SIGNIFICANCE IS C LOST AND IERR=4. IN ORDER TO USE THE INT FUNCTION,
         * ARGUMENTS C MUST BE FURTHER RESTRICTED NOT TO EXCEED THE LARGEST MACHINE C INTEGER, U3=I1MACH(9). THUS, THE
         * MAGNITUDE OF Z AND FNU+N-1 IS C RESTRICTED BY MIN(U2,U3). ON 32 BIT MACHINES, U1,U2, AND U3 C ARE
         * APPROXIMATELY 2.0E+3, 4.2E+6, 2.1E+9 IN SINGLE PRECISION C ARITHMETIC AND 1.3E+8, 1.8E+16, 2.1E+9 IN DOUBLE
         * PRECISION C ARITHMETIC RESPECTIVELY. THIS MAKES U2 AND U3 LIMITING IN C THEIR RESPECTIVE ARITHMETICS. THIS
         * MEANS THAT ONE CAN EXPECT C TO RETAIN, IN THE WORST CASES ON 32 BIT MACHINES, NO DIGITS C IN SINGLE AND ONLY
         * 7 DIGITS IN DOUBLE PRECISION ARITHMETIC. C SIMILAR CONSIDERATIONS HOLD FOR OTHER MACHINES. C C THE
         * APPROXIMATE RELATIVE ERROR IN THE MAGNITUDE OF A COMPLEX C BESSEL FUNCTION CAN BE EXPRESSED BY P*10**S WHERE
         * P=MAX(UNIT C ROUNDOFF,1.0E-18) IS THE NOMINAL PRECISION AND 10**S REPRE- C SENTS THE INCREASE IN ERROR DUE TO
         * ARGUMENT REDUCTION IN THE C ELEMENTARY FUNCTIONS. HERE, S=MAX(1,ABS(LOG10(CABS(Z))), C ABS(LOG10(FNU)))
         * APPROXIMATELY (I.E. S=MAX(1,ABS(EXPONENT OF C CABS(Z),ABS(EXPONENT OF FNU)) ). HOWEVER, THE PHASE ANGLE MAY C
         * HAVE ONLY ABSOLUTE ACCURACY. THIS IS MOST LIKELY TO OCCUR WHEN C ONE COMPONENT (IN ABSOLUTE VALUE) IS LARGER
         * THAN THE OTHER BY C SEVERAL ORDERS OF MAGNITUDE. IF ONE COMPONENT IS 10**K LARGER C THAN THE OTHER, THEN ONE
         * CAN EXPECT ONLY MAX(ABS(LOG10(P))-K, C 0) SIGNIFICANT DIGITS; OR, STATED ANOTHER WAY, WHEN K EXCEEDS C THE
         * EXPONENT OF P, NO SIGNIFICANT DIGITS REMAIN IN THE SMALLER C COMPONENT. HOWEVER, THE PHASE ANGLE RETAINS
         * ABSOLUTE ACCURACY C BECAUSE, IN COMPLEX ARITHMETIC WITH PRECISION P, THE SMALLER C COMPONENT WILL NOT (AS A
         * RULE) DECREASE BELOW P TIMES THE C MAGNITUDE OF THE LARGER COMPONENT. IN THESE EXTREME CASES, C THE PRINCIPAL
         * PHASE ANGLE IS ON THE ORDER OF +P, -P, PI/2-P, C OR -PI/2+P. C C***REFERENCES HANDBOOK OF MATHEMATICAL
         * FUNCTIONS BY M. ABRAMOWITZ C AND I. A. STEGUN, NBS AMS SERIES 55, U.S. DEPT. OF C COMMERCE, 1955. C C
         * COMPUTATION OF BESSEL FUNCTIONS OF COMPLEX ARGUMENT C BY D. E. AMOS, SAND83-0083, MAY, 1983. C C COMPUTATION
         * OF BESSEL FUNCTIONS OF COMPLEX ARGUMENT C AND LARGE ORDER BY D. E. AMOS, SAND83-0643, MAY, 1983 C C A
         * SUBROUTINE PACKAGE FOR BESSEL FUNCTIONS OF A COMPLEX C ARGUMENT AND NONNEGATIVE ORDER BY D. E. AMOS, SAND85-
         * C 1018, MAY, 1985 C C A PORTABLE PACKAGE FOR BESSEL FUNCTIONS OF A COMPLEX C ARGUMENT AND NONNEGATIVE ORDER
         * BY D. E. AMOS, ACM C TRANS. MATH. SOFTWARE, VOL. 12, NO. 3, SEPTEMBER 1986, C PP 265-273.
         */
        final double[] cipr = new double[] {1.0, 0.0, -1.0, 0.0};
        final double[] cipi = new double[] {0.0, 1.0, 0.0, -1.0};
        final double hpi = Math.PI / 2.0;
        double zzr;
        double zzi;
        double znr;
        double zni;
        final int[] nz1 = new int[1];
        final int[] nz2 = new int[1];
        int ifnu;
        double ffnu;
        double arg;
        double csgnr;
        double csgni;
        int i4;
        double str;
        double sti;
        double rhpi;
        int i;
        double exr;
        double exi;
        double ey;
        double tay;
        double rtol;
        double ascle;
        double zvr;
        double zvi;
        double atol;
        double zur;
        double zui;
        double cspnr;
        double cspni;
        double neweps;
        int k;

        zzr = zr;
        zzi = zi;

        if (zi < 0.0) {
            zzi = -zzi;
        } // if (zi < 0.0)

        znr = zzi;
        zni = -zzr;
        zbesi(znr, zni, fnu, kode, n, cyr, cyi, nz1, ierr);

        if ( (ierr[0] != 0) && (ierr[0] != 3)) {
            nz[0] = 0;

            return;
        } // if (ierr[0] != 0) && (ierr[0] != 3)

        zbesk(znr, zni, fnu, kode, n, cwrkr, cwrki, nz2, ierr);

        if ( (ierr[0] != 0) && (ierr[0] != 3)) {
            nz[0] = 0;

            return;
        } // if (ierr[0] != 0) && (ierr[0] != 3)

        nz[0] = Math.min(nz1[0], nz2[0]);
        ifnu = (int) (fnu);
        ffnu = fnu - ifnu;
        arg = hpi * ffnu;
        csgnr = Math.cos(arg);
        csgni = Math.sin(arg);
        i4 = (ifnu % 4) + 1;
        str = (csgnr * cipr[i4 - 1]) - (csgni * cipi[i4 - 1]);
        csgni = (csgnr * cipi[i4 - 1]) + (csgni * cipr[i4 - 1]);
        csgnr = str;
        rhpi = 1.0 / hpi;
        cspnr = csgnr * rhpi;
        cspni = -csgni * rhpi;
        str = -csgni;
        csgni = csgnr;
        csgnr = str;

        if (kode != 2) {

            for (i = 1; i <= n; i++) {
                str = (csgnr * cyr[i - 1]) - (csgni * cyi[i - 1]);
                str = str - ( (cspnr * cwrkr[i - 1]) - (cspni * cwrki[i - 1]));
                sti = (csgnr * cyi[i - 1]) + (csgni * cyr[i - 1]);
                sti = sti - ( (cspnr * cwrki[i - 1]) + (cspni * cwrkr[i - 1]));
                cyr[i - 1] = str;
                cyi[i - 1] = sti;
                str = -csgni;
                csgni = csgnr;
                csgnr = str;
                str = cspni;
                cspni = -cspnr;
                cspnr = str;
            } // for (i = 1; i <= n; i++)

            if (zi < 0.0) {

                for (i = 1; i <= n; i++) {
                    cyi[i - 1] = -cyi[i - 1];
                } // for (i = 1; i <= n; i++)
            } // if (zi < 0.0)

            return;
        } // if (kode != 2)

        // here kode == 2
        exr = Math.cos(zr);
        exi = Math.sin(zr);

        // epsilon = D1MACH(4)
        // Machine epsilon is the smallest positive epsilon such that
        // (1.0 + epsilon) != 1.0.
        // epsilon = 2**(1 - doubleDigits) = 2**(1 - 53) = 2**(-52)
        // epsilon = 2.224460e-16
        // epsilon is called the largest relative spacing
        epsilon = 1.0;
        neweps = 1.0;

        while (true) {

            if (1.0 == (1.0 + neweps)) {
                break;
            } else {
                epsilon = neweps;
                neweps = neweps / 2.0;
            }
        }

        tol = Math.max(epsilon, 1.0e-18);

        // emin, the smallest exponent E for double precision, is I1MACH(15)
        // tiny = D1MACH(1) = 2**(emin - 1) = 2**(-1022)
        // D1MACH(1) is the smallest normalized number, which preserves the
        // full precision of the mantissa.
        // Double.MIN_VALUE = 2**(-1074) is the smallest denormalized number,
        // which preseerves only a portion of the fraction's precision.
        emin = -1021;
        tiny = Math.pow(2, -1022);

        // emax, the largest exponent E for double precision, is I1MACH(16)
        // D1MACH(2) = 2**(emax)*(1 - 2**(-doubleDigits)) = 2**1024*(1 - 2**-53)
        // D1MACH(2) = Double.MAX_VALUE
        emax = 1024;

        // r1m5 = log10(2), which is D1MACH(5)
        r1m5 = 0.434294481903251 * Math.log(2.0);
        k = Math.min(Math.abs(emin), Math.abs(emax));

        // elim is the approximate under- and overflow limit
        elim = 2.303 * ( (k * r1m5) - 3.0);
        ey = 0.0;
        tay = Math.abs(zi + zi);

        if (tay < elim) {
            ey = Math.exp( -tay);
        } // if (tay < elim)

        str = ( (exr * cspnr) - (exi * cspni)) * ey;
        cspni = ( (exr * cspni) + (exi * cspnr)) * ey;
        cspnr = str;
        nz[0] = 0;
        rtol = 1.0 / tol;
        ascle = 1.0E3 * tiny * rtol;

        for (i = 1; i <= n; i++) {

            // cy[i-1] = csgn*cy[i-1] - cspn*cwrk[i-1]: products are computed in
            // scaled mode if cy[i-1] or cwrk[i-1] are close to underflow to
            // prevent underflow in an intermediate computation
            zvr = cwrkr[i - 1];
            zvi = cwrki[i - 1];
            atol = 1.0;

            if (Math.max(Math.abs(zvr), Math.abs(zvi)) <= ascle) {
                zvr = zvr * rtol;
                zvi = zvi * rtol;
                atol = tol;
            } // if (Math.max(Math.abs(zvr),Math.abs(zvi)) <= ascle)

            str = ( (zvr * cspnr) - (zvi * cspni)) * atol;
            zvi = ( (zvr * cspni) + (zvi * cspnr)) * atol;
            zvr = str;
            zur = cyr[i - 1];
            zui = cyi[i - 1];
            atol = 1.0;

            if (Math.max(Math.abs(zur), Math.abs(zui)) <= ascle) {
                zur = zur * rtol;
                zui = zui * rtol;
                atol = tol;
            } // if (Math.max(Math.abs(zur),Math.abs(zui)) <= ascle)

            str = ( (zur * csgnr) - (zui * csgni)) * atol;
            zui = ( (zur * csgni) + (zui * csgnr)) * atol;
            zur = str;
            cyr[i - 1] = zur - zvr;
            cyi[i - 1] = zui - zvi;

            if (zi < 0.0) {
                cyi[i - 1] = -cyi[i - 1];
            } // if (zi < 0.0)

            if ( (cyr[i - 1] == 0.0) && (cyi[i - 1] == 0.0) && (ey == 0.0)) {
                nz[0] = nz[0] + 1;
            } // if ((cyr[i-1] == 0.0) && (cyi[i-1] == 0.0) && (ey == 0.0))

            str = -csgni;
            csgni = csgnr;
            csgnr = str;
            str = cspni;
            cspni = -cspnr;
            cspnr = str;
        } // for (i = 1; i <= n; i++)

        return;
    }

    /**
     * An older version of zbesy used by the ZQCBY quick check routine.
     * 
     * @param zr double
     * @param zi double
     * @param fnu double
     * @param kode int
     * @param n int
     * @param cyr double[]
     * @param cyi double[]
     * @param nz int[]
     * @param cwrkr double[]
     * @param cwrki double[]
     * @param ierr int[]
     */
    private void zbesyh(final double zr, final double zi, final double fnu, final int kode, final int n,
            final double[] cyr, final double[] cyi, final int[] nz, final double[] cwrkr, final double[] cwrki,
            final int[] ierr) {

        /**
         * *BEGIN PROLOGUE ZBESYH C***DATE WRITTEN 830501 (YYMMDD) C***REVISION DATE 890801, 930101 (YYMMDD)
         * C***CATEGORY NO. B5K C***KEYWORDS Y-BESSEL FUNCTION,BESSEL FUNCTION OF COMPLEX ARGUMENT, C BESSEL FUNCTION OF
         * SECOND KIND C***AUTHOR AMOS, DONALD E., SANDIA NATIONAL LABORATORIES C***PURPOSE TO COMPUTE THE Y-BESSEL
         * FUNCTION OF A COMPLEX ARGUMENT C***DESCRIPTION C C ***A DOUBLE PRECISION ROUTINE*** C C ON KODE=1, ZBESYH
         * COMPUTES AN N MEMBER SEQUENCE OF COMPLEX C BESSEL FUNCTIONS CY(I)=Y(FNU+I-1,Z) FOR REAL, NONNEGATIVE C ORDERS
         * FNU+I-1, I=1,...,N AND COMPLEX Z IN THE CUT PLANE C -PI.LT.ARG(Z).LE.PI. ON KODE=2, ZBESYH RETURNS THE SCALED
         * C FUNCTIONS C C CY(I)=EXP(-ABS(Y))*Y(FNU+I-1,Z) I = 1,...,N , Y=AIMAG(Z) C C WHICH REMOVE THE EXPONENTIAL
         * GROWTH IN BOTH THE UPPER AND C LOWER HALF PLANES FOR Z TO INFINITY. DEFINITIONS AND NOTATION C ARE FOUND IN
         * THE NBS HANDBOOK OF MATHEMATICAL FUNCTIONS C (REF. 1). C C INPUT ZR,ZI,FNU ARE DOUBLE PRECISION C ZR,ZI -
         * Z=CMPLX(ZR,ZI), Z.NE.CMPLX(0.0D0,0.0D0), C -PI.LT.ARG(Z).LE.PI C FNU - ORDER OF INITIAL Y FUNCTION,
         * FNU.GE.0.0D0 C KODE - A PARAMETER TO INDICATE THE SCALING OPTION C KODE= 1 RETURNS C CY(I)=Y(FNU+I-1,Z),
         * I=1,...,N C = 2 RETURNS C CY(I)=Y(FNU+I-1,Z)*EXP(-ABS(Y)), I=1,...,N C WHERE Y=AIMAG(Z) C N - NUMBER OF
         * MEMBERS OF THE SEQUENCE, N.GE.1 C CWRKR, - DOUBLE PRECISION WORK VECTORS OF DIMENSION AT C CWRKI AT LEAST N C
         * C OUTPUT CYR,CYI ARE DOUBLE PRECISION C CYR,CYI- DOUBLE PRECISION VECTORS WHOSE FIRST N COMPONENTS C CONTAIN
         * REAL AND IMAGINARY PARTS FOR THE SEQUENCE C CY(I)=Y(FNU+I-1,Z) OR C CY(I)=Y(FNU+I-1,Z)*EXP(-ABS(Y)) I=1,...,N
         * C DEPENDING ON KODE. C NZ - NZ=0 , A NORMAL RETURN C NZ.GT.0 , NZ COMPONENTS OF CY SET TO ZERO DUE TO C
         * UNDERFLOW (GENERALLY ON KODE=2) C IERR - ERROR FLAG C IERR=0, NORMAL RETURN - COMPUTATION COMPLETED C IERR=1,
         * INPUT ERROR - NO COMPUTATION C IERR=2, OVERFLOW - NO COMPUTATION, FNU IS C TOO LARGE OR CABS(Z) IS TOO SMALL
         * OR BOTH C IERR=3, CABS(Z) OR FNU+N-1 LARGE - COMPUTATION DONE C BUT LOSSES OF SIGNIFCANCE BY ARGUMENT C
         * REDUCTION PRODUCE LESS THAN HALF OF MACHINE C ACCURACY C IERR=4, CABS(Z) OR FNU+N-1 TOO LARGE - NO COMPUTA- C
         * TION BECAUSE OF COMPLETE LOSSES OF SIGNIFI- C CANCE BY ARGUMENT REDUCTION C IERR=5, ERROR - NO COMPUTATION, C
         * ALGORITHM TERMINATION CONDITION NOT MET C C***LONG DESCRIPTION C C THE COMPUTATION IS CARRIED OUT BY THE
         * FORMULA C C Y(FNU,Z)=0.5*(H(1,FNU,Z)-H(2,FNU,Z))/I C C WHERE I**2 = -1 AND THE HANKEL BESSEL FUNCTIONS
         * H(1,FNU,Z) C AND H(2,FNU,Z) ARE CALCULATED IN ZBESH. C C FOR NEGATIVE ORDERS,THE FORMULA C C Y(-FNU,Z) =
         * Y(FNU,Z)*COS(PI*FNU) + J(FNU,Z)*SIN(PI*FNU) C C CAN BE USED. HOWEVER,FOR LARGE ORDERS CLOSE TO HALF ODD C
         * INTEGERS THE FUNCTION CHANGES RADICALLY. WHEN FNU IS A LARGE C POSITIVE HALF ODD INTEGER,THE MAGNITUDE OF
         * Y(-FNU,Z)=J(FNU,Z)* C SIN(PI*FNU) IS A LARGE NEGATIVE POWER OF TEN. BUT WHEN FNU IS C NOT A HALF ODD INTEGER,
         * Y(FNU,Z) DOMINATES IN MAGNITUDE WITH A C LARGE POSITIVE POWER OF TEN AND THE MOST THAT THE SECOND TERM C CAN
         * BE REDUCED IS BY UNIT ROUNDOFF FROM THE COEFFICIENT. THUS, C WIDE CHANGES CAN OCCUR WITHIN UNIT ROUNDOFF OF A
         * LARGE HALF C ODD INTEGER. HERE, LARGE MEANS FNU.GT.CABS(Z). C C IN MOST COMPLEX VARIABLE COMPUTATION, ONE
         * MUST EVALUATE ELE- C MENTARY FUNCTIONS. WHEN THE MAGNITUDE OF Z OR FNU+N-1 IS C LARGE, LOSSES OF SIGNIFICANCE
         * BY ARGUMENT REDUCTION OCCUR. C CONSEQUENTLY, IF EITHER ONE EXCEEDS U1=SQRT(0.5/UR), THEN C LOSSES EXCEEDING
         * HALF PRECISION ARE LIKELY AND AN ERROR FLAG C IERR=3 IS TRIGGERED WHERE UR=DMAX1(D1MACH(4),1.0D-18) IS C
         * DOUBLE PRECISION UNIT ROUNDOFF LIMITED TO 18 DIGITS PRECISION. C IF EITHER IS LARGER THAN U2=0.5/UR, THEN ALL
         * SIGNIFICANCE IS C LOST AND IERR=4. IN ORDER TO USE THE INT FUNCTION, ARGUMENTS C MUST BE FURTHER RESTRICTED
         * NOT TO EXCEED THE LARGEST MACHINE C INTEGER, U3=I1MACH(9). THUS, THE MAGNITUDE OF Z AND FNU+N-1 IS C
         * RESTRICTED BY MIN(U2,U3). ON 32 BIT MACHINES, U1,U2, AND U3 C ARE APPROXIMATELY 2.0E+3, 4.2E+6, 2.1E+9 IN
         * SINGLE PRECISION C ARITHMETIC AND 1.3E+8, 1.8E+16, 2.1E+9 IN DOUBLE PRECISION C ARITHMETIC RESPECTIVELY. THIS
         * MAKES U2 AND U3 LIMITING IN C THEIR RESPECTIVE ARITHMETICS. THIS MEANS THAT ONE CAN EXPECT C TO RETAIN, IN
         * THE WORST CASES ON 32 BIT MACHINES, NO DIGITS C IN SINGLE AND ONLY 7 DIGITS IN DOUBLE PRECISION ARITHMETIC. C
         * SIMILAR CONSIDERATIONS HOLD FOR OTHER MACHINES. C C THE APPROXIMATE RELATIVE ERROR IN THE MAGNITUDE OF A
         * COMPLEX C BESSEL FUNCTION CAN BE EXPRESSED BY P*10**S WHERE P=MAX(UNIT C ROUNDOFF,1.0E-18) IS THE NOMINAL
         * PRECISION AND 10**S REPRE- C SENTS THE INCREASE IN ERROR DUE TO ARGUMENT REDUCTION IN THE C ELEMENTARY
         * FUNCTIONS. HERE, S=MAX(1,ABS(LOG10(CABS(Z))), C ABS(LOG10(FNU))) APPROXIMATELY (I.E. S=MAX(1,ABS(EXPONENT OF
         * C CABS(Z),ABS(EXPONENT OF FNU)) ). HOWEVER, THE PHASE ANGLE MAY C HAVE ONLY ABSOLUTE ACCURACY. THIS IS MOST
         * LIKELY TO OCCUR WHEN C ONE COMPONENT (IN ABSOLUTE VALUE) IS LARGER THAN THE OTHER BY C SEVERAL ORDERS OF
         * MAGNITUDE. IF ONE COMPONENT IS 10**K LARGER C THAN THE OTHER, THEN ONE CAN EXPECT ONLY MAX(ABS(LOG10(P))-K, C
         * 0) SIGNIFICANT DIGITS; OR, STATED ANOTHER WAY, WHEN K EXCEEDS C THE EXPONENT OF P, NO SIGNIFICANT DIGITS
         * REMAIN IN THE SMALLER C COMPONENT. HOWEVER, THE PHASE ANGLE RETAINS ABSOLUTE ACCURACY C BECAUSE, IN COMPLEX
         * ARITHMETIC WITH PRECISION P, THE SMALLER C COMPONENT WILL NOT (AS A RULE) DECREASE BELOW P TIMES THE C
         * MAGNITUDE OF THE LARGER COMPONENT. IN THESE EXTREME CASES, C THE PRINCIPAL PHASE ANGLE IS ON THE ORDER OF +P,
         * -P, PI/2-P, C OR -PI/2+P. C C***REFERENCES HANDBOOK OF MATHEMATICAL FUNCTIONS BY M. ABRAMOWITZ C AND I. A.
         * STEGUN, NBS AMS SERIES 55, U.S. DEPT. OF C COMMERCE, 1955. C C COMPUTATION OF BESSEL FUNCTIONS OF COMPLEX
         * ARGUMENT C BY D. E. AMOS, SAND83-0083, MAY, 1983. C C COMPUTATION OF BESSEL FUNCTIONS OF COMPLEX ARGUMENT C
         * AND LARGE ORDER BY D. E. AMOS, SAND83-0643, MAY, 1983 C C A SUBROUTINE PACKAGE FOR BESSEL FUNCTIONS OF A
         * COMPLEX C ARGUMENT AND NONNEGATIVE ORDER BY D. E. AMOS, SAND85- C 1018, MAY, 1985 C C A PORTABLE PACKAGE FOR
         * BESSEL FUNCTIONS OF A COMPLEX C ARGUMENT AND NONNEGATIVE ORDER BY D. E. AMOS, TRANS. C MATH. SOFTWARE, 12,
         * NO. 3, SEPTEMBER 1986, PP 265-273.
         */
        double hcii;
        final int[] nz1 = new int[1];
        final int[] nz2 = new int[1];
        int i;
        double str;
        double sti;
        int k;
        double exr;
        double exi;
        double ey;
        double tay;
        double c1r;
        double c1i;
        double c2r;
        double c2i;
        double rtol;
        double ascle;
        double aa;
        double bb;
        double atol;
        double neweps;

        ierr[0] = 0;
        nz[0] = 0;

        if ( (zr == 0.0) && (zi == 0.0)) {
            ierr[0] = 1;
        } // if ((zr == 0.0) && (zi == 0.0))

        if (fnu < 0.0) {
            ierr[0] = 1;
        } // if (fnu < 0.0)

        if ( (kode < 1) || (kode > 2)) {
            ierr[0] = 1;
        } // if ((kode < 1) || (kode > 2))

        if (n < 1) {
            ierr[0] = 1;
        } // if (n < 1)

        if (ierr[0] != 0) {
            return;
        } // if (ierr[0] != 0)

        hcii = 0.5;
        zbesh(zr, zi, fnu, kode, 1, n, cyr, cyi, nz1, ierr);

        if ( (ierr[0] != 0) && (ierr[0] != 3)) {
            nz[0] = 0;

            return;
        } // if ((ierr[0] != 0) && (ierr[0] != 3))

        zbesh(zr, zi, fnu, kode, 2, n, cwrkr, cwrki, nz2, ierr);

        if ( (ierr[0] != 0) && (ierr[0] != 3)) {
            nz[0] = 0;

            return;
        } // if ((ierr[0] != 0) && (ierr[0] != 3))

        nz[0] = Math.min(nz1[0], nz2[0]);

        if (kode != 2) {

            for (i = 1; i <= n; i++) {
                str = cwrkr[i - 1] - cyr[i - 1];
                sti = cwrki[i - 1] - cyi[i - 1];
                cyr[i - 1] = -sti * hcii;
                cyi[i - 1] = str * hcii;
            } // for (i = 1; i <= n; i++)

            return;
        } // if (kode != 2)

        // Here kode = 2
        // epsilon = D1MACH(4)
        // Machine epsilon is the smallest positive epsilon such that
        // (1.0 + epsilon) != 1.0.
        // epsilon = 2**(1 - doubleDigits) = 2**(1 - 53) = 2**(-52)
        // epsilon = 2.224460e-16
        // epsilon is called the largest relative spacing
        epsilon = 1.0;
        neweps = 1.0;

        while (true) {

            if (1.0 == (1.0 + neweps)) {
                break;
            } else {
                epsilon = neweps;
                neweps = neweps / 2.0;
            }
        }

        tol = Math.max(epsilon, 1.0e-18);

        // emin, the smallest exponent E for double precision, is I1MACH(15)
        // tiny = D1MACH(1) = 2**(emin - 1) = 2**(-1022)
        // D1MACH(1) is the smallest normalized number, which preserves the
        // full precision of the mantissa.
        // Double.MIN_VALUE = 2**(-1074) is the smallest denormalized number,
        // which preseerves only a portion of the fraction's precision.
        emin = -1021;
        tiny = Math.pow(2, -1022);

        // emax, the largest exponent E for double precision, is I1MACH(16)
        // D1MACH(2) = 2**(emax)*(1 - 2**(-doubleDigits)) = 2**1024*(1 - 2**-53)
        // D1MACH(2) = Double.MAX_VALUE
        emax = 1024;

        // r1m5 = log10(2), which is D1MACH(5)
        r1m5 = 0.434294481903251 * Math.log(2.0);
        k = Math.min(Math.abs(emin), Math.abs(emax));

        // elim is the approximate under- and overflow limit
        elim = 2.303 * ( (k * r1m5) - 3.0);
        exr = Math.cos(zr);
        exi = Math.sin(zr);
        ey = 0.0;
        tay = Math.abs(zi + zi);

        if (tay < elim) {
            ey = Math.exp( -tay);
        } // if (tay < elim)

        if (zi < 0.0) {
            c1r = exr;
            c1i = exi;
            c2r = exr * ey;
            c2i = -exi * ey;
        } // if (zi < 0.0)
        else { // zi >= 0.0
            c1r = exr * ey;
            c1i = exi * ey;
            c2r = exr;
            c2i = -exi;
        } // else zi >= 0.0

        nz[0] = 0;
        rtol = 1.0 / tol;
        ascle = 1.0E3 * tiny * rtol;

        for (i = 1; i <= n; i++) {
            aa = cwrkr[i - 1];
            bb = cwrki[i - 1];
            atol = 1.0;

            if (Math.max(Math.abs(aa), Math.abs(bb)) <= ascle) {
                aa = aa * rtol;
                bb = bb * rtol;
                atol = tol;
            } // if (Math.max(Math.abs(aa),Math.abs(bb)) <= ascle)

            str = ( (aa * c2r) - (bb * c2i)) * atol;
            sti = ( (aa * c2i) + (bb * c2r)) * atol;
            aa = cyr[i - 1];
            bb = cyi[i - 1];
            atol = 1.0;

            if (Math.max(Math.abs(aa), Math.abs(bb)) <= ascle) {
                aa = aa * rtol;
                bb = bb * rtol;
                atol = tol;
            } // if (Math.max(Math.abs(aa),Math.abs(bb)) <= ascle)

            str = str - ( ( (aa * c1r) - (bb * c1i)) * atol);
            sti = sti - ( ( (aa * c1i) + (bb * c1r)) * atol);
            cyr[i - 1] = -sti * hcii;
            cyi[i - 1] = str * hcii;

            if ( (str == 0.0) && (sti == 0.0) && (ey == 0.0)) {
                nz[0] = nz[0] + 1;
            } // if ((str == 0.0) && (sti== 0.0) && (ey == 0.0))
        } // for (i = 1; i <= n; i++)

        return;
    }

    /**
     * zbinu calculates the I function in the right half z plane.
     * 
     * @param zr double
     * @param zi double
     * @param fnu double
     * @param kode int
     * @param n int
     * @param cyr double[]
     * @param cyi double[]
     * @param nz int[]
     */
    private void zbinu(final double zr, final double zi, final double fnu, final int kode, final int n,
            final double[] cyr, final double[] cyi, final int[] nz) {
        double az;
        int nn;
        double dfnu;
        final int[] nw = new int[1];
        int inw;
        boolean doMiller = true;
        boolean doOverflow = true;
        boolean doTest = true;
        final double[] cwr = new double[2];
        final double[] cwi = new double[2];
        int i;
        int nui;
        final int[] nlast = new int[1];

        nz[0] = 0;
        az = zabs(zr, zi);
        nn = n;
        dfnu = fnu + n - 1.0;

        if ( (az <= 2.0) || ( (0.25 * az * az) <= (dfnu + 1.0))) {

            // power series
            zseri(zr, zi, fnu, kode, nn, cyr, cyi, nw);
            inw = Math.abs(nw[0]);
            nz[0] = nz[0] + inw;
            nn = nn - inw;

            if ( (nn == 0) || (nw[0] >= 0)) {
                return;
            }

            dfnu = fnu + nn - 1.0;
        } // if ((az <= 2.0) || (0.25*az*az <= (dfnu+1.0)))

        if ( (az >= rl) && ( (dfnu <= 1.0) || ( (dfnu > 1.0) && ( (2.0 * az) >= (dfnu * dfnu))))) {

            // Asymptotic expansion for large z
            zasyi(zr, zi, fnu, kode, nn, cyr, cyi, nw);

            if (nw[0] == -2) {
                nz[0] = -2;
            } else if (nw[0] < 0) {
                nz[0] = -1;
            }

            return;
        } // if ((az >= rl) && ((dfnu <= 1.0) ||
        // ((dfnu > 1.0) && ((2.0*az) >= (dfnu*dfnu)))))

        if ( ( (az >= rl) && (dfnu > 1.0) && ( (2.0 * az) < (dfnu * dfnu))) || ( (az < rl) && (dfnu > 1.0))) {

            // overflow and underflow test on I sequence for Miller algorithm
            zuoik(zr, zi, fnu, kode, 1, nn, cyr, cyi, nw);

            if (nw[0] == -2) {
                nz[0] = -2;

                return;
            } else if (nw[0] < 0) {
                nz[0] = -1;

                return;
            }

            nz[0] = nz[0] + nw[0];
            nn = nn - nw[0];

            if (nn == 0) {
                return;
            }

            dfnu = fnu + nn - 1.0;

            if ( (dfnu > fnul) || (az > fnul)) {
                doMiller = false;
                doOverflow = false;
            }
        } //
        else {
            doTest = false;
        }

        while (true) {

            if (doTest && (az > rl)) {
                doMiller = false;
            }

            if (doMiller) {

                // Miller algorithm normalized by the series
                zmlri(zr, zi, fnu, kode, nn, cyr, cyi, nw);

                if (nw[0] == -2) {
                    nz[0] = -2;
                } else if (nw[0] < 0) {
                    nz[0] = -1;
                }

                return;
            } // if (doMiller)

            if (doOverflow) {

                // Miller algorithm normalized by the Wronskian
                // Overflow test on K functions used in Wronskian
                zuoik(zr, zi, fnu, kode, 2, 2, cwr, cwi, nw);

                if (nw[0] < 0) {
                    nz[0] = nn;

                    for (i = 0; i < nn; i++) {
                        cyr[i] = 0.0;
                        cyi[i] = 0.0;
                    }

                    return;
                } // if (nw[0] < 0)
                else if (nw[0] > 0) {
                    nz[0] = -1;

                    return;
                }

                zwrsk(zr, zi, fnu, kode, nn, cyr, cyi, nw, cwr, cwi);

                if (nw[0] == -2) {
                    nz[0] = -2;
                } else if (nw[0] < 0) {
                    nz[0] = -1;
                }

                return;
            } // if (doOverflow)

            // Increment fnu + nn - 1 up to fnul, compute and recur
            // backwards
            nui = (int) (fnul - dfnu) + 1;
            nui = Math.max(nui, 0);
            zbuni(zr, zi, fnu, kode, nn, cyr, cyi, nw, nui, nlast, fnul);

            if (nw[0] == -2) {
                nz[0] = -2;

                return;
            } else if (nw[0] < 0) {
                nz[0] = -1;

                return;
            }

            nz[0] = nz[0] + nw[0];

            if (nlast[0] == 0) {
                return;
            }

            nn = nlast[0];
            doTest = true;
            doMiller = true;
            doOverflow = true;
        } // while (true)

    }

    /**
     * DOCUMENT ME!
     * 
     * @param zr double
     * @param zi double
     * @param id int
     * @param kode int
     * @param bir double[]
     * @param bii double[]
     * @param ierr int[]
     */
    private void zbiry(final double zr, final double zi, final int id, final int kode, final double[] bir,
            final double[] bii, final int[] ierr) {

        /**
         * *BEGIN PROLOGUE ZBIRY C***DATE WRITTEN 830501 (YYMMDD) C***REVISION DATE 890801, 930101 (YYMMDD) C***CATEGORY
         * NO. B5K C***KEYWORDS AIRY FUNCTION,BESSEL FUNCTIONS OF ORDER ONE THIRD C***AUTHOR AMOS, DONALD E., SANDIA
         * NATIONAL LABORATORIES C***PURPOSE TO COMPUTE AIRY FUNCTIONS BI(Z) AND DBI(Z) FOR COMPLEX Z C***DESCRIPTION C
         * C ***A DOUBLE PRECISION ROUTINE*** C ON KODE=1, CBIRY COMPUTES THE COMPLEX AIRY FUNCTION BI(Z) OR C ITS
         * DERIVATIVE DBI(Z)/DZ ON ID=0 OR ID=1 RESPECTIVELY. ON C KODE=2, A SCALING OPTION CEXP(-AXZTA)*BI(Z) OR
         * CEXP(-AXZTA)* C DBI(Z)/DZ IS PROVIDED TO REMOVE THE EXPONENTIAL BEHAVIOR IN C BOTH THE LEFT AND RIGHT HALF
         * PLANES WHERE C ZTA=(2/3)*Z*CSQRT(Z)=CMPLX(XZTA,YZTA) AND AXZTA=ABS(XZTA). C DEFINTIONS AND NOTATION ARE FOUND
         * IN THE NBS HANDBOOK OF C MATHEMATICAL FUNCTIONS (REF. 1). C C INPUT ZR,ZI ARE DOUBLE PRECISION C ZR,ZI -
         * Z=CMPLX(ZR,ZI) C ID - ORDER OF DERIVATIVE, ID=0 OR ID=1 C KODE - A PARAMETER TO INDICATE THE SCALING OPTION C
         * KODE= 1 RETURNS C BI=BI(Z) ON ID=0 OR C BI=DBI(Z)/DZ ON ID=1 C = 2 RETURNS C BI=CEXP(-AXZTA)*BI(Z) ON ID=0 OR
         * C BI=CEXP(-AXZTA)*DBI(Z)/DZ ON ID=1 WHERE C ZTA=(2/3)*Z*CSQRT(Z)=CMPLX(XZTA,YZTA) C AND AXZTA=ABS(XZTA) C C
         * OUTPUT BIR,BII ARE DOUBLE PRECISION C BIR,BII- COMPLEX ANSWER DEPENDING ON THE CHOICES FOR ID AND C KODE C
         * IERR - ERROR FLAG C IERR=0, NORMAL RETURN - COMPUTATION COMPLETED C IERR=1, INPUT ERROR - NO COMPUTATION C
         * IERR=2, OVERFLOW - NO COMPUTATION, REAL(Z) C TOO LARGE ON KODE=1 C IERR=3, CABS(Z) LARGE - COMPUTATION
         * COMPLETED C LOSSES OF SIGNIFCANCE BY ARGUMENT REDUCTION C PRODUCE LESS THAN HALF OF MACHINE ACCURACY C
         * IERR=4, CABS(Z) TOO LARGE - NO COMPUTATION C COMPLETE LOSS OF ACCURACY BY ARGUMENT C REDUCTION C IERR=5,
         * ERROR - NO COMPUTATION, C ALGORITHM TERMINATION CONDITION NOT MET C C***LONG DESCRIPTION C C BI AND DBI ARE
         * COMPUTED FOR CABS(Z).GT.1.0 FROM THE I BESSEL C FUNCTIONS BY C C BI(Z)=C*SQRT(Z)*( I(-1/3,ZTA) + I(1/3,ZTA) )
         * C DBI(Z)=C * Z * ( I(-2/3,ZTA) + I(2/3,ZTA) ) C C=1.0/SQRT(3.0) C ZTA=(2/3)*Z**(3/2) C C WITH THE POWER
         * SERIES FOR CABS(Z).LE.1.0. C C IN MOST COMPLEX VARIABLE COMPUTATION, ONE MUST EVALUATE ELE- C MENTARY
         * FUNCTIONS. WHEN THE MAGNITUDE OF Z IS LARGE, LOSSES C OF SIGNIFICANCE BY ARGUMENT REDUCTION OCCUR.
         * CONSEQUENTLY, IF C THE MAGNITUDE OF ZETA=(2/3)*Z**1.5 EXCEEDS U1=SQRT(0.5/UR), C THEN LOSSES EXCEEDING HALF
         * PRECISION ARE LIKELY AND AN ERROR C FLAG IERR=3 IS TRIGGERED WHERE UR=DMAX1(D1MACH(4),1.0D-18) IS C DOUBLE
         * PRECISION UNIT ROUNDOFF LIMITED TO 18 DIGITS PRECISION. C ALSO, IF THE MAGNITUDE OF ZETA IS LARGER THAN
         * U2=0.5/UR, THEN C ALL SIGNIFICANCE IS LOST AND IERR=4. IN ORDER TO USE THE INT C FUNCTION, ZETA MUST BE
         * FURTHER RESTRICTED NOT TO EXCEED THE C LARGEST INTEGER, U3=I1MACH(9). THUS, THE MAGNITUDE OF ZETA C MUST BE
         * RESTRICTED BY MIN(U2,U3). ON 32 BIT MACHINES, U1,U2, C AND U3 ARE APPROXIMATELY 2.0E+3, 4.2E+6, 2.1E+9 IN
         * SINGLE C PRECISION ARITHMETIC AND 1.3E+8, 1.8E+16, 2.1E+9 IN DOUBLE C PRECISION ARITHMETIC RESPECTIVELY. THIS
         * MAKES U2 AND U3 LIMIT- C ING IN THEIR RESPECTIVE ARITHMETICS. THIS MEANS THAT THE MAG- C NITUDE OF Z CANNOT
         * EXCEED 3.1E+4 IN SINGLE AND 2.1E+6 IN C DOUBLE PRECISION ARITHMETIC. THIS ALSO MEANS THAT ONE CAN C EXPECT TO
         * RETAIN, IN THE WORST CASES ON 32 BIT MACHINES, C NO DIGITS IN SINGLE PRECISION AND ONLY 7 DIGITS IN DOUBLE C
         * PRECISION ARITHMETIC. SIMILAR CONSIDERATIONS HOLD FOR OTHER C MACHINES. C C THE APPROXIMATE RELATIVE ERROR IN
         * THE MAGNITUDE OF A COMPLEX C BESSEL FUNCTION CAN BE EXPRESSED BY P*10**S WHERE P=MAX(UNIT C ROUNDOFF,1.0E-18)
         * IS THE NOMINAL PRECISION AND 10**S REPRE- C SENTS THE INCREASE IN ERROR DUE TO ARGUMENT REDUCTION IN THE C
         * ELEMENTARY FUNCTIONS. HERE, S=MAX(1,ABS(LOG10(CABS(Z))), C ABS(LOG10(FNU))) APPROXIMATELY (I.E.
         * S=MAX(1,ABS(EXPONENT OF C CABS(Z),ABS(EXPONENT OF FNU)) ). HOWEVER, THE PHASE ANGLE MAY C HAVE ONLY ABSOLUTE
         * ACCURACY. THIS IS MOST LIKELY TO OCCUR WHEN C ONE COMPONENT (IN ABSOLUTE VALUE) IS LARGER THAN THE OTHER BY C
         * SEVERAL ORDERS OF MAGNITUDE. IF ONE COMPONENT IS 10**K LARGER C THAN THE OTHER, THEN ONE CAN EXPECT ONLY
         * MAX(ABS(LOG10(P))-K, C 0) SIGNIFICANT DIGITS; OR, STATED ANOTHER WAY, WHEN K EXCEEDS C THE EXPONENT OF P, NO
         * SIGNIFICANT DIGITS REMAIN IN THE SMALLER C COMPONENT. HOWEVER, THE PHASE ANGLE RETAINS ABSOLUTE ACCURACY C
         * BECAUSE, IN COMPLEX ARITHMETIC WITH PRECISION P, THE SMALLER C COMPONENT WILL NOT (AS A RULE) DECREASE BELOW
         * P TIMES THE C MAGNITUDE OF THE LARGER COMPONENT. IN THESE EXTREME CASES, C THE PRINCIPAL PHASE ANGLE IS ON
         * THE ORDER OF +P, -P, PI/2-P, C OR -PI/2+P. C C***REFERENCES HANDBOOK OF MATHEMATICAL FUNCTIONS BY M.
         * ABRAMOWITZ C AND I. A. STEGUN, NBS AMS SERIES 55, U.S. DEPT. OF C COMMERCE, 1955. C C COMPUTATION OF BESSEL
         * FUNCTIONS OF COMPLEX ARGUMENT C AND LARGE ORDER BY D. E. AMOS, SAND83-0643, MAY, 1983 C C A SUBROUTINE
         * PACKAGE FOR BESSEL FUNCTIONS OF A COMPLEX C ARGUMENT AND NONNEGATIVE ORDER BY D. E. AMOS, SAND85- C 1018,
         * MAY, 1985 C C A PORTABLE PACKAGE FOR BESSEL FUNCTIONS OF A COMPLEX C ARGUMENT AND NONNEGATIVE ORDER BY D. E.
         * AMOS, ACM C TRANS. MATH. SOFTWARE, VOL. 12, NO. 3, SEPTEMBER 1986, C PP 265-273.
         */
        final double[] cyr = new double[2];
        final double[] cyi = new double[2];
        final double tth = 2.0 / 3.0;
        final double c1 = 6.14926627446000736E-01;
        final double c2 = 4.48288357353826359E-01;
        final double coef = 5.77350269189625765E-01;
        final int[] nz = new int[1];
        double az;
        double fid;
        double s1r;
        double s1i;
        double s2r;
        double s2i;
        double aa;
        double trm1r;
        double trm1i;
        double trm2r;
        double trm2i;
        double atrm;
        final double[] str = new double[1];
        final double[] sti = new double[1];
        double z3r;
        double z3i;
        double az3;
        double ak;
        double bk;
        double ck;
        double dk;
        double d1;
        double d2;
        double ad;
        int k;
        double ztar;
        double ztai;
        double eaa;
        double cc;
        double neweps;
        double dig;
        double bb;
        final double[] csqr = new double[1];
        final double[] csqi = new double[1];
        double sfac;
        double fmr;
        double fnu;
        int k1;

        ierr[0] = 0;
        nz[0] = 0;

        if ( (id < 0) || (id > 1)) {
            ierr[0] = 1;
        } // if ((id < 0) || (id > 1))

        if ( (kode < 1) || (kode > 2)) {
            ierr[0] = 1;
        } // if ((kode < 1) || (kode > 2)

        if (ierr[0] != 0) {
            return;
        } // if (ierr[0] != 0)

        az = zabs(zr, zi);

        // epsilon = D1MACH(4)
        // Machine epsilon is the smallest positive epsilon such that
        // (1.0 + epsilon) != 1.0.
        // epsilon = 2**(1 - doubleDigits) = 2**(1 - 53) = 2**(-52)
        // epsilon = 2.224460e-16
        // epsilon is called the largest relative spacing
        epsilon = 1.0;
        neweps = 1.0;

        while (true) {

            if (1.0 == (1.0 + neweps)) {
                break;
            } else {
                epsilon = neweps;
                neweps = neweps / 2.0;
            }
        }

        tol = Math.max(epsilon, 1.0e-18);
        fid = id;

        if (az <= 1.0) {

            // Power series for CABS(z) <= 1
            s1r = 1.0;
            s1i = 0.0;
            s2r = 1.0;
            s2i = 0.0;

            if (az < tol) {
                aa = (c1 * (1.0 - fid)) + (fid * c2);
                bir[0] = aa;
                bii[0] = 0.0;

                return;
            } // if (az < tol)

            aa = az * az;

            if (aa >= (tol / az)) {
                trm1r = 1.0;
                trm1i = 0.0;
                trm2r = 1.0;
                trm2i = 0.0;
                atrm = 1.0;
                str[0] = (zr * zr) - (zi * zi);
                sti[0] = 2.0 * zr * zi;
                z3r = (str[0] * zr) - (sti[0] * zi);
                z3i = (str[0] * zi) + (sti[0] * zr);
                az3 = az * aa;
                ak = 2.0 + fid;
                bk = 3.0 - fid - fid;
                ck = 4.0 - fid;
                dk = 3.0 + fid + fid;
                d1 = ak * dk;
                d2 = bk * ck;
                ad = Math.min(d1, d2);
                ak = 24.0 + (9.0 * fid);
                bk = 30.0 - (9.0 * fid);

                for (k = 1; k <= 25; k++) {
                    str[0] = ( (trm1r * z3r) - (trm1i * z3i)) / d1;
                    trm1i = ( (trm1r * z3i) + (trm1i * z3r)) / d1;
                    trm1r = str[0];
                    s1r = s1r + trm1r;
                    s1i = s1i + trm1i;
                    str[0] = ( (trm2r * z3r) - (trm2i * z3i)) / d2;
                    trm2i = ( (trm2r * z3i) + (trm2i * z3r)) / d2;
                    trm2r = str[0];
                    s2r = s2r + trm2r;
                    s2i = s2i + trm2i;
                    atrm = atrm * az3 / ad;
                    d1 = d1 + ak;
                    d2 = d2 + bk;
                    ad = Math.min(d1, d2);

                    if (atrm < (tol * ad)) {
                        break;
                    } // if (atrm < (tol*ad))

                    ak = ak + 18.0;
                    bk = bk + 18.0;
                } // for (k = 1; k <= 25; k++)
            } // if (aa >= (tol/az))

            if (id != 1) {
                bir[0] = (c1 * s1r) + (c2 * ( (zr * s2r) - (zi * s2i)));
                bii[0] = (c1 * s1i) + (c2 * ( (zr * s2i) + (zi * s2r)));

                if (kode == 1) {
                    return;
                } // if (kode == 1)

                zsqrt(zr, zi, str, sti);
                ztar = tth * ( (zr * str[0]) - (zi * sti[0]));
                ztai = tth * ( (zr * sti[0]) + (zi * str[0]));
                aa = ztar;
                aa = -Math.abs(aa);
                eaa = Math.exp(aa);
                bir[0] = bir[0] * eaa;
                bii[0] = bii[0] * eaa;

                return;
            } // if (id != 1)

            bir[0] = s2r * c2;
            bii[0] = s2i * c2;

            if (az > tol) {
                cc = c1 / (1.0 + fid);
                str[0] = (s1r * zr) - (s1i * zi);
                sti[0] = (s1r * zi) + (s1i * zr);
                bir[0] = bir[0] + (cc * ( (str[0] * zr) - (sti[0] * zi)));
                bii[0] = bii[0] + (cc * ( (str[0] * zi) + (sti[0] * zr)));
            } // if (az > tol)

            if (kode == 1) {
                return;
            } // if (kode == 1)

            zsqrt(zr, zi, str, sti);
            ztar = tth * ( (zr * str[0]) - (zi * sti[0]));
            ztai = tth * ( (zr * sti[0]) + (zi * str[0]));
            aa = ztar;
            aa = -Math.abs(aa);
            eaa = Math.exp(aa);
            bir[0] = bir[0] * eaa;
            bii[0] = bii[0] * eaa;

            return;
        } // if (az <= 1.0)

        // Case for CABS(z) > 1.0
        fnu = (1.0 + fid) / 3.0;

        /**
         * SET PARAMETERS RELATED TO MACHINE CONSTANTS. C TOL IS THE APPROXIMATE UNIT ROUNDOFF LIMITED TO 1.0E-18. C
         * ELIM IS THE APPROXIMATE EXPONENTIAL OVER- AND UNDERFLOW LIMIT. C EXP(-ELIM).LT.EXP(-ALIM)=EXP(-ELIM)/TOL AND
         * C EXP(ELIM).GT.EXP(ALIM)=EXP(ELIM)*TOL ARE INTERVALS NEAR C UNDERFLOW AND OVERFLOW LIMITS WHERE SCALED
         * ARITHMETIC IS DONE. C RL IS THE LOWER BOUNDARY OF THE ASYMPTOTIC EXPANSION FOR LARGE Z. C DIG = NUMBER OF
         * BASE 10 DIGITS IN TOL = 10**(-DIG).C FNUL IS THE LOWER BOUNDARY OF THE ASYMPTOTIC SERIES FOR LARGE FNU.
         */

        // emin, the smallest exponent E for double precision, is I1MACH(15)
        // tiny = D1MACH(1) = 2**(emin - 1) = 2**(-1022)
        // D1MACH(1) is the smallest normalized number, which preserves the
        // full precision of the mantissa.
        // Double.MIN_VALUE = 2**(-1074) is the smallest denormalized number,
        // which preseerves only a portion of the fraction's precision.
        emin = -1021;
        tiny = Math.pow(2, -1022);

        // emax, the largest exponent E for double precision, is I1MACH(16)
        // D1MACH(2) = 2**(emax)*(1 - 2**(-doubleDigits)) = 2**1024*(1 - 2**-53)
        // D1MACH(2) = Double.MAX_VALUE
        emax = 1024;

        // r1m5 = log10(2), which is D1MACH(5)
        r1m5 = 0.434294481903251 * Math.log(2.0);
        k = Math.min(Math.abs(emin), Math.abs(emax));
        elim = 2.303 * ( (k * r1m5) - 3.0);

        // doubleDigits is I1MACH(14), the number of base-B digits for double
        // precision. IEEE 754 double precision has 64 bits with 1 sign bit,
        // 11 exponent bits, and 52 fraction, mantissa, or signficand bits.
        // Floating-point numbers are typically stored in normalized form,
        // with the radix point after the first non-zero digit. In base two,
        // the only possible non-zero digit is 1. Thus, a leading digit of 1
        // can always be assumed and need not be explicitly represented.
        // Therefore, the mantissa effectively has 53 bits of resolution.
        doubleDigits = 53;
        k1 = doubleDigits - 1;
        aa = r1m5 * k1;
        dig = Math.min(aa, 18.0);
        aa = 2.303 * aa;
        alim = elim + Math.max( -aa, -41.45);
        rl = (1.2 * dig) + 3.0;
        fnul = 10.0 + (6.0 * (dig - 3.0));

        // Test for range
        aa = 0.5 / tol;

        // I1MACH(9) = Integer.MAX_VALUE;
        bb = 0.5 * Integer.MAX_VALUE;
        aa = Math.min(aa, bb);
        aa = Math.pow(aa, tth);

        if (az > aa) {
            ierr[0] = 4;
            nz[0] = 0;

            return;
        } // if (az > aa)

        aa = Math.sqrt(aa);

        if (az > aa) {
            ierr[0] = 3;
        } // if (az > aa)

        zsqrt(zr, zi, csqr, csqi);
        ztar = tth * ( (zr * csqr[0]) - (zi * csqi[0]));
        ztai = tth * ( (zr * csqi[0]) + (zi * csqr[0]));

        // Re(zta) <= 0 when Re(z) < 0, especially when Im(z) is small
        sfac = 1.0;
        ak = ztai;

        if (zr < 0.0) {
            bk = ztar;
            ck = -Math.abs(bk);
            ztar = ck;
            ztai = ak;
        } // if (zr < 0.0)

        if ( (zi == 0.0) && (zr <= 0.0)) {
            ztar = 0.0;
            ztai = ak;
        } // if ((zi == 0.0) && (zr <= 0.0))

        aa = ztar;

        if (kode != 2) {

            // Overflow test
            bb = Math.abs(aa);

            if (bb >= alim) {
                bb = bb + (0.25 * Math.log(az));
                sfac = tol;

                if (bb > elim) {
                    ierr[0] = 2;
                    nz[0] = 0;

                    return;
                } // if (bb > elim)
            } // if (bb >= alim)
        } // if (kode != 2)

        fmr = 0.0;

        if ( (aa < 0.0) || (zr <= 0.0)) {
            fmr = Math.PI;

            if (zi < 0.0) {
                fmr = -Math.PI;
            } // if (zi < 0.0)

            ztar = -ztar;
            ztai = -ztai;
        } // if ((aa < 0.0) || (zr <= 0.0))

        // aa = factor for analytic continuation of I(fnu,zeta)
        // kode = 2 returns exp(-abs(xzta))*I(fnu,zta) from zbesi
        zbinu(ztar, ztai, fnu, kode, 1, cyr, cyi, nz);

        if (nz[0] == -1) {
            ierr[0] = 2;
            nz[0] = 0;

            return;
        } // if (nz[0] == -1)
        else if (nz[0] < 0) {
            nz[0] = 0;
            ierr[0] = 5;

            return;
        } // else if (nz[0] < 0)

        aa = fmr * fnu;
        z3r = sfac;
        str[0] = Math.cos(aa);
        sti[0] = Math.sin(aa);
        s1r = ( (str[0] * cyr[0]) - (sti[0] * cyi[0])) * z3r;
        s1i = ( (str[0] * cyi[0]) + (sti[0] * cyr[0])) * z3r;
        fnu = (2.0 - fid) / 3.0;
        zbinu(ztar, ztai, fnu, kode, 2, cyr, cyi, nz);
        cyr[0] = cyr[0] * z3r;
        cyi[0] = cyi[0] * z3r;
        cyr[1] = cyr[1] * z3r;
        cyi[1] = cyi[1] * z3r;

        // Backward recur one step for orders -1/3 or -2/3
        zdiv(cyr[0], cyi[0], ztar, ztai, str, sti);
        s2r = ( (fnu + fnu) * str[0]) + cyr[1];
        s2i = ( (fnu + fnu) * sti[0]) + cyi[1];
        aa = fmr * (fnu - 1.0);
        str[0] = Math.cos(aa);
        sti[0] = Math.sin(aa);
        s1r = coef * (s1r + (s2r * str[0]) - (s2i * sti[0]));
        s1i = coef * (s1i + (s2r * sti[0]) + (s2i * str[0]));

        if (id != 1) {
            str[0] = (csqr[0] * s1r) - (csqi[0] * s1i);
            s1i = (csqr[0] * s1i) + (csqi[0] * s1r);
            s1r = str[0];
            bir[0] = s1r / sfac;
            bii[0] = s1i / sfac;

            return;
        } // if (id != 1)

        // id == 1
        str[0] = (zr * s1r) - (zi * s1i);
        s1i = (zr * s1i) + (zi * s1r);
        s1r = str[0];
        bir[0] = s1r / sfac;
        bii[0] = s1i / sfac;

        return;
    }

    /**
     * zbknu computes the k Bessel function in the right half z plane.
     * 
     * @param zr double
     * @param zi double
     * @param fnu double
     * @param kode int
     * @param n int
     * @param yr double[]
     * @param yi double[]
     * @param nz int[]
     */
    private void zbknu(double zr, double zi, final double fnu, final int kode, final int n, final double[] yr,
            final double[] yi, final int[] nz) {
        final int kmax = 30;
        final double ctwor = 2.0;
        final double r1 = 2.0;
        final double rthpi = 1.25331413731550025;
        final double spi = 1.90985931710274403;
        final double fpi = 1.89769999331517738;
        final double tth = 2.0 / 3.0;
        final double[] cc = new double[] {5.77215664901532861E-01, -4.20026350340952355E-02, -4.21977345555443367E-02,
                7.21894324666309954E-03, -2.15241674114950973E-04, -2.01348547807882387E-05, 1.13302723198169588E-06,
                6.11609510448141582E-09};
        double caz;
        double csclr;
        double crscr;
        final double[] cssr = new double[3];
        final double[] csrr = new double[3];
        final double[] bry = new double[3];
        int iflag;
        int koded;
        double rcaz;
        final double[] str = new double[1];
        final double[] sti = new double[1];
        double rzr;
        double rzi;
        int inu;
        double dnu;
        double dnu2 = 0.0;
        double fc;
        final double[] smur = new double[1];
        final double[] smui = new double[1];
        final int[] idum = new int[1];
        double fmur;
        double fmui;
        final double[] cshr = new double[1];
        final double[] cshi = new double[1];
        final double[] cchr = new double[1];
        final double[] cchi = new double[1];
        double a2;
        double t2;
        double t1;
        double ak = 0.0;
        double s;
        int k;
        double tm;
        double g1;
        double g2;
        final double[] fr = new double[1];
        final double[] fi = new double[1];
        double pr;
        double pi;
        final double[] ptr = new double[1];
        final double[] pti = new double[1];
        double qr;
        double qi;
        final double[] s1r = new double[1];
        final double[] s1i = new double[1];
        final double[] s2r = new double[1];
        final double[] s2i = new double[1];
        double a1;
        double ckr = 0.0;
        double cki = 0.0;
        double bk;
        final double[] czr = new double[1];
        final double[] czi = new double[1];
        double rak;
        final double[] yr0 = new double[1];
        final double[] yi0 = new double[1];
        int kflag = 0;
        final double[] coefr = new double[1];
        final double[] coefi = new double[1];
        double fhs = 0.0;
        double etest;
        double fk = 0.0;
        double fks;
        double cbr;
        double p2r;
        double p2i;
        double p1r;
        double p1i;
        double aa;
        double bb;
        double cbi;
        double csr;
        double csi;
        double zdr = 0.0;
        double zdi = 0.0;
        int inub = 0;
        double ascle;
        double p2m;
        int kk = 1;
        int i;
        double helim;
        double elm;
        double celmr;
        int ic;
        int j;
        double as;
        double alas;
        final int[] nw = new int[1];
        boolean seg1 = true;
        boolean seg2 = true;
        boolean seg3 = true;
        boolean seg4 = true;
        boolean seg5 = true;
        boolean seg6 = true;
        boolean seg7 = true;
        boolean seg8 = true;
        boolean seg9 = true;
        boolean seg10 = true;
        boolean seg11 = true;

        caz = zabs(zr, zi);
        csclr = 1.0 / tol;
        crscr = tol;
        cssr[0] = csclr;
        cssr[1] = 1.0;
        cssr[2] = crscr;
        csrr[0] = crscr;
        csrr[1] = 1.0;
        csrr[2] = csclr;
        bry[0] = 1.0E3 * tiny / tol;
        bry[1] = 1.0 / bry[0];
        bry[2] = Double.MAX_VALUE;
        nz[0] = 0;
        iflag = 0;
        koded = kode;
        rcaz = 1.0 / caz;
        str[0] = zr * rcaz;
        sti[0] = -zi * rcaz;
        rzr = (str[0] + str[0]) * rcaz;
        rzi = (sti[0] + sti[0]) * rcaz;
        inu = (int) (fnu + 0.5);
        dnu = fnu - inu;

        if (Math.abs(dnu) != 0.5) {
            dnu2 = 0.0;

            if (Math.abs(dnu) > tol) {
                dnu2 = dnu * dnu;
            } // if (Math.abs(dnu) > tol)

            if (caz <= r1) {

                // Series for CABS(z) <= r1
                fc = 1.0;
                zlog(rzr, rzi, smur, smui, idum);
                fmur = smur[0] * dnu;
                fmui = smui[0] * dnu;
                zshch(fmur, fmui, cshr, cshi, cchr, cchi);

                if (dnu != 0.0) {
                    fc = dnu * Math.PI;
                    fc = fc / Math.sin(fc);
                    smur[0] = cshr[0] / dnu;
                    smui[0] = cshi[0] / dnu;
                } // if (dnu != 0.0)

                a2 = 1.0 + dnu;

                // gam(1-z)*gam(1+z) = PI*z/sin(PI*z), t1 = 1/gam(1-dnu),
                // t2 = 1/(gam+dnu)
                t2 = Math.exp( -dgamln(a2, idum));
                t1 = 1.0 / (t2 * fc);

                if (Math.abs(dnu) <= 0.1) {

                    // Series for F0 to resolve indeterminancy for small ABS(dnu)
                    ak = 1.0;
                    s = cc[0];

                    for (k = 2; k <= 8; k++) {
                        ak = ak * dnu2;
                        tm = cc[k - 1] * ak;
                        s = s + tm;

                        if (Math.abs(tm) < tol) {
                            break;
                        } // if (math.abs(tm) < tol)
                    } // for (k = 2; k <= 8; k++)

                    g1 = -s;
                } // if (Math.abs(dnu) <= 0.1)
                else {
                    g1 = (t1 - t2) / (dnu + dnu);
                } // else

                g2 = 0.5 * (t1 + t2);
                fr[0] = fc * ( (cchr[0] * g1) + (smur[0] * g2));
                fi[0] = fc * ( (cchi[0] * g1) + (smui[0] * g2));
                zexp(fmur, fmui, str, sti);
                pr = 0.5 * str[0] / t2;
                pi = 0.5 * sti[0] / t2;
                zdiv(0.5, 0.0, str[0], sti[0], ptr, pti);
                qr = ptr[0] / t1;
                qi = pti[0] / t1;
                s1r[0] = fr[0];
                s1i[0] = fi[0];
                s2r[0] = pr;
                s2i[0] = pi;
                ak = 1.0;
                a1 = 1.0;
                ckr = 1.0;
                cki = 0.0;
                bk = 1.0 - dnu2;

                if ( (inu <= 0) && (n <= 1)) {

                    // Generate K(fnu,z), 0.0 <= fnu < 0.5 and n=1
                    if (caz >= tol) {
                        zmlt(zr, zi, zr, zi, czr, czi);
                        czr[0] = 0.25 * czr[0];
                        czi[0] = 0.25 * czi[0];
                        t1 = 0.25 * caz * caz;

                        while (true) {
                            fr[0] = ( (fr[0] * ak) + pr + qr) / bk;
                            fi[0] = ( (fi[0] * ak) + pi + qi) / bk;
                            str[0] = 1.0 / (ak - dnu);
                            pr = pr * str[0];
                            pi = pi * str[0];
                            str[0] = 1.0 / (ak + dnu);
                            qr = qr * str[0];
                            qi = qi * str[0];
                            str[0] = (ckr * czr[0]) - (cki * czi[0]);
                            rak = 1.0 / ak;
                            cki = ( (ckr * czi[0]) + (cki * czr[0])) * rak;
                            ckr = str[0] * rak;
                            s1r[0] = (ckr * fr[0]) - (cki * fi[0]) + s1r[0];
                            s1i[0] = (ckr * fi[0]) + (cki * fr[0]) + s1i[0];
                            a1 = a1 * t1 * rak;
                            bk = bk + ak + ak + 1.0;
                            ak = ak + 1.0;

                            if (a1 <= tol) {
                                break;
                            } // if (a1 <= tol)
                        } // while (true)
                    } // if (caz >= tol)

                    yr[0] = s1r[0];
                    yi[0] = s1i[0];

                    if (koded == 1) {
                        return;
                    } // if (koded == 1)

                    zexp(zr, zi, str, sti);
                    zmlt(s1r[0], s1i[0], str[0], sti[0], yr0, yi0);
                    yr[0] = yr0[0];
                    yi[0] = yi0[0];

                    return;
                } // if ((inu <= 0) && (n <= 1)

                if (caz >= tol) {
                    zmlt(zr, zi, zr, zi, czr, czi);
                    czr[0] = 0.25 * czr[0];
                    czi[0] = 0.25 * czi[0];
                    t1 = 0.25 * caz * caz;

                    while (true) {
                        fr[0] = ( (fr[0] * ak) + pr + qr) / bk;
                        fi[0] = ( (fi[0] * ak) + pi + qi) / bk;
                        str[0] = 1.0 / (ak - dnu);
                        pr = pr * str[0];
                        pi = pi * str[0];
                        str[0] = 1.0 / (ak + dnu);
                        qr = qr * str[0];
                        qi = qi * str[0];
                        str[0] = (ckr * czr[0]) - (cki * czi[0]);
                        rak = 1.0 / ak;
                        cki = ( (ckr * czi[0]) + (cki * czr[0])) * rak;
                        ckr = str[0] * rak;
                        s1r[0] = (ckr * fr[0]) - (cki * fi[0]) + s1r[0];
                        s1i[0] = (ckr * fi[0]) + (cki * fr[0]) + s1i[0];
                        str[0] = pr - (fr[0] * ak);
                        sti[0] = pi - (fi[0] * ak);
                        s2r[0] = (ckr * str[0]) - (cki * sti[0]) + s2r[0];
                        s2i[0] = (ckr * sti[0]) + (cki * str[0]) + s2i[0];
                        a1 = a1 * t1 * rak;
                        bk = bk + ak + ak + 1.0;
                        ak = ak + 1.0;

                        if (a1 <= tol) {
                            break;
                        } // if (a1 <= tol)
                    } // while (true)
                } // if (caz >= tol)

                kflag = 2;
                a1 = fnu + 1.0;
                ak = a1 * Math.abs(smur[0]);

                if (ak > alim) {
                    kflag = 3;
                } // if (ak > alim)

                str[0] = cssr[kflag - 1];
                p2r = s2r[0] * str[0];
                p2i = s2i[0] * str[0];
                zmlt(p2r, p2i, rzr, rzi, s2r, s2i);
                s1r[0] = s1r[0] * str[0];
                s1i[0] = s1i[0] * str[0];

                if (koded != 1) {
                    zexp(zr, zi, fr, fi);
                    zmlt(s1r[0], s1i[0], fr[0], fi[0], s1r, s1i);
                    zmlt(s2r[0], s2i[0], fr[0], fi[0], s2r, s2i);
                } // if (koded != 1)

                seg1 = false;
            } // if (caz <= r1)
        } // if (Math.abs(dnu) != 0.5)

        if (seg1) {

            // iflag = 0 means no underflow occurred
            // iflag = 1 means an underflow occurred - computation proceeds with
            // koded = 2 and a test for on scale values is made during forward
            // recursion
            zsqrt(zr, zi, str, sti);
            zdiv(rthpi, 0.0, str[0], sti[0], coefr, coefi);
            kflag = 2;

            if (koded != 2) {

                if (zr > alim) {

                    // scale by exp(z), iflag = 1 cases
                    koded = 2;
                    iflag = 1;
                    kflag = 2;
                } // if (zr > alim)
                else {
                    str[0] = Math.exp( -zr) * cssr[kflag - 1];
                    sti[0] = -str[0] * Math.sin(zi);
                    str[0] = str[0] * Math.cos(zi);
                    zmlt(coefr[0], coefi[0], str[0], sti[0], coefr, coefi);
                } // else
            } // if (koded != 2)

            if (Math.abs(dnu) == 0.5) {
                s1r[0] = coefr[0];
                s1i[0] = coefi[0];
                s2r[0] = coefr[0];
                s2i[0] = coefi[0];
                seg2 = false;
            } // if (Math.abs(dnu) == 0.5)
            else {

                // Miller algorithm for cabs(z) > r1
                ak = Math.cos(Math.PI * dnu);
                ak = Math.abs(ak);

                if (ak == 0.0) {
                    s1r[0] = coefr[0];
                    s1i[0] = coefi[0];
                    s2r[0] = coefr[0];
                    s2i[0] = coefi[0];
                    seg2 = false;
                } // if (ak == 0.0)
                else {
                    fhs = Math.abs(0.25 - dnu2);

                    if (fhs == 0.0) {
                        s1r[0] = coefr[0];
                        s1i[0] = coefi[0];
                        s2r[0] = coefr[0];
                        s2i[0] = coefi[0];
                        seg2 = false;
                    } // if (fhs == 0.0)
                } // else
            } // else

            if (seg2) {

                // Compute r2 = F(e). If CABS(Z) >= r2, use forward recurrence to
                // determine the bakcward index k. r2 = F(e) is a straight line on
                // 12 <= e <= 60. e is computed from 2**(-e) = 2**(1 - I1MACH(14)) =
                // tol
                t1 = doubleDigits - 1.0;
                t1 = t1 * r1m5 * 3.321928094;
                t1 = Math.max(t1, 12.0);
                t1 = Math.min(t1, 60.0);
                t2 = (tth * t1) - 6.0;

                if (zr == 0.0) {
                    t1 = Math.PI / 2.0;
                } // if (zr == 0.0)
                else { // zr != 0.0
                    t1 = Math.atan(zi / zr);
                    t1 = Math.abs(t1);
                } // else zr != 0.0

                if (t2 <= caz) {

                    // Forward recurrence loop when cabs(z) >= r2
                    etest = ak / (Math.PI * caz * tol);
                    fk = 1.0;

                    if (etest < 1.0) {
                        seg3 = false;
                        seg4 = false;
                    }

                    if (seg3) {
                        fks = ctwor;
                        ckr = caz + caz + ctwor;
                        p1r = 0.0;
                        p2r = 1.0;

                        group: {

                            for (i = 1; i <= kmax; i++) {
                                ak = fhs / fks;
                                cbr = ckr / (fk + 1.0);
                                ptr[0] = p2r;
                                p2r = (cbr * p2r) - (p1r * ak);
                                p1r = ptr[0];
                                ckr = ckr + ctwor;
                                fks = fks + fk + fk + ctwor;
                                fhs = fhs + fk + fk;
                                fk = fk + 1.0;
                                str[0] = Math.abs(p2r) * fk;

                                if (etest < str[0]) {
                                    break group;
                                }
                            } // for (i = 1; i <= kmax; i++)

                            nz[0] = -2;

                            return;
                        } // group

                        fk = fk + (spi * t1 * Math.sqrt(t2 / caz));
                        fhs = Math.abs(0.25 - dnu2);
                        seg4 = false;
                    } // if (seg3)

                    seg3 = true;
                } // if (t2 <= caz)

                if (seg4) {

                    // Compute backward index k for CABS(z) < r2
                    a2 = Math.sqrt(caz);
                    ak = fpi * ak / (tol * Math.sqrt(a2));
                    aa = 3.0 * t1 / (1.0 + caz);
                    bb = 14.7 * t1 / (28.0 + caz);
                    ak = (Math.log(ak) + (caz * Math.cos(aa) / (1.0 + (0.008 * caz)))) / Math.cos(bb);
                    fk = (0.12125 * ak * ak / caz) + 1.5;
                } // if (seg4)

                seg4 = true;

                // Backward recurrence loop for Miller algorithm
                k = (int) (fk);
                fk = k;
                fks = fk * fk;
                p1r = 0.0;
                p1i = 0.0;
                p2r = tol;
                p2i = 0.0;
                csr = p2r;
                csi = p2i;

                for (i = 1; i <= k; i++) {
                    a1 = fks - fk;
                    ak = (fks + fk) / (a1 + fhs);
                    rak = 2.0 / (fk + 1.0);
                    cbr = (fk + zr) * rak;
                    cbi = zi * rak;
                    ptr[0] = p2r;
                    pti[0] = p2i;
                    p2r = ( (ptr[0] * cbr) - (pti[0] * cbi) - p1r) * ak;
                    p2i = ( (pti[0] * cbr) + (ptr[0] * cbi) - p1i) * ak;
                    p1r = ptr[0];
                    p1i = pti[0];
                    csr = csr + p2r;
                    csi = csi + p2i;
                    fks = a1 - fk + 1.0;
                    fk = fk - 1.0;
                } // for (i = 1; i <= k; i++)

                // Compute (p2/cs) = (p2/CABS(cs))*(CONJG(cs)/CABS(cs)) for better
                // scaling
                tm = zabs(csr, csi);
                ptr[0] = 1.0 / tm;
                s1r[0] = p2r * ptr[0];
                s1i[0] = p2i * ptr[0];
                csr = csr * ptr[0];
                csi = -csi * ptr[0];
                zmlt(coefr[0], coefi[0], s1r[0], s1i[0], str, sti);
                zmlt(str[0], sti[0], csr, csi, s1r, s1i);

                if ( (inu <= 0) && (n <= 1)) {
                    zdr = zr;
                    zdi = zi;
                    seg5 = false;
                    seg6 = false;
                    seg7 = false;

                    if (iflag == 1) {
                        seg8 = false;
                        seg9 = false;
                        seg10 = false;
                    } // if (iflag == 1)
                } // if ((inu <= 0) && (n <= 1))

                if (seg5) {

                    // Compute p1/p2 = (p1/CABS(p2)*CONJG(p2)/CABS(p2) for scaling
                    tm = zabs(p2r, p2i);
                    ptr[0] = 1.0 / tm;
                    p1r = p1r * ptr[0];
                    p1i = p1i * ptr[0];
                    p2r = p2r * ptr[0];
                    p2i = -p2i * ptr[0];
                    zmlt(p1r, p1i, p2r, p2i, ptr, pti);
                    str[0] = dnu + 0.5 - ptr[0];
                    sti[0] = -pti[0];
                    zdiv(str[0], sti[0], zr, zi, str, sti);
                    str[0] = str[0] + 1.0;
                    zmlt(str[0], sti[0], s1r[0], s1i[0], s2r, s2i);
                } // if (seg5)

                seg5 = true;
            } // if (seg2)

            seg2 = true;
        } // if (seg1)

        seg1 = true;

        // Forward recursion on the three term recursion with relation with
        // scaling near exponent extremes on kflag = 1 or kflag = 3
        if (seg6) {
            str[0] = dnu + 1.0;
            ckr = str[0] * rzr;
            cki = str[0] * rzi;

            if (n == 1) {
                inu = inu - 1;
            } // if (n == 1)

            if (inu <= 0) {

                if (n <= 1) {
                    s1r[0] = s2r[0];
                    s1i[0] = s2i[0];
                } // if (n <= 1)

                zdr = zr;
                zdi = zi;
                seg7 = false;

                if (iflag == 1) {
                    seg8 = false;
                    seg9 = false;
                    seg10 = false;
                } // if (iflag == 1)
            } // if (inu <= 0)
            else { // inu > 0
                inub = 1;

                if (iflag == 1) {
                    seg7 = false;
                    seg8 = false;
                    seg9 = false;
                } // if (iflag == 1)
            } // else inu > 0
        } // if (seg6)

        seg6 = true;

        while (true) {

            if (seg7) {
                p1r = csrr[kflag - 1];
                ascle = bry[kflag - 1];

                for (i = inub; i <= inu; i++) {
                    str[0] = s2r[0];
                    sti[0] = s2i[0];
                    s2r[0] = (ckr * str[0]) - (cki * sti[0]) + s1r[0];
                    s2i[0] = (ckr * sti[0]) + (cki * str[0]) + s1i[0];
                    s1r[0] = str[0];
                    s1i[0] = sti[0];
                    ckr = ckr + rzr;
                    cki = cki + rzi;

                    if (kflag >= 3) {
                        continue;
                    } // if (kflag >= 3)

                    p2r = s2r[0] * p1r;
                    p2i = s2i[0] * p1r;
                    str[0] = Math.abs(p2r);
                    sti[0] = Math.abs(p2i);
                    p2m = Math.max(str[0], sti[0]);

                    if (p2m <= ascle) {
                        continue;
                    } // if (p2m <= ascle)

                    kflag = kflag + 1;
                    ascle = bry[kflag - 1];
                    s1r[0] = s1r[0] * p1r;
                    s1i[0] = s1i[0] * p1r;
                    s2r[0] = p2r;
                    s2i[0] = p2i;
                    str[0] = cssr[kflag - 1];
                    s1r[0] = s1r[0] * str[0];
                    s1i[0] = s1i[0] * str[0];
                    s2r[0] = s2r[0] * str[0];
                    s2i[0] = s2i[0] * str[0];
                    p1r = csrr[kflag - 1];
                } // for (i = inub; i<= inu; i++)

                if (n == 1) {
                    s1r[0] = s2r[0];
                    s1i[0] = s2i[0];
                } // if (n == 1)
            } // if (seg7)

            seg7 = true;

            if (seg8) {
                str[0] = csrr[kflag - 1];
                yr[0] = s1r[0] * str[0];
                yi[0] = s1i[0] * str[0];

                if (n == 1) {
                    return;
                } // if (n == 1)

                yr[1] = s2r[0] * str[0];
                yi[1] = s2i[0] * str[0];

                if (n == 2) {
                    return;
                }

                kk = 2;
            } // if (seg8)

            seg8 = true;

            if (seg9) {
                kk = kk + 1;

                if (kk > n) {
                    return;
                } // if (kk > n)

                p1r = csrr[kflag - 1];
                ascle = bry[kflag - 1];

                for (i = kk; i <= n; i++) {
                    p2r = s2r[0];
                    p2i = s2i[0];
                    s2r[0] = (ckr * p2r) - (cki * p2i) + s1r[0];
                    s2i[0] = (cki * p2r) + (ckr * p2i) + s1i[0];
                    s1r[0] = p2r;
                    s1i[0] = p2i;
                    ckr = ckr + rzr;
                    cki = cki + rzi;
                    p2r = s2r[0] * p1r;
                    p2i = s2i[0] * p1r;
                    yr[i - 1] = p2r;
                    yi[i - 1] = p2i;

                    if (kflag >= 3) {
                        continue;
                    } // if (kflag >= 3)

                    str[0] = Math.abs(p2r);
                    sti[0] = Math.abs(p2i);
                    p2m = Math.max(str[0], sti[0]);

                    if (p2m <= ascle) {
                        continue;
                    } // if (p2m <= ascle)

                    kflag = kflag + 1;
                    ascle = bry[kflag - 1];
                    s1r[0] = s1r[0] * p1r;
                    s1i[0] = s1i[0] * p1r;
                    s2r[0] = p2r;
                    s2i[0] = p2i;
                    str[0] = cssr[kflag - 1];
                    s1r[0] = s1r[0] * str[0];
                    s1i[0] = s1i[0] * str[0];
                    s2r[0] = s2r[0] * str[0];
                    s2i[0] = s2i[0] * str[0];
                    p1r = csrr[kflag - 1];
                } // for (i = kk; i <= n; i++)

                return;
            } // if (seg9)

            seg9 = true;

            // iflag = 1 cases, forward recurrence on scaled values on underflow
            if (seg10) {
                helim = 0.5 * elim;
                elm = Math.exp( -elim);
                celmr = elm;
                ascle = bry[0];
                zdr = zr;
                zdi = zi;
                ic = -1;
                j = 2;

                group2: {

                    for (i = 1; i <= inu; i++) {
                        str[0] = s2r[0];
                        sti[0] = s2i[0];
                        s2r[0] = (str[0] * ckr) - (sti[0] * cki) + s1r[0];
                        s2i[0] = (sti[0] * ckr) + (str[0] * cki) + s1i[0];
                        s1r[0] = str[0];
                        s1i[0] = sti[0];
                        ckr = ckr + rzr;
                        cki = cki + rzi;
                        as = zabs(s2r[0], s2i[0]);
                        alas = Math.log(as);
                        p2r = -zdr + alas;

                        if (p2r >= ( -elim)) {
                            zlog(s2r[0], s2i[0], str, sti, idum);
                            p2r = -zdr + str[0];
                            p2i = -zdi + sti[0];
                            p2m = Math.exp(p2r) / tol;
                            p1r = p2m * Math.cos(p2i);
                            p1i = p2m * Math.sin(p2i);
                            zuchk(p1r, p1i, nw, ascle);

                            if (nw[0] == 0) {
                                j = 3 - j;
                                cyr[j - 1] = p1r;
                                cyi[j - 1] = p1i;

                                if (ic == (i - 1)) {
                                    break group2;
                                } // if (ic == (i-1))

                                ic = i;

                                continue;
                            } // if (nw[0] == 0)
                        } // if (p2r >= (-elim))

                        if (alas < helim) {
                            continue;
                        } // if (alas < helim)

                        zdr = zdr - elim;
                        s1r[0] = s1r[0] * celmr;
                        s1i[0] = s1i[0] * celmr;
                        s2r[0] = s2r[0] * celmr;
                        s2i[0] = s2i[0] * celmr;
                    } // for (i = 1; i <= inu; i++)

                    if (n == 1) {
                        s1r[0] = s2r[0];
                        s1i[0] = s2i[0];
                    } // if (n == 1)

                    seg11 = false;
                } // group2

                if (seg11) {
                    kflag = 1;
                    inub = i + 1;
                    s2r[0] = cyr[j - 1];
                    s2i[0] = cyi[j - 1];
                    j = 3 - j;
                    s1r[0] = cyr[j - 1];
                    s1i[0] = cyi[j - 1];

                    if (inub <= inu) {
                        continue;
                    } // if (inub <= inu)

                    if (n != 1) {
                        seg7 = false;

                        continue;
                    } // if (n != 1)

                    s1r[0] = s2r[0];
                    s1i[0] = s2i[0];
                    seg7 = false;

                    continue;
                } // if (seg11)

                seg11 = true;
            } // if (seg10)

            seg10 = true;
            yr[0] = s1r[0];
            yi[0] = s1i[0];

            if (n != 1) {
                yr[1] = s2r[0];
                yi[1] = s2i[0];
            } // if (n != 1)

            ascle = bry[0];
            zkscl(zdr, zdi, fnu, n, yr, yi, nz, rzr, rzi, ascle);
            inu = n - nz[0];

            if (inu <= 0) {
                return;
            } // if (inu <= 0)

            kk = nz[0] + 1;
            s1r[0] = yr[kk - 1];
            s1i[0] = yi[kk - 1];
            yr[kk - 1] = s1r[0] * csrr[0];
            yi[kk - 1] = s1i[0] * csrr[0];

            if (inu == 1) {
                return;
            } // if (inu == 1)

            kk = nz[0] + 2;
            s2r[0] = yr[kk - 1];
            s2i[0] = yi[kk - 1];
            yr[kk - 1] = s2r[0] * csrr[0];
            yi[kk - 1] = s2i[0] * csrr[0];

            if (inu == 2) {
                return;
            } // if (inu == 2)

            t2 = fnu + kk - 1.0;
            ckr = t2 * rzr;
            cki = t2 * rzi;
            kflag = 1;
            seg7 = false;
            seg8 = false;
        } // while (true)
    }

    /**
     * ZBUNI COMPUTES THE I BESSEL FUNCTION FOR LARGE CABS(Z).GT. FNUL AND fnu+N-1.LT.FNUL. THE ORDER IS INCREASED FROM
     * fnu+N-1 GREATER THAN FNUL BY ADDING NUI AND COMPUTING ACCORDING TO THE UNIFORM ASYMPTOTIC EXPANSION FOR I(fnu,Z)
     * ON IFORM=1 AND THE EXPANSION FOR J(fnu,Z) ON IFORM=2
     * 
     * @param zr double
     * @param zi double
     * @param fnu double
     * @param kode int
     * @param n int
     * @param yr double[]
     * @param yi double[]
     * @param nz int[]
     * @param nui int
     * @param nlast int[]
     * @param fnul double
     */
    private void zbuni(final double zr, double zi, final double fnu, final int kode, final int n, final double[] yr,
            final double[] yi, final int[] nz, final int nui, final int[] nlast, final double fnul) {
        double ax;
        double ay;
        int iform;
        double fnui;
        double dfnu;
        final double[] cyr = new double[2];
        final double[] cyi = new double[2];
        final double[] bry = new double[3];
        double gnu;
        final int[] nw = new int[1];
        double str;
        int iflag;
        double ascle;
        double csclr;
        double cscrr;
        double s1r;
        double s1i;
        double s2r;
        double s2i;
        double raz;
        double sti;
        double rzr;
        double rzi;
        double c1r;
        double c1i;
        double c1m;
        int nl;
        int k;
        int i;

        nz[0] = 0;
        ax = 1.7321 * Math.abs(zr);
        ay = Math.abs(zi);
        iform = 1;

        if (ay > ax) {
            iform = 2;
        } // if (ay > ax)

        if (nui == 0) {

            if (iform != 2) {

                // Asymptotic expansion for I(fnu,z) for large fnu applied in
                // -PI/3 <= arg(z) <= PI/3
                zuni1(zr, zi, fnu, kode, n, yr, yi, nw, nlast, fnul);
            } // if (iform != 2)
            else { // iform == 2

                // Asymptotic expansion for J(fnu,z*exp(m*hpi)) for large fnu
                // applied in PI/3 < Math.abs(arg(z)) <= PI/2 where m = +i or -i
                // and hpi = PI/2
                zuni2(zr, zi, fnu, kode, n, yr, yi, nw, nlast, fnul);
            } // else iform == 2

            if (nw[0] == -2) {
                nz[0] = -2;

                return;
            } // if (nw[0] == -2)
            else if (nw[0] < 0) {
                nz[0] = -1;

                return;
            } // else if (nw[0] < 0)
            else {
                nz[0] = nw[0];

                return;
            } // else
        } // if (nui == 0)

        fnui = nui;
        dfnu = fnu + n - 1.0;
        gnu = dfnu + fnui;

        if (iform != 2) {

            // Asymptotic expansion for I(fnu,z) for large fnu applied in
            // -PI/3 <= arg(z) <= PI/3
            zuni1(zr, zi, gnu, kode, 2, cyr, cyi, nw, nlast, fnul);
        } // if (iform != 2)
        else { // iform == 2

            // Asymptotic expansion for J(fnu,z*exp(m*hpi) for large fnu
            // applied in PI/3 < Math.abs(arg(z)) <= PI/2 where M = +i or -i
            // and hpi = PI/2
            zuni2(zr, zi, gnu, kode, 2, cyr, cyi, nw, nlast, fnul);
        } // else iform == 2

        if (nw[0] == -2) {
            nz[0] = -2;

            return;
        } // if (nw[0] == -2)
        else if (nw[0] < 0) {
            nz[0] = -1;

            return;
        } // else if (nw[0] < 0)
        else if (nw[0] != 0) {
            nlast[0] = n;

            return;
        } // else if (nw[0] != 0)

        str = zabs(cyr[0], cyi[0]);

        // Scale backward recurrence, bry[2] is defined but never used
        bry[0] = 1.0E3 * tiny / tol;
        bry[1] = 1.0 / bry[0];
        bry[2] = bry[1];
        iflag = 2;
        ascle = bry[1];
        csclr = 1.0;

        if (str <= bry[0]) {
            iflag = 1;
            ascle = bry[0];
            csclr = 1.0 / tol;
        } // if (str <= bry[0])
        else if (str >= bry[1]) {
            iflag = 3;
            ascle = bry[2];
            csclr = tol;
        } // else if (str >= bry[1])

        cscrr = 1.0 / csclr;
        s1r = cyr[1] * csclr;
        s1i = cyi[1] * csclr;
        s2r = cyr[0] * csclr;
        s2i = cyi[0] * csclr;
        raz = 1.0 / zabs(zr, zi);
        str = zr * raz;
        sti = -zi * raz;
        rzr = (str + str) * raz;
        rzi = (sti + sti) * raz;

        for (i = 1; i <= nui; i++) {
            str = s2r;
            sti = s2i;
            s2r = ( (dfnu + fnui) * ( (rzr * str) - (rzi * sti))) + s1r;
            s2i = ( (dfnu + fnui) * ( (rzr * sti) + (rzi * str))) + s1i;
            s1r = str;
            s1i = sti;
            fnui = fnui - 1.0;

            if (iflag >= 3) {
                continue;
            } // if (iflag >= 3)

            str = s2r * cscrr;
            sti = s2i * cscrr;
            c1r = Math.abs(str);
            c1i = Math.abs(sti);
            c1m = Math.max(c1r, c1i);

            if (c1m <= ascle) {
                continue;
            } // if (c1m <= ascle)

            iflag = iflag + 1;
            ascle = bry[iflag - 1];
            s1r = s1r * cscrr;
            s1i = s1i * cscrr;
            s2r = str;
            s2i = sti;
            csclr = csclr * tol;
            cscrr = 1.0 / csclr;
            s1r = s1r * csclr;
            s1i = s1i * csclr;
            s2r = s2r * csclr;
            s2i = s2i * csclr;
        } // for (i = 1; i <= nui; i++)

        yr[n - 1] = s2r * cscrr;
        yi[n - 1] = s2i * cscrr;

        if (n == 1) {
            return;
        } // if (n == 1)

        nl = n - 1;
        fnui = nl;
        k = nl;

        for (i = 1; i <= nl; i++) {
            str = s2r;
            sti = s2i;
            s2r = ( (fnu + fnui) * ( (rzr * str) - (rzi * sti))) + s1r;
            s2i = ( (fnu + fnui) * ( (rzr * sti) + (rzi * str))) + s1i;
            s1r = str;
            s1i = sti;
            str = s2r * cscrr;
            sti = s2i * cscrr;
            yr[k - 1] = str;
            yi[k - 1] = sti;
            fnui = fnui - 1.0;
            k = k - 1;

            if (iflag >= 3) {
                continue;
            } // if (iflag >= 3)

            c1r = Math.abs(str);
            c1i = Math.abs(sti);
            c1m = Math.max(c1r, c1i);

            if (c1m <= ascle) {
                continue;
            } // if (c1m <= ascle)

            iflag = iflag + 1;
            ascle = bry[iflag - 1];
            s1r = s1r * cscrr;
            s1i = s1i * cscrr;
            s2r = str;
            s2i = sti;
            csclr = csclr * tol;
            cscrr = 1.0 / csclr;
            s1r = s1r * csclr;
            s1i = s1i * csclr;
            s2r = s2r * csclr;
            s2i = s2i * csclr;
        } // for (i = 1; i <= nl i++)

        return;
    }

    /**
     * ZBUNK COMPUTES THE K BESSEL FUNCTION FOR FNU.GT.FNUL. ACCORDING TO THE UNIFORM ASYMPTOTIC EXPANSION FOR K(FNU,Z)
     * IN ZUNK1 AND THE EXPANSION FOR H(2,FNU,Z) IN ZUNK2
     * 
     * @param zr double
     * @param zi double
     * @param fnu double
     * @param kode int
     * @param mr int
     * @param n int
     * @param yr double[]
     * @param yi double[]
     * @param nz int[]
     */
    private void zbunk(final double zr, final double zi, final double fnu, final int kode, final int mr, final int n,
            final double[] yr, final double[] yi, final int[] nz) {
        double ax;
        double ay;

        nz[0] = 0;
        ax = 1.7321 * Math.abs(zr);
        ay = Math.abs(zi);

        if (ay <= ax) {

            // Asymptotic expansion for K(fnu,z) for large fnu applied in
            // -PI/3 <= arg(z) <= PI/3
            zunk1(zr, zi, fnu, kode, mr, n, yr, yi, nz);

            return;
        } // if (ay <= ax)

        // Asymptotic expansion for H(2,fnu,z*exp*(M*HPI)) for large fnu
        // applied in PI/3 < abs(arg(z) <= PI/2 where M = +i or -i
        // and HPI = PI/2
        zunk2(zr, zi, fnu, kode, mr, n, yr, yi, nz);

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
     * complex exponential function b = exp(a).
     * 
     * @param ar double
     * @param ai double
     * @param br double[]
     * @param bi double[]
     */
    private void zexp(final double ar, final double ai, final double[] br, final double[] bi) {
        double zm, ca, cb;
        zm = Math.exp(ar);
        ca = zm * Math.cos(ai);
        cb = zm * Math.sin(ai);
        br[0] = ca;
        bi[0] = cb;

        return;
    }

    /**
     * Set K functions to zero on underflow, continue recurrence on scaled functions until two members come on scale,
     * then return with min(zn[0]+2,n) values scaled by 1/tol.
     * 
     * @param zrr double
     * @param zri double
     * @param fnu double
     * @param n int
     * @param yr double[]
     * @param yi double[]
     * @param nz int[]
     * @param rzr double
     * @param rzi double
     * @param ascle double
     */
    private void zkscl(double zrr, final double zri, final double fnu, final int n, final double[] yr,
            final double[] yi, final int[] nz, final double rzr, final double rzi, final double ascle) {
        int ic;
        int nn;
        int i;
        double s1r;
        double s1i;
        final double[] cyr = new double[2];
        final double[] cyi = new double[2];
        double as;
        double acs;
        final double[] csr = new double[1];
        final double[] csi = new double[1];
        final int[] idum = new int[1];
        double str;
        double fn;
        double ckr;
        double cki;
        double s2r;
        double s2i;
        double helim;
        double elm;
        double celmr;
        double zdr;
        double zdi;
        int kk;
        double alas;
        final int[] nw = new int[1];
        int i2;

        nz[0] = 0;
        ic = 0;
        nn = Math.min(2, n);

        for (i = 1; i <= nn; i++) {
            s1r = yr[i - 1];
            s1i = yi[i - 1];
            cyr[i - 1] = s1r;
            cyi[i - 1] = s1i;
            as = zabs(s1r, s1i);
            acs = -zrr + Math.log(as);
            nz[0] = nz[0] + 1;
            yr[i - 1] = 0.0;
            yi[i - 1] = 0.0;

            if (acs < ( -elim)) {
                continue;
            } // if (acs < (-elim))

            zlog(s1r, s1i, csr, csi, idum);
            csr[0] = csr[0] - zrr;
            csi[0] = csi[0] - zri;
            str = Math.exp(csr[0]) / tol;
            csr[0] = str * Math.cos(csi[0]);
            csi[0] = str * Math.sin(csi[0]);
            zuchk(csr[0], csi[0], nw, ascle);

            if (nw[0] != 0) {
                continue;
            } // if (nw[0] != 0)

            yr[i - 1] = csr[0];
            yi[i - 1] = csi[0];
            ic = i;
            nz[0] = nz[0] - 1;
        } // for (i = 1; i <= nn; i++)

        if (n == 1) {
            return;
        } // if (n == 1)

        if (ic <= 1) {
            yr[0] = 0.0;
            yi[0] = 0.0;
            nz[0] = 2;
        } // if (ic <= 1)

        if ( (n == 2) || (nz[0] == 0)) {
            return;
        } // if ((n == 2) || (nz[0] == 0))

        fn = fnu + 1.0;
        ckr = fn * rzr;
        cki = fn * rzi;
        s1r = cyr[0];
        s1i = cyi[0];
        s2r = cyr[1];
        s2i = cyi[1];
        helim = 0.5 * elim;
        elm = Math.exp( -elim);
        celmr = elm;
        zdr = zrr;
        zdi = zri;

        // Find two consecutive Y values on scale. Scale recurrence if s2
        // gets larger than exp(elim/2)
        for (i = 3; i <= n; i++) {
            kk = i;
            csr[0] = s2r;
            csi[0] = s2i;
            s2r = (ckr * csr[0]) - (cki * csi[0]) + s1r;
            s2i = (cki * csr[0]) + (ckr * csi[0]) + s1i;
            s1r = csr[0];
            s1i = csi[0];
            ckr = ckr + rzr;
            cki = cki + rzi;
            as = zabs(s2r, s2i);
            alas = Math.log(as);
            acs = -zdr + alas;
            nz[0] = nz[0] + 1;
            yr[i - 1] = 0.0;
            yi[i - 1] = 0.0;

            if (acs >= ( -elim)) {
                zlog(s2r, s2i, csr, csi, idum);
                csr[0] = csr[0] - zdr;
                csi[0] = csi[0] - zdi;
                str = Math.exp(csr[0]) / tol;
                csr[0] = str * Math.cos(csi[0]);
                csi[0] = str * Math.sin(csi[0]);
                zuchk(csr[0], csi[0], nw, ascle);

                if (nw[0] == 0) {
                    yr[i - 1] = csr[0];
                    yi[i - 1] = csi[0];
                    nz[0] = nz[0] - 1;

                    if (ic == (kk - 1)) {
                        nz[0] = kk - 2;

                        for (i2 = 1; i2 <= nz[0]; i2++) {
                            yr[i2 - 1] = 0.0;
                            yi[i2 - 1] = 0.0;
                        } // for (i2 = 1; i2 <= nz[0]; i2++)

                        return;
                    } // if (ic == (kk-1)

                    ic = kk;

                    continue;
                } // if (nw[0] == 0)
            } // if (acs >= (-elim))

            if (alas < helim) {
                continue;
            } // if (alas < helim)

            zdr = zdr - elim;
            s1r = s1r * celmr;
            s1i = s1i * celmr;
            s2r = s2r * celmr;
            s2i = s2i * celmr;
        } // for (i = 3; i <= n; i++)

        nz[0] = n;

        if (ic == n) {
            nz[0] = n - 1;
        } // if (ic == n)

        for (i = 1; i <= nz[0]; i++) {
            yr[i - 1] = 0.0;
            yi[i - 1] = 0.0;
        } // for (i = 1; i <= nz[0]; i++)

        return;
    }

    /**
     * complex logarithm b = clog(a).
     * 
     * @param ar double
     * @param ai double
     * @param br double[]
     * @param bi double[]
     * @param ierr int[] ierr = 0, normal return ierr = 1, z = cmplx(0.0, 0.0)
     */
    private void zlog(final double ar, final double ai, final double[] br, final double[] bi, final int[] ierr) {
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

        if ( (theta <= 0.0) && (ar < 0.0)) {
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
     * zmlri computes the I Bessel function for the Real(z) >= 0.0 by the Miller Algorithm normalized by a Neumann
     * series.
     * 
     * @param zr double
     * @param zi double
     * @param fnu double
     * @param kode int
     * @param n int
     * @param yr double[]
     * @param yi double[]
     * @param nz int[]
     */
    private void zmlri(final double zr, double zi, final double fnu, final int kode, final int n, final double[] yr,
            final double[] yi, final int[] nz) {
        double scle;
        double az;
        int iaz;
        int ifnu;
        int inu;
        double at;
        double raz;
        final double[] str = new double[1];
        final double[] sti = new double[1];
        double ckr;
        double cki;
        double rzr;
        double rzi;
        double p1r;
        double p1i;
        double p2r;
        double p2i;
        double ack;
        double rho;
        double rho2;
        double tst;
        double ak;
        int i;
        double ptr;
        double pti;
        double ap;
        int k;
        int itime;
        double flam;
        double fkap;
        int kk;
        double fkk;
        double fnf;
        double tfnf;
        double bk;
        final int[] idum = new int[1];
        double sumr;
        double sumi;
        int km;
        int m;
        final double[] cnormr = new double[1];
        final double[] cnormi = new double[1];

        scle = tiny / tol;
        nz[0] = 0;
        az = zabs(zr, zi);
        iaz = (int) (az);
        ifnu = (int) (fnu);
        inu = ifnu + n - 1;
        at = iaz + 1.0;
        raz = 1.0 / az;
        str[0] = zr * raz;
        sti[0] = -zi * raz;
        ckr = str[0] * at * raz;
        cki = sti[0] * at * raz;
        rzr = (str[0] + str[0]) * raz;
        rzi = (sti[0] + sti[0]) * raz;
        p1r = 0.0;
        p1i = 0.0;
        p2r = 1.0;
        p2i = 0.0;
        ack = (at + 1.0) * raz;
        rho = ack + Math.sqrt( (ack * ack) - 1.0);
        rho2 = rho * rho;
        tst = (rho2 + rho2) / ( (rho2 - 1.0) * (rho - 1.0));
        tst = tst / tol;

        // Compute relative truncation error index for series
        ak = at;

        group: {

            for (i = 1; i <= 80; i++) {
                ptr = p2r;
                pti = p2i;
                p2r = p1r - ( (ckr * ptr) - (cki * pti));
                p2i = p1i - ( (cki * ptr) + (ckr * pti));
                p1r = ptr;
                p1i = pti;
                ckr = ckr + rzr;
                cki = cki + rzi;
                ap = zabs(p2r, p2i);

                if (ap > (tst * ak * ak)) {
                    break group;
                } // if (ap > (tst*ak*ak))

                ak = ak + 1.0;
            } // for (i = 1; i <= 80; i++)

            nz[0] = -2;

            return;
        } // group

        i = i + 1;
        k = 0;

        if (inu >= iaz) {

            // Compute relative truncation error for ratios
            p1r = 0.0;
            p1i = 0.0;
            p2r = 1.0;
            p2i = 0.0;
            at = inu + 1.0;
            str[0] = zr * raz;
            sti[0] = -zi * raz;
            ckr = str[0] * at * raz;
            cki = sti[0] * at * raz;
            ack = at * raz;
            tst = Math.sqrt(ack / tol);
            itime = 1;

            group2: {

                for (k = 1; k <= 80; k++) {
                    ptr = p2r;
                    pti = p2i;
                    p2r = p1r - ( (ckr * ptr) - (cki * pti));
                    p2i = p1i - ( (ckr * pti) + (cki * ptr));
                    p1r = ptr;
                    p1i = pti;
                    ckr = ckr + rzr;
                    cki = cki + rzi;
                    ap = zabs(p2r, p2i);

                    if (ap < tst) {
                        continue;
                    } // if (ap < tst)

                    if (itime == 2) {
                        break group2;
                    } // if (itime == 2)

                    ack = zabs(ckr, cki);
                    flam = ack + Math.sqrt( (ack * ack) - 1.0);
                    fkap = ap / zabs(p1r, p1i);
                    rho = Math.min(flam, fkap);
                    tst = tst * Math.sqrt(rho / ( (rho * rho) - 1.0));
                    itime = 2;
                } // for (k = 1; k <= 80; k++)

                nz[0] = -2;

                return;
            } // group2
        } // if (inu >= iaz)

        // Backward recurrence and sum normalizing relation
        k = k + 1;
        kk = Math.max(i + iaz, k + inu);
        fkk = kk;
        p1r = 0.0;
        p1i = 0.0;

        // Scale p2 and sum by scle
        p2r = scle;
        p2i = 0.0;
        fnf = fnu - ifnu;
        tfnf = fnf + fnf;
        bk = dgamln(fkk + tfnf + 1.0, idum) - dgamln(fkk + 1.0, idum) - dgamln(tfnf + 1.0, idum);
        bk = Math.exp(bk);
        sumr = 0.0;
        sumi = 0.0;
        km = kk - inu;

        for (i = 1; i <= km; i++) {
            ptr = p2r;
            pti = p2i;
            p2r = p1r + ( (fkk + fnf) * ( (rzr * ptr) - (rzi * pti)));
            p2i = p1i + ( (fkk + fnf) * ( (rzi * ptr) + (rzr * pti)));
            p1r = ptr;
            p1i = pti;
            ak = 1.0 - (tfnf / (fkk + tfnf));
            ack = bk * ak;
            sumr = sumr + ( (ack + bk) * p1r);
            sumi = sumi + ( (ack + bk) * p1i);
            bk = ack;
            fkk = fkk - 1.0;
        } // for (i = 1; i <= km; i++)

        yr[n - 1] = p2r;
        yi[n - 1] = p2i;

        if (n != 1) {

            for (i = 2; i <= n; i++) {
                ptr = p2r;
                pti = p2i;
                p2r = p1r + ( (fkk + fnf) * ( (rzr * ptr) - (rzi * pti)));
                p2i = p1i + ( (fkk + fnf) * ( (rzi * ptr) + (rzr * pti)));
                p1r = ptr;
                p1i = pti;
                ak = 1.0 - (tfnf / (fkk + tfnf));
                ack = bk * ak;
                sumr = sumr + ( (ack + bk) * p1r);
                sumi = sumi + ( (ack + bk) * p1i);
                bk = ack;
                fkk = fkk - 1.0;
                m = n - i + 1;
                yr[m - 1] = p2r;
                yi[m - 1] = p2i;
            } // for (i = 2; i <= n; i++)
        } // if (n != 1)

        if (ifnu > 0) {

            for (i = 1; i <= ifnu; i++) {
                ptr = p2r;
                pti = p2i;
                p2r = p1r + ( (fkk + fnf) * ( (rzr * ptr) - (rzi * pti)));
                p2i = p1i + ( (fkk + fnf) * ( (rzr * pti) + (rzi * ptr)));
                p1r = ptr;
                p1i = pti;
                ak = 1.0 - (tfnf / (fkk + tfnf));
                ack = bk * ak;
                sumr = sumr + ( (ack + bk) * p1r);
                sumi = sumi + ( (ack + bk) * p1i);
                bk = ack;
                fkk = fkk - 1.0;
            } // for (i = 1; i <= ifnu; i++)
        } // if (ifnu > 0)

        ptr = zr;
        pti = zi;

        if (kode == 2) {
            ptr = 0.0;
        } // if (kode == 2)

        zlog(rzr, rzi, str, sti, idum);
        p1r = ( -fnf * str[0]) + ptr;
        p1i = ( -fnf * sti[0]) + pti;
        ap = dgamln(fnf + 1.0, idum);
        ptr = p1r - ap;
        pti = p1i;

        // The division cexp(pt)/(sum+p2) is altered to avoid overflow
        // in the denominator by squaring large quantities
        p2r = p2r + sumr;
        p2i = p2i + sumi;
        ap = zabs(p2r, p2i);
        p1r = 1.0 / ap;
        zexp(ptr, pti, str, sti);
        ckr = str[0] * p1r;
        cki = sti[0] * p1r;
        ptr = p2r * p1r;
        pti = -p2i * p1r;
        zmlt(ckr, cki, ptr, pti, cnormr, cnormi);

        for (i = 1; i <= n; i++) {
            str[0] = (yr[i - 1] * cnormr[0]) - (yi[i - 1] * cnormi[0]);
            yi[i - 1] = (yr[i - 1] * cnormi[0]) + (yi[i - 1] * cnormr[0]);
            yr[i - 1] = str[0];
        } // for (i = 1; i <= n; i++)

        return;
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
     * DOCUMENT ME!
     * 
     * @param mqc int Default is mqc = 1 does not check underflow limits and overflow limits. mqc != 1 checks underflow
     *            and overflow limits
     */
    private void zqcai(final int mqc) {

        /**
         * *DATE WRITTEN 830501 (YYMMDD) C***REVISION DATE 890801, 930101 (YYMMDD) C C *** A DOUBLE PRECISION ROUTINE
         * *** C C ZQCAI IS A QUICK CHECK ROUTINE FOR THE COMPLEX AIRY FUNCTIONS C GENERATED BY SUBROUTINES ZAIRY AND
         * ZBIRY. C C ZQCAI GENERATES AIRY FUNCTIONS AND THEIR DERIVATIVES FROM ZAIRY C AND ZBIRY AND CHECKS THEM
         * AGAINST THE WRONSKIAN EVALUATION IN THE C REGION -PI/3 .LE. ARG(Z) .LE. PI/3: C C
         * AI(Z)*BI'(Z)-AI'(Z)*BI(Z)=1/PI. C C IN THE REMAINDER OF THE CUT PLANE, THE IDENTITIES C C AI(Z) = SQRT(-Z)*(
         * J(-1/3,ZR) + J(1/3,ZR) )/3 C C AI'(Z) = Z*( J(-2/3,ZR) - J(2/3,ZR) )/3 C C BI(Z) = I*SQRT(-Z/3)*(
         * C1*H(1/3,1,ZR) - C2*H(1/3,2,ZR) )/2 C C BI'(Z) = I*(-Z)/SQRT(3)*( C2*H(2/3,1,ZR) - C1*H(2/3,2,ZR) )/2 C C ARE
         * CHECKED WHERE ZR = (2/3)(-Z)**(3/2) WITH C1 = EXP(PI*I/6), C C2 = CONJG(C1) AND I**2 = -1. C C THE PARAMETER
         * MQC CAN HAVE VALUES 1 (THE DEFAULT) FOR A FASTER, C LESS DEFINITIVE TEST OR 2 FOR A SLOWER, MORE DEFINITIVE
         * TEST.
         */
        final double[] er = new double[5];
        final double[] t = new double[20];
        final double[] yr = new double[20];
        final double[] yi = new double[20];
        final double[] yyr = new double[20];
        final double[] yyi = new double[20];
        final double[] wr = new double[20];
        final double[] wi = new double[20];
        final int[] keps = new int[20];
        final int[] kdo = new int[20];
        double neweps;
        int k;
        int k1;
        double aa;
        double dig;
        double slak;
        double ertol;
        double rm;
        double atol;
        final double rpi = 1.0 / Math.PI;
        final double spi = Math.PI / 6.0;
        double con1r;
        double con1i;
        double con2r;
        double con2i;
        double pi3;
        double c13;
        double c23;
        double c43;
        double cavr;
        double cavi;
        double chir;
        double chii;
        double cir;
        double cii;
        int icl;
        int il;
        int i;
        double eps;
        double film;
        double ts;
        int itl;
        int lflg;
        int icase;
        int kode;
        int irset;
        int irb;
        int ir;
        double r;
        int it;
        double ct;
        double st;
        double zr;
        double zi;
        final int[] nz = new int[1];
        final int[] ierr = new int[1];
        final double[] cwr = new double[1];
        final double[] cwi = new double[1];
        final double[] cyr = new double[1];
        final double[] cyi = new double[1];
        final double[] cvr = new double[1];
        final double[] cvi = new double[1];
        final double[] car = new double[1];
        final double[] cai = new double[1];
        double zrr;
        double zri;
        final double[] str = new double[1];
        final double[] sti = new double[1];
        double ptr;
        int jb;
        int jl;
        double ab;
        double zwr = 0.0;
        double zwi = 0.0;
        double scr = 0.0;
        double sci = 0.0;
        double conar;
        double conai;
        double conbr;
        double conbi;
        double concr;
        double conci;
        double condr;
        double condi;
        final double[] yr0 = new double[1];
        final double[] yi0 = new double[1];
        final double[] yr1 = new double[1];
        final double[] yi1 = new double[1];
        final double[] yr2 = new double[1];
        final double[] yi2 = new double[1];
        final double[] yr3 = new double[1];
        final double[] yi3 = new double[1];
        final double[] yr4 = new double[1];
        final double[] yi4 = new double[1];
        final double[] wr0 = new double[1];
        final double[] wi0 = new double[1];
        final double[] wr1 = new double[1];
        final double[] wi1 = new double[1];
        int j;

        /**
         * SET PARAMETERS RELATED TO MACHINE CONSTANTS. TOL IS THE APPROXIMATE UNIT ROUNDOFF LIMITED TO 1.0E-18. ELIM IS
         * THE APPROXIMATE EXPONENTIAL OVER- AND UNDERFLOW LIMIT. EXP(-ELIM).LT.EXP(-ALIM)=EXP(-ELIM)/TOL AND
         * EXP(ELIM).GT.EXP(ALIM)=EXP(ELIM)*TOL ARE INTERVALS NEAR UNDERFLOW AND OVERFLOW LIMITS WHERE SCALED ARITHMETIC
         * IS DONE. RL IS THE LOWER BOUNDARY OF THE ASYMPTOTIC EXPANSION FOR LARGE Z. DIG = NUMBER OF BASE 10 DIGITS IN
         * TOL = 10**(-DIG). FNUL IS THE LOWER BOUNDARY OF THE ASYMPTOTIC SERIES FOR LARGE FNU
         */
        // epsilon = D1MACH(4)
        // Machine epsilon is the smallest positive epsilon such that
        // (1.0 + epsilon) != 1.0.
        // epsilon = 2**(1 - doubleDigits) = 2**(1 - 53) = 2**(-52)
        // epsilon = 2.224460e-16
        // epsilon is called the largest relative spacing
        epsilon = 1.0;
        neweps = 1.0;

        while (true) {

            if (1.0 == (1.0 + neweps)) {
                break;
            } else {
                epsilon = neweps;
                neweps = neweps / 2.0;
            }
        }

        tol = Math.max(epsilon, 1.0e-18);

        // emin, the smallest exponent E for double precision, is I1MACH(15)
        // tiny = D1MACH(1) = 2**(emin - 1) = 2**(-1022)
        // D1MACH(1) is the smallest normalized number, which preserves the
        // full precision of the mantissa.
        // Double.MIN_VALUE = 2**(-1074) is the smallest denormalized number,
        // which preseerves only a portion of the fraction's precision.
        emin = -1021;
        tiny = Math.pow(2, -1022);

        // emax, the largest exponent E for double precision, is I1MACH(16)
        // D1MACH(2) = 2**(emax)*(1 - 2**(-doubleDigits)) = 2**1024*(1 - 2**-53)
        // D1MACH(2) = Double.MAX_VALUE
        emax = 1024;

        // r1m5 = log10(2), which is D1MACH(5)
        r1m5 = 0.434294481903251 * Math.log(2.0);
        k = Math.min(Math.abs(emin), Math.abs(emax));
        elim = 2.303 * ( (k * r1m5) - 3.0);

        // doubleDigits is I1MACH(14), the number of base-B digits for double
        // precision. IEEE 754 double precision has 64 bits with 1 sign bit,
        // 11 exponent bits, and 52 fraction, mantissa, or signficand bits.
        // Floating-point numbers are typically stored in normalized form,
        // with the radix point after the first non-zero digit. In base two,
        // the only possible non-zero digit is 1. Thus, a leading digit of 1
        // can always be assumed and need not be explicitly represented.
        // Therefore, the mantissa effectively has 53 bits of resolution.
        doubleDigits = 53;
        k1 = doubleDigits - 1;
        aa = r1m5 * k1;
        dig = Math.min(aa, 18.0);
        aa = 2.303 * aa;
        alim = elim + Math.max( -aa, -41.45);
        rl = (1.2 * dig) + 3.0;
        fnul = 10.0 + (6.0 * (dig - 3.0));
        slak = 3.0 + (4.0 * ( ( -0.434294481903251 * Math.log(tol)) - 7.0) / 11.0);
        slak = Math.max(slak, 3.0);
        ertol = tol * Math.pow(10.0, slak);
        rm = 0.5 * (alim + elim);
        rm = Math.min(rm, 200.0);
        rm = Math.max(rm, rl + 10.0);
        Preferences.debug("Quick check routine for the AIRY functions from zairy and zbiry\n", Preferences.DEBUG_ALGORITHM);
        Preferences.debug("tol = " + tol + "\n", Preferences.DEBUG_ALGORITHM);
        Preferences.debug("elim = " + elim + "\n", Preferences.DEBUG_ALGORITHM);
        Preferences.debug("alim = " + alim + "\n", Preferences.DEBUG_ALGORITHM);
        Preferences.debug("rl = " + rl + "\n", Preferences.DEBUG_ALGORITHM);
        Preferences.debug("fnul = " + fnul + "\n", Preferences.DEBUG_ALGORITHM);
        Preferences.debug("dig = " + dig + "\n", Preferences.DEBUG_ALGORITHM);
        atol = 100.0 * tol;
        con1r = Math.cos(spi);
        con1i = Math.sin(spi);
        con2r = con1r;
        con2i = -con1i;
        pi3 = Math.PI / 3.0;
        c13 = 1.0 / 3.0;
        c23 = c13 + c13;
        c43 = c23 + c23;
        cavr = Math.sqrt(c13);
        cavi = 0.0;
        chir = 0.0;
        chii = 0.5;
        cir = 0.0;
        cii = 1.0;
        Preferences.debug("Checks in the (z,fnu) space with mqc = " + mqc + "\n", Preferences.DEBUG_ALGORITHM);

        /**
         * TEST VALUES OF Z IN -PI.LT.ARG(Z).LE.PI
         * C-----------------------------------------------------------------------
         * C----------------------------------------------------------------------- C KDO(K), K=1,IL DETERMINES WHICH OF
         * THE IL ANGLES IN -PI TO PI C ARE USE TO COMPUTE VALUES OF Z C KDO(K) = 0 MEANS THAT THE INDEX K WILL BE USED
         * FOR ONE OR TWO C VALUES OF Z, DEPENDING ON THE CHOICE OF KEPS(K) C = 1 MEANS THAT THE INDEX K AND THE
         * CORRESPONDING ANGLE C WILL BE SKIPPED C KEPS(K), K=1,IL DETERMINES WHICH OF THE ANGLES GET INCREMENTED C UP
         * AND DOWN TO PUT VALUES OF Z IN REGIONS WHERE DIFFERENT C FORMULAE ARE USED. C KEPS(K) =0 MEANS THAT THE ANGLE
         * WILL BE USED WITHOUT CHANGE C =1 MEANS THAT THE ANGLE WILL BE INCREMENTED UP AND C DOWN BY EPSC THE ANGLES TO
         * BE USED ARE STORED IN THE T(I) ARRAY, I=1,ITL
         */
        if (mqc != 2) {
            icl = 1;
            il = 5;

            for (i = 1; i <= il; i++) {
                kdo[i - 1] = 0;
                keps[i - 1] = 0;
            } // for (i = 1; i <= il; i++)
        } // if (mqc != 2)
        else { // mqc == 2
            icl = 2;
            il = 7;

            for (i = 1; i <= il; i++) {
                kdo[i - 1] = 0;
                keps[i - 1] = 0;
            } // for (i = 1; i <= il; i++)

            keps[1] = 1;
            keps[2] = 1;
            keps[4] = 1;
            keps[5] = 1;
        } // else mqc == 2

        i = 2;
        eps = 0.01;
        film = il - 1.0;
        t[0] = -Math.PI + eps;

        for (k = 2; k <= il; k++) {

            if (kdo[k - 1] == 0) {
                t[i - 1] = Math.PI * ( -il + (2.0 * k) - 1.0) / film;

                if (keps[k - 1] != 0) {
                    ts = t[i - 1];
                    t[i - 1] = ts - eps;
                    i = i + 1;
                    t[i - 1] = ts + eps;
                } // if (keps[k-1] != 0)

                i = i + 1;
            } // if (kdo[k-1] == 0)
        } // for (k = 2; k <= il; k++)

        itl = i - 1;
        lflg = 0;

        for (icase = 1; icase <= icl; icase++) {

            for (kode = 1; kode <= 2; kode++) {

                for (irset = 1; irset <= 3; irset++) {
                    irb = Math.min(irset, 2);

                    for (ir = irb; ir <= 4; ir++) {

                        if (irset == 1) {
                            r = ( (0.2 * (4.0 - ir)) + (2.0 * (ir - 1.0))) / 3.0;
                        } // if (irset == 1)
                        else if (irset == 2) {
                            r = ( (2.0 * (4.0 - ir)) + (rl * (ir - 1.0))) / 3.0;
                        } // else if (irset == 2)
                        else {
                            r = ( (rl * (4.0 - ir)) + (rm * (ir - 1.0))) / 3.0;
                        } // else

                        for (it = 1; it <= itl; it++) {
                            ct = Math.cos(t[it - 1]);
                            st = Math.sin(t[it - 1]);

                            if (Math.abs(ct) < atol) {
                                ct = 0.0;
                            } // if (Math.abs(ct) < atol)

                            if (Math.abs(st) < atol) {
                                st = 0.0;
                            } // if (Math.abs(st) < atol)

                            zr = r * ct;
                            zi = r * st;

                            if (Math.abs(t[it - 1]) <= pi3) {

                                // Wronskian check in -pi/3 < arg(z) < pi/3, test #1
                                zairy(zr, zi, 0, kode, yr0, yi0, nz, ierr);
                                yr[0] = yr0[0];
                                yi[0] = yi0[0];

                                if ( (nz[0] != 0) || (ierr[0] != 0)) {
                                    continue;
                                } // if ((nz[0] != 0) || (ierr[0] != 0))

                                zairy(zr, zi, 1, kode, yr1, yi1, nz, ierr);
                                yr[1] = yr1[0];
                                yi[1] = yi1[0];

                                if ( (nz[0] != 0) || (ierr[0] != 0)) {
                                    continue;
                                } // if ((nz[0] != 0) || (ierr[0] != 0))

                                zbiry(zr, zi, 0, kode, wr0, wi0, ierr);
                                wr[0] = wr0[0];
                                wi[0] = wi0[0];
                                zbiry(zr, zi, 1, kode, wr1, wi1, ierr);
                                wr[1] = wr1[0];
                                wi[1] = wi1[0];
                                cwr[0] = (yr[0] * wr[1]) - (yi[0] * wi[1]);
                                cwi[0] = (yr[0] * wi[1]) + (yi[0] * wr[1]);
                                cyr[0] = (yr[1] * wr[0]) - (yi[1] * wi[0]);
                                cyi[0] = (yr[1] * wi[0]) + (yi[1] * wr[0]);
                                cvr[0] = rpi;
                                cvi[0] = 0.0;

                                if (kode == 2) {
                                    zsqrt(zr, zi, car, cai);
                                    zrr = ( (zr * car[0]) - (zi * cai[0])) * c23;
                                    zri = ( (zr * cai[0]) + (zi * car[0])) * c23;
                                    aa = Math.abs(zrr);
                                    car[0] = zrr - aa;
                                    cai[0] = zri;
                                    zexp(car[0], cai[0], str, sti);
                                    ptr = (str[0] * cvr[0]) - (str[0] * cvi[0]);
                                    cvi[0] = (str[0] * cvi[0]) + (sti[0] * cvr[0]);
                                    cvr[0] = ptr;
                                } // if (kode == 2)

                                cyr[0] = cwr[0] - cyr[0] - cvr[0];
                                cyi[0] = cwi[0] - cyi[0] - cvi[0];
                                er[0] = zabs(cyr[0], cyi[0]) / zabs(cvr[0], cvi[0]);
                                jb = 1;
                                jl = 1;
                            } // if (Math.abs(t[it-1] <= pi3)
                            else { // Math.abs(t[it-1] > pi3
                                zairy(zr, zi, 0, kode, yr1, yi1, nz, ierr);
                                yr[1] = yr1[0];
                                yi[1] = yi1[0];

                                if ( (nz[0] != 0) || (ierr[0] != 0)) {
                                    continue;
                                } // if ((nz[0] != 0) || (ierr[0] != 0))

                                zrr = -zr;
                                zri = -zi;
                                zsqrt(zrr, zri, cvr, cvi);
                                ptr = ( (zrr * cvr[0]) - (zri * cvi[0])) * c23;
                                zri = ( (zrr * cvi[0]) + (zri * cvr[0])) * c23;
                                zrr = ptr;
                                zbesj(zrr, zri, c23, kode, 2, yyr, yyi, nz, ierr);

                                if ( (nz[0] != 0) || (ierr[0] != 0)) {
                                    continue;
                                } // if ((nz[0] != 0) || (ierr[0] != 0))

                                str[0] = yyr[0] * c43;
                                sti[0] = yyi[0] * c43;
                                zdiv(str[0], sti[0], zrr, zri, str, sti);
                                cyr[0] = str[0] - yyr[1];
                                cyi[0] = sti[0] - yyi[1];
                                car[0] = yyr[0];
                                cai[0] = yyi[0];
                                zbesj(zrr, zri, c13, kode, 2, yyr, yyi, nz, ierr);

                                if ( (nz[0] != 0) || (ierr[0] != 0)) {
                                    continue;
                                } // if ((nz[0] != 0) || (ierr[0] != 0))

                                if (kode == 2) {
                                    ab = Math.abs(zri);
                                    zsqrt(zr, zi, cwr, cwi);
                                    zwr = ( (zr * cwr[0]) - (zi * cwi[0])) * c23;
                                    zwi = ( (zr * cwi[0]) + (zi * cwr[0])) * c23;
                                    cwr[0] = zwr + ab;
                                    cwi[0] = zwi;
                                    zexp(cwr[0], cwi[0], str, sti);
                                    cwr[0] = str[0];
                                    cwi[0] = sti[0];
                                    str[0] = (yyr[0] * cwr[0]) - (yyi[0] * cwi[0]);
                                    yyi[0] = (yyr[0] * cwi[0]) + (yyi[0] * cwr[0]);
                                    yyr[0] = str[0];
                                    str[0] = (yyr[1] * cwr[0]) - (yyi[1] * cwi[0]);
                                    yyi[1] = (yyr[1] * cwi[0]) + (yyi[1] * cwr[0]);
                                    yyr[1] = str[0];
                                    str[0] = (cyr[0] * cwr[0]) - (cyi[0] * cwi[0]);
                                    cyi[0] = (cyr[0] * cwi[0]) + (cyi[0] * cwr[0]);
                                    cyr[0] = str[0];
                                    str[0] = (car[0] * cwr[0]) - (cai[0] * cwi[0]);
                                    cai[0] = (car[0] * cwi[0]) + (cai[0] * cwr[0]);
                                    car[0] = str[0];
                                    scr = cwr[0];
                                    sci = cwi[0];
                                } // if (kode == 2)

                                cwr[0] = cvr[0] * c13;
                                cwi[0] = cvi[0] * c13;
                                wr[1] = (cwr[0] * (yyr[0] + cyr[0])) - (cwi[0] * (yyi[0] + cyi[0]));
                                wi[1] = (cwr[0] * (yyi[0] + cyi[0])) + (cwi[0] * (yyr[0] + cyr[0]));
                                str[0] = yr[1] - wr[1];
                                sti[0] = yi[1] - wi[1];
                                er[1] = zabs(str[0], sti[0]);

                                if ( (zi != 0.0) || (zr >= 0.0)) {
                                    er[1] = er[1] / zabs(yr[1], yi[1]);
                                } // if ((zi != 0.0) || (zr >= 0.0))
                                else if (kode == 2) {
                                    er[1] = er[1] / zabs(scr, sci);
                                } // else if (kode == 2)

                                // Check Ai' Test #3
                                str[0] = yyr[0] * c23;
                                sti[0] = yyi[0] * c23;
                                zdiv(str[0], sti[0], zrr, zri, cyr, cyi);
                                cyr[0] = cyr[0] - yyr[1] - car[0];
                                cyi[0] = cyi[0] - yyi[1] - cai[0];
                                wr[2] = ( (zr * cyr[0]) - (zi * cyi[0])) * c13;
                                wi[2] = ( (zr * cyi[0]) + (zi * cyr[0])) * c13;
                                zairy(zr, zi, 1, kode, yr2, yi2, nz, ierr);
                                yr[2] = yr2[0];
                                yi[2] = yi2[0];
                                str[0] = yr[2] - wr[2];
                                sti[0] = yi[2] - wi[2];

                                if ( (zi != 0.0) || (zr >= 0.0)) {
                                    er[2] = er[2] / zabs(yr[2], yi[2]);
                                } // if ((zi != 0.0) || (zr >= 0.0))
                                else if (kode == 2) {
                                    er[2] = er[2] / zabs(scr, sci);
                                } // else if (kode == 2)

                                // Check Bi Test #4
                                zbesh(zrr, zri, c13, kode, 1, 1, yr, yi, nz, ierr);

                                if ( (nz[0] != 0) || (ierr[0] != 0)) {
                                    continue;
                                } // if ((nz[0] != 0) || (ierr[0] != 0))

                                zbesh(zrr, zri, c13, kode, 2, 1, yyr, yyi, nz, ierr);

                                if ( (nz[0] != 0) || (ierr[0] != 0)) {
                                    continue;
                                } // if (nz[0] != 0) || (ierr[0] != 0))

                                conar = con1r;
                                conai = con1i;
                                conbr = con2r;
                                conbi = con2i;
                                concr = con2r;
                                conci = con2i;
                                condr = con1r;
                                condi = con1i;

                                if (kode == 2) {
                                    aa = Math.abs(zwr);
                                    zwr = (cir * zrr) - (cii * zri) - aa;
                                    zwi = (cir * zri) + (cii * zrr);
                                    zexp(zwr, zwi, cwr, cwi);
                                    str[0] = (conar * cwr[0]) - (conai * cwi[0]);
                                    conai = (conar * cwi[0]) + (conai * cwr[0]);
                                    conar = str[0];
                                    str[0] = (concr * cwr[0]) - (conci * cwi[0]);
                                    conci = (concr * cwi[0]) + (conci * cwr[0]);
                                    concr = str[0];
                                    zwr = - ( (cir * zrr) - (cii * zri)) - aa;
                                    zwi = - ( (cir * zri) + (cii * zrr));
                                    zexp(zwr, zwi, cwr, cwi);
                                    str[0] = (conbr * cwr[0]) - (conbi * cwi[0]);
                                    conbi = (conbr * cwi[0]) + (conbi * cwr[0]);
                                    conbr = str[0];
                                    str[0] = (condr * cwr[0]) - (condi * cwi[0]);
                                    condi = (condr * cwi[0]) + (condi * cwr[0]);
                                    condr = str[0];
                                    scr = cwr[0];
                                    sci = cwi[0];
                                } // if (kode == 2)

                                cwr[0] = (conar * yr[0]) - (conai * yi[0]);
                                cwi[0] = (conar * yi[0]) + (conai * yr[0]);
                                cwr[0] = cwr[0] - ( (conbr * yyr[0]) - (conbi * yyi[0]));
                                cwi[0] = cwi[0] - ( (conbr * yyi[0]) + (conbi * yyr[0]));
                                str[0] = (cvr[0] * cavr) - (cvi[0] * cavi);
                                sti[0] = (cvr[0] * cavi) + (cvi[0] * cavr);
                                ptr = (str[0] * cwr[0]) - (sti[0] * cwi[0]);
                                cwi[0] = (str[0] * cwi[0]) + (sti[0] * cwr[0]);
                                cwr[0] = ptr;
                                wr[3] = (cwr[0] * chir) - (cwi[0] * chii);
                                wi[3] = (cwr[0] * chii) + (cwi[0] * chir);
                                zbiry(zr, zi, 0, kode, yr3, yi3, ierr);
                                yr[3] = yr3[0];
                                yi[3] = yi3[0];
                                str[0] = yr[3] - wr[3];
                                sti[0] = yi[3] - wi[3];
                                er[3] = zabs(str[0], sti[0]);

                                if ( (zi != 0.0) || (zr >= 0.0)) {
                                    er[3] = er[3] / zabs(yr[3], yi[3]);
                                } // if ((zi != 0.0) || (zr >= 0.0))
                                else if (kode == 2) {
                                    er[3] = er[3] / zabs(scr, sci);
                                } // else if (kode == 2)

                                // Check Bi' Test #5
                                zbesh(zrr, zri, c23, kode, 1, 1, yr, yi, nz, ierr);

                                if ( (nz[0] != 0) || (ierr[0] != 0)) {
                                    continue;
                                } // if ((nz[0] != 0) || (ierr[0] != 0))

                                zbesh(zrr, zri, c23, kode, 2, 1, yyr, yyi, nz, ierr);

                                if ( (nz[0] != 0) || (ierr[0] != 0)) {
                                    continue;
                                } // if ((nz[0] != 0) || (ierr[0] != 0))

                                cwr[0] = (concr * yr[0]) - (conci * yi[0]);
                                cwi[0] = (concr * yi[0]) + (conci * yr[0]);
                                cwr[0] = cwr[0] - ( (condr * yyr[0]) - (condi * yyi[0]));
                                cwi[0] = cwi[0] - ( (condr * yyi[0]) + (condi * yyr[0]));
                                str[0] = - ( (zr * cavr) - (zi * cavi));
                                sti[0] = - ( (zr * cavi) + (zi * cavr));
                                ptr = (str[0] * cwr[0]) - (sti[0] * cwi[0]);
                                cwi[0] = (str[0] * cwi[0]) + (sti[0] * cwr[0]);
                                cwr[0] = ptr;
                                wr[4] = (cwr[0] * chir) - (cwi[0] * chii);
                                wi[4] = (cwr[0] * chii) + (cwi[0] * chir);
                                zbiry(zr, zi, 1, kode, yr4, yi4, ierr);
                                yr[4] = yr4[0];
                                yi[4] = yi4[0];
                                str[0] = yr[4] - wr[4];
                                sti[0] = yi[4] - wi[4];
                                er[4] = zabs(str[0], sti[0]);

                                if ( (zi != 0.0) || (zr >= 0.0)) {
                                    er[4] = er[4] / zabs(yr[4], yi[4]);
                                } // if ((zi != 0.00 || (zr >= 0.0))
                                else if (kode == 2) {
                                    er[4] = er[4] / zabs(yr[4], yi[4]);
                                } // else if (kode == 2)

                                jb = 2;
                                jl = 5;
                            } // else Math.abs(t[it-1] > pi3)

                            for (j = jb; j <= jl; j++) {

                                if (er[j - 1] < ertol) {
                                    continue;
                                } // if (er[j-1] < ertol)

                                if (lflg != 1) {
                                    Preferences.debug("Cases which violate the relative error " + "test with ertol = "
                                            + ertol + "\n", Preferences.DEBUG_ALGORITHM);
                                    Preferences.debug("OUTPUT FORMAT kode,ir,it,irset,icase\n", Preferences.DEBUG_ALGORITHM);
                                    Preferences.debug("er j z, y[j-1] w[j-1], on the jth test, " + "j = 1,5\n", 
                                    		Preferences.DEBUG_ALGORITHM);
                                    lflg = 1;
                                } // if (lflg != 1)

                                Preferences.debug("kode = " + kode + "\n", Preferences.DEBUG_ALGORITHM);
                                Preferences.debug("ir = " + ir + "\n", Preferences.DEBUG_ALGORITHM);
                                Preferences.debug("it = " + it + "\n", Preferences.DEBUG_ALGORITHM);
                                Preferences.debug("irset = " + irset + "\n", Preferences.DEBUG_ALGORITHM);
                                Preferences.debug("icase = " + icase + "\n", Preferences.DEBUG_ALGORITHM);
                                Preferences.debug("er[" + (j - 1) + "] = " + er[j - 1] + "\n", Preferences.DEBUG_ALGORITHM);
                                Preferences.debug("j = " + j + "\n", Preferences.DEBUG_ALGORITHM);
                                Preferences.debug("zr = " + zr + "\n", Preferences.DEBUG_ALGORITHM);
                                Preferences.debug("zi = " + zi + "\n", Preferences.DEBUG_ALGORITHM);
                                Preferences.debug("yr[" + (j - 1) + "] = " + yr[j - 1] + "\n", Preferences.DEBUG_ALGORITHM);
                                Preferences.debug("yi[" + (j - 1) + "] = " + yi[j - 1] + "\n", Preferences.DEBUG_ALGORITHM);
                                Preferences.debug("wr[" + (j - 1) + "] = " + wr[j - 1] + "\n", Preferences.DEBUG_ALGORITHM);
                                Preferences.debug("wi[" + (j - 1) + "] = " + wi[j - 1] + "\n", Preferences.DEBUG_ALGORITHM);
                            } // for (j = jb; j <= jl; j++)
                        } // for (it = 1; it <= itl; it++)
                    } // for (ir = irb; ir <= 4; ir++)
                } // for (irset = 1; irset <= 3; irset++)
            } // for (kode = 1; kode <= 2; kode++)
        } // for (icase = 1; icase <= icl; icase++)

        if (lflg == 0) {
            Preferences.debug("AIRY QUICK CHECKS OK\n", Preferences.DEBUG_ALGORITHM);
        } // if (lflg == 0)

        return;
    }

    /**
     * DOCUMENT ME!
     * 
     * @param mqc int Default is mqc = 1 does not check underflow limits and overflow limits. mqc != 1 checks underflow
     *            and overflow limits
     */
    private void zqcbh(final int mqc) {

        /**
         * C ZQCBH IS A QUICK CHECK ROUTINE FOR THE COMPLEX H BESSEL FUNCTIONS C GENERATED BY SUBROUTINE ZBESH. C C
         * ZQCBH GENERATES SEQUENCES OF H BESSEL FUNCTIONS FOR KIND 2 FROM C ZBESH AND CHECKS THEM AGAINST ANALYTIC
         * CONTINUATION FORMULAS C IN THE (Z,FNU) SPACE: C C KODE = 1 TESTS (ANALYTIC CONTINUATION FORMULAE, I**2 = -1):
         * C C H(FNU,2,Z)=-EXP(I*PI*FNU)*H(FNU,1,-Z), -PI.LT.ARG(Z).LE.0 C C = 2*COS(PI*FNU)*H(FNU,2,-Z) +
         * EXP(I*PI*FNU)*H(FNU,1,-Z), C C 0.LT.ARG(Z).LE.PI C C KODE = 2 TESTS FOR KINDS 1 AND 2: C C
         * EXP(-I*Z)*H(FNU,1,Z) = [EXP(-I*Z)*H(FNU,1,Z)] C C EXP( I*Z)*H(FNU,2,Z) = [EXP( I*Z)*H(FNU,2,Z)] C C WHERE THE
         * LEFT SIDE IS COMPUTED WITH KODE = 1 AND THE RIGHT SIDE C WITH KODE = 2. C C THE PARAMETER MQC CAN HAVE VALUES
         * 1 (THE DEFAULT) FOR A FASTER, C LESS DEFINITIVE TEST OR 2 FOR A SLOWER, MORE DEFINITIVE TEST. C C MACHINE
         * CONSTANTS ARE DEFINED IN FUNCTIONS I1MACH, R1MACH, AND C D1MACH. THESE MUST BE SELECTED BY THE USER OR SET
         * ACCORDING TO C PROLOGUE INSTRUCTIONS.
         */
        final double[] t = new double[20];
        final double[] aer = new double[20];
        final double[] xnu = new double[20];
        final double[] ur = new double[20];
        final double[] ui = new double[20];
        final double[] vr = new double[20];
        final double[] vi = new double[20];
        final double[] wr = new double[20];
        final double[] wi = new double[20];
        final double[] yr = new double[20];
        final double[] yi = new double[20];
        final int[] keps = new int[20];
        final int[] kdo = new int[20];
        double neweps;
        int k;
        int k1;
        double aa;
        double dig;
        double slak;
        double ertol;
        double rm;
        double r2;
        double atol;
        int nL;
        int il;
        int i;
        int nul;
        double eps;
        double film;
        double ts;
        int itl;
        int lflg;
        int kode;
        int n;
        int nu;
        double fnu;
        int icase;
        int irb;
        int ir;
        double r;
        int it;
        double ct;
        double st;
        double zr;
        double zi;
        int m;
        final int[] ierr = new int[1];
        final int[] nz1 = new int[1];
        int ihp;
        final double[] znr = new double[1];
        final double[] zni = new double[1];
        final int[] nz2 = new int[1];
        final int[] nz3 = new int[1];
        double ab;
        double csgnr;
        double csgni;
        int mflg;
        double av;
        double er;
        int kk;
        final double[] str = new double[1];
        final double[] sti = new double[1];
        double err;
        double cwr;
        double cwi;

        /**
         * SET PARAMETERS RELATED TO MACHINE CONSTANTS. TOL IS THE APPROXIMATE UNIT ROUNDOFF LIMITED TO 1.0E-18. ELIM IS
         * THE APPROXIMATE EXPONENTIAL OVER- AND UNDERFLOW LIMIT. EXP(-ELIM).LT.EXP(-ALIM)=EXP(-ELIM)/TOL AND
         * EXP(ELIM).GT.EXP(ALIM)=EXP(ELIM)*TOL ARE INTERVALS NEAR UNDERFLOW AND OVERFLOW LIMITS WHERE SCALED ARITHMETIC
         * IS DONE. RL IS THE LOWER BOUNDARY OF THE ASYMPTOTIC EXPANSION FOR LARGE Z. DIG = NUMBER OF BASE 10 DIGITS IN
         * TOL = 10**(-DIG). FNUL IS THE LOWER BOUNDARY OF THE ASYMPTOTIC SERIES FOR LARGE FNU
         */
        // epsilon = D1MACH(4)
        // Machine epsilon is the smallest positive epsilon such that
        // (1.0 + epsilon) != 1.0.
        // epsilon = 2**(1 - doubleDigits) = 2**(1 - 53) = 2**(-52)
        // epsilon = 2.224460e-16
        // epsilon is called the largest relative spacing
        epsilon = 1.0;
        neweps = 1.0;

        while (true) {

            if (1.0 == (1.0 + neweps)) {
                break;
            } else {
                epsilon = neweps;
                neweps = neweps / 2.0;
            }
        }

        tol = Math.max(epsilon, 1.0e-18);

        // emin, the smallest exponent E for double precision, is I1MACH(15)
        // tiny = D1MACH(1) = 2**(emin - 1) = 2**(-1022)
        // D1MACH(1) is the smallest normalized number, which preserves the
        // full precision of the mantissa.
        // Double.MIN_VALUE = 2**(-1074) is the smallest denormalized number,
        // which preseerves only a portion of the fraction's precision.
        emin = -1021;
        tiny = Math.pow(2, -1022);

        // emax, the largest exponent E for double precision, is I1MACH(16)
        // D1MACH(2) = 2**(emax)*(1 - 2**(-doubleDigits)) = 2**1024*(1 - 2**-53)
        // D1MACH(2) = Double.MAX_VALUE
        emax = 1024;

        // r1m5 = log10(2), which is D1MACH(5)
        r1m5 = 0.434294481903251 * Math.log(2.0);
        k = Math.min(Math.abs(emin), Math.abs(emax));
        elim = 2.303 * ( (k * r1m5) - 3.0);

        // doubleDigits is I1MACH(14), the number of base-B digits for double
        // precision. IEEE 754 double precision has 64 bits with 1 sign bit,
        // 11 exponent bits, and 52 fraction, mantissa, or signficand bits.
        // Floating-point numbers are typically stored in normalized form,
        // with the radix point after the first non-zero digit. In base two,
        // the only possible non-zero digit is 1. Thus, a leading digit of 1
        // can always be assumed and need not be explicitly represented.
        // Therefore, the mantissa effectively has 53 bits of resolution.
        doubleDigits = 53;
        k1 = doubleDigits - 1;
        aa = r1m5 * k1;
        dig = Math.min(aa, 18.0);
        aa = 2.303 * aa;
        alim = elim + Math.max( -aa, -41.45);
        rl = (1.2 * dig) + 3.0;
        fnul = 10.0 + (6.0 * (dig - 3.0));
        slak = 3.0 + (4.0 * ( ( -0.434294481903251 * Math.log(tol)) - 7.0) / 11.0);
        slak = Math.max(slak, 3.0);
        ertol = tol * Math.pow(10.0, slak);
        rm = 0.5 * (alim + elim);
        rm = Math.min(rm, 200.0);
        rm = Math.max(rm, rl + 10.0);
        r2 = Math.min(rm, fnul);
        Preferences.debug("Quick check routine for the H Bessel function from ZBESH\n", Preferences.DEBUG_ALGORITHM);
        Preferences.debug("tol = " + tol + "\n", Preferences.DEBUG_ALGORITHM);
        Preferences.debug("elim = " + elim + "\n", Preferences.DEBUG_ALGORITHM);
        Preferences.debug("alim = " + alim + "\n", Preferences.DEBUG_ALGORITHM);
        Preferences.debug("rl = " + rl + "\n", Preferences.DEBUG_ALGORITHM);
        Preferences.debug("fnul = " + fnul + "\n", Preferences.DEBUG_ALGORITHM);
        Preferences.debug("dig = " + dig + "\n", Preferences.DEBUG_ALGORITHM);
        atol = 100.0 * tol;
        Preferences.debug("Checks in the (z,fnu) space with mqc = " + mqc + "\n", Preferences.DEBUG_ALGORITHM);

        /**
         * TEST VALUES OF Z IN -PI.LT.ARG(Z).LE.PI NEAR FORMULA BOUNDARIES
         * C-----------------------------------------------------------------------
         * C----------------------------------------------------------------------- C KDO(K), K=1,IL DETERMINES WHICH OF
         * THE IL ANGLES IN -PI TO PI C ARE USE TO COMPUTE VALUES OF Z C KDO(K) = 0 MEANS THAT THE INDEX K WILL BE USED
         * FOR ONE OR TWO C VALUES OF Z, DEPENDING ON THE CHOICE OF KEPS(K) C = 1 MEANS THAT THE INDEX K AND THE
         * CORRESPONDING ANGLE C WILL BE SKIPPED C KEPS(K), K=1,IL DETERMINES WHICH OF THE ANGLES GET INCREMENTED C UP
         * AND DOWN TO PUT VALUES OF Z IN REGIONS WHERE DIFFERENT C FORMULAE ARE USED. C KEPS(K) =0 MEANS THAT THE ANGLE
         * WILL BE USED WITHOUT CHANGE C =1 MEANS THAT THE ANGLE WILL BE INCREMENTED UP AND C DOWN BY EPSC THE ANGLES TO
         * BE USED ARE STORED IN THE T(I) ARRAY, I=1,ITL
         */
        if (mqc != 2) {
            nL = 2;
            il = 5;

            for (i = 1; i <= il; i++) {
                keps[i - 1] = 0;
                kdo[i - 1] = 0;
            } // for (i = 1; i <= il; i++)

            nul = 5;
            xnu[0] = 0.0;
            xnu[1] = 1.0;
            xnu[2] = 2.0;
            xnu[3] = 0.5 * fnul;
            xnu[4] = fnul + 1.1;
        } // if (mqc != 2)
        else { // mqc == 2
            nL = 4;
            il = 13;

            for (i = 1; i <= il; i++) {
                kdo[i - 1] = 0;
                keps[i - 1] = 0;
            } // for (i = 1; i <= il; i++)

            kdo[1] = 1;
            kdo[5] = 1;
            kdo[7] = 1;
            kdo[11] = 1;
            keps[2] = 1;
            keps[3] = 1;
            keps[4] = 1;
            keps[8] = 1;
            keps[9] = 1;
            keps[10] = 1;
            nul = 6;
            xnu[0] = 0.0;
            xnu[1] = 0.6;
            xnu[2] = 1.3;
            xnu[3] = 2.0;
            xnu[4] = 0.5 * fnul;
            xnu[5] = fnul + 1.1;
        } // else mqc == 2

        i = 2;
        eps = 0.01;
        film = il - 1.0;
        t[0] = -Math.PI + eps;

        for (k = 2; k <= il; k++) {

            if (kdo[k - 1] == 0) {
                t[i - 1] = Math.PI * ( -il + (2.0 * k) - 1.0) / film;

                if (keps[k - 1] != 0) {
                    ts = t[i - 1];
                    t[i - 1] = ts - eps;
                    i = i + 1;
                    t[i - 1] = ts + eps;
                } // if (keps[k-1] != 0)

                i = i + 1;
            } // if (kdo[k-1] == 0)
        } // for (k = 2; k <= il; k++)

        itl = i - 1;
        lflg = 0;

        for (kode = 1; kode <= 2; kode++) {

            for (n = 1; n <= nL; n++) {

                for (nu = 1; nu <= nul; nu++) {
                    fnu = xnu[nu - 1];

                    group: for (icase = 1; icase <= 3; icase++) {
                        irb = Math.min(icase, 2);

                        for (ir = irb; ir <= 3; ir++) {

                            if (icase == 1) {
                                r = ( (eps * (3.0 - ir)) + (2.0 * (ir - 1.0))) / 2.0;
                            } // if (icase == 1)
                            else if (icase == 2) {
                                r = ( (2.0 * (3.0 - ir)) + (r2 * (ir - 1.0))) / 2.0;
                            } // else if (icase == 2)
                            else if ( (icase == 3) && (r2 >= rm)) {
                                continue group;
                            } // else if ((icase == 3) && (r2 >= rm))
                            else {
                                r = ( (r2 * (3.0 - ir)) + (rm * (ir - 1.0))) / 2.0;
                            } // else

                            for (it = 1; it <= itl; it++) {
                                ct = Math.cos(t[it - 1]);
                                st = Math.sin(t[it - 1]);

                                if (Math.abs(ct) < atol) {
                                    ct = 0.0;
                                } // if (Math.abs(ct) < atol)

                                if (Math.abs(st) < atol) {
                                    st = 0.0;
                                } // if (Math.abs(st) < atol)

                                zr = r * ct;
                                zi = r * st;

                                if (kode == 1) {
                                    m = 2;
                                    zbesh(zr, zi, fnu, kode, m, n, yr, yi, nz1, ierr);

                                    if ( (ierr[0] != 0) || (nz1[0] != 0)) {
                                        continue;
                                    } // if ((ierr[0] != 0) || (nz1[0] != 0)

                                    if ( (st < 0.0) || ( (st == 0.0) && (ct > 0.0))) {
                                        ihp = 1;
                                        znr[0] = -zr;
                                        zni[0] = -zi;
                                        m = 1;
                                        zbesh(znr[0], zni[0], fnu, kode, m, n, wr, wi, nz2, ierr);

                                        if ( (ierr[0] != 0) || (nz2[0] != 0)) {
                                            continue;
                                        } // if ((ierr[0] != 0) || (nz2[0] != 0))
                                    } // if ((st < 0.0) || ((st == 0.0) && (ct > 0.0)))
                                    else {
                                        ihp = 2;
                                        znr[0] = -zr;
                                        zni[0] = -zi;
                                        m = 2;
                                        zbesh(znr[0], zni[0], fnu, kode, m, n, wr, wi, nz3, ierr);

                                        if ( (ierr[0] != 0) || (nz3[0] != 0)) {
                                            continue;
                                        } // if ((ierr[0] != 0) || (nz3[0] != 0))

                                        m = 1;
                                        zbesh(znr[0], zni[0], fnu, kode, m, n, vr, vi, nz2, ierr);

                                        if ( (ierr[0] != 0) || (nz2[0] != 0)) {
                                            continue;
                                        } // if ((ierr[0] != 0) || (nz2[0] != 0)
                                    } // else

                                    ab = (fnu % 2.0) * Math.PI;
                                    csgnr = Math.cos(ab);
                                    csgni = Math.sin(ab);
                                    mflg = 0;

                                    for (i = 1; i <= n; i++) {
                                        ab = fnu + i - 1.0;
                                        aa = Math.max(0.5, ab);

                                        if (ihp == 1) {
                                            vr[i - 1] = - ( (csgnr * wr[i - 1]) - (csgni * wi[i - 1]));
                                            vi[i - 1] = - ( (csgnr * wi[i - 1]) + (csgni * wr[i - 1]));
                                            cwr = yr[i - 1] - vr[i - 1];
                                            cwi = yi[i - 1] - vi[i - 1];
                                        } // if (ihp == 1)
                                        else { // ihp != 1
                                            cwr = csgnr + csgnr;
                                            str[0] = (cwr * wr[i - 1]) + (csgnr * vr[i - 1]) - (csgni * vi[i - 1]);
                                            vi[i - 1] = (cwr * wi[i - 1]) + (csgnr * vi[i - 1]) + (csgni * vr[i - 1]);
                                            vr[i - 1] = str[0];
                                            cwr = yr[i - 1] - vr[i - 1];
                                            cwi = yi[i - 1] - vi[i - 1];
                                        } // else ihp != 1

                                        av = zabs(yr[i - 1], yi[i - 1]);
                                        er = zabs(cwr, cwi);

                                        if (zi == 0.0) {

                                            if (Math.abs(zr) < aa) {
                                                er = er / av;
                                            } // if (Math.abs(zr) < aa)
                                        } // if (zi == 0.0)
                                        else { // zi != 0.0
                                            er = er / av;
                                        } // else zi != 0.0

                                        aer[i - 1] = er;

                                        if (er > ertol) {
                                            mflg = 1;
                                        } // if (er > ertol)

                                        csgnr = -csgnr;
                                        csgni = -csgni;
                                    } // for (i = 1; i <= n; i++)
                                } // if (kode == 1)
                                else { // kode != 1
                                    m = 1;
                                    kk = 1;
                                    zbesh(zr, zi, fnu, kk, m, n, ur, ui, nz1, ierr);

                                    if ( (ierr[0] != 0) || (nz1[0] != 0)) {
                                        continue;
                                    } // if ((ierr[0] != 0) || (nz1[0] != 0)

                                    zbesh(zr, zi, fnu, kode, m, n, vr, vi, nz2, ierr);

                                    if ( (ierr[0] != 0) || (nz2[0] != 0)) {
                                        continue;
                                    } // if ((ierr[0] != 0) || (nz2[0] != 0)

                                    m = 2;
                                    kk = 1;
                                    zbesh(zr, zi, fnu, kk, m, n, wr, wi, nz1, ierr);

                                    if ( (ierr[0] != 0) || (nz1[0] != 0)) {
                                        continue;
                                    } // if ((ierr[0] != 0) || (nz1[0] != 0)

                                    zbesh(zr, zi, fnu, kode, m, n, yr, yi, nz2, ierr);

                                    if ( (ierr[0] != 0) || (nz2[0] != 0)) {
                                        continue;
                                    } // if ((ierr[0] != 0) || (nz2[0] != 0)

                                    znr[0] = -zi;
                                    zni[0] = zr;
                                    zexp(znr[0], zni[0], znr, zni);
                                    mflg = 0;

                                    for (i = 1; i <= n; i++) {
                                        ab = fnu + i - 1.0;
                                        aa = Math.max(0.5, ab);
                                        zdiv(ur[i - 1], ui[i - 1], znr[0], zni[0], str, sti);
                                        cwr = str[0] - vr[i - 1];
                                        cwi = sti[0] - vi[i - 1];
                                        av = zabs(vr[i - 1], vi[i - 1]);
                                        er = zabs(cwr, cwi);

                                        if (zi == 0.0) {

                                            if (Math.abs(zr) < aa) {
                                                er = er / av;
                                            } // if (Math.abs(zr) < aa)
                                        } // if (zi == 0.0)
                                        else { // zi != 0.0
                                            er = er / av;
                                        } // else zi != 0.0

                                        err = er;

                                        if (er > ertol) {
                                            mflg = 1;
                                        } // if (er > ertol)

                                        cwr = (znr[0] * wr[i - 1]) - (zni[0] * wi[i - 1]) - yr[i - 1];
                                        cwi = (znr[0] * wi[i - 1]) + (zni[0] * wr[i - 1]) - yi[i - 1];
                                        av = zabs(yr[i - 1], yi[i - 1]);
                                        er = zabs(cwr, cwi);

                                        if (zi == 0.0) {

                                            if (Math.abs(zr) < aa) {
                                                er = er / av;
                                            } // if (Math.abs(zr) < aa)
                                        } // if (zi == 0.0)
                                        else { // zi != 0.0
                                            er = er / av;
                                        } // else zi != 0.0

                                        if (er > ertol) {
                                            mflg = 1;
                                        } // if (er > ertol)

                                        aer[i - 1] = er + err;
                                    } // for (i = 1; i <= n; i++)
                                } // else kode != 1

                                if (mflg == 0) {
                                    continue;
                                } // if (mflg == 0)

                                if (lflg != 1) {
                                    Preferences.debug("Cases which violate the relative error " + "test with ertol = "
                                            + ertol + "\n", Preferences.DEBUG_ALGORITHM);
                                    Preferences.debug("OUTPUT FORMAT kode,n,ir,it,icase\n", Preferences.DEBUG_ALGORITHM);
                                    Preferences.debug("aer[k-1], k=1,n z,fnu,v[0],y[0]\n", Preferences.DEBUG_ALGORITHM);
                                    lflg = 1;
                                } // if (lflg != 1)

                                Preferences.debug("kode = " + kode + "\n", Preferences.DEBUG_ALGORITHM);
                                Preferences.debug("n = " + n + "\n", Preferences.DEBUG_ALGORITHM);
                                Preferences.debug("ir = " + ir + "\n", Preferences.DEBUG_ALGORITHM);
                                Preferences.debug("it = " + it + "\n", Preferences.DEBUG_ALGORITHM);
                                Preferences.debug("icase = " + icase + "\n", Preferences.DEBUG_ALGORITHM);

                                for (k = 1; k <= n; k++) {
                                    Preferences.debug("aer[" + (k - 1) + "] = " + aer[k - 1] + "\n", 
                                    		Preferences.DEBUG_ALGORITHM);
                                } // for (k = 1; k <= n; k++)

                                Preferences.debug("zr = " + zr + "\n", Preferences.DEBUG_ALGORITHM);
                                Preferences.debug("zi = " + zi + "\n", Preferences.DEBUG_ALGORITHM);
                                Preferences.debug("fnu = " + fnu + "\n", Preferences.DEBUG_ALGORITHM);
                                Preferences.debug("vr[0] = " + vr[0] + "\n", Preferences.DEBUG_ALGORITHM);
                                Preferences.debug("vi[0] = " + vi[0] + "\n", Preferences.DEBUG_ALGORITHM);
                                Preferences.debug("yr[0] = " + yr[0] + "\n", Preferences.DEBUG_ALGORITHM);
                                Preferences.debug("yi[0] = " + yi[0] + "\n", Preferences.DEBUG_ALGORITHM);
                            } // for (it = 1; it <= itl; it++)
                        } // for (ir = irb; ir <= 3; ir++)
                    } // for (icase = 1; icase <= 3; icase++)
                } // for (nu = 1; nu <= nul; nu++)
            } // for (n = 1; n <= nL; n++)
        } // for (kode = 1; kode <= 2; kode++)

        if (lflg == 0) {
            Preferences.debug("H QUICK CHECKS OK\n", Preferences.DEBUG_ALGORITHM);
        } // if (lflg == 0)

        return;
    }

    /**
     * DOCUMENT ME!
     * 
     * @param mqc int Default is mqc = 1 does not check underflow limits and overflow limits. mqc != 1 checks underflow
     *            and overflow limits
     */
    private void zqcbi(final int mqc) {

        /**
         * ZQCBI IS A QUICK CHECK ROUTINE FOR THE COMPLEX I BESSEL FUNCTION C GENERATED BY SUBROUTINE ZBESI. C C ZQCBK
         * GENERATES SEQUENCES OF I AND K BESSEL FUNCTIONS FROM C ZBESI AND CBESK AND CHECKS THE WRONSKIAN EVALUATION C
         * C I(FNU,Z)*K(FNU+1,Z) + I(FNU+1,Z)*K(FNU,Z) = 1/Z C C IN THE RIGHT HALF PLANE AND A MODIFIED FORM C C
         * I(FNU+1,Z)*K(FNU,ZR) - I(FNU,Z)*K(FNU+1,ZR) = C/Z C C IN THE LEFT HALF PLANE WHERE ZR=-Z AND C=EXP(I*FNU*SGN)
         * WITH C SGN=+1 FOR IM(Z).GE.0 AND SGN=-1 FOR IM(Z).LT.0. C C THE PARAMETER MQC CAN HAVE VALUES 1 (THE DEFAULT)
         * FOR A FASTER, C LESS DEFINITIVE TEST OR 2 FOR A SLOWER, MORE DEFINITIVE TEST. C C MACHINE CONSTANTS ARE
         * DEFINED IN FUNCTIONS I1MACH, R1MACH, AND C D1MACH. THESE MUST BE SELECTED BY THE USER OR SET ACCORDING TOC
         * PROLOGUE INSTRUCTIONS.
         */
        // default is mqc = 1
        double neweps;
        int k;
        int k1;
        double aa;
        double dig;
        double slak;
        double ertol;
        double rm;
        double r2;
        double atol;
        int nL;
        int il;
        final double[] t = new double[20];
        final double[] aer = new double[20];
        final double[] yr = new double[20];
        final double[] yi = new double[20];
        final double[] wr = new double[20];
        final double[] wi = new double[20];
        final double[] xnu = new double[20];
        final int[] keps = new int[20];
        final int[] kdo = new int[20];
        int i;
        int nul;
        double eps;
        double film;
        double ts;
        int itl;
        int lflg;
        int kode;
        int n = 20;
        double fnu;
        int ifnu;
        double ffnu;
        double arg;
        double csgnr;
        double csgni;
        int icase;
        int irb;
        int ir;
        double r;
        int it;
        double ct;
        double st;
        double zr;
        double zi;
        final int[] nz = new int[1];
        final int[] ierr = new int[1];
        final double[] cvr = new double[1];
        final double[] cvi = new double[1];
        final double[] str = new double[1];
        final double[] sti = new double[1];
        double ccr;
        double zrr;
        double zri;
        double cwr;
        double cwi;
        int mflg;
        int kk;
        double er;
        double gnu;
        int nu;
        int nONE;
        double cyr;
        double cyi;
        int iprnt;

        /**
         * SET PARAMETERS RELATED TO MACHINE CONSTANTS. TOL IS THE APPROXIMATE UNIT ROUNDOFF LIMITED TO 1.0E-18. ELIM IS
         * THE APPROXIMATE EXPONENTIAL OVER- AND UNDERFLOW LIMIT. EXP(-ELIM).LT.EXP(-ALIM)=EXP(-ELIM)/TOL AND
         * EXP(ELIM).GT.EXP(ALIM)=EXP(ELIM)*TOL ARE INTERVALS NEAR UNDERFLOW AND OVERFLOW LIMITS WHERE SCALED ARITHMETIC
         * IS DONE. RL IS THE LOWER BOUNDARY OF THE ASYMPTOTIC EXPANSION FOR LARGE Z. DIG = NUMBER OF BASE 10 DIGITS IN
         * TOL = 10**(-DIG). FNUL IS THE LOWER BOUNDARY OF THE ASYMPTOTIC SERIES FOR LARGE FNU
         */
        // epsilon = D1MACH(4)
        // Machine epsilon is the smallest positive epsilon such that
        // (1.0 + epsilon) != 1.0.
        // epsilon = 2**(1 - doubleDigits) = 2**(1 - 53) = 2**(-52)
        // epsilon = 2.224460e-16
        // epsilon is called the largest relative spacing
        epsilon = 1.0;
        neweps = 1.0;

        while (true) {

            if (1.0 == (1.0 + neweps)) {
                break;
            } else {
                epsilon = neweps;
                neweps = neweps / 2.0;
            }
        }

        tol = Math.max(epsilon, 1.0e-18);

        // emin, the smallest exponent E for double precision, is I1MACH(15)
        // tiny = D1MACH(1) = 2**(emin - 1) = 2**(-1022)
        // D1MACH(1) is the smallest normalized number, which preserves the
        // full precision of the mantissa.
        // Double.MIN_VALUE = 2**(-1074) is the smallest denormalized number,
        // which preseerves only a portion of the fraction's precision.
        emin = -1021;
        tiny = Math.pow(2, -1022);

        // emax, the largest exponent E for double precision, is I1MACH(16)
        // D1MACH(2) = 2**(emax)*(1 - 2**(-doubleDigits)) = 2**1024*(1 - 2**-53)
        // D1MACH(2) = Double.MAX_VALUE
        emax = 1024;

        // r1m5 = log10(2), which is D1MACH(5)
        r1m5 = 0.434294481903251 * Math.log(2.0);
        k = Math.min(Math.abs(emin), Math.abs(emax));
        elim = 2.303 * ( (k * r1m5) - 3.0);

        // doubleDigits is I1MACH(14), the number of base-B digits for double
        // precision. IEEE 754 double precision has 64 bits with 1 sign bit,
        // 11 exponent bits, and 52 fraction, mantissa, or signficand bits.
        // Floating-point numbers are typically stored in normalized form,
        // with the radix point after the first non-zero digit. In base two,
        // the only possible non-zero digit is 1. Thus, a leading digit of 1
        // can always be assumed and need not be explicitly represented.
        // Therefore, the mantissa effectively has 53 bits of resolution.
        doubleDigits = 53;
        k1 = doubleDigits - 1;
        aa = r1m5 * k1;
        dig = Math.min(aa, 18.0);
        aa = 2.303 * aa;
        alim = elim + Math.max( -aa, -41.45);
        rl = (1.2 * dig) + 3.0;
        fnul = 10.0 + (6.0 * (dig - 3.0));
        slak = 3.0 + (4.0 * ( ( -0.434294481903251 * Math.log(tol)) - 7.0) / 11.0);
        slak = Math.max(slak, 3.0);
        ertol = tol * Math.pow(10.0, slak);
        rm = 0.5 * (alim + elim);
        rm = Math.min(rm, 200.0);
        rm = Math.max(rm, rl + 10.0);
        r2 = Math.min(rm, fnul);
        Preferences.debug("Quick check routine for the I Bessel function from ZBESI\n", Preferences.DEBUG_ALGORITHM);
        Preferences.debug("tol = " + tol + "\n", Preferences.DEBUG_ALGORITHM);
        Preferences.debug("elim = " + elim + "\n", Preferences.DEBUG_ALGORITHM);
        Preferences.debug("alim = " + alim + "\n", Preferences.DEBUG_ALGORITHM);
        Preferences.debug("rl = " + rl + "\n", Preferences.DEBUG_ALGORITHM);
        Preferences.debug("fnul = " + fnul + "\n", Preferences.DEBUG_ALGORITHM);
        Preferences.debug("dig = " + dig + "\n", Preferences.DEBUG_ALGORITHM);
        atol = 100.0 * tol;
        Preferences.debug("Checks in the (z,fnu) space with mqc = " + mqc + "\n", Preferences.DEBUG_ALGORITHM);

        /**
         * TEST VALUES OF Z IN -PI.LT.ARG(Z).LE.PI NEAR FORMULA BOUNDARIES
         * C-----------------------------------------------------------------------
         * C----------------------------------------------------------------------- C KDO(K), K=1,IL DETERMINES WHICH OF
         * THE IL ANGLES IN -PI TO PI C ARE USE TO COMPUTE VALUES OF Z C KDO(K) = 0 MEANS THAT THE INDEX K WILL BE USED
         * FOR ONE OR TWO C VALUES OF Z, DEPENDING ON THE CHOICE OF KEPS(K) C = 1 MEANS THAT THE INDEX K AND THE
         * CORRESPONDING ANGLE C WILL BE SKIPPED C KEPS(K), K=1,IL DETERMINES WHICH OF THE ANGLES GET INCREMENTED C UP
         * AND DOWN TO PUT VALUES OF Z IN REGIONS WHERE DIFFERENT C FORMULAE ARE USED. C KEPS(K) =0 MEANS THAT THE ANGLE
         * WILL BE USED WITHOUT CHANGE C =1 MEANS THAT THE ANGLE WILL BE INCREMENTED UP AND C DOWN BY EPSC THE ANGLES TO
         * BE USED ARE STORED IN THE T(I) ARRAY, I=1,ITL
         */
        if (mqc != 2) {
            nL = 2;
            il = 5;

            for (i = 1; i <= il; i++) {
                keps[i - 1] = 0;
                kdo[i - 1] = 0;
            } // for (i = 1; i <= il; i++)

            nul = 5;
            xnu[0] = 0.0;
            xnu[1] = 1.0;
            xnu[2] = 2.0;
            xnu[3] = 0.5 * fnul;
            xnu[4] = fnul + 1.1;
        } // if (mqc != 2)
        else { // mqc == 2
            nL = 4;
            il = 13;

            for (i = 1; i <= il; i++) {
                kdo[i - 1] = 0;
                keps[i - 1] = 0;
            } // for (i = 1; i <= il; i++)

            kdo[1] = 1;
            kdo[5] = 1;
            kdo[7] = 1;
            kdo[11] = 1;
            keps[2] = 1;
            keps[3] = 1;
            keps[4] = 1;
            keps[8] = 1;
            keps[9] = 1;
            keps[10] = 1;
            nul = 6;
            xnu[0] = 0.0;
            xnu[1] = 0.6;
            xnu[2] = 1.3;
            xnu[3] = 2.0;
            xnu[4] = 0.5 * fnul;
            xnu[5] = fnul + 1.1;
        } // else mqc == 2

        i = 2;
        eps = 0.01;
        film = il - 1.0;
        t[0] = -Math.PI + eps;

        for (k = 2; k <= il; k++) {

            if (kdo[k - 1] == 0) {
                t[i - 1] = Math.PI * ( -il + (2.0 * k) - 1.0) / film;

                if (keps[k - 1] != 0) {
                    ts = t[i - 1];
                    t[i - 1] = ts - eps;
                    i = i + 1;
                    t[i - 1] = ts + eps;
                } // if (keps[k-1] != 0)

                i = i + 1;
            } // if (kdo[k-1] == 0)
        } // for (k = 2; k <= il; k++)

        itl = i - 1;
        lflg = 0;

        for (kode = 1; kode <= 2; kode++) {

            for (n = 1; n <= nL; n++) {
                nONE = n + 1;

                group: for (nu = 1; nu <= nul; nu++) {
                    fnu = xnu[nu - 1];
                    ifnu = (int) (fnu);
                    ffnu = fnu - ifnu;
                    arg = Math.PI * ffnu;
                    csgnr = Math.cos(arg);
                    csgni = Math.sin(arg);

                    if ( (ifnu % 2) != 0) {
                        csgnr = -csgnr;
                        csgni = -csgni;
                    } // if ((ifnu%2) != 0)

                    for (icase = 1; icase <= 3; icase++) {
                        irb = Math.min(2, icase);

                        for (ir = irb; ir <= 4; ir++) {

                            if (icase == 1) {
                                r = ( (0.2 * (4.0 - ir)) + (2.0 * (ir - 1.0))) / 3.0;
                            } // if (icase == 1)
                            else if (icase == 2) {
                                r = ( (2.0 * (4.0 - ir)) + (r2 * (ir - 1.0))) / 3.0;
                            } // else if (icase == 2)
                            else if ( (icase == 3) && (r2 == rm)) {
                                continue group;
                            } // else if ((icase == 3) && (r2 == rm))
                            else { // icase == 3
                                r = ( (r2 * (4.0 - ir)) + (rm * (ir - 1.0))) / 3.0;
                            } // else icase == 3

                            for (it = 1; it <= itl; it++) {
                                ct = Math.cos(t[it - 1]);
                                st = Math.sin(t[it - 1]);

                                if (Math.abs(ct) < atol) {
                                    ct = 0.0;
                                } // if (Math.abs(ct) < atol)

                                if (Math.abs(st) < atol) {
                                    st = 0.0;
                                } // if (Math.abs(st) < atol)

                                zr = r * ct;
                                zi = r * st;

                                if (ct >= 0.0) {

                                    // Wronskian checks in the right half plane
                                    zbesi(zr, zi, fnu, kode, nONE, wr, wi, nz, ierr);

                                    if ( (nz[0] != 0) || (ierr[0] != 0)) {
                                        continue;
                                    } // if ((nz[0] != 0) || (ierr[0] != 0))

                                    zbesk(zr, zi, fnu, kode, nONE, yr, yi, nz, ierr);

                                    if ( (nz[0] != 0) || (ierr[0] != 0)) {
                                        continue;
                                    } // if ((nz[0] != 0) || (ierr[0] != 0))

                                    // Adjustments to Wronskian due to scaling of I and K
                                    // functions on kode = 2
                                    zdiv(1.0, 0.0, zr, zi, cvr, cvi);

                                    if (kode == 2) {
                                        str[0] = Math.cos(zi);
                                        sti[0] = Math.sin(zi);
                                        aa = (str[0] * cvr[0]) - (sti[0] * cvi[0]);
                                        cvi[0] = (str[0] * cvi[0]) + (sti[0] * cvr[0]);
                                        cvr[0] = aa;
                                    } // if (kode == 2)

                                    ccr = 1.0;
                                } // if (ct >= 0.0)
                                else { // ct < 0.0

                                    // Wronskian checks in the left half plane
                                    zrr = -zr;
                                    zri = -zi;
                                    zbesi(zr, zi, fnu, kode, nONE, yr, yi, nz, ierr);

                                    if ( (nz[0] != 0) || (ierr[0] != 0)) {
                                        continue;
                                    } // if ((nz[0] != 0) || (ierr[0] != 0))

                                    zbesk(zrr, zri, fnu, kode, nONE, wr, wi, nz, ierr);

                                    if ( (nz[0] != 0) || (ierr[0] != 0)) {
                                        continue;
                                    } // if ((nz[0] != 0) || (ierr[0] != 0))

                                    cvr[0] = csgnr;
                                    cvi[0] = csgni;

                                    if (zi < 0.0) {
                                        cvi[0] = -cvi[0];
                                    } // if (zi < 0.0)

                                    zdiv(cvr[0], cvi[0], zr, zi, cvr, cvi);

                                    if (kode == 2) {

                                        // Adjustments to Wronskian due to scaling of I and K
                                        // functions on kode = 2. Scale factor = exp(-i*yy)
                                        // for Re(z) < 0.0
                                        cwr = Math.cos(zi);
                                        cwi = -Math.sin(zi);
                                        str[0] = (cvr[0] * cwr) - (cvi[0] * cwi);
                                        cvi[0] = (cvr[0] * cwi) + (cvi[0] * cwr);
                                        cvr[0] = str[0];
                                    } // if (kode == 2)

                                    ccr = -1.0;
                                } // else ct < 0.0

                                mflg = 0;
                                kk = 0;

                                for (i = 1; i <= n; i++) {
                                    cwr = (wr[i - 1] * yr[i]) - (wi[i - 1] * yi[i]);
                                    cwi = (wr[i - 1] * yi[i]) + (wi[i - 1] * yr[i]);
                                    cyr = (yr[i - 1] * wr[i]) - (yi[i - 1] * wi[i]);
                                    cyi = (yr[i - 1] * wi[i]) + (yi[i - 1] * wr[i]);
                                    cyr = ccr * cyr;
                                    cyi = ccr * cyi;
                                    cyr = cyr + cwr - cvr[0];
                                    cyi = cyi + cwi - cvi[0];
                                    er = zabs(cyr, cyi) / zabs(cvr[0], cvi[0]);
                                    aer[i - 1] = er;

                                    if ( (er > ertol) && (kk == 0)) {
                                        mflg = 1;
                                        kk = i;
                                    } // if ((er > ertol) && (kk == 0))

                                    if (ct < 0.0) {
                                        cvr[0] = -cvr[0];
                                        cvi[0] = -cvi[0];
                                    } // if (ct < 0.0)
                                } // for (i = 1; i <= n; i++)

                                if (mflg == 0) {
                                    continue;
                                } // if (mflg == 0)

                                if (lflg != 1) {
                                    Preferences.debug("Cases which violate the relative error" + " test with ertol = "
                                            + ertol + "\n", Preferences.DEBUG_ALGORITHM);
                                    Preferences.debug("Output format\n", Preferences.DEBUG_ALGORITHM);
                                    Preferences.debug("kode, n, ir, it, icase kk\n", Preferences.DEBUG_ALGORITHM);
                                    Preferences.debug("aer[k-1], k = 1,n z, fnu, y[kk-1]\n", Preferences.DEBUG_ALGORITHM);
                                    Preferences.debug("kk = index of first non-zero pair\n", Preferences.DEBUG_ALGORITHM);
                                    lflg = 1;
                                } // if (lflg != 1)

                                Preferences.debug("kode = " + kode + "\n", Preferences.DEBUG_ALGORITHM);
                                Preferences.debug("n = " + n + "\n", Preferences.DEBUG_ALGORITHM);
                                Preferences.debug("ir = " + ir + "\n", Preferences.DEBUG_ALGORITHM);
                                Preferences.debug("it = " + it + "\n", Preferences.DEBUG_ALGORITHM);
                                Preferences.debug("icase = " + icase + "\n", Preferences.DEBUG_ALGORITHM);
                                Preferences.debug("kk = " + kk + "\n", Preferences.DEBUG_ALGORITHM);

                                for (k = 1; k <= n; k++) {
                                    Preferences.debug("aer[" + (k - 1) + "] = " + aer[k - 1] + "\n",
                                    		Preferences.DEBUG_ALGORITHM);
                                } // for (k = 1; k <= n; k++)

                                Preferences.debug("zr = " + zr + "\n", Preferences.DEBUG_ALGORITHM);
                                Preferences.debug("zi = " + zi + "\n", Preferences.DEBUG_ALGORITHM);
                                Preferences.debug("fnu = " + fnu + "\n", Preferences.DEBUG_ALGORITHM);
                                Preferences.debug("yr[" + (kk - 1) + "] = " + yr[kk - 1] + "\n", Preferences.DEBUG_ALGORITHM);
                                Preferences.debug("yi[" + (kk - 1) + "] = " + yi[kk - 1] + "\n", Preferences.DEBUG_ALGORITHM);
                            } // for (it = 1; it <= itl; it++)
                        } // for (ir = irb; ir <= 4; ir++)
                    } // for (icase = 1; icase <= 3; icase++)
                } // for (nu = 1; nu <= nul; nu++)
            } // for (n = 1; n <= nL; n++)
        } // for (kode = 1; kode <= 2; kode++)

        if (lflg == 0) {
            Preferences.debug("I QUICK CHECKS OK\n", Preferences.DEBUG_ALGORITHM);
        } // if (lflg == 0)

        if (mqc == 1) {
            return;
        } // if (mqc == 1)

        // Checks near underflow limits on series (i=1) and uniform
        // asymptotic expansion (i=2)
        Preferences.debug("I Checks near underflow and overflow limits\n", Preferences.DEBUG_ALGORITHM);
        zr = 1.4;
        zi = 1.4;
        iprnt = 0;

        for (i = 1; i <= 2; i++) {
            fnu = 10.2;
            kode = 1;
            n = 20;

            while (true) {
                zbesi(zr, zi, fnu, kode, n, yr, yi, nz, ierr);

                if (nz[0] == 0) {
                    fnu = fnu + 5.0;

                    continue;
                } // if (nz[0] == 0)

                if (nz[0] < 10) {
                    break;
                } // if (nz[0] < 10)

                fnu = fnu - 1.0;
            } // while (true)

            zbesk(zr, zi, fnu, kode, 2, wr, wi, nz, ierr);
            zdiv(1.0, 0.0, zr, zi, str, sti);
            cyr = (wr[0] * yr[1]) - (wi[0] * yi[1]);
            cyi = (wr[0] * yi[1]) + (wi[0] * yr[1]);
            cwr = (wr[1] * yr[0]) - (wi[1] * yi[0]);
            cwi = (wr[1] * yi[0]) + (wi[1] * yr[0]);
            cwr = cwr + cyr - str[0];
            cwi = cwi + cyi - sti[0];
            er = zabs(cwr, cwi) / zabs(str[0], sti[0]);

            if (er >= ertol) {

                if (iprnt != 1) {
                    Preferences.debug("Output format error, z, fnu, kode, n\n", Preferences.DEBUG_ALGORITHM);
                    iprnt = 1;
                } // if (iprnt != 1)

                Preferences.debug("er = " + er + "\n", Preferences.DEBUG_ALGORITHM);
                Preferences.debug("zr = " + zr + "\n", Preferences.DEBUG_ALGORITHM);
                Preferences.debug("zi = " + zi + "\n", Preferences.DEBUG_ALGORITHM);
                Preferences.debug("fnu = " + fnu + "\n", Preferences.DEBUG_ALGORITHM);
                Preferences.debug("kode = " + kode + "\n", Preferences.DEBUG_ALGORITHM);
                Preferences.debug("n = " + n + "\n", Preferences.DEBUG_ALGORITHM);
            } // if (er >= ertol)

            zr = rl + rl;
            zi = 0.0;
        } // for (i = 1; i <= 2; i++)

        // Check near overflow limits
        zr = elim;
        zi = 0.0;
        fnu = 0.0;

        while (true) {
            zbesk(zr, zi, fnu, kode, n, yr, yi, nz, ierr);

            if (nz[0] < 10) {
                break;
            } // if (nz[0] < 10)

            if (nz[0] == n) {
                fnu = fnu + 3.0;
            } // if (nz[0] == n)

            fnu = fnu + 2.0;
        } // while (true)

        gnu = fnu + n - 2.0;
        zbesi(zr, zi, gnu, kode, 2, wr, wi, nz, ierr);
        zdiv(1.0, 0.0, zr, zi, str, sti);
        cyr = (yr[n - 2] * wr[1]) - (yi[n - 2] * wi[1]);
        cyi = (yr[n - 2] * wi[1]) + (yi[n - 2] * wr[1]);
        cwr = (yr[n - 1] * wr[0]) - (yi[n - 1] * wi[0]);
        cwi = (yr[n - 1] * wi[0]) + (yi[n - 1] * wr[0]);
        cwr = cwr + cyr - str[0];
        cwi = cwi + cyi - sti[0];
        er = zabs(cwr, cwi) / zabs(str[0], sti[0]);

        if (er >= ertol) {

            if (iprnt != 1) {
                Preferences.debug("Output format error, z, fnu, kode, n\n", Preferences.DEBUG_ALGORITHM);
                iprnt = 1;
            } // if (iprnt != 1)

            Preferences.debug("er = " + er + "\n", Preferences.DEBUG_ALGORITHM);
            Preferences.debug("zr = " + zr + "\n", Preferences.DEBUG_ALGORITHM);
            Preferences.debug("zi = " + zi + "\n", Preferences.DEBUG_ALGORITHM);
            Preferences.debug("fnu = " + fnu + "\n", Preferences.DEBUG_ALGORITHM);
            Preferences.debug("kode = " + kode + "\n", Preferences.DEBUG_ALGORITHM);
            Preferences.debug("n = " + n + "\n", Preferences.DEBUG_ALGORITHM);
        } // if (er >= ertol)

        if (iprnt == 0) {
            Preferences.debug("I QUICK CHECKS OK ON UNDERFLOW AND OVERFLOW\n", Preferences.DEBUG_ALGORITHM);
        }

        return;
    }

    /**
     * DOCUMENT ME!
     * 
     * @param mqc int Default is mqc = 1 does not check underflow limits and overflow limits. mqc != 1 checks underflow
     *            and overflow limits
     */
    private void zqcbj(final int mqc) {

        /**
         * ZQCBJ IS A QUICK CHECK ROUTINE FOR THE COMPLEX J BESSEL FUNCTION C GENERATED BY SUBROUTINE ZBESJ. C C ZQCBJ
         * GENERATES SEQUENCES OF J AND H BESSEL FUNCTIONS FROM ZBESJ C AND ZBESH AND CHECKS THE WRONSKIANS C C
         * J(FNU,Z)*H(FNU+1,1,Z)-J(FNU+1,Z)*H(FNU,1,Z)=2/(PI*I*Z) Y.GE.0 C C
         * J(FNU,Z)*H(FNU+1,2,Z)-J(FNU+1,Z)*H(FNU,2,Z)=-2/(PI*I*Z) Y.LT.0 C C IN THEIR RESPECTIVE HALF PLANES. C C THE
         * PARAMETER MQC CAN HAVE VALUES 1 (THE DEFAULT) FOR A FASTER,C LESS DEFINITIVE TEST OR 2 FOR A SLOWER, MORE
         * DEFINITIVE TEST.
         */
        final double[] t = new double[20];
        final double[] aer = new double[25];
        final double[] xnu = new double[20];
        final double[] cjr = new double[20];
        final double[] cji = new double[20];
        final double[] chr = new double[20];
        final double[] chi = new double[20];
        final int[] keps = new int[20];
        final int[] kdo = new int[20];
        double neweps;
        int k;
        int k1;
        double aa;
        double dig;
        double slak;
        double ertol;
        double rm;
        double r2;
        double atol;
        final double hpi = Math.PI / 2.0;
        int nL;
        int il;
        int i;
        int nul;
        double eps;
        double film;
        double ts;
        int itl;
        int lflg;
        int kode;
        double conr;
        double coni;
        int n;
        int np;
        double fnu;
        int nu;
        double gnu;
        int icase;
        int irb;
        int ir;
        double r;
        int it;
        double ct;
        double st;
        double zr;
        double zi;
        final double[] wrr = new double[1];
        final double[] wri = new double[1];
        int m;
        final int[] nzj = new int[1];
        final int[] ierrj = new int[1];
        final int[] nzh = new int[1];
        final int[] ierrh = new int[1];
        double sgn;
        double str;
        double sti;
        double t1r;
        int kk;
        int mflg;
        double t1i;
        double t2r;
        double t2i;
        double cerr;
        double ceri;
        double er;

        /**
         * SET PARAMETERS RELATED TO MACHINE CONSTANTS. TOL IS THE APPROXIMATE UNIT ROUNDOFF LIMITED TO 1.0E-18. ELIM IS
         * THE APPROXIMATE EXPONENTIAL OVER- AND UNDERFLOW LIMIT. EXP(-ELIM).LT.EXP(-ALIM)=EXP(-ELIM)/TOL AND
         * EXP(ELIM).GT.EXP(ALIM)=EXP(ELIM)*TOL ARE INTERVALS NEAR UNDERFLOW AND OVERFLOW LIMITS WHERE SCALED ARITHMETIC
         * IS DONE. RL IS THE LOWER BOUNDARY OF THE ASYMPTOTIC EXPANSION FOR LARGE Z. DIG = NUMBER OF BASE 10 DIGITS IN
         * TOL = 10**(-DIG). FNUL IS THE LOWER BOUNDARY OF THE ASYMPTOTIC SERIES FOR LARGE FNU
         */
        // epsilon = D1MACH(4)
        // Machine epsilon is the smallest positive epsilon such that
        // (1.0 + epsilon) != 1.0.
        // epsilon = 2**(1 - doubleDigits) = 2**(1 - 53) = 2**(-52)
        // epsilon = 2.224460e-16
        // epsilon is called the largest relative spacing
        epsilon = 1.0;
        neweps = 1.0;

        while (true) {

            if (1.0 == (1.0 + neweps)) {
                break;
            } else {
                epsilon = neweps;
                neweps = neweps / 2.0;
            }
        }

        tol = Math.max(epsilon, 1.0e-18);

        // emin, the smallest exponent E for double precision, is I1MACH(15)
        // tiny = D1MACH(1) = 2**(emin - 1) = 2**(-1022)
        // D1MACH(1) is the smallest normalized number, which preserves the
        // full precision of the mantissa.
        // Double.MIN_VALUE = 2**(-1074) is the smallest denormalized number,
        // which preseerves only a portion of the fraction's precision.
        emin = -1021;
        tiny = Math.pow(2, -1022);

        // emax, the largest exponent E for double precision, is I1MACH(16)
        // D1MACH(2) = 2**(emax)*(1 - 2**(-doubleDigits)) = 2**1024*(1 - 2**-53)
        // D1MACH(2) = Double.MAX_VALUE
        emax = 1024;

        // r1m5 = log10(2), which is D1MACH(5)
        r1m5 = 0.434294481903251 * Math.log(2.0);
        k = Math.min(Math.abs(emin), Math.abs(emax));
        elim = 2.303 * ( (k * r1m5) - 3.0);

        // doubleDigits is I1MACH(14), the number of base-B digits for double
        // precision. IEEE 754 double precision has 64 bits with 1 sign bit,
        // 11 exponent bits, and 52 fraction, mantissa, or signficand bits.
        // Floating-point numbers are typically stored in normalized form,
        // with the radix point after the first non-zero digit. In base two,
        // the only possible non-zero digit is 1. Thus, a leading digit of 1
        // can always be assumed and need not be explicitly represented.
        // Therefore, the mantissa effectively has 53 bits of resolution.
        doubleDigits = 53;
        k1 = doubleDigits - 1;
        aa = r1m5 * k1;
        dig = Math.min(aa, 18.0);
        aa = 2.303 * aa;
        alim = elim + Math.max( -aa, -41.45);
        rl = (1.2 * dig) + 3.0;
        fnul = 10.0 + (6.0 * (dig - 3.0));
        slak = 3.0 + (4.0 * ( ( -0.434294481903251 * Math.log(tol)) - 7.0) / 11.0);
        slak = Math.max(slak, 3.0);
        ertol = tol * Math.pow(10.0, slak);
        rm = 0.5 * (alim + elim);
        rm = Math.min(rm, 200.0);
        rm = Math.max(rm, rl + 10.0);
        r2 = Math.min(rm, fnul);
        Preferences.debug("Quick check routine for the J Bessel function from ZBESJ\n", Preferences.DEBUG_ALGORITHM);
        Preferences.debug("tol = " + tol + "\n", Preferences.DEBUG_ALGORITHM);
        Preferences.debug("elim = " + elim + "\n", Preferences.DEBUG_ALGORITHM);
        Preferences.debug("alim = " + alim + "\n", Preferences.DEBUG_ALGORITHM);
        Preferences.debug("rl = " + rl + "\n", Preferences.DEBUG_ALGORITHM);
        Preferences.debug("fnul = " + fnul + "\n", Preferences.DEBUG_ALGORITHM);
        Preferences.debug("dig = " + dig + "\n", Preferences.DEBUG_ALGORITHM);
        atol = 100.0 * tol;
        conr = 0.0;
        coni = -1.0 / hpi;
        Preferences.debug("Checks in the (z,fnu) space with mqc = " + mqc + "\n", Preferences.DEBUG_ALGORITHM);

        /**
         * TEST VALUES OF Z IN -PI.LT.ARG(Z).LE.PI NEAR FORMULA BOUNDARIES
         * C-----------------------------------------------------------------------
         * C----------------------------------------------------------------------- C KDO(K), K=1,IL DETERMINES WHICH OF
         * THE IL ANGLES IN -PI TO PI C ARE USE TO COMPUTE VALUES OF Z C KDO(K) = 0 MEANS THAT THE INDEX K WILL BE USED
         * FOR ONE OR TWO C VALUES OF Z, DEPENDING ON THE CHOICE OF KEPS(K) C = 1 MEANS THAT THE INDEX K AND THE
         * CORRESPONDING ANGLE C WILL BE SKIPPED C KEPS(K), K=1,IL DETERMINES WHICH OF THE ANGLES GET INCREMENTED C UP
         * AND DOWN TO PUT VALUES OF Z IN REGIONS WHERE DIFFERENT C FORMULAE ARE USED. C KEPS(K) =0 MEANS THAT THE ANGLE
         * WILL BE USED WITHOUT CHANGE C =1 MEANS THAT THE ANGLE WILL BE INCREMENTED UP AND C DOWN BY EPSC THE ANGLES TO
         * BE USED ARE STORED IN THE T(I) ARRAY, I=1,ITL
         */
        if (mqc != 2) {
            nL = 2;
            il = 5;

            for (i = 1; i <= il; i++) {
                keps[i - 1] = 0;
                kdo[i - 1] = 0;
            } // for (i = 1; i <= il; i++)

            nul = 5;
            xnu[0] = 0.0;
            xnu[1] = 1.0;
            xnu[2] = 2.0;
            xnu[3] = 0.5 * fnul;
            xnu[4] = fnul + 1.1;
        } // if (mqc != 2)
        else { // mqc == 2
            nL = 4;
            il = 13;

            for (i = 1; i <= il; i++) {
                kdo[i - 1] = 0;
                keps[i - 1] = 0;
            } // for (i = 1; i <= il; i++)

            kdo[1] = 1;
            kdo[5] = 1;
            kdo[7] = 1;
            kdo[11] = 1;
            keps[2] = 1;
            keps[3] = 1;
            keps[4] = 1;
            keps[8] = 1;
            keps[9] = 1;
            keps[10] = 1;
            nul = 6;
            xnu[0] = 0.0;
            xnu[1] = 0.6;
            xnu[2] = 1.3;
            xnu[3] = 2.0;
            xnu[4] = 0.5 * fnul;
            xnu[5] = fnul + 1.1;
        } // else mqc == 2

        i = 2;
        eps = 0.01;
        film = il - 1.0;
        t[0] = -Math.PI + eps;

        for (k = 2; k <= il; k++) {

            if (kdo[k - 1] == 0) {
                t[i - 1] = Math.PI * ( -il + (2.0 * k) - 1.0) / film;

                if (keps[k - 1] != 0) {
                    ts = t[i - 1];
                    t[i - 1] = ts - eps;
                    i = i + 1;
                    t[i - 1] = ts + eps;
                } // if (keps[k-1] != 0)

                i = i + 1;
            } // if (kdo[k-1] == 0)
        } // for (k = 2; k <= il; k++)

        itl = i - 1;
        lflg = 0;

        for (kode = 1; kode <= 2; kode++) {

            for (n = 1; n <= nL; n++) {
                np = n + 1;

                for (nu = 1; nu <= nul; nu++) {
                    fnu = xnu[nu - 1];
                    gnu = fnu + n;
                    gnu = Math.sqrt(gnu);
                    gnu = Math.min(gnu, 0.5 * rl);

                    group: for (icase = 1; icase <= 3; icase++) {
                        irb = Math.min(2, icase);

                        for (ir = irb; ir <= 4; ir++) {

                            if (icase == 1) {
                                r = ( (gnu * (4.0 - ir)) + (2.0 * (ir - 1.0))) / 3.0;
                            } // if (icase == 1)
                            else if (icase == 2) {
                                r = ( (2.0 * (4.0 - ir)) + (r2 * (ir - 1.0))) / 3.0;
                            } // else if (icase == 2)
                            else if ( (icase == 3) && (r2 >= rm)) {
                                continue group;
                            } // else if ((icase == 3) && (r2 >= rm))
                            else {
                                r = ( (r2 * (4.0 - ir)) + (rm * (ir - 1.0))) / 3.0;
                            } // else

                            for (it = 1; it <= itl; it++) {
                                ct = Math.cos(t[it - 1]);
                                st = Math.sin(t[it - 1]);

                                if (Math.abs(ct) < atol) {
                                    ct = 0.0;
                                } // if Math.abs(ct) < atol)

                                if (Math.abs(st) < atol) {
                                    st = 0.0;
                                } // if (Math.abs(st) < atol)

                                zr = r * ct;
                                zi = r * st;

                                if ( (zr == 0.0) && (zi == 0.0)) {
                                    continue;
                                } // if ((zr == 0.0) && (zi == 0.0))

                                zdiv(conr, coni, zr, zi, wrr, wri);
                                m = 1;

                                if (zi < 0.0) {
                                    m = 2;
                                    wrr[0] = -wrr[0];
                                    wri[0] = -wri[0];
                                } // if (zi < 0.0)

                                zbesj(zr, zi, fnu, kode, np, cjr, cji, nzj, ierrj);
                                zbesh(zr, zi, fnu, kode, m, np, chr, chi, nzh, ierrh);

                                if ( (nzj[0] != 0) || (nzh[0] != 0) || (ierrj[0] != 0) || (ierrh[0] != 0)) {
                                    continue;
                                }

                                if (kode == 2) {
                                    sgn = 3.0 - (2.0 * m);
                                    str = Math.cos(zr);
                                    sti = -sgn * Math.sin(zr);
                                    t1r = (wrr[0] * str) - (wri[0] * sti);
                                    wri[0] = (wrr[0] * sti) + (wri[0] * str);
                                    wrr[0] = t1r;
                                } // if (kode == 2)

                                kk = 0;
                                mflg = 0;

                                for (i = 1; i <= n; i++) {
                                    str = (cjr[i - 1] * chr[i]) - (cji[i - 1] * chi[i]);
                                    t1i = (cjr[i - 1] * chi[i]) + (cji[i - 1] * chr[i]);
                                    t1r = str;
                                    str = (cjr[i] * chr[i - 1]) - (cji[i] * chi[i - 1]);
                                    t2i = (cjr[i] * chi[i - 1]) + (cji[i] * chr[i - 1]);
                                    t2r = str;
                                    cerr = t1r - t2r - wrr[0];
                                    ceri = t1i - t2i - wri[0];
                                    er = zabs(cerr, ceri) / zabs(wrr[0], wri[0]);

                                    if ( (er > ertol) && (mflg == 0)) {
                                        mflg = 1;
                                        kk = i;
                                    } // if ((er > ertol) && (mflg == 0))

                                    aer[i - 1] = er;
                                } // for (i = 1; i <= n; i++)

                                if (mflg == 0) {
                                    continue;
                                } // if (mflg == 0)

                                if (lflg != 1) {
                                    Preferences.debug("Cases which violate the relative error " + "test with ertol = "
                                            + ertol + "\n", Preferences.DEBUG_ALGORITHM);
                                    Preferences.debug("OUTPUT FORMAT kode,n,ir,it,nzj,nzh,icase\n", 
                                    		Preferences.DEBUG_ALGORITHM);
                                    Preferences.debug("aer[k-1], k=1,n z,fnu,cj[kk-1],ch[kk-1], "
                                            + "kk = index of first non-zero y,w pair\n", Preferences.DEBUG_ALGORITHM);
                                    lflg = 1;
                                } // if (lflg != 1)

                                Preferences.debug("kode = " + kode + "\n", Preferences.DEBUG_ALGORITHM);
                                Preferences.debug("n = " + n + "\n", Preferences.DEBUG_ALGORITHM);
                                Preferences.debug("ir = " + ir + "\n", Preferences.DEBUG_ALGORITHM);
                                Preferences.debug("it = " + it + "\n", Preferences.DEBUG_ALGORITHM);
                                Preferences.debug("nzj = " + nzj[0] + "\n", Preferences.DEBUG_ALGORITHM);
                                Preferences.debug("nzh = " + nzh[0] + "\n", Preferences.DEBUG_ALGORITHM);
                                Preferences.debug("icase = " + icase + "\n", Preferences.DEBUG_ALGORITHM);

                                for (k = 1; k <= n; k++) {
                                    Preferences.debug("aer[" + (k - 1) + "] = " + aer[k - 1] + "\n", 
                                    		Preferences.DEBUG_ALGORITHM);
                                } // for (k = 1; k <= n; k++)

                                Preferences.debug("zr = " + zr + "\n", Preferences.DEBUG_ALGORITHM);
                                Preferences.debug("zi = " + zi + "\n", Preferences.DEBUG_ALGORITHM);
                                Preferences.debug("fnu = " + fnu + "\n", Preferences.DEBUG_ALGORITHM);
                                Preferences.debug("cjr[" + (kk - 1) + "] = " + cjr[kk - 1] + "\n", 
                                		Preferences.DEBUG_ALGORITHM);
                                Preferences.debug("cji[" + (kk - 1) + "] = " + cji[kk - 1] + "\n", 
                                		Preferences.DEBUG_ALGORITHM);
                                Preferences.debug("chr[" + (kk - 1) + "] = " + chr[kk - 1] + "\n", 
                                		Preferences.DEBUG_ALGORITHM);
                                Preferences.debug("chi[" + (kk - 1) + "] = " + chi[kk - 1] + "\n", 
                                		Preferences.DEBUG_ALGORITHM);
                            } // for (it = 1; it <= itl; it++)
                        } // for (ir = irb; ir <= 4; ir++)
                    } // for (icase = 1; icase <= 3; icase++)
                } // for (nu = 1; nu <= nul; nu++)
            } // for (n = 1; n <= nL; n++)
        } // for (kode = 1; kode <= 2; kode++)

        if (lflg == 0) {
            Preferences.debug("J QUICK CHECKS OK\n", Preferences.DEBUG_ALGORITHM);
        } // if (lflg == 0)

        return;
    }

    /**
     * DOCUMENT ME!
     * 
     * @param mqc int Default is mqc = 1 does not check underflow limits and overflow limits. mqc != 1 checks underflow
     *            and overflow limits
     */
    private void zqcbk(final int mqc) {

        /**
         * ZQCBK IS A QUICK CHECK ROUTINE FOR THE COMPLEX K BESSEL FUNCTION C GENERATED BY SUBROUTINE ZBESK. C C ZQCBK
         * GENERATES SEQUENCES OF I AND K BESSEL FUNCTIONS FROM C ZBESI AND ZBESK AND CHECKS THEM AGAINST THE WRONSKIAN
         * EVALUATION C C I(FNU,Z)*K(FNU+1,Z) + I(FNU+1,Z)*K(FNU,Z) = 1/Z C C IN THE RIGHT HALF PLANE AND THE ANALYTIC
         * CONTINUATION FORMULA C FOR H(FNU,2,Z) IN TERMS OF THE K FUNCTION C C K(FNU,Z) = C3*H(FNU,2,ZR) +
         * C4*H(FNU,1,ZR) IM(Z).GE.0 C C = CONJG(K(FNU,CONJG(Z))) IM(Z).LT.0 C C IN THE LEFT HALF PLANE WHERE
         * C3=C1*CONJG(C2)*C5, C4 = C2*C5 C C1=2*COS(PI*FNU), C2=EXP(PI*FNU*I/2), C5 =-PI*I/2 AND C ZR =
         * Z*EXP(-3*PI*I/2) = Z*I C C THE PARAMETER MQC CAN HAVE VALUES 1 (THE DEFAULT) FOR A FASTER,C LESS DEFINITIVE
         * TEST OR 2 FOR A SLOWER, MORE DEFINITIVE TEST.
         */
        double neweps;
        int k;
        int k1;
        double aa;
        double dig;
        double slak;
        double ertol;
        double rm;
        double r2;
        double atol;
        double hpi = Math.PI / 2.0;
        int nL;
        int il;
        final double[] t = new double[20];
        final double[] aer = new double[25];
        final double[] xnu = new double[20];
        final double[] vr = new double[20];
        final double[] vi = new double[20];
        final double[] yr = new double[20];
        final double[] yi = new double[20];
        final double[] wr = new double[20];
        final double[] wi = new double[20];
        final int[] keps = new int[20];
        final int[] kdo = new int[20];
        final double[] cipr = new double[] {1.0, 0.0, -1.0, 0.0};
        final double[] cipi = new double[] {0.0, 1.0, 0.0, -1.0};
        int i;
        int nul;
        double eps;
        double film;
        double ts;
        int itl;
        int lflg;
        int n;
        int nONE;
        int nu;
        double fnu;
        int ifnu;
        double ffnu;
        double arg;
        double csgnr;
        double csgni;
        int i4;
        double str;
        int kode;
        int icase;
        int irb;
        int ir;
        double r;
        int it;
        double ct;
        double st;
        double zr;
        double zi;
        final int[] nz = new int[1];
        final int[] ierr = new int[1];
        final double[] cvr = new double[1];
        final double[] cvi = new double[1];
        int mflg;
        int kk;
        double er;
        double zzr;
        double zzi;
        double zrr;
        double zri;
        int m;
        double coei;
        double c1r;
        double c1i;
        double c2r;
        double c2i;
        double c3r;
        double c3i;
        double c4r;
        double c4i;
        double ab;
        double cyr;
        double cyi;
        double sti;
        double cwr;
        double cwi;

        /**
         * SET PARAMETERS RELATED TO MACHINE CONSTANTS. TOL IS THE APPROXIMATE UNIT ROUNDOFF LIMITED TO 1.0E-18. ELIM IS
         * THE APPROXIMATE EXPONENTIAL OVER- AND UNDERFLOW LIMIT. EXP(-ELIM).LT.EXP(-ALIM)=EXP(-ELIM)/TOL AND
         * EXP(ELIM).GT.EXP(ALIM)=EXP(ELIM)*TOL ARE INTERVALS NEAR UNDERFLOW AND OVERFLOW LIMITS WHERE SCALED ARITHMETIC
         * IS DONE. RL IS THE LOWER BOUNDARY OF THE ASYMPTOTIC EXPANSION FOR LARGE Z. DIG = NUMBER OF BASE 10 DIGITS IN
         * TOL = 10**(-DIG). FNUL IS THE LOWER BOUNDARY OF THE ASYMPTOTIC SERIES FOR LARGE FNU
         */
        // epsilon = D1MACH(4)
        // Machine epsilon is the smallest positive epsilon such that
        // (1.0 + epsilon) != 1.0.
        // epsilon = 2**(1 - doubleDigits) = 2**(1 - 53) = 2**(-52)
        // epsilon = 2.224460e-16
        // epsilon is called the largest relative spacing
        epsilon = 1.0;
        neweps = 1.0;

        while (true) {

            if (1.0 == (1.0 + neweps)) {
                break;
            } else {
                epsilon = neweps;
                neweps = neweps / 2.0;
            }
        }

        tol = Math.max(epsilon, 1.0e-18);

        // emin, the smallest exponent E for double precision, is I1MACH(15)
        // tiny = D1MACH(1) = 2**(emin - 1) = 2**(-1022)
        // D1MACH(1) is the smallest normalized number, which preserves the
        // full precision of the mantissa.
        // Double.MIN_VALUE = 2**(-1074) is the smallest denormalized number,
        // which preseerves only a portion of the fraction's precision.
        emin = -1021;
        tiny = Math.pow(2, -1022);

        // emax, the largest exponent E for double precision, is I1MACH(16)
        // D1MACH(2) = 2**(emax)*(1 - 2**(-doubleDigits)) = 2**1024*(1 - 2**-53)
        // D1MACH(2) = Double.MAX_VALUE
        emax = 1024;

        // r1m5 = log10(2), which is D1MACH(5)
        r1m5 = 0.434294481903251 * Math.log(2.0);
        k = Math.min(Math.abs(emin), Math.abs(emax));
        elim = 2.303 * ( (k * r1m5) - 3.0);

        // doubleDigits is I1MACH(14), the number of base-B digits for double
        // precision. IEEE 754 double precision has 64 bits with 1 sign bit,
        // 11 exponent bits, and 52 fraction, mantissa, or signficand bits.
        // Floating-point numbers are typically stored in normalized form,
        // with the radix point after the first non-zero digit. In base two,
        // the only possible non-zero digit is 1. Thus, a leading digit of 1
        // can always be assumed and need not be explicitly represented.
        // Therefore, the mantissa effectively has 53 bits of resolution.
        doubleDigits = 53;
        k1 = doubleDigits - 1;
        aa = r1m5 * k1;
        dig = Math.min(aa, 18.0);
        aa = 2.303 * aa;
        alim = elim + Math.max( -aa, -41.45);
        rl = (1.2 * dig) + 3.0;
        fnul = 10.0 + (6.0 * (dig - 3.0));
        slak = 3.0 + (4.0 * ( ( -0.434294481903251 * Math.log(tol)) - 7.0) / 11.0);
        slak = Math.max(slak, 3.0);
        ertol = tol * Math.pow(10.0, slak);
        rm = 0.5 * (alim + elim);
        rm = Math.min(rm, 200.0);
        rm = Math.max(rm, rl + 10.0);
        r2 = Math.min(rm, fnul);
        Preferences.debug("QUICK CHECK ROUTINE FOR THE K BESSEL FUNCTION FROM\n", Preferences.DEBUG_ALGORITHM);
        Preferences.debug("ZBESK and ZHESK\n", Preferences.DEBUG_ALGORITHM);
        Preferences.debug("tol = " + tol + "\n", Preferences.DEBUG_ALGORITHM);
        Preferences.debug("elim = " + elim + "\n", Preferences.DEBUG_ALGORITHM);
        Preferences.debug("alim = " + alim + "\n", Preferences.DEBUG_ALGORITHM);
        Preferences.debug("rl = " + rl + "\n", Preferences.DEBUG_ALGORITHM);
        Preferences.debug("fnul = " + fnul + "\n", Preferences.DEBUG_ALGORITHM);
        Preferences.debug("dig = " + dig + "\n", Preferences.DEBUG_ALGORITHM);
        atol = 100.0 * tol;
        Preferences.debug("Checks in the (z,fnu) space with mqc = " + mqc + "\n", Preferences.DEBUG_ALGORITHM);

        /**
         * TEST VALUES OF Z IN -PI.LT.ARG(Z).LE.PI NEAR FORMULA BOUNDARIES
         * C-----------------------------------------------------------------------
         * C----------------------------------------------------------------------- C KDO(K), K=1,IL DETERMINES WHICH OF
         * THE IL ANGLES IN -PI TO PI C ARE USE TO COMPUTE VALUES OF Z C KDO(K) = 0 MEANS THAT THE INDEX K WILL BE USED
         * FOR ONE OR TWO C VALUES OF Z, DEPENDING ON THE CHOICE OF KEPS(K) C = 1 MEANS THAT THE INDEX K AND THE
         * CORRESPONDING ANGLE C WILL BE SKIPPED C KEPS(K), K=1,IL DETERMINES WHICH OF THE ANGLES GET INCREMENTED C UP
         * AND DOWN TO PUT VALUES OF Z IN REGIONS WHERE DIFFERENT C FORMULAE ARE USED. C KEPS(K) =0 MEANS THAT THE ANGLE
         * WILL BE USED WITHOUT CHANGE C =1 MEANS THAT THE ANGLE WILL BE INCREMENTED UP AND C DOWN BY EPSC THE ANGLES TO
         * BE USED ARE STORED IN THE T(I) ARRAY, I=1,ITL
         */
        if (mqc != 2) {
            nL = 2;
            il = 5;

            for (i = 1; i <= il; i++) {
                keps[i - 1] = 0;
                kdo[i - 1] = 0;
            } // for (i = 1; i <= il; i++)

            nul = 5;
            xnu[0] = 0.0;
            xnu[1] = 1.0;
            xnu[2] = 2.0;
            xnu[3] = 0.5 * fnul;
            xnu[4] = fnul + 1.1;
        } // if (mqc != 2)
        else { // mqc == 2
            nL = 4;
            il = 13;

            for (i = 1; i <= il; i++) {
                kdo[i - 1] = 0;
                keps[i - 1] = 0;
            } // for (i = 1; i <= il; i++)

            kdo[1] = 1;
            kdo[5] = 1;
            kdo[7] = 1;
            kdo[11] = 1;
            keps[2] = 1;
            keps[3] = 1;
            keps[4] = 1;
            keps[8] = 1;
            keps[9] = 1;
            keps[10] = 1;
            nul = 6;
            xnu[0] = 0.0;
            xnu[1] = 0.6;
            xnu[2] = 1.3;
            xnu[3] = 2.0;
            xnu[4] = 0.5 * fnul;
            xnu[5] = fnul + 1.1;
        } // else mqc == 2

        i = 2;
        eps = 0.01;
        film = il - 1.0;
        t[0] = -Math.PI + eps;

        for (k = 2; k <= il; k++) {

            if (kdo[k - 1] == 0) {
                t[i - 1] = Math.PI * ( -il + (2.0 * k) - 1.0) / film;

                if (keps[k - 1] != 0) {
                    ts = t[i - 1];
                    t[i - 1] = ts - eps;
                    i = i + 1;
                    t[i - 1] = ts + eps;
                } // if (keps[k-1] != 0)

                i = i + 1;
            } // if (kdo[k-1] == 0)
        } // for (k = 2; k <= il; k++)

        itl = i - 1;
        lflg = 0;

        for (kode = 1; kode <= 2; kode++) {

            for (n = 1; n <= nL; n++) {
                nONE = n + 1;

                group: for (nu = 1; nu <= nul; nu++) {
                    fnu = xnu[nu - 1];
                    ifnu = (int) (fnu);
                    ffnu = fnu - ifnu;
                    arg = hpi * ffnu;
                    csgnr = Math.cos(arg);
                    csgni = Math.sin(arg);
                    i4 = (ifnu % 4) + 1;
                    str = (csgnr * cipr[i4 - 1]) - (csgni * cipi[i4 - 1]);
                    csgni = (csgnr * cipi[i4 - 1]) + (csgni * cipr[i4 - 1]);
                    csgnr = str;

                    for (icase = 1; icase <= 3; icase++) {
                        irb = Math.min(2, icase);

                        for (ir = irb; ir <= 4; ir++) {

                            if (icase == 1) {
                                r = ( (0.2 * (4.0 - ir)) + (2.0 * (ir - 1.0))) / 3.0;
                            } // if (icase == 1)
                            else if (icase == 2) {
                                r = ( (2.0 * (4.0 - ir)) + (r2 * (ir - 1.0))) / 3.0;
                            } // else if (icase == 2)
                            else if ( (icase == 3) && (r2 == rm)) {
                                continue group;
                            } // else if ((icase == 3) && (r2 == rm))
                            else { // icase == 3
                                r = ( (r2 * (4.0 - ir)) + (rm * (ir - 1.0))) / 3.0;
                            } // else icase == 3

                            group2: for (it = 1; it <= itl; it++) {
                                ct = Math.cos(t[it - 1]);
                                st = Math.sin(t[it - 1]);

                                if (Math.abs(ct) < atol) {
                                    ct = 0.0;
                                } // if (Math.abs(ct) < atol)

                                if (Math.abs(st) < atol) {
                                    st = 0.0;
                                } // if (Math.abs(st) < atol)

                                zr = r * ct;
                                zi = r * st;

                                if (zr >= 0.0) {

                                    // Wronskian checks in the right half plane
                                    zbesi(zr, zi, fnu, kode, nONE, wr, wi, nz, ierr);

                                    if ( (nz[0] != 0) || (ierr[0] != 0)) {
                                        continue;
                                    } // if ((nz[0] != 0) || (ierr[0] != 0))

                                    zbesk(zr, zi, fnu, kode, nONE, yr, yi, nz, ierr);

                                    if ( (nz[0] != 0) || (ierr[0] != 0)) {
                                        continue;
                                    } // if ((nz[0] != 0) || (ierr[0] != 0))

                                    // Adjustments to Wronskian due to scaling of I and K
                                    // functions on kode = 2
                                    zdiv(1.0, 0.0, zr, zi, cvr, cvi);

                                    if (kode == 2) {
                                        str = Math.cos(zi);
                                        sti = Math.sin(zi);
                                        aa = (str * cvr[0]) - (sti * cvi[0]);
                                        cvi[0] = (str * cvi[0]) + (sti * cvr[0]);
                                        cvr[0] = aa;
                                    } // if (kode == 2)

                                    mflg = 0;
                                    kk = 0;

                                    for (i = 1; i <= n; i++) {
                                        cwr = (wr[i - 1] * yr[i]) - (wi[i - 1] * yi[i]);
                                        cwi = (wr[i - 1] * yi[i]) + (wi[i - 1] * yr[i]);
                                        cyr = (yr[i - 1] * wr[i]) - (yi[i - 1] * wi[i]);
                                        cyi = (yr[i - 1] * wi[i]) + (yi[i - 1] * wr[i]);
                                        cyr = cyr + cwr - cvr[0];
                                        cyi = cyi + cwi - cvi[0];
                                        er = zabs(cyr, cyi) / zabs(cvr[0], cvi[0]);
                                        aer[i - 1] = er;

                                        if ( (er > ertol) && (kk == 0)) {
                                            mflg = 1;
                                            kk = i;
                                        } // if (er > ertol) && (kk == 0))
                                    } // for (i = 1; i <= n; i++)
                                } // if (zr >= 0.0)
                                else { // zr < 0.0

                                    // Analytic continuation formula checks for left half
                                    // plane in terms of H(fnu,1,z) and H(fnu,2,z)
                                    zzr = zr;
                                    zzi = zi;

                                    if (zi < 0.0) {
                                        zzi = -zzi;
                                    } // if (zi < 0.0)

                                    zrr = -zzi;
                                    zri = zzr;
                                    m = 1;
                                    zbesh(zrr, zri, fnu, kode, m, n, wr, wi, nz, ierr);

                                    if (ierr[0] != 0) {
                                        continue;
                                    } // if (ierr[0] != 0)

                                    m = 2;
                                    zbesh(zrr, zri, fnu, kode, m, n, vr, vi, nz, ierr);

                                    if (ierr[0] != 0) {
                                        continue;
                                    } // if (ierr[0] != 0)

                                    zbesk(zr, zi, fnu, kode, n, yr, yi, nz, ierr);

                                    if ( (nz[0] != 0) || (ierr[0] != 0)) {
                                        continue;
                                    } // if ((nz[0] != 0) || (ierr[0] != 0))

                                    coei = -hpi;
                                    mflg = 0;
                                    kk = 0;
                                    aa = 2.0 * Math.cos(Math.PI * ffnu);

                                    if ( (ifnu % 2) != 0) {
                                        aa = -aa;
                                    } // if ((ifnu%2) != 0)

                                    c1r = aa;
                                    c1i = 0.0;
                                    c2r = csgnr;
                                    c2i = csgni;

                                    for (i = 1; i <= n; i++) {
                                        c3r = c1r;
                                        c3i = c1i;
                                        c4r = c2r;
                                        c4i = c2i;

                                        if (kode == 2) {

                                            // Adjustments to coefficients due to scaling of
                                            // H(fnu,1,z) and H(fnu,2,z) functions on kode = 2.
                                            ab = zabs(vr[i - 1], vi[i - 1]);
                                            aa = Math.log(ab) + zr + zr;

                                            if (aa > elim) {
                                                continue group2;
                                            } // if (aa > elim)

                                            if (aa < -elim) {
                                                c3r = 0.0;
                                                c3i = 0.0;
                                            } // if (aa < -elim)
                                            else { // aa >= -elim
                                                str = zzr + zzr;
                                                sti = zzi + zzi;
                                                zexp(str, sti, cvr, cvi);
                                                str = (c3r * cvr[0]) - (c3i * cvi[0]);
                                                c3i = (c3r * cvi[0]) + (c3i * cvr[0]);
                                                c3r = str;
                                            } // else aa >= -elim
                                        } // if (kode == 2)

                                        str = (c3r * c2r) + (c3i * c2i);
                                        sti = ( -c3r * c2i) + (c3i * c2r);
                                        cyr = (str * vr[i - 1]) - (sti * vi[i - 1]);
                                        cyi = (str * vi[i - 1]) + (sti * vr[i - 1]);
                                        cyr = cyr + (c4r * wr[i - 1]) - (c4i * wi[i - 1]);
                                        cyi = cyi + (c4r * wi[i - 1]) + (c4i * wr[i - 1]);
                                        str = -cyi * coei;
                                        cyi = cyr * coei;
                                        cyr = str;

                                        if (zi < 0.0) {
                                            cyi = -cyi;
                                        } // if (zi < 0.0)

                                        str = cyr - yr[i - 1];
                                        sti = cyi - yi[i - 1];
                                        er = zabs(str, sti) / zabs(yr[i - 1], yi[i - 1]);
                                        aer[i - 1] = er;

                                        if ( (er > ertol) && (kk == 0)) {
                                            mflg = 1;
                                            kk = i;
                                        } // if (er > ertol) && (kk == 0)

                                        str = -c2i;
                                        c2i = c2r;
                                        c2r = str;
                                        c1r = -c1r;
                                        c1i = -c1i;
                                    } // for (i = 1; i <= n; i++)
                                } // else zr < 0.0

                                if (mflg == 0) {
                                    continue;
                                } // if (mflg == 0)

                                if (lflg != 1) {
                                    Preferences.debug("Cases which violate the relative error" + " test with ertol = "
                                            + ertol + "\n", Preferences.DEBUG_ALGORITHM);
                                    Preferences.debug("Output format kode,n,ir,it,icase,kk\n", Preferences.DEBUG_ALGORITHM);
                                    Preferences.debug("aer[k-1], k=1,n z,fnu,y[kk-1] "
                                            + "kk = index of first non-zero pair\n", Preferences.DEBUG_ALGORITHM);
                                    lflg = 1;
                                } // if (lflg != 1)

                                Preferences.debug("kode = " + kode + "\n", Preferences.DEBUG_ALGORITHM);
                                Preferences.debug("n = " + n + "\n", Preferences.DEBUG_ALGORITHM);
                                Preferences.debug("ir = " + ir + "\n", Preferences.DEBUG_ALGORITHM);
                                Preferences.debug("it = " + it + "\n", Preferences.DEBUG_ALGORITHM);
                                Preferences.debug("icase = " + icase + "\n", Preferences.DEBUG_ALGORITHM);
                                Preferences.debug("kk = " + kk + "\n", Preferences.DEBUG_ALGORITHM);

                                for (k = 1; k <= n; k++) {
                                    Preferences.debug("aer[" + (k - 1) + "] = " + aer[k - 1] + "\n", 
                                    		Preferences.DEBUG_ALGORITHM);
                                } // for (k = 1; k <= n; k++)

                                Preferences.debug("zr = " + zr + "\n", Preferences.DEBUG_ALGORITHM);
                                Preferences.debug("zi = " + zi + "\n", Preferences.DEBUG_ALGORITHM);
                                Preferences.debug("fnu = " + fnu + "\n", Preferences.DEBUG_ALGORITHM);
                                Preferences.debug("yr[" + (kk - 1) + "] = " + yr[kk - 1] + "\n", Preferences.DEBUG_ALGORITHM);
                                Preferences.debug("yi[" + (kk - 1) + "] = " + yi[kk - 1] + "\n", Preferences.DEBUG_ALGORITHM);
                            } // for (it = 1; it <= itl; it++)
                        } // for (ir = irb; ir <= 4; ir++)
                    } // for (icase = 1; icase <= 3; icase++)
                } // for (nu = 1; nu <= nul; nu++)
            } // for (n = 1; n <= nL; n++)
        } // for (kode = 1; kode <= 2; kode++)

        if (lflg == 0) {
            Preferences.debug("K QUICK CHECKS OK\n", Preferences.DEBUG_ALGORITHM);
        } // if (lflg == 0)

        return;
    }

    /**
     * DOCUMENT ME!
     * 
     * @param mqc int Default is mqc = 1 does not check underflow limits and overflow limits. mqc != 1 checks underflow
     *            and overflow limits
     */
    private void zqcby(final int mqc) {

        /**
         * ZQCBY IS A QUICK CHECK ROUTINE FOR THE COMPLEX Y BESSEL FUNCTION C GENERATED BY SUBROUTINE ZBESY. C C ZQCBY
         * GENERATES SEQUENCES OF Y BESSEL FUNCTIONS FROM ZBESY AND C ZBESYH AND COMPARES THEM FOR A VARIETY OF VALUES
         * IN THE (Z,FNU) C SPACE. ZBESYH IS AN OLD VERSION OF ZBESY WHICH COMPUTES THE Y C FUNCTION FROM THE H
         * FUNCTIONS OF KINDS 1 AND 2. C C THE PARAMETER MQC CAN HAVE VALUES 1 (THE DEFAULT) FOR A FASTER,C LESS
         * DEFINITIVE TEST OR 2 FOR A SLOWER, MORE DEFINITIVE TEST.
         */
        double neweps;
        int k;
        int k1;
        double aa;
        double dig;
        double slak;
        double ertol;
        double rm;
        double r2;
        double atol;
        int nL;
        int il;
        final double[] t = new double[20];
        final double[] aer = new double[20];
        final double[] xnu = new double[20];
        final double[] wr = new double[20];
        final double[] wi = new double[20];
        final double[] vr = new double[20];
        final double[] vi = new double[20];
        final double[] cwrkr = new double[20];
        final double[] cwrki = new double[20];
        final int[] kdo = new int[20];
        final int[] keps = new int[20];
        int i;
        int nul;
        double eps;
        double film;
        double ts;
        int itl;
        int lflg;
        int kode;
        int n;
        int nu;
        double fnu;
        int ifnu;
        double ffnu;
        int icase;
        int irb;
        int ir;
        double r;
        int it;
        double ct;
        double st;
        double zr;
        double zi;
        final int[] nz2 = new int[1];
        final int[] ierr = new int[1];
        final int[] nz1 = new int[1];
        int mflg;
        double ab;
        double cwr;
        double cwi;
        double av;
        double er;
        int kk;

        /**
         * SET PARAMETERS RELATED TO MACHINE CONSTANTS. TOL IS THE APPROXIMATE UNIT ROUNDOFF LIMITED TO 1.0E-18. ELIM IS
         * THE APPROXIMATE EXPONENTIAL OVER- AND UNDERFLOW LIMIT. EXP(-ELIM).LT.EXP(-ALIM)=EXP(-ELIM)/TOL AND
         * EXP(ELIM).GT.EXP(ALIM)=EXP(ELIM)*TOL ARE INTERVALS NEAR UNDERFLOW AND OVERFLOW LIMITS WHERE SCALED ARITHMETIC
         * IS DONE. RL IS THE LOWER BOUNDARY OF THE ASYMPTOTIC EXPANSION FOR LARGE Z. DIG = NUMBER OF BASE 10 DIGITS IN
         * TOL = 10**(-DIG).FNUL IS THE LOWER BOUNDARY OF THE ASYMPTOTIC SERIES FOR LARGE FNU
         */
        // epsilon = D1MACH(4)
        // Machine epsilon is the smallest positive epsilon such that
        // (1.0 + epsilon) != 1.0.
        // epsilon = 2**(1 - doubleDigits) = 2**(1 - 53) = 2**(-52)
        // epsilon = 2.224460e-16
        // epsilon is called the largest relative spacing
        epsilon = 1.0;
        neweps = 1.0;

        while (true) {

            if (1.0 == (1.0 + neweps)) {
                break;
            } else {
                epsilon = neweps;
                neweps = neweps / 2.0;
            }
        }

        tol = Math.max(epsilon, 1.0e-18);

        // emin, the smallest exponent E for double precision, is I1MACH(15)
        // tiny = D1MACH(1) = 2**(emin - 1) = 2**(-1022)
        // D1MACH(1) is the smallest normalized number, which preserves the
        // full precision of the mantissa.
        // Double.MIN_VALUE = 2**(-1074) is the smallest denormalized number,
        // which preseerves only a portion of the fraction's precision.
        emin = -1021;
        tiny = Math.pow(2, -1022);

        // emax, the largest exponent E for double precision, is I1MACH(16)
        // D1MACH(2) = 2**(emax)*(1 - 2**(-doubleDigits)) = 2**1024*(1 - 2**-53)
        // D1MACH(2) = Double.MAX_VALUE
        emax = 1024;

        // r1m5 = log10(2), which is D1MACH(5)
        r1m5 = 0.434294481903251 * Math.log(2.0);
        k = Math.min(Math.abs(emin), Math.abs(emax));
        elim = 2.303 * ( (k * r1m5) - 3.0);

        // doubleDigits is I1MACH(14), the number of base-B digits for double
        // precision. IEEE 754 double precision has 64 bits with 1 sign bit,
        // 11 exponent bits, and 52 fraction, mantissa, or signficand bits.
        // Floating-point numbers are typically stored in normalized form,
        // with the radix point after the first non-zero digit. In base two,
        // the only possible non-zero digit is 1. Thus, a leading digit of 1
        // can always be assumed and need not be explicitly represented.
        // Therefore, the mantissa effectively has 53 bits of resolution.
        doubleDigits = 53;
        k1 = doubleDigits - 1;
        aa = r1m5 * k1;
        dig = Math.min(aa, 18.0);
        aa = 2.303 * aa;
        alim = elim + Math.max( -aa, -41.45);
        rl = (1.2 * dig) + 3.0;
        fnul = 10.0 + (6.0 * (dig - 3.0));
        slak = 3.0 + (4.0 * ( ( -0.434294481903251 * Math.log(tol)) - 7.0) / 11.0);
        slak = Math.max(slak, 3.0);
        ertol = tol * Math.pow(10.0, slak);
        rm = 0.5 * (alim + elim);
        rm = Math.min(rm, 200.0);
        rm = Math.max(rm, rl + 10.0);
        r2 = Math.min(rm, fnul);
        Preferences.debug("Quick check routine for the Y Bessel function from ZBESY\n", Preferences.DEBUG_ALGORITHM);
        Preferences.debug("tol = " + tol + "\n", Preferences.DEBUG_ALGORITHM);
        Preferences.debug("elim = " + elim + "\n", Preferences.DEBUG_ALGORITHM);
        Preferences.debug("alim = " + alim + "\n", Preferences.DEBUG_ALGORITHM);
        Preferences.debug("rl = " + rl + "\n", Preferences.DEBUG_ALGORITHM);
        Preferences.debug("fnul = " + fnul + "\n", Preferences.DEBUG_ALGORITHM);
        Preferences.debug("dig = " + dig + "\n", Preferences.DEBUG_ALGORITHM);
        atol = 100.0 * tol;
        Preferences.debug("Checks in the (z,fnu) space with mqc = " + mqc + "\n", Preferences.DEBUG_ALGORITHM);

        /**
         * TEST VALUES OF Z IN -PI/2.LT.ARG(Z).LE.PI NEAR FORMULA BOUNDARIES
         * C-----------------------------------------------------------------------
         * C----------------------------------------------------------------------- C KDO(K), K=1,IL DETERMINES WHICH OF
         * THE IL ANGLES IN -PI TO PI C ARE USE TO COMPUTE VALUES OF Z C KDO(K) = 0 MEANS THAT THE INDEX K WILL BE USED
         * FOR ONE OR TWO C VALUES OF Z, DEPENDING ON THE CHOICE OF KEPS(K) C = 1 MEANS THAT THE INDEX K AND THE
         * CORRESPONDING ANGLE C WILL BE SKIPPED C KEPS(K), K=1,IL DETERMINES WHICH OF THE ANGLES GET INCREMENTED C UP
         * AND DOWN TO PUT VALUES OF Z IN REGIONS WHERE DIFFERENT C FORMULAE ARE USED. C KEPS(K) =0 MEANS THAT THE ANGLE
         * WILL BE USED WITHOUT CHANGE C =1 MEANS THAT THE ANGLE WILL BE INCREMENTED UP AND C DOWN BY EPSC THE ANGLES TO
         * BE USED ARE STORED IN THE T(I) ARRAY, I=1,ITL
         */
        if (mqc != 2) {
            nL = 2;
            il = 5;

            for (i = 1; i <= il; i++) {
                keps[i - 1] = 0;
                kdo[i - 1] = 0;
            } // for (i = 1; i <= il; i++)

            kdo[4] = 1;
            nul = 5;
            xnu[0] = 0.0;
            xnu[1] = 1.0;
            xnu[2] = 2.0;
            xnu[3] = 0.5 * fnul;
            xnu[4] = fnul + 1.2;
        } // if (mqc != 2)
        else { // mqc == 2
            nL = 4;
            il = 13;

            for (i = 1; i <= il; i++) {
                kdo[i - 1] = 0;
                keps[i - 1] = 0;
            } // for (i = 1; i <= il; i++)

            kdo[1] = 1;
            kdo[5] = 1;
            kdo[7] = 1;
            kdo[10] = 1;
            kdo[11] = 1;
            kdo[12] = 1;
            keps[2] = 1;
            keps[3] = 1;
            keps[4] = 1;
            keps[8] = 1;
            nul = 6;
            xnu[0] = 0.0;
            xnu[1] = 0.6;
            xnu[2] = 1.3;
            xnu[3] = 2.0;
            xnu[4] = 0.5 * fnul;
            xnu[5] = fnul + 1.2;
        } // else mqc == 2

        i = 2;
        eps = 0.01;
        film = il - 1.0;
        t[0] = -Math.PI + eps;

        for (k = 2; k <= il; k++) {

            if (kdo[k - 1] == 0) {
                t[i - 1] = Math.PI * ( -il + (2.0 * k) - 1.0) / film;

                if (keps[k - 1] != 0) {
                    ts = t[i - 1];
                    t[i - 1] = ts - eps;
                    i = i + 1;
                    t[i - 1] = ts + eps;
                } // if (keps[k-1] != 0)

                i = i + 1;
            } // if (kdo[k-1] == 0)
        } // for (k = 2; k <= il; k++)

        itl = i - 1;
        lflg = 0;

        for (kode = 1; kode <= 2; kode++) {

            for (n = 1; n <= nL; n++) {

                for (nu = 1; nu <= nul; nu++) {
                    fnu = xnu[nu - 1];
                    ifnu = (int) (fnu);
                    ffnu = fnu - ifnu;

                    group: for (icase = 1; icase <= 3; icase++) {
                        irb = Math.min(2, icase);

                        for (ir = irb; ir <= 4; ir++) {

                            if (icase == 1) {
                                r = ( (eps * (4.0 - ir)) + (2.0 * (ir - 1.0))) / 3.0;
                            } // if (icase == 1)
                            else if (icase == 2) {
                                r = ( (2.0 * (4.0 - ir)) + (r2 * (ir - 1.0))) / 3.0;
                            } // else if (icase == 2)
                            else if ( (icase == 3) && (rm == r2)) {
                                continue group;
                            } // else if ((icase == 3) && (rm == r2))
                            else {
                                r = ( (r2 * (4.0 - ir)) + (rm * (ir - 1.0))) / 3.0;
                            } // else

                            for (it = 1; it <= itl; it++) {
                                ct = Math.cos(t[it - 1]);
                                st = Math.sin(t[it - 1]);

                                if (Math.abs(ct) < atol) {
                                    ct = 0.0;
                                } // if (Math.abs(ct) < atol)

                                if (Math.abs(st) < atol) {
                                    st = 0.0;
                                } // if (Math.abs(st) < atol)

                                zr = r * ct;
                                zi = r * st;
                                zbesy(zr, zi, fnu, kode, n, vr, vi, nz2, cwrkr, cwrki, ierr);

                                if ( (nz2[0] != 0) || (ierr[0] != 0)) {
                                    continue;
                                } // if ((nz2[0] != 0) || (ierr[0] != 0))

                                zbesyh(zr, zi, fnu, kode, n, wr, wi, nz1, cwrkr, cwrki, ierr);

                                if ( (nz1[0] != 0) || (ierr[0] != 0)) {
                                    continue;
                                } // if ((nz1[0] != 0) || (ierr[0] != 0))

                                mflg = 0;

                                for (i = 1; i <= n; i++) {
                                    ab = fnu + i - 1.0;
                                    aa = Math.max(0.5, ab);
                                    cwr = wr[i - 1] - vr[i - 1];
                                    cwi = wi[i - 1] - vi[i - 1];
                                    av = zabs(vr[i - 1], vi[i - 1]);
                                    er = zabs(cwr, cwi);

                                    if (av != 0.0) {

                                        if (zi == 0.0) {

                                            if (zr > 0.0) {

                                                if (Math.abs(zr) < aa) {
                                                    er = er / av;
                                                } // if (Math.abs(zr) < zz)
                                            } // if (zr > 0.0)
                                            else { // zr <= 0.0

                                                if (Math.abs(ffnu - 0.5) < 0.125) {

                                                    if (Math.abs(zr) < aa) {
                                                        er = er / av;
                                                    } // if Math.abs(zr) < aa)
                                                } // if (Math.abs(ffnu - 0.5) < 0.125)
                                                else { // Math.abs(ffnu - 0.5) >= 0.125) {
                                                    er = er / av;
                                                } // else Math.abs(ffnu - 0.5) >= 0.125)
                                            } // else zr <= 0.0
                                        } // if (zi == 0.0)
                                        else { // (zi != 0.0)
                                            er = er / av;
                                        } // else (zi != 0.0)
                                    } // if (av != 0.0)

                                    aer[i - 1] = er;

                                    if (er > ertol) {
                                        mflg = 1;
                                    } // if (er > ertol)
                                } // for (i = 1; i <= n; i++)

                                if (mflg == 0) {
                                    continue;
                                } // if (mflg == 0)

                                if (lflg != 1) {
                                    Preferences.debug("Cases which violate relative error test " + "with ertol = "
                                            + ertol + "\n", Preferences.DEBUG_ALGORITHM);
                                    Preferences.debug("OUTPUT FORMAT kode,n,ir,it,nz1,nz2,icase\n", 
                                    		Preferences.DEBUG_ALGORITHM);
                                    Preferences.debug("aer[k-1], k = 1,n z,fnu,w[kk-1],v[kk-1]\n", 
                                    		Preferences.DEBUG_ALGORITHM);
                                    Preferences.debug("kk = index of first non-zero w,v pair\n", Preferences.DEBUG_ALGORITHM);
                                    lflg = 1;
                                } // if (lflg != 1)

                                kk = Math.max(nz1[0], nz2[0]) + 1;
                                kk = Math.min(n, kk);
                                Preferences.debug("kode = " + kode + "\n", Preferences.DEBUG_ALGORITHM);
                                Preferences.debug("n = " + n + "\n", Preferences.DEBUG_ALGORITHM);
                                Preferences.debug("ir = " + ir + "\n", Preferences.DEBUG_ALGORITHM);
                                Preferences.debug("it = " + it + "\n", Preferences.DEBUG_ALGORITHM);
                                Preferences.debug("nz1 = " + nz1[0] + "\n", Preferences.DEBUG_ALGORITHM);
                                Preferences.debug("nz2 = " + nz2[0] + "\n", Preferences.DEBUG_ALGORITHM);
                                Preferences.debug("icase = " + icase + "\n", Preferences.DEBUG_ALGORITHM);

                                for (k = 1; k <= n; k++) {
                                    Preferences.debug("aer[" + (k - 1) + "] = " + aer[k - 1] + "\n", 
                                    		Preferences.DEBUG_ALGORITHM);
                                } // for (k = 1; k <= n; k++)

                                Preferences.debug("zr = " + zr + "\n", Preferences.DEBUG_ALGORITHM);
                                Preferences.debug("zi = " + zi + "\n", Preferences.DEBUG_ALGORITHM);
                                Preferences.debug("fnu = " + fnu + "\n", Preferences.DEBUG_ALGORITHM);
                                Preferences.debug("wr[" + (kk - 1) + "] = " + wr[kk - 1] + "\n", Preferences.DEBUG_ALGORITHM);
                                Preferences.debug("wi[" + (kk - 1) + "] = " + wi[kk - 1] + "\n", Preferences.DEBUG_ALGORITHM);
                                Preferences.debug("vr[" + (kk - 1) + "] = " + vr[kk - 1] + "\n", Preferences.DEBUG_ALGORITHM);
                                Preferences.debug("vi[" + (kk - 1) + "] = " + vi[kk - 1] + "\n", Preferences.DEBUG_ALGORITHM);
                            } // for (it = 1; it <= itl; it++)
                        } // for (ir = irb; ir <= 4; ir++)
                    } // for (icase = 1; icase <= 3; icase++)
                } // for (nu = 1; nu <= nul; nu++)
            } // for (n = 1; n <= nL; n++)
        } // for (kode = 1; kode <= 2; kode++)

        if (lflg == 0) {
            Preferences.debug("Y QUICK CHECKS OK\n", Preferences.DEBUG_ALGORITHM);
        } // if (lflg == 0)

        return;
    }

    /**
     * ZRATI COMPUTES RATIOS OF I BESSEL FUNCTIONS BY BACKWARD RECURRENCE. THE STARTING INDEX IS DETERMINED BY FORWARD
     * RECURRENCE AS DESCRIBED IN J. RES. OF NAT. BUR. OF STANDARDS-B, MATHEMATICAL SCIENCES, VOL 77B, P111-114,
     * SEPTEMBER, 1973, BESSEL FUNCTIONS I AND J OF COMPLEX ARGUMENT AND INTEGER ORDER, BY D. J. SOOKNE.
     * 
     * @param zr double
     * @param zi double
     * @param fnu double
     * @param n int
     * @param cyr double[]
     * @param cyi double[]
     */
    private void zrati(final double zr, final double zi, final double fnu, final int n, final double[] cyr,
            final double[] cyi) {
        final double rt2 = Math.sqrt(2.0);
        double az;
        int inu;
        int idnu;
        int magz;
        double amagz;
        double fdnu;
        double fnup;
        int id;
        int itime;
        int k;
        double ptr;
        double rzr;
        double rzi;
        double t1r;
        double t1i;
        double p2r;
        double p2i;
        double p1r;
        double p1i;
        double ap2;
        double ap1;
        double arg;
        double test1;
        double test;
        double rap1;
        double ak;
        double flam;
        double rho;
        int kk;
        double dfnu;
        double pti;
        double ttr;
        double tti;
        final double[] cyrnm1 = new double[1];
        final double[] cyinm1 = new double[1];
        double cdfnur;
        double cdfnui;
        double rak;
        int i;

        az = zabs(zr, zi);
        inu = (int) (fnu);
        idnu = inu + n - 1;
        magz = (int) (az);
        amagz = magz + 1.0;
        fdnu = idnu;
        fnup = Math.max(amagz, fdnu);
        id = idnu - magz - 1;
        itime = 1;
        k = 1;
        ptr = 1.0 / az;
        rzr = ptr * (zr + zr) * ptr;
        rzi = -ptr * (zi + zi) * ptr;
        t1r = rzr * fnup;
        t1i = rzi * fnup;
        p2r = -t1r;
        p2i = -t1i;
        p1r = 1.0;
        p1i = 0.0;
        t1r = t1r + rzr;
        t1i = t1i + rzi;

        if (id > 0) {
            id = 0;
        } // if (id > 0)

        ap2 = zabs(p2r, p2i);
        ap1 = zabs(p1r, p1i);

        // The overflow test on K(fnu+i-1,z) before the call to cbknu
        // guarantees that p2 is on scale. Scale test1 and all subsequent
        // p2 values by ap1 to ensure that an overflow does not occur
        // prematurely.
        arg = (ap2 + ap2) / (ap1 * tol);
        test1 = Math.sqrt(arg);
        test = test1;
        rap1 = 1.0 / ap1;
        p1r = p1r * rap1;
        p1i = p1i * rap1;
        p2r = p2r * rap1;
        p2i = p2i * rap1;
        ap2 = ap2 * rap1;

        while (true) {
            k = k + 1;
            ap1 = ap2;
            ptr = p2r;
            pti = p2i;
            p2r = p1r - ( (t1r * ptr) - (t1i * pti));
            p2i = p1i - ( (t1r * pti) + (t1i * ptr));
            p1r = ptr;
            p1i = pti;
            t1r = t1r + rzr;
            t1i = t1i + rzi;
            ap2 = zabs(p2r, p2i);

            if (ap1 <= test) {
                continue;
            } // if (ap1 <= test)

            if (itime == 2) {
                break;
            } // if (itime == 2)

            ak = 0.5 * zabs(t1r, t1i);
            flam = ak + Math.sqrt( (ak * ak) - 1.0);
            rho = Math.min(ap2 / ap1, flam);
            test = test1 * Math.sqrt(rho / ( (rho * rho) - 1.0));
            itime = 2;
        } // while (true)

        kk = k + 1 - id;
        ak = kk;
        t1r = ak;
        t1i = 0.0;
        dfnu = fnu + n - 1.0;
        p1r = 1.0 / ap2;
        p1i = 0.0;
        p2r = 0.0;
        p2i = 0.0;

        for (i = 1; i <= kk; i++) {
            ptr = p1r;
            pti = p1i;
            rap1 = dfnu + t1r;
            ttr = rzr * rap1;
            tti = rzi * rap1;
            p1r = ( (ptr * ttr) - (pti * tti)) + p2r;
            p1i = ( (ptr * tti) + (pti * ttr)) + p2i;
            p2r = ptr;
            p2i = pti;
            t1r = t1r - 1.0;
        } // for ( i = 1; i <= kk; i++)

        if ( (p1r == 0.0) && (p1i == 0.0)) {
            p1r = tol;
            p1i = tol;
        } // if ((p1r == 0.0) && (p1i == 0.0))

        zdiv(p2r, p2i, p1r, p1i, cyrnm1, cyinm1);
        cyr[n - 1] = cyrnm1[0];
        cyi[n - 1] = cyinm1[0];

        if (n == 1) {
            return;
        } // if (n == 1)

        k = n - 1;
        ak = k;
        t1r = ak;
        t1i = 0.0;
        cdfnur = fnu * rzr;
        cdfnui = fnu * rzi;

        for (i = 2; i <= n; i++) {
            ptr = cdfnur + ( (t1r * rzr) - (t1i * rzi)) + cyr[k];
            pti = cdfnui + ( (t1r * rzi) + (t1i * rzr)) + cyi[k];
            ak = zabs(ptr, pti);

            if (ak == 0.0) {
                ptr = tol;
                pti = tol;
                ak = tol * rt2;
            } // if (ak == 0.0)

            rak = 1.0 / ak;
            cyr[k - 1] = rak * ptr * rak;
            cyi[k - 1] = -rak * pti * rak;
            t1r = t1r - 1.0;
            k = k - 1;
        } // for (i = 2; i <= n; i++)

        return;
    }

    /**
     * ZS1S2 TESTS FOR A POSSIBLE UNDERFLOW RESULTING FROM THE ADDITION OF THE I AND K FUNCTIONS IN THE ANALYTIC CON-
     * TINUATION FORMULA WHERE S1=K FUNCTION AND S2=I FUNCTION. ON KODE=1 THE I AND K FUNCTIONS ARE DIFFERENT ORDERS OF
     * MAGNITUDE, BUT FOR KODE=2 THEY CAN BE OF THE SAME ORDER OF MAGNITUDE AND THE MAXIMUM MUST BE AT LEAST ONE
     * PRECISION ABOVE THE UNDERFLOW LIMIT.
     * 
     * @param zrr double
     * @param zri double
     * @param s1r double[]
     * @param s1i double[]
     * @param s2r double[]
     * @param s2i double[]
     * @param nz int[]
     * @param ascle double
     * @param iuf int[]
     */
    private void zs1s2(double zrr, final double zri, final double[] s1r, final double[] s1i, final double[] s2r,
            final double[] s2i, final int[] nz, final double ascle, final int[] iuf) {
        double as1;
        double as2;
        double aln;
        double s1dr;
        double s1di;
        final double[] c1r = new double[1];
        final double[] c1i = new double[1];
        final int[] idum = new int[1];
        double aa;

        nz[0] = 0;
        as1 = zabs(s1r[0], s1i[0]);
        as2 = zabs(s2r[0], s2i[0]);

        if ( ( (s1r[0] != 0.0) || (s1i[0] != 0.0)) && (as1 != 0.0)) {
            aln = -zrr - zrr + Math.log(as1);
            s1dr = s1r[0];
            s1di = s1i[0];
            s1r[0] = 0.0;
            s1i[0] = 0.0;
            as1 = 0.0;

            if (aln >= ( -alim)) {
                zlog(s1dr, s1di, c1r, c1i, idum);
                c1r[0] = c1r[0] - zrr - zrr;
                c1i[0] = c1i[0] - zri - zri;
                zexp(c1r[0], c1i[0], s1r, s1i);
                as1 = zabs(s1r[0], s1i[0]);
                iuf[0] = iuf[0] + 1;
            } // if (aln >= (-alim))
        } // (((s1r[0] != 0.0) || (s1i[0] != 0.0)) && (as1 != 0.0))

        aa = Math.max(as1, as2);

        if (aa > ascle) {
            return;
        } // if (aa > ascle)

        s1r[0] = 0.0;
        s1i[0] = 0.0;
        s2r[0] = 0.0;
        s2i[0] = 0.0;
        nz[0] = 1;
        iuf[0] = 0;

        return;
    }

    /**
     * ZSERI COMPUTES THE I BESSEL FUNCTION FOR REAL(Z).GE.0.0 BY MEANS OF THE POWER SERIES FOR LARGE CABS(Z) IN THE
     * REGION CABS(Z).LE.2*SQRT(fnu+1). NZ=0 IS A NORMAL RETURN. NZ.GT.0 MEANS THAT THE LAST NZ COMPONENTS WERE SET TO
     * ZERO DUE TO UNDERFLOW. NZ.LT.0 MEANS UNDERFLOW OCCURRED, BUT THE CONDITION CABS(Z).LE.2*SQRT(fnu+1) WAS VIOLATED
     * AND THE COMPUTATION MUST BE COMPLETED IN ANOTHER ROUTINE WITH N=N-ABS(NZ).
     * 
     * @param zr double
     * @param zi double
     * @param fnu double
     * @param kode int
     * @param n int
     * @param yr double[]
     * @param yi double[]
     * @param nz int[]
     */
    private void zseri(final double zr, double zi, final double fnu, final int kode, final int n, final double[] yr,
            final double[] yi, final int[] nz) {
        double az;
        int i;
        double arm;
        double rtr1;
        double crscr;
        int iflag;
        double hzr;
        double hzi;
        final double[] czr = new double[1];
        final double[] czi = new double[1];
        double acz;
        int nn;
        final double[] ckr = new double[1];
        final double[] cki = new double[1];
        final int[] idum = new int[1];
        double dfnu = 0.0;
        double fnup = 1.0;
        double ak1r = 0.0;
        double ak1i = 0.0;
        double ak = 0.0;
        boolean seg1 = true;
        boolean seg2 = true;
        boolean seg3 = true;
        boolean seg4 = true;
        boolean seg5 = true;
        double ss = 1.0;
        double ascle = 0.0;
        double aa;
        double coefr;
        double coefi;
        double atol;
        int il;
        double s1r = 0.0;
        double s1i = 0.0;
        double s = 1.0;
        double rs;
        final double[] str = new double[1];
        final double[] sti = new double[1];
        final double[] wr = new double[2];
        final double[] wi = new double[2];
        int m;
        int k;
        double raz;
        double rzr;
        double rzi;
        int ib = 0;
        double s2r;
        double s2i;
        final int[] nw = new int[1];
        int l;

        nz[0] = 0;
        az = zabs(zr, zi);

        if (az == 0.0) {
            yr[0] = 0.0;
            yi[0] = 0.0;

            if (fnu == 0.0) {
                yr[0] = 1.0;
            }

            if (n == 1) {
                return;
            }

            for (i = 1; i < n; i++) {
                yr[i] = 0.0;
                yi[i] = 0.0;
            }

            return;
        } // if (az == 0.0)

        arm = 1.0E3 * tiny;
        rtr1 = Math.sqrt(arm);
        crscr = 1.0;
        iflag = 0;

        if (az < arm) {
            nz[0] = n;

            if (fnu == 0.0) {
                nz[0] = nz[0] - 1;
            }

            yr[0] = 0.0;
            yi[0] = 0.0;

            if (fnu == 0.0) {
                yr[0] = 1.0;
            }

            if (n == 1) {
                return;
            }

            for (i = 1; i < n; i++) {
                yr[i] = 0.0;
                yi[i] = 0.0;
            }

            return;
        } // if (az < arm)

        hzr = 0.5 * zr;
        hzi = 0.5 * zi;
        czr[0] = 0.0;
        czi[0] = 0.0;

        if (az > rtr1) {
            zmlt(hzr, hzi, hzr, hzi, czr, czi);
        }

        acz = zabs(czr[0], czi[0]);
        nn = n;
        zlog(hzr, hzi, ckr, cki, idum);

        loop: while (true) {

            if (seg1) {
                dfnu = fnu + nn - 1.0;
                fnup = dfnu + 1.0;

                // underflow test
                ak1r = ckr[0] * dfnu;
                ak1i = cki[0] * dfnu;
                ak = dgamln(fnup, idum);
                ak1r = ak1r - ak;

                if (kode == 2) {
                    ak1r = ak1r - zr;
                }

                if (ak1r > ( -elim)) {
                    seg2 = false;
                }
            } // if (seg1)

            seg1 = true;

            if (seg2) {
                nz[0] = nz[0] + 1;
                yr[nn - 1] = 0.0;
                yi[nn - 1] = 0.0;

                if (acz > dfnu) {
                    nz[0] = -nz[0];

                    return;
                }

                nn = nn - 1;

                if (nn == 0) {
                    return;
                }

                continue;
            } // if (seg2)

            seg2 = true;

            if (ak1r <= ( -alim)) {
                iflag = 1;
                ss = 1.0 / tol;
                crscr = tol;
                ascle = arm * ss;
            } // if (ak1r <= (-alim))

            aa = Math.exp(ak1r);

            if (iflag == 1) {
                aa = aa * ss;
            }

            coefr = aa * Math.cos(ak1i);
            coefi = aa * Math.sin(ak1i);
            atol = tol * acz / fnup;
            il = Math.min(2, nn);

            for (i = 1; i <= il; i++) {
                dfnu = fnu + nn - i;
                fnup = dfnu + 1.0;
                s1r = 1.0;
                s1i = 0.0;

                if (acz < (tol * fnup)) {
                    seg3 = false;
                    seg4 = false;
                }

                if (seg3) {
                    ak1r = 1.0;
                    ak1i = 0.0;
                    ak = fnup + 2.0;
                    s = fnup;
                    aa = 2.0;
                } // if (seg3)

                seg3 = true;

                if (seg4) {

                    do {
                        rs = 1.0 / s;
                        str[0] = (ak1r * czr[0]) - (ak1i * czi[0]);
                        sti[0] = (ak1r * czi[0]) + (ak1i * czr[0]);
                        ak1r = str[0] * rs;
                        ak1i = sti[0] * rs;
                        s1r = s1r + ak1r;
                        s1i = s1i + ak1i;
                        s = s + ak;
                        ak = ak + 2.0;
                        aa = aa * acz * rs;
                    } while (aa > atol);
                } // if (seg4)

                seg4 = true;
                s2r = (s1r * coefr) - (s1i * coefi);
                s2i = (s1r * coefi) + (s1i * coefr);
                wr[i - 1] = s2r;
                wi[i - 1] = s2i;

                if (iflag != 0) {
                    zuchk(s2r, s2i, nw, ascle);

                    if (nw[0] != 0) {
                        seg1 = false;

                        continue loop;
                    }
                } // if (iflag != 0)

                m = nn - i + 1;
                yr[m - 1] = s2r * crscr;
                yi[m - 1] = s2i * crscr;

                if (i != il) {
                    zdiv(coefr, coefi, hzr, hzi, str, sti);
                    coefr = str[0] * dfnu;
                    coefi = sti[0] * dfnu;
                } // if (i != il)
            } // for (i = 1; i <= il; i++)

            break loop;
        } // while (true)

        if (nn <= 2) {
            return;
        }

        k = nn - 2;
        ak = k;
        raz = 1.0 / az;
        str[0] = zr * raz;
        sti[0] = -zi * raz;
        rzr = 2.0 * str[0] * raz;
        rzi = 2.0 * sti[0] * raz;

        if (iflag == 1) {
            seg5 = false;
        } // if (iflag == 1)

        if (seg5) {
            ib = 3;

            for (i = ib; i <= nn; i++) {
                yr[k - 1] = ( (ak + fnu) * ( (rzr * yr[k]) - (rzi * yi[k]))) + yr[k + 1];
                yi[k - 1] = ( (ak + fnu) * ( (rzr * yi[k]) + (rzi * yr[k]))) + yi[k + 1];
                ak = ak - 1.0;
                k = k - 1;
            } // for (i = ib; i <= nn; i++)

            return;
        } // if (seg5)

        seg5 = true;

        // Recur backward with scaled values
        // exp(-alim) = exp(-elim)/tol = approximately one precision above the
        // underflow limit = ascle = D1MACH(1) * ss * 1.0E+3
        s1r = wr[0];
        s1i = wi[0];
        s2r = wr[1];
        s2i = wi[1];

        group: {

            for (l = 3; l <= nn; l++) {
                ckr[0] = s2r;
                cki[0] = s2i;
                s2r = s1r + ( (ak + fnu) * ( (rzr * ckr[0]) - (rzi * cki[0])));
                s2i = s1i + ( (ak + fnu) * ( (rzr * cki[0]) + (rzi * ckr[0])));
                s1r = ckr[0];
                s1i = cki[0];
                ckr[0] = s2r * crscr;
                cki[0] = s2i * crscr;
                yr[k - 1] = ckr[0];
                yi[k - 1] = cki[0];
                ak = ak - 1.0;
                k = k - 1;

                if (zabs(ckr[0], cki[0]) > ascle) {
                    break group;
                }
            } // for (l = 3; l <= nn; l++)

            return;
        } // group

        ib = l + 1;

        if (ib > nn) {
            return;
        }

        for (i = ib; i <= nn; i++) {
            yr[k - 1] = ( (ak + fnu) * ( (rzr * yr[k]) - (rzi * yi[k]))) + yr[k + 1];
            yi[k - 1] = ( (ak + fnu) * ( (rzr * yi[k]) + (rzi * yr[k]))) + yi[k + 1];
            ak = ak - 1.0;
            k = k - 1;
        } // for (i = ib; i <= nn; i++)

        return;
    }

    /**
     * zshch computes the complex hyperbolic functions csh = sinh(x+i*y) and cch = cosh(x+i*y).
     * 
     * @param zr double
     * @param zi double
     * @param cshr double[]
     * @param cshi double[]
     * @param cchr double[]
     * @param cchi double[]
     */
    private void zshch(double zr, final double zi, final double[] cshr, final double[] cshi, final double[] cchr,
            final double[] cchi) {
        double pexp;
        double mexp;
        double sh;
        double ch;
        double sn;
        double cn;

        pexp = Math.exp(zr);
        mexp = Math.exp( -zr);
        sh = 0.5 * (pexp - mexp);
        ch = 0.5 * (pexp + mexp);
        sn = Math.sin(zi);
        cn = Math.cos(zi);
        cshr[0] = sh * cn;
        cshi[0] = ch * sn;
        cchr[0] = ch * cn;
        cchi[0] = sh * sn;

        return;
    }

    /**
     * complex square root b = csqrt(a).
     * 
     * @param ar double
     * @param ai double
     * @param br double[]
     * @param bi double[]
     */
    private void zsqrt(final double ar, final double ai, final double[] br, final double[] bi) {
        final double drt = 1.0 / Math.sqrt(2.0);
        double zm;
        double theta;

        zm = zabs(ar, ai);
        zm = Math.sqrt(zm);

        if (ar == 0.0) {

            if (ai == 0.0) {
                br[0] = 0.0;
                bi[0] = 0.0;

                return;
            } // if (ai == 0.0)
            else if (ai > 0.0) {
                br[0] = zm * drt;
                bi[0] = zm * drt;

                return;
            } // else if (ai > 0.0)
            else { // ai < 0.0
                br[0] = zm * drt;
                bi[0] = -zm * drt;

                return;
            } // else ai < 0.0
        } // if (ar == 0.0)
        else if (ai == 0.0) {

            if (ar > 0.0) {
                br[0] = Math.sqrt(ar);
                bi[0] = 0.0;

                return;
            } // if (ar > 0.0)
            else { // ar < 0.0
                br[0] = 0.0;
                bi[0] = Math.sqrt(Math.abs(ar));

                return;
            } // ar < 0.0
        } // else if (ai == 0.0)

        theta = Math.atan(ai / ar);

        if (theta <= 0.0) {

            if (ar < 0.0) {
                theta = theta + Math.PI;
            }
        } else if (ar < 0.0) {
            theta = theta - Math.PI;
        }

        theta = 0.5 * theta;
        br[0] = zm * Math.cos(theta);
        bi[0] = zm * Math.sin(theta);

        return;
    }

    /**
     * Y ENTERS AS A SCALED QUANTITY WHOSE MAGNITUDE IS GREATER THAN EXP(-ALIM)=ASCLE=1.0E+3*D1MACH(1)/TOL. THE TEST IS
     * MADE TO SEE IF THE MAGNITUDE OF THE REAL OR IMAGINARY PART WOULD UNDERFLOW WHEN Y IS SCALED (BY TOL) TO ITS
     * PROPER VALUE. Y IS ACCEPTED IF THE UNDERFLOW IS AT LEAST ONE PRECISION BELOW THE MAGNITUDE OF THE LARGEST
     * COMPONENT; OTHERWISE THE PHASE ANGLE DOES NOT HAVE ABSOLUTE ACCURACY AND AN UNDERFLOW IS ASSUMED.
     * 
     * @param yr double
     * @param yi double
     * @param nz int[]
     * @param ascle double
     */
    private void zuchk(final double yr, final double yi, final int[] nz, final double ascle) {
        double wr, wi, ss, st;
        nz[0] = 0;
        wr = Math.abs(yr);
        wi = Math.abs(yi);
        st = Math.min(wr, wi);

        if (st > ascle) {
            return;
        }

        ss = Math.max(wr, wi);
        st = st / tol;

        if (ss < st) {
            nz[0] = 1;
        }

        return;
    }

    /**
     * REFERENCES HANDBOOK OF MATHEMATICAL FUNCTIONS BY M. ABRAMOWITZ AND I.A. STEGUN, AMS55, NATIONAL BUREAU OF
     * STANDARDS, 1965, CHAPTER 9.
     * 
     * <p>
     * ASYMPTOTICS AND SPECIAL FUNCTIONS BY F.W.J. OLVER, ACADEMIC PRESS, N.Y., 1974, PAGE 420
     * </p>
     * 
     * <p>
     * ABSTRACT ZUNHJ COMPUTES PARAMETERS FOR BESSEL FUNCTIONS C(FNU,Z) = J(FNU,Z), Y(FNU,Z) OR H(I,FNU,Z) I=1,2 FOR
     * LARGE ORDERS FNU BY MEANS OF THE UNIFORM ASYMPTOTIC EXPANSION
     * </p>
     * 
     * <p>
     * C(FNU,Z)=C1*PHI*( ASUM*AIRY(ARG) + C2*BSUM*DAIRY(ARG) )
     * </p>
     * 
     * <p>
     * FOR PROPER CHOICES OF C1, C2, AIRY AND DAIRY WHERE AIRY IS AN AIRY FUNCTION AND DAIRY IS ITS DERIVATIVE.
     * </p>
     * 
     * <p>
     * (2/3)*FNU*ZETA**1.5 = ZETA1-ZETA2,
     * </p>
     * 
     * <p>
     * ZETA1=0.5*FNU*CLOG((1+W)/(1-W)), ZETA2=FNU*W FOR SCALING PURPOSES IN AIRY FUNCTIONS FROM CAIRY OR CBIRY.
     * </p>
     * 
     * <p>
     * MCONJ=SIGN OF AIMAG(Z), BUT IS AMBIGUOUS WHEN Z IS REAL AND MUST BE SPECIFIED. IPMTR=0 RETURNS ALL PARAMETERS.
     * IPMTR=1 COMPUTES ALL EXCEPT ASUM AND BSUM.
     * </p>
     * 
     * @param zr double
     * @param zi double
     * @param fnu double
     * @param ipmtr int
     * @param phir double[]
     * @param phii double[]
     * @param argr double[]
     * @param argi double[]
     * @param zeta1r double[]
     * @param zeta1i double[]
     * @param zeta2r double[]
     * @param zeta2i double[]
     * @param asumr double[]
     * @param asumi double[]
     * @param bsumr double[]
     * @param bsumi double[]
     */
    private void zunhj(final double zr, final double zi, final double fnu, final int ipmtr, final double[] phir,
            final double[] phii, final double[] argr, final double[] argi, final double[] zeta1r,
            final double[] zeta1i, final double[] zeta2r, final double[] zeta2i, final double[] asumr,
            final double[] asumi, final double[] bsumr, final double[] bsumi) {
        final double[] ar = new double[] {1.00000000000000000E+00, 1.04166666666666667E-01, 8.35503472222222222E-02,
                1.28226574556327160E-01, 2.91849026464140464E-01, 8.81627267443757652E-01, 3.32140828186276754E+00,
                1.49957629868625547E+01, 7.89230130115865181E+01, 4.74451538868264323E+02, 3.20749009089066193E+03,
                2.40865496408740049E+04, 1.98923119169509794E+05, 1.79190200777534383E+06};
        final double[] br = new double[] {1.00000000000000000E+00, -1.45833333333333333E-01, -9.87413194444444444E-02,
                -1.43312053915895062E-01, -3.17227202678413548E-01, -9.42429147957120249E-01, -3.51120304082635426E+00,
                -1.57272636203680451E+01, -8.22814390971859444E+01, -4.92355370523670524E+02, -3.31621856854797251E+03,
                -2.48276742452085896E+04, -2.04526587315129788E+05, -1.83844491706820990E+06};
        final double[] c = new double[] {1.00000000000000000E+00, -2.08333333333333333E-01, 1.25000000000000000E-01,
                3.34201388888888889E-01, -4.01041666666666667E-01, 7.03125000000000000E-02, -1.02581259645061728E+00,
                1.84646267361111111E+00, -8.91210937500000000E-01, 7.32421875000000000E-02, 4.66958442342624743E+00,
                -1.12070026162229938E+01, 8.78912353515625000E+00, -2.36408691406250000E+00, 1.12152099609375000E-01,
                -2.82120725582002449E+01, 8.46362176746007346E+01, -9.18182415432400174E+01, 4.25349987453884549E+01,
                -7.36879435947963170E+00, 2.27108001708984375E-01, 2.12570130039217123E+02, -7.65252468141181642E+02,
                1.05999045252799988E+03, -6.99579627376132541E+02, 2.18190511744211590E+02, -2.64914304869515555E+01,
                5.72501420974731445E-01, -1.91945766231840700E+03, 8.06172218173730938E+03, -1.35865500064341374E+04,
                1.16553933368645332E+04, -5.30564697861340311E+03, 1.20090291321635246E+03, -1.08090919788394656E+02,
                1.72772750258445740E+00, 2.02042913309661486E+04, -9.69805983886375135E+04, 1.92547001232531532E+05,
                -2.03400177280415534E+05, 1.22200464983017460E+05, -4.11926549688975513E+04, 7.10951430248936372E+03,
                -4.93915304773088012E+02, 6.07404200127348304E+00, -2.42919187900551333E+05, 1.31176361466297720E+06,
                -2.99801591853810675E+06, 3.76327129765640400E+06, -2.81356322658653411E+06, 1.26836527332162478E+06,
                -3.31645172484563578E+05, 4.52187689813627263E+04, -2.49983048181120962E+03, 2.43805296995560639E+01,
                3.28446985307203782E+06, -1.97068191184322269E+07, 5.09526024926646422E+07, -7.41051482115326577E+07,
                6.63445122747290267E+07, -3.75671766607633513E+07, 1.32887671664218183E+07, -2.78561812808645469E+06,
                3.08186404612662398E+05, -1.38860897537170405E+04, 1.10017140269246738E+02, -4.93292536645099620E+07,
                3.25573074185765749E+08, -9.39462359681578403E+08, 1.55359689957058006E+09, -1.62108055210833708E+09,
                1.10684281682301447E+09, -4.95889784275030309E+08, 1.42062907797533095E+08, -2.44740627257387285E+07,
                2.24376817792244943E+06, -8.40054336030240853E+04, 5.51335896122020586E+02, 8.14789096118312115E+08,
                -5.86648149205184723E+09, 1.86882075092958249E+10, -3.46320433881587779E+10, 4.12801855797539740E+10,
                -3.30265997498007231E+10, 1.79542137311556001E+10, -6.56329379261928433E+09, 1.55927986487925751E+09,
                -2.25105661889415278E+08, 1.73951075539781645E+07, -5.49842327572288687E+05, 3.03809051092238427E+03,
                -1.46792612476956167E+10, 1.14498237732025810E+11, -3.99096175224466498E+11, 8.19218669548577329E+11,
                -1.09837515608122331E+12, 1.00815810686538209E+12, -6.45364869245376503E+11, 2.87900649906150589E+11,
                -8.78670721780232657E+10, 1.76347306068349694E+10, -2.16716498322379509E+09, 1.43157876718888981E+08,
                -3.87183344257261262E+06, 1.82577554742931747E+04};
        final double[] alfa = new double[] { -4.44444444444444444E-03, -9.22077922077922078E-04,
                -8.84892884892884893E-05, 1.65927687832449737E-04, 2.46691372741792910E-04, 2.65995589346254780E-04,
                2.61824297061500945E-04, 2.48730437344655609E-04, 2.32721040083232098E-04, 2.16362485712365082E-04,
                2.00738858762752355E-04, 1.86267636637545172E-04, 1.73060775917876493E-04, 1.61091705929015752E-04,
                1.50274774160908134E-04, 1.40503497391269794E-04, 1.31668816545922806E-04, 1.23667445598253261E-04,
                1.16405271474737902E-04, 1.09798298372713369E-04, 1.03772410422992823E-04, 9.82626078369363448E-05,
                9.32120517249503256E-05, 8.85710852478711718E-05, 8.42963105715700223E-05, 8.03497548407791151E-05,
                7.66981345359207388E-05, 7.33122157481777809E-05, 7.01662625163141333E-05, 6.72375633790160292E-05,
                6.93735541354588974E-04, 2.32241745182921654E-04, -1.41986273556691197E-05, -1.16444931672048640E-04,
                -1.50803558053048762E-04, -1.55121924918096223E-04, -1.46809756646465549E-04, -1.33815503867491367E-04,
                -1.19744975684254051E-04, -1.06184319207974020E-04, -9.37699549891194492E-05, -8.26923045588193274E-05,
                -7.29374348155221211E-05, -6.44042357721016283E-05, -5.69611566009369048E-05, -5.04731044303561628E-05,
                -4.48134868008882786E-05, -3.98688727717598864E-05, -3.55400532972042498E-05, -3.17414256609022480E-05,
                -2.83996793904174811E-05, -2.54522720634870566E-05, -2.28459297164724555E-05, -2.05352753106480604E-05,
                -1.84816217627666085E-05, -1.66519330021393806E-05, -1.50179412980119482E-05, -1.35554031379040526E-05,
                -1.22434746473858131E-05, -1.10641884811308169E-05, -3.54211971457743841E-04, -1.56161263945159416E-04,
                3.04465503594936410E-05, 1.30198655773242693E-04, 1.67471106699712269E-04, 1.70222587683592569E-04,
                1.56501427608594704E-04, 1.36339170977445120E-04, 1.14886692029825128E-04, 9.45869093034688111E-05,
                7.64498419250898258E-05, 6.07570334965197354E-05, 4.74394299290508799E-05, 3.62757512005344297E-05,
                2.69939714979224901E-05, 1.93210938247939253E-05, 1.30056674793963203E-05, 7.82620866744496661E-06,
                3.59257485819351583E-06, 1.44040049814251817E-07, -2.65396769697939116E-06, -4.91346867098485910E-06,
                -6.72739296091248287E-06, -8.17269379678657923E-06, -9.31304715093561232E-06, -1.02011418798016441E-05,
                -1.08805962510592880E-05, -1.13875481509603555E-05, -1.17519675674556414E-05, -1.19987364870944141E-05,
                3.78194199201772914E-04, 2.02471952761816167E-04, -6.37938506318862408E-05, -2.38598230603005903E-04,
                -3.10916256027361568E-04, -3.13680115247576316E-04, -2.78950273791323387E-04, -2.28564082619141374E-04,
                -1.75245280340846749E-04, -1.25544063060690348E-04, -8.22982872820208365E-05, -4.62860730588116458E-05,
                -1.72334302366962267E-05, 5.60690482304602267E-06, 2.31395443148286800E-05, 3.62642745856793957E-05,
                4.58006124490188752E-05, 5.24595294959114050E-05, 5.68396208545815266E-05, 5.94349820393104052E-05,
                6.06478527578421742E-05, 6.08023907788436497E-05, 6.01577894539460388E-05, 5.89199657344698500E-05,
                5.72515823777593053E-05, 5.52804375585852577E-05, 5.31063773802880170E-05, 5.08069302012325706E-05,
                4.84418647620094842E-05, 4.60568581607475370E-05, -6.91141397288294174E-04, -4.29976633058871912E-04,
                1.83067735980039018E-04, 6.60088147542014144E-04, 8.75964969951185931E-04, 8.77335235958235514E-04,
                7.49369585378990637E-04, 5.63832329756980918E-04, 3.68059319971443156E-04, 1.88464535514455599E-04,
                3.70663057664904149E-05, -8.28520220232137023E-05, -1.72751952869172998E-04, -2.36314873605872983E-04,
                -2.77966150694906658E-04, -3.02079514155456919E-04, -3.12594712643820127E-04, -3.12872558758067163E-04,
                -3.05678038466324377E-04, -2.93226470614557331E-04, -2.77255655582934777E-04, -2.59103928467031709E-04,
                -2.39784014396480342E-04, -2.20048260045422848E-04, -2.00443911094971498E-04, -1.81358692210970687E-04,
                -1.63057674478657464E-04, -1.45712672175205844E-04, -1.29425421983924587E-04, -1.14245691942445952E-04,
                1.92821964248775885E-03, 1.35592576302022234E-03, -7.17858090421302995E-04, -2.58084802575270346E-03,
                -3.49271130826168475E-03, -3.46986299340960628E-03, -2.82285233351310182E-03, -1.88103076404891354E-03,
                -8.89531718383947600E-04, 3.87912102631035228E-06, 7.28688540119691412E-04, 1.26566373053457758E-03,
                1.62518158372674427E-03, 1.83203153216373172E-03, 1.91588388990527909E-03, 1.90588846755546138E-03,
                1.82798982421825727E-03, 1.70389506421121530E-03, 1.55097127171097686E-03, 1.38261421852276159E-03,
                1.20881424230064774E-03, 1.03676532638344962E-03, 8.71437918068619115E-04, 7.16080155297701002E-04,
                5.72637002558129372E-04, 4.42089819465802277E-04, 3.24724948503090564E-04, 2.20342042730246599E-04,
                1.28412898401353882E-04, 4.82005924552095464E-05};
        final double[] beta = new double[] {1.79988721413553309E-02, 5.59964911064388073E-03, 2.88501402231132779E-03,
                1.80096606761053941E-03, 1.24753110589199202E-03, 9.22878876572938311E-04, 7.14430421727287357E-04,
                5.71787281789704872E-04, 4.69431007606481533E-04, 3.93232835462916638E-04, 3.34818889318297664E-04,
                2.88952148495751517E-04, 2.52211615549573284E-04, 2.22280580798883327E-04, 1.97541838033062524E-04,
                1.76836855019718004E-04, 1.59316899661821081E-04, 1.44347930197333986E-04, 1.31448068119965379E-04,
                1.20245444949302884E-04, 1.10449144504599392E-04, 1.01828770740567258E-04, 9.41998224204237509E-05,
                8.74130545753834437E-05, 8.13466262162801467E-05, 7.59002269646219339E-05, 7.09906300634153481E-05,
                6.65482874842468183E-05, 6.25146958969275078E-05, 5.88403394426251749E-05, -1.49282953213429172E-03,
                -8.78204709546389328E-04, -5.02916549572034614E-04, -2.94822138512746025E-04, -1.75463996970782828E-04,
                -1.04008550460816434E-04, -5.96141953046457895E-05, -3.12038929076098340E-05, -1.26089735980230047E-05,
                -2.42892608575730389E-07, 8.05996165414273571E-06, 1.36507009262147391E-05, 1.73964125472926261E-05,
                1.98672978842133780E-05, 2.14463263790822639E-05, 2.23954659232456514E-05, 2.28967783814712629E-05,
                2.30785389811177817E-05, 2.30321976080909144E-05, 2.28236073720348722E-05, 2.25005881105292418E-05,
                2.20981015361991429E-05, 2.16418427448103905E-05, 2.11507649256220843E-05, 2.06388749782170737E-05,
                2.01165241997081666E-05, 1.95913450141179244E-05, 1.90689367910436740E-05, 1.85533719641636667E-05,
                1.80475722259674218E-05, 5.52213076721292790E-04, 4.47932581552384646E-04, 2.79520653992020589E-04,
                1.52468156198446602E-04, 6.93271105657043598E-05, 1.76258683069991397E-05, -1.35744996343269136E-05,
                -3.17972413350427135E-05, -4.18861861696693365E-05, -4.69004889379141029E-05, -4.87665447413787352E-05,
                -4.87010031186735069E-05, -4.74755620890086638E-05, -4.55813058138628452E-05, -4.33309644511266036E-05,
                -4.09230193157750364E-05, -3.84822638603221274E-05, -3.60857167535410501E-05, -3.37793306123367417E-05,
                -3.15888560772109621E-05, -2.95269561750807315E-05, -2.75978914828335759E-05, -2.58006174666883713E-05,
                -2.41308356761280200E-05, -2.25823509518346033E-05, -2.11479656768912971E-05, -1.98200638885294927E-05,
                -1.85909870801065077E-05, -1.74532699844210224E-05, -1.63997823854497997E-05, -4.74617796559959808E-04,
                -4.77864567147321487E-04, -3.20390228067037603E-04, -1.61105016119962282E-04, -4.25778101285435204E-05,
                3.44571294294967503E-05, 7.97092684075674924E-05, 1.03138236708272200E-04, 1.12466775262204158E-04,
                1.13103642108481389E-04, 1.08651634848774268E-04, 1.01437951597661973E-04, 9.29298396593363896E-05,
                8.40293133016089978E-05, 7.52727991349134062E-05, 6.69632521975730872E-05, 5.92564547323194704E-05,
                5.22169308826975567E-05, 4.58539485165360646E-05, 4.01445513891486808E-05, 3.50481730031328081E-05,
                3.05157995034346659E-05, 2.64956119950516039E-05, 2.29363633690998152E-05, 1.97893056664021636E-05,
                1.70091984636412623E-05, 1.45547428261524004E-05, 1.23886640995878413E-05, 1.04775876076583236E-05,
                8.79179954978479373E-06, 7.36465810572578444E-04, 8.72790805146193976E-04, 6.22614862573135066E-04,
                2.85998154194304147E-04, 3.84737672879366102E-06, -1.87906003636971558E-04, -2.97603646594554535E-04,
                -3.45998126832656348E-04, -3.53382470916037712E-04, -3.35715635775048757E-04, -3.04321124789039809E-04,
                -2.66722723047612821E-04, -2.27654214122819527E-04, -1.89922611854562356E-04, -1.55058918599093870E-04,
                -1.23778240761873630E-04, -9.62926147717644187E-05, -7.25178327714425337E-05, -5.22070028895633801E-05,
                -3.50347750511900522E-05, -2.06489761035551757E-05, -8.70106096849767054E-06, 1.13698686675100290E-06,
                9.16426474122778849E-06, 1.56477785428872620E-05, 2.08223629482466847E-05, 2.48923381004595156E-05,
                2.80340509574146325E-05, 3.03987774629861915E-05, 3.21156731406700616E-05, -1.80182191963885708E-03,
                -2.43402962938042533E-03, -1.83422663549856802E-03, -7.62204596354009765E-04, 2.39079475256927218E-04,
                9.49266117176881141E-04, 1.34467449701540359E-03, 1.48457495259449178E-03, 1.44732339830617591E-03,
                1.30268261285657186E-03, 1.10351597375642682E-03, 8.86047440419791759E-04, 6.73073208165665473E-04,
                4.77603872856582378E-04, 3.05991926358789362E-04, 1.60315694594721630E-04, 4.00749555270613286E-05,
                -5.66607461635251611E-05, -1.32506186772982638E-04, -1.90296187989614057E-04, -2.32811450376937408E-04,
                -2.62628811464668841E-04, -2.82050469867598672E-04, -2.93081563192861167E-04, -2.97435962176316616E-04,
                -2.96557334239348078E-04, -2.91647363312090861E-04, -2.83696203837734166E-04, -2.73512317095673346E-04,
                -2.61750155806768580E-04, 6.38585891212050914E-03, 9.62374215806377941E-03, 7.61878061207001043E-03,
                2.83219055545628054E-03, -2.09841352012720090E-03, -5.73826764216626498E-03, -7.70804244495414620E-03,
                -8.21011692264844401E-03, -7.65824520346905413E-03, -6.47209729391045177E-03, -4.99132412004966473E-03,
                -3.45612289713133280E-03, -2.01785580014170775E-03, -7.59430686781961401E-04, 2.84173631523859138E-04,
                1.10891667586337403E-03, 1.72901493872728771E-03, 2.16812590802684701E-03, 2.45357710494539735E-03,
                2.61281821058334862E-03, 2.67141039656276912E-03, 2.65203073395980430E-03, 2.57411652877287315E-03,
                2.45389126236094427E-03, 2.30460058071795494E-03, 2.13684837686712662E-03, 1.95896528478870911E-03,
                1.77737008679454412E-03, 1.59690280765839059E-03, 1.42111975664438546E-03};
        final double[] gama = new double[] {6.29960524947436582E-01, 2.51984209978974633E-01, 1.54790300415655846E-01,
                1.10713062416159013E-01, 8.57309395527394825E-02, 6.97161316958684292E-02, 5.86085671893713576E-02,
                5.04698873536310685E-02, 4.42600580689154809E-02, 3.93720661543509966E-02, 3.54283195924455368E-02,
                3.21818857502098231E-02, 2.94646240791157679E-02, 2.71581677112934479E-02, 2.51768272973861779E-02,
                2.34570755306078891E-02, 2.19508390134907203E-02, 2.06210828235646240E-02, 1.94388240897880846E-02,
                1.83810633800683158E-02, 1.74293213231963172E-02, 1.65685837786612353E-02, 1.57865285987918445E-02,
                1.50729501494095594E-02, 1.44193250839954639E-02, 1.38184805735341786E-02, 1.32643378994276568E-02,
                1.27517121970498651E-02, 1.22761545318762767E-02, 1.18338262398482403E-02};
        final double ex1 = 3.33333333333333333E-01;
        final double ex2 = 6.66666666666666667E-01;
        final double hpi = Math.PI / 2.0;
        final double thpi = 1.5 * Math.PI;
        double rfnu;
        double test;
        double ac;
        double zbr;
        double zbi;
        double rfnu2;
        double fn13;
        double fn23;
        double rfn13;
        double w2r;
        double w2i;
        double aw2;
        int k;
        final double[] pr = new double[30];
        final double[] pi = new double[30];
        double sumar;
        double sumai;
        final double[] ap = new double[30];
        int kmax;
        final double[] zar = new double[1];
        final double[] zai = new double[1];
        final double[] str = new double[1];
        final double[] sti = new double[1];
        double sumbr;
        double sumbi;
        int l1;
        int l2;
        double btol;
        double atol;
        double pp;
        int ias;
        int ibs;
        int m;
        final double[] wr = new double[1];
        final double[] wi = new double[1];
        final double[] zcr = new double[1];
        final double[] zci = new double[1];
        final int[] idum = new int[1];
        double zthr;
        double zthi;
        double azth;
        double ang;
        double zetar;
        double zetai;
        final double[] rtztr = new double[1];
        final double[] rtzti = new double[1];
        double tzar;
        double tzai;
        double raw;
        double tfnr;
        double tfni;
        double razth;
        double rzthr;
        double rzthi;
        double raw2;
        double t2r;
        double t2i;
        final double[] upr = new double[14];
        final double[] upi = new double[14];
        double przthr;
        double przthi;
        double ptfnr;
        double ptfni;
        int ks;
        int kp1;
        int l;
        int lr;
        int lrp1;
        final double[] crr = new double[14];
        final double[] cri = new double[14];
        final double[] drr = new double[14];
        final double[] dri = new double[14];
        int ju;
        int jr;
        int is;
        int j;

        rfnu = 1.0 / fnu;

        // Overflow test (z/fnu too small)
        test = 1.0E3 * tiny;
        ac = fnu * test;

        if ( (Math.abs(zr) <= ac) && (Math.abs(zi) <= ac)) {
            zeta1r[0] = (2.0 * Math.abs(Math.log(test))) + fnu;
            zeta1i[0] = 0.0;
            zeta2r[0] = fnu;
            zeta2i[0] = 0.0;
            phir[0] = 1.0;
            phii[0] = 0.0;
            argr[0] = 1.0;
            argi[0] = 0.0;

            return;
        } // if ((Math.abs(zr) <= ac) && (Math.abs(zi) <= ac))

        zbr = zr * rfnu;
        zbi = zi * rfnu;
        rfnu2 = rfnu * rfnu;

        // Compute in the fourth quadrant
        fn13 = Math.pow(fnu, ex1);
        fn23 = fn13 * fn13;
        rfn13 = 1.0 / fn13;
        w2r = 1.0 - (zbr * zbr) + (zbi * zbi);
        w2i = -2.0 * zbr * zbi;
        aw2 = zabs(w2r, w2i);

        if (aw2 <= 0.25) {

            // Power series for cabs(w2) <= 0.25
            k = 1;
            pr[0] = 1.0;
            pi[0] = 0.0;
            sumar = gama[0];
            sumai = 0.0;
            ap[0] = 1.0;

            group: {

                if (aw2 >= tol) {

                    for (k = 2; k <= 30; k++) {
                        pr[k - 1] = (pr[k - 2] * w2r) - (pi[k - 2] * w2i);
                        pi[k - 1] = (pr[k - 2] * w2i) + (pi[k - 2] * w2r);
                        sumar = sumar + (pr[k - 1] * gama[k - 1]);
                        sumai = sumai + (pi[k - 1] * gama[k - 1]);
                        ap[k - 1] = ap[k - 2] * aw2;

                        if (ap[k - 1] < tol) {
                            break group;
                        }
                    } // for (k = 2; k <= 30; k++)

                    k = 30;
                } // if (aw2 >= tol)
            } // group

            kmax = k;
            zetar = (w2r * sumar) - (w2i * sumai);
            zetai = (w2r * sumai) + (w2i * sumar);
            argr[0] = zetar * fn23;
            argi[0] = zetai * fn23;
            zsqrt(sumar, sumai, zar, zai);
            zsqrt(w2r, w2i, str, sti);
            zeta2r[0] = str[0] * fnu;
            zeta2i[0] = sti[0] * fnu;
            str[0] = 1.0 + (ex2 * ( (zetar * zar[0]) - (zetai * zai[0])));
            sti[0] = ex2 * ( (zetar * zai[0]) + (zetai * zar[0]));
            zeta1r[0] = (str[0] * zeta2r[0]) - (sti[0] * zeta2i[0]);
            zeta1i[0] = (str[0] * zeta2i[0]) + (sti[0] * zeta2r[0]);
            zar[0] = zar[0] + zar[0];
            zai[0] = zai[0] + zai[0];
            zsqrt(zar[0], zai[0], str, sti);
            phir[0] = str[0] * rfn13;
            phii[0] = sti[0] * rfn13;

            if (ipmtr == 1) {
                return;
            }

            // Sum series for asum and bsum
            sumbr = 0.0;
            sumbi = 0.0;

            for (k = 1; k <= kmax; k++) {
                sumbr = sumbr + (pr[k - 1] * beta[k - 1]);
                sumbi = sumbi + (pi[k - 1] * beta[k - 1]);
            } // for (k = 1; k <= kmax; k++)

            asumr[0] = 0.0;
            asumi[0] = 0.0;
            bsumr[0] = sumbr;
            bsumi[0] = sumbi;
            l1 = 0;
            l2 = 30;
            btol = tol * (Math.abs(bsumr[0]) + Math.abs(bsumi[0]));
            atol = tol;
            pp = 1.0;
            ias = 0;
            ibs = 0;

            if (rfnu2 < tol) {
                asumr[0] = asumr[0] + 1.0;
                pp = rfnu * rfn13;
                bsumr[0] = bsumr[0] * pp;
                bsumi[0] = bsumi[0] * pp;

                return;
            } // if (rfnu2 < tol)

            for (is = 2; is <= 7; is++) {
                atol = atol / rfnu2;
                pp = pp * rfnu2;

                if (ias != 1) {
                    sumar = 0.0;
                    sumai = 0.0;

                    for (k = 1; k <= kmax; k++) {
                        m = l1 + k;
                        sumar = sumar + (pr[k - 1] * alfa[m - 1]);
                        sumai = sumai + (pi[k - 1] * alfa[m - 1]);

                        if (ap[k - 1] < atol) {
                            break;
                        }
                    } // for (k = 1; k <= kmax; k++)

                    asumr[0] = asumr[0] + (sumar * pp);
                    asumi[0] = asumi[0] + (sumai * pp);

                    if (pp < tol) {
                        ias = 1;
                    }
                } // if (ias != 1)

                if (ibs != 1) {
                    sumbr = 0.0;
                    sumbi = 0.0;

                    for (k = 1; k <= kmax; k++) {
                        m = l2 + k;
                        sumbr = sumbr + (pr[k - 1] * beta[m - 1]);
                        sumbi = sumbi + (pi[k - 1] * beta[m - 1]);

                        if (ap[k - 1] < atol) {
                            break;
                        }
                    } // for (k = 1; k <= kmax; k++)

                    bsumr[0] = bsumr[0] + (sumbr * pp);
                    bsumi[0] = bsumi[0] + (sumbi * pp);

                    if (pp < btol) {
                        ibs = 1;
                    }
                } // if (ibs != 1)

                if ( (ias == 1) && (ibs == 1)) {
                    asumr[0] = asumr[0] + 1.0;
                    pp = rfnu * rfn13;
                    bsumr[0] = bsumr[0] * pp;
                    bsumi[0] = bsumi[0] * pp;

                    return;
                } // if ((ias == 1) && (ibs == 1))

                l1 = l1 + 30;
                l2 = l2 + 30;
            } // for (is = 2; is <= 7; is++)

            asumr[0] = asumr[0] + 1.0;
            pp = rfnu * rfn13;
            bsumr[0] = bsumr[0] * pp;
            bsumi[0] = bsumi[0] * pp;

            return;
        } // if (aw2 <= 0.25)

        // CABS(w2) > 0.25
        zsqrt(w2r, w2i, wr, wi);

        if (wr[0] < 0.0) {
            wr[0] = 0.0;
        }

        if (wi[0] < 0.0) {
            wi[0] = 0.0;
        }

        str[0] = wr[0] + 1.0;
        sti[0] = wi[0];
        zdiv(str[0], sti[0], zbr, zbi, zar, zai);
        zlog(zar[0], zai[0], zcr, zci, idum);

        if (zci[0] < 0.0) {
            zci[0] = 0.0;
        }

        if (zci[0] > hpi) {
            zci[0] = hpi;
        }

        if (zcr[0] < 0.0) {
            zcr[0] = 0.0;
        }

        zthr = (zcr[0] - wr[0]) * 1.5;
        zthi = (zci[0] - wi[0]) * 1.5;
        zeta1r[0] = zcr[0] * fnu;
        zeta1i[0] = zci[0] * fnu;
        zeta2r[0] = wr[0] * fnu;
        zeta2i[0] = wi[0] * fnu;
        azth = zabs(zthr, zthi);
        ang = thpi;

        if ( (zthr < 0.0) || (zthi >= 0.0)) {
            ang = hpi;

            if (zthr != 0.0) {
                ang = Math.atan(zthi / zthr);

                if (zthr < 0.0) {
                    ang = ang + Math.PI;
                }
            } // if (zthr != 0.0)
        } // if ((zthr < 0.0) || (zthi >= 0.0))

        pp = Math.pow(azth, ex2);
        ang = ang * ex2;
        zetar = pp * Math.cos(ang);
        zetai = pp * Math.sin(ang);

        if (zetai < 0.0) {
            zetai = 0.0;
        }

        argr[0] = zetar * fn23;
        argi[0] = zetai * fn23;
        zdiv(zthr, zthi, zetar, zetai, rtztr, rtzti);
        zdiv(rtztr[0], rtzti[0], wr[0], wi[0], zar, zai);
        tzar = zar[0] + zar[0];
        tzai = zai[0] + zai[0];
        zsqrt(tzar, tzai, str, sti);
        phir[0] = str[0] * rfn13;
        phii[0] = sti[0] * rfn13;

        if (ipmtr == 1) {
            return;
        }

        raw = 1.0 / Math.sqrt(aw2);
        str[0] = wr[0] * raw;
        sti[0] = -wi[0] * raw;
        tfnr = str[0] * rfnu * raw;
        tfni = sti[0] * rfnu * raw;
        razth = 1.0 / azth;
        str[0] = zthr * razth;
        sti[0] = -zthi * razth;
        rzthr = str[0] * razth * rfnu;
        rzthi = sti[0] * razth * rfnu;
        zcr[0] = rzthr * ar[1];
        zci[0] = rzthi * ar[1];
        raw2 = 1.0 / aw2;
        str[0] = w2r * raw2;
        sti[0] = -w2i * raw2;
        t2r = str[0] * raw2;
        t2i = sti[0] * raw2;
        str[0] = (t2r * c[1]) + c[2];
        sti[0] = t2i * c[1];
        upr[1] = (str[0] * tfnr) - (sti[0] * tfni);
        upi[1] = (str[0] * tfni) + (sti[0] * tfnr);
        bsumr[0] = upr[1] + zcr[0];
        bsumi[0] = upi[1] + zci[0];
        asumr[0] = 0.0;
        asumi[0] = 0.0;

        if (rfnu < tol) {
            asumr[0] = asumr[0] + 1.0;
            str[0] = -bsumr[0] * rfn13;
            sti[0] = -bsumi[0] * rfn13;
            zdiv(str[0], sti[0], rtztr[0], rtzti[0], bsumr, bsumi);

            return;
        } // if (rfnu < tol)

        przthr = rzthr;
        przthi = rzthi;
        ptfnr = tfnr;
        ptfni = tfni;
        upr[0] = 1.0;
        upi[0] = 0.0;
        pp = 1.0;
        btol = tol * (Math.abs(bsumr[0]) + Math.abs(bsumi[0]));
        ks = 0;
        kp1 = 2;
        l = 3;
        ias = 0;
        ibs = 0;

        for (lr = 2; lr <= 12; lr += 2) {
            lrp1 = lr + 1;

            // Compute two additional cr, dr, and up for two more terms in next
            // suma and sumb
            for (k = lr; k <= lrp1; k++) {
                ks = ks + 1;
                kp1 = kp1 + 1;
                l = l + 1;
                zar[0] = c[l - 1];
                zai[0] = 0.0;

                for (j = 2; j <= kp1; j++) {
                    l = l + 1;
                    str[0] = (zar[0] * t2r) - (t2i * zai[0]) + c[l - 1];
                    zai[0] = (zar[0] * t2i) + (zai[0] * t2r);
                    zar[0] = str[0];
                } // for (j = 2; j <= kp1; j++)

                str[0] = (ptfnr * tfnr) - (ptfni * tfni);
                ptfni = (ptfnr * tfni) + (ptfni * tfnr);
                ptfnr = str[0];
                upr[kp1 - 1] = (ptfnr * zar[0]) - (ptfni * zai[0]);
                upi[kp1 - 1] = (ptfni * zar[0]) + (ptfnr * zai[0]);
                crr[ks - 1] = przthr * br[ks];
                cri[ks - 1] = przthi * br[ks];
                str[0] = (przthr * rzthr) - (przthi * rzthi);
                przthi = (przthr * rzthi) + (przthi * rzthr);
                przthr = str[0];
                drr[ks - 1] = przthr * ar[ks + 1];
                dri[ks - 1] = przthi * ar[ks + 1];
            } // for (k = lr; k <= lrp1; k++)

            pp = pp * rfnu2;

            if (ias != 1) {
                sumar = upr[lrp1 - 1];
                sumai = upi[lrp1 - 1];
                ju = lrp1;

                for (jr = 1; jr <= lr; jr++) {
                    ju = ju - 1;
                    sumar = sumar + (crr[jr - 1] * upr[ju - 1]) - (cri[jr - 1] * upi[ju - 1]);
                    sumai = sumai + (crr[jr - 1] * upi[ju - 1]) + (cri[jr - 1] * upr[ju - 1]);
                } // for (jr = 1; jr <= lr; jr++)

                asumr[0] = asumr[0] + sumar;
                asumi[0] = asumi[0] + sumai;
                test = Math.abs(sumar) + Math.abs(sumai);

                if ( (pp < tol) && (test < tol)) {
                    ias = 1;
                }
            } // if (ias != 1)

            if (ibs != 1) {
                sumbr = upr[lr + 1] + (upr[lrp1 - 1] * zcr[0]) - (upi[lrp1 - 1] * zci[0]);
                sumbi = upi[lr + 1] + (upr[lrp1 - 1] * zci[0]) + (upi[lrp1 - 1] * zcr[0]);
                ju = lrp1;

                for (jr = 1; jr <= lr; jr++) {
                    ju = ju - 1;
                    sumbr = sumbr + (drr[jr - 1] * upr[ju - 1]) - (dri[jr - 1] * upi[ju - 1]);
                    sumbi = sumbi + (drr[jr - 1] * upi[ju - 1]) + (dri[jr - 1] * upr[ju - 1]);
                } // for (jr = 1; jr <= lr; jr++)

                bsumr[0] = bsumr[0] + sumbr;
                bsumi[0] = bsumi[0] + sumbi;
                test = Math.abs(sumbr) + Math.abs(sumbi);

                if ( (pp < btol) && (test < btol)) {
                    ibs = 1;
                }
            } // if (ibs != 1)

            if ( (ias == 1) && (ibs == 1)) {
                break;
            }
        } // for (lr = 2; lr <= 12; lr+=2)

        asumr[0] = asumr[0] + 1.0;
        str[0] = -bsumr[0] * rfn13;
        sti[0] = -bsumi[0] * rfn13;
        zdiv(str[0], sti[0], rtztr[0], rtzti[0], bsumr, bsumi);

        return;
    }

    /**
     * ZUNI1 COMPUTES I(FNU,Z) BY MEANS OF THE UNIFORM ASYMPTOTIC EXPANSION FOR I(FNU,Z) IN -PI/3.LE.ARG Z.LE.PI/3.
     * 
     * <p>
     * NUL IS THE SMALLEST ORDER PERMITTED FOR THE ASYMPTOTIC EXPANSION. NLAST=0 MEANS ALL OF THE Y VALUES WERE SET.
     * NLAST.NE.0 IS THE NUMBER LEFT TO BE COMPUTED BY ANOTHER FORMULA FOR ORDERS FNU TO FNU+NLAST-1 BECAUSE
     * FNU+NLAST-1.LT.FNUL. Y(I)=CZERO FOR I=NLAST+1,N
     * </p>
     * 
     * @param zr double
     * @param zi double
     * @param fnu double
     * @param kode int
     * @param n int
     * @param yr double[]
     * @param yi double[]
     * @param nz int[]
     * @param nlast int[]
     * @param fnul double
     */
    private void zuni1(final double zr, double zi, final double fnu, final int kode, final int n, final double[] yr,
            final double[] yi, final int[] nz, final int[] nlast, final double fnul) {
        final double[] bry = new double[3];
        final double[] cwrkr = new double[16];
        final double[] cwrki = new double[16];
        final double[] cssr = new double[3];
        final double[] csrr = new double[3];
        final double[] cyr = new double[2];
        final double[] cyi = new double[2];
        int nd;
        double cscl;
        double crsc;
        double fn;
        int init;
        final double[] phir = new double[1];
        final double[] phii = new double[1];
        final double[] zeta1r = new double[1];
        final double[] zeta1i = new double[1];
        final double[] zeta2r = new double[1];
        final double[] zeta2i = new double[1];
        final double[] sumr = new double[1];
        final double[] sumi = new double[1];
        double str;
        double sti;
        double rast;
        double s1r;
        double s1i;
        double rs1;
        double aphi;
        double s2r;
        double s2i;
        final int[] nw = new int[1];
        int m;
        double rzr;
        double rzi;
        double c1r;
        double ascle;
        int k;
        int i;
        double c2r;
        double c2i;
        double c2m;
        final int[] nuf = new int[1];
        int nn;
        int iflag = 1;

        nz[0] = 0;
        nd = n;
        nlast[0] = 0;

        // Computed values with exponents between alim and elim in magnitude
        // are scaled to keep intermediate arithmetic on scale,
        // exp(alim) = tol * exp(elim)
        cscl = 1.0 / tol;
        crsc = tol;
        cssr[0] = cscl;
        cssr[1] = 1.0;
        cssr[2] = crsc;
        csrr[0] = crsc;
        csrr[1] = 1.0;
        csrr[2] = cscl;
        bry[0] = 1.0E3 * tiny / tol;

        // Check for underflow and overflow on first member
        fn = Math.max(fnu, 1.0);
        init = 0;
        zunik(zr, zi, fn, 1, 1, init, phir, phii, zeta1r, zeta1i, zeta2r, zeta2i, sumr, sumi, cwrkr, cwrki);

        if (kode != 1) {
            str = zr + zeta2r[0];
            sti = zi + zeta2i[0];
            rast = fn / zabs(str, sti);
            str = str * rast * rast;
            sti = -sti * rast * rast;
            s1r = -zeta1r[0] + str;
            s1i = -zeta1i[0] + sti;
        } // if (kode != 1)
        else { // kode == 1
            s1r = -zeta1r[0] + zeta2r[0];
            s1i = -zeta1i[0] + zeta2i[0];
        } // else kode == 1

        rs1 = s1r;

        if (Math.abs(rs1) > elim) {

            if (rs1 > 0.0) {
                nz[0] = -1;

                return;
            } // if (rs1 > 0.0)

            nz[0] = n;

            for (i = 1; i <= n; i++) {
                yr[i - 1] = 0.0;
                yi[i - 1] = 0.0;
            } // for (i = 1; i <= n; i++)

            return;
        } // if (Math.abs(rs1) > elim)

        while (true) {
            nn = Math.min(2, nd);

            group: {

                for (i = 1; i <= nn; i++) {
                    fn = fnu + nd - i;
                    init = 0;
                    zunik(zr, zi, fn, 1, 0, init, phir, phii, zeta1r, zeta1i, zeta2r, zeta2i, sumr, sumi, cwrkr, cwrki);

                    if (kode != 1) {
                        str = zr + zeta2r[0];
                        sti = zi + zeta2i[0];
                        rast = fn / zabs(str, sti);
                        str = str * rast * rast;
                        sti = -sti * rast * rast;
                        s1r = -zeta1r[0] + str;
                        s1i = -zeta1i[0] + sti + zi;
                    } // if (kode != 1)
                    else { // kode == 1
                        s1r = -zeta1r[0] + zeta2r[0];
                        s1i = -zeta1i[0] + zeta2i[0];
                    } // else kode == 1

                    // Test for underflow and overflow
                    rs1 = s1r;

                    if (Math.abs(rs1) > elim) {
                        break group;
                    } // if (Math.abs(rs1) > elim)

                    if (i == 1) {
                        iflag = 2;
                    } // if (i == 1)

                    if (Math.abs(rs1) >= alim) {

                        // Refine test and scale
                        aphi = zabs(phir[0], phii[0]);
                        rs1 = rs1 + Math.log(aphi);

                        if (Math.abs(rs1) > elim) {
                            break group;
                        } // if (Math.abs(rs1) > elim)

                        if (i == 1) {
                            iflag = 1;
                        } // if (i == 1)

                        if ( (rs1 >= 0.0) && (i == 1)) {
                            iflag = 3;
                        } // if ((rs1 >= 0.0) && (i == 1))
                    } // if (Math.abs(rs1) >= alim)

                    // Scale s1 if cabs(s1) < ascle
                    s2r = (phir[0] * sumr[0]) - (phii[0] * sumi[0]);
                    s2i = (phir[0] * sumi[0]) + (phii[0] * sumr[0]);
                    str = Math.exp(s1r) * cssr[iflag - 1];
                    s1r = str * Math.cos(s1i);
                    s1i = str * Math.sin(s1i);
                    str = (s2r * s1r) - (s2i * s1i);
                    s2i = (s2r * s1i) + (s2i * s1r);
                    s2r = str;

                    if (iflag == 1) {
                        zuchk(s2r, s2i, nw, bry[0]);

                        if (nw[0] != 0) {
                            break group;
                        } // if (nw[0] != 0)
                    } // if (iflag == 1)

                    cyr[i - 1] = s2r;
                    cyi[i - 1] = s2i;
                    m = nd - i + 1;
                    yr[m - 1] = s2r * csrr[iflag - 1];
                    yi[m - 1] = s2i * csrr[iflag - 1];
                } // for (i = 1; i <= nn; i++)

                if (nd <= 2) {
                    return;
                } // if (nd <= 2)

                rast = 1.0 / zabs(zr, zi);
                str = zr * rast;
                sti = -zi * rast;
                rzr = (str + str) * rast;
                rzi = (sti + sti) * rast;
                bry[1] = 1.0 / bry[0];
                bry[2] = Double.MAX_VALUE;
                s1r = cyr[0];
                s1i = cyi[0];
                s2r = cyr[1];
                s2i = cyi[1];
                c1r = csrr[iflag - 1];
                ascle = bry[iflag - 1];
                k = nd - 2;
                fn = k;

                for (i = 3; i <= nd; i++) {
                    c2r = s2r;
                    c2i = s2i;
                    s2r = s1r + ( (fnu + fn) * ( (rzr * c2r) - (rzi * c2i)));
                    s2i = s1i + ( (fnu + fn) * ( (rzr * c2i) + (rzi * c2r)));
                    s1r = c2r;
                    s1i = c2i;
                    c2r = s2r * c1r;
                    c2i = s2i * c1r;
                    yr[k - 1] = c2r;
                    yi[k - 1] = c2i;
                    k = k - 1;
                    fn = fn - 1.0;

                    if (iflag >= 3) {
                        continue;
                    } // if (iflag >= 3)

                    str = Math.abs(c2r);
                    sti = Math.abs(c2i);
                    c2m = Math.max(str, sti);

                    if (c2m <= ascle) {
                        continue;
                    } // if (c2m <= ascle)

                    iflag = iflag + 1;
                    ascle = bry[iflag - 1];
                    s1r = s1r * c1r;
                    s1i = s1i * c1r;
                    s2r = c2r;
                    s2i = c2i;
                    s1r = s1r * cssr[iflag - 1];
                    s1i = s1i * cssr[iflag - 1];
                    s2r = s2r * cssr[iflag - 1];
                    s2i = s2i * cssr[iflag - 1];
                    c1r = csrr[iflag - 1];
                } // for (i = 3; i <= nd; i++)

                return;
            } // group

            // Set underflow and update parameters
            if (rs1 > 0.0) {
                nz[0] = -1;

                return;
            } // if (rs1 > 0.0)

            yr[nd - 1] = 0.0;
            yi[nd - 1] = 0.0;
            nz[0] = nz[0] + 1;
            nd = nd - 1;

            if (nd == 0) {
                return;
            } // if (nd == 0)

            zuoik(zr, zi, fnu, kode, 1, nd, yr, yi, nuf);

            if (nuf[0] < 0) {
                nz[0] = -1;

                return;
            } // if (nuf[0] < 0)

            nd = nd - nuf[0];
            nz[0] = nz[0] + nuf[0];

            if (nd == 0) {
                return;
            } // if (nd == 0)

            fn = fnu + nd - 1.0;

            if (fn < fnul) {
                nlast[0] = nd;

                return;
            } // if (fn < fnul)
        } // while (true)
    }

    /**
     * ZUNI2 COMPUTES I(FNU,Z) IN THE RIGHT HALF PLANE BY MEANS OF UNIFORM ASYMPTOTIC EXPANSION FOR J(FNU,ZN) WHERE ZN
     * IS Z*I OR -Z*I AND ZN IS IN THE RIGHT HALF PLANE ALSO.
     * 
     * <p>
     * FNUL IS THE SMALLEST ORDER PERMITTED FOR THE ASYMPTOTIC EXPANSION. NLAST=0 MEANS ALL OF THE Y VALUES WERE SET.
     * NLAST.NE.0 IS THE NUMBER LEFT TO BE COMPUTED BY ANOTHER FORMULA FOR ORDERS FNU TO FNU+NLAST-1 BECAUSE
     * FNU+NLAST-1.LT.FNUL. Y(I)=CZERO FOR I=NLAST+1,N
     * </p>
     * 
     * @param zr double
     * @param zi double
     * @param fnu double
     * @param kode int
     * @param n int
     * @param yr double[]
     * @param yi double[]
     * @param nz int[]
     * @param nlast int[]
     * @param fnul double
     */
    private void zuni2(double zr, double zi, final double fnu, final int kode, final int n, final double[] yr,
            final double[] yi, final int[] nz, final int[] nlast, final double fnul) {
        final double[] bry = new double[3];
        final double[] cipr = new double[] {1.0, 0.0, -1.0, 0.0};
        final double[] cipi = new double[] {0.0, 1.0, 0.0, -1.0};
        final double[] cssr = new double[3];
        final double[] csrr = new double[3];
        final double[] cyr = new double[2];
        final double[] cyi = new double[2];
        final double hpi = Math.PI / 2.0;
        final double aic = 1.265512123484645396;
        int nd;
        double cscl;
        double crsc;
        double znr;
        double zni;
        double zbr;
        double zbi;
        double cidi;
        int inu;
        double ang;
        double c2r;
        double c2i;
        double car;
        double sar;
        int in;
        double str;
        double fn;
        final double[] phir = new double[1];
        final double[] phii = new double[1];
        final double[] argr = new double[1];
        final double[] argi = new double[1];
        final double[] zeta1r = new double[1];
        final double[] zeta1i = new double[1];
        final double[] zeta2r = new double[1];
        final double[] zeta2i = new double[1];
        final double[] asumr = new double[1];
        final double[] asumi = new double[1];
        final double[] bsumr = new double[1];
        final double[] bsumi = new double[1];
        double sti;
        double rast;
        double s1r;
        double s1i;
        double rs1;
        int nn;
        int i;
        int iflag = 1;
        double aphi;
        double aarg;
        final int[] nw = new int[1];
        int j;
        double c1r;
        double ascle;
        int k;
        double c2m;
        final int[] idum = new int[1];
        final double[] air = new double[1];
        final double[] aii = new double[1];
        final int[] nai = new int[1];
        final double[] dair = new double[1];
        final double[] daii = new double[1];
        final int[] ndai = new int[1];
        double s2r;
        double s2i;
        double raz;
        double rzr;
        double rzi;
        final int[] nuf = new int[1];

        nz[0] = 0;
        nd = n;
        nlast[0] = 0;

        // Computed values with exponents between alim and elim in magnitude
        // are scaled to keep intermediate arithmetic on scale,
        // exp(alim) = tol * exp(elim)
        cscl = 1.0 / tol;
        crsc = tol;
        cssr[0] = cscl;
        cssr[1] = 1.0;
        cssr[2] = crsc;
        csrr[0] = crsc;
        csrr[1] = 1.0;
        csrr[2] = cscl;
        bry[0] = 1.0E3 * tiny / tol;

        // zn is in the right half plane after rotation by ci or -ci
        znr = zi;
        zni = -zr;
        zbr = zr;
        zbi = zi;
        cidi = -1.0;
        inu = (int) (fnu);
        ang = hpi * (fnu - inu);
        c2r = Math.cos(ang);
        c2i = Math.sin(ang);
        car = c2r;
        sar = c2i;
        in = inu + n - 1;
        in = (in % 4) + 1;
        str = (c2r * cipr[in - 1]) - (c2i * cipi[in - 1]);
        c2i = (c2r * cipi[in - 1]) + (c2i * cipr[in - 1]);
        c2r = str;

        if (zi <= 0.0) {
            znr = -znr;
            zbi = -zbi;
            cidi = -cidi;
            c2i = -c2i;
        } // if (zi <= 0.0)

        // Check for underflow and overflow on first member
        fn = Math.max(fnu, 1.0);
        zunhj(znr, zni, fn, 1, phir, phii, argr, argi, zeta1r, zeta1i, zeta2r, zeta2i, asumr, asumi, bsumr, bsumi);

        if (kode != 1) {
            str = zbr + zeta2r[0];
            sti = zbi + zeta2i[0];
            rast = fn / zabs(str, sti);
            str = str * rast * rast;
            sti = -sti * rast * rast;
            s1r = -zeta1r[0] + str;
            s1i = -zeta1i[0] + sti;
        } // if (kode != 1)
        else { // kode == 1
            s1r = -zeta1r[0] + zeta2r[0];
            s1i = -zeta1i[0] + zeta2i[0];
        } // else kode == 1

        rs1 = s1r;

        if (Math.abs(rs1) > elim) {

            if (rs1 > 0.0) {
                nz[0] = -1;

                return;
            } // if (rs1 > 0.0)

            nz[0] = n;

            for (i = 1; i <= n; i++) {
                yr[i - 1] = 0.0;
                yi[i - 1] = 0.0;
            } // for (i = 1; i <= n; i++)

            return;
        } // if (Math.abs(rs1) > elim)

        while (true) {
            nn = Math.min(2, nd);

            group: {

                for (i = 1; i <= nn; i++) {
                    fn = fnu + nd - i;
                    zunhj(znr, zni, fn, 0, phir, phii, argr, argi, zeta1r, zeta1i, zeta2r, zeta2i, asumr, asumi, bsumr,
                            bsumi);

                    if (kode != 1) {
                        str = zbr + zeta2r[0];
                        sti = zbi + zeta2i[0];
                        rast = fn / zabs(str, sti);
                        str = str * rast * rast;
                        sti = -sti * rast * rast;
                        s1r = -zeta1r[0] + str;
                        s1i = -zeta1i[0] + sti + Math.abs(zi);
                    } // if (kode != 1)
                    else { // kode == 1
                        s1r = -zeta1r[0] + zeta2r[0];
                        s1i = -zeta1i[0] + zeta2i[0];
                    } // else kode == 1

                    // Test for underflow and overflow
                    rs1 = s1r;

                    if (Math.abs(rs1) > elim) {
                        break group;
                    } // if (Math.abs(rs1) > elim)

                    if (i == 1) {
                        iflag = 2;
                    } // if (i == 1)

                    if (Math.abs(rs1) >= alim) {

                        // Refine test and scale
                        aphi = zabs(phir[0], phii[0]);
                        aarg = zabs(argr[0], argi[0]);
                        rs1 = rs1 + Math.log(aphi) - (0.25 * Math.log(aarg)) - aic;

                        if (Math.abs(rs1) > elim) {
                            break group;
                        } // if (Math.abs(rs1) > elim)

                        if (i == 1) {
                            iflag = 1;
                        } // if (i == 1)

                        if ( (rs1 >= 0.0) && (i == 1)) {
                            iflag = 3;
                        } // if ((rs1 >= 0.0) && (i == 1))
                    } // if Math.abs(rs1) >= alim)

                    // Scale s1 to keep intermediate artihmetic on scale near
                    // exponent extremes
                    zairy(argr[0], argi[0], 0, 2, air, aii, nai, idum);
                    zairy(argr[0], argi[0], 1, 2, dair, daii, ndai, idum);
                    str = (dair[0] * bsumr[0]) - (daii[0] * bsumi[0]);
                    sti = (dair[0] * bsumi[0]) + (daii[0] * bsumr[0]);
                    str = str + ( (air[0] * asumr[0]) - (aii[0] * asumi[0]));
                    sti = sti + ( (air[0] * asumi[0]) + (aii[0] * asumr[0]));
                    s2r = (phir[0] * str) - (phii[0] * sti);
                    s2i = (phir[0] * sti) + (phii[0] * str);
                    str = Math.exp(s1r) * cssr[iflag - 1];
                    s1r = str * Math.cos(s1i);
                    s1i = str * Math.sin(s1i);
                    str = (s2r * s1r) - (s2i * s1i);
                    s2i = (s2r * s1i) + (s2i * s1r);
                    s2r = str;

                    if (iflag == 1) {
                        zuchk(s2r, s2i, nw, bry[0]);

                        if (nw[0] != 0) {
                            break group;
                        } // if (nw[0] != 0)
                    } // if (iflag == 1)

                    if (zi <= 0.0) {
                        s2i = -s2i;
                    } // if (zi <= 0.0)

                    str = (s2r * c2r) - (s2i * c2i);
                    s2i = (s2r * c2i) + (s2i * c2r);
                    s2r = str;
                    cyr[i - 1] = s2r;
                    cyi[i - 1] = s2i;
                    j = nd - i + 1;
                    yr[j - 1] = s2r * csrr[iflag - 1];
                    yi[j - 1] = s2i * csrr[iflag - 1];
                    str = -c2i * cidi;
                    c2i = c2r * cidi;
                    c2r = str;
                } // for (i = 1; i <= nn; i++)

                if (nd <= 2) {
                    return;
                } // if (nd <= 2)

                raz = 1.0 / zabs(zr, zi);
                str = zr * raz;
                sti = -zi * raz;
                rzr = (str + str) * raz;
                rzi = (sti + sti) * raz;
                bry[1] = 1.0 / bry[0];
                bry[2] = Double.MAX_VALUE;
                s1r = cyr[0];
                s1i = cyi[0];
                s2r = cyr[1];
                s2i = cyi[1];
                c1r = csrr[iflag - 1];
                ascle = bry[iflag - 1];
                k = nd - 2;
                fn = k;

                for (i = 3; i <= nd; i++) {
                    c2r = s2r;
                    c2i = s2i;
                    s2r = s1r + ( (fnu + fn) * ( (rzr * c2r) - (rzi * c2i)));
                    s2i = s1i + ( (fnu + fn) * ( (rzr * c2i) + (rzi * c2r)));
                    s1r = c2r;
                    s1i = c2i;
                    c2r = s2r * c1r;
                    c2i = s2i * c1r;
                    yr[k - 1] = c2r;
                    yi[k - 1] = c2i;
                    k = k - 1;
                    fn = fn - 1.0;

                    if (iflag >= 3) {
                        continue;
                    } // if (iflag >= 3)

                    str = Math.abs(c2r);
                    sti = Math.abs(c2i);
                    c2m = Math.max(str, sti);

                    if (c2m <= ascle) {
                        continue;
                    } // if (c2m <= ascle)

                    iflag = iflag + 1;
                    ascle = bry[iflag - 1];
                    s1r = s1r * c1r;
                    s1i = s1i * c1r;
                    s2r = c2r;
                    s2i = c2i;
                    s1r = s1r * cssr[iflag - 1];
                    s1i = s1i * cssr[iflag - 1];
                    s2r = s2r * cssr[iflag - 1];
                    s2i = s2i * cssr[iflag - 1];
                    c1r = csrr[iflag - 1];
                } // for (i = 3; i <= nd; i++)

                return;
            } // group

            if (rs1 > 0.0) {
                nz[0] = -1;

                return;
            } // if (rs1 > 0.0)

            // Set underflow and update parameters
            yr[nd - 1] = 0.0;
            yi[nd - 1] = 0.0;
            nz[0] = nz[0] + 1;
            nd = nd - 1;

            if (nd == 0) {
                return;
            } // if (nd == 0)

            zuoik(zr, zi, fnu, kode, 1, nd, yr, yi, nuf);

            if (nuf[0] < 0) {
                nz[0] = -1;

                return;
            } // if (nuf[0] < 0)

            nd = nd - nuf[0];
            nz[0] = nz[0] + nuf[0];

            if (nd == 0) {
                return;
            } // if (nd == 0)

            fn = fnu + nd - 1.0;

            if (fn < fnul) {
                nlast[0] = nd;

                return;
            } // if (fn < fnul)

            in = inu + nd - 1;
            in = (in % 4) + 1;
            c2r = (car * cipr[in - 1]) - (sar * cipi[in - 1]);
            c2i = (car * cipi[in - 1]) + (sar * cipr[in - 1]);

            if (zi <= 0.0) {
                c2i = -c2i;
            } // if (zi <= 0.0)
        } // while (true)
    }

    /**
     * ZUNIK COMPUTES PARAMETERS FOR THE UNIFORM ASYMPTOTIC EXPANSIONS OF THE I AND K FUNCTIONS ON IKFLG= 1 OR 2
     * RESPECTIVELY BY.
     * 
     * <p>
     * W(fnu,ZR) = PHI*EXP(ZETA)*SUM
     * </p>
     * 
     * <p>
     * WHERE ZETA=-ZETA1 + ZETA2 OR ZETA1 - ZETA2
     * </p>
     * 
     * <p>
     * THE FIRST CALL MUST HAVE INIT=0. SUBSEQUENT CALLS WITH THE SAME ZR AND FNU WILL RETURN THE I OR K FUNCTION ON
     * IKFLG= 1 OR 2 WITH NO CHANGE IN INIT. CWRK IS A COMPLEX WORK ARRAY. IPMTR=0 COMPUTES ALL PARAMETERS. IPMTR=1
     * COMPUTES PHI, ZETA1,ZETA2.
     * </p>
     * 
     * @param zrr double
     * @param zri double
     * @param fnu double
     * @param ikflg int
     * @param ipmtr int
     * @param init int
     * @param phir double[]
     * @param phii double[]
     * @param zeta1r double[]
     * @param zeta1i double[]
     * @param zeta2r double[]
     * @param zeta2i double[]
     * @param sumr double[]
     * @param sumi double[]
     * @param cwrkr double[]
     * @param cwrki double[]
     */
    private void zunik(final double zrr, final double zri, final double fnu, final int ikflg, final int ipmtr,
            int init, final double[] phir, final double[] phii, final double[] zeta1r, final double[] zeta1i,
            final double[] zeta2r, final double[] zeta2i, final double[] sumr, final double[] sumi,
            final double[] cwrkr, final double[] cwrki) {
        final double[] con = new double[] {3.98942280401432678E-01, 1.25331413731550025};
        final double[] c = new double[] {1.00000000000000000E+00, -2.08333333333333333E-01, 1.25000000000000000E-01,
                3.34201388888888889E-01, -4.01041666666666667E-01, 7.03125000000000000E-02, -1.02581259645061728E+00,
                1.84646267361111111E+00, -8.91210937500000000E-01, 7.32421875000000000E-02, 4.66958442342624743E+00,
                -1.12070026162229938E+01, 8.78912353515625000E+00, -2.36408691406250000E+00, 1.12152099609375000E-01,
                -2.82120725582002449E+01, 8.46362176746007346E+01, -9.18182415432400174E+01, 4.25349987453884549E+01,
                -7.36879435947963170E+00, 2.27108001708984375E-01, 2.12570130039217123E+02, -7.65252468141181642E+02,
                1.05999045252799988E+03, -6.99579627376132541E+02, 2.18190511744211590E+02, -2.64914304869515555E+01,
                5.72501420974731445E-01, -1.91945766231840700E+03, 8.06172218173730938E+03, -1.35865500064341374E+04,
                1.16553933368645332E+04, -5.30564697861340311E+03, 1.20090291321635246E+03, -1.08090919788394656E+02,
                1.72772750258445740E+00, 2.02042913309661486E+04, -9.69805983886375135E+04, 1.92547001232531532E+05,
                -2.03400177280415534E+05, 1.22200464983017460E+05, -4.11926549688975513E+04, 7.10951430248936372E+03,
                -4.93915304773088012E+02, 6.07404200127348304E+00, -2.42919187900551333E+05, 1.31176361466297720E+06,
                -2.99801591853810675E+06, 3.76327129765640400E+06, -2.81356322658653411E+06, 1.26836527332162478E+06,
                -3.31645172484563578E+05, 4.52187689813627263E+04, -2.49983048181120962E+03, 2.43805296995560639E+01,
                3.28446985307203782E+06, -1.97068191184322269E+07, 5.09526024926646422E+07, -7.41051482115326577E+07,
                6.63445122747290267E+07, -3.75671766607633513E+07, 1.32887671664218183E+07, -2.78561812808645469E+06,
                3.08186404612662398E+05, -1.38860897537170405E+04, 1.10017140269246738E+02, -4.93292536645099620E+07,
                3.25573074185765749E+08, -9.39462359681578403E+08, 1.55359689957058006E+09, -1.62108055210833708E+09,
                1.10684281682301447E+09, -4.95889784275030309E+08, 1.42062907797533095E+08, -2.44740627257387285E+07,
                2.24376817792244943E+06, -8.40054336030240853E+04, 5.51335896122020586E+02, 8.14789096118312115E+08,
                -5.86648149205184723E+09, 1.86882075092958249E+10, -3.46320433881587779E+10, 4.12801855797539740E+10,
                -3.30265997498007231E+10, 1.79542137311556001E+10, -6.56329379261928433E+09, 1.55927986487925751E+09,
                -2.25105661889415278E+08, 1.73951075539781645E+07, -5.49842327572288687E+05, 3.03809051092238427E+03,
                -1.46792612476956167E+10, 1.14498237732025810E+11, -3.99096175224466498E+11, 8.19218669548577329E+11,
                -1.09837515608122331E+12, 1.00815810686538209E+12, -6.45364869245376503E+11, 2.87900649906150589E+11,
                -8.78670721780232657E+10, 1.76347306068349694E+10, -2.16716498322379509E+09, 1.43157876718888981E+08,
                -3.87183344257261262E+06, 1.82577554742931747E+04, 2.86464035717679043E+11, -2.40629790002850396E+12,
                9.10934118523989896E+12, -2.05168994109344374E+13, 3.05651255199353206E+13, -3.16670885847851584E+13,
                2.33483640445818409E+13, -1.23204913055982872E+13, 4.61272578084913197E+12, -1.19655288019618160E+12,
                2.05914503232410016E+11, -2.18229277575292237E+10, 1.24700929351271032E+09, -2.91883881222208134E+07,
                1.18838426256783253E+05};
        double rfn;
        double test;
        double ac;
        final double[] tr = new double[1];
        final double[] ti = new double[1];
        double sr;
        double si;
        final double[] srr = new double[1];
        final double[] sri = new double[1];
        final double[] str = new double[1];
        final double[] sti = new double[1];
        final double[] znr = new double[1];
        final double[] zni = new double[1];
        int l;
        int k;
        int j;
        final double[] t2r = new double[1];
        final double[] t2i = new double[1];
        final double[] cwrkr15 = new double[1];
        final double[] cwrki15 = new double[1];
        final int[] idum = new int[1];
        double crfnr;
        double crfni;
        int i;

        if (init == 0) {

            // initialize all variables
            rfn = 1.0 / fnu;

            // Overflow test (zr/fnu too small)
            test = 1.0E3 * tiny;
            ac = fnu * test;

            if ( (Math.abs(zrr) <= ac) && (Math.abs(zri) <= ac)) {
                zeta1r[0] = (2.0 * Math.abs(Math.log(test))) + fnu;
                zeta1i[0] = 0.0;
                zeta2r[0] = fnu;
                zeta2i[0] = 0.0;
                phir[0] = 1.0;
                phii[0] = 0.0;

                return;
            } // if ((Math.abs(zrr) <= ac) && (Math.abs(zri) <= ac))

            tr[0] = zrr * rfn;
            ti[0] = zri * rfn;
            sr = 1.0 + ( (tr[0] * tr[0]) - (ti[0] * ti[0]));
            si = 2.0 * tr[0] * ti[0];
            zsqrt(sr, si, srr, sri);
            str[0] = 1.0 + srr[0];
            sti[0] = sri[0];
            zdiv(str[0], sti[0], tr[0], ti[0], znr, zni);
            zlog(znr[0], zni[0], str, sti, idum);
            zeta1r[0] = fnu * str[0];
            zeta1i[0] = fnu * sti[0];
            zeta2r[0] = fnu * srr[0];
            zeta2i[0] = fnu * sri[0];
            zdiv(1.0, 0.0, srr[0], sri[0], tr, ti);
            srr[0] = tr[0] * rfn;
            sri[0] = ti[0] * rfn;
            zsqrt(srr[0], sri[0], cwrkr15, cwrki15);
            cwrkr[15] = cwrkr15[0];
            cwrki[15] = cwrki15[0];
            phir[0] = cwrkr[15] * con[ikflg - 1];
            phii[0] = cwrki[15] * con[ikflg - 1];

            if (ipmtr != 0) {
                return;
            }

            zdiv(1.0, 0.0, sr, si, t2r, t2i);
            cwrkr[0] = 1.0;
            cwrki[0] = 0.0;
            crfnr = 1.0;
            crfni = 0.0;
            ac = 1.0;
            l = 1;

            group: {

                for (k = 2; k <= 15; k++) {
                    sr = 0.0;
                    si = 0.0;

                    for (j = 1; j <= k; j++) {
                        l = l + 1;
                        str[0] = (sr * t2r[0]) - (si * t2i[0]) + c[l - 1];
                        si = (sr * t2i[0]) + (si * t2r[0]);
                        sr = str[0];
                    } // for (j = 1; j <= k; j++)

                    str[0] = (crfnr * srr[0]) - (crfni * sri[0]);
                    crfni = (crfnr * sri[0]) + (crfni * srr[0]);
                    crfnr = str[0];
                    cwrkr[k - 1] = (crfnr * sr) - (crfni * si);
                    cwrki[k - 1] = (crfnr * si) + (crfni * sr);
                    ac = ac * rfn;
                    test = Math.abs(cwrkr[k - 1]) + Math.abs(cwrki[k - 1]);

                    if ( (ac < tol) && (test < tol)) {
                        break group;
                    }
                } // for (k = 2; k <= 15; k++)

                k = 15;
            } // group

            init = k;
        } // if (init == 0)

        if (ikflg != 2) {

            // Compute sum for the I function
            sr = 0.0;
            si = 0.0;

            for (i = 1; i <= init; i++) {
                sr = sr + cwrkr[i - 1];
                si = si + cwrki[i - 1];
            } // for (i = 1; i <= init; i++)

            sumr[0] = sr;
            sumi[0] = si;
            phir[0] = cwrkr[15] * con[0];
            phii[0] = cwrki[15] * con[0];

            return;
        } // if (ikflg != 2)

        // Compute sum for the k function
        sr = 0.0;
        si = 0.0;
        tr[0] = 1.0;

        for (i = 1; i <= init; i++) {
            sr = sr + (tr[0] * cwrkr[i - 1]);
            si = si + (tr[0] * cwrki[i - 1]);
            tr[0] = -tr[0];
        } // for (i = 1; i <= init; i++)

        sumr[0] = sr;
        sumi[0] = si;
        phir[0] = cwrkr[15] * con[1];
        phii[0] = cwrki[15] * con[1];

        return;
    }

    /**
     * ZUNK1 COMPUTES K(FNU,Z) AND ITS ANALYTIC CONTINUATION FROM THE RIGHT HALF PLANE TO THE LEFT HALF PLANE BY MEANS
     * OF THE UNIFORM ASYMPTOTIC EXPANSION. MR INDICATES THE DIRECTION OF ROTATION FOR ANALYTIC CONTINUATION. NZ=-1
     * MEANS AN OVERFLOW WILL OCCUR
     * 
     * @param zr double
     * @param zi double
     * @param fnu double
     * @param kode int
     * @param mr int
     * @param n int
     * @param yr double[]
     * @param yi double[]
     * @param nz int[]
     */
    private void zunk1(double zr, double zi, final double fnu, final int kode, final int mr, final int n,
            final double[] yr, final double[] yi, final int[] nz) {
        final double[] bry = new double[3];
        final int[] init = new int[2];
        final double[] sumr = new double[2];
        final double[] sumi = new double[2];
        final double[] zeta1r = new double[2];
        final double[] zeta1i = new double[2];
        final double[] zeta2r = new double[2];
        final double[] zeta2i = new double[2];
        final double[] cyr = new double[2];
        final double[] cyi = new double[2];
        final double[][] cwrkr = new double[16][3];
        final double[][] cwrki = new double[16][3];
        final double[] cssr = new double[3];
        final double[] csrr = new double[3];
        final double[] phir = new double[2];
        final double[] phii = new double[2];
        double cscl;
        double crsc;
        double zrr;
        double zri;
        int j;
        int i;
        double fn = 0.0;
        final double[] phirj = new double[1];
        final double[] phiij = new double[1];
        final double[] zeta1rj = new double[1];
        final double[] zeta1ij = new double[1];
        final double[] zeta2rj = new double[1];
        final double[] zeta2ij = new double[1];
        final double[] sumrj = new double[1];
        final double[] sumij = new double[1];
        final double[] cwrkrj = new double[16];
        final double[] cwrkij = new double[16];
        double str;
        double sti;
        final double[] s1r = new double[1];
        final double[] s1i = new double[1];
        double rast;
        double rs1;
        int kdflg;
        int kflag = 0;
        final double[] s2r = new double[1];
        final double[] s2i = new double[1];
        final int[] nw = new int[1];
        double razr;
        double rzr;
        double rzi;
        double ckr;
        double cki;
        int ib;
        int ipard;
        int initd = 0;
        final double[] phidr = new double[1];
        final double[] phidi = new double[1];
        final double[] zet1dr = new double[1];
        final double[] zet1di = new double[1];
        final double[] zet2dr = new double[1];
        final double[] zet2di = new double[1];
        final double[] sumdr = new double[1];
        final double[] sumdi = new double[1];
        final double[] cwrkr3 = new double[16];
        final double[] cwrki3 = new double[16];
        double aphi;
        final double[] c1r = new double[1];
        final double[] c1i = new double[1];
        double ascle;
        final double[] c2r = new double[1];
        final double[] c2i = new double[1];
        double c2m;
        double fmr;
        double sgn;
        double csgni;
        int inu;
        double fnf;
        int ifn;
        double ang;
        double cspnr;
        double cspni;
        double asc;
        final int[] iuf = new int[1];
        int kk;
        int ic;
        final double[] cwrkrm = new double[16];
        final double[] cwrkim = new double[16];
        int iflag = 0;
        int il;
        double csr;
        boolean seg1 = true;
        boolean seg2 = true;
        boolean seg3 = true;
        boolean seg4 = true;
        boolean seg5 = true;
        boolean seg6 = true;
        boolean seg7 = true;
        boolean seg8 = true;
        boolean seg9 = true;
        boolean seg10 = true;
        int k;
        int m;
        int p;

        kdflg = 1;
        nz[0] = 0;

        // exp(-alim) = exp(-elim)/tol = approximately one precision greater
        // than the underflow limit
        cscl = 1.0 / tol;
        crsc = tol;
        cssr[0] = cscl;
        cssr[1] = 1.0;
        cssr[2] = crsc;
        csrr[0] = crsc;
        csrr[1] = 1.0;
        csrr[2] = cscl;
        bry[0] = 1.0E3 * tiny / tol;
        bry[1] = 1.0 / bry[0];
        bry[2] = Double.MAX_VALUE;
        zrr = zr;
        zri = zi;

        if (zr < 0.0) {
            zrr = -zr;
            zri = -zi;
        } // if (zr < 0.0)

        j = 2;

        group: {

            for (i = 1; i <= n; i++) {

                // j flip flops between 1 and 2 in j = 3 - j
                j = 3 - j;
                fn = fnu + i - 1.0;
                init[j - 1] = 0;
                zunik(zrr, zri, fn, 2, 0, init[j - 1], phirj, phiij, zeta1rj, zeta1ij, zeta2rj, zeta2ij, sumrj, sumij,
                        cwrkrj, cwrkij);
                phir[j - 1] = phirj[0];
                phii[j - 1] = phiij[0];
                zeta1r[j - 1] = zeta1rj[0];
                zeta1i[j - 1] = zeta1ij[0];
                zeta2r[j - 1] = zeta2rj[0];
                zeta2i[j - 1] = zeta2ij[0];
                sumr[j - 1] = sumrj[0];
                sumi[j - 1] = sumij[0];

                for (p = 0; p < 16; p++) {
                    cwrkr[p][j - 1] = cwrkrj[p];
                    cwrki[p][j - 1] = cwrkij[p];
                }

                if (kode != 1) {
                    str = zrr + zeta2r[j - 1];
                    sti = zri + zeta2i[j - 1];
                    rast = fn / zabs(str, sti);
                    str = str * rast * rast;
                    sti = -sti * rast * rast;
                    s1r[0] = zeta1r[j - 1] - str;
                    s1i[0] = zeta1i[j - 1] - sti;
                } // if (kode != 1)
                else { // kode == 1
                    s1r[0] = zeta1r[j - 1] - zeta2r[j - 1];
                    s1i[0] = zeta1i[j - 1] - zeta2i[j - 1];
                } // else kode == 1

                rs1 = s1r[0];

                // Test for underflow and overflow
                if (Math.abs(rs1) <= elim) {

                    if (kdflg == 1) {
                        kflag = 2;
                    } // if (kdflg == 1)

                    if (Math.abs(rs1) >= alim) {

                        // Refine test and scale
                        aphi = zabs(phir[j - 1], phii[j - 1]);
                        rs1 = rs1 + Math.log(aphi);

                        if (Math.abs(rs1) > elim) {
                            seg1 = false;
                            seg2 = false;
                        } // if (Math.abs(rs1) > elim)

                        if (seg1) {

                            if (kdflg == 1) {
                                kflag = 1;
                            } // if (kdflg == 1)

                            if ( (rs1 >= 0.0) && (kdflg == 1)) {
                                kflag = 3;
                            } // if ((rs1 >= 0.0) && (kdflg == 1))
                        } // if (seg1)

                        seg1 = true;
                    } // if (Math.abs(rs1) >= alim)

                    if (seg2) {

                        // Scale s1 to keep intermediate arithmetic on scale near
                        // exponent extremes
                        s2r[0] = (phir[j - 1] * sumr[j - 1]) - (phii[j - 1] * sumi[j - 1]);
                        s2i[0] = (phir[j - 1] * sumi[j - 1]) + (phii[j - 1] * sumr[j - 1]);
                        str = Math.exp(s1r[0]) * cssr[kflag - 1];
                        s1r[0] = str * Math.cos(s1i[0]);
                        s1i[0] = str * Math.sin(s1i[0]);
                        str = (s2r[0] * s1r[0]) - (s2i[0] * s1i[0]);
                        s2i[0] = (s1r[0] * s2i[0]) + (s2r[0] * s1i[0]);
                        s2r[0] = str;

                        if (kflag == 1) {
                            zuchk(s2r[0], s2i[0], nw, bry[0]);

                            if (nw[0] != 0) {
                                seg3 = false;
                            } // if (nw[0] != 0)
                        } // if (kflag == 1)

                        if (seg3) {
                            cyr[kdflg - 1] = s2r[0];
                            cyi[kdflg - 1] = s2i[0];
                            yr[i - 1] = s2r[0] * csrr[kflag - 1];
                            yi[i - 1] = s2i[0] * csrr[kflag - 1];

                            if (kdflg == 2) {
                                break group;
                            } // if (kdflg == 2)

                            kdflg = 2;

                            continue;
                        } // if (seg3)

                        seg3 = true;
                    } // if (seg2)

                    seg2 = true;
                } // if (Math.abs(rs1) <= elim)

                if (rs1 > 0.0) {
                    nz[0] = -1;

                    return;
                } // if (rs1 > 0.0)

                // For zr < 0.0, the I function to be added will overflow
                if (zr < 0.0) {
                    nz[0] = -1;

                    return;
                } // if (zr < 0.0)

                kdflg = 1;
                yr[i - 1] = 0.0;
                yi[i - 1] = 0.0;
                nz[0] = nz[0] + 1;

                if (i == 1) {
                    continue;
                } // if (i == 1)

                if ( (yr[i - 2] == 0.0) && (yi[i - 2] == 0.0)) {
                    continue;
                } // if ((yr[i-2] == 0.0) && (yi[i-2] == 0.0))

                yr[i - 2] = 0.0;
                yi[i - 2] = 0.0;
                nz[0] = nz[0] + 1;
            } // for (i = 1; i <= n; i++)

            i = n;
        } // group

        razr = 1.0 / zabs(zrr, zri);
        str = zrr * razr;
        sti = -zri * razr;
        rzr = (str + str) * razr;
        rzi = (sti + sti) * razr;
        ckr = fn * rzr;
        cki = fn * rzi;
        ib = i + 1;

        if (n >= ib) {

            // test last member for underflow and overflow. Set sequence to zero
            // on underflow
            fn = fnu + n - 1.0;
            ipard = 1;

            if (mr != 0) {
                ipard = 0;
            } // if (mr != 0)

            initd = 0;
            zunik(zrr, zri, fn, 2, ipard, initd, phidr, phidi, zet1dr, zet1di, zet2dr, zet2di, sumdr, sumdi, cwrkr3,
                    cwrki3);

            for (p = 0; p < 16; p++) {
                cwrkr[p][2] = cwrkr3[p];
                cwrki[p][2] = cwrki3[p];
            }

            if (kode != 1) {
                str = zrr + zet2dr[0];
                sti = zri + zet2di[0];
                rast = fn / zabs(str, sti);
                str = str * rast * rast;
                sti = -sti * rast * rast;
                s1r[0] = zet1dr[0] - str;
                s1i[0] = zet1di[0] - sti;
            } // if (kode != 1)
            else { // kode == 1
                s1r[0] = zet1dr[0] - zet2dr[0];
                s1i[0] = zet1di[0] - zet2di[0];
            } // else kode == 1

            rs1 = s1r[0];

            if (Math.abs(rs1) <= elim) {

                if (Math.abs(rs1) < alim) {
                    seg4 = false;
                    seg5 = false;
                } // if (Math.abs(rs1) < alim)

                if (seg4) {

                    // Refine estimate and test
                    aphi = zabs(phidr[0], phidi[0]);
                    rs1 = rs1 + Math.log(aphi);

                    if (Math.abs(rs1) < elim) {
                        seg5 = false;
                    } // if (Math.abs(rs1) < elim)
                } // if (seg4)

                seg4 = true;
            } // if (Math.abs(rs1) <= elim)

            if (seg5) {

                if (Math.abs(rs1) > 0.0) {
                    nz[0] = -1;

                    return;
                } // if (Math.abs(rs1) > 0.0)

                // For zr < 0.0, the I function to be added will overflow
                if (zr < 0.0) {
                    nz[0] = -1;

                    return;
                } // if (zr < 0.0)

                nz[0] = n;

                for (i = 1; i <= n; i++) {
                    yr[i - 1] = 0.0;
                    yi[i - 1] = 0.0;
                } // for (i = 1; i <= n; i++)

                return;
            } // if (seg5)

            seg5 = true;

            // Forward recur for remainder of the sequence
            s1r[0] = cyr[0];
            s1i[0] = cyi[0];
            s2r[0] = cyr[1];
            s2i[0] = cyi[1];
            c1r[0] = csrr[kflag - 1];
            ascle = bry[kflag - 1];

            for (i = ib; i <= n; i++) {
                c2r[0] = s2r[0];
                c2i[0] = s2i[0];
                s2r[0] = (ckr * c2r[0]) - (cki * c2i[0]) + s1r[0];
                s2i[0] = (ckr * c2i[0]) + (cki * c2r[0]) + s1i[0];
                s1r[0] = c2r[0];
                s1i[0] = c2i[0];
                ckr = ckr + rzr;
                cki = cki + rzi;
                c2r[0] = s2r[0] * c1r[0];
                c2i[0] = s2i[0] * c1r[0];
                yr[i - 1] = c2r[0];
                yi[i - 1] = c2i[0];

                if (kflag >= 3) {
                    continue;
                } // if (kflag >= 3)

                str = Math.abs(c2r[0]);
                sti = Math.abs(c2i[0]);
                c2m = Math.max(str, sti);

                if (c2m <= ascle) {
                    continue;
                } // if (c2m <= ascle)

                kflag = kflag + 1;
                ascle = bry[kflag - 1];
                s1r[0] = s1r[0] * c1r[0];
                s1i[0] = s1i[0] * c1r[0];
                s2r[0] = c2r[0];
                s2i[0] = c2i[0];
                s1r[0] = s1r[0] * cssr[kflag - 1];
                s1i[0] = s1i[0] * cssr[kflag - 1];
                s2r[0] = s2r[0] * cssr[kflag - 1];
                s2i[0] = s2i[0] * cssr[kflag - 1];
                c1r[0] = csrr[kflag - 1];
            } // for (i = ib; i <= n; i++)
        } // if (n >= ib)

        if (mr == 0) {
            return;
        } // if (mr == 0)

        // Analytic continuation for Re(z) < 0.0
        nz[0] = 0;
        fmr = mr;

        if (fmr >= 0.0) {
            sgn = -Math.PI;
        } else {
            sgn = Math.PI;
        }

        // cspn and csgn are coeff of K and I function resp.
        csgni = sgn;
        inu = (int) (fnu);
        fnf = fnu - inu;
        ifn = inu + n - 1;
        ang = fnf * sgn;
        cspnr = Math.cos(ang);
        cspni = Math.sin(ang);

        if ( (ifn % 2) != 0) {
            cspnr = -cspnr;
            cspni = -cspni;
        } // if ((ifn%2) != 0)

        asc = bry[0];
        iuf[0] = 0;
        kk = n;
        kdflg = 1;
        ib = ib - 1;
        ic = ib - 1;

        group2: {

            for (k = 1; k <= n; k++) {
                fn = fnu + kk - 1.0;

                // Logic to sort out cases whose parameters were set for the K
                // function above
                m = 3;

                if (n > 2) {
                    seg6 = false;
                } // if (n > 2)

                while (true) {

                    if (seg6) {
                        initd = init[j - 1];
                        phidr[0] = phir[j - 1];
                        phidi[0] = phii[j - 1];
                        zet1dr[0] = zeta1r[j - 1];
                        zet1di[0] = zeta1i[j - 1];
                        zet2dr[0] = zeta2r[j - 1];
                        zet2di[0] = zeta2i[j - 1];
                        sumdr[0] = sumr[j - 1];
                        sumdi[0] = sumi[j - 1];
                        m = j;
                        j = 3 - j;

                        break;
                    } // if (seg6)

                    seg6 = true;

                    if ( (kk == n) && (ib < n)) {
                        break;
                    } // if ((kk == n) && (ib < n))

                    if ( (kk == ib) || (kk == ic)) {
                        continue;
                    } // if ((kk == ib) || (kk == ic))

                    initd = 0;

                    break;
                } // while (true)

                zunik(zrr, zri, fn, 1, 0, initd, phidr, phidi, zet1dr, zet1di, zet2dr, zet2di, sumdr, sumdi, cwrkrm,
                        cwrkim);

                for (p = 0; p < 16; p++) {
                    cwrkr[p][m - 1] = cwrkrm[p];
                    cwrki[p][m - 1] = cwrkim[p];
                }

                if (kode != 1) {
                    str = zrr + zet2dr[0];
                    sti = zri + zet2di[0];
                    rast = fn / zabs(str, sti);
                    str = str * rast * rast;
                    sti = -sti * rast * rast;
                    s1r[0] = -zet1dr[0] + str;
                    s1i[0] = -zet1di[0] + sti;
                } // if (kode != 1)
                else { // kode == 1
                    s1r[0] = -zet1dr[0] + zet2dr[0];
                    s1i[0] = -zet1di[0] + zet2di[0];
                } // else kode == 1

                // Test for underflow and overflow
                rs1 = s1r[0];

                if (Math.abs(rs1) > elim) {
                    seg7 = false;
                    seg9 = false;
                    seg10 = false;
                } // if (Math.abs(rs1) > elim)

                if (seg7) {

                    if (kdflg == 1) {
                        iflag = 2;
                    } // if (kdflg == 1)

                    if (Math.abs(rs1) >= alim) {

                        // Refine test and scale
                        aphi = zabs(phidr[0], phidi[0]);
                        rs1 = rs1 + Math.log(aphi);

                        if (Math.abs(rs1) > elim) {
                            seg8 = false;
                            seg9 = false;
                            seg10 = false;
                        } // if (Math.abs(rs1) > elim)

                        if (seg8) {

                            if (kdflg == 1) {
                                iflag = 1;
                            } // if (kdflg == 1)

                            if ( (rs1 >= 0.0) && (kdflg == 1)) {
                                iflag = 3;
                            } // if ((rs1 >= 0.0) && (kdflg == 1))
                        } // if (seg8)

                        seg8 = true;
                    } // if (Math.abs(rs1) >= alim)
                } // if (seg7)

                seg7 = true;

                if (seg9) {
                    str = (phidr[0] * sumdr[0]) - (phidi[0] * sumdi[0]);
                    sti = (phidr[0] * sumdi[0]) + (phidi[0] * sumdr[0]);
                    s2r[0] = -csgni * sti;
                    s2i[0] = csgni * str;
                    str = Math.exp(s1r[0]) * cssr[iflag - 1];
                    s1r[0] = str * Math.cos(s1i[0]);
                    s1i[0] = str * Math.sin(s1i[0]);
                    str = (s2r[0] * s1r[0]) - (s2i[0] * s1i[0]);
                    s2i[0] = (s2r[0] * s1i[0]) + (s2i[0] * s1r[0]);
                    s2r[0] = str;

                    if (iflag == 1) {
                        zuchk(s2r[0], s2i[0], nw, bry[0]);

                        if (nw[0] != 0) {
                            s2r[0] = 0.0;
                            s2i[0] = 0.0;
                        } // if (nw[0] != 0)
                    } // if (iflag == 1)
                } // if (seg9)

                seg9 = true;

                while (true) {

                    if (seg10) {
                        cyr[kdflg - 1] = s2r[0];
                        cyi[kdflg - 1] = s2i[0];
                        c2r[0] = s2r[0];
                        c2i[0] = s2i[0];
                        s2r[0] = s2r[0] * csrr[iflag - 1];
                        s2i[0] = s2i[0] * csrr[iflag - 1];

                        // Add I and K functions, K sequence in Y(i), i = 1,n
                        s1r[0] = yr[kk - 1];
                        s1i[0] = yi[kk - 1];

                        if (kode != 1) {
                            zs1s2(zrr, zri, s1r, s1i, s2r, s2i, nw, asc, iuf);
                            nz[0] = nz[0] + nw[0];
                        } // if (kode != 1)

                        yr[kk - 1] = (s1r[0] * cspnr) - (s1i[0] * cspni) + s2r[0];
                        yi[kk - 1] = (cspnr * s1i[0]) + (cspni * s1r[0]) + s2i[0];
                        kk = kk - 1;
                        cspnr = -cspnr;
                        cspni = -cspni;

                        if ( (c2r[0] == 0.0) && (c2i[0] == 0.0)) {
                            kdflg = 1;

                            break;
                        } // if (c2r[0] == 0.0) && (c2i[0] == 0.0))

                        if (kdflg == 2) {
                            break group2;
                        } // if (kdflg = 2)

                        kdflg = 2;

                        break;
                    } // if (seg10)

                    seg10 = true;

                    if (rs1 > 0.0) {
                        nz[0] = -1;

                        return;
                    } // if (rs1 > 0.0)

                    s2r[0] = 0.0;
                    s2i[0] = 0.0;
                } // while (true)
            } // for (k = 1; k <= n; k++)

            k = n;
        } // group2

        il = n - k;

        if (il == 0) {
            return;
        } // if (il == 0)

        // Recur backward for remainder of I sequence and add in the K functions,
        // scaling the I sequence during recurrence to keep intermediate
        // arithmetic on scale near exponent extremes
        s1r[0] = cyr[0];
        s1i[0] = cyi[0];
        s2r[0] = cyr[1];
        s2i[0] = cyi[1];
        csr = csrr[iflag - 1];
        ascle = bry[iflag - 1];
        fn = inu + il;

        for (i = 1; i <= il; i++) {
            c2r[0] = s2r[0];
            c2i[0] = s2i[0];
            s2r[0] = s1r[0] + ( (fn + fnf) * ( (rzr * c2r[0]) - (rzi * c2i[0])));
            s2i[0] = s1i[0] + ( (fn + fnf) * ( (rzr * c2i[0]) + (rzi * c2r[0])));
            s1r[0] = c2r[0];
            s1i[0] = c2i[0];
            fn = fn - 1.0;
            c2r[0] = s2r[0] * csr;
            c2i[0] = s2i[0] * csr;
            ckr = c2r[0];
            cki = c2i[0];
            c1r[0] = yr[kk - 1];
            c1i[0] = yi[kk - 1];

            if (kode != 1) {
                zs1s2(zrr, zri, c1r, c1i, c2r, c2i, nw, asc, iuf);
                nz[0] = nz[0] + nw[0];
            } // if (kode != 1)

            yr[kk - 1] = (c1r[0] * cspnr) - (c1i[0] * cspni) + c2r[0];
            yi[kk - 1] = (c1r[0] * cspni) + (c1i[0] * cspnr) + c2i[0];
            kk = kk - 1;
            cspnr = -cspnr;
            cspni = -cspni;

            if (iflag >= 3) {
                continue;
            } // if (iflag >= 3)

            c2r[0] = Math.abs(ckr);
            c2i[0] = Math.abs(cki);
            c2m = Math.max(c2r[0], c2i[0]);

            if (c2m <= ascle) {
                continue;
            } // if (c2m <= ascle)

            iflag = iflag + 1;
            ascle = bry[iflag - 1];
            s1r[0] = s1r[0] * csr;
            s1i[0] = s1i[0] * csr;
            s2r[0] = ckr;
            s2i[0] = cki;
            s1r[0] = s1r[0] * cssr[iflag - 1];
            s1i[0] = s1i[0] * cssr[iflag - 1];
            s2r[0] = s2r[0] * cssr[iflag - 1];
            s2i[0] = s2i[0] * cssr[iflag - 1];
            csr = csrr[iflag - 1];
        } // for (i = 1; i <= il; i++)

        return;
    }

    /**
     * ZUNK2 COMPUTES K(FNU,Z) AND ITS ANALYTIC CONTINUATION FROM THE RIGHT HALF PLANE TO THE LEFT HALF PLANE BY MEANS
     * OF THE UNIFORM ASYMPTOTIC EXPANSIONS FOR H(KIND,FNU,ZN) AND J(FNU,ZN) WHERE ZN IS IN THE RIGHT HALF PLANE,
     * KIND=(3-MR)/2, MR=+1 OR -1. HERE ZN=ZR*I OR -ZR*I WHERE ZR=Z IF Z IS IN THE RIGHT HALF PLANE OR ZR=-Z IF Z IS IN
     * THE LEFT HALF PLANE. MR INDIC- ATES THE DIRECTION OF ROTATION FOR ANALYTIC CONTINUATION. NZ=-1 MEANS AN OVERFLOW
     * WILL OCCUR
     * 
     * @param zr double
     * @param zi double
     * @param fnu double
     * @param kode int
     * @param mr int
     * @param n int
     * @param yr double[]
     * @param yi double[]
     * @param nz int[]
     */
    private void zunk2(double zr, double zi, final double fnu, final int kode, final int mr, final int n,
            final double[] yr, final double[] yi, final int[] nz) {
        final double cr1r = 1.0;
        final double cr1i = 1.73205080756887729;
        final double cr2r = -0.5;
        final double cr2i = -8.66025403784438647E-01;
        double hpi = Math.PI / 2.0;
        final double aic = 1.26551212348464539;
        final double[] cipr = new double[] {1.0, 0.0, -1.0, 0.0};
        final double[] cipi = new double[] {0.0, -1.0, 0.0, 1.0};
        final double[] bry = new double[3];
        final double[] asumr = new double[2];
        final double[] asumi = new double[2];
        final double[] bsumr = new double[2];
        final double[] bsumi = new double[2];
        final double[] phir = new double[2];
        final double[] phii = new double[2];
        final double[] argr = new double[2];
        final double[] argi = new double[2];
        final double[] zeta1r = new double[2];
        final double[] zeta1i = new double[2];
        final double[] zeta2r = new double[2];
        final double[] zeta2i = new double[2];
        final double[] cyr = new double[2];
        final double[] cyi = new double[2];
        final double[] cssr = new double[3];
        final double[] csrr = new double[3];
        int kdflg;
        double cscl;
        double crsc;
        double zrr;
        double zri;
        double yy;
        double znr;
        double zni;
        double zbr;
        double zbi;
        int inu;
        double fnf;
        double ang;
        double car;
        double sar;
        final double[] c2r = new double[1];
        final double[] c2i = new double[1];
        int kk;
        double str;
        double sti;
        double csr;
        double csi;
        int j;
        int i;
        double fn = 0.0;
        final double[] phirj = new double[1];
        final double[] phiij = new double[1];
        final double[] argrj = new double[1];
        final double[] argij = new double[1];
        final double[] zeta1rj = new double[1];
        final double[] zeta1ij = new double[1];
        final double[] zeta2rj = new double[1];
        final double[] zeta2ij = new double[1];
        final double[] asumrj = new double[1];
        final double[] asumij = new double[1];
        final double[] bsumrj = new double[1];
        final double[] bsumij = new double[1];
        double rast;
        final double[] s1r = new double[1];
        final double[] s1i = new double[1];
        double rs1;
        int kflag = 0;
        double aphi;
        double aarg;
        final double[] air = new double[1];
        final double[] aii = new double[1];
        final int[] nai = new int[1];
        final int[] idum = new int[1];
        final double[] dair = new double[1];
        final double[] daii = new double[1];
        final int[] ndai = new int[1];
        double ptr;
        double pti;
        final double[] s2r = new double[1];
        final double[] s2i = new double[1];
        final int[] nw = new int[1];
        double razr;
        double rzr;
        double rzi;
        double ckr;
        double cki;
        int ib;
        int ipard;
        final double[] phidr = new double[1];
        final double[] phidi = new double[1];
        final double[] argdr = new double[1];
        final double[] argdi = new double[1];
        final double[] zet1dr = new double[1];
        final double[] zet1di = new double[1];
        final double[] zet2dr = new double[1];
        final double[] zet2di = new double[1];
        final double[] asumdr = new double[1];
        final double[] asumdi = new double[1];
        final double[] bsumdr = new double[1];
        final double[] bsumdi = new double[1];
        double c2m;
        double fmr;
        double sgn;
        double csgni;
        int ifn;
        double cspnr;
        double cspni;
        int in;
        double asc;
        int ic;
        int iflag = 0;
        int il;
        final double[] c1r = new double[1];
        final double[] c1i = new double[1];
        double ascle;
        final int[] iuf = new int[1];
        int k;
        boolean seg1 = true;
        boolean seg2 = true;
        boolean seg3 = true;
        boolean seg4 = true;
        boolean seg5 = true;
        boolean seg6 = true;
        boolean seg7 = true;
        boolean seg8 = true;
        boolean seg9 = true;
        boolean seg10 = true;

        kdflg = 1;
        nz[0] = 0;

        // exp(-alim) = exp(-elim)/tol = approximately one precision greater
        // than the underflow limit
        cscl = 1.0 / tol;
        crsc = tol;
        cssr[0] = cscl;
        cssr[1] = 1.0;
        cssr[2] = crsc;
        csrr[0] = crsc;
        csrr[1] = 1.0;
        csrr[2] = cscl;
        bry[0] = 1.0E3 * tiny / tol;
        bry[1] = 1.0 / bry[0];
        bry[2] = Double.MAX_VALUE;
        zrr = zr;
        zri = zi;

        if (zr < 0.0) {
            zrr = -zr;
            zri = -zi;
        } // if (zr < 0.0)

        yy = zri;
        znr = zri;
        zni = -zrr;
        zbr = zrr;
        zbi = zri;
        inu = (int) (fnu);
        fnf = fnu - inu;
        ang = -hpi * fnf;
        car = Math.cos(ang);
        sar = Math.sin(ang);
        c2r[0] = hpi * sar;
        c2i[0] = -hpi * car;
        kk = (inu % 4) + 1;
        str = (c2r[0] * cipr[kk - 1]) - (c2i[0] * cipi[kk - 1]);
        sti = (c2r[0] * cipi[kk - 1]) + (c2i[0] * cipr[kk - 1]);
        csr = (cr1r * str) - (cr1i * sti);
        csi = (cr1r * sti) + (cr1i * str);

        if (yy <= 0.0) {
            znr = -znr;
            zbi = -zbi;
        } // if (yy <= 0.0)

        // K(fnu,z) is computed form H(2,fnu,-i*z) where z is in the first
        // quadrant. Fourth quadrant values (yy <= 0.0) are computed by
        // conjugation since the K function is real on the positive real axis.
        j = 2;

        group: {

            for (i = 1; i <= n; i++) {

                // j flip flops between 1 and 2 in j = 3 - j
                j = 3 - j;
                fn = fnu + i - 1.0;
                zunhj(znr, zni, fn, 0, phirj, phiij, argrj, argij, zeta1rj, zeta1ij, zeta2rj, zeta2ij, asumrj, asumij,
                        bsumrj, bsumij);
                phir[j - 1] = phirj[0];
                phii[j - 1] = phiij[0];
                argr[j - 1] = argrj[0];
                argi[j - 1] = argij[0];
                zeta1r[j - 1] = zeta1rj[0];
                zeta1i[j - 1] = zeta1ij[0];
                zeta2r[j - 1] = zeta2rj[0];
                zeta2i[j - 1] = zeta2ij[0];
                asumr[j - 1] = asumrj[0];
                asumi[j - 1] = asumij[0];
                bsumr[j - 1] = bsumrj[0];
                bsumi[j - 1] = bsumij[0];

                if (kode != 1) {
                    str = zbr + zeta2r[j - 1];
                    sti = zbi + zeta2i[j - 1];
                    rast = fn / zabs(str, sti);
                    str = str * rast * rast;
                    sti = -sti * rast * rast;
                    s1r[0] = zeta1r[j - 1] - str;
                    s1i[0] = zeta1i[j - 1] - sti;
                } // if (kode != 1)
                else { // kode == 1
                    s1r[0] = zeta1r[j - 1] - zeta2r[j - 1];
                    s1i[0] = zeta1i[j - 1] - zeta2i[j - 1];
                } // else kode == 1

                // Test for underflow and overflow
                rs1 = s1r[0];

                if (Math.abs(rs1) <= elim) {

                    if (kdflg == 1) {
                        kflag = 2;
                    } // if (kdflg == 1)

                    if (Math.abs(rs1) >= alim) {

                        // Refine test and scale
                        aphi = zabs(phir[j - 1], phii[j - 1]);
                        aarg = zabs(argr[j - 1], argi[j - 1]);
                        rs1 = rs1 + Math.log(aphi) - (0.25 * Math.log(aarg)) - aic;

                        if (Math.abs(rs1) > elim) {
                            seg1 = false;
                            seg2 = false;
                        } // if (Math.abs(rs1) > elim)

                        if (seg1) {

                            if (kdflg == 1) {
                                kflag = 1;
                            } // if (kdflg == 1)

                            if ( (rs1 >= 0.0) && (kdflg == 1)) {
                                kflag = 3;
                            } // if ((rs1 >= 0.0) && (kdflg == 1))
                        } // if (seg1)

                        seg1 = true;
                    } // if (Math.abs(rs1) >= alim)

                    if (seg2) {

                        // Scale s1 to keep intermediate arithmetic on scale near
                        // exponent extremes
                        c2r[0] = (argr[j - 1] * cr2r) - (argi[j - 1] * cr2i);
                        c2i[0] = (argr[j - 1] * cr2i) + (argi[j - 1] * cr2r);
                        zairy(c2r[0], c2i[0], 0, 2, air, aii, nai, idum);
                        zairy(c2r[0], c2i[0], 1, 2, dair, daii, ndai, idum);
                        str = (dair[0] * bsumr[j - 1]) - (daii[0] * bsumi[j - 1]);
                        sti = (dair[0] * bsumi[j - 1]) + (daii[0] * bsumr[j - 1]);
                        ptr = (str * cr2r) - (sti * cr2i);
                        pti = (str * cr2i) + (sti * cr2r);
                        str = ptr + ( (air[0] * asumr[j - 1]) - (aii[0] * asumi[j - 1]));
                        sti = pti + ( (air[0] * asumi[j - 1]) + (aii[0] * asumr[j - 1]));
                        ptr = (str * phir[j - 1]) - (sti * phii[j - 1]);
                        pti = (str * phii[j - 1]) + (sti * phir[j - 1]);
                        s2r[0] = (ptr * csr) - (pti * csi);
                        s2i[0] = (ptr * csi) + (pti * csr);
                        str = Math.exp(s1r[0]) * cssr[kflag - 1];
                        s1r[0] = str * Math.cos(s1i[0]);
                        s1i[0] = str * Math.sin(s1i[0]);
                        str = (s2r[0] * s1r[0]) - (s2i[0] * s1i[0]);
                        s2i[0] = (s1r[0] * s2i[0]) + (s2r[0] * s1i[0]);
                        s2r[0] = str;

                        if (kflag == 1) {
                            zuchk(s2r[0], s2i[0], nw, bry[0]);

                            if (nw[0] != 0) {
                                seg3 = false;
                            } // if (nw[0] != 0)
                        } // if (kflag == 1)

                        if (seg3) {

                            if (yy <= 0.0) {
                                s2i[0] = -s2i[0];
                            } // if (yy <= 0.0)

                            cyr[kdflg - 1] = s2r[0];
                            cyi[kdflg - 1] = s2i[0];
                            yr[i - 1] = s2r[0] * csrr[kflag - 1];
                            yi[i - 1] = s2i[0] * csrr[kflag - 1];
                            str = csi;
                            csi = -csr;
                            csr = str;

                            if (kdflg == 2) {
                                break group;
                            } // if (kdflg == 2)

                            kdflg = 2;

                            continue;
                        } // if (seg3)

                        seg3 = true;
                    } // if (seg2)

                    seg2 = true;
                } // if (Math.abs(rs1) <= elim)

                if (rs1 > 0.0) {
                    nz[0] = -1;

                    return;
                } // if (rs1 > 0.0)

                // For zr < 0.0, the I function to be added will overflow
                if (zr < 0.0) {
                    nz[0] = -1;

                    return;
                } // if (zr < 0.0)

                kdflg = 1;
                yr[i - 1] = 0.0;
                yi[i - 1] = 0.0;
                nz[0] = nz[0] + 1;
                str = csi;
                csi = -csr;
                csr = str;

                if (i == 1) {
                    continue;
                } // if (i == 1)

                if ( (yr[i - 2] == 0.0) && (yi[i - 2] == 0.0)) {
                    continue;
                } // if ((yr[i-2] == 0.0) && (yi[i-2] == 0.0))

                yr[i - 2] = 0.0;
                yi[i - 2] = 0.0;
                nz[0] = nz[0] + 1;
            } // for (i = 1; i <= n; i++)

            i = n;
        } // group

        razr = 1.0 / zabs(zrr, zri);
        str = zrr * razr;
        sti = -zri * razr;
        rzr = (str + str) * razr;
        rzi = (sti + sti) * razr;
        ckr = fn * rzr;
        cki = fn * rzi;
        ib = i + 1;

        if (n >= ib) {

            // Test last member for underflow and overflow. Set sequence to zero
            // on underflow
            fn = fnu + n - 1.0;
            ipard = 1;

            if (mr != 0) {
                ipard = 0;
            } // if (mr != 0)

            zunhj(znr, zni, fn, ipard, phidr, phidi, argdr, argdi, zet1dr, zet1di, zet2dr, zet2di, asumdr, asumdi,
                    bsumdr, bsumdi);

            if (kode != 1) {
                str = zbr + zet2dr[0];
                sti = zbi + zet2di[0];
                rast = fn / zabs(str, sti);
                str = str * rast * rast;
                sti = -sti * rast * rast;
                s1r[0] = zet1dr[0] - str;
                s1i[0] = zet1di[0] - sti;
            } // if (kode != 1)
            else { // kode == 1
                s1r[0] = zet1dr[0] - zet2dr[0];
                s1i[0] = zet1di[0] - zet2di[0];
            } // else kode == 1

            rs1 = s1r[0];

            if (Math.abs(rs1) <= elim) {

                if (Math.abs(rs1) < alim) {
                    seg4 = false;
                    seg5 = false;
                } // if (Math.abs(rs1) < alim)

                if (seg4) {

                    // Refine estimate and test
                    aphi = zabs(phidr[0], phidi[0]);
                    rs1 = rs1 + Math.log(aphi);

                    if (Math.abs(rs1) < elim) {
                        seg5 = false;
                    } // if (Math.abs(rs1) < elim)
                } // if (seg4)

                seg4 = true;
            } // if (Math.abs(rs1) <= elim)

            if (seg5) {

                if (rs1 > 0.0) {
                    nz[0] = -1;

                    return;
                } // if (rs1 > 0.0)

                // For zr < 0.0, the I function to be added will overflow
                if (zr < 0.0) {
                    nz[0] = -1;

                    return;
                } // if (zr < 0.0)

                nz[0] = n;

                for (i = 1; i <= n; i++) {
                    yr[i - 1] = 0.0;
                    yi[i - 1] = 0.0;
                } // for (i = 1; i <= n; i++)

                return;
            } // if (seg5)

            seg5 = true;
            s1r[0] = cyr[0];
            s1i[0] = cyi[0];
            s2r[0] = cyr[1];
            s2i[0] = cyi[1];
            c1r[0] = csrr[kflag - 1];
            ascle = bry[kflag - 1];

            for (i = ib; i <= n; i++) {
                c2r[0] = s2r[0];
                c2i[0] = s2i[0];
                s2r[0] = (ckr * c2r[0]) - (cki * c2i[0]) + s1r[0];
                s2i[0] = (ckr * c2i[0]) + (cki * c2r[0]) + s1i[0];
                s1r[0] = c2r[0];
                s1i[0] = c2i[0];
                ckr = ckr + rzr;
                cki = cki + rzi;
                c2r[0] = s2r[0] * c1r[0];
                c2i[0] = s2i[0] * c1r[0];
                yr[i - 1] = c2r[0];
                yi[i - 1] = c2i[0];

                if (kflag >= 3) {
                    continue;
                } // if (kflag >= 3)

                str = Math.abs(c2r[0]);
                sti = Math.abs(c2i[0]);
                c2m = Math.max(str, sti);

                if (c2m <= ascle) {
                    continue;
                } // if (c2m <= ascle)

                kflag = kflag + 1;
                ascle = bry[kflag - 1];
                s1r[0] = s1r[0] * c1r[0];
                s1i[0] = s1i[0] * c1r[0];
                s2r[0] = c2r[0];
                s2i[0] = c2i[0];
                s1r[0] = s1r[0] * cssr[kflag - 1];
                s1i[0] = s1i[0] * cssr[kflag - 1];
                s2r[0] = s2r[0] * cssr[kflag - 1];
                s2i[0] = s2i[0] * cssr[kflag - 1];
                c1r[0] = csrr[kflag - 1];
            } // for (i = ib; i <= n; i++)
        } // if (n >= ib)

        if (mr == 0) {
            return;
        } // if (mr == 0)

        // Analytic continuation for Re(z) < 0.0
        nz[0] = 0;
        fmr = mr;

        if (fmr >= 0.0) {
            sgn = -Math.PI;
        } else {
            sgn = Math.PI;
        }

        // cspn and csgn are coeff of K and I functions respectively
        csgni = sgn;

        if (yy <= 0.0) {
            csgni = -csgni;
        } // if (yy <= 0.0)

        ifn = inu + n - 1;
        ang = fnf * sgn;
        cspnr = Math.cos(ang);
        cspni = Math.sin(ang);

        if ( (ifn % 2) != 0) {
            cspnr = -cspnr;
            cspni = -cspni;
        } // if ((ifn%2) != 0)

        // cs = coeff of the J function to get the I function. I(fnu,z) is
        // computed from exp(i*fnu*hpi)*J(fnu,-i*z) where z is in the first
        // quadrant. Fourth quadrant values (yy <= 0.0) are computed by
        // conjugation since the I function is real on the positive real axis
        csr = sar * csgni;
        csi = car * csgni;
        in = (ifn % 4) + 1;
        c2r[0] = cipr[in - 1];
        c2i[0] = cipi[in - 1];
        str = (csr * c2r[0]) + (csi * c2i[0]);
        csi = ( -csr * c2i[0]) + (csi * c2r[0]);
        csr = str;
        asc = bry[0];
        iuf[0] = 0;
        kk = n;
        kdflg = 1;
        ib = ib - 1;
        ic = ib - 1;

        group2: {

            for (k = 1; k <= n; k++) {
                fn = fnu + kk - 1.0;

                // Logic to sort out cases whose parameters were set for the K
                // function above
                if (n > 2) {
                    seg6 = false;
                } // if (n > 2)

                while (true) {

                    if (seg6) {
                        phidr[0] = phir[j - 1];
                        phidi[0] = phii[j - 1];
                        argdr[0] = argr[j - 1];
                        argdi[0] = argi[j - 1];
                        zet1dr[0] = zeta1r[j - 1];
                        zet1di[0] = zeta1i[j - 1];
                        zet2dr[0] = zeta2r[j - 1];
                        zet2di[0] = zeta2i[j - 1];
                        asumdr[0] = asumr[j - 1];
                        asumdi[0] = asumi[j - 1];
                        bsumdr[0] = bsumr[j - 1];
                        bsumdi[0] = bsumi[j - 1];
                        j = 3 - j;

                        break;
                    } // if (seg6)

                    seg6 = true;

                    if ( (kk == n) && (ib < n)) {
                        break;
                    } // if ((kk == n) && (ib < n))

                    if ( (kk == ib) || (kk == ic)) {
                        continue;
                    } // if ((kk == ib) || (kk == ic))

                    zunhj(znr, zni, fn, 0, phidr, phidi, argdr, argdi, zet1dr, zet1di, zet2dr, zet2di, asumdr, asumdi,
                            bsumdr, bsumdi);

                    break;
                } // while (true)

                if (kode != 1) {
                    str = zbr + zet2dr[0];
                    sti = zbi + zet2di[0];
                    rast = fn / zabs(str, sti);
                    str = str * rast * rast;
                    sti = -sti * rast * rast;
                    s1r[0] = -zet1dr[0] + str;
                    s1i[0] = -zet1di[0] + sti;
                } // if (kode != 1)
                else { // kode == 1
                    s1r[0] = -zet1dr[0] + zet2dr[0];
                    s1i[0] = -zet1di[0] + zet2di[0];
                } // else kode == 1

                // Test for underflow and overflow
                rs1 = s1r[0];

                if (Math.abs(rs1) > elim) {
                    seg7 = false;
                    seg8 = false;
                } // if (Math.abs(rs1) > elim)

                if (seg7) {

                    if (kdflg == 1) {
                        iflag = 2;
                    } // if (kdflg == 1)

                    if (Math.abs(rs1) >= alim) {

                        // Refine test and scale
                        aphi = zabs(phidr[0], phidi[0]);
                        aarg = zabs(argdr[0], argdi[0]);
                        rs1 = rs1 + Math.log(aphi) - (0.25 * Math.log(aarg)) - aic;

                        if (Math.abs(rs1) > elim) {
                            seg8 = false;
                            seg9 = false;
                            seg10 = false;
                        } // if (Math.abs(rs1) > elim)

                        if (seg9) {

                            if (kdflg == 1) {
                                iflag = 1;
                            } // if (kdflg == 1)

                            if ( (rs1 >= 0.0) && (kdflg == 1)) {
                                iflag = 3;
                            } // if (rs1 >= 0.0) && (kdflg == 1)
                        } // if (seg9)

                        seg9 = true;
                    } // if (Math.abs(rs1) >= alim)

                    if (seg10) {
                        zairy(argdr[0], argdi[0], 0, 2, air, aii, nai, idum);
                        zairy(argdr[0], argdi[0], 1, 2, dair, daii, ndai, idum);
                        str = (dair[0] * bsumdr[0]) - (daii[0] * bsumdi[0]);
                        sti = (dair[0] * bsumdi[0]) + (daii[0] * bsumdr[0]);
                        str = str + ( (air[0] * asumdr[0]) - (aii[0] * asumdi[0]));
                        sti = sti + ( (air[0] * asumdi[0]) + (aii[0] * asumdr[0]));
                        ptr = (str * phidr[0]) - (sti * phidi[0]);
                        pti = (str * phidi[0]) + (sti * phidr[0]);
                        s2r[0] = (ptr * csr) - (pti * csi);
                        s2i[0] = (ptr * csi) + (pti * csr);
                        str = Math.exp(s1r[0]) * cssr[iflag - 1];
                        s1r[0] = str * Math.cos(s1i[0]);
                        s1i[0] = str * Math.sin(s1i[0]);
                        str = (s2r[0] * s1r[0]) - (s2i[0] * s1i[0]);
                        s2i[0] = (s2r[0] * s1i[0]) + (s2i[0] * s1r[0]);
                        s2r[0] = str;

                        if (iflag == 1) {
                            zuchk(s2r[0], s2i[0], nw, bry[0]);

                            if (nw[0] != 0) {
                                s2r[0] = 0.0;
                                s2i[0] = 0.0;
                            } // if nw[0] != 0)
                        } // if (iflag == 1)
                    } // if (seg10)

                    seg10 = true;
                } // if (seg7)

                seg7 = true;

                while (true) {

                    if (seg8) {

                        if (yy <= 0.0) {
                            s2i[0] = -s2i[0];
                        } // if (yy <= 0.0)

                        cyr[kdflg - 1] = s2r[0];
                        cyi[kdflg - 1] = s2i[0];
                        c2r[0] = s2r[0];
                        c2i[0] = s2i[0];
                        s2r[0] = s2r[0] * csrr[iflag - 1];
                        s2i[0] = s2i[0] * csrr[iflag - 1];

                        // Add I and K functions, K sequence in Y(i), i = 1,n
                        s1r[0] = yr[kk - 1];
                        s1i[0] = yi[kk - 1];

                        if (kode != 1) {
                            zs1s2(zrr, zri, s1r, s1i, s2r, s2i, nw, asc, iuf);
                            nz[0] = nz[0] + nw[0];
                        } // if (kode != 1)

                        yr[kk - 1] = (s1r[0] * cspnr) - (s1i[0] * cspni) + s2r[0];
                        yi[kk - 1] = (s1r[0] * cspni) + (s1i[0] * cspnr) + s2i[0];
                        kk = kk - 1;
                        cspnr = -cspnr;
                        cspni = -cspni;
                        str = csi;
                        csi = -csr;
                        csr = str;

                        if ( (c2r[0] == 0.0) && (c2i[0] == 0.0)) {
                            kdflg = 1;

                            break;
                        } // if ((c2r[0] == 0.0) && (c2i[0] == 0.0))

                        if (kdflg == 2) {
                            break group2;
                        } // if (kdflg == 2)

                        kdflg = 2;

                        break;
                    } // if (seg8)

                    seg8 = true;

                    if (rs1 > 0.0) {
                        nz[0] = -1;

                        return;
                    } // if (rs1 > 0.0)

                    s2r[0] = 0.0;
                    s2i[0] = 0.0;
                } // while (true)
            } // for (k = 1; k <= n; k++)

            k = n;
        } // group2

        il = n - k;

        if (il == 0) {
            return;
        } // if (il == 0)

        // Recur backward for remainder of I sequence and add in the K functions,
        // scaling the I sequence during recurrence to keep intermediate
        // arithmetic on scale near exponent extremes.
        s1r[0] = cyr[0];
        s1i[0] = cyi[0];
        s2r[0] = cyr[1];
        s2i[0] = cyi[1];
        csr = csrr[iflag - 1];
        ascle = bry[iflag - 1];
        fn = inu + il;

        for (i = 1; i <= il; i++) {
            c2r[0] = s2r[0];
            c2i[0] = s2i[0];
            s2r[0] = s1r[0] + ( (fn + fnf) * ( (rzr * c2r[0]) - (rzi * c2i[0])));
            s2i[0] = s1i[0] + ( (fn + fnf) * ( (rzr * c2i[0]) + (rzi * c2r[0])));
            s1r[0] = c2r[0];
            s1i[0] = c2i[0];
            fn = fn - 1.0;
            c2r[0] = s2r[0] * csr;
            c2i[0] = s2i[0] * csr;
            ckr = c2r[0];
            cki = c2i[0];
            c1r[0] = yr[kk - 1];
            c1i[0] = yi[kk - 1];

            if (kode != 1) {
                zs1s2(zrr, zri, c1r, c1i, c2r, c2i, nw, asc, iuf);
                nz[0] = nz[0] + nw[0];
            } // if (kode != 1)

            yr[kk - 1] = (c1r[0] * cspnr) - (c1i[0] * cspni) + c2r[0];
            yi[kk - 1] = (c1r[0] * cspni) + (c1i[0] * cspnr) + c2i[0];
            kk = kk - 1;
            cspnr = -cspnr;
            cspni = -cspni;

            if (iflag >= 3) {
                continue;
            } // if (iflag >= 3)

            c2r[0] = Math.abs(ckr);
            c2i[0] = Math.abs(cki);
            c2m = Math.max(c2r[0], c2i[0]);

            if (c2m <= ascle) {
                continue;
            } // if (c2m <= ascle)

            iflag = iflag + 1;
            ascle = bry[iflag - 1];
            s1r[0] = s1r[0] * csr;
            s1i[0] = s1i[0] * csr;
            s2r[0] = ckr;
            s2i[0] = cki;
            s1r[0] = s1r[0] * cssr[iflag - 1];
            s1i[0] = s1i[0] * cssr[iflag - 1];
            s2r[0] = s2r[0] * cssr[iflag - 1];
            s2i[0] = s2i[0] * cssr[iflag - 1];
            csr = csrr[iflag - 1];
        } // for (i = 1; i <= il; i++)

        return;
    }

    /**
     * ZUOIK COMPUTES THE LEADING TERMS OF THE UNIFORM ASYMPTOTIC EXPANSIONS FOR THE I AND K FUNCTIONS AND COMPARES THEM
     * (IN LOGARITHMIC FORM) TO ALIM AND ELIM FOR OVER AND UNDERFLOW WHERE ALIM.LT.ELIM. IF THE MAGNITUDE, BASED ON THE
     * LEADING EXPONENTIAL, IS LESS THAN ALIM OR GREATER THAN -ALIM, THEN THE RESULT IS ON SCALE. IF NOT, THEN A REFINED
     * TEST USING OTHER MULTIPLIERS (IN LOGARITHMIC FORM) IS MADE BASED ON ELIM. HERE EXP(-ELIM)=SMALLEST MACHINE
     * NUMBER*1.0E+3 AND EXP(-ALIM)= EXP(-ELIM)/TOL
     * 
     * <p>
     * IKFLG=1 MEANS THE I SEQUENCE IS TESTED =2 MEANS THE K SEQUENCE IS TESTED NUF = 0 MEANS THE LAST MEMBER OF THE
     * SEQUENCE IS ON SCALE =-1 MEANS AN OVERFLOW WOULD OCCUR IKFLG=1 AND NUF.GT.0 MEANS THE LAST NUF Y VALUES WERE SET
     * TO ZERO THE FIRST N-NUF VALUES MUST BE SET BY ANOTHER ROUTINE IKFLG=2 AND NUF.EQ.N MEANS ALL Y VALUES WERE SET TO
     * ZERO IKFLG=2 AND 0.LT.NUF.LT.N NOT CONSIDERED. Y MUST BE SET BY ANOTHER ROUTINE
     * </p>
     * 
     * @param zr double
     * @param zi double
     * @param fnu double
     * @param kode int
     * @param ikflg int
     * @param n int
     * @param yr double[]
     * @param yi double[]
     * @param nuf int[]
     */
    private void zuoik(double zr, double zi, final double fnu, final int kode, final int ikflg, final int n,
            final double[] yr, final double[] yi, final int[] nuf) {
        final double aic = 1.265512123484645396;
        int nn;
        double zrr;
        double zri;
        double zbr;
        double zbi;
        double ax;
        double ay;
        int iform;
        double gnu;
        double fnn;
        double gnn;
        int init;
        double czr;
        double czi;
        double znr = 0.0;
        double zni = 0.0;
        final double[] zeta1r = new double[1];
        final double[] zeta2r = new double[1];
        final double[] zeta1i = new double[1];
        final double[] zeta2i = new double[1];
        final double[] cwrkr = new double[16];
        final double[] cwrki = new double[16];
        final double[] phir = new double[1];
        final double[] phii = new double[1];
        final double[] argr = new double[1];
        final double[] argi = new double[1];
        double aphi;
        double rcz;
        final double[] sumr = new double[1];
        final double[] sumi = new double[1];
        final double[] asumr = new double[1];
        final double[] asumi = new double[1];
        final double[] bsumr = new double[1];
        final double[] bsumi = new double[1];
        int i;
        double ascle;
        final int[] idum = new int[1];
        double aarg = 1.0;
        boolean seg1 = true;
        boolean seg2 = true;
        boolean seg3 = true;
        boolean seg4 = true;
        final double[] str = new double[1];
        final double[] sti = new double[1];
        final int[] nw = new int[1];

        nuf[0] = 0;
        nn = n;
        zrr = zr;
        zri = zi;

        if (zr < 0.0) {
            zrr = -zr;
            zri = -zi;
        }

        zbr = zrr;
        zbi = zri;
        ax = 1.7321 * Math.abs(zr);
        ay = Math.abs(zi);
        iform = 1;

        if (ay > ax) {
            iform = 2;
        }

        gnu = Math.max(fnu, 1.0);

        if (ikflg != 1) {
            fnn = nn;
            gnn = fnu + fnn - 1.0;
            gnu = Math.max(gnn, fnn);
        } // if (ikflg != 1)

        // Only the magnitude of arg and phi are needed along with the real
        // parts of zeta1, zeta2, and zb. No attempt is made to get the sign
        // of the imaginary part correct
        if (iform != 2) {
            init = 0;
            zunik(zrr, zri, gnu, ikflg, 1, init, phir, phii, zeta1r, zeta1i, zeta2r, zeta2i, sumr, sumi, cwrkr, cwrki);
            czr = -zeta1r[0] + zeta2r[0];
            czi = -zeta1i[0] + zeta2i[0];
        } // if (iform != 2)
        else { // iform == 2
            znr = zri;
            zni = -zrr;

            if (zi <= 0.0) {
                znr = -znr;
            }

            zunhj(znr, zni, gnu, 1, phir, phii, argr, argi, zeta1r, zeta1i, zeta2r, zeta2i, asumr, asumi, bsumr, bsumi);
            czr = -zeta1r[0] + zeta2r[0];
            czi = -zeta1i[0] + zeta2i[0];
            aarg = zabs(argr[0], argi[0]);
        } // else iform == 2

        if (kode != 1) {
            czr = czr - zbr;
            czi = czi - zbi;
        } // if (kode != 1)

        if (ikflg != 1) {
            czr = -czr;
            czi = -czi;
        } // if (ikflg != 1)

        aphi = zabs(phir[0], phii[0]);
        rcz = czr;

        // Overflow test
        if (rcz > elim) {
            nuf[0] = -1;

            return;
        } // if (rcz > elim)

        while (true) {

            if (seg1) {

                if (rcz >= alim) {
                    rcz = rcz + Math.log(aphi);

                    if (iform == 2) {
                        rcz = rcz - (0.25 * Math.log(aarg)) - aic;
                    } // if (iform == 2)

                    if (rcz > elim) {
                        nuf[0] = -1;

                        return;
                    } // if (rcz > elim)

                    break;
                } // if (rcz >= alim)

                // Underflow test
                if (rcz >= ( -elim)) {

                    if (rcz > ( -alim)) {
                        break;
                    }

                    rcz = rcz + Math.log(aphi);

                    if (iform == 2) {
                        rcz = rcz - (0.25 * Math.log(aarg)) - aic;
                    } // if (iform == 2)

                    if (rcz > ( -elim)) {
                        seg2 = false;
                    }
                } // if (rcz >= -elim)
            } // if (seg1)

            seg1 = true;

            if (seg2) {

                for (i = 1; i <= nn; i++) {
                    yr[i - 1] = 0.0;
                    yi[i - 1] = 0.0;
                } // for (i = 1; i <= nn; i++)

                nuf[0] = nn;

                return;
            } // if (seg2)

            seg2 = true;
            ascle = 1.0E3 * tiny / tol;
            zlog(phir[0], phii[0], str, sti, idum);
            czr = czr + str[0];
            czi = czi + sti[0];

            if (iform != 1) {
                zlog(argr[0], argi[0], str, sti, idum);
                czr = czr - (0.25 * str[0]) - aic;
                czi = czi - (0.25 * sti[0]);
            } // if (iform != 1)

            ax = Math.exp(rcz) / tol;
            ay = czi;
            czr = ax * Math.cos(ay);
            czi = ax * Math.sin(ay);
            zuchk(czr, czi, nw, ascle);

            if (nw[0] == 0) {
                break;
            }

            seg1 = false;
        } // while (true)

        if ( (ikflg == 2) || (n == 1)) {
            return;
        }

        while (true) {

            if (seg3) {
                gnu = fnu + nn - 1.0;

                if (iform != 2) {
                    init = 0;
                    zunik(zrr, zri, gnu, ikflg, 1, init, phir, phii, zeta1r, zeta1i, zeta2r, zeta2i, sumr, sumi, cwrkr,
                            cwrki);
                    czr = -zeta1r[0] + zeta2r[0];
                    czi = -zeta1i[0] + zeta2i[0];
                } // if (iform != 2)
                else {
                    zunhj(znr, zni, gnu, 1, phir, phii, argr, argi, zeta1r, zeta1i, zeta2r, zeta2i, asumr, asumi,
                            bsumr, bsumi);
                    czr = -zeta1r[0] + zeta2r[0];
                    czi = -zeta1i[0] + zeta2i[0];
                    aarg = zabs(argr[0], argi[0]);
                } // else

                if (kode != 1) {
                    czr = czr - zbr;
                    czi = czi - zbi;
                } // if (kode != 1)

                aphi = zabs(phir[0], phii[0]);
                rcz = czr;

                if (rcz >= ( -elim)) {

                    if (rcz > ( -alim)) {
                        return;
                    } // if (rcz > (-alim)

                    rcz = rcz + Math.log(aphi);

                    if (iform == 2) {
                        rcz = rcz - (0.25 * Math.log(aarg)) - aic;
                    }

                    if (rcz > ( -elim)) {
                        seg4 = false;
                    } // if (rcz > (-elim))
                } // if (rcz >= (-elim))
            } // if (seg3)

            seg3 = true;

            if (seg4) {
                yr[nn - 1] = 0.0;
                yi[nn - 1] = 0.0;
                nn = nn - 1;
                nuf[0] = nuf[0] + 1;

                if (nn == 0) {
                    return;
                } // if (nn == 0)

                continue;
            } // if (seg4)

            seg4 = true;
            ascle = 1.0E3 * tiny / tol;
            zlog(phir[0], phii[0], str, sti, idum);
            czr = czr + str[0];
            czi = czi + sti[0];

            if (iform != 1) {
                zlog(argr[0], argi[0], str, sti, idum);
                czr = czr - (0.25 * str[0]) - aic;
                czi = czi - (0.25 * sti[0]);
            } // if (iform != 1)

            ax = Math.exp(rcz) / tol;
            ay = czi;
            czr = ax * Math.cos(ay);
            czi = ax * Math.sin(ay);
            zuchk(czr, czi, nw, ascle);

            if (nw[0] != 0) {
                seg3 = false;

                continue;
            }

            return;
        } // while (true)
    }

    /**
     * zwrsk computes the I Bessel function for Real(z) >- 0.0 by normalizing the I function ratios from zrati by the
     * Wronskian.
     * 
     * @param zrr double
     * @param zri double
     * @param fnu double
     * @param kode int
     * @param n int
     * @param yr double[]
     * @param yi double[]
     * @param nz int[]
     * @param cwr double[]
     * @param cwi double[]
     */
    private void zwrsk(final double zrr, final double zri, final double fnu, final int kode, final int n,
            final double[] yr, final double[] yi, final int[] nz, final double[] cwr, final double[] cwi) {
        final int[] nw = new int[1];
        double cinur;
        double cinui;
        double acw;
        double ascle;
        double csclr;
        double c1r;
        double c1i;
        double c2r;
        double c2i;
        double str;
        double sti;
        double ptr;
        double pti;
        double ctr;
        double cti;
        double act;
        double ract;
        int i;

        // I(fnu+i-1,Z) BY BACKWARD RECURRENCE FOR RATIOS
        // Y(I)=I(fnu+i,Z)/I(fnu+i-1,Z) FROM CRATI NORMALIZED BY THE
        // WRONSKIAN WITH K(fnu,Z) AND K(fnu+1,Z) FROM CBKNU.
        nz[0] = 0;
        zbknu(zrr, zri, fnu, kode, 2, cwr, cwi, nw);

        if (nw[0] == -2) {
            nz[0] = -2;

            return;
        } // if (nw[0] == -2)
        else if (nw[0] != 0) {
            nz[0] = -1;

            return;
        } // else if (nw[0] != 0)

        zrati(zrr, zri, fnu, n, yr, yi);

        // Recur forward on I(fnu+1,z) = R(fnu,z)*I(fnu,z),
        // R(fnu+j-1,z) = Y(j), j = 1,...,n
        cinur = 1.0;
        cinui = 0.0;

        if (kode != 1) {
            cinur = Math.cos(zri);
            cinui = Math.sin(zri);
        } // if (kode != 1)

        // On low exponent machines the k functions can be close to both the
        // under and overflow limits and the normalization must be scaled to
        // prevent over or underflow. zuoik has determined that the result is
        // on scale.
        acw = zabs(cwr[1], cwi[1]);
        ascle = 1.0E3 * tiny / tol;
        csclr = 1.0;

        if (acw <= ascle) {
            csclr = 1.0 / tol;
        } // if (acw <= ascle
        else { // acw > ascle
            ascle = 1.0 / ascle;

            if (acw >= ascle) {
                csclr = tol;
            } // if (acw >= ascle)
        } // else acw > ascle

        c1r = cwr[0] * csclr;
        c1i = cwi[0] * csclr;
        c2r = cwr[1] * csclr;
        c2i = cwi[1] * csclr;
        str = yr[0];
        sti = yi[0];

        // cinu = cinu*(CONJG(ct)/CABS(ct))*(1.0/CABS(ct)) prevents
        // under- or overflow prematurely by squaring CABS(ct)
        ptr = (str * c1r) - (sti * c1i);
        pti = (str * c1i) + (sti * c1r);
        ptr = ptr + c2r;
        pti = pti + c2i;
        ctr = (zrr * ptr) - (zri * pti);
        cti = (zrr * pti) + (zri * ptr);
        act = zabs(ctr, cti);
        ract = 1.0 / act;
        ctr = ctr * ract;
        cti = -cti * ract;
        ptr = cinur * ract;
        pti = cinui * ract;
        cinur = (ptr * ctr) - (pti * cti);
        cinui = (ptr * cti) + (pti * ctr);
        yr[0] = cinur * csclr;
        yi[0] = cinui * csclr;

        if (n == 1) {
            return;
        } // if (n == 1)

        for (i = 2; i <= n; i++) {
            ptr = (str * cinur) - (sti * cinui);
            cinui = (str * cinui) + (sti * cinur);
            cinur = ptr;
            str = yr[i - 1];
            sti = yi[i - 1];
            yr[i - 1] = cinur * csclr;
            yi[i - 1] = cinui * csclr;
        } // for (i = 2; i <= n; i++)

        return;
    }

}
